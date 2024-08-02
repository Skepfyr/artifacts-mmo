#![feature(is_none_or)]

use core::fmt;
use std::{
    collections::{HashMap, HashSet},
    convert::identity,
    ops::{Index, IndexMut},
    sync::{Arc, Mutex},
    time::Duration,
    vec,
};

use enum_map::EnumMap;
use linked_hash_map::LinkedHashMap;
use sdk::{
    ApiError, Character, ContentType, Craft, CraftSkill, EffectType, Elements, FightResult,
    GatherSkill, Inventory, Item, ItemSlot, ItemStack, ItemType, Map, Monster, Name, Position,
    Resource, Server,
};
use slotmap::{DefaultKey, SlotMap};
use strum::IntoEnumIterator;
use time::OffsetDateTime;

#[tokio::main]
async fn main() {
    let server = Server::new(
        "https://api.artifactsmmo.com/",
        std::env::var("API_TOKEN").unwrap(),
    );
    let game = Game::new(server).await;
    game.complete(Task::anyone(TaskKind::HaveItem(ItemStack {
        code: "lich_crown".to_string(),
        quantity: 1,
    })))
    .await;
}

type RCharacter = Result<Character, Character>;

struct Game {
    server: Server,
    world: World,
    characters: HashMap<Name, Arc<Mutex<Character>>>,
    items: HashMap<String, Item>,
    monsters: HashMap<String, Monster>,
    resources: HashMap<String, Resource>,
    bank: Bank,
}

impl Game {
    async fn new(server: Server) -> Self {
        let world = World::new(server.all_maps().await.unwrap());
        let characters = server
            .characters()
            .await
            .unwrap()
            .into_iter()
            .map(|c| (c.name.clone(), Arc::new(Mutex::new(c))))
            .collect();
        let items = server
            .all_items()
            .await
            .unwrap()
            .into_iter()
            .map(|i| (i.code.clone(), i))
            .collect();
        let monsters = server
            .all_monsters()
            .await
            .unwrap()
            .into_iter()
            .map(|m| (m.code.clone(), m))
            .collect();
        let resources = server
            .all_resources()
            .await
            .unwrap()
            .into_iter()
            .map(|r| (r.code.clone(), r))
            .collect();
        let bank = Bank::new(server.clone()).await;
        Self {
            server,
            characters,
            world,
            items,
            monsters,
            resources,
            bank,
        }
    }

    async fn complete(&self, task: Task) {
        let characters = self.server.characters().await.unwrap();
        let mut tasks = TaskTree::new();
        tasks.add_root(task);
        let tasks = Arc::new(Mutex::new(tasks));
        let (tx, mut rx) = tokio::sync::mpsc::channel(characters.len());
        let tx = &tx;
        moro::scope_fn(|s| {
            Box::pin(async move {
                for character in characters {
                    s.spawn(async move {
                        sleep_for_cooldown(&character).await;
                        tx.send(character).await.unwrap();
                    });
                }
                'outer: while let Some(character) = rx.recv().await {
                    *self.characters[&character.name].lock().unwrap() = character.clone();
                    let task = loop {
                        let tasks = &mut tasks.lock().unwrap();
                        if let Some(task) = tasks
                            .find_available(|task| self.can_immediately_complete(&character, task))
                        {
                            break task;
                        } else if let Some(task) = tasks.pop_available(|task| {
                            if let TaskKind::HaveItem(stack) = &task.kind {
                                task.is_for(&character)
                                    || self.bank.quantity(&stack.code) < stack.quantity
                            } else {
                                true
                            }
                        }) {
                            let task_data = &tasks[task];
                            let dependencies = self.compute_dependencies(task_data, &character);
                            println!("\nIn order to complete: {}", task_data);
                            println!("We need the following dependencies:");
                            for dependency in &dependencies {
                                println!("- {}", dependency);
                            }
                            println!();
                            tasks.add_dependencies(task, dependencies);
                        } else {
                            break 'outer;
                        }
                    };
                    let task_data = tasks.lock().unwrap()[task].clone();
                    println!("{} has picked up: {}", character.name, task_data);
                    let tasks = tasks.clone();
                    s.spawn(async move {
                        let character = self
                            .immediately_complete(character, task_data.clone())
                            .await;
                        match &character {
                            Ok(character) => {
                                println!("{} has completed: {}", character.name, task_data)
                            }
                            Err(character) => {
                                println!("{} failed to complete: {}", character.name, task_data)
                            }
                        }
                        tasks.lock().unwrap().complete_task(task);
                        tx.send(character.unwrap_or_else(identity)).await.unwrap();
                    });
                }
            })
        })
        .await;
    }

    async fn move_to(&self, character: Character, position: Position) -> Character {
        if character.position == position {
            return character;
        }
        match self.server.move_character(&character.name, position).await {
            Ok(response) => response.character,
            Err(err) => panic!("Failed to move {}: {}", character.name, err),
        }
    }

    fn has_items(&self, character: &Character, stack: &ItemStack) -> bool {
        let character_count: u32 = character
            .inventory
            .items
            .iter()
            .flatten()
            .filter(|s| s.code == stack.code)
            .map(|s| s.quantity)
            .sum();
        let bank_count = self
            .bank
            .items
            .lock()
            .unwrap()
            .get(&stack.code)
            .map(|s| s.quantity)
            .unwrap_or(0);
        character_count + bank_count >= stack.quantity
    }

    fn best_item_for_effect_level(&self, effect_type: EffectType, level: u32) -> Option<Item> {
        self.items
            .values()
            .filter(|item| item.level < level)
            .filter_map(|item| {
                item.effects
                    .iter()
                    .find(|effect| effect.name == effect_type)
                    .map(|effect| (item, effect))
            })
            .min_by_key(|(_, effect)| effect.value)
            .map(|(item, _effect)| item.clone())
    }

    fn item_is_obtainable(&self, item: &Item, monster: &Monster, character: &Character) -> bool {
        if item.level > character.level.level && item.level > monster.level {
            return false;
        }
        let mut items = vec![item.code.clone()];
        while let Some(item) = items.pop() {
            let dependencies = self.compute_dependencies(
                &Task::anyone(TaskKind::HaveItem(ItemStack {
                    code: item,
                    quantity: 1,
                })),
                character,
            );
            for dependency in dependencies {
                let obtainable = match dependency.kind {
                    TaskKind::HaveItem(item) => {
                        let level = self.items[&item.code].level;
                        items.push(item.code);
                        level <= character.level.level || level <= monster.level
                    }
                    TaskKind::Craft(_, craft, _) => {
                        items.extend(craft.items.into_iter().map(|item| item.code));
                        character.has_craft_skill(craft.skill, craft.level)
                            || craft.level <= monster.level
                    }
                    TaskKind::Fight(other, _) => other.level < monster.level,
                    TaskKind::Gather(resource, _) => {
                        character.has_gather_skill(resource.skill, resource.level)
                            || resource.level <= monster.level
                    }
                    TaskKind::FightSkill(level) => {
                        level <= character.level.level || level <= monster.level
                    }
                    TaskKind::CraftSkill(skill, level) => {
                        character.has_craft_skill(skill, level) || level <= monster.level
                    }
                    TaskKind::GatherSkill(skill, level) => {
                        character.has_gather_skill(skill, level) || level <= monster.level
                    }
                    TaskKind::CompleteTask | TaskKind::ExchangeTaskCoins => true,
                };
                if !obtainable {
                    return false;
                }
            }
        }
        true
    }

    fn can_immediately_complete(&self, character: &Character, task: &Task) -> bool {
        fn is_completed(game: &Game, character: &Character, task: &TaskKind) -> Option<bool> {
            Some(match task {
                TaskKind::HaveItem(stack) => game.has_items(character, stack),
                TaskKind::Craft(_, _, _) | TaskKind::Gather(_, _) | TaskKind::Fight(_, _) => {
                    return None
                }
                TaskKind::FightSkill(level) => character.level.level >= *level,
                &TaskKind::CraftSkill(skill, level) => match skill {
                    CraftSkill::Mining => character.mining_level.level >= level,
                    CraftSkill::Woodcutting => character.woodcutting_level.level >= level,
                    CraftSkill::WeaponCrafting => character.weapon_crafting_level.level >= level,
                    CraftSkill::GearCrafting => character.gear_crafting_level.level >= level,
                    CraftSkill::JewelryCrafting => character.jewelry_crafting_level.level >= level,
                    CraftSkill::Cooking => character.cooking_level.level >= level,
                },
                &TaskKind::GatherSkill(skill, level) => match skill {
                    GatherSkill::Mining => character.mining_level.level >= level,
                    GatherSkill::Woodcutting => character.woodcutting_level.level >= level,
                    GatherSkill::Fishing => character.fishing_level.level >= level,
                },
                TaskKind::CompleteTask => character
                    .task
                    .as_ref()
                    .is_none_or(|task| task.remaining() == 0),
                TaskKind::ExchangeTaskCoins => return None,
            })
        }
        task.is_for(character)
            && is_completed(self, character, &task.kind).unwrap_or_else(|| {
                self.compute_dependencies(task, character)
                    .iter()
                    .all(|task| is_completed(self, character, &task.kind).unwrap())
            })
    }

    async fn immediately_complete(&self, mut character: Character, task: Task) -> RCharacter {
        assert!(task.is_for(&character));
        match task.kind {
            TaskKind::HaveItem(_) => {}
            TaskKind::Craft(code, craft, mut num_times) => {
                let num_ingredients = craft.items.iter().map(|s| s.quantity).sum::<u32>();
                while num_times > 0 {
                    let num_crafts =
                        u32::min(num_times, character.inventory.max_items / num_ingredients);
                    let has_ingredients = craft.items.iter().all(|item| {
                        character.inventory.items.iter().flatten().any(|i| {
                            i.code == item.code && i.quantity >= item.quantity * num_crafts
                        })
                    });
                    if !has_ingredients {
                        let bank = self
                            .world
                            .nearest(ContentType::Bank, None, character.position);
                        character = self.move_to(character, bank).await;
                        sleep_for_cooldown(&character).await;
                        character = self.bank.deposit_items(character).await;
                        sleep_for_cooldown(&character).await;
                        for item in craft.items.iter() {
                            let stack = ItemStack {
                                code: item.code.clone(),
                                quantity: item.quantity * num_crafts,
                            };
                            character = self.bank.withdraw_items(character, stack).await?;
                            sleep_for_cooldown(&character).await;
                        }
                    }

                    let workshop = self.world.nearest(
                        ContentType::Workshop,
                        Some(&craft.skill.to_string()),
                        character.position,
                    );
                    character = self.move_to(character, workshop).await;
                    sleep_for_cooldown(&character).await;
                    character = self
                        .server
                        .craft(&character.name, code.clone(), num_crafts)
                        .await
                        .unwrap()
                        .character;
                    num_times -= num_crafts;
                    sleep_for_cooldown(&character).await;
                    // If it's the last one and it's for us then we can keep the items.
                    if num_times > 0 || task.character.is_none() {
                        let bank = self
                            .world
                            .nearest(ContentType::Bank, None, character.position);
                        character = self.move_to(character, bank).await;
                        sleep_for_cooldown(&character).await;
                        character = self.bank.deposit_items(character).await;
                        sleep_for_cooldown(&character).await;
                    }
                }
            }
            TaskKind::Gather(resource, mut repeat) => {
                let bank = self
                    .world
                    .nearest(ContentType::Bank, None, character.position);
                let gather_pos =
                    self.world
                        .nearest(ContentType::Resource, Some(&resource.code), bank);
                let bank = self.world.nearest(ContentType::Bank, None, gather_pos);

                let effect_type = match resource.skill {
                    GatherSkill::Mining => EffectType::Mining,
                    GatherSkill::Woodcutting => EffectType::Woodcutting,
                    GatherSkill::Fishing => EffectType::Fishing,
                };
                let mut gather_weapons: Vec<_> = self
                    .items
                    .values()
                    .filter_map(|item| {
                        item.effects
                            .iter()
                            .find(|effect| effect.name == effect_type)
                            .map(|effect| (item, effect))
                    })
                    .filter(|(item, _)| {
                        self.has_items(
                            &character,
                            &ItemStack {
                                code: item.code.clone(),
                                quantity: 1,
                            },
                        )
                    })
                    .collect();
                gather_weapons.sort_by_key(|(_, effect)| -effect.value);

                if let Some((best_item, _)) = gather_weapons.pop() {
                    let best_item_stack = ItemStack {
                        code: best_item.code.clone(),
                        quantity: 1,
                    };
                    let has_best_item_equipped = character
                        .inventory
                        .weapon
                        .as_ref()
                        .is_some_and(|i| *i == best_item.code);
                    if !has_best_item_equipped {
                        let item_to_unequip = character.inventory.weapon.is_some();
                        let has_best_item = character
                            .inventory
                            .items
                            .iter()
                            .flatten()
                            .any(|i| i.code == best_item.code);
                        if !has_best_item
                            || (item_to_unequip
                                && !character.inventory.has_space_for(&best_item_stack))
                        {
                            character = self.move_to(character, bank).await;
                            sleep_for_cooldown(&character).await;
                            character = self.bank.deposit_items(character).await;
                            sleep_for_cooldown(&character).await;
                            character =
                                self.bank.withdraw_items(character, best_item_stack).await?;
                            sleep_for_cooldown(&character).await;
                        }
                        if item_to_unequip {
                            character = self
                                .server
                                .unequip_item(&character.name, ItemSlot::Weapon)
                                .await
                                .unwrap()
                                .character;
                            sleep_for_cooldown(&character).await;
                        }
                        character = self
                            .server
                            .equip_item(&character.name, &best_item.code, ItemSlot::Weapon)
                            .await
                            .unwrap()
                            .character;
                        sleep_for_cooldown(&character).await;
                    }
                }
                if let Repeat::Items(items) = &mut repeat {
                    if resource
                        .drops
                        .iter()
                        .find(|item| item.code == items.code)
                        .unwrap()
                        .rate
                        < 3
                    {
                        items.quantity = u32::max(5, items.quantity)
                    }
                }
                let mut remaining = repeat;
                while remaining.count() > 0 {
                    if !resource.drops.iter().all(|drop| {
                        character.inventory.has_space_for(&ItemStack {
                            code: drop.code.clone(),
                            quantity: drop.max_quantity,
                        })
                    }) {
                        character = self.move_to(character, bank).await;
                        sleep_for_cooldown(&character).await;
                        character = self.bank.deposit_items(character).await;
                        sleep_for_cooldown(&character).await;
                    }
                    character = self.move_to(character, gather_pos).await;
                    sleep_for_cooldown(&character).await;
                    let gather = self.server.gather(&character.name).await.unwrap();
                    character = gather.character;
                    match &mut remaining {
                        Repeat::Times(n) => *n -= 1,
                        Repeat::Items(items) => {
                            let gathered = gather
                                .details
                                .items
                                .iter()
                                .find(|item| item.code == items.code)
                                .map(|item| item.quantity)
                                .unwrap_or(0);
                            items.quantity = items.quantity.saturating_sub(gathered);
                        }
                    }
                    sleep_for_cooldown(&character).await;
                }
                if task.character.is_none() {
                    character = self.move_to(character, bank).await;
                    sleep_for_cooldown(&character).await;
                    character = self.bank.deposit_items(character).await;
                    sleep_for_cooldown(&character).await;
                }
            }
            TaskKind::Fight(monster, items) => {
                let bank = self
                    .world
                    .nearest(ContentType::Bank, None, character.position);
                let monster_pos =
                    self.world
                        .nearest(ContentType::Monster, Some(&monster.code), bank);
                let bank = self.world.nearest(ContentType::Bank, None, monster_pos);

                let (load_out, best_turns) = best_load_out(self, &character, &monster);

                let mut remaining = items;
                while remaining.count() > 0 {
                    // We might be able to do better than the "best" because
                    // we might be fighting a weak monster so the "best" was
                    // constrained.
                    // Need to do inside the loop because of consumables.
                    if can_defeat(self, &character.inventory, &monster)
                        .is_none_or(|num_turns| num_turns > best_turns)
                    {
                        for slot in ItemSlot::iter() {
                            let character_item = character.inventory.get(slot);
                            let load_out_item = load_out.items[slot].clone();

                            let character_has_item = match (&character_item, &load_out_item) {
                                (Some(character_item), Some(load_out_item)) => {
                                    character_item.has(load_out_item)
                                }
                                (None, Some(_)) => false,
                                (_, None) => true,
                            };

                            if !character_has_item {
                                if !character_item
                                    .as_ref()
                                    .is_some_and(|item| character.inventory.has_space_for(item))
                                {
                                    character = self.move_to(character, bank).await;
                                    sleep_for_cooldown(&character).await;
                                    character = self.bank.deposit_items(character).await;
                                    sleep_for_cooldown(&character).await;
                                }
                                if character_item.is_some() {
                                    character = self
                                        .server
                                        .unequip_item(&character.name, slot)
                                        .await
                                        .unwrap()
                                        .character;
                                    sleep_for_cooldown(&character).await;
                                }
                                if let Some(stack) = &load_out_item {
                                    if !character
                                        .inventory
                                        .items
                                        .iter()
                                        .flatten()
                                        .any(|i| i.has(stack))
                                    {
                                        let mut needs = self.multiply_consumable(
                                            stack, &remaining, &monster, best_turns,
                                        );
                                        needs.quantity -= character
                                            .inventory
                                            .items
                                            .iter()
                                            .flatten()
                                            .find(|i| i.code == stack.code)
                                            .map(|i| i.quantity)
                                            .unwrap_or(0);
                                        character = self.move_to(character, bank).await;
                                        sleep_for_cooldown(&character).await;
                                        if !character.inventory.has_space_for(&needs) {
                                            character = self.bank.deposit_items(character).await;
                                            sleep_for_cooldown(&character).await;
                                        }
                                        character =
                                            self.bank.withdraw_items(character, needs).await?;
                                        sleep_for_cooldown(&character).await;
                                    }
                                    character = self
                                        .server
                                        .equip_item(&character.name, &stack.code, slot)
                                        .await
                                        .unwrap()
                                        .character;
                                    sleep_for_cooldown(&character).await;
                                }
                            }
                        }
                    }

                    if !monster.drops.iter().all(|drop| {
                        character.inventory.has_space_for(&ItemStack {
                            code: drop.code.clone(),
                            quantity: drop.max_quantity,
                        })
                    }) {
                        character = self.move_to(character, bank).await;
                        sleep_for_cooldown(&character).await;
                        character = self.bank.deposit_items(character).await;
                        sleep_for_cooldown(&character).await;
                    }
                    character = self.move_to(character, monster_pos).await;
                    sleep_for_cooldown(&character).await;
                    let fight = self.server.fight(&character.name).await.unwrap();
                    character = fight.character;
                    assert!(fight.fight.result == FightResult::Win);
                    match &mut remaining {
                        Repeat::Times(n) => *n -= 1,
                        Repeat::Items(items) => {
                            let gathered = fight
                                .fight
                                .drops
                                .iter()
                                .find(|item| item.code == items.code)
                                .map(|item| item.quantity)
                                .unwrap_or(0);
                            items.quantity = items.quantity.saturating_sub(gathered);
                        }
                    }
                    sleep_for_cooldown(&character).await;
                }
                character = self.move_to(character, bank).await;
                sleep_for_cooldown(&character).await;
                character = self.bank.deposit_items(character).await;
                sleep_for_cooldown(&character).await;
            }
            TaskKind::FightSkill(_) | TaskKind::CraftSkill(_, _) | TaskKind::GatherSkill(_, _) => {}
            TaskKind::CompleteTask => {
                let tasks_master = self.world.nearest(
                    ContentType::TasksMaster,
                    Some("monsters"),
                    character.position,
                );
                character = self.move_to(character, tasks_master).await;
                sleep_for_cooldown(&character).await;
                // This looks a bit odd but accepting a task progress towards
                // completing a task so this is good enough.
                character = match character.task {
                    Some(_task) => {
                        self.server
                            .complete_task(&character.name)
                            .await
                            .unwrap()
                            .character
                    }
                    None => {
                        self.server
                            .accept_task(&character.name)
                            .await
                            .unwrap()
                            .character
                    }
                };
                sleep_for_cooldown(&character).await;
            }
            TaskKind::ExchangeTaskCoins => {
                let bank = self
                    .world
                    .nearest(ContentType::Bank, None, character.position);
                let tasks_master = self.world.nearest(ContentType::TasksMaster, None, bank);
                let bank = self.world.nearest(ContentType::Bank, None, tasks_master);

                character = self.move_to(character, bank).await;
                sleep_for_cooldown(&character).await;
                character = self.bank.deposit_items(character).await;
                sleep_for_cooldown(&character).await;
                let max_items = character.inventory.max_items;
                character = self
                    .bank
                    .withdraw_items(
                        character,
                        ItemStack {
                            code: "tasks_coin".to_owned(),
                            quantity: u32::min(max_items, self.bank.quantity("tasks_coin")),
                        },
                    )
                    .await?;
                sleep_for_cooldown(&character).await;
                character = self.move_to(character, tasks_master).await;
                sleep_for_cooldown(&character).await;
                while character
                    .inventory
                    .items
                    .iter()
                    .flatten()
                    .filter_map(|item| (item.code == "tasks_coin").then_some(item.quantity))
                    .sum::<u32>()
                    >= 3
                {
                    character = self
                        .server
                        .exchange_task_coins(&character.name)
                        .await
                        .unwrap()
                        .character;
                    sleep_for_cooldown(&character).await;
                }
                character = self.move_to(character, bank).await;
                sleep_for_cooldown(&character).await;
                character = self.bank.deposit_items(character).await;
                sleep_for_cooldown(&character).await;
            }
        }
        Ok(character)
    }

    fn compute_dependencies(&self, task: &Task, character: &Character) -> Vec<Task> {
        match &task.kind {
            TaskKind::HaveItem(stack) => {
                let item = &self.items[&stack.code];
                if let Some(craft) = &item.craft {
                    let best_character = match craft.skill {
                        CraftSkill::Mining | CraftSkill::Woodcutting => None,
                        CraftSkill::WeaponCrafting
                        | CraftSkill::GearCrafting
                        | CraftSkill::JewelryCrafting
                        | CraftSkill::Cooking => self
                            .characters
                            .values()
                            .map(|c| c.lock().unwrap())
                            .max_by_key(|c| match craft.skill {
                                CraftSkill::WeaponCrafting => c.weapon_crafting_level.level,
                                CraftSkill::GearCrafting => c.gear_crafting_level.level,
                                CraftSkill::JewelryCrafting => c.jewelry_crafting_level.level,
                                CraftSkill::Cooking => c.cooking_level.level,
                                _ => unreachable!(),
                            })
                            .map(|c| c.name.clone()),
                    };
                    vec![Task {
                        character: best_character,
                        kind: TaskKind::Craft(
                            item.code.clone(),
                            craft.clone(),
                            ((stack.quantity - 1) / craft.quantity) + 1,
                        ),
                    }]
                } else if let Some((resource, _)) = self
                    .resources
                    .values()
                    .filter_map(|resource| {
                        resource
                            .drops
                            .iter()
                            .find(|drop| drop.code == stack.code)
                            .map(|drop| (resource, drop))
                    })
                    .min_by_key(|(_, drop)| drop.rate)
                {
                    vec![Task::anyone(TaskKind::Gather(
                        resource.clone(),
                        Repeat::Items(stack.clone()),
                    ))]
                } else if let Some(monster) = self
                    .monsters
                    .values()
                    .find(|m| m.drops.iter().any(|drop| drop.code == stack.code))
                {
                    vec![Task::anyone(TaskKind::Fight(
                        monster.clone(),
                        Repeat::Items(stack.clone()),
                    ))]
                } else if item.code == "tasks_coin" {
                    vec![Task::anyone(TaskKind::CompleteTask); stack.quantity as usize]
                } else if item.item_subtype == "task" {
                    vec![Task::anyone(TaskKind::ExchangeTaskCoins)]
                } else {
                    panic!("No way to obtain item: {:?}", stack);
                }
            }
            TaskKind::Craft(_item, craft, num_times) => {
                let task_character;
                let character = match &task.character {
                    Some(name) => {
                        task_character = self.characters[name].lock().unwrap().clone();
                        &task_character
                    }
                    None => character,
                };
                craft
                    .items
                    .iter()
                    .map(|stack| {
                        let mut stack = stack.clone();
                        stack.quantity *= num_times;
                        TaskKind::HaveItem(stack)
                    })
                    .chain(
                        (!character.has_craft_skill(craft.skill, craft.level))
                            .then_some(TaskKind::CraftSkill(craft.skill, craft.level)),
                    )
                    .map(|kind| Task {
                        character: Some(character.name.clone()),
                        kind,
                    })
                    .collect()
            }
            TaskKind::Gather(resource, _quantity) => {
                let task_character;
                let character = match &task.character {
                    Some(name) => {
                        task_character = self.characters[name].lock().unwrap().clone();
                        &task_character
                    }
                    None => character,
                };
                let effect_type = match resource.skill {
                    GatherSkill::Mining => EffectType::Mining,
                    GatherSkill::Woodcutting => EffectType::Woodcutting,
                    GatherSkill::Fishing => EffectType::Fishing,
                };
                (!character.has_gather_skill(resource.skill, resource.level))
                    .then_some(TaskKind::GatherSkill(resource.skill, resource.level))
                    .into_iter()
                    .chain(
                        self.best_item_for_effect_level(effect_type, resource.level)
                            .map(|item| {
                                TaskKind::HaveItem(ItemStack {
                                    code: item.code,
                                    quantity: 1,
                                })
                            }),
                    )
                    .map(|kind| Task {
                        character: Some(character.name.clone()),
                        kind,
                    })
                    .collect()
            }
            TaskKind::Fight(monster, repeat) => {
                let task_character;
                let character = match &task.character {
                    Some(name) => {
                        task_character = self.characters[name].lock().unwrap().clone();
                        &task_character
                    }
                    None => character,
                };
                let (load_out, best_turns) = best_load_out(self, character, monster);
                let required_level = load_out
                    .items
                    .values()
                    .flatten()
                    .map(|i| self.items[&i.code].level)
                    .max()
                    .unwrap_or(1);
                load_out
                    .items
                    .values()
                    .flatten()
                    .map(|stack| {
                        let stack = self.multiply_consumable(stack, repeat, monster, best_turns);
                        TaskKind::HaveItem(stack)
                    })
                    .chain(
                        (required_level > character.level.level)
                            .then_some(TaskKind::FightSkill(required_level)),
                    )
                    .map(|kind| Task {
                        character: Some(character.name.clone()),
                        kind,
                    })
                    .collect()
            }
            TaskKind::FightSkill(level) => {
                assert!(task.character.is_some());
                let mut possible_monsters: Vec<_> = self
                    .monsters
                    .values()
                    .filter(|monster| monster.level < *level)
                    .collect();
                let max_level = possible_monsters.iter().map(|m| m.level).max().unwrap();
                possible_monsters.retain(|monster| monster.level == max_level);
                possible_monsters
                    .into_iter()
                    .map(|monster| TaskKind::Fight(monster.clone(), Repeat::Times(5)))
                    .map(|kind| Task {
                        character: task.character.clone(),
                        kind,
                    })
                    .collect()
            }
            TaskKind::CraftSkill(skill, level) => {
                assert!(task.character.is_some());
                let mut possible_crafts: Vec<_> = self
                    .items
                    .values()
                    .filter_map(|item| item.craft.as_ref().map(|c| (&item.code, c)))
                    .filter(|(_, craft)| craft.skill == *skill && craft.level < *level)
                    .collect();
                let max_level = possible_crafts.iter().map(|(_, c)| c.level).max().unwrap();
                possible_crafts.retain(|(_, craft)| craft.level == max_level);
                possible_crafts
                    .into_iter()
                    .map(|(item, craft)| TaskKind::Craft(item.clone(), craft.clone(), 5))
                    .map(|kind| Task {
                        character: task.character.clone(),
                        kind,
                    })
                    .collect()
            }
            TaskKind::GatherSkill(skill, level) => {
                assert!(task.character.is_some());
                let effect_type = match skill {
                    GatherSkill::Mining => EffectType::Mining,
                    GatherSkill::Woodcutting => EffectType::Woodcutting,
                    GatherSkill::Fishing => EffectType::Fishing,
                };
                let gather_item =
                    self.best_item_for_effect_level(effect_type, *level)
                        .map(|item| {
                            TaskKind::HaveItem(ItemStack {
                                code: item.code,
                                quantity: 1,
                            })
                        });

                let resource = self
                    .resources
                    .values()
                    .filter(|r| r.skill == *skill && r.level < *level)
                    .max_by_key(|r| r.level)
                    .unwrap();
                std::iter::once(TaskKind::Gather(resource.clone(), Repeat::Times(5)))
                    .chain(gather_item)
                    .map(|kind| Task {
                        character: task.character.clone(),
                        kind,
                    })
                    .collect()
            }
            TaskKind::CompleteTask => {
                let task = character.task.as_ref().unwrap();
                vec![Task {
                    character: Some(character.name.clone()),
                    kind: TaskKind::Fight(
                        self.monsters[&task.code].clone(),
                        Repeat::Times(task.remaining()),
                    ),
                }]
            }
            TaskKind::ExchangeTaskCoins => vec![Task {
                character: task.character.clone(),
                kind: TaskKind::HaveItem(ItemStack {
                    code: "tasks_coin".to_string(),
                    quantity: 3,
                }),
            }],
        }
    }

    fn multiply_consumable(
        &self,
        stack: &ItemStack,
        repeat: &Repeat,
        monster: &Monster,
        best_turns: u8,
    ) -> ItemStack {
        let item = &self.items[&stack.code];
        let quantity = if item.item_type == ItemType::Consumable {
            if item
                .effects
                .iter()
                .any(|effect| effect.name == EffectType::Restore)
            {
                u32::max(stack.quantity, best_turns as u32) * repeat.estimate_times(monster)
            } else {
                stack.quantity * repeat.estimate_times(monster)
            }
        } else {
            stack.quantity
        };
        let quantity = u32::min(quantity, 100);
        ItemStack {
            code: stack.code.clone(),
            quantity,
        }
    }
}

async fn sleep_for_cooldown(character: &Character) {
    let cooldown_duration = (character.cooldown_expiration + Duration::from_millis(10)
        - OffsetDateTime::now_utc())
    .try_into()
    .unwrap_or(Duration::ZERO);
    tokio::time::sleep(cooldown_duration).await;
}

trait ItemSlots {
    fn get(&self, slot: ItemSlot) -> Option<ItemStack>;
}

impl ItemSlots for Inventory {
    fn get(&self, slot: ItemSlot) -> Option<ItemStack> {
        self.get(slot)
    }
}

struct LoadOut {
    items: EnumMap<ItemSlot, Option<ItemStack>>,
}

impl ItemSlots for LoadOut {
    fn get(&self, slot: ItemSlot) -> Option<ItemStack> {
        self.items[slot].clone()
    }
}

fn can_defeat<I>(game: &Game, load_out: &I, monster: &Monster) -> Option<u8>
where
    I: ItemSlots,
{
    let mut hp = 120;
    let mut attack = Elements {
        fire: 0,
        water: 0,
        earth: 0,
        air: 0,
    };
    let mut damage = Elements {
        fire: 0,
        water: 0,
        earth: 0,
        air: 0,
    };
    let mut resistance = Elements {
        fire: 0,
        water: 0,
        earth: 0,
        air: 0,
    };
    let mut restores = vec![];
    let mut level = 0;
    for slot in ItemSlot::iter() {
        let item = load_out.get(slot);
        if let Some(item_stack) = item {
            let item = &game.items[&item_stack.code];
            level = level.max(item.level);
            for effect in &item.effects {
                match effect.name {
                    EffectType::Hp | EffectType::BoostHp => hp += effect.value,
                    EffectType::FireAttack => attack.fire += effect.value,
                    EffectType::WaterAttack => attack.water += effect.value,
                    EffectType::EarthAttack => attack.earth += effect.value,
                    EffectType::AirAttack => attack.air += effect.value,
                    EffectType::FireDamage | EffectType::BoostFireDamage => {
                        damage.fire += effect.value
                    }
                    EffectType::WaterDamage | EffectType::BoostWaterDamage => {
                        damage.water += effect.value
                    }
                    EffectType::EarthDamage | EffectType::BoostEarthDamage => {
                        damage.earth += effect.value
                    }
                    EffectType::AirDamage | EffectType::BoostAirDamage => {
                        damage.air += effect.value
                    }
                    EffectType::FireResistance => resistance.fire += effect.value,
                    EffectType::WaterResistance => resistance.water += effect.value,
                    EffectType::EarthResistance => resistance.earth += effect.value,
                    EffectType::AirResistance => resistance.air += effect.value,
                    EffectType::Restore => restores.push((item_stack.quantity, effect.value)),
                    _ => {}
                }
            }
        }
    }
    hp += 5 * level as i32;
    let starting_hp = hp;

    let mut monster_hp = monster.hp as i32;
    let attack = attack.fire as f32
        * (1.0 + damage.fire as f32 * 0.01 - 0.01 * monster.resistance.fire as f32)
        + attack.water as f32
            * (1.0 + damage.water as f32 * 0.01 - 0.01 * monster.resistance.water as f32)
        + attack.earth as f32
            * (1.0 + damage.earth as f32 * 0.01 - 0.01 * monster.resistance.earth as f32)
        + attack.air as f32
            * (1.0 + damage.air as f32 * 0.01 - 0.01 * monster.resistance.air as f32);
    let attack = attack.round() as i32;
    let monster_attack = monster.attack.fire as f32 * (1.0 - 0.01 * resistance.fire as f32)
        + monster.attack.water as f32 * (1.0 - 0.01 * resistance.water as f32)
        + monster.attack.earth as f32 * (1.0 - 0.01 * resistance.earth as f32)
        + monster.attack.air as f32 * (1.0 - 0.01 * resistance.air as f32);
    let monster_attack = monster_attack.round() as i32;
    for round in 0..25 {
        if hp < starting_hp / 2 {
            for (quantity, value) in &mut restores {
                if *quantity > 0 {
                    hp += *value;
                    *quantity -= 1;
                }
            }
        }
        monster_hp -= attack;
        if monster_hp <= 0 {
            return Some(round);
        }
        hp -= monster_attack;
        if hp <= 0 {
            return None;
        }
    }
    None
}

fn best_load_out(game: &Game, character: &Character, monster: &Monster) -> (LoadOut, u8) {
    let mut best_load_out = LoadOut {
        items: EnumMap::default(),
    };
    if monster.level == 1 {
        best_load_out.items[ItemSlot::Weapon] = Some(ItemStack {
            code: "copper_dagger".to_owned(),
            quantity: 1,
        });
        let turns = can_defeat(game, &best_load_out, monster).unwrap();
        return (best_load_out, turns);
    }
    let mut attack = Elements {
        fire: 0,
        water: 0,
        earth: 0,
        air: 0,
    };
    for slot in ItemSlot::iter() {
        let mut possible_items: Vec<_> = game
            .items
            .values()
            .filter(|item| {
                item.item_type == slot.item_type()
                    && !best_load_out
                        .items
                        .values()
                        .flatten()
                        .any(|i| i.code == item.code)
                    && game.item_is_obtainable(item, monster, character)
                    && item
                        .effects
                        .iter()
                        .all(|effect| effect.name != EffectType::Restore)
            })
            .collect();
        possible_items.sort_by_cached_key(|item| {
            item.effects
                .iter()
                .map(|effect| match effect.name {
                    EffectType::Restore => 0,
                    EffectType::Hp | EffectType::BoostHp => effect.value * 10,
                    EffectType::FireDamage | EffectType::BoostFireDamage => {
                        attack.fire * effect.value
                    }
                    EffectType::EarthDamage | EffectType::BoostEarthDamage => {
                        attack.earth * effect.value
                    }
                    EffectType::WaterDamage | EffectType::BoostWaterDamage => {
                        attack.water * effect.value
                    }
                    EffectType::AirDamage | EffectType::BoostAirDamage => attack.air * effect.value,
                    EffectType::FireResistance => monster.attack.fire * effect.value,
                    EffectType::EarthResistance => monster.attack.earth * effect.value,
                    EffectType::WaterResistance => monster.attack.water * effect.value,
                    EffectType::AirResistance => monster.attack.air * effect.value,
                    EffectType::FireAttack => effect.value * (100 - monster.resistance.fire),
                    EffectType::EarthAttack => effect.value * (100 - monster.resistance.earth),
                    EffectType::WaterAttack => effect.value * (100 - monster.resistance.water),
                    EffectType::AirAttack => effect.value * (100 - monster.resistance.air),
                    EffectType::Haste
                    | EffectType::Mining
                    | EffectType::Woodcutting
                    | EffectType::Fishing => 0,
                })
                .sum::<i32>()
        });
        let Some(item) = possible_items.pop() else {
            continue;
        };
        best_load_out.items[slot] = Some(ItemStack {
            code: item.code.clone(),
            quantity: 1,
        });
        for effect in &item.effects {
            match effect.name {
                EffectType::FireAttack => attack.fire += effect.value,
                EffectType::WaterAttack => attack.water += effect.value,
                EffectType::EarthAttack => attack.earth += effect.value,
                EffectType::AirAttack => attack.air += effect.value,
                _ => {}
            }
        }
    }
    let consumable_1 = best_load_out.items[ItemSlot::Consumable1].take();
    let consumable_2 = best_load_out.items[ItemSlot::Consumable2].take();
    // Try without consumables first
    if let Some(turns) = can_defeat(game, &best_load_out, monster) {
        return (best_load_out, turns);
    }
    best_load_out.items[ItemSlot::Consumable1] = consumable_1;
    best_load_out.items[ItemSlot::Consumable2] = consumable_2;
    // Fine...use consumables
    if let Some(turns) = can_defeat(game, &best_load_out, monster) {
        return (best_load_out, turns);
    }
    // Oh no, need restoratives.
    let mut restoratives = game
        .items
        .values()
        .filter(|item| {
            item.item_type == ItemType::Consumable
                && game.item_is_obtainable(item, monster, character)
        })
        .filter_map(|item| {
            item.effects
                .iter()
                .find(|effect| effect.name == EffectType::Restore)
                .map(|effect| (item, effect))
        })
        .collect::<Vec<_>>();
    restoratives.sort_by_key(|(_, effect)| effect.value);
    for slot in [ItemSlot::Consumable2, ItemSlot::Consumable1] {
        let restorative = restoratives.pop().unwrap();
        for count in 0..25 {
            best_load_out.items[slot] = Some(ItemStack {
                code: restorative.0.code.clone(),
                quantity: count + 1,
            });
            if let Some(turns) = can_defeat(game, &best_load_out, monster) {
                return (best_load_out, turns);
            }
        }
    }
    panic!("No load out found");
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Task {
    character: Option<Name>,
    limits: Limits,
    kind: TaskKind,
}

impl Task {
    pub fn is_for(&self, character: &Character) -> bool {
        self.character
            .as_ref()
            .is_none_or(|name| name == &character.name)
    }
}

impl fmt::Display for Task {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(character) = &self.character {
            write!(f, "({}) ", character)?;
        }
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct Limits {
    fight_skill: Option<u32>,

}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TaskKind {
    HaveItem(ItemStack),
    Craft(String, Craft, u32),
    Gather(Resource, Repeat),
    Fight(Monster, Repeat),
    FightSkill(u32),
    CraftSkill(CraftSkill, u32),
    GatherSkill(GatherSkill, u32),
    CompleteTask,
    ExchangeTaskCoins,
}

impl fmt::Display for TaskKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TaskKind::HaveItem(stack) => write!(f, "Have {} {}", stack.quantity, stack.code),
            TaskKind::Craft(code, _, num_times) => write!(f, "Craft {} x{}", code, num_times),
            TaskKind::Gather(resource, Repeat::Items(items)) => {
                write!(
                    f,
                    "Gather {} {} at {}",
                    items.quantity, items.code, resource.code
                )
            }
            TaskKind::Gather(resource, Repeat::Times(times)) => {
                write!(f, "Gather {} {}", times, resource.code)
            }
            TaskKind::Fight(monster, Repeat::Times(times)) => {
                write!(f, "Fight {} {}", times, monster.name)
            }
            TaskKind::Fight(monster, Repeat::Items(items)) => write!(
                f,
                "Fight {} for {} {}",
                monster.code, items.quantity, items.code
            ),
            TaskKind::FightSkill(level) => write!(f, "Level up to level {}", level),
            TaskKind::CraftSkill(skill, level) => {
                write!(f, "Level up {} to level {}", skill, level)
            }
            TaskKind::GatherSkill(skill, level) => {
                write!(f, "Level up {} to level {}", skill, level)
            }
            TaskKind::CompleteTask => write!(f, "Complete task"),
            TaskKind::ExchangeTaskCoins => write!(f, "Exchange task coins"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Repeat {
    Times(u32),
    Items(ItemStack),
}
impl Repeat {
    fn count(&self) -> u32 {
        match self {
            Repeat::Times(times) => *times,
            Repeat::Items(stack) => stack.quantity,
        }
    }

    fn estimate_times(&self, monster: &Monster) -> u32 {
        match self {
            Repeat::Times(times) => *times,
            Repeat::Items(stack) => {
                let drop = monster
                    .drops
                    .iter()
                    .find(|item| item.code == stack.code)
                    .unwrap();
                stack.quantity * drop.rate * 2 / (drop.max_quantity + drop.min_quantity)
            }
        }
    }
}

struct DependencyInfo<T> {
    value: T,
    parent: Option<DefaultKey>,
    dependencies: HashSet<DefaultKey>,
}

struct TaskTree {
    tasks: SlotMap<DefaultKey, DependencyInfo<Task>>,
    available: LinkedHashMap<DefaultKey, ()>,
}

impl TaskTree {
    fn new() -> Self {
        Self {
            tasks: SlotMap::new(),
            available: LinkedHashMap::new(),
        }
    }

    fn add_root(&mut self, task: Task) -> DefaultKey {
        let key = self.tasks.insert(DependencyInfo {
            value: task,
            parent: None,
            dependencies: HashSet::new(),
        });
        self.available.insert(key, ());
        key
    }

    fn find_available(&mut self, mut f: impl FnMut(&Task) -> bool) -> Option<DefaultKey> {
        let available = self.available.iter().find_map(|(&key, ())| {
            if f(&self.tasks[key].value) {
                Some(key)
            } else {
                None
            }
        });
        if let Some(available) = &available {
            self.available.remove(available);
        }
        available
    }

    fn pop_available(&mut self, mut available: impl FnMut(&Task) -> bool) -> Option<DefaultKey> {
        let key = self
            .available
            .iter()
            .rev()
            .find(|(&task, ())| available(&self.tasks[task].value))
            .map(|(&key, ())| key);
        if let Some(key) = &key {
            self.available.remove(key);
        }
        key
    }

    fn complete_task(&mut self, key: DefaultKey) {
        let Some(task) = self.tasks.remove(key) else {
            return;
        };
        if let Some(parent_key) = task.parent {
            let parent = &mut self.tasks[parent_key];
            parent.dependencies.remove(&key);
            if parent.dependencies.is_empty() {
                self.available.insert(parent_key, ());
            }
        }
    }

    fn add_dependencies(&mut self, key: DefaultKey, dependencies: impl IntoIterator<Item = Task>) {
        // It shouldn't be there but it doesn't hurt to make sure.
        self.available.remove(&key);
        for dependency in dependencies {
            let dependency_key = self.tasks.insert(DependencyInfo {
                value: dependency,
                parent: Some(key),
                dependencies: HashSet::new(),
            });
            self.tasks[key].dependencies.insert(dependency_key);
            self.available.insert(dependency_key, ());
        }
    }
}

impl Index<DefaultKey> for TaskTree {
    type Output = Task;

    fn index(&self, key: DefaultKey) -> &Self::Output {
        &self.tasks[key].value
    }
}
impl IndexMut<DefaultKey> for TaskTree {
    fn index_mut(&mut self, key: DefaultKey) -> &mut Self::Output {
        &mut self.tasks[key].value
    }
}

#[derive(Debug)]
struct Bank {
    server: Server,
    items: Arc<Mutex<HashMap<String, ItemStack>>>,
}

impl Bank {
    async fn new(server: Server) -> Self {
        let items = server
            .bank_items()
            .await
            .unwrap()
            .into_iter()
            .map(|stack| (stack.code.clone(), stack))
            .collect();
        Self {
            server,
            items: Arc::new(Mutex::new(items)),
        }
    }

    fn quantity(&self, code: &str) -> u32 {
        self.items
            .lock()
            .unwrap()
            .get(code)
            .map(|b| b.quantity)
            .unwrap_or(0)
    }

    async fn withdraw_items(&self, character: Character, stack: ItemStack) -> RCharacter {
        let res = match self
            .server
            .withdraw_items(&character.name, stack.clone())
            .await
        {
            Ok(res) => res,
            Err(ApiError::ClientError(status))
                if status.as_u16() == 478 || status.as_u16() == 404 =>
            {
                return Err(character)
            }
            Err(e) => panic!(
                "Failed to withdraw {} {}: {}",
                stack.quantity, stack.code, e
            ),
        };
        *self.items.lock().unwrap() = res
            .bank
            .into_iter()
            .map(|stack| (stack.code.clone(), stack))
            .collect();
        Ok(res.character)
    }

    async fn deposit_items(&self, mut character: Character) -> Character {
        for stack in character.inventory.items.clone() {
            let Some(stack) = stack else {
                continue;
            };
            character = self
                .server
                .deposit_items(&character.name, stack)
                .await
                .unwrap()
                .character;
            sleep_for_cooldown(&character).await;
        }

        *self.items.lock().unwrap() = self
            .server
            .bank_items()
            .await
            .unwrap()
            .into_iter()
            .map(|stack| (stack.code.clone(), stack))
            .collect();
        character
    }
}

#[derive(Debug)]
struct World {
    maps: Vec<Map>,
}

impl World {
    fn new(maps: Vec<Map>) -> Self {
        Self { maps }
    }

    fn nearest(
        &self,
        content_type: ContentType,
        subtype: Option<&str>,
        position: Position,
    ) -> Position {
        self.maps
            .iter()
            .filter(|map| {
                map.content.as_ref().is_some_and(|content| {
                    content.content_type == content_type
                        && subtype.is_none_or(|subtype| subtype == content.code)
                })
            })
            .min_by_key(|map| {
                let dx = map.position.x - position.x;
                let dy = map.position.y - position.y;
                dx * dx + dy * dy
            })
            .unwrap()
            .position
    }
}
