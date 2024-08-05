#![feature(is_none_or, duration_constructors, let_chains)]

use core::fmt;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    convert::identity,
    error::Error,
    ops::{Index, IndexMut},
    sync::{Arc, Mutex},
    time::Duration,
    vec,
};

use enum_map::EnumMap;
use sdk::{
    ApiError, Character, ContentType, Craft, CraftSkill, EffectType, Elements, FightResult,
    GatherSkill, Inventory, Item, ItemSlot, ItemStack, ItemType, Map, Monster, Name, Position,
    Resource, Server,
};
use slotmap::{DefaultKey, SlotMap};
use strum::IntoEnumIterator;

#[tokio::main]
async fn main() {
    let server = Server::new(std::env::var("API_TOKEN").unwrap()).await;
    let game = Game::new(server).await;
    game.complete(Task {
        character: None,
        limits: Limits::default(),
        kind: TaskKind::HaveItem(ItemStack {
            code: "lich_crown".to_string(),
            quantity: 1,
        }),
    })
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
        let world = World::new(server.clone()).await;
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
                        self.sleep_for_cooldown(&character).await;
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
                        } else if let Some(task) = tasks.find_needs_dependencies() {
                            let task_data = &tasks[task];
                            let dependencies = self.compute_dependencies(task_data, &character);
                            println!("\nIn order to complete: {}", task_data);
                            println!("We need the following dependencies:");
                            for dependency in &dependencies {
                                println!(
                                    "- ({}) {}",
                                    dependency.character.as_deref().unwrap_or("anyone"),
                                    dependency.kind
                                );
                            }
                            println!();
                            tasks.add_dependencies(task, dependencies);
                        } else {
                            s.spawn(async {
                                println!("{} has no tasks to complete", character.name);
                                tokio::time::sleep(Duration::from_secs(60)).await;
                                tx.send(character).await.unwrap();
                            });
                            continue 'outer;
                        }
                    };
                    let task_data = tasks.lock().unwrap()[task].clone();
                    println!("{} has picked up: {}", character.name, task_data.kind);
                    let tasks = tasks.clone();
                    s.spawn(async move {
                        let character = self
                            .immediately_complete(character, task_data.clone())
                            .await;
                        match &character {
                            Ok(character) => {
                                println!("{} has completed: {}", character.name, task_data.kind)
                            }
                            Err(character) => {
                                println!(
                                    "{} failed to complete: {}",
                                    character.name, task_data.kind
                                )
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

    fn best_gather_item_in_limits(
        &self,
        gather_skill: GatherSkill,
        limits: &Limits,
    ) -> Option<Item> {
        let effect_type = match gather_skill {
            GatherSkill::Mining => EffectType::Mining,
            GatherSkill::Woodcutting => EffectType::Woodcutting,
            GatherSkill::Fishing => EffectType::Fishing,
        };
        self.items
            .values()
            .filter_map(|item| {
                item.effects
                    .iter()
                    .find(|effect| effect.name == effect_type)
                    .map(|effect| (item, effect))
            })
            .filter(|(item, _)| {
                let can_equip = limits.can_equip(item, None);
                let is_obtainable = self.item_is_obtainable(item, limits);
                can_equip && is_obtainable
            })
            .min_by_key(|(_, effect)| effect.value)
            .map(|(item, _effect)| item.clone())
    }

    fn item_is_obtainable(&self, item: &Item, limits: &Limits) -> bool {
        let stack = ItemStack {
            code: item.code.clone(),
            quantity: 1,
        };
        let Ok(dependencies) = self.item_dependencies(&stack, limits) else {
            return false;
        };
        for dependency in dependencies {
            let obtainable = match dependency.kind {
                TaskKind::HaveItem(stack) => {
                    self.item_is_obtainable(&self.items[&stack.code], limits)
                }
                TaskKind::Craft(code, craft, _) => {
                    dependency.limits.can_craft(&self.items[&code]).is_some()
                        && craft
                            .items
                            .into_iter()
                            .all(|item| self.item_is_obtainable(&self.items[&item.code], limits))
                }
                TaskKind::Fight(monster, _) => limits.can_fight(&monster),
                TaskKind::Gather(resource, _) => dependency.limits.can_gather(&resource),
                TaskKind::FightSkill(level) => dependency.limits.can_obtain_fight_skill(level),
                TaskKind::CraftSkill(skill, level) => {
                    dependency.limits.can_obtain_craft_skill(skill, level)
                }
                TaskKind::GatherSkill(skill, level) => {
                    dependency.limits.can_obtain_gather_skill(skill, level)
                }
                TaskKind::CompleteTask | TaskKind::ExchangeTaskCoins => {
                    self.characters.values().any(|character| {
                        character.lock().unwrap().task.as_ref().is_none_or(|task| {
                            dependency
                                .limits
                                .can_complete_tasks(&self.monsters[&task.code])
                        })
                    })
                }
            };
            if !obtainable {
                return false;
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
                &TaskKind::CraftSkill(skill, level) => level <= character.craft_skill(skill),
                &TaskKind::GatherSkill(skill, level) => level <= character.gather_skill(skill),
                TaskKind::CompleteTask => character
                    .task
                    .as_ref()
                    .is_none_or(|task| task.remaining() == 0),
                TaskKind::ExchangeTaskCoins => return None,
            })
        }
        if !task.is_for(character) {
            return false;
        }
        if let Some(completed) = is_completed(self, character, &task.kind) {
            return completed;
        }
        self.compute_dependencies(task, character)
            .iter()
            .all(|task| is_completed(self, character, &task.kind).unwrap())
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
                            .nearest(ContentType::Bank, None, character.position)
                            .unwrap();
                        character = self.move_to(character, bank).await;
                        self.sleep_for_cooldown(&character).await;
                        character = self.bank.deposit_all_items(character).await;
                        self.sleep_for_cooldown(&character).await;
                        for item in craft.items.iter() {
                            let stack = ItemStack {
                                code: item.code.clone(),
                                quantity: item.quantity * num_crafts,
                            };
                            character = self.bank.withdraw_items(character, stack).await?;
                            self.sleep_for_cooldown(&character).await;
                        }
                    }

                    let workshop = self
                        .world
                        .nearest(
                            ContentType::Workshop,
                            Some(&craft.skill.to_string()),
                            character.position,
                        )
                        .unwrap();
                    character = self.move_to(character, workshop).await;
                    self.sleep_for_cooldown(&character).await;
                    character = self
                        .server
                        .craft(&character.name, code.clone(), num_crafts)
                        .await
                        .unwrap()
                        .character;
                    num_times -= num_crafts;
                    self.sleep_for_cooldown(&character).await;
                    // If it's the last one and it's for us then we can keep the items.
                    if num_times > 0 || task.character.is_none() {
                        let bank = self
                            .world
                            .nearest(ContentType::Bank, None, character.position)
                            .unwrap();
                        character = self.move_to(character, bank).await;
                        self.sleep_for_cooldown(&character).await;
                        character = self.bank.deposit_all_items(character).await;
                        self.sleep_for_cooldown(&character).await;
                    }
                }
            }
            TaskKind::Gather(resource, mut repeat) => {
                let bank = self
                    .world
                    .nearest(ContentType::Bank, None, character.position)
                    .unwrap();
                let gather_pos = self
                    .world
                    .nearest(ContentType::Resource, Some(&resource.code), bank)
                    .unwrap();
                let bank = self
                    .world
                    .nearest(ContentType::Bank, None, gather_pos)
                    .unwrap();

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
                            self.sleep_for_cooldown(&character).await;
                            character = self.bank.deposit_all_items(character).await;
                            self.sleep_for_cooldown(&character).await;
                            character =
                                self.bank.withdraw_items(character, best_item_stack).await?;
                            self.sleep_for_cooldown(&character).await;
                        }
                        if item_to_unequip {
                            character = self
                                .server
                                .unequip_item(&character.name, ItemSlot::Weapon)
                                .await
                                .unwrap()
                                .character;
                            self.sleep_for_cooldown(&character).await;
                        }
                        character = self
                            .server
                            .equip_item(&character.name, &best_item.code, ItemSlot::Weapon)
                            .await
                            .unwrap()
                            .character;
                        self.sleep_for_cooldown(&character).await;
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
                        self.sleep_for_cooldown(&character).await;
                        character = self.bank.deposit_all_items(character).await;
                        self.sleep_for_cooldown(&character).await;
                    }
                    character = self.move_to(character, gather_pos).await;
                    self.sleep_for_cooldown(&character).await;
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
                    self.sleep_for_cooldown(&character).await;
                }
                if task.character.is_none() {
                    character = self.move_to(character, bank).await;
                    self.sleep_for_cooldown(&character).await;
                    character = self.bank.deposit_all_items(character).await;
                    self.sleep_for_cooldown(&character).await;
                }
            }
            TaskKind::Fight(monster, repeat) => {
                let bank = self
                    .world
                    .nearest(ContentType::Bank, None, character.position)
                    .unwrap();
                let Some(monster_pos) =
                    self.world
                        .nearest(ContentType::Monster, Some(&monster.code), bank)
                else {
                    return Err(character);
                };
                let bank = self
                    .world
                    .nearest(ContentType::Bank, None, monster_pos)
                    .unwrap();

                let (load_out, best_turns) = best_load_out(self, &monster, |item, slot| {
                    let is_current_item = character
                        .inventory
                        .get(slot)
                        .is_some_and(|i| item.code == i.code);
                    let item_is_available = self.has_items(
                        &character,
                        &ItemStack {
                            code: item.code.clone(),
                            quantity: 1,
                        },
                    );
                    item.level < character.level.level && (is_current_item || item_is_available)
                })
                .unwrap();

                let mut remaining = repeat;
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
                                    self.sleep_for_cooldown(&character).await;
                                    character = self.bank.deposit_all_items(character).await;
                                    self.sleep_for_cooldown(&character).await;
                                }
                                if character_item.is_some() {
                                    character = self
                                        .server
                                        .unequip_item(&character.name, slot)
                                        .await
                                        .unwrap()
                                        .character;
                                    self.sleep_for_cooldown(&character).await;
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
                                        self.sleep_for_cooldown(&character).await;
                                        if !character.inventory.has_space_for(&needs) {
                                            character =
                                                self.bank.deposit_all_items(character).await;
                                            self.sleep_for_cooldown(&character).await;
                                        }
                                        character =
                                            self.bank.withdraw_items(character, needs).await?;
                                        self.sleep_for_cooldown(&character).await;
                                    }
                                    character = self
                                        .server
                                        .equip_item(&character.name, &stack.code, slot)
                                        .await
                                        .unwrap()
                                        .character;
                                    self.sleep_for_cooldown(&character).await;
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
                        self.sleep_for_cooldown(&character).await;
                        character = self.bank.deposit_all_items(character).await;
                        self.sleep_for_cooldown(&character).await;
                    }
                    character = self.move_to(character, monster_pos).await;
                    self.sleep_for_cooldown(&character).await;
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
                    self.sleep_for_cooldown(&character).await;
                }
                character = self.move_to(character, bank).await;
                self.sleep_for_cooldown(&character).await;
                character = self.bank.deposit_all_items(character).await;
                self.sleep_for_cooldown(&character).await;
            }
            TaskKind::FightSkill(_) | TaskKind::CraftSkill(_, _) | TaskKind::GatherSkill(_, _) => {}
            TaskKind::CompleteTask => {
                let tasks_master = self
                    .world
                    .nearest(
                        ContentType::TasksMaster,
                        Some("monsters"),
                        character.position,
                    )
                    .unwrap();
                character = self.move_to(character, tasks_master).await;
                self.sleep_for_cooldown(&character).await;
                // This looks a bit odd but accepting a task progress towards
                // completing a task so this is good enough.
                match character.task {
                    Some(_task) => {
                        character = self
                            .server
                            .complete_task(&character.name)
                            .await
                            .unwrap()
                            .character;
                        self.sleep_for_cooldown(&character).await;
                    }
                    None => {
                        character = self
                            .server
                            .accept_task(&character.name)
                            .await
                            .unwrap()
                            .character;
                        self.sleep_for_cooldown(&character).await;
                        let bank = self
                            .world
                            .nearest(ContentType::Bank, None, character.position)
                            .unwrap();
                        character = self.move_to(character, bank).await;
                        self.sleep_for_cooldown(&character).await;
                        character = self.bank.deposit_all_items(character).await;
                        self.sleep_for_cooldown(&character).await;
                    }
                }
            }
            TaskKind::ExchangeTaskCoins => {
                let bank = self
                    .world
                    .nearest(ContentType::Bank, None, character.position)
                    .unwrap();
                let tasks_master = self
                    .world
                    .nearest(ContentType::TasksMaster, None, bank)
                    .unwrap();
                let bank = self
                    .world
                    .nearest(ContentType::Bank, None, tasks_master)
                    .unwrap();

                character = self.move_to(character, bank).await;
                self.sleep_for_cooldown(&character).await;
                character = self.bank.deposit_all_items(character).await;
                self.sleep_for_cooldown(&character).await;
                if self.bank.quantity("tasks_coin") > 0 {
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
                    self.sleep_for_cooldown(&character).await;
                }
                character = self.move_to(character, tasks_master).await;
                self.sleep_for_cooldown(&character).await;
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
                    self.sleep_for_cooldown(&character).await;
                }
                character = self.move_to(character, bank).await;
                self.sleep_for_cooldown(&character).await;
                character = self.bank.deposit_all_items(character).await;
                self.sleep_for_cooldown(&character).await;
            }
        }
        Ok(character)
    }

    fn compute_dependencies(&self, task: &Task, character: &Character) -> Vec<Task> {
        match &task.kind {
            TaskKind::HaveItem(stack) => {
                if self.bank.quantity(&stack.code) >= stack.quantity {
                    return Vec::new();
                }
                match self.item_dependencies(stack, &task.limits) {
                    Ok(dependencies) => dependencies,
                    Err(err) => {
                        println!("Failed to compute dependencies for {}:\n{}", task, err);
                        panic!("No way to obtain item");
                    }
                }
            }
            TaskKind::Craft(_, craft, num_times) => {
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
                        (character.craft_skill(craft.skill) < craft.level)
                            .then_some(TaskKind::CraftSkill(craft.skill, craft.level)),
                    )
                    .map(|kind| Task {
                        character: Some(character.name.clone()),
                        limits: task.limits.clone(),
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
                let mut tasks = Vec::new();
                if character.gather_skill(resource.skill) < resource.level {
                    tasks.push(Task {
                        character: Some(character.name.clone()),
                        limits: task.limits.clone(),
                        kind: TaskKind::GatherSkill(resource.skill, resource.level),
                    });
                }
                if let Some(gather_item) =
                    self.best_gather_item_in_limits(resource.skill, &task.limits)
                {
                    tasks.push(Task {
                        character: Some(character.name.clone()),
                        limits: task.limits.limit_equip_item(&gather_item),
                        kind: TaskKind::HaveItem(ItemStack {
                            code: gather_item.code,
                            quantity: 1,
                        }),
                    });
                    if character.level.level < gather_item.level {
                        tasks.push(Task {
                            character: Some(character.name.clone()),
                            limits: task.limits.clone(),
                            kind: TaskKind::FightSkill(gather_item.level),
                        });
                    }
                }
                tasks
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
                let (load_out, best_turns) = best_load_out(self, monster, |item, _slot| {
                    task.limits.can_equip(item, Some(monster))
                        && self.item_is_obtainable(
                            item,
                            &task.limits.limit_fight_monster(monster, item.level),
                        )
                })
                .unwrap();
                let required_level = load_out
                    .items
                    .values()
                    .flatten()
                    .map(|i| self.items[&i.code].level)
                    .max()
                    .unwrap_or(1);
                let limits = task.limits.limit_fight_monster(monster, required_level);
                load_out
                    .items
                    .into_iter()
                    .flat_map(|(slot, stack)| stack.map(|stack| (slot, stack)))
                    .filter_map(|(slot, stack)| {
                        let stack = self.multiply_consumable(&stack, repeat, monster, best_turns);
                        if character
                            .inventory
                            .get(slot)
                            .is_some_and(|slot| slot.has(&stack))
                        {
                            return None;
                        }
                        Some(Task {
                            character: Some(character.name.clone()),
                            limits: limits.clone(),
                            kind: TaskKind::HaveItem(stack),
                        })
                    })
                    .chain((character.level.level < required_level).then_some(Task {
                        character: Some(character.name.clone()),
                        limits: task.limits.clone(),
                        kind: TaskKind::FightSkill(required_level),
                    }))
                    .collect()
            }
            &TaskKind::FightSkill(level) => {
                assert!(task.character.is_some());
                let limits = task.limits.limit_fight_skill(level);
                let mut possible_monsters: Vec<_> = self
                    .monsters
                    .values()
                    .filter(|monster| {
                        best_load_out(self, monster, |item, _slot| {
                            limits.can_equip(item, Some(monster))
                                && self.item_is_obtainable(
                                    item,
                                    &limits.limit_fight_monster(monster, item.level),
                                )
                        })
                        .is_some()
                    })
                    .collect();
                let max_level = possible_monsters.iter().map(|m| m.level).max().unwrap();
                possible_monsters.retain(|monster| monster.level == max_level);
                possible_monsters
                    .into_iter()
                    .map(|monster| Task {
                        character: task.character.clone(),
                        limits: limits.clone(),
                        kind: TaskKind::Fight(monster.clone(), Repeat::Times(1)),
                    })
                    .collect()
            }
            &TaskKind::CraftSkill(skill, level) => {
                assert!(task.character.is_some());
                let limits = task.limits.limit_craft_skill(skill, level);
                let mut possible_crafts: Vec<_> =
                    self.items
                        .values()
                        .filter_map(|item| limits.can_craft(item).map(|craft| (item, craft)))
                        .filter(|(_, craft)| craft.skill == skill && craft.level < level)
                        .filter(|(_, craft)| {
                            craft.items.iter().all(|item| {
                                self.item_is_obtainable(&self.items[&item.code], &limits)
                            })
                        })
                        .collect();
                let max_level = possible_crafts.iter().map(|(_, c)| c.level).max().unwrap();
                possible_crafts.retain(|(_, craft)| craft.level == max_level);
                possible_crafts
                    .into_iter()
                    .map(|(item, craft)| TaskKind::Craft(item.code.clone(), craft.clone(), 5))
                    .map(|kind| Task {
                        character: task.character.clone(),
                        limits: limits.clone(),
                        kind,
                    })
                    .collect()
            }
            &TaskKind::GatherSkill(skill, level) => {
                assert!(task.character.is_some());
                let limits = task.limits.limit_gather_skill(skill, level);
                let resource = self
                    .resources
                    .values()
                    .filter(|r| r.skill == skill && limits.can_gather(r))
                    .max_by_key(|r| r.level)
                    .unwrap();
                vec![Task {
                    character: task.character.clone(),
                    limits: limits.clone(),
                    kind: TaskKind::Gather(resource.clone(), Repeat::Times(5)),
                }]
            }
            TaskKind::CompleteTask => {
                let character_task = character.task.as_ref().unwrap();
                let monster = self.monsters[&character_task.code].clone();
                let limits = task.limits.limit_complete_task(&monster);
                vec![Task {
                    character: Some(character.name.clone()),
                    limits,
                    kind: TaskKind::Fight(monster, Repeat::Times(character_task.remaining())),
                }]
            }
            TaskKind::ExchangeTaskCoins => vec![Task {
                character: task.character.clone(),
                limits: task.limits.clone(),
                kind: TaskKind::HaveItem(ItemStack {
                    code: "tasks_coin".to_string(),
                    quantity: 3,
                }),
            }],
        }
    }

    fn item_dependencies(
        &self,
        stack: &ItemStack,
        limits: &Limits,
    ) -> Result<Vec<Task>, Impossible> {
        let item = &self.items[&stack.code];
        let tasks = if let Some(craft) = limits.can_craft(item) {
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
                limits: limits.clone(),
                kind: TaskKind::Craft(
                    item.code.clone(),
                    craft.clone(),
                    ((stack.quantity - 1) / craft.quantity) + 1,
                ),
            }]
        } else if let Some((resource, _)) = self
            .resources
            .values()
            .filter(|resource| limits.can_gather(resource))
            .filter_map(|resource| {
                resource
                    .drops
                    .iter()
                    .find(|drop| drop.code == stack.code)
                    .map(|drop| (resource, drop))
            })
            .min_by_key(|(_, drop)| drop.rate)
        {
            vec![Task {
                character: None,
                limits: limits.clone(),
                kind: TaskKind::Gather(resource.clone(), Repeat::Items(stack.clone())),
            }]
        } else if let Some((monster, _)) = self
            .monsters
            .values()
            .filter_map(|m| {
                m.drops
                    .iter()
                    .find(|drop| drop.code == stack.code)
                    .map(|d| (m, d))
            })
            .filter(|(monster, _drop)| limits.can_fight(monster))
            .min_by_key(|(monster, d)| (d.rate, monster.level))
        {
            vec![Task {
                character: None,
                limits: limits.clone(),
                kind: TaskKind::Fight(monster.clone(), Repeat::Items(stack.clone())),
            }]
        } else if item.code == "tasks_coin" {
            vec![
                Task {
                    character: None,
                    limits: limits.clone(),
                    kind: TaskKind::CompleteTask,
                };
                stack.quantity as usize
            ]
        } else if item.item_subtype == "task"
            && self.characters.values().any(|character| {
                character
                    .lock()
                    .unwrap()
                    .task
                    .as_ref()
                    .is_none_or(|task| limits.can_complete_tasks(&self.monsters[&task.code]))
            })
        {
            vec![Task {
                character: None,
                limits: limits.clone(),
                kind: TaskKind::ExchangeTaskCoins,
            }]
        } else {
            return Err(Impossible(format!(
                "No way to obtain item: {}\n{}",
                stack.code, limits
            )));
        };
        Ok(tasks)
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

    async fn sleep_for_cooldown(&self, character: &Character) {
        tokio::time::sleep_until(
            self.server
                .instant_for(character.cooldown_expiration)
                .into(),
        )
        .await;
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Impossible(String);

impl fmt::Display for Impossible {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Task is impossible because {}", self.0)
    }
}
impl Error for Impossible {}

trait ItemSlots {
    fn get(&self, slot: ItemSlot) -> Option<ItemStack>;
}

impl ItemSlots for Inventory {
    fn get(&self, slot: ItemSlot) -> Option<ItemStack> {
        self.get(slot)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    for round in 0..50 {
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

fn best_load_out(
    game: &Game,
    monster: &Monster,
    is_item_allowed: impl Fn(&Item, ItemSlot) -> bool,
) -> Option<(LoadOut, u8)> {
    let mut best_load_out = LoadOut {
        items: EnumMap::default(),
    };
    if monster.level == 1 {
        best_load_out.items[ItemSlot::Weapon] = Some(ItemStack {
            code: "copper_dagger".to_owned(),
            quantity: 1,
        });
        let turns = can_defeat(game, &best_load_out, monster).unwrap();
        return Some((best_load_out, turns));
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
                    && is_item_allowed(item, slot)
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
        return Some((best_load_out, turns));
    }
    best_load_out.items[ItemSlot::Consumable1] = consumable_1;
    best_load_out.items[ItemSlot::Consumable2] = consumable_2;
    // Fine...use consumables
    if let Some(turns) = can_defeat(game, &best_load_out, monster) {
        return Some((best_load_out, turns));
    }
    // Oh no, need restoratives.
    for slot in [ItemSlot::Consumable2, ItemSlot::Consumable1] {
        let restorative = game
            .items
            .values()
            .filter(|item| {
                item.item_type == ItemType::Consumable
                    && best_load_out.items[ItemSlot::Consumable2]
                        .as_ref()
                        .is_none_or(|i| item.code != i.code)
                    && is_item_allowed(item, slot)
            })
            .filter_map(|item| {
                item.effects
                    .iter()
                    .find(|effect| effect.name == EffectType::Restore)
                    .map(|effect| (item, effect))
            })
            .max_by_key(|(_, effect)| effect.value)
            .unwrap();
        for count in 0..25 {
            best_load_out.items[slot] = Some(ItemStack {
                code: restorative.0.code.clone(),
                quantity: count + 1,
            });
            if let Some(turns) = can_defeat(game, &best_load_out, monster) {
                return Some((best_load_out, turns));
            }
        }
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        writeln!(f, "{}", self.kind)?;
        write!(f, "{}", self.limits)?;
        Ok(())
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct Limits {
    fight_skill: Option<u32>,
    fight_monsters: BTreeMap<String, u32>,
    equip_items: BTreeMap<String, u32>,
    craft_skill: EnumMap<CraftSkill, Option<u32>>,
    gather_skill: EnumMap<GatherSkill, Option<u32>>,
    complete_task: Option<u32>,
}
impl Limits {
    fn limit_fight_skill(&self, level: u32) -> Self {
        let mut this = self.clone();
        this.fight_skill = self
            .fight_skill
            .map_or(Some(level), |limit| Some(limit.min(level)));
        this.fight_monsters
            .retain(|_, monster_level| *monster_level < level);
        this.equip_items.retain(|_, item_level| *item_level < level);
        this
    }

    fn can_obtain_fight_skill(&self, level: u32) -> bool {
        self.fight_skill.map_or(true, |limit| level < limit)
    }

    fn limit_fight_monster(&self, monster: &Monster, level: u32) -> Self {
        let mut this = self.clone();
        if this
            .fight_skill
            .is_none_or(|level_limit| level <= level_limit)
        {
            this.fight_monsters
                .entry(monster.code.clone())
                .and_modify(|limit| *limit = level.min(*limit))
                .or_insert(level);
        }
        this
    }

    fn can_fight(&self, monster: &Monster) -> bool {
        self.fight_monsters
            .get(&monster.code)
            .is_none_or(|limit| monster.level < *limit)
    }

    fn limit_equip_item(&self, item: &Item) -> Self {
        let mut this = self.clone();
        if this
            .fight_skill
            .is_none_or(|level_limit| item.level < level_limit)
        {
            this.equip_items.insert(item.code.clone(), item.level);
        }
        this
    }

    fn can_equip(&self, item: &Item, monster: Option<&Monster>) -> bool {
        self.fight_skill
            .is_none_or(|level_limit| item.level < level_limit)
            && !self.equip_items.contains_key(&item.code)
            && monster.is_none_or(|monster| {
                self.fight_monsters
                    .get(&monster.code)
                    .is_none_or(|limit| item.level < *limit)
            })
    }

    fn limit_craft_skill(&self, skill: CraftSkill, level: u32) -> Self {
        let mut this = self.clone();
        let limit = this.craft_skill[skill].get_or_insert(level);
        *limit = level.min(*limit);
        this
    }

    fn can_obtain_craft_skill(&self, skill: CraftSkill, level: u32) -> bool {
        self.craft_skill[skill].map_or(true, |limit| level < limit)
    }

    fn can_craft<'a>(&self, item: &'a Item) -> Option<&'a Craft> {
        let Some(craft) = &item.craft else {
            return None;
        };
        if self.craft_skill[craft.skill].map_or(true, |limit| craft.level < limit) {
            Some(craft)
        } else {
            None
        }
    }

    fn limit_gather_skill(&self, skill: GatherSkill, level: u32) -> Self {
        let mut this = self.clone();
        let limit = this.gather_skill[skill].get_or_insert(level);
        *limit = level.min(*limit);
        this
    }

    fn can_obtain_gather_skill(&self, skill: GatherSkill, level: u32) -> bool {
        self.gather_skill[skill].map_or(true, |limit| level < limit)
    }

    fn can_gather(&self, resource: &Resource) -> bool {
        self.gather_skill[resource.skill].map_or(true, |limit| resource.level < limit)
    }

    fn limit_complete_task(&self, monster: &Monster) -> Limits {
        let mut this = self.clone();
        this.complete_task = match this.complete_task {
            Some(limit) => Some(limit.min(monster.level)),
            None => Some(monster.level),
        };
        this
    }

    fn can_complete_tasks(&self, monster: &Monster) -> bool {
        self.complete_task.is_none_or(|limit| monster.level < limit)
    }
}

impl fmt::Display for Limits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Limits:")?;
        if let Some(level) = self.fight_skill {
            writeln!(f, "- Fight skill level < {level}")?;
        }
        for (monster, level) in &self.fight_monsters {
            writeln!(f, "- Fight {monster} level < {level}")?;
        }
        for item in self.equip_items.keys() {
            writeln!(f, "- Without equipping {item}")?;
        }
        for (skill, level) in &self.craft_skill {
            if let Some(limit) = level {
                writeln!(f, "- {skill} level < {limit}")?;
            }
        }
        for (skill, level) in &self.gather_skill {
            if let Some(limit) = level {
                writeln!(f, "- {skill} level < {limit}")?;
            }
        }
        if let Some(level) = self.complete_task {
            writeln!(f, "- Without completing tasks by fighting level >= {level}")?;
        }
        Ok(())
    }
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
    dependencies: Option<HashSet<DefaultKey>>,
}

struct TaskTree {
    tasks: SlotMap<DefaultKey, DependencyInfo<Task>>,
    available: Vec<DefaultKey>,
}

impl TaskTree {
    fn new() -> Self {
        Self {
            tasks: SlotMap::new(),
            available: Vec::new(),
        }
    }

    fn add_root(&mut self, task: Task) -> DefaultKey {
        let key = self.tasks.insert(DependencyInfo {
            value: task,
            parent: None,
            dependencies: None,
        });
        self.available.push(key);
        key
    }

    fn find_available(&mut self, mut f: impl FnMut(&Task) -> bool) -> Option<DefaultKey> {
        self.available
            .iter()
            .position(|&key| f(&self.tasks[key].value))
            .map(|index| self.available.remove(index))
    }

    fn find_needs_dependencies(&mut self) -> Option<DefaultKey> {
        self.available
            .iter()
            .copied()
            .enumerate()
            .rev()
            .find(|&(_, task)| self.tasks[task].dependencies.is_none())
            .map(|(idx, _)| self.available.remove(idx))
    }

    fn complete_task(&mut self, key: DefaultKey) {
        let Some(task) = self.tasks.remove(key) else {
            return;
        };
        if let Some(parent) = task.parent.and_then(|key| self.tasks.get_mut(key)) {
            parent.dependencies.as_mut().unwrap().remove(&key);
        }
        let mut removed_tasks = HashSet::new();
        removed_tasks.insert(key);
        let mut tasks_to_remove: Vec<_> = task.dependencies.iter().flatten().copied().collect();
        while let Some(key) = tasks_to_remove.pop() {
            if let Some(task) = self.tasks.remove(key) {
                removed_tasks.insert(key);
                tasks_to_remove.extend(task.dependencies.iter().flatten().copied());
            }
        }
        self.available.retain(|key| !removed_tasks.contains(key));
    }

    fn add_dependencies(&mut self, key: DefaultKey, dependencies: Vec<Task>) {
        let task_dependencies = &mut self.tasks[key].dependencies;
        assert!(task_dependencies.is_none());
        *task_dependencies = Some(HashSet::new());
        for dependency in dependencies {
            let dependency_key = self.tasks.insert(DependencyInfo {
                value: dependency,
                parent: Some(key),
                dependencies: None,
            });
            self.tasks[key]
                .dependencies
                .as_mut()
                .unwrap()
                .insert(dependency_key);
            self.available.push(dependency_key)
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

    async fn deposit_all_items(&self, mut character: Character) -> Character {
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
            tokio::time::sleep_until(
                self.server
                    .instant_for(character.cooldown_expiration)
                    .into(),
            )
            .await;
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
struct MapInfo {
    map: Map,
    update_handle: Option<tokio::task::JoinHandle<()>>,
}

#[derive(Debug)]
struct World {
    maps: Arc<Mutex<HashMap<Position, MapInfo>>>,
    refresh_handle: tokio::task::JoinHandle<()>,
}

impl Drop for World {
    fn drop(&mut self) {
        self.maps
            .lock()
            .unwrap()
            .drain()
            .filter_map(|(_, MapInfo { update_handle, .. })| update_handle)
            .for_each(|handle| {
                handle.abort();
            });
        self.refresh_handle.abort();
    }
}

impl World {
    async fn new(server: Server) -> Self {
        let maps = Arc::new(Mutex::new(
            server
                .all_maps()
                .await
                .unwrap()
                .into_iter()
                .map(|map| {
                    (
                        map.position,
                        MapInfo {
                            map,
                            update_handle: None,
                        },
                    )
                })
                .collect::<HashMap<_, MapInfo>>(),
        ));
        let refresh_handle = tokio::spawn({
            let maps_mutex = maps.clone();
            async move {
                loop {
                    tokio::time::sleep(Duration::from_mins(10)).await;
                    let new_events = server.events().await.unwrap();
                    let mut maps = maps_mutex.lock().unwrap();
                    for event in new_events {
                        let pos = event.map.position;
                        let map_info = maps.get_mut(&pos).unwrap();
                        match &map_info.update_handle {
                            Some(handle) if !handle.is_finished() => continue,
                            None | Some(_) => {}
                        }
                        let server = server.clone();
                        let maps_mutex = maps_mutex.clone();
                        let new_handle = tokio::spawn(async move {
                            tokio::time::sleep_until(server.instant_for(event.created_at).into())
                                .await;
                            maps_mutex.lock().unwrap().get_mut(&pos).unwrap().map = event.map;
                            tokio::time::sleep_until(server.instant_for(event.expiration).into())
                                .await;
                            let maps = &mut maps_mutex.lock().unwrap();
                            let map = &mut maps.get_mut(&pos).unwrap().map;
                            map.skin = event.previous_skin.clone();
                            map.content = None;
                        });
                        map_info.update_handle = Some(new_handle);
                    }
                }
            }
        });
        Self {
            maps,
            refresh_handle,
        }
    }

    fn nearest(
        &self,
        content_type: ContentType,
        subtype: Option<&str>,
        position: Position,
    ) -> Option<Position> {
        self.maps
            .lock()
            .unwrap()
            .values()
            .map(|map_info| &map_info.map)
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
            .map(|map| map.position)
    }
}
