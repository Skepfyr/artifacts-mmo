#![feature(array_try_map, is_none_or)]

use std::{error::Error, fmt, ops::Deref, sync::Arc};

use enum_map::Enum;
use eyre::Context;
use reqwest::{header, Client, IntoUrl, Request, StatusCode, Url};
use serde::{
    de::{self, DeserializeOwned},
    Deserialize, Deserializer, Serialize,
};
use serde_with::with_prefix;
use strum::EnumIter;
use time::OffsetDateTime;

#[derive(Debug, Clone)]
pub struct Server {
    url: Arc<Url>,
    api_token: Arc<str>,
    client: reqwest::Client,
}

impl Server {
    pub fn new<T: IntoUrl>(url: T, api_token: String) -> Self {
        Self {
            url: url.into_url().unwrap().into(),
            api_token: api_token.into(),
            client: Client::new(),
        }
    }

    async fn send_request<T: DeserializeOwned>(&self, request: Request) -> Result<T, ApiError> {
        let bytes = self
            .client
            .execute(request)
            .await?
            .error_for_status()?
            .bytes()
            .await?;
        serde_json::from_slice(&bytes)
            .wrap_err_with(|| format!("Failed to decode {}", std::str::from_utf8(&bytes).unwrap()))
            .map_err(|err| ApiError::Other(err.into()))
    }

    async fn get<T: DeserializeOwned>(&self, path: &str) -> Result<T, ApiError> {
        Ok(self
            .send_request::<DataWrapper<T>>(
                self.client
                    .get(self.url.join(path)?)
                    .bearer_auth(&self.api_token)
                    .header(header::ACCEPT, "application/json")
                    .build()?,
            )
            .await?
            .data)
    }

    async fn get_paginated<T: DeserializeOwned>(&self, path: &str) -> Result<Vec<T>, ApiError> {
        let mut url = self.url.join(path)?;
        #[derive(Deserialize)]
        struct Paginated<T> {
            data: Vec<T>,
            total: usize,
            page: u32,
            pages: u32,
        }
        let mut results = Vec::new();
        let mut page = 1;
        loop {
            url.set_query(Some(&format!("size=100&page={}", page)));
            let paginated: Paginated<T> = self
                .send_request(
                    self.client
                        .get(url.clone())
                        .bearer_auth(&self.api_token)
                        .header(header::ACCEPT, "application/json")
                        .build()?,
                )
                .await?;
            results.reserve(paginated.total - results.len());
            results.extend(paginated.data);
            if paginated.page == paginated.pages {
                break;
            }
            page += 1;
        }
        Ok(results)
    }

    async fn get_image(&self, path: &str) -> Result<Vec<u8>, ApiError> {
        Ok(self
            .client
            .get(self.url.join(path)?)
            .bearer_auth(&self.api_token)
            .send()
            .await?
            .error_for_status()?
            .bytes()
            .await?
            .to_vec())
    }

    async fn post<Request, Response>(
        &self,
        path: &str,
        body: &Request,
    ) -> Result<Response, ApiError>
    where
        Request: Serialize + ?Sized,
        Response: DeserializeOwned,
    {
        Ok(self
            .send_request::<DataWrapper<Response>>(
                self.client
                    .post(self.url.join(path)?)
                    .bearer_auth(&self.api_token)
                    .header(header::ACCEPT, "application/json")
                    .json(body)
                    .build()?,
            )
            .await?
            .data)
    }

    pub async fn status(&self) -> Result<Status, ApiError> {
        self.get("/").await
    }

    pub async fn move_character(&self, character: &Name, to: Position) -> Result<Move, ApiError> {
        self.post(&format!("/my/{character}/action/move"), &to)
            .await
    }

    pub async fn equip_item(
        &self,
        character: &Name,
        code: &str,
        slot: ItemSlot,
    ) -> Result<Equip, ApiError> {
        #[derive(Serialize)]
        struct EquipRequest<'a> {
            code: &'a str,
            slot: ItemSlot,
        }
        self.post(
            &format!("/my/{character}/action/equip/"),
            &EquipRequest { code, slot },
        )
        .await
    }

    pub async fn unequip_item(&self, character: &Name, slot: ItemSlot) -> Result<Equip, ApiError> {
        #[derive(Serialize)]
        struct UnequipRequest {
            slot: ItemSlot,
        }
        self.post(
            &format!("/my/{character}/action/unequip/"),
            &UnequipRequest { slot },
        )
        .await
    }

    pub async fn fight(&self, character: &Name) -> Result<CharacterFight, ApiError> {
        self.post(&format!("/my/{character}/action/fight"), &())
            .await
    }

    pub async fn gather(&self, character: &Name) -> Result<SkillData, ApiError> {
        self.post(&format!("/my/{character}/action/gathering"), &())
            .await
    }

    pub async fn craft(
        &self,
        character: &Name,
        code: String,
        quantity: u32,
    ) -> Result<SkillData, ApiError> {
        self.post(
            &format!("/my/{character}/action/crafting"),
            &ItemStack { code, quantity },
        )
        .await
    }

    pub async fn deposit_items(
        &self,
        character: &Name,
        items: ItemStack,
    ) -> Result<BankItem, ApiError> {
        self.post(&format!("/my/{character}/action/bank/deposit"), &items)
            .await
    }

    pub async fn deposit_gold(
        &self,
        character: &Name,
        gold: Gold,
    ) -> Result<GoldTransaction, ApiError> {
        self.post(&format!("/my/{character}/action/bank/deposit/gold"), &gold)
            .await
    }

    pub async fn recycle(&self, character: Name, items: ItemStack) -> Result<Recycling, ApiError> {
        self.post(&format!("/my/{character}/action/recycling"), &items)
            .await
    }

    pub async fn withdraw_items(
        &self,
        character: &Name,
        items: ItemStack,
    ) -> Result<BankItem, ApiError> {
        self.post(&format!("/my/{character}/action/bank/withdraw"), &items)
            .await
    }

    pub async fn withdraw_gold(
        &self,
        character: &Name,
        gold: Gold,
    ) -> Result<GoldTransaction, ApiError> {
        self.post(&format!("/my/{character}/action/bank/withdraw/gold"), &gold)
            .await
    }

    pub async fn buy_item(
        &self,
        character: &Name,
        item: ItemStack,
        price: u32,
    ) -> Result<ExchangeTransaction, ApiError> {
        #[derive(Serialize)]
        struct BuyRequest {
            code: String,
            quantity: u32,
            price: u32,
        }
        self.post(
            &format!("/my/{character}/action/ge/buy"),
            &BuyRequest {
                code: item.code,
                quantity: item.quantity,
                price,
            },
        )
        .await
    }

    pub async fn sell_item(
        &self,
        character: &Name,
        item: ItemStack,
        price: u32,
    ) -> Result<ExchangeTransaction, ApiError> {
        #[derive(Serialize)]
        struct SellRequest {
            code: String,
            quantity: u32,
            price: u32,
        }
        self.post(
            &format!("/my/{character}/action/ge/sell"),
            &SellRequest {
                code: item.code,
                quantity: item.quantity,
                price,
            },
        )
        .await
    }

    pub async fn accept_task(&self, character: &Name) -> Result<AcceptedTask, ApiError> {
        self.post(&format!("/my/{character}/action/task/new"), &())
            .await
    }

    pub async fn complete_task(&self, character: &Name) -> Result<TaskReward, ApiError> {
        self.post(&format!("/my/{character}/action/task/complete"), &())
            .await
    }

    pub async fn exchange_task_coins(&self, character: &Name) -> Result<TaskReward, ApiError> {
        self.post(&format!("/my/{character}/action/task/exchange"), &())
            .await
    }

    pub async fn delete_item(
        &self,
        character: &Name,
        item: ItemStack,
    ) -> Result<DeletedItem, ApiError> {
        self.post(&format!("/my/{character}/action/delete"), &item)
            .await
    }

    pub async fn character_logs(&self, character: &Name) -> Result<Vec<Action>, ApiError> {
        self.get_paginated(&format!("/my/{character}/logs")).await
    }

    pub async fn characters_logs(&self) -> Result<Vec<Action>, ApiError> {
        self.get_paginated("/my/logs").await
    }

    pub async fn characters(&self) -> Result<Vec<Character>, ApiError> {
        self.get("/my/characters").await
    }

    pub async fn bank_items(&self) -> Result<Vec<ItemStack>, ApiError> {
        match self.get_paginated("/my/bank/items").await {
            Ok(items) => Ok(items),
            Err(ApiError::ClientError(StatusCode::NOT_FOUND)) => Ok(Vec::new()),
            Err(err) => Err(err),
        }
    }

    pub async fn bank_gold(&self) -> Result<Gold, ApiError> {
        self.get("/my/bank/gold").await
    }

    pub async fn create_character(&self, name: &Name, skin: Skin) -> Result<Character, ApiError> {
        #[derive(Serialize)]
        struct CreateCharacter<'a> {
            name: &'a Name,
            skin: Skin,
        }
        self.post("/characters/create", &CreateCharacter { name, skin })
            .await
    }

    pub async fn all_characters(&self) -> Result<Vec<Character>, ApiError> {
        self.get_paginated("/characters/").await
    }

    pub async fn character(&self, name: &Name) -> Result<Character, ApiError> {
        self.get(&format!("/characters/{name}")).await
    }

    pub async fn all_maps(&self) -> Result<Vec<Map>, ApiError> {
        self.get_paginated("/maps/").await
    }

    pub async fn map(&self, position: Position) -> Result<Map, ApiError> {
        self.get(&format!("/maps/{x}/{y}", x = position.x, y = position.y))
            .await
    }

    pub async fn all_items(&self) -> Result<Vec<Item>, ApiError> {
        self.get_paginated("/items/").await
    }

    pub async fn item(&self, code: &str) -> Result<SingleItem, ApiError> {
        self.get(&format!("/items/{code}")).await
    }

    pub async fn all_monsters(&self) -> Result<Vec<Monster>, ApiError> {
        self.get_paginated("/monsters/").await
    }

    pub async fn monster(&self, code: &str) -> Result<Monster, ApiError> {
        self.get(&format!("/monsters/{code}")).await
    }

    pub async fn all_resources(&self) -> Result<Vec<Resource>, ApiError> {
        self.get_paginated("/resources/").await
    }

    pub async fn resource(&self, code: &str) -> Result<Resource, ApiError> {
        self.get(&format!("/resources/{code}")).await
    }

    pub async fn events(&self) -> Result<Vec<Event>, ApiError> {
        match self.get_paginated("/events/").await {
            Ok(events) => Ok(events),
            Err(ApiError::ClientError(StatusCode::NOT_FOUND)) => Ok(Vec::new()),
            Err(err) => Err(err),
        }
    }

    pub async fn all_exchange_items(&self) -> Result<Vec<ItemExchangeInfo>, ApiError> {
        self.get_paginated("/ge/").await
    }

    pub async fn exchange_item(&self, code: &str) -> Result<ItemExchangeInfo, ApiError> {
        self.get(&format!("/ge/{code}")).await
    }

    pub async fn character_image(&self, skin: Skin) -> Result<Vec<u8>, ApiError> {
        self.get_image(&format!("/images/characters/{skin}.png"))
            .await
    }

    pub async fn item_image(&self, code: &str) -> Result<Vec<u8>, ApiError> {
        self.get_image(&format!("/images/items/{code}.png")).await
    }

    pub async fn monster_image(&self, code: &str) -> Result<Vec<u8>, ApiError> {
        self.get_image(&format!("/images/monsters/{code}.png"))
            .await
    }

    pub async fn map_image(&self, skin: &str) -> Result<Vec<u8>, ApiError> {
        self.get_image(&format!("/images/maps/{skin}.png")).await
    }

    pub async fn resource_image(&self, code: &str) -> Result<Vec<u8>, ApiError> {
        self.get_image(&format!("/images/resources/{code}.png"))
            .await
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct DataWrapper<T> {
    data: T,
}

#[derive(Debug)]
pub enum ApiError {
    ClientError(StatusCode),
    Other(Box<dyn Error + Send + Sync + 'static>),
}

impl Error for ApiError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ClientError(_) => None,
            Self::Other(err) => Some(err.as_ref()),
        }
    }
}

impl fmt::Display for ApiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ClientError(code) => {
                write!(f, "API returned status code: {}", code)
            }
            Self::Other(err) => {
                write!(f, "API returned an error: {}", err)
            }
        }
    }
}

impl From<reqwest::Error> for ApiError {
    fn from(err: reqwest::Error) -> Self {
        match err.status() {
            Some(status) if status.is_client_error() => Self::ClientError(status),
            _ => Self::Other(Box::new(err)),
        }
    }
}

impl From<url::ParseError> for ApiError {
    fn from(err: url::ParseError) -> Self {
        Self::Other(Box::new(err))
    }
}

#[derive(Debug, Deserialize)]
pub struct Status {
    pub status: String,
    pub version: String,
    pub characters_online: u32,
    pub announcements: Vec<Announcement>,
    pub last_wipe: String,
    pub next_wipe: String,
}

#[derive(Debug, Deserialize)]
pub struct Announcement {
    pub message: String,
    pub created_at: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Name(Box<str>);

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for Name {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Character {
    pub name: Name,
    pub skin: Skin,
    #[serde(flatten)]
    pub level: Level,
    pub gold: u32,
    #[serde(flatten, with = "mining")]
    pub mining_level: Level,
    #[serde(flatten, with = "woodcutting")]
    pub woodcutting_level: Level,
    #[serde(flatten, with = "fishing")]
    pub fishing_level: Level,
    #[serde(flatten, with = "weaponcrafting")]
    pub weapon_crafting_level: Level,
    #[serde(flatten, with = "gearcrafting")]
    pub gear_crafting_level: Level,
    #[serde(flatten, with = "jewelrycrafting")]
    pub jewelry_crafting_level: Level,
    #[serde(flatten, with = "cooking")]
    pub cooking_level: Level,
    pub hp: u32,
    pub haste: u32,
    pub critical_strike: u32,
    pub stamina: u32,
    #[serde(flatten, with = "attack")]
    pub attack: Elements,
    #[serde(flatten, with = "dmg")]
    pub damage: Elements,
    #[serde(flatten, with = "res")]
    pub resistance: Elements,
    #[serde(flatten)]
    pub position: Position,
    pub cooldown: u32,
    #[serde(with = "time::serde::rfc3339")]
    pub cooldown_expiration: OffsetDateTime,
    #[serde(flatten)]
    pub inventory: Inventory,
    #[serde(flatten, deserialize_with = "option_task")]
    pub task: Option<Task>,
}

impl Character {
    pub fn has_craft_skill(&self, skill: CraftSkill, level: u32) -> bool {
        match skill {
            CraftSkill::Mining => self.mining_level.level >= level,
            CraftSkill::Woodcutting => self.woodcutting_level.level >= level,
            CraftSkill::WeaponCrafting => self.weapon_crafting_level.level >= level,
            CraftSkill::GearCrafting => self.gear_crafting_level.level >= level,
            CraftSkill::JewelryCrafting => self.jewelry_crafting_level.level >= level,
            CraftSkill::Cooking => self.cooking_level.level >= level,
        }
    }

    pub fn has_gather_skill(&self, skill: GatherSkill, level: u32) -> bool {
        match skill {
            GatherSkill::Mining => self.mining_level.level >= level,
            GatherSkill::Woodcutting => self.woodcutting_level.level >= level,
            GatherSkill::Fishing => self.fishing_level.level >= level,
        }
    }
}

fn option_task<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Option<Task>, D::Error> {
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Either {
        Task(Task),
        Bad { task: String },
    }
    let task = Either::deserialize(deserializer)?;
    match task {
        Either::Task(task) => Ok(Some(task)),
        Either::Bad { task } if task.is_empty() => Ok(None),
        Either::Bad { task } => Err(de::Error::invalid_value(
            de::Unexpected::Str(&task),
            &"a task",
        )),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Level {
    pub level: u32,
    pub xp: u32,
    pub max_xp: u32,
}
with_prefix!(mining "mining_");
with_prefix!(woodcutting "woodcutting_");
with_prefix!(fishing "fishing_");
with_prefix!(weaponcrafting "weaponcrafting_");
with_prefix!(gearcrafting "gearcrafting_");
with_prefix!(jewelrycrafting "jewelrycrafting_");
with_prefix!(cooking "cooking_");

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Elements {
    pub fire: i32,
    pub earth: i32,
    pub water: i32,
    pub air: i32,
}
with_prefix!(attack "attack_");
with_prefix!(dmg "dmg_");
with_prefix!(res "res_");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Position {
    pub x: i32,
    pub y: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum, EnumIter, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ItemSlot {
    Weapon,
    Shield,
    Helmet,
    BodyArmor,
    LegArmor,
    Boots,
    #[serde(rename = "ring1")]
    Ring1,
    #[serde(rename = "ring2")]
    Ring2,
    Amulet,
    #[serde(rename = "artifact")]
    Artifact1,
    #[serde(rename = "artifact2")]
    Artifact2,
    #[serde(rename = "artifact3")]
    Artifact3,
    #[serde(rename = "consumable1")]
    Consumable1,
    #[serde(rename = "consumable2")]
    Consumable2,
}

impl ItemSlot {
    pub fn item_type(self) -> ItemType {
        match self {
            Self::Weapon => ItemType::Weapon,
            Self::Shield => ItemType::Shield,
            Self::Helmet => ItemType::Helmet,
            Self::BodyArmor => ItemType::BodyArmor,
            Self::LegArmor => ItemType::LegArmor,
            Self::Boots => ItemType::Boots,
            Self::Ring1 | Self::Ring2 => ItemType::Ring,
            Self::Amulet => ItemType::Amulet,
            Self::Artifact1 | Self::Artifact2 | Self::Artifact3 => ItemType::Artifact,
            Self::Consumable1 | Self::Consumable2 => ItemType::Consumable,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Inventory {
    #[serde(rename = "weapon_slot", deserialize_with = "empty_string")]
    pub weapon: Option<String>,
    #[serde(rename = "shield_slot", deserialize_with = "empty_string")]
    pub shield: Option<String>,
    #[serde(rename = "helmet_slot", deserialize_with = "empty_string")]
    pub helmet: Option<String>,
    #[serde(rename = "body_armor_slot", deserialize_with = "empty_string")]
    pub body_armor: Option<String>,
    #[serde(rename = "leg_armor_slot", deserialize_with = "empty_string")]
    pub leg_armor: Option<String>,
    #[serde(rename = "boots_slot", deserialize_with = "empty_string")]
    pub boots: Option<String>,
    #[serde(flatten, deserialize_with = "rings")]
    pub rings: [Option<String>; 2],
    #[serde(rename = "amulet_slot", deserialize_with = "empty_string")]
    pub amulet: Option<String>,
    #[serde(flatten, deserialize_with = "artifacts")]
    pub artifacts: [Option<String>; 3],
    #[serde(flatten, deserialize_with = "consumables")]
    pub consumables: [Option<ItemStack>; 2],
    #[serde(rename = "inventory", deserialize_with = "inventory")]
    pub items: [Option<ItemStack>; 20],
    #[serde(rename = "inventory_max_items")]
    pub max_items: u32,
}
impl Inventory {
    pub fn has_space_for(&self, stack: &ItemStack) -> bool {
        let has_capacity = self
            .items
            .iter()
            .map(|stack| stack.as_ref().map(|stack| stack.quantity).unwrap_or(0))
            .sum::<u32>()
            + stack.quantity
            <= self.max_items;
        let has_slot = self
            .items
            .iter()
            .any(|s| s.as_ref().is_none_or(|s| s.code == stack.code));
        has_capacity && has_slot
    }

    pub fn get(&self, slot: ItemSlot) -> Option<ItemStack> {
        let code = match slot {
            ItemSlot::Weapon => self.weapon.as_ref(),
            ItemSlot::Shield => self.shield.as_ref(),
            ItemSlot::Helmet => self.helmet.as_ref(),
            ItemSlot::BodyArmor => self.body_armor.as_ref(),
            ItemSlot::LegArmor => self.leg_armor.as_ref(),
            ItemSlot::Boots => self.boots.as_ref(),
            ItemSlot::Ring1 => self.rings[0].as_ref(),
            ItemSlot::Ring2 => self.rings[1].as_ref(),
            ItemSlot::Amulet => self.amulet.as_ref(),
            ItemSlot::Artifact1 => self.artifacts[0].as_ref(),
            ItemSlot::Artifact2 => self.artifacts[1].as_ref(),
            ItemSlot::Artifact3 => self.artifacts[2].as_ref(),
            ItemSlot::Consumable1 => return self.consumables[0].clone(),
            ItemSlot::Consumable2 => return self.consumables[1].clone(),
        };
        code.map(|code| ItemStack {
            code: code.clone(),
            quantity: 1,
        })
    }
}
fn empty_string<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Option<String>, D::Error> {
    let s: Option<String> = Deserialize::deserialize(deserializer)?;
    Ok(s.filter(|s| !s.is_empty()))
}
fn rings<'de, D: Deserializer<'de>>(deserializer: D) -> Result<[Option<String>; 2], D::Error> {
    slot_array(deserializer, "ring")
}
fn artifacts<'de, D: Deserializer<'de>>(deserializer: D) -> Result<[Option<String>; 3], D::Error> {
    slot_array(deserializer, "artifact")
}
fn consumables<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<[Option<ItemStack>; 2], D::Error> {
    slot_array_with_quantity(deserializer, "consumable")
}
fn slot_array<'de, D, const N: usize>(
    deserializer: D,
    name: &str,
) -> Result<[Option<String>; N], D::Error>
where
    D: Deserializer<'de>,
{
    struct Visitor<'a, const N: usize> {
        name: &'a str,
    }
    impl<'a, 'de, const N: usize> de::Visitor<'de> for Visitor<'a, N> {
        type Value = [Option<String>; N];

        fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "a numbered list of {} slots", N)
        }

        fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
            let mut slots = [const { None }; N];
            while let Some(key) = map.next_key::<&str>()? {
                let Some(key) = key.strip_prefix(self.name) else {
                    continue;
                };
                let Some(index) = key.strip_suffix("_slot") else {
                    continue;
                };
                let Ok(index) = index.parse::<usize>() else {
                    continue;
                };
                let content: String = map.next_value()?;
                slots[index - 1] = (!content.is_empty()).then_some(content);
            }
            Ok(slots)
        }
    }
    deserializer.deserialize_map(Visitor { name })
}
fn slot_array_with_quantity<'de, D, const N: usize>(
    deserializer: D,
    name: &str,
) -> Result<[Option<ItemStack>; N], D::Error>
where
    D: Deserializer<'de>,
{
    struct Visitor<'a, const N: usize> {
        name: &'a str,
    }
    impl<'a, 'de, const N: usize> de::Visitor<'de> for Visitor<'a, N> {
        type Value = [Option<ItemStack>; N];

        fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "a numbered list of {} slots", N)
        }

        fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
            let mut slots: [(Option<String>, Option<u32>); N] = [const { (None, None) }; N];
            while let Some(key) = map.next_key::<&str>()? {
                let Some(key) = key.strip_prefix(self.name) else {
                    continue;
                };
                let Some((index, rest)) = key.split_once("_slot") else {
                    continue;
                };
                let Ok(index) = index.parse::<usize>() else {
                    continue;
                };
                match rest {
                    "" => {
                        let content: String = map.next_value()?;
                        slots[index - 1].0 = (!content.is_empty()).then_some(content);
                    }
                    "_quantity" => {
                        let content: u32 = map.next_value()?;
                        slots[index - 1].1 = (content > 0).then_some(content);
                    }
                    _ => continue,
                }
            }
            slots.try_map(|(item, quantity)| match (item, quantity) {
                (Some(item), Some(quantity)) => Ok(Some(ItemStack {
                    code: item,
                    quantity,
                })),
                (None, None) => Ok(None),
                _ => Err(de::Error::custom(
                    "item and quantity must be both present or both absent",
                )),
            })
        }
    }
    deserializer.deserialize_map(Visitor { name })
}
fn inventory<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> Result<[Option<ItemStack>; 20], D::Error> {
    let mut items = <[Option<ItemStack>; 20]>::deserialize(deserializer)?;
    for item in &mut items {
        item.take_if(|item| item.quantity == 0);
    }
    Ok(items)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Task {
    #[serde(rename = "task")]
    pub code: String,
    pub task_type: TaskType,
    #[serde(rename = "task_progress")]
    pub progress: u32,
    #[serde(rename = "task_total")]
    pub total: u32,
}

impl Task {
    pub fn remaining(&self) -> u32 {
        self.total - self.progress
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskType {
    Monsters,
}

#[derive(Debug, Deserialize)]
pub struct Move {
    pub cooldown: Cooldown,
    pub destination: Destination,
    pub character: Character,
}

#[derive(Debug, Deserialize)]
pub struct Cooldown {
    pub total_seconds: u32,
    pub remaining_seconds: u32,
    #[serde(with = "time::serde::rfc3339")]
    pub expiration: OffsetDateTime,
    pub reason: String,
}

#[derive(Debug, Deserialize)]
pub struct Destination {
    pub name: String,
    pub x: i32,
    pub y: i32,
    pub content: Option<MapContent>,
}

#[derive(Debug, Deserialize)]
pub struct MapContent {
    #[serde(rename = "type")]
    pub content_type: ContentType,
    pub code: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ContentType {
    Monster,
    Resource,
    Workshop,
    Bank,
    GrandExchange,
    TasksMaster,
}

#[derive(Debug, Deserialize)]
pub struct Equip {
    pub cooldown: Cooldown,
    pub slot: ItemSlot,
    pub item: Item,
    pub character: Character,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Item {
    pub name: String,
    pub code: String,
    pub level: u32,
    #[serde(rename = "type")]
    pub item_type: ItemType,
    #[serde(rename = "subtype")]
    pub item_subtype: String,
    pub description: String,
    pub effects: Vec<Effect>,
    pub craft: Option<Craft>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Enum, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ItemType {
    Consumable,
    BodyArmor,
    Weapon,
    Resource,
    LegArmor,
    Helmet,
    Boots,
    Shield,
    Amulet,
    Ring,
    Artifact,
    Currency,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Effect {
    pub name: EffectType,
    pub value: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EffectType {
    #[serde(rename = "restore")]
    Restore,
    #[serde(rename = "boost_hp")]
    BoostHp,
    #[serde(rename = "hp")]
    Hp,
    #[serde(rename = "dmg_fire")]
    FireDamage,
    #[serde(rename = "dmg_earth")]
    EarthDamage,
    #[serde(rename = "dmg_water")]
    WaterDamage,
    #[serde(rename = "dmg_air")]
    AirDamage,
    #[serde(rename = "boost_dmg_fire")]
    BoostFireDamage,
    #[serde(rename = "boost_dmg_earth")]
    BoostEarthDamage,
    #[serde(rename = "boost_dmg_water")]
    BoostWaterDamage,
    #[serde(rename = "boost_dmg_air")]
    BoostAirDamage,
    #[serde(rename = "res_fire")]
    FireResistance,
    #[serde(rename = "res_earth")]
    EarthResistance,
    #[serde(rename = "res_water")]
    WaterResistance,
    #[serde(rename = "res_air")]
    AirResistance,
    #[serde(rename = "attack_fire")]
    FireAttack,
    #[serde(rename = "attack_earth")]
    EarthAttack,
    #[serde(rename = "attack_water")]
    WaterAttack,
    #[serde(rename = "attack_air")]
    AirAttack,
    #[serde(rename = "haste")]
    Haste,
    #[serde(rename = "mining")]
    Mining,
    #[serde(rename = "woodcutting")]
    Woodcutting,
    #[serde(rename = "fishing")]
    Fishing,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Craft {
    pub skill: CraftSkill,
    pub level: u32,
    pub items: Vec<ItemStack>,
    pub quantity: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CraftSkill {
    Mining,
    Woodcutting,
    WeaponCrafting,
    GearCrafting,
    JewelryCrafting,
    Cooking,
}

impl fmt::Display for CraftSkill {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.serialize(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ItemStack {
    pub code: String,
    pub quantity: u32,
}
impl ItemStack {
    pub fn has(&self, stack: &ItemStack) -> bool {
        self.code == stack.code && self.quantity >= stack.quantity
    }
}

#[derive(Debug, Deserialize)]
pub struct CharacterFight {
    pub cooldown: Cooldown,
    pub fight: Fight,
    pub character: Character,
}

#[derive(Debug, Deserialize)]
pub struct Fight {
    pub xp: u32,
    pub gold: u32,
    pub drops: Vec<ItemStack>,
    pub turns: u32,
    pub monster_blocked_hits: BlockedHits,
    pub player_blocked_hits: BlockedHits,
    pub logs: Vec<String>,
    pub result: FightResult,
}

#[derive(Debug, Deserialize)]
pub struct BlockedHits {
    pub fire: u32,
    pub earth: u32,
    pub water: u32,
    pub air: u32,
    pub total: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FightResult {
    Win,
    Lose,
}

#[derive(Debug, Deserialize)]
pub struct SkillData {
    pub cooldown: Cooldown,
    pub details: SkillInfo,
    pub character: Character,
}

#[derive(Debug, Deserialize)]
pub struct SkillInfo {
    pub xp: u32,
    pub items: Vec<ItemStack>,
}

#[derive(Debug, Deserialize)]
pub struct BankItem {
    pub cooldown: Cooldown,
    pub item: Item,
    pub bank: Vec<ItemStack>,
    pub character: Character,
}

#[derive(Debug, Deserialize)]
pub struct GoldTransaction {
    pub cooldown: Cooldown,
    pub bank: Gold,
    pub character: Character,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Gold {
    pub quantity: u32,
}

#[derive(Debug, Deserialize)]
pub struct Recycling {
    pub cooldown: Cooldown,
    pub details: RecyclingItems,
    pub character: Character,
}

#[derive(Debug, Deserialize)]
pub struct RecyclingItems {
    pub items: Vec<ItemStack>,
}

#[derive(Debug, Deserialize)]
pub struct ExchangeTransaction {
    pub cooldown: Cooldown,
    pub transaction: Transaction,
    pub character: Character,
}

#[derive(Debug, Deserialize)]
pub struct Transaction {
    pub code: String,
    pub quantity: u32,
    pub price: u32,
    pub total_price: u32,
}

#[derive(Debug, Deserialize)]
pub struct AcceptedTask {
    pub cooldown: Cooldown,
    pub task: TaskData,
    pub character: Character,
}

#[derive(Debug, Deserialize)]
pub struct TaskData {
    pub code: String,
    #[serde(rename = "type")]
    pub task_type: TaskType,
    pub total: u32,
}

#[derive(Debug, Deserialize)]
pub struct TaskReward {
    pub cooldown: Cooldown,
    pub reward: ItemStack,
    pub character: Character,
}

#[derive(Debug, Deserialize)]
pub struct DeletedItem {
    pub cooldown: Cooldown,
    pub item: ItemStack,
    pub character: Character,
}

#[derive(Debug, Deserialize)]
pub struct Action {
    pub character: Name,
    pub account: String,
    #[serde(rename = "type")]
    pub action_type: String,
    pub description: String,
    pub cooldown: u32,
    pub cooldown_expiration: String,
    pub created_at: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Skin {
    Men1,
    Men2,
    Men3,
    Women1,
    Women2,
    Women3,
}

impl fmt::Display for Skin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.serialize(f)
    }
}

#[derive(Debug, Deserialize)]
pub struct Map {
    pub name: String,
    pub skin: String,
    #[serde(flatten)]
    pub position: Position,
    pub content: Option<MapContent>,
}

#[derive(Debug, Deserialize)]
pub struct SingleItem {
    pub item: Item,
    pub exchange: Option<ItemExchangeInfo>,
}

#[derive(Debug, Deserialize)]
pub struct ItemExchangeInfo {
    pub item: String,
    pub stock: u32,
    pub price: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Monster {
    pub name: String,
    pub code: String,
    pub level: u32,
    pub hp: u32,
    #[serde(flatten, with = "attack")]
    pub attack: Elements,
    #[serde(flatten, with = "res")]
    pub resistance: Elements,
    pub min_gold: u32,
    pub max_gold: u32,
    pub drops: Vec<ItemDrop>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct ItemDrop {
    pub code: String,
    pub rate: u32,
    pub min_quantity: u32,
    pub max_quantity: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize)]
pub struct Resource {
    pub name: String,
    pub code: String,
    pub skill: GatherSkill,
    pub level: u32,
    pub drops: Vec<ItemDrop>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GatherSkill {
    Mining,
    Woodcutting,
    Fishing,
}

impl fmt::Display for GatherSkill {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.serialize(f)
    }
}

#[derive(Debug, Deserialize)]
pub struct Event {
    pub name: String,
    pub map: Map,
    pub previous_skin: String,
    pub duration: u32,
    pub expiration: String,
    pub created_at: String,
}
