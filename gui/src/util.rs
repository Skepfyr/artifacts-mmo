use std::sync::LazyLock;

use directories::ProjectDirs;
use eyre::Result;
use macroquad::prelude::*;
use sdk::{Server, Skin};

static PROJECT_DIRS: LazyLock<ProjectDirs> =
    LazyLock::new(|| ProjectDirs::from("com.artifactsmmo", "Skepfyr", "Artifacts MMO").unwrap());

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImageIdentifier {
    Character(Skin),
    Item(String),
    Monster(String),
    Map(String),
    Resource(String),
}

pub async fn cached_image(server: &Server, identifier: ImageIdentifier) -> Result<Vec<u8>> {
    let image_type = match identifier {
        ImageIdentifier::Character(_) => "characters",
        ImageIdentifier::Item(_) => "items",
        ImageIdentifier::Monster(_) => "monsters",
        ImageIdentifier::Map(_) => "maps",
        ImageIdentifier::Resource(_) => "resources",
    };
    let name: String;
    let image_name = match &identifier {
        ImageIdentifier::Character(skin) => {
            name = skin.to_string();
            &*name
        }
        ImageIdentifier::Item(name) => name,
        ImageIdentifier::Monster(name) => name,
        ImageIdentifier::Map(name) => name,
        ImageIdentifier::Resource(name) => name,
    };
    let image_path = PROJECT_DIRS
        .cache_dir()
        .join(format!("images/{image_type}/{image_name}.png"));
    if let Ok(image) = tokio::fs::read(&image_path).await {
        debug!("Loaded image from cache: {:?}", identifier);
        return Ok(image);
    }
    debug!("Downloading image: {:?}", identifier);
    let image = match &identifier {
        ImageIdentifier::Character(skin) => server.character_image(*skin).await?,
        ImageIdentifier::Item(name) => server.item_image(name).await?,
        ImageIdentifier::Monster(name) => server.monster_image(name).await?,
        ImageIdentifier::Map(name) => server.map_image(name).await?,
        ImageIdentifier::Resource(name) => server.resource_image(name).await?,
    };
    tokio::fs::create_dir_all(image_path.parent().unwrap()).await?;
    if let Err(e) = tokio::fs::write(&image_path, &image).await {
        error!("Failed to write image to cache: {}", e);
    }
    info!("Downloaded image {:?} and saved to cache", identifier);
    Ok(image)
}
