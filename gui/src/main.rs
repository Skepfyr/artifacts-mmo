use std::{
    collections::HashMap, future, sync::{Arc, Mutex}, time::Duration
};

use macroquad::prelude::*;
use sdk::Server;
use tokio::runtime::Handle;

use self::util::ImageIdentifier;

mod util;

#[macroquad::main("Artifacts MMO")]
pub async fn main() {
    let server = Server::new(
        "https://api.artifactsmmo.com/",
        std::env::var("API_TOKEN").unwrap(),
    );
    let (tx, rx) = std::sync::mpsc::sync_channel(1);
    std::thread::spawn(move || {
        let rt = tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap();
        tx.send(rt.handle().clone()).unwrap();
        rt.block_on(future::pending::<()>())
    });
    let handle = rx.recv().unwrap();
    drop(rx);
    Game::new(handle, server).main_loop().await;
}

struct Game {
    handle: Handle,
    server: Server,
    images: HashMap<ImageIdentifier, Texture2D>,
    camera: Camera2D,
}

impl Game {
    fn new(handle: Handle, server: Server) -> Self {
        Self {
            handle,
            server,
            images: HashMap::new(),
            camera: Default::default(),
        }
    }

    async fn main_loop(mut self) {
        self.camera.zoom = vec2(1.0, screen_width() / screen_height());
        let maps = self.handle.block_on(self.server.all_maps()).unwrap();
        let characters = self.handle.block_on(self.server.characters()).unwrap();
        let characters = Arc::new(Mutex::new(characters));
        let character_update = self.handle.spawn({
            let server = self.server.clone();
            let characters = characters.clone();
            async move {
                loop {
                    let new_characters = server.characters().await.unwrap();
                    *characters.lock().unwrap() = new_characters;
                    tokio::time::sleep(Duration::from_secs(5)).await;
                }
            }
        });
        loop {
            if is_key_pressed(KeyCode::Escape) || is_key_pressed(KeyCode::Q) {
                break;
            }
            if is_mouse_button_down(MouseButton::Left) {
                self.camera.target += mouse_delta_position() / self.camera.zoom;
            }
            self.camera.zoom *= 1.0 + mouse_wheel().1 * 0.1;
            self.camera.zoom.y = self.camera.zoom.x * screen_width() / screen_height();
            set_camera(&self.camera);
            for map in &maps {
                draw_texture_ex(
                    self.image(ImageIdentifier::Map(map.skin.clone())),
                    map.position.x as f32 - 0.5,
                    map.position.y as f32 - 0.5,
                    WHITE,
                    DrawTextureParams {
                        dest_size: Some(vec2(1.0, 1.0)),
                        ..Default::default()
                    },
                );
            }
            for character in &*characters.lock().unwrap() {
                let text_size = measure_text(&character.name, None, 16, 0.01);
                draw_text_ex(
                    &character.name,
                    character.position.x as f32 - text_size.width * 0.5,
                    character.position.y as f32 + 0.19,
                    TextParams {
                        font_size: 16,
                        font_scale: 0.01,
                        color: WHITE,
                        ..Default::default()
                    },
                );
                draw_texture_ex(
                    self.image(ImageIdentifier::Character(character.skin)),
                    character.position.x as f32 - 0.1,
                    character.position.y as f32 + 0.2 + (get_time() as f32 * 10.00).sin() * 0.01,
                    WHITE,
                    DrawTextureParams {
                        dest_size: Some(vec2(0.2, 0.2)),
                        ..Default::default()
                    },
                );
            }
            set_default_camera();
            next_frame().await;
        }
        character_update.abort();
    }

    fn image(&mut self, id: ImageIdentifier) -> &Texture2D {
        self.images.entry(id.clone()).or_insert_with(|| {
            let image = self
                .handle
                .block_on(util::cached_image(&self.server, id))
                .unwrap();
            Texture2D::from_file_with_format(&image, Some(ImageFormat::Png))
        })
    }
}
