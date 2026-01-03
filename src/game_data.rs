use std::{
    collections::HashMap,
    collections::HashSet,
    error::Error,
    path::{Path, PathBuf},
    sync::Arc,
};

use super::*;

/// Returns the project root directory (where Cargo.toml is located).
pub fn project_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Returns the path to the assets directory.
pub fn assets_path() -> PathBuf {
    project_root().join("assets")
}

/// Returns the path to the data directory.
pub fn data_path() -> PathBuf {
    project_root().join("data")
}

pub struct GameData {
    pub map_headers: HashMap<String, MapHeaderInfo>,
    pub map_events: HashMap<String, MapEvents>,
    pub map_id_to_name: HashMap<String, String>,
    pub tileset_bases: HashMap<String, String>,
    pub tileset_grass_tile_ids: HashMap<String, Option<u8>>,
    pub tileset_collision: HashMap<String, HashSet<u8>>,
    pub sprite_id_to_png: HashMap<u8, PathBuf>,
    pub item_display_names: HashMap<String, String>,
    pub item_prices: HashMap<String, u32>,
    pub tm_prices: Vec<u32>,
    pub trainer_parties: HashMap<String, Vec<TrainerParty>>,
    pub trainer_base_reward_money: HashMap<String, u32>,
    pub wild_encounters: HashMap<String, WildEncounterTable>,
    pub wild_slot_thresholds: Vec<u8>,
    pub move_display_names: HashMap<String, String>,
    pub move_db: HashMap<String, MoveData>,
    pub pokemon_display_names: HashMap<String, String>,
    pub pokemon_stats: HashMap<String, BaseStats>,
    pub pokemon_moves: HashMap<String, PokemonMoves>,
    pub type_chart: TypeChart,
    pub audio_pitches: Arc<Vec<u16>>,
    pub map_music_constants: HashMap<String, String>,
}

impl GameData {
    /// Load game data from local, Rust-owned RON files.
    pub fn load(assets_root: &Path) -> Result<Self, Box<dyn Error>> {
        #[derive(Clone, Debug, Deserialize)]
        struct RonConnection {
            direction: String,
            target_map: String,
            offset: i32,
        }

        #[derive(Clone, Debug, Deserialize)]
        struct RonMapHeader {
            tileset: String,
            map_id: String,
            connections: Vec<RonConnection>,
        }

        let data_dir = data_path();

        let map_headers_ron: HashMap<String, RonMapHeader> =
            data::load_ron(&data_dir.join("map_headers.ron"))?;
        let mut map_headers: HashMap<String, MapHeaderInfo> = HashMap::new();
        let mut map_id_to_name: HashMap<String, String> = HashMap::new();
        for (map_name, header) in map_headers_ron {
            let mut connections = MapConnections::default();
            for conn in header.connections {
                let map_conn = MapConnection {
                    map_name: conn.target_map,
                    offset_blocks: conn.offset,
                };
                match conn.direction.as_str() {
                    "north" => connections.north = Some(map_conn),
                    "south" => connections.south = Some(map_conn),
                    "west" => connections.west = Some(map_conn),
                    "east" => connections.east = Some(map_conn),
                    _ => {}
                }
            }

            map_id_to_name.insert(header.map_id.clone(), map_name.clone());
            map_headers.insert(
                map_name.clone(),
                MapHeaderInfo {
                    map_name,
                    map_id: header.map_id,
                    tileset: header.tileset,
                    connections,
                },
            );
        }

        let tileset_bases: HashMap<String, String> = data::load_ron(&data_dir.join("tilesets.ron"))?;
        let tileset_grass_tile_ids: HashMap<String, Option<u8>> =
            data::load_ron(&data_dir.join("tileset_headers.ron"))?;
        let tileset_collision_raw: HashMap<String, Vec<u8>> =
            data::load_ron(&data_dir.join("tileset_collision.ron"))?;
        let tileset_collision: HashMap<String, HashSet<u8>> = tileset_collision_raw
            .into_iter()
            .map(|(k, v)| (k, v.into_iter().collect()))
            .collect();

        let sprite_id_to_png_raw: HashMap<u8, PathBuf> =
            data::load_ron(&data_dir.join("sprite_mapping.ron"))?;
        let sprite_id_to_png: HashMap<u8, PathBuf> = sprite_id_to_png_raw
            .into_iter()
            .map(|(id, p)| {
                let p = if p.is_absolute() { p } else { assets_root.join(p) };
                (id, p)
            })
            .collect();

        let map_events: HashMap<String, MapEvents> = data::load_ron(&data_dir.join("map_events.ron"))?;

        let item_display_names: HashMap<String, String> =
            data::load_ron(&data_dir.join("item_names.ron"))?;
        let item_prices: HashMap<String, u32> = data::load_ron(&data_dir.join("item_prices.ron"))?;
        let tm_prices: Vec<u32> = data::load_ron(&data_dir.join("tm_prices.ron"))?;

        let trainer_parties: HashMap<String, Vec<TrainerParty>> =
            data::load_ron(&data_dir.join("trainer_parties.ron"))?;
        let trainer_base_reward_money: HashMap<String, u32> =
            data::load_ron(&data_dir.join("trainer_rewards.ron"))?;

        let wild_encounters: HashMap<String, WildEncounterTable> =
            data::load_ron(&data_dir.join("wild_encounters.ron"))?;
        let wild_slot_thresholds = load_wild_slot_thresholds(assets_root)?;

        let move_display_names: HashMap<String, String> =
            data::load_ron(&data_dir.join("move_names.ron"))?;
        let move_db: HashMap<String, MoveData> = data::load_ron(&data_dir.join("moves.ron"))?;

        let pokemon_stats: HashMap<String, BaseStats> = data::load_ron(&data_dir.join("pokemon_stats.ron"))?;
        let pokemon_moves: HashMap<String, PokemonMoves> =
            data::load_ron(&data_dir.join("pokemon_moves.ron"))?;
        let pokemon_display_names: HashMap<String, String> = HashMap::new();

        let type_chart = load_type_chart(assets_root)?;

        let audio_pitches = Arc::new(load_audio_pitches(assets_root)?);
        let map_music_constants = load_map_music_constants(assets_root)?;

        Ok(Self {
            map_headers,
            map_events,
            map_id_to_name,
            tileset_bases,
            tileset_grass_tile_ids,
            tileset_collision,
            sprite_id_to_png,
            item_display_names,
            item_prices,
            tm_prices,
            trainer_parties,
            trainer_base_reward_money,
            wild_encounters,
            wild_slot_thresholds,
            move_display_names,
            move_db,
            pokemon_display_names,
            pokemon_stats,
            pokemon_moves,
            type_chart,
            audio_pitches,
            map_music_constants,
        })
    }
}
