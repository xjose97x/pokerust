use std::{
    collections::HashMap,
    error::Error,
    path::{Path, PathBuf},
    sync::Arc,
};

use super::*;

pub struct GameData {
    pub map_id_to_name: HashMap<String, String>,
    pub sprite_constants: HashMap<String, u8>,
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
    pub pokemon_constant_ids: HashMap<String, u8>,
    pub pokemon_display_names: HashMap<String, String>,
    pub evos_moves_asm: String,
    pub evos_moves_ptrs: Vec<String>,
    pub type_chart: TypeChart,
    pub audio_pitches: Arc<Vec<u16>>,
    pub map_music_constants: HashMap<String, String>,
}

impl GameData {
    pub fn load(pokered_root: &Path) -> Result<Self, Box<dyn Error>> {
        let pokered_root = pokered_root.to_path_buf();

        let map_id_to_name = build_map_id_to_name(&pokered_root)?;
        let sprite_constants = load_sprite_constants(&pokered_root)?;
        let sprite_id_to_png = load_sprite_id_to_png(&pokered_root, &sprite_constants)?;

        let item_display_names = load_item_display_names(&pokered_root)?;
        let item_prices = load_item_prices(&pokered_root)?;
        let tm_prices = load_tm_prices(&pokered_root)?;

        let trainer_parties = load_trainer_parties(&pokered_root)?;
        let trainer_base_reward_money = load_trainer_base_reward_money(&pokered_root)?;

        let wild_encounters = load_wild_encounters(&pokered_root)?;
        let wild_slot_thresholds = load_wild_slot_thresholds(&pokered_root)?;

        let move_display_names = load_move_display_names(&pokered_root)?;
        let move_db = load_move_db(&pokered_root)?;

        let pokemon_constant_ids = load_pokemon_constant_ids(&pokered_root)?;
        let pokemon_display_names =
            load_pokemon_display_names(&pokered_root, &pokemon_constant_ids)?;

        let evos_moves_path = pokered_root.join("data/pokemon/evos_moves.asm");
        let evos_moves_asm = fs::read_to_string(&evos_moves_path)?;
        let evos_moves_ptrs = parse_evos_moves_pointer_table(&evos_moves_asm);

        let type_chart = load_type_chart(&pokered_root)?;

        let audio_pitches = Arc::new(load_audio_pitches(&pokered_root)?);
        let map_music_constants = load_map_music_constants(&pokered_root)?;

        Ok(Self {
            map_id_to_name,
            sprite_constants,
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
            pokemon_constant_ids,
            pokemon_display_names,
            evos_moves_asm,
            evos_moves_ptrs,
            type_chart,
            audio_pitches,
            map_music_constants,
        })
    }
}
