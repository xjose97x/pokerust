//! Data loading infrastructure for RON-based game data.
//!
//! This module provides generic RON loading utilities and type definitions
//! for game data that has been converted from pokered ASM format.

pub mod item_constants;
pub mod map_constants;
pub mod move_constants;
pub mod pokemon_constants;
pub mod sprite_constants;
pub mod trainer_constants;

use serde::de::DeserializeOwned;
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::path::Path;

/// Load and deserialize a RON file into the specified type.
pub fn load_ron<T: DeserializeOwned>(path: &Path) -> Result<T, Box<dyn Error>> {
    let contents = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read `{}`: {e}", path.display()))?;
    let data: T = ron::from_str(&contents)
        .map_err(|e| format!("Failed to parse `{}`: {e}", path.display()))?;
    Ok(data)
}

/// Load a RON file containing a string-to-string mapping (e.g., names).
pub fn load_string_map(path: &Path) -> Result<HashMap<String, String>, Box<dyn Error>> {
    load_ron(path)
}

/// Load a RON file containing a string-to-u32 mapping (e.g., prices).
pub fn load_u32_map(path: &Path) -> Result<HashMap<String, u32>, Box<dyn Error>> {
    load_ron(path)
}

/// Load a RON file containing a string-to-u8 mapping (e.g., constants).
pub fn load_u8_map(path: &Path) -> Result<HashMap<String, u8>, Box<dyn Error>> {
    load_ron(path)
}

/// Load a RON file containing a vector of values.
pub fn load_vec<T: DeserializeOwned>(path: &Path) -> Result<Vec<T>, Box<dyn Error>> {
    load_ron(path)
}
