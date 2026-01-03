// Packaged, Rust-owned music data.
//
// Music tracks are generated at build time into `OUT_DIR/packaged_music_gen.rs` so that runtime
// doesn't need to parse any source-format files.

use super::*;

include!(concat!(env!("OUT_DIR"), "/packaged_music_gen.rs"));
