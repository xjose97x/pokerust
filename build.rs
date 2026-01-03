use std::{
    collections::HashMap,
    env, fs,
    path::{Path, PathBuf},
};

#[derive(Clone, Debug)]
enum MusicInstr {
    Tempo(u16),
    MasterVolume { left: u8, right: u8 },
    NoteType { speed: u8, volume: u8 },
    DrumSpeed(u8),
    DutyCycle(u8),
    Octave(u8),
    Note { note: u8, length: u8 },
    Rest(u8),
    DrumNote { instrument: u8, length: u8 },
    SoundCall(String),
    SoundLoop { count: u8, target: String },
    SoundRet,
}

#[derive(Clone, Debug)]
struct ChannelProgram {
    instructions: Vec<MusicInstr>,
    labels: HashMap<String, usize>,
}

#[derive(Clone, Debug)]
struct Song {
    channels: [Option<ChannelProgram>; 4],
}

fn strip_asm_comment(line: &str) -> &str {
    line.split_once(';').map(|(a, _)| a).unwrap_or(line)
}

fn parse_asm_i32(s: &str) -> Option<i32> {
    let s = s.trim();
    let (sign, digits) = if let Some(rest) = s.strip_prefix('-') {
        (-1i32, rest.trim())
    } else {
        (1i32, s)
    };

    let value = if let Some(hex) = digits.strip_prefix('$') {
        i32::from_str_radix(hex, 16).ok()?
    } else {
        digits.parse::<i32>().ok()?
    };
    Some(sign * value)
}

fn parse_asm_u8(s: &str) -> Option<u8> {
    let v = parse_asm_i32(s)?;
    u8::try_from(v).ok()
}

fn parse_asm_u16(s: &str) -> Option<u16> {
    let v = parse_asm_i32(s)?;
    u16::try_from(v).ok()
}

fn parse_music_note_name(note: &str) -> Option<u8> {
    match note.trim() {
        "C_" => Some(0),
        "C#" => Some(1),
        "D_" => Some(2),
        "D#" => Some(3),
        "E_" => Some(4),
        "F_" => Some(5),
        "F#" => Some(6),
        "G_" => Some(7),
        "G#" => Some(8),
        "A_" => Some(9),
        "A#" => Some(10),
        "B_" => Some(11),
        _ => None,
    }
}

fn parse_song_from_asm(song_label: &str, asm: &str) -> Song {
    #[derive(Clone, Debug)]
    struct Builder {
        scope: String,
        labels: HashMap<String, usize>,
        instrs: Vec<MusicInstr>,
    }

    let mut builders: [Option<Builder>; 4] = [None, None, None, None];
    let mut current_ch: Option<usize> = None;
    let mut current_scope: String = String::new();

    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.is_empty() {
            continue;
        }

        if line.ends_with(':') || line.ends_with("::") {
            let label = line
                .trim_end_matches(':')
                .trim()
                .trim_end_matches(':')
                .trim();
            if label.starts_with(song_label) {
                if let Some(rest) = label.strip_prefix(song_label) {
                    if let Some(ch_part) = rest.strip_prefix("_Ch") {
                        let ch_num = ch_part
                            .chars()
                            .next()
                            .and_then(|c| c.to_digit(10))
                            .map(|d| d as usize);
                        if let Some(ch_num) = ch_num {
                            if (1..=4).contains(&ch_num) {
                                let idx = ch_num - 1;
                                current_ch = Some(idx);
                                current_scope = label.to_string();
                                let b = builders[idx].get_or_insert_with(|| Builder {
                                    scope: current_scope.clone(),
                                    labels: HashMap::new(),
                                    instrs: Vec::new(),
                                });
                                b.scope = current_scope.clone();
                                b.labels.insert(label.to_string(), b.instrs.len());
                                continue;
                            }
                        }
                    }
                }
            }

            if let Some(idx) = current_ch {
                let b = builders[idx].get_or_insert_with(|| Builder {
                    scope: current_scope.clone(),
                    labels: HashMap::new(),
                    instrs: Vec::new(),
                });
                let full = if label.starts_with('.') {
                    format!("{}{label}", b.scope)
                } else {
                    label.to_string()
                };
                b.labels.insert(full, b.instrs.len());
            }
            continue;
        }

        let Some(idx) = current_ch else {
            continue;
        };
        let b = builders[idx].get_or_insert_with(|| Builder {
            scope: current_scope.clone(),
            labels: HashMap::new(),
            instrs: Vec::new(),
        });

        let (cmd, rest) = line.split_once(' ').unwrap_or((line, ""));
        let args = rest.trim();
        match cmd {
            "tempo" => {
                if let Some(v) = args.split_whitespace().next().and_then(parse_asm_u16) {
                    b.instrs.push(MusicInstr::Tempo(v));
                }
            }
            "volume" => {
                let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
                if parts.len() >= 2 {
                    if let (Some(l), Some(r)) = (parse_asm_u8(parts[0]), parse_asm_u8(parts[1])) {
                        b.instrs
                            .push(MusicInstr::MasterVolume { left: l, right: r });
                    }
                }
            }
            "note_type" => {
                let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
                if parts.len() >= 2 {
                    if let (Some(speed), Some(vol)) =
                        (parse_asm_u8(parts[0]), parse_asm_u8(parts[1]))
                    {
                        b.instrs.push(MusicInstr::NoteType { speed, volume: vol });
                    }
                }
            }
            "drum_speed" => {
                if let Some(speed) = args.split_whitespace().next().and_then(parse_asm_u8) {
                    b.instrs.push(MusicInstr::DrumSpeed(speed));
                }
            }
            "duty_cycle" => {
                if let Some(v) = args.split_whitespace().next().and_then(parse_asm_u8) {
                    b.instrs.push(MusicInstr::DutyCycle(v));
                }
            }
            "octave" => {
                if let Some(v) = args.split_whitespace().next().and_then(parse_asm_u8) {
                    b.instrs.push(MusicInstr::Octave(v));
                }
            }
            "note" => {
                let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
                if parts.len() >= 2 {
                    if let (Some(note), Some(length)) =
                        (parse_music_note_name(parts[0]), parse_asm_u8(parts[1]))
                    {
                        b.instrs.push(MusicInstr::Note { note, length });
                    }
                }
            }
            "rest" => {
                if let Some(length) = args.split_whitespace().next().and_then(parse_asm_u8) {
                    b.instrs.push(MusicInstr::Rest(length));
                }
            }
            "drum_note" => {
                let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
                if parts.len() >= 2 {
                    if let (Some(inst), Some(length)) =
                        (parse_asm_u8(parts[0]), parse_asm_u8(parts[1]))
                    {
                        b.instrs.push(MusicInstr::DrumNote {
                            instrument: inst,
                            length,
                        });
                    }
                }
            }
            "sound_call" => {
                let target = args.split_whitespace().next().unwrap_or("");
                if !target.is_empty() {
                    let full = if target.starts_with('.') {
                        format!("{}{target}", b.scope)
                    } else {
                        target.to_string()
                    };
                    b.instrs.push(MusicInstr::SoundCall(full));
                }
            }
            "sound_loop" => {
                let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
                if parts.len() >= 2 {
                    let count = parse_asm_u8(parts[0]).unwrap_or(0);
                    let target = parts[1];
                    if !target.is_empty() {
                        let full = if target.starts_with('.') {
                            format!("{}{target}", b.scope)
                        } else {
                            target.to_string()
                        };
                        b.instrs.push(MusicInstr::SoundLoop {
                            count,
                            target: full,
                        });
                    }
                }
            }
            "sound_ret" => {
                b.instrs.push(MusicInstr::SoundRet);
            }
            _ => {}
        }
    }

    let mut channels: [Option<ChannelProgram>; 4] = [None, None, None, None];
    for (i, b) in builders.into_iter().enumerate() {
        if let Some(b) = b {
            channels[i] = Some(ChannelProgram {
                instructions: b.instrs,
                labels: b.labels,
            });
        }
    }
    Song { channels }
}

fn parse_music_const_to_label(asm: &str) -> Vec<(String, String)> {
    let mut out: Vec<(String, String)> = Vec::new();
    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        let Some(rest) = line.strip_prefix("music_const") else {
            continue;
        };
        let args = rest.trim();
        let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
        if parts.len() < 2 {
            continue;
        }
        let k = parts[0];
        let v = parts[1];
        if k.starts_with("MUSIC_") && v.starts_with("Music_") {
            out.push((k.to_string(), v.to_string()));
        }
    }
    out
}

fn index_music_files(pokered_music_dir: &Path) -> Result<HashMap<String, PathBuf>, String> {
    let mut out: HashMap<String, PathBuf> = HashMap::new();
    let entries = fs::read_dir(pokered_music_dir).map_err(|e| e.to_string())?;
    for entry in entries {
        let entry = entry.map_err(|e| e.to_string())?;
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("asm") {
            continue;
        }
        let asm = fs::read_to_string(&path).map_err(|e| e.to_string())?;
        for raw_line in asm.lines() {
            let line = strip_asm_comment(raw_line).trim();
            if !(line.ends_with(':') || line.ends_with("::")) {
                continue;
            }
            let label = line
                .trim_end_matches(':')
                .trim()
                .trim_end_matches(':')
                .trim();
            if !label.starts_with("Music_") {
                continue;
            }
            let Some((base, rest)) = label.split_once("_Ch") else {
                continue;
            };
            if rest.is_empty() {
                continue;
            }
            out.entry(base.to_string()).or_insert_with(|| path.clone());
        }
    }
    Ok(out)
}

fn escape_rust_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('\"', "\\\"")
}

fn emit_instr(instr: &MusicInstr) -> String {
    match instr {
        MusicInstr::Tempo(v) => format!("MusicInstr::Tempo({v})"),
        MusicInstr::MasterVolume { left, right } => {
            format!("MusicInstr::MasterVolume {{ left: {left}, right: {right} }}")
        }
        MusicInstr::NoteType { speed, volume } => {
            format!("MusicInstr::NoteType {{ speed: {speed}, volume: {volume} }}")
        }
        MusicInstr::DrumSpeed(v) => format!("MusicInstr::DrumSpeed({v})"),
        MusicInstr::DutyCycle(v) => format!("MusicInstr::DutyCycle({v})"),
        MusicInstr::Octave(v) => format!("MusicInstr::Octave({v})"),
        MusicInstr::Note { note, length } => {
            format!("MusicInstr::Note {{ note: {note}, length: {length} }}")
        }
        MusicInstr::Rest(v) => format!("MusicInstr::Rest({v})"),
        MusicInstr::DrumNote { instrument, length } => {
            format!("MusicInstr::DrumNote {{ instrument: {instrument}, length: {length} }}")
        }
        MusicInstr::SoundCall(target) => {
            format!("MusicInstr::SoundCall(\"{}\")", escape_rust_string(target))
        }
        MusicInstr::SoundLoop { count, target } => format!(
            "MusicInstr::SoundLoop {{ count: {count}, target: \"{}\" }}",
            escape_rust_string(target)
        ),
        MusicInstr::SoundRet => "MusicInstr::SoundRet".to_string(),
    }
}

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let pokered_root = manifest_dir.join("../pokered");
    let music_constants_path = pokered_root.join("constants/music_constants.asm");
    let pokered_music_dir = pokered_root.join("audio/music");

    println!("cargo:rerun-if-changed={}", music_constants_path.display());
    if let Ok(entries) = fs::read_dir(&pokered_music_dir) {
        for entry in entries.flatten() {
            println!("cargo:rerun-if-changed={}", entry.path().display());
        }
    }

    let constants_asm = fs::read_to_string(&music_constants_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", music_constants_path.display()));
    let music_const_to_label = parse_music_const_to_label(&constants_asm);

    let label_to_file = index_music_files(&pokered_music_dir).unwrap_or_else(|e| {
        panic!(
            "Failed to index music files in {}: {e}",
            pokered_music_dir.display()
        )
    });

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let out_path = out_dir.join("packaged_music_gen.rs");

    let mut out = String::new();
    out.push_str("// @generated by build.rs\n");
    out.push_str("// Packaged music data generated from local sources.\n\n");

    for (music_const, label) in &music_const_to_label {
        let Some(path) = label_to_file.get(label) else {
            continue;
        };
        let asm = fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {e}", path.display()));
        let song = parse_song_from_asm(label, &asm);

        out.push_str(&format!("static {music_const}: Song = Song {{\n"));
        out.push_str(&format!("    label: \"{music_const}\",\n"));
        out.push_str("    channels: [\n");
        for ch in 0..4 {
            match &song.channels[ch] {
                None => out.push_str("        None,\n"),
                Some(prog) => {
                    let mut labels: Vec<(String, usize)> =
                        prog.labels.iter().map(|(k, v)| (k.clone(), *v)).collect();
                    labels.sort_by(|a, b| a.1.cmp(&b.1).then_with(|| a.0.cmp(&b.0)));

                    out.push_str("        Some(ChannelProgram {\n");
                    out.push_str("            instructions: &[\n");
                    for instr in &prog.instructions {
                        out.push_str("                ");
                        out.push_str(&emit_instr(instr));
                        out.push_str(",\n");
                    }
                    out.push_str("            ],\n");
                    out.push_str("            labels: &[\n");
                    for (name, pc) in &labels {
                        out.push_str(&format!(
                            "                (\"{}\", {pc}),\n",
                            escape_rust_string(name)
                        ));
                    }
                    out.push_str("            ],\n");
                    out.push_str("        }),\n");
                }
            }
        }
        out.push_str("    ],\n");
        out.push_str("};\n\n");
    }

    out.push_str("pub fn song_for_music_const(music_const: &str) -> Option<&'static Song> {\n");
    out.push_str("    match music_const {\n");
    for (music_const, _label) in &music_const_to_label {
        out.push_str(&format!(
            "        \"{music_const}\" => Some(&{music_const}),\n"
        ));
    }
    out.push_str("        _ => None,\n");
    out.push_str("    }\n");
    out.push_str("}\n");

    fs::write(&out_path, out)
        .unwrap_or_else(|e| panic!("Failed to write {}: {e}", out_path.display()));
}
