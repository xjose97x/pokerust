use std::{
    collections::HashMap,
    collections::HashSet,
    collections::VecDeque,
    error::Error,
    fs,
    num::NonZeroU32,
    path::{Path, PathBuf},
    rc::Rc,
    sync::{Arc, mpsc},
    time::{Duration, Instant},
};

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use image::GenericImageView;
use serde::{Deserialize, Serialize};
use winit::{
    dpi::PhysicalSize,
    event::{ElementState, Event, KeyEvent, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    keyboard::{Key, NamedKey},
    window::WindowBuilder,
};

mod data;
mod game_data;
mod packaged_data;
mod packaged_music;
mod packaged_type_chart;

const GB_WIDTH: u32 = 160;
const GB_HEIGHT: u32 = 144;
const TILE_SIZE: u32 = 8;
const SCREEN_TILES_W: u32 = GB_WIDTH / TILE_SIZE;
const SCREEN_TILES_H: u32 = GB_HEIGHT / TILE_SIZE;
const BLOCK_TILES: u32 = 4;
const PLAYER_W_TILES: i32 = 2;
const PLAYER_H_TILES: i32 = 2;
const PLAYER_STEP_TILES: i32 = 2;
const PLAYER_FRAME_SIZE_PX: u32 = 16;
const PLAYER_STEP_PX: i32 = PLAYER_STEP_TILES * TILE_SIZE as i32;
const PLAYER_SPEED_PX_PER_TICK: i32 = 2;
const CAMERA_MARGIN_TILES_X: i32 = 6;
const CAMERA_MARGIN_TILES_Y: i32 = 4;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Mode {
    Title,
    Map,
    Battle,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
enum Facing {
    Down,
    Up,
    Left,
    Right,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum WarpDest {
    LastMap,
    MapId(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct WarpEvent {
    x: i32,
    y: i32,
    dest_map: WarpDest,
    dest_warp_id: usize,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct ObjectEvent {
    sprite_id: u8,
    tx: i32,
    ty: i32,
    kind: ObjectKind,
    movement: ObjectMovement,
    movement_range: ObjectMovementRange,
    text_id: String,
    facing: Facing,
    #[serde(default, skip_serializing_if = "is_zero_u8")]
    walk_intra_counter: u8,
    #[serde(default, skip_serializing_if = "is_zero_u8")]
    walk_anim_frame: u8,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    move_anim: Option<MoveAnim>,
    #[serde(default, skip_serializing_if = "is_zero_u8")]
    move_delay: u8,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct BgEvent {
    tx: i32,
    ty: i32,
    text_id: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct HiddenEvent {
    tx: i32,
    ty: i32,
    facing: Option<Facing>,
    action: HiddenAction,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum HiddenAction {
    OpenPokemonCenterPC,
    OpenRedsPC,
    Other(String),
}

struct Dialog {
    lines: Vec<String>,
    cursor: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum MenuScreen {
    Root,
    Items,
    Party,
    Options,
    Pokedex { cursor: usize, scroll: usize },
    TeachMove { item_id: String },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct GameOptions {
    text_speed: TextSpeed,
    battle_animations: bool,
    sound_enabled: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
enum TextSpeed {
    Fast,
    Medium,
    Slow,
}

impl Default for GameOptions {
    fn default() -> Self {
        Self {
            text_speed: TextSpeed::Medium,
            battle_animations: true,
            sound_enabled: true,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
enum GrowthRate {
    #[serde(rename = "GROWTH_MEDIUM_FAST")]
    MediumFast,
    #[serde(rename = "GROWTH_SLIGHTLY_FAST")]
    SlightlyFast,
    #[serde(rename = "GROWTH_SLIGHTLY_SLOW")]
    SlightlySlow,
    #[serde(rename = "GROWTH_MEDIUM_SLOW")]
    MediumSlow,
    #[serde(rename = "GROWTH_FAST")]
    Fast,
    #[serde(rename = "GROWTH_SLOW")]
    Slow,
}

impl GrowthRate {
    fn from_const(s: &str) -> Option<Self> {
        match s {
            "GROWTH_MEDIUM_FAST" => Some(Self::MediumFast),
            "GROWTH_SLIGHTLY_FAST" => Some(Self::SlightlyFast),
            "GROWTH_SLIGHTLY_SLOW" => Some(Self::SlightlySlow),
            "GROWTH_MEDIUM_SLOW" => Some(Self::MediumSlow),
            "GROWTH_FAST" => Some(Self::Fast),
            "GROWTH_SLOW" => Some(Self::Slow),
            _ => None,
        }
    }
}

fn exp_for_level(growth: GrowthRate, level: u8) -> u32 {
    if level <= 1 {
        return 0;
    }
    let n = level as i64;

    let (a, b, c, d, e): (i64, i64, i64, i64, i64) = match growth {
        GrowthRate::MediumFast => (1, 1, 0, 0, 0),
        GrowthRate::SlightlyFast => (3, 4, 10, 0, 30),
        GrowthRate::SlightlySlow => (3, 4, 20, 0, 70),
        GrowthRate::MediumSlow => (6, 5, -15, 100, 140),
        GrowthRate::Fast => (4, 5, 0, 0, 0),
        GrowthRate::Slow => (5, 4, 0, 0, 0),
    };

    let n2 = n * n;
    let n3 = n2 * n;
    let mut v = (a * n3) / b + c * n2 + d * n - e;
    if v < 0 {
        v = 0;
    }
    v as u32
}

fn level_from_exp(exp: u32, growth: GrowthRate) -> u8 {
    let mut level: u8 = 1;
    while level < 100 {
        let next = level + 1;
        if exp < exp_for_level(growth, next) {
            break;
        }
        level = next;
    }
    level
}

struct MenuState {
    screen: MenuScreen,
    selection: usize,
    scroll: usize,
}

#[derive(Clone, Debug)]
struct ShopItem {
    item_id: String,
    price: u32,
}

struct ShopState {
    items: Vec<ShopItem>,
    selection: usize,
    scroll: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PcLocation {
    PokemonCenter,
    RedsHouse,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PcScreen {
    Root,
    BillsPc,
    Withdraw,
    Deposit,
    Release,
    ChangeBox,
    PlayersPc,
    WithdrawItem,
    DepositItem,
    TossItem,
}

#[derive(Clone, Debug)]
struct PcState {
    location: PcLocation,
    screen: PcScreen,
    selection: usize,
    scroll: usize,
}

#[derive(Clone, Debug)]
enum ChoiceKind {
    Starter { species: String },
    ReleaseBoxMon { box_index: usize, index: usize },
    TossPcItem { item_id: String },
}

#[derive(Clone, Debug)]
struct ChoiceState {
    kind: ChoiceKind,
    selection: usize,
}

#[derive(Clone, Debug)]
struct PlayerMon {
    species: String,
    level: u8,
    hp: u16,
    max_hp: u16,
    exp: u32,
    moves: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct TrainerPartyMon {
    level: u8,
    species: String,
}

type TrainerParty = Vec<TrainerPartyMon>;
type TypeChart = HashMap<String, HashMap<String, u8>>;

#[derive(Clone, Debug, Serialize, Deserialize)]
struct WildMonSlot {
    level: u8,
    species: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct WildEncounterTable {
    grass_rate: u8,
    grass: Vec<WildMonSlot>,
    water_rate: u8,
    water: Vec<WildMonSlot>,
}

#[derive(Clone, Debug)]
struct Song {
    #[allow(dead_code)]
    label: &'static str,
    channels: [Option<ChannelProgram>; 4],
}

enum AudioCommand {
    PlaySong(Option<&'static Song>),
    SetVolume(f32),
}

struct AudioEngine {
    tx: mpsc::Sender<AudioCommand>,
    _stream: cpal::Stream,
}

struct MusicRuntime {
    song: &'static Song,
    pitches: Arc<Vec<u16>>,
    tempo: u16,
    master_left: u8,
    master_right: u8,
    channels: [ChannelRuntime; 4],
}

struct ChannelRuntime {
    active: bool,
    pc: usize,
    call_stack: Vec<usize>,
    loop_counter: u8,
    delay: u8,
    frac: u8,
    octave: u8,
    note_speed: u8,
    note_volume: u8,
    duty: u8,
    freq_hz: f32,
    phase: f32,
    noise_hz: f32,
    noise_phase: f32,
    noise_lfsr: u32,
    note_on: bool,
}

impl ChannelRuntime {
    fn new(active: bool) -> Self {
        Self {
            active,
            pc: 0,
            call_stack: Vec::new(),
            loop_counter: 1,
            delay: 1,
            frac: 0,
            octave: 4,
            note_speed: 1,
            note_volume: 8,
            duty: 2,
            freq_hz: 0.0,
            phase: 0.0,
            noise_hz: 8_000.0,
            noise_phase: 0.0,
            noise_lfsr: 0xACE1u32,
            note_on: false,
        }
    }
}

impl MusicRuntime {
    fn new(song: &'static Song, pitches: Arc<Vec<u16>>) -> Self {
        let mut channels = std::array::from_fn(|i| ChannelRuntime::new(song.channels[i].is_some()));
        for c in &mut channels {
            c.delay = 1;
            c.frac = 0;
            c.loop_counter = 1;
            c.call_stack.clear();
            c.pc = 0;
            c.note_speed = 1;
            c.note_volume = 8;
            c.duty = 2;
            c.octave = 4;
            c.freq_hz = 0.0;
            c.phase = 0.0;
            c.note_on = false;
        }
        Self {
            song,
            pitches,
            tempo: 0x0100,
            master_left: 7,
            master_right: 7,
            channels,
        }
    }

    fn tick(&mut self) {
        for i in 0..4 {
            if !self.channels[i].active {
                continue;
            }
            let has_prog = self.song.channels[i].is_some();
            if !has_prog {
                self.channels[i].active = false;
                self.channels[i].note_on = false;
                continue;
            }

            if self.channels[i].delay == 1 {
                self.play_next_note(i);
            } else {
                let d = self.channels[i].delay;
                self.channels[i].delay = d.wrapping_sub(1);
            }
        }
    }

    fn play_next_note(&mut self, channel_index: usize) {
        let Some(program) = self.song.channels[channel_index] else {
            self.channels[channel_index].active = false;
            self.channels[channel_index].note_on = false;
            return;
        };

        let mut steps = 0usize;
        while steps < 10_000 {
            steps += 1;
            if self.channels[channel_index].pc >= program.instructions.len() {
                self.channels[channel_index].active = false;
                self.channels[channel_index].note_on = false;
                return;
            }

            let instr = &program.instructions[self.channels[channel_index].pc];
            self.channels[channel_index].pc += 1;
            match instr {
                MusicInstr::Tempo(v) => {
                    self.tempo = *v;
                    for ch in &mut self.channels {
                        ch.frac = 0;
                    }
                }
                MusicInstr::MasterVolume { left, right } => {
                    self.master_left = (*left).min(7);
                    self.master_right = (*right).min(7);
                }
                MusicInstr::NoteType { speed, volume } => {
                    self.channels[channel_index].note_speed = (*speed).min(15);
                    self.channels[channel_index].note_volume = (*volume).min(15);
                }
                MusicInstr::DrumSpeed(speed) => {
                    self.channels[channel_index].note_speed = (*speed).min(15);
                }
                MusicInstr::DutyCycle(v) => {
                    self.channels[channel_index].duty = (*v).min(3);
                }
                MusicInstr::Octave(v) => {
                    self.channels[channel_index].octave = (*v).clamp(1, 8);
                }
                MusicInstr::SoundCall(target) => {
                    if let Some(dest) = program.label_pc(target) {
                        let ret = self.channels[channel_index].pc;
                        self.channels[channel_index].call_stack.push(ret);
                        self.channels[channel_index].pc = dest;
                    }
                }
                MusicInstr::SoundLoop { count, target } => {
                    if let Some(dest) = program.label_pc(target) {
                        if *count == 0 {
                            self.channels[channel_index].pc = dest;
                        } else if self.channels[channel_index].loop_counter == *count {
                            self.channels[channel_index].loop_counter = 1;
                        } else {
                            self.channels[channel_index].loop_counter =
                                self.channels[channel_index].loop_counter.wrapping_add(1);
                            self.channels[channel_index].pc = dest;
                        }
                    }
                }
                MusicInstr::SoundRet => {
                    if let Some(ret) = self.channels[channel_index].call_stack.pop() {
                        self.channels[channel_index].pc = ret;
                    } else {
                        self.channels[channel_index].active = false;
                        self.channels[channel_index].note_on = false;
                        return;
                    }
                }
                MusicInstr::Note { note, length } => {
                    let hz = gb_note_hz(
                        &self.pitches,
                        *note,
                        self.channels[channel_index].octave,
                        channel_index == 2,
                    );
                    self.channels[channel_index].freq_hz = hz;
                    self.channels[channel_index].phase = 0.0;
                    self.channels[channel_index].note_on = hz > 0.0;
                    self.set_note_delay(channel_index, *length);
                    return;
                }
                MusicInstr::Rest(length) => {
                    self.channels[channel_index].note_on = false;
                    self.set_note_delay(channel_index, *length);
                    return;
                }
                MusicInstr::DrumNote { instrument, length } => {
                    self.channels[channel_index].note_on = true;
                    self.channels[channel_index].noise_hz =
                        600.0 + (*instrument as f32).clamp(1.0, 19.0) * 180.0;
                    self.channels[channel_index].noise_phase = 0.0;
                    self.channels[channel_index].noise_lfsr =
                        0xACE1u32 ^ (*instrument as u32).wrapping_mul(1_103_515_245);
                    self.set_note_delay(channel_index, *length);
                    return;
                }
            }
        }

        self.channels[channel_index].active = false;
        self.channels[channel_index].note_on = false;
    }

    fn set_note_delay(&mut self, channel_index: usize, length: u8) {
        let note_length = length.clamp(1, 16) as u16;
        let note_speed = self.channels[channel_index].note_speed as u16;
        let product = note_length * note_speed;
        let total = self.channels[channel_index].frac as u16 + product * self.tempo;
        self.channels[channel_index].frac = (total & 0xff) as u8;
        self.channels[channel_index].delay = (total >> 8) as u8;
    }

    fn render_stereo_sample(&mut self, sample_rate: u32) -> (f32, f32) {
        let mut mix = 0.0f32;
        for (i, ch) in self.channels.iter_mut().enumerate() {
            if !ch.active || !ch.note_on {
                continue;
            }
            let amp = match i {
                0 | 1 => (ch.note_volume as f32) / 15.0 * 0.18,
                2 => {
                    let v = match ch.note_volume {
                        0 => 0.0,
                        1 => 1.0,
                        2 => 0.5,
                        3 => 0.25,
                        other => (other as f32) / 15.0,
                    };
                    v * 0.14
                }
                3 => 0.12,
                _ => 0.0,
            };
            mix += amp * ch_sample(i, ch, sample_rate);
        }

        let left = (self.master_left as f32) / 7.0;
        let right = (self.master_right as f32) / 7.0;
        let l = (mix * left).clamp(-1.0, 1.0);
        let r = (mix * right).clamp(-1.0, 1.0);
        (l, r)
    }
}

fn ch_sample(channel_index: usize, ch: &mut ChannelRuntime, sample_rate: u32) -> f32 {
    let sr = sample_rate as f32;
    match channel_index {
        0 | 1 => {
            let hz = ch.freq_hz;
            if hz <= 0.0 {
                return 0.0;
            }
            ch.phase += hz / sr;
            if ch.phase >= 1.0 {
                ch.phase -= 1.0;
            }
            let duty = match ch.duty {
                0 => 0.125,
                1 => 0.25,
                2 => 0.5,
                3 => 0.75,
                _ => 0.5,
            };
            if ch.phase < duty { 1.0 } else { -1.0 }
        }
        2 => {
            let hz = ch.freq_hz;
            if hz <= 0.0 {
                return 0.0;
            }
            ch.phase += hz / sr;
            if ch.phase >= 1.0 {
                ch.phase -= 1.0;
            }
            let t = ch.phase;
            2.0 * (2.0 * t - 1.0).abs() - 1.0
        }
        3 => {
            let hz = ch.noise_hz.max(1.0);
            ch.noise_phase += hz / sr;
            while ch.noise_phase >= 1.0 {
                ch.noise_phase -= 1.0;
                let lsb = ch.noise_lfsr & 1;
                ch.noise_lfsr >>= 1;
                if lsb != 0 {
                    ch.noise_lfsr ^= 0xB400;
                }
            }
            if (ch.noise_lfsr & 1) == 0 { -1.0 } else { 1.0 }
        }
        _ => 0.0,
    }
}

struct MusicPlayer {
    pitches: Arc<Vec<u16>>,
    sample_rate: u32,
    tick_accum: u32,
    volume: f32,
    runtime: Option<MusicRuntime>,
}

impl MusicPlayer {
    fn new(pitches: Arc<Vec<u16>>, sample_rate: u32) -> Self {
        Self {
            pitches,
            sample_rate,
            tick_accum: 0,
            volume: 0.25,
            runtime: None,
        }
    }

    fn apply_command(&mut self, cmd: AudioCommand) {
        match cmd {
            AudioCommand::PlaySong(song) => {
                self.runtime = song.map(|s| MusicRuntime::new(s, self.pitches.clone()));
            }
            AudioCommand::SetVolume(v) => {
                self.volume = v.clamp(0.0, 1.0);
            }
        }
    }

    fn next_stereo_sample(&mut self) -> (f32, f32) {
        self.tick_accum = self.tick_accum.wrapping_add(60);
        if self.tick_accum >= self.sample_rate {
            self.tick_accum -= self.sample_rate;
            if let Some(rt) = &mut self.runtime {
                rt.tick();
            }
        }

        let (l, r) = if let Some(rt) = &mut self.runtime {
            rt.render_stereo_sample(self.sample_rate)
        } else {
            (0.0, 0.0)
        };

        (l * self.volume, r * self.volume)
    }
}

fn gb_note_hz(pitches: &[u16], note: u8, octave: u8, wave_channel: bool) -> f32 {
    let idx = note as usize;
    if idx >= pitches.len() {
        return 0.0;
    }
    let base = pitches[idx];
    let shift = octave.saturating_sub(1) as u32;
    let shifted = ((base as i16) >> shift) as u16;
    let reg = shifted & 0x7ff;
    let denom = 2048.0 - reg as f32;
    if denom <= 1.0 {
        return 0.0;
    }
    let numerator = if wave_channel { 65_536.0 } else { 131_072.0 };
    numerator / denom
}

impl AudioEngine {
    fn new(pitches: Arc<Vec<u16>>) -> Result<Self, Box<dyn Error>> {
        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .ok_or("No output audio device available")?;
        let supported = device.default_output_config()?;
        let sample_format = supported.sample_format();
        let config: cpal::StreamConfig = supported.config();
        let channels = config.channels as usize;
        let sample_rate = config.sample_rate.0;

        let (tx, rx) = mpsc::channel::<AudioCommand>();
        let err_fn = |err| eprintln!("audio stream error: {err}");

        let stream = match sample_format {
            cpal::SampleFormat::F32 => {
                let mut player = MusicPlayer::new(pitches, sample_rate);
                device.build_output_stream(
                    &config,
                    move |data: &mut [f32], _| {
                        for cmd in rx.try_iter() {
                            player.apply_command(cmd);
                        }
                        for frame in data.chunks_mut(channels) {
                            let (l, r) = player.next_stereo_sample();
                            if channels == 1 {
                                frame[0] = (l + r) * 0.5;
                            } else {
                                frame[0] = l;
                                frame[1] = r;
                                for s in frame.iter_mut().skip(2) {
                                    *s = (l + r) * 0.5;
                                }
                            }
                        }
                    },
                    err_fn,
                    None,
                )?
            }
            cpal::SampleFormat::I16 => {
                let mut player = MusicPlayer::new(pitches, sample_rate);
                device.build_output_stream(
                    &config,
                    move |data: &mut [i16], _| {
                        for cmd in rx.try_iter() {
                            player.apply_command(cmd);
                        }
                        for frame in data.chunks_mut(channels) {
                            let (l, r) = player.next_stereo_sample();
                            let mono = (l + r) * 0.5;
                            let to_i16 = |v: f32| -> i16 {
                                let v = v.clamp(-1.0, 1.0);
                                (v * i16::MAX as f32) as i16
                            };
                            if channels == 1 {
                                frame[0] = to_i16(mono);
                            } else {
                                frame[0] = to_i16(l);
                                frame[1] = to_i16(r);
                                for s in frame.iter_mut().skip(2) {
                                    *s = to_i16(mono);
                                }
                            }
                        }
                    },
                    err_fn,
                    None,
                )?
            }
            cpal::SampleFormat::U16 => {
                let mut player = MusicPlayer::new(pitches, sample_rate);
                device.build_output_stream(
                    &config,
                    move |data: &mut [u16], _| {
                        for cmd in rx.try_iter() {
                            player.apply_command(cmd);
                        }
                        for frame in data.chunks_mut(channels) {
                            let (l, r) = player.next_stereo_sample();
                            let mono = (l + r) * 0.5;
                            let to_u16 = |v: f32| -> u16 {
                                let v = v.clamp(-1.0, 1.0);
                                let v = v * 0.5 + 0.5;
                                (v * u16::MAX as f32) as u16
                            };
                            if channels == 1 {
                                frame[0] = to_u16(mono);
                            } else {
                                frame[0] = to_u16(l);
                                frame[1] = to_u16(r);
                                for s in frame.iter_mut().skip(2) {
                                    *s = to_u16(mono);
                                }
                            }
                        }
                    },
                    err_fn,
                    None,
                )?
            }
            other => {
                return Err(format!("Unsupported audio sample format: {other:?}").into());
            }
        };

        stream.play()?;
        Ok(Self {
            tx,
            _stream: stream,
        })
    }

    fn play_song(&self, song: Option<&'static Song>) {
        let _ = self.tx.send(AudioCommand::PlaySong(song));
    }

    fn set_volume(&self, volume: f32) {
        let _ = self.tx.send(AudioCommand::SetVolume(volume));
    }
}

#[derive(Clone, Copy, Debug)]
struct ChannelProgram {
    instructions: &'static [MusicInstr],
    labels: &'static [(&'static str, usize)],
}

impl ChannelProgram {
    fn label_pc(&self, target: &str) -> Option<usize> {
        for (name, pc) in self.labels {
            if *name == target {
                return Some(*pc);
            }
        }
        None
    }
}

#[derive(Clone, Copy, Debug)]
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
    SoundCall(&'static str),
    SoundLoop { count: u8, target: &'static str },
    SoundRet,
}

#[derive(Clone, Debug)]
enum PendingBattle {
    Trainer {
        map_name: String,
        text_id: String,
        opponent: String,
        trainer_number: String,
    },
    Wild {
        map_name: String,
    },
}

#[derive(Clone, Debug)]
enum BattleKind {
    Trainer {
        map_name: String,
        text_id: String,
        opponent: String,
    },
    Wild {
        map_name: String,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BattleUiScreen {
    Command,
    Fight,
    Party,
    Items,
    LearnMove,
    Evolution,
}

struct BattleUi {
    screen: BattleUiScreen,
    selection: usize,
    scroll: usize,
    move_to_learn: Option<String>,
    evolution_target: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StatusCondition {
    None,
    Paralysis,
    Poison,
    Burn,
    Freeze,
    Sleep(u8), // Sleep counter (0-7 turns)
}

struct BattleMon {
    species: String,
    level: u8,
    types: (String, String),
    max_hp: u16,
    hp: u16,
    attack: u16,
    defense: u16,
    speed: u16,
    special: u16,
    catch_rate: u8,
    base_exp: u8,
    moves: Vec<String>,
    move_pp: Vec<u8>, // Current PP for each move
    front_sprite: GrayscaleImage,
    back_sprite: GrayscaleImage,
    status: StatusCondition,
    // Stat stages: -6 to +6, reset on switch-out
    stat_stage_attack: i8,
    stat_stage_defense: i8,
    stat_stage_speed: i8,
    stat_stage_special: i8,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BattleResult {
    Win,
    Lose,
    Run,
    Caught,
}

#[derive(Clone, Debug)]
enum BattleEvent {
    Message(Vec<String>),
    PlayerAttack { move_id: String },
    EnemyAttack { move_id: String },
    SendOutNextEnemy,
    SendOutNextPlayer,
    EndBattle { result: BattleResult },
    LearnMove { move_id: String },
    Evolution { target_species: String },
}

struct BattleState {
    kind: BattleKind,
    player: BattleMon,
    player_party: Vec<PlayerMon>,
    player_party_index: usize,
    inventory: HashMap<String, u32>,
    enemy: BattleMon,
    enemy_party: Vec<TrainerPartyMon>,
    enemy_party_index: usize,
    ui: BattleUi,
    dialog: Option<Dialog>,
    events: VecDeque<BattleEvent>,
    rng: u32,
    pokedex_seen: HashSet<String>,
    pokedex_caught: HashSet<String>,
}

const ROOT_MENU_ITEMS: [&str; 6] = ["POKéDEX", "POKéMON", "ITEM", "SAVE", "OPTION", "EXIT"];
const ITEMS_VISIBLE_ROWS: usize = 12;
const BATTLE_ITEMS_VISIBLE_ROWS: usize = 4;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
enum ObjectMovement {
    Walk,
    Stay,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
enum ObjectMovementRange {
    AnyDir,
    UpDown,
    LeftRight,
    None,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
enum ObjectKind {
    Person,
    Item {
        item_id: String,
    },
    Trainer {
        opponent: String,
        trainer_number: String,
    },
}

impl Dialog {
    fn has_more(&self) -> bool {
        self.cursor + 2 < self.lines.len()
    }

    fn advance(&mut self) -> bool {
        if self.has_more() {
            self.cursor += 2;
            false
        } else {
            true
        }
    }
}

#[derive(Clone, Debug, Default)]
struct MapConnections {
    north: Option<MapConnection>,
    south: Option<MapConnection>,
    west: Option<MapConnection>,
    east: Option<MapConnection>,
}

impl MapConnections {
    fn connection_for_exit(&self, dir: Facing) -> Option<&MapConnection> {
        match dir {
            Facing::Up => self.north.as_ref(),
            Facing::Down => self.south.as_ref(),
            Facing::Left => self.west.as_ref(),
            Facing::Right => self.east.as_ref(),
        }
    }

    fn connection_for_oob_coords(
        &self,
        oob_tx: i32,
        oob_ty: i32,
        map_w_tiles: i32,
        map_h_tiles: i32,
    ) -> Option<(Facing, &MapConnection)> {
        if oob_tx < 0 {
            return self.west.as_ref().map(|c| (Facing::Left, c));
        }
        if oob_tx >= map_w_tiles {
            return self.east.as_ref().map(|c| (Facing::Right, c));
        }
        if oob_ty < 0 {
            return self.north.as_ref().map(|c| (Facing::Up, c));
        }
        if oob_ty >= map_h_tiles {
            return self.south.as_ref().map(|c| (Facing::Down, c));
        }
        None
    }
}

#[derive(Clone, Debug)]
struct MapConnection {
    map_name: String,
    offset_blocks: i32,
}

#[derive(Default)]
struct InputState {
    up: bool,
    down: bool,
    left: bool,
    right: bool,
}

impl InputState {
    fn direction(&self) -> Option<Facing> {
        if self.up {
            Some(Facing::Up)
        } else if self.down {
            Some(Facing::Down)
        } else if self.left {
            Some(Facing::Left)
        } else if self.right {
            Some(Facing::Right)
        } else {
            None
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = std::env::args();
    let _exe = args.next();
    let map_name_arg = args.next();
    let map_name = map_name_arg.as_deref().unwrap_or("RedsHouse2F");

    let event_loop = EventLoop::new()?;
    let window = Rc::new(
        WindowBuilder::new()
            .with_title("pokerust")
            .with_inner_size(PhysicalSize::new(GB_WIDTH * 4, GB_HEIGHT * 4))
            .with_resizable(true)
            .build(&event_loop)?,
    );

    let context = softbuffer::Context::new(window.clone())?;
    let mut surface = softbuffer::Surface::new(&context, window.clone())?;
    let window_size = window.inner_size();
    surface.resize(
        NonZeroU32::new(window_size.width.max(1)).unwrap(),
        NonZeroU32::new(window_size.height.max(1)).unwrap(),
    )?;

    // Use local `../pokered` assets (PNG/BLK/BST); game data is loaded from `data/*.ron`.
    let project_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let pokered_root = project_root.join("../pokered");
    let pokered_root = pokered_root.canonicalize().unwrap_or(pokered_root);
    let pokered_title_dir = pokered_root.join("gfx/title");
    let logo = load_grayscale_png(&pokered_title_dir.join("pokemon_logo.png"))?;
    let player = load_grayscale_png(&pokered_title_dir.join("player.png"))?;
    let gamefreak = load_grayscale_png(&pokered_title_dir.join("gamefreak_inc.png"))?;
    let red_version = load_grayscale_png(&pokered_title_dir.join("red_version.png"))?;
    let blue_version = load_grayscale_png(&pokered_title_dir.join("blue_version.png"))?;
    let font = load_grayscale_png(&pokered_root.join("gfx/font/font.png"))?;

    let data = game_data::GameData::load(&pokered_root)?;

    let audio = AudioEngine::new(data.audio_pitches.clone())?;
    audio.set_volume(0.25);
    let mut current_music_const: Option<String> = None;

    let mut title_is_red = true;
    let mut mode = if map_name_arg.is_some() {
        Mode::Map
    } else {
        Mode::Title
    };
    let empty_picked_up_items: HashSet<(String, String)> = HashSet::new();
    let empty_events: HashSet<String> = HashSet::new();
    let mut map_view = load_map_view(
        &pokered_root,
        &data,
        map_name,
        None,
        &empty_picked_up_items,
        &empty_events,
    )?;
    map_view.rng = 0x1234_5678;
    recenter_camera_on_player(
        &map_view.map,
        &mut map_view.camera_tx,
        &mut map_view.camera_ty,
        map_view.player.tx,
        map_view.player.ty,
    );

    let mut input = InputState::default();
    let tick_duration = Duration::from_secs_f64(1.0 / 60.0);
    let mut next_tick = Instant::now();

    let mut needs_redraw = true;
    let mut gb_frame = vec![dmg_palette_color(0); (GB_WIDTH * GB_HEIGHT) as usize];
    let mut battle_state: Option<BattleState> = None;

    event_loop.run(move |event, elwt| match event {
        Event::WindowEvent { event, .. } => match event {
            WindowEvent::CloseRequested => elwt.exit(),
            WindowEvent::KeyboardInput {
                event: KeyEvent {
                    logical_key, state, ..
                },
                ..
            } => {
                let pressed = state == ElementState::Pressed;

                match &logical_key {
                    Key::Named(NamedKey::Escape) if pressed => elwt.exit(),
                    Key::Character(s) if pressed && s.eq_ignore_ascii_case("q") => elwt.exit(),
                    Key::Character(s)
                        if pressed && s.eq_ignore_ascii_case("l") && mode == Mode::Title =>
                    {
                        match load_game(
                            &pokered_root,
                            &data,
                        ) {
                            Ok(mut loaded) => {
                                loaded.menu = None;
                                loaded.shop = None;
                                loaded.choice = None;
                                loaded.dialog = None;
                                loaded.pc = None;
                                map_view = loaded;
                                mode = Mode::Map;
                                input = InputState::default();
                                map_view.player.move_anim = None;
                                recenter_camera_on_player(
                                    &map_view.map,
                                    &mut map_view.camera_tx,
                                    &mut map_view.camera_ty,
                                    map_view.player.tx,
                                    map_view.player.ty,
                                );
                                next_tick = Instant::now();
                                needs_redraw = true;
                            }
                            Err(err) => {
                                eprintln!("load error: {err}");
                            }
                        }
                    }
                    Key::Character(s)
                        if pressed && s.eq_ignore_ascii_case("z") && mode == Mode::Map =>
                    {
                        if let Err(err) = handle_a_button(
                            &mut map_view,
                            &pokered_root,
                            &data,
                        ) {
                            eprintln!("A-button error: {err}");
                            elwt.exit();
                            return;
                        }
                        needs_redraw = true;
                    }
                    Key::Character(s)
                        if pressed && s.eq_ignore_ascii_case("z") && mode == Mode::Battle =>
                    {
                        let mut result: Option<BattleResult> = None;
                        if let Some(battle) = battle_state.as_mut() {
                            match battle_handle_a_button(
                                battle,
                                &data.move_db,
                                &data.item_display_names,
                                &data.move_display_names,
                                &data.pokemon_display_names,
                                &data.type_chart,
                                &data.pokemon_stats,
                                &data.pokemon_moves,
                                &pokered_root,
                            ) {
                                Ok(r) => result = r,
                                Err(err) => eprintln!("battle error: {err}"),
                            }
                        }
                        if let Some(result) = result {
                            if let Some(battle) = battle_state.take() {
                                end_battle(
                                    result,
                                    battle,
                                    &mut map_view,
                                    &data.trainer_base_reward_money,
                                );
                            }
                            mode = Mode::Map;
                            input = InputState::default();
                        }
                        needs_redraw = true;
                    }
                    Key::Character(s)
                        if pressed && s.eq_ignore_ascii_case("x") && mode == Mode::Map =>
                    {
                        handle_b_button(&mut map_view);
                        input = InputState::default();
                        needs_redraw = true;
                    }
                    Key::Character(s)
                        if pressed && s.eq_ignore_ascii_case("x") && mode == Mode::Battle =>
                    {
                        let mut result: Option<BattleResult> = None;
                        if let Some(battle) = battle_state.as_mut() {
                            match battle_handle_b_button(
                                battle,
                                &data.move_db,
                                &data.item_display_names,
                                &data.move_display_names,
                                &data.pokemon_display_names,
                                &data.type_chart,
                                &data.pokemon_stats,
                                &data.pokemon_moves,
                                &pokered_root,
                            ) {
                                Ok(r) => result = r,
                                Err(err) => eprintln!("battle error: {err}"),
                            }
                        }
                        if let Some(result) = result {
                            if let Some(battle) = battle_state.take() {
                                end_battle(
                                    result,
                                    battle,
                                    &mut map_view,
                                    &data.trainer_base_reward_money,
                                );
                            }
                            mode = Mode::Map;
                            input = InputState::default();
                        }
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::Enter) if pressed && mode == Mode::Map => {
                        if map_view.dialog.is_none()
                            && map_view.player.move_anim.is_none()
                            && map_view.shop.is_none()
                            && map_view.choice.is_none()
                            && map_view.pc.is_none()
                        {
                            toggle_menu(&mut map_view);
                            input = InputState::default();
                            needs_redraw = true;
                        }
                    }
                    Key::Named(NamedKey::ArrowUp)
                        if pressed && mode == Mode::Map && map_view.choice.is_some() =>
                    {
                        choice_nav(&mut map_view, -1);
                        input = InputState::default();
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowUp)
                        if pressed
                            && mode == Mode::Map
                            && map_view.dialog.is_none()
                            && map_view.pc.is_some() =>
                    {
                        pc_nav(&mut map_view, -1);
                        input = InputState::default();
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowUp)
                        if pressed
                            && mode == Mode::Map
                            && map_view.dialog.is_none()
                            && map_view.shop.is_some() =>
                    {
                        shop_nav(&mut map_view, -1);
                        input = InputState::default();
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowUp)
                        if pressed
                            && mode == Mode::Map
                            && map_view.dialog.is_none()
                            && map_view.menu.is_some() =>
                    {
                        menu_nav(&mut map_view, -1);
                        input = InputState::default();
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowUp) if pressed && mode == Mode::Battle => {
                        if let Some(battle) = battle_state.as_mut() {
                            battle_nav(battle, 0, -1);
                        }
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowDown)
                        if pressed && mode == Mode::Map && map_view.choice.is_some() =>
                    {
                        choice_nav(&mut map_view, 1);
                        input = InputState::default();
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowDown)
                        if pressed
                            && mode == Mode::Map
                            && map_view.dialog.is_none()
                            && map_view.pc.is_some() =>
                    {
                        pc_nav(&mut map_view, 1);
                        input = InputState::default();
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowDown)
                        if pressed
                            && mode == Mode::Map
                            && map_view.dialog.is_none()
                            && map_view.shop.is_some() =>
                    {
                        shop_nav(&mut map_view, 1);
                        input = InputState::default();
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowDown)
                        if pressed
                            && mode == Mode::Map
                            && map_view.dialog.is_none()
                            && map_view.menu.is_some() =>
                    {
                        menu_nav(&mut map_view, 1);
                        input = InputState::default();
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowDown) if pressed && mode == Mode::Battle => {
                        if let Some(battle) = battle_state.as_mut() {
                            battle_nav(battle, 0, 1);
                        }
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowLeft) if pressed && mode == Mode::Battle => {
                        if let Some(battle) = battle_state.as_mut() {
                            battle_nav(battle, -1, 0);
                        }
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::ArrowRight) if pressed && mode == Mode::Battle => {
                        if let Some(battle) = battle_state.as_mut() {
                            battle_nav(battle, 1, 0);
                        }
                        needs_redraw = true;
                    }
                    Key::Character(s)
                        if pressed && s.eq_ignore_ascii_case("t") && mode == Mode::Title =>
                    {
                        title_is_red = !title_is_red;
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::Enter) if pressed && mode == Mode::Title => {
                        mode = Mode::Map;
                        input = InputState::default();
                        map_view.player.move_anim = None;
                        map_view.dialog = None;
                        map_view.menu = None;
                        map_view.shop = None;
                        map_view.choice = None;
                        map_view.pc = None;
                        battle_state = None;
                        recenter_camera_on_player(
                            &map_view.map,
                            &mut map_view.camera_tx,
                            &mut map_view.camera_ty,
                            map_view.player.tx,
                            map_view.player.ty,
                        );
                        next_tick = Instant::now();
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::Backspace) if pressed && mode == Mode::Map => {
                        mode = Mode::Title;
                        input = InputState::default();
                        map_view.player.move_anim = None;
                        map_view.dialog = None;
                        map_view.menu = None;
                        map_view.shop = None;
                        map_view.choice = None;
                        map_view.pc = None;
                        battle_state = None;
                        needs_redraw = true;
                    }
                    Key::Named(NamedKey::Backspace) if pressed && mode == Mode::Battle => {
                        mode = Mode::Title;
                        input = InputState::default();
                        battle_state = None;
                        needs_redraw = true;
                    }
                    _ => {}
                }

                if mode == Mode::Map {
                    match &logical_key {
                        Key::Named(NamedKey::ArrowUp) => input.up = pressed,
                        Key::Named(NamedKey::ArrowDown) => input.down = pressed,
                        Key::Named(NamedKey::ArrowLeft) => input.left = pressed,
                        Key::Named(NamedKey::ArrowRight) => input.right = pressed,
                        _ => {}
                    }
                }

                if pressed {
                    needs_redraw = true;
                }
            }
            WindowEvent::Resized(size) => {
                if let (Some(w), Some(h)) =
                    (NonZeroU32::new(size.width), NonZeroU32::new(size.height))
                {
                    let _ = surface.resize(w, h);
                    needs_redraw = true;
                }
            }
            WindowEvent::RedrawRequested => {
                match mode {
                    Mode::Title => render_title_frame(
                        &mut gb_frame,
                        &logo,
                        &player,
                        &gamefreak,
                        if title_is_red {
                            &red_version
                        } else {
                            &blue_version
                        },
                    ),
                    Mode::Map => render_map_frame(
                        &mut gb_frame,
                        &map_view,
                        &font,
                        &data.item_display_names,
                        &data.pokemon_display_names,
                        &data.move_display_names,
                    ),
                    Mode::Battle => {
                        if let Some(battle) = &battle_state {
                            render_battle_frame(
                                &mut gb_frame,
                                battle,
                                &font,
                                &data.item_display_names,
                                &data.move_display_names,
                                &data.pokemon_display_names,
                            );
                        } else {
                            gb_frame.fill(dmg_palette_color(0));
                        }
                    }
                }

                if let Ok(mut buffer) = surface.buffer_mut() {
                    let dst_w = buffer.width().get();
                    let dst_h = buffer.height().get();
                    blit_scaled_letterbox(
                        &gb_frame,
                        GB_WIDTH,
                        GB_HEIGHT,
                        &mut buffer,
                        dst_w,
                        dst_h,
                        dmg_palette_color(3),
                    );
                    if buffer.present().is_err() {
                        elwt.exit();
                    }
                } else {
                    elwt.exit();
                }
            }
            _ => {}
        },
        Event::AboutToWait => {
            if mode == Mode::Map
                && map_view.dialog.is_none()
                && map_view.menu.is_none()
                && map_view.shop.is_none()
                && map_view.choice.is_none()
                && map_view.pc.is_none()
                && map_view.player.move_anim.is_none()
            {
                if let Some(pending) = map_view.pending_battle.take() {
                    match start_battle_from_pending(
                        &pokered_root,
                        pending,
                        &data.trainer_parties,
                        &data.wild_encounters,
                        &data.wild_slot_thresholds,
                        &map_view.party,
                        &map_view.inventory,
                        &data.pokemon_display_names,
                        &data.pokemon_stats,
                        &data.pokemon_moves,
                        map_view.rng,
                        map_view.pokedex_seen.clone(),
                        map_view.pokedex_caught.clone(),
                    ) {
                        Ok(battle) => {
                            battle_state = Some(battle);
                            mode = Mode::Battle;
                            input = InputState::default();
                            needs_redraw = true;
                        }
                        Err(err) => {
                            open_dialog_from_lines(
                                &mut map_view,
                                vec!["Battle start failed.".to_string(), format!("{err}")],
                            );
                        }
                    }
                }
            }

            let now = Instant::now();
            let wants_tick = mode == Mode::Map
                && map_view.dialog.is_none()
                && map_view.menu.is_none()
                && map_view.shop.is_none()
                && map_view.choice.is_none()
                && map_view.pc.is_none();

            if wants_tick {
                if now >= next_tick {
                    if let Err(err) = tick_map(
                        &mut map_view,
                        &input,
                        &pokered_root,
                        &data,
                    ) {
                        eprintln!("tick error: {err}");
                        elwt.exit();
                        return;
                    }
                    next_tick = now + tick_duration;
                    needs_redraw = true;
                }
                elwt.set_control_flow(ControlFlow::WaitUntil(next_tick));
            } else {
                elwt.set_control_flow(ControlFlow::Wait);
            }

            let desired_music: Option<&str> = match mode {
                Mode::Title => Some("MUSIC_TITLE_SCREEN"),
                Mode::Map => data
                    .map_music_constants
                    .get(&map_view.map_id)
                    .map(|s| s.as_str()),
                Mode::Battle => battle_state.as_ref().map(|battle| match &battle.kind {
                    BattleKind::Trainer { .. } => "MUSIC_TRAINER_BATTLE",
                    BattleKind::Wild { .. } => "MUSIC_WILD_BATTLE",
                }),
            };
            let music_changed = match (&current_music_const, desired_music) {
                (Some(cur), Some(next)) => cur != next,
                (None, None) => false,
                _ => true,
            };
            if music_changed {
                current_music_const = desired_music.map(|s| s.to_string());
                let song = desired_music.and_then(packaged_music::song_for_music_const);
                audio.play_song(song);
            }

            if needs_redraw {
                window.request_redraw();
                needs_redraw = false;
            }
        }
        _ => {}
    })?;

    Ok(())
}

fn load_grayscale_png(path: &Path) -> Result<GrayscaleImage, Box<dyn Error>> {
    let img = image::open(path)?;
    let (width, height) = img.dimensions();
    let luma = img.to_luma8().into_raw();
    Ok(GrayscaleImage {
        width,
        height,
        luma,
    })
}

struct GrayscaleImage {
    width: u32,
    height: u32,
    luma: Vec<u8>,
}

struct Tileset {
    img: GrayscaleImage,
    tiles_per_row: u32,
    tile_count: u32,
    grass_tile_id: Option<u8>,
    // Counter tiles allow talking to NPCs 2 tiles away (across counters)
    // From tileset_headers.asm: Pokecenter/Mart use $18,$19,$1E
    counter_tiles: Vec<u8>,
}

struct Player {
    tx: i32,
    ty: i32,
    facing: Facing,
    walk_intra_counter: u8,
    walk_anim_frame: u8,
    sprite: GrayscaleImage,
    move_anim: Option<MoveAnim>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct MoveAnim {
    dir: Facing,
    start_tx: i32,
    start_ty: i32,
    dest_tx: i32,
    dest_ty: i32,
    progress_px: i32,
}

struct Blockset {
    blocks: Vec<[u8; 16]>,
}

struct MapData {
    width_blocks: u32,
    height_blocks: u32,
    blocks: Vec<u8>,
}

struct MapView {
    map_name: String,
    map_id: String,
    is_outside: bool,
    last_outside_map: Option<String>,
    connections: MapConnections,
    warp_events: Vec<WarpEvent>,
    bg_events: Vec<BgEvent>,
    hidden_events: Vec<HiddenEvent>,
    object_events: Vec<ObjectEvent>,
    object_sprites: HashMap<u8, GrayscaleImage>,
    tileset: Tileset,
    blockset: Blockset,
    passable_tiles: HashSet<u8>,
    map: MapData,
    player: Player,
    camera_tx: i32,
    camera_ty: i32,
    overworld_subtick: u8,
    dialog: Option<Dialog>,
    menu: Option<MenuState>,
    shop: Option<ShopState>,
    choice: Option<ChoiceState>,
    pending_battle: Option<PendingBattle>,
    rng: u32,
    money: u32,
    events: HashSet<String>,
    inventory: HashMap<String, u32>,
    pc_items: HashMap<String, u32>,
    picked_up_items: HashSet<(String, String)>,
    defeated_trainers: HashSet<(String, String)>,
    party: Vec<PlayerMon>,
    pc_boxes: Vec<Vec<PlayerMon>>,
    pc_current_box: usize,
    pc: Option<PcState>,
    options: GameOptions,
    pokedex_seen: HashSet<String>,
    pokedex_caught: HashSet<String>,
}

struct MapHeaderInfo {
    map_name: String,
    map_id: String,
    tileset: String,
    connections: MapConnections,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct MapEvents {
    warp_events: Vec<WarpEvent>,
    bg_events: Vec<BgEvent>,
    hidden_events: Vec<HiddenEvent>,
    object_events: Vec<ObjectEvent>,
}

fn render_title_frame(
    gb_frame: &mut [u32],
    logo: &GrayscaleImage,
    player: &GrayscaleImage,
    gamefreak: &GrayscaleImage,
    version: &GrayscaleImage,
) {
    gb_frame.fill(dmg_palette_color(0));

    let logo_x = (GB_WIDTH as i32 - logo.width as i32) / 2;
    let logo_y = 8;
    blit_grayscale_to_gb(gb_frame, GB_WIDTH, GB_HEIGHT, logo, logo_x, logo_y);

    let player_x = 16;
    let player_y = 64;
    blit_grayscale_to_gb(gb_frame, GB_WIDTH, GB_HEIGHT, player, player_x, player_y);

    let version_x = 48;
    let version_y = 104;
    blit_grayscale_to_gb(gb_frame, GB_WIDTH, GB_HEIGHT, version, version_x, version_y);

    let gf_x = (GB_WIDTH as i32 - gamefreak.width as i32) / 2;
    let gf_y = 132;
    blit_grayscale_to_gb(gb_frame, GB_WIDTH, GB_HEIGHT, gamefreak, gf_x, gf_y);
}

fn render_map_frame(
    gb_frame: &mut [u32],
    view: &MapView,
    font: &GrayscaleImage,
    item_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
    move_display_names: &HashMap<String, String>,
) {
    gb_frame.fill(dmg_palette_color(0));

    let map_w_tiles = (view.map.width_blocks * BLOCK_TILES) as i32;
    let map_h_tiles = (view.map.height_blocks * BLOCK_TILES) as i32;

    for screen_ty in 0..SCREEN_TILES_H as i32 {
        let map_ty = view.camera_ty + screen_ty;
        if map_ty < 0 || map_ty >= map_h_tiles {
            continue;
        }
        for screen_tx in 0..SCREEN_TILES_W as i32 {
            let map_tx = view.camera_tx + screen_tx;
            if map_tx < 0 || map_tx >= map_w_tiles {
                continue;
            }

            let block_x = (map_tx as u32) / BLOCK_TILES;
            let block_y = (map_ty as u32) / BLOCK_TILES;
            let block_i = (block_y * view.map.width_blocks + block_x) as usize;
            let Some(&block_id) = view.map.blocks.get(block_i) else {
                continue;
            };

            let block_id = block_id as usize;
            let Some(block) = view.blockset.blocks.get(block_id) else {
                continue;
            };

            let tile_x = (map_tx as u32) % BLOCK_TILES;
            let tile_y = (map_ty as u32) % BLOCK_TILES;
            let tile_i = (tile_y * BLOCK_TILES + tile_x) as usize;
            let tile_id = block[tile_i];

            draw_tileset_tile(
                gb_frame,
                &view.tileset,
                tile_id,
                (screen_tx as u32) * TILE_SIZE,
                (screen_ty as u32) * TILE_SIZE,
            );
        }
    }

    draw_overworld_sprites(gb_frame, view);
    if view.shop.is_some() {
        draw_shop(gb_frame, font, view, item_display_names);
    } else if view.pc.is_some() {
        draw_pc(
            gb_frame,
            font,
            view,
            item_display_names,
            pokemon_display_names,
        );
    } else if view.menu.is_some() {
        draw_menu(
            gb_frame,
            font,
            view,
            item_display_names,
            pokemon_display_names,
            move_display_names,
        );
    }
    if let Some(dialog) = &view.dialog {
        draw_text_box(gb_frame, font, dialog);
    }
    if let Some(choice) = &view.choice {
        draw_yes_no_box(gb_frame, font, choice);
    }
}

fn render_battle_frame(
    gb_frame: &mut [u32],
    battle: &BattleState,
    font: &GrayscaleImage,
    item_display_names: &HashMap<String, String>,
    move_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
) {
    gb_frame.fill(dmg_palette_color(0));

    let box_h = (6 * TILE_SIZE) as i32;
    let field_h = GB_HEIGHT as i32 - box_h;

    let enemy_x = GB_WIDTH as i32 - battle.enemy.front_sprite.width as i32 - 16;
    let enemy_y = 16;
    blit_grayscale_to_gb_transparent(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        &battle.enemy.front_sprite,
        enemy_x,
        enemy_y,
    );

    let player_x = 16;
    let player_y = field_h - battle.player.back_sprite.height as i32 - 8;
    blit_grayscale_to_gb_transparent(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        &battle.player.back_sprite,
        player_x,
        player_y,
    );

    draw_battle_status_boxes(gb_frame, font, battle, pokemon_display_names);
    draw_battle_bottom_box(
        gb_frame,
        font,
        battle,
        item_display_names,
        move_display_names,
        pokemon_display_names,
    );
}

fn draw_tileset_tile(gb_frame: &mut [u32], tileset: &Tileset, tile_id: u8, dst_x: u32, dst_y: u32) {
    if dst_x + TILE_SIZE > GB_WIDTH || dst_y + TILE_SIZE > GB_HEIGHT {
        return;
    }

    let tile_id_u32 = tile_id as u32;
    if tile_id_u32 >= tileset.tile_count {
        return;
    }

    let tile_x0 = (tile_id_u32 % tileset.tiles_per_row) * TILE_SIZE;
    let tile_y0 = (tile_id_u32 / tileset.tiles_per_row) * TILE_SIZE;

    for y in 0..TILE_SIZE {
        for x in 0..TILE_SIZE {
            let sx = tile_x0 + x;
            let sy = tile_y0 + y;
            let src_i = (sy * tileset.img.width + sx) as usize;
            let luma = tileset.img.luma[src_i];
            let gb_color = luma_to_gb_color(luma);
            let dst_i = ((dst_y + y) * GB_WIDTH + (dst_x + x)) as usize;
            gb_frame[dst_i] = dmg_palette_color(gb_color);
        }
    }
}

struct OverworldDrawCmd<'a> {
    sort_y: i32,
    sort_x: i32,
    tie_break: u8,
    sheet: &'a GrayscaleImage,
    frame_index: u32,
    dst_x: i32,
    dst_y: i32,
    mirror_x: bool,
}

fn draw_overworld_sprites(gb_frame: &mut [u32], view: &MapView) {
    let mut draws: Vec<OverworldDrawCmd<'_>> = Vec::new();

    for obj in &view.object_events {
        let Some(sheet) = view.object_sprites.get(&obj.sprite_id) else {
            continue;
        };
        let (base_tx, base_ty, offset_px_x, offset_px_y, frame_index, mirror_x) =
            object_draw_params(obj, sheet);
        let world_x = base_tx * TILE_SIZE as i32 + offset_px_x;
        let world_y = base_ty * TILE_SIZE as i32 + offset_px_y;
        let dst_x = world_x - view.camera_tx * TILE_SIZE as i32;
        let dst_y = world_y - view.camera_ty * TILE_SIZE as i32;

        draws.push(OverworldDrawCmd {
            sort_y: world_y + PLAYER_FRAME_SIZE_PX as i32 - 1,
            sort_x: world_x,
            tie_break: 0,
            sheet,
            frame_index,
            dst_x,
            dst_y,
            mirror_x,
        });
    }

    let (base_tx, base_ty, offset_px_x, offset_px_y, frame_index, mirror_x) =
        player_draw_params(&view.player);
    let world_x = base_tx * TILE_SIZE as i32 + offset_px_x;
    let world_y = base_ty * TILE_SIZE as i32 + offset_px_y;

    draws.push(OverworldDrawCmd {
        sort_y: world_y + PLAYER_FRAME_SIZE_PX as i32 - 1,
        sort_x: world_x,
        tie_break: 1,
        sheet: &view.player.sprite,
        frame_index,
        dst_x: world_x - view.camera_tx * TILE_SIZE as i32,
        dst_y: world_y - view.camera_ty * TILE_SIZE as i32,
        mirror_x,
    });

    draws.sort_by_key(|d| (d.sort_y, d.sort_x, d.tie_break));
    for d in draws {
        blit_sprite_frame_to_gb(
            gb_frame,
            GB_WIDTH,
            GB_HEIGHT,
            d.sheet,
            d.frame_index,
            d.dst_x,
            d.dst_y,
            d.mirror_x,
        );
    }
}

fn object_draw_params(
    obj: &ObjectEvent,
    sprite_sheet: &GrayscaleImage,
) -> (i32, i32, i32, i32, u32, bool) {
    let (base_tx, base_ty, offset_px_x, offset_px_y, facing) = match &obj.move_anim {
        Some(m) => {
            let (dx, dy) = facing_to_dir_px(m.dir, m.progress_px);
            (m.start_tx, m.start_ty, dx, dy, m.dir)
        }
        None => (obj.tx, obj.ty, 0, 0, obj.facing),
    };

    if sprite_sheet.width != PLAYER_FRAME_SIZE_PX || sprite_sheet.height < PLAYER_FRAME_SIZE_PX {
        return (base_tx, base_ty, offset_px_x, offset_px_y, 0, false);
    }

    if sprite_sheet.height < PLAYER_FRAME_SIZE_PX * 6 {
        return (base_tx, base_ty, offset_px_x, offset_px_y, 0, false);
    }

    let moving = obj.move_anim.is_some();
    let anim = obj.walk_anim_frame & 3;
    let use_walk = moving && (anim == 1 || anim == 3);
    let use_flip = moving && anim == 3;

    let (frame_index, mirror_x) = match facing {
        Facing::Down => {
            let frame = if use_walk { 3 } else { 0 };
            (frame, use_flip)
        }
        Facing::Up => {
            let frame = if use_walk { 4 } else { 1 };
            (frame, use_flip)
        }
        Facing::Left => {
            let frame = if use_walk { 5 } else { 2 };
            (frame, false)
        }
        Facing::Right => {
            let frame = if use_walk { 5 } else { 2 };
            (frame, true)
        }
    };
    (
        base_tx,
        base_ty,
        offset_px_x,
        offset_px_y,
        frame_index,
        mirror_x,
    )
}

fn draw_text_box(gb_frame: &mut [u32], font: &GrayscaleImage, dialog: &Dialog) {
    let box_h = (6 * TILE_SIZE) as i32;
    let y0 = GB_HEIGHT as i32 - box_h;

    fill_rect(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        0,
        y0,
        GB_WIDTH as i32,
        box_h,
        dmg_palette_color(3),
    );
    fill_rect(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        2,
        y0 + 2,
        GB_WIDTH as i32 - 4,
        box_h - 4,
        dmg_palette_color(0),
    );

    let start = dialog.cursor.min(dialog.lines.len());
    let end = (start + 2).min(dialog.lines.len());
    let text_x = 8;
    let text_y0 = y0 + 8;
    for (i, line) in dialog.lines[start..end].iter().enumerate() {
        draw_text_line(
            gb_frame,
            GB_WIDTH,
            GB_HEIGHT,
            font,
            line,
            text_x,
            text_y0 + i as i32 * TILE_SIZE as i32,
        );
    }

    if dialog.has_more() {
        draw_text_line(
            gb_frame,
            GB_WIDTH,
            GB_HEIGHT,
            font,
            "▼",
            GB_WIDTH as i32 - 16,
            y0 + box_h - 16,
        );
    }
}

fn draw_menu(
    gb_frame: &mut [u32],
    font: &GrayscaleImage,
    view: &MapView,
    item_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
    _move_display_names: &HashMap<String, String>,
) {
    let Some(menu) = &view.menu else {
        return;
    };

    match menu.screen {
        MenuScreen::Root => {
            let outer_w = (10 * TILE_SIZE) as i32 + 4;
            let outer_h = (ROOT_MENU_ITEMS.len() as i32 * TILE_SIZE as i32) + 16;
            let x0 = GB_WIDTH as i32 - outer_w - 4;
            let y0 = 4;

            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                x0,
                y0,
                outer_w,
                outer_h,
                dmg_palette_color(3),
            );
            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                x0 + 2,
                y0 + 2,
                outer_w - 4,
                outer_h - 4,
                dmg_palette_color(0),
            );

            let text_x = x0 + 8;
            for (i, label) in ROOT_MENU_ITEMS.iter().enumerate() {
                let text_y = y0 + 8 + (i as i32) * TILE_SIZE as i32;
                if i == menu.selection {
                    fill_rect(
                        gb_frame,
                        GB_WIDTH,
                        GB_HEIGHT,
                        x0 + 4,
                        text_y - 2,
                        outer_w - 8,
                        TILE_SIZE as i32,
                        dmg_palette_color(3),
                    );
                    draw_text_line_tinted(
                        gb_frame, GB_WIDTH, GB_HEIGHT, font, label, text_x, text_y, 0,
                    );
                } else {
                    draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, label, text_x, text_y);
                }
            }
        }
        MenuScreen::Items => {
            let ids = inventory_item_ids_sorted(view);

            let outer_x = 4;
            let outer_y = 4;
            let outer_w = GB_WIDTH as i32 - 8;
            let outer_h = GB_HEIGHT as i32 - 8;

            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x,
                outer_y,
                outer_w,
                outer_h,
                dmg_palette_color(3),
            );
            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x + 2,
                outer_y + 2,
                outer_w - 4,
                outer_h - 4,
                dmg_palette_color(0),
            );

            draw_text_line(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                font,
                "ITEM",
                outer_x + 8,
                outer_y + 8,
            );

            let list_x = outer_x + 8;
            let list_y0 = outer_y + 24;

            if ids.is_empty() {
                draw_text_line(
                    gb_frame,
                    GB_WIDTH,
                    GB_HEIGHT,
                    font,
                    "No items.",
                    list_x,
                    list_y0,
                );
            } else {
                for row in 0..ITEMS_VISIBLE_ROWS {
                    let idx = menu.scroll + row;
                    if idx >= ids.len() {
                        break;
                    }
                    let item_id = &ids[idx];
                    let display_name = item_display_names
                        .get(item_id)
                        .cloned()
                        .unwrap_or_else(|| fallback_item_display_name(item_id));
                    let count = *view.inventory.get(item_id).unwrap_or(&0);
                    let count_s = format!("x{count}");
                    let count_x =
                        outer_x + outer_w - 8 - (count_s.chars().count() as i32 * TILE_SIZE as i32);

                    let text_y = list_y0 + (row as i32) * TILE_SIZE as i32;
                    let selected = idx == menu.selection;
                    if selected {
                        fill_rect(
                            gb_frame,
                            GB_WIDTH,
                            GB_HEIGHT,
                            outer_x + 4,
                            text_y - 2,
                            outer_w - 8,
                            TILE_SIZE as i32,
                            dmg_palette_color(3),
                        );
                        draw_text_line_tinted(
                            gb_frame,
                            GB_WIDTH,
                            GB_HEIGHT,
                            font,
                            &display_name,
                            list_x,
                            text_y,
                            0,
                        );
                        draw_text_line_tinted(
                            gb_frame, GB_WIDTH, GB_HEIGHT, font, &count_s, count_x, text_y, 0,
                        );
                    } else {
                        draw_text_line(
                            gb_frame,
                            GB_WIDTH,
                            GB_HEIGHT,
                            font,
                            &display_name,
                            list_x,
                            text_y,
                        );
                        draw_text_line(
                            gb_frame, GB_WIDTH, GB_HEIGHT, font, &count_s, count_x, text_y,
                        );
                    }
                }

                if ids.len() > menu.scroll + ITEMS_VISIBLE_ROWS {
                    draw_text_line(
                        gb_frame,
                        GB_WIDTH,
                        GB_HEIGHT,
                        font,
                        "▼",
                        outer_x + outer_w - 16,
                        outer_y + outer_h - 16,
                    );
                }
            }
        }
        MenuScreen::Party => {
            let outer_x = 4;
            let outer_y = 4;
            let outer_w = GB_WIDTH as i32 - 8;
            let outer_h = GB_HEIGHT as i32 - 8;

            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x,
                outer_y,
                outer_w,
                outer_h,
                dmg_palette_color(3),
            );
            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x + 2,
                outer_y + 2,
                outer_w - 4,
                outer_h - 4,
                dmg_palette_color(0),
            );

            draw_text_line(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                font,
                "POKéMON",
                outer_x + 8,
                outer_y + 8,
            );

            let list_x = outer_x + 8;
            let list_y0 = outer_y + 24;
            if view.party.is_empty() {
                draw_text_line(
                    gb_frame,
                    GB_WIDTH,
                    GB_HEIGHT,
                    font,
                    "No Pokémon.",
                    list_x,
                    list_y0,
                );
            } else {
                for (i, mon) in view.party.iter().take(6).enumerate() {
                    let name = pokemon_display_names
                        .get(&mon.species)
                        .cloned()
                        .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));
                    let hp_s = format!("{}/{}", mon.hp, mon.max_hp);
                    let hp_x = outer_x + outer_w - 8 - text_width_px(&hp_s);
                    let y = list_y0 + (i as i32) * TILE_SIZE as i32;
                    if i == menu.selection {
                        fill_rect(
                            gb_frame,
                            GB_WIDTH,
                            GB_HEIGHT,
                            outer_x + 4,
                            y - 2,
                            outer_w - 8,
                            TILE_SIZE as i32,
                            dmg_palette_color(3),
                        );
                        draw_text_line_tinted(
                            gb_frame, GB_WIDTH, GB_HEIGHT, font, &name, list_x, y, 0,
                        );
                        draw_text_line_tinted(
                            gb_frame, GB_WIDTH, GB_HEIGHT, font, &hp_s, hp_x, y, 0,
                        );
                    } else {
                        draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, &name, list_x, y);
                        draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, &hp_s, hp_x, y);
                    }
                }
            }
        }
        MenuScreen::Options => {
            let outer_x = 4;
            let outer_y = 4;
            let outer_w = GB_WIDTH as i32 - 8;
            let outer_h = GB_HEIGHT as i32 - 8;

            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x,
                outer_y,
                outer_w,
                outer_h,
                dmg_palette_color(3),
            );
            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x + 2,
                outer_y + 2,
                outer_w - 4,
                outer_h - 4,
                dmg_palette_color(0),
            );

            draw_text_line(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                font,
                "OPTIONS",
                outer_x + 8,
                outer_y + 8,
            );

            let list_x = outer_x + 8;
            let list_y0 = outer_y + 24;

            // Draw options with current values
            let options = [
                format!("TEXT SPEED: {:?}", view.options.text_speed).to_uppercase(),
                format!("BATTLE ANIM: {}", if view.options.battle_animations { "ON" } else { "OFF" }),
                format!("SOUND: {}", if view.options.sound_enabled { "ON" } else { "OFF" }),
            ];

            for (i, option) in options.iter().enumerate() {
                let y = list_y0 + (i as i32) * TILE_SIZE as i32;
                if i == menu.selection {
                    fill_rect(
                        gb_frame,
                        GB_WIDTH,
                        GB_HEIGHT,
                        outer_x + 4,
                        y - 2,
                        outer_w - 8,
                        TILE_SIZE as i32,
                        dmg_palette_color(3),
                    );
                    draw_text_line_tinted(
                        gb_frame, GB_WIDTH, GB_HEIGHT, font, option, list_x, y, 0,
                    );
                } else {
                    draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, option, list_x, y);
                }
            }
        }
        MenuScreen::TeachMove { ref item_id } => {
            // Draw Pokemon selection screen
            let outer_x = 4;
            let outer_y = 4;
            let outer_w = GB_WIDTH as i32 - 8;
            let outer_h = GB_HEIGHT as i32 - 8;

            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_WIDTH,
                outer_x,
                outer_y,
                outer_w,
                outer_h,
                dmg_palette_color(3),
            );
            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x + 2,
                outer_y + 2,
                outer_w - 4,
                outer_h - 4,
                dmg_palette_color(0),
            );

            // Draw title
            let title = if item_id.starts_with("TM_") {
                "Teach TM"
            } else {
                "Teach HM"
            };
            draw_text_line(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                font,
                title,
                outer_x + 8,
                outer_y + 8,
            );

            // Draw Pokemon list
            let list_x = outer_x + 8;
            let list_y0 = outer_y + 24;

            for (i, mon) in view.party.iter().enumerate() {
                let y = list_y0 + (i as i32) * TILE_SIZE as i32;
                let name = pokemon_display_names
                    .get(&mon.species)
                    .map(|s| s.as_str())
                    .unwrap_or(mon.species.as_str());
                let line = format!("{} L{}", name, mon.level);

                if i == menu.selection {
                    fill_rect(
                        gb_frame,
                        GB_WIDTH,
                        GB_HEIGHT,
                        outer_x + 4,
                        y - 2,
                        outer_w - 8,
                        TILE_SIZE as i32,
                        dmg_palette_color(3),
                    );
                    draw_text_line_tinted(gb_frame, GB_WIDTH, GB_HEIGHT, font, &line, list_x, y, 0);
                } else {
                    draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, &line, list_x, y);
                }
            }
        }
        MenuScreen::Pokedex { cursor, scroll } => {
            // Draw outer box
            let outer_x = 4;
            let outer_y = 4;
            let outer_w = GB_WIDTH as i32 - 8;
            let outer_h = GB_HEIGHT as i32 - 8;

            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x,
                outer_y,
                outer_w,
                outer_h,
                dmg_palette_color(3),
            );
            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x + 2,
                outer_y + 2,
                outer_w - 4,
                outer_h - 4,
                dmg_palette_color(0),
            );

            // Title
            draw_text_line(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                font,
                "POKéDEX",
                outer_x + 8,
                outer_y + 8,
            );

            // Stats on right side
            let seen = view.pokedex_seen.len();
            let caught = view.pokedex_caught.len();

            draw_text_line(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                font,
                &format!("Seen:{}", seen),
                GB_WIDTH as i32 - 50,
                outer_y + 8,
            );
            draw_text_line(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                font,
                &format!("Own:{}", caught),
                GB_WIDTH as i32 - 50,
                outer_y + 16,
            );

            // Pokémon list (simplified - showing first 151 entries)
            // For full implementation, we'd need Pokemon name data
            let list_start_y = outer_y + 28;
            let visible_rows = 8;

            for i in 0..visible_rows {
                let dex_num = scroll + i + 1;
                if dex_num > 151 {
                    break;
                }

                let y = list_start_y + (i as i32 * 12);
                let is_selected = (scroll + i) == cursor;

                // Draw cursor
                if is_selected {
                    draw_text_line(
                        gb_frame,
                        GB_WIDTH,
                        GB_HEIGHT,
                        font,
                        ">",
                        outer_x + 8,
                        y,
                    );
                }

                // Draw Pokedex number and status indicator
                let num_text = format!("{:03}", dex_num);
                draw_text_line(
                    gb_frame,
                    GB_WIDTH,
                    GB_HEIGHT,
                    font,
                    &num_text,
                    outer_x + 20,
                    y,
                );

                // For a full implementation, we'd show Pokemon names here
                // For now, just show caught/seen indicator
                // This is a simplified version
            }
        }
    }
}

fn draw_pc(
    gb_frame: &mut [u32],
    font: &GrayscaleImage,
    view: &MapView,
    item_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
) {
    let Some(pc) = &view.pc else {
        return;
    };

    let outer_x = 4;
    let outer_y = 4;
    let outer_w = GB_WIDTH as i32 - 8;
    let outer_h = GB_HEIGHT as i32 - 8;

    fill_rect(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        outer_x,
        outer_y,
        outer_w,
        outer_h,
        dmg_palette_color(3),
    );
    fill_rect(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        outer_x + 2,
        outer_y + 2,
        outer_w - 4,
        outer_h - 4,
        dmg_palette_color(0),
    );

    let title = match pc.screen {
        PcScreen::Root => "PC".to_string(),
        PcScreen::BillsPc => "BILL'S PC".to_string(),
        PcScreen::Withdraw => format!("WITHDRAW (BOX {})", view.pc_current_box + 1),
        PcScreen::Deposit => format!("DEPOSIT (BOX {})", view.pc_current_box + 1),
        PcScreen::Release => format!("RELEASE (BOX {})", view.pc_current_box + 1),
        PcScreen::ChangeBox => "CHANGE BOX".to_string(),
        PcScreen::PlayersPc => "PLAYER'S PC".to_string(),
        PcScreen::WithdrawItem => "WITHDRAW ITEM".to_string(),
        PcScreen::DepositItem => "DEPOSIT ITEM".to_string(),
        PcScreen::TossItem => "TOSS ITEM".to_string(),
    };
    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        &title,
        outer_x + 8,
        outer_y + 8,
    );

    let items: Vec<String> = match pc.screen {
        PcScreen::Root => match pc.location {
            PcLocation::PokemonCenter => vec![
                "BILL'S PC".to_string(),
                "PLAYER'S PC".to_string(),
                "PROF.OAK'S PC".to_string(),
                "LOG OFF".to_string(),
            ],
            PcLocation::RedsHouse => vec![
                "WITHDRAW ITEM".to_string(),
                "DEPOSIT ITEM".to_string(),
                "TOSS ITEM".to_string(),
                "LOG OFF".to_string(),
            ],
        },
        PcScreen::BillsPc => vec![
            "WITHDRAW PKMN".to_string(),
            "DEPOSIT PKMN".to_string(),
            "RELEASE PKMN".to_string(),
            "CHANGE BOX".to_string(),
            "LOG OFF".to_string(),
        ],
        PcScreen::PlayersPc => vec![
            "WITHDRAW ITEM".to_string(),
            "DEPOSIT ITEM".to_string(),
            "TOSS ITEM".to_string(),
            "LOG OFF".to_string(),
        ],
        PcScreen::Withdraw | PcScreen::Release => {
            let mut out: Vec<String> = Vec::new();
            if let Some(box_mons) = view.pc_boxes.get(view.pc_current_box) {
                for mon in box_mons {
                    let name = pokemon_display_names
                        .get(&mon.species)
                        .cloned()
                        .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));
                    out.push(format!("{name} L{}", mon.level));
                }
            }
            out.push("CANCEL".to_string());
            out
        }
        PcScreen::Deposit => {
            let mut out: Vec<String> = Vec::new();
            for mon in view.party.iter().take(6) {
                let name = pokemon_display_names
                    .get(&mon.species)
                    .cloned()
                    .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));
                out.push(format!("{name} L{}", mon.level));
            }
            out.push("CANCEL".to_string());
            out
        }
        PcScreen::ChangeBox => {
            let mut out: Vec<String> = Vec::new();
            let box_count = view.pc_boxes.len().max(1);
            for i in 0..box_count {
                out.push(format!("BOX {}", i + 1));
            }
            out.push("CANCEL".to_string());
            out
        }
        PcScreen::WithdrawItem | PcScreen::TossItem => {
            let mut out: Vec<String> = Vec::new();
            for item_id in inventory_item_ids_sorted_map(&view.pc_items) {
                let count = view.pc_items.get(&item_id).copied().unwrap_or(0);
                let name = item_display_names
                    .get(&item_id)
                    .cloned()
                    .unwrap_or_else(|| fallback_item_display_name(&item_id));
                out.push(format!("{name} x{count}"));
            }
            out.push("CANCEL".to_string());
            out
        }
        PcScreen::DepositItem => {
            let mut out: Vec<String> = Vec::new();
            for item_id in inventory_item_ids_sorted_map(&view.inventory) {
                let count = view.inventory.get(&item_id).copied().unwrap_or(0);
                let name = item_display_names
                    .get(&item_id)
                    .cloned()
                    .unwrap_or_else(|| fallback_item_display_name(&item_id));
                out.push(format!("{name} x{count}"));
            }
            out.push("CANCEL".to_string());
            out
        }
    };

    let list_x = outer_x + 8;
    let list_y0 = outer_y + 24;
    if items.is_empty() {
        draw_text_line(
            gb_frame,
            GB_WIDTH,
            GB_HEIGHT,
            font,
            "Nothing here.",
            list_x,
            list_y0,
        );
        return;
    }

    for row in 0..ITEMS_VISIBLE_ROWS {
        let i = pc.scroll + row;
        if i >= items.len() {
            break;
        }
        let y = list_y0 + (row as i32) * TILE_SIZE as i32;
        let label = &items[i];
        if i == pc.selection {
            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                outer_x + 4,
                y - 2,
                outer_w - 8,
                TILE_SIZE as i32,
                dmg_palette_color(3),
            );
            draw_text_line_tinted(gb_frame, GB_WIDTH, GB_HEIGHT, font, label, list_x, y, 0);
        } else {
            draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, label, list_x, y);
        }
    }

    if pc.scroll > 0 {
        draw_text_line(
            gb_frame,
            GB_WIDTH,
            GB_HEIGHT,
            font,
            "▲",
            outer_x + outer_w - 16,
            outer_y + 16,
        );
    }
    if items.len() > pc.scroll + ITEMS_VISIBLE_ROWS {
        draw_text_line(
            gb_frame,
            GB_WIDTH,
            GB_HEIGHT,
            font,
            "▼",
            outer_x + outer_w - 16,
            outer_y + outer_h - 16,
        );
    }
}

fn draw_shop(
    gb_frame: &mut [u32],
    font: &GrayscaleImage,
    view: &MapView,
    item_display_names: &HashMap<String, String>,
) {
    let Some(shop) = &view.shop else {
        return;
    };

    let outer_x = 4;
    let outer_y = 4;
    let outer_w = GB_WIDTH as i32 - 8;
    let outer_h = GB_HEIGHT as i32 - 8;

    fill_rect(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        outer_x,
        outer_y,
        outer_w,
        outer_h,
        dmg_palette_color(3),
    );
    fill_rect(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        outer_x + 2,
        outer_y + 2,
        outer_w - 4,
        outer_h - 4,
        dmg_palette_color(0),
    );

    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        "MART",
        outer_x + 8,
        outer_y + 8,
    );
    let money_s = format!("${}", view.money);
    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        &money_s,
        outer_x + outer_w - 8 - text_width_px(&money_s),
        outer_y + 8,
    );

    let list_x = outer_x + 8;
    let list_y0 = outer_y + 24;

    if shop.items.is_empty() {
        draw_text_line(
            gb_frame,
            GB_WIDTH,
            GB_HEIGHT,
            font,
            "Sold out.",
            list_x,
            list_y0,
        );
    } else {
        for row in 0..ITEMS_VISIBLE_ROWS {
            let idx = shop.scroll + row;
            if idx >= shop.items.len() {
                break;
            }
            let it = &shop.items[idx];
            let display_name = item_display_names
                .get(&it.item_id)
                .cloned()
                .unwrap_or_else(|| fallback_item_display_name(&it.item_id));
            let price_s = format!("${}", it.price);
            let price_x = outer_x + outer_w - 8 - text_width_px(&price_s);
            let text_y = list_y0 + (row as i32) * TILE_SIZE as i32;

            let selected = idx == shop.selection;
            if selected {
                fill_rect(
                    gb_frame,
                    GB_WIDTH,
                    GB_HEIGHT,
                    outer_x + 4,
                    text_y - 2,
                    outer_w - 8,
                    TILE_SIZE as i32,
                    dmg_palette_color(3),
                );
                draw_text_line_tinted(
                    gb_frame,
                    GB_WIDTH,
                    GB_HEIGHT,
                    font,
                    &display_name,
                    list_x,
                    text_y,
                    0,
                );
                draw_text_line_tinted(
                    gb_frame, GB_WIDTH, GB_HEIGHT, font, &price_s, price_x, text_y, 0,
                );
            } else {
                draw_text_line(
                    gb_frame,
                    GB_WIDTH,
                    GB_HEIGHT,
                    font,
                    &display_name,
                    list_x,
                    text_y,
                );
                draw_text_line(
                    gb_frame, GB_WIDTH, GB_HEIGHT, font, &price_s, price_x, text_y,
                );
            }
        }

        if shop.items.len() > shop.scroll + ITEMS_VISIBLE_ROWS {
            draw_text_line(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                font,
                "▼",
                outer_x + outer_w - 16,
                outer_y + outer_h - 16,
            );
        }
    }

    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        "A:BUY  B:QUIT",
        outer_x + 8,
        outer_y + outer_h - 16,
    );
}

fn draw_yes_no_box(gb_frame: &mut [u32], font: &GrayscaleImage, choice: &ChoiceState) {
    let dialog_y0 = GB_HEIGHT as i32 - (6 * TILE_SIZE) as i32;
    let box_w = (6 * TILE_SIZE) as i32;
    let box_h = (4 * TILE_SIZE) as i32;
    let x0 = GB_WIDTH as i32 - box_w - 8;
    let y0 = dialog_y0 + 8;
    draw_ui_box(gb_frame, GB_WIDTH, GB_HEIGHT, x0, y0, box_w, box_h);

    let options = ["YES", "NO"];
    for (i, opt) in options.iter().enumerate() {
        let text_y = y0 + 8 + (i as i32) * TILE_SIZE as i32;
        if i == choice.selection {
            fill_rect(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                x0 + 4,
                text_y - 2,
                box_w - 8,
                TILE_SIZE as i32,
                dmg_palette_color(3),
            );
            draw_text_line_tinted(gb_frame, GB_WIDTH, GB_HEIGHT, font, opt, x0 + 8, text_y, 0);
        } else {
            draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, opt, x0 + 8, text_y);
        }
    }
}

fn draw_battle_status_boxes(
    gb_frame: &mut [u32],
    font: &GrayscaleImage,
    battle: &BattleState,
    pokemon_display_names: &HashMap<String, String>,
) {
    let enemy_box_x = 8;
    let enemy_box_y = 8;
    let enemy_box_w = 96;
    let enemy_box_h = 32;
    draw_ui_box(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        enemy_box_x,
        enemy_box_y,
        enemy_box_w,
        enemy_box_h,
    );

    let enemy_name = pokemon_display_names
        .get(&battle.enemy.species)
        .map(|s| s.as_str())
        .unwrap_or(battle.enemy.species.as_str());
    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        enemy_name,
        enemy_box_x + 6,
        enemy_box_y + 4,
    );
    let enemy_level = format!("L{}", battle.enemy.level);
    let level_x = enemy_box_x + enemy_box_w - 6 - text_width_px(&enemy_level);
    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        &enemy_level,
        level_x,
        enemy_box_y + 4,
    );
    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        "HP",
        enemy_box_x + 6,
        enemy_box_y + 20,
    );
    draw_hp_bar(
        gb_frame,
        enemy_box_x + 30,
        enemy_box_y + 22,
        56,
        6,
        battle.enemy.hp,
        battle.enemy.max_hp,
    );

    let player_box_w = 88;
    let player_box_h = 40;
    let player_box_x = GB_WIDTH as i32 - player_box_w - 8;
    let player_box_y = (GB_HEIGHT as i32 - (6 * TILE_SIZE) as i32) - player_box_h - 4;
    draw_ui_box(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        player_box_x,
        player_box_y,
        player_box_w,
        player_box_h,
    );

    let player_name = pokemon_display_names
        .get(&battle.player.species)
        .map(|s| s.as_str())
        .unwrap_or(battle.player.species.as_str());
    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        player_name,
        player_box_x + 6,
        player_box_y + 4,
    );
    let player_level = format!("L{}", battle.player.level);
    let player_level_x = player_box_x + player_box_w - 6 - text_width_px(&player_level);
    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        &player_level,
        player_level_x,
        player_box_y + 4,
    );

    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        "HP",
        player_box_x + 6,
        player_box_y + 20,
    );
    draw_hp_bar(
        gb_frame,
        player_box_x + 30,
        player_box_y + 22,
        56,
        6,
        battle.player.hp,
        battle.player.max_hp,
    );
    let hp_text = format!("{}/{}", battle.player.hp, battle.player.max_hp);
    draw_text_line(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        font,
        &hp_text,
        player_box_x + player_box_w - 6 - text_width_px(&hp_text),
        player_box_y + 30,
    );
}

fn draw_battle_bottom_box(
    gb_frame: &mut [u32],
    font: &GrayscaleImage,
    battle: &BattleState,
    item_display_names: &HashMap<String, String>,
    move_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
) {
    if let Some(dialog) = &battle.dialog {
        draw_text_box(gb_frame, font, dialog);
        return;
    }

    let box_h = (6 * TILE_SIZE) as i32;
    let y0 = GB_HEIGHT as i32 - box_h;

    draw_ui_box(gb_frame, GB_WIDTH, GB_HEIGHT, 0, y0, GB_WIDTH as i32, box_h);

    match battle.ui.screen {
        BattleUiScreen::Command => {
            let player_name = pokemon_display_names
                .get(&battle.player.species)
                .map(|s| s.as_str())
                .unwrap_or(battle.player.species.as_str());
            draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, "What will", 8, y0 + 8);
            draw_text_line(
                gb_frame,
                GB_WIDTH,
                GB_HEIGHT,
                font,
                &format!("{player_name} do?"),
                8,
                y0 + 16,
            );

            let menu_x = 80;
            let menu_y = y0 + 4;
            let menu_w = 78;
            let menu_h = box_h - 8;
            draw_ui_box(
                gb_frame, GB_WIDTH, GB_HEIGHT, menu_x, menu_y, menu_w, menu_h,
            );

            let opts = ["FIGHT", "PKMN", "ITEM", "RUN"];
            for i in 0..4usize {
                let row = i / 2;
                let col = i % 2;
                let x = menu_x + 8 + (col as i32) * 32;
                let y = menu_y + 8 + (row as i32) * 16;
                let selected = i == battle.ui.selection;
                if selected {
                    let w = text_width_px(opts[i]) + 4;
                    fill_rect(
                        gb_frame,
                        GB_WIDTH,
                        GB_HEIGHT,
                        x - 2,
                        y - 2,
                        w,
                        TILE_SIZE as i32,
                        dmg_palette_color(3),
                    );
                    draw_text_line_tinted(gb_frame, GB_WIDTH, GB_HEIGHT, font, opts[i], x, y, 0);
                } else {
                    draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, opts[i], x, y);
                }
            }
        }
        BattleUiScreen::Fight => {
            draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, "FIGHT", 8, y0 + 8);

            let moves: Vec<&String> = battle.player.moves.iter().take(4).collect();
            for (i, mov) in moves.iter().enumerate() {
                let display = move_display_names
                    .get(*mov)
                    .map(|s| s.as_str())
                    .unwrap_or(mov.as_str());
                let y = y0 + 16 + (i as i32) * TILE_SIZE as i32;
                let selected = i == battle.ui.selection;
                if selected {
                    fill_rect(
                        gb_frame,
                        GB_WIDTH,
                        GB_HEIGHT,
                        6,
                        y - 2,
                        GB_WIDTH as i32 - 12,
                        TILE_SIZE as i32,
                        dmg_palette_color(3),
                    );
                    draw_text_line_tinted(gb_frame, GB_WIDTH, GB_HEIGHT, font, display, 8, y, 0);
                } else {
                    draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, display, 8, y);
                }
            }
        }
        BattleUiScreen::Party => {
            draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, "PKMN", 8, y0 + 8);
            let list_y0 = y0 + 16;
            if battle.player_party.is_empty() {
                draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, "None.", 8, list_y0);
            } else {
                for (i, mon) in battle.player_party.iter().take(6).enumerate() {
                    let name = pokemon_display_names
                        .get(&mon.species)
                        .map(|s| s.as_str())
                        .unwrap_or(mon.species.as_str());
                    let hp_s = format!("{}/{}", mon.hp, mon.max_hp);
                    let hp_x = GB_WIDTH as i32 - 8 - text_width_px(&hp_s);
                    let y = list_y0 + (i as i32) * TILE_SIZE as i32;
                    let selected = i == battle.ui.selection;
                    if selected {
                        fill_rect(
                            gb_frame,
                            GB_WIDTH,
                            GB_HEIGHT,
                            6,
                            y - 2,
                            GB_WIDTH as i32 - 12,
                            TILE_SIZE as i32,
                            dmg_palette_color(3),
                        );
                        draw_text_line_tinted(gb_frame, GB_WIDTH, GB_HEIGHT, font, name, 8, y, 0);
                        draw_text_line_tinted(
                            gb_frame, GB_WIDTH, GB_HEIGHT, font, &hp_s, hp_x, y, 0,
                        );
                    } else {
                        draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, name, 8, y);
                        draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, &hp_s, hp_x, y);
                    }
                }
            }
        }
        BattleUiScreen::Items => {
            draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, "ITEM", 8, y0 + 8);

            let ids = inventory_item_ids_sorted_map(&battle.inventory);
            let ids: Vec<String> = ids
                .into_iter()
                .filter(|id| battle.inventory.get(id).copied().unwrap_or(0) > 0)
                .collect();
            let list_y0 = y0 + 16;
            if ids.is_empty() {
                draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, "No items.", 8, list_y0);
            } else {
                let max_scroll = ids.len().saturating_sub(BATTLE_ITEMS_VISIBLE_ROWS);
                let scroll = battle.ui.scroll.min(max_scroll);
                for row in 0..BATTLE_ITEMS_VISIBLE_ROWS {
                    let i = scroll + row;
                    if i >= ids.len() {
                        break;
                    }
                    let item_id = &ids[i];
                    let count = battle.inventory.get(item_id).copied().unwrap_or(0);
                    let name = item_display_names
                        .get(item_id)
                        .map(|s| s.as_str())
                        .unwrap_or(item_id.as_str());
                    let count_s = format!("x{count}");
                    let count_x = GB_WIDTH as i32 - 8 - text_width_px(&count_s);
                    let y = list_y0 + (row as i32) * TILE_SIZE as i32;
                    let selected = i == battle.ui.selection;
                    if selected {
                        fill_rect(
                            gb_frame,
                            GB_WIDTH,
                            GB_HEIGHT,
                            6,
                            y - 2,
                            GB_WIDTH as i32 - 12,
                            TILE_SIZE as i32,
                            dmg_palette_color(3),
                        );
                        draw_text_line_tinted(gb_frame, GB_WIDTH, GB_HEIGHT, font, name, 8, y, 0);
                        draw_text_line_tinted(
                            gb_frame, GB_WIDTH, GB_HEIGHT, font, &count_s, count_x, y, 0,
                        );
                    } else {
                        draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, name, 8, y);
                        draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, &count_s, count_x, y);
                    }
                }
            }
        }
        BattleUiScreen::LearnMove => {
            let move_to_learn = battle.ui.move_to_learn.as_ref().map(|s| s.as_str()).unwrap_or("");
            let move_name = move_display_names
                .get(move_to_learn)
                .map(|s| s.as_str())
                .unwrap_or(move_to_learn);

            draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, &format!("Learning {}", move_name), 8, y0 + 8);
            draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, "Forget which?", 8, y0 + 16);

            let list_y0 = y0 + 24;
            let mon = &battle.player_party[battle.player_party_index];

            // Show current moves + "Don't learn" option
            for (i, mov) in mon.moves.iter().enumerate() {
                let display = move_display_names
                    .get(mov)
                    .map(|s| s.as_str())
                    .unwrap_or(mov.as_str());
                let y = list_y0 + (i as i32) * TILE_SIZE as i32;
                let selected = i == battle.ui.selection;
                if selected {
                    fill_rect(
                        gb_frame,
                        GB_WIDTH,
                        GB_HEIGHT,
                        6,
                        y - 2,
                        GB_WIDTH as i32 - 12,
                        TILE_SIZE as i32,
                        dmg_palette_color(3),
                    );
                    draw_text_line_tinted(gb_frame, GB_WIDTH, GB_HEIGHT, font, display, 8, y, 0);
                } else {
                    draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, display, 8, y);
                }
            }

            // Show "Don't learn" option
            let cancel_i = mon.moves.len();
            let y = list_y0 + (cancel_i as i32) * TILE_SIZE as i32;
            let selected = cancel_i == battle.ui.selection;
            if selected {
                fill_rect(
                    gb_frame,
                    GB_WIDTH,
                    GB_HEIGHT,
                    6,
                    y - 2,
                    GB_WIDTH as i32 - 12,
                    TILE_SIZE as i32,
                    dmg_palette_color(3),
                );
                draw_text_line_tinted(gb_frame, GB_WIDTH, GB_HEIGHT, font, "Don't learn", 8, y, 0);
            } else {
                draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, "Don't learn", 8, y);
            }
        }
        BattleUiScreen::Evolution => {
            // Evolution screen just shows dialog, no special UI needed
            draw_text_line(gb_frame, GB_WIDTH, GB_HEIGHT, font, "Evolution...", 8, y0 + 8);
        }
    }
}

fn draw_ui_box(gb_frame: &mut [u32], gb_w: u32, gb_h: u32, x0: i32, y0: i32, w: i32, h: i32) {
    fill_rect(gb_frame, gb_w, gb_h, x0, y0, w, h, dmg_palette_color(3));
    fill_rect(
        gb_frame,
        gb_w,
        gb_h,
        x0 + 2,
        y0 + 2,
        w - 4,
        h - 4,
        dmg_palette_color(0),
    );
}

fn draw_hp_bar(gb_frame: &mut [u32], x0: i32, y0: i32, w: i32, h: i32, hp: u16, max_hp: u16) {
    fill_rect(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        x0,
        y0,
        w,
        h,
        dmg_palette_color(3),
    );
    fill_rect(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        x0 + 1,
        y0 + 1,
        w - 2,
        h - 2,
        dmg_palette_color(0),
    );
    if max_hp == 0 || hp == 0 {
        return;
    }
    let inner_w = (w - 2).max(0);
    let filled = ((inner_w as i64 * hp as i64) / max_hp as i64).clamp(1, inner_w as i64) as i32;
    fill_rect(
        gb_frame,
        GB_WIDTH,
        GB_HEIGHT,
        x0 + 1,
        y0 + 1,
        filled,
        h - 2,
        dmg_palette_color(2),
    );
}

fn text_width_px(s: &str) -> i32 {
    s.chars().count() as i32 * TILE_SIZE as i32
}

fn fill_rect(
    gb_frame: &mut [u32],
    gb_w: u32,
    gb_h: u32,
    x0: i32,
    y0: i32,
    w: i32,
    h: i32,
    color: u32,
) {
    let x1 = (x0 + w).clamp(0, gb_w as i32);
    let y1 = (y0 + h).clamp(0, gb_h as i32);
    let x0 = x0.clamp(0, gb_w as i32);
    let y0 = y0.clamp(0, gb_h as i32);
    for y in y0..y1 {
        for x in x0..x1 {
            gb_frame[(y as u32 * gb_w + x as u32) as usize] = color;
        }
    }
}

fn draw_text_line(
    gb_frame: &mut [u32],
    gb_w: u32,
    gb_h: u32,
    font: &GrayscaleImage,
    text: &str,
    x0: i32,
    y0: i32,
) {
    let mut x = x0;
    for ch in text.chars() {
        if ch == ' ' {
            x += TILE_SIZE as i32;
            continue;
        }
        let Some(code) = char_to_font_code(ch) else {
            x += TILE_SIZE as i32;
            continue;
        };
        blit_font_glyph_to_gb(gb_frame, gb_w, gb_h, font, code, x, y0);
        x += TILE_SIZE as i32;
    }
}

fn draw_text_line_tinted(
    gb_frame: &mut [u32],
    gb_w: u32,
    gb_h: u32,
    font: &GrayscaleImage,
    text: &str,
    x0: i32,
    y0: i32,
    color_index: u8,
) {
    let mut x = x0;
    for ch in text.chars() {
        if ch == ' ' {
            x += TILE_SIZE as i32;
            continue;
        }
        let Some(code) = char_to_font_code(ch) else {
            x += TILE_SIZE as i32;
            continue;
        };
        blit_font_glyph_to_gb_tinted(gb_frame, gb_w, gb_h, font, code, x, y0, color_index);
        x += TILE_SIZE as i32;
    }
}

fn char_to_font_code(ch: char) -> Option<u8> {
    match ch {
        'A'..='Z' => Some(0x80 + (ch as u8 - b'A')),
        'a'..='z' => Some(0xa0 + (ch as u8 - b'a')),
        '0'..='9' => Some(0xf6 + (ch as u8 - b'0')),
        '(' => Some(0x9a),
        ')' => Some(0x9b),
        ':' => Some(0x9c),
        ';' => Some(0x9d),
        '[' => Some(0x9e),
        ']' => Some(0x9f),
        'é' => Some(0xba),
        '\'' => Some(0xe0),
        '-' => Some(0xe3),
        '?' => Some(0xe6),
        '!' => Some(0xe7),
        '.' => Some(0xe8),
        '/' => Some(0xf3),
        ',' => Some(0xf4),
        '▼' => Some(0xee),
        _ => None,
    }
}

fn blit_font_glyph_to_gb(
    gb_frame: &mut [u32],
    gb_w: u32,
    gb_h: u32,
    font: &GrayscaleImage,
    code: u8,
    dst_x: i32,
    dst_y: i32,
) {
    if code < 0x80 {
        return;
    }
    if font.width % TILE_SIZE != 0 || font.height % TILE_SIZE != 0 {
        return;
    }

    let tiles_per_row = font.width / TILE_SIZE;
    let tile_index = (code - 0x80) as u32;
    let src_x0 = (tile_index % tiles_per_row) * TILE_SIZE;
    let src_y0 = (tile_index / tiles_per_row) * TILE_SIZE;

    if src_x0 + TILE_SIZE > font.width || src_y0 + TILE_SIZE > font.height {
        return;
    }

    for y in 0..TILE_SIZE as i32 {
        let sy = src_y0 as i32 + y;
        let dy = dst_y + y;
        if dy < 0 || dy >= gb_h as i32 {
            continue;
        }
        for x in 0..TILE_SIZE as i32 {
            let sx = src_x0 as i32 + x;
            let dx = dst_x + x;
            if dx < 0 || dx >= gb_w as i32 {
                continue;
            }

            let src_i = (sy as u32 * font.width + sx as u32) as usize;
            let luma = font.luma[src_i];
            let gb_color = luma_to_gb_color(luma);
            if gb_color == 0 {
                continue;
            }
            gb_frame[(dy as u32 * gb_w + dx as u32) as usize] = dmg_palette_color(gb_color);
        }
    }
}

fn blit_font_glyph_to_gb_tinted(
    gb_frame: &mut [u32],
    gb_w: u32,
    gb_h: u32,
    font: &GrayscaleImage,
    code: u8,
    dst_x: i32,
    dst_y: i32,
    color_index: u8,
) {
    if code < 0x80 {
        return;
    }
    if font.width % TILE_SIZE != 0 || font.height % TILE_SIZE != 0 {
        return;
    }

    let tiles_per_row = font.width / TILE_SIZE;
    let tile_index = (code - 0x80) as u32;
    let src_x0 = (tile_index % tiles_per_row) * TILE_SIZE;
    let src_y0 = (tile_index / tiles_per_row) * TILE_SIZE;

    if src_x0 + TILE_SIZE > font.width || src_y0 + TILE_SIZE > font.height {
        return;
    }

    let fg = dmg_palette_color(color_index);
    for y in 0..TILE_SIZE as i32 {
        let sy = src_y0 as i32 + y;
        let dy = dst_y + y;
        if dy < 0 || dy >= gb_h as i32 {
            continue;
        }
        for x in 0..TILE_SIZE as i32 {
            let sx = src_x0 as i32 + x;
            let dx = dst_x + x;
            if dx < 0 || dx >= gb_w as i32 {
                continue;
            }

            let src_i = (sy as u32 * font.width + sx as u32) as usize;
            let luma = font.luma[src_i];
            if luma_to_gb_color(luma) == 0 {
                continue;
            }
            gb_frame[(dy as u32 * gb_w + dx as u32) as usize] = fg;
        }
    }
}

fn player_draw_params(player: &Player) -> (i32, i32, i32, i32, u32, bool) {
    let (base_tx, base_ty, offset_px_x, offset_px_y, facing, progress_px) = match &player.move_anim
    {
        Some(m) => {
            let (dx, dy) = facing_to_dir_px(m.dir, m.progress_px);
            (m.start_tx, m.start_ty, dx, dy, m.dir, m.progress_px)
        }
        None => (player.tx, player.ty, 0, 0, player.facing, 0),
    };

    let _ = progress_px;
    let (frame_index, mirror_x) = player_sprite_frame(player, facing);
    (
        base_tx,
        base_ty,
        offset_px_x,
        offset_px_y,
        frame_index,
        mirror_x,
    )
}

fn player_sprite_frame(player: &Player, facing: Facing) -> (u32, bool) {
    let moving = player.move_anim.is_some();
    let anim = player.walk_anim_frame & 3;
    let use_walk = moving && (anim == 1 || anim == 3);
    let use_flip = moving && anim == 3;

    match facing {
        Facing::Down => {
            let frame = if use_walk { 3 } else { 0 };
            (frame, use_flip)
        }
        Facing::Up => {
            let frame = if use_walk { 4 } else { 1 };
            (frame, use_flip)
        }
        Facing::Left => {
            let frame = if use_walk { 5 } else { 2 };
            (frame, false)
        }
        Facing::Right => {
            let frame = if use_walk { 5 } else { 2 };
            (frame, true)
        }
    }
}

fn blit_sprite_frame_to_gb(
    gb_frame: &mut [u32],
    gb_w: u32,
    gb_h: u32,
    sprite_sheet: &GrayscaleImage,
    frame_index: u32,
    dst_x: i32,
    dst_y: i32,
    mirror_x: bool,
) {
    let frame_w = PLAYER_FRAME_SIZE_PX as i32;
    let frame_h = PLAYER_FRAME_SIZE_PX as i32;
    let src_x0 = 0i32;
    let src_y0 = (frame_index * PLAYER_FRAME_SIZE_PX) as i32;

    if sprite_sheet.width as i32 != frame_w {
        return;
    }
    if src_y0 + frame_h > sprite_sheet.height as i32 {
        return;
    }

    for y in 0..frame_h {
        let sy = src_y0 + y;
        let dy = dst_y + y;
        if dy < 0 || dy >= gb_h as i32 {
            continue;
        }
        for x in 0..frame_w {
            let sx = if mirror_x {
                src_x0 + (frame_w - 1 - x)
            } else {
                src_x0 + x
            };
            let dx = dst_x + x;
            if dx < 0 || dx >= gb_w as i32 {
                continue;
            }

            let src_i = (sy as u32 * sprite_sheet.width + sx as u32) as usize;
            let luma = sprite_sheet.luma[src_i];
            let gb_color = luma_to_gb_color(luma);
            if gb_color == 0 {
                continue;
            }
            gb_frame[(dy as u32 * gb_w + dx as u32) as usize] = dmg_palette_color(gb_color);
        }
    }
}

fn blit_grayscale_to_gb(
    gb_frame: &mut [u32],
    gb_w: u32,
    gb_h: u32,
    img: &GrayscaleImage,
    dst_x: i32,
    dst_y: i32,
) {
    for y in 0..img.height as i32 {
        let sy = y as u32;
        let dy = dst_y + y;
        if dy < 0 || dy >= gb_h as i32 {
            continue;
        }
        for x in 0..img.width as i32 {
            let sx = x as u32;
            let dx = dst_x + x;
            if dx < 0 || dx >= gb_w as i32 {
                continue;
            }

            let src_i = (sy * img.width + sx) as usize;
            let luma = img.luma[src_i];
            let gb_color = luma_to_gb_color(luma);
            gb_frame[(dy as u32 * gb_w + dx as u32) as usize] = dmg_palette_color(gb_color);
        }
    }
}

fn blit_grayscale_to_gb_transparent(
    gb_frame: &mut [u32],
    gb_w: u32,
    gb_h: u32,
    img: &GrayscaleImage,
    dst_x: i32,
    dst_y: i32,
) {
    for y in 0..img.height as i32 {
        let sy = y as u32;
        let dy = dst_y + y;
        if dy < 0 || dy >= gb_h as i32 {
            continue;
        }
        for x in 0..img.width as i32 {
            let sx = x as u32;
            let dx = dst_x + x;
            if dx < 0 || dx >= gb_w as i32 {
                continue;
            }

            let src_i = (sy * img.width + sx) as usize;
            let luma = img.luma[src_i];
            let gb_color = luma_to_gb_color(luma);
            if gb_color == 0 {
                continue;
            }
            gb_frame[(dy as u32 * gb_w + dx as u32) as usize] = dmg_palette_color(gb_color);
        }
    }
}

fn blit_scaled_letterbox(
    src: &[u32],
    src_w: u32,
    src_h: u32,
    dst: &mut [u32],
    dst_w: u32,
    dst_h: u32,
    border_color: u32,
) {
    dst.fill(border_color);

    let scale = (dst_w / src_w).min(dst_h / src_h).max(1);
    let render_w = src_w * scale;
    let render_h = src_h * scale;
    let x0 = (dst_w - render_w) / 2;
    let y0 = (dst_h - render_h) / 2;

    for y in 0..render_h {
        let src_y = y / scale;
        let dst_y = y0 + y;
        for x in 0..render_w {
            let src_x = x / scale;
            let dst_x = x0 + x;
            dst[(dst_y * dst_w + dst_x) as usize] = src[(src_y * src_w + src_x) as usize];
        }
    }
}

fn luma_to_gb_color(luma: u8) -> u8 {
    let level = ((luma as u16) * 3 + 127) / 255;
    (3 - level) as u8
}

fn dmg_palette_color(color_index: u8) -> u32 {
    match color_index {
        0 => 0x00E0F8D0,
        1 => 0x0088C070,
        2 => 0x00346856,
        _ => 0x00081820,
    }
}

fn load_map_view(
    pokered_root: &Path,
    game_data: &game_data::GameData,
    map_name: &str,
    last_outside_map: Option<String>,
    picked_up_items: &HashSet<(String, String)>,
    events: &HashSet<String>,
) -> Result<MapView, Box<dyn Error>> {
    let header = game_data
        .map_headers
        .get(map_name)
        .ok_or_else(|| format!("Missing map header for `{map_name}`"))?;

    let (width_blocks, height_blocks) = map_dimensions_blocks_for_id(&header.map_id).ok_or_else(|| {
        format!(
            "Missing map dimensions for `{}` (see `src/data/map_constants.rs`)",
            header.map_id
        )
    })?;

    let map_blk_path = pokered_root.join("maps").join(format!("{map_name}.blk"));
    let blocks = fs::read(&map_blk_path)?;
    let expected_len = (width_blocks * height_blocks) as usize;
    if blocks.len() != expected_len {
        return Err(format!(
            "Map `{map_name}` expected `{expected_len}` bytes in `{map_blk_path:?}`, got `{}`",
            blocks.len()
        )
        .into());
    }

    let tileset_label = tileset_symbol_to_label(&header.tileset);
    let tileset_base = game_data
        .tileset_bases
        .get(&tileset_label)
        .ok_or_else(|| {
            format!(
                "Unknown tileset `{}` (label `{tileset_label}`) in `data/tilesets.ron`",
                header.tileset,
            )
        })?
        .to_string();

    let tileset_png_path = pokered_root
        .join("gfx/tilesets")
        .join(format!("{tileset_base}.png"));
    let tileset_png = load_grayscale_png(&tileset_png_path)?;
    if tileset_png.width % TILE_SIZE != 0 || tileset_png.height % TILE_SIZE != 0 {
        return Err(format!(
            "Tileset image `{tileset_png_path:?}` is `{}`x`{}`, not divisible by `{TILE_SIZE}`",
            tileset_png.width, tileset_png.height
        )
        .into());
    }

    let tiles_per_row = tileset_png.width / TILE_SIZE;
    let tile_count = (tileset_png.width / TILE_SIZE) * (tileset_png.height / TILE_SIZE);
    let grass_tile_id = game_data
        .tileset_grass_tile_ids
        .get(&tileset_label)
        .copied()
        .ok_or_else(|| format!("Missing grass tile id for `{tileset_label}` in `data/tileset_headers.ron`"))?;
    let counter_tiles = get_counter_tiles_for_tileset(&tileset_label);
    let tileset = Tileset {
        img: tileset_png,
        tiles_per_row,
        tile_count,
        grass_tile_id,
        counter_tiles,
    };

    let passable_tiles = game_data
        .tileset_collision
        .get(&tileset_label)
        .cloned()
        .ok_or_else(|| format!("Missing collision list for `{tileset_label}` in `data/tileset_collision.ron`"))?;

    let blockset_path = pokered_root
        .join("gfx/blocksets")
        .join(format!("{tileset_base}.bst"));
    let blockset_bytes = fs::read(&blockset_path)?;
    if blockset_bytes.len() % 16 != 0 {
        return Err(format!(
            "Blockset `{blockset_path:?}` has `{}` bytes, not divisible by 16",
            blockset_bytes.len()
        )
        .into());
    }

    let mut blockset_blocks = Vec::with_capacity(blockset_bytes.len() / 16);
    for chunk in blockset_bytes.chunks_exact(16) {
        let mut block = [0u8; 16];
        block.copy_from_slice(chunk);
        blockset_blocks.push(block);
    }
    let blockset = Blockset {
        blocks: blockset_blocks,
    };

    let player_sprite = load_grayscale_png(&pokered_root.join("gfx/sprites/red.png"))?;
    // Match original Pokemon Red spawn positions (special_warps.asm)
    // NewGameWarp: REDS_HOUSE_2F at (3, 6) in block coords = (6, 12) in tile coords
    let (player_tx, player_ty) = if map_name == "RedsHouse2F" {
        (6, 12) // Original new game spawn position
    } else {
        find_spawn_position(
            width_blocks,
            height_blocks,
            &blocks,
            &blockset,
            &passable_tiles,
        )
        .unwrap_or((0, 0))
    };

    let events_data = game_data
        .map_events
        .get(map_name)
        .ok_or_else(|| format!("Missing map events for `{map_name}`"))?;
    let warp_events = events_data.warp_events.clone();
    let bg_events = events_data.bg_events.clone();
    let hidden_events = events_data.hidden_events.clone();
    let mut object_events = events_data.object_events.clone();
    object_events.retain(|o| {
        matches!(o.kind, ObjectKind::Person | ObjectKind::Trainer { .. })
            || !picked_up_items.contains(&(header.map_name.clone(), o.text_id.clone()))
    });
    if header.map_name == "OaksLab" && events.contains("EVENT_GOT_STARTER") {
        let ids = [
            "TEXT_OAKSLAB_CHARMANDER_POKE_BALL",
            "TEXT_OAKSLAB_SQUIRTLE_POKE_BALL",
            "TEXT_OAKSLAB_BULBASAUR_POKE_BALL",
        ];
        object_events.retain(|o| !ids.contains(&o.text_id.as_str()));
    }
    if header.map_name == "OaksLab" && events.contains("EVENT_BATTLED_RIVAL_IN_OAKS_LAB") {
        object_events.retain(|o| o.text_id != "TEXT_OAKSLAB_RIVAL");
    }
    // Match original Pokemon Red: OAK2 (at bottom of lab) is only shown during intro sequence
    // when Oak walks in from the door. OAK1 (at top) is the main Oak after intro.
    // Since we skip the intro, always hide OAK2 (see hide_show_data.asm: both start HIDDEN,
    // OAK2 shown briefly during entry, then hidden when OAK1 is shown at the top)
    if header.map_name == "OaksLab" {
        object_events.retain(|o| o.text_id != "TEXT_OAKSLAB_OAK2");
    }
    if header.map_name == "PalletTown" {
        object_events.retain(|o| o.text_id != "TEXT_PALLETTOWN_OAK");
    }
    if header.map_name == "Route22" {
        let ids = ["TEXT_ROUTE22_RIVAL1", "TEXT_ROUTE22_RIVAL2"];
        let wants_battle = events.contains("EVENT_ROUTE22_RIVAL_WANTS_BATTLE")
            || (events.contains("EVENT_GOT_POKEDEX")
                && !events.contains("EVENT_BEAT_ROUTE22_RIVAL_1ST_BATTLE"));
        if !wants_battle || events.contains("EVENT_BEAT_ROUTE22_RIVAL_1ST_BATTLE") {
            object_events.retain(|o| !ids.contains(&o.text_id.as_str()));
        } else {
            object_events.retain(|o| o.text_id != "TEXT_ROUTE22_RIVAL2");
        }
    }
    if header.map_name == "BluesHouse" {
        if events.contains("EVENT_GOT_TOWN_MAP") {
            object_events.retain(|o| o.text_id != "TEXT_BLUESHOUSE_TOWN_MAP");
        }
        object_events.retain(|o| o.text_id != "TEXT_BLUESHOUSE_DAISY_WALKING");
    }
    let object_sprites = load_object_sprite_sheets(&object_events, &game_data.sprite_id_to_png)?;

    Ok(MapView {
        map_name: header.map_name.clone(),
        map_id: header.map_id.clone(),
        is_outside: is_outside_tileset(&header.tileset),
        last_outside_map,
        connections: header.connections.clone(),
        warp_events,
        bg_events,
        hidden_events,
        object_events,
        object_sprites,
        tileset,
        blockset,
        passable_tiles,
        map: MapData {
            width_blocks,
            height_blocks,
            blocks,
        },
        player: Player {
            tx: player_tx,
            ty: player_ty,
            facing: Facing::Down,
            walk_intra_counter: 0,
            walk_anim_frame: 0,
            sprite: player_sprite,
            move_anim: None,
        },
        camera_tx: 0,
        camera_ty: 0,
        overworld_subtick: 0,
        dialog: None,
        menu: None,
        shop: None,
        choice: None,
        pending_battle: None,
        rng: 0,
        money: 3000,
        events: events.clone(),
        inventory: HashMap::new(),
        pc_items: HashMap::new(),
        picked_up_items: HashSet::new(),
        defeated_trainers: HashSet::new(),
        party: Vec::new(),
        pc_boxes: vec![Vec::new(); 12],
        pc_current_box: 0,
        pc: None,
        options: GameOptions::default(),
        pokedex_seen: HashSet::new(),
        pokedex_caught: HashSet::new(),
    })
}

fn parse_map_header(map_header_asm: &str) -> Option<MapHeaderInfo> {
    let mut header: Option<MapHeaderInfo> = None;
    let mut connections = MapConnections::default();

    for line in map_header_asm.lines() {
        let line = strip_asm_comment(line).trim();
        if line.starts_with("map_header") {
            let args = line.trim_start_matches("map_header").trim();
            let mut parts = args.split(',').map(|s| s.trim());
            let map_name = parts.next()?.to_string();
            let map_id = parts.next()?.to_string();
            let tileset = parts.next()?.to_string();
            header = Some(MapHeaderInfo {
                map_name,
                map_id,
                tileset,
                connections: MapConnections::default(),
            });
            continue;
        }

        if line.starts_with("connection") {
            let args = line.trim_start_matches("connection").trim();
            let mut parts = args.split(',').map(|s| s.trim());
            let dir = parts.next()?;
            let dest_map_name = parts.next()?.to_string();
            let _dest_map_id = parts.next()?;
            let offset = parse_asm_i32(parts.next()?)?;
            let conn = MapConnection {
                map_name: dest_map_name,
                offset_blocks: offset,
            };
            match dir {
                "north" => connections.north = Some(conn),
                "south" => connections.south = Some(conn),
                "west" => connections.west = Some(conn),
                "east" => connections.east = Some(conn),
                _ => {}
            }
        }
    }
    let mut header = header?;
    header.connections = connections;
    Some(header)
}

fn parse_map_dimensions(map_id: &str, map_constants_asm: &str) -> Option<(u32, u32)> {
    for line in map_constants_asm.lines() {
        let line = strip_asm_comment(line).trim();
        if !line.starts_with("map_const") {
            continue;
        }

        let args = line.trim_start_matches("map_const").trim();
        let mut parts = args.split(',').map(|s| s.trim());
        let name = parts.next()?;
        if name != map_id {
            continue;
        }
        let width = parts.next()?.parse().ok()?;
        let height = parts.next()?.parse().ok()?;
        return Some((width, height));
    }
    None
}

fn map_dimensions_blocks_for_id(map_id: &str) -> Option<(u32, u32)> {
    for (id, w, h) in data::map_constants::MAP_DIMENSIONS {
        if *id == map_id {
            return Some((*w as u32, *h as u32));
        }
    }
    None
}

fn build_map_id_to_name(pokered_root: &Path) -> Result<HashMap<String, String>, Box<dyn Error>> {
    let headers_dir = pokered_root.join("data/maps/headers");
    let mut map = HashMap::new();
    for entry in fs::read_dir(&headers_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("asm") {
            continue;
        }
        let asm = fs::read_to_string(&path)?;
        let Some(header) = parse_map_header(&asm) else {
            continue;
        };
        map.insert(header.map_id, header.map_name);
    }
    Ok(map)
}

fn is_outside_tileset(tileset: &str) -> bool {
    tileset == "OVERWORLD" || tileset == "PLATEAU"
}

fn parse_warp_events(object_asm: &str) -> Vec<WarpEvent> {
    let mut warps = Vec::new();
    let mut in_warps = false;
    for raw_line in object_asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.starts_with("def_warp_events") {
            in_warps = true;
            continue;
        }
        if !in_warps {
            continue;
        }
        if line.starts_with("def_bg_events")
            || line.starts_with("def_object_events")
            || line.starts_with("def_warps_to")
        {
            break;
        }
        if !line.starts_with("warp_event") {
            continue;
        }

        let args = line.trim_start_matches("warp_event").trim();
        let mut parts = args.split(',').map(|s| s.trim());
        let x = parts.next().and_then(parse_asm_i32);
        let y = parts.next().and_then(parse_asm_i32);
        let dest_map = parts.next().map(|s| s.trim());
        let dest_warp_id = parts.next().and_then(parse_asm_usize);
        let (Some(x), Some(y), Some(dest_map), Some(dest_warp_id)) = (x, y, dest_map, dest_warp_id)
        else {
            continue;
        };

        let dest_map = if dest_map == "LAST_MAP" {
            WarpDest::LastMap
        } else {
            WarpDest::MapId(dest_map.to_string())
        };

        warps.push(WarpEvent {
            x,
            y,
            dest_map,
            dest_warp_id,
        });
    }
    warps
}

fn parse_bg_events(object_asm: &str) -> Vec<BgEvent> {
    let mut events = Vec::new();
    let mut in_bg = false;
    for raw_line in object_asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.starts_with("def_bg_events") {
            in_bg = true;
            continue;
        }
        if !in_bg {
            continue;
        }
        if line.starts_with("def_object_events") || line.starts_with("def_warps_to") {
            break;
        }
        if !line.starts_with("bg_event") {
            continue;
        }

        let args = line.trim_start_matches("bg_event").trim();
        let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
        if parts.len() < 3 {
            continue;
        }
        let x = parts.first().and_then(|s| parse_asm_i32(s));
        let y = parts.get(1).and_then(|s| parse_asm_i32(s));
        let text_id = parts.last().map(|s| (*s).to_string());
        let (Some(x), Some(y), Some(text_id)) = (x, y, text_id) else {
            continue;
        };
        events.push(BgEvent {
            tx: x * PLAYER_STEP_TILES,
            ty: y * PLAYER_STEP_TILES,
            text_id,
        });
    }
    events
}

fn parse_object_events(
    object_asm: &str,
    sprite_constants: &HashMap<String, u8>,
) -> Vec<ObjectEvent> {
    let mut objects = Vec::new();
    let mut in_objects = false;
    for raw_line in object_asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.starts_with("def_object_events") {
            in_objects = true;
            continue;
        }
        if !in_objects {
            continue;
        }
        if line.starts_with("def_warps_to") {
            break;
        }
        if !line.starts_with("object_event") {
            continue;
        }

        let args = line.trim_start_matches("object_event").trim();
        let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
        if parts.len() < 6 {
            continue;
        };
        let x_units = parts.first().and_then(|s| parse_asm_i32(s));
        let y_units = parts.get(1).and_then(|s| parse_asm_i32(s));
        let sprite_sym = parts.get(2).copied();
        let movement_sym = parts.get(3).copied();
        let range_sym = parts.get(4).copied();
        let text_id = parts.get(5).map(|s| (*s).to_string());
        let (
            Some(x_units),
            Some(y_units),
            Some(sprite_sym),
            Some(movement_sym),
            Some(range_sym),
            Some(text_id),
        ) = (
            x_units,
            y_units,
            sprite_sym,
            movement_sym,
            range_sym,
            text_id,
        )
        else {
            continue;
        };
        let Some(sprite_id) = sprite_constants.get(sprite_sym).copied() else {
            continue;
        };

        let movement = match movement_sym {
            "WALK" => ObjectMovement::Walk,
            "STAY" => ObjectMovement::Stay,
            _ => ObjectMovement::Stay,
        };

        let movement_range = match range_sym {
            "ANY_DIR" => ObjectMovementRange::AnyDir,
            "UP_DOWN" => ObjectMovementRange::UpDown,
            "LEFT_RIGHT" => ObjectMovementRange::LeftRight,
            _ => ObjectMovementRange::None,
        };

        let facing = match range_sym {
            "DOWN" => Facing::Down,
            "UP" => Facing::Up,
            "LEFT" => Facing::Left,
            "RIGHT" => Facing::Right,
            _ => Facing::Down,
        };

        let kind = if parts.len() >= 8 && parts[6].starts_with("OPP_") {
            ObjectKind::Trainer {
                opponent: parts[6].to_string(),
                trainer_number: parts[7].to_string(),
            }
        } else if parts.len() >= 7 && parts[6] != "0" {
            ObjectKind::Item {
                item_id: parts[6].to_string(),
            }
        } else {
            ObjectKind::Person
        };

        let tx = x_units * PLAYER_STEP_TILES;
        let ty = y_units * PLAYER_STEP_TILES;
        let move_delay = (24 + ((x_units as u32 * 13 + y_units as u32 * 17) % 48)) as u8;

        objects.push(ObjectEvent {
            sprite_id,
            tx,
            ty,
            kind,
            movement,
            movement_range,
            text_id,
            facing,
            walk_intra_counter: 0,
            walk_anim_frame: 0,
            move_anim: None,
            move_delay,
        });
    }
    objects
}

fn parse_asm_i32(s: &str) -> Option<i32> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
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

fn parse_asm_usize(s: &str) -> Option<usize> {
    let v = parse_asm_i32(s)?;
    usize::try_from(v).ok()
}

fn parse_asm_u8(s: &str) -> Option<u8> {
    let v = parse_asm_i32(s)?;
    u8::try_from(v).ok()
}

fn parse_asm_quoted_string(s: &str) -> Option<String> {
    let s = s.trim();
    let Some((_, rest)) = s.split_once('"') else {
        return None;
    };
    let Some((value, _)) = rest.split_once('"') else {
        return None;
    };
    Some(value.to_string())
}

fn parse_sprite_constants(asm: &str) -> HashMap<String, u8> {
    let mut out: HashMap<String, u8> = HashMap::new();
    let mut value: u8 = 0;
    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.is_empty() {
            continue;
        }
        if line.starts_with("const_def") {
            value = 0;
            continue;
        }
        if !line.starts_with("const ") {
            continue;
        }
        let name = line.trim_start_matches("const").trim();
        if name.is_empty() {
            continue;
        }
        out.insert(name.to_string(), value);
        value = value.wrapping_add(1);
    }
    out
}

fn parse_trainer_parties(asm: &str) -> HashMap<String, Vec<TrainerParty>> {
    let mut out: HashMap<String, Vec<TrainerParty>> = HashMap::new();
    let mut current_label: Option<String> = None;

    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.is_empty() {
            continue;
        }

        if line.ends_with(':') || line.ends_with("::") {
            let label = line.trim_end_matches(':').trim();
            let label = label.trim_end_matches(':').trim();
            if label.ends_with("Data") {
                current_label = Some(label.to_string());
                out.entry(label.to_string()).or_default();
            } else {
                current_label = None;
            }
            continue;
        }

        if !line.starts_with("db") {
            continue;
        }
        let Some(label) = current_label.clone() else {
            continue;
        };

        let args = line.trim_start_matches("db").trim();
        if args.is_empty() {
            continue;
        }
        let tokens: Vec<&str> = args
            .split(',')
            .map(|t| t.trim())
            .filter(|t| !t.is_empty())
            .collect();
        if tokens.is_empty() {
            continue;
        }

        let mut party: TrainerParty = Vec::new();
        if tokens[0] == "$FF" {
            let mut i = 1usize;
            while i < tokens.len() {
                let level_tok = tokens[i];
                if level_tok == "0" {
                    break;
                }
                let Some(level) = parse_asm_u8(level_tok) else {
                    break;
                };
                let Some(species_tok) = tokens.get(i + 1).copied() else {
                    break;
                };
                if species_tok == "0" {
                    break;
                }
                party.push(TrainerPartyMon {
                    level,
                    species: species_tok.to_string(),
                });
                i += 2;
            }
        } else {
            let Some(level) = parse_asm_u8(tokens[0]) else {
                continue;
            };
            for &species_tok in tokens.iter().skip(1) {
                if species_tok == "0" {
                    break;
                }
                party.push(TrainerPartyMon {
                    level,
                    species: species_tok.to_string(),
                });
            }
        }

        if !party.is_empty() {
            out.entry(label).or_default().push(party);
        }
    }

    out
}

fn load_wild_encounters(
    pokered_root: &Path,
) -> Result<HashMap<String, WildEncounterTable>, Box<dyn Error>> {
    let dir = pokered_root.join("data/wild/maps");
    let mut out: HashMap<String, WildEncounterTable> = HashMap::new();
    for entry in fs::read_dir(&dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("asm") {
            continue;
        }
        let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
            continue;
        };
        let map_name = stem.to_string();
        if map_name.is_empty() {
            continue;
        }
        let asm = fs::read_to_string(&path)?;
        if let Some(table) = parse_wild_encounter_table(&asm) {
            out.insert(map_name, table);
        }
    }
    Ok(out)
}

fn parse_wild_encounter_table(asm: &str) -> Option<WildEncounterTable> {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    enum Section {
        None,
        Grass,
        Water,
    }

    let mut section = Section::None;
    let mut grass_rate: Option<u8> = None;
    let mut water_rate: Option<u8> = None;
    let mut grass: Vec<WildMonSlot> = Vec::new();
    let mut water: Vec<WildMonSlot> = Vec::new();

    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.is_empty() {
            continue;
        }

        if let Some(rest) = line.strip_prefix("def_grass_wildmons") {
            let rate = rest.split_whitespace().next().and_then(parse_asm_u8);
            grass_rate = rate.or(Some(0));
            section = Section::Grass;
            continue;
        }
        if line.starts_with("end_grass_wildmons") {
            section = Section::None;
            continue;
        }
        if let Some(rest) = line.strip_prefix("def_water_wildmons") {
            let rate = rest.split_whitespace().next().and_then(parse_asm_u8);
            water_rate = rate.or(Some(0));
            section = Section::Water;
            continue;
        }
        if line.starts_with("end_water_wildmons") {
            section = Section::None;
            continue;
        }

        if section == Section::None {
            continue;
        }
        if !line.starts_with("db") {
            continue;
        }

        let args = line.trim_start_matches("db").trim();
        let mut parts = args.split(',').map(|s| s.trim());
        let level = parts.next().and_then(parse_asm_u8)?;
        let species = parts.next()?.to_string();
        let slot = WildMonSlot { level, species };

        match section {
            Section::Grass => grass.push(slot),
            Section::Water => water.push(slot),
            Section::None => {}
        }
    }

    Some(WildEncounterTable {
        grass_rate: grass_rate.unwrap_or(0),
        grass,
        water_rate: water_rate.unwrap_or(0),
        water,
    })
}

fn load_wild_slot_thresholds(_pokered_root: &Path) -> Result<Vec<u8>, Box<dyn Error>> {
    Ok(packaged_data::WILD_SLOT_THRESHOLDS.to_vec())
}

fn load_audio_pitches(_pokered_root: &Path) -> Result<Vec<u16>, Box<dyn Error>> {
    Ok(packaged_data::AUDIO_PITCHES.to_vec())
}

fn load_map_music_constants(
    _pokered_root: &Path,
) -> Result<HashMap<String, String>, Box<dyn Error>> {
    Ok(packaged_data::map_music_constants())
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct BaseStats {
    hp: u8,
    attack: u8,
    defense: u8,
    speed: u8,
    special: u8,
    type1: String,
    type2: String,
    catch_rate: u8,
    base_exp: u8,
    growth_rate: GrowthRate,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct MoveData {
    power: u8,
    move_type: String,
    accuracy: u8,
    pp: u8,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct PokemonEvolution {
    method: String,
    #[serde(default)]
    level: Option<u8>,
    #[serde(default)]
    item: Option<String>,
    species: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct PokemonLearnsetMove {
    level: u8,
    #[serde(rename = "move")]
    move_id: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct PokemonMoves {
    evolutions: Vec<PokemonEvolution>,
    learnset: Vec<PokemonLearnsetMove>,
}

fn load_type_chart(_pokered_root: &Path) -> Result<TypeChart, Box<dyn Error>> {
    Ok(packaged_type_chart::type_chart())
}

fn species_const_to_ron_key(species_const: &str) -> String {
    species_const.to_ascii_uppercase().replace('_', "")
}

fn base_stats_for_species<'a>(
    pokemon_stats: &'a HashMap<String, BaseStats>,
    species_const: &str,
) -> Result<&'a BaseStats, Box<dyn Error>> {
    let key = species_const_to_ron_key(species_const);
    pokemon_stats.get(&key).ok_or_else(|| {
        format!("Missing base stats for `{species_const}` (key `{key}`) in `data/pokemon_stats.ron`")
            .into()
    })
}

fn species_const_to_gfx_slug(species_const: &str) -> String {
    match species_const {
        "MR_MIME" => "mr.mime".to_string(),
        _ => species_const.to_ascii_lowercase().replace('_', ""),
    }
}

fn parse_evos_moves_pointer_table(asm: &str) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let mut in_table = false;
    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.is_empty() {
            continue;
        }
        if line.ends_with("EvosMovesPointerTable:") {
            in_table = true;
            continue;
        }
        if !in_table {
            continue;
        }
        if line.starts_with("assert_table_length") {
            break;
        }
        if !line.starts_with("dw ") {
            continue;
        }
        let label = line.trim_start_matches("dw").trim();
        if !label.is_empty() {
            out.push(label.to_string());
        }
    }
    out
}

fn parse_learnset_for_label(asm: &str, label: &str) -> Vec<(u8, String)> {
    let mut out: Vec<(u8, String)> = Vec::new();
    let mut in_block = false;
    let mut in_moves = false;

    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.ends_with(':') || line.ends_with("::") {
            let head = line.trim_end_matches(':').trim();
            let head = head.trim_end_matches(':').trim();
            if head == label {
                in_block = true;
                in_moves = false;
                continue;
            }
            if in_block && !head.starts_with('.') {
                break;
            }
        }

        if !in_block {
            continue;
        }

        if !line.starts_with("db") {
            continue;
        }
        let args = line.trim_start_matches("db").trim();
        if args.is_empty() {
            continue;
        }
        let tokens: Vec<&str> = args
            .split(',')
            .map(|t| t.trim())
            .filter(|t| !t.is_empty())
            .collect();
        if tokens.len() == 1 && tokens[0] == "0" {
            if !in_moves {
                in_moves = true;
                continue;
            }
            break;
        }
        if !in_moves {
            continue;
        }
        if tokens.len() < 2 {
            continue;
        }
        let Some(level) = parse_asm_u8(tokens[0]) else {
            continue;
        };
        let mov = tokens[1].to_string();
        out.push((level, mov));
    }

    out
}

fn pokemon_moves_at_level(
    species_const: &str,
    level: u8,
    pokemon_moves: &HashMap<String, PokemonMoves>,
) -> Vec<String> {
    let key = species_const_to_ron_key(species_const);
    let Some(moves) = pokemon_moves.get(&key) else {
        return vec![];
    };
    let mut learned: Vec<String> = Vec::new();
    for entry in &moves.learnset {
        if entry.level == 0 || entry.level > level {
            continue;
        }
        if entry.move_id == "NO_MOVE" {
            continue;
        }
        learned.push(entry.move_id.clone());
    }
    learned
        .into_iter()
        .rev()
        .take(4)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect()
}

fn pokemon_moves_learned_at_level(
    species_const: &str,
    level: u8,
    pokemon_moves: &HashMap<String, PokemonMoves>,
) -> Vec<String> {
    let key = species_const_to_ron_key(species_const);
    let Some(moves) = pokemon_moves.get(&key) else {
        return vec![];
    };
    let mut learned: Vec<String> = Vec::new();
    for entry in &moves.learnset {
        if entry.level == level && entry.move_id != "NO_MOVE" {
            learned.push(entry.move_id.clone());
        }
    }
    learned
}

fn pokemon_evolution_at_level(
    species_const: &str,
    level: u8,
    pokemon_moves: &HashMap<String, PokemonMoves>,
) -> Option<String> {
    let key = species_const_to_ron_key(species_const);
    let Some(moves) = pokemon_moves.get(&key) else {
        return None;
    };
    for evo in &moves.evolutions {
        if evo.method == "Level" && evo.level == Some(level) {
            return Some(evo.species.clone());
        }
    }
    None
}

fn trainer_party_for_opponent(
    trainer_parties: &HashMap<String, Vec<TrainerParty>>,
    opponent: &str,
    trainer_number: &str,
) -> Option<TrainerParty> {
    let class = opponent.strip_prefix("OPP_")?;
    let label = trainer_data_label_from_class_const(class);
    let parties = trainer_parties.get(&label)?;
    let idx = trainer_number.parse::<usize>().ok()?.saturating_sub(1);
    parties.get(idx).cloned()
}

fn trainer_data_label_from_class_const(class: &str) -> String {
    match class {
        "PSYCHIC_TR" => "Psychic".to_string(),
        _ => title_case_joined(class),
    }
}

fn title_case_joined(s: &str) -> String {
    let mut out = String::new();
    for part in s.split('_').filter(|p| !p.is_empty()) {
        out.push_str(&title_case(part));
    }
    out
}

fn title_case(s: &str) -> String {
    let lower = s.to_ascii_lowercase();
    let mut chars = lower.chars();
    let mut out = String::new();
    let Some(first) = chars.next() else {
        return out;
    };
    out.push(first.to_ascii_uppercase());
    out.extend(chars);
    out
}

fn parse_item_names(asm: &str) -> Vec<String> {
    let mut out: Vec<String> = vec![String::new()];
    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.starts_with("assert_list_length") {
            break;
        }
        if !line.starts_with("li ") {
            continue;
        }
        let rest = line.trim_start_matches("li").trim();
        let Some(s) = parse_asm_quoted_string(rest) else {
            continue;
        };
        out.push(s);
    }
    out
}

fn parse_item_constant_ids(asm: &str) -> HashMap<String, u8> {
    let mut out: HashMap<String, u8> = HashMap::new();
    let mut value: i32 = 0;
    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.is_empty() {
            continue;
        }

        if let Some(rest) = line.strip_prefix("const_def") {
            value = rest.trim().parse::<i32>().unwrap_or(0);
            continue;
        }

        if let Some(rest) = line.strip_prefix("const_next") {
            if let Some(v) = parse_asm_i32(rest.trim()) {
                value = v;
            }
            continue;
        }

        if !line.starts_with("const ") {
            continue;
        }
        let name = line.trim_start_matches("const").trim();
        if name.is_empty() {
            continue;
        }
        if !(0..=255).contains(&value) {
            value += 1;
            continue;
        }
        out.insert(name.to_string(), value as u8);
        value += 1;
    }
    out
}

fn parse_tmhm_display_names(asm: &str) -> HashMap<String, String> {
    let mut out: HashMap<String, String> = HashMap::new();
    let mut hm_num: u8 = 1;
    let mut tm_num: u8 = 1;
    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if let Some(rest) = line.strip_prefix("add_hm") {
            let mov = rest.trim().split_whitespace().next().unwrap_or("");
            if !mov.is_empty() {
                out.insert(format!("HM_{mov}"), format!("HM{hm_num:02}"));
                hm_num = hm_num.wrapping_add(1);
            }
            continue;
        }
        if let Some(rest) = line.strip_prefix("add_tm") {
            let mov = rest.trim().split_whitespace().next().unwrap_or("");
            if !mov.is_empty() {
                out.insert(format!("TM_{mov}"), format!("TM{tm_num:02}"));
                tm_num = tm_num.wrapping_add(1);
            }
            continue;
        }
    }
    out
}

fn parse_sprite_sheet_table(asm: &str) -> HashMap<String, String> {
    let mut out: HashMap<String, String> = HashMap::new();
    for raw_line in asm.lines() {
        let line = raw_line.trim();
        if !line.starts_with("overworld_sprite") {
            continue;
        }

        let (before_comment, comment) = raw_line.split_once(';').unwrap_or((raw_line, ""));
        let before_comment = before_comment.trim();
        if !before_comment.starts_with("overworld_sprite") {
            continue;
        }

        let args = before_comment.trim_start_matches("overworld_sprite").trim();
        let mut parts = args.split(',').map(|s| s.trim());
        let Some(label) = parts.next() else {
            continue;
        };
        if label.is_empty() {
            continue;
        }

        let Some(const_name) = comment.trim().split_whitespace().next() else {
            continue;
        };
        if const_name.is_empty() {
            continue;
        }

        out.insert(const_name.to_string(), label.to_string());
    }
    out
}

fn parse_gfx_sprite_png_paths(asm: &str) -> HashMap<String, PathBuf> {
    let mut out: HashMap<String, PathBuf> = HashMap::new();
    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if !line.contains("INCBIN") {
            continue;
        }

        let Some((label_part, rest)) = line.split_once("INCBIN") else {
            continue;
        };
        let label_part = label_part.trim();
        let label = label_part.strip_suffix("::").unwrap_or(label_part).trim();
        if label.is_empty() {
            continue;
        }

        let Some((_, after_open)) = rest.split_once('"') else {
            continue;
        };
        let Some((path_str, _)) = after_open.split_once('"') else {
            continue;
        };
        let path_str = path_str.trim();
        if path_str.is_empty() {
            continue;
        }

        let png_path = if let Some(base) = path_str.strip_suffix(".2bpp") {
            format!("{base}.png")
        } else {
            continue;
        };
        out.insert(label.to_string(), PathBuf::from(png_path));
    }
    out
}

fn load_object_sprite_sheets(
    object_events: &[ObjectEvent],
    sprite_id_to_png: &HashMap<u8, PathBuf>,
) -> Result<HashMap<u8, GrayscaleImage>, Box<dyn Error>> {
    let unique_ids: HashSet<u8> = object_events.iter().map(|o| o.sprite_id).collect();
    let mut out: HashMap<u8, GrayscaleImage> = HashMap::new();
    for sprite_id in unique_ids {
        if sprite_id == 0 {
            continue;
        }
        let png_path = sprite_id_to_png.get(&sprite_id).ok_or_else(|| {
            format!("Missing sprite PNG mapping for sprite id `{sprite_id:#04x}`")
        })?;
        let img = load_grayscale_png(png_path)?;
        out.insert(sprite_id, img);
    }
    Ok(out)
}

fn parse_tileset_asset_bases(tilesets_asm: &str) -> HashMap<String, String> {
    let mut bases = HashMap::new();
    let mut pending_labels: Vec<String> = Vec::new();

    for raw_line in tilesets_asm.lines() {
        let line = strip_asm_comment(raw_line);
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        if let Some(idx) = line.find("_GFX::") {
            let label = line[..idx].trim();
            if !label.is_empty() {
                pending_labels.push(label.to_string());
            }

            if let Some(base) = parse_incbin_tileset_base(line) {
                for label in pending_labels.drain(..) {
                    bases.insert(label, base.clone());
                }
            }

            continue;
        }

        if pending_labels.is_empty() {
            continue;
        }

        if let Some(base) = parse_incbin_tileset_base(line) {
            for label in pending_labels.drain(..) {
                bases.insert(label, base.clone());
            }
        }
    }

    bases
}

fn tileset_symbol_to_label(symbol: &str) -> String {
    let mut out = String::new();
    for part in symbol.split('_') {
        if part.chars().all(|c| c.is_ascii_digit()) {
            out.push_str(part);
        } else {
            out.push_str(&titlecase_ascii(part));
        }
    }
    out
}

fn titlecase_ascii(s: &str) -> String {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return String::new();
    };

    let mut out = String::new();
    out.push(first.to_ascii_uppercase());
    for c in chars {
        out.push(c.to_ascii_lowercase());
    }
    out
}

fn parse_incbin_tileset_base(line: &str) -> Option<String> {
    let needle = "INCBIN \"gfx/tilesets/";
    let start = line.find(needle)? + needle.len();
    let rest = &line[start..];
    let end = rest.find(".2bpp")?;
    Some(rest[..end].to_string())
}

fn strip_asm_comment(line: &str) -> &str {
    line.split(';').next().unwrap_or("")
}

fn is_zero_u8(v: &u8) -> bool {
    *v == 0
}

fn parse_tileset_grass_tile_id(asm: &str, tileset_label: &str) -> Option<Option<u8>> {
    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if !line.starts_with("tileset ") {
            continue;
        }
        let args = line.trim_start_matches("tileset").trim();
        let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
        if parts.len() < 5 {
            continue;
        }
        if parts[0] != tileset_label {
            continue;
        }
        let grass = parts[4];
        if grass == "-1" {
            return Some(None);
        }
        return Some(Some(parse_asm_u8(grass)?));
    }
    None
}

/// Returns the counter tiles for a tileset that allow talking across counters.
/// From tileset_headers.asm - format: tileset Name, counter1, counter2, counter3, grass, anim
fn get_counter_tiles_for_tileset(tileset_label: &str) -> Vec<u8> {
    // Counter tiles from pokered/data/tilesets/tileset_headers.asm
    // These tiles allow talking to NPCs 2 tiles away (across the counter)
    match tileset_label {
        "Pokecenter" | "Mart" | "POKECENTER" | "MART" => vec![0x18, 0x19, 0x1E],
        "Dojo" | "Gym" | "DOJO" | "GYM" => vec![0x3A],
        "ForestGate" | "Museum" | "Gate" | "FOREST_GATE" | "MUSEUM" | "GATE" => vec![0x17, 0x32],
        "Cemetery" | "Facility" | "CEMETERY" | "FACILITY" => vec![0x12],
        "Lobby" | "LOBBY" => vec![0x15, 0x36],
        "Club" | "CLUB" => vec![0x07, 0x17],
        _ => vec![],
    }
}

fn parse_passable_tiles_for_label(asm: &str, label: &str) -> Option<HashSet<u8>> {
    let mut pending_labels: Vec<String> = Vec::new();
    for raw_line in asm.lines() {
        let line = strip_asm_comment(raw_line).trim();
        if line.is_empty() {
            continue;
        }

        if let Some(l) = line.strip_suffix("::") {
            if l.ends_with("_Coll") {
                pending_labels.push(l.to_string());
            }
            continue;
        }

        if !line.starts_with("coll_tiles") {
            continue;
        }

        let is_target = pending_labels.iter().any(|l| l == label);
        let args = line.trim_start_matches("coll_tiles").trim();
        let mut set = HashSet::new();
        if !args.is_empty() {
            for token in args.split(',') {
                let token = token.trim();
                if token.is_empty() {
                    continue;
                }
                if let Some(hex) = token.strip_prefix('$') {
                    if let Ok(v) = u8::from_str_radix(hex, 16) {
                        set.insert(v);
                    }
                } else if let Ok(v) = token.parse::<u8>() {
                    set.insert(v);
                }
            }
        }

        pending_labels.clear();

        if is_target {
            return Some(set);
        }
    }
    None
}

fn find_spawn_position(
    width_blocks: u32,
    height_blocks: u32,
    blocks: &[u8],
    blockset: &Blockset,
    passable_tiles: &HashSet<u8>,
) -> Option<(i32, i32)> {
    let map_w_tiles = (width_blocks * BLOCK_TILES) as i32;
    let map_h_tiles = (height_blocks * BLOCK_TILES) as i32;

    let mut center_tx = ((map_w_tiles - PLAYER_W_TILES) / 2).max(0);
    let mut center_ty = ((map_h_tiles - PLAYER_H_TILES) / 2).max(0);
    center_tx = (center_tx / PLAYER_STEP_TILES) * PLAYER_STEP_TILES;
    center_ty = (center_ty / PLAYER_STEP_TILES) * PLAYER_STEP_TILES;

    let max_r = (map_w_tiles.max(map_h_tiles) / PLAYER_STEP_TILES) + 1;
    for r in 0..=max_r {
        let r = r as i32;
        for dy in -r..=r {
            for dx in -r..=r {
                if dx.abs() != r && dy.abs() != r {
                    continue;
                }
                let tx = center_tx + dx * PLAYER_STEP_TILES;
                let ty = center_ty + dy * PLAYER_STEP_TILES;
                if tx < 0
                    || ty < 0
                    || tx + PLAYER_W_TILES > map_w_tiles
                    || ty + PLAYER_H_TILES > map_h_tiles
                {
                    continue;
                }
                if is_area_passable(
                    width_blocks,
                    height_blocks,
                    blocks,
                    blockset,
                    passable_tiles,
                    tx,
                    ty,
                ) {
                    return Some((tx, ty));
                }
            }
        }
    }

    for ty in (0..map_h_tiles).step_by(PLAYER_STEP_TILES as usize) {
        for tx in (0..map_w_tiles).step_by(PLAYER_STEP_TILES as usize) {
            if is_area_passable(
                width_blocks,
                height_blocks,
                blocks,
                blockset,
                passable_tiles,
                tx,
                ty,
            ) {
                return Some((tx, ty));
            }
        }
    }

    None
}

fn tick_map(
    view: &mut MapView,
    input: &InputState,
    pokered_root: &Path,
    game_data: &game_data::GameData,
) -> Result<(), Box<dyn Error>> {
    if try_run_pending_map_sequences(view, pokered_root, game_data)? {
        return Ok(());
    }

    let old_tx = view.player.tx;
    let old_ty = view.player.ty;

    view.overworld_subtick ^= 1;
    let do_overworld_tick = view.overworld_subtick == 0;
    tick_player(view, input, do_overworld_tick);
    tick_object_events(view, do_overworld_tick);

    let moved =
        view.player.move_anim.is_none() && (view.player.tx != old_tx || view.player.ty != old_ty);
    if moved {
        if try_warp_transition(view, pokered_root, game_data)? {
            return Ok(());
        }
        if try_connection_transition(view, pokered_root, game_data)? {
            return Ok(());
        }
        if try_trigger_pallet_town_oak_escort(view, pokered_root)? {
            return Ok(());
        }
        if try_trigger_route22_rival1_battle(view, pokered_root)? {
            return Ok(());
        }
        if try_trigger_oaks_lab_rival_battle(view, pokered_root)? {
            return Ok(());
        }
        try_trigger_wild_encounter(view, &game_data.wild_encounters);
    }

    if view.player.move_anim.is_none() {
        update_camera_follow(view);
    }
    Ok(())
}

fn try_run_pending_map_sequences(
    view: &mut MapView,
    pokered_root: &Path,
    game_data: &game_data::GameData,
) -> Result<bool, Box<dyn Error>> {
    if view.player.move_anim.is_some() {
        return Ok(false);
    }

    if view.events.contains("EVENT_OAK_ESCORT_TO_LAB_PENDING") {
        let mut new_view = load_map_view(
            pokered_root,
            game_data,
            "OaksLab",
            Some("PalletTown".to_string()),
            &view.picked_up_items,
            &view.events,
        )?;
        place_player_on_warp_id(&mut new_view, 2)?;
        new_view.player.facing = Facing::Up;
        recenter_camera_on_player(
            &new_view.map,
            &mut new_view.camera_tx,
            &mut new_view.camera_ty,
            new_view.player.tx,
            new_view.player.ty,
        );

        new_view.rng = view.rng;
        new_view.money = view.money;
        new_view.events = view.events.clone();
        new_view.events.remove("EVENT_OAK_ESCORT_TO_LAB_PENDING");
        new_view
            .events
            .insert("EVENT_FOLLOWED_OAK_INTO_LAB".to_string());
        new_view.inventory = view.inventory.clone();
        new_view.picked_up_items = view.picked_up_items.clone();
        new_view.defeated_trainers = view.defeated_trainers.clone();
        new_view.party = view.party.clone();
        new_view.pc_boxes = view.pc_boxes.clone();
        new_view.pc_current_box = view.pc_current_box;
        *view = new_view;
        return Ok(true);
    }

    if view.map_name == "OaksLab" && view.events.contains("EVENT_OAK_POKEDEX_SEQUENCE_PENDING") {
        view.events.remove("EVENT_OAK_POKEDEX_SEQUENCE_PENDING");
        view.events.insert("EVENT_GOT_POKEDEX".to_string());
        view.events
            .insert("EVENT_1ST_ROUTE22_RIVAL_BATTLE".to_string());
        view.events
            .insert("EVENT_ROUTE22_RIVAL_WANTS_BATTLE".to_string());

        let lines = vec![
            "OAK: I have a request".to_string(),
            "of you two.".to_string(),
            String::new(),
            "On the desk there is my".to_string(),
            "invention, POKéDEX!".to_string(),
            String::new(),
            "It automatically records".to_string(),
            "data on POKéMON you've".to_string(),
            "seen or caught.".to_string(),
            String::new(),
            "Take this POKéDEX!".to_string(),
        ];
        open_dialog_from_lines(view, lines);
        return Ok(true);
    }

    Ok(false)
}

fn try_trigger_wild_encounter(
    view: &mut MapView,
    wild_encounters: &HashMap<String, WildEncounterTable>,
) {
    if view.pending_battle.is_some() {
        return;
    }
    let Some(table) = wild_encounters.get(&view.map_name) else {
        return;
    };
    if table.grass_rate == 0 || table.grass.is_empty() {
        return;
    }

    let tile_tx = view.player.tx;
    let tile_ty = view.player.ty + (PLAYER_H_TILES - 1);
    let Some(tile_id) = map_tile_id(
        view.map.width_blocks,
        view.map.height_blocks,
        &view.map.blocks,
        &view.blockset,
        tile_tx,
        tile_ty,
    ) else {
        return;
    };

    if let Some(grass_tile_id) = view.tileset.grass_tile_id {
        if tile_id != grass_tile_id {
            return;
        }
    }

    let roll = next_rand_u8(&mut view.rng);
    if roll >= table.grass_rate {
        return;
    }

    view.pending_battle = Some(PendingBattle::Wild {
        map_name: view.map_name.clone(),
    });
}

fn try_trigger_pallet_town_oak_escort(
    view: &mut MapView,
    pokered_root: &Path,
) -> Result<bool, Box<dyn Error>> {
    if view.map_name != "PalletTown" {
        return Ok(false);
    }
    if view.events.contains("EVENT_FOLLOWED_OAK_INTO_LAB")
        || view.events.contains("EVENT_OAK_ESCORT_TO_LAB_PENDING")
    {
        return Ok(false);
    }
    let (_, y) = player_coords_units(&view.player);
    if y != 1 {
        return Ok(false);
    }

    let lines = vec![
        "OAK: Hey! Wait!".to_string(),
        "Don't go out!".to_string(),
        String::new(),
        "It's unsafe! Wild POKéMON".to_string(),
        "live in tall grass!".to_string(),
    ];
    open_dialog_from_lines(view, lines);
    view.events
        .insert("EVENT_OAK_ESCORT_TO_LAB_PENDING".to_string());
    Ok(true)
}

fn try_trigger_route22_rival1_battle(
    view: &mut MapView,
    pokered_root: &Path,
) -> Result<bool, Box<dyn Error>> {
    if view.map_name != "Route22" {
        return Ok(false);
    }
    if view.pending_battle.is_some()
        || view.dialog.is_some()
        || view.menu.is_some()
        || view.shop.is_some()
    {
        return Ok(false);
    }
    if view.events.contains("EVENT_GOT_POKEDEX")
        && !view.events.contains("EVENT_BEAT_ROUTE22_RIVAL_1ST_BATTLE")
    {
        view.events
            .insert("EVENT_1ST_ROUTE22_RIVAL_BATTLE".to_string());
        view.events
            .insert("EVENT_ROUTE22_RIVAL_WANTS_BATTLE".to_string());
    }
    if !view.events.contains("EVENT_ROUTE22_RIVAL_WANTS_BATTLE")
        || view.events.contains("EVENT_BEAT_ROUTE22_RIVAL_1ST_BATTLE")
    {
        return Ok(false);
    }

    let (x, y) = player_coords_units(&view.player);
    if x != 29 || (y != 4 && y != 5) {
        return Ok(false);
    }

    let trainer_number = route22_rival1_trainer_number(view).unwrap_or_else(|| "4".to_string());

    let lines = vec!["RIVAL: Hey PLAYER!".to_string(), "Let's battle!".to_string()];

    open_dialog_from_lines(view, lines);
    view.pending_battle = Some(PendingBattle::Trainer {
        map_name: view.map_name.clone(),
        text_id: "TEXT_ROUTE22_RIVAL1".to_string(),
        opponent: "OPP_RIVAL1".to_string(),
        trainer_number,
    });
    Ok(true)
}

fn try_trigger_oaks_lab_rival_battle(
    view: &mut MapView,
    pokered_root: &Path,
) -> Result<bool, Box<dyn Error>> {
    if view.map_name != "OaksLab" {
        return Ok(false);
    }
    if view.pending_battle.is_some()
        || view.dialog.is_some()
        || view.menu.is_some()
        || view.shop.is_some()
    {
        return Ok(false);
    }
    if !view.events.contains("EVENT_GOT_STARTER") {
        return Ok(false);
    }
    if view.events.contains("EVENT_BATTLED_RIVAL_IN_OAKS_LAB") {
        return Ok(false);
    }
    if view
        .defeated_trainers
        .contains(&(view.map_name.clone(), "TEXT_OAKSLAB_RIVAL".to_string()))
    {
        view.events
            .insert("EVENT_BATTLED_RIVAL_IN_OAKS_LAB".to_string());
        return Ok(false);
    }

    let (_, y) = player_coords_units(&view.player);
    if y != 6 {
        return Ok(false);
    }

    let trainer_number = oaks_lab_rival_trainer_number(view).unwrap_or_else(|| "1".to_string());

    let lines = vec![
        "RIVAL: Wait PLAYER!".to_string(),
        "Come on, I'll take you on!".to_string(),
    ];

    open_dialog_from_lines(view, lines);
    view.pending_battle = Some(PendingBattle::Trainer {
        map_name: view.map_name.clone(),
        text_id: "TEXT_OAKSLAB_RIVAL".to_string(),
        opponent: "OPP_RIVAL1".to_string(),
        trainer_number,
    });
    Ok(true)
}

fn tick_player(view: &mut MapView, input: &InputState, do_overworld_tick: bool) {
    if let Some(m) = &mut view.player.move_anim {
        if do_overworld_tick {
            view.player.walk_intra_counter = view.player.walk_intra_counter.wrapping_add(1);
            if view.player.walk_intra_counter >= 4 {
                view.player.walk_intra_counter = 0;
                view.player.walk_anim_frame = (view.player.walk_anim_frame.wrapping_add(1)) & 3;
            }

            m.progress_px = (m.progress_px + PLAYER_SPEED_PX_PER_TICK).min(PLAYER_STEP_PX);
            if m.progress_px >= PLAYER_STEP_PX {
                view.player.tx = m.dest_tx;
                view.player.ty = m.dest_ty;
                view.player.move_anim = None;
            }
        }
        return;
    }

    let Some(dir) = input.direction() else {
        view.player.walk_intra_counter = 0;
        view.player.walk_anim_frame = 0;
        return;
    };

    view.player.facing = dir;
    if do_overworld_tick {
        try_start_player_move(view, dir);
    }
}

fn tick_object_events(view: &mut MapView, do_overworld_tick: bool) {
    if !do_overworld_tick {
        return;
    }

    for i in 0..view.object_events.len() {
        let mut start_move_dir: Option<Facing> = None;

        {
            let obj = &mut view.object_events[i];
            if let Some(m) = &mut obj.move_anim {
                obj.walk_intra_counter = obj.walk_intra_counter.wrapping_add(1);
                if obj.walk_intra_counter >= 4 {
                    obj.walk_intra_counter = 0;
                    obj.walk_anim_frame = (obj.walk_anim_frame.wrapping_add(1)) & 3;
                }

                m.progress_px = (m.progress_px + PLAYER_SPEED_PX_PER_TICK).min(PLAYER_STEP_PX);
                if m.progress_px >= PLAYER_STEP_PX {
                    obj.tx = m.dest_tx;
                    obj.ty = m.dest_ty;
                    obj.move_anim = None;
                    obj.walk_intra_counter = 0;
                    obj.walk_anim_frame = 0;
                }
                continue;
            }

            if obj.movement != ObjectMovement::Walk {
                continue;
            }

            if obj.move_delay > 0 {
                obj.move_delay -= 1;
                continue;
            }

            let range = obj.movement_range;
            let dir = choose_object_move_dir(range, &mut view.rng);
            obj.facing = dir;

            // 25% chance: just turn this time.
            if (next_rand_u8(&mut view.rng) & 3) != 0 {
                start_move_dir = Some(dir);
            }

            obj.move_delay = next_object_move_delay(&mut view.rng);
        }

        if let Some(dir) = start_move_dir {
            try_start_object_move(view, i, dir);
        }
    }
}

fn try_start_object_move(view: &mut MapView, obj_i: usize, dir: Facing) {
    if view.object_events[obj_i].move_anim.is_some() {
        return;
    }

    let (dx, dy) = facing_to_dir_tiles(dir);
    let dest_tx = view.object_events[obj_i].tx + dx;
    let dest_ty = view.object_events[obj_i].ty + dy;

    if tile_is_blocked(view, Some(obj_i), dest_tx, dest_ty) {
        return;
    }

    let can_walk = is_area_passable(
        view.map.width_blocks,
        view.map.height_blocks,
        &view.map.blocks,
        &view.blockset,
        &view.passable_tiles,
        dest_tx,
        dest_ty,
    );
    if !can_walk {
        return;
    }

    view.object_events[obj_i].move_anim = Some(MoveAnim {
        dir,
        start_tx: view.object_events[obj_i].tx,
        start_ty: view.object_events[obj_i].ty,
        dest_tx,
        dest_ty,
        progress_px: PLAYER_SPEED_PX_PER_TICK,
    });
}

fn tile_is_blocked(view: &MapView, ignore_object_i: Option<usize>, tx: i32, ty: i32) -> bool {
    let (map_w_tiles, map_h_tiles) = map_dimensions_tiles(&view.map);
    if tx < 0 || ty < 0 || tx + PLAYER_W_TILES > map_w_tiles || ty + PLAYER_H_TILES > map_h_tiles {
        return true;
    }

    if player_blocks_tile(&view.player, tx, ty) {
        return true;
    }

    for (i, obj) in view.object_events.iter().enumerate() {
        if ignore_object_i == Some(i) {
            continue;
        }
        if object_blocks_tile(obj, tx, ty) {
            return true;
        }
    }
    false
}

fn player_blocks_tile(player: &Player, tx: i32, ty: i32) -> bool {
    if let Some(m) = &player.move_anim {
        (m.start_tx == tx && m.start_ty == ty) || (m.dest_tx == tx && m.dest_ty == ty)
    } else {
        player.tx == tx && player.ty == ty
    }
}

fn object_blocks_tile(obj: &ObjectEvent, tx: i32, ty: i32) -> bool {
    if let Some(m) = &obj.move_anim {
        (m.start_tx == tx && m.start_ty == ty) || (m.dest_tx == tx && m.dest_ty == ty)
    } else {
        obj.tx == tx && obj.ty == ty
    }
}

/// Get the tile ID at a given tile coordinate.
/// Returns None if the coordinates are out of bounds.
fn get_tile_id_at(map: &MapData, blockset: &Blockset, tx: i32, ty: i32) -> Option<u8> {
    if tx < 0 || ty < 0 {
        return None;
    }
    let tx = tx as u32;
    let ty = ty as u32;
    let map_w_tiles = map.width_blocks * BLOCK_TILES;
    let map_h_tiles = map.height_blocks * BLOCK_TILES;
    if tx >= map_w_tiles || ty >= map_h_tiles {
        return None;
    }
    let block_x = tx / BLOCK_TILES;
    let block_y = ty / BLOCK_TILES;
    let block_i = (block_y * map.width_blocks + block_x) as usize;
    if block_i >= map.blocks.len() {
        return None;
    }
    let block_id = map.blocks[block_i] as usize;
    if block_id >= blockset.blocks.len() {
        return None;
    }
    let block = &blockset.blocks[block_id];
    let tile_x = tx % BLOCK_TILES;
    let tile_y = ty % BLOCK_TILES;
    let tile_i = (tile_y * BLOCK_TILES + tile_x) as usize;
    Some(block[tile_i])
}

/// Check if the tile at the given position is a counter tile for the current tileset.
fn is_counter_tile(view: &MapView, tx: i32, ty: i32) -> bool {
    if view.tileset.counter_tiles.is_empty() {
        return false;
    }
    if let Some(tile_id) = get_tile_id_at(&view.map, &view.blockset, tx, ty) {
        view.tileset.counter_tiles.contains(&tile_id)
    } else {
        false
    }
}

/// Check if there's a counter tile anywhere between start and end positions (exclusive of start).
/// This handles the case where counter tiles are at odd offsets that the player can't target directly.
fn has_counter_between(view: &MapView, start_tx: i32, start_ty: i32, end_tx: i32, end_ty: i32) -> bool {
    if view.tileset.counter_tiles.is_empty() {
        return false;
    }
    // Determine step direction
    let dx = (end_tx - start_tx).signum();
    let dy = (end_ty - start_ty).signum();
    // Check all tiles from start (exclusive) to end (exclusive)
    let mut tx = start_tx + dx;
    let mut ty = start_ty + dy;
    while tx != end_tx || ty != end_ty {
        if is_counter_tile(view, tx, ty) {
            return true;
        }
        tx += dx;
        ty += dy;
    }
    false
}

fn choose_object_move_dir(range: ObjectMovementRange, rng: &mut u32) -> Facing {
    match range {
        ObjectMovementRange::UpDown => {
            if (next_rand_u8(rng) & 1) == 0 {
                Facing::Up
            } else {
                Facing::Down
            }
        }
        ObjectMovementRange::LeftRight => {
            if (next_rand_u8(rng) & 1) == 0 {
                Facing::Left
            } else {
                Facing::Right
            }
        }
        _ => match next_rand_u8(rng) & 3 {
            0 => Facing::Up,
            1 => Facing::Down,
            2 => Facing::Left,
            _ => Facing::Right,
        },
    }
}

fn next_object_move_delay(rng: &mut u32) -> u8 {
    24 + (next_rand_u8(rng) % 48)
}

fn next_rand_u8(rng: &mut u32) -> u8 {
    *rng = rng.wrapping_mul(1103515245).wrapping_add(12345);
    (*rng >> 16) as u8
}

fn save_file_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("savegame.txt")
}

fn facing_to_save_str(facing: Facing) -> &'static str {
    match facing {
        Facing::Down => "Down",
        Facing::Up => "Up",
        Facing::Left => "Left",
        Facing::Right => "Right",
    }
}

fn parse_save_facing(s: &str) -> Option<Facing> {
    match s {
        "Down" => Some(Facing::Down),
        "Up" => Some(Facing::Up),
        "Left" => Some(Facing::Left),
        "Right" => Some(Facing::Right),
        _ => None,
    }
}

fn inventory_item_ids_sorted(view: &MapView) -> Vec<String> {
    inventory_item_ids_sorted_map(&view.inventory)
}

fn inventory_item_ids_sorted_map(inventory: &HashMap<String, u32>) -> Vec<String> {
    let mut ids: Vec<String> = inventory.keys().cloned().collect();
    ids.sort();
    ids
}

fn save_game(view: &MapView) -> Result<(), Box<dyn Error>> {
    let mut out = String::new();
    out.push_str("VERSION 6\n");
    out.push_str(&format!("MAP {}\n", view.map_name));
    match &view.last_outside_map {
        Some(m) => out.push_str(&format!("LAST_OUTSIDE {}\n", m)),
        None => out.push_str("LAST_OUTSIDE -\n"),
    }
    out.push_str(&format!(
        "PLAYER {} {} {}\n",
        view.player.tx,
        view.player.ty,
        facing_to_save_str(view.player.facing)
    ));
    out.push_str(&format!("RNG {}\n", view.rng));
    out.push_str(&format!("MONEY {}\n", view.money));

    let mut events: Vec<String> = view.events.iter().cloned().collect();
    events.sort();
    for ev in events {
        out.push_str(&format!("EVENT {}\n", ev));
    }

    for item_id in inventory_item_ids_sorted(view) {
        let count = view.inventory.get(&item_id).copied().unwrap_or(0);
        out.push_str(&format!("INVENTORY {} {}\n", item_id, count));
    }

    for item_id in inventory_item_ids_sorted_map(&view.pc_items) {
        let count = view.pc_items.get(&item_id).copied().unwrap_or(0);
        out.push_str(&format!("PC_ITEM {} {}\n", item_id, count));
    }

    let mut picked: Vec<(String, String)> = view.picked_up_items.iter().cloned().collect();
    picked.sort();
    for (map_name, text_id) in picked {
        out.push_str(&format!("PICKED {} {}\n", map_name, text_id));
    }

    let mut defeated: Vec<(String, String)> = view.defeated_trainers.iter().cloned().collect();
    defeated.sort();
    for (map_name, text_id) in defeated {
        out.push_str(&format!("DEFEATED {} {}\n", map_name, text_id));
    }

    for (i, mon) in view.party.iter().enumerate() {
        out.push_str(&format!(
            "PARTY {} {} {} {} {} {}",
            i, mon.species, mon.level, mon.hp, mon.max_hp, mon.exp
        ));
        for mov in mon.moves.iter().take(4) {
            out.push(' ');
            out.push_str(mov);
        }
        out.push('\n');
    }

    out.push_str(&format!("PC_CURBOX {}\n", view.pc_current_box));
    for (box_i, box_mons) in view.pc_boxes.iter().enumerate() {
        for (slot_i, mon) in box_mons.iter().enumerate() {
            out.push_str(&format!(
                "BOX {} {} {} {} {} {} {}",
                box_i, slot_i, mon.species, mon.level, mon.hp, mon.max_hp, mon.exp
            ));
            for mov in mon.moves.iter().take(4) {
                out.push(' ');
                out.push_str(mov);
            }
            out.push('\n');
        }
    }

    out.push_str("END\n");
    fs::write(save_file_path(), out)?;
    Ok(())
}

fn load_game(
    pokered_root: &Path,
    game_data: &game_data::GameData,
) -> Result<MapView, Box<dyn Error>> {
    let path = save_file_path();
    let text = fs::read_to_string(&path)?;

    let mut version: Option<u32> = None;
    let mut map_name: Option<String> = None;
    let mut last_outside_map: Option<Option<String>> = None;
    let mut player_tx: Option<i32> = None;
    let mut player_ty: Option<i32> = None;
    let mut facing: Option<Facing> = None;
    let mut rng: Option<u32> = None;
    let mut money: Option<u32> = None;
    let mut events: HashSet<String> = HashSet::new();
    let mut inventory: HashMap<String, u32> = HashMap::new();
    let mut pc_items: HashMap<String, u32> = HashMap::new();
    let mut picked_up_items: HashSet<(String, String)> = HashSet::new();
    let mut defeated_trainers: HashSet<(String, String)> = HashSet::new();
    let mut party_entries: Vec<(usize, PlayerMon)> = Vec::new();
    let mut pc_current_box: Option<usize> = None;
    let mut pc_box_entries: Vec<(usize, usize, PlayerMon)> = Vec::new();

    for (line_no, raw_line) in text.lines().enumerate() {
        let line = raw_line.trim();
        if line.is_empty() {
            continue;
        }
        let parts: Vec<&str> = line.split_whitespace().collect();
        let Some(tag) = parts.first().copied() else {
            continue;
        };

        match tag {
            "VERSION" => {
                version = parts.get(1).and_then(|s| s.parse::<u32>().ok());
            }
            "MAP" => {
                map_name = parts.get(1).map(|s| (*s).to_string());
            }
            "LAST_OUTSIDE" => {
                last_outside_map = match parts.get(1) {
                    Some(&"-") | None => Some(None),
                    Some(v) => Some(Some((*v).to_string())),
                };
            }
            "PLAYER" => {
                let tx = parts.get(1).and_then(|s| s.parse::<i32>().ok());
                let ty = parts.get(2).and_then(|s| s.parse::<i32>().ok());
                let f = parts.get(3).and_then(|s| parse_save_facing(s));
                player_tx = tx;
                player_ty = ty;
                facing = f;
            }
            "RNG" => {
                rng = parts.get(1).and_then(|s| s.parse::<u32>().ok());
            }
            "MONEY" => {
                money = parts.get(1).and_then(|s| s.parse::<u32>().ok());
            }
            "EVENT" => {
                let Some(ev) = parts.get(1) else {
                    continue;
                };
                events.insert((*ev).to_string());
            }
            "INVENTORY" => {
                let Some(item_id) = parts.get(1) else {
                    continue;
                };
                let Some(count) = parts.get(2).and_then(|s| s.parse::<u32>().ok()) else {
                    continue;
                };
                inventory.insert((*item_id).to_string(), count);
            }
            "PC_ITEM" => {
                let Some(item_id) = parts.get(1) else {
                    continue;
                };
                let Some(count) = parts.get(2).and_then(|s| s.parse::<u32>().ok()) else {
                    continue;
                };
                pc_items.insert((*item_id).to_string(), count);
            }
            "PICKED" => {
                let Some(map) = parts.get(1) else {
                    continue;
                };
                let Some(text_id) = parts.get(2) else {
                    continue;
                };
                picked_up_items.insert(((*map).to_string(), (*text_id).to_string()));
            }
            "DEFEATED" => {
                let Some(map) = parts.get(1) else {
                    continue;
                };
                let Some(text_id) = parts.get(2) else {
                    continue;
                };
                defeated_trainers.insert(((*map).to_string(), (*text_id).to_string()));
            }
            "PARTY" => {
                if parts.len() < 6 {
                    continue;
                }
                let Some(slot) = parts.get(1).and_then(|s| s.parse::<usize>().ok()) else {
                    continue;
                };
                let Some(species) = parts.get(2).map(|s| (*s).to_string()) else {
                    continue;
                };
                let Some(level) = parts.get(3).and_then(|s| s.parse::<u8>().ok()) else {
                    continue;
                };
                let Some(hp) = parts.get(4).and_then(|s| s.parse::<u16>().ok()) else {
                    continue;
                };
                let Some(max_hp) = parts.get(5).and_then(|s| s.parse::<u16>().ok()) else {
                    continue;
                };
                let mut move_start = 6usize;
                let mut exp: Option<u32> = None;
                if let Some(tok) = parts.get(6) {
                    if let Ok(v) = tok.parse::<u32>() {
                        exp = Some(v);
                        move_start = 7;
                    }
                }
                let mut moves: Vec<String> = Vec::new();
                for mov in parts.iter().skip(move_start) {
                    if *mov == "-" {
                        continue;
                    }
                    moves.push((*mov).to_string());
                }
                let exp = match exp {
                    Some(v) => v,
                    None => {
                        let base = base_stats_for_species(&game_data.pokemon_stats, &species)?;
                        exp_for_level(base.growth_rate, level)
                    }
                };
                party_entries.push((
                    slot,
                    PlayerMon {
                        species,
                        level,
                        hp: hp.min(max_hp),
                        max_hp,
                        exp,
                        moves,
                    },
                ));
            }
            "PC_CURBOX" => {
                pc_current_box = parts.get(1).and_then(|s| s.parse::<usize>().ok());
            }
            "BOX" => {
                if parts.len() < 7 {
                    continue;
                }
                let Some(box_i) = parts.get(1).and_then(|s| s.parse::<usize>().ok()) else {
                    continue;
                };
                let Some(slot_i) = parts.get(2).and_then(|s| s.parse::<usize>().ok()) else {
                    continue;
                };
                let Some(species) = parts.get(3).map(|s| (*s).to_string()) else {
                    continue;
                };
                let Some(level) = parts.get(4).and_then(|s| s.parse::<u8>().ok()) else {
                    continue;
                };
                let Some(hp) = parts.get(5).and_then(|s| s.parse::<u16>().ok()) else {
                    continue;
                };
                let Some(max_hp) = parts.get(6).and_then(|s| s.parse::<u16>().ok()) else {
                    continue;
                };

                let mut move_start = 7usize;
                let mut exp: Option<u32> = None;
                if let Some(tok) = parts.get(7) {
                    if let Ok(v) = tok.parse::<u32>() {
                        exp = Some(v);
                        move_start = 8;
                    }
                }
                let mut moves: Vec<String> = Vec::new();
                for mov in parts.iter().skip(move_start) {
                    if *mov == "-" {
                        continue;
                    }
                    moves.push((*mov).to_string());
                }
                let exp = match exp {
                    Some(v) => v,
                    None => {
                        let base = base_stats_for_species(&game_data.pokemon_stats, &species)?;
                        exp_for_level(base.growth_rate, level)
                    }
                };
                pc_box_entries.push((
                    box_i,
                    slot_i,
                    PlayerMon {
                        species,
                        level,
                        hp: hp.min(max_hp),
                        max_hp,
                        exp,
                        moves,
                    },
                ));
            }
            "END" => break,
            _ => {
                let _ = line_no;
                continue;
            }
        }
    }

    let version = version.ok_or_else(|| format!("Save `{path:?}` missing VERSION"))?;
    if version != 1 && version != 2 && version != 3 && version != 4 && version != 5 && version != 6
    {
        return Err(format!("Unsupported save version `{version}`").into());
    }
    let map_name = map_name.ok_or_else(|| format!("Save `{path:?}` missing MAP"))?;
    let last_outside_map = last_outside_map.unwrap_or(None);
    let player_tx = player_tx.ok_or_else(|| format!("Save `{path:?}` missing PLAYER tx"))?;
    let player_ty = player_ty.ok_or_else(|| format!("Save `{path:?}` missing PLAYER ty"))?;
    let facing = facing.ok_or_else(|| format!("Save `{path:?}` missing PLAYER facing"))?;
    let rng = rng.unwrap_or(0);

    if player_tx % PLAYER_STEP_TILES != 0 || player_ty % PLAYER_STEP_TILES != 0 {
        return Err(format!("Save `{path:?}` has misaligned player coords").into());
    }

    let mut view = load_map_view(
        pokered_root,
        game_data,
        &map_name,
        last_outside_map,
        &picked_up_items,
        &events,
    )?;
    view.picked_up_items = picked_up_items;
    view.inventory = inventory;
    view.pc_items = pc_items;
    view.defeated_trainers = defeated_trainers;
    if !party_entries.is_empty() {
        party_entries.sort_by_key(|(i, _)| *i);
        view.party = party_entries.into_iter().map(|(_, m)| m).take(6).collect();
    }
    view.rng = rng;
    view.money = money.unwrap_or(3000);
    view.events = events;
    view.player.tx = player_tx;
    view.player.ty = player_ty;
    view.player.facing = facing;
    view.player.walk_intra_counter = 0;
    view.player.walk_anim_frame = 0;
    view.player.move_anim = None;
    view.overworld_subtick = 0;
    view.dialog = None;
    view.menu = None;
    view.shop = None;
    view.choice = None;
    view.pending_battle = None;
    view.pc = None;

    let mut pc_boxes: Vec<Vec<PlayerMon>> = vec![Vec::new(); 12];
    if !pc_box_entries.is_empty() {
        pc_box_entries.sort_by_key(|(b, i, _)| (*b, *i));
        for (box_i, _slot_i, mon) in pc_box_entries {
            if let Some(dst) = pc_boxes.get_mut(box_i) {
                if dst.len() < 20 {
                    dst.push(mon);
                }
            }
        }
    }
    view.pc_boxes = pc_boxes;
    view.pc_current_box = pc_current_box
        .unwrap_or(0)
        .min(view.pc_boxes.len().saturating_sub(1));

    recenter_camera_on_player(
        &view.map,
        &mut view.camera_tx,
        &mut view.camera_ty,
        player_tx,
        player_ty,
    );
    Ok(view)
}

fn start_battle_from_pending(
    pokered_root: &Path,
    pending: PendingBattle,
    trainer_parties: &HashMap<String, Vec<TrainerParty>>,
    wild_encounters: &HashMap<String, WildEncounterTable>,
    wild_slot_thresholds: &[u8],
    player_party: &[PlayerMon],
    inventory: &HashMap<String, u32>,
    pokemon_display_names: &HashMap<String, String>,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
    rng: u32,
    pokedex_seen: HashSet<String>,
    pokedex_caught: HashSet<String>,
) -> Result<BattleState, Box<dyn Error>> {
    let mut rng = rng;
    let (kind, enemy_party) = match pending {
        PendingBattle::Trainer {
            map_name,
            text_id,
            opponent,
            trainer_number,
        } => {
            let enemy_party =
                trainer_party_for_opponent(trainer_parties, &opponent, &trainer_number)
                    .unwrap_or_else(|| {
                        vec![TrainerPartyMon {
                            level: 5,
                            species: "RATTATA".to_string(),
                        }]
                    });
            (
                BattleKind::Trainer {
                    map_name,
                    text_id,
                    opponent,
                },
                enemy_party,
            )
        }
        PendingBattle::Wild { map_name } => {
            let table = wild_encounters
                .get(&map_name)
                .ok_or_else(|| format!("Missing wild data for map `{map_name}`"))?;
            if table.grass.is_empty() {
                return Err(format!("Wild data for map `{map_name}` has no grass slots").into());
            }
            if wild_slot_thresholds.is_empty() {
                return Err("Wild slot thresholds are empty".into());
            }
            let roll = next_rand_u8(&mut rng);
            let slot_i = wild_slot_thresholds
                .iter()
                .position(|&t| roll <= t)
                .unwrap_or(wild_slot_thresholds.len().saturating_sub(1));
            let slot = table
                .grass
                .get(slot_i)
                .or_else(|| table.grass.first())
                .ok_or_else(|| "Wild grass slots are empty".to_string())?;
            let enemy_party = vec![TrainerPartyMon {
                level: slot.level,
                species: slot.species.clone(),
            }];
            (BattleKind::Wild { map_name }, enemy_party)
        }
    };

    let enemy0 = enemy_party
        .first()
        .cloned()
        .ok_or_else(|| "Trainer party is empty".to_string())?;
    let enemy = make_battle_mon(
        pokered_root,
        &enemy0.species,
        enemy0.level,
        pokemon_stats,
        pokemon_moves,
    )?;

    let mut player_party: Vec<PlayerMon> = if player_party.is_empty() {
        vec![make_player_mon(
            "BULBASAUR",
            5,
            pokemon_stats,
            pokemon_moves,
        )?]
    } else {
        player_party.to_vec()
    };
    for mon in &mut player_party {
        if mon.moves.is_empty() {
            mon.moves = pokemon_moves_at_level(
                &mon.species,
                mon.level,
                pokemon_moves,
            );
            if mon.moves.is_empty() {
                mon.moves.push("TACKLE".to_string());
            }
        }
        if mon.hp > mon.max_hp {
            mon.hp = mon.max_hp;
        }
    }
    let player_party_index = player_party.iter().position(|m| m.hp > 0).unwrap_or(0);
    let player = make_battle_mon_from_player_mon(
        pokered_root,
        &player_party[player_party_index],
        pokemon_stats,
        pokemon_moves,
    )?;

    let mut pokedex_seen = pokedex_seen;
    let pokedex_caught = pokedex_caught;

    let mut battle = BattleState {
        kind,
        player,
        player_party,
        player_party_index,
        inventory: inventory.clone(),
        enemy,
        enemy_party,
        enemy_party_index: 0,
        ui: BattleUi {
            screen: BattleUiScreen::Command,
            selection: 0,
            scroll: 0,
            move_to_learn: None,
            evolution_target: None,
        },
        dialog: None,
        events: VecDeque::new(),
        rng,
        pokedex_seen: pokedex_seen.clone(),
        pokedex_caught: pokedex_caught.clone(),
    };

    // Mark enemy Pokemon as seen
    pokedex_seen.insert(battle.enemy.species.clone());
    battle.pokedex_seen = pokedex_seen;

    let enemy_name = pokemon_display_names
        .get(&battle.enemy.species)
        .map(|s| s.as_str())
        .unwrap_or(battle.enemy.species.as_str());
    let player_name = pokemon_display_names
        .get(&battle.player.species)
        .map(|s| s.as_str())
        .unwrap_or(battle.player.species.as_str());

    match &battle.kind {
        BattleKind::Trainer { .. } => {
            battle.dialog = Some(Dialog {
                lines: wrap_dialog_lines(
                    vec!["Trainer wants to fight!".to_string(), String::new()],
                    18,
                ),
                cursor: 0,
            });
            battle.events.push_back(BattleEvent::Message(vec![
                format!("Enemy sent out {enemy_name}!"),
                String::new(),
            ]));
            battle.events.push_back(BattleEvent::Message(vec![
                format!("Go! {player_name}!"),
                String::new(),
            ]));
        }
        BattleKind::Wild { .. } => {
            battle.dialog = Some(Dialog {
                lines: wrap_dialog_lines(
                    vec![format!("Wild {enemy_name} appeared!"), String::new()],
                    18,
                ),
                cursor: 0,
            });
            battle.events.push_back(BattleEvent::Message(vec![
                format!("Go! {player_name}!"),
                String::new(),
            ]));
        }
    }

    Ok(battle)
}

fn make_player_mon(
    species_const: &str,
    level: u8,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
) -> Result<PlayerMon, Box<dyn Error>> {
    let base = base_stats_for_species(pokemon_stats, species_const)?;
    let (max_hp, _attack, _defense, _speed, _special) = calc_battle_stats(&base, level);
    let mut moves = pokemon_moves_at_level(species_const, level, pokemon_moves);
    if moves.is_empty() {
        moves.push("TACKLE".to_string());
    }
    Ok(PlayerMon {
        species: species_const.to_string(),
        level,
        hp: max_hp,
        max_hp,
        exp: exp_for_level(base.growth_rate, level),
        moves,
    })
}

fn make_battle_mon_from_player_mon(
    pokered_root: &Path,
    mon: &PlayerMon,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
) -> Result<BattleMon, Box<dyn Error>> {
    let base = base_stats_for_species(pokemon_stats, &mon.species)?;
    let (max_hp, attack, defense, speed, special) = calc_battle_stats(&base, mon.level);
    let mut moves = mon.moves.clone();
    if moves.is_empty() {
        moves = pokemon_moves_at_level(&mon.species, mon.level, pokemon_moves);
    }
    if moves.is_empty() {
        moves.push("TACKLE".to_string());
    }

    let slug = species_const_to_gfx_slug(&mon.species);
    let front_sprite = load_grayscale_png(
        &pokered_root
            .join("gfx/pokemon/front")
            .join(format!("{slug}.png")),
    )?;
    let back_sprite = load_grayscale_png(
        &pokered_root
            .join("gfx/pokemon/back")
            .join(format!("{slug}b.png")),
    )?;

    let hp = mon.hp.min(max_hp);

    // Initialize PP for each move (default 20 PP per move)
    // TODO: Load actual PP from move_db when available
    let move_pp: Vec<u8> = moves.iter().map(|_| 20).collect();

    Ok(BattleMon {
        species: mon.species.clone(),
        level: mon.level,
        types: (base.type1.clone(), base.type2.clone()),
        max_hp,
        hp,
        attack,
        defense,
        speed,
        special,
        catch_rate: base.catch_rate,
        base_exp: base.base_exp,
        moves,
        move_pp,
        front_sprite,
        back_sprite,
        status: StatusCondition::None,
        stat_stage_attack: 0,
        stat_stage_defense: 0,
        stat_stage_speed: 0,
        stat_stage_special: 0,
    })
}

fn make_battle_mon(
    pokered_root: &Path,
    species_const: &str,
    level: u8,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
) -> Result<BattleMon, Box<dyn Error>> {
    let base = base_stats_for_species(pokemon_stats, species_const)?;
    let (max_hp, attack, defense, speed, special) = calc_battle_stats(&base, level);
    let mut moves = pokemon_moves_at_level(species_const, level, pokemon_moves);
    if moves.is_empty() {
        moves.push("TACKLE".to_string());
    }

    let slug = species_const_to_gfx_slug(species_const);
    let front_sprite = load_grayscale_png(
        &pokered_root
            .join("gfx/pokemon/front")
            .join(format!("{slug}.png")),
    )?;
    let back_sprite = load_grayscale_png(
        &pokered_root
            .join("gfx/pokemon/back")
            .join(format!("{slug}b.png")),
    )?;

    // Initialize PP for each move (default 20 PP per move)
    // TODO: Load actual PP from move_db when available
    let move_pp: Vec<u8> = moves.iter().map(|_| 20).collect();

    Ok(BattleMon {
        species: species_const.to_string(),
        level,
        types: (base.type1.clone(), base.type2.clone()),
        max_hp,
        hp: max_hp,
        attack,
        defense,
        speed,
        special,
        catch_rate: base.catch_rate,
        base_exp: base.base_exp,
        moves,
        move_pp,
        front_sprite,
        back_sprite,
        status: StatusCondition::None,
        stat_stage_attack: 0,
        stat_stage_defense: 0,
        stat_stage_speed: 0,
        stat_stage_special: 0,
    })
}

fn calc_battle_stats(base: &BaseStats, level: u8) -> (u16, u16, u16, u16, u16) {
    const DV: u16 = 8;
    let level = level as u16;

    let hp = ((((base.hp as u16 + DV) * 2) * level) / 100) + level + 10;
    let atk = ((((base.attack as u16 + DV) * 2) * level) / 100) + 5;
    let def = ((((base.defense as u16 + DV) * 2) * level) / 100) + 5;
    let spd = ((((base.speed as u16 + DV) * 2) * level) / 100) + 5;
    let spc = ((((base.special as u16 + DV) * 2) * level) / 100) + 5;
    (hp, atk, def, spd, spc)
}

fn battle_handle_a_button(
    battle: &mut BattleState,
    move_db: &HashMap<String, MoveData>,
    item_display_names: &HashMap<String, String>,
    move_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
    type_chart: &TypeChart,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
    pokered_root: &Path,
) -> Result<Option<BattleResult>, Box<dyn Error>> {
    if let Some(dialog) = &mut battle.dialog {
        if dialog.advance() {
            battle.dialog = None;
        }
        return battle_pump_events(
            battle,
            move_db,
            move_display_names,
            pokemon_display_names,
            type_chart,
            pokemon_stats,
            pokemon_moves,
            pokered_root,
        );
    }

    match battle.ui.screen {
        BattleUiScreen::Command => match battle.ui.selection {
            0 => {
                battle.ui.screen = BattleUiScreen::Fight;
                battle.ui.selection = 0;
                battle.ui.scroll = 0;
            }
            1 => {
                battle.ui.screen = BattleUiScreen::Party;
                battle.ui.selection = battle
                    .player_party_index
                    .min(battle.player_party.len().saturating_sub(1));
                battle.ui.scroll = 0;
            }
            2 => {
                battle.ui.screen = BattleUiScreen::Items;
                battle.ui.selection = 0;
                battle.ui.scroll = 0;
            }
            3 => {
                battle.events.clear();
                battle.events.push_back(BattleEvent::Message(vec![
                    "Got away safely!".to_string(),
                    String::new(),
                ]));
                battle.events.push_back(BattleEvent::EndBattle {
                    result: BattleResult::Run,
                });
            }
            _ => {
                battle.dialog = Some(Dialog {
                    lines: wrap_dialog_lines(
                        vec!["Not implemented.".to_string(), String::new()],
                        18,
                    ),
                    cursor: 0,
                });
            }
        },
        BattleUiScreen::Fight => {
            let moves_len = battle.player.moves.len().min(4);
            if moves_len == 0 {
                battle.ui.screen = BattleUiScreen::Command;
                battle.ui.selection = 0;
                battle.ui.scroll = 0;
            } else {
                let idx = battle.ui.selection.min(moves_len - 1);
                let chosen = battle.player.moves[idx].clone();
                let enemy_move = battle_choose_enemy_move(battle);

                battle.ui.screen = BattleUiScreen::Command;
                battle.ui.selection = 0;
                battle.ui.scroll = 0;

                battle.events.clear();
                let player_first = battle.player.speed >= battle.enemy.speed;
                if player_first {
                    battle
                        .events
                        .push_back(BattleEvent::PlayerAttack { move_id: chosen });
                    battle.events.push_back(BattleEvent::EnemyAttack {
                        move_id: enemy_move,
                    });
                } else {
                    battle.events.push_back(BattleEvent::EnemyAttack {
                        move_id: enemy_move,
                    });
                    battle
                        .events
                        .push_back(BattleEvent::PlayerAttack { move_id: chosen });
                }
            }
        }
        BattleUiScreen::Party => {
            if battle.player_party.is_empty() {
                battle.ui.screen = BattleUiScreen::Command;
                battle.ui.selection = 0;
                battle.ui.scroll = 0;
            } else {
                let idx = battle
                    .ui
                    .selection
                    .min(battle.player_party.len().saturating_sub(1));
                if battle.player_party[idx].hp == 0 {
                    battle.dialog = Some(Dialog {
                        lines: wrap_dialog_lines(
                            vec!["No! There's no will to fight!".to_string(), String::new()],
                            18,
                        ),
                        cursor: 0,
                    });
                } else if idx == battle.player_party_index {
                    battle.ui.screen = BattleUiScreen::Command;
                    battle.ui.selection = 0;
                    battle.ui.scroll = 0;
                } else {
                    battle.player_party_index = idx;
                    battle.player = make_battle_mon_from_player_mon(
                        pokered_root,
                        &battle.player_party[idx],
                        pokemon_stats,
                        pokemon_moves,
                    )?;
                    let enemy_move = battle_choose_enemy_move(battle);
                    let player_name = pokemon_display_names
                        .get(&battle.player.species)
                        .cloned()
                        .unwrap_or_else(|| battle.player.species.clone());

                    battle.ui.screen = BattleUiScreen::Command;
                    battle.ui.selection = 0;
                    battle.ui.scroll = 0;

                    battle.events.clear();
                    battle.events.push_back(BattleEvent::Message(vec![
                        format!("Go! {player_name}!"),
                        String::new(),
                    ]));
                    battle.events.push_back(BattleEvent::EnemyAttack {
                        move_id: enemy_move,
                    });
                }
            }
        }
        BattleUiScreen::Items => {
            let ids = inventory_item_ids_sorted_map(&battle.inventory);
            let ids: Vec<String> = ids
                .into_iter()
                .filter(|id| battle.inventory.get(id).copied().unwrap_or(0) > 0)
                .collect();
            if ids.is_empty() {
                battle.ui.screen = BattleUiScreen::Command;
                battle.ui.selection = 0;
                battle.ui.scroll = 0;
            } else {
                let idx = battle.ui.selection.min(ids.len().saturating_sub(1));
                let item_id = ids[idx].clone();
                let count = battle.inventory.get(&item_id).copied().unwrap_or(0);
                if count == 0 {
                    battle.dialog = Some(Dialog {
                        lines: wrap_dialog_lines(vec!["No items.".to_string(), String::new()], 18),
                        cursor: 0,
                    });
                } else if matches!(
                    item_id.as_str(),
                    "POKE_BALL" | "GREAT_BALL" | "ULTRA_BALL" | "MASTER_BALL" | "SAFARI_BALL"
                ) {
                    if matches!(battle.kind, BattleKind::Trainer { .. }) {
                        battle.dialog = Some(Dialog {
                            lines: wrap_dialog_lines(
                                vec!["The trainer blocked the ball!".to_string(), String::new()],
                                18,
                            ),
                            cursor: 0,
                        });
                    } else {
                        consume_item(&mut battle.inventory, &item_id, 1);
                        let (captured, shakes) = battle_pokeball_capture(battle, &item_id);
                        let enemy_name = pokemon_display_names
                            .get(&battle.enemy.species)
                            .cloned()
                            .unwrap_or_else(|| battle.enemy.species.clone());
                        let ball_name = item_display_names
                            .get(&item_id)
                            .cloned()
                            .unwrap_or_else(|| item_id.clone());

                        battle.ui.screen = BattleUiScreen::Command;
                        battle.ui.selection = 0;
                        battle.ui.scroll = 0;

                        battle.events.clear();
                        if captured {
                            // Mark Pokemon as caught in Pokedex
                            battle.pokedex_caught.insert(battle.enemy.species.clone());

                            let had_room = battle.player_party.len() < 6;
                            if had_room {
                                if let Ok(mut mon) = make_player_mon(
                                    &battle.enemy.species,
                                    battle.enemy.level,
                                    pokemon_stats,
                                    pokemon_moves,
                                ) {
                                    mon.hp = battle.enemy.hp.min(mon.max_hp);
                                    battle.player_party.push(mon);
                                }
                            }
                            battle.events.push_back(BattleEvent::Message(vec![
                                format!("Threw {ball_name}!"),
                                format!("All right! {enemy_name} was caught!"),
                            ]));
                            if !had_room {
                                battle.events.push_back(BattleEvent::Message(vec![
                                    "Party is full.".to_string(),
                                    String::new(),
                                ]));
                            }
                            battle.events.push_back(BattleEvent::EndBattle {
                                result: BattleResult::Caught,
                            });
                        } else {
                            let fail_line = match shakes {
                                0 => "You missed the POKéMON!".to_string(),
                                1 => "Darn! The POKéMON broke free!".to_string(),
                                2 => "Aww! It appeared to be caught!".to_string(),
                                _ => "Shoot! It was so close too!".to_string(),
                            };
                            battle.events.push_back(BattleEvent::Message(vec![
                                format!("Threw {ball_name}!"),
                                fail_line,
                            ]));
                            let enemy_move = battle_choose_enemy_move(battle);
                            battle.events.push_back(BattleEvent::EnemyAttack {
                                move_id: enemy_move,
                            });
                        }
                    }
                } else if matches!(
                    item_id.as_str(),
                    "POTION" | "SUPER_POTION" | "HYPER_POTION" | "MAX_POTION"
                ) {
                    let heal_amount: u16 = match item_id.as_str() {
                        "POTION" => 20,
                        "SUPER_POTION" => 50,
                        "HYPER_POTION" => 200,
                        _ => u16::MAX,
                    };
                    if battle.player.hp >= battle.player.max_hp {
                        battle.dialog = Some(Dialog {
                            lines: wrap_dialog_lines(
                                vec!["It won't have any effect.".to_string(), String::new()],
                                18,
                            ),
                            cursor: 0,
                        });
                    } else {
                        consume_item(&mut battle.inventory, &item_id, 1);
                        let old_hp = battle.player.hp;
                        let new_hp = if heal_amount == u16::MAX {
                            battle.player.max_hp
                        } else {
                            battle
                                .player
                                .hp
                                .saturating_add(heal_amount)
                                .min(battle.player.max_hp)
                        };
                        battle.player.hp = new_hp;
                        if let Some(active) = battle.player_party.get_mut(battle.player_party_index)
                        {
                            active.hp = new_hp;
                        }
                        let restored = new_hp.saturating_sub(old_hp);

                        let item_name = item_display_names
                            .get(&item_id)
                            .cloned()
                            .unwrap_or_else(|| item_id.clone());
                        let player_name = pokemon_display_names
                            .get(&battle.player.species)
                            .cloned()
                            .unwrap_or_else(|| battle.player.species.clone());

                        battle.ui.screen = BattleUiScreen::Command;
                        battle.ui.selection = 0;
                        battle.ui.scroll = 0;

                        battle.events.clear();
                        battle.events.push_back(BattleEvent::Message(vec![
                            format!("Used {item_name}!"),
                            format!("{player_name} recovered {restored} HP!"),
                        ]));
                        let enemy_move = battle_choose_enemy_move(battle);
                        battle.events.push_back(BattleEvent::EnemyAttack {
                            move_id: enemy_move,
                        });
                    }
                } else {
                    battle.dialog = Some(Dialog {
                        lines: wrap_dialog_lines(
                            vec!["Not implemented.".to_string(), String::new()],
                            18,
                        ),
                        cursor: 0,
                    });
                }
            }
        }
        BattleUiScreen::LearnMove => {
            let mon = &battle.player_party[battle.player_party_index];
            let num_moves = mon.moves.len();
            let move_to_learn = battle.ui.move_to_learn.clone().unwrap_or_default();
            let player_name = pokemon_display_names
                .get(&battle.player.species)
                .cloned()
                .unwrap_or_else(|| battle.player.species.clone());
            let move_name = move_display_names
                .get(&move_to_learn)
                .cloned()
                .unwrap_or_else(|| move_to_learn.clone());

            if battle.ui.selection < num_moves {
                // Replace the selected move
                let old_move = battle.player_party[battle.player_party_index].moves[battle.ui.selection].clone();
                let old_move_name = move_display_names
                    .get(&old_move)
                    .map(|s| s.as_str())
                    .unwrap_or(old_move.as_str());

                battle.player_party[battle.player_party_index].moves[battle.ui.selection] = move_to_learn.clone();
                battle.player.moves[battle.ui.selection] = move_to_learn;

                battle.ui.screen = BattleUiScreen::Command;
                battle.ui.selection = 0;
                battle.ui.scroll = 0;
                battle.ui.move_to_learn = None;

                battle.dialog = Some(Dialog {
                    lines: wrap_dialog_lines(
                        vec![
                            format!("1, 2, and... Poof!"),
                            format!("{player_name} forgot {old_move_name}."),
                            format!("And..."),
                            format!("{player_name} learned {move_name}!"),
                        ],
                        18,
                    ),
                    cursor: 0,
                });
            } else {
                // Don't learn the move
                battle.ui.screen = BattleUiScreen::Command;
                battle.ui.selection = 0;
                battle.ui.scroll = 0;
                battle.ui.move_to_learn = None;

                battle.dialog = Some(Dialog {
                    lines: wrap_dialog_lines(
                        vec![
                            format!("{player_name} did not learn {move_name}."),
                            String::new(),
                        ],
                        18,
                    ),
                    cursor: 0,
                });
            }
        }
        BattleUiScreen::Evolution => {
            // Pressing A confirms evolution
            let target_species = battle.ui.evolution_target.clone().unwrap_or_default();
            let old_name = pokemon_display_names
                .get(&battle.player.species)
                .cloned()
                .unwrap_or_else(|| battle.player.species.clone());
            let new_name = pokemon_display_names
                .get(&target_species)
                .cloned()
                .unwrap_or_else(|| target_species.clone());

            // Evolve the Pokemon in the party
            battle.player_party[battle.player_party_index].species = target_species.clone();

            // Update the battle mon
            if let Ok(base) = base_stats_for_species(pokemon_stats, &target_species) {
                let mon_level = battle.player_party[battle.player_party_index].level;
                let old_hp = battle.player_party[battle.player_party_index].hp;
                let old_max_hp = battle.player_party[battle.player_party_index].max_hp;

                let (max_hp, attack, defense, speed, special) = calc_battle_stats(&base, mon_level);
                battle.player_party[battle.player_party_index].max_hp = max_hp;

                // Heal the difference in max HP
                if max_hp > old_max_hp {
                    battle.player_party[battle.player_party_index].hp = old_hp + (max_hp - old_max_hp);
                } else {
                    battle.player_party[battle.player_party_index].hp = old_hp.min(max_hp);
                }

                // Update battle mon stats
                battle.player.species = target_species.clone();
                battle.player.types = (base.type1.clone(), base.type2.clone());
                battle.player.max_hp = max_hp;
                battle.player.hp = battle.player_party[battle.player_party_index].hp;
                battle.player.attack = attack;
                battle.player.defense = defense;
                battle.player.speed = speed;
                battle.player.special = special;
                battle.player.catch_rate = base.catch_rate;
                battle.player.base_exp = base.base_exp;
            }

            battle.ui.screen = BattleUiScreen::Command;
            battle.ui.selection = 0;
            battle.ui.scroll = 0;
            battle.ui.evolution_target = None;

            battle.dialog = Some(Dialog {
                lines: wrap_dialog_lines(
                    vec![
                        format!("Congratulations! Your {old_name}"),
                        format!("evolved into {new_name}!"),
                    ],
                    18,
                ),
                cursor: 0,
            });
        }
    }

    battle_pump_events(
        battle,
        move_db,
        move_display_names,
        pokemon_display_names,
        type_chart,
        pokemon_stats,
        pokemon_moves,
        pokered_root,
    )
}

fn battle_handle_b_button(
    battle: &mut BattleState,
    move_db: &HashMap<String, MoveData>,
    item_display_names: &HashMap<String, String>,
    move_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
    type_chart: &TypeChart,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
    pokered_root: &Path,
) -> Result<Option<BattleResult>, Box<dyn Error>> {
    if battle.dialog.is_some() {
        return battle_handle_a_button(
            battle,
            move_db,
            item_display_names,
            move_display_names,
            pokemon_display_names,
            type_chart,
            pokemon_stats,
            pokemon_moves,
            pokered_root,
        );
    }

    if battle.ui.screen == BattleUiScreen::Fight
        || battle.ui.screen == BattleUiScreen::Party
        || battle.ui.screen == BattleUiScreen::Items
    {
        battle.ui.screen = BattleUiScreen::Command;
        battle.ui.selection = 0;
        battle.ui.scroll = 0;
    } else if battle.ui.screen == BattleUiScreen::LearnMove {
        // Pressing B cancels learning the move
        let move_to_learn = battle.ui.move_to_learn.clone().unwrap_or_default();
        let player_name = pokemon_display_names
            .get(&battle.player.species)
            .cloned()
            .unwrap_or_else(|| battle.player.species.clone());
        let move_name = move_display_names
            .get(&move_to_learn)
            .cloned()
            .unwrap_or_else(|| move_to_learn.clone());

        battle.ui.screen = BattleUiScreen::Command;
        battle.ui.selection = 0;
        battle.ui.scroll = 0;
        battle.ui.move_to_learn = None;

        battle.dialog = Some(Dialog {
            lines: wrap_dialog_lines(
                vec![
                    format!("{player_name} did not learn {move_name}."),
                    String::new(),
                ],
                18,
            ),
            cursor: 0,
        });
    } else if battle.ui.screen == BattleUiScreen::Evolution {
        // Pressing B cancels evolution
        let player_name = pokemon_display_names
            .get(&battle.player.species)
            .cloned()
            .unwrap_or_else(|| battle.player.species.clone());

        battle.ui.screen = BattleUiScreen::Command;
        battle.ui.selection = 0;
        battle.ui.scroll = 0;
        battle.ui.evolution_target = None;

        battle.dialog = Some(Dialog {
            lines: wrap_dialog_lines(
                vec![
                    format!("Huh? {player_name} stopped evolving!"),
                    String::new(),
                ],
                18,
            ),
            cursor: 0,
        });
    }

    Ok(None)
}

fn battle_pump_events(
    battle: &mut BattleState,
    move_db: &HashMap<String, MoveData>,
    move_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
    type_chart: &TypeChart,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
    pokered_root: &Path,
) -> Result<Option<BattleResult>, Box<dyn Error>> {
    loop {
        if battle.dialog.is_some() {
            return Ok(None);
        }

        let Some(event) = battle.events.pop_front() else {
            return Ok(None);
        };

        match event {
            BattleEvent::Message(lines) => {
                battle.dialog = Some(Dialog {
                    lines: wrap_dialog_lines(lines, 18),
                    cursor: 0,
                });
                return Ok(None);
            }
            BattleEvent::PlayerAttack { move_id } => {
                battle_resolve_attack(
                    battle,
                    true,
                    &move_id,
                    move_db,
                    move_display_names,
                    pokemon_display_names,
                    type_chart,
                    pokemon_stats,
                    pokemon_moves,
                )?;
                return Ok(None);
            }
            BattleEvent::EnemyAttack { move_id } => {
                battle_resolve_attack(
                    battle,
                    false,
                    &move_id,
                    move_db,
                    move_display_names,
                    pokemon_display_names,
                    type_chart,
                    pokemon_stats,
                    pokemon_moves,
                )?;
                return Ok(None);
            }
            BattleEvent::SendOutNextEnemy => {
                if battle.enemy_party_index >= battle.enemy_party.len() {
                    battle.events.clear();
                    battle.events.push_back(BattleEvent::EndBattle {
                        result: BattleResult::Win,
                    });
                    continue;
                }
                let spec = battle.enemy_party[battle.enemy_party_index].clone();
                battle.enemy = make_battle_mon(
                    pokered_root,
                    &spec.species,
                    spec.level,
                    pokemon_stats,
                    pokemon_moves,
                )?;
                // Mark new enemy Pokemon as seen
                battle.pokedex_seen.insert(battle.enemy.species.clone());

                let enemy_name = pokemon_display_names
                    .get(&battle.enemy.species)
                    .map(|s| s.as_str())
                    .unwrap_or(battle.enemy.species.as_str());
                battle.dialog = Some(Dialog {
                    lines: wrap_dialog_lines(
                        vec![format!("Enemy sent out {enemy_name}!"), String::new()],
                        18,
                    ),
                    cursor: 0,
                });
                return Ok(None);
            }
            BattleEvent::SendOutNextPlayer => {
                let next_i = battle
                    .player_party
                    .iter()
                    .position(|m| m.hp > 0)
                    .unwrap_or(usize::MAX);
                if next_i == usize::MAX {
                    battle.events.clear();
                    battle.events.push_back(BattleEvent::EndBattle {
                        result: BattleResult::Lose,
                    });
                    continue;
                }
                battle.player_party_index = next_i;
                battle.player = make_battle_mon_from_player_mon(
                    pokered_root,
                    &battle.player_party[next_i],
                    pokemon_stats,
                    pokemon_moves,
                )?;
                battle.ui.screen = BattleUiScreen::Command;
                battle.ui.selection = 0;
                battle.ui.scroll = 0;

                let player_name = pokemon_display_names
                    .get(&battle.player.species)
                    .map(|s| s.as_str())
                    .unwrap_or(battle.player.species.as_str());
                battle.dialog = Some(Dialog {
                    lines: wrap_dialog_lines(
                        vec![format!("Go! {player_name}!"), String::new()],
                        18,
                    ),
                    cursor: 0,
                });
                return Ok(None);
            }
            BattleEvent::Evolution { target_species } => {
                let player_name = pokemon_display_names
                    .get(&battle.player.species)
                    .map(|s| s.as_str())
                    .unwrap_or(battle.player.species.as_str());

                battle.ui.screen = BattleUiScreen::Evolution;
                battle.ui.selection = 0;
                battle.ui.evolution_target = Some(target_species.clone());

                battle.dialog = Some(Dialog {
                    lines: wrap_dialog_lines(
                        vec![
                            format!("What? {player_name} is evolving!"),
                            String::new(),
                        ],
                        18,
                    ),
                    cursor: 0,
                });
                return Ok(None);
            }
            BattleEvent::LearnMove { move_id } => {
                let player_name = pokemon_display_names
                    .get(&battle.player.species)
                    .map(|s| s.as_str())
                    .unwrap_or(battle.player.species.as_str());
                let move_name = move_display_names
                    .get(&move_id)
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| move_id.clone());

                let mon = &battle.player_party[battle.player_party_index];
                if mon.moves.len() < 4 {
                    // Automatically learn the move if we have room
                    battle.player_party[battle.player_party_index]
                        .moves
                        .push(move_id.clone());
                    battle.player.moves.push(move_id);
                    battle.dialog = Some(Dialog {
                        lines: wrap_dialog_lines(
                            vec![
                                format!("{player_name} learned {move_name}!"),
                                String::new(),
                            ],
                            18,
                        ),
                        cursor: 0,
                    });
                } else {
                    // Need to choose which move to forget
                    battle.ui.screen = BattleUiScreen::LearnMove;
                    battle.ui.selection = 0;
                    battle.ui.move_to_learn = Some(move_id);
                    battle.dialog = Some(Dialog {
                        lines: wrap_dialog_lines(
                            vec![
                                format!("{player_name} is trying to learn {move_name}."),
                                format!("But {player_name} already knows 4 moves."),
                            ],
                            18,
                        ),
                        cursor: 0,
                    });
                }
                return Ok(None);
            }
            BattleEvent::EndBattle { result } => return Ok(Some(result)),
        }
    }
}

fn battle_choose_enemy_move(battle: &mut BattleState) -> String {
    let moves: Vec<&String> = battle.enemy.moves.iter().take(4).collect();
    if moves.is_empty() {
        return "TACKLE".to_string();
    }
    let idx = (next_rand_u8(&mut battle.rng) as usize) % moves.len();
    moves[idx].clone()
}

fn consume_item(inventory: &mut HashMap<String, u32>, item_id: &str, amount: u32) {
    let Some(count) = inventory.get_mut(item_id) else {
        return;
    };
    if *count <= amount {
        inventory.remove(item_id);
    } else {
        *count -= amount;
    }
}

fn battle_pokeball_capture(battle: &mut BattleState, item_id: &str) -> (bool, u8) {
    if item_id == "MASTER_BALL" {
        return (true, 3);
    }

    let max_hp = battle.enemy.max_hp.max(1) as u32;
    let hp = battle.enemy.hp as u32;
    let catch_rate = battle.enemy.catch_rate;

    let ball_factor: u32 = if item_id == "GREAT_BALL" { 8 } else { 12 };
    let w = ((max_hp * 255) / ball_factor) / ((hp / 4).max(1));
    let x = w.min(255) as u8;
    let w_gt_255 = w > 255;

    let rand1 = next_rand_u8(&mut battle.rng);
    if rand1 > catch_rate {
        let shakes = battle_pokeball_shakes(item_id, x, catch_rate);
        return (false, shakes);
    }
    if w_gt_255 {
        return (true, 3);
    }

    let rand2 = next_rand_u8(&mut battle.rng);
    if rand2 > x {
        let shakes = battle_pokeball_shakes(item_id, x, catch_rate);
        return (false, shakes);
    }

    (true, 3)
}

fn battle_pokeball_shakes(item_id: &str, x: u8, catch_rate: u8) -> u8 {
    let ball_factor2: u32 = match item_id {
        "POKE_BALL" => 255,
        "GREAT_BALL" => 200,
        "ULTRA_BALL" | "SAFARI_BALL" => 150,
        _ => 255,
    };
    let y = (catch_rate as u32 * 100) / ball_factor2;
    let z = (x as u32 * y) / 255;
    if z < 10 {
        0
    } else if z < 30 {
        1
    } else if z < 70 {
        2
    } else {
        3
    }
}

fn battle_exp_gain(battle: &BattleState, enemy_base_exp: u8, enemy_level: u8) -> u32 {
    if enemy_base_exp == 0 || enemy_level == 0 {
        return 0;
    }
    let mut exp = (enemy_base_exp as u32 * enemy_level as u32) / 7;
    if exp == 0 {
        exp = 1;
    }
    if matches!(battle.kind, BattleKind::Trainer { .. }) {
        exp = (exp * 3) / 2;
    }
    exp.max(1)
}

fn battle_award_exp_to_active_player(
    battle: &mut BattleState,
    exp: u32,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
) -> Result<(), Box<dyn Error>> {
    if exp == 0 {
        return Ok(());
    }
    let Some(mon) = battle.player_party.get_mut(battle.player_party_index) else {
        return Ok(());
    };

    mon.exp = mon.exp.saturating_add(exp);
    let base = base_stats_for_species(pokemon_stats, &mon.species)?;
    let max_exp = exp_for_level(base.growth_rate, 100);
    if mon.exp > max_exp {
        mon.exp = max_exp;
    }
    let new_level = level_from_exp(mon.exp, base.growth_rate);
    if new_level == mon.level {
        return Ok(());
    }

    let old_max_hp = mon.max_hp;
    mon.level = new_level;
    let (new_max_hp, _atk, _def, _spd, _spc) = calc_battle_stats(&base, new_level);
    mon.max_hp = new_max_hp;
    if new_max_hp > old_max_hp {
        mon.hp = mon
            .hp
            .saturating_add(new_max_hp - old_max_hp)
            .min(new_max_hp);
    } else {
        mon.hp = mon.hp.min(new_max_hp);
    }

    // Check for evolution at this level (before learning new moves)
    if let Some(target_species) = pokemon_evolution_at_level(&mon.species, mon.level, pokemon_moves) {
        battle.events.push_back(BattleEvent::Evolution { target_species });
    }

    // Queue LearnMove events for each new move at this level
    let new_moves = pokemon_moves_learned_at_level(
        &mon.species,
        mon.level,
        pokemon_moves,
    );
    for move_id in new_moves {
        battle.events.push_back(BattleEvent::LearnMove { move_id });
    }

    // Ensure Pokemon has at least one move
    if mon.moves.is_empty() {
        mon.moves.push("TACKLE".to_string());
    }

    let (max_hp, attack, defense, speed, special) = calc_battle_stats(&base, mon.level);
    battle.player.level = mon.level;
    battle.player.max_hp = max_hp;
    battle.player.hp = mon.hp.min(max_hp);
    battle.player.attack = attack;
    battle.player.defense = defense;
    battle.player.speed = speed;
    battle.player.special = special;
    battle.player.catch_rate = base.catch_rate;
    battle.player.base_exp = base.base_exp;
    battle.player.moves = mon.moves.clone();

    Ok(())
}

fn battle_resolve_attack(
    battle: &mut BattleState,
    attacker_is_player: bool,
    move_id: &str,
    move_db: &HashMap<String, MoveData>,
    move_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
    type_chart: &TypeChart,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
) -> Result<(), Box<dyn Error>> {
    let attacker_species = if attacker_is_player {
        battle.player.species.as_str()
    } else {
        battle.enemy.species.as_str()
    };
    let attacker_name = pokemon_display_names
        .get(attacker_species)
        .map(|s| s.as_str())
        .unwrap_or(attacker_species);
    let move_name = move_display_names
        .get(move_id)
        .map(|s| s.as_str())
        .unwrap_or(move_id);

    let data = move_db.get(move_id);
    let power = data.map(|d| d.power).unwrap_or(0);
    let accuracy = data.map(|d| d.accuracy).unwrap_or(100);
    let move_type = data.map(|d| d.move_type.as_str()).unwrap_or("NORMAL");

    let mut line2 = String::new();
    if power == 0 {
        line2 = "But nothing happened!".to_string();
    } else {
        let acc = if accuracy == 0 { 100 } else { accuracy };
        let roll = next_rand_u8(&mut battle.rng) % 100;
        if roll >= acc {
            line2 = "But it missed!".to_string();
        } else {
            let (damage, mult10, is_crit) = {
                let (attacker, defender) = if attacker_is_player {
                    (&battle.player, &battle.enemy)
                } else {
                    (&battle.enemy, &battle.player)
                };
                battle_calc_damage(
                    attacker,
                    defender,
                    power,
                    move_type,
                    type_chart,
                    &mut battle.rng,
                )
            };
            if mult10 == 0 {
                line2 = "It had no effect!".to_string();
            } else {
                if attacker_is_player {
                    battle.enemy.hp = battle.enemy.hp.saturating_sub(damage);
                } else {
                    battle.player.hp = battle.player.hp.saturating_sub(damage);
                    if let Some(active) = battle.player_party.get_mut(battle.player_party_index) {
                        active.hp = battle.player.hp;
                    }
                }
                if is_crit {
                    line2 = "A critical hit!".to_string();
                } else if mult10 > 10 {
                    line2 = "It's super effective!".to_string();
                } else if mult10 < 10 {
                    line2 = "It's not very effective...".to_string();
                }
            }
        }
    }

    battle.dialog = Some(Dialog {
        lines: wrap_dialog_lines(
            vec![format!("{attacker_name} used {move_name}!"), line2],
            18,
        ),
        cursor: 0,
    });

    if attacker_is_player {
        if battle.enemy.hp == 0 && battle.enemy.max_hp > 0 && power > 0 {
            battle.events.clear();
            let enemy_name = pokemon_display_names
                .get(&battle.enemy.species)
                .map(|s| s.as_str())
                .unwrap_or(battle.enemy.species.as_str());
            battle.events.push_back(BattleEvent::Message(vec![
                format!("Enemy {enemy_name} fainted!"),
                String::new(),
            ]));

            let exp = battle_exp_gain(battle, battle.enemy.base_exp, battle.enemy.level);
            if exp > 0 {
                let player_name = pokemon_display_names
                    .get(&battle.player.species)
                    .cloned()
                    .unwrap_or_else(|| battle.player.species.clone());
                let old_level = battle.player.level;
                battle_award_exp_to_active_player(
                    battle,
                    exp,
                    pokemon_stats,
                    pokemon_moves,
                )?;
                battle.events.push_back(BattleEvent::Message(vec![
                    format!("{player_name} gained {exp} EXP. Points!"),
                    String::new(),
                ]));
                let new_level = battle.player.level;
                if new_level > old_level {
                    battle.events.push_back(BattleEvent::Message(vec![
                        format!("{player_name} grew to level {new_level}!"),
                        String::new(),
                    ]));
                }
            }
            if battle.enemy_party_index + 1 < battle.enemy_party.len() {
                battle.enemy_party_index += 1;
                battle.events.push_back(BattleEvent::SendOutNextEnemy);
            } else {
                battle.events.push_back(BattleEvent::EndBattle {
                    result: BattleResult::Win,
                });
            }
        }
    } else if battle.player.hp == 0 && battle.player.max_hp > 0 && power > 0 {
        battle.events.clear();
        let player_name = pokemon_display_names
            .get(&battle.player.species)
            .map(|s| s.as_str())
            .unwrap_or(battle.player.species.as_str());
        battle.events.push_back(BattleEvent::Message(vec![
            format!("{player_name} fainted!"),
            String::new(),
        ]));
        let has_any_alive = battle.player_party.iter().any(|m| m.hp > 0);
        if has_any_alive {
            battle.events.push_back(BattleEvent::SendOutNextPlayer);
        } else {
            battle.events.push_back(BattleEvent::EndBattle {
                result: BattleResult::Lose,
            });
        }
    }

    Ok(())
}

fn battle_calc_damage(
    attacker: &BattleMon,
    defender: &BattleMon,
    power: u8,
    move_type: &str,
    type_chart: &TypeChart,
    rng: &mut u32,
) -> (u16, u8, bool) {
    if power == 0 {
        return (0, 10, false);
    }

    let (atk, def) = if is_special_type(move_type) {
        (attacker.special, defender.special)
    } else {
        (attacker.attack, defender.defense)
    };
    let atk = atk.max(1);
    let def = def.max(1);

    let level = attacker.level as u16;
    let power = power as u16;
    let mut damage = (((((2 * level) / 5 + 2) * power * atk) / def) / 50) + 2;

    // Critical hit calculation based on base speed
    let crit_chance = (attacker.speed / 2).max(1);
    let crit_roll = next_rand_u8(rng);
    let is_crit = crit_roll < (crit_chance as u8 / 4).min(255);

    // Apply critical hit multiplier (2x)
    if is_crit {
        damage = damage * 2;
    }

    let rand = 217u16 + (next_rand_u8(rng) as u16 % 39);
    damage = (damage * rand) / 255;

    if attacker.types.0 == move_type || attacker.types.1 == move_type {
        damage = (damage * 3) / 2;
    }

    let mult10 = battle_type_multiplier_10(move_type, defender, type_chart);
    damage = (damage * mult10 as u16) / 10;
    if mult10 > 0 && damage == 0 {
        damage = 1;
    }
    (damage, mult10, is_crit)
}

fn battle_type_multiplier_10(move_type: &str, defender: &BattleMon, type_chart: &TypeChart) -> u8 {
    let mut mult: u16 = 10;
    for def_type in [&defender.types.0, &defender.types.1] {
        if def_type.is_empty() {
            continue;
        }
        let factor = type_chart
            .get(move_type)
            .and_then(|row| row.get(def_type.as_str()))
            .copied()
            .unwrap_or(10) as u16;
        mult = (mult * factor) / 10;
    }
    mult.min(255) as u8
}

fn is_special_type(typ: &str) -> bool {
    matches!(
        typ,
        "WATER" | "GRASS" | "FIRE" | "ELECTRIC" | "PSYCHIC_TYPE" | "ICE" | "DRAGON"
    )
}

fn end_battle(
    result: BattleResult,
    battle: BattleState,
    map_view: &mut MapView,
    trainer_base_reward_money: &HashMap<String, u32>,
) {
    let BattleState {
        kind,
        player_party,
        inventory,
        enemy_party,
        rng,
        pokedex_seen,
        pokedex_caught,
        ..
    } = battle;

    map_view.rng = rng;
    map_view.menu = None;
    map_view.pending_battle = None;
    map_view.party = player_party;
    map_view.inventory = inventory;
    map_view.pokedex_seen = pokedex_seen;
    map_view.pokedex_caught = pokedex_caught;

    match (&kind, result) {
        (
            BattleKind::Trainer {
                map_name,
                text_id,
                opponent,
            },
            BattleResult::Win,
        ) => {
            map_view
                .defeated_trainers
                .insert((map_name.clone(), text_id.clone()));
            if map_name == "OaksLab" && text_id == "TEXT_OAKSLAB_RIVAL" && opponent == "OPP_RIVAL1"
            {
                let mut lines = vec![
                    "WHAT? Unbelievable!".to_string(),
                    "I picked the wrong POKéMON!".to_string(),
                ];

                let last_level = enemy_party.last().map(|m| m.level).unwrap_or(0);
                let class = opponent.strip_prefix("OPP_").unwrap_or(opponent);
                let base = trainer_base_reward_money.get(class).copied().unwrap_or(0);
                let reward = (base * last_level as u32) / 100;
                if reward > 0 {
                    map_view.money = map_view.money.saturating_add(reward);
                    lines.push(format!("Got ${reward} for"));
                    lines.push("winning!".to_string());
                }

                map_view
                    .events
                    .insert("EVENT_BATTLED_RIVAL_IN_OAKS_LAB".to_string());
                for mon in &mut map_view.party {
                    mon.hp = mon.max_hp;
                }
                map_view
                    .object_events
                    .retain(|o| o.text_id != "TEXT_OAKSLAB_RIVAL");
                open_dialog_from_lines(map_view, lines);
                return;
            }

            if map_name == "Route22" && text_id == "TEXT_ROUTE22_RIVAL1" && opponent == "OPP_RIVAL1"
            {
                let mut lines = vec![
                    "Awww!".to_string(),
                    "You just lucked out!".to_string(),
                    String::new(),
                    "You should quit dawdling".to_string(),
                    "and get a move on!".to_string(),
                ];

                let last_level = enemy_party.last().map(|m| m.level).unwrap_or(0);
                let class = opponent.strip_prefix("OPP_").unwrap_or(opponent);
                let base = trainer_base_reward_money.get(class).copied().unwrap_or(0);
                let reward = (base * last_level as u32) / 100;
                if reward > 0 {
                    map_view.money = map_view.money.saturating_add(reward);
                    lines.push(format!("Got ${reward} for"));
                    lines.push("winning!".to_string());
                }

                map_view
                    .events
                    .insert("EVENT_BEAT_ROUTE22_RIVAL_1ST_BATTLE".to_string());
                map_view.events.remove("EVENT_1ST_ROUTE22_RIVAL_BATTLE");
                map_view.events.remove("EVENT_ROUTE22_RIVAL_WANTS_BATTLE");
                map_view.object_events.retain(|o| {
                    o.text_id != "TEXT_ROUTE22_RIVAL1" && o.text_id != "TEXT_ROUTE22_RIVAL2"
                });

                open_dialog_from_lines(map_view, lines);
                return;
            }
            let last_level = enemy_party.last().map(|m| m.level).unwrap_or(0);
            let class = opponent.strip_prefix("OPP_").unwrap_or(opponent);
            let base = trainer_base_reward_money.get(class).copied().unwrap_or(0);
            let reward = (base * last_level as u32) / 100;
            if reward > 0 {
                map_view.money = map_view.money.saturating_add(reward);
            }

            let mut lines = vec![
                "You defeated the trainer!".to_string(),
                format!("[{text_id}]"),
            ];
            if reward > 0 {
                lines.push(format!("Got ${reward} for"));
                lines.push("winning!".to_string());
            }
            open_dialog_from_lines(map_view, lines);
        }
        _ => {}
    }
}

fn battle_nav(battle: &mut BattleState, dx: i32, dy: i32) {
    if battle.dialog.is_some() {
        return;
    }

    match battle.ui.screen {
        BattleUiScreen::Command => {
            if dx < 0 && (battle.ui.selection % 2) == 1 {
                battle.ui.selection -= 1;
            } else if dx > 0 && (battle.ui.selection % 2) == 0 {
                battle.ui.selection = (battle.ui.selection + 1).min(3);
            }

            if dy < 0 && battle.ui.selection >= 2 {
                battle.ui.selection -= 2;
            } else if dy > 0 && battle.ui.selection + 2 < 4 {
                battle.ui.selection += 2;
            }
        }
        BattleUiScreen::Fight => {
            let len = battle.player.moves.len().min(4);
            if len == 0 {
                battle.ui.selection = 0;
                return;
            }
            if dy < 0 {
                battle.ui.selection = battle.ui.selection.saturating_sub(1);
            } else if dy > 0 {
                battle.ui.selection = (battle.ui.selection + 1).min(len - 1);
            }
        }
        BattleUiScreen::Party => {
            let len = battle.player_party.len().min(6);
            if len == 0 {
                battle.ui.selection = 0;
                return;
            }
            if dy < 0 {
                battle.ui.selection = battle.ui.selection.saturating_sub(1);
            } else if dy > 0 {
                battle.ui.selection = (battle.ui.selection + 1).min(len - 1);
            }
        }
        BattleUiScreen::Items => {
            let ids = inventory_item_ids_sorted_map(&battle.inventory);
            let len = ids
                .iter()
                .filter(|id| battle.inventory.get(*id).copied().unwrap_or(0) > 0)
                .count();
            if len == 0 {
                battle.ui.selection = 0;
                battle.ui.scroll = 0;
                return;
            }
            if dy < 0 {
                battle.ui.selection = battle.ui.selection.saturating_sub(1);
            } else if dy > 0 {
                battle.ui.selection = (battle.ui.selection + 1).min(len - 1);
            }

            let max_scroll = len.saturating_sub(BATTLE_ITEMS_VISIBLE_ROWS);
            if battle.ui.selection < battle.ui.scroll {
                battle.ui.scroll = battle.ui.selection.min(max_scroll);
            } else if battle.ui.selection >= battle.ui.scroll + BATTLE_ITEMS_VISIBLE_ROWS {
                battle.ui.scroll = (battle.ui.selection + 1)
                    .saturating_sub(BATTLE_ITEMS_VISIBLE_ROWS)
                    .min(max_scroll);
            }
        }
        BattleUiScreen::LearnMove => {
            // 4 moves + "Don't learn" option = 5 total options
            let len = 5;
            if dy < 0 {
                battle.ui.selection = battle.ui.selection.saturating_sub(1);
            } else if dy > 0 {
                battle.ui.selection = (battle.ui.selection + 1).min(len - 1);
            }
        }
        BattleUiScreen::Evolution => {
            // No navigation needed for evolution screen (just A to confirm, B to cancel)
        }
    }
}

fn toggle_menu(view: &mut MapView) {
    if view.menu.is_some() {
        view.menu = None;
        return;
    }
    view.menu = Some(MenuState {
        screen: MenuScreen::Root,
        selection: 0,
        scroll: 0,
    });
}

fn menu_nav(view: &mut MapView, delta: i32) {
    let Some(menu) = view.menu.as_mut() else {
        return;
    };

    // Special handling for Pokedex which uses its own cursor/scroll
    if let MenuScreen::Pokedex { ref mut cursor, ref mut scroll } = menu.screen {
        const POKEDEX_VISIBLE_ROWS: usize = 8;
        let new_cursor = (*cursor as i32 + delta).clamp(0, 150) as usize; // 0-150 for 151 Pokemon
        *cursor = new_cursor;

        // Update scroll to keep cursor visible
        let max_scroll = 151usize.saturating_sub(POKEDEX_VISIBLE_ROWS);
        if new_cursor < *scroll {
            *scroll = new_cursor.min(max_scroll);
        } else if new_cursor >= *scroll + POKEDEX_VISIBLE_ROWS {
            *scroll = (new_cursor + 1)
                .saturating_sub(POKEDEX_VISIBLE_ROWS)
                .min(max_scroll);
        }
        return;
    }

    let item_count = match menu.screen {
        MenuScreen::Root => ROOT_MENU_ITEMS.len(),
        MenuScreen::Items => view.inventory.len(),
        MenuScreen::Party => view.party.len(),
        MenuScreen::Options => 3, // Text Speed, Battle Animations, Sound
        MenuScreen::Pokedex { .. } => 151, // All Pokemon in Pokedex
        MenuScreen::TeachMove { .. } => view.party.len(), // Pokemon selection
    };
    if item_count == 0 {
        menu.selection = 0;
        menu.scroll = 0;
        return;
    }

    let new_sel = (menu.selection as i32 + delta).clamp(0, item_count as i32 - 1);
    menu.selection = new_sel as usize;

    if menu.screen == MenuScreen::Items {
        let max_scroll = item_count.saturating_sub(ITEMS_VISIBLE_ROWS);
        if menu.selection < menu.scroll {
            menu.scroll = menu.selection.min(max_scroll);
        } else if menu.selection >= menu.scroll + ITEMS_VISIBLE_ROWS {
            menu.scroll = (menu.selection + 1)
                .saturating_sub(ITEMS_VISIBLE_ROWS)
                .min(max_scroll);
        }
    } else {
        menu.scroll = 0;
    }
}

fn shop_nav(view: &mut MapView, delta: i32) {
    let Some(shop) = view.shop.as_mut() else {
        return;
    };

    let item_count = shop.items.len();
    if item_count == 0 {
        shop.selection = 0;
        shop.scroll = 0;
        return;
    }

    let new_sel = (shop.selection as i32 + delta).clamp(0, item_count as i32 - 1);
    shop.selection = new_sel as usize;

    let max_scroll = item_count.saturating_sub(ITEMS_VISIBLE_ROWS);
    if shop.selection < shop.scroll {
        shop.scroll = shop.selection.min(max_scroll);
    } else if shop.selection >= shop.scroll + ITEMS_VISIBLE_ROWS {
        shop.scroll = (shop.selection + 1)
            .saturating_sub(ITEMS_VISIBLE_ROWS)
            .min(max_scroll);
    }
}

fn open_pc(view: &mut MapView, location: PcLocation) {
    view.menu = None;
    view.shop = None;
    view.choice = None;
    view.dialog = None;
    view.pc = Some(PcState {
        location,
        screen: PcScreen::Root,
        selection: 0,
        scroll: 0,
    });
}

fn pc_nav(view: &mut MapView, delta: i32) {
    let Some(pc) = view.pc.as_mut() else {
        return;
    };

    let item_count = match pc.screen {
        PcScreen::Root => match pc.location {
            PcLocation::PokemonCenter => 4,
            PcLocation::RedsHouse => 4,
        },
        PcScreen::BillsPc => 5,
        PcScreen::Withdraw => {
            view.pc_boxes
                .get(view.pc_current_box)
                .map(|b| b.len())
                .unwrap_or(0)
                + 1
        }
        PcScreen::Deposit => view.party.len() + 1,
        PcScreen::Release => {
            view.pc_boxes
                .get(view.pc_current_box)
                .map(|b| b.len())
                .unwrap_or(0)
                + 1
        }
        PcScreen::ChangeBox => view.pc_boxes.len().max(1) + 1,
        PcScreen::PlayersPc => 4,
        PcScreen::WithdrawItem => view.pc_items.len() + 1,
        PcScreen::DepositItem => view.inventory.len() + 1,
        PcScreen::TossItem => view.pc_items.len() + 1,
    };

    if item_count == 0 {
        pc.selection = 0;
        pc.scroll = 0;
        return;
    }

    let new_sel = (pc.selection as i32 + delta).clamp(0, item_count as i32 - 1);
    pc.selection = new_sel as usize;

    let max_scroll = item_count.saturating_sub(ITEMS_VISIBLE_ROWS);
    if pc.selection < pc.scroll {
        pc.scroll = pc.selection.min(max_scroll);
    } else if pc.selection >= pc.scroll + ITEMS_VISIBLE_ROWS {
        pc.scroll = (pc.selection + 1)
            .saturating_sub(ITEMS_VISIBLE_ROWS)
            .min(max_scroll);
    }
}

fn choice_nav(view: &mut MapView, delta: i32) {
    let Some(choice) = view.choice.as_mut() else {
        return;
    };
    let option_count: i32 = 2;
    let new_sel = (choice.selection as i32 + delta).clamp(0, option_count - 1);
    choice.selection = new_sel as usize;
}

fn handle_b_button(view: &mut MapView) {
    if view.dialog.is_some() {
        view.dialog = None;
        view.choice = None;
        return;
    }

    if view.shop.is_some() {
        view.shop = None;
        return;
    }

    if view.pc.is_some() {
        handle_pc_b_button(view);
        return;
    }

    let Some(mut menu) = view.menu.take() else {
        return;
    };

    match menu.screen {
        MenuScreen::Items => {
            menu.screen = MenuScreen::Root;
            menu.selection = 2;
            menu.scroll = 0;
            view.menu = Some(menu);
        }
        MenuScreen::Party => {
            menu.screen = MenuScreen::Root;
            menu.selection = 1;
            menu.scroll = 0;
            view.menu = Some(menu);
        }
        MenuScreen::Options => {
            menu.screen = MenuScreen::Root;
            menu.selection = 4;
            menu.scroll = 0;
            view.menu = Some(menu);
        }
        MenuScreen::Pokedex { .. } => {
            menu.screen = MenuScreen::Root;
            menu.selection = 0;
            menu.scroll = 0;
            view.menu = Some(menu);
        }
        MenuScreen::TeachMove { .. } => {
            menu.screen = MenuScreen::Items;
            menu.selection = 0;
            menu.scroll = 0;
            view.menu = Some(menu);
        }
        MenuScreen::Root => {
            view.menu = None;
        }
    }
}

fn handle_pc_b_button(view: &mut MapView) {
    let Some(mut pc) = view.pc.take() else {
        return;
    };

    match pc.screen {
        PcScreen::Root => {
            view.pc = None;
        }
        PcScreen::BillsPc => {
            pc.screen = PcScreen::Root;
            pc.selection = 0;
            pc.scroll = 0;
            view.pc = Some(pc);
        }
        PcScreen::Withdraw | PcScreen::Deposit | PcScreen::Release | PcScreen::ChangeBox => {
            pc.screen = PcScreen::BillsPc;
            pc.selection = 0;
            pc.scroll = 0;
            view.pc = Some(pc);
        }
        PcScreen::PlayersPc => {
            pc.screen = PcScreen::Root;
            pc.selection = 0;
            pc.scroll = 0;
            view.pc = Some(pc);
        }
        PcScreen::WithdrawItem | PcScreen::DepositItem | PcScreen::TossItem => {
            pc.screen = match pc.location {
                PcLocation::PokemonCenter => PcScreen::PlayersPc,
                PcLocation::RedsHouse => PcScreen::Root,
            };
            pc.selection = 0;
            pc.scroll = 0;
            view.pc = Some(pc);
        }
    }
}

fn handle_shop_a_button(view: &mut MapView, item_display_names: &HashMap<String, String>) {
    let Some(shop) = &mut view.shop else {
        return;
    };
    if shop.items.is_empty() {
        return;
    }
    let idx = shop.selection.min(shop.items.len() - 1);
    let it = &shop.items[idx];
    if it.price == 0 {
        open_dialog_from_lines(view, vec!["Not for sale.".to_string(), String::new()]);
        return;
    }
    if view.money < it.price {
        open_dialog_from_lines(view, vec!["Not enough money.".to_string(), String::new()]);
        return;
    }

    view.money -= it.price;
    *view.inventory.entry(it.item_id.clone()).or_insert(0) += 1;
    let display_name = item_display_names
        .get(&it.item_id)
        .cloned()
        .unwrap_or_else(|| fallback_item_display_name(&it.item_id));
    open_dialog_from_lines(view, vec![format!("Bought {display_name}!"), String::new()]);
}

fn handle_pc_a_button(
    view: &mut MapView,
    item_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
) {
    let Some(mut pc) = view.pc.take() else {
        return;
    };

    match pc.screen {
        PcScreen::Root => match pc.location {
            PcLocation::PokemonCenter => match pc.selection {
                0 => {
                    pc.screen = PcScreen::BillsPc;
                    pc.selection = 0;
                    pc.scroll = 0;
                    view.pc = Some(pc);
                }
                1 => {
                    pc.screen = PcScreen::PlayersPc;
                    pc.selection = 0;
                    pc.scroll = 0;
                    view.pc = Some(pc);
                }
                2 => {
                    view.pc = Some(pc);
                    open_dialog_from_lines(
                        view,
                        vec!["PROF.OAK'S PC".to_string(), "Not implemented.".to_string()],
                    );
                }
                _ => {
                    view.pc = None;
                    open_dialog_from_lines(view, vec!["Logged off.".to_string(), String::new()]);
                }
            },
            PcLocation::RedsHouse => match pc.selection {
                0 => {
                    pc.screen = PcScreen::WithdrawItem;
                    pc.selection = 0;
                    pc.scroll = 0;
                    view.pc = Some(pc);
                }
                1 => {
                    pc.screen = PcScreen::DepositItem;
                    pc.selection = 0;
                    pc.scroll = 0;
                    view.pc = Some(pc);
                }
                2 => {
                    pc.screen = PcScreen::TossItem;
                    pc.selection = 0;
                    pc.scroll = 0;
                    view.pc = Some(pc);
                }
                _ => {
                    view.pc = None;
                    open_dialog_from_lines(view, vec!["Logged off.".to_string(), String::new()]);
                }
            },
        },
        PcScreen::BillsPc => match pc.selection {
            0 => {
                pc.screen = PcScreen::Withdraw;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
            }
            1 => {
                pc.screen = PcScreen::Deposit;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
            }
            2 => {
                pc.screen = PcScreen::Release;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
            }
            3 => {
                pc.screen = PcScreen::ChangeBox;
                pc.selection = view
                    .pc_current_box
                    .min(view.pc_boxes.len().saturating_sub(1));
                pc.scroll = 0;
                view.pc = Some(pc);
            }
            _ => {
                pc.screen = PcScreen::Root;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
            }
        },
        PcScreen::Withdraw => {
            let box_i = view
                .pc_current_box
                .min(view.pc_boxes.len().saturating_sub(1));
            let box_len = view.pc_boxes.get(box_i).map(|b| b.len()).unwrap_or(0);
            if pc.selection >= box_len {
                pc.screen = PcScreen::BillsPc;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
                return;
            }
            if view.party.len() >= 6 {
                view.pc = Some(pc);
                open_dialog_from_lines(
                    view,
                    vec!["Your party is full.".to_string(), String::new()],
                );
                return;
            }

            let mon = {
                let box_ref = &mut view.pc_boxes[box_i];
                box_ref.remove(pc.selection)
            };
            let name = pokemon_display_names
                .get(&mon.species)
                .cloned()
                .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));
            view.party.push(mon);
            view.pc = Some(pc);
            open_dialog_from_lines(view, vec![format!("Withdrew {name}."), String::new()]);
        }
        PcScreen::Deposit => {
            if pc.selection >= view.party.len() {
                pc.screen = PcScreen::BillsPc;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
                return;
            }
            if view.party.len() <= 1 {
                view.pc = Some(pc);
                open_dialog_from_lines(
                    view,
                    vec![
                        "Can't deposit your".to_string(),
                        "last Pokémon.".to_string(),
                    ],
                );
                return;
            }
            let box_i = view
                .pc_current_box
                .min(view.pc_boxes.len().saturating_sub(1));
            let box_len = view.pc_boxes.get(box_i).map(|b| b.len()).unwrap_or(0);
            if box_len >= 20 {
                view.pc = Some(pc);
                open_dialog_from_lines(view, vec!["BOX is full.".to_string(), String::new()]);
                return;
            }

            let mon = view.party.remove(pc.selection);
            let name = pokemon_display_names
                .get(&mon.species)
                .cloned()
                .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));
            view.pc_boxes[box_i].push(mon);
            view.pc = Some(pc);
            open_dialog_from_lines(view, vec![format!("Deposited {name}."), String::new()]);
        }
        PcScreen::Release => {
            let box_i = view
                .pc_current_box
                .min(view.pc_boxes.len().saturating_sub(1));
            let box_len = view.pc_boxes.get(box_i).map(|b| b.len()).unwrap_or(0);
            if pc.selection >= box_len {
                pc.screen = PcScreen::BillsPc;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
                return;
            }

            let mon = view.pc_boxes[box_i].get(pc.selection);
            let name = mon
                .map(|m| {
                    pokemon_display_names
                        .get(&m.species)
                        .cloned()
                        .unwrap_or_else(|| fallback_pokemon_display_name(&m.species))
                })
                .unwrap_or_else(|| "POKéMON".to_string());

            view.dialog = Some(Dialog {
                lines: wrap_dialog_lines(vec![format!("Release {name}?"), String::new()], 18),
                cursor: 0,
            });
            view.choice = Some(ChoiceState {
                kind: ChoiceKind::ReleaseBoxMon {
                    box_index: box_i,
                    index: pc.selection,
                },
                selection: 1,
            });
            view.pc = Some(pc);
        }
        PcScreen::ChangeBox => {
            let box_count = view.pc_boxes.len().max(1);
            if pc.selection >= box_count {
                pc.screen = PcScreen::BillsPc;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
                return;
            }
            view.pc_current_box = pc.selection;
            pc.screen = PcScreen::BillsPc;
            pc.selection = 0;
            pc.scroll = 0;
            view.pc = Some(pc);
            open_dialog_from_lines(
                view,
                vec![
                    format!("Switched to BOX {}.", view.pc_current_box + 1),
                    String::new(),
                ],
            );
        }
        PcScreen::PlayersPc => match pc.selection {
            0 => {
                pc.screen = PcScreen::WithdrawItem;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
            }
            1 => {
                pc.screen = PcScreen::DepositItem;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
            }
            2 => {
                pc.screen = PcScreen::TossItem;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
            }
            _ => {
                pc.screen = PcScreen::Root;
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
            }
        },
        PcScreen::WithdrawItem => {
            let ids = inventory_item_ids_sorted_map(&view.pc_items);
            if pc.selection >= ids.len() {
                pc.screen = match pc.location {
                    PcLocation::PokemonCenter => PcScreen::PlayersPc,
                    PcLocation::RedsHouse => PcScreen::Root,
                };
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
                return;
            }

            let item_id = ids[pc.selection].clone();
            consume_item(&mut view.pc_items, &item_id, 1);
            *view.inventory.entry(item_id.clone()).or_insert(0) += 1;

            let name = item_display_names
                .get(&item_id)
                .cloned()
                .unwrap_or_else(|| fallback_item_display_name(&item_id));

            let len = view.pc_items.len();
            pc.selection = pc.selection.min(len);
            pc.scroll = pc.scroll.min(len.saturating_sub(ITEMS_VISIBLE_ROWS));
            view.pc = Some(pc);
            open_dialog_from_lines(view, vec![format!("Withdrew {name}."), String::new()]);
        }
        PcScreen::DepositItem => {
            let ids = inventory_item_ids_sorted_map(&view.inventory);
            if pc.selection >= ids.len() {
                pc.screen = match pc.location {
                    PcLocation::PokemonCenter => PcScreen::PlayersPc,
                    PcLocation::RedsHouse => PcScreen::Root,
                };
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
                return;
            }

            let item_id = ids[pc.selection].clone();
            consume_item(&mut view.inventory, &item_id, 1);
            *view.pc_items.entry(item_id.clone()).or_insert(0) += 1;

            let name = item_display_names
                .get(&item_id)
                .cloned()
                .unwrap_or_else(|| fallback_item_display_name(&item_id));

            let len = view.inventory.len();
            pc.selection = pc.selection.min(len);
            pc.scroll = pc.scroll.min(len.saturating_sub(ITEMS_VISIBLE_ROWS));
            view.pc = Some(pc);
            open_dialog_from_lines(view, vec![format!("Deposited {name}."), String::new()]);
        }
        PcScreen::TossItem => {
            let ids = inventory_item_ids_sorted_map(&view.pc_items);
            if pc.selection >= ids.len() {
                pc.screen = match pc.location {
                    PcLocation::PokemonCenter => PcScreen::PlayersPc,
                    PcLocation::RedsHouse => PcScreen::Root,
                };
                pc.selection = 0;
                pc.scroll = 0;
                view.pc = Some(pc);
                return;
            }

            let item_id = ids[pc.selection].clone();
            let name = item_display_names
                .get(&item_id)
                .cloned()
                .unwrap_or_else(|| fallback_item_display_name(&item_id));
            view.dialog = Some(Dialog {
                lines: wrap_dialog_lines(vec![format!("Toss {name}?"), String::new()], 18),
                cursor: 0,
            });
            view.choice = Some(ChoiceState {
                kind: ChoiceKind::TossPcItem { item_id },
                selection: 1,
            });
            view.pc = Some(pc);
        }
    }
}

fn handle_choice_a_button(
    view: &mut MapView,
    item_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
    pokemon_stats: &HashMap<String, BaseStats>,
    pokemon_moves: &HashMap<String, PokemonMoves>,
) -> Result<(), Box<dyn Error>> {
    if let Some(dialog) = &mut view.dialog {
        if dialog.has_more() {
            dialog.advance();
            return Ok(());
        }
    }
    let Some(choice) = view.choice.take() else {
        return Ok(());
    };
    let confirmed_yes = choice.selection == 0;
    view.dialog = None;

    if !confirmed_yes {
        return Ok(());
    }

    match choice.kind {
        ChoiceKind::Starter { species } => {
            if view.party.len() >= 6 {
                open_dialog_from_lines(
                    view,
                    vec!["Your party is full.".to_string(), String::new()],
                );
                return Ok(());
            }

            let mon = make_player_mon(
                &species,
                5,
                pokemon_stats,
                pokemon_moves,
            )?;
            view.party.push(mon);
            view.events.insert("EVENT_GOT_STARTER".to_string());
            set_oaks_lab_starters(view, &species);

            if view.map_name == "OaksLab" {
                let ids = [
                    "TEXT_OAKSLAB_CHARMANDER_POKE_BALL",
                    "TEXT_OAKSLAB_SQUIRTLE_POKE_BALL",
                    "TEXT_OAKSLAB_BULBASAUR_POKE_BALL",
                ];
                view.object_events
                    .retain(|o| !ids.contains(&o.text_id.as_str()));
            }

            let name = pokemon_display_names
                .get(&species)
                .cloned()
                .unwrap_or_else(|| fallback_pokemon_display_name(&species));
            open_dialog_from_lines(
                view,
                vec![
                    "This POKéMON is".to_string(),
                    "really energetic!".to_string(),
                    "PLAYER received".to_string(),
                    format!("a {name}!"),
                ],
            );
        }
        ChoiceKind::ReleaseBoxMon { box_index, index } => {
            let Some(box_ref) = view.pc_boxes.get_mut(box_index) else {
                open_dialog_from_lines(view, vec!["Nothing happened.".to_string(), String::new()]);
                return Ok(());
            };
            if index >= box_ref.len() {
                open_dialog_from_lines(view, vec!["Nothing happened.".to_string(), String::new()]);
                return Ok(());
            }
            let mon = box_ref.remove(index);
            let name = pokemon_display_names
                .get(&mon.species)
                .cloned()
                .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));
            open_dialog_from_lines(view, vec![format!("Released {name}."), String::new()]);

            if let Some(pc) = view.pc.as_mut() {
                if pc.screen == PcScreen::Release && view.pc_current_box == box_index {
                    let box_len = view.pc_boxes[box_index].len();
                    pc.selection = pc.selection.min(box_len);
                    pc.scroll = pc.scroll.min(box_len.saturating_sub(ITEMS_VISIBLE_ROWS));
                }
            }
        }
        ChoiceKind::TossPcItem { item_id } => {
            if !view.pc_items.contains_key(&item_id) {
                open_dialog_from_lines(view, vec!["Nothing happened.".to_string(), String::new()]);
                return Ok(());
            }
            consume_item(&mut view.pc_items, &item_id, 1);
            let name = item_display_names
                .get(&item_id)
                .cloned()
                .unwrap_or_else(|| fallback_item_display_name(&item_id));
            open_dialog_from_lines(view, vec![format!("Threw away {name}."), String::new()]);

            if let Some(pc) = view.pc.as_mut() {
                if pc.screen == PcScreen::TossItem {
                    let item_len = view.pc_items.len();
                    pc.selection = pc.selection.min(item_len);
                    pc.scroll = pc.scroll.min(item_len.saturating_sub(ITEMS_VISIBLE_ROWS));
                }
            }
        }
    }

    Ok(())
}

fn handle_menu_a_button(
    view: &mut MapView,
    pokered_root: &Path,
    item_display_names: &HashMap<String, String>,
    pokemon_display_names: &HashMap<String, String>,
    move_display_names: &HashMap<String, String>,
) -> Result<(), Box<dyn Error>> {
    let Some(mut menu) = view.menu.take() else {
        return Ok(());
    };

    match menu.screen {
        MenuScreen::Root => match menu.selection {
            0 => {
                menu.screen = MenuScreen::Pokedex { cursor: 0, scroll: 0 };
                menu.selection = 0;
                menu.scroll = 0;
                view.menu = Some(menu);
            }
            1 => {
                menu.screen = MenuScreen::Party;
                menu.selection = 0;
                menu.scroll = 0;
                view.menu = Some(menu);
            }
            2 => {
                menu.screen = MenuScreen::Items;
                menu.selection = 0;
                menu.scroll = 0;
                view.menu = Some(menu);
            }
            3 => {
                if let Err(err) = save_game(view) {
                    open_dialog_from_lines(view, vec![format!("Save failed."), format!("{err}")]);
                } else {
                    open_dialog_from_lines(view, vec!["Saved.".to_string(), String::new()]);
                }
            }
            4 => {
                menu.screen = MenuScreen::Options;
                menu.selection = 0;
                menu.scroll = 0;
                view.menu = Some(menu);
            }
            5 => {}
            _ => {
                view.menu = Some(menu);
                open_dialog_from_lines(view, vec!["Not implemented.".to_string(), String::new()]);
            }
        },
        MenuScreen::Items => {
            let ids = inventory_item_ids_sorted(view);
            if ids.is_empty() {
                view.menu = Some(menu);
                open_dialog_from_lines(view, vec!["No items.".to_string(), String::new()]);
            } else {
                let idx = menu.selection.min(ids.len() - 1);
                let item_id = ids[idx].clone();
                let display_name = item_display_names
                    .get(&item_id)
                    .cloned()
                    .unwrap_or_else(|| fallback_item_display_name(&item_id));

                // Check if this is a healing item
                if matches!(
                    item_id.as_str(),
                    "POTION" | "SUPER_POTION" | "HYPER_POTION" | "MAX_POTION" | "FRESH_WATER" | "SODA_POP" | "LEMONADE"
                ) {
                    let heal_amount: u16 = match item_id.as_str() {
                        "POTION" => 20,
                        "SUPER_POTION" => 50,
                        "HYPER_POTION" => 200,
                        "MAX_POTION" => 999,
                        "FRESH_WATER" => 50,
                        "SODA_POP" => 60,
                        "LEMONADE" => 80,
                        _ => 0,
                    };

                    // Find first injured Pokemon
                    if let Some(mon_idx) = view.party.iter().position(|m| m.hp < m.max_hp && m.hp > 0) {
                        let mon = &mut view.party[mon_idx];
                        let old_hp = mon.hp;
                        mon.hp = (mon.hp + heal_amount).min(mon.max_hp);
                        let healed = mon.hp - old_hp;

                        consume_item(&mut view.inventory, &item_id, 1);

                        let pokemon_name = pokemon_display_names
                            .get(&mon.species)
                            .cloned()
                            .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));

                        view.menu = Some(menu);
                        open_dialog_from_lines(
                            view,
                            vec![
                                format!("Used {display_name}!"),
                                format!("{pokemon_name} recovered {healed} HP!"),
                            ],
                        );
                    } else {
                        view.menu = Some(menu);
                        open_dialog_from_lines(
                            view,
                            vec!["All Pokémon are already healthy!".to_string(), String::new()],
                        );
                    }
                } else if matches!(item_id.as_str(), "POKE_BALL" | "GREAT_BALL" | "ULTRA_BALL" | "MASTER_BALL") {
                    view.menu = Some(menu);
                    open_dialog_from_lines(
                        view,
                        vec!["Cannot use that here.".to_string(), String::new()],
                    );
                } else if item_id.starts_with("TM_") || item_id.starts_with("HM_") {
                    // TM/HM teaching - show Pokemon selection screen
                    menu.screen = MenuScreen::TeachMove { item_id: item_id.clone() };
                    menu.selection = 0;
                    menu.scroll = 0;
                    view.menu = Some(menu);
                } else {
                    view.menu = Some(menu);
                    open_dialog_from_lines(
                        view,
                        vec![display_name, "Use not implemented yet.".to_string()],
                    );
                }
            }
        }
        MenuScreen::Party => {
            if view.party.is_empty() {
                view.menu = Some(menu);
                open_dialog_from_lines(view, vec!["No Pokémon.".to_string(), String::new()]);
            } else {
                let idx = menu.selection.min(view.party.len() - 1);
                let mon = &view.party[idx];
                let name = pokemon_display_names
                    .get(&mon.species)
                    .cloned()
                    .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));
                let hp_line = format!("HP {}/{}", mon.hp, mon.max_hp);
                let mut move_lines: Vec<String> = Vec::new();
                for mov in mon.moves.iter().take(2) {
                    let disp = move_display_names
                        .get(mov)
                        .cloned()
                        .unwrap_or_else(|| fallback_item_display_name(mov));
                    move_lines.push(disp);
                }
                let mut lines = vec![format!("{name} L{}", mon.level), hp_line];
                if let Some(first) = move_lines.get(0).cloned() {
                    lines.push(first);
                }
                if let Some(second) = move_lines.get(1).cloned() {
                    lines.push(second);
                }
                view.menu = Some(menu);
                open_dialog_from_lines(view, lines);
            }
        }
        MenuScreen::Options => {
            // Options: Text Speed, Battle Animations, Sound
            let option_index = menu.selection;
            match option_index {
                0 => {
                    // Cycle text speed
                    view.options.text_speed = match view.options.text_speed {
                        TextSpeed::Fast => TextSpeed::Medium,
                        TextSpeed::Medium => TextSpeed::Slow,
                        TextSpeed::Slow => TextSpeed::Fast,
                    };
                }
                1 => {
                    // Toggle battle animations
                    view.options.battle_animations = !view.options.battle_animations;
                }
                2 => {
                    // Toggle sound
                    view.options.sound_enabled = !view.options.sound_enabled;
                }
                _ => {}
            }
            view.menu = Some(menu);
        }
        MenuScreen::Pokedex { .. } => {
            // For now, just keep the menu open, no action on selection
            view.menu = Some(menu);
        }
        MenuScreen::TeachMove { ref item_id } => {
            // TM/HM teaching - Pokemon selection
            if view.party.is_empty() {
                view.menu = Some(menu);
                open_dialog_from_lines(view, vec!["No Pokémon.".to_string(), String::new()]);
            } else {
                let idx = menu.selection.min(view.party.len() - 1);

                // Extract move name from item_id (e.g., "TM_MEGA_PUNCH" -> "MEGA_PUNCH")
                let move_id = if item_id.starts_with("TM_") {
                    item_id.strip_prefix("TM_").unwrap_or("").to_string()
                } else if item_id.starts_with("HM_") {
                    item_id.strip_prefix("HM_").unwrap_or("").to_string()
                } else {
                    item_id.clone()
                };

                let move_name = move_display_names
                    .get(&move_id)
                    .cloned()
                    .unwrap_or_else(|| fallback_item_display_name(&move_id));

                let mon = &mut view.party[idx];

                // Check if Pokemon already knows the move
                if mon.moves.contains(&move_id) {
                    let pokemon_name = pokemon_display_names
                        .get(&mon.species)
                        .cloned()
                        .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));
                    view.menu = Some(menu);
                    open_dialog_from_lines(
                        view,
                        vec![
                            format!("{pokemon_name} already"),
                            format!("knows {move_name}."),
                        ],
                    );
                } else if mon.moves.len() < 4 {
                    // Can learn the move directly
                    mon.moves.push(move_id.clone());

                    // Consume TMs but not HMs
                    let is_tm = item_id.starts_with("TM_");
                    if is_tm {
                        consume_item(&mut view.inventory, item_id, 1);
                    }

                    let pokemon_name = pokemon_display_names
                        .get(&mon.species)
                        .cloned()
                        .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));

                    menu.screen = MenuScreen::Items;
                    menu.selection = 0;
                    menu.scroll = 0;
                    view.menu = Some(menu);
                    open_dialog_from_lines(
                        view,
                        vec![
                            format!("{pokemon_name} learned"),
                            format!("{move_name}!"),
                        ],
                    );
                } else {
                    // Pokemon knows 4 moves - would need to choose which to forget
                    // For now, just show a message
                    let pokemon_name = pokemon_display_names
                        .get(&mon.species)
                        .cloned()
                        .unwrap_or_else(|| fallback_pokemon_display_name(&mon.species));
                    view.menu = Some(menu);
                    open_dialog_from_lines(
                        view,
                        vec![
                            format!("{pokemon_name} already"),
                            "knows 4 moves.".to_string(),
                        ],
                    );
                }
            }
        }
    }

    let _ = pokered_root;
    Ok(())
}

fn handle_a_button(
    view: &mut MapView,
    pokered_root: &Path,
    game_data: &game_data::GameData,
) -> Result<(), Box<dyn Error>> {
    if view.choice.is_some() {
        handle_choice_a_button(
            view,
            &game_data.item_display_names,
            &game_data.pokemon_display_names,
            &game_data.pokemon_stats,
            &game_data.pokemon_moves,
        )?;
        return Ok(());
    }

    if let Some(dialog) = &mut view.dialog {
        if dialog.advance() {
            view.dialog = None;
        }
        return Ok(());
    }

    if view.shop.is_some() {
        handle_shop_a_button(view, &game_data.item_display_names);
        return Ok(());
    }

    if view.pc.is_some() {
        handle_pc_a_button(view, &game_data.item_display_names, &game_data.pokemon_display_names);
        return Ok(());
    }

    if view.menu.is_some() {
        return handle_menu_a_button(
            view,
            pokered_root,
            &game_data.item_display_names,
            &game_data.pokemon_display_names,
            &game_data.move_display_names,
        );
    }

    if view.player.move_anim.is_some() {
        return Ok(());
    }

    let (dx, dy) = facing_to_dir_tiles(view.player.facing);
    let target_tx = view.player.tx + dx;
    let target_ty = view.player.ty + dy;

    // Check immediate target tile for NPC
    // If not found and there's a counter between player and far target, check across the counter
    // (Match original Pokemon Red: counter tiles extend talking range - see overworld.asm)
    // Note: counter tiles may be at odd offsets, so we check all tiles between player and far target
    let obj_i = view
        .object_events
        .iter()
        .position(|o| o.tx == target_tx && o.ty == target_ty)
        .or_else(|| {
            // Check if any tile between player and far target is a counter tile
            // Player moves in 2-tile steps, but counter tiles can be at any position
            let far_tx = target_tx + dx;
            let far_ty = target_ty + dy;
            let has_counter = has_counter_between(view, view.player.tx, view.player.ty, far_tx, far_ty);
            if has_counter {
                view.object_events
                    .iter()
                    .position(|o| o.tx == far_tx && o.ty == far_ty)
            } else {
                None
            }
        });

    if let Some(obj_i) = obj_i {
        match view.object_events[obj_i].kind.clone() {
            ObjectKind::Item { item_id } => {
                let text_id = view.object_events[obj_i].text_id.clone();
                view.picked_up_items
                    .insert((view.map_name.clone(), text_id));
                *view.inventory.entry(item_id.clone()).or_insert(0) += 1;
                view.object_events.remove(obj_i);

                let display_name = game_data
                    .item_display_names
                    .get(&item_id)
                    .cloned()
                    .unwrap_or_else(|| fallback_item_display_name(&item_id));
                open_dialog_from_lines(view, vec![format!("Found {display_name}!"), String::new()]);
            }
            ObjectKind::Trainer {
                opponent,
                trainer_number,
            } => {
                let text_id = view.object_events[obj_i].text_id.clone();
                view.object_events[obj_i].facing = facing_opposite(view.player.facing);
                view.object_events[obj_i].move_anim = None;
                view.object_events[obj_i].walk_intra_counter = 0;
                view.object_events[obj_i].walk_anim_frame = 0;
                if view.map_name == "OaksLab" && text_id == "TEXT_OAKSLAB_RIVAL" {
                    run_oaks_lab_rival_interaction(view)?;
                    return Ok(());
                }
                if view
                    .defeated_trainers
                    .contains(&(view.map_name.clone(), text_id.clone()))
                {
                    open_dialog_from_lines(
                        view,
                        vec![format!("[{text_id}]"), "...".to_string(), String::new()],
                    );
                } else {
                    view.pending_battle = Some(PendingBattle::Trainer {
                        map_name: view.map_name.clone(),
                        text_id: text_id.clone(),
                        opponent,
                        trainer_number,
                    });
                    open_dialog_from_lines(
                        view,
                        vec![
                            "Trainer wants to battle!".to_string(),
                            format!("[{text_id}]"),
                        ],
                    );
                }
            }
            ObjectKind::Person => {
                let text_id = view.object_events[obj_i].text_id.clone();
                let sprite_id = view.object_events[obj_i].sprite_id;
                view.object_events[obj_i].facing = facing_opposite(view.player.facing);
                view.object_events[obj_i].move_anim = None;
                view.object_events[obj_i].walk_intra_counter = 0;
                view.object_events[obj_i].walk_anim_frame = 0;

                if sprite_id == data::sprite_constants::SPRITE_NURSE {
                    run_pokecenter_nurse(view)?;
                    return Ok(());
                }
                if sprite_id == data::sprite_constants::SPRITE_CLERK
                    && (view.map_name.ends_with("Mart") || view.map_name == "IndigoPlateauLobby")
                {
                    if view.map_name == "ViridianMart" && text_id == "TEXT_VIRIDIANMART_CLERK" {
                        run_viridian_mart_clerk(view, game_data)?;
                        return Ok(());
                    }
                    let item_ids = mart_item_ids_for_map(&view.map_name);
                    open_mart_shop(
                        view,
                        item_ids,
                        &game_data.item_display_names,
                        &game_data.item_prices,
                        &game_data.tm_prices,
                    );
                    return Ok(());
                }
                open_dialog_for_text_id(
                    view,
                    game_data,
                    &text_id,
                )?;
            }
        }
        return Ok(());
    }

    if let Some(text_id) = view
        .bg_events
        .iter()
        .find(|e| e.tx == target_tx && e.ty == target_ty)
        .map(|e| e.text_id.clone())
    {
        open_dialog_for_text_id(
            view,
            game_data,
            &text_id,
        )?;
        return Ok(());
    }

    if let Some(hidden) = view.hidden_events.iter().find(|e| {
        e.tx == target_tx && e.ty == target_ty && e.facing.map_or(true, |f| f == view.player.facing)
    }) {
        match &hidden.action {
            HiddenAction::OpenPokemonCenterPC => {
                open_pc(view, PcLocation::PokemonCenter);
            }
            HiddenAction::OpenRedsPC => {
                open_pc(view, PcLocation::RedsHouse);
            }
            HiddenAction::Other(action) => {
                open_dialog_from_lines(view, vec![format!("[Hidden: {action}]"), String::new()]);
            }
        }
        return Ok(());
    }

    Ok(())
}

fn fallback_item_display_name(item_id: &str) -> String {
    item_id.replace('_', " ")
}

fn fallback_pokemon_display_name(species: &str) -> String {
    match species {
        "MR_MIME" => "MR. MIME".to_string(),
        _ => species.replace('_', " "),
    }
}

fn open_dialog_for_text_id(
    view: &mut MapView,
    game_data: &game_data::GameData,
    text_id: &str,
) -> Result<(), Box<dyn Error>> {
    if view.map_name == "BluesHouse" {
        match text_id {
            "TEXT_BLUESHOUSE_DAISY_SITTING" => {
                return run_blues_house_daisy_sitting(view);
            }
            "TEXT_BLUESHOUSE_DAISY_WALKING" => {
                return run_blues_house_daisy_walking(view);
            }
            "TEXT_BLUESHOUSE_TOWN_MAP" => {
                return run_blues_house_town_map(view);
            }
            _ => {}
        }
    }
    if view.map_name == "OaksLab" {
        match text_id {
            "TEXT_OAKSLAB_CHARMANDER_POKE_BALL" => {
                return run_oaks_lab_starter_ball(
                    view,
                    "CHARMANDER",
                );
            }
            "TEXT_OAKSLAB_SQUIRTLE_POKE_BALL" => {
                return run_oaks_lab_starter_ball(
                    view,
                    "SQUIRTLE",
                );
            }
            "TEXT_OAKSLAB_BULBASAUR_POKE_BALL" => {
                return run_oaks_lab_starter_ball(
                    view,
                    "BULBASAUR",
                );
            }
            "TEXT_OAKSLAB_OAK1" | "TEXT_OAKSLAB_OAK2" => {
                if run_oaks_lab_parcel_delivery(view)? {
                    return Ok(());
                }
                if run_oaks_lab_give_pokeballs(view)? {
                    return Ok(());
                }
                if !view.events.contains("EVENT_GOT_STARTER") {
                    view.events
                        .insert("EVENT_OAK_ASKED_TO_CHOOSE_MON".to_string());
                    open_dialog_from_lines(
                        view,
                        vec![
                            "OAK: Now, PLAYER,".to_string(),
                            "which POKéMON do you want?".to_string(),
                        ],
                    );
                    return Ok(());
                }
                run_oaks_lab_oak_post_starter(view)?;
                return Ok(());
            }
            _ => {}
        }
    }
    let _ = game_data;
    open_dialog_from_lines(view, vec![format!("[{text_id}]"), String::new()]);
    Ok(())
}

fn open_dialog_from_lines(view: &mut MapView, lines: Vec<String>) {
    let lines = wrap_dialog_lines(lines, 18);
    view.dialog = Some(Dialog { lines, cursor: 0 });
}

fn mart_item_ids_for_map(map_name: &str) -> Vec<String> {
    match map_name {
        "ViridianMart" => vec![
            "POKE_BALL".to_string(),
            "ANTIDOTE".to_string(),
            "PARLYZ_HEAL".to_string(),
            "BURN_HEAL".to_string(),
        ],
        _ => Vec::new(),
    }
}

fn shop_item_price(
    item_id: &str,
    item_display_names: &HashMap<String, String>,
    item_prices: &HashMap<String, u32>,
    tm_prices: &[u32],
) -> u32 {
    if let Some(tm_code) = item_display_names
        .get(item_id)
        .and_then(|s| s.strip_prefix("TM"))
    {
        if let Ok(n) = tm_code.parse::<usize>() {
            if n >= 1 && n <= tm_prices.len() {
                return tm_prices[n - 1];
            }
        }
    }
    item_prices.get(item_id).copied().unwrap_or(0)
}

fn open_mart_shop(
    view: &mut MapView,
    item_ids: Vec<String>,
    item_display_names: &HashMap<String, String>,
    item_prices: &HashMap<String, u32>,
    tm_prices: &[u32],
) {
    let mut items: Vec<ShopItem> = Vec::new();
    for item_id in item_ids {
        let price = shop_item_price(&item_id, item_display_names, item_prices, tm_prices);
        items.push(ShopItem { item_id, price });
    }
    view.menu = None;
    view.shop = Some(ShopState {
        items,
        selection: 0,
        scroll: 0,
    });
}

fn run_pokecenter_nurse(view: &mut MapView) -> Result<(), Box<dyn Error>> {
    for mon in &mut view.party {
        mon.hp = mon.max_hp;
    }
    let lines = vec![
        "Welcome to our POKéMON CENTER!".to_string(),
        "Your POKéMON were healed.".to_string(),
    ];
    open_dialog_from_lines(view, lines);
    Ok(())
}

fn run_viridian_mart_clerk(
    view: &mut MapView,
    game_data: &game_data::GameData,
) -> Result<(), Box<dyn Error>> {
    if view.events.contains("EVENT_OAK_GOT_PARCEL") {
        open_mart_shop(
            view,
            mart_item_ids_for_map("ViridianMart"),
            &game_data.item_display_names,
            &game_data.item_prices,
            &game_data.tm_prices,
        );
        return Ok(());
    }

    let has_parcel = view.events.contains("EVENT_GOT_OAKS_PARCEL")
        || view.inventory.get("OAKS_PARCEL").copied().unwrap_or(0) > 0;

    if has_parcel {
        open_dialog_from_lines(
            view,
            vec![
                "Okay! Say hi to".to_string(),
                "PROF.OAK for me!".to_string(),
            ],
        );
        return Ok(());
    }

    *view.inventory.entry("OAKS_PARCEL".to_string()).or_insert(0) += 1;
    view.events.insert("EVENT_GOT_OAKS_PARCEL".to_string());
    open_dialog_from_lines(view, vec!["Got OAK's PARCEL!".to_string(), String::new()]);
    Ok(())
}

fn run_blues_house_daisy_sitting(
    view: &mut MapView,
) -> Result<(), Box<dyn Error>> {
    if view.events.contains("EVENT_GOT_TOWN_MAP") {
        open_dialog_from_lines(
            view,
            vec![
                "Use the TOWN MAP".to_string(),
                "to find out where you are.".to_string(),
            ],
        );
        return Ok(());
    }

    if !view.events.contains("EVENT_GOT_POKEDEX") {
        open_dialog_from_lines(
            view,
            vec!["Hi PLAYER!".to_string(), "RIVAL is at the lab.".to_string()],
        );
        return Ok(());
    }

    *view.inventory.entry("TOWN_MAP".to_string()).or_insert(0) += 1;
    view.events.insert("EVENT_GOT_TOWN_MAP".to_string());
    view.object_events
        .retain(|o| o.text_id != "TEXT_BLUESHOUSE_TOWN_MAP");

    let lines = vec![
        "It's a TOWN MAP!".to_string(),
        String::new(),
        "PLAYER got a".to_string(),
        "TOWN MAP!".to_string(),
        String::new(),
        "Use the TOWN MAP".to_string(),
        "to find out where you are.".to_string(),
    ];
    open_dialog_from_lines(view, lines);
    Ok(())
}

fn run_blues_house_daisy_walking(
    view: &mut MapView,
) -> Result<(), Box<dyn Error>> {
    open_dialog_from_lines(
        view,
        vec![
            "POKéMON are living things!".to_string(),
            "If they get tired, rest.".to_string(),
        ],
    );
    Ok(())
}

fn run_blues_house_town_map(view: &mut MapView) -> Result<(), Box<dyn Error>> {
    open_dialog_from_lines(view, vec!["It's a big map!".to_string(), String::new()]);
    Ok(())
}

fn run_oaks_lab_parcel_delivery(
    view: &mut MapView,
) -> Result<bool, Box<dyn Error>> {
    if view.events.contains("EVENT_OAK_GOT_PARCEL") {
        return Ok(false);
    }

    let count = view.inventory.get("OAKS_PARCEL").copied().unwrap_or(0);
    if count == 0 {
        return Ok(false);
    }

    if count <= 1 {
        view.inventory.remove("OAKS_PARCEL");
    } else {
        view.inventory.insert("OAKS_PARCEL".to_string(), count - 1);
    }
    view.events.insert("EVENT_OAK_GOT_PARCEL".to_string());
    if !view.events.contains("EVENT_GOT_POKEDEX") {
        view.events
            .insert("EVENT_OAK_POKEDEX_SEQUENCE_PENDING".to_string());
    }

    let lines = vec![
        "PLAYER delivered".to_string(),
        "OAK's PARCEL.".to_string(),
        String::new(),
        "OAK: Thank you!".to_string(),
    ];
    open_dialog_from_lines(view, lines);
    Ok(true)
}

fn run_oaks_lab_give_pokeballs(
    view: &mut MapView,
) -> Result<bool, Box<dyn Error>> {
    if view.events.contains("EVENT_GOT_POKEBALLS_FROM_OAK") {
        return Ok(false);
    }
    if !view.events.contains("EVENT_GOT_POKEDEX") {
        return Ok(false);
    }
    if !view.events.contains("EVENT_BEAT_ROUTE22_RIVAL_1ST_BATTLE") {
        return Ok(false);
    }
    if view.inventory.get("POKE_BALL").copied().unwrap_or(0) > 0 {
        return Ok(false);
    }

    *view.inventory.entry("POKE_BALL".to_string()).or_insert(0) += 5;
    view.events
        .insert("EVENT_GOT_POKEBALLS_FROM_OAK".to_string());

    let lines = vec!["PLAYER got 5 POKé BALLs!".to_string(), String::new()];
    open_dialog_from_lines(view, lines);
    Ok(true)
}

fn run_oaks_lab_oak_post_starter(
    view: &mut MapView,
) -> Result<(), Box<dyn Error>> {
    let label = if !view.events.contains("EVENT_BATTLED_RIVAL_IN_OAKS_LAB") {
        "_OaksLabOak1YourPokemonCanFightText"
    } else if !view.events.contains("EVENT_GOT_POKEDEX") {
        "_OaksLabOak1RaiseYourYoungPokemonText"
    } else if view.inventory.get("POKE_BALL").copied().unwrap_or(0) > 0
        || view.events.contains("EVENT_GOT_POKEBALLS_FROM_OAK")
    {
        "_OaksLabOak1ComeSeeMeSometimesText"
    } else {
        "_OaksLabOak1PokemonAroundTheWorldText"
    };

    let lines = match label {
        "_OaksLabOak1YourPokemonCanFightText" => vec![
            "OAK: If a wild".to_string(),
            "POKéMON appears,".to_string(),
            "your POKéMON can".to_string(),
            "fight against it!".to_string(),
        ],
        "_OaksLabOak1RaiseYourYoungPokemonText" => vec![
            "OAK: PLAYER,".to_string(),
            "raise your young".to_string(),
            "POKéMON by making".to_string(),
            "it fight!".to_string(),
        ],
        "_OaksLabOak1ComeSeeMeSometimesText" => vec![
            "OAK: Come see me".to_string(),
            "sometimes.".to_string(),
            String::new(),
            "I want to know how".to_string(),
            "your POKéDEX is".to_string(),
            "coming along.".to_string(),
        ],
        _ => vec![
            "POKéMON around the".to_string(),
            "world wait for".to_string(),
            "you, PLAYER!".to_string(),
        ],
    };
    open_dialog_from_lines(view, lines);
    Ok(())
}

fn run_oaks_lab_starter_ball(
    view: &mut MapView,
    species: &str,
) -> Result<(), Box<dyn Error>> {
    if view.events.contains("EVENT_GOT_STARTER") {
        open_dialog_from_lines(
            view,
            vec!["That's PROF.OAK's".to_string(), "last POKéMON!".to_string()],
        );
        return Ok(());
    }

    if !view.events.contains("EVENT_OAK_ASKED_TO_CHOOSE_MON") {
        open_dialog_from_lines(view, vec!["Those are POKé".to_string(), "BALLs.".to_string()]);
        return Ok(());
    }

    let name = fallback_pokemon_display_name(species);
    open_dialog_from_lines(view, vec![format!("So! You want"), format!("{name}?")]);
    view.choice = Some(ChoiceState {
        kind: ChoiceKind::Starter {
            species: species.to_string(),
        },
        selection: 0,
    });
    Ok(())
}

fn set_oaks_lab_starters(view: &mut MapView, player_species: &str) {
    for ev in [
        "EVENT_PLAYER_STARTER_CHARMANDER",
        "EVENT_PLAYER_STARTER_SQUIRTLE",
        "EVENT_PLAYER_STARTER_BULBASAUR",
        "EVENT_RIVAL_STARTER_CHARMANDER",
        "EVENT_RIVAL_STARTER_SQUIRTLE",
        "EVENT_RIVAL_STARTER_BULBASAUR",
    ] {
        view.events.remove(ev);
    }

    match player_species {
        "CHARMANDER" => {
            view.events
                .insert("EVENT_PLAYER_STARTER_CHARMANDER".to_string());
            view.events
                .insert("EVENT_RIVAL_STARTER_SQUIRTLE".to_string());
        }
        "SQUIRTLE" => {
            view.events
                .insert("EVENT_PLAYER_STARTER_SQUIRTLE".to_string());
            view.events
                .insert("EVENT_RIVAL_STARTER_BULBASAUR".to_string());
        }
        "BULBASAUR" => {
            view.events
                .insert("EVENT_PLAYER_STARTER_BULBASAUR".to_string());
            view.events
                .insert("EVENT_RIVAL_STARTER_CHARMANDER".to_string());
        }
        _ => {}
    }
}

fn oaks_lab_rival_trainer_number(view: &MapView) -> Option<String> {
    if view.events.contains("EVENT_RIVAL_STARTER_SQUIRTLE") {
        return Some("1".to_string());
    }
    if view.events.contains("EVENT_RIVAL_STARTER_BULBASAUR") {
        return Some("2".to_string());
    }
    if view.events.contains("EVENT_RIVAL_STARTER_CHARMANDER") {
        return Some("3".to_string());
    }

    let starter = if view.events.contains("EVENT_PLAYER_STARTER_CHARMANDER") {
        Some("CHARMANDER")
    } else if view.events.contains("EVENT_PLAYER_STARTER_SQUIRTLE") {
        Some("SQUIRTLE")
    } else if view.events.contains("EVENT_PLAYER_STARTER_BULBASAUR") {
        Some("BULBASAUR")
    } else {
        view.party.first().map(|m| m.species.as_str())
    }?;

    match starter {
        "CHARMANDER" => Some("1".to_string()),
        "SQUIRTLE" => Some("2".to_string()),
        "BULBASAUR" => Some("3".to_string()),
        _ => None,
    }
}

fn route22_rival1_trainer_number(view: &MapView) -> Option<String> {
    if view.events.contains("EVENT_RIVAL_STARTER_SQUIRTLE") {
        return Some("4".to_string());
    }
    if view.events.contains("EVENT_RIVAL_STARTER_BULBASAUR") {
        return Some("5".to_string());
    }
    if view.events.contains("EVENT_RIVAL_STARTER_CHARMANDER") {
        return Some("6".to_string());
    }
    None
}

fn run_oaks_lab_rival_interaction(
    view: &mut MapView,
) -> Result<(), Box<dyn Error>> {
    let label = if view.events.contains("EVENT_BATTLED_RIVAL_IN_OAKS_LAB")
        || view
            .defeated_trainers
            .contains(&(view.map_name.clone(), "TEXT_OAKSLAB_RIVAL".to_string()))
    {
        "_OaksLabRivalSmellYouLaterText"
    } else if !view.events.contains("EVENT_OAK_ASKED_TO_CHOOSE_MON") {
        "_OaksLabRivalGrampsIsntAroundText"
    } else if !view.events.contains("EVENT_GOT_STARTER") {
        "_OaksLabRivalGoAheadAndChooseText"
    } else {
        "_OaksLabRivalMyPokemonLooksStrongerText"
    };

    let lines = match label {
        "_OaksLabRivalSmellYouLaterText" => vec!["SMELL YA LATER!".to_string(), String::new()],
        "_OaksLabRivalGrampsIsntAroundText" => vec!["RIVAL: Gramps".to_string(), "isn't around.".to_string()],
        "_OaksLabRivalGoAheadAndChooseText" => vec!["RIVAL: Go ahead,".to_string(), "pick one!".to_string()],
        _ => vec!["RIVAL: My POKéMON".to_string(), "looks stronger!".to_string()],
    };
    open_dialog_from_lines(view, lines);
    Ok(())
}

fn wrap_dialog_lines(lines: Vec<String>, max_width: usize) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for line in lines {
        let line = line.trim_end().to_string();
        if line.is_empty() {
            out.push(String::new());
            continue;
        }

        let mut current = String::new();
        for word in line.split_whitespace() {
            if current.is_empty() {
                current.push_str(word);
                continue;
            }
            let next_len = current.chars().count() + 1 + word.chars().count();
            if next_len <= max_width {
                current.push(' ');
                current.push_str(word);
            } else {
                out.push(current);
                current = word.to_string();
            }
        }

        while current.chars().count() > max_width {
            let head: String = current.chars().take(max_width).collect();
            out.push(head);
            current = current.chars().skip(max_width).collect();
        }

        if !current.is_empty() {
            out.push(current);
        }
    }
    out
}

fn facing_opposite(dir: Facing) -> Facing {
    match dir {
        Facing::Down => Facing::Up,
        Facing::Up => Facing::Down,
        Facing::Left => Facing::Right,
        Facing::Right => Facing::Left,
    }
}

fn try_warp_transition(
    view: &mut MapView,
    pokered_root: &Path,
    game_data: &game_data::GameData,
) -> Result<bool, Box<dyn Error>> {
    let (wx, wy) = player_coords_units(&view.player);
    let Some(warp) = view.warp_events.iter().find(|w| w.x == wx && w.y == wy) else {
        return Ok(false);
    };

    if view.map_name == "OaksLab"
        && matches!(warp.dest_map, WarpDest::LastMap)
        && !view.events.contains("EVENT_GOT_STARTER")
    {
        open_dialog_from_lines(
            view,
            vec![
                "OAK: Hey! Don't go away yet!".to_string(),
                String::new(),
            ],
        );
        return Ok(true);
    }

    let dest_map_name = match &warp.dest_map {
        WarpDest::LastMap => view
            .last_outside_map
            .clone()
            .ok_or_else(|| "Warp uses LAST_MAP but no last outside map is set".to_string())?,
        WarpDest::MapId(map_id) => game_data
            .map_id_to_name
            .get(map_id)
            .cloned()
            .ok_or_else(|| format!("Unknown destination map id `{map_id}`"))?,
    };

    let next_last_outside_map = if view.is_outside {
        Some(view.map_name.clone())
    } else {
        view.last_outside_map.clone()
    };

    let mut new_view = load_map_view(
        pokered_root,
        game_data,
        &dest_map_name,
        next_last_outside_map,
        &view.picked_up_items,
        &view.events,
    )?;
    place_player_on_warp_id(&mut new_view, warp.dest_warp_id)?;
    recenter_camera_on_player(
        &new_view.map,
        &mut new_view.camera_tx,
        &mut new_view.camera_ty,
        new_view.player.tx,
        new_view.player.ty,
    );
    new_view.rng = view.rng;
    new_view.money = view.money;
    new_view.events = view.events.clone();
    new_view.inventory = view.inventory.clone();
    new_view.picked_up_items = view.picked_up_items.clone();
    new_view.defeated_trainers = view.defeated_trainers.clone();
    new_view.party = view.party.clone();
    new_view.pc_boxes = view.pc_boxes.clone();
    new_view.pc_current_box = view.pc_current_box;
    *view = new_view;
    Ok(true)
}

fn try_connection_transition(
    view: &mut MapView,
    pokered_root: &Path,
    game_data: &game_data::GameData,
) -> Result<bool, Box<dyn Error>> {
    let (map_w_tiles, map_h_tiles) = map_dimensions_tiles(&view.map);
    let is_oob = view.player.tx < 0
        || view.player.ty < 0
        || view.player.tx >= map_w_tiles
        || view.player.ty >= map_h_tiles;
    if !is_oob {
        return Ok(false);
    }

    let Some((dir, conn)) = view.connections.connection_for_oob_coords(
        view.player.tx,
        view.player.ty,
        map_w_tiles,
        map_h_tiles,
    ) else {
        return Ok(false);
    };

    let (cur_wx, cur_wy) = player_coords_units(&view.player);
    let mut new_view = load_map_view(
        pokered_root,
        game_data,
        &conn.map_name,
        view.last_outside_map.clone(),
        &view.picked_up_items,
        &view.events,
    )?;

    let (dest_w_units, dest_h_units) = map_dimensions_units(&new_view.map);
    if dest_w_units <= 0 || dest_h_units <= 0 {
        return Err(format!("Connected map `{}` has invalid dimensions", conn.map_name).into());
    }

    let adjust_units = -2 * conn.offset_blocks;
    let (mut new_wx, mut new_wy) = match dir {
        Facing::Left => (dest_w_units - 1, cur_wy + adjust_units),
        Facing::Right => (0, cur_wy + adjust_units),
        Facing::Up => (cur_wx + adjust_units, dest_h_units - 1),
        Facing::Down => (cur_wx + adjust_units, 0),
    };

    new_wx = new_wx.clamp(0, dest_w_units - 1);
    new_wy = new_wy.clamp(0, dest_h_units - 1);

    new_view.player.tx = new_wx * PLAYER_STEP_TILES;
    new_view.player.ty = new_wy * PLAYER_STEP_TILES;
    new_view.player.facing = dir;
    new_view.player.move_anim = None;
    new_view.player.walk_intra_counter = 0;
    new_view.player.walk_anim_frame = 0;

    recenter_camera_on_player(
        &new_view.map,
        &mut new_view.camera_tx,
        &mut new_view.camera_ty,
        new_view.player.tx,
        new_view.player.ty,
    );
    new_view.rng = view.rng;
    new_view.money = view.money;
    new_view.events = view.events.clone();
    new_view.inventory = view.inventory.clone();
    new_view.picked_up_items = view.picked_up_items.clone();
    new_view.defeated_trainers = view.defeated_trainers.clone();
    new_view.party = view.party.clone();
    new_view.pc_boxes = view.pc_boxes.clone();
    new_view.pc_current_box = view.pc_current_box;
    *view = new_view;
    Ok(true)
}

fn place_player_on_warp_id(view: &mut MapView, warp_id: usize) -> Result<(), Box<dyn Error>> {
    let idx = warp_id
        .checked_sub(1)
        .ok_or_else(|| "Warp id must be >= 1".to_string())?;
    let warp = view.warp_events.get(idx).ok_or_else(|| {
        format!(
            "Destination warp id `{warp_id}` out of range for map `{}`",
            view.map_name
        )
    })?;

    view.player.tx = warp.x * PLAYER_STEP_TILES;
    view.player.ty = warp.y * PLAYER_STEP_TILES;
    view.player.facing = Facing::Down;
    view.player.move_anim = None;
    view.player.walk_intra_counter = 0;
    view.player.walk_anim_frame = 0;
    Ok(())
}

fn player_coords_units(player: &Player) -> (i32, i32) {
    (player.tx / PLAYER_STEP_TILES, player.ty / PLAYER_STEP_TILES)
}

fn try_start_player_move(view: &mut MapView, dir: Facing) {
    if view.player.move_anim.is_some() {
        return;
    }

    let (dx, dy) = facing_to_dir_tiles(dir);
    let dest_tx = view.player.tx + dx;
    let dest_ty = view.player.ty + dy;

    if view
        .object_events
        .iter()
        .any(|o| object_blocks_tile(o, dest_tx, dest_ty))
    {
        return;
    }

    let can_walk = is_area_passable(
        view.map.width_blocks,
        view.map.height_blocks,
        &view.map.blocks,
        &view.blockset,
        &view.passable_tiles,
        dest_tx,
        dest_ty,
    );
    if !can_walk {
        let (map_w_tiles, map_h_tiles) = map_dimensions_tiles(&view.map);
        let dest_in_bounds = dest_tx >= 0
            && dest_ty >= 0
            && dest_tx + PLAYER_W_TILES <= map_w_tiles
            && dest_ty + PLAYER_H_TILES <= map_h_tiles;
        if dest_in_bounds {
            return;
        }
        if view.connections.connection_for_exit(dir).is_none() {
            return;
        }
    }

    view.player.move_anim = Some(MoveAnim {
        dir,
        start_tx: view.player.tx,
        start_ty: view.player.ty,
        dest_tx,
        dest_ty,
        progress_px: PLAYER_SPEED_PX_PER_TICK,
    });
}

fn update_camera_follow(view: &mut MapView) {
    let player_tx = view.player.tx;
    let player_ty = view.player.ty;

    let min_tx = view.camera_tx + CAMERA_MARGIN_TILES_X;
    let max_tx =
        view.camera_tx + (SCREEN_TILES_W as i32 - CAMERA_MARGIN_TILES_X - PLAYER_W_TILES).max(0);
    if player_tx < min_tx {
        view.camera_tx -= PLAYER_STEP_TILES;
    } else if player_tx > max_tx {
        view.camera_tx += PLAYER_STEP_TILES;
    }

    let min_ty = view.camera_ty + CAMERA_MARGIN_TILES_Y;
    let max_ty =
        view.camera_ty + (SCREEN_TILES_H as i32 - CAMERA_MARGIN_TILES_Y - PLAYER_H_TILES).max(0);
    if player_ty < min_ty {
        view.camera_ty -= PLAYER_STEP_TILES;
    } else if player_ty > max_ty {
        view.camera_ty += PLAYER_STEP_TILES;
    }

    clamp_camera(&view.map, &mut view.camera_tx, &mut view.camera_ty);
}

fn facing_to_dir_tiles(dir: Facing) -> (i32, i32) {
    match dir {
        Facing::Down => (0, PLAYER_STEP_TILES),
        Facing::Up => (0, -PLAYER_STEP_TILES),
        Facing::Left => (-PLAYER_STEP_TILES, 0),
        Facing::Right => (PLAYER_STEP_TILES, 0),
    }
}

fn facing_to_dir_px(dir: Facing, amount_px: i32) -> (i32, i32) {
    match dir {
        Facing::Down => (0, amount_px),
        Facing::Up => (0, -amount_px),
        Facing::Left => (-amount_px, 0),
        Facing::Right => (amount_px, 0),
    }
}

fn map_dimensions_tiles(map: &MapData) -> (i32, i32) {
    (
        map.width_blocks as i32 * BLOCK_TILES as i32,
        map.height_blocks as i32 * BLOCK_TILES as i32,
    )
}

fn map_dimensions_units(map: &MapData) -> (i32, i32) {
    let (w_tiles, h_tiles) = map_dimensions_tiles(map);
    (w_tiles / PLAYER_STEP_TILES, h_tiles / PLAYER_STEP_TILES)
}

fn is_area_passable(
    width_blocks: u32,
    height_blocks: u32,
    blocks: &[u8],
    blockset: &Blockset,
    passable_tiles: &HashSet<u8>,
    tx: i32,
    ty: i32,
) -> bool {
    let tile_tx = tx;
    let tile_ty = ty + (PLAYER_H_TILES - 1);
    let Some(tile_id) = map_tile_id(
        width_blocks,
        height_blocks,
        blocks,
        blockset,
        tile_tx,
        tile_ty,
    ) else {
        return false;
    };
    passable_tiles.contains(&tile_id)
}

fn map_tile_id(
    width_blocks: u32,
    height_blocks: u32,
    blocks: &[u8],
    blockset: &Blockset,
    tx: i32,
    ty: i32,
) -> Option<u8> {
    if tx < 0 || ty < 0 {
        return None;
    }

    let tx = tx as u32;
    let ty = ty as u32;
    let map_w_tiles = width_blocks * BLOCK_TILES;
    let map_h_tiles = height_blocks * BLOCK_TILES;
    if tx >= map_w_tiles || ty >= map_h_tiles {
        return None;
    }

    let block_x = tx / BLOCK_TILES;
    let block_y = ty / BLOCK_TILES;
    if block_x >= width_blocks || block_y >= height_blocks {
        return None;
    }

    let block_index = (block_y * width_blocks + block_x) as usize;
    let block_id = *blocks.get(block_index)? as usize;
    let block = *blockset.blocks.get(block_id)?;
    let inner_x = (tx % BLOCK_TILES) as usize;
    let inner_y = (ty % BLOCK_TILES) as usize;
    Some(block[inner_y * (BLOCK_TILES as usize) + inner_x])
}

fn recenter_camera_on_player(
    map: &MapData,
    camera_tx: &mut i32,
    camera_ty: &mut i32,
    player_tx: i32,
    player_ty: i32,
) {
    let player_center_tx = player_tx + (PLAYER_W_TILES / 2);
    let player_center_ty = player_ty + (PLAYER_H_TILES / 2);
    *camera_tx = player_center_tx - (SCREEN_TILES_W as i32 / 2);
    *camera_ty = player_center_ty - (SCREEN_TILES_H as i32 / 2);
    clamp_camera(map, camera_tx, camera_ty);
}

fn clamp_camera(map: &MapData, camera_tx: &mut i32, camera_ty: &mut i32) {
    let map_w_tiles = (map.width_blocks * BLOCK_TILES) as i32;
    let map_h_tiles = (map.height_blocks * BLOCK_TILES) as i32;
    let max_tx = (map_w_tiles - SCREEN_TILES_W as i32).max(0);
    let max_ty = (map_h_tiles - SCREEN_TILES_H as i32).max(0);
    *camera_tx = (*camera_tx).clamp(0, max_tx);
    *camera_ty = (*camera_ty).clamp(0, max_ty);
}
