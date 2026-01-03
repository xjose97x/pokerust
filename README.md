# pokerust

Native (non-emulator) Rust reimplementation of Pokémon Red/Blue, using the local `../pokered` disassembly as the source-of-truth reference for behavior and assets.

## Current status

- Opens a window and renders a simple title-screen composition using `pokered/gfx/title/*.png`.
- Press `Enter` to show an overworld map renderer for `PalletTown` using `pokered/maps/*.blk` + `pokered/gfx/tilesets/*.png` + `pokered/gfx/blocksets/*.bst`.
- `Esc` / `Q` quits; `T` toggles the “version” banner (red/blue); `Backspace` returns to title; `L` loads `pokerust/savegame.txt` (if present).
- In map view: arrow keys move the player (with a simple 2‑frame walk animation + camera follow) using tileset passability data.
- In map view: `Z` is the A button (talk/read signs, advance/close text); `X` is the B button (close text / back out).
- In map view: `Enter` opens the Start menu (ITEM / SAVE implemented).
- Renders overworld NPC sprites from `pokered/gfx/sprites/*.png` (with basic `WALK`/`STAY` behavior and facing).
- Item balls can be picked up with `Z` (removed from the map and persisted across transitions).
- Trainer NPCs can start a basic battle (player is currently a placeholder Bulbasaur Lv5).
- Battle controls: arrow keys select, `Z` confirm/advance, `X` back/cancel; win marks trainer defeated + shows end-battle text.
- Save (`SAVE` in menu) also persists defeated trainers and picked-up items.
- PC: Bill’s PC (Pokémon storage) + Player’s PC (item storage) with deposit/withdraw/toss.
- Music: title / overworld / battle background music.
- Early-game scripting implemented (via targeted per-map logic, not a full script engine yet):
  - Pallet Town “Hey! Wait!” Oak escort to the lab when trying to leave without a starter.
  - Oak’s Lab starter selection (Yes/No) + first rival battle on exit.
  - Viridian Mart Oak’s Parcel quest + delivery to Oak.
  - Oak’s Lab Pokédex award sequence (simplified dialog).
  - Route 22 rival 1 scripted battle trigger + post-win state.
  - Daisy gives the Town Map in Blue’s House after getting the Pokédex.
  - Oak gives 5 Poké Balls after beating Route 22 rival 1 (if you have none).

## Run

```bash
cd pokerust
cargo run
```

Start directly in map view (by map name):

```bash
cargo run -- PalletTown
```

## Roadmap (high level)

1. **Platform layer**: window, input mapping, audio output, save-file location.
2. **Renderer**: GB-style 160×144 framebuffer, 8×8 tiles, sprites, palettes, text rendering.
3. **Data pipeline**: consume `pokered/` data (maps, text, pokemon data, move data, etc.) into Rust-friendly formats.
4. **Game core**: map engine + scripts, menus, battle system, audio engine; aim for deterministic “tick(input) -> state/frame/audio” updates.
5. **Compatibility**: save format compatibility and behavior tests (golden tests driven by known-good reference outputs).
