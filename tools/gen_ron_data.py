#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
from pathlib import Path
from typing import Any


PLAYER_STEP_TILES = 2


def strip_asm_comment(line: str) -> str:
    return line.split(";", 1)[0].strip()


def parse_asm_int(token: str) -> int:
    tok = token.strip()
    if not tok:
        raise ValueError("empty token")
    sign = 1
    if tok.startswith("-"):
        sign = -1
        tok = tok[1:].strip()
    if tok.startswith("$"):
        return sign * int(tok[1:], 16)
    if tok.startswith("%"):
        return sign * int(tok[1:], 2)
    return sign * int(tok, 10)


def ron_escape(s: str) -> str:
    return s.replace("\\", "\\\\").replace('"', '\\"')


def parse_sprite_constants_rs(path: Path) -> dict[str, int]:
    mapping: dict[str, int] = {}
    pat = re.compile(r"^pub const (SPRITE_[A-Z0-9_]+): u8 = 0x([0-9A-Fa-f]+);")
    for raw in path.read_text(encoding="utf-8").splitlines():
        m = pat.match(raw.strip())
        if not m:
            continue
        name = m.group(1)
        value = int(m.group(2), 16)
        mapping[name] = value
    if not mapping:
        raise RuntimeError(f"Failed to parse any sprite constants from {path}")
    return mapping


def sprite_png_for_const(root: Path, sprite_const: str) -> Path:
    if not sprite_const.startswith("SPRITE_"):
        raise ValueError(sprite_const)
    name = sprite_const[len("SPRITE_") :].lower()

    if name.startswith("unused_"):
        name = name[len("unused_") :]
    name = re.sub(r"_\d+$", "", name)

    path = root / "gfx" / "sprites" / f"{name}.png"
    if not path.exists():
        raise FileNotFoundError(f"Missing sprite PNG for {sprite_const}: {path}")
    return path


def write_ron_map(path: Path, entries: list[tuple[str, str]]) -> None:
    lines: list[str] = ["{"]
    for k, v in entries:
        lines.append(f'    "{ron_escape(k)}": {v},')
    lines.append("}")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def generate_tilesets_ron(data_dir: Path) -> None:
    tilesets = {
        "Overworld": "overworld",
        "RedsHouse1": "reds_house",
        "RedsHouse2": "reds_house",
        "House": "house",
        "Mansion": "mansion",
        "ShipPort": "ship_port",
        "Interior": "interior",
        "Plateau": "plateau",
        "Dojo": "gym",
        "Gym": "gym",
        "Mart": "pokecenter",
        "Pokecenter": "pokecenter",
        "ForestGate": "gate",
        "Museum": "gate",
        "Gate": "gate",
        "Forest": "forest",
        "Facility": "facility",
        "Cemetery": "cemetery",
        "Cavern": "cavern",
        "Lobby": "lobby",
        "Ship": "ship",
        "Lab": "lab",
        "Club": "club",
        "Underground": "underground",
    }
    write_ron_map(data_dir / "tilesets.ron", [(k, f'"{v}"') for k, v in sorted(tilesets.items())])


def generate_tileset_headers_ron(data_dir: Path) -> None:
    # Tall grass tile id values come from pokered's `data/tilesets/tileset_headers.asm`.
    grass: dict[str, Any] = {
        "Overworld": 0x52,
        "Forest": 0x20,
        "Plateau": 0x45,
    }
    tileset_symbols = [
        "Cavern",
        "Cemetery",
        "Club",
        "Dojo",
        "Facility",
        "Forest",
        "ForestGate",
        "Gate",
        "Gym",
        "House",
        "Interior",
        "Lab",
        "Lobby",
        "Mansion",
        "Mart",
        "Museum",
        "Overworld",
        "Plateau",
        "Pokecenter",
        "RedsHouse1",
        "RedsHouse2",
        "Ship",
        "ShipPort",
        "Underground",
    ]
    entries: list[tuple[str, str]] = []
    for label in tileset_symbols:
        if label in grass:
            entries.append((label, f"Some({grass[label]})"))
        else:
            entries.append((label, "None"))
    write_ron_map(data_dir / "tileset_headers.ron", entries)


def generate_sprite_mapping_ron(pokerust_root: Path, pokered_root: Path, data_dir: Path) -> None:
    sprite_consts = parse_sprite_constants_rs(
        pokerust_root / "src" / "data" / "sprite_constants.rs"
    )

    entries: list[tuple[int, str]] = []
    for const_name, sprite_id in sorted(sprite_consts.items(), key=lambda kv: kv[1]):
        if sprite_id == 0:
            continue
        png_path = sprite_png_for_const(pokered_root, const_name)
        rel = png_path.relative_to(pokered_root).as_posix()
        entries.append((sprite_id, f'"{ron_escape(rel)}"'))

    lines = ["{"]
    for k, v in entries:
        lines.append(f"    {k}: {v},")
    lines.append("}")
    (data_dir / "sprite_mapping.ron").write_text("\n".join(lines) + "\n", encoding="utf-8")


def parse_moves_asm(path: Path) -> dict[str, dict[str, Any]]:
    move_db: dict[str, dict[str, Any]] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = strip_asm_comment(raw)
        if not line.startswith("move "):
            continue
        args = line[len("move ") :].strip()
        parts = [p.strip() for p in args.split(",") if p.strip()]
        if len(parts) < 6:
            continue
        move_id = parts[0]
        power = parse_asm_int(parts[2])
        move_type = parts[3]
        accuracy = parse_asm_int(parts[4])
        move_db[move_id] = {"power": power, "move_type": move_type, "accuracy": accuracy}
    if not move_db:
        raise RuntimeError(f"No moves parsed from {path}")
    return move_db


def generate_moves_ron(pokered_root: Path, data_dir: Path) -> None:
    move_db = parse_moves_asm(pokered_root / "data" / "moves" / "moves.asm")

    lines: list[str] = ["{"]
    for move_id in sorted(move_db.keys()):
        d = move_db[move_id]
        lines.append(f'    "{ron_escape(move_id)}": (')
        lines.append(f"        power: {d['power']},")
        lines.append(f'        move_type: "{ron_escape(str(d["move_type"]))}",')
        lines.append(f"        accuracy: {d['accuracy']},")
        lines.append("    ),")
    lines.append("}")
    (data_dir / "moves.ron").write_text("\n".join(lines) + "\n", encoding="utf-8")


def parse_hidden_objects(path: Path) -> dict[str, list[dict[str, Any]]]:
    events_by_map: dict[str, list[dict[str, Any]]] = {}
    current: str | None = None
    label_re = re.compile(r"^([A-Za-z0-9_]+)HiddenObjects:$")

    for raw in path.read_text(encoding="utf-8").splitlines():
        line = strip_asm_comment(raw)
        if not line:
            continue

        m = label_re.match(line)
        if m:
            current = m.group(1)
            events_by_map.setdefault(current, [])
            continue

        if current is None:
            continue

        if line.startswith("db -1"):
            current = None
            continue

        if not line.startswith("hidden_object"):
            continue

        args = line[len("hidden_object") :].strip()
        parts = [p.strip() for p in args.split(",") if p.strip()]
        if len(parts) < 4:
            continue

        x_units = parse_asm_int(parts[0])
        y_units = parse_asm_int(parts[1])
        facing_tok = parts[2]
        action_tok = parts[3]

        if facing_tok == "ANY_FACING":
            facing = None
        elif facing_tok == "SPRITE_FACING_UP":
            facing = "Up"
        elif facing_tok == "SPRITE_FACING_DOWN":
            facing = "Down"
        elif facing_tok == "SPRITE_FACING_LEFT":
            facing = "Left"
        elif facing_tok == "SPRITE_FACING_RIGHT":
            facing = "Right"
        else:
            facing = None

        if action_tok == "OpenPokemonCenterPC":
            action = "OpenPokemonCenterPC"
        elif action_tok == "OpenRedsPC":
            action = "OpenRedsPC"
        else:
            action = f'Other("{ron_escape(action_tok)}")'

        events_by_map[current].append(
            {
                "tx": x_units * PLAYER_STEP_TILES,
                "ty": y_units * PLAYER_STEP_TILES,
                "facing": facing,
                "action": action,
            }
        )

    return events_by_map


def parse_map_object_file(
    path: Path, sprite_consts: dict[str, int]
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]]:
    warp_events: list[dict[str, Any]] = []
    bg_events: list[dict[str, Any]] = []
    object_events: list[dict[str, Any]] = []

    section: str | None = None

    for raw in path.read_text(encoding="utf-8").splitlines():
        line = strip_asm_comment(raw)
        if not line:
            continue

        if line.startswith("def_warp_events"):
            section = "warps"
            continue
        if line.startswith("def_bg_events"):
            section = "bg"
            continue
        if line.startswith("def_object_events"):
            section = "objects"
            continue
        if line.startswith("def_warps_to"):
            section = None
            continue

        if section == "warps" and line.startswith("warp_event"):
            args = line[len("warp_event") :].strip()
            parts = [p.strip() for p in args.split(",") if p.strip()]
            if len(parts) < 4:
                continue
            x = parse_asm_int(parts[0])
            y = parse_asm_int(parts[1])
            dest_map_tok = parts[2]
            dest_warp_id = parse_asm_int(parts[3])
            if dest_map_tok == "LAST_MAP":
                dest_map = "LastMap"
            else:
                dest_map = f'MapId("{ron_escape(dest_map_tok)}")'
            warp_events.append(
                {"x": x, "y": y, "dest_map": dest_map, "dest_warp_id": dest_warp_id}
            )
            continue

        if section == "bg" and line.startswith("bg_event"):
            args = line[len("bg_event") :].strip()
            parts = [p.strip() for p in args.split(",") if p.strip()]
            if len(parts) < 3:
                continue
            x_units = parse_asm_int(parts[0])
            y_units = parse_asm_int(parts[1])
            text_id = parts[-1]
            bg_events.append(
                {
                    "tx": x_units * PLAYER_STEP_TILES,
                    "ty": y_units * PLAYER_STEP_TILES,
                    "text_id": text_id,
                }
            )
            continue

        if section == "objects" and line.startswith("object_event"):
            args = line[len("object_event") :].strip()
            parts = [p.strip() for p in args.split(",")]
            if len(parts) < 6:
                continue
            x_units = parse_asm_int(parts[0])
            y_units = parse_asm_int(parts[1])
            sprite_sym = parts[2].strip()
            movement_sym = parts[3].strip()
            range_sym = parts[4].strip()
            text_id = parts[5].strip()

            if sprite_sym not in sprite_consts:
                raise KeyError(f"Unknown sprite symbol {sprite_sym} in {path}")
            sprite_id = sprite_consts[sprite_sym]

            movement = "Walk" if movement_sym == "WALK" else "Stay"
            if range_sym == "ANY_DIR":
                movement_range = "AnyDir"
            elif range_sym == "UP_DOWN":
                movement_range = "UpDown"
            elif range_sym == "LEFT_RIGHT":
                movement_range = "LeftRight"
            else:
                movement_range = "None"

            if range_sym == "UP":
                facing = "Up"
            elif range_sym == "DOWN":
                facing = "Down"
            elif range_sym == "LEFT":
                facing = "Left"
            elif range_sym == "RIGHT":
                facing = "Right"
            else:
                facing = "Down"

            kind = "Person"
            if len(parts) >= 8 and parts[6].strip().startswith("OPP_"):
                opponent = parts[6].strip()
                trainer_number = parts[7].strip()
                kind = (
                    f'Trainer(opponent: "{ron_escape(opponent)}", '
                    f'trainer_number: "{ron_escape(trainer_number)}")'
                )
            elif len(parts) >= 7 and parts[6].strip() != "0":
                item_id = parts[6].strip()
                kind = f'Item(item_id: "{ron_escape(item_id)}")'

            object_events.append(
                {
                    "sprite_id": sprite_id,
                    "tx": x_units * PLAYER_STEP_TILES,
                    "ty": y_units * PLAYER_STEP_TILES,
                    "kind": kind,
                    "movement": movement,
                    "movement_range": movement_range,
                    "text_id": text_id,
                    "facing": facing,
                }
            )

    return warp_events, bg_events, object_events


def generate_map_events_ron(pokered_root: Path, pokerust_root: Path, data_dir: Path) -> None:
    sprite_consts = parse_sprite_constants_rs(
        pokerust_root / "src" / "data" / "sprite_constants.rs"
    )
    hidden_by_map = parse_hidden_objects(
        pokered_root / "data" / "events" / "hidden_objects.asm"
    )

    objects_dir = pokered_root / "data" / "maps" / "objects"
    entries: list[str] = ["{"]

    asm_files = sorted(p for p in objects_dir.iterdir() if p.suffix == ".asm")
    if not asm_files:
        raise RuntimeError(f"No map object ASM files found under {objects_dir}")

    for asm_path in asm_files:
        map_name = asm_path.stem
        warp_events, bg_events, object_events = parse_map_object_file(asm_path, sprite_consts)
        hidden_events = hidden_by_map.get(map_name, [])

        entries.append(f'    "{ron_escape(map_name)}": (')

        entries.append("        warp_events: [")
        for w in warp_events:
            entries.append(
                f"            (x: {w['x']}, y: {w['y']}, dest_map: {w['dest_map']}, dest_warp_id: {w['dest_warp_id']}),"
            )
        entries.append("        ],")

        entries.append("        bg_events: [")
        for b in bg_events:
            entries.append(
                f'            (tx: {b["tx"]}, ty: {b["ty"]}, text_id: "{ron_escape(b["text_id"])}"),'
            )
        entries.append("        ],")

        entries.append("        hidden_events: [")
        for h in hidden_events:
            facing_s = "None" if h["facing"] is None else f"Some({h['facing']})"
            entries.append(
                f"            (tx: {h['tx']}, ty: {h['ty']}, facing: {facing_s}, action: {h['action']}),"
            )
        entries.append("        ],")

        entries.append("        object_events: [")
        for o in object_events:
            entries.append("            (")
            entries.append(f"                sprite_id: {o['sprite_id']},")
            entries.append(f"                tx: {o['tx']},")
            entries.append(f"                ty: {o['ty']},")
            entries.append(f"                kind: {o['kind']},")
            entries.append(f"                movement: {o['movement']},")
            entries.append(f"                movement_range: {o['movement_range']},")
            entries.append(f'                text_id: "{ron_escape(o["text_id"])}",')
            entries.append(f"                facing: {o['facing']},")
            entries.append("            ),")
        entries.append("        ],")

        entries.append("    ),")

    entries.append("}")
    (data_dir / "map_events.ron").write_text("\n".join(entries) + "\n", encoding="utf-8")


def write_empty_map(path: Path) -> None:
    path.write_text("{\n}\n", encoding="utf-8")


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Generate Rust-owned RON data for pokerust from a pokered checkout."
    )
    default_out_root = Path(__file__).resolve().parents[1]
    default_pokered_root = default_out_root.joinpath("../pokered")
    ap.add_argument(
        "--pokered-root",
        type=Path,
        default=default_pokered_root,
        help="Path to the pokered disassembly checkout (input).",
    )
    ap.add_argument(
        "--out-root",
        type=Path,
        default=default_out_root,
        help="Path to the pokerust repo root (output).",
    )
    args = ap.parse_args()

    pokered_root: Path = args.pokered_root
    pokerust_root: Path = args.out_root
    data_dir = pokerust_root / "data"
    data_dir.mkdir(parents=True, exist_ok=True)

    generate_tilesets_ron(data_dir)
    generate_tileset_headers_ron(data_dir)
    generate_sprite_mapping_ron(pokerust_root, pokered_root, data_dir)
    generate_moves_ron(pokered_root, data_dir)
    generate_map_events_ron(pokered_root, pokerust_root, data_dir)

    # Ensure these files parse as the types expected by the Rust runtime.
    write_empty_map(data_dir / "item_names.ron")
    write_empty_map(data_dir / "move_names.ron")
    write_empty_map(data_dir / "trainer_parties.ron")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
