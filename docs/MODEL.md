# The `civ-model` game model

A skeleton of the state + rules for a Civilization-like 4X game. It is a
**pure model**: no SDL, no I/O. The SDL front-end (`civ-lisp`) is just one view
of it; the AI, a network client, tests, and replays would be others.

## Layering

```
civ-model   state + rules            (this system — pure, headless, serializable)
   ▲
civ-lisp    SDL2 view + input        (reads the model, turns clicks into commands)
```

The whole game is `seed + ordered list of commands`. Because the RNG lives in
the state and all change flows through `apply-command`, the same seed and
commands reproduce the same game — which is what makes saves, replays,
networking and AI tractable.

## Files (`src/model/`)

| file | contents |
|------|----------|
| `package.lisp`  | the `civ-model` package (nickname `civm`) |
| `defs.lisp`     | the rulebook as **data**: `*terrain* *units* *buildings* *techs*` tables |
| `map.lisp`      | `tile`, `game-map`, `tile-at`, `neighbors`, `do-tiles` |
| `entities.lisp` | `player`, `unit`, `city` structs (referenced by integer id) |
| `state.lisp`    | `game-state` root + `make-new-game` |
| `rules.lisp`    | yields, city growth/production, research, the `end-turn` loop |
| `commands.lisp` | `apply-command` — the only sanctioned way to mutate state |
| `ai.lisp`       | a simple AI opponent that issues commands (`run-ai-players`, called from `end-turn`) |

## Core state

```
game-state
  turn / year / phase / rng / id-counter
  map        -> game-map of tiles (terrain, improvements, owner, city, units)
  players[]  -> player (gold, government, techs, research, tax/sci/lux split)
  units      -> id -> unit (type, owner, x/y, hp, moves-left, orders)
  cities     -> id -> city (size, food-box, shield-box, buildings, production, worked)
```

Yields (tile and city) are **derived** from the definition tables each turn
rather than stored, to avoid desync.

## Commands

A command is a tagged plist describing intent; `apply-command` validates and
applies it (or signals `command-error`):

```lisp
(civm:apply-command s '(:found-city :unit 1 :name "Rome"))
(civm:apply-command s '(:move-unit :unit 3 :dx 1 :dy 0))
(civm:apply-command s '(:set-production :city 7 :item (:building :library)))
(civm:apply-command s '(:end-turn))
```

## The turn loop

`end-turn` runs: process cities (auto-assign worked tiles → growth → production
→ economy) → research → refresh unit movement → advance the calendar. A fuller
game would interleave per-player movement/combat phases here.

## What's intentionally left as TODO

This is a scaffold — the seams are in place, the depth is not:

* **Combat** exists (moving into an enemy tile fights to the death, with
  terrain/fortify/city defense bonuses; damage carries and units heal between
  turns — fortified/garrisoned units heal faster).  Still missing zones of
  control and **pathfinding** for `:goto`.
* **Fog of war** / per-player visibility.
* **Diplomacy**, and a smarter **AI** (a basic one exists in `ai.lisp` — it
  expands, produces and researches; it doesn't yet fight, defend or negotiate).
* **Happiness / health**, **government** effects, **wonders**, **victory checks**.
* Richer **map generation** (continents, resources, rivers).

The SDL view (`civ-lisp`, `src/view.lisp` + `src/main.lisp`) already reads a
live `game-state` and renders map/units/cities — with Civ1-style edge-blended
terrain (TER257 neighbour-bitmask overlays + ocean coastlines) — turning
keyboard input into commands. See the project README for controls.
