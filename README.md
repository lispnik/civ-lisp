# civ-lisp

An **SDL2** front-end in Common Lisp (SBCL, [ocicl](https://github.com/ocicl/ocicl)
for dependencies) for a Civilization-like 4X game — explore a wrapping world
dotted with tribal huts, found cities, climb the advance tree, govern, wage war,
do diplomacy / espionage / trade, manage pollution, and win by conquest or the
space race. It
renders a live [`civ-model`](docs/MODEL.md) game using sprites extracted from
the DOS game *Sid Meier's Civilization* (the sibling
[civ-extract](https://github.com/lispnik/civ-extract) project), with the
**torch** graphic (sliced from the same sprite sheet) as the mouse cursor.

![civ-lisp rendering a game with edge-blended terrain](docs/screenshot.png)

*A developed realm: the player's cities **Rome** (walled) and **Ostia**, linked
by a **railroad** and ringed with **irrigated** farmland and **mined** hills; a
**fort** garrisons the southern approach, and the rival **Veii** (red borders)
looms east with cavalry. Edge-blended terrain (TER257), rivers and resource
specials (SP257); blue/red borders mark each civilization.*

![cities close-up](docs/cities.png)

*Cities rendered Civ1-style: a size box (population, in the owner's colour), the
city graphic, optional walls, a name label, and a **black border** when a
military unit garrisons it. Rome (size 7) and Nineveh (size 2) are both
defended; text uses a font extracted from the game's `FONTS.CV`.*

![fog of war](docs/fog.png)

*Fog of war from the human's view: black = unexplored, dim = explored but out of
sight, bright = currently visible. A scout's trail is dimmed behind it.*

### Selecting a unit

Selecting a unit opens a Civ1-style info box in the bottom-left: the owner and
unit type, attack/defense, moves left and HP, the city it garrisons (if any),
and the terrain under it with its yields and defensive bonus. Below that is a
row of **every unit sharing the square** — the selected one outlined in cyan,
the rest in their owner's colour.

![selected-unit info box](docs/unit-panel.png)

*A wounded Legion (HP 6) selected inside Rome, stacked with a Warriors and a
Phalanx; the info box shows its stats, the city and grassland terrain, and the
three-unit garrison with the Legion highlighted.*

### Building a wonder

Selecting a friendly city opens its **build menu**: every unit, improvement and
wonder the city's owner has unlocked through the [advance tree](docs/MODEL.md),
priced in shields. One-per-game wonders are marked with `*`.

![build menu with the Pyramids selected](docs/wonder-before.png)

*Rome's build menu set to the **Pyramids** (300 shields). The roster is gated by
tech, so only what's been researched appears.*

Production accumulates shields each turn until the item completes. Below, after
Rome has grown and worked its tiles for ~150 turns, the Pyramids are done: they
drop out of the buildable list (one per game) and appear in the **Built:**
section with their effect.

![the Pyramids in Rome's built list](docs/wonder-after.png)

*After completion — `Pyramids — boosts production` now shows under **Built:**,
and AI cities (Akkad, Uruk) have grown alongside.*

## Controls

| key | action |
|-----|--------|
| left-click | select a unit (wakes a fortified one), open a friendly city's **build menu**, or — on empty ground — **recenter** the view there |
| 1–9 / click | in the build menu, choose a unit, improvement, or wonder (`*`) to build; Esc closes |
| arrow keys / numpad | move the selected unit — the **numpad moves diagonally** too (1/3/7/9) |
| Tab | cycle to the next active unit (skips fortified / out-of-moves) |
| W | wait — send the unit to the end of this turn's cycle |
| B | found a city (with a settlers unit) |
| F | fortify the selected unit (defense + faster healing) |
| Shift+D | disband the selected unit (recovers shields if in a city) |
| R / I / M | settlers: build **road** (then **railroad** once known) / **irrigation** / **mine** |
| T / C | settlers: build a **fort** / **clear forest** to plains |
| P | settlers: **clean pollution** on the tile |
| Z / X / D | diplomat: **steal tech** / **sabotage** / open the full **spy menu** (embassy, investigate, incite revolt, bribe…) |
| H / J | caravan: **help build a wonder** / **establish a trade route** |
| G, then left-click | send the selected unit to a tile (auto-paths each turn); the cursor becomes the **Go** arrow |
| V | start a **revolution** — pick a new government from the menu |
| Y / E | open the **diplomacy** (war/peace) / **trade** (tech & gold) menu |
| , / . | shift the **luxury** rate down / up (trades against science) |
| Enter | end turn |
| S / L | **save** / **load** the game (single quicksave slot) |
| ~ / K | open the **Lisp console** / start–stop a **Slynk** server (connect from Emacs) |
| ? | toggle the **help** overlay (a keybinding cheat-sheet) |
| Esc | close a menu / cancel a pending Go; otherwise quit |

Pressing `?` brings up an in-window cheat-sheet of every key:

![help overlay](docs/help.png)

The map is covered by **fog of war**: unexplored tiles are black, tiles you've
seen but can't currently see are dimmed, and enemy units/cities show only while
in sight of one of your units or cities. Fog clears the moment a unit moves, not
at end of turn. The selected unit shows an info box in the bottom-left (owner,
type, attack/defense, moves, HP, city, terrain, and the units sharing its square)
— see [Selecting a unit](#selecting-a-unit) above.

Scattered across the map are **tribal huts**. Move a unit onto one and it springs
a surprise: a windfall of gold, a free advance, friendly mercenaries or wandering
settlers joining your cause, an advanced tribe handing you a new city — or a
barbarian ambush. Huts beside your own cities are tame (just friendly scouts
bearing gold), so there's no farming them next to your capital.

Settlers can **terraform** the tile they stand on, over several turns during
which the unit holds position; the improvement then feeds straight into that
tile's yield: `R` builds a **road** (+1 trade) and, once **Railroad** is
researched, upgrades it to a **railroad** (+1 shield); `I` **irrigates** (+1
food); `M` **mines** (+1 shield); `C` **clears forest** to plains; `T` builds a
**fort** (+100% defense in the field, like city walls); and `P` **cleans
pollution**. Industrial cities throw off **pollution** once a civ reaches
Industrialization — a blight that halves a tile's output until a settler clears
it — and if it piles up across the map it triggers **global warming**, which
degrades random land terrain (grassland → plains → desert). City improvements
carry a gold **upkeep** charged every turn; a player who can't pay sells off
improvements (priciest first) until solvent.

Press `S` to **save** and `L` to **load** — because the whole `civ-model` state
is flat, serializable data (the RNG included), a loaded game continues rolling
identically, so save/load is fully deterministic.

Each city's citizens are **happy / content / unhappy**: a few are content for
free and the rest start unhappy, quieted by temples, colosseums and cathedrals,
by **luxuries** (`.`/`,` adjust the rate), by military garrisons under
martial-law governments, and by a handful of wonders. If a city has more unhappy
than happy citizens it falls into **civil disorder** (no production, trade or
growth); get at least half its citizens happy and it **celebrates** — under a
republic or democracy that triggers **"We Love the King" rapture growth**, +1
population every turn (up to the size-8 cap, lifted by an **aqueduct**). The city
menu shows the mood and flags both states. Your **government** (`V` to start a
revolution — one turn of anarchy, then the new regime) sets the rules: despotism
docks busy tiles and bridles the economy; a republic or democracy adds trade but
bans martial law and suffers **war weariness** — every military unit out in the
field makes a citizen back home unhappy (one under a republic, two under a
democracy). Corruption, rate caps and martial-law limits all vary by government,
and the advance tree unlocks Monarchy, Communism, The Republic and Democracy.

Terrain uses the original **edge-blending** scheme (CivOne's algorithm): a land
tile is a generic base plus a TER257 overlay chosen by a bitmask of which
cardinal neighbours share its terrain (N=1 E=2 S=4 W=8), so mismatched edges
feather into their neighbours; ocean tiles add coastline sub-tiles from the
eight surrounding land directions. Rivers (SP257 connection variants, +1 trade)
and per-terrain resource **specials** (SP257; e.g. grassland shields, ocean
fish, mountain gold) are drawn as overlays and feed into tile yields. Units and
cities use SP257 sprites. The world is an 80×50 **horizontal cylinder** (it wraps
east–west, with poles top and bottom); the window is a **scrolling 20×15-tile
viewport** (16 px tiles, scaled 2× → 640×480) whose camera follows the selected
unit, and tiles are stitched seamlessly across the seam. Keyboard input is turned
into `civ-model` commands — the view never mutates the model directly. Several
rival **civilizations** (the default game has four) plus roving **barbarians**
are each run by a simple **AI** that issues the same commands — founding and
spacing out cities, setting production, exploring, researching, even swapping
advances and declaring wars — and take their turns automatically when you end
yours.

**Combat** is **war-gated**: you can't enter a tile held by a civ you're at
peace with, so you **declare war** first (the diplomacy menu, `Y`). Moving into
an enemy-occupied tile then triggers a Civ1-style fight to the death with
terrain, fortification, fort and city-wall bonuses; win and your unit advances
onto the cleared tile. Damage carries between fights, so units **heal** between
turns when they stay put — fully in a city, faster when **fortified** (`F`).
Adjacent enemy units exert a **zone of control**.

**Diplomacy & trade.** Relations are war or peace per pair (`Y` to declare war /
sue for peace); `E` opens a **trade** menu to swap advances or buy/sell them for
gold, and the AIs trade among themselves too. **Diplomats** (`Z`/`X`/`D`) run
the full espionage suite — steal tech, sabotage, establish an embassy,
investigate, incite a city to revolt, or bribe a unit — where a defended city
may catch the spy (lost, plus war). **Caravans** (`H`/`J`) help build a wonder
or open a trade route (a gold windfall plus recurring trade).

**Winning.** The game ends by **conquest** — last civilization with a city or
unit standing — or the **space race**: build the **Apollo Program**, then with
**Space Flight** assemble ten **spaceship parts**; the ship launches and, after
its flight, arrives for the win. A **VICTORY / DEFEAT** banner declares the
result.

> **macOS / arm64 note:** cl-sdl2's high-level event accessors (`scancode-value`,
> the `:x`/`:y` destructuring) read the wrong `SDL_Event` struct offsets against
> SDL2 2.x on Apple Silicon, so keyboard/mouse came through as garbage. The front
> end instead runs its own `SDL_PollEvent` loop and reads fields at the documented
> byte offsets (scancode @16, mouse x/y @20/24). See `src/main.lisp`.

## Two systems

| system | what it is | depends on |
|--------|-----------|------------|
| **`civ-model`** | the pure game model — state + rules, **no SDL, no I/O** | (nothing) |
| **`civ-lisp`** | the SDL2 front-end (window, cursor, scaling, menus) | `civ-model`, `sdl2`, `sdl2-image`, `slynk` |

The model is deliberately independent of rendering so it can be tested headless
and reused by tools / AI / a future networked client. See
[`docs/MODEL.md`](docs/MODEL.md) for the design and
[`examples/model-demo.lisp`](examples/model-demo.lisp) for a runnable tour:

```sh
sbcl --non-interactive --load examples/model-demo.lisp
```

## Requirements

* SBCL
* ocicl
* Native SDL2 + SDL2_image libraries (macOS: `brew install sdl2 sdl2_image`)

## Running

cl-sdl2's FFI bindings need a larger heap to compile, so pass
`--dynamic-space-size`:

```sh
ocicl install            # fetch sdl2 / sdl2-image (first time)
sbcl --dynamic-space-size 4096 --non-interactive --load run.lisp
```

A 640×480 window opens with the torch image as the cursor (drawn centred so you
can see it). Close the window or press **Escape** to quit.

### Live coding (Emacs / SLY)

Press **`~`** in the window for an in-game Lisp console (it binds `*state*` to
the live game), or attach a full REPL from Emacs: press **`K`** to start (or
stop) a Slynk server — equivalently `(civ-lisp:start-slynk)` / `(stop-slynk)`,
from the `~` console or before `(civ-lisp:main)` — then **`M-x sly-connect`**
to `localhost` port `4005`. The server runs in its own thread, so you can
inspect and reshape the running game while it plays.

## Global 2× scaling

The original Civilization assets are tiny on a modern display, so the whole app
is scaled by `*scale*` (default `2`). Two separate things are scaled:

* **Renderer drawing** — `SDL_RenderSetScale` multiplies every render coordinate,
  so the app draws in logical (320×240) space and SDL doubles it.
* **The mouse cursor** — OS cursors are *not* touched by the renderer, so the
  cursor surface is upscaled (nearest-neighbour via `SDL_UpperBlitScaled`)
  before the colour cursor is created. The 13×14 torch becomes a 26×28 cursor.

Change the factor at runtime:

```lisp
(setf civ-lisp:*scale* 3)
(civ-lisp:run)              ; or (civ-lisp:run :scale 3)
```

## Standalone executable

Dump a self-contained binary with `save-lisp-and-die`:

```sh
make                 # or: sbcl --dynamic-space-size 4096 --non-interactive --load build.lisp
./civ-lisp
```

This produces a ~15 MB `./civ-lisp` (a compressed SBCL core with the app and
all Lisp dependencies baked in). The native SDL2 / SDL2_image **shared
libraries are not embedded** — CFFI reloads them at startup, so they must be
installed on whatever machine runs the binary.

`make` targets: `make` (build), `make run` (run from source), `make deps`
(ocicl install), `make clean`.

## How it works

The mouse cursors are sliced straight out of the sprite sheet: the torch is
SP257 cell (7,2) and the goto "GO" cursor is (2,2).  `cell-surface` blits a
16×16 cell into a fresh SDL surface, which becomes a colour cursor via the raw
FFI call `SDL_CreateColorCursor`, then made active with `SDL_SetCursor` (cl-sdl2
doesn't wrap these high-level cursor calls, so we use the autowrap
`sdl2-ffi.functions` layer directly). See `src/main.lisp`.

```lisp
(asdf:load-system :civ-lisp)
(civ-lisp:main)                       ; runs on the macOS main thread
;; or, with options:
(civ-lisp:run :width 800 :height 600)
```

The cursor hotspot defaults to the top-left `(0,0)`; the torch image is 13×14.