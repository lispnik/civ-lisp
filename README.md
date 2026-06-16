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

### Starting a game

Each game opens on a **setup screen**: pick your **civilization** (which sets
your city-name roster and a **free starting advance**), the **difficulty**
(Chieftain … Emperor — higher levels let the AI research faster and turn
warlike), the **number of rivals**, and the **map size**. ↑/↓ choose a row, ←/→
change its value, Enter starts, Esc quits.

Unlike Civilization (1991), where every civ begins knowing nothing, here each
nation starts with one thematic **root advance** — Rome and Germany with Bronze
Working, the Mongols with Horseback Riding, China and Egypt with Masonry,
Greece and France with the Alphabet, and so on. (A deliberate deviation; all are
prerequisite-free advances, so the tech tree stays consistent.)

![the new-game setup screen](docs/setup-screen.png)

### The status pane

A Civ1-style status pane sits in the top-left, reading out the state of your
civilization at a glance: the **date** and turn, your total **population**
(the classic 10k / 30k / 60k… per-city formula, summed and comma-grouped), your
**gold reserves** and **net income per turn**, your **government** and the
**tax / luxury / science** split, and your current **research** target with its
**percent progress** and **turns remaining** to the next advance. A spaceship
line appears once one is in flight. Press **Ctrl+M** to flip the whole pane
(text and minimap) between the left and right edges of the screen.

![the civilization status pane](docs/hud-shot.png)

*Despotism Rome in 3700 BC: population 60,000, 144 gold (+2/turn), a 50/0/50 tax
split, and Writing 56% researched — six turns out.*

Below the readout sits an **overview minimap**: one block per tile coloured by
terrain, cities as dots in their owner's colour, and a white rectangle marking
the part of the world currently on screen. Under fog of war, tiles you have
never seen stay black and explored-but-unseen tiles are dimmed, so the minimap
doubles as your exploration record. **Click anywhere on the minimap to recentre
the main view** there.

![the overview minimap](docs/minimap-shot.png)

*The whole world at a glance — green land, blue sea, rival cities as coloured
dots, and the viewport box showing where the main view is looking.*

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

Every wonder now carries a **live effect**. Empire-wide: the **Pyramids** boost
production, **Hoover Dam** acts as a power plant in every city (+50% shields),
**Hanging Gardens** add content, the **SETI Program** adds +50% science, the
**Lighthouse** trains veteran ships, **Magellan's Expedition** gives ships +1
move, **Women's Suffrage** halves military unhappiness, and the **United Nations**
stays the AI's hand against war. In their own city: the **Great Library**,
**Copernicus' Observatory** and **Isaac Newton's College** pile on science, the
**Colossus** boosts trade, and **Darwin's Voyage** grants two free advances the
moment it completes.

## Controls

| key | action |
|-----|--------|
| left-click | select a unit (wakes a fortified one), open a friendly city's **build menu**, or — on empty ground — **recenter** the view there |
| click minimap | **recenter** the main view on that spot of the world |
| Ctrl+M | dock the status pane + minimap on the opposite (left/right) edge |
| O | open the **Civilopedia** (←/→ section, ↑/↓ scroll, Esc close) |
| 1–9 / click | in the build menu, choose a unit, improvement, or wonder (`*`) to build; Esc closes |
| arrow keys / numpad | move the selected unit — the **numpad moves diagonally** too (1/3/7/9) |
| Tab | cycle to the next active unit (skips fortified / out-of-moves) |
| W | wait — send the unit to the end of this turn's cycle |
| B | found a city (with a settlers unit) — prompts for a name, prefilled from your nation's roster (Enter confirms, Esc cancels) |
| F | fortify the selected unit (defense + faster healing) |
| Shift+D | disband the selected unit (recovers shields if in a city) |
| U | upgrade the selected obsolete unit to its successor (for gold, in a city) |
| N | detonate the selected nuclear missile (3×3 blast centred on its tile) |
| R / I / M | settlers: build **road** (then **railroad** once known) / **irrigation** / **mine** |
| T / A / C | settlers: build a **fort** / **airbase** / **clear** forest→plains, jungle/swamp→grassland |
| P | settlers: **clean pollution** on the tile |
| Z / X / D | diplomat: **steal tech** / **sabotage** / open the full **spy menu** (embassy, investigate, incite revolt, bribe…) |
| H / J | caravan: **help build a wonder** / **establish a trade route** |
| G, then left-click | send the selected unit to a tile (auto-paths each turn); the cursor becomes the **Go** arrow |
| V | start a **revolution** — pick a new government from the menu |
| Y / E | open the **diplomacy** (war / peace / alliance / gift) / **trade** (tech & gold) menu |
| research chooser | opens automatically when your research is idle — pick the next advance (1–9 / click; Esc takes the default) |
| , / . | shift the **luxury** rate down / up (trades against science) |
| [ / ] | shift the **tax** rate down / up — gold vs **science** (capped by your government) |
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
food); `M` **mines** (+1 shield); `C` **clears** vegetation/wetland to the land
beneath (forest→plains, jungle and swamp→grassland); `T` builds a
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

**Founding a city** (`B` with a settler) prompts for its **name**, prefilled with
the next unused name from your **nation's roster** — Rome, Caesarea, Carthage… for
the Romans; Thebes, Memphis, Heliopolis… for the Egyptians — drawn from the
original Civilization name lists (per CivOne, CC0). Type to edit, Enter to found,
Esc to cancel. The AI names its cities the same way.

![naming a new city](docs/name-prompt.png)

Each city's citizens are **happy / content / unhappy**: a few are content for
free and the rest start unhappy, quieted by temples, colosseums and cathedrals,
by **luxuries** (`.`/`,` adjust the rate), by military garrisons under
martial-law governments, and by a handful of wonders. If a city has more unhappy
than happy citizens it falls into **civil disorder** (no production, trade or
growth); get at least half its citizens happy and it **celebrates** — under a
republic or democracy that triggers **"We Love the King" rapture growth**, +1
population every turn (up to the size-8 cap, raised to 12 by an **aqueduct** and
lifted entirely by a **sewer system**). The city
menu shows the mood and flags both states. Your **government** (`V` to start a
revolution — one turn of anarchy, then the new regime) sets the rules: despotism
docks busy tiles and bridles the economy; a republic or democracy adds trade but
bans martial law and suffers **war weariness** — every military unit out in the
field makes a citizen back home unhappy (one under a republic, two under a
democracy). Corruption, rate caps and martial-law limits all vary by government,
and the advance tree unlocks Monarchy, Communism, The Republic and Democracy.

**Research.** You **choose what to research**: at the start of the game and each
time you complete an advance, a chooser lists every advance whose prerequisites
you now meet, and you pick the next one to pursue (Esc takes the first as a
default, so research never stalls). The status pane tracks the percent progress
toward it. The AI civilizations beeline their own priorities automatically.

![the research chooser](docs/research-menu.png)

*Choosing the next advance — only those whose prerequisites are met are offered.*

**Civilopedia.** Press `O` for a browsable in-game reference, read straight from
the rule tables: every **advance** with its prerequisites, every **unit** with
its attack/defense/move and cost, and every **building** and **wonder** with its
cost, required advance, and effect. Entries you can't have yet — an unresearched
advance, or a unit/building/wonder whose prerequisite advance you lack — are
**dimmed**, so the bright entries are what's available to you now. `←`/`→` switch
sections, `↑`/`↓` scroll, `Esc` closes.

![the Civilopedia, wonders section](docs/pedia-wonders.png)

*The wonders section: cost, the advance that unlocks each, and its effect. The
four whose advance this civ already has are bright; the rest are dimmed.*

**City improvements** build up the economy, each with a live effect: a
**marketplace** then a **bank** and **stock exchange** each add +50% gold (the
first two boost luxuries too); a **library**, **university** and (wonder) Great
Library each add +50% science; a **courthouse** halves corruption; a **temple**,
**colosseum** and **cathedral** keep citizens content; and on the production
side a **factory** adds +50% shields, a **power plant** (or hydro/nuclear plant,
or the Hoover Dam) adds +50% more to a factory city, and a **manufacturing
plant** adds +50% again — with **mass transit** and **recycling centers** to
scrub the pollution that heavy industry brings. The AI builds these too: it
beelines the economy advances and raises infrastructure in its cities even
between wars.

Terrain uses the original **edge-blending** scheme (CivOne's algorithm): a land
tile is a generic base plus a TER257 overlay chosen by a bitmask of which
cardinal neighbours share its terrain (N=1 E=2 S=4 W=8), so mismatched edges
feather into their neighbours; ocean tiles add coastline sub-tiles from the
eight surrounding land directions. Rivers (SP257 connection variants, +1 trade)
and per-terrain resource **specials** (SP257; e.g. grassland shields, ocean
fish, mountain gold, jungle gems, arctic seals) are drawn as overlays and feed
into tile yields. Units and cities use SP257 sprites. The world is **generated**
as continents grown out of open ocean with a **latitude climate** — arctic and
tundra at the poles, temperate grassland/forest/hills between, jungle and swamp
at the equator — across all eleven terrain types.

Two freshly-generated 80×50 worlds (whole map, fog off — mountain ranges thread
the continents, ice hugs the poles, fish dot the seas):

![generated world](docs/worldgen-1.png)
![generated world](docs/worldgen-2.png)

It is an 80×50 **horizontal
cylinder** (it wraps
east–west, with poles top and bottom); the window is a **scrolling 20×15-tile
viewport** (16 px tiles, scaled 2× → 640×480) whose camera follows the selected
unit, and tiles are stitched seamlessly across the seam. Keyboard input is turned
into `civ-model` commands — the view never mutates the model directly. Several
rival **civilizations** (the default game has four) plus roving **barbarians**
are each run by a simple **AI** that issues the same commands — founding and
spacing out cities, setting production, exploring, researching, even swapping
advances — and take their turns automatically when you end yours. Each unit
spends its **whole movement** each turn; cities always keep a **garrison**
(rebuilding a defender whenever one is left empty); and the AI only **declares
war** on a rival it is at least as strong as, **suing for peace** once it is down
to half a rival's cities.

Each AI is dealt a **personality** at the start — **aggressive**, **expansionist**,
**builder**, or **scientific** — that biases how readily it goes to war, how far
it expands, how eagerly it raises wonders and infrastructure, how willing it is
to ally, and which advances it chases first, so a six-civ game has real variety.
A game-wide **difficulty** (`:chieftain` … `:emperor`, default Prince) sets the
AI handicap: at higher levels the rival civilizations research markedly faster
and go to war more readily. Surplus troops **march on the nearest enemy city**,
and at war a coastal AI builds a **transport**, loads an invasion force,
ferries it across the sea, and lands the troops on your shore. It also fields its
**whole toolbox** — defending **aircraft** that fly home to refuel, **diplomats**
that spy on adjacent cities, **caravans** that open trade routes, and **nuclear
missiles** it flies at your cities and detonates. The AI also runs an
**economy**, not just an army: it **beelines** the advances that matter
(Monarchy, Trade, the Republic, the wonder techs), leads a **revolution** toward
the best government it can adopt (Monarchy while at war, Republic or Democracy in
peace), and invests its **largest city** in **world wonders** — whose effects are
all live (see below). [`examples/economy-demo.lisp`](examples/economy-demo.lisp)
plays a full six-AI game and reports the governments adopted and wonders raised.

**Combat** is **war-gated**: you can't enter a tile held by a civ you're at
peace with, so you **declare war** first (the diplomacy menu, `Y`). Moving into
an enemy-occupied tile then triggers a Civ1-style fight to the death with
terrain, fortification, fort and city-wall bonuses; win and your unit advances
onto the cleared tile. A land unit that enters an enemy **city** with no
defenders left **captures** it — ownership flips, it shrinks by one, its
buildings and wonders carry over, and trade routes break; a size-1 city is
**razed** instead. Take a civ's last city and you win by **conquest**. Damage
carries between fights, so units **heal** between turns when they stay put —
fully in a city, faster when **fortified** (`F`).
Adjacent enemy units exert a **zone of control**.

**Naval transport.** Land units can't swim, but they can ride ships. Move a land
unit onto an adjacent sea tile holding one of your **transport-capable** ships
(trireme 2, sail 3, frigate 4, transport 8) to **board** it; the ship then
carries its cargo wherever it sails. Move a passenger back onto land to
**disembark**, or straight onto an adjacent enemy coastal tile for an
**amphibious assault**. A ship only takes on cargo up to its capacity, and a
ship **sunk** at sea drowns everyone aboard. (Carriers carry air units, which
matters once air units gain range.)

**Obsolescence.** Advances retire old units: once you research the superseding
tech (Gunpowder retires Warriors/Phalanx, Conscription retires Legions and
Musketeers, Combustion retires Frigates and Ironclads, and so on) the unit drops
out of the build menu. Existing ones keep fighting, but you can **upgrade** one
to its successor while it sits in a city (`U`) for gold proportional to the cost
difference.

**Air power.** Fighters, bombers and the nuclear missile fly anywhere, but burn
**fuel**: each carries a limited number of turns of flight (2 / 5 / 8) and must
end a turn on a friendly **city**, **airbase** (a settler improvement, `A`) or
**carrier** to refuel — run dry in the air and the unit crashes. Carriers are
mobile airbases: parked planes refuel on them and ride along when the carrier
sails. The selected unit's info box shows its remaining **Fuel**.

**Nuclear weapons.** Once you research Rocketry you can build the **nuclear
missile**. Fly it next to a target and detonate (`N`): everything on the centre
tile and the eight around it is vaporised, any **city** in the blast is
devastated (population halved), and the ground is left with **fallout**
(pollution, which feeds global warming). The strike is indiscriminate — it kills
your own units too — and dropping it on a civ you were at peace with is an act of
**war**. Detonation plays a 40-frame **mushroom-cloud animation**
(`assets/nuke.png`) over the blast.

**Diplomacy & trade.** Relations are **war**, **peace**, or **alliance** per
pair. The `Y` **diplomacy menu** lists, for each rival, the moves open to you:
make peace or propose a **cease-fire** with an enemy (a timed truce the AI won't
break for a while), declare war on a neighbour, **propose an alliance** (an AI
weighs it — a civ welcomes an ally at least as strong as itself), **break** one,
**demand tribute** (a weaker, fearful AI pays gold; a confident one refuses), or
**gift 50 gold** as a goodwill gesture. Allies can't attack each other, and the
AI won't pounce on an ally (you can still betray one by declaring war). The AIs
also conduct **their own diplomacy** — forming alliances and agreeing cease-fires
among themselves as the game unfolds — and they **make offers to you**: a friendly
civ proposes an alliance, and one losing a war to you sues for a cease-fire. Such
an offer pops up a prompt you answer with **Y** (accept) or **N** (decline); it
isn't applied unless you agree. `E` opens a **trade** menu to swap advances or
buy/sell them for gold.

![the diplomacy menu](docs/diplo-menu.png)

*Per-rival diplomacy: at war with Egypt (make peace / cease-fire), allied with
Zulu (break / declare war), at peace with Greece (war / alliance / tribute / gift).*

![an incoming AI offer](docs/offer-prompt.png)

*An AI's offer is yours to accept or decline.*

**Diplomats** (`Z`/`X`/`D`) run
the full espionage suite — steal tech, sabotage, establish an embassy,
investigate, incite a city to revolt, or bribe a unit — where a defended city
may catch the spy (lost, plus war). **Caravans** (`H`/`J`) help build a wonder
or open a trade route (a gold windfall plus recurring trade).

**Winning.** The game ends by **conquest** — last civilization with a city or
unit standing — or the **space race**: build the **Apollo Program**, then with
**Space Flight** (the structurals) and **Fusion Power** (the ship's fuel)
assemble ten **spaceship parts**; the ship launches and, after its flight,
arrives for the win.

**Score.** As in Civilization (1991), every civ earns a running **score** —
**+2** per happy citizen and **+1** per content one, **+3** per advance, **+5**
per wonder, **+1** for each turn spent at war with nobody, and **−1** per
polluted tile around its cities (never below zero). When the game ends, the
**VICTORY / DEFEAT** verdict is followed by the **final standings**, every
civilization ranked by score in its own colour.

![the end-game score summary](docs/score-summary.png)

*Greece wins by conquest with 90 points; the also-rans are ranked beneath.*

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