;;;; view.lisp -- render a civ-model GAME-STATE with SDL2.
;;;;
;;;; The view is read-only: it draws the model and never mutates it.
;;;;
;;;; Terrain uses the original Civilization edge-blending scheme (as in CivOne):
;;;; a land tile is a generic land *base* (SP257) plus a terrain *overlay* taken
;;;; from TER257 at column = bitmask of the four cardinal neighbours that share
;;;; the same terrain (N=1 E=2 S=4 W=8), row = terrain id.  Where a neighbour
;;;; differs, that edge of the overlay feathers out, blending the tiles.  Ocean
;;;; tiles draw an ocean base plus coastline sub-tiles chosen from the eight
;;;; surrounding land directions.  Units and cities use SP257 sprites.

(in-package #:civ-lisp)

(defparameter *tile* 16 "Native sprite/tile size in pixels.")

(defparameter *sprites-image*
  (merge-pathnames "assets/sprites.png" (asdf:system-source-directory :civ-lisp))
  "The SP257 sheet (units, cities, land base).")
(defparameter *terrain-image*
  (merge-pathnames "assets/terrain.png" (asdf:system-source-directory :civ-lisp))
  "The TER257 sheet (terrain blend variants, ocean, coast).")
(defparameter *nuke-image*
  (merge-pathnames "assets/nuke.png" (asdf:system-source-directory :civ-lisp))
  "Nuclear-detonation animation: 28 centred frames (7x4), green-keyed.")
(defparameter +nuke-bg-key+ '(44 120 0) "Green background keyed out of nuke.png.")
(defparameter +nuke-pitch+ 45 "Cell pitch in nuke.png (1px green separators at each multiple of 45).")
(defparameter +nuke-frame+ 43 "Content size of one detonation frame within its cell.")
(defparameter +nuke-cols+ 7 "Frame columns in nuke.png.")
(defparameter +nuke-frames+ 28 "Detonation frames: 7 cols x 4 rows.")

;;; --- sprite atlas coordinates ----------------------------------------------

(defparameter *terrain-rows*
  '((:desert . 0) (:plains . 1) (:grassland . 2) (:forest . 3) (:hills . 4)
    (:mountains . 5) (:tundra . 6) (:arctic . 7) (:swamp . 8) (:jungle . 9)
    (:ocean . 10))
  "civ-model terrain -> TER257 row (terrain id).")

(defun terrain-row (terrain) (or (cdr (assoc terrain *terrain-rows*)) 2))

;; land base in SP257 (x,y); ocean base in TER257 (x,y)
(defparameter +land-base+ '(0 . 64))
(defparameter +ocean-base+ '(0 . 160))

(defparameter *unit-sprites*
  '(;; row 10: land + air units, in SP257 column order
    (:settlers . (0 . 10)) (:warriors . (1 . 10)) (:phalanx . (2 . 10))
    (:legion . (3 . 10)) (:musketeers . (4 . 10)) (:riflemen . (5 . 10))
    (:cavalry . (6 . 10)) (:knights . (7 . 10)) (:catapult . (8 . 10))
    (:cannon . (9 . 10)) (:chariot . (10 . 10)) (:armor . (11 . 10))
    (:mech-inf . (12 . 10)) (:artillery . (13 . 10)) (:fighter . (14 . 10))
    (:bomber . (15 . 10))
    ;; cols 16-19 + row 11 cols 0-4: the naval units
    (:trireme . (16 . 10)) (:sail . (17 . 10)) (:frigate . (18 . 10))
    (:ironclad . (19 . 10)) (:cruiser . (0 . 11)) (:battleship . (1 . 11))
    (:submarine . (2 . 11)) (:carrier . (3 . 11)) (:transport . (4 . 11))
    ;; row 11 cols 5-7: the special units
    (:nuclear . (5 . 11)) (:diplomat . (6 . 11)) (:caravan . (7 . 11)))
  "civ-model unit type -> (col . row) in the SP257 sprite atlas.")
(defparameter +default-unit-sprite+ '(1 . 10))
(defparameter +city-sprite+ '(12 . 7))   ; SP257 col 12, row 7 (tile_007_012)
(defparameter +city-walls-sprite+ '(13 . 7)) ; SP257 col 13, row 7 (tile_007_013): walls overlay
(defparameter +irrigation-sprite+ '(4 . 2))  ; SP257 col 4, row 2 (tile_002_004): irrigation overlay
(defparameter +mine-sprite+ '(5 . 2))         ; SP257 col 5, row 2 (tile_002_005): mine overlay
(defparameter +pollution-sprite+ '(6 . 2))    ; SP257 col 6, row 2 (tile_002_006): pollution blight
(defparameter +fort-sprite+ '(14 . 7))         ; SP257 col 14, row 7 (tile_007_014): field fort
(defparameter +hut-sprite+ '(15 . 7))           ; SP257 col 15, row 7 (tile_007_015): tribal hut

(defun unit-sprite (type)
  (or (cdr (assoc type *unit-sprites*)) +default-unit-sprite+))

;;; cardinal direction bits (match CivOne's Direction enum)
(defconstant +n+ 1) (defconstant +e+ 2) (defconstant +s+ 4) (defconstant +w+ 8)
(defconstant +nw+ 16) (defconstant +ne+ 32) (defconstant +sw+ 64) (defconstant +se+ 128)

;;; --- neighbour bitmasks ----------------------------------------------------

(defun cardinal-same-mask (map x y terrain)
  "Bitmask of the N/E/S/W neighbours of (X,Y) that share TERRAIN."
  (flet ((same (nx ny)
           (let ((tl (civm:tile-at map nx ny)))
             (and tl (eq (civm:tile-terrain tl) terrain)))))
    (logior (if (same x (1- y)) +n+ 0) (if (same (1+ x) y) +e+ 0)
            (if (same x (1+ y)) +s+ 0) (if (same (1- x) y) +w+ 0))))

(defun river-mask (map x y)
  "Bitmask of N/E/S/W neighbours of (X,Y) that are river or ocean (a river
connects to other rivers and flows into the sea)."
  (flet ((wet (nx ny)
           (let ((tl (civm:tile-at map nx ny)))
             (and tl (or (civm:tile-river tl)
                         (eq (civm:tile-terrain tl) :ocean))))))
    (logior (if (wet x (1- y)) +n+ 0) (if (wet (1+ x) y) +e+ 0)
            (if (wet x (1+ y)) +s+ 0) (if (wet (1- x) y) +w+ 0))))

(defun ocean-land-mask (map x y)
  "Bitmask of the eight neighbours of ocean tile (X,Y) that are land."
  (let ((m 0))
    (flet ((land (nx ny bit)
             (let ((tl (civm:tile-at map nx ny)))
               (when (and tl (not (eq (civm:tile-terrain tl) :ocean)))
                 (setf m (logior m bit))))))
      (land x (1- y) +n+)  (land (1+ x) y +e+)
      (land x (1+ y) +s+)  (land (1- x) y +w+)
      (land (1- x) (1- y) +nw+) (land (1+ x) (1- y) +ne+)
      (land (1- x) (1+ y) +sw+) (land (1+ x) (1+ y) +se+))
    m))

;;; --- colours ---------------------------------------------------------------

(defparameter *player-colors*
  '((1 80 150 235) (2 220 70 70) (3 90 200 120) (4 230 200 80)
    (5 200 110 220) (6 230 150 60) (7 70 200 210) (8 40 40 50))
  "Player color index -> (r g b).  Index 8 (dark) is used for barbarians.")

(defparameter +unit-bg-key+ '(96 224 100)
  "The green placeholder behind every SP257 unit sprite, keyed out at load time so
the owning team's colour shows through instead.")

(defparameter +sprite-grid-key+ '(0 168 168)
  "Leftover cyan cell-grid lines along the top/left of many SP257 cells; stripped
at load time where they sit on a 16px boundary (interior cyan -- e.g. the water
on naval sprites -- is real art and is left alone).")

(defun owner-color (state owner-id)
  (let ((p (and owner-id (civm:player-by-id state owner-id))))
    (if p (or (cdr (assoc (civm:player-color p) *player-colors*)) '(230 230 230))
        '(180 180 180))))

;;; --- painter (reuses two rects to avoid per-draw allocation) ---------------

(defstruct (painter (:constructor make-painter (ren sprites terrain src dst)))
  ren sprites terrain src dst (font nil) (nuke nil))

;; defined in font.lisp (loaded after this file)
(declaim (ftype (function (t t t t t t t t) t) draw-text))
(declaim (ftype (function (t t t t t) t) draw-label))
(declaim (ftype (function (t t) t) text-width))

(defun make-renderer-painter (ren sprites-tex terrain-tex)
  (make-painter ren sprites-tex terrain-tex
                (sdl2:make-rect 0 0 *tile* *tile*)
                (sdl2:make-rect 0 0 *tile* *tile*)))

(defun set-rect (r x y w h)
  (setf (sdl2:rect-x r) x (sdl2:rect-y r) y
        (sdl2:rect-width r) w (sdl2:rect-height r) h))

(defun blit (p tex sx sy w h dx dy)
  "Copy the W x H region at (SX,SY) of TEX to (DX,DY) (logical coords)."
  (set-rect (painter-src p) sx sy w h)
  (set-rect (painter-dst p) dx dy w h)
  (sdl2:render-copy (painter-ren p) tex
                    :source-rect (painter-src p) :dest-rect (painter-dst p)))

(defun draw-sprite (p col row dx dy)
  (blit p (painter-sprites p) (* col *tile*) (* row *tile*) *tile* *tile* dx dy))

(defun draw-marker (p px py w h rgb)
  "Fill a small W x H rectangle near the top-left of the tile drawn at (PX,PY)."
  (destructuring-bind (r g b) rgb
    (sdl2:set-render-draw-color (painter-ren p) r g b 255)
    (set-rect (painter-dst p) (+ px 1) (+ py 1) w h)
    (sdl2:render-fill-rect (painter-ren p) (painter-dst p))))

(defun draw-frame (p px py rgb &optional (inset 0))
  "Draw a rectangle outline INSET pixels inside the tile drawn at (PX,PY)."
  (destructuring-bind (r g b) rgb
    (sdl2:set-render-draw-color (painter-ren p) r g b 255)
    (set-rect (painter-dst p) (+ px inset) (+ py inset)
              (- *tile* (* 2 inset)) (- *tile* (* 2 inset)))
    (sdl2:render-draw-rect (painter-ren p) (painter-dst p))))

(defun draw-border (p px py rgb) (draw-frame p px py rgb 0))

;;; --- terrain ---------------------------------------------------------------

(defun draw-coast (p land px py)
  "Composite an ocean coastline from 8x8 TER257 sub-tiles given the LAND mask."
  (flet ((b (sx sy lx ly)
           (blit p (painter-terrain p) sx sy 8 8 (+ px lx) (+ py ly)))
         (h (d) (plusp (logand land d))))
    ;; cardinal coast segments (two 8x8 halves each)
    (when (h +n+)
      (b (cond ((h +w+) 80) ((h +nw+) 96) (t 64)) 176 0 0)
      (b (cond ((h +e+) 88) ((h +ne+) 56) (t 24)) 176 8 0))
    (when (h +e+)
      (b (cond ((h +n+) 88) ((h +ne+) 104) (t 72)) 176 8 0)
      (b (cond ((h +s+) 88) ((h +se+) 56) (t 24)) 184 8 8))
    (when (h +s+)
      (b (cond ((h +w+) 80) ((h +sw+) 48) (t 16)) 184 0 8)
      (b (cond ((h +e+) 88) ((h +se+) 104) (t 72)) 184 8 8))
    (when (h +w+)
      (b (cond ((h +n+) 80) ((h +nw+) 48) (t 16)) 176 0 0)
      (b (cond ((h +s+) 80) ((h +sw+) 96) (t 64)) 184 0 8))
    ;; diagonal-only coasts
    (when (and (h +nw+) (not (h +n+)) (not (h +w+))) (b 32 176 0 0))
    (when (and (h +ne+) (not (h +n+)) (not (h +e+))) (b 40 176 8 0))
    (when (and (h +sw+) (not (h +s+)) (not (h +w+))) (b 32 184 0 8))
    (when (and (h +se+) (not (h +s+)) (not (h +e+))) (b 40 184 8 8))))

(defparameter +river-row+ 80)         ; SP257 y of river connection variants
(defparameter +special-row+ 112)      ; SP257 y of terrain special resources

(defun draw-special (p terr px py)
  "Draw TERR's special-resource icon (transparent overlay) at (PX,PY).
Indexed by terrain id along SP257 row 7; these cells have transparent
backgrounds, unlike the grassland shield sub-tile CivOne colour-keys."
  (blit p (painter-sprites p) (* (terrain-row terr) *tile*) +special-row+
        *tile* *tile* px py))

;;; roads/railroads are drawn additively: a directional segment toward each
;;; connected neighbour, OR'd together.  SP257 row 3 cols 0-7 hold the road
;;; segments (clockwise from north); row 6 cols 8-15 hold the railroad ones.
(defparameter +road-row+ 3)
(defparameter +railroad-row+ 6)
(defparameter +track-dirs+
  '((0 -1 . 0) (1 -1 . 1) (1 0 . 2) (1 1 . 3)
    (0 1 . 4) (-1 1 . 5) (-1 0 . 6) (-1 -1 . 7))
  "(dx dy . k) per direction, clockwise from north; the segment for direction k
is at sprite column (base + k).")

(defun road-link-p (map x y)
  "T if tile (X,Y) carries a road/railroad or a city (roads connect into cities)."
  (let ((tl (civm:tile-at map x y)))
    (and tl (or (civm:tile-road tl) (civm:tile-railroad tl) (civm:tile-city tl)))))

(defun rail-link-p (map x y)
  "T if tile (X,Y) carries a railroad or a city."
  (let ((tl (civm:tile-at map x y)))
    (and tl (or (civm:tile-railroad tl) (civm:tile-city tl)))))

(defun draw-track (p map x y px py row base link-fn stub-rgb)
  "Composite a road/rail from the directional segments (row ROW, columns BASE+k)
toward each neighbour LINK-FN accepts; an isolated track gets a small stub."
  (let ((linked nil))
    (dolist (d +track-dirs+)
      (destructuring-bind (dx dy . k) d
        (when (funcall link-fn map (+ x dx) (+ y dy))
          (setf linked t)
          (blit p (painter-sprites p) (* (+ base k) *tile*) (* row *tile*)
                *tile* *tile* px py))))
    (unless linked
      (destructuring-bind (r g b) stub-rgb
        (sdl2:set-render-draw-color (painter-ren p) r g b 255))
      (set-rect (painter-dst p) (+ px 6) (+ py 6) 4 4)
      (sdl2:render-fill-rect (painter-ren p) (painter-dst p)))))

(defun draw-road (p map x y px py)
  (draw-track p map x y px py +road-row+ 0 #'road-link-p '(150 110 70)))

(defun draw-railroad (p map x y px py)
  (draw-track p map x y px py +railroad-row+ 8 #'rail-link-p '(120 120 130)))

(defun draw-terrain-tile (p state x y px py)
  "Draw the world tile (X,Y) at screen pixel (PX,PY)."
  (let* ((map (civm:gs-map state))
         (tile (civm:tile-at map x y))
         (terr (civm:tile-terrain tile)))
    (if (eq terr :ocean)
        (progn
          (blit p (painter-terrain p) (car +ocean-base+) (cdr +ocean-base+)
                *tile* *tile* px py)
          (draw-coast p (ocean-land-mask map x y) px py))
        (progn
          ;; generic land base, then the blended terrain overlay
          (blit p (painter-sprites p) (car +land-base+) (cdr +land-base+)
                *tile* *tile* px py)
          (blit p (painter-terrain p)
                (* (cardinal-same-mask map x y terr) *tile*)
                (* (terrain-row terr) *tile*)
                *tile* *tile* px py)))
    ;; terrain improvements: irrigation / mine overlays, then roads
    (when (civm:tile-irrigation tile)
      (blit p (painter-sprites p) (* (car +irrigation-sprite+) *tile*)
            (* (cdr +irrigation-sprite+) *tile*) *tile* *tile* px py))
    (when (civm:tile-mine tile)
      (blit p (painter-sprites p) (* (car +mine-sprite+) *tile*)
            (* (cdr +mine-sprite+) *tile*) *tile* *tile* px py))
    ;; a railroad supersedes the plain road graphic on its tile
    (cond ((civm:tile-railroad tile) (draw-railroad p map x y px py))
          ((civm:tile-road tile)     (draw-road p map x y px py)))
    ;; rivers (SP257 connection variants) then the special-resource icon
    (when (civm:tile-river tile)
      (blit p (painter-sprites p) (* (river-mask map x y) *tile*) +river-row+
            *tile* *tile* px py))
    (when (civm:tile-special tile)
      (draw-special p terr px py))
    (when (civm:tile-fort tile)
      (blit p (painter-sprites p) (* (car +fort-sprite+) *tile*)
            (* (cdr +fort-sprite+) *tile*) *tile* *tile* px py))
    (when (civm:tile-airbase tile)            ; airbase pad (a light landing strip)
      (draw-marker p px py (- *tile* 4) 3 '(210 210 230))
      (draw-frame p px py '(120 120 160) 1))
    ;; pollution blight sits on top of everything else on the tile
    (when (civm:tile-pollution tile)
      (blit p (painter-sprites p) (* (car +pollution-sprite+) *tile*)
            (* (cdr +pollution-sprite+) *tile*) *tile* *tile* px py))))

;;; --- the frame -------------------------------------------------------------

(defun colorkey-surface! (surf rgb)
  "Make every pixel of SURF (an ABGR8888 surface, byte order R,G,B,A) matching RGB
fully transparent, in place."
  (destructuring-bind (r g b) rgb
    (let ((w (sdl2:surface-width surf))
          (h (sdl2:surface-height surf))
          (pitch (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
          (px (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)))
      (sdl2-ffi.functions:sdl-lock-surface surf)
      (dotimes (y h)
        (let ((row (* y pitch)))
          (dotimes (x w)
            (let ((o (+ row (* x 4))))
              (when (and (= (cffi:mem-ref px :uint8 o) r)
                         (= (cffi:mem-ref px :uint8 (+ o 1)) g)
                         (= (cffi:mem-ref px :uint8 (+ o 2)) b))
                (setf (cffi:mem-ref px :uint8 (+ o 3)) 0))))))
      (sdl2-ffi.functions:sdl-unlock-surface surf))))

(defun strip-grid-lines! (surf rgb)
  "On an ABGR8888 SURF, make pixels matching RGB transparent only where they lie
on a *TILE*-pixel cell boundary (x or y a multiple of *TILE*) -- the sprite
sheet's leftover grid lines.  Interior pixels of that colour (real art) stay."
  (destructuring-bind (r g b) rgb
    (let ((w (sdl2:surface-width surf))
          (h (sdl2:surface-height surf))
          (pitch (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
          (px (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)))
      (sdl2-ffi.functions:sdl-lock-surface surf)
      (dotimes (y h)
        (let ((row (* y pitch)) (y-edge (zerop (mod y *tile*))))
          (dotimes (x w)
            (when (or y-edge (zerop (mod x *tile*)))
              (let ((o (+ row (* x 4))))
                (when (and (= (cffi:mem-ref px :uint8 o) r)
                           (= (cffi:mem-ref px :uint8 (+ o 1)) g)
                           (= (cffi:mem-ref px :uint8 (+ o 2)) b))
                  (setf (cffi:mem-ref px :uint8 (+ o 3)) 0)))))))
      (sdl2-ffi.functions:sdl-unlock-surface surf))))

(defun load-atlas (ren path &optional colorkey)
  "Load PATH as a blend-enabled texture (caller destroys it).  When COLORKEY is
an (r g b) list, those pixels are made transparent first and the sheet's leftover
cyan cell-grid lines are stripped."
  (let ((surf (sdl2-image:load-image (namestring path))))
    (when colorkey
      ;; normalise to ABGR8888 so the pixel walk can assume R,G,B,A byte order
      (let ((conv (sdl2-ffi.functions:sdl-convert-surface-format
                   surf sdl2-ffi:+sdl-pixelformat-abgr8888+ 0)))
        (sdl2-ffi.functions:sdl-free-surface surf)
        (setf surf conv))
      (colorkey-surface! surf colorkey)
      (strip-grid-lines! surf +sprite-grid-key+))
    (let ((tex (sdl2:create-texture-from-surface ren surf)))
      (sdl2-ffi.functions:sdl-free-surface surf)
      (sdl2-ffi.functions:sdl-set-texture-blend-mode tex 1) ; SDL_BLENDMODE_BLEND
      tex)))

(defun dim-tile (p px py)
  "Darken the tile drawn at (PX,PY) (explored but not currently visible)."
  (sdl2:set-render-draw-color (painter-ren p) 0 0 0 120)
  (set-rect (painter-dst p) px py *tile* *tile*)
  (sdl2:render-fill-rect (painter-ren p) (painter-dst p)))

(defun human-player (state)
  (find :human (civm:gs-players state) :key #'civm:player-kind))

(defun year-text (year)
  (if (minusp year) (format nil "~D BC" (- year)) (format nil "AD ~D" year)))

(defun draw-city (painter state city px py)
  "Civ1-style city drawn at screen pixel (PX,PY): skyline, optional walls, a size
box, an owner/black border (black when a military unit garrisons it), and a name
label below."
  (let* ((font (painter-font painter))
         (ren (painter-ren painter)))
    ;; the city sprite is transparent, so paint the owner's colour square first
    (destructuring-bind (r g b) (owner-color state (civm:city-owner city))
      (sdl2:set-render-draw-color ren r g b 255)
      (set-rect (painter-dst painter) px py *tile* *tile*)
      (sdl2:render-fill-rect ren (painter-dst painter)))
    (draw-sprite painter (car +city-sprite+) (cdr +city-sprite+) px py)
    (when (member :walls (civm:city-buildings city))         ; city-walls overlay
      (draw-sprite painter (car +city-walls-sprite+) (cdr +city-walls-sprite+) px py))
    ;; a black border marks a city garrisoned by a military unit
    (when (civm:city-defended-p state city)
      (draw-border painter px py '(0 0 0)))
    (when font
      ;; population number in a small black box at the top-left
      (let* ((label (princ-to-string (civm:city-size city)))
             (bw (1+ (text-width font label))))
        (sdl2:set-render-draw-color ren 0 0 0 255)
        (set-rect (painter-dst painter) px py bw (+ 2 (gfont-height font)))
        (sdl2:render-fill-rect ren (painter-dst painter))
        (draw-text painter font label px (1+ py) 255 255 255))
      ;; city name centred just below the tile
      (draw-label painter font (civm:city-name city)
                  (+ px (floor *tile* 2)) (+ py *tile* 1)))))

(defun draw-unit (painter state u px py)
  "Draw unit U's sprite over a team-colour background at screen pixel (PX,PY),
plus a fortify marker."
  (let ((spr (unit-sprite (civm:unit-type u)))
        (ren (painter-ren painter)))
    ;; the sprite's green placeholder is keyed out, so paint the owner's colour
    ;; behind it -- that team colour shows through the unit's background
    (destructuring-bind (r g b) (owner-color state (civm:unit-owner u))
      (sdl2:set-render-draw-color ren r g b 255)
      (set-rect (painter-dst painter) px py *tile* *tile*)
      (sdl2:render-fill-rect ren (painter-dst painter)))
    (draw-sprite painter (car spr) (cdr spr) px py)
    (when (eq (civm:unit-orders u) :fortified)
      (draw-marker painter px py 3 3 '(245 245 245)))))

(defun blink-on-p ()
  "Toggles every 500 ms (drives the selected-unit blink)."
  (evenp (floor (sdl2-ffi.functions:sdl-get-ticks) 500)))

(defun draw-unit-panel (painter state u)
  "A Civ1-style info box for the selected unit, anchored bottom-left: owner and
unit type, attack/defense, moves/HP, the city (if any) and terrain under it,
plus a row of every unit sharing the square (the selected one outlined cyan)."
  (let* ((font (painter-font painter))
         (ren (painter-ren painter))
         (h (gfont-height font))
         (map (civm:gs-map state))
         (tile (civm:tile-at map (civm:unit-x u) (civm:unit-y u)))
         (terr (civm:tile-terrain tile))
         (type (civm:unit-type u))
         (owner (civm:player-by-id state (civm:unit-owner u)))
         (cid (civm:tile-city tile))
         (city (and cid (civm:city-by-id state cid)))
         (units (remove nil (mapcar (lambda (id) (civm:unit-by-id state id))
                                    (civm:tile-units tile))))
         (lines (remove nil
                        (list (and owner (civm:player-name owner))
                              (string-capitalize (symbol-name type))
                              (format nil "Atk ~D  Def ~D"
                                      (civm:unit-def type :attack 0)
                                      (civm:unit-def type :defense 0))
                              (format nil "Moves: ~D  HP ~D"
                                      (civm:unit-moves-left u) (civm:unit-hp u))
                              (and (plusp (civm:unit-def type :range 0))
                                   (format nil "Fuel: ~D" (civm:unit-fuel u)))
                              (let ((w (civm:unit-work u)))
                                (and w (format nil "~A ~D"
                                               (string-capitalize
                                                (civm:terraform-def w :verb))
                                               (civm:unit-work-left u))))
                              (and city (civm:city-name city))
                              (format nil "(~A)" (string-capitalize (symbol-name terr)))
                              (format nil "F~D S~D T~D  Def+~D%"
                                      (civm:terrain-def terr :food 0)
                                      (civm:terrain-def terr :shields 0)
                                      (civm:terrain-def terr :trade 0)
                                      (civm:terrain-def terr :defense 0)))))
         (text-h (* (length lines) (1+ h)))
         (row-w (if units (+ 2 (* (length units) (1+ *tile*))) 0))
         (pw (+ 4 (max (reduce #'max lines :key (lambda (s) (text-width font s))) row-w)))
         (ph (+ 4 text-h (if units (+ 2 *tile*) 0)))
         (py (- (* (civm:map-height map) *tile*) ph)))
    ;; panel background + owner-coloured accent line along the top
    (sdl2:set-render-draw-color ren 0 0 0 200)
    (set-rect (painter-dst painter) 0 py pw ph)
    (sdl2:render-fill-rect ren (painter-dst painter))
    (destructuring-bind (r g b) (owner-color state (civm:unit-owner u))
      (sdl2:set-render-draw-color ren r g b 255))
    (set-rect (painter-dst painter) 0 py pw 1)
    (sdl2:render-fill-rect ren (painter-dst painter))
    ;; text lines
    (loop for line in lines for i from 0
          do (draw-text painter font line 2 (+ py 2 (* i (1+ h))) 255 255 255))
    ;; the units stacked on this square; the selected one gets a cyan outline
    (loop with sy = (+ py 2 text-h)
          for ou in units for i from 0
          for sx = (+ 2 (* i (1+ *tile*)))
          for spr = (unit-sprite (civm:unit-type ou))
          do ;; team-colour fill behind the keyed-out sprite background
             (destructuring-bind (r g b) (owner-color state (civm:unit-owner ou))
               (sdl2:set-render-draw-color ren r g b 255))
             (set-rect (painter-dst painter) sx sy *tile* *tile*)
             (sdl2:render-fill-rect ren (painter-dst painter))
             (draw-sprite painter (car spr) (cdr spr) sx sy)
             ;; outline: cyan for the selected unit, owner colour otherwise
             (destructuring-bind (r g b)
                 (if (eql (civm:unit-id ou) (civm:unit-id u))
                     '(0 240 240)
                     (owner-color state (civm:unit-owner ou)))
               (sdl2:set-render-draw-color ren r g b 255))
             (set-rect (painter-dst painter) sx sy *tile* *tile*)
             (sdl2:render-draw-rect ren (painter-dst painter)))))

;;; --- build menu ------------------------------------------------------------

(defparameter *menu-x* 92 "Build-menu panel position (logical px).")
(defparameter *menu-y* 52)

(defparameter *unit-order*
  '(:warriors :cavalry :legion :phalanx :diplomat :musketeers :riflemen :cannon
    :catapult :chariot :frigate :knights :sail :settlers :trireme :caravan :mech-inf
    :submarine :transport :artillery :fighter :ironclad :armor :cruiser :bomber
    :battleship :carrier :nuclear))
(defparameter *improvement-order*
  '(:barracks :temple :granary :courthouse :library :marketplace :colosseum :aqueduct
    :bank :walls :cathedral :mass-transit :nuclear-plant :power-plant :university
    :factory :palace :recycling-center :sdi-defense :hydro-plant :mfg-plant))
(defparameter *wonder-order*
  '(:colossus :lighthouse :copernicus-observatory :darwins-voyage :great-library
    :great-wall :hanging-gardens :michelangelos-chapel :oracle :pyramids
    :isaac-newtons-college :j-s-bachs-cathedral :magellans-expedition
    :shakespeares-theatre :apollo-program :cure-for-cancer :hoover-dam
    :manhattan-project :s-e-t-i-program :united-nations :womens-suffrage))

(defun item-cost (item)
  (ecase (first item)
    (:unit (civm:unit-def (second item) :cost 0))
    (:building (civm:building-def (second item) :cost 0))
    (:wonder (civm:wonder-def (second item) :cost 0))
    (:spaceship civm:*spaceship-part-cost*)))

(defun item-label (item)
  (if (eq (first item) :spaceship) "Spaceship part"
      (string-capitalize (symbol-name (second item)))))

(defun buildable-items (state city)
  "Production items (:unit/:building/:wonder ...) CITY can currently build."
  (let ((owner (civm:player-by-id state (civm:city-owner city)))
        (items '()))
    (dolist (type *unit-order*)
      (when (and (civm:player-has-tech-p owner (civm:unit-def type :requires))
                 (not (civm:unit-obsolete-p owner type)))   ; retired units leave the menu
        (push (list :unit type) items)))
    (dolist (b *improvement-order*)        ; improvements not already built here
      (when (and (civm:player-has-tech-p owner (civm:building-def b :requires))
                 (not (member b (civm:city-buildings city))))
        (push (list :building b) items)))
    (dolist (w *wonder-order*)             ; wonders not yet built anywhere
      (when (and (civm:player-has-tech-p owner (civm:wonder-def w :requires))
                 (not (civm:wonder-built-p state w)))
        (push (list :wonder w) items)))
    (when (and (civm:wonder-built-p state :apollo-program)   ; spaceship parts
               (civm:player-has-tech-p owner :space-flight))
      (push (list :spaceship) items))
    (nreverse items)))

(defun build-menu-lines (state city)
  "List of (index item label) for CITY's build menu (1-based index)."
  (loop for item in (buildable-items state city)
        for i from 1
        collect (list i item
                      (format nil "~D ~A (~D)~A" i
                              (item-label item)
                              (item-cost item)
                              (if (eq (first item) :wonder) " *" "")))))

(defun build-menu-pick (painter state city ly)
  "Return the unit type whose menu line is at logical y LY, or NIL."
  (let ((row (floor (- ly (+ *menu-y* 2)) (1+ (gfont-height (painter-font painter)))))
        (lines (build-menu-lines state city)))
    (when (and (>= row 1) (<= row (length lines)))
      (second (nth (1- row) lines)))))

(defun built-effect (key)
  "Human-readable effect string for a built improvement or wonder."
  (or (civm:building-def key :effect) (civm:wonder-def key :effect) ""))

(defun built-lines (city)
  "Strings (\"Name\" or \"Name - effect\") for the city's improvements/wonders."
  (loop for key in (append *improvement-order* *wonder-order*)
        when (member key (civm:city-buildings city))
          collect (let ((eff (built-effect key))
                        (name (string-capitalize (symbol-name key))))
                    (if (string= eff "") name (format nil "~A - ~A" name eff)))))

(defun city-mood-lines (state city)
  "Trailing status lines for the build menu: citizen mood and a disorder /
celebration banner."
  (multiple-value-bind (happy content unhappy)
      (civm:city-happiness state city (nth-value 2 (civm:city-yields state city)))
    (list* (format nil "Mood: ~D happy ~D content ~D unhappy" happy content unhappy)
           (cond ((civm:city-disorder-p state city)   (list "** CIVIL DISORDER **"))
                 ((civm:city-celebrating-p state city) (list "** CELEBRATING **"))
                 (t nil)))))

(defun draw-build-menu (painter state city)
  (let* ((font (painter-font painter)) (ren (painter-ren painter))
         (h (gfont-height font))
         (lines (build-menu-lines state city))
         (built (built-lines city))
         (mood (city-mood-lines state city))
         (title (format nil "Build (~A):" (civm:city-name city)))
         ;; layout rows (kept stable so mouse-picking maps to buildable items):
         ;; title | buildable... | [Built: ...] | mood...
         (texts (append (list title) (mapcar #'third lines)
                        (when built (cons "Built:" built)) mood))
         (pw (+ 4 (reduce #'max texts :key (lambda (s) (text-width font s)))))
         (ph (+ 4 (* (length texts) (1+ h)))))
    (sdl2:set-render-draw-color ren 0 0 0 230)
    (set-rect (painter-dst painter) *menu-x* *menu-y* pw ph)
    (sdl2:render-fill-rect ren (painter-dst painter))
    (sdl2:set-render-draw-color ren 220 220 220 255)
    (sdl2:render-draw-rect ren (painter-dst painter))
    (flet ((line (text row r g b)
             (draw-text painter font text (+ *menu-x* 2)
                        (+ *menu-y* 2 (* row (1+ h))) r g b)))
      (line title 0 255 230 120)                               ; title
      (loop for (i item label) in lines                        ; buildable
            do (let ((cur (equal (civm:city-production city) item)))
                 (line label i (if cur 120 255) 255 (if cur 120 255))))
      (let ((row 1))                                           ; below the buildables
        (incf row (length lines))
        (when built                                            ; already built
          (line "Built:" row 180 180 180) (incf row)
          (loop for s in built do (line s row 150 200 150) (incf row)))
        (loop for s in mood for k from 0                       ; mood / banner
              do (line s row
                       (if (zerop k) 200 255)
                       (if (zerop k) 200 120)
                       (if (zerop k) 120 120))
                 (incf row))))))

;;; --- government menu (revolution) ------------------------------------------

(defparameter *gov-order* '(:despotism :monarchy :communism :republic :democracy))

(defun gov-menu-lines (state)
  "(index gov label available-p) for each selectable government."
  (let ((p (human-player state)))
    (loop for g in *gov-order* for i from 1
          for ok = (civm:player-has-tech-p p (civm:government-def g :requires))
          collect (list i g
                        (format nil "~D ~A~A" i (civm:government-def g :name)
                                (cond ((eq g (civm:player-government p)) " (current)")
                                      ((not ok) " (locked)")
                                      (t "")))
                        ok))))

(defun gov-menu-pick (painter state ly)
  "Government at logical y LY in the menu, or NIL (locked/out of range)."
  (let ((row (floor (- ly (+ *menu-y* 2)) (1+ (gfont-height (painter-font painter)))))
        (lines (gov-menu-lines state)))
    (when (and (>= row 1) (<= row (length lines)))
      (let ((entry (nth (1- row) lines)))
        (when (fourth entry) (second entry))))))

(defun draw-gov-menu (painter state)
  (let* ((font (painter-font painter)) (ren (painter-ren painter))
         (h (gfont-height font))
         (p (human-player state))
         (lines (gov-menu-lines state))
         (title "Revolution! New government:")
         (foot "Esc cancels — 1 turn of anarchy")
         (texts (append (list title) (mapcar #'third lines) (list foot)))
         (pw (+ 4 (reduce #'max texts :key (lambda (s) (text-width font s)))))
         (ph (+ 4 (* (length texts) (1+ h)))))
    (declare (ignore p))
    (sdl2:set-render-draw-color ren 0 0 0 230)
    (set-rect (painter-dst painter) *menu-x* *menu-y* pw ph)
    (sdl2:render-fill-rect ren (painter-dst painter))
    (sdl2:set-render-draw-color ren 220 220 220 255)
    (sdl2:render-draw-rect ren (painter-dst painter))
    (flet ((line (text row r g b)
             (draw-text painter font text (+ *menu-x* 2)
                        (+ *menu-y* 2 (* row (1+ h))) r g b)))
      (line title 0 255 230 120)
      (loop for (i g label ok) in lines
            do (progn g)
               (line label i (if ok 255 120) (if ok 255 120) 120))
      (line foot (1+ (length lines)) 160 160 160))))

(defparameter *help-lines*
  '("CONTROLS"
    "Arrows / Numpad  move (numpad = 8-way)"
    "Tab  next unit      W  wait"
    "B  found city       F  fortify"
    "Shift+D disband  U upgrade  N detonate nuke"
    "R / I / M  road (then rail) / irrigate / mine"
    "T fort  A airbase  C clear forest/jungle/swamp  P de-pollute"
    "G then click  go to a tile"
    "V  revolution   Y  diplomacy   E  trade"
    "Z / X  diplomat steal / sabotage   D  spy menu"
    "H / J  caravan: help wonder / trade route"
    ",/.  luxury -/+"
    "Enter  end turn"
    "S / L  save / load game"
    "Left-click  select unit/city; empty: recenter"
    "~  Lisp console   K  Slynk server   ?  help"
    "Esc  close menu / quit")
  "Lines shown in the help overlay (first line is the title).")

(defun draw-help (painter state)
  "A keybinding cheat-sheet overlay, centred on the map."
  (let* ((font (painter-font painter)) (ren (painter-ren painter))
         (map (civm:gs-map state))
         (h (gfont-height font))
         (pw (+ 8 (reduce #'max *help-lines* :key (lambda (s) (text-width font s)))))
         (ph (+ 8 (* (length *help-lines*) (1+ h))))
         (px (max 0 (floor (- (* (civm:map-width map) *tile*) pw) 2)))
         (py (max 0 (floor (- (* (civm:map-height map) *tile*) ph) 2))))
    (sdl2:set-render-draw-color ren 0 0 0 235)
    (set-rect (painter-dst painter) px py pw ph)
    (sdl2:render-fill-rect ren (painter-dst painter))
    (sdl2:set-render-draw-color ren 220 220 220 255)
    (sdl2:render-draw-rect ren (painter-dst painter))
    (loop for line in *help-lines* for i from 0
          do (draw-text painter font line (+ px 4) (+ py 4 (* i (1+ h)))
                        (if (zerop i) 255 220) (if (zerop i) 230 220)
                        (if (zerop i) 120 220)))))

(defun draw-console (painter state input output)
  "The `~` Lisp console: OUTPUT lines from the last eval, then the input line
with a blinking caret (the bitmap font has no underscore glyph)."
  (declare (ignore state))
  (let* ((font (painter-font painter)) (ren (painter-ren painter))
         (h (gfont-height font))
         (clip (lambda (s) (if (> (length s) 52) (subseq s 0 52) s)))
         (prompt-line (funcall clip (format nil "> ~A" input)))
         (lines (append (mapcar clip output) (list prompt-line)))
         (last (1- (length lines)))
         (pw (+ 8 (reduce #'max lines :key (lambda (s) (text-width font s)))))
         (ph (+ 4 (* (length lines) (1+ h)))))
    (sdl2:set-render-draw-color ren 0 0 0 240)
    (set-rect (painter-dst painter) 0 0 pw ph)
    (sdl2:render-fill-rect ren (painter-dst painter))
    (sdl2:set-render-draw-color ren 120 200 120 255)
    (sdl2:render-draw-rect ren (painter-dst painter))
    (loop for s in lines for i from 0
          for prompt = (= i last)
          for err = (and (>= (length s) 5) (string= "ERROR" (subseq s 0 5)))
          do (draw-text painter font s 2 (+ 2 (* i (1+ h)))
                        (cond (err 255) (prompt 255) (t 180))
                        (cond (err 120) (prompt 255) (t 230))
                        (cond (err 120) (prompt 120) (t 180))))
    ;; a blinking caret just past the input text
    (when (blink-on-p)
      (sdl2:set-render-draw-color ren 255 255 120 255)
      (set-rect (painter-dst painter)
                (+ 2 (text-width font prompt-line)) (+ 2 (* last (1+ h))) 4 h)
      (sdl2:render-fill-rect ren (painter-dst painter)))))

(defun gov-hud-text (state)
  "Government + rate readout for the HUD, noting a pending revolution."
  (let ((p (human-player state)))
    (when p
      (format nil "~A  T~D/L~D/S~D~@[ ->~A~]"
              (civm:government-def (civm:player-government p) :name)
              (civm:player-tax-rate p) (civm:player-luxury-rate p)
              (civm:player-science-rate p)
              (let ((tgt (civm:player-gov-target p)))
                (and tgt (civm:government-def tgt :name)))))))

(defun pop-hud-text (state)
  "Total civilization population for the HUD (Civ1 grouping with commas)."
  (let ((p (human-player state)))
    (when p
      (format nil "Pop ~:D" (civm:civ-population state (civm:player-id p))))))

(defun gold-hud-text (state)
  "Gold reserves for the HUD."
  (let ((p (human-player state)))
    (when p (format nil "Gold ~D" (civm:player-gold p)))))

(defun science-hud-text (state)
  "Current research target and percent progress toward the next advance."
  (let ((p (human-player state)))
    (when p
      (let ((tech (civm:player-researching p)))
        (if tech
            (format nil "Sci ~A ~D%"
                    (civm:tech-def tech :name)
                    (max 0 (min 99 (floor (* (civm:player-beakers p) 100)
                                          (max 1 (civm:research-cost p))))))
            "Sci (choosing)")))))

;;; --- diplomacy menu --------------------------------------------------------

(defun diplo-menu-lines (state)
  "(index other-id label) for each rival civilization the human can treat with."
  (let ((me (civm:player-id (human-player state))))
    (loop for p across (civm:gs-players state)
          for oid = (civm:player-id p)
          when (/= oid me)
            collect (let ((war (civm:at-war-p state me oid)))
                      (list nil oid
                            (format nil "~A ~A -> ~A" (civm:player-name p)
                                    (if war "[WAR]" "[peace]")
                                    (if war "make peace" "declare war"))))
              into rows
          finally (return (loop for r in rows for i from 1
                                collect (list i (second r) (third r)))))))

(defun diplo-menu-pick (painter state ly)
  (let ((row (floor (- ly (+ *menu-y* 2)) (1+ (gfont-height (painter-font painter)))))
        (lines (diplo-menu-lines state)))
    (when (and (>= row 1) (<= row (length lines)))
      (second (nth (1- row) lines)))))

(defun draw-diplo-menu (painter state)
  (let* ((font (painter-font painter)) (ren (painter-ren painter))
         (h (gfont-height font))
         (lines (diplo-menu-lines state))
         (title "Diplomacy:")
         (texts (cons title (mapcar #'third lines)))
         (pw (+ 4 (reduce #'max texts :key (lambda (s) (text-width font s)))))
         (ph (+ 4 (* (length texts) (1+ h)))))
    (sdl2:set-render-draw-color ren 0 0 0 230)
    (set-rect (painter-dst painter) *menu-x* *menu-y* pw ph)
    (sdl2:render-fill-rect ren (painter-dst painter))
    (sdl2:set-render-draw-color ren 220 220 220 255)
    (sdl2:render-draw-rect ren (painter-dst painter))
    (flet ((line (text row r g b)
             (draw-text painter font text (+ *menu-x* 2)
                        (+ *menu-y* 2 (* row (1+ h))) r g b)))
      (line title 0 255 230 120)
      (loop for (i oid label) in lines
            do (progn oid)
               (destructuring-bind (r g b) (owner-color state oid)
                 (line label i r g b))))))

;;; --- trade menu ------------------------------------------------------------

(defun trade-menu-lines (state)
  "(index other-id deal label) for each rival the human can offer a trade."
  (let ((me (civm:player-id (human-player state))))
    (loop for p across (civm:gs-players state)
          for oid = (civm:player-id p)
          when (and (/= oid me) (not (civm:barbarian-id-p state oid)))
            collect (let ((best (civm:best-trade-with state me oid)))
                      (list oid (and best (cdr best))
                            (format nil "~A: ~A" (civm:player-name p)
                                    (if best (car best) "no deal"))))
              into rows
          finally (return (loop for r in rows for i from 1
                                collect (list* i r))))))

(defun trade-menu-pick (painter state ly)
  (let ((row (floor (- ly (+ *menu-y* 2)) (1+ (gfont-height (painter-font painter)))))
        (lines (trade-menu-lines state)))
    (when (and (>= row 1) (<= row (length lines)))
      (let ((entry (nth (1- row) lines)))   ; (i oid deal label)
        (and (third entry) entry)))))

(defun draw-trade-menu (painter state)
  (let* ((font (painter-font painter)) (ren (painter-ren painter))
         (h (gfont-height font))
         (lines (trade-menu-lines state))
         (title "Trade (gold & tech):")
         (texts (cons title (mapcar #'fourth lines)))
         (pw (+ 4 (reduce #'max texts :key (lambda (s) (text-width font s)))))
         (ph (+ 4 (* (length texts) (1+ h)))))
    (sdl2:set-render-draw-color ren 0 0 0 230)
    (set-rect (painter-dst painter) *menu-x* *menu-y* pw ph)
    (sdl2:render-fill-rect ren (painter-dst painter))
    (sdl2:set-render-draw-color ren 220 220 220 255)
    (sdl2:render-draw-rect ren (painter-dst painter))
    (flet ((line (text row r g b)
             (draw-text painter font text (+ *menu-x* 2)
                        (+ *menu-y* 2 (* row (1+ h))) r g b)))
      (line title 0 255 230 120)
      (loop for (i oid deal label) in lines
            do (progn oid)
               (line label i (if deal 230 130) (if deal 230 130) (if deal 150 130))))))

;;; --- diplomat (spy) action menu --------------------------------------------

(defparameter *diplomat-actions*
  '((:steal-tech "Steal tech" :city) (:sabotage "Sabotage" :city)
    (:investigate "Investigate" :city) (:establish-embassy "Establish embassy" :city)
    (:incite-revolt "Incite revolt" :city) (:bribe-unit "Bribe unit" :unit))
  "(command label target-kind) for the spy menu; :city or :unit must be adjacent.")

(defun spy-menu-lines (state unit)
  "(index command label enabled-p) for each diplomat action available to UNIT."
  (loop for (cmd label need) in *diplomat-actions* for i from 1
        for target = (if (eq need :unit) (civm:adjacent-enemy-unit state unit)
                         (civm:adjacent-enemy-city state unit))
        collect (list i cmd
                      (format nil "~D ~A~A" i label
                              (cond ((not target) " (no target)")
                                    ((eq cmd :incite-revolt)
                                     (format nil " ~Dg" (civm:incite-cost target)))
                                    (t "")))
                      (and target t))))

(defun spy-menu-pick (painter state unit ly)
  (let ((row (floor (- ly (+ *menu-y* 2)) (1+ (gfont-height (painter-font painter)))))
        (lines (spy-menu-lines state unit)))
    (when (and (>= row 1) (<= row (length lines)))
      (let ((e (nth (1- row) lines)))
        (when (fourth e) (second e))))))      ; the command, if enabled

(defun draw-spy-menu (painter state unit)
  (let* ((font (painter-font painter)) (ren (painter-ren painter))
         (h (gfont-height font))
         (lines (spy-menu-lines state unit))
         (title "Diplomat:")
         (texts (cons title (mapcar #'third lines)))
         (pw (+ 4 (reduce #'max texts :key (lambda (s) (text-width font s)))))
         (ph (+ 4 (* (length texts) (1+ h)))))
    (sdl2:set-render-draw-color ren 0 0 0 230)
    (set-rect (painter-dst painter) *menu-x* *menu-y* pw ph)
    (sdl2:render-fill-rect ren (painter-dst painter))
    (sdl2:set-render-draw-color ren 220 220 220 255)
    (sdl2:render-draw-rect ren (painter-dst painter))
    (flet ((line (text row r g b)
             (draw-text painter font text (+ *menu-x* 2)
                        (+ *menu-y* 2 (* row (1+ h))) r g b)))
      (line title 0 255 230 120)
      (loop for (i cmd label ok) in lines
            do (progn cmd) (line label i (if ok 235 130) (if ok 235 130) (if ok 150 130))))))

(defun spaceship-hud-text (state)
  "Spaceship progress for the human, or NIL."
  (let ((p (human-player state)))
    (when p
      (cond ((plusp (civm:player-landing p))
             (format nil "SHIP launched -> turn ~D" (civm:player-landing p)))
            ((plusp (civm:player-spaceship p))
             (format nil "SHIP ~D/~D parts" (civm:player-spaceship p) civm:*spaceship-parts*))))))

(defun draw-banner (painter state view-w)
  "A VICTORY / DEFEAT banner centred in the VIEW-W-wide viewport."
  (let* ((font (painter-font painter)) (ren (painter-ren painter))
         (human (human-player state))
         (win (and human (eql (civm:gs-winner state) (civm:player-id human))))
         (who (civm:player-name (civm:player-by-id state (civm:gs-winner state))))
         (kind (string-downcase (symbol-name (civm:gs-victory state))))
         (msg (if win (format nil "VICTORY by ~A!" kind)
                  (format nil "DEFEAT -- ~A wins by ~A" who kind)))
         (tw (text-width font msg)) (h (gfont-height font))
         (px (max 0 (floor (- view-w (+ tw 8)) 2))) (py 100))
    (sdl2:set-render-draw-color ren 0 0 0 235)
    (set-rect (painter-dst painter) px py (+ tw 8) (+ 6 h))
    (sdl2:render-fill-rect ren (painter-dst painter))
    (sdl2:set-render-draw-color ren (if win 90 230) (if win 220 70) 90 255)
    (sdl2:render-draw-rect ren (painter-dst painter))
    (draw-text painter font msg (+ px 4) (+ py 3)
               (if win 120 255) (if win 255 120) 120)))

(defun draw-explosion-frame (painter frame px py)
  "Blit detonation FRAME (0..+NUKE-FRAMES+-1) from the nuke sheet, scaled to a
3x3-tile burst centred on the tile drawn at screen pixel (PX,PY)."
  (let ((tex (painter-nuke painter)))
    (when tex
      (set-rect (painter-src painter)
                (1+ (* (mod frame +nuke-cols+) +nuke-pitch+))
                (1+ (* (floor frame +nuke-cols+) +nuke-pitch+))
                +nuke-frame+ +nuke-frame+)
      (set-rect (painter-dst painter) (- px *tile*) (- py *tile*) (* 3 *tile*) (* 3 *tile*))
      (sdl2:render-copy (painter-ren painter) tex
                        :source-rect (painter-src painter)
                        :dest-rect (painter-dst painter)))))

(defun render-game (painter state selected-id
                    &key (fog t) build-city gov-menu diplo-menu trade-menu spy-menu help console
                         overlay (cam-x 0) (cam-y 0) (vw 20) (vh 15))
  "Draw STATE through a VW x VH-tile viewport whose top-left world tile is
(CAM-X, CAM-Y); the map wraps east-west.  With FOG, unexplored tiles are black,
explored-but-unseen tiles are dimmed, and units/cities show only while visible."
  (let* ((ren (painter-ren painter))
         (map (civm:gs-map state))
         (w (civm:map-width map))
         (human (and fog (human-player state)))
         (vis (and human (civm:visible-set state human))))
    (labels ((visible (x y) (or (not human) (gethash (+ x (* y w)) vis)))
             (scol (x) (let ((c (mod (- x cam-x) w))) (when (< c vw) c))) ; screen col or NIL
             (srow (y) (let ((r (- y cam-y))) (when (<= 0 r (1- vh)) r))))
      (sdl2:set-render-draw-color ren 0 0 0 255)
      (sdl2:render-clear ren)
      ;; terrain across the viewport (x wraps around the cylinder)
      (dotimes (sy vh)
        (dotimes (sx vw)
          (let ((wx (civm:wrap-x map (+ cam-x sx))) (wy (+ cam-y sy)))
            (when (and (civm:tile-at map wx wy)
                       (or (not human) (civm:seen-p state human wx wy)))
              (let ((px (* sx *tile*)) (py (* sy *tile*)))
                (draw-terrain-tile painter state wx wy px py)
                (when (civm:tile-hut (civm:tile-at map wx wy))   ; tribal hut overlay
                  (draw-sprite painter (car +hut-sprite+) (cdr +hut-sprite+) px py))
                (unless (visible wx wy) (dim-tile painter px py)))))))
      ;; cities (only those in view and currently visible)
      (maphash (lambda (id c) (declare (ignore id))
                 (let ((sx (scol (civm:city-x c))) (sy (srow (civm:city-y c))))
                   (when (and sx sy (visible (civm:city-x c) (civm:city-y c)))
                     (draw-city painter state c (* sx *tile*) (* sy *tile*)))))
               (civm:gs-cities state))
      ;; units, except the selected one and city garrisons (the city stands in
      ;; for a garrison; the selected unit is drawn last so it's on top)
      (maphash (lambda (id u)
                 (let ((ux (civm:unit-x u)) (uy (civm:unit-y u)))
                   (let ((sx (scol ux)) (sy (srow uy)))
                     (when (and sx sy (visible ux uy)
                                (not (eql id selected-id))
                                (not (civm:tile-city (civm:tile-at map ux uy))))
                       (draw-unit painter state u (* sx *tile*) (* sy *tile*))))))
               (civm:gs-units state))
      ;; the selected unit blinks on top of everything on its square -- but a
      ;; unit auto-travelling under a goto order is drawn solid (no blink)
      (let* ((sel (and selected-id (civm:unit-by-id state selected-id)))
             (traveling (and sel (eq (civm:unit-orders sel) :goto)))
             (sx (and sel (scol (civm:unit-x sel))))
             (sy (and sel (srow (civm:unit-y sel)))))
        (when (and sel sx sy (visible (civm:unit-x sel) (civm:unit-y sel))
                   (or traveling (blink-on-p)))
          (draw-unit painter state sel (* sx *tile*) (* sy *tile*))
          (draw-border painter (* sx *tile*) (* sy *tile*) '(255 240 60)))
        ;; stats panel for the selected unit (hidden while the build menu is up)
        (when (and sel (not build-city) (painter-font painter))
          (draw-unit-panel painter state sel)))
      ;; build / government / diplomacy menu overlay (mutually exclusive)
      (cond ((and gov-menu (painter-font painter))
             (draw-gov-menu painter state))
            ((and diplo-menu (painter-font painter))
             (draw-diplo-menu painter state))
            ((and trade-menu (painter-font painter))
             (draw-trade-menu painter state))
            ((and spy-menu selected-id (painter-font painter))
             (let ((u (civm:unit-by-id state selected-id)))
               (when u (draw-spy-menu painter state u))))
            ((and build-city (painter-font painter))
             (let ((c (civm:city-by-id state build-city)))
               (when c (draw-build-menu painter state c)))))
      ;; HUD, top-left: a Civ1-style status pane -- date, population, gold, the
      ;; government and tax/lux/science split, research progress, spaceship status
      (let ((font (painter-font painter)))
        (when font
          (let* ((fh (gfont-height font))
                 (lines (remove nil (list (format nil "~A   TURN ~D"
                                                  (year-text (civm:gs-year state))
                                                  (civm:gs-turn state))
                                          (pop-hud-text state)
                                          (gold-hud-text state)
                                          (gov-hud-text state)
                                          (science-hud-text state)
                                          (spaceship-hud-text state))))
                 (tw (reduce #'max lines :key (lambda (s) (text-width font s)))))
            (sdl2:set-render-draw-color ren 0 0 0 190)
            (set-rect (painter-dst painter) 0 0 (+ tw 2) (+ 1 (* (length lines) (1+ fh))))
            (sdl2:render-fill-rect ren (painter-dst painter))
            (loop for s in lines for i from 0
                  do (draw-text painter font s 1 (+ 1 (* i (1+ fh)))
                                (if (zerop i) 255 200) (if (zerop i) 255 220)
                                (if (zerop i) 255 255))))))
      ;; victory / defeat banner
      (when (and (civm:gs-winner state) (painter-font painter))
        (draw-banner painter state (* vw *tile*)))
      ;; help overlay, drawn last so it sits on top of everything
      (when (and help (painter-font painter))
        (draw-help painter state))
      ;; the `~` console sits on top of even the help overlay
      (when (and console (painter-font painter))
        (draw-console painter state (car console) (cdr console)))
      ;; a transient effect overlay (e.g. a nuclear detonation animation frame)
      (when overlay (funcall overlay painter))
      (sdl2:render-present ren))))
