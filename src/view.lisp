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
  '((:settlers . (0 . 10)) (:warriors . (1 . 10)) (:phalanx . (2 . 10))
    (:legion . (3 . 10)) (:catapult . (7 . 10)))
  "civ-model unit type -> (col . row) in SP257.")
(defparameter +default-unit-sprite+ '(1 . 10))
(defparameter +city-sprite+ '(12 . 7))   ; SP257 col 12, row 7 (tile_007_012)

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
  '((1 80 150 235) (2 220 70 70) (3 90 200 120) (4 230 200 80))
  "Player color index -> (r g b).")

(defun owner-color (state owner-id)
  (let ((p (and owner-id (civm:player-by-id state owner-id))))
    (if p (or (cdr (assoc (civm:player-color p) *player-colors*)) '(230 230 230))
        '(180 180 180))))

;;; --- painter (reuses two rects to avoid per-draw allocation) ---------------

(defstruct (painter (:constructor make-painter (ren sprites terrain src dst)))
  ren sprites terrain src dst (font nil))

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

(defun draw-marker (p tx ty w h rgb)
  "Fill a small W x H rectangle at the top-left of tile (TX,TY)."
  (destructuring-bind (r g b) rgb
    (sdl2:set-render-draw-color (painter-ren p) r g b 255)
    (set-rect (painter-dst p) (+ (* tx *tile*) 1) (+ (* ty *tile*) 1) w h)
    (sdl2:render-fill-rect (painter-ren p) (painter-dst p))))

(defun draw-frame (p tx ty rgb &optional (inset 0))
  "Draw a rectangle outline INSET pixels inside tile (TX,TY)."
  (destructuring-bind (r g b) rgb
    (sdl2:set-render-draw-color (painter-ren p) r g b 255)
    (set-rect (painter-dst p) (+ (* tx *tile*) inset) (+ (* ty *tile*) inset)
              (- *tile* (* 2 inset)) (- *tile* (* 2 inset)))
    (sdl2:render-draw-rect (painter-ren p) (painter-dst p))))

(defun draw-border (p tx ty rgb) (draw-frame p tx ty rgb 0))

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

(defun draw-terrain-tile (p state x y)
  (let* ((map (civm:gs-map state))
         (tile (civm:tile-at map x y))
         (terr (civm:tile-terrain tile))
         (px (* x *tile*)) (py (* y *tile*)))
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
    ;; rivers (SP257 connection variants) then the special-resource icon
    (when (civm:tile-river tile)
      (blit p (painter-sprites p) (* (river-mask map x y) *tile*) +river-row+
            *tile* *tile* px py))
    (when (civm:tile-special tile)
      (draw-special p terr px py))))

;;; --- the frame -------------------------------------------------------------

(defun load-atlas (ren path)
  "Load PATH as a blend-enabled texture (caller destroys it)."
  (let* ((surf (sdl2-image:load-image (namestring path)))
         (tex (sdl2:create-texture-from-surface ren surf)))
    (sdl2-ffi.functions:sdl-free-surface surf)
    (sdl2-ffi.functions:sdl-set-texture-blend-mode tex 1) ; SDL_BLENDMODE_BLEND
    tex))

(defun dim-tile (p tx ty)
  "Darken a tile (explored but not currently visible) with a translucent wash."
  (sdl2:set-render-draw-color (painter-ren p) 0 0 0 120)
  (set-rect (painter-dst p) (* tx *tile*) (* ty *tile*) *tile* *tile*)
  (sdl2:render-fill-rect (painter-ren p) (painter-dst p)))

(defun human-player (state)
  (find :human (civm:gs-players state) :key #'civm:player-kind))

(defun draw-city (painter state city)
  "Civ1-style city: skyline, optional walls, a size box, an owner/black border
(black when a military unit garrisons it), and a name label below."
  (let* ((cx (civm:city-x city)) (cy (civm:city-y city))
         (px (* cx *tile*)) (py (* cy *tile*))
         (font (painter-font painter))
         (ren (painter-ren painter)))
    ;; the city sprite is transparent, so paint the owner's colour square first
    (destructuring-bind (r g b) (owner-color state (civm:city-owner city))
      (sdl2:set-render-draw-color ren r g b 255)
      (set-rect (painter-dst painter) px py *tile* *tile*)
      (sdl2:render-fill-rect ren (painter-dst painter)))
    (draw-sprite painter (car +city-sprite+) (cdr +city-sprite+) px py)
    (when (member :walls (civm:city-buildings city))
      (draw-frame painter cx cy '(180 178 160) 1))          ; stone walls
    ;; a black border marks a city garrisoned by a military unit
    (when (civm:city-defended-p state city)
      (draw-border painter cx cy '(0 0 0)))
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

(defun draw-unit (painter state u)
  "Draw unit U's sprite, owner border and fortify marker."
  (let ((spr (unit-sprite (civm:unit-type u)))
        (ux (civm:unit-x u)) (uy (civm:unit-y u)))
    (draw-sprite painter (car spr) (cdr spr) (* ux *tile*) (* uy *tile*))
    (draw-border painter ux uy (owner-color state (civm:unit-owner u)))
    (when (eq (civm:unit-orders u) :fortified)
      (draw-marker painter ux uy 3 3 '(245 245 245)))))

(defun blink-on-p ()
  "Toggles every 500 ms (drives the selected-unit blink)."
  (evenp (floor (sdl2-ffi.functions:sdl-get-ticks) 500)))

(defun render-game (painter state selected-id &key (fog t))
  "Draw STATE from the human player's perspective.  With FOG, unexplored tiles
are black, explored-but-unseen tiles are dimmed, and units/cities are shown
only on currently-visible tiles."
  (let* ((ren (painter-ren painter))
         (map (civm:gs-map state))
         (w (civm:map-width map))
         (human (and fog (human-player state)))
         (vis (and human (civm:visible-set state human))))
    (flet ((visible (x y) (or (not human) (gethash (+ x (* y w)) vis))))
      (sdl2:set-render-draw-color ren 0 0 0 255)
      (sdl2:render-clear ren)
      ;; terrain (only explored tiles; dim the ones not currently in sight)
      (civm:do-tiles (x y tile map)
        (declare (ignore tile))
        (when (or (not human) (civm:seen-p state human x y))
          (draw-terrain-tile painter state x y)
          (unless (visible x y) (dim-tile painter x y))))
      ;; cities and units (only where currently visible)
      (maphash (lambda (id c) (declare (ignore id))
                 (when (visible (civm:city-x c) (civm:city-y c))
                   (draw-city painter state c)))
               (civm:gs-cities state))
      ;; units, except the selected one and city garrisons (the city stands in
      ;; for a garrison; the selected unit is drawn last so it's on top)
      (maphash (lambda (id u)
                 (let ((ux (civm:unit-x u)) (uy (civm:unit-y u)))
                   (when (and (visible ux uy)
                              (not (eql id selected-id))
                              (not (civm:tile-city (civm:tile-at map ux uy))))
                     (draw-unit painter state u))))
               (civm:gs-units state))
      ;; the selected unit blinks on top of everything on its square
      (let ((sel (and selected-id (civm:unit-by-id state selected-id))))
        (when (and sel (visible (civm:unit-x sel) (civm:unit-y sel)) (blink-on-p))
          (draw-unit painter state sel)
          (draw-border painter (civm:unit-x sel) (civm:unit-y sel) '(255 240 60))))
      (sdl2:render-present ren))))
