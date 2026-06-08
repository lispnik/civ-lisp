;;;; view.lisp -- render a civ-model GAME-STATE with SDL2.
;;;;
;;;; The view is read-only: it draws the model and never mutates it.  Terrain is
;;;; drawn as solid colours (the original TER257 sheet uses edge-blend variants
;;;; that need a neighbour-bitmask lookup -- left for later); units and cities
;;;; use sprites from the extracted SP257 sheet (assets/sprites.png), addressed
;;;; as a 16x16 texture atlas.

(in-package #:civ-lisp)

(defparameter *tile* 16 "Native sprite/tile size in pixels.")

(defparameter *sprites-image*
  (merge-pathnames "assets/sprites.png" (asdf:system-source-directory :civ-lisp)))

;;; --- sprite atlas coordinates (col . row) in the SP257 sheet ---------------

(defparameter *unit-sprites*
  '((:settlers . (0 . 10)) (:warriors . (1 . 10)) (:phalanx . (2 . 10))
    (:legion . (3 . 10)) (:catapult . (7 . 10)))
  "civ-model unit type -> (col . row) in the sprite sheet.")

(defparameter +default-unit-sprite+ '(1 . 10))
(defparameter +city-sprite+ '(9 . 9))

(defun unit-sprite (type)
  (or (cdr (assoc type *unit-sprites*)) +default-unit-sprite+))

;;; --- colours ---------------------------------------------------------------

(defparameter *terrain-colors*
  '((:ocean 40 90 165) (:grassland 86 150 70) (:plains 168 158 92)
    (:forest 38 96 52) (:hills 120 108 72) (:mountains 110 110 122)
    (:desert 200 188 120))
  "Terrain -> (r g b) fill colour.")

(defun terrain-color (terrain)
  (or (cdr (assoc terrain *terrain-colors*)) '(60 60 60)))

(defparameter *player-colors*
  '((1 80 150 235) (2 220 70 70) (3 90 200 120) (4 230 200 80))
  "Player color index -> (r g b).")

(defun owner-color (state owner-id)
  (let ((p (and owner-id (civm:player-by-id state owner-id))))
    (if p (or (cdr (assoc (civm:player-color p) *player-colors*)) '(230 230 230))
        '(180 180 180))))

;;; --- low-level drawing (reuses two rects to avoid per-draw allocation) -----

(defstruct (painter (:constructor make-painter (ren tex src dst)))
  ren tex src dst)

(defun make-renderer-painter (ren tex)
  (make-painter ren tex (sdl2:make-rect 0 0 *tile* *tile*)
                (sdl2:make-rect 0 0 *tile* *tile*)))

(defun set-rect (r x y w h)
  (setf (sdl2:rect-x r) x (sdl2:rect-y r) y
        (sdl2:rect-width r) w (sdl2:rect-height r) h))

(defun fill-tile (p tx ty rgb)
  (destructuring-bind (r g b) rgb
    (sdl2:set-render-draw-color (painter-ren p) r g b 255)
    (set-rect (painter-dst p) (* tx *tile*) (* ty *tile*) *tile* *tile*)
    (sdl2:render-fill-rect (painter-ren p) (painter-dst p))))

(defun draw-sprite (p col row tx ty)
  (set-rect (painter-src p) (* col *tile*) (* row *tile*) *tile* *tile*)
  (set-rect (painter-dst p) (* tx *tile*) (* ty *tile*) *tile* *tile*)
  (sdl2:render-copy (painter-ren p) (painter-tex p)
                    :source-rect (painter-src p) :dest-rect (painter-dst p)))

(defun draw-border (p tx ty rgb &optional (inset 0))
  (destructuring-bind (r g b) rgb
    (sdl2:set-render-draw-color (painter-ren p) r g b 255)
    (set-rect (painter-dst p)
              (+ (* tx *tile*) inset) (+ (* ty *tile*) inset)
              (- *tile* (* 2 inset)) (- *tile* (* 2 inset)))
    (sdl2:render-draw-rect (painter-ren p) (painter-dst p))))

;;; --- the frame -------------------------------------------------------------

(defun load-atlas (ren path)
  "Load PATH as a texture (the caller owns/destroys it)."
  (let* ((surf (sdl2-image:load-image (namestring path)))
         (tex (sdl2:create-texture-from-surface ren surf)))
    (sdl2-ffi.functions:sdl-free-surface surf)
    tex))

(defun render-game (painter state selected-id)
  "Draw STATE: terrain, then cities, then units (SELECTED-ID highlighted)."
  (let ((ren (painter-ren painter))
        (map (civm:gs-map state)))
    (sdl2:set-render-draw-color ren 0 0 0 255)
    (sdl2:render-clear ren)
    ;; terrain
    (civm:do-tiles (x y tile map)
      (fill-tile painter x y (terrain-color (civm:tile-terrain tile))))
    ;; cities
    (maphash (lambda (id c) (declare (ignore id))
               (draw-sprite painter (car +city-sprite+) (cdr +city-sprite+)
                            (civm:city-x c) (civm:city-y c))
               (draw-border painter (civm:city-x c) (civm:city-y c)
                            (owner-color state (civm:city-owner c))))
             (civm:gs-cities state))
    ;; units
    (maphash (lambda (id u)
               (let ((spr (unit-sprite (civm:unit-type u))))
                 (draw-sprite painter (car spr) (cdr spr)
                              (civm:unit-x u) (civm:unit-y u))
                 (draw-border painter (civm:unit-x u) (civm:unit-y u)
                              (owner-color state (civm:unit-owner u)))
                 (when (eql id selected-id)
                   (draw-border painter (civm:unit-x u) (civm:unit-y u)
                                '(255 240 60)))))
             (civm:gs-units state))
    (sdl2:render-present ren)))
