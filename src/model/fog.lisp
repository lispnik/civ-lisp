;;;; fog.lisp -- fog of war (per-player tile visibility).
;;;;
;;;; Each player remembers which tiles it has ever SEEN (player-seen).  A tile
;;;; is currently VISIBLE if one of the player's units or cities is within sight
;;;; range (1 tile).  UPDATE-VISIBILITY (called at game start and each turn)
;;;; accumulates the explored set; VISIBLE-SET recomputes the transient
;;;; currently-visible set for rendering.

(in-package #:civ-model)

(defparameter *sight* 1 "Sight range in tiles (chebyshev) of units and cities.")

(defun reveal-around (table state cx cy)
  "Mark every tile within *SIGHT* of (CX,CY) in TABLE (key = x + y*width)."
  (let* ((map (gs-map state)) (w (map-width map)))
    (loop for dy from (- *sight*) to *sight* do
      (loop for dx from (- *sight*) to *sight* do
        (let ((x (+ cx dx)) (y (+ cy dy)))
          (when (in-bounds-p map x y)
            (setf (gethash (+ x (* y w)) table) t)))))))

(defun %collect-sight (state player table)
  "Reveal around all of PLAYER's units and cities into TABLE."
  (let ((pid (player-id player)))
    (maphash (lambda (id u) (declare (ignore id))
               (when (= (unit-owner u) pid) (reveal-around table state (unit-x u) (unit-y u))))
             (gs-units state))
    (maphash (lambda (id c) (declare (ignore id))
               (when (= (city-owner c) pid) (reveal-around table state (city-x c) (city-y c))))
             (gs-cities state))
    table))

(defun update-visibility (state)
  "Accumulate each player's explored (seen) set from current unit/city sight."
  (loop for p across (gs-players state)
        do (%collect-sight state p (player-seen p))))

(defun visible-set (state player)
  "A fresh hash-table of the tiles PLAYER can currently see."
  (%collect-sight state player (make-hash-table :test 'eql)))

(defun seen-p (state player x y)
  "Has PLAYER ever explored tile (X,Y)?"
  (gethash (+ x (* y (map-width (gs-map state)))) (player-seen player)))
