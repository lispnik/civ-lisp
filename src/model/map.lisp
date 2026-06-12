;;;; map.lisp -- the world grid.

(in-package #:civ-model)

(defstruct (tile (:constructor make-tile (&key (terrain :grassland))))
  (terrain :grassland :type keyword)
  (feature nil)                ; reserved for future overlays
  (resource nil)              ; reserved
  (river nil)                 ; a river runs through this tile
  (special nil)               ; tile has its terrain's special resource
  ;; improvements
  (road nil)
  (railroad nil)              ; railroad (upgrades a road)
  (irrigation nil)
  (mine nil)
  (fort nil)                  ; a field fort (defensive structure)
  (pollution nil)             ; a pollution blight sits on this tile
  (hut nil)                   ; an unexplored tribal hut (goody hut) sits here
  ;; occupancy
  (owner nil)                 ; player id or NIL
  (city nil)                  ; city id or NIL (a city sits on this tile)
  (units '()))                ; list of unit ids currently on the tile

(defstruct (game-map (:constructor %make-game-map) (:conc-name map-))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (tiles nil :type (or null simple-vector)))   ; row-major width*height

(defun make-game-map (width height &key (terrain :grassland))
  "An all-TERRAIN map of WIDTH x HEIGHT fresh tiles."
  (let ((tiles (make-array (* width height))))
    (dotimes (i (* width height))
      (setf (svref tiles i) (make-tile :terrain terrain)))
    (%make-game-map :width width :height height :tiles tiles)))

(declaim (inline in-bounds-p wrap-x))
(defun in-bounds-p (map x y)
  (and (>= x 0) (< x (map-width map))
       (>= y 0) (< y (map-height map))))

;;; The world is a horizontal cylinder: x wraps east-west (the poles, top and
;;; bottom rows, do not wrap).  WRAP-X normalizes a column; MAP-DX is the
;;; shortest east-west distance and SIGNED-DX the corresponding +/-1 step.
(defun wrap-x (map x) (mod x (map-width map)))

(defun map-dx (map x1 x2)
  "Shortest east-west distance between columns X1 and X2 around the cylinder."
  (let* ((w (map-width map)) (d (mod (abs (- x1 x2)) w)))
    (min d (- w d))))

(defun signed-dx (map fromx tox)
  "Signed horizontal step from FROMX toward TOX, taking the short way around."
  (let* ((w (map-width map)) (raw (- tox fromx)) (half (floor w 2)))
    (cond ((> raw half) (- raw w))
          ((< raw (- half)) (+ raw w))
          (t raw))))

(defun tile-at (map x y)
  "The tile at (X,Y), or NIL if Y is out of bounds.  X wraps around the cylinder."
  (when (and (>= y 0) (< y (map-height map)))
    (svref (map-tiles map) (+ (wrap-x map x) (* y (map-width map))))))

(defparameter +neighbor-offsets+
  '((-1 . -1) (0 . -1) (1 . -1)
    (-1 .  0)          (1 .  0)
    (-1 .  1) (0 .  1) (1 .  1))
  "The 8 surrounding directions (square grid, king moves).")

(defun neighbors (map x y)
  "List of (x y tile) for the 8-neighbours of (X,Y); X coordinates are wrapped
around the cylinder, and tiles past the poles are omitted."
  (loop for (dx . dy) in +neighbor-offsets+
        for ny = (+ y dy)
        for tile = (tile-at map (+ x dx) ny)
        when tile collect (list (wrap-x map (+ x dx)) ny tile)))

(defun tile-improvement (tile flag)
  "Read a terraform improvement FLAG (:road/:railroad/:irrigation/:mine) on TILE."
  (ecase flag
    (:road (tile-road tile))
    (:railroad (tile-railroad tile))
    (:irrigation (tile-irrigation tile))
    (:mine (tile-mine tile))
    (:fort (tile-fort tile))))

(defun (setf tile-improvement) (value tile flag)
  (ecase flag
    (:road (setf (tile-road tile) value))
    (:railroad (setf (tile-railroad tile) value))
    (:irrigation (setf (tile-irrigation tile) value))
    (:mine (setf (tile-mine tile) value))
    (:fort (setf (tile-fort tile) value))))

(defmacro do-tiles ((x y tile map) &body body)
  "Iterate X Y TILE over every tile of MAP."
  (let ((m (gensym)))
    `(let ((,m ,map))
       (dotimes (,y (map-height ,m))
         (dotimes (,x (map-width ,m))
           (let ((,tile (tile-at ,m ,x ,y)))
             ,@body))))))
