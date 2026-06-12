;;;; state.lisp -- the root game state and new-game setup.
;;;;
;;;; GAME-STATE owns everything.  It is a plain struct of plain data, so it can
;;;; be serialized wholesale (save = write it out) and a turn is a transition
;;;; from one GAME-STATE to the next.  The RNG lives in the state so the game is
;;;; deterministic: same seed + same commands => same game.

(in-package #:civ-model)

(defstruct (game-state (:constructor %make-game-state) (:conc-name gs-))
  (turn 1 :type fixnum)
  (year -4000 :type fixnum)
  map
  (players #() :type simple-vector)
  (units (make-hash-table :test 'eql))    ; id -> unit
  (cities (make-hash-table :test 'eql))   ; id -> city
  (id-counter 1 :type fixnum)              ; monotonic id allocator
  (random (make-random-state nil))         ; seeded per game
  (warming 0 :type fixnum)                 ; number of global-warming events so far
  (relations (make-hash-table :test 'eql)) ; player-pair key -> :war (absent = :peace)
  (stolen (make-hash-table :test 'eql))    ; (thief*256+victim) -> t: a tech has been stolen
  (embassies (make-hash-table :test 'eql)) ; (observer*256+observed) -> t: an embassy exists
  (routes '())                             ; list of (city-a . city-b) trade routes (a<=b)
  (winner nil)                             ; player id of the victor, once decided
  (victory nil)                            ; :conquest or :space
  (phase :start :type keyword))

(defun gs-next-id (state)
  "Allocate a fresh monotonic entity id."
  (prog1 (gs-id-counter state) (incf (gs-id-counter state))))

;; defined in fog.lisp (loaded after this file); declared so MAKE-NEW-GAME compiles
(declaim (ftype (function (t) t) update-visibility))

(defun gs-rand (state n)
  "A deterministic random integer in [0,N) drawn from STATE's RNG."
  (random n (gs-random state)))

(defun player-by-id (state id)
  (find id (gs-players state) :key #'player-id))

;;; --- diplomacy (relations are symmetric; default is peace) -----------------

(defun rel-key (a b) (if (<= a b) (+ (* a 256) b) (+ (* b 256) a)))

(defun barbarian-id-p (state id)
  (let ((p (player-by-id state id)))
    (and p (eq (player-kind p) :barbarian))))

(defun relation (state a b)
  "Diplomatic relation between players A and B (:war or :peace)."
  (cond ((= a b) :peace)
        ((or (barbarian-id-p state a) (barbarian-id-p state b)) :war) ; barbarians: always
        (t (gethash (rel-key a b) (gs-relations state) :peace))))

(defun (setf relation) (value state a b)
  (setf (gethash (rel-key a b) (gs-relations state)) value))

(defun at-war-p (state a b)
  (and (/= a b) (eq (relation state a b) :war)))
(defun unit-by-id (state id) (gethash id (gs-units state)))
(defun city-by-id (state id) (gethash id (gs-cities state)))

;;; --- registration helpers --------------------------------------------------

(defun register-unit (state &key type owner x y)
  "Create a unit, place it on the map, and index it.  Returns the unit."
  (let ((u (make-unit :id (gs-next-id state) :type type :owner owner :x x :y y)))
    (setf (unit-moves-left u) (unit-def type :move 1)
          (gethash (unit-id u) (gs-units state)) u)
    (push (unit-id u) (tile-units (tile-at (gs-map state) x y)))
    u))

(defun register-city (state &key name owner x y)
  "Create a city on the map and index it.  Returns the city."
  (let ((c (make-city :id (gs-next-id state) :name name :owner owner :x x :y y))
        (tile (tile-at (gs-map state) x y)))
    (setf (gethash (city-id c) (gs-cities state)) c
          (tile-city tile) (city-id c)
          (tile-owner tile) owner)
    c))

;;; --- new game --------------------------------------------------------------

(defun make-new-game (&key (width 20) (height 15) (players '("You" "Rival"))
                           (seed 0) barbarians)
  "Build a fresh GAME-STATE: a small map, the given players, each with a
starting settlers + warriors unit.  SEED makes the game reproducible.  With
BARBARIANS, append a unit-less barbarian player that spawns roaming raiders."
  (let* ((nciv (length players))
         (all (if barbarians (append players (list "Barbarians")) players))
         (map (make-game-map width height :terrain :grassland))
         (pvec (make-array (length all)))
         (state (%make-game-state
                 :map map :players pvec
                 :random (sb-ext:seed-random-state seed))))
    ;; a little terrain variety so the map isn't uniform
    (dotimes (i (round (* width height 1/4)))
      (let ((tile (tile-at map (gs-rand state width) (gs-rand state height))))
        (when tile
          (setf (tile-terrain tile)
                (nth (gs-rand state 4) '(:plains :forest :hills :ocean))))))
    ;; a couple of meandering rivers (random walks over non-ocean tiles)
    (dotimes (r 2)
      (let ((x (gs-rand state width)) (y (gs-rand state height)))
        (dotimes (step (+ 8 (gs-rand state 8)))
          (let ((tile (tile-at map x y)))
            (when (and tile (not (eq (tile-terrain tile) :ocean)))
              (setf (tile-river tile) t)))
          (ecase (gs-rand state 4)
            (0 (incf x)) (1 (decf x)) (2 (incf y)) (3 (decf y)))
          (setf x (max 0 (min (1- width) x))
                y (max 0 (min (1- height) y))))))
    ;; scatter special resources (~1 in 16 tiles)
    (do-tiles (x y tile map)
      (declare (ignore x y))
      (when (zerop (gs-rand state 16))
        (setf (tile-special tile) t)))
    ;; players + their starting units, spread across the map
    (loop for name in all
          for i from 0
          for barb = (and barbarians (= i nciv))   ; the appended barbarian player
          for px = (max 1 (min (- width 2) (* (1+ i) (floor width (1+ nciv)))))
          for py = (floor height 2)
          for p = (make-player :id (1+ i) :name name
                               :kind (cond (barb :barbarian) ((zerop i) :human) (t :ai))
                               :color (if barb 8 (1+ i)))
          do (setf (svref pvec i) p)
             (unless barb           ; barbarians have no capital and no start units
               ;; a grassland start on a river -- a capital site with baseline trade
               (setf (tile-terrain (tile-at map px py)) :grassland)
               (setf (tile-river (tile-at map px py)) t)
               (register-unit state :type :settlers :owner (player-id p) :x px :y py)
               (register-unit state :type :warriors :owner (player-id p) :x px :y py)))
    (update-visibility state)        ; reveal each player's starting surroundings
    state))
