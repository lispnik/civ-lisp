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
  (relations (make-hash-table :test 'eql)) ; player-pair key -> :war/:alliance (absent = :peace)
  (contacts (make-hash-table :test 'eql))  ; player-pair key -> t once the civs have met
  (truces (make-hash-table :test 'eql))    ; player-pair key -> turn a cease-fire expires
  (stolen (make-hash-table :test 'eql))    ; (thief*256+victim) -> t: a tech has been stolen
  (embassies (make-hash-table :test 'eql)) ; (observer*256+observed) -> t: an embassy exists
  (routes '())                             ; list of (city-a . city-b) trade routes (a<=b)
  (winner nil)                             ; player id of the victor, once decided
  (victory nil)                            ; :conquest or :space
  (message nil)                            ; transient last-event text (e.g. a hut outcome)
  (log '())                                ; chronicle of notable events (newest first)
  (difficulty :prince :type keyword)       ; AI handicap level (see *DIFFICULTIES*)
  (offers '())                             ; AI diplomatic offers awaiting the human's reply
  (history '())                            ; per-turn (turn . ((pid . score)...)) for the replay
  (foundings '())                          ; (turn . pid) each time a city is founded
  (phase :start :type keyword))

(defparameter *difficulties*
  '((:chieftain . 1) (:warlord . 2) (:prince . 3) (:king . 4) (:emperor . 5))
  "The five Civ1 difficulty levels and their numeric strength (1=easiest).")

(defun difficulty-level (state)
  "The numeric strength (1..5) of STATE's difficulty setting."
  (or (cdr (assoc (gs-difficulty state) *difficulties*)) 3))

(defun gs-note (state fmt &rest args)
  "Record a timestamped line in the game's event chronicle (GS-LOG)."
  (push (format nil "T~D: ~A" (gs-turn state) (apply #'format nil fmt args))
        (gs-log state)))

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

(defun next-city-name (state pid)
  "The next city name for player PID: the first of its nation's roster not yet in
use, or a numbered fallback once the roster is exhausted."
  (let* ((p (player-by-id state pid))
         (used (loop for c being the hash-values of (gs-cities state)
                     when (= (city-owner c) pid) collect (city-name c))))
    (or (find-if-not (lambda (n) (member n used :test #'string=)) (player-city-names p))
        (format nil "~A ~D" (or (player-name p) "City") (1+ (length used))))))

;;; --- diplomacy (relations are symmetric; default is peace) -----------------

(defun rel-key (a b) (if (<= a b) (+ (* a 256) b) (+ (* b 256) a)))

(defun barbarian-id-p (state id)
  (let ((p (player-by-id state id)))
    (and p (eq (player-kind p) :barbarian))))

(defun met-p (state a b)
  "T if civilizations A and B have made contact (so diplomacy is possible).
Distinct civs are strangers until one's unit/city sees the other's -- but an
established war/alliance or a truce presupposes prior contact (and keeps old
saves, which predate the contacts table, working)."
  (or (= a b)
      (gethash (rel-key a b) (gs-contacts state))
      (and (not (barbarian-id-p state a)) (not (barbarian-id-p state b))
           (or (not (eq (relation state a b) :peace))
               (truce-active-p state a b)))))

(defun make-contact (state a b)
  "Record that A and B have met (no-op for barbarians or self).  Returns T if
this is the first contact between them."
  (when (and (/= a b)
             (not (barbarian-id-p state a)) (not (barbarian-id-p state b)))
    (let ((key (rel-key a b)))
      (unless (gethash key (gs-contacts state))
        (setf (gethash key (gs-contacts state)) t)))))

(defun relation (state a b)
  "Diplomatic relation between players A and B (:war, :peace, or :alliance)."
  (cond ((= a b) :peace)
        ((or (barbarian-id-p state a) (barbarian-id-p state b)) :war) ; barbarians: always
        (t (gethash (rel-key a b) (gs-relations state) :peace))))

(defun (setf relation) (value state a b)
  (setf (gethash (rel-key a b) (gs-relations state)) value))

(defun at-war-p (state a b)
  (and (/= a b) (eq (relation state a b) :war)))

(defun senate-p (state pid)
  "T if PID's government has a senate (Republic/Democracy) -- it forbids declaring
war and forces the acceptance of cease-fires."
  (let ((p (player-by-id state pid)))
    (and p (government-def (player-government p) :senate))))

(defun allied-p (state a b)
  "T if A and B are bound by an alliance (a peace they have both committed to)."
  (and (/= a b) (eq (relation state a b) :alliance)))

(defun truce-active-p (state a b)
  "T while a cease-fire between A and B still holds (no war may be declared)."
  (< (gs-turn state) (gethash (rel-key a b) (gs-truces state) 0)))

(defparameter *ceasefire-turns* 16
  "How long a cease-fire bars a fresh declaration of war.")

(defun set-truce (state a b)
  "Record a cease-fire between A and B lasting *CEASEFIRE-TURNS* turns."
  (setf (gethash (rel-key a b) (gs-truces state))
        (+ (gs-turn state) *ceasefire-turns*)))

;;; --- diplomatic offers awaiting the human's reply --------------------------

(defun human-id (state)
  "The id of the human player, or NIL in an all-AI game."
  (let ((p (find :human (gs-players state) :key #'player-kind)))
    (and p (player-id p))))

(defun pending-offer-p (state from kind)
  "T if an offer of KIND from civ FROM is already awaiting the human's reply."
  (find-if (lambda (o) (and (= (getf o :from) from) (eq (getf o :kind) kind)))
           (gs-offers state)))

(defun add-offer (state from kind)
  "Queue a diplomatic offer (KIND from civ FROM) for the human to accept/decline."
  (unless (pending-offer-p state from kind)
    (setf (gs-offers state) (append (gs-offers state) (list (list :from from :kind kind))))))

(defun remove-offer (state from kind)
  "Drop the queued offer of KIND from civ FROM."
  (setf (gs-offers state)
        (remove-if (lambda (o) (and (= (getf o :from) from) (eq (getf o :kind) kind)))
                   (gs-offers state))))
(defun unit-by-id (state id) (gethash id (gs-units state)))
(defun city-by-id (state id) (gethash id (gs-cities state)))

;;; --- registration helpers --------------------------------------------------

(defun register-unit (state &key type owner x y)
  "Create a unit, place it on the map, and index it.  Returns the unit."
  (let ((u (make-unit :id (gs-next-id state) :type type :owner owner :x x :y y)))
    (setf (unit-moves-left u) (unit-def type :move 1)
          (unit-fuel u) (unit-def type :range 0)      ; air units launch with a full tank
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

;;; --- map generation: continents + latitude climate ------------------------

(defun climate-terrain (state h y)
  "A land terrain for row Y of an H-tall map: arctic/tundra near the poles,
jungle/swamp near the equator, temperate land between, with local variety."
  (let* ((mid (/ (1- h) 2.0))
         (lat (if (plusp mid) (/ (abs (- y mid)) mid) 0.0))  ; 0 equator .. 1 pole
         (r (gs-rand state 100)))
    (cond
      ;; thin polar caps, then a broad habitable belt, then the equator
      ((> lat 0.88) :arctic)
      ((> lat 0.72) (cond ((< r 60) :tundra) ((< r 78) :arctic)
                          ((< r 90) :hills) (t :forest)))
      ((< lat 0.30) (cond ((< r 26) :jungle) ((< r 40) :swamp) ((< r 57) :grassland)
                          ((< r 72) :plains) ((< r 84) :forest) ((< r 93) :hills)
                          (t :desert)))
      (t            (cond ((< r 26) :grassland) ((< r 44) :plains) ((< r 60) :forest)
                          ((< r 78) :hills) ((< r 92) :mountains) (t :desert))))))

(defun grow-continents (state map)
  "Carve organic landmasses out of an all-ocean MAP by accretion from a handful
of seeds, then paint each land tile a climate-appropriate terrain."
  (let* ((w (map-width map)) (h (map-height map))
         (target (round (* w h 0.38)))                 ; ~38% land
         (nseeds (max 2 (round (isqrt (* w h)) 4)))
         (land (make-hash-table :test 'eql))           ; key = x + y*w
         (frontier (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((key (x y) (+ x (* y w)))
           (add (x y) (let ((k (+ x (* y w))))
                        (unless (gethash k land)
                          (setf (gethash k land) t)
                          (vector-push-extend (cons x y) frontier)))))
      (dotimes (i nseeds)                               ; seeds, away from the poles
        (add (gs-rand state w) (+ 2 (gs-rand state (max 1 (- h 4))))))
      (loop repeat (* target 8)
            while (and (< (hash-table-count land) target) (plusp (length frontier))) do
        (let* ((cell (aref frontier (gs-rand state (length frontier))))
               (d (aref #((1 . 0) (-1 . 0) (0 . 1) (0 . -1)) (gs-rand state 4)))
               (nxx (wrap-x map (+ (car cell) (car d))))
               (nyy (+ (cdr cell) (cdr d))))
          (when (< 0 nyy (1- h))                        ; keep the pole rows ocean
            (add nxx nyy))))
      (loop for k being the hash-keys of land
            do (setf (tile-terrain (svref (map-tiles map) k))
                     (climate-terrain state h (floor k w))))
      ;; carve a few mountain ranges across the land for relief
      (dotimes (i (max 1 (round (* w h) 800)))
        (let ((x (gs-rand state w)) (y (+ 4 (gs-rand state (max 1 (- h 8))))))
          (dotimes (step (+ 4 (gs-rand state 7)))
            (let ((tile (tile-at map x y)))
              (when (and tile (not (eq (tile-terrain tile) :ocean))
                         (not (eq (tile-terrain tile) :arctic)))
                (setf (tile-terrain tile) (if (zerop (gs-rand state 4)) :hills :mountains))))
            (ecase (gs-rand state 4) (0 (incf x)) (1 (decf x)) (2 (incf y)) (3 (decf y)))
            (setf x (wrap-x map x) y (max 0 (min (1- h) y)))))))))

(defun find-land-near (map x0 y0)
  "Nearest non-ocean tile to (X0,Y0), spiralling outward; (values x y)."
  (let ((w (map-width map)) (h (map-height map)))
    (dotimes (r (max w h))
      (loop for dy from (- r) to r do
        (loop for dx from (- r) to r
              when (= r (max (abs dx) (abs dy)))        ; the ring at radius R
                do (let ((x (wrap-x map (+ x0 dx))) (y (+ y0 dy)))
                     (when (and (<= 0 y (1- h))
                                (not (eq (tile-terrain (tile-at map x y)) :ocean)))
                       (return-from find-land-near (values x y)))))))
    (values x0 y0)))

(defun make-new-game (&key (width 20) (height 15) (players '("You" "Rival"))
                           (seed 0) barbarians (difficulty :prince))
  "Build a fresh GAME-STATE: a small map, the given players, each with a
starting settlers + warriors unit.  SEED makes the game reproducible.  With
BARBARIANS, append a unit-less barbarian player that spawns roaming raiders.
DIFFICULTY (a key from *DIFFICULTIES*) sets how hard the AI civilizations play."
  (let* ((nciv (length players))
         (all (if barbarians (append players (list "Barbarians")) players))
         (map (make-game-map width height :terrain :ocean))
         (pvec (make-array (length all)))
         (state (%make-game-state
                 :map map :players pvec :difficulty difficulty
                 :random (sb-ext:seed-random-state seed))))
    ;; carve continents out of the ocean and give them a latitude climate
    (grow-continents state map)
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
    ;; scatter tribal huts on land (~1 in 22 tiles) -- exploration rewards
    (do-tiles (x y tile map)
      (declare (ignore x y))
      (when (and (not (eq (tile-terrain tile) :ocean))
                 (zerop (gs-rand state 22)))
        (setf (tile-hut tile) t)))
    ;; players + their starting units, spread across the map
    (loop for name in all
          for i from 0
          for barb = (and barbarians (= i nciv))   ; the appended barbarian player
          for p = (make-player :id (1+ i) :name name
                               :kind (cond (barb :barbarian) ((zerop i) :human) (t :ai))
                               :color (if barb 8 (1+ i)))
          do (setf (svref pvec i) p)
             ;; give each AI a temperament, cycling so a game has variety
             (when (eq (player-kind p) :ai)
               (setf (player-personality p)
                     (aref #(:aggressive :expansionist :builder :scientific)
                           (mod (1- i) 4))))
             ;; assign a city-name roster: the nation matching the name, else
             ;; a distinct one cycled by player index
             (unless barb
               (setf (player-city-names p)
                     (or (nation-names-for name)
                         (cdr (nth (mod i (length *nation-city-names*))
                                   *nation-city-names*))))
               ;; grant the nation's free starting advance(s)
               (dolist (tech (nation-starting-techs name))
                 (setf (gethash tech (player-techs p)) t)))
             (unless barb           ; barbarians have no capital and no start units
               ;; spread starts across the map, snapped to the nearest land tile
               (multiple-value-bind (px py)
                   (find-land-near map
                                   (max 1 (min (- width 2) (* (1+ i) (floor width (1+ nciv)))))
                                   (floor height 2))
                 ;; a grassland start on a river -- a capital site with baseline trade
                 (setf (tile-terrain (tile-at map px py)) :grassland
                       (tile-river (tile-at map px py)) t
                       (tile-hut (tile-at map px py)) nil)   ; never start on a hut
                 (register-unit state :type :settlers :owner (player-id p) :x px :y py)
                 (register-unit state :type :warriors :owner (player-id p) :x px :y py))))
    (update-visibility state)        ; reveal each player's starting surroundings
    state))
