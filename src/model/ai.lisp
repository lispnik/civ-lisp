;;;; ai.lisp -- a simple AI opponent.
;;;;
;;;; The AI is just another controller: it inspects the state and issues the
;;;; same COMMANDS a human would (found-city / move-unit / set-production)
;;;; through APPLY-COMMAND.  RUN-AI-PLAYERS is invoked from END-TURN, so every
;;;; AI civilization takes its turn whenever the turn advances.

(in-package #:civ-model)

(defparameter *ai-city-names*
  #("Babylon" "Nineveh" "Ashur" "Ur" "Uruk" "Akkad" "Eridu" "Kish"
    "Lagash" "Mari" "Isin" "Sippar")
  "Names the AI gives new cities.")

(defun ai-cmd (state cmd)
  "Issue CMD, swallowing illegal-move errors (the AI may guess wrong)."
  (handler-case (apply-command state cmd)
    (command-error () nil)))

(defun player-unit-list (state pid)
  (loop for u being the hash-values of (gs-units state)
        when (= (unit-owner u) pid) collect u))

(defun player-city-list (state pid)
  (loop for c being the hash-values of (gs-cities state)
        when (= (city-owner c) pid) collect c))

(defun city-near-p (state x y dist)
  "T if any city lies within DIST (chebyshev) of (X,Y)."
  (loop for c being the hash-values of (gs-cities state)
        thereis (and (<= (map-dx (gs-map state) (city-x c) x) dist)
                     (<= (abs (- (city-y c) y)) dist))))

(defun ai-move-random (state unit)
  "Wander one tile in a random cardinal direction."
  (let ((d (aref #((0 . -1) (0 . 1) (-1 . 0) (1 . 0)) (gs-rand state 4))))
    (ai-cmd state (list :move-unit :unit (unit-id unit)
                        :dx (car d) :dy (cdr d)))))

(defun adjacent-enemy (state unit)
  "A unit OWNER is at war with on a tile bordering UNIT, or NIL."
  (loop for (x y tile) in (neighbors (gs-map state) (unit-x unit) (unit-y unit))
        do (loop for id in (tile-units tile)
                 for e = (unit-by-id state id)
                 when (and e (at-war-p state (unit-owner e) (unit-owner unit)))
                   do (return-from adjacent-enemy e))))

(defun ai-diplomacy (state player)
  "Pick fights it can win and bow out of ones it is losing: declare war only on a
peer it is at least as strong as, and sue for peace when down to half a rival's
cities (strength measured by city count)."
  (let ((pid (player-id player))
        (mine (length (player-city-list state (player-id player)))))
    (loop for other across (gs-players state)
          for oid = (player-id other)
          when (and (/= oid pid) (not (eq (player-kind other) :barbarian)))
            do (let ((theirs (length (player-city-list state oid))))
                 (cond
                   ;; at war and clearly losing -> sue for peace (a city-less
                   ;; roaming force has nothing to protect, so it fights on)
                   ((and (at-war-p state pid oid) (plusp mine) (<= (* 2 mine) theirs))
                    (setf (relation state pid oid) :peace))
                   ;; at peace, not weaker, and they have something to take -> pounce
                   ((and (eq (relation state pid oid) :peace)
                         (plusp theirs) (>= mine theirs)
                         (< (gs-rand state 100) 3))
                    (setf (relation state pid oid) :war)))))))

(defun ai-adjacent-enemy-city (state unit)
  "Coordinates (x y) of an at-war enemy city bordering UNIT, or NIL."
  (loop for (x y tile) in (neighbors (gs-map state) (unit-x unit) (unit-y unit))
        for cid = (tile-city tile)
        when (and cid (at-war-p state (city-owner (city-by-id state cid)) (unit-owner unit)))
          return (list x y)))

;; forward references to the invasion helpers defined further down
(declaim (ftype (function (t t t t) t) nearest-enemy-city ai-step-toward)
         (ftype (function (t t) t) only-defender-p))

(defun ai-military (state unit)
  "Attack an adjacent enemy, walk into an adjacent undefended enemy city to take
it, march surplus troops on the nearest enemy city in wartime, else garrison an
own city (keeping one defender) or explore."
  (let* ((enemy (adjacent-enemy state unit))
         (cityxy (ai-adjacent-enemy-city state unit))
         (tile (tile-at (gs-map state) (unit-x unit) (unit-y unit)))
         ;; surplus troops (not a city's lone defender) head for the front
         (target (and (not (only-defender-p state unit))
                      (nearest-enemy-city state (unit-owner unit)
                                          (unit-x unit) (unit-y unit)))))
    (cond
      (enemy
       (ai-cmd state (list :move-unit :unit (unit-id unit)
                           :dx (signum (- (unit-x enemy) (unit-x unit)))
                           :dy (signum (- (unit-y enemy) (unit-y unit))))))
      (cityxy
       (ai-cmd state (list :move-unit :unit (unit-id unit)
                           :dx (signum (signed-dx (gs-map state) (unit-x unit) (first cityxy)))
                           :dy (signum (- (second cityxy) (unit-y unit))))))
      (target (ai-step-toward state unit (city-x target) (city-y target)))
      ((and tile (tile-city tile) (eql (tile-owner tile) (unit-owner unit)))
       (unless (eq (unit-orders unit) :fortified)
         (ai-cmd state (list :fortify :unit (unit-id unit)))))
      (t (ai-move-random state unit)))))

(defun ai-settler (state unit)
  "Found a city on good open ground, otherwise move to find some."
  (let ((tile (tile-at (gs-map state) (unit-x unit) (unit-y unit))))
    (if (and tile
             (not (eq (tile-terrain tile) :ocean))
             (not (tile-city tile))
             (not (city-near-p state (unit-x unit) (unit-y unit) 3)))
        (ai-cmd state (list :found-city :unit (unit-id unit)
                            :name (aref *ai-city-names*
                                        (mod (hash-table-count (gs-cities state))
                                             (length *ai-city-names*)))))
        (ai-move-random state unit))))

;;; --- sea invasion ----------------------------------------------------------

(defun coastal-city-p (state city)
  "T if CITY borders an ocean tile (so it can build and launch ships)."
  (loop for (x y tile) in (neighbors (gs-map state) (city-x city) (city-y city))
        thereis (eq (tile-terrain tile) :ocean)))

(defun nearest-enemy-city (state pid fromx fromy)
  "The city of a civ PID is at war with that is closest to (FROMX,FROMY), or NIL."
  (let ((map (gs-map state)) best bestd)
    (loop for c being the hash-values of (gs-cities state)
          when (and (/= (city-owner c) pid) (at-war-p state pid (city-owner c)))
            do (let ((d (+ (map-dx map (city-x c) fromx) (abs (- (city-y c) fromy)))))
                 (when (or (null bestd) (< d bestd)) (setf best c bestd d))))
    best))

(defun ai-has-invasion-target-p (state pid)
  "T if PID is at war with a non-barbarian civ that still holds a city."
  (loop for o across (gs-players state)
        thereis (and (/= (player-id o) pid)
                     (not (eq (player-kind o) :barbarian))
                     (at-war-p state pid (player-id o))
                     (player-city-list state (player-id o)))))

(defparameter *ai-attackers*
  '(:armor :artillery :cannon :musketeers :knights :catapult :legion :phalanx :warriors)
  "Land units, best first, the AI will build for an invasion / defense force.")

(defparameter *ai-defenders*
  '(:mech-inf :riflemen :musketeers :phalanx :warriors)
  "Land units, best defender first, the AI garrisons its cities with.")

(defun ai-buildable (player list)
  "The first unit in LIST that PLAYER can build and that isn't obsolete."
  (find-if (lambda (u) (and (player-has-tech-p player (unit-def u :requires))
                            (not (unit-obsolete-p player u))))
           list))

(defun ai-best-attacker (player) (ai-buildable player *ai-attackers*))
(defun ai-best-defender (player) (ai-buildable player *ai-defenders*))

(defun ai-step-toward (state unit tx ty)
  "Move UNIT one tile toward (TX,TY) -- diagonal first, then either axis.
Returns non-NIL if it actually moved (terrain permitting)."
  (let* ((map (gs-map state))
         (sx (signum (signed-dx map (unit-x unit) tx)))
         (sy (signum (- ty (unit-y unit))))
         (tries (remove '(0 . 0)
                        (remove-duplicates (list (cons sx sy) (cons sx 0) (cons 0 sy))
                                           :test #'equal)
                        :test #'equal)))
    (some (lambda (d) (ai-cmd state (list :move-unit :unit (unit-id unit)
                                          :dx (car d) :dy (cdr d))))
          tries)))

(defun ai-launch-to-water (state unit)
  "Nose a ship sitting on a (coastal-city) land tile out onto adjacent open water."
  (loop for (x y tile) in (neighbors (gs-map state) (unit-x unit) (unit-y unit))
        when (eq (tile-terrain tile) :ocean)
          do (return (ai-cmd state (list :move-unit :unit (unit-id unit)
                                         :dx (signum (signed-dx (gs-map state) (unit-x unit) x))
                                         :dy (signum (- y (unit-y unit))))))))

(defun ai-cargo (state unit)
  "The land units riding on UNIT's tile (its passengers)."
  (loop for id in (tile-units (tile-at (gs-map state) (unit-x unit) (unit-y unit)))
        for p = (unit-by-id state id)
        when (and p (/= id (unit-id unit)) (eq (unit-def (unit-type p) :domain) :land))
          collect p))

(defun ai-transport (state unit)
  "Drive a land-carrying ship: empty ones nose out to water and wait for troops;
loaded ones sail at the nearest enemy city and put a passenger ashore on arrival."
  (let* ((map (gs-map state))
         (pid (unit-owner unit))
         (cargo (ai-cargo state unit))
         (target (nearest-enemy-city state pid (unit-x unit) (unit-y unit))))
    (cond
      ((or (null cargo) (null target))
       ;; nothing aboard (or no one to invade): get off land into open water, then wait
       (unless (eq (tile-terrain (tile-at map (unit-x unit) (unit-y unit))) :ocean)
         (ai-launch-to-water state unit)))
      (t
       (let ((dist (+ (map-dx map (city-x target) (unit-x unit))
                      (abs (- (city-y target) (unit-y unit)))))
             (lands (loop for (x y tile) in (neighbors map (unit-x unit) (unit-y unit))
                          unless (eq (tile-terrain tile) :ocean)
                            collect (list x y))))
         (if (and (<= dist 4) lands)
             ;; at the enemy shore: send a passenger onto the land tile nearest the city
             (let* ((best (first (sort lands #'<
                                       :key (lambda (s)
                                              (+ (map-dx map (first s) (city-x target))
                                                 (abs (- (second s) (city-y target))))))))
                    (p (first cargo)))
               (ai-cmd state (list :move-unit :unit (unit-id p)
                                   :dx (signum (signed-dx map (unit-x p) (first best)))
                                   :dy (signum (- (second best) (unit-y p))))))
             ;; otherwise sail toward the target
             (ai-step-toward state unit (city-x target) (city-y target))))))))

(defun only-defender-p (state unit)
  "T if UNIT is the lone unit garrisoning one of its owner's cities."
  (let ((tile (tile-at (gs-map state) (unit-x unit) (unit-y unit))))
    (and tile (tile-city tile) (eql (tile-owner tile) (unit-owner unit))
         (= 1 (length (tile-units tile))))))

(defun ai-try-board (state unit)
  "If there's an enemy worth invading and a friendly transport with room sits on
an adjacent sea tile, board it.  Returns non-NIL if UNIT boarded."
  (when (and (nearest-enemy-city state (unit-owner unit) (unit-x unit) (unit-y unit))
             (not (adjacent-enemy state unit))     ; fight what's next to you first
             (not (only-defender-p state unit)))
    (let ((spot (loop for (x y tile) in (neighbors (gs-map state) (unit-x unit) (unit-y unit))
                      when (and (eq (tile-terrain tile) :ocean)
                                (sea-transport-room-p state tile (unit-owner unit)))
                        return (list x y))))
      (when spot
        (ai-cmd state (list :move-unit :unit (unit-id unit)
                            :dx (signum (signed-dx (gs-map state) (unit-x unit) (first spot)))
                            :dy (signum (- (second spot) (unit-y unit)))))))))

(defun ai-city-production (state player city)
  "Keep cities content, expand while small, then build a library or defenders."
  (let* ((pid (player-id player))
         (at-war (ai-has-invasion-target-p state pid))
         (item (cond
                ;; an undefended city must raise a garrison before anything else
                ((not (city-defended-p state city))
                 (list :unit (ai-best-defender player)))
                ;; a growing city needs a temple to stave off disorder
                ((and (>= (city-size city) 4)
                      (player-has-tech-p player :ceremonial-burial)
                      (not (member :temple (city-buildings city))))
                 '(:building :temple))
                ((< (length (player-city-list state pid)) 3)
                 '(:unit :settlers))
                ;; at war: a coastal city builds a transport for the invasion,
                ;; then everyone pumps out attackers (to load and to defend)
                ((and at-war (coastal-city-p state city)
                      (player-has-tech-p player :industrialization)
                      (not (find :transport (player-unit-list state pid) :key #'unit-type)))
                 '(:unit :transport))
                (at-war (list :unit (ai-best-attacker player)))
                ((and (player-has-tech-p player :writing)
                      (not (member :library (city-buildings city))))
                 '(:building :library))
                ;; cheap filler that is never obsolete for this player
                (t (list :unit (or (ai-best-attacker player) :warriors))))))
    (ai-cmd state (list :set-production :city (city-id city) :item item))))

(defun ai-try-trade (state player)
  "Occasionally an AI swaps an advance with a peer it is at peace with, so tech
spreads among the civilizations."
  (when (< (gs-rand state 100) 5)
    (let ((pid (player-id player)))
      (loop for o across (gs-players state)
            for oid = (player-id o)
            when (and (/= oid pid) (eq (player-kind o) :ai)
                      (eq (relation state pid oid) :peace))
              do (let ((give (a-tech-other-lacks state player o))
                       (want (a-tech-other-lacks state o player)))
                   (when (and give want)
                     (ai-cmd state (list :propose-trade :player pid :to oid
                                         :give (list (list :tech give))
                                         :want (list (list :tech want))))
                     (return)))))))

(defun ai-act (state u)
  "One action for unit U: settlers settle; ships ferry invasions; land troops
board a waiting transport when there's an enemy to hit, else fight/explore."
  (cond
    ((eq (unit-type u) :settlers) (ai-settler state u))
    ((eq (unit-def (unit-type u) :carries) :land) (ai-transport state u))
    ((and (eq (unit-def (unit-type u) :domain) :land)
          (plusp (unit-def (unit-type u) :attack 0))
          (ai-try-board state u)))                  ; boarded -> done
    (t (ai-military state u))))

(defun ai-unit-turn (state u)
  "Let U act repeatedly until it is spent, consumed, or makes no progress, so a
unit uses its whole movement allowance in one turn."
  (let ((id (unit-id u)))
    (dotimes (guard 10)
      (unless (and (unit-by-id state id) (plusp (unit-moves-left u))) (return))
      (let ((before (unit-moves-left u)))
        (ai-act state u)
        (when (or (null (unit-by-id state id))      ; consumed (founded a city, died)
                  (= (unit-moves-left u) before))    ; no progress -> stop spinning
          (return))))))

(defun ai-take-turn (state player)
  "Issue this AI PLAYER's commands for the current turn."
  (ai-diplomacy state player)
  (ai-try-trade state player)
  (let ((pid (player-id player)))
    (dolist (u (player-unit-list state pid))
      (when (unit-by-id state (unit-id u))         ; may have been consumed/killed
        (ai-unit-turn state u)))
    (dolist (c (player-city-list state pid))
      (ai-city-production state player c))))

(defparameter *barbarian-spawn-chance* 8
  "Percent chance per turn a new barbarian raider appears.")

(defun barbarian-player (state)
  (find :barbarian (gs-players state) :key #'player-kind))

(defun spawn-barbarian (state barb)
  "Place a barbarian raider on a random empty, non-city land tile."
  (let ((map (gs-map state)) (cands '()))
    (do-tiles (x y tile map)
      (when (and (not (eq (tile-terrain tile) :ocean))
                 (null (tile-units tile)) (not (tile-city tile)))
        (push (list x y) cands)))
    (when cands
      (destructuring-bind (x y) (nth (gs-rand state (length cands)) cands)
        (register-unit state :type (if (zerop (gs-rand state 2)) :legion :warriors)
                       :owner (player-id barb) :x x :y y)))))

(defun barbarians-take-turn (state barb)
  "Barbarians (always at war with everyone) spawn raiders and attack/roam."
  (when (< (gs-rand state 100) *barbarian-spawn-chance*)
    (spawn-barbarian state barb))
  (dolist (u (player-unit-list state (player-id barb)))
    (when (unit-by-id state (unit-id u))
      (ai-unit-turn state u))))           ; raid with full movement

(defun run-ai-players (state)
  "Run AI for every non-human player.  Called from END-TURN."
  (loop for p across (gs-players state)
        when (eq (player-kind p) :ai)
          do (ai-take-turn state p))
  (let ((barb (barbarian-player state)))
    (when barb (barbarians-take-turn state barb))))
