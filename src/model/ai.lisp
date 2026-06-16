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

;;; --- AI personalities ------------------------------------------------------
;;; Each AI is given a temperament at game start (see MAKE-NEW-GAME) that biases
;;; how readily it goes to war, how far it expands, how eagerly it builds wonders
;;; and infrastructure, how willing it is to ally, and what advances it chases.

(defparameter *ai-personalities*
  '((:aggressive   :war-chance 9 :min-cities 2 :wonder-chance 20 :build-chance 30
                   :ally-chance 15 :tech-focus :military)
    (:expansionist :war-chance 4 :min-cities 6 :wonder-chance 30 :build-chance 50
                   :ally-chance 35 :tech-focus :expansion)
    (:builder      :war-chance 1 :min-cities 3 :wonder-chance 70 :build-chance 85
                   :ally-chance 60 :tech-focus :economy)
    (:scientific   :war-chance 2 :min-cities 3 :wonder-chance 45 :build-chance 80
                   :ally-chance 50 :tech-focus :science))
  "AI temperaments -> behavioural traits, read via AI-TRAIT.")

(defparameter *ai-tech-focus*
  '((:military   :bronze-working :iron-working :monarchy :feudalism :gunpowder)
    (:expansion  :pottery :ceremonial-burial :currency :code-of-laws :monarchy)
    (:economy    :bronze-working :currency :trade :banking :the-corporation)
    (:science    :alphabet :writing :literacy :philosophy :university))
  "Advances each tech-focus chases first, ahead of the common goal list.")

(defun ai-trait (player key &optional default)
  "A trait of PLAYER's AI personality, or DEFAULT (also used for the human)."
  (let ((profile (cdr (assoc (player-personality player) *ai-personalities*))))
    (if profile (getf profile key default) default)))

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
                   ;; at peace, not weaker, with something to take, and not
                   ;; shielded by the United Nations -> pounce.  How readily
                   ;; depends on temperament (a warlike civ pounces far more
                   ;; often) and on the difficulty (harder = more aggressive).
                   ((and (eq (relation state pid oid) :peace)
                         (plusp theirs) (>= mine theirs)
                         (not (player-wonder-p state oid :united-nations))
                         (< (gs-rand state 100)
                            (+ (ai-trait player :war-chance 3)
                               (1- (difficulty-level state)))))
                    (setf (relation state pid oid) :war)))))))

(defparameter *ai-gov-order* '(:democracy :republic :monarchy)
  "Governments the AI prefers, best first.")

(defun ai-best-government (state player)
  "The best government PLAYER can adopt -- but only Monarchy while at war, since
Republic/Democracy suffer war-weariness."
  (let ((at-war (ai-has-invasion-target-p state (player-id player))))
    (find-if (lambda (g) (and (player-has-tech-p player (government-def g :requires))
                              (or (not at-war) (eq g :monarchy))))
             *ai-gov-order*)))

(defun ai-government (state player)
  "Occasionally lead a revolution toward a better government."
  (let ((target (ai-best-government state player)))
    (when (and target
               (not (eq (player-government player) target))
               (not (eq (player-government player) :anarchy))
               (zerop (player-anarchy-left player))
               (< (gs-rand state 100) 8))
      (ai-cmd state (list :set-government :player (player-id player) :to target)))))

(defparameter *ai-wonder-order*
  ;; cheapest first, so a modest empire can actually finish one before the game
  ;; ends: 200-shield wonders, then the 300s, then the dearer late ones
  '(:colossus :lighthouse                                            ; 200
    :pyramids :hanging-gardens :great-library :copernicus-observatory ; 300
    :michelangelos-chapel
    :isaac-newtons-college :j-s-bachs-cathedral :magellans-expedition ; 400
    :shakespeares-theatre
    :hoover-dam :womens-suffrage :united-nations :s-e-t-i-program)    ; 600
  "Wonders the AI will try to build, cheapest first.")

(defun ai-largest-city (state pid)
  "PID's most populous city (where the AI concentrates wonder-building), or NIL."
  (let (best)
    (loop for c being the hash-values of (gs-cities state)
          when (= (city-owner c) pid)
            do (when (or (null best) (> (city-size c) (city-size best))) (setf best c)))
    best))

(defun ai-best-wonder (state player)
  "An unbuilt wonder PLAYER has the tech for, or NIL."
  (find-if (lambda (w) (and (player-has-tech-p player (wonder-def w :requires))
                            (not (wonder-built-p state w))))
           *ai-wonder-order*))

(defparameter *ai-tech-goals*
  '(:monarchy :bronze-working :currency :trade :construction :masonry :writing
    :literacy :the-republic :philosophy :mathematics :banking :university
    :democracy :gunpowder :industrialization :the-corporation :electronics
    :computers
    ;; the space-race endgame: Space Flight for the Apollo Program and ship
    ;; structurals, Fusion Power for its fuel
    :rocketry :space-flight :super-conductor :nuclear-power :fusion-power)
  "Advances the AI beelines for, best first -- governments plus the economy and
wonder techs (Currency/Trade/Banking for gold, Construction for aqueducts,
University for science, Industrialization for factories).  The AI researches
whichever prerequisite of the first unmet goal is within reach, so it actually
arrives at Monarchy, the Republic, factories, and the rest.")

(defun ai-tech-goals (player)
  "PLAYER's research priorities: the advances its temperament chases first
(*AI-TECH-FOCUS*), then the common goal list."
  (append (cdr (assoc (ai-trait player :tech-focus) *ai-tech-focus*))
          *ai-tech-goals*))

(defun ai-next-tech (player)
  "The advance the AI should research next: walk its goal list and return the
nearest researchable step toward the first goal it lacks, or NIL once all are in.
Only ever returns a real advance, so a stray goal name can't grant a phantom tech."
  (let ((have (player-techs player)))
    (labels ((toward (tech)                       ; deepest first-unmet prerequisite
               (or (some (lambda (pre) (unless (gethash pre have) (toward pre)))
                         (tech-def tech :prereqs))
                   tech)))
      (loop for goal in (ai-tech-goals player)
            when (and (gethash goal *techs*) (not (gethash goal have)))
              return (toward goal)))))

(defun ai-research (state player)
  "Steer this AI's research toward governments, trade, and wonder advances."
  (declare (ignore state))
  (let ((tech (ai-next-tech player)))
    (when tech (setf (player-researching player) tech))))

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
(defun ai-best-air (player) (ai-buildable player '(:bomber :fighter)))

;;; --- the AI's full toolbox: aircraft, nukes, diplomats, caravans -----------

(defun nearest-own-city (state u)
  "The owner's city closest to unit U, or NIL."
  (let ((map (gs-map state)) best bestd)
    (loop for c being the hash-values of (gs-cities state)
          when (= (city-owner c) (unit-owner u))
            do (let ((d (+ (map-dx map (city-x c) (unit-x u)) (abs (- (city-y c) (unit-y u))))))
                 (when (or (null bestd) (< d bestd)) (setf best c bestd d))))
    best))

(defun ai-air (state u)
  "Aircraft (non-nuke) defend: strike an adjacent enemy, garrison an own city,
or fly back to the nearest city to refuel (so they never run dry in the field)."
  (let ((enemy (adjacent-enemy state u))
        (tile (tile-at (gs-map state) (unit-x u) (unit-y u))))
    (cond
      (enemy (ai-cmd state (list :move-unit :unit (unit-id u)
                                 :dx (signum (- (unit-x enemy) (unit-x u)))
                                 :dy (signum (- (unit-y enemy) (unit-y u))))))
      ((and tile (tile-city tile) (eql (tile-owner tile) (unit-owner u)))
       (unless (eq (unit-orders u) :fortified) (ai-cmd state (list :fortify :unit (unit-id u)))))
      (t (let ((c (nearest-own-city state u)))
           (if c (ai-step-toward state u (city-x c) (city-y c)) (ai-move-random state u)))))))

(defun ai-nuke (state u)
  "Fly a missile at the nearest enemy city and detonate once it is in reach."
  (let ((tgt (nearest-enemy-city state (unit-owner u) (unit-x u) (unit-y u))))
    (cond
      ((null tgt) (ai-move-random state u))
      ((or (ai-adjacent-enemy-city state u) (adjacent-enemy state u))
       (ai-cmd state (list :nuke :unit (unit-id u))))
      (t (ai-step-toward state u (city-x tgt) (city-y tgt))))))

(defun ai-diplomat (state u)
  "March a diplomat to the nearest enemy city and run espionage when adjacent."
  (let ((tgt (nearest-enemy-city state (unit-owner u) (unit-x u) (unit-y u))))
    (cond
      ((adjacent-enemy-city state u)
       (or (ai-cmd state (list :establish-embassy :unit (unit-id u)))
           (ai-cmd state (list :steal-tech :unit (unit-id u)))
           (ai-move-random state u)))
      (tgt (ai-step-toward state u (city-x tgt) (city-y tgt)))
      (t (ai-move-random state u)))))

(defun ai-caravan (state u)
  "Use a caravan: help a wonder or open a trade route from a city, else head to one."
  (cond
    ((ai-cmd state (list :help-wonder :unit (unit-id u))))
    ((ai-cmd state (list :trade-route :unit (unit-id u))))
    (t (let ((c (nearest-own-city state u)))
         (if c (ai-step-toward state u (city-x c) (city-y c)) (ai-move-random state u))))))

(defun ai-special-unit (state player)
  "A wartime extra to build: occasionally a nuke, a defending aircraft, or a spy."
  (let ((pid (player-id player)) (r (gs-rand state 100)))
    (cond
      ((and (< r 15) (player-has-tech-p player :rocketry)
            (wonder-built-p state :manhattan-project)
            (not (find :nuclear (player-unit-list state pid) :key #'unit-type)))
       :nuclear)
      ((and (< r 40) (player-has-tech-p player :flight)
            (< (count-if (lambda (u) (eq (unit-def (unit-type u) :domain) :air))
                         (player-unit-list state pid))
               2))
       (or (ai-best-air player) :fighter))
      ((and (< r 65) (player-has-tech-p player :writing)
            (notany (lambda (u) (eq (unit-type u) :diplomat)) (player-unit-list state pid)))
       :diplomat))))

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

(defparameter *ai-building-order*
  '(:marketplace :library :aqueduct :university :bank :factory
    :power-plant :sewer-system :stock-exchange :colosseum :courthouse)
  "City improvements the AI raises in peacetime, most valuable first.")

(defun ai-next-building (player city)
  "The next worthwhile improvement CITY can build, or NIL.  Growth and happiness
buildings are gated on size; a power plant waits for the factory it powers."
  (find-if (lambda (bld)
             (and (player-has-tech-p player (building-def bld :requires))
                  (not (member bld (city-buildings city)))
                  (case bld
                    (:aqueduct     (>= (city-size city) 6))
                    (:sewer-system (>= (city-size city) 11))
                    (:colosseum    (>= (city-size city) 5))
                    (:power-plant  (member :factory (city-buildings city)))
                    (t t))))
           *ai-building-order*))

(defun ai-city-production (state player city)
  "Keep cities content, expand while small, then build up the economy or an army."
  (let* ((pid (player-id player))
         (at-war (ai-has-invasion-target-p state pid))
         (special (and at-war (ai-special-unit state player)))
         ;; the AI builds wonders in its largest, well-established city
         (wonder (and (eq city (ai-largest-city state pid))
                      (ai-best-wonder state player)))
         (building (ai-next-building player city))
         (item (cond
                ;; once committed to a wonder, see it through (don't fritter the
                ;; shield box away on cheap units before it can ever complete)
                ((and (eq (first (city-production city)) :wonder)
                      (not (wonder-built-p state (second (city-production city))))
                      (city-defended-p state city))
                 (city-production city))
                ;; likewise finish an improvement already under way, rather than
                ;; re-rolling to a cheap unit that would eat the shields first
                ((and (eq (first (city-production city)) :building)
                      (not (member (second (city-production city)) (city-buildings city)))
                      (city-defended-p state city))
                 (city-production city))
                ;; an undefended city must raise a garrison before anything else
                ((not (city-defended-p state city))
                 (list :unit (ai-best-defender player)))
                ;; a growing city needs a temple to stave off disorder
                ((and (>= (city-size city) 4)
                      (player-has-tech-p player :ceremonial-burial)
                      (not (member :temple (city-buildings city))))
                 '(:building :temple))
                ;; expand to the empire size this temperament likes
                ((< (length (player-city-list state pid))
                    (ai-trait player :min-cities 3))
                 '(:unit :settlers))
                ;; the capital invests in a world wonder -- how eagerly depends on
                ;; temperament (a builder reaches for them far more than a warlord)
                ((and wonder (>= (city-size city) 4)
                      (< (gs-rand state 100) (ai-trait player :wonder-chance 60)))
                 (list :wonder wonder))
                ;; develop the economy even in wartime: a defended city often
                ;; raises an improvement rather than another unit (less so at war),
                ;; at a rate set by temperament
                ((and building
                      (< (gs-rand state 100)
                         (let ((b (ai-trait player :build-chance 80)))
                           (if at-war (floor b 2) b))))
                 (list :building building))
                ;; at war: a coastal city builds a transport for the invasion,
                ;; then everyone pumps out attackers (to load and to defend)
                ((and at-war (coastal-city-p state city)
                      (player-has-tech-p player :industrialization)
                      (not (find :transport (player-unit-list state pid) :key #'unit-type)))
                 '(:unit :transport))
                ;; a wartime toolbox extra (a nuke, a defending plane, a spy)
                (special (list :unit special))
                (at-war (list :unit (ai-best-attacker player)))
                ;; peacetime economy: a caravan to open trade routes between cities
                ((and (player-has-tech-p player :trade)
                      (>= (length (player-city-list state pid)) 2)
                      (notany (lambda (u) (eq (unit-type u) :caravan))
                              (player-unit-list state pid))
                      (< (gs-rand state 100) 15))
                 '(:unit :caravan))
                ;; any remaining improvement worth building
                (building (list :building building))
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
    ((member :nuke (unit-def (unit-type u) :abilities)) (ai-nuke state u))
    ((member :espionage (unit-def (unit-type u) :abilities)) (ai-diplomat state u))
    ((member :caravan (unit-def (unit-type u) :abilities)) (ai-caravan state u))
    ((eq (unit-def (unit-type u) :carries) :land) (ai-transport state u))
    ((eq (unit-def (unit-type u) :domain) :air) (ai-air state u))   ; non-nuke aircraft
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
  (ai-research state player)
  (ai-government state player)
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
