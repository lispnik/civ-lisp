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
        thereis (and (<= (abs (- (city-x c) x)) dist)
                     (<= (abs (- (city-y c) y)) dist))))

(defun ai-move-random (state unit)
  "Wander one tile in a random cardinal direction."
  (let ((d (aref #((0 . -1) (0 . 1) (-1 . 0) (1 . 0)) (gs-rand state 4))))
    (ai-cmd state (list :move-unit :unit (unit-id unit)
                        :dx (car d) :dy (cdr d)))))

(defun adjacent-enemy (state unit)
  "An enemy unit on a tile bordering UNIT, or NIL."
  (loop for (x y tile) in (neighbors (gs-map state) (unit-x unit) (unit-y unit))
        do (loop for id in (tile-units tile)
                 for e = (unit-by-id state id)
                 when (and e (/= (unit-owner e) (unit-owner unit)))
                   do (return-from adjacent-enemy e))))

(defun ai-military (state unit)
  "Attack an adjacent enemy; else garrison (fortify) in an own city; else explore."
  (let ((enemy (adjacent-enemy state unit))
        (tile (tile-at (gs-map state) (unit-x unit) (unit-y unit))))
    (cond
      (enemy
       (ai-cmd state (list :move-unit :unit (unit-id unit)
                           :dx (signum (- (unit-x enemy) (unit-x unit)))
                           :dy (signum (- (unit-y enemy) (unit-y unit))))))
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

(defun ai-city-production (state player city)
  "Expand while small, then build a library (if able) or defenders."
  (let ((item (cond
                ((< (length (player-city-list state (player-id player))) 3)
                 '(:unit :settlers))
                ((and (player-has-tech-p player :writing)
                      (not (member :library (city-buildings city))))
                 '(:building :library))
                (t '(:unit :warriors)))))
    (ai-cmd state (list :set-production :city (city-id city) :item item))))

(defun ai-take-turn (state player)
  "Issue this AI PLAYER's commands for the current turn."
  (let ((pid (player-id player)))
    ;; units: settlers settle/seek; everyone else explores
    (dolist (u (player-unit-list state pid))
      (when (unit-by-id state (unit-id u))         ; may have been consumed/killed
        (if (eq (unit-type u) :settlers)
            (ai-settler state u)
            (ai-military state u))))
    ;; cities: pick production
    (dolist (c (player-city-list state pid))
      (ai-city-production state player c))))

(defun run-ai-players (state)
  "Run AI for every non-human player.  Called from END-TURN."
  (loop for p across (gs-players state)
        when (eq (player-kind p) :ai)
          do (ai-take-turn state p)))
