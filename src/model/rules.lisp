;;;; rules.lisp -- the systems that advance the state one turn at a time.
;;;;
;;;; Everything here is a function GAME-STATE -> (mutated) GAME-STATE.  Keeping
;;;; the rules as plain functions (no globals, no I/O) is what makes the model
;;;; testable headless and lets the AI / network / replay layers reuse it.

(in-package #:civ-model)

;;; --- tile & city yields (derived, not stored) ------------------------------

(defun tile-yield (tile)
  "Return (values food shields trade) for TILE incl. improvements,
river (+1 trade) and its special resource."
  (let ((tt (tile-terrain tile)))
    (let ((f (terrain-def tt :food 0))
          (s (terrain-def tt :shields 0))
          (tr (terrain-def tt :trade 0)))
      (when (tile-irrigation tile) (incf f))
      (when (tile-mine tile) (incf s))
      (when (tile-road tile) (incf tr))
      (when (tile-river tile) (incf tr))             ; rivers add trade
      (when (tile-special tile)
        (let ((bonus (cdr (assoc tt *special-bonus*))))
          (when bonus
            (incf f (first bonus)) (incf s (second bonus)) (incf tr (third bonus)))))
      (values f s tr))))

(defun city-auto-work (state city)
  "Assign the city's SIZE citizens to surrounding tiles.  First secure
subsistence (each citizen eats 2 food) by working the highest-food tiles, then
fill the remaining slots preferring trade (so research progresses), then
shields, then food.  The city centre is always worked for free."
  (let* ((map (gs-map state))
         (size (city-size city))
         ;; candidate tiles as (x y food shields trade)
         (cands (loop for (x y tile) in (neighbors map (city-x city) (city-y city))
                      collect (multiple-value-bind (f s tr) (tile-yield tile)
                                (list x y f s tr))))
         (chosen '()))
    (multiple-value-bind (cf cs ctr) (tile-yield (tile-at map (city-x city)
                                                          (city-y city)))
      (declare (ignore cs ctr))
      (let ((food cf) (need (* 2 size)))
        (flet ((take (key)
                 (let ((best (first (sort (copy-list cands) #'> :key key))))
                   (when best
                     (push best chosen)
                     (setf cands (remove best cands))
                     (incf food (third best))))))
          ;; phase 1: secure food
          (loop while (and (< food need) (< (length chosen) size) cands)
                do (take #'third))
          ;; phase 2: maximize trade, then shields, then food
          (loop while (and (< (length chosen) size) cands)
                do (take (lambda (e) (+ (* 3 (fifth e)) (* 2 (fourth e)) (third e))))))
        (setf (city-worked city)
              (mapcar (lambda (e) (list (first e) (second e))) chosen))))))

(defun city-yields (state city)
  "Return (values food shields trade) produced by CITY this turn.
The city centre is worked for free and, per Civ1, always yields at least
1 food / 1 shield / 1 trade so every city can grow, build and research."
  (let ((map (gs-map state)) (f 0) (s 0) (tr 0)
        (b (city-buildings city)))
    (multiple-value-bind (cf cs ct)
        (tile-yield (tile-at map (city-x city) (city-y city)))
      (incf f (max 1 cf)) (incf s (max 1 cs)) (incf tr (max 1 ct)))
    (dolist (w (city-worked city))
      (multiple-value-bind (a b c) (tile-yield (tile-at map (first w) (second w)))
        (incf f a) (incf s b) (incf tr c)))
    ;; wonder yield effects (local to the city that built them)
    (when (member :hanging-gardens b) (incf f 1))           ; +1 food
    (when (member :pyramids b) (setf s (floor (* s 3) 2)))  ; +50% shields
    (when (member :colossus b) (setf tr (floor (* tr 3) 2))); +50% trade
    (values f s tr)))

;;; --- combat ----------------------------------------------------------------

(defparameter +max-hp+ 10 "Hit points each unit fights with.")

(defun attack-strength (unit)
  (let ((a (unit-def (unit-type unit) :attack 0)))
    (if (unit-veteran unit) (round (* a 3/2)) a)))   ; veterans (barracks) +50%

(defun defense-strength (state unit)
  "Defender strength incl. terrain, fortification, city, walls and veteran."
  (let* ((tile (tile-at (gs-map state) (unit-x unit) (unit-y unit)))
         (base (unit-def (unit-type unit) :defense 0))
         (terr (/ (terrain-def (tile-terrain tile) :defense 0) 100))
         (fort (if (eq (unit-orders unit) :fortified) 1/2 0))
         (cityobj (and (tile-city tile) (city-by-id state (tile-city tile))))
         (city (if cityobj 1/2 0))
         (walls (if (and cityobj (or (member :walls (city-buildings cityobj))
                                     (member :great-wall (city-buildings cityobj))))
                    1 0))                                ; city walls +100%
         (vet (if (unit-veteran unit) 1/2 0)))           ; veteran +50%
    (max 1 (round (* base (+ 1 terr fort city walls vet))))))

(defun destroy-unit (state unit)
  (let ((tile (tile-at (gs-map state) (unit-x unit) (unit-y unit))))
    (when tile (setf (tile-units tile) (remove (unit-id unit) (tile-units tile)))))
  (remhash (unit-id unit) (gs-units state)))

(defun enemy-adjacent-p (state x y owner)
  "T if a tile bordering (X,Y) holds a unit not owned by OWNER -- i.e. (X,Y)
lies in an enemy zone of control."
  (loop for cell in (neighbors (gs-map state) x y)
        for tile = (third cell)
        thereis (loop for id in (tile-units tile)
                      for un = (unit-by-id state id)
                      thereis (and un (/= (unit-owner un) owner)))))

(defun city-defended-p (state city)
  "T if a combat unit (attack > 0) is garrisoned on CITY's tile."
  (let ((tile (tile-at (gs-map state) (city-x city) (city-y city))))
    (and tile
         (some (lambda (id)
                 (let ((u (unit-by-id state id)))
                   (and u (plusp (unit-def (unit-type u) :attack 0)))))
               (tile-units tile)))))

(defun tile-enemies (state tile owner)
  "Units on TILE not belonging to OWNER."
  (loop for id in (tile-units tile)
        for u = (unit-by-id state id)
        when (and u (/= (unit-owner u) owner)) collect u))

(defun resolve-combat (state attacker defender)
  "Fight ATTACKER vs DEFENDER to the death using a Civ1-style round loop:
each round, with probability A/(A+D) the defender takes a hit, else the
attacker does.  Both start from their current HP, so wounded units are weaker.
Returns :attacker or :defender; the loser is removed and the winner keeps its
remaining HP (it heals back over later turns)."
  (let ((a (max 1 (attack-strength attacker)))
        (d (defense-strength state defender))
        (ahp (unit-hp attacker)) (dhp (unit-hp defender)))
    (loop while (and (plusp ahp) (plusp dhp))
          do (if (< (gs-rand state (+ a d)) a) (decf dhp) (decf ahp)))
    (cond ((plusp ahp) (setf (unit-hp attacker) ahp)
                       (destroy-unit state defender) :attacker)
          (t           (setf (unit-hp defender) dhp)
                       (destroy-unit state attacker) :defender))))

;;; --- per-turn city processing ---------------------------------------------

(defun wonder-built-p (state key)
  "T if any city has already built wonder KEY (wonders are one per game)."
  (loop for c being the hash-values of (gs-cities state)
        thereis (member key (city-buildings c))))

(defun production-cost (item)
  (ecase (first item)
    (:unit     (unit-def (second item) :cost 9999))
    (:building (building-def (second item) :cost 9999))
    (:wonder   (wonder-def (second item) :cost 9999))))

(defun city-try-complete (state city)
  "Finish the current production if enough shields have accumulated."
  (let ((item (city-production city)))
    (when item
      (let ((cost (production-cost item)))
        (when (>= (city-shield-box city) cost)
          (ecase (first item)
            (:unit (let ((nu (register-unit state :type (second item)
                                            :owner (city-owner city)
                                            :x (city-x city) :y (city-y city))))
                     (when (member :barracks (city-buildings city))
                       (setf (unit-veteran nu) t))))      ; barracks -> veterans
            ((:building :wonder) (pushnew (second item) (city-buildings city))))
          (decf (city-shield-box city) cost)
          ;; buildings and wonders are one-shot; units keep producing
          (when (member (first item) '(:building :wonder))
            (setf (city-production city) nil)))))))

(defun process-city (state city)
  (city-auto-work state city)
  (multiple-value-bind (food shields trade) (city-yields state city)
    ;; growth: each citizen eats 2 food
    (let ((net (- food (* 2 (city-size city))))
          (threshold (* 10 (1+ (city-size city)))))
      (incf (city-food-box city) net)
      (cond ((>= (city-food-box city) threshold)
             (incf (city-size city))
             ;; a granary keeps half the food box after growth
             (setf (city-food-box city)
                   (if (member :granary (city-buildings city)) (floor threshold 2) 0)))
            ((minusp (city-food-box city))            ; starvation
             (when (> (city-size city) 1) (decf (city-size city)))
             (setf (city-food-box city) 0))))
    ;; production
    (incf (city-shield-box city) shields)
    (city-try-complete state city)
    ;; economy: trade splits into the owner's gold and science.  Science is
    ;; accrued in fine (percent-trade) units so a city with only 1 trade still
    ;; makes progress instead of flooring to zero (research-cost is scaled to
    ;; match); gold keeps whole units.
    (let ((p (player-by-id state (city-owner city))))
      (when p
        (let ((sci (* trade (player-science-rate p))))
          (when (member :library (city-buildings city))       ; library +50%
            (setf sci (floor (* sci 3) 2)))
          (when (member :great-library (city-buildings city)) ; great library +50%
            (setf sci (floor (* sci 3) 2)))
          (incf (player-gold p)    (floor (* trade (player-tax-rate p)) 100))
          (incf (player-beakers p) sci))))))

(defun process-cities (state)
  (maphash (lambda (id c) (declare (ignore id)) (process-city state c))
           (gs-cities state)))

;;; --- research --------------------------------------------------------------

(defun researchable-techs (player)
  "Techs PLAYER doesn't have but whose prerequisites are all met."
  (loop for tech being the hash-keys of *techs*
        unless (player-has-tech-p player tech)
          when (every (lambda (pre) (player-has-tech-p player pre))
                      (tech-def tech :prereqs))
            collect tech))

(defun research-cost (player)
  "Beakers needed for the next advance (grows with the number known).  In the
same fine units as accrued science: 1000 = 10 'trade-turns' at 100% science."
  (* 1000 (1+ (hash-table-count (player-techs player)))))

(defun process-research (state)
  (loop for p across (gs-players state) do
    (unless (player-researching p)
      (setf (player-researching p) (first (researchable-techs p))))
    (let ((tech (player-researching p)))
      (when (and tech (>= (player-beakers p) (research-cost p)))
        (setf (gethash tech (player-techs p)) t)
        (decf (player-beakers p) (research-cost p))
        (setf (player-researching p) (first (researchable-techs p)))))))

;;; --- the turn loop ---------------------------------------------------------

(defparameter +open-heal+ 2 "HP a resting unit regains per turn in the open.")
(defparameter +fortify-heal+ 4 "HP a fortified unit regains per turn in the open.")

(defun heal-units (state)
  "Heal units that did not move/fight this turn: fully if garrisoned in a city,
+FORTIFY-HEAL+ if fortified, else +OPEN-HEAL+ (all capped at +MAX-HP+).  Called
before REFRESH-UNITS, so an unspent movement allowance marks a unit as rested."
  (maphash
   (lambda (id u) (declare (ignore id))
     (let ((rested (>= (unit-moves-left u) (unit-def (unit-type u) :move 1)))
           (tile (tile-at (gs-map state) (unit-x u) (unit-y u))))
       (when (and rested (< (unit-hp u) +max-hp+))
         (setf (unit-hp u)
               (cond ((and tile (tile-city tile)) +max-hp+)
                     ((eq (unit-orders u) :fortified)
                      (min +max-hp+ (+ (unit-hp u) +fortify-heal+)))
                     (t (min +max-hp+ (+ (unit-hp u) +open-heal+))))))))
   (gs-units state)))

(defun refresh-units (state)
  "Restore every unit's movement allowance at the start of a turn."
  (maphash (lambda (id u) (declare (ignore id))
             (setf (unit-moves-left u) (unit-def (unit-type u) :move 1)))
           (gs-units state)))

(defun process-terraform (state)
  "Advance each settler's terraform job; when the work runs out, stamp the
improvement onto the tile.  A unit still working holds position (no moves this
turn).  Run after REFRESH-UNITS so a busy unit's restored moves are taken back."
  (maphash
   (lambda (id u) (declare (ignore id))
     (when (unit-work u)
       (decf (unit-work-left u))
       (if (<= (unit-work-left u) 0)
           (let ((tile (tile-at (gs-map state) (unit-x u) (unit-y u))))
             (setf (tile-improvement tile (terraform-def (unit-work u) :flag)) t)
             (setf (unit-work u) nil (unit-work-left u) 0))
           (setf (unit-moves-left u) 0))))      ; still busy
   (gs-units state)))

(defun city-upkeep (city)
  "Total gold upkeep of CITY's improvements (wonders cost nothing to maintain)."
  (reduce #'+ (city-buildings city) :initial-value 0
          :key (lambda (b) (building-def b :upkeep 0))))

(defun sell-a-building (city)
  "Drop CITY's costliest-to-maintain improvement (a bankruptcy fire-sale) and
return its key, or NIL if the city has no sellable improvement."
  (let ((worst (first (sort (remove-if-not (lambda (b) (gethash b *buildings*))
                                           (copy-list (city-buildings city)))
                            #'> :key (lambda (b) (building-def b :upkeep 0))))))
    (when worst
      (setf (city-buildings city) (remove worst (city-buildings city)))
      worst)))

(defun process-economy (state)
  "Charge every player gold upkeep for their improvements.  A player who can't
cover it sells improvements (priciest upkeep first) until solvent, then floors
at zero gold."
  (loop for p across (gs-players state)
        for pid = (player-id p) do
          (let ((cities '()))
            (maphash (lambda (id c) (declare (ignore id))
                       (when (= (city-owner c) pid)
                         (push c cities)
                         (decf (player-gold p) (city-upkeep c))))
                     (gs-cities state))
            ;; bankruptcy: sell one building per city per pass until back in black
            (loop while (and (minusp (player-gold p)) cities) do
              (let ((sold nil))
                (dolist (c cities)
                  (let ((b (sell-a-building c)))
                    (when b
                      (incf (player-gold p) (building-def b :upkeep 0))
                      (setf sold t))))
                (unless sold (return))))
            (when (minusp (player-gold p)) (setf (player-gold p) 0)))))

(defun turn->year (turn)
  "Map a turn number to a (simplified) calendar year."
  (+ -4000 (* (1- turn) 40)))

;; defined in later files (ai.lisp / pathfind.lisp); declared so END-TURN
;; compiles without forward-reference warnings
(declaim (ftype (function (t) t) run-ai-players process-goto))

(defun end-turn (state)
  "Advance the whole world one turn and return STATE.
Phases: AI players act -> process cities -> research -> heal units -> refresh
units -> advance clock.  (A full game would interleave per-player movement and
combat phases here.)"
  (run-ai-players state)
  (process-cities state)
  (process-economy state)       ; charge improvement upkeep (sell on bankruptcy)
  (process-research state)
  (heal-units state)            ; rested/garrisoned units recover HP
  (refresh-units state)
  (process-terraform state)     ; advance settler road/irrigation/mine jobs
  (process-goto state)          ; units on :goto walk toward their target
  (update-visibility state)     ; reveal newly-scouted tiles (fog of war)

  (incf (gs-turn state))
  (setf (gs-year state) (turn->year (gs-turn state)))
  state)
