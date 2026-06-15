;;;; rules.lisp -- the systems that advance the state one turn at a time.
;;;;
;;;; Everything here is a function GAME-STATE -> (mutated) GAME-STATE.  Keeping
;;;; the rules as plain functions (no globals, no I/O) is what makes the model
;;;; testable headless and lets the AI / network / replay layers reuse it.

(in-package #:civ-model)

;;; --- tile & city yields (derived, not stored) ------------------------------

(defun tile-yield (tile &optional gov)
  "Return (values food shields trade) for TILE incl. improvements,
river (+1 trade) and its special resource.  When GOV is supplied, apply that
government's tile effects: the despotic -1 penalty on any category yielding 3+,
or the republic/democracy +1 trade on any tile already producing trade."
  (let ((tt (tile-terrain tile)))
    (let ((f (terrain-def tt :food 0))
          (s (terrain-def tt :shields 0))
          (tr (terrain-def tt :trade 0)))
      (when (tile-irrigation tile) (incf f))
      (when (tile-mine tile) (incf s))
      (when (tile-road tile) (incf tr))
      (when (tile-river tile) (incf tr))             ; rivers add trade
      (when (and (tile-railroad tile) (plusp s)) (incf s)) ; railroad: +1 shield
      (when (tile-special tile)
        (let ((bonus (cdr (assoc tt *special-bonus*))))
          (when bonus
            (incf f (first bonus)) (incf s (second bonus)) (incf tr (third bonus)))))
      ;; pollution halves a tile's output until it is cleaned
      (when (tile-pollution tile)
        (setf f (floor f 2) s (floor s 2) tr (floor tr 2)))
      (when gov
        (when (government-def gov :tile-penalty)     ; despotism / anarchy
          (when (>= f 3) (decf f))
          (when (>= s 3) (decf s))
          (when (>= tr 3) (decf tr)))
        (when (and (government-def gov :trade-bonus)  ; republic / democracy
                   (plusp tr))
          (incf tr)))
      (values f s tr))))

(defun city-gov (state city)
  "The government of CITY's owner (NIL if the city is unowned)."
  (let ((p (player-by-id state (city-owner city))))
    (and p (player-government p))))

;;; --- trade routes (caravans) -----------------------------------------------

(defun route-pair (a b) (if (<= a b) (cons a b) (cons b a)))
(defun route-exists-p (state a b) (member (route-pair a b) (gs-routes state) :test #'equal))
(defun add-route (state a b) (pushnew (route-pair a b) (gs-routes state) :test #'equal))

(defun city-route-count (state cid)
  "Active trade routes CID is in -- the other endpoint city must still exist."
  (count-if (lambda (pr) (and (or (= (car pr) cid) (= (cdr pr) cid))
                              (city-by-id state (car pr)) (city-by-id state (cdr pr))))
            (gs-routes state)))

(defun city-auto-work (state city)
  "Assign the city's SIZE citizens to surrounding tiles.  First secure
subsistence (each citizen eats 2 food) by working the highest-food tiles, then
fill the remaining slots preferring trade (so research progresses), then
shields, then food.  The city centre is always worked for free."
  (let* ((map (gs-map state))
         (size (city-size city))
         (gov (city-gov state city))
         ;; candidate tiles as (x y food shields trade)
         (cands (loop for (x y tile) in (neighbors map (city-x city) (city-y city))
                      collect (multiple-value-bind (f s tr) (tile-yield tile gov)
                                (list x y f s tr))))
         (chosen '()))
    (multiple-value-bind (cf cs ctr) (tile-yield (tile-at map (city-x city)
                                                          (city-y city))
                                                 gov)
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
        (b (city-buildings city))
        (gov (city-gov state city)))
    (multiple-value-bind (cf cs ct)
        (tile-yield (tile-at map (city-x city) (city-y city)) gov)
      (incf f (max 1 cf)) (incf s (max 1 cs)) (incf tr (max 1 ct)))
    (dolist (w (city-worked city))
      (multiple-value-bind (a b c) (tile-yield (tile-at map (first w) (second w)) gov)
        (incf f a) (incf s b) (incf tr c)))
    ;; wonder yield effects (local to the city that built them)
    (when (member :hanging-gardens b) (incf f 1))           ; +1 food
    (when (member :pyramids b) (setf s (floor (* s 3) 2)))  ; +50% shields
    (when (member :colossus b) (setf tr (floor (* tr 3) 2))); +50% trade
    ;; trade routes: +1 trade each, up to three
    (incf tr (min 3 (city-route-count state (city-id city))))
    ;; corruption: a government-dependent slice of trade is simply lost
    (when gov
      (let ((corrupt (government-def gov :corruption 0)))
        (when (plusp corrupt) (decf tr (floor (* tr corrupt) 100)))))
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
         (dug-in (if (eq (unit-orders unit) :fortified) 1/2 0))
         (fortress (if (tile-fort tile) 1 0))            ; a field fort +100%
         (cityobj (and (tile-city tile) (city-by-id state (tile-city tile))))
         (city (if cityobj 1/2 0))
         (walls (if (and cityobj (or (member :walls (city-buildings cityobj))
                                     (member :great-wall (city-buildings cityobj))))
                    1 0))                                ; city walls +100%
         (vet (if (unit-veteran unit) 1/2 0)))           ; veteran +50%
    (max 1 (round (* base (+ 1 terr dug-in fortress city walls vet))))))

(declaim (ftype (function (t t) t) destroy-unit))  ; mutually recursive below
(defun drown-stranded-cargo (state tile)
  "Destroy land units on an ocean TILE that the remaining ships can no longer
carry (e.g. after a transport is sunk)."
  (let ((cap (loop for id in (tile-units tile)
                   for p = (unit-by-id state id)
                   when (and p (eq (unit-def (unit-type p) :carries) :land))
                     sum (unit-def (unit-type p) :capacity 0)))
        (cargo (loop for id in (tile-units tile)
                     for p = (unit-by-id state id)
                     when (and p (eq (unit-def (unit-type p) :domain) :land))
                       collect p)))
    (loop while (> (length cargo) cap)
          do (destroy-unit state (pop cargo)))))

(defun destroy-unit (state unit)
  (let ((tile (tile-at (gs-map state) (unit-x unit) (unit-y unit))))
    (when tile (setf (tile-units tile) (remove (unit-id unit) (tile-units tile))))
    (remhash (unit-id unit) (gs-units state))
    ;; sinking a transport at sea drowns any cargo it can no longer hold
    (when (and tile (eq (tile-terrain tile) :ocean)
               (eq (unit-def (unit-type unit) :carries) :land))
      (drown-stranded-cargo state tile))))

(defun enemy-adjacent-p (state x y owner)
  "T if a tile bordering (X,Y) holds a unit of a civilization OWNER is at war
with -- i.e. (X,Y) lies in an enemy zone of control."
  (loop for cell in (neighbors (gs-map state) x y)
        for tile = (third cell)
        thereis (loop for id in (tile-units tile)
                      for un = (unit-by-id state id)
                      thereis (and un (at-war-p state (unit-owner un) owner)))))

(defun city-defended-p (state city)
  "T if a combat unit (attack > 0) is garrisoned on CITY's tile."
  (let ((tile (tile-at (gs-map state) (city-x city) (city-y city))))
    (and tile
         (some (lambda (id)
                 (let ((u (unit-by-id state id)))
                   (and u (plusp (unit-def (unit-type u) :attack 0)))))
               (tile-units tile)))))

(defun remove-city-routes (state cid)
  "Drop any trade routes that touch city CID (it changed hands or was razed)."
  (setf (gs-routes state)
        (remove-if (lambda (pair) (or (eql (car pair) cid) (eql (cdr pair) cid)))
                   (gs-routes state))))

(defun raze-city (state city)
  "Destroy CITY entirely (a too-small conquest), clearing its tile and routes."
  (let ((tile (tile-at (gs-map state) (city-x city) (city-y city))))
    (setf (tile-city tile) nil (tile-owner tile) nil))
  (remove-city-routes state (city-id city))
  (remhash (city-id city) (gs-cities state)))

(defun capture-city (state city captor)
  "CAPTOR (a player id) takes CITY: a size-1 city is razed; a larger one changes
hands -- shrunk by one, production and stored food/shields reset, trade routes
broken (its buildings and any wonders carry over).  Records GS-MESSAGE."
  (let ((loser (city-owner city))
        (tile (tile-at (gs-map state) (city-x city) (city-y city))))
    (cond
      ((<= (city-size city) 1)
       (raze-city state city)
       (setf (gs-message state) (format nil "~A razed." (city-name city))))
      (t
       (decf (city-size city))
       (setf (city-owner city) captor
             (tile-owner tile) captor
             (city-production city) nil
             (city-food-box city) 0
             (city-shield-box city) 0
             (city-worked city) '())
       (remove-city-routes state (city-id city))
       (setf (gs-message state)
             (format nil "~A captured from ~(~A~)!" (city-name city)
                     (player-name (player-by-id state loser))))))))

(defun tile-enemies (state tile owner)
  "Units on TILE not belonging to OWNER (any other civilization)."
  (loop for id in (tile-units tile)
        for u = (unit-by-id state id)
        when (and u (/= (unit-owner u) owner)) collect u))

(defun tile-hostiles (state tile owner)
  "Units on TILE belonging to a civilization OWNER is at war with."
  (loop for id in (tile-units tile)
        for u = (unit-by-id state id)
        when (and u (at-war-p state (unit-owner u) owner)) collect u))

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

(defparameter *base-content*
  4 "Citizens that are content for free in a city; the rest start unhappy.")

(defun count-city-military (state city)
  "Number of military (attack>0) units standing in CITY."
  (let ((tile (tile-at (gs-map state) (city-x city) (city-y city))))
    (count-if (lambda (id) (let ((u (unit-by-id state id)))
                             (and u (plusp (unit-def (unit-type u) :attack 0)))))
              (tile-units tile))))

(defun unit-in-friendly-city-p (state u)
  "T if unit U stands in a city its own owner holds."
  (let ((tile (tile-at (gs-map state) (unit-x u) (unit-y u))))
    (and (tile-city tile) (eql (tile-owner tile) (unit-owner u)))))

(defun nearest-city-id (state pid x y)
  "Id of PID's city nearest (chebyshev) to (X,Y), or NIL if PID has no city."
  (let (best bestd)
    (maphash (lambda (id c) (declare (ignore id))
               (when (= (city-owner c) pid)
                 (let ((d (max (map-dx (gs-map state) (city-x c) x)
                               (abs (- (city-y c) y)))))
                   (when (or (null bestd) (< d bestd))
                     (setf best (city-id c) bestd d)))))
             (gs-cities state))
    best))

(defun city-military-unhappiness (state city)
  "Unhappy citizens CITY suffers from its owner's units in the field (war
weariness).  Only republics and democracies feel it: each owned military unit
that is NOT in a friendly city is 'homed' to the owner's nearest city, adding 1
unhappy there under a republic, 2 under a democracy."
  (let* ((pid (city-owner city))
         (owner (player-by-id state pid))
         (gov (and owner (player-government owner))))
    (if (and gov (government-def gov :trade-bonus))      ; republic / democracy only
        (let ((per (if (eq gov :democracy) 2 1)) (n 0))
          (maphash (lambda (id u) (declare (ignore id))
                     (when (and (= (unit-owner u) pid)
                                (plusp (unit-def (unit-type u) :attack 0))
                                (not (unit-in-friendly-city-p state u))
                                (eql (nearest-city-id state pid (unit-x u) (unit-y u))
                                     (city-id city)))
                       (incf n)))
                   (gs-units state))
          (* per n))
        0)))

(defun city-happiness (state city trade)
  "Classify CITY's citizens given its TRADE this turn.
Returns (values happy content unhappy).  Citizens past *BASE-CONTENT* start
unhappy (plus war weariness from units in the field under republic/democracy);
temples/colosseums/cathedrals and (in martial-law governments) military units
quiet them; luxuries (2 arrows each) push unhappy->content->happy; a few wonders
help globally."
  (let* ((size (city-size city))
         (owner (player-by-id state (city-owner city)))
         (gov (and owner (player-government owner)))
         (b (city-buildings city))
         (content (min size *base-content*))
         (unhappy (max 0 (- size *base-content*)))
         (happy 0))
    ;; war weariness: units in the field flip content citizens to unhappy
    (let ((flip (min content (city-military-unhappiness state city))))
      (decf content flip) (incf unhappy flip))
    (flet ((calm (n)                       ; up to N unhappy -> content
             (let ((k (min (max 0 n) unhappy))) (decf unhappy k) (incf content k)))
           (cheer (n)                       ; up to N: unhappy->content, then content->happy
             (dotimes (i (max 0 n))
               (cond ((plusp unhappy) (decf unhappy) (incf content))
                     ((plusp content) (decf content) (incf happy))))))
      ;; happiness buildings
      (when (member :temple b)
        (calm (if (wonder-built-p state :oracle) 4 2)))
      (when (member :colosseum b) (calm 3))
      (when (or (member :cathedral b)
                (wonder-built-p state :michelangelos-chapel))   ; chapel = cathedral everywhere
        (calm 3))
      ;; martial law: each military unit quiets one unhappy, up to the gov's cap
      (let ((ml (and gov (government-def gov :martial-law 0))))
        (when (and ml (plusp ml))
          (calm (min ml (count-city-military state city)))))
      ;; luxuries: every 2 luxury arrows make one citizen happier
      (when owner
        (cheer (floor (floor (* trade (player-luxury-rate owner)) 100) 2)))
      ;; global-happiness wonders
      (when (wonder-built-p state :hanging-gardens) (cheer 1))
      (when (wonder-built-p state :cure-for-cancer) (cheer 1))
      (when (wonder-built-p state :j-s-bachs-cathedral) (calm 2))
      (when (member :shakespeares-theatre b) (calm unhappy))     ; no unhappy here
      (values happy content unhappy))))

(defun city-disorder-p (state city)
  "T if CITY has more unhappy than happy citizens (civil disorder)."
  (multiple-value-bind (h c u) (city-happiness state city (nth-value 2 (city-yields state city)))
    (declare (ignore c))
    (> u h)))

(defun city-celebrating-p (state city)
  "T if CITY is celebrating (\"We Love the King\"): at least half its citizens
happy, none unhappy, size >= 3."
  (multiple-value-bind (h c u) (city-happiness state city (nth-value 2 (city-yields state city)))
    (declare (ignore c))
    (and (>= (city-size city) 3) (zerop u) (>= h (ceiling (city-size city) 2)))))

(defparameter *spaceship-part-cost* 160 "Shields per spaceship part.")

(defun unit-obsolete-p (player type)
  "T if PLAYER has researched the advance that retires unit TYPE (a newer unit
has superseded it), so it can no longer be built."
  (let ((tech (unit-def type :obsolete-by)))
    (and tech (player-has-tech-p player tech))))

(defun production-cost (item)
  (ecase (first item)
    (:unit     (unit-def (second item) :cost 9999))
    (:building (building-def (second item) :cost 9999))
    (:wonder   (wonder-def (second item) :cost 9999))
    (:spaceship *spaceship-part-cost*)))

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
            ((:building :wonder) (pushnew (second item) (city-buildings city)))
            (:spaceship (incf (player-spaceship              ; assemble a ship part
                               (player-by-id state (city-owner city))))))
          (decf (city-shield-box city) cost)
          ;; buildings and wonders are one-shot; units and parts keep producing
          (when (member (first item) '(:building :wonder))
            (setf (city-production city) nil)))))))

(defun celebration-trade-bonus (state city)
  "Extra trade a celebrating republic/democracy city earns: +1 per worked tile
(centre included) already producing trade."
  (let* ((map (gs-map state))
         (gov (city-gov state city)))
    (if (and gov (government-def gov :trade-bonus))
        (+ (if (plusp (nth-value 2 (tile-yield (tile-at map (city-x city)
                                                        (city-y city)) gov))) 1 0)
           (loop for w in (city-worked city)
                 count (plusp (nth-value 2 (tile-yield (tile-at map (first w) (second w))
                                                       gov)))))
        0)))

(defun city-growth-cap (city)
  "Largest size CITY can reach with its water infrastructure: 8 without an
aqueduct (Civ1), unbounded with one."
  (if (member :aqueduct (city-buildings city)) most-positive-fixnum 8))

(defun city-pollution-chance (shields buildings)
  "Percent chance (per turn) a city emits pollution, from its SHIELDS, raised by
dirty factories/plants and lowered by clean infrastructure."
  (let ((p shields))
    (when (member :factory buildings)     (incf p (floor p 2)))   ; +50%
    (when (member :power-plant buildings) (incf p (floor p 2)))   ; +50%
    (when (member :mass-transit buildings)    (setf p (floor p 2)))
    (when (member :recycling-center buildings)(setf p (floor p 2)))
    (when (member :hydro-plant buildings)     (setf p (floor p 2)))
    (when (member :nuclear-plant buildings)   (setf p (floor p 2)))
    (max 0 p)))

(defun pollute-random-tile (state city)
  "Blight a random eligible tile in CITY's work radius with pollution."
  (let* ((map (gs-map state))
         (cands (loop for (x y tile) in (neighbors map (city-x city) (city-y city))
                      unless (or (tile-pollution tile) (tile-city tile)
                                 (eq (tile-terrain tile) :ocean))
                        collect tile)))
    (when cands
      (setf (tile-pollution (nth (gs-rand state (length cands)) cands)) t))))

(defun maybe-pollute (state city shields)
  "Once a civilization has Industrialization, a high-output city may pollute a
nearby tile this turn."
  (let ((owner (player-by-id state (city-owner city))))
    (when (and owner (player-has-tech-p owner :industrialization))
      (let ((chance (city-pollution-chance shields (city-buildings city))))
        (when (and (plusp chance) (< (gs-rand state 100) chance))
          (pollute-random-tile state city))))))

(defparameter *warming-shift*
  '((:grassland . :plains) (:plains . :desert) (:forest . :plains))
  "How global warming degrades land terrain: hotter and drier.")
(defparameter *warming-threshold*
  3 "Polluted tiles tolerated before global warming becomes a risk.")

(defun count-pollution (state)
  "Number of polluted tiles across the whole map."
  (let ((n 0))
    (do-tiles (x y tile (gs-map state)) (declare (ignore x y))
      (when (tile-pollution tile) (incf n)))
    n))

(defun degrade-one-tile (state)
  "Convert a random eligible land tile to its warmer terrain (see
*WARMING-SHIFT*).  Returns T if a tile changed."
  (let ((cands '()))
    (do-tiles (x y tile (gs-map state)) (declare (ignore x y))
      (when (assoc (tile-terrain tile) *warming-shift*) (push tile cands)))
    (when cands
      (let ((tile (nth (gs-rand state (length cands)) cands)))
        (setf (tile-terrain tile) (cdr (assoc (tile-terrain tile) *warming-shift*)))
        t))))

(defun process-global-warming (state)
  "Accumulated pollution may trigger global warming, which degrades land terrain
across the map.  The more polluted tiles, the likelier and worse each event."
  (let ((poll (count-pollution state)))
    (when (> poll *warming-threshold*)
      (let ((chance (min 90 (* (- poll *warming-threshold*) 15))))
        (when (< (gs-rand state 100) chance)
          (incf (gs-warming state))
          (dotimes (i (min poll 3)) (degrade-one-tile state)))))))

(defun process-city (state city)
  (city-auto-work state city)
  (multiple-value-bind (food shields trade) (city-yields state city)
    (multiple-value-bind (happy content unhappy) (city-happiness state city trade)
      (declare (ignore content))
      (let* ((size (city-size city))
             (disorder (> unhappy happy))
             (p (player-by-id state (city-owner city)))
             (gov (and p (player-government p)))
             (celebrating (and (>= size 3) (zerop unhappy)
                               (>= happy (ceiling size 2)))))
        (cond
          (disorder
           ;; civil disorder: no growth, no production, no economy this turn
           nil)
          (t
           ;; growth: each citizen eats 2 food
           (let ((net (- food (* 2 size)))
                 (threshold (* 10 (1+ size)))
                 (cap (city-growth-cap city))
                 ;; "We Love the King" rapture growth: a celebrating republic or
                 ;; democracy with a food surplus grows by 1 every turn
                 (rapture (and celebrating gov (government-def gov :trade-bonus))))
             (incf (city-food-box city) net)
             (cond ((and rapture (>= net 0) (< size cap))
                    (incf (city-size city))
                    (setf (city-food-box city)
                          (if (member :granary (city-buildings city))
                              (floor threshold 2) 0)))
                   ((and (>= (city-food-box city) threshold) (< size cap))
                    (incf (city-size city))
                    ;; a granary keeps half the food box after growth
                    (setf (city-food-box city)
                          (if (member :granary (city-buildings city))
                              (floor threshold 2) 0)))
                   ((>= (city-food-box city) threshold)      ; full but capped: hold
                    (setf (city-food-box city) threshold))
                   ((minusp (city-food-box city))            ; starvation
                    (when (> (city-size city) 1) (decf (city-size city)))
                    (setf (city-food-box city) 0))))
           ;; production
           (incf (city-shield-box city) shields)
           (city-try-complete state city)
           ;; a celebrating republic/democracy city earns bonus trade
           (when celebrating
             (incf trade (celebration-trade-bonus state city)))
           ;; economy: trade splits into the owner's gold and science.  Science is
           ;; accrued in fine (percent-trade) units so a city with only 1 trade
           ;; still makes progress instead of flooring to zero (research-cost is
           ;; scaled to match); gold keeps whole units.  Anarchy does no research.
           (when p
             (let ((sci (* trade (player-science-rate p))))
               (when (member :library (city-buildings city))       ; library +50%
                 (setf sci (floor (* sci 3) 2)))
               (when (member :great-library (city-buildings city)) ; great library +50%
                 (setf sci (floor (* sci 3) 2)))
               (incf (player-gold p) (floor (* trade (player-tax-rate p)) 100))
               (when (government-def (player-government p) :science)
                 (incf (player-beakers p) sci))))
           ;; dirty industry may blight a nearby tile with pollution
           (maybe-pollute state city shields)))))))

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

(declaim (ftype (function (t) t) clamp-rates))   ; defined in commands.lisp

(defun process-revolution (state)
  "Count down each player's anarchy; when it ends, the chosen government takes
power and the player's rates are clamped to its cap."
  (loop for p across (gs-players state) do
    (when (plusp (player-anarchy-left p))
      (decf (player-anarchy-left p))
      (when (and (zerop (player-anarchy-left p)) (player-gov-target p))
        (setf (player-government p) (player-gov-target p)
              (player-gov-target p) nil)
        (clamp-rates p)))))

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
             (case (unit-work u)
               (:clean-pollution (setf (tile-pollution tile) nil))   ; clears the blight
               (:clear-forest    (setf (tile-terrain tile)           ; reveal the land beneath
                                       (or (cdr (assoc (tile-terrain tile)
                                                       (terraform-def :clear-forest :becomes)))
                                           :plains)))
               (t (setf (tile-improvement tile (terraform-def (unit-work u) :flag)) t)))
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

;;; --- victory ---------------------------------------------------------------

(defparameter *spaceship-parts* 10 "Parts that complete a spaceship.")
(defparameter *spaceship-flight* 15 "Turns a launched spaceship takes to arrive.")

(defun player-alive-p (state player)
  "A non-barbarian player is still in the game while it holds a city or a unit."
  (and (not (eq (player-kind player) :barbarian))
       (or (loop for c being the hash-values of (gs-cities state)
                 thereis (= (city-owner c) (player-id player)))
           (loop for u being the hash-values of (gs-units state)
                 thereis (= (unit-owner u) (player-id player))))))

(defun declare-victory (state pid kind)
  (setf (gs-winner state) pid (gs-victory state) kind (gs-phase state) :game-over))

(defun process-victory (state)
  "Decide the game: a launched spaceship that has arrived wins the space race;
otherwise the last surviving civilization wins by conquest."
  (unless (gs-winner state)
    ;; launch a completed ship; land an arrived one
    (loop for p across (gs-players state)
          when (and (>= (player-spaceship p) *spaceship-parts*) (zerop (player-landing p)))
            do (setf (player-landing p) (+ (gs-turn state) *spaceship-flight*)))
    (loop for p across (gs-players state)
          when (and (plusp (player-landing p)) (>= (gs-turn state) (player-landing p))
                    (player-alive-p state p))
            do (return-from process-victory (declare-victory state (player-id p) :space)))
    ;; conquest: exactly one non-barbarian survivor (and someone has been knocked out)
    (let* ((civs (remove-if (lambda (p) (eq (player-kind p) :barbarian)) (gs-players state)))
           (alive (remove-if-not (lambda (p) (player-alive-p state p)) civs)))
      (when (and (> (length civs) 1) (= (length alive) 1))
        (declare-victory state (player-id (elt alive 0)) :conquest)))))

(defun end-turn (state)
  "Advance the whole world one turn and return STATE.
Phases: AI players act -> process cities -> research -> heal units -> refresh
units -> advance clock.  (A full game would interleave per-player movement and
combat phases here.)"
  (run-ai-players state)
  (process-cities state)
  (process-global-warming state); accumulated pollution may degrade terrain
  (process-economy state)       ; charge improvement upkeep (sell on bankruptcy)
  (process-revolution state)    ; end anarchy; adopt the chosen government
  (process-research state)
  (heal-units state)            ; rested/garrisoned units recover HP
  (refresh-units state)
  (process-terraform state)     ; advance settler road/irrigation/mine jobs
  (process-goto state)          ; units on :goto walk toward their target
  (update-visibility state)     ; reveal newly-scouted tiles (fog of war)

  (incf (gs-turn state))
  (setf (gs-year state) (turn->year (gs-turn state)))
  (process-victory state)       ; conquest or space-race win?
  state)
