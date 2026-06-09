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
  (let ((map (gs-map state)) (f 0) (s 0) (tr 0))
    (multiple-value-bind (cf cs ct)
        (tile-yield (tile-at map (city-x city) (city-y city)))
      (incf f (max 1 cf)) (incf s (max 1 cs)) (incf tr (max 1 ct)))
    (dolist (w (city-worked city))
      (multiple-value-bind (a b c) (tile-yield (tile-at map (first w) (second w)))
        (incf f a) (incf s b) (incf tr c)))
    (values f s tr)))

;;; --- per-turn city processing ---------------------------------------------

(defun production-cost (item)
  (ecase (first item)
    (:unit     (unit-def (second item) :cost 9999))
    (:building (building-def (second item) :cost 9999))))

(defun city-try-complete (state city)
  "Finish the current production if enough shields have accumulated."
  (let ((item (city-production city)))
    (when item
      (let ((cost (production-cost item)))
        (when (>= (city-shield-box city) cost)
          (ecase (first item)
            (:unit (register-unit state :type (second item)
                                  :owner (city-owner city)
                                  :x (city-x city) :y (city-y city)))
            (:building (pushnew (second item) (city-buildings city))))
          (decf (city-shield-box city) cost)
          ;; buildings are one-shot; units keep producing
          (when (eq (first item) :building)
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
             (setf (city-food-box city) 0))
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
        (incf (player-gold p)    (floor (* trade (player-tax-rate p)) 100))
        (incf (player-beakers p) (* trade (player-science-rate p)))))))

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

(defun refresh-units (state)
  "Restore every unit's movement allowance at the start of a turn."
  (maphash (lambda (id u) (declare (ignore id))
             (setf (unit-moves-left u) (unit-def (unit-type u) :move 1)))
           (gs-units state)))

(defun turn->year (turn)
  "Map a turn number to a (simplified) calendar year."
  (+ -4000 (* (1- turn) 40)))

;; defined in ai.lisp (loaded after this file); declared so END-TURN compiles clean
(declaim (ftype (function (t) t) run-ai-players))

(defun end-turn (state)
  "Advance the whole world one turn and return STATE.
Phases: AI players act -> process cities -> research -> refresh units ->
advance clock.  (A full game would interleave per-player movement/combat here.)"
  (run-ai-players state)
  (process-cities state)
  (process-research state)
  (refresh-units state)
  (incf (gs-turn state))
  (setf (gs-year state) (turn->year (gs-turn state)))
  state)
