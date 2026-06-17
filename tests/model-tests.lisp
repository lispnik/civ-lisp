;;;; model-tests.lisp -- FiveAM tests for civ-model.

(in-package #:civ-model/tests)

(def-suite civ-model :description "The civ-model game model.")
(in-suite civ-model)

(defun run-tests () (run! 'civ-model))

;;; --- helpers ---------------------------------------------------------------

(defun bare-state (w h &key (terrain :grassland) (seed 0))
  "A clean game-state: a uniform W x H map, two players, no units/cities."
  (civ-model::%make-game-state
   :map (civ-model::make-game-map w h :terrain terrain)
   :players (vector (make-player :id 1 :name "P1" :kind :human)
                    (make-player :id 2 :name "P2" :kind :ai))
   :random (sb-ext:seed-random-state seed)))

(defun add-unit (state type owner x y)
  (civ-model::register-unit state :type type :owner owner :x x :y y))

(defun terrain! (state x y terr)
  (setf (tile-terrain (tile-at (gs-map state) x y)) terr))

(defun a-city (state &optional owner)
  (loop for c being the hash-values of (gs-cities state)
        when (or (null owner) (= (city-owner c) owner)) return c))

(defun a-unit (state owner type)
  (loop for u being the hash-values of (gs-units state)
        when (and (= (unit-owner u) owner) (eq (unit-type u) type)) return u))

(defun city-named (state name)
  (loop for c being the hash-values of (gs-cities state)
        when (string= (city-name c) name) return c))

(defun unit-positions (state)
  (sort (loop for u being the hash-values of (gs-units state)
              collect (list (unit-id u) (unit-type u) (unit-x u) (unit-y u)))
        #'< :key #'first))

;;; --- definitions & map -----------------------------------------------------

(test tech-tree
  ;; the full Civilization advance tree loaded and is internally consistent
  (is (= 67 (hash-table-count *techs*)))
  (maphash (lambda (tech def) (declare (ignore def))
             (dolist (pre (tech-def tech :prereqs))
               (is-true (nth-value 1 (gethash pre *techs*)))))   ; prereq exists
           *techs*)
  ;; classic no-prerequisite starting advances
  (dolist (start '(:pottery :bronze-working :masonry :alphabet
                   :ceremonial-burial :horseback-riding :the-wheel))
    (is (null (tech-def start :prereqs))))
  ;; a deep advance has the right prerequisites
  (is (equal '(:flight :electricity) (tech-def :advanced-flight :prereqs))))

(test tech-tree-fully-wired
  ;; A durable guard so the advance tree can't silently rot as content is added:
  ;; every advance is reachable, every requirement names a real advance, and no
  ;; advance is a useless orphan (it must either unlock something or feed another).
  (let ((techs (loop for k being the hash-keys of *techs* collect k)))
    ;; 1. reachability: every advance is derivable from the no-prereq roots
    ;;    (this also rejects any prerequisite cycle, which would be unreachable)
    (let ((known (make-hash-table)))
      (dolist (tk techs)
        (when (null (tech-def tk :prereqs)) (setf (gethash tk known) t)))
      (loop with changed = t while changed do
        (setf changed nil)
        (dolist (tk techs)
          (when (and (not (gethash tk known))
                     (every (lambda (p) (gethash p known)) (tech-def tk :prereqs)))
            (setf (gethash tk known) t changed t))))
      (dolist (tk techs)
        (is-true (gethash tk known) "advance ~A is unreachable from the roots" tk)))
    ;; 2. everything gated by an advance (units, buildings, wonders, governments)
    ;;    names a real advance
    (dolist (table (list *units* *buildings* *wonders* *governments*))
      (loop for k being the hash-keys of table
            for req = (def-get table k :requires)
            when req
              do (is-true (gethash req *techs*)
                          "~A requires non-existent advance ~A" k req)))
    ;; 3. no orphan advances: each either unlocks content (a unit/building/wonder/
    ;;    government, or the code-gated spaceship) or is a prerequisite of another
    (let ((useful (make-hash-table)))
      (dolist (table (list *units* *buildings* *wonders* *governments*))
        (loop for k being the hash-keys of table
              for req = (def-get table k :requires)
              when req do (setf (gethash req useful) t)))
      ;; the spaceship is gated in CMD-SET-PRODUCTION, not a def table
      (dolist (req '(:space-flight :fusion-power)) (setf (gethash req useful) t))
      (dolist (tk techs)
        (dolist (pre (tech-def tk :prereqs)) (setf (gethash pre useful) t)))
      (dolist (tk techs)
        (is-true (gethash tk useful)
                 "advance ~A is an orphan: it unlocks nothing and leads nowhere" tk)))))

(test terrain-and-unit-defs
  (is (= 2 (terrain-def :grassland :food)))
  (is (= 2 (terrain-def :forest :shields)))
  (is (= 50 (terrain-def :hills :defense)))
  (is (= 0 (unit-def :settlers :attack)))
  (is (= 3 (unit-def :legion :attack)))
  (is (eq :sea (unit-def :battleship :domain)))
  (is (eq :air (unit-def :fighter :domain)))
  (is (member :found-city (unit-def :settlers :abilities))))

(test map-basics
  (let ((m (civ-model::make-game-map 5 4)))
    (is (= 5 (map-width m)))
    (is (= 4 (map-height m)))
    (is-true (in-bounds-p m 0 0))
    (is-true (in-bounds-p m 4 3))
    (is-false (in-bounds-p m 5 0))
    (is-false (in-bounds-p m -1 0))
    (is (null (tile-at m 0 4)))                   ; past the south pole: no tile
    (is (eq (tile-at m 0 0) (tile-at m 5 0)))     ; x wraps around the cylinder
    (is (eq (tile-at m 4 1) (tile-at m -1 1)))
    (is (eq :grassland (tile-terrain (tile-at m 2 2))))))

(test neighbor-counts
  (let ((m (civ-model::make-game-map 5 5)))
    (is (= 5 (length (neighbors m 0 0))))    ; "corner": x wraps, only the pole row missing
    (is (= 8 (length (neighbors m 0 2))))    ; left edge wraps to a full 8
    (is (= 5 (length (neighbors m 2 0))))    ; top edge (pole)
    (is (= 8 (length (neighbors m 2 2))))))  ; interior

;;; --- yields -----------------------------------------------------------------

(test tile-yields
  (let* ((s (bare-state 5 5)) (tile (tile-at (gs-map s) 2 2)))
    (multiple-value-bind (f sh tr) (tile-yield tile)
      (is (= 2 f)) (is (= 0 sh)) (is (= 0 tr)))
    (setf (tile-river tile) t)
    (is (= 1 (nth-value 2 (tile-yield tile))))     ; river: +1 trade
    (setf (tile-special tile) t)
    (is (= 1 (nth-value 1 (tile-yield tile))))     ; grassland special: +1 shield
    (setf (tile-terrain tile) :forest (tile-river tile) nil (tile-special tile) nil)
    (multiple-value-bind (f sh tr) (tile-yield tile)
      (declare (ignore tr)) (is (= 1 f)) (is (= 2 sh)))))

(test city-center-minimum
  ;; grassland centre (2/0/0) must still give >=1 shield and >=1 trade
  (let ((s (bare-state 5 5)))
    (civ-model::register-city s :name "C" :owner 1 :x 2 :y 2)
    (multiple-value-bind (f sh tr) (city-yields s (a-city s))
      (is (>= f 1)) (is (>= sh 1)) (is (>= tr 1)))))

;;; --- new game ---------------------------------------------------------------

(test new-game-deterministic
  (let ((a (make-new-game :seed 7)) (b (make-new-game :seed 7)))
    (is (= 2 (length (gs-players a))))
    (is (= 4 (hash-table-count (gs-units a))))     ; 2 players x (settler+warrior)
    (is (eq :human (player-kind (player-by-id a 1))))
    (is (eq :ai (player-kind (player-by-id a 2))))
    (is (equal (unit-positions a) (unit-positions b)))))   ; same seed => same game

;;; --- commands ---------------------------------------------------------------

(test found-city
  (let* ((s (bare-state 6 6))
         (u (add-unit s :settlers 1 3 3)))
    (apply-command s (list :found-city :unit (unit-id u) :name "Rome"))
    (is (null (unit-by-id s (unit-id u))))         ; settler consumed
    (is (= 1 (hash-table-count (gs-cities s))))
    (let ((c (a-city s)))
      (is (string= "Rome" (city-name c)))
      (is (= 3 (city-x c))) (is (= 3 (city-y c)))
      (is (equal '(:unit :warriors) (city-production c)))
      (is (eql (city-id c) (tile-city (tile-at (gs-map s) 3 3)))))))

(test found-city-illegal
  (let* ((s (bare-state 6 6)) (w (add-unit s :warriors 1 2 2)))
    (signals command-error (apply-command s (list :found-city :unit (unit-id w)))))
  (let* ((s (bare-state 6 6))
         (a (add-unit s :settlers 1 2 2))
         (b (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id a)))
    (signals command-error (apply-command s (list :found-city :unit (unit-id b))))))

(test move-unit
  (let* ((s (bare-state 6 6)) (u (add-unit s :warriors 1 2 2)))
    (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))
    (is (= 3 (unit-x u))) (is (= 2 (unit-y u)))
    (is (= 0 (unit-moves-left u)))
    (is (member (unit-id u) (tile-units (tile-at (gs-map s) 3 2))))
    (is-false (member (unit-id u) (tile-units (tile-at (gs-map s) 2 2))))))

(test move-illegal
  (let* ((s (bare-state 4 4)) (u (add-unit s :warriors 1 0 0)))
    (signals command-error (apply-command s (list :move-unit :unit (unit-id u) :dx 0 :dy -1)))  ; off the pole
    (signals command-error (apply-command s (list :move-unit :unit (unit-id u) :dx 2 :dy 0)))   ; not one tile
    (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))
    (signals command-error (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0)))))  ; no moves left

(test set-production
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (apply-command s (list :set-production :city (city-id c) :item '(:unit :warriors)))
      (is (equal '(:unit :warriors) (city-production c)))
      (signals command-error
        (apply-command s (list :set-production :city (city-id c) :item '(:building :library))))
      (setf (gethash :writing (player-techs (player-by-id s 1))) t)
      (apply-command s (list :set-production :city (city-id c) :item '(:building :library)))
      (is (equal '(:building :library) (city-production c)))
      (signals command-error
        (apply-command s (list :set-production :city (city-id c) :item '(:unit :zerg)))))))

(test build-improvements-and-wonders
  (let* ((s (bare-state 8 8)) (st (add-unit s :settlers 1 4 4)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)) (p (player-by-id s 1)))
      (setf (gethash :pottery (player-techs p)) t      ; granary, hanging-gardens
            (gethash :masonry (player-techs p)) t)      ; walls, pyramids
      ;; an improvement: granary
      (apply-command s (list :set-production :city (city-id c) :item '(:building :granary)))
      (is (equal '(:building :granary) (city-production c)))
      ;; complete it and confirm it can't be re-built
      (setf (city-shield-box c) 999) (civ-model::city-try-complete s c)
      (is (member :granary (city-buildings c)))
      (signals command-error
        (apply-command s (list :set-production :city (city-id c) :item '(:building :granary))))
      ;; a wonder: pyramids
      (apply-command s (list :set-production :city (city-id c) :item '(:wonder :pyramids)))
      (is (equal '(:wonder :pyramids) (city-production c)))
      (setf (city-shield-box c) 999) (civ-model::city-try-complete s c)
      (is-true (wonder-built-p s :pyramids))
      ;; a wonder is one-per-game: another city can't build it
      (let* ((s2 (add-unit s :settlers 1 1 1)))
        (apply-command s (list :found-city :unit (unit-id s2) :name "Veii"))
        (signals command-error
          (apply-command s (list :set-production :city (city-id (city-named s "Veii"))
                                 :item '(:wonder :pyramids)))))
      ;; tech gate: great-library needs writing
      (signals command-error
        (apply-command s (list :set-production :city (city-id c) :item '(:wonder :great-library)))))))

(test building-effects
  ;; library boosts a city's science output by 50%
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)) (p (player-by-id s 1)))
      (civ-model::city-auto-work s c)
      (multiple-value-bind (f sh tr) (civ-model::city-yields s c) (declare (ignore f sh))
        (let ((base-sci (* tr (player-science-rate p))))
          (push :library (city-buildings c))
          (let ((b0 (player-beakers p)))
            (civ-model::process-city s c)
            (is (= (- (player-beakers p) b0)
                   (floor (* base-sci 3) 2))))))))   ; +50%
  ;; barracks -> veteran units, which hit harder
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (push :barracks (city-buildings c))
      (setf (city-production c) '(:unit :catapult) (city-shield-box c) 999)
      (civ-model::city-try-complete s c)
      (let ((vet (a-unit s 1 :catapult)))
        (is-true (unit-veteran vet))
        (is (= 9 (civ-model::attack-strength vet))))))  ; catapult 6 * 1.5
  ;; walls add +100% defense
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let* ((c (a-city s)) (u (add-unit s :phalanx 1 2 2)))   ; phalanx def 2, in city
      (let ((d0 (civ-model::defense-strength s u)))           ; city +50% -> 3
        (push :walls (city-buildings c))
        (is (> (civ-model::defense-strength s u) d0))))))     ; walls add more

(test wonder-effects
  ;; player-wonder-p sees a civ-wide wonder only in its owner's empire
  (let* ((s (bare-state 8 8)) (st (add-unit s :settlers 1 4 4)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (is-false (civ-model::player-wonder-p s 1 :hoover-dam))
      (push :hoover-dam (city-buildings c))
      (is-true  (civ-model::player-wonder-p s 1 :hoover-dam))
      (is-false (civ-model::player-wonder-p s 2 :hoover-dam))))   ; not the rival's
  ;; Hoover Dam acts as a power plant: +50% shields in every owned city
  (flet ((shield-gain (dam)
           (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
             (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
             (let ((c (a-city s)))
               (civ-model::city-auto-work s c)
               (when dam (push :hoover-dam (city-buildings c)))
               (setf (city-production c) '(:wonder :pyramids)   ; too dear to finish
                     (city-shield-box c) 0)
               (civ-model::process-city s c)
               (city-shield-box c)))))
    (is (= (shield-gain t) (floor (* (shield-gain nil) 3) 2))))
  ;; SETI program boosts science civ-wide by 50%
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)) (p (player-by-id s 1)))
      (civ-model::city-auto-work s c)
      (multiple-value-bind (f sh tr) (civ-model::city-yields s c) (declare (ignore f sh))
        (let ((base-sci (* tr (player-science-rate p))))
          (push :s-e-t-i-program (city-buildings c))
          (let ((b0 (player-beakers p)))
            (civ-model::process-city s c)
            (is (= (- (player-beakers p) b0) (floor (* base-sci 3) 2))))))))
  ;; Darwin's Voyage grants two free advances when completed
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let* ((c (a-city s)) (p (player-by-id s 1))
           (n0 (hash-table-count (player-techs p))))
      (setf (city-production c) '(:wonder :darwins-voyage) (city-shield-box c) 999)
      (civ-model::city-try-complete s c)
      (is-true (member :darwins-voyage (city-buildings c)))
      (is (= 2 (- (hash-table-count (player-techs p)) n0)))))
  ;; Magellan's Expedition gives the owner's ships +1 movement
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2))
         (ship (add-unit s :trireme 1 3 3)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (push :magellans-expedition (city-buildings (a-city s)))
    (civ-model::refresh-units s)
    (is (= (1+ (unit-def :trireme :move)) (unit-moves-left ship))))
  ;; the Lighthouse trains veteran ships
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (push :lighthouse (city-buildings c))
      (setf (city-production c) '(:unit :trireme) (city-shield-box c) 999)
      (civ-model::city-try-complete s c)
      (is-true (unit-veteran (a-unit s 1 :trireme))))))

(test city-economy
  ;; production multipliers: factory +50%, a power plant +50% more (but only with
  ;; a factory), a manufacturing plant +50% more again
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)) (base 8))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (is (= base (civm::city-shield-output s c base)))               ; no plant
      (push :factory (city-buildings c))
      (is (= (civm::pct+50 base) (civm::city-shield-output s c base))); +50%
      (push :power-plant (city-buildings c))
      (is (= (civm::pct+50 (civm::pct+50 base))                        ; +50% more
             (civm::city-shield-output s c base)))
      (push :mfg-plant (city-buildings c))
      (is (= (civm::pct+50 (civm::pct+50 (civm::pct+50 base)))         ; +50% again
             (civm::city-shield-output s c base)))))
  ;; a power plant does nothing without a factory to power
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)) (base 8))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (push :power-plant (city-buildings c))
      (is (= base (civm::city-shield-output s c base)))))
  ;; gold multipliers: marketplace +50%, bank +50% more, stock exchange +50% again
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)) (base 8))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (is (= base (civm::city-gold-output c base)))
      (push :marketplace (city-buildings c))
      (is (= (civm::pct+50 base) (civm::city-gold-output c base)))
      (push :bank (city-buildings c))
      (is (= (civm::pct+50 (civm::pct+50 base)) (civm::city-gold-output c base)))
      (push :stock-exchange (city-buildings c))
      (is (= (civm::pct+50 (civm::pct+50 (civm::pct+50 base)))
             (civm::city-gold-output c base)))
      ;; luxury follows the marketplace and bank but NOT the stock exchange
      (is (= (civm::pct+50 (civm::pct+50 base)) (civm::city-luxury-output c base)))))
  ;; university boosts science by 50%, stacking with a library (+50% each)
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)) (p (player-by-id s 1)))
      (civ-model::city-auto-work s c)
      (multiple-value-bind (f sh tr) (civ-model::city-yields s c) (declare (ignore f sh))
        (let ((base-sci (* tr (player-science-rate p))))
          (setf (city-buildings c) (list :library :university))
          (let ((b0 (player-beakers p)))
            (civ-model::process-city s c)
            (is (= (- (player-beakers p) b0)
                   (civm::pct+50 (civm::pct+50 base-sci)))))))))   ; +50% +50%
  ;; a courthouse halves the corruption a despotism loses, keeping more trade
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 3 3)))
    (loop for y from 1 to 5 do (loop for x from 1 to 5    ; rivers everywhere -> trade
                                     do (setf (tile-river (tile-at (gs-map s) x y)) t)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (setf (city-size c) 4)
      (civ-model::city-auto-work s c)
      (let ((without (nth-value 2 (civ-model::city-yields s c))))
        (push :courthouse (city-buildings c))
        (is (> (nth-value 2 (civ-model::city-yields s c)) without)))))
  ;; water infrastructure lifts the size cap: 8 -> 12 (aqueduct) -> unbounded (sewer)
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (is (= 8 (civ-model::city-growth-cap c)))
      (push :aqueduct (city-buildings c))
      (is (= 12 (civ-model::city-growth-cap c)))
      (push :sewer-system (city-buildings c))
      (is (= most-positive-fixnum (civ-model::city-growth-cap c))))))

(test population-formula
  ;; the classic Civ1 city-population table: 10k, 30k, 60k, 100k, 150k, ...
  (let* ((s (bare-state 8 8)) (st (add-unit s :settlers 1 4 4)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (dolist (pair '((1 . 10000) (2 . 30000) (3 . 60000) (4 . 100000) (5 . 150000)))
        (setf (city-size c) (car pair))
        (is (= (cdr pair) (civ-model::city-population c))))
      ;; an empire's population sums its cities
      (setf (city-size c) 4)                                  ; 100,000
      (let ((st2 (add-unit s :settlers 1 1 1)))
        (apply-command s (list :found-city :unit (unit-id st2) :name "Veii"))
        (setf (city-size (city-named s "Veii")) 2)            ; +30,000
        (is (= 130000 (civ-model::civ-population s 1)))
        (is (= 0 (civ-model::civ-population s 2)))))))         ; rival has no cities

(test civilization-score
  ;; Civ1-style score: advances, wonders, peace, citizens, less pollution
  (let* ((s (bare-state 8 8)) (st (add-unit s :settlers 1 4 4)) (p (player-by-id s 1)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (setf (gethash :pottery (player-techs p)) t
            (gethash :masonry (player-techs p)) t)        ; 2 advances -> 6 pts
      (push :pyramids (city-buildings c))                  ; 1 wonder -> 5 pts
      (setf (player-peace-turns p) 10))                    ; peace -> 10 pts
    (let* ((bd (civ-model::score-breakdown s p))
           (total (reduce #'+ bd :key #'cdr)))
      (is (= 6  (cdr (assoc "Advances" bd :test #'string=))))
      (is (= 5  (cdr (assoc "Wonders"  bd :test #'string=))))
      (is (= 10 (cdr (assoc "Peace"    bd :test #'string=))))
      (is (= (max 0 total) (civ-model::compute-score s p)))   ; total, floored at 0
      (is (>= (civ-model::compute-score s p) 21)))))           ; 6+5+10 + citizens

(test peace-bonus-accrues-only-at-peace
  (let* ((s (bare-state 6 6)) (p (player-by-id s 1)))
    (civ-model::update-scores s)
    (is (= 1 (player-peace-turns p)))            ; at peace with everyone -> +1
    (apply-command s (list :declare-war :player 1 :against 2))
    (civ-model::update-scores s)
    (is (= 1 (player-peace-turns p)))            ; now at war -> no peace credit
    (apply-command s (list :make-peace :player 1 :against 2))
    (civ-model::update-scores s)
    (is (= 2 (player-peace-turns p)))))          ; peace restored -> +1 again

(test city-name-roster
  ;; each civ gets its nation's city roster, matched by name
  (let ((s (make-new-game :seed 1 :players '("Rome" "Egypt" "Zulu"))))
    (is (string= "Rome"     (first (player-city-names (player-by-id s 1)))))   ; Roman
    (is (string= "Thebes"   (first (player-city-names (player-by-id s 2)))))   ; Egyptian
    (is (string= "Zimbabwe" (first (player-city-names (player-by-id s 3))))))) ; Zulu

(test can-found-here
  (let* ((s (bare-state 6 6)) (settler (add-unit s :settlers 1 2 2))
         (warrior (add-unit s :warriors 1 4 4)))
    (is-true  (civ-model::can-found-here-p s settler))   ; a settler on open ground
    (is-false (civ-model::can-found-here-p s warrior))   ; warriors can't found
    ;; once a city sits on the tile, no second city there
    (civ-model::register-city s :name "Rome" :owner 1 :x 2 :y 2)
    (is-false (civ-model::can-found-here-p s settler))))

(test nations-start-with-a-free-advance
  ;; a deviation from Civ1: each nation begins knowing one root advance
  (let ((s (make-new-game :seed 1 :players '("Roman" "Mongol" "Chinese"))))
    (is-true (player-has-tech-p (player-by-id s 1) :bronze-working))    ; Roman
    (is-true (player-has-tech-p (player-by-id s 2) :horseback-riding))  ; Mongol
    (is-true (player-has-tech-p (player-by-id s 3) :masonry)))          ; Chinese
  ;; every starting advance is a real root advance, so no prerequisite dangles
  (dolist (entry civ-model::*nation-techs*)
    (dolist (tch (cdr entry))
      (is-true (gethash tch *techs*))
      (is (null (tech-def tch :prereqs))))))

(test next-city-name-skips-used
  (let ((s (bare-state 8 8)))
    (setf (player-city-names (player-by-id s 1)) '("Rome" "Caesarea" "Carthage"))
    (is (string= "Rome" (next-city-name s 1)))
    (civ-model::register-city s :name "Rome" :owner 1 :x 2 :y 2)
    (is (string= "Caesarea" (next-city-name s 1)))     ; Rome taken -> next free name
    (civ-model::register-city s :name "Caesarea" :owner 1 :x 4 :y 4)
    (civ-model::register-city s :name "Carthage" :owner 1 :x 6 :y 6)
    (is (search "P1" (next-city-name s 1)))))           ; roster spent -> numbered fallback

(test economy-rates-and-eta
  (let* ((s (bare-state 8 8)) (st (add-unit s :settlers 1 4 4)) (p (player-by-id s 1)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (civ-model::city-auto-work s c)
      ;; the empire's per-turn science equals its single city's contribution
      (let ((trade (nth-value 2 (civ-model::city-yields s c))))
        (is (= (or (civ-model::city-research-output s c trade) 0)
               (civ-model::civ-research-rate s 1)))))
    (is (integerp (civ-model::civ-gold-rate s 1)))         ; net gold/turn is a number
    ;; ETA: with the next advance's beakers already banked, it lands in one turn
    (apply-command s (list :set-research :player 1 :tech :pottery))
    (setf (player-beakers p) (civ-model::research-cost p))
    (is (= 1 (civ-model::research-eta s p)))
    ;; anarchy does no science -> no progress -> NIL eta
    (setf (player-beakers p) 0 (player-government p) :anarchy)
    (is (null (civ-model::research-eta s p)))))

(test ai-economy
  ;; ai-best-wonder picks the first buildable, unbuilt wonder it has tech for
  (let* ((s (bare-state 6 6)) (p (player-by-id s 1)))
    (is-false (civ-model::ai-best-wonder s p))           ; no tech yet
    (setf (gethash :masonry (player-techs p)) t)         ; unlocks pyramids
    (is (eq :pyramids (civ-model::ai-best-wonder s p)))
    (setf (gethash :pottery (player-techs p)) t)         ; unlocks hanging-gardens
    (let* ((st (add-unit s :settlers 1 2 2)))            ; build the pyramids somewhere
      (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
      (push :pyramids (city-buildings (a-city s))))
    (is (eq :hanging-gardens (civ-model::ai-best-wonder s p))))  ; pyramids taken
  ;; ai-best-government prefers the best form the player has tech for
  (let* ((s (bare-state 6 6)) (p (player-by-id s 1)))
    (is-false (civ-model::ai-best-government s p))        ; only despotism available
    (setf (gethash :monarchy (player-techs p)) t)
    (is (eq :monarchy (civ-model::ai-best-government s p)))
    (setf (gethash :the-republic (player-techs p)) t
          (gethash :code-of-laws (player-techs p)) t
          (gethash :writing (player-techs p)) t)
    (is (eq :republic (civ-model::ai-best-government s p)))
    (setf (gethash :democracy (player-techs p)) t
          (gethash :banking (player-techs p)) t
          (gethash :invention (player-techs p)) t
          (gethash :university (player-techs p)) t)
    (is (eq :democracy (civ-model::ai-best-government s p)))))

(test ai-personalities
  ;; each AI is dealt a temperament at game start; the human and barbarians none
  (let ((s (make-new-game :seed 1 :players '("You" "A" "B" "C" "D") :barbarians t)))
    (is (null (player-personality (player-by-id s 1))))          ; the human
    (is (eq :aggressive   (player-personality (player-by-id s 2))))  ; AIs cycle
    (is (eq :expansionist (player-personality (player-by-id s 3))))
    (is (eq :builder      (player-personality (player-by-id s 4))))
    (let ((barb (find :barbarian (gs-players s) :key #'player-kind)))
      (is (null (player-personality barb)))))
  ;; ai-trait reads the profile, with a default for personality-less players
  (let* ((s (bare-state 6 6)) (p (player-by-id s 2)))
    (setf (player-personality p) :aggressive)
    (is (= 9 (civ-model::ai-trait p :war-chance 3)))
    (is (eq :military (civ-model::ai-trait p :tech-focus)))
    (is (= 3 (civ-model::ai-trait (player-by-id s 1) :war-chance 3)))   ; no profile
    ;; the tech-focus steers which advance the AI chases first
    (is (eq :bronze-working (first (civ-model::ai-tech-goals p))))      ; military focus
    (setf (player-personality p) :scientific)
    (is (eq :alphabet (first (civ-model::ai-tech-goals p))))))          ; science focus

(test difficulty-levels
  (is (= 3 (difficulty-level (bare-state 4 4))))                  ; default Prince
  (is (= 1 (difficulty-level (make-new-game :seed 1 :difficulty :chieftain))))
  (is (= 5 (difficulty-level (make-new-game :seed 1 :difficulty :emperor)))))

(test fortify-and-clear
  (let* ((s (bare-state 6 6)) (u (add-unit s :warriors 1 2 2)))
    (apply-command s (list :fortify :unit (unit-id u)))
    (is (eq :fortified (unit-orders u)))
    (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))
    (is (eq :idle (unit-orders u)))))           ; moving breaks fortify

(test wake-clears-orders
  (let* ((s (bare-state 6 6)) (u (add-unit s :warriors 1 2 2)))
    (apply-command s (list :fortify :unit (unit-id u)))
    (apply-command s (list :wake :unit (unit-id u)))
    (is (eq :idle (unit-orders u)))))           ; clicking a fortified unit wakes it

(test terrain-domain
  ;; land units may not enter ocean, but rivers (land terrain) are fine
  (let ((s (bare-state 6 6)))
    (terrain! s 3 2 :ocean)
    (setf (tile-river (tile-at (gs-map s) 1 2)) t)   ; river on a land tile
    (let ((u (add-unit s :warriors 1 2 2)))
      (signals command-error (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))) ; ->ocean
      (is (= 2 (unit-x u)))                          ; didn't move
      (apply-command s (list :move-unit :unit (unit-id u) :dx -1 :dy 0))  ; ->river tile
      (is (= 1 (unit-x u)))))                         ; rivers are passable
  ;; sea units may not move onto land, but move on ocean
  (let ((s (bare-state 6 6)))
    (terrain! s 2 2 :ocean) (terrain! s 3 2 :ocean)
    (let ((u (add-unit s :trireme 1 2 2)))
      (signals command-error (apply-command s (list :move-unit :unit (unit-id u) :dx 0 :dy 1))) ; ->land
      (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))  ; ->ocean
      (is (= 3 (unit-x u))))))

;;; --- combat -----------------------------------------------------------------

(test combat-strong-beats-weak
  (let ((wins 0))
    (dotimes (i 50)
      (let* ((s (bare-state 6 6 :seed i))
             (a (add-unit s :legion 1 2 2))
             (d (add-unit s :warriors 2 4 4)))
        (when (eq :attacker (resolve-combat s a d)) (incf wins))))
    (is (>= wins 45))))                         ; legion (4) crushes warriors (1)

(test combat-via-move-advances
  (let* ((s (bare-state 6 6 :seed 1))
         (a (add-unit s :legion 1 2 2))
         (d (add-unit s :warriors 2 3 2)))
    (setf (civ-model::relation s 1 2) :war)
    (apply-command s (list :move-unit :unit (unit-id a) :dx 1 :dy 0))
    (is (null (unit-by-id s (unit-id d))))      ; defender destroyed
    (is (= 3 (unit-x a)))))                     ; attacker advanced onto the tile

(test settlers-cannot-attack
  (let* ((s (bare-state 6 6))
         (a (add-unit s :settlers 1 2 2)))
    (add-unit s :warriors 2 3 2)
    (signals command-error (apply-command s (list :move-unit :unit (unit-id a) :dx 1 :dy 0)))))

(test defense-bonuses
  (let* ((s (bare-state 6 6)) (u (add-unit s :phalanx 1 2 2)))   ; phalanx def 2
    (is (= 2 (civ-model::defense-strength s u)))
    (terrain! s 2 2 :hills)                                       ; +50%
    (is (= 3 (civ-model::defense-strength s u)))
    (terrain! s 2 2 :grassland)
    (apply-command s (list :fortify :unit (unit-id u)))           ; +50%
    (is (= 3 (civ-model::defense-strength s u)))))

(test combat-carries-damage
  (let ((damaged 0))
    (dotimes (i 30)
      (let* ((s (bare-state 6 6 :seed (+ 100 i)))
             (a (add-unit s :warriors 1 2 2))
             (d (add-unit s :warriors 2 3 2)))
        (resolve-combat s a d)
        (let ((winner (or (unit-by-id s (unit-id a)) (unit-by-id s (unit-id d)))))
          (when (< (unit-hp winner) civ-model::+max-hp+) (incf damaged)))))
    (is (>= damaged 25))))                      ; the winner almost always took hits

;;; --- healing ----------------------------------------------------------------

(test healing-open-rested
  (let* ((s (bare-state 6 6)) (u (add-unit s :warriors 1 2 2)))
    (setf (unit-hp u) 3)
    (heal-units s)
    (is (= 5 (unit-hp u)))))                    ; +2 resting in the open

(test healing-fortified
  (let* ((s (bare-state 6 6)) (u (add-unit s :warriors 1 2 2)))
    (setf (unit-hp u) 3)
    (apply-command s (list :fortify :unit (unit-id u)))
    (heal-units s)
    (is (= 7 (unit-hp u)))))                    ; +4 fortified

(test healing-city-full
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "C"))
    (let ((g (add-unit s :warriors 1 2 2)))
      (setf (unit-hp g) 2)
      (heal-units s)
      (is (= civ-model::+max-hp+ (unit-hp g))))))

(test healing-none-after-move
  (let* ((s (bare-state 6 6)) (u (add-unit s :warriors 1 2 2)))
    (setf (unit-hp u) 4)
    (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))
    (heal-units s)
    (is (= 4 (unit-hp u)))))                    ; moved => no heal

;;; --- zones of control -------------------------------------------------------

(test zoc-predicate
  (let ((s (bare-state 6 6)))
    (setf (civ-model::relation s 1 2) :war)
    (add-unit s :warriors 2 3 2)
    (is-true (enemy-adjacent-p s 2 2 1))
    (is-false (enemy-adjacent-p s 0 0 1))))

(test zoc-blocks-slip
  (let ((s (bare-state 6 6)))
    (setf (civ-model::relation s 1 2) :war)
    (add-unit s :warriors 2 3 1)
    (add-unit s :warriors 2 3 3)
    (let ((u (add-unit s :legion 1 3 2)))
      (signals command-error
        (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))))))

(test zoc-attack-allowed
  (let ((s (bare-state 6 6 :seed 1)))
    (setf (civ-model::relation s 1 2) :war)
    (add-unit s :warriors 2 3 1)
    (let ((d (add-unit s :warriors 2 3 3))
          (u (add-unit s :legion 1 3 2)))
      (finishes (apply-command s (list :move-unit :unit (unit-id u) :dx 0 :dy 1)))
      (is (null (unit-by-id s (unit-id d)))))))

(test zoc-friendly-tile-exempt
  (let ((s (bare-state 6 6)))
    (setf (civ-model::relation s 1 2) :war)
    (add-unit s :warriors 2 3 1)
    (add-unit s :warriors 2 3 3)
    (add-unit s :warriors 1 4 2)                ; friendly at destination
    (let ((u (add-unit s :legion 1 3 2)))
      (finishes (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0)))
      (is (= 4 (unit-x u))))))

;;; --- turn loop, research, growth, AI ---------------------------------------

(test turn-advances
  (let ((s (make-new-game :seed 1)))
    (is (= 1 (gs-turn s)))
    (end-turn s)
    (is (= 2 (gs-turn s)))
    (is (= -3950 (gs-year s)))))    ; 50 years/turn in antiquity (Civ1 schedule)

(test civ1-year-schedule-accelerates
  (flet ((y (turn) (civ-model::turn->year turn)))
    (is (= -4000 (y 1)))                       ; the game starts in 4000 BC
    (is (= -1000 (y 61)))                       ; 60 turns of 50 years -> 1000 BC
    (is (= 50 (- (y 2) (y 1))))                 ; 50 years/turn early
    (is (loop for turn from 2 to 250 always (> (y turn) (y (1- turn))))) ; strictly rising
    (is (loop for turn from 1 to 250 never (zerop (y turn))))            ; no year 0
    (is (= 1 (- (y 250) (y 249))))))            ; 1 year/turn in the modern era

(test research-progresses
  (let ((s (make-new-game :seed 11)))
    (apply-command s (list :found-city :unit (unit-id (a-unit s 1 :settlers)) :name "Rome"))
    (apply-command s (list :set-research :player 1 :tech :pottery))  ; the human chooses
    (dotimes (i 40) (end-turn s))
    (is (>= (hash-table-count (player-techs (player-by-id s 1))) 1))))

(test city-grows
  (let (c (s (make-new-game :seed 11)))
    (apply-command s (list :found-city :unit (unit-id (a-unit s 1 :settlers)) :name "Rome"))
    (setf c (a-city s 1))
    (dotimes (i 30) (end-turn s))
    (is (>= (city-size c) 2))))

;;; --- map wraparound (cylinder) ---------------------------------------------

(test cylinder-distance-and-step
  (let ((m (civ-model::make-game-map 20 15)))
    (is (= 19 (abs (- 0 19))))                 ; flat distance is 19...
    (is (= 1 (map-dx m 0 19)))                 ; ...but 1 around the cylinder
    (is (= 5 (map-dx m 2 17)))
    (is (= -1 (signed-dx m 0 19)))             ; the short step west wraps
    (is (= 1 (signed-dx m 19 0)))
    (is (= 3 (signed-dx m 5 8)))))             ; ordinary step unchanged

(test neighbors-wrap-east-west
  (let ((m (civ-model::make-game-map 10 10)))
    ;; the west neighbour of column 0 is column 9
    (is-true (member 9 (mapcar #'first (neighbors m 0 5))))
    ;; the east neighbour of the last column is column 0
    (is-true (member 0 (mapcar #'first (neighbors m 9 5))))
    ;; but the poles don't wrap: row 0 has no northern neighbours
    (is (= 5 (length (neighbors m 3 0))))))    ; 8 minus the 3 off the top

(test unit-moves-across-the-seam
  (let* ((s (bare-state 10 10))
         (u (add-unit s :warriors 1 0 5)))
    (apply-command s (list :move-unit :unit (unit-id u) :dx -1 :dy 0))
    (is (= 9 (unit-x u)))                       ; wrapped from 0 to 9
    (setf (unit-moves-left u) 1)
    (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))
    (is (= 0 (unit-x u)))))                      ; and back

(test goto-takes-the-short-way-round
  (let* ((s (bare-state 20 10))
         (u (add-unit s :warriors 1 1 5)))
    ;; goal at x=18 is 17 east but only 3 west around the seam
    (apply-command s (list :goto :unit (unit-id u) :x 18 :y 5))
    (dotimes (i 5) (end-turn s))
    (is (= 18 (unit-x u)))                       ; arrived
    (is (= 5 (unit-y u)))))

;;; --- pathfinding & goto -----------------------------------------------------

(test find-path-open
  (let ((s (bare-state 10 10)))
    (let ((path (find-path s 1 1 5 1 1)))
      (is-true path)
      (is (= 4 (length path)))                  ; straight line, 4 steps east
      (is (equal '(5 1) (car (last path)))))))   ; ends at the goal

(test find-path-around-wall
  ;; a vertical ocean wall at x=3 (rows 0..8) with a gap at y=9; path must detour
  (let ((s (bare-state 8 12)))
    (dotimes (y 9) (terrain! s 3 y :ocean))
    (let ((path (find-path s 1 1 5 1 1)))
      (is-true path)
      (is-false (find '(3 1) path :test #'equal))   ; never steps onto the wall
      (is (equal '(5 1) (car (last path)))))))

(test find-path-blocked
  ;; on a cylinder one wall isn't enough (you go round the seam); two full
  ;; ocean columns at x=3 and x=7 split the start (x=1) from the goal (x=6)
  (let ((s (bare-state 8 6)))
    (dotimes (y 6) (terrain! s 3 y :ocean) (terrain! s 7 y :ocean))
    (is (null (find-path s 1 1 6 1 1)))))

(test goto-moves-immediately
  ;; issuing :goto advances the unit the same turn (responsive UI), not only on end-turn
  (let* ((s (bare-state 12 6))
         (u (add-unit s :legion 1 1 3)))
    (apply-command s (list :goto :unit (unit-id u) :x 8 :y 3))
    (is (/= (unit-x u) 1))                    ; already stepped toward the target
    (is (eq :goto (unit-orders u)))))         ; and still en route

(test goto-moves-and-arrives
  (let* ((s (bare-state 12 6))
         (u (add-unit s :legion 1 1 3)))            ; legion: 1 move/turn
    (apply-command s (list :goto :unit (unit-id u) :x 8 :y 3))
    (is (eq :goto (unit-orders u)))
    (dotimes (i 12) (end-turn s))                   ; plenty of turns to walk 7 tiles
    (is (= 8 (unit-x u))) (is (= 3 (unit-y u)))
    (is (eq :idle (unit-orders u)))                 ; order cleared on arrival
    (is (null (unit-goto-x u)))))

(test goto-avoids-ocean
  (let* ((s (bare-state 10 8))
         (u (add-unit s :legion 1 1 1)))
    (dotimes (y 6) (terrain! s 4 y :ocean))         ; wall with a gap at y=6,7
    (apply-command s (list :goto :unit (unit-id u) :x 7 :y 1))
    (dotimes (i 30) (end-turn s))
    (is (= 7 (unit-x u))) (is (= 1 (unit-y u)))      ; detoured around and arrived
    (is (not (eq :ocean (tile-terrain (tile-at (gs-map s) (unit-x u) (unit-y u))))))))

(test explore-reveals-new-ground
  (let* ((s (bare-state 16 8))
         (u (add-unit s :warriors 1 1 4))
         (p (player-by-id s 1)))
    (update-visibility s)
    (let ((seen0 (hash-table-count (player-seen p))))
      (apply-command s (list :explore :unit (unit-id u)))
      (is (eq :explore (unit-orders u)))               ; set off exploring
      (dotimes (i 6) (update-visibility s) (civ-model::process-explore s))
      (update-visibility s)
      (is (> (hash-table-count (player-seen p)) seen0)))))  ; mapped more ground

(test explore-stops-on-contact
  (let* ((s (bare-state 12 8))
         (u (add-unit s :warriors 1 5 4)))
    (add-unit s :warriors 2 6 4)                          ; a foreign unit right beside us
    (update-visibility s)
    (apply-command s (list :explore :unit (unit-id u)))
    (is (eq :idle (unit-orders u)))))                     ; sighting halts exploration

(test explore-wakes-when-nothing-left
  (let* ((s (bare-state 8 8))
         (u (add-unit s :warriors 1 3 3)))
    (civ-model::reveal-map s)                             ; whole map already explored
    (apply-command s (list :explore :unit (unit-id u)))
    (is (eq :idle (unit-orders u)))))                     ; nothing to find -> idle

(test air-units-cannot-explore
  (let* ((s (bare-state 8 8))
         (u (add-unit s :fighter 1 3 3)))
    (signals command-error (apply-command s (list :explore :unit (unit-id u))))))

(test city-defended
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)))
    (apply-command s (list :found-city :unit (unit-id st) :name "C"))
    (let ((c (a-city s)))
      (is-false (city-defended-p s c))            ; freshly founded: no garrison
      (add-unit s :warriors 1 2 2)
      (is-true (city-defended-p s c))))           ; a combat unit defends it
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 3 3)))
    (apply-command s (list :found-city :unit (unit-id st) :name "C2"))
    (add-unit s :settlers 1 3 3)                  ; a settler (attack 0) is not a defender
    (is-false (city-defended-p s (a-city s)))))

;;; --- fog of war -------------------------------------------------------------

(test fog-initial-sight
  (let ((s (bare-state 12 12)))
    (add-unit s :warriors 1 5 5)
    (update-visibility s)
    (let ((p (player-by-id s 1)))
      (is-true (seen-p s p 5 5))               ; on the unit
      (is-true (seen-p s p 6 6))               ; within sight (diagonal)
      (is-true (seen-p s p 4 5))
      (is-false (seen-p s p 8 8)))))           ; out of range, unexplored

(test fog-reveals-on-move
  (let ((s (bare-state 12 12)))
    (let ((u (add-unit s :warriors 1 2 2)))
      (update-visibility s)
      (let ((p (player-by-id s 1)))
        (is-false (seen-p s p 5 2))            ; not yet seen
        ;; walk east toward it (warriors move 1/turn)
        (dotimes (i 3) (end-turn s)
          (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0)))
        (update-visibility s)
        (is-true (seen-p s p 5 2))))))         ; now explored

(test fog-clears-immediately-on-move
  ;; moving reveals the new surroundings at once -- no end-turn needed (Civ1 behavior)
  (let ((s (bare-state 12 12)))
    (let ((u (add-unit s :warriors 1 2 2)))
      (update-visibility s)
      (let ((p (player-by-id s 1)))
        (is-false (seen-p s p 4 2))            ; two tiles east: not yet seen
        (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0)) ; step to (3,2)
        (is-true (seen-p s p 4 2))             ; revealed by the move, before any end-turn
        (is-true (seen-p s p 4 3))))))

(test fog-visible-set
  (let ((s (bare-state 12 12)))
    (add-unit s :warriors 1 5 5)
    (let* ((p (player-by-id s 1)) (vis (visible-set s p)) (w (map-width (gs-map s))))
      (is-true (gethash (+ 5 (* 5 w)) vis))    ; currently visible
      (is-true (gethash (+ 6 (* 5 w)) vis))
      (is-false (gethash (+ 9 (* 9 w)) vis))))) ; far away, not visible

(test fog-seen-persists-but-not-visible
  ;; a tile explored then left behind stays seen but is no longer visible
  (let ((s (bare-state 12 12)))
    (let ((u (add-unit s :warriors 1 2 5)))
      (update-visibility s)
      (let ((p (player-by-id s 1)))
        (dotimes (i 6) (end-turn s)
          (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0)))
        (update-visibility s)
        (is-true (seen-p s p 2 5))                          ; still remembered
        (is-false (gethash (+ 2 (* 5 (map-width (gs-map s))))
                           (visible-set s p)))))))          ; but not in current sight

;;; --- tribal huts (goody huts) ----------------------------------------------

(test huts-scatter-on-land
  ;; a new game seeds some huts, never on ocean and never on a start tile
  (let ((s (make-new-game :seed 3 :width 24 :height 18 :players '("You" "Red")))
        (huts 0))
    (do-tiles (x y tile (gs-map s))
      (declare (ignore x y))
      (when (tile-hut tile)
        (incf huts)
        (is-false (eq (tile-terrain tile) :ocean))   ; huts sit on land
        (is-false (tile-city tile))))
    (is (plusp huts))
    ;; no unit starts standing on a hut
    (loop for u being the hash-values of (gs-units s)
          do (is-false (tile-hut (tile-at (gs-map s) (unit-x u) (unit-y u)))))))

(test hut-consumed-and-announced-on-entry
  ;; stepping onto a hut clears it and records an outcome message
  (let ((s (bare-state 12 12)))
    (let ((u (add-unit s :warriors 1 4 5)))
      (setf (tile-hut (tile-at (gs-map s) 4 4)) t)
      (apply-command s (list :move-unit :unit (unit-id u) :dx 0 :dy -1))
      (is-false (tile-hut (tile-at (gs-map s) 4 4)))   ; the hut is gone
      (is (stringp (gs-message s))))))                 ; and the outcome is reported

(test hut-near-a-city-yields-gold
  ;; huts beside your empire are tame: friendly scouts bearing gold, no surprises
  (let ((s (bare-state 12 12)))
    (civ-model::register-city s :name "Rome" :owner 1 :x 5 :y 5)
    (let ((u (add-unit s :warriors 1 4 5))
          (p (player-by-id s 1)))
      (setf (tile-hut (tile-at (gs-map s) 4 4)) t)
      (let ((g0 (player-gold p)))
        (apply-command s (list :move-unit :unit (unit-id u) :dx 0 :dy -1))
        (is (> (player-gold p) g0))))))                ; gold gained

(test barbarian-unit-ignores-huts
  ;; a barbarian walking onto a hut does not trigger it (no free loot for raiders)
  (let ((s (bare-state 12 12)))
    (setf (gs-players s) (vector (make-player :id 1 :name "P1" :kind :human)
                                 (make-player :id 2 :name "Barb" :kind :barbarian)))
    (let ((u (add-unit s :legion 2 4 5)))
      (setf (tile-hut (tile-at (gs-map s) 4 4)) t)
      (apply-command s (list :move-unit :unit (unit-id u) :dx 0 :dy -1))
      (is-true (tile-hut (tile-at (gs-map s) 4 4)))    ; still there
      (is-false (gs-message s)))))

(test save-load-preserves-huts
  (let ((s (bare-state 8 8)))
    (setf (tile-hut (tile-at (gs-map s) 3 3)) t)
    (let ((s2 (load-game-form (dump-game s))))
      (is-true (tile-hut (tile-at (gs-map s2) 3 3)))
      (is-false (tile-hut (tile-at (gs-map s2) 0 0))))))

;;; --- disbanding units ------------------------------------------------------

(test disband-in-field-removes-unit
  ;; a unit disbanded in the open field is simply gone -- no shields recovered
  (let ((s (bare-state 8 8)))
    (let ((u (add-unit s :warriors 1 5 5)))
      (apply-command s (list :disband-unit :unit (unit-id u)))
      (is-false (unit-by-id s (unit-id u)))            ; removed from the game
      (is (stringp (gs-message s))))))                 ; outcome reported

(test disband-in-city-recovers-a-fraction-of-shields
  ;; disbanding inside a friendly city banks build-cost / *disband-shield-divisor*
  (let ((s (bare-state 8 8)))
    (let* ((c (civ-model::register-city s :name "Rome" :owner 1 :x 3 :y 3))
           (u (add-unit s :warriors 1 3 3))
           (expected (floor (unit-def :warriors :cost 0)
                            civ-model::*disband-shield-divisor*))
           (box0 (city-shield-box c)))
      (is (plusp expected))
      (apply-command s (list :disband-unit :unit (unit-id u)))
      (is-false (unit-by-id s (unit-id u)))
      (is (= (+ box0 expected) (city-shield-box c))))))  ; fraction of cost recovered

(test disband-recovery-capped-at-current-cost
  ;; recovered shields can finish the current build but never overflow past it
  (let ((s (bare-state 8 8)))
    (let* ((c (civ-model::register-city s :name "Rome" :owner 1 :x 3 :y 3))
           (cost (unit-def :warriors :cost 0)))
      (setf (city-production c) (list :unit :warriors)   ; cost = warriors cost
            (city-shield-box c) (1- cost))               ; needs just 1 more
      (let ((u (add-unit s :phalanx 1 3 3)))             ; phalanx half-cost > 1
        (apply-command s (list :disband-unit :unit (unit-id u)))
        (is (= cost (city-shield-box c)))))))            ; capped exactly at cost

(test disband-into-wonder-recovers-nothing
  ;; wonders can only be hurried by caravans, never by recycling units
  (let ((s (bare-state 8 8)))
    (let* ((c (civ-model::register-city s :name "Rome" :owner 1 :x 3 :y 3))
           (box0 (city-shield-box c))
           (u (add-unit s :warriors 1 3 3)))
      (setf (city-production c) (list :wonder :pyramids))
      (apply-command s (list :disband-unit :unit (unit-id u)))
      (is-false (unit-by-id s (unit-id u)))            ; unit still removed
      (is (= box0 (city-shield-box c))))))             ; but no shields toward the wonder

(test disband-in-enemy-city-recovers-nothing
  ;; standing in a city you don't own returns no shields to it
  (let ((s (bare-state 8 8)))
    (let* ((c (civ-model::register-city s :name "Theirs" :owner 2 :x 3 :y 3))
           (box0 (city-shield-box c))
           (u (add-unit s :warriors 1 3 3)))            ; player 1 unit on player 2's city
      (apply-command s (list :disband-unit :unit (unit-id u)))
      (is-false (unit-by-id s (unit-id u)))
      (is (= box0 (city-shield-box c))))))               ; their city gains nothing

;;; --- unit obsolescence -----------------------------------------------------

(test unit-becomes-obsolete-with-its-successor-tech
  (let ((s (bare-state 6 6)))
    (let ((p (player-by-id s 1)))
      (is-false (unit-obsolete-p p :warriors))           ; fine to start
      (setf (gethash :gunpowder (player-techs p)) t)     ; muskets supersede warriors
      (is-true (unit-obsolete-p p :warriors))
      (is-false (unit-obsolete-p p :musketeers)))))      ; the successor is not obsolete

(test cannot-build-an-obsolete-unit
  (let ((s (bare-state 6 6)))
    (let ((c (civ-model::register-city s :name "Rome" :owner 1 :x 3 :y 3))
          (p (player-by-id s 1)))
      (apply-command s (list :set-production :city (city-id c) :item '(:unit :warriors))) ; ok now
      (is (equal '(:unit :warriors) (city-production c)))
      (setf (gethash :gunpowder (player-techs p)) t)
      (signals command-error
        (apply-command s (list :set-production :city (city-id c) :item '(:unit :warriors)))))))

(test upgrade-obsolete-unit-in-a-city
  (let ((s (bare-state 6 6)))
    (let ((c (civ-model::register-city s :name "Rome" :owner 1 :x 3 :y 3))
          (p (player-by-id s 1)))
      (declare (ignore c))
      (setf (gethash :gunpowder (player-techs p)) t       ; warriors now obsolete
            (player-gold p) 999)
      (let* ((u (add-unit s :warriors 1 3 3))
             (cost (upgrade-cost :warriors :musketeers))
             (g0 (player-gold p)))
        (apply-command s (list :upgrade-unit :unit (unit-id u)))
        (is (eq :musketeers (unit-type u)))               ; became the successor
        (is (= (- g0 cost) (player-gold p)))))))          ; paid for it

(test upgrade-needs-a-city-obsolescence-and-gold
  (let ((s (bare-state 6 6)))
    (let ((p (player-by-id s 1)))
      ;; not obsolete yet -> refused
      (let ((u (add-unit s :warriors 1 1 1)))
        (signals command-error (apply-command s (list :upgrade-unit :unit (unit-id u)))))
      (setf (gethash :gunpowder (player-techs p)) t)
      ;; obsolete but out in the field (no city) -> refused
      (let ((u (add-unit s :warriors 1 2 2)))
        (signals command-error (apply-command s (list :upgrade-unit :unit (unit-id u)))))
      ;; obsolete, in a city, but broke -> refused
      (civ-model::register-city s :name "Rome" :owner 1 :x 4 :y 4)
      (setf (player-gold p) 0)
      (let ((u (add-unit s :warriors 1 4 4)))
        (signals command-error (apply-command s (list :upgrade-unit :unit (unit-id u))))))))

;;; --- naval transport -------------------------------------------------------

(test land-unit-boards-an-adjacent-transport
  (let ((s (bare-state 8 8)))
    (terrain! s 4 4 :ocean)                         ; a sea tile
    (add-unit s :transport 1 4 4)                   ; transport waiting on it
    (let ((w (add-unit s :warriors 1 3 4)))         ; warrior on the shore beside it
      (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))
      (is (= 4 (unit-x w)))                          ; boarded: now on the sea tile
      (is (= 4 (unit-y w)))
      (is (member (unit-id w) (tile-units (tile-at (gs-map s) 4 4)))))))

(test land-unit-cannot-swim-without-a-transport
  (let ((s (bare-state 8 8)))
    (terrain! s 4 4 :ocean)
    (let ((w (add-unit s :warriors 1 3 4)))
      (signals command-error
        (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))))))

(test transport-capacity-is-enforced
  ;; a trireme carries 2; a third land unit can't board
  (let ((s (bare-state 8 8)))
    (terrain! s 4 4 :ocean)
    (add-unit s :trireme 1 4 4)                     ; capacity 2
    (let ((a (add-unit s :warriors 1 3 4))
          (b (add-unit s :warriors 1 5 4))
          (c (add-unit s :warriors 1 4 3)))
      (apply-command s (list :move-unit :unit (unit-id a) :dx 1 :dy 0))   ; (3,4)->(4,4)
      (apply-command s (list :move-unit :unit (unit-id b) :dx -1 :dy 0))  ; (5,4)->(4,4)
      (signals command-error                                              ; full
        (apply-command s (list :move-unit :unit (unit-id c) :dx 0 :dy 1)))))) ; (4,3)->(4,4)

(test a-transport-carries-its-cargo
  (let ((s (bare-state 8 8)))
    (terrain! s 4 4 :ocean) (terrain! s 5 4 :ocean)  ; a short sea lane
    (let ((tr (add-unit s :transport 1 4 4))
          (w (add-unit s :warriors 1 3 4)))
      (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))   ; board at (4,4)
      (apply-command s (list :move-unit :unit (unit-id tr) :dx 1 :dy 0))  ; sail east
      (is (= 5 (unit-x w)))                          ; cargo came along
      (is (= 4 (unit-y w)))
      (is (member (unit-id w) (tile-units (tile-at (gs-map s) 5 4))))
      (is-false (member (unit-id w) (tile-units (tile-at (gs-map s) 4 4)))))))

(test cargo-unloads-onto-land
  (let ((s (bare-state 8 8)))
    (terrain! s 4 4 :ocean)
    (add-unit s :transport 1 4 4)
    (let ((w (add-unit s :warriors 1 3 4)))
      (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))   ; board (4,4)
      (setf (unit-moves-left w) 1)                                        ; fresh moves
      (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))   ; step ashore (5,4)
      (is (= 5 (unit-x w)))
      (is (eq :grassland (tile-terrain (tile-at (gs-map s) 5 4)))))))

(test sinking-a-transport-drowns-its-cargo
  (let ((s (bare-state 8 8)))
    (terrain! s 4 4 :ocean)
    (let ((tr (add-unit s :transport 1 4 4))
          (w (add-unit s :warriors 1 3 4)))
      (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))   ; board
      (civ-model::destroy-unit s tr)                                      ; the ship is sunk
      (is-false (unit-by-id s (unit-id w))))))                            ; cargo lost with it

;;; --- AI sea invasion -------------------------------------------------------

(test transport-launching-from-a-city-leaves-the-garrison
  ;; a ship built in a coastal city must not drag the garrison out to sea
  (let ((s (bare-state 8 8)))
    (terrain! s 4 3 :ocean)                         ; water north of the city
    (civ-model::register-city s :name "Port" :owner 1 :x 4 :y 4)
    (let ((tr (add-unit s :transport 1 4 4))        ; transport sitting in the city
          (g (add-unit s :warriors 1 4 4)))         ; the garrison
      (apply-command s (list :move-unit :unit (unit-id tr) :dx 0 :dy -1))  ; sail north
      (is (and (= 4 (unit-x g)) (= 4 (unit-y g))))   ; garrison stayed put
      (is (= 3 (unit-y tr))))))                       ; ship left

(test ai-builds-a-transport-to-invade
  (let ((s (bare-state 10 10)))
    (terrain! s 4 4 :ocean)                          ; sea beside the coastal city
    (let ((p2 (player-by-id s 2)))
      (setf (gethash :industrialization (player-techs p2)) t
            (relation s 1 2) :war)
      (civ-model::register-city s :name "Foe" :owner 1 :x 8 :y 8)   ; invasion target
      (civ-model::register-city s :name "A" :owner 2 :x 1 :y 1)     ; >=3 cities so it
      (civ-model::register-city s :name "B" :owner 2 :x 2 :y 2)     ;   isn't still settling
      (let ((coastal (civ-model::register-city s :name "C" :owner 2 :x 3 :y 4)))
        (add-unit s :phalanx 2 3 4)             ; already garrisoned, so it can look outward
        (civ-model::ai-city-production s p2 coastal)
        (is (equal '(:unit :transport) (city-production coastal)))))))

(test ai-land-unit-boards-a-friendly-transport
  (let ((s (bare-state 10 10)))
    (terrain! s 4 4 :ocean)
    (setf (relation s 1 2) :war)
    (civ-model::register-city s :name "Foe" :owner 1 :x 8 :y 8)     ; gives a target
    (add-unit s :transport 2 4 4)                                   ; AI ship on the water
    (let ((sol (add-unit s :legion 2 3 4)))                         ; AI legion on the shore
      (civ-model::ai-try-board s sol)
      (is (and (= 4 (unit-x sol)) (= 4 (unit-y sol)))))))           ; boarded

(test ai-transport-sails-toward-the-enemy
  (let ((s (bare-state 14 8)))
    (loop for x from 1 to 9 do (terrain! s x 4 :ocean))             ; a sea lane
    (setf (relation s 1 2) :war)
    (let ((foe (civ-model::register-city s :name "Foe" :owner 1 :x 10 :y 4))
          (tr (add-unit s :transport 2 5 4)))
      (add-unit s :legion 2 5 4)                                    ; cargo aboard
      (flet ((dist () (+ (map-dx (gs-map s) (city-x foe) (unit-x tr))
                         (abs (- (city-y foe) (unit-y tr))))))
        (let ((d0 (dist)))
          (civ-model::ai-transport s tr)
          (is (< (dist) d0)))))))                                   ; closed on the enemy

(test ai-transport-puts-troops-ashore
  (let ((s (bare-state 10 10)))
    (terrain! s 4 5 :ocean)                                         ; the ship's tile
    (setf (relation s 1 2) :war)
    (civ-model::register-city s :name "Foe" :owner 1 :x 6 :y 5)     ; enemy just inland
    (let ((tr (add-unit s :transport 2 4 5))
          (leg (add-unit s :legion 2 4 5)))                         ; aboard
      (civ-model::ai-transport s tr)
      (is (not (and (= (unit-x leg) 4) (= (unit-y leg) 5))))        ; left the ship
      (is (eq :grassland (tile-terrain (tile-at (gs-map s)          ; onto dry land
                                                (unit-x leg) (unit-y leg))))))))

(test ai-mounts-a-sea-invasion
  ;; end to end: across a strait, the AI boards a unit, ferries it, and lands it
  (let ((s (bare-state 12 6)))
    (loop for x from 3 to 6 do                                      ; ocean columns 3..6
      (dotimes (y 6) (terrain! s x y :ocean)))
    (setf (relation s 1 2) :war)
    (civ-model::register-city s :name "Foe" :owner 1 :x 8 :y 3)     ; enemy continent
    (add-unit s :warriors 1 8 3)                                    ; a defender there
    (add-unit s :transport 2 3 3)                                   ; AI ship, launched
    (add-unit s :legion 2 2 2)                                      ; AI invader on home soil
    (let ((landed nil))
      (dotimes (i 15)
        (end-turn s)
        (when (loop for u being the hash-values of (gs-units s)
                    thereis (and (= (unit-owner u) 2)
                                 (eq (unit-def (unit-type u) :domain) :land)
                                 (>= (unit-x u) 7)))                ; reached the far shore
          (setf landed t) (return)))
      (is-true landed))))

;;; --- city capture ----------------------------------------------------------

(test capture-an-undefended-enemy-city
  (let ((s (bare-state 8 8)))
    (setf (relation s 1 2) :war)
    (let ((c (civ-model::register-city s :name "Theirs" :owner 2 :x 4 :y 4)))
      (setf (city-size c) 3)
      (let ((w (add-unit s :warriors 1 3 4)))
        (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))
        (is (= 1 (city-owner c)))                ; the city flipped to us
        (is (= 2 (city-size c)))                  ; shrunk by one
        (is (= 4 (unit-x w)))))))                 ; our unit now garrisons it

(test razing-a-size-one-city
  (let ((s (bare-state 8 8)))
    (setf (relation s 1 2) :war)
    (let ((c (civ-model::register-city s :name "Hamlet" :owner 2 :x 4 :y 4)))  ; size 1
      (let ((w (add-unit s :warriors 1 3 4)))
        (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))
        (is-false (city-by-id s (city-id c)))                       ; razed away
        (is-false (tile-city (tile-at (gs-map s) 4 4)))             ; tile cleared
        (is (= 4 (unit-x w)))                                        ; unit stands on the ruins
        (is (search "razed" (or (first (gs-log s)) "")))))))         ; cause recorded

(test capture-is-recorded-in-the-log
  (let ((s (bare-state 8 8)))
    (setf (relation s 1 2) :war)
    (let ((c (civ-model::register-city s :name "Big" :owner 2 :x 4 :y 4)))
      (setf (city-size c) 4)
      (apply-command s (list :move-unit :unit (unit-id (add-unit s :warriors 1 3 4)) :dx 1 :dy 0))
      (is (search "captured" (or (first (gs-log s)) ""))))))

(test cannot-enter-a-peaceful-citys-tile
  (let ((s (bare-state 8 8)))                     ; relation defaults to peace
    (civ-model::register-city s :name "Theirs" :owner 2 :x 4 :y 4)
    (let ((w (add-unit s :warriors 1 3 4)))
      (signals command-error
        (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))))))

(test capturing-the-last-city-wins-the-game
  (let ((s (bare-state 8 8)))
    (setf (relation s 1 2) :war)
    (civ-model::register-city s :name "Last" :owner 2 :x 4 :y 4)   ; player 2's only foothold
    (let ((w (add-unit s :warriors 1 3 4)))                         ; player 1 stays alive via it
      (apply-command s (list :move-unit :unit (unit-id w) :dx 1 :dy 0))   ; raze the last city
      (civ-model::process-victory s)
      (is (eql 1 (gs-winner s)))
      (is (eq :conquest (gs-victory s))))))

(test veteran-promotion
  (let ((s (bare-state 6 6)))
    (let ((u (add-unit s :legion 1 2 2))
          (civ-model::*veteran-promotion-chance* 100))
      (civ-model::maybe-promote s u)
      (is-true (unit-veteran u)))                  ; a sure promotion
    (let ((v (add-unit s :phalanx 1 3 3))
          (civ-model::*veteran-promotion-chance* 0))
      (civ-model::maybe-promote s v)
      (is-false (unit-veteran v)))))               ; no chance -> no stripes

(test capture-loots-gold
  (let ((s (bare-state 8 8)))
    (let ((c (civ-model::register-city s :name "Ur" :owner 2 :x 4 :y 4)))
      (setf (city-size c) 4
            (player-gold (player-by-id s 2)) 200
            (player-gold (player-by-id s 1)) 0)
      (civ-model::capture-city s c 1)              ; player 1 sacks player 2's city
      (is (plusp (player-gold (player-by-id s 1))))     ; captor looted gold
      (is (< (player-gold (player-by-id s 2)) 200)))))  ; from the loser's treasury

(test civil-war-on-capital-capture
  (let* ((s (bare-state 14 14))
         (cap (civ-model::register-city s :name "Capital" :owner 2 :x 2 :y 2)))
    (setf (city-size cap) 3)
    (push :palace (city-buildings cap))             ; the capital holds the Palace
    (dolist (xy '((5 5) (8 8) (11 11) (13 2)))      ; four more cities for player 2
      (setf (city-size (civ-model::register-city s :name (format nil "C~D~D" (first xy) (second xy))
                                                 :owner 2 :x (first xy) :y (second xy)))
            2))
    (let ((n (length (gs-players s))))
      (civ-model::capture-city s cap 1)             ; player 1 takes the capital
      (is (= (1+ n) (length (gs-players s))))        ; a rebel civ split off
      (let ((rebel (find-if (lambda (p) (search "Rebels" (player-name p))) (gs-players s))))
        (is-true rebel)
        (is (plusp (loop for c being the hash-values of (gs-cities s)
                         count (= (city-owner c) (player-id rebel)))))   ; holding cities
        (is (civ-model::at-war-p s (player-id rebel) 2))))               ; at odds with the old regime
    ;; player 2's surviving empire crowns a new capital (Palace relocated)
    (is-true (loop for c being the hash-values of (gs-cities s)
                   thereis (and (= (city-owner c) 2) (member :palace (city-buildings c)))))))

(test ai-captures-an-adjacent-undefended-city
  (let ((s (bare-state 8 8)))
    (setf (relation s 1 2) :war)
    (let ((c (civ-model::register-city s :name "Theirs" :owner 1 :x 4 :y 4)))
      (setf (city-size c) 3)
      (let ((raider (add-unit s :legion 2 3 4)))   ; an AI (player 2) legion next door
        (civ-model::ai-military s raider)
        (is (= 2 (city-owner c)))))))              ; the AI marched in and took it

;;; --- civil disorder, random events, Manhattan Project, SDI -----------------

(test prolonged-disorder-causes-riots
  (let ((s (bare-state 8 8)))
    (let ((c (civ-model::register-city s :name "Unrest" :owner 1 :x 3 :y 3)))
      (setf (city-size c) 7)                          ; big despotic city, no temple
      (is-true (city-disorder-p s c))                 ; so it's in disorder
      (let ((sz (city-size c)))
        (dotimes (i 3) (civ-model::process-city s c)) ; three turns of unrest
        (is (< (city-size c) sz))))))                  ; boils over into riots

(test event-plague-shrinks-a-crowded-city
  (let ((s (bare-state 8 8)))
    (let ((c (civ-model::register-city s :name "Big" :owner 1 :x 3 :y 3)))
      (setf (city-size c) 5)
      (is-true (civ-model::event-plague s))
      (is (= 4 (city-size c))))))

(test event-fire-destroys-a-building
  (let ((s (bare-state 8 8)))
    (let ((c (civ-model::register-city s :name "Town" :owner 1 :x 3 :y 3)))
      (setf (city-buildings c) (list :barracks :library))
      (is-true (civ-model::event-fire s))
      (is (= 1 (length (city-buildings c)))))))

(test event-rich-vein-adds-shields
  (let ((s (bare-state 8 8)))
    (let* ((c (civ-model::register-city s :name "Mine" :owner 1 :x 3 :y 3))
           (b (city-shield-box c)))
      (is-true (civ-model::event-rich-vein s))
      (is (> (city-shield-box c) b)))))

(test nukes-need-the-manhattan-project
  (let ((s (bare-state 8 8)))
    (let ((c (civ-model::register-city s :name "Lab" :owner 1 :x 3 :y 3))
          (p (player-by-id s 1)))
      (setf (gethash :rocketry (player-techs p)) t)
      (signals command-error                           ; not yet
        (apply-command s (list :set-production :city (city-id c) :item '(:unit :nuclear))))
      (push :manhattan-project (city-buildings c))     ; now the project exists
      (apply-command s (list :set-production :city (city-id c) :item '(:unit :nuclear)))
      (is (equal '(:unit :nuclear) (city-production c))))))

(test sdi-shoots-down-a-nuke
  (let ((s (bare-state 10 10)))
    (setf (relation s 1 2) :war)
    (let ((c (civ-model::register-city s :name "Shield" :owner 2 :x 6 :y 5)))
      (setf (city-size c) 8)
      (push :sdi-defense (city-buildings c))           ; defended by SDI
      (let ((nuke (add-unit s :nuclear 1 5 5)))        ; adjacent
        (apply-command s (list :nuke :unit (unit-id nuke)))
        (is-false (unit-by-id s (unit-id nuke)))        ; missile destroyed
        (is (= 8 (city-size c)))))))                     ; city unharmed

;;; --- nuclear weapons -------------------------------------------------------

(test nuke-flattens-the-blast-area
  (let ((s (bare-state 10 10)))
    (setf (relation s 1 2) :war)
    (let ((nuke (add-unit s :nuclear 1 5 5))
          (a (add-unit s :warriors 2 4 5))    ; adjacent
          (b (add-unit s :phalanx 2 6 6))     ; diagonal -- still in the 3x3
          (far (add-unit s :legion 2 8 8)))   ; well outside
      (apply-command s (list :nuke :unit (unit-id nuke)))
      (is-false (unit-by-id s (unit-id nuke)))   ; missile expended
      (is-false (unit-by-id s (unit-id a)))
      (is-false (unit-by-id s (unit-id b)))
      (is-true (unit-by-id s (unit-id far))))))  ; survives

(test nuke-devastates-a-city
  (let ((s (bare-state 10 10)))
    (setf (relation s 1 2) :war)
    (let ((c (civ-model::register-city s :name "Foe" :owner 2 :x 6 :y 5)))
      (setf (city-size c) 8)
      (apply-command s (list :nuke :unit (unit-id (add-unit s :nuclear 1 5 5))))
      (is (= 4 (city-size c))))))                ; halved, not razed

(test nuke-leaves-fallout
  (let ((s (bare-state 10 10)))
    (apply-command s (list :nuke :unit (unit-id (add-unit s :nuclear 1 5 5))))
    (is-true (tile-pollution (tile-at (gs-map s) 5 5)))     ; ground zero
    (is-true (tile-pollution (tile-at (gs-map s) 4 4)))))   ; and the rim

(test nuking-a-civ-is-an-act-of-war
  (let ((s (bare-state 10 10)))                  ; starts at peace
    (add-unit s :warriors 2 4 5)                 ; an enemy in the blast
    (apply-command s (list :nuke :unit (unit-id (add-unit s :nuclear 1 5 5))))
    (is-true (at-war-p s 1 2))))

(test only-a-nuclear-weapon-can-detonate
  (let ((s (bare-state 8 8)))
    (let ((u (add-unit s :warriors 1 4 4)))
      (signals command-error (apply-command s (list :nuke :unit (unit-id u)))))))

;;; --- air units: fuel, airbases, carriers -----------------------------------

(test air-unit-launches-with-a-full-tank
  (let ((s (bare-state 8 8)))
    (let ((f (add-unit s :fighter 1 4 4)))
      (is (= (unit-def :fighter :range) (unit-fuel f))))))

(test air-unit-refuels-over-a-city
  (let ((s (bare-state 8 8)))
    (civ-model::register-city s :name "Base" :owner 1 :x 4 :y 4)
    (let ((f (add-unit s :fighter 1 4 4)))
      (setf (unit-fuel f) 0)                 ; nearly out of fuel, but sitting in a city
      (civ-model::process-fuel s)
      (is-true (unit-by-id s (unit-id f)))   ; didn't crash
      (is (= (unit-def :fighter :range) (unit-fuel f))))))  ; topped back up

(test air-unit-burns-fuel-and-crashes-when-dry
  (let ((s (bare-state 8 8)))
    (let ((f (add-unit s :fighter 1 4 4)))   ; out in the open field
      (setf (unit-fuel f) 1)
      (civ-model::process-fuel s)            ; burns a turn -> 0
      (is (= 0 (unit-fuel f)))
      (is-true (unit-by-id s (unit-id f)))
      (civ-model::process-fuel s)            ; still airborne, dry -> crash
      (is-false (unit-by-id s (unit-id f))))))

(test airbase-refuels-air-units
  (let ((s (bare-state 8 8)))
    (setf (tile-airbase (tile-at (gs-map s) 4 4)) t)
    (let ((f (add-unit s :fighter 1 4 4)))
      (setf (unit-fuel f) 0)
      (civ-model::process-fuel s)
      (is-true (unit-by-id s (unit-id f)))
      (is (= (unit-def :fighter :range) (unit-fuel f))))))

(test settler-can-build-an-airbase
  (let ((s (bare-state 8 8)))
    (let ((u (add-unit s :settlers 1 3 3))
          (p (player-by-id s 1)))
      (setf (gethash :flight (player-techs p)) t)        ; airbases need Flight
      (apply-command s (list :build-airbase :unit (unit-id u)))
      (dotimes (i (civ-model::terraform-def :build-airbase :turns))
        (civ-model::process-terraform s))
      (is-true (tile-airbase (tile-at (gs-map s) 3 3))))))

(test carrier-refuels-and-ferries-its-planes
  (let ((s (bare-state 8 8)))
    (terrain! s 4 4 :ocean) (terrain! s 5 4 :ocean)      ; a sea lane
    (let ((carrier (add-unit s :carrier 1 4 4))
          (f (add-unit s :fighter 1 4 4)))               ; a fighter parked on the carrier
      (setf (unit-fuel f) 0)
      (civ-model::process-fuel s)                        ; the carrier refuels it
      (is (= (unit-def :fighter :range) (unit-fuel f)))
      (apply-command s (list :move-unit :unit (unit-id carrier) :dx 1 :dy 0))
      (is (= 5 (unit-x f))) (is (= 4 (unit-y f))))))     ; the plane sailed along

;;; --- smarter AI ------------------------------------------------------------

(test ai-uses-its-full-movement
  ;; a fast unit should spend its whole movement allowance in one turn
  (let ((s (bare-state 12 12)))
    (let ((u (add-unit s :cavalry 2 6 6)))          ; cavalry: 2 moves, AI-owned
      (is (= 2 (unit-moves-left u)))
      (civ-model::ai-unit-turn s u)
      (is (= 0 (unit-moves-left u))))))             ; both moves used, not just one

(test ai-rebuilds-a-defender-for-an-undefended-city
  (let ((s (bare-state 8 8)))
    (let ((c (civ-model::register-city s :name "Open" :owner 2 :x 4 :y 4))
          (p2 (player-by-id s 2)))
      (civ-model::ai-city-production s p2 c)         ; nothing garrisons it
      (is (eq :unit (first (city-production c))))
      (is (member (second (city-production c)) '(:warriors :phalanx :musketeers :riflemen :mech-inf))))))

(test ai-sues-for-peace-when-clearly-losing
  (let ((s (bare-state 8 8)))
    (setf (relation s 1 2) :war)
    (civ-model::register-city s :name "Mine" :owner 1 :x 1 :y 1)          ; player 1: 1 city
    (dotimes (i 4) (civ-model::register-city s :name "T" :owner 2 :x (+ 3 i) :y 4)) ; player 2: 4
    (civ-model::ai-diplomacy s (player-by-id s 1))   ; the weak side reconsiders
    (is (not (at-war-p s 1 2)))))                     ; made peace

(test war-or-alliance-implies-contact
  ;; civs are strangers by default, but an established relation presupposes contact
  (let ((s (bare-state 8 8)))
    (is-false (met-p s 1 2))
    (setf (relation s 1 2) :war)
    (is-true (met-p s 1 2))))

(test units-make-contact-on-sight
  (let* ((s (bare-state 12 8))
         (far (add-unit s :warriors 2 9 5)))
    (add-unit s :warriors 1 2 2)
    (civ-model::detect-contacts s)
    (is-false (met-p s 1 2))                          ; out of sight: strangers
    ;; move player 2's unit adjacent to player 1's
    (setf (tile-units (tile-at (gs-map s) 9 5))
          (remove (unit-id far) (tile-units (tile-at (gs-map s) 9 5))))
    (setf (unit-x far) 3 (unit-y far) 2)
    (push (unit-id far) (tile-units (tile-at (gs-map s) 3 2)))
    (civ-model::detect-contacts s)
    (is-true (met-p s 1 2))))                          ; within sight: met

(test ai-leaves-unmet-civs-alone
  ;; the AI must not pounce on (or otherwise contact) a civ it has never met
  (let ((s (bare-state 8 8)))
    (dotimes (i 3) (civ-model::register-city s :name "M" :owner 1 :x (+ 1 i) :y 1)) ; strong
    (civ-model::register-city s :name "T" :owner 2 :x 6 :y 5)
    (dotimes (i 50) (civ-model::ai-diplomacy s (player-by-id s 1)))
    (is-false (met-p s 1 2))
    (is (not (at-war-p s 1 2)))))                      ; left alone until contact

(test ai-wont-start-a-war-it-would-lose
  (let ((s (bare-state 8 8)))                         ; relation defaults to peace
    (civ-model::register-city s :name "Mine" :owner 1 :x 1 :y 1)          ; player 1: 1 city
    (dotimes (i 4) (civ-model::register-city s :name "T" :owner 2 :x (+ 3 i) :y 4)) ; player 2: 4
    (dotimes (i 50) (civ-model::ai-diplomacy s (player-by-id s 1)))       ; many chances
    (is (not (at-war-p s 1 2)))))                     ; never picks the losing fight

(test ai-marches-on-the-enemy-in-wartime
  (let ((s (bare-state 16 8)))
    (setf (relation s 1 2) :war)
    (let ((foe (civ-model::register-city s :name "Foe" :owner 1 :x 12 :y 4))
          (raider (add-unit s :legion 2 3 4)))        ; an AI legion in the field
      (flet ((dist () (+ (map-dx (gs-map s) (city-x foe) (unit-x raider))
                         (abs (- (city-y foe) (unit-y raider))))))
        (let ((d0 (dist)))
          (civ-model::ai-military s raider)
          (is (< (dist) d0)))))))                      ; advanced toward the enemy city

;;; --- AI uses its full toolbox ----------------------------------------------

(test ai-aircraft-returns-to-base
  (let ((s (bare-state 14 8)))
    (civ-model::register-city s :name "Base" :owner 2 :x 2 :y 4)
    (let ((f (add-unit s :fighter 2 9 4)))         ; out in the field
      (flet ((d () (+ (map-dx (gs-map s) 2 (unit-x f)) (abs (- 4 (unit-y f))))))
        (let ((d0 (d)))
          (civ-model::ai-air s f)
          (is (< (d) d0)))))))                       ; flew toward base to refuel

(test ai-diplomat-spies-on-an-adjacent-city
  (let ((s (bare-state 10 10)))
    (setf (relation s 1 2) :war)
    (civ-model::register-city s :name "Foe" :owner 1 :x 5 :y 5)
    (let ((dip (add-unit s :diplomat 2 4 5)))
      (civ-model::ai-diplomat s dip)
      (is-true (has-embassy-p s 2 1)))))             ; established an embassy

(test ai-nuke-detonates-on-an-adjacent-enemy
  (let ((s (bare-state 10 10)))
    (setf (relation s 1 2) :war)
    (let ((c (civ-model::register-city s :name "Foe" :owner 1 :x 5 :y 5)))
      (setf (city-size c) 8)
      (let ((nuke (add-unit s :nuclear 2 4 5)))
        (civ-model::ai-nuke s nuke)
        (is-false (unit-by-id s (unit-id nuke)))     ; it went off
        (is (< (city-size c) 8))))))                  ; and devastated the city

(test ai-caravan-opens-a-trade-route
  (let ((s (bare-state 10 10)))
    (civ-model::register-city s :name "A" :owner 2 :x 3 :y 3)
    (civ-model::register-city s :name "B" :owner 2 :x 7 :y 3)
    (let ((car (add-unit s :caravan 2 3 3)))         ; a caravan in city A
      (civ-model::ai-caravan s car)
      (is (plusp (length (gs-routes s)))))))          ; opened a trade route

;;; --- terrain transformation (clearing) -------------------------------------

(defun clear-to-completion (s u)
  (apply-command s (list :clear-forest :unit (unit-id u)))
  (dotimes (i (civ-model::terraform-def :clear-forest :turns))
    (civ-model::process-terraform s)))

(test clearing-forest-yields-plains
  (let ((s (bare-state 6 6)))
    (terrain! s 3 3 :forest)
    (clear-to-completion s (add-unit s :settlers 1 3 3))
    (is (eq :plains (tile-terrain (tile-at (gs-map s) 3 3))))))

(test clearing-jungle-yields-grassland
  (let ((s (bare-state 6 6)))
    (terrain! s 3 3 :jungle)
    (clear-to-completion s (add-unit s :settlers 1 3 3))
    (is (eq :grassland (tile-terrain (tile-at (gs-map s) 3 3))))))

(test clearing-swamp-yields-grassland
  (let ((s (bare-state 6 6)))
    (terrain! s 3 3 :swamp)
    (clear-to-completion s (add-unit s :settlers 1 3 3))
    (is (eq :grassland (tile-terrain (tile-at (gs-map s) 3 3))))))

(test cannot-clear-open-grassland
  (let ((s (bare-state 6 6)))                       ; bare-state is grassland
    (let ((u (add-unit s :settlers 1 3 3)))
      (signals command-error
        (apply-command s (list :clear-forest :unit (unit-id u)))))))

(test roads-can-be-built-on-the-new-terrains
  (let ((s (bare-state 6 6)))
    (terrain! s 3 3 :tundra)
    (let ((u (add-unit s :settlers 1 3 3)))
      (apply-command s (list :build-road :unit (unit-id u)))   ; must not signal
      (dotimes (i (civ-model::terraform-def :build-road :turns))
        (civ-model::process-terraform s))
      (is-true (tile-road (tile-at (gs-map s) 3 3))))))

;;; --- continental map generation --------------------------------------------

(test the-four-new-terrains-have-yields-and-specials
  (dolist (terr '(:tundra :arctic :swamp :jungle))
    (is (integerp (terrain-def terr :food)))      ; in the *terrain* table now
    (is (integerp (terrain-def terr :move 1)))
    (is (assoc terr civ-model::*special-bonus*)))) ; and has a special bonus

(test new-game-has-both-land-and-sea
  (let ((s (make-new-game :seed 7 :width 40 :height 26 :players '("A" "B" "C")))
        (land 0) (sea 0))
    (do-tiles (x y tile (gs-map s))
      (declare (ignore x y))
      (if (eq (tile-terrain tile) :ocean) (incf sea) (incf land)))
    (is (plusp land))
    (is (plusp sea))
    (is (< 1/10 (/ land (+ land sea)) 3/5))))      ; a sane continental land fraction

(test climate-keeps-ice-off-the-equator
  (let ((s (make-new-game :seed 3 :width 40 :height 26 :players '("A" "B"))))
    (let* ((h (map-height (gs-map s)))
           (mid (/ (1- h) 2.0)) (bad 0))
      (do-tiles (x y tile (gs-map s))
        (declare (ignore x))
        (when (and (member (tile-terrain tile) '(:arctic :tundra))
                   (< (abs (- y mid)) (* 0.45 mid)))   ; firmly the equatorial half
          (incf bad)))
      (is (zerop bad)))))                              ; ice/tundra only toward the poles

(test every-civ-starts-on-land
  (let ((s (make-new-game :seed 5 :width 40 :height 26 :players '("A" "B" "C"))))
    (is (plusp (hash-table-count (gs-units s))))
    (loop for u being the hash-values of (gs-units s)
          do (is-false (eq (tile-terrain (tile-at (gs-map s) (unit-x u) (unit-y u)))
                           :ocean)))))

(test ai-expands
  ;; the AI should found and expand to several cities on its own
  (let ((s (make-new-game :seed 7)))
    (dotimes (i 70) (end-turn s))
    (is (>= (loop for c being the hash-values of (gs-cities s)
                  count (= (city-owner c) 2))
            2))))

;;; --- terraform -------------------------------------------------------------

(test terraform-builds-improvement
  ;; a settler's road job takes 2 turns; it holds position while working
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (u (add-unit s :settlers 1 2 2))
         (tile (tile-at (gs-map s) 2 2)))
    (apply-command s (list :build-road :unit (unit-id u)))
    (is (eq :build-road (unit-work u)))
    (is (= 0 (unit-moves-left u)))          ; busy: no movement
    (is-false (tile-road tile))
    (end-turn s)
    (is-false (tile-road tile))             ; still working
    (is (= 0 (unit-moves-left u)))          ; re-zeroed while busy
    (end-turn s)
    (is-true (tile-road tile))              ; finished
    (is-false (unit-work u))
    (is (> (unit-moves-left u) 0))))        ; free to move again

(test terraform-yield-bonus
  ;; a mine on hills adds +1 shield, which flows through TILE-YIELD
  (let* ((s (bare-state 6 6 :terrain :hills))
         (u (add-unit s :settlers 1 2 2))
         (tile (tile-at (gs-map s) 2 2))
         (s0 (nth-value 1 (tile-yield tile))))
    (apply-command s (list :mine :unit (unit-id u)))
    (dotimes (i 4) (end-turn s))            ; mine takes 4 turns
    (is-true (tile-mine tile))
    (is (= (1+ s0) (nth-value 1 (tile-yield tile))))))

(test irrigation-adds-food
  ;; irrigating grassland is a 4-turn job that adds +1 food via TILE-YIELD
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (u (add-unit s :settlers 1 2 2))
         (tile (tile-at (gs-map s) 2 2))
         (f0 (nth-value 0 (tile-yield tile))))
    (apply-command s (list :irrigate :unit (unit-id u)))
    (is (eq :irrigate (unit-work u)))
    (dotimes (i 3) (end-turn s))
    (is-false (tile-irrigation tile))         ; still working after 3 turns
    (end-turn s)
    (is-true (tile-irrigation tile))          ; done on the 4th
    (is (= (1+ f0) (nth-value 0 (tile-yield tile))))))

(test terraform-restrictions
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (settler (add-unit s :settlers 1 2 2))
         (warrior (add-unit s :warriors 1 3 3)))
    ;; only units with the :terraform ability can build
    (signals command-error
      (apply-command s (list :build-road :unit (unit-id warrior))))
    ;; a mine needs the right terrain (not grassland)
    (signals command-error
      (apply-command s (list :mine :unit (unit-id settler))))
    ;; no double-building the same improvement
    (setf (tile-road (tile-at (gs-map s) 2 2)) t)
    (signals command-error
      (apply-command s (list :build-road :unit (unit-id settler))))))

(test terraform-cancelled-by-move
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (u (add-unit s :settlers 1 2 2)))
    (apply-command s (list :build-road :unit (unit-id u)))
    (is (eq :build-road (unit-work u)))
    (setf (unit-moves-left u) 1)            ; give it a step
    (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))
    (is-false (unit-work u))                ; moving abandons the job
    (is-false (tile-road (tile-at (gs-map s) 2 2)))))

(test railroad-requires-tech-and-road
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (u (add-unit s :settlers 1 2 2))
         (p (player-by-id s 1)))
    ;; no Railroad advance yet
    (signals command-error (apply-command s (list :build-railroad :unit (unit-id u))))
    (setf (gethash :rail-road (player-techs p)) t)
    ;; tech but no underlying road
    (signals command-error (apply-command s (list :build-railroad :unit (unit-id u))))
    (setf (tile-road (tile-at (gs-map s) 2 2)) t)
    (apply-command s (list :build-railroad :unit (unit-id u)))
    (is (eq :build-railroad (unit-work u)))
    (dotimes (i 3) (end-turn s))
    (is-true (tile-railroad (tile-at (gs-map s) 2 2)))))

(test clear-forest
  (let* ((s (bare-state 6 6 :terrain :forest))
         (u (add-unit s :settlers 1 2 2))
         (g (add-unit s :settlers 1 4 4)))
    ;; only works on forest
    (setf (tile-terrain (tile-at (gs-map s) 4 4)) :grassland)
    (signals command-error (apply-command s (list :clear-forest :unit (unit-id g))))
    (apply-command s (list :clear-forest :unit (unit-id u)))
    (is (eq :clear-forest (unit-work u)))
    (dotimes (i 3) (end-turn s))
    (is (eq :plains (tile-terrain (tile-at (gs-map s) 2 2))))))   ; forest -> plains

(test fort-requires-tech-and-boosts-defense
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (u (add-unit s :settlers 1 2 2))
         (p (player-by-id s 1)))
    ;; needs Construction
    (signals command-error (apply-command s (list :build-fort :unit (unit-id u))))
    (setf (gethash :construction (player-techs p)) t)
    (apply-command s (list :build-fort :unit (unit-id u)))
    (is (eq :build-fort (unit-work u)))
    (dotimes (i 3) (end-turn s))
    (is-true (tile-fort (tile-at (gs-map s) 2 2)))
    ;; a defender on the fort is twice as strong (+100%)
    (let ((d (add-unit s :phalanx 1 3 3))
          (f (add-unit s :phalanx 1 2 2)))           ; 2,2 has the fort
      (is (= (* 2 (civ-model::defense-strength s d))
             (civ-model::defense-strength s f))))))

(test railroad-shield-bonus
  (let* ((s (bare-state 6 6 :terrain :hills))
         (tile (tile-at (gs-map s) 3 3)))
    (setf (tile-mine tile) t)                       ; hills + mine = 1 shield
    (is (= 1 (nth-value 1 (tile-yield tile))))
    (setf (tile-railroad tile) t)                   ; railroad: +1 shield
    (is (= 2 (nth-value 1 (tile-yield tile))))
    ;; no free shields where there were none
    (let ((g (tile-at (gs-map s) 4 4)))
      (setf (tile-railroad g) t)                    ; grassland: 0 shields
      (is (= 0 (nth-value 1 (tile-yield g)))))))

;;; --- economy / upkeep ------------------------------------------------------

(test improvement-upkeep
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (setf (city-buildings c) '(:library :marketplace))   ; upkeep 1 + 1
    (setf (player-gold p) 10)
    (is (= 2 (city-upkeep c)))
    (civ-model::process-economy s)
    (is (= 8 (player-gold p)))))

(test wonders-have-no-upkeep
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-buildings c) '(:pyramids :library))      ; only library costs
    (is (= 1 (city-upkeep c)))))

(test bankruptcy-sells-buildings
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (setf (city-buildings c) '(:university :factory))    ; upkeep 3 + 4 = 7
    (setf (player-gold p) 0)
    (civ-model::process-economy s)
    (is (= 0 (player-gold p)))                           ; floored, never negative
    (is (< (length (city-buildings c)) 2))))             ; sold to stay solvent

;;; --- government & happiness ------------------------------------------------

(test tile-yield-government
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (tile (tile-at (gs-map s) 3 3)))
    (setf (tile-irrigation tile) t (tile-special tile) t)   ; grassland food 2+1 = 3
    (is (= 3 (nth-value 0 (tile-yield tile))))              ; no government
    (is (= 2 (nth-value 0 (tile-yield tile :despotism))))   ; despotic -1 on 3+
    (setf (tile-river tile) t)                              ; +1 trade
    (is (= 1 (nth-value 2 (tile-yield tile))))
    (is (= 2 (nth-value 2 (tile-yield tile :republic))))    ; republic +1 trade
    (is (= 1 (nth-value 2 (tile-yield tile :monarchy))))))  ; monarchy: no bonus

(test happiness-content-and-disorder
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 3)
    (is (equal '(0 3 0) (multiple-value-list (city-happiness s c 0))))  ; all content
    (is-false (city-disorder-p s c))
    (setf (city-size c) 6)                                  ; past the content base
    (multiple-value-bind (h cont u) (city-happiness s c 0)
      (is (= 0 h)) (is (= 4 cont)) (is (= 2 u)))
    (is-true (city-disorder-p s c))                         ; unhappy > happy
    (setf (city-buildings c) '(:temple))                    ; a temple calms 2
    (is-false (city-disorder-p s c))))

(test martial-law
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (setf (city-size c) 6)
    (add-unit s :warriors 1 2 2)
    (add-unit s :phalanx 1 2 2)
    (is (= 2 (count-city-military s c)))
    (is (= 0 (nth-value 2 (city-happiness s c 0))))   ; despotism: 2 units calm 2 unhappy
    (setf (player-government p) :democracy)            ; no martial law
    (is (= 2 (nth-value 2 (city-happiness s c 0))))))

(test luxuries-make-citizens-happy
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (setf (city-size c) 4)
    (setf (player-luxury-rate p) 100 (player-tax-rate p) 0 (player-science-rate p) 0)
    ;; 100 trade at 100% luxury -> 50 upgrades, all 4 content become happy
    (is (= 4 (nth-value 0 (city-happiness s c 100))))))

(test specialists-reduce-workers-and-tiles
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 4)
    (civ-model::city-auto-work s c)
    (is (= 4 (length (city-worked c))))                 ; all four work tiles
    (is (null (city-specialists c)))
    (apply-command s (list :set-specialists :city (city-id c) :op :add))
    (is (= 3 (city-worker-count c)))                    ; one pulled off a tile
    (is (= 3 (length (city-worked c))))
    (is (equal '(:entertainer) (city-specialists c)))))

(test specialist-cycle-and-remove
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 4)
    (apply-command s (list :set-specialists :city (city-id c) :op :add))
    (apply-command s (list :set-specialists :city (city-id c) :op :cycle :index 0))
    (is (equal '(:taxman) (city-specialists c)))        ; entertainer -> taxman
    (apply-command s (list :set-specialists :city (city-id c) :op :cycle :index 0))
    (is (equal '(:scientist) (city-specialists c)))     ; taxman -> scientist
    (apply-command s (list :set-specialists :city (city-id c) :op :remove))
    (is (null (city-specialists c)))                    ; back to working
    (is (= 4 (city-worker-count c)))))

(test forced-specialists-when-no-tiles
  ;; more citizens than workable tiles -> the surplus become specialists
  (let* ((s (bare-state 6 6 :terrain :ocean))
         (c (civ-model::register-city s :name "Isle" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 12)
    (civ-model::city-auto-work s c)
    (is (= 8 (length (city-worked c))))                 ; only 8 neighbour tiles
    (is (= 4 (length (city-specialists c))))            ; 12 - 8 forced into jobs
    (is (every (lambda (j) (eq j :entertainer)) (city-specialists c)))))

(test entertainer-quells-disorder
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 6)
    (is-true (city-disorder-p s c))                     ; 4 content, 2 unhappy
    (setf (city-specialists c) '(:entertainer :entertainer))
    (is-false (city-disorder-p s c))))                  ; specialists are content

(test specialist-output-gold-and-beakers
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (setf (city-size c) 4 (player-government p) :monarchy)
    (let ((g0 (player-gold p)) (b0 (player-beakers p)))
      (setf (city-specialists c) '(:taxman :taxman :scientist))
      (civ-model::process-city s c)
      (is (<= (+ g0 4) (player-gold p)))                ; +2 gold per taxman
      (is (<= (+ b0 200) (player-beakers p))))))         ; +200 fine beakers per scientist

(test specialists-round-trip-through-save
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-specialists c) '(:taxman :scientist))
    (let ((c2 (civ-model::list->city (civ-model::city->list c))))
      (is (equal '(:taxman :scientist) (city-specialists c2))))))

(test manual-tile-frees-citizen-to-specialist
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 4)
    (civ-model::city-auto-work s c)
    (is (= 4 (length (city-worked c))))
    (let ((tl (first (city-worked c))))
      (apply-command s (list :work-tile :city (city-id c)
                             :x (first tl) :y (second tl)))     ; click a worked tile
      (is-true (city-manual-tiles c))                            ; enters manual mode
      (is (= 3 (length (city-worked c))))                        ; that tile is freed
      (is (= 1 (length (city-specialists c))))                   ; its citizen -> specialist
      (is-false (member tl (city-worked c) :test #'equal)))))

(test manual-tile-assigns-idle-tile
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 3)
    (civ-model::city-auto-work s c)
    (let* ((radius (civ-model::city-radius-tiles s c))
           (idle (find-if-not (lambda (tl) (member tl (city-worked c) :test #'equal)) radius))
           (worked (first (city-worked c))))
      (apply-command s (list :work-tile :city (city-id c)         ; free a worker first
                             :x (first worked) :y (second worked)))
      (is (= 1 (length (city-specialists c))))
      (apply-command s (list :work-tile :city (city-id c)         ; put it on an idle tile
                             :x (first idle) :y (second idle)))
      (is (= 3 (length (city-worked c))))                         ; specialist took the tile
      (is (= 0 (length (city-specialists c))))
      (is-true (member idle (city-worked c) :test #'equal)))))

(test manual-tile-auto-hands-back-to-governor
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 4)
    (apply-command s (list :work-tile :city (city-id c) :x 2 :y 1))
    (is-true (city-manual-tiles c))
    (apply-command s (list :work-tile :city (city-id c) :auto t))
    (is-false (city-manual-tiles c))
    (is (null (city-tile-locks c)))))

(test manual-tiles-round-trip-through-save
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-manual-tiles c) t (city-tile-locks c) '((2 1) (3 3)))
    (let ((c2 (civ-model::list->city (civ-model::city->list c))))
      (is-true (city-manual-tiles c2))
      (is (equal '((2 1) (3 3)) (city-tile-locks c2))))))

;;; --- unit shield support -----------------------------------------------------

(defun home-units (s c n type &key (x (city-x c)) (y (city-y c)))
  "Create N units of TYPE homed to city C (test helper)."
  (dotimes (i n)
    (setf (unit-home (civ-model::register-unit s :type type :owner (city-owner c) :x x :y y))
          (city-id c))))

(test unit-built-in-city-is-homed
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 4
          (city-production c) (list :unit :warriors) (city-shield-box c) 10)
    (civ-model::city-try-complete s c)
    (let ((u (loop for uu being the hash-values of (gs-units s)
                   when (eq (unit-type uu) :warriors) return uu)))
      (is (eql (unit-home u) (city-id c))))))             ; built unit is homed here

(test monarchy-grants-a-small-free-support-allowance
  (let* ((s (bare-state 8 8 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 3 :y 3))
         (p (player-by-id s 1)))
    (setf (city-size c) 4 (player-government p) :monarchy)
    (home-units s c 2 :warriors)
    (is (= 0 (civ-model::city-shield-support s c)))        ; 2 units within the free allowance
    (home-units s c 1 :warriors)
    (is (= 1 (civ-model::city-shield-support s c)))))      ; the 3rd costs a shield

(test settler-build-costs-a-population-point
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 3
          (city-production c) (list :unit :settlers) (city-shield-box c) 40)
    (civ-model::city-try-complete s c)
    (is (= 2 (city-size c)))                               ; the citizen left to settle
    (is (find :settlers (loop for u being the hash-values of (gs-units s) collect (unit-type u))))))

(test size-1-city-sends-a-homeless-settler
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 1
          (city-production c) (list :unit :settlers) (city-shield-box c) 40)
    (civ-model::city-try-complete s c)
    (is (= 1 (city-size c)))                               ; a lone capital keeps its citizen
    (let ((u (find :settlers (loop for uu being the hash-values of (gs-units s) collect uu)
                   :key #'unit-type)))
      (is-true u)                                          ; but a settler is still produced
      (is (null (unit-home u))))))                          ; homeless -> no upkeep

(test settler-food-upkeep-by-government
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (setf (city-size c) 3)
    (home-units s c 1 :settlers)
    (is (= 1 (civ-model::city-settler-food s c)))          ; despotism: 1 food
    (setf (player-government p) :monarchy)
    (is (= 2 (civ-model::city-settler-food s c)))))         ; else: 2 food

(test despotism-supports-units-free-up-to-size
  (let* ((s (bare-state 8 8 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 3 :y 3)))
    (setf (city-size c) 2)                                ; despotism: 2 free
    (home-units s c 3 :warriors)
    (is (= 1 (civ-model::city-shield-support s c)))))      ; 3 units - 2 free = 1

(test diplomats-and-caravans-need-no-support
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (setf (city-size c) 1 (player-government p) :monarchy)
    (home-units s c 1 :diplomat) (home-units s c 1 :caravan)
    (is (= 0 (civ-model::city-shield-support s c)))))

(test unit-support-list-marks-free-then-paid
  (let* ((s (bare-state 8 8 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 3 :y 3)))
    (setf (city-size c) 1)                                ; despotism: first 1 free
    (home-units s c 2 :warriors)
    (is (equal '(0 1) (mapcar #'cdr (civ-model::city-unit-support-list s c))))))

(test over-supported-city-disbands-the-farthest-unit
  (let* ((s (bare-state 8 8 :terrain :grassland))
         (c (civ-model::register-city s :name "A" :owner 1 :x 3 :y 3))
         (p (player-by-id s 1)))
    (setf (city-size c) 2 (player-government p) :monarchy)  ; monarchy: every unit costs 1
    (home-units s c 5 :warriors :x 6 :y 6)                  ; support 5 >> production
    (let ((before (hash-table-count (gs-units s))))
      (civ-model::process-city s c)
      (is (= (1- before) (hash-table-count (gs-units s)))))))  ; one disbanded

(test unit-home-round-trips-through-save
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (u (civ-model::register-unit s :type :warriors :owner 1 :x 2 :y 2)))
    (setf (unit-home u) (city-id c))
    (let ((u2 (civ-model::list->unit (civ-model::unit->list u))))
      (is (eql (city-id c) (unit-home u2))))))

(test government-trade-advantage
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (setf (tile-river (tile-at (gs-map s) 2 2)) t
          (tile-road  (tile-at (gs-map s) 2 2)) t)     ; trade at the centre
    (setf (player-government p) :despotism)
    (let ((despot (nth-value 2 (city-yields s c))))
      (setf (player-government p) :democracy)          ; +trade bonus, no corruption
      (is (> (nth-value 2 (city-yields s c)) despot)))))

(test revolution-and-government-tech
  (let* ((s (bare-state 6 6))
         (p (player-by-id s 1)))
    (setf (gethash :monarchy (player-techs p)) t)
    (signals command-error                              ; lacks The Republic
      (apply-command s (list :set-government :player 1 :to :republic)))
    (apply-command s (list :set-government :player 1 :to :monarchy))
    (is (eq :anarchy (player-government p)))            ; revolution: one turn of anarchy
    (is (eq :monarchy (player-gov-target p)))
    (end-turn s)
    (is (eq :monarchy (player-government p)))           ; then the new government takes power
    (is (= 0 (player-anarchy-left p)))))

(test anarchy-halts-research
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "A" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (setf (tile-river (tile-at (gs-map s) 2 2)) t
          (player-government p) :anarchy (player-beakers p) 0)
    (civ-model::process-city s c)
    (is (= 0 (player-beakers p)))))                     ; no science under anarchy

(test research-completes-without-going-negative
  ;; learning an advance must not overdraw the beaker pool: the cost is read once,
  ;; before the new tech raises RESEARCH-COST (regression -- it used to subtract
  ;; the post-learning cost, leaving science negative for several turns)
  (let* ((s (bare-state 6 6)) (p (player-by-id s 1)))
    (setf (player-researching p) :alphabet
          (player-beakers p) (civ-model::research-cost p))   ; exactly enough
    (civ-model::process-research s)
    (is-true (player-has-tech-p p :alphabet))                ; learned it
    (is (= 0 (player-beakers p)))                            ; spent exactly the cost, no debt
    ;; an overflow carries over intact (and still never negative)
    (setf (player-researching p) :pottery
          (player-beakers p) (+ (civ-model::research-cost p) 250))
    (civ-model::process-research s)
    (is-true (player-has-tech-p p :pottery))
    (is (= 250 (player-beakers p)))))

(test set-rates-validation
  (let* ((s (bare-state 6 6))
         (p (player-by-id s 1)))
    (signals command-error (apply-command s (list :set-rates :player 1 :tax 50 :luxury 50 :science 50)))
    (signals command-error (apply-command s (list :set-rates :player 1 :tax 70 :luxury 0 :science 30)))
    (signals command-error (apply-command s (list :set-rates :player 1 :tax -10 :luxury 60 :science 50)))
    (apply-command s (list :set-rates :player 1 :tax 60 :luxury 0 :science 40))
    (is (= 60 (player-tax-rate p)))
    (is (= 40 (player-science-rate p)))))

(test war-weariness
  (let* ((s (bare-state 8 8))
         (c (civ-model::register-city s :name "A" :owner 1 :x 4 :y 4))
         (p (player-by-id s 1)))
    (setf (city-size c) 5 (player-government p) :republic)
    (is (= 1 (nth-value 2 (city-happiness s c 0))))   ; size 5: one unhappy baseline
    (add-unit s :legion 1 0 0)                         ; a military unit in the field
    (is (= 2 (nth-value 2 (city-happiness s c 0))))    ; republic: +1 from the field unit
    (setf (player-government p) :democracy)
    (is (= 3 (nth-value 2 (city-happiness s c 0))))    ; democracy: +2
    (setf (player-government p) :despotism)
    (is (= 1 (nth-value 2 (city-happiness s c 0))))    ; despotism feels no war weariness
    ;; a garrison inside a city is not "in the field"
    (setf (player-government p) :republic)
    (add-unit s :phalanx 1 4 4)
    (is (= 1 (civ-model::city-military-unhappiness s c)))))

(test rapture-growth
  (let* ((s (bare-state 7 7))
         (c (civ-model::register-city s :name "B" :owner 1 :x 3 :y 3))
         (p (player-by-id s 1)))
    (setf (player-government p) :republic (city-size c) 3
          (player-luxury-rate p) 80 (player-tax-rate p) 0 (player-science-rate p) 20)
    ;; rich surroundings: irrigated, rivered, roaded grassland for food & trade
    (dolist (xy '((2 2)(3 2)(4 2)(2 3)(4 3)(2 4)(3 4)(4 4)))
      (let ((tl (tile-at (gs-map s) (first xy) (second xy))))
        (setf (tile-irrigation tl) t (tile-river tl) t (tile-road tl) t)))
    (setf (tile-river (tile-at (gs-map s) 3 3)) t (tile-road (tile-at (gs-map s) 3 3)) t)
    (civ-model::city-auto-work s c)
    (is-true (city-celebrating-p s c))
    ;; "We Love the King": +1 size every turn while celebrating (no 40-food wait)
    (civ-model::process-city s c)
    (is (= 4 (city-size c)))
    (civ-model::process-city s c)
    (is (= 5 (city-size c)))))

(test growth-cap-and-aqueduct
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "C" :owner 1 :x 2 :y 2)))
    (setf (city-size c) 8
          (city-buildings c) '(:temple :colosseum :cathedral))  ; keep it content
    (is-false (city-disorder-p s c))
    (setf (city-food-box c) 1000)
    (civ-model::process-city s c)
    (is (= 8 (city-size c)))                ; without an aqueduct a city can't pass 8
    (push :aqueduct (city-buildings c))
    (setf (city-food-box c) 1000)
    (civ-model::process-city s c)
    (is (= 9 (city-size c)))))              ; an aqueduct lifts the cap

;;; --- pollution -------------------------------------------------------------

(test pollution-halves-yield
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (tile (tile-at (gs-map s) 3 3)))
    (setf (tile-irrigation tile) t)                  ; grassland 2 food +1 = 3
    (is (= 3 (nth-value 0 (tile-yield tile))))
    (setf (tile-pollution tile) t)
    (is (= 1 (nth-value 0 (tile-yield tile))))))     ; halved (floor 3/2)

(defun setup-industrial-city (s c)
  "Surround C with sustainable, high-output land (irrigated grassland with a
shield special) so the city keeps producing instead of starving."
  (dolist (xy '((3 3)(4 3)(5 3)(3 4)(5 4)(3 5)(4 5)(5 5)))
    (let ((tl (tile-at (gs-map s) (first xy) (second xy))))
      (setf (tile-terrain tl) :grassland (tile-irrigation tl) t (tile-special tl) t)))
  (setf (city-size c) 8))

(test pollution-generation
  ;; an industrialized, dirty, high-shield city blights nearby tiles
  (let* ((s (bare-state 8 8 :terrain :grassland))
         (c (civ-model::register-city s :name "Smog" :owner 1 :x 4 :y 4))
         (p (player-by-id s 1)))
    (setup-industrial-city s c)
    (setf (gethash :industrialization (player-techs p)) t
          (city-buildings c) '(:factory :power-plant :temple :colosseum :cathedral))
    (is-false (city-disorder-p s c))
    (dotimes (i 50) (civ-model::process-city s c))
    (is (plusp (loop for (x y tl) in (neighbors (gs-map s) 4 4)
                     count (tile-pollution tl))))))

(test no-pollution-before-industrialization
  (let* ((s (bare-state 8 8 :terrain :grassland))
         (c (civ-model::register-city s :name "Clean" :owner 1 :x 4 :y 4)))
    (setup-industrial-city s c)
    (setf (city-buildings c) '(:factory :power-plant :temple :colosseum :cathedral))
    (dotimes (i 50) (civ-model::process-city s c))
    (is (zerop (loop for (x y tl) in (neighbors (gs-map s) 4 4)
                     count (tile-pollution tl))))))    ; no industrialization -> no pollution

(test clean-pollution
  (let* ((s (bare-state 6 6 :terrain :grassland))
         (tile (tile-at (gs-map s) 3 3))
         (u (add-unit s :settlers 1 3 3)))
    (setf (tile-pollution tile) t)
    ;; only a settler on a polluted tile can clean
    (let ((w (add-unit s :warriors 1 1 1)))
      (signals command-error (apply-command s (list :clean-pollution :unit (unit-id w)))))
    (apply-command s (list :clean-pollution :unit (unit-id u)))
    (is (eq :clean-pollution (unit-work u)))
    (is (= 0 (unit-moves-left u)))                   ; busy
    (end-turn s) (end-turn s)
    (is-true (tile-pollution tile))                  ; still working
    (end-turn s)
    (is-false (tile-pollution tile))                 ; cleaned after 3 turns
    (is-false (unit-work u))))

(test global-warming
  ;; heavy pollution degrades land terrain over time
  (let* ((s (bare-state 8 8 :terrain :grassland)))
    ;; pollute well past the threshold
    (dolist (xy '((0 0)(1 0)(2 0)(3 0)(4 0)(5 0)(0 1)(1 1)))
      (setf (tile-pollution (tile-at (gs-map s) (first xy) (second xy))) t))
    (is (= 0 (gs-warming s)))
    (dotimes (i 30) (civ-model::process-global-warming s))
    (is (plusp (gs-warming s)))                 ; warming events occurred
    ;; some grassland has degraded to plains/desert
    (is (plusp (let ((n 0))
                 (do-tiles (x y tile (gs-map s)) (declare (ignore x y))
                   (unless (member (tile-terrain tile) '(:grassland :ocean)) (incf n)))
                 n)))))

(test no-warming-below-threshold
  (let* ((s (bare-state 8 8 :terrain :grassland)))
    (setf (tile-pollution (tile-at (gs-map s) 0 0)) t)   ; 1 polluted (<= threshold)
    (dotimes (i 30) (civ-model::process-global-warming s))
    (is (= 0 (gs-warming s)))))

;;; --- diplomacy -------------------------------------------------------------

(test relations-default-peace
  (let ((s (bare-state 6 6)))
    (is (eq :peace (relation s 1 2)))
    (is-false (at-war-p s 1 2))
    (apply-command s (list :declare-war :player 1 :against 2))
    (is (eq :war (relation s 1 2)))
    (is-true (at-war-p s 1 2))
    (is-true (at-war-p s 2 1))               ; symmetric
    (apply-command s (list :make-peace :player 1 :against 2))
    (is-false (at-war-p s 1 2))))

(test peace-blocks-attack-war-allows-it
  (let* ((s (bare-state 6 6 :seed 1))
         (a (add-unit s :legion 1 2 2))
         (d (add-unit s :warriors 2 3 2)))
    ;; at peace: moving into their tile is refused, defender survives
    (signals command-error (apply-command s (list :move-unit :unit (unit-id a) :dx 1 :dy 0)))
    (is-true (unit-by-id s (unit-id d)))
    ;; declare war, now the attack goes through
    (apply-command s (list :declare-war :player 1 :against 2))
    (apply-command s (list :move-unit :unit (unit-id a) :dx 1 :dy 0))
    (is (null (unit-by-id s (unit-id d))))))

(test alliances
  (let ((s (bare-state 6 6)))                       ; both city-less: the AI accepts
    (is-false (allied-p s 1 2))
    (apply-command s (list :propose-alliance :player 1 :against 2))
    (is (eq :alliance (relation s 1 2)))
    (is-true (allied-p s 1 2))
    (is-true (allied-p s 2 1))                       ; symmetric
    (is-false (at-war-p s 1 2))                       ; an alliance is not war
    ;; can't re-propose an existing alliance
    (signals command-error (apply-command s (list :propose-alliance :player 1 :against 2)))
    ;; breaking it reverts to plain peace
    (apply-command s (list :break-alliance :player 1 :against 2))
    (is-false (allied-p s 1 2))
    (is (eq :peace (relation s 1 2)))
    (signals command-error (apply-command s (list :break-alliance :player 1 :against 2)))
    ;; you must make peace before you can ally from a state of war
    (apply-command s (list :declare-war :player 1 :against 2))
    (signals command-error (apply-command s (list :propose-alliance :player 1 :against 2)))))

(test senate-forbids-declaring-war
  (let* ((s (bare-state 6 6)) (p (player-by-id s 1)))
    ;; despotism: a declaration goes through
    (apply-command s (list :declare-war :player 1 :against 2))
    (is-true (at-war-p s 1 2))
    (apply-command s (list :make-peace :player 1 :against 2))
    ;; under a Republic the Senate vetoes it
    (setf (player-government p) :republic)
    (is-true (senate-p s 1))
    (signals command-error (apply-command s (list :declare-war :player 1 :against 2)))
    (is-false (at-war-p s 1 2))))

(test senate-forces-acceptance-of-cease-fire
  (let ((s (bare-state 6 6)))
    (dolist (x '(1 2 3 4))
      (civ-model::register-city s :name (format nil "C~D" x) :owner 1 :x x :y 1)) ; strong human
    (civ-model::register-city s :name "Ur" :owner 2 :x 1 :y 4)                    ; weak AI
    (setf (player-government (player-by-id s 1)) :republic
          (relation s 1 2) :war)                       ; a war that predates the revolution
    (civ-model::ai-diplomacy s (player-by-id s 2))     ; the losing AI sues for a cease-fire
    ;; the Senate forces the human to accept -- peace applied at once, no prompt
    (is (eq :peace (relation s 1 2)))
    (is-true (civ-model::truce-active-p s 1 2))
    (is-false (civ-model::pending-offer-p s 2 :ceasefire))))

(test cease-fire
  (let ((s (bare-state 6 6)))                   ; both city-less -> the AI accepts
    (apply-command s (list :declare-war :player 1 :against 2))
    ;; you can't extort tribute mid-war
    (signals command-error (apply-command s (list :demand-tribute :player 1 :against 2)))
    (apply-command s (list :propose-ceasefire :player 1 :against 2))
    (is (eq :peace (relation s 1 2)))            ; the war ends
    (is-true (civ-model::truce-active-p s 1 2))  ; and a truce holds
    ;; the truce lapses after *ceasefire-turns*
    (incf (gs-turn s) civ-model::*ceasefire-turns*)
    (is-false (civ-model::truce-active-p s 1 2))
    ;; a cease-fire needs an actual war to end
    (signals command-error (apply-command s (list :propose-ceasefire :player 1 :against 2)))))

(test tribute
  ;; a weaker AI with gold to spare pays up (bare-state's seed rolls a yes)
  (let ((s (bare-state 6 6)))
    (civ-model::register-city s :name "Rome" :owner 1 :x 2 :y 2)   ; the demander is stronger
    (setf (player-gold (player-by-id s 1)) 0
          (player-gold (player-by-id s 2)) 100)
    (apply-command s (list :demand-tribute :player 1 :against 2))
    (is (= 50 (player-gold (player-by-id s 1))))   ; *tribute-amount* changes hands
    (is (= 50 (player-gold (player-by-id s 2)))))
  ;; a civ that is not the weaker one refuses
  (let ((s (bare-state 6 6)))
    (civ-model::register-city s :name "Ur" :owner 2 :x 1 :y 1)     ; the AI holds the city
    (setf (player-gold (player-by-id s 2)) 100)
    (signals command-error (apply-command s (list :demand-tribute :player 1 :against 2)))
    (is (= 100 (player-gold (player-by-id s 2))))))                 ; nothing paid

(test ai-offers-the-human-a-cease-fire
  ;; an AI losing a war to the human offers a cease-fire -- queued, not applied,
  ;; until the human answers
  (let ((s (bare-state 6 6)))
    (dolist (x '(1 2 3 4)) (civ-model::register-city s :name (format nil "C~D" x)
                                                       :owner 1 :x x :y 1))  ; strong human
    (civ-model::register-city s :name "Ur" :owner 2 :x 1 :y 4)              ; weak AI
    (apply-command s (list :declare-war :player 1 :against 2))
    (civ-model::ai-diplomacy s (player-by-id s 2))     ; the AI takes its diplomacy turn
    (is-true (civ-model::pending-offer-p s 2 :ceasefire))
    (is-true (at-war-p s 1 2))                          ; nothing changes until accepted
    (apply-command s (list :resolve-offer :player 1 :from 2 :kind :ceasefire :accept t))
    (is (eq :peace (relation s 1 2)))
    (is-true (civ-model::truce-active-p s 1 2))
    (is-false (civ-model::pending-offer-p s 2 :ceasefire))))

(test resolving-diplomatic-offers
  ;; declining leaves relations untouched; accepting an alliance forms it
  (let ((s (bare-state 6 6)))
    (civ-model::add-offer s 2 :alliance)
    (apply-command s (list :resolve-offer :player 1 :from 2 :kind :alliance :accept nil))
    (is (eq :peace (relation s 1 2)))
    (is-false (civ-model::pending-offer-p s 2 :alliance))
    (civ-model::add-offer s 2 :alliance)
    (apply-command s (list :resolve-offer :player 1 :from 2 :kind :alliance :accept t))
    (is (eq :alliance (relation s 1 2)))))

(test stale-offers-are-pruned
  ;; an alliance offer is voided once war breaks out
  (let ((s (bare-state 6 6)))
    (add-unit s :warriors 2 5 5)                        ; keep the AI alive
    (civ-model::add-offer s 2 :alliance)
    (apply-command s (list :declare-war :player 1 :against 2))
    (civ-model::prune-offers s)
    (is-false (civ-model::pending-offer-p s 2 :alliance))))

(test allies-cannot-attack-each-other
  (let* ((s (bare-state 6 6 :seed 1))
         (a (add-unit s :legion 1 2 2))
         (d (add-unit s :warriors 2 3 2)))
    (apply-command s (list :propose-alliance :player 1 :against 2))
    ;; like peace, an alliance forbids moving into the ally's tile
    (signals command-error (apply-command s (list :move-unit :unit (unit-id a) :dx 1 :dy 0)))
    (is-true (unit-by-id s (unit-id d)))))

(test choose-research-target
  (let* ((s (bare-state 6 6)) (p (player-by-id s 1)))
    ;; pick a reachable advance
    (apply-command s (list :set-research :player 1 :tech :pottery))
    (is (eq :pottery (player-researching p)))
    ;; an advance whose prerequisites aren't met is refused
    (signals command-error (apply-command s (list :set-research :player 1 :tech :the-republic)))
    ;; an already-known advance is refused
    (setf (gethash :alphabet (player-techs p)) t)
    (signals command-error (apply-command s (list :set-research :player 1 :tech :alphabet)))
    ;; ...but its now-reachable successor is allowed
    (apply-command s (list :set-research :player 1 :tech :writing))
    (is (eq :writing (player-researching p)))))

(test human-is-reprompted-after-each-advance
  ;; a human's research target clears when an advance completes (the view then
  ;; prompts for the next), while an AI rolls straight on to a new target
  (let* ((s (bare-state 6 6)) (p (player-by-id s 1)))
    (setf (player-kind p) :human
          (player-researching p) :pottery
          (player-beakers p) (civ-model::research-cost p))
    (civ-model::process-research s)
    (is-true (player-has-tech-p p :pottery))
    (is (null (player-researching p))))             ; cleared -> the view will prompt
  (let* ((s (bare-state 6 6)) (p (player-by-id s 2)))  ; player 2 is an AI
    (setf (player-researching p) :pottery
          (player-beakers p) (civ-model::research-cost p))
    (civ-model::process-research s)
    (is-true (player-has-tech-p p :pottery))
    (is-true (player-researching p))))              ; AI auto-picked the next

(test more-than-two-civilizations
  (let ((s (make-new-game :seed 3 :players '("A" "B" "C" "D"))))
    (is (= 4 (length (gs-players s))))
    (is (eq :peace (relation s 1 4)))
    ;; each civ starts with its own colour and a couple of units
    (is (= 8 (hash-table-count (gs-units s))))))   ; 4 civs x (settler+warriors)

(test barbarians-always-at-war
  (let* ((s (make-new-game :seed 2 :players '("You" "Rival") :barbarians t))
         (barb (loop for p across (gs-players s)
                     when (eq (player-kind p) :barbarian) return (player-id p))))
    (is-true barb)
    (is-true (at-war-p s 1 barb))                ; barbarians are at war with all
    (is-true (at-war-p s 2 barb))
    (is (eq :war (relation s 1 barb)))
    ;; you can't sue barbarians for peace
    (signals command-error (apply-command s (list :make-peace :player 1 :against barb)))))

(test barbarians-spawn-raiders
  (let ((s (make-new-game :seed 2 :players '("You" "Rival") :barbarians t))
        (barb nil))
    (setf barb (loop for p across (gs-players s)
                     when (eq (player-kind p) :barbarian) return (player-id p)))
    (dotimes (i 40) (end-turn s))
    (is (plusp (loop for u being the hash-values of (gs-units s)
                     count (= (unit-owner u) barb))))))   ; raiders appeared

(test trade-tech-for-gold
  (let* ((s (bare-state 6 6))
         (a (player-by-id s 1))    ; human buyer
         (b (player-by-id s 2)))   ; AI seller
    (setf (gethash :writing (player-techs b)) t   ; B knows Writing, A doesn't
          (player-gold a) 300)
    ;; A buys Writing from B for 250 gold
    (apply-command s (list :propose-trade :player 1 :to 2
                           :give '((:gold 250)) :want '((:tech :writing))))
    (is-true (player-has-tech-p a :writing))       ; A learned it
    (is-true (player-has-tech-p b :writing))       ; B still knows it (shared)
    (is (= 50 (player-gold a)))
    (is (= 250 (player-gold b)))))

(test trade-rejected-when-lopsided
  (let* ((s (bare-state 6 6))
         (a (player-by-id s 1)) (b (player-by-id s 2)))
    (setf (gethash :writing (player-techs b)) t (player-gold a) 300)
    ;; offer only 10 gold for a 250-valued advance: the AI refuses
    (signals command-error
      (apply-command s (list :propose-trade :player 1 :to 2
                             :give '((:gold 10)) :want '((:tech :writing)))))
    (is-false (player-has-tech-p a :writing))
    (is (= 300 (player-gold a)))))

(test trade-tech-swap
  (let* ((s (bare-state 6 6))
         (a (player-by-id s 1)) (b (player-by-id s 2)))
    (setf (gethash :pottery (player-techs a)) t      ; A has Pottery
          (gethash :masonry (player-techs b)) t)      ; B has Masonry
    (apply-command s (list :propose-trade :player 1 :to 2
                           :give '((:tech :pottery)) :want '((:tech :masonry))))
    (is-true (player-has-tech-p a :masonry))
    (is-true (player-has-tech-p b :pottery))))

(test trade-validation
  (let* ((s (bare-state 6 6))
         (a (player-by-id s 1)) (b (player-by-id s 2)))
    (setf (gethash :writing (player-techs a)) t
          (gethash :writing (player-techs b)) t)      ; both already have it
    ;; can't sell a tech the buyer already owns
    (signals command-error
      (apply-command s (list :propose-trade :player 1 :to 2
                             :give '((:tech :writing)) :want '((:gold 0)))))
    ;; can't give gold you don't have
    (is (= 0 (player-gold a)))
    (signals command-error
      (apply-command s (list :propose-trade :player 1 :to 2
                             :give '((:gold 999)) :want nil)))))

;;; --- diplomat espionage ----------------------------------------------------

(test diplomat-steals-tech
  (let* ((s (bare-state 6 6))
         (dip (add-unit s :diplomat 1 3 2))         ; adjacent to the enemy city
         (v (player-by-id s 2)) (me (player-by-id s 1)))
    (civ-model::register-city s :name "Babylon" :owner 2 :x 3 :y 3)
    (setf (gethash :writing (player-techs v)) t)    ; victim has Writing, I don't
    (is-false (player-has-tech-p me :writing))
    (apply-command s (list :steal-tech :unit (unit-id dip)))
    (is-true (player-has-tech-p me :writing))        ; stolen
    (is-false (unit-by-id s (unit-id dip)))))         ; the diplomat is spent

(test diplomat-sabotages-building
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Babylon" :owner 2 :x 3 :y 3))
         (dip (add-unit s :diplomat 1 3 2)))
    (setf (city-buildings c) '(:library :barracks))  ; library (80) costlier than barracks (40)
    (apply-command s (list :sabotage :unit (unit-id dip)))
    (is-false (member :library (city-buildings c)))   ; the priciest is wrecked
    (is-true (member :barracks (city-buildings c)))
    (is-false (unit-by-id s (unit-id dip)))))

(test sabotage-wrecks-production-when-no-buildings
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Ur" :owner 2 :x 3 :y 3))
         (dip (add-unit s :diplomat 1 3 2)))
    (setf (city-shield-box c) 40 (city-buildings c) nil)
    (apply-command s (list :sabotage :unit (unit-id dip)))
    (is (= 0 (city-shield-box c)))))

(test espionage-needs-a-diplomat-and-a-target
  (let* ((s (bare-state 6 6))
         (w (add-unit s :warriors 1 3 2))            ; not a diplomat
         (dip (add-unit s :diplomat 1 0 0)))          ; nowhere near a city
    (civ-model::register-city s :name "Babylon" :owner 2 :x 3 :y 3)
    (signals command-error (apply-command s (list :steal-tech :unit (unit-id w))))
    (signals command-error (apply-command s (list :sabotage :unit (unit-id dip))))))

(test first-theft-covert-second-means-war
  ;; an undefended city is a free target; the first theft is covert, the second
  ;; from the same civ provokes war
  (let* ((s (bare-state 6 6))
         (v (player-by-id s 2)))
    (civ-model::register-city s :name "Babylon" :owner 2 :x 3 :y 3)
    (dolist (k '(:masonry :writing)) (setf (gethash k (player-techs v)) t))
    (apply-command s (list :steal-tech :unit (unit-id (add-unit s :diplomat 1 3 2))))
    (is-true (player-has-tech-p (player-by-id s 1) :masonry))
    (is-false (at-war-p s 1 2))                  ; first theft: covert
    (apply-command s (list :steal-tech :unit (unit-id (add-unit s :diplomat 1 3 2))))
    (is-true (player-has-tech-p (player-by-id s 1) :writing))
    (is-true (at-war-p s 1 2))))                 ; caught stealing again -> war

(test defended-city-can-catch-the-spy
  ;; a walled, garrisoned city sometimes catches the spy: it is lost and war is
  ;; declared (deterministic over the seeded RNG)
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Fort" :owner 2 :x 3 :y 3))
         (v (player-by-id s 2)) (caught nil))
    (pushnew :walls (city-buildings c))
    (add-unit s :phalanx 2 3 3) (add-unit s :phalanx 2 3 3)   ; garrison -> high catch
    (dolist (k '(:masonry :writing :pottery :bronze-working :alphabet :the-wheel))
      (setf (gethash k (player-techs v)) t))
    (loop for i below 40 until caught do
      (let ((d (add-unit s :diplomat 1 3 2)))
        (handler-case (apply-command s (list :steal-tech :unit (unit-id d)))
          (command-error (e)
            (when (search "caught" (princ-to-string e))
              (setf caught t)
              (is-false (unit-by-id s (unit-id d)))   ; the spy is lost
              (is-true (at-war-p s 1 2)))))))           ; and war is declared
    (is-true caught)))

(test sabotage-is-overt-and-provokes-war
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Ur" :owner 2 :x 3 :y 3)))
    (setf (city-buildings c) '(:library))            ; undefended -> succeeds
    (apply-command s (list :sabotage :unit (unit-id (add-unit s :diplomat 1 3 2))))
    (is-false (member :library (city-buildings c)))
    (is-true (at-war-p s 1 2))))

(test establish-embassy
  (let* ((s (bare-state 6 6))
         (dip (add-unit s :diplomat 1 3 2)))
    (civ-model::register-city s :name "Babylon" :owner 2 :x 3 :y 3)
    (is-false (civ-model:has-embassy-p s 1 2))
    (apply-command s (list :establish-embassy :unit (unit-id dip)))
    (is-true (civ-model:has-embassy-p s 1 2))         ; embassy opened
    (is-false (unit-by-id s (unit-id dip)))            ; diplomat spent
    (is-false (at-war-p s 1 2))))                       ; benign

(test investigate-city
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Babylon" :owner 2 :x 3 :y 3))
         (dip (add-unit s :diplomat 1 3 2)))
    (setf (city-size c) 5 (city-buildings c) '(:walls))
    (let ((report (civ-model:city-report s c)))         ; what the view shows
      (is-true (search "Babylon" report))
      (is-true (search "walls" report)))
    (apply-command s (list :investigate :unit (unit-id dip)))
    (is-false (unit-by-id s (unit-id dip)))))           ; diplomat spent, no war

(test incite-revolt-flips-city
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Babylon" :owner 2 :x 3 :y 3))
         (g (add-unit s :phalanx 2 3 3))               ; the city's garrison
         (me (player-by-id s 1)))
    (setf (city-size c) 2 (player-gold me) 500)        ; cost = 2*50 = 100
    ;; one phalanx in the city makes it catchable -- attempt until it flips
    (let ((flipped nil))
      (loop for i below 40 until flipped do
        (let ((d (add-unit s :diplomat 1 3 2)))
          (handler-case
              (progn (apply-command s (list :incite-revolt :unit (unit-id d)))
                     (setf flipped t))
            (command-error () nil))))
      (is-true flipped)
      (is (= 1 (city-owner c)))                         ; city is now mine
      (is (= 1 (unit-owner g)))                          ; so is its garrison
      (is (< (player-gold me) 500))                      ; gold spent
      (is-true (at-war-p s 1 2)))))

(test incite-cannot-touch-a-capital
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Babylon" :owner 2 :x 3 :y 3))
         (dip (add-unit s :diplomat 1 3 2))
         (me (player-by-id s 1)))
    (setf (city-buildings c) '(:palace) (player-gold me) 500)
    (signals command-error (apply-command s (list :incite-revolt :unit (unit-id dip))))
    (is (= 2 (city-owner c)))))

(test bribe-unit
  (let* ((s (bare-state 6 6))
         (target (add-unit s :legion 2 3 3))            ; lone enemy unit, adjacent
         (dip (add-unit s :diplomat 1 3 2))
         (me (player-by-id s 1)))
    (setf (player-gold me) 500)                          ; legion cost 20 -> bribe 40
    (apply-command s (list :bribe-unit :unit (unit-id dip)))
    (is (= 1 (unit-owner target)))                       ; the legion defected
    (is (= 460 (player-gold me)))
    (is-false (unit-by-id s (unit-id dip)))               ; diplomat spent
    (is-true (at-war-p s 1 2))))

;;; --- caravans (trade routes, help build wonder) ----------------------------

(test caravan-helps-wonder
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Rome" :owner 1 :x 3 :y 3))
         (car (add-unit s :caravan 1 3 2)))
    (setf (city-production c) '(:wonder :pyramids) (city-shield-box c) 10)
    (apply-command s (list :help-wonder :unit (unit-id car)))
    (is (= 60 (city-shield-box c)))                ; +50 (the caravan's cost)
    (is-false (unit-by-id s (unit-id car)))))       ; caravan spent

(test help-wonder-needs-a-wonder-in-your-city
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Rome" :owner 1 :x 3 :y 3)))
    (setf (city-production c) '(:unit :warriors))   ; not a wonder
    (signals command-error
      (apply-command s (list :help-wonder :unit (unit-id (add-unit s :caravan 1 3 2)))))
    ;; an enemy city building a wonder is not yours -> refused
    (let ((e (civ-model::register-city s :name "Babylon" :owner 2 :x 3 :y 5)))
      (setf (city-production e) '(:wonder :pyramids))
      (signals command-error
        (apply-command s (list :help-wonder :unit (unit-id (add-unit s :caravan 1 3 4))))))))

(test caravan-establishes-trade-route
  (let* ((s (bare-state 12 8))
         (origin (civ-model::register-city s :name "Rome" :owner 1 :x 1 :y 4))
         (dest (civ-model::register-city s :name "Babylon" :owner 2 :x 8 :y 4))
         (car (add-unit s :caravan 1 7 4))          ; adjacent to dest
         (me (player-by-id s 1)))
    (setf (player-gold me) 0)
    (apply-command s (list :trade-route :unit (unit-id car)))
    (is (plusp (player-gold me)))                   ; one-time revenue
    (is-false (unit-by-id s (unit-id car)))          ; caravan spent
    (is (= 1 (civ-model::city-route-count s (city-id dest))))
    (is (= 1 (civ-model::city-route-count s (city-id origin))))
    ;; the same pair can't be linked twice
    (signals command-error
      (apply-command s (list :trade-route :unit (unit-id (add-unit s :caravan 1 7 4)))))))

(test trade-route-adds-trade
  (let* ((s (bare-state 6 6))
         (a (civ-model::register-city s :name "A" :owner 1 :x 1 :y 1))
         (b (civ-model::register-city s :name "B" :owner 1 :x 4 :y 4)))
    (let ((t0 (nth-value 2 (civ-model::city-yields s a))))
      (civ-model::add-route s (city-id a) (city-id b))
      (is (= (1+ t0) (nth-value 2 (civ-model::city-yields s a)))))))   ; +1 trade

;;; --- victory ---------------------------------------------------------------

(test conquest-victory
  ;; with one civ wiped out (no cities, no units), the survivor wins
  (let* ((s (bare-state 6 6)))
    (add-unit s :warriors 1 1 1)
    (civ-model::register-city s :name "Rome" :owner 1 :x 2 :y 2)
    ;; player 2 has nothing -> eliminated
    (civ-model::process-victory s)
    (is (eql 1 (gs-winner s)))
    (is (eq :conquest (gs-victory s)))))

(test no-conquest-while-rivals-survive
  (let* ((s (bare-state 6 6)))
    (add-unit s :warriors 1 1 1)
    (add-unit s :warriors 2 4 4)            ; rival still has a unit
    (civ-model::process-victory s)
    (is-false (gs-winner s))))

(test apollo-reveals-the-map
  (let* ((s (bare-state 6 6)) (st (add-unit s :settlers 1 2 2)) (p (player-by-id s 1)))
    (apply-command s (list :found-city :unit (unit-id st) :name "Rome"))
    (let ((c (a-city s)))
      (is-false (civ-model::seen-p s p 5 5))               ; a far tile is unexplored
      (setf (city-production c) '(:wonder :apollo-program) (city-shield-box c) 9999)
      (civ-model::city-try-complete s c)
      (is-true (member :apollo-program (city-buildings c)))
      ;; the whole map is now seen -- for every civilization, not just the builder
      (is-true (civ-model::seen-p s p 5 5))
      (is-true (civ-model::seen-p s (player-by-id s 2) 0 0)))))

(test spaceship-part-requires-apollo-and-tech
  (let* ((s (bare-state 6 6))
         (c (civ-model::register-city s :name "Rome" :owner 1 :x 2 :y 2))
         (p (player-by-id s 1)))
    (signals command-error (apply-command s (list :set-production :city (city-id c)
                                                  :item '(:spaceship))))   ; no Apollo yet
    (push :apollo-program (city-buildings c))               ; Apollo now built
    (signals command-error (apply-command s (list :set-production :city (city-id c)
                                                  :item '(:spaceship))))   ; still no space-flight
    (setf (gethash :space-flight (player-techs p)) t)
    (signals command-error (apply-command s (list :set-production :city (city-id c)
                                                  :item '(:spaceship))))   ; still no fusion-power
    (setf (gethash :fusion-power (player-techs p)) t)       ; the ship's fuel
    (apply-command s (list :set-production :city (city-id c) :item '(:spaceship)))
    (is (equal '(:spaceship) (civ-model::city-production c)))))

(test space-race-victory
  (let* ((s (bare-state 6 6))
         (p (player-by-id s 1)))
    (civ-model::register-city s :name "Rome" :owner 1 :x 2 :y 2)   ; keep player 1 alive
    (add-unit s :warriors 2 4 4)                                    ; keep player 2 alive (no conquest)
    (setf (player-spaceship p) civ-model::*spaceship-parts*)        ; ship complete
    (civ-model::process-victory s)                                  ; launches
    (is (plusp (player-landing p)))
    (is-false (gs-winner s))                                        ; still in flight
    (setf (gs-turn s) (+ (player-landing p) 1))                     ; arrival time passes
    (civ-model::process-victory s)
    (is (eql 1 (gs-winner s)))
    (is (eq :space (gs-victory s)))))

;;; --- save / load -----------------------------------------------------------

(test save-load-roundtrip
  (let ((s (make-new-game :seed 9)))
    (dotimes (i 5) (end-turn s))
    (uiop:with-temporary-file (:pathname path :type "lisp")
      (save-game s path)
      (let ((s2 (load-game path)))
        (is (= (gs-turn s) (gs-turn s2)))
        (is (= (gs-year s) (gs-year s2)))
        (is (= (hash-table-count (gs-units s)) (hash-table-count (gs-units s2))))
        (is (= (hash-table-count (gs-cities s)) (hash-table-count (gs-cities s2))))
        (is (equal (unit-positions s) (unit-positions s2)))
        (is (eq (tile-terrain (tile-at (gs-map s) 3 3))
                (tile-terrain (tile-at (gs-map s2) 3 3))))
        ;; deterministic RNG: a loaded game rolls identically
        (is (= (gs-rand s 1000000) (gs-rand s2 1000000)))))))

(test save-load-preserves-detail
  (let* ((s (bare-state 6 6))
         (u (add-unit s :settlers 1 2 2))
         (p (player-by-id s 1)))
    (setf (gethash :pottery (player-techs p)) t
          (gethash :monarchy (player-techs p)) t
          (player-gold p) 42)
    (apply-command s (list :build-road :unit (unit-id u)))   ; job in progress
    (apply-command s (list :set-government :player 1 :to :monarchy)) ; revolution pending
    (apply-command s (list :set-rates :player 1 :tax 50 :luxury 10 :science 40))
    (civ-model::register-city s :name "Rome" :owner 1 :x 4 :y 4)
    (uiop:with-temporary-file (:pathname path :type "lisp")
      (save-game s path)
      (let* ((s2 (load-game path))
             (u2 (a-unit s2 1 :settlers))
             (p2 (player-by-id s2 1)))
        (is (eq :build-road (unit-work u2)))
        (is (= (unit-work-left u) (unit-work-left u2)))
        (is-true (player-has-tech-p p2 :pottery))
        (is (= 42 (player-gold p2)))
        (is (eq :anarchy (player-government p2)))         ; revolution preserved
        (is (eq :monarchy (player-gov-target p2)))
        (is (= 1 (player-anarchy-left p2)))
        (is (= 10 (player-luxury-rate p2)))
        (is-true (city-named s2 "Rome"))
        ;; tile occupancy is rebuilt from the entity lists
        (is (member (unit-id u2) (tile-units (tile-at (gs-map s2) 2 2))))
        (is (= (city-id (city-named s2 "Rome"))
               (tile-city (tile-at (gs-map s2) 4 4))))))))
