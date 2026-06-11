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
    (is (null (tile-at m 5 0)))
    (is (eq :grassland (tile-terrain (tile-at m 2 2))))))

(test neighbor-counts
  (let ((m (civ-model::make-game-map 5 5)))
    (is (= 3 (length (neighbors m 0 0))))    ; corner
    (is (= 5 (length (neighbors m 2 0))))    ; edge
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
    (signals command-error (apply-command s (list :move-unit :unit (unit-id u) :dx -1 :dy 0)))
    (signals command-error (apply-command s (list :move-unit :unit (unit-id u) :dx 2 :dy 0)))
    (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))
    (signals command-error (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0)))))

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
    (add-unit s :warriors 2 3 2)
    (is-true (enemy-adjacent-p s 2 2 1))
    (is-false (enemy-adjacent-p s 0 0 1))))

(test zoc-blocks-slip
  (let ((s (bare-state 6 6)))
    (add-unit s :warriors 2 3 1)
    (add-unit s :warriors 2 3 3)
    (let ((u (add-unit s :legion 1 3 2)))
      (signals command-error
        (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))))))

(test zoc-attack-allowed
  (let ((s (bare-state 6 6 :seed 1)))
    (add-unit s :warriors 2 3 1)
    (let ((d (add-unit s :warriors 2 3 3))
          (u (add-unit s :legion 1 3 2)))
      (finishes (apply-command s (list :move-unit :unit (unit-id u) :dx 0 :dy 1)))
      (is (null (unit-by-id s (unit-id d)))))))

(test zoc-friendly-tile-exempt
  (let ((s (bare-state 6 6)))
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
    (is (= -3960 (gs-year s)))))

(test research-progresses
  (let ((s (make-new-game :seed 11)))
    (apply-command s (list :found-city :unit (unit-id (a-unit s 1 :settlers)) :name "Rome"))
    (dotimes (i 40) (end-turn s))
    (is (>= (hash-table-count (player-techs (player-by-id s 1))) 1))))

(test city-grows
  (let (c (s (make-new-game :seed 11)))
    (apply-command s (list :found-city :unit (unit-id (a-unit s 1 :settlers)) :name "Rome"))
    (setf c (a-city s 1))
    (dotimes (i 30) (end-turn s))
    (is (>= (city-size c) 2))))

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
  ;; full ocean wall across x=3 -> no route
  (let ((s (bare-state 8 6)))
    (dotimes (y 6) (terrain! s 3 y :ocean))
    (is (null (find-path s 1 1 6 1 1)))))

(test goto-moves-immediately
  ;; issuing :goto advances the unit the same turn (responsive UI), not only on end-turn
  (let* ((s (bare-state 12 6))
         (u (add-unit s :legion 1 1 3)))
    (apply-command s (list :goto :unit (unit-id u) :x 8 :y 3))
    (is (> (unit-x u) 1))                    ; already stepped toward the target
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
