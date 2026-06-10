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

(defun unit-positions (state)
  (sort (loop for u being the hash-values of (gs-units state)
              collect (list (unit-id u) (unit-type u) (unit-x u) (unit-y u)))
        #'< :key #'first))

;;; --- definitions & map -----------------------------------------------------

(test terrain-and-unit-defs
  (is (= 2 (terrain-def :grassland :food)))
  (is (= 2 (terrain-def :forest :shields)))
  (is (= 50 (terrain-def :hills :defense)))
  (is (= 0 (unit-def :settlers :attack)))
  (is (= 4 (unit-def :legion :attack)))
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

(test fortify-and-clear
  (let* ((s (bare-state 6 6)) (u (add-unit s :warriors 1 2 2)))
    (apply-command s (list :fortify :unit (unit-id u)))
    (is (eq :fortified (unit-orders u)))
    (apply-command s (list :move-unit :unit (unit-id u) :dx 1 :dy 0))
    (is (eq :idle (unit-orders u)))))           ; moving breaks fortify

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
