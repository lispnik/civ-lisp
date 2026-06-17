;;;; commands.lisp -- the only sanctioned way to change the state.
;;;;
;;;; A command is a tagged plist describing a player's intent, e.g.
;;;;   (:move-unit :unit 3 :dx 1 :dy 0)
;;;;   (:found-city :unit 3 :name "Rome")
;;;;   (:set-production :city 7 :item (:building :library))
;;;;   (:end-turn)
;;;; APPLY-COMMAND validates it against the state and applies it (or signals a
;;;; COMMAND-ERROR).  Routing all change through one entry point is what gives
;;;; the model undo, replays, networking and a clean AI seam.

(in-package #:civ-model)

(define-condition command-error (error)
  ((message :initarg :message :reader command-error-message))
  (:report (lambda (c s) (format s "~A" (command-error-message c)))))

(defun fail (fmt &rest args)
  (error 'command-error :message (apply #'format nil fmt args)))

(defun apply-command (state command)
  "Validate and apply COMMAND to STATE.  Returns STATE; signals COMMAND-ERROR
on an illegal move."
  (ecase (first command)
    (:move-unit      (cmd-move-unit state command))
    (:found-city     (cmd-found-city state command))
    (:set-production (cmd-set-production state command))
    (:set-specialists (cmd-set-specialists state command))
    (:work-tile      (cmd-work-tile state command))
    (:fortify        (cmd-fortify state command))
    (:wake           (cmd-wake state command))
    (:disband-unit   (cmd-disband-unit state command))
    (:upgrade-unit   (cmd-upgrade-unit state command))
    (:nuke           (cmd-nuke state command))
    (:goto           (cmd-goto state command))
    (:explore        (cmd-explore state command))
    ((:build-road :build-railroad :irrigate :mine :build-fort :build-airbase :clear-forest)
     (cmd-terraform state command))
    (:clean-pollution (cmd-clean-pollution state command))
    (:set-rates      (cmd-set-rates state command))
    (:set-government (cmd-set-government state command))
    (:declare-war    (cmd-declare-war state command))
    (:make-peace     (cmd-make-peace state command))
    (:propose-alliance (cmd-propose-alliance state command))
    (:break-alliance (cmd-break-alliance state command))
    (:propose-ceasefire (cmd-propose-ceasefire state command))
    (:demand-tribute (cmd-demand-tribute state command))
    (:resolve-offer  (cmd-resolve-offer state command))
    (:propose-trade  (cmd-propose-trade state command))
    (:set-research   (cmd-set-research state command))
    (:steal-tech     (cmd-steal-tech state command))
    (:sabotage       (cmd-sabotage state command))
    (:establish-embassy (cmd-establish-embassy state command))
    (:investigate    (cmd-investigate state command))
    (:incite-revolt  (cmd-incite-revolt state command))
    (:bribe-unit     (cmd-bribe-unit state command))
    (:help-wonder    (cmd-help-wonder state command))
    (:trade-route    (cmd-trade-route state command))
    (:end-turn       (end-turn state)))
  state)

(defun clear-work (u)
  "Abandon any in-progress terraform job on U."
  (setf (unit-work u) nil (unit-work-left u) 0))

;; defined in pathfind.lisp (loaded later)
(declaim (ftype (function (t t) t) advance-goto advance-explore))

(defun cmd-explore (state command)
  "Set a unit to auto-explore: each turn it heads for the nearest unexplored tile
(PROCESS-EXPLORE), stopping when nothing's left, it's blocked, or it sights
another civilization.  Land and sea units only."
  (let ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit"))))
    (when (eq (unit-def (unit-type u) :domain) :air)
      (fail "air units can't auto-explore"))
    (clear-work u)
    (setf (unit-orders u) :explore (unit-goto-x u) nil (unit-goto-y u) nil)
    (advance-explore state u)         ; set off now, don't wait for end-of-turn
    u))

(defun cmd-goto (state command)
  "Set a unit's destination and start moving it there immediately (and on each
following turn via PROCESS-GOTO)."
  (let* ((args (rest command))
         (u (or (unit-by-id state (getf args :unit)) (fail "no such unit")))
         (tx (getf args :x)) (ty (getf args :y)))
    (unless (tile-at (gs-map state) tx ty) (fail "goto target out of bounds"))
    (clear-work u)
    (setf (unit-goto-x u) tx (unit-goto-y u) ty (unit-orders u) :goto)
    (advance-goto state u)          ; move now, don't wait for end-of-turn
    u))

(defun cmd-fortify (state command)
  "Order a unit to fortify: it digs in for a defense bonus and faster healing
until it next moves."
  (let ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit"))))
    (setf (unit-orders u) :fortified)
    (clear-work u)
    u))

(defun cmd-wake (state command)
  "Clear a unit's standing orders (fortify/goto/terraform), making it active again."
  (let ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit"))))
    (setf (unit-orders u) :idle (unit-goto-x u) nil (unit-goto-y u) nil)
    (clear-work u)
    u))

;;; --- diplomat espionage ----------------------------------------------------

(defun adjacent-enemy-city (state unit)
  "An enemy city on UNIT's own tile or a neighbouring tile, or NIL."
  (let ((map (gs-map state)) (owner (unit-owner unit)))
    (flet ((enemy-city-at (x y)
             (let* ((tile (tile-at map x y)) (cid (and tile (tile-city tile)))
                    (c (and cid (city-by-id state cid))))
               (and c (/= (city-owner c) owner) c))))
      (or (enemy-city-at (unit-x unit) (unit-y unit))
          (loop for (x y tile) in (neighbors map (unit-x unit) (unit-y unit))
                do (progn tile)
                thereis (enemy-city-at x y))))))

(defun espionage-target (state command)
  "Validate an espionage order: returns (values diplomat enemy-city)."
  (let ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit"))))
    (unless (member :espionage (unit-def (unit-type u) :abilities))
      (fail "~(~A~) cannot perform espionage" (unit-type u)))
    (values u (or (adjacent-enemy-city state u)
                  (fail "no enemy city next to the diplomat")))))

(defun espionage-caught-p (state city diplomat)
  "Civ1-style catch roll: an undefended city is a free target; a garrisoned or
walled one may catch the spy (likelier the more it is defended), and a veteran
diplomat halves the risk."
  (let ((garrison (count-city-military state city))
        (walls (or (member :walls (city-buildings city))
                   (member :great-wall (city-buildings city)))))
    (if (and (zerop garrison) (not walls))
        nil                                       ; undefended: the spy slips in
        (let ((chance (min 90 (+ 20 (* 25 garrison) (if walls 20 0)))))
          (when (unit-veteran diplomat) (setf chance (floor chance 2)))
          (< (gs-rand state 100) chance)))))

(defun cmd-steal-tech (state command)
  "A diplomat steals an advance the adjacent enemy city's owner has and we lack,
then is spent.  A defended city may catch the spy (lost, and war); a second
theft from the same civilization also provokes war."
  (multiple-value-bind (u c) (espionage-target state command)
    (let* ((thief (player-by-id state (unit-owner u)))
           (victim (player-by-id state (city-owner c)))
           (tid (player-id thief)) (vid (player-id victim))
           (tech (a-tech-other-lacks state victim thief)))
      (unless tech (fail "~A has no advance worth stealing" (player-name victim)))
      (when (espionage-caught-p state c u)
        (destroy-unit state u)
        (setf (relation state tid vid) :war)
        (fail "your diplomat was caught and executed -- ~A declares war!"
              (player-name victim)))
      (setf (gethash tech (player-techs thief)) t)
      (destroy-unit state u)               ; the diplomat is consumed
      (let ((k (+ (* tid 256) vid)))        ; stealing twice from a civ means war
        (if (gethash k (gs-stolen state))
            (setf (relation state tid vid) :war)
            (setf (gethash k (gs-stolen state)) t)))
      tech)))

(defun cmd-sabotage (state command)
  "A diplomat wrecks the adjacent enemy city's priciest improvement (or, if it
has none, its current production), then is spent.  A defended city may catch the
spy; either way sabotage is an overt act and provokes war."
  (multiple-value-bind (u c) (espionage-target state command)
    (let ((tid (unit-owner u)) (vid (city-owner c)))
      (when (espionage-caught-p state c u)
        (destroy-unit state u)
        (setf (relation state tid vid) :war)
        (fail "your diplomat was caught -- war!"))
      (let ((target (first (sort (remove-if-not
                                  (lambda (b) (and (gethash b *buildings*)
                                                   (not (eq b :palace))))
                                  (copy-list (city-buildings c)))
                                 #'> :key (lambda (b) (building-def b :cost 0))))))
        (if target
            (setf (city-buildings c) (remove target (city-buildings c)))
            (setf (city-shield-box c) 0))     ; nothing built: wreck the production
        (destroy-unit state u)
        (setf (relation state tid vid) :war)  ; sabotage is overt -> war
        (or target :production)))))

(defun has-embassy-p (state observer observed)
  "T if OBSERVER has an embassy with OBSERVED."
  (gethash (+ (* observer 256) observed) (gs-embassies state)))

(defun cmd-establish-embassy (state command)
  "A diplomat establishes a permanent embassy with the adjacent city's owner --
a benign act (no catch, no war) that opens that civilization's intelligence."
  (multiple-value-bind (u c) (espionage-target state command)
    (setf (gethash (+ (* (unit-owner u) 256) (city-owner c)) (gs-embassies state)) t)
    (destroy-unit state u)
    c))

(defun city-report (state city)
  "A human-readable intelligence summary of CITY (for INVESTIGATE)."
  (format nil "~A sz~D (~A) ~Adef~Abuild ~A"
          (city-name city) (city-size city)
          (player-name (player-by-id state (city-owner city)))
          (count-city-military state city)
          (if (member :walls (city-buildings city)) " walls " " ")
          (if (city-production city)
              (format nil "~(~A~)" (second (city-production city))) "idle")))

(defun cmd-investigate (state command)
  "A diplomat inspects the adjacent enemy city (the caller reads CITY-REPORT),
then is spent.  A peek, so no war."
  (multiple-value-bind (u c) (espionage-target state command)
    (destroy-unit state u)
    c))

(defun incite-cost (city)
  (* (max 1 (city-size city)) 50))

(defun cmd-incite-revolt (state command)
  "Bribe an adjacent enemy city (and its garrison) to defect for gold; the
diplomat is spent and the victim goes to war.  Capitals cannot be incited, and a
defended city may catch the spy."
  (multiple-value-bind (u c) (espionage-target state command)
    (let* ((briber (player-by-id state (unit-owner u)))
           (vid (city-owner c))
           (cost (incite-cost c)))
      (when (member :palace (city-buildings c))
        (fail "a capital cannot be incited to revolt"))
      (when (< (player-gold briber) cost)
        (fail "inciting ~A costs ~D gold" (city-name c) cost))
      (when (espionage-caught-p state c u)
        (destroy-unit state u)
        (setf (relation state (player-id briber) vid) :war)
        (fail "your diplomat was caught -- war!"))
      (decf (player-gold briber) cost)
      (let ((tile (tile-at (gs-map state) (city-x c) (city-y c))))
        (dolist (id (copy-list (tile-units tile)))      ; the garrison defects too
          (let ((gu (unit-by-id state id)))
            (when gu (setf (unit-owner gu) (player-id briber)))))
        (setf (city-owner c) (player-id briber)
              (tile-owner tile) (player-id briber)))
      (destroy-unit state u)
      (setf (relation state (player-id briber) vid) :war)
      c)))

(defun adjacent-enemy-unit (state unit)
  "A lone enemy unit (not garrisoned in a city) on a tile adjacent to UNIT, or
NIL."
  (let ((map (gs-map state)) (owner (unit-owner unit)))
    (loop for (x y tile) in (neighbors map (unit-x unit) (unit-y unit))
          unless (tile-city tile)
            do (loop for id in (tile-units tile)
                     for e = (unit-by-id state id)
                     when (and e (/= (unit-owner e) owner))
                       do (return-from adjacent-enemy-unit e)))))

(defun cmd-bribe-unit (state command)
  "Bribe a lone adjacent enemy unit to defect for gold; the diplomat is spent and
the victim goes to war."
  (let ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit"))))
    (unless (member :espionage (unit-def (unit-type u) :abilities))
      (fail "~(~A~) cannot bribe" (unit-type u)))
    (let* ((target (or (adjacent-enemy-unit state u) (fail "no lone enemy unit to bribe")))
           (briber (player-by-id state (unit-owner u)))
           (vid (unit-owner target))
           (cost (* 2 (unit-def (unit-type target) :cost 10))))
      (when (< (player-gold briber) cost)
        (fail "bribing that ~(~A~) costs ~D gold" (unit-type target) cost))
      (decf (player-gold briber) cost)
      (setf (unit-owner target) (player-id briber))
      (destroy-unit state u)
      (setf (relation state (player-id briber) vid) :war)
      target)))

;;; --- caravan (trade routes, help build wonder) -----------------------------

(defun adjacent-city-if (state unit pred)
  "First city on UNIT's tile or a neighbour satisfying PRED, or NIL."
  (let ((map (gs-map state)))
    (flet ((city-at (x y)
             (let* ((tl (tile-at map x y)) (cid (and tl (tile-city tl)))
                    (c (and cid (city-by-id state cid))))
               (and c (funcall pred c) c))))
      (or (city-at (unit-x unit) (unit-y unit))
          (loop for (x y tile) in (neighbors map (unit-x unit) (unit-y unit))
                do (progn tile) thereis (city-at x y))))))

(defun nearest-own-city-other (state pid x y exclude-id)
  "PID's city nearest (X,Y) other than EXCLUDE-ID, or NIL."
  (let (best bestd)
    (maphash (lambda (id c) (declare (ignore id))
               (when (and (= (city-owner c) pid) (/= (city-id c) exclude-id))
                 (let ((d (max (map-dx (gs-map state) (city-x c) x) (abs (- (city-y c) y)))))
                   (when (or (null bestd) (< d bestd)) (setf best c bestd d)))))
             (gs-cities state))
    best))

(defun cmd-help-wonder (state command)
  "A caravan in/next to one of your cities that is building a wonder adds its
shields to that wonder, then is spent."
  (let ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit"))))
    (unless (member :caravan (unit-def (unit-type u) :abilities))
      (fail "~(~A~) cannot help build a wonder" (unit-type u)))
    (let ((c (or (adjacent-city-if state u (lambda (c) (= (city-owner c) (unit-owner u))))
                 (fail "no city of yours next to the caravan"))))
      (unless (eq (first (city-production c)) :wonder)
        (fail "~A is not building a wonder" (city-name c)))
      (incf (city-shield-box c) (unit-def (unit-type u) :cost 50))
      (destroy-unit state u)
      c)))

(defun cmd-trade-route (state command)
  "A caravan next to a city opens a trade route between that city and its owner's
nearest other city, paying a one-time gold windfall (and recurring trade)."
  (let ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit"))))
    (unless (member :caravan (unit-def (unit-type u) :abilities))
      (fail "~(~A~) cannot establish a trade route" (unit-type u)))
    (let* ((dest (or (adjacent-city-if state u (constantly t))
                     (fail "no city next to the caravan")))
           (oid (unit-owner u))
           (origin (or (nearest-own-city-other state oid (unit-x u) (unit-y u) (city-id dest))
                       (fail "you have no other city to trade from"))))
      (when (route-exists-p state (city-id origin) (city-id dest))
        (fail "a trade route already links ~A and ~A" (city-name origin) (city-name dest)))
      (when (or (>= (city-route-count state (city-id origin)) 3)
                (>= (city-route-count state (city-id dest)) 3))
        (fail "a city already has three trade routes"))
      (add-route state (city-id origin) (city-id dest))
      (let ((revenue (* 3 (+ (+ (map-dx (gs-map state) (city-x origin) (city-x dest))
                                (abs (- (city-y origin) (city-y dest))))
                             (city-size origin) (city-size dest)))))
        (incf (player-gold (player-by-id state oid)) revenue))
      (destroy-unit state u)
      dest)))

(defun cmd-terraform (state command)
  "Order a settler to begin a terraform job (:build-road/:irrigate/:mine) on the
tile it stands on.  The job takes several turns (see *TERRAFORM*); the unit holds
position until PROCESS-TERRAFORM finishes it and sets the tile improvement."
  (let* ((job (first command))
         (u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit")))
         (tile (tile-at (gs-map state) (unit-x u) (unit-y u)))
         (terr (tile-terrain tile))
         (flag (terraform-def job :flag)))
    (unless (member :terraform (unit-def (unit-type u) :abilities))
      (fail "~(~A~) cannot build terrain improvements" (unit-type u)))
    (unless (member terr (terraform-def job :terrains))
      (fail "can't do ~A on ~(~A~)" (terraform-def job :verb) terr))
    (when (and flag (tile-improvement tile flag))
      (fail "~A already here" (terraform-def job :verb)))
    (let ((req (terraform-def job :requires)))      ; tech gate (e.g. railroad)
      (when (and req (not (player-has-tech-p (player-by-id state (unit-owner u)) req)))
        (fail "~A requires the ~(~A~) advance" (terraform-def job :verb) req)))
    (let ((needs (terraform-def job :needs)))        ; prerequisite improvement
      (when (and needs (not (tile-improvement tile needs)))
        (fail "~A needs a ~(~A~) first" (terraform-def job :verb) needs)))
    (when (<= (unit-moves-left u) 0) (fail "unit has no moves left"))
    (setf (unit-work u) job
          (unit-work-left u) (terraform-def job :turns)
          (unit-orders u) :idle
          (unit-moves-left u) 0)            ; busy: no movement while working
    u))

;;; --- tribal huts (goody huts) ----------------------------------------------

(defparameter *hut-mercenaries* '(:legion :phalanx :musketeers :cavalry)
  "Unit types that can emerge from a hut as friendly 'mercenaries'.")

(defun barbarian-player (state)
  (find :barbarian (gs-players state) :key #'player-kind))

(defun city-near-p (state x y radius)
  "T if any city lies within RADIUS tiles (chebyshev, x wrapped) of (X,Y)."
  (let ((map (gs-map state)))
    (loop for c being the hash-values of (gs-cities state)
          thereis (and (<= (abs (signed-dx map (city-x c) x)) radius)
                       (<= (abs (- (city-y c) y)) radius)))))

(defun hut-gold (state player)
  (let ((g (+ 25 (gs-rand state 76))))      ; 25-100
    (incf (player-gold player) g)
    (format nil "You find ~D gold in an ancient hut." g)))

(defun enter-hut (state u tile)
  "U (a non-barbarian unit) has stepped onto a hut TILE: clear the hut and roll
an outcome -- gold, a free advance, mercenaries, wandering settlers, a friendly
city, or a barbarian ambush.  Stores the outcome in GS-MESSAGE and returns it."
  (setf (tile-hut tile) nil)
  (let* ((owner (unit-owner u))
         (p (player-by-id state owner))
         (x (unit-x u)) (y (unit-y u))
         (research (researchable-techs p))
         (roll (gs-rand state 100))
         (msg
          (cond
            ;; near your own empire: just friendly scouts bearing gold (no
            ;; cities/barbarians spawned right next to your capital)
            ((city-near-p state x y 3) (hut-gold state p))
            ((< roll 35) (hut-gold state p))
            ;; free advance
            ((and (< roll 55) research)
             (let ((tech (nth (gs-rand state (length research)) research)))
               (setf (gethash tech (player-techs p)) t)
               (format nil "Ancient scrolls teach you ~A!" (tech-def tech :name))))
            ;; mercenaries: a free military unit
            ((< roll 70)
             (let ((type (nth (gs-rand state (length *hut-mercenaries*)) *hut-mercenaries*)))
               (register-unit state :type type :owner owner :x x :y y)
               (format nil "~A emerge from the hut to join you!"
                       (string-capitalize (symbol-name type)))))
            ;; wandering nomads -> free settlers
            ((< roll 85)
             (register-unit state :type :settlers :owner owner :x x :y y)
             "Wandering nomads join you as settlers.")
            ;; an advanced tribe joins -> a new city on the hut tile
            ((and (< roll 95) (not (tile-city tile)))
             (let ((c (register-city state :name "Village" :owner owner :x x :y y)))
               (setf (city-production c) '(:unit :warriors)))
             "An advanced tribe joins your civilization!")
            ;; otherwise: barbarian ambush (falls back to gold with no barbarians)
            (t (let ((barb (barbarian-player state)))
                 (if barb
                     (progn
                       (dolist (n (remove-if (lambda (nb) (eq (tile-terrain (third nb)) :ocean))
                                             (neighbors (gs-map state) x y)))
                         (when (and (null (tile-units (third n))) (null (tile-city (third n)))
                                    (< (gs-rand state 2) 1))   ; ~half the open tiles
                           (register-unit state :type :legion :owner (player-id barb)
                                          :x (first n) :y (second n))))
                       "Barbarian raiders burst from the hut!")
                     (hut-gold state p)))))))
    (setf (gs-message state) msg)
    msg))

(defparameter *disband-shield-divisor* 4
  "A unit disbanded in a friendly city refunds (build-cost / this) shields toward
its production.  4 = a quarter of the cost; the original Civilization refunded
nothing, so this is a deliberately modest convenience -- raise/lower to taste.")

(defun cmd-disband-unit (state command)
  "Remove a unit from the game.

Shield reallocation: a unit disbanded while standing in one of its owner's
cities returns build-cost / *DISBAND-SHIELD-DIVISOR* shields (rounded down, a
quarter by default) into that city's production box -- but only up to what the
city's current build still needs, so
the recovered shields can finish (but never overflow past) the item in
progress.  A unit with no current production simply banks the half-cost.  A unit
disbanded in the open field returns nothing -- those shields are lost.  A city
building a *wonder* also gains nothing: wonders can only be hurried by caravans
(see CMD-HELP-WONDER), never by recycling units.  The outcome is recorded in
GS-MESSAGE for the UI to report."
  (let* ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit")))
         (type (unit-type u))
         (tile (tile-at (gs-map state) (unit-x u) (unit-y u)))
         (cid (tile-city tile))
         (city (and cid (city-by-id state cid)))
         (name (string-capitalize (symbol-name type)))
         (recover 0))
    (when (and city (= (city-owner city) (unit-owner u))
               (not (eq (first (city-production city)) :wonder)))
      (let ((refund (floor (unit-def type :cost 0) *disband-shield-divisor*)))
        (when (plusp refund)
          (let ((cap (if (city-production city)
                         (production-cost (city-production city))
                         most-positive-fixnum)))
            (setf recover (max 0 (min refund (- cap (city-shield-box city)))))
            (incf (city-shield-box city) recover)))))
    (destroy-unit state u)
    (setf (gs-message state)
          (if (plusp recover)
              (format nil "~A disbanded: ~D shields to ~A." name recover (city-name city))
              (format nil "~A disbanded." name)))
    u))

;;; --- naval transport -------------------------------------------------------

(defun sea-transport-room-p (state tile owner)
  "T if OWNER has a land-carrying ship on TILE with spare room for one more land
unit (current land passengers < total transport capacity there)."
  (let ((cap 0) (load 0))
    (dolist (id (tile-units tile))
      (let ((p (unit-by-id state id)))
        (when p
          (cond ((and (= (unit-owner p) owner)
                      (eq (unit-def (unit-type p) :carries) :land))
                 (incf cap (unit-def (unit-type p) :capacity 0)))
                ((eq (unit-def (unit-type p) :domain) :land)
                 (incf load))))))            ; land units already aboard
    (> cap load)))

(defun carry-passengers (state old transport)
  "TRANSPORT has just moved to its new tile; bring up to its capacity of the cargo
left on OLD along with it (rides for free).  A land transport ferries land units,
a carrier ferries air units; only OCEAN passengers ride, so units sharing a
coastal *city* tile (a garrison, parked planes) are left behind."
  (let ((carries (unit-def (unit-type transport) :carries)))
    (when (and (eq (tile-terrain old) :ocean) carries)
      (let ((cap (unit-def (unit-type transport) :capacity 0))
            (dest (tile-at (gs-map state) (unit-x transport) (unit-y transport)))
            (moved 0))
        (dolist (id (copy-list (tile-units old)))
          (let ((p (unit-by-id state id)))
            (when (and p (< moved cap)
                       (eq (unit-def (unit-type p) :domain) carries))
              (setf (tile-units old) (remove id (tile-units old)))
              (push id (tile-units dest))
              (setf (unit-x p) (unit-x transport) (unit-y p) (unit-y transport))
              (incf moved)
              (reveal-around (player-seen (player-by-id state (unit-owner p)))
                             state (unit-x p) (unit-y p)))))))))

(defun upgrade-cost (from to)
  "Gold to upgrade unit FROM into TO: twice the build-cost difference (min 10)."
  (max 10 (* 2 (max 0 (- (unit-def to :cost 0) (unit-def from :cost 0))))))

(defun cmd-upgrade-unit (state command)
  "Upgrade an obsolete unit to its successor for gold, while in a friendly city.
Records the outcome in GS-MESSAGE."
  (let* ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit")))
         (from (unit-type u))
         (to (unit-def from :upgrade-to))
         (owner (player-by-id state (unit-owner u)))
         (tile (tile-at (gs-map state) (unit-x u) (unit-y u)))
         (city (and (tile-city tile) (city-by-id state (tile-city tile)))))
    (unless to (fail "~(~A~) has no upgrade" from))
    (unless (unit-obsolete-p owner from) (fail "~(~A~) is not obsolete yet" from))
    (unless (and city (= (city-owner city) (unit-owner u)))
      (fail "must be in one of your cities to upgrade"))
    (let ((cost (upgrade-cost from to)))
      (when (< (player-gold owner) cost)
        (fail "upgrading to ~(~A~) costs ~D gold" to cost))
      (decf (player-gold owner) cost)
      (setf (unit-type u) to
            (unit-moves-left u) 0)            ; the upgrade uses up its turn
      (setf (gs-message state)
            (format nil "~A upgraded to ~A for ~D gold."
                    (string-capitalize (symbol-name from))
                    (string-capitalize (symbol-name to)) cost))
      u)))

(defun maybe-capture (state u tile)
  "If land unit U has just entered an enemy city on TILE, capture (or raze) it.
Only land units take cities; ships and aircraft can't hold ground."
  (let ((cid (tile-city tile)))
    (when (and cid (eq (unit-def (unit-type u) :domain) :land))
      (let ((c (city-by-id state cid)))
        (when (and c (/= (city-owner c) (unit-owner u)))
          (capture-city state c (unit-owner u)))))))

(defun cmd-nuke (state command)
  "Detonate a nuclear missile, centred on its own tile (plus an optional DX/DY
offset).  Every unit on the centre tile and the eight around it is destroyed,
any city in the blast is devastated (population halved), and the ground is left
with fallout (pollution).  The missile is expended and every civilization caught
in the blast is now at war with the attacker."
  (let* ((args (rest command))
         (u (or (unit-by-id state (getf args :unit)) (fail "no such unit")))
         (map (gs-map state)))
    (unless (member :nuke (unit-def (unit-type u) :abilities))
      (fail "~(~A~) is not a nuclear weapon" (unit-type u)))
    (when (<= (unit-moves-left u) 0) (fail "unit has no moves left"))
    (let ((cx (wrap-x map (+ (unit-x u) (getf args :dx 0))))
          (cy (+ (unit-y u) (getf args :dy 0)))
          (owner (unit-owner u)))
      (unless (tile-at map cx cy) (fail "target out of bounds"))
      ;; SDI Defense in or beside the target shoots the missile down
      (when (loop for cell in (cons (list cx cy) (neighbors map cx cy))
                  for tl = (tile-at map (first cell) (second cell))
                  for cid = (and tl (tile-city tl))
                  thereis (and cid (member :sdi-defense
                                           (city-buildings (city-by-id state cid)))))
        (destroy-unit state u)
        (setf (gs-message state) "Nuclear missile shot down by SDI!")
        (return-from cmd-nuke t))
      (flet ((act-of-war (against)
               (when (and (/= against owner) (not (barbarian-id-p state against)))
                 (setf (relation state owner against) :war))))
        (destroy-unit state u)                       ; the missile is expended
        (dolist (cell (cons (list cx cy) (neighbors map cx cy)))
          (let ((tl (tile-at map (first cell) (second cell))))
            (when tl
              ;; vaporise every unit on the tile
              (dolist (id (copy-list (tile-units tl)))
                (let ((v (unit-by-id state id)))
                  (when v (act-of-war (unit-owner v)) (destroy-unit state v))))
              ;; devastate a city (population halved, never below 1)
              (let ((cid (tile-city tl)))
                (when cid
                  (let ((c (city-by-id state cid)))
                    (when c
                      (act-of-war (city-owner c))
                      (setf (city-size c) (max 1 (floor (city-size c) 2)))))))
              ;; fallout: pollution on the scorched land
              (unless (eq (tile-terrain tl) :ocean)
                (setf (tile-pollution tl) t))))))
      (setf (gs-message state) "Nuclear detonation!")
      t)))

(defun cmd-move-unit (state command)
  (let* ((args (rest command))
         (u (or (unit-by-id state (getf args :unit)) (fail "no such unit")))
         (dx (getf args :dx 0))
         (dy (getf args :dy 0))
         (map (gs-map state))
         (nx (wrap-x map (+ (unit-x u) dx)))   ; x wraps around the cylinder
         (ny (+ (unit-y u) dy)))
    (unless (<= 1 (max (abs dx) (abs dy)) 1) (fail "must move exactly one tile"))
    (unless (tile-at map nx ny) (fail "destination out of bounds"))   ; past a pole
    (when (<= (unit-moves-left u) 0) (fail "unit has no moves left"))
    (let* ((dest (tile-at map nx ny))
           (sea-dest (eq (tile-terrain dest) :ocean))
           (foreign (tile-enemies state dest (unit-owner u)))
           (hostiles (tile-hostiles state dest (unit-owner u)))
           (encity (let ((cid (tile-city dest)))      ; an enemy city on the dest tile
                     (and cid (let ((c (city-by-id state cid)))
                                (and (/= (city-owner c) (unit-owner u)) c))))))
      ;; terrain domain: land units can't enter ocean; sea units can't land;
      ;; air units go anywhere.  (rivers are land terrain, so land units cross them)
      (ecase (unit-def (unit-type u) :domain :land)
        (:sea (unless sea-dest (fail "naval unit can't move onto land")))
        ;; land units may enter the water only to board a friendly transport
        ;; with room; moving back onto land disembarks them
        (:land (when (and sea-dest
                          (not (sea-transport-room-p state dest (unit-owner u))))
                 (fail "land unit can't move into the water")))
        (:air nil))
      ;; can't enter a tile held by a civilization you're only at peace with
      (when (and foreign (not hostiles))
        (fail "at peace with ~(~A~); declare war first"
              (player-name (player-by-id state (unit-owner (first foreign))))))
      ;; ...nor walk into a peaceful neighbour's (undefended) city
      (when (and encity (not (at-war-p state (unit-owner u) (city-owner encity))))
        (fail "at peace with ~(~A~); declare war first"
              (player-name (player-by-id state (city-owner encity)))))
      (if hostiles
          ;; attack: fight the strongest defender, advance if the tile clears
          (progn
            (when (zerop (attack-strength u))
              (fail "~(~A~) cannot attack" (unit-type u)))
            (let* ((defender (first (sort hostiles #'>
                                          :key (lambda (e) (defense-strength state e)))))
                   (result (resolve-combat state u defender)))
              (when (eq result :attacker)
                (clear-work u)                      ; attacking breaks terraform
                (setf (unit-moves-left u) 0
                      (unit-orders u) :idle)        ; attacking breaks fortify
                (unless (tile-hostiles state dest (unit-owner u))
                  (let ((old (tile-at map (unit-x u) (unit-y u))))
                    (setf (tile-units old) (remove (unit-id u) (tile-units old)))
                    (push (unit-id u) (tile-units dest))
                    (setf (unit-x u) nx (unit-y u) ny)
                    (reveal-around (player-seen (player-by-id state (unit-owner u)))
                                   state nx ny)        ; clear fog as we advance
                    (maybe-capture state u dest))))    ; take the city we just cleared
              result))
          ;; normal move (possibly onto friendly units)
          (progn
            ;; zone of control: can't slip directly from one enemy-ZOC tile to
            ;; another, unless the destination has a friendly unit or is a city
            (when (and (enemy-adjacent-p state (unit-x u) (unit-y u) (unit-owner u))
                       (enemy-adjacent-p state nx ny (unit-owner u))
                       (null (tile-units dest))
                       (not (tile-city dest)))
              (fail "blocked by enemy zone of control"))
            (let ((cost (terrain-def (tile-terrain dest) :move 1))
                  (old (tile-at map (unit-x u) (unit-y u))))
              (setf (tile-units old) (remove (unit-id u) (tile-units old)))
              (push (unit-id u) (tile-units dest))
              (clear-work u)                        ; moving breaks terraform
              (setf (unit-x u) nx (unit-y u) ny
                    (unit-orders u) :idle)          ; moving breaks fortify
              (reveal-around (player-seen (player-by-id state (unit-owner u)))
                             state nx ny)            ; clear fog around the new tile
              ;; a ship that moved brings its embarked cargo along
              (when (plusp (unit-def (unit-type u) :capacity 0))
                (carry-passengers state old u))
              (decf (unit-moves-left u) (max 1 (min cost (unit-moves-left u))))
              ;; walking into an undefended enemy city takes it
              (maybe-capture state u dest)
              ;; stepping onto a tribal hut springs its surprise
              (when (and (tile-hut dest) (not (barbarian-id-p state (unit-owner u))))
                (enter-hut state u dest))
              u))))))

(defparameter *clean-pollution-turns* 3
  "Turns a settler spends cleaning a polluted tile.")

(defun cmd-clean-pollution (state command)
  "Order a settler standing on a polluted tile to clean it (takes several turns)."
  (let* ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit")))
         (tile (tile-at (gs-map state) (unit-x u) (unit-y u))))
    (unless (member :terraform (unit-def (unit-type u) :abilities))
      (fail "~(~A~) cannot clean pollution" (unit-type u)))
    (unless (tile-pollution tile) (fail "no pollution to clean here"))
    (when (<= (unit-moves-left u) 0) (fail "unit has no moves left"))
    (setf (unit-work u) :clean-pollution
          (unit-work-left u) *clean-pollution-turns*
          (unit-orders u) :idle
          (unit-moves-left u) 0)
    u))

(defun cmd-declare-war (state command)
  "Player :PLAYER declares war on player :AGAINST -- unless a senate (Republic or
Democracy) vetoes it."
  (let* ((args (rest command))
         (a (getf args :player 1)) (b (getf args :against)))
    (unless (and (player-by-id state a) (player-by-id state b)) (fail "no such player"))
    (when (= a b) (fail "can't declare war on yourself"))
    (when (senate-p state a)
      (fail "the Senate refuses to declare war (Republic/Democracy)"))
    (setf (relation state a b) :war)
    state))

(defun cmd-make-peace (state command)
  "Player :PLAYER makes peace with player :AGAINST (mutual, accepted at once)."
  (let* ((args (rest command))
         (a (getf args :player 1)) (b (getf args :against)))
    (unless (and (player-by-id state a) (player-by-id state b)) (fail "no such player"))
    (when (barbarian-id-p state b) (fail "barbarians never make peace"))
    (setf (relation state a b) :peace)
    state))

(defun ai-accepts-alliance-p (state proposer ai)
  "Whether AI accepts an alliance proposed by PROPOSER: a civ welcomes an ally at
least as strong as itself (protection); otherwise it depends on temperament --
a trusting builder allies readily, a warlike civ rarely."
  (let ((mine   (length (player-city-list state (player-id ai))))
        (theirs (length (player-city-list state (player-id proposer)))))
    (or (>= theirs mine)                                  ; an equal-or-stronger friend
        (< (gs-rand state 100) (ai-trait ai :ally-chance 35)))))

(defun cmd-propose-alliance (state command)
  "Player :PLAYER proposes an alliance to :AGAINST.  The two must be at peace
(make peace first if at war).  A human ally accepts at once; an AI weighs it."
  (let* ((args (rest command))
         (a (getf args :player 1)) (b (getf args :against))
         (pa (player-by-id state a)) (pb (player-by-id state b)))
    (unless (and pa pb) (fail "no such player"))
    (when (= a b) (fail "can't ally with yourself"))
    (when (or (eq (player-kind pa) :barbarian) (eq (player-kind pb) :barbarian))
      (fail "barbarians have no allies"))
    (when (at-war-p state a b) (fail "make peace before proposing an alliance"))
    (when (allied-p state a b) (fail "already allied"))
    (when (and (eq (player-kind pb) :ai) (not (ai-accepts-alliance-p state pa pb)))
      (fail "~A declines the alliance" (player-name pb)))
    (setf (relation state a b) :alliance)
    state))

(defun cmd-break-alliance (state command)
  "Player :PLAYER renounces its alliance with :AGAINST, reverting to plain peace."
  (let* ((args (rest command))
         (a (getf args :player 1)) (b (getf args :against)))
    (unless (and (player-by-id state a) (player-by-id state b)) (fail "no such player"))
    (unless (allied-p state a b) (fail "no alliance to break"))
    (setf (relation state a b) :peace)
    state))

(defun ai-accepts-ceasefire-p (state proposer ai)
  "Whether AI agrees to a cease-fire from PROPOSER: a civ that is not winning is
glad to pause the war; a winning one parleys only by temperament (a warlike civ
presses its advantage)."
  (let ((mine   (length (player-city-list state (player-id ai))))
        (theirs (length (player-city-list state (player-id proposer)))))
    (or (<= mine theirs)
        (< (gs-rand state 100) (- 70 (* 3 (ai-trait ai :war-chance 3)))))))

(defun cmd-propose-ceasefire (state command)
  "Player :PLAYER proposes a cease-fire to :AGAINST -- it ends the war and bars
either side from re-declaring for *CEASEFIRE-TURNS* turns.  The two must be at
war; an AI weighs it (and readily accepts when it is not winning)."
  (let* ((args (rest command))
         (a (getf args :player 1)) (b (getf args :against))
         (pa (player-by-id state a)) (pb (player-by-id state b)))
    (unless (and pa pb) (fail "no such player"))
    (when (barbarian-id-p state b) (fail "barbarians never parley"))
    (unless (at-war-p state a b) (fail "not at war"))
    (when (and (eq (player-kind pb) :ai) (not (ai-accepts-ceasefire-p state pa pb)))
      (fail "~A fights on" (player-name pb)))
    (setf (relation state a b) :peace)
    (set-truce state a b)
    state))

(defparameter *tribute-amount* 50 "Gold paid when a tribute demand succeeds.")

(defun ai-pays-tribute-p (state demander ai)
  "Whether AI yields tribute to DEMANDER: only a weaker civ with gold to spare
pays up, and a warlike one would sooner refuse."
  (let ((mine   (length (player-city-list state (player-id ai))))
        (theirs (length (player-city-list state (player-id demander)))))
    (and (plusp (player-gold ai))
         (< mine theirs)
         (< (gs-rand state 100) (- 80 (* 5 (ai-trait ai :war-chance 3)))))))

(defun cmd-demand-tribute (state command)
  "Player :PLAYER demands tribute from :AGAINST (with whom it is at peace).  A
weaker or fearful AI pays *TRIBUTE-AMOUNT* gold (capped at what it has); a
confident one refuses."
  (let* ((args (rest command))
         (a (getf args :player 1)) (b (getf args :against))
         (pa (player-by-id state a)) (pb (player-by-id state b)))
    (unless (and pa pb) (fail "no such player"))
    (when (barbarian-id-p state b) (fail "barbarians pay no tribute"))
    (when (at-war-p state a b) (fail "make peace before extorting tribute"))
    (when (and (eq (player-kind pb) :ai) (not (ai-pays-tribute-p state pa pb)))
      (fail "~A refuses your demand" (player-name pb)))
    (let ((amount (min *tribute-amount* (player-gold pb))))
      (decf (player-gold pb) amount)
      (incf (player-gold pa) amount))
    state))

(defun cmd-resolve-offer (state command)
  "The human accepts (:ACCEPT t) or declines a pending AI offer of :KIND from civ
:FROM.  Accepting applies the relation change; either way the offer is cleared."
  (let* ((args (rest command))
         (me (getf args :player 1)) (from (getf args :from)) (kind (getf args :kind)))
    (unless (player-by-id state from) (fail "no such player"))
    (when (getf args :accept)
      (ecase kind
        (:alliance  (setf (relation state from me) :alliance))
        (:ceasefire (setf (relation state from me) :peace) (set-truce state from me))))
    (remove-offer state from kind)
    state))

;;; --- trade (gold + tech) ---------------------------------------------------

(defparameter *tech-trade-value* 250
  "Gold-equivalent value of an advance when valuing a trade offer.")

(defun item-value (item)
  "Gold-equivalent worth of a trade item: (:gold N) or (:tech KEY)."
  (ecase (first item)
    (:gold (second item))
    (:tech *tech-trade-value*)))

(defun bundle-value (items)
  (reduce #'+ items :key #'item-value :initial-value 0))

(defun validate-bundle (state giver receiver items)
  "Signal a COMMAND-ERROR unless GIVER can hand each of ITEMS to RECEIVER."
  (declare (ignore state))
  (dolist (it items)
    (ecase (first it)
      (:gold (when (> (second it) (player-gold giver))
               (fail "~A can't afford ~D gold" (player-name giver) (second it))))
      (:tech (let ((k (second it)))
               (unless (player-has-tech-p giver k)
                 (fail "~A doesn't have ~(~A~)" (player-name giver) k))
               (when (player-has-tech-p receiver k)
                 (fail "~A already has ~(~A~)" (player-name receiver) k)))))))

(defun transfer-bundle (giver receiver items)
  "Move/share each of ITEMS from GIVER to RECEIVER (tech is shared, not lost)."
  (dolist (it items)
    (ecase (first it)
      (:gold (decf (player-gold giver) (second it))
             (incf (player-gold receiver) (second it)))
      (:tech (setf (gethash (second it) (player-techs receiver)) t)))))

(defun a-tech-other-lacks (state haver lacker)
  "An advance HAVER knows that LACKER does not (the lowest by name, for
determinism), or NIL."
  (declare (ignore state))
  (first (sort (loop for k being the hash-keys of (player-techs haver)
                     unless (player-has-tech-p lacker k) collect k)
               #'string< :key #'symbol-name)))

(defun best-trade-with (state me oid)
  "A (label . deal) the human ME can profitably offer civ OID, or NIL.  DEAL is
a (:give items :want items) plist.  Prefers a tech swap, then buying a tech for
gold, then selling one."
  (let* ((mine (player-by-id state me)) (them (player-by-id state oid))
         (i-want (a-tech-other-lacks state them mine))    ; tech they have, I lack
         (they-want (a-tech-other-lacks state mine them)));tech I have, they lack
    (cond
      ((and i-want they-want)
       (cons (format nil "swap ~(~A~) for ~(~A~)" they-want i-want)
             (list :give (list (list :tech they-want)) :want (list (list :tech i-want)))))
      ((and i-want (>= (player-gold mine) *tech-trade-value*))
       (cons (format nil "buy ~(~A~) (~Dg)" i-want *tech-trade-value*)
             (list :give (list (list :gold *tech-trade-value*)) :want (list (list :tech i-want)))))
      ((and they-want (>= (player-gold them) *tech-trade-value*))
       (cons (format nil "sell ~(~A~) (~Dg)" they-want *tech-trade-value*)
             (list :give (list (list :tech they-want)) :want (list (list :gold *tech-trade-value*)))))
      (t nil))))

(defun cmd-propose-trade (state command)
  "Player :PLAYER offers :TO a deal: it hands over :GIVE and asks for :WANT (each
a list of (:gold N)/(:tech KEY)).  An AI recipient accepts only if what it gains
is worth at least what it gives up; on acceptance the exchange executes."
  (let* ((args (rest command))
         (a (player-by-id state (getf args :player 1)))
         (b (player-by-id state (getf args :to)))
         (give (getf args :give)) (want (getf args :want)))
    (unless (and a b) (fail "no such player"))
    (when (eq a b) (fail "can't trade with yourself"))
    (when (or (eq (player-kind a) :barbarian) (eq (player-kind b) :barbarian))
      (fail "barbarians don't trade"))
    (validate-bundle state a b give)        ; A delivers GIVE to B
    (validate-bundle state b a want)        ; B delivers WANT to A
    ;; the AI recipient weighs what it receives (GIVE) against what it parts with (WANT)
    (when (and (eq (player-kind b) :ai)
               (< (bundle-value give) (bundle-value want)))
      (fail "~A rejected the offer" (player-name b)))
    (transfer-bundle a b give)
    (transfer-bundle b a want)
    state))

(defun cmd-set-research (state command)
  "Set player :PLAYER's current research target to advance :TECH, which must be
one the player can research now (prerequisites met, not already known)."
  (let* ((args (rest command))
         (p (or (player-by-id state (getf args :player 1)) (fail "no such player")))
         (tech (getf args :tech)))
    (unless (gethash tech *techs*) (fail "no such advance ~(~A~)" tech))
    (when (player-has-tech-p p tech) (fail "~A already has ~(~A~)" (player-name p) tech))
    (unless (member tech (researchable-techs p))
      (fail "prerequisites for ~(~A~) are not met" tech))
    (setf (player-researching p) tech)
    p))

(defun cmd-set-rates (state command)
  "Set a player's tax/luxury/science split (percent of trade).  They must sum to
100 and none may exceed the current government's cap."
  (let* ((args (rest command))
         (p (or (player-by-id state (getf args :player 1)) (fail "no such player")))
         (tax (getf args :tax)) (lux (getf args :luxury)) (sci (getf args :science))
         (gov (player-government p))
         (maxr (government-def gov :max-rate 100)))
    (unless (and tax lux sci) (fail "set-rates needs :tax, :luxury and :science"))
    (when (some #'minusp (list tax lux sci)) (fail "rates can't be negative"))
    (unless (= 100 (+ tax lux sci)) (fail "rates must sum to 100"))
    (when (> (max tax lux sci) maxr)
      (fail "~A caps each rate at ~D%" (government-def gov :name) maxr))
    (setf (player-tax-rate p) tax (player-luxury-rate p) lux (player-science-rate p) sci)
    p))

(defun clamp-rates (p)
  "Force P's rates within the current government's cap and back to a 100 sum
(used after a government change, which may tighten the cap)."
  (let ((maxr (government-def (player-government p) :max-rate 100)))
    (setf (player-tax-rate p)     (min (player-tax-rate p) maxr)
          (player-science-rate p) (min (player-science-rate p) maxr)
          (player-luxury-rate p)  (min (player-luxury-rate p) maxr))
    (let ((short (- 100 (+ (player-tax-rate p) (player-science-rate p)
                           (player-luxury-rate p)))))
      ;; pour any shortfall into whichever rates still have room under the cap
      (incf (player-tax-rate p) (min short (- maxr (player-tax-rate p))))
      (setf short (- 100 (+ (player-tax-rate p) (player-science-rate p)
                            (player-luxury-rate p))))
      (incf (player-science-rate p) (min short (- maxr (player-science-rate p))))
      (setf short (- 100 (+ (player-tax-rate p) (player-science-rate p)
                            (player-luxury-rate p))))
      (incf (player-luxury-rate p) short))))

(defun cmd-set-government (state command)
  "Start a revolution: the player spends one turn in anarchy, then adopts
government :TO (which must be unlocked by an advance)."
  (let* ((args (rest command))
         (p (or (player-by-id state (getf args :player 1)) (fail "no such player")))
         (to (getf args :to)))
    (unless (gethash to *governments*) (fail "unknown government ~A" to))
    (when (eq to :anarchy) (fail "can't deliberately choose anarchy"))
    (when (eq to (player-government p)) (fail "already a ~(~A~)" to))
    (let ((req (government-def to :requires)))
      (unless (player-has-tech-p p req) (fail "~(~A~) requires the ~(~A~) advance" to req)))
    (setf (player-government p) :anarchy
          (player-gov-target p) to
          (player-anarchy-left p) 1)
    (clamp-rates p)
    p))

(defun can-found-here-p (state unit)
  "T if UNIT may found a city where it stands: a settler-class unit on a tile that
does not already hold a city (matches CMD-FOUND-CITY's checks)."
  (and (member :found-city (unit-def (unit-type unit) :abilities))
       (let ((tile (tile-at (gs-map state) (unit-x unit) (unit-y unit))))
         (and tile (not (tile-city tile))))))

(defun cmd-found-city (state command)
  (let* ((args (rest command))
         (u (or (unit-by-id state (getf args :unit)) (fail "no such unit"))))
    (unless (member :found-city (unit-def (unit-type u) :abilities))
      (fail "~(~A~) cannot found a city" (unit-type u)))
    (let ((tile (tile-at (gs-map state) (unit-x u) (unit-y u))))
      (when (tile-city tile) (fail "a city already exists here"))
      (let* ((owner (unit-owner u))
             (first-city (notany (lambda (c) (= (city-owner c) owner))
                                 (loop for c being the hash-values of (gs-cities state) collect c)))
             (c (register-city state
                              :name (getf args :name "City")
                              :owner owner
                              :x (unit-x u) :y (unit-y u))))
        (setf (city-production c) '(:unit :warriors))   ; default build
        (when first-city (push :palace (city-buildings c)))  ; the capital holds the Palace
        (push (cons (gs-turn state) (unit-owner u)) (gs-foundings state))  ; for the replay
        ;; consume the settler
        (remhash (unit-id u) (gs-units state))
        (setf (tile-units tile) (remove (unit-id u) (tile-units tile)))
        c))))

(defun cmd-set-production (state command)
  (let* ((args (rest command))
         (c (or (city-by-id state (getf args :city)) (fail "no such city")))
         (item (getf args :item))
         (owner (player-by-id state (city-owner c))))
    (ecase (first item)
      (:unit
       (unless (gethash (second item) *units*) (fail "unknown unit ~A" (second item)))
       (let ((req (unit-def (second item) :requires)))
         (unless (player-has-tech-p owner req) (fail "requires tech ~(~A~)" req)))
       (when (unit-obsolete-p owner (second item))
         (fail "~(~A~) is obsolete" (second item)))
       (when (and (eq (second item) :nuclear)        ; nukes need the Manhattan Project
                  (not (wonder-built-p state :manhattan-project)))
         (fail "nuclear weapons require the Manhattan Project")))
      (:building
       (unless (gethash (second item) *buildings*) (fail "unknown building ~A" (second item)))
       (when (member (second item) (city-buildings c)) (fail "already built"))
       (let ((req (building-def (second item) :requires)))
         (unless (player-has-tech-p owner req) (fail "requires tech ~(~A~)" req))))
      (:wonder
       (unless (gethash (second item) *wonders*) (fail "unknown wonder ~A" (second item)))
       (when (wonder-built-p state (second item)) (fail "wonder already built"))
       (let ((req (wonder-def (second item) :requires)))
         (unless (player-has-tech-p owner req) (fail "requires tech ~(~A~)" req))))
      (:spaceship
       (unless (wonder-built-p state :apollo-program)
         (fail "the Apollo Program must be built first"))
       (unless (player-has-tech-p owner :space-flight)
         (fail "requires the space-flight advance"))     ; structurals
       (unless (player-has-tech-p owner :fusion-power)
         (fail "requires the fusion-power advance"))))    ; the ship's fuel
    (setf (city-production c) item)
    c))

(defun next-specialist (kind)
  "Cycle a specialist's job: entertainer -> taxman -> scientist -> entertainer."
  (ecase kind
    (:entertainer :taxman)
    (:taxman :scientist)
    (:scientist :entertainer)))

(defun cmd-set-specialists (state command)
  "Adjust a city's specialists.  :op is
  :add    -- pull a tile-working citizen off to a specialist job (an entertainer),
  :remove -- send a specialist back to working a tile,
  :cycle  -- change specialist :index's job (entertainer/taxman/scientist).
After the change the city re-optimises its tile assignment."
  (let* ((args (rest command))
         (c (or (city-by-id state (getf args :city)) (fail "no such city")))
         (op (getf args :op)))
    (ecase op
      (:add (when (plusp (city-worker-count c))
              (setf (city-specialists c) (cons :entertainer (city-specialists c)))))
      (:remove (when (city-specialists c)            ; auto-work re-adds any forced ones
                 (setf (city-specialists c) (rest (city-specialists c)))))
      (:cycle (let ((i (getf args :index)))
                (when (and i (< -1 i (length (city-specialists c))))
                  (setf (nth i (city-specialists c))
                        (next-specialist (nth i (city-specialists c))))))))
    (city-auto-work state c)
    c))

(defun cmd-work-tile (state command)
  "Hand-manage CITY's tiles like the Civ1 city screen.  With :auto T, hand control
back to the governor.  Otherwise toggle tile (:x,:y): a worked tile is freed (its
citizen becomes a specialist) and an idle tile is worked (a specialist takes it).
The first manual touch seeds the locks from the current worked set."
  (let* ((args (rest command))
         (c (or (city-by-id state (getf args :city)) (fail "no such city"))))
    (cond
      ((getf args :auto)
       (setf (city-manual-tiles c) nil (city-tile-locks c) '()))
      (t
       (unless (city-manual-tiles c)                ; enter manual mode
         (setf (city-manual-tiles c) t
               (city-tile-locks c) (copy-list (city-worked c))))
       (let* ((tile (list (getf args :x) (getf args :y)))
              (radius (city-radius-tiles state c)))
         (cond
           ((member tile (city-tile-locks c) :test #'equal)   ; worked -> free it
            (setf (city-tile-locks c) (remove tile (city-tile-locks c) :test #'equal)))
           ((and (member tile radius :test #'equal)            ; idle, workable, and a
                 (< (length (city-tile-locks c)) (city-size c)))  ; citizen is free
            (push tile (city-tile-locks c)))))))
    (city-auto-work state c)
    c))
