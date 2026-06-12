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
    (:fortify        (cmd-fortify state command))
    (:wake           (cmd-wake state command))
    (:goto           (cmd-goto state command))
    ((:build-road :build-railroad :irrigate :mine :build-fort :clear-forest)
     (cmd-terraform state command))
    (:clean-pollution (cmd-clean-pollution state command))
    (:set-rates      (cmd-set-rates state command))
    (:set-government (cmd-set-government state command))
    (:declare-war    (cmd-declare-war state command))
    (:make-peace     (cmd-make-peace state command))
    (:propose-trade  (cmd-propose-trade state command))
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
(declaim (ftype (function (t t) t) advance-goto))

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
           (hostiles (tile-hostiles state dest (unit-owner u))))
      ;; terrain domain: land units can't enter ocean; sea units can't land;
      ;; air units go anywhere.  (rivers are land terrain, so land units cross them)
      (ecase (unit-def (unit-type u) :domain :land)
        (:sea (unless sea-dest (fail "naval unit can't move onto land")))
        (:land (when sea-dest (fail "land unit can't move into the water")))
        (:air nil))
      ;; can't enter a tile held by a civilization you're only at peace with
      (when (and foreign (not hostiles))
        (fail "at peace with ~(~A~); declare war first"
              (player-name (player-by-id state (unit-owner (first foreign))))))
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
                    (setf (unit-x u) nx (unit-y u) ny))))
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
              (decf (unit-moves-left u) (max 1 (min cost (unit-moves-left u))))
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
  "Player :PLAYER declares war on player :AGAINST."
  (let* ((args (rest command))
         (a (getf args :player 1)) (b (getf args :against)))
    (unless (and (player-by-id state a) (player-by-id state b)) (fail "no such player"))
    (when (= a b) (fail "can't declare war on yourself"))
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

(defun cmd-found-city (state command)
  (let* ((args (rest command))
         (u (or (unit-by-id state (getf args :unit)) (fail "no such unit"))))
    (unless (member :found-city (unit-def (unit-type u) :abilities))
      (fail "~(~A~) cannot found a city" (unit-type u)))
    (let ((tile (tile-at (gs-map state) (unit-x u) (unit-y u))))
      (when (tile-city tile) (fail "a city already exists here"))
      (let ((c (register-city state
                              :name (getf args :name "City")
                              :owner (unit-owner u)
                              :x (unit-x u) :y (unit-y u))))
        (setf (city-production c) '(:unit :warriors))   ; default build
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
         (unless (player-has-tech-p owner req) (fail "requires tech ~(~A~)" req))))
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
         (fail "requires the space-flight advance"))))
    (setf (city-production c) item)
    c))
