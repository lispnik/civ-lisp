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
    (unless (in-bounds-p (gs-map state) tx ty) (fail "goto target out of bounds"))
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
         (nx (+ (unit-x u) dx))
         (ny (+ (unit-y u) dy)))
    (unless (<= 1 (max (abs dx) (abs dy)) 1) (fail "must move exactly one tile"))
    (unless (in-bounds-p map nx ny) (fail "destination out of bounds"))
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
         (unless (player-has-tech-p owner req) (fail "requires tech ~(~A~)" req)))))
    (setf (city-production c) item)
    c))
