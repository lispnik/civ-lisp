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
    (:goto           (cmd-goto state command))
    (:end-turn       (end-turn state)))
  state)

;; defined in pathfind.lisp (loaded later)
(declaim (ftype (function (t t) t) advance-goto))

(defun cmd-goto (state command)
  "Set a unit's destination and start moving it there immediately (and on each
following turn via PROCESS-GOTO)."
  (let* ((args (rest command))
         (u (or (unit-by-id state (getf args :unit)) (fail "no such unit")))
         (tx (getf args :x)) (ty (getf args :y)))
    (unless (in-bounds-p (gs-map state) tx ty) (fail "goto target out of bounds"))
    (setf (unit-goto-x u) tx (unit-goto-y u) ty (unit-orders u) :goto)
    (advance-goto state u)          ; move now, don't wait for end-of-turn
    u))

(defun cmd-fortify (state command)
  "Order a unit to fortify: it digs in for a defense bonus and faster healing
until it next moves."
  (let ((u (or (unit-by-id state (getf (rest command) :unit)) (fail "no such unit"))))
    (setf (unit-orders u) :fortified)
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
           (enemies (tile-enemies state dest (unit-owner u))))
      (if enemies
          ;; attack: fight the strongest defender, advance if the tile clears
          (progn
            (when (zerop (attack-strength u))
              (fail "~(~A~) cannot attack" (unit-type u)))
            (let* ((defender (first (sort enemies #'>
                                          :key (lambda (e) (defense-strength state e)))))
                   (result (resolve-combat state u defender)))
              (when (eq result :attacker)
                (setf (unit-moves-left u) 0
                      (unit-orders u) :idle)        ; attacking breaks fortify
                (unless (tile-enemies state dest (unit-owner u))
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
              (setf (unit-x u) nx (unit-y u) ny
                    (unit-orders u) :idle)          ; moving breaks fortify
              (decf (unit-moves-left u) (max 1 (min cost (unit-moves-left u))))
              u))))))

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
         (unless (player-has-tech-p owner req) (fail "requires tech ~(~A~)" req)))))
    (setf (city-production c) item)
    c))
