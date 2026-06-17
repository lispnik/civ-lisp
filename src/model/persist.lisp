;;;; persist.lisp -- save and load a whole game.
;;;;
;;;; GAME-STATE is a flat graph of plain data referenced by integer id, which is
;;;; exactly what makes this cheap: DUMP-GAME walks it into a readable
;;;; s-expression (structs become plists; tile occupancy is reconstructed from
;;;; the entity lists rather than duplicated) and LOAD-GAME-FORM rebuilds it.
;;;; The RNG is stored verbatim -- SBCL prints random-states readably -- so a
;;;; loaded game continues to roll identically: save/load is fully deterministic.

(in-package #:civ-model)

(defparameter +save-version+ 1)

;;; --- struct <-> plist ------------------------------------------------------

(defun unit->list (u)
  (list :id (unit-id u) :type (unit-type u) :owner (unit-owner u)
        :x (unit-x u) :y (unit-y u) :hp (unit-hp u)
        :moves-left (unit-moves-left u) :orders (unit-orders u)
        :goto-x (unit-goto-x u) :goto-y (unit-goto-y u)
        :work (unit-work u) :work-left (unit-work-left u)
        :fuel (unit-fuel u) :veteran (unit-veteran u) :home (unit-home u)))

(defun list->unit (ul)
  (let ((u (make-unit :id (getf ul :id) :type (getf ul :type)
                      :owner (getf ul :owner) :x (getf ul :x) :y (getf ul :y))))
    (setf (unit-hp u) (getf ul :hp)
          (unit-moves-left u) (getf ul :moves-left)
          (unit-orders u) (getf ul :orders)
          (unit-goto-x u) (getf ul :goto-x)
          (unit-goto-y u) (getf ul :goto-y)
          (unit-work u) (getf ul :work)
          (unit-work-left u) (getf ul :work-left)
          (unit-fuel u) (or (getf ul :fuel) 0)
          (unit-veteran u) (getf ul :veteran)
          (unit-home u) (getf ul :home))
    u))

(defun city->list (c)
  (list :id (city-id c) :name (city-name c) :owner (city-owner c)
        :x (city-x c) :y (city-y c) :size (city-size c)
        :food-box (city-food-box c) :shield-box (city-shield-box c)
        :buildings (city-buildings c) :production (city-production c)
        :worked (city-worked c) :specialists (city-specialists c)
        :manual-tiles (city-manual-tiles c) :tile-locks (city-tile-locks c)
        :disorder (city-disorder c)))

(defun list->city (cl)
  (let ((c (make-city :id (getf cl :id) :name (getf cl :name)
                      :owner (getf cl :owner) :x (getf cl :x) :y (getf cl :y))))
    (setf (city-size c) (getf cl :size)
          (city-food-box c) (getf cl :food-box)
          (city-shield-box c) (getf cl :shield-box)
          (city-buildings c) (getf cl :buildings)
          (city-production c) (getf cl :production)
          (city-worked c) (getf cl :worked)
          (city-specialists c) (getf cl :specialists)
          (city-manual-tiles c) (getf cl :manual-tiles)
          (city-tile-locks c) (getf cl :tile-locks)
          (city-disorder c) (or (getf cl :disorder) 0))
    c))

(defun player->list (p)
  (list :id (player-id p) :name (player-name p) :kind (player-kind p)
        :color (player-color p) :gold (player-gold p)
        :government (player-government p)
        :gov-target (player-gov-target p) :anarchy-left (player-anarchy-left p)
        :tax-rate (player-tax-rate p) :science-rate (player-science-rate p)
        :luxury-rate (player-luxury-rate p)
        :techs (loop for k being the hash-keys of (player-techs p) collect k)
        :researching (player-researching p) :beakers (player-beakers p)
        :seen (loop for k being the hash-keys of (player-seen p) collect k)
        :score (player-score p) :peace-turns (player-peace-turns p)
        :personality (player-personality p) :city-names (player-city-names p)
        :spaceship (player-spaceship p) :landing (player-landing p)))

(defun list->player (pl)
  (let ((p (make-player :id (getf pl :id) :name (getf pl :name)
                        :kind (getf pl :kind) :color (getf pl :color))))
    (setf (player-gold p) (getf pl :gold)
          (player-government p) (getf pl :government)
          (player-gov-target p) (getf pl :gov-target)
          (player-anarchy-left p) (getf pl :anarchy-left)
          (player-tax-rate p) (getf pl :tax-rate)
          (player-science-rate p) (getf pl :science-rate)
          (player-luxury-rate p) (getf pl :luxury-rate)
          (player-researching p) (getf pl :researching)
          (player-beakers p) (getf pl :beakers)
          (player-score p) (getf pl :score)
          (player-peace-turns p) (or (getf pl :peace-turns) 0)
          (player-personality p) (getf pl :personality)
          (player-city-names p) (getf pl :city-names)
          (player-spaceship p) (or (getf pl :spaceship) 0)
          (player-landing p) (or (getf pl :landing) 0))
    (dolist (k (getf pl :techs)) (setf (gethash k (player-techs p)) t))
    (dolist (k (getf pl :seen))  (setf (gethash k (player-seen p)) t))
    p))

(defun tile->list (tile)
  "Just the intrinsic terrain data; occupancy (units/city/owner) is rebuilt from
the entity lists on load."
  (list (tile-terrain tile)
        (and (tile-river tile) t) (and (tile-special tile) t)
        (and (tile-road tile) t) (and (tile-irrigation tile) t)
        (and (tile-mine tile) t) (and (tile-pollution tile) t)
        (and (tile-railroad tile) t) (and (tile-fort tile) t)
        (and (tile-hut tile) t) (and (tile-airbase tile) t)))

(defun restore-tile (tile spec)
  (destructuring-bind (terrain river special road irrigation mine
                       &optional pollution railroad fort hut airbase) spec
    (setf (tile-terrain tile) terrain
          (tile-river tile) river (tile-special tile) special
          (tile-road tile) road (tile-irrigation tile) irrigation
          (tile-mine tile) mine (tile-pollution tile) pollution
          (tile-railroad tile) railroad (tile-fort tile) fort
          (tile-hut tile) hut (tile-airbase tile) airbase)))

;;; --- whole game ------------------------------------------------------------

(defun dump-game (state)
  "Serialize STATE into a readable s-expression."
  (let ((map (gs-map state)))
    (list :format :civ-save :version +save-version+
          :turn (gs-turn state) :year (gs-year state)
          :id-counter (gs-id-counter state) :phase (gs-phase state)
          :winner (gs-winner state) :victory (gs-victory state)
          :warming (gs-warming state) :difficulty (gs-difficulty state)
          :relations (loop for k being the hash-keys of (gs-relations state)
                             using (hash-value v) collect (cons k v))
          :contacts (loop for k being the hash-keys of (gs-contacts state) collect k)
          :truces (loop for k being the hash-keys of (gs-truces state)
                          using (hash-value v) collect (cons k v))
          :offers (copy-tree (gs-offers state))
          :history (copy-tree (gs-history state))
          :foundings (copy-tree (gs-foundings state))
          :stolen (loop for k being the hash-keys of (gs-stolen state) collect k)
          :embassies (loop for k being the hash-keys of (gs-embassies state) collect k)
          :routes (copy-tree (gs-routes state))
          :random (gs-random state)
          :map (list :width (map-width map) :height (map-height map)
                     :tiles (map 'list #'tile->list (map-tiles map)))
          :players (map 'list #'player->list (gs-players state))
          :units (loop for u being the hash-values of (gs-units state)
                       collect (unit->list u))
          :cities (loop for c being the hash-values of (gs-cities state)
                        collect (city->list c)))))

(defun load-game-form (form)
  "Rebuild a GAME-STATE from the s-expression produced by DUMP-GAME."
  (unless (eq (getf form :format) :civ-save)
    (error "not a civ-lisp save (missing :civ-save marker)"))
  (let* ((mspec (getf form :map))
         (w (getf mspec :width)) (h (getf mspec :height))
         (map (make-game-map w h))
         (state (%make-game-state
                 :turn (getf form :turn) :year (getf form :year)
                 :map map
                 :id-counter (getf form :id-counter)
                 :random (getf form :random)
                 :warming (or (getf form :warming) 0)
                 :winner (getf form :winner) :victory (getf form :victory)
                 :difficulty (or (getf form :difficulty) :prince)
                 :phase (getf form :phase))))
    (loop for spec in (getf mspec :tiles) for i from 0
          do (restore-tile (svref (map-tiles map) i) spec))
    (loop for (k . v) in (getf form :relations)
          do (setf (gethash k (gs-relations state)) v))
    (loop for k in (getf form :contacts)
          do (setf (gethash k (gs-contacts state)) t))
    (loop for (k . v) in (getf form :truces)
          do (setf (gethash k (gs-truces state)) v))
    (setf (gs-offers state) (copy-tree (getf form :offers)))
    (setf (gs-history state) (copy-tree (getf form :history)))
    (setf (gs-foundings state) (copy-tree (getf form :foundings)))
    (loop for k in (getf form :stolen) do (setf (gethash k (gs-stolen state)) t))
    (loop for k in (getf form :embassies) do (setf (gethash k (gs-embassies state)) t))
    (setf (gs-routes state) (copy-tree (getf form :routes)))
    (setf (gs-players state)
          (coerce (mapcar #'list->player (getf form :players)) 'simple-vector))
    (dolist (ul (getf form :units))
      (let ((u (list->unit ul)))
        (setf (gethash (unit-id u) (gs-units state)) u)
        (push (unit-id u) (tile-units (tile-at map (unit-x u) (unit-y u))))))
    (dolist (cl (getf form :cities))
      (let ((c (list->city cl)))
        (setf (gethash (city-id c) (gs-cities state)) c)
        (let ((tile (tile-at map (city-x c) (city-y c))))
          (setf (tile-city tile) (city-id c)
                (tile-owner tile) (city-owner c)))))
    state))

(defun save-game (state path)
  "Write STATE to PATH (overwriting).  Returns PATH."
  (with-open-file (out path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-readably* t) (*print-circle* nil))
        (prin1 (dump-game state) out)
        (terpri out))))
  path)

(defun load-game (path)
  "Read and rebuild a GAME-STATE from the save file at PATH."
  (with-open-file (in path :direction :input)
    (with-standard-io-syntax
      (load-game-form (read in)))))
