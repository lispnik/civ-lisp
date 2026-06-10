;;;; defs.lisp -- the rulebook as DATA, not code.
;;;;
;;;; Terrain, units, buildings and techs are plain tables keyed by keyword.
;;;; Balancing or modding the game = editing these literals.  Each entry is a
;;;; plist; look values up with DEF-GET.  This deliberately mirrors the
;;;; data-driven *sprite-sheets* table in the sibling civ-extract project.

(in-package #:civ-model)

(defun %table (entries)
  "Build a keyword->plist hash-table from ((:key . plist) ...)."
  (let ((h (make-hash-table :test 'eq)))
    (dolist (e entries h)
      (setf (gethash (car e) h) (cdr e)))))

(defparameter *terrain*
  (%table
   '((:ocean     :food 1 :shields 0 :trade 2 :move 1 :defense 0)
     (:grassland :food 2 :shields 0 :trade 0 :move 1 :defense 0)
     (:plains    :food 1 :shields 1 :trade 0 :move 1 :defense 0)
     (:forest    :food 1 :shields 2 :trade 0 :move 2 :defense 25)
     (:hills     :food 1 :shields 0 :trade 0 :move 2 :defense 50)
     (:mountains :food 0 :shields 1 :trade 0 :move 3 :defense 100)
     (:desert    :food 0 :shields 1 :trade 0 :move 1 :defense 0)))
  "Terrain types -> base yields, move cost, % defense bonus.")

(defparameter *units*
  (%table
   '((:settlers :attack 0 :defense 1 :move 1 :cost 30 :requires nil :domain :land
      :abilities (:found-city))
     (:warriors :attack 1 :defense 1 :move 1 :cost 10 :requires nil :domain :land)
     (:phalanx  :attack 1 :defense 2 :move 1 :cost 20 :requires :bronze-working :domain :land)
     (:legion   :attack 4 :defense 2 :move 1 :cost 20 :requires :iron-working :domain :land)
     (:catapult :attack 6 :defense 1 :move 1 :cost 40 :requires :mathematics :domain :land)
     (:trireme  :attack 1 :defense 1 :move 3 :cost 40 :requires :map-making :domain :sea)))
  "Unit types -> combat stats, build cost, tech requirement, abilities.
:domain is :land (blocked from ocean) or :sea (must stay on ocean).")

(defparameter *buildings*
  (%table
   '((:granary :cost 60 :upkeep 1 :requires :pottery     :effect (:food-keep 1/2))
     (:library :cost 80 :upkeep 1 :requires :writing     :effect (:science 1/2))
     (:walls   :cost 60 :upkeep 0 :requires :masonry     :effect (:defense 2))
     (:barracks :cost 40 :upkeep 1 :requires nil         :effect (:veteran t))))
  "Building types -> cost, gold upkeep, tech requirement, effect.")

(defparameter *wonders*
  (%table
   '((:pyramids         :cost 200 :requires :masonry        :effect "+50% shields")
     (:hanging-gardens  :cost 120 :requires :pottery        :effect "+1 food")
     (:colossus         :cost 120 :requires :bronze-working :effect "+50% trade")
     (:great-library    :cost 300 :requires :writing        :effect "+50% science")
     (:great-wall       :cost 180 :requires :masonry        :effect "+100% city defense")))
  "World wonders (one per game) -> cost, tech requirement, effect.")

(defparameter *techs*
  (%table
   '((:pottery       :cost 0  :prereqs ()                      :unlocks (:granary))
     (:bronze-working :cost 0 :prereqs ()                      :unlocks (:phalanx))
     (:masonry       :cost 0  :prereqs ()                      :unlocks (:walls))
     (:writing       :cost 0  :prereqs (:alphabet)             :unlocks (:library))
     (:alphabet      :cost 0  :prereqs ()                      :unlocks ())
     (:iron-working  :cost 0  :prereqs (:bronze-working)       :unlocks (:legion))
     (:mathematics   :cost 0  :prereqs (:masonry :alphabet)    :unlocks (:catapult))))
  "Tech tree -> research cost, prerequisite techs, what it unlocks.")

(defparameter *special-bonus*
  ;; terrain -> (food shields trade) added when the tile has its special resource
  '((:grassland 0 1 0)   ; shield
    (:plains    0 2 0)   ; horses
    (:forest    2 0 0)   ; game
    (:hills     0 2 0)   ; coal
    (:mountains 0 0 6)   ; gold
    (:desert    3 0 0)   ; oasis
    (:ocean     2 0 0))  ; fish
  "Yield bonus per terrain's special resource.")

(declaim (inline def-get))
(defun def-get (table key prop &optional default)
  "Look up PROP for KEY in a definition TABLE."
  (getf (gethash key table) prop default))

;; convenience wrappers
(defun terrain-def  (key prop &optional d) (def-get *terrain* key prop d))
(defun unit-def     (key prop &optional d) (def-get *units* key prop d))
(defun building-def (key prop &optional d) (def-get *buildings* key prop d))
(defun wonder-def   (key prop &optional d) (def-get *wonders* key prop d))
(defun tech-def     (key prop &optional d) (def-get *techs* key prop d))
