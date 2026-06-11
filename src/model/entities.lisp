;;;; entities.lisp -- players, units, cities.
;;;;
;;;; All entities are referenced elsewhere by integer id (so the state stays a
;;;; flat, serializable graph rather than a tangle of object pointers).

(in-package #:civ-model)

(defstruct (player (:constructor make-player (&key id name (kind :ai) (color 1))))
  (id 0 :type fixnum)
  name
  (kind :ai :type keyword)        ; :human or :ai
  (color 1 :type fixnum)
  (gold 0 :type fixnum)
  (government :despotism :type keyword)
  ;; tax/science/luxury split (percent of trade), must sum to 100
  (tax-rate 50) (science-rate 50) (luxury-rate 0)
  (techs (make-hash-table :test 'eq))   ; researched advances (set: key -> t)
  (researching nil)                     ; tech currently being researched
  (beakers 0 :type fixnum)              ; science accumulated toward it
  (seen (make-hash-table :test 'eql))   ; explored tiles (fog of war): key x+y*w -> t
  (score 0 :type fixnum))

(defun player-has-tech-p (player tech)
  (or (null tech) (gethash tech (player-techs player))))

(defstruct (unit (:constructor make-unit (&key id type owner x y)))
  (id 0 :type fixnum)
  (type :warriors :type keyword)
  (owner 0 :type fixnum)          ; player id
  (x 0 :type fixnum) (y 0 :type fixnum)
  (hp 10 :type fixnum)
  (moves-left 1 :type fixnum)
  (orders :idle :type keyword)    ; :idle :fortified :sentry :goto ...
  (goto-x nil) (goto-y nil)       ; :goto target tile
  (work nil)                      ; terraform job in progress: :build-road/:irrigate/:mine or NIL
  (work-left 0 :type fixnum)      ; turns of work remaining on WORK
  (veteran nil))

(defstruct (city (:constructor make-city (&key id name owner x y (size 1))))
  (id 0 :type fixnum)
  name
  (owner 0 :type fixnum)          ; player id
  (x 0 :type fixnum) (y 0 :type fixnum)
  (size 1 :type fixnum)           ; population
  (food-box 0 :type fixnum)       ; food accumulated toward growth
  (shield-box 0 :type fixnum)     ; shields accumulated toward production
  (buildings '())                 ; list of building keywords already built
  (production nil)                ; what we're building: (:unit k) | (:building k)
  (worked '()))                   ; list of (x y) tiles citizens are working
