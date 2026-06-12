;;;; pathfind.lisp -- A* pathfinding and the :goto order.
;;;;
;;;; A unit given a :goto target walks the shortest path toward it each turn
;;;; (PROCESS-GOTO, called from END-TURN), routing around ocean and enemy units
;;;; using terrain movement costs.  It stops if blocked (e.g. a zone of control)
;;;; and resumes next turn; the order clears on arrival.

(in-package #:civ-model)

(defun passable-p (state x y owner)
  "Can OWNER's land unit stand on (X,Y)?  Not ocean, not enemy-occupied."
  (let ((tile (tile-at (gs-map state) x y)))
    (and tile
         (not (eq (tile-terrain tile) :ocean))
         (not (tile-enemies state tile owner)))))

(declaim (inline %key))
(defun %key (x y w) (+ x (* y w)))

(defun find-path (state sx sy gx gy owner)
  "A* shortest path from (SX,SY) to (GX,GY) for OWNER over passable tiles,
weighted by terrain move cost (8-directional).  Returns a list of (x y) steps
from the first move through the goal, or NIL if unreachable / already there."
  (when (and (passable-p state gx gy owner) (not (and (= sx gx) (= sy gy))))
    (let* ((map (gs-map state))
           (w (map-width map))
           (start (%key sx sy w))
           (goal (%key gx gy w))
           (g (make-hash-table :test 'eql))   ; best cost to reach a node
           (came (make-hash-table :test 'eql))
           (coord (make-hash-table :test 'eql)) ; key -> (x . y)
           (open (list start)))               ; small maps: linear open set
      (flet ((h (x y) (max (map-dx map x gx) (abs (- y gy)))))  ; chebyshev, wrapped
        (setf (gethash start g) 0
              (gethash start coord) (cons sx sy))
        (loop while open do
          ;; pop the open node with the lowest f = g + h
          (let* ((best (reduce (lambda (a b)
                                 (let ((ca (gethash a coord)) (cb (gethash b coord)))
                                   (if (<= (+ (gethash a g) (h (car ca) (cdr ca)))
                                           (+ (gethash b g) (h (car cb) (cdr cb))))
                                       a b)))
                               open))
                 (bc (gethash best coord)) (bx (car bc)) (by (cdr bc)))
            (setf open (remove best open))
            (when (= best goal)
              ;; reconstruct
              (return-from find-path
                (let ((path '()) (k goal))
                  (loop until (= k start)
                        do (let ((c (gethash k coord)))
                             (push (list (car c) (cdr c)) path))
                           (setf k (gethash k came)))
                  path)))
            (loop for (nx ny tile) in (neighbors map bx by) do
              (when (and tile
                         (or (= (%key nx ny w) goal)        ; goal may be reached
                             (passable-p state nx ny owner)))
                (let* ((nk (%key nx ny w))
                       (step-cost (max 1 (terrain-def (tile-terrain tile) :move 1)))
                       (tentative (+ (gethash best g) step-cost)))
                  (when (or (null (gethash nk g)) (< tentative (gethash nk g)))
                    (setf (gethash nk g) tentative
                          (gethash nk came) best
                          (gethash nk coord) (cons nx ny))
                    (pushnew nk open)))))))
        nil))))

(defun clear-goto (unit)
  (setf (unit-orders unit) :idle (unit-goto-x unit) nil (unit-goto-y unit) nil))

(defun advance-goto (state unit)
  "Move UNIT along its goto path as far as this turn's movement allows.
Stops (keeping the order) if a step is blocked; clears the order on arrival."
  (let ((path (and (unit-goto-x unit)
                   (find-path state (unit-x unit) (unit-y unit)
                              (unit-goto-x unit) (unit-goto-y unit)
                              (unit-owner unit)))))
    (if (null path)
        (clear-goto unit)                       ; arrived or no route
        (dolist (step path)
          (when (<= (unit-moves-left unit) 0) (return))
          (destructuring-bind (nx ny) step
            (handler-case
                (cmd-move-unit state (list :move-unit :unit (unit-id unit)
                                           ;; a step across the seam is still +/-1
                                           :dx (signed-dx (gs-map state) (unit-x unit) nx)
                                           :dy (- ny (unit-y unit))))
              (command-error () (return)))      ; blocked (e.g. ZOC): try next turn
            ;; cmd-move-unit set orders to :idle; restore unless we've arrived
            (if (and (= (unit-x unit) (unit-goto-x unit))
                     (= (unit-y unit) (unit-goto-y unit)))
                (progn (clear-goto unit) (return))
                (setf (unit-orders unit) :goto)))))))

(defun process-goto (state)
  "Advance every unit currently on a :goto order."
  (let ((units '()))
    (maphash (lambda (id u) (declare (ignore id))
               (when (eq (unit-orders u) :goto) (push u units)))
             (gs-units state))
    (dolist (u units) (advance-goto state u))))
