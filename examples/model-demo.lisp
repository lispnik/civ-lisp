;;;; model-demo.lisp -- drive the pure game model headless (no SDL).
;;;;   sbcl --non-interactive --load examples/model-demo.lisp

(asdf:load-system :civ-model)

(defun find-unit (state owner type)
  (loop for u being the hash-values of (civm:gs-units state)
        when (and (eql (civm:unit-owner u) owner) (eq (civm:unit-type u) type))
          return u))

(defun first-city (state)
  (loop for c being the hash-values of (civm:gs-cities state) return c))

(let ((s (civm:make-new-game :seed 42 :width 12 :height 8
                             :players '("You" "Rival"))))
  (format t "~&New game: turn ~D, year ~D, ~D players, ~D units~%"
          (civm:gs-turn s) (civm:gs-year s)
          (length (civm:gs-players s)) (hash-table-count (civm:gs-units s)))

  ;; found a city with the human player's settler
  (let ((settler (find-unit s 1 :settlers)))
    (format t "Founding Rome with settler ~D at (~D,~D)~%"
            (civm:unit-id settler) (civm:unit-x settler) (civm:unit-y settler))
    (civm:apply-command s (list :found-city :unit (civm:unit-id settler)
                                :name "Rome")))

  ;; commands are validated: a library needs the Writing advance
  (handler-case
      (civm:apply-command s (list :set-production :city (civm:city-id (first-city s))
                                  :item '(:building :library)))
    (civm:command-error (e) (format t "Rejected (as expected): ~A~%" e)))

  ;; run the turn loop
  (dotimes (i 20) (civm:end-turn s))

  (let ((city (first-city s)) (p (civm:player-by-id s 1)))
    (format t "~%After 20 turns: turn ~D, year ~D~%" (civm:gs-turn s) (civm:gs-year s))
    (format t "  ~A: size ~D, shields ~D, buildings ~A~%"
            (civm:city-name city) (civm:city-size city)
            (civm:city-shield-box city) (civm:city-buildings city))
    (format t "  ~A: gold ~D, techs ~D, researching ~A~%"
            (civm:player-name p) (civm:player-gold p)
            (hash-table-count (civm:player-techs p)) (civm:player-researching p))))
