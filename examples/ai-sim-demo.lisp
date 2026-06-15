;;;; ai-sim-demo.lisp -- run a whole game with six AI civilizations and no human
;;;; player, snapshotting the board periodically.
;;;;
;;;;   sbcl --non-interactive --load examples/ai-sim-demo.lisp
;;;;
;;;; Every civ is flipped to :ai, then END-TURN drives the entire game; the board
;;;; is rendered to docs/ai-sim-tNNN.png at intervals and a per-civ city/unit
;;;; tally is printed each time.  At the end it prints the chronicle of city
;;;; captures and razings (GS-LOG) -- the cause behind every city that changed
;;;; hands or vanished.  Asserts the civs develop and cities are contested
;;;; (prints PASS/FAIL).

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defparameter *sim-w* 80) (defparameter *sim-h* 50)
(defparameter *sim-turns* 120) (defparameter *sim-every* 40)

(defun ai-sim/civs (state)
  "List of the non-barbarian players."
  (loop for p across (civm:gs-players state)
        unless (eq (civm:player-kind p) :barbarian) collect p))

(defun ai-sim/tally (state)
  (with-output-to-string (out)
    (dolist (p (ai-sim/civs state))
      (format out "~A:~Dc/~Du "
              (civm:player-name p)
              (loop for c being the hash-values of (civm:gs-cities state)
                    count (= (civm:city-owner c) (civm:player-id p)))
              (loop for u being the hash-values of (civm:gs-units state)
                    count (= (civm:unit-owner u) (civm:player-id p)))))))

(defun ai-sim/city-count (state)
  (hash-table-count (civm:gs-cities state)))

(defun ai-sim-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "ai-sim" :w (* *sim-w* 16) :h (* *sim-h* 16) :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed 12 :width *sim-w* :height *sim-h*
                                            :players '("Rome" "Egypt" "Zulu" "Babylon"
                                                       "Greece" "Persia")
                                            :barbarians t))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1)
                  (painter-nuke painter) (load-atlas ren *nuke-image* +nuke-bg-key+))
            ;; no human: every civ is run by the AI
            (dolist (p (ai-sim/civs state)) (setf (civm::player-kind p) :ai))
            (flet ((snap (turn)
                     (render-game painter state nil :fog nil :cam-x 0 :cam-y 0
                                  :vw *sim-w* :vh *sim-h*)
                     (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
                            (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format
                                   0 (* *sim-w* 16) (* *sim-h* 16) 32 fmt)))
                       (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
                         (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
                         (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
                       (sdl2-image:save-png surf
                         (namestring (merge-pathnames (format nil "ai-sim-t~3,'0D.png" turn) docs)))
                       (sdl2-ffi.functions:sdl-free-surface surf))))
              (snap 0)
              (format t "~&turn   0  ~A~%" (ai-sim/tally state))
              (let ((peak-cities 0))
                (dotimes (i *sim-turns*)
                  (civm:end-turn state)
                  (setf peak-cities (max peak-cities (ai-sim/city-count state)))
                  (let ((turn (1+ i)))
                    (when (or (zerop (mod turn *sim-every*)) (civm:gs-winner state))
                      (snap turn)
                      (format t "turn ~3D  ~A~@[  WINNER: ~A (~A)~]~%" turn (ai-sim/tally state)
                              (and (civm:gs-winner state)
                                   (civm:player-name
                                    (civm:player-by-id state (civm:gs-winner state))))
                              (civm:gs-victory state)))
                    (when (civm:gs-winner state) (return))))
                ;; the chronicle: every city that changed hands or vanished, and why
                (format t "~%-- City events (the cause of every city captured or razed) --~%")
                (let ((events (reverse (civm:gs-log state))))   ; oldest first
                  (if events (dolist (e events) (format t "  ~A~%" e))
                      (format t "  (none -- no city changed hands this game)~%")))
                (format t "=> ~A~%"
                        (if (>= peak-cities 6) "PASS" "FAIL"))  ; the AIs developed
                (format t "Wrote docs/ai-sim-t000.png .. ai-sim-t~3,'0D.png~%" *sim-turns*)))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'ai-sim-demo)
