;;;; economy-demo.lisp -- show the AI running an economy, not just an army.
;;;;
;;;;   sbcl --non-interactive --load examples/economy-demo.lisp
;;;;
;;;; Six AI civilizations play a long game with no human player.  This demo
;;;; exercises the "smarter economy" wiring: the AI beelines key advances
;;;; (AI-RESEARCH), leads revolutions toward better governments (AI-GOVERNMENT),
;;;; and invests its largest city in world wonders (AI-BEST-WONDER), whose
;;;; effects are now live in the rules.  It prints each civ's final government
;;;; and the wonders it completed, snapshots the final board to
;;;; docs/economy-final.png, and asserts the AIs revolted and raised wonders.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defparameter *eco-w* 60) (defparameter *eco-h* 40)
(defparameter *eco-turns* 320) (defparameter *eco-seed* 1)

(defun eco/civs (state)
  (loop for p across (civm:gs-players state)
        unless (eq (civm:player-kind p) :barbarian) collect p))

(defun eco/player-wonders (state pid)
  "World wonders standing in PID's cities."
  (loop for c being the hash-values of (civm:gs-cities state)
        when (= (civm:city-owner c) pid)
          append (intersection (civm:city-buildings c) civm::*ai-wonder-order*)))

(defun economy-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "economy" :w (* *eco-w* 16) :h (* *eco-h* 16) :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed *eco-seed* :width *eco-w* :height *eco-h*
                                            :players '("Rome" "Egypt" "Zulu" "Babylon"
                                                       "Greece" "China")))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1)
                  (painter-nuke painter) (load-atlas ren *nuke-image* +nuke-bg-key+))
            (dolist (p (eco/civs state)) (setf (civm::player-kind p) :ai))
            (dotimes (i *eco-turns*)
              (civm:end-turn state)
              (when (civm:gs-winner state) (return)))
            ;; render the final board
            (render-game painter state nil :fog nil :cam-x 0 :cam-y 0 :vw *eco-w* :vh *eco-h*)
            (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
                   (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format
                          0 (* *eco-w* 16) (* *eco-h* 16) 32 fmt)))
              (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
                (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
                (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
              (sdl2-image:save-png surf (namestring (merge-pathnames "economy-final.png" docs)))
              (sdl2-ffi.functions:sdl-free-surface surf))
            ;; report
            (format t "~&After ~D turns (year ~D):~%" (civm:gs-turn state) (civm:gs-year state))
            (let ((revolted 0) (wonders '()))
              (dolist (p (eco/civs state))
                (let ((gov (civm:player-government p))
                      (ws  (eco/player-wonders state (civm:player-id p)))
                      (cities (loop for c being the hash-values of (civm:gs-cities state)
                                    count (= (civm:city-owner c) (civm:player-id p)))))
                  (when (member gov '(:monarchy :republic :democracy)) (incf revolted))
                  (setf wonders (append wonders ws))
                  (format t "  ~8A gov=~10A techs=~2D cities=~D~@[ wonders=~S~]~%"
                          (civm:player-name p) gov
                          (hash-table-count (civm:player-techs p)) cities
                          (and ws ws))))
              (format t "~%Wonders standing: ~S~%" wonders)
              (format t "Civs past despotism: ~D~%" revolted)
              (format t "Wrote docs/economy-final.png~%")
              (format t "=> ~A~%"
                      (if (and (>= revolted 3) wonders) "PASS" "FAIL")))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'economy-demo)
