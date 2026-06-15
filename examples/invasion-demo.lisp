;;;; invasion-demo.lisp -- verify (and illustrate) the AI mounting a sea
;;;; invasion: boarding troops, ferrying them across a strait, and landing them
;;;; on the enemy shore -- all driven by the AI's own turn logic (END-TURN).
;;;;
;;;;   sbcl --non-interactive --load examples/invasion-demo.lisp
;;;;
;;;; It builds a two-continent scenario, runs end-turns, and asserts an AI land
;;;; unit boards the transport, sails out to sea, and reaches the enemy
;;;; continent (prints PASS/FAIL).  Writes start/sailing/landing screenshots to
;;;; docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun invasion-demo/shot (painter state path)
  (let ((ren (painter-ren painter)))
    (render-game painter state nil :fog nil :cam-x 0 :cam-y 0 :vw 20 :vh 12)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 320 192 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun invasion-demo/ai-land-unit (state)
  "The AI's (player 2) land unit, if any."
  (loop for u being the hash-values of (civm:gs-units state)
        when (and (= (civm:unit-owner u) 2)
                  (eq (civm:unit-def (civm:unit-type u) :domain) :land))
          return u))

(defun invasion-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "invasion-demo" :w 320 :h 192 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed 1 :width 16 :height 10
                                            :players '("You" "Red")))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*)))
                 (map (civm:gs-map state)))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            ;; clear the random starting units so the scene is just the invasion
            (dolist (id (loop for u being the hash-values of (civm:gs-units state)
                              collect (civm:unit-id u)))
              (civm::destroy-unit state (civm:unit-by-id state id)))
            ;; a strait: ocean columns 5..9 split the AI (west) and foe (east)
            (loop for x from 5 to 9 do
              (dotimes (y 10) (setf (civm:tile-terrain (civm:tile-at map x y)) :ocean)))
            (setf (civm:relation state 1 2) :war)
            (civm::register-city state :name "Yorvik" :owner 1 :x 11 :y 5)   ; the target
            (civm::register-unit state :type :musketeers :owner 1 :x 11 :y 5) ; its defender
            (civm::register-city state :name "Akkad"  :owner 2 :x 3 :y 5)     ; AI port
            (civm::register-unit state :type :transport :owner 2 :x 5 :y 5)   ; AI ship, launched
            (civm::register-unit state :type :legion :owner 2 :x 4 :y 5)      ; AI invader
            (civm:update-visibility state)
            (invasion-demo/shot painter state (merge-pathnames "invasion-start.png" docs))
            (let ((sailed nil) (landed nil))
              (dotimes (i 12)
                (civm:end-turn state)
                (let* ((l (invasion-demo/ai-land-unit state)))
                  (when l
                    (let ((sea (eq (civm:tile-terrain
                                    (civm:tile-at map (civm:unit-x l) (civm:unit-y l)))
                                   :ocean)))
                      (when (and sea (not sailed) (>= (civm:unit-x l) 6))
                        (setf sailed t)
                        (invasion-demo/shot painter state
                                            (merge-pathnames "invasion-sailing.png" docs)))
                      (when (and (not landed) (>= (civm:unit-x l) 10))
                        (setf landed t)
                        (invasion-demo/shot painter state
                                            (merge-pathnames "invasion-landing.png" docs))
                        (return))))))
              (format t "~&AI boarded & sailed troops out to sea? ~A~%" sailed)
              (format t "AI landed troops on the enemy continent? ~A~%" landed)
              (format t "=> ~A~%" (if (and sailed landed) "PASS" "FAIL"))
              (format t "Wrote docs/invasion-{start,sailing,landing}.png~%"))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'invasion-demo)
