;;;; fog-demo.lisp -- verify (and illustrate) that fog of war clears the instant
;;;; a unit moves, without waiting for end-turn -- matching Civ1.
;;;;
;;;;   sbcl --non-interactive --load examples/fog-demo.lisp
;;;;
;;;; It drives the real view headlessly (a hidden window + software renderer),
;;;; asserts the newly-adjacent tile becomes explored after a single move, and
;;;; writes before/after screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun fog-demo/render-to (painter state path)
  "Render STATE through the fog-of-war view and save a 320x240 PNG to PATH."
  (let ((ren (painter-ren painter)))
    (render-game painter state nil :fog t :cam-x 0 :cam-y 0 :vw 20 :vh 15)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 320 240 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun fog-demo/step (state u)
  "Move U one tile, preferring east but taking any legal direction; T on success."
  (dolist (d '((1 . 0) (0 . 1) (0 . -1) (-1 . 0)) nil)
    (when (handler-case
              (progn (civm:apply-command state
                       (list :move-unit :unit (civm:unit-id u) :dx (car d) :dy (cdr d)))
                     t)
            (civm:command-error () nil))
      ;; demo only: refill moves so one unit can walk several tiles in a single
      ;; turn, making the freshly-revealed trail obvious in the screenshot.
      (setf (civm:unit-moves-left u) 1)
      (return t))))

(defun fog-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "fog-demo" :w 320 :h 240 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed 5 :width 20 :height 15
                                            :players '("You" "Red")))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            (let* ((human (human-player state))
                   (hid (civm:player-id human))
                   (u (loop for x being the hash-values of (civm:gs-units state)
                            when (= (civm:unit-owner x) hid) return x))
                   (sx (civm:unit-x u)) (sy (civm:unit-y u)))
              (format t "~&Unit ~D starts at (~D,~D)~%" (civm:unit-id u) sx sy)
              (fog-demo/render-to painter state (merge-pathnames "fog-move-before.png" docs))

              ;; the verification: a single legal move must reveal the tile two
              ;; squares ahead, with NO end-turn / update-visibility in between.
              (let ((ahead-x (civm:wrap-x (civm:gs-map state) (+ sx 2))))
                (format t "Tile (~D,~D) seen before move? ~A~%"
                        ahead-x sy (and (civm:seen-p state human ahead-x sy) t))
                (fog-demo/step state u)
                (let ((now (and (civm:seen-p state human ahead-x sy) t)))
                  (format t "Tile (~D,~D) seen after one move? ~A  => ~A~%"
                          ahead-x sy now (if now "PASS" "FAIL"))))

              ;; walk a few more tiles for a clear before/after picture
              (dotimes (i 4) (fog-demo/step state u))
              (fog-demo/render-to painter state (merge-pathnames "fog-move-after.png" docs))
              (format t "Unit walked to (~D,~D); wrote docs/fog-move-{before,after}.png~%"
                      (civm:unit-x u) (civm:unit-y u)))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'fog-demo)
