;;;; clearing-demo.lisp -- verify (and illustrate) terrain transformation: a
;;;; settler clears a jungle tile down to grassland.
;;;;
;;;;   sbcl --non-interactive --load examples/clearing-demo.lisp
;;;;
;;;; It drives the real view headlessly (a hidden window + software renderer),
;;;; orders a settler to clear a jungle tile, runs the job to completion, and
;;;; asserts the tile became grassland (prints PASS/FAIL).  Writes before/after
;;;; screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun clearing-demo/shot (painter state path cam-x cam-y &optional sel)
  (let ((ren (painter-ren painter)))
    (render-game painter state sel :fog t :cam-x cam-x :cam-y cam-y :vw 20 :vh 15)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 320 240 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun clearing-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "clearing-demo" :w 320 :h 240 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed 4 :width 16 :height 10
                                            :players '("You" "Red")))
                 (map (civm:gs-map state))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*)))
                 (cx 6) (cy 5))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            ;; a patch of jungle with a settler standing on it
            (dolist (d '((0 . 0) (1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
              (setf (civm:tile-terrain (civm:tile-at map (+ cx (car d)) (+ cy (cdr d)))) :jungle))
            (let ((u (civm::register-unit state :type :settlers :owner 1 :x cx :y cy))
                  (cam-x (max 0 (- cx 10))) (cam-y (max 0 (- cy 7))))
              (civm:update-visibility state)
              (format t "~&Tile (~D,~D) before: ~(~A~)~%"
                      cx cy (civm:tile-terrain (civm:tile-at map cx cy)))
              (clearing-demo/shot painter state (merge-pathnames "clearing-before.png" docs)
                                  cam-x cam-y (civm:unit-id u))
              ;; order the clear, then run it to completion
              (civm:apply-command state (list :clear-forest :unit (civm:unit-id u)))
              (dotimes (i (civm:terraform-def :clear-forest :turns))
                (civm::process-terraform state))
              (let ((after (civm:tile-terrain (civm:tile-at map cx cy))))
                (format t "Tile (~D,~D) after clearing: ~(~A~)~%" cx cy after)
                (clearing-demo/shot painter state (merge-pathnames "clearing-after.png" docs)
                                    cam-x cam-y (civm:unit-id u))
                (format t "=> ~A~%" (if (eq after :grassland) "PASS" "FAIL")))
              (format t "Wrote docs/clearing-{before,after}.png~%"))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'clearing-demo)
