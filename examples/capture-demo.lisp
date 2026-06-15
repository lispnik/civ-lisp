;;;; capture-demo.lisp -- verify (and illustrate) city capture: a land unit
;;;; walks into an enemy city and takes it; a size-1 city is razed.
;;;;
;;;;   sbcl --non-interactive --load examples/capture-demo.lisp
;;;;
;;;; It drives the real view headlessly (a hidden window + software renderer),
;;;; marches a legion into a size-3 enemy city and another into a size-1 one,
;;;; and asserts the first flips owner (shrunk) and the second is razed off the
;;;; map (prints PASS/FAIL).  Writes before/after screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun capture-demo/shot (painter state path)
  (let ((ren (painter-ren painter)))
    (render-game painter state nil :fog nil :cam-x 0 :cam-y 0 :vw 20 :vh 12)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 320 192 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun capture-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "capture-demo" :w 320 :h 192 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed 2 :width 16 :height 10
                                            :players '("You" "Red")))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            ;; clear the random starting units so the scene is just the assault
            (dolist (id (loop for u being the hash-values of (civm:gs-units state)
                              collect (civm:unit-id u)))
              (civm::destroy-unit state (civm:unit-by-id state id)))
            (setf (civm:relation state 1 2) :war)
            (let ((carthage (civm::register-city state :name "Carthage" :owner 2 :x 5 :y 5))
                  (hamlet   (civm::register-city state :name "Hamlet"   :owner 2 :x 8 :y 5))
                  (a (civm::register-unit state :type :legion :owner 1 :x 4 :y 5))
                  (b (civm::register-unit state :type :legion :owner 1 :x 7 :y 5)))
              (setf (civm:city-size carthage) 3)
              (format t "~&Before: Carthage owner ~D size ~D; Hamlet owner ~D size ~D~%"
                      (civm:city-owner carthage) (civm:city-size carthage)
                      (civm:city-owner hamlet) (civm:city-size hamlet))
              (capture-demo/shot painter state (merge-pathnames "capture-before.png" docs))
              ;; capture the bigger city, raze the small one
              (civm:apply-command state (list :move-unit :unit (civm:unit-id a) :dx 1 :dy 0))
              (format t "~A~%" (civm:gs-message state))
              (civm:apply-command state (list :move-unit :unit (civm:unit-id b) :dx 1 :dy 0))
              (format t "~A~%" (civm:gs-message state))
              (capture-demo/shot painter state (merge-pathnames "capture-after.png" docs))
              (let ((captured (and (= (civm:city-owner carthage) 1)
                                   (= (civm:city-size carthage) 2)))
                    (razed (null (civm:city-by-id state (civm:city-id hamlet)))))
                (format t "Carthage captured (owner 1, size 2)? ~A~%" captured)
                (format t "Hamlet razed off the map? ~A~%" razed)
                (format t "=> ~A~%" (if (and captured razed) "PASS" "FAIL")))
              (format t "Wrote docs/capture-{before,after}.png~%"))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'capture-demo)
