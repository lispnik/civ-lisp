;;;; disband-demo.lisp -- verify (and illustrate) disbanding a unit in a city.
;;;;
;;;;   sbcl --non-interactive --load examples/disband-demo.lisp
;;;;
;;;; It drives the real view headlessly (a hidden window + software renderer),
;;;; garrisons a catapult in a city building a barracks, disbands it, and asserts
;;;; the unit is gone and exactly build-cost / *disband-shield-divisor* shields
;;;; were refunded into the city (prints PASS/FAIL).  Writes before/after
;;;; screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun disband-demo/shot (painter state path cam-x cam-y &optional sel)
  (let ((ren (painter-ren painter)))
    (render-game painter state sel :fog t :cam-x cam-x :cam-y cam-y :vw 20 :vh 15)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 320 240 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun disband-demo/units-on (state x y)
  (length (civm:tile-units (civm:tile-at (civm:gs-map state) x y))))

(defun disband-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "disband-demo" :w 320 :h 240 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed 5 :width 24 :height 16
                                            :players '("You" "Red")))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            ;; found a city with the human settler, then garrison a catapult in it
            (let* ((settler (loop for u being the hash-values of (civm:gs-units state)
                                  when (and (= (civm:unit-owner u) 1)
                                            (eq (civm:unit-type u) :settlers)) return u))
                   (cx (civm:unit-x settler)) (cy (civm:unit-y settler)))
              (civm:apply-command state (list :found-city :unit (civm:unit-id settler) :name "Rome"))
              (let* ((city (civm:city-by-id state
                            (civm:tile-city (civm:tile-at (civm:gs-map state) cx cy))))
                     (cat (civm::register-unit state :type :catapult :owner 1 :x cx :y cy))
                     (cost (civm:unit-def :catapult :cost 0))
                     (expected (floor cost civm::*disband-shield-divisor*))
                     (cam-x (max 0 (- cx 10))) (cam-y (max 0 (- cy 7))))
                (setf (civm:city-production city) (list :building :barracks)) ; high-cost build
                (civm:update-visibility state)
                (format t "~&Rome building barracks (cost ~D); catapult (cost ~D) garrisoned.~%"
                        (civm::production-cost (civm:city-production city)) cost)
                (format t "Units on Rome's tile: ~D; shield box: ~D~%"
                        (disband-demo/units-on state cx cy) (civm:city-shield-box city))
                (disband-demo/shot painter state (merge-pathnames "disband-before.png" docs)
                                   cam-x cam-y (civm:unit-id cat))
                ;; disband the catapult inside the city
                (civm:apply-command state (list :disband-unit :unit (civm:unit-id cat)))
                (format t "Disbanded -> ~A~%" (civm:gs-message state))
                (disband-demo/shot painter state (merge-pathnames "disband-after.png" docs)
                                   cam-x cam-y nil)
                (let ((gone (null (civm:unit-by-id state (civm:unit-id cat))))
                      (refund (civm:city-shield-box city)))
                  (format t "Unit removed? ~A   shields refunded: ~D (expected ~D)~%"
                          gone refund expected)
                  (format t "=> ~A~%"
                          (if (and gone (= refund expected)) "PASS" "FAIL")))
                (format t "Wrote docs/disband-{before,after}.png~%")))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'disband-demo)
