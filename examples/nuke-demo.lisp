;;;; nuke-demo.lisp -- verify (and illustrate) a nuclear detonation: a missile
;;;; flattens a 3x3 area, devastates the city in it, and leaves fallout.
;;;;
;;;;   sbcl --non-interactive --load examples/nuke-demo.lisp
;;;;
;;;; It drives the real view headlessly (a hidden window + software renderer),
;;;; rings an enemy city with units, detonates a missile beside it, and asserts
;;;; the blast wiped the ring (sparing a unit outside it), halved the city, and
;;;; left fallout (prints PASS/FAIL).  Writes before/after screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun nuke-demo/shot (painter state path)
  (let ((ren (painter-ren painter)))
    (render-game painter state nil :fog nil :cam-x 0 :cam-y 0 :vw 16 :vh 10)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 256 160 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun nuke-demo/enemy-count (s)
  (loop for u being the hash-values of (civm:gs-units s)
        count (= (civm:unit-owner u) 2)))

(defun nuke-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "nuke-demo" :w 256 :h 160 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((s (civm::%make-game-state
                     :map (civm::make-game-map 16 10 :terrain :grassland)
                     :players (vector (civm::make-player :id 1 :name "You" :kind :human)
                                      (civm::make-player :id 2 :name "Foe" :kind :ai))
                     :random (sb-ext:seed-random-state 1)))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            (setf (civm:relation s 1 2) :war)
            (let ((c (civm::register-city s :name "Aztec" :owner 2 :x 8 :y 5)))
              (setf (civm:city-size c) 8)
              (civm::register-unit s :type :phalanx :owner 2 :x 8 :y 5)   ; garrison
              (dolist (p '((7 . 4) (6 . 5) (8 . 6) (6 . 4) (7 . 6)))      ; ringed defenders
                (civm::register-unit s :type :legion :owner 2 :x (car p) :y (cdr p)))
              (civm::register-unit s :type :legion :owner 2 :x 10 :y 5)   ; outside the blast
              (let ((nuke (civm::register-unit s :type :nuclear :owner 1 :x 7 :y 5))
                    (n0 (nuke-demo/enemy-count s)))
                (format t "~&Before: Aztec size ~D; ~D enemy units~%" (civm:city-size c) n0)
                (nuke-demo/shot painter s (merge-pathnames "nuke-before.png" docs))
                (civm:apply-command s (list :nuke :unit (civm:unit-id nuke)))
                (let ((survivors (nuke-demo/enemy-count s))
                      (gz (civm:tile-pollution (civm:tile-at (civm:gs-map s) 7 5))))
                  (format t "After:  Aztec size ~D; ~D enemy units; fallout at ground zero? ~A~%"
                          (civm:city-size c) survivors (and gz t))
                  (nuke-demo/shot painter s (merge-pathnames "nuke-after.png" docs))
                  (format t "=> ~A~%"
                          (if (and (= (civm:city-size c) 4)   ; 8 -> halved
                                   (= survivors 1)            ; only the unit outside survives
                                   gz)                        ; fallout left behind
                              "PASS" "FAIL")))
                (format t "Wrote docs/nuke-{before,after}.png~%")))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'nuke-demo)
