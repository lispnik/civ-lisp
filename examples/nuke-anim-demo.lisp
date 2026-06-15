;;;; nuke-anim-demo.lisp -- verify (and illustrate) the nuclear detonation
;;;; animation (assets/nuke.png) renders centred on the target tile.
;;;;
;;;;   sbcl --non-interactive --load examples/nuke-anim-demo.lisp
;;;;
;;;; It renders the scene with and without a fireball overlay, diffs the two to
;;;; isolate the explosion pixels, and asserts their centroid sits on the target
;;;; tile (prints PASS/FAIL) -- proving the blast is centred.  Writes
;;;; flash/fireball/aftermath screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defparameter *na-w* 256) (defparameter *na-h* 160)

(defun na-render (painter s &optional frame)
  "Render the scene (optionally with detonation FRAME overlaid) and return a
fresh read-back surface; the caller frees it."
  (let ((ren (painter-ren painter)))
    (render-game painter s nil :fog nil :cam-x 0 :cam-y 0 :vw 16 :vh 10
                 :overlay (and frame (lambda (p) (draw-explosion-frame p frame (* 8 16) (* 5 16)))))
    (let ((surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format
                 0 *na-w* *na-h* 32 sdl2-ffi:+sdl-pixelformat-abgr8888+)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer)
        sdl2-ffi:+sdl-pixelformat-abgr8888+
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      surf)))

(defun na-save (painter s path &optional frame)
  (let ((surf (na-render painter s frame)))
    (sdl2-image:save-png surf (namestring path))
    (sdl2-ffi.functions:sdl-free-surface surf)))

(defun na-diff-centroid (base ov)
  "Centroid (values cx cy n) of pixels that differ between surfaces BASE and OV."
  (let ((pb (plus-c:c-ref base sdl2-ffi:sdl-surface :pixels))
        (po (plus-c:c-ref ov sdl2-ffi:sdl-surface :pixels))
        (pitch (plus-c:c-ref base sdl2-ffi:sdl-surface :pitch))
        (xs 0) (ys 0) (n 0))
    (dotimes (y *na-h*)
      (dotimes (x *na-w*)
        (let ((o (+ (* y pitch) (* x 4))))
          (unless (and (= (cffi:mem-ref pb :uint8 o) (cffi:mem-ref po :uint8 o))
                       (= (cffi:mem-ref pb :uint8 (+ o 1)) (cffi:mem-ref po :uint8 (+ o 1)))
                       (= (cffi:mem-ref pb :uint8 (+ o 2)) (cffi:mem-ref po :uint8 (+ o 2))))
            (incf xs x) (incf ys y) (incf n)))))
    (if (zerop n) (values 0 0 0) (values (round xs n) (round ys n) n))))

(defun nuke-anim-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "nuke-anim" :w *na-w* :h *na-h* :flags '(:hidden))
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
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1)
                  (painter-nuke painter) (load-atlas ren *nuke-image* +nuke-bg-key+))
            (setf (civm:relation s 1 2) :war)
            (let ((c (civm::register-city s :name "Aztec" :owner 2 :x 8 :y 5)))
              (setf (civm:city-size c) 8)
              (dolist (p '((7 . 4) (9 . 4) (7 . 6) (9 . 6)))
                (civm::register-unit s :type :legion :owner 2 :x (car p) :y (cdr p)))
              (let ((nuke (civm::register-unit s :type :nuclear :owner 1 :x 8 :y 5)))
                ;; --- centring check: diff a fireball frame against the base ---
                (let ((base (na-render painter s)) (ov (na-render painter s 8)))
                  (multiple-value-bind (cx cy n) (na-diff-centroid base ov)
                    (let ((tx (+ (* 8 16) 8)) (ty (+ (* 5 16) 8)))  ; target tile centre
                      (format t "~&Explosion centroid (~D,~D) vs target (~D,~D); ~D px~%" cx cy tx ty n)
                      (format t "=> ~A~%"
                              (if (and (plusp n) (<= (abs (- cx tx)) 8) (<= (abs (- cy ty)) 8))
                                  "PASS" "FAIL"))))
                  (sdl2-ffi.functions:sdl-free-surface base)
                  (sdl2-ffi.functions:sdl-free-surface ov))
                ;; --- screenshots: a few stages, then the aftermath ----------
                (na-save painter s (merge-pathnames "nuke-anim-flash.png" docs) 1)
                (na-save painter s (merge-pathnames "nuke-anim-fireball.png" docs) 8)
                (civm:apply-command s (list :nuke :unit (civm:unit-id nuke)))
                (na-save painter s (merge-pathnames "nuke-anim-aftermath.png" docs))
                (format t "Wrote docs/nuke-anim-{flash,fireball,aftermath}.png~%")))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'nuke-anim-demo)
