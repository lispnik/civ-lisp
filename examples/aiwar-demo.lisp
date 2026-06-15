;;;; aiwar-demo.lisp -- verify (and illustrate) the smarter AI waging a land war:
;;;; it marches a field army on an enemy city and captures it, driven entirely by
;;;; its own turn logic (END-TURN -> RUN-AI-PLAYERS).
;;;;
;;;;   sbcl --non-interactive --load examples/aiwar-demo.lisp
;;;;
;;;; It sets up an AI army across the map from a lightly-defended enemy city, runs
;;;; end-turns, and asserts the AI vanguard advances and the city changes hands
;;;; (prints PASS/FAIL).  Writes start/march/captured screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun aiwar-demo/shot (painter state path)
  (let ((ren (painter-ren painter)))
    (render-game painter state nil :fog nil :cam-x 0 :cam-y 0 :vw 16 :vh 10)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 256 160 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun aiwar-demo/vanguard-x (state)
  (loop for u being the hash-values of (civm:gs-units state)
        when (and (= (civm:unit-owner u) 2) (eq (civm:unit-type u) :legion))
          maximize (civm:unit-x u)))

(defun aiwar-demo/athens (state)
  (loop for c being the hash-values of (civm:gs-cities state)
        when (string= (civm:city-name c) "Athens") return c))

(defun aiwar-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "aiwar-demo" :w 256 :h 160 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((s (civm::%make-game-state
                     :map (civm::make-game-map 16 10 :terrain :grassland)
                     :players (vector (civm::make-player :id 1 :name "You" :kind :human)
                                      (civm::make-player :id 2 :name "AI" :kind :ai))
                     :random (sb-ext:seed-random-state 5)))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            (setf (civm:relation s 1 2) :war)
            ;; player 1 (human) target city, lightly defended
            (let ((a (civm::register-city s :name "Athens" :owner 1 :x 12 :y 5)))
              (setf (civm:city-size a) 3)
              (civm::register-unit s :type :warriors :owner 1 :x 12 :y 5))
            ;; player 2 (AI): a home city + garrison, and a field army to send
            (civm::register-city s :name "Sparta" :owner 2 :x 3 :y 5)
            (civm::register-unit s :type :warriors :owner 2 :x 3 :y 5)
            (dolist (yy '(4 5 6)) (civm::register-unit s :type :legion :owner 2 :x 4 :y yy))
            (aiwar-demo/shot painter s (merge-pathnames "aiwar-start.png" docs))
            (format t "~&Turn 0: Athens owner ~D; AI army at x=4~%"
                    (civm:city-owner (aiwar-demo/athens s)))
            (let ((x0 (aiwar-demo/vanguard-x s)) (mid nil) (taken nil))
              (dotimes (i 20)
                (civm:end-turn s)
                (let ((a (aiwar-demo/athens s)))
                  (when (and (not mid) (>= (aiwar-demo/vanguard-x s) 8))
                    (setf mid t) (aiwar-demo/shot painter s (merge-pathnames "aiwar-march.png" docs)))
                  (when (and a (= (civm:city-owner a) 2))
                    (setf taken (1+ i))
                    (aiwar-demo/shot painter s (merge-pathnames "aiwar-captured.png" docs))
                    (return))))
              (format t "AI vanguard advanced from x=~D to x=~D~%" x0 (aiwar-demo/vanguard-x s))
              (format t "AI captured Athens? ~A (turn ~A)~%" (and taken t) taken)
              (format t "=> ~A~%"
                      (if (and taken (> (aiwar-demo/vanguard-x s) x0)) "PASS" "FAIL"))
              (format t "Wrote docs/aiwar-{start,march,captured}.png~%"))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'aiwar-demo)
