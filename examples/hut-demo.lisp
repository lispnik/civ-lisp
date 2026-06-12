;;;; hut-demo.lisp -- verify (and illustrate) tribal huts: moving a unit onto a
;;;; hut springs an outcome and consumes the hut -- matching Civ1's goody huts.
;;;;
;;;;   sbcl --non-interactive --load examples/hut-demo.lisp
;;;;
;;;; It drives the real view headlessly (a hidden window + software renderer),
;;;; asserts the hut is consumed and an outcome reported after a single move
;;;; (prints PASS/FAIL), and writes before/after screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun hut-demo/shot (painter state path cam-x cam-y)
  "Render STATE through the fog-of-war view at (CAM-X,CAM-Y) and save a PNG."
  (let ((ren (painter-ren painter)))
    (render-game painter state nil :fog t :cam-x cam-x :cam-y cam-y :vw 20 :vh 15)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 320 240 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun hut-demo/setup (seed)
  "Build a game and plant a hut directly east of the human warrior.
Returns (values state warrior cam-x cam-y)."
  (let* ((state (civm:make-new-game :seed seed :width 30 :height 20
                                    :players '("You" "Red") :barbarians t))
         (u (loop for x being the hash-values of (civm:gs-units state)
                  when (and (= (civm:unit-owner x) 1)
                            (eq (civm:unit-type x) :warriors)) return x))
         (map (civm:gs-map state))
         (hx (civm:wrap-x map (1+ (civm:unit-x u)))) (hy (civm:unit-y u))
         (htile (civm:tile-at map hx hy)))
    (when (eq (civm:tile-terrain htile) :ocean)       ; make sure it's walkable land
      (setf (civm:tile-terrain htile) :grassland))
    (setf (civm:tile-hut htile) t)
    (civm:update-visibility state)                    ; reveal the hut to the human
    (values state u (max 0 (- (civm:unit-x u) 9)) (max 0 (- hy 7)))))

(defun hut-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "hut-demo" :w 320 :h 240 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let ((painter (make-renderer-painter ren
                           (load-atlas ren *sprites-image* +unit-bg-key+)
                           (load-atlas ren *terrain-image*))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            ;; pick a seed whose hut yields a visible newcomer (a unit) so the
            ;; before/after picture clearly shows what the hut gave us
            (let ((chosen 2))
              (dolist (seed '(2 5 7 11 13 17 19 23 29 31 37 41))
                (multiple-value-bind (st u cx cy) (hut-demo/setup seed)
                  (declare (ignore cx cy))
                  (let ((n0 (hash-table-count (civm:gs-units st))))
                    (civm:apply-command st (list :move-unit :unit (civm:unit-id u) :dx 1 :dy 0))
                    (when (> (hash-table-count (civm:gs-units st)) n0)
                      (setf chosen seed) (return)))))
              ;; final run: render before, step onto the hut, verify, render after
              (multiple-value-bind (st u cx cy) (hut-demo/setup chosen)
                (let ((hx (civm:wrap-x (civm:gs-map st) (1+ (civm:unit-x u))))
                      (hy (civm:unit-y u)))
                  (format t "~&seed ~D: warrior at (~D,~D), hut at (~D,~D)~%"
                          chosen (civm:unit-x u) (civm:unit-y u) hx hy)
                  (hut-demo/shot painter st (merge-pathnames "hut-before.png" docs) cx cy)
                  (civm:apply-command st (list :move-unit :unit (civm:unit-id u) :dx 1 :dy 0))
                  (let ((gone (null (civm:tile-hut (civm:tile-at (civm:gs-map st) hx hy))))
                        (msg (civm:gs-message st)))
                    (format t "Outcome: ~A~%" msg)
                    (format t "Hut consumed? ~A   Outcome reported? ~A   => ~A~%"
                            gone (and msg t)
                            (if (and gone (stringp msg)) "PASS" "FAIL")))
                  (hut-demo/shot painter st (merge-pathnames "hut-after.png" docs) cx cy)
                  (format t "Wrote docs/hut-{before,after}.png~%"))))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'hut-demo)
