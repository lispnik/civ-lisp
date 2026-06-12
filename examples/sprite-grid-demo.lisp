;;;; sprite-grid-demo.lisp -- verify (and illustrate) that the sprite sheet's
;;;; leftover cyan cell-grid lines are stripped at load time, so overlay sprites
;;;; like the field fort (and walls / undefended cities) render cleanly.
;;;;
;;;;   sbcl --non-interactive --load examples/sprite-grid-demo.lisp
;;;;
;;;; It renders the same scene twice -- once with the old un-stripped atlas and
;;;; once with the real (stripped) one -- reads the rendered pixels back, asserts
;;;; the fort tile shows cyan on its top edge BEFORE and none AFTER (PASS/FAIL),
;;;; and writes both screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defparameter *grid-cyan* '(0 168 168))

(defun load-atlas-nostrip (ren path colorkey)
  "The old behavior: green-key the sprites but leave the cyan grid lines in."
  (let ((surf (sdl2-image:load-image (namestring path))))
    (let ((conv (sdl2-ffi.functions:sdl-convert-surface-format
                 surf sdl2-ffi:+sdl-pixelformat-abgr8888+ 0)))
      (sdl2-ffi.functions:sdl-free-surface surf) (setf surf conv))
    (colorkey-surface! surf colorkey)
    (let ((tex (sdl2:create-texture-from-surface ren surf)))
      (sdl2-ffi.functions:sdl-free-surface surf)
      (sdl2-ffi.functions:sdl-set-texture-blend-mode tex 1)
      tex)))

(defun sprite-grid/scene ()
  "A tiny game with an undefended city and a field fort on visible tiles."
  (let* ((state (civm:make-new-game :seed 4 :width 12 :height 9
                                    :players '("You" "Red")))
         (map (civm:gs-map state)))
    (loop for tl across (civm:map-tiles map) do (setf (civm:tile-hut tl) nil)) ; declutter
    (civm::register-city state :name "Rome" :owner 1 :x 4 :y 4)   ; undefended -> no border
    (setf (civm:tile-fort (civm:tile-at map 6 4)) t)               ; a field fort
    (civm:update-visibility state)
    state))

(defun count-cyan-top-edge (surf tile-col tile-row)
  "Count *grid-cyan* pixels along the top edge of the tile at (TILE-COL,TILE-ROW)
in the read-back ABGR8888 SURF (byte order R,G,B,A)."
  (destructuring-bind (cr cg cb) *grid-cyan*
    (let ((pitch (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
          (px (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels))
          (y (* tile-row *tile*)) (n 0))
      (loop for x from (* tile-col *tile*) below (* (1+ tile-col) *tile*)
            for o = (+ (* y pitch) (* x 4))
            do (when (and (= (cffi:mem-ref px :uint8 o) cr)
                          (= (cffi:mem-ref px :uint8 (+ o 1)) cg)
                          (= (cffi:mem-ref px :uint8 (+ o 2)) cb))
                 (incf n)))
      n)))

(defun sprite-grid/render (ren make-painter state path)
  "Render the scene with MAKE-PAINTER, save PATH, and return the read-back surface
(caller frees) so callers can inspect pixels."
  (let* ((painter (funcall make-painter))
         (w (* 12 *tile*)) (h (* 9 *tile*))
         (fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
         (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 w h 32 fmt)))
    (render-game painter state nil :fog nil :cam-x 0 :cam-y 0 :vw 12 :vh 9)
    (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
      (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
      (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
    (sdl2-image:save-png surf (namestring path))
    surf))

(defun sprite-grid-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "sprite-grid" :w (* 12 16) :h (* 9 16) :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let ((terrain (load-atlas ren *terrain-image*))
                (font (load-gfont (namestring *font-file*) 1))
                (state (sprite-grid/scene)))
            (flet ((painter-with (sprites)
                     (let ((p (make-renderer-painter ren sprites terrain)))
                       (setf (painter-font p) font) p)))
              (let* ((sb (sprite-grid/render
                          ren (lambda () (painter-with
                                          (load-atlas-nostrip ren *sprites-image* +unit-bg-key+)))
                          state (merge-pathnames "sprite-grid-before.png" docs)))
                     (sa (sprite-grid/render
                          ren (lambda () (painter-with
                                          (load-atlas ren *sprites-image* +unit-bg-key+)))
                          state (merge-pathnames "sprite-grid-after.png" docs)))
                     ;; fort is at tile (6,4); count cyan along its top edge
                     (before (count-cyan-top-edge sb 6 4))
                     (after  (count-cyan-top-edge sa 6 4)))
                (format t "~&Fort top-edge cyan pixels: before=~D, after=~D~%" before after)
                (format t "Artifact present before & gone after? => ~A~%"
                        (if (and (plusp before) (zerop after)) "PASS" "FAIL"))
                (format t "Wrote docs/sprite-grid-{before,after}.png~%")
                (sdl2-ffi.functions:sdl-free-surface sb)
                (sdl2-ffi.functions:sdl-free-surface sa)))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'sprite-grid-demo)
