;;;; font-demo.lisp -- illustrate the bitmap fonts packed in assets/fonts.cv.
;;;;
;;;;   sbcl --non-interactive --load examples/font-demo.lisp
;;;;
;;;; Reads the font count from the FONTS.CV header, loads each font, and renders
;;;; a sample line per font (labelled with its index and pixel height) to
;;;; docs/fonts.png.  Font 1 is the small label font the HUD uses.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun font-demo ()
  (let* ((path (namestring *font-file*))
         (n (%u16 (%read-octets path) 0))
         (fonts (loop for i below n collect (load-gfont path i)))
         (small (nth 1 fonts))
         (sample "The quick brown fox  ABCabc 0123456789")
         (labels (loop for i below n collect (format nil "~D  h~D" i (gfont-height (nth i fonts)))))
         (labw (+ 6 (reduce #'max labels :key (lambda (s) (text-width small s)))))
         (rowhs (loop for f in fonts collect (+ 5 (max (gfont-height f) (gfont-height small)))))
         (w (+ 8 labw (reduce #'max fonts :key (lambda (f) (text-width f sample)))))
         (title-h 14)
         (h (+ title-h 4 (reduce #'+ rowhs))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "font-demo" :w w :h h :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let ((p (make-renderer-painter ren nil nil)))
            (sdl2:set-render-draw-color ren 16 22 40 255)
            (sdl2:render-clear ren)
            (draw-text p (nth 0 fonts) (format nil "FONTS.CV  --  ~D fonts" n) 4 3 255 230 120)
            (let ((y (+ title-h 2)))
              (loop for f in fonts for lbl in labels for rh in rowhs for i from 0 do
                (when (oddp i)                       ; faint stripe for legibility
                  (sdl2:set-render-draw-color ren 255 255 255 14)
                  (set-rect (painter-dst p) 0 (1- y) w rh)
                  (sdl2:render-fill-rect ren (painter-dst p)))
                (draw-text p small lbl 4 (+ y 1) 150 200 255)        ; index + height
                (draw-text p f sample labw y 235 235 235)            ; the font itself
                (incf y rh)))
            (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
                   (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 w h 32 fmt))
                   (out (merge-pathnames "docs/fonts.png" (asdf:system-source-directory :civ-lisp))))
              (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
                (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
                (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
              (sdl2-image:save-png surf (namestring out))
              (sdl2-ffi.functions:sdl-free-surface surf)
              (format t "~&wrote docs/fonts.png (~Dx~D, ~D fonts)~%" w h n))))))))

(sdl2:make-this-thread-main #'font-demo)
