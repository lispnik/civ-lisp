;;;; font-glyphs-demo.lisp -- a glyph specimen sheet for the bitmap fonts in
;;;; assets/fonts.cv: each font's name (index + metrics) and its full glyph set.
;;;;
;;;;   sbcl --non-interactive --load examples/font-glyphs-demo.lisp
;;;;
;;;; For every font in FONTS.CV it draws a label (index, pixel height, glyph
;;;; count and code range) followed by every glyph the font defines, wrapped to
;;;; the page width, and writes the sheet to docs/font-glyphs.png.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun font-glyphs-demo/string (f)
  "Every character F defines, FIRST..LAST, as a string."
  (coerce (loop for c from (gfont-first f) to (gfont-last f) collect (code-char c)) 'string))

(defun font-glyphs-demo/wrap-lines (f str maxw)
  "How many lines STR needs in font F within MAXW px."
  (let ((pen 0) (lines 1))
    (loop for ch across str for cw = (1+ (char-px-width f ch))
          do (when (> (+ pen cw) maxw) (setf pen 0) (incf lines)) (incf pen cw))
    lines))

(defun font-glyphs-demo/draw (p f str x y maxw)
  "Draw STR in font F wrapped within MAXW; return the y past the last line."
  (let ((pen x) (cy y) (lh (+ 2 (gfont-height f))))
    (loop for ch across str for cw = (1+ (char-px-width f ch))
          do (when (> (+ (- pen x) cw) maxw) (setf pen x cy (+ cy lh)))
             (draw-text p f (string ch) pen cy 235 235 235)
             (incf pen cw))
    (+ cy lh)))

(defun font-glyphs-demo ()
  (let* ((path (namestring *font-file*))
         (n (%u16 (%read-octets path) 0))
         (fonts (loop for i below n collect (load-gfont path i)))
         (small (nth 1 fonts))
         (w 720) (maxw (- w 16)) (sh (gfont-height small))
         (heights (loop for f in fonts
                        collect (+ sh 3 (* (font-glyphs-demo/wrap-lines f (font-glyphs-demo/string f) maxw)
                                           (+ 2 (gfont-height f))) 8)))
         (h (+ 18 (reduce #'+ heights))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "font-glyphs-demo" :w w :h h :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let ((p (make-renderer-painter ren nil nil)) (y 18))
            (sdl2:set-render-draw-color ren 16 22 40 255)
            (sdl2:render-clear ren)
            (draw-text p (nth 0 fonts) (format nil "FONTS.CV glyph specimens (~D fonts)" n)
                       4 4 255 230 120)
            (loop for f in fonts for i from 0 do
              (sdl2:set-render-draw-color ren 255 255 255 12)        ; separator stripe
              (set-rect (painter-dst p) 0 (1- y) w 1)
              (sdl2:render-fill-rect ren (painter-dst p))
              (draw-text p small
                         (format nil "Font ~D    ~Dpx    ~D glyphs (~D-~D)"
                                 i (gfont-height f) (gfont-charcount f)
                                 (gfont-first f) (gfont-last f))
                         4 (+ y 2) 150 200 255)
              (setf y (+ (font-glyphs-demo/draw p f (font-glyphs-demo/string f) 8 (+ y 3 sh) maxw) 8)))
            (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
                   (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 w h 32 fmt))
                   (out (merge-pathnames "docs/font-glyphs.png" (asdf:system-source-directory :civ-lisp))))
              (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
                (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
                (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
              (sdl2-image:save-png surf (namestring out))
              (sdl2-ffi.functions:sdl-free-surface surf)
              (format t "~&wrote docs/font-glyphs.png (~Dx~D)~%" w h))))))))

(sdl2:make-this-thread-main #'font-glyphs-demo)
