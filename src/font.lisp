;;;; font.lisp -- draw text with Civilization's bitmap fonts (FONTS.CV).
;;;;
;;;; Parses one font out of FONTS.CV (see civ-extract for the full format) and
;;;; draws strings pixel-by-pixel through the renderer.  Glyphs are 1bpp,
;;;; MSB-first, row-interleaved across all characters; metadata sits in the 7
;;;; bytes before the font's data pointer, preceded by the per-char width table.

(in-package #:civ-lisp)

(defstruct gfont
  bytes off first last byte-length height charcount widths)

(defun %read-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      buf)))

(defun %u16 (b i) (logior (aref b i) (ash (aref b (1+ i)) 8)))

(defun load-gfont (path index)
  "Load font INDEX from the FONTS.CV file at PATH."
  (let* ((bytes (%read-octets path))
         (off (%u16 bytes (+ 2 (* 2 index))))
         (first (aref bytes (- off 8)))
         (last (aref bytes (- off 7)))
         (byte-len (aref bytes (- off 6)))
         (top (aref bytes (- off 5)))
         (bottom (aref bytes (- off 4)))
         (cc (1+ (- last first)))
         (widths (make-array cc)))
    (dotimes (ci cc)
      (setf (aref widths ci) (aref bytes (+ (- off 9 cc) 1 ci))))
    (make-gfont :bytes bytes :off off :first first :last last
                :byte-length byte-len :height (1+ (- bottom top))
                :charcount cc :widths widths)))

(defun gfont-bit (f ci row x)
  (logbitp (- 7 (mod x 8))
           (aref (gfont-bytes f)
                 (+ (gfont-off f) (* ci (gfont-byte-length f))
                    (* row (* (gfont-byte-length f) (gfont-charcount f)))
                    (floor x 8)))))

(defun char-px-width (f ch)
  (let ((c (char-code ch)))
    (if (and (>= c (gfont-first f)) (<= c (gfont-last f)))
        (min (aref (gfont-widths f) (- c (gfont-first f)))
             (* (gfont-byte-length f) 8))
        3)))                                  ; unknown char: blank space

(defun text-width (f str)
  (loop for ch across str sum (1+ (char-px-width f ch))))

(defun draw-text (painter f str x y r g b)
  "Draw STR at logical (X,Y) in colour (R,G,B), one filled pixel per glyph bit."
  (let ((ren (painter-ren painter)) (pen x))
    (sdl2:set-render-draw-color ren r g b 255)
    (loop for ch across str
          for c = (char-code ch)
          do (when (and (>= c (gfont-first f)) (<= c (gfont-last f)))
               (let ((ci (- c (gfont-first f)))
                     (w (char-px-width f ch)))
                 (dotimes (row (gfont-height f))
                   (dotimes (gx w)
                     (when (gfont-bit f ci row gx)
                       (set-rect (painter-dst painter) (+ pen gx) (+ y row) 1 1)
                       (sdl2:render-fill-rect ren (painter-dst painter)))))))
             (incf pen (1+ (char-px-width f ch))))))

(defun draw-label (painter f str cx top)
  "Draw STR centred horizontally on CX, its top at TOP, as white text on a
translucent dark bar (for legible city names over the map)."
  (let* ((tw (text-width f str))
         (x (- cx (floor tw 2)))
         (ren (painter-ren painter)))
    (sdl2:set-render-draw-color ren 0 0 0 170)
    (set-rect (painter-dst painter) (1- x) (1- top) (+ tw 2) (+ (gfont-height f) 2))
    (sdl2:render-fill-rect ren (painter-dst painter))
    (draw-text painter f str x top 255 255 255)))
