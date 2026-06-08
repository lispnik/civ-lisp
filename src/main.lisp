;;;; main.lisp -- open an SDL2 window and use the torch image as the cursor.
;;;;
;;;; The whole app is scaled by *SCALE* (default 2x) so the small original
;;;; Civilization assets are legible on a modern display.  Two things scale:
;;;;   1. everything drawn through the renderer -- via SDL_RenderSetScale, and
;;;;   2. the OS mouse cursor -- which the renderer never touches, so its
;;;;      surface is upscaled (nearest-neighbour) before the cursor is built.

(in-package #:civ-lisp)

(defparameter *cursor-image*
  (merge-pathnames "assets/torch.png"
                   (asdf:system-source-directory :civ-lisp))
  "The torch graphic extracted from Civilization, used as the mouse cursor.")

(defparameter *scale* 2
  "Global integer scale factor applied to the whole app.")

(defun scale-surface (src factor)
  "Return a new SDL surface FACTOR times larger than SRC (nearest-neighbour,
alpha preserved).  Caller must free the result with SDL_FreeSurface."
  (if (= factor 1)
      src
      (let* ((w (sdl2:surface-width src))
             (h (sdl2:surface-height src))
             ;; integer pixel-format enum (surface-format-format returns a keyword)
             (fmt (plus-c:c-ref src sdl2-ffi:sdl-surface :format :format))
             (dst (sdl2-ffi.functions:sdl-create-rgb-surface-with-format
                   0 (* w factor) (* h factor) 32 fmt)))
        (when (cffi:null-pointer-p (autowrap:ptr dst))
          (error "SDL_CreateRGBSurfaceWithFormat failed: ~A"
                 (sdl2-ffi.functions:sdl-get-error)))
        ;; copy source pixels (incl. alpha) verbatim, just stretched
        (sdl2-ffi.functions:sdl-set-surface-blend-mode src 0) ; SDL_BLENDMODE_NONE
        (sdl2-ffi.functions:sdl-upper-blit-scaled
         src (cffi:null-pointer) dst (cffi:null-pointer))
        dst)))

(defun set-image-cursor (surface &key (scale 1) (hot-x 0) (hot-y 0))
  "Build a colour cursor from SURFACE (scaled by SCALE) and make it active.
Returns the scaled cursor surface so the caller can free it on exit."
  (let* ((cur-surf (scale-surface surface scale))
         (cursor (sdl2-ffi.functions:sdl-create-color-cursor
                  cur-surf (* hot-x scale) (* hot-y scale))))
    (when (cffi:null-pointer-p (autowrap:ptr cursor))
      (error "SDL_CreateColorCursor failed: ~A"
             (sdl2-ffi.functions:sdl-get-error)))
    (sdl2-ffi.functions:sdl-set-cursor cursor)
    (sdl2:show-cursor)
    (values cursor cur-surf)))

(defun run (&key (width 640) (height 480) (scale *scale*)
                 (cursor-image *cursor-image*))
  "Open a WIDTH x HEIGHT SDL2 window scaled globally by SCALE, with the torch
image as the cursor (and drawn centred to show the scaling).  Quits on window
close or Escape."
  (sdl2:with-init (:video)
    (sdl2-image:init '(:png))
    (sdl2:with-window (win :title "civ-lisp" :w width :h height :flags '(:shown))
      (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
        ;; global scale: all renderer drawing is multiplied by SCALE
        (sdl2-ffi.functions:sdl-render-set-scale ren (float scale 1.0) (float scale 1.0))
        (let* ((surface (sdl2-image:load-image (namestring cursor-image)))
               (tw (sdl2:surface-width surface))
               (th (sdl2:surface-height surface))
               ;; logical (pre-scale) drawing area
               (lw (floor width scale))
               (lh (floor height scale)))
          (multiple-value-bind (cursor cursor-surface)
              (set-image-cursor surface :scale scale)
            (let ((tex (sdl2:create-texture-from-surface ren surface)))
              (unwind-protect
                   (sdl2:with-event-loop (:method :poll)
                     (:quit () t)
                     (:keydown (:keysym k)
                               (when (sdl2:scancode= (sdl2:scancode-value k)
                                                     :scancode-escape)
                                 (sdl2:push-quit-event)))
                     (:idle ()
                            (sdl2:set-render-draw-color ren 24 28 64 255)
                            (sdl2:render-clear ren)
                            ;; drawn in logical coords; SDL scales it up by SCALE
                            (sdl2:render-copy
                             ren tex
                             :dest-rect (sdl2:make-rect (- (floor lw 2) (floor tw 2))
                                                        (- (floor lh 2) (floor th 2))
                                                        tw th))
                            (sdl2:render-present ren)))
                (sdl2:destroy-texture tex)
                (sdl2-ffi.functions:sdl-free-cursor cursor)
                (sdl2-ffi.functions:sdl-free-surface cursor-surface)
                (sdl2-ffi.functions:sdl-free-surface surface)
                (sdl2-image:quit)))))))))

(defun main ()
  "Entry point.  On macOS the SDL event loop must run on the main thread."
  (sdl2:make-this-thread-main #'run))
