;;;; main.lisp -- open an SDL2 window and use the torch image as the cursor.

(in-package #:civ-lisp)

(defparameter *cursor-image*
  (merge-pathnames "assets/torch.png"
                   (asdf:system-source-directory :civ-lisp))
  "The torch graphic extracted from Civilization, used as the mouse cursor.")

(defun set-image-cursor (path &key (hot-x 0) (hot-y 0))
  "Load PATH as a colour cursor and make it the active mouse cursor.
Returns (values cursor surface) so the caller can free them on exit."
  (let* ((surface (sdl2-image:load-image (namestring path)))
         (cursor (sdl2-ffi.functions:sdl-create-color-cursor surface hot-x hot-y)))
    (when (cffi:null-pointer-p (autowrap:ptr cursor))
      (error "SDL_CreateColorCursor failed: ~A"
             (sdl2-ffi.functions:sdl-get-error)))
    (sdl2-ffi.functions:sdl-set-cursor cursor)
    (sdl2:show-cursor)
    (values cursor surface)))

(defun run (&key (width 640) (height 480) (cursor-image *cursor-image*))
  "Open an SDL2 window with the torch image as the cursor.  Closes on window
close or Escape."
  (sdl2:with-init (:video)
    (sdl2-image:init '(:png))
    (sdl2:with-window (win :title "civ-lisp"
                           :w width :h height :flags '(:shown))
      (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
        (multiple-value-bind (cursor surface) (set-image-cursor cursor-image)
          (unwind-protect
               (sdl2:with-event-loop (:method :poll)
                 (:quit () t)
                 (:keydown (:keysym keysym)
                           (when (sdl2:scancode= (sdl2:scancode-value keysym)
                                                 :scancode-escape)
                             (sdl2:push-quit-event)))
                 (:idle ()
                        (sdl2:set-render-draw-color ren 24 28 64 255)
                        (sdl2:render-clear ren)
                        (sdl2:render-present ren)))
            (sdl2-ffi.functions:sdl-free-cursor cursor)
            (sdl2-ffi.functions:sdl-free-surface surface)
            (sdl2-image:quit)))))))

(defun main ()
  "Entry point.  On macOS the SDL event loop must run on the main thread."
  (sdl2:make-this-thread-main #'run))