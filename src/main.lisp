;;;; main.lisp -- SDL2 front-end: drive and render a civ-model game.
;;;;
;;;; Opens a window showing the map, units and cities from a GAME-STATE, scaled
;;;; globally by *SCALE*, with the torch image as the mouse cursor.  Keyboard
;;;; input is turned into civ-model COMMANDS -- the view never mutates the model
;;;; directly.
;;;;
;;;;   arrows / WASD : move selected unit      Tab : cycle selected unit
;;;;   B            : found city (settlers)    Enter : end turn
;;;;   Esc / close  : quit

(in-package #:civ-lisp)

(defparameter *cursor-image*
  (merge-pathnames "assets/torch.png"
                   (asdf:system-source-directory :civ-lisp))
  "The torch graphic extracted from Civilization, used as the mouse cursor.")

(defparameter *scale* 2
  "Global integer scale factor applied to the whole app.")

;;; --- cursor (the OS cursor is not affected by the render scale, so its
;;;     surface is upscaled separately) ----------------------------------------

(defun scale-surface (src factor)
  "Return a new SDL surface FACTOR times larger than SRC (nearest-neighbour,
alpha preserved).  Caller must free the result with SDL_FreeSurface."
  (if (= factor 1)
      src
      (let* ((w (sdl2:surface-width src))
             (h (sdl2:surface-height src))
             (fmt (plus-c:c-ref src sdl2-ffi:sdl-surface :format :format))
             (dst (sdl2-ffi.functions:sdl-create-rgb-surface-with-format
                   0 (* w factor) (* h factor) 32 fmt)))
        (when (cffi:null-pointer-p (autowrap:ptr dst))
          (error "SDL_CreateRGBSurfaceWithFormat failed: ~A"
                 (sdl2-ffi.functions:sdl-get-error)))
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

;;; --- selection helpers -----------------------------------------------------

(defun human-player-ids (state)
  (loop for p across (civm:gs-players state)
        when (eq (civm:player-kind p) :human) collect (civm:player-id p)))

(defun human-unit-ids (state)
  (let ((humans (human-player-ids state)))
    (sort (loop for id being the hash-keys of (civm:gs-units state)
                  using (hash-value u)
                when (member (civm:unit-owner u) humans) collect id)
          #'<)))

(defun first-human-unit (state) (first (human-unit-ids state)))

(defun next-human-unit (state current)
  (let ((ids (human-unit-ids state)))
    (cond ((null ids) nil)
          ((null current) (first ids))
          (t (or (cadr (member current ids)) (first ids))))))

(defun year-string (year)
  (if (minusp year) (format nil "~D BC" (- year)) (format nil "AD ~D" year)))

;;; --- main loop -------------------------------------------------------------

(defun run (&key (scale *scale*) (seed 0) (cursor-image *cursor-image*))
  "Open a window, start a new game, and render/drive it until quit."
  (sdl2:with-init (:video)
    (sdl2-image:init '(:png))
    (let* ((state (civm:make-new-game :seed seed))
           (map (civm:gs-map state))
           (lw (* (civm:map-width map) *tile*))
           (lh (* (civm:map-height map) *tile*))
           (selected (first-human-unit state)))
      (sdl2:with-window (win :title "civ-lisp" :w (* lw scale) :h (* lh scale)
                             :flags '(:shown))
        (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
          (sdl2-ffi.functions:sdl-render-set-scale ren (float scale 1.0)
                                                   (float scale 1.0))
          (let ((painter (make-renderer-painter ren
                                                (load-atlas ren *sprites-image*)
                                                (load-atlas ren *terrain-image*)))
                (torch (sdl2-image:load-image (namestring cursor-image))))
            (multiple-value-bind (cursor cursor-surface)
                (set-image-cursor torch :scale scale)
              (sdl2-ffi.functions:sdl-free-surface torch)
              (flet ((retitle ()
                       (sdl2:set-window-title
                        win (format nil "civ-lisp — turn ~D, ~A"
                                    (civm:gs-turn state)
                                    (year-string (civm:gs-year state)))))
                     (try (cmd)
                       (handler-case (civm:apply-command state cmd)
                         (civm:command-error () nil))))
                (retitle)
                (unwind-protect
                     (sdl2:with-event-loop (:method :poll)
                       (:quit () t)
                       (:keydown (:keysym k)
                         (let ((sc (sdl2:scancode-value k)))
                           (cond
                             ((sdl2:scancode= sc :scancode-escape)
                              (sdl2:push-quit-event))
                             ((sdl2:scancode= sc :scancode-return)
                              (try '(:end-turn))
                              (setf selected (first-human-unit state))
                              (retitle))
                             ((sdl2:scancode= sc :scancode-tab)
                              (setf selected (next-human-unit state selected)))
                             ((sdl2:scancode= sc :scancode-b)
                              (when selected
                                (try (list :found-city :unit selected :name "City"))
                                (setf selected (first-human-unit state))))
                             ((sdl2:scancode= sc :scancode-f)
                              (when selected
                                (try (list :fortify :unit selected))))
                             ((or (sdl2:scancode= sc :scancode-up)
                                  (sdl2:scancode= sc :scancode-w))
                              (when selected
                                (try (list :move-unit :unit selected :dx 0 :dy -1))))
                             ((or (sdl2:scancode= sc :scancode-down)
                                  (sdl2:scancode= sc :scancode-s))
                              (when selected
                                (try (list :move-unit :unit selected :dx 0 :dy 1))))
                             ((or (sdl2:scancode= sc :scancode-left)
                                  (sdl2:scancode= sc :scancode-a))
                              (when selected
                                (try (list :move-unit :unit selected :dx -1 :dy 0))))
                             ((or (sdl2:scancode= sc :scancode-right)
                                  (sdl2:scancode= sc :scancode-d))
                              (when selected
                                (try (list :move-unit :unit selected :dx 1 :dy 0)))))))
                       (:mousebuttondown (:x mx :y my)
                         ;; left-click sets a goto target for the selected unit
                         (when selected
                           (try (list :goto :unit selected
                                      :x (floor mx (* *tile* scale))
                                      :y (floor my (* *tile* scale))))))
                       (:idle ()
                         (render-game painter state selected)))
                  ;; cleanup
                  (sdl2:destroy-texture (painter-sprites painter))
                  (sdl2:destroy-texture (painter-terrain painter))
                  (sdl2-ffi.functions:sdl-free-cursor cursor)
                  (sdl2-ffi.functions:sdl-free-surface cursor-surface)
                  (sdl2-image:quit))))))))))

(defun main ()
  "Entry point.  On macOS the SDL event loop must run on the main thread."
  (sdl2:make-this-thread-main #'run))
