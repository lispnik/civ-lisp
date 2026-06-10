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

(defparameter *go-cursor-image*
  (merge-pathnames "assets/go.png" (asdf:system-source-directory :civ-lisp))
  "The Civilization \"Go\" arrow, used as the cursor while choosing a goto tile.")

(defparameter *font-file*
  (merge-pathnames "assets/fonts.cv" (asdf:system-source-directory :civ-lisp))
  "Civilization's bitmap font file (FONTS.CV); used for in-window text.")

(defparameter *scale* 2
  "Global integer scale factor applied to the whole app.")

;;; --- raw SDL_Event field access --------------------------------------------
;;; cl-sdl2's high-level accessors (scancode-value, the :x/:y event
;;; destructuring) read the wrong struct offsets for SDL2 2.x on arm64 macOS,
;;; yielding garbage.  We read the fields ourselves at the documented SDL_Event
;;; byte offsets, which is correct regardless of the (mismatched) FFI spec.

(defun ev-type (ev)     (cffi:mem-ref (autowrap:ptr ev) :uint32 0))
(defun ev-scancode (ev) (cffi:mem-ref (autowrap:ptr ev) :uint32 16)) ; key.keysym.scancode
(defun ev-mouse-x (ev)  (cffi:mem-ref (autowrap:ptr ev) :int32 20))  ; button.x
(defun ev-mouse-y (ev)  (cffi:mem-ref (autowrap:ptr ev) :int32 24))  ; button.y

(defconstant +ev-quit+ #x100)
(defconstant +ev-keydown+ #x300)
(defconstant +ev-mousebuttondown+ #x401)

;; SDL scancodes for the keys we use
(defconstant +sc-a+ 4) (defconstant +sc-b+ 5) (defconstant +sc-d+ 7)
(defconstant +sc-f+ 9) (defconstant +sc-g+ 10) (defconstant +sc-s+ 22)
(defconstant +sc-w+ 26) (defconstant +sc-return+ 40) (defconstant +sc-escape+ 41)
(defconstant +sc-tab+ 43) (defconstant +sc-right+ 79) (defconstant +sc-left+ 80)
(defconstant +sc-down+ 81) (defconstant +sc-up+ 82)
(defconstant +sc-kp-enter+ 88)          ; numpad Enter

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

(defun make-cursor (path scale &key (hot-x 0) (hot-y 0))
  "Load image PATH, scale it, and build a colour cursor (without activating it).
Returns the cursor (the loaded surfaces leak until process exit, which is fine)."
  (let* ((base (sdl2-image:load-image (namestring path)))
         (scaled (scale-surface base scale))
         (cursor (sdl2-ffi.functions:sdl-create-color-cursor
                  scaled (* hot-x scale) (* hot-y scale))))
    (when (cffi:null-pointer-p (autowrap:ptr cursor))
      (error "SDL_CreateColorCursor failed: ~A" (sdl2-ffi.functions:sdl-get-error)))
    cursor))

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

(defparameter *waited* (make-hash-table)
  "Unit id -> wait sequence number this turn; waited units cycle last.")
(defparameter *wait-seq* 0)

(defun cycle-key (id)
  "Sort key for the unit cycle: waited units sort after all un-waited ones."
  (let ((w (gethash id *waited*)))
    (if w (+ 1000000 w) id)))

(defun active-human-unit-ids (state)
  "Human unit ids for cycling: fortified and out-of-moves units excluded,
waited units sorted last."
  (sort (remove-if (lambda (id)
                     (let ((u (civm:unit-by-id state id)))
                       (or (eq (civm:unit-orders u) :fortified)
                           (<= (civm:unit-moves-left u) 0))))
                   (human-unit-ids state))
        #'< :key #'cycle-key))

(defun first-human-unit (state) (first (active-human-unit-ids state)))

(defun human-unit-at (state tx ty)
  "Id of one of the player's units standing on tile (TX,TY), or NIL -- includes
fortified units and city garrisons, so a click can wake them."
  (loop for id in (human-unit-ids state)
        for u = (civm:unit-by-id state id)
        when (and u (= (civm:unit-x u) tx) (= (civm:unit-y u) ty)
                  (> (civm:unit-moves-left u) 0))   ; spent units aren't selectable
          return id))

(defun next-human-unit (state current)
  (let ((ids (active-human-unit-ids state)))
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
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1) ; for fog dimming
          (let ((painter (make-renderer-painter ren
                                                (load-atlas ren *sprites-image*)
                                                (load-atlas ren *terrain-image*)))
                (font (load-gfont (namestring *font-file*) 1))   ; small label font
                (torch-cursor (make-cursor cursor-image scale))
                (go-cursor (make-cursor *go-cursor-image* scale))
                (goto-mode nil))
            (setf (painter-font painter) font)
            (sdl2-ffi.functions:sdl-set-cursor torch-cursor)
            (sdl2:show-cursor)
            (sdl2:raise-window win)        ; bring the window to the front / focus it
            (let ((ev (autowrap:alloc 'sdl2-ffi:sdl-event))
                  (running t))
              (labels ((torch! () (setf goto-mode nil)
                         (sdl2-ffi.functions:sdl-set-cursor torch-cursor))
                       (retitle ()
                         (sdl2:set-window-title
                          win (format nil "civ-lisp — turn ~D, ~A~@[   ~A~]"
                                      (civm:gs-turn state)
                                      (year-string (civm:gs-year state))
                                      (and goto-mode "[GO: click a destination]"))))
                       (try (cmd)
                         (handler-case (civm:apply-command state cmd)
                           (civm:command-error () nil)))
                       (step! (dx dy)
                         ;; move the selected unit, then auto-advance off it once
                         ;; it's spent (out of moves) or gone (lost in combat)
                         (when selected
                           (try (list :move-unit :unit selected :dx dx :dy dy))
                           (let ((u (civm:unit-by-id state selected)))
                             (when (or (null u) (<= (civm:unit-moves-left u) 0))
                               (setf selected (next-human-unit state selected)))))))
                (retitle)
                (unwind-protect
                     ;; manual poll loop, reading event fields at raw SDL offsets
                     (loop while running do
                       (loop while (/= 0 (sdl2-ffi.functions:sdl-poll-event ev)) do
                         (let ((type (ev-type ev)))
                           (cond
                             ((= type +ev-quit+) (setf running nil))
                             ((= type +ev-keydown+)
                              (let ((sc (ev-scancode ev)))
                                (cond
                                  ((= sc +sc-escape+)
                                   (if goto-mode (progn (torch!) (retitle))
                                       (setf running nil)))
                                  ((= sc +sc-g+)
                                   (when selected
                                     (setf goto-mode t)
                                     (sdl2-ffi.functions:sdl-set-cursor go-cursor)
                                     (retitle)))
                                  ((or (= sc +sc-return+) (= sc +sc-kp-enter+))
                                   (when goto-mode (torch!))
                                   (try '(:end-turn))
                                   (clrhash *waited*) (setf *wait-seq* 0)
                                   (setf selected (first-human-unit state))
                                   (retitle))
                                  ((= sc +sc-w+)
                                   ;; wait: send this unit to the end of the cycle
                                   (when selected
                                     (setf (gethash selected *waited*) (incf *wait-seq*))
                                     (setf selected (next-human-unit state selected))))
                                  ((= sc +sc-tab+)
                                   (setf selected (next-human-unit state selected)))
                                  ((= sc +sc-b+)
                                   (when selected
                                     (try (list :found-city :unit selected :name "City"))
                                     (setf selected (first-human-unit state))))
                                  ((= sc +sc-f+)
                                   (when selected (try (list :fortify :unit selected))))
                                  ((= sc +sc-up+) (step! 0 -1))
                                  ((= sc +sc-down+) (step! 0 1))
                                  ((= sc +sc-left+) (step! -1 0))
                                  ((= sc +sc-right+) (step! 1 0)))))
                             ((= type +ev-mousebuttondown+)
                              (let ((tx (floor (ev-mouse-x ev) (* *tile* scale)))
                                    (ty (floor (ev-mouse-y ev) (* *tile* scale))))
                                (cond
                                  ;; in goto mode: send the selected unit there
                                  ((and goto-mode selected)
                                   (try (list :goto :unit selected :x tx :y ty))
                                   (torch!) (retitle))
                                  ;; otherwise: select the unit/garrison on that
                                  ;; tile, waking it if it was fortified
                                  (t (let ((u (human-unit-at state tx ty)))
                                       (when u
                                         (setf selected u)
                                         (when (eq (civm:unit-orders
                                                    (civm:unit-by-id state u))
                                                   :fortified)
                                           (try (list :wake :unit u))))))))))))
                       (render-game painter state selected)
                       (sdl2:delay 16))
                  ;; cleanup
                  (sdl2:destroy-texture (painter-sprites painter))
                  (sdl2:destroy-texture (painter-terrain painter))
                  (sdl2-ffi.functions:sdl-free-cursor torch-cursor)
                  (sdl2-ffi.functions:sdl-free-cursor go-cursor)
                  (sdl2-image:quit))))))))))

(defun main ()
  "Entry point.  On macOS the SDL event loop must run on the main thread."
  (sdl2:make-this-thread-main #'run))
