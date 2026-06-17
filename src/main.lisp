;;;; main.lisp -- SDL2 front-end: drive and render a civ-model game.
;;;;
;;;; Opens a window showing the map, units and cities from a GAME-STATE, scaled
;;;; globally by *SCALE*, with the torch image as the mouse cursor.  Keyboard
;;;; input is turned into civ-model COMMANDS -- the view never mutates the model
;;;; directly.
;;;;
;;;;   arrows / numpad : move selected unit (numpad moves diagonally too)
;;;;   B  : found city (settlers)     F : fortify
;;;;   R / I / M / T : road (then railroad) / irrigate / mine / fort
;;;;   C : clear forest      P : clean pollution
;;;;   G  : goto (then click)         Enter : end turn
;;;;   V  : revolution    Y : diplomacy    E : trade
;;;;   , / . : luxury rate    [ / ] : tax rate (science takes the remainder)
;;;;   Z / X : diplomat steal tech / sabotage   D : diplomat (spy) action menu
;;;;   H / J : caravan help build wonder / establish a trade route
;;;;   ?  : help overlay        ~ : Lisp console (evals a form; Esc closes)
;;;;   K  : start/stop the Slynk server (connect from Emacs with M-x sly-connect)
;;;;   S / L : save / load game       Esc / close : quit

(in-package #:civ-lisp)

(defparameter +torch-cursor-cell+ '(7 . 2)
  "SP257 col 7, row 2: the torch graphic, used as the default mouse cursor.")

(defparameter +go-cursor-cell+ '(2 . 2)
  "SP257 col 2, row 2: the \"GO\" cursor, used while choosing a goto tile.")

(defparameter *font-file*
  (merge-pathnames "assets/fonts.cv" (asdf:system-source-directory :civ-lisp))
  "Civilization's bitmap font file (FONTS.CV); used for in-window text.")

(defparameter *scale* 1
  "Global integer scale factor applied to the whole app (1 = native, unscaled).")

;;; --- raw SDL_Event field access --------------------------------------------
;;; cl-sdl2's high-level accessors (scancode-value, the :x/:y event
;;; destructuring) read the wrong struct offsets for SDL2 2.x on arm64 macOS,
;;; yielding garbage.  We read the fields ourselves at the documented SDL_Event
;;; byte offsets, which is correct regardless of the (mismatched) FFI spec.

(defun ev-type (ev)     (cffi:mem-ref (autowrap:ptr ev) :uint32 0))
(defun ev-scancode (ev) (cffi:mem-ref (autowrap:ptr ev) :uint32 16)) ; key.keysym.scancode
(defun ev-mouse-x (ev)  (cffi:mem-ref (autowrap:ptr ev) :int32 20))  ; button.x
(defun ev-mouse-y (ev)  (cffi:mem-ref (autowrap:ptr ev) :int32 24))  ; button.y
(defun ev-keymod (ev)   (cffi:mem-ref (autowrap:ptr ev) :uint16 24)) ; key.keysym.mod
(defun ev-ctrl-p (ev)   (logtest (ev-keymod ev) #xc0))               ; L/R Ctrl held
(defun ev-shift-p (ev)  (logtest (ev-keymod ev) #x3))                ; L/R Shift held

(defun ev-text (ev)
  "The UTF-8 text of an SDL_TEXTINPUT event (text char[32] at byte offset 12)."
  (let ((ptr (autowrap:ptr ev)))
    (with-output-to-string (s)
      (loop for i from 12 below 44
            for b = (cffi:mem-ref ptr :uint8 i)
            until (zerop b) do (write-char (code-char b) s)))))

(defconstant +ev-quit+ #x100)
(defconstant +ev-keydown+ #x300)
(defconstant +ev-textinput+ #x303)
(defconstant +ev-mousebuttondown+ #x401)

;; SDL scancodes for the keys we use
(defconstant +sc-a+ 4) (defconstant +sc-b+ 5) (defconstant +sc-c+ 6) (defconstant +sc-d+ 7)
(defconstant +sc-e+ 8) (defconstant +sc-k+ 14)
(defconstant +sc-f+ 9) (defconstant +sc-g+ 10) (defconstant +sc-h+ 11) (defconstant +sc-i+ 12)
(defconstant +sc-j+ 13)
(defconstant +sc-l+ 15) (defconstant +sc-m+ 16) (defconstant +sc-r+ 21)
(defconstant +sc-n+ 17) (defconstant +sc-o+ 18) (defconstant +sc-p+ 19)
(defconstant +sc-q+ 20) (defconstant +sc-s+ 22) (defconstant +sc-t+ 23)
(defconstant +sc-v+ 25) (defconstant +sc-u+ 24)
(defconstant +sc-x+ 27) (defconstant +sc-y+ 28) (defconstant +sc-z+ 29)
(defconstant +sc-w+ 26) (defconstant +sc-return+ 40) (defconstant +sc-escape+ 41)
(defconstant +sc-tab+ 43)
(defconstant +sc-minus+ 45) (defconstant +sc-equals+ 46)   ; specialists -/+
(defconstant +sc-comma+ 54) (defconstant +sc-period+ 55)   ; luxury down / up
(defconstant +sc-lbracket+ 47) (defconstant +sc-rbracket+ 48)  ; tax down / up
(defconstant +sc-slash+ 56)             ; '/' (Shift+/ = '?'): toggle help
(defconstant +sc-right+ 79) (defconstant +sc-left+ 80)
(defconstant +sc-down+ 81) (defconstant +sc-up+ 82)
(defconstant +sc-backspace+ 42) (defconstant +sc-grave+ 53)  ; backspace, `~` console
(defconstant +sc-kp-enter+ 88)          ; numpad Enter
;; numpad 1-9 (8-way movement); SDL scancodes 89..97
(defconstant +sc-kp-1+ 89) (defconstant +sc-kp-2+ 90) (defconstant +sc-kp-3+ 91)
(defconstant +sc-kp-4+ 92) (defconstant +sc-kp-6+ 94)
(defconstant +sc-kp-7+ 95) (defconstant +sc-kp-8+ 96) (defconstant +sc-kp-9+ 97)
(defconstant +sc-1+ 30)                 ; '1'..'9' are 30..38

(defparameter *save-path*
  (merge-pathnames "civ-save.lisp" (asdf:system-source-directory :civ-lisp))
  "Single-slot quicksave file (S saves, L loads).")

(defparameter *hof-path*
  (merge-pathnames "civ-halloffame.lisp" (asdf:system-source-directory :civ-lisp))
  "Hall of Fame: the best finished games, ranked by score.")

(defparameter *autosave-path*
  (merge-pathnames "civ-autosave.lisp" (asdf:system-source-directory :civ-lisp))
  "Written at the end of every turn; the setup screen offers to resume it.")

(defun read-hall-of-fame ()
  "The saved Hall of Fame records (a list of plists), or NIL."
  (handler-case
      (when (probe-file *hof-path*)
        (with-open-file (in *hof-path*) (read in nil nil)))
    (error () nil)))

(defun record-hall-of-fame (state)
  "Append the human's finished-game result and keep the top ten by score.
Returns the updated record list."
  (let ((human (human-player state)))
    (when human
      (let* ((rec (list :civ (civm:player-name human)
                        :score (civm:player-score human)
                        :difficulty (civm:gs-difficulty state)
                        :year (civm:gs-year state)
                        :won (eql (civm:gs-winner state) (civm:player-id human))
                        :victory (civm:gs-victory state)))
             (all (subseq (sort (cons rec (read-hall-of-fame)) #'>
                                :key (lambda (r) (getf r :score 0)))
                          0 (min 10 (1+ (length (read-hall-of-fame)))))))
        (handler-case
            (with-open-file (out *hof-path* :direction :output
                                            :if-exists :supersede :if-does-not-exist :create)
              (prin1 all out))
          (error () nil))
        all))))

(defun human-city-at (state tx ty)
  "City id of a human-owned city on tile (TX,TY), or NIL."
  (let ((tile (civm:tile-at (civm:gs-map state) tx ty)))
    (let ((cid (and tile (civm:tile-city tile))))
      (when cid
        (let ((c (civm:city-by-id state cid)))
          (when (eq (civm:player-kind (civm:player-by-id state (civm:city-owner c)))
                    :human)
            cid))))))

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

(defun cell-surface (sheet col row &optional (size *tile*))
  "Copy the SIZE x SIZE sprite cell at (COL,ROW) of SHEET into a fresh surface
(alpha preserved).  Caller must free the result with SDL_FreeSurface."
  (let* ((fmt (plus-c:c-ref sheet sdl2-ffi:sdl-surface :format :format))
         (dst (sdl2-ffi.functions:sdl-create-rgb-surface-with-format
               0 size size 32 fmt))
         (src (sdl2:make-rect (* col size) (* row size) size size)))
    (when (cffi:null-pointer-p (autowrap:ptr dst))
      (error "SDL_CreateRGBSurfaceWithFormat failed: ~A"
             (sdl2-ffi.functions:sdl-get-error)))
    (sdl2-ffi.functions:sdl-set-surface-blend-mode sheet 0) ; copy RGBA verbatim
    (sdl2-ffi.functions:sdl-upper-blit sheet src dst (cffi:null-pointer))
    dst))

(defun make-sprite-cursor (sheet cell scale &key (hot-x 0) (hot-y 0))
  "Build a colour cursor from the sprite CELL (col . row) of SHEET, scaled to the
render scale (without activating it)."
  (let* ((base (cell-surface sheet (car cell) (cdr cell)))
         (scaled (scale-surface base scale))
         (cursor (sdl2-ffi.functions:sdl-create-color-cursor
                  scaled (* hot-x scale) (* hot-y scale))))
    (sdl2-ffi.functions:sdl-free-surface base)
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

;;; --- Slynk (connect from Emacs) --------------------------------------------

(defvar *slynk-port* nil "Port of the running Slynk server, or NIL.")

(defun start-slynk (&optional (port 4005))
  "Load Slynk (if needed) and start a server on PORT so you can attach to the
running game from Emacs: M-x sly-connect, host localhost, port PORT.  Safe to
call from the `~` console -- e.g. (start-slynk).  Returns the port."
  (if *slynk-port*
      (format t "~&Slynk already listening on port ~D~%" *slynk-port*)
      (progn
        (handler-case (asdf:load-system :slynk)
          (error (e)
            (error "couldn't load Slynk (~A); is SLY/Slynk on your asdf path?" e)))
        (uiop:symbol-call :slynk :create-server :port port :dont-close t)
        (setf *slynk-port* port)
        (format t "~&Slynk listening on ~D -- M-x sly-connect RET localhost RET ~D RET~%"
                port port)))
  *slynk-port*)

(defun stop-slynk ()
  "Stop the running Slynk server, if any."
  (when *slynk-port*
    (ignore-errors (uiop:symbol-call :slynk :stop-server *slynk-port*))
    (format t "~&Slynk on ~D stopped~%" *slynk-port*)
    (setf *slynk-port* nil))
  *slynk-port*)

;;; --- `~` Lisp console ------------------------------------------------------

(defvar *state* nil
  "Handle on the live game state.  RUN sets it when a game starts or is loaded,
so it is reachable both from the `~` console and from a SLY connection
(e.g. (civm:gs-turn civ-lisp::*state*)).")

(defun split-lines (s)
  (loop with start = 0
        for i = (position #\Newline s :start start)
        collect (subseq s start (or i (length s)))
        while i do (setf start (1+ i))))

(defun console-eval (input state)
  "Read and evaluate INPUT (a Lisp form) with *STATE* bound to the game; return a
list of display lines: the echoed input, any printed output, then the value(s),
or an error message."
  (let ((*state* state) (*package* (find-package :civ-lisp)))
    (handler-case
        (let* ((out (make-string-output-stream))
               (form (read-from-string input))
               (vals (multiple-value-list
                      (let ((*standard-output* out)) (eval form))))
               (printed (get-output-stream-string out)))
          (append (list (format nil "> ~A" input))
                  (when (plusp (length printed)) (split-lines printed))
                  (or (mapcar #'prin1-to-string vals) (list "; no values"))))
      (error (e) (list (format nil "> ~A" input) (format nil "ERROR: ~A" e))))))

;;; --- main loop -------------------------------------------------------------

(defparameter *civilizations* '("You" "Rome" "Egypt" "Zulu")
  "The civilizations in a new game; the first is the human player.")

;; *view-cols* / *view-rows* now live in view.lisp (the HUD panes anchor to them)

(defun clamp-cam-y (state cy)
  "Clamp a camera row so the viewport stays within the (non-wrapping) poles."
  (max 0 (min cy (max 0 (- (civm:map-height (civm:gs-map state)) *view-rows*)))))

(defparameter *setup-map-sizes*
  '(("Tiny" 30 20) ("Small" 40 26) ("Standard" 60 40) ("Large" 80 50) ("Huge" 100 60))
  "Map-size presets for the setup screen: (label width height).")

(defun setup-screen (painter win ren)
  "Run the new-game setup screen and return a MAKE-NEW-GAME argument plist, or
NIL if the player quits.  Choose civilization, difficulty, rivals, and map size."
  (declare (ignore win))
  (let* ((nations (mapcar #'car civm:*nation-city-names*))
         (diffs (mapcar #'car civm:*difficulties*))
         (civ 0) (diff 2) (rivals 3) (size 2)   ; defaults: Prince, 3 rivals, Standard
         (sel 0)
         (can-resume (probe-file *autosave-path*))   ; an autosaved game to continue
         (ev (autowrap:alloc 'sdl2-ffi:sdl-event))
         (result :pending))
    (labels ((nat (k) (string-capitalize (symbol-name k)))
             (rows ()
               (let* ((k (nth civ nations))
                      (techs (civm:nation-starting-techs (nat k))))
                 (list (list "Civilization"
                             (format nil "~A~@[ (~A)~]" (nat k)
                                     (and techs (civm:tech-def (first techs) :name))))
                       (list "Difficulty" (string-capitalize (symbol-name (nth diff diffs))))
                       (list "Rivals"     (format nil "~D" rivals))
                       (list "Map"        (first (nth size *setup-map-sizes*))))))
             (change (d)
               (case sel
                 (0 (setf civ (mod (+ civ d) (length nations))))
                 (1 (setf diff (mod (+ diff d) (length diffs))))
                 (2 (setf rivals (max 1 (min 6 (+ rivals d)))))
                 (3 (setf size (mod (+ size d) (length *setup-map-sizes*))))))
             (build ()
               (let* ((human (nat (nth civ nations)))
                      (others (remove (nth civ nations) nations))
                      (rivnames (mapcar #'nat (subseq others 0 (min rivals (length others)))))
                      (sz (nth size *setup-map-sizes*)))
                 (list :seed (logand (sdl2-ffi.functions:sdl-get-ticks) #xffff)
                       :players (cons human rivnames) :barbarians t
                       :difficulty (nth diff diffs)
                       :width (second sz) :height (third sz)))))
      (unwind-protect
          (progn
            (sdl2-ffi.functions:sdl-stop-text-input)   ; arrows/Enter only here
            (loop while (eq result :pending) do
              (loop while (/= 0 (sdl2-ffi.functions:sdl-poll-event ev)) do
                (let ((type (ev-type ev)))
                  (cond
                    ((= type +ev-quit+) (setf result nil))
                    ((= type +ev-keydown+)
                     (let ((sc (ev-scancode ev)))
                       (cond
                         ((= sc +sc-escape+) (setf result nil))
                         ((or (= sc +sc-return+) (= sc +sc-kp-enter+)) (setf result (build)))
                         ((and can-resume (= sc +sc-r+)) (setf result :resume))
                         ((= sc +sc-up+)    (setf sel (mod (1- sel) 4)))
                         ((= sc +sc-down+)  (setf sel (mod (1+ sel) 4)))
                         ((= sc +sc-left+)  (change -1))
                         ((= sc +sc-right+) (change 1))))))))
              (sdl2:set-render-draw-color ren 16 24 48 255)
              (sdl2:render-clear ren)
              (draw-setup painter (* *view-cols* *tile*) (* *view-rows* *tile*) (rows) sel
                          can-resume)
              (sdl2:render-present ren)
              (sdl2:delay 16))
            (and (not (eq result :pending)) result))
        (autowrap:free ev)))))

(defun run (&key (scale *scale*) (seed 0)
                 (players *civilizations*) (width 80) (height 50))
  "Open a window, show the setup screen, then play the chosen game until quit.
SEED/PLAYERS/WIDTH/HEIGHT are legacy args; the setup screen now sets these."
  (declare (ignore seed players width height))
  (sdl2:with-init (:video)
    (sdl2-image:init '(:png))
    (let* ((lw (* *view-cols* *tile*))      ; the window is a fixed viewport,
           (lh (* *view-rows* *tile*))       ; not the whole (scrolling) map
           (cam-x 0) (cam-y 0))
      (sdl2:with-window (win :title "civ-lisp" :w (* lw scale) :h (* lh scale)
                             :flags '(:shown))
        (sdl2:with-renderer (ren win :flags '(:accelerated :presentvsync))
          (sdl2-ffi.functions:sdl-render-set-scale ren (float scale 1.0)
                                                   (float scale 1.0))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1) ; for fog dimming
          (let* ((painter (make-renderer-painter ren
                                                 (load-atlas ren *sprites-image* +unit-bg-key+)
                                                 (load-atlas ren *terrain-image*)))
                 (font (load-gfont (namestring *font-file*) 1))   ; small label font
                 ;; cursors are sliced from the sprite sheet (a one-shot surface load)
                 (cursor-sheet (sdl2-image:load-image (namestring *sprites-image*)))
                 (torch-cursor (make-sprite-cursor cursor-sheet +torch-cursor-cell+ scale))
                 (go-cursor (make-sprite-cursor cursor-sheet +go-cursor-cell+ scale))
                 (goto-mode nil))
            (sdl2-ffi.functions:sdl-free-surface cursor-sheet)
            (setf (painter-font painter) font
                  (painter-nuke painter) (load-atlas ren *nuke-image* +nuke-bg-key+)
                  ;; resource icons sit on a teal grid; key it out for clean blits
                  (painter-icons painter) (load-atlas ren *sprites-image* +icon-bg-key+))
            (sdl2-ffi.functions:sdl-set-cursor torch-cursor)
            (sdl2:show-cursor)
            (sdl2:raise-window win)        ; bring the window to the front / focus it
            (let ((cfg (setup-screen painter win ren)))   ; choose civ/difficulty/map
             (when cfg                                    ; NIL = quit at the setup screen
              (let* ((state (if (eq cfg :resume)
                                (civm:load-game *autosave-path*)   ; continue last game
                                (apply #'civm:make-new-game cfg)))
                     (selected (first-human-unit state)))
               (setf *state* state)          ; publish the live game for the console / SLY
               (sdl2-ffi.functions:sdl-set-cursor torch-cursor)
               (let ((ev (autowrap:alloc 'sdl2-ffi:sdl-event))
                  (running t)
                  (build-city nil)    ; city id whose build menu is open
                  (gov-menu nil)      ; T while the revolution menu is open
                  (diplo-menu nil)    ; T while the diplomacy menu is open
                  (trade-menu nil)    ; T while the trade menu is open
                  (research-menu nil) ; T while the "choose next advance" chooser is open
                  (hud-right nil)     ; T to dock the status pane + minimap on the right
                  (naming nil)        ; unit id founding a city while its name is typed
                  (name-input "")     ; the city name being entered
                  (skip-text nil)     ; eat the TEXTINPUT from the key that opened a text box
                  (pedia nil)         ; Civilopedia: (category-index . scroll) while open
                  (replay nil)        ; T while the replay/timeline graph is shown
                  (hof nil)           ; Hall of Fame records, read once the game ends
                  (recorded nil)      ; T once this game's result has been recorded
                  (spy-menu nil)      ; T while the diplomat action menu is open
                  (help nil)          ; T while the help overlay is shown
                  (console nil)       ; T while the `~` Lisp console is open
                  (con-input "")      ; the form being typed into the console
                  (con-output nil)    ; lines from the last console evaluation
                  (con-history nil)   ; previously entered forms, newest first
                  (con-hist-pos -1))  ; position when browsing history (-1 = fresh line)
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
                           ;; a tribal hut popped on this move? announce its outcome
                           (when (civm:gs-message state)
                             (sdl2:set-window-title
                              win (format nil "civ-lisp — ~A" (civm:gs-message state)))
                             (setf (civm:gs-message state) nil))
                           (let ((u (civm:unit-by-id state selected)))
                             (when (or (null u) (<= (civm:unit-moves-left u) 0))
                               (setf selected (next-human-unit state selected))))))
                       (terra (job)
                         ;; order the selected settler to terraform; it goes busy,
                         ;; so move the cursor on to the next active unit
                         (when selected
                           (try (list job :unit selected))
                           (setf selected (next-human-unit state selected))))
                       (disband! ()
                         ;; remove the selected unit (recovering shields if in a
                         ;; city); report the outcome and advance the cursor
                         (when selected
                           (try (list :disband-unit :unit selected))
                           (when (civm:gs-message state)
                             (sdl2:set-window-title
                              win (format nil "civ-lisp — ~A" (civm:gs-message state)))
                             (setf (civm:gs-message state) nil))
                           (setf selected (next-human-unit state selected))))
                       (nuke! ()
                         ;; detonate the selected nuclear missile on its own tile,
                         ;; playing the blast animation where it stood
                         (when selected
                           (let* ((u (civm:unit-by-id state selected))
                                  (cx (and u (civm:unit-x u))) (cy (and u (civm:unit-y u))))
                             (try (list :nuke :unit selected))
                             (when (and cx (null (civm:unit-by-id state selected)))  ; it went off
                               (let ((sx (mod (- cx cam-x)
                                              (civm:map-width (civm:gs-map state))))
                                     (sy (- cy cam-y)))
                                 (when (and (< sx *view-cols*) (<= 0 sy (1- *view-rows*)))
                                   (dotimes (f +nuke-frames+)
                                     (let ((f f))
                                       (render-game painter state selected :fog t
                                                    :cam-x cam-x :cam-y cam-y
                                                    :vw *view-cols* :vh *view-rows*
                                                    :hud-right hud-right
                                                    :overlay (lambda (p)
                                                               (draw-explosion-frame
                                                                p f (* sx *tile*) (* sy *tile*)))))
                                     (sdl2:delay 25)))))
                             (when (civm:gs-message state)
                               (sdl2:set-window-title
                                win (format nil "civ-lisp — ~A" (civm:gs-message state)))
                               (setf (civm:gs-message state) nil))
                             (setf selected (next-human-unit state selected)))))
                       (upgrade! ()
                         ;; upgrade the selected (obsolete) unit in its city for gold
                         (when selected
                           (handler-case
                               (progn
                                 (civm:apply-command state (list :upgrade-unit :unit selected))
                                 (sdl2:set-window-title
                                  win (format nil "civ-lisp — ~A" (civm:gs-message state)))
                                 (setf (civm:gs-message state) nil))
                             (civm:command-error (e)
                               (sdl2:set-window-title win (format nil "civ-lisp — ~A" e))))))
                       (lux! (delta)
                         ;; shift DELTA percent between science and luxury
                         (let* ((pid (first (human-player-ids state)))
                                (p (civm:player-by-id state pid))
                                (cap (civm:government-def
                                      (civm:player-government p) :max-rate 100))
                                (lux (max 0 (min cap (+ (civm:player-luxury-rate p) delta))))
                                (sci (- 100 (civm:player-tax-rate p) lux)))
                           (when (<= 0 sci cap)
                             (try (list :set-rates :player pid
                                        :tax (civm:player-tax-rate p)
                                        :luxury lux :science sci))
                             (retitle))))
                       (tax! (delta)
                         ;; shift DELTA percent between science and tax (gold);
                         ;; luxury stays put, the government caps each rate
                         (let* ((pid (first (human-player-ids state)))
                                (p (civm:player-by-id state pid))
                                (cap (civm:government-def
                                      (civm:player-government p) :max-rate 100))
                                (tax (max 0 (min cap (+ (civm:player-tax-rate p) delta))))
                                (sci (- 100 (civm:player-luxury-rate p) tax)))
                           (when (<= 0 sci cap)
                             (try (list :set-rates :player pid
                                        :tax tax :luxury (civm:player-luxury-rate p)
                                        :science sci))
                             (retitle))))
                       (espionage! (cmd ok-msg)
                         ;; a selected diplomat's steal/sabotage on an adjacent
                         ;; enemy city; report the result (incl. being caught)
                         (when selected
                           (let ((u selected))
                             (handler-case
                                 (progn (civm:apply-command state (list cmd :unit u))
                                        (sdl2:set-window-title
                                         win (format nil "civ-lisp — ~A" ok-msg))
                                        (setf selected (next-human-unit state selected)))
                               (civm:command-error (e)
                                 (sdl2:set-window-title win (format nil "civ-lisp — ~A" e))
                                 (unless (civm:unit-by-id state u)   ; caught/consumed
                                   (setf selected (next-human-unit state selected))))))))
                       (do-spy-action (cmd)
                         ;; run a spy-menu action; investigate reports the city
                         (if (eq cmd :investigate)
                             (let* ((u (and selected (civm:unit-by-id state selected)))
                                    (c (and u (civm:adjacent-enemy-city state u))))
                               (espionage! :investigate
                                           (if c (civm:city-report state c) "investigate")))
                             (espionage! cmd (case cmd
                                               (:steal-tech "advance stolen!")
                                               (:sabotage "sabotage!")
                                               (:establish-embassy "embassy established")
                                               (:incite-revolt "city incited!")
                                               (:bribe-unit "unit bribed!")
                                               (t "done")))))
                       (do-diplo (pick)
                         ;; run a diplomacy-menu action; PICK is (oid action)
                         (when pick
                           (let* ((me (first (human-player-ids state)))
                                  (oid (first pick)) (action (second pick))
                                  (name (civm:player-name (civm:player-by-id state oid))))
                             (ecase action
                               (:declare-war (try (list :declare-war :player me :against oid)))
                               (:make-peace  (try (list :make-peace  :player me :against oid)))
                               (:break-alliance
                                (try (list :break-alliance :player me :against oid)))
                               (:ceasefire
                                (sdl2:set-window-title
                                 win (if (try (list :propose-ceasefire :player me :against oid))
                                         (format nil "civ-lisp — cease-fire with ~A" name)
                                         (format nil "civ-lisp — ~A fights on" name))))
                               (:demand-tribute
                                (sdl2:set-window-title
                                 win (if (try (list :demand-tribute :player me :against oid))
                                         (format nil "civ-lisp — ~A pays you tribute" name)
                                         (format nil "civ-lisp — ~A refuses tribute" name))))
                               (:propose-alliance
                                (sdl2:set-window-title
                                 win (if (try (list :propose-alliance :player me :against oid))
                                         (format nil "civ-lisp — ~A accepts your alliance" name)
                                         (format nil "civ-lisp — ~A declines the alliance" name))))
                               (:gift
                                (sdl2:set-window-title
                                 win (if (try (list :propose-trade :player me :to oid
                                                    :give '((:gold 50)) :want '()))
                                         (format nil "civ-lisp — gave ~A 50 gold" name)
                                         "civ-lisp — not enough gold to gift")))))))
                       (prompt-research! ()
                         ;; pop the research chooser when the human has no target
                         (let ((p (human-player state)))
                           (when (and p (null (civm:player-researching p))
                                      (civm:researchable-techs p))
                             (setf research-menu t)))))
                (retitle)
                (prompt-research!)   ; ask which advance to pursue first
                (unwind-protect
                     ;; manual poll loop, reading event fields at raw SDL offsets
                     (loop while running do
                       (loop while (/= 0 (sdl2-ffi.functions:sdl-poll-event ev)) do
                         (let ((type (ev-type ev)))
                           (cond
                             ((= type +ev-quit+) (setf running nil))
                             ;; typed text goes into the console line, or the
                             ;; city-name entry while founding a city
                             ((= type +ev-textinput+)
                              (cond (skip-text (setf skip-text nil)) ; the opening keystroke
                                    (console
                                     (setf con-input
                                           (concatenate 'string con-input (ev-text ev))))
                                    (naming
                                     (setf name-input
                                           (concatenate 'string name-input (ev-text ev))))))
                             ((= type +ev-keydown+)
                              (let ((sc (ev-scancode ev))
                                    (ctrl (ev-ctrl-p ev))
                                    (shift (ev-shift-p ev)))
                                (cond
                                  ;; Lisp console open: capture editing keys
                                  (console
                                   (cond
                                     ((= sc +sc-escape+)
                                      (setf console nil)
                                      (sdl2-ffi.functions:sdl-stop-text-input))
                                     ((or (= sc +sc-return+) (= sc +sc-kp-enter+))
                                      (when (plusp (length (string-trim " " con-input)))
                                        (push con-input con-history))   ; record in history
                                      (setf con-output (console-eval con-input state)
                                            con-input "" con-hist-pos -1))
                                     ((= sc +sc-backspace+)
                                      (when (plusp (length con-input))
                                        (setf con-input
                                              (subseq con-input 0 (1- (length con-input))))))
                                     ;; history: Up / C-p = older, Down / C-n = newer
                                     ((or (= sc +sc-up+) (and ctrl (= sc +sc-p+)))
                                      (when con-history
                                        (setf con-hist-pos
                                              (min (1+ con-hist-pos) (1- (length con-history)))
                                              con-input (nth con-hist-pos con-history))))
                                     ((or (= sc +sc-down+) (and ctrl (= sc +sc-n+)))
                                      (if (> con-hist-pos 0)
                                          (setf con-hist-pos (1- con-hist-pos)
                                                con-input (nth con-hist-pos con-history))
                                          (setf con-hist-pos -1 con-input "")))))
                                  ;; naming a new city: type the name, Enter founds
                                  (naming
                                   (cond
                                     ((or (= sc +sc-return+) (= sc +sc-kp-enter+))
                                      (let ((nm (string-trim " " name-input)))
                                        (when (plusp (length nm))
                                          (try (list :found-city :unit naming :name nm))
                                          (setf selected (first-human-unit state))))
                                      (setf naming nil)
                                      (sdl2-ffi.functions:sdl-stop-text-input)
                                      (retitle))
                                     ((= sc +sc-escape+)
                                      (setf naming nil)
                                      (sdl2-ffi.functions:sdl-stop-text-input))
                                     ((= sc +sc-backspace+)
                                      (when (plusp (length name-input))
                                        (setf name-input
                                              (subseq name-input 0 (1- (length name-input))))))))
                                  ;; Civilopedia open: browse sections and scroll
                                  (pedia
                                   (let ((ncat (length *pedia-categories*)))
                                     (cond
                                       ((or (= sc +sc-escape+) (= sc +sc-o+)) (setf pedia nil))
                                       ((= sc +sc-left+)
                                        (setf pedia (cons (mod (1- (car pedia)) ncat) 0)))
                                       ((= sc +sc-right+)
                                        (setf pedia (cons (mod (1+ (car pedia)) ncat) 0)))
                                       ((= sc +sc-up+)
                                        (setf (cdr pedia) (max 0 (- (cdr pedia) 5))))
                                       ((= sc +sc-down+)
                                        (let ((n (length (pedia-lines
                                                          (aref *pedia-categories* (car pedia))))))
                                          (setf (cdr pedia) (min (max 0 (1- n)) (+ (cdr pedia) 5))))))))
                                  ;; replay overlay open: any key (Esc/Q) dismisses it
                                  (replay (setf replay nil))
                                  ;; `~` opens the console
                                  ((= sc +sc-grave+)
                                   (setf console t con-input "" skip-text t  ; eat the '`'
                                         con-output '("Lisp console -- *state* is the game"
                                                      "Enter evals, Up/C-p & Down/C-n history, Esc closes"))
                                   (sdl2-ffi.functions:sdl-start-text-input))
                                  ;; help overlay up: any key dismisses it
                                  (help (setf help nil))
                                  ;; government menu open: number picks a government
                                  (gov-menu
                                   (cond
                                     ((and (>= sc +sc-1+) (<= sc (+ +sc-1+ 8)))
                                      (let ((pick (nth (- sc +sc-1+) (gov-menu-lines state))))
                                        (when (and pick (fourth pick))
                                          (try (list :set-government
                                                     :player (first (human-player-ids state))
                                                     :to (second pick)))))
                                      (setf gov-menu nil) (retitle))
                                     ((= sc +sc-escape+) (setf gov-menu nil))))
                                  ;; diplomacy menu open: number toggles war/peace
                                  (diplo-menu
                                   (cond
                                     ((and (>= sc +sc-1+) (<= sc (+ +sc-1+ 8)))
                                      (let ((row (nth (- sc +sc-1+) (diplo-menu-lines state))))
                                        (when row     ; row = (i oid action label)
                                          (do-diplo (list (second row) (third row)))))
                                      (setf diplo-menu nil))
                                     ((= sc +sc-escape+) (setf diplo-menu nil))))
                                  (research-menu
                                   (cond
                                     ((and (>= sc +sc-1+) (<= sc (+ +sc-1+ 8)))
                                      (let ((row (nth (- sc +sc-1+) (research-menu-lines state))))
                                        (when row     ; row = (i tech label)
                                          (try (list :set-research
                                                     :player (first (human-player-ids state))
                                                     :tech (second row)))))
                                      (setf research-menu nil))
                                     ((= sc +sc-escape+)
                                      ;; dismiss = take the first listed advance, so
                                      ;; research never stalls with no target
                                      (let ((rows (research-menu-lines state)))
                                        (when rows
                                          (try (list :set-research
                                                     :player (first (human-player-ids state))
                                                     :tech (second (first rows))))))
                                      (setf research-menu nil))))
                                  ;; spy menu open: number runs a diplomat action
                                  (spy-menu
                                   (cond
                                     ((and (>= sc +sc-1+) (<= sc (+ +sc-1+ 8)))
                                      (let* ((u (and selected (civm:unit-by-id state selected)))
                                             (pick (and u (nth (- sc +sc-1+)
                                                               (spy-menu-lines state u)))))
                                        (when (and pick (fourth pick)) (do-spy-action (second pick))))
                                      (setf spy-menu nil))
                                     ((= sc +sc-escape+) (setf spy-menu nil))))
                                  ;; trade menu open: number executes that civ's offer
                                  (trade-menu
                                   (cond
                                     ((and (>= sc +sc-1+) (<= sc (+ +sc-1+ 8)))
                                      (let* ((me (first (human-player-ids state)))
                                             (pick (nth (- sc +sc-1+) (trade-menu-lines state)))
                                             (oid (and pick (second pick)))
                                             (deal (and pick (third pick))))
                                        (when (and oid deal)
                                          (let ((ok (try (list* :propose-trade
                                                                :player me :to oid deal))))
                                            (sdl2:set-window-title
                                             win (if ok "civ-lisp — trade agreed"
                                                     "civ-lisp — trade rejected")))))
                                      (setf trade-menu nil))
                                     ((= sc +sc-escape+) (setf trade-menu nil))))
                                  ;; build menu open: number picks a unit (and
                                  ;; closes), +/- hire/return a specialist (menu
                                  ;; stays up), Esc closes
                                  (build-city
                                   (cond
                                     ((= sc +sc-equals+)        ; '+': hire a specialist
                                      (try (list :set-specialists :city build-city :op :add)))
                                     ((= sc +sc-minus+)         ; '-': return one to a tile
                                      (try (list :set-specialists :city build-city :op :remove)))
                                     ((and (>= sc +sc-1+) (<= sc (+ +sc-1+ 8)))
                                      (let ((pick (nth (- sc +sc-1+)
                                                       (build-menu-lines state
                                                        (civm:city-by-id state build-city)))))
                                        (when pick
                                          (try (list :set-production :city build-city
                                                     :item (second pick)))))
                                      (setf build-city nil))
                                     ((= sc +sc-escape+) (setf build-city nil))))
                                  ;; an AI offer is pending: Y accepts, N/Esc declines
                                  ;; (modal -- other keys wait until you answer)
                                  ((civm:gs-offers state)
                                   (let* ((o (first (civm:gs-offers state)))
                                          (me (first (human-player-ids state))))
                                     (cond
                                       ((= sc +sc-y+)
                                        (try (list :resolve-offer :player me
                                                   :from (getf o :from) :kind (getf o :kind)
                                                   :accept t)))
                                       ((or (= sc +sc-n+) (= sc +sc-escape+))
                                        (try (list :resolve-offer :player me
                                                   :from (getf o :from) :kind (getf o :kind)
                                                   :accept nil))))))
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
                                   ;; game just ended? record it in the Hall of Fame
                                   (when (and (civm:gs-winner state) (not recorded))
                                     (setf hof (record-hall-of-fame state) recorded t))
                                   (ignore-errors (civm:save-game state *autosave-path*)) ; autosave
                                   (prompt-research!)   ; choose the next advance
                                   (retitle))
                                  ((= sc +sc-w+)
                                   ;; wait: send this unit to the end of the cycle
                                   (when selected
                                     (setf (gethash selected *waited*) (incf *wait-seq*))
                                     (setf selected (next-human-unit state selected))))
                                  ((= sc +sc-tab+)
                                   (setf selected (next-human-unit state selected)))
                                  ((= sc +sc-b+)
                                   ;; only prompt if the unit can actually found a
                                   ;; city here; otherwise say why
                                   (let ((u (and selected (civm:unit-by-id state selected))))
                                     (cond
                                       ((and u (civm:can-found-here-p state u))
                                        (setf naming selected
                                              name-input (civm:next-city-name
                                                          state (first (human-player-ids state)))
                                              skip-text t)   ; ignore this 'b' keystroke
                                        (sdl2-ffi.functions:sdl-start-text-input))
                                       (u (sdl2:set-window-title
                                           win "civ-lisp — can't found a city here")))))
                                  ((= sc +sc-f+)
                                   (when selected (try (list :fortify :unit selected))))
                                  ((= sc +sc-r+)
                                   ;; R builds a road, or upgrades an existing
                                   ;; road to a railroad once Railroad is known
                                   (let* ((u (and selected (civm:unit-by-id state selected)))
                                          (tl (and u (civm:tile-at (civm:gs-map state)
                                                                   (civm:unit-x u) (civm:unit-y u))))
                                          (p (civm:player-by-id
                                              state (first (human-player-ids state)))))
                                     (if (and tl (civm:tile-road tl)
                                              (not (civm:tile-railroad tl))
                                              (civm:player-has-tech-p p :rail-road))
                                         (terra :build-railroad)
                                         (terra :build-road))))
                                  ((= sc +sc-i+) (terra :irrigate))
                                  ((and ctrl (= sc +sc-m+))   ; Ctrl+M: flip HUD side
                                   (setf hud-right (not hud-right)))
                                  ((= sc +sc-m+) (terra :mine))
                                  ((= sc +sc-t+) (terra :build-fort))
                                  ((= sc +sc-a+) (terra :build-airbase))
                                  ((= sc +sc-c+) (terra :clear-forest))
                                  ((= sc +sc-p+) (terra :clean-pollution))
                                  ((= sc +sc-v+) (setf gov-menu t))    ; revolution menu
                                  ((= sc +sc-y+) (setf diplo-menu t))  ; diplomacy menu
                                  ((= sc +sc-e+) (setf trade-menu t))  ; trade menu
                                  ((= sc +sc-z+) (espionage! :steal-tech "advance stolen!"))
                                  ((= sc +sc-x+) (espionage! :sabotage "sabotage!"))
                                  ((= sc +sc-h+) (espionage! :help-wonder "wonder boosted!"))
                                  ((= sc +sc-j+) (espionage! :trade-route "trade route opened!"))
                                  ((and shift (= sc +sc-d+)) (disband!)) ; Shift+D: disband unit
                                  ((= sc +sc-u+) (upgrade!))             ; U: upgrade obsolete unit
                                  ((= sc +sc-n+) (nuke!))                ; N: detonate a nuke
                                  ((= sc +sc-d+)               ; diplomat action menu
                                   (let ((u (and selected (civm:unit-by-id state selected))))
                                     (when (and u (eq (civm:unit-type u) :diplomat))
                                       (setf spy-menu t))))
                                  ((= sc +sc-k+)                       ; toggle Slynk server
                                   (handler-case
                                       (if *slynk-port*
                                           (progn (stop-slynk)
                                                  (sdl2:set-window-title win "civ-lisp — Slynk stopped"))
                                           (progn (start-slynk)
                                                  (sdl2:set-window-title
                                                   win (format nil "civ-lisp — Slynk on port ~D"
                                                               *slynk-port*))))
                                     (error (e)
                                       (sdl2:set-window-title
                                        win (format nil "civ-lisp — Slynk error: ~A" e)))))
                                  ((= sc +sc-comma+) (lux! -10))       ; luxury down
                                  ((= sc +sc-period+) (lux! 10))       ; luxury up
                                  ((= sc +sc-lbracket+) (tax! -10))    ; tax down (more science)
                                  ((= sc +sc-rbracket+) (tax! 10))     ; tax up (more gold)
                                  ((= sc +sc-slash+) (setf help t))    ; ? : help
                                  ((= sc +sc-o+) (setf pedia (cons 0 0)))  ; Civilopedia
                                  ((= sc +sc-q+) (setf replay t))          ; replay/timeline graph
                                  ((= sc +sc-s+)
                                   (civm:save-game state *save-path*)
                                   (sdl2:set-window-title win "civ-lisp — game saved"))
                                  ((= sc +sc-l+)
                                   (when (probe-file *save-path*)
                                     (when goto-mode (torch!))
                                     (setf state (civm:load-game *save-path*)
                                           *state* state          ; republish for SLY
                                           build-city nil
                                           selected (first-human-unit state))
                                     (clrhash *waited*) (setf *wait-seq* 0)
                                     (retitle)))
                                  ((= sc +sc-up+) (step! 0 -1))
                                  ((= sc +sc-down+) (step! 0 1))
                                  ((= sc +sc-left+) (step! -1 0))
                                  ((= sc +sc-right+) (step! 1 0))
                                  ;; numpad: 8-way movement (diagonals included)
                                  ((= sc +sc-kp-8+) (step!  0 -1))
                                  ((= sc +sc-kp-2+) (step!  0  1))
                                  ((= sc +sc-kp-4+) (step! -1  0))
                                  ((= sc +sc-kp-6+) (step!  1  0))
                                  ((= sc +sc-kp-7+) (step! -1 -1))
                                  ((= sc +sc-kp-9+) (step!  1 -1))
                                  ((= sc +sc-kp-1+) (step! -1  1))
                                  ((= sc +sc-kp-3+) (step!  1  1)))))
                             ((= type +ev-mousebuttondown+)
                              (let ((tx (civm:wrap-x (civm:gs-map state)
                                          (+ cam-x (floor (ev-mouse-x ev) (* *tile* scale)))))
                                    (ty (+ cam-y (floor (ev-mouse-y ev) (* *tile* scale)))))
                                (cond
                                  ;; help overlay up: a click dismisses it
                                  (help (setf help nil))
                                  ;; an AI offer is pending: answer it with Y/N, not
                                  ;; the mouse -- swallow clicks until then
                                  ((civm:gs-offers state))
                                  ;; naming a city: finish it from the keyboard
                                  (naming)
                                  ;; Civilopedia is keyboard-navigated
                                  (pedia)
                                  ;; the replay overlay is dismissed from the keyboard
                                  (replay)
                                  ;; government menu open: click a row to pick it
                                  (gov-menu
                                   (let ((g (gov-menu-pick painter state
                                                           (floor (ev-mouse-y ev) scale))))
                                     (when g
                                       (try (list :set-government
                                                  :player (first (human-player-ids state))
                                                  :to g))))
                                   (setf gov-menu nil) (retitle))
                                  ;; diplomacy menu open: click a row to run that move
                                  (diplo-menu
                                   (do-diplo (diplo-menu-pick painter state
                                                              (floor (ev-mouse-y ev) scale)))
                                   (setf diplo-menu nil))
                                  ;; research chooser open: click an advance to research it
                                  (research-menu
                                   (let ((tech (research-menu-pick painter state
                                                                   (floor (ev-mouse-y ev) scale))))
                                     (when tech
                                       (try (list :set-research
                                                  :player (first (human-player-ids state))
                                                  :tech tech))))
                                   (setf research-menu nil))
                                  ;; spy menu open: click an action to run it
                                  (spy-menu
                                   (let* ((u (and selected (civm:unit-by-id state selected)))
                                          (cmd (and u (spy-menu-pick painter state u
                                                       (floor (ev-mouse-y ev) scale)))))
                                     (when cmd (do-spy-action cmd)))
                                   (setf spy-menu nil))
                                  ;; trade menu open: click a civ to execute its offer
                                  (trade-menu
                                   (let* ((me (first (human-player-ids state)))
                                          (pick (trade-menu-pick painter state
                                                                 (floor (ev-mouse-y ev) scale))))
                                     (when pick
                                       (let ((ok (try (list* :propose-trade :player me
                                                             :to (second pick) (third pick)))))
                                         (sdl2:set-window-title
                                          win (if ok "civ-lisp — trade agreed"
                                                  "civ-lisp — trade rejected")))))
                                   (setf trade-menu nil))
                                  ;; build menu open: a click on a line picks it,
                                  ;; anywhere else closes the menu
                                  (build-city
                                   (let* ((c (civm:city-by-id state build-city))
                                          (lx (floor (ev-mouse-x ev) scale))
                                          (ly (floor (ev-mouse-y ev) scale))
                                          (face (and c (specialist-pick painter state c lx ly)))
                                          (tilepick (and c (city-map-pick painter state c lx ly))))
                                     (cond
                                       ((integerp face)       ; a specialist icon: cycle its job
                                        (try (list :set-specialists :city build-city
                                                   :op :cycle :index face)))
                                       ((eq face :mood))       ; a citizen face: ignore, stay open
                                       ((eq tilepick :auto)    ; the city centre: governor resumes
                                        (try (list :work-tile :city build-city :auto t)))
                                       ((consp tilepick)       ; a work-radius tile: work / free it
                                        (try (list :work-tile :city build-city
                                                   :x (first tilepick) :y (second tilepick))))
                                       (t                     ; a line picks production; else close
                                        (let ((pick (build-menu-pick painter state c ly)))
                                          (when pick
                                            (try (list :set-production :city build-city
                                                       :item pick))))
                                        (setf build-city nil)))))
                                  ;; a click on the overview minimap recentres the
                                  ;; camera there (deselecting, so the auto-follow
                                  ;; below doesn't snap the view back to a unit)
                                  ((multiple-value-bind (wx wy)
                                       (minimap-hit painter state
                                                    (floor (ev-mouse-x ev) scale)
                                                    (floor (ev-mouse-y ev) scale)
                                                    hud-right (* *view-cols* *tile*))
                                     (when wx
                                       (setf selected nil
                                             cam-x (civm:wrap-x (civm:gs-map state)
                                                    (- wx (floor *view-cols* 2)))
                                             cam-y (clamp-cam-y state
                                                    (- wy (floor *view-rows* 2))))
                                       t)))
                                  ;; in goto mode: send the selected unit there
                                  ((and goto-mode selected)
                                   (try (list :goto :unit selected :x tx :y ty))
                                   (torch!) (retitle))
                                  ;; clicking a friendly city opens its build menu
                                  ((human-city-at state tx ty)
                                   (setf build-city (human-city-at state tx ty)))
                                  ;; otherwise: select the unit/garrison on that
                                  ;; tile (waking a fortified one), or -- on empty
                                  ;; ground -- recentre the view there (deselecting,
                                  ;; so the camera stays put instead of snapping back)
                                  (t (let ((u (human-unit-at state tx ty)))
                                       (if u
                                           (progn
                                             (setf selected u)
                                             (when (eq (civm:unit-orders
                                                        (civm:unit-by-id state u))
                                                       :fortified)
                                               (try (list :wake :unit u))))
                                           (setf selected nil
                                                 cam-x (civm:wrap-x (civm:gs-map state)
                                                        (- tx (floor *view-cols* 2)))
                                                 cam-y (clamp-cam-y state
                                                        (- ty (floor *view-rows* 2)))))))))))))
                       ;; keep the camera centred on the selected unit (it
                       ;; follows as you move/cycle; the map scrolls and wraps)
                       (let ((u (and selected (civm:unit-by-id state selected))))
                         (when u
                           (setf cam-x (civm:wrap-x (civm:gs-map state)
                                        (- (civm:unit-x u) (floor *view-cols* 2)))
                                 cam-y (clamp-cam-y state (- (civm:unit-y u)
                                                             (floor *view-rows* 2))))))
                       (render-game painter state selected
                                    :build-city build-city :gov-menu gov-menu
                                    :diplo-menu diplo-menu :trade-menu trade-menu
                                    :research-menu research-menu :hud-right hud-right
                                    :naming (and naming name-input) :pedia pedia
                                    :replay replay :hall-of-fame hof
                                    :spy-menu spy-menu :help help
                                    :console (and console (cons con-input con-output))
                                    :cam-x cam-x :cam-y cam-y
                                    :vw *view-cols* :vh *view-rows*)
                       (sdl2:delay 16))
                  ;; cleanup
                  (sdl2:destroy-texture (painter-sprites painter))
                  (sdl2:destroy-texture (painter-terrain painter))
                  (when (painter-nuke painter) (sdl2:destroy-texture (painter-nuke painter)))
                  (when (painter-icons painter) (sdl2:destroy-texture (painter-icons painter)))
                  (sdl2-ffi.functions:sdl-free-cursor torch-cursor)
                  (sdl2-ffi.functions:sdl-free-cursor go-cursor)
                  (sdl2-image:quit)))))))))))))

(defun main ()
  "Entry point.  On macOS the SDL event loop must run on the main thread."
  (sdl2:make-this-thread-main #'run))
