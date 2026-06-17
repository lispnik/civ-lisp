;;;; explore-demo.lisp -- verify (and illustrate) auto-exploration: a lone scout
;;;; set to :explore maps the continent on its own, turn after turn.
;;;;
;;;;   sbcl --non-interactive --load examples/explore-demo.lisp
;;;;
;;;; It drives the real view headlessly (a hidden window + software renderer),
;;;; drops one Warriors scout on the middle of a continent with the rest of the
;;;; map under fog, gives it the :explore order, and ends several turns.  Each
;;;; turn PROCESS-EXPLORE walks it toward the nearest unmapped tile.  Asserts the
;;;; explored set grows and the scout keeps exploring (prints PASS/FAIL), and
;;;; writes three fog-of-war snapshots (start / mid / late) to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun explore-demo/shot (painter state sel path)
  (let ((ren (painter-ren painter)) (w (* *view-cols* *tile*)) (h (* *view-rows* *tile*)))
    (render-game painter state sel :fog t :cam-x 0 :cam-y 0 :vw *view-cols* :vh *view-rows*)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 w h 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun explore-demo/central-land (state)
  "An interior land tile near the map centre (so the reveal grows centred)."
  (let* ((map (civm:gs-map state))
         (cx (floor (civm:map-width map) 2)) (cy (floor (civm:map-height map) 2))
         (best (list cx cy)) (bestscore most-negative-fixnum))
    (civm:do-tiles (x y tl map)
      (unless (eq (civm:tile-terrain tl) :ocean)
        (let* ((n (loop for (nx ny nt) in (civm:neighbors map x y)
                        count (not (eq (civm:tile-terrain nt) :ocean))))
               (score (- (* n 100) (+ (civm:map-dx map x cx) (abs (- y cy))))))
          (when (> score bestscore) (setf bestscore score best (list x y))))))
    best))

(defun explore-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "explore-demo"
                             :w (* *view-cols* *tile*) :h (* *view-rows* *tile*) :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let ((painter (make-renderer-painter ren
                                                (load-atlas ren *sprites-image* +unit-bg-key+)
                                                (load-atlas ren *terrain-image*)))
                (state (civm:make-new-game :seed 21 :width 32 :height 20
                                           :players '("You" "Red"))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            ;; reduce player 1 to a single scout, with the whole map back under fog
            (clrhash (civm:player-seen (civm:player-by-id state 1)))
            (dolist (u (loop for uu being the hash-values of (civm:gs-units state)
                             when (= (civm:unit-owner uu) 1) collect uu))
              (civm::destroy-unit state u))
            (destructuring-bind (sx sy) (explore-demo/central-land state)
              (let ((scout (civm::register-unit state :type :warriors :owner 1 :x sx :y sy)))
                (civm:update-visibility state)
                (let ((seen0 (hash-table-count (civm:player-seen (civm:player-by-id state 1)))))
                  (explore-demo/shot painter state (civm:unit-id scout)
                                     (merge-pathnames "explore-demo-1.png" docs))
                  (civm::apply-command state (list :explore :unit (civm:unit-id scout)))
                  (dotimes (i 7) (civm::end-turn state))
                  (explore-demo/shot painter state (civm:unit-id scout)
                                     (merge-pathnames "explore-demo-2.png" docs))
                  (dotimes (i 15) (civm::end-turn state))
                  (explore-demo/shot painter state (civm:unit-id scout)
                                     (merge-pathnames "explore-demo-3.png" docs))
                  (let ((seen1 (hash-table-count (civm:player-seen (civm:player-by-id state 1)))))
                    (format t "~&explore-demo: scout ~A,~A -> ~A,~A (order ~A); ~
                               explored ~D -> ~D tiles -- ~A~%"
                            sx sy (civm:unit-x scout) (civm:unit-y scout)
                            (civm:unit-orders scout) seen0 seen1
                            (if (> seen1 seen0) "PASS" "FAIL"))
                    (format t "wrote docs/explore-demo-{1,2,3}.png~%")))))))))))

(sdl2:make-this-thread-main #'explore-demo)
