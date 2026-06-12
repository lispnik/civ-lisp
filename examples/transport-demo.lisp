;;;; transport-demo.lisp -- verify (and illustrate) naval transport: a land unit
;;;; boards a ship, which then carries it as it sails.
;;;;
;;;;   sbcl --non-interactive --load examples/transport-demo.lisp
;;;;
;;;; It drives the real view headlessly (a hidden window + software renderer),
;;;; carves a short sea lane beside a warrior, boards it onto a transport, sails
;;;; the transport, and asserts the warrior ends up on the sea tile (aboard) and
;;;; is carried along when the ship moves (prints PASS/FAIL).  Writes
;;;; before/boarded/sailed screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun transport-demo/shot (painter state path cam-x cam-y &optional sel)
  (let ((ren (painter-ren painter)))
    (render-game painter state sel :fog t :cam-x cam-x :cam-y cam-y :vw 20 :vh 15)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 320 240 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun transport-demo/sea-p (state x y)
  (eq (civm:tile-terrain (civm:tile-at (civm:gs-map state) x y)) :ocean))

(defun transport-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "transport-demo" :w 320 :h 240 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed 4 :width 24 :height 16
                                            :players '("You" "Red")))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*)))
                 (map (civm:gs-map state)))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            (let* ((w (loop for u being the hash-values of (civm:gs-units state)
                            when (and (= (civm:unit-owner u) 1)
                                      (eq (civm:unit-type u) :warriors)) return u))
                   (wx (civm:unit-x w)) (wy (civm:unit-y w))
                   (sea1 (civm:wrap-x map (1+ wx))) (sea2 (civm:wrap-x map (+ 2 wx)))
                   (cam-x (max 0 (- wx 10))) (cam-y (max 0 (- wy 7))))
              ;; carve a short sea lane east of the warrior and park a transport
              (setf (civm:tile-terrain (civm:tile-at map sea1 wy)) :ocean
                    (civm:tile-terrain (civm:tile-at map sea2 wy)) :ocean)
              (let ((tr (civm::register-unit state :type :transport :owner 1 :x sea1 :y wy)))
                (civm:update-visibility state)
                (format t "~&Warrior at (~D,~D); transport at (~D,~D)~%" wx wy sea1 wy)
                (transport-demo/shot painter state (merge-pathnames "transport-before.png" docs)
                                     cam-x cam-y (civm:unit-id w))
                ;; board: warrior steps east onto the transport's sea tile
                (civm:apply-command state (list :move-unit :unit (civm:unit-id w) :dx 1 :dy 0))
                (let ((aboard (and (= (civm:unit-x w) sea1) (= (civm:unit-y w) wy)
                                   (transport-demo/sea-p state (civm:unit-x w) (civm:unit-y w)))))
                  (format t "Boarded? warrior now at (~D,~D) on ~:[land~;sea~] => ~A~%"
                          (civm:unit-x w) (civm:unit-y w)
                          (transport-demo/sea-p state (civm:unit-x w) (civm:unit-y w))
                          (if aboard "PASS" "FAIL"))
                  (transport-demo/shot painter state
                                       (merge-pathnames "transport-boarded.png" docs)
                                       cam-x cam-y (civm:unit-id w))
                  ;; sail east; the cargo should ride along
                  (civm:apply-command state (list :move-unit :unit (civm:unit-id tr) :dx 1 :dy 0))
                  (let ((carried (and (= (civm:unit-x w) (civm:unit-x tr))
                                      (= (civm:unit-y w) (civm:unit-y tr)))))
                    (format t "Carried? transport at (~D,~D), warrior at (~D,~D) => ~A~%"
                            (civm:unit-x tr) (civm:unit-y tr)
                            (civm:unit-x w) (civm:unit-y w)
                            (if carried "PASS" "FAIL"))
                    (transport-demo/shot painter state
                                         (merge-pathnames "transport-sailed.png" docs)
                                         cam-x cam-y (civm:unit-id w))
                    (format t "=> ~A~%" (if (and aboard carried) "PASS" "FAIL"))))
                (format t "Wrote docs/transport-{before,boarded,sailed}.png~%")))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'transport-demo)
