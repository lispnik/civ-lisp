;;;; upgrade-demo.lisp -- verify (and illustrate) unit obsolescence + in-city
;;;; upgrades: a Warriors garrison is retired by Gunpowder and upgraded to
;;;; Musketeers for gold.
;;;;
;;;;   sbcl --non-interactive --load examples/upgrade-demo.lisp
;;;;
;;;; It drives the real view headlessly (a hidden window + software renderer),
;;;; researches the superseding advance, upgrades the unit via apply-command,
;;;; and asserts its type changed and the gold cost was charged (prints
;;;; PASS/FAIL).  Writes before/after screenshots to docs/.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun upgrade-demo/shot (painter state path cam-x cam-y &optional sel)
  (let ((ren (painter-ren painter)))
    (render-game painter state sel :fog t :cam-x cam-x :cam-y cam-y :vw 20 :vh 15)
    (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
           (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format 0 320 240 32 fmt)))
      (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
        (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
      (sdl2-image:save-png surf (namestring path))
      (sdl2-ffi.functions:sdl-free-surface surf))))

(defun upgrade-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp))))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "upgrade-demo" :w 320 :h 240 :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed 4 :width 16 :height 10
                                            :players '("You" "Red")))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*))))
            (setf (painter-font painter) (load-gfont (namestring *font-file*) 1))
            (let* ((p (civm:player-by-id state 1))
                   (cx 6) (cy 5))
              (setf (civm:player-gold p) 200)
              (civm::register-city state :name "Rome" :owner 1 :x cx :y cy)
              (let ((w (civm::register-unit state :type :warriors :owner 1 :x cx :y cy)))
                (civm:update-visibility state)
                (let ((cam-x (max 0 (- cx 10))) (cam-y (max 0 (- cy 7))))
                  (format t "~&Garrison: ~(~A~) (ATK ~D DEF ~D); gold ~D~%"
                          (civm:unit-type w) (civm:unit-def (civm:unit-type w) :attack)
                          (civm:unit-def (civm:unit-type w) :defense) (civm:player-gold p))
                  (upgrade-demo/shot painter state (merge-pathnames "upgrade-before.png" docs)
                                     cam-x cam-y (civm:unit-id w))
                  ;; research the superseding advance -> warriors become obsolete
                  (setf (gethash :gunpowder (civm:player-techs p)) t)
                  (let ((cost (civm:upgrade-cost :warriors :musketeers))
                        (g0 (civm:player-gold p)))
                    (civm:apply-command state (list :upgrade-unit :unit (civm:unit-id w)))
                    (format t "~A~%" (civm:gs-message state))
                    (format t "Garrison now: ~(~A~) (ATK ~D DEF ~D); gold ~D~%"
                            (civm:unit-type w) (civm:unit-def (civm:unit-type w) :attack)
                            (civm:unit-def (civm:unit-type w) :defense) (civm:player-gold p))
                    (upgrade-demo/shot painter state (merge-pathnames "upgrade-after.png" docs)
                                       cam-x cam-y (civm:unit-id w))
                    (format t "=> ~A~%"
                            (if (and (eq (civm:unit-type w) :musketeers)
                                     (= (civm:player-gold p) (- g0 cost)))
                                "PASS" "FAIL"))
                    (format t "Wrote docs/upgrade-{before,after}.png~%")))))))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'upgrade-demo)
