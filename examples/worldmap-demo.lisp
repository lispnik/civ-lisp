;;;; worldmap-demo.lisp -- verify (and illustrate) continental map generation:
;;;; landmasses grown from open ocean with a latitude climate (ice at the poles,
;;;; jungle/swamp at the equator), across all eleven terrain types.
;;;;
;;;;   sbcl --non-interactive --load examples/worldmap-demo.lisp
;;;;
;;;; It generates a map, asserts there is both land and sea, that ice/tundra
;;;; stays off the equatorial band, and that every civ starts on land (prints
;;;; PASS/FAIL), then renders the whole world to docs/worldmap.png.

(asdf:load-system :civ-lisp)
(in-package :civ-lisp)

(defun worldmap-demo ()
  (let ((docs (merge-pathnames "docs/" (asdf:system-source-directory :civ-lisp)))
        (w 40) (h 26))
    (sdl2:with-init (:video)
      (sdl2-image:init '(:png))
      (sdl2:with-window (win :title "worldmap-demo" :w (* w 16) :h (* h 16) :flags '(:hidden))
        (sdl2:with-renderer (ren win :flags '(:software))
          (sdl2-ffi.functions:sdl-set-render-draw-blend-mode ren 1)
          (let* ((state (civm:make-new-game :seed 7 :width w :height h
                                            :players '("You" "Rome" "Egypt")))
                 (map (civm:gs-map state))
                 (painter (make-renderer-painter ren
                            (load-atlas ren *sprites-image* +unit-bg-key+)
                            (load-atlas ren *terrain-image*))))
            ;; --- model checks ---------------------------------------------
            (let ((land 0) (sea 0) (ice-on-equator 0)
                  (mid (/ (1- h) 2.0))
                  (tiles (civm:map-tiles map)))
              (dotimes (k (length tiles))
                (let ((tile (svref tiles k)) (y (floor k w)))
                  (if (eq (civm:tile-terrain tile) :ocean) (incf sea) (incf land))
                  (when (and (member (civm:tile-terrain tile) '(:arctic :tundra))
                             (< (abs (- y mid)) (* 0.45 mid)))
                    (incf ice-on-equator))))
              (let* ((frac (/ land (+ land sea) 1.0))
                     (starts-on-land
                       (loop for u being the hash-values of (civm:gs-units state)
                             always (not (eq (civm:tile-terrain
                                              (civm:tile-at map (civm:unit-x u) (civm:unit-y u)))
                                             :ocean)))))
                (format t "~&Land/sea: ~D land, ~D sea (~,0F% land)~%" land sea (* 100 frac))
                (format t "Ice/tundra on the equatorial band: ~D~%" ice-on-equator)
                (format t "Every civ starts on land: ~A~%" starts-on-land)
                (format t "=> ~A~%"
                        (if (and (plusp land) (plusp sea) (< 0.1 frac 0.6)
                                 (zerop ice-on-equator) starts-on-land)
                            "PASS" "FAIL"))))
            ;; --- screenshot of the whole world ----------------------------
            (render-game painter state nil :fog nil :cam-x 0 :cam-y 0 :vw w :vh h)
            (let* ((fmt sdl2-ffi:+sdl-pixelformat-abgr8888+)
                   (surf (sdl2-ffi.functions:sdl-create-rgb-surface-with-format
                          0 (* w 16) (* h 16) 32 fmt)))
              (sdl2-ffi.functions:sdl-render-read-pixels ren (cffi:null-pointer) fmt
                (plus-c:c-ref surf sdl2-ffi:sdl-surface :pixels)
                (plus-c:c-ref surf sdl2-ffi:sdl-surface :pitch))
              (sdl2-image:save-png surf (namestring (merge-pathnames "worldmap.png" docs)))
              (sdl2-ffi.functions:sdl-free-surface surf))
            (format t "Wrote docs/worldmap.png~%")))))))

;; SDL video must run on the main thread (required on macOS).
(sdl2:make-this-thread-main #'worldmap-demo)
