;;;; run.lisp -- launch the app.  SDL2's FFI bindings need a larger heap to
;;;; compile, so invoke with:
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load run.lisp
(asdf:load-system :civ-lisp)
(civ-lisp:main)