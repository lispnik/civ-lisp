;;;; build.lisp -- dump a standalone executable.
;;;;
;;;; Usage:
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load build.lisp
;;;; Produces a self-contained ./civ-lisp binary.  The native SDL2 /
;;;; SDL2_image shared libraries are reloaded at startup by CFFI, so they must
;;;; still be installed on the machine that runs the binary.

(asdf:load-system :civ-lisp)

(sb-ext:save-lisp-and-die
 "civ-lisp"
 :executable t
 :compression (and (member :sb-core-compression *features*) t)
 :save-runtime-options t
 :toplevel (lambda ()
             (handler-case
                 (progn (civ-lisp:main) 0)
               (error (e)
                 (format *error-output* "~&civ-lisp: ~A~%" e)
                 1))))