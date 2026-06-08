;;;; civ-lisp.asd
;;;;
;;;; A minimal SDL2 application (SBCL, ocicl-managed deps) that opens a window
;;;; and sets the mouse cursor to the "torch" graphic extracted from the DOS
;;;; game Sid Meier's Civilization (see the sibling civ-extract project).

(asdf:defsystem "civ-lisp"
  :description "SDL2 window using the Civilization torch image as the mouse cursor."
  :author "mkennedy@swiftsensors.com"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("sdl2" "sdl2-image")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "main")))