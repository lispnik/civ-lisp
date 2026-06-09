;;;; civ-model.asd
;;;;
;;;; The pure game model for a Civilization-like 4X game: state + rules only,
;;;; with no SDL and no I/O.  Kept as its own system so it can be loaded and
;;;; tested headless, and reused by tools / AI / tests independently of the
;;;; SDL front-end in civ-lisp.

(asdf:defsystem "civ-model"
  :description "Pure game model for a Civilization-like 4X game (no SDL)."
  :author "burnsidemk@gmail.com"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :pathname "src/model"
  :components ((:file "package")
               (:file "defs")
               (:file "map")
               (:file "entities")
               (:file "state")
               (:file "rules")
               (:file "commands")
               (:file "ai")))
