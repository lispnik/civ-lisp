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
               (:file "fog")
               (:file "rules")
               (:file "commands")
               (:file "ai")
               (:file "pathfind")
               (:file "persist"))
  :in-order-to ((asdf:test-op (asdf:test-op "civ-model/tests"))))

(asdf:defsystem "civ-model/tests"
  :description "FiveAM tests for the civ-model game model."
  :depends-on ("civ-model" "fiveam")
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "model-tests"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :civ-model :civ-model/tests))))
