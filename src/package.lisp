;;;; package.lisp

(defpackage #:civ-lisp
  (:use #:cl)
  (:export #:run #:main #:*scale* #:scale-surface
           #:start-slynk #:stop-slynk))