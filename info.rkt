#lang setup/infotab
(define version "0.8")
(define collection 'multi)
(define deps '("base"
               "rackunit-lib"
               ["threading" "1.0.0"])) ;; FIXME: Version for commit efa04c
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))
