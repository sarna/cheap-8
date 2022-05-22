#lang info
(define collection "cheap-8")
(define deps '("base"
               "struct-define"
               "bitsyntax"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/cheap-8.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(sarna))
(define license '(Apache-2.0 OR MIT))
