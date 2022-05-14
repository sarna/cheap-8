#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29


  (require racket/port
           struct-define)

  ; cpu

  (define memory-size (* 1024 4))

  (struct chip
    ([running #:mutable]
     registers
     [index #:mutable]
     [program-counter #:mutable]
     stack
     memory
     [loaded-rom #:mutable]))

  (define (make-chip)
    (chip #true
          (make-bytes 16)
          0
          #x200
          (vector (make-bytes 16) 0)
          (make-bytes memory-size)
          #false))

  (define (chip-flag chip)
    (bytes-ref (chip-registers chip) #xf))

  (define (set-chip-flag! chip value)
    (bytes-set! (chip-registers chip) #xf value))

  (define (reset a-chip)
    (struct-define chip a-chip)
    (bytes-fill! memory 0)
    (bytes-fill! registers 0)
    (bytes-copy! memory #x200 (call-with-input-file loaded-rom
                                (λ (in) (port->bytes in))))
    (set! running #true)
    (set! program-counter #x200)
    (bytes-fill! (vector-ref stack 0) 0)
    (vector-set! stack 0 0))

  (define (load-rom chip filename)
    (set-chip-loaded-rom! chip filename)
    (reset chip))

  (define cycles-per-second 500)
  (define cycles-before-sleep 10)

  (define (run-cpu chip)
    (let loop ([tick 0])
      (when (chip-running chip)
        (emulate-cycle chip)
        (when (= tick cycles-before-sleep)
          (sleep (/ cycles-before-sleep cycles-per-second)))
        (loop (add1 (remainder tick cycles-before-sleep))))))

  (define (emulate-cycle a-chip)
    (values)) ; TODO

  ; main loop

  (define (run rom-filename)
    (define chip (make-chip))
    (load-rom chip rom-filename)
    (run-cpu chip))

  ; fiddling

  (define c (make-chip))
  (run-cpu c)
  )
