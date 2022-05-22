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

(require racket/port
         struct-define
         bitsyntax)

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
  (bytes-ref (chip-registers chip) #xF))

(define (set-chip-flag! chip value)
  (bytes-set! (chip-registers chip) #xF value))

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
  (struct-define chip a-chip)
  (define instruction (subbytes memory program-counter (add1 program-counter)))
  (set! program-counter (bitwise-bit-field (+ 2 program-counter) 0 12))
  (dispatch-instruction a-chip instruction))

;; (define (dispatch-instruction a-chip instruction)
;;   (define (call name) (name chip instruction))
;;   (case (bitwise-and #xF000 instruction)
;;     [(#x0000)
;;      (case instruction
;;        [(#x00E0) (call op-cls)]
;;        [(#x00EE) (call op-ret)])]
;;     [(#x1000) (call op-jp-imm)]
;;     [(#x2000) (call op-call)]
;;     [(#x3000) (call op-se-reg-imm)]
;;     [(#x4000) (call op-sne-reg-imm)]
;;     [(#x5000)
;;      (case (bitwise-and #x000F instruction)
;;        [(#x0) (call op-se-reg-reg)])]
;;     [(#x6000) (call op-ld-reg<imm)]
;;     [(#x7000) (call op-add-reg<imm)]
;;     [(#x8000)
;;      (case (bitwise-and #x000F instruction)
;;        [(#x0) (call op-ld-reg<reg)]
;;        [(#x1) (call op-or)]
;;        [(#x2) (call op-and)]
;;        [(#x3) (call op-xor)]
;;        [(#x4) (call op-add-reg<reg)]
;;        [(#x5) (call op-sub-reg<reg)]
;;        [(#x6) (call op-shr)]
;;        [(#x7) (call op-subn-reg<reg)]
;;        [(#xE) (call op-shl)])]
;;     [(#x9000)
;;      (case (bitwise-and #x000F instruction)
;;        [(#x0) (call op-sne-reg-reg)])]
;;     [(#xA000) (call op-ld-i<imm)]
;;     [(#xB000) (call op-jp-imm+reg)]
;;     [(#xC000) (call op-rand)]
;;     [(#xD000) (call op-draw)]
;;     [(#xE000)
;;      (case (bitwise-and #x00FF instruction)
;;        [(#x9E) (call op-skp)]
;;        [(#xA1) (call op-sknp)])]
;;     [(#xF000)
;;      (case (bitwise-and #x00FF instruction)
;;        [(#x07) (call op-ld-reg<dt)]
;;        [(#x0A) (call op-ld-reg<key)]
;;        [(#x15) (call op-ld-dt<reg)]
;;        [(#x18) (call op-ld-st<reg)]
;;        [(#x1E) (call op-add-index<reg)]
;;        [(#x29) (call op-ld-font<vx)]
;;        [(#x33) (call op-ld-bcd<vx)]
;;        [(#x55) (call op-ld-mem<regs)]
;;        [(#x65) (call op-ld-regs<mem)])]))

(define (dispatch-instruction a-chip instruction)
  (define (call name) (name chip instruction))
  (bit-string-case instruction
    ([(= #x0 :: bits 4) (_ :: binary)]
     (case instruction
       [(#x00E0) (call 'op-cls)]
       [(#x00EE) (call 'op-ret)]))
    ([(= #x1 :: bits 4) (addr :: bits 12)]
     (op-jp-imm a-chip addr))
    ([(= #x2 :: bits 4) (_ :: binary)]
     (call 'op-call))
    ([(= #x3 :: bits 4) (_ :: binary)]
     (call 'op-se-reg-imm))
    ([(= #x4 :: bits 4) (_ :: binary)]
     (call 'op-sne-reg-imm))
    ([(= #x5 :: bits 4) (_ :: binary)]
     (case (bitwise-and #x000F instruction)
       [(#x0) (call 'op-se-reg-reg)]))
    ([(= #x6 :: bits 4) (_ :: binary)]
     (call 'op-ld-reg<imm))
    ([(= #x7 :: bits 4) (_ :: binary)]
     (call 'op-add-reg<imm))
    ([(= #x8 :: bits 4) (_ :: binary)]
     (case (bitwise-and #x000F instruction)
       [(#x0) (call 'op-ld-reg<reg)]
       [(#x1) (call 'op-or)]
       [(#x2) (call 'op-and)]
       [(#x3) (call 'op-xor)]
       [(#x4) (call 'op-add-reg<reg)]
       [(#x5) (call 'op-sub-reg<reg)]
       [(#x6) (call 'op-shr)]
       [(#x7) (call 'op-subn-reg<reg)]
       [(#xE) (call 'op-shl)]))
    ([(= #x9 :: bits 4) (_ :: binary)]
     (case (bitwise-and #x000F instruction)
       [(#x0) (call 'op-sne-reg-reg)]))
    ([(= #xA :: bits 4) (_ :: binary)]
     (call 'op-ld-i<imm))
    ([(= #xB :: bits 4) (addr :: bits 12)]
     (op-jp-imm+reg a-chip addr))
    ([(= #xC :: bits 4) (register :: bits 4) (mask :: bytes 1)]
     (op-rand a-chip register mask))
    ([(= #xD :: bits 4) (_ :: binary)]
     (call 'op-draw))
    ([(= #xE :: bits 4) (_ :: binary)]
     (case (bitwise-and #x00FF instruction)
       [(#x9E) (call 'op-skp)]
       [(#xA1) (call 'op-sknp)]))
    ([(= #xF :: bits 4) (_ :: binary)]
     (case (bitwise-and #x00FF instruction)
       [(#x07) (call 'op-ld-reg<dt)]
       [(#x0A) (call 'op-ld-reg<key)]
       [(#x15) (call 'op-ld-dt<reg)]
       [(#x18) (call 'op-ld-st<reg)]
       [(#x1E) (call 'op-add-index<reg)]
       [(#x29) (call 'op-ld-font<vx)]
       [(#x33) (call 'op-ld-bcd<vx)]
       [(#x55) (call 'op-ld-mem<regs)]
       [(#x65) (call 'op-ld-regs<mem)]))))

(define (op-rand a-chip register mask)
  (struct-define chip a-chip)
  (bytes-set! registers register (bitwise-and (random 256) mask)))

(module+ test
  (test-case "op-rand"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "register #x2, mask #xFF"
      (define instr (bytes #xC2 #xFF))
      (dispatch-instruction c instr)
      (check-true ((bytes-ref registers #x2) . <= . #xFF)))

    (test-case "register #x7, mask #xB2"
      (define instr (bytes #xC7 #xB2))
      (dispatch-instruction c instr)
      (check-true ((bytes-ref registers #x7) . <= . #xB2)))

    (test-case "register #x5, mask #x00"
      (define instr (bytes #xC5 #x00))
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref registers #x5) #x0))))

(define (op-jp-imm a-chip addr)
  (struct-define chip a-chip)
  (set! program-counter addr))

(module+ test
  (test-case "op-jp-imm"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "addr #x123"
      (define instr (bytes #x11 #x23))
      (dispatch-instruction c instr)
      (check-true (= program-counter #x123)))

    (test-case "addr #x94E"
      (define instr (bytes #x19 #x4E))
      (dispatch-instruction c instr)
      (check-true (= program-counter #x94E)))))

(define (op-jp-imm+reg a-chip addr)
  (struct-define chip a-chip)
  (set! program-counter (+ addr (bytes-ref registers 0))))

(module+ test
  (test-case "op-jp-imm+reg"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "addr #x73B, empty register 0"
      (define instr (bytes #xB7 #x3B))
      (dispatch-instruction c instr)
      (check-true (= program-counter #x73B)))

    (test-case "addr #x10F, #x8 in register 0"
      (define instr (bytes #xB1 #x0F))
      (bytes-set! registers 0 #x8)
      (dispatch-instruction c instr)
      (check-true (= program-counter (+ #x10F #x8))))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  ; main loop

  (define (run rom-filename)
    (define chip (make-chip))
    (load-rom chip rom-filename)
    (run-cpu chip))

  ; fiddling

  (define c (make-chip))
  (dispatch-instruction c #"\302\0")
  ;(run-cpu c)
  ;(dispatch-instruction c #x00EE)
  )

