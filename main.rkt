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
         bitsyntax
         syntax/parse/define
         (for-syntax racket/base))

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

(define (make-chip
         #:running         [running         #true]
         #:registers       [registers       (make-bytes 16)]
         #:index           [index           0]
         #:program-counter [program-counter #x200]
         #:stack           [stack           (stack (make-vector 16) 0)]
         #:memory          [memory          (make-bytes memory-size)]
         #:loaded-rom      [loaded-rom      #false])
  (chip running registers index program-counter stack memory loaded-rom))

(define (chip-flag chip)
  (bytes-ref (chip-registers chip) #xF))

(define (set-chip-flag! chip value)
  (bytes-set! (chip-registers chip) #xF value))

(struct stack
  (contents
   [fill-ptr #:mutable]))

(define (stack-push! a-stack value)
  (struct-define stack a-stack)
  (vector-set! contents fill-ptr value)
  (set! fill-ptr (add1 fill-ptr)))

(define (stack-pop! a-stack)
  (struct-define stack a-stack)
  (set! fill-ptr (sub1 fill-ptr))
  (vector-ref contents fill-ptr))

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

(define (dispatch-instruction a-chip instruction)
  (define (call name) (name chip instruction))
  (bit-string-case instruction
    ([(= #x00E0 :: bytes 2)]
     (call 'op-cls))
    ([(= #x00EE :: bytes 2)]
     (op-ret a-chip))
    ([(= #x1 :: bits 4) (addr :: bits 12)]
     (op-jp-imm a-chip addr))
    ([(= #x2 :: bits 4) (addr :: bits 12)]
     (op-call a-chip addr))
    ([(= #x3 :: bits 4) (_ :: binary)]
     (call 'op-se-reg-imm))
    ([(= #x4 :: bits 4) (_ :: binary)]
     (call 'op-sne-reg-imm))
    ([(= #x5 :: bits 4) (_ :: binary)]
     (case (bitwise-and #x000F instruction)
       [(#x0) (call 'op-se-reg-reg)]))
    ([(= #x6 :: bits 4) (_ :: binary)]
     (call 'op-ld-reg<imm))
    ([(= #x7 :: bits 4) (register :: bits 4) (value :: bytes 1)]
     (op-add-reg<imm a-chip register value))
    ([(= #x8 :: bits 4) (x :: bits 4) (y :: bits 4) (= #x4 :: bits 4)]
     (op-add-reg<reg a-chip x y))
    ([(= #x8 :: bits 4) (x :: bits 4) (y :: bits 4) (= #x5 :: bits 4)]
     (op-sub-reg<reg a-chip x y))
    ([(= #x8 :: bits 4) (x :: bits 4) (y :: bits 4) (= #x7 :: bits 4)]
     (op-subn-reg<reg a-chip x y))
    ([(= #x8 :: bits 4) (_ :: binary)]
     (case (bitwise-and #x000F instruction)
       [(#x0) (call 'op-ld-reg<reg)]
       [(#x1) (call 'op-or)]
       [(#x2) (call 'op-and)]
       [(#x3) (call 'op-xor)]
       [(#x6) (call 'op-shr)]
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
    ([(= #xF :: bits 4) (register :: bits 4) (= #x33 :: bytes 1)]
     (op-ld-bcd<vx a-chip register))
    ([(= #xF :: bits 4) (register :: bits 4) (= #x1E :: bytes 1)]
     (op-add-index<reg a-chip register))
    ([(= #xF :: bits 4) (_ :: binary)]
     (case (bitwise-and #x00FF instruction)
       [(#x07) (call 'op-ld-reg<dt)]
       [(#x0A) (call 'op-ld-reg<key)]
       [(#x15) (call 'op-ld-dt<reg)]
       [(#x18) (call 'op-ld-st<reg)]
       [(#x29) (call 'op-ld-font<vx)]
       [(#x55) (call 'op-ld-mem<regs)]
       [(#x65) (call 'op-ld-regs<mem)]))))


(define-syntax-parse-rule (define-instruction (name args ...) body ...)
  #:with chip-instance (datum->syntax this-syntax 'chip-instance)
  (define (name chip-instance args ...)
    (struct-define chip chip-instance)
    body ...))


(define-instruction (op-ret)
  (set! program-counter (stack-pop! stack)))

(module+ test
  (test-case "op-ret"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "call at #x394 and return"
      (define call-instr (bytes #x23 #x94))
      (define ret-instr (bytes #x00 #xEE))
      (define old-program-counter program-counter)
      (dispatch-instruction c call-instr)
      (dispatch-instruction c ret-instr)
      (check-eq? program-counter old-program-counter))))


(define-instruction (op-rand register mask)
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


(define-instruction (op-jp-imm addr)
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


(define-instruction (op-call addr)
  (stack-push! stack program-counter)
  (set! program-counter addr))

(module+ test
  (test-case "op-call"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "addr #x139, stack with 0 elements"
      (define instr (bytes #x21 #x39))
      (define old-program-counter program-counter)
      (dispatch-instruction c instr)
      (check-eq? old-program-counter (vector-ref (stack-contents stack) 0))
      (check-eq? program-counter #x139))

    (test-case "addr #xAAE, stack with 1 element"
      (define instr (bytes #x2A #xAE))
      (define old-program-counter program-counter)
      (dispatch-instruction c instr)
      (check-eq? old-program-counter (vector-ref (stack-contents stack) 1))
      (check-eq? program-counter #xAAE))))


(define (+_8 x y)
  (define result (+ x y))
  (values
   (bitwise-bit-field result 0 8) ; result
   (if (> result 255) 1 0)))      ; carry


(define-instruction (op-add-reg<imm register value)
  (define register-value (bytes-ref registers register))
  ;; for some weird reason the ADD immediate op doesn't set the flag
  (define-values (result _ignore) (+_8 register-value value))
  (bytes-set! registers register result))

(module+ test
  (test-case "op-add-reg<imm"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "reg #x8 and value #x14"
      (define instr (bytes #x78 #x14))
      (define x #x93)
      (define y #x14)
      (define previous-flag-value (chip-flag c))
      (bytes-set! registers #x8 x)
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref registers #x8) (+ x y))
      (check-eq? (chip-flag c) previous-flag-value))

    (test-case "reg #x3 and overflow"
      (define instr (bytes #x73 #xFF))
      (define x #xF3)
      (define y #xFF)
      (define previous-flag-value (chip-flag c))
      (bytes-set! registers #x3 x)
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref registers #x3)
                 ((+ x y) . - . #x100)) ; substract 256 to account for overflow
      (check-eq? (chip-flag c) previous-flag-value))))


(define-instruction (op-add-reg<reg x y)
  (define first (bytes-ref registers x))
  (define second (bytes-ref registers y))
  (define-values (result carry) (+_8 first second))
  (bytes-set! registers x result)
  (set-chip-flag! chip-instance carry))

(module+ test
  (test-case "op-add-reg<reg"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "reg #x2 and #xB"
      (define instr (bytes #x82 #xB4))
      (define x #x5)
      (define y #x9)
      (bytes-set! registers #x2 x)
      (bytes-set! registers #xB y)
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref registers #x2) (+ x y))
      (check-eq? (chip-flag c) #x0))

    (test-case "reg #x1 and #x4, overflow"
      (define instr (bytes #x81 #x44))
      (define x #xFA)
      (define y #xBA)
      (bytes-set! registers #x1 x)
      (bytes-set! registers #x4 y)
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref registers #x1)
                 ((+ x y) . - . #x100)) ; substract 256 to account for overflow
      (check-eq? (chip-flag c) #x1))))


(define (-_8 x y)
  (define result (- x y))
  (values
   (bitwise-bit-field result 0 8) ; result
   (if (> x y) 1 0)))             ; NOT borrow


(define-instruction (op-sub-reg<reg x y)
  (define first (bytes-ref registers x))
  (define second (bytes-ref registers y))
  (define-values (result not-borrow) (-_8 first second))
  (bytes-set! registers x result)
  (set-chip-flag! chip-instance not-borrow))

(module+ test
  (test-case "op-sub-reg<reg"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "reg #x8 and #xA"
      (define instr (bytes #x88 #xA5))
      (define x #x15)
      (define y #x7)
      (bytes-set! registers #x8 x)
      (bytes-set! registers #xA y)
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref registers #x8) (- x y))
      (check-eq? (chip-flag c) #x1))

    (test-case "reg #xB and #x1, underflow"
      (define instr (bytes #x8B #x15))
      (define x #x3)
      (define y #xBA)
      (bytes-set! registers #xB x)
      (bytes-set! registers #x1 y)
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref registers #xB)
                 ((- x y) . + . #x100)) ; add 256 to account for underflow
      (check-eq? (chip-flag c) #x0))))


(define-instruction (op-subn-reg<reg x y)
  (define first (bytes-ref registers x))
  (define second (bytes-ref registers y))
  (define-values (result not-borrow) (-_8 second first))
  (bytes-set! registers x result)
  (set-chip-flag! chip-instance not-borrow))

(module+ test
  (test-case "op-subn-reg<reg"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "reg #x8 and #xA, underflow"
      (define instr (bytes #x88 #xA7))
      (define x #x15)
      (define y #x7)
      (bytes-set! registers #x8 x)
      (bytes-set! registers #xA y)
      (dispatch-instruction c instr)
      (check-eq?
       (bytes-ref registers #x8)
       ((- y x) . + . #x100)) ; add 256 to account for underflow
      (check-eq? (chip-flag c) #x0))

    (test-case "reg #xB and #x1"
      (define instr (bytes #x8B #x17))
      (define x #x3)
      (define y #xBA)
      (bytes-set! registers #xB x)
      (bytes-set! registers #x1 y)
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref registers #xB) (- y x))
      (check-eq? (chip-flag c) #x1))))


(define-instruction (op-jp-imm+reg addr)
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
      (bytes-set! registers #x0 #x8)
      (dispatch-instruction c instr)
      (check-true (= program-counter (+ #x10F #x8))))))


(define-instruction (op-ld-bcd<vx register)

  (define (integer->digits integer)
    (define third  (modulo (floor (/ integer 1))   10))
    (define second (modulo (floor (/ integer 10))  10))
    (define first  (modulo (floor (/ integer 100)) 10))
    (values first second third))

  (define number (bytes-ref registers register))
  (define-values (first second third) (integer->digits number))
  (bytes-set! memory (+ index 0) first)
  (bytes-set! memory (+ index 1) second)
  (bytes-set! memory (+ index 2) third))

(module+ test
  (test-case "op-ld-bcd<vx"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "123 in register #x3"
      (define instr (bytes #xF3 #x33))
      (set! index (+ program-counter 39))
      (bytes-set! registers #x3 123)
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref memory (+ index 0)) 1)
      (check-eq? (bytes-ref memory (+ index 1)) 2)
      (check-eq? (bytes-ref memory (+ index 2)) 3))

    (test-case "#x8 in register 1"
      (define instr (bytes #xF1 #x33))
      (set! index (+ program-counter 2))
      (bytes-set! registers #x1 #x8)
      (dispatch-instruction c instr)
      (check-eq? (bytes-ref memory (+ index 0)) 0)
      (check-eq? (bytes-ref memory (+ index 1)) 0)
      (check-eq? (bytes-ref memory (+ index 2)) 8))))


(define-instruction (op-add-index<reg register)
  (define value (+ index (bytes-ref registers register)))
  (set! index (bitwise-bit-field value 0 16)))

(module+ test
  (test-case "op-add-index<reg"
    (define c (make-chip))
    (struct-define chip c)

    (test-case "#x42 in register #x7, index was #x10"
      (define instr (bytes #xF7 #x1E))
      (set! index #x10)
      (bytes-set! registers #x7 #x42)
      (dispatch-instruction c instr)
      (check-eq? index (+ #x42 #x10)))

    (test-case "#x3 in register #x4, index was #xffff"
      (define instr (bytes #xF4 #x1E))
      (set! index #xffff)
      (bytes-set! registers #x4 #x3)
      (dispatch-instruction c instr)
      (check-eq? index #x2))))          ; should overflow

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

