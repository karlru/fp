#!/usr/local/bin/chez --script

; @generated
(import (chezscheme))
(case (machine-type)
  [(i3fb ti3fb a6fb ta6fb) #f]
  [(i3le ti3le a6le ta6le) (load-shared-object "libc.so.6")]
  [(i3osx ti3osx a6osx ta6osx) (load-shared-object "libc.dylib")]
  [(i3nt ti3nt a6nt ta6nt) (load-shared-object "msvcrt.dll")                           (load-shared-object "ws2_32.dll")]
  [else (load-shared-object "libc.so")])



(let ()
(define (blodwen-os)
  (case (machine-type)
    [(i3le ti3le a6le ta6le) "unix"]  ; GNU/Linux
    [(i3ob ti3ob a6ob ta6ob) "unix"]  ; OpenBSD
    [(i3fb ti3fb a6fb ta6fb) "unix"]  ; FreeBSD
    [(i3nb ti3nb a6nb ta6nb) "unix"]  ; NetBSD
    [(i3osx ti3osx a6osx ta6osx) "darwin"]
    [(i3nt ti3nt a6nt ta6nt) "windows"]
    [else "unknown"]))

(define blodwen-toSignedInt
  (lambda (x bits)
    (if (logbit? bits x)
        (logor x (ash (- 1) bits))
        (logand x (- (ash 1 bits) 1)))))

(define blodwen-toUnsignedInt
  (lambda (x bits)
    (modulo x (ash 1 bits))))

(define bu+ (lambda (x y bits) (blodwen-toUnsignedInt (+ x y) bits)))
(define bu- (lambda (x y bits) (blodwen-toUnsignedInt (- x y) bits)))
(define bu* (lambda (x y bits) (blodwen-toUnsignedInt (* x y) bits)))
(define bu/ (lambda (x y bits) (blodwen-toUnsignedInt (quotient x y) bits)))

(define bs+ (lambda (x y bits) (blodwen-toSignedInt (+ x y) bits)))
(define bs- (lambda (x y bits) (blodwen-toSignedInt (- x y) bits)))
(define bs* (lambda (x y bits) (blodwen-toSignedInt (* x y) bits)))
(define bs/ (lambda (x y bits) (blodwen-toSignedInt (quotient x y) bits)))

(define b+ (lambda (x y bits) (remainder (+ x y) (ash 1 bits))))
(define b- (lambda (x y bits) (remainder (- x y) (ash 1 bits))))
(define b* (lambda (x y bits) (remainder (* x y) (ash 1 bits))))
(define b/ (lambda (x y bits) (remainder (exact-floor (/ x y)) (ash 1 bits))))

(define integer->bits8 (lambda (x) (modulo x (expt 2 8))))
(define integer->bits16 (lambda (x) (modulo x (expt 2 16))))
(define integer->bits32 (lambda (x) (modulo x (expt 2 32))))
(define integer->bits64 (lambda (x) (modulo x (expt 2 64))))

(define bits16->bits8 (lambda (x) (modulo x (expt 2 8))))
(define bits32->bits8 (lambda (x) (modulo x (expt 2 8))))
(define bits32->bits16 (lambda (x) (modulo x (expt 2 16))))
(define bits64->bits8 (lambda (x) (modulo x (expt 2 8))))
(define bits64->bits16 (lambda (x) (modulo x (expt 2 16))))
(define bits64->bits32 (lambda (x) (modulo x (expt 2 32))))

(define blodwen-bits-shl-signed (lambda (x y bits) (blodwen-toSignedInt (ash x y) bits)))

(define blodwen-bits-shl (lambda (x y bits) (remainder (ash x y) (ash 1 bits))))

(define blodwen-shl (lambda (x y) (ash x y)))
(define blodwen-shr (lambda (x y) (ash x (- y))))
(define blodwen-and (lambda (x y) (logand x y)))
(define blodwen-or (lambda (x y) (logor x y)))
(define blodwen-xor (lambda (x y) (logxor x y)))

(define cast-num
  (lambda (x)
    (if (number? x) x 0)))
(define destroy-prefix
  (lambda (x)
    (cond
      ((equal? x "") "")
      ((equal? (string-ref x 0) #\#) "")
      (else x))))

(define exact-floor
  (lambda (x)
    (inexact->exact (floor x))))

(define exact-truncate
  (lambda (x)
    (inexact->exact (truncate x))))

(define exact-truncate-boundedInt
  (lambda (x y)
    (blodwen-toSignedInt (exact-truncate x) y)))

(define exact-truncate-boundedUInt
  (lambda (x y)
    (blodwen-toUnsignedInt (exact-truncate x) y)))

(define cast-char-boundedInt
  (lambda (x y)
    (blodwen-toSignedInt (char->integer x) y)))

(define cast-char-boundedUInt
  (lambda (x y)
    (blodwen-toUnsignedInt (char->integer x) y)))

(define cast-string-int
  (lambda (x)
    (exact-truncate (cast-num (string->number (destroy-prefix x))))))

(define cast-string-boundedInt
  (lambda (x y)
    (blodwen-toSignedInt (cast-string-int x) y)))

(define cast-string-boundedUInt
  (lambda (x y)
    (blodwen-toUnsignedInt (cast-string-int x) y)))

(define cast-int-char
  (lambda (x)
    (if (or
          (and (>= x 0) (<= x #xd7ff))
          (and (>= x #xe000) (<= x #x10ffff)))
        (integer->char x)
        (integer->char 0))))

(define cast-string-double
  (lambda (x)
    (cast-num (string->number (destroy-prefix x)))))

(define (string-concat xs) (apply string-append xs))
(define (string-unpack s) (string->list s))
(define (string-pack xs) (list->string xs))

(define string-cons (lambda (x y) (string-append (string x) y)))
(define string-reverse (lambda (x)
  (list->string (reverse (string->list x)))))
(define (string-substr off len s)
    (let* ((l (string-length s))
          (b (max 0 off))
          (x (max 0 len))
          (end (min l (+ b x))))
          (if (> b l)
              ""
              (substring s b end))))

(define (blodwen-string-iterator-new s)
  0)

(define (blodwen-string-iterator-to-string _ s ofs f)
  (f (substring s ofs (string-length s))))

(define (blodwen-string-iterator-next s ofs)
  (if (>= ofs (string-length s))
      '() ; EOF
      (cons (string-ref s ofs) (+ ofs 1))))

(define either-left
  (lambda (x)
    (vector 0 x)))

(define either-right
  (lambda (x)
    (vector 1 x)))

(define blodwen-error-quit
  (lambda (msg)
    (display msg)
    (newline)
    (exit 1)))

(define (blodwen-get-line p)
    (if (port? p)
        (let ((str (get-line p)))
            (if (eof-object? str)
                ""
                str))
        void))

(define (blodwen-get-char p)
    (if (port? p)
        (let ((chr (get-char p)))
            (if (eof-object? chr)
                #\nul
                chr))
        void))

;; Buffers

(define (blodwen-new-buffer size)
  (make-bytevector size 0))

(define (blodwen-buffer-size buf)
  (bytevector-length buf))

(define (blodwen-buffer-setbyte buf loc val)
  (bytevector-u8-set! buf loc val))

(define (blodwen-buffer-getbyte buf loc)
  (bytevector-u8-ref buf loc))

(define (blodwen-buffer-setbits16 buf loc val)
  (bytevector-u16-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getbits16 buf loc)
  (bytevector-u16-ref buf loc (native-endianness)))

(define (blodwen-buffer-setbits32 buf loc val)
  (bytevector-u32-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getbits32 buf loc)
  (bytevector-u32-ref buf loc (native-endianness)))

(define (blodwen-buffer-setbits64 buf loc val)
  (bytevector-u64-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getbits64 buf loc)
  (bytevector-u64-ref buf loc (native-endianness)))

(define (blodwen-buffer-setint32 buf loc val)
  (bytevector-s32-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getint32 buf loc)
  (bytevector-s32-ref buf loc (native-endianness)))

(define (blodwen-buffer-setint buf loc val)
  (bytevector-s64-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getint buf loc)
  (bytevector-s64-ref buf loc (native-endianness)))

(define (blodwen-buffer-setdouble buf loc val)
  (bytevector-ieee-double-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getdouble buf loc)
  (bytevector-ieee-double-ref buf loc (native-endianness)))

(define (blodwen-stringbytelen str)
  (bytevector-length (string->utf8 str)))

(define (blodwen-buffer-setstring buf loc val)
  (let* [(strvec (string->utf8 val))
         (len (bytevector-length strvec))]
    (bytevector-copy! strvec 0 buf loc len)))

(define (blodwen-buffer-getstring buf loc len)
  (let [(newvec (make-bytevector len))]
    (bytevector-copy! buf loc newvec 0 len)
    (utf8->string newvec)))

(define (blodwen-buffer-copydata buf start len dest loc)
  (bytevector-copy! buf start dest loc len))

;; Threads

(define-record thread-handle (semaphore))

(define (blodwen-thread proc)
  (let [(sema (blodwen-make-semaphore 0))]
    (fork-thread (lambda () (proc (vector 0)) (blodwen-semaphore-post sema)))
    (make-thread-handle sema)
    ))

(define (blodwen-thread-wait handle)
  (blodwen-semaphore-wait (thread-handle-semaphore handle)))

;; Thread mailboxes

(define blodwen-thread-data
  (make-thread-parameter #f))

(define (blodwen-get-thread-data ty)
  (blodwen-thread-data))

(define (blodwen-set-thread-data a)
  (blodwen-thread-data a))

;; Semaphore

(define-record semaphore (box mutex condition))

(define (blodwen-make-semaphore init)
  (make-semaphore (box init) (make-mutex) (make-condition)))

(define (blodwen-semaphore-post sema)
  (with-mutex (semaphore-mutex sema)
    (let [(sema-box (semaphore-box sema))]
      (set-box! sema-box (+ (unbox sema-box) 1))
      (condition-signal (semaphore-condition sema))
    )))

(define (blodwen-semaphore-wait sema)
  (with-mutex (semaphore-mutex sema)
    (let [(sema-box (semaphore-box sema))]
      (when (= (unbox sema-box) 0)
        (condition-wait (semaphore-condition sema) (semaphore-mutex sema)))
      (set-box! sema-box (- (unbox sema-box) 1))
      )))

;; Barrier

(define-record barrier (count-box num-threads mutex cond))

(define (blodwen-make-barrier num-threads)
  (make-barrier (box 0) num-threads (make-mutex) (make-condition)))

(define (blodwen-barrier-wait barrier)
  (let [(count-box (barrier-count-box barrier))
        (num-threads (barrier-num-threads barrier))
        (mutex (barrier-mutex barrier))
        (condition (barrier-cond barrier))]
    (with-mutex mutex
    (let* [(count-old (unbox count-box))
           (count-new (+ count-old 1))]
      (set-box! count-box count-new)
      (if (= count-new num-threads)
          (condition-broadcast condition)
          (condition-wait condition mutex))
      ))))

;; Channel

(define-record channel (box mutex semaphore-get semaphore-put))

(define (blodwen-make-channel ty)
  (make-channel
   (box '())
   (make-mutex)
   (blodwen-make-semaphore 0)
   (blodwen-make-semaphore 0)))

(define (blodwen-channel-get ty chan)
  (blodwen-semaphore-post (channel-semaphore-get chan))
  (blodwen-semaphore-wait (channel-semaphore-put chan))
  (with-mutex (channel-mutex chan)
    (let* [(chan-box (channel-box chan))
           (chan-msg-queue (unbox chan-box))]
      (set-box! chan-box (cdr chan-msg-queue))
      (car chan-msg-queue)
      )))

(define (blodwen-channel-put ty chan val)
  (with-mutex (channel-mutex chan)
    (let* [(chan-box (channel-box chan))
           (chan-msg-queue (unbox chan-box))]
      (set-box! chan-box (append chan-msg-queue (list val)))))
  (blodwen-semaphore-post (channel-semaphore-put chan))
  (blodwen-semaphore-wait (channel-semaphore-get chan)))

;; Mutex

(define (blodwen-make-mutex)
  (make-mutex))
(define (blodwen-mutex-acquire mutex)
  (mutex-acquire mutex))
(define (blodwen-mutex-release mutex)
  (mutex-release mutex))

;; Condition variable

(define (blodwen-make-condition)
  (make-condition))
(define (blodwen-condition-wait condition mutex)
  (condition-wait condition mutex))
(define (blodwen-condition-wait-timeout condition mutex timeout)
  (let* [(sec (div timeout 1000000))
         (micro (mod timeout 1000000))]
    (condition-wait condition mutex (make-time 'time-duration (* 1000 micro) sec))))
(define (blodwen-condition-signal condition)
  (condition-signal condition))
(define (blodwen-condition-broadcast condition)
  (condition-broadcast condition))

;; Future

(define-record future-internal (result ready mutex signal))
(define (blodwen-make-future work)
  (let ([future (make-future-internal #f #f (make-mutex) (make-condition))])
    (fork-thread (lambda ()
      (let ([result (work)])
        (with-mutex (future-internal-mutex future)
          (set-future-internal-result! future result)
          (set-future-internal-ready! future #t)
          (condition-broadcast (future-internal-signal future))))))
    future))
(define (blodwen-await-future ty future)
  (let ([mutex (future-internal-mutex future)])
    (with-mutex mutex
      (if (not (future-internal-ready future))
          (condition-wait (future-internal-signal future) mutex))
      (future-internal-result future))))

(define (blodwen-sleep s) (sleep (make-time 'time-duration 0 s)))
(define (blodwen-usleep s)
  (let ((sec (div s 1000000))
        (micro (mod s 1000000)))
       (sleep (make-time 'time-duration (* 1000 micro) sec))))

(define (blodwen-time) (time-second (current-time)))
(define (blodwen-clock-time-utc) (current-time 'time-utc))
(define (blodwen-clock-time-monotonic) (current-time 'time-monotonic))
(define (blodwen-clock-time-duration) (current-time 'time-duration))
(define (blodwen-clock-time-process) (current-time 'time-process))
(define (blodwen-clock-time-thread) (current-time 'time-thread))
(define (blodwen-clock-time-gccpu) (current-time 'time-collector-cpu))
(define (blodwen-clock-time-gcreal) (current-time 'time-collector-real))
(define (blodwen-is-time? clk) (if (time? clk) 1 0))
(define (blodwen-clock-second time) (time-second time))
(define (blodwen-clock-nanosecond time) (time-nanosecond time))


(define (blodwen-arg-count)
  (length (command-line)))

(define (blodwen-arg n)
  (if (< n (length (command-line))) (list-ref (command-line) n) ""))

(define (blodwen-hasenv var)
  (if (eq? (getenv var) #f) 0 1))

(define (blodwen-system cmd)
  (system cmd))

;; Randoms
(define random-seed-register 0)
(define (initialize-random-seed-once)
  (if (= (virtual-register random-seed-register) 0)
      (let ([seed (time-nanosecond (current-time))])
        (set-virtual-register! random-seed-register seed)
        (random-seed seed))))

(define (blodwen-random-seed seed)
  (set-virtual-register! random-seed-register seed)
  (random-seed seed))
(define blodwen-random
  (case-lambda
    ;; no argument, pick a real value from [0, 1.0)
    [() (begin
          (initialize-random-seed-once)
          (random 1.0))]
    ;; single argument k, pick an integral value from [0, k)
    [(k)
      (begin
        (initialize-random-seed-once)
        (if (> k 0)
              (random k)
              (assertion-violationf 'blodwen-random "invalid range argument ~a" k)))]))

;; For finalisers

(define blodwen-finaliser (make-guardian))
(define (blodwen-register-object obj proc)
  (let [(x (cons obj proc))]
       (blodwen-finaliser x)
       x))
(define blodwen-run-finalisers
  (lambda ()
    (let run ()
      (let ([x (blodwen-finaliser)])
        (when x
          (((cdr x) (car x)) 'erased)
          (run))))))
(define PreludeC-45IO-prim__putStr (lambda (farg-0 farg-1) ((foreign-procedure "idris2_putStr" (string) void) farg-0) (vector 0 )))
(define u--prim__add_Integer (lambda (arg-0 arg-1) (+ arg-0 arg-1)))
(define u--prim__sub_Integer (lambda (arg-0 arg-1) (- arg-0 arg-1)))
(define u--prim__mul_Integer (lambda (arg-0 arg-1) (* arg-0 arg-1)))
(define Builtin-believe_me (lambda (ext-0) ext-0))
(define PreludeC-45Types-u--compare_Ord_Nat (lambda (arg-0 arg-1) (cond ((equal? arg-0 0) (cond ((equal? arg-1 0) 1)(else 0)))(else (let ((e-0 (- arg-0 1))) (cond ((equal? arg-1 0) 2)(else (let ((e-2 (- arg-1 1))) (PreludeC-45Types-u--compare_Ord_Nat e-0 e-2)))))))))
(define PreludeC-45Types-prim__integerToNat (lambda (arg-0) (let ((sc0 (let ((sc1 (or (and (<= 0 arg-0) 1) 0))) (cond ((equal? sc1 0) 0)(else 1))))) (cond ((equal? sc0 1) (Builtin-believe_me arg-0)) (else 0)))))
(define PreludeC-45TypesC-45String-C-43C-43 (lambda (arg-0 arg-1) (string-append arg-0 arg-1)))
(define PreludeC-45Num-fromInteger (lambda (arg-1 ext-0) (let ((e-3 (vector-ref arg-1 2))) (e-3 ext-0))))
(define PreludeC-45Num-defaultInteger (lambda () (vector (lambda (arg-2) (lambda (arg-3) (+ arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (* arg-4 arg-5))) (lambda (arg-6) arg-6))))
(define PreludeC-45Num-C-42 (lambda (arg-1 ext-1 ext-0) (let ((e-2 (vector-ref arg-1 1))) ((e-2 ext-1) ext-0))))
(define PreludeC-45EqOrd-u--compare_Ord_Integer (lambda (arg-0 arg-1) (let ((sc0 (PreludeC-45EqOrd-u--C-60_Ord_Integer arg-0 arg-1))) (cond ((equal? sc0 1) 0) (else (let ((sc1 (PreludeC-45EqOrd-u--C-61C-61_Eq_Integer arg-0 arg-1))) (cond ((equal? sc1 1) 1) (else 2))))))))
(define PreludeC-45EqOrd-u--C-61C-61_Eq_Ordering (lambda (arg-0 arg-1) (cond ((equal? arg-0 0) (cond ((equal? arg-1 0) 1)(else 0))) ((equal? arg-0 1) (cond ((equal? arg-1 1) 1)(else 0))) ((equal? arg-0 2) (cond ((equal? arg-1 2) 1)(else 0)))(else 0))))
(define PreludeC-45EqOrd-u--C-61C-61_Eq_Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 0)(else 1)))))
(define PreludeC-45EqOrd-u--C-61C-61_Eq_Char (lambda (arg-0 arg-1) (let ((sc0 (or (and (char=? arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 0)(else 1)))))
(define PreludeC-45EqOrd-u--C-60_Ord_Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (< arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 0)(else 1)))))
(define PreludeC-45EqOrd-u--C-47C-61_Eq_Ordering (lambda (arg-0 arg-1) (let ((sc0 (PreludeC-45EqOrd-u--C-61C-61_Eq_Ordering arg-0 arg-1))) (cond ((equal? sc0 1) 0) (else 1)))))
(define PrimIO-unsafePerformIO (lambda (arg-1) (PrimIO-unsafeCreateWorld (lambda (u--w) (PrimIO-unsafeDestroyWorld 'erased (arg-1 u--w))))))
(define PrimIO-unsafeDestroyWorld (lambda (arg-1 arg-2) arg-2))
(define PrimIO-unsafeCreateWorld (lambda (arg-1) (arg-1 #f)))
(define PreludeC-45Show-u--show_Show_Integer (lambda (arg-0) (PreludeC-45Show-u--showPrec_Show_Integer (vector 0 ) arg-0)))
(define PreludeC-45Show-u--showPrec_Show_Integer (lambda (ext-0 ext-1) (PreludeC-45Show-primNumShow (lambda (eta-0) (number->string eta-0)) ext-0 ext-1)))
(define PreludeC-45Show-u--compare_Ord_Prec (lambda (arg-0 arg-1) (case (vector-ref arg-0 0) ((4) (let ((e-0 (vector-ref arg-0 1))) (case (vector-ref arg-1 0) ((4) (let ((e-1 (vector-ref arg-1 1))) (PreludeC-45Types-u--compare_Ord_Nat e-0 e-1)))(else (PreludeC-45EqOrd-u--compare_Ord_Integer (PreludeC-45Show-precCon arg-0) (PreludeC-45Show-precCon arg-1))))))(else (PreludeC-45EqOrd-u--compare_Ord_Integer (PreludeC-45Show-precCon arg-0) (PreludeC-45Show-precCon arg-1))))))
(define PreludeC-45Show-u--__Impl_Show_Integer (lambda () (cons (lambda (u--x) (PreludeC-45Show-u--show_Show_Integer u--x)) (lambda (u--d) (lambda (u--x) (PreludeC-45Show-u--showPrec_Show_Integer u--d u--x))))))
(define PreludeC-45Show-u--C-62C-61_Ord_Prec (lambda (arg-0 arg-1) (PreludeC-45EqOrd-u--C-47C-61_Eq_Ordering (PreludeC-45Show-u--compare_Ord_Prec arg-0 arg-1) 0)))
(define PreludeC-45Show-showParens (lambda (arg-0 arg-1) (cond ((equal? arg-0 0) arg-1) (else (PreludeC-45TypesC-45String-C-43C-43 "(" (PreludeC-45TypesC-45String-C-43C-43 arg-1 ")"))))))
(define PreludeC-45Show-show (lambda (arg-1 ext-0) (let ((e-1 (car arg-1))) (e-1 ext-0))))
(define PreludeC-45Show-primNumShow (lambda (arg-1 arg-2 arg-3) (let ((u--str (arg-1 arg-3))) (PreludeC-45Show-showParens (let ((sc0 (PreludeC-45Show-u--C-62C-61_Ord_Prec arg-2 (vector 5 )))) (cond ((equal? sc0 1) (PreludeC-45Show-firstCharIs (lambda (arg-0) (PreludeC-45EqOrd-u--C-61C-61_Eq_Char arg-0 #\-)) u--str)) (else 0))) u--str))))
(define PreludeC-45Show-precCon (lambda (arg-0) (case (vector-ref arg-0 0) ((0) 0) ((1) 1) ((2) 2) ((3) 3) ((4) 4) ((5) 5) (else 6))))
(define PreludeC-45Show-firstCharIs (lambda (arg-0 arg-1) (cond ((equal? arg-1 "") 0)(else (arg-0 (string-ref arg-1 0))))))
(define PreludeC-45IO-u--map_Functor_IO (lambda (arg-2 arg-3 ext-0) (let ((act-5 (arg-3 ext-0))) (arg-2 act-5))))
(define PreludeC-45IO-u--__Impl_HasLinearIO_IO (lambda () (cons (vector (vector (lambda (u--b) (lambda (u--a) (lambda (u--func) (lambda (arg-133) (lambda (eta-0) (PreludeC-45IO-u--map_Functor_IO u--func arg-133 eta-0)))))) (lambda (u--a) (lambda (arg-527) (lambda (eta-0) arg-527))) (lambda (u--b) (lambda (u--a) (lambda (arg-528) (lambda (arg-530) (lambda (eta-0) (let ((act-17 (arg-528 eta-0))) (let ((act-16 (arg-530 eta-0))) (act-17 act-16))))))))) (lambda (u--b) (lambda (u--a) (lambda (arg-811) (lambda (arg-812) (lambda (eta-0) (let ((act-24 (arg-811 eta-0))) ((arg-812 act-24) eta-0))))))) (lambda (u--a) (lambda (arg-814) (lambda (eta-0) (let ((act-51 (arg-814 eta-0))) (act-51 eta-0)))))) (lambda (u--a) (lambda (arg-6923) arg-6923)))))
(define PreludeC-45IO-u--__Impl_HasIO_C-36io (lambda (arg-1) (cons (let ((e-1 (car arg-1))) e-1) (lambda (u--a) (lambda (arg-6894) (let ((e-2 (cdr arg-1))) ((e-2 'erased) arg-6894)))))))
(define PreludeC-45IO-putStrLn (lambda (arg-1 arg-2) (PreludeC-45IO-putStr arg-1 (string-append arg-2 "\xa;"))))
(define PreludeC-45IO-putStr (lambda (arg-1 arg-2) (let ((e-2 (cdr arg-1))) ((e-2 'erased) (lambda (eta-0) (PreludeC-45IO-prim__putStr arg-2 eta-0))))))
(load-shared-object "libidris2_support.dylib")
(collect-request-handler (lambda () (collect) (blodwen-run-finalisers)))
(PrimIO-unsafePerformIO (PreludeC-45IO-putStrLn (PreludeC-45IO-u--__Impl_HasIO_C-36io (PreludeC-45IO-u--__Impl_HasLinearIO_IO)) (PreludeC-45Show-show (PreludeC-45Show-u--__Impl_Show_Integer) (PreludeC-45Num-C-42 (PreludeC-45Num-defaultInteger) (PreludeC-45Num-fromInteger (PreludeC-45Num-defaultInteger) 47) (PreludeC-45Num-fromInteger (PreludeC-45Num-defaultInteger) 2)))))(collect 4)
(blodwen-run-finalisers)
)
