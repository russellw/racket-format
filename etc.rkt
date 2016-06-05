#lang racket
(provide (all-defined-out))
(define-syntax (cond-trace stx)
 (syntax-case
  stx
  (else)
  ((_ (else b ...)) #`(let ((r #f))
                       (begin
                        (indent trace-level (current-error-port))
                        (display #,(syntax-source stx) (current-error-port))
                        (display ":" (current-error-port))
                        (display #,(syntax-line stx) (current-error-port))
                        (display ": else" (current-error-port))
                        (newline (current-error-port))
                        (inc-trace-level!)
                        (set! r
                              (let ()
                               b
                               ...))
                        (dec-trace-level!)
                        r)))
  ((_ (x b ...) rest ...)
   #`(let ((c x)
           (r #f))
      (if c
       (begin
        (indent trace-level (current-error-port))
        (display #,(syntax-source stx) (current-error-port))
        (display ":" (current-error-port))
        (display #,(syntax-line stx) (current-error-port))
        (display ": " (current-error-port))
        (write 'x (current-error-port))
        (display ": " (current-error-port))
        (write c (current-error-port))
        (newline (current-error-port))
        (inc-trace-level!)
        (set! r
              (let ()
               b
               ...))
        (dec-trace-level!)
        r)
       (cond-trace rest ...))))))

(define-syntax (debug stx)
 (syntax-case stx
              ()
              ((_ x) #`(let ((r x))
                        (indent trace-level (current-error-port))
                        (display #,(syntax-source stx) (current-error-port))
                        (display ":" (current-error-port))
                        (display #,(syntax-line stx) (current-error-port))
                        (display ": " (current-error-port))
                        (write 'x (current-error-port))
                        (display ": " (current-error-port))
                        (write r (current-error-port))
                        (newline (current-error-port))
                        r))))

(define-syntax (if-trace stx)
 (syntax-case
  stx
  ()
  ((_ x true false) #`(let ((c x)
                            (r #f))
                       (indent trace-level (current-error-port))
                       (display #,(syntax-source stx) (current-error-port))
                       (display ":" (current-error-port))
                       (display #,(syntax-line stx) (current-error-port))
                       (display ": " (current-error-port))
                       (write 'x (current-error-port))
                       (display ": " (current-error-port))
                       (write c (current-error-port))
                       (newline (current-error-port))
                       (inc-trace-level!)
                       (set! r
                             (if c
                              true
                              false))
                       (dec-trace-level!)
                       r))))

(define-syntax (unless-trace stx)
 (syntax-case
  stx
  ()
  ((_ x body ...) #`(let ((c x)
                          (r #f))
                     (indent trace-level (current-error-port))
                     (display #,(syntax-source stx) (current-error-port))
                     (display ":" (current-error-port))
                     (display #,(syntax-line stx) (current-error-port))
                     (display ": " (current-error-port))
                     (write 'x (current-error-port))
                     (display ": " (current-error-port))
                     (write c (current-error-port))
                     (newline (current-error-port))
                     (inc-trace-level!)
                     (set! r
                           (unless c
                            body
                            ...))
                     (dec-trace-level!)
                     r))))

(define-syntax (when-trace stx)
 (syntax-case
  stx
  ()
  ((_ x body ...) #`(let ((c x)
                          (r #f))
                     (indent trace-level (current-error-port))
                     (display #,(syntax-source stx) (current-error-port))
                     (display ":" (current-error-port))
                     (display #,(syntax-line stx) (current-error-port))
                     (display ": " (current-error-port))
                     (write 'x (current-error-port))
                     (display ": " (current-error-port))
                     (write c (current-error-port))
                     (newline (current-error-port))
                     (inc-trace-level!)
                     (set! r
                           (when c
                            body
                            ...))
                     (dec-trace-level!)
                     r))))

(define-syntax any-rec?
 (syntax-rules ()
  ((_ x xs body ...)
   (let loop ((x xs))
    (or (begin
         body
         ...)
        (and (pair? x)
             (or (loop (car x))
                 (loop (cdr x)))))))))

(define-syntax collect
 (syntax-rules ()
  ((_ body ...)
   (let loop ()
    (define x
            (let ()
             body
             ...))
    (if x
     (append x (loop))
     '())))))

(define-syntax dec!
 (syntax-rules ()
  ((_ x)
   (set! x (- x 1)))
  ((_ x delta)
   (set! x (- x delta)))))

(define-syntax filt
 (syntax-rules ()
  ((_ x xs body ...)
   (filter (lambda (x)
            body
            ...)
           xs))))

(define-syntax for*
 (syntax-rules ()
  ((_ x xs ys body ...)
   (let loop ((xs ys))
    (if (atom? xs)
     xs
     (let ((x (car xs)))
      (cons (begin
             body
             ...)
            (loop (cdr xs)))))))))

(define-syntax inc!
 (syntax-rules ()
  ((_ x)
   (set! x (+ x 1)))
  ((_ x delta)
   (set! x (+ x delta)))))

(define-syntax map-rec
 (syntax-rules ()
  ((_ x xs body ...)
   (let loop ((x xs))
    (when (pair? x)
     (set! x (cons (loop (car x)) (loop (cdr x)))))
    (begin
     body
     ...)))))

(define-syntax receive
 (syntax-rules ()
  ((receive formals
    expression
    body
    ...)
   (call-with-values (lambda ()
                      expression)
                     (lambda formals
                      body
                      ...)))))

(define-syntax trace
 (syntax-rules ()
  ((_ f)
   (let ((g f))
    (set! f
          (lambda args
           (define r #f)

           ; Trace call
           (indent trace-level (current-error-port))
           (write (cons 'f args) (current-error-port))
           (newline (current-error-port))

           ; Call
           (inc-trace-level!)
           (set! r (apply g args))
           (dec-trace-level!)

           ; Trace result
           (indent trace-level (current-error-port))
           (display "-> " (current-error-port))
           (write r (current-error-port))
           (newline (current-error-port))

           ; Result
           r))))))

(define-syntax transform
 (syntax-rules ()
  ((_ xs ys body ...)
   (let loop ((xs ys))
    (if (atom? xs)
     xs
     (receive (xs ys)
      (begin
       body
       ...)
      (append xs (loop ys))))))))

(define (atom? x)
 (not (pair? x)))

(define (cadr? x y)
 (and (pair? y)
      (pair? (cdr y))
      (eqv? x (cadr y))))

(define (car? x y)
 (and (pair? y)
      (eqv? x (car y))))

(define (dec-trace-level!)
 (dec! trace-level))

(define (defun? x)
 (and (car? 'define x)
      (length? 2 x)
      (pair? (cadr x))))

(define (defvar? x)
 (and (car? 'define x)
      (length? 2 x)
      (atom? (cadr x))))

(define (frag p xs)
 (cond
  ((atom? xs)
   xs)
  ((p (car xs))
   (receive (a b)
    (splitf-at xs p)
    (cons a (frag p b))))
  (else
   (receive (a b)
    (splitf-at xs (negate p))
    (cons a (frag p b))))))

(define (improper-list? x)
 (cond
  ((null? x)
   #f)
  ((atom? x)
   #t)
  (else
   (improper-list? (cdr x)))))

(define (inc-trace-level!)
 (inc! trace-level))

(define (indent n (port (current-output-port)))
 (for ((i n)) (display " " port)))

(define (length? n x)
 (if (list? x)
  (<= n (length x))
  (<= n (string-length x))))

(define (list< xs ys)
 (cond
  ((eq? xs ys)
   #f)
  ((null? xs)
   #t)
  ((value<? (car xs) (car ys))
   #t)
  ((value<? (car ys) (car xs))
   #f)
  (else
   (list< (cdr xs) (cdr ys)))))

(define (symbol<? x y)
 (string<? (symbol->string x) (symbol->string y)))

(define (typeof x)
 (cond
  ((boolean? x)
   'boolean)
  ((char? x)
   'char)
  ((null? x)
   'null)
  ((number? x)
   'number)
  ((pair? x)
   'pair)
  ((port? x)
   'port)
  ((procedure? x)
   'procedure)
  ((string? x)
   'string)
  ((symbol? x)
   'symbol)
  ((vector? x)
   'vector)
  (else
   #f)))

(define (value<? x y)
 (if (eq? (typeof x) (typeof y))
  (cond
   ((char? x)
    (char<? x y))
   ((number? x)
    (< x y))
   ((pair? x)
    (list< x y))
   ((string? x)
    (string<? x y))
   ((symbol? x)
    (symbol<? x y)))
  (symbol<? (typeof x) (typeof y))))

(define trace-level 0)
