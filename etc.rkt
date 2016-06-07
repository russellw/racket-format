#lang racket
(provide (all-defined-out))
(define-syntax any-rec?
 (syntax-rules ()
  ((_ x xs b ...)
   (let loop ((x xs))
    (or (begin
         b
         ...)
        (and (pair? x)
             (or (loop (car x))
                 (loop (cdr x)))))))))

(define (atom? x)
 (not (pair? x)))

(define (cadr? x y)
 (and (pair? y)
      (pair? (cdr y))
      (eqv? x (cadr y))))

(define (car? x y)
 (and (pair? y)
      (eqv? x (car y))))

(define-syntax collect
 (syntax-rules ()
  ((_ b ...)
   (let loop ()
    (define x
            (let ()
             b
             ...))
    (if x
     (append x (loop))
     '())))))

(define-syntax (cond-trace stx)
 (syntax-case
  stx
  (else)
  ((_ (else b ...)) #`(let ((r #f))
                       (eprintf "~a~a:~a: else\n"
                                (make-string trace-level #\space)
                                #,(syntax-source stx)
                                #,(syntax-line stx))
                       (inc-trace-level!)
                       (set! r
                             (let ()
                              b
                              ...))
                       (dec-trace-level!)
                       r))
  ((_ (x b ...) rest ...) #`(let ((c x)
                                  (r #f))
                             (if c
                              (begin
                               (eprintf "~a~a:~a: ~s -> ~a\n"
                                        (make-string trace-level #\space)
                                        #,(syntax-source stx)
                                        #,(syntax-line stx)
                                        'x
                                        c)
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
                        (eprintf "~a~a:~a: ~s -> ~a\n"
                                 (make-string trace-level #\space)
                                 #,(syntax-source stx)
                                 #,(syntax-line stx)
                                 'x
                                 r)
                        r))))

(define-syntax dec!
 (syntax-rules ()
  ((_ x)
   (dec! x 1))
  ((_ x delta)
   (set! x (- x delta)))))

(define (dec-trace-level!)
 (dec! trace-level))

(define (defun? x)
 (or (and (car? 'define x)
          (length? 2 x)
          (pair? (cadr x)))
     (and (car? 'define-syntax x)
          (length? 2 x))))

(define (defvar? x)
 (and (car? 'define x)
      (length? 2 x)
      (atom? (cadr x))))

(define-syntax for*
 (syntax-rules ()
  ((_ x xs ys b ...)
   (let loop ((xs ys))
    (if (atom? xs)
     xs
     (let ((x (car xs)))
      (cons (begin
             b
             ...)
            (loop (cdr xs)))))))))

(define (fragments p xs)
 (cond
  ((atom? xs)
   xs)
  ((p (car xs))
   (receive (a b)
    (splitf-at xs p)
    (cons a (fragments p b))))
  (else
   (receive (a b)
    (splitf-at xs (negate p))
    (cons a (fragments p b))))))

(define-syntax (if-trace stx)
 (syntax-case stx
              ()
              ((_ x true false) #`(let ((c x)
                                        (r #f))
                                   (eprintf "~a~a:~a: ~s -> ~a\n"
                                            (make-string trace-level #\space)
                                            #,(syntax-source stx)
                                            #,(syntax-line stx)
                                            'x
                                            c)
                                   (inc-trace-level!)
                                   (set! r
                                         (if c
                                          true
                                          false))
                                   (dec-trace-level!)
                                   r))))

(define (improper-list? x)
 (cond
  ((null? x)
   #f)
  ((atom? x)
   #t)
  (else
   (improper-list? (cdr x)))))

(define-syntax inc!
 (syntax-rules ()
  ((_ x)
   (inc! x 1))
  ((_ x delta)
   (set! x (+ x delta)))))

(define (inc-trace-level!)
 (inc! trace-level))

(define (indent n (port (current-output-port)))
 (for ((i n)) (display " " port)))

(define (length? n x)
 (<= n (length x)))

(define-syntax map-rec
 (syntax-rules ()
  ((_ x xs b ...)
   (let loop ((x xs))
    (when (pair? x)
     (set! x (cons (loop (car x)) (loop (cdr x)))))
    (begin
     b
     ...)))))

(define-syntax receive
 (syntax-rules ()
  ((receive args
    x
    b
    ...)
   (call-with-values (lambda ()
                      x)
                     (lambda args
                      b
                      ...)))))

(define-syntax trace
 (syntax-rules ()
  ((_ f)
   (let ((g f))
    (set! f
          (lambda args
           (define r #f)

           ; trace call
           (indent trace-level (current-error-port))
           (write (cons 'f args) (current-error-port))
           (newline (current-error-port))

           ; call
           (inc-trace-level!)
           (set! r (apply g args))
           (dec-trace-level!)

           ; trace result
           (indent trace-level (current-error-port))
           (display "-> " (current-error-port))
           (write r (current-error-port))
           (newline (current-error-port))

           ; result
           r))))))

(define-syntax transform
 (syntax-rules ()
  ((_ xs ys b ...)
   (let loop ((xs ys))
    (if (atom? xs)
     xs
     (receive (xs ys)
      (begin
       b
       ...)
      (append xs (loop ys))))))))

(define-syntax (unless-trace stx)
 (syntax-case stx
              ()
              ((_ x b ...) #`(let ((c x)
                                   (r #f))
                              (eprintf "~a~a:~a: ~s -> ~a\n"
                                       (make-string trace-level #\space)
                                       #,(syntax-source stx)
                                       #,(syntax-line stx)
                                       'x
                                       c)
                              (inc-trace-level!)
                              (set! r
                                    (unless c
                                     b
                                     ...))
                              (dec-trace-level!)
                              r))))

(define-syntax (when-trace stx)
 (syntax-case stx
              ()
              ((_ x b ...) #`(let ((c x)
                                   (r #f))
                              (eprintf "~a~a:~a: ~s -> ~a\n"
                                       (make-string trace-level #\space)
                                       #,(syntax-source stx)
                                       #,(syntax-line stx)
                                       'x
                                       c)
                              (inc-trace-level!)
                              (set! r
                                    (when c
                                     b
                                     ...))
                              (dec-trace-level!)
                              r))))

(define trace-level 0)
