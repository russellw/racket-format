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
      (equal? x (cadr y))))

(define (car? x y)
 (and (pair? y)
      (equal? x (car y))))

(define-syntax (cond-trace stx)
 (syntax-case
  stx
  (else)
  ((_ (else b ...)) #`(let ()
                       (eprintf "~a~a:~a: else\n"
                                (make-string (trace-level) #\space)
                                #,(syntax-source stx)
                                #,(syntax-line stx))
                       (parameterize ((trace-level (add1 (trace-level))))
                                     (let ()
                                      b
                                      ...))))
  ((_ (x b ...) rest ...)
   #`(let ((c x)
           (r #f))
      (if c
       (begin
        (eprintf "~a~a:~a: ~s -> ~s\n"
                 (make-string (trace-level) #\space)
                 #,(syntax-source stx)
                 #,(syntax-line stx)
                 'x
                 c)
        (set! r
              (parameterize ((trace-level (add1 (trace-level))))
                            (let ()
                             b
                             ...)))
        r)
       (cond-trace rest ...))))))

(define-syntax (debug stx)
 (syntax-case stx
              ()
              ((_ x) #`(let ((r x))
                        (eprintf "~a~a:~a: ~s -> ~s\n"
                                 (make-string (trace-level) #\space)
                                 #,(syntax-source stx)
                                 #,(syntax-line stx)
                                 'x
                                 r)
                        r))))

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

(define-syntax (if-trace stx)
 (syntax-case
  stx
  ()
  ((_ x true false) #`(let ((c x))
                       (eprintf "~a~a:~a: ~s -> ~s\n"
                                (make-string (trace-level) #\space)
                                #,(syntax-source stx)
                                #,(syntax-line stx)
                                'x
                                c)
                       (parameterize ((trace-level (add1 (trace-level))))
                                     (if c
                                      true
                                      false))))))

(define (length? n x)
 (<= n (length x)))

(define-syntax map-lists
 (syntax-rules ()
  ((_ x x1 b ...)
   (let loop ((x x1))
    (if (atom? x)
     x
     (begin
      (set! x (for/list ((y x)) (loop y)))
      b
      ...))))))

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
      (eprintf "~a~s\n" (make-string (trace-level) #\space) (cons 'f args))
      (set! r
            (parameterize ((trace-level (add1 (trace-level)))) (apply g args)))
      (eprintf "~a-> ~s\n" (make-string (trace-level) #\space) r)
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
 (syntax-case
  stx
  ()
  ((_ x b ...) #`(let ((c x))
                  (eprintf "~a~a:~a: ~s -> ~s\n"
                           (make-string (trace-level) #\space)
                           #,(syntax-source stx)
                           #,(syntax-line stx)
                           'x
                           c)
                  (parameterize ((trace-level (add1 (trace-level))))
                                (unless c
                                 b
                                 ...))))))

(define-syntax (when-trace stx)
 (syntax-case
  stx
  ()
  ((_ x b ...) #`(let ((c x))
                  (eprintf "~a~a:~a: ~s -> ~s\n"
                           (make-string (trace-level) #\space)
                           #,(syntax-source stx)
                           #,(syntax-line stx)
                           'x
                           c)
                  (parameterize ((trace-level (add1 (trace-level))))
                                (when c
                                 b
                                 ...))))))

(define-syntax while/list
 (syntax-rules ()
  ((_ c b ...)
   (let loop ()
    (if c
     (cons (let ()
            b
            ...)
           (loop))
     '())))))

(define trace-level (make-parameter 0))
