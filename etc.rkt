#lang racket
(provide (all-defined-out))
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
                               (eprintf "~a~a:~a: ~s: ~a\n"
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
                        (eprintf "~a~a:~a: ~s: ~a\n"
                                 (make-string trace-level #\space)
                                 #,(syntax-source stx)
                                 #,(syntax-line stx)
                                 'x
                                 r)
                        r))))

(define-syntax (if-trace stx)
 (syntax-case stx
              ()
              ((_ x true false) #`(let ((c x)
                                        (r #f))
                                   (eprintf "~a~a:~a: ~s: ~a\n"
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

(define-syntax (unless-trace stx)
 (syntax-case stx
              ()
              ((_ x body ...) #`(let ((c x)
                                      (r #f))
                                 (eprintf "~a~a:~a: ~s: ~a\n"
                                          (make-string trace-level #\space)
                                          #,(syntax-source stx)
                                          #,(syntax-line stx)
                                          'x
                                          c)
                                 (inc-trace-level!)
                                 (set! r
                                       (unless c
                                        body
                                        ...))
                                 (dec-trace-level!)
                                 r))))

(define-syntax (when-trace stx)
 (syntax-case stx
              ()
              ((_ x body ...) #`(let ((c x)
                                      (r #f))
                                 (eprintf "~a~a:~a: ~s: ~a\n"
                                          (make-string trace-level #\space)
                                          #,(syntax-source stx)
                                          #,(syntax-line stx)
                                          'x
                                          c)
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
   (dec! x 1))
  ((_ x delta)
   (set! x (- x delta)))))

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
   (inc! x 1))
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

(define trace-level 0)
