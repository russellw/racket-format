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

(define-syntax (debug stx)
 (syntax-case
  stx
  ()
  ((_ x)
   #`(let ((r x))
      (eprintf "~a:~a: ~s: ~s\n"
               #,(syntax-source stx)
               #,(syntax-line stx)
               'x
               r)
      r))))

(define (defun? x)
 (match x
  ((list 'define (list w ...) b ...)
   #t)
  ((list 'define-syntax b ...)
   #t)
  (_
   #f)))

(define (defvar? x)
 (and (car? 'define x)
      (length? 2 x)
      (atom? (cadr x))))

(define (length? n x)
 (<= n (length x)))

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
