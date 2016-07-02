#lang racket
(provide (all-defined-out))

(define (atom? x)
 (not (pair? x)))

(define-syntax (debug stx)
 (syntax-case stx ()
  ((_ x)
   #`(let ((r x))
      (eprintf "~a:~a: ~s: ~s\n"
       #,(syntax-source stx)
       #,(syntax-line stx)
       'x
       r)
      r))))

(define (decl? v)
 (match v
  ((list (or 'define
             'define/memo
             'define/memo*)
         (list w ...)
         b ...)
   #t)
  ((list 'define-syntax b ...)
   #t)
  (_ #f)))

(define-syntax receive
 (syntax-rules ()
  ((receive args
    x
    b ...)
   (call-with-values (lambda ()
                      x)
                     (lambda args
                      b ...)))))
