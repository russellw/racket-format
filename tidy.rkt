#lang racket
(require "etc.rkt" "read.rkt")

(provide tidy)

(define (decl? x)
 (match x
  ((list 'define (list w ...) b ...)
   #t)
  ((list 'define-syntax b ...)
   #t)
  (_
   #f)))

(define-syntax for/sublists
 (syntax-rules ()
  ((_ ((x x1)) b ...)
   (let loop ((x x1))
    (if (atom? x)
     x
     (begin
      (set! x
            (for/list ((y x))
             (loop y)))
      b
      ...))))))

(define (fragments p xs)
 (cond
  ((null? xs)
   xs)
  ((p (car xs))
   (receive (a b)
    (splitf-at xs p)
    (cons a (fragments p b))))
  (else
   (receive (a b)
    (splitf-at xs (negate p))
    (cons a (fragments p b))))))

(define (list<? xs ys)
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
   (list<? (cdr xs) (cdr ys)))))

(define (name x)
 (match x
  ((list def (list id args ...) b ...)
   id)
  ((list def id b ...)
   id)))

(define (sort-case v)
 (match v
  ((list (list c ...) b ...)
   (cons (sort c value<?) b))
  (_
   v)))

(define (sort-cases lst)
 (set! lst (map sort-case lst))
 (sort lst value<?))

(define (tidy m)
 ; space at start of comment
 (set! m
       (for/sublists ((y m))
        (match y
         ((list (== comment-symbol) s)
          #:when
          (char-alphabetic? (string-ref s 1))
          (list comment-symbol (string-append "; " (substring s 1))))
         (_
          y))))

 ; sort
 (for/sublists ((x m))
  (begin
   ; case
   (set! x
         (match x
          ((list 'case v lst ...)
           `(case ,v
             (unquote-splicing
              (sort-cases lst))))
          (_
           x)))

   ; declarations
   (set! x
         (append* (for/list ((fragment (fragments decl? x)))
                   (if (decl? (car fragment))
                    (sort fragment
                          (lambda (a b)
                           (symbol<? (name a) (name b))))
                    fragment))))

   ; provides
   (set! x
         (append* (for/list ((fragment (fragments (curry car? 'provide) x)))
                   (if (car? 'provide (car fragment))
                    (sort fragment value<?)
                    fragment))))

   ; requires
   (set! x
         (append* (for/list ((fragment (fragments (curry car? 'require) x)))
                   (if (car? 'require (car fragment))
                    (sort fragment value<?)
                    fragment))))

   ; sorted
   x)))

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
  ((string? x)
   'string)
  ((symbol? x)
   'symbol)
  ((vector? x)
   'vector)))

(define (value<? x y)
 (if (eq? (typeof x) (typeof y))
  (cond
   ((char? x)
    (char<? x y))
   ((number? x)
    (< x y))
   ((pair? x)
    (list<? x y))
   ((string? x)
    (string<? x y))
   ((symbol? x)
    (symbol<? x y)))
  (symbol<? (typeof x) (typeof y))))
