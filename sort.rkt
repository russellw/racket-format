#lang racket
(require "etc.rkt"
         "read.rkt")

(provide sort-module)

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
      b ...))))))

(define (fragments pred lst)
 (cond
  ((null? lst)
   lst)
  ((pred (car lst))
   (receive (a b)
    (splitf-at lst pred)
    (cons a (fragments pred b))))
  (else
   (receive (a b)
    (splitf-at lst (negate pred))
    (cons a (fragments pred b))))))

(define (list<? lst1 lst2)
 (cond
  ((eq? lst1 lst2)
   #f)
  ((null? lst1)
   #t)
  ((value<? (car lst1) (car lst2))
   #t)
  ((value<? (car lst2) (car lst1))
   #f)
  (else
   (list<? (cdr lst1) (cdr lst2)))))

(define (name x)
 (match x
  ((list def (list id args ...) b ...)
   id)
  ((list def id b ...)
   id)))

(define (quoted-symbol? v)
 (match v
  ((list (== quote-symbol) w)
   (symbol? w))
  (_ #f)))

(define (sort-case v)
 (match v
  ((list (list c ...) b ...)
   (cons (sort c value<?) b))
  (_ v)))

(define (sort-cases lst)
 (set! lst (map sort-case lst))
 (sort lst value<?))

(define (sort-module m)
 (for/sublists ((x m))
  (begin
   (set! x
         (match x
          ((list 'case a b ...)
           `(case ,a
             ,@(sort-cases b)))
          ((list 'or b ...)
           #:when
           (andmap quoted-symbol? b)
           `(or ,@(sort b value<?)))
          ((list 'provide b ...)
           `(provide ,@(sort b value<?)))
          ((list 'require b ...)
           `(require ,@(sort b value<?)))
          (_ x)))
   (append* (for/list ((fragment (fragments decl? x)))
             (if (decl? (car fragment))
              (sort fragment
                    (lambda (v w)
                     (symbol<? (name v) (name w))))
              fragment))))))

(define (typeof v)
 (cond
  ((boolean? v)
   'boolean)
  ((char? v)
   'char)
  ((null? v)
   'null)
  ((number? v)
   'number)
  ((pair? v)
   'pair)
  ((string? v)
   'string)
  ((symbol? v)
   'symbol)
  ((vector? v)
   'vector)))

(define (value<? v w)
 (if (eq? (typeof v) (typeof w))
  (cond
   ((char? v)
    (char<? v w))
   ((number? v)
    (< v w))
   ((pair? v)
    (list<? v w))
   ((string? v)
    (string<? v w))
   ((symbol? v)
    (symbol<? v w)))
  (symbol<? (typeof v) (typeof w))))
