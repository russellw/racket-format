#lang racket
(require "etc.rkt")
(require "read.rkt")
(provide blank-symbol)
(provide tidy)

(define (add-betweenf lst proc)
 (cond
  ((null? lst)
   lst)
  ((null? (cdr lst))
   lst)
  (else
   (let ((v (proc (car lst) (cadr lst))))
    (if v
     (list* (car lst) v (add-betweenf (cdr lst) proc))
     (list* (car lst) (add-betweenf (cdr lst) proc)))))))

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

(define (name x)
 (set! x (cadr x))
 (if (pair? x)
  (car x)
  x))

(define (prelude? v)
 (match v
  ((list (== lang-symbol) s)
   #t)
  ((list 'require w)
   #t)
  ((list 'provide w)
   #t)
  (_
   #f)))

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
       (map-lists y m
        (match y
         ((list (== comment-symbol) s)
          #:when
          (char-alphabetic? (string-ref s 1))
          `(,comment-symbol ,(string-append "; " (substring s 1))))
         (_
          y))))

 ; sort
 (set! m
  (map-lists x m
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

    ; functions
    (set! x
          (append* (for/list ((fragment (fragments defun? x)))
                             (if (defun? (car fragment))
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

 ; blank lines around prelude
 (set! m
       (add-betweenf m
                     (lambda (v w)
                      (cond
                       ((eq? v blank-symbol)
                        #f)
                       ((eq? w blank-symbol)
                        #f)
                       ((xor (prelude? v) (prelude? w))
                        blank-symbol)
                       (else
                        #f)))))

 ; blank lines around functions
 (set! m
       (map-lists x m
        (add-betweenf x
                      (lambda (v w)
                       (cond
                        ((eq? v blank-symbol)
                         #f)
                        ((eq? w blank-symbol)
                         #f)
                        ((or (defun? v)
                             (defun? w))
                         blank-symbol)
                        (else
                         #f))))))

 ; blank lines before comments
 (set! m
       (map-lists x m
        (add-betweenf x
                      (lambda (v w)
                       (cond
                        ((eq? v blank-symbol)
                         #f)
                        ((and (not (car? comment-symbol v))
                              (car? comment-symbol w))
                         blank-symbol)
                        (else
                         #f))))))

 ; result
 m)

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

(define blank-symbol (gensym))
