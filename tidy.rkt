#lang racket
(require "etc.rkt")
(require "read.rkt")
(provide blank-symbol)
(provide tidy)
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

(define (tidy m)
 ; space at start of comment
 (set! m
  (map-lists y m
   (if (and (car? comment-symbol y)
            (char-alphabetic? (string-ref (cadr y) 1)))
    (list comment-symbol
          (string-append "; " (substring (cadr y) 1 (string-length (cadr y)))))
    y)))

 ; sort
 (set! m
  (map-lists x m
   (begin
    ; case
    (when (and (car? 'case x)
               (length? 2 x))
     (set! x (list* (car x) (cadr x) (sort (cddr x) value<?))))

    ; functions
    (set! x
          (append* (for/list ((fragment (fragments defun? x)))
                             (if (defun? (car fragment))
                              (sort fragment
                                    (lambda (a b)
                                     (symbol<? (name a) (name b))))
                              fragment))))

    ; memq
    (when (and (car? 'memq x)
               (length? 3 x)
               (car? 'quote (caddr x)))
     (set! x `(,(car x) ,(cadr x) ',(sort (cadr (caddr x)) value<?))))

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

 ; blank line after import
 (set! m
       (map-lists x m
        (transform zs x
         (values (if (and (car? 'import (car zs))
                          (pair? (cdr zs))
                          (not (car? 'import (cadr zs))))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line after use
 (set! m
       (map-lists x m
        (transform zs x
         (values (if (and (car? 'use (car zs))
                          (pair? (cdr zs))
                          (not (car? 'use (cadr zs))))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line before include
 (set! m
       (map-lists x m
        (transform zs x
         (values (if (and (not (car? 'include (car zs)))
                          (not (car? comment-symbol (car zs)))
                          (pair? (cdr zs))
                          (car? 'include (cadr zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line after include
 (set! m
       (map-lists x m
        (transform zs x
         (values (if (and (car? 'include (car zs))
                          (pair? (cdr zs))
                          (not (car? 'include (cadr zs))))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line before comment
 (set! m
       (map-lists x m
        (transform zs x
         (values (if (and (not (car? comment-symbol (car zs)))
                          (pair? (cdr zs))
                          (car? comment-symbol (cadr zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line after function
 (set! m
       (map-lists x m
        (transform zs x
         (values (if (and (defun? (car zs))
                          (not (cadr? blank-symbol zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; remove multiple blanks
 (set! m
       (map-lists x m
        (transform zs x
         (values (list (car zs))
                 (if (car? blank-symbol zs)
                  (dropf zs (curry eq? blank-symbol))
                  (cdr zs))))))

 ; remove trailing blanks
 (set! m
       (map-lists x m
        (transform zs x
         (values (if (and (car? blank-symbol zs)
                          (null? (cdr zs)))
                  '()
                  (list (car zs)))
                 (cdr zs)))))

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
