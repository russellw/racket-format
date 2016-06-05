#lang racket
(require "etc.rkt")
(require "read.rkt")
(provide tidy)
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

(define (tidy x)
 ; space at start of comment
 (set! x
  (map-rec y x
   (if (and (car? comment-symbol y)
            (length? 2 (cadr y))
            (char-alphabetic? (string-ref (cadr y) 1)))
    (list comment-symbol
          (string-append "; " (substring (cadr y) 1 (string-length (cadr y)))))
    y)))

 ; sort cases
 (set! x
       (map-rec y x
        (if (and (car? 'case y)
                 (length? 2 y))
         (list* (car y) (cadr y) (sort (cddr y) value<?))
         y)))

 ; sort functions
 (set! x
       (map-rec y x
        (if (list? y)
         (append* (for/list ((fragment (fragments defun? y)))
                            (if (defun? (car fragment))
                             (sort fragment value<?)
                             fragment)))
         y)))

 ; sort macros
 (set! x
  (map-rec y x
   (if (list? y)
    (append* (for/list ((fragment (fragments (curry car? 'define-syntax) y)))
                       (if (car? 'define-syntax (car fragment))
                        (sort fragment value<?)
                        fragment)))
    y)))

 ; sort memq
 (set! x
       (map-rec y x
        (if (and (car? 'memq y)
                 (length? 3 y)
                 (car? 'quote (caddr y)))
         (list (car y) (cadr y) (list 'quote (sort (cadr (caddr y)) value<?)))
         y)))

 ; sort requires
 (set! x
       (map-rec y x
        (if (list? y)
         (append* (for/list ((fragment (fragments (curry car? 'require) y)))
                            (if (car? 'require (car fragment))
                             (sort fragment value<?)
                             fragment)))
         y)))

 ; blank line after import
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'import (car zs))
                          (pair? (cdr zs))
                          (not (car? 'import (cadr zs))))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line after use
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'use (car zs))
                          (pair? (cdr zs))
                          (not (car? 'use (cadr zs))))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line before include
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (not (car? 'include (car zs)))
                          (not (car? comment-symbol (car zs)))
                          (pair? (cdr zs))
                          (car? 'include (cadr zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line after include
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'include (car zs))
                          (pair? (cdr zs))
                          (not (car? 'include (cadr zs))))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line before comment
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (not (car? comment-symbol (car zs)))
                          (pair? (cdr zs))
                          (car? comment-symbol (cadr zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line after macro
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'define-syntax (car zs))
                          (not (cadr? blank-symbol zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line after record
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'define-record-type (car zs))
                          (not (cadr? blank-symbol zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (not (car? 'define-record-printer (car zs)))
                          (not (car? comment-symbol (car zs)))
                          (pair? (cdr zs))
                          (car? 'define-record-printer (cadr zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'define-record-printer (car zs))
                          (not (cadr? blank-symbol zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'define (car zs))
                          (pair? (cdr zs))
                          (car? 'defstruct (cadr zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'defstruct (car zs))
                          (not (cadr? blank-symbol zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; blank line after function
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (defun? (car zs))
                          (not (cadr? blank-symbol zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; remove multiple blanks
 (set! x
       (map-rec y x
        (transform zs y
         (values (list (car zs))
                 (if (car? blank-symbol zs)
                  (dropf zs (curry eq? blank-symbol))
                  (cdr zs))))))

 ; remove trailing blanks
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? blank-symbol zs)
                          (null? (cdr zs)))
                  '()
                  (list (car zs)))
                 (cdr zs)))))

 ; result
 x)

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
    (list< x y))
   ((string? x)
    (string<? x y))
   ((symbol? x)
    (symbol<? x y)))
  (symbol<? (typeof x) (typeof y))))
