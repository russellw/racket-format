#lang racket
 (require "etc.rkt")
 (require "read.rkt")
 (provide tidy)

(define (tidy x)
 ; Space at start of comment
 (set! x
  (map-rec y x
   (if (and (car? comment-symbol y)
            (length? 2 (cadr y))
            (char-alphabetic? (string-ref (cadr y) 1)))
    (list comment-symbol
          (string-append "; " (substring (cadr y) 1 (string-length (cadr y)))))
    y)))

 ; Sort cases
 (set! x
       (map-rec y x
        (if (and (car? 'case y)
                 (length? 2 y))
         (list* (car y) (cadr y) (sort (cddr y) value<?))
         y)))

 ; Sort functions
 (set! x
       (map-rec y x
        (if (list? y)
         (append* (for ((zs (frag defun? y)))
                       (if (defun? (car zs))
                        (sort zs value<?)
                        zs)))
         y)))

 ; Sort macros
 (set! x
       (map-rec y x
        (if (list? y)
         (append* (for ((zs (frag (curry car? 'define-syntax) y)))
                       (if (car? 'define-syntax (car zs))
                        (sort zs value<?)
                        zs)))
         y)))

 ; Sort memq
 (set! x
       (map-rec y x
        (if (and (car? 'memq y)
                 (length? 3 y)
                 (car? 'quote (caddr y)))
         (list (car y) (cadr y) (list 'quote (sort (cadr (caddr y)) value<?)))
         y)))

 ; Blank line after import
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'import (car zs))
                          (pair? (cdr zs))
                          (not (car? 'import (cadr zs))))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; Blank line after use
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'use (car zs))
                          (pair? (cdr zs))
                          (not (car? 'use (cadr zs))))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; Blank line before include
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

 ; Blank line after include
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'include (car zs))
                          (pair? (cdr zs))
                          (not (car? 'include (cadr zs))))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; Blank line before comment
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (not (car? comment-symbol (car zs)))
                          (pair? (cdr zs))
                          (car? comment-symbol (cadr zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; Blank line after macro
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? 'define-syntax (car zs))
                          (not (cadr? blank-symbol zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; Blank line after record
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

 ; Blank line after function
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (defun? (car zs))
                          (not (cadr? blank-symbol zs)))
                  (list (car zs) blank-symbol)
                  (list (car zs)))
                 (cdr zs)))))

 ; Remove multiple blanks
 (set! x
       (map-rec y x
        (transform zs y
         (values (list (car zs))
                 (if (car? blank-symbol zs)
                  (dropf zs(curry eq? blank-symbol) )
                  (cdr zs))))))

 ; Remove trailing blanks
 (set! x
       (map-rec y x
        (transform zs y
         (values (if (and (car? blank-symbol zs)
                          (null? (cdr zs)))
                  '()
                  (list (car zs)))
                 (cdr zs)))))

 ; Result
 x)
