#lang racket
(require (planet dyoo/while-loop:1:=1)
         "etc.rkt"
         "read.rkt"
         memoize)

(provide format-module)

(define (abbrev-prefix v)
 (match v
  ((list (== quasiquote-symbol) _)
   "`")
  ((list (== quasisyntax-symbol) _)
   "#`")
  ((list (== quote-symbol) _)
   "'")
  ((list (== syntax-symbol) _)
   "#'")
  ((list (== unquote-splicing-symbol) _)
   ",@")
  ((list (== unquote-symbol) _)
   ",")
  ((list (== unsyntax-splicing-symbol) _)
   "#,@")
  ((list (== unsyntax-symbol) _)
   "#,")
  (_ #f)))

(define (blank-after-decls lst)
 (let loop ((lst lst))
  (match lst
   ((list a (== blank-symbol) b ...)
    (list* a blank-symbol (loop b)))
   ((list (? decl*? a) b c ...)
    (list* a blank-symbol (loop (cons b c))))
   ((list a b ...)
    (cons a (loop b)))
   (_ '()))))

(define (blank-before-comments lst)
 (let loop ((lst lst))
  (match lst
   ((list (== blank-symbol) a ...)
    (cons blank-symbol (loop a)))
   ((list (list (== comment-symbol) a) b ...)
    (list* (list comment-symbol a) (loop b)))
   ((list a (list (== comment-symbol) b) c ...)
    (list* a blank-symbol (list comment-symbol b) (loop c)))
   ((list a b ...)
    (cons a (loop b)))
   (_ '()))))

(define (decl*? x)
 (match x
  ((list (or 'provide
             'require)
         b ...)
   #t)
  (_ (decl? x))))

(define (expr col v)
 (define col1 (+ col 1))
 (define col2
         (when (pair? v)
          (+ col 2 (width (car v)))))
 (define op
         (when (pair? v)
          (~a (car v))))
 (define op2
         (when (pair? v)
          (format "(~a " (car v))))
 (match v
  ; simple form
  ((== blank-symbol)
   "")
  ((? atom? _)
   (~s v))
  ((list (== comment-symbol) s)
   s)
  ((? abbrev-prefix (list _ w))
   (define s (abbrev-prefix v))
   (string-append s (expr (+ col (string-length s)) w)))

  ; special form
  ((list (or 'begin
             'cond)
         b ...)
   (string-append "("
                  op
                  "\n"
                  (make-string col1 #\space)
                  (multilines col1 b)
                  ")"))
  ((list (or 'case
             'define-syntax
             'for
             'for*
             'for*/and
             'for*/first
             'for*/hash
             'for*/hasheq
             'for*/hasheqv
             'for*/last
             'for*/list
             'for*/or
             'for*/product
             'for*/sum
             'for/and
             'for/first
             'for/hash
             'for/hasheq
             'for/hasheqv
             'for/last
             'for/list
             'for/or
             'for/product
             'for/sublists
             'for/sum
             'if
             'lambda
             'lambda/memo
             'lambda/memo*
             'match
             'receive
             'syntax-rules
             'unless
             'when
             'while
             'while/list)
         a
         b ...)
   (string-append op2
                  (expr col2 a)
                  "\n"
                  (make-string col1 #\space)
                  (multilines col1 b)
                  ")"))
  ((list (or 'define
             'define/memo
             'define/memo*
             'let)
         (list a ...)
         b ...)
   (string-append op2
                  (expr col2 a)
                  "\n"
                  (make-string col1 #\space)
                  (multilines col1 b)
                  ")"))
  ((list 'let id (list a ...) b ...)
   (string-append op2
                  (~a id)
                  " ("
                  (multilines (+ col2 (width id) 2) a)
                  ")\n"
                  (make-string col1 #\space)
                  (multilines col1 b)
                  ")"))

  ; no args
  ((list f)
   (string-append "(" (expr col1 f) ")"))

  ; args inline
  ((list (not (or 'and
                  'or
                  'provide
                  'require))
         a ...)
   #:when
   (and (symbol? (car v))
        (inlines? col v))
   (inline v))

  ; args aligned
  ((list (? symbol? f) a ...)
   #:when
   (for/and ((w a))
    (< (+ col2 (width w)) 80))
   (string-append op2 (multilines col2 a) ")"))

  ; args unaligned
  ((list f a ...)
   (string-append "("
                  (expr col1 f)
                  "\n"
                  (make-string col1 #\space)
                  (multilines col1 a)
                  ")"))))

(define (format-module m)
 (trim-lines (multilines 0 m)))

(define (inline v)
 (match v
  ; simple form
  ((== blank-symbol)
   (error "blank inline"))
  ((? atom? _)
   (~s v))
  ((list (== comment-symbol) _)
   (error "line comment inline"))
  ((? abbrev-prefix (list _ w))
   (define s (abbrev-prefix v))
   (string-append s (inline w)))

  ; compound form
  (_ (string-append "(" (inlines v) ")"))))

(define (inline? v)
 (and (not (member comment-symbol (flatten v)))
      (not (string-contains? (expr 0 v) "\n"))))

(define (inlines lst)
 (string-join (map inline lst) " "))

(define (inlines? col lst)
 (and (andmap inline? lst)
      (< (+ col (length lst) (apply + (map width lst))) 80)))

(define (max-line-length s)
 (define lines (string-split s "\n"))
 (if (null? lines)
  0
  (apply max (map string-length lines))))

(define (multilines col lst)
 (set! lst (blank-after-decls lst))
 (set! lst (blank-before-comments lst))
 (string-join (let loop ((lst lst))
               (match lst
                ((list a '... c ...)
                 (cons (string-append (expr col a) " ...") (loop c)))
                ((list a b ...)
                 (cons (expr col a) (loop b)))
                (_ '())))
              (string-append "\n" (make-string col #\space))))

(define (trim-lines s)
 (define lines (string-split s "\n"))
 (set! lines
       (for/list ((line lines))
        (string-trim line #:left? #f)))
 (string-join lines "\n" #:after-last "\n"))

(define/memo* (width v)
 (max-line-length (expr 0 v)))

(define blank-symbol (gensym))
