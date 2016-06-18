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
  (_
   #f)))

(define (bindings col lst)
 (add-between (for/list ((v lst))
               (expr col v))
              (list "\n" (make-string col #\space))))

(define (blank-after-decls lst)
 (let loop ((lst lst))
  (match lst
   ((list a (== blank-symbol) b ...)
    (list* a blank-symbol (loop b)))
   ((list (? decl*? a) b c ...)
    (list* a blank-symbol (loop (cons b c))))
   ((list a b ...)
    (cons a (loop b)))
   (_
    '()))))

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
   (_
    '()))))

(define (clause col v)
 (define col1 (+ col 1))
 (match v
  ; simple form
  ((== blank-symbol)
   '())
  ((? atom? _)
   (~s v))
  ((list (== comment-symbol) s)
   s)
  ((? abbrev-prefix (list _ w))
   (define s (abbrev-prefix v))
   (list s (expr (+ col (string-length s)) w)))

  ; clause
  ((list a b ...)
   (list "("
         (expr col1 a)
         "\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))))

(define (clauses col lst)
 (set! lst (blank-before-comments lst))
 (add-between (map (curry clause col) lst)
              (list "\n" (make-string col #\space))))

(define (decl*? x)
 (match x
  ((list (or 'provide
             'require)
         b ...)
   #t)
  (_
   (decl? x))))

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
   '())
  ((? atom? _)
   (~s v))
  ((list (== comment-symbol) s)
   s)
  ((? abbrev-prefix (list _ w))
   (define s (abbrev-prefix v))
   (list s (expr (+ col (string-length s)) w)))

  ; special form
  ((list (or 'and
             'or)
         b ...)
   #:when
   (for/and ((w b))
    (< (+ col2 (width w)) 80))
   (list op2 (multilines col2 b) ")"))
  ((list 'begin b ...)
   (list "(" op "\n" (make-string col1 #\space) (multilines col1 b) ")"))
  ((list (or 'case
             'match)
         a
         b ...)
   (list op2 (expr col2 a) "\n" (make-string col1 #\space) (clauses col1 b) ")"))
  ((list 'cond b ...)
   (list "(" op "\n" (make-string col1 #\space) (clauses col1 b) ")"))
  ((list (or 'define
             'define/memo
             'define/memo*)
         (list a ...)
         b ...)
   (list op2
         (expr col2 a)
         "\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))
  ((list 'define-syntax a b ...)
   (list op2
         (expr col2 a)
         "\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))
  ((list (or 'for
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
             'for/sum)
         a
         b ...)
   (list op2
         "("
         (bindings (+ col2 1) a)
         ")\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))
  ((list 'if a b ...)
   (list op2
         (expr col2 a)
         "\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))
  ((list (or 'lambda
             'lambda/memo
             'lambda/memo*)
         a
         b ...)
   (list op2
         (expr col2 a)
         "\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))
  ((list 'let (list a ...) b ...)
   (list op2
         "("
         (bindings (+ col2 1) a)
         ")\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))
  ((list 'let id (list a ...) b ...)
   (list op2
         (~a id)
         " ("
         (bindings (+ col2 (width id) 2) a)
         ")\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))
  ((list (or 'provide
             'require)
         b ...)
   (list op2 (multilines col2 b) ")"))
  ((list 'receive a b ...)
   (list op2
         (expr col2 a)
         "\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))
  ((list 'syntax-rules a b ...)
   (list op2 (expr col2 a) "\n" (make-string col1 #\space) (clauses col1 b) ")"))
  ((list (or 'unless
             'when
             'while
             'while/list)
         a
         b ...)
   (list op2
         (expr col2 a)
         "\n"
         (make-string col1 #\space)
         (multilines col1 b)
         ")"))

  ; no args
  ((list f)
   (list "(" (expr col1 f) ")"))

  ; args inline
  (_
   #:when
   (inlines? col v)
   (inline v))

  ; args aligned
  ((list (? symbol? f) a ...)
   #:when
   (for/and ((w a))
    (< (+ col2 (width w)) 80))
   (list op2 (multilines col2 a) ")"))

  ; args unaligned
  ((list f a ...)
   (list "("
         (expr col1 f)
         "\n"
         (make-string col1 #\space)
         (multilines col1 a)
         ")"))))

(define (exprs col lst)
 (if (inlines? col lst)
  (inlines lst)
  (multilines col lst)))

(define (format-module m)
 (trim-lines (string-append* (flatten (multilines 0 m)))))

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
   (list s (inline w)))

  ; compound form
  (_
   (list "(" (inlines v) ")"))))

(define (inline? v)
 (and (not (member comment-symbol (flatten v)))
      (not (string-contains? (string-append* (flatten (expr 0 v))) "\n"))))

(define (inlines lst)
 (add-between (map inline lst) " "))

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
 (add-between (let loop ((lst lst))
               (match lst
                ((list a '... c ...)
                 (cons (list (expr col a) " ...") (loop c)))
                ((list a b ...)
                 (cons (expr col a) (loop b)))
                (_
                 '())))
              (list "\n" (make-string col #\space))))

(define (trim-lines s)
 (define lines (string-split s "\n"))
 (string-join (for/list ((line lines))
               (string-trim line #:left? #f))
              "\n"
              #:after-last
              "\n"))

(define/memo* (width v)
 (max-line-length (string-append* (flatten (expr 0 v)))))

(define blank-symbol (gensym))
