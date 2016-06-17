#lang racket
(require (planet dyoo/while-loop:1:=1)
         "etc.rkt"
         "read.rkt"
         memoize)

(provide write-module)

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
               (if (atom? v)
                (~s v)
                (list "("
                      (inline (car v))
                      " "
                      (expr (+ col 1 (width (car v)) 1) (cadr v))
                      ")")))
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
   (list "(" (expr col1 a) "\n" (make-string col1 #\space) (exprs col1 b) ")"))))

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
 (define col*
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
    (< (+ col* (width w)) 80))
   (list op2 (exprs col* b) ")"))
  ((list 'begin b ...)
   (list "(" op "\n" (make-string col1 #\space) (exprs col1 b) ")"))
  ((list (or 'case
             'match)
         a
         b ...)
   (list op2 (expr col* a) "\n" (make-string col1 #\space) (clauses col1 b) ")"))
  ((list 'cond b ...)
   (list "(" op "\n" (make-string col1 #\space) (clauses col1 b) ")"))
  ((list (or 'define
             'define/memo
             'define/memo*)
         (list a ...)
         b ...)
   (list op2 (inline a) "\n" (make-string col1 #\space) (exprs col1 b) ")"))
  ((list 'define-syntax a b ...)
   (list op2 (inline a) "\n" (make-string col1 #\space) (exprs col1 b) ")"))
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
         (bindings (+ col* 1) a)
         ")\n"
         (make-string col1 #\space)
         (exprs col1 b)
         ")"))
  ((list 'if a b ...)
   (list op2 (expr col* a) "\n" (make-string col1 #\space) (exprs col1 b) ")"))
  ((list (or 'lambda
             'lambda/memo
             'lambda/memo*)
         a
         b ...)
   (list op2 (inline a) "\n" (make-string col1 #\space) (exprs col1 b) ")"))
  ((list 'let (list a ...) b ...)
   (list op2
         "("
         (bindings (+ col* 1) a)
         ")\n"
         (make-string col1 #\space)
         (exprs col1 b)
         ")"))
  ((list 'let id (list a ...) b ...)
   (list op2
         (~a id)
         " ("
         (bindings (+ col* (width id) 2) a)
         ")\n"
         (make-string col1 #\space)
         (exprs col1 b)
         ")"))
  ((list (or 'provide
             'require)
         b ...)
   (list op2 (exprs col* b) ")"))
  ((list 'receive a b ...)
   (list op2 (inline a) "\n" (make-string col1 #\space) (exprs col1 b) ")"))
  ((list 'syntax-rules a b ...)
   (list op2 (inline a) "\n" (make-string col1 #\space) (clauses col1 b) ")"))
  ((list (or 'unless
             'when
             'while
             'while/list)
         a
         b ...)
   (list op2 (expr col* a) "\n" (make-string col1 #\space) (exprs col1 b) ")"))
  (_
   (cond
    ; args inline
    ((inlines? col v)
     (inline v))

    ; args aligned with first
    ((and (length? 2 v)
          (inline? (car v))
          (for/and ((y (cdr v)))
           (< (+ col 1 (width (car v)) 1 (width y)) 80)))
     (list "("
           (inline (car v))
           " "
           (expr (+ col 1 (width (car v)) 1) (cadr v))
           "\n"
           (make-string (+ col 1 (width (car v)) 1) #\space)
           (exprs (+ col 1 (width (car v)) 1) (cddr v))
           ")"))

    ; first arg inline anyway
    ((and (length? 2 v)
          (memq (car v) '(define set!)))
     (list "("
           (inline (car v))
           " "
           (inline (cadr v))
           "\n"
           (make-string col1 #\space)
           (exprs col1 (cddr v))
           ")"))

    ; args unaligned
    (else
     (list "("
           (expr col1 (car v))
           "\n"
           (make-string col1 #\space)
           (exprs col1 (cdr v))
           ")"))))))

(define (exprs col lst)
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
 (define lst (flatten v))

 ; todo - member check necessary?
 (and (not (member comment-symbol lst))
      (not (string-contains? (string-append* (flatten (expr 0 v))) "\n"))))

(define (inlines lst)
 (add-between (map inline lst) " "))

(define (inlines? col lst)
 (and (andmap inline? lst)
      (< (+ col (length lst) (apply + (map width lst))) 80)))

(define (max-line-length s)
 (if (string=? s "")
  0
  (apply max (map string-length (string-split s "\n")))))

(define/memo* (width v)
 (max-line-length (string-append* (flatten (expr 0 v)))))

(define (write-module m)
 (define lines (string-split (string-append* (flatten (exprs 0 m))) "\n"))
 (for ((s lines))
  (displayln (string-trim s #:left? #f))))

(define blank-symbol (gensym))
