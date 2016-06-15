#lang racket
(require (planet dyoo/while-loop:1:=1)
         "etc.rkt"
         "read.rkt")

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

(define (bindings lst col)
 (add-between (for/list ((v lst))
               (if (atom? v)
                (~s v)
                (list "("
                      (inline (car v))
                      " "
                      (expr (cadr v) (+ col 1 (width (car v)) 1))
                      ")")))
              (list "\n" (make-string col #\space))))

(define (blank-after-decls lst)
 (add-betweenf lst
               (lambda (v w)
                (and (decl? v)
                     (not (eq? w blank-symbol))
                     blank-symbol))))

(define (blank-before-comments lst)
 (add-betweenf lst
               (lambda (v w)
                (and (not (car? comment-symbol v))
                     (not (eq? v blank-symbol))
                     (car? comment-symbol w)
                     blank-symbol))))

(define (clauses lst col)
 (define col1 (+ col 1))
 (set! lst (blank-before-comments lst))
 (add-between (for/list ((clause lst))
               (cond
                ((eq? blank-symbol clause)
                 '())
                ((car? comment-symbol clause)
                 (cadr clause))
                ((atom? clause)
                 (~s clause))
                (else
                 (list "("
                       (expr (car clause) col1)
                       "\n"
                       (make-string col1 #\space)
                       (exprs (cdr clause) col1)
                       ")"))))
              (list "\n" (make-string col #\space))))

(define (decl? x)
 (match x
  ((list 'define (list w ...) b ...)
   #t)
  ((list 'define-syntax b ...)
   #t)
  ((list 'provide b ...)
   #t)
  ((list 'require b ...)
   #t)
  (_
   #f)))

(define (expr v col)
 (define col1 (+ col 1))
 (define col*
         (when (pair? v)
          (+ col 2 (width (car v)))))
 (match v
  ; atom
  ((== blank-symbol)
   '())
  ((? atom? _)
   (~s v))

  ; text
  ((list (== comment-symbol) s)
   s)
  ((list (== lang-symbol) s)
   s)

  ; abbrev prefix
  ((? abbrev-prefix (list _ w))
   (define s (abbrev-prefix v))
   (list s (expr w (+ col (string-length s)))))

  ; special form
  ((list 'and b ...)
   #:when (for/and ((w b)) (< (+ col* (width w)) 80))
   (list "(and " (exprs b col*) ")"))
  ((list 'begin b ...)
   (list "(begin\n" (make-string col1 #\space) (exprs b col1) ")"))
  ((list 'case a b ...)
   (list "(case "
         (expr a col*)
         "\n"
         (make-string col1 #\space)
         (clauses b col1)
         ")"))
  ((list 'cond b ...)
   (list "(cond\n" (make-string col1 #\space) (clauses b col1) ")"))
  ((list 'define (list a ...) b ...)
   (list "(define "
         (inline a)
         "\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'define-syntax a b ...)
   (list "(define-syntax "
         (inline a)
         "\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'for a b ...)
   (list "(for ("
         (bindings a (+ col* 1))
         ")\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'for/list a b ...)
   (list "(for/list ("
         (bindings a (+ col* 1))
         ")\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'for/sublists a b ...)
   (list "(for/sublists ("
         (bindings a (+ col* 1))
         ")\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'if a b ...)
   (list "(if "
         (expr a col*)
         "\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'lambda a b ...)
   (list "(lambda "
         (inline a)
         "\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'let (list a ...) b ...)
   (list "(let ("
         (bindings a (+ col* 1))
         ")\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'let id (list a ...) b ...)
   (list "(let "
         (~a id)
         " ("
         (bindings a (+ col* (width id) 2))
         ")\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'match a b ...)
   (list "(match "
         (expr a col*)
         "\n"
         (make-string col1 #\space)
         (clauses b col1)
         ")"))
  ((list 'or b ...)
   #:when (for/and ((w b)) (< (+ col* (width w)) 80))
   (list "(or " (exprs b col*) ")"))
  ((list 'provide b ...)
   (list "(provide " (exprs b col*) ")"))
  ((list 'receive a b ...)
   (list "(receive "
         (inline a)
         "\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'require b ...)
   (list "(require " (exprs b col*) ")"))
  ((list 'syntax-rules a b ...)
   (list "(syntax-rules "
         (inline a)
         "\n"
         (make-string col1 #\space)
         (clauses b col1)
         ")"))
  ((list 'unless a b ...)
   (list "(unless "
         (expr a col*)
         "\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'when a b ...)
   (list "(when "
         (expr a col*)
         "\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  ((list 'while a b ...)
   (list "(while "
         (expr a col*)
         "\n"
         (make-string col1 #\space)
         (exprs b col1)
         ")"))
  (_
   (cond
    ; 2 special args
    ((and (length? 3 v)
          (memq (car v) '(any-rec?)))
     (list "("
           (~a (car v))
           " "
           (~a (cadr v))
           " "
           (inline (caddr v))
           "\n"
           (make-string col1 #\space)
           (exprs (cdddr v) col1)
           ")"))

    ; args inline
    ((and (not (memq (car v) '(and or)))
          (andmap inline? v)
          (< (+ col 1 (length v) (apply + (map width v))) 80))
     (inline v))

    ; args aligned with first
    ((and (length? 2 v)
          (inline? (car v))
          (for/and ((y (cdr v))) (< (+ col 1 (width (car v)) 1 (width y)) 80)))
     (list "("
           (inline (car v))
           " "
           (expr (cadr v) (+ col 1 (width (car v)) 1))
           "\n"
           (make-string (+ col 1 (width (car v)) 1) #\space)
           (exprs (cddr v) (+ col 1 (width (car v)) 1))
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
           (exprs (cddr v) col1)
           ")"))

    ; args unaligned
    (else
     (list "("
           (expr (car v) col1)
           "\n"
           (make-string col1 #\space)
           (exprs (cdr v) col1)
           ")"))))))

(define (exprs lst col)
 (set! lst (blank-after-decls lst))
 (set! lst (blank-before-comments lst))
 (add-between (let loop ((lst lst))
               (match lst
                ((list a '... c ...)
                 (cons (list (expr a col) " ...") (loop c)))
                ((list a b c ...)
                 #:when (keyword? a)
                 (cons (list (expr a col) " " (expr b (+ col (width a) 1)))
                       (loop (cddr lst))))
                ((list a b ...)
                 (cons (expr a col) (loop b)))
                (_
                 '())))
              (list "\n" (make-string col #\space))))

(define (format-module m)
 (trim-lines (string-append* (flatten (exprs m 0)))))

(define (inline v)
 (match v
  ; atom
  ((? atom? _)
   (~s v))

  ; abbrev prefix
  ((? abbrev-prefix (list _ w))
   (define s (abbrev-prefix v))
   (list s (inline w)))

  ; else
  (_
   (list "(" (add-between (map inline v) " ") ")"))))

(define (inline? v)
 (and (not (any-rec? y v
            (eq? y blank-symbol))
           )
      (not (any-rec? y v
            (eq? y comment-symbol))
           )
      (not (string-contains? (string-append* (flatten (expr v 0))) "\n"))))

(define (max-line-width s)
 (if (string=? s "")
  0
  (apply max (map string-length (string-split s "\n")))))

(define (trim-lines s)
 (string-join (for/list ((s (string-split s "\n")))
               (string-trim s #:left? #f))
              "\n"
              #:after-last "\n"))

(define (width v)
 (or (hash-ref widths v #f))
 (let ((w (max-line-width (string-append* (flatten (expr v 0))))))
  (hash-set! widths v w)
  w))

(define blank-symbol (gensym))
(define widths (make-hash))
