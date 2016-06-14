#lang racket
(require (planet dyoo/while-loop:1:=1) "etc.rkt" "read.rkt")

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

(define (args lst col)
 (set! lst (blank-after-decls lst))
 (set! lst (blank-before-comments lst))
 (list (for/list ((v lst))
        (list* "\n" (make-string col #\space) (expr v col)))
       ")"))

(define (bindings lst col)
 (list (add-between (for/list ((v lst))
                     (if (atom? v)
                      (~s v)
                      (list "("
                            (inline (car v))
                            " "
                            (expr (cadr v) (+ col 1 (width (car v)) 1))
                            ")")))
                    (list "\n" (make-string col #\space)))
       ")"))

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
 (set! lst (blank-before-comments lst))
 (list (for/list ((clause lst))
        (list "\n"
              (cond
               ((eq? blank-symbol clause)
                '())
               ((car? comment-symbol clause)
                (list (make-string col #\space) (cadr clause)))
               ((atom? clause)
                (list (make-string col #\space) (~s clause)))
               (else
                (list (make-string col #\space)
                      "("
                      (expr (car clause) (add1 col))
                      (args (cdr clause) (add1 col)))))))
       ")"))

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

(define (expr x col)
 (match x
  ; atom
  ((== blank-symbol)
   '())
  ((? atom? _)
   (~s x))

  ; text
  ((list (== comment-symbol) s)
   s)
  ((list (== lang-symbol) s)
   s)

  ; abbrev prefix
  ((? abbrev-prefix (list _ w))
   (define s (abbrev-prefix x))
   (list s (expr w (+ col (string-length s)))))

  ; special form
  ((list 'begin b ...)
   (list "(begin" (args b (+ col 1))))
  ((list 'case a b ...)
   (list "(case " (expr a (+ col 6)) (clauses b (+ col 1))))
  ((list 'cond b ...)
   (list "(cond" (clauses b (+ col 1))))
  ((list 'define (list a ...) b ...)
   (list "(define " (inline a) (args b (+ col 1))))
  ((list 'define-syntax a b ...)
   (list "(define-syntax " (inline a) (args b (+ col 1))))
  ((list 'for a b ...)
   (list "(for (" (bindings a (+ col 6)) (args b (+ col 1))))
  ((list 'for/list a b ...)
   (list "(for/list (" (bindings a (+ col 11)) (args b (+ col 1))))
  ((list 'for/sublists a b ...)
   (list "(for/sublists (" (bindings a (+ col 15)) (args b (+ col 1))))
  ((list 'if a b ...)
   (list "(if " (expr a (+ col 4)) (args b (+ col 1))))
  ((list 'lambda a b ...)
   (list "(lambda " (inline a) (args b (+ col 1))))
  ((list 'let (list a ...) b ...)
   (list "(let (" (bindings a (+ col 6)) (args b (+ col 1))))
  ((list 'let id (list a ...) b ...)
   (list "(let "
         (~a id)
         " ("
         (bindings a (+ col 5 (width id) 2))
         (args b (+ col 1))))
  ((list 'match a b ...)
   (list "(match " (expr a (+ col 7)) (clauses b (+ col 1))))
  ((list 'receive a b ...)
   (list "(receive " (inline a) (args b (+ col 1))))
  ((list 'syntax-rules a b ...)
   (list "(syntax-rules " (inline a) (clauses b (+ col 1))))
  ((list 'unless a b ...)
   (list "(unless " (expr a (+ col 8)) (args b (+ col 1))))
  ((list 'when a b ...)
   (list "(when " (expr a (+ col 6)) (args b (+ col 1))))
  ((list 'while a b ...)
   (list "(while " (expr a (+ col 7)) (args b (+ col 1))))
  (_
   (cond
    ; 2 special args
    ((and (length? 3 x)
          (memq (car x) '(any-rec?)))
     (list "("
           (~a (car x))
           " "
           (~a (cadr x))
           " "
           (inline (caddr x))
           (args (cdddr x) (add1 col))))

    ; args inline
    ((and (not (memq (car x) '(and or)))
          (andmap inline? x)
          (< (+ col 1 (length x) (apply + (map width x))) 80))
     (inline x))

    ; args aligned with first
    ((and (length? 2 x)
          (inline? (car x))
          (for/and ((y (cdr x))) (< (+ col 1 (width (car x)) 1 (width y)) 80)))
     (list "("
           (inline (car x))
           " "
           (expr (cadr x) (+ col 1 (width (car x)) 1))
           (args (cddr x) (+ col 1 (width (car x)) 1))))

    ; first arg inline anyway
    ((and (length? 2 x)
          (memq (car x) '(define set!)))
     (list "("
           (inline (car x))
           " "
           (inline (cadr x))
           (args (cddr x) (add1 col))))

    ; args unaligned
    (else
     (list "(" (expr (car x) (add1 col)) (args (cdr x) (add1 col))))))))

(define (format-module m)
 (trim-lines (string-append* (flatten (drop-right (args m 0) 1)))))

(define (inline x)
 (cond
  ((car? quasiquote-symbol x)
   (list "`" (inline (cadr x))))
  ((car? quasisyntax-symbol x)
   (list "#`" (inline (cadr x))))
  ((car? quote-symbol x)
   (list "'" (inline (cadr x))))
  ((car? syntax-symbol x)
   (list "#'" (inline (cadr x))))
  ((car? unquote-splicing-symbol x)
   (list ",@" (inline (cadr x))))
  ((car? unquote-symbol x)
   (list "," (inline (cadr x))))
  ((car? unsyntax-splicing-symbol x)
   (list "#,@" (inline (cadr x))))
  ((car? unsyntax-symbol x)
   (list "#," (inline (cadr x))))
  ((list? x)
   (list "(" (add-between (map inline x) " ") ")"))
  (else
   (~s x))))

(define (inline? x)
 (and (not (any-rec? y x
            (eq? y blank-symbol)))
      (not (any-rec? y x
            (eq? y comment-symbol)))
      (not (string-contains? (string-append* (flatten (expr x 0))) "\n"))))

(define (max-line-width s)
 (if (string=? s "")
  0
  (apply max (map string-length (string-split s "\n")))))

(define (trim-lines s)
 (string-join (for/list ((s (string-split s "\n")))
               (string-trim s #:left? #f))
              "\n"
              #:after-last
              "\n"))

(define (width x)
 (or (hash-ref widths x #f))
 (let ((w (max-line-width (string-append* (flatten (expr x 0))))))
  (hash-set! widths x w)
  w))

(define blank-symbol (gensym))
(define widths (make-hash))
