#lang racket

(require (planet dyoo/while-loop:1:=1) "etc.rkt" "read.rkt" "tidy.rkt")

(provide format-module)

(define (args lst col)
 (set! lst (blank-before-comments lst))
 (string-append*
  (flatten (list (for/list ((v lst))
                  (list* "\n" (make-string col #\space) (expr v col)))
                 ")"))))

(define (bindings lst col)
 (string-append*
  (flatten
   (list (add-between (for/list ((v lst))
                       (if (atom? v)
                        (~s v)
                        (list "("
                              (inline (car v))
                              " "
                              (expr (cadr v) (+ col 1 (width (car v)) 1))
                              ")")))
                      (list "\n" (make-string col #\space)))
         ")"))))

(define (blank-before-comments lst)
 (add-betweenf lst
               (lambda (v w)
                (cond
                 ((eq? v blank-symbol)
                  #f)
                 ((and (not (car? comment-symbol v))
                       (car? comment-symbol w))
                  blank-symbol)
                 (else
                  #f)))))

(define (clauses lst col)
 (set! lst (blank-before-comments lst))
 (string-append*
  (flatten (list (for/list ((clause lst))
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
                 ")"))))

(define (expr x col)
 (string-append*
  (flatten
   (match x
    ; atom
    ((== blank-symbol)
     '())
    (_
     #:when
     (atom? x)
     (~s x))

    ; text
    ((list (== comment-symbol) s)
     s)
    ((list (== lang-symbol) s)
     s)

    ; prefix
    ((list (== quasiquote-symbol) w)
     (list "`" (expr w (+ col 1))))
    ((list (== quasisyntax-symbol) w)
     (list "#`" (expr w (+ col 2))))
    ((list (== quote-symbol) w)
     (list "'" (expr w (+ col 1))))
    ((list (== syntax-symbol) w)
     (list "#'" (expr w (+ col 2))))
    ((list (== unquote-splicing-symbol) w)
     (list ",@" (expr w (+ col 2))))
    ((list (== unquote-symbol) w)
     (list "," (expr w (+ col 1))))
    ((list (== unsyntax-splicing-symbol) w)
     (list "#,@" (expr w (+ col 3))))
    ((list (== unsyntax-symbol) w)
     (list "#," (expr w (+ col 2))))

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
      ((and
        (length? 2 x)
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
       (list "(" (expr (car x) (add1 col)) (args (cdr x) (add1 col))))))))))

(define (format-module m)
 (trim-lines (string-append* (flatten (map (lambda (x)
                                            (list (expr x 0) "\n"))
                                           m)))))

(define (inline x)
 (string-append* (flatten (cond
                           ((car? quote-symbol x)
                            (list "'" (inline (cadr x))))
                           ((car? quasiquote-symbol x)
                            (list "`" (inline (cadr x))))
                           ((car? quasisyntax-symbol x)
                            (list "#`" (inline (cadr x))))
                           ((car? syntax-symbol x)
                            (list "#'" (inline (cadr x))))
                           ((car? unquote-symbol x)
                            (list "," (inline (cadr x))))
                           ((car? unquote-splicing-symbol x)
                            (list ",@" (inline (cadr x))))
                           ((car? unsyntax-symbol x)
                            (list "#," (inline (cadr x))))
                           ((car? unsyntax-splicing-symbol x)
                            (list "#,@" (inline (cadr x))))
                           ((list? x)
                            (list "(" (add-between (map inline x) " ") ")"))
                           (else
                            (~s x))))))

(define (inline? x)
 (and (not (any-rec? y x
            (eq? y blank-symbol)))
      (not (any-rec? y x
            (eq? y comment-symbol)))
      (not (string-contains? (expr x 0) "\n"))))

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
 (let ((w (max-line-width (expr x 0))))
  (hash-set! widths x w)
  w))

(define widths (make-hash))
