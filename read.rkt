#lang racket
(require (planet dyoo/while-loop)
         "etc.rkt")

(provide block-comment-symbol
         expr-comment-symbol
         line-comment-symbol
         quasiquote-symbol
         quasisyntax-symbol
         quote-symbol
         read-module
         syntax-symbol
         unquote-splicing-symbol
         unquote-symbol
         unsyntax-splicing-symbol
         unsyntax-symbol)

(define (block-comment)
 (list* (read-char)
        (read-char)
        (let loop ()
         (cond
          ((eof-object? (peek-char))
           (error "unterminated block comment"))
          ((equal? (peek-string 2 0) "#|")
           (cons (block-comment) (loop)))
          ((equal? (peek-string 2 0) "|#")
           (list (read-char) (read-char)))
          (else
           (cons (read-char) (loop)))))))

(define (identifier)
 (list->string (while/list (subsequent? (peek-char))
                (read-char))))

(define (initial? c)
 (or (char-alphabetic? c)
     (special-initial? c)))

(define (peek? s (skip 0))
 (define c (peek-char (current-input-port) skip))
 (and (not (eof-object? c))
      (char=? c (string-ref s 0))))

(define (read*)
 (whitespace)
 (cond
  ((eof-object? (peek-char))
   (peek-char))
  ((peek? "#")
   (cond
    ((equal? (peek-string 5 0) "#lang")
     `(,line-comment-symbol
       ,(read-line)))
    ((or (equal? (peek-string 3 0) "#! ")
         (equal? (peek-string 3 0) "#!/"))
     (list line-comment-symbol
           (string-append* (let loop ()
                            (define s (read-line))
                            (if (string-suffix? s "\\")
                             (cons (string-append s "\n") (loop))
                             (list s))))))
    ((peek? "'" 1)
     (read-char)
     (read-char)
     (list syntax-symbol (read*)))
    ((peek? "," 1)
     (read-char)
     (read-char)
     (if (peek? "@")
      (begin
       (read-char)
       (list unsyntax-splicing-symbol (read*)))
      (list unsyntax-symbol (read*))))
    ((peek? ";" 1)
     (read-char)
     (read-char)
     (list expr-comment-symbol (read*)))
    ((peek? "`" 1)
     (read-char)
     (read-char)
     (list quasisyntax-symbol (read*)))
    ((peek? "|" 1)
     (list block-comment-symbol (list->string (flatten (block-comment)))))
    (else
     (read))))
  ((peek? "'")
   (read-char)
   (list quote-symbol (read*)))
  ((peek? "(")
   (read-char)
   (let loop ()
    (whitespace)
    (cond
     ((eof-object? (peek-char))
      (error "unterminated list"))
     ((peek? ")")
      (read-char)
      '())
     ((peek? ".")
      (let ((s (identifier)))
       (if (string=? s ".")
        (let ((x (read)))
         (while (not (peek? ")"))
          (whitespace)
          (cond
           ((eof-object? (peek-char))
            (error "expected ')'"))
           ((peek? ";")
            (read-line))))
         (read-char)
         x)
        (cons (string->symbol s) (loop)))))
     (else
      (cons (read*) (loop))))))
  ((peek? ",")
   (read-char)
   (if (peek? "@")
    (begin
     (read-char)
     (list unquote-splicing-symbol (read*)))
    (list unquote-symbol (read*))))
  ((peek? ";")
   (read-char)
   (list line-comment-symbol
         (string-append (if (and (not (eof-object? (peek-char)))
                                 (char-alphabetic? (peek-char)))
                         "; "
                         ";")
                        (read-line))))
  ((peek? "`")
   (read-char)
   (list quasiquote-symbol (read*)))
  (else
   (read))))

(define (read-module)
 (define v (read*))
 (if (eof-object? v)
  '()
  (cons v (read-module))))

(define (special-initial? c)
 (string-contains? "!#$%&*/:<=>?^_~" (make-string 1 c)))

(define (special-subsequent? c)
 (string-contains? "+-.@" (make-string 1 c)))

(define (subsequent? c)
 (or (initial? c)
     (char-numeric? c)
     (special-subsequent? c)))

(define-syntax while/list
 (syntax-rules ()
  ((_ a b ...)
   (let loop ()
    (if a
     (cons (let ()
            b ...)
           (loop))
     '())))))

(define (whitespace)
 (while (and (not (eof-object? (peek-char)))
             (char-whitespace? (peek-char)))
  (read-char)))

(define block-comment-symbol (gensym))
(define expr-comment-symbol (gensym))
(define line-comment-symbol (gensym))
(define quasiquote-symbol (gensym))
(define quasisyntax-symbol (gensym))
(define quote-symbol (gensym))
(define syntax-symbol (gensym))
(define unquote-splicing-symbol (gensym))
(define unquote-symbol (gensym))
(define unsyntax-splicing-symbol (gensym))
(define unsyntax-symbol (gensym))
