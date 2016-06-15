#lang racket
(require (planet dyoo/while-loop)
         "etc.rkt")

(provide comment-symbol
         lang-symbol
         quasiquote-symbol
         quasisyntax-symbol
         quote-symbol
         read-module
         syntax-symbol
         unquote-splicing-symbol
         unquote-symbol
         unsyntax-splicing-symbol
         unsyntax-symbol)

(define (identifier)
 (list->string (while/list (subsequent? (peek-char)) (read-char))))

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
     `(,lang-symbol ,(read-line)))
    ((peek? "'" 1)
     (read-char)
     (read-char)
     (list syntax-symbol (read*)))
    ((peek? "`" 1)
     (read-char)
     (read-char)
     (list quasisyntax-symbol (read*)))
    ((peek? "," 1)
     (read-char)
     (read-char)
     (if (peek? "@")
      (begin
       (read-char)
       (list unsyntax-splicing-symbol (read*)))
      (list unsyntax-symbol (read*))))
    ((peek? ":" 1)
     (read))
    ((peek? "\\" 1)
     (read))
    (else
     (let ((s (identifier)))
      (case s
       (("#")
        (list->vector (read)))
       (("#F" "#f")
        #f)
       (("#T" "#t")
        #t)
       (else
        (string->symbol s)))))))
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
   (list comment-symbol (read-line)))
  ((peek? "`")
   (read-char)
   (list quasiquote-symbol (read*)))
  (else
   (read))))

(define (read-module)
 (let loop ()
  (define x (read*))
  (if (eof-object? x)
   '()
   (cons x (loop)))))

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
  ((_ c b ...)
   (let loop ()
    (if c
     (cons (let ()
            b
            ...)
           (loop))
     '())))))

(define (whitespace)
 (when (and (not (eof-object? (peek-char)))
            (char-whitespace? (peek-char)))
  (read-char)
  (whitespace)))

(define comment-symbol (gensym))
(define lang-symbol (gensym))
(define quasiquote-symbol (gensym))
(define quasisyntax-symbol (gensym))
(define quote-symbol (gensym))
(define syntax-symbol (gensym))
(define unquote-splicing-symbol (gensym))
(define unquote-symbol (gensym))
(define unsyntax-splicing-symbol (gensym))
(define unsyntax-symbol (gensym))
