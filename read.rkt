#lang racket
(require (planet dyoo/while-loop:1:=1))
 (require "etc.rkt")
 (provide read/comments)
 (provide comment-symbol)
 (provide blank-symbol)
 (define (identifier)
  (list->string (collect
                 (if (subsequent? (peek-char))
                  (list (read-char))
                  #f))))

 (define (initial? c)
  (or (char-alphabetic? c)
      (special-initial? c)))

 (define (peek? s)
  (and (not (eof-object? (peek-char)))
       (char=? (peek-char) (string-ref s 0))))

 (define (read*)
  (whitespace)
  (cond
   ((eof-object? (peek-char))
    (peek-char))
   ((peek? "'")
    (read-char)
    (list 'quote (read*)))
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
        (if (string=? s"." )
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
      (list 'unquote-splicing (read*)))
     (list 'unquote (read*))))
   ((peek? ";")
    (list comment-symbol (read-line)))
   ((peek? "`")
    (read-char)
    (list 'quasiquote (read*)))
   (else
    (read))))

 (define (special-initial? c)
  (string-contains? "!$%&*/:<=>?^_~" (make-string 1 c)))

 (define (special-subsequent? c)
  (string-contains? "+-.@" (make-string 1 c)))

 (define (subsequent? c)
  (or (initial? c)
      (char-numeric? c)
      (special-subsequent? c)))

 (define (whitespace)
  (when (and (not (eof-object? (peek-char)))
             (char-whitespace? (peek-char)))
   (read-char)
   (whitespace)))

(define (read/comments)
 (collect
  (define x (read*))
  (if (eof-object? x)
   #f
   (list x))))
(define blank-symbol (gensym))
(define comment-symbol (gensym))
