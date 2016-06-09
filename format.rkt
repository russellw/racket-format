#lang racket
(require (planet dyoo/while-loop:1:=1))
(require "etc.rkt")
(require "tidy.rkt")
(require "read.rkt")
(provide format-code)
(define (abbrev-prefix x)
 (and (list? x)
      (= (length x) 2)
      (case (car x)
       ((quasiquote)
        "`")
       ((quasisyntax)
        "#`")
       ((quote)
        "'")
       ((syntax)
        "#'")
       ((unquote)
        ",")
       ((unquote-splicing)
        ",@")
       ((unsyntax)
        "#,")
       ((unsyntax-splicing)
        "#,@")
       (else
        #f))))

(define (args xs col)
	 (when (car? blank-symbol xs)
  	(set! xs (cdr xs)))
  (string-append*
  	(flatten
  		(list
		  	(for/list ((x xs))
		  		(list*
		  			"\n"
		  			(make-string col #\space)
					  (block x col)))
				")"))
				))

(define (bindings xs col)
	 (when (car? blank-symbol xs)
  	(set! xs (cdr xs)))
  (string-append*
  	(flatten
  		(list
		  	(for/list ((x xs))
				  (if (atom? x)
				  	(~s x)
			  		(list
			  			"("
					    (inline (car x))
					    " "
					    (block (cadr x) (+ col 1 (width (car x)) 1))
					    ")")))
				(if(pair?(cdr xs))
					(list
						"\n"
						(make-string col #\space)
)
					'())
				")"))))

(define (block x col)
  (string-append*
  	(flatten
 (cond
  ((eq? blank-symbol x)
   '())
  ((atom? x)
   (inline x))
  ((null? x)
   (inline x))
  ((car? comment-symbol x)
   (cadr x))
  ((and (abbrev-prefix x)
        (list? (cadr x))
        (andmap atom? (cadr x)))
   (inline x))
  ((abbrev-prefix x)
  	(list
   	 (abbrev-prefix x)
	   (block (cadr x) (+ col (string-length (abbrev-prefix x))))))

  ; 0 special args
  ((memq (car x) '(begin collect))
  	(list
   		"("
 	    (~a (car x))
	   (args (cdr x) (add1 col))))
  ((memq (car x) '(cond))
  	(list
   		"("
 	    (~a (car x))
   (clauses (cdr x) (add1 col))))

  ; 1 special arg
  ((and (length? 2 x)
        (memq (car x) '(case match syntax-rules)))
  	(list
   		"("
   (inline (car x))
   " "
   (block (cadr x) (+ col 1 (width (car x)) 1))
   (clauses (cddr x) (add1 col))))
  ((and (length? 2 x)
        (or (defun? x)
            (memq (car x) '(define-syntax lambda receive))))
  	(list
   		"("
 	    (~a (car x))
 	    " "
   (inline (cadr x))
   (args (cddr x) (add1 col))))
  ((and (length? 2 x)
        (memq (car x) '(if unless when while)))
  	(list
   		"("
 	    (~a (car x))
 	    " "
   (block (cadr x) (+ col 1 (width (car x)) 1))
   (args (cddr x) (add1 col))))

  ; 2 special args
  ((and (length? 3 x)
        (memq (car x) '(any-rec? for map-rec transform)))
  	(list
   		"("
 	    (~a (car x))
 	    " "
 	    (~a (cadr x))
 	    " "
   (inline (caddr x))
   (args (cdddr x) (add1 col))))

  ; let
  ((and (length? 3 x)
        (memq (car x) '(let let* letrec ))
        (list? (cadr x)))
  	(list
   		"("
 	    (~a (car x))
 	    " ("
   (bindings (cadr x) (+ col 1 (width (car x)) 2))
   (args (cddr x) (add1 col))))
  ((and (length? 3 x)
        (memq (car x) '(let)))
  	(list
   		"("
 	    (~a (car x))
 	    " "
 	    (~a (cadr x))
 	    " ("
   (bindings (caddr x) (+ col 1 (width (car x)) 1 (width (cadr x)) 2))
   (args (cdddr x) (add1 col))))

  ; args inline
  ((and (not (memq (car x) '(and or)))
        (andmap inline? x)
        (< (+ col 1 (length x) (apply + (map width x))) 80))
   (inline x))

  ; args aligned with first
  ((and (length? 2 x)
        (inline? (car x))
        (andmap (lambda (y)
                 (< (+ col 1 (width (car x)) 1 (width y)) 80))
                (cdr x))
        (cdr x))
  	(list
   		"("
   (inline (car x))
    " "
   (block (cadr x) (+ col 1 (width (car x)) 1))
   (args (cddr x) (+ col 1 (width (car x)) 1))))

  ; first arg inline anyway
  ((and (length? 2 x)
        (memq (car x) '(define set!)))
  	(list
   		"("
   (inline (car x))
    " "
   (inline (cadr x))
   (args (cddr x) (add1 col))))

  ; args unaligned
  (else
  	(list
   		"("
   (block (car x) (add1 col))
   (args (cdr x) (add1 col))))
   ))))

(define (clauses xs col)
 (when (car? blank-symbol xs)
  (set! xs (cdr xs)))
  (string-append*
  	(flatten
  		(list
		  	(for/list ((clause xs))
		  		(list
		  			"\n"
  (cond
   ((eq? blank-symbol clause)
   	'())
   ((car? comment-symbol clause)
   (list
    (make-string col #\space)
    (cadr clause)))
   ((atom? clause)
   (list
    (make-string col #\space)
    (~a clause)))
   (else
   (list
    (make-string col #\space)
     "("
    (block (car clause) (add1 col))
    (args (cdr clause) (add1 col)))))))
  ")"))))

(define (inline x)
  (string-append*
  	(flatten
 (cond
  ((abbrev-prefix x)
   (list
    (abbrev-prefix x)
   (inline (cadr x))))
  ((list? x)
   (list
    "("
    (add-between(map inline x)" ")
    ")"))
  (else
   (~a x))))))

(define (inline? x)
 (and (not (any-rec? y x
            (eq? y blank-symbol)))
      (not (any-rec? y x
            (eq? y comment-symbol)))
      (not (string-contains?   (block x 0)
                             "\n"))))

(define (max-line-width s)
 (if (string=? s "")
  0
  (apply max (map string-length (string-split s "\n")))))

(define (width x)
 (or (hash-ref widths x #f))
 (let ((w (max-line-width   (block x 0))))
  (hash-set! widths x w)
  w))

(define (format-code xs)
(add-between(map(lambda(x)(block x 0))xs)"\n"))

(define widths (make-hash))
