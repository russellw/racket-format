#lang racket
(require "etc.rkt")
(require "read.rkt")
(require "tidy.rkt")
(require "write.rkt")

; options
(define end-options #f)
(define inplace #f)
(define files
        (filter (lambda (s)
                 (cond
                  ((or (not (string-prefix? s "-"))
                       end-options)
                   #t)
                  ((or (string-prefix? s "--h")
                       (string-prefix? s "-h"))
                   (displayln "Usage: racket-format [options] files")
                   (newline)
                   (displayln "-h  Show help")
                   (displayln "-i  Inplace edit")
                   (displayln "-v  Show version")
                   (exit 0))
                  ((or (string-prefix? s "--i")
                       (string-prefix? s "-i"))
                   (set! inplace #t)
                   #f)
                  ((or (string-prefix? s "--v")
                       (string-prefix? s "-V")
                       (string-prefix? s "-v"))
                   (displayln "racket-format version 1")
                   (exit 0))
                  ((string=? s "--")
                   (set! end-options #t)
                   #f)
                  (else
                   (eprintf "~a: unknown option\n" s)
                   (exit 1))))
                (vector->list (current-command-line-arguments))))

; format
(for ((file files)) (define xs (with-input-from-file file read/comments))
 (set! xs (tidy xs))
 (define s
         (with-output-to-string (lambda ()
                                 (write/comments xs))))
 (if inplace
  (with-output-to-file file
                       (lambda ()
                        (display s))
                       #:exists
                       'replace)
  (display s)))
