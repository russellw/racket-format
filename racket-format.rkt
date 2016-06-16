#lang racket
(require "read.rkt"
         "sort.rkt"
         "write.rkt")

(define inplace (make-parameter #f))
(define show-version (make-parameter #f))
(define files
 (command-line #:program
               "racket-format"
               #:once-each (("-i") "Inplace edit" (inplace #t))
               #:once-each (("-v" "--version") "Show version"
                                               (show-version #t))
               #:args files
               files))
(when (show-version)
 (displayln "racket-format version 0")
 (exit 0))
(for ((path files))
 (define m (with-input-from-file path read-module))
 (set! m (sort-module m))
 (if (inplace)
  (with-output-to-file path
                       (lambda ()
                        (write-module m))
                       #:exists 'replace)
  (write-module m)))
