#lang racket
(require "format.rkt"
         "read.rkt"
         "sort.rkt")

(define inplace (make-parameter #f))
(define show-version (make-parameter #f))
(define files
 (command-line #:program
               "racket-format"
               #:once-each
               (("-i")
                "Inplace edit"
                (inplace #t))
               (("-v"
                 "--version")
                "Show version"
                (show-version #t))
               #:args
               files
               files))
(when (show-version)
 (displayln "racket-format version 0")
 (exit 0))
(when (null? files)
 (set! files '("-")))
(for ((path files))
 (define m
  (if (string=? path "-")
   (read-module)
   (with-input-from-file path read-module)))
 (set! m (sort-module m))
 (define s (format-module m))
 (if (and (inplace)
          (not (string=? path "-")))
  (display-to-file s path #:exists 'replace)
  (display s)))
