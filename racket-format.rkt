#lang racket

 ; Include macros first
 (require "etc.rkt")

 ; Include everything else
 (require "read.rkt")
 (require "tidy.rkt")
 (require "write.rkt")

 ; Options
 (define end-options #f)
 (define inplace #f)
 (define files
         (filt s (vector->list  (current-command-line-arguments))
          (or (not (string-prefix? s "-"))
              end-options
              (begin
               (cond
                ((or (string-prefix? s "--h")
                     (string-prefix? s "-h"))
                 (print "Usage: scheme-format [options] files")
                 (print)
                 (print "-h  Show help")
                 (print "-i  Inplace edit")
                 (print "-v  Show version")
                 (exit 0))
                ((or (string-prefix? s "--i")
                     (string-prefix? s "-i"))
                 (set! inplace #t))
                ((or (string-prefix? s "--v")
                     (string-prefix? s "-V")
                     (string-prefix? s "-v"))
                 (print "scheme-format version 1")
                 (exit 0))
                ((string=? s"--" )
                 (set! end-options #t))
                (else
                 (print s ": unknown option")
                 (exit 1)))
               #f))))

 ; Format
 (for ((file files))
  (define xs (with-input-from-file file read/comments))
  (set! xs (tidy xs))
  (define s (with-output-to-string (lambda()( write/comments xs))))
  (if inplace
   (with-output-to-file file
                        (lambda ()
                         (display s))
    #:mode                    'binary)
   (display s)))
