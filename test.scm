(define-syntax match-next
 (syntax-rules (=>)
  (x
   (let ((failure (lambda ()
                   (match-next v g+s . rest))))))))
