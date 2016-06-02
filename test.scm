(define-syntax match-next
 (syntax-rules (=>)
  ;; named failure continuation
  ((match-next v g+s (pat (=> failure) . body) . rest)
   (let ((failure (lambda ()
                   (match-next v g+s . rest))))))))
