(define-syntax match-next
 (syntax-rules (=>)
  ;; no more clauses, the match failed
  ((match-next v g+s)
   (error 'match "no matching pattern"))

  ;; named failure continuation
  ((match-next v g+s (pat (=> failure) . body) . rest)
   (let ((failure (lambda ()
                   (match-next v g+s . rest))))
    ;; match-one analyzes the pattern for us
    (match-one v pat g+s (match-drop-ids (begin . body)) (failure) ())))

  ;; anonymous failure continuation, give it a dummy name
  ((match-next v g+s (pat . body) . rest)
   (match-next v g+s (pat (=> failure) . body) . rest))))
