(define (last-pair l)
 (let ((last (length l)))
  (list (list-ref l (- last 1)))))
