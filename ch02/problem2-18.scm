(define (reverse l)
 (define (reverse-iter result leng lst)
  (if (<= leng 0) result
   (reverse-iter (cons (car lst) result) (- (length lst) 1) (cdr lst))))
 (reverse-iter (list) (- (length l) 1) l))

