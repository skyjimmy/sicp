(define (expt b n)
 (expt-iter b n 1))

(define (expt-iter b count product)
 (if (= count 0) product
     (expt-iter b (- count 1) (* b product))))
 
