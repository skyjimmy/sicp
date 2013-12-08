(define (square x)
  (* x x))

;; 1.41
(define (inc i)
  (+ i 1))

(define (double f)
  (lambda (x) (f (f x))))

;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; 1.43 - ing
(define (repeated f n)
  ((compose square square) n))
