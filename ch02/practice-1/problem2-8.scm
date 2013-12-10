;; 하한과 상한을 뺄샘으로 구하는 문제이다
;; 하한과 상한을 구할 때는 
;; 하한 : 가장 작은 값에서 가장 큰 값을 뺀다
;; 상한 : 가장 큰 값에서 가장 작은 값을 뺀다
;; 라는 생각에서 출발하여 구하게 되면 가장 작은 값과
;; 가장 큰 값을 찾을 수 있다.

(define (sub-interval x y)
 (make-interval (- (lower-interval x) (upper-interval y))
                (- (upper-interval x) (lower-interval y))))

(define (make-interval a b) (cons a b))
(define (lower-interval i) (car i))
(define (upper-interval i) (cdr i))


