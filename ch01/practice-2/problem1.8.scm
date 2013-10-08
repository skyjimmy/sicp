;; sqrt의 방법으로 세제곱근을 구하는 것을 만들었음
;; 그러나 현재 구하는 식으로는 소수점은 구하지 못할 것으로 보임

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* guess 2)) 3))

(define (guess-enough? before-guess guess x)
  (< (abs (- (improve guess x) before-guess)) 0.001))

(define (three-sqrt-iter before-guess guess x)
  (if (guess-enough? before-guess guess x) guess
	(three-sqrt-iter guess (improve guess x) x)))

(define (three-sqrt x)
  (three-sqrt-iter 1.0 (improve 1.0 x) x))

