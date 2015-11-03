;; sqrt의 방법으로 세제곱근을 구하는 것을 만들었음
;; 현재 8, 27에 대해서는 세제곱근이 구해 짐

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* guess 2)) 3))

(define (guess-enough? before-guess guess x)
  (< (abs (- (improve guess x) before-guess)) 0.001))

(define (three-sqrt-iter before-guess guess x)
  (if (guess-enough? before-guess guess x) guess
	(three-sqrt-iter guess (improve guess x) x)))

(define (three-sqrt x)
  (three-sqrt-iter 1.0 (improve 1.0 x) x))

(three-sqrt 8)
(three-sqrt 27)
