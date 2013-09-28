(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
		(else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x) guess
		  (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; new-if 사용시 sqrt-iter가 함수의 인자로 받아 들여지므로
;; 인자 먼저 계산법으로 인하여 sqrt-iter가 무한 반복이 되는 현상이다.
;; 즉, if 사용시에는 인저 먼저 계산법으로 받아들여 지지 않는다는
;; 얘기 인 것 같다.
;;
