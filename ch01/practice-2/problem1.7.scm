;; (sqrt 4)의 값을 구할 때 흔히 알고 있는 2라는 값이 출력 되는
;; 것이 아니라 2.0000102400262145라는 값이 출력 되고
;; 역시 마찬가지로 (sqrt 90000000000)의 값을 구할 때도
;; 300000.이라는 값으로 출력이 된다.
;; 정상적으로 나오게 하면 되는 것이 아닐까 싶다.
;; 
;; 인터넷에서 찾아보니 작은 값의 경우 (sqrt 0.001)의 값을 구할 수가
;; 없다느 것이다.
;; 아주 큰 값의 경우에는 9로 시작하는 20자리 이상의 값을 입력 할 경우에
;; 값이 구해 지지 않고 멈추는 현상이 발생 한다.
(define (sqrt-iter guess x)
  (if (good-enough? guess x) guess
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

