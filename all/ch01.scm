;; 1.1.1
;; 486

;; (+ 137 349)
;; 486

;; (- 1000 334)
;; 666

;; (* 5 99)
;; 495

;; (/ 10 5)
;; 2

;; (+ 2.7 10)
;; 12.7

;; (+ 21 35 12 7)
;; 75

;; (* 25 4 12)
;; 1200

; (+ (* 3 5) (- 10 6))
;; 19

;; 1.1.2
; (define size 2)

; (* 5 size)
;; 10

; (define pi 3.14159)

; (define radius 10)

; (* pi (* radius radius)) 
; 314.159

; (define circumference (* 2 pi radius))

; circumference ;; 62.8318

;; 1.1.3
;(* (+ 2 (* 4 6))
;   (+ 3 5 7))

;; 1.1.4
(define (square x) (* x x))

;(square 21)
;; 441

;(square (+ 2 5))
;; 49

; (square (square 3))
;; 81

(define (sum-of-squares x y)
  (+ (square x) (square y)))

; (sum-of-squares 3 4)
;; 25

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

; (f 5)
;; 136

;; 1.1.6
(define (abs x)
  (cond ((> x 0) x)
		((= x 0) 0)
		((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
		(else x)))

(define (abs x)
  (if (< x 0) (- x)
	x))

; problem 1.1
10
;; 10

(+ 5 3 4)
;; 12

(- 9 1)
;; 8

(/ 6 2)
;; 3

(+ (* 2 4) (- 4 6))
;; 6

(define a 3)

(define b (+ a 1))
;; 4

(+ a b (* a b))
;; 19

(= a b)
;; #f

(if (and (> b a) (< b (* a b))) b
  a)
;; 4

(cond ((= a 4) 6)
	  ((= b 4) (+ 6 7 a))
	  (else 25))
;; 16

(+ 2 (if (> b a) b a))
;; 6

(* (cond ((> a b) a)
		 ((< a b) b)
		 (else -1))
   (+ a 1))
;; 16

; problem 1-2 
; (/ (* 3 (* (- 6 2) (- 2 7))) (+ (+ 5 4) (- 2 (- 3 (+ 6 (/ 5 4))))))

; problem 1-3
(define (plus-square a b c)
  (cond ((and (> a b) (> a c)) 
		 (if (> b c) (sum-of-squares a b)
		   (sum-of-squares a c)))
		((and (> b c) (> b a))
		 (if (> a c) (sum-of-squares b a)
		   (sum-of-squares b c)))
		((and (> c a) (> c b))
		 (if (> a b) (sum-of-squares c a)
		   (sum-of-squares c b)))))

; problem 1-4
; b의 값에 따라서 b가 양수아면
; + 로 값을 구하고
; b가 음수 이면 - 값으로 
; 음수에 -를 더해 양수로 만들어 값을 구한다
; 만약 b가 -2 이면
; (-(-2)) 형태가 되어 결국에는 양수로 된다.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; problem 1-5
(define (p) (p))

(define (test x y)
  (if (= x 0)
	0
	y))

; 인자 먼저 계산 법에서 (test 0 (p)) 실행 결과
; 인자인 (p) 프로지서의 값을 먼저 구해야 되므로
; (p) 프로시저가 실행이 되나. (p)는 자신을 호출 하므로
; 영원히 종료되지 않는다.
; (test 0 (p))
; (p) -> (p) -> (p) -> (p) -> (p) -> ........

; 정의 대로 계산 법에서 (test 0 (p)) 실행 결과
; 인자인 0, (p)가 내부의 값으로 변경
; (test 0 (p))
; (if (= 0 0) 
;     0
;     (p))
; 0 = 0 이므로 0으로 종료됨

; 1.1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x) guess
	(sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; problem 1-6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
		(else else-clause)))
; new-if 사용시 sqrt-iter가 함수의 인자로 받아 들여지므로
; 인자 먼저 계산법으로 인하여 sqrt-iter가 무한 반복이 되는 현상이다.
; 즉, if 사용시에는 인저 먼저 계산법으로 받아들여 지지 않는다는
; 얘기 인 것 같다.

; problem 1-7
; (sqrt 4)의 값을 구할 때 흔히 알고 있는 2라는 값이 출력 되는
; 것이 아니라 2.0000102400262145라는 값이 출력 되고
; 역시 마찬가지로 (sqrt 90000000000)의 값을 구할 때도
; 300000.이라는 값으로 출력이 된다.
; 정상적으로 나오게 하면 되는 것이 아닐까 싶다.

; 인터넷에서 찾아보니 작은 값의 경우 (sqrt 0.001)의 값을 구할 수가
; 없다는 것이다.
; 아주 큰 값의 경우에는 9로 시작하는 20자리 이상의 값을 입력 할 경우에
; 값이 구해 지지 않고 멈추는 현상이 발생 한다.
; 문제를 보면 참 값에 더 가까운 값 guess를 구하기 위해 어림 잡은 값을 
; 조금씩 고쳐 나가면서 헌 값에 견주어 고친 값이 그다지 나아지지 않을 
; 때까지 계산을 이어가는 것이다 라는 얘기가 나온다.
; 이 말의 뜻이 정확히 어떤 뜻인지 잘 모르겠다.
; 왜 작은 값과 큰 값의 경우에 수가 정확하게 나오지 않는지 내 생각을
; 정리 하는 것이 필요 할 듯 싶다.
; 오차를 줄이기 위한 과정이 하나 더 추가 된거 같긴 한데
; 왜 (* 0.001 guess)가 추가된 건지는 잘 모르겠다.
; 좀 더 고민 해 봐야 될 문제 이다.
(define (sqrt-iter before-guess guess x)
  (if (good-enough? before-guess guess) guess
		  (sqrt-iter guess (improve guess x) x)))

(define (good-enough? before-guess guess)
  (< (abs (- before-guess guess)) (* 0.001 guess)))

(define (sqrt x)
  (sqrt-iter 1.0 (improve 1.0 x) x))

; problem1-8
; sqrt의 방법으로 세제곱근을 구하는 것을 만들었음
; 그러나 현재 구하는 식으로는 소수점은 구하지 못할 것으로 보임

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* guess 2)) 3))

(define (guess-enough? before-guess guess x)
  (< (abs (- (improve guess x) before-guess)) 0.001))

(define (three-sqrt-iter before-guess guess x)
  (if (guess-enough? before-guess guess x) guess
	(three-sqrt-iter guess (improve guess x) x)))

(define (three-sqrt x)
  (three-sqrt-iter 1.0 (improve 1.0 x) x))

; 1.1.8
(define (square x)
  (exp (double (log x))))

(define (double x) (+ x x))

(define (sqrt x)
  (define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
	(average guess (/ x guess)))
  (define (sqrt-iter guess x)
	(if (good-enough? guess x) guess
	  (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (sqrt x)
  (define (good-enough guess)
	(< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
	(average guess (/ x guess)))
  (define (sqrt-iter guess)
	(if (good-enough? guess x) guess
	  (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; 1.2
;; 1.2.1
(define (factorial n)
  (if (= n 1) 1
	(* n (factorial (- n 1)))))

(define (factorial2 n)
  (define (iter product counter)
	(if (> counter n) product
	  (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(define (factorial3 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count) product
	(fact-iter (* counter product) (+ counter 1) max-count)))

; problem 1-9
; 아래 식은 inc 계산을 미루는 되도는 프로세스 이다.
; (define (+ a b)
;   (if (= a 0) b
;	 (inc (+ (dec a) b))))

 (define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

; 아래 식은 자기의 계산을 반복하는 반복 프로세스 이다.
; (define (+ a b)
;   (if (= a 0) b
; 	 (+ (dec a) (inc b))))

; problem 1-10
(define (A x y)
  (cond ((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (A (- x 1)
				 (A x (- y 1))))))

;; 1024
(A 1 10)

;; 65536
(A 2 4)

;; 65536
(A 3 3)

;; 2n
; (define (f n) (A 0 n))

;; 2^n
; (define (g n) (A 1 n))

;; 2^n*n
; (define (h n) (A 2 n))

;; 1.2.2
(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib2 n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0) b
	(fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
		((or (< amount 0) (= kinds-of-coins 0)) 0)
		(else (+ (cc amount (- kinds-of-coins 1))
				 (cc (- amount (first-denomination kinds-of-coins))
					 kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
		((= kinds-of-coins 2) 5)
		((= kinds-of-coins 3) 10)
		((= kinds-of-coins 4) 25)
		((= kinds-of-coins 5) 50)))

;; 1.2.4
(define (expt b n)
  (if (= n 0) 1
	(* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0) product
	(expt-iter b (- counter 1) (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (gcd a b)
  (if (= b 0) a
	(gcd  b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder (square (expmod base (/ exp 2) m)) m))
		(else
		  (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)))

;; 1.3
(define (cube x) (* x x x))

;; 1.3.1
(define (sum-integers a b)
  (if (> a b) 0
	(+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b) 0
	(+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b) 0
	(+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b) 0
	(+ (term a)
	   (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes2 a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers2 a b)
  (sum identity a inc b))

(define (pi-sum2 a b)
  (define (pi-term x)
	(/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
	(+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;;1.3.2
(define (pi-sum3 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) a
	   (lambda (x) (+ x 4)) b))

(define (integral3 f a b dx)
  (* (sum f (+ a (/ dx 2.0))
		  (lambda (x) (+ x dx)) b)
	 dx))

(define plus4 (lambda (x) (+ x 4)))

(define (f x y)
  (define (f-helper a b)
	(+ (* x (square a))
	   (* y b)
	   (* a b)))
  (f-helper (+ 1 (* x y))
			(- 1 y)))

(define (f-lambda x y)
  ((lambda (a b)
	 (+ (* x (square a))
		(* y b)
		(* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f-let x y)
  (let ((a (+ 1 (* x y)))
		(b (- 1 y)))
	(+ (* x (square a))
	   (* y b)
	   (* a b))))

(define (f-define x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
	 (* y b)
	 (* a b)))

; problem 1-34
; (define (f g)
;   (g 2))

; (define (square x)
;   (* x x))

; (f f)의 계산은 (f f) -> (f 2) -> (2 2)의 흐름으로
; 계산이 된다. 그러므로 (2 2)는 계산이 될 수 없다.

;; 1.3.3
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
	(if (close-enough? neg-point pos-point) midpoint
	  (let ((test-value (f midpoint)))
		(cond ((positive? test-value)
			   (search f neg-point midpoint))
			  ((negative? test-value)
			   (search f midpoint pos-point))
			  (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
		(b-value (f b)))
	(cond ((and (negative? a-value) (positive? b-value))
		   (search f a b))
		  ((and (negative? b-value) (positive? a-value))
		   (search f b a))
		  (else
			(error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

(define (sqrt-fixed-point x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; 1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-pro-pro x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (deriv g)
  (lambda (x)
	(/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
	(- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fixed-trans x)
  (fixed-point-of-transform (lambda (y) (/ x y)) 
							average-damp 1.0))

(define (sqrt-fixed-trans2 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
							newton-transform
							1.0))
; problem 1-41
(define (inc i)
  (+ i 1))

(define (double f)
  (lambda (x) (f (f x))))

;; problem 1-42
(define (compose f g)
  (lambda (x) (f (g x))))

