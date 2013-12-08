;; 1.1.1
486

(+ 137 349)
;; 486

(- 1000 334)
;; 666

(* 5 99)
;; 495

(/ 10 5)
;; 2

(+ 2.7 10)
;; 12.7

(+ 21 35 12 7)
;; 75

(* 25 4 12)
;; 1200

(+ (* 3 5) (- 10 6))
;; 19

;; 1.1.2
(define size 2)

(* 5 size)
;; 10

(define pi 3.14159)

(define radius 10)

(* pi (* radius radius))

(define circumference (* 2 pi radius))

circumference ;; 62.8318

;; 1.1.3
(* (+ 2 (* 4 6))
   (+ 3 5 7))

;; 1.1.4
(define (square x) (* x x))

(square 21)
;; 441

(square (+ 2 5))
;; 49

(square (square 3))
;; 81

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)
;; 25

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)
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

;; 1.1.7
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

;; 1.1.8
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

