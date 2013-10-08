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

