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
