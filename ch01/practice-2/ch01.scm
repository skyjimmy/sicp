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

