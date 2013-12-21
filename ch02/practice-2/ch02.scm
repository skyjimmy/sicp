(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define (linear-combination2 a b x y)
  (add (mul a x) (mul b y)))

(define (add a b)
  (+ a b))

(define (mul a b)
  (* a b))

;; 2.1
(define (make-rat n d) 
  (let ((g (gcd n d)))
	(cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))

(define x (cons 1 2))

(car x) ;; 1
(cdr x) ;; 2

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(car (car z)) ;; 1
(car (cdr z)) ;; 3

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;; problem 2.1 - no

;; 2.1.2
(define (make-rat-2 n d)
  (cons n d))

(define (numer-2 x)
  (let ((g (gcd (car x) (cdr x))))
	(/ (car x) g)))

(define (denom-2 x)
  (let ((g (gcd (car x) (cdr x))))
	(/ (cdr x) g)))

;; problem 2-2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (cons (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2) (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

;; problem 2-3 - no

;; 2.1.3
(define (cons213 x y)
  (define (dispatch m)
	(cond ((= m 0) x)
		  ((= m 1) y)
		  (else (error "Arcument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car213 z) (z 0))
(define (cdr213 z) (z 1))

;; problem 2-4
;; (define (cons24 x y)
;;   (lambda (m) (m x y)))

;; (define (car24 z)
;;   (z (lambda (p q) p)))

;; 맞바꿈 게산으로 (car24 (cons24 x y)) 풀이
;; (car24 (cons24 x y))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; (lambda (x y) x)
;; x

;; cdr 정의 
;; (define (cdr24 z)
;;   (z (lambda (p q) q)))

;; problem 2-5 - no 

;; problem 2-6 - no

;; 2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
								 (/ 1.0 (lower-bound y)))))

;; problem 2-7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))

;; problem 2-8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
				 (- (upper-bound x) (upper-bound y))))

;; problem 2-9 - no

;; problem 2-10 - no 

;; problem 2-11 - no

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; problem 2-12 - no

;; problem 2-13 - no

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
				(add-interval r2 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
	(div-interval one (add-interval (div-interval one r1)
									(div-interval one r2)))))

;; problem 2-14 - no

;; problem 2-15 - no

;; problem 2-16 - no

;; 2.2
;; 2.2.1
;; (cons 1 (cons 2 (cons 3 (cons 4 nil))))
(define one-through-four (list 1 2 3 4))

(define (list-ref-221 items n)
  (if (= n 0)
	(car items)
	(list-ref-221 (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length-221 items)
  (if (null? items) 0
	(+ 1 (length-221 (cdr items)))))

(define odds (list 1 3 5 7))

(define (length-221-2 items)
  (define (length-iter a count)
	(if (null? a) count
	  (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append-221 list1 list2)
  (if (null? list1) list2
	(cons (car list1) (append-221 (cdr list1) list2))))

;; problem 2-17
(define (last-pair items)
  (if (= (length-221 items) 1) items
	(last-pair (cdr items))))

