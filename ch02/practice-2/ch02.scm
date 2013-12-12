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



