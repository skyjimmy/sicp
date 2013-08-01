;; 2.1.1
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

(define (equal-rat? x y)
 (= (* (numer x) (denom y))
    (* (numer y) (denom x))))

(define (make-rat n d)
 (let ((g (gcd n d)))
  (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
 (newline)
 (display (numer x))
 (display "/")
 (display (denom x))
 (newline))
        
;; 2.1.3
(define (cons x y)
 (define (dispatch m)
  (cond ((= m 0) x)
        ((= m 1) y)
        (else (error "Argument not 0 or 1 -- CONS" m))))
 dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))


