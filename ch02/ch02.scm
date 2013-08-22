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

;; 2.2.1
(define one-though-four (list 1 2 3 4))

one-though-four

(car one-though-four)
;; 1

(cdr one-though-four)
;; (2 3 4)

(car (cdr one-though-four))
;; 2

(cons 10 one-though-four)
;; (10 1 2 3 4)

(cons 5 one-though-four)
;; (5 1 2 3 4)

(define (list-ref items n)
 (if (= n 0) (car items)
  (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)
;; 16

(define (length items)
 (if (null? items) 0
  (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(length odds)
;; 4

(define (length items)
 (define (length-iter a count)
  (if (null? a) count
   (length-iter (cdr a) (+ 1 count))))
 (length-iter items 0))

(append squares odds)
;; (1 4 9 16 25 1 3 5 7)

(append odds squares)
;; (1 3 5 7 1 4 9 16 25)

(define (append list1 list2)
 (if (null? list1) list2
  (cons (car list1) (append (cdr list1) list2))))

(define nil ())
 
;; list mapping
(define (scale-list items factor)
 (if (null? items) nil
     (cons (* (car items) factor)
           (scale-list (cdr items) factor))))

;; (scale-list (list 1 2 3 4 5) 10)
;; (10 20 30 40 50)

(define (map proc items)
 (if (null? items) nil
     (cons (proc (car items))
           (map (proc (cdr items))))))

;; (map abs (list -10 2.5 -11.6 17))
;; (10 2.5 11.6 17)

(map (lambda (x) (* x x))
     (list 1 2 3 4))

(define (scale-list items factor)
 (map (lambda (x) (* x factor))
      items))

;; 2.2.2
(cons (list 1 2) (list 3 4))

(define x (cons (list 1 2) (list 3 4)))

(length x)
;; 3

(count-leaves x)
;; 4

(list x x)
;; (((1 2) 3 4) ((1 2) 3 4))

(length (list x x))
;; 2

(count-leaves (list x x))
;; 8

(define (count-leaves x)
 (cond ((null? x) 0)
       ((not (pair? x)) 1)
       (else (+ (count-leaves (car x))
                (count-leaves (cdr x))))))

