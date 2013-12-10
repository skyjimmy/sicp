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

(define (list-ref items n)
 (if (= n 0) (car items)
  (list-ref (cdr items) (- n 1))))

(define (length items)
 (if (null? items) 0
  (+ 1 (length (cdr items)))))


(define (length items)
 (define (length-iter a count)
  (if (null? a) count
   (length-iter (cdr a) (+ 1 count))))
 (length-iter items 0))

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

(define (scale-list items factor)
 (map (lambda (x) (* x factor))
      items))

(define (count-leaves x)
 (cond ((null? x) 0)
       ((not (pair? x)) 1)
       (else (+ (count-leaves (car x))
                (count-leaves (cdr x))))))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
		((not (pair? tree)) (* tree factor))
		(else (cons (scale-tree (car tree) factor)
					(scale-tree (cdr tree) factor)))))


(define (scale-tree tree factor)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
		     (scale-tree sub-tree factor)
			 (* sub-tree factor))) tree))


