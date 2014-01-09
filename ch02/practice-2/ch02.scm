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

(define (append list1 list2)
  (if (null? list1) list2
	(cons (car list1) (append (cdr list1) list2))))

;; problem 2-17
(define (last-pair items)
  (if (= (length-221 items) 1) items
	(last-pair (cdr items))))

;; problem 2-18
;; 아래 결과는 괄호에 묶인 형태로 결과가 출력됨
;; (((((() 5) 4) 3) 2) 1)
(define (reverse-218 items)
  (if (null? items) (list) 
	(list (reverse-218 (cdr items)) (car items))))

;; list-ref는 0부터 시작 마지막 값은 lenght 의 -1 이다.
(define (reverse-218-2 items)
  (define (reverse-218-2-iter len)
	(if (< len 0) (list)
	  (let ((value (list-ref-221 items len)))
		(cons value (reverse-218-2-iter (- len 1))))))
  (let ((endvalue (- (length-221 items) 1)))
	(reverse-218-2-iter endvalue)))

;; problem 2-19
;; 코인 단위의 위치를 변경해도 결과에 영향이 없는 이유
;; 값을 꺼내와서 계산을 하는 것이기 때문에 값이 변경되지 않는 한
;; 위치는 상관이 없다.
(define us-coins (list 1 5 50 25 10))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else
		  (+ (cc amount (except-first-denomination coin-values))
			 (cc (- amount (first-denomination coin-values))
				 coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;; problem 2-20
(define (same-parity f . l)
  (define (same-parity-iter remain lis)
	(cond ((null? lis) (list))
		  ((= (remainder (car lis) 2) remain)
		   (cons (car lis) (same-parity-iter remain (cdr lis))))
		  (else (same-parity-iter remain (cdr lis)))))
  (if (= (remainder f 2) 0) (cons f (same-parity-iter 0 l))
	(cons f (same-parity-iter 1 l))))

;; list mapping
(define (scale-list items factor)
  (if (null? items) '()
	(cons (* (car items) factor)
		  (scale-list (cdr items) factor))))

(define (map-221 proc items)
  (if (null? items) '()
	(cons (proc (car items))
		  (map-221 proc (cdr items)))))

(define (scale-list-2 items factor)
  (map-221 (lambda (x) (* x factor)) items))

;; problem 2-21
(define (square-list-1 items)
  (if (null? items) '()
	(cons (* (car items) (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

;; problem 2-22
;; 순서가 왜 뒤집혀서 출력되는지 설명하기
;; 아래 식을 풀어보면 items이 1 2 3 일때
;; (iter (1 2 3) ())
;; (iter (2 3) (cons 1 ()))
;; (iter (3) (cons 4 1 ()))
;; (iter () (cons 9 4 1 ()))
;; 순으로 식이 풀이가 된다. 즉 answer의 값이 뒤에 추가가 되기 때문이다.
(define (square x) (* x x))

(define (square-list-222 items)
  (define (iter things answer)
	(if (null? things) answer
	  (iter (cdr things) (cons (square (car things)) answer))))
  (iter items '()))

;; 아래 식은 answer의 위치를 바꾸었을 때의 프로시져이다.
;; items가 (list 1 2 3)일때 (((() . 1) . 4) . 9)로 결과가
;; 출력되는 이유 설명하기
;; cons의 특성상 (cons 10 (list 1 2)) 일때는 정상적으로
;; (10 1 2)로 출려이 되지만
;; (cons (list 1 2) 10) 일때는 ((1 2) 10)으로 출력되기 때문이다.

(define (square-list-222-2 items)
  (define (iter things answer)
	(if (null? things) answer
	  (iter (cdr things) (cons answer (square (car things))))))
  (iter items '()))

;; problem-2-23
(define (for-each-223 f items)
  (cond ((not (null? items)) (f (car items))
							 (for-each-223 f (cdr items)))))

;; 2.2.2
;; (cons (list 1 2) (list 3 4)) -> ((1 2) 3 4)
;; tree
(define (count-leaves x)
  (cond ((null? x) 0)
		((not (pair? x)) 1)
		(else (+ (count-leaves (car x))
				 (count-leaves (cdr x))))))
;; problem 2-24 - 종이에 그림

;; problem 2-25
;; (1 3 (5 7) 9) -> (car (cdr (car (cdr (cdr x)))))
;; ((7)) -> (car (car x))
;; (1 (2 (3 (4 (5 (6 7)))))) -> (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

;; problem 2-26
(define x (list 1 2 3))
(define y (list 4 5 6))

;; (append x y) -> (1 2 3 4 5 6)
;; (cons x y) -> ((1 2 3) 4 5 6)
;; (list x y) -> ((1 2 3) (4 5 6))

;; problem 2-27
(define x-227 (list (list 1 2) (list 3 4)))

(define (deep-reverse x)
  (cond ((null? x) '())
		((not (pair? x)) x)
		(else (cons (deep-reverse (car (reverse x))) (deep-reverse (cdr (reverse x)))))))

;; problem 2-28
;; append는 list를 매개변수로 받아야 계산 가능하다
(define (fringe x)
  (cond ((null? x) '())
		((not (pair? x)) (list x))
		(else (append (fringe (car x)) (fringe (cdr x))))))
				   
;; problem 2-29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a. left-branch, right-branch, branch-length, branch-structure 만들기
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;; b. total-weight 만들기 - no
;; c. cons로 변경 했을 때 프로그램 수정 정도 - no

;; tree 매핑
(define (scale-tree tree factor)
  (cond ((null? tree) '())
		((not (pair? tree)) (* tree factor))
		(else (cons (scale-tree (car tree) factor)
					(scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree) 
		   (scale-tree-map sub-tree factor)
		   (* sub-tree factor)))
	   tree))

;; problem 2-30
;; 바로 정의
(define (square-tree tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (* tree tree))
		(else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

;; map으로 정의
(define (square-tree-map tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree) (square-tree-map sub-tree)
		   (* sub-tree sub-tree)))
	   tree))

  
;; problem 2-31
(define (tree-map fn tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (fn tree))
		(else (cons (tree-map fn (car tree)) (tree-map fn (cdr tree))))))

;; problem 2-32
;; 시작 subsets (list 1 2 3)
;; 1번째 let의 subsets (2 3)
;; 2번째 let의 subsets (3)
;; 3번째 let의 subsets '()
;; 지금까지의 상황 (subsets (subsets (subsets '())))
;; 1번째 return 값 (()) s 값은 '() 
;; 2번째 return 값 (() (3)) lambda의 (car s) 값은 3이고  원본 list는(3) 
;; 3번째 return 값 (() (3) (2) (2 3)) lambda의 (car s) 값은 2이고 원본 list는 (2 3)
;; 마지막 return 값 (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)) lambda의 (car s) 값은 1이고 원본 list는 (1 2 3)
;; 원래 (() (3)) 이라면 (car (list 1 2 3)) 일때 (() (3) (1) (1 3))으로 늘어아는 구조 이다.

(define (subsets s)
  (if (null? s) (list '())
	(let ((rest (subsets (cdr s))))
	  (display (car s))
	  (newline)
	  (display rest)
	  (newline)
	  (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; 2.2.3
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
		((not (pair? tree))
		 (if (odd? tree) (square tree) 0))
		(else (+ (sum-odd-squares (car tree))
				 (sum-odd-squares (cdr tree))))))

(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1)) (fib (- n 2))))))

(define (even-fibs n)
  (define (next k)
	(if (> k n) '()
	  (let ((f (fib k)))
		(if (even? f)
		  (cons  f (next (+ k 1)))
		  (next (+ k 1))))))
  (next 0))

;; 차례열 연산
(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high) '()
	(cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons '() (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons '() (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

;; problem 2-33
;; map의 계산 흐름
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; problem 2-34
;; 책에서 든 다항식 연산 예제
;; x=2일때 1 + 3x + 5x^3 + x^5를 호너의 규칙으로 변환 하면
;; x^5 부터 거꾸로 연산 된다.
;; ((((1*x + 0)*x + 5)*x + 0)*x + 3)*x + 1
;; x에 2를 대입 하면
;; ((((2 + 0)2 + 5)2 + 0)2 + 3)2 + 1
;; 안쪽 괄호부터 계산 하면
;; (((4 + 5)2 + 0)2 + 3)2 + 1
;; ((18 + 0)2 + 3)2 + 1
;; (36 + 3)2 + 1
;; 78 + 1 = 79
;; 위 다항식을 손으로 계산해도 79가 나온다
(define (horner-evel x coefficient-sequence)
  (accumulate (lambda (this-coeff highter-terms) (+ this-coeff (* x highter-terms)))
			  0
			  coefficient-sequence))
