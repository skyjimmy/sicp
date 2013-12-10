;; 뭘 요구 하는 것인가?
;; 데이터 추상화에 대한 사용성에 대해서 묻는 문제이다.
;; 어떤 의미가 있는가?
;; 데이터를 추상화 함으로써 변경이 용이하고 다른 프로시저에 영향도를
;; 최소화 할 수 있다.
(define (make-point x y)
 (cons x y))

(define (x-point p)
 (car p))

(define (y-point p)
 (cdr p))

(define (make-segment f e)
 (cons f e))

(define (start-segment s)
 (car s))

(define (end-segment s)
 (cdr s))

(define (midpoint-segment s)
 (cons (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
       (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

(define (print-point p)
 (newline)
 (display "(")
 (display (x-point p))
 (display ".")
 (display (y-point p))
 (display ")"))

