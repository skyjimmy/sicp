;; 아래 식은 inc 계산을 미루는 되도는 프로세스 이다.
(define (+ a b)
  (if (= a 0) b
	(inc (+ (dec a) b))))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

;; 아래 식은 자기의 계산을 반복하는 반복 프로세스 이다.
(define (+ a b)
  (if (= a 0) b
	(+ (dec a) (inc b))))

