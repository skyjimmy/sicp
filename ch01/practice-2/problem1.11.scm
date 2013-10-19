;;  되도는 프로세스
(define (f n)
  (if (> 3 n) n
	(+ (f (- n 1)) (* (f (- n 2)) 2) (* (f (- n 3)) 3))))

;; 반복 프로세스
;; 몇일 동안 계산 방법을 생각하다가 도저히 모르겠어서
;; 인터넷을 참조 하였다.
;; 위의 계산식에 보는 바와 같이 되도는 프로세에서는
;; 레벨이 커질수록 이전에 구했던 값들을 가지고 있다.
;; 반복 프로세스에서도 이전에 구했던 값을 가지고 있도록
;; 수정을 해야한다.

(define (f2 n)
  (f2-iter 2 1 0 n))

(define (f2-iter curr prv prv_prv count)
  (if (> 3 count) curr 
	(f2-iter (+ curr (* 2 prv) (* 3 prv_prv)) curr prv (- count 1))))

