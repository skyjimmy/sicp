(define (cons x y)
 (lambda (m) (m x y)))

(define (car z)
 (z (lambda (p q) p)))

;; (car (cons x y)) 가 x 임을 보여라
;; lambda의 활용 관련 문제 인거 같은데 
;; lambda 관련해서 잘 이해가 가지 않는 부분이 있다.
;; 우선 위 수식을 맞바꿈 계산법으로 풀이를 했을 때 생각을 해본다면
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; 이렇게 일단 풀어 낼 수가 있다
;; 하지만 뒤의 lambda가 어떻게 앞의 lambda 프로시저로 들어가게 되는건지
;; 잘 이해가 가지 않는다.
;; 항상 뒤쪽에서 앞쪽으로 들어간다고 이해를 해야하는 건가?
;; 프로시저이기 때문이라고 생각을 해야하지 않을까 라는 생각이 든다.

;; cdr 정의
(define (cdr z)
 (z (lambda (p q) q)))

