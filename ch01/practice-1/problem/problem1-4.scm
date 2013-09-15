(define (a-plus-abs-b a b)
 ((if (> b 0) + -) a b))

;; b가 양수이면 + 연산
;; 음수이면 - 연산을 수행한다.
