(define (f g)
  (g 2))

(f f)

(define (square x)
  (* x x))

;; (f f)의 계산은 (f f) -> (f 2) -> (2 2)의 흐름으로
;; 계산이 된다. 그러므로 (2 2)는 계산이 될 수 없다.
