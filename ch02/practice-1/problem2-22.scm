(define nil (list))

;; 값이 반대로 나오는 이유
;; 아래 계산 식에서 cons 부분의 계산 식이
;; (square) 부분이 먼저 계산 되므로 먼저 계산 된 값이
;; list에서 뒤로 밀리므로 값이 결과가 반대로 나온다

(define (square-list items)
 (define (iter things answer)
  (if (null? things) answer
      (iter (cdr things) (cons (square (car things)) answer))))
 (iter items nil))

(define (square-list-a items)
 (define (iter things answer)
  (if (null? things) answer
      (iter (cdr things) (cons answer (square (car things))))))
 (iter items nil))


