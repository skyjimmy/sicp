(load "1.1.4.scm")

(define (problem1-3 a b c)
 (cond ((and (> a b) (> b c)) (+ (square a) (square b)))
       ((and (> a b) (< b c)) (+ (square a) (square c)))
       ((and (> b a) (> a c)) (+ (square b) (square a)))
       ((and (> b a) (< a c)) (+ (square b) (square c)))
       ((and (> c a) (> a b)) (+ (square c) (square a)))
       ((and (> c a) (> b a)) (+ (square c) (square b)))))

