(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;problem 3-1
(define (make-accumulator ac)
  (define (accumulator amount)
    (begin (set! ac (+ ac amount))
	   ac))
  accumulator)

; problem 3-2
(define (make-monitored f)
  (define manycalls 0)
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) manycalls)
	  (else (begin (set! manycalls (+ manycalls 1))
		       (f m)))))
  dispatch)

; problem 3-3
(define (make-account balance pass)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (cond ((eq? password pass)
	   (cond ((eq? m 'withdraw) withdraw)
		 ((eq? m 'deposit) deposit)
		 (else (error "Unknown request -- MAKE-ACCOUNT" m))))
	  (else (error "Incorrect password" m))))
  dispatch)

; problem 3-4 - ing
(define (make-account balance pass)
  (define warerror 0)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)    
    (cond ((eq? password pass)
	   (cond ((eq? m 'withdraw) withdraw)
		 ((eq? m 'deposit) deposit)
		 (else (error "Unknown request -- MAKE-ACCOUNT" m))))
	  ((> warerror 7)
	   (display 'call-the-cops))
	  (else (set! warerror (+ warerror 1)))))
  dispatch)

;3.1.2
; add
(define random-init 7)

; add
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

; rand
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; rand-update
(define (estimate-pi-2 trials)
  (sqrt (/ 6 (random-gcd-test-2 trials random-init))))

(define (random-gcd-test-2 trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
	(cond ((= trials-remaining 0)
	       (/ trials-passed trials))
	      ((= (gcd x1 x2) 1)
	       (iter (- trials-remaining 1)
		     (+ trials-passed 1)
		     x2))
	       (else
		(iter (- trials-remaining 1)
		      trials-passed
		      x2))))))
  (iter trials 0 initial-x))

	
    
	
