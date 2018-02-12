#lang racket

#|
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
|#

(require math/number-theory)

(module+ test
  (require rackunit))

; N -> N
; what is the sum of the primes below n?
(define (sum-primes-below n)
  (foldr + 0 (primes-below n)))

(module+ test
  (check-equal? (sum-primes-below 6) 10)
  (check-equal? (sum-primes-below 10) 17))

(define (primes-below n)
  (cond [(= n 2) '()]
        [else (define next-one (prev-prime n))
              (cons next-one (primes-below next-one))]))

(module+ test
  (check-equal? (primes-below 6) '(5 3 2)))


; ANSWER:
(sum-primes-below 2000000)