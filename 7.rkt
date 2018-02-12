#lang racket
#|
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
|#


(require rackunit)
(require math/number-theory)

; N -> N
; what is the nth prime number?
(define (nth-prime n)
  (first
   (for/fold ([primes '()])
             ([i (in-naturals)])
     #:break (= (length primes) n)
     (if (prime? i)
         (cons i primes)
         primes))))


(module+ test
  (check-equal? (nth-prime 6) 13)
  (check-equal? (nth-prime 4) 7))


; Answer:
(nth-prime 10001)