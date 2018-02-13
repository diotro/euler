#lang racket
#|

Let d(n) be defined as the sum of proper divisors of n
(numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are
an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.


|#

(require math/number-theory
         rackunit)

; amicable-sum : N -> N
; returns the sum of all amicable numbers below n
(define (amicable-sum n)
  (foldr + 0 (filter amicable? (build-list n identity))))

(define (amicable? n)
  (define (divisor-sum n)
    (foldr + 0 (remove n (divisors n))))
  (and
   (not (= n (divisor-sum n)))
   (= n (divisor-sum (divisor-sum n)))))

(module+ test
  (check-pred amicable? 220)
  (check-pred amicable? 284))

(amicable-sum 10000)