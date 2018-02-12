#lang racket
#|

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?

|#

(require rackunit)

; power-digit-sum : N -> N
; produces the sum of the digits of 2 ^ n
(define (power-digit-sum n)
  (sum-digits (expt 2 n)))

(module+ test
  (check-equal? (power-digit-sum 15) 26))

; sum-digits : N -> N
; what is the sum of the digits of the given number?
(define (sum-digits n)
  (cond [(< n 10) n]
        [else (+ (remainder n 10) (sum-digits (quotient n 10)))]))

(module+ test
  (check-equal? (sum-digits 3) 3)
  (check-equal? (sum-digits 34) 7)
  (check-equal? (sum-digits 345) 12))


; Answer:
(power-digit-sum 1000)