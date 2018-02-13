#lang racket

#|

n! means n × (n - 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!

|#

(require rackunit)

; factorial-digit-sum : N -> N
; what is the sum of the digits of n factorial?
(define (factorial-digit-sum n)
  (digit-sum (! n)))

(module+ test
  (check-equal? (factorial-digit-sum 10) 27))


(define (! n)
  (cond [(= n 1) 1]
        [else (* n (! (sub1 n)))]))

(define (digit-sum n)
  (cond [(< n 10) n]
        [else (+ (modulo n 10) (digit-sum (quotient n 10)))]))


; Answer:
(factorial-digit-sum 100)