#lang racket
#|
A perfect number is a number for which the sum of its proper divisors is exactly
equal to the number. For example, the sum of the proper divisors of 28 would be
1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number
that can be written as the sum of two abundant numbers is 24. By mathematical analysis,
it can be shown that all integers greater than 28123 can be written as the sum of two
abundant numbers. However, this upper limit cannot be reduced any further by analysis
even though it is known that the greatest number that cannot be expressed as the sum of
 two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as
the sum of two abundant numbers.
|#

(require math/number-theory)
; N -> N
; Sums all abundant numbers up to n
; NOTE: there are no abundant numbers above 28123
(define (sum-abundant n)
  (define (abundant? x)
    (> (- (divisor-sum x) x) x))

  (define abundant-map (apply vector (cons "idx 0" (build-list 28123 (λ (x) (abundant? (add1 x)))))))
  (define (abundant?-fast n)
    (vector-ref abundant-map n))
     
  (define abundant-nums (filter abundant?-fast (build-list 28123 add1)))
  (define (not-sum-of-two-abundant? n)
    (not (for/or ([abundant1 abundant-nums]
                  #:when (> n abundant1))
           (abundant?-fast (- n abundant1)))))
  
  (foldr + 0 (filter not-sum-of-two-abundant? (build-list n add1))))

(module+ test
  (require rackunit)
  (check-equal? (sum-abundant 3) 6)
  (check-equal? (sum-abundant 24) (sum-abundant 23)))


; Answer:
(sum-abundant 28123)