#lang racket

; N -> N
; what is the difference between the sum of the squares and the square of the sum
; of all natural numbers up to and including n?
(define (sum-square-difference n)
  (- (square-of-sums n) (sum-of-squares n)))

(module+ test
  (require rackunit)
  (check-equal? (sum-square-difference 10) 2640))


(define (sum-of-squares n)
  (foldr + 0 (build-list n (λ (n) (expt (add1 n) 2)))))

(define (square-of-sums n)
  (expt (foldr + 0 (build-list n add1)) 2))

; Answer:
(sum-square-difference 100)