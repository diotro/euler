#lang racket
#|
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
|#

(require rackunit)

; N -> N
; produces the product of a pythagorean triplet where a + b + c = n
(define (pythagorean-triplet-product n)
  (apply * (pythagorean-triplet n)))

(module+ test
  (check-equal? (pythagorean-triplet-product 12) (* 3 4 5)))


; N -> (list N N N)
; produces a pythagorean triplet that sums to n
(define (pythagorean-triplet n)
  (define (works? a b)
    (define c (sqrt (+ (expt a 2) (expt b 2))))
    (and (integer? c)
         (= n (+ a b c))))
  
  (for*/last ([a (in-range 1 n)]
              [b (in-range 1 n)]
    #:when (works? a b))
    (list b a (sqrt (+ (expt a 2) (expt b 2))))))


(module+ test
  (check-equal? (pythagorean-triplet (+ 3 4 5)) '(3 4 5))
  (check-equal? (pythagorean-triplet (+ 5 12 13)) '(5 12 13)))


; The answer:
(pythagorean-triplet-product 1000)