#lang racket

(require rackunit)

#|
If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
|#

; PositiveN -> N
; what is the sum of all the multiples of 3 or 5 below n?
(define (sum-multiples-of-3-or-5 up-to)
  (define (find-sum-below n)
    (cond [(zero? n) 0]
        [else (if (divisible-by-three-or-five? n)
                  (+ n (find-sum-below (sub1 n)))
                  (find-sum-below (sub1 n)))]))
  (find-sum-below (sub1 up-to)))

(module+ test
  (check-equal? (sum-multiples-of-3-or-5 1) 0)
  (check-equal? (sum-multiples-of-3-or-5 2) 0)
  (check-equal? (sum-multiples-of-3-or-5 3) 0)
  (check-equal? (sum-multiples-of-3-or-5 4) 3)
  (check-equal? (sum-multiples-of-3-or-5 10) 23))

; N -> Boolean
(define (divisible-by-three-or-five? n)
  (or (zero? (modulo n 3))
      (zero? (modulo n 5))))
         
; Answer:
(sum-multiples-of-3-or-5 1000)

(module+ test
  (check-equal? (sum-multiples-of-3-or-5.v2 1) 0)
  (check-equal? (sum-multiples-of-3-or-5.v2 2) 0)
  (check-equal? (sum-multiples-of-3-or-5.v2 3) 0)
  (check-equal? (sum-multiples-of-3-or-5.v2 4) 3)
  (check-equal? (sum-multiples-of-3-or-5.v2 10) 23))
