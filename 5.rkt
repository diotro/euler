#lang racket

#|
2520 is the smallest number that can be divided by each of the numbers
 from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
|#

(module+ test
  (require rackunit))


; this solution is slow, you don't need to check every number because you can do some\
; tricks with factorization

; N -> N
; what is the smallest number that divides all numbers up to the given number?
#;
(define (find-smallest-all-divisor max-divisor)
  (define (finder/acc cur-num)
    (cond [(evenly-divisible-by-all-up-to? cur-num max-divisor) cur-num]
          [else (finder/acc (add1 cur-num))]))
    (finder/acc 1))


; N N -> N
; is the given number divisible by all number sup to th
(define (evenly-divisible-by-all-up-to? number max-divisor)
  (define (checker cur-divisor)
    (cond [(= cur-divisor 1) #t]
          [else (if (zero? (modulo number cur-divisor))
                    (checker (sub1 cur-divisor))
                    #f)]))
  (checker max-divisor))

(module+ test
  (check-true (evenly-divisible-by-all-up-to? 2520 10))
  (check-true (evenly-divisible-by-all-up-to? 12 4))
  (check-false (evenly-divisible-by-all-up-to? 12 5))
  (check-false (evenly-divisible-by-all-up-to? 13 2)))


; Answer:
(find-smallest-all-divisor 20)