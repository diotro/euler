#lang racket


#|
What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
|#

; N -> N
; what is the index of the first fibonacci number to have n digits?
(define (first-fib-n-digits n)
  ; N -> N
  ; how many digits does the given number have?
  (define (num-length x)
    (string-length (number->string x)))

  
  ; N N N -> N
  ; ACCUMULATOR: the last two fibonacci numbers, and the current index
  (define (fib/acc fib1 fib2 index)
    (define new-fib (+ fib1 fib2))
    (if (<= n (num-length new-fib))
        (add1 index)
        (fib/acc fib2  new-fib (add1 index))))

  (fib/acc 1 1 2))

(module+ test
  (require rackunit)
  ; 1 1 2 3 5 8 13 21 34 55 89 3-digitt
  (check-equal? (first-fib-n-digits 3) 12)
  (check-equal? (first-fib-n-digits 2) 7))

; Answer
(first-fib-n-digits 1000)