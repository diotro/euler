#lang racket

#|
A permutation is an ordered arrangement of objects. For example, 3124 is one
possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
are listed numerically or alphabetically, we call it lexicographic order. 
The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
|#

; I'm aware that there's a cleverer mathy solution where you calculate exactly which swaps
; have to be made at each point, and find the millionth, but this runs in
; less than a minute so I'm fine with it


; [List-of N] N -> String
; what is the nth lexicographic permutation of the given digits?
(define (nth-lexicographic-permutation digits n)
  (list-ref
   (sort (map (λ (l) (string-join l ""))
              (permutations (map number->string digits)))
         string<?)
   (sub1 n)))

(module+ test
  (require rackunit)
  (check-equal? (nth-lexicographic-permutation '(0 1 2) 1) "012")
  (check-equal? (nth-lexicographic-permutation '(0 1 2) 6) "210"))


; Answer:
(nth-lexicographic-permutation (build-list 10 identity) 1000000)