#lang racket

#|

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers
finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
|#


(require rackunit)


; longest-collatz : N -> N
; produces the starting number, below the given number, which creates the
; longest collatz sequence
(define (longest-collatz n)
  (first (argmax length (map collatz (build-list n add1)))))

(module+ test
  (check-equal? (longest-collatz 3) 3))

; collatz : N -> [List-of N]
; produces the collatz sequence for the given number
(define (collatz n)
  (cond [(<= n 1) (list n)]
        [else (cons n (collatz (collatz-step n)))]))

(module+ test
  (check-equal? (collatz 4) '(4 2 1)))

; collatz-step : N -> N
; applies the collatz formula to the given number
(define (collatz-step n)
  (if (even? n) (/ n 2) (add1 (* 3 n))))

; Answer:
; (longest-collatz 1000000)

;; ^ This way took too long, I ran out of memory.
;; There's a better way, maybe I'll build a map from each number to where it goes next
;; and then remove each number that has something pointing to it recursively. Whatever
;; is left takes the most steps to reach 1.


; longest-collatz : N -> N
; produces the starting number, below the given number, which creates the
; longest collatz sequence
(define (longest-collatz.v2 n)
  (find-longest (build-collatz-map n) n))

; build-collatz-map : N -> [Map N N]
; builds a map from each number to the next step in the collatz path
(define (build-collatz-map n)
  ; N [Map N N] -> [Map N N]
  (define (add-to-collatz-map next map-so-far)
    (cond [(hash-has-key? map-so-far next) map-so-far]
          [else (add-to-collatz-map
                 (collatz-step next)
                 (hash-set map-so-far
                           next
                           (collatz-step next)))]))
  (foldr add-to-collatz-map (hash) (build-list n add1)))


; find-longest : [Map N N] N -> N
; finds the number below n with the longest path you could take
; if you followed from keys to values recursively
; TERMINATES if all keys lead to 1 eventually
(define (find-longest hash n)
  (define (path-to-1 key)
    (cond [(= key 1) '(1)]
          [else (cons key (path-to-1 (hash-ref hash key)))]))
    
  (define-values [max-len max-key]
    (for/fold ([longest-so-far 0]
               [key 0])
              ([cur-key (build-list n add1)])
      (define cur-key-length (length (path-to-1 cur-key)))
      (if (> cur-key-length longest-so-far)
          (values cur-key-length cur-key)
          (values longest-so-far key))))
  max-key)
     
(module+ test
  (check-equal? (find-longest (hash 4 2 2 1 3 10 10 5 5 16 16 8 8 4) 4) 3))

; Answer:
(longest-collatz.v2 1000000)