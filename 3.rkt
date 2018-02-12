#lang racket

#|
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
|#


(require rackunit)

;; Racket has nice builtins:
(require math/number-theory)


(define (largest-prime-factor n)
  (apply max (filter prime? (map first (factorize n)))))

(module+ test
  (check-equal? (largest-prime-factor 6) 3)
  (check-equal? (largest-prime-factor 4) 2)
  (check-equal? (largest-prime-factor 30) 5))


;; But to prove a point, I'll rewrite factorize and prime?...


; N -> [AList N N]
; returns the number's factors and their exponents
(define (factorize n)
  (define (find-factor cur-n cur-factor)
    (cond
      [(= 1 cur-n) '()]
      [(zero? (modulo cur-n cur-factor))
       (incr-alist-counter cur-factor (find-factor (/ cur-n cur-factor) 2))]
      [else (find-factor cur-n (add1 cur-factor))]))
  (reverse (find-factor n 2)))

(module+ test
  (check-equal? (factorize 1) '())
  (check-equal? (factorize 16) '((2 4)))
  (check-equal? (factorize 35) '((5 1) (7 1)))
  (check-equal? (factorize 110) '((2 1) (5 1) (11 1)))
  (check-equal? (factorize 100) '((2 2) (5 2)))
  )


; incr-alist-counter : N [AList N N] -> [AList N N]
; increments the counter of the given key in the given a-list,
; or adds a new one if it doesn't exist
(define (incr-alist-counter key alist)
  (cond [(empty? alist) `((,key 1))]
        [else (if (= key (first (first alist)))
                  (cons (list (first (first alist))
                              (add1 (second (first alist))))
                        (rest alist))
                  (cons (first alist) (incr-alist-counter key (rest alist))))]))

(module+ test
  (check-equal? (incr-alist-counter 1 '()) '((1 1)))
  (check-equal? (incr-alist-counter 3 '((1 2) (3 4))) '((1 2) (3 5))))


; prime? : N -> Boolean
; is the given number prime?
; NOTE: not even close to effecient
(define (prime? n)
  (= 1 (length (factorize n))))



;; ANSWER:
(largest-prime-factor 600851475143)




