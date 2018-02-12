#lang racket

#|
A palindromic number reads the same both ways. The largest palindrome made from the
product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
|#

(module+ test
  (require rackunit))

; N -> N
; what's the biggest palindrome you can create with the product of two numbers,
; both of which are below n?
(define (biggest-palindrome-of-product-up-to n)
  (apply max (filter palindrome? (all-products-up-to n))))

(define (palindrome? n)
  (define (number->reverse-string n)
    (list->string (reverse (string->list (number->string n)))))
           
  (string=? (number->string n) (number->reverse-string n)))

(module+ test
  (check-true (palindrome? 1))
  (check-true (palindrome? 10101)))

(define (all-products-up-to n)
  (define (products-with given-base)
    (build-list n (λ (x) (* given-base (add1 x)))))
  (apply append (build-list n (λ (n) (products-with (add1 n))))))

(module+ test
  (check-equal? (all-products-up-to 2)
                '(1 2 2 4)))

(biggest-palindrome-of-product-up-to 999)
