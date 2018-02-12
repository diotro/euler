#lang racket

#|
By starting at the top of the triangle below and moving to adjacent numbers on the row below,
the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:


Problem 67 is the same, with a different triangle, so I'll solve both
|#

(require rackunit)

; The clever idea here is to build a tree, and just apply max of each child and add the
; current number.

(define LINES-18 (call-with-input-file "triangle-18.txt"
                (λ (port) (port->lines port))))
(define LINES-67 (call-with-input-file "triangle-67.txt"
                (λ (port) (port->lines port))))

(define (lines->triangle lines)
  (reverse (map (λ (str) (map string->number (string-split str " "))) lines)))


(define TRIANGLE-18 (lines->triangle LINES-18))
(define TRIANGLE-67 (lines->triangle LINES-67))




(define (max-path-sum triangle)
  (cond [(one-num-left? triangle) (first (first triangle))]
        [else
         (define (collapse-bottom-row triangle)
           (define bottom (first triangle))
           (define cur (second triangle))
           (cons (build-list (length cur) (λ (index) (+ (list-ref cur index)
                                                  (max (list-ref bottom index)
                                                       (list-ref bottom (add1 index))))))
                 (rest (rest triangle))))
         (max-path-sum (collapse-bottom-row triangle))]))

(define (one-num-left? triangle) (empty? (rest triangle)))

(module+ test
  (check-equal? (max-path-sum '((2 1) (0))) 2)
  (check-equal? (max-path-sum '((3 2 1)
                                (1 10)
                                (1))) 13))

; Answer for 18:
(max-path-sum TRIANGLE-18)

; Answer for 67
(max-path-sum TRIANGLE-67)