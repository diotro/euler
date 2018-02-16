#lang racket

#|

Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over
five-thousand first names, begin by sorting it into alphabetical order. Then working out the
alphabetical value for each name, multiply this value by its alphabetical position in the list
 to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth

    3 + 15 + 12 + 9 + 14 = 53

is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
What is the total of all the name scores in the file?
|#

(define NAMES (first (call-with-input-file "names-22.txt"
                       (λ (port) (port->list read-line port)))))


(define CLEAN-NAMES (string-split (substring NAMES 1 (sub1 (string-length NAMES))) "\",\""))



; [List-of String] -> N
; produces the sum of the name scores in the file
(define (name-total names)
  (define sorted-names (sort names string<?))
  (define (name-score name)
    (* (alpha-score name) (add1 (index-of sorted-names name))))

  (define (alpha-score name)
    (foldr + 0 (map (λ (char) (- (char->integer char) 64)) (string->list name))))
  (foldr + 0 (map name-score names)))


(name-total CLEAN-NAMES)