#lang racket
#|

If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there
 are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
 how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
The use of "and" when writing out numbers is in compliance with British usage.

|#
(require rackunit)


; number-letter-count : Integer -> Integer
; how many letters are used when counting from 1 to n?
(define (number-letter-count n)
  (foldr + 0 (map string-length
                  (map remove-spaces-and-hyphens
                       (map number->english
                            (build-list n add1))))))

(module+ test
  (check-equal? (number-letter-count 5) 19))


; Int -> String
; how would you write the given int in english?
(define (number->english n)
  (define (digit->english digit)
    (cond [(= digit 1) "one"]
          [(= digit 2) "two"]
          [(= digit 3) "three"]
          [(= digit 4) "four"]
          [(= digit 5) "five"]
          [(= digit 6) "six"]
          [(= digit 7) "seven"]
          [(= digit 8) "eight"]
          [(= digit 9) "nine"]
          [(= digit 0) ""]))

  (define (under-20->english n)
    (cond [(= n 10) "ten"]
          [(= n 11) "eleven"]
          [(= n 12) "twelve"]
          [(= n 13) "thirteen"]
          [(= n 14) "fourteen"]
          [(= n 15) "fifteen"]
          [(= n 16) "sixteen"]
          [(= n 17) "seventeen"]
          [(= n 18) "eighteen"]
          [else "nineteen"]))

  (define (tens-word n)
    (cond [(< n 30) "twenty"]
          [(< n 40) "thirty"]
          [(< n 50) "forty"]
          [(< n 60) "fifty"]
          [(< n 70) "sixty"]
          [(< n 80) "seventy"]
          [(< n 90) "eighty"]
          [else "ninety"]))
  
  (define (tens-part->english n)
    (cond
      [(< n 10) (digit->english n)]
      [(< n 20) (under-20->english n)]
      [else (define digit-str (digit->english (remainder n 10)))
            (define tens-part (remainder n 100))
            (if (< tens-part 20)
                (under-20->english tens-part)
                (string-append
                 (string-append (tens-word tens-part))
                 (if (string=? digit-str "")
                     ""
                     (string-append "-" digit-str))))]))
    

  (define (hundreds-part->english n)
    (cond [(< n 100) ""]
          [else (string-append (digit->english (quotient n 100)) " hundred")]))

  (cond [(= n 1000) "one thousand"]
        [(= 0 (modulo n 100)) (hundreds-part->english n)]
        [else
         (define h (hundreds-part->english n))
         (string-append h (if (string=? h "") "" " and ")
                        (tens-part->english (remainder n 100)))]))

(module+ test
  (check-equal? (number->english 3)    "three")
  (check-equal? (number->english 13)   "thirteen")
  (check-equal? (number->english 12)   "twelve")
  (check-equal? (number->english 32)   "thirty-two")
  (check-equal? (number->english 99)   "ninety-nine")
  (check-equal? (number->english 100)   "one hundred")
  (check-equal? (number->english 115)  "one hundred and fifteen")
  (check-equal? (number->english 123)  "one hundred and twenty-three")
  (check-equal? (number->english 323)  "three hundred and twenty-three")
  (check-equal? (number->english 329)  "three hundred and twenty-nine")
  (check-equal? (number->english 330)  "three hundred and thirty")
  (check-equal? (number->english 1000) "one thousand")
  (check-equal? (number->english 555)  "five hundred and fifty-five"))



(define (remove-spaces-and-hyphens str)
  (string-replace (string-replace str "-" "") " " ""))



; Answer:
(number-letter-count 1000)