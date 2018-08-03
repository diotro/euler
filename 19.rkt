#lang racket

#|
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4,
but not on a century unless it is divisible by 400.


How many Sundays fell on the first of the month during the
twentieth century (1 Jan 1901 to 31 Dec 2000)?

|#
(require math/number-theory)
(module+ test
  (require rackunit))

; This first, brute force attempt failed, but made me think about the math:

; what we need to keep track of:
; - day of week
; - day of month
; - month
; - year


(struct date [weekday day month year] #:transparent)

; January 1, 1901; a monday
(define START-DATE (date 1 1 1 1901))
; December 31, 2000; a sunday
(define END-DATE (date 7 31 12 2000))

(define MONTH-LENGTHS (hash 1 31
                            2 28
                            3 31
                            4 30
                            5 31
                            6 30
                            7 31
                            8 31
                            9 30
                            10 31
                            11 30
                            12 31))

(define LEAP-YEAR-MONTH-LENGTHS (hash-set MONTH-LENGTHS 2 29))


; count-sundays : Date Date -> Int
; how many sundays fell on the first of a month between the two dates?
(define (count-sundays start-date end-date)
  
  (define (is-sunday-and-first-of-month? date)
    (and (= (date-weekday date) 7)
         (= (date-day date) 1)))
  (cond [(equal? start-date end-date) 0]
        [else (+ (if (is-sunday-and-first-of-month? start-date) 1 0)
                 (count-sundays (increment-date start-date) end-date))]))

; increment-date : Date -> Date
; returns the next day
(define (increment-date the-date)
  (match-define (date dow day month year) the-date)
  (define month-lengths (if (is-leap-year? the-date)
                            MONTH-LENGTHS
                            LEAP-YEAR-MONTH-LENGTHS))
  (define month-length (hash-ref month-lengths month))
  (define next-dow (modulo (add1 dow) 7))
  (cond [(= day month-length) (if (= month 12)
                                  (date next-dow 1 1 (add1 year))
                                  (date next-dow 1 (add1 month) year))]
        [else (date next-dow (add1 day) month year)]))

(define (is-leap-year? date)
  (and (divides? 4 (date-year date))
       (or (not (divides? 100 (date-year date)))
           (divides? 400 (date-year date)))))


; ---------------------------------------------------------------------------------------------------
; You can also just calculate the day of the week on the first of each month

; MONTH-OFFSETS is a mapping from the month to the shift in the day of week of the first
; day of that month
(define MONTH-OFFSETS (for/hash ([(k v) (in-hash MONTH-LENGTHS)])
                        (values k (modulo v 7))))
(define LEAP-MONTH-OFFSETS (for/hash ([(k v) (in-hash LEAP-YEAR-MONTH-LENGTHS)])
                             (values k (modulo v 7))))

(struct date2 [month year] #:transparent)
(define (count-sundays.v2 start-date end-date)
  (define (count-sundays/acc cur-date cur-offset)
    (cond [(equal? cur-date end-date) 0]
          [else (+ (if (= cur-offset 0) 1 0)
                   (count-sundays/acc (next-month cur-date) (next-offset cur-date cur-offset)))]))
  (count-sundays/acc start-date 1))

(define (next-month d)
  (if (= (date2-month d) 12)
      (date2 1 (add1 (date2-year d)))
      (date2 (add1 (date2-month d)) (date2-year d))))

(module+ test
  (check-equal? (next-month (date2 1 1901)) (date2 2 1901))
  (check-equal? (next-month (date2 12 1901)) (date2 1 1902)))

(define (next-offset date2 offset)
  (define month-offsets (if (divides? 4 (date2-year date2)) LEAP-MONTH-OFFSETS MONTH-OFFSETS))
  (modulo (+ offset (hash-ref month-offsets (date2-month date2))) 7))

(module+ test
  (check-equal? (next-offset (date2 1 1901) 0) 3))

(count-sundays.v2 (date2 1 1901) (date2 1 2000))