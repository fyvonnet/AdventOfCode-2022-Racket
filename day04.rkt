#lang racket

(define (read-input port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      '()
      (cons
        (map string->number (cdr (regexp-match #rx"([0-9]*)-([0-9]*),([0-9]*)-([0-9]*)" line)))
        (read-input port)))))

(define (overlap-part-one? a b c d)
  (or
    (and (<= a c) (<= d b))
    (and (<= c a) (<= b d))))

(define (overlap-part-two? a b c d)
  (not
    (or
      (and (<= a b) (< b c) (<= c d))
      (and (<= c d) (< d a) (<= a b)))))

(define (solve input func)
  (foldl
    (lambda (ranges count)
      (match ranges
        [(list a b c d)
         (if (func a b c d)
           (add1 count)
           count)]))
    0
    input))

(let ([input (call-with-input-file "inputs/day04" read-input)])
  (displayln (solve input overlap-part-one?))
  (displayln (solve input overlap-part-two?)))

