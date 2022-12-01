#lang racket

(define (read-input port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      '()
      (cons
        (string->number line)
        (read-input port)))))

(match
  (for/fold
    ([acc 0] [lst '()] #:result (sort (cons acc lst) >))
    ([i (call-with-input-file "inputs/day01" read-input)])
    (match i
      [#f (values 0 (cons acc lst))]
      [n (values (+ acc n) lst)]))
  [(list-rest a b c _) (printf "~a~%~a~%" a (+ a b c))])

