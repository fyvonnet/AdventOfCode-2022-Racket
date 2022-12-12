#lang racket

(struct monkey ([items #:mutable] operation test if-true if-false [times #:mutable]))

(define (lower-worry level)
  (floor (/ level 3)))

(define (read-input port)
  (read-line port)
  (cons
    (monkey
      (let ([line (read-line port)])
        (map string->number (string-split (substring line 18) ", ")))
      (let*
        ([line (read-line port)]
         [operator (case (string-ref line 23) [(#\+) +] [(#\*) *])])
        (match (string->number (substring line 25))
          [#f (lambda (old) (lower-worry (operator old old)))]
          [n  (lambda (old) (lower-worry (operator old n)))]))
      (let
        ([divisor  (string->number (substring (read-line port) 21))])
        (lambda (n) (zero? (remainder n divisor))))
      (string->number (substring (read-line port) 29))
      (string->number (substring (read-line port) 30))
      0)
    (let ([line (read-line port)])
      (if (eof-object? line)
        '()
        (read-input port))))) 

(define (give-to-monkey monkeys i items)
  (let ([dest-monkey (vector-ref monkeys i)])
    (set-monkey-items! dest-monkey (append (monkey-items dest-monkey) items))))

(let ([monkeys (list->vector (call-with-input-file "inputs/day11" read-input))])
  (for*
    ([_ (in-range 20)]
     [m (in-range (vector-length monkeys))])
  ;(let ([m 0])
    (let ([this-monkey (vector-ref monkeys m)])
      (set-monkey-times! this-monkey (+ (length (monkey-items this-monkey)) (monkey-times this-monkey)))
      (let-values
        ([(items-true items-false)
          (partition (monkey-test this-monkey) (map (monkey-operation this-monkey) (monkey-items this-monkey)))])
        (set-monkey-items! this-monkey '())
        (give-to-monkey monkeys (monkey-if-true  this-monkey) items-true)
        (give-to-monkey monkeys (monkey-if-false this-monkey) items-false))))
  (let ([sorted (vector-sort (vector-map monkey-times monkeys) >)])
    (displayln (* (vector-ref sorted 0) (vector-ref sorted 1)))))

