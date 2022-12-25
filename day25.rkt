#lang racket


(define sums    (vector  0  1  2 -2 -1  0  1  2 -2 -1  0))
(define carries (vector -1 -1 -1  0  0  0  0  0  1  1  1))

(define (snafu-car p)
  (if (null? p) 0 (car p)))

(define (snafu-cdr p)
  (if (null? p) '() (cdr p)))

(define (string->snafu str)
  (let rec ([lst (string->list str)] [snafu '()])
    (if (null? lst)
      snafu
      (rec
        (cdr lst)
        (cons
          (match (car lst)
            [#\= -2]
            [#\- -1]
            [#\0  0]
            [#\1  1]
            [#\2  2])
          snafu)))))

(define (read-input port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      '()
      (cons
        (string->snafu line)
        (read-input port)))))

(define (snafu+ a b [carry 0])
  (if (and (null? a) (null? b))
    (if (zero? carry)
      '()
      (list carry))
    (let ([i (+ 5 (snafu-car a) (snafu-car b) carry)])
      (cons
        (vector-ref sums i)
        (snafu+
          (snafu-cdr a)
          (snafu-cdr b)
          (vector-ref carries i))))))

(define (snafu->string snafu)
  (list->string
    (let rec ([lst snafu] [outlst '()])
      (if (null? lst)
        outlst
        (rec
          (cdr lst)
          (cons
            (string-ref "=-012" (+ 2 (car lst)))
            outlst))))))

(displayln
  (snafu->string
    (foldl
      snafu+
      (string->snafu "0")
      (call-with-input-file "inputs/day25" read-input))))

