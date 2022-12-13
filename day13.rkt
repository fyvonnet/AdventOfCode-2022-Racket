#lang racket

(define (compare-packets left right)
  (let ([null-left (null? left)] [null-right (null? right)])
  (cond
    [(and null-left  (not null-right)) #t]
    [(and null-right (not null-left )) #f]
    [(and null-left null-right) 0]
    [else
      (match-let
        ([(list-rest fst-left  rst-left ) left ]
         [(list-rest fst-right rst-right) right])
        (match
          (if (list? fst-left)
            (if (list? fst-right)
              (compare-packets fst-left fst-right)
              (compare-packets fst-left (list fst-right)))
            (if (list? fst-right)
              (compare-packets (list fst-left) fst-right)
              (cond
                [(< fst-left fst-right) #t]
                [(> fst-left fst-right) #f]
                [else 0])))
          [0 (compare-packets rst-left rst-right)]
          [b b]))])))

(define (solve-part-one packets [n 1])
  (match packets
    ['() 0]
    [(list-rest left right rst)
     (+
       (if (compare-packets left right) n 0)
       (solve-part-one rst (add1 n)))]))

(define (evaluate str)
  (read (open-input-string (string-replace str "," " "))))

(define (read-input port [packets null])
  (let ([line (read-line port)])
    (cond
      [(eof-object? line) (reverse packets)]
      [(non-empty-string? line) (read-input port (cons (evaluate line) packets))]
      [else (read-input port packets)])))

(let*
  ([packets (call-with-input-file "inputs/day13" read-input)]
   [divider-packets (map evaluate '("[[2]]" "[[6]]"))]
   [packets-vec (list->vector (sort (append divider-packets packets) compare-packets))])
  (printf
    "~a~%~a~%"
    (solve-part-one packets)
    (apply * (map (λ (p) (add1 (vector-member p packets-vec))) divider-packets))))

