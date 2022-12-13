#lang racket

(define (compare-lists left right)
  (cond
    [(and (null?  left) (not (null? right))) #t]
    [(and (null? right) (not (null?  left))) #f]
    [(and (null? left) (null? right)) 0]
    [else
      (match-let
        ([(list-rest fst-left  rst-left ) left ]
         [(list-rest fst-right rst-right) right])
        (match
          (if (list? fst-left)
            (if (list? fst-right)
              (compare-lists fst-left fst-right)
              (compare-lists fst-left (list fst-right)))
            (if (list? fst-right)
              (compare-lists (list fst-left) fst-right)
              (cond
                [(< fst-left fst-right) #t]
                [(> fst-left fst-right) #f]
                [else 0])))
          [0 (compare-lists rst-left rst-right)]
          [b b]))]))

(define (solve lst [n 1])
  (match lst
    ['() 0]
    [(list-rest (list l r) rst)
     (+ (if (compare-lists l r) n 0) (solve rst (add1 n)))]))

(define (read-input port)
  (cons
    (map
      (lambda (str) (read (open-input-string (string-replace str "," " "))))
      (list (read-line port) (read-line port)))
    (if (eof-object? (read-line port))
      '()
      (read-input port))))

(displayln (solve (call-with-input-file "inputs/day13" read-input)))

