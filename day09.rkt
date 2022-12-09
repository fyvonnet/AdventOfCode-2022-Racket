#lang racket

(define (read-input port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      '()
      (cons
        (let ([d (string-ref line 0)])
          (cons
            (case d [(#\U) '( 0  1)] [(#\D) '( 0 -1)] [(#\L) '(-1  0)] [(#\R) '( 1  0)])
            (string->number (substring line 2))))
        (read-input port)))))

(define (get-tail-move rp)
  (match rp
    [(list x y)
     (vector-ref
       (vector '(99 99) '(-1 +1) '( 0 +1) '(+1 +1) '(99 99)
               '(-1 +1) '( 0  0) '( 0  0) '( 0  0) '(+1 +1)
               '(-1  0) '( 0  0) '( 0  0) '( 0  0) '(+1  0)
               '(-1 -1) '( 0  0) '( 0  0) '( 0  0) '(+1 -1)
               '(99 99) '(-1 -1) '( 0 -1) '(+1 -1) '(99 99))
       (+ (* 5 (- 2 y)) (+ 2 x)))]))


(define (solve input [set (set)] [head-coord '(0 0)] [tail-coord '(0 0)])
  (if (null? input)
    (length (set->list set))
    (match (car input)
      [(cons dir steps)
       (let rec ([n steps] [s set] [hc head-coord] [tc tail-coord])
         (if (zero? n)
           (solve (cdr input) s hc tc)
           (let*
             ([new-hc (map + dir hc)]
              [rel-pos (map - new-hc tc)]
              [new-tc (map + tc (get-tail-move rel-pos))])
             (rec (sub1 n) (set-add s new-tc) new-hc new-tc))))])))

(let ([input (call-with-input-file "inputs/day09" read-input)])
  (displayln (solve input)))

