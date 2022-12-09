#lang racket

(define (read-input port [coords (list '(0 0))])
  (let ([line (read-line port)])
    (if (eof-object? line)
      (reverse coords)
      (let
        ([move 
           (case (string-ref line 0)
             [(#\U) '( 0  1)] [(#\D) '( 0 -1)] [(#\L) '(-1  0)] [(#\R) '( 1  0)])]
         [steps (string->number (substring line 2))])
        (let rec ([s steps] [cs coords])
          (if (zero? s)
            (read-input port cs)
            (rec (sub1 s) (cons (map + (car cs) move) cs))))))))

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

(define (new-follower-coord leader follower)
  (let ([rel-pos (map - leader follower)])
    (map + follower (get-tail-move rel-pos))))

(let ([head-coords (call-with-input-file "inputs/day09" read-input)])
  (for/fold
    ([tail-coord '(0 0)]
     [coords-set (set '(0 0))]
     #:result (length (set->list coords-set)))
    ([head-coord head-coords])
    (let ([new-tail-coord (new-follower-coord head-coord tail-coord)])
      (values new-tail-coord (set-add coords-set new-tail-coord)))))
