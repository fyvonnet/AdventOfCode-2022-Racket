#lang racket


(define (read-input port [n 0] [add 0] [x 1] [lst '()])
  (if (zero? n)
    (let
      ([line (read-line port)]
       [new-x (+ x add)])
      (if (eof-object? line)
        (reverse lst)
        (case (string-ref line 0)
          [(#\n) (read-input port 0 0 new-x (cons new-x lst))]
          [(#\a) 
           (let ([val (string->number (substring line 5))])
             (read-input port 1 val new-x (cons new-x lst)))])))
    (read-input port (sub1 n) add x (cons x lst))))

(let*
  ([input (call-with-input-file "inputs/day10" read-input)]
   [input-vec (list->vector input)])
  (displayln (apply + (map (lambda (x) (* x (vector-ref input-vec (sub1 x)))) '(20 60 100 140 180 220))))
  (for ([sprite-pos input] [c (in-range 240)])
    (let ([hpos (remainder c 40)])
      (display (if (and (>= hpos (sub1 sprite-pos)) (<= hpos (add1 sprite-pos))) "##" "  "))
      (when (= 39 hpos) (display "\n")))))

