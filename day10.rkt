#lang racket


(define (solve port [n 0] [add 0] [x 1] [cycle 0] [cycles '(20 60 100 140 180 220)] [strength 0])
  (if (null? cycles)
    strength
    (let ([new-cycle (add1 cycle)])
      (let-values
        ([(new-cycles new-strength)
          (if (= cycle (car cycles))
            (values (cdr cycles) (+ strength (* (car cycles) x)))
            (values cycles strength))])
        (if (zero? n)
          (let
            ([line (read-line port)]
             [new-x (+ x add)])
            (case (string-ref line 0)
              [(#\n) (solve port 0 0 new-x new-cycle new-cycles new-strength)]
              [(#\a) 
               (let ([val (string->number (substring line 5))])
                 (solve port 1 val new-x new-cycle new-cycles new-strength))]))
          (solve port (sub1 n) add x new-cycle new-cycles new-strength))))))

(displayln (call-with-input-file "inputs/day10" solve))

