#lang racket

(define points (vector 4 1 7 8 5 2 3 9 6))
(define player-rounds (vector 2 0 1 0 1 2 1 2 0))

(define (read-input port [input1 '()] [input2 '()])
  (let ([line (read-line port)])
    (if (eof-object? line )
      (list input1 input2)
      (let* 
        ([opponent-round (case (string-ref line 0) ((#\A) 0) ((#\B) 1) ((#\C) 2))]
         [second-column  (case (string-ref line 2) ((#\X) 0) ((#\Y) 1) ((#\Z) 2))]
         [player-round (vector-ref player-rounds (+ (* 3 second-column) opponent-round))])
        (read-input
          port
          (cons (cons opponent-round second-column) input1)
          (cons (cons opponent-round player-round) input2))))))

(for ([input (call-with-input-file "inputs/day02" read-input)])
  (displayln 
    (foldl
      (lambda (round score)
        (match round
          ((cons o p) (+ score (vector-ref points (+ (* 3 p) o))))))
      0
      input)))

