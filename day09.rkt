#lang racket

(define (read-input port [head-coord '(0 0)] [tail-coords (list '(0 0))])
  (let ([line (read-line port)])
    (if (eof-object? line)
      (reverse tail-coords)
      (let
        ([move 
           (case (string-ref line 0)
             [(#\U) '( 0  1)] [(#\D) '( 0 -1)] [(#\L) '(-1  0)] [(#\R) '( 1  0)])]
         [steps (string->number (substring line 2))])
        (let rec ([s steps] [hc head-coord] [tcs tail-coords])
          (if (zero? s)
            (read-input port hc tcs)
            (let ([new-hc (map + hc move)])
              (rec (sub1 s) new-hc (cons (new-follower-coord new-hc (car tcs)) tcs)))))))))

(define (new-follower-coord leader follower)
  (match (map - leader follower)
    [(list x y)
     (map
       +
       follower
       (vector-ref
         (vector '(-1 +1) '(-1 +1) '( 0 +1) '(+1 +1) '(+1 +1)
                 '(-1 +1) '( 0  0) '( 0  0) '( 0  0) '(+1 +1)
                 '(-1  0) '( 0  0) '( 0  0) '( 0  0) '(+1  0)
                 '(-1 -1) '( 0  0) '( 0  0) '( 0  0) '(+1 -1)
                 '(-1 -1) '(-1 -1) '( 0 -1) '(+1 -1) '(+1 -1))
         (+ (* 5 (- 2 y)) (+ 2 x))))]))

(define (solve-part-two coords [followers (make-list 8 '(0 0))] [set (set)])
  (if (null? coords)
    (set-count set)
    (let rec ([new-fs (list (car coords))] [fs followers])
      (if (null? fs)
        (solve-part-two (cdr coords) (cdr (reverse new-fs)) (set-add set (car new-fs)))
        (rec (cons (new-follower-coord (car new-fs) (car fs)) new-fs) (cdr fs))))))

(let ([coords (call-with-input-file "inputs/day09" read-input)])
  (displayln (set-count (list->set coords)))
  (displayln (solve-part-two coords)))

