#lang racket

(require "queue.rkt")
(require "matrix.rkt")

(define (read-input port [row 0] [start #f] [end #f] [heights '()])
  (let ([line (read-line port)])
    (if (eof-object? line)
      (values (list->matrix (reverse heights)) start end)
      (let rec ([lst (string->list line)] [s start] [e end] [col 0] [hs '()])
        (if (null? lst)
          (read-input port (add1 row) s e (cons (reverse hs) heights))
          (match (car lst)
            [#\S (rec (cdr lst) (list col row) e (add1 col) (cons 0 hs))]
            [#\E (rec (cdr lst) s (list col row) (add1 col) (cons 26 hs))]
            [c (rec (cdr lst) s e (add1 col) (cons (- (char->integer c) 97) hs))]))))))

(define (fill-queue heightmap not-visited pos height steps)
  (lambda (move queue)
    (match (map + pos move)
      [(list c r)
       (match (matrix-ref-safe heightmap c r #f)
         [#f queue]
         [new-height
           (if (and (<= new-height (add1 height)) (matrix-ref not-visited c r))
             (begin
               (matrix-set! not-visited c r #f)
               (queue-snoc queue (list (list c r) (add1 steps) new-height)))
             queue)])])))

(let-values
  ([(heightmap start end) (call-with-input-file "inputs/day12" read-input)])
  (let ([not-visited (make-matrix (matrix-cols heightmap) (matrix-rows heightmap) #t)])
    (displayln
      (let rec ([queue (queue-snoc (empty-queue) (list start 0 0))])
        (match (queue-head queue)
          [(list pos steps height)
           (if (equal? pos end)
             steps
             (rec
               (foldl
                 (fill-queue heightmap not-visited pos height steps)
                 (queue-tail queue)
                 (list '(1 0) '(-1 0) '(0 -1) '(0 1)))))])))))

