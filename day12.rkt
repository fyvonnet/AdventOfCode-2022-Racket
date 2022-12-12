#lang racket

(require "queue.rkt")
(require "matrix.rkt")

(define (read-input port [row 0] [start #f] [end #f] [heights '()] [start-points '()])
  (let ([line (read-line port)])
    (if (eof-object? line)
      (values (list->matrix (reverse heights)) start end start-points)
      (let rec ([lst (string->list line)] [s start] [e end] [col 0] [hs '()] [sp start-points])
        (if (null? lst)
          (read-input port (add1 row) s e (cons (reverse hs) heights) sp)
          (let-values 
            ([(new-s new-e new-hs new-sp)
              (let
                ([coord (list col row)])
                (match (car lst)
                  [#\S (values coord e (cons  0 hs) (cons coord sp))]
                  [#\E (values s coord (cons 26 hs) sp)]
                  [c   (values s e (cons (- (char->integer c) 97) hs) (if (char=? c #\a) (cons coord sp) sp))]))])
            (rec (cdr lst) new-s new-e (add1 col) new-hs new-sp)))))))

(define (fill-queue heightmap not-visited pos height steps trail)
  (lambda (move queue)
    (let
      ([new-pos (map + pos move)])
      (match new-pos
        [(list c r)
         (match (matrix-ref-safe heightmap c r #f)
           [#f queue]
           [new-height
             (if (and (<= new-height (add1 height)) (matrix-ref not-visited c r))
               (begin
                 (matrix-set! not-visited c r #f)
                 (queue-snoc queue (list new-pos (add1 steps) new-height (cons new-pos trail))))
               queue)])]))))

(define (reach-end heightmap end)
  (lambda (start) 
    (let
      ([not-visited (make-matrix (matrix-cols heightmap) (matrix-rows heightmap) #t)])
      (let rec ([queue (queue-snoc (empty-queue) (list start 0 0 '()))])
        (match (queue-head queue)
          [(list pos steps height trail)
           (if (equal? pos end)
             steps
             (let
               ([new-queue
                  (foldl
                    (fill-queue heightmap not-visited pos height steps trail)
                    (queue-tail queue)
                    (list '(1 0) '(-1 0) '(0 -1) '(0 1)))])
               (if (queue-empty? new-queue)
                 #f
                 (rec new-queue))))])))))

(let-values
  ([(heightmap start end start-points) (call-with-input-file "inputs/day12" read-input)])
  (let* 
    ([func (reach-end heightmap end)]
     [answer1 (func start)])
    (displayln answer1)
    (displayln
      (foldl
        (lambda (s m) (match (func s) [#f m] [n (min m n)]))
        answer1
        start-points))))

