#lang racket

(require "array.rkt")

(define slice void)

(define (read-input port [paths '()] [row-max 0])
  (let ([line (read-line port)])
    (if (eof-object? line)
      (values paths (+ 2 row-max))
      (let rec ([lst (map string->number (string-split line #px",| -> "))] [p '()] [rm row-max])
        (match lst
          ['() (read-input port (cons p paths) rm)]
          [(list-rest col row rst) (rec rst (cons (list col row) p) (max row rm))])))))

(define (try-moves coord [moves (list (list 0 1) (list -1 1) (list 1 1))])
  (if (null? moves)
    #f
    (let ([new-coord (map + coord (car moves))])
      (if (eq? 'air (array-ref slice new-coord))
        new-coord
        (try-moves coord (cdr moves))))))

(define (drop-sand [coord (list 500 0)])
  (match (try-moves coord)
    [#f
     (array-set! slice coord 'sand)
     coord]
    [new-coord (drop-sand new-coord)]))

(define (count-sand row-limit n)
  (if (= (second (drop-sand)) row-limit)
    (add1 n)
    (count-sand row-limit (add1 n))))



(let-values ([(paths row-max) (call-with-input-file "inputs/day14" read-input)])
  (let ([col-min (- 500 row-max)] [col-max (+ 500 row-max)])

    (set! slice (make-array (list (cons col-min col-max) (cons 0 row-max)) 'air))

    (for ([p paths])
      (let add-path ([coords p])
        (match coords
          [(list _) void]
          [(list-rest (list col-a row-a) (list col-b row-b) _)
           (if (= row-a row-b)
             (for ([col (in-range (min col-a col-b) (add1 (max col-a col-b)))])
               (array-set! slice (list col row-a) 'rock))
             (for ([row (in-range (min row-a row-b) (add1 (max row-a row-b)))])
               (array-set! slice (list col-a row) 'rock)))
           (add-path (cdr coords))])))

    (for ([col (in-range col-min (add1 col-max))])
      (array-set! slice (list col row-max) 'rock))

    (let ([sand-units (count-sand (sub1 row-max) 0)])
      (printf
        "~a~%~a~%"
        (sub1 sand-units) ; last one drops to the bottom
        (count-sand 0 sand-units)))))

