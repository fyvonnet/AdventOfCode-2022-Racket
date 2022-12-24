#lang racket

(define NORTHWEST 0)
(define NORTH     1)
(define NORTHEAST 2)
(define WEST      3)
(define EAST      4)
(define SOUTHWEST 5)
(define SOUTH     6)
(define SOUTHEAST 7)

(define moves
  (vector
    '(-1 -1) '( 0 -1) '( 1 -1)
    '(-1  0)          '( 1  0)
    '(-1  1) '( 0  1) '( 1  1)))

(define proposals 
  (vector
    (list NORTH NORTHEAST NORTHWEST)
    (list SOUTH SOUTHEAST SOUTHWEST)
    (list WEST  NORTHWEST SOUTHWEST)
    (list EAST  NORTHEAST SOUTHEAST)))

(define (read-input port)
  (let ([line (read-line port)] [elf-coords '()])
    (for ([r (in-naturals)]
          #:break (eof-object? line))
      (for ([chr line] [c (in-naturals)])
        (when (char=? #\# chr) (set! elf-coords (cons (list c r) elf-coords))))
      (set! line (read-line port)))
    elf-coords))

(define (propose-moves elves prop-order)
  (let ([elves-set (list->set elves)])
    (map
      (lambda (e)
        (let ([around (vector-map (lambda (m) (not (set-member? elves-set (map + e m)))) moves)])
          (if (andmap identity (vector->list around))
            e
            (let rec ([ps (map (lambda (i) (vector-ref proposals i)) prop-order)])
              (cond
                [(null? ps) e]
                [(andmap (lambda (m) (vector-ref around m)) (car ps)) (map + e (vector-ref moves (caar ps)))]
                [else (rec (cdr ps))])))))
      elves)))

(define (do-moves elves prop-order)
  (let*
    ([proposed-moves (propose-moves elves prop-order)]
     [hash
       (foldl
         (lambda (pm h) (hash-set h pm (add1 (hash-ref h pm 0))))
         (make-immutable-hash)
         proposed-moves)])
    (for/list
      ([e elves] [pm proposed-moves])
      (match (hash-ref hash pm 0) [0 (error "zero")] [1 pm] [_ e]))))

(define (run-rounds n elves [prop-order '(0 1 2 3)])
  (if (zero? n)
    (values elves prop-order)
    (run-rounds
      (sub1 n)
      (do-moves elves prop-order)
      (map (lambda (x) (modulo (add1 x) 4)) prop-order))))

(define (run-rounds-until-no-moves elves n prop-order)
  (let ([new-elves (do-moves elves prop-order)])
    (if (andmap equal? elves new-elves)
      n
      (run-rounds-until-no-moves
        new-elves
        (add1 n)
        (map (lambda (x) (modulo (add1 x) 4)) prop-order)))))

(let ([elves (call-with-input-file "inputs/day23" read-input)])
  (let*-values
    ([(end-state prop-order) (run-rounds 10 elves)]
     [(min-x max-x min-y max-y)
      (for/fold
        ([min-x (caar  end-state)]
         [max-x (caar  end-state)]
         [min-y (cadar end-state)]
         [max-y (cadar end-state)])
        ([coord (cdr end-state)])
        (match-let ([(list x y) coord])
          (values (min x min-x) (max x max-x) (min y min-y) (max y max-y))))])
    (let ([end-state-set (list->set end-state)])
      (println
        (for*/fold
          ([counter 0])
          ([x (range min-x (add1 max-x))]
           [y (range min-y (add1 max-y))])
          (if (set-member? end-state-set (list x y))
            counter
            (add1 counter)))))
    (println (run-rounds-until-no-moves end-state 11 prop-order))))

