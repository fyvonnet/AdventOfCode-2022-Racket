#lang racket

(require "array.rkt")
(require control)

(define maze-map  0)
(define wrap-dest (make-hash))

(define (read-maze port)
  (let ([line (read-line port)])
    (if (non-empty-string? line)
      (cons line (read-maze port))
      '())))

(define (read-path str)
  (match (regexp-match #px"^(\\d+)([LR])(.*)$" str)
    [#f (list (string->number str))]
    [(list _ a b c)
     (cons
       (string->number a)
       (cons
         (match (string-ref b 0) [#\L #t] [#\R #f])
         (read-path c)))]))

(define (move path coord dir [n (car path)])
  (if (zero? n)
    (turn (cdr path) coord dir)
    (move
      path
      (let*
        ([adj-coord
           (case dir
             [(UP   ) (map + coord '( 0 -1))]
             [(DOWN ) (map + coord '( 0  1))]
             [(LEFT ) (map + coord '(-1  0))]
             [(RIGHT) (map + coord '( 1  0))])]
         [next-coord
           (if (char=? (array-ref maze-map adj-coord) #\Space)
             (hash-ref wrap-dest (list dir (case dir
                                             [(UP DOWN)    (first  adj-coord)]
                                             [(LEFT RIGHT) (second adj-coord)])))
             adj-coord)])
        (if (char=? (array-ref maze-map next-coord) #\#)
          coord
          next-coord))
      dir
      (sub1 n))))

(define (turn path coord dir)
  (if (null? path)
    (list coord dir)
    (move
      (cdr path)
      coord
      (let ([turn-left (car path)])
        (case dir
          [(UP   ) (if turn-left 'LEFT  'RIGHT)]
          [(DOWN ) (if turn-left 'RIGHT 'LEFT )]
          [(LEFT ) (if turn-left 'DOWN  'UP   )]
          [(RIGHT) (if turn-left 'UP    'DOWN )])))))

(let*
  ([port (open-input-file "inputs/day22")]
   [maze-strs (read-maze port)]
   [path (read-path (read-line port))]
   [maze-width (apply max (map string-length maze-strs))]
   [maze-height (length maze-strs)])

  (set! maze-map
    (make-array (list (+ 2 maze-width) (+ 2 maze-height)) #\Space))

  (for ([row maze-strs] [r (in-naturals 1)])
    (for ([chr row] [c (in-naturals 1)])
      (array-set! maze-map (list c r) chr)))

  (for ([r (in-range 1 (add1 maze-height))])
    (let ([cls 1] [crs (add1 maze-width)])
      (while (char=? #\Space (array-ref maze-map (list cls r)))
        (set! cls (add1 cls)))
      (while (char=? #\Space (array-ref maze-map (list crs r)))
        (set! crs (sub1 crs)))
      (hash-set! wrap-dest (list 'RIGHT r) (list cls r))
      (hash-set! wrap-dest (list 'LEFT  r) (list crs r))))

  (for ([c (in-range 1 (add1 maze-width))])
    (let ([rus 1] [rds (add1 maze-height)])
      (while (char=? #\Space (array-ref maze-map (list c rus)))
        (set! rus (add1 rus)))
      (while (char=? #\Space (array-ref maze-map (list c rds)))
        (set! rds (sub1 rds)))
      (hash-set! wrap-dest (list 'DOWN c) (list c rus))
      (hash-set! wrap-dest (list 'UP   c) (list c rds))))

  (match-let
    ([(list (list c r) d) (move path (hash-ref wrap-dest (list 'RIGHT 1)) 'RIGHT)])
    (displayln
      (+ (* 1000 r)
         (* 4 c)
         (case d [(RIGHT) 0] [(DOWN) 1] [(LEFT) 2] [(UP) 3])))))

