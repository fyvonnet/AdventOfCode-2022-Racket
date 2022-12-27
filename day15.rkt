#lang racket

(require "array.rkt")

(define (read-input port [sensors '()] [beacons '()])
  (let ([line (read-line port)])
    (if (eof-object? line)
      (values sensors beacons)
      (match-let
        ([(list sensor-x sensor-y beacon-x beacon-y)
          (map
            string->number
            (cdr
              (regexp-match
                #px"Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)" line))) ])
        (read-input port (cons (cons sensor-x sensor-y) sensors) (cons (cons beacon-x beacon-y) beacons))))))

(define (manhattan-distance a b)
  (match-let ([(cons ax ay) a] [(cons bx by) b])
    (+ (abs (- ax bx)) (abs (- ay by)))))

(define (get-limits sensors manh-dists [limits '()])
  (if (null? sensors)
    limits
    (match-let ([(cons sensor-x sensor-y) (car sensors)])
      (let ([horiz-dist (- (car manh-dists) (abs (- sensor-y 2000000)))])
        (get-limits
          (cdr sensors)
          (cdr manh-dists)
          (if (negative? horiz-dist)
            limits
            (cons (cons (- sensor-x horiz-dist) (+ sensor-x horiz-dist)) limits)))))))

(let-values ([(sensors beacons) (call-with-input-file "inputs/day15" read-input)])
  (let*
    ([manh-dists (map manhattan-distance sensors beacons)]
     [limits (get-limits sensors manh-dists)]
     [limit-left  (apply min (map car  limits))]
     [limit-right (apply max (map cdr  limits))]
     [row (make-array (list (cons limit-left limit-right)) #f)])

    (for* ([l limits] [x (in-range (car l) (add1 (cdr l)))])
      (array-set! row (list x) #t))

    (for ([b beacons])
      (when (= 2000000 (cdr b))
        (array-set! row (list (car b)) #f)))

    (let ([counter 0])
      (for ([i (in-range limit-left (add1 limit-right))])
        (when (array-ref row (list i))
          (set! counter (add1 counter))))
      (displayln counter))))

