#lang racket

(require "array.rkt")
(require "queue.rkt")

(define adjacent-coords
  (list
    (list  1  0  0)
    (list -1  0  0)
    (list  0  1  0)
    (list  0 -1  0)
    (list  0  0  1)
    (list  0  0 -1)))

(define (read-input port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      '()
      (cons
        (map string->number (string-split line ","))
        (read-input port)))))

(let*
  ([cubes (call-with-input-file "inputs/day18" read-input)]
   [scan
     (make-array 
       (for/fold
         ([mins (car cubes)]
          [maxs (car cubes)]
          #:result (map list (map sub1 mins) (map add1 maxs)))
         ([x (cdr cubes)])
         (values
           (map min x mins)
           (map max x maxs)))
       'air)])

  (for ([coord cubes])
    (array-set! scan coord 'cube))

  (let ([start-coord (map car (array-bounds scan))])
    (array-set! scan start-coord 'outside)
    (let rec ([queue (empty-queue start-coord)])
      (unless (queue-empty? queue)
        (let ([coord (queue-head queue)])
          (rec
            (foldl
              (lambda (m q)
                (let ([next-coord (map + coord m)])
                  (if (eq? 'air (array-ref-safe scan next-coord))
                    (begin
                      (array-set! scan next-coord 'outside)
                      (queue-snoc q next-coord))
                    q)))
              (queue-tail queue)
              adjacent-coords))))))

  (for/fold
    ([count1 0]
     [count2 0]
     #:result (printf "~a~%~a~%" count1 count2))
    ([coord cubes])
    (let rec ([c1 count1] [c2 count2] [acs adjacent-coords])
      (if (null? acs)
        (values c1 c2)
        (let ([new-coord (map + coord (car acs))])
          (let-values
            ([(new-c1 new-c2)
              (case (array-ref scan new-coord)
                ['air     (values (add1 c1)       c2 )]
                ['outside (values (add1 c1) (add1 c2))]
                ['cube    (values       c1        c2 )])])
            (rec new-c1 new-c2 (cdr acs))))))))

