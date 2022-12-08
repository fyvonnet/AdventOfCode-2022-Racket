#lang racket

(define mtx-width  0)
(define mtx-height 0)

(define (mtx-index r c)
  (+ (* r mtx-width) c))

(define (mtx-set! mtx r c val)
  (vector-set! mtx (mtx-index r c) val))

(define (mtx-ref mtx r c)
  (vector-ref mtx (mtx-index r c)))

(define (read-input port [lst null] [n 0])
  (let ([line (read-line port)])
    (if (eof-object? line)
      (begin
        (set! mtx-width (/ (length lst) n))
        (set! mtx-height n)
        (list->vector (map (lambda (c) (- (char->integer c) 48)) lst)))
      (read-input port (append lst (string->list line)) (add1 n)))))

(let*
  ([input (call-with-input-file "inputs/day08" read-input)]
   [visible (make-vector (vector-length input) #f)])

  (for ([r (in-range mtx-height)])
    (mtx-set! visible r 0 #t)
    (mtx-set! visible r (sub1 mtx-width) #t))

  (for ([c (in-range mtx-width)])
    (mtx-set! visible 0 c #t)
    (mtx-set! visible (sub1 mtx-height) c #t))

  (for ([r (in-range 1 (sub1 mtx-height))])
    (let ([tallest (mtx-ref input r 0)])
      (for
        ([c (in-range 1 (sub1 mtx-width))])
        (when (> (mtx-ref input r c) tallest)
          (mtx-set! visible r c #t)
          (set! tallest (mtx-ref input r c)))))
    (let ([tallest (mtx-ref input r (sub1 mtx-width))])
      (for
        ([c (in-range (- mtx-width 2) -1 -1)])
        (when (> (mtx-ref input r c) tallest)
          (mtx-set! visible r c #t)
          (set! tallest (mtx-ref input r c))))))

  (for ([c (in-range 1 (sub1 mtx-width))])
    (let ([tallest (mtx-ref input 0 c)])
      (for
        ([r (in-range 1 (sub1 mtx-height))])
        (when (> (mtx-ref input r c) tallest)
          (mtx-set! visible r c #t)
          (set! tallest (mtx-ref input r c)))))
    (let ([tallest (mtx-ref input (sub1 mtx-height) c)])
      (for
        ([r (in-range (- mtx-height 2) -1 -1)])
        (when (> (mtx-ref input r c) tallest)
          (mtx-set! visible r c #t)
          (set! tallest (mtx-ref input r c))))))

  (displayln (length (filter identity (vector->list visible))))

  (let ([highest-score 0])
    (for* 
      ([tree-row (in-range 1 (sub1 mtx-height))] [tree-col (in-range 1 (sub1 mtx-width))])
      (let ([tree-height (mtx-ref input tree-row tree-col)])
        (set! highest-score
          (max
            highest-score
            (*
              ; up
              (let rec ([rows (in-range (sub1 tree-row) -1 -1)] [score 0])
                (cond
                  [(stream-empty? rows) score]
                  [(>= (mtx-ref input (stream-first rows) tree-col) tree-height) (add1 score)]
                  [#t (rec (stream-rest rows) (add1 score))]))
              ; down
              (let rec ([rows (in-range (add1 tree-row) mtx-height 1)] [score 0])
                (cond
                  [(stream-empty? rows) score]
                  [(>= (mtx-ref input (stream-first rows) tree-col) tree-height) (add1 score)]
                  [#t (rec (stream-rest rows) (add1 score))]))
              ; left
              (let rec ([cols (in-range (sub1 tree-col) -1 -1)] [score 0])
                (cond
                  [(stream-empty? cols) score]
                  [(>= (mtx-ref input tree-row (stream-first cols)) tree-height) (add1 score)]
                  [#t (rec (stream-rest cols) (add1 score))]))
              ; right
              (let rec ([cols (in-range (add1 tree-col) mtx-width 1)] [score 0])
                (cond
                  [(stream-empty? cols) score]
                  [(>= (mtx-ref input tree-row (stream-first cols)) tree-height) (add1 score)]
                  [#t (rec (stream-rest cols) (add1 score))])))))))
    (displayln highest-score)))

