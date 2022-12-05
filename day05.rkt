#lang racket

(define (read-stacks port [lines '()])
  (let ([line (read-line port)])
    (if (char=? #\[ (string-ref line 0))
      (read-stacks port (cons line lines))
      (let*
        ([nstacks
           (let rec ([chars (string->list (car lines))] [c 0])
             (if (null? chars)
               c
               (rec
                 (cdr chars)
                 (if (char=? (car chars) #\[)
                   (add1 c)
                   c))))]
         [stacks (make-vector nstacks '())])
        (for ([l lines])
          (for ([i (in-range nstacks)])
            (let
              ([stack (vector-ref stacks i)]
               [crate (string-ref l (add1 (* 4 i)))])
              (unless (char=? crate #\space)
                (vector-set! stacks i (cons crate stack))))))
        stacks))))

(define (read-procedure port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      null
      (match (map string->number (cdr (regexp-match #px"move (\\d+) from (\\d+) to (\\d+)" line)))
        [(list a b c)
         (cons (list a (sub1 b) (sub1 c)) (read-procedure port))]))))

(define (move-crates-one n orig-stack dest-stack)
  (if (zero? n)
    (values orig-stack dest-stack)
    (move-crates-one (sub1 n) (cdr orig-stack) (cons (car orig-stack) dest-stack))))

(define (move-crates-two n orig-stack dest-stack)
  (let-values ([(taken left) (split-at orig-stack n)])
    (values left (append taken dest-stack))))

(define (apply-procedure procedure stacks move-crates)
  (for ([step procedure])
    (match step
      [(list n o d)
       (let-values
         ([(new-orig-stack new-dest-stack)
           (move-crates n (vector-ref stacks o) (vector-ref stacks d))])
         (vector-set! stacks o new-orig-stack)
         (vector-set! stacks d new-dest-stack))]))
  (list->string (map car (vector->list stacks))))

(let*
  ([port (open-input-file "inputs/day05")]
   [stacks-one (read-stacks port)]
   [stacks-two (vector-copy stacks-one)]
   [_ (read-line port)] ; skip empty line
   [procedure (read-procedure port)])
  (for
    ([s (list stacks-one stacks-two)]
     [mc (list move-crates-one move-crates-two)])
    (displayln (apply-procedure procedure s mc))))

