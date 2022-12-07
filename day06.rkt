#lang racket

(define (solve lst [n 4])
  (match lst
    [(list-rest a b c d _) 
     (if
       (and 
         (not (char=? a b))
         (not (char=? a c))
         (not (char=? a d))
         (not (char=? b c))
         (not (char=? b d))
         (not (char=? c d)))
       (begin
         (printf "~c~c~c~c~%" a b c d)
       n)
       (solve (cdr lst) (add1 n)))]))

(define (all-different-chars? lst)
  (match lst
    [(list _) #t]
    [(list-rest fst rst)
     (if
       (let rec ([lst2 (cdr lst)])
         (if (null? lst2)
           #t
           (if (char=? (car lst) (car lst2))
             #f
             (rec (cdr lst2)))))
       (all-different-chars? (cdr lst))
       #f)]))

(define (solve2 lst [n 14])
  (if (all-different-chars? (take lst 14))
    n
    (solve2 (cdr lst) (add1 n))))



;(let ([input (call-with-input-file "inputs/day06-test" read-line)])
(let ([input (call-with-input-file "inputs/day06" read-line)])
  (println (solve (string->list input)))
  (println (solve2 (string->list input)))
  )
