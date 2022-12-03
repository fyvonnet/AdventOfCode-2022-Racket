#lang racket


(define (letter-priority letter)
  (cond
    [(and (char>=? letter #\a) (char<=? letter #\z))
     (- (char->integer letter) 96)]
    [(and (char>=? letter #\A) (char<=? letter #\Z))
     (- (char->integer letter) 38)]
    [#t (error (format "wrong character: ~c\n" letter))]))


(define (read-input port)
  (let ([line (read-line port)])
    (if (eof-object? line)
      '()
      (cons line (read-input port)))))

(define common-letters
  (lambda strs
    (letter-priority
      (first
        (set->list
          (apply set-intersect (map (lambda (s) (list->set (string->list s))) strs)))))))

(define (solve-part-one lst)
  (if (null? lst)
    0
    (+
      (letter-priority
        (first
          (match
            (map
              (lambda (str) (list->set (string->list str)))
              (let* 
                ([line (car lst)]
                 [halflen (/ (string-length line) 2)])
                (list
                  (substring line 0 halflen)
                  (substring line halflen (string-length line)))))
            [(list a b) (set->list (set-intersect a b))])))
      (solve-part-one (cdr lst)))))

(define (solve-part-two lst)
  (match lst
    ['() 0]
    [(list-rest a b c rst)
     (+ (common-letters a b c) (solve-part-two rst))]))

(let ([input (call-with-input-file "inputs/day03" read-input)])
  (displayln (solve-part-one input))
  (displayln (solve-part-two input)))

