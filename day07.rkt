#lang racket

(define total-space  70000000)
(define needed-space 30000000)

(define total-small-dirs 0)
(define dirs-sizes null)

(define (end-of-dir sum)
  (when (< sum 100000)
    (set! total-small-dirs (+ total-small-dirs sum)))
  (set! dirs-sizes (cons sum dirs-sizes))
  sum)

(define (browse-fs port)
  (read-line port) ; skip ls
  (let rec ([sum 0])
    (let*
      ([line (read-line port)])
      (if (eof-object? line)
        (end-of-dir sum)
        (let ([c (string-ref line 0)])
          (cond
            [(char=? c #\d) (rec sum)] ; skip dirs
            [(char-numeric? c)
             (rec (+ sum (string->number (car (string-split line)))))]
            [(char=? c #\$)
             (if (char=? #\. (string-ref line 5))
               (end-of-dir sum)
               (rec (+ sum (browse-fs port))))]))))))

(let ([port (open-input-file "inputs/day07")])
  (read-line port) ; skip cd /
  (let*
    ([total-used (browse-fs port)]
     [unused-space (- total-space total-used)]
     [need-to-free (- needed-space unused-space)])
    (displayln total-small-dirs)
    (displayln 
      (foldl
        (lambda (x r) (if (>= x need-to-free) (min x r) r))
        total-space ; let's assume no directory is larger than the disk
        dirs-sizes))))

