(require "test.scm")

(define real-input (read-port-to-string (open-input-file "day2.txt")))
(define test-input "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(define (get-order diff)
  (cond
    ((> diff 0) 'pos)
    ((< diff 0) 'neg)
    (else 'none)))

(define (incdec row order)
  (displayln row order)
  (when (< (length row) 2) (return! #t))
  (define diff (- (car row) (cadr row)))
  (define new-order (get-order diff))
  (when (not (eq? order new-order))
    (return! #f))
  (if
    (> (abs diff) 3)
    #f
    (incdec (cdr row) order)))

(define (sol1 input)
  (define lines (split-many (trim input) "\n"))
  (define lists
    (map (fn (curr)
          (define row (map string->number (split-many curr " ")))
          (define diff (- (car row) (cadr row)))
          (define res (incdec row (get-order diff)))
          (displayln row res)
          res)
      lines))

  (length (filter id lists)))

(assert (sol1 test-input) 2)

(assert (sol1 real-input) 213)
