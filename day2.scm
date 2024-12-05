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
          (define res
            (incdec row (get-order diff)))
          res)
      lines))

  (length (filter id lists)))

(assert (sol1 test-input) 2)

(assert (sol1 real-input) 213)

;;@doc
;;returns next row or #f if it's impossible
(define (iter row order)
  (when (< (length row) 2) (return! #t))
  (define diff (abs (- (car row) (cadr row))))

  (cond
    [(or (> diff 3) (< diff 1)) #f]
    [(and (eq? order 'pos) (> (car row) (cadr row)))
      (begin #t)]
    [(and (eq? order 'neg) (< (car row) (cadr row)))
      (begin #t)]
    [else #f]))

(assert (iter (list 1 2) 'neg) #t)
(assert (iter (list 1 2) 'pos) #f)
(assert (iter (list 2 1) 'pos) #t)
(assert (iter (list 2 1) 'neg) #f)

(define (init-order row curr-order) (if (null? curr-order) (get-order (- (car row) (cadr row))) curr-order))
(define (incdec2 row arg-order remove-used)
  (define order (init-order row arg-order))
  (define res (iter row order))
  (cond
    [(and res (null? row)) #t]
    [res (or
          (incdec2 (cdr row) order remove-used)
          (and
            (not remove-used)
            (begin
              (define row (cons (car row) (cddr row)))
              (incdec2 row (init-order row arg-order) #t))))]
    [(and (eq? res #f) remove-used) #f]
    [(not res)

      (and
        (not remove-used)
        (begin
          (define row (cons (car row) (cddr row)))
          (incdec2 row (init-order row arg-order) #t)))]
    [else (raise-error "weird case")]))

(define (sol2 input)
  (define lines (split-many (trim input) "\n"))
  (define lists
    (map (fn (curr)
          (define row (map string->number (split-many curr " ")))
          (define res
            (incdec2 row '() #f))
          (or res (incdec2 (cdr row) '() #t)))
      lines))
  (length (filter id lists)))

(assert (sol2 test-input) 4)

(assert (sol2 "1 7 6 5 4") 1)

(assert (sol2 "9 7 6 5 4") 1)

(assert (sol2 "1 2 7 8 9") 0)

(assert (sol2 "7 1 1 6 5 4") 0)

(assert (sol2 real-input) 285)
