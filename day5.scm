(require "test.scm")
(define real-input (read-port-to-string (open-input-file "day5.txt")))
(define test-input
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")
(define (index-of list elem)
  (define (index-of-iter list curr-index)
    (cond
      [(null? list) (None)]
      [(vector? list)
        (if
          (equal? (vector-ref list 0) elem)
          curr-index
          (index-of-iter (vec-rest list) (+ curr-index 1)))]
      [(equal? (first list) elem) curr-index]
      [else (index-of-iter (rest list) (+ curr-index 1))]))
  (index-of-iter list 0))
(assert (index-of (list 'a 'b 'c) 'a) 0)
(assert (index-of (list) 'a) (None))
(assert (index-of (list 'a 'b 'c) 'c) 2)
(assert (index-of (list 'a 'b 'c) 'd) (None))
(assert (index-of (list->vector (list 'a 'b 'c)) 'd) (None))
(assert (index-of (list->vector (list 'a 'b 'c)) 'a) 0)

(define
  (all? pred list)
  (cond
    [(null? list) #t]
    [else (and (pred (car list)) (all? pred (cdr list)))]))

(define (ok-update rules update)
  (all?
    (fn (rule)
      (define fst-index (index-of update (first rule)))
      (define snd-index (index-of update (second rule)))
      (when (or (None? fst-index) (None? snd-index)) (return! #t))
      (define res (< fst-index snd-index))
      res)
    rules))

(define (sol1 input)
  (define full-split (split-once (trim input) "\n\n"))
  (define rules (map (fn (row) (split-once row "|")) (split-many (first full-split) "\n")))
  (define updates (map (fn (row) (split-many row ",")) (split-many (second full-split) "\n")))

  (define ok-updates
    (filter
      (fn (update) (ok-update rules update))
      updates))
  (sum (map (fn (x)
             (define parsed (string->number x))
             (unless parsed (error! (string-join (list "error when parsing" x))))
             parsed)
        (map (fn (update) (list-ref update (floor (/ (length update) 2)))) ok-updates))))

(assert (sol1 test-input) 143)
(assert (sol1 real-input) 6041)

(define (insert-sort thing into)
  (cond
    ((null? into) (list thing))
    ((< thing (car into)) (cons thing into))
    (else (cons (first into) (insert-sort thing (rest into))))))
(define (set-current-dir!) (immutable-vector-set))

(define (sol2 input)
  (define full-split (split-once (trim input) "\n\n"))
  (define rules (map (fn (row) (split-once row "|")) (split-many (first full-split) "\n")))
  (define updates (map (fn (row) (split-many row ",")) (split-many (second full-split) "\n")))

  (define not-ok-updates
    (filter
      (fn (update)
        (not (all?
              (fn (rule)
                (define fst-index (index-of update (first rule)))
                (define snd-index (index-of update (second rule)))
                (when (or (None? fst-index) (None? snd-index)) (return! #t))
                (define res (< fst-index snd-index))
                res)
              rules)))
      updates))
  (define ordered (map
                   (fn (update) (fix-ordering rules (list->vector update)))
                   not-ok-updates))
  (sum
    (map (fn (update) (string->number (vector-ref update (floor (/ (vector-length update) 2))))) ordered)))

(define (fix-ordering rules update)
  (define result
    (reduce
      (fn (rule update)
        (define fst-value (first rule))
        (define snd-value (second rule))
        (define fst-index (index-of update fst-value))
        (define snd-index (index-of update snd-value))
        (when (or (None? fst-index) (None? snd-index)) (return! update))
        (define res (<= fst-index snd-index))
        (define reordered (immutable-vector-set
                           (immutable-vector-set update fst-index snd-value)
                           snd-index
                           fst-value))
        (if (not res)
          (begin
            reordered)
          update))
      update
      rules))
  (if (ok-update rules result) result (fix-ordering rules result)))
(assert (sol2 test-input) 123)

; 4748 too low
(assert (sol2 real-input) 4884)
