(require "test.scm")
; (define real-input (read-port-to-string (open-input-file "day5.txt")))
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
  (define (index-of-iter list elem curr-index)
    (cond
      [(null? list) (None)]
      [(equal? (first list) elem) curr-index]
      [else (index-of-iter (cdr list) elem (+ curr-index 1))]))
  (index-of-iter list elem 0))
(assert (index-of (list 'a 'b 'c) 'a) 0)
(assert (index-of (list) 'a) (None))
(assert (index-of (list 'a 'b 'c) 'c) 2)
(assert (index-of (list 'a 'b 'c) 'd) (None))
(define
  (all? pred list)
  (cond
    [(null? list) #t]
    [else (and (pred (car list)) (all? pred (cdr list)))]))

(define (sol1 input)
  (define full-split (split-once input "\n\n"))
  (define rules (map (fn (row) (split-once row "|")) (split-many (first full-split) "\n")))
  (define updates (map (fn (row) (split-many row ",")) (split-many (second full-split) "\n")))

  (displayln rules)
  (for-each displayln updates)
  (define ok-updates

    (filter
      (fn (update)
        (all?
          (fn (rule)
            (define fst-index (index-of update (first rule)))
            (define snd-index (index-of update (second rule)))
            (when (or (None? fst-index) (None? snd-index)) (return! #t))
            (define res (< fst-index snd-index))
            res)
          rules))
      updates))
  (sum (map string->number (map (fn (update) (list-ref update (floor (/ (length update) 2)))) ok-updates))))

(assert (sol1 test-input) 143)
