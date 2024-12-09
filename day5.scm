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

(define (sol1 input)
  (define full-split (split-once input "\n\n"))
  (displayln full-split)
  (define rules (map (fn (row) (split-once row "|")) (~> full-split (first) (split-many "\n"))))
  (define updates (map (fn (row) (split-many row ",")) (~> full-split (second) (split-many "\n"))))

  (displayln rules)
  (displayln updates)

  0)
(assert (sol1 test-input) 143)
