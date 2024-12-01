(require "test.scm")

(define real-input (read-port-to-string (open-input-file "day1.txt")))
(define test-input "3   4
4   3
2   5
1   3
3   9
3   3")

(define (insert-sort thing into)
  (cond
    ((null? into) (list thing))
    ((< thing (car into)) (cons thing into))
    (else (cons (first into) (insert-sort thing (rest into))))))
(define (reduce-list fst-list snd-list acc)
  (cond [
         (and
           (null? fst-list)
           (null? snd-list))
         acc]
    [else
      (reduce-list
        (rest fst-list)
        (rest snd-list)
        (+ acc (abs (- (first fst-list) (first snd-list)))))]))

(define (sol1 input)
  (define lines (split-many (trim input) "\n"))
  (define lists
    (reduce
      (fn (curr acc)
        (define split (split-once curr "   "))
        (displayln curr split)
        (define snd-nr (string->number (second split)))
        (define fst-nr (string->number (first split)))
        (list
          (insert-sort fst-nr (first acc))
          (insert-sort snd-nr (second acc))))
      (list '() '())
      lines))
  (reduce-list (first lists) (second lists) 0))

(assert (sol1 test-input) 11)

(assert (sol1 real-input) 1882714)
