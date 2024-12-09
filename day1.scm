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

(define (sol1 input)
  (define lines (~> input (trim) (split-many "\n")))
  (define lists
    (reduce
      (fn (curr acc)
        (define split (split-once curr "   "))
        (define snd-nr (string->number (second split)))
        (define fst-nr (string->number (first split)))
        (list
          (insert-sort fst-nr (first acc))
          (insert-sort snd-nr (second acc))))
      (list '() '())
      lines))

  (~>
    (transduce
      (first lists)
      (zipping (second lists))
      (mapping (fn (item) (abs (- (first item) (second item)))))
      (into-list))
    (sum)))

(assert (sol1 test-input) 11)

(assert (sol1 real-input) 1882714)

(define (sol2 input)
  (define lines (~> input (trim) (split-many "\n")))
  (define lists
    (reduce
      (fn (curr acc)
        (define split (split-once curr "   "))
        (define snd-nr (string->number (second split)))
        (define fst-nr (string->number (first split)))
        (list
          (insert-sort fst-nr (first acc))
          (insert-sort snd-nr (second acc))))
      (list '() '())
      lines))
  (define occurences-hash
    (reduce
      (fn (curr acc)
        (define curr-value (if (hash-contains? acc curr) (hash-ref acc curr) 0))
        (hash-insert acc curr (+ 1 curr-value)))
      (hash)
      (second lists)))

  (reduce (fn (curr acc)
           (define occurences
             (if (hash-contains? occurences-hash curr) (hash-ref occurences-hash curr) 0))
           (+ acc (* occurences curr)))
    0
    (first lists)))

(assert (sol2 test-input) 31)

(assert (sol2 real-input) 19437052)
