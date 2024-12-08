(require "test.scm")
(define real-input (read-port-to-string (open-input-file "day4.txt")))
(define test-input
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(define (get-char str-list x y)
  (define row (with-handler (fn (_) (None)) (list-ref str-list y)))
  (when (null? row) (return! (None)))
  (define col (with-handler (fn (_) (None)) (substring row x (+ 1 x))))
  col)
(define get-char-test (split-many "abc
def
ghi"
                       "\n"))

(assert (get-char get-char-test 0 0) "a")
(assert (get-char get-char-test 2 0) "c")
(assert (get-char get-char-test 3 0) (None))
(assert (get-char get-char-test 0 2) "g")
(assert (get-char get-char-test 0 3) (None))
(assert (get-char get-char-test 2 2) "i")

(define pos-coords (list (list '(0 0) '(0 1) '(0 2) '(0 3))
                    (list '(0 0) '(1 0) '(2 0) '(3 0))
                    (list '(0 0) '(1 1) '(2 2) '(3 3))
                    (list '(0 0) '(1 -1) '(2 -2) '(3 -3))))

(define (get-all-combos str-list x y)
  (define coords (map
                  (fn (row)
                    (map
                      (fn (coord)
                        (list (+ x (first coord)) (+ y (second coord))))
                      row))
                  pos-coords))
  (define char-colls (map (fn (row)
                           (map (fn (coord)
                                 (define x (first coord))
                                 (define y (second coord))
                                 (get-char str-list x y))
                             row))
                      coords))

  (map
    (fn (row) (string-join (filter string? row)))
    char-colls))

(define
  get-combs-input
  (split-many
    "abcd
efgh
ijkl
mnop"
    "\n"))
(define input-split (split-many test-input "\n"))
(define (count-xmas str-list x y)
  (define combos (get-all-combos str-list x y))
  (define filtered
    (filter (fn (x)
             (or
               (equal? x "SAMX")
               (equal? x "XMAS")))
      combos))
  (length filtered))
(assert (count-xmas input-split 4 0) 1)
(assert (count-xmas input-split 0 0) 0)
(assert (count-xmas input-split 5 0) 1)

(define (sol1 input)
  (define str-lists (split-many input "\n"))
  (define max-x (string-length (list-ref str-lists 0)))
  (define max-y (length str-lists))
  (sum (transduce
        (range 0 max-y)
        (flat-mapping (fn (y) (map (fn (x) (list x y)) (range 0 max-x))))
        (mapping (fn (coords)
                  (count-xmas str-lists (first coords) (second coords))))
        (into-list))))
(assert (sol1 test-input) 18)
(assert (sol1 real-input) 2514)

(define (sol2 input)
  (define str-lists (split-many input "\n"))
  (define max-x (string-length (list-ref str-lists 0)))
  (define max-y (length str-lists))
  (sum (transduce
        (range 0 max-y)
        (flat-mapping (fn (y) (map (fn (x) (list x y)) (range 0 max-x))))
        (mapping (fn (init-coord)
                  (when (not (equal? "A"
                              (get-char str-lists (first init-coord) (second init-coord))))
                    (return! 0))

                  (define relative-coords1 (list '(-1 -1) '(0 0) '(1 1)))
                  (define relative-coords2 (list '(1 -1) '(0 0) '(-1 1)))
                  (define absolute-coords1
                    (map (fn (rel-coord)
                          (list
                            (+ (first rel-coord) (first init-coord))
                            (+ (second rel-coord) (second init-coord))))
                      relative-coords1))
                  (define absolute-coords2
                    (map (fn (rel-coord)
                          (list
                            (+ (first rel-coord) (first init-coord))
                            (+ (second rel-coord) (second init-coord))))
                      relative-coords2))

                  (define char-colls1 (string-join (filter string? (map
                                                                    (fn (coord)
                                                                      (define x (first coord))
                                                                      (define y (second coord))
                                                                      (get-char str-lists x y))
                                                                    absolute-coords1))))
                  (define char-colls2 (string-join (filter string? (map
                                                                    (fn (coord)
                                                                      (define x (first coord))
                                                                      (define y (second coord))
                                                                      (get-char str-lists x y))
                                                                    absolute-coords2))))
                  (define res (if (or
                                   (and (equal? char-colls1 "MAS") (equal? char-colls2 "SAM"))
                                   (and (equal? char-colls1 "SAM") (equal? char-colls2 "SAM"))
                                   (and (equal? char-colls1 "MAS") (equal? char-colls2 "MAS"))
                                   (and (equal? char-colls1 "SAM") (equal? char-colls2 "MAS")))
                               1
                               0))
                  res))
        (into-list))))

(assert (sol2 test-input) 9)
(assert (sol2 real-input) 1888)

; (displayln (with-handler (fn (_) '()) (string-ref "" 10)))
