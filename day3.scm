(require "test.scm")
(define real-input (read-port-to-string (open-input-file "day3.txt")))
(define test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(struct ParseResult (parsed rest))

(define (collect-int str)
  ;; acc is ParseResult
  (define (collect-int-iter parseResult)
    (when (= 0 (string-length (ParseResult-rest parseResult))) (return! parseResult))
    (define nextChar (string->number (substring (ParseResult-rest parseResult) 0 1)))
    (define nextRest (substring (ParseResult-rest parseResult) 1))
    (define isInitialChar (equal? (ParseResult-rest parseResult) str))
    (when
      (and isInitialChar (not nextChar))
      (return! (ParseResult '() str)))
    (define parsed (ParseResult-parsed parseResult))

    (if nextChar
      (collect-int-iter (ParseResult (+ nextChar (* 10 (if (null? parsed) 0 parsed))) nextRest))
      parseResult))

  (collect-int-iter (ParseResult '() str)))

(define reshash (collect-int "#0=()"))
(assert (ParseResult-parsed reshash) (list))
(assert (ParseResult-rest reshash) "#0=()")
(define res (collect-int "10"))
(assert (ParseResult-parsed res) 10)
(assert (ParseResult-rest res) "")
(define res2 (collect-int "10xxx"))
(assert (ParseResult-parsed res2) 10)
(assert (to-string (ParseResult-rest res2)) "xxx")

(define res3 (collect-int "x10xx"))
(assert (ParseResult-parsed res3) (list))
(assert (ParseResult-rest res3) "x10xx")

(define (collect-seq input seq)
  (if (starts-with? input seq)
    (ParseResult seq (substring input (string-length seq)))
    (ParseResult '() input)))

(define res-seq (collect-seq "mul(1,3)" "mul("))
(assert (ParseResult-parsed res-seq) "mul(")
(assert (ParseResult-rest res-seq) "1,3)")

(define
  (collect-instr str)
  (when (not (starts-with? str "mul(")) (return! #f)))

(define (parse input acc)
  (when (equal? (string-length input) 0) (return! acc))
  (define (next) (parse (substring input 1) acc))

  (define mul-res (collect-seq input "mul("))
  (when (null? (ParseResult-parsed mul-res)) (return! (parse (substring input 1) acc)))

  (define fst-nr-res (collect-int (ParseResult-rest mul-res)))
  (when (null? (ParseResult-parsed fst-nr-res)) (return! (parse (substring input 1) acc)))

  (define comma-res (collect-seq (ParseResult-rest fst-nr-res) ","))
  (when (null? (ParseResult-parsed comma-res)) (return! (parse (substring input 1) acc)))

  (define snd-nr-res (collect-int (ParseResult-rest comma-res)))
  (when (null? (ParseResult-parsed comma-res)) (return! (parse (substring input 1) acc)))

  (define end-paren-res (collect-seq (ParseResult-rest snd-nr-res) ")"))
  (when (null? (ParseResult-parsed end-paren-res)) (return! (parse (substring input 1) acc)))

  (parse
    (ParseResult-rest end-paren-res)
    (cons (list (ParseResult-parsed fst-nr-res) (ParseResult-parsed snd-nr-res)) acc)))
(define (sol1 input)
  (reduce
    (fn (curr acc) (+ acc (* (first curr) (second curr))))
    0
    (parse input '())))

(assert (sol1 "mul(1,2)") 2)
(assert (sol1 test-input) 161)
(assert (sol1 real-input) 159892596)
(define (parse2 input acc enabled-arg)
  (define enabled
    (cond
      [(starts-with? input "do()") #t]
      [(starts-with? input "don't()") #f]
      [#t enabled-arg]))

  ; (displayln input acc enabled-arg)
  (when (equal? (string-length input) 0) (return! acc))
  (define (next) (parse2 (substring input 1) acc enabled))
  (when (not enabled) (return! (parse2 (substring input 1) acc enabled)))

  (define mul-res (collect-seq input "mul("))
  (when (null? (ParseResult-parsed mul-res)) (return! (parse2 (substring input 1) acc enabled)))

  (define fst-nr-res (collect-int (ParseResult-rest mul-res)))
  (when (null? (ParseResult-parsed fst-nr-res)) (return! (parse2 (substring input 1) acc enabled)))

  (define comma-res (collect-seq (ParseResult-rest fst-nr-res) ","))
  (when (null? (ParseResult-parsed comma-res)) (return! (parse2 (substring input 1) acc enabled)))

  (define snd-nr-res (collect-int (ParseResult-rest comma-res)))
  (when (null? (ParseResult-parsed comma-res)) (return! (parse2 (substring input 1) acc enabled)))

  (define end-paren-res (collect-seq (ParseResult-rest snd-nr-res) ")"))
  (when (null? (ParseResult-parsed end-paren-res)) (return! (parse2 (substring input 1) acc enabled)))

  (parse2
    (ParseResult-rest end-paren-res)
    (cons (list (ParseResult-parsed fst-nr-res) (ParseResult-parsed snd-nr-res)) acc)
    enabled))
(define (sol2 input)
  (reduce
    (fn (curr acc) (+ acc (* (first curr) (second curr))))
    0
    (parse2 input '() #t)))
(assert (sol2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))") 48)
(assert (sol2 real-input) 92626942)
