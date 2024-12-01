(define *failed-assertions* (vector))
(define-syntax assert
  (syntax-rules ()
    [(assert a b)
      (let* (
             [aEvaled a]
             [bEvaled b]
             [aStringified (trim-start-matches (to-string 'a) "'")]
             [bStringified (trim-start-matches (to-string 'b) "'")])
        (when
          (not (eq? aEvaled bEvaled))
          (error-with-span
            (list
              (first (#%syntax-span a))
              (second (#%syntax-span b))
              (third (#%syntax-span a)))
            (string-join
              (list
                (to-string)
                aStringified
                " is not equal to "
                bStringified
                " ("
                (to-string aEvaled)
                " !== "
                (to-string bEvaled)
                ")")))
          (displayln)))]))

(define (run-tests)
  (for-each (lambda (x) (displayln x)) (vector->list *failed-assertions*)))

;; assert(1+2, 3+4)
