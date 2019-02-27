(define-syntax int->bool
    (syntax-case
        [(int->bool 0) #f]
        [(int->bool _) #t]))

(or (int->bool 5) (int->bool 0))
