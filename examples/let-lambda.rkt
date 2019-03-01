(define-syntax let'
    (syntax-case
        [(_ ((i e) ...) b ...)
            ((lambda ((i)...) b ...) e ...)]))

(let' ((x 3) (y 5)) (+ x y))
