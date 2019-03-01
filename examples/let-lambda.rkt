(define-syntax let'
    (syntax-case
        [(let' ((i e) ...) b)
            ((lambda ((i) ...) b) e ...)]))

(let' ((x 3) (y 5)) (+ x y))
