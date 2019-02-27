(define-syntax add-to
    (syntax-case
        [(add-to x y ...) ((+ x y)...)]))

(add-to (+ 1 2) 2 3 4)
