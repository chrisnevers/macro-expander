(let-syntax ([add1 (lambda (stx)
    (list (quote-syntax +) (quote 1) (second stx)))])

(add1 (+ 8 5)))
