(let-syntax ([add1 (lambda (stx)
    (list (quote-syntax +) 1 (second stx)))])

(add1 (+ 8 5)))
