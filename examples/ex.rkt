(let-syntax ([one (lambda (stx)
                    (quote-syntax '1))])
(one))

(let-syntax ([thunk (lambda (stx)
                        (list (quote-syntax lambda)
                               (list (quote-syntax x))
                               (second stx)))])
(thunk '1))

(quote (+ 1 2))
(quote x)
