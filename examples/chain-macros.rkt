(let-syntax ([times2 (lambda (stx)
    (list (quote-syntax *) 2 (second stx)))])

(let-syntax ([sub1 (lambda (stx)
    (list (quote-syntax -) (second stx) 1))])

(times2 (sub1 5))))
