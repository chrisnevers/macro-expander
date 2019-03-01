(let-syntax ([times2 (lambda ((stx))
    (* 2 (second stx)))])

(let-syntax ([sub1 (lambda ((stx))
    (times2 (- (second stx) 1)))])

(sub1 5)))
