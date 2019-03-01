(define (xor [l : Int][r : Int]) : Bool
    (let-syntax ([zero? (lambda ((stx))
                        (eq? (second stx) 0))])
    (let-syntax ([one? (lambda ((stx))
                        (eq? (second stx) 1))])
    (or (and (zero? l) (one? r))
        (and (one? l) (zero? r))))))

(xor 1 0)
