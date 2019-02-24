(let-syntax ([x (lambda ([stx])
                   (lambda ([x : Bool]) : Bool
                        (second stx)))])
(x 1))
