(define-syntax and
    (lambda ([stx])
        (if (second stx)
            (third stx) #f)))

(and #t #f)
