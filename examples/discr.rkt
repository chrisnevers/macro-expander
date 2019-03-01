(let-syntax ([discriminate (lambda ([stx])
    (let ((a (nth stx 2)))
    (let ((b (nth stx 3)))
    (let ((c (nth stx 4)))
    (- (* b b) (+ 4 a c))))))])

(discriminate 1 2 3))

;;; output =>
;;; (let ([a_6 1])
;;; (let ([b_8 2])
;;; (let ([c_10 3])
;;;     (- (* 2 2) (+ 4 1 3)))))
