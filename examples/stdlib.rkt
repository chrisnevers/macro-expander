(define-syntax and
  (syntax-case
    [(_) #t]
    [(_ x) x]
    [(_ x y) (if x y #f)]
    [(_ x y z ...) (if x (and y z ...) #f)]))

(define-syntax or
  (syntax-case
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2) (if e1 #t e2)]
    [(_ e1 e2 e3 ...) (let ((t e1)) (if t t (or e2 e3 ...)))]))

(define-syntax when
  (syntax-case
    [(_ e0 e1 e2 ...) (if e0 (begin e1 e2 ...))]))

(define-syntax unless
  (syntax-case
    [(_ e0 e1 e2 ...) (when (not e0) e1 e2 ...)]))


;;; (unless  1 2 3)
;;; (and)
(or 4 5 6)
