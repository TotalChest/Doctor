#lang scheme/base

(define (fun1 lst)
    (if (null? lst)
        '()
        (let
            ((pair 
                (foldl
                  (lambda (x y) 
                    (if (> x (car y))
                      (cons x (car y))
                      (if (and (> x (cdr y)) (not (= x (car y))))
                        (cons (car y) x)
                        y
                      )
                    )
                  )
                  (cons (car lst) (car lst))
                  lst
                )
            ))
            (if (= (car pair) (cdr pair))
                '()
                (cdr pair)           
            )
        )
    )
)

;(fun1 (list))
;(fun1 '(1 1))
;(fun1 (list -1 0 1 -1 0 1 -1))
;(fun1 (list 5 5 5 5 5))
