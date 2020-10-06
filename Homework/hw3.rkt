#lang scheme/base


; Упражнение 1.1)
(define (list-fib-squares-1 n)
    (map (lambda (x) (* x x))
        (let fib ((i n) (fib-1 1) (fib-2 0) (result '()))
            (if (= 0 i)
                (reverse result)
                (fib (- i 1) (+ fib-1 fib-2) fib-1 (cons fib-2 result))
            )
        )
    )
)


; Упражнение 1.2)
(define (list-fib-squares-2 n)
    (foldl 
        (lambda (x y)
            (cons (* x x) y)
        )
        '()
        (let fib ((i n) (fib-1 1) (fib-2 0) (result '()))
            (if (= 0 i)
                result
                (fib (- i 1) (+ fib-1 fib-2) fib-1 (cons fib-2 result))
            )
        )
    )
)

(list-fib-squares-1 10)
(list-fib-squares-2 10)


; Упражнение 2)
(define (process lst)
    (let ((first-mul (foldl * 1 (list-ref lst 0))))
        (filter
            (lambda(x)
                (if
                    (>
                        (foldl + 0 x)
                        first-mul
                    )
                    #t
                    #f
                )
            )
            lst
        )
    )
)

(process '((5 2) (1 2) () (3 4) (2 3) (2 3 4) (3 4 7 8) (5 6)))
