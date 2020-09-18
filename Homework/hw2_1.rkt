#lang scheme/base

; 1) Линейно итеративный процесс, записанный с именованным let
(define (even-fib-list n)
    (if (and (integer? n) (> n 0))
        (let loop ((i n) (fib-n-1 1) (fib-n-2 0) (result '())) 
            (if (= i 0)
                (reverse result)
                (if (even? fib-n-2)
                    (loop (- i 1) (+ fib-n-1 fib-n-2) fib-n-1 (cons fib-n-2 result))
                    (loop i (+ fib-n-1 fib-n-2) fib-n-1 result)
                )
            )
        )
        '()
    )
)

; 2) Линейно рекурсивный процесс
(define (even-fib-list-2 n)
    (if (and (integer? n) (> n 0))
        (let fib ((i n) (fib-n-1 1) (fib-n-2 0))
            (if (= i 0)
                '()
                (if (even? fib-n-2)
                    (cons fib-n-2 (fib (- i 1) (+ fib-n-1 fib-n-2) fib-n-1))
                    (fib i (+ fib-n-1 fib-n-2) fib-n-1)
                )
            )
        )
        '()
    )
)

(even-fib-list 8)
(even-fib-list-2 8)

; Фомин Сергей 428
