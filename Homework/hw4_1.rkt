#lang scheme/base


(define (task-03-2020 lst)
    (sqrt (foldl
        (lambda (x y)
            (+ y (* x x))
        )
        0
        (map
            (lambda (l)
                (/
                    (foldl + 0 l)
                    (length l)
                )
            )
            lst
        )
    ))
)


(task-03-2020 (list (list 4 4 4) (list 0 0 -2 2 0) (list 3 3 6 0 3)))
