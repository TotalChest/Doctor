#lang scheme/base


(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (tree-empty? t) (equal? t #()))


(define (fun4 tree r1 r2)
    (if (> r1 r2)
        (fun4 tree r2 r1)
        (if (tree-empty? tree)
            0
            (if (and (<= r1 0) (>= r2 0))
                (if (> (tree-data tree) 0)
                    (+ 1 (fun4 (tree-left tree) (- r1 1) (- r2 1))
                         (fun4 (tree-right tree) (- r1 1) (- r2 1))
                    )
                    (+ (fun4 (tree-left tree) (- r1 1) (- r2 1))
                       (fun4 (tree-right tree) (- r1 1) (- r2 1))
                    )
                )
                (if (> r1 0)
                    (+ (fun4 (tree-left tree) (- r1 1) (- r2 1))
                       (fun4 (tree-right tree) (- r1 1) (- r2 1))
                    )
                    0
                )
            )
        )
    )
)

(fun4 #(1 #(-1 #(3 #() #()) #(3 #() #())) #(-2 #() #())) 2 1)
(fun4 #() 1 10)
(fun4 #(10 #() #()) 0 0)
