#lang scheme/base


; Описание дерева
(define empty-tree #())
(define make-tree vector)
(define (tree-left tree) (vector-ref tree 0))
(define (tree-right tree) (vector-ref tree 1))
(define (tree-empty? tree) (equal? tree #()))
(define (leaf-tree? tree) (not (vector? tree)))
(define (tree-data tree)
	(if (leaf-tree? tree) tree '()))


(define (task-4-2020 h)
    (let h-tree ((curr-h h) (curr-data 1))
        (if (= curr-h 0)
            empty-tree
            (let ((next-h (- curr-h 1)) (data (* 2 curr-data)))
                (make-tree
                    curr-data
                    (h-tree next-h data)
                    (h-tree next-h data)
                )
            )
        )
    )
)

(task-4-2020 0)
(task-4-2020 1)
(task-4-2020 2)
(task-4-2020 3)
