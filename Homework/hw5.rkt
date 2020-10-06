#lang scheme/base


; Описание дерева
(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (tree-empty? t) (equal? t #()))


(define (print-tree-by-level-asc t)
    (for-each
        (lambda (x)
            (display x)
            (if (not (equal? x "\n"))
                (display " ")
                (display "")
            )
        )
        (let loop ((queue (list (list t 0))) (h 0) (cc (lambda (x) x)))
            (if (null? queue)
                (cc '())
                (let* ((curr-item (car queue)) (curr-t (car curr-item)) (curr-h (cadr curr-item)))
                    (if (< h curr-h)
                        (loop queue curr-h (lambda (x) (cc (cons "\n" x))))
                        ; (cons "\n" (loop queue curr-h))
                        (if (tree-empty? curr-t)
                            (loop (cdr queue) curr-h cc)
                            (begin
                                (loop
                                    (append
                                        (cdr queue)
                                        (list 
                                            (list (tree-left curr-t) (+ 1 curr-h))
                                            (list (tree-right curr-t) (+ 1 curr-h))
                                        )
                                    )
                                    curr-h
                                    (lambda (x) (cc (cons (tree-data curr-t) x))) 
                                )
                            )
                        )
                    )
                )
            )
        )
    ) 
)


(print-tree-by-level-asc #())
(print-tree-by-level-asc #(1 #() #())) 
(print-tree-by-level-asc #(10 #(21 #() #()) #(22 #() #())))
(print-tree-by-level-asc #(10 #(21 #(31 #() #()) #(32 #() #())) #(22 #() #(34 #() #()))))
(print-tree-by-level-asc #(10 #(21 #(31 #() #(42 #() #())) #(32 #() #())) #(22 #() #(34 #(47 #() #()) #()))))
