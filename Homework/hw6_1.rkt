#lang scheme/base
(require scheme/mpair)


(define (make-queue)
    (mcons 'queue '())
)

(define (queue? q)
    (and
        (mpair? q)
        (eq? 'queue (mcar q))
    )
)

(define (empty-queue? q)
    (and
        (queue? q)
        (null? (mcdr q))
    )
)

(define (insert-queue! q e)
    (if (queue? q)
        (set-mcdr! q (mcons e (mcdr q)))
        q
    )
)

(define (front-queue q)
    (if (and (queue? q) (not (empty-queue? q)))
        (let loop ((curr (mcdr q)))
            (if (null? (mcdr curr))
                (mcar curr)
                (loop (mcdr curr))
            )
        )
        "empty queue"
    )
)

(define (delete-queue! q)
    (if (and (queue? q) (not (empty-queue? q)))
        (let loop ((curr q) (next (mcdr q)))
            (if (null? (mcdr next))
                (set-mcdr! curr '())
                (loop (mcdr curr) (mcdr next))
            )
        )
        q
    )
)

(define A (make-queue))
(queue? A)
(empty-queue? A)
A
(insert-queue! A 10)
A
(insert-queue! A 20)
(insert-queue! A 4)
A
(front-queue A)
(front-queue A)
(delete-queue! A)
A
(delete-queue! A)
A
