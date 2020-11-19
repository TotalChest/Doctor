#lang scheme/base
(require racket/class)
(require racket/string)


(define 2tree<%>
    (interface ()
        isEmpty?
        printTree
    )
)

(define Empty2tree%
    (class* object% (2tree<%>)
        (super-new)
        (define/public (isEmpty?) #t)
        (define/public (printTree) (printf ""))
    )
)

(define Nonempty2tree%
    (class* object% (2tree<%>)
        (super-new)
        (init-field tag)
        (init-field data)
        (field (left 2tree<%>))
        (field (right 2tree<%>))
        (define/public (isEmpty?) #f)
        (define/public (printTree)
            (begin
                (send left printTree)
                (send right printTree)              
                (printf "~a\n" tag)
            )
        )
        (define/public (get-tag) tag)
        (define/public (get-data) data)
        (define/public (set-tag! t) (set! tag t))
        (define/public (set-data! d) (set! data d))
        (define/public (get-left) left)
        (define/public (get-right) right)
        (define/public (set-left! tr) (set! left tr))
        (define/public (set-right! tr) (set! right tr))
    )
)

(define a (new Empty2tree%))
(send a isEmpty?)

(define b (new Nonempty2tree% (tag 12) (data 'text)))
(send b set-left! (new Empty2tree%))
(send b set-right! (new Empty2tree%))
(send b isEmpty?)
(send b printTree)

(send b set-tag! 55)
(define c (new Nonempty2tree% (tag 80) (data 'text80)))
(send c set-left! (new Empty2tree%))
(send c set-right! (new Empty2tree%))
(send b set-left! c)
(send b printTree)
