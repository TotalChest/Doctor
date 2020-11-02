#lang scheme/base
(require scheme/mpair)


(define (rot-left! mlst)
    (let loop ((first (mcar mlst)) (curr mlst))
        (if (null? (mcdr curr))
            (set-mcar! curr first)
            (begin
                (set-mcar! curr (mcar(mcdr curr)))
                (loop first (mcdr curr))
            )
        )
    )
)


(define-syntax rot-left2!
    (syntax-rules ()
        ((_ mlst)
            (let loop ((first (mcar mlst)) (curr mlst))
                (if (null? (mcdr curr))
                    (set-mcar! curr first)
                    (begin
                        (set-mcar! curr (mcar(mcdr curr)))
                        (loop first (mcdr curr))
                    )
                )
            )
        )
    )
)

(define l (mlist 1 2 3 4 5 6)) 
l
(rot-left! l)
l
(rot-left2! l)
l

; Более уместная в данном случае первая реализация через обычную функцию. Если можно обойтись без макроса - стараемся обойтись без него, так как макросы не являются объектами первого класса (их нельзя предевать в функцию), макросы усложнаяют отладку.



