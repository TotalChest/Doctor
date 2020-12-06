#lang scheme/base

(define Y
    (lambda (f)
        ((lambda (x) (x x))
            (lambda (g)
                (f 
                    (lambda
                        args
                        (apply (g g) args)
                    )
                )
            )
        )
    )
)

(define n!!!!
    (lambda (n)
        ( 
            (Y
                (lambda (f)
                    (lambda (i e-1 e-2 e-3 e-4)
                        (cond
                            ((= i 0) e-1)
                            ((= i 1) e-2)
                            ((= i 2) e-3)
                            ((= i 3) e-4)
                            (else
                                (f
                                    (- i 4)
                                    (* i e-1)
                                    (* i e-2)
                                    (* i e-3)
                                    (* i e-4)
                                )
                            )
                        )
                    )
                )
            )
            n 1 1 2 3
        )
    )
)

(n!!!! 0)
(n!!!! 1)
(n!!!! 2)
(n!!!! 3)
(n!!!! 4)
(n!!!! 5)
(n!!!! 6)
(n!!!! 7)
(n!!!! 8)
(n!!!! 9)
(n!!!! 10)
