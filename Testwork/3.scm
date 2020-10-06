#lang scheme/base

(define (fun3 n)
    (let loop ((number 0) (curr 4) (result 1))
        (if (= number n)
            result
            (let
                ((prime
                	(let loop ((d 2))
                		(if (> (* d d) curr)
                            #t
                            (if (= (modulo curr d) 0)
                                #f
                                (loop (+ d 1))
                            )
                        )
                    )
                ))   
                (if (not prime)
                    (loop (+ number 1) (+ curr 1) (* result curr))
                    (loop number (+ curr 1) result)
                )
            )
        )
    )
)

(fun3 0)
