#lang scheme/base

(define (fun2a n)
    (let loop ((del 4))
        (if (> del n)
            '()
            (let (
                (prime
                    (let loop ((d 2))
			            (if (> (* d d) del)
				            #t
				            (if (= (modulo del d) 0)
				                #f
				                (loop (+ d 1))
				            )
				        )
			        )
			    ))  
		        (if (and (= (modulo n del) 0) (not prime))
		            (cons del (loop (+ del 1)))
	                (loop (+ del 1))
		        )
		    )
        )
    )
)

(define (fun2b n)
    (reverse 
        (let loop ((del 4) (result '()))
            (if (> del n)
                result
                (let (
                    (prime
                        (let loop ((d 2))
				            (if (> (* d d) del)
					            #t
					            (if (= (modulo del d) 0)
					                #f
					                (loop (+ d 1))
					            )
					        )
				        )
				    ))  
			        (if (and (= (modulo n del) 0) (not prime))
		                (loop (+ del 1) (cons del result))
		                (loop (+ del 1) result)
			        )
			    )
            )
        )
    )
)

(display (fun2a 24))
(display (fun2b 24))

