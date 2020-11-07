#lang scheme/base
(require scheme/mpair)


(define (odd-abundant n)
    (define (abundant? x)
        (let loop ((d 1) (result 0))
            (if (<= d x)
                (if (= (modulo x d) 0)
                    (loop (+ d 1) (+ result d))
                    (loop (+ d 1) result)
                )
                (if (< (* 2 x) result)
                    #t
                    #f
                )
            )
        )
    )
    (let loop ((i 1) (num 945))
        (if (abundant? num)
            (if (= i n)
                num
                (loop (+ i 1) (+ num 2))            
            )
            (loop i (+ num 2)) 
        )
    )
)

(odd-abundant 60)
(odd-abundant 60)


(define abundant-table (make-hash (list (cons 1 945))))

(define (memo-odd-abundant n)
    (define (abundant? x)
        (let loop ((d 1) (result 0))
            (if (<= d x)
                (if (= (modulo x d) 0)
                    (loop (+ d 1) (+ result d))
                    (loop (+ d 1) result)
                )
                (if (< (* 2 x) result)
                    #t
                    #f
                )
            )
        )
    )
    (define (find-max tbl n)
        (let loop ((curr n) (find (hash-ref tbl n #f)))
            (if find
                curr
                (loop (- curr 1) (hash-ref tbl (- curr 1) #f))
            )
        )
    )
    (let ((max_num (find-max abundant-table n)))
        (let loop ((i max_num) (num (hash-ref abundant-table max_num #f)))
            (if (abundant? num)
                (begin
                    (hash-set! abundant-table n num)
                    (if (= i n)
                        num
                        (loop (+ i 1) (+ num 2))            
                    )
                )
                (loop i (+ num 2)) 
            )
        )
    )
)

(memo-odd-abundant 60)
(memo-odd-abundant 70)
(memo-odd-abundant 65)

; Мемоизированная версия дает выигрыш в случае, если функция вызывается повторно. Если вызываемая повторно функция имеет больший параметр n, то выбирается наибольший вычисленный параметр меньший n и поиск начинается с него. Если параметр меньше вычисленного, то функция сразу возвращает результат, так как все промежуточные избыточные числа запоминаются.
