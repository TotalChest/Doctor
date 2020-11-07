#lang scheme/base
(require racket/stream)


(define (gen k) 
    (let gen-let ((a 1))
        (stream-cons a (gen-let (* a k)))
    )
)

(define squares-cubes
    (let loop ((curr2 (gen 2)) (curr3 (stream-rest (gen 3))))
        (if (< (stream-first curr2) (stream-first curr3))
            (stream-cons (stream-first curr2)
                (loop (stream-rest curr2) curr3))
            (stream-cons (stream-first curr3)
                (loop curr2 (stream-rest curr3)))
        )
    )
)

(stream-ref squares-cubes 0)
(stream-ref squares-cubes 1)
(stream-ref squares-cubes 2)
(stream-ref squares-cubes 3)
(stream-ref squares-cubes 4)
(stream-ref squares-cubes 5)
(stream-ref squares-cubes 6)
(stream-ref squares-cubes 7)
(stream-ref squares-cubes 8)
(stream-ref squares-cubes 9)
(stream-ref squares-cubes 10)


