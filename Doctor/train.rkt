#lang scheme/base
(require racket/string)
(require 2htdp/batch-io)
(require racket/format)


(define N 4)
(define text (read-file "Texts/test.txt"))


; Фильтрация текста и разбиение на лексемы
(define (split text)
    (regexp-split
        #px"\\s+" ; Разделение по пробельным символам
        (regexp-replace*
            #px"([\\.\\?!;,:\\(\\)])" ; Добавление разделителей для знаков
            (regexp-replace*
                #px"[^\\w\\.\\?!;,:\\s\\(\\)]+" ; Очистка от лишних символов
                text
                ""
            )
            " \\1 "
        )
    )
)


; Разбивка на N-грамы
(define (n-gramming lst)
    (let loop ((result '()) (lst  lst) (curr-n '()))
        (if (null? lst)
            result
            (if (= (length curr-n) N)
                (loop  (cons curr-n result) (cdr lst) (append (cdr curr-n) (list (car lst))))
                (loop  result (cdr lst) (append curr-n (list (car lst))))
            )
        )
    )
) 


; Получение списка n-грамм
(define n-text (n-gramming (split text)))


; Формирование первых и последних (N-1)-грамм в предложении с частотами
(define (n-starting n-lst)
    (let loop ((n-lst n-lst) (start-result (make-hash)) (end-result (make-hash)))
        (if (null? n-lst)
            (cons start-result end-result)
            (begin            
                (cond
                    ((equal? (list-ref (car n-lst) (- N 1)) ".")
                        (let ((key (reverse (cdr (reverse (car n-lst))))))
                            (hash-set! end-result key (+ (hash-ref end-result key 0) 1))
                        )
                    )
                    ((equal? (list-ref (car n-lst) 0) ".")
                        (hash-set! start-result (cdr (car n-lst)) (+ (hash-ref start-result (cdr (car n-lst)) 0) 1))
                    )
                )
                (loop (cdr n-lst) start-result end-result)
            )
        )
    )
)


; Запись в файл хеш-таблицы
(define (write-hash hash file)
    (let loop ((keys (hash-keys hash)) (result '()))
        (if (null? keys)
            (write-file file (string-join result))
            (loop
                (cdr keys)
                (cons
                    (string-join (append (list "|" (~a (hash-ref hash (car keys) 0))) (car keys)))
                    result
                )
            )
        
        )
    )
)


; Запись в файл самых первых и последних N-грамм с частотами
(define border (n-starting n-text))
(write-hash (car border) "Graphs/starting.txt")
(write-hash (cdr border) "Graphs/ending.txt")


; Формирование N грамм из середины предложения c частотами
(define (make-graphs n-text)
    (let loop ((n-lst n-text) (result-forward (make-hash)) (result-backward (make-hash)))
        (if (null? n-lst)
            (cons result-forward result-backward)
            (if (member "." (cdr (reverse (cdr (car n-lst))))) ; Точка не находится в центре n-граммы
                (loop (cdr n-lst) result-forward result-backward)
                (begin
                    (let ((last-word (car (reverse (car n-lst)))) (prev (reverse (cdr (reverse (car n-lst)))))) ; forward
                        (hash-set!
                            result-forward
                            prev
                            (let search ((curr-lst (hash-ref result-forward prev '())) (result '()) (exist #f))
                                (if (null? curr-lst)
                                    (if (not exist)
                                        (cons (list last-word 1) result)
                                        result
                                    )
                                    (if (equal? (caar curr-lst) last-word)
                                        (search (cdr curr-lst) (cons (list last-word (+ 1 (cadar curr-lst))) result) #t)
                                        (search (cdr curr-lst) (cons (car curr-lst) result) exist)
                                    )
                                )
                            )
                        )
                    )
                    (let ((first-word (car (car n-lst))) (post (cdr (car n-lst)))) ; backward
                        (hash-set!
                            result-backward
                            post
                            (let search ((curr-lst (hash-ref result-backward post '())) (result '()) (exist #f))
                                (if (null? curr-lst)
                                    (if (not exist)
                                        (cons (list first-word 1) result)
                                        result
                                    )
                                    (if (equal? (caar curr-lst) first-word)
                                        (search (cdr curr-lst) (cons (list first-word (+ 1 (cadar curr-lst))) result) #t)
                                        (search (cdr curr-lst) (cons (car curr-lst) result) exist)
                                    )
                                )
                            )
                        )
                    )
                    (loop (cdr n-lst) result-forward result-backward)
                )
            )
        )
    )
)


; Запись в файл графа
(define (write-graph graph file)
    (let loop ((keys (hash-keys graph)) (result '()))
        (if (null? keys)
            (write-file file (string-join result))
            (loop
                (cdr keys)
                (cons
                    (string-join
                        (append
                            (list "|")
                            (car keys)
                            (let linear ((lst (hash-ref graph (car keys) '())) (result '()))
                                (if (null? lst)
                                    result
                                    (let ((word (caar lst)) (frec (~a (cadar lst))))
                                        (linear (cdr lst) (append (append (list "$") (list word frec)) result))
                                    )
                                )
                            )
                        )
                    )
                    result
                )
            )
        )
    )
)


(define graphs (make-graphs n-text))
; Запись forward графа в файл
(write-graph (car graphs) "Graphs/forward-graph.txt")
; Запись backward графа в файл
(write-graph (cdr graphs) "Graphs/backward-graph.txt")
