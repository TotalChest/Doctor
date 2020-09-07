#lang scheme/base

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
    (printf "Hello, ~a!\n" name)
    (print '(what seems to be the trouble?))
    (doctor-driver-loop name)
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
        (cond 
            ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
                (printf "Goodbye, ~a!\n" name)
                (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                (doctor-driver-loop name)
            )
        )
    )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
    (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
        ((0) (qualifier-answer user-response)) ; 1й способ
        ((1) (hedge))  ; 2й способ
    )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
    (append
        (pick-random 
            '((you seem to think that)
              (you feel that)
              (why do you believe that)
              (why do you say that)
              (you muttered that)
              (why are you telling me that)
              (it seemed to me or you said that))
        )
        (change-person user-response)
    )
)

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
    (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
    (many-replace-2
        '((am are)
          (are am)
          (i you)
          (me you)
          (mine yours)
          (my your)
          (myself yourself)
          (you i)
          (your my)
          (yours mine)
          (yourself myself))
        phrase
    )
)

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace-1 replacement-pairs lst)
    (cond
        ((null? lst) lst)
        (else 
            (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                (cons
                    (if pat-rep
                        (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                        (car lst) ; иначе в начале ответа помещается начало списка без изменений
                    )
                    (many-replace-1 replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                )
            )
        )
    )
)
  
; Упражнение 2
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace-2 replacement-pairs lst)
    (let loop ((new_lst lst) (result '()))
        (if (null? new_lst)
            (reverse result)
            (let ((pat-rep (assoc (car new_lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                (loop
                    (cdr new_lst)
                    (cons
                        (if pat-rep
                            (cadr pat-rep) ; если поиск был удачен, то в результат Доктор пишет замену
                            (car new_lst) ; иначе в результат помещается начало списка без изменений
                        )
                        result
                    )
                )
            )
        )
    )
)

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
    (pick-random 
        '((please go on)
          (many people have the same sorts of feelings)
          (many of my patients have told me the same thing)
          (please continue)
          (it is very common problem)
          (do not stop)
          (i understand you))
    )
)

; запуск Доктора
(visit-doctor "Sergey")
