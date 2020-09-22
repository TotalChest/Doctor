#lang scheme/base

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor stop-word clients-count)
    (let clients-loop ((curr-count clients-count))
        (if (> curr-count 0)
            (let ((name (ask-patient-name)))
                (if (equal? name stop-word)
                    (clients-loop 0)
                    (begin
                        (printf "Hello, ~a!\n" name)
                        (print '(what seems to be the trouble?))
                        (doctor-driver-loop name)
                        (clients-loop (- curr-count 1))
                    )
                )
            )
            (println '(time to go home))
        )
    )
)

; Упражнение 5
(define (ask-patient-name)
    (begin
        (println '(next!))
        (println '(who are you?))
        (print '**)
        (car (read))
    ) 
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (let responses-loop ((name name) (responses '()))
        (newline)
        (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
        (let ((user-response (read)))
            (cond 
                ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
                    (printf "Goodbye, ~a!\n" name)
                    (println '(see you next week)))
                (else
                    (print (reply user-response responses)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                    (responses-loop name (cons user-response responses))
                )
            )
        )
    )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response responses)
    (let ((keywords (keywords? user-response)) (first (null? responses)))
        (if keywords
            (case (random (if first 3 4))
                ((0) (qualifier-answer user-response))
                ((1) (hedge))
                ((2) (pick-template user-response))
                ((3) (history-answer responses))
                
            )
            (case (random (if first 2 3))
                ((0) (qualifier-answer user-response))
                ((1) (hedge))
                ((2) (history-answer responses))
            ) 
        )
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
    (many-replace-3
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
                        (cadr pat-rep) ; если поиск был удачен, то делаем замену
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
                            (cadr pat-rep) ; если поиск был удачен, то делаем замену
                            (car new_lst) ; иначе в результат помещается начало списка без изменений
                        )
                        result
                    )
                )
            )
        )
    )
)

; Упражнение 3
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace-3 replacement-pairs lst)
    (map
        (lambda(x)
            (let ((pat-rep (assoc x replacement-pairs)))
                (if pat-rep
                    (cadr pat-rep) ; если поиск был удачен, то делаем замену
                    x ; иначе в результат помещается начало списка без изменений
                )
            )
        )
        lst
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

; Упражнение 4
(define (history-answer responses)
    (append
        '(earlier you said that)
        (change-person (pick-random responses))
    )
)

; Вернуть #t если есть хотя бы одно ключевое слово
(define (keywords? user-response)
    (ormap
        (lambda (x) 
            (member x get-words)
        )
        user-response
    )
)

; Вернуть список ключевых слов в реплике
(define (find-keywords user-response)
    (filter
        (lambda (x) 
            (member x get-words)
        ) 
        user-response
    )
)

; Выбор шаблона по наличию ключевых слов
(define (pick-template user-response)
    (let*
        (
        (user-keywords (find-keywords user-response))
        (keyword (pick-random user-keywords))
        (template (get-template keyword))
        )
        (many-replace-3
            (list (list '* keyword))
            template
        )
    )
)

; Выбор шаблона по ключевому слову
(define (get-template keyword)
    (pick-random
        (foldl
            (lambda (x y)
                (if (member keyword (car x))
                    (foldl
                        cons
                        y
                        (cadr x)
                    )
                    y
                )
            
            )  
            '()
            templates
        )
    )
)

; Упражнение 6
(define templates
    '(
        (
            (depressed suicide exams university)
            (
                (when you feel depressed, go out for ice cream)
                (depression is a disease that can be treated)
                (don't think bad)
                (thoughts about * will go away with time)
	        )
        )
        (
            (mother father parents brother sister uncle ant grandma grandpa )
            (
                (tell me more about your * , i want to know all about your *)
                (why do you feel that way about your * ?)
                (tell me more about it)
                (* wants to see you i'm sure)
	        )
        )
        (
            (university scheme lections)
	        (
                (your education is important)
                (how many time do you spend to learning ?)
                (* makes you smart)
                (why are you talking to me about education ?)
	        )
        )
        (
            (sport swim run football tennis jump basketball)
	        (
                (sport is an important part of your life)
                (do you like * ?)
                (sport makes you healthy)
	        )
        )
        (
            (work job employment service profession)
	        (
                (do you like your job ?)
                (what do you do after work ?)
                (work brings you income)
	        )
        )
    )
)

; Вернуть линейный список ключевых слов 
(define get-words
	(foldl 
	    (lambda (x y)
	        (foldl cons y (car x))
	    )
	    '()
        templates
    )
)

; запуск Доктора
(visit-doctor 'stop 3)
