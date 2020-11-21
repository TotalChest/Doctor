#lang scheme/base
(require racket/string)
(require 2htdp/batch-io)


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
                        (printf "what seems to be the trouble?\n")
                        (doctor-driver-loop name)
                        (clients-loop (- curr-count 1))
                    )
                )
            )
            (printf "time to go home\n")
        )
    )
)

; Упражнение 5
(define (ask-patient-name)
    (begin
        (printf "next!\n")
        (printf "who are you?\n")
        (print '>>>)
        (read-line)
    ) 
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (let responses-loop ((name name) (responses '()))
        (print '>>>) ; доктор ждёт ввода реплики пациента, приглашением к которому является >>>
        (let ((user-response (read-line)))
            (cond 
                ((equal? user-response "goodbye") ; реплика "goodbye" служит для выхода из цикла
                    (printf "Goodbye, ~a!\n" name)
                    (printf "see you next week"))
                (else
                    (let ((spliting-user-response (preprocess user-response))) ; Предобработка реплики
                        (printf
                            (join-string
                                (reply
                                    (linear spliting-user-response)
                                    responses
                                )
                            )
                        )
                        (newline)
                        (responses-loop name (foldl cons responses spliting-user-response))
                    )
                )
            )
        )
    )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response responses)
    (let
        (
            (
                lst
                (foldl
                    (lambda (x y)
                        (if ((car x) user-response responses)
                            (cons x y)
                            y
                        )
                    )
                    '()
                    responses-struct
                )
            )
        )
        (
            (pick-random-with-weight lst)
            user-response
            responses
        )
    )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
    (cons
        (pick-random 
            '("you seem to think that"
              "you feel that"
              "why do you believe that"
              "why do you say that"
              "you muttered that"
              "why are you telling me that"
              "it seemed to me or you said that")
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
        '(("am" "are")
          ("are" "am")
          ("i" "you")
          ("me" "you")
          ("mine" "yours")
          ("my" "your")
          ("myself" "yourself")
          ("you" "i")
          ("your" "my")
          ("yours" "mine")
          ("yourself" "myself"))
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
        '(("please go on")
          ("many people have the same sorts of feelings")
          ("many of my patients have told me the same thing")
          ("please continue")
          ("it is very common problem")
          ("do not stop")
          ("i understand you"))
    )
)

; Упражнение 4
(define (history-answer responses)
    (cons
        "earlier you said that"
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
            (list (list "*" keyword))
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
; Структра ответных реплик по ключевым словам
(define templates
    '(
        (
            ("depressed" "suicide" "exams" "university")
            (
                ("when you feel depressed, go out for ice cream")
                ("depression is a disease that can be treated")
                ("don't think bad")
                ("thoughts about" "*" "will go away with time")
            )
        )
        (
            ("mother" "father" "parents" "brother" "sister" "uncle" "ant" "grandma" "grandpa")
            (
                ("tell me more about your" "*" ", i want to know all about your" "*")
                ("why do you feel that way about your" "*" "?")
                ("tell me more about it")
                ("*" "wants to see you i'm sure")
            )
        )
        (
            ("university" "scheme" "lections")
            (
                ("your education is important")
                ("how many time do you spend to learning?")
                ("*" "makes you smart")
                ("why are you talking to me about education?")
            )
        )
        (
            ("sport" "swim" "run" "football" "tennis" "jump" "basketball")
            (
                ("sport is an important part of your life")
                ("do you like" "*" "?")
                ("sport makes you healthy")
            )
        )
        (
            ("work" "job" "employment" "service" "profession")
            (
                ("do you like your job?")
                ("what do you do after work?")
                ("work brings you income")
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

; Упражнение 7
; Структура данных для стратегий построения реплик
(define responses-struct
    (list
        (list
            (lambda (response history) #t)
            1
            (lambda (response history) (hedge))
        )
        (list
            (lambda (response  history) #t)
            2
            (lambda (response history) (qualifier-answer response))
        )
        (list
            (lambda (response history) (not (null? history)))
            1
            (lambda (response history) (history-answer history))
        )
        (list
            (lambda (response history) (keywords? response))
            20
            (lambda (response history) (pick-template response))
        )
        (list
            (lambda (response history) #t)
            5
            (lambda (response history) (forward-generation))
        )
        (list
            (lambda (response history) #t)
            5
            (lambda (response history) (backward-generation))
        )
        (list
            (lambda (response history) #t)
            20
            (lambda (response history) (advanced-generation response))
        )
    )
)

; Случайный элемент с учетом весов
(define (pick-random-with-weight lst)
    (let loop
        (
            (target
                (random
                    (foldl
                        (lambda (x y)
                            (+ (cadr x) y)
                        )
                        0
                        lst
                    )
                )
            )
            (cur-lst lst)
        )
        (let ((new_target (- target (cadar cur-lst))))
            (if (>= new_target 0)
                (loop new_target (cdr cur-lst))
                (caddar cur-lst)
            )
        )
    )
)

; Весна
; Создание списка списков из реплики
(define (preprocess user-response)
    (foldl 
        (lambda (x y) 
            (let
                (
                    (sentence
                        (filter
                            (lambda (z) (not (equal? z "")))
                            (regexp-split #px"\\s+" x) ; Разделяем по пробельным символам   
                        )
                    )
                )
                (if (null? sentence) y (cons sentence y))
            )
        )
        '()
        (regexp-split
            #px"[\\.\\?!]" ; Разделение по предложенииям
            (regexp-replace*
                #px"([;,:\\(\\)])" ; Добавление разделителей для знаков
                (regexp-replace*
                    #px"[^\\w\\.\\?!;,:\\s\\(\\)]+" ; Очистка от лишних символов
                    user-response
                    ""
                )
                " \\1 "
            )
        )
    )
)

; Объединение списка в предложение
(define (join-string lst)
    (regexp-replace*
        #px"([\\(]) "
        (regexp-replace*
            #px" ([;,:\\)])"
            (string-join lst)
            "\\1"
        )
        "\\1"
    )        
)

; Линеризация списка списков
(define (linear lst)
    (reverse
        (foldl
            (lambda (x y)
                (foldl cons y x)
            )
            '()
            lst    
        )
    )
)


; Стратегия прямой генерации ответа
(define (forward-generation)
    (let ((start (pick-random-with-weight starts)))
        (append start (forward-generation-2 start))
    )
)
(define (forward-generation-2 start)
    (let loop ((result '()) (prev start))
        (let ((last-word (pick-random-with-weight (hash-ref forward-graph prev (list(list 'None 1 "$NONE$"))))))
            (if (or (equal? last-word ".") (equal? last-word "$NONE$"))
                result
                (loop
                    (append result (list last-word))
                    (append (cdr prev) (list last-word))
                )
            )
        )
    )
)

; Стратегия обратной генерации ответа
(define (backward-generation)
    (let ((end (pick-random-with-weight ends)))
         (append (backward-generation-2 end) end)
    )
)
(define (backward-generation-2 end)
    (let loop ((result '()) (post end))
        (let ((first-word (pick-random-with-weight (hash-ref backward-graph post (list(list 'None 1 "$NONE$"))))))
            (if (or (equal? first-word ".") (equal? first-word "$NONE$"))
                result
                (loop
                    (append (list first-word) result)
                    (append (list first-word) (reverse (cdr (reverse post))))
                )
            )
        )
    )
)

; Двухсторонняя стратерия генерации ответа
(define (advanced-generation response)
    (define (num-members keys)
        (let loop ((keys keys) (res 0))
            (if (null? keys)
                res
                (if (member (car keys) response)
                    (loop (cdr keys) (+ res 1))
                    (loop (cdr keys) res)
                )
            )
        )
    )
    (define (freq-keys)
        (let loop ((keys (hash-keys forward-graph)) (result '()))
            (if (null? keys)
                (if (null? result)
                    (list (list 'None 1 (pick-random (hash-keys forward-graph))))
                    result
                )
                (let ((num (num-members (car keys))))
                    (if (> num 0)
                        (loop (cdr keys) (cons (list 'None num (car keys)) result))
                        (loop (cdr keys) result)
                    )
                )
            )
        )
    )
    (let ((start (pick-random-with-weight (freq-keys))))
        (append (backward-generation-2 start) start (forward-generation-2 start))
    )
)


; Чтение начальных и конечных реплик из n-грамм с частотами
(define (read-struct file)
    (let ((prepare-string (substring (read-file file) 2)))
        (map
            (lambda (x)
                (let ((lst (regexp-split #px" " x)))
                    (list 'None (string->number (car lst)) (cdr lst))
                )
            )
            (regexp-split
                #px" \\| "
                prepare-string
            )
        )
    )
)

; Чтение графов из n-грамм с частотами
(define (read-graph file)
    (let ((prepare-string (substring (read-file file) 2)) (hash (make-hash)))
        (map
            (lambda (x)
                (let ((lst (regexp-split #px" \\$ " x)))
                    (hash-set!
                        hash
                        (regexp-split #px" " (car lst))
                        (let loop ((words (cdr lst)) (result '()))
                            (if (null? words)
                                result
                                (let ((list-words (regexp-split #px" " (car words))))
                                    (loop (cdr words) (cons (list
                                        'None
                                        (string->number (cadr list-words))
                                        (car list-words))
                                    result))
                                )
                            )
                        )
                    )
                )
            )
            (regexp-split
                #px" \\| "
                prepare-string
            )
        )
        hash
    )
)

; Чтение начальных и конечных реплик
(define starts (read-struct "Graphs/starting.txt"))
(define ends (read-struct "Graphs/ending.txt"))

; Чтение графов
(define forward-graph (read-graph "Graphs/forward-graph.txt"))
(define backward-graph (read-graph "Graphs/backward-graph.txt"))

; запуск Доктора
(visit-doctor "stop" 3)
