#lang racket

;;;;Аверков Всеволод
;; Пауза на P
;; Запись очков в файл (есть проблема с повторной записью 
; (define (tofile  file  score)
;   (write-to-file  score file
;     #:exists  'update
;    #:mode 'text
;      )) )

;; Смена режимов игры (кнопки 1 и 2)
;; запуск (game)
;;Сделана телепортация яблок по таймеру
  

;;ЛИТЕРАТУРА

;;http://picturingprograms.com/
;;https://htdp.org/2003-09-26/Book/curriculum-Z-H-1.html
(require picturing-programs)
(require racket/file)
(require htdp/gui)
(require htdp/dir)
(require racket/sandbox)
(struct area (snake apple) #:transparent)
;ВСЕ ВМЕСТЕ

(struct snake (dir segs) #:transparent)
;Голова змеи , и куски .

(struct apple (loc expire) #:transparent)
;; Поле expire - это натуральное число, представляющее число
;;
(struct posn (x y) #:transparent)
;; Скорость
(define SPEED 0.1)
(define-values ( PAUSE?) #f)
;;Границы
(define SIZE 30)

(define path 15)

;; Константа времени  для яблок

(define EXPIRATION-TIME 150)

;; Область
(define WIDTH-PX  (* path 30))
(define HEIGHT-PX (* path 30))

;; Визуальная часть
;;ШРИФТ
(define END 50)
(define MESSAGE(text "Для смены режима нажмите 1 или 2. Пауза на 'p'. Выход 'q'." 15 "yellow"))
(define MAIN (place-image/align MESSAGE  20 5 "left" "top"
                                (place-image/align (rectangle 450 25 "solid" "blue") 0 0 "left" "top"
                                                   (empty-scene WIDTH-PX HEIGHT-PX "black"))))

(define APPLE-IMG (scale 0.020 (bitmap "44.png")))
(define SEG-IMG  (square 10 "solid" "yellow"))
(define HEAD-IMG (square 10 "solid" "red"))
(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))


;;;BIGBANG
(define (s)
  (big-bang (area (snake  "down" (list (posn 1 4)))
                  (list (fresh-apple)
                        (fresh-apple)
                        (fresh-apple)
                        (fresh-apple)))
    (on-tick next SPEED)
    (name "Exam Project") 
    (on-key direct-snake)
    (to-draw render-area)
    (stop-when hit? render-end)
    (close-on-stop 5)))



(define (game)
  (tofile "Score.txt"(length(drop-right(snake-segs(area-snake(s))))))
  (game))


;; Есть или идти (шаг)
(define (next w)
  (define snake (area-snake w))
  (define apples  (area-apple w))
  (define apple-to-eat (can-eat snake apples))
  (if PAUSE? w 
      (if apple-to-eat
          (area (grow snake) (age-apple (eat apples apple-to-eat)));;area(snake apple score +1)
          (area (slither snake) (age-apple apples)))))

;;обработка нажатий
(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [( gamemode ke w)]
        [else w]))


;; загрузка мира (сцена)
(define (render-area w )
  (snake+scene (area-snake w)
               (apple-list+scene (area-apple w) MAIN)))


;; ПРОВЕРКА НА СТОЛКНОВЕНИК
(define (hit? w)
  (define snake (area-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

;; КОНЕЦ ИГРЫ
(define (render-over)
  (text "GAME OVER"  END  "RED"))

(define (pause w )
  (set! PAUSE? (not PAUSE? )) w )

;; Пауза и режимы ,Выход 
(define (gamemode d w)
  ( cond
     [(key=? d "1")
      (area (snake  "down" (list (posn  1 4)))
            (list (fresh-apple)
                  (fresh-apple)
                  (fresh-apple)
                  (fresh-apple)))]
     [ (key=? d "2")
       (area (snake  "down" (list (posn  1 4)))
             (list (fresh-apple)))]
     [(key=? d "p")
          
      (pause w) ]
        
     [(key=? d "q") (exit)] 
     [else w]
     )
  )

(define (rendersnake w)
  (text (string-append 
         "Съел:" 
         (number->string (length(drop-right(snake-segs (area-snake w))))))
        END
        "PURPLE"))

;;Рендер геймовера
(define (render-end w )
  (overlay (overlay/offset (render-over)
                           0 END
                           (rendersnake w))
           (render-area w)))

; (define (tofile )
; 
; (display-to-file (length(OUT-ONE(snake-segs (area-snake x))))) 
; 
;     (string-append
;      (path->string
;       (current-directory))
;                             "snake/Score.txt"))



; (define out (open-output-file file 'require))
; 
;      (define (o  file score)
;         (display   score out)
;          (close-output-port out)
;        )

       
(define (tofile  file  score)
(write-to-file  score file   #:exists  'update
                  #:mode 'text))

;; ЕДА РОСТ

;;Может ли змея что то скушать 
;; > (can-eat (snake "right" `(,(posn 3 3))) `(,(apple (posn 3 3) 130)))
;; (apple (posn 3 3) 130)
(define (can-eat snake apple)
  (cond [(empty? apple) #f]
        [else (if (close? (snake-head snake) (first apple))
                  (first apple)
                  (can-eat snake (rest apple)))]))

(define  (sum  y)
  (+ 1 y))
;; 
;; Едим яблоко и пополняем
;; > (eat (list (apple (posn 5 5) 5)) (apple(posn 5 5) 5))
;;(list (apple (posn 14 12) 150))
(define (eat apples apple-to-eat)
  ;  (set! score (+ 1 score))
   
  (cons (fresh-apple) (remove apple-to-eat apples)))

;; Проверка на яблоко
;; > (close? (posn 1 2) (apple (posn 1 2) 4))
;; #t
(define (close? s g)
  (posn=? s (apple-loc g)))

;; Наращивание хвоста


;>(snake "right" `(,(posn 2 1) ,(posn 1 1)))
;(snake "right" (list (posn 2 1) (posn 1 1)))
;> (grow (snake "right" `(,(posn 2 1) ,(posn 1 1))))
;(snake "right" (list (posn 3 1) (posn 2 1) (posn 1 1)))
;> 
(define (grow sn)
  (snake (snake-dir sn) (cons (next-head sn) (snake-segs sn))))

;;;;;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;; ДВИЖЕНИЕ 
;; на один вперед

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (drop-right (snake-segs sn)))))

;; Вычисляет следующую позицию головы змеи.
(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

;; Перемещение позиции 
;; > (posn-move (posn 1 1) 2 3)
;; (posn 3 4)
(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))


;;Возвращает список, который не содержит последний элемент данного списка.
;; > (dropright '(1 2 3 4))
;; '(1 2 3)
(define (drop-right segs)
  (cond [(empty? (rest segs))
         '()]
        [else (cons (first segs) 
                    (drop-right (rest segs)))]))


;; 
;;обновление или  истечение времени 
(define (age-apple apples)
  (exp (renew apples)))

;; 
;; обновляет яблоки 
(define (renew apple)
  (cond [(empty? apple) empty]
        [(done? (first apple))
         (cons (fresh-apple) (renew (rest apple)))]
        [else
         (cons (first apple) (renew (rest apple)))]))

;;  
;; 
(define (exp g)
  (cond [(empty? g)
         '()]
        [else (cons (countdown (first g))
                    (exp (rest g)))]))

;; 
;; срок годности 
;; > (done? (apple 1 2)
;; #t
(define (done? g)
  (zero? (apple-expire g)))

;; 
;; ументшает  на 1 счетсик
;; > (countdown (apple (posn 1 2) 2))
;; (apple(posn 1 2) 1)
(define (countdown g)
  (apple (apple-loc g) (sub1 (apple-expire g))))

;; 
;; Рандом для яблок 
;; 
;; ,
;;  
(define (fresh-apple)
  (apple (posn (add1 (random (sub1 SIZE)))
               (add1 (random   2 (sub1 SIZE))))EXPIRATION-TIME))


;; тип напрадения 
;; > (dir? "up")
;; #t
(define (dir? x)
  (or (string=? x "up") 
      (string=? x "down") 
      (string=? x "left") 
      (string=? x "right")
      ))
      
;; 
;; Измените направление если не напротив текущей позиции)
;;
(define (world-change-dir w d)
  (define the-snake (area-snake w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d) 
              ;;  состоит из головы и хотя бы одного сегмента
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else 
         (area (snake-change-dir the-snake d) 
               (area-apple  w))]))

;; 
;; 
;;Являются ли d1 и d2 противоположными?
;; > (opposite-dir? "up" "down")
;; #t
(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up")    (string=? d2 "down")]
        [(string=? d1 "down")  (string=? d2 "up")]
        [(string=? d1 "left")  (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

;; 
;; РИСУЮ ЗМЕЮ от положения
;; > 
(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene  (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake) 
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))





;; рисую яблоки
(define (apple-list+scene apple scene)
  ;;
  ;;ПОЗИЦИИ ЯБЛОК
  ;; > (get-posns-from-apple (list (apple (posn 2 2) 1) (apple (posn 3 3) 1))
  ;; (list (posn 2 2) (posn 3 3))
  (define (get-posns-from-apple apple)
    (cond [(empty? apple)
           '()]
          [else (cons (apple-loc (first apple))
                      (get-posns-from-apple (rest apple)))]))
  (img-list+scene (get-posns-from-apple apple) APPLE-IMG scene))
;;(posn-y(apple-loc(first(list (apple (posn 2 2) 1) (apple (posn 3 3) 1)))))
;; 
;; Рисует изображение для каждой позиции в списке
;; > (img-list+scene (list (posn 1 1)) APPLE-IMG MAIN)
;; (place-image APPLE-IMG 8 8
;;              (img-list+scene empty APPLE-IMG MAIN))
(define (img-list+scene posns img scene)
  (cond[(empty? posns) scene]
       [else (img+scene (first posns)
                        img 
                        (img-list+scene (rest posns) img scene))]))

;; 
;; 
;;Рисует данное изображение на сцене .
;; > (img+scene (posn 2 2) APPLE-IMG MAIN)
;; (place-image APPLE-IMG 32 32 MAIN)

(define (img+scene posn img scene)
  (place-image img 
               (* (posn-x posn) path)
               (* (posn-y posn) path)
               scene))
 

;;Опред, сталкивается ли змея с собой
;; > (self-colliding? (snake "up" (list (posn 1 1) (posn 2 1)
;;                                      (posn 2 2) (posn 1 2)
;;                                      (posn 1 1))))
;; #t
(define (self-colliding? sn)
  (cons? (member (snake-head sn) (snake-body sn))))

;;Сопределяет сталкивается земля с какой либо из стен
;; > (wall-colliding? (snake "up" (list (posn 0 1))))
;; #t
(define (wall-colliding? sn)
  (define x (posn-x (snake-head sn)))
  (define y (posn-y (snake-head sn)))
  (or (= 0 x) (= x SIZE)
      (= 2 y) (= y  SIZE)))

;;Одинаковы ли два положения?
;; > (posn=? (posn 1 1) (posn 1 1))
;; true
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;; Достуа к позиции змеи
;; > (snake-head (snake "right" (list (posn 1 1) (posn 2 1)))
;; (posn 1 1)
(define (snake-head sn)
  (first (snake-segs sn)))

;; (snake-body (snake "right" (list (posn 30 22) (posn 29 22) (posn 28 22) (posn 27 22) (posn 26 22))))
;;(list (posn 29 22) (posn 28 22) (posn 27 22) (posn 26 22))
;; возвращает тело змеи,кроме головы
(define (snake-body sn)
  (rest (snake-segs sn)))

;; Смена расположения змеи 
(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))

; > (s)
; (area
;  (snake "right" (list (posn 30 22) (posn 29 22) (posn 28 22) (posn 27 22) (posn 26 22)))
;  (list (apple (posn 28 26) 133) (apple (posn 19 4) 112) (apple (posn 28 29) 56) (apple (posn 14 26) 56)))
; > (area
;  (snake "right" (list (posn 30 22) (posn 29 22) (posn 28 22) (posn 27 22) (posn 26 22)))
;  (list (apple (posn 28 26) 133) (apple (posn 19 4) 112) (apple (posn 28 29) 56) (apple (posn 14 26) 56)))
; (area
;  (snake "right" (list (posn 30 22) (posn 29 22) (posn 28 22) (posn 27 22) (posn 26 22)))
;  (list (apple (posn 28 26) 133) (apple (posn 19 4) 112) (apple (posn 28 29) 56) (apple (posn 14 26) 56)))
; > (define s0 (area
;  (snake "right" (list (posn 30 22) (posn 29 22) (posn 28 22) (posn 27 22) (posn 26 22)))
;  (list (apple (posn 28 26) 133) (apple (posn 19 4) 112) (apple (posn 28 29) 56) (apple (posn 14 26) 56))))
; > s0
; (area
;  (snake "right" (list (posn 30 22) (posn 29 22) (posn 28 22) (posn 27 22) (posn 26 22)))
;  (list (apple (posn 28 26) 133) (apple (posn 19 4) 112) (apple (posn 28 29) 56) (apple (posn 14 26) 56)))
; > (snake-segs (area-snake s0))
; (list (posn 30 22) (posn 29 22) (posn 28 22) (posn 27 22) (posn 26 22))
; > (begin (match-define (area (snake dir (list head body ...)) apple) s0)
;          body)
; (list (posn 29 22) (posn 28 22) (posn 27 22) (posn 26 22))
; > (s)
; (area
;  (snake "left" (list (posn 0 4) (posn 1 4) (posn 2 4) (posn 3 4) (posn 4 4)))
;  (list (apple (posn 9 29) 136) (apple (posn 1 26) 126) (apple (posn 22 3) 119) (apple (posn 8 26) 86)))
; > 
; 