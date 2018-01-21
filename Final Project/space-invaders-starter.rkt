;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


;; Space Invaders


;; Constants:
;; ================

(define WIDTH  300)
(define HEIGHT 500)
(define IXS 1.5)  ;invader x speed, speeds (not velocities) in pixels per tick
(define IYS 1.5)  ;invader y speed 
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)
(define HIT-RANGE 3)
(define INVADE-RATE 100)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer
(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define MISSILE (ellipse 5 15 "solid" "red"))


;; Data Definitions:
;; ================


;; Game
(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


;; Tank
(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


;; Invader
(define-struct invader (x y dir))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         dir is 1 for right, -1 for left

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvaders
;; is one of:
;;  - empty
;; (cons Invader ListOfInvaders)

(define LOI1 empty)
(define LOI2 (list (make-invader 150 100 10)))
(define LOI3 (list (make-invader 150 100 10) (make-invader 100 50 -10)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))


;; missile
(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissiles
;; is one of:
;;  - empty
;; (cons Missile ListOfMissiles)
;; interp. list of missiles

(define LOM1 empty)
(define LOM2 (list M1))
(define LOM3 (list M1 M2))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Examples of Game

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; Functions
;; ================

;; main
;; Game -> Game
;; main game engine

(define (main game)
  (big-bang game
    (on-tick   tock)
    (to-draw   render)
    (stop-when killed)
    (on-key    handler)))

;; tock
;; Game -> Game
;; advance tank, invaders, and missiles to next state
;; tests in each next-* function

;(define (tock game) game); stub

(define (tock game)
  (make-game (next-invaders (game-invaders game) (game-missiles game))
             (next-missiles (game-missiles game))
             (next-tank (game-tank game))))


;; next-invaders
;; ListOfInvaders -> ListOfInvaders
;; get list of next invaders
(check-expect (next-invaders empty empty) empty)
(check-expect (next-invaders (list (make-invader 0 0 1)) empty)
              (list (make-invader IXS IYS 1)))
(check-expect (next-invaders (list (make-invader WIDTH 0 -1)) empty)
              (list (make-invader (- WIDTH IXS) IYS -1)))
(check-expect (next-invaders (list (make-invader 0 0 -1)) empty)
              (list (make-invader IXS IYS 1)))
(check-expect (next-invaders (list (make-invader WIDTH 0 1)) empty)
              (list (make-invader (- WIDTH IXS) IYS -1)))
(check-expect (next-invaders (list (make-invader 0 0 1)
                                   (make-invader WIDTH 0 -1)
                                   (make-invader 0 0 -1)
                                   (make-invader WIDTH 0 1)) empty)
              (list (make-invader IXS IYS 1)
                    (make-invader (- WIDTH IXS) IYS -1)
                    (make-invader IXS IYS 1)
                    (make-invader (- WIDTH IXS) IYS -1)))
(check-expect (next-invaders (list (make-invader 50 50 1)) (list (make-missile 50 51)))
              empty)

;(define (next-invaders invaders) invaders); stub

(define (next-invaders loi lom)
  (if (empty? loi)
      (if (= (random INVADE-RATE) 1)
          (cons (make-invader (random WIDTH) 0 1) empty)
          empty)
      (if (destroyed? (first loi) lom)
          (next-invaders (rest loi) lom)
          (cons (advance-invader (first loi))
                (next-invaders (rest loi) lom)))))


;; destroyed?
;; Invader ListOfMissiles -> Boolean
;; Return true if Invader should be destroyed, else false
(check-expect (destroyed? (make-invader 0 0 1) empty) false)
(check-expect (destroyed? (make-invader 0 0 1) (list (make-missile (+ 0 HIT-RANGE) (+ 0 HIT-RANGE)))) true)
(check-expect (destroyed? (make-invader 10 10 1) (list (make-missile (- 10 HIT-RANGE) (- 10 HIT-RANGE)) (make-missile 50 50))) true)
(check-expect (destroyed? (make-invader 0 0 1) (list (make-missile 50 50) (make-missile 100 100))) false)

;(define (destroyed? invader lom) false)

(define (destroyed? invader lom)
  (cond [(empty? lom) false]
        [else
         (if (and (and (<= (- (missile-x (first lom)) (invader-x invader)) HIT-RANGE)
                       (>= (- (missile-x (first lom)) (invader-x invader)) (- 0 HIT-RANGE)))
                  (<= (- (missile-y (first lom)) (invader-y invader)) HIT-RANGE))
             true
             (destroyed? invader (rest lom)))]))


;; advance-invader
;; Invader -> Invader
;; produce next invader
;; tests in next-invaders

;(define (advance-invader invader) invader); stub

(define (advance-invader invader)
  (if (> (invader-dir invader) 0) ; if moving right
      (if (>= (invader-x invader) WIDTH) ; and if equal to width or over
          (make-invader (- (invader-x invader) IXS) (+ (invader-y invader) IYS) -1) ; change left
          (make-invader (+ (invader-x invader) IXS) (+ (invader-y invader) IYS) 1)) ; continue right
      (if (<= (invader-x invader) 0) ; if moving left and equal to 0 or under
          (make-invader (+ (invader-x invader) IXS) (+ (invader-y invader) IYS) 1) ; change right
          (make-invader (- (invader-x invader) IXS) (+ (invader-y invader) IYS) -1)))) ; continue left


;; next-missiles
;; ListOfMissiles -> ListOfMissiles
;; advance each missile by MISSILE-SPEED
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (list (make-missile 0 HEIGHT))) (list (make-missile 0 (- HEIGHT MISSILE-SPEED))))
(check-expect (next-missiles (list (make-missile 0 HEIGHT) (make-missile 5 HEIGHT)))
              (list (make-missile 0 (- HEIGHT MISSILE-SPEED)) (make-missile 5 (- HEIGHT MISSILE-SPEED))))
(check-expect (next-missiles (list (make-missile 0 0))) empty)

;(define (next-missiles missiles) missiles); stub

(define (next-missiles missiles)
  (cond [(empty? missiles) empty]
        [else
         (if (empty? (advance-missile (first missiles)))
             (next-missiles (rest missiles))
             (cons (advance-missile (first missiles)) (next-missiles (rest missiles))))]))


;; advance-missile
;; Missile -> Missile
;; produce next missile
;; tests in next-missiles

;(define (advance-missile missile) missile)

(define (advance-missile missile)
  (if (< (missile-y missile) 1)
      empty
      (make-missile (missile-x missile) (- (missile-y missile) MISSILE-SPEED))))


;; next-tank
;; Tank -> Tank
;; produces next tank
(check-expect (next-tank (make-tank 0 -1)) (make-tank 0 -1))
(check-expect (next-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (next-tank (make-tank 0 1)) (make-tank TANK-SPEED 1))
(check-expect (next-tank (make-tank WIDTH -1)) (make-tank (- WIDTH TANK-SPEED) -1))

;(define (next-tank tank) tank); stub

(define (next-tank tank)
  (make-tank
   (if (= (tank-dir tank) 1)
       (if (> (+ (tank-x tank) TANK-SPEED) WIDTH)
           WIDTH
           (+ (tank-x tank) TANK-SPEED))
       (if (< (- (tank-x tank) TANK-SPEED) 0)
           0
           (- (tank-x tank) TANK-SPEED)))
   (tank-dir tank)))


;; render
;; Game -> Image
;; make game images
(check-expect (render (make-game empty empty T0))
              (place-image (square 0 "solid" "white") 0 0
                           (place-image (square 0 "solid" "white") 0 0
                                        (place-image TANK
                                                     (/ WIDTH 2)
                                                     (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))
(check-expect (render (make-game (list (make-invader 0 0 1)) empty T0))
              (place-image INVADER 0 0
                           (place-image (square 0 "solid" "white") 0 0
                                        (place-image TANK
                                                     (/ WIDTH 2)
                                                     (- HEIGHT TANK-HEIGHT/2)
                                                     BACKGROUND))))
(check-expect (render (make-game (list (make-invader 0 0 1) (make-invader 50 50 -1)) empty T0))
              (place-image INVADER 0 0
                           (place-image INVADER 50 50
                                        (place-image (square 0 "solid" "white") 0 0
                                                     (place-image TANK
                                                                  (/ WIDTH 2)
                                                                  (- HEIGHT TANK-HEIGHT/2)
                                                                  BACKGROUND)))))
(check-expect (render (make-game (list (make-invader 100 10 1) (make-invader 50 50 -1))
                         (list (make-missile 60 10))
                         T0))
              (place-image INVADER 100 10
                           (place-image INVADER 50 50
                                        (place-image MISSILE 60 10
                                                     (place-image TANK
                                                                  (/ WIDTH 2)
                                                                  (- HEIGHT TANK-HEIGHT/2)
                                                                  BACKGROUND)))))
(check-expect (render (make-game (list (make-invader 100 10 1) (make-invader 50 50 -1))
                         (list (make-missile 60 10) (make-missile 80 40))
                         T0))
              (place-image INVADER 100 10
                           (place-image INVADER 50 50
                                        (place-image MISSILE 60 10
                                                     (place-image MISSILE 80 40
                                                                  (place-image TANK
                                                                               (/ WIDTH 2)
                                                                               (- HEIGHT TANK-HEIGHT/2)
                                                                               BACKGROUND))))))

;(define (render game) (square 0 "solid" "white"))

(define (render game)
   (render-missiles (game-missiles game)
                    (game-invaders game)
                    (game-tank game)))

;; render-tank
;; Tank Image -> Image
;; tests in render

;(define (render-tank tank bg) (square 0 "solid" "white"))

(define (render-tank tank)
  (place-image TANK (tank-x tank) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; render-invaders
;; ListOfInvaders Image -> Image
;; Consumes image of tank(w/ background) and places invaders
;; tests in render

;(define (render-invaders loi) (square 0 "solid" "white"))

(define (render-invaders loi tank)
  (cond [(empty? loi) (render-tank tank)]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) tank))]))


;; render-missiles
;; ListOfMissiles Image -> Image
;; Consumes image of tank, invaders, background and places missiles
;; tests in render

;(define (render-missiles lom) (square 0 "solid" "white"))

(define (render-missiles lom loi tank)
  (cond [(empty? lom) (render-invaders loi tank)]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) loi tank))]))


;; killed
;; Game -> Boolean
;; return true if should die
(check-expect (killed (make-game empty empty T0)) false)
(check-expect (killed (make-game (list (make-invader 0 0 1)) empty T0)) false)
(check-expect (killed (make-game (list (make-invader 0 HEIGHT 1)) empty T0)) true)
(check-expect (killed (make-game (list (make-invader 0 HEIGHT 1) (make-invader 0 0 1)) empty T0)) true)
(check-expect (killed (make-game (list (make-invader 0 0 1) (make-invader 10 HEIGHT -1)) empty T0)) true)

;(define (killed game) false)

(define (killed game)
  (invaded? (game-invaders game)))


;; invaded?
;; ListOfInvaders -> Boolean
;; return true if one of the invaders reaches HEIGHT
;; tests in killed

;(define (invaded loi) false)

(define (invaded? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (invaded? (rest loi)))]))


;; handler
;; Game KeyEvent -> Game
;; produce new game based on keyevent
(check-expect (handler (make-game empty empty (make-tank 0 1)) "left")
              (make-game empty empty (make-tank 0 -1)))
(check-expect (handler (make-game empty empty (make-tank WIDTH -1)) "right")
              (make-game empty empty (make-tank WIDTH 1)))
(check-expect (handler (make-game empty empty T0) " ")
              (make-game empty (list (make-missile (tank-x T0) (- HEIGHT (image-height TANK)))) T0))
(check-expect (handler (make-game empty (list (make-missile 50 100)) T0) " ")
              (make-game empty
                         (list (make-missile (tank-x T0) (- HEIGHT (image-height TANK))) (make-missile 50 100))
                         T0))

;(define (handler game ke) game)

(define (handler game ke)
  (cond [(key=? ke "left") (make-game (game-invaders game)
                                      (game-missiles game)
                                      (make-tank (tank-x (game-tank game)) -1))]
        [(key=? ke "right") (make-game (game-invaders game)
                                       (game-missiles game)
                                       (make-tank (tank-x (game-tank game)) 1))]
        [(key=? ke " ") (make-game (game-invaders game)
                                   (add-missile (game-missiles game) (game-tank game))
                                   (game-tank game))]
        [else game]))

;; add-missile
;; ListOfMissiles Tank -> ListOfMissiles
(check-expect (add-missile empty T1) (cons (make-missile 50 (- HEIGHT (image-height TANK))) empty))
(check-expect (add-missile (list M1) T1) (list (make-missile 50 (- HEIGHT (image-height TANK))) M1)) 

;(define (add-missile lom tank) lom)

(define (add-missile lom tank)
  (cons (make-missile (tank-x tank) (- HEIGHT (image-height TANK))) lom))
