;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

Snake: The Game
Katrina Brown, Annie Pompa

|#

;; imports
(require 2htdp/image)
(require 2htdp/universe)

;; data definitions and constants
;; A Direction is one of:
;; - "w"
;; - "a"
;; - "s"
;; - "d"
;; - "up"
;; - "down"
;; - "right"
;; - "left"

;; A World is a (make-world [Direction [ListOf Posn] Posn])
(define-struct world [dir lop food])

(define num-squares 30)
(define factor 8)
(define size 405)

;; cool-color : Number Number -> Color
;; returns a color from two numbers
(define (cool-color n1 n2)
  (make-color (* factor n1) (* factor n2) 100))

;; TODO see if this can be a constant instead of what is is now
;; ls-cool-colors : Anything -> [ListOf [ListOf Color]]
;; generates a list of lists of colors when given anything
(define (ls-cool-colors a)
  (local[
         ;; ls : Number -> [ListOf Number]
         ;; returns a list of number with the first arg of color constructor factor * num
         (define (ls num) (build-list num-squares (λ (x) (cool-color num x))))
         ]
    (build-list num-squares (λ (x) (ls x)))))

;; rec-bg : Number Number Image -> Image
(define (rec-bg n1 n2 img)
  (place-image (square 15 "solid" (list-ref (list-ref colors-grid n2) n1))
               (* 15 n1) (* 15 n2) img))

;; place-row : Number Number Image -> Image
(define (place-row n1 n2 img)
  (cond
    [(= n1 -1) img]
    [(= n2 -1) (place-row (sub1 n1) (- num-squares 1) img)]
    [else (place-row n1 (sub1 n2) (rec-bg n1 n2 img))]))

;; functions above used to generate grid, along with grid colors and game attributes
(define colors-grid (ls-cool-colors "lol"))
(define bg (place-row (- num-squares 1) (- num-squares 1) (empty-scene size size)))
(define snake (square 15 'solid 'coral))
(define food (circle 7 'solid 'white))
(define new-food (make-posn (+ 15 (* 15 (random 25))) (+ 15 (* 15 (random 25)))))
(define p1 (make-posn 45 45))
(define init-world (make-world "s" (list p1 (make-posn 30 45) (make-posn 15 45)) (make-posn 75 75)))

;; remove-last : [Listof Posn] -> [Listof Posn]
;; removes the last posn in the given list
(define (remove-last lop)
  (cond
    [(or (empty? lop) (empty? (rest lop))) empty]
    [else (cons (first lop) (remove-last (rest lop)))]))

(check-expect (remove-last '()) '())
(check-expect (remove-last (list (make-posn 4 5))) '())
(check-expect (remove-last (list (make-posn 4 5) (make-posn 6 7))) (list (make-posn 4 5)))

;; add-lop : [ListOf Posn] Direction -> [ListOf Posn]
;; returns an updated list with a posn added based on Direction
(define (add-lop lop d)
  (cond
    [(or (string=? "up" d)
         (string=? "w" d)) (cons (make-posn (posn-x (first lop))
                                            (- (posn-y (first lop)) 15)) lop)]
    [(or (string=? "down" d)
         (string=? "s" d)) (cons (make-posn (posn-x (first lop))
                                            (+ 15 (posn-y (first lop)))) lop)]
    [(or (string=? "right" d)
         (string=? "d" d)) (cons (make-posn (+ 15 (posn-x (first lop)))
                                            (posn-y (first lop))) lop)]
    [(or (string=? "left" d)
         (string=? "a" d)) (cons (make-posn (- (posn-x (first lop)) 15)
                                            (posn-y (first lop))) lop)]
    [else lop]))

(check-expect (add-lop (list (make-posn 15 15)) "w")
              (list (make-posn 15 0) (make-posn 15 15)))
(check-expect (add-lop (list (make-posn 15 15)) "up")
              (list (make-posn 15 0) (make-posn 15 15)))
(check-expect (add-lop (list (make-posn 15 15)) "down")
              (list (make-posn 15 30) (make-posn 15 15)))
(check-expect (add-lop (list (make-posn 15 15)) "s")
              (list (make-posn 15 30) (make-posn 15 15)))
(check-expect (add-lop (list (make-posn 15 15)) "right")
              (list (make-posn 30 15) (make-posn 15 15)))
(check-expect (add-lop (list (make-posn 15 15)) "d")
              (list (make-posn 30 15) (make-posn 15 15)))
(check-expect (add-lop (list (make-posn 15 15)) "left")
              (list (make-posn 0 15) (make-posn 15 15)))
(check-expect (add-lop (list (make-posn 15 15)) "a")
              (list (make-posn 0 15) (make-posn 15 15)))
(check-expect (add-lop (list (make-posn 5 5)) "t")
              (list (make-posn 5 5)))

;; move-lop : [ListOf Posn] Direction -> [ListOf Posn]
;; removes the last posn in the list and adds a posn to the head based on Direction
(define (move-lop lop dir)
  (remove-last (add-lop lop dir)))

(check-expect (move-lop (list (make-posn 15 15)) "w") (list (make-posn 15 0)))

;; move : World -> [ListOf Posn]
;; returns the list from the input World after move-lop is applied
(define (move w)
  (move-lop (world-lop w) (world-dir w)))

(check-expect (move init-world) (list (make-posn 45 60) (make-posn 45 45) (make-posn 30 45)))

;; update-food : World -> World
;; returns an updated World based on if the food has been eaten
(define (update-food w)
  (cond
    [(is-eating? w) (make-world (world-dir w)
                                (add-lop (world-lop w) (world-dir w))
                                new-food)]
    [else w]))

(check-expect (update-food init-world)
              (make-world
               "s"
               (list (make-posn 45 45) (make-posn 30 45) (make-posn 15 45))
               (make-posn 75 75)))

;; tick : World -> World
;; returns an updated World that moves snake based on direction pressed
(define (tick w)
  (update-food (make-world (world-dir w) (move w) (world-food w))))

(check-expect (tick init-world)
              (make-world "s"
                          (list (make-posn 45 60) (make-posn 45 45) (make-posn 30 45))
                          (make-posn 75 75)))

;; is-eating? : World -> Boolean
;; returns whether the snake is eating the power up
(define (is-eating? w)
  (lop-overlap? (world-lop w) (world-food w)))

(check-expect (is-eating? init-world) #f)

;; lop-overlap? : [ListOf Posn] Posn -> Boolean
;; returns if the given lop and posn ever overlap
(define (lop-overlap? lop p)
  (cond
    [(empty? lop) false]
    [else (or (posn=? (first lop) p) (lop-overlap? (rest lop) p))]))

(check-expect (lop-overlap? (list (make-posn 15 15)) (make-posn 15 15)) #t)
(check-expect (lop-overlap? empty (make-posn 15 15)) #f)

;; posn=? : Posn Posn -> Boolean
;; determines if the given posns are equal
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2)) (= (posn-y p1) (posn-y p2))))

(check-expect (posn=? (make-posn 30 35) (make-posn 15 30)) #f)
(check-expect (posn=? (make-posn 15 30) (make-posn 15 30)) #t)

;; key : World KeyEvent -> World
;; on-key handler for snake, updates direction of snake in world
(define (key w ke)
  (cond
    [(or (string=? "w" ke) (string=? "a" ke) (string=? "s" ke) (string=? "d" ke)
         (string=? "right" ke) (string=? "down" ke) (string=? "left" ke) (string=? "up" ke))
     (make-world ke (world-lop w) (world-food w))]
    [else w]))

(check-expect (key init-world "w")
              (make-world "w" (world-lop init-world) (world-food init-world)))
(check-expect (key init-world "a")
              (make-world "a" (world-lop init-world) (world-food init-world)))
(check-expect (key init-world "s")
              (make-world "s" (world-lop init-world) (world-food init-world)))
(check-expect (key init-world "d")
              (make-world "d" (world-lop init-world) (world-food init-world)))
(check-expect (key init-world "right")
              (make-world "right" (world-lop init-world) (world-food init-world)))
(check-expect (key init-world "down")
              (make-world "down" (world-lop init-world) (world-food init-world)))
(check-expect (key init-world "left")
              (make-world "left" (world-lop init-world) (world-food init-world)))
(check-expect (key init-world "up")
              (make-world "up" (world-lop init-world) (world-food init-world)))

;----------------------------------------------------------------------------------------------------
;; draw : [NEList-of Posn] -> Image
;; draws nelop on the background at position
(define (draw nelop)
  (cond
    [(empty? (rest nelop))
     (place-image snake
                  (posn-x (first nelop))
                  (posn-y (first nelop))
                  bg)]
    [else (place-image snake
                       (posn-x (first nelop))
                       (posn-y (first nelop))
                       (draw (rest nelop)))]))

(check-expect (draw (list (make-posn 20 30)))
              (place-image snake 20 30 bg))
(check-expect (draw (list (make-posn 20 50) (make-posn 35 50)))
              (place-image snake 20 50
                           (place-image snake 35 50 bg)))

(define snakes (list (make-posn 20 50) (make-posn 35 50)))

;; draw-enemy : Posn [NEList-of Posn] -> Image
;; draws food and snake
(define (draw-enemy p n)
  (place-image food (posn-x p) (posn-y p) (draw n)))

(check-expect (draw-enemy (make-posn 15 15) (list (make-posn 30 30)))
              (place-image food 15 15 (draw (list (make-posn 30 30)))))

;; draw-world : World -> Image
;; returns an image with snake, score, and food drawn
(define (draw-world w)
  (place-image
   (text (string-append "Score: " (number->string (score (world-lop w))))
         20 'coral)
   340 16
   (draw-enemy (world-food w) (world-lop w))))

(check-expect (draw-world init-world)
              (place-image
               (text
                (string-append "Score: " (number->string (score (world-lop init-world)))) 20 'coral)
               340 16
               (draw-enemy (world-food init-world) (world-lop init-world))))

;; score : [NElist-of Posn] -> Number
;; returns the score of the current game based on list length
(define (score nelop)
  (* 10 (sub1 (length nelop))))

(check-expect (score snakes) 10)

;; off? : Posn -> Boolean
;; determines if the posn is offscreen
(define (off? p)
  (not (and (< 0 (posn-x p) (image-width bg))
            (< 0 (posn-y p) (image-height bg)))))

(check-expect (off? (make-posn 30 40)) #f)
(check-expect (off? (make-posn -39 2)) #t)

(define dead (list (make-posn 30 30) (make-posn -3 29) (make-posn 20 49)))

;; offscreen? : lop -> boolean
;; determines if any posn in list is offscreeen
(define (offscreen? lop)
  (ormap off? lop))

(check-expect (offscreen? snakes) #f)
(check-expect (offscreen? dead) #t)

(define dead2 (list (make-posn 30 30) (make-posn 45 30) (make-posn 30 30)))

;; eating-self? : lop -> boolean
;; determines if any posn in list is colliding with self
(define (eating-self? lop)
  (ormap (λ (x) (posn=? x (first lop))) (rest lop)))

(check-expect (eating-self? dead2) #t)
(check-expect (eating-self? (list (make-posn 5 10))) #f)

;; dead? : lop -> boolean
;; determines if the player is either offscreen or eating itself then
(define (dead? lop)
  (or (offscreen? lop) (eating-self? lop)))

(check-expect (dead? dead) #t)
(check-expect (dead? dead2) #t)
(check-expect (dead? (list (make-posn 5 10))) #f)
;---------------------------------------------------------------

(define game (make-world
              "s"
              (list (make-posn 45 45))
              (make-posn 75 75)))

(big-bang game
  [to-draw draw-world]
  [on-tick tick .8]
  [on-key key]
  [stop-when (λ (w) (dead? (world-lop w)))])

