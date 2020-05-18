;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname corona) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; a State is one of:
;; - "sick"
;; - "healthy"
;; - "recovered"

;; a Person is a (make-person State Posn Posn Number)
(define-struct person [state place dir age])

(define p1 (make-person "sick" (make-posn 10 40) (make-posn 2 3) 0))
(define p2 (make-person "healthy" (make-posn 40 30) (make-posn -1 2) 0))
(define p3 (make-person "recovered" (make-posn 60 30) (make-posn 4 2) 15))
(define p4 (make-person "healthy" (make-posn 62 32) (make-posn 4 2) 15))
(define p5 (make-person "sick" (make-posn 62 400) (make-posn 4 2) 15))

;; a Town is a [Listof Person]
(define wid 600)
(define hei 300)
(define bg (empty-scene wid hei))
(define p-img (circle 6 'solid 'blue))
(define p-infect (circle 6 'solid 'brown))
(define p-recover (circle 6 'solid 'pink))

;; draw-person : Person -> Image
;; represents a person as a circle
(define (draw-person p)
  (local[ ;; to return diff color based off diff state:)
         (define color-changer
           (cond
             [(string=? (person-state p) "healthy") p-img]
             [(string=? (person-state p) "sick") p-infect]
             [else p-recover]))]
    (place-image color-changer (posn-x (person-place p)) (posn-y (person-place p)) bg)))

(check-expect (draw-person p1) (place-image p-infect 10 40 bg))
(check-expect (draw-person p2) (place-image p-img 40 30 bg))
(check-expect (draw-person p3) (place-image p-recover 60 30 bg))

;; note : infect will have to be nested within something bc no void returns

; dist : Posn Posn -> Number
; computes the distance between two Posns
(define (dist p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

;; person-collide? : Person Person -> Boolean
;; determines if a person collides
;; TODO test the number 6
(define (person-collide? curr other)
  (<= (dist (person-place curr) (person-place other)) 6))

(check-expect (person-collide? p1 p2) #f)
(check-expect (person-collide? p2 p3) #f)
(check-expect (person-collide? p3 p4) #t)

;; edge-collide? : Person -> Boolean
;; determines if a person is at the edge of the screen
(define (edge-collide p)
  (local[
         (define x (posn-x (person-place p)))
         (define y (posn-y (person-place p)))]
    (or (<= x 0)
        (>= x wid)
        (<= y 0)
        (>= y hei))))

(check-expect (edge-collide p1) #f)
(check-expect (edge-collide p2) #f)
(check-expect (edge-collide p5) #t)

;; bounce-back : Person -> Person
;; updates a person's direction to opposite of current
(define (bounce-back p)
  (make-person (person-state p)
               (person-place p)
               (make-posn (+ (add1 (random 3)) (* -1 (posn-x (person-place p))))
                          (+ (add1 (random 3)) (* -1 (posn-x (person-place p)))))
               (person-age p)))

;; move-person : Person -> Person
;; moves person by direction of dir
(define (move-person p)
  (make-person (person-state p)
               (make-posn (+ (posn-x (person-place p)) (posn-x (person-dir p)))
                          (+ (posn-y (person-place p)) (posn-y (person-dir p))))
               (person-dir p)
               (person-age p)))

;; person-recover : Person -> Person
(define (person-recover p)
  (if (and (string=? (person-state p) "sick") (<= 15 (person-age p)))
      (make-person "recovered"
                   (person-place p)
                   (person-dir p)
                   0)
      p))

(check-expect (person-recover p5) (make-person "recovered" (make-posn 62 400) (make-posn 4 2) 0))
(check-expect (person-recover p4) p4)
(check-expect (person-recover p1) p1)

;; draw-lop : [Listof Person] -> Image
#;(foldr (λ (p img) (place-)))







