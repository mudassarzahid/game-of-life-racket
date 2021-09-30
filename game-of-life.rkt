#lang racket
(require 2htdp/image)
(require 2htdp/universe)

; Images for dead and alive
(define DEAD (bitmap "images/dead.png"))
(define ALIVE (bitmap "images/alive.png"))

; Size of board
(define SIZE 30)

; Creates a boolean-grid of (SIZE) 
(define (create-grid SIZE)
  (map (lambda(row)
         (map (lambda(x) (if (= 1 (random 2)) #t #f)) row)) (make-list SIZE (make-list SIZE 0))))

; WorldState
(define WORLD0 (create-grid SIZE))

; row of booleans --> row of rectangles
(define (row->img r)
  (map (lambda (x) (if x ALIVE DEAD)) r))

; row of rectangles --> img of rectangle-row
(define (draw-row r)
  (apply beside (row->img r)))

; grid of booleans --> image of grid
(define (draw-grid g)
  (apply above (map draw-row g)))

; get neighbours of field at position x y
(define (get-neighbours g x y)
  (if (and (<= 0 x (- SIZE 1)) (<= 0 y (- SIZE 1)))  
      (read-pos g x y)
      #f))

; get field value at position x y
(define (read-pos g x y)
  (list-ref (list-ref g y) x))

; return number of alive neighbours of field at position x y
(define (8-neighbourhood g x y)
  (let ([neighbours (list
                     (get-neighbours g (- x 1)  (- y 1)  )         ; upper left
                     (get-neighbours g x        (- y 1)  )         ; upper
                     (get-neighbours g (+ x 1)  (- y 1)  )         ; upper right
                     (get-neighbours g (- x 1)  y        )         ; left
                   ; (get-neighbours g x        y        )         ; self
                     (get-neighbours g (+ x 1)  y        )         ; right
                     (get-neighbours g (- x 1)  (+ y 1)  )         ; lower left
                     (get-neighbours g x        (+ y 1)  )         ; lower
                     (get-neighbours g (+ x 1)  (+ y 1)  )         ; lower right
                     )])
    (boolean->num neighbours)))
  
; list of booleans --> list of numbers           
(define (boolean->num liste)
  (apply + (map (lambda (x) (if x 1 0)) liste)))

; rule decider
(define (rule-decider g x y)
  (if
   (eq? #t (read-pos g x y))
   (rule-alive g x y) (rule-dead g x y)))
   
; rule for alives
(define (rule-alive g x y)
  (if (or (< 3 (8-neighbourhood g x y)) (> 2 (8-neighbourhood g x y)))
      #f
      (read-pos g x y)))

; rule for deads
(define (rule-dead g x y)
  (if (= 3 (8-neighbourhood g x y))
      #t
      (read-pos g x y)))

; state changer
(define (conditions g)
  (map (lambda (y)
         (map (lambda (x)
                (rule-decider g x y)) (range SIZE))) (range SIZE)))

; reset game
(define (reset GRID KEY)
  (create-grid SIZE))

; display game
(big-bang WORLD0
  [on-tick conditions 0.25]
  [on-key reset]
  [to-draw draw-grid])