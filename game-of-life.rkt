#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "pattern.rkt")

; Images for dead and alive
(define DEAD  (bitmap "images/dead.png"))
(define ALIVE (bitmap "images/alive.png"))

; Accessors
(define (state wrld)
  (first wrld))
(define (grid wrld)
  (second wrld))
(define (alives wrld)
  (third wrld))

; Size of a cell (n pixels)
(define CELL-SIZE 20)

; Size of grid (n*n cells)
(define GRID-SIZE 30)

; Custom-made colors
(define GREEN (color 182 238 49))
(define GREY (color 179 194 212))

; Creates a binary grid of size (CELL-SIZE); 0 --> dead, 1 --> alive
; Number --> Binary Grid (List of Lists)
(define (create-grid size)
  (map (lambda(row)
         (map (lambda (x) (random 2)) row)) (make-list size (make-list size 0))))

; Count alive cells in binary grid
; Binary Grid --> Number
(define (count-alive-cells grid)
  (foldl + 0 (map (lambda (row) (foldl + 0 row)) grid)))

; Starting state
; WorldState contains: state ('continue, 'pause, 'quit), Binary Grid, number of alive cells
(define WORLD0
  (let ([grid (create-grid GRID-SIZE)])
    (list 'continue
          grid
          (count-alive-cells grid))))

; Rendering function
; WorldState --> Image
(define (render wrld)
    (above
     (overlay/align 'left 'middle (text (~v (alives wrld)) 16 'black)
                    (beside
                     (rectangle                            (alives wrld)  CELL-SIZE 'solid GREEN)
                     (rectangle (- (* GRID-SIZE CELL-SIZE) (alives wrld)) CELL-SIZE 'solid GREY)))
     (rectangle (alives wrld) 4 'solid 'transparent)
     (draw-grid (grid wrld))))

; Draw binary grid
; Binary Grid --> Image
(define (draw-grid grid)
  (let ([draw-rows (lambda (row) (apply beside (map (lambda (x) (if (= x 1) ALIVE DEAD)) row)))])
    (apply above (map draw-rows grid))))

; Get field value at position x y in binary grid
; Binary Grid, Number, Number --> Number (value at x y)
(define (read-pos grid x y)
  (list-ref (list-ref grid y) x))

; Get number of alive neighbours of field at position x y
; Binary Grid, Number, Number --> Number (number of alive neighbours)
(define (alive-neighbours grid x y)
  (let ([neighbours (list
                     (read-pos grid (modulo (- x 1) GRID-SIZE)  (modulo (- y 1) GRID-SIZE))         ; upper left
                     (read-pos grid (modulo x GRID-SIZE)        (modulo (- y 1) GRID-SIZE))         ; upper
                     (read-pos grid (modulo (+ x 1) GRID-SIZE)  (modulo (- y 1) GRID-SIZE))         ; upper right
                     (read-pos grid (modulo (- x 1) GRID-SIZE)  (modulo y       GRID-SIZE))         ; left
                     (read-pos grid (modulo (+ x 1) GRID-SIZE)  (modulo y       GRID-SIZE))         ; right
                     (read-pos grid (modulo (- x 1) GRID-SIZE)  (modulo (+ y 1) GRID-SIZE))         ; lower left
                     (read-pos grid (modulo x GRID-SIZE)        (modulo (+ y 1) GRID-SIZE))         ; lower
                     (read-pos grid (modulo (+ x 1) GRID-SIZE)  (modulo (+ y 1) GRID-SIZE)))])      ; lower right
    (foldl + 0 neighbours)))

; Decide which game rule has to be applied
; Binary Grid, Number, Number --> Procedure
(define (rule-decider grid x y)
  (let ([current (read-pos grid x y)])
    (cond
      [(equal? 1 current) (rule-alive (alive-neighbours grid x y) current)]
      [(equal? 0 current) (rule-dead (alive-neighbours grid x y) current)])))
   
; Rule for alives: If less than 3 OR more than 2 alive neighbours: become dead; else: unchanged
; Number (alive neighbours), Number (current life state) --> Number (life state)
(define (rule-alive alive-neighbours current)
  (if (or (< 3 alive-neighbours) (> 2 alive-neighbours))
      0
      current))

; Rule for deads If exactly 3 alive neighbours: become alive; else: unchanged
; Number (alive neighbours), Number (current life state) --> Number (life state)
(define (rule-dead alive-neighbours current)
  (if (= 3 alive-neighbours)
      1
      current))

; Apply game rules on clock tick
; WorldState --> new WorldState
(define (apply-conditions wrld)
  (cond
    [(equal? (state wrld) 'pause) wrld]
    [else (let ([new_grid (map (lambda (y) (map (lambda (x) (rule-decider (grid wrld) x y)) (range GRID-SIZE))) (range GRID-SIZE))]
                [number_alives (count-alive-cells (grid wrld))])
            (list (state wrld)
                  new_grid
                  number_alives))]))

; Key handler: Reset on "Return", Quit on "q", Pause on "Spacebar", Grid with cool pattern on "r"
; WorldState, Key-Event --> new WorldState
(define (handle-key wrld KEY)
  (cond
    [(key=? KEY "\r") (let ([new_grid (create-grid GRID-SIZE)])
                        (list 'continue
                              new_grid
                              (count-alive-cells new_grid)))]
    [(key=? KEY "q")  (list 'quit
                            (grid wrld)
                            (alives wrld))]
    [(key=? KEY " ")  (list (if (equal? (state wrld) 'continue)
                                'pause
                                'continue)
                            (grid wrld)
                            (alives wrld))]
    [(key=? KEY "r")  (let ([new_grid (create-pattern-grid)])
                        (list 'continue
                              new_grid
                              (count-alive-cells new_grid)))]
    [else wrld]))

; Stop game if user quits
; WorldState --> new WorldState
(define (last-world? wrld)
  (equal? (state wrld) 'quit))

; Last Image if user quits
; WorldState --> Image
(define (last-image wrld)
  (overlay
   (text/font "Thanks for playing! :)" 48 "black" "Montserrat" 'swiss 'normal 'bold #f)
   (rectangle (* CELL-SIZE GRID-SIZE) (+ 24 (* CELL-SIZE GRID-SIZE)) 'solid (color 255 255 255 120))
   (render wrld)))

; -----------------------------
; Main
(big-bang WORLD0
  [on-tick apply-conditions 0.25]
  [on-key handle-key]
  [stop-when last-world? last-image]
  [to-draw render])