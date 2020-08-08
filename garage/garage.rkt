#lang racket

(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)



(struct state (ticks door car-num shutdown))

(define DOOR-CLOSED  0)
(define DOOR-OPENED  1)

(define canvas (empty-scene 640 480))
(define buffer (empty-scene 640 480))
(define blank  (rectangle 2 2 "solid" "white"))
(define mid-width  320)
(define mid-height 240)


(define cars (list (bitmap "car1.png") (bitmap "car2.png")
                   (bitmap "car3.png") (bitmap "car4.png")
                   (bitmap "car5.png")))


(define garage-door (list(bitmap "door1.png") (bitmap "door2.png")
                         (bitmap "door3.png") (bitmap "door4.png") ))


;(define (getGIdx tic)
;  (if (> tic 9) 3 (quotient tic 3)))

; try the hash table way
(define gidxTable (make-hash))
(hash-set*! gidxTable 0 0 1 0 2 0 3 1 4 1 5 1 6 2 7 2 8 2
                 9 3 10 3 11 3 12 3 13 3 14 3 15 3 16 3
                 17 3 18 3 19 3 20 3 21 3 22 3 23 3 24 3
                 25 2 26 2 27 2 28 1 29 1 30 1)

(define (getGIdx tic)
  (if (hash-has-key? gidxTable tic)
      (hash-ref gidxTable tic)
      0))




(define (index-at n lst)
  (car (drop lst n)))

(define (getCarPos tic)
  (if (> tic 9)
      (+ (* tic 5) 10)
      10))

(define (getNextCar index)
  (if (= index 4)
      0
      (+ index 1)))


;
; the render function that regularly occurs to draw the
; animation, depending on the state of the program
;
(define (draw-handler ws)
  (let* ([gIdx (getGIdx (state-ticks ws))]
         [carpos (getCarPos (state-ticks ws))]
         [caridx (state-car-num ws)]
         [b1 (place-image blank 70 70 buffer)]
         [b2 (place-image (index-at gIdx garage-door) 40 70 b1)]
         [b3 (place-image (index-at caridx cars) carpos 90 b2)])
    (place-image b3 mid-width mid-height canvas)))


;
; if the door is open, increment the animation clock
; otherwise keep that at zero
;
(define (tick-handler ws)
    (let* ([door (state-door ws)]
           [b (+ 1 1)           ]
           [c (+ 1 1)           ])
      (if (= door DOOR-OPENED)
          (state (+ 1 (state-ticks ws))
                 (state-door       ws)
                 (state-car-num    ws)
                 (state-shutdown   ws))
          (state 0
                 (state-door       ws)
                 (state-car-num    ws)
                 (state-shutdown   ws)))))


;
; set the door state to open and setart the animation clock
; at zero when user clicks the mouse button
;
(define (mouse-handler ws x y evt)
  (if (eq? evt "button-up")
    (state 0
           DOOR-OPENED
           (getNextCar (state-car-num ws))
           (state-shutdown   ws)  )
    ws))


;
; check for shutdown state. big bang will regularly use this
; to see if the program should be terminated
;
(define (end-check ws)
  (= (state-shutdown ws) 1) )


(define (main y)
  (big-bang (state 0 DOOR-CLOSED 0 0)
                              [to-draw draw-handler]
                              [on-mouse mouse-handler]
                              [on-tick tick-handler]
                              [stop-when end-check]))



#|

> (define rr (state 1 1 1 1 1))
> (define ss (struct-copy state rr))
> (define ss (struct-copy state rr [ticks-door 10]))
> (state-ticks-door rr)
1
> (state-ticks-door ss)
10



(define u (bitmap "car5.png"))



; ---- tests ----
(check-expect (clock-tick-handler 400) 403)
(check-expect (end? 400) #t)

|#
