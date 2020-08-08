#lang racket

(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)



(struct state (ticks is-forward color shutdown xpos))

(define BACKWARDS  #f)
(define FORWARD    #t)

(define BLUE   0 )
(define GREEN  1 )
(define ORANGE 2 )
(define PURPLE 3 )

(define canvas (empty-scene 640 480))
(define buffer (empty-scene 640 480))
(define blank  (rectangle 2 2 "solid" "white"))
(define mid-width  320)
(define mid-height 240)


(define bflies (list (scale 0.3 (bitmap"b1.png"))
                     (scale 0.3 (bitmap"b2.png"))
                     (scale 0.3 (bitmap"b3.png"))
                     (scale 0.3 (bitmap"b4.png"))
                     (scale 0.3 (bitmap"b5.png"))
                     (scale 0.3 (bitmap"b6.png"))
                     (scale 0.3 (bitmap"g1.png"))
                     (scale 0.3 (bitmap"g2.png"))
                     (scale 0.3 (bitmap"g3.png"))
                     (scale 0.3 (bitmap"g4.png"))
                     (scale 0.3 (bitmap"g5.png"))
                     (scale 0.3 (bitmap"g6.png"))
                     (scale 0.3 (bitmap"o1.png"))
                     (scale 0.3 (bitmap"o2.png"))
                     (scale 0.3 (bitmap"o3.png"))
                     (scale 0.3 (bitmap"o4.png"))
                     (scale 0.3 (bitmap"o5.png"))
                     (scale 0.3 (bitmap"o6.png"))
                     (scale 0.3 (bitmap"p1.png"))
                     (scale 0.3 (bitmap"p2.png"))
                     (scale 0.3 (bitmap"p3.png"))
                     (scale 0.3 (bitmap"p4.png"))
                     (scale 0.3 (bitmap"p5.png"))
                     (scale 0.3 (bitmap"p6.png"))))


(define (elem val lst)
  (not (eq? (member val lst) #f)))


(define (getAnimOffset ticks)
  (let* ([f (modulo ticks 9)])
    (cond
      [(elem f (list 0 1 2)) 0]
      [(elem f (list 3 4 5)) 1]
      [(elem f (list 6 7 8)) 2]
      [ else                 0])))


(define (change-color c)
  (if (= c PURPLE) BLUE (+ c 1) ))


(define (index-at n lst)
  (car (drop lst n)))


(define (calcX xpos is-forward)
  (cond
    [is-forward (+ xpos 2)]
    [else  (- xpos 2) ]))

(define (set-is-forward xpos is-backward)
  (cond
    [(> xpos 500) #f]
    [(< xpos 39)  #t]
    [else is-backward]))  


;
; the render function that regularly occurs to draw the
; animation, depending on the state of the program
;
(define (draw-handler ws)
  (let* ([t (state-ticks ws)]
         [f (state-is-forward ws)]
         [c (state-color ws)]
         [x (state-xpos ws)]
         [y (* (sin (/ (* (- x 40) pi) 115)) 50) ]
         [a-idx  (* c 6)]
         [f-idx  (if f 0 3)]
         [a-off (getAnimOffset t)]
         [b1 (place-image blank 70 70 buffer)]
         [b2 (place-image (index-at (+ (+ a-idx a-off) f-idx) bflies) x (+ mid-height y) b1)])
    (place-image b2 mid-width mid-height canvas)))


;
; if the door is open, increment the animation clock
; otherwise keep that at zero
;
(define (tick-handler ws)
  (let* ([t (state-ticks ws)]
         [f (state-is-forward ws)]
         [x (state-xpos ws)])
    (struct-copy state ws [ticks (+ t 1)]
                          [is-forward (set-is-forward x f)]
                          [xpos (calcX x f)])))



;
; set the door state to open and setart the animation clock
; at zero when user clicks the mouse button
;
(define (mouse-handler ws x y evt)
  (if (eq? evt "button-up")
    (state (state-ticks ws)
           (state-is-forward ws)
           (change-color (state-color ws))
           (state-shutdown   ws)
           (state-xpos   ws))
    ws))



;
(define (end-check ws)
  (= (state-shutdown ws) 1) )


(define (main y)
  (big-bang (state 0 FORWARD BLUE 0 40 )
                              [to-draw    draw-handler ]
                              [on-mouse   mouse-handler]
                              [on-tick    tick-handler ]
                              [stop-when  end-check    ]))



#|
roll my own elem

> (not (eq? (member 8 (list 1 2 3 4 5)) #f))
#f
> (not (eq? (member 1 (list 1 2 3 4 5)) #f))
#t


|#
