(define pi 3.14)
(define scale 2)
(define ui-scale 2)
(define unscaled-bird-width 32)
;technically has a height of 33 px
(define unscaled-bird-height 32)
(define bird-width (* unscaled-bird-width scale))
(define bird-height (* unscaled-bird-height scale))
(define bird-speed (* 20 scale))
;time between animations
(define bird-frame-time 100)
(define screen-width (* bird-width 14))
(define screen-height (* bird-width 8))
(define fall-speed  (* 30 scale))