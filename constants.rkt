(define pi 3.14159265358979)

(define scale 2)
(define unscaled-bird-width 32)
;technically has a height of 33 px
(define unscaled-bird-height 32)
(define bird-width (* unscaled-bird-width scale))
(define bird-height (* unscaled-bird-height scale))
;time between animations
(define bird-frame-time 100)
(define screen-width (* bird-width 14))
(define screen-height (* bird-width 8))