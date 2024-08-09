;;; Scaling Factors
(define pi 3.14)
(define scale 2)
(define ui-scale scale)

;;; Bird Dimensions and Speed
(define unscaled-bird-width 32)
(define unscaled-bird-height 32) ; technically has a height of 33 px
(define bird-width (* unscaled-bird-width scale))
(define bird-height (* unscaled-bird-height scale))
(define bird-speed (* 20 scale))
(define slowed-factor 0.6)
(define slowed-duration (* 5 1000))

;;; Animation Timings
(define bird-frame-time 100) ; time between animations

;;; Screen Dimensions
(define screen-width (* bird-width 14))
(define screen-height (* bird-width 8))

;;; Game Mechanics
(define fall-speed-factor 1.5)
(define max-bounces 5)
(define border-radius (* 150 scale))
(define parabolic-const (- 0.0015))
(define crow-target-radius 20)
(define spawn-rate (* 2 1000))

;;; Focus and Double
(define focus! set-car!)
(define double! set-cdr!)
(define focus car)
(define double cdr)
(define double-duration (* 1000 20))
(define focus-duration (* 1000 25))

;;; Burst Fire Parameters
(define burst-cooldown (* 1000 5))
(define burst-delay 250)
(define burst-shots 5)

;;; Game Objects
(define tree (make-bitmap-tile "sprites/tree/tree1.png" "sprites/tree/tree1mask.png"))
(define tree2 (make-bitmap-tile "sprites/tree/tree1.png" "sprites/tree/tree1mask.png"))
(define stone (make-bitmap-tile "sprites/stone/stone1.png" "sprites/stone/stone1mask.png"))
(define stone2 (make-bitmap-tile "sprites/stone/stone1.png" "sprites/stone/stone1mask.png"))
(define crosshair (make-bitmap-tile "sprites/crosshair/crosshair1.png" "sprites/crosshair/crosshair1mask.png"))
(define double-indicator (make-bitmap-tile "sprites/double/double1.png"))
(define focus-indicator (make-bitmap-tile "sprites/focus/focus1.png"))
(define air-gun-tile (make-bitmap-tile "sprites/guns/gun1.png" "sprites/guns/gun1mask.png"))
(define turbo-gun-tile (make-bitmap-tile "sprites/guns/gun2.png" "sprites/guns/gun2mask.png"))
(define water-gun-tile (make-bitmap-tile "sprites/guns/gun3.png" "sprites/guns/gun3mask.png"))

;;; Object Data
(define stone1_pos 400)
(define stone2_pos 700)

(define level2_pos (cons 100 400))
(define level3_pos (cons 400 700))


(define (stone_data) (vector stone stone1_pos (- screen-height (stone 'get-h) 75) 'stone))
(define (stone_data2) (vector stone2 stone2_pos (- screen-height (stone2 'get-h) 75) 'stone))

;;; Levels
(define (level1) (list (vector tree 100 (- screen-height (tree 'get-h) 150) 'tree)
                     (stone_data)
                     (stone_data2)))

(define (stone-vector) (vector (stone_data) (stone_data2)))

(define (level2) (list (vector tree 700 (- screen-height (tree 'get-h) 150) 'tree)
                     (stone_data)
                     (stone_data2)))

(define (level3) (list (vector tree 200 (- screen-height (tree 'get-h) 150) 'tree)
                     (stone_data)
                     (stone_data2)))

(define probability-table-easy (vector 'duck 'crow 'enemy))
(define probability-table-medium (vector 'duck 'seagull 'crow 'enemy))
(define probability-table-hard (vector 'duck 'seagull 'buzzard))

;;; UI Setup
((double-indicator 'set-x!) (- screen-width 50))
((double-indicator 'set-y!) 20)
((focus-indicator 'set-x!) (- screen-width 50))
((focus-indicator 'set-y!) 70)
((double-indicator 'set-scale!) scale)
((focus-indicator 'set-scale!) scale)
((tree 'set-scale!) ui-scale)
((tree2 'set-scale!) ui-scale)
((stone 'set-scale!) (* ui-scale 2))
((stone2 'set-scale!) (* ui-scale 2))

((air-gun-tile 'set-x!) 20)
((air-gun-tile 'set-y!) 80)
((turbo-gun-tile 'set-x!) 20)
((turbo-gun-tile 'set-y!) 80)
((water-gun-tile 'set-x!) 20)
((water-gun-tile 'set-y!) 80)

((air-gun-tile 'set-scale!) (* ui-scale 2))
((turbo-gun-tile 'set-scale!) (* ui-scale 2))
((water-gun-tile 'set-scale!) (* ui-scale 2))