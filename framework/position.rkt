;Full functionality of the Position ADT
#lang r5rs

;imports
(#%require "vec2D_base.rkt")
(#%require "position_base.rkt")

;export
(#%provide position::new 
         position::x 
         position::y 
         position::x! 
         position::y!
         position::?
         position::equal?
         position::+
         position::-
         position::distance
         )
         
;declarations
(define (position::+ pos vec)
    (position::new (+ (position::x pos) (vec2D::x vec)) 
                   (+ (position::y pos) (vec2D::y vec))))

(define (position::- pos vec)
    (position::new (- (position::x pos) (vec2D::x vec)) 
                   (- (position::y pos) (vec2D::y vec))))

