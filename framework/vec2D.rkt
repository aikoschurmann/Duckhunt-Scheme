;Full functionality of the vec2D ADT
#lang r5rs

;imports
(#%require "vec2D_base.rkt")
(#%require "position_base.rkt")
(#%require "math.rkt")

;export
(#%provide vec2D::new
           vec2D::x 
           vec2D::y
           vec2D::x!
           vec2D::y!
           vec2D::?
           vec2D::equal?
           vec2D::+
           vec2D::-
           vec2D::distance
           vec2D::distance-sqr
           vec2D::magnitude
           vec2D::normalize
           vec2D::get_normalised_vec
           vec2D::rotate
           vec2D::rotate-fast
           )

;constants
(define SIGFIGS 3)

;declarations
;returns te normalized vector between two points
(define (vec2D::get_normalised_vec pos1 pos2)
    (vec2D::normalize 
        (vec2D::new (- (position::x pos2) (position::x pos1)) 
                    (- (position::y pos2) (position::y pos1)))))


(define (vec2D::rotate vec theta)
  (let ((cos-theta (cos theta)) 
        (sin-theta (sin theta))     
        (x (vec2D::x vec))
        (y (vec2D::y vec)))
    (vec2D::new (math::decimal-round (+ (* x cos-theta) 
                                        (* y sin-theta)) SIGFIGS) 
                (math::decimal-round (- (* y cos-theta) 
                                        (* x sin-theta)) SIGFIGS))))