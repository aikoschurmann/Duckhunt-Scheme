#lang r5rs
(#%require "framework/vec2D.rkt")
(#%require "framework/position.rkt")
(#%require "framework/bird.rkt")

(define bird (bird::new 'pigeon 1 (position::new 0 0) (vec2D::new 1 1)))
(define test-position (position::new 1 2))
(define test-vector (vec2D::new 3 4))
