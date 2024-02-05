#lang r5rs

; Export
(#%provide position::new 
           position::x 
           position::y
           position::x!
           position::y!
           position::?
           position::equal?
           position::distance)

; Constants
(define position-tag 'position)
(define tag-index 0)
(define x-index 1)
(define y-index 2)

; Declarations
(define (position::new x y)
  (vector position-tag x y))

(define (position::x pos)
  (vector-ref pos x-index))

(define (position::y pos)
  (vector-ref pos y-index))

(define (position::x! pos x)
  (vector-set! pos x-index x) pos)

(define (position::y! pos y)
  (vector-set! pos y-index y) pos)

(define (position::? pos)
  (and (vector? pos)
       (eq? (vector-ref pos tag-index) position-tag)))

(define (position::equal? pos1 pos2)
  (and (= (position::x pos1) (position::x pos2)) 
       (= (position::y pos1) (position::y pos2))))

(define (position::distance pos1 pos2)
  (sqrt (+ (expt (- (position::x pos2) (position::x pos1)) 2)
           (expt (- (position::y pos2) (position::y pos1)) 2))))
