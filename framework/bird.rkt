#lang r5rs

;Constants 
(define bird-tag 'bird)
(define tag-index 0)
(define type-index 1)
(define health-index 2)
(define pos-index 3)
(define vec-index 4)

;export
(#%provide bird::new
           bird::type
           bird::health
           bird::pos
           bird::position
           bird::vec
           bird::vector
           bird::health!
           bird::pos!
           bird::position!
           bird::vec!
           bird::vector!
           bird::?)

;declarations
(define (bird::new type health position vec) 
    (vector bird-tag type health position vec))

(define (bird::type bird)
    (vector-ref bird type-index))

(define (bird::health bird)
    (vector-ref bird health-index))

(define (bird::pos bird)
    (vector-ref bird pos-index))
(define bird::position bird::pos)

(define (bird::vec bird)
    (vector-ref bird vec-index))
(define bird::vector bird::vec)

(define (bird::health! bird health)
    (vector-set! bird health-index health) bird)

(define (bird::pos! bird position)
    (vector-set! bird pos-index position) bird)
(define bird::position! bird::pos!)

(define (bird::vec! bird vec)
    (vector-set! bird vec-index vec) bird)
(define bird::vector! bird::vec!)

(define (bird::? bird)
  (and (vector? bird)
       (eq? (vector-ref bird tag-index) bird-tag)))