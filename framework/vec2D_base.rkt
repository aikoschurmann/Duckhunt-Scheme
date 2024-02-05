#lang r5rs

; Constants
(define vec2D-tag 'vec2D)
(define tag-index 0)
(define x-index 1)
(define y-index 2)

; Exported functions
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
           vec2D::rotate-fast)

; Declarations
(define (vec2D::new x y)
  (vector vec2D-tag x y))

(define (vec2D::x vec)
  (vector-ref vec x-index))

(define (vec2D::y vec)
  (vector-ref vec y-index))

(define (vec2D::x! vec x)
  (vector-set! vec x-index x) vec)

(define (vec2D::y! vec y)
  (vector-set! vec y-index y) vec)

(define (vec2D::? vec)
  (and (vector? vec)
       (eq? (vector-ref vec tag-index) vec2D-tag)))

(define (vec2D::equal? vec1 vec2)
  (and (= (vec2D::x vec1) (vec2D::x vec2)) 
       (= (vec2D::y vec1) (vec2D::y vec2))))

(define (vec2D::+ vec1 vec2)
  (vec2D::new (+ (vec2D::x vec1) (vec2D::x vec2)) 
              (+ (vec2D::y vec1) (vec2D::y vec2))))

(define (vec2D::- vec1 vec2)
  (vec2D::new (- (vec2D::x vec1) (vec2D::x vec2)) 
              (- (vec2D::y vec1) (vec2D::y vec2))))

; Faster version of distance calculation
(define (vec2D::distance-sqr vec1 vec2)
  (+ (expt (- (vec2D::x vec2) (vec2D::x vec1)) 2)
     (expt (- (vec2D::y vec2) (vec2D::y vec1)) 2)))

; Slower version of distance calculation
(define (vec2D::distance vec1 vec2)
  (sqrt (vec2D::distance-sqr vec1 vec2)))

; Calculates the length of the vector + alias
(define (vec2D::magnitude vec)
  (sqrt (+ (* (vec2D::x vec) (vec2D::x vec))
           (* (vec2D::y vec) (vec2D::y vec)))))

(define vec2D::length vec2D::magnitude)

; Returns vector with the same direction but magnitude of 1
(define (vec2D::normalize vec)
  (let ((length (vec2D::magnitude vec)))
    (if (zero? length)
        vec   
        (vec2D::new (/ (vec2D::x vec) length)
                    (/ (vec2D::y vec) length)))))

; Rotates the vector by an angle theta (radians)
(define (vec2D::rotate-fast vec theta)
  (let ((cos-theta (cos theta))
        (sin-theta (sin theta))
        (x (vec2D::x vec))
        (y (vec2D::y vec)))
    (vec2D::new (+ (* x cos-theta) (* y sin-theta))
                (- (* y cos-theta) (* x sin-theta)))))
