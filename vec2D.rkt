(#%require (only racket random error))
;Functional wrappers for dispatch calls.
;Dispatcher uses (message . args) to simplify this.
; - Simplifies 'OOP use'
; - Argument handling is done by procedure definitions.

; V1  ((vec1 'vec2D::equal?) vec2)
;     gets mapped to due to (message . args)
; V2  (vec1 'vec2D::equal? vec2)

;     V1 isn't valid syntax for this implementation due to (message . args)


;Not in the dispatcher because this procedure doesn't need a predifined vector.
(define (vec2D::get_normalised_vec point1 point2)
  ;Returns the normalized vector between two points.
  (let ((vec (vec2D::new (- (point2 'point2D::x) (point1 'point2D::x)) 
                         (- (point2 'point2D::y) (point1 'point2D::y)))))
    (vec 'vec2D::normalize!)
    vec))

(define (vec2D::new x y)
  ;Constructor for creating a new Vec2D.
  ;Usage: (define my-vector (vec2D::new 1 2))
  (define (vec2D::x! args)
    ;Setter for the x-coordinate of the vector.
    (let ((new-x (car args)))
      (set! x new-x)))
  
  (define (vec2D::y! args)
    ;Setter for the y-coordinate of the vector.
    (let ((new-y (car args)))
      (set! y new-y)))

  (define (vec2D::flip-x!)
    (set! x (- x)))

  (define (vec2D::flip-y!)
    (set! y (- y)))
  
  (define (vec2D::equal? args)
    ;Check if two vectors are equal.
    (let ((other-vec (car args)))
      (and (= x (other-vec 'vec2D::x))
           (= y (other-vec 'vec2D::y)))))
  
  (define (vec2D::+! args)
    ;Add the components of another vector to this vector.
    (let ((other-vec (car args)))
      (set! x (+ x (other-vec 'vec2D::x)))
      (set! y (+ y (other-vec 'vec2D::y)))))
  
  (define (vec2D::-! args)
    ;Subtract the components of another vector from this vector.
    (let ((other-vec (car args)))
      (set! x (- x (other-vec 'vec2D::x)))
      (set! y (- y (other-vec 'vec2D::y)))))
  
  (define (vec2D::*! args)
    ;multiply the components of a vector by a constant.
    (let ((const (car args)))
      (set! x (* x const))
      (set! y (* y const))))
  
  (define (vec2D::magnitude)
    ;Calculate the magnitude (length) of the vector.
    (sqrt (+ (* x x) (* y y))))

  (define (vec2D::distance-sqr args)
    ;Calculate the squared distance between two vectors.
    (let ((other-vec (car args)))
      (+ (expt (- x (other-vec 'vec2D::x)) 2)
         (expt (- y (other-vec 'vec2D::y)) 2))))

  (define (vec2D::distance args)
    ;Calculate the distance between two vectors.
    (sqrt (vec2D::distance-sqr args)))

  (define (vec2D::normalize!)
    ;Normalize (scale to unit length) the vector.
    (let ((length (vec2D::magnitude)))
      (if (not (zero? length))
          (begin (set! x (/ x length))
                 (set! y (/ y length))))))

  (define (vec2D::rotate! args)
    ;Rotate the vector by a specified angle (in radians).
    (let* ((theta (car args))
           (cos-theta (cos theta))
           (sin-theta (sin theta)))
      (begin (set! x (+ (* x cos-theta) (* y sin-theta)))
             (set! y (- (* y cos-theta) (* x sin-theta))))))

  (define (vec2D::copy)
    ;Creates a copy of the given vector
     (vec2D::new x y))

  (define (dispatch-vector message . args)
    ;Message dispatcher for vector operations.
    (cond ((eq? message 'vec2D::x) x)
          ((eq? message 'vec2D::y) y)
          ((eq? message 'vec2D::x!) (vec2D::x! args))
          ((eq? message 'vec2D::y!) (vec2D::y! args))
          ((eq? message 'vec2D::flip-x!) (vec2D::flip-x!))
          ((eq? message 'vec2D::flip-y!) (vec2D::flip-y!))
          ((eq? message 'vec2D::equal?) (vec2D::equal? args))
          ((eq? message 'vec2D::+!) (vec2D::+! args))
          ((eq? message 'vec2D::-!) (vec2D::-! args))
          ((eq? message 'vec2D::*!) (vec2D::*! args))
          ((eq? message 'vec2D::magnitude) (vec2D::magnitude))
          ((eq? message 'vec2D::distance-sqr) (vec2D::distance-sqr args))
          ((eq? message 'vec2D::distance) (vec2D::distance args))
          ((eq? message 'vec2D::normalize!) (vec2D::normalize!))
          ((eq? message 'vec2D::rotate!) (vec2D::rotate! args))
          ((eq? message 'vec2D::copy) (vec2D::copy))
          (else (error "Vec2D ADT unknown message: " message))))
  dispatch-vector)