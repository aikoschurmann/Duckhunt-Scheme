(define (vec2D::get_normalised_vec point1 point2)
  ;Returns the normalized vector between two points.
  (let ((vec (vec2D::new (- (point2 'point2D::x) (point1 'point2D::x)) 
                         (- (point2 'point2D::y) (point1 'point2D::y)))))
    (vec 'vec2D::normalize!)
    vec))

(define (vec2D::random-unit-vector)
  (let ((angle (* 2 pi (random)))) 
    (let ((x (cos angle)) 
          (y (sin angle))) 
      (vec2D::new x y))))

(define (vec2D::random-unit-vector-up)
  (let* ((min-angle (- (/ pi 2) 0.4))  
         (max-angle (+ (/ pi 2) 0.4))  
         (angle (+ min-angle (* (random) (- max-angle min-angle)))))  
    (let ((x (cos angle))
          (y (- (sin angle))))
      (vec2D::new x y))))


(define (vec2D::random-unit-vector-down)
  (let* ((min-angle (- (/ pi 2) 0.4)) 
         (max-angle (+ (/ pi 2) 0.4 ))  
         (angle (+ min-angle (* (random) (- max-angle min-angle))))) 
    (let ((x (cos angle))
          (y (sin angle)))
      (vec2D::new x y))))



(define (vec2D::new x y)
  (define (vec2D::x! new-x)
    (set! x new-x))
  
  (define (vec2D::y! new-y)
    (set! y new-y))

  (define (vec2D::flip-x!)
    (set! x (- x)))

  (define (vec2D::flip-y!)
    (set! y (- y)))
  
  (define (vec2D::equal? other-vec)
    (and (= x (other-vec 'vec2D::x))
         (= y (other-vec 'vec2D::y)))
         )
  
  (define (vec2D::+! other-vec)
    (set! x (+ x (other-vec 'vec2D::x)))
    (set! y (+ y (other-vec 'vec2D::y))))
  
  (define (vec2D::-! other-vec)
    (set! x (- x (other-vec 'vec2D::x)))
    (set! y (- y (other-vec 'vec2D::y))))
  
  (define (vec2D::*! const)
    (set! x (* x const))
    (set! y (* y const)))
  
  (define (vec2D::magnitude)
    (sqrt (+ (* x x) (* y y))))
  
  (define (vec2D::distance-sqr other-vec)
    (+ (expt (- x (other-vec 'vec2D::x)) 2)
       (expt (- y (other-vec 'vec2D::y)) 2)))

  (define (vec2D::distance other-vec)
    (sqrt (vec2D::distance-sqr other-vec)))

  (define (vec2D::normalize!)
    (let ((length (vec2D::magnitude)))
      (if (not (zero? length))
          (begin
            (set! x (/ x length))
            (set! y (/ y length))))))

  (define (vec2D::rotate! theta)
    (let* ((cos-theta (cos theta))
           (sin-theta (sin theta))
           (new-x (+ (* x cos-theta) (* y sin-theta)))
           (new-y (- (* y cos-theta) (* x sin-theta))))
      (set! x new-x)
      (set! y new-y)))

  (define (vec2D::copy)
    (vec2D::new x y))

  ;; Dispatch function using case
  (define (dispatch-vector message . args)
    (apply (case message
             ((vec2D::x) (lambda () x))
             ((vec2D::y) (lambda () y))
             ((vec2D::x!) vec2D::x!)
             ((vec2D::y!) vec2D::y!)
             ((vec2D::flip-x!) vec2D::flip-x!)
             ((vec2D::flip-y!) vec2D::flip-y!)
             ((vec2D::equal?) vec2D::equal?)
             ((vec2D::+!) vec2D::+!)
             ((vec2D::-!) vec2D::-!)
             ((vec2D::*!) vec2D::*!)
             ((vec2D::magnitude) (lambda () (vec2D::magnitude)))
             ((vec2D::distance-sqr) vec2D::distance-sqr)
             ((vec2D::distance) vec2D::distance)
             ((vec2D::normalize!) vec2D::normalize!)
             ((vec2D::rotate!) vec2D::rotate!) ; not used
             ((vec2D::copy) (lambda () (vec2D::copy)))
             (else (lambda () (error "Vec2D ADT unknown message: " message)))) args))
  
  dispatch-vector)


