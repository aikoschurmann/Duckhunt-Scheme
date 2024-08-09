(define (generate-position-middle)
      (let* ((fractions 30)
             (fraction-x (/ (random 0 fractions) fractions))
             (fraction-y (/ (random 0 fractions) fractions))
             (padding-height (/ screen-height fractions))
             (padding-width (/ screen-height fractions))
             (y (+ padding-height (* fraction-y (- screen-height (* 2 padding-height)))))
             (x (+ padding-width (* fraction-x (- screen-width (* 2 padding-width))))))
        (point2D::new x y)))

(define (generate-position-bottom)
  (let* ((fraction (/ (random 0 11) 10))
         (padding (/ screen-width 10))
         (x (+ padding (* fraction (- screen-width (* 2 padding)))))
         (y (+ screen-height (* 25 scale))))
    (point2D::new x y)))

(define (generate-position-top)
  (let* ((fraction (/ (random 0 11) 10))
         (padding (/ screen-width 10))
         (x (+ padding (* fraction (- screen-width (* 2 padding)))))
         (y (- (* 25 scale))))
    (point2D::new x y)))

(define (generate-position-side)
  (let* ((fraction (/ (random 0 11) 10))
         (padding (/ screen-height 15))
         (y (+ padding (* fraction (- screen-height (* 2 padding)))))
         (x -25))
    (point2D::new x y)))


(define (point2D::new x y)
  ;; Setters for x and y coordinates
  (define (point2D::x! new-x)
    (set! x new-x))

  (define (point2D::y! new-y)
    (set! y new-y))

  (define (point2D::equal? other-point)
    (and (= x (other-point 'point2D::x))
         (= y (other-point 'point2D::y))))

  (define (point2D::+! vec)
    (set! x (+ x (vec 'vec2D::x)))
    (set! y (+ y (vec 'vec2D::y))))
  
  (define (point2D::-! vec)
    (set! x (- x (vec 'vec2D::x)))
    (set! y (- y (vec 'vec2D::y))))

  (define (point2D::distance-sqr point)
    (let* ((delta-x (- x (point 'point2D::x)))
           (delta-y (- y (point 'point2D::y))))
      (+ (* delta-x delta-x) (* delta-y delta-y))))

  (define (point2D::distance point)
    (sqrt (point2D::distance-sqr point)))
      
  (define (point2D::copy)
    ;; Creates a copy of the given point
    (point2D::new x y))

  ;; Dispatch function using case
  (define (dispatch-point2D message . args)
    (apply (case message
             ((point2D::x) (lambda () x))
             ((point2D::y) (lambda () y))
             ((point2D::x!) point2D::x!)
             ((point2D::y!) point2D::y!)
             ((point2D::equal?) point2D::equal?)
             ((point2D::+!) point2D::+!)
             ((point2D::-!) point2D::-!)
             ((point2D::distance-sqr) point2D::distance-sqr)
             ((point2D::distance) point2D::distance)
             ((point2D::copy) (lambda () (point2D::copy)))
             (else (lambda () (error "point2D ADT unknown message" message)))) args))
  
  dispatch-point2D)

