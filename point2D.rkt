;Functional wrappers for dispatch calls.
;Dispatcher uses (message . args) to simplify this.
; - Simplifies 'OOP use'
; - Argument handling is done by procedure definitions.

; V1  ((vec1 'vec2D::equal?) vec2)
;     gets mapped to due to (message . args)
; V2  (vec1 'vec2D::equal? vec2)

;     V1 isn't valid syntax for this implementation due to (message . args)


(define (point2D::new x y)
  (define (point2D::x args)
    (let ((new-x (car args)))
      (set! x new-x)))

  (define (point2D::y args)
    (let ((new-y (car args)))
      (set! y new-y)))

  (define (point2D::equal? args)
    (let ((other-point (car args)))
      (and (= x (other-point 'point2D::x))
           (= y (other-point 'point2D::y)))))

  (define (point2D::+! args)
    (let ((vec (car args)))
      (set! x (+ x (vec 'vec2D::x)))
      (set! y (+ y (vec 'vec2D::y)))))
  
  (define (point2D::-! args)
    (let ((vec (car args)))
      (set! x (- x (vec 'vec2D::x)))
      (set! y (- y (vec 'vec2D::y)))))

  (define (point2D::distance-sqr args)
    (let* ((point (car args))
          (delta-x (- x (point 'point2D::x)))
          (delta-y (- y (point 'point2D::y))))
      (+ (* delta-x delta-x) (* delta-y delta-y))))

  (define (point2D::distance args)
    (sqrt (point2D::distance-sqr args)))
      
  (define (point2D::copy)
    ;Creates a copy of the given point
     (point2D::new x y))

  (define (dispatch-point2D message . args)
    (cond ((eq? message 'point2D::x) x)
          ((eq? message 'point2D::y) y)
          ((eq? message 'point2D::x!) (point2D::x args))
          ((eq? message 'point2D::y!) (point2D::y args))
          ((eq? message 'point2D::equal?) (point2D::equal? args))
          ((eq? message 'point2D::+!) (point2D::+! args))
          ((eq? message 'point2D::-!) (point2D::-! args))
          ((eq? message 'point2D::distance-sqr) (point2D::distance-sqr args))
          ((eq? message 'point2D::distance) (point2D::distance args))
          ((eq? message 'point2D::copy) (point2D::copy))

          (else (error "point2D ADT unkown message" message))))
  dispatch-point2D)

