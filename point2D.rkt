;Code duplication between point2D and vec2D is staggering.
;In this OOP approach it's unclear to me on how to mitigate this.

;Functional wrappers for dispatch calls.
;Dispatcher uses (message . args) to simplify this further.
; - Prefix notation is achieved.
; - Simplifies 'OOP use'
; - Argument handling is done by procedure definitions.


; V1  ((vec1 'vec2D::equal?) vec2)
;     gets mapped to
; V2  (vec2D::equal? vec1 vec2)
;     however dispatcher calls are still available (simplified due to (message . args))
; V3  (vec1 'vec2D::equal? vec2)

;     codebase will use V2 OUTSIDE of the implementations.
;     V1 isn't valid syntax for this implementation due to (message . args)

(define (point2D::x point1)
  (point1 'point2D::x))
(define (point2D::y point1)
  (point1 'point2D::y))
(define (point2D::x! point1 new_x)
  (point1 'point2D::x! new_x))
(define (point2D::y! point1 new_y)
  (point1 'point2D::y! new_y))
(define (point2D::equal? point1 point2)
  (point1 'point2D::equal? point2))
(define (point2D::+! point1 vec)
  (point1 'point2D::+! vec))
(define (point2D::-! point1 vec)
  (point1 'point2D::-! vec))

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
          ((eq? message 'point2D::copy) (point2D::copy))

          (else (error "point2D ADT unkown message" message))))
  dispatch-point2D)

