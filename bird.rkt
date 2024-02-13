;wraper functions
(define (bird::decrease-health! bird dhealth)
  (bird 'bird::decrease-health! dhealth))
(define (bird::dead? bird)
  (bird 'bird::dead?))
(define (bird::move! bird dt)
  (bird 'bird::move! dt))
(define (bird::update bird dt)
  (bird 'bird::update dt))

(define (bird::new type health position vector speed)
  (define animation_dt 0)
  (define lifetime 0)
  (define nextframe? #f)
  (define right? #f)
  (define correction-height (* (/ (- scale 1) 2) unscaled-bird-height))
  (define correction-width (* (/ (- scale 1) 2) unscaled-bird-width))

  (define (bird::decrease-health! args)
    (let ((dhealth (car args)))
      (set! health (- health dhealth))))

  (define (bird::update args)
    (let* ((dt (car args))
           (passed-time (+ animation_dt dt)))
      ;lifetime update
      (set! lifetime (+ lifetime dt))
      ;reflection update
      (if (> (vector 'vec2D::x) 0)
          (set! right? #t)
          (set! right? #f))
      ;animation update
      (if (> passed-time bird-frame-time)
          (begin (set! animation_dt (modulo passed-time bird-frame-time)) 
                 (set! nextframe? #t))
          (begin (set! animation_dt passed-time) 
                 (set! nextframe? #f)))
      (bird::move! (list dt))))
      
  (define (bird::dead?)
    (<= health 0))

  (define (bird::move! args)
    (if (not (bird::dead?))
        ;dx is a displacement factor proportional to dt and speed of the bird.
        (let ((dx (* (/ (car args) 1000) speed))
              (temp-vector (vector 'vec2D::copy)))
          (temp-vector 'vec2D::*! dx)
          (position 'point2D::+! temp-vector)

          (define (top) (- (position 'point2D::y) correction-height))
          (define (left) (- (position 'point2D::x) correction-width))
          (define (bottom) (+ (position 'point2D::y) correction-height unscaled-bird-height))
          (define (right) (+ (position 'point2D::x) correction-width unscaled-bird-width))

          (cond ((< (top) 0) (if (< (vector 'vec2D::y) 0) (vector 'vec2D::flip-y!)))                       ; out of bounds left
                ((< (left) 0) (if (< (vector 'vec2D::x) 0) (vector 'vec2D::flip-x!)))                      ; out of bounds top
                ((> (right) screen-width) (if (> (vector 'vec2D::x) 0) (vector 'vec2D::flip-x!)))          ; out of bounds right
                ((> (bottom) screen-height) (if (>= (vector 'vec2D::y) 0) (vector 'vec2D::flip-y!)))))))   ; out of bounds bottom 
                                                  

  (define (dispatch-bird message . args)
    (cond ((eq? message 'bird::type) type)
          ((eq? message 'bird::health) health)
          ((eq? message 'bird::pos) position)
          ((eq? message 'bird::vec) vector)
          ((eq? message 'bird::speed) speed)
          ((eq? message 'bird::lifetime) lifetime)
          ((eq? message 'bird::decrease-health!) (bird::decrease-health! args))
          ((eq? message 'bird::dead?) (bird::dead?))
          ((eq? message 'bird::move!) (bird::move! args))
          ((eq? message 'bird::update) (bird::update args))
          ((eq? message 'bird::nextframe?) nextframe?)
          ((eq? message 'bird::right?) right?)
          (else (error "bird ADT unkown message" message))))
  dispatch-bird)
