(define (bird::new type health position vector speed score z-index)
  (let* ((animation-dt 0)
         (nextframe? #f)
         (right? #f)
         ;; Needed because scaling isn't from the origin
         (correction-height (* (/ (- scale 1) 2) unscaled-bird-height))
         (correction-width (* (/ (- scale 1) 2) unscaled-bird-width))
         ;; Needed to know if the bird is falling or not
         (hasDied? #f)
         (fall-vector (vec2D::new 0 fall-speed-factor))
         (bounces 0)
         (bird::generate-life-list (lambda () (map (lambda (_) (make-bitmap-tile "sprites/icons/feather1.png" "sprites/icons/feather1mask.png")) (make-list (max health 0) '()))))
         (slowed? #f)
         (slowed-time-remaining 0)
         (life-list (bird::generate-life-list))
         (water-tile (make-bitmap-tile "sprites/icons/drop1.png" "sprites/icons/drop1mask.png"))
         ;; only used for crows
         (has-target? #f)
         (target #f)
         (is-stone? #t))

    ((water-tile 'set-scale!) scale)
    
    (define (increase-bounce)
      (set! bounces (+ bounces 1)))

    (define (bird::decrease-health! amount)
      (set! health (- health amount)))

    (define (bird::speed! new-speed)
      (set! speed new-speed))
    (define (bird::vec! new-vec)
      (set! vector new-vec))

    (define (bird::life-list! new-list)
      (set! life-list new-list))

    (define (bird::update! dt)
      ;; Reflection update
      (if (> (vector 'vec2D::x) 0)
          (set! right? #t)
          (set! right? #f))
      ;; Update animation time
      (set! animation-dt (+ animation-dt dt))
      ;; Calculate if enough time has passed for the next frame
      (set! nextframe? (> animation-dt bird-frame-time))
      ;; Reset animation-dt
      (set! animation-dt (modulo animation-dt bird-frame-time))

      (set! slowed-time-remaining (max (- slowed-time-remaining dt) 0))
      (if (= slowed-time-remaining 0) (set! slowed? #f))
      ;; Move bird given dt
      (bird::move! dt))

    (define (bird::dead?)
      (<= health 0))

    (define (center) (point2D::new (+ (position 'point2D::x) (/ unscaled-bird-width 2)) (+ (position 'point2D::y) (/ unscaled-bird-height 2))))
    (define (top) (- (position 'point2D::y) correction-height))
    (define (left) (- (position 'point2D::x) correction-width))
    (define (bottom) (+ (position 'point2D::y) correction-height unscaled-bird-height))
    (define (right) (+ (position 'point2D::x) correction-width unscaled-bird-width))
    
    (define (check_bounds_and_reflect!)
      (cond ((< (top) 0) (if (< (vector 'vec2D::y) 0) (begin (increase-bounce) (vector 'vec2D::flip-y!))))  ; Out of bounds left
            ((< (left) 0) (if (< (vector 'vec2D::x) 0) (begin (increase-bounce) (vector 'vec2D::flip-x!))))  ; Out of bounds top
            ((> (right) screen-width) (if (> (vector 'vec2D::x) 0) (begin (increase-bounce) (vector 'vec2D::flip-x!))))  ; Out of bounds right
            ((> (bottom) screen-height) (if (>= (vector 'vec2D::y) 0) (begin (increase-bounce) (vector 'vec2D::flip-y!))))))  ; Out of bounds bottom

    ;; Note: speed is individual for each Bird ADT on creation
    ;; bird-speed is a constant to give a base speed
    ;; dx is therefore a displacement factor proportional to dt and speed of the bird.
    (define (bird::dx dt)
      (* (/ dt 1000) bird-speed speed (if slowed? slowed-factor 1)))

    ;; d-vector is a factor with the same direction as the vector provided and proportional to dt and speed of the bird.
    (define (bird::d-vector dt vector)
      (let ((copy (vector 'vec2D::copy)))
        (copy 'vec2D::*! (bird::dx dt))
        copy))

    ;; Fall straight down
    (define (bird::move-dead! dt)
      (let ((d-vec (bird::d-vector dt fall-vector)))
        (position 'point2D::+! d-vec)))
        
    ;; Parabolic-const is another global const
    ;; It can be seen as the derivative of nx^2
    ;; And therefore controls how fast it 'curves'.
   (define (bird::move-false! dt)
      (let ((dx (bird::dx dt))
            (copy (vector 'vec2D::copy)))
        (copy 'vec2D::*! dx)
        (position 'point2D::+! copy)
        (vector 'vec2D::y! (- (vector 'vec2D::y)   (* parabolic-const dx))))
        )

    (define (bird::move-bird! dt)
      (let ((d-vec (bird::d-vector dt vector)))
        (position 'point2D::+! d-vec))
      (if (< bounces max-bounces)
          (check_bounds_and_reflect!)))

    (define (bird::slowed!) 
      (set! slowed? #t)
      (set! slowed-time-remaining slowed-duration)
    )
    ;; Returns tile
    (define (generate-stone-from-vector)
      (let ((random-index (random 0 (vector-length (stone-vector)))))
        (vector-ref (vector-ref (stone-vector) random-index) 0)))

    (define (generate-crow-vector) 
      (if has-target?
          vector
          (if is-stone?
            (let* ((stone (generate-stone-from-vector))
                  (target-point (point2D::new (stone 'get-x) (stone 'get-y))))
              (set! is-stone? #f)
              (set! has-target? #t)
              (set! target target-point)
              (set! bounces (+ bounces 1))
              (vec2D::get_normalised_vec position target-point))
            
            (let ((target-point (generate-position-middle)))
              (set! is-stone? #t)
              (set! has-target? #t)
              (set! target target-point)
              (set! bounces (+ bounces 1))
              (vec2D::get_normalised_vec position target-point)))))

    (define (bird::move-crow! dt)
      (let* ((gen-vector (generate-crow-vector))
             (d-vec (bird::d-vector dt vector)))
        (set! vector gen-vector)
        (position 'point2D::+! d-vec))

      (if (< bounces max-bounces) 
        (if (< (position 'point2D::distance target) crow-target-radius)
          (set! has-target? #f))))

    (define (bird::move-seagull! dt)
      (let ((d-vec (bird::d-vector dt vector)))
        (position 'point2D::+! d-vec)))

    (define (bird::move! dt)
      (cond (hasDied? (bird::move-dead! dt))
            ((eq? type 'false) (bird::move-false! dt))
            ((eq? type 'seagull) (bird::move-seagull! dt))
            ((eq? type 'crow) (bird::move-crow! dt))
            (else (bird::move-bird! dt))))

    (define (bird::hasDied!)
      (set! hasDied? #t))

    (define (dispatch-bird message . args)
      (apply (case message
               ((bird::type) (lambda () type))
               ((bird::health) (lambda () health))
               ((bird::pos) (lambda () position))
               ((bird::vec) (lambda () vector))
               ((bird::speed) (lambda () speed))
               ((bird::score) (lambda () score))
               ((bird::z-index) (lambda () z-index)) ; used for defining on which layer the bird is (before or behind trees)
               ((bird::life-list) (lambda () life-list)) ; list of tiles (feathers) to represent health
               ((bird::hasDied?) (lambda () hasDied?)) ; true if the bird is in its falling state
               ((bird::nextframe?) (lambda () nextframe?)) ; true if the next frame (flying sequence) should happen
               ((bird::right?) (lambda () right?)) ; true if the bird is flying to the right
               ((bird::water-tile) (lambda () water-tile)) ; a tile used for displaying if the bird is in its slowed state
               ((bird::slowed?) (lambda () slowed?)) ; true if the bird is in it's slowed state (water gun)

               ((bird::dead?) bird::dead?) ; true if health is below or equal to 0
               ((bird::decrease-health!) bird::decrease-health!) ; decreases health with n
               ((bird::speed!) bird::speed!) 
               ((bird::vec!) bird::vec!)
               ((bird::move!) bird::move!)
               ((bird::update) bird::update!)
               ((bird::hasDied!) bird::hasDied!)
               ((bird::life-list!) bird::life-list!)
               ((bird::slowed!) bird::slowed!)
               ((bird::generate-life-list) bird::generate-life-list) ; generates a list with (life) amount of tiles (feather)

               ((bird::center) center)
               ((bird::left) left)
               ((bird::right) right)
               ((bird::top) top)
               ((bird::bottom) bottom)
               

               (else (error "bird ADT unknown message" message))) args))

    dispatch-bird))
