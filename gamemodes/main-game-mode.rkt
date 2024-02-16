(define (main-game-mode::new engine)
  (let ((input (engine 'engine::input))
        (air-gun (gun::new 'air-gun 1 (* 20 scale) 200 -1 #f))
        (boom-gun (gun::new 'boom-gun 1 (* 20 scale) 200 -1 #t)))

    (define crosshair (make-bitmap-tile "crosshair.png" "crosshair_mask.png"))
    (define gun-switcher (vector air-gun boom-gun))
    (define active-gun-index 1)
    (define obstacle-hitbox (rect2D::new (point2D::new 100 100) 100 100))

  
    (define (init)  
      ((crosshair 'set-scale!) ui-scale))

    (define (do-splash-hits list pos active-gun)
      (if  (not (null? list))
        (let* ((current-bird (car (car list)))
               (bird-location (current-bird 'bird::pos-center))
               (distance (bird-location 'point2D::distance pos)))
          (if (< distance (active-gun 'gun::range))
            (begin (current-bird 'bird::decrease-health! (active-gun 'gun::damage)) 
                   (if (not (current-bird 'bird::hasDied?))
                     (begin (engine 'engine::next-duck-animation! (car list))
                            (current-bird 'bird::hasDied!)))))
          (do-splash-hits (cdr list) pos active-gun))))

    (define (prune-birds list)
      (if (null? list)
        '()
        (let ((curr-bird (car (car list)))
              (animation (cdr (car list))))
          (if (> ((curr-bird 'bird::pos) 'point2D::y) screen-height)
            (begin (((engine 'engine::bird-layer) 'remove-drawable!) animation)
                     (prune-birds (cdr list)))
            (cons (car list) (prune-birds (cdr list)))))))
  
    (define (draw-update) 
        ;code to add duck upon clicking d
        (if (input 'input::key-down? #\b)
          (let* ((location (input 'input::mouse-location)) ; get mouse location (conscell)
                 (point (point2D::new (car location) (cdr location))) ; convert to point2D
                 (direction-vector (vec2D::random-unit-vector))) ; generate random vector (len 1)
            (direction-vector 'vec2D::*! bird-speed) 
            (engine 'engine::new-bird 'duck point direction-vector))) 

        ;shooting code
        ;click

        (if (input 'input::button-down? 'left)
          (let ((mouse-location (input 'input::mouse-location))
                (active-birds (engine 'engine::active-birds))
                (active-gun (vector-ref gun-switcher (modulo active-gun-index (vector-length gun-switcher)))))
            ((crosshair 'set-x!) (- (car mouse-location) (/ (crosshair 'get-w) 2)))
            ((crosshair 'set-y!) (- (cdr mouse-location) (/ (crosshair 'get-h) 2)))
            (((engine 'engine::ui-layer) 'add-drawable!) crosshair)
            
            (if (active-gun 'gun::shoot!)
              (if (active-gun 'gun::splash?) 
                (do-splash-hits (engine 'engine::active-birds) (point2D::new (car mouse-location) (cdr mouse-location)) active-gun)))))
        
        ;release
        (if (input 'input::button-up? 'left)
          (((engine 'engine::ui-layer) 'remove-drawable!) crosshair))

        (engine 'engine::render-birds)

        ;clear out of bounds birds
        ;TODO: make counter for falling birds if 0 then pruning is not needed
        (engine 'engine::active-birds! (prune-birds (engine 'engine::active-birds)))
        ;technically not a draw function but draw-update executes after logic-update
        (input 'input::reset!))
  
    (define (logic-update dt) 
        (let ((active-gun (vector-ref gun-switcher (modulo active-gun-index (vector-length gun-switcher)))))
          (engine 'engine::update-internal-time! dt)
          (engine 'engine::update-birds dt)
          (active-gun 'gun::update! (engine 'engine::internal-time))))
  
    (define (dispatch-game-mode message . args)
      (cond ((eq? message 'game-mode::init) (init))
            ((eq? message 'game-mode::get-draw-update) draw-update)
            ((eq? message 'game-mode::get-logic-update) logic-update)
            (else (error "game-mode ADT unkown message" message))))
    dispatch-game-mode))