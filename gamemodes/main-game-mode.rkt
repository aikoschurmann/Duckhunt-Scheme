(define (main-game-mode::new engine)
  (let ((input (engine 'engine::input)))
    (define (init) 0)
  
    (define (draw-update) 
        (engine 'engine::render-birds))
  
    (define (logic-update dt) 
        ;code to add duck upon clicking d
        (if (input 'input::key-pressed? #\d)
          (let* ((location (input 'input::mouse-location))
                 (point (point2D::new (car location) (cdr location)))
                 (direction-vector (vec2D::random-unit-vector)))
            (direction-vector 'vec2D::*! 50)
            (engine 'engine::new-bird 'duck point direction-vector)))
        
        (engine 'engine::update-birds dt)
        (input 'input::reset!))
  
    (define (dispatch-game-mode message . args)
      (cond ((eq? message 'game-mode::init) (init))
            ((eq? message 'game-mode::get-draw-update) draw-update)
            ((eq? message 'game-mode::get-logic-update) logic-update)
            (else (error "game-mode ADT unkown message" message))))
    dispatch-game-mode))