(define bird-position (point2D::new (/ screen-width 2) (/ screen-height 2)))
(define bird-position2 (point2D::new (/ screen-width 2) (/ screen-height 2)))

;50 * 4 = 200 meaning vx = 200px / s 
;40 * 4 = 160 meaning vy = 160px / s 
(define bird-vector (vec2D::new 50 40))
(define bird-vector2 (vec2D::new 60 40))


(define (main-game-mode::new engine)
  (define (init)
    (engine 'engine::new-bird 'duck bird-position bird-vector)
    (engine 'engine::new-bird 'duck bird-position2 bird-vector2))

  (define (draw-update) 
      (engine 'engine::render-ducks))

  (define (logic-update dt) 
      (engine 'engine::update-ducks dt))

  (define (dispatch-game-mode message . args)
    (cond ((eq? message 'game-mode::init) (init))
          ((eq? message 'game-mode::get-draw-update) draw-update)
          ((eq? message 'game-mode::get-logic-update) logic-update)
          (else (error "game-mode ADT unkown message" message))))
  dispatch-game-mode)


