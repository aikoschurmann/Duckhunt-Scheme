(define (game-mode-selector::new game-mode engine)
  
  (define main-game-mode (main-game-mode::new engine))
  (define active-level main-game-mode)
  (main-game-mode 'game-mode::init)

  (define (game-mode-selector::game-mode! game-mode)
    (set! active-game-mode game-mode))   

  (define (game-mode-selector::get-draw-update)
    
    (active-level 'game-mode::get-draw-update))

  (define (game-mode-selector::get-logic-update)
    (active-level 'game-mode::get-logic-update))

  (define (dispatch-game-mode-selector message . args)
    (cond ((eq? message 'active-level!) (game-mode-slector::game-mode! args))
          ((eq? message 'get-draw-update) (game-mode-selector::get-draw-update))
          ((eq? message 'get-logic-update) (game-mode-selector::get-logic-update))
          (else (error "game-mode-selector ADT unkown message" message))))
  dispatch-game-mode-selector)