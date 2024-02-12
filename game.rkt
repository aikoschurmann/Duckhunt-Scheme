(define (game::new)
 (let* ((engine (engine::new screen-width screen-height))
        (game-mode-selector (game-mode-selector::new 'test engine)))
  
  (define (start!)
    (let* ((draw-update (game-mode-selector 'get-draw-update))
           (logic-update (game-mode-selector 'get-logic-update))
           (input (engine 'engine::input))
           (key-update (input 'input::get-key-callback))
           (mouse-move-update (input 'input::get-mouse-move-callback)))

             ;fix syntax !!!
             (engine::initialise-draw-callback! engine draw-update)
             (engine::initialise-update-callback! engine logic-update)
             (engine::initialise-key-callback! engine key-update)
             (engine::initialise-mouse-move-callback! engine mouse-move-update)))
  
  (define (dispatch-game message . args)
    (cond ((eq? message 'test) args)
          ((eq? message 'start!) (start!))
          (else (error "game ADT unkown message" message))))
  dispatch-game))