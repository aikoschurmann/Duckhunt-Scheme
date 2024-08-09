(define (game::new)
  (let* ((engine (engine::new screen-width screen-height))
         (game-mode-selector (game-mode-selector::new engine)))
    
    ;; Define the start! function to initialize the engine with various callbacks
    (define (start!)
      (let* ((draw-update (game-mode-selector 'get-draw-update))
             (logic-update (game-mode-selector 'get-logic-update))
             (input (engine 'engine::input))
             (key-update (input 'input::get-key-callback))
             (mouse-move-update (input 'input::get-mouse-move-callback))
             (mouse-click-update (input 'input::get-mouse-click-callback)))
        
        ;; Initialize the engine with said respective callbacks
        ;; TODO: Should get converted back from wrapper functions to apply case variant 
        (engine::initialise-draw-callback! engine draw-update)
        (engine::initialise-update-callback! engine logic-update)
        (engine::initialise-key-callback! engine key-update)
        (engine::initialise-mouse-move-callback! engine mouse-move-update)
        (engine::initialise-mouse-click-callback! engine mouse-click-update)))
    
    (define (dispatch-game message . args)
      (apply (case message
               ((start!) (lambda () (start!)))
               (else (lambda () (error "game ADT unknown message" message)))) args))
    

    dispatch-game))


