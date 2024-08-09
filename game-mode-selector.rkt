;; Note, this ADT is rather useless at the current state of the game. 
;; However it provides easy expandability when it comes to having multiple gamemodes
;; Some de-init function would need to be made since I think tiles don't get removed from inactive gamemodes. (would require testing)
;; This functionality also needs to be added in Game ADT (i.e call cycle and update callbacks)

(define (game-mode-selector::new engine)
  ;; Initialize a list of game modes 
  ;; Should add functions to add gamemodes or pass a full list (although this is initialised once and should not change during runtime)
  ;; This need is therefore rather slim
  (define game-modes (list (main-game-mode::new engine)))
  (define current-mode-index 0)

  ;; Helper function to get the current game mode
  (define (current-game-mode)
    (list-ref game-modes current-mode-index))

  ;; Function to initialize the current game mode
  (define (initialize-current-mode)
    ((current-game-mode) 'game-mode::init))

  ;; Initialize the first game mode
  (initialize-current-mode)

  ;; Define functions to get draw and logic updates from the current game mode
  (define (game-mode-selector::get-draw-update)
  
    ((current-game-mode) 'game-mode::get-draw-update))

  (define (game-mode-selector::get-logic-update)
    ((current-game-mode) 'game-mode::get-logic-update))

  ;; Function to cycle to the next game mode
  (define (game-mode-selector::cycle-mode)
    ;; Increment the current mode index and wrap around if necessary
    (set! current-mode-index (modulo (+ 1 current-mode-index) (length game-modes)))
    ;; Initialize the new current game mode
    (initialize-current-mode))

  (define (dispatch-game-mode-selector message . args)
    (apply (case message
             ((get-draw-update) (lambda () (game-mode-selector::get-draw-update)))
             ((get-logic-update) (lambda () (game-mode-selector::get-logic-update)))
             ((cycle-mode) (lambda () (game-mode-selector::cycle-mode)))
             (else (lambda () (error "game-mode-selector ADT unknown message" message)))) args))

  dispatch-game-mode-selector)

