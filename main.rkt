(#%require "Graphics.rkt")
(#%require (only racket random error))

(load "constants.rkt")

(load "point2D.rkt")
(load "vec2D.rkt")

(load "bird.rkt")
(load "gun.rkt")

(load "engine.rkt")

(load "game.rkt")

(load "game-mode-selector.rkt")
(load "input.rkt")

(load "gamemodes/main-game-mode.rkt")


(define game (game::new))
(game 'start!)