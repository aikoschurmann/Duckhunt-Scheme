;imports
(#%require "Graphics.rkt")
(#%require "position.rkt")

;constants
(define WIDTH 800)
(define HEIGHT 600)
(define FPS 500)
(define WINDOW_TITLE "Duck Hunt Project I")

;initialisation 
(define window (make-window WIDTH HEIGHT WINDOW_TITLE FPS))

(define pos (make_position 10 10))
(position? pos)

;abstractions
(define newlayer (window 'new-layer!))

;code
(define z_index_-1 (newlayer))
(define z_index_0 (newlayer))
(define z_index_1 (newlayer))

(define mijn-tile (make-tile 200 100))
((mijn-tile 'draw-rectangle!) 10 10 180 80 "red")
((z_index_0 'add-drawable!) mijn-tile)

((window 'set-update-callback!)
 (lambda (ms)
   (let* ((huidige-y (mijn-tile 'get-y))
          (nieuwe-y (+ huidige-y (* 0.10 ms))))
     ((mijn-tile 'set-y!) nieuwe-y))))
