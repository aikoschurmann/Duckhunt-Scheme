(define (engine::initialise-update-callback! engine fun)
  (engine 'engine::initialise-update-callback! fun))
(define (engine::initialise-draw-callback! engine fun)
  (engine 'engine::initialise-draw-callback! fun))
(define (engine::initialise-key-callback! engine fun)
  (engine 'engine::initialise-key-callback! fun))
(define (engine::initialise-mouse-move-callback! engine fun)
  (engine 'engine::initialise-mouse-move-callback! fun))
(define (engine::initialise-mouse-click-callback! engine fun)
  (engine 'engine::initialise-mouse-click-callback! fun))

(define (engine::new width height)
  (let ((window (make-window width height "Duck Hunt")) 
        (input (input::new)))
  
    ;actively rendered birds (list because it dynamically grows and shrinks)
    (define active-birds '())
    (define bird-layer ((window 'new-layer!)))
    

    ;useless crap
    (define (engine::set-background-colour! args)
      (let ((colour (car args)))
        ((window 'set-background!) colour)))

    ;logic updating (dt)
    (define (engine::initialise-update-callback! args)
      (let ((function (car args)))
        ((window 'set-update-callback!) function)))

    ;graphics updating (no dt)
    (define (engine::initialise-draw-callback! args)
      (let ((function (car args)))
        ((window 'set-draw-callback!) function)))

    (define (engine::initialise-key-callback! args)
      (let ((function (car args)))
        ((window 'set-key-callback!) function)))

    (define (engine::initialise-mouse-move-callback! args)
      (let ((function (car args)))
        ((window 'set-mouse-move-callback!) function)))

    (define (engine::initialise-mouse-click-callback! args)
      (let ((function (car args)))
        ((window 'set-mouse-click-callback!) function)))

    ;makes duck object and bitmap animation, stores them together and adds them to the active birds list
    ;TODO: make generic
    (define (engine::new-duck duck-position duck-vector)
      (let* ((duck (bird::new 'duck 1 duck-position duck-vector 6))
             (duck-tile-1 (make-bitmap-tile "duck/duck1.png" "duck/duck1mask.png"))
             (duck-tile-2 (make-bitmap-tile "duck/duck2.png" "duck/duck2mask.png"))
             (duck-tile-3 (make-bitmap-tile "duck/duck3.png" "duck/duck3mask.png"))
             (duck-animation (make-tile-sequence (list duck-tile-1 duck-tile-2 duck-tile-3)))
             (new-duck (cons duck duck-animation)))
        (set! active-birds (cons new-duck active-birds))
        ((duck-animation 'set-scale!) scale)
        ((bird-layer 'add-drawable!) duck-animation)))
      
    (define (engine::render-bird bird)
      (let ((bird-object (car bird))
            (bird-tile (cdr bird)))

        ;render at bird-object positions
        ((bird-tile 'set-x!) ((bird-object 'bird::pos) 'point2D::x))
        ((bird-tile 'set-y!) ((bird-object 'bird::pos) 'point2D::y))
        
        ;update animation
        (if (bird-object 'bird::nextframe?)
            ((bird-tile 'set-next!)))))

    (define (engine::render-birds)
      (define (render-birds-iter lst)
        (if (not (null? lst))
            (begin
              (let ((bird (car lst)))
                (engine::render-bird bird))
              (render-birds-iter (cdr lst)))))
      (render-birds-iter active-birds))

    (define (engine::update-bird bird dt)
      (let ((bird-object (car bird))
            (bird-tile (cdr bird)))
        
        ;updates lifetime, flying direction, animation status and movement
        (bird-object 'bird::update dt)

        ;setting bird reflection
        (if (bird-object 'bird::right?)
            ((bird-tile 'set-x-scale!) scale)
            ((bird-tile 'set-x-scale!) (- scale)))))
    
    (define (engine::update-birds args)
      (let ((dt (car args)))
        (define (update-birds-iter lst)
          (if (not (null? lst))
              (begin
                (let ((bird (car lst)))
                  (engine::update-bird bird dt))
                (update-birds-iter (cdr lst)))))
        (update-birds-iter active-birds)))

    (define (engine::new-bird args)
      (let ((type (car args))
            (bird-position (cadr args))
            (bird-vector (caddr args)))
        (cond ((eq? type 'duck) (engine::new-duck bird-position bird-vector)))))



    (define (dispatch-engine message . args)
      (cond ((eq? message 'engine::set-background-colour!) (engine::set-background-colour! args))
            ((eq? message 'engine::initialise-update-callback!) (engine::initialise-update-callback! args))
            ((eq? message 'engine::initialise-draw-callback!) (engine::initialise-draw-callback! args))
            ((eq? message 'engine::initialise-key-callback!) (engine::initialise-key-callback! args))
            ((eq? message 'engine::initialise-mouse-move-callback!) (engine::initialise-mouse-move-callback! args))
            ((eq? message 'engine::initialise-mouse-click-callback!) (engine::initialise-mouse-click-callback! args))
            ((eq? message 'engine::start!) (engine::start!))
            ((eq? message 'engine::new-bird) (engine::new-bird args))
            ((eq? message 'engine::render-birds) (engine::render-birds))
            ((eq? message 'engine::update-birds) (engine::update-birds args))
            ((eq? message 'engine::input) input)
            (else (error "engine ADT unknown message: " message))))
    dispatch-engine))