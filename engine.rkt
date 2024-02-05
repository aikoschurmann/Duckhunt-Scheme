(define (engine::initialise-update-callback! engine fun)
  (engine 'engine::initialise-update-callback! fun))
(define (engine::initialise-draw-callback! engine fun)
  (engine 'engine::initialise-draw-callback! fun))


(define (engine::new width height)
  (let ((window (make-window width height "Duck Hunt")))
  
    ;actively rendered ducks (list because it dynamically grows and shrinks)
    (define active-ducks '())
    (define duck-layer ((window 'new-layer!)))

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

    ;makes duck object and bitmap animation, stores them together and adds them to the active ducks list
    
    (define (engine::new-duck duck-position duck-vector)
      (let* ((duck (bird::new 'duck 1 duck-position duck-vector 6))
             (duck-tile-1 (make-bitmap-tile "duck/duck1.png" "duck/duck1mask.png"))
             (duck-tile-2 (make-bitmap-tile "duck/duck2.png" "duck/duck2mask.png"))
             (duck-tile-3 (make-bitmap-tile "duck/duck3.png" "duck/duck3mask.png"))
             (duck-animation (make-tile-sequence (list duck-tile-1 duck-tile-2 duck-tile-3)))
             (new-duck (cons duck duck-animation)))
        (set! active-ducks (cons new-duck active-ducks))
        ((duck-animation 'set-scale!) scale)
        ((duck-layer 'add-drawable!) duck-animation)))
      
    (define (engine::render-duck duck)
      (let ((duck-object (car duck))
            (duck-tile (cdr duck)))

        ;render at duck-object positions
        ((duck-tile 'set-x!) ((duck-object 'bird::pos) 'point2D::x))
        ((duck-tile 'set-y!) ((duck-object 'bird::pos) 'point2D::y))
        
        ;update animation
        (if (duck-object 'bird::nextframe?)
            ((duck-tile 'set-next!)))))

    (define (engine::update-duck duck dt)
      (let ((duck-object (car duck))
            (duck-tile (cdr duck)))
        
        ;updates lifetime, flying direction, animation status and movement
        (duck-object 'bird::update dt)

        ;rendering bird reflection
        (if (duck-object 'bird::right?)
            ((duck-tile 'set-x-scale!) scale)
            ((duck-tile 'set-x-scale!) (- scale)))))

    (define (engine::render-ducks)
      (define (render-ducks-iter lst)
        (if (not (null? lst))
            (begin
              (let ((duck (car lst)))
                (engine::render-duck duck))
              (render-ducks-iter (cdr lst)))))
      (render-ducks-iter active-ducks))
    
    (define (engine::update-ducks args)
      (let ((dt (car args)))
        (define (update-ducks-iter lst)
          (if (not (null? lst))
              (begin
                (let ((duck (car lst)))
                  (engine::update-duck duck dt))
                (update-ducks-iter (cdr lst)))))
        (update-ducks-iter active-ducks)))

    (define (engine::new-bird args)
      (let ((type (car args))
            (duck-position (cadr args))
            (duck-vector (caddr args)))
        (cond ((eq? type 'duck) (engine::new-duck duck-position duck-vector)))))

    (define (dispatch-engine message . args)
      (cond ((eq? message 'engine::set-background-colour!) (engine::set-background-colour! args))
            ((eq? message 'engine::initialise-update-callback!) (engine::initialise-update-callback! args))
            ((eq? message 'engine::initialise-draw-callback!) (engine::initialise-draw-callback! args))
            ((eq? message 'engine::start!) (engine::start!))
            ((eq? message 'engine::new-bird) (engine::new-bird args))
            ((eq? message 'engine::render-ducks) (engine::render-ducks))
            ((eq? message 'engine::update-ducks) (engine::update-ducks args))
            (else (error "engine ADT unknown message: " message))))
    dispatch-engine))