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
  (let* ((window (make-window width height "Duck Hunt"))
         (input (input::new))
         (internal-time 0)
         (active-birds '())
         (active-power-ups '())
         (bird-layer ((window 'new-layer!)))
         (scenery-layer ((window 'new-layer!)))
         (stone-layer ((window 'new-layer!)))
         (bird-layer2 ((window 'new-layer!)))
         (ui-layer ((window 'new-layer!)))
         (score 0)
         (lives 3))


    (define (engine::reset!)
      (set! internal-time 0)
      (set! active-birds '())
      (set! active-power-ups '())
      ((bird-layer    'empty!))
      ((scenery-layer 'empty!))
      ((stone-layer   'empty!))
      ((bird-layer2   'empty!))
      ((ui-layer   'empty!))
      (set! score 0))

    ;useless crap
    (define (engine::set-background-colour! colour)
      ((window 'set-background!) colour))

    ;logic updating (dt)
    (define (engine::initialise-update-callback! function)
      ((window 'set-update-callback!) function))

    ;graphics updating (no dt)
    (define (engine::initialise-draw-callback! function)
      ((window 'set-draw-callback!) function))

    (define (engine::initialise-key-callback! function)
      ((window 'set-key-callback!) function))

    (define (engine::initialise-mouse-move-callback! function)
      ((window 'set-mouse-move-callback!) function))

    (define (engine::initialise-mouse-click-callback! function)
      ((window 'set-mouse-click-callback!) function))

    (define (engine::make-tile-sequence path start amount)
      (let loop ((current start) (result '()))
        (if (>= current (+ start amount))
            (make-tile-sequence (reverse result))
            (loop (+ current 1) 
                  (cons (make-bitmap-tile (string-append path (number->string current) ".png")
                                          (string-append path (number->string current) "mask.png")) result)))))

    (define (engine::make-falling-sequence type)
      (let ((animation (engine::make-tile-sequence (string-append "sprites/" (symbol->string type) "/" (symbol->string type)) 4 2)))
        ((animation 'set-scale!) scale)
        animation))

    (define (engine::add-bird-life-tiles! bird-object)
      (if (not (eq? (bird-object 'bird::type) 'false))
          (for-each (lambda (tile) 
                      ((tile 'set-scale!) scale)
                      (if (= (bird-object 'bird::z-index) 1)  
                        ((bird-layer 'add-drawable!) tile) 
                        ((bird-layer2 'add-drawable!) tile)))    
                    (bird-object 'bird::life-list))))

    (define (engine::remove-bird-life-tiles! bird-object)
      (if (not (eq? (bird-object 'bird::type) 'false))
          (for-each (lambda (tile) 
                      ((tile 'set-scale!) scale)
                      (if (= (bird-object 'bird::z-index) 1)  
                        ((bird-layer 'remove-drawable!) tile) 
                        ((bird-layer2 'remove-drawable!) tile)))    
                    (bird-object 'bird::life-list))))

    ;makes duck object and bitmap animation, stores them together and adds them to the active birds list
    (define (engine::new-bird type bird-object tile-amount)
      (let* ((base-path (string-append "sprites/" (symbol->string type) "/" (symbol->string type)))
             (tile-sequence (engine::make-tile-sequence base-path 1 tile-amount)))

        
        (engine::add-bird-life-tiles! bird-object)

        (set! active-birds (cons (cons bird-object tile-sequence) active-birds))
        ((tile-sequence 'set-scale!) scale)
        (if (= (bird-object 'bird::z-index) 1)
            ((bird-layer 'add-drawable!) tile-sequence)
            ((bird-layer2 'add-drawable!) tile-sequence))))

    ; (cons type tile)
    (define (engine::new-power-up type position)
      (let* ((path (string-append "sprites/" (symbol->string type) "/" (symbol->string type) "1.png"))
             (tile (make-bitmap-tile path))
             (power-up (cons type tile)))
        ((tile 'set-scale!) scale)
        (set! active-power-ups (cons power-up active-power-ups))
        ((tile 'set-x!) (position 'point2D::x))
        ((tile 'set-y!) (position 'point2D::y))
        ((ui-layer 'add-drawable!) tile)))


    (define (engine::remove-drawable! tile z-index) 
      (if (= z-index 1)
          ((bird-layer 'remove-drawable!) tile)
          ((bird-layer2 'remove-drawable!) tile)))

    (define (engine::add-drawable! tile z-index) 
      (if (= z-index 1)
          ((bird-layer 'add-drawable!) tile)
          ((bird-layer2 'add-drawable!) tile)))

    ;; car -> ADT cdr -> TileSequence
    (define (engine::next-duck-animation! bird-cons)
      (let* ((bird-object (car bird-cons))
             (bird-tile (cdr bird-cons))
             (z-index (bird-object 'bird::z-index))
             (new-animation (engine::make-falling-sequence (bird-object 'bird::type))))
        (engine::remove-drawable! bird-tile z-index)
        (bird-object 'bird::speed! 5)
        (set-cdr! bird-cons new-animation)
        (engine::add-drawable! new-animation z-index)))

    (define (engine::render-bird-icons bird)
      (let* ((bird-object (car bird))
            (bird-tile (cdr bird))
            (life-list (bird-object 'bird::life-list))
            (water-tile (bird-object 'bird::water-tile)))

      (if (not (bird-object 'bird::slowed?))
        (engine::remove-drawable! (bird-object 'bird::water-tile) (bird-object 'bird::z-index)))


        (define (draw-lives rest index)
          (if (not (null? rest))
            (let ((health-tile (car rest)))
              ((health-tile 'set-x!) (+ ((bird-object 'bird::pos) 'point2D::x) (* index 20)))
              ((health-tile 'set-y!) (- ((bird-object 'bird::pos) 'point2D::y) 30))
              (draw-lives (cdr rest) (+ index 1)))))

        (draw-lives life-list 0)
        ((water-tile 'set-x!) ((bird-object 'bird::pos) 'point2D::x))
        ((water-tile 'set-y!) (+ ((bird-object 'bird::pos) 'point2D::y) 30 (bird-tile 'get-h)))))

    (define (engine::render-bird bird)
      (let ((bird-object (car bird))
            (bird-tile (cdr bird)))

        ;render at bird-object positions
        ((bird-tile 'set-x!) ((bird-object 'bird::pos) 'point2D::x))
        ((bird-tile 'set-y!) ((bird-object 'bird::pos) 'point2D::y))

        (engine::render-bird-icons bird)

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
    
    (define (engine::update-birds dt)
      (define (update-birds-iter lst)
        (if (not (null? lst))
            (begin
              (let ((bird (car lst)))
                (engine::update-bird bird dt))
              (update-birds-iter (cdr lst)))))
      (update-birds-iter active-birds))

    (define (engine::update-internal-time! dt)
      (set! internal-time (+ internal-time dt)))

    (define (engine::active-birds! list)
      (set! active-birds list))

    (define (engine::lives! new-lives)
      (set! lives new-lives))

    (define (engine::collision? tile point)
      (define correction-height (* (/ (- scale 1) 2) (tile 'get-h)))
      (define correction-width (* (/ (- scale 1) 2) (tile 'get-w)))
      (define (top) (- (tile 'get-y) correction-height))
      (define (left) (- (tile 'get-x) correction-width))
      (define (bottom) (+ (tile 'get-y) correction-height (tile 'get-h)))
      (define (right) (+ (tile 'get-x) correction-width (tile 'get-w)))

      (and (>= (point 'point2D::x) (left))
           (<= (point 'point2D::x) (right))
           (>= (point 'point2D::y) (top))
           (<= (point 'point2D::y) (bottom))))

    (define (engine::remove-live!)
      (set! lives (- lives 1)))

    (define (engine::add-score! score_add)
      (set! score (+ score score_add)))

    (define (engine::active-power-ups! new)
      (set! active-power-ups new))

    (define (dispatch-engine message . args)
      (apply (cond ((eq? message 'engine::set-background-colour!) engine::set-background-colour!)
                   ((eq? message 'engine::initialise-update-callback!) engine::initialise-update-callback!)
                   ((eq? message 'engine::initialise-draw-callback!) engine::initialise-draw-callback!)
                   ((eq? message 'engine::initialise-key-callback!) engine::initialise-key-callback!)
                   ((eq? message 'engine::initialise-mouse-move-callback!) engine::initialise-mouse-move-callback!)
                   ((eq? message 'engine::initialise-mouse-click-callback!) engine::initialise-mouse-click-callback!)
                   
                   ((eq? message 'engine::new-bird) engine::new-bird) ;documented (generates tile-sequence and pairs it with the passed bird adt adds this construction to active birds)
                   ((eq? message 'engine::render-birds) engine::render-birds) ;documented (uses birdADT to update tileSequence)
                   ((eq? message 'engine::update-birds) engine::update-birds) ;documented (updates BirdADT and does reflections of TileSequence)
                   ((eq? message 'engine::active-birds!) engine::active-birds!) ;documented (setter of variable)
                   ((eq? message 'engine::remove-live!) engine::remove-live!) ;not documented because trivial and this adt is enormous (decreases lives by 1)
                   ((eq? message 'engine::lives!) engine::lives!) ;idem (sets lives to passed parameter)
                   ((eq? message 'engine::add-score!) engine::add-score!) ;idem (increments score by passed parameter)
                   ((eq? message 'engine::update-internal-time!) engine::update-internal-time!) ;idem (increments internal-time by dt)
                   ((eq? message 'engine::next-duck-animation!) engine::next-duck-animation!) ;documented (changes (Bird . Tilesequence) to its faling variant)
                   ((eq? message 'engine::collision?) engine::collision?) ;documented (checks if point falls within bitmap (takes into account weird center top right scaling))
                   ((eq? message 'engine::add-drawable!) engine::add-drawable!) ;documented (adds a bitmap to either the layer behind the trees or infront of it depending on z-index)
                   ((eq? message 'engine::new-power-up) engine::new-power-up) ;documented (generates bitmap adds it to correct layer and adds this to active-power-ups list)
                   ((eq? message 'engine::add-bird-life-tiles!) engine::add-bird-life-tiles!) ;documented (adds life tiles of bird to correct layer)
                   ((eq? message 'engine::remove-bird-life-tiles!) engine::remove-bird-life-tiles!) ;documented (removes life tiles of bird from correct layer)
                   ;((eq? message 'engine::generate-position-middle) engine::generate-position-middle)
                   ((eq? message 'engine::input) (lambda () input));idem (simply a getter)
                   ((eq? message 'engine::ui-layer) (lambda () ui-layer)) ;fused documentation           
                   ((eq? message 'engine::bird-layer) (lambda () bird-layer)) ;fused documentation  
                   ((eq? message 'engine::bird-layer2) (lambda () bird-layer2)) ;fused documentation  
                   ((eq? message 'engine::scenery-layer) (lambda () scenery-layer)) ;fused documentation  
                   ((eq? message 'engine::stone-layer) (lambda () stone-layer)) ;fused documentation  
                   ((eq? message 'engine::active-birds) (lambda () active-birds)) ; documented (returns (list (Bird . Tilesequence) ...))
                   ((eq? message 'engine::active-power-ups) (lambda () active-power-ups)) ; documented (returns (list (sumbol . Bitmap) ...))
                   ((eq? message 'engine::active-power-ups!) engine::active-power-ups!) ;idem (simple setter)
                   ((eq? message 'engine::internal-time) (lambda () internal-time)) ;idem (simple getter)
                   ((eq? message 'engine::lives) (lambda () lives)) ;idem (simple getter)
                   ((eq? message 'engine::score) (lambda () score)) ;idem (simple getter)
                   ((eq? message 'engine::reset!) engine::reset!) ;documented (resets internal variables)
        
                   (else (lambda () (error "engine ADT unknown message: " message)))) args))

    dispatch-engine))