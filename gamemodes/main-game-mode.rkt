(define (main-game-mode::new engine)
  (let ((input (engine 'engine::input))
        (air-gun (gun::new 'air-gun 1 (* 20 scale) 500 -1 #f))
        (boom-gun (gun::new 'boom-gun 1 (* 20 scale) 200 -1 #t)))

    (define crosshair (make-bitmap-tile "crosshair.png" "crosshair_mask.png"))

    (define grass_1 (make-bitmap-tile "grass.png" "grass_mask.png"))
    (define grass_2 (make-bitmap-tile "grass.png" "grass_mask.png"))
    (define grass_3 (make-bitmap-tile "grass.png" "grass_mask.png"))
    
    (define tree (make-bitmap-tile "tree.png" "tree_mask.png"))

    (define gun-switcher (vector air-gun boom-gun))
    (define active-gun-index 0)
    ;(define obstacle (rect2D::new (point2D::new 100 100) 100 100))

  
    (define (init)  
      ((crosshair 'set-scale!) ui-scale)



      ;TODO: make a function for it and fix scaling math because its wrong
      (((engine 'engine::scenery-layer) 'add-drawable!) grass_1)
      ((grass_1 'set-scale!) ui-scale)
      ((grass_1 'set-y!) (- screen-height (grass_1 'get-h)))
      ((grass_1 'set-x!) (/ (grass_1 'get-w) 2))

      (((engine 'engine::scenery-layer) 'add-drawable!) grass_2)
      ((grass_2 'set-scale!) ui-scale)
      ((grass_2 'set-y!) (- screen-height (grass_2 'get-h)))
      ((grass_2 'set-x!) (- (+ (/ (grass_2 'get-w) 2) (* (grass_2 'get-w) ui-scale)) 2)) ;-2 due to mask interpolation giving halo and + width / 2 due to weird scaling

      (((engine 'engine::scenery-layer) 'add-drawable!) grass_3)
      ((grass_3 'set-scale!) ui-scale)
      ((grass_3 'set-y!) (- screen-height (grass_3 'get-h)))
      ((grass_3 'set-x!) (- (+ (/ (grass_3 'get-w) 2) (* (grass_3 'get-w) ui-scale 2)) 4)) ;-2 again due to mask interpolation giving halo
      (((engine 'engine::scenery-layer) 'add-drawable!) tree)
      
      ((tree 'set-scale!) ui-scale)
      ((tree 'set-y!) (- screen-height (tree 'get-h) 50))
      ((tree 'set-x!) (+ (/ (tree 'get-w) 2) 100))

      (engine 'engine::set-background-colour! "lightblue")
      )

    ;TODO: improve syntax
    (define (do-splash-hits list pos active-gun)
      (if (not (null? list))
        (let* ((current-bird (car (car list)))
               (bird-location (current-bird 'bird::pos-center))
               (distance (bird-location 'point2D::distance pos)))
          (if (< distance (active-gun 'gun::range))
            (begin (current-bird 'bird::decrease-health! (active-gun 'gun::damage)) 
                   (if (not (and (current-bird 'bird::hasDied?) (current-bird 'bird::dead?)))
                     (begin (engine 'engine::next-duck-animation! (car list))
                            (current-bird 'bird::hasDied!))))
                            (engine 'engine::add-score! (current-bird 'bird::score)))
          (do-splash-hits (cdr list) pos active-gun))))

    (define (do-hits list pos active-gun)
      (if (not (null? list))
        (let* ((current-bird (car (car list)))
               (bird-tile (cdr (car list)))
               (bird-location (current-bird 'bird::pos-center)))
          (if (engine 'engine::collision? bird-tile pos)
            (begin (current-bird 'bird::decrease-health! (active-gun 'gun::damage))
                    (if (not (and (current-bird 'bird::hasDied?) (current-bird 'bird::dead?)))
                      (begin (engine 'engine::next-duck-animation! (car list))
                             (current-bird 'bird::hasDied!)))))      
            (do-hits (cdr list) pos active-gun))))

    (define (prune-birds list)
      (if (null? list)    
        '()
        (let ((curr-bird (car (car list)))
              (animation (cdr (car list))))
          (if (or (< (curr-bird 'bird::top) (- border-radius)) 
                  (< (curr-bird 'bird::left) (- border-radius)) 
                  (> (curr-bird 'bird::right) (+ screen-width border-radius))  
                  (> (curr-bird 'bird::bottom) (+ screen-height border-radius)))
            (begin (((engine 'engine::bird-layer) 'remove-drawable!) animation)
                     (prune-birds (cdr list)))
            (cons (car list) (prune-birds (cdr list)))))))
  
    (define (draw-update) 
        ;code to add duck upon clicking d
        (if (input 'input::key-down? #\b)
          (let* ((location (input 'input::mouse-location)) ; get mouse location (conscell)
                 (point (point2D::new (car location) (cdr location))) ; convert to point2D
                 (direction-vector (vec2D::random-unit-vector))) ; generate random vector (len 1)
            (direction-vector 'vec2D::*! bird-speed) 
            (engine 'engine::new-bird 'duck point direction-vector))) 

        ;shooting code
        ;click
        (if (input 'input::button-down? 'left)
          (let ((mouse-location (input 'input::mouse-location))
                (active-birds (engine 'engine::active-birds))
                (active-gun (vector-ref gun-switcher (modulo active-gun-index (vector-length gun-switcher)))))
            ((crosshair 'set-x!) (- (car mouse-location) (/ (crosshair 'get-w) 2)))
            ((crosshair 'set-y!) (- (cdr mouse-location) (/ (crosshair 'get-h) 2)))
            (((engine 'engine::ui-layer) 'add-drawable!) crosshair)
            (if (active-gun 'gun::shoot!)
              (if (not (engine 'engine::collision? tree (point2D::new (car mouse-location) (cdr mouse-location))))
                (if (active-gun 'gun::splash?) 
                  (do-splash-hits (engine 'engine::active-birds) (point2D::new (car mouse-location) (cdr mouse-location)) active-gun)
                  (do-hits (engine 'engine::active-birds) (point2D::new (car mouse-location) (cdr mouse-location)) active-gun))))))
        
        ;release
        (if (input 'input::button-up? 'left)
          (((engine 'engine::ui-layer) 'remove-drawable!) crosshair))

        (engine 'engine::render-birds)

        ;clear out of bounds birds
        ;TODO: make counter for falling birds if 0 then pruning is not needed
        (engine 'engine::active-birds! (prune-birds (engine 'engine::active-birds)))
        ;technically not a draw function but draw-update executes after logic-update
        (input 'input::reset!))
  
    (define (logic-update dt) 
        (let ((active-gun (vector-ref gun-switcher (modulo active-gun-index (vector-length gun-switcher)))))
          (engine 'engine::update-internal-time! dt)
          (engine 'engine::update-birds dt)
          (active-gun 'gun::update! (engine 'engine::internal-time))))
  
    (define (dispatch-game-mode message . args)
      (cond ((eq? message 'game-mode::init) (init))
            ((eq? message 'game-mode::get-draw-update) draw-update)
            ((eq? message 'game-mode::get-logic-update) logic-update)
            (else (error "game-mode ADT unkown message" message))))
    dispatch-game-mode))