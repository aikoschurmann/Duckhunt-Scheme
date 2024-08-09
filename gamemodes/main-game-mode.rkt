(define (main-game-mode::new engine)
  (let* ((input (engine 'engine::input))
         (round-manager (round-manager::new engine))
         (air-gun (gun::new 'air-gun 1 (* 20 scale) 750 -1 #f #f))
         (turbo-gun (gun::new 'turbo-gun 1 (* 50 scale) burst-cooldown -1 #f #t))
         (water-gun (gun::new 'water-gun 0 (* 50 scale) 1000 10 #t #f))
         (gun-switcher (vector air-gun turbo-gun water-gun))
         (gun-tiles (vector air-gun-tile turbo-gun-tile water-gun-tile))
         (active-gun-index 0)
         (score-tile (make-tile 100 100))
         (life-tile (make-tile 100 100))
         (level-tile (make-tile 100 100))
         (ammo-tile (make-tile 100 120))
         (display-tile (make-tile 200 50))
         (cooldown-tile (make-tile 100 100))
         (cooldown-tile-background (make-tile 100 100))
         (has-reset? #f))
    

    (define (main::game-mode::reset!)
      (set! air-gun (gun::new 'air-gun 1 (* 20 scale) 750 -1 #f #f))
      (set! turbo-gun (gun::new 'turbo-gun 1 (* 50 scale) burst-cooldown -1 #f #t))
      (set! water-gun (gun::new 'water-gun 0 (* 50 scale) 1000 10 #f #f))
      (set! gun-switcher (vector air-gun turbo-gun water-gun))
    )
    ;since scaling happens around the center you have to correct
    (define (set-tile! tile x y) 
      (let ((correction-x (* (/ (- scale 1) 2) (tile 'get-w)))
            (correction-y (* (/ (- scale 1) 2) (tile 'get-h)))) 
        ((tile 'set-x!) (+ x correction-x))
        ((tile 'set-y!) (+ y correction-y))))

    (define (add-tiles-horizontal! path mask layer n)
      (define (add-tile i)
        (if (< i n)
            (let* ((tile (make-bitmap-tile path mask))
                   (x (* i (tile 'get-w) ui-scale))
                   (y (- screen-height (* (tile 'get-h) ui-scale))))
              (((engine layer) 'add-drawable!) tile)
              ((tile 'set-scale!) ui-scale)
              (set-tile! tile x y)
              (add-tile (+ i 1)))))
      (add-tile 0))


    ; stay
    (define (draw-obstacles)
      (((engine 'engine::scenery-layer) 'empty!))
      (let ((obstacle-list (round-manager 'round-manager::get-obstacles)))
        (define (draw-obstacle list)
          (if (not (null? list))
              (let* ((current-obstacle (car list))
                     (tile (vector-ref current-obstacle 0))
                     (x (vector-ref current-obstacle 1))
                     (y (vector-ref current-obstacle 2)))

                
                (set-tile! tile x y)
                (((engine 'engine::scenery-layer) 'add-drawable!) tile)

                (draw-obstacle (cdr list)))))
        (draw-obstacle obstacle-list)))

    ;stay
    (define (do-damage current-bird active-gun)
      ; (current-bird 'bird::hasDied?) checks if bird died right now
      ; (current-bird 'bird::dead?) checks if bird was dead before
      ; Hence we only want to execute this code if the bird just died and not when it already was
      (let ((current-bird-adt (car current-bird))) 
        (current-bird-adt 'bird::decrease-health! (active-gun 'gun::damage))
        
        (engine 'engine::remove-bird-life-tiles! current-bird-adt)
        (current-bird-adt 'bird::life-list! (current-bird-adt 'bird::generate-life-list))
        (engine 'engine::add-bird-life-tiles! current-bird-adt)
       
        (current-bird-adt 'bird::vec! (vec2D::random-unit-vector))
        (if (and (not (current-bird-adt 'bird::hasDied?)) (current-bird-adt 'bird::dead?))
            ; makes the falling animation run 
            (begin (engine 'engine::next-duck-animation! current-bird)
                   (current-bird-adt 'bird::hasDied!)
                   (if (round-manager 'round-manager::double-score?)
                       (engine 'engine::add-score! (* (current-bird-adt 'bird::score) 2))
                       (engine 'engine::add-score! (current-bird-adt 'bird::score)))

                   ((score-tile 'clear!))
                   ((score-tile 'draw-text!) (number->string (engine 'engine::score)) 16 0 0 "black"))
            (if (eq? (active-gun 'gun::type) 'water-gun) 
                  (begin 
                    (if (not (current-bird-adt 'bird::slowed?))
                        (engine 'engine::add-drawable! (current-bird-adt 'bird::water-tile) (current-bird-adt 'bird::z-index)))
                    (current-bird-adt 'bird::slowed!))))))
    ;stay
    (define (do-splash-hits birds pos active-gun)
      (if (not (null? birds))
          (let* ((current-bird (car birds))
                 (current-bird-adt (car current-bird))
                 (bird-location (current-bird-adt 'bird::center))
                 (distance (bird-location 'point2D::distance pos)))
            (if (< distance (active-gun 'gun::range))
                (if (eq? (current-bird-adt 'bird::type) 'false)
                    (handle-enemy current-bird)
                    (do-damage current-bird active-gun)))
            (do-splash-hits (cdr birds) pos active-gun))))
    
    ; delete and decrease life when hitting false bird
    (define (handle-enemy current-bird)
      (let* ((current-bird-tile (cdr current-bird)))
        (((engine 'engine::bird-layer) 'remove-drawable!) current-bird-tile)
        (engine 'engine::remove-live!)
        ((life-tile 'clear!))
        ((life-tile 'draw-text!) (number->string (engine 'engine::lives)) 16 0 20 "black")))

    ; convert (list (tile x y) ...) -> (list tile ...) (move)
    (define (isolate-obstacle-objects obstacles)
      (if (null? obstacles)
          '()
          (cons (vector-ref (car obstacles) 0) (isolate-obstacle-objects (cdr obstacles)))))

    ; check if there was ANY collision given a pos and list of tiles
    (define (check-collisions tiles pos)
      (if (null? tiles)
          #f
          (if (engine 'engine::collision? (car tiles) pos)
              #t
              (check-collisions (cdr tiles) pos))))
    
    ; removes first collision and returns (collision . rest-of-list)
    (define (filter-collision lst pos)
      (define (filter-collision-iter remaining filtered found)
        (if (null? remaining)
            (cons found (reverse filtered))
            (let ((current (car remaining)))
              (if (engine 'engine::collision? (cdr current) pos)
                  (filter-collision-iter '() (append  (cdr remaining) filtered) current)
                  (filter-collision-iter (cdr remaining) (cons current filtered) found)))))
      (filter-collision-iter lst '() '()))


    ; handles shooting collisions with obstacles
    ; handles logic for hitting false enemy
    ; and hitting normal birds
    (define (do-hits birds pos active-gun)
      (if (not (null? birds))
          (let* ((current-bird (car birds))
                 (current-bird-adt (car current-bird))
                 (current-bird-tile (cdr current-bird))
                 (bird-location (current-bird-adt 'bird::center)))
            (if (and (engine 'engine::collision? current-bird-tile pos)
                     (or (= 2 (current-bird-adt 'bird::z-index))
                         (not (check-collisions (isolate-obstacle-objects (round-manager 'round-manager::get-obstacles)) pos))))
                (if (eq? (current-bird-adt 'bird::type) 'false)
                    (handle-enemy current-bird)
                    (do-damage current-bird active-gun))

                (do-hits (cdr birds) pos active-gun)))))

    ; removes offscreen birds
    (define (prune-birds list)
      (if (null? list)    
          '()
          (let* ((curr-bird (car (car list)))
                 (animation (cdr (car list)))
                 (layer (if (= (curr-bird 'bird::z-index) 1) 'engine::bird-layer 'engine::bird-layer2)))
            (if (or (< (curr-bird 'bird::top) (- border-radius)) 
                    (< (curr-bird 'bird::left) (- border-radius)) 
                    (> (curr-bird 'bird::right) (+ screen-width border-radius))  
                    (> (curr-bird 'bird::bottom) (+ screen-height border-radius)))
                (begin (((engine layer) 'remove-drawable!) animation)
                       (prune-birds (cdr list)))
                (cons (car list) (prune-birds (cdr list)))))))

    (define (update-ammo-display)
      (let* ((gun (vector-ref gun-switcher (modulo active-gun-index (vector-length gun-switcher))))
            (ammo (gun 'gun::ammo)))
      (if (>= ammo 0)
        (begin ((ammo-tile 'clear!)) ((ammo-tile 'draw-text!) (number->string ammo) 16 0 100 "black"))
        ((ammo-tile 'clear!)))))

    (define (handle-shooting mouse-point active-gun)
      (let* ((filter-res (filter-collision (engine 'engine::active-power-ups) mouse-point))
             (hit-power-up (car filter-res))
             (rest (cdr filter-res))
             (ammo (active-gun 'gun::ammo)))
             
        (if (active-gun 'gun::splash?) 
            (do-splash-hits (engine 'engine::active-birds) mouse-point active-gun)
            (do-hits (engine 'engine::active-birds) mouse-point active-gun))

        
        (update-ammo-display)
        
        (if (not (null? hit-power-up))
            (begin 
                   (if (eq? (car hit-power-up) 'focus) (for-each (lambda (gun) (gun 'gun::reset!)) (vector->list gun-switcher)))
                   (round-manager 'round-manager::powerup! (car hit-power-up))
                   (engine 'engine::active-power-ups! rest)
                   (((engine 'engine::ui-layer) 'remove-drawable!) (cdr hit-power-up))))))


    (define (handle-keypresses)

      (if (input 'input::key-down? 'left)
        (begin 
            (((engine 'engine::ui-layer) 'remove-drawable!) (vector-ref gun-tiles (modulo active-gun-index (vector-length gun-tiles))))
            (set! active-gun-index (- active-gun-index 1))
            (update-ammo-display)
            (((engine 'engine::ui-layer) 'add-drawable!) (vector-ref gun-tiles (modulo active-gun-index (vector-length gun-tiles))))

          ))
      (if (input 'input::key-down? 'right)
        (begin 
            (((engine 'engine::ui-layer) 'remove-drawable!) (vector-ref gun-tiles (modulo active-gun-index (vector-length gun-tiles))))
            (set! active-gun-index (+ active-gun-index 1))
            (update-ammo-display)
            (((engine 'engine::ui-layer) 'add-drawable!) (vector-ref gun-tiles (modulo active-gun-index (vector-length gun-tiles))))

          ))

      ;(if (input 'input::key-down? #\d)
      ;    (round-manager 'round-manager::generate-duck))
      ;
      ;(if (input 'input::key-down? #\s)
      ;    (round-manager 'round-manager::generate-seagull))
      ;
      ;(if (input 'input::key-down? #\b)
      ;    (round-manager 'round-manager::generate-buzzard))
;
      ;(if (input 'input::key-down? #\f)
      ;    (let* ((location (input 'input::mouse-location)) ; get mouse location (conscell)
      ;           (point (point2D::new (car location) (cdr location))) ; convert to point2D
      ;           (direction-vector (vec2D::random-unit-vector-up))
      ;           (bird (bird::new 'false 1 point direction-vector 10 100 1))) ; generate random vector (len 1)
;
      ;      (engine 'engine::new-bird 'bomb bird 2)))
      
      (if (input 'input::button-down? 'left)
          
          (let ((mouse-location (input 'input::mouse-location))
                (active-birds (engine 'engine::active-birds))
                (active-gun (vector-ref gun-switcher (modulo active-gun-index (vector-length gun-switcher)))))
            ((crosshair 'set-x!) (- (car mouse-location) (/ (crosshair 'get-w) 2)))
            ((crosshair 'set-y!) (- (cdr mouse-location) (/ (crosshair 'get-h) 2)))
            (((engine 'engine::ui-layer) 'add-drawable!) crosshair)
            ;if able to shoot
            (if (active-gun 'gun::shoot! (if (round-manager 'round-manager::focus?) 2 1))
                (handle-shooting (point2D::new (car mouse-location) (cdr mouse-location)) active-gun))))

      (if (input 'input::button-up? 'left)
          (((engine 'engine::ui-layer) 'remove-drawable!) crosshair)))

    (define (draw-cooldown-bar)
      (let ((active-gun (vector-ref gun-switcher (modulo active-gun-index (vector-length gun-switcher)))))
        ((cooldown-tile 'clear!))
        ((cooldown-tile 'draw-rectangle!) 0 40 (min (* (/ (active-gun 'gun::time-since-last-shot ) (active-gun 'gun::cooldown (if (round-manager 'round-manager::focus?) 2 1))) 100) 100) 20 "black")))

    (define (init)  
      ((crosshair 'set-scale!) ui-scale)
      (draw-obstacles)

      (add-tiles-horizontal! "sprites/grass/grass1.png" "sprites/grass/grass1mask.png" 'engine::scenery-layer 3)
      
      ((score-tile 'draw-text!) (number->string (engine 'engine::score)) 16 0 0 "black")
      (((engine 'engine::scenery-layer) 'add-drawable!) score-tile)

      ((life-tile 'draw-text!) (number->string (engine 'engine::lives)) 16 0 20 "black")
      (((engine 'engine::scenery-layer) 'add-drawable!) life-tile)

      ((cooldown-tile-background 'draw-rectangle!) 0 40 100 20 "gray")

      (((engine 'engine::scenery-layer) 'add-drawable!) cooldown-tile)
      (((engine 'engine::scenery-layer) 'add-drawable!) cooldown-tile-background)
      
      (((engine 'engine::ui-layer) 'add-drawable!) level-tile)
      (((engine 'engine::ui-layer) 'add-drawable!) ammo-tile)
      (((engine 'engine::ui-layer) 'add-drawable!) (vector-ref gun-tiles (modulo active-gun-index (vector-length gun-tiles))))
      (update-ammo-display)

      ((display-tile 'draw-text!) "press space to continue" 16 0 0 "black")
      (set-tile! display-tile (- (/ screen-width 2) (display-tile 'get-w)) (- (/ screen-height 2) (display-tile 'get-h)))

      

      (engine 'engine::set-background-colour! "lightblue"))

    (define (game-loop)
      ;(set! has-reset? #f)
      ; spawns next bird if enough time has passed
      (round-manager 'round-manager::get-next!)
      ; spawns next power-up if enough time has passed
      (round-manager 'round-manager::spawn-power-ups!)
      
      (engine 'engine::render-birds)
      ;clear out of bounds birds
      (engine 'engine::active-birds! (prune-birds (engine 'engine::active-birds)))
      
      (handle-keypresses)
      
      ;cooldown draw
      (draw-cooldown-bar))

    (define (draw-next-round-screen)
        (if (input 'input::key-down? #\space)
          (begin (set! has-reset? #f) 
                 (round-manager 'round-manager::next-level! #f) 
                 (((engine 'engine::scenery-layer) 'remove-drawable!) display-tile))))

    (define (reset-data!)
      (let ((level (round-manager 'round-manager::level)))
        (if (= level 1)
          (begin (set! stone1_pos (car level2_pos))
                 (set! stone2_pos (cdr level2_pos)))
          (begin (set! stone1_pos (car level3_pos))
                 (set! stone2_pos (cdr level3_pos))))

        (((engine 'engine::bird-layer) 'empty!))
        (((engine 'engine::bird-layer2) 'empty!))
        (((engine 'engine::stone-layer) 'empty!))
        (((engine 'engine::ui-layer) 'empty!))
        (engine 'engine::active-birds! '())
        (engine 'engine::active-power-ups! '())

        (init)

        (((engine 'engine::scenery-layer) 'add-drawable!) display-tile)

        
        (set! has-reset? #t)))

    (define (reset-data-complete!)
      (set! stone1_pos (car level3_pos))
      (set! stone2_pos (cdr level3_pos))
      (engine 'engine::reset!)
      (engine 'engine::lives! 3)
      (round-manager 'round-manager::reset!)
      (main::game-mode::reset!)
      ((score-tile 'clear!))
      ((life-tile 'clear!))
      (init)
      (set! has-reset? #t))

    (define (draw-play-again-screen)
       (if (input 'input::key-down? #\space)
          (reset-data-complete!)))

    (define (draw-update) 
      (if (and (not (<= (engine 'engine::lives) 0)) (< (round-manager 'round-manager::level) 3))
        (if (not (round-manager 'round-manager::next-level?))
          (game-loop)
        
          (if (not has-reset?)
            (reset-data!)
            (draw-next-round-screen)))
            
          (if (not has-reset?)
            (begin (reset-data!) ((display-tile 'clear!)) ((display-tile 'draw-text!) "press space to play again" 16 0 0 "black"))
            (draw-play-again-screen)))
      
      ;technically not a draw function but draw-update executes after logic-update
      (input 'input::reset!))
  
    (define (logic-update dt) 
      (let ((active-gun (vector-ref gun-switcher (modulo active-gun-index (vector-length gun-switcher)))))
        (engine 'engine::update-internal-time! dt)
        (engine 'engine::update-birds dt)
        (round-manager 'round-manager::update! dt)
        (active-gun 'gun::update! (engine 'engine::internal-time))))
  
    (define (dispatch-game-mode message . args)
      (cond ((eq? message 'game-mode::init) (init))
            ((eq? message 'game-mode::get-draw-update) draw-update)
            ((eq? message 'game-mode::get-logic-update) logic-update)
            (else (error "game-mode ADT unkown message" message))))
    dispatch-game-mode))