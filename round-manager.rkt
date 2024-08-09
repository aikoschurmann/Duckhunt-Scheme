(define (round-manager::new engine)
  (let* ((round 0)
         (iteration 0)
         (level 0)
         (round-dt 0)
         (round-spawn-rate spawn-rate)
         (spawn? #f)
         (power-dt 0)
         (power-spawn-rate (random (* 1000 1) (* 1000 2)))
         (spawn-power? #f)
         (power-up-states (cons #f #f))
         (double-rest-duration 0)
         (focus-rest-duration 0)
         (next-level? #f))

    (define (round-manager::set-round! new-round) (set! round new-round))
    (define (round-manager::next-level! bool) (set! next-level? bool))
    (define (round-manager::set-iteration! new-iteration) (set! iteration new-iteration))
    (define (round-manager::set-level! new-level) (set! level new-level))

    (define (round-manager::update! dt)
      (set! round-dt (+ round-dt dt))

      (if (> round-dt (- round-spawn-rate (* level 500))) (set! spawn? #t))
      (set! round-dt (modulo round-dt (- round-spawn-rate (* level 500))))

      (set! power-dt (+ power-dt dt))
      
      (set! double-rest-duration (max (- double-rest-duration dt) 0))

      ;add to remove powerup-tile
      
      (if (= double-rest-duration 0)
          (begin (double! power-up-states #f)  (((engine 'engine::ui-layer) 'remove-drawable!) double-indicator)))

      
      (if (= focus-rest-duration 0)
          (begin (focus! power-up-states #f)  (((engine 'engine::ui-layer) 'remove-drawable!) focus-indicator)))
      
      (set! focus-rest-duration (max (- focus-rest-duration dt) 0))

      (if (> power-dt power-spawn-rate)
          (begin (set! spawn-power? #t)
                 (set! power-dt (modulo power-dt power-spawn-rate))
                 (set! power-spawn-rate (random (* 1000 30) (* 1000 45))))
          (set! power-dt (modulo power-dt power-spawn-rate))))



    

    (define (generate-duck)
      (let ((adt (bird::new 'duck 1 (generate-position-bottom) (vec2D::random-unit-vector-up) 5 100 (random 1 3))))
        (engine 'engine::new-bird 'duck adt 3)))

    (define (generate-buzzard)
      (let ((adt (bird::new 'buzzard 2 (generate-position-bottom) (vec2D::random-unit-vector-up) 10 100 2)))
        (engine 'engine::new-bird 'buzzard adt 3)))

    (define (generate-seagull)
      (let ((adt (bird::new 'seagull 1 (generate-position-side) (vec2D::new 1 0) 10 100 (random 1 3))))
        (engine 'engine::new-bird 'seagull adt 3)))

    (define (generate-crow)
      (let ((adt (bird::new 'crow 1 (generate-position-top) (vec2D::random-unit-vector) 5 100 1)))
        (engine 'engine::new-bird 'crow adt 3)))

    (define (generate-enemy)
      (let ((adt (bird::new 'false 1 (generate-position-bottom) (vec2D::random-unit-vector-up) 10 100 1)))
        (engine 'engine::new-bird 'bomb adt 2)))

    (define (type->bird type)
      (case type
        ((duck) (generate-duck))
        ((seagull) (generate-seagull))
        ((buzzard) (generate-buzzard))
        ((enemy) (generate-enemy))
        ((crow) (generate-crow))
        (else (error "Unknown bird type"))))

    (define (round-manager::get-next!)
      (if spawn? (spawn!)))

    (define (generate-bird-from-table probability-table)
      (let ((random-index (random 0 (vector-length probability-table))))
        (vector-ref probability-table random-index)))


    (define (round-manager::generate-bird)
      (cond ((<= round 2) (generate-bird-from-table probability-table-easy))
            ((<= round 4) (generate-bird-from-table probability-table-medium))
            (else (generate-bird-from-table probability-table-hard))))


    (define (spawn-helper n)
      (if (> n 0)
          (begin
            (type->bird (round-manager::generate-bird))
            (set! spawn? #f)
            (set! iteration (+ iteration 1))
            (if (> iteration 10)
                (begin
                  (set! iteration 1)
                  (set! round (+ round 1))))
            (if (> round 6)
                (begin
                  (set! iteration 0)
                  (set! round 0)
                  (set! level (+ level 1))
                  (set! next-level? #t)))
            (spawn-helper (- n 1)))))

    (define (spawn!)
      (let ((spawn-count (min (random 1 4) (+ level 1))))
        (spawn-helper spawn-count)))

    (define (spawn-power-ups)
      (let* ((type (if (= (random 0 2) 0) 'focus 'double))
             (powerup! (if (eq? type 'fucus) focus! double!)))
        ;你好吗？ 
        (engine 'engine::new-power-up type (generate-position-middle))
        (set! spawn-power? #f)))

    (define (spawn-power-ups!)
      (if spawn-power?
          (spawn-power-ups)))

    (define (round-manager::get-obstacles)
      (let ((obstacle-vector (vector (level1) (level2) (level3))))
        (vector-ref obstacle-vector (modulo level (vector-length obstacle-vector)))))

    (define (round-manager::focus?)
      (focus power-up-states))
    
    (define (round-manager::double-score?)
      (double power-up-states))


    (define (round-manager::powerup! type)
      (if (eq? type 'double)
          (begin (set! double-rest-duration double-duration)
                 (double! power-up-states #t)
                 (((engine 'engine::ui-layer) 'add-drawable!) double-indicator))
          (begin (set! focus-rest-duration focus-duration)
                 (focus! power-up-states #t)
                 (((engine 'engine::ui-layer) 'add-drawable!) focus-indicator))))

    (define (round-manager::reset!)
      (set! round 0)
      (set! iteration 0)
      (set! level 0)
      (set! round-dt 0)
      (set! spawn? #f)
      (set! power-dt 0)
      (set! spawn-power? #f)
      (set! power-up-states (cons #f #f))
      (set! double-rest-duration 0)
      (set! focus-rest-duration 0)
      (set! next-level? #f))


    (lambda (message . args)
      (apply (case message
               ((round-manager::spawn-power-ups!) spawn-power-ups!);documented
               ((round-manager::get-next!) round-manager::get-next!);documented
               ((round-manager::get-obstacles) round-manager::get-obstacles)
               ((round-manager::next-level!) round-manager::next-level!);documented
               ((round-manager::update!) round-manager::update!);documented (updates internal cooldowns)
               ((round-manager::double-score?) round-manager::double-score?) ; documented (chekcs if double score power up is active)
               ((round-manager::focus?) round-manager::focus?) ; documented (chekcs if focus power up is active)
               ((round-manager::powerup!) round-manager::powerup!);documented  used when hitting powerup box
               ((round-manager::reset!) round-manager::reset!) ;documented
               ((round-manager::level) (lambda () level));documented
               ((round-manager::next-level?) (lambda () next-level?));documented
               ;((round-manager::round!) round-manager::set-round!)
               ;((round-manager::iteration) (lambda () iteration))
               ;((round-manager::iteration!) round-manager::set-iteration!)
               ;((round-manager::round) (lambda () round))
               ;((round-manager::level!) round-manager::set-level!)
               ;((round-manager::generate-duck) generate-duck)
               ;((round-manager::generate-seagull) generate-seagull)
               ;((round-manager::generate-buzzard) generate-buzzard)

               (else (error "Unknown message")))
             args))))
