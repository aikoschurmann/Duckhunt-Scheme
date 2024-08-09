(define (gun::new type damage range cooldown ammo splash burst)
  (define last-total-dt 0)
  (define last-shot-dt 0)
  (define burst-shots-left 0)
  (define burst-last-shot-dt 0)


  ;no cooldown on instantiation

  (define (gun::reset!)
    (set! last-shot-dt (- cooldown)))

  (gun::reset!)

  (define (gun::update! total-dt)
    ;total-dt : amount of ms passed since launch game
    ;last-total-dt : amount of ms passed since launch game at the time of last update
    (set! last-total-dt total-dt))
  
  (define (gun::ammo! args)
    (let ((new-ammo (car args)))
      (set! ammo new-ammo)))

  (define (burst-shoot! boost)
    (cond ((not (can-shoot? cooldown last-shot-dt boost)) #f)
          ((> burst-shots-left 0)
           (if (can-shoot? burst-delay burst-last-shot-dt boost)
               (begin
                 (set! burst-last-shot-dt last-total-dt)
                 (set! burst-shots-left (- burst-shots-left 1))
                 (set! ammo (- ammo 1))
                 (if (= burst-shots-left 0)
                   (set! last-shot-dt last-total-dt))
                 #t)
               #f))
          (else
           (begin
             (set! burst-shots-left burst-shots)
             (set! burst-last-shot-dt last-total-dt)
             #t))))

  (define (can-shoot? cooldown last-shot-time boost)
    (and (> (- last-total-dt last-shot-time) (/ cooldown boost)) (not (= ammo 0))))

  (define (single-shoot! boost)
    (if (can-shoot? cooldown last-shot-dt boost)
        (begin
          (set! last-shot-dt last-total-dt)
          (set! ammo (- ammo 1))
          #t)
        #f))

  (define (gun::shoot! boost)
      (if burst
        (burst-shoot! boost)
        (single-shoot! boost)))

        

  (define (gun::time-since-last-shot)
    (if burst
        (if (> burst-shots-left 0)
            (- last-total-dt burst-last-shot-dt)
            (- last-total-dt last-shot-dt))
        (- last-total-dt last-shot-dt)))

  (define (gun::cooldown boost)
    (if burst
        (if (> burst-shots-left 0)
            burst-delay
            (/ cooldown boost))
        (/ cooldown boost)))

  (lambda (message . args)
    (apply (case message
              ((gun::type) (lambda () type))
              ((gun::damage) (lambda () damage))
              ((gun::cooldown) gun::cooldown)
              ((gun::range) (lambda ()range))
              ((gun::ammo) (lambda () ammo))
              ((gun::ammo!) gun::ammo!)
              ((gun::shoot!) gun::shoot!)
              ((gun::splash?) (lambda () splash))
              ((gun::reset!) gun::reset!)
              ((gun::update!) gun::update!)
              ((gun::time-since-last-shot) gun::time-since-last-shot)
              (else (error "gun ADT unknown message" message))) args)))