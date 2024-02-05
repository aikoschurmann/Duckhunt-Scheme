(define (gun::new type damage cooldown ammo)
  (define last-total-dt 0)
  (define last-shot-dt 0)

  (define (gun::update args)
    ;total-dt : amount of ms passed since launch game
    ;last-total-dt : amount of ms passed since launch game at the time of last update
    (let* ((total-dt (car args))
           (dt (- total-dt last-total-dt)))
      (set! last-total-dt total-dt)))
  
  (define (gun::ammo! args)
    (let ((new-ammo (car args)))
      (set! ammo new-ammo)))

  (define (gun::shoot!)
    (if (> (- last-total-dt last-shot-dt) cooldown)
       (begin (set! last-shot-dt last-total-dt) #t)
        #f))

  (define (dispatch-gun message . args)
    (cond ((eq? message 'gun::type) type)
          ((eq? message 'gun::damage) damage)
          ((eq? message 'gun::cooldown) cooldown)
          ((eq? message 'gun::ammo) ammo)
          ((eq? message 'gun::ammo!) (gun::ammo! args))
          ((eq? message 'gun::shoot!) (gun::shoot!))
          (else (error "gun ADT unkown message" message))))
  dispatch-gun)