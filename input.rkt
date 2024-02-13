(define (input::new)

  (define (filter-list lst key)
      (cond ((null? lst) lst)
            ((eq? (car lst) key) (cdr lst))
            (else (cons (car lst) (filter-list (cdr lst) key)))))

  ;Keyboard logic
  (define key-down-list '())
  (define key-up-list '())
  (define key-pressed-list '())
  (define (input::key-down-list! args)
    (let ((list (car args)))
      (set! key-down-list list)))

  (define (input::key-up-list! args)
    (let ((list (car args)))
      (set! key-up-list list)))

  (define (input::key-pressed-list! args)
    (let ((list (car args)))
      (set! key-pressed-list list)))
  
  (define (input::key-pressed? args)
    (let ((key (car args)))
      (if (memq key key-pressed-list)
          #t 
          #f)))

  (define (input::key-down? args)
    (let ((key (car args)))
      (if (memq key key-down-list)
          #t 
          #f)))

  (define (input::key-up? args)
    (let ((key (car args)))
      (if (memq key key-up-list)
          #t 
          #f)))
  (define (input::key-update! state key)
    ;since realistically no more than 4 keys will be pressed down at the same time tail recursion is justified (filter-list)
    (if (eq? state 'pressed)
      ;pressed
      (begin (set! key-down-list (cons key key-down-list))
             (set! key-pressed-list (cons key key-pressed-list)))
      ;released
      (begin (set! key-up-list (cons key key-up-list))
             (set! key-pressed-list (filter-list key-pressed-list key)))))

  ;Mouse movement logic
  (define mouse-location '())
  (define (input::mouse-move-update! x y)
    (set! mouse-location (cons x y)))
  ;Mouse click logic
  (define button-down-list '())
  (define button-up-list '())
  (define button-pressed-list '())
  ;This code is nearly identical to key-update,
  ;but due to left (keybord arrow) and left (left mouse click) being the same,
  ;different lists are needed.
  ;(I could make the lists part of the function call but this is moving data around and thus slower)
  (define (input::mouse-click-update! btn state x y)
    (set! mouse-location (cons x y))
    (if (eq? state 'pressed)
      ;pressed
      (begin (set! button-down-list (cons btn button-down-list))
             (set! button-pressed-list (cons btn button-pressed-list)))
      ;released
      (begin (set! button-up-list (cons btn button-up-list))
             (set! button-pressed-list (filter-list button-pressed-list btn)))))

    (define (input::button-down-list! args)
    (let ((list (car args)))
      (set! button-down-list list)))

  (define (input::button-up-list! args)
    (let ((list (car args)))
      (set! button-up-list list)))

  (define (input::button-pressed-list! args)
    (let ((list (car args)))
      (set! button-pressed-list list)))
  
  (define (input::button-pressed? args)
    (let ((btn (car args)))
      (if (memq btn button-pressed-list)
          #t 
          #f)))

  (define (input::button-down? args)
    (let ((btn (car args)))
      (if (memq btn button-down-list)
          #t 
          #f)))

  (define (input::button-up? args)
    (let ((btn (car args)))
      (if (memq btn button-up-list)
          #t 
          #f)))

  (define (input::reset!)
    (set! key-down-list '())
    (set! key-up-list '())

    (set! button-down-list '())
    (set! button-up-list '()))

  (define (dispatch-input message . args)
    (cond ((eq? message 'input::get-key-callback) input::key-update!)
          ((eq? message 'input::key-down-list) key-down-list)
          ((eq? message 'input::key-up-list) key-down-list)
          ((eq? message 'input::key-pressed-list) key-pressed-list)
          ((eq? message 'input::key-down-list!) (input::key-down-list! args))
          ((eq? message 'input::key-up-list!) (input::key-up-list! args))
          ((eq? message 'input::key-pressed-list!) (input::key-pressed-list! args))
          ((eq? message 'input::key-pressed?) (input::key-pressed? args))
          ((eq? message 'input::key-down?) (input::key-down? args))
          ((eq? message 'input::key-up?) (input::key-up? args))

          ((eq? message 'input::get-mouse-move-callback) input::mouse-move-update!)
          ((eq? message 'input::mouse-location) mouse-location)
          ((eq? message 'input::get-mouse-click-callback) input::mouse-click-update!)

          ((eq? message 'input::button-down-list) button-down-list)
          ((eq? message 'input::button-up-list) button-down-list)
          ((eq? message 'input::button-pressed-list) button-pressed-list)
          ((eq? message 'input::button-down-list!) (input::button-down-list! args))
          ((eq? message 'input::button-up-list!) (input::button-up-list! args))
          ((eq? message 'input::button-pressed-list!) (input::button-pressed-list! args))
          ((eq? message 'input::button-pressed?) (input::button-pressed? args))
          ((eq? message 'input::button-down?) (input::button-down? args))
          ((eq? message 'input::button-up?) (input::button-up? args))

          ((eq? message 'input::reset!) (input::reset!))
          (else (error "input ADT unkown message" message))))
  dispatch-input)