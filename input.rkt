(define (input::new)


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
      (if (memq key key-pressed-list)
          #t 
          #f)))

  (define (input::key-up? args)
    (let ((key (car args)))
      (if (memq key key-pressed-list)
          #t 
          #f)))
  (define (input::key-update! state key)
    ;since realistically no more than 4 keys will be pressed down at the same time tail recursion is justified
    (define (filter-list lst key)
      (cond ((null? lst) lst)
            ((eq? (car lst) key) (cdr lst))
            (else (cons (car lst) (filter-list (cdr lst key))))))

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


  (define (input::reset!)
    (set! key-down-list '())
    (set! key-up-list '()))

  (define (dispatch-input message . args)
    (cond ((eq? message 'input::get-key-callback) input::key-update!)
          ((eq? message 'input::key-down-list) key-down-list)
          ((eq? message 'input::key-up-list) key-down-list)
          ((eq? message 'input::key-pressed-list) key-pressed-list)
          ((eq? message 'input::key-down-list!) (input::key-down-list! args))
          ((eq? message 'input::key-up-list!) (input::key-up-list! args))
          ((eq? message 'input::key-pressed-list!) (input::key-pressed-list! args))
          ((eq? message 'input::key-pressed?) (input::key-pressed?))
          ((eq? message 'input::key-down?) (input::key-down?))
          ((eq? message 'input::key-up?) (input::key-up?))

          ((eq? message 'input::get-mouse-move-callback) input::mouse-move-update!)
          ((eq? message 'input::mouse-location) mouse-location)
          
          ((eq? message 'input::reset!) (input::reset!))
          (else (error "input ADT unkown message" message))))
  dispatch-input)