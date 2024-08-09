(define (input::new)
  (define (filter-list lst key)
    (cond ((null? lst) lst)
          ((eq? (car lst) key) (cdr lst))
          (else (cons (car lst) (filter-list (cdr lst) key)))))

  (define key-down-list '())
  (define key-up-list '())
  (define key-pressed-list '())

  (define (input::key-down-list! list)
      (set! key-down-list list))

  (define (input::key-up-list! list)
      (set! key-up-list list))

  (define (input::key-pressed-list! list)
      (set! key-pressed-list list))

  (define (input::key-pressed? key)
      (memq key key-pressed-list))

  (define (input::key-down? key)
      (memq key key-down-list))

  (define (input::key-up? key)
      (memq key key-up-list))

  (define (input::key-update! state key)
    ;; Update key lists based on the state
    (if (eq? state 'pressed)
        (begin
          (set! key-down-list (cons key key-down-list))
          (set! key-pressed-list (cons key key-pressed-list)))
        (begin
          (set! key-up-list (cons key key-up-list))
          (set! key-pressed-list (filter-list key-pressed-list key)))))

  ;; Mouse movement logic
  (define mouse-location '())
  (define (input::mouse-move-update! x y)
    (set! mouse-location (cons x y)))

  ;; Mouse click logic
  (define button-down-list '())
  (define button-up-list '())
  (define button-pressed-list '())

  (define (input::mouse-click-update! btn state x y)
    (set! mouse-location (cons x y))
    (if (eq? state 'pressed)
        (begin
          (set! button-down-list (cons btn button-down-list))
          (set! button-pressed-list (cons btn button-pressed-list)))
        (begin
          (set! button-up-list (cons btn button-up-list))
          (set! button-pressed-list (filter-list button-pressed-list btn)))))

  (define (input::button-down-list! list)
      (set! button-down-list list))

  (define (input::button-up-list! list)
      (set! button-up-list list))

  (define (input::button-pressed-list! list)
      (set! button-pressed-list list))

  (define (input::button-pressed? btn)
      (memq btn button-pressed-list))

  (define (input::button-down? btn)
      (memq btn button-down-list))

  (define (input::button-up? btn)
      (memq btn button-up-list))

  (define (input::reset!)
    (set! key-down-list '())
    (set! key-up-list '())
    (set! button-down-list '())
    (set! button-up-list '()))

  ;; Dispatch function using case
  (define (dispatch-input message . args)
    (apply (case message
             ((input::get-key-callback) (lambda () input::key-update!))
             ;((input::key-down-list) (lambda () key-down-list))
             ;((input::key-up-list) (lambda () key-up-list))
             ;((input::key-pressed-list) (lambda () key-pressed-list))
             ;((input::key-down-list!) input::key-down-list!)
             ;((input::key-up-list!) input::key-up-list!)
             ;((input::key-pressed-list!) input::key-pressed-list!)

             ((input::key-pressed?) input::key-pressed?)
             ((input::key-down?) input::key-down?)
             ((input::key-up?) input::key-up?)

             ((input::get-mouse-move-callback) (lambda () input::mouse-move-update!))
             ((input::mouse-location) (lambda () mouse-location))
             ((input::get-mouse-click-callback) (lambda () input::mouse-click-update!))

             ;((input::button-down-list) (lambda () button-down-list))
             ;((input::button-up-list) (lambda () button-up-list))
             ;((input::button-pressed-list) (lambda () button-pressed-list))
             ;((input::button-down-list!) input::button-down-list!)
             ;((input::button-up-list!) input::button-up-list!)
             ;((input::button-pressed-list!) input::button-pressed-list!)

             ((input::button-pressed?) input::button-pressed?)
             ((input::button-down?) input::button-down?)
             ((input::button-up?) input::button-up?)
             ((input::reset!) input::reset!)
             (else (lambda () (error "input ADT unknown message" message)))) args))

  dispatch-input)


