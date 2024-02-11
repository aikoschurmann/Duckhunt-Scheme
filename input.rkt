(define (input::new engine)
  (define (input::update state key)
    ;resetting of lists
    (engine 'engine::key-down-list! '())
    (engine 'engine::key-up-list! '())

    ;since realistically no more than 4 keys will be pressed down at the same time tail recursion is justified
    (define (filter-list lst key)
      (cond ((null? lst) lst)
            ((eq? (car lst) key) (cdr lst))
            (else (cons (car lst) (filter-list (cdr lst key))))))

    (if (eq? state 'pressed)
        ;pressed
        (let ((current-key-down-list (engine 'engine::key-down-list))
              (current-key-pressed-list (engine 'engine::key-pressed-list)))
           (engine 'engine::key-down-list! (cons key current-key-down-list))
           (engine 'engine::key-pressed-list! (cons key current-key-pressed-list)))

        ;released
        (let ((current-key-up-list (engine 'engine::key-up-list))
              (current-key-pressed-list (engine 'engine::key-pressed-list)))
           (engine 'engine::key-up-list! (cons key current-key-up-list))
           (engine 'engine::key-pressed-list! (filter-list current-key-pressed-list key)))))

  (define (dispatch-input message . args)
    (cond ((eq? message 'input::get-input-update) input::update)
          (else (error "input ADT unkown message" message))))
  dispatch-input)