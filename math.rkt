(define (math::decimal-round number decimals)
  (let ((decimal (expt 10 decimals)))
    (/ (round (* number decimal)) decimal)))

(define (make-list n val) (if (<= n 0) '() (cons val (make-list (- n 1) val))))

;code was taken from user448810 on github
;      (random) does not work ???
;(define random
;  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
;    (lambda new-seed
;      (if (pair? new-seed)
;          (set! seed (car new-seed))
;          (set! seed (modulo (+ (* seed a) c) m)))
;      (/ seed m))))
;
;code was taken from user448810 on github
;(define (randint . args)
;  (cond ((= (length args) 1)
;          (floor (* (random) (car args))))
;        ((= (length args) 2)
;          (+ (car args) (floor (* (random) (- (cadr args) (car args))))))
;        (else (error 'randint "usage: (randint [lo] hi)"))))