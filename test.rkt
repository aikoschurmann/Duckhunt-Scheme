(define (decimal-round number decimals)
  (let ((decimal (expt 10 decimals)))
    (/ (round (* number decimal)) decimal)))

(define (var-manager::new . bindings)
  (let ((global-env '()))

    (define (merge-bindings env bindings)
      (if (null? bindings)
          (set! global-env env)
          (merge-bindings (cons (car bindings) env) (cdr bindings))))

    (merge-bindings global-env bindings)

    (define (string-suffix? suffix str)
      (let ((suffix-length (string-length suffix))
            (str-length (string-length str)))
        (and (<= suffix-length str-length)
             (string=? (substring str (- str-length suffix-length) str-length) suffix))))

    (define (lookup-var var)
      (let ((binding (assoc var global-env)))
        (if binding
            (cdr binding)
            #f)))

    (define (var-exists? var)
      (not (not (assoc var global-env))))

    (define (redefine-var! var new-val)
      (if (var-exists? var)
          (let ((binding (assoc var global-env)))
            (set-cdr! binding new-val))
          #f))

    (define (new-var var new-val)
      (if (not (var-exists? var))
          (merge-bindings global-env (list (cons var new-val)))
          (redefine-var! var new-val)))

    (define (remove condition list)
      (cond ((null? list) '())
            ((condition (car list)) (remove condition (cdr list)))
            (else (cons (car list) (remove condition (cdr list))))))

    (define (delete-var var)
      (if (var-exists? var)
          (set! global-env (remove (lambda (binding) (eq? (car binding) var)) global-env))
          #f))

    (define (list-vars)
      (map car global-env))

    (lambda (message . args)
      (apply (case message
               ((define) new-var)
               ((get) lookup-var)
               ((set!) redefine-var!)
               ((delete) delete-var)
               ((list-vars) list-vars)
               ((exists?) var-exists?)
               ((env) (lambda () global-env))
               (else (lambda () (error "Unknown message")))) args))))

(define manager (var-manager::new (cons 'x 5) (cons 'y 10)))

;; Look up the value of variable 'x'
(manager 'get 'x) ; Returns 5

;; Look up the value of an undefined variable 'z'
(manager 'get 'z) ; Returns #f

;; Check if variable 'z' exists
(manager 'exists? 'z) ; Returns #f

;; Define a new variable 'z'
(manager 'define 'z 15)

;; Look up the value of variable 'z'
(manager 'get 'z) ; Returns 15

;; List all variables
(manager 'list-vars) ; Returns (z y x)

;; Redefine the value of variable 'x'
(manager 'set! 'x 20)

;; Delete the variable 'y'
(manager 'delete 'y)

;; List all variables after deletion
(manager 'list-vars) ; Returns (z x)

;; Define a new variable 'w'
(manager 'define 'w 25)

;; Look up the current environment
(manager 'env) ; Returns ((w . 25) (z . 15) (x . 20))


(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...)))))

(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))

(define-syntax for
  (syntax-rules ()
    ((_ (i from to) b1 ...)
     (let loop((i from))
       (when (< i to)
	  b1 ...
	  (loop (+ i 1)))))))



(define (symbol-append sym1 sym2)
  (string->symbol (string-append (symbol->string sym1) (symbol->string sym2))))



(define-syntax define-class
  (syntax-rules ()
    ((_ (class-name var ...)
        (proc-name proc-lambda)... )
     
     (define (class-name)
       
         (define var 0)...
         (define proc-name proc-lambda)...

         (lambda (message . args)
          (apply (case message
                  ((vars) (lambda () vars))
                  ((proc-name) proc-lambda)
                  ...
                  ((var) (lambda () var))
                  ...
                  ;(((symbol-append 'var '!)) (lambda (new-val) (set! var new-val)))
                  ;...
                  (else (lambda () (error "Unknown message")))) args))))))


(define-syntax define-class
  (syntax-rules ()
    ((_ (class-name var ...)
        (proc-name proc-lambda)... )
     
     (define (class-name)
       
         (define var 0)...
         (define proc-name proc-lambda)...

         (lambda (message . args)
          (apply (case message
                  
                  ((proc-name) proc-lambda)
                  ...
                  ((var) (lambda () var))
                  ...
                  ((var!) (lambda (new-val) (set! var new-val)))
                  ...
                  ;doesn't generate x! y! z! but instead var! 3 times
                  (else (lambda () (error "Unknown message")))) args))))))


(define-class (vector x y z)
  (x! (lambda (new-val) (set! x new-val)))
  (y! (lambda (new-val) (set! y new-val)))
  (z! (lambda (new-val) (set! z new-val))))   

;hence needs to be defined here...  

(define-class (vector x y z)
  (x! (lambda (new-val) (set! x new-val)))
  (y! (lambda (new-val) (set! y new-val)))
  (z! (lambda (new-val) (set! z new-val)))

  ; doesn't work

  ; (((symbol-append 'var '!)) (lambda (new-val) (set! var new-val)))
  ; ...
  ; ((var!) (lambda (new-val) (set! var new-val)))
  ; ...

  (flip-x! (lambda () (set! x (- x))))
  (flip-y! (lambda () (set! y (- y))))
  (flip-z! (lambda () (set! z (- z))))

  (equal? (lambda (other-vec) (and (= x (other-vec 'x))
                                   (= y (other-vec 'y))
                                   (= z (other-vec 'z)))))

  (add (lambda (other-vec) (set! x (+ x (other-vec 'x)))
                           (set! y (+ y (other-vec 'y)))
                           (set! z (+ z (other-vec 'z)))))
  
  (substract (lambda (other-vec) (set! x (- x (other-vec 'x)))
                                 (set! y (- y (other-vec 'y)))
                                 (set! z (- z (other-vec 'z)))))
  
  (scale (lambda (const) (set! x (* x const))
                         (set! y (* y const))
                         (set! z (* z const))))

  (print (lambda () (display "x: ") (display x) (display " y: ") (display y) (display " z: ") (display z) (newline)))

  (magnitude (lambda () (sqrt (+ (* x x) (* y y) (* z z)))))

  (distance (lambda (other-vec) (sqrt (+ (expt (- x (other-vec 'x)) 2)
                                         (expt (- y (other-vec 'y)) 2)
                                         (expt (- z (other-vec 'z)) 2)))))

  (normalize! (lambda () (let ((mag (magnitude)))
                            (set! x (/ x mag))
                            (set! y (/ y mag))
                            (set! z (/ z mag)))))

  (is-normal? (lambda () (= (decimal-round (magnitude) 4) 1))))

;; Create two vectors
(define vec1 (vector))
(define vec2 (vector))

;; Test setter methods
(vec1 'x! 6)
(vec1 'y! 7)
(vec1 'z! 8)

(vec1 'x! -6)
(vec1 'y! -7)
(vec1 'z! -8)

;; Test flip methods
(vec1 'flip-x!)
(vec1 'flip-y!)
(vec1 'flip-z!)

;; Test comparison method
(vec1 'equal? vec2) 

;; Test arithmetic methods
(vec1 'add vec2)
(vec1 'substract vec2)
(vec1 'scale 2)



;; Test utility methods
(vec1 'print) 
(vec1 'magnitude)
(vec1 'distance vec2)
(vec1 'normalize!)
(vec1 'print)
(vec1 'is-normal?)
(vec1 'magnitude)