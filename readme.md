exploration of OOP in scheme

Approaches Explored

1.Nested Functions Approach
In this approach, each object is represented as a closure containing instance variables and methods defined as nested functions. Methods directly manipulate the instance variables.

```scheme
(define (vec x y z)

    (define (x! new-val)
        (set! x new-value))

    (define (y! new-val)
        (set! y new-value))

    (define (z! new-val)
        (set! z new-value))
    
    (define (dispatch msg)
        (cond 
            ((eq? msg 'x) x)
            ((eq? msg 'y) z)
            ((eq? msg 'z) z)
            ((eq? msg 'z!) x!)
            ((eq? msg 'z!) y!)
            ((eq? msg 'z!) z!)))

    dispatch)

(define vec1 (vec 1 2 3))

((vec1 'x!) 7)

;this leads to redundant nesting
```
Strengths: Simple and straightforward organization of methods within an object.

Limitations: May lead to redundant nesting and verbose code.






2. Dot Notation Approach
This approach aims to elimanate nesting.

```scheme
(define (vec x y z)

    (define (x! args)
      (let ((new-val (car args)))
        (set! x new-value)))

    (define (y! args)
      (let ((new-val (car args)))
        (set! y new-value)))

    (define (z! args)
      (let ((new-val (car args)))
        (set! z new-value)))
        
    ;however this introcuded redundant unpacking of variables
    
    (define (dispatch msg . args)
        (cond 
            ((eq? msg 'x) x)
            ((eq? msg 'y) z)
            ((eq? msg 'z) z)
            ((eq? msg 'z!) (x! args))
            ((eq? msg 'z!) (y! args))
            ((eq? msg 'z!) (z! args))))

    dispatch)

(define vec1 (vec 1 2 3))

(vec1 'x! 7)```

Strengths: No more nesting in calls

Limitations: Redundant unpacking of arguments within called functions, leading to verbosity.






3. Apply Function Approach
Using the apply function, this approach automatically unpacks the arguments

```scheme
(define (vec x y z)

    (define (x! new-val)
        (set! x new-value))

    (define (y! new-val)
        (set! y new-value))

    (define (z! new-val)
        (set! z new-value))
    
    (define (dispatch msg)
        (apply (case 
                ((x) (lambda () x))
                ((y) (lambda () y))
                ((z) (lambda () z))
                ; Note variables should be wrapped in lambdas
                ((z!) x!)
                ((z!) y!)
                ((z!) z!)) args))

    dispatch)

; This has no notable shortcommings besides the elaborate syntax
(define vec1 (vec 1 2 3))

(vec1 'x! 7)```

Strengths: No nested calls, & no unpacking within functions

Limitations: Requires explicit wrapping of variables in lambdas, which can be cumbersome. & elaborate syntax






4. Syntax Rules Approach
In this approach, a macro (define-class) is defined using syntax rules to create a more concise & intuitive syntax for defining classes & methods. The macro generates code to create classes & methods, aiming for a cleaner & more readable syntax.


```scheme
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
                  ;(((symbol-append 'var '!)) (lambda (new-val) (set! var new-val)))
                  ;...
                  (else (lambda () (error "Unknown message")))) args))))))

(define-class (vector x y z)
  (x! (lambda (new-val) (set! x new-val)))
  (y! (lambda (new-val) (set! y new-val)))
  (z! (lambda (new-val) (set! z new-val))))

(define vec1 (vector))

(vec1 'x! 1)
(vec1 'y! 2)
(vec1 'z! 3)

(define (make-vec2d x y)
  (let ((vector (vector)))
    (vector 'x! x)
    (vector 'y! y)
    vector))
```

Strengths: Provides a clean & concise syntax resembling traditional class definitions in other languages.

Limitations: Difficulties in automating the generation of setters & defining initial values upon creation of instances.





Conclusion

This exploration demonstrates various ways to implement OOP concepts in Scheme & highlights potetntial strengths & weaknesses. I chose to not use the last version in the code base because it might be confusing & perhaps not apreciated