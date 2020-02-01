; Liam Wynn, 1/31/2020, Constraint System

; Taken from Structure and Interpretation of Computer Programs

; In most programs, your typical modus operandi is you take input,
; do relevant computation(s), and give output. This is great for
; programs where the inputs and outputs are pretty obvious. That is,
; at run time I give you input A always, and you give me output B always.

; So let's suppose I had a program that did the conversion of celsius to farenheit.
; Their relationship is represented as:

; 9C = 5(F - 32)

; Where C is the celsius temperature, and F is farenheit. How would you typically implement
; this program? You could create two procedures: one that converts a given C to an F, and
; vice-versa. For a trivial program like this, this may be fine. But what if we have more
; complicated relationships we want to model?

; An alternative approach is to make a constraint network. So if we were to set the value of
; F, we could propagate that to all the connections in the network. In doing so, we could
; figure out the corresponding C. See page 287 of Structure and Interpretation of Computer
; Programs for a richer explanation.

; Interface for connectors

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; Utility functions for connectors

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction!" (list value newval)))
            (else 'ignored)))

    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)

    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
  me))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))

          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))

          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (procedures-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))

  (define (process-new-value)
    (print-probe (get-value connector)))

  (define (process-forget-value)
    (print-proble "?"))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Unknown request -- PROBE" request))))

  (connect connector me)
me)
