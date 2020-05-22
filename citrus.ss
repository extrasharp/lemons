#!/bin/guile
!#

(use-modules (ice-9 format))

(define (format-symbol format-str . args)
  (string->symbol (apply format #f format-str args)))

(define-syntax define-struct
  (lambda (x)
    (let* ((expr (syntax->datum x))
           (struct-name (cadr expr))
           (struct-fields (cddr expr))
           (record-fields (map cadr struct-fields))
           (record-type-define
             `(define ,struct-name
                (make-record-type ,(symbol->string struct-name) ',record-fields)))
           (constructor-define
             `(define ,(format-symbol "_make-~a" struct-name)
                (record-constructor ,struct-name)))
           (predicate-define
             `(define ,(format-symbol "~a?" struct-name)
                (record-predicate ,struct-name)))
           (field-defines
             (let loop ((lst struct-fields)
                        (acc '()))
               (if (null? lst)
                   (reverse acc)
                   (let* ((obj (car lst))
                          (is-mutable (eq? (car obj) 'mut))
                          (name (cadr obj))
                          (accessor `(define ,(format-symbol "~a-~a" struct-name name)
                                       (record-accessor ,struct-name ',name)))
                          (modifier `(define ,(format-symbol "~a-~a!" struct-name name)
                                       (record-modifier ,struct-name ',name))))
                     (loop
                       (cdr lst)
                       (if is-mutable
                           (cons* accessor modifier acc)
                           (cons accessor acc))))))))
      (datum->syntax x
        `(begin ,record-type-define
                ,constructor-define
                ,predicate-define
                ,@field-defines)))))

;

(use-modules (srfi srfi-19))

(define (wrap val lo hi)
  (let ((range (- hi lo))
        (adjusted (- val lo)))
    (+ lo (modulo adjusted range))))

;

(define-struct anim
  (immut frames)
  (mut at))

(define (make-anim frames)
  (_make-anim frames 0))

(define (anim-peek a)
  (vector-ref (anim-frames a)
              (anim-at a)))

(define (anim-advance! a ct)
  (let ((at (wrap (+ ct (anim-at a))
                  0
                  (vector-length (anim-frames a)))))
    (anim-at! a at)
    (anim-peek a)))

(define (anim-next! a)
  (anim-advance! a 1))

(define (anim-prev! a)
  (anim-advance! a -1))

;

(define +battery-dir+ "/sys/class/power_supply/BAT0/")

(define (get-battery-property which)
  (let ((res (call-with-input-file (string-append +battery-dir+ which) read)))
    (if (eof-object? res)
        #f
        res)))

(define (get-battery)
  (let ((capacity (or (get-battery-property "capacity") 0))
        (status (string-ref (symbol->string (get-battery-property "status")) 0)))
    (when (char=? status #\D)
      (cond
        ((< capacity 9) 0)
        ((< capacity 15) 0)))
    (format #f
            "~a~a"
            capacity
            (case status
              ((#\D)     " ")
              ((#\C #\F) "+")
              ((#\U)     "!")
              (else      "?")))))

(define (get-time)
  (let* ((dt (current-date))
         (hr (modulo (date-hour dt) 12)) 
         (hr (if (= hr 0) 12 hr)))
    (format #f "~2,'0d:~2,'0d" hr (date-minute dt))))

(define *wink*
  (make-anim
    #(" (oo"
      " (oo"
      "★(-o")))

(define (gen-bar)
  (let ((batt (get-battery))
        (tm (get-time)))
    (format #f
            "~a #[fg=colour233,bold]~5@a ~4@a"
            (anim-next! *wink*)
            tm
            batt)))

(let loop ()
  (call-with-output-file "/tmp/.citrus"
    (lambda (p)
      (display (gen-bar) p)
      (newline p)))
  (sleep 1)
  (loop))
