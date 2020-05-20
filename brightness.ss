#!/bin/guile
!#

(define (clamp val vmin vmax)
  (max (min val vmax) vmin))

(define (to-zetone val vmin vmax)
  (let ((range (- vmax vmin))
        (adj-val (- val vmin)))
    (exact->inexact (/ adj-val range))))

(define (from-zetone val vmin vmax)
  (let ((range (- vmax vmin)))
    (inexact->exact (floor (+ vmin (* range val))))))

(define (zetone-step-expt zt step x)
  (expt (clamp (+ (expt zt (/ 1 x)) step) 0. 1.) x))

;

(define (string->int str)
  (let ((val (call-with-input-string str read)))
    (and (integer? val) val)))

;

(define _filepath "/sys/class/backlight/intel_backlight/brightness")
(define _min 10)
(define _max 450)
(define _step (/ 1 6))

(define (get-brightness)
  (call-with-input-file _filepath read))

(define (set-brightness int)
  (call-with-output-file _filepath
    (lambda (p)
      (display int p))))

(define (move-brightness step)
  (let* ((curr-val (get-brightness))
         (curr-zetone (to-zetone curr-val _min _max))
         (next-zetone (zetone-step-expt curr-zetone step 2))
         (next-val (from-zetone next-zetone _min _max)))
    (set-brightness next-val)))

(let ((cmds (cdr (command-line))))
  (if (null? cmds)
      (begin
        (display (get-brightness))
        (newline))
      (let ((str (car cmds)))
        (cond
          ((string->int str) => set-brightness)
          ((string=? str "++")
           (set-brightness _max))
          ((string=? str "--")
           (set-brightness _min))
          ((string=? str "+")
           (move-brightness _step))
          ((string=? str "-")
           (move-brightness (- _step)))
          (else
           (error "invalid argument" cmds))))))
