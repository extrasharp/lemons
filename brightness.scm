#!/bin/csi -s

(use
  posix
  utils
  )

(define filepath "/sys/class/backlight/amdgpu_bl0/brightness")
(define bmin 10)
(define bmax 190)
(define jump-amt 25)

(define (clamp v vmin vmax)
  (max (min v vmax) vmin))

(define (setb to)
  (let (
    (f (file-open filepath open/wronly))
    (to (number->string (clamp to bmin bmax)))
    )
    (print to)
    (when f
      (file-write f to)
      (file-close f))
    ))

(let (
  (curr (with-input-from-file filepath read))
  (args (cdddr (argv)))
  )
  (if (null? args)
      (print curr)
      (cond
        ((string=? (car args) "+")
          (setb (+ curr jump-amt)))
        ((string=? (car args) "-")
          (setb (- curr jump-amt)))
        ((string->number (car args))
          (setb (string->number (car args))))
        (else
          (error "invalid args"))
        )))
