#!/bin/csi -s

(import
  chicken.file.posix
  chicken.process-context
  )

(define filepath "/sys/class/backlight/amdgpu_bl0/brightness")
(define bmin 10)
(define bmax 190)
(define jump-amt 23)

(define (clamp v vmin vmax)
  (max (min v vmax) vmin))

(define (setb to)
  (let (
    (f (file-open filepath open/wronly))
    (to (number->string (clamp to 1 255)))
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
          (setb (clamp (+ curr jump-amt) bmin bmax)))
        ((string=? (car args) "-")
          (setb (clamp (- curr jump-amt) bmin bmax)))
        ((string=? (car args) "stay")
          (setb (- curr 1))
          (setb curr))
        ((string->number (car args))
          (setb (string->number (car args))))
        (else
          (error "invalid args"))
        )))
