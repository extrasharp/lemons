#!/bin/csi -s

(import
  srfi-1
  chicken.file
  chicken.file.posix
  chicken.process
  chicken.process-context
  chicken.io
  chicken.irregex
  )

(define args (cdddr (argv)))
(when (or (null? args)
          (not (member (car args) '("next" "prev")))
          )
  (exit))
(define cmd (car args))

(define bg-dir "/home/mel/images/bgs")

(define files
  (list->vector
    (filter-map
      (lambda (f)
        (let (
          (f (string-append bg-dir "/" f))
          )
          (if (directory? f) #f f)
          ))
      (directory bg-dir))
    ))

(define last-feh (with-input-from-file "/home/mel/.fehbg" read-string))
(define last-desktop
  (irregex-match-substring
    (irregex-search
     '(: #\' ($ #\/ (+ any)) #\') last-feh)
    1)
  )

(define next
  (let loop (
    (n 0)
    )
    (cond
      ((>= n (vector-length files))
        0)
      ((string=? last-desktop (vector-ref files n))
        (modulo
          ((if (string=? cmd "next") + -) n 1)
          (vector-length files)))
      (else
        (loop (+ n 1)))
      ))
  )

(system
  (string-append
    "feh --bg-fill " (vector-ref files next)))
