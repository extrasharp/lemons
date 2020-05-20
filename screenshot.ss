#!/bin/guile
!#

(use-modules (srfi srfi-19)
             (ice-9 format))

; todo named screenshots
;        open dmenu, put name first but still add on date and time

(define* (gen-filename #:optional (basename #f))
  (let ((dt (current-date)))
    (format #f "/home/mel/images/_screenshots/~a~d-~2,'0d-~2,'0d-~2,'0dh-~2,'0dm-~2,'0ds.png"
            (if basename
                (string-append "_" basename "_")
                "")
            (date-year dt)
            (date-month dt)
            (date-day dt)
            (date-hour dt)
            (date-minute dt)
            (date-second dt))))

(let* ((cmds (cdr (command-line)))
       (cmd (car cmds)))
  (cond
    ((string=? cmd "all")
     (system* "/bin/maim" "-u" (gen-filename)))
    ((string=? cmd "part")
     (system* "/bin/maim" "-u" "-s" "-n"
                          "-c" "0,0.8,0.6,1"
                          (gen-filename)))))
