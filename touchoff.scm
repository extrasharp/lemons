#!/bin/csi -s

(define args (cdddr (argv)))
(when (or (null? args)
        (not (member (car args) '("on" "off"))))
  (exit))
(define arg (car args))

(use posix)

(receive (i o n)
    (process "xinput list --id-only 'Wacom Bamboo 2FG 6x8 Finger'")
  (let* (
    (num (read i))
    (val (if (eof-object? num)
             #f
             (number->string num)))
    )
    (when val
      (print (string-append "turning " arg))
      (system
        (sprintf "xinput set-prop ~A \"Device Enabled\" ~A"
          val (if (string=? arg "on") 1 0)))
      ))
  (close-input-port i)
  (close-output-port o)
  )
