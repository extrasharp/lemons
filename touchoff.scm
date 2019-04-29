#!/bin/chicken-csi -s

(import
  chicken.process
  chicken.process-context
  chicken.format
  )

(define args (cdddr (argv)))
(when (or (null? args)
          (not (member (car args) '("on" "off")))
          )
  (exit))
(define arg (car args))

(let* (
  (num (call-with-input-pipe
         "xinput list --id-only 'Wacom Bamboo 2FG 6x8 Finger'"
         read))
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
