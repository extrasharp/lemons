#!/bin/guile
!#

(use-modules (ice-9 textual-ports)
             (ice-9 popen))

(define (get-device-id device-name)
  (let* ((pipe
           (open-input-pipe (string-append "/bin/xinput list --id-only \""
                                           device-name
                                           "\"")))
         (id (get-line pipe)))
    (if (= 0 (status:exit-val (close-pipe pipe)))
        id
        #f)))

(define (device-set-enabled id do-enable)
  (system* "/bin/xinput"
           "set-prop" id
           "Device Enabled" (if do-enable "1" "0")))

(let* ((cmds (cdr (command-line)))
       (cmd (car cmds))
       (id (get-device-id "Wacom Bamboo 2FG 6x8 Finger touch")))
  (cond
    ((not id)
     (error "could not get id"))
    ((string=? cmd "on")
     (display "turning on")
     (newline)
     (device-set-enabled id #t))
    ((string=? cmd "off")
     (display "turning off")
     (newline)
     (device-set-enabled id #f))))
