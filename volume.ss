#!/bin/guile
!#

(let* ((cmds (cdr (command-line)))
       (cmd (car cmds)))
  (cond
    ((string=? cmd "up")
     (system* "/bin/amixer" "-q" "sset" "Master" "2+"))
    ((string=? cmd "down")
     (system* "/bin/amixer" "-q" "sset" "Master" "2-"))
    ((string=? cmd "mute")
     ; (system* "/bin/amixer" "-D" "pulse" "-q" "sset" "Headphones" "unmute")
     ; (system* "/bin/amixer" "-q" "sset" "Headphones" "unmute")
     ; (system* "/bin/amixer" "-q" "sset" "Speaker" "unmute")
     (system* "/bin/amixer" "-q" "sset" "Master" "toggle"))))
