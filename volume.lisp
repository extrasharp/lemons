#!/bin/sbcl --script

(when (or (/= (length *posix-argv*) 2)
          (not (member
                 (cadr *posix-argv*)
                 '("up" "down" "mute")
                 :test #'string-equal)))
  (exit))

(let ((cmd (cadr *posix-argv*)))
  (cond
    ((string-equal cmd "up")
     (run-program "/bin/amixer" `("-q" "sset" "Master" "2+")))
    ((string-equal cmd "down")
     (run-program "/bin/amixer" `("-q" "sset" "Master" "2-")))
    ((string-equal cmd "mute")
     (run-program "/bin/amixer" `("-D" "pulse" "-q" "sset" "Headphones" "unmute"))
     (run-program "/bin/amixer" `("-q" "sset" "Speaker" "unmute"))
     (run-program "/bin/amixer" `("-q" "sset" "Master" "toggle")))))
