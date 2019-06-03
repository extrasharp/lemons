#!/bin/sbcl --script

(when (or (/= (length *posix-argv*) 2)
          (not (member
                 (cadr *posix-argv*)
                 '("on" "off")
                 :test #'string-equal)))
  (exit))

(let* ((cmd (cadr *posix-argv*))
       (process
         (run-program "/bin/xinput"
                      '("list" "--id-only" "Wacom Bamboo 2FG 6x8 Finger")
                      :output :stream))
       (id (read-line (process-output process))))
  (if (/= 0 (process-exit-code process))
      (error "could not get id")
      (progn
        (format t "turning ~a~%" cmd)
        (run-program "/bin/xinput"
                     `("set-prop" ,id "Device Enabled" ,(if (string= cmd "on") "1" "0"))))))
