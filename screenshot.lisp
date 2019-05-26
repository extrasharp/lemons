#!/bin/sbcl --script

(when (or (/= (length *posix-argv*) 2)
          (not (member
                 (cadr *posix-argv*)
                 '("all" "part")
                 :test #'string-equal)))
  (exit))

(defvar filename
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "/home/mel/images/_screenshots/~d-~2,'0d-~2,'0d-~2,'0dh-~2,'0dm-~2,'0ds.png"
            year
            month
            day
            hour
            minute
            second)))

(let ((cmd (cadr *posix-argv*)))
  (cond
    ((string-equal cmd "all")
     (run-program "/bin/maim" `("-u" ,filename)))
    ((string-equal cmd "part")
     (run-program "/bin/maim" `("-u" "-s" "-c" "0,0.8,0.6,1" ,filename)))))
