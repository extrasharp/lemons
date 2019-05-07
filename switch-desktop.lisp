#!/bin/sbcl --script

(when (or (/= (length *posix-argv*) 2)
          (not (member
                 (cadr *posix-argv*)
                 '("next" "prev")
                 :test #'string-equal)))
  (exit))

(defun read-file (pname)
  (format nil "~{~a~^~%~}"
    (with-open-file (stream pname)
      (loop for line = (read-line stream nil)
            while line
            collect line))))

(let* ((cmd (cadr *posix-argv*))
       (files
         (remove-if-not
           #'pathname-type
           (directory
             (make-pathname
               :name :wild
               :type :wild
               :defaults #p"/home/mel/images/bgs/"))))
       (file-names (map 'list #'pathname-name files))
       (last-file
         (let* ((feh-str (read-file #p"/home/mel/.fehbg"))
                (start (position #\' feh-str))
                (end (position #\' feh-str :from-end t)))
           (pathname (subseq feh-str (+ start 1) end))))
       (next-file
         (let* ((pos (position (pathname-name last-file) file-names
                               :test #'string=))
                (next-idx
                  (cond
                    ((not pos) 0)
                    ((string-equal cmd "next")
                     (mod (+ pos 1) (length file-names)))
                    ((string-equal cmd "prev")
                     (mod (- pos 1) (length file-names))))))
           (elt files next-idx))))
  (run-program "/bin/feh" `("--bg-fill" ,(namestring next-file))))
