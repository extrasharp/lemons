#!/bin/sbcl --script

; todo check inputs

(defconstant +brightness-file+ "/sys/class/backlight/amdgpu_bl0/brightness")
(defconstant +min-brightness+ 1)
(defconstant +max-brightness+ 190)
(defconstant +brightness-jump+ 23)

(defun clamp (val valmin valmax)
  (max (min val valmax) valmin))

(defun set-brightness (to)
  (let ((to (clamp to 1 255)))
    (with-open-file (stream +brightness-file+
                            :direction :output
                            :if-exists :overwrite)
      (princ to stream))))

(let ((current-brightness
        (parse-integer
          (with-open-file (stream +brightness-file+)
            (read-line stream nil)))))
  (if (= (length *posix-argv*) 1)
      (progn
        (princ current-brightness)
        (terpri))
      (let ((command (cadr *posix-argv*)))
        (cond
          ((parse-integer command :junk-allowed t)
           (set-brightness (parse-integer command)))
          ((string= command "+")
           (set-brightness (clamp (+ current-brightness +brightness-jump+)
                                  +min-brightness+
                                  +max-brightness+)))
          ((string= command "-")
           (set-brightness (clamp (- current-brightness +brightness-jump+)
                                  +min-brightness+
                                  +max-brightness+)))
          ((string= command "stay")
           (set-brightness (+ current-brightness 1))
           (set-brightness current-brightness))
          (t
            (error "invalid args"))))))
