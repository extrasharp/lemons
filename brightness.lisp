#!/bin/sbcl --script

; todo check inputs

(defun clamp (val valmin valmax)
  (max (min val valmax) valmin))

(defun to-frange (val bottom top)
  (let ((range (- top bottom))
        (adj-val (- val bottom)))
    (float (/ adj-val range))))

(defun from-frange (fr bottom top)
  (let ((range (- top bottom)))
    (floor (+ bottom (* range fr)))))

;

(defconstant +brightness-file+ "/sys/class/backlight/intel_backlight/brightness")
(defconstant +min-brightness+ 10)
(defconstant +max-brightness+ 450)
(defconstant +brightness-ratio+ (/ 1 6))

(defun set-brightness (to)
  ; (let ((to (clamp to +min-brightness+ +max-brightness+)))
    (with-open-file (stream +brightness-file+
                            :direction :output
                            :if-exists :overwrite)
      (princ to stream)))

(defun move-brightness (current-brightness fl)
  (let* ((curr-frange (to-frange current-brightness
                                +min-brightness+
                                +max-brightness+))
         (next-frange (expt (clamp (+ fl (expt curr-frange 1/2)) 0. 1.) 2))
         (next-brightness (from-frange next-frange
                                       +min-brightness+
                                       +max-brightness+)))
    (set-brightness next-brightness)))

(let ((current-brightness
        (with-open-file (stream +brightness-file+)
          (read stream nil))))
  (if (= (length *posix-argv*) 1)
      (progn
        (princ current-brightness)
        (terpri))
      (let ((command (cadr *posix-argv*)))
        (cond
          ((parse-integer command :junk-allowed t)
           (set-brightness (parse-integer command)))
          ((string= command "++")
           (set-brightness +max-brightness+))
          ((string= command "--")
           (set-brightness +min-brightness+))
          ((string= command "+")
           (move-brightness current-brightness +brightness-ratio+))
          ((string= command "-")
           (move-brightness current-brightness (- +brightness-ratio+)))
          ((string= command "stay")
           (set-brightness (+ current-brightness 1))
           (set-brightness current-brightness))
          (t
            (error "invalid args"))))))
