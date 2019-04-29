#!/bin/sbcl --script

(defstruct anim
  frames
  at)

(defun anim-peek (a)
  (elt (anim-frames a) (anim-at a)))

(defun anim-next (a)
  (setf (anim-at a) (+ (anim-at a) 1))
  (when (>= (anim-at a) (length (anim-frames a)))
    (setf (anim-at a) 0))
  (anim-peek a))

(defun anim-prev (a)
  (setf (anim-at a) (- (anim-at a) 1))
  (when (< (anim-at a) 0)
    (setf (anim-at a) (length (anim-frames a))))
  (anim-peek a))

;

(defun get-battery ()
  (let* ((capacity
           (or (parse-integer
                 (with-open-file (stream "/sys/class/power_supply/BAT1/capacity")
                   (read-line stream nil)))
               0))
         (status
           (elt
             (with-open-file (stream "/sys/class/power_supply/BAT1/status")
               (read-line stream nil))
             0)))
    (when (char= status #\D)
      (cond
        ((< capacity 9)
          (run-program "/bin/notify-send" '("-u" "critical" "critcal battery")))
        ((< capacity 15)
          (run-program "/bin/notify-send" '("low battery")))))
    (format nil "~a~a"
            capacity
            (case status
              ((#\D)     " ")
              ((#\C #\F) "+")
              ((#\U)     "!")
              (otherwise "?")))))

(defun get-time ()
  (multiple-value-bind (sc mn hr) (get-decoded-time)
    (let* ((hr (mod hr 12))
           (hr (if (= hr 0) 12 hr)))
      (format nil "~2,'0d:~2,'0d" hr mn))))

;

(defvar *wink*
  (make-anim
    :frames '(" (oo"
              " (oo"
              "★(-o")
    :at 0))

(defvar *bar-cycle* 0)

(defun gen-bar ()
  (let ((batt (get-battery))
        (tm (get-time)))
    (setf *bar-cycle* (+ *bar-cycle* 1))
    (format nil "~a < ~5@a"
            (anim-next *wink*)
            (if (evenp *bar-cycle*)
              batt
              tm))))

(loop
  (print (gen-bar))
  (sleep 1))