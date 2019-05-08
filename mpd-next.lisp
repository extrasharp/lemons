#!/bin/sbcl --script

(require 'sb-bsd-sockets)
(use-package 'sb-bsd-sockets)

(when (or (/= (length *posix-argv*) 2)
          (not (member
                 (cadr *posix-argv*)
                 '("next" "toggle" "prev")
                 :test #'string-equal)))
  (exit))

(defun init-mpd-connection (socket)
 ;(socket-connect socket #(127 0 0 1) 6600)
  (socket-connect socket (posix-getenv "MPD_HOST"))
  (let ((mpd-stream
          (socket-make-stream socket
                              :input t
                              :output t
                              :buffering :line)))
    (read-line mpd-stream)
    mpd-stream))

(defun get-status (mpd-stream)
  (let ((results
          (progn
            (write-line "status" mpd-stream)
            (loop for line = (read-line mpd-stream nil)
                  while (string/= line "OK")
                  collect line)))
        (ht (make-hash-table :test #'equal)))
    (loop for result in results
          do (let* ((cut (position #\: result))
                    (key (subseq result 0 cut))
                    (val (subseq result (+ cut 2))))
               (setf (gethash key ht) val)))
    ht))

(let* ((cmd (cadr *posix-argv*))
      ;(mpd-socket
      ;  (make-instance 'inet-socket
      ;                 :type :stream
      ;                 :protocol :tcp))
       (mpd-socket (make-instance 'local-socket :type :stream))
       (mpd-stream (init-mpd-connection mpd-socket))
       (status (get-status mpd-stream))
       (state (gethash "state" status))
       (playing (string= state "play"))
       (tm (gethash "time" status))
       (seconds
         (if tm
             (let ((cut (position #\: tm
                                  :from-end t)))
               (parse-integer (subseq tm 0 cut)))
             0)))
  (flet ((send (cmd)
           (write-line cmd mpd-stream)
           (finish-output mpd-stream)
           (read-line mpd-stream)))
    (cond
      ((string-equal cmd "next")
       (send "next")
       (when (not playing)
         (send "pause")))
      ((string-equal cmd "prev")
       (if (and playing (> seconds 5))
           (send "seekcur 0")
           (send "previous"))
       (when (not playing)
         (send "pause")))
      ((string-equal cmd "toggle")
       (send (if playing "pause" "play"))))))
