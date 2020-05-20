#!/bin/guile
!#

(use-modules (srfi srfi-69)
             (ice-9 textual-ports))

;

(define (open-mpd-port)
  (let ((s (socket PF_UNIX SOCK_STREAM 0)))
    (connect s AF_UNIX (getenv "MPD_HOST"))
    (get-line s)
    s))

(define (mpd-port-do-command p cmd)
  (display cmd p)
  (newline p)
  (let loop ((acc '()))
    (let ((reply (get-line p)))
      (cond
        ((string=? reply "OK")
         (reverse acc))
        ((string=? (substring reply 0 3) "ACK")
         (error "mpd error" reply))
        (else
         (loop (cons reply acc)))))))

(define (mpd-port-reply->hash-table reply)
  (let ((ht (make-hash-table)))
    (for-each
      (lambda (line)
        (let* ((colon-idx (string-index line #\:))
               (key (substring line 0 colon-idx))
               (value (substring line (+ colon-idx 2))))
          (hash-table-set! ht key value)))
      reply)
    ht))

(define (mpd-port-next! p status-ht)
  (let* ((state (hash-table-ref status-ht "state"))
         (is-playing (string=? state "play")))
    (mpd-port-do-command p "next")
    (unless is-playing
      (mpd-port-do-command p "pause"))))

(define (mpd-port-prev! p status-ht)
  (let* ((state (hash-table-ref status-ht "state"))
         (is-playing (string=? state "play"))
         (elapsed (hash-table-ref/default status-ht "elapsed" #f))
         (seconds (if elapsed
                      (string->number elapsed)
                      0)))
    (if (and is-playing (> seconds 5))
        (mpd-port-do-command p "seekcur 0")
        (mpd-port-do-command p "previous"))
    (unless is-playing
      (mpd-port-do-command p "pause"))))

(define (mpd-port-toggle! p status-ht)
  (let* ((state (hash-table-ref status-ht "state"))
         (is-playing (string=? state "play")))
    (mpd-port-do-command p
                         (if is-playing
                             "pause"
                             "play"))))

;

(let* ((cmds (cdr (command-line)))
       (cmd (car cmds))
       (p (open-mpd-port))
       (status-ht (mpd-port-reply->hash-table (mpd-port-do-command p "status"))))
  (cond
    ((string=? cmd "next")
     (mpd-port-next! p status-ht))
    ((string=? cmd "prev")
     (mpd-port-prev! p status-ht))
    ((string=? cmd "toggle")
     (mpd-port-toggle! p status-ht)))
  (close-port p))
