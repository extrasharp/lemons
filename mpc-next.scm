#!/bin/csi -s

(define args (cdddr (argv)))

(when (or (null? args)
          (not (member (car args) '("next" "toggle" "prev")))
          )
  (exit))

;

(use
  (only tcp tcp-connect)
  irregex)

(define cmd (car args))

(define (get-results port)
  (let loop (
    (acc '())
    (r (read port))
    )
    (if (eq? r 'OK)
        (reverse acc)
        (loop (cons r acc) (read port))
        )))

(define (make-sender port)
  (lambda (x)
    (display (string-append x "\n") port)))

(receive (i o) (tcp-connect "localhost" 6600)
  (let (
    (send (make-sender o))
    )
    (read-line i)
    (send "status")
    (let* (
      (results (get-results i))
      (state (symbol->string (get-keyword #:state results)))
      (time
        (if (not (string=? state "stop"))
            (symbol->string (get-keyword #:time results))
            #f))
      (secs
        (if time
            (string->number
              (irregex-match-substring
                (irregex-search '(: ($ integer) #\:) time) 1))
            0))
      (playing (string=? state "play"))
      )
      (cond
        ((string=? cmd "next")
          (send "next")
          (when (not playing)
            (send "pause"))
          )
        ((string=? cmd "prev")
          (if (and playing (> secs 5))
            (send "seekcur 0")
            (send "previous"))
          (when (not playing)
            (send "pause")))
        ((string=? cmd "toggle")
          (send (if playing "pause" "play")))
        )
      )
    )
  (close-input-port i)
  (close-output-port o)
  )
