#!/bin/chicken-csi -s

; todo
;   truncate song names if too long
;   get-mpc can probably use the tcp interface
;     some issue when mpc cant start cuz of jack idek

(import
  srfi-13
  chicken.format
  chicken.file.posix
  chicken.process
  chicken.irregex
  chicken.io
  )

(define (anim . frames)
  (vector (list->vector frames) -1))

(define (anim.frames a) (vector-ref a 0))
(define (anim.at a)     (vector-ref a 1))
(define (anim.at! a to) (vector-set! a 1 to))

(define (anim.peek a)
  (vector-ref (anim.frames a) (anim.at a)))

(define (anim.next! a)
  (anim.at! a (+ (anim.at a) 1))
  (when (>= (anim.at a) (vector-length (anim.frames a)))
    (anim.at! a 0))
  (anim.peek a))

(define (anim.prev! a)
  (anim.at! a (- (anim.at a) 1))
  (when (< (anim.at a) 0)
    (anim.at! a (- (vector-length (anim.frames a)) 1)))
  (anim.peek a))

;

(define (get-battery)
  (let* (
    (capacity (or (string->number
                    (string-trim-right
                      (with-input-from-file "/sys/class/power_supply/BAT1/capacity" read-string)))
                  0))
    (status (string-ref
              (with-input-from-file "/sys/class/power_supply/BAT1/status" read-string)
              0))
    )
    (when (equal? status #\D)
      (cond
        ((< capacity 9)
          (system "notify-send -u critical 'critcal battery'"))
        ((< capacity 15)
          (system "notify-send 'low battery'"))))
    (sprintf "~A~A" capacity
      (case status
        ((#\D)     " ")
        ((#\C #\F) "+")
        ((#\U)     "!")
        (else      "?")
      ))
  ))

(define (get-time)
  (let* (
    (time (call-with-input-pipe "date +'%I%M%S'" (cut read-string #f <>)))
    (hr   (substring time 0 2))
    (min  (substring time 2 4))
    (sec  (string->number (substring time 4 6)))
    (msec (- sec (modulo sec 3)))
    )
    ; (sprintf "~A:~A.~A" hr min
        ; (string-pad (number->string msec) 2 #\0))))
    (sprintf "~A:~A" hr min)
  ))

(define (get-date)
  (let* (
    (date (call-with-input-pipe "date +'%b%d'" (cut read-string #f <>)))
    (mon  (substring date 0 3))
    (day  (substring date 3 5))
    )
    (sprintf "~A~A" mon day)
  ))

;

(define sep "\a")

(define mpc-cmd
  (sprintf "mpc -f '~A'"
    (apply string-append
      (map
        (lambda (s)
          (string-append ".%" s "%#" sep))
        '("artist" "title" "file" "album"))
      )))


(define (truncate-filepath str)
  (irregex-match-substring
    (irregex-search "([^/]*\\..*)" str)
    1))

(define paused
  (anim
    "   (__"
    "   (__"
    "   (__"
    "  .(__"
    "  z(__"
    " Z (__"
    "z  (__"
  ))

; TODO some type of error when mpc isnt started or something
(define (get-mpd)
  (call/cc
    (lambda (return)
      (let* (
        (mpc (open-input-pipe mpc-cmd))
        (info-str (read-line mpc))
        (status-str (read-line mpc))
        )
        (close-input-pipe mpc)
        (when (not (and (string? info-str)
                        (string? status-str)))
          (return (anim.next! paused)))
        ; (display info-str) (newline)
        (let (
          (info (map
                  (lambda (str)
                    (and (> (string-length str) 1) (substring str 1)))
                  (irregex-split sep info-str)))
          (is-playing (string-contains status-str "playing"))
          )
          ; (display info)
          ; (newline)
          (let-values (
            ((artist title file album) (apply values info))
            )
            (if is-playing
                (sprintf "~A ~A ~A ~A ~A"
                  (or artist "")
                  (if artist "o" "*")
                  (or title (truncate-filepath file) "<void>")
                  (if album "o" "*")
                  (or album ""))
                (anim.next! paused)))
          )
        ))
    ))

;

(define wink
  (anim
    " (oo"
    " (oo"
    "★(-o"
  ))

(define ct 0)

(define (gen-bar)
  (let (
    ; done here so even if batt isnt showing
    ;   u get notified on low battery
    (b (get-battery))
    )
    (set! ct (+ ct 1))
    (sprintf "~A #[fg=colour233,bold]< ~A < ~A\n"
      (anim.next! wink)
      (get-mpd)
      (let* (
        (m (modulo ct 5))
        (txt
          (cond
            ((= m 0) (get-time))
            ((= m 1) b)
            ((= m 2) (get-time))
            ((= m 3) b)
            ((= m 4) (get-date))))
        )
        (string-pad txt 5))
        )
    ))

(define fifo-file "/home/mel/.citrus")
(unless (fifo? fifo-file)
  (create-fifo fifo-file))
(define out (file-open fifo-file open/wronly))

(let loop ()
  (define text (gen-bar))
  (file-write out text)
  ; (display text)
  (sleep 1)
  (loop)
  )
