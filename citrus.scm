;; chicken scheme

(import foreign)
;; todo
(use posix
     utf8-srfi-13
     irregex
     utils)

;; util

(define (cmd->string name)
  (call-with-input-pipe name read-all))

(define file->string read-all)

(define (dnL . args)
  (for-each
    (lambda (it) (display it) (display " "))
    args)
  (newline))

;; animations

(define (animation . frames)
  (vector (list->vector frames) -1))

(define (animation.frames a) (vector-ref a 0))
(define (animation.at a)     (vector-ref a 1))
(define (animation.at! a to) (vector-set! a 1 to))

(define (animation.peek a)
  (vector-ref (animation.frames a) (animation.at a)))

(define (animation.next! a)
  (animation.at! a (+ (animation.at a) 1))
  (when (>= (animation.at a) (vector-length (animation.frames a)))
    (animation.at! a 0))
  (animation.peek a))

(define (animation.prev! a)
  (animation.at! a (- (animation.at a) 1))
  (when (< (animation.at a) 0)
    (animation.at! a (- (vector-length (animation.frames a)) 1)))
  (animation.peek a))

;;

(define (get-battery)
  (let* ( (capacity (or (string->number
                          (string-trim-right
                            (file->string "/sys/class/power_supply/BAT1/capacity")))
                        0))
          (status (string-ref (file->string "/sys/class/power_supply/BAT1/status") 0)) )
    (when (equal? status #\D)
      (cond ((< capacity 9)  (system "notify-send -u critical 'critcal battery'"))
            ((< capacity 15) (system "notify-send 'low battery'"))))
    (sprintf "~A~A" capacity
      (case status ((#\D) " ")
                   ((#\C #\F) "c")
                   ((#\U) "!")
                   (else "?")))))

(define (get-time)
  (let* ( (time (cmd->string "date +'%I%M%S'"))
          (hr   (substring time 0 2))
          (min  (substring time 2 4))
          (sec  (string->number (substring time 4 6)))
          (msec (- sec (modulo sec 3)))
          (date (cmd->string "date +'%b%d'"))
          (mon  (substring date 0 3))
          (day  (substring date 3 5)) )
    (sprintf "~A~A ~A:~A.~A" mon day hr min
        (string-pad (number->string msec) 2 #\0))))

;; get-mpd

(define sep "\a")

(define mpc-cmd
  (sprintf "mpc -f '~A'"
    (apply string-append
      (map (lambda (s) (string-append ".%" s "%#" sep))
        '("artist" "title" "file" "album")))))

(define (truncate-filepath str)
  (irregex-match-substring
    (irregex-search "([^/]*\\..*)" str)
    1))

(define paused
  (animation
    "   (__"
    "   (__"
    "   (__"
    "  .(__"
    "  z(__"
    " Z (__"
    "z  (__"))

(define (get-mpd)
  (call/cc
    (lambda (return)
      (let* ( (mpc (open-input-pipe mpc-cmd))
              (info-str (read-line mpc))
              (status-str (read-line mpc)) )
        (close-input-pipe mpc)
        (when (not (and (string? info-str) (string? status-str)))
          (return (animation.next! paused)))
        (let ( (info (map (lambda (str) (and (> (string-length str) 1) (substring str 1)))
                          (irregex-split sep info-str)))
               (is-playing (string-contains status-str "playing")) )
          (let-values ( ((artist title file album) (apply values info)) )
            (if is-playing
                (sprintf "~A ~A ~A ~A ~A"
                  (or artist "")
                  (or (and artist "o") "*")
                  (or (or title (truncate-filepath file)) "<void>")
                  (or (and album "o") "*")
                  (or album ""))
                (animation.next! paused))))))))

;;

(define (colorize a b)
  (sprintf "%{F~A}%{B~A}" a b))

(define colors.bg "#222222")
(define colors.fg "#AAAAAA")
(define colors.normal (colorize colors.bg colors.fg))
(define colors.invert (colorize colors.fg colors.bg))
(define colors.reset  (colorize "-" "-"))

(define wink
  (animation
    " (oo"
    " (oo"
    "★(-o"))

(define (right)
  (sprintf "~A #[fg=colour233,bold]< ~A < ~A < ~A\n"
    (animation.next! wink)
    (get-mpd)
    (get-time)
    (get-battery)
    ))

(display (right))

(define fifo-file "/home/mel/.citrus")
(if (not (fifo? fifo-file))
  (create-fifo fifo-file))
(define out (file-open fifo-file open/wronly))
(let loop ( (text (right)) )
  (file-write out text)
  (display text)
  (sleep 1)
  (loop (right))
  )

