;; chicken sceme

(import foreign)
(use lolevel utf8-srfi-13 utils posix irregex)

#>
#include <mqueue.h>

struct msg {
  int do_quit;
  int current;
  int windows[6];
  int urgent[6];
};
<#

(define mq-open
  (foreign-lambda int "mq_open" c-string int))
(define mq-receive
  (foreign-lambda int "mq_receive" int c-pointer int c-pointer))

(define-foreign-type msg* (c-pointer "struct msg"))

(define (make-msg)
  (set-finalizer!
    (allocate (foreign-type-size "struct msg"))
    free))

(define msg._do_quit (foreign-lambda* int ((msg* m)) "C_return(m->do_quit);"))
(define msg.current (foreign-lambda* int ((msg* m)) "C_return(m->current);"))
(define msg.windows (foreign-lambda* int ((msg* m) (int at)) "C_return(m->windows[at]);"))
(define msg.urgent (foreign-lambda* int ((msg* m) (int at)) "C_return(m->urgent[at]);"))

(define (msg.do_quit? m) (not (= (msg._do_quit m) 0)))


;;

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

(define (cmd->string name)
  (call-with-input-pipe name read-all))

(define file->string read-all)

(define (dnL . args)
  (for-each
    (lambda (it) (display it) (display " "))
    args)
  (newline))

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
      (case status ((#\D) ".")
                   ((#\C #\F) ":")
                   ((#\U) "!")
                   (else "?")))))

(define (get-time)
  (let* ( (time (cmd->string "date +'%I%M%S'"))
          (hr   (substring time 0 2))
          (min  (substring time 2 4))
          (sec  (string->number (substring time 4 6)))
          (msec (- sec (modulo sec 3))) )
    (sprintf "~A:~A.~A" hr min
        (string-pad (number->string msec) 2 #\0))))

;;

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

(define (msg->string m desktop-names)
  (let ( (ret
           (apply string-append
             (map
               (lambda (i name)
                 (let ( (indc
                          (cond ((= i (msg.current m)) ":")
                                ((> (msg.urgent m i) 0) "!")
                                ((> (msg.windows m i) 0) ".")
                                (else #f))) )
                   (or (and indc (string-append indc name " ")) "")))
               '(0 1 2 3 4 5)
               desktop-names))) )
    (substring ret 0 (- (string-length ret) 1))))

;;

(define (colorize a b)
  (sprintf "%{F~A}%{B~A}" a b))

(define colors.bg "#191919")
(define colors.fg "#AAAAAA")
(define colors.normal (colorize colors.bg colors.fg))
(define colors.invert (colorize colors.fg colors.bg))
(define colors.reset  (colorize "-" "-"))

(define wink
  (animation
    " (oo"
    " (oo"
    "★(-o"))

(define bar-cmd #<#CMD
  ~/dotfiles/_wm/bar/lemonbar -f "lucy tewi:pixelsize=10" \
  -f "Kochi Gothic:pixelsize=10:antialias=false" \
  -B "#{colors.bg}" -F "#{colors.fg}" -g x12
CMD
)

(define desktop-names
  '("lov♡" "kis♡" "ya ♡"
    "2 ♡♡" "♡jx " "(vv)"))

(define (get-mqd for)
  (call/cc
    (lambda (return)
      (let loop ( (num -1) )
        (if (>= num 0)
            (return num)
            (begin
              ; (file-select #f #f 0.01)
              (loop (mq-open for open/rdonly))))))))

(define (run)
  (let ( (mqd (get-mqd "/monsterwm"))
         (msg (make-msg))
         (bar
           (if (and (> (length (argv)) 1) (equal? (cadr (argv)) "debug"))
               (current-output-port)
               (open-output-pipe bar-cmd))) )
    (call/cc
      (lambda (quit)
        (let loop ()
          (let-values ( ((rd _) (file-select mqd #f 1)) )
            (when rd
              (mq-receive mqd msg (foreign-type-size "struct msg") #f)
              (when (msg.do_quit? msg)
                (quit)))
            (display
              (sprintf "%{l}~A~A~A %{c}~A %{r}~A~A ♡ ~A ♡ ~A~A\n"
                colors.normal
                (msg->string msg desktop-names)
                colors.invert

                (get-mpd)

                colors.normal
                (animation.next! wink)
                (get-time)
                (get-battery)
                colors.reset
              )
              bar
            )
            (flush-output bar)
            (loop)
            ))))
    (close-output-pipe bar)
    (dnL "goodbye")))

(run)
