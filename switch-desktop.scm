#!/bin/csi -s

(define args (cdddr (argv)))
(when (or (null? args)
        (not (member (car args) '("next" "prev"))))
  (exit))
(define cmd (car args))

(use utils)

(define files '#(
  "/home/mel/images/bgs/acid_fast_leprae.jpg"
  "/home/mel/images/bgs/alien-nine.jpg"
  "/home/mel/images/bgs/bacteria.jpg"
  "/home/mel/images/bgs/chip.jpg"
  "/home/mel/images/bgs/yellow.png"
  ; "/home/mel/images/bgs/bw.jpg"
  ; "/home/mel/images/bgs/yaya.jpg"
  ; "/home/mel/images/bgs/ring.png"
  ; "/home/mel/images/bgs/chalkboard.jpg"
  ; "/home/mel/images/bgs/HQPT32.jpg"
  ; "/home/mel/images/bgs/tAG_21252.jpg"
  ))

(define last-feh (with-input-from-file "/home/mel/.fehbg" read-all))
(define last-desktop
  (irregex-match-substring
    (irregex-search
     '(: #\' ($ (+ any)) #\') last-feh)
    1)
  )

(define next #f)

(let loop (
  (n 0)
  )
  (cond
    ((>= n (vector-length files))
      (set! next 0))
    ((string=? last-desktop (vector-ref files n))
      (set! next
        (modulo
          (if (string=? cmd "next")
              (+ n 1)
              (- n 1))
          (vector-length files))))
    (else
      (loop (+ n 1)))
    ))

(system (string-append
  "feh --bg-fill " (vector-ref files next)))
