#!/bin/guile
!#

(use-modules (srfi srfi-9))

(define (wrap val lo hi)
  (let ((range (- hi lo))
        (adjusted (- val lo)))
    (+ lo (modulo adjusted range))))

(define-record-type <animation>
  (_make-animation frames at)
  animation?
  (frames animation-frames)
  (at animation-at animation-at!))

(define (make-animation frames)
  (_make-animation frames 0))

(define (animation-peek a)
  (vector-ref (animation-frames a)
              (animation-at a)))

(define (animation-advance! a ct)
  (let ((at (wrap (+ ct (animation-at a))
                  0
                  (vector-length (animation-frames a)))))
    (animation-at! a at)
    (animation-peek a)))

(define (animation-next! a)
  (animation-advance! a 1))

(define (animation-prev! a)
  (animation-advance! a -1))

(define anim (make-animation #(a b c d)))
