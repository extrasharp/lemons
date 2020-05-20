#!/bin/guile
!#

(use-modules (srfi srfi-1)
             (srfi srfi-43)
             (ice-9 textual-ports)
             (ice-9 ftw))

(define (directory-get-regular-files dir)
  (filter-map
    (lambda (file)
      (let ((fname (car file))
            (st (cadr file)))
        (and (eq? (stat:type st) 'regular) fname)))
    (cddr (file-system-tree dir))))

(define (filepath-strip-directory fp)
  (substring fp (+ 1 (string-rindex fp #\/))))

(define _bgs-dir "/home/mel/images/_bgs/")
(define _feh-cache "/home/mel/.fehbg")

(let* ((cmds (cdr (command-line)))
       (cmd (car cmds))
       (files (list->vector (directory-get-regular-files _bgs-dir)))
       (last-file
         (let* ((feh-str (call-with-input-file _feh-cache get-string-all))
                (fp-start (+ (string-index feh-str #\') 1))
                (fp-end (string-rindex feh-str #\'))
                (filepath (substring feh-str fp-start fp-end)))
           (filepath-strip-directory filepath)))
       (next-file
         (let* ((pos (vector-index (lambda (fp)
                                     (string=? fp last-file))
                                   files))
                (next-idx
                  (cond
                    ((not pos) 0)
                    ((string=? cmd "next")
                     (modulo (+ pos 1) (vector-length files)))
                    ((string=? cmd "prev")
                     (modulo (- pos 1) (vector-length files))))))
           (vector-ref files next-idx))))
  (system (string-append "/bin/feh --bg-fill " _bgs-dir next-file)))

