#!/bin/gsi

(define (filter pred lst)
  (fold-right
    (lambda (next acc)
      (if (pred next)
          (cons next acc)
          acc))
    '()
    lst))

;

(define (read-file filepath)
  (call-with-input-file filepath
    (lambda (p)
      (apply string-append
        (read-all p read-line)))))

;

(define (vector-search vec pred #!key (start-index 0) (from-end #f))
  (let ((inc (if from-end -1 1))
        (len (vector-length vec)))
    (let rec ((idx (if from-end (- len start-index 1) start-index)))
      (if (or (< idx 0) (>= idx len))
          #f
          (if (pred (vector-ref vec idx))
              idx
              (rec (+ idx inc)))))))

(define (string-search str pred #!key (start-index 0) (from-end #f))
  (let ((inc (if from-end -1 1))
        (len (string-length str)))
    (let rec ((idx (if from-end (- len start-index 1) start-index)))
      (if (or (< idx 0) (>= idx len))
          #f
          (if (pred (string-ref str idx))
              idx
              (rec (+ idx inc)))))))

(define (string-find str ch #!key (start-index 0) (from-end #f))
  (string-search str
                 (lambda (sch) (char=? sch ch))
                 start-index: start-index
                 from-end: from-end))

;

(define _bgs-dir "/home/mel/images/_bgs/")
(define _feh-cache "/home/mel/.fehbg")

(let* ((cmds (cdr (command-line)))
       (cmd (car cmds))
       (files (list->vector
                (filter
                  (lambda (fname)
                    (eq? (file-type (string-append _bgs-dir fname)) 'regular))
                  (directory-files _bgs-dir))))
       (last-file
         (let* ((feh-str (read-file _feh-cache))
                (fp-start (string-find feh-str #\'))
                (fp-end (string-find feh-str #\' from-end: #t)))
           (path-strip-directory (substring feh-str fp-start fp-end))))
       (next-file
         (let* ((pos (vector-search files
                                    (lambda (fp)
                                      (string=? fp last-file))))
                (next-idx
                  (cond
                    ((not pos) 0)
                    ((string=? cmd "next")
                     (modulo (+ pos 1) (vector-length files)))
                    ((string=? cmd "prev")
                     (modulo (- pos 1) (vector-length files))))))
           (vector-ref files next-idx))))
  (shell-command (string-append "/bin/feh --bg-fill " _bgs-dir next-file)))

