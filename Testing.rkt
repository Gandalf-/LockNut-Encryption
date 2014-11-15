#lang racket

(require racket/gui)

(define (read-this file)
  (let ((out '() ))
    (for ([c (in-port read-char (open-input-file file))])
      (set! out (flatten (cons out c))))
    out))

;Moves the file into a string
(define (file->listChars filename)
  (port->string (open-input-file filename)))

;Creates a UTF8 .txt file
(define (init-file name)
  (when (file-exists? name)
    (delete-file name))
  ;Copy UTF8
  (copy-file "Support/UTF8.txt" name #f))

;Prints x to a file
(define (print-this x name)
  (call-with-output-file* name #:exists 'replace
                          (lambda (output-port)
                            (display x output-port))))

;(init-file "ABC.txt")
;(print-this "“Papa? What is it?” λ, α, γ ..." "ABC.txt")

;(file->listChars "ABC.txt")
;(list->string (read-this "Out.txt"))


(define f
  (new frame%
       (label "Simple Edit")
       (width 800)
       (height 600)))

(define c
  (new editor-canvas%
       (parent f)))

(define b
  (new button%
       (label "Done")
       (parent f)
       (callback (lambda (b e)
                   (send t save-file "Out.txt" 'text)
                   (send f show #f)))))

(define t
  (new text%))

(define mb (new menu-bar% [parent f]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))
(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)


(send c set-editor t)
(send f show #t)
(send t load-file "Hebrew.txt")


;(send t save-file "Example.txt" 'text)
 





