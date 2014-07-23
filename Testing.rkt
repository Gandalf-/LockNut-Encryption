#lang racket

;Prints x to a file
(define (print-this x name)
  (call-with-output-file* name #:exists 'replace
                          (lambda (output-port)
                            (display x output-port))))
(define (make-all-chars)
  (let loop ((i 0) (output ""))
    (if (= i 255)
        (print-this output "ASCII.txt")
        (loop (+ i 1) (string-append output " " (string (integer->char i)))))))

;(make-all-chars)

(let ((a (string->list "apple") )
      (b "banana"))
  (set! a (append (string->list b) a))
  a)
    