#lang racket

(require racket/port)
(require racket/gui)
(require "Encrypt.rkt")

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

;(directory-list (current-directory))

(with-handlers ((exn:fail:contract:divide-by-zero?
                 (lambda (exn) "Oops")))
  (/ 1 0))

;(string (integer->char 55000))

(define x 51)

(procedure? run-encrypt-decrypt)







