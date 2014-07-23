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

(letrec ((a 5)
      (b (+ a 1)))
  b)
    
(define default '(1 2 3 4 5 6 8))

(let loop ((i 0) (output '() ))
  (if (= i 25)
      output
      (loop (+ i 1) (flatten (cons output (random 100))))))
      