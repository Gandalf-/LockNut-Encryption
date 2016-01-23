#lang racket

(define (alter-key-list default-key-list pass-input-list)
  (let loop ((default-list default-key-list)
             (pass-list pass-input-list)
             (output '() ))
    (if (empty? default-list)
        output
        (begin
          ;Reset the pass-list
          (when (empty? pass-list)
            (set! pass-list pass-input-list))
          
          (loop (cdr default-list)
                (cdr pass-list) 
                (flatten (cons output (+ (car default-list)
                                         (car pass-list)))))
          ))
    ))

(define A '(1 2 3 4 5 6 7))
(define B '(7 6 5 4 3 2 1))
(alter-key-list A B)