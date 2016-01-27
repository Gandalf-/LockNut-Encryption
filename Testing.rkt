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

(define (func a b)
  (if (< (length b) (length a))
      (func a (append b b))
      (map
       (lambda (x y) (+ x y))
       a
       (take b (length a)))))

(define A '(1 2 3 4 5 6 7 6 5 4 3 2 1 2 3 4 4))
(define B '(7 6 5 4 3 2 1))

(alter-key-list A B)
(func A B)

(build-list 250 (lambda (x) (random 100)))

(list->string
 (flatten
  (map
   string->list
   '("wow" "lol" "amazing" "golly"))))

(foldr string-append "" '("wow" "lol" "amazing" "golly"))

; list of chars to strings of len length
(define (split-list input len)
  (let loop ((out '() )
             (curr input))
    (if (> len (length curr))

        ;Remove potential empty string in results
        (let ((res (reverse (cons (list->string curr)
                                  out))))
          (if (string=? (last res) "")
              (take res (- (length res) 1))
              res))

        ;Add len elements to output
        (loop (cons (list->string (take curr len))
                    out)
              (list-tail curr len)) )))

(define (splitter input len)
  (let loop ((

