#lang racket

; common
;
; variables and misc functions

(provide (all-defined-out))


;Globals
(define curr-file-name "")
(define (set-curr-file-name n) (set! curr-file-name n))

(define unbuffered-password "")
(define (set-unbuffered-password n) (set! unbuffered-password n))


; Define default-key, key-list
(define A
  '(82 85 71 2 12 37 88 56 29 79 30 27 82 2 91 67 86 94 72 45 96 92 53 84 1
    54 23 56 65 65 78 90 93 9 87 51 10 54 5 67 21 57 62 93 59 58 60 58 3 21))
(define B
  '(8 65 5 42 29 60 46 48 44 68 64 82 50 86 38 3 67 35 88 4 73 30 51 56 38
    18 72 45 69 17 3 30 16 54 78 40 38 31 23 27 4 98 7 55 45 30 16 9 54 66))
(define C
  '(60 22 41 53 76 78 6 70 34 25 56 88 37 18 60 67 34 57 52 55 33 27 14 18
    98 40 24 24 52 61 41 17 90 63 39 27 80 55 15 1 20 2 4 96 7 79 37 9 55 17))
(define D
  '(49 21 59 0 28 31 51 43 45 34 44 32 28 94 0 10 69 30 72 2 12 28 86 53 91
    84 6 32 17 51 78 56 58 12 6 39 51 54 14 37 65 8 43 88 2 8 30 87 16 13))
(define E
  '(75 27 9 31 28 66 63 29 68 46 10 2 16 20 13 58 24 15 13 29 85 17 70 9 62
    76 67 59 93 60 22 88 99 6 67 32 11 91 89 83 58 83 41 37 6 20 9 43 12 84))


; Default key, 250 (* 50 5), random integers [0-100]
(define default-key (append A B C D E))
(define default-password "6AQO*fvr*RQ7Uv!mCnPc8vxKdia45a$uh'7B5K06Rcj863RMyg")

; Both uniquely assigned during encrypt/decrypt
(define key-list '() )
(define (set-key-list l) (set! key-list l))


(define (generate-personal-key)
  ; Creates a string of 250 random integers [0-200]
  ;
  ; @return   string

  (string-join
    (build-list
      250
      (lambda (_) (number->string (random 200))))))


(define (get-personal-key)
  ;Gets the personal key from file
  ;
  ; string -> list of integers

  (map
    string->number
    (string-split (file->listChars "ln_data/PersonalKey.locknut"))))

;----------------------------------------------------------------------
;FILE IO
;----------------------------------------------------------------------

(define (file->listChars filename)
  ;Moves the file into a string
  ;
  ; string -> string

  (letrec ((in (open-input-file filename))
           (out (port->string in)))
    (close-input-port in)
    out))


(define (init-file name)
  ;Creates a new UTF8 .txt file by copying a blank copy
  ;
  ; string -> none

  (when (file-exists? name)
    (delete-file name))
  (copy-file "ln_support.txt" name #f))


(define (print-this x name)
  ;Prints x to a file
  ; any, string -> none

  (call-with-output-file* name #:exists 'replace
                          (lambda (output-port)
                            (display x output-port))))

; helpers

; Buffers the password to 50 characters
;
; string -> string
(define (buff-password password)
  ; Check if it's longer than 50 characters already, if it is, we truncate it
  (if (>= (string-length password) 50)
    (substring password 0 50)

    ;Buffer it using the default password
    (string-append
      password
      (substring default-password 0 (- 50 (string-length password))) )))


;Buffers a string >60 characters
;
; string -> string
(define (buff-string input)
  (let ((str-len (string-length input)))
    (if (>= str-len 60)
      (string-append "..." (substring input (- str-len 60) str-len))
      input) ))
