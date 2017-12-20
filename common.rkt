#lang typed/racket

; common
;
; variables and misc functions

(provide (all-defined-out))


;Globals
(define curr-file-name "")

(: set-curr-file-name (-> String Void))
(define (set-curr-file-name n)
  (set! curr-file-name n))

(define unbuffered-password "")
(: set-unbuffered-password (-> String Void))
(define (set-unbuffered-password n)
  (set! unbuffered-password n))


(define default-key
  (append
    '(82 85 71 2 12 37 88 56 29 79 30 27 82 2 91 67 86 94 72 45 96 92 53 84 1
      54 23 56 65 65 78 90 93 9 87 51 10 54 5 67 21 57 62 93 59 58 60 58 3 21)
    '(8 65 5 42 29 60 46 48 44 68 64 82 50 86 38 3 67 35 88 4 73 30 51 56 38
      18 72 45 69 17 3 30 16 54 78 40 38 31 23 27 4 98 7 55 45 30 16 9 54 66)
    '(60 22 41 53 76 78 6 70 34 25 56 88 37 18 60 67 34 57 52 55 33 27 14 18
      98 40 24 24 52 61 41 17 90 63 39 27 80 55 15 1 20 2 4 96 7 79 37 9 55 17)
    '(49 21 59 0 28 31 51 43 45 34 44 32 28 94 0 10 69 30 72 2 12 28 86 53 91
      84 6 32 17 51 78 56 58 12 6 39 51 54 14 37 65 8 43 88 2 8 30 87 16 13)
    '(75 27 9 31 28 66 63 29 68 46 10 2 16 20 13 58 24 15 13 29 85 17 70 9 62
      76 67 59 93 60 22 88 99 6 67 32 11 91 89 83 58 83 41 37 6 20 9 43 12 84)))

(define default-password
  "6AQO*fvr*RQ7Uv!mCnPc8vxKdia45a$uh'7B5K06Rcj863RMyg")

(define key-list '() )
(: set-key-list (-> (Listof Any) Void))
(define (set-key-list l)
  (set! key-list l))


(: generate-personal-key (-> String))
(define (generate-personal-key)
  ; Creates a string of 250 random integers [0-200]

  (string-join
    (build-list 250 (lambda (_) (number->string (random 200))))))


; (: get-personal-key (-> (Listof Integer)))
(define (get-personal-key)
  ; Gets the personal key from file

  (map
    string->number
    (string-split (file->string "ln_data/PersonalKey.locknut"))))


(: file->string (-> String String))
(define (file->string filename)
  ; Moves the file into a string

  (letrec ((in (open-input-file filename))
           (out (port->string in)))
    (close-input-port in)
    out))


(: init-file (-> String Void))
(define (init-file name)
  ; Creates a new UTF8 .txt file by copying a blank copy

  (when (file-exists? name)
    (delete-file name))
  (copy-file "ln_support.txt" name))


; (define (print-this x name)
;   ; Prints x to a file
;
;   (call-with-output-file*
;     name #:exists 'replace
;     (lambda (output-port)
;       (display x output-port))))
(: print-this (-> String String Void))
(define (print-this content fname)
  (with-output-to-file
    fname
    (lambda () (printf content))))


(: buffer-password (-> String String))
(define (buffer-password p)
  ; Buffers the password to 50 characters. Check if it's longer than 50
  ; characters already, if it is, we truncate it. this is necessary so we
  ; can verify that decryption succeeded by comparing the first 50 characters
  ; of the decrypted file with the password provided

  (if (>= (string-length p) 50)

    (substring p 0 50)

    ;Buffer it using the default password
    (string-append
      p
      (substring default-password 0 (- 50 (string-length p))) )))


(: buffer-fname (-> String Integer String))
(define (buffer-fname s n)
  ; truncates strings over n characters, this is used by the gui to make very
  ; long file names readable. if len(s) < n, prepend '...' to s

  (let ((size : Integer (string-length s)))
    (if (< size n)
      s
      (string-append "..." (substring s (- size n) size))
      )))


(: swap-extension (-> String String String String))
(define (swap-extension base a b)
  ; remove 'a' and add 'b' to base

  (string-append
    (substring base 0 (- (string-length base) (string-length a)))
    b))
