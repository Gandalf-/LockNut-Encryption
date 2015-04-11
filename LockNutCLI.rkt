#lang racket

;LockNut
;LockNut Encryption Program
;Austin Voecks, Spring 2015

;CLI Version
; -p password
; -f file
; -e encrypt
; -d decrypt
; -s shareable
; -V password-is-key
; -h help
;--------------------------------------------
(require "Encrypt.rkt")

(define help-mode #f)
(define wait-time 1.5)

;Decrypt

;Encrypt

;Creates a string of 250 random integers [0-200]
(define (generate-personal-key)
  (let loop ((out "" )
             (i 0))
    (if (= i 250)
      out
      (loop (string-append out (number->string (random 100)) " ")
            (+ i 1)))
    ))

;Startup sequence. Check for PersonalKey file, otherwise
; run first time setup: Generate personal key, offer readme
(define (startup)
  (if (file-exists? "Data/PersonalKey.locknut")
    ;Already ran init sequence
    (displayln "Personal key loaded. Ready.")

    ;Create personal key
    (begin
      (displayln "Running first-time initialization sequence...")
      (unless (directory-exists? "Data")
        (make-directory "Data"))
      (print-this (generate-personal-key) "Data/PersonalKey.locknut")
      (sleep 1.5)
      (displayln "Personal encryption key generated. Ready.")
      ))

  (startup)
