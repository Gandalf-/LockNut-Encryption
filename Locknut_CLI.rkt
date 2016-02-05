#lang racket

;LockNut
;LockNut Encryption Program
;Austin Voecks, Spring 2015

;CLI Version
; -p password
; -e encrypt
; -d decrypt
; -s shareable
; -V password-is-key
; -h help
;--------------------------------------------
(require "Encrypt_CLI.rkt")

; Commandline arguments
(define pass "")
(define file "")
(define encrypt? #f)
(define decrypt? #f)
(define share? #f)
(define pass-is-key? #f)
(define help? #f)

(define locknut
  (command-line
    #:program "Locknut"
    #:once-any
    (("-e") "Encrypt file"
            (set! encrypt? #t))
    (("-d") "Decrypt file"
            (set! decrypt? #t))
    #:once-each
    (("-s") "Use standard key"
            (set! share? #t))
    (("-V") "Password is key"
            (set! pass-is-key? #t))
    (("-p") password
            "Password"
            (set! pass password))
    #:args (filename)
    (set! file filename)))

;Creates a string of 250 random integers [0-200]
(define (generate-personal-key)
  (let loop ((out "" )
             (i 0))
    (if (= i 250)
      out
      (loop (string-append out (number->string (random 100)) " ")
            (+ i 1))) ))

;Startup sequence. Check for PersonalKey file, otherwise
; run first time setup: Generate personal key, offer readme
(define (startup)
  (unless (file-exists? "ln_data/PersonalKey.locknut")
    ;Create personal key
    (displayln "Running first-time initialization sequence...")
    (unless (directory-exists? "Data")
      (make-directory "Data"))
    (print-this (generate-personal-key) "ln_data/PersonalKey.locknut")
    (sleep 1.5)
    (displayln "Personal encryption key generated. Ready.") ))

(startup)

;Decrypt
(when decrypt?
  (displayln
    (decrypt file pass pass-is-key? share?)))

;Encrypt
(when encrypt?
  (displayln
    (create-file file pass pass-is-key? share?)))
