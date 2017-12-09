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
(require "EncryptCLI.rkt")

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


(define (generate-personal-key)
  ;Creates a string of 250 random integers [0-200]

  (string-join
    (build-list
      250
      (lambda (_) (number->string (random 200))))))


(define (startup)
  ;Startup sequence. Check for PersonalKey file, otherwise run first time
  ;setup: Generate personal key, offer readme

  (unless (file-exists? "ln_data/PersonalKey.locknut")
    ;Create personal key
    (displayln "Running first-time initialization sequence...")
    (unless (directory-exists? "Data")
      (make-directory "Data"))
    (print-this (generate-personal-key) "ln_data/PersonalKey.locknut")
    (sleep 1.5)
    (displayln "Personal encryption key generated. Ready.") ))

(define (main)
  (begin
    (startup)

    (when decrypt?
      ; Decrypt
      (displayln
        (decrypt file pass pass-is-key? share?)))

    (when encrypt?
      ; Encrypt
      (displayln
        (create-file file pass pass-is-key? share?)))
    ))

(main)
