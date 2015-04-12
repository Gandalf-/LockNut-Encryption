#lang racket

;Encrypt
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;Functions that work between the algorithm and the GUI, handle file IO
;----------------------------------------------------------------------

(require racket/port)
(require "Encrypt-Algorithm.rkt")

;Provisions to LockNut.rkt
(provide decrypt)
(provide create-file)
(provide print-this)

;Globals
(define curr-file-name "")
(define unbuffed-password "")

;FILE IO
;----------------------------------------------------------------------

;Moves the file into a string
(define (file->listChars filename)
  (letrec ((in (open-input-file filename))
           (out (port->string in)))
    (close-input-port in)
    out))

;Creates a UTF8 .txt file
(define (init-file name)
  (when (file-exists? name)
    (delete-file name))
  ;Copy UTF8
  (copy-file "Support/UTF8.txt" name #f))

;Prints x to a file
(define (print-this x name)
  (call-with-output-file*
    name #:exists 'replace
    (lambda (output-port)
      (display x output-port))))

;HELPERS
;------------
;Buffers the password to 50 characters using the default password
(define (buff-password password)
  (if (>= (string-length password) 50)
    (substring password 0 50)
    (string-append password 
                   (substring default-password 
                              0
                              (- 50 (string-length password)))) ))

;Buffers a string >60 characters
(define (buff-string input)
  (let ((str-len (string-length input)))
    (if (>= str-len 60)
      (string-append "..." (substring input (- str-len 60) str-len))
      input) ))

;CALLEES
;================================================

;FILE ENCRYPTION
; CALLED BY CREATE-FILE, decrypt
; Takes a file as input and prints the encrypted version of the file to a .locknut file
;--------------------------------------------------------------------------------
(define (encrypt-file input-file-name password)
  (let (;Get list of chars from source file
        (chars-list (string->list
                      (file->listChars input-file-name)))
        ;Remove .txt and add .locknut extension
        (new-file-name (string-append
                         (substring input-file-name
                                    0
                                    (- (string-length input-file-name) 4))
                         ".locknut")))

    ;Add the buffered password onto the beginning of the chars-list, which is the file
    (set! chars-list (append (string->list password)
                             chars-list))

    ;Remove the older version of the output file if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))

    ;Encrypt and print
    (print-this (list->string (encrypt chars-list key-list password))
                new-file-name)

    ;Delete the input file
    (delete-file input-file-name)
    "Encryption complete" ))


;FILE DECRYPTION
; CALLED BY CREATE-FILE, decrypt
; Takes an encrypted .locknut file as input and prints the decrypted version of the 
; file to a text file.
; If glancing, open the file in notepad and delete when the user is finished.
; Otherwise, rename the decrypted text file to the original name of the input
;--------------------------------------------------------------------------------
(define (decrypt-file input-file-name password)
  ;Get the char-list from the encryped file
  (let ((chars-list (string->list
                      (file->listChars input-file-name)))
        ;Remove .locknut extension and add .txt
        (new-file-name (string-append
                         (substring input-file-name
                                    0
                                    (- (string-length input-file-name) 8))
                         ".txt")))

    ;Remove the older version of output file, if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))

    ;Decrypt the file with the given buffered password
    (let ((decrypted-file
            (list->string
              (encrypt chars-list solver-list password)) ))

      ;Verify password against buffered password, and decrypt
      (if (equal? (substring decrypted-file 0 50) password)
        (begin
          ;Valid password: print to file; remove encrypted file
          (print-this (substring decrypted-file 50 (string-length decrypted-file))
                      new-file-name)
          (when (file-exists? input-file-name)
            (delete-file input-file-name)) )

        ;Invalid password
        #f)) ))


;CALLERS
;================================================================

;decrypt
; CALLS DECRYPT-FILE
; Checks the filename and passes info back up
;---------------------------------------------
(define (decrypt file-name password password-is-key?-value shareable?)
  ;Save password in case the user wants to re-encrypt
  ;Buffer password, then generate the cipher key-list and solver-list
  (set! unbuffed-password password)
  (set! password (generate-key-and-solver
                   (buff-password password)
                   password-is-key?-value
                   shareable?))
  ;Get file choice
  (let ((chosen-file file-name))
    (if (not (file-exists? chosen-file))
      "File does not exist"

      ;Check if file is .locknut extension
      (if (equal? ".locknut" (substring chosen-file (- (string-length chosen-file) 8)))
        (if (decrypt-file chosen-file password)
          "Decryption complete"
          "Invalid password or incorrect base-key")

        "File must be encrypted .locknut file") )))


;CREATE NEW ENCRYPTED FILE
;CALLS ENCRYPT
;-----------------------------------------------
(define (create-file given-file-name password password-is-key?-value shareable?)

  ;Buffer password and generate the cipher key-list and solver-list
  (set! password (generate-key-and-solver 
                   (buff-password password)
                   password-is-key?-value
                   shareable?))
  ;Encrypt file
  (encrypt-file given-file-name password))
