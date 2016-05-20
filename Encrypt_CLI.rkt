#lang racket

;----------------------------------------------------------------------
;Encrypt
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;Functions that work between the algorithm and the GUI, handle file IO
;----------------------------------------------------------------------

(require racket/port)
(require "Encrypt_Core.rkt")
(require "waterfall/Waterfall.rkt")


;Provisions to LockNut.rkt
(provide decrypt)
(provide create-file)
(provide print-this)
(provide Encrypt_CLI-test)


;Globals
(define curr-file-name "")
(define unbuffed-password "")


;FILE ENCRYPTION
; CALLED BY CREATE-FILE, decrypt
; Takes a file as input and prints the encrypted version of the file to a .locknut file
;
; string, string -> string
;--------------------------------------------------------------------------------
(define (encrypt-file input-file-name password)
  (let (;Get string from source file, add password to front
        (plain-text
          (string-append
            password
            (file->listChars input-file-name)))

        ; Add .locknut extension
        (new-file-name (string-append input-file-name ".locknut")))

    ;Remove the older version of the output file if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))

    ;Encrypt and print
    (print-this
      (waterfall 
        plain-text
        (list->string (map (lambda (x) (integer->char x)) key-list))
        #t)
      new-file-name)

    ;Delete the input file
    (delete-file input-file-name) )
  "Done")


;FILE DECRYPTION
; CALLED BY CREATE-FILE, decrypt
; Takes an encrypted .locknut file as input and prints the decrypted version of the 
; file to a text file.
; If glancing, open the file in notepad and delete when the user is finished.
; Otherwise, rename the decrypted text file to the original name of the input
;
; string, string -> string || bool
;--------------------------------------------------------------------------------
(define (decrypt-file input-file-name password)
  ;Get the char-list from the encryped file
  (let ((chars-list
          (string->list 
            (file->listChars input-file-name)))

        ;Remove .locknut extension, length checked by create-file and decrypt
        (new-file-name 
            (substring input-file-name 0 (- (string-length input-file-name) 8))))

    ;Remove the older version of output file, if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))

    ;Decrypt the file with the given buffered password
    (let ((decrypted-file 
            (waterfall
              (list->string chars-list)
              (list->string (map (lambda (x) (integer->char x)) key-list))
              #f)) )

      ;Verify password against buffered password, and decrypt
      (if (equal? (substring decrypted-file 0 50) password)
        (begin
          ;Valid password: print to file; remove encrypted file
          (print-this 
            (substring decrypted-file 50 (string-length decrypted-file))
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
;
; string, string, bool, bool -> string
;---------------------------------------------
(define (decrypt file-name password password-is-key?-value shareable?)
  ;Save password in case the user wants to re-encrypt
  ;Buffer password, then generate the cipher key-list
  (set! unbuffed-password password)
  (set! password (generate-key-and-solver
                   (buff-password password)
                   password-is-key?-value
                   shareable?))
  ;Get file choice
  (let ((chosen-file file-name))
    (if (not (file-exists? chosen-file))
      "Error: file does not exist"

      ;Check if file is .locknut extension
      (if (and
            (> (string-length chosen-file) 8)
            (equal? ".locknut" 
                    (substring chosen-file (- (string-length chosen-file) 8))))
        (if (decrypt-file chosen-file password)
          "Done"
          "Error: invalid password or incorrect base-key")

        "Error: file must be encrypted .locknut file") )))


;CREATE NEW ENCRYPTED FILE
;CALLS ENCRYPT
;
;string, string, bool, bool -> string
;-----------------------------------------------
(define (create-file given-file-name password password-is-key?-value shareable?)

  ;Buffer password and generate the cipher key-list
  (set! password (generate-key-and-solver 
                   (buff-password password)
                   password-is-key?-value
                   shareable?))
  (if (not (file-exists? given-file-name))
    "Error: file does not exist"

    (if (and
          (> (string-length given-file-name) 8)
          (equal? ".locknut"
                (substring given-file-name (- (string-length given-file-name) 8))))
      ; fail
      "Error: cannot re-encrypt .locknut files"

      ; encrypt file
      (encrypt-file given-file-name password))))


; TESTS
; ==========================
(define (Encrypt_CLI-test)
  (displayln "Encrypt-CLI tests complete"))
