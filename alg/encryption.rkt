#lang racket

(provide (all-defined-out))
(require "../common.rkt")
(require "../gui/common.rkt")
(require "waterfall.rkt")

;----------------------------------------------------------------------
;FILE ENCRYPTION
;----------------------------------------------------------------------

; encrypt-file
; Called by create-file, decrypt
; Takes a file as input and prints the encrypted version of the file to a .locknut file
;
; string, string -> none
;--------------------------------------------------------------------------------
(define (encrypt-file input-file-name password)
  (let (;Get string from source file, add password to front
        (plain-text
          (string-append
            password
            (file->listChars input-file-name)))

        ;Remove .txt and add .locknut extension
        (new-file-name
          (string-append
            (substring input-file-name 0 (- (string-length input-file-name) 4))
            ".locknut")))

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
    (delete-file input-file-name) ))


;decrypt-file
; Called by create-file, decrypt
; Takes an encrypted .locknut file as input and prints the decrypted version of the
; file to a text file.
; If glancing, open the file in notepad and delete when the user is finished.
; Otherwise, rename the decrypted text file to the original name of the input
;
; string, string -> none
;--------------------------------------------------------------------------------
(define (decrypt-file input-file-name password)

  (let (;Get the char-list from the encryped file
        (chars-list (string->list (file->listChars input-file-name)))

        ;Remove .locknut extension and add .txt
        (new-file-name
          (string-append
            (substring input-file-name 0 (- (string-length input-file-name) 8))
            ".txt")))

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

        ;Valid password: print file, show editor, load in editor
        (begin
          (set-curr-file-name new-file-name)

          ;Print file
          (print-this (substring decrypted-file 50 (string-length decrypted-file))
                      "ln_data/tmp.locknut")
          (send file-info set-label (buff-string input-file-name))
          (send text-editor load-file "ln_data/tmp.locknut")
          (send editor-frame show #t)

          ;Cleanup
          (delete-file "ln_data/tmp.locknut"))

        ;Invalid password
        #f)) ))

