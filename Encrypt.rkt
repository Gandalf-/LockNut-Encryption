#lang racket

;----------------------------------------------------------------------
;Encrypt
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;Functions that work between the algorithm and the GUI, handle file IO
;----------------------------------------------------------------------

(require racket/gui)
(require racket/port)
(require "Waterfall.rkt")

;Provisions to LockNut.rkt
(provide decrypt)
(provide create-file)
(provide print-this)

;Globals
(define curr-file-name "")
(define unbuffed-password "")

;----------------------------------------------------------------------
; Waterfall required
;----------------------------------------------------------------------

; Define default-key, key-list and solver-list
(define A '(82 85 71 2 12 37 88 56 29 79 30 27 82 2 91 67 86 94 72 45 96 92 53 84 1
            54 23 56 65 65 78 90 93 9 87 51 10 54 5 67 21 57 62 93 59 58 60 58 3 21))
(define B '(8 65 5 42 29 60 46 48 44 68 64 82 50 86 38 3 67 35 88 4 73 30 51 56 38
            18 72 45 69 17 3 30 16 54 78 40 38 31 23 27 4 98 7 55 45 30 16 9 54 66))
(define C '(60 22 41 53 76 78 6 70 34 25 56 88 37 18 60 67 34 57 52 55 33 27 14 18
            98 40 24 24 52 61 41 17 90 63 39 27 80 55 15 1 20 2 4 96 7 79 37 9 55 17))
(define D '(49 21 59 0 28 31 51 43 45 34 44 32 28 94 0 10 69 30 72 2 12 28 86 53 91
            84 6 32 17 51 78 56 58 12 6 39 51 54 14 37 65 8 43 88 2 8 30 87 16 13))
(define E '(75 27 9 31 28 66 63 29 68 46 10 2 16 20 13 58 24 15 13 29 85 17 70 9 62
            76 67 59 93 60 22 88 99 6 67 32 11 91 89 83 58 83 41 37 6 20 9 43 12 84))

; Default key, 250 (* 50 5), random integers [0-100]
(define default-key (append A B C D E))
(define default-password "6AQO*fvr*RQ7Uv!mCnPc8vxKdia45a$uh'7B5K06Rcj863RMyg")

; Both uniquely assigned during encrypt/decrypt
(define key-list '() )
(define solver-list '() )

;Gets the personal key from file
;
; string -> list of integers
(define (get-personal-key)
  (map
    string->number
    (string-split (file->listChars "ln_data/PersonalKey.locknut"))))


;Take the inverse of the encryption key to make the decryption key
;Creates the inverse of the key-list for solving
;
; list of integers -> list of integers
(define (create-solver key-list)
  (map (lambda (x) (* -1 x)) key-list))


;Make encryption key
;Generate a key-list (list of integers indicating shift amounts) from the given password
;
; string -> list of integers
(define (password->key-list password)
  (map char->integer (string->list password)))


;Add the password-key-list to the default key-list, to make it unique to the given password
;
; list of integers -> list of integers
(define (alter-key-list default-key-list pass-input-list)
  (if (< (length pass-input-list)
         (length default-key-list))
    (alter-key-list
      default-key-list
      (append pass-input-list pass-input-list))

    (map
      (lambda (x y) (+ x y))
      default-key-list
      (take pass-input-list (length default-key-list)))))


; Checks for blank password, then generates the both the encryption and decryption
; keys. These aren't passed out. They're values are set to key-list and solver-list,
; predefined variables. This is done because they're needed in multiple places and
; between mutliple runs of these functions
;
; string, bool, bool -> string
(define (generate-key-and-solver password password-is-key?-value shareable?)

  ;Determine which base-key to use. Default for shareable, Personal for not shareable
  (let ((base-key '()))
    (if (equal? #t shareable?)
      ;Use the default, shareable
      (set! base-key default-key)

      ;Use personal key, non-shareable
      (set! base-key (get-personal-key)))

    ;Check if password is being used as the cipher key
    (if password-is-key?-value

      ;Use password as key
      (begin
        (set! key-list (password->key-list password))
        (set! solver-list (create-solver key-list)))

      ;Use default key, which includes the password
      (begin
        ;Modify the key-list with the password
        (set! key-list (alter-key-list base-key (password->key-list password)))
        (set! solver-list (create-solver key-list))) )

    ;Return password in case it's been set to the default value
    password ))


;----------------------------------------------------------------------
;FILE IO
;----------------------------------------------------------------------

;Moves the file into a string
;
; string -> string
(define (file->listChars filename)
  (letrec ((in (open-input-file filename))
           (out (port->string in)))
    (close-input-port in)
    out))


;Creates a new UTF8 .txt file by copying a blank copy
;
; string -> none
(define (init-file name)
  (when (file-exists? name)
    (delete-file name))
  (copy-file "ln_support.txt" name #f))


;Prints x to a file
; any, string -> none
(define (print-this x name)
  (call-with-output-file* name #:exists 'replace
                          (lambda (output-port)
                            (display x output-port))))

;----------------------------------------------------------------------
;HELPERS
;----------------------------------------------------------------------

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
          (set! curr-file-name new-file-name)

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


;CALLERS
;================================================================

;decrypt
; CALLS DECRYPT-FILE
; Checks the filename and passes info back up
;
; string, bool, bool -> string
;---------------------------------------------
(define (decrypt password password-is-key?-value shareable?)
  ;Save password in case the user wants to re-encrypt or decrypt
  (set! unbuffed-password password)

  ;Buffer password, then generate the cipher key-list and solver-list
  (set! password
    (generate-key-and-solver
      (buff-password password)
      password-is-key?-value
      shareable?))

  ;Get file choice
  (let ((chosen-file (get-file)))

    ;Check if file choice is valid
    (if (equal? chosen-file #f)
      "No file chosen"
      (begin
        (set! chosen-file (path->string chosen-file))

        ;Check if file is .locknut extension
        (if (equal? ".locknut"
                    (substring chosen-file (- (string-length chosen-file) 8)))

          ; Check if decryption was successful
          (if (decrypt-file chosen-file password)
            "Finished decrypting!"
            "Invalid password or incorrect base-key")

          "File must be encrypted .locknut file") )) ))


;CREATE NEW ENCRYPTED FILE
;CALLS ENCRYPT
;
; string, string, bool, bool -> none
;-----------------------------------------------
(define (create-file given-file-name password password-is-key?-value shareable?)

  ;Buffer password and generate the cipher key-list and solver-list
  (set! password
    (generate-key-and-solver
      (buff-password password)
      password-is-key?-value
      shareable?))

  ;Encrypt file
  (encrypt-file given-file-name password))


;----------------------------------------------
; BUILT IN TEXT EDITOR
;----------------------------------------------
  ; Top level frame for editor
  ;
  ; gui frame
  (define editor-frame
    (new frame%
         (label "LockNut Editor")
         (width 500)
         (height 400)))

  (send editor-frame create-status-line)

  ; Panel for editor text fields
  ;
  ; gui v pannel
  (define editor-info-panel
    (instantiate
      vertical-panel% (editor-frame)
      (stretchable-height #f) ))

  ; Displays the filename of the decrypted file
  ;
  ; gui message
  (define file-info
    (new message%
         (label "Filename: ")
         (parent editor-info-panel)
         (stretchable-width #t)
         (auto-resize #t) ))

  (define editor-canvas
    (new editor-canvas% (parent editor-frame)))

  (define text-editor (new text%))
  (define mb (new menu-bar% [parent editor-frame]))
  (define m-edit (new menu% [label "Edit"] [parent mb]))
  (define m-font (new menu% [label "Font"] [parent mb]))

  (append-editor-operation-menu-items m-edit #f)
  (append-editor-font-menu-items m-font)
  (send editor-canvas set-editor text-editor)

  ; Panel for editor buttons
  ;
  ; gui h panel
  (define editor-button-panel
    (instantiate
      horizontal-panel% (editor-frame)
      (stretchable-height #f) ))

  ; Save and reencrypt the changes to the file
  ;
  ; gui button
  (define editor-reencrypt
    (new button%
         (label "Encrypt changes")
         (parent editor-button-panel)
         (callback
           (lambda (b e)
             (send editor-frame set-status-text "Encrypting...")

             ;Save file as .txt
             (send text-editor save-file curr-file-name 'text)

             ;Encrypt: Print .locknut, delete .txt
             (encrypt-file curr-file-name (buff-password unbuffed-password))
             (send editor-frame set-status-text
                   "Encryption finished. Original deleted.") )) ))

  ;Save to plain text
  ;
  ; gui button
  (define editor-decrypt
    (new button%
         (label "Save to plaintext")
         (parent editor-button-panel)
         (callback
           (lambda (b e)
             (send editor-frame set-status-text
                   "File saved. Encrypted version deleted.")

             ;Save to .txt
             (send text-editor save-file curr-file-name 'text)

             ;Delete the .locknut
             (let ((lockname
                     (string-append
                       (substring curr-file-name 0 (- (string-length curr-file-name) 4))
                       ".locknut")))
               (when (file-exists? lockname)
                 (delete-file lockname))) )) ))

  ;Close
  ;
  ; gui button
  (define editor-close
    (new button%
         (label "Close")
         (parent editor-button-panel)
         (callback
           (lambda (b e)
             ;(send t save-file file-name 'text)
             (send editor-frame show #f)))))
