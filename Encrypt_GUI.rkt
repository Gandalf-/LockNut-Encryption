#lang racket

;----------------------------------------------------------------------
;Encrypt
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;Functions that work between the algorithm and the GUI, handle file IO
;----------------------------------------------------------------------

(require racket/gui)
(require racket/port)
(require "Encrypt_Core.rkt")
(require "waterfall/Waterfall.rkt")

;Provisions to LockNut.rkt
(provide decrypt)
(provide create-file)
(provide print-this)
(provide Encrypt_GUI-test)

;Globals
(define curr-file-name "")
(define unbuffed-password "")

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

        ;Remove .locknut extension
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

        ;Valid password: print file, show editor, load in editor
        (begin
          (set! curr-file-name new-file-name)

          ;Print tmp file, open in editor
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

  ;Buffer password, then generate the cipher key-list
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
        (if (and 
              (> (string-length chosen-file) 8)
              (equal? ".locknut"
                      (substring chosen-file (- (string-length chosen-file) 8))))

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

  ;Buffer password and generate the cipher key-list 
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
(define editor-frame
  (new frame%
       (label "LockNut Editor")
       (width 500)
       (height 400)))

(send editor-frame create-status-line)

; Panel for editor text fields
(define editor-info-panel
  (instantiate
    vertical-panel% (editor-frame)
    (stretchable-height #f) ))

; Displays the filename of the decrypted file
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
(define editor-button-panel
  (instantiate 
    horizontal-panel% (editor-frame)
    (stretchable-height #f) ))

; Save and reencrypt the changes to the file
(define editor-reencrypt
  (new button%
       (label "Encrypt changes")
       (parent editor-button-panel)
       (callback 
         (lambda (b e)
           (send editor-frame set-status-text "Encrypting...")

           ;Save file as plain text
           (send text-editor save-file curr-file-name 'text)

           ;Encrypt: Print .locknut, delete original
           (encrypt-file curr-file-name (buff-password unbuffed-password))
           (send editor-frame set-status-text 
                 "Encryption finished. Original deleted.") )) ))

;Save to plain text
(define editor-decrypt
  (new button%
       (label "Save to plaintext")
       (parent editor-button-panel)
       (callback 
         (lambda (b e)
           (send editor-frame set-status-text 
                 "File saved. Encrypted version deleted.")

           ;Save as plain text
           (send text-editor save-file curr-file-name 'text)

           ;Delete the .locknut
           (let ((lockname 
                   (string-append
                     (substring curr-file-name 0 (- (string-length curr-file-name) 4))
                     ".locknut")))
             (when (file-exists? lockname)
               (delete-file lockname))) )) ))

;Close
(define editor-close
  (new button%
       (label "Close")
       (parent editor-button-panel)
       (callback 
         (lambda (b e)
           ;(send t save-file file-name 'text)
           (send editor-frame show #f)))))


; TESTS
; ==========================
(define (Encrypt_GUI-test)
  (displayln "Encrypt-GUI tests complete"))
