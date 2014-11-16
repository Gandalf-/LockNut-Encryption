#lang racket

;Encrypt
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;Functions that work between the algorithm and the GUI, handle file IO
;----------------------------------------------------------------------

(require racket/gui)
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
  (call-with-output-file* name #:exists 'replace
                          (lambda (output-port)
                            (display x output-port))))

;HELPERS
;------------
;Buffers the password to 50 characters
(define (buff-password password)
  ;Check if it's longer than 50 characters already
  (if (>= (string-length password) 50)
      (substring password 0 50)
      ;Buffer it using the default password
      (string-append password 
                     (substring default-password 0 (- 50 (string-length password))))
      ))

;Buffers a string >60 characters
(define (buff-string input)
  (let ((str-len (string-length input)))
    (if (>= str-len 60)
        (string-append "..." (substring input (- str-len 60) str-len))
        input)
    ))

;CALLEES
;================================================

;FILE ENCRYPTION
; CALLED BY CREATE-FILE, decrypt
; Takes a file as input and prints the encrypted version of the file to a .locknut file
;--------------------------------------------------------------------------------
(define (encrypt-file input-file-name password)
  
  (let (;Get list of chars from source file
        (chars-list (string->list (file->listChars input-file-name)))
        ;Remove .txt and add .locknut extension
        (new-file-name (string-append (substring input-file-name 0 (- (string-length input-file-name) 4))
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
    ))


;FILE DECRYPTION
; CALLED BY CREATE-FILE, decrypt
; Takes an encrypted .locknut file as input and prints the decrypted version of the 
; file to a text file.
; If glancing, open the file in notepad and delete when the user is finished.
; Otherwise, rename the decrypted text file to the original name of the input
;--------------------------------------------------------------------------------
(define (decrypt-file input-file-name password)
  
  (let (;Get the char-list from the encryped file
        (chars-list (string->list (file->listChars input-file-name)))
        ;Remove .locknut extension and add .txt
        (new-file-name (string-append
                        (substring input-file-name 0 (- (string-length input-file-name) 8))
                        ".txt")))
    
    ;Remove the older version of output file, if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))
    
    ;Decrypt the file with the given buffered password
    (let ((decrypted-file (list->string (encrypt chars-list
                                                 solver-list
                                                 password)) ))
      
      ;Verify password against buffered password, and decrypt
      (if (equal? (substring decrypted-file 0 50) password)
          ;Valid password: print file, show editor, load in editor
          (begin
            (set! curr-file-name new-file-name)
            ;Print file
            (print-this (substring decrypted-file 50 (string-length decrypted-file))
                        "Data/tmp.locknut")
            (send file-info set-label (buff-string input-file-name))
            (send text-editor load-file "Data/tmp.locknut")
            (send editor-frame show #t)
            ;Cleanup
            (delete-file "Data/tmp.locknut"))
          
          ;Invalid password
          #f))
    ))


;CALLERS
;================================================================

;decrypt
; CALLS DECRYPT-FILE
; Checks the filename and passes info back up
;---------------------------------------------
(define (decrypt password password-is-key?-value shareable?)
  ;Save password in case the user wants to re-encrypt
  (set! unbuffed-password password)
  
  ;Buffer password, then generate the cipher key-list and solver-list
  (set! password (generate-key-and-solver (buff-password password)
                                          password-is-key?-value
                                          shareable?))
  ;Get file choice
  (let ((chosen-file (get-file)))
    ;Valid file choice?
    (if (equal? chosen-file #f)
        "No file chosen"
        (begin
          (set! chosen-file (path->string chosen-file))
          ;Check if file is .locknut extension
          (if (equal? ".locknut" (substring chosen-file (- (string-length chosen-file) 8)))
              (begin
                (if (decrypt-file chosen-file password)
                    "Finished decrypting!"
                    "Invalid password or incorrect base-key"))
              
              "File must be encrypted .locknut file")
          ))
    ))


;CREATE NEW ENCRYPTED FILE
;CALLS ENCRYPT
;-----------------------------------------------
(define (create-file given-file-name password password-is-key?-value shareable?)
  
  ;Buffer password and generate the cipher key-list and solver-list
  (set! password (generate-key-and-solver (buff-password password)
                                          password-is-key?-value
                                          shareable?))
  ;Encrypt file
  (encrypt-file given-file-name password)
  )


;NEW WINDOW EDITOR
;----------------------------------------------
(define editor-frame
  (new frame%
       (label "LockNut Editor")
       (width 500)
       (height 400)))

(send editor-frame create-status-line)

;Panel for editor text fields
(define editor-info-panel
  (instantiate vertical-panel% (editor-frame)
    (stretchable-height #f)
    ))

;Displays the filename of the decrypted file
(define file-info
  (new message%
       (label "Filename: ")
       (parent editor-info-panel)
       (stretchable-width #t)
       (auto-resize #t)
       ))

(define editor-canvas
  (new editor-canvas%
       (parent editor-frame)))

(define text-editor
  (new text%))

(define mb (new menu-bar% [parent editor-frame]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))
(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)


(send editor-canvas set-editor text-editor)

;Panel for editor buttons
(define editor-button-panel
  (instantiate horizontal-panel% (editor-frame)
    (stretchable-height #f)
    ))

;Save and reencrypt the changes to the file
(define editor-reencrypt
  (new button%
       (label "Encrypt changes")
       (parent editor-button-panel)
       (callback (lambda (b e)
                   (send editor-frame set-status-text "Encrypting...")
                   ;Save file as .txt
                   (send text-editor save-file curr-file-name 'text)
                   ;Encrypt: Print .locknut, delete .txt
                   (encrypt-file curr-file-name (buff-password unbuffed-password))
                   (send editor-frame set-status-text "Encryption finished. Original deleted.")
                   ))
       ))

;Save to plain text
(define editor-decrypt
  (new button%
       (label "Save to plaintext")
       (parent editor-button-panel)
       (callback (lambda (b e)
                   (send editor-frame set-status-text "File saved. Encrypted version deleted.")
                   ;Save to .txt
                   (send text-editor save-file curr-file-name 'text)
                   ;Delete the .locknut
                   (let ((lockname (string-append (substring curr-file-name 0 (- (string-length curr-file-name) 4))
                                                  ".locknut")))
                     (when (file-exists? lockname)
                       (delete-file lockname)))
                   ))
       ))

;Close
(define editor-close
  (new button%
       (label "Close")
       (parent editor-button-panel)
       (callback (lambda (b e)
                   ;(send t save-file file-name 'text)
                   (send editor-frame show #f)))))







