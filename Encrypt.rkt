#lang racket

;LockNut Encryption Program
;Austin Voecks, Summer 2014
;----------------------------------------------------------------------

(require racket/gui)
(require racket/port)

;FILE IO
;----------------------------------------------------------------------

;Moves the file into a string
(define (file->listChars filename)
  (port->string (open-input-file filename)))

;Prints x to a file
(define (print-this x name)
  (call-with-output-file* name #:exists 'replace
                          (lambda (output-port)
                            (display x output-port))))


;ENCRYPTION SETUP
;----------------------------------------------------------------------

;Creates the inverse of the key-list for solving
(define (create-solver key-list)
  (let loop ((input key-list) (output '()))
    (if (empty? input)
        output
        (loop (cdr input) (flatten (cons output (* -1 (car input))))))))

;Define default-key, key-list and solver-list
(define A '(28 72 99 85 89 21 40 86 42 26 23 14 50 6 28 51 79 7 34 90 63 90 37 81 18))
(define B '(97 40 51 70 55 53 97 64 12 96 19 71 16 7 9 60 26 5 46 13 96 60 89 85 81))
(define C '(84 10 89 95 91 90 83 69 57 33 72 29 46 10 77 49 89 82 41 0 5 96 20 69 99))
(define D '(61 23 76 35 86 47 3 91 6 79 39 56 31 77 54 96 90 20 81 67 31 65 65 56 61))

(define default-key (append A B C D))
(define key-list default-key)
(define solver-list (create-solver key-list))

;Generate a key-list from the given password
(define (password->key-list password)
  (let loop ((input (string->list password)) (output '() ))
    (if (empty? input)
        output
        (loop (cdr input) (flatten (cons output (char->integer (car input))))))))


;ENCRYPTION ALGORITHM
;----------------------------------------------------------------------
; Takes a list of characters and returns a list of encrypted characters  
(define (encrypt input-list key-list password)
  
  ;Run algorithm
  (let loop ((output-list '() )
             (remaining-input input-list)
             (current-key-list key-list)
             )
    
    (if (empty? remaining-input)
        output-list
        (begin
          ;Refill the key-list
          (when (empty? current-key-list)
            (set! current-key-list key-list))
          
          ;Create the new shift amount from the key-list
          (let ((new-shift-amount (+ (car current-key-list) 
                                     (char->integer (car remaining-input)))))
            
            ;Continue loop
            (loop (flatten (cons output-list
                                 (integer->char new-shift-amount)))
                  (cdr remaining-input)
                  (cdr current-key-list)
                  )))
        )))


;FILE ENCRYPTION
; Takes a file as input and prints the encrypted version of the file to a text file
;--------------------------------------------------------------------------------
(define (encrypt-file input-file-name password)
  
  (let (;Get list of chars from source file
        (chars-list (string->list (file->listChars input-file-name)))
        ;Add .locknut extension
        (new-file-name (string-append input-file-name ".locknut")))
    
    ;If checking password, add the password onto the beginning of the chars-list
    (when (send check-password-checkbox get-value)
      (set! chars-list (append (string->list password) chars-list)))
    
    ;Remove the older version of the output file if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))
    
    ;Encrypt and print
    (print-this (list->string (encrypt chars-list key-list password)) input-file-name)
    
    ;Rename the input file
    (rename-file-or-directory input-file-name new-file-name))
  )


;FILE DECRYPTION
; Takes an encrypted .locknut file as input and prints the decrypted version of the file to a text file
;--------------------------------------------------------------------------------
(define (decrypt-file input-file-name password glance?)
  
  (let (;Get the char-list from the encryped file
        (chars-list (string->list (file->listChars input-file-name)))
        ;Remove .locknut extension
        (new-file-name (substring input-file-name 0 (- (string-length input-file-name) 8))))
    
    ;Remove the older version of output file, if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))
    
    ;Start decryption 
    (let ((decrypted-file (list->string (encrypt chars-list solver-list password)) ))
      
      (if (equal? #t glance?)
          ;Glance : decrypt and open in notepad
          (if (send check-password-checkbox get-value)
              
              ;Verify password
              (if (equal? (substring decrypted-file 0 (string-length password)) password)
                  
                  ;Valid password, remove password from beginning and show in notepad
                  (begin
                    (print-this (substring decrypted-file (string-length password) (string-length decrypted-file))  "temp.txt")
                    (system "notepad.exe temp.txt")
                    (delete-file "temp.txt"))
                  
                  ;Invalid password
                  #f)
              
              ;Don't verify password
              (begin
                ;Remove password from beginning and show in notepad
                (print-this (substring decrypted-file (string-length password) (string-length decrypted-file))  "temp.txt")
                (system "notepad.exe temp.txt")
                (delete-file "temp.txt")))
          
          ;Don't glance : decrypt and print
          (if (send check-password-checkbox get-value)
              
              ;Verify password
              (if (equal? (substring decrypted-file 0 (string-length password)) password)
                  
                  ;Valid password
                  (begin
                    (print-this (substring decrypted-file (string-length password) (string-length decrypted-file)) input-file-name)
                    ;Rename the encrypted version
                    (rename-file-or-directory input-file-name new-file-name))
                  
                  ;Invalid password
                  #f)
              
              ;Don't verify password
              (begin
                (print-this (substring decrypted-file (string-length password) (string-length decrypted-file)) input-file-name)
                ;Rename the encrypted version
                (rename-file-or-directory input-file-name new-file-name))
              )
          ))
    ))


;RUN-ENCRYPT-DECRYPT
; Checks the password field, prompts the user to
; choose a file, detect where to encrypt or decrypt,
; pass user options along to encrypt()
;---------------------------------------------
(define (run-encrypt-decrypt)
  ;Get passcode
  (letrec ((password (send passcode-field get-value))
           (no-pass? (equal? password "")))
    
    ;If password is blank, set it to a dummy value so verification works properly
    (when no-pass?
      (set! password "Qa30EfdB5h"))
    
    ;Check if password is being used as the cipher key
    (if (send password-is-key-checkbox get-value)
        ;Use password as key
        (begin
          (set! key-list (password->key-list (send passcode-field get-value)))
          (set! solver-list (create-solver key-list)))
        ;Use default
        (begin
          (set! key-list default-key)
          (set! solver-list (create-solver key-list))))
    
    ;Get file choice
    (let ((chosen-file (get-file)))
      (if (equal? chosen-file #f)
          (send main-frame set-status-text "No file chosen")
          (begin
            (set! chosen-file (path->string chosen-file))
            
            ;Decrypt .locknut files, encrypt all other files
            (if (equal? ".locknut" (substring chosen-file (- (string-length chosen-file) 8)))
                
                (begin
                  (send main-frame set-status-text "Decrypting...")
                  ;Run decryption
                  (if (decrypt-file chosen-file password #f)
                      ;Success
                      (send main-frame set-status-text "Finished decrypting!")
                      ;Invalid password, nothing done
                      (send main-frame set-status-text "Invalid password")))
                
                (begin
                  (send main-frame set-status-text "Encrypting...")
                  (encrypt-file chosen-file password)
                  (send main-frame set-status-text "Finished encrypting!")))
            
            ))
      )))

;GLANCE
; Decrypts a file using the optional password,
; but just opens it in notepad. Decrypted file isn't saved
;---------------------------------------------
(define (glance)
  ;Get passcode
  (letrec ((password (send passcode-field get-value))
           (no-pass? (equal? password "")))
    
    ;If password is blank, set it to a dummy value so verification works properly
    (when no-pass?
      (set! password "Qa30EfdB5h"))
    
    ;Check if password is being used as the cipher key
    (if (send password-is-key-checkbox get-value)
        ;Use password as key
        (begin
          (set! key-list (password->key-list (send passcode-field get-value)))
          (set! solver-list (create-solver key-list)))
        ;Use default
        (begin
          (set! key-list default-key)
          (set! solver-list (create-solver key-list))))
    
    ;Get file choice
    (let ((chosen-file (get-file)))
      (if (equal? chosen-file #f)
          (send main-frame set-status-text "No file chosen")
          (begin
            (set! chosen-file (path->string chosen-file))
            
            ;Decrypt .locknut files, open in notepad
            ;Check if file is .locknut extension
            (if (equal? ".locknut" (substring chosen-file (- (string-length chosen-file) 8)))
                (begin
                  (send main-frame set-status-text "Decrypting...")
                  (if (decrypt-file chosen-file password #t)
                      ;Success
                      (send main-frame set-status-text "Finished decrypting!")
                      ;Invalid password, nothing done
                      (send main-frame set-status-text "Invalid password")))
                ;Wrong extension
                (send main-frame set-status-text "File must be encrypted .locknut file"))
            
            ))
      ))
  )

;GUI SETUP
;---------------------------------------------

(define my-font (make-object font%
                  10
                  'modern))

;MAIN-WINDOW
;---------------------------------------------
(define main-frame 
  (new frame%
       (label "LockNut Encryption")
       (stretchable-width #f)
       (stretchable-height #f)
       ))

;Background, custom nut image
(define background-image (new message% [parent main-frame] 
                              (label (make-object bitmap% "Nut.jpg"))))

;Panel for file encryption or decryption, glance, and passcode-field
(define panel (instantiate horizontal-panel% (main-frame)
                (stretchable-height #f)
                ))

;Panel for options and info
(define lower-panel (instantiate horizontal-panel% (main-frame)
                      (stretchable-height #f)
                      ))

;Gets optional password from user
; Default is to generate a seed that alters the encryption algorithm's output
; Option in option-panel to use password as the encryption key
(define passcode-field
  (new text-field%
       (label "Password")
       (parent main-frame)
       (font my-font)
       ))

;Button for file encryption or decryption
(define run-button
  (new button% 
       (label "Encrypt/Decrypt File")
       (parent panel)
       (horiz-margin 17)
       (callback (lambda (button event)
                   (run-encrypt-decrypt)))
       ))

;Button for glance procedure
(define glance-button
  (new button%
       (label "Glance")
       (parent panel)
       (horiz-margin 8)
       (callback (lambda (button event)
                   (glance)))
       ))

;CREATE NEW ENCRYPTED FILE
;-----------------------------------------------
(define (create-file)
  (let ((given-file-name (send filename-field get-value)))
    ;Check if file name is not blank
    (if (equal? given-file-name "")
        ;Invalid file name
        (send create-file-frame set-status-text "File name must not be blank")
        
        ;Check if file name already exists
        (if (file-exists? given-file-name)
            (send create-file-frame set-status-text "File of that name already exists")
            
            (begin
              (letrec ((full-file-name (string-append given-file-name ".txt"))
                       (password (send create-file-passcode-field get-value))
                       (no-pass? (equal? password "")))
                
                ;If password is blank, set it to a dummy value so verification works properly
                (when no-pass?
                  (set! password "Qa30EfdB5h"))
                
                ;Check if password is being used as the cipher key
                (if (send password-is-key-checkbox get-value)
                    ;Use password as key
                    (begin
                      (set! key-list (password->key-list (send passcode-field get-value)))
                      (set! solver-list (create-solver key-list)))
                    ;Use default
                    (begin
                      (set! key-list default-key)
                      (set! solver-list (create-solver key-list))))
                
                ;Create file
                (print-this "" full-file-name)
                ;Open file
                (system (string-append "notepad.exe " full-file-name))
                ;Hide create-file window
                (send create-file-frame show #f)
                
                (send main-frame set-status-text "Encrypting...")
                (encrypt-file full-file-name password)
                (send main-frame set-status-text "Finished encrypting!")
                ))
            ))
    ))

(define create-file-frame
  (new frame%
       (label "New Encrypted File")
       (min-width 350)
       (min-height 150)
       ))

(send create-file-frame create-status-line)

;Button that opens create-file-frame
(define create-file-button
  (new button%
       (label "Create File")
       (parent lower-panel)
       (horiz-margin 8)
       (callback (lambda (button event)
                   (send create-file-frame show #t)))
       ))

;Panel for create-file buttons
(define create-file-panel (instantiate vertical-panel% (create-file-frame)
                            (stretchable-height #f)
                            ))

;Filename field
(define filename-field
  (new text-field%
       (label "Filename")
       (parent create-file-panel)
       (font my-font)
       ))

;Create file password
(define create-file-passcode-field
  (new text-field%
       (label "Password")
       (parent create-file-panel)
       (font my-font)
       ))

;Generate new file button
(define generate-file-button
  (new button%
       (label "Create")
       (parent create-file-panel)
       (callback (lambda (button event)
                   (create-file)))
       ))

;Message explaining details
(define create-file-message
  (new message%
       (label "\nSample text")
       (font my-font)
       (parent create-file-frame)
       ))


;OPTIONS WINDOW
;-----------------------------------------------
(define options-frame
  (new frame%
       (label "Advanced Options")
       (min-width 350)
       (min-height 150)
       ))

(send options-frame create-status-line)

;Button that opens advanced options window
(define options-button
  (new button%
       (label "Options")
       (parent lower-panel)
       (horiz-margin 8)
       (callback (lambda (button event)
                   (send options-frame show #t)))
       ))

;Panel for options buttons
(define options-panel (instantiate vertical-panel% (options-frame)
                        (stretchable-height #f)
                        ))

;Use password as the Vigenere key
(define password-is-key-checkbox
  (new check-box%
       (label "Use password as Vigenere cipher key")
       (parent options-panel)
       (callback (lambda (button event)
                   (if (send password-is-key-checkbox get-value)
                       ;Use password as key
                       (send options-frame set-status-text "Warning: A short password greatly weakens the encryption")
                       ;Use default
                       (send options-frame set-status-text "Encryption key set back to default value"))))
       ))

;Check password
(define check-password-checkbox
  (new check-box%
       (label "Check password before decryption")
       (parent options-panel)
       (value #t)
       (callback (lambda (button event)
                   (if (send check-password-checkbox get-value)
                       ;Use password as key
                       (send options-frame set-status-text "Will verify password used to encrypt before decryption")
                       ;Use default
                       (send options-frame set-status-text "Warning: incorrect password may ruin the file permanently."))))
       ))


;INFORMATION WINDOW
;-----------------------------------------------
(define info-frame
  (new frame%
       (label "Information")
       (min-width 200)
       (min-height 100)
       ))

;Button that displays info
(define info-button
  (new button%
       (label "Info")
       (parent lower-panel)
       (horiz-margin 8)
       (callback (lambda (button event)
                   (send info-frame show #t)))
       ))

(define info-message
  (new message%
       (label "LockNut: personal file encryption program\n\n Protect personal files with a powerful vigenere cipher\n and optional password for additional strength.\n\n  Austin Voecks, Summer 2014")
       (font my-font)
       (parent info-frame)
       ))

(send main-frame create-status-line)
(send main-frame show #t)



