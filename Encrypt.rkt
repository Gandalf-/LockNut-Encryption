#lang racket

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

;Define default key-list and solver-list
(define default-key '(210 186 204 51 43 67 133 168 222 15 104 36 195 94 168 242 226 50 74 198 185 249 121 133 225 87))
(set! default-key (append default-key '(93 235 133 164 3 215 210 100 5 155 250 2 163 77 244 104 162 199 61 193 226 92 131 241 2 135)))

(define key-list default-key)
(define solver-list (create-solver key-list))

;Generate a seed from the given password
(define (password->seed password)
  (let loop ((input (string->list password)) (output 0))
    (if (empty? input)
        (+ 1 (modulo output 10))
        (loop (cdr input) (+ (char->integer (car input)) output)))))

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
             (seed (password->seed password)))
    
    (if (empty? remaining-input)
        output-list
        (begin
          ;Refill the key-list
          (when (empty? current-key-list)
            (set! current-key-list key-list))
          
          (let ((new-shift-amount (modulo 
                                   (+ (* seed (car current-key-list)) (char->integer
                                                                       (car remaining-input)))
                                   200)))
            
            ;Check for max modulo case, fix if necessary
            (when (and (= 0 new-shift-amount) 
                       (not (= 0 (char->integer (car remaining-input)))))
              (set! new-shift-amount 200))
            
            ;Continue loop
            (loop (flatten (cons output-list
                                 (integer->char new-shift-amount)))
                  (cdr remaining-input)
                  (cdr current-key-list)
                  seed
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
    
    ;Remove the older version if necessary
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
    
    (if (equal? #t glance?)
        ;Decrypt and open in notepad
        (begin
          (print-this (list->string (encrypt chars-list solver-list password)) "temp.txt")
          (system "notepad.exe temp.txt")
          (delete-file "temp.txt"))
        
        ;Decrypt and print
        (begin
          (print-this (list->string (encrypt chars-list solver-list password)) input-file-name)
          
          ;Rename the encrypted version
          (rename-file-or-directory input-file-name new-file-name))
        )))

;GUI SETUP
;---------------------------------------------

(define my-font (make-object font%
                  10
                  'modern))

;MAIN-FRAME
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
       (horiz-margin 10)
       (callback (lambda (button event)
                   ;Get passcode
                   (let ((password (send passcode-field get-value)))
                     
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
                                   (decrypt-file chosen-file password #f)
                                   (send main-frame set-status-text "Finished decrypting!"))
                                 (begin
                                   (send main-frame set-status-text "Encrypting...")
                                   (encrypt-file chosen-file password)
                                   (send main-frame set-status-text "Finished encrypting!")))
                             
                             ))
                       ))
                   ))))

;Decrypts a file using the optional password, but just opens it in notepad. Decrypted file isn't saved
(define glance-button
  (new button%
       (label "Glance")
       (parent panel)
       (horiz-margin 10)
       (callback (lambda (button event)
                   ;Get passcode
                   (let ((password (send passcode-field get-value)))
                     
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
                             (if (equal? ".locknut" (substring chosen-file (- (string-length chosen-file) 8)))
                                 (begin
                                   (send main-frame set-status-text "Decrypting...")
                                   (decrypt-file chosen-file password #t)
                                   (send main-frame set-status-text "Finished decrypting!"))
                                 (send main-frame set-status-text "File must be encrypted .locknut file"))
                             
                             ))
                       )))
                 )))

;OPTIONS WINDOW
;-----------------------------------------------
(define option-frame
  (new frame%
       (label "Advanced Options")
       (min-width 350)
       (min-height 150)
       ))

;Button that opens advanced options window
(define options-button
  (new button%
       (label "Options")
       (parent lower-panel)
       (horiz-margin 35)
       (callback (lambda (button event)
                   (send option-frame show #t)))
       ))

;Panel for options buttons
(define options-panel (instantiate vertical-panel% (option-frame)
                        (stretchable-height #f)
                        ))

;Checkbox
(define password-is-key-checkbox
  (new check-box%
       (label "Use password as Vigenere cipher key")
       (parent options-panel)
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
       (horiz-margin 25)
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



