#lang racket

;LockNut
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;GUI
;--------------------------------------------

(require racket/gui)
(require "Encrypt.rkt")

;GUI SETUP
;---------------------------------------------

(define my-font (make-object font%
                  10
                  'modern))

(define help-mode #f)
(define wait-time 1.5)

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

;Panel for options
(define lower-panel (instantiate horizontal-panel% (main-frame)
                      (stretchable-height #f)
                      ))

(define lowest-panel (instantiate horizontal-panel% (main-frame)
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
                   (if (equal? help-mode #f)
                       (begin
                         ;Run encrypt/decrypt, update status text
                         (send main-frame set-status-text 
                               (run-encrypt-decrypt (send passcode-field get-value)
                                                    (send password-is-key-checkbox get-value)))
                         ;Push password changes into other passcode fields
                         (send create-file-passcode-field set-value (send passcode-field get-value)))
                       ;Help mode
                       (begin
                         (send main-frame set-status-text "Choose a text file to be encrypted...")
                         (sleep/yield wait-time)
                         (send main-frame set-status-text " or a .locknut file to be unencrypted")
                         (sleep/yield wait-time)
                         (send main-frame set-status-text " ")
                         (set! help-mode #f)))
                   ))
       
       ))

;Button for glance procedure
(define glance-button
  (new button%
       (label "Glance")
       (parent panel)
       (horiz-margin 8)
       (callback (lambda (button event)
                   (if (equal? help-mode #f)
                       (begin
                         ;Run glance, update status text
                         (send main-frame set-status-text
                               (glance (send passcode-field get-value)
                                       (send password-is-key-checkbox get-value)))
                         ;Push password changes into other passcode fields
                         (send create-file-passcode-field set-value (send passcode-field get-value)))
                       ;Help mode
                       (begin
                         (set! help-mode #f)
                         (send main-frame set-status-text "Choose a .locknut file to view unecrypted...")
                         (sleep/yield wait-time)
                         (send main-frame set-status-text " but keep the file itself encrypted.")
                         (sleep/yield wait-time)
                         (send main-frame set-status-text " ")))
                   ))
       ))

;CREATE FILE
;----------------------------------------------------------------------
(define create-file-frame
  (new frame%
       (label "New Encrypted File")
       (min-width 350)
       (min-height 150)
       ))

(send create-file-frame create-status-line)

;Button that opens create-file-frame, MAIN-FRAME
(define create-file-button
  (new button%
       (label "Create New File")
       (parent lower-panel)
       (horiz-margin 30)
       (callback (lambda (button event)
                   (if (equal? help-mode #f)
                       (send create-file-frame show #t)
                       ;Help mode
                       (begin
                         (set! help-mode #f)
                         (send main-frame set-status-text "Opens window with options for creating a...")
                         (sleep/yield wait-time)
                         (send main-frame set-status-text "new encrypted file right now")
                         (sleep/yield wait-time)
                         (send main-frame set-status-text " ")))
                   ))
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
                   (let ((file-name (send filename-field get-value))
                         (password (send create-file-passcode-field get-value))
                         (pass-key? (send password-is-key-checkbox get-value)))
                     ;Run encrypt/decrypt, update status text
                     (send create-file-frame set-status-text 
                           (create-file file-name
                                        password
                                        pass-key?
                                        ))
                     (send main-frame set-status-text "File created and encrypted!")
                     )
                   ;Hide create-file window
                   (send create-file-frame show #f)
                   ;Password changes onto other passcode fields
                   (send passcode-field set-value (send create-file-passcode-field get-value))
                   ))
       ))

;Message explaining details
(define create-file-message
  (new message%
       (label "Filename: name of the new encrypted file \nPassword: optional password used during encryption \n Create : Creates, opens file. Make your edits\n and save, then close notepad. File is now encrypted.")
       (font my-font)
       (parent create-file-frame)
       ))

;Button that opens advanced options window
(define options-button
  (new button%
       (label "Options")
       (parent lower-panel)
       (horiz-margin 8)
       (callback (lambda (button event)
                   (if (equal? help-mode #f)
                   (send options-frame show #t)
                   ;Help mode
                   (begin
                     (set help-mode #f)
                     (send main-frame set-status-text "Opens window with advanced options")
                     (sleep/yield wait-time)
                     (send main-frame set-status-text " ")))
                   ))
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
                       (send options-frame set-status-text "Encryption key set back to default values"))))
       ))

;Button that displays info, MAIN-FRAME
(define info-button
  (new button%
       (label "Information")
       (parent lowest-panel)
       (horiz-margin 40)
       (callback (lambda (button event)
                   (if (equal? help-mode #f)
                       (send info-frame show #t)
                       ;Help mode
                       (begin
                         (send main-frame set-status-text "Open window with program information")
                         (sleep/yield wait-time)
                         (send main-frame set-status-text " ")
                         (set! help-mode #f)))
                   ))
       ))

;Help button, MAIN-FRAME
(define help-button
  (new button%
       (label "Help Mode")
       (parent lowest-panel)
       (horiz-margin 3)
       (callback (lambda
                     (button event)
                   (set! help-mode #t)
                   (send main-frame set-status-text "Click on a button for help")))
       ))

;INFORMATION WINDOW
;-----------------------------------------------
(define info-frame
  (new frame%
       (label "Information")
       (width 200)
       (height 100)
       (stretchable-width #f)
       (stretchable-height #f)
       ))

(define info-message
  (new message%
       (label "LockNut: personal file encryption program  \n Austin Voecks, Summer 2014")
       (font my-font)
       (parent info-frame)
       ))

(define readme-button
  (new button%
       (label "View readme")
       (parent info-frame)
       (horiz-margin 35)
       (callback (lambda (b e)
                   (system "notepad.exe LockNutReadme.txt")))
       ))

(send main-frame create-status-line)
(send main-frame show #t)
