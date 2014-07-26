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
                   ;Run encrypt/decrypt, update status text
                   (send main-frame set-status-text 
                         (run-encrypt-decrypt (send passcode-field get-value)
                                              (send password-is-key-checkbox get-value)))
                   ;Push password changes into other passcode fields
                   (send create-file-passcode-field set-value (send passcode-field get-value))
                   ))
       ))

;Button for glance procedure
(define glance-button
  (new button%
       (label "Glance")
       (parent panel)
       (horiz-margin 8)
       (callback (lambda (button event)
                   ;Run glance, update status text
                   (send main-frame set-status-text
                         (glance (send passcode-field get-value)
                                 (send password-is-key-checkbox get-value)))
                   ;Push password changes into other passcode fields
                   (send create-file-passcode-field set-value (send passcode-field get-value))
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
                   (let ((file-name (send filename-field get-value))
                         (password (send create-file-passcode-field get-value))
                         (pass-key? (send password-is-key-checkbox get-value)))
                     ;Run encrypt/decrypt, update status text
                     (send create-file-frame set-status-text 
                           (create-file file-name
                                        password
                                        pass-key?
                                        ))
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
       (label "\nFilename: name of the new encrypted file \nPassword: optional password used during encryption \n Create : Creates, opens file. Make your edits\n and save, then close notepad. File is now encrypted.")
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
                       (send options-frame set-status-text "Encryption key set back to default values"))))
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
       (label "LockNut: personal file encryption program\n\n Protect personal files with a powerful Vigenere \n cipher using password verification.\n\n  Austin Voecks, Summer 2014\n")
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
