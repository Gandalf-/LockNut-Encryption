#lang racket

;LockNut
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;--------------------------------------------
;GUI
;--------------------------------------------

(require racket/gui)
(require "Encrypt_GUI.rkt")

(define my-font (make-object font% 10 'modern))
(define help-mode #f)
(define wait-time 1.5)

;----------------------------------------------------------------------
;MAIN-WINDOW
;----------------------------------------------------------------------
(define main-frame 
  (new frame%
       (label "LockNut")
       (stretchable-width #f)
       (stretchable-height #f)))

;Background, custom nut image
(define background-image 
  (new message% [parent main-frame] 
       (label (make-object bitmap% "ln_support/Nut2.jpg"))))

;Panel for file encrypt and decrypt
(define panel 
  (instantiate 
    horizontal-panel% (main-frame)
    (stretchable-height #f)))

;Panel for information and help buttons
(define info-help-panel 
  (instantiate 
    horizontal-panel% (main-frame)
    (stretchable-height #f)))

;Gets optional password from user
(define passcode-field
  (new text-field%
       (label "Password")
       (parent main-frame)
       (font my-font)))

;Button that opens create-file-frame, MAIN-FRAME
(define create-file-button
  (new button%
       (label "Encrypted File")
       (parent panel)
       (horiz-margin 22)
       (callback 
         (lambda (b e)
           (if (equal? help-mode #f)
             (send create-file-frame show #t)

             ;Help mode
             (begin
               (set! help-mode #f)
               (send main-frame set-status-text 
                     "Opens window with options for creating a...")
               (sleep/yield wait-time)
               (send main-frame set-status-text 
                     "new encrypted file right now")
               (sleep/yield wait-time)
               (send main-frame set-status-text " "))) )) ))

;Button for unencrypt procedure
(define decrypt-button
  (new button%
       (label "Decrypt File")
       (parent panel)
       (horiz-margin 10)
       (callback 
         (lambda (b e)
           (if (equal? help-mode #f)
             (begin
               ;Run decrypt, update status text
               (send main-frame set-status-text 
                     "Working...")

               ;Decrypt
               (send main-frame set-status-text
                     (decrypt
                       (send passcode-field get-value)
                       (send password-is-key-checkbox get-value)
                       (send shareable-file-checkbox get-value)))

               ;Push password changes into other passcode fields
               (send create-file-passcode-field set-value
                     (send passcode-field get-value)))

             ;Help mode
             (begin
               (set! help-mode #f)
               (send main-frame set-status-text 
                     "Shows options to decrypt .locknut files")
               (sleep/yield wait-time)
               (send main-frame set-status-text " "))) )) ))


;Button that opens advanced options window
;-> gui button
(define options-button
  (new button%
       (label "Options")
       (parent info-help-panel)
       (horiz-margin 15)
       (callback 
         (lambda (b e)
           (if (equal? help-mode #f)
             (send options-frame show #t)

             ;Help mode
             (begin
               (set help-mode #f)
               (send main-frame set-status-text 
                     "Opens window with advanced options")
               (sleep/yield wait-time)
               (send main-frame set-status-text " "))) )) ))


;Button that displays info, MAIN-FRAME
;-> gui button
(define info-button
  (new button%
       (label "Info")
       (parent info-help-panel)
       (horiz-margin 0)
       (callback 
         (lambda (b e)
           (if (equal? help-mode #f)
             (send info-frame show #t)

             ;Help mode
             (begin
               (send main-frame set-status-text 
                     "Open window with program information")
               (sleep/yield wait-time)
               (send main-frame set-status-text " ")
               (set! help-mode #f))) )) ))


;Help button, MAIN-FRAME
;-> gui button
(define help-button
  (new button%
       (label "Help")
       (parent info-help-panel)
       (horiz-margin 12)
       (callback 
         (lambda
           (b e)
           (set! help-mode #t)
           (send main-frame set-status-text
                 "Click on a button for help")))))


;----------------------------------------------------------------------
;CREATE FILE WINDOW
;----------------------------------------------------------------------
(define create-file-frame
  (new frame%
       (label "New Encrypted File")
       (min-width 500)
       (min-height 450)))

;Panel for create-file text fields
(define create-file-panel-text 
  (instantiate
    vertical-panel% (create-file-frame)
    (stretchable-height #f)))

(define mb (new menu-bar% [parent create-file-frame]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))

(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)
(send create-file-frame create-status-line)

;Filename field
(define create-file-filename-field
  (new text-field%
       (label "Filename")
       (parent create-file-panel-text)
       (font my-font)))

;Create file password
(define create-file-passcode-field
  (new text-field%
       (label "Password")
       (parent create-file-panel-text)
       (font my-font)))

;Editor canvas
(define editor-canvas
  (new editor-canvas%
       (parent create-file-frame)))

;Text editor setup
(define text-field (new text%))
(send editor-canvas set-editor text-field)

;Panel for create-file buttons
(define create-file-panel 
  (instantiate 
    horizontal-panel% (create-file-frame)
    (stretchable-height #f)))

;Generate new file button
(define generate-file-button
  (new button%
       (label "Create and Encrypt")
       (parent create-file-panel)
       (callback 
         (lambda (b e)
           (let ((file-name 
                   (string-append 
                     (send create-file-filename-field get-value) 
                     ".txt"))
                 (password (send create-file-passcode-field get-value))
                 (pass-key? (send password-is-key-checkbox get-value))
                 (shareable? (send shareable-file-checkbox get-value)))

             ;Check for empty file name
             (if (string=? "" file-name)
               (send create-file-frame set-status-text 
                     "File name must not be blank")

               (begin
                 ;Create the file
                 (send text-field save-file file-name 'text)

                 ;Run encrypt/decrypt, update status text
                 (send create-file-frame set-status-text 
                       "Working...")
                 (create-file 
                   file-name password pass-key? shareable?)

                 (send main-frame set-status-text 
                       "Encryption complete")

                 ;Hide create-file window
                 (send create-file-frame show #f)

                 ;Password changes onto other passcode fields
                 (send passcode-field set-value 
                       (send create-file-passcode-field get-value)) )) ))) ))


;Load preexisting file into the editor
(define create-file-load
  (new button%
       (label "Load existing file")
       (parent create-file-panel)
       (callback 
         (lambda (b e)

           ;Get and verify file
           (let ((chosen-file (get-file)))
             (if (equal? chosen-file #f)
               (send create-file-frame set-status-text 
                     "No file chosen")

               (begin
                 (set! chosen-file (path->string chosen-file))

                 (if (equal? ".locknut" 
                             (substring chosen-file (- (string-length chosen-file) 8)))
                   (send create-file-frame set-status-text 
                         "Cannot load .locknut files")

                   (begin
                     ;Load file
                     (send text-field load-file chosen-file)

                     ;Update fields, remove .txt from loaded file name
                     (send create-file-passcode-field set-value
                           (send passcode-field get-value))
                     (send create-file-filename-field set-value
                           (substring chosen-file 0 (- (string-length chosen-file) 4)))
                     (send create-file-frame set-status-text 
                           "File loaded")) ))) ))) ))


;Button that cancels the create-file sequence
(define create-file-cancel
  (new button%
       (label "Cancel")
       (parent create-file-panel)
       (callback 
         (lambda (b e)
           (send create-file-frame show #f))) ))


;----------------------------------------------------------------------
;OPTIONS WINDOW
;----------------------------------------------------------------------
(define options-frame
  (new frame%
       (label "Advanced Options")
       (min-width 350)
       (min-height 150)))
(send options-frame create-status-line)


;Panel for options buttons
(define options-panel 
  (instantiate 
    vertical-panel% (options-frame)
    (stretchable-height #f)))


;Use password as the Vigenere key
(define password-is-key-checkbox
  (new check-box%
       (label "Use password as Vigenere cipher key")
       (parent options-panel)
       (callback 
         (lambda (b e)
           (if (send password-is-key-checkbox get-value)

             ;Use password as key
             (send options-frame set-status-text 
                   "Warning: A short password greatly weakens the encryption")

             ;Use default
             (send options-frame set-status-text 
                   "Encryption key set back to default value")))) ))


;Shareable files use the standard base key instead of personal key
(define shareable-file-checkbox
  (new check-box%
       (label "Encrypt/Decrypt with standard key. Shareable mode.")
       (parent options-panel)
       (callback 
         (lambda (b e)
           (if (send shareable-file-checkbox get-value)

             ;Shareable file
             (begin
               (send options-frame set-status-text 
                     "Encrypt/decrypt with standard base-key.")
               (send main-frame set-status-text 
                     "Standard base-key loaded. Ready."))

             ;Not Shareable
             (begin
               (send options-frame set-status-text 
                     "Encrypt/decrypt with the personal base-key on this system.")
               (send main-frame set-status-text 
                     "Personal base-key loaded. Ready."))))) ))


;Close the window
(define options-close
  (new button%
       (label "Close")
       (parent options-panel)
       (callback 
         (lambda (b e)
           (send options-frame show #f))) ))


;----------------------------------------------------------------------
;INFORMATION WINDOW
;----------------------------------------------------------------------

; top level frame for information window
; => gui frame
(define info-frame
  (new frame%
       (label "Information")
       (width 200)
       (height 100)
       (stretchable-width #f)
       (stretchable-height #f)))

; => gui message
(define info-message
  (new message%
       (label "LockNut: personal file encryption program  \n Austin Voecks, Summer 2014")
       (font my-font)
       (parent info-frame)))

; => gui button
(define readme-button
  (new button%
       (label "View readme")
       (parent info-frame)
       (horiz-margin 35)
       (callback (lambda (b e)
                   (system "notepad.exe README.md")))))

; => gui button
(define info-close
  (new button%
       (label "Close")
       (parent info-frame)
       (horiz-margin 35)
       (callback (lambda (b e)
                   (send info-frame show #f)))))

;----------------------------------------------------------------------
;INIT
;----------------------------------------------------------------------

;Top level frame for init window
; => gui frame
(define init-frame
  (new frame%
       (label "Welcome to LockNut!")
       (width 400)
       (height 800)
       (stretchable-width #f)
       (stretchable-height #f)))


;Background, custom nut image
; => gui message
(define init-background-image 
  (new message% [parent init-frame] 
       (label (make-object bitmap% "ln_support/Nut.jpg"))))


;Open the readme in notepad
; => gui button
(define init-readme-button
  (new button%
       (label "View readme")
       (parent init-frame)
       (horiz-margin 35)
       (callback (lambda (b e)
                   (system "notepad.exe README.md")))))


;Close the init-window, open the main window
; => gui button
(define init-close
  (new button%
       (label "Let's go!")
       (parent init-frame)
       (callback 
         (lambda (b e)
           (send main-frame show #t)
           (send main-frame create-status-line)
           (send init-frame show #f)))))


;Creates a string of 250 random integers [0-200]
; => none -> string
(define (generate-personal-key)
  (build-list 250 (lambda (x) (random 200))))


;Startup sequence. Check for PersonalKey file, otherwise
; run first time setup: Generate personal key, offer readme
; => none -> none
(define (startup)
  (if (file-exists? "ln_data/PersonalKey.locknut")

    ;Assume already ran init sequence
    (begin
      (send main-frame create-status-line)
      (send main-frame show #t)
      (send main-frame set-status-text 
            "Personal key loaded. Ready."))

    ;Create personal key
    (begin
      (send init-frame create-status-line)
      (send init-frame show #t)
      (send init-frame set-status-text 
            "Running first-time initialization sequence...")

      (unless (directory-exists? "ln_data")
        (make-directory "ln_data"))

      (print-this (generate-personal-key) 
                  "ln_data/PersonalKey.locknut")
      (sleep 1.5)
      (send init-frame set-status-text 
            "Personal encryption key generated. Ready."))))

(startup)
