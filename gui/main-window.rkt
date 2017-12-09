#lang racket

; main-window
;
; main-window gui elements

(require racket/gui)
(require "../Encrypt.rkt")

(require "common.rkt")
(require "create-file.rkt")
(require "options-window.rkt")
(require "info-window.rkt")

(provide (all-defined-out))

; frames

; messages
(define background-image
  ; background, custom nut image

  (new message% [parent main-frame]
       (label (make-object bitmap% "ln_support/Nut2.jpg"))))


; panels
(define panel
  ;Panel for file encrypt and decrypt

  (instantiate
    horizontal-panel% (main-frame)
    (stretchable-height #f)))

(define info-help-panel
  ;Panel for information and help buttons

  (instantiate
    horizontal-panel% (main-frame)
    (stretchable-height #f)))


; buttons
(define create-file-button
  ;Button that opens create-file-frame, MAIN-FRAME

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
               (help-mode-off)
               (send main-frame set-status-text
                     "Opens window with options for creating a...")
               (sleep/yield wait-time)
               (send main-frame set-status-text
                     "new encrypted file right now")
               (sleep/yield wait-time)
               (send main-frame set-status-text " "))) )) ))

(define decrypt-button
  ;Button for unencrypt procedure

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
               (help-mode-off)
               (send main-frame set-status-text
                     "Shows options to decrypt .locknut files")
               (sleep/yield wait-time)
               (send main-frame set-status-text " "))) )) ))

(define options-button
  ;Button that opens advanced options window

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

(define info-button
  ;Button that displays info, MAIN-FRAME

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
               (help-mode-off))) )) ))

(define help-button
  ;Help button, MAIN-FRAME

  (new button%
       (label "Help")
       (parent info-help-panel)
       (horiz-margin 12)
       (callback
         (lambda
           (b e)
           (help-mode-on)
           (send main-frame set-status-text
                 "Click on a button for help")))))


