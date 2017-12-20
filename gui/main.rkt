#lang racket

; main
;
; main-window gui elements

(require racket/gui)
(require "../core.rkt")
(require "common.rkt")
(require "create.rkt")
(require "options.rkt")
(require "info.rkt")

(provide (all-defined-out))


; message

(define background-image
  ; background, custom nut image

  (new message% [parent main-frame]
       (label (make-object bitmap% "ln_support/Nut2.jpg"))))


; panel

(define crypto-panel
  ; panel for create-file and decrypt buttons

  (instantiate
    horizontal-panel% (main-frame)
    (stretchable-height #f)))

(define info-help-panel
  ; panel for info and help buttons

  (instantiate
    horizontal-panel% (main-frame)
    (stretchable-height #f)))


; button

(define create-file-button
  ;Button that opens create-file-frame, MAIN-FRAME

  (new button%
       (label "Encrypted File")
       (parent crypto-panel)
       (horiz-margin 22)
       (callback
         (lambda (b e)
           (if (not help-mode)
             (send create-file-frame show #t)

             (begin
               (help-mode-off)
               (send main-frame set-status-text
                     "Opens window with options for creating a...")
               (sleep/yield wait-time)
               (send main-frame set-status-text
                     "new encrypted file right now")
               (sleep/yield wait-time)
               (send main-frame set-status-text " "))

             )))))

(define decrypt-button
  ;Button for unencrypt procedure

  (new button%
       (label "Decrypt File")
       (parent crypto-panel)
       (horiz-margin 10)
       (callback
         (lambda (b e)
           (if (not help-mode)
             (begin
               ;Run decrypt, update status text
               (send main-frame set-status-text "Working...")

               ;Decrypt
               (send main-frame set-status-text
                     (decrypt
                       (send passcode-field get-value)
                       (send password-is-key-checkbox get-value)
                       (send shareable-file-checkbox get-value)))

               ;Push password changes into other passcode fields
               (send create-file-passcode-field set-value
                     (send passcode-field get-value)))

             (begin
               (help-mode-off)
               (send main-frame set-status-text
                     "Shows options to decrypt .locknut files")
               (sleep/yield wait-time)
               (send main-frame set-status-text " "))

             )))))

(define options-button
  ;Button that opens advanced options window

  (new button%
       (label "Options")
       (parent info-help-panel)
       (horiz-margin 15)
       (callback
         (lambda (b e)
           (if (not help-mode)
             (send options-frame show #t)

             (begin
               (set help-mode #f)
               (send main-frame set-status-text
                     "Opens window with advanced options")
               (sleep/yield wait-time)
               (send main-frame set-status-text " "))

             )))))

(define info-button
  ;Button that displays info, MAIN-FRAME

  (new button%
       (label "Info")
       (parent info-help-panel)
       (horiz-margin 0)
       (callback
         (lambda (b e)
           (if (not help-mode)
             (send info-frame show #t)

             (begin
               (send main-frame set-status-text
                     "Open window with program information")
               (sleep/yield wait-time)
               (send main-frame set-status-text " ")
               (help-mode-off))

             )))))

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

