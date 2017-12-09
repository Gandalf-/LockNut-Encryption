#lang racket

(require racket/gui)
(require "common.rkt")

(provide (all-defined-out))


; frame

(define options-frame
  (new frame%
       (label "Advanced Options")
       (min-width 350)
       (min-height 150)))
(send options-frame create-status-line)


; panel

(define options-panel
  ;Panel for options buttons

  (instantiate
    vertical-panel% (options-frame)
    (stretchable-height #f)))


; check-box

(define password-is-key-checkbox
  ;Use password as the Vigenere key

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


(define shareable-file-checkbox
  ;Shareable files use the standard base key instead of personal key

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


; button

(define options-close
  ;Close the window

  (new button%
       (label "Close")
       (parent options-panel)
       (callback
         (lambda (b e)
           (send options-frame show #f))) ))

