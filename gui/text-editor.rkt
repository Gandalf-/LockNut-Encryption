#lang racket

; text-editor
;
; built in text editor

(require racket/gui)
(require "../alg/encryption.rkt")
(require "../common.rkt")
(require "common.rkt")

(provide (all-defined-out))



(send editor-frame create-status-line)


(define editor-canvas
  (new editor-canvas% (parent editor-frame)))

(define mb (new menu-bar% [parent editor-frame]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))

(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)
(send editor-canvas set-editor text-editor)

; Panel for editor buttons
;
; gui h panel
(define editor-button-panel
  (instantiate
    horizontal-panel% (editor-frame)
    (stretchable-height #f) ))

; Save and reencrypt the changes to the file
;
; gui button
(define editor-reencrypt
  (new button%
       (label "Encrypt changes")
       (parent editor-button-panel)
       (callback
         (lambda (b e)
           (send editor-frame set-status-text "Encrypting...")

           ;Save file as .txt
           (send text-editor save-file curr-file-name 'text)

           ;Encrypt: Print .locknut, delete .txt
           (encrypt-file curr-file-name (buff-password unbuffered-password))
           (send editor-frame set-status-text
                 "Encryption finished. Original deleted.") )) ))

;Save to plain text
;
; gui button
(define editor-decrypt
  (new button%
       (label "Save to plaintext")
       (parent editor-button-panel)
       (callback
         (lambda (b e)
           (send editor-frame set-status-text
                 "File saved. Encrypted version deleted.")

           ;Save to .txt
           (send text-editor save-file curr-file-name 'text)

           ;Delete the .locknut
           (let ((lockname
                   (string-append
                     (substring curr-file-name 0 (- (string-length curr-file-name) 4))
                     ".locknut")))
             (when (file-exists? lockname)
               (delete-file lockname))) )) ))

;Close
;
; gui button
(define editor-close
  (new button%
       (label "Close")
       (parent editor-button-panel)
       (callback
         (lambda (b e)
           ;(send t save-file file-name 'text)
           (send editor-frame show #f)))))
