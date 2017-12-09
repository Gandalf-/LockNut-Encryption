#lang racket

(require racket/gui)

(require "../Encrypt.rkt")
(require "common.rkt")
(require "options.rkt")

(provide (all-defined-out))


; frame

(define create-file-frame
  (new frame%
       (label "New Encrypted File")
       (min-width 500)
       (min-height 450)))

(send create-file-frame create-status-line)


; panel

(define create-file-panel-text
  ;Panel for create-file text fields

  (instantiate
    vertical-panel% (create-file-frame)
    (stretchable-height #f)))

(define create-file-panel
  ;Panel for create-file buttons

  (instantiate
    horizontal-panel% (create-file-frame)
    (stretchable-height #f)))


; text editor

(define mb (new menu-bar% [parent create-file-frame]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))

(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)


; text-field

(define create-file-filename-field
  ;Filename field

  (new text-field%
       (label "Filename")
       (parent create-file-panel-text)
       (font my-font)))

(define create-file-passcode-field
  ;Create file password

  (new text-field%
       (label "Password")
       (parent create-file-panel-text)
       (font my-font)))


; editor canvas

(define editor-canvas
  ;Editor canvas

  (new editor-canvas%
       (parent create-file-frame)))

(define text-field (new text%))
(send editor-canvas set-editor text-field)


; button

(define generate-file-button
  ;Generate new file button

  (new button%
       (label "Create and Encrypt")
       (parent create-file-panel)
       (callback
         (lambda (b e)
           (let ((file-name
                   (string-append
                     (send create-file-filename-field get-value) ".txt"))

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


(define create-file-load
  ;Load preexisting file into the editor

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
                             (substring chosen-file
                                        (- (string-length chosen-file) 8)))
                   (send create-file-frame set-status-text
                         "Cannot load .locknut files")

                   (begin
                     ;Load file
                     (send text-field load-file chosen-file)

                     ;Update fields, remove .txt from loaded file name
                     (send create-file-passcode-field set-value
                           (send passcode-field get-value))

                     (send create-file-filename-field set-value
                           (substring chosen-file
                                      0 (- (string-length chosen-file) 4)))

                     (send create-file-frame set-status-text
                           "File loaded")) ))) ))) ))


(define create-file-cancel
  ;Button that cancels the create-file sequence

  (new button%
       (label "Cancel")
       (parent create-file-panel)
       (callback
         (lambda (b e)
           (send create-file-frame show #f))) ))

