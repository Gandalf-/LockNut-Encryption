#lang racket

; init
;
; special window that's only run once per system, other miscellaneous elements

(require racket/gui)
(require "common.rkt")

(provide (all-defined-out))


; frame

(define init-frame
  ; top level frame for init window

  (new frame%
       (label "Welcome to LockNut!")
       (width 400)
       (height 800)
       (stretchable-width #f)
       (stretchable-height #f)))


; message

(define init-background-image
  ; background, custom nut image

  (new message% [parent init-frame]
       (label (make-object bitmap% "ln_support/Nut.jpg"))))


; button

(define init-readme-button
  ; open the readme in notepad

  (new button%
       (label "View readme")
       (parent init-frame)
       (horiz-margin 35)
       (callback (lambda (b e)
                   (system "notepad.exe README.md")))))

(define init-close
  ; close the init-window, open the main window

  (new button%
       (label "Let's go!")
       (parent init-frame)
       (callback
         (lambda (b e)
           (send main-frame show #t)
           (send main-frame create-status-line)
           (send init-frame show #f)))))

