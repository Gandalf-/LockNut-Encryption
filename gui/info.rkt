#lang racket

; info
;
; info window gui elements

(require racket/gui)
(require "common.rkt")

(provide (all-defined-out))


; frame

(define info-frame
  ; top level frame for information window

  (new frame%
       (label "Information")
       (width 200)
       (height 100)
       (stretchable-width #f)
       (stretchable-height #f)))


; message

(define info-message
  (new message%
       (label
         "Locknut: file encryption program  \n Austin Voecks, Summer 2014")
       (font my-font)
       (parent info-frame)))


; button

(define readme-button
  (new button%
       (label "View readme")
       (parent info-frame)
       (horiz-margin 35)
       (callback (lambda (b e)
                   (system "notepad.exe README.md")))))

(define info-close
  (new button%
       (label "Close")
       (parent info-frame)
       (horiz-margin 35)
       (callback (lambda (b e)
                   (send info-frame show #f)))))

