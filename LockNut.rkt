#lang racket

;LockNut
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;--------------------------------------------
;GUI
;--------------------------------------------

(require racket/gui)
(require "Encrypt.rkt")

(require "gui/common.rkt")
(require "gui/main-window.rkt")
(require "gui/info-window.rkt")

;----------------------------------------------------------------------
;INIT
;----------------------------------------------------------------------

(define init-frame
  ;Top level frame for init window

  (new frame%
       (label "Welcome to LockNut!")
       (width 400)
       (height 800)
       (stretchable-width #f)
       (stretchable-height #f)))


(define init-background-image
  ;Background, custom nut image

  (new message% [parent init-frame]
       (label (make-object bitmap% "ln_support/Nut.jpg"))))


(define init-readme-button
  ;Open the readme in notepad

  (new button%
       (label "View readme")
       (parent init-frame)
       (horiz-margin 35)
       (callback (lambda (b e)
                   (system "notepad.exe README.md")))))


(define init-close
  ;Close the init-window, open the main window
  (new button%
       (label "Let's go!")
       (parent init-frame)
       (callback
         (lambda (b e)
           (send main-frame show #t)
           (send main-frame create-status-line)
           (send init-frame show #f)))))


(define (generate-personal-key)
  ;Creates a string of 250 random integers [0-200]

  (build-list 250 (lambda (x) (random 200))))


(define (startup)
  ;Startup sequence. Check for PersonalKey file, otherwise run first time
  ;setup: Generate personal key, offer readme

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
