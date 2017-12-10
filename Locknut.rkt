#lang racket

; locknut
;
; main file for gui version of locknut

(require racket/gui)
(require "core.rkt")
(require "common.rkt")
(require "gui/common.rkt")
(require "gui/main.rkt")
(require "gui/info.rkt")
(require "gui/init.rkt")


(define (main)
  ; Startup sequence. Check for PersonalKey file, otherwise run first time
  ; setup: Generate personal key, offer readme

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

(main)
