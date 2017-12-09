#lang racket

(require racket/gui)

(provide (all-defined-out))

; definitions

(define my-font (make-object font% 10 'modern))
(define wait-time 1.5)

(define help-mode #f)
(define (help-mode-off) (set! help-mode #f))
(define (help-mode-on) (set! help-mode #t))


; frame

(define main-frame
  ; main frame, parent to all other frames and panels

  (new frame%
       (label "Locknut")
       (stretchable-width #f)
       (stretchable-height #f)))


; text-field

(define passcode-field
  ;Gets optional password from user
  (new text-field%
       (label "Password")
       (parent main-frame)
       (font my-font)))

