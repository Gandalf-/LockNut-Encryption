#lang racket

(require test-engine/racket-tests)
(require "../common.rkt")
(require "common.rkt")


; generate-personal-key
(check-expect
  (string? (generate-personal-key))
  #t)


; get-personal-key
(check-expect
  (list? (get-personal-key))
  #t)


; file->string
(check-expect
  (string? (file->string temp-file))
  #t)


; buffer-password
;; extend
(check-expect
  (string-length
    (buffer-password "hello"))
  50)

;; truncate
(check-expect
  (string-length
    (buffer-password
      (make-string 60 #\a)))
  50)

;; empty
(check-expect
  (string-length
    (buffer-password ""))
  50)

;; empty -> default
(check-expect
  (buffer-password "")
  default-password)


; buffer-fname
;; no change
(check-expect
  (buffer-fname "hello" 10)
  "hello")

;; truncate
(check-expect
  (buffer-fname "hello" 3)
  "...llo")


; swap-extension
(check-expect
  (swap-extension "hello.txt" ".txt" ".tar.gz")
  "hello.tar.gz")

(check-expect
  (swap-extension "hello.tar.gz" ".tar.gz" ".txt")
  "hello.txt")


(test)
