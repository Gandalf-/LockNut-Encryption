#lang racket

(require test-engine/racket-tests)
(require "../common.rkt")


; generate-personal-key
(check-expect
  (string? (generate-personal-key))
  #t)

; get-personal-key
(check-expect
  (list? (get-personal-key))
  #t)

; swap-extension
(check-expect
  (swap-extension "hello.txt" ".txt" ".tar.gz")
  "hello.tar.gz")

(check-expect
  (swap-extension "hello.tar.gz" ".tar.gz" ".txt")
  "hello.txt")

(test)
