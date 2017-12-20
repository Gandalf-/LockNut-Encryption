#lang racket

(require test-engine/racket-tests)

(require "../alg/encryption.rkt")
(require "common.rkt")


; encrypt-file
(check-expect
  (encrypt-file temp-file "password")
  #t)

; decrypt-file
(check-expect
  (decrypt-file temp-file "password")
  #t)

(test)
