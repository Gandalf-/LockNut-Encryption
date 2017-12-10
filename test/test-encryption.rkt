#lang racket

(require test-engine/racket-tests)
(require "../alg/encryption.rkt")


(define test-file "data/alice.txt")

; encrypt-file
(check-expect
  (encrypt-file test-file "password")
  #t)

; decrypt-file
(check-expect
  (decrypt-file test-file "password")
  #t)

(test)
