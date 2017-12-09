#lang racket

(require test-engine/racket-tests)
(require "../alg/waterfall.rkt")

(define message
  "hello there this is a much longer message because it turns out hashes are quite long too")

(define encrypted_message
  '("igoh" "\aE\u0018\u0004" "\nR\u0011H" "\u0011\u001A\fS" "T\u0001\u001AS" "AI\u001EU" "\u0002HM\u0019" "\f\u0006G\t" "\u001DN\n\u0000" "\u0001S\f\u0002" "\u0016S\u0003\u0002" "\u0006A\u0017\u0016" "\u0006A\u001C\a" "ET\u001C\u0006" "N\aU\u001D" "\e\a\u0000\a" "\u0014\aH\r" "\u0012S\t\u0017" "\u0016\u0000\u0010\a" "\fT\u0014U" "\u0005\e\vG" "L\e\u0001\b"))

(define key '(4 5 2 5 1))


; ord-string
(check-expect
  (ord-string "")
  '())

(check-expect
  (ord-string "abc")
  '(97 98 99))

; concat
(check-expect
  (concat '())
  "")

(check-expect
  (concat '("abc" "def" "ghi"))
  "abcdefghi")

; extend
(check-expect
  (extend '(1 2) 4)
  '(1 2 1 2))

(check-expect
  (extend '(1 2 3) 4)
  '(1 2 3 1))

(check-expect
  (extend '(1 2 3 4 5) 2)
  '(1 2))


; split-list
;; chunk size is less than the input
(check-expect
  (split-list (string->list "hello hello hello") 5)
  '("hello" " hell" "o hel" "lo"))

;; chunk size is one
(check-expect
  (split-list (string->list "hello") 1)
  '("h" "e" "l" "l" "o"))

;; chunk size is greater than the input
(check-expect
  (split-list (string->list "hello") 10)
  '("hello"))


; vigenere
;; key longer than message
(check-expect
  (let ((key '(1 2 3 4 5 6 7 8 9 10)))
    (vigenere
      (vigenere message key)
      key))
  message)

;; decrypt then encrypt
(check-expect
  (let ((key '(1 2 3 4 5 6 7 8 9 10)))
    (vigenere
      (vigenere message key)
      key))
  message)

;; message longer than key
(check-expect
  (let ((key '(1 2 3)))
    (vigenere
      (vigenere message key)
      key))
  message)

;; huge key values
(check-expect
  (let ((key '(1000 2000 3000)))
    (vigenere
      (vigenere message key)
      key))
  message)

;; huge key values, decrypt then encrypt
(check-expect
  (let ((key '(1000 2000 3000)))
    (vigenere
      (vigenere message key)
      key))
  message)


; waterfall-encrypt
; single value key
(check-expect
  (waterfall-encrypt
    '("a" "b" "c")
    '(1))
  '("`" "\u0003" "\u0001"))

; algorithm check 1
(check-expect
  (waterfall-encrypt
    '("ab" "bc" "de")
    '(1 2))
  '("``" "\u0003\u0001" "\u0006\u0006"))

; multi value key
(check-expect
  (waterfall-encrypt
    '("ab" "cd")
    '(1 2))
  '("``" "\u0002\u0006"))

; multi value key, large message
(check-expect
  (waterfall-encrypt
    (split-list (string->list message) 4)
    '(1 2 3 4))
  encrypted_message)

;; final chunk is smaller than key length
(check-expect
  (waterfall-encrypt
    '("abcd" "efgh" "cd")
    '(1 2 3 4))
  '("````" "\u0004\u0004\u0004\f" "\u0006\u0002"))


; waterfall-decrypt
; single value key
(check-expect
  (waterfall-decrypt
    '("`" "\u0003" "\u0001")
    '(1))
  '("a" "b" "c"))

; algorithm check 1
(check-expect
  (waterfall-decrypt
    '("``" "\u0003\u0001" "\u0006\u0006")
    '(1 2))
  '("ab" "bc" "de"))

; multi value key
(check-expect
  (waterfall-decrypt
    '("``" "\u0002\u0006")
    '(1 2))
  '("ab" "cd"))

; multi value key, large message
(check-expect
  (waterfall-decrypt
    encrypted_message
    '(1 2 3 4))
  (split-list (string->list message) 4))

;; final chunk is smaller than key length
(check-expect
  (waterfall-decrypt
    '("````" "\u0004\u0004\u0004\f" "\u0006\u0002")
    '(1 2 3 4))
  '("abcd" "efgh" "cd"))


;waterfall
;; encrypt, decrypt
(check-expect
  (waterfall
    (waterfall message "apple" #t)
    "apple" #f)
  message)

;; message is shorter than the key
(check-expect
  (waterfall
    (waterfall "hello" "apple sauce this is long" #t)
    "apple sauce this is long" #f)
  "hello")

;; decrypt, encrypt
(check-expect
  (waterfall
    (waterfall message "apple" #f)
    "apple" #t)
  message)

;; decrypt, encrypt
(check-expect
  (waterfall
    (waterfall message "apple sauce this is long" #f)
    "apple sauce this is long" #t)
  message)

(test)
