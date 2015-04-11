#lang racket

;Encrypt-Algorithm
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;Contains the actual encryption algorithm, default key-list, 
; default password, and all functions that deal with them
;---------------------------------------------------------------------

(require racket/port)

;Provisions for Encrypt.rkt
(provide encrypt)
(provide generate-key-and-solver)
(provide default-password)
(provide key-list)
(provide solver-list)

;Encryption key length
(define key-length 250)

;Moves the file into a string
(define (file->listChars filename)
  (port->string (open-input-file filename)))

;ENCRYPTION SETUP
;----------------------------------------------------------------------

;Gets the personal key from file
(define (get-personal-key)
  map
  string->number
  (string-split (file->listChars "Data/PersonalKey.locknut")))


;Take the inverse of the encryption key to make the decryption key
;Creates the inverse of the key-list for solving
(define (create-solver key-list)
  (map
   (lambda (x) (* -1 x))
   key-list))

;Make encryption key
;Generate a key-list (list of integers indicating shift amounts) from the given password
(define (password->key-list password)
  (map
   (lambda (x) (char->integer x))
   (string->list password)))


;Add the password-key-list to the default key-list, to make it unique to the given password
(define (alter-key-list def-key pass-key)
  (if (< (length pass-key) (length default-key))
      (alter-key-list def-key (flatten (cons pass-key pass-key)))
      (map
       (lambda (x y)
         (+ x y))
       (map string->number def-key)
       (take (flatten pass-key) (length def-key)))))


;Determine the starting position in the default key from the given password
; Add all integer values of chars in password-key-list and modulo by the length
; of the default key
(define (password->starting-position password)
  (modulo 
    (foldl + 0 (password->key-list password))
    key-length))


;Checks for blank password, then generates the both the encryption and decryption
; keys. These aren't passed out. They're values are set to key-list and solver-list,
; predefined variables
(define (generate-key-and-solver password pass-is-key? shareable?)

  ;Determine which base-key to use. Default for shareable, Personal for not shareable
  (let ((base-key '() ))
    (if (equal? #t shareable?)
      ;Use the default, shareable
      (set! base-key default-key)
      ;Use personal key, non-shareable
      (set! base-key (get-personal-key)))

    ;Check if password is being used as the cipher key
    (if pass-is-key?
      ;Use password as key
      (begin
        (set! key-list (password->key-list password))
        (set! solver-list (create-solver key-list)))
      ;Use default key, which includes the password
      (begin
        ;Modify the key-list with the password
        (set! key-list (alter-key-list base-key (password->key-list password)))
        (set! solver-list (create-solver key-list)))
      )
    ;Return password in case it's been set to the default value
    password))


;DEFINITIONS
;----------------------------------------------------------------------

;Define default-key, key-list and solver-list
(define A '(82 85 71 2 12 37 88 56 29 79 30 27 82 2 91 67 86 94 72 45 96 92 53 84 1 54 23 56 65 65 78 90 93 9 87 51 10 54 5 67 21 57 62 93 59 58 60 58 3 21))
(define B '(8 65 5 42 29 60 46 48 44 68 64 82 50 86 38 3 67 35 88 4 73 30 51 56 38 18 72 45 69 17 3 30 16 54 78 40 38 31 23 27 4 98 7 55 45 30 16 9 54 66))
(define C '(60 22 41 53 76 78 6 70 34 25 56 88 37 18 60 67 34 57 52 55 33 27 14 18 98 40 24 24 52 61 41 17 90 63 39 27 80 55 15 1 20 2 4 96 7 79 37 9 55 17))
(define D '(49 21 59 0 28 31 51 43 45 34 44 32 28 94 0 10 69 30 72 2 12 28 86 53 91 84 6 32 17 51 78 56 58 12 6 39 51 54 14 37 65 8 43 88 2 8 30 87 16 13))
(define E '(75 27 9 31 28 66 63 29 68 46 10 2 16 20 13 58 24 15 13 29 85 17 70 9 62 76 67 59 93 60 22 88 99 6 67 32 11 91 89 83 58 83 41 37 6 20 9 43 12 84))

;Default key, 250 (* 50 5), random integers [0-100]
(define default-key (append A B C D E))
(define default-password "6AQO*fvr*RQ7Uv!mCnPc8vxKdia45a$uh'7B5K06Rcj863RMyg")

;Both uniquely assigned during encrypt/decrypt
(define key-list '() )
(define solver-list '() )


;ENCRYPTION ALGORITHM
;----------------------------------------------------------------------
; Takes a list of characters and returns a list of encrypted characters  
(define (encrypt input-list key-list password)

  (define (prep-key key)
    (if (< (length key) (length input-list))
      (prep-key (flatten (cons key key-list)))
      (take (flatten key) (length input-list))))
  (map
    (lambda (x y)
      ; Incorrect passwords can result in negative shifts, default to zero.
      (integer->char (if (> 0 (+ x y))
                       0
                       (+ x y))))
    (map char->integer
         input-list)
    (prep-key (list-tail
                key-list
                (password->starting-position password)))))
