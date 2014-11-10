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

(provide key-list)
(provide solver-list)
(provide verification-char)

;Encryption key length
(define key-length 100)

;Moves the file into a string
(define (file->listChars filename)
  (port->string (open-input-file filename)))

;ENCRYPTION SETUP
;----------------------------------------------------------------------

;Gets the personal key from file
(define (get-personal-key)
  (let loop ((in (string-split (file->listChars "Data/PersonalKey.locknut")))
             (out '() ))
    (if (empty? in)
        out
        (loop (cdr in)
              (flatten (cons out (string->number (car in))))
              ))
    ))

;Take the inverse of the encryption key to make the decryption key
;Creates the inverse of the key-list for solving
(define (create-solver key-list)
  (let loop ((input key-list)
             (output '()))
    (if (empty? input)
        output
        (loop (cdr input)
              (flatten (cons output (* -1 (car input)))))
        )))

;Make encryption key
;Generate a key-list (list of integers indicating shift amounts) from the given password
(define (password->key-list password)
  (let loop ((input (string->list password))
             (output '() ))
    (if (empty? input)
        output
        (loop (cdr input)
              (flatten (cons output (char->integer (car input)))))
        )))

;Add the password-key-list to the default key-list, to make it unique to the given password
(define (alter-key-list default-key-list pass-input-list)
  (let loop ((default-list default-key-list)
             (pass-list pass-input-list)
             (output '() ))
    (if (empty? default-list)
        output
        (begin
          ;Reset the pass-list
          (when (empty? pass-list)
            (set! pass-list pass-input-list))
          
          (loop (cdr default-list)
                (cdr pass-list) 
                (flatten (cons output (+ (car default-list)
                                         (car pass-list)))))
          ))
    ))

;Determine the starting position in the default key from the given password
; Add all integer values of chars in password-key-list and modulo by the length
; of the default key
(define (password->starting-position password)
  (let loop ((pass-list (password->key-list password))
             (output 0))
    (if (empty? pass-list)
        (modulo output 100)
        (loop (cdr pass-list)
              (+ output (car pass-list))))
    ))


;Checks for blank password, then generates the both the encryption and decryption
; keys. These aren't passed out. They're values are set to key-list and solver-list,
; predefined variables
(define (generate-key-and-solver password password-is-key?-value shareable?)
  
  ;Determine which base-key to use. Default for shareable, Personal for not shareable
  (let ((base-key '()))
    (if (equal? #t shareable?)
        ;Use the default, shareable
        (set! base-key default-key)
        ;Use personal key, non-shareable
        (set! base-key (get-personal-key)))
    
    ;If password is blank, set it to default-password
    (when (string=? password "")
      (set! password default-password))
    
    ;Check if password is being used as the cipher key
    (if password-is-key?-value
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
    password
    ))


;DEFINITIONS
;----------------------------------------------------------------------

;Define default-key, key-list and solver-list
(define A '(28 72 99 85 89 21 40 86 42 26 23 14 50 6 28 51 79 7 34 90 63 90 37 81 18))
(define B '(97 40 51 70 55 53 97 64 12 96 19 71 16 7 9 60 26 5 46 13 96 60 89 85 81))
(define C '(84 10 89 95 91 90 83 69 57 33 72 29 46 10 77 49 89 82 41 0 5 96 20 69 99))
(define D '(61 23 76 35 86 47 3 91 6 79 39 56 31 77 54 96 90 20 81 67 31 65 65 56 61))

;Default key, 100 random integers [0-100]
(define default-key (append A B C D))
(define default-password "QUz7x5SW3dvpuIhCRjXgNXdFJU8a")

;Verification character is tacked onto the end of the password given during encryption.
; It's used to make sure the an incorrect password is allowed to decrypt because its a
; substring of the correct password.
(define verification-char "훘")

;Both uniquely assigned during encrypt/decrypt
(define key-list '() )
(define solver-list '() )



;ENCRYPTION ALGORITHM
;----------------------------------------------------------------------
; Takes a list of characters and returns a list of encrypted characters  
(define (encrypt input-list key-list password)
  
  ;Run algorithm
  (let loop ((output-list '() )
             (remaining-input input-list)
             ;Choose the starting position of the input-key with password->starting position
             (current-key-list (list-tail key-list
                                          (password->starting-position password)
                                          )))
    (if (empty? remaining-input)
        output-list
        
        (begin
          ;Refill the key-list when it's empty
          (when (empty? current-key-list)
            (set! current-key-list key-list))
          
          ;Create the new shifted-integer from the current key-list value added
          ; to the current input-list value
          (let ((shifted-integer (+ (car current-key-list) 
                                    (char->integer (car remaining-input)))))
            
            ;Incorrect password results in negative shift amount check
            ; Default to 0, the result will be incorrect anyway
            (when (> 0 shifted-integer)
              (set! shifted-integer 0))
            
            ;Continue loop with new encrypted character added to the output list
            (loop (flatten (cons output-list
                                 (integer->char shifted-integer)))
                  (cdr remaining-input)
                  (cdr current-key-list)
                  ))
          ))
    ))
