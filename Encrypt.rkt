#lang racket

;LockNut Encryption Program
;Austin Voecks, Summer 2014
;----------------------------------------------------------------------

(require racket/gui)
(require racket/port)

(provide run-encrypt-decrypt)
(provide glance)
(provide create-file)

;FILE IO
;----------------------------------------------------------------------

;Moves the file into a string
(define (file->listChars filename)
  (port->string (open-input-file filename)))

;Prints x to a file
(define (print-this x name)
  (call-with-output-file* name #:exists 'replace
                          (lambda (output-port)
                            (display x output-port))))


;ENCRYPTION SETUP
;----------------------------------------------------------------------

;Creates the inverse of the key-list for solving
(define (create-solver key-list)
  (let loop ((input key-list) (output '()))
    (if (empty? input)
        output
        (loop (cdr input) (flatten (cons output (* -1 (car input)))))
        )))

;Generate a key-list (list of integers indicating shift amounts) from the given password
(define (password->key-list password)
  (let loop ((input (string->list password)) (output '() ))
    (if (empty? input)
        output
        (loop (cdr input) (flatten (cons output (char->integer (car input)))))
        )))

;Add the password-key-list to the default key-list, to make it unique to the given password
(define (alter-key-list default-key-list pass-input-list)
  (let loop ((default-list default-key-list) (pass-list pass-input-list) (output '()))
    (if (empty? default-list)
        output
        (begin
          (when (empty? pass-list)
            (set! pass-list pass-input-list))
          (loop (cdr default-list)
                (cdr pass-list) 
                (flatten (cons output (+ (car default-list)
                                         (car pass-list)))))
          ))))

(define (generate-key-and-solver password password-is-key?-value)
  ;Check if password is being used as the cipher key
  (if password-is-key?-value
      ;Use password as key
      (begin
        (set! key-list (password->key-list password))
        (set! solver-list (create-solver key-list)))
      ;Use default key, which includes the password
      (begin
        ;Modify the key-list with the password
        (set! key-list (alter-key-list default-key (password->key-list password)))
        (set! solver-list (create-solver key-list)))
      ))


;DEFINITIONS
;----------------------------------------------------------------------

;Define default-key, key-list and solver-list
(define A '(28 72 99 85 89 21 40 86 42 26 23 14 50 6 28 51 79 7 34 90 63 90 37 81 18))
(define B '(97 40 51 70 55 53 97 64 12 96 19 71 16 7 9 60 26 5 46 13 96 60 89 85 81))
(define C '(84 10 89 95 91 90 83 69 57 33 72 29 46 10 77 49 89 82 41 0 5 96 20 69 99))
(define D '(61 23 76 35 86 47 3 91 6 79 39 56 31 77 54 96 90 20 81 67 31 65 65 56 61))

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
             (current-key-list key-list)
             )
    
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
                  )))
        )))


;FILE ENCRYPTION
; Takes a file as input and prints the encrypted version of the file to a text file
;--------------------------------------------------------------------------------
(define (encrypt-file input-file-name password)
  
  (let (;Get list of chars from source file
        (chars-list (string->list (file->listChars input-file-name)))
        ;Remove .txt and add .locknut extension
        (new-file-name (string-append (substring input-file-name 0 (- (string-length input-file-name) 4)) ".locknut")))
    
    ;Add the password, with the special verification character at the end, onto the beginning of the chars-list
    (set! chars-list (append (string->list (string-append password
                                                          verification-char))
                             chars-list))
    
    ;Remove the older version of the output file if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))
    
    ;Encrypt and print
    (print-this (list->string (encrypt chars-list key-list password)) input-file-name)
    
    ;Rename the input file
    (rename-file-or-directory input-file-name new-file-name))
  )


;FILE DECRYPTION
; Takes an encrypted .locknut file as input and prints the decrypted version of the 
; file to a text file.
; If glancing, open the file in notepad and delete when the user is finished.
; Otherwise, rename the decrypted text file to the original name of the input
;--------------------------------------------------------------------------------
(define (decrypt-file input-file-name password glance?)
  
  (let (;Get the char-list from the encryped file
        (chars-list (string->list (file->listChars input-file-name)))
        ;Remove .locknut extension and add .txt
        (new-file-name (string-append
                        (substring input-file-name 0 (- (string-length input-file-name) 8))
                        ".txt")))
    
    ;Remove the older version of output file, if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))
    
    ;Decrypt the file with the given password
    (let ((decrypted-file (list->string (encrypt chars-list solver-list password)) ))
      
      ;Check whether to glance or save decrypt file
      (if (equal? #t glance?)
          
          ;Verify password, with added verification character, and glance
          (if (equal? (substring decrypted-file 0 (+ 1(string-length password)))
                      (string-append password verification-char))
              
              ;Valid password, remove password from beginning and show in notepad
              (begin
                (print-this (substring decrypted-file (+ 1 (string-length password)) (string-length decrypted-file))  "Glance.txt")
                (system "notepad.exe Glance.txt")
                (delete-file "Glance.txt"))
              
              ;Invalid password
              #f)
          
          ;Verify password and save decrypted file
          (if (equal? (substring decrypted-file 0 (+ 1 (string-length password)))
                      (string-append password verification-char))
              
              ;Valid password
              (begin
                (print-this (substring decrypted-file (+ 1 (string-length password)) (string-length decrypted-file)) input-file-name)
                ;Rename the encrypted version
                (rename-file-or-directory input-file-name new-file-name))
              
              ;Invalid password
              #f)
          )
      ))
  )


;RUN-ENCRYPT-DECRYPT
; Checks the password field, prompts the user to
; choose a file, detect where to encrypt or decrypt,
; pass user options along to encrypt()
;---------------------------------------------
(define (run-encrypt-decrypt password password-is-key?-value)
  ;Get passcode
  (let (;(password (send passcode-field get-value))
           (no-pass? (equal? password "")))
    
    ;If password is blank, set it to default-password
    (when no-pass?
      (set! password default-password))
    
    (generate-key-and-solver password password-is-key?-value)
    
    ;Get file choice
    (let ((chosen-file (get-file)))
      (if (equal? chosen-file #f)
          "No file chosen"
          
          (begin
            (set! chosen-file (path->string chosen-file))
            
            ;Decrypt .locknut files, encrypt all other files
            (if (equal? ".locknut" (substring chosen-file (- (string-length chosen-file) 8)))
                ;Run decryption
                (begin
                  ;(send main-frame set-status-text "Decrypting...")
                  (if (decrypt-file chosen-file password #f)
                      ;Success
                      "Finished decrypting!"
                      
                      ;Invalid password, nothing done
                      "Invalid password"))
                
                ;Run encryption
                (begin
                  (encrypt-file chosen-file password)
                  "Finished encrypting!"))
            ))
      )))


;GLANCE
; Decrypts a file using the optional password,
; but just opens it in notepad. Decrypted file isn't saved
;---------------------------------------------
(define (glance password password-is-key?-value)
  ;Get passcode
  (letrec (;(password (send passcode-field get-value))
           (no-pass? (equal? password "")))
    
    ;If password is blank, set it to a dummy value so verification works properly
    (when no-pass?
      (set! password default-password))
    
    (generate-key-and-solver password password-is-key?-value)
    
    ;Get file choice
    (let ((chosen-file (get-file)))
      ;Valid file choice?
      (if (equal? chosen-file #f)
          "No file chosen"
          
          (begin
            (set! chosen-file (path->string chosen-file))
            
            ;Decrypt .locknut files, open in notepad
            ;Check if file is .locknut extension
            (if (equal? ".locknut" (substring chosen-file (- (string-length chosen-file) 8)))
                (begin
                  (if (decrypt-file chosen-file password #t)
                      ;Success
                      "Finished decrypting!"
                      
                      ;Invalid password, nothing done
                      "Invalid password"))
                
                ;Wrong extension
                "File must be encrypted .locknut file")
            ))
      ))
  )


;CREATE NEW ENCRYPTED FILE
;-----------------------------------------------
(define (create-file given-file-name password password-is-key?-value)
  ;(let ((given-file-name (send filename-field get-value)))
  ;Check if file name is not blank
  (if (equal? given-file-name "")
      ;Invalid file name
      "File name must not be blank"
      
      ;Check if file name already exists
      (if (file-exists? (string-append given-file-name ".txt"))
          "File of that name already exists"
          
          (begin
            ;(send create-file-frame set-status-text "Creating file...")
            
            (letrec ((full-file-name (string-append given-file-name ".txt"))
                     ;(password (send create-file-passcode-field get-value))
                     (no-pass? (equal? password "")))
              
              ;If password is blank, set it to a dummy value so verification works properly
              (when no-pass?
                (set! password default-password))
              
              (generate-key-and-solver password password-is-key?-value)
              
              ;Create file
              (print-this "" full-file-name)
              ;Open file
              (system (string-append "notepad.exe " full-file-name))
              ;                ;Hide create-file window
              ;                (send create-file-frame show #f)
              
              ;(send main-frame set-status-text "Encrypting...")
              (encrypt-file full-file-name password)
              "Finished encrypting!"
              ))
          ))
  )




