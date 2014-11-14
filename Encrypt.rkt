#lang racket

;Encrypt
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;Functions that work between the algorithm and the GUI, handle file IO
;----------------------------------------------------------------------

(require racket/gui)
(require racket/port)
(require "Encrypt-Algorithm.rkt")

;Provisions to LockNut.rkt
(provide run-encrypt-decrypt)
(provide glance)
(provide create-file)
(provide print-this)

(define file-name "Out.txt")
(define curr-password "")

;FILE IO
;----------------------------------------------------------------------

;Moves the file into a string
(define (file->listChars filename)
  (port->string (open-input-file filename)))

;Creates a UTF8 .txt file
(define (init-file name)
  (when (file-exists? name)
    (delete-file name))
  ;Copy UTF8
  (copy-file "Support/UTF8.txt" name #f))

;Prints x to a file
(define (print-this x name)
  (call-with-output-file* name #:exists 'replace
                          (lambda (output-port)
                            (display x output-port))))

;HELPERS
;------------
;Buffers the password to 50 characters
(define (buff-password password)
  ;Check if it's longer than 50 characters already
  (if (>= (string-length password) 50)
      (substring password 0 50)
      ;Buffer it using the default password
      (string-append password 
                     (substring default-password 0 (- 50 (string-length password))))
      ))

;CALLEES
;================================================

;FILE ENCRYPTION
; CALLED BY CREATE-FILE, GLANCE, AND RUN-ENCRYPT-DECRYPT
; Takes a file as input and prints the encrypted version of the file to a text file
;--------------------------------------------------------------------------------
(define (encrypt-file input-file-name password)
  
  (let (;Get list of chars from source file
        (chars-list (string->list (file->listChars input-file-name)))
        ;Remove .txt and add .locknut extension
        (new-file-name (string-append (substring input-file-name 0 (- (string-length input-file-name) 4))
                                      ".locknut")))
    
    ;Add the buffered password onto the beginning of the chars-list, which is the file
    (set! chars-list (append (string->list password)
                             chars-list))
    
    ;Remove the older version of the output file if necessary
    (when (file-exists? new-file-name)
      (delete-file new-file-name))
    
    ;Encrypt and print
    (print-this (list->string (encrypt chars-list key-list password))
                input-file-name)
    
    ;Rename the input file
    (rename-file-or-directory input-file-name
                              new-file-name))
  )


;FILE DECRYPTION
; CALLED BY CREATE-FILE, GLANCE, AND RUN-ENCRYPT-DECRYPT
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
    
    ;Decrypt the file with the given buffered password
    (let ((decrypted-file (list->string (encrypt chars-list
                                                 solver-list
                                                 password)) ))
      
      ;Check whether to glance or save decrypt file
      (if (equal? #t glance?)
          
          ;Verify password against buffered password, and glance
          (if (equal? (substring decrypted-file 0 50)
                      password)
              
              ;Valid password, remove password from beginning and show in notepad
              (begin
                (print-this (substring decrypted-file 50 (string-length decrypted-file))
                            "Glance.txt")
                (system "notepad.exe Glance.txt")
                (delete-file "Glance.txt"))
              
              ;Invalid password
              #f)
          
          ;Verify buffered password and save decrypted file
          (if (equal? (substring decrypted-file 0 50)
                      password)
              
              ;Valid password
              (begin
                ;Remove the password and print the file
                (print-this (substring decrypted-file 50 (string-length decrypted-file))
                            input-file-name)
                ;Rename the encrypted version
                (rename-file-or-directory input-file-name
                                          new-file-name))
              
              ;Invalid password
              #f)
          )
      ))
  )


;CALLERS
;================================================================

;RUN-ENCRYPT-DECRYPT
; CALLS ENCRYPT-FILE AND DECRYPT-FILE
; Checks the password field, prompts the user to
; choose a file, detect where to encrypt or decrypt,
; pass user options along to encrypt()
;---------------------------------------------
(define (run-encrypt-decrypt password password-is-key?-value shareable?)
  
  ;Check for blank password, then generate the cipher key-list and solver-list
  (set! password (generate-key-and-solver (buff-password password)
                                          password-is-key?-value
                                          shareable?))
  ;Get file choice
  (let ((chosen-file (get-file)))
    (if (equal? chosen-file #f)
        "No file chosen"
        
        (begin
          (set! chosen-file (path->string chosen-file))
          
          ;Decrypt .locknut files, encrypt all other files
          (if (equal? ".locknut" (substring chosen-file (- (string-length chosen-file) 8)))
              ;Run decryption
              (if (decrypt-file chosen-file password #f)
                  ;Success
                  "Finished decrypting!"
                  
                  ;Invalid password, nothing done
                  "Invalid password or incorrect base-key")
              
              ;Check if .txt && run encryption
              (if (equal? ".txt" (substring chosen-file (- (string-length chosen-file) 4)))
                  (begin
                    (encrypt-file chosen-file password)
                    "Finished encrypting!")
                  
                  "Only .txt files may be encrypted"))
          ))
    ))


;GLANCE
; CALLS DECRYPT-FILE
; Decrypts a file using the optional password,
; but just opens it in notepad. Decrypted file isn't saved
;---------------------------------------------
(define (glance password password-is-key?-value shareable?)
  
  ;Buffer password, then generate the cipher key-list and solver-list
  (set! password (generate-key-and-solver (buff-password password)
                                          password-is-key?-value
                                          shareable?))
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
                    "Invalid password or incorrect base-key"))
              
              ;Wrong extension
              "File must be encrypted .locknut file")
          ))
    ))


;CREATE NEW ENCRYPTED FILE
;CALLS ENCRYPT
;-----------------------------------------------
(define (create-file given-file-name password password-is-key?-value shareable?)
  
  ;Buffer password and generate the cipher key-list and solver-list
  (set! password (generate-key-and-solver (buff-password password)
                                          password-is-key?-value
                                          shareable?))
  ;Encrypt file
  (encrypt-file given-file-name password)
  )





