#lang racket

;----------------------------------------------------------------------
;Encrypt
;LockNut Encryption Program
;Austin Voecks, Summer 2014

;Functions that work between the algorithm and the GUI, handle file IO
;----------------------------------------------------------------------

(require racket/gui)
(require racket/port)

(require "alg/encryption.rkt")
(require "common.rkt")
(require "gui/text-editor.rkt")

(provide (all-defined-out))


(define (password->key-list password)
  ;Make encryption key Generate a key-list (list of integers indicating shift
  ;amounts) from the given password
  ;
  ; string -> list of integers

  (map char->integer (string->list password)))


(define (alter-key-list default-key-list pass-input-list)
  ;Add the password-key-list to the default key-list, to make it unique to the
  ;given password
  ;
  ; list of integers -> list of integers

  (if (< (length pass-input-list)
         (length default-key-list))
    (alter-key-list
      default-key-list
      (append pass-input-list pass-input-list))

    (map
      (lambda (x y) (+ x y))
      default-key-list
      (take pass-input-list (length default-key-list)))))


(define (generate-key-and-solver
          password password-is-key? shareable?)
  ; Checks for blank password, then generates the both the encryption and
  ; decryption keys. These aren't passed out. They're values are set to
  ; key-list and solver-list, predefined variables. This is done because
  ; they're needed in multiple places and between mutliple runs of these
  ; functions
  ;
  ; string, bool, bool -> string

  ;Determine which base-key to use. Default for shareable, Personal for not
  ;shareable

  (let ((base-key '()))
    (if shareable?
      (set! base-key default-key)
      (set! base-key (get-personal-key)))

    ;Check if password is being used as the cipher key
    (if password-is-key?

      ;Use password as key
      (set-key-list
        (password->key-list password))

      ;Use default key, which includes the password
      (set-key-list
        (alter-key-list base-key (password->key-list password))))

    ;Return password in case it's been set to the default value
    password ))



;----------------------------------------------------------------------
;HELPERS
;----------------------------------------------------------------------


;CALLERS
;================================================================

;decrypt
; CALLS DECRYPT-FILE
; Checks the filename and passes info back up
;
; string, bool, bool -> string
;---------------------------------------------
(define (decrypt password password-is-key? shareable?)

  ;Save password in case the user wants to re-encrypt or decrypt
  (set-unbuffered-password password)

  ;Buffer password, then generate the cipher key-list
  (set! password
    (generate-key-and-solver
      (buff-password password)
      password-is-key?
      shareable?))

  ;Get file choice
  (let ((chosen-file (get-file)))

    ;Check if file choice is valid
    (if (equal? chosen-file #f)
      "No file chosen"
      (begin
        (set! chosen-file (path->string chosen-file))

        ;Check if file is .locknut extension
        (if (equal? ".locknut"
                    (substring chosen-file (- (string-length chosen-file) 8)))

          ; Check if decryption was successful
          (if (decrypt-file chosen-file password)
            "Finished decrypting!"
            "Invalid password or incorrect base-key")

          "File must be encrypted .locknut file") )) ))


;CREATE NEW ENCRYPTED FILE
;CALLS ENCRYPT
;
; string, string, bool, bool -> none
;-----------------------------------------------
(define (create-file given-file-name password password-is-key? shareable?)

  ;Buffer password and generate the cipher key-list
  (set! password
    (generate-key-and-solver
      (buff-password password)
      password-is-key?
      shareable?))

  ;Encrypt file
  (encrypt-file given-file-name password))

