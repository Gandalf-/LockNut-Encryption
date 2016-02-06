#lang racket

(require openssl/sha1)
(provide waterfall)
(provide Waterfall-test)

;HELPERS
;====================================
;Takes the inverse of the list
;[ list of any -> list of any ]
(define (inverse-list key-list)
  (map
    (lambda (x) (* -1 x))
    key-list))

;Replace password->key-list
;[ string -> list of integer ]
(define (string->integer-list in)
  (map
    char->integer
    (string->list in)))

;Takes a list of strings and appends them together
;[ list of string -> string ]
(define (list-strings->string in)
  (foldr string-append "" in))

;Split the input list of characters into strings of len length
;[ list of chars -> list of strings ]
(define (split-list input len)
  (let loop ((out '() )
             (curr input))
    (if (> len (length curr))

      ;Remove potential empty string in results
      (let ((res (reverse (cons (list->string curr)
                                out))))
        (if (string=? (last res) "")
          (take res (- (length res) 1))
          res))

      ;Add len elements to output
      (loop (cons (list->string (take curr len))
                  out)
            (list-tail curr len)) )))

;ENCRYPTION FUNCS
;====================================
;Encrypts input with the key. Simple Viegenere Cipher
;[ string, list of integer -> string ]
(define (cipher input key)
  (let loop ((in (string->integer-list input))
             (k key))

    ;Buffer key to length of input
    (if (> (length in) (length k))
      (loop in
            (flatten (cons k k)))

      ;Substitution cipher, no negative values
      (list->string
        (map
          (lambda (x y)
            (if (< 0 (+ x y))
              (integer->char (+ x y))
              (integer->char (+ x 0))))
          in (take k (length in))) ))))

;Encrypts a list of characters broken into sublists
;[ list of strings, list of integers -> list of strings ]
(define (waterfall-encrypt input key)
  (let loop ((out '() )
             (curr input))

    (if (= 1 (length curr))
      ;Add on the first element, encrypted with the key
      (cons (cipher (car input) key)
            (reverse out))

      ;Encrypt the next element with the current element as its key
      (loop (cons (cipher (car (cdr curr))
                          (string->integer-list (car curr)))
                  out)
            (cdr curr)) )))

;Decrypts a waterfall encrypted list of characters
;[ list of strings, list of integers -> list of strings ]
(define (waterfall-decrypt input-list key-list)
  ; decrypt the first element with the key-list
  (let loop ((out (list (cipher 
                          (car input-list)
                          key-list)))
             (curr input-list))

    (if (= 1 (length curr))
      (reverse out)

      ;Decrypt the next element with the current, already decrypted element
      (loop (cons (cipher
                    (cadr curr)
                    (inverse-list (string->integer-list (car out))))
                  out)
            (cdr curr)) )))

;MAIN
;====================================
;Encrypts or decrypts strings
;[ list of strings, string, bool -> string ]
(define (waterfall input-string key-string encrypt-mode)
  ;Prepare input: hash password, in -> list
  (let ((key-list (string->integer-list
                    (sha1 (open-input-bytes
                            (string->bytes/utf-8 key-string)))))
        (input-list (string->list input-string)))

    ;Get length for spliting, and split the input
    (letrec ((key-len (length key-list))           
             (lists (split-list input-list key-len)))

      (if encrypt-mode
        ;Encrypt
        (list-strings->string
          (waterfall-encrypt lists key-list))

        ;Decrypt
        (list-strings->string
          (waterfall-decrypt lists (inverse-list key-list)))) )))

;TEST
;====================================
(define (Waterfall-test)
  ; inverse-list
  (unless (equal? '(-5 -4 -3 -2 -1)
                  (inverse-list '(5 4 3 2 1)))
    (displayln "error: inverse-list"))

  ; string->integer-list
  (unless (equal? '(65 66 67 68)
                  (string->integer-list "ABCD"))
    (displayln "error: string->integer-list"))

  ; list-strings->string
  (unless (equal? "ABCD" 
                  (list-strings->string '("A" "B" "C" "D")))
    (displayln "error: list-strings->string"))

  ; split-list
  (unless (equal? '("AB" "CD")
                  (split-list '(#\A #\B #\C #\D) 2))
    (displayln "error: split-list"))

  ; cipher
  (unless (and (equal? "BCDEFGH" (cipher "ABCDEFG" '(1)))
               (equal? "BCDEFGH" (cipher "ABCDEFG" '(1 1 1 1 1 1 1 1 1)))
               (equal? "AAAAAAA" (cipher "AAAAAAA" '(-5000)))
               )
    (displayln "error: cipher"))

  ; waterfall-encrypt
  (letrec 
    ((plain '("AAA" "BBB" "CCC"))
     (cipher (waterfall-encrypt plain '(1 2 3)))
     (cplain (waterfall-decrypt cipher '(1 2 3))))

    (unless (equal? plain cplain)
      (displayln plain)
      (displayln cipher)
      (displayln cplain)
      (displayln "error: waterfall-encrypt waterfall-decrypt")))

  ; waterfall-decrypt

  ; waterfall

  (displayln "Waterfall tests complete"))
