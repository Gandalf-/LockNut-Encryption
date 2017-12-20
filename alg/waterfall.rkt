#lang typed/racket

; waterfall
;
; cryptography functions

(require typed/openssl/sha1)
(provide (all-defined-out))


(: ord-string (-> String (Listof Integer)))
(define (ord-string in)
  ; list of string to list of ints where the ints are the ascii value for the
  ; string character at that position

  (map
    char->integer
    (string->list in)))


(: concat (-> (Listof String) String))
(define (concat in)
  ; concatentate a list of strings

  (foldr string-append "" in))


(: split-list (-> (Listof Char) Integer (Listof String)))
(define (split-list l chunk)
  ; split the input list of characters into strings of chunk length

  (let splitter ((result : (Listof String) '())
                 (remaining : (Listof Char) l))

    (if (empty? remaining)
      result

      (let ((size (min chunk (length remaining))))
        (splitter
          (append result
                  (list (list->string (take remaining
                                            size))))
          (list-tail remaining size)
          )))))


(: extend (All (a) (-> (Listof a) Integer (Listof a))))
(define (extend l n)
  ; extend a list to length 'n' by appending it to itself

  (take (append* (make-list n l)) n))


(: drop-last (All (a) (-> (Listof a) (Listof a))))
(define (drop-last l)
  ; drop the last element from a list

  (take l (- (length l) 1)))


(: vigenere (-> String (Listof Integer) String))
(define (vigenere input key)
  ; encrypts input with the key. Simple Viegenere Cipher
  ; substitution cipher, no negative values

  (let ((characters (ord-string input)))
    (list->string
      (map
        (lambda ((x : Integer) (y : Integer))
          (integer->char
            (bitwise-xor x y)))

        characters

        ; make the input and key the same size by extending the key if needed
        (extend key (length characters))
        ))))


(: waterfall-encrypt (-> (Listof String) (Listof Integer) (Listof String)))
(define (waterfall-encrypt input key)
  ; encrypts a list of characters broken into sublists. we work forwards,
  ; encrypting the current chunk with the next chunk. the first chunk uses the
  ; argument key for encryption

  (map vigenere
       input
       (append
         (list key)
         (map ord-string (drop-last input))
         )))


(: waterfall-decrypt (-> (Listof String) (Listof Integer) (Listof String)))
(define (waterfall-decrypt input key)
  ; decrypts a waterfall encrypted list of characters

  (: cipher (-> (Listof String) (Listof Integer) (Listof String) (Listof String)))
  (define (cipher top bottom output)

    (if (empty? top)
      output

      (let ((result : String (vigenere (car top) bottom)))
        (cipher
          (cdr top)
          (ord-string result)
          (append output (list result))
          ))))

  (cipher input key '() ))


(: waterfall (-> String String Boolean String))
(define (waterfall input key encrypt)
  ; Encrypts or decrypts strings using the waterfall algorithm, the input key
  ; is only used with the first chunk of the input, subsequence chunks' key is
  ; the previous chunk. chunk size is determined by the length of the key

  (let* ((key-list : (Listof Integer)
                   (ord-string
                     (sha1 (open-input-bytes
                             (string->bytes/locale key)))))

         (msg-list : (Listof String)
                   (split-list (string->list input) (length key-list))))

    (concat
      ((if encrypt
         waterfall-encrypt
         waterfall-decrypt)
       msg-list key-list))))

