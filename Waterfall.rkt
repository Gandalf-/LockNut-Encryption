#lang racket

(require openssl/sha1)
(provide (all-defined-out))


(define (inverse-list l)
  ; takes the inverse of the list
  ;
  ; @l  list of real

  (map (lambda (x) (* -1 x)) l))


(define (ord-string in)
  ; list of string to list of ints where the ints are the ascii value for the
  ; string character at that position
  ;
  ; @in   string

  (map
    char->integer
    (string->list in)))


(define (concat in)
  ; concatentate a list of strings
  ;
  ; @in   list of string

  (foldr string-append "" in))


(define (split-list l chunk)
  ; split the input list of characters into strings of chunk length
  ; 
  ; @l      list of char
  ; @chunk  int

  (let splitter ((result '())
                 (remaining l))

    (if (empty? remaining)
      result

      (let ((size (min chunk (length remaining))))
        (splitter 
          (append result 
                  (list (list->string (take remaining 
                                            size))))
          (list-tail remaining size)
          )))))


(define (extend l n)
  ; extend a list to length 'n' by appending it to itself
  ;
  ; @l  list of any
  ; @n  int

  (take (flatten (make-list n l)) n))


(define (drop-last l)
  ; drop the last element from a list
  ; 
  ; @l  list of any

  (take l (- (length l) 1)))


(define (vigenere input key)
  ; encrypts input with the key. Simple Viegenere Cipher
  ; substitution cipher, no negative values
  ;
  ; @input string
  ; @key   list of int

  (let ((characters (ord-string input)))
    (list->string
      (map
        (lambda (x y)
          (integer->char
            (bitwise-xor x y)))

        characters

        ; make the input and key the same size by extending the key if needed
        (extend key (length characters))
        ))))


(define (waterfall-encrypt input key)
  ; encrypts a list of characters broken into sublists. we work forwards,
  ; encrypting the current chunk with the next chunk until we get to the last
  ; chunk, which is encrypted with the argument key
  ;
  ; @input  list of string
  ; @key    list of int

  (map vigenere
       input
       (append 
         (list key)
         (map ord-string (drop-last input))
         )))


(define (waterfall-decrypt input key)
  ; decrypts a waterfall encrypted list of characters
  ; 
  ; @input  list of string
  ; @key    list of int

  (define (worker top bottom output)

    (if (empty? top)
      output

      (let ((result (vigenere (car top) bottom)))
        (worker
          (cdr top)
          (ord-string result)
          (append output (list result))
          ))))

  (worker input key '() ))


(define (waterfall input key encrypt)
  ; Encrypts or decrypts strings using the waterfall algorithm, the input key
  ; is only used with the first chunk of the input, subsequence chunks' key is
  ; the previous chunk. chunk size is determined by the length of the key
  ;
  ; @input    string
  ; @key      string
  ; @encrypt  bool

  (let* ((key-list    ; list of int
           (ord-string
             (sha1 (open-input-bytes
                     (string->bytes/locale key)))))

         (msg-list    ; list of string
           (split-list (string->list input) (length key-list))))

    (if (equal? encrypt #t)
      (concat
        (waterfall-encrypt msg-list key-list))

      (concat
        (waterfall-decrypt msg-list key-list)))) )
