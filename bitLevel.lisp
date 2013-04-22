(in-package "ACL2")

; Note: all operations use little-endian format

; (numeral->nat b xs)
; Takes a list of numbers and converts it
; to an integer in whatever base you specify
; b - the base
; xs - list of numbers
(defun numeral->nat (b xs)
  (if (consp xs)
      (+ (car xs) (* b (numeral->nat b (cdr xs))))
      0))

; (nat->bits n)
; Converts a natural number to a list of bits
; n - number
(defun nat->bits (n)
    (if (zp n)
        nil
        (cons (mod n 2)
              (nat->bits (floor n 2)))))

; (nat->bytes n)
; Converts a natural number to list of bytes
; n - number
(defun nat->bytes (n)
    (if (zp n)
        nil
        (cons (mod n 256)
              (nat->bytes (floor n 256)))))

; (bytes->nat xs)
; Converts a list of bytes to a natural number
; xs - list of bytes
(defun bytes->nat (xs)
  (numeral->nat 256 xs))
  
; (bits->nat xs)
; Converts a list of bits to a natural number
; xs - list of bits
(defun bits->nat (xs)
  (numeral->nat 2 xs))