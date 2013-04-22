(in-package "ACL2")
(include-book "doublecheck" :dir :teachpacks)
(include-book "testing" :dir :teachpacks)
(include-book "compression")

; Property checks that compress-rle creates a list
; that is divisble by 2
(defproperty compress-length-even
  (xs :value (random-list-of (random-between 0 255))
      :where (consp xs))
  (= 0 (mod (length (compress-rle xs 1 nil)) 2)))

; Theorem for above property
(defthm compress-length-even-thm
  (implies (consp xs)
           (= 0 (mod (length (compress-rle xs 1 nil)) 2))))

; Property checks that if all elements are the same,
; compress-rle creates a list with a specific length
; (((floor of (length of byte list / 255)) * 2) + 2)
; = length of result
(defproperty compress-length-specific
  (xs :value (random-list-of (random-between 0 0))
      :where (consp xs))
  (equal (length (compress-rle xs 1 nil))
         (+ (* (floor (length xs) 255) 2) 2)))

; Property checks to see that if no elements are
; repeated (as in twice in a row), compress-rle 
; creates a list that is double the length of
; the original
(defproperty compress-length-norepeat
  (xs :value (random-list-of (random-between 0 255))
      :where (and (consp (cdr xs))
                  (no-duplicatesp xs)))
  (= (* (length xs) 2)
     (length (compress-rle xs 1 nil))))

; Decompress-length-specific helper method
(defun decomp-help (xs)
  (if (consp xs)
      (+ (car xs) (if (consp (cddr xs))
                      (decomp-help (cddr xs))
                      0))
      nil))

; Property checks that decompress-rle creates
; a list that is the length (car xs) of the 
; original list xs, when xs is only two elements
(defproperty decompress-length-specific
  (xs :value (random-list-of (random-between 1 255))
      :where (and (consp xs) (evenp (length xs))))
  (= (length (decompress-rle xs -1 nil))
     (decomp-help xs)))

; Check-expect helper function to construct list of repeated element of length count
(defun check-help (val count rs)
  (if (< count 1)
      rs
      (check-help val (- count 1) (append rs (list val)))))

(check-expect (compress-rle '(2 2 3 3 4 4) 1 nil) (reverse '(2 2 2 3 2 4)))
(check-expect (compress-rle '(1 2 3 4) 1 nil) (reverse '(1 1 1 2 1 3 1 4)))
; This test assumes that if the count is greater than 255, the count will roll over to 1
(check-expect (compress-rle (check-help 100 256 nil) 1 nil) (reverse '(255 100 1 100)))

(check-expect (decompress-rle '(4 5 6 7) -1 nil) (reverse '(5 5 5 5 7 7 7 7 7 7)))
(check-expect (decompress-rle '(1 10 1 11 1 12 1 13) -1 nil) (reverse '(10 11 12 13)))

; Checks that the length of the 16-bit conversion string is 2/3 that of the original
; minus the padding
(defproperty img16-length-two-thirds
  (img :value (random-list-of (random-between 0 255))
       :where (and (consp img) (integerp (/ (length img) 3))))
  (= (/ (* 2 (length img)) 3) (- (length (img-16 img nil '(66 77 178 110 6 0 0 0 0 0 54 0 0 0 40 0 0 0 244 1 0
                                                     0 25 1 0 0 1 0 24 0 0 0 0 0 124 110 6 0 35 46    
                                                     0 35 46 0 0 0 0 0 0 0 0 0 0) 0)) 2)))

(check-expect (img-16 '(0 0 0) nil '(66 77 178 110 6 0 0 0 0 0 54 0 0 0 40 0 0 0 244 1 0 0 25 1 0 0 1 0 24 0 0 0 0 0 124 110 6 0 35 46 0 35 46 0 0 0 0 0 0 0 0 0 0)
                      0) '(0 0 0 0))
(check-expect (img-16 '(255 255 255) nil '(66 77 178 110 6 0 0 0 0 0 54 0 0 0 40 0 0 0 244 1 0 0 25 1 0 0 1 0 24 0 0 0 0 0 124 110 6 0 35 46 0 35 46 0 0 0 0 0 0 0 0 0 0)
                      0) (reverse '(255 127 0 0)))
(check-expect (img-16 '(100 233 144) nil '(66 77 178 110 6 0 0 0 0 0 54 0 0 0 40 0 0 0 244 1 0 0 25 1 0 0 1 0 24 0 0 0 0 0 124 110 6 0 35 46 0 35 46 0 0 0 0 0 0 0 0 0 0)
                      0) (reverse '(172 75 0 0)))