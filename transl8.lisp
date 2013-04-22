(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "binary-io-utilities" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)
(include-book "avl-rational-keys" :dir :teachpacks)
(include-book "compression")
(include-book "transpix")

;(defun bw8 (xs contrast rs))
; Converts each pixel to either black or white (in the palette) based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; xs = color palette
; contrast = level to distinguish between black and white, default = 250
; rs = transformed palette
(defun bw8 (xs contrast rs)
  (if (endp xs)
      rs
      (let ((pix (+ (nfix (first xs))
                    (nfix (second xs))
                    (nfix (third xs)))))
        (if (< pix contrast)
            (bw8 (cddddr xs) contrast
                 (append (list 0 0 0 0) rs))
            (bw8 (cddddr xs) contrast
                 (append (list 0 255 255 255) rs))))))

;(defun blu-w8 (xs contrast rs))
; Converts each pixel (in the palette) to either blue or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; To set blue, the blue channel is set to 150 and the others to 0
; xs = image data to be manipulated
; contrast = level to distinguish between blue and white, default = 250
; rs = transformed data
(defun blu-w8 (xs contrast rs)
  (if (endp xs)
      rs
      (let ((pix (+ (nfix (first xs))
                    (nfix (second xs))
                    (nfix (third xs)))))
        (if (< pix contrast)
            (blu-w8 (cddddr xs) contrast
                   (append (list 0 0 0 150) rs))
            (blu-w8 (cddddr xs) contrast
                   (append (list 0 255 255 255) rs))))))

;(defun grn-w8 (xs contrast rs))
; Converts each pixel (in the palette) to either green or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; To set green, the green channel is set to 150 and the others to 0
; xs = color palette
; contrast = level to distinguish between green and white, default = 250
; rs = transformed palette
(defun grn-w8 (xs contrast rs)
  (if (endp xs)
      rs
      (let ((pix (+ (nfix (first xs))
                    (nfix (second xs))
                    (nfix (third xs)))))
        (if (< pix contrast)
            (grn-w8 (cddddr xs) contrast
                   (append (list 0 0 150 0) rs))
            (grn-w8 (cddddr xs) contrast
                   (append (list 0 255 255 255) rs))))))

;(defun red-w8 (xs contrast rs))
; Converts each pixel (in the palette) to either red or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; To set red, the red channel is set to 150 and the others to 0
; xs = color palette
; contrast = level to distinguish between red and white, default = 250
; rs = transformed palette
(defun red-w8 (xs contrast rs)
  (if (endp xs)
      rs
      (let ((pix (+ (nfix (first xs))
                    (nfix (second xs))
                    (nfix (third xs)))))
        (if (< pix contrast)
            (red-w8 (cddddr xs) contrast
                   (append (list 0 150 0 0) rs))
            (red-w8 (cddddr xs) contrast
                   (append (list 0 255 255 255) rs))))))

;(defun two-tone8 (xs b1 b2 b3 w1 w2 w3 rs))
; Converts each pixel to one of two specified colors based on the sum of the
; BGR channels and whether they are above or below a specified threshold number.
; Just use this to replace color palette, append image data to it
; xs = color palette (usually starts at 54th byte of header, zero-indexed)
; b1 = blue channel of first color
; b2 = green channel of the first color
; b3 = red channel of the first color
; w1 = blue channel of second color
; w2 = green channel of the second color
; w3 = red channel of the second color
; rs = transformed palette
(defun two-tone8 (xs b1 b2 b3 w1 w2 w3 rs)
  (if (endp xs)
      rs
      (let ((pix (+ (nfix (first xs))
                    (nfix (second xs))
                    (nfix (third xs)))))
        (if (< pix 250)
            (two-tone8 (cddddr xs) b1 b2 b3 w1 w2 w3
                       (append (list 0 b3 b2 b1) rs))
            (two-tone8 (cddddr xs) b1 b2 b3 w1 w2 w3
                       (append (list 0 w3 w2 w1) rs))))))

;(defun gray8 (xs rs)
; Converts all colors in color palette to grayscale
; Just call it and replace palette in header with this,
; then append regular image data
; xs = color palette
; rs = transformed palette
(defun gray8 (xs rs)
  (if (endp xs)
      rs
      (let* ((pix (truncate(+ (nfix (first xs))
                              (nfix (second xs))
                              (nfix (third xs))) 3)))
        (gray8 (cddddr xs)
               (append (list 0 pix pix pix) rs)))))

;(defun tint-blu8 (xs rs))
; Similar to grayscale, converts each pixel by calculating the average of the BGR channels 
; and setting the red and green channels to the average and increasing the blue channel
; to 90 above the average. For 8 bit, basically just added 0 to front of list, recurse cddddr instead of cdddr.
; Remember to replace palette with this in header, then append regular image data.
; xs = color palette
; rs = transformed palette
(defun tint-blu8 (xs rs)
  (if (endp xs)
      rs
      (let* ((pix (truncate(+ (nfix (first xs))
                              (nfix (second xs))
                              (nfix (third xs))) 3)))
        (if (< pix 165)
            (tint-blu8 (cddddr xs)
                       (append (list 0 pix pix (+ pix 90)) rs)) 
            (tint-blu8 (cddddr xs)
                       (append (list 0 pix pix 255) rs))))))

;(defun tint-grn8 (xs rs))
; Similar to grayscale, converts each pixel by calculating the average of the BGR channels 
; and setting the red and blue channels to the average and increasing the green channel
; to 50 above the average. For 8 bit, basically just added 0 to front of list, recurse cddddr instead of cdddr.
; Remember to replace palette with this in header, then append regular image data.
; xs = color palette
; rs = transformed palette
(defun tint-grn8 (xs rs)
  (if (endp xs)
      rs
      (let* ((pix (truncate(+ (nfix (first xs))
                              (nfix (second xs))
                              (nfix (third xs))) 3)))
        (if (< pix 205)
            (tint-grn8 (cddddr xs)
                       (append (list 0 pix (+ pix 50) pix) rs)) 
            (tint-grn8 (cddddr xs)
                       (append (list 0 pix 255 pix) rs))))))
;(defun tint-red8 (xs rs))
; Similar to grayscale, converts each pixel by calculating the average of the BGR channels 
; and setting the green and blue channels to the average and increasing the red channel
; to 100 above the average. For 8 bit, basically just added 0 to front of list, recurse cddddr instead of cdddr.
; Remember to replace palette with this in header, then append regular image data.
; xs = color palette
; rs = transformed palette
(defun tint-red8 (xs rs)
  (if (endp xs)
      rs
      (let* ((pix (truncate(+ (nfix (first xs))
                              (nfix (second xs))
                              (nfix (third xs))) 3)))
        (if (< pix 155)
            (tint-red8 (cddddr xs)
                       (append (list 0 (+ pix 100) pix pix) rs)) 
            (tint-red8 (cddddr xs)
                       (append (list 0 255 pix pix) rs))))))

;(defun negatize8 (xs rs))
; Converts each pixel to its complement by subtracting each channel value from
; 255 and setting the difference as the values for the "negative" image.
; Call it same way as above
; xs = color palette
; rs = transformed palette
(defun negatize8 (xs rs)
  (if (endp xs)
      rs
      (negatize8 (cddddr xs) (append (list 0 (- 255 (caddr xs))
                                             (- 255 (cadr xs))
                                             (- 255 (car xs))) rs))))

 ;(defun flip-H8 (n xs))
 ; Flips the image horizontally by reversing each row;  
 ; n = row width in bytes. USE pix-W8
 ; xs = image data
 (defun flip-H8 (n xs)
    (quip n (reverse xs) nil))
 
;(defun rot90L8 (xs height width nr diff tree)
; Rotates image 90 degrees to the left
; xs = original image data
; height = image height in pixel
; width = row width in pixel
; nr = new row numbering                         start at 0
; diff = decreasing row length (as processed)    start at 0
; tree = bytes stored by position in new image
(defun rot90L8 (xs height width nr diff tree)
  (declare (xargs :guard (endp xs)
                  :verify-guards nil
                  :measure diff))
  (if (endp xs)                          ;image is completely processed
      (unpack (avl-flatten tree) nil)
      (if (< nr width)                  ;new row
              (rot90L8 (cdr xs) height width (+ nr 1) diff 
                         (avl-insert tree (- (+ (* nr height) (- height 1)) 
                                             diff) (car xs)))                
              (rot90L8 xs height width 0 (+ diff 1) tree))))
			  
;(defun rot90R8 (xs height width nr cnt tree)
; Rotates image 90 degrees to the right
; xs = original image data
; height = image height in pixel
; width = row width in pixel
; nr = new row numbering                         start at width - 1
; cnt = increasing row length (as processed)    start at 0
; tree = bytes stored by position in new image
(defun rot90R8 (xs height width nr cnt tree)
  (declare (xargs :guard (endp xs)
                  :verify-guards nil
                  :measure cnt))
  (if (endp xs)
      (unpack (avl-flatten tree) nil)
      (if (> nr -1)
          (rot90R8 (cdr xs) height width (- nr 1) cnt
                   (avl-insert tree (+ (* nr height) cnt) (car xs)))
          (rot90R8 xs height width (- width 1) (+ cnt 1) tree))))

;(defun crop8 (params xs rs))
 ; Pulls one row from the list, copies it, then appends
 ; each reversed row to double the height of the image
 ; params = the parameters specified by the user for how many pixels to crop in both w and h
 ; xs = image data
 ; rs = new image that has been cropped
  (defun crop8 (params xs rs)
    (if (> (len xs) (* (car params) (cadddr params)))
        (crop8 params (nthcdr (car params) xs) (append rs (take (- (car params) (cadr params)) xs)))
        rs
        ))

;(defun magnify8 (xs rs))
; Magnerrrrrific
  (defun magnify8 (xs rs)
    (if (endp xs)
        rs
        (magnify8 (cdr xs)
                  (append (list (car xs) (car xs))
                          rs))))