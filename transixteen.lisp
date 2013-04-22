(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "binary-io-utilities" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)
(include-book "avl-rational-keys" :dir :teachpacks)
(include-book "compression")
(include-book "transpix")

;(defun bw16 (xs rs))
; Converts each pixel to either black or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; Baseline contrast is 30
; xs = image data to be manipulated
; contrast = level to distinguish between black and white
; rs = transformed data

(defun bw16 (xs contrast rs)
  (if (endp xs)
      rs
      (let* ((six (16bit->RGB (car xs) (cadr xs)))
             (pix (+ (first six) (second six) (third six)))
             (trans (if (< pix contrast)
                       (RGB->16bit 0 0 0)
                       (reverse (RGB->16bit 31 31 31)))))
        (bw16 (cddr xs) contrast (append trans rs)))))

;;8bit format bw converter; contrast = 128
;(defun bw8 (xs contrast rs)
;  (if (endp xs)
;      rs
;      (let* ((pix (car xs))
;             (trans (if (< pix contrast)
;                       0
;                       255)))
;        (bw8 (cdr xs) contrast (append (list trans) rs)))))

				   
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
		
;--------------------------------------------------

;4bit format bw converter; contrast = 6
(defun bw4 (xs contrast rs)
  (if (endp xs)
      rs
      (let* ((pix (4bit-map (car xs)))
             (frst (if (< (car pix) contrast)
                       0
                       15))
             (scnd (if (< (car pix) contrast)
                       0
                       15))
             (trans (map-4bit frst scnd)))
        (bw4 (cdr xs) contrast (append trans rs)))))		
		
;(defun blu-w16 (xs contrast rs))
; Converts each pixel to either blue or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; To set blue, the blue channel is set to 16 and the others to 0
; xs = image data to be manipulated
; contrast = level to distinguish between blue and white
; rs = transformed data
(defun blu-w16 (xs contrast rs)
  (if (endp xs)
      rs
      (let* ((six (16bit->RGB (car xs) (cadr xs)))
             (pix (+ (first six) (second six) (third six)))
             (trans (if (< pix contrast)
                       (reverse (RGB->16bit 15 0 0))
                       (reverse (RGB->16bit 31 31 31)))))
        (blu-w16 (cddr xs) contrast (append trans rs)))))

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

;(defun grn-w16 (xs rs))
; Converts each pixel to either green or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; To set green, the green channel is set to 15 and the others to 0
; xs = image data to be manipulated
; contrast = level to distinguish between green and white
; rs = transformed data
(defun grn-w16 (xs contrast rs)
  (if (endp xs)
      rs
      (let* ((six (16bit->RGB (car xs) (cadr xs)))
             (pix (+ (first six) (second six) (third six)))
             (trans (if (< pix contrast)
                       (reverse (RGB->16bit 0 15 0))
                       (reverse (RGB->16bit 31 31 31)))))
        (grn-w16 (cddr xs) contrast (append trans rs)))))

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

;(defun red-w (xs rs))
; Converts each pixel to either red or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; To set red, the red channel is set to 15 and the others to 0
; xs = image data to be manipulated
; contrast = level to distinguish between red and white
; rs = transformed data
(defun red-w16 (xs contrast rs)
  (if (endp xs)
      rs
      (let* ((six (16bit->RGB (car xs) (cadr xs)))
             (pix (+ (first six) (second six) (third six)))
             (trans (if (< pix contrast)
                       (reverse (RGB->16bit 0 0 15))
                       (reverse (RGB->16bit 31 31 31)))))
        (red-w16 (cddr xs) contrast (append trans rs)))))

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

;;(defun two-tone8 (xs b1 b2 b3 w1 w2 w3 rs))
;; Converts each pixel to one of two specified colors based on the sum of the
;; BGR channels and whether they are above or below a specified threshold number.
;; Just use this to replace color palette, append image data to it
;; xs = color palette (usually starts at 54th byte of header, zero-indexed)
;; b1 = blue channel of first color
;; b2 = green channel of the first color
;; b3 = red channel of the first color
;; w1 = blue channel of second color
;; w2 = green channel of the second color
;; w3 = red channel of the second color
;; rs = transformed palette
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

;(defun gray (xs rs))
; Converts each pixel by calculating the average of the BGR channels and 
; setting all channels to the average
; xs = image data to be manipulated
; rs = transformed data
(defun gray16 (xs rs)
  (if (endp xs)
      rs
      (let* ((six (16bit->RGB (car xs) (cadr xs)))
             (pix (truncate (+ (first six) (second six) (third six)) 3)))
        (gray16 (cddr xs) (append (reverse (RGB->16bit pix pix pix)) rs)))))

;(defun palette-gray8 (xs rs)
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

;;(defun tint-blu8 (xs rs))
;; Similar to grayscale, converts each pixel by calculating the average of the BGR channels 
;; and setting the red and green channels to the average and increasing the blue channel
;; to 90 above the average. For 8 bit, basically just added 0 to front of list, recurse cddddr instead of cdddr.
;; Remember to replace palette with this in header, then append regular image data.
;; xs = color palette
;; rs = transformed palette
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

;;(defun tint-grn8 (xs rs))
;; Similar to grayscale, converts each pixel by calculating the average of the BGR channels 
;; and setting the red and blue channels to the average and increasing the green channel
;; to 50 above the average. For 8 bit, basically just added 0 to front of list, recurse cddddr instead of cdddr.
;; Remember to replace palette with this in header, then append regular image data.
;; xs = color palette
;; rs = transformed palette
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

;;(defun tint-red8 (xs rs))
;; Similar to grayscale, converts each pixel by calculating the average of the BGR channels 
;; and setting the green and blue channels to the average and increasing the red channel
;; to 100 above the average. For 8 bit, basically just added 0 to front of list, recurse cddddr instead of cdddr.
;; Remember to replace palette with this in header, then append regular image data.
;; xs = color palette
;; rs = transformed palette
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

;;(defun negatize8 (xs rs))
;; Converts each pixel to its complement by subtracting each channel value from
;; 255 and setting the difference as the values for the "negative" image.
;; Call it same way as above
;; xs = color palette
;; rs = transformed palette
(defun negatize8 (xs rs)
  (if (endp xs)
      rs
      (negatize8 (cddddr xs) (append (list 0 (- 255 (caddr xs))
                                             (- 255 (cadr xs))
                                             (- 255 (car xs))) rs))))

;;(defun rot-180 (xs))
;; Rotates the image 180 degrees, just reverses the image data
;; Call with pix-W8
;; xs = image data
(defun rot8-180 (xs)
  (reverse xs))
 
; ;(defun flip-H8 (n xs))
; ; Flips the image horizontally;  calls reverse to invert the order of
; ; the pixels in preparation for reversing each row of pixels
; ; n = row width in bytes. USE pix-W8
; ; xs = image data
 (defun flip-H8 (n xs)
    (quip n (reverse xs) nil))
 
; ;(defun flip-V8 (n xs))
; ; Flips the image vertically;  calls quip to reverse each line 
; ; then reverses the resulting list
; ; n = row width in bytes. USE pix-W8
; ; xs = image data
 (defun flip-V8 (n xs)
   (quip n xs nil))

;;between (x low up)
; ;tests whether or not a number is within a range
; ; x   - number to test
; ; low - number for x to be greater than in order to return true
; ; up  - upper limit for x to be
; (defun between (x low up)
;   (if (and (<= x up) (>= x low))
;       t
;       nil)) 
; 
;;fltr-hlp (xs up-b lo-b up-g lo-g up-r lo-r)
;;grays colors that are do not have close RGB values to those specified
;;if the red, blue, and green values are all within the range values given,
;;the color is kept; otherwise, gray it out.
;; xs     - list of bytes: blue then green then red
;; rs     - list of changed bytes; starts as nil
;; up-b   - upper limit of accepted blue values
;; lo-b   - lower limit of accepted blue values
;; up-g   - upper limit of accepted green values
;; lo-g   - lower limit of accepted green values
;; up-r   - upper limit of accepted red values
;; lo-r   - lower limit of accepted red values
;(defun fltr-hlp (xs rs up-b lo-b up-g lo-g up-r lo-r)
;  (if (endp xs)
;      rs
;      (mv-let (c-b c-g c-r)
;              (mv (car xs) (cadr xs) (caddr xs))
;              (if (and (between c-b lo-b up-b)
;                       (between c-g lo-g up-g)
;                       (between c-r lo-r up-r))
;                  (fltr-hlp (cdddr xs) 
;                            (append (list c-r c-g c-b ) rs) 
;                            up-b lo-b up-g lo-g up-r lo-r)
;                  (let* ((pix (truncate (+ c-r c-g c-b) 3)))
;                    (fltr-hlp (cdddr xs) 
;                              (append (list pix pix pix) rs)
;                              up-b lo-b up-g lo-g up-r lo-r))))))
;
;;fltr (xs r g b lev)
;;calculates the upper and lower bounds of the RGB values given a lev.
;;Hands the calculated upper and lower bounds to fltr-hlp2
;; lev - range of values above and below the real R,G,B values to accept 
;;       into the filter. 
;(defun fltr (xs r g b lev)
;  (if (consp xs)
;      (mv-let (up-b lo-b up-g lo-g up-r lo-r)
;              (mv (+ b lev) (- b lev) 
;                  (+ g lev) (- g lev) 
;                  (+ r lev) (- r lev))
;              (fltr-hlp xs nil up-b lo-b up-g lo-g up-r lo-r))
;      nil))
;	  
;; (defun crop (bmp x1 y1 x2 y2))
;; crops an image given certain xs and ys that represent the boundries in pixels.
;; bmp - the list of bytes
;; x1 - left boundry
;; y1 - top boundry
;; x2 - right boundry
;; y2 - bottom boundry
;
;
; ;(defun fmt-img (xs)
;; Appends enough byte(s) to each row to make it a multiple of 4 
;; xs = raw image data
;; txs = padded image data
;; nrow = new row width, which is the old height
;; pad = the padding added to make the new row a multiple of 4
; (defun fmt-img (xs txs nrow pad)
;   (if (endp xs)
;       txs
;       (let* ((splt (break-at-nth nrow xs))
;              (news (append (car splt) pad)))
;         (fmt-img (cadr splt) (append txs news) nrow pad))))
;
;;(defun unpad (width)
;; Determines if rows need to be padded and by how much
;; Returns the list to be appended to each row
;; width = row width in bytes
; (defun unpad (width)
;   (if (= (mod width 4) 0)
;       nil
;       (let* ((rem (mod width 4))
;              (pad (case rem
;                     (1 3)
;                     (2 2)
;                     (3 1))))
;         pad)))
;
;;(defun fmt-img (xs)
;; Appends enough byte(s) to each row to make it a multiple of 4 
;; xs = raw image data
;; txs = padded image data
;; nrow = new row width, which is the old height
;; pad = the padding added to make the new row a multiple of 4
; (defun unfmt-img (xs txs width unpad)
;   (if (endp xs)
;       txs
;       (let* ((splt (break-at-nth (+ width unpad) xs))
;              (strip (reverse (subseq (car splt) 0 width))))
;         (unfmt-img (cadr splt) (append strip txs) width unpad))))
;
;;(defun unpack (xs txs)
;; Creates a list of the pixels in the order stored in the tree
;; xs = AVL tree with pixel bytes
;; txs = image data
;(defun unpack (xs txs)
;  (if (endp xs)
;      (reverse txs)
;      (unpack (cdr xs) (append (list (cdar xs)) txs))))
;
;;(defun 90 (xs height width nr pos count diff tree)
;; Rotates image 90 degrees to the left
;; xs = original image data
;; height = image height in pixel
;; width = row width in pixel
;; nr = new row numbering                         start at 0
;; pos = byte position in pixel                   start at 0
;; count = old row number being processed         start at 1
;; diff = decreasing row length (as processed)    start at 0
;; tree = bytes stored by position in new image
;(defun rot90L (xs height width nr pos count diff tree)
;  (declare (xargs :guard (endp xs)
;                  :verify-guards nil
;                  :measure diff))
;  (if (endp xs)                         ;image is completely processed
;      (unpack (avl-flatten tree) nil)
;      (if (< nr width)                  ;new row
;              (if (< pos 3)             ;byte position in pixel
;                  (rot90L (cdr xs) height width nr (+ pos 1) count diff 
;                         (avl-insert tree (- (+ (* nr 3) pos (* count (* (- height 1) 3))) 
;                                             (* diff 3)) (car xs)))                
;                  (rot90L xs height width (+ nr 1) 0 (+ count 1) diff tree)) 
;              (rot90L xs height width 0 0 1 (+ diff 1) tree))))
;			  
;;(defun 90R (xs height width nr pos count diff tree)
;; Rotates image 90 degrees to the left
;; xs = original image data
;; height = image height in pixel
;; width = row width in pixel
;; nr = new row numbering                         start at width - 1
;; pos = byte position in pixel                   start at 0
;; count = old row number being processed         start at 3
;; diff = decreasing row length (as processed)    start at 0
;; tree = bytes stored by position in new image
;(defun rot90R (xs height width nr pos cnt diff tree)
;  (declare (xargs :guard (endp xs)
;                  :verify-guards nil
;                  :measure diff))
;  (if (endp xs)                         ;image is completely processed
;      (unpack (avl-flatten tree) nil)
;      (if (> nr -1)                  ;new row
;              (if (< pos 3)             ;byte position in pixel
;                  (rot90R (cdr xs) height width nr (+ pos 1) cnt diff 
;                         (avl-insert tree (+ (* nr 3) pos (* cnt (* (- height 1) 3))
;                                             (* diff 3)) (car xs)))                
;                  (rot90R xs height width (- nr 1) 0 (- cnt 1) diff tree)) 
;              (rot90R xs height width (- width 1) 0 3 (+ diff 1) tree))))
;
;;(defun double (n xs rs))
; ; Pulls one row from the list, copies it, then appends
; ; each reversed row to double the height of the image
; ; n = row width in bytes
; ; xs = image data
; ; rs = new image with doubled height
; (defun double (n xs rs)
;   (if (endp xs)
;       rs
;       (let* ((spline (break-at-nth n xs)))
;         (double n (cadr spline) (append (reverse (car spline)) (reverse (car spline)) rs)))))
;
;;(defun magnify-tail (xs rs))
;; Copies each pixel and appends it to the original to double the width of the row
;; xs = image data
;; rs = new image with doubled row size
;(defun magnify-tail (xs rs)
;   (if (endp xs)
;       rs
;       (let (( 1st (nfix (first xs)))
;               (2nd (nfix (second xs)))
;               (3rd (nfix (third xs))))
;                
;               (magnify-tail (cdddr xs)
;                                (append (list 3rd 2nd 1st 3rd 2nd 1st) rs)))))

;