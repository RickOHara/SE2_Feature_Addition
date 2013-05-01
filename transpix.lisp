(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "binary-io-utilities" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)
(include-book "avl-rational-keys" :dir :teachpacks)


;(defun sepia (xs rs))
; transforms each pixel value to a sepia value
; xs = image data to be manipulated
; rs = transformed data
(defun sepia (xs rs size curx cury)
   (if (endp xs)
       rs
       (let* ((redIn (caddr xs))
              (greenIn (cadr xs))
              (blueIn (car xs))
              (redOut (min 255 (floor (+ (* redIn 393/1000)
                                  (* greenIn 769/1000)
                                  (* blueIn 189/1000)) 1)))
              (greenOut (min 255 (floor (+ (* redIn 349/1000)
                                    (* greenIn 686/1000)
                                    (* blueIn 168/1000)) 1)))
              (blueOut (min 255 (floor (+ (* redIn 272/1000)
                                   (* greenIn 534/1000)
                                   (* blueIn 131/1000)) 1))))
             
			(if (and (and (> curx (car size)) (< curx (+ (car size) (caddr size))))
				    (and (> cury (cadr size)) (< cury (+ (cadr size) (cadddr size)))))
             		(sepia (cdddr xs) 
                      (append (list redOut greenOut blueOut) rs)
                    		size 
						(mod (+ curx 1) (fifth size))
						(if (>= (+ curx 1) (fifth size))
							(+ cury 1)
							cury))
             		(sepia (cdddr xs) 
                			(append (list (third xs) (second xs) (first xs)) rs)
                    		size 
						(mod (+ curx 1) (fifth size))
						(if (>= (+ curx 1) (fifth size))
							(+ cury 1)
							cury))))))


; blur - duplicate of sepia
(defun blur (xs rs size curx cury)
   (if (endp xs)
       rs
       (let* (
  		    (redIn (third xs))
              (greenIn (second xs))
              (blueIn (first xs))
  		    (redNext (if (equal nil (sixth xs)) redIn (sixth xs)))
              (greenNext (if (equal nil (fifth xs)) greenIn (fifth xs)))
              (blueNext (if (equal nil (fourth xs)) blueIn (fourth xs)))           
  		    (redNextNext (if (equal nil (ninth xs)) redNext (ninth xs)))
              (greenNextNext (if (equal nil (eighth xs)) greenNext (eighth xs)))
              (blueNextNext (if (equal nil (seventh xs)) blueNext (seventh xs)))           
 		    ;(redNextNextNext (if (equal nil (nth 11 xs)) redNext (nth 11 xs)))
              ;(greenNextNextNext (if (equal nil (nth 10 xs)) greenNext (nth 10 xs)))
              ;(blueNextNextNext (if (equal nil (nth 9 xs)) blueNext (nth 9 xs)))     
              (redNextNextNext (if (equal nil (third rs)) redIn (third rs)))
              (greenNextNextNext (if (equal nil (second rs)) greenIn (second rs)))
              (blueNextNextNext (if (equal nil (first rs)) blueIn (first rs)))
  		    (redOut (floor (/ (+ redIn redNext redNextNext redNextNextNext) 4)1))
              (greenOut (floor (/ (+ greenIn greenNext greenNextNext greenNextNextNext ) 4)1))
              (blueOut (floor(/ (+ blueIn blueNext blueNextNext blueNextNextNext) 4)1))
             )


		(if (and (and (> curx (car size)) (< curx (+ (car size) (caddr size))))
			    (and (> cury (cadr size)) (< cury (+ (cadr size) (cadddr size)))))
             (blur (cdddr xs) 
                   (append (list redOut greenOut blueOut) rs)
                    		size 
						(mod (+ curx 1) (fifth size))
						(if (>= (+ curx 1) (fifth size))
							(+ cury 1)
							cury))
       		(blur (cdddr xs)
               
                			(append (list (third xs) (second xs) (first xs)) rs)
                    		size 
						(mod (+ curx 1) (fifth size))
						(if (>= (+ curx 1) (fifth size))
							(+ cury 1)
							cury))))))


;(defun bw (xs rs))
; Converts each pixel to either black or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; xs = image data to be manipulated
; rs = transformed data
(defun bw (xs contrast rs size curx cury)
	(if (endp xs)
		rs
		(let ((pix (+ (nfix (first xs))
			(nfix (second xs))
			(nfix (third xs)))))
			(if (and (and (> curx (car size)) (< curx (+ (car size) (caddr size))))
				    (and (> cury (cadr size)) (< cury (+ (cadr size) (cadddr size)))))
				(if (< pix contrast)
					(bw (cdddr xs) contrast
						(append (list 0 0 0) rs) 
						size 
						(mod (+ curx 1) (fifth size))
						(if (>= (+ curx 1) (fifth size))
							(+ cury 1)
							cury))
					(bw (cdddr xs) contrast
						(append (list 255 255 255) rs) 
						size  
						(mod (+ curx 1) (fifth size))
						(if (>= (+ curx 1) (fifth size))
							(+ cury 1)
							cury)))
				(bw (cdddr xs) contrast
                (append (list (third xs) (second xs) (first xs)) rs)
					size
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))))))

;(defun blu-w (xs rs))
; Converts each pixel to either blue or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; To set blue, the blue channel is set to 150 and the others to 0
; xs = image data to be manipulated
; rs = transformed data
(defun blu-w (xs contrast rs)
  (if (endp xs)
      rs
      (let ((pix (+ (nfix (first xs))
                    (nfix (second xs))
                    (nfix (third xs)))))
        (if (< pix contrast)
            (blu-w (cdddr xs) contrast
                   (append (list 0 0 150) rs))
            (blu-w (cdddr xs) contrast
                   (append (list 255 255 255) rs))))))

;(defun grn-w (xs rs))
; Converts each pixel to either green or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; To set green, the green channel is set to 150 and the others to 0
; xs = image data to be manipulated
; rs = transformed data
(defun grn-w (xs contrast rs)
  (if (endp xs)
      rs
      (let ((pix (+ (nfix (first xs))
                    (nfix (second xs))
                    (nfix (third xs)))))
        (if (< pix contrast)
            (grn-w (cdddr xs) contrast
                   (append (list 0 150 0) rs))
            (grn-w (cdddr xs) contrast
                   (append (list 255 255 255) rs))))))

;(defun red-w (xs rs))
; Converts each pixel to either red or white based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; To set red, the red channel is set to 150 and the others to 0
; xs = image data to be manipulated
; rs = transformed data
(defun red-w (xs contrast rs)
  (if (endp xs)
      rs
      (let ((pix (+ (nfix (first xs))
                    (nfix (second xs))
                    (nfix (third xs)))))
        (if (< pix contrast)
            (red-w (cdddr xs) contrast
                   (append (list 150 0 0) rs))
            (red-w (cdddr xs) contrast
                   (append (list 255 255 255) rs))))))

;(defun two-tone (xs b1 b2 b3 w1 w2 w3 rs))
; Converts each pixel to one of two specifie colors based on the sum of the
; BGR channels and whether they are above or below a specified threshold number
; xs = image data to be manipulated
; b1 = blue channel of first color
; b2 = green channel of the first color
; b3 = red channel of the first color
; w1 = blue channel of second color
; w2 = green channel of the second color
; w3 = red channel of the second color
; rs = transformed data
(defun two-tone (xs b1 b2 b3 w1 w2 w3 rs size curx cury)
  (if (endp xs)
      rs
      (let ((pix (+ (nfix (first xs))
                    (nfix (second xs))
                    (nfix (third xs)))))
			(if (and (and (> curx (first size)) (< curx (+ (first size) (third size))))
				    (and (> cury (second size)) (< cury (+ (second size) (fourth size)))))
		        (if (< pix 250)
		            (two-tone (cdddr xs) b1 b2 b3 w1 w2 w3
		                      (append (list b3 b2 b1) rs)
					size
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))
		            (two-tone (cdddr xs) b1 b2 b3 w1 w2 w3
		                      (append (list w3 w2 w1) rs)
					size
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury)))
            (two-tone (cdddr xs) b1 b2 b3 w1 w2 w3
                (append (list (third xs) (second xs) (first xs)) rs) 
					size
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))))))

;(defun gray (xs rs))
; Converts each pixel by calculating the average of the BGR channels and 
; setting all channels to the average
; xs = image data to be manipulated
; rs = transformed data
(defun gray (xs rs size curx cury)
  (if (endp xs)
      rs
      (let* ((pix (truncate(+ (nfix (first xs))
                              (nfix (second xs))
                              (nfix (third xs))) 3)))
			(if (and (and (> curx (first size)) (< curx (+ (first size) (third size))))
				    (and (> cury (second size)) (< cury (+ (second size) (fourth size)))))
		        (gray (cdddr xs)
		               (append (list pix pix pix) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
					(+ cury 1)
					cury))
       		   (gray (cdddr xs)
                (append (list (third xs) (second xs) (first xs)) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))))))

;(defun tint-blu (xs rs))
; Similar to grayscale, converts each pixel by calculating the average of the BGR channels 
; and setting the red and green channels to the average and increasing the blue channel
; to 90 above the average
; xs = image data to be manipulated
; rs = transformed data
(defun tint-blu (xs rs size curx cury)
  (if (endp xs)
      rs
      (let* ((pix (truncate(+ (nfix (first xs))
                              (nfix (second xs))
                              (nfix (third xs))) 3)))
			(if (and (and (> curx (first size)) (< curx (+ (first size) (third size))))
				    (and (> cury (second size)) (< cury (+ (second size) (fourth size)))))
        (if (< pix 165)
            (tint-blu (cdddr xs)
                      (append (list pix pix (+ pix 90)) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))
            (tint-blu (cdddr xs)
                      (append (list pix pix 255) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury)))
     	(tint-blu (cdddr xs)
                (append (list (third xs) (second xs) (first xs)) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))))))

;(defun tint-grn (xs rs))
; Similar to grayscale, converts each pixel by calculating the average of the BGR channels 
; and setting the red and blue channels to the average and increasing the green channel
; to 50 above the average
; xs = image data to be manipulated
; rs = transformed data
(defun tint-grn (xs rs size curx cury)
  (if (endp xs)
      rs
      (let* ((pix (truncate(+ (nfix (first xs))
                              (nfix (second xs))
                              (nfix (third xs))) 3)))
			(if (and (and (> curx (first size)) (< curx (+ (first size) (third size))))
				    (and (> cury (second size)) (< cury (+ (second size) (fourth size)))))
        (if (< pix 205)
            (tint-grn (cdddr xs)
                      (append (list pix (+ pix 50) pix) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))
            (tint-grn (cdddr xs)
                      (append (list pix 255 pix) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury)))
     	(tint-grn (cdddr xs)
                (append (list (third xs) (second xs) (first xs)) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))))))

;(defun tint-red (xs rs))
; Similar to grayscale, converts each pixel by calculating the average of the BGR channels 
; and setting the blue and green channels to the average and increasing the red channel
; to 100 above the average
; xs = image data to be manipulated
; rs = transformed data
(defun tint-red (xs rs size curx cury)
  (if (endp xs)
      rs
      (let* ((pix (truncate(+ (nfix (first xs))
                              (nfix (second xs))
                              (nfix (third xs))) 3)))
			(if (and (and (> curx (first size)) (< curx (+ (first size) (third size))))
				    (and (> cury (second size)) (< cury (+ (second size) (fourth size)))))
        (if (< pix 155)
            (tint-red (cdddr xs)
                      (append (list (+ pix 100) pix pix) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))
            (tint-red (cdddr xs)
                      (append (list 255 pix pix) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury)))
     	(tint-red (cdddr xs)
                (append (list (third xs) (second xs) (first xs)) rs)
					size 
					(mod (+ curx 1) (fifth size))
					(if (>= (+ curx 1) (fifth size))
						(+ cury 1)
						cury))))))

;(defun negatize (xs rs))
; Converts each pixel to its complement by subtracting each channel value from
; 255 and setting the difference as the values for the "negative" image
; xs = image data to be manipulated
; rs = transformed data
(defun negatize (xs rs size curx cury)
  (if (endp xs)
      rs
			(if (and (and (> curx (first size)) (< curx (+ (first size) (third size))))
				    (and (> cury (second size)) (< cury (+ (second size) (fourth size)))))
      (negatize (cdddr xs) 
                (append (list (- 255 (caddr xs))
                                         (- 255 (cadr xs))
                                         (- 255 (car xs))) rs)
                size
			(mod (+ curx 1) (fifth size))
			(if (>= (+ curx 1) (fifth size))
				(+ cury 1)
				cury))
    (negatize (cdddr xs) 
                (append (list (third xs) (second xs) (first xs)) rs)
                size
			(mod (+ curx 1) (fifth size))
			(if (>= (+ curx 1) (fifth size))
				(+ cury 1)
				cury)))))

;(defun rot-180 (xs rs))
; Rotates the image 180 degrees; calls reorder to invert the order of
; the color channels prior to reversing the entire list (xs)
(defun rot-180 (xs rs)
  (if (endp xs)
      rs
      (rot-180 (cdddr xs) (append (list (car xs) 
                                        (cadr xs) 
                                        (caddr xs)) rs))))

;(defun reorder (xs rs))
;  Reverses color channel of each pixel; BGR to RGB
;  in order to get non-tail recursive version, must reverse the list this yields
(defun reorder (xs rs)
  (if (endp xs)
      rs
      (reorder (cdddr xs) (append (list (car xs)
                                        (cadr xs)
                                        (caddr xs)) rs))))

 ;(defun quip (n xs rs))
 ; Pulls one row from the list and sends to reverse, then appends
 ; each reversed row to produce a horizonally flipped image
 ; n = row width in bytes
 ; xs = image data
 (defun quip (n xs rs)
   (if (endp xs)
       rs
       (let* ((spline (break-at-nth n xs)))
         (quip n (cadr spline) (append (car spline) rs)))))
 
 ;(defun flip-H (n xs))
 ; Flips the image horizontally;  calls reorder to invert the order of
 ; the color channels in preparation for reversing each row of pixels
 ; n = row width in bytes. USE pix-W
 ; xs = image data
 (defun flip-H (n xs)
   (let* ((prep (reorder xs nil)))
    (quip n prep nil)))
 
 ;(defun flip-V (n xs))
 ; Flips the image vertically;  calls quip to reverse each line 
 ; then reverses the resulting list
 ; n = row width in bytes. USE pix-W
 ; xs = image data
 (defun flip-V (n xs)
   (quip n xs nil))

;between (x low up)
 ;tests whether or not a number is within a range
 ; x   - number to test
 ; low - number for x to be greater than in order to return true
 ; up  - upper limit for x to be
 (defun between (x low up)
   (if (and (<= x up) (>= x low))
       t
       nil)) 
 
;fltr-hlp (xs up-b lo-b up-g lo-g up-r lo-r)
;grays colors that are do not have close RGB values to those specified
;if the red, blue, and green values are all within the range values given,
;the color is kept; otherwise, gray it out.
; xs     - list of bytes: blue then green then red
; rs     - list of changed bytes; starts as nil
; up-b   - upper limit of accepted blue values
; lo-b   - lower limit of accepted blue values
; up-g   - upper limit of accepted green values
; lo-g   - lower limit of accepted green values
; up-r   - upper limit of accepted red values
; lo-r   - lower limit of accepted red values
(defun fltr-hlp (xs rs up-b lo-b up-g lo-g up-r lo-r)
  (if (endp xs)
      rs
      (mv-let (c-b c-g c-r)
              (mv (car xs) (cadr xs) (caddr xs))
              (if (and (between c-b lo-b up-b)
                       (between c-g lo-g up-g)
                       (between c-r lo-r up-r))
                  (fltr-hlp (cdddr xs) 
                            (append (list c-r c-g c-b ) rs) 
                            up-b lo-b up-g lo-g up-r lo-r)
                  (let* ((pix (truncate (+ c-r c-g c-b) 3)))
                    (fltr-hlp (cdddr xs) 
                              (append (list pix pix pix) rs)
                              up-b lo-b up-g lo-g up-r lo-r))))))

;fltr (xs r g b lev)
;calculates the upper and lower bounds of the RGB values given a lev.
;Hands the calculated upper and lower bounds to fltr-hlp2
; lev - range of values above and below the real R,G,B values to accept 
;       into the filter. 
(defun fltr (xs r g b lev)
   (declare (xargs :guard (endp xs)
                  :verify-guards nil
                  :measure diff))
  (if (consp xs)
      (mv-let (up-b lo-b up-g lo-g up-r lo-r)
              (mv (+ b lev) (- b lev) 
                  (+ g lev) (- g lev) 
                  (+ r lev) (- r lev))
              (fltr-hlp xs nil up-b lo-b up-g lo-g up-r lo-r))
      nil))
	  
; (defun crop (bmp x1 y1 x2 y2))
; crops an image given certain xs and ys that represent the boundries in pixels.
; bmp - the list of bytes
; x1 - left boundry
; y1 - top boundry
; x2 - right boundry
; y2 - bottom boundry


;(defun padding (width)
; Determines if rows need to be padded and by how much
; width = row width in bytes
 (defun padding (width)
   (if (= (mod width 4) 0)
       nil
       (let* ((rem (mod width 4))
              (pad (case rem
                     (1 (list 0 0 0))
                     (2 (list 0 0))
                     (3 (list 0)))))
         pad)))
 
 ;(defun fmt-img (xs txs nrow pad)
; Appends enough byte(s) to each row to make it a multiple of 4 
; xs = raw image data
; txs = padded image data
; nrow = new row width, which is the old height
; pad = the padding added to make the new row a multiple of 4
 (defun fmt-img (xs txs nrow pad)
   (if (endp xs)
       txs
       (let* ((splt (break-at-nth nrow xs))
              (news (append (car splt) pad)))
         (fmt-img (cadr splt) (append txs news) nrow pad))))

;(defun unpad (width)
; Determines if rows need to be padded and by how much
; Returns the list to be appended to each row
; width = row width in bytes
 (defun unpad (width)
   (if (= (mod width 4) 0)
       nil
       (let* ((rem (mod width 4))
              (pad (case rem
                     (1 3)
                     (2 2)
                     (3 1))))
         pad)))

;(defun unfmt-img (xs txs width unpad)
; Strips the padding of rows prior to image transformation
; xs = raw image data
; txs = unpadded image data
; width = row width
; unpad = the number of bytes that need to be stripped
 (defun unfmt-img (xs txs width unpad)
   (if (endp xs)
       txs
       (let* ((splt (break-at-nth (+ width unpad) xs))
              (strip (reverse (subseq (car splt) 0 width))))
         (unfmt-img (cadr splt) (append strip txs) width unpad))))

;(defun unpack (xs txs)
; Creates a list of the pixels in the order stored in the tree
; xs = AVL tree with pixel bytes
; txs = image data
(defun unpack (xs txs)
  (if (endp xs)
      (reverse txs)
      (unpack (cdr xs) (append (list (cdar xs)) txs))))

;(defun 90L (xs height width nr pos count diff tree)
; Rotates image 90 degrees to the left
; xs = original image data
; height = image height in pixel
; width = row width in pixel
; nr = new row numbering                         start at 0
; pos = byte position in pixel                   start at 0
; count = old row number being processed         start at 1
; diff = decreasing row length (as processed)    start at 0
; tree = bytes stored by position in new image
(defun rot90L (xs height width nr pos count diff tree)
  (declare (xargs :guard (endp xs)
                  :verify-guards nil
                  :measure diff))
  (if (endp xs)                         ;image is completely processed
      (unpack (avl-flatten tree) nil)
      (if (< nr width)                  ;new row
              (if (< pos 3)             ;byte position in pixel
                  (rot90L (cdr xs) height width nr (+ pos 1) count diff 
                         (avl-insert tree (- (+ (* nr 3) pos (* count (* (- height 1) 3))) 
                                             (* diff 3)) (car xs)))                
                  (rot90L xs height width (+ nr 1) 0 (+ count 1) diff tree)) 
              (rot90L xs height width 0 0 1 (+ diff 1) tree))))
			  
;(defun 90R (xs height width nr pos count diff tree)
; Rotates image 90 degrees to the right
; xs = original image data
; height = image height in pixel
; width = row width in pixel
; nr = new row numbering                         start at width - 1
; pos = byte position in pixel                   start at 0
; count = old row number being processed         start at 3
; diff = decreasing row length (as processed)    start at 0
; tree = bytes stored by position in new image
(defun rot90R (xs height width nr pos cnt diff tree)
  (declare (xargs :guard (endp xs)
                  :verify-guards nil
                  :measure diff))
  (if (endp xs)                         ;image is completely processed
      (unpack (avl-flatten tree) nil)
      (if (> nr -1)                  ;new row
              (if (< pos 3)             ;byte position in pixel
                  (rot90R (cdr xs) height width nr (+ pos 1) cnt diff 
                         (avl-insert tree (+ (* nr 3) pos (* cnt (* (- height 1) 3))
                                             (* diff 3)) (car xs)))                
                  (rot90R xs height width (- nr 1) 0 (- cnt 1) diff tree)) 
              (rot90R xs height width (- width 1) 0 3 (+ diff 1) tree))))

;(defun double (n xs rs))
 ; Pulls one row from the list, copies it, then appends
 ; each reversed row to double the height of the image
 ; n = row width in bytes
 ; xs = image data
 ; rs = new image with doubled height
 (defun double (n xs rs)
   (if (endp xs)
       rs
       (let* ((spline (break-at-nth n xs)))
         (double n (cadr spline) (append (reverse (car spline)) (reverse (car spline)) rs)))))

;(defun magnify-tail (xs rs))
; Copies each pixel and appends it to the original to double the width of the row
; xs = image data
; rs = new image with doubled row size
(defun magnify-tail (xs rs)
   (if (endp xs)
       rs
       (let (( 1st (nfix (first xs)))
               (2nd (nfix (second xs)))
               (3rd (nfix (third xs))))
                
               (magnify-tail (cdddr xs)
                                (append (list 3rd 2nd 1st 3rd 2nd 1st) rs)))))

;(defun crop (params xs rs))
 ; Pulls one row from the list, copies it, then appends
 ; each reversed row to double the height of the image
 ; params = the parameters specified by the user for how many pixels to crop in both w and h
 ; xs = image data
 ; rs = new image that has been cropped
  (defun crop (params xs rs)
    (if (> (len xs) (/ (* (car params) (cadddr params)) 3))
        (crop params (nthcdr (car params) xs) (append rs (take (- (car params) (cadr params)) xs)))
        rs
        ))