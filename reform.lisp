(in-package "ACL2")

;(defun pix-W (xs))
 ; Used for flip-V  and flip-H 
 ; Determines row width; takes the 19th - 22nd elements of the header,
 ; multiplies the 20th by 256, the 21st by 256 squared,  
 ; the 22nd by 256 cubed, then multiplies the sum of all the values
 ; by three to get the number of bytes per row
 ; xs = header
 (defun pix-W (xs)
   (let* ((w1 (nth 18 xs))
          (w2 (* (nth 19 xs) 256))
          (w3 (* (nth 20 xs) 65536))
          (w4 (* (nth 21 xs) 16777216))
          )
    (* (+ w1 w2 w3 w4) 3)))
 
 ;(defun pix-W8 (xs))
 ; Same as pix-W, doesn't multiply by 3
 ; since there's one byte per pixel
  (defun pix-W8 (xs)
   (let* ((w1 (nth 18 xs))
          (w2 (* (nth 19 xs) 256))
          (w3 (* (nth 20 xs) 65536))
          (w4 (* (nth 21 xs) 16777216))
          )
    (+ w1 w2 w3 w4)))
 
 ;(defun pix-H (xs))
 ; NOT USED FOR flip-V OR flip-H
 ; Determines column height; takes the 23rd - 26th elements of the header,
 ; multiplies the 24th by 256, the 25th by 256 squared,  
 ; the 26th by 256 cubed, then multiplies the sum of all the values
 ; by three to get the number of bytes for a new row (90 deg rotation)  
 ; xs = header
 (defun pix-H (xs)
   (let* ((h1 (nth 22 xs))
          (h2 (* (nth 23 xs) 256))
          (h3 (* (nth 24 xs) 65536))
          (h4 (* (nth 25 xs) 16777216))
          )
    (* (+ h1 h2 h3 h4) 3)))
 
 ;(defun pix-H8 (xs))
 ; Same as pix-H, doesn't multiply by 3
 ; since there's one byte per pixel
  (defun pix-H8 (xs)
   (let* ((h1 (nth 22 xs))
          (h2 (* (nth 23 xs) 256))
          (h3 (* (nth 24 xs) 65536))
          (h4 (* (nth 25 xs) 16777216))
          )
    (+ h1 h2 h3 h4)))
 
 ; (defun endian->int (xs))
 ; convenience method for reading header information,
 ; takes 4 bytes in a list and returns the integer value
 ; xs - list of 4 bytes
 (defun endian->int (xs)
   (let* ((h1 (nth 0 xs))
          (h2 (* (nth 1 xs) 256))
          (h3 (* (nth 2 xs) 65536))
          (h4 (* (nth 3 xs) 16777216)))
     (+ h1 h2 h3 h4)))
          
	
;(defun sizing (xs)
; Transforms an integer to little-endian format
; Get image data length for data size; add header to data size for file size
; xs = the number to be formatted into little-endian form
 (defun sizing (xs)
   (let* ((four (truncate xs 16777216))
         (remfour (mod xs 16777216))
         (three (truncate remfour 65536))
         (remthree (mod remfour 65536))
         (two (truncate remthree 256))
         (one (mod remthree 256)))
    (list one two three four)))
	
;(defun reform-rot90 (xs))
; Use with rotate 90 left or right
; Swaps width and height bytes and updates image size information resulting
; from the rotation of the image by 90 degrees
; xs = header to be modified
; fsize = header plus image data byte count
; isize = image data byte count
 (defun reform-rot90 (xs fsize isize)
   (mv-let (w1 w2 w3 w4 h1 h2 h3 h4)
           (mv (Nth 18 xs) (Nth 19 xs) (Nth 20 xs) (Nth 21 xs) 
               (Nth 22 xs) (Nth 23 xs) (Nth 24 xs) (Nth 25 xs))
           (append (subseq xs 0 2) fsize (subseq xs 6 18) 
                   (list h1 h2 h3 h4) (list w1 w2 w3 w4)
                   (subseq xs 26 34) isize
                   (subseq xs 38 nil))))

;(defun reform-magnify (xs fsize row height isize))
; Insert changes to header for new double sized image
; xs = header to be modified
; fsize = header plus image data byte count
; row = new row width (doubled)
; height = new row height (doubled)
; isize = image data byte count				   
 (defun reform-magnify (xs fsize row height isize)
   (append (subseq xs 0 2) fsize (subseq xs 6 18) row height 
           (subseq xs 26 34) isize (subseq xs 38 nil)))
 
;(defun reform-crop (xs fsize row height isize))
; Insert changes to header for new double sized image
; xs = header to be modified
; fsize = header plus image data byte count
; isize = image data byte count
; height = new row height (cropped) 
; width = new row width (cropped)
 (defun reform-crop (xs fsize isize height width)
   (append (subseq xs 0 2) fsize (subseq xs 6 18) 
           width height
           (subseq xs 26 34) isize
           (subseq xs 38 nil)))
 
 (defun magnify-HDR (xs)
   (append (subseq xs 0 38)
           (sizing ( / (endian->int (subseq xs 38 42)) 2))
           (sizing ( / (endian->int (subseq xs 42 46)) 2))
           (subseq xs 46 nil))
   )

(defun magnify-Vert (xs)
   (append (subseq xs 0 38)
           (subseq xs 38 42)
           (sizing ( / (endian->int (subseq xs 42 46)) 2))
           (subseq xs 46 nil))
   )

(defun magnify-Hrz (xs)
   (append (subseq xs 0 38)
           (sizing ( / (endian->int (subseq xs 38 42)) 2))
           (subseq xs 42 46)
           (subseq xs 46 nil))
   )