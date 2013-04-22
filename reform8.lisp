(in-package "ACL2")
(include-book "transl8")

;(defun reform-bw8 (hdr contrast))
; Changes the palette to convert the image to black and white 
; Sends the palette to bw8 where the BGR values get changed to 
; black or white.
; hdr = bitmap header
; contrast = threshold for black versus white
(defun reform-bw8 (hdr contrast)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(bw8 palet contrast nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-blu-w8 (hdr contrast))
; Changes the palette to convert the image to blue and white 
; Sends the palette to blu-w8 where the BGR values get changed to 
; blue or white.
; hdr = bitmap header
; contrast = threshold for blue versus white
(defun reform-blu-w8 (hdr contrast)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(blu-w8 palet contrast nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-grn-w8 (hdr contrast))
; Changes the palette to convert the image to green and white 
; Sends the palette to grn-w8 where the BGR values get changed to 
; green or white.
; hdr = bitmap header
; contrast = threshold for green versus white
(defun reform-grn-w8 (hdr contrast)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(grn-w8 palet contrast nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-red-w8 (hdr contrast))
; Changes the palette to convert the image to red and white 
; Sends the palette to red-w8 where the BGR values get changed to 
; red or white.
; hdr = bitmap header
; contrast = threshold for red versus white
(defun reform-red-w8 (hdr contrast)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(red-w8 palet contrast nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-two-tone8 (hdr b1 b2 b3 w1 w2 w3))
; Changes the palette to convert the image to the specified colors 
; Sends the palette to two-tone8 where the BGR values get changed to 
; the specified colors.
; hdr = bitmap header
; b1 = blue channel of first color
; b2 = green channel of the first color
; b3 = red channel of the first color
; w1 = blue channel of second color
; w2 = green channel of the second color
; w3 = red channel of the second color
(defun reform-two-tone8 (hdr b1 b2 b3 w1 w2 w3)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(two-tone8 palet b1 b2 b3 w1 w2 w3 nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-gray8 (hdr))
; Changes the palette to convert the image to gray scale
; Sends the palette to gray8 where the BGR values get changed to 
; the appropriate shade of gray.
; hdr = bitmap header
(defun reform-gray8 (hdr)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(gray8 palet nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-tint-blu8 (hdr))
; Changes the palette to convert the image to blue scale
; Sends the palette to tint-blu8 where the BGR values get changed to 
; the appropriate shade of blue.
; hdr = bitmap header
(defun reform-tint-blu8 (hdr)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(tint-blu8 palet nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-tint-grn8 (hdr))
; Changes the palette to convert the image to green scale
; Sends the palette to tint-grn8 where the BGR values get changed to 
; the appropriate shade of green.
; hdr = bitmap header
(defun reform-tint-grn8 (hdr)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(tint-grn8 palet nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-tint-red8 (hdr))
; Changes the palette to convert the image to red scale
; Sends the palette to tint-red8 where the BGR values get changed to 
; the appropriate shade of red.
; hdr = bitmap header
(defun reform-tint-red8 (hdr)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(tint-red8 palet nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-negatize8 (hdr))
; Changes the palette to convert the image to its complement
; Sends the palette to negatize8 where the BGR values get changed to 
; their complement.
; hdr = bitmap header
(defun reform-negatize8 (hdr)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(negatize8 palet nil))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-fltr8 (hdr r g b lev))
; Changes the palette to convert the image for filter
; Sends the palette to fltr8 where the BGR values are either 
; filtered to gray or kept the same.
; hdr = bitmap header
; r = red value to keep
; g = green value to keep
; b = blue value to keep
; lev = amount to accept around the r g b values specified
(defun reform-fltr8 (hdr r g b lev)
   (let* ((palet (subseq hdr 54 nil))
          (maplet (reverse(fltr palet r g b lev))))
     (append (subseq hdr 0 54) maplet)))

;(defun reform-crop8 (xs fsize row height isize))
; Insert changes to header for new double sized image
; xs = header to be modified
; fsize = header plus image data byte count
; isize = image data byte count
; height = new row height (cropped) 
; width = new row width (cropped)
 (defun reform-crop8 (xs fsize isize height width)
   (append (subseq xs 0 2) fsize (subseq xs 6 18) 
           width height
           (subseq xs 26 34) isize
           (subseq xs 38 nil)))