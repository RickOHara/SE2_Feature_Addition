(in-package "ACL2")
(include-book "bitLevel")
(include-book "list-utilities" :dir :teachpacks)


; (compress-rle xs count rs)
; Run-length-encoding
; Takes image data (usually b&w) and compresses multiple
; pixel values into two values - a number "count" preceding
; each value indicating the number of pixels in a run, and 
; the pixel value itself
; xs - img data in list of bytes
; count - initially 1, keeps track of runs
(defun compress-rle (xs count rs)
  (if (endp (cdr xs))
      (append (list (car xs) count) rs)
      (if (= (car xs) (cadr xs))
          (if (> count 254)
              (compress-rle (cdr xs) 1 (append (list (car xs) count) rs))
              (compress-rle (cdr xs) (+ count 1) rs))
          (compress-rle (cdr xs) 1 (append (list (car xs) count) rs)))))
         
; (decode-rle xs count rs)
; Run-length-decoding
; The opposite of all that other stuff
; Call it with count = -1
; xs - RLE encoded image data
; count - keeps track of runs, 1st element for every two elements
(defun decompress-rle (xs count rs)
  (declare (xargs :measure (- count 1)))
  (if (endp xs)
      rs
      (if (= count -1)
          (decompress-rle xs (car xs) rs)
          (if (> count 1)
              (decompress-rle xs (- count 1) (append (list (cadr xs)) rs))
              (decompress-rle (cddr xs) -1 (append (list (cadr xs)) rs))))))

; (padbits xs count)
; Takes a nat->bits list, pads with 0's until
; it has num (the variable) bits
; call it with count = -1
; xs - list of natural num in binary format
; count - keeps track of location
; num - max number of bits to pad to
(defun padbits (xs count num)
  (declare (xargs :measure (+ count 1)))
  (if (consp xs)
      (if (= count -1)
          (padbits xs (len xs) num)
          (if (not (= count num))
              (padbits (append xs '(0)) (+ count 1) num)
              xs))
      nil))

; (hdr-convert-16 hdr)
; Changes hdr data into 16-bit format
; hdr - header data
; count - keeps track of location
(defun hdr-16 (hdr count)
  (if (consp hdr)
      (if (= count 29)
          (cons 16 (hdr-16 (cdr hdr) (+ count 1)))
          (cons (car hdr) (hdr-16 (cdr hdr) (+ count 1))))
      nil))

; (getwidth hdr)
; gets width of image, in pixels,
; from the 19-22 bytes of header.
; Is kind of redundant but I don't really
; feel like replacing every instance of
; getwidth with pix-W at the moment
; hdr - header data
(defun getwidth (hdr)
  (if (endp hdr)
      nil
      (bits->nat 
       (append (padbits (nat->bits (nth 18 hdr)) -1 8)
               (padbits (nat->bits (nth 19 hdr)) -1 8)
               (padbits (nat->bits (nth 20 hdr)) -1 8)
               (padbits (nat->bits (nth 21 hdr)) -1 8)))))

; (numpadbytes hdr)
; Determines the number of bytes
; original image is padded by
; hdr - header data
(defun numpadbytes (hdr)
  (if (endp hdr)
      nil
      (if (= (mod (getwidth hdr) 4) 0)
          0
          (if (= (mod (getwidth hdr) 4) 1)
              1
              (if (= (mod (getwidth hdr) 4) 2)
                  2
                  (if (= (mod (getwidth hdr) 4) 3)
                      3
                      nil))))))

; (img-16 img run hdr num)
; Takes image data, converts each byte to 16-bit color
; (by doing an arithmetic shift of 3 for each RGB value),
; converts each byte to a binary representation in the triplet,
; discards 0 bits (since we're using 16 bits instead of 24),
; smashes the bits together, appends 0 at the end for the alpha value,
; and converts them back to numbers.
; Each pixel represented by three integers is converted to a
; pixel represented by two integers.
; Also handles padding to images that have pixel widths that
; aren't multiples of 4.
; img - image data
; run - for tail recursion
; hdr - header data
; num - keeps track of location
(defun img-16 (img run hdr num)
  (if (consp (cddr img))
      (let* ((15bits 
              (append
               (if (= (ash (car img) -3) 0)
                   (list 0 0 0 0 0)
                   (padbits (nat->bits (ash (car img) -3)) -1 5))
               (if (= (ash (cadr img) -3) 0)
                   (list 0 0 0 0 0)
                   (padbits (nat->bits (ash (cadr img) -3)) -1 5))
               (if (= (ash (caddr img) -3) 0)
                   (list 0 0 0 0 0)
                   (padbits (nat->bits (ash (caddr img) -3)) -1 5)))))
        (if (and (and (= (mod num (* (getwidth hdr) 2)) 0) 
                 (not (= (numpadbytes hdr) 0)))
                 (not (zp num)))
            (img-16 (nthcdr (numpadbytes hdr) img)
                    (append (list 0 0)
                            run) hdr 0)
            (img-16 (cdddr img) 
                    (append
                     (list (bits->nat (append (nthcdr 8 15bits) '(0))))
                     (list (bits->nat (reverse (nthcdr 7 (reverse 15bits)))))
                     run) hdr (+ num 2))))
            (append (list 0 0) run)))

; (convert24->16 hdr img)
; Color space reduction
; Takes image data, converts into 16-bit format
; hdr - header data
; img - image data
(defun convert24->16 (hdr img)
  (append (hdr-16 hdr 1) (reverse (img-16 img nil hdr 0))))

; (16bit->RGB frst scnd)
; Converts two bytes representing a 16-bit
; RGB color to three integers, each representing
; its own value
(defun 16bit->RGB (frst scnd)
  (let ((bits (append 
               (if (= frst 0)
                   (list 0 0 0 0 0 0 0 0)
                   (padbits (nat->bits frst) -1 8))             
               (if (= scnd 0)
                   (list 0 0 0 0 0 0 0 0)
                   (padbits (nat->bits scnd) -1 8)))))
    (list (bits->nat (subseq bits 0 5))
          (bits->nat (subseq bits 5 10))
          (bits->nat (subseq bits 10 15)))))

; (RGB->16bit frst scnd thrd)
; Converts three integers representing
; a 16-bit color to two bytes
(defun RGB->16bit (frst scnd thrd)
  (let ((bits (append
               (if (= frst 0)
                   (list 0 0 0 0 0)
                   (padbits (nat->bits frst) -1 5))
               (if (= scnd 0)
                   (list 0 0 0 0 0)
                   (padbits (nat->bits scnd) -1 5))
               (if (= thrd 0)
                   (list 0 0 0 0 0)
                   (padbits (nat->bits thrd) -1 5))
               '(0))))
    (list (bits->nat (subseq bits 0 8))
          (bits->nat (subseq bits 8 16)))))
		  
; (4bit-map pix)
; Converts one byte containing 2 4-bit
; RGB color to two integers, each representing
; the map to a color in the palette
; pix = byte to be dissected
(defun 4bit-map (pix)
  (let ((bits (if (= pix 0)
                   (list 0 0 0 0 0 0 0 0)
                   (padbits (nat->bits pix) -1 8))))
    (list (bits->nat (subseq bits 0 4))
          (bits->nat (subseq bits 4 nil)))))

;(defun map-4bit (frst scnd))		  
; Convert two integers, each representing a pixel to a 4bit format byte
; frst = first pixel
; scnd = second pixel
(defun map-4bit (frst scnd)
  (let ((bits (append
               (if (= frst 0)
                   (list 0 0 0 0)
                   (padbits (nat->bits frst) -1 4))
               (if (= scnd 0)
                   (list 0 0 0 0)
                   (padbits (nat->bits scnd) -1 4)))))               
    (list (bits->nat bits))))