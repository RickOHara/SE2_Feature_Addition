(in-package "ACL2")   
(set-state-ok T)
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "binary-io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "avl-rational-keys" :dir :teachpacks)
  (include-book "reform")
  
    (defun numeral->nat (b xs)
    (if (consp xs)
        (+ (car xs) (* b (numeral->nat b (cdr xs))))
        0))
  
   (defun bytes->nat (xs)
    (numeral->nat 256 xs))
  
  (defun split (n xs)
    (if (and (posp n) (consp xs))
        (let* ((x1 (car xs))
               (s  (split (- n 1) (cdr xs)))
               (x2-n (car s))
               (xrst (cadr s)))
          (list (cons x1 x2-n)
                xrst))
        (list nil xs)))
  
  (defun firstN (n xs)
    (car (split n xs)))
  
  (defun hdr/img (bmp-bytes)
    (let*
        ((offset (bytes->nat (firstN 4 (nthcdr 10 bmp-bytes)))))
      (split offset bmp-bytes)))

 ;(defun pix-W (xs))
 ; Used for flip-V  and flip-H 
 ; Determines row width; takes the 19th - 22th elements of the header,
 ; multiplies the 20th by 256 and adds the two
 ; xs = header
 (defun pix-W (xs)
   (let* ((w1 (nth 18 xs))
          (w2 (* (nth 19 xs) 256))
          (w3 (* (nth 20 xs) 65536))
          (w4 (* (nth 21 xs) 16777216)))
    (* (+ w1 w2 w3 w4) 3)))
 
 ;(defun pix-H (xs))
 ; NOT USED FOR flip-V OR flip-H
 ; Determines column height; takes the 23th - 26th elements of the header,
 ; multiplies the 20th by 256 and adds the two
 ; xs = header
 (defun pix-H (xs)
   (let* ((h1 (nth 22 xs))
          (h2 (* (nth 23 xs) 256))
          (h3 (* (nth 24 xs) 65536))
          (h4 (* (nth 25 xs) 16777216))
          )
    (* (+ h1 h2 h3 h4) 3)))

 ;(defun double (n xs rs))
 ; Pulls one row from the list and sends to reverse, then appends
 ; each reversed row to produce a horizonally flipped image
 ; n = row width in bytes
 ; xs = image data
 (defun double (n xs rs)
   (if (endp xs)
       rs
       (let* ((spline (break-at-nth n xs)))
         (double n (cadr spline) (append (reverse (car spline)) (reverse (car spline)) rs)))))

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
	
;(defun reform (xs)
; Swaps width and height bytes and updates image size information resulting
; from the rotation of the image by 90 degrees
; xs = header to be modified
; fsize = header plus image data byte count
; isize = image data byte count
 (defun reform (xs fsize row height isize)
  ; (mv-let (w1 w2 w3 w4 h1 h2 h3 h4)
   ;        (mv (Nth 18 xs) (Nth 19 xs) (Nth 20 xs) (Nth 21 xs) 
    ;           (Nth 22 xs) (Nth 23 xs) (Nth 24 xs) (Nth 25 xs))
           (append (subseq xs 0 2) fsize (subseq xs 6 18) 
                   row
                   height
                   ;(list h1 h2 h3 h4) (list w1 w2 w3 w4)
                   (subseq xs 26 34) 
                   isize
                   (subseq xs 38 nil)))
 
;(defun unpack (xs txs)
; Creates a list of the pixels in the order stored in the tree
; xs = AVL tree with pixel bytes
; txs = image data
(defun unpack (xs txs)
  (if (endp xs)
      (reverse txs)
      (unpack (cdr xs) (append (list (cdar xs)) txs))))


;(defun padding (width)
; Determines if rows need to be padded and by how much
; Returns the list to be appended to each row
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

;(defun fmt-img (xs)
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
         (fmt-img (cadr splt) (append txs news) nrow pad))))                                ;put bitmap back together

  
  ;(pms "Avatarjakeneytiri.bmp" state)