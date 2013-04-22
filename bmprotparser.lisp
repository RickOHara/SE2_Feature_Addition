  (set-state-ok T)
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "binary-io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "arithmetic-5/top" :dir :system)

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
  
    ;(defun reorder (xs))
  ;  Reverses color channel of each pixel; BGR to RGB
 (defun reorder (xs)
  (if (consp xs)
    (append(list (caddr xs) (cadr xs) (car xs))(reorder (cdddr xs)))
    nil))
 
 (defun respline (xs)
   (reverse xs))
 
 ;(defun quip (xs))
 ; Pulls one row from the list and sends to reverse, then appends
 ; each reversed row to produce a horizonally flipped image
 (defun quip (xs)
   (if (consp xs)
       (let* ((spline (break-at-nth 1500 xs))) 
         (append (respline (car spline)) (cdr spline)))
       nil))
 
 ;(defun flip-H (xs))
 ; Flips the image horizontally;  calls reorder to invert the order of
 ; the color channels in preparation for reversing each row of pixels
 (defun flip-H (xs)
   (let* ((prep (reorder xs))) 
    (quip prep)))
  
  ;(defun transform (xs))
  ; Converts rgb values to black or white depending on total
  ;  Less than 382 = 0 0 0
  ;  More than or equal to 382 = 255 255 255
 (defun transform (xs)
  (if (consp xs)
    (let* ((pix (+ (car xs) (cadr xs) (caddr xs))))
      (if (< pix 250)
          (append (list 0 0 0) (transform (cdddr xs)))
          (append (list 255 255 255) (transform (cdddr xs)))))
    nil))
   
 ;(defun pix-W (xs))
 ; Determines row width; takes the 19th and 20th elements of the header,
 ; multiplies the 20th by 256 and adds the two
 (defun pix-W (xs)
   (let* ((split (break-at-nth 18 xs))
          (w1 (caadr split))
          (w2 (*(cadadr split) 256)))
    (* (+ w1 w2) 3)))
 
; (defun rot-90-left (final leftover xs count)
;   (if(or (consp xs) (consp leftover)) 
;      (if(and (not(consp xs))(consp leftover))   ; xs empty, leftover full
;         (rot-90-left final nil leftover (+ count 1))
;         (let* ((row (break-at-nth (- 1500 (* count 3)) xs))
;                (head (list (caar row)(cadar row)(caddar row))) 
;                (ahead (append final head))
;                (shead (break-at-nth 3 row)))
;           (append final head (rot-90-left ahead (cdr shead) (cdr row) (+ count 1)))))
;      nil))
 
 ;(defun helper (row iter txs))
 ; Inserts a row into the final tranformed image row by increasing
 ; the point of breaking by three with each iteration
 ; row = line to be inserted
 ; txs = transformed image data
 ; rxs = remaining portion of txs 
 ; iter = number of iterations
 (defun helper (row trans rxs iter)
   (if(endp row)
      rxs
      (let* ((brk (break-at-nth (* iter 3) trans))
             (ins (list (car row) (cadr row) (caddr row))))     ; pixel to be inserted
         (helper (cdddr row) (cadr brk) (append rxs (car brk) ins) iter))))
 
 ;(defun rot-90-left (xs txs it width)
 ; Rotate image 90 degrees to the left
 ; xs = image data
 ; txs = transformed image data
 ; width = image row width
 ; Example call:  rot-90-left img nil 0 pixw
 (defun rot-90-left (xs txs it width)
   (if(endp xs)
      txs
      (let* ((iter (+ it 1))
             (split (break-at-nth width xs))                 ;break off a row
             (trans (helper (car split)  txs nil iter)))     ;insert car split into txs
        (rot-90-left (cadr split) trans iter width))))
 
  (defun steg-file-bytes (bmp-bytes)
    (let* ((hdr-img (hdr/img bmp-bytes))
           (hdr (car hdr-img))
           (img (cadr hdr-img))
           (transpix (rot-90-left img nil 0 3)))
;           (trap "Done"))
      transpix))
  

  
  (defun pms (bmp-fn state)
    (mv-let (bmp err-opn-bmp state)
            (binary-file->byte-list bmp-fn state)
         (if err-opn-bmp
            (mv err-opn-bmp state)
             (steg-file-bytes bmp))))
  
  ;(pms "RSlogo.bmp" state)
  ;(pms "Avatarjakeneytiri.bmp" state)
  ;(pms "spot.bmp" state)
  
  ;(rot-90-left '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18) nil 0 3)
  ;(helper '(1 2 3 4 5 6 7 8 9 10 11 12) '(0 0 0 0 0 0 0 0 0 0 0 0) nil 1)