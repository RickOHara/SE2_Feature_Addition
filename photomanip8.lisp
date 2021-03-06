  (set-state-ok T)
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "binary-io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "avl-rational-keys" :dir :teachpacks)
  (include-book "transpix")
  (include-book "transl8")
  (include-book "reform8")
  (include-book "reform")
  

;  (defun numeral->nat (b xs)
;    (if (consp xs)
;        (+ (car xs) (* b (numeral->nat b (cdr xs))))
;        0))
;  
;   (defun bytes->nat (xs)
;    (numeral->nat 256 xs))
  
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
  
;(defun controller8 (bmp-bytes method param))
 ; Calls the appropriate method to transform the image and/or 
  ; header as needed.
  ; bmp-bytes = file data
  ; method = integer denoting the transformation requested
  ; param = parameters needed by the method; most are nil (don't require params)
  (defun controller8 (bmp-bytes method param)
    (let* ((hdr-img (hdr/img bmp-bytes))
           (hdr (car hdr-img)) 
           (width (/(pix-W hdr) 3))
           (height (/ (pix-H hdr) 3))
           (img (if(unpad width)
                   (unfmt-img (cadr hdr-img) nil width (unpad width))
                   (cadr hdr-img)))
           (trans (case method
                    (11 (reverse img))
                    (12 (flip-H8 (pix-W hdr) img))
                    (13 (flip-V (pix-W hdr) img))
                    (14 ;(if (padding height)
                            ;(reverse (rot90L8 img height width 0 0 nil))
                            (rot90L8 img height width 0 0 nil));)
                    (15 ;(if (padding height)
                            ;(reverse (rot90L8 img height width 0 0 nil))
                            (rot90R8 img height width (- width 1) 0 nil));)
                    (21 (crop8 (list width (car param) height (cadr param)) img nil))
                       (otherwise img)))
           (transpix (case method 
                      (14 (if (padding height)
                               (fmt-img trans nil height (padding height))
                               trans))
                       (15 (if (padding height)
                               (fmt-img trans nil height (padding height))
                               trans))
                       (21 (fmt-img trans nil (- width (car param)) (padding (- width (car param)))))
                       (otherwise  (if(padding width)
                                      (reverse (fmt-img trans nil width (padding width)))
                                      trans))))
		(ref (case method
                       (1 (if (equal param nil)
                              (reform-bw8 hdr 250)
                              (reform-bw8 hdr param)))
                       (2 (if (equal param nil)
                              (reform-blu-w8 hdr 250)
                              (reform-blu-w8 hdr param)))
                       (3 (if (equal param nil)
                              (reform-grn-w8 hdr 250)
                              (reform-grn-w8 hdr param)))
                       (4 (if (equal param nil)
                              (reform-red-w8 hdr 250)
                              (reform-red-w8 hdr param)))
                       (5 (if (equal param nil)
                              (reform-two-tone8 hdr (first param) (second param)
                                     (third param) (fourth param)
                                     (fifth param) (sixth param))
                              (reform-two-tone8 hdr (first param) (second param)
                                     (third param) (fourth param)
                                     (fifth param) (sixth param))))
                       (6 (reform-gray8 hdr))
                       (7 (reform-tint-blu8 hdr))
                       (8 (reform-tint-grn8 hdr))
                       (9 (reform-tint-red8 hdr))
                       (10 (reform-negatize8 hdr))
                       (14 (reform-rot90 hdr (sizing (+ (len hdr) (len transpix)))
                             (sizing (len transpix))))
                       (15 (reform-rot90 hdr (sizing (+ (len hdr) (len transpix)))
                             (sizing (len transpix))))
                       (16 (if (< (length param) 4)
                               (reform-fltr8 hdr 0 0 0 0)
                               (reform-fltr8 hdr (first param) (second param)
                                      (third param) (fourth param))))
                       (21  (reform-crop8 hdr (sizing (len transpix)) (sizing (+ (len transpix) (len hdr))) 
                             (sizing (- height (cadr param))) 
                             (sizing (- width (car param)))))
			(otherwise hdr))))
      (append ref transpix)))
  

  
;(defun pms (method bmp-fn state)
; Takes user provided bmp file and transformation request with required parameters
; method = integer denoting requested transformation
; bmp-fn = bitmap file to be transformed
; param = required parameters; nil for most
  (defun pms (method bmp-fn param state)
    (mv-let (bmp err-opn-bmp state)
            (binary-file->byte-list bmp-fn state)
         (if err-opn-bmp
            (mv err-opn-bmp state)
            (byte-list->binary-file
             (string-append (case method
                              (1 "bw8-")
                              (2 "blu-w8-")
                              (3 "grn-w8-")
                              (4 "red-w8-")
                              (5 "two-tone8-")
                              (6 "gray8-")
                              (7 "tint-blu8-")
                              (8 "tint-grn8-")
                              (9 "tint-red8-")
                              (10 "neg8-")
                              (11 "rot-180-")
                              (12 "flip-H8")
                              (13 "flip-V8-")
                              (14 "rot90L8-")
                              (15 "rot90R8-")
                              (16 "fltr8-")
                              (21 "crop8-"))
                            bmp-fn)
             (controller8 bmp method param) state))))
  
  (pms 21 "AJN8.bmp" '(213 68) state)
  ;(pms 5 "AJN8.bmp" '(50 12 250 79 236 255) state)
  ;(pms 6 "AJN8.bmp" nil state)
  ;(pms 14 "AJN8.bmp" nil state)
  ;(pms 14 "Avatarjakeneytiri8.bmp" nil state)
  ;(pms 15 "WeatherSatBMP8.bmp" '(59 152 0 70) state)
  ;(pms 1 "Rover-16bit.bmp" nil state)
  ;(pms 1 "AJN16.bmp" nil state)
  