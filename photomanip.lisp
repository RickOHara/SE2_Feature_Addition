(in-package "ACL2")  
(set-state-ok T)
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "binary-io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "avl-rational-keys" :dir :teachpacks)
  (include-book "transpix")
  (include-book "reform")
  (include-book "compression")
  
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
  
;(defun controller (bmp-bytes method param))
 ; Calls the appropriate method to transform the image and/or 
  ; header as needed.
  ; bmp-bytes = file data
  ; method = integer denoting the transformation requested
  ; param = parameters needed by the method; most are nil (don't require params)
  (defun controller (bmp-bytes method param)
    (let* ((hdr-img (hdr/img bmp-bytes))
           (hdr (car hdr-img))
           (width (pix-W hdr))
           (height (pix-H hdr))
           (img (if(unpad width)
                   (unfmt-img (cadr hdr-img) nil width (unpad width))
                   (cadr hdr-img)))
           (trans (case method
                       (1 (if (equal param nil)
                              (reverse (bw img 250 nil))
                              (reverse (bw img param nil))))
                       (2 (if (equal param nil)
                              (reverse (blu-w img 250 nil))
                              (reverse (blu-w img param nil))))
                       (3 (if (equal param nil)
                              (reverse (grn-w img 250 nil))
                              (reverse (grn-w img param nil))))
                       (4 (if (equal param nil)
                              (reverse (red-w img 250 nil))
							  (reverse (red-w img param nil))))
                       (5 (reverse (gray img nil)))
                       (6 (reverse (tint-blu img nil)))
                       (7 (reverse (tint-grn img nil)))
                       (8 (reverse (tint-red img nil)))
                       (9 (reverse (negatize img nil)))
                       (10 (rot-180 img nil))
                       (11 (flip-H (pix-W hdr) img))
                       (12 (flip-V (pix-W hdr) img))
                       (13 (reverse(two-tone img (first param) (second param)
                                     (third param) (fourth param)
                                     (fifth param) (sixth param) nil)))
                       (14 (if (padding height)
                               (reverse (rot90L img (/ height 3) (/ width 3) 0 0 1 0 nil))
                               (rot90L img (/ height 3) (/ width 3) 0 0 1 0 nil)))
                       (15 (if (padding height)
                               (reverse (rot90R img (/ height 3) (/ width 3) (- (/ width 3) 1) 0 3 0 nil))
                               (rot90R img (/ height 3) (/ width 3) (- (/ width 3) 1) 0 3 0 nil)))
                       (16 (if (padding (* 2 width))
                               (reverse (double (* 2 width) (magnify-tail img nil) nil))
                               (double (* 2 width) (magnify-tail img nil) nil)))
                       (17 (reverse (img-16 img nil hdr 0)))
                       (18 (compress-rle img 1))
                       (19 (decompress-rle img -1))
                       (20 (crop (list width (car param) height (cadr param)) img nil))
                    ))
           (transpix (case method
                       (14 (if (padding height)
                               (fmt-img trans nil height (padding height))
                               trans))
                       (15 (if (padding height)
                               (fmt-img trans nil height (padding height))
                               trans))
                       (16 (if (padding (* 2 width))
                               (fmt-img trans nil (* 2 width) (padding (* 2 width)))
                               trans))
                       (20 (fmt-img trans nil (- width (car param)) (padding (- width (car param)))))
                       (otherwise  (if(padding width)
                                      (reverse (fmt-img trans nil width (padding width)))
                                      trans))
                       ))
		(ref (case method
                       (14 (reform-rot90 hdr (sizing (+ (len hdr) (len transpix)))
                             (sizing (len transpix))))
                       (15 (reform-rot90 hdr (sizing (+ (len hdr) (len transpix)))
                           (sizing (len transpix))))
                       (16 (reform-magnify hdr (sizing (+ (len hdr) (len transpix)))
                           (sizing (/ (* 2 width) 3)) (sizing (/ (* 2 height) 3))
                           (sizing (len transpix))))
                       (17 (hdr-16 hdr 1))
                       (20  (reform-crop hdr (sizing (len transpix)) (sizing (+ (len transpix) (len hdr))) 
                             (sizing (/ (- height (cadr param)) 3)) 
                             (sizing (/ (- width (car param)) 3))))
			(otherwise hdr)
                       ))
                )
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
                              (1 "bw-")
                              (2 "blu-w-")
                              (3 "grn-w-")
                              (4 "red-w-")
                              (5 "gray-w-")
                              (6 "tint-blu-")
                              (7 "tint-grn-")
                              (8 "tint-red-")
                              (9 "neg-")
                              (10 "rot-180-")
                              (11 "flip-H-")
                              (12 "flip-V-")
                              (13 "two-tone-")
                              (14 "rot90-left-")
                              (15 "rot90-right-")
                              (16 "magnify-")
                              (17 "16bit-")
                              (18 "rle-")
                              (19 "decomp")
                              (20 "crop"))
                            bmp-fn)
             (controller bmp method param) state))))
  
  (pms 20 "pic.bmp" '(639 204) state)
  ;(pms 1 "AJNrot.bmp" nil state)