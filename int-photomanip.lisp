(in-package "ACL2")  
(set-state-ok T)
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "binary-io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "avl-rational-keys" :dir :teachpacks)
  (include-book "transpix")
  (include-book "transl8")
  (include-book "reform")
  (include-book "reform8")
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
  
;(defun controller24 (bmp-bytes method param))
 ; Calls the appropriate method to transform the image and/or 
  ; header as needed.
  ; bmp-bytes = file data
  ; method = integer denoting the transformation requested
  ; param = parameters needed by the method; most are nil (don't require params)
  (defun controller24 (bmp-bytes method param)
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
                              (reverse (bw img (car param) nil))))
                       (2 (if (equal param nil)
                              (reverse (blu-w img 250 nil))
                              (reverse (blu-w img (car param) nil))))
                       (3 (if (equal param nil)
                              (reverse (grn-w img 250 nil))
                              (reverse (grn-w img (car param) nil))))
                       (4 (if (equal param nil)
                              (reverse (red-w img 250 nil))
                              (reverse (red-w img (car param) nil))))
                       (5 (reverse(two-tone img (first param) (second param)
                                     (third param) (fourth param)
                                     (fifth param) (sixth param) nil)))
                       (6 (reverse (gray img nil)))
                       (7 (reverse (tint-blu img nil)))
                       (8 (reverse (tint-grn img nil)))
                       (9 (reverse (tint-red img nil)))
                       (10 (reverse (negatize img nil)))
                       (11 (rot-180 img nil))
                       (12 (flip-H (pix-W hdr) img))
                       (13 (flip-V (pix-W hdr) img))
                       
                       (14 (rot90L img (/ height 3) (/ width 3) 0 0 1 0 nil))
                       (15 (rot90R img (/ height 3) (/ width 3) (- (/ width 3) 1) 0 3 0 nil))
                        (16 (if (< (length param) 4)
                               (fltr img 0 0 0 0)
                               (reverse (fltr img 
                                          (first param) (second param) (third param) (fourth param)))))
                       (17 (if (padding (* 2 width))
                               (reverse (double (* 2 width) (magnify-tail img nil) nil))
                               (double (* 2 width) (magnify-tail img nil) nil)))
                       (18 (reverse (img-16 (cadr hdr-img) nil hdr 0)))
                       (19 (reverse (compress-rle img 1 nil)))
                       (20 (reverse (decompress-rle img -1 nil)))
                       (21 (if (or (< width (* 3 (car param))) (< height (* 3 (cadr param))))
                               img
                               (crop (list width (* 3 (car param)) height (* 3 (cadr param))) img nil)))
                    (otherwise img)
                    ))
           (transpix (case method
                       (14 (if (padding height)
                               (fmt-img trans nil height (padding height))
                               trans))
                       (15 (if (padding height)
                               (fmt-img trans nil height (padding height))
                               trans))
                       (17 (if (padding (* 2 width))
                               (fmt-img trans nil (* 2 width) (padding (* 2 width)))
                               trans))
                       (18 trans)        
                       (21 (fmt-img trans nil (- width (car param)) (padding (- width (car param)))))
                       (otherwise  (if(padding width)
                                      (reverse (fmt-img trans nil width (padding width)))
                                      trans))))
		(ref (case method
                       (14 (reform-rot90 hdr (sizing (+ (len hdr) (len transpix)))
                             (sizing (len transpix))))
                       (15 (reform-rot90 hdr (sizing (+ (len hdr) (len transpix)))
                           (sizing (len transpix))))
                       (17 (reform-magnify hdr (sizing (+ (len hdr) (len transpix)))
                           (sizing (/ (* 2 width) 3)) (sizing (/ (* 2 height) 3))
                           (sizing (len transpix))))
                       (18 (hdr-16 hdr 1))
                       (21 (if (or (< width (* 3 (car param))) (< height (* 3 (cadr param))))
                               hdr
                               (reform-crop hdr (sizing (len transpix)) (sizing (+ (len transpix) (len hdr))) 
                                            (sizing (/ (- height (* 3 (cadr param))) 3)) 
                                            (sizing (/ (- width (* 3 (car param))) 3)))))
                       (22 (magnify-HDR hdr))
                       (23 (magnify-Vert hdr))
                       (24 (magnify-Hrz hdr))
			(otherwise hdr))))
      (append ref transpix)))
 
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
                    (12 (flip-H8 width img))
                    (13 (flip-V width img))
                    (14 (rot90L8 img height width 0 0 nil))
                    (15 (rot90R8 img height width (- width 1) 0 nil))
                    (17 (if (padding (* 2 width))
                            (reverse (double (* 2 width) (magnify8 img nil) nil))
                            (double (* 2 width) (magnify8 img nil) nil)))
                    (21 (if (or (< width (car param)) (< height (cadr param)))
                            img
                            (crop8 (list width (car param) height (cadr param)) img nil)))
                       (otherwise img)))
           (transpix (case method 
                      (14 (if (padding height)
                               (fmt-img trans nil height (padding height))
                               trans))
                       (15 (if (padding height)
                               (fmt-img trans nil height (padding height))
                               trans))
                       (17 (if (padding (* 2 width))
                               (fmt-img trans nil (* 2 width) (padding (* 2 width)))
                               trans))
                       (21 (fmt-img trans nil (- width (car param)) (padding (- width (car param)))))
                       (otherwise  (if(padding width)
                                      (reverse (fmt-img trans nil width (padding width)))
                                      trans))))
		(ref (case method
                       (1 (if (equal param nil)
                              (reform-bw8 hdr 250)
                              (reform-bw8 hdr (car param))))
                       (2 (if (equal param nil)
                              (reform-blu-w8 hdr 250)
                              (reform-blu-w8 hdr (car param))))
                       (3 (if (equal param nil)
                              (reform-grn-w8 hdr 250)
                              (reform-grn-w8 hdr (car param))))
                       (4 (if (equal param nil)
                              (reform-red-w8 hdr 250)
                              (reform-red-w8 hdr (car param))))
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
                       (17 (reform-magnify hdr (sizing (+ (len hdr) (len transpix)))
                           (sizing (* 2 width)) (sizing (* 2 height))
                           (sizing (len transpix))))
                       (21 (if (or (< width (car param)) (< height (cadr param)))
                               hdr
                               (reform-crop8 hdr (sizing (len transpix)) (sizing (+ (len transpix) (len hdr))) 
                                             (sizing (- height (cadr param))) 
                                             (sizing (- width (car param))))))
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
                              (1 "bw-")
                              (2 "blu-w-")
                              (3 "grn-w-")
                              (4 "red-w-")
                              (5 "two-tone-")
                              (6 "gray-w-")
                              (7 "tint-blu-")
                              (8 "tint-grn-")
                              (9 "tint-red-")
                              (10 "neg-")
                              (11 "rot-180-")
                              (12 "flip-H-")
                              (13 "flip-V-")                              
                              (14 "rot90-left-")
                              (15 "rot90-right-")
                              (16 "filter-")
                              (17 "magnify-")
                              (18 "16bit-")
                              (19 "rle-")
                              (20 "decomp-")
                              (21 "crop-")
                              (22 "magnifyHDR-")
                              (23 "magnifyVert-")
                              (24 "magnifyHrz-"))
                            bmp-fn)
             (controller24 bmp method param) state))))
  
  ;(defun pms (method bmp-fn state)
; Takes user provided bmp file and transformation request with required parameters
; method = integer denoting requested transformation
; bmp-fn = bitmap file to be transformed
; param = required parameters; nil for most
  (defun pms8 (method bmp-fn param state)
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
                              (17 "mag8-"))
                            bmp-fn)
             (controller8 bmp method param) state))))
  
  ;(pms 20 "Avatarjakeneytiri-rle.bmp" nil state)
  ;(pms 1 "AJNrot.bmp" nil state)