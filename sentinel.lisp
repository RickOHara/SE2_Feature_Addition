  (include-book "io-utilities" :dir :teachpacks)
  (include-book "binary-io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)
  ;(include-book "arithmetic-5/top" :dir :system)
  ;(in-package "ACL2")
  (include-book "photomanip")
  (set-state-ok T)

(defun sentinel24(hdr)
  (if (and (and (equal (nth 0 hdr) 66) (equal (nth 1 hdr) 77))
           (or (equal (nth 28 hdr) 24) (equal (nth 29 hdr) 24)))
      t
      nil
      ))

;will need to revise this for later to actually check for 8 bit
(defun sentinel8(hdr)
  (if (and (and (equal (nth 0 hdr) 66) (equal (nth 1 hdr) 77))
           (or (equal (nth 28 hdr) 8) (equal (nth 29 hdr) 8)))
      t
      nil
      ))

   (defun steg-file-bytes2 (bmp-bytes)
    (let* ((hdr-img (hdr/img bmp-bytes))
           (hdr (car hdr-img))
           (img (reverse (cadr hdr-img))))
           (append hdr (bw img 250 nil))))

  (defun pms2 (bmp-fn params state)
    (mv-let (bmp err-opn-bmp state)
            (binary-file->byte-list bmp-fn state)
         (if err-opn-bmp
            (mv err-opn-bmp state)
            (if (sentinel24 bmp)
                (byte-list->binary-file 
                 (string-append (subseq bmp-fn 0 (- (length bmp-fn) 4)) "bw.bmp")
                                        (controller bmp 1 nil) state)
;                 (string-append (subseq bmp-fn 0 (- (length bmp-fn) 4)) "bw.bmp")
;                                        (steg-file-bytes2 bmp) state)
                (if (sentinel8 bmp)
                	(byte-list->binary-file 
                 	(string-append (subseq bmp-fn 0 (- (length bmp-fn) 4)) "bw.bmp")
                                        (controller bmp 1 params) state)
                	(mv err-opn-bmp state))
            )
          )))
 
 (defun jvm-sucks (chars new)
       (if (not (equal (car chars) nil))
           (if (equal (car chars) #\\)
               (jvm-sucks (cdr chars) (cons #\/ new))
               (jvm-sucks (cdr chars) (cons (car chars) new))
               )
           (chrs->str new)
       ))
                   
(defun main (state)
   (mv-let (filestring err-opn-bmp state)
           (file->string "thefile2.txt" state)
      (let* ((xs (words filestring))
             ;(filepath (jvm-sucks (reverse (str->chrs (car xs))) nil))
             (filepath (car xs))
             (params (cdr xs)))
        (if (stringp filepath)
   	(pms2 filepath params state)
        (mv err-opn-bmp state))))
   )