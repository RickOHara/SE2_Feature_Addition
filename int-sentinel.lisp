(include-book "io-utilities" :dir :teachpacks)
(include-book "binary-io-utilities" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)
(include-book "int-photomanip")
(set-state-ok T)

;(defun sentinel24 (hdr))
; This is a check to see if the current file is of 24 bit format by checking to see it is a bmp and that it is 24 bit
; hdr - the hdr in bytes
(defun sentinel24(hdr)
  (if (and (and (equal (nth 0 hdr) 66) (equal (nth 1 hdr) 77)) ;bmp check on this line
           (or (equal (nth 28 hdr) 24) (equal (nth 29 hdr) 24))) ;24 bit check on this line
      t
      nil
      ))

;(defun sentinel8 (hdr))
; This is a check to see if the current file is of 8 bit format by checking to see it is a bmp and that it is 8 bit
; hdr - the hdr in bytes
(defun sentinel8(hdr)
  (if (and (and (equal (nth 0 hdr) 66) (equal (nth 1 hdr) 77)) ;bmp check on this line
           (or (equal (nth 28 hdr) 8) (equal (nth 29 hdr) 8))) ;8 bit check on this line
      t
      nil
      ))

;(defun pms2 (bmp-fn method params state))
;This function will call sentinel to check compatibility as well as check to see if the format is 24 or 8 bit, from
;there it will call a certain manipulation based on the value of method and then create the image according to that
;value.
; bmp-fn - the picture's filepath
; method - the value of the method to be performed
; params - the list of parameters defined by the user
; state - the state

(defun pms2 (bmp-fn method params state)
  (mv-let (bmp err-opn-bmp state)
          (binary-file->byte-list bmp-fn state)
          (if err-opn-bmp
              (mv err-opn-bmp state)
              (if (sentinel24 bmp)
                  (byte-list->binary-file 
                   (string-append (subseq bmp-fn 0 (- (length bmp-fn) 4)) 
                                  (case method
                                    (1 "-bw.bmp")
                                    (2 "-blu-w.bmp")
                                    (3 "-grn-w.bmp")
                                    (4 "-red-w.bmp")
                                    (5 "-two-tone.bmp")
                                    (6 "-gray.bmp")
                                    (7 "-tint-blu.bmp")
                                    (8 "-tint-grn.bmp")
                                    (9 "-tint-red.bmp")
                                    (10 "-neg.bmp")
                                    (11 "-rot180.bmp")
                                    (12 "-flipH.bmp")
                                    (13 "-flipV.bmp")                              
                                    (14 "-rot90-left.bmp")
                                    (15 "-rot90-right.bmp")
                                    (16 "-filter.bmp")
                                    (17 "-magnify.bmp")
                                    (18 "-16bit.bmp")
                                    (19 "-rle.bmp")
                                    (20 "-decomp.bmp")
                                    (21 "-crop.bmp")
                                    (22 "-magnifyHDR.bmp")
                                    (23 "-magnifyVert.bmp")
                                    (24 "-magnifyHrz.bmp")
                                    (25 "-sepia.bmp")))
                   (controller24 bmp method params) state)
                  (if (sentinel8 bmp)
                      (byte-list->binary-file 
                       (string-append (subseq bmp-fn 0 (- (length bmp-fn) 4)) 
                                      (case method
                                        (1 "-bw.bmp")
                                        (2 "-blu-w.bmp")
                                        (3 "-grn-w.bmp")
                                        (4 "-red-w.bmp")
                                        (5 "-two-tone.bmp")
                                        (6 "-gray.bmp")
                                        (7 "-tint-blu.bmp")
                                        (8 "-tint-grn.bmp")
                                        (9 "-tint-red.bmp")
                                        (10 "-neg.bmp")
                                        (11 "-rot-180.bmp")
                                        (12 "-flipH.bmp")
                                        (13 "-flipV.bmp")
                                        (14 "-rot90-left.bmp")
                                        (15 "-rot90-right.bmp")
                                        (16 "-filter.bmp")
                                        (17 "-magnify.bmp")
                                        (21 "-crop.bmp")))
                       (controller8 bmp method params) state)
                      (mv err-opn-bmp state))
                  )
              )))

;(defun parse-list (xs))
; This function will take an a list of strings and parse them into ints, returns nil if there is no value to be found.
;xs - the list of strings
(defun parse-list (xs)
  (if (consp xs)
      (cons (str->int (car xs)) (parse-list (cdr xs)))
      nil
      ))

;(defun fix-path (xs))
; This function is needed to fix the filepath for windows systems so that proofpad runs it correctly. Returns the modified
;filepath.
;xs - the file path
(defun fix-path (chars new)
       (if (not (equal (car chars) nil))
           (if (equal (car chars) #\\)
               (fix-path (cdr chars) (cons #\/ new))			   
               (fix-path (cdr chars) (cons (car chars) new))
               )
           (chrs->str new)
       ))

;(defun main (state))
; Entry point for the program, it reads from pmsInfo.txt and then parses the information to be passed and manipulated.                 
(defun main (state)
  (mv-let (filestring err-opn-bmp state)
          (file->string "pmsInfo.txt" state)
          (let* ((xs (words filestring))
                 (filepath (fix-path (reverse (str->chrs (car xs))) nil))
                 (method (str->int(cadr xs)))
                 (params (parse-list (cddr xs))))
            (if (stringp filepath)
                (pms2 filepath method params state)
                (mv err-opn-bmp state)))))