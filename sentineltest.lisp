 (include-book "testing" :dir :teachpacks)

 ;(defun sentinel (hdr))
 ;Takes in the header of the image and will check to see if it is a bmp and if it is
 ;24 bits. Returns t if it is a 24 bit format bmp and nil otherwise.
 ;hdr - The header of the image passed in
(defun sentinel(hdr)
  (if (and (and (equal (nth 0 hdr) 66) (equal (nth 1 hdr) 77))
           (or (equal (nth 28 hdr) 24) (equal (nth 29 hdr) 24)))
      t
      nil
      ))

;Test suite for sentinel
(check-expect (sentinel nil) nil)
(check-expect (sentinel '(0 0 0 0 0 120 0 0 30)) nil)
(check-expect (sentinel '(66 77 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) nil)
(check-expect (sentinel '(66 77 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 24)) t)
(check-expect (sentinel '(66 77 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 24 24)) t)
(check-expect (sentinel '(66 77 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 24 0)) t)