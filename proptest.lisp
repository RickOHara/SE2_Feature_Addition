(include-book "doublecheck" :dir :teachpacks)
(include-book "transpix")

;Predicate based test for bw
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty bw-integrity-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (bw xs 250 nil)) (len xs)))

;Predicate based test for bw
; Tests the property that the function changes pixel values to
; 0 or 255 only
(defproperty bw-value-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x :value (random-between 0 149))
  (or (= (Nth x (bw xs 250 nil)) 0) (= (Nth x (bw xs 250 nil)) 255)))
  
;Predicate based test for blu-w
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty blu-w-integrity-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (blu-w xs 250 nil)) (len xs)))

;Predicate based test for blu-w
; Tests the property that the function changes pixel blue channel
; values to 150 or 255 only
(defproperty blu-w-value-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x :value (random-between 0 49))
  (let* ((n (+(* x 3) 2)))
    (or (= (Nth n (blu-w xs 250 nil)) 150) (= (Nth n (blu-w xs 250 nil)) 255))))

;Predicate based test for grn-w
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty grn-w-integrity-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (grn-w xs 250 nil)) (len xs)))

;Predicate based test for grn-w
; Tests the property that the function changes pixel green channel
; values to 0 or 255 only
(defproperty grn-w-value-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x :value (random-between 0 49))
  (let* ((n (+(* x 3) 1)))
    (or (= (Nth n (grn-w xs 250 nil)) 150) (= (Nth n (grn-w xs 250 nil)) 255))))

;Predicate based test for red-w
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty red-w-integrity-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (red-w xs 250 nil)) (len xs)))

;Predicate based test for red-w
; Tests the property that the function changes pixel red channel
; values to 0 or 255 only
(defproperty red-w-value-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x :value (random-between 0 49))
  (let* ((n (* x 3)))
    (or (= (Nth n (red-w xs 250 nil)) 150) (= (Nth n (red-w xs 250 nil)) 255))))

;Predicate based test for two-tone
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty two-tone-integrity-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (two-tone xs 0 0 0 255 255 255 nil)) (len xs)))

;Predicate based test for two-tone
; Tests the property that the function changes pixels to black or
; yellow based on the pixel channel total values
(defproperty two-tone-value-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x :value (random-between 0 49))
  (let* ((n (* x 3)))
    (or (and (= (Nth n (reverse (two-tone xs 0 0 0 79 236 255 nil))) 79)
         (= (Nth (+ n 1) (reverse (two-tone xs 0 0 0 79 236 255 nil))) 236)
         (= (Nth (+ n 2) (reverse (two-tone xs 0 0 0 79 236 255 nil))) 255))
    (and (= (Nth n (reverse (two-tone xs 0 0 0 79 236 255 nil))) 0)
         (= (Nth (+ n 1) (reverse (two-tone xs 0 0 0 79 236 255 nil))) 0)
         (= (Nth (+ n 2) (reverse (two-tone xs 0 0 0 79 236 255 nil))) 0)))))

;Predicate based test for gray
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty gray-integrity-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (gray xs nil)) (len xs)))

;Predicate based test for gray
; Tests the property that the function changes pixel channel
; values to average value of pixel channel numbers
(defproperty gray-value-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x :value (random-between 0 49))
  (let* ((n (* x 3))
         (pix (truncate (+ (Nth n xs) (Nth (+ n 1) xs)
                           (Nth (+ n 2) xs)) 3)))
     (and (= (Nth n (reverse (gray xs nil))) pix)
          (= (Nth (+ n 1) (reverse (gray xs nil))) pix)
          (= (Nth (+ n 2) (reverse (gray xs nil))) pix))))

;Predicate based test for tint-blu
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty tint-blu-integrity-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (tint-blu xs nil)) (len xs)))

;Predicate based test for tint-blu
; Tests the property that the function changes pixel blue channel
; values to the average of the pixel channel values plus 90 or 255
(defproperty tint-blu-value-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x :value (random-between 0 49))
  (let* ((n (* x 3))
         (pix (truncate (+ (Nth n xs) (Nth (+ n 1) xs)
                           (Nth (+ n 2) xs)) 3)))
     (or (= (Nth n (reverse (tint-blu xs nil))) (+ pix 90))
            (= (Nth n (reverse(tint-blu xs nil))) 255))))

;Predicate based test for tint-grn
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty tint-grn-integrity-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (tint-grn xs nil)) (len xs)))

;Predicate based test for tint-grn
; Tests the property that the function changes pixel green channel
; values to the average of the pixel channel values plus 50 or 255
(defproperty tint-grn-value-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x :value (random-between 0 49))
  (let* ((n (* x 3))
         (pix (truncate (+ (Nth n xs) (Nth (+ n 1) xs)
                           (Nth (+ n 2) xs)) 3)))
     (or (= (Nth (+ n 1) (reverse (tint-grn xs nil))) (+ pix 50))
            (= (Nth (+ n 1) (reverse(tint-grn xs nil))) 255))))

;Predicate based test for tint-red
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty tint-red-integrity-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (tint-red xs nil)) (len xs)))

;Predicate based test for tint-red
; Tests the property that the function changes pixel red channel
; values to the average of the pixel channel values plus 100 or 255
(defproperty tint-red-value-test :repeat 2000
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x :value (random-between 0 49))
  (let* ((n (* x 3))
         (pix (truncate (+ (Nth n xs) (Nth (+ n 1) xs)
                           (Nth (+ n 2) xs)) 3)))
     (or (= (Nth (+ n 2) (reverse (tint-red xs nil))) (+ pix 100))
            (= (Nth (+ n 2) (reverse(tint-red xs nil))) 255))))

;Property for between
;between returns true or nil
(defproperty between-returns-true-or-nil
  (a :value (random-between 0 255)
   b :value (random-between 0 255)
   c :value (random-between 0 255))
  (or (equal (between a b c) t)
      (equal (between a b c) nil)))

;Predicate test for fltr
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty fltr-integrity-test :repeat 100
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (fltr xs 0 0 0 0)) (len xs)))

;predicate test for fltr
;tests fltr either returns the grayscale version of the pixel
; or the pixel original values
(defproperty fltr-value-test :repeat 100
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x  :value (random-between 0 49)
   r  :value (random-between 0 255)
   g  :value (random-between 0 255)
   b  :value (random-between 0 255)
   lev :value (random-between 0 255))
  (let* ((n (* x 3))
         (gr (truncate (+ (Nth n xs) (Nth (+ n 1) xs)
                          (Nth (+ n 2) xs)) 3)))
    (or (and (equal (Nth n (reverse (fltr xs r g b lev))) gr)
             (equal (Nth (+ n 1) (reverse (fltr xs r g b lev))) gr)
             (equal (Nth (+ n 2) (reverse (fltr xs r g b lev))) gr))
        (and (equal (Nth n (reverse (fltr xs r g b lev))) (Nth n xs))
             (equal (Nth (+ n 1) (reverse (fltr xs r g b lev))) (Nth (+ n 1) xs))
             (equal (Nth (+ n 2) (reverse (fltr xs r g b lev))) (Nth (+ n 2) xs))))))

;predicate test for negatize
; Tests the property that the function returns the same number
; of bytes as the original image data.  It does not add to or 
; subtract pixels from the image
(defproperty negatize-integrity-test :repeat 100
  (xs :value (random-list-of (random-between 0 255) :size 150))
  (= (len (negatize xs nil)) (len xs)))

;Predicate test for negatize
;checks that negatize returns 255 - original value 
(defproperty negatize-value-test :repeat 200
  (xs :value (random-list-of (random-between 0 255) :size 150)
   x  :value (random-between 0 49))
  (let* ((n (* x 3)))
    (and (equal (Nth n (reverse (negatize xs nil))) (- 255 (Nth n xs)))
         (equal (Nth (+ n 1) (reverse (negatize xs nil))) (- 255 (Nth (+ n 1) xs)))
         (equal (Nth (+ n 2) (reverse (negatize xs nil))) (- 255 (Nth (+ n 2) xs))))))
