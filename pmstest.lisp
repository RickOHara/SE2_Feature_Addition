(include-book "testing" :dir :teachpacks)
(include-book "tail-transformers")

;Test suite for bw; final output is reversed
;xs is empty
(check-expect (bw '() nil) nil)

;Black pixel then white pixel
(check-expect (bw '(100 20 20 200 150 150) nil) 
              (reverse'(0 0 0 255 255 255)))

;249 pixel then 250 pixel
(check-expect (bw '(100 100 49 100 100 50) nil) 
              (reverse'(0 0 0 255 255 255)))

;All white
(check-expect (bw '(100 200 200 200 100 155 209 200 10) nil) 
              (reverse'(255 255 255 255 255 255 255 255 255)))

;All black
(check-expect (bw '(10 20 200 20 100 15 29 25 10) nil) 
              (reverse'(0 0 0 0 0 0 0 0 0)))

;Test suite for blu-w; final output is reversed
;xs is empty
(check-expect (blu-w '() nil) nil)

;Black pixel then white pixel
(check-expect (blu-w '(100 20 20 200 150 150) nil) 
              (reverse'(150 0 0 255 255 255)))

;249 pixel then 250 pixel
(check-expect (blu-w '(100 100 49 100 100 50) nil) 
              (reverse'(150 0 0 255 255 255)))

;All white
(check-expect (blu-w '(100 200 200 200 100 155 209 200 10) nil) 
              (reverse'(255 255 255 255 255 255 255 255 255)))

;All blue
(check-expect (blu-w '(10 20 200 20 100 15 29 25 10) nil) 
              (reverse'(150 0 0 150 0 0 150 0 0)))

;Test suite for grn-w; final output is reversed
;xs is empty
(check-expect (grn-w '() nil) nil)

;Green pixel then white pixel
(check-expect (grn-w '(100 20 20 200 150 150) nil) 
              (reverse'(0 150 0 255 255 255)))

;249 pixel then 250 pixel
(check-expect (grn-w '(100 100 49 100 100 50) nil) 
              (reverse'(0 150 0 255 255 255)))

;All white
(check-expect (grn-w '(100 200 200 200 100 155 209 200 10) nil) 
              (reverse'(255 255 255 255 255 255 255 255 255)))

;All green
(check-expect (grn-w '(10 20 200 20 100 15 29 25 10) nil) 
              (reverse'(0 150 0 0 150 0 0 150 0)))

;Test suite for red-w; final output is reversed
;xs is empty
(check-expect (red-w '() nil) nil)

;Red pixel then white pixel
(check-expect (red-w '(100 20 20 200 150 150) nil) 
              (reverse'(0 0 150 255 255 255)))

;249 pixel then 250 pixel
(check-expect (red-w '(100 100 49 100 100 50) nil) 
              (reverse'(0 0 150 255 255 255)))

;All white
(check-expect (red-w '(100 200 200 200 100 155 209 200 10) nil) 
              (reverse'(255 255 255 255 255 255 255 255 255)))

;All red
(check-expect (red-w '(10 20 200 20 100 15 29 25 10) nil) 
              (reverse'(0 0 150 0 0 150 0 0 150)))

;Test suite for two-tone; final output is reversed
;xs is empty
(check-expect (two-tone '() 0 0 150 79 236 255 nil) nil)

;Red on yellow pixel
(check-expect (two-tone '(100 20 20 200 150 150)
                        0 0 150 79 236 255 nil) 
                        (reverse'(0 0 150 79 236 255)))

;249 pixel then 250 pixel
(check-expect (two-tone '(100 100 49 100 100 50)
                        0 0 150 79 236 255 nil) 
                        (reverse'(0 0 150 79 236 255)))

;All yellow
(check-expect (two-tone '(100 200 200 200 100 155 209 200 10) 
                        0 0 150 79 236 255 nil) 
                        (reverse'(79 236 255 79 236 255 79 236 255)))

;All red
(check-expect (two-tone '(10 20 200 20 100 15 29 25 10)
                        0 0 150 79 236 255 nil) 
                        (reverse'(0 0 150 0 0 150 0 0 150)))

;Test suite for gray; final output is reversed
;xs is empty
(check-expect (gray '() nil) nil)

;Dark then light pixel
(check-expect (gray '(100 20 20 200 150 150) nil) 
              (reverse'(46 46 46 166 166 166)))

;Light then dark pixel
(check-expect (gray '(200 150 249 107 118 50) nil) 
              (reverse'(199 199 199 91 91 91)))

;Light to dark
(check-expect (gray '(215 200 200 150 171 155 29 38 2) nil) 
              (reverse'(205 205 205 158 158 158 23 23 23)))

;Dark to light
(check-expect (gray '(17 4 120 104 96 99 229 235 107) nil) 
              (reverse'(47 47 47 99 99 99 190 190 190)))

;Dark light dark
(check-expect (gray '(10 42 85 202 177 158 39 25 12) nil) 
              (reverse'(45 45 45 179 179 179 25 25 25)))

;test suite for fltr
;base case
(check-expect (fltr '() 0 0 0 0) nil)
;some gray, some color
(check-expect (fltr '(100 200 200 200 100 156 209 200 10)  200 200 150 50)
              '(100 200 200 152 152 152 139 139 139))
;no gray
(check-expect (fltr '(100 200 50 140 180 70 60 235 25) 50 200 100 40)
              '(100 200 50 140 180 70 60 235 25))
;all gray
(check-expect (fltr '(100 200 50 140 180 70 60 235 25) 150 210 60 8)
              '(116 116 116 130 130 130 106 106 106))

;test suite for fltr-hlp
;checks that it returns the the original list if the values are out of range
(check-expect (fltr-hlp '(100 200 50 140 180 70 60 235 25) 60 140 160 240 10 90)
              '(100 200 50 140 180 70 60 235 25))

; test suite for between
;base case
(check-expect (between 0 0 0) t)
;number is greater than both elements
(check-expect (between 5 1 2) nil)
;true case
(check-expect (between 5 4 7) t)
;number is less than the lower bound, but greater than the upper bound
(check-expect (between 5 7 4) nil)
;number is less than both elements
(check-expect (between 3 5 8)

; test suite for negatize
;no elements
(check-expect (negatize '()) nil)
;checks the negative of the negative returns the original list
(check-expect (negatize (negatize '(100 20 20 200 150 150)))
              '(100 20 20 200 150 150))
;checks the normal case
(check-expect (negatize '(10 20 200 20 100 15 29 25 10))
              '(245 235 55 235 155 240 226 230 245))


