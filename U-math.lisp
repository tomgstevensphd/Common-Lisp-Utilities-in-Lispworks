;;************************** U-math.lisp ****************************
;;
;; ALSO SEE THE FOLDER = MATH 

;;FROM LW SPECIAL PACKAGES (ALREADY DEFINED)
;;
;;
;;CALC-MEAN-SD-VARIANCE-ETC
;;
;;ddd
(defun calc-mean-sd-variance-etc (score-list)
  "In U-math, returns (values  mean sd variance total num-scores sum-sq-scores)"
  (let
      ((num-scores (list-length score-list))
       (total-score)
       (mean)
       (variance)
       (SD)
       (sum-sq-scores 0)
       (sq-dif-score)
       )
    ;;find total, mean
    (setf total-score (* 1.000 (apply '+ score-list))
          mean  (/ (* 1.000 total-score) num-scores))
    ;;find variance, sd
    (loop
       for score in score-list
       with dif-sq-score
       do
       (let*
           ((dif-sq-score)
            (dif-score)
            )
       (setf dif-score (- score   mean)
             sq-dif-score (* dif-score dif-score)
             sum-sq-scores (* (+ sum-sq-scores sq-dif-score) 1.000))
       ;;end loop
       ))
    (setf variance (/ sum-sq-scores num-scores)
          SD (sqrt variance))
    (values  mean sd variance total-score num-scores sum-sq-scores)
    ))
;;TEST
;;  (calc-mean-sd-variance-etc '( 2 5 77 2 9))


;;FROM P SEIBEL

#|1.0 = .0
1e0 = 1.0
1d0 = 1.0d0
123.0 =  123.0
123e0 =  123.0
0.123 = 0.123
.123 = 0.123
123e-3= 0.123
123E-3 = 0.123
0.123e20 = 1.23e+19
123d23 = 1.23d+25|#



(defun my-normalize (num-list &key min-val (return-ratios-p T))
  "In U-math Returns sqrt of sum of squares of nums in num-list. If min-val + norm=0, returns min-val. Note often used as divisor. return-ratios-p causes return of ratio-list.  RETURNS (values norm sumsq sum return-ratios)"
  (let
      ((sum 0.0)
       ;;(N (list-length num-list))
       (norm)
       (sumsq 0.0)
       (ratio)
       (ratio-list)
       (return-ratios)
       (sum-ratios 0.0)
       )
    (loop
     for num in num-list
     do
     (setf sum (+  sum num)
            sumsq (+ sumsq (* num num)))   
     ;;end loop
     )
    (setf norm (sqrt sumsq))
    (if (and min-val (= norm 0))
        (setf norm min-val))

    (when return-ratios-p
        (loop
         for num in num-list
         do
         (setf ratio (/ num norm)
               sum-ratios (+ sum-ratios ratio)
               return-ratios (append return-ratios (list ratio)))
         ;;end loop, if
         ))
    (values norm sumsq sum return-ratios sum-ratios)
    ))
;;TEST and see how values vary compared to sums
;;  (my-normalize '(1 1 1 1.0)) = 2.0 4.0 4.0  (0.5 0.5 0.5 0.5)  2.0
;;  (my-normalize '(.1 .1 .1 .1)) = 0.2  0.040000003  0.4  (0.5 0.5 0.5 0.5)  2.0
;;  (my-normalize '(1 2 3 4 5)) = 7.4161983 55.0  15.0  (0.13483998 0.26967996 0.40451992 0.5393599 0.6741999)  2.0225997
;;  (my-normalize '(11 12 13 14)) = 25.0998  630.0  50.0  (0.4382505 0.47809145 0.5179324 0.55777336)  1.9920478
;;  (my-normalize '(1 -2  3 -4 5)) = 7.4161983 55.0  3.0  (0.13483998 -0.26967996 0.40451992 -0.5393599 0.6741999)  0.4045199
;;  (my-normalize '(0.1 0.2 0.3 0.4 0.5)) = 0.7416199 0.55 1.5 (0.13483997 0.26967993 0.40451992 0.53935987 0.6741998)  2.0225995
;;  (my-normalize '(0.01 0.02 0.03 0.04 0.05)) = 0.074161984
;;  (my-normalize '(1 2 3 4 1000)) = 1000.015 1000030.0  1010.0  (9.99985E-4 0.00199997 0.0029999549 0.00399994 0.999985)  1.0099849
;;  (my-normalize '(1 1 1  .0001)) = 1.7320508  3.0  3.0001  (0.57735026 0.57735026 0.57735026 5.7735026E-5)  1.7321085
;; (my-normalize '(0.0010999999 0.0010999999 0.0010999999 10.221644 0.0010999999 0.0010999999 0.0010999999 0.55927766 0.55924994))


;;FRANDOM
;;Floating point  random numbers:
;;
;;ddd
(defun frandom (low high)
  "In  U-math, Makes Floating point  random numbers within range low-high"
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))


;;WHOLE-NUMBER-P
;;
;;ddd
(defun whole-number-p (num)
  "In U-math, tests to see if num is a whole number (not necessarily integer). RETURNS (values whole-num-p left-dec  right-dec)"
  (let
      ((whole-num-p)
       )
    (multiple-value-bind (left-dec right-dec)
        (floor num)
    (when (= right-dec 0)
      (setf  whole-num-p left-dec))
    (values whole-num-p left-dec right-dec)
    ;;end mvb, let, whole-number-p
    )))
;;TEST
;; (whole-number-p 22.22) = NIL 22  0.21999932
;; (whole-number-p 22.00) =   22 22 0.0
;; (whole-number-p 22) = 22 22 0



 ;;(floor 3.33) = 3  0.32999992
;; (floor  33.33   2) = 16  0.3300018
;; (floor  333.33   2) = 166  1.3299866
;; (floor  333.33   3) =   111 0.32998657


;;FLOATING POINT NOT ACCURATE! 
;;
#|
CL-USER 1 > *read-default-float-format* = SINGLE-FLOAT
CL-USER 3 > (+ 0.1 0.6) = 0.70000005
CL-USER 3 > (= (+ 0.1 0.6) 0.7) = NIL
CL-USER 8 > (/ 7.0 3.0) = 2.3333333 ;;much shorter than if double-float below

;;CHANGE *read-default-float-format* TO DOUBLE-FLOAT
CL-USER 4 > (setf *read-default-float-format*  'double-float)
CL-USER 5 > (+ 0.1 0.6) = 0.7
CL-USER 6 > (= (+ 0.1 0.6) 0.7) = T
CL-USER 5 > (/ 7.0 3.0) = 2.3333333333333335
;;ALSO
CL-USER 2 > (setf *read-default-float-format*  'short-float) = SHORT-FLOAT
CL-USER 3 > (+ 0.6 0.1) = 0.69999695
CL-USER 10 > (/ 7.0 3.0) = 2.3333283
;;TO AVOID PROBLEM OF NOT TESTING RIGHT (GET NIL)
;; 1. USE FRACTIONS
CL-USER 11 > (/ 7 3) = 7/3
;;2. USE MY= FUNCTION BELOW
|#

;;MY=
;;
;;ddd
(defun my= (x &rest y-list) ;; &key (delta .0001))
  "My= replaces = for any numbers. Used especially for floating point numbers because the results are NOT exact and they will eval to NIL even if really equal."
  (let
      ((result T)
       (delta .0001)
       )
    (dolist (y y-list)
      (cond
       ((< (abs (- x y)) delta)
        NIL)
       (T (setf result nil)
          (return))))
    result
    ))
;;TEST
;;  (my= 1 1) = T    
;;   (my= 1 2) = NIL
;; CL-USER 24 > *read-default-float-format* = SINGLE-FLOAT or= SHORT-FLOAT
;; WORKS
;;   (my= (+ 0.1 0.6) 0.7) = T
;;where CL-USER 27 > (= (+ 0.1 0.6) 0.7) = NIL




;;FIND-EVERY-NTH-ITEM
;;
;;ddd
(defun find-every-nth-item (nth list &key  max-length)
  "In U-math, returns a list of  every nth item in a list. max-length. is max of return list. RETURNS (values new-list new-list-n)"
  (let
      ( (nth-item)
        (new-list-n)
        (new-list)
       )
    (loop
     for item in list
     for n from 0 to (list-length list)
     do
     (cond
      ((and max-length (>= (list-length new-list) max-length))
       (return))     
      ((whole-number-p  (/ n nth))
       (setf new-list (append new-list (list item)))))
     )
    (setf new-list-n   (list-length new-list))
    (values new-list new-list-n)
    ))
;;TEST
;;  (find-every-nth-item 2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))
;; works= (1 3 5 7 9 11 13 15 17 19 21)     11     
;; (find-every-nth-item 3  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))
;; works= (1 4 7 10 13 16 19)   7
;; (find-every-nth-item 2 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21) :max-length  8) 
;; works= (1 3 5 7 9 11 13 15)    8
      


;;XXX 
;; RADIAN DEFINITION=the angle subtended at the centre of a circle, measured in radians, is equal to the RATIO of the LENGTH OF THE ENCLOSED ARC to the LENGTH OF THE CIRCLE'S RADIUS. 
;; 180 degrees= p; 90 degrees= p2; 60 degrees= p3; 45 degrees= p4; 30 degrees= p6
;; Step 1 Calculate Opposite/Adjacent = 300/400 = 0.75.
;; Step 2 Find the ANGLE from your calculator using tan-1
;; In right triangle: 
;;360 (degrees) = 2 * pi (radians); 180 (degrees) = pi (radians)
;;1 degree = .01745 radians.
;;1 radian = 57.2957 degrees.
;; (atan .5) = 0.4636 (about 45 degrees)
;; 45 degrees = about .785 radians = 1/4 pi;  90 degrees = about 1.57 radians = 1/2 pi
;; 180 degrees = about 3.14  = pi  ; .360 degrees = about 6.28 radians = 2 pi


;;CALC-ANGLES-FOR-TRIANGLE
;;2018
;;ddd
#|(defun calc-angles-for-triangle (adjacent opposite)
  "In   For right triangle only.  RETURNS    INPUT:  "
  (let*
      ((radians (calc-radians-from-triangle adjacent opposite))
       (angle (* radians (/ 180 pi)))
       (ratio (/ opposite adjacent))
       ;;(angle (atan radians))
       )
    (values angle radians ratio)
    ;;end let, calc-angles-for-triangle
    ))|#
;;TEST
;; (calc-angles-for-triangle 2 1)
;; works?= 26.56505088990245D0  0.4636476  1/2
;; (calc-angles-for-triangle  3 3)'
;; works= 45.00000125223908D0   0.7853982   1


;CALC-RADIANS-FROM-2PTS
;;2018
;;ddd
(defun calc-radians-from-2pts (x1 y1 x2 y2 &key radius center-pt (dec 3))
  "U-math   RETURNS (values sweep-angle-radians start-angle-radians sweep-angle start-angle center-x center-y center-xx center-yy)  INPUT: Must specify radius or center-pt  dec= decimals"
  (let*
      ((center-x )
       (center-y)
       (dist-betw-pts (realpart (calc-dist-betw-2-pts x1 y1 x2 y2)))
       (min-radius (realpart (* 0.5 dist-betw-pts))) ;;= len-oppline of triangle
      ;; (tri1-radians)
      ;; (tri1-radians)
       (radians)
       ;;xy0-xy1-angle
       (start-angle-radians)
       (sweep-angle-radians)
       ;;xy0-xy1-angle
       (start-angle)
       (sweep-angle)
       (len-cntrline)
       (len-start-oppline) ;;line perpend to x-axis from center-x,y
       (center-xx)
       (center-yy)
       )
    ;;FIND CENTER POINT(S)
    (cond
     (center-pt
      (setf center-x (round-real (car center-pt) :dec dec)
            center-y (round-real (second center-pt) :dec dec)
            center-pt (list center-x center-y)
            radius (round-real (calc-dist-betw-2-pts center-x center-y x1 y1) :dec dec)))
     (t 
      (multiple-value-bind (center-x1 center-y1 center-xx1 center-yy1 radius1)
          ;;rest is pts-dist  pts-line-direction  x3 y3 centerline-dir  len-centerline)
          (calc-center-from-pts x1 y1 x2 y2 :radius radius)
        (setf radius (round-real radius1 :dec dec)
              center-x (round-real center-x1 :dec dec)
              center-y (round-real center-y1 :dec dec)
              center-xx (round-real center-xx1 :dec dec)
              center-yy (round-real center-yy1 :dec dec))
#|              center-pt (list center-x center-y)
              center-pt2 (list (realpart (car center-pt3)) (realpart (second center-pt3))))|#
        ;;(break "0")
        ;;end mvb, t, cond
        )))
    ;;CALC  len-start-oppline, START & SWEEP RADIANS, ANGLES
    (setf len-start-oppline (- y1 center-y)
          start-angle-radians (round-real (asin (/ len-start-oppline radius)) :dec dec)
          sweep-angle-radians
          (* 2 (round-real (calc-radians-from-triangle nil min-radius :len-hypot radius) :dec dec)))
    (when (numberp sweep-angle-radians)
        (setf  sweep-angle (round-real (calc-radians-to-angle sweep-angle-radians) :dec dec)))
    (when (numberp start-angle-radians)
           (setf   start-angle (round-real (calc-radians-to-angle start-angle-radians) :dec dec)))
    ;;(break "1")
    (values sweep-angle-radians start-angle-radians sweep-angle start-angle 
            center-x center-y center-xx center-yy)
    ;;end let, calc-radians-from-2pts
    ))
;;TEST
;;  (calc-radians-from-2pts 100 0  90  80)
;;= 0.68 -0.215 38.961 -12.319         -18.137  25.858        208.137 54.142
;; sweep-angle-radians start-angle-radians sweep-angle start-angle center-x center-y center-xx center-yy)
;; ;;  (calc-radians-from-2pts 100 30  90  70)
;;  =  0.68  -0.095  38.961 -5.443      38.432 35.858     151.568 64.142




;;CALC-ANGLE-TO-RADIANS
;;2018
;;ddd
(defun calc-angle-to-radians (angle) 
  "U-math   RETURNS radians . Note:  radians = pi * angle/180"
  (let*
      ((radians (when (numberp angle)
                  (/ (* pi angle) 180)))
       )
    radians
    ;;end let, calc-angle-to-radians
    ))
;;TEST
;; (calc-angle-to-radians 45) = 0.7853981633974483D0  
;; (calc-angle-to-radians 90) = 1.5707963267948966D0
;; (calc-angle-to-radians 135) = 2.356194490192345D0
;; (calc-angle-to-radians 180) = 3.141592653589793D0
;; (calc-angle-to-radians 270) = 4.71238898038469D0
;; (calc-angle-to-radians 360) = 6.283185307179586D0



;;CALC-RADIANS-TO-ANGLE
;;2018
;;ddd
(defun calc-radians-to-angle (radians &key (round-dec 3)) 
  "U-math   RETURNS angle . Note:  angle = (180 * radians)/pi "
  (let
      ((angle 
        (when (numberp radians)
          (* (/ 180 pi) radians)))
       )
    (when round-dec
      (setf angle (my-round angle :dec round-dec)))
    angle
    ;;end let, calc-radians-to-angle
    ))
;;TEST
;; (calc-radians-to-angle pi) = 180.0D0  180.0D0 
;; (calc-radians-to-angle 1.5) =  85.944
;; (calc-radians-to-angle 1.5 :round-dec nil) = 85.94366926962349D0




;;CALC-RADIANS-FROM-TRIANGLE
;;2018-02
;;ddd
(defun calc-radians-from-triangle (len-adjacent len-opposite &key len-hypot
                                                round-dec)
  "U-math  RETURNS (values radians type ratio) of angle from sides of right triangle. TYPE either asin acos or atan.  NOTE: #C(real imaginary) components of number"
  (let*
      ((radians)
       (ratio)
       (type)
       )
    (cond
     ((and len-hypot len-opposite)
      (setf ratio (/ len-opposite len-hypot)
            type 'asin
            radians (asin ratio)))
     ((and len-hypot len-adjacent)
      (setf ratio (/ len-adjacent len-hypot)
            type 'acos
            radians (acos ratio)))
     ((and len-adjacent len-opposite)
      (setf ratio (/ len-opposite len-adjacent)
            type 'atan
            radians (atan ratio))))
    (when round-dec
      (setf radians (my-round radians :dec round-dec)))
    ;; (break)
    (values radians type ratio)
    ;;end let, calc-radians-from-triangle
    ))
;;TEST
;;ATAN
;; (calc-radians-from-triangle 1.0 1.0) 
;;45 = 0.7853982  ATAN 1.0
;; (calc-radians-from-triangle 2.0  0.0001) 
;;0 for almost 0 degree angle = 5.0E-5  ATAN  5.0E-5
;; (calc-radians-from-triangle 2.0  3.9 ) 
;;180 for almost 180 degree angle = 1.096945  ATAN  1.95
;;ASIN
;; (calc-radians-from-triangle NIL 2  :len-hypot 2.828427)
;;45 = 0.7853981  ASIN  0.70710677
;;90 (calc-radians-from-triangle NIL 1.0  :len-hypot 1.0)
;;90 = 1.5707964  ASIN  1.0
;;ACOS
;; (calc-radians-from-triangle  2 NIL :len-hypot 2.828427)
;;45 = 0.7853982  ACOS  0.70710677
;; (calc-radians-from-triangle 0.0001 nil :len-hypot 3.0)
;; 0 degree angle = 1.570763
;; (calc-radians-from-triangle 3.99 nil :len-hypot 2.0)
;; 179 degree angle = #C(0.0 1.3140663) ACOS  1.995;  0 is real part
;; (calc-radians-from-triangle nil 1  :len-hypot 3)
;; = 0.33983693
;; From table:  
;;  DEGREES  0             30             45             60            90        180      270      360
;;    RADIANS  0          Pi/6          Pi/4            Pi/3        Pi/2     **Pi       3Pi/2     2Pi
;;  *Radians      0         0.5235    0.7853        1.0471     1.5707  3.1415  4.7123  6.2831
;;         SIN        0            1/2       1/(sqrt 2)   (sqrt 3)/2     1           0          -1         0
;;         COS       1      (sqrt 3)/2  1/(sqrt 2)      1/2             0          -1          0           1
;;         TAN       0      1/(sqrt 3)        1             (sqrt 3)   undef      0      undef      0
;;         SEC       1       2/(sqrt 3)    (sqrt 2)        2            infin
;;       COSEC   infin     2             (sqrt 2)      2/(sqrt 3)     1
;;         COT      infin   (sqrt 3)         1           1/(sqrt 3)      0


;;CALC-ANGLES-FROM-TRIANGLE
;;2018-02
;;ddd
(defun calc-angle-from-triangle (len-adjacent len-opposite &key len-hypot
                                              (round-dec 3))
  "U-math  RETURNS (values radians type ratio) of angle from sides of right triangle. TYPE either asin acos or atan.  NOTE: #C(real imaginary) components of number"
  (let*
      ((angle)
       )
    (multiple-value-bind (radians type ratio)
        (calc-radians-from-triangle len-adjacent len-opposite 
                                    :len-hypot len-hypot)
      (setq angle (calc-radians-to-angle radians))     
      (when round-dec
        (setf angle (my-round angle :dec round-dec)))
      (values angle radians type ratio)
      ;;end mvb, let, calc-angle-from-triangle
  )))
;;TEST
;; (calc-angle-from-triangle 10 10)
;; works= 45.0   0.7853982 ATAN  1
;; w round-dec nil
;; works= 45.00000125223908D0   0.7853982  ATAN  1



;;CALC-CENTER-FROM-PTS
;;2018
;;ddd
(defun calc-center-from-pts (x1 y1 x2 y2  &key (rel-r 3.0) radius arc-ht  (dec 3))
  "U-math   RETURNS (values  x y xx yy radius  pts-dist  pts-line-direction  x3 y3
            centerline-dir  len-centerline)   INPUT: REL-R = multiple of half of pts-dist = min-radius = length farside of R-triangle, Centerline = nearside of R-triangle, radius = hypotenuse of R-triangle.  [Note: center= x,y, radius=xy-xy1 or xy-xy2=hypot, min-radius= xy1-xy3 or xy2-xy3, centerline=xy-xy3 xx,yy= other center 180deg away.]. DEC=num decimals."
  (let*
      ((pts-dist (round-real (calc-dist-betw-2-pts x1 y1 x2 y2) :dec dec)) ;;correct
        ;;(sqrt (+ (expt (- x2 x1) 2.0) (expt (- y2 y1) 2.0)))
       (min-radius (* 0.5 pts-dist)) ;;correct
       ;;radius = R triangle hypotenuse
       (radius (cond 
                (radius radius) 
                (arc-ht
                 (setf radius (calc-radius-from-arc-height x1 y1 x2 y2 arc-ht
                                          :sweep-angle NIL :pts-dist pts-dist :dec dec)))
                (t (* rel-r min-radius)))) ;;given
       (x3  (round-real (/ (+ x1 x2) 2.0):dec dec)) ;;correct
       (y3  (round-real (/ (+ y1 y2) 2.0) :dec dec)) ;;correct                               
       (len-centerline  (round-real (sqrt (- (expt radius 2.0) (expt min-radius 2.0))) :dec dec)) ;;correct
       (pts-line-direction (list (- x2 x1) (- y2 y1))) ;;??
       ;;centerline-dir (since 90deg to pts-line) 
       (centerline-dir (list (- y2 y1)(- x1 x2))) ;;??
       (x (- x3  (round-real (/ (* len-centerline 0.5 (- y2 y1))  min-radius) :dec dec)))
        ;;was (+  min-radius x3));; (+ (sqrt (- (expt radius 2.0) (expt len-centerline 2.0)) ) x3))
       (y (+ y3  (round-real (/ (* len-centerline 0.5 (- x2 x1))  min-radius) :dec dec)))
       ;;was(+  min-radius y3))   ;;(+ (sqrt (- (expt radius 2.0) (expt len-centerline 2.0)) ) x3))    
       (xx  (round-real (+ x3 (/ (* len-centerline 0.5 (- y2 y1))  min-radius)) :dec dec))
       (yy  (round-real (- y3 (/ (* len-centerline 0.5 (- x2 x1))  min-radius)) :dec dec))
       )
    ;;(break "calc-center-from-pts")
    (values  x y xx yy radius  pts-dist  pts-line-direction  x3 y3
            centerline-dir  len-centerline)
    ;;end let, calc-center-from-pts
    ))
;;TEST
;; (calc-center-from-pts 0 0  100 0); along x axis,  centerline all x= 50.0
;; works= all correct? =
;; = 50.0  141.421  50.0  -141.421 150.0  100.0  (100 0)  50.0  0.0  (0 -100)  141.421 
;;(values  x y xx yy radius  pts-dist  pts-line-direction  x3 y3 centerline-dir  len-centerline)
;; ;; (calc-center-from-pts 100 100  200 200)
;;works? = 8.578003  291.422  291.422  8.578  212.1315 141.421 (100 100) 150.0 150.0 (100 -100) 200.0




;;CALC-RADIUS-FROM-ARC-HEIGHT
;;2018
;;ddd
(defun calc-radius-from-arc-height (x1 y1 x2 y2 arc-ht 
                                       &key sweep-angle pts-dist (dec 3) find-arc-centers-p)
  "U-math REWRITE THIS:  RETURNS (values  radius sweep-angle sweep-rads pts-dist x3 y3 x0 y0 xx0 yy0) [centers only if find-arc-center-p]  INPUT:  radius = hypotenuse of R-triangle.  [Note: center= x,y, radius=xy-xy1 or xy-xy2=hypot, min-radius= xy1-xy3 or xy2-xy3, centerline=xy-xy3 xx,yy= other center 180deg away.]. DEC=num decimals."
  (unless pts-dist
    (setf pts-dist (round-real (calc-dist-betw-2-pts x1 y1 x2 y2) :dec dec)))     
  (let*
      ((min-radius (* 0.5 pts-dist)) ;;correct
       ;;radius = R triangle hypotenuse
       (radius) 
       (x3  (round-real (/ (+ x1 x2) 2.0):dec dec)) ;;correct
       (y3  (round-real (/ (+ y1 y2) 2.0) :dec dec)) ;;correct    
       (half-sweep (when sweep-angle
                     (* 0.5 sweep-angle)))
       (sweep-rads)
       (tan-half-sweep) 
       (half-sweep-rads) 
       (x)
       (y)
       (xx) ;; (round-real (+ x3 (/ (* len-centerline 0.5 (- y2 y1))  min-radius)) :dec dec))
       (yy) ;;  (round-real (- y3 (/ (* len-centerline 0.5 (- x2 x1))  min-radius)) :dec dec))
       (cent-x)
       (cent-y)
       (sin-half-sweep)
       (x0)
       (y0)
       (xx0)
       (yy0)
       )
    (cond
     (half-sweep
      (setf half-sweep-rads (calc-angle-to-radians half-sweep)))
     (t  (setf tan-half-sweep (/ arc-ht min-radius)
               half-sweep-rads (atan tan-half-sweep))))
    ;;find the radius
    (setf half-sweep  (calc-radians-to-angle half-sweep-rads)
       sweep-rads (round-real (* 2.0 half-sweep-rads) :dec dec)
       sweep-angle (round-real (calc-radians-to-angle sweep-rads) :dec dec)
       sin-half-sweep (sin half-sweep-rads)
       radius (round-real (/ min-radius sin-half-sweep) :dec dec))
    ;;SSS LATER FIND X & Y and XX,YY centers--or use separate function
    (when find-arc-centers-p
      (multiple-value-setq (x0 y0 xx0 yy0)
          (calc-center-from-pts x1 x2 y1 y2 :radius radius))) 
    (values  radius sweep-angle sweep-rads pts-dist x3 y3 x0 y0 xx0 yy0)
    ;;end let, calc-radius-from-arc-height
    ))
;;TEST
;; (calc-radius-from-arc-height  100 150  150 250  40 :find-arc-centers-p T)
;; = 96.065  71.161   1.242 111.803 125.0 200.0   55.123 234.939 194.877 165.061  




;; WERE IN U-art-math-utilities.lisp *****;

;;MY-FLOOR-CEILING
;;
;;ddd
(defun my-floor-ceiling (number &key floor ceiling)
  "In U-math"
  (let 
      ((return-num number)
       )
    (when floor
      (when (> floor number)
        (setf return-num floor)))
    (when ceiling
      (when (< ceiling number)
        (setf return-num ceiling)))
    return-num
    ))
;;TEST
;;  (my-floor-ceiling 50 :floor 60) = 60
;;  (my-floor-ceiling 50 :ceiling 40) = 40
;;  (my-floor-ceiling 50 :floor 0 :ceiling 100) = 50
;;  (my-floor-ceiling -20 :floor 0 :ceiling 100) = 0
;;  (my-floor-ceiling 200  :floor 0 :ceiling 100) = 100



;;MY-ROUND
;;2018
;;ddd
(defun my-round (number &key (dec 3))
  "U-math  RETURNS new number unless dec=NIL, returns orig number."
  (cond
   (dec
    (let*
        ((mult (expt 10 dec))
         (bignum (* mult number))
         (roundbig (round bignum))
         )
      (setf number  (/ (* 1.0 roundbig) mult))
      ))
   (t number))
    ;;end my-round
    )
;;TEST
;;  (my-round  222.33333333) = 222.333
;;  (my-round  222.33333333 :dec 0) = 222.0
;;  (my-round  222.33333333 :dec nil) = 222.33333
;; (my-round  222.33333333 :dec -1) = 220.0
    

;;ROUND-REAL
;;2018
;;ddd
(defun round-real (number &key (dec 3))
  "U-math  RETURNS new number unless dec=NIL, returns orig number. FIRST finds the realpart of the number."
  (let*
      ((realnum (realpart number))
       )
    (my-round realnum :dec dec)
    ;;end round-real
    ))
;;TEST
;; (round-real 97/101) = 0.96




;;CONVERT-TO-INTEGER
;;2020
;;ddd
(defun convert-to-integer (number)
  "U-math   RETURNS (values  integer int-string)   INPUT: Float number,  ratio number"
  (let*
      ((num-str (format nil "~A" (my-round number)))
       (dec-point-n (search "." num-str))
       (int-string (subseq num-str 0 dec-point-n))
       (integer (convert-string-to-integer int-string))
       )
    (values  integer int-string)
    ;;end let, defun
    ))
;;TEST
;; (convert-to-integer 33.45678)
;; works = 33  "33"
;; (convert-to-integer 334)
;; works= 334  "334"
;; (convert-to-integer (/ 7 3)) = 2  "2"


;;MY-FLOOR
;;
;;ddd
(defun my-floor (number &key (if-above-floor-p T)  (floor 0) )
  "In U-math, returns floor if number is below floor (negative), unless if-above-floor-p, then returns floor if number is above floor (positive)"
  (let
      ((return-num floor)
       )
    (cond
     ;;if if-above-floor-p = T
     (if-above-floor-p
      ;;if number is above floor, return number otherwise return floor
      (if (> number floor)
          (setf return-num number)))
     ;;if. if-above-floor-p = T
     (t 
      ;;if-above-floor-p = NIL
      ;;if number is below floor, return number otherwise return floor
      (if (< number floor)
          (setf return-num number))))
    return-num
    ;;end let, my-zero-floor
    ))
;;TEST
;;  (my-floor 0.2) = 0.2
;;  (my-floor -0.2) = 0
;;  (my-floor 0.2 :if-above-floor-p nil) = 0
;;  (my-floor -0.2 :if-above-floor-p nil)  = -0.2
;;  (my-floor 0.2 :floor 0.1) = 0.2
;;  (my-floor -0.2 :floor 0.1) = 0.1
;;  (my-floor 0.2 :floor  7) = 7




;;MY-LIST-ITEM-FLOOR
;;
;;ddd
(defun my-list-item-floor (list  &key if-above-floor-p  (floor 0) )
  "In U-math, returns 0 if number is negative, unless if-positive-zero-p, then returns 0 if number is positive FOR EACH MEMBER OF A LIST"
  (let
      ((return-list)
       (return-item)
       )
    (loop
     for item in list
     do
     (cond
      ((numberp item)
       (setf return-item (my-floor item :if-above-floor-p if-above-floor-p  :floor floor)))
      (t (setf return-item item)))
     (setf return-list (append return-list (list return-item)))
     ;;end loop
     )
    return-list
    ;;end let, my-list-zero-floor
  ))
;;TEST
;;  SSS 
;;  (my-list-item-floor  '(1 -1 0.3 -0.1))  = (1 0 0.3 0)
;; (my-list-item-floor  '(1 -1 0.3 -0.1) :if-above-floor-p nil :floor 6) = (1 -1 0.3 -0.1)
;; (my-list-item-floor  '(1 -1 0.3 -0.1) :if-above-floor-p nil) = (0 -1 0 -0.1)





;;CALC-ARC-POINTS
;;2018
;;ddd
(defun calc-arc-points (x1 y1 x2 y2  &key x0 y0 xy1-xy2angle 
                           xy1-xy2angle-rads radius (rel-radius 1.5) radius2  
                           ;;add later? (radians/pt 2.0)
                           (n-pts 5) xy1-xy2 start-angle start-angle-rads
                           (rel-radius2-1 1.0) (dec 3) clockwise-p) 
  "In   RETURNS (values arc-points x0 y0 x2 y2 xy1-xy2angle start-angle 
            radius radius2 xy1-xy2angle-rads start-angle-rads)   INPUT:  REL-RADIUS= relative to xy1-xy2 distance. CENTER=x0,y0"
  (unless xy1-xy2
    (setf xy1-xy2 (sqrt (+ (expt (- x2 x1) 2)(expt (- y2 y1) 2)))))
  (unless radius
    (setf radius (* rel-radius xy1-xy2)))
  (let*
      ((xx0)
       (yy0)
       (pts-dist)
       (arc-points)
       (incr-angle)
       (incr-angle-rads)
       )
   
    ;;CALC THE OVERALL ANGLE?
    (unless xy1-xy2angle
      (multiple-value-setq (xy1-xy2angle xy1-xy2angle-rads radius)
          (calc-arc-angle x1 y1 x2 y2 :x0  x0 :y0 y0  :radius radius
                          :rel-radius rel-radius :dec dec)))
   
    ;;CALC THE CENTER?
    (unless (and x0 y0)
      (multiple-value-setq (x0 y0  xx0 yy0 radius  xy1-xy2) 
          ;;not needed? pts-line-direction  x3 y3  centerline-dir  xy0-xy3)
          (calc-center-from-pts x1 y1 x2 y2 :rel-r  rel-radius
                                :radius radius :dec dec)))
    ;;CALC RADIUS--don't find radius2, that will make it gradually increase 
    ;; inside the loop
    (unless radius
      (when xy1-xy2
        (setf radius (* rel-radius xy1-xy2))))

    ;;CALC OVERALL ANGLE BETW X1 and X2 IF NOT GIV
    (unless (and xy1-xy2angle start-angle xy1-xy2angle-rads
                 start-angle-rads x2 y2)
      (multiple-value-setq (x2 y2 xy1-xy2angle start-angle radius radius2 
                               xy1-xy2angle-rads start-angle-rads)
          (calc-arc-point x0 y0 x1 y1 xy1-xy2angle   
                          :start-angle start-angle :radius radius :radius2   radius2 
                          :rel-radius2-1 rel-radius2-1 :dec dec
                          :angle-rads xy1-xy2angle-rads :start-angle-rads  start-angle-rads 
                          :clockwise-p  clockwise-p))) ;; :x3 y3 )));; :xy1-xy xy0-xy3 )))

    ;;DIVIDE THE XY1-XY2ANGLE INTO N-PTS PARTS
    (setf incr-angle (/ xy1-xy2angle n-pts)
          incr-angle-rads (/ xy1-xy2angle-rads n-pts))

    ;;LOOP FOR ALL POINTS
    (loop
     for n from 1 to n-pts 
     do
     (let*
         ((angle1 (* n incr-angle))
          (angle1-rads (* n incr-angle-rads))                    
          )
       ;;calc each point
       (multiple-value-bind (x y angle start-angle radius radius2 
                               angle-rads start-angle-rads)
           (calc-arc-point x0 y0 x1 y1 angle1   
                           :start-angle start-angle :radius radius :radius2   radius2 
                           :rel-radius2-1 rel-radius2-1 :dec dec
                           :angle-rads angle1-rads :start-angle-rads  start-angle-rads 
                           :clockwise-p  clockwise-p) ;;:x3 y3  :xy1-xy xy0-xy3 )
         ;;APPEND THE NEW X,Y POINT
         (setf arc-points (append arc-points (list 
                                              (list x y (round-real angle1 :dec dec)))))
         ;;end mvb,let, loop
         )))
    (values arc-points (round-real x0 :dec dec) (round-real y0 :dec dec)
            x2 y2 xy1-xy2angle start-angle 
            radius radius2 xy1-xy2angle-rads start-angle-rads)   
    ;;end let, calc-arc-points
    ))
;;TEST
;;  (calc-arc-points  100 30 90 80)
;;arc-points=((76.276 -5.655 3.896) (76.484 -0.459 7.792) (76.338 4.739 11.689) (75.84 9.915 15.585) (74.991 15.045 19.481))
;; x0 y0 x2 y2 xy1-xy2angle start-angle radius radius2 xy1-xy2angle-rads start-angle-rads)    
;; 24.289 40.858   74.991 15.045   19.481  -8.136           76.485  76.485  0.34 -0.142 
;;
;; (calc-arc-points  100 30 90 80 :rel-radius2-1 1.2)
;; ((91.531 -6.786 3.896) (91.78 -0.551 7.792) (91.606 5.687 11.689) (91.008 11.898 15.585) (89.989 18.054 19.481))
;; 24.289  40.858   89.989 18.054  19.481 -8.136  76.485  91.782006 0.34 -0.142
;; NOTE:  (89.989 18.054 19.481) has larger x,y, same radius as (74.991 15.045 19.481); both were last points in loop



;;CALC-ARC-POINT
;;2018
;;ddd
(defun calc-arc-point (x0 y0 x1 y1 angle   
                          &key start-angle radius radius2 (rel-radius2-1 1.0) (dec 3)
                          angle-rads start-angle-rads clockwise-p  x3 y3  xy1-xy xy0-xy3 )
  "In   RETURNS (values x y angle start-angle radius radius2 angle-rads start-angle-rads).  XY is new pt on arc, angle (counterclock) from xy1.  INPUT:   XY3 is a point on radius to xy that is 90deg to point xy1. START-ANGLE is angle from line parallel to x-axis to xy0-xy1. DEC= round to dec decimals"
  (let
      ((x)
       (y)
       (angle-sum)
       (angle-sum-radians)
       (centerline-dir)
       (sin0)
       (cos0)
       (sin-sum)
       (cos-sum)
       (x0-x1)
       (y0-y1)
       (tan-start-angle)
       (start-angle-rads)
       (start-angle)
       (tan-angle-sum)
       (angle-sum-rads)
       (sin-angle-start)
       (cos-angle-start) 
       )
    ;;IF NO ANGLE or RADIUS GIVEN, CALC -- FIX THIS SECTION LATER?
    (when (and (null radius) x0 y0 x1 y1)
      (setf radius (round-real (calc-dist-betw-2-pts x0 y0 x1 y1) :dec dec)
            radius2 (round-real (* rel-radius2-1 radius) :dec dec)))
    (when (null radius2)
      (setf radius2 (* rel-radius2-1 radius)))

    (when (and (null angle)(null angle-rads))
      (cond
       ((and xy0-xy3 radius) NIL)
       ((and x3 y3 radius)
        (setf xy0-xy3 (calc-dist-betw-2-pts x1 y1 x3 y3)))
       (t nil))
      (when  xy0-xy3
        (setf cos0 (/ xy0-xy3 radius)
              angle-rads (round-real (acos cos0) :dec dec))))
    ;;CALC BOTH ANGLE AND ANGLE-RADS       
    (when (and (null angle) angle-rads)
      (setf angle (round-real (calc-radians-to-angle angle-rads) :dec dec)))
    (when (and angle (null angle-rads))
      (setf angle-rads (round-real (calc-angle-to-radians angle) :dec dec)))

    ;;FIND START-ANGLE AND ANGLE-SUM
    (cond
     ((null clockwise-p)
      (unless start-angle
        (setf x0-x1 (- x1 x0)
              y0-y1 (- y1 y0)
              tan-start-angle (/ y0-y1 x0-x1)
              start-angle-rads (round-real (atan tan-start-angle) :dec dec)
              start-angle (round-real (calc-radians-to-angle start-angle-rads) :dec dec)))
      (setf angle-sum (+ angle start-angle)
            angle-sum-rads (round-real (calc-angle-to-radians angle-sum) :dec dec))
      ;;CALC X and Y
      (setf sin-sum (sin angle-sum-rads)
            cos-sum (cos angle-sum-rads)
            x (round-real (* cos-sum radius2) :dec dec)
            y (round-real (* sin-sum radius2) :dec dec))
      ;;(break)
      )
     (t 
      (unless start-angle
        (setf x0-x1 (- x1 x0)
              y0-y1 (- y1 y0  )
              tan-angle-sum (/ y0-y1 x0-x1)
              angle-sum-rads (atan tan-angle-sum)
              angle-sum (calc-radians-to-angle angle-sum-rads)))
      (setf start-angle (round-real  (- angle-sum angle ) :dec dec)
            start-angle-rads (round-real  (calc-angle-to-radians start-angle) :dec dec))
      ;;CALC X and Y
      (setf sin-angle-start (sin start-angle-rads)
            cos-angle-start (cos start-angle-rads)
            x (round-real (* cos-angle-start radius2) :dec dec)
            y (round-real (* sin-angle-start radius2) :dec dec))
      ))
    (values x y angle start-angle radius radius2 angle-rads start-angle-rads)
    ;;end let, calc-arc-point
    ))
;;TEST
;; simple eg
;; (calc-arc-point  0 0 100 0 45)
;;     (values x y angle start-angle radius radius2 angle-rads start-angle-rads)
;; = 70.711  70.711  45  0.0D0  100.0 100.0  0.7853981633974483D0  0.0
;; offset
;; ;; (calc-arc-point  20 30 100 40 45)
;; = 49.546  63.603  45  7.105 80.623 80.623 0.785 0.124
;; dif radiuses
;; (calc-arc-point  20 30 100 40 45 :rel-radius2-1 1.5)
;; = 74.318   95.404 45 7.105 80.623 120.934  0.785 0.124
;; ;;clockwise
;; ;; (calc-arc-point  20 30 100 40 45 nil :clockwise-p T)
;; = 63.642  -49.495  45 -37.875 80.623 80.623 0.785 -0.661
;; dif radiuses
;; (calc-arc-point  20 30 100 40 45 :rel-radius2-1 1.5)
;; = 74.318   95.404 45 7.105 80.623 120.934  0.785 0.124

;;DELETE IF ABOVE WORKS?
#|(defun calc-arc-point (x0 y0 x1 y1 angle &optional radius angle-rads
                             &key  x3 y3  xy1-xy xy0-xy3 clockwise-p)
  "In   RETURNS (values x y angle radius angle-rads xy0-xy3 xy1-xy3) xy is new pt on arc, angle (counterclock) from xy1.  INPUT:   XY3 is a point on radius to xy that is 90deg to point xy1."
  (let
      ((xy1-xy3)
        ;;(rad-cntline-ratio)
        (x)
        (y)
        (centerline-dir)
       (sin0)
       (cos0)
       )
    ;;IF NO ANGLE or RADIUS GIVEN, CALC
    (when (and (null radius) x0 y0 x1 y1)
      (setf radius (calc-dist-betw-2-pts x0 y0 x1 y1)))      
    (when (and (null angle)(null angle-rads))
      (cond
       ((and xy0-xy3 radius) NIL)
       ((and x3 y3 radius)
        (setf xy0-xy3 (calc-dist-betw-2-pts x1 y1 x3 y3)))
       (t nil))
      (when  xy0-xy3
        (setf cos0 (/ xy0-xy3 radius)
              angle-rads (acos cos0))))
    (when xy1-xy
      (setf xy1-xy3 (* 0.5 xy1-xy)))
    ;;CALC BOTH ANGLE AND ANGLE-RADS       
    (when (and (null angle) angle-rads)
      (setf angle (calc-radians-to-angle angle-rads)))
    (when (and angle (null angle-rads))
      (setf angle-rads (calc-angle-to-radians angle)))

    (cond
     ((and angle radius)
      (when (null xy0-xy3)
        (setf cos0 (cos angle-rads)
             xy0-xy3 (* cos0 radius)))
      (when (null xy1-xy)
        (setf sin0 (sin angle-rads)
         xy1-xy3 (* cos0 radius)
         xy1-xy (* 2.0 xy1-xy3))))
      (t nil))

    ;;CALC XY3

    (cond
     ((null clockwise-p)
      ;;SS MUST FIND XY3 ABOVE??
      (setf x (- x3  (round-real (/ (* xy0-xy3  (- y3 y1))  xy1-xy3) :dec dec))
            ;;was (+  min-radius x3));; (+ (sqrt (- (expt radius 2.0) (expt len-centerline 2.0)) ) x3))
            y (- y3  (round-real (/ (* xy0-xy3  (- x3 x1))  xy1-xy3) :dec dec)))
      ;;copied    y (+ y3  (round-real (/ (* len-centerline 0.5 (- x2 x1))  min-radius) :dec dec)))
      )
     (t 
      ;;SS FINISH FOR COUNTERCLOCK
      ))
                    
    (values x y angle radius angle-rads xy0-xy3 xy1-xy3)
    ;;end let, calc-arc-point
    ))|#






;;CALC-ARC-POINT-FROM-ANGLE
;;2018
;;ddd
(defun calc-arc-point-from-angle (point-angle x0 y0 x1 y1 &key radius start-angle (dec 3))
  "U-math, Calcs x,y of arc point at point-angle from xy1.   RETURNS (values x y start-angle1)   INPUT: sweep-angle is from xy1. xy0=center, xy3 midpoint of line betw 2 arc ends. start-angle betw xy0-xy1 and x-axis; sum-angle= sum point-angle + xy0-angle."
  (unless radius
    (setf radius (sqrt (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2)))))
  (let*
      ((xy1-xy3)
        (rad-cntline-ratio)
        (x)
        (y)
        (xy3)
        (len-centerline)
        (half-pt-angle (* 0.5 point-angle))
        ;;(half-pt-angle-rads (calc-angle-to-radians half-pt-angle))
        (tan-start-angle (/ (- y1 y0) (- x1 x0)))
        (start-angle-rads (atan tan-start-angle))
        (start-angle1 (cond
                      (start-angle start-angle)
                      (t (round-real (calc-radians-to-angle start-angle-rads) :dec dec))))
        (sum-angle (+ point-angle start-angle1))
        (sum-angle-rads (calc-angle-to-radians sum-angle))
        (sin-sum-angle (sin sum-angle-rads))
        (cos-sum-angle (cos sum-angle-rads))
        (x0-x (* radius sin-sum-angle))
        (y0-y (* radius cos-sum-angle))
        (x (round-real (+ x0 x0-x) :dec dec))
        (y (round-real (+ y0 y0-y) :dec dec))
       )
    (values x y start-angle1)
    ;;end let, calc-arc-point-from-angle  
    ))
;;TEST
;; (calc-arc-point-from-angle 30 50  40   150 200) = 55.664  228.595 57.995




;;CALC-ARC-ANGLE
;;2018
;;ddd
(defun calc-arc-angle (x1 y1 x2 y2 &key x0 y0 radius   (rel-radius 1.5) (dec 3) )
  "In   RETURNS (values  arc-angle angle-rads radius)    INPUT:  rel-radius= radius set as ratio to dist xy1-xy2. x0,y0 is center. DEC is round decimals."
  (let*
      ((arc-angle)
       (arc-rads)
       (xy1-xy2 (calc-dist-betw-2-pts x1 y1 x2 y2))
       (min-radius (* 0.5 xy1-xy2))
       (sin-angle)         
       (angle-rads)
       )
    ;;CALC RADIUS?
    (cond
     (radius nil)
     ((and (null radius) x0 y0)
      (setf radius (calc-dist-betw-2-pts x0 y0 x1 y1)))
     (xy1-xy2
      (setf radius (* rel-radius xy1-xy2)))
     (t nil))

    ;;CALC ANGLE
    (setf sin-angle (/ min-radius radius)
          angle-rads (round-real (asin sin-angle) :dec dec)
          arc-angle (round-real (calc-radians-to-angle angle-rads) :dec dec))
    (values  arc-angle angle-rads (round-real radius :dec dec))
    ;;end let, calc-arc-angle
    ))
;;TEST
;; (calc-arc-angle  100 30  90 60)  = 19.481  0.34 47.434
;; radius given, not calc with rel-radius
;; (calc-arc-angle  100 30  90 60  :radius 90) = 10.141  0.177 90.0



;;CALC-DIST-BETW-2-PTS
;;2018
;;ddd
(defun calc-dist-betw-2-pts (x1 y1 x2 y2)
"U-math   RETURNS    INPUT:  "
  (let
      ((distance (realpart (sqrt (+ (expt (- x2 x1) 2.0) (expt (- y2 y1) 2.0)))))
       )
    distance
    ;;end let, defun
    ))
;;TEST
;; (calc-dist-betw-2-pts  10  20   40 50) = 42.426407


;;solve for x2
;;  (expt distance 2.0) = (+ (expt (- x2 x1) 2.0) (expt (- y2 y1) 2.0))
;; (expt (- x2 x1) 2.0)j = (- (expt distance 2.0)(expt (- y2 y1) 2.0))
;; (- x2 x1) =  (sqrt (- (expt distance 2.0)(expt (- y2 y1) 2.0)))
;;  x2 =    (+ x1 (sqrt (- (expt distance 2.0)(expt (- y2 y1) 2.0))))

;;solve for y2:
;;  (expt distance 2.0) = (+ (expt (- x2 x1) 2.0) (expt (- y2 y1) 2.0))
;;  (expt (- y2 y1) 2.0) =  (- (expt distance 2.0) (expt (- x2 x1) 2.0))
;;  (- y2 y1)  =  (sqrt (- (expt distance 2.0) (expt (- x2 x1) 2.0)))
;;    y2 = (-  (realpart (sqrt (- (expt distance 2.0) (expt (- x2 x1) 2.0))) y1))
;;solve for x2




;;CALC-RANGLE-SIDE
;;2020
;;ddd
(defun calc-rangle-side (len-hyp len-side1)
  "U-math"
  (let*
      ((sq-len-side2 (- (expt len-hyp 2.0) (expt len-side1 2.0)))
       (len-side2 (sqrt sq-len-side2))
       )
    (values len-side2 sq-len-side2)
    ;;end let,calc-rangle-side
    ))
;;TEST
;; (calc-rangle-side 10 5) = 8.6602545   75.0



;;CALC-RANGLE-HYPOT
;;2020
;;ddd
(defun calc-rangle-hypot (len-side1 len-side2)
  "U-math"
  (let*
      ((sq-len-hypot (+ (expt len-side1 2.0) (expt len-side2 2.0)))
       (len-hypot (sqrt sq-len-hypot))
       )
    (values len-hypot sq-len-hypot)
    ;;end let,calc-rangle-hypot
    ))
;;TEST
;; (calc-rangle-hypot 8.6602545  5.0)  = 10.0   100.00001


;;CALC-HALF-ARROW-XYS
;;2020
;;ddd
;;DOES NOT WORK--Except in limited cases
;; Math became to complex/time consuming!!
#|(defun calc-half-arrow-xys (front-x front-y end-cent-x end-cent-y 
                               len-side len-vertical)
  "U-math"   ;;len-side = d2 len-vertical = d3
  (let*
      ((len-center-line (calc-dist-betw-2-pts front-x front-y 
                                              end-cent-x end-cent-y)) ;;d1
       (len-arrowXcent (calc-rangle-side len-side len-vertical)) ;;d4
       (len-xy2-xya (calc-rangle-hypot len-vertical 
                                       (- len-center-line len-arrowXcent))) ;;d7
       (y-arrow-end1 (abs (/ (- (expt len-xy2-xya 2.0) (expt len-side 2.0))
                       (* 2 (-  front-y end-cent-y)))))
       (x-arrow-end1 (abs (- front-x 
                       (realpart (sqrt (- (expt (- front-y y-arrow-end1) 2.0)
                                                  (expt len-side 2.0)))))))
       ;;sideb 
       ;;nothing like above works.  Did math & found equation too complex
       ;; to solve w/o extensive work
#|       (y-arrow-end2 (/ (- (expt len-xy2-xya 2.0) (expt len-side 2.0))
                       (* 2 (-  front-y end-cent-y)))00.+                                                                                 )
       (x-arrow-end2 (- front-x 
                       (realpart (sqrt (- (expt (- front-y y-arrow-end2) 2.0)
                                                  (expt len-side 2.0))))))|#
       )
  (values  x-arrow-end1  y-arrow-end1 len-arrowXcent)
  ))|#
;;TEST
;; (calc-half-arrow-xys 300 250 100 150 20 10)
;; 266.8338  211.27022  17.320509
;; (for graphic test, see H-GP-Drawing.lisp)
;; (calc-half-arrow-xys 300 250 100 150 8 4)
;; ;; (calc-half-arrow-xys 400 50 600 100 8 4)




;; DELETE =========================================
;;CALC-XYPOINT-FROM-2-PTS&DISTS
;;2018
;;ddd
       #|  FLAWED, INCOMPLETE MATH -- USE CALC-CENTER-FROM-2-PTS
(defun calc-xypoint-from-2-pts&dists (x1 y1 x2 y2 xy1-xy3  xy2-xy3 &key key )
  "U-math RETURNS    INPUT:  "
  (let
      ((x3)
       (y3)
       (
       )  ;;solve for x1, y1, rest are known.
   xy1-xy2^2 =  x1^2 - 2*x1*x2  + x2^2    +     y1^2 - 2*y1*y2 + y2^2 
          0 = -xy1-xy2^2  + x1^2 - 2*x1*x2  + x2^2    +     y1^2 - 2*y1*y2 + y2^2 
  A          x1^2 - 2*x1*x2  =  xy1-xy2^2  -  x2^2  +   y1^2 - 2*y1*y2 + y2^2 
  B          x1^2 - 2*x1*x3  =  xy1-xy3^2  -  x3^2  +   y1^2 - 2*y1*y3 + y3^2 

A-B     0  -2*x1*x2 + 2*x1*x3 = xy1-xy2^2 - xy1-xy3^2 -x2^2 + x3^2  0  -2*y1*y2 + y2^2 +2*y1*y3 - y3^2 
OR:  2*X1 * ( -x2 + x3) = xy1-xy2^2 - xy1-xy3^2 -x2^2 + x3^2 + 2Y1*(-y2 +  y3) + y2^2 - y3^2

OR:  X1 =[ xy1-xy2^2 - xy1-xy3^2 -x2^2 + x3^2 + 2Y1*(-y2 +  y3) + y2^2 - y3^2]  /  2( -x2 + x3)

SOLVE ANOTHER FOR Y1:
   x1^2 - 2*x1*x2  =  xy1-xy2^2  -  x2^2  +   Y1^2 - 2*Y1*y2 + y2^2 
   Y1^2 - 2*Y1*y2 =    x1^2 - 2*x1*x2 - xy1-xy2^2   -  x2^2 - y2^2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                

  |#


#|
If I have three points A, B, C and I know the distances between them and A is at 2D coordinates {0,0} and B is at {ab,0}, then what would be the formula to find the coordinates of the point C?
The point {cx, cy} has to solve two equations:

    cx^2+cy^2==ac^2 ;;dist from 
   (cx-ab)^2+cy^2==bc^2

=> cx^2-(cx-ab)^2==ac^2-bc^2
=> 2*cx*ab==ac^2-bc^2+ab^2

=> cx = (ac^2-bc^2+ab^2)/(2*ab)

=> cy = +/- sqrt(ac^2-cx^2)   iff ac^2-cx^2 > 0
=> cy = 0   iff ac^2-cx^2 = 0
=> no solution    else
There are either two points which both have the desired distances. But based on ac^2-cx^2 there may also be only one solution or none at all.

        (let*
            (


    (cond
     (
      )
     (
      )
     (t nil))

    (values    )
    ;;end let, calc-xypoint-from-2-pts&dists
    ))
|#
;;TEST
;;(calc-xypoint-from-2-pts&dists x2 y2 x3 y3 xy1-xy2 xy1-xy3)
;; ============= END DELETE ============================

;;CALC-TRIANGLE-VALS
;;2018
;;ddd
(defun calc-triangle-vals (adjacent opposite &optional  hypot
                                         &key  tan  sin  cos (return-angles-p T) (dec 3)) 
  "U-math. VERY FLEXIBLE: Put in any 2 values and returns all the rest (for RIGHT TRIANGLES ONLY) RETURNS (values hypot adjacent opposite sin cos tan angle opp-angle radians opp-radians) If RETURN-ANGLES-P RETURNS, otherwise RETURNS (values hypot adjacent opposite sin cos tan ) ,  
  INPUT:  Use NIL for missing data. NEEDS ONLY at least LENGTH of one side and 1 of rest of input vars. dec= num of dec places to round to."
  (let
      ((angle)
       (radians) 
       (opp-angle)
       (sin-opp) 
       (opp-radians)
       (angle-radians)
       )
    ;;Hypot must be >= either other side for right triangle.
    (unless (or (and adjacent hypot (> adjacent hypot))
            (and opposite hypot (> opposite hypot)))
        
    (cond
     ;;given hypot
     (hypot 
      (cond 
       (opposite
        (setf sin (realpart (/ opposite hypot)))
        (setf angle-radians (asin sin)
              angle (calc-radians-to-angle angle-radians)
              cos (my-cos angle)
              tan (my-tan angle)
              adjacent (/ opposite tan))) ;;tan= opp/adj
       (adjacent
        (setf cos (realpart (/ adjacent hypot)))
        (setf angle-radians (acos cos)
              angle (calc-radians-to-angle angle-radians)
              sin (my-sin angle)
              tan (my-tan angle)
              opposite (* adjacent tan)))
       (sin  ;;sin = opp/hyp
             (setf opposite (realpart  (* sin hypot)))
             (setf angle-radians (asin sin)
              angle (calc-angle-to-radians angle-radians)
              cos (my-cos angle)
              tan (my-tan angle)
              adjacent (/ opposite tan)))
       (cos
        (setf adjacent (* cos hypot))
        (setf angle-radians (acos cos)
              angle (calc-radians-to-angle angle-radians)
              sin (my-sin angle)
              tan (my-tan angle)
              opposite (* adjacent tan)))
       (tan
        (setf angle-radians (atan tan)
              angle (calc-radians-to-angle angle-radians)
              sin (my-sin angle)
              cos (my-cos angle)
              adjacent (* cos hypot)
              opposite (* sin hypot)))
       (t nil)))
     ;;given opposite
     (opposite 
      (cond
       (adjacent
        (setf tan (/ opposite adjacent)))
       (tan  ;;tan = opp/adj
             (setf adjacent (/ opposite tan)))
       (sin  ;;sin = opp/hyp
             (setf hypot (/ opposite sin)))
       (cos
        (setf angle-radians (acos cos)
              angle (calc-radians-to-angle angle-radians)
              sin (my-sin angle)
              tan (my-tan angle)
              adjacent (/ opposite tan)
              hypot (/ opposite sin)))))
     ;;given adjacent
     (adjacent
      (cond
       (cos   
        (setf hypot (/ adjacent  cos )))
       (tan
        (setf opposite (* tan adjacent)))
       (sin
        (setf angle-radians (asin sin)
              angle (calc-radians-to-angle angle-radians)
              cos (my-cos angle)
              tan (my-tan angle)
              opposite (* adjacent tan) ;;tan= opp/adj
              hypot (/ opposite sin)))))  ;;sin=opp/hyp
     (t nil))

    ;;FILL IN MISSING VALUES
    (when (null hypot)
      (cond
       ((and adjacent opposite)
        (setf hypot (sqrt (+ (expt opposite 2.0)(expt adjacent 2.0)))))
       ((and tan opposite)
        (setf adjacent (/ opposite tan)))
       ((and tan adjacent)  ;;tan = opp/adj, adj = opp/tan
        (setf opposite (* adjacent tan)))))
    (when (null adjacent)
      (cond
       ((and hypot opposite)
        (setf adjacent (sqrt (- (expt hypot 2.0)(expt opposite 2.0)))))
       ((and opposite tan)  ;;cos = adj/hyp
        (setf adjacent (/ opposite tan)))
       ))
    (when (null opposite)
      (cond
       ((and hypot adjacent)
        (setf opposite (sqrt (- (expt hypot 2.0)(expt adjacent 2.0)))))
       ((and adjacent tan) ;;sin = opp/hyp  ;;tan = opp/adj
        (setf opposite (* tan adjacent)))       
       ))
    (when (null tan)
      (setf tan (/ opposite adjacent)))
    (when (null sin)
      (setf sin (/ opposite hypot)))
    (when (null cos)
      (setf cos (/ adjacent hypot)))  
          
    ;;RETURN ALL ANGLES? 
    (cond
     (return-angles-p
      (setf radians (asin sin)
            angle (calc-radians-to-angle radians)
            opp-angle (- 90 angle)
            sin-opp (my-sin opp-angle)
            opp-radians (asin sin-opp))  
      (values (round-real hypot :dec dec)(round-real adjacent :dec dec) 
              (round-real opposite :dec dec) (round-real sin :dec dec)  (round-real cos :dec dec) 
              (round-real tan  :dec dec) (round-real angle :dec dec)(round-real opp-angle :dec dec)  
              (round-real radians :dec dec)  (round-real opp-radians :dec dec)))
     (t 
      (values (round-real hypot :dec dec)(round-real adjacent :dec dec)
              (round-real opposite :dec dec) (round-real sin :dec dec) 
              (round-real cos :dec dec)  (round-real tan  :dec dec))))
     ;;end unless hypot too small
      )
    ;;end let, calc-triangle-vals
    ))
;;TEST
;; (values hypot adjacent opposite sin cos tan angle opp-angle radians opp-radians))
;; ;; (calc-triangle-vals 100 nil 112)
;; 112.0   100.0  50.438  0.45  0.893  0.504  26.766  63.234  0.467 1.104
;;given adj, opp
;; (calc-triangle-vals 50 20) = 53.85 50 20 0.3713 0.9284 2/5 angle=21.80  68.198 0.380 1.190
;;given opp,hyp
;; NO-hypot can't be shorter than another side.
;; (calc-triangle-vals nil 50 20) = NIL
;; (calc-triangle-vals 50 nil 20) = NIL
;;given adj, sin
;; (calc-triangle-vals 50 nil nil :sin .40) 
;; = 54.554  50.0  21.822  0.4  0.917  0.436  23.578  66.422   0.412  1.159
;;given hyp, tan
;; (calc-triangle-vals nil nil 100 :tan 1.0) 
;; = 100.0  70.711  70.711  0.707  0.707  1.0  45.0  45.0  0.785  0.785
;; (calc-triangle-vals  20 NIL  50)
;; = 50.0  20.0  45.82 0.917 0.4  2.291  66.491  23.509  1.16  0.41




;;FOR CALC-XY-TRANSFORMS-FOR-TRIANGLE
#|

                                                  xy2a
                                           
                                       angle0    xy3a
                                     trans-angle 
       (x0+xa0)--  xy1a - - - - -   x4a x5a - - - - -parallel to xy1-xy3
                                         xy2              |
                    angle0                            (y0 + ya0)  
          xy1-------------------xy3             |
|#

;;CALC-XY-TRANSFORMS-FOR-TRIANGLE
;;2018
;;ddd
(defun calc-xy-transforms-for-triangle (x1 y1 x2 y2 x3 y3 trans-angle 
                                           &key xy1-xy2 angle0 sin0 cos0 tan0   (xa0 0) (ya0 0) (dec 3))
  ;;width ht (move-origin-to-xy1-p T))
  "U-math For right triangles only. RETURNS (x3 y   INPUT: xy1=betw adj+hypot,xy2=end hypot,xy3=at90deg.  Given at least 1 xy-point and EITHER a second point, a sin,cos,or tan OR a 3rd point OR an angle in degrees or radians AND A TRANSFORM ANGLE= trans-angle, calcs new triangle-vals in ORIGINAL xy-coord system. ALSO, calculates movement of origin from origin0 to (x + xtrans) and (y + ytrans) Must INPUT xa0,ya0-the new ORIGIN (in first origin coords). These are included in the returned values.
   XY1-XY2 = RADIUS = HYPOT most of time. XY2-XY3=MIN-RADIUS if 1/2 of line connecting 2 pts in an arc.  DEC is the number of decimals to round to. If nil, no round.
 [OLD: Given pts xy1 and xy2, returns x1a y1a x2a y2a on a new xa ya coordinates rotated angle trans-angle.  xy1-xy2 is the hypot, xy1-xy3 is the side-adj, and xy2-xy3 is the side opposite. The original angle0 of a line xy1-xy2 to the xy-coords was newaxis. The origin of the new coordinates is xa = (+ x0  width) and ya =  (+ y0 ht)] Note: x0-x0a=x0a, y0-y0a=y0a.
       (y0-y0a  y0a).  "
  (let*
      ((x1a0)
       (y1a0)
       (x2a0)
       (y2a0)
       (x3a0)
       (y3a0)
       (x1a)
       (y1a)
       (x2a)
       (y2a)
       (x3a)
       (y3a)
       (x4a)
       (x5a)
       ;;pts at right angle in triangle on original x-axis--with xy1-xy2 as hypotenuse
       (xy1-xy3)
       (xy2-xy3sq)   
       (xy2-xy3)
       (xy1-xy5)
       (xy3-xy5)
       (sin-trans)
       (cos-trans)
       (sin-com)
       (cos-com)
       ;;(trans-angle-rads)      ;;
       (opp-angle)
       (opp-angle-rads)
       (combined-angle)
       (combined-angle-rads)
       (xy1a-xy2a)
       (xy1a-xy3a)
       (xy2a-xy3a)
       (xy1a-xy5a)
       ;;for calc-center-from-pts return--not needed?
       (x4)
       (y4)
      (xx1)
       (yy1) 
       (xy1-xy2sq)
       (xy1xy2sq)
       (angle0+trans-tan)
       (angle0+trans-rads)
       (opp-rads0)
       (angle0-rads) 
       (xy1-xy3sq)
       (angle0+trans)
       (opp-angle0)
       (radians0)
       (xy2-xy4)
       (x4a-y2a)
       (x1a-x5a)
       (x4a-y2a)
       (xx1)
       (yy1)
       (xy1a-xy5a)
       )
    (when (and x1 x2)
      (setf xy1-xy2  (calc-dist-betw-2-pts x1 y1 x2 y2)))
    (when sin0
      (setf angle0-rads (asin sin0)
            angle0 (calc-radians-to-angle angle0-rads)))
    (when cos0
      (setf angle0-rads (acos cos0)
            angle0 (calc-radians-to-angle angle0-rads)))
    (when tan0
      (setf angle0-rads (atan tan0)
            angle0 (calc-radians-to-angle angle0-rads)))
   
    ;;FIND ALL MAIN TRIANGLE SIDE LENGTHS--
     ;;  given 1 or 2 sides OR 1 side and angle0
    (cond
     ((and x1 x2)
      (setf xy1-xy2 (calc-dist-betw-2-pts x1 y1 x2 y2))
      (cond
       (x3
        (setf xy1-xy3 (calc-dist-betw-2-pts x1 y1 x3 y3)
              xy2-xy3sq (- (expt xy1-xy2 2)(expt xy1-xy3 2))
              xy2-xy3 (sqrt xy2-xy3sq)))
       (angle0
        (setf sin0 (my-sin angle0)
              xy2-xy3 (* sin0 xy1-xy2)
              xy1-xy3sq (- (expt xy1-xy2 2)(expt xy2-xy3 2))
              xy1-xy3 (sqrt xy1-xy3sq)))
       ((and x1 x3)
        (setf xy1-xy3 (calc-dist-betw-2-pts x1 y1 x3 y3))
        (cond
         (angle0
          (setf cos0 (my-cos angle0)
                xy1-xy2 (/ xy1-xy3 cos0)
                xy1-xy2sq (- (expt xy1-xy2 2)(expt xy1-xy3 2))
                xy1-xy2 (sqrt xy1xy2sq)))
         (t nil))
        ))
      ;;end and x1 x2
      )
     ((and x2 x3)
      (cond
       ((or xy1-xy2 angle0)  ;;xy1-xy2 = RADIUS most of time
        (cond
         (xy1-xy2
          (setf xy2-xy3 (calc-dist-betw-2-pts x2 y2 x3 y3)
                sin0 (/ xy2-xy3 xy1-xy2)
                angle0-rads (asin sin0)
                angle0 (calc-radians-to-angle angle0-rads)
                cos0 (my-cos angle0)
                xy1-xy3 (* cos0 xy1-xy2))      
         ;;(break "xy1-xy3")
          ;;find trans-angle, angle0+trans
          #|        (cond
         (trans-angle
          (setf angle0+trans (+ angle0 trans-angle)))
          (t
          (setf angle0+trans-tan (/ y2 x2)
                angle0+trans-rads (atan angle0+trans-tan)
                angle0+trans (calc-radians-to-angle angle0+trans-rads)
                trans-angle (- angle0+trans angle0))))
        ;;find sides of triangle parallel to x-axis
        (setf sin-trans (sin trans-angle)
              cos-trans (my-cos trans-angle)
              xy3-xy5 (* sin-trans xy1-xy3)
              xy1-xy5 (* cos-trans xy1-xy3)
              ;;CALC x1,y1 (often = circle center)
              y1 (- y3 xy3-xy5)
              x1 (- x3 xy1-xy5))|#
          ;;end xy1-xy2
          )
         (angle0
          (setf xy2-xy3 (calc-dist-betw-2-pts x2 y2 x3 y3)
                xy2-xy3 (* 0.5 xy2-xy3)
                sin0 (my-sin angle0)
                xy1-xy2 (/ xy2-xy3 sin0)
                cos0 (my-cos angle0)
                xy1-xy3 (* cos0 xy2-xy3)) 
          ;;end angle0
          )
         (t nil))

        ;;FIND TRANS-ANGLE, ANGLE0+TRANS
        (cond
         (trans-angle
          (setf angle0+trans (+ angle0 trans-angle)))
         (t
          (setf angle0+trans-tan (/ y2 x2)
                angle0+trans-rads (atan angle0+trans-tan)
                angle0+trans (calc-radians-to-angle angle0+trans-rads)
                trans-angle (- angle0+trans angle0))))

              ;;NO-DO FOR ALL LATER CALC x1,y1 (often = circle center)
#|              y1 (- y3 xy3-xy5)
              x1 (- x3 xy1-xy5))|#
        ;;(BREAK "all side lengths?")
        ;;end or xy1-xy2 angle0
        )
       (t nil))
      ;;end ((and x2 x3)
      )
     (t nil))

    ;;FIND ALL THE TRIANGLE VALUES USING LENGTHS
    ;;FOR BASE TRIANGLE 
    (when (and xy1-xy3 xy1-xy2)
      (multiple-value-setq (xy1-xy2 xy1-xy3 xy2-xy3 sin0 cos0 tan0 
                                    angle0 opp-angle0 radians0 opp-rads0)
          (calc-triangle-vals  xy1-xy3 NIL  xy1-xy2))

      ;;(break "after mvs-angle0")
      
      ;;CALC STILL MISSING VALUES
      ;;find original xy1 often= CENTER?
      (unless (and x1 y1)
        (setf  xy2-xy4 (* 2.0 xy2-xy3) 
                 ;; midx = (0.5 (+ x1 x2))  x2 = 2.0 * midx - x1  OR  x1 - 2.0 * midx [works better]?
               ;;xy4 is imaginary pt at end of 2x line xy2-xy3 so can use calc-center-from-pts
               x4  (- x2 (* 2.0 x3))   
               y4  (- y2 (* 2.0 y3)))          
               ;; xx4  (- x2 (* 2.0 x3))   yy4  (- y2 (* 2.0 y3))       
        (multiple-value-setq (x1 y1) 
            ;;rest not needed? xx1 yy1 radius  pts-dist  pts-line-direction  x3 y3 
            ;; centerline-dir xy1-xy3)
            (calc-center-from-pts x2 y2 x4 y4 :radius xy1-xy2 :dec 3))
        ;;end unless x1,y1
        )
      ;;here2
      ;;CALC THE ROTATED-DISPLACED TRIANGLE VALS
      ;;translated points WITHOUT ROTATION
      (setf x1a0 (+ x1 xa0)
            y1a0 (+ y1 ya0)
            x2a0 (+ x2 xa0)
            y2a0 (+ y2 ya0)
            x3a0 (+ x3 xa0)
            y3a0 (+ y3 ya0))

      ;;rotate the new points--rotation is alpha even tho xy1 is not on the origin!
      ;; so x1a still = (+ x1 xa0) and y1a0 (+ y1 ya0) AFTER ROTATION.
      ;;calc rest from triangles drawn to either original x or original y axis 
      ;;    from rotated points.  sin= opp/hyp, cos=adj/hyp, tan=opp/adj
      ;; 1-find sin= opp/hyp, cos=adj/hyp, tan=opp/adj for NEW ANGLES
      (setf combined-angle (+ angle0 trans-angle))

      ;;FIND SIDE LENGTHS
      ;;hypot same as pre-rotated,but other sides change
      (setf sin-com (my-sin combined-angle)
            cos-com (my-cos combined-angle)
            xy1a-xy2a  xy1-xy2
            xy1a-xy3a (round-real (* cos-com xy1a-xy2a) :dec dec)
            xy2a-xy3a (round-real  (* sin-com xy1a-xy2a)  :dec dec))
      (break)
      ;;FIND THE NEW POINTS
      ;;Only pts xy2a and xy3a change from above translated values
      ;;for xy1a
      (setf x1a (round-real x1a0 :dec dec)
            y1a   (round-real y1a0 :dec dec)
            ;;for xy2a
            x2a xy1a-xy3a ;; (+ x2a0 xy1a-xy3a)
            y2a xy2a-xy3a ;;(+ y2a0 xy2a-xy3a)
            sin-trans (round-real  (my-sin trans-angle)  :dec dec)
            cos-trans (round-real  (my-cos trans-angle)  :dec dec)
            x4a-y2a (round-real  (* sin-trans  xy1-xy3)  :dec dec)
           ;adj side of original triangle projected on axis 
            x1a-x5a (round-real  (* cos-trans xy1-xy3)  :dec dec)       
            ;;for xy3a
            x3a x1a-x5a  ;;do I need to add the xa0 and ya0 to these??
            y3a x4a-y2a)  
      (values  x1a y1a x2a y2a x3a y3a  xy1a-xy2a  xy1a-xy3a xy2a-xy3a angle0 trans-angle)
    ;;x1 y1 x2 y2 x3 y3 trans-angle &key angle0 sin0 cos0 tan0  xa0 ya0)
    ;;end when, let, calc-xy-transforms-for-triangle
    )))
;;TEST
;; x1a y1a x2a y2a x3a y3a  xy1a-xy2a  xy1a-xy3a xy2a-xy3a  angle0 trans-angle)
;; (calc-xy-transforms-for-triangle 0  0  100  50 100 0 30)  
;; SSSS These finally seem about right--CHECK THEM MORE CLOSELY 
;;original=  0  0   100        50       100      0 
;;                0  0  61.602  93.301  86.6  50.0  111.803  61.602  93.301  26.565  30
;; (calc-xy-transforms-for-triangle nil nil  100  50 100 0 30 :xy1-xy2 111.8)  
;;  =    0.0  0.008  61.49  93.353  86.556  49.974  111.8  61.49  93.353  26.615  30
;; note: last 2 results should be same--only one of initial points missing--rest inputs same.



;;MY-SIN
;;2018
;;ddd
(defun my-sin (angle &key (dec 3))
  "U-math   RETURNS realpart (values cos rads) INPUT: angle in degrees. rounded to dec, unless dec=NIL."
  (let*
      ((angle1 (realpart angle))
       (rads (calc-angle-to-radians angle1))
       (sin)
       )
    (cond
     ((member angle '(0 180 360 ))
      (setf sin 0.0001))
     (t 
      (setf sin (round-real (sin rads) :dec dec))))
    (values sin rads)
    ;;end let, my-sin
    ))
;;TEST
;; (my-sin 0) = 1.0E-4  0.0D0
;; (my-sin 30) =0.5  0.5235987755982988D0
;; (my-sin 45) = 0.707   0.7853981633974483D0
;; (my-sin 90) = 1.0D0 1.5707963267948966D0




;;MY-COS
;;2018
;;ddd
(defun my-cos (angle &key (dec 3))
"U-math   RETURNS realpart (values cos rads) INPUT: angle in degrees. rounded to dec, unless dec=NIL."
    (let*
      ((angle1 (realpart angle))
       (rads (calc-angle-to-radians angle1))
       (cos (round-real (cos rads) :dec dec))
       )
    (values cos rads)
    ;;end let, defun
    ))
;;TEST
;; (my-cos 0) = 1.0 0.0D0
;; (my-cos 30) =  0.866 0.5235987755982988D0
;; (my-cos 45) = 0.707  0.7853981633974483D0
;; (my-cos 90) = 0.0  1.5707963267948966D0



;;MY-TAN
;;2018
;;ddd
(defun my-tan (angle &key (dec 3))
  "U-math   RETURNS realpart (values tan rads) INPUT: angle in degrees. rounded to dec, unless dec=NIL."
  (let*
      ((angle1 (realpart angle))
       (rads (calc-angle-to-radians angle1))
       (tan (round-real (tan rads) :dec dec))
       )
    (values tan rads)
    ;;end let, defun
    ))
;;TEST
;;  (my-tan 30) =   0.5773502691896257D0  0.5235987755982988D0
;; (my-tan 45) =0.9999999999999999D0  0.7853981633974483D0




;;MAKE-TRANSFORM-LIST
;;2018
;;ddd
(defun make-transform-list (U V  &key (P 1.0)(R 1.0)(Q 1.0)(S 1.0)  )
  "In   RETURNS    INPUT: Where x'=x+U, y'= y+Vfor TRANSLATION, and  Where x'=Px+Ry and y'=Qx+Sy for SCALING/ROTATION."
  (let*
      ((x)
       )
  (list P Q R S U V)
  ))
;;TEST
;; (make-transform-list  2.0 3.0)


;;XXX ============== TRANSFORMATION TESTS ===================
;; TRANSFORMATIONS
;; SCALING & ROTATION: a’ = Pa + Rb,  b’ = Qa + Sb.
;;TEST-TRANSFORM-XY
;;2018
;;ddd
(defun test-transform-xy ( angle &optional new-x0 new-y0 
                                 &key  (x1 0)(y1 0) (x2 300) (y2 0) invert-y-p
                                 (x-multiplier 1.0)(y-multiplier 1.0)
                                 (P 1.0)(Q 0) (u 0)(v 400) (dec 3))
  "In U-math. For transform eqs:   x’ = Px + Ry + new-x0, y’ = Qx + Sy + new-y0"
  (let*
      ((R 0)
       (S)
       (inst (make-instance 'drawtest-interface-NO-CALLBACK)) 
       ;; in U-function-plotter
       (trans-scale)
       (get-trans-list)
       (transform)
       (rads)
       (tan-rads)
       (p)
       )
    (when invert-y-p
      (setf y1 (- y1)
            y2 (- y2)))

    ;;CALC THE TRANSFORM
    (multiple-value-setq (transform Q R rads tan-rads)
        (calc-xy-coords-transform angle  new-x0 new-y0 
                           :x-multiplier x-multiplier :y-multiplier y-multiplier  
                           :q q :r r :u u  :v v :dec dec))
    ;;display
    (capi:display inst)
    ;;(break)
    (with-slots (output-pane-1) inst   
      (gp:with-graphics-state (output-pane-1   :transform transform) 
        (setf trans-scale (gp:get-transform-scale transform)
              ;;find current transform list --should be same as just set above
              get-trans-list (gp:graphics-state-transform
                              (gp:port-graphics-state output-pane-1)))
        ;;(break)
        ;;to test transform, pute gp:draw-line -
        ;;U,V TRANSLATION NOT WORK
        (gp:draw-line output-pane-1 x1 y1 x2 y2)

        ;;(values transform Q S rads tan-rads)
        (values get-trans-list trans-scale transform rads tan-rads)
        ;;end with gp,with, let, test transform-xy
        ))))
;;TEST
;; (test-transform-xy 45)
;; works, 45deg line from 0,0 
;; SSS START TESTING NEW calc-xy-coords-transform HERE
;; (test-transform-xy 10)
;; (test-transform-xy 30  200 )
;; = (1.0D0 -0.578000009059906D0 0.0D0 1.0D0 200.0D0 400.0D0)  1.0  (1.0 -0.578 0 1.0 200 400)  -0.524  -0.5778853 ; 30deg line
;; ;; (test-transform-xy -30  200 )



;;CALC-XY-TRANSFORM
;;2018
;;ddd
(defun calc-xy-transform (x y &key (angle 0) (x0 0)(y0 0)
                                  (x-multiplier 1.0)(y-multiplier 1.0)
                                 (Q 0)(R 0) (u 0)(x-axis-y 600) (dec 3) 
                                 (return-info T))
  "In U-math. For window with x0,y0 in top,left: with transform eqs:   x’ = Px + Ry + x0, y’ = Qx + Sy + y0. NOTE: NOT for GP transform--use calc-xy-gp-transform. Translation new origin, x0,y0. y1= non-windows value, y1w= windows value (to plot with win-y0 at top)."
  (let*
      ((rads);;  (round-real (calc-angle-to-radians angle) :dec dec))
       (tan-rads);; (tan rads))
       (P)
       (S)
       (V x-axis-y)
       (x1)
       (pre-x x)
       (pre-y)
       (y1)
       (y1w)
       (transform)
       )
      ;;FIND RADIANS
      (cond
       ((or (and (>= angle 0) (<= angle 89.95))(= angle 360))
        (setf rads  (round-real (calc-angle-to-radians angle) :dec dec)))
       ((and (> angle 89.95)(<= angle 90))
        (setf rads (-  (round-real (calc-angle-to-radians angle) :dec dec))))
       ((and (> angle 90)(< angle 180))
        (setf rads (- (round-real (calc-angle-to-radians angle) :dec dec))
              pre-x (- x)))
       ((and (> angle 180)(< angle 270))
        (setf rads (- (round-real (calc-angle-to-radians angle) :dec dec))
                                          pre-x (- x)))
       ((or (and (>=  angle 270)(< angle 360))
            (and (>=  angle -90)(< angle 0)))             
        (setf rads  (round-real (calc-angle-to-radians angle) :dec dec)))

       ;;(- 90 (- angle 270))) :dec dec)))
       (t nil))
      ;;FIND TAN-RADS
        (setf  tan-rads  (tan rads))
       ;;(break)
      (when x0
        (setf u x0))
      ;;adjusts for pre-move x-axis to bottom 
      ;;(for new coordis relative to normal x-axis)
      (when (not (= y0 0))
        (setf v (- v y0)))
      (setf P x-multiplier
            S y-multiplier)

      ;;Change Q to change rotation angle
      (setf Q (round-real tan-rads :dec dec)
            transform (list p q r s u v))
      ;;FIND THE NEW X AND Y
      (setf x1 (+ (* p pre-x)(* r y) u)
            pre-y (+ (* q  x)(* s y)))
      ;;calc y1 and y1w
      (setf y1w (- v pre-y)      
            y1 (+ pre-y y0))

#|      (cond
       ((or (and (>= angle 0) (<= angle 180))(= angle 360))
                (setf y1w (- v pre-y)))
#|       ((and (> angle 90)(< angle 180))
                (setf y1w (-  v pre-y)))|#
       ((and (> angle 180)(< angle 270))
                (setf y1w  (+ pre-y v)))
       ((or (and (>=  angle 270)(< angle 360))
            (and (>=  angle -90)(< angle 0)))             
                (setf y1w  (+ pre-y v)))
       ;;(- 90 (- angle 270))) :dec dec)))
       (t nil))|#
       
#|      (cond
       ;;pre-y is negative or less than v 
       ((or (< pre-y 0)
            (> v pre-y))
        (setf y1w (+ pre-y v)))
       ;;pre-y is positive
       (t
        (setf y1w (- v pre-y))))   |#   
      ;;return
      (cond
       (return-info
        (values x1 y1w y1 transform Q R rads tan-rads pre-x pre-y))
       (t (values x1 y1w y1)))
    ;;end let, calc-xy-transform
    ))
;;TEST
;; (calc-xy-transform  100 200)
;; works= 100.0 400.0 200.0 (1.0 0.0 0 1.0 0 600) 0.0 0 0.0 0.0 100 200.0
;;FOR ROTATION TRANFORM
;; (calc-xy-transform  100 200 :angle 45)
;; works= 100.0 300.1 299.9 (1.0 0.999 0 1.0 0 600) 0.999 0 0.785 0.99920404 100 299.9
;;  (calc-xy-transform  100 200 :angle 80)
;;works= 100.0 -166.29999 766.3 (1.0 5.663 0 1.0 0 600) 5.663 0 1.396 5.6625605 100 766.3 ;;note: -166 means y is ABOVE top of window.
;; (calc-xy-transform  100 200 :angle 90)
;; works= 100.0  -490630.8 491230.8 (1.0 4910.308 0 1.0 0 600) 4910.308 0 -1.571 4910.3086 100 491230.8 ;; -490630.8 is above top of window
;; (calc-xy-transform  100 200 :angle 135)
;; works= -100.0 300.0 300.0 (1.0 1.0 0 1.0 0 600) 1.0 0 -2.356 1.0003892 -100 300.0
;; (calc-xy-transform  100 200 :angle 315) 
 ;; works= 100.0 500.0 100.0 (1.0 -1.0 0 1.0 0 600) -1.0 0 5.498 -0.99957407 100 100.0  ;;note: y displaced from 200 to 100
;; FOR X, Y TRANSLATIONS
;;no translation
;; (calc-xy-transform  100 200)
;;works= 100.0 400.0 200.0 (1.0 0.0 0 1.0 0 600) 0.0 0 -0.0 -0.0 100 200.0
;; (calc-xy-transform  100 200 :x0 50 :y0 50)
;;works= 150.0  350.0 250.0 (1.0 0.0 0 1.0 50 550) 0.0 0 -0.0 -0.0 100 200.0
;;X & Y MULTIPLIERS
;; ;; (calc-xy-transform  100 200 :X-MULTIPLIER 2.0 :Y-MULTIPLIER 2.0)
;;works= 200.0 200.0 400.0 (2.0 0.0 0 2.0 0 600) 0.0 0 0.0 0.0 100 400.0




;;CALC-XY-COORDS-TRANSFORM
;;2018
;;ddd
(defun calc-xy-gp-transform (angle &optional new-x0 new-y0 
                                 &key (x-multiplier 1.0)(y-multiplier 1.0)
                                 (Q 0)(R 0) (u 0)(v 600) (dec 3) not-allow-90to270-p)
  "In U-math. Calcs GP TRANSFORM LIST (p q r s u v). For transform eqs:   x’ = Px + Ry + new-x0, y’ = Qx + Sy + new-y0. NOTE: GP transform only works for rotations 0 to +/-90deg?"
  (let*
      ((rads);;  (round-real (calc-angle-to-radians angle) :dec dec))
       (tan-rads);; (tan rads))
       (P)
       (S)
       (transform)
       )
    (when (and (null not-allow-90to270-p)
               (or (and  (>= angle -90)(<= angle 90))
                   (and (>= angle 270)(<= angle 360))))
      ;;FIND RADIANS
      (cond
       ((or (and (>= angle 0) (<= angle 90))(= angle 360))
        (setf rads ( - (round-real (calc-angle-to-radians angle) :dec dec))))
       ((and (> angle 90)(< angle 180))
        (setf rads (round-real (calc-angle-to-radians angle) :dec dec)))
       ((and (> angle 180)(< angle 270))
        (setf rads (round-real (calc-angle-to-radians angle) :dec dec)))
       ((and (< angle 0)(> angle -90))
        (setf rads (- (round-real (calc-angle-to-radians angle) :dec dec))))
       ((and (>  angle 270)(< angle 360))
        (setf rads (round-real (calc-angle-to-radians (- 90 (- angle 270))) :dec dec)))
       (t nil))
      ;;FIND TAN-RADS
      (setf tan-rads (tan rads))

      (when new-x0
        (setf u new-x0))
      ;;adjusts for pre-move x-axis to bottom 
      ;;(for new coordis relative to normal x-axis)
      (when new-y0
        (setf v (- v new-y0)))
      (setf P x-multiplier
            S y-multiplier)
      ;;Change Q to change rotation angle
      (setf Q (round-real tan-rads :dec dec)
            transform (list p q r s u v))
      ;;end when
      )
    (values transform Q R rads tan-rads)
    ))
;;TEST
;; (calc-xy-gp-transform 45) = (1.0 -0.999 0 1.0 0 600)   -0.999  0 -0.785 -0.99920404
;; (calc-xy-gp-transform 10)
;; (calc-xy-gp-transform 100) = NIL  0  0  NIL  NIL
;; (calc-xy-gp-transform 90) = (1.0 4910.308 0 1.0 0 600) 4910.308  0 -1.571 4910.3086



;;OLDER VERSION ------------------------------------------------------------------------
(defun test-transform  (p q r s  u v &key (x1 0)(y1 0) (x2 300) (y2 0) invert-y-p)
  (let ((inst (make-instance 'drawtest-interface-NO-CALLBACK)) ;; in U-function-plotter
        (trans-scale)
        (get-trans-list)
        )
    (when invert-y-p
      (setf y1 (- y1)
            y2 (- y2)))
      (capi:display inst)
    (with-slots (output-pane-1) inst   
     (gp:with-graphics-state (output-pane-1   :transform (list p q r s u v )) 
        (setf trans-scale (gp:get-transform-scale (list p q r s u v ))
          ;;find current transform list --should be same as just set above
          get-trans-list (gp:graphics-state-transform (gp:port-graphics-state output-pane-1)))
          ;;to test transform, pute gp:draw-line -
          ;;U,V TRANSLATION NOT WORK
          (gp:draw-line output-pane-1 x1 y1 x2 y2)
          (values get-trans-list trans-scale (list p q r s u v ))
          ))))
;; SSS START HERE--TRY TO FIX TRANSFORMATIONS--LOOK AT RESULTS 
;;  START AT ORIGIN X1=0, Y1=0; MUST USE THIS FOR TESTING
;; ;; a’ = Pa + Rb, b’ = Qa + Sb.   If a&b=1.0  b= Q+S
;; RULES?
;; 1. CLOCKWISE ROTATION = +
;; 2. V=600 moves x-axis to y= 600
;; 3. 1.0 0 0 0 1.0   0 0 is no transformation
;; 4. 
;;TEST
;; no transform (except v=600 (subtracts 600 from Ys?)
;; (test-transform 1.0 0 0 1.0 0 600 :x1 0 :y1 0 :x2 300 :y2 0) ;; :invert-y-p T)
;; change 
;; (test-transform 1.0 -1.0 0 1.0 1.0 600 :x1 0 :y1 0 :x2 300 :y2 0)
;; rotates 45deg only
;; (test-transform 1.0 -0.5 0 1.0 1.0 600 :x1 0 :y1 0 :x2 300 :y2 0)
;; rotates about 30deg only
;; (test-transform 1.0 -9.0 0 1.0 1.0 600 :x1 0 :y1 0 :x2 300 :y2 0)
;; rotates about 80deg?
;; (test-transform 1.0 -999.0 0 1.0 1.0 600 :x1 0 :y1 0 :x2 300 :y2 0)
;; rotates about 90deg only
;; (test-transform 1.0 -1.0 0 1.0 200 400 :x1 0 :y1 0 :x2 300 :y2 0)
;; =45deg line
;; (test-transform 1.0 1.0 0 1.0 200 400 :x1 0 :y1 0 :x2 300 :y2 0)
;; = -45deg = 315deg
;; (test-transform 1.0 -999.0 0 1.0 200 400 :x1 0 :y1 0 :x2 300 :y2 0)
;; = 90deg
;; (test-transform 1.0 999.0 0 1.0 200 400 :x1 0 :y1 0 :x2 300 :y2 0)
;; = -90deg = 270deg



;;CALC-INTERVALN-FOR-NUM
;;2019
;;ddd
(defun calc-intervaln-for-num (num  &key num-intervals min max intervals-list
                                    (return-decimals 3))
  "U-math.  RETURNS (values value  return-int-n len-list interval-width)
   INPUT: interval-list item= (return-value min max) ; num can be string or num.
   Note: normally the return-value is the nth item in list. Must be less than interval max to be in interval (except last interval)."
  (when (stringp num)
    (setf num (convert-string-to-float num)))
    (let
        ((len-list) 
         (value)
         (return-int-n)
         (num-intervals1)
         (interval-width)
         )
    (cond
     ((null intervals-list)
      (multiple-value-setq (intervals-list num-intervals1 interval-width)
            (calc-num-intervals-list num-intervals min max
                                                   :return-decimals return-decimals)))
     (t (setf interval-width (- (third (car intervals-list))(second (car intervals-list))))))
    (setf len-list (list-length intervals-list))
      ;;find the interval num is in
      (loop
       for int in intervals-list
       for n from 1 to len-list
       do
       (let*
           ((intvalue (first int))
            (intmin (second int))
            (intmax (third int))                    
            )
         (when (or 
                (and (>= num intmin)(< num intmax))
                (and (= n len-list)(<= num intmax)))
           (setf value intvalue
                 return-int-n n)
           (return))         
         ;;end let,loop
         ))
      (values value  return-int-n len-list interval-width)
      ;;end let, calc-intervaln-for-num
      ))
;;TEST
;; (calc-intervaln-for-num 5.5 :num-intervals 10 :min 0 :max 10)
;; works= 6   6   10
;; (calc-intervaln-for-num 3.2 :interval-list '((0 1 2)(0.5 2 3)(0.7 3 4)(0.9 4 5)))
;; works= 0.7     3     4




;;CALC-NUM-INTERVALS-LIST
;;2019
;;ddd
(defun calc-num-intervals-list (num-intervals bottomn topn 
                                              &key (return-decimals 3))
  "U-math. RETURNS: (values interval-lists  num-intervals interval-width) eg  3 0 3= ((1 0 1)(2 0 2)(3 0 3))  3  List is from min to max. Normally set bottomn= 0."
  (let*
      ((interval-lists)
       (interval-width (/ (- topn bottomn) num-intervals))
       (last-topn bottomn)
       )
    (loop
     for n from 1 to  num-intervals
     do
     (let
         ((new-topn (* n interval-width))
          )
       (when return-decimals
         (setf new-topn (round-real new-topn :dec return-decimals)))

       (cond
        ((< n num-intervals)
         (setf interval-lists (append interval-lists (list (list n last-topn new-topn)))
               last-topn new-topn))
        (t (setf interval-lists (append interval-lists (list (list n last-topn topn))))))
       ;;end let,loop
       ))
    (when return-decimals
      (setf interval-width (round-real interval-width :dec return-decimals)))
    (values interval-lists  num-intervals interval-width)
    ;;end let, calc-num-intervals-list
    ))
;;TEST
;;(calc-num-intervals-list 10 0 10)
;; works= ((1 0 1) (2 1 2) (3 2 3) (4 3 4) (5 4 5) (6 5 6) (7 6 7) (8 7 8) (9 8 9) (10 9 10))    10   1
;;  (calc-num-intervals-list 10 0 9)
;;works= ((1 0 0.9) (2 0.9 1.8) (3 1.8 2.7) (4 2.7 3.6) (5 3.6 4.5) (6 4.5 5.4) (7 5.4 6.3) (8 6.3 7.2) (9 7.2 8.1) (10 8.1 9))    10   0.9



;;INCF-LIST-NTH
;;2019
;;ddd
(defun incf-list-nths (nths numlist  &key  (delta-n 1) decf-p  )
  "U-math Increments nums by delta-n in list matching items on list NTHS.   RETURNS    INPUT: nths list item. When DECF-P, decreases y DELTA-N instead of incf.
   NTHS can be a single n, n begins w 0."
  ;;nths can be a single n
  (unless (listp nths)
    (setf nths (list nths)))
  (let
      ((new-numlist)
       )
    (loop
     for old-n in numlist
     for n from 0 to 1000
     do   
     (let*
         ((new-n)
          )
       (cond
        ;;numberp and member NTHS, modify it
        ((and (numberp old-n)
              (member n nths :test '=))
         (cond
          (decf-p
           (setf new-n (- old-n delta-n)))
          (t (setf new-n (+ old-n delta-n))))
         (setf new-numlist (append new-numlist (list new-n)))
         )
        ;;otherwise, just append new-list
        (t (setf new-numlist (append new-numlist (list old-n)))))
       ;;end let,loop
       ))
    new-numlist
    ;;end let, incf-list-nths
    ))
;;TEST
;; (incf-list-nths 3 '(1 2 3 4 5))
;; works=  (1 2 3 5 5)  
;; (incf-list-nths 3 '(1 2 3 4 5) :delta-n 2 :decf-p T)
;; works= (1 2 3 2 5)  
;; (incf-list-nths '( 2 3) '(1 2 3 4 5) :delta-n 2 :decf-p T)
;; works= (1 2 1 2 5)




#| not accurate??
(defun calc-QS-transforms (angle &key (P 1.0) (R 0) (u 0)(v 0) (dec 3))
  (let*
      ((rads (round-real (calc-angle-to-radians 45) :dec dec))
       (tan-rads (tan rads))
       (ratio-qs tan-rads)
       (q (round-real(/ 0.5 ratio-qs) :dec dec))
       (S Q)
       (transform (list p q r s u v))
       )
    ;;  q = s
    ;;     q/s (q + s) = 1.0
#|       q/s*q + q/s*s = 1.0  ;;q/s is a constant= tan angle
       q = (1.0 - q/s*s) / q/s  ;; q = s
       q =  1.0/q/s - q
       2q = 1.0/q/s
       q = (/ 0.5 q/s)|#
    (values transform Q S rads tan-rads)
    ))|#









;;=========== FOLLOWING not start at origin, but ROTATION is FROM ORIGIN==
;; NO TRANSFORM 
;; (test-transform 1.0 0 0 1.0 0 0) = (1 0 0 1 0 0)  1.0D0  (1.0 0 0 1.0 0 0) 

;; TESTING TRANSLATION
;; MODIFY U -- MUST BE AN INTEGER or doesn't work!!
;; (test-transform 1.0 0 0 1.0 500 0) = (1.0D0 0.0D0 0.0D0 1.0D0 500.0D0 0.0D0)  1.0 (1.0 0 0 1.0 500 0)
;; moves far to right
;; MODIFY V  -- MUST BE AN INTEGER or doesn't work!!
;; (test-transform 1.0 0 0 1.0 0 500) = (1.0D0 0.0D0 0.0D0 1.0D0 0.0D0 500.0D0) 1.0 (1.0 0 0 1.0 0 500)
;; moves far to bottom
;;TESTING SCALING & ROTATION a’ = Pa + Rb, b’ = Qa + Sb.
;; modify p
;; (test-transform 3.0 0 0 1.0 0 0) = (3.0D0 0.0D0 0.0D0 1.0D0 0.0D0 0.0D0) 1.0 (3.0 0 0 1.0 0 0)
;; above line longer, moved to right
;; modify q
;; (test-transform 1.0 2.0 0 1.0 0 0) = (1.0D0 2.0D0 0.0D0 1.0D0 0.0D0 0.0D0) 1.0 (1.0 2.0 0 1.0 0 0)
;; above line moved down, and rotated clockwise

;; TEST Y-TRANSLATION AND REVERSE Y NUMS FOR BOTTOM X-AXIS
;; a’ = Pa + Rb, b’ = Qa + Sb.   If a&b=1.0  b= Q+S
;; (test-transform 1.0 0 0 1.0 0 600 :x1 150 :y1 300 :x2 350 :y2 300 :invert-y-p T)
;;;; ROTATION
;; (test-transform 1.0 0.5 0 1.0 0 600 :x1 150 :y1 300 :x2 350 :y2 300 :invert-y-p T)
;; = (1.0D0 2.0D0 0.0D0 1.0D0 0.0D0 600.0D0) 1.0 (1.0 2.0 0 1.0 0 600)
;; = rotates @-40deg, lowers
;; (test-transform 1.0 -0.5 0 1.0 0 600 :x1 150 :y1 300 :x2 350 :y2 300 :invert-y-p T)
;; = (1.0D0 -0.5D0 0.0D0 1.0D0 0.0D0 600.0D0)  1.0  (1.0 -0.5 0 1.0 0 600)
;; = rotates @40deg, raises
;; (test-transform 1.0 0 0 0.5  0 600 :x1 150 :y1 300 :x2 350 :y2 300 :invert-y-p T)
;; lowers the line
;; (test-transform 1.0 0 0 1.5  0 600 :x1 150 :y1 300 :x2 350 :y2 300 :invert-y-p T)
;; raises the line
;; (test-transform 1.0 0 -0.5 1.0  0 600 :x1 150 :y1 300 :x2 350 :y2 300 :invert-y-p T)
;; moves to right
;; (test-transform 1.0 1.0 0 1.0  0 600 :x1 150 :y1 300 :x2 350 :y2 300 :invert-y-p T)
;; -45deg, moves down
;; (test-transform 1.0 -1.0 0 -1.0  0 600 :x1 150 :y1 300 :x2 350 :y2 300 :invert-y-p T)
;;  45deg, moves down
;; NO INVERT Y
;; (test-transform 1.0 -1.0 0 -1.0  0 600 :x1 150 :y1 300 :x2 350 :y2 300) 
;; 45deg, moves up
;; (test-transform 1.0 (- (* 0.25 pi)) 0 (- (* 0.25 pi))  0 600 :x1 150 :y1 300 :x2 350 :y2 300) ;; 45deg, raises
;; (test-transform 1.0 -0.66 0 -0.66  0 600 :x1 150 :y1 300 :x2 350 :y2 300) 
;; 40?deg, about same start


;;xxx =============== HELP ===================================
#|
TRANSLATION & TRANSFORMATION OF X,Y AXES

1. PURE TRANSLATION (moving origin to new point, but NO ROTATION, SCALING, or other changes)
    Transformation of Coordinates Involving Pure Translation
	
x = x' + x0
y = y' + y0
    or   		
x' = x - x0
y' = y - y0
where (x, y) are old coordinates [i.e. coordinates relative to xy system], (x',y') are new coordinates [relative to x'y' system] and (x0, y0) are the coordinates of the new origin 0' relative to the old xy coordinate system.
 In LISP, it is done in GP by :transform '(  )

2. TRANSFORMATION (SCALING, ROTATION, or other eg. non-linear changes)
  (See pdf file in H-HELP folder for good explaination)

13.6.1 GENERALIZED POINTS
An (x, y) coordinate pair can be transformed to another coordinate system by
SCALING, ROTATION and translation. 
The first two can be implemented using 2 x 2
matrices to hold the coefficients:
If the point P is (a, b) and it is transformed to the point Q (a’, b’)
P => Q or (a, b) => (a’, b’)
a’ = Pa + Rb, b’ = Qa + Sb.

TRANSLATION can be included in this if the points P and Q are regarded as 3-vectors instead of 2-vectors, with the 3rd element being unity:

The COEFFICIENTS U AND V SPECIFY THE TRANSLATION.
ME: For translation for origin x0,y0 to x'0, y'0; 
        where x'0 = x0 + U and y'0 = y'0 + V
           x' = (x + U) and y' =  (y + V)
So, THE SIX ELEMENTS (P, Q, R, S, U, AND V) of the 3 x 3 matrix contain all the transformation information.
 These ELEMENTS ARE STORED IN A LIST (OF TYPE TRANSFORM)
in the graphics-state slot transform.
 ME-EG: 
Transforms can be combined by matrix multiplication to effect successions of
translation, scaling and rotation operations.

FUNCTIONS are provided in Graphics Ports which apply translation, scaling and rotation to a transform, combine transforms by pre- or post-multiplication, invert a transform, perform some operations while ignoring an established
transform, and so on. The macros with-graphics-rotation, with-graphics-scale and with-graphics-translation pre-multiply a supplied transform while a body of code is executed




GENERAL EQUATION FOR CIRCLES, PARABULAS, ELLIPSES, HYPERBOLES, ETC
(from Thomas p490) 
 ==>  Ax^2 + Bxy + Cy^2 + Dx + Ey + F = 0 (a second degree equation)
SPECIAL CASES:
1. CIRCLE: A = C (= 1?); B = 0; D = -2h; E = -2h;  F = h^2 + k^2 - r^2==> 
 ==> (x - h)^2 + (y - k)^2 = r^2
2. PARABOLA:  A = 1, E = -4p,  B = C= D = F= 0 
 ==> x^2 = 4py [p= shortest dist from focus to curve].
3. ELLIPSE: 
 ==> x^2/a^2 + y^2/b^2 = 1;  Where center of ellipse goes thru origin (0,0); a & -a are tangent to top & bottom of ellipse; and b & -b are tangent to the right and left ends.



COMPLETING THE SQUARE: CIRCLE EQUATIONS
Equation of a Circle on MathHelp.com 
;;from http://www.purplemath.com/modules/sqrcircle.htm
 
EQUATION OF A CIRCLE
The technique of completing the square is used to turn a quadratic into the sum of a squared binomial and a number: (x – a)2 + b. 
The CENTER-RADIUS FORM OF THE CIRCLE EQUATION is in the format (x – h)2 + (y – k)2 = r2, with the CENTER being at the POINT (H, K) and the radius being "r". This FORM OF THE EQUATION IS HELPFUL, SINCE YOU CAN EASILY FIND THE CENTER AND THE RADIUS.

But CIRCLE EQUATIONS ARE OFTEN GIVEN IN THE GENERAL FORMAT 
of     ax2 + by2 + cx + dy + e = 0, 
When you are given this general form of equation and TOLD TO FIND THE CENTER AND RADIUS of a circle, you WILL HAVE TO "COMPLETE THE SQUARE" TO CONVERT the equation to center-radius form. 
This lesson explains how to make that conversion. Copyright © Elizabeth Stapel 2000-2011 All Rights Reserved
EXAMPLE:
Find the center and radius of the circle having the following equation:
         4x2 + 4y2 – 16x – 24y + 51 = 0.
Here is the equation they've given you:  4X2 + 4Y2 - 16X - 24Y + 51 = 0	
Move the loose number over to the other side. 	4X2 + 4Y2 - 16X - 24Y = -51
Group the x-stuff together. Group the y-stuff together. 4x2 - 16x + 4y2 - 24y = -51	
Whatever is multiplied on the squared terms (it'll always be the same number), divide it off from every term.                    	x^2 – 4x + y^2 – 6y = –51/4
This is the complicated step. 
You'll need space inside your groupings, because this is where YOU'LL ADD THE SQUARING TERM. 
Take the X-TERM COEFFICIENT, MULTIPLY IT BY ONE-HALF, SQUARE IT, and then add this to BOTH SIDES OF THE EQUATION, as shown.  .5 * -4 = -2; -2 * -2= 4
Do the SAME WITH THE Y-TERM COEFFICIENT.  0.5 * -6= -3; -3 * -3= 9
  me:  in between step: (x^2 - 4x + 4)+(y^2 - 6y + 9)= - 51/4 +4 +9 [4+9= 16/4 + 36/4=52/4]
Convert the left side to squared form, and simplify the right side. 
                                      	(x – 2)^2 + (y – 3)^2 = 1/4
Read off the answer from the rearranged equation. 
      	The center is at (h, k) = (x, y) = (2, 3).
        The radius is r = sqrt( 1/4 ) = 1/2
Warning: Don't misinterpret the final equation. Remember that the circle formula is (x – h)2 + (y – k)2 = r2. If you end up with an equation like (x + 4)2 + (y + 5)2 = 5, you have to keep straight that h and k are subtracted in the center-radius form, so you really have (x – (–4))2 + (y – (–5))2 = 5. That is, the center is at the point (–4, –5), not at (4, 5). Be careful with the signs; don't just "read off the answer" without thinking. Also, remember that the formula says "r2", not "r", so the radius in this case is sqrt(5), not 5.

In the course of the above procedure, about the only other thing that can be a problem is forgetting the sign on the step where you multiply by one-half. Warning: If you drop a negative, you'll get the wrong answer for the coordinates of the center, so be careful of this. Don't try to do this step in your head: write it out!

Here's one more example of how completing the square works for circle equations:

    Find the center and radius of the circle with the following equation:
         100x2 + 100y2 – 100x + 240y – 56 = 0.

In a two-variable problem rewrite the equations so that when the EQUATIONS ARE ADDED, one of the variables is eliminated, and then solve for the remaining variable. 
Step 1: Multiply equation (1) by -5 and add it to equation 
(2) to form equation (3) with just one variable.

EG.                    x + 3y-4x = -7  TO -3x -9y + 12x = 21
                         3x + y + 2x = 5  TO   3x + y + 2x = 5
  ADDED:                                            0  -8y 

;;FROM http://www.sosmath.com/soe/SE211105/SE211105.html
 A system of equations is a collection of two or more equations with the same set of unknowns. In solving a system of equations, we try to find values for each of the unknowns that will satisfy every equation in the system.
The equations in the system can be linear or non-linear. This tutorial reviews systems of linear equations.
A problem can be expressed in narrative form or the problem can be expressed in algebraic form.
     A system of linear equations can be solved four different ways:
        Substitution
        Elimination
        Matrices
        Graphing 
1. THE METHOD OF SUBSTITUTION: 
  1.8x + 1.2y = 4
     9x +    6y = 3
Step1: solve for x in eq 2;  x = (3 - 6y) / 9
Step2: substitute the value for x in eq1; 1.8 ((3 - 6y) / 9) + 1.2y = 4
Step2: solve for y;   a.  9[1.8 ((3 - 6y) / 9) + 1.2y] = 36
                                  b.    1.8(3 - 6y) + 10.8y = 36
                                  c.    5.4 - 10.8y + 10.8y = 36
                                  d.      5.5  not= 36; therefore THERE IS NO SOLUTION
When we added the changed, but equivalent equation (1) to equation (2), the result was a false statement. What does this mean? It means there is no unique solution to this problem.
2. THE METHOD OF ELIMINATION: 
The process of substitution involves several steps:
Step1: In a two-variable problem REWRITE THE EQUATIONS so that 
when the equations are added, one of the variables is eliminated, and then solve for the remaining variable. [me--see example from purple math above]
-5(1.8x + 1.2y) = -5(4)
-9x - 6y =-20
 9x + 6y = 3
 0   + 0   = -17; FALSE = NOT SOLVABLE
3. THE METHOD OF MATRICES: 
This method is essentially a SHORTCUT FOR THE METHOD OF ELIMINATION.
Rewrite equations (1) and (2) without the variables and operators. The left column contains the coefficients of the x's, the middle column contains the coefficients of the y's, and the right column contains the constants. 
    | 1.8  1.2 ; 4 |
    |    9     6 ; 3 |
 The objective is to reorganize the original matrix into one that looks like 
    |   1      0 ; a |
    |   0      1 ; b |
  where a and b are the solutions to the system.
Step1:Manipulate the matrix so that the number in cell 11 (row 1-col 1) is 1. To achieve this, multiply Row 1 by $\frac{1}{1.8}.$
 1/1.8 [row 1] = new row1
   | 1  2/3 ; 20/9 |
   | 9   6   ;     3  |
Step2: Manipulate the matrix so that the number in cell 21 (row 2-col 1) is 0. Do this by adding -9 times Row 1 to Row 2 to form a new Row 2. 
   -9 [row1} + {row2} = [new row2]
    | 1  2/3 ; 20/9 |
    | 0    0  ; -17   |
Step 3: The last row of the matrix STATES THAT 0=-17, WHICH IS A FALSE STATEMENT. The conclusion is that there is NO SOLUTION to this system. 
4. THE METHOD OF GRAPHING: 
SOLVE FOR Y IN EACH EQUATION AND GRAPH.
 Note that the TWO LINES NEVER INTERSECT. Therefore, there is no solution. 
|#



#|
ME:
SOLVE FOR Y:
1.  ax^2 + by^2 + cx + dy + e = 0
2.  by^2 + dy =  -ax^2 -cx -e
3.  y * (by + d) =  -(ax^2 + cx +e)

TEST OF PRIMARY EQUATION:
  (defun test-circle-draw-eq (x-list)
    (let
        ((xy-list)
         )
      (loop
       for x in x-list
       do
       (setf y


|#



;;XXX ===================== OLD LATER DELETE? ==================

;;WORKS BUT REPLACED BY calc-equation-for-varlists
#|(defun calc-equation-for-x-list2 ( x-list &key key1 )
  "U-math REPLACED BY calc-equation-for-varlists
   RETURNS    INPUT: Can be list of x's or one x. Equation must be in form of a quoted list function and arg-list eg '(my-test-eq1 x  2  3) OR
     lisp expression Eg (quote `(+ (* 3 ,x (+ 4 ,x))  7)), note: backquote,comas"
  (let*
      ((y-list)
       (n-x-list 0)
       )
 (unless (listp x-list)
   (setf x-list (list x-list)))
    (loop
     for x in x-list
     do
     (incf n-x-list)
     (let*
         ((y (eval `(+ (* 3 ,x (+ 4 ,x))  7)))
;;`(+ (* 3 ,x (+ 4 ,x))  7));; (list filled-eq))                ;;works (eval `(+ (* 3 ,x (+ 4 ,x))  7)))            )
          )
       (setf y-list (append y-list (list y)))
       ;;(break "1") 
    ;;end let,loop
     ))
    (values y-list  n-x-list )
    ;;end let, calc-equation-for-x-list
    ))
;;TEST
;; (calc-equation-for-x-list2  '(1 2))
;; works = (22 43)  2


(defmacro calc-equation-for-x-list (equation x-list &key key1 )
  "U-math   RETURNS    INPUT: Can be list of x's or one x. Equation must be in form of a quoted list function and arg-list eg '(my-test-eq1 x  2  3) OR
     lisp expression Eg (quote `(+ (* 3 ,x (+ 4 ,x))  7)), note: backquote,comas"
  (eval  `(progn
            (let*
                ((y-list)
                 (n-x-list 0)
                 )
#|              (unless (listp x-list)
                (setf x-list (list x-list)))|#
              (loop
               for x in ,x-list
               do
               (incf n-x-list)
               (let*
                   ((y  ,equation))
                 ;;`(+ (* 3 ,x (+ 4 ,x))  7));; (list filled-eq))                ;;works (eval `(+ (* 3 ,x (+ 4 ,x))  7)))            )
                 )
               (setf y-list (append y-list (list y)))
               (break "1")
 
              ;;end let,loop
               ))
            (values y-list  n-x-list )
            ;;end let, progn, calc-equation-for-x-list
            )))
|#



;;      LATER DELETE??
;;CALC-ANGLES-FOR-TRIANGLE
;;2018
;;ddd
#|(defun calc-angles-for-triangle (side-opp side-adj  &optional hypot
                                     &key  tan sin cos (return-all-sides-p T))
  "U-math. USE CALC-TRIANGLE-VALS INSTEAD!!  RETURNS (values angle radians other-angle type tan sin cos ) Only returns the tan,sin,or cos that was used to calc angle.  INPUT:  tan, etc can be given, and if so, used instead of side lengths."
  (let
      ((angle)      
       (other-angle)
       (radians)
       (type)
       )
    (cond
     (tan
      (setf radians (atan tan)
            type 'tan))
     (sin
      (setf radians (asin sin)
            type 'sin))
     (cos
            (setf radians (acos cos)
                  type 'cos))
     ((and side-opp side-adj)
      (setf tan (/ (- y2 y1) (- x2 x1))
            type 'tan))
     ((and side-adj hypot)
      (setf sin (/ side-adj hypot)
            type 'sin))
     ((and side-opp hypot)
      (setf tan (/ side-opp hypot)
            type 'cos))
     (t nil))
    ;;FIND  RADIANS AND ANGLE
      (cond
       (tan    
        (setf radians   (atan tan)))
       (sin       
        (setf radians   (asin sin)))
       (cos       
        (setf radians   (acos cos)))
       (t nil))           
    ;;calc angle
      (setf angle (* radians (/ 180 pi))
            other-angle (- 90 angle))

      (cond
       (return-all-sides-p
        (multiple-value-setq  (hypot adjacent opposite sin cos tan)
            (calc-triangle-vals adjacent opposite  hypot :tan tan :sin sin :cos cos))
        ;;not needed     :angle-type type :angle angle)))
        (values angle radians other-angle type tan sin cos hypot adjacent opposite))
       (t (values angle radians other-angle type tan sin cos)))
    ;;end let, calc-angles-for-triangle
    ))|#
;;TEST
;; (calc-angles-for-triangle nil nil  1.0)
;; works= 45.00000125223908D0   0.7853982  1.0



