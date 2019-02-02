;;******************** ARC-curve-function-tests.lisp ****************************


;;TEST-INTERFACE-2
;;
;;ddd
(capi:define-interface test-interface-2 ()
  ()
  (:panes
   (output-pane-1
    capi:output-pane
    :visible-min-width 800
    :visible-min-height 700

    )
   ;;end panes
   )
  (:layouts
   (column-layout-1
    capi:column-layout
    '(output-pane-1)))
  (:default-initargs
   :best-height 750
   :best-width 850
   :layout 'column-layout-1
   :title "test-interface-2"))






;;CALC-ARC-PTS-BETW-2-PTS-BY-TRIANGLES
;;2018
;;ddd
(defun calc-arc-pts-betw-2-pts-by-triangles (x1 y1 x2 y2 &key (rel-ht 0.3 )(base1-pct 0.5)
                                                (n-pts 4))
  "In   RETURNS    INPUT:   REL-HT is triangle ht relative to  base1. BASE1-PCT= proportion of whole base for base1 side. Note: 0.5 for symetrical triangles (creates circular arcs). N-PTS is the number of x3,y3 points that are calculated by this method."
  (let*
      ((base-sq (+ (expt (- x2 x1) 2.0)(expt (- y2 y1) 2.0)))
       (base (sqrt base-sq))
       (base1 (* base1-pct base))
       (base2 (- base base1))
       (tri-ht (* rel-ht base1))
       (hyp1sq (+ (expt base1 2.0)(expt tri-ht 2.0)))
       (hyp1 (sqrt hyp1sq))
       (hyp2sq (+ (expt base2 2.0)(expt tri-ht 2.0)))
       (x1sq (expt x1 2.0))
       (x2sq (expt x2 2.0))
       (y1sq (expt y1 2.0))
       (y2sq (expt y2 2.0))

       ;; hyp1-sq = (x3 - x1)sq + (y3 - y1)sq = x3sq - x1x3 + x1sq +      y3sq - y1y3 + y1sq
       ;; hyp2-sq = (x3 - x2)sq + (y3 - y2)sq = x3sq - x2x3 + x2sq +      y3sq - y2y3 + y2sq ;; subtract
       ;; hyp1-sq - hyp2-sq =                          0 +x3(x2 - x1)+(x1sq-x2sq) 0+y3(y2-y1)+ (y1sq-y2sq)
       ;;  -x3(x2 - x1) = hyp2sq - hyp1sq +x3(x2 - x1)+(x1sq-x2sq) +y3(y2-y1)+ (y1sq-y2sq)
       ;; X3 = [hyp2sq - hyp1sq + (x1sq-x2sq) +y3(y2-y1)+ (y1sq-y2sq)]  / (x1 - x2)
       ;; -y3(y2-y1) = hyp2-sq - hyp1-sq +x3(x2 - x1)+(x1sq-x2sq) + (y1sq-y2sq)
       ;; Y3 = [hyp2-sq - hyp1-sq +x3(x2 - x1)+(x1sq-x2sq) + (y1sq-y2sq)] / (y1 - y2)
       ;; y3 = x3sq - x1x3 + x1sq +  y3sq - y1y3 + y1sq - hyp1-sq
       (x3  (/ (+ (- hyp2sq hyp1sq)  (- x1sq x2sq)  (* y3 (- y2 y1))  (- y1sq y2sq)  )  (- x1  x2)))
       (y3  (/ (+ (- hyp2sq hyp1sq)  (* x3 (- x2 x1))  (- x1sq x2sq)  (- y1sq y2sq))  (- y1  y2)))
       (n-pts2 (round (/ n-pts 2.0)))
       (all-xy-points1)
       (all-xy-points2)
       (all-points)
       )
    (cond
     ((< n-pts 1) NIL)    
     (t
      (multiple-value-setq (all-xy-points1)
          (calc-arc-pts-betw-2-pts-by-triangles x1 x3 y1 y3 :rel-ht rel-ht :base1-pct base1pct
                                                :n-pts n-pts2))
      (multiple-value-setq (all-xy-points2)
          (calc-arc-pts-betw-2-pts-by-triangles x2 x3 y2 y3 :rel-ht rel-ht :base1-pct base1pct
                                                :n-pts n-pts2))
      (setf  all-points (append all-points all-xy-points1 all-xy-points2))      
      ))
    (values all-points all-xy-points1 all-xy-points2)
    ;;end let, calc-arc-pts-betw-2-pts-by-triangles
    ))
;;TEST
;; (calc-arc-pts-betw-2-pts-by-triangles  100 0  90  40)
;; (calc-arc-pts-betw-2-pts-by-triangles








;;ELLIPSE
;; 2018
;;ddd
(defun test-draw-ellipse (x-radius y-radius width height &key (x 200) (y 200)
                                    filled (foreground :black))
  (let ((inst (make-instance 'test-interface-2)) ;; in U-function-plotter
        )
    ;;DISPLAY INST FIRST?
      (capi:display inst)

    (with-slots (output-pane-1) inst           
      (gp:draw-ellipse output-pane-1 x y x-radius y-radius   
       :filled filled
       :foreground foreground)  ;;foreground was   (if (> x-radius y-radius) :red  :yellow))
      )))
;;TEST [REMEMBER:  FOR ALL POINTS:  X-RADIUS + Y-RADIUS = CONSTANT
;; (test-draw-ellipse 100.0 50.0 400 0) => WIDTH & HT MAKE NO DIF??
;; (test-draw-ellipse 200.0 40.0 0 0) => wide and short
;; (test-draw-ellipse 40.0 200.0  0 0) => tall and narrow


;; CIRCLE, when x-radius = y-radius, then width and height DO NOT MATTER.
;; (test-draw-ellipse 50.0 50.0 0 0) => A CIRCLE



;;;SSSSS TRY X AND Y SOLUTIONS ABOVE IN FUNCTION ABOVE
;;(values (x y)(xx yy) radius  pts-dist  pts-line-direction  (x3 y3) centerline-dir len-centerline)
#|(calc-center-from-pts 100 0 0 0)
xy (100.0 0)(50 0)
xxyy(0 0)(50 0)
((0 50.000004 0))
(150.00002 0)
(100.00001 0)
(100 0)
(50.0 0.0)
(0 -100)
(141.4214 0)|#

#|;;mult
    (x - x1)
    (x - x1)
    -x1*x + x1^2
 x^2 - x1*x
x^2 - 2*x1*x + x1^2
therefore:  x^2 - 2*x1*x + x1^2 + y^2 - 2*y1*y + y1^2 = r^2
 y^2 - 2*y1*y =    r^2 - y1^2 -  x^2 + 2*x1*x - x1^2 
try subst     y  =  (sqrt (r^2 -  (x - x2)^2) + y2))
  y^2 - 2*y1*[ (sqrt (r^2 -  (x - x2)^2) + y2) ]  =  

;; Y FROM X, ETC IF R KNOWN
or y =  (sqrt [R^2 - y1^2 -  x^2 + 2*x1*x - x1^2 + - 2*y1*[ (sqrt (R^2 -  (x - x2)^2) + y2) ]  ]

;;ALSO
    (x - x1)^2  +  (y -  y1)^2 = r^2 =  (x - x2)^2  +  (y - y2)^2 
    (x - x1)^2  +  (y -  y1)^2 - (x - x2)^2  -  (y - y2)^2  = 0
or
1.  x^2 - 2*x1*x + x1^2 + y^2 - 2*y1*y + y1^2 = r^2
and
 2. x^2 - 2*x2*x + x2^2 + y^2 - 2*y2*y + y2^2 =  r^2
subt 2 from 1:
      0  -  2( x1*x - x2*x) + (x1^2 - x2^2) - 2(y1*y - y2*y) + (y1^2 - y2^2) = 0
      x1*x - x2*x  =  (x1^2 - x2^2) - 2(y1*y - y2*y) + (y1^2 - y2^2)  /  2
              x (x1 - x2) = 
 ;;X FROM Y ETC IF R1 = R2
                 x =  [ (x1^2 - x2^2) - 2(y1*y - y2*y) + (y1^2 - y2^2)  /  2  ]   / (x1 - x2) 
|#

(defun test-draw-circle (radius   &key (x 200) (y 200)
                                    filled (foreground :black))
  (let ((inst (make-instance 'test-interface-2)) ;; in U-function-plotter
        )
    ;;DISPLAY INST FIRST?
      (capi:display inst)

    (with-slots (output-pane-1) inst           
      (gp:draw-circle output-pane-1  x y  radius  :filled filled
       :foreground foreground)  ;;foreground was   (if (> x-radius y-radius) :red  :yellow))
      )))
  



#|
11.3.3 An example pinboard object
To create your own pinboard objects, the class drawn-pinboard-object is provided, which is a pinboard-object that accepts a display-callback to display itself. The following example creates a new subclass of drawn-pinboard-object that displays an ellipse.
(defun draw-ellipse-pane (gp pane
                             x y
                             width height)
  (with-geometry pane
    (let ((x-radius
           (1- (floor %width% 2)))
          (y-radius
           (1- (floor %height% 2))))
      (gp:draw-ellipse 
       gp
       (1+ (+ %x% x-radius))
       (1+ (+ %y% y-radius))
       x-radius y-radius
       :filled t
       :foreground 
       (if (> x-radius y-radius) 
           :red 
         :yellow)))))
 
(defclass ellipse-pane
     (drawn-pinboard-object)
  ()
  (:default-initargs
   :display-callback 'draw-ellipse-pane
   :visible-min-width 50
   :visible-min-height 50))
 
(contain
 (make-instance 'ellipse-pane)
 :best-width 200
 :best-height 100)
Figure 11.7 An ellipse-pane class

The with-geometry macro is used to set the size and position, or geometry, of the ellipse drawn by the draw-ellipse-pane function. The fill color depends on the radii of the ellipse - try resizing the window to see this. See the LispWorks CAPI Reference Manual for more details of drawn-pinboard-object .
Now that you have a new ellipse-pane class, you can create instances of them and place them inside layouts. For instance, the example below creates nine ellipse panes and place them in a three by three grid.
(contain
 (make-instance
  'grid-layout
  :description
  (loop for i below 9
        collect
        (make-instance 'ellipse-pane))
  :columns 3)
 :best-width 300
 :best-height 400)


|#


;;xxx --------------------------------------------------------------------------------------

;; (draw-equations-curves  '((arcline  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1 3 .8  2) (2 2  1.0  prev-y) 10 ("cyc1" "cyc2")  "Eq1" 1)  (arcline2  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1 3 .8  2) (2 2  (add-expdelta 1.5 0 1.0)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1))   :multiple-graphs-p T  :y-multiplier 10 :x-multiplier 10  :x-string "  This is X String  " :y-string " y string vertically" :return-output-p NIL)

;;(draw-equations-curves  '((arcline1  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (0.5 1.0  1.5  2.0) (0.5 0.5  (add-expdelta 2.0 0 1.5)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1))   :multiple-graphs-p T  :y-multiplier 10 :x-multiplier 10  :x-string "  This is X String  " :y-string " y string vertically")

;;mult-expdelta
#|
 (draw-equations-curves
  '(
       (arcline1  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.1  2.0) (1.0 1.0 0.1   prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1) 
    (arcline2  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.05  2.0) (1.0 1.0  0.05  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
  ;;  (arcline3 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  1.1  2.0) (1.0 1.0  (mult-expdelta 1.5 0 1.1)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
  ;;  (arcline4  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  1.1  2.0) (1.0 1.0  (mult-expdelta 1.5 0 1.1)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline5 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  1.1  2.0) (1.0 1.0  (add-expdelta 1.5 0 1.1)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline5  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  1.01  2.0) (1.0 1.0  (add-expdelta 1.01 0 1.1)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline6  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  1.01  2.0) (1.0 1.0  (add-expdelta 1.01 0.1 1.001)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline7  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.02  2.0) (1.0 1.0  (add-expdelta 1.0 0.01 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.001  2.0) (1.0 1.0 (add-expdelta 1.0 0.001 1.000)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1) 
  ) :multiple-graphs-p T  :y-multiplier 10.0 :x-multiplier 10.0  :x-string "  This is X String  " :y-string " y string vertically")|#

#|
;; VARY EXP (If base is < 1.0, then SMALLER the EXP, the LARGER the RESULT)
 (draw-equations-curves
  '(
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001  2.0) (1.0 1.0 (add-expdelta 0.9 0.001 0.0)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1) 
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001  2.0) (1.0 1.0 (add-expdelta 0.8 0.001 0.0)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001  2.0) (1.0 1.0 (add-expdelta 0.7 0.001 0.0)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001  2.0) (1.0 1.0 (add-expdelta 0.6 0.001 0.0)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001  2.0) (1.0 1.0 (add-expdelta 0.5 0.001 0.0)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    ) :multiple-graphs-p T  :y-multiplier 10.0 :x-multiplier 10.0  :x-string "  This is X String  " :y-string " y string vertically")|#

#|
;; VARYING INITIAL DY/DX (SLOPE)  HAD LITTLE EFFECT

;;USING HIGH exp and CHANGING C produces good change in curves--tho these all NOT very rounded.
 (draw-equations-curves
  '(
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001  2.0) (1.0 1.0 (add-expdelta 0.9 0.001 0.0)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1) 
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001  2.0) (1.0 1.0 (add-expdelta 0.9 0.001 0.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001  2.0) (1.0 1.0 (add-expdelta 0.9 0.001 0.02)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001 2.0) (1.0 1.0 (add-expdelta 0.9 0.001 0.03)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  0.0001  2.0) (1.0 1.0 (add-expdelta 0.9 0.001 0.1)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    ) :multiple-graphs-p T  :y-multiplier 10.0 :x-multiplier 10.0  :x-string "  This is X String  " :y-string " y string vertically")|#

#|
;;TRY MULT-EXPDELTA
;; ADDING C (.001 < C <  .3)  MAKES A BIG DIFFERENCE IN STEEPNESS, MUST KEEP EXP SMALL (< 1.1?)
 (draw-equations-curves
  '(
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .01  2.0) (1.0 1.0 (mult-expdelta 1.01 0.001 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1) 
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .05  2.0) (1.0 1.0 (mult-expdelta 1.05 0.01 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1  2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1 2.0) (1.0 1.0 (mult-expdelta 1.05 0.2 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline8 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1  2.0) (1.0 1.0 (mult-expdelta 1.05 0.3 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    ) :multiple-graphs-p T  :y-multiplier 10.0 :x-multiplier 10.0  :x-string "  This is X String  " :y-string " y string vertically")|#
;; FOR MULT-EXPDELTA
;;INITIAL-DELTA MOSTLY AFFECTS RAPIDITY OF STEEPNESS CLIMB
;; KEEP INITIAL VALUES (1.0 < INITIAL-DELTA < 1.3) 





#|
(draw-equations-curves
  '(
    (arcline1  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1  2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline2  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1  2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.05)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline3 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1   2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.1)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline4 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1  2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.2)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline5  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1  2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.3)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    ) :multiple-graphs-p T  :y-multiplier 10.0 :x-multiplier 10.0  :x-string "  This is X String  " :y-string " y string vertically")
|#

;; WHEN MULT-EXPDELTA
;;IF MODIFY INITIAL DY/DX (SLOPE), KEEP INITIAL-DELTA SMALL (< 1.05 ?)
;; GOOD FOR CHANGING CURVATURE 
#|
(draw-equations-curves
  '(
    (arcline1  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1  2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline2  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .05  2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline3 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .1   2.0) (1.0 1.0 (mult-expdelta 1.05 0. 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline4 `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .15  2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    (arcline5  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1.0 2.0  .2  2.0) (1.0 1.0 (mult-expdelta 1.05 0.1 1.01)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1)
    ) :multiple-graphs-p T  :y-multiplier 10.0 :x-multiplier 10.0  :x-string "  This is X String  " :y-string " y string vertically")
|#
