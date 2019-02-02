;;************************* U-GP-utilities.lisp ***************************




;;GLOBAL VARS
(defparameter *temp-my-draw-curve-points nil)
(defparameter *all-temp-my-draw-curve-points nil)




;;MY-DRAW-PATHS
;;2018
;;ddd
(defun my-draw-paths (port arglists  &key (dec 3) )
  "In   RETURNS    INPUT: Arglists are lists with keyword :line, :circle, :arc, :string, :rectangle followed by an arglist to appropriate function creating that portion of the line.  Lines do NOT have to be continuous. dec= decimals returned by each.
   NOTE: :ARC ARGS= port x1 y1 &optional x2 y2 &key (n-pts 6) draw-string (str-x0 50) (str-y0 550) (rel-r 3.0) radius arc-ht (rel-height 0.5) (x0 80) (y0 50) (win-bottom 600) radius2 (rel-radius2-1 1.0) sweep-angle start-angle sweep-angle-rads start-angle-rads (x-multiplier 1.0) (y-multiplier 1.0) (dec 3) clockwise-p x-max y-max xy1-xy2 draw-radius-p draw-xyaxes-p x-label y-label)"
  (let
      ((output-lists)
       (n-paths (list-length arglists))
       )
    (loop
     for arglist in arglists
     for n from 1 to n-paths
     do
     (let*
         ((key (car arglist))
          (args (append (list port) (cdr arglist)))
          (function)        
          )
     (cond
      ((equal key :line)
       (setf function 'gp:draw-line))
       ((equal key :string)
        (setf function 'my-draw-string))
       ((member key '(:my-arc :arc))
        (setf function 'draw-arc-from-pts))
       ((member key '(:rectangle :rect))
        (setf function 'gp:draw-rectangle))
       ((member key '(:circle :cir))
        (setf function 'gp:draw-circle))
      (t nil))
     (multiple-value-bind (a b c d e f g h i j k l m)
         (apply function args)
       (setf outputs (list a b c d e f g h i j k l m)
             outputs (delete-last-items nil outputs )
             output-lists (append output-lists (list outputs)))
       ;;end mvb
       )
     ;;end let,loop
     ))
    (values  output-lists  )
    ;;end let, my-draw-paths
    ))
;;TEST
;; (my-draw-paths 
;; (test-my-draw-paths-with-gp-transform '((:line  100 100 300 300))) = ((0)) works
;; (test-my-draw-paths-with-gp-transform '((:line  100 100 300 300)(:string "TEST STRING HERE" 150 175)))
;; works= ((0)(0))
;; (test-my-draw-paths-with-gp-transform '((:arc  70 60 100 140 :draw-radius-p T)))
;; works= ((((106.778 620.877) (114.147 608.27) (119.99 595.028) (124.302 581.21) (127.045 566.868) (128.137 552.435)) (106.778 114.147 119.99 124.302 127.045 128.137) (620.877 608.27 595.028 581.21 566.868 552.435) 80 50 550 6 -108.137 507.574 128.16 -40.05 38.961 -0.699))
 



;;USE FOLLOWING TO TEST MY-DRAW-PATHS

(defun test-my-draw-paths (arglists &key node1 node2 f-args1 f-args2 info1 p q r s  u v)
  (let ((inst (make-instance 'drawtest-interface-NO-CALLBACK)) ;; in U-function-plotter
        (trans-list (make-transform-list u v :p p :q q :r r :s s))
        (trans-scale)
        (get-trans-list)
        )
    ;;DISPLAY INST FIRST
      (capi:display inst)
    (with-slots (output-pane-1) inst   
      
     ;;NO-CHANGE X & Y
      (gp:with-graphics-state (output-pane-1   :transform (list p q r s u v )) ;; trans-list)) ;;     ;;or with-graphics-transform ( )  ;; :transform '(1.0 0 0 1.0 0 0))
      ;;not work either for uv: (gp:set-graphics-state output-pane-1 :transform (list p q r s u v))
        (setf trans-scale (gp:get-transform-scale (list p q r s u v )) ;;  trans-list))
          ;;find current transform list --should be same as just set above -- redundant?
          get-trans-list (gp:graphics-state-transform (gp:port-graphics-state output-pane-1)))
        ;;to test my-draw-paths
        (when arglists
            (my-draw-paths output-pane-1 arglists))

          ;;to test my-draw-arrow NEEDS WORK
          (my-draw-arrow output-pane-1 200 200 30)

          ;;to test transform, pute gp:draw-line -
          ;;U,V TRANSLATION NOT WORK, PQRS CONFUSING
          (gp:draw-line output-pane-1  200 100  350  140)

        ;;TO TEST draw-node-path
        (when node1
            (draw-node-path output-pane-1 (car node1)(second node1)
                            (car node2)(second node2)
                            :f-args1  f-args1 :f-args2 f-args2))
          (values get-trans-list trans-scale trans-list)
          ;;end test-my-draw-paths
          ))))
;;TEST



(defun test-my-draw-paths-with-gp-transform (arglists &key node1 node2 f-args1 f-args2 info1 p q r s  u v)
  (let ((inst (make-instance 'drawtest-interface-NO-CALLBACK)) ;; in U-function-plotter
        (trans-list (make-transform-list u v :p p :q q :r r :s s))
        (trans-scale)
        (get-trans-list)
        )
    ;;DISPLAY INST FIRST
      (capi:display inst)
    (with-slots (output-pane-1) inst   
      ;;SSSSSS START HERE, LEARN HOW TO USE TRANSFORM, 
      ;;  AND MAYBE MAKE A FUNCTION TO USE IT EASIER--SO NUMS MAKE SENSE
      
     ;;NO-CHANGE X & Y
      (gp:with-graphics-state (output-pane-1   :transform (list p q r s u v )) ;; trans-list)) ;;     ;;or with-graphics-transform ( )  ;; :transform '(1.0 0 0 1.0 0 0))
      ;;not work either for uv: (gp:set-graphics-state output-pane-1 :transform (list p q r s u v))
        (setf trans-scale (gp:get-transform-scale (list p q r s u v )) ;;  trans-list))
          ;;find current transform list --should be same as just set above -- redundant?
          get-trans-list (gp:graphics-state-transform (gp:port-graphics-state output-pane-1)))
        ;;to test my-draw-paths
        (when arglists
            (my-draw-paths output-pane-1 arglists))

          ;;to test my-draw-arrow NEEDS WORK
          (my-draw-arrow output-pane-1 200 200 30)

          ;;to test transform, pute gp:draw-line -
          ;;U,V TRANSLATION NOT WORK, PQRS CONFUSING
          (gp:draw-line output-pane-1  200 100  350  140)

        ;;TO TEST draw-node-path
        (when node1
            (draw-node-path output-pane-1 (car node1)(second node1)
                            (car node2)(second node2)
                            :f-args1  f-args1 :f-args2 f-args2))
          (values get-trans-list trans-scale trans-list)
          ;;end test-my-draw-paths-with-gp-transform
          ))))
;;TEST
;; ;;  (test-my-draw-paths-with-gp-transform '((:line  100 100 300 300)) :P 1.0 :Q 1.0 :R 1.0 :S 1.0 :U 1.0 :V 1.0)
;; = (1 0 0 1 0 0) 1.4142135  (1.0 1.0 1.0 1.0 1.0 1.0)
;;  (test-my-draw-paths-with-gp-transform '((:line  100 100 300 300)) :P 1.0 :Q 1.0 :R 1.0 :S 1.0 :U 500.0 :V -500.0)
;; = NO obvious effectS.
;;= (1.0D0 1.0D0 1.0D0 1.0D0 500.0D0 -500.0D0)  1.4142135  (1.0 1.0 1.0 1.0 500.0 -500.0)
;;   (test-my-draw-paths-with-gp-transform '((:line  100 100 300 300)) :P 3.0 :Q 1.0 :R 1.0 :S 1.0 :U 1.0 :V 1.0) 
;; = makes diagonal line much longer, moves down-to R, and rotates minimally?
;; ;;   (test-my-draw-paths-with-gp-transform '((:line  100 100 300 300)) :P 1.0 :Q 2.0 :R 1.0 :S 1.0 :U 1.0 :V 1.0) 
;;  = rotates clockwise and moves line down significantly
;; (test-my-draw-paths-with-gp-transform '((:line  100 100 300 300)) :P 1.0 :Q 1.0 :R 3.0 :S 1.0 :U 3.0 :V 1.0)
;; moves line to R, makes much longer,  and rotates counterclockwise significantly
;; (test-my-draw-paths-with-gp-transform '((:line  100 100 300 300)) :P 1.0 :Q 1.0 :R 1.0 :S 3.0 :U 1.0 :V 1.0)
;;  = rotates clockwise and moves line down significantly
;; ;;  (test-my-draw-paths-with-gp-transform '((:line  100 100 300 300)) :P 1.0 :Q 1.0 :R 1.0 :S 3.0 :U 1.0 :V 1.0)
;;  = rotates clockwise and moves line down significantly (about? same as :R)


;;DRAW-NODE-PATH
;;2018
;;ddd
(defun draw-node-path (port  x1 y1 x2 y2 
                            &key  f-args1 f-args2 (rev-a T) ( rev-b T) (draw-rectangle-p T)(rev-rect-p T)
                            (info-loc 0.5)(info-type :string) (ht 15)(length 15) (color :blue)
                            (dash '(4 2 4 4))(dashed T) (thickness 2) (font-weight :normal)(font-size 7)
                            (rect-ht/line 12))
  "In   RETURNS    INPUT:  F-ARGS=format nil args (string args) for (format nil string args) REV-A, REV-B causes lines drawn opp direction."
;;  graphics-state parameters transform, FOREGROUND, background, operation, PATTERN, THICKNESS, scale-thickness, DASHED, DASH, LINE-END-STYLE, mask, SHAPE-MODE and compositing-mode are used
  (let*
       ((xa (cond (rev-a  (- x1 ht))(t (+ x1 ht))))
       (ya (- y1 length))
       (xb (cond (rev-b  (- x1 ht))(t (+ x1 ht)))) ;;was (+ x2 ht ))
       (yb (+ y2 length))
       ;;(xya-xyb (calc-dist-betw-2-pts xa ya xb yb))
       (a-b (calc-dist-betw-2-pts xa ya xb yb))
      (info-dist (* info-loc a-b))
       (info1 (when f-args1 (eval (append '(format nil)(list (car f-args1))(cdr f-args1)))))
       (info2 (when f-args2 (eval (append '(format nil)(list (car f-args2))(cdr f-args2)))))
       (info1-w (port-string-width port info1))
       (info2-w (when info2 (port-string-width port info2)))
       (str-ht (port-string-height port info1))
      (info-x (- (+ info-dist xa) 30)) ;;see rev-rect below 
       (info-y (- ya  info-dist ))
       (rect-x)
       (rect-y (- info-y  10))
       )
    (my-draw-paths port  (list 
                   (list :line x1 y1 xa ya :foreground color :dash dash :dashed dashed 
                         :thickness thickness) ;;not work :line-end-style :round)
                   (list :line xa ya xb yb :foreground color :dash dash :dashed dashed
                         :thickness thickness)
                   (list :line xb yb x2 y2 :foreground color :dash dash :dashed dashed
                         :thickness thickness))) ;;not work :line-end-style :projecting)))

    ;;DRAW RECTANGLE?
    (When draw-rectangle-p
      ;;set the rect-len
      (cond
       ((and info1 info2)
        (cond
         ((>= (setf rect-len info1-w) info2-w) NIL)
          (t (setf rect-len info2-w)))
                  (setf rect-ht (+ 5  (* 2 str-ht))))
        (info1 
         (setf rect-ht (+ 4 str-ht))))

      ;;set the info-x, rect-x
      (cond
       (rev-rect-p
         (setf info-x (-  xa info-dist rect-len 70)))
        (t (setf info-x (- (+ info-dist xa) 30))))
      (when (< info-x 10) (setf info-x 15))
      ;;rect-x
      (setf rect-x (- info-x 5))
     ;;draw the rectangle
     (gp:draw-rectangle port rect-x rect-y  (* 3.5 (+ rect-len 10))   rect-ht
                           :foreground color :dash dash :dashed dashed  :thickness thickness)
     ;;end when
     )
    ;;DRAW TEXT?
    (when (and (equal info-type :string) info1)
      (my-draw-string port info1 info-x info-y :weight font-weight :size font-size))
    (when (and (equal info-type :string) info2)
      (my-draw-string port info2 info-x (+ info-y str-ht 5) :weight font-weight :size font-size))

      (values xa ya xb yb info-x info-y)
      ;;end let, draw-node-path
      ))
;;TEST
;; (test-my-draw-paths-with-gp-transform nil :node1 '(150 150) :node2 '(150 50) :f-args1 '( "TEST NODE"))
;;  works
;; ;; (test-my-draw-paths-with-gp-transform nil :node1 '(150 150) :node2 '(150 50) :f-args1 '( "TEST NODE") :f-args2 '("  value1= ~A value2= ~A " (99  'this-val)))






;;MY-DRAW-STRING
;;2018
;;ddd
(defun my-draw-string (pane string x y  &rest args 
                            &key transform (start 0) end block  
                            (family "Times New Roman")
                            (weight :normal)(size 9))
  "U-GP-utilities.lisp"
  (let*
      ((font (gp:find-best-font pane 
                                 (gp:make-font-description 
                                    :family family :weight weight  :size size)))
       )
    (gp:draw-string pane string x y :font font)
    ))



;;MY-DRAW-RECTANGLE-PATH
;;2018-03
;;ddd
(defun my-draw-rectangle-path (pane  x1 y1 x2 y2 &key (path-height 8) 
                                     (random-ht-p T) (lo-ht 3))
  "U-GP-utilities.lisp.  If  random-ht-p, sets height between lo-ht and (+ lo-ht height) "
  (let
      ((x1a)
       (x2a) 
       )
    (when random-ht-p
      (setf path-height (+ lo-ht (random path-height))))

    (cond
     ((= x1 x2)
      (setf x1a (+ x1 path-height)
          x2a  (+ x2 path-height)))
     ((> x2 x1)
      (setf x1a (+ x1 path-height)
          x2a  (- x2 path-height  7)))
     ((< x2 x1)
      (setf x1a (- x1 path-height)
          x2a  (+ x2 path-height)))
      (t nil))
    (gp:draw-line pane x1 y1 x1a y1)
    (gp:draw-line pane x1a y1 x2a y2)
    (gp:draw-line pane x2a y2 x2 y2) 
    ;;(values    )
    ;;end let, my-draw-rectangle-path
    ))
;;TEST
;; (my-draw-rectangle-path x1 y1 x2 y2 :height 10)


;;MY-DRAW-ARROW
;;2018
;;ddd
(defun my-draw-arrow (port x y angle  &key (len-side 7)(len-cent 6)(ab 7)(thickness 2) color
                           (multiplier 5.0))
  "U-GP-utilities.   RETURNS    INPUT: ANGLE= angle of arrow, 0 to 360 deg, A-B= dist from a to b."
  ;;a= top,R line, b= bot,L line, c= center line
  (when (not (= multiplier 1.0))
    (setf len-side (* multiplier len-side)
          len-cent (* multiplier len-cent)))
  (let*
      ((a)
       (b)
       (c)
    ;;For a triangle with sides parallel x,y axes and hyp= xy-c
     ;;Rangle at pt
     (angle-rads (calc-angle-to-radians angle))
       (sin-angle (sin angle-rads))
       (cos-angle (cos angle-rads))
       (x3-xy (* sin-angle len-cent))
       (x3-c (* cos-angle len-cent))
       (xc (- x x3-c  ))
       (yc (- y x3-xy ))
       ;;find b x,y coordinates
       ;;half-xyangle is half of angle between 2 arrow sides
       (sin-half-xyangle (/ (* 0.5 ab ) len-side))
       (half-xyangle-rads (asin sin-half-xyangle))
       (half-xyangle (calc-radians-to-angle half-xyangle-rads))
       ;;angle-xyb= angle betw line parallel to yaxis to xy-b line
       (angle-xyb (- angle half-xyangle))
       (angle-xyb-rads (calc-radians-to-angle angle-xyb))
       (sin-angle-xyb (sin angle-xyb-rads))
       (cos-angle-xyb (cos angle-xyb-rads))
       (xb-x (* sin-angle-xyb len-side)) 
       (yb-y (* cos-angle-xyb len-side))
       (xb (- x xb-x))
       (yb (- y yb-y))
       ;;find a x,y coords
       ;;find 3 angles = 180degs:                  
       ;;1-angle-b1,angle betw b-side and line parallel yaxis; 
       (angle-b1 (- 90 angle-xyb))
       ;; 2-angle-b2,angle betw ab and xyb; 
       (angle-b2 (- 90 half-xyangle))
       ;;  3-angle-b3 angle betw ab and line parallel xaxis.
       (angle-bx2 (- 180 (+ angle-b1 angle-b2)))
       (sin-angle-bx2 (sin angle-bx2))
       (cos-angle-bx2 (cos angle-bx2))
       (xa-xb (* cos-angle-bx2 ab))
       (ya-yb (* sin-angle-bx2 ab))
       (xa (- xb xa-xb))
       (ya (+ yb ya-yb))
       )

    ;;DRAW THE LINES
    ;;a
#|    (gp:draw-line port x y xa ya :foreground :red  ;;:dash dash :dashed dashed 
                         :thickness thickness)
    ;;b
    (gp:draw-line port x y xb yb :foreground :green  ;;:dash dash :dashed dashed 
                         :thickness thickness)|#
    ;;c
     (gp:draw-line port x y xc yc :foreground :black  ;;:dash dash :dashed dashed 
                         :thickness thickness)
    
    ;;SSSSS TEST THIS FOR ONE SIDE OF ARROW, THEN FINISH
    (values xa ya xb yb xc yc )
    ;;end let, my-draw-arrow
    ))
;;TEST
;;  (my-draw-arrow port 100 100 30)


;;MY-DRAW-BEZIER
;;2018
;;ddd
(defun my-draw-bezier (pane x1 y1 x2 y2 &key draw-cs-path-p
                            reverse-a-p reverse-b-p
                          xa ya xb yb pxa pya pxb pyb)
  "U-GP-utilities.lisp When draw-cs-path-p, sets tested parameters for drawing a cs-path. Draws a curve from x1,y1 to a point A, then to point B, then to point x2, y2.  Points A and B are relative to x1,y1."
  (let
      ((xdif (- x2 x1))
       (ydif (- y2 y1))
       )
    (when reverse-a-p
      (setf xa (- xa)))
    (when reverse-b-p
      (setf xb (- xb)))

    (when draw-cs-path-p
      (setf  pxa 0.40
             pya -0.10
             pxb 0.30
             pyb 1.2
             xa  (+ (* pxa xdif) 15)
             ya (+ (* pya ydif) -10)
             xb (+ (* pxb xdif) 10)
             yb (* pyb ydif))
      (when (and (> x2 x1) (null reverse-b-p))
        (setf xb (- xb 5))))

    ;;DRAW THE BEZIER
    (gp:draw-path pane (list (list :bezier xa ya xb yb xdif ydif )) x1 y1)

    ;;(afout 'out (format nil "x1=~A  y1=~A   x2=~A   y2=~A   xa=~A   ya=~A   xb=~A   yb=~A   " x1 y1 x2 y2 xa ya xb yb))
    (values x1 y1 x2 y2 xa ya xb yb)
    ;;end let, my-draw-bezier
    ))

#|
                              |
                              |  dy
      ____________|
                 dx
           dy = a * dx (linear)
           dy = a * da dx (non-linear if da varies?)
 

|#
;;MY-DRAW-CURVE
;;2018
;;ddd
;;new 
(defun my-draw-curve (pane  speclists  &key (dots/unit 10)(dx 1.0)
                            (xtrans 1.0)(ytrans 1.0)
                            (delta-type 'add) (xy0-bottom-p T)
                            (win-width 800)(win-ht 500)
                            (da0ratio-/n-p T)
                            transform filled closed)        ;;no using dots/unit (dots/unit 10))
  "U-GP-utilities.lisp  spec-lists ((x1 y1 x2 y2 a0 a dxc dyc exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c label color) etc). da1ratio = multiple of change of a1 per dx  cda= constant change in a1; X'S AND Y'S ARE IN UNITS NOT PIXELS.   dx= num units/dx.  dots/unit is the scaling factor affecting all dimensions.  If want x1, y1 etc to be in pixels, must set dots/unit = 1.0, then can make dx= 10.0 if want fewer cycles. If (null label) or (null color), does nothing. da0ratio-/n-p causes dx to remain a constant?"
  (let
      ((all-points-list)
       (all-dys)
       )
    (loop
     for speclist in speclists
     do
     (multiple-value-bind (x1 y1 x2 y2 a0 a dxc dyc exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c label color)
         (values-list speclist)

       (let*
           ((ndx (round (/ (- x2  x1) dx))) 
            (x0  x1)       
            (y0  y1)
            (spec-points-list (list :PARS (list x1 y1 x2 y2 a0 a dxc dyc exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c label color)))
            )

         (loop
          for n from 1 to ndx
          do
         ;;To keep dxn the same for each n
         (when da0ratio-/n-p
           (setf da0ratio (/ 1.0 n)))
          (let*
              ;;(x1 y1 x2 y2 a0 a dxc dyc exp0 exp  da0ratio da0ratio-mult 
              ;;                                               daratio daratio-mult cda0 cda c0 c)
              ;; da ratios calc at end
              ((dxn (+ (* a0 (expt (+ (* n da0ratio dx) dxc) exp0))  c0) )
               (xn  (round (* (+ x0  dxn) xtrans)))          
               (dyn (+  (* a  (expt (+ (* n daratio dxn) dyc) exp)) ) )
               (yn  (round (* (+ y0 dyn) ytrans)))
               (points-list (list :N n))
               )
            ;;(break "dxn dyn")
            #|    Use later incorporating above??
        (cond
             ((equal delta-type 'mult)
              (setf  da (* (abs da) da-ratio)))
             ((equal delta-type 'add)             
              (setf  da (+  da da-ratio)))
             ((equal delta-type 'constant)  
              NIL)
             )|#

            ;;DRAW THE SEGMENT
            ;;(break "x1 y1 xn yn")
            (let*
                ((xpix0 (* x0 dots/unit))
                 (ypix0 (* y0 dots/unit))
                 (xpixn (* xn dots/unit))
                 (ypixn (* yn dots/unit))
                 (ypix0-draw ypix0)
                 (ypixn-draw ypixn)
                 )

              (when (null color)
                (setf color :black))

            ;; TRANSLATE TO WIN COORDINATES
            (when  xy0-bottom-p
              (setf ypix0-draw (-  win-ht dots/unit ypix0)
                    ypixn-draw (-  win-ht dots/unit  ypixn)))
              (afout 'out (format nil "exp0= ~A exp= ~A da0ratio= ~A  daratio= ~A  dxn= ~A dyn= ~A  (x0= ~A  y0= ~A) (xn= ~A  yn= ~A)~% PIXELS:  (xpixn= ~A  ypixn-draw= ~A) "exp0 exp  da0ratio daratio  dxn dyn x0 y0 xn yn xpixn  ypixn-draw ))
              (gp:draw-line pane xpix0 ypix0-draw xpixn ypixn-draw
                            :transform transform :foreground color)

            ;;SET NEW INTIAL POINT
            (setf x0 xn
                  y0 yn
                  points-list (append points-list (list (list :dxyn dxn dyn :xyn xn yn   (list :XYPIXN   xpixn ypixn ypixn-draw)))))
            ;;FIND NEW da ratios
            (setf da0ratio (+ (* (abs da0ratio) da0ratio-mult) cda0)
                  daratio (+ (* (abs daratio) daratio-mult) cda))

            (setf spec-points-list (append spec-points-list (list points-list)))
            ;;end lets, inner loop
            )))     
         (setf all-points-list (append all-points-list (list spec-points-list)))
         ;;end let,mvb,outer loop 
         )))
    (setf *temp-my-draw-curve-points all-points-list
          *all-temp-my-draw-curve-points 
          (append *all-temp-my-draw-curve-points (list all-points-list)))

    ;;end let, my-draw-curve
    ))
;;TEST
;; speclist= (x1 y1 x2 y2 a0 a dxc dyc exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c)                             
;;(make-drawtest-instance 'my-draw-curve '(((200.0 200.0 300.0 100.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.8 0 0 0  0 ))))
;;MAKE MULTIPLE CURVES
#|
 (make-drawtest-instance 'my-draw-curve '((
  ;;  (x1 y1 x2 y2  a0  a dxc dyc  exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 C)    
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil NIL)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  1.0 nil :red)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  2.0 nil :blue)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  3.0 nil :brown)
                            ))) ;;=> 4 colored straight lines
 (make-drawtest-instance 'my-draw-curve '((
  ;;  (x1 y1 x2 y2  a0  a  dxc dyc exp0 exp  da0ratio da0ratio-mult DARATIO daratio-mult cda0 cda c0 c)    
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil NIL)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   0.9   1.0   0  0  0  0 nil :red)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   0.8   1.0   0  0  0  0 nil :blue)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   0.7    1.0   0  0  0  0 nil :brown)
                            ))) ;;==> 1 visible line
 (make-drawtest-instance 'my-draw-curve '((
  ;;  (x1 y1 x2 y2  a0  a dxc dyc  exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c)    
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil NIL)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   0.9    0.9   0  0  0  0 nil :red)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   0.8    0.8   0  0  0  0 nil :blue)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   0.7    0.7   0  0  0  0 nil :brown)
                            )))  ;;==>  45deg line w/ 3 lines split to right parallel to baseline

 (make-drawtest-instance 'my-draw-curve '((
  ;;(x1 y1 x2 y2    a0  a   dxc dyc  exp0  exp   da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c label color)    
      (20 10 30 40   1.0 -0.8  0   2.0    1.0    1.5          1.0   1.0                1.0   0.2   0  0  0  0 nil  NIL)
      (20 10 30 40   1.0  -1.5  0   2.0    1.0    2.0          1.0   1.0                1.0    0.2   0  0  0  0 nil :red)
      (20 10 30 40   1.0    1.0  0   2.0    1.0    -1.5          1.0   1.0               1.0     0.2   0  0  0  0 nil :blue)
      (20 10 30 40   1.0    1.0  0   2.0    1.0    -2.0      1.0   1.0                1.0    0.2    0  0  0  0 nil :brown)
                            )))  ;;==> one straight long line at 45deg
 ;; (pprint *all-temp-my-draw-curve-points)
 (make-drawtest-instance 'my-draw-curve '((
  ;;  (x1 y1 x2 y2  a0  a dxc dyc exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c)    
      (20 10 30 40   1.0 1.0 0  0.2 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil NIL)
      (20 10 30 40   1.0 1.0 0  0.2 1.2    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :red)
      (20 10 30 40   1.0 1.0 0  0.2 1.5    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :blue)
      (20 10 30 40   1.0 1.0 0  0.5 2.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :brown)
                            )))
(make-drawtest-instance 'my-draw-curve '((
  ;;  (x1 y1 x2 y2  a0  a   exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c)    
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil NIL)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :red)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :blue)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :brown)
                            )))
(make-drawtest-instance 'my-draw-curve '((
  ;;  (x1 y1 x2 y2  a0  a   exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c)    
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil NIL)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :red)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :blue)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :brown)
                            )))
(make-drawtest-instance 'my-draw-curve '((
  ;;  (x1 y1 x2 y2  a0  a   exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c)    
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil NIL)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :red)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :blue)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :brown)
                            )))
(make-drawtest-instance 'my-draw-curve '((
  ;;  (x1 y1 x2 y2  a0  a   exp0 exp  da0ratio da0ratio-mult daratio daratio-mult cda0 cda c0 c)    
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil NIL)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :red)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :blue)
      (20 10 30 40   1.0 1.0 0  0 1.0    1.0    1.0          1.0                   1.0    1.0   0  0  0  0 nil :brown)
                            )))
 ;;SSSS START HERE W MULTI TESTS


 (make-drawtest-instance 'my-draw-curve '((
                               ;;  (x1 y1 x2 y2   a0   a dxc dyc exp0 exp  da0ratio daratio cda0 cda c0 c)    
                            (200 200 300 100  1.0 1.0 0  0 1.0 1.0    1.0          0.8           0    1.0   0  0 ))))
|#


    ;;SSS START HERE LEARN lw-gt:make-basic-graph-spec 
#|    ;;  AND lw-gt:generate-graph-from-graph-spec
    (setf graph-spec
    (lw-gt:make-basic-graph-spec function start-x step-x range &key color
thickness name x-offset y-offset x-scale y-scale var1 var2 var3 var4
var5 var6))

    ;;(list (list :bezier xa ya xb yb xdif ydif ))
        ;;(gp:draw-path pane pathspecs x1 y1 :transform transform :filled filled             :closed closed)

    (values    )
    ;;end let, my-draw-curve
    ))|#
;;TEST
;;



;;xxx
;; ********************* DRAWING AN ARC -- ----------------------
;; CAN ALSO  USE DRAWING A SERIES OF LINES


;;MY-DRAW-ARC
;;2018
;;ddd
(defun my-draw-arc (output-pane x1 y1 x2 y2  &key arc-height 
                                (start-angle 0)(sweep-angle 180)  (x0 0)(y0 0)
                                (x-multiplier 1.0)(y-multiplier 1.0) (color :black)
                                (draw-x1y1-x2y2-p T))
  "In U-GP-utilities, USE THIS FOR DRAWING ARCS! (1) To draw an arc arc-height high from x1,y1 to x2,y2, just specify pts and arc-height OR (2) to use to do a partial arc from those points, specify start and sweep angles.
  NOTE: gp:draw-arc draws arc inside rectangle x,y to x+width,y+ht from start-angle to start+sweep. GP draws from mid-ht to mid-ht if use 180deg sweep(w/start-angle=0) Uses ellipse with centers  parallel to x or y axes only.  Must do a transform to change orientation, etc.(y0 600) moves x-axis down 600 from window top."

;; (+ x1 width) = x2;  width = x2 - x1
  (let*
      ((width (sqrt (+ (expt (- x2 x1) 2)(expt (- y2 y1) 2))))
       (height (* 2.0 arc-height)) ;; (* 2.0 arc-height))
       (rect-y  (- y1 arc-height))
       (len-oppside (- y2 y1))
        (len-nearside (- x2 x1))
        (tan-trans-angle (/  len-oppside len-nearside))
        (trans-angle-rads (atan tan-trans-angle))
        (trans-angle (calc-radians-to-angle trans-angle-rads))
        (arc-tan-angle (* 1.0 tan-trans-angle)) ;; (* 2.0 tan-trans-angle))
        ;;create transform
        (transform (list  x-multiplier tan-trans-angle 0 y-multiplier x0 y0))
       ;; (arc-trans (list  x-multiplier arc-tan-angle 0 y-multiplier x0 y0))
       (start-angle-rads (calc-angle-to-radians start-angle))
       (sweep-angle-rads (calc-angle-to-radians sweep-angle))
       ;;find trans rectangle width to match the width betw x1y1-x2y2
       ;;For arc/rect width becomes original x2 - x1
       (trans-rect-width len-nearside)
       )     
   ;; (break "before draw-arc")
  (gp:draw-arc output-pane x1 rect-y trans-rect-width height 
               start-angle-rads sweep-angle-rads
                 :graphics-args (list :foreground color) :transform transform)
  (when draw-x1y1-x2y2-p
     ;;NO TRANSFORM, this is original x1y1-x2y2 line
    (gp:draw-line output-pane x1 y1 x2 y2
                  :graphics-args (list :foreground color))
    ;;the rectangle enclosing the arc lowered so arc is on top line = x1y1-x2y2 line
   ;; (gp:draw-rectangle output-pane x1 y1 trans-rect-width height :transform transform)
    )
  ))
;;TEST 
;; THIS FUNCTION WORKS, USE FOLLOWING FOR TESTING
;; MUST UNQUOTE FUNCTIONS BELOW
;; HOW TO DRAW ARC FROM X1,Y1 TO X2,Y2
;; note: points are on line parallel to x-axis.
;; (my-draw-arc-example  0 100  200 100  0  180 :use-my-draw-arc-p T)
;; works= draws arc from x1,y1 to x2,y2
;; (my-draw-arc-example  0 100  200 100  30  80 :arc-height 20 :use-my-draw-arc-p T)
;; works= draws arc from from 30 to 80deg partial full arc above x1,y1 to x2,y2; 
;;    also the height is less
;; ROTATION TRANSORM
;; points NOT on line parallel to either axis
;; (my-draw-arc-example  0 100  200 200  0  180 :arc-height 40 :use-my-draw-arc-p T)
#|
(defun my-draw-arc-example ( x1 y1 x2 y2  start-angle sweep-angle 
                                &key angle (arc-height 50) (x0 0)(y0 0) (use-my-draw-arc-p T))
  (setf *myexx1 x1
        *myexy1 y1
        *myexx2 x2
        *myexy2 y2
        *my-start-angle start-angle
        *my-sweep-angle  sweep-angle
        *my-start-angle-rads (calc-angle-to-radians start-angle)
        *my-sweep-angle-rads (calc-angle-to-radians  sweep-angle)
        *arc-height arc-height
        *myx0 x0
        *myy0 y0)

  (cond
   (use-my-draw-arc-p
    (setf *use-my-draw-arc-p T))
   (t (setf use-my-draw-arc-p NIL)))

 (setq *my-example-pane* 
        (capi:contain (make-instance 'capi:output-pane
                                     :background :white
                                     :DISPLAY-CALLBACK 'my-example-display-callback
                                     :initial-constraints '(:min-width 400 :min-height 400))
                      :title "My example"))
;; If want rotation?
#|  (capi:apply-in-pane-process *my-rotation-example-pane*
                              'metafile-rotation-example-do-animate 
                              *metafile-rotation-example-pane* 200 200)|#
  )


(defun my-example-display-callback (pane &rest x)
  (declare (ignore x))
  (let* ((transform1 (list 1.0 0 0 1.0 *myx0 *myy0)) ;; (gp:make-transform))
         (angle *myex-angle)
         (x1  *myexx1)
         (y1  *myexy1 )
         (x2  *myexx2 )
         (y2  *myexy2 )
         (width  (sqrt (+ (expt (- x2 x1) 2)(expt (- y2 y1) 2)))) ;;(- x2 x1))
         (height *arc-height) ;; (- y2 y1))
         (start-angle *my-start-angle)
         (sweep-angle *my-sweep-angle)
         (x0 *myx0)
         (y0 *myy0)
         (x-multiplier 1.0)
         (y-multiplier 1.0)
         )
      
    ;;WORKS (rotates around 100 100)
    ;;(gp:apply-rotation-around-point trans angle (* 2 x1)(* 2 y1))
;; results (* .5 (- x2 x1))(* .5 (- x2 x1)))=no-show  ;; x1 y1= left side) ;; 100 100)

    (when *use-my-draw-arc-p
      ;;transform done within my-draw-arc
      (my-draw-arc pane x1 y1 x2 y2  :arc-height height :start-angle start-angle 
                   :sweep-angle sweep-angle :x0 x0 :y0 y0  :color :red
                                :x-multiplier x-multiplier :y-multiplier y-multiplier))
    ;;(break "end of my-draw-arc")
    ;;OTHER LINES
    (gp:with-graphics-transform (pane transform1)         
      (gp:draw-line pane x1 y1 x2 y2 ) ;;:transform transform)
#|      (gp:draw-arc pane   x1  y1 width height *my-start-angle-rads 
                   *my-sweep-angle-rads 
                   :graphics-args (list :foreground :red) :transform transform1) ;;(- x2 x1)|#
      ;;draw rectangle that encloses arc
      ;;(break "rect")
      (gp:draw-rectangle pane x1 y1 width height :transform transform1)

      ;;draw line from origin to x2 y2
      (gp:draw-line pane 0  0  x2 y2)
      ;;from y at lower x-axis
      (gp:draw-line pane x1 y0  x2 y0)
      (gp:draw-string pane "X-AXIS" x1 (+ y0  15))
      )
    ;;end let*, my-example-display-callback
    ))
;;END TEST FOR MY-DRAW-ARC
|#


;;DRAWTEST-INTERFACE
;;2018
;;ddd
(capi:define-interface drawtest-interface ()
  ;;SLOTS
  ((drawing-function                                 
    :initarg :drawing-function
    :accessor drawing-function
    :initform nil
    :documentation "Drawing function symbol")
    (function-args                                 
    :initarg :function-args
    :accessor function-args
    :initform nil
    :documentation "Drawing function-argslist")
    ;;end SLOTS
    )
  (:panes
   (editor-pane-1
    capi:rich-text-pane
    :visible-max-height 40)
   (output-pane-1
    capi:output-pane
    :display-callback 'drawtest-callback
    :visible-min-height 600
    :visible-min-width 900
    )
   (editor-pane-2
    capi:rich-text-pane
    :visible-max-height 40)
   (push-button-1
    capi:push-button
    :text "Push-Button-1")
   (push-button-2
    capi:push-button
    :text "Push-Button-2"))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(editor-pane-1 output-pane-1 editor-pane-2 row-layout-1))
   (row-layout-1
    capi:row-layout
    '(push-button-1 push-button-2)))
  (:default-initargs
   :best-height 750
   :best-width 900
   :layout 'column-layout-1
   :title "Drawtest-Interface"))

(capi:define-interface drawtest-interface-NO-CALLBACK ()
  ;;SLOTS
  ((drawing-function                                 
    :initarg :drawing-function
    :accessor drawing-function
    :initform nil
    :documentation "Drawing function symbol")
    (function-args                                 
    :initarg :function-args
    :accessor function-args
    :initform nil
    :documentation "Drawing function-argslist")
    ;;end SLOTS
    )
  (:panes
   (editor-pane-1
    capi:rich-text-pane
    :visible-max-height 40)
   (output-pane-1
    capi:output-pane
   ;; :display-callback 'drawtest-callback
    :visible-min-height 600
    :visible-min-width 900
    )
   (editor-pane-2
    capi:rich-text-pane
    :visible-max-height 40)
   (push-button-1
    capi:push-button
    :text "Push-Button-1")
   (push-button-2
    capi:push-button
    :text "Push-Button-2"))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(editor-pane-1 output-pane-1 editor-pane-2 row-layout-1))
   (row-layout-1
    capi:row-layout
    '(push-button-1 push-button-2)))
  (:default-initargs
   :best-height 750
   :best-width 900
   :layout 'column-layout-1
   :title "Drawtest-Interface-NO-CALLBACK"))


;;MAKE-DRAWTEST-INSTANCE
;;2018-02
;;ddd
(defun make-drawtest-instance (drawing-function function-args)
  "U-GP-utilities.lisp  spec-lists ((x1 y1 x2 y2 a b dx) etc) "
  (let
      ((inst (make-instance 'drawtest-interface 
                            :drawing-function (quote my-draw-curve)
                            :function-args function-args))
       )
    (capi:display inst)
    ;;end let, make-drawtest-instance
    ))
;;TEST





;;DRAWTEST-CALLBACK
;;2018-02
;;ddd
(defun drawtest-callback (pane x y width height)
  "U-GP-utilities"
  (let*
      ((interface (capi:top-level-interface pane))
       (drawing-function (slot-value interface 'drawing-function))
       (function-args (slot-value interface 'function-args))
       )
    ;;(break "args")
    ;;EVAL THE FUNCTION
    (apply drawing-function pane function-args)
    ;;end let, drawtest-callback
    ))
;;TEST
;; 


;;DRAW-ARC-FROM-PTS
;;2018
;;ddd
(defun draw-arc-from-pts (port x1 y1 &optional x2 y2
                               &key (n-pts 6) draw-string (str-x0 50) (str-y0 550)
                               (rel-r 3.0) radius   arc-ht  (rel-height 0.5)
                               (x0 80)(y0 50)(win-bottom 600) radius2 (rel-radius2-1 1.0)
                               sweep-angle start-angle sweep-angle-rads  start-angle-rads
                               (x-multiplier 1.0)(y-multiplier 1.0) (dec 3) clockwise-p
                               x-max y-max xy1-xy2
                               draw-radius-p draw-xyaxes-p x-label y-label)
  "U-GP-utilities RETURNS (values new-xypts new-xmpts new-ympts x0 y0 new-y0 n-xpts  center-x center-y radius start-angle sweep-angle start-angle-rads sweep-angle-rads   pts-dist len-centerline)   INPUT:    REL-HEIGHT= arc-ht/radius. If height provided--not used. x-multiplier and y-multiplier multiply values of x2 y2 and radius by xy-multiplier. Will also DRAW FROM 1 PT and ANGLES."
  (let
      ((xy-multiplier x-multiplier)
       (x1m)
       (y1m)
       (x2m)
       (y2m)
       (orig-x1 x1)
       (orig-y1 y1)
       (orig-x2 x2)
       (orig-y2 y2)
       (center-xm)
       (center-ym)
       (center-x1) ;;may not need this, see below
       (center-x2)  ;;may not need this, see below
       (new-xpts)
       (new-xmpts)
       (new-ypts)
       (new-ympts)
       (new-y0)
        (n-xpts)
       (n-ypts)
       (arc-points)   ;; x0 y0 x2 y2 sweep-angle start-angle 
                              ;;             radius radius2 sweep-angle-rads  start-angle-rads)
       )
    ;;SSSSS START HERE draw-arc-from-pts

    ;;1-FIND THE CIR CENTER(S)
    ;;SSSS CHECK TO SEE IF WHEN ENDS IN RIGHT PLACE (END OF DEFUN)
      (when (and x2 y2)
        ;;was (when (and x2m y2m)
        (multiple-value-bind (center-x center-y xx yy radius1
                                       pts-dist  pts-line-direction  x3 y3 centerline-dir  len-centerline)
            (calc-center-from-pts x1 y1 x2 y2  :rel-r rel-r :radius radius :arc-ht arc-ht)
        ;;was (calc-center-from-pts x1m y1m x2m y2m  :rel-r rel-r :radius radius)       
      
        (unless radius
          (setf radius radius1)) ;;NO-do in transform (* radius1 xy-multiplier)))
        ;;(break "center-x")

        ;;2-CONVERT 2 PTS TO RADIANS--calc start-angle and sweep-angle
       (multiple-value-bind (sweep-angle-rads1 start-angle-rads1 sweep-angle1
                                                  start-angle1) ;; center-x center-y center-xx center-yy)
            (calc-radians-from-2pts  x1 y1 x2 y2 :radius  radius)
         (when (and sweep-angle1 (null sweep-angle))
           (setf sweep-angle sweep-angle1
                 sweep-angle-rads sweep-angle-rads1))
         (when (and start-angle1 (null start-angle))
           (setf start-angle start-angle1
                start-angle-rads start-angle-rads1))
         ;;(break "pre-calc-pts")

          ;;3-CALC THE ARC POINTS
          (multiple-value-setq (arc-points center-x1 center-y1 x2 y2 sweep-angle start-angle 
                                           radius radius2 sweep-angle-rads  start-angle-rads)
              (calc-arc-points x1 y1 x2 y2  :x0 center-x
                               :y0 center-y  :xy1-xy2angle sweep-angle
                               :xy1-xy2angle-rads sweep-angle-rads
                               :radius  radius :rel-radius rel-r :radius2  radius2 
                               ;;add later? (radians/pt 2.0)
                               :n-pts n-pts :xy1-xy2 xy1-xy2
                               :start-angle start-angle :start-angle-rads start-angle-rads
                               :rel-radius2-1 rel-radius2-1 :dec dec  :clockwise-p clockwise-p))
          (unless center-x
            (setf center-x center-x1 center-y center-y1))
         ;;(break "pre trans pts")

          ;;TRANSFORM THE POINTS 
          (multiple-value-setq (new-xypts new-xmpts new-ympts new-xpts new-ypts  
                                          x0 y0 new-y0 n-xpts n-ypts )
              (transform-draw-axes-on-xypoint-lists arc-points :win-bottom win-bottom
                            :x0 x0 :y0 y0 :x-multiplier x-multiplier :y-multiplier y-multiplier))
          ;;transform centers too
           (setf center-xm (- center-x x0)
                 center-ym (+ (- win-bottom center-y)  y0))
          ;;(break "pts win-bottom")

          ;;4-DRAW THE ARC (and string?)
          ;;new-xypts = 
          ;;(my-draw-path port arc-points  x-max  y-max   :xy-margin-list (list x0 win-bottom))
          ;;when draw-string
          (when draw-string
            (my-draw-string port (format nil "~A" draw-string)  str-x0 (- win-bottom str-y0)))
          ;;draw the arc (and radiuses?)
          (loop
           for xxx in new-xmpts
           for yyy in  new-ympts
           for n from 1 to (list-length new-xmpts)
           do
           (cond
            ((= n 1)
             (setf xxx0 xxx
                   yyy0 yyy))
            (t (gp:draw-line port xxx0 yyy0 xxx yyy)               
               (setf xxx0 xxx
                     yyy0 yyy)))
           (when draw-radius-p
             (gp:draw-line port center-xm center-ym  xxx yyy))
           ;;end loop
           )          
         ;;LW WAY-NOT WORK
         ;;x1 100, y1 40, x2 90, y2 80; r 62, c-xm 88, c-ym 504; ptd 42, arch 62
#|          (gp:draw-arc port center-xm center-ym (* pts-dist x-multiplier) arc-ht  
                            start-angle-radians sweep-angle-radians)|#
          ;; &rest args &key transform filled &allow-other-key

          ;;OPTIONALLY, DRAW lines             
       ;;   (when (and x1m y1m  x2m y2m draw-min-diameter-p)
         ;;   (gp:draw-line port x1m y1m  x2m y2m)) ;;(* x2T x-multiplier)(*  y2 y-multiplier)))
         ;;(break)
         ;;DRAW X & Y AXES and LABELS?
          (when draw-xyaxes-p 
            (gp:draw-line port x0 (- win-bottom y0 )  700 (- win-bottom y0 ))
            (gp:draw-line port x0 (- win-bottom y0 )  x0  60)
            (when x-label 
              (my-draw-string port  x-label  (+ x0 50)(+ (- win-bottom y0 ) 40)))
            (when y-label 
              (my-draw-string port y-label (- x0 50) (- (- win-bottom y0) 100)))
            ;;end when
            )
#|            ;;FOR THE RECTANGLE
            ;;must REVERSE SIGNS FOR Y1M BEC OF WIN Y PROBLEM
            ;;bottom line
            (gp:draw-line port x1m y1m (+ x1m  arc-ht) y1m) ;;was pts-dist
            ;;left side
            (gp:draw-line port x1m y1m  x1m  (- y1m pts-dist))  ;;was arc-ht
            ;;right side
            (gp:draw-line port (+ x1m arc-ht) y1m (+ x1m arc-ht)(- y1m pts-dist))
            ;;was  pts-dist, pts-dist,then arc-ht
            ;;top
            (gp:draw-line port  x1m (- y1m pts-dist)   (+ x1m arc-ht)(- y1m pts-dist)))   
          ;;was pts-dist, pts-dist,then arc-ht|#         
          (values new-xypts new-xmpts new-ympts 
                  x0 y0 new-y0 n-xpts  center-xm center-ym radius              
                  start-angle sweep-angle  start-angle-rads sweep-angle-rads 
                  pts-dist   len-centerline)
                  ;;end when,mvbs, let, draw-arc-from-pts
                  )))))
;;TEST
;;  (test-draw-arc 100  40  90  80 :rel-r 3.0 :rel-height 2.0 :draw-radius-p  T :draw-min-diameter-p T)
;; = ((61.824 555.812) (61.795 551.607) (61.479 547.414) (60.88 543.252) (60.0 539.141) (58.842 535.099) (57.412 531.144) (55.716 527.297) (53.764 523.573) (51.562 519.99))
;; (61.824 61.795 61.479 60.88 60.0 58.842 57.412 55.716 53.764 51.562)
;; (555.812 551.607 547.414 543.252 539.141 535.099 531.144 527.297 523.573 519.99)
;; 38.432  45.858 554.14197 10 38.432 45.858 61.846497 -0.095 0.68 -5.443 38.961 41.231 58.309
;; (test-draw-arc 100  40  90  80 :rel-r 1.3  :draw-radius-p T  T :draw-min-diameter-p T )
;;(values center-x center-y radius  pts-dist   len-centerline start-angle-radians sweep-angle-radians start-angle sweep-angle)
;; ;; (test-draw-arc 100  40  90  80 :rel-r 3.0 :draw-radius-p  T :draw-min-diameter-p T )
;; ;; (test-draw-arc 100  40  90  180 :rel-r 3.0 :rel-height 2.0 :draw-radius-p  T  :draw-radius-p T :draw-xyaxes-p T :x-label "X-axis" :y-label "Y-axis" :x-multiplier 1.5 :y-multiplier 1.5 )
;; ;; (test-draw-arc 100 200 300 300)
;;(values new-xypts new-xmpts new-ympts 
;;     x0 y0 new-y0 n-xpts  center-x center-y radius  start-angle sweep-angle start-angle-rads sweep-angle-rads  pts-dist   len-centerline)
;; = xy((78.684 876.051) (115.262 864.984) (150.045 849.978) (182.913 831.146) (213.707 808.51404) (241.494 782.768))  xm(78.684 115.262 150.045 182.913 213.707 241.494) 
;;  ym (876.051 864.984 849.978 831.146 808.51404 782.768)
;;  50 50 550 6  x0,58.579 y0,532.843 r,335.4105 start,-82.907 38.961 srads, -1.447 0.68  223.607 316.228



;; USE BELOW?
(defun test-draw-arc (x1 y1 x2 y2 &key (draw-string "TEST-DRAW-ARC")
                         (win-bottom 600)    draw-radius-p draw-xyaxes-p x-label y-label
                         (rel-r 3.0)  arc-ht  (rel-height .3) (rel-r 3.0)
                         (n-pts 6) radius 
                         (x-multiplier 1.0)(y-multiplier 1.0)  )
  "U-GP-utilities RETURNS (values (list center-x center-y) radius  pts-dist   len-centerline start-angle-radians sweep-angle-radians start-angle sweep-angle)"
 ;;(x-radius y-radius width height &key (x 200) (y 200)       filled (foreground :black))
  (let ((inst (make-instance 'drawtest-interface-NO-CALLBACK)) ;; in U-function-plotter
        )
    ;;DISPLAY INST FIRST?
      (capi:display inst)

    (with-slots (output-pane-1) inst   
      (draw-arc-from-pts output-pane-1 x1 y1 x2 y2
                          :win-bottom win-bottom :draw-string draw-string :rel-r rel-r 
                           :draw-xyaxes-p draw-xyaxes-p :x-label x-label :y-label y-label
                         :radius radius :arc-ht arc-ht :rel-height rel-height
                         :n-pts n-pts  :rel-r rel-r   :rel-height rel-height
                         :x0 50 :y0 50 :win-bottom 600 :radius2 NIL
                         :x-multiplier x-multiplier  :y-multiplier y-multiplier
                         :draw-radius-p draw-radius-p  )

#|      (gp:draw-ellipse output-pane-1 x y x-radius y-radius   
       :filled filled
       :foreground foreground)  ;;foreground was   (if (> x-radius y-radius) :red  :yellow))|#
      )))
;;TEST
;; (test-draw-arc 100  40  90  80 :rel-r 3.0 :rel-height 2.0 :draw-radius-p  T :draw-min-diameter-p T)
;; 38.432  45.858  61.846497  41.231 58.309 -0.095 0.68 -5.443 38.961
;; (test-draw-arc 100  40  90  80  :draw-radius-p  T :draw-min-diameter-p T )
;;(values center-x center-y radius  pts-dist   len-centerline start-angle-radians sweep-angle-radians start-angle sweep-angle)
;;= x78.386  y55.847 rad26.80   41.231 17.125  strad -0.633  1.756  sta -36.268 100.611
;; 7777
;; (test-draw-arc 100   40  90  80)
;;
;; (test-draw-arc 100   40  90  80 :x-multiplier 5.0 :y-multiplier 5.0 :draw-radius-p  T :draw-min-diameter-p T  :rel-height 0.2)
;; =(391.93378 279.23343)   134.0009  206.15525  85.622696
;;     -0.63265765  1.7552725  -36.248614119864435D0   100.56971128993598D0




;;TRANSFORM-DRAW-AXES-ON-XYPOINT-LISTS
;;2018
;;ddd
(defun transform-draw-axes-on-xypoint-lists (xy-point-lists  &key (x0 0) (y0 0) 
                                                             (win-bottom 600) (x-multiplier 1.0) (y-multiplier 1.0))
  "U-GP-utilities.  RETURNS (values new-xypts new-xmpts new-ympts new-xpts   new-ypts   x0 y0 new-y0 n-xpts n-ypts )   INPUT: XPTS,YPTS=rel to math xy-coord x=0,y=0.  X0 & Y0= adjust from window bottom/left.  WIN-BOTTOM loc in window of x-axis from top of window. NOTE: ALL Y ADD/SUBT OPERATIONS MUST REVERSE THE SIGN OF Y."
  (let
      ((xpts)
       (ypts)
       (new-xypts)
       )
    ;;SPLIT XY-POINTS INTO SEPARATE Xpts AND YPTS 
    ;;BEC  Y PTS MAY START FROM BOTTOM INSTEAD OF TOP 
    (loop
     for xypt in xy-point-lists
     do
     (let*
         ((x (car xypt))
          (y (second xypt))
          )
       (setf xpts (append xpts (list x))
             ypts (append ypts (list y)))
     ;;end let,loop
     ))
    (multiple-value-bind (new-xmpts new-ympts  new-xpts   new-ypts  
                                    x0 y0 new-y0 n-xpts n-ypts )
        (transform-draw-axes xpts ypts  :win-bottom  win-bottom
                             :x0 x0 :y0 y0  :x-multiplier x-multiplier :y-multiplier y-multiplier)
      ;; PUT LISTS BACK TOGETHER
      (loop
       for x in new-xmpts
       for y in new-ympts
       do
         (setf new-xypts (append new-xypts (list (list x y))))
         ;;end ,loop
         )
      ;;(break "end transform-draw-axes-on-xypoint-lists")
  (values new-xypts new-xmpts new-ympts  new-xpts new-ypts   x0 y0 new-y0 n-xpts n-ypts)
    ;;end let, transform-draw-axes-on-xypoint-lists
    )))
;;TEST
;; (transform-draw-axes-on-xypoint-lists  '((10 25)(20 35)(40 45))  :x0 50 :y0 50)
;; works?= ((10.0 525.0) (20.0 515.0) (40.0 505.0))  (10.0 20.0 40.0)  (525.0 515.0 505.0)  (60.0 70.0 90.0)  (525 515 505)  50  50 550 3 3
;; (transform-draw-axes-on-xypoint-lists  '((10 25)(20 35)(40 45))  :x0 50 :y0 50 :x-multiplier 2.0 :y-multiplier 2.0)
;;= ((20.0 500.0) (40.0 480.0) (80.0 460.0))  (20.0 40.0 80.0)  (500.0 480.0 460.0)  (70.0 90.0 130.0)  (525 515 505)  50 50 550 3 3
;; (transform-draw-axes-on-xypoint-lists '(( 100  40)(  90  80))  :x0 38 :y0 46)
;;=  ((100.0 514.0) (90.0 474.0)) (100.0 90.0) (514.0 474.0) (138.0 128.0) (514 474) 38  46 554 2  2




;;TRANSFORM-DRAW-AXES
;;2018
;;ddd
(defun transform-draw-axes (x-pts y-pts  &key (x0 0) (y0 0) (win-bottom 600)
                                  (x-multiplier 1.0) (y-multiplier 1.0))
  "U-GP-utilities.  RETURNS (values new-xmpts new-ympts new-xpts   new-ypts   x0 y0 new-y0 n-xpts n-ypts )   INPUT: X-PTS,Y-PTS=rel to math xy-coord x=0,y=0.  X0 & Y0= adjust from window bottom/left.  WIN-BOTTOM loc in window of x-axis from top of window. NOTE: ALL Y ADD/SUBT OPERATIONS MUST REVERSE THE SIGN OF Y.
   NOTE:  WIN-BOTTOM is bottom (eg 600) If y0=50,  X-AXIS is at 550."
  (let
      ((new-xpts)
       (new-ypts)
       (new-xypts)
       (new-xmpts)
       (new-ympts)
       (orig-x0 x0)
       (orig-y0 y0)
       (new-y0 0)
       (n-xpts (list-length x-pts))
       (n-ypts (list-length y-pts))
       )
    ;; MULTIPLIERS NOT EQUAL?
    (when  (not (= x-multiplier y-multiplier))
      (setf  xy-multiplier  (sqrt (+ (expt x-multiplier 2.0)(expt x-multiplier 2.0)))))
    (when win-bottom
      (setf  new-y0 (- win-bottom y0)))

    ;;FOR X-PTS
    (loop
     for x in x-pts
     do
     (let*
         ((new-x x)
          (new-xm)
          )
       ;;MULTIPLIER (multiply BEFORE adjusting location
       (setf new-xm (* x x-multiplier))
       ;;ADJUST X FOR NEW AXIS
       (setf new-x (+ new-xm x0)
             new-xpts (append new-xpts (list new-x))
             new-xmpts (append new-xmpts (list  new-xm)))
       ;;end let,loop
       ))
    ;;FOR Y-PTS
    (loop
     for  y in y-pts
     do
     (let*
         ((new-y)
          (new-ym)
          )     
       #|     (cond
      ;;Must SUBTRACT if win-bottom
      (win-bottom
       (setf new-y (- ypt y0)))
      (t (setf new-y (+ ypt y0))))|#
       ;;MULTIPLIER
       (setf new-ym (* y y-multiplier))

       ;;ADJUST Y FOR NEW AXISES
       (setf new-y (+ y y0)
             new-ym (+ new-ym y0))
       ;;windows top y to bottom y adjustment     
       (when win-bottom
         (setf new-y (- win-bottom new-y)
               new-ym (- win-bottom new-ym)))
       ;;append
       (setf new-ypts (append new-ypts (list new-y))
             new-ympts (append new-ympts (list  new-ym)))
       ;;end let,loop
       ))
    (values new-xmpts new-ympts new-xpts   new-ypts   x0 y0 new-y0 n-xpts n-ypts )
    ;;end let, transform-draw-axes
    ))
;;TEST
;;  (transform-draw-axes '(20 30 40 50) '(35 45 55 65)) 
;;=(565 555 545 535) (565.0 555.0 545.0 535.0)  (20.0 30.0 40.0 50.0)  (20.0 30.0 40.0 50.0) 0 0 600 4 4
;; new-xpts  new-xmpts new-ypts  new-ympts x0 y0 n-xpts n-ypts )
;; (transform-draw-axes '(20 30 40 50) '(35 45 55 65)  :x0 100 :y0 100)
;;works=(120.0 130.0 140.0 150.0) (20.0 30.0 40.0 50.0)  (465 455 445 435) (465.0 455.0 445.0 435.0) 100 100 500 4 4
;;(transform-draw-axes '(20 30 40 50) '(35 45 55 65)  :x0 100 :y0 100 :x-multiplier 1.5 :y-multiplier 1.5)
;; = (130.0 145.0 160.0 175.0) (30.0 45.0 60.0 75.0) (465 455 445 435) (447.5 432.5 417.5 402.5) 100 100 500 4  4







#|
;;xxx === HELP:  CALC CIRCLE CENTER FROM 2 PTS AND RADIUS =======
;;
FROM https://math.stackexchange.com/questions/1781438/finding-the-center-of-a-circle-given-two-points-and-a-radius-algebraically
;;GIVEN
1.  CIRCLE EQUATIONS;RADII ARE SAME;
    (x - x1)^2  +  (y -  y1)^2 = r^2
    (x - x2)^2  +  (y - y2)^2 = r^2
2. Subtract the equations (and expand the squared binomials):
[x^2-2(x1)(x)+x1^2 +y^2 -2(y1)(y) +y1^2]-
 [x^2-2(X2)(x)+X2^2 +y^2 -2(y2)(y) +y2^2] = 0
------------------------------------------------------------------
-2(x1)(x)-2(y1)(y)+2(x2)(x)+2(y2)(y)+x1^2+y21-x2^2-y2^2=0
simplify:
-2x(x1-x2)-2y(y1-y2)+x1^2+y1^2-x2^2-y2^2=0
Let c=x1^2+y1^2-x2^2-y2^2
Let x3=x1-x2;; where x3,y3 is center of line betw 2 points
Let y3=y1-y2
from above substituting constants:
2y(y3)=-2x(x3)+c
y=(-x3/y3)(x)+(1/2y3)(c)
substitute y into previous eq:
(x-x1)2+[(-x3/y3)(x) + (1/2y3)(c)  -y1)]^2= r^2
Let cy = [1/(2)(y3)] (c)  -y1
(x-x1)2+ [(-x3/y3 (x) + cy)]^2=r^2
(x^2-2(x1)(x)+x1^2)+[(x3/y3) (x)]^2+ 2(cy)(-x3y/3) (x)+cy^2)=r^2
(x^2+(x3/y3 * x)^2)+ (-2(x1)(x)+2(cy)(-x3/y3 * x))+(x1^2+cy^2-r^2)=0
Above is quadratic eq. Smplifing ax^2, bx, and c2:
skipped a long step here
FINAL ANSWER:
 CENTER POINT:
  X = [-b+/- (sqrt (b^2 - 4ac2] / 2a 
  Y = (-x3/y3 * x) + (1/2y3 * c)
NOT EXACTLY RIGHT?


2
down vote
Here's a completely different approach that might be easier to encode.
;;In this example X, Y IS THE UNKNOWN CIRCLE CENTER
;;(note: I changed from original where x,y was circle center and x3,y3 was midpt between 2 pts.)
Your two given points ((x1,y1) and (x2,y2)) and the centers of the two desired circles are at the four vertices of a rhombus with side length r. You can use the Pythagorean Theorem to find the length of the diagonal of the rhombus from (x1,y1) to (x2,y2). Better still, divide by two so you now have half the length of the diagonal. The two diagonals of a rhombus are perpendicular, so the point (x1,y1), the center of the rhombus, and one of the circles' centers make a right triangle with hypotenuse r and one leg equal to half the known diagonal. Use the Pythagorean Theorem to find half the length of the other diagonal.

Now you just need to construct line segments of that length with one end at the center of the rhombus, perpendicular to the known diagonal, and the other end of each segment will be the center of one of the desired circles.

That's the entire rationale of the procedure. For the detailed calculations, I'll assign names to various lengths as we go along in order to keep the equations from getting too ugly. (You'd probably want to do this anyway when you do this in software.)

Let xa=1/2(x2-x1) and ya=1/2(y2-y1); then the center of the rhombus is at (x3,y3)=(x1+xa,y1+ya) and half the length of the known diagonal is a=xa^2+ya^2. 
The length of the other diagonal is b=r^2 -a^2.
Now suppose (for example) that (x3,y3) is above and to the right of (x1,y1) (assuming, at least for now, the usual mathematical Cartesian x,y coordinates). To get from (x1,y1) to (x3,y3) along a straight line, we go to the right xa units and up ya units for every a units of movement. So to get from (x3,y3) to the center of one of the circles, (x,y), we can just turn those rules 90 degrees: we can go down xa units and to the right ya units for every a units of movement; that is, XAA DOWN AND YAA RIGHT FOR EACH UNIT TRAVELED along the diagonal. But the distance from (x3,y3) to (x,y) is b, so we will end up going down bxaa units and to the right byaa units. In other words,
  ANSWER:
    X = x3 +bya/a ;;  ya=1/2(y2-y1) 
    Y = y3 -bxa/a  ;; xa=1/2(x2-x1) 
      ;;Where:  a = min-radius length and  b =  length [x3y3 to xy] nearside
To get to the center of the other circle, (x4,y4), we just go the same amount in the opposite direction from (x3,y3):
    X = x3 - bya/a
    Y = y3 +bxa/a
If (x3,y3) is not above and to the right of (x1,y1), or if you're working in a graphics coordinate system with y downward, or both, the signs of either xa or ya (or both) might be negative instead of positive; but all the formulas above continue to work exactly the same way as before.
[Where you have "y=-xy+12c", you should have "y=-xy+12yc". There may be more errors, but I stopped there since this omission is propagated subsequently so is likely not a typographical omission.]

;;CIRCULAR ARCS BETW 2 POINTS (x1 y1) (x2 y2)
;;FACTS
;; DISTANCE = (sqrt (+ (expt (- x2 x1) 2.0) (expt (- y2 y1) 2.0))))
;; MIN RADIUS = (* 0.5 DISTANCE)  MAX-RADIUS = INFINITY
;; CENTER lies on line perpendicular to line connecting the 2 points.
;;  a. That line divides triangle betw 2 pts and center into 2 right triangles--the hypotenuses are = the circle radius. The length of line across from the center is (* 0.5 DISTANCE).
;;  b. The 3rd side distance to center is ARBITRARY AND WILL DETERMINE THE ARC curvature.
;;  c. Therefore, 1. find the distance betw 2 pts; 2. SET ARBITRARY LENGTH; 3. Calc HYPOTENUSE = CIRCLE RADIUS; 4. CALC-CENTER X,Y -- see below
;; ANGLE betw 2 points at center can vary from  0 > ANGLE <= 180 degrees.
;; If draw arcs from each pt with radii = arc radius, they will INTERSECT AT CENTER.

2. DISTANCE BETW PTS
    DIST-PTS =  (sqrt  (x2 - x1)^2  +  (y2 -  y1)^2 )
    x3 (center of connecting-line) = x3 = (x2 + x1)/2;  y3 = (y2 + y1)/2
3. LEN ACROSS SIDE of triangle =  DIST-PTS
    Min radius = DIST-PTS/2
    Len  HYPOTENUSE = RADIUS therefore LEN-NEARSIDE^2 = LEN-X3Y3-XY^2 =  (hypt^2 - acr-side^2)







;;XXX THIS VERSION USES GP:DRAW-ARC WHICH SEEMS ALMOST UNMANAGEABLE TRYING TO DRAW AN ARC BETW 2 POINTS
NOTE:  If change radius of the arc, it changes the angle betw the points!! which is fundamental for gp:draw-arc
(defun draw-arc-from-pts (port x1 y1 &optional x2 y2
                               &key (rel-r 3.0) radius   arc-ht  (rel-height 0.5)
                               (x0 50)(y0 50)(win-bottom 600)
                               (x-multiplier 1.0)(y-multiplier 1.0) 
                               draw-radius-p draw-min-diameter-p)
  "U-GP-utilities RETURNS (values  center-x center-y radius  pts-dist   len-centerline start-angle-radians sweep-angle-radians start-angle sweep-angle)   INPUT:    REL-HEIGHT= arc-ht/radius. If height provided--not used. x-multiplier and y-multiplier multiply values of x2 y2 and radius by xy-multiplier."
  (let
      ((xy-multiplier x-multiplier)
       (x1m)
       (y1m)
       (x2m)
       (y2m)
       (orig-x1 x1)
       (orig-y1 y1)
       (orig-x2 x2)
       (orig-y2 y2)
       (center-xm)
       (center-ym)
       (new-xpts)
       (new-xmpts)
       (new-ypts)
       (new-ympts)
       (new-y0)
       (n-xpts)
       (n-ypts)
       )
#|    ;;ADJUST X & Y FOR NEW AXISES?
    (unless (= x0 0)
      (setf x1 (+ x1 x0))
      (when x2
        (setf  x2 (+ x1 x0)))
            x2 (+ x2 x0))
    (unless (= y0 0)
      (setf y1 (+ y1 y0))
      (when x2
        (setf  y2 (+ y2 y0))))            
    (when win-bottom
      (setf y0 (- win-bottom y0)
            y1 (- win-bottom y1))
      (when y2
        (setf  y2 (- win-bottom y2))))

     ;; MULTIPLIERS NOT EQUAL?
    (when (not (= x-multiplier y-multiplier))
          (setf  xy-multiplier  (sqrt (+ (expt x-multiplier 2.0)(expt x-multiplier 2.0)))))
    (setf x1m (* x1 x-multiplier)
          y1m (* y1 y-multiplier)
          x2m (* x2 x-multiplier)
          y2m (* y2 y-multiplier))|#

    ;;1-FIND THE CIR CENTER(S)
    ;;SSSS CHECK TO SEE IF WHEN ENDS IN RIGHT PLACE (END OF DEFUN)
      (when (and x2 y2)
        ;;was (when (and x2m y2m)
        (multiple-value-bind (center-x center-y xx yy radius1
                                       pts-dist  pts-line-direction  x3 y3 centerline-dir  len-centerline)
            (calc-center-from-pts x1 y1 x2 y2  :rel-r rel-r :radius radius)
                      ;;was (calc-center-from-pts x1m y1m x2m y2m  :rel-r rel-r :radius radius)
        (unless radius
          (setf radius radius1)) ;; (* radius1 xy-multiplier)))
        (unless arc-ht
          (setf arc-ht (* rel-height radius)))
        ;;(break "center-x")

        ;;2-CONVERT 2 PTS TO RADIANS--calc start-angle and sweep-angle
        (multiple-value-bind (sweep-angle-radians start-angle-radians sweep-angle
                                                  start-angle) ;; center-x center-y center-xx center-yy)
            (calc-radians-from-2pts  x1 y1 x2 y2 :radius  radius)
            ;;(calc-radians-from-2pts  x1m y1m x2m y2m :radius  radius) ;;  :center-pt center-pt) ;; :radius  radius
          ;; len-centerline (* len-centerline xy-multiplier)
          ;; pts-dist (* pts-dist xy-multiplier))


          ;;SSSS WHERE TO PUT THIS??  JUST BEFORE CAPI DRAW FUNCTIONS?
          ;; OR MAKE MY FUNCTION & PUT INSIDE OF IT??
          (multiple-value-setq (new-xpts  new-xmpts new-ypts  new-ympts x0 y0 new-y0 
                                          n-xpts n-ypts)
              (transform-draw-axes  (list x1 x2 center-x) (list y1 y2 center-y) 
                                     win-bottom
                                    :x0 x0 :y0 y0 :x-multiplier x-multiplier :y-multiplier y-multiplier))
          (setf x1m (car new-xmpts)
                x2m (second new-xmpts)
                y1m (car new-ympts)
                y2m (second new-ympts)
                center-xm (third new-xmpts)
                center-ym (third new-ympts))        
          ;;END OF CONFUSED PART?         

          ;;3-DRAW THE ARC
          (break "x1m etc") 
         ;;x1 100, y1 40, x2 90, y2 80; r 62, c-xm 88, c-ym 504; ptd 42, arch 62
          (gp:draw-arc port center-xm center-ym  (* 62 x-multiplier) 62 ;;(- 600 130) 
                       start-angle-radians 60) ;; sweep-angle-radians)
#|          (gp:draw-arc port center-xm center-ym (* pts-dist x-multiplier) arc-ht  
                            start-angle-radians sweep-angle-radians)|#
          ;; &rest args &key transform filled &allow-other-key
          ;;OPTIONALLY, DRAW lines             
          (when draw-min-diameter-p
            (gp:draw-line port x1m y1m  x2m y2m)) ;;(* x2T x-multiplier)(*  y2 y-multiplier)))
         ;;(break)
          (when draw-radius-p
            ;;(gp:draw-line port center-x center-y x1m y1m)
            ;;(gp:draw-line port center-x center-y x2m y2m)
            ;;FOR THE RECTANGLE
            ;;must REVERSE SIGNS FOR Y1M BEC OF WIN Y PROBLEM
            ;;bottom line
            (gp:draw-line port x1m y1m (+ x1m  arc-ht) y1m) ;;was pts-dist
            ;;left side
            (gp:draw-line port x1m y1m  x1m  (- y1m pts-dist))  ;;was arc-ht
            ;;right side
            (gp:draw-line port (+ x1m arc-ht) y1m (+ x1m arc-ht)(- y1m pts-dist))
            ;;was  pts-dist, pts-dist,then arc-ht
            ;;top
            (gp:draw-line port  x1m (- y1m pts-dist)   (+ x1m arc-ht)(- y1m pts-dist)))   
          ;;was pts-dist, pts-dist,then arc-ht

          ;;REVISE VALUES? 7777
          (values center-x center-y radius  pts-dist   len-centerline                 
                  start-angle-radians sweep-angle-radians start-angle sweep-angle)
          ;;end when,mvbs, let, draw-arc-from-pts
          )))))
|#