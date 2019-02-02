;;************************ U-function-plotter.lisp *************************
;;

;;ALSO SEE U-GP-utilities.lisp  for MY-DRAW-CURVE (uses math functions to draw a curve--good for GRAPHS.

;; MY BASIC POINT ID LIST
;; output-list =  ((time0  (x1 x2 ... xn) "label" "extra stuff goes here") (time1 (x1 x2 .. xn) etc)) 
;;EG. ((1 (40 0.7) "Input1" (1)) (2 (80 -0.067839995) "Input2" (2)) (3 (120 0.05008) "Input3" (3)) (4 (160 1.04512) "Input4" (4)) (5 (200 0.97264) "Input5" (5))...)
;; (draw-graph-window nil '( (2 (80 -0.067839995) "Input2" (2)) (3 (120 0.05008) "Input3" (3)) (4 (160 1.04512) "Input4" (4)) (5 (200 0.97264) "Input5" (5)) (6 (240 1.03376) "Input6" (6)) (7 (280 0.023199998) "Input7" (7)) (8 (320 -0.050559998) "Input8" (8)) (9 (360 0.047519997) "Input9" (9)))   400 400 100 100 :y-multiplier 100 :x-string "  This is X String  " :y-string " y string vertically")




;;DRAW-EQUATIONS-CURVES
;;2018
;;ddd
(defun draw-equations-curves  (calc-eq-arglists &key (find-points-xn 1) (call-func-p T) 
                               makunbound-func-p  (return-output-p T)                           
                               (interface-class 'interface-display) (output-pane 'output-pane-1)
                               ;;(output-pane 'output-pane-1) pane MUST BE output-pane-1
                               (x-max 600) (y-max 600) (x-margin 100) (y-margin 100)
                               (multiple-graphs-p T) append-point-list
                               begin-num (label-margin 10) 
                               (graph-color :red) 
                               (graph-color-list '(:red :blue :orange :green :black :yellow 
                                                   :lightblue :magenta :pink  :gray :violet))
                               (x-mark-interval 20)  (y-mark-interval 20)  (half-hash-length 3)
                               (label-margin 10)
                               x-label-color  y-label-color 
                               x-label-list  y-label-list
                               (x-label-type :point-label)
                               (x-increment 10)   (y-increment 10)
                               x-string y-string ( x-string-margin 35) (y-string-margin 30)
                               (y-element-space 20) 
                               (x-font-list '(:family "Times New Roman" :size 10 :weight :normal)) 
                               (y-font-list '(:family "Times New Roman" :size 10 :weight :normal))
                               (x-string-color :black) (y-string-color :black)
                               (center-x-string-p T) (center-y-string-p T)
                               (x-multiplier 1.0) (y-multiplier 1.0)   y-floor-value)
  "U-function-plotter. REVISE--COPIED FROM BELOW-- INPUT: CALC-EQS-ARGLISTS one for each equation. Each EQ-ARGLIST a list of (func-sym equation varlist value-list0 delta-items n-cycles cycle-labels eq-label find-points-xn) 
  DELTA-ITEMS: (listp delta-val), SPECIAL CASES: PREV-Y; ADD-DELTA= delta (+ prev-delta (second delta-val)) EG:(add-deltas +delta  delta0), (+  delta val))); MULT-DELTA-VAL= (+  (* delta val) (third delta-val)); MULT-DELTA=(+  (* delta val) (third delta-val)) val) EG, (mult-delta *delta  delta0); ADD-EXPDELTA=(+  (expt prev-delta exp) (third delta-val)) EG,(add-expdelta exp +c  delta0) & DEFAULT DELTA=number=> (+ val delta)   
 RESULTS: (values all-eq-outputs all-points-lists n-eqs)  
ALL-EQ-OUTPUTS= list of all eq-output-lists. 
  Each EQ-OUTPUT-LIST=(y-list eq-outputs eq-varlist eq-n-cycles func-sym eq-xy-points).  CYCLE-OUTPUTS = list of cycle-output-lists. 
  Each CYCLE-OUTPUT-LIST= (y varvals delta-vals xy-pt cyclen cycle-label)  
  GRAPH POINTS:  y-list is the y-axis output.  FIND-POINTS-XN= either integer for nth location of x in the varlist of each output.  If (listp find-points-xn) finds more points--for 3 or more dim coords.  NOTES: 1.Interface pane MUST BE output-pane-1"
  (let*
      ((n-pt-lists) 
       (all-eq-outputs)
       (all-points-lists)
       (n-eqs)
       (interface-inst)
       (port)
       )
    ;;STEP 1: CALC ALL POINTS FROM ALL EQUATIONS
    (multiple-value-setq (all-eq-outputs all-points-lists n-eqs)
        (calc-equations-for-delta-varlists calc-eq-arglists 
                                           :call-func-p T :makunbound-func-p NIL))

    ;;Point-list egs.= ((1 (2 5)(1 .8) "") (2 (4 6)(2 .7) "")) &  ((1 (1 5 3)(2 2 .6)"pt1") (2 (3 6 5)(2 2 .5)) "pt2"))   
    ;;in above cases (x y)  and (x y z)
    ;;(setf **temp-pts all-points-lists)
    ;;(break "step 2")

    ;;STEP 2: DRAW THE GRAPHS FROM ALL-POINTS-LISTS
    ;;PLOT EQUATION RESULT POINTS

    ;;STEP 2.1: WRITE A DISPLAY-CALLBACK FUNCTION graph-display-callback
    ;; and use it to run draw-graph-window
    (eval
     `(make-callback-to-run-function graph-display-callback (port x y width height)
             (draw-graph-window port (quote ,all-points-lists)
                                ,x-max ,y-max ,x-margin ,y-margin 
                                :multiple-graphs-p ,multiple-graphs-p  :append-point-list ,append-point-list
                                :begin-num ,begin-num :label-margin ,label-margin
                                :graph-color ,graph-color :graph-color-list (quote ,graph-color-list)
                                :x-mark-interval ,x-mark-interval :y-mark-interval ,y-mark-interval
                                :half-hash-length ,half-hash-length :label-margin ,label-margin
                                :x-label-list (quote ,x-label-list) :y-label-list  (quote ,y-label-list)
                                :x-label-color ,x-label-color :y-label-color ,y-label-color 
                                :x-label-type ,x-label-type
                                :x-increment ,x-increment :y-increment ,y-increment
                                :x-string ,x-string :y-string ,y-string :x-string-margin ,x-string-margin
                                :y-string-margin ,y-string-margin :y-element-space ,y-element-space
                                :x-font-list (quote ,x-font-list) :y-font-list (quote ,y-font-list)
                                :x-string-color ,x-string-color :y-string-color ,y-string-color
                                :center-x-string-p ,center-x-string-p   :center-y-string-p ,center-y-string-p 
                                :x-multiplier ,x-multiplier :y-multiplier ,y-multiplier
                                :y-floor-value  ,y-floor-value)  
    :eval-inner-func-p NIL :makunbound-func-p NIL )
             ;;end eval
             )
    ;;STEP 2.2:  MAKE INTERFACE INSTANCE AND DISPLAY IT
    (setf interface-inst (make-instance interface-class)
                    port output-pane)
         (capi:display interface-inst)

    (when return-output-p
      (values all-eq-outputs all-points-lists n-eqs))
    ;;end let, draw-equations-curves
    ))
;;TEST
;; SSS START HERE TESTING
;; (draw-equations-curves  '((arcline  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1 3 .8  2) (2 2  0  prev-y) 10 ("cyc1" "cyc2")  "Eq1" 1)) :multiple-graphs-p T  :y-multiplier 30 :x-multiplier 30  :x-string "  This is X String  " :y-string " y string vertically" )
;; works
;; (draw-equations-curves  '((arcline  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1 3 .8  2) (2 2  1.0  prev-y) 10 ("cyc1" "cyc2")  "Eq1" 1)  (arcline2  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1 3 .8  2) (2 2  (add-expdelta 1.5 0 1.0)  prev-y) 10 ("cyc1" "cyc2")  "Eq2" 1))   :multiple-graphs-p T  :y-multiplier 10 :x-multiplier 10  :x-string "  This is X String  " :y-string " y string vertically") ;; :return-output-p NIL)
;; WORKS-- 2 graphs, 1 window, & renews on minimize








;;CALC-EQUATION-FOR-VARLISTS
;;2018
;;ddd
(defmacro calc-equation-for-varlists (func-sym equation varlist value-lists
                                               &key eq-label  cycle-labels (call-func-p T) 
                                               makunbound-func-p)
  "U-function-plotter   RETURNS (values y-list  n-value-lists func-sym varlist) y-list is a list of  y results for calling the new function FUNC-SYM  on EACH of the value-lists.   INPUT: Equation must be in form of a backquoted list function with its variables MATCHING the SYMs in the arg-list (eg. if x1,x2 in equation, must have varlist= (x1 x2). The value-lists The varlist and value-lists must be UNQUOTED. eg '(my-test-eq1 x  2  3) OR   lisp expression Eg (quote `(+ (* 3 ,x (+ 4 ,x))  7)), NOTE:NO QUOTE FOR FUNC-SYM, BACKQUOTE,COMAS FOR EQUATION. Also, length of each value-list must be same as varlist. Eg. (x1 x2 x3) & ((1 2 3)(2 4 6)). 
 FOR REPEAT CALLS, use FUNC-SYM as function call sym with varlist value-lists (each QUOTED). If (null call-func-p), then won't eval func-sym function. WARNING: DEFINES VARLIST VARS AS GLOBAL VARS--you MUST use those SAME VARS IN VARLIST. However it also makes those vars (eg x1 x2) UNBOUND at end."
  ;;STEP 1: WRITE A NEW FUNCTION
  (eval ;; (setf *mactest ;; 
  `(defun ,func-sym (varlist value-lists &optional cycle-labels)
  (let*
      ((y-list)
       (n-value-lists 0)
       (all-outcomes)
       )
    (unless ,eq-label
      (setf eq-label (format nil "~A" (quote ,equation))))

    (loop
     for value-list in value-lists
     do
     (incf n-value-lists)
     (unless (listp value-list)
       (setf value-list (list value-list)))  
     (let*
        ((testval)
         (outcome-list)
         (y)
         )
       (loop
        for value in value-list
        for var in varlist
        do
        (set var value)
         (setf testval (eval var))
        ;;end let,loop
        )
       (setf  y  (eval ,equation)
              y-list (append y-list (list y)))
       (cond
        (,cycle-labels
              (setf outcome-list (list y value-list n-value-lists (nth (- n-value-lists 1) ,cycle-labels))))
        (t (setf outcome-list (list y value-list  n-value-lists))))
       (setf all-outcomes (append all-outcomes (list outcome-list)))
       ;;end let,loop
       ))
    ;;MAKUNBOUND GLOBAL VARS & FUNC-SYM FUNCTION?
    (makunbound-vars varlist)
    (when ,makunbound-func-p (makunbound func-sym))

    (values y-list all-outcomes  n-value-lists (quote ,func-sym) (quote ,varlist)  eq-label)
    ;;end let, func-sym defun
    ))
  ;;end eval
  )
 ;;STEP 2: FILL IN NEW FUNCTION WITH ARGS AND EVAL
 (when call-func-p
   ;;(multiple-value-bind (y-list1 nvals func-sym1 varlist1)
     `(,func-sym (quote ,varlist) (quote ,value-lists)))

 ;;END CALC-EQUATION-FOR-VARLISTS MACRO
 )
;;TEST
;; (calc-equation-for-varlists myeqtestfun `(+ ,a ,b ,c) (a b c) ((1 2 3)(4 5 6)) :call-func-p T)
;; works = (6 15)   ((6 (1 2 3) 1) (15 (4 5 6) 2))  2  MYEQTESTFUN  (A B C)  "(BQ-LIST (QUOTE +) A B C)"
;; ALSO
;; (MYEQTESTFUN '(a b c) '((1 1 1)(2 2 2)))
;; works= (3 6)   ((3 (1 1 1) 1) (6 (2 2 2) 2))  2  MYEQTESTFUN  (A B C)  "(BQ-LIST (QUOTE +) A B C)"
;; FOR CYCLE LABELS
;; (calc-equation-for-varlists myeqtestfun `(+ ,a ,b ,c) (a b c) ((1 2 3)(4 5 6)) :CYCLE-LABELS  '("first cycle" "2nd cycle")  :call-func-p T)
;;works= (6 15)   ((6 (1 2 3) 1 "first cycle") (15 (4 5 6) 2 "2nd cycle"))  2  MYEQTESTFUN  (A B C)  "(BQ-LIST (QUOTE +) A B C)"
;;
;;FOR A CIRCLE?
;; (calc-equation-for-varlists arc-equation `(- (/ (- ,x2 ,x1) ,dx/dy) ,y1) (x1 x2 y1 dx/dy)((1  3  1 0.8)))
;; works= (1.5)  1 ARC-EQUATION (X1 X2 Y1 DX/DY)
;; CHANGE dx/dy at a constant rate (acceleration) (* 0.6 dx/dy) (* 0.6 0.8) = 0.48
;; x2 is unknown or arbitrary?? try same dif as before= 2 ;;NO-X MUST BEGIN TO DECREASE AT SOME POINT  SSSSS START HERE WORK ON X1
;; (ARC-EQUATION '(x1 x2 y1 dx/dy) '((3 5 1.5 .48)))
;;works=  (2.666667)  1  ARC-EQUATION (X1 X2 Y1 DX/DY)
;; (ARC-EQUATION '(x1 x2 y1 dx/dy) '((5 7 2.7 .29)))   ;; (* .48 .6) = 0.29
;; 
;;pts so far (1 1)(3 1.5)(5 2.7)

;; (calc-equation-for-varlists arclineX  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) ((1 3 .8  2) (2 4 .6 1))) 
;;works= (3.6 2.2)  2  ARCLINEX  (X1 X2 DY/DX Y1)
;; (arclineX '(x1 x2 dy/dx y1) '((3 5 .6 2)))
;; works= (3.2)  1  ARCLINEX  (X1 X2 DY/DX Y1)

;; DOES NOT WORK,  (l m n) DOESN'T WORK -- gets an unbound var error for a b c
;;  MYEQTESTFUN USES OLD a,b,c values in the equation (they are global) 
;; (MYEQTESTFUN '(l m n) '((1 1 1)(2 2 2)))
;; (calc-equation-for-varlists myeqtestfun2
;;USING THE (SETF *MACTEST instead of (eval ... at beginning
;; (calc-equation-for-varlists myeqtestfun `(+ ,a ,b ,c) (a b c) ((1 2 3)(4 5 6)) :call-func-p NIL)
#|
;; slightly modified above since pprint below
;;(pprint *mactest) = 
(DEFUN MYEQTESTFUN (VARLIST VALUE-LISTS)
  (LET* ((Y-LIST) (N-VALUE-LISTS 0))
    (LOOP FOR VALUE-LIST IN VALUE-LISTS
          DO (INCF N-VALUE-LISTS)
             (UNLESS (LISTP VALUE-LIST)
               (SETF VALUE-LIST (LIST VALUE-LIST)))
             (LET* ((TESTVAL))
               (LOOP FOR VALUE IN VALUE-LIST
                     FOR VAR IN VARLIST
                     DO (SET VAR VALUE)(SETF TESTVAL (EVAL VAR)))
               (BREAK "1")
               (SETF Y (EVAL `(+ ,A ,B ,C))
                     Y-LIST (APPEND Y-LIST (LIST Y)))))
    (VALUES Y-LIST N-VALUE-LISTS 'MYEQTESTFUN)))
|#





;;CALC-EQUATION-FOR-DELTA-VARLISTS
;;2018
;;ddd
(defun calc-equation-for-delta-varlists (func-sym equation varlist value-list0 
                                                  delta-items n-cycles
                                                  &key (find-points-xn 1) cycle-labels
                                                  (call-func-p T) makunbound-func-p)
  "U-function-plotter  INPUT: All args must be quoted if actual syms, lists. EQUATION must be in form (quote, backquote and comma with each var. RETURNS (values y-list all-outputs varlist n-vars func-sym xy-points) y-list is a list of  y results for each cycle; all-outputs is (y new-values) for each cycle. For calling the new function FUNC-SYM  on EACH of the value-lists.  When FIND-POINTS-XN= integer, returns xy-points [y= (car output) x= (nth integer)].
  DELTA-ITEMS: (listp delta-val), SPECIAL CASES: PREV-Y; ADD-DELTA= delta (+ prev-delta (second delta-val)) EG:(add-deltas +delta  delta0), (+  delta val))); MULT-DELTA-VAL= (+  (* delta val) (third delta-val)); MULT-DELTA=(+  (* delta val) (third delta-val)) val) EG, (mult-delta *delta  delta0); ADD-EXPDELTA=(+  (expt prev-delta exp) (third delta-val)) EG,(add-expdelta exp +c  delta0) & DEFAULT DELTA=number=> (+ val delta)   
  FOR REPEAT CALLS, use FUNC-SYM as function call sym with varlist value-lists (each QUOTED). If (null call-func-p), then won't eval func-sym function. WARNING: DEFINES VARLIST VARS AS GLOBAL VARS--you MUST use those SAME VARS IN VARLIST. However it also makes those vars (eg x1 x2) UNBOUND at end. If USE-LAST&DELTA-VARS [a list numbers to increase the previous values-list vars--0 if no change.]. Next round uses previous round of vars and increases each by its delta-var value. N-CYCLES is the number of cycles using delta increases. CYCLE-LABELS & CYCLE-NS, put in each cycle output-list in order of labels or cycle-ns in list"
  (let*
      ((y-list)
       (all-outputs)
       (n-vars (list-length varlist))
       (all-deltas)
       (xy-points)
       )
    (loop
     for cyclen from 1 to n-cycles
     do
     (let*
         ((new-values)
          (prev-y)
          (cycle-deltas)
          (cycle-output)
          )
       ;;UPDATE NEW VALUES (+ DELTA-VALS PREV-VALS
       ;;CALC NEW Y
       (loop
        for delta-val in delta-items
        for nval from 0 to n-vars
        do
        (let*
            ((newval)
             (val)
             (curval)
             (prev-y)
             (prev-delta)
             (delta)
             )
          ;;use last calculated val for val
          (cond
           ((> cyclen 1)
            (setf val (nth nval (second (car (last all-outputs))))))
           (t  (setf val  (nth nval value-list0))))
           ;;(break "val")
          ;;find incremented values  prev-value + delta-value, make new value-list
          (cond
           ;; SPECIAL CASES: (listp delta-val) 
           ;;      PREV-Y MULT-DELTA-VAL MULT-DELTA ADD-EXPDELTA 
           ;;If delta is a list (prev-y number), set newval prev-y + number (real delta))
           ((and (listp delta-val)
                 (equal (car delta-val) 'prev-y))
            (setf prev-y (car (last y-list))
                              delta (second delta-val)
                              newval (+ prev-y delta)))  
           ;;Note: If delta = 0 just set  newval to prev-y,
           ((and (> cyclen 1)(equal delta-val 'prev-y))
            (setf val (car (last y-list))
                  delta 0
                  newval val))
           ;;normal case, add delta to val
           ((and (> cyclen 1)(numberp delta-val))
            (setf delta delta-val   
                  newval (+ val delta))
            ;;(break "add 1")
            )
           ;;Mult delta X val + C; Calc newval= ((second delta-val) x val) + (third delta-val)
           ;; (+  (* delta val) (third delta-val))  (+ val delta (third delta-val))  ;;(+  (expt prev-delta exp) (third delta-val)) & (+ val delta)
           ;;ADD-DELTAS  (add-deltas +delta  delta0)
           ((and (> cyclen 1)(listp delta-val)
                 (equal (car delta-val)  'add-deltas))
            (cond
             ((= cyclen 1)
              (setf prev-delta (third delta-val)))
             (t (setf prev-delta (nth nval (car (last all-deltas))))))
            (setf delta (+ prev-delta (second delta-val))
                  newval (+  delta val)))
           ;;MULT-DELTA-VAL
           ((and (> cyclen 1)(listp delta-val)
                 (equal (car delta-val)  'mult-delta-val))
            (setf delta (second delta-val)
                  newval (+  (* delta val) (third delta-val))))
           ;;MULT DELTA X OLD-DELTA + VAL + C  (mult-delta *delta  delta0)
           ((and (> cyclen 1)(listp delta-val)
                 (equal (car delta-val) 'mult-delta))
            (cond
             ((= cyclen 1)
              (setf prev-delta (third delta-val)))
             (t (setf prev-delta (nth nval (car (last all-deltas))))))
            (setf  delta2 (second delta-val)
             delta (* prev-delta delta2)
             newval (+ val delta (third delta-val))))
           ;;POWER:  ADD VAL TO DELTA-EXP + C  (add-expdelta exp +c  delta0)
           ((and (listp delta-val)   
                 (equal (car delta-val) 'add-expdelta))
            (cond
             ((= cyclen 1)
              (setf prev-delta (FOURTH delta-val)))
             (t (setf prev-delta (nth nval (car (last all-deltas))))))
            (setf exp (second delta-val)
                  delta (+  (expt prev-delta exp) (third delta-val))
                  newval (+ val delta))
            ;;(BREAK "add-expdelta")
            )
           ;;POWER: MULT VAL BY DELTA-EXP + C (mult-expdelta exp + c delta0)
           ((and (listp delta-val)   
                 (equal (car delta-val) 'mult-expdelta))
            (cond
             ((= cyclen 1)
              (setf prev-delta (FOURTH delta-val)))
             (t (setf prev-delta (nth nval (car (last all-deltas))))))
            ;;(BREAK "mult-expdelta")
            (setf exp (second delta-val)
                  delta (+ (expt prev-delta exp) (third delta-val))
                  newval (* val delta)))
           ((= cyclen 1)
            (setf delta 0
                  newval val))
           (t (cond
               ((listp delta-val)
                (setf delta (second delta-val)
                    newval (+ val delta  )))
               (t (setf delta delta-val
                        newval (+ val delta))
                  (break "add")
                  ))))

          ;;UPDATE THE LIST
          (setf new-values (append new-values (list newval))
                cycle-deltas (append cycle-deltas  (list delta)))
          ;;end let,inner loop
          ))
       ;;RECURSE USING NEW LISTS
       (cond
        ;;Use MACRO version to calc
        ((or (= cyclen 1) (not (functionp (symbol-function func-sym))))
         ;;(break "1")
         (setf prev-y (car
                    (eval  `(calc-equation-for-varlists ,func-sym ,equation ,varlist ,(list new-values)
                                                  :call-func-p T  :makunbound-func-p ,makunbound-func-p))))
         )
        ;;Use macro DEFINED FUNCTION to calc values (more efficient?)
        (t
         ;;(BREAK "new-values")
         (setf prev-y (car (eval `(,func-sym (quote ,varlist) (quote ,(list new-values))))))

         ))
       ;;Set results lists
       (setf y-list (append y-list (list prev-y))       
             cycle-output (list prev-y new-values cycle-deltas cyclen)) 
         
       (when cycle-labels
        (setf cycle-label (nth (- cyclen 1) cycle-labels)
          cycle-output (append cycle-output (list cycle-label)))) 

       (setf all-deltas (append all-deltas (list cycle-deltas))
             all-outputs (append all-outputs (list cycle-output)))
       ;;end let, outer loop
       ))
      (setf xy-points  (find-xy-points all-outputs find-points-xn))

    (values y-list all-outputs varlist n-cycles func-sym xy-points)
    ;;end let, func-sym calc-equation-for-delta-varlists
    ))
;;TEST
;; (calc-equation-for-delta-varlists 'arcline  (quote `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1)) '(x1 x2 dy/dx y1) `(1 3 .8  2) '(2 2  0  prev-y) 2 :find-points-xn 1)
;; (3.6 5.2)   ((3.6 (1 3 0.8 2) (0 0 0 0) 1) (5.2 (3 5 0.8 3.6) (2 2 0 0) 2))  (X1 X2 DY/DX Y1)  4  ARCLINE   ((1 (3 3.6) "") (2 (5 5.2) ""))
;;(arcline '(x1 x2 dy/dx y1) '((3 5 .6 2)))
;; works= (3.2)  ((3.2 (3 5 0.6 2) 1))  1  ARCLINE  (X1 X2 DY/DX Y1)  "(BQ-LIST (QUOTE +) (BQ-LIST (QUOTE *) DY/DX (BQ-LIST (QUOTE -) X2 X1)) Y1)"   (((0 0 0 0) (3 3.6) 1) ((2 2 0 0) (5 5.2) 2))
;;WITH :CYCLE-LABELS
;; (calc-equation-for-delta-varlists 'arcline  (quote `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1)) '(x1 x2 dy/dx y1) `(1 3 .8  2) '(2 2  0  prev-y) 2 :CYCLE-LABELS '("1st cycle" "2nd cycle") :find-points-xn 1)
;; works= (3.6 5.2)   ((3.6 (1 3 0.8 2) (0 0 0 0) 1 "1st cycle") (5.2 (3 5 0.8 3.6) (2 2 0 0) 2 "2nd cycle"))   (X1 X2 DY/DX Y1)   4  ARCLINE  ((1 (3 3.6) "") (2 (5 5.2) ""))

;;TEST ARC EQUATIONS
;; delta (+ val delta (third delta-val))
;; (calc-equation-for-delta-varlists 'arcline  (quote `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1)) '(x1 x2 dy/dx y1) `(1 3 .8  2) '(2 2  (add-delta .2)  prev-y) 6 :find-points-xn 1)
;; works=(3.6 5.6 8.0 10.8 14.0 17.6)
;;((3.6 (1 3 0.8 2) (0 0 0 0) 1) (5.6 (3 5 1.0 3.6) (2 2 0.2 0) 2) (8.0 (5 7 1.2 5.6) (2 2 0.2 0) 3) (10.8 (7 9 1.4000001 8.0) (2 2 0.2 0) 4) (14.0 (9 11 1.6000002 10.8) (2 2 0.2 0) 5) (17.6 (11 13 1.8000002 14.0) (2 2 0.2 0) 6))
;; (X1 X2 DY/DX Y1)     6  ARCLINE   
;; points= ((1 (3 3.6) "") (2 (5 5.6) "") (3 (7 8.0) "") (4 (9 10.8) "") (5 (11 14.0) "") (6 (13 17.6) ""))






;;CALC-EQUATIONS-FOR-DELTA-VARLISTS
;;2018
;;ddd
(defun calc-equations-for-delta-varlists (calc-eqs-arglists 
                                          &key  (call-func-p T) makunbound-func-p)
  "U-function-plotter.  INPUT: CALC-EQS-ARGLISTS one for each equation. Each EQ-ARGLIST a list of (func-sym equation varlist value-list0 delta-items n-cycles cycle-labels eq-label find-points-xn)      
  NOTE: To draw an arc betw 2 pts--use CALC-CENTER-FROM-PTS function.
  DELTA-ITEMS: (listp delta-val), SPECIAL CASES: PREV-Y; ADD-DELTA= delta (+ prev-delta (second delta-val)) EG:(add-deltas +delta  delta0), (+  delta val))); MULT-DELTA-VAL= (+  (* delta val) (third delta-val)); MULT-DELTA=(+  (* delta val) (third delta-val)) val) EG, (mult-delta *delta  delta0); ADD-EXPDELTA=(+  (expt prev-delta exp) (third delta-val)) EG,(add-expdelta exp +c  delta0) & DEFAULT DELTA=number=> (+ val delta)   
ALL-EQ-OUTPUTS= list of all eq-output-lists. 
  Each EQ-OUTPUT-LIST=(y-list eq-outputs eq-varlist eq-n-cycles func-sym eq-xy-points).  CYCLE-OUTPUTS = list of cycle-output-lists. 
  Each CYCLE-OUTPUT-LIST= (y varvals delta-vals xy-pt cyclen cycle-label)  
  GRAPH POINTS:  y-list is the y-axis output.  FIND-POINTS-XN= either integer for nth location of x in the varlist of each output.  If (listp find-points-xn) finds more points--for 3 or more dim coords. "
  (let*
      ((n-eqs (list-length calc-eqs-arglists))
       (all-eq-outputs)
       (all-points-lists)
       )
    (loop
     for eq-args in calc-eqs-arglists
     for eq-n from 1 to n-eqs
     do
     (let*
         ((y-lists)
          (eq-output-list)
          )

       ;;GET EQ ARGS 
   ;;eg  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1 3 .8  2) (2 2  0  prev-y) 2 ("cyc1" "cyc2")  "Eq1" 1)))
       (multiple-value-bind (func-sym equation varlist value-list0 delta-items n-cycles
                                       cycle-labels eq-label find-points-xn)
           (values-list eq-args)
         ;;(break "1")
       ;;CALC EQ OUTPUT (incl xy-points)
       (multiple-value-bind (y-list eq-outputs eq-varlist eq-n-cycles func-sym eq-xy-points)
           (calc-equation-for-delta-varlists func-sym equation varlist value-list0 
                                             delta-items n-cycles 
                                             :find-points-xn find-points-xn
                                             :cycle-labels cycle-labels
                                             :call-func-p call-func-p
                                             :makunbound-func-p makunbound-func-p)  
         ;;LISTS
         (setf eq-output-list (list y-list eq-outputs eq-varlist eq-n-cycles func-sym 
                                    eq-xy-points eq-label)
               all-eq-outputs (append all-eq-outputs (list eq-output-list))               
               all-points-lists (append all-points-lists (list eq-xy-points)))               
         ;;end mvbs, let,loop
         ))))
    (values all-eq-outputs  all-points-lists)   
    ;;end calc-equations-for-delta-varlists
    ))
;;TEST
;; arglist= (func-sym equation varlist value-list0 delta-items n-cycles cycle-labels eq-label find-points-xn)
;;test for 1 equation only:
;; (calc-equations-for-delta-varlists '((arcline  `(+ (* ,dy/dx (- ,x2  ,x1)) ,y1) (x1 x2 dy/dx y1) (1 3 .8  2) (2 2  0  prev-y) 2 ("cyc1" "cyc2")  "Eq1" 1)))
;; works= (((3.6 5.2) ((3.6 (1 3 0.8 2) (0 0 0 0) 1 "cyc1") (5.2 (3 5 0.8 3.6) (2 2 0 0) 2 "cyc2")) (X1 X2 DY/DX Y1) 2 ARCLINE ((1 (3 3.6) "") (2 (5 5.2) "")) "Eq1"))
;;points= (((1 (3 3.6) "") (2 (5 5.2) "")))



;
;;xxx  --------------- ORIGINAL FUNCTIONS TO DRAW-GRAPH-WINDOW ----
;; 
;; PLOTTED NICE WINDOW, BUT CAN'T COPE WITH COMPLEX EQUATIONS AS DRAW-EQUATION-CURVES (above) It's also more general/flexible



;; -------------------------------------- MY GRAPH DRAWING FUNCTION -----------------

;;DRAW-GRAPH-WINDOW
;;          
;;ddd 
(defun draw-graph-window (port point-list x-max y-max x-margin y-margin 
                               &key multiple-graphs-p 
                               (interface-class 'interface-display) 
                               ;;(output-pane 'output-pane-1) ;;ONLY WORKS W/output-pane-1 NOW.
                               append-point-list  begin-num (label-margin 10) 
                               (graph-color :red) 
                               (graph-color-list '(:red :blue :orange :green :black :yellow
                                                   :lightblue :magenta :pink  :gray :violet))
                               (x-mark-interval 20)  (y-mark-interval 20)  (half-hash-length 3)
                               (label-margin 10)
                               x-label-color  y-label-color 
                               x-label-list  y-label-list
                               (x-label-type :point-label)
                               (x-increment 10)   (y-increment 10)
                               x-string y-string ( x-string-margin 35)
                               (y-string-margin 30)
                               (y-element-space 20)
                               x-font-list y-font-list  (x-string-color :black) (y-string-color :black)
                               (center-x-string-p T) (center-y-string-p T)
                               (x-multiplier 1.0) (y-multiplier 1.0)
                               (use-test-interface-p NIL)
                                y-floor-value)
  "In function-plotter.lisp, If multiple-graphs-p, will draw graphs from point-list= a list of point-lists (if NIL, point-list is ordinary list of points, draws 1 graph.)   
  ARGS (port point-list x-max y-max x-margin y-margin 
[NOTE: PORT MUST = NIL unless it is a real port in ALREADY DISPLAYED WINDOW--otherwise get an ERROR: \"Can't find a font device for ..\"
                               &key multiple-graphs-p
                              begin-num  label-margin graph-color
                             x-label-list y-label-list
                             x-mark-interval y-mark-interval half-hash-length  label-margin
                              x-increment  y-increment      x-label-color  y-label-color
                             (graph-color :red) (graph-color-list '(:red :blue :orange :green :black :yellow :lightblue :magenta :pink  :gray :violet))
                             x-string y-string x-string-margin y-element-space
                             center-x-string-p center-y-string-p
                              x-multiplier y-multiplier Y-FLOOR-VALUE WON'T allow y to go below that value if not NIL. 
  FOR MULTI-GRAPH-LINE windows, POINT-LIST must be divided into a point-list for EACH graph, each beginning with :dimsim number.
     X-axis LABELS: key X-LABEL-TYPE can be   :point-label, :point-n-label,  :dim-n-label :value-label
     If :dim-n-label must be list (:dim-n-label nth-dim) nth-dim= integer. If x-label-list exists it overrides this. :point-labels are the labels included in the point-list.     A simple POINT-LIST= (point-n xpix-val-list xlabel dims)  
     PROBLEM: ONLY WORKS W/output-pane-1 NOW.                      
"
  ;; (afout 'out (format nil "XXXX In draw-graph-window, :x-multiplier= ~A :y-multiplier= ~A~% POINT-LIST= ~A~%" x-multiplier  y-multiplier point-list))

  ;;EXTRA INTERFACE MADE HERE--DON'T USE THIS FOR PRODUCTION
  (let
      ((test-interface)
       (dimlists)
       (first-point-list)
       (first-dimlist)
       (point)
       (value)
       (value-lists)
       (y-val-list)
       (dim1-value-list)  
       (point-n)
       (point-n-list)
       (xlabel)
       (xlabel-list)
       (x-val-list)
       (point-n)
       (point-n-list)
       (xlabel)
       (xlabel-list)
       (dim-n)
       (dim-n-list)
       (dim-label-nth)
       (xpix-val-list)
       (point-list1)
       (dims)
      )

    ;;STEP 1: MAKE THE INTERFACE WITH COORDINATES, ETC.

    ;;THE INTERFACE AND PORT
    (when use-test-interface-p
      (setf test-interface  (make-instance interface-class)))
#|    (unless port
      (setf test-interface  (make-instance 'interface-display)))   |#

      ;;NOTE: ONLY WORKS WITH OUTPUT-PANE-1
    (with-slots (output-pane-1) test-interface

      (when (or  (null port) test-interface)
        (setf port output-pane-1)
         (capi:display test-interface))

      ;;When append-point-list (a non dimlist list)
      (when append-point-list
        (cond
         ((equal (caar point-list) :dimsim)
          (setf point-list (append point-list 
                                   (list (list :dimsim (incf (length point-list))) append-point-list))))
         (t 
           (setf point-list (list (list :dimsim 1 point-list)(list :dimsim 2 append-point-list))))))

      ;;ORGANIZE POINT-LISTS into proper format,and
      ;; DIVIDE FOR MULTI-GRAPH-LINES
      (when (equal (caar point-list) :dimsim)
        (setf dimlists point-list))    
      ;;(afout 'out (format nil "x dimlists= ~A~%" dimlists))
      (cond
       ((null multiple-graphs-p)
        ;;when dimlists, even 1dim lists are nested--with only 1 point-list
        (when dimlists 
          (setf point-list (third (car dimlists))
                first-point-list point-list)))
       ;;if multiple graphs in window using same coordinates etc.
       (t 
        (setf first-dimlist (car point-list))
        ;;eg point-list1 = ((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1)))
        (when (and (listp first-dimlist)(equal (car first-dimlist) :dimsim))
          (setf  dim-n (second first-dimlist)
                 first-point-list (third first-dimlist)))))

      ;;TO GET POINT VALUES FOR MAKING X LABELS, etc
      (loop
       for point in first-point-list
       do
       (multiple-value-setq (point-n xpix-val-list xlabel dims)
           (values-list point))
       (setf  dim1-value-list (append dim1-value-list (list (second xpix-val-list)))
              point-n-list (append point-n-list (list point-n))
              xlabel-list (append xlabel-list (list xlabel))
              y-val-list (append y-val-list (list (second xpix-val-list))))
       (when (and (listp  x-label-type)(equal (car x-label-type) :dim-n-label))
         (setf dim-label-nth (second x-label-type)
          dim-n-list (append dim-n-list (list (nth dim-label-nth dims)))))
       )
      
      ;;WHAT TYPE OF LABELS TO PRINT ON X-AXIS;  key=  :x-label-type
      (cond
       ;;Priority, if x-increment, adds numbers, done below
       (x-increment NIL )
       ;;Priority, if x-label-list, adds labels in a separate list, done below
       (x-label-list NIL )
       ;;Following all use KEY  :X-LABEL-TYPE to determine x labels.
       ;;adds labels "label" built in to points-list 
       ((equal x-label-type :point-label)
        (setf x-label-list xlabel-list))
       ;;adds  first number in point as label
       ((equal x-label-type :point-n-label)
        (setf x-label-list point-n-list))
        ;;add y-values as label
       ((equal x-label-type :value-label)
        (setf x-label-list y-val-list))        
       ;;adds labels from a selected nth dim from dims
       ((and (listp  x-label-type)(equal (car x-label-type) :dim-n-label))
        (setf  x-label-list dim-n-list))
       (t nil))

      ;;When draw multiple-graphs
      (when (and multiple-graphs-p graph-color-list (null x-string))  
        (if (or (null x-string)(string-equal x-string ""))
            (setf x-string "Graphs: "))
        (loop
         for color in graph-color-list
         for num from 1 to (-  (length graph-color-list) 1)
         do
         (setf x-string (format nil "~A ~A= ~A" x-string num color))
         )) 

      ;;DRAWS XY COORDINATES (below)
      (draw-xy-axes port x-max y-max x-margin y-margin 
                    :x-mark-interval x-mark-interval :y-mark-interval y-mark-interval
                    :half-hash-length half-hash-length :label-margin label-margin
                    :x-label-list x-label-list :y-label-list y-label-list
                    :x-increment x-increment :y-increment y-increment
                    :x-label-color x-label-color :y-label-color y-label-color
                    :x-string x-string :y-string y-string :x-string-margin x-string-margin
                    :x-font-list x-font-list  :y-font-list y-font-list
                    :x-string-color x-string-color :y-string-color y-string-color
                    :y-element-space y-element-space
                    :center-x-string-p center-x-string-p :center-y-string-p center-y-string-p)

      ;; (afout 'out (format nil "BEFORE draw-graph-line, port point-list= ~A~%  x-max= ~A y-max= ~A x-margin= ~A y-margin= ~A~% :graph-color= ~A  x-multiplier= ~A~%   y-multiplier= ~A~%"  point-list x-max y-max x-margin y-margin graph-color  x-multiplier   y-multiplier) "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\outfile.lisp")

      #|      ;;ORGANIZE POINT-LISTS into proper format,and
      ;; DIVIDE FOR MULTI-GRAPH-LINES
      (when (equal (caar point-list) :dimsim)
        (setf dimlists point-list))
      
      (afout 'out (format nil "x dimlists= ~A~%" dimlists))|#

      ;;(BREAK "dimlists before draw")
      ;;STEP 2: DRAW THE GRAPH(S)
      ;;MAIN GRAPH DRAWING FUNCTION (below)
      (cond
       ;;FOR 1 GRAPH WINDOWS
       ((null multiple-graphs-p)
        ;;when dimlists, even 1dim lists are nested--with only 1 point-list
        (when dimlists (setf point-list (third (car dimlists))))
        ;;for 1 graph line point list, draw it.
        ;;(afout 'out (format nil "BB point-list= ~A~%" point-list))
        (draw-graph-line port point-list x-max y-max x-margin y-margin
                         :graph-color graph-color 
                         :x-multiplier x-multiplier  :y-multiplier  y-multiplier
                         :y-floor-value  y-floor-value)
        ;;end clause
        )
       ;;IF MULTIPLE GRAPHS IN WINDOW USING SAME COORDINATES ETC.
       ;;1. FOR LISTS WITH :dimsim as car
          ;; and point-list1= ((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1)))
       (dimlists
        (loop
         for dimlist  in dimlists
         for graph-color in graph-color-list
         do
          (when (and (listp dimlist)(equal (car dimlist) :dimsim))
            (setf  dim-n (second dimlist)
                   point-list1 (third dimlist))
            (if (listp point-list1)
                (draw-graph-line port point-list1  x-max y-max x-margin y-margin 
                                 :graph-color graph-color :x-multiplier x-multiplier
                                 :y-multiplier  y-multiplier
                                 :y-floor-value  y-floor-value))
            ;;end when,loop, dimlist version
            )))
          ;;2. FOR NON-DIMLIST VERSION
          ;;eg point-list = ((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1)))
          ((and point-list (listp (car point-list)))
           (loop
            for sub-point-list in point-list
            for graph-color in graph-color-list
            do
            (draw-graph-line port sub-point-list  x-max y-max x-margin y-margin 
                             :graph-color graph-color :x-multiplier x-multiplier
                             :y-multiplier  y-multiplier
                             :y-floor-value  y-floor-value)
            ;;(break "in loop")
            (sleep 0.3)
            ;;end loop, clause
            ))
         ;;end outer t,cond   
          (t nil))
      ;;(fout out)
      ;;end with-slots,let,draw-graph-window
      )))
;;TEST
;;REAL DATA FOR *RESETNIN-POINTS and *RESETNOUT-POINTS
;;  (draw-graph-window nil  *RESETNIN-POINTS  400 300 20 20   :multiple-graphs-p T :y-multiplier 1.0  :y-label-list '(y1 y2 y3)  :y-string  "      Y-STRING " :x-label-type  :point-label :APPEND-POINT-LIST *RESETNOUT-POINTS)
;; REAL DATA FOR  *INPUT-POINTS
;;   (draw-graph-window nil  *WUPI-3-2TOI-1-3-POINTS  400 300 20 20   :multiple-graphs-p T :y-multiplier 500.0  :y-label-list '(y1 y2 y3)  :y-string  "      Y-STRING " :x-label-type  :point-label) ;;WORKS
;; PRACTICE/TEST DATA  ;;here 2
;;   (draw-graph-window nil '((1 (40 0.7) "Input1" (1)) (2 (80 -0.067839995) "Input2" (2)) (3 (120 0.05008) "Input3" (3)) (4 (160 1.04512) "Input4" (4)) (5 (200 0.97264) "Input5" (5)) (6 (240 1.03376) "Input6" (6)) (7 (280 0.023199998) "Input7" (7)) (8 (320 -0.050559998) "Input8" (8)) (9 (360 0.047519997) "Input9" (9)))   400 400 100 100 :y-multiplier 100 :x-string "  This is X String  " :y-string " y string vertically") ;;NOTE: Y-MULTIPLIER HAS TO BE 100 FOR GIVEN DATA ABOVE = WORKS
;; (testdgw)
;;   (draw-graph-window nil '((0 (50 30) "P1")(1 (100 200) "P2")(3 (150 75)"P3")(4 (200 300)"P4"))  400 400 100 100) 
;; works
;;   (draw-graph-window nil '((:dimsim 1 ((0 (50 30) "P1")(1 (100 200) "P2")(3 (150 75)"P3")(4 (200 300)"P4"))))  400 400 100 100) = works

;;   (draw-graph-window nil '((:dimsim 1 ((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1)))) (:dimsim 2 ((1 (40 50) WUP1-2 (1 2)) (2 (80  20) WUP2-2 (2 2))))) 400 400 100 100 :multiple-graphs-p T :y-multiplier 1.0 :x-label-list '("x1" "x2" "x3") :y-label-list '(y1 y2 y3)  :y-string  "      Y-STRING "))) = WORKS
;;NOTE: Y-MULTIPLIER HAS TO BE 1.0 FOR GIVEN DATA ABOVE = WORKS
;;TESTED THE DIFFERENT :x-label-type BELOW:
;;(draw-graph-window nil '((:dimsim 1 ((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1)))) (:dimsim 2 ((1 (40 50) WUP1-2 (1 2)) (2 (80  20) WUP2-2 (2 2))))) 400 400 100 100 :multiple-graphs-p T :y-multiplier 1.0  :y-label-list '(y1 y2 y3)  :y-string  "      Y-STRING " :x-label-type  :point-label)

;;   (draw-graph-window nil '((:dimsim 1 ((1 (40 110) INPUT1 (1)) (2 (80 222 ) INPUT2 (2)) (3 (120 333) INPUT2 (3))))) 400 400 100 100 :x-label-list '("x1" "x2" "x3") :y-label-list '(y1 y2 y3) :x-string "   X   String    "  :y-string  "      Y-STRING ")) = works

;;   (testdgw) = works
;;   (draw-graph-window nil '((0 (40 0.01991048)) (1 (80  3.0)) (2 (120 0.030651689)) (3 (160 0.02819966)) (4 (200 0.020918929)) (5 (240 0)) (6 (280 0)) (7 (320 0)) (8 (360 0)))  400 400 100 100  :y-multiplier 50.0 :y-floor-value 0 )

;;DRAW 2 GRAPHS --SSSSS START HERE-WORKS THO
;; (draw-graph-window nil '(((1 (40 0.033760004) "Input1" (1)) (2 (80 0.070879996) "Input2" (2)) (3 (120 -0.021760002) "Input3" (3)) (4 (160 0.97904) "Input4" (4)) (5 (200 1.05632) "Input5" (5)) (6 (240 0.92912) "Input6" (6)) (7 (280 0.05392) "Input7" (7)) (8 (320 0.05088) "Input8" (8)) (9 (360 -0.006240003) "Input9" (9)))((1 (40 0.033760004) "Input1" (1)) (2 (80 0.070879996) "Input2" (2)) (3 (120 -0.021760002) "Input3" (3)) (4 (160 0.97904) "Input4" (4)) (5 (200 .65) "Input5" (5)) (6 (240 .8) "Input6" (6)) (7 (280 .4) "Input7" (7)) (8 (320 .45) "Input8" (8)) (9 (360 .7) "Input9" (9))))  400 400 100 100  :y-multiplier 50.0 :y-floor-value 0 :multiple-graphs-p T )
;; works
#|(defun testdgw ()
  (setf out nil)
  (draw-graph-window nil '((0 (50 30) "P1")(1 (100 200) "P2")(3 (150 75)"P3")(4 (200 300)"P4"))     400 400 100 100 :begin-num 0  :label-margin 10 :graph-color :blue
                            :x-mark-interval 20 :y-mark-interval 20  :half-hash-length 3  :label-margin 20
                            :x-label-list '(x1 x2 x3 x4 x5 x6 x7)
                             :y-label-list nil
                             :x-font-list '(:family "Times New Roman" :weight :normal :size 10)
                             :y-font-list '(:family "Times New Roman" :weight :normal :size 10)
                            :x-label-color :green :y-label-color :red
                             :x-string "This is a test x-string" 
                             :y-string '("AA" "BB" "CC" "DD" "YY")
                             ;; :x-increment  10  :y-increment  10
                              :x-string-margin 40 :y-element-space 20
                              :x-string-color :red :y-string-color :blue
                              :center-x-string-p t :center-y-string-p nil
                              :x-multiplier 1.0 :y-multiplier 1.0 :y-floor-value 0)
 (fout out)
  )|#    


;;
;;TEST FOR A SINGLE GRAPH IN A WINDOW
#|(defun testdgw2 ()
  (setf out nil)
  (draw-graph-window nil '((0 (40 0.01991048)) (1 (80  3.0)) (2 (120 30)) (3 (160 0.02819966)) (4 (200 0.020918929)) (5 (240 0)) (6 (280 0)) (7 (320 0)) (8 (360 0)))     400 400 100 100 :begin-num 0  :label-margin 10 :graph-color :blue
                          :x-mark-interval 20 :y-mark-interval 20  :half-hash-length 3  :label-margin 20
                            :x-label-list '(x1 x2 x3 x4 x5 x6 x7)
                             :y-label-list nil
                             :x-font-list '(:family "Times New Roman" :weight :normal :size 10)
                             :y-font-list '(:family "Times New Roman" :weight :normal :size 10)
                            :x-label-color :green :y-label-color :red
                             :x-string "This is a test x-string" 
                             :y-string '("AA" "BB" "CC" "DD" "YY")
                             ;; :x-increment  10  :y-increment  10
                              :x-string-margin 40 :y-element-space 20
                              :x-string-color :red :y-string-color :blue
                              :center-x-string-p t :center-y-string-p nil
                              :x-multiplier 1.0 :y-multiplier 1.0 :y-floor-value 0)
 (fout out)
  )|#
;;  (testdgw2)
;;
;;TEST FOR MULTIPLE GRAPHS IN A WINDOW
#|(defun testdgw3 ()
  (setf out nil)
  (draw-graph-window nil '(((0 (40 0.01991048)) (1 (80  3.0)) (2 (120 30)) (3 (160 0.02819966)) (4 (200 0.020918929)) (5 (240 0)) (6 (280 0)) (7 (320 0)) (8 (360 0)))
      ((0 (40 0.8)) (1 (80  1.0)) (2 (120 2.0)) (3 (160 22.0)) (4 (200 11.0)) (5 (240 5.0)) (6 (7.0)) (7 (320 15.0)) (8 (360 10.0))))     400 400 100 100 :multiple-graphs-p T  :begin-num 0  :label-margin 10 :graph-color :blue
                          :x-mark-interval 20 :y-mark-interval 20  :half-hash-length 3  :label-margin 20
                            :x-label-list '(x1 x2 x3 x4 x5 x6 x7)
                             :y-label-list nil
                             :x-font-list '(:family "Times New Roman" :weight :normal :size 10)
                             :y-font-list '(:family "Times New Roman" :weight :normal :size 10)
                            :x-label-color :green :y-label-color :red
                             :x-string "This is a test x-string" 
                             :y-string '("AA" "BB" "CC" "DD" "YY")
                             ;; :x-increment  10  :y-increment  10
                              :x-string-margin 40 :y-element-space 20
                              :x-string-color :red :y-string-color :blue
                              :center-x-string-p t :center-y-string-p nil
                              :x-multiplier 1.0 :y-multiplier 1.0 :y-floor-value 0)
 (fout out)
  )|#
;;  (testdgw3)



;;DRAW-GRAPH-LINE
;;
;;ddd
(defun draw-graph-line (port point-list x-max y-max x-margin y-margin &key graph-color  (x-multiplier 1.0) ( y-multiplier 1.0) y-floor-value )
  "In function-plotter.lisp, uses (list time coordinates-list label) as basic data element. If y-floor-value, then changes y-coord to floor-value if y is less than that value."
  (let
      ((t1 0)
       (x1 0)
       (y1 0)
       (label1)
       (t2 0)
       (coordinates)
       (x2 0)
       (y2 0)
       (label2)
       (n 0)
       (xy-coord-list)
       (xy-spec-list)
       (x1-pix 0)
       (y1-pix 0)
       (x2-pix 0)
       (y2-pix 0)
       (x-origin-pix 0)
       (y-origin-pix 0)
       (pane-height (capi::pane-height port))
       (pane-width (capi::pane-width port))
       (top-margin 20)
       (right-margin 20)
       (x-coord) 
       (y-coord) 
       )
    ;;should have been determined in let above
    (unless pane-height (setf pane-height 100))
      ;;determine placement and margins
      (setf top-margin (-  pane-height (+ y-max y-margin))
            right-margin (- pane-width (+ x-max x-margin))
            x-origin-pix  x-margin
            y-origin-pix  (+ y-max top-margin))

     ;;(if (> *print-details 1) (afout 'out (format nil "y-margin= ~A x-margin= ~A y-origin-pix= ~A~%" y-margin x-margin y-origin-pix)))

   (dolist (point point-list)
      (incf n)
      (setf coordinates (second point)
            x-coord (car coordinates)
            y-coord (second coordinates))

      ;;if floor-value, adjust values in coordinates so they are not less than floor-value
      (when (and y-floor-value y-coord)
        (setf  y-coord (my-floor y-coord :floor y-floor-value)))
        
      (cond
       ((listp point)
      (cond
       ((= n 1)
        (setf t1 (car point)
              ;;coordinates (second point)
              label1 (third point))
        (when (and (numberp x-coord)(numberp y-coord))
          (setf x1(* x-coord x-multiplier)
                y1 (* y-coord y-multiplier)
                x1-pix (+ x-origin-pix x1)
                y1-pix (- y-origin-pix y1)
                ;;more code here if want more than 2 dimensions
                )))
       (t (setf t2 (car point)
              ;;coordinates (second point)
              label2 (third point))
        (when (and (numberp x-coord)(numberp y-coord))
          (setf x2 (* x-coord x-multiplier)
                y2 (* y-coord y-multiplier)
                x2-pix (+ x-origin-pix x2)
                y2-pix (- y-origin-pix y2)))
                ;;more code here if want more than 2 dimensions
                ;;(if (>  *print-details 1)   (afout 'out (format nil "In draw-graph-line, x-multiplier= ~A x1= ~A y-multiplier= ~A y1= ~A~%" x-multiplier x1 y-multiplier y1)))
               ;;(if (>  *print-details 1)  (afout 'out (format nil "coordinates= ~A x2=~A x2-pix= ~A y-margin= ~A x-margin= ~A y-origin-pix= ~A~%" coordinates x2 x2-pix y-margin x-margin y-origin-pix)) )

      ;;add to draw-lines spec list
      (setf xy-spec-list (append xy-spec-list (list x1-pix y1-pix x2-pix y2-pix))
            xy-coord-list (append xy-coord-list (list x1 y1 x2 y2)))
    ;;(if (>  *print-details 1)  (afout 'out (format nil "x1-pix= ~A y1-pix= ~A x2-pix= ~A y2-pix= ~A~%"  x1-pix y1-pix x2-pix y2-pix)))
      
      (gp:draw-line port x1-pix y1-pix x2-pix y2-pix :foreground graph-color)
      ;;reset the initial point for next cycle
      (setf x1 x2 y1 y2 x1-pix x2-pix  y1-pix y2-pix t1 t2 label1 label2)
      )
      ;;end dolist, etc.
      ))))
   ))
;;TEST
;;tested inside of draw-graph-window above


            

;;DRAW-XY-AXES
;;
;;ddd
(defun draw-xy-axes (port x-max y-max x-margin y-margin 
                          &key x-label-list y-label-list 
                          (x-mark-interval 20) (y-mark-interval 20)
                          (x-increment 10) (y-increment 10)
                          half-hash-length  label-margin
                          x-label-color y-label-color
                          X-STRING Y-STRING 
                          (x-string-margin 30) (y-string-margin 30)
                          y-element-space
                          center-x-string-p center-y-string-p
                          x-font-list y-font-list
                          x-string-color y-string-color
                          (x-origin-pix 0)
                           (y-origin-pix 0)
                           (port-pane-ht (capi::pane-height port))
                           (port-pane-width (capi::pane-width port))
                          (pane-height 100)
                          (pane-width  200)
                          (top-margin 20)
                          (right-margin 20)
                          (label-loc 0)
                          (print-ystring-vertically-p T)
                          )
  "In function-plotter.lisp, draws xy axes"
  (let 
      ((x-font (my-find-best-font port x-font-list))
       (y-font (my-find-best-font port y-font-list))
       (x-max-pix)
       )

       (if port-pane-ht 
           (setf pane-height (capi::pane-height port)))
       (if port-pane-width 
           (setf pane-width (capi::pane-width port)))

    (setf top-margin (-  pane-height (+ y-max y-margin))
          right-margin (- pane-width (+ x-max x-margin))
          x-origin-pix x-margin
          y-origin-pix  (+ y-max top-margin)
          x-max-pix (+ x-origin-pix x-max) )

     ;;(afout 'out (format nil "In draw-xy-axes, pane-height=~A y-margin= ~A x-margin= ~A x-max-pix= ~A y-origin-pix= ~A~%  "pane-height y-margin x-margin top-margin y-origin-pix))
 ;;  (afout 'out (format nil "In draw-xy-axes, x-string= ~A~% y-string= ~A~%" x-string y-string))
    ;;draw the axes
    (gp:draw-line port x-origin-pix top-margin  x-origin-pix y-origin-pix)
    (gp:draw-line port x-origin-pix y-origin-pix  x-max-pix y-origin-pix)

    (if x-mark-interval 
        ;;(defun draw-hash-marks (port interval origin-pix end-pix axis-pix  half-hash-length vertical-p )
        (draw-hash-marks port x-label-list x-mark-interval x-origin-pix x-max-pix 
                         y-origin-pix half-hash-length :label-margin label-margin
                         :incr-num x-increment
                          :x-font x-font :y-font y-font
                         :x-label-color x-label-color ))   ;;:y-label-color y-label-color)
    (if y-mark-interval
        (draw-hash-marks port y-label-list  y-mark-interval y-origin-pix 
                         top-margin x-origin-pix
                         half-hash-length :vertical-p t :label-margin label-margin 
                          :incr-num y-increment 
                          :x-font x-font :y-font y-font
                          :y-label-color y-label-color) )
   
    ;;DRAW X-STRING AND/OR Y-STRING
    (cond
     (x-string
      (let
          ((x-string-x-pix  x-origin-pix)
           (x-string-y-pix  (+ y-origin-pix x-string-margin))
           )
       ;;note: length measures number of chars NOT Pixels, Until I find how to measure length
       ;; of a string in pixels, will guestimate a correction factor of  the length of the string
       ;; so I don't divide it by 2 as I otherwise would 
        (if center-x-string-p 
            (setf x-string-x-pix
                  (+ x-origin-pix  (round (-   (/  x-max 2.0) (* (length x-string) 2.0))))))

       ;;doesn't work until AFTER string drawn in port
       ;;( gp:get-string-extent port x-string) 

       ;;(afout 'out (format nil "center-x-string-p= ~A x-string-x-pix= ~A x-origin-pix=~A x-max= ~A~%"center-x-string-p x-string-x-pix x-origin-pix x-max ))
       ;;actual drawing of the string
        (gp:draw-string port x-string x-string-x-pix x-string-y-pix :font x-font
                        :foreground x-string-color )
        )))
    ;;mmm
          ;; (afout 'out (format nil "In draw-xy-axes, y-string= ~A~%" y-string))
    (cond
     (y-string
      (let
          ((y-string-x-pix  (-  x-origin-pix y-string-margin  10)) ;;was   (length x-string)))
           (y-string-y-pix  (- y-origin-pix y-max))
           )
        
        ;;first convert y-string to a list to be printed vertically
        (when (and print-ystring-vertically-p (stringp y-string))
          (setf y-string (convert-string-to-char-strings y-string)))

        ;;then print it vertically (could already be a list)
        (cond
         ((listp y-string)

          (dolist ( y-string-element y-string)
            (gp:draw-string port y-string-element y-string-x-pix y-string-y-pix :font y-font
 :foreground y-string-color )
            (setf y-string-y-pix (+ y-string-y-pix y-element-space))
            ;;what is y-string
            ))
         (t
          (if center-y-string-p
              (setf y-string-y-pix (+ y-origin-pix (round (/ y-max 2)))))

          (gp:draw-string port y-string y-string-x-pix y-string-y-pix :font y-font
                          :start 0 :end (gp:get-string-extent port y-string))))
        )))
    ;;end let, defun
    ))


;;DRAW-HASH-MARKS
;;
;;ddd
(defun draw-hash-marks (port x-label-list  interval origin-pix end-pix axis-pix half-hash-length &key vertical-p  (begin-num 0) (incr-num 10) (label-margin 20)
                             x-font y-font
                             x-label-color y-label-color )
  (let
      ((num-hash-marks 0)
       (hash-begin 0)
       (hash-end 0)
       (current-x-loc origin-pix)
       (current-y-loc origin-pix)
       (current-label begin-num)
       (current-label-string) 
       (label-x-margin)
       (label-y-margin)
       (current-label-x-loc)
       (current-label-y-loc) 
       )
#|    (unless begin-num (setf begin-num 0))
    (unless incr-num (setf incr-num 10))
    (unless label-margin (setf label-margin 20))
    (setf current-label begin-num)|#

     ;;(afout 'out (format nil "starting IN DRAW-HASH-MARKS, origin-pix ~A end-pix= ~A vertical-p+ ~A~%" origin-pix end-pix vertical-p))

      (setf num-hash-marks (round (/ (abs (- end-pix origin-pix)) interval)))
       ;;mmm
      ;;make each hash imark
      (loop
       for i from 1 to num-hash-marks
       do
      ;was;(dotimes (i num-hash-marks)
        ;;is label from x-label-list or increment numbers?
        (cond
         (x-label-list
          (cond
           ((equal x-label-list 'empty)
            (setf  current-label ""
                   x-label-list 'empty))
           ((> (length x-label-list) 1)
              (setf  current-label (car x-label-list)
                     x-label-list (cdr x-label-list)))
           ((= (length x-label-list) 1)
            (setf current-label (car x-label-list)
                  x-label-list 'empty))))
         ((and incr-num (numberp current-label))

          ;;chop extra digits here SSS  FIND (I DID IT BEFORE?) OR WRITE A
       ;; FUNCTION THAT  TAKES 3.33333 TO FIX NUM OF DIGITS  ETC 3.33
          (setf  current-label (+ current-label incr-num)))
         (t nil))
        ;; set the values to use in draw functions
        (setf  current-label-string (format nil "~A" current-label)
               hash-begin (+ axis-pix half-hash-length)
               hash-end (- axis-pix half-hash-length)
               current-x-loc  (+ current-x-loc  interval)
               current-y-loc (- current-y-loc interval)
               label-x-margin (+ label-margin  10)
               label-y-margin (+ label-margin 5)
               current-label-x-loc 
               (- hash-end label-x-margin (gp:get-string-extent port current-label-string))
               current-label-y-loc (+ hash-begin label-y-margin)
               )

       ;;(afout 'out (format nil "IN DRAW-HASH-MARKS x-label-list= ~A~%interval= ~A hash-begin= ~A hash-end= ~A~% current-x-loc = ~A current-y-loc= ~A num-hash-marks= ~A vertical-p= ~A ~%" x-label-list interval hash-begin hash-end current-x-loc current-y-loc num-hash-marks vertical-p))
        ;;mmm
        (cond
         (vertical-p
          (gp:draw-line port hash-begin  current-y-loc  hash-end current-y-loc)
          ;;draw the label
          ;; x-start y-start &key begin-num incr-num label-margin color)
          (gp:draw-string port current-label-string current-label-x-loc current-y-loc
                          :font y-font :foreground y-label-color))
         (t 
          (gp:draw-line port  current-x-loc hash-begin  current-x-loc hash-end)
          ;;draw the label
          (gp:draw-string port current-label-string (- current-x-loc 5) current-label-y-loc
                         :font x-font :foreground x-label-color)
          ))
        ;;end loop
        )
      ;;end let, draw-hash-marks
        ))
;;TEST
;; draw-hash-marks tested inside other functions above





 ;;MY-DRAW-PATH --------------------
;;
;; PROBLEM WITH DRAW-PATH IS THAT THE X Y START POINT  ALSO BECOMES
;;   THE MAIN NEW REFERENCE POINT from which all new points are calculated relative 
;; to that point (pixel values relative to that point) for EACH FIGURE IN THE SPEC.
;;  the   :MOVE KEYWORD STARTS A NEW FIGURE WITH THE X Y SPEC WITH :MOVE as the new reference point for the new figure.  
;;  THIS CONTINUAL MOVEMENT OF THE REFERENCE COORDINATES MAKES DRAW PATH PROBLEMATIC FOR DRAWING GRAPHS!!!
;;  
  
;;MY-DRAW-PATH -- NOT BEING USED AS OF 2015-03-16
;;
;;ddd
(defun my-draw-path (port path-point-list &optional x-max y-max &key xy-margin-list figure-type) 
  "In function-plotter.lisp, takes a list of  xy-coordinate points like draw-path except offsets for starting origin.
   Good for drawing graphs of function outputs of a point list. xy-margin-list is position of xy-origin from TOP-LEFT CORNER OF PANE. path-point-list INCLUDES starting point (unlike draw-path)"
  (let 
      ((path-spec)
       (path-element)
       (x-pix)
       (y-pix)
       (x-origin-pix 0)
       (y-origin-pix 0)
       (x-margin 0)
       (y-margin 0)
       (pane-height (capi::pane-height port))
       (pane-width (capi::pane-width port))
       (top-margin 20)
       (right-margin 20)
       (start-list)
      (x-start 0)
       (y-start 0)
#|        (previous-x 0)
       (previous-y 0)|#
#|       (start-xy-element)
       (rest-of-points)|#
       (n 0)
       )
       
    (unless x-max
      (setf x-max 0))
    (unless y-max
      (setf y-max 300))

    (cond
     (xy-margin-list
      (setf x-margin (car xy-margin-list)
            y-margin (second xy-margin-list)
            top-margin (-  pane-height (+ y-max y-margin))
            right-margin (- pane-width (+ x-max x-margin))
            x-origin-pix x-margin
            y-origin-pix  (+ y-max top-margin)))
    (t nil))

    (unless figure-type
      (setf figure-type ':line))

      (afout 'out (format nil "x-origin-pix= ~A y-origin-pix= ~A~%x-margin= ~A y-margin= ~A~%top-margin= ~A right-margin= ~A~%pane-height= ~A pane-width= ~A~% x-max= ~A y-max= ~A~%"  x-origin-pix y-origin-pix x-margin y-margin top-margin right-margin pane-height pane-width x-max y-max))            

    ;;process each point-note first point is first in list, unlike draw-path
    (dolist (point-list path-point-list)
      (incf  n)
      ;;(afout 'out (format nil "point-list= ~A~%" point-list))

      ;;screen start is same (left) for x axis, opposite (top vs bottom) for y axis
      (setf x-pix (+  (car point-list) x-origin-pix)
            y-pix  (- y-origin-pix  (second point-list) ))
         
      (setf  path-element `(,figure-type ,x-pix ,y-pix)
             path-spec (append path-spec  (list path-element) ))
      ;;end dolist
      )

    ;;take first point and make it draw-path starting point
    (setf start-list (car path-spec)
          x-start (second start-list)  ;;:line is car of list
          y-start (third start-list))

    ;;(afout 'out (format nil "cdr path-spec= ~A~% x-start= ~A y-start= ~A~%" (cdr path-spec)  x-start y-start))

#|      (setf x-margin (car xy-margin-list)
            y-margin (second xy-margin-list)
            top-margin (-  pane-height (+ y-max y-margin))
            right-margin (- pane-width (+ x-max x-margin))
            x-origin-pix x-margin
            y-origin-pix  (+ y-max top-margin)))|#

    ;;draw the xy coordinates
#|    (gp:draw-path port `( (:line ,x-origin-pix ,y-origin-pix)(:line ,(+ x-origin-pix x-max)
                                                                     ,y-origin-pix)) x-origin-pix top-margin)|#

    (gp:draw-point port 200 200 )
    (gp:draw-path port `( (:line 0 ,y-max)(:line ,x-max ,y-max)) x-origin-pix top-margin)
    ;;draw the graph line
  ;;  (gp:draw-path port (cdr path-spec)  x-start y-start)
    ))
;;TEST
;;   ((LINE 50 100) (LINE 50 180) (LINE 50 -30) (LINE 50 40) (LINE 50 60))
;;  
#|(defun test-mdp (x y x-max y-max path-spec &key xy-margin-list)
  (setf out nil)
  (let
      ((test-interface (make-instance 'interface-display))
       )
    (with-slots (output-pane-1) test-interface
      (capi:display test-interface)
      ;; (draw-xy-coordinates output-pane-1 x y  x-max y-max)
      ;; (gp:draw-lines output-pane-1 path-spec)
      (my-draw-path output-pane-1 path-spec x-max y-max :xy-margin-list xy-margin-list )
      ;;  (gp:draw-path  output-pane-1 '((:LINE 50 100) (:LINE 100 180) (:LINE 150 -30) (:LINE 200 40) (:LINE 250 60))  50 200)
      (fout out)
      )))      |#
  ;;   
;; WORKS
;;  (test-mdp 50 50  500 600   '((0 400)(50 60)(100 100)(150 70)(200 250)(250 350)) :xy-margin-list '(50 100))
;;  (test-mdp 50 50  500 400   '((0 220)(20 20)(30 30)(40 40)(50 50)(60 60)) :xy-margin-list '(50 100))




;;MY-FIND-BEST-FONT
;;
;;ddd
(defun my-find-best-font (pane font-list)
  "In U-function-plotter, takes a font-list, uses gp:make-font-description in  gp:find-best-font to make a font. LIST uses key-words :family :weight :size"
  (let*
      ((family "Times New Roman") 
       (weight :normal)
       (size 10)
       (font)
       (n  0)
       )
    (unless (or (null font-list) (equal font-list 'NIL)(< (list-length font-list) 2))
      (dolist (item font-list)
        (incf n)
        (if (equal item :family)
            (setf family (nth n font-list)))
        (if (equal item :weight)
            (setf weight (nth n font-list)))
        (if (equal item :size)
            (setf size (nth n font-list)))
        ;;end dolist, unless
        ))
       (setf font (gp:find-best-font pane 
                                 (gp:make-font-description 
                                    :family family :weight weight  :size size)))
    ))



;;MAKE-PLOTTER-POINTS
;;2018
;;ddd
(defun make-plotter-points (pt-coords-lists &optional times labels  &key info-items-lists 
                                            list-default-label)
  "In   RETURNS (values  plotter-pts-list n-pts) where each item= (time coords label info-items-list) LIST-DEFAULT-LABEL lists the label for default (eg. \"\") Also see FIND-XY-POINTS.  "
  (let
      ((plotter-pts-list)
       (n-pts (list-length pt-coords-lists))
       )
    (loop
     for pt-coords in pt-coords-lists
     for n from 1 to n-pts
     for nth from 0 to n-pts
     do    
     (let*
         ((time (nth nth times))
          (label (nth nth labels))
          (info-items (nth nth info-items-lists))
          (plotter-pt-list) 
          )
     (unless time
       (setf time n))
     (setf  plotter-pt-list (list time pt-coords))
     (cond
      (label
       (setf plotter-pt-list (append plotter-pt-list (list label))))
      (list-default-label
       (setf plotter-pt-list (append plotter-pt-list (list list-default-label)))))       
     (when info-items
       (setf plotter-pt-list (append plotter-pt-list info-items)))
       ;;append
       (setf plotter-pts-list (append plotter-pts-list (list plotter-pt-list)))
       ;;end let,loop
       ))
    (values  plotter-pts-list n-pts)
    ;;end let, make-plotter-points
    ))
;;TEST
;; (make-plotter-points '((10 15)(20 25)(30 45)))
;; works= ((1 (10 15)) (2 (20 25)) (3 (30 45)))  3
;; (make-plotter-points '((10 15)(20 25)(30 45)) nil nil :list-default-label "label")
;; works= ((1 (10 15) "label") (2 (20 25) "label") (3 (30 45) "label"))   3
;; (make-plotter-points '((10 15)(20 25)(30 45)) '(t0 t1 t2 t3) '("lab1" "lab2""lab3") :info-items-lists '((a b c)(d e f)(h i j)))
;; works= ((T0 (10 15) "lab1" A B C) (T1 (20 25) "lab2" D E F) (T2 (30 45) "lab3" H I J))   3





;;MAKE-POINT
;;
;;ddd
(defmacro make-point (output-list time coordinates-list label &rest info-items)
  "In function-plotter.lisp, makes a point at a specific time-point and appends it to an ordered master-list=output-list. WHERE output-list =  ((time0  (x1 x2 ... xn) \"label\" extra stuff goes here) (time1 (x1 x2 .. xn) etc)). RETURNS (values  ,time ,coordinates-list  ,label ,info-items ,output-list)"
  (cond
   (info-items
    (eval `(setf ,output-list (append  ,output-list (list (list ,time ,coordinates-list ,label ,info-items))))))
   (t (eval `(setf ,output-list (append  ,output-list (list (list ,time ,coordinates-list ,label )))))))
  `(values  ,time ,coordinates-list  ,label ,info-items ,output-list))
;;TEST
;;   (setf  xl  '((t0 (0 0 0))))
;;  (make-point xl 't1  '(10 20 30) "Label"))
;; works= T1  10 20 30)  "Label"  NIL  ((T0 (0 0 0)) (T1 (10 20 30) "Label"))






;;GET-POINT
;;ddd
(defun get-point (output-list &key time coordinates-list label)
    "In function-plotter.lisp, gets a point and stored info from either specific time-point coordinates-list, or label"
    (let
        ((matching-point)
         )
      (dolist (point output-list)
      (cond 
       (time
        (if (equal time (car point))
            (return (setf matching-point point))))
        (coordinates-list
         (if (equal coordinates-list (second point))
              (return (setf matching-point point))))
        (label
         (if  (equal (third point) label)
              (return (setf matching-point point))))
        
        (t nil)))))
;;TEST using make-point above
;;  (get-point xl  :time 't1) = (T1 (10 20 30) "Label")
;; (get-point xl  :label "Label") = (T1 (10 20 30) "Label")
;; (get-point xl :coordinates-list '(10 20 30)) = (T1 (10 20 30) "Label")






;;-------------------------- TEST AREA --------------------------------------
;;

;;; The display callback. 
;;SSS START HERE -- LEARN HOW TO SPECIFY FONT-- MAKE SMALLER
;;  FOR MY GRAPH





;;DRAW-XY-COORDINATES;;
;;ddd
(defun draw-xy-coordinates (pane x y  x-max y-max)   ;; path-spec path-x path-y &key closed)
  "In function-plotter.lisp, draws a set of x, y coordinates--x y is top left corner"
  (let
      ((path-spec `((:line  0  ,y-max )(:line  ,x-max  ,y-max)))
       )
   ;; (afout 'out (format nil "path-spec= ~A~% x=~A y= ~A~%" path-spec x y))
      (gp:draw-path pane path-spec  x y)
      ) )
;;TEST
#|(defun test-xyc ( x y  x-max y-max)
  (let
      ((test-interface (make-instance 'interface-display))
       )
    (with-slots (output-pane-1) test-interface
      (capi:display test-interface)
      (draw-xy-coordinates  output-pane-1  x y  x-max y-max)
      )
    ))|#
;;  (test-xyc 20 20 300 200) ;;works









;;------------------ OTHER TESTS ---------------------------
;;TEST
#|(defun test-dls (path-spec)
  (let
      ((test-interface (make-instance 'interface-display))
       )
    (with-slots (output-pane-1) test-interface
      (capi:display test-interface)
      (gp:draw-lines output-pane-1 path-spec)
      )
    ))|#
;;  (test-dls '(x1 y1 x2 y2     x3 y3 x4 y4   etc)  ;;each line is 2 points
;;  (test-dls '(10 10 20 20   30 30 40 40)) ;;works
;;  (test-dls '(10 10 20 50 20 50 150 190 150 190 30 10 30 10 60 100)) ;;works
;;

;;
#|(defun test-dp (path-spec  x y)
  (let
      ((test-interface (make-instance 'interface-display))
       )
    (with-slots (output-pane-1) test-interface
      (capi:display test-interface)
      (gp:draw-path output-pane-1 path-spec  x y)
      )    ))|#
;;  (test-dp '((:line 100 0) (:line 100 100)) 40 30)
;;  (test-dp '((:line 200 0) (:line 200 200)) 10 200)
;;  (test-dp '((:line 0 300) (:line 500 300)) 60 10)  ;;coordinate system


;;(test-dpm '(10 10 30 40 60 90 100 130))
#|(defun test-dpm (path-spec)
  (let
      ((test-interface (make-instance 'interface-display))
       )
    (with-slots (output-pane-1) test-interface
      (capi:display test-interface)
      (my-draw-lines output-pane-1 path-spec)
      )
    ))|#
        




;;INTERFACE-DISPLAY
;;
;;ddd
(capi:define-interface interface-display ()
  ()
  (:panes
   (output-pane-1
    capi:output-pane
    :display-callback 'graph-display-callback
    :visible-min-height 650
    :visible-min-width 800
    ))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(output-pane-1)))
  (:default-initargs
   :best-height 750
   :best-width 900
   :layout 'column-layout-1
   :title "Interface-Display"))





;;FIND-XY-POINTS
;;2018
;;ddd
(defun find-xy-points ( all-calc-outputs  xn  &key label-list (default-label "") (y-nth 1)
                                     (pointn-n 3)    (labeln 4))
  "U-function-plotter. INPUT: all-outputs from calc-equation-for-delta-varlists. One OUTPUT, Eg (3.6 (1 3 0.8 2) (0 0 0 0) 1 \"1st cycle\")  RETURNS (values xy-points  npts )  NOTE: If (listp XN) will return multidim point (eg x, y, z). LABEL-LIST overides any labels in output. Y-NTH is place to move value of y in final xy-point list xyzetc coordinates. Also see MAKE-PLOTTER-POINTS"
  (let
      ((xy-points)
       (npts (list-length all-calc-outputs))
       )
    (loop
     for output in all-calc-outputs
     for n from 1 to npts
     do
     (let*
         ((x)  
          (y (car output))
          (xylist )
          (label)
          (pointn (nth pointn-n output))
          )
       ;;override (or add) label
       (when label-list
         (setf label (nth (- n 1) label-list)))
       ;;If no pointn, add one
       (when (null pointn)
         (setf pointn n))
       (when (null label)
         (setf label  default-label))

       (cond
        ((integerp xn)
         (setf xylist (list (nth xn (second output)))))
        ((and xn (listp xn))
         (loop
          for n in xn
          do
          (let
              ((val (nth n (second output)))
               )         
            (setf xylist (append  xylist (list val)))
            ;;end inner let,loop,clause
            )))
        (t nil))
       (setf xylist (append-nth y y-nth xylist)
        point (list pointn xylist label)
        xy-points (append xy-points (list point)))    
       ;;end let,loop
       ))
    ;;(break "end find-xy-points")
    (values xy-points  npts )
    ;;end let, find-xy-points
    ))
;;TEST
;; (find-xy-points '((5 (1 2 3)(2 2 .8))(6 (3 4 5)(2 3 .7))) 1)
;; works= ((1 (2 5) "") (2 (4 6) ""))  2
;; (find-xy-points '((5 (1 2 3)(2 2 .8) 3)(6 (3 4 5)(2 3 .7) 4)) 1 :label-list '("label 1""label 2"))
;; works= ((3 (2 5) "label 1") (4 (4 6) "label 2"))   2
;; FOR SHORTER OUTPUT LIST-(no deltavals), set :pointn 2 :labeln 3
;; (find-xy-points '((5 (1 2 3))(6 (3 4 5))) 1 :POINTN 2 :LABELN 3)
;; works= ((1 (2 5) "") (2 (4 6) ""))  2
;; MULTI-DIM w/labels
;;  (find-xy-points '((5 (1 2 3) 10 "pt1")(6 (3 4 5) 20 "pt2")) '(0 2)  :pointn 2 :labeln 3)
;; works= ((10 (1 5 3) "pt1") (20 (3 6 5) "pt2"))    2







;;**************** OLD DELETE?? *****************

;;
#| OLD-IRRELEVANT-DELETE??
(setf *path-spec '((:line 100 0) (:line 100 100)))
;;
;;(test-dl *path-spec 10 10)
;;
(defun test-dl (path-spec x y)
  (let
      ((test-interface (make-instance 'interface-display))
       )
    (with-slots (output-pane-1) test-interface
      (capi:display test-interface)
      (gp:draw-lineS output-pane-1 '(5 20   15 40      100 100   120 120))
      ;;(gp:draw-lines output-pane-1 path-spec x y)
      )
    )) |#


;;DELETE IF CHANGES WORK
#|
(defun draw-graph-line (port point-list x-max y-max x-margin y-margin &key Graph-color  (x-multiplier 1.0) ( y-multiplier 1.0) floor-value )
  "In function-plotter.lisp, uses (list time coordinates-list label) as basic data element"
  (let
      ((t1 0)
       (x1 0)
       (y1 0)
       (label1)
       (t2 0)
       (coordinates)
       (x2 0)
       (y2 0)
       (label2)
       (n 0)
       (xy-coord-list)
       (xy-spec-list)
       (x1-pix 0)
       (y1-pix 0)
       (x2-pix 0)
       (y2-pix 0)
       (x-origin-pix 0)
       (y-origin-pix 0)
       (pane-height (capi::pane-height port))
       (pane-width (capi::pane-width port))
       (top-margin 20)
       (right-margin 20)
       )
    ;;should have been determined in let above
    (unless pane-height (setf pane-height 100))
      ;;determine placement and margins
      (setf top-margin (-  pane-height (+ y-max y-margin))
            right-margin (- pane-width (+ x-max x-margin))
            x-origin-pix  x-margin
            y-origin-pix  (+ y-max top-margin))

     (if (> *print-details 1) (afout 'out (format nil "y-margin= ~A x-margin= ~A y-origin-pix= ~A~%" y-margin x-margin y-origin-pix)))

   (dolist (point point-list)
      (incf n)
      (cond
       ((listp point)
      (cond
       ((= n 1)
        (setf t1 (car point)
              coordinates (second point)
              label1 (third point))
          (setf x1(* (car coordinates) x-multiplier)
                y1 (* (second coordinates) y-multiplier)
                x1-pix (+ x-origin-pix x1)
                y1-pix (- y-origin-pix y1)
                ;;more code here if want more than 2 dimensions
                ))
       (t (setf t2 (car point)
              coordinates (second point)
              label2 (third point))
          (setf x2 (* (car coordinates) x-multiplier)
                y2 (* (second coordinates) y-multiplier)
                x2-pix (+ x-origin-pix x2)
                y2-pix (- y-origin-pix y2))
                ;;more code here if want more than 2 dimensions
                (if (>  *print-details 1)   (afout 'out (format nil "In draw-graph-line, x-multiplier= ~A x1= ~A y-multiplier= ~A y1= ~A~%" x-multiplier x1 y-multiplier y1)))
               (if (>  *print-details 1)  (afout 'out (format nil "coordinates= ~A x2=~A x2-pix= ~A y-margin= ~A x-margin= ~A y-origin-pix= ~A~%" coordinates x2 x2-pix y-margin x-margin y-origin-pix)) )

      ;;add to draw-lines spec list
      (setf xy-spec-list (append xy-spec-list (list x1-pix y1-pix x2-pix y2-pix))
            xy-coord-list (append xy-coord-list (list x1 y1 x2 y2)))
     (if (>  *print-details 1)  (afout 'out (format nil "x1-pix= ~A y1-pix= ~A x2-pix= ~A y2-pix= ~A~%"  x1-pix y1-pix x2-pix y2-pix)))
      
      (gp:draw-line port x1-pix y1-pix x2-pix y2-pix :foreground graph-color)
      ;;reset the initial point for next cycle
      (setf x1 x2 y1 y2 x1-pix x2-pix  y1-pix y2-pix t1 t2 label1 label2)
      )
      ;;end dolist, etc.
      ))))
   ))
|#

#|  OLD VERSION--DELETE?
(defun draw-graph-window (port point-list x-max y-max x-margin y-margin 
                               &key begin-num (label-margin 10) (graph-color :red)
                             x-mark-interval y-mark-interval half-hash-length  label-margin
                             x-label-color  y-label-color
                             x-label-list  y-label-list
                              (x-increment 10)  (y-increment 10)
                             x-string y-string ( string-margin 10)
                             y-element-space 
                             x-font-list y-font-list  (x-string-color :black) (y-string-color :black)
                             center-x-string-p center-y-string-p
                            (x-multiplier 1.0) (y-multiplier 1.0)  y-floor-value)
    "In function-plotter.lisp,   ARGS(port point-list x-max y-max x-margin y-margin 
                               &key begin-num  label-margin graph-color
                             x-label-list y-label-list
                             x-mark-interval y-mark-interval half-hash-length  label-margin
                              x-increment  y-increment
                             x-label-color  y-label-color
                             x-string y-string string-margin y-element-space
                             center-x-string-p center-y-string-p
                              x-multiplier y-multiplier Y-FLOOR-VALUE WON'T allow y to go below that value if not NIL."
     (afout 'out (format nil "XXXX In draw-graph-window, :x-multiplier= ~A :y-multiplier= ~A~% POINT-LIST= ~A~%" x-multiplier  y-multiplier point-list))

;;EXTRA INTERFACE MADE HERE--DON'T USE THIS FOR PRODUCTION
  (let
      ((test-interface)  
       )
    (unless port
      (setf test-interface  (make-instance 'interface-display)))

    (with-slots (output-pane-1) test-interface
      (cond
       ((null port)
        (setf port output-pane-1)
      (capi:display test-interface))
       (t nil))
;;mmm
;;(defun draw-xy-axes (port x-max y-max x-margin y-margin)
#|was    (draw-xy-axes port x-max y-max x-margin y-margin
                   :x-mark-interval 20 :y-mark-interval 20 :half-hash-length 3)|#

      ;;DRAWS XY COORDINATES (below)
      (draw-xy-axes port x-max y-max x-margin y-margin 
                          :x-mark-interval x-mark-interval :y-mark-interval y-mark-interval
                          :half-hash-length half-hash-length :label-margin label-margin
                          :x-label-list x-label-list :y-label-list y-label-list
                           :x-increment x-increment :y-increment y-increment
                          :x-label-color x-label-color :y-label-color y-label-color
                          :x-string x-string :y-string y-string :string-margin string-margin
                          :x-font-list x-font-list  :y-font-list y-font-list
                           :x-string-color x-string-color :y-string-color y-string-color
                          :y-element-space y-element-space
                          :center-x-string-p center-x-string-p :center-y-string-p center-y-string-p)

   (afout 'out (format nil "BEFORE draw-graph-line, port point-list= ~A~%  x-max= ~A y-max= ~A x-margin= ~A y-margin= ~A~% :graph-color= ~A  x-multiplier= ~A~%   y-multiplier= ~A~%"  point-list x-max y-max x-margin y-margin graph-color  x-multiplier   y-multiplier) "C:\\TOM\\LISP PROJECTS TS\\ART-LW\\outfile.lisp")

      ;;MAIN GRAPH DRAWING FUNCTION (below)
     (draw-graph-line port point-list x-max y-max x-margin y-margin :graph-color graph-color :x-multiplier x-multiplier  :y-multiplier  y-multiplier :y-floor-value  y-floor-value)
;;
     ;; (fout out)
      )))
|#


;;EXTRA FROM ABOVE ---------------- DELETE LATER ------------

       #|(draw-graph-window port ,all-points-lists
                       x-max y-max x-margin y-margin 
                       :interface-class interface-class ;; :output-pane output-pane
                       :multiple-graphs-p multiple-graphs-p :append-point-list append-point-list
                       :begin-num begin-num :label-margin label-margin
                       :graph-color graph-color :graph-color-list  graph-color-list
                       :x-mark-interval x-mark-interval :y-mark-interval y-mark-interval
                       :half-hash-length half-hash-length :label-margin label-margin
                       :x-label-list  x-label-list :y-label-list  y-label-list
                       :x-label-color x-label-color :y-label-color y-label-color 
                       :x-label-type x-label-type
                       :x-increment x-increment :y-increment y-increment
                       :x-string x-string :y-string y-string :x-string-margin x-string-margin
                       :y-string-margin y-string-margin :y-element-space y-element-space
                       :x-font-list  x-font-list :y-font-list y-font-list
                       :x-string-color x-string-color :y-string-color y-string-color
                       :center-x-string-p center-x-string-p   :center-y-string-p center-y-string-p  
                       :x-multiplier x-multiplier :y-multiplier y-multiplier
                       :y-floor-value  y-floor-value
                       )|#        

    ;;(break "after make-callb")
    ;;MAKE WINDOW, DRAW CURVES
    #|(draw-graph-window port all-points-lists
                       x-max y-max x-margin y-margin 
                       :interface-class interface-class ;; :output-pane output-pane
                       :multiple-graphs-p multiple-graphs-p :append-point-list append-point-list
                       :begin-num begin-num :label-margin label-margin
                       :graph-color graph-color :graph-color-list  graph-color-list
                       :x-mark-interval x-mark-interval :y-mark-interval y-mark-interval
                       :half-hash-length half-hash-length :label-margin label-margin
                       :x-label-list  x-label-list :y-label-list  y-label-list
                       :x-label-color x-label-color :y-label-color y-label-color 
                       :x-label-type x-label-type
                       :x-increment x-increment :y-increment y-increment
                       :x-string x-string :y-string y-string :x-string-margin x-string-margin
                       :y-string-margin y-string-margin :y-element-space y-element-space
                       :x-font-list  x-font-list :y-font-list y-font-list
                       :x-string-color x-string-color :y-string-color y-string-color
                       :center-x-string-p center-x-string-p   :center-y-string-p center-y-string-p  
                       :x-multiplier x-multiplier :y-multiplier y-multiplier
                       :y-floor-value  y-floor-value)|#  

    #|(eval `(with-slots (,output-pane) ,win-inst
             (draw-graph-window ,output-pane (quote ,all-points-lists)
                                ,x-max ,y-max ,x-margin ,y-margin 
                                :multiple-graphs-p ,multiple-graphs-p :append-point-list ,append-point-list
                                :begin-num ,begin-num :label-margin ,label-margin
                                :graph-color ,graph-color :graph-color-list (quote ,graph-color-list)
                                :x-mark-interval ,x-mark-interval :y-mark-interval ,y-mark-interval
                                :half-hash-length ,half-hash-length :label-margin ,label-margin
                                :x-label-list (quote ,x-label-list) :y-label-list  (quote ,y-label-list)
                                :x-label-color ,x-label-color :y-label-color ,y-label-color 
                                :x-label-type ,x-label-type
                                :x-increment ,x-increment :y-increment ,y-increment
                                :x-string ,x-string :y-string ,y-string :x-string-margin ,x-string-margin
                                :y-string-margin ,y-string-margin :y-element-space ,y-element-space
                                :x-font-list (quote ,x-font-list) :y-font-list (quote ,y-font-list)
                                :x-string-color ,x-string-color :y-string-color ,y-string-color
                                :center-x-string-p ,center-x-string-p   :center-y-string-p ,center-y-string-p  
                                :x-multiplier ,x-multiplier :y-multiplier ,y-multiplier
                                :y-floor-value  ,y-floor-value)  
             ;;end with-slots,eval
             ))|#



;;hhh *************************** HELP *****************************
        ;;              x-string (format nil "~%~A~%  ~{~S= ~S  ~} " graph-n-list graph-color-list))
        ;; (format nil "~%~A~%  ~{~S= ~S  ~} "  pane-label point-info-list))
        ;; (format nil "~%~A  ~{~S= ~S  ~} "  "this" '(a b c d)) = this   A= B  C= D   "
        ;; (format nil "~%~A  ~{ ~S  ~} "  "this" '(a b c d))  = this   A   B   C   D   " 
        ;; (format nil "~%~A  ~{ ~S  ~}  ~{ ~S  ~} "  "this" '(1 2 3 4)  '(a b c d)) = this   1   2   3   4     A   B   C   D   "