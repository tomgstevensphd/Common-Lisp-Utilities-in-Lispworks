;; *********************************** U-capi.lisp ********************************
;;
;;My general CAPI UTILITY FUNCTIONS

;;USE CAPI:PROMPT-FOR-*   FUNCTIONS, JUST USE TAB
;;EG (capi:prompt-for-confirmation
#|DESCRIPTION
The function prompt-for-confirmation displays a dialog box containing message , with Yes and No buttons. When either Yes or No is pressed, it RETURNS TWO VALUES:
    A boolean indicating whether Yes was pressed.
    t (for compatibility with other prompt functions).
CANCEL-BUTTON specifies whether a Cancel button also appears on the dialog. When Cancel is pressed, abort is called and the dialog is dismissed. The default value of cancel-button is nil.
DEFAULT-BUTTON specifies which button HAS THE INPUT FOCUS when the dialog appears (and is thus selected when the user immediately presses Return).The value :ok means Yes , the value :cancel means Cancel , and any other value means No . The default value of default-button is nil.
OWNER specifies an owner window for the dialog. See Dialogs: Prompting for Input for details.
If CONTINUATION is non-nil, then it must be A FUNCTION WITH A LAMBDA LIST THAT ACCEPTS TWO ARGUMENTS. The continuation function is called with the values that would normally be returned by prompt-for-continuation. The with-dialog-results macro provides a convenient way to create a continuation function.
EXAMPLE|#
;;(capi:prompt-for-confirmation "Continue?")
;;RESULT, If yes returns T,T; If no, returns NIL,T
;;Example
#|(multiple-value-bind (res success)
    (capi:prompt-for-confirmation  "Yes, No or Cancel" :cancel-button t)
  (if success res
    (abort)))|#
;;RESULT of above If yes, returns T, if no NIL, if cancel aborts.



;;MAKE-CALLBACK-TO-RUN-FUNCTION
;;2018
;;ddd
(defmacro make-callback-to-run-function (callback-sym callback-args run-func-call-list
                                                &key data  x y width height (eval-inner-func-p T) 
                                                makunbound-func-p)
  "U-capi   RETURNS a NEW defuned callback function = callback-sym with args eg (interface x y width height) for callback-display.   INPUT:  the callback-sym in the interface and as long as this macro is run ahead of time, then the inner function should be called WITH THE INTERFACE=CALLING INTERFACE and REST OF ARGS whatever is put in the run-func-call-list WHEN THE MACRO IS CREATED. "
  (let
      ((zz)
       )
    ;;(setf *macout
    (eval
    `(defun ,callback-sym ,callback-args
           ,run-func-call-list
         )    
    ;;end setf
     )
    ;;following doesn't work.  Not needed because just put the callback-sym in the interface and as long as this macro is run ahead of time, then the inner function should be called WITH THE INTERFACE=CALLING INTERFACE and REST OF ARGS whatever is put in the run-func-call-list WHEN THE MACRO IS CREATED.
#|    (when eval-inner-func-p
      (multiple-value-bind (a b c d e f g h)
          (eval `(,callback-sym ,interface))       ;; `(funcall #',callback-sym interface)
        (values a b c d e f g h)))|#
    (when makunbound-func-p 
      (makunbound callback-sym))
    ;;end mvb,let, make-callback-to-run-function
    ))
;;TEST
;;  (defun test-make-callback (interface a b c)     (list interface a b c))
;; (make-callback-to-run-function testcallback (interface)  (test-make-callback interface 'x 'y 'z)) 
;; works: CL-USER 90 > (testcallback 'xx)  = (XX X Y Z) [note: xx is interface passed thru
;;test part
;;(setf *test-all-points-lists '(((1 (3 3.6) "") (2 (5 5.2) "") (3 (7 6.7999997) "") (4 (9 8.4) "") (5 (11 10.0) "") (6 (13 11.6) "") (7 (15 13.200001) "") (8 (17 14.800001) "") (9 (19 16.400002) "") (10 (21 18.000002) ""))))
;; (make-callback-to-run-function graph-display-callback1 (port x y width height)   (draw-graph-window port *test-all-points-lists )   :eval-inner-func-p t :makunbound-func-p NIL )

;;


;; xxx --------------------- TEST OF MACRO-- IT WORKS -------------------------------------
#|
;;1. DEFINE interface-test1
(capi:define-interface interface-test1 ()
  ()
  (:panes
   (output-pane-1
    capi:output-pane
    :visible-min-width 150
    :visible-min-height 150
    :display-callback 'test-made-display-callback
    ))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(output-pane-1)))
  (:default-initargs
   :best-height 300
   :best-width 300
   :layout 'column-layout-1
   :title "Interface-1"))

;;2. WRITE AN INNER FUNCTION REALLY WANT TO USE INDEPENDENTLY
(defun my-test-write-to-outpane (pane string x  y)
        (gp:draw-string pane string x y ))

;;3. MAKE THE DISPLAY-CALLBACK USING THE MACRO & FUNC IN 2.
(make-callback-to-run-function test-made-display-callback (interface x y width height)
                         (my-test-write-to-outpane interface "This is a test string"  30 40)
                    ;;     :x x :y y :width width :height height
                         :eval-inner-func-p NIL)

;;4. WRITE THE CALLING FUNCTION TO MAKE THE INTERFACE
;;  The callback-display function (with required args interface x y width height) fills in the output of the output pane automatically when pane is made.
;; USE OF CALLBACK-DISPLAY REWRITES OUTPUT EVEN IF MINIMIZED, ETC.
(defun my-test-display-callbackXX ()
  (let
      ((inst (make-instance 'interface-test1))
       )
    (capi:display inst)
    ))
;;TEST
;; (my-test-display-callbackXX)
;; works= prints "this is a test string" in the output pane of interface-test1 
 |#
                            
;; --------------------- END TEST -----------------------------




;;MY-CONFIRM
;;
;;ddd
(defun my-confirm (message &key extra-info  question-string cancel-button owner 
                           callback-func)
  "In U-capi,  callback-func must have two args for callback-func.  Can't get return value from the callback-func [must set a global var or slot-value?]"
  (let ((callback-func-result)
        )
    (multiple-value-bind (confirmed-p not-cancelled-p callback-func-result)
        (capi:prompt-for-confirmation message :extra-info extra-info 
                                      :question-string question-string :cancel-button cancel-button
                                      :continuation callback-func)
      (values confirmed-p not-cancelled-p) ;;doesn't work? callback-func-result)
              )))
;;TEST
;; (my-confirm "CONFIRM?" :question-string "Qutestion String"  :extra-info "Extra info" :cancel-button T)
;; RESULTS  yes= T,T,NIL  no= NIL,T,NIL cancel= NIL,NIL,NIL
;; For callback-func
;; (my-confirm "CONFIRM? callback function" :question-string "Qutestion String"  :extra-info "Extra info" :cancel-button T :callback-func 'text-callb-func)
;;RESULTS  WORKS, RETURNS :CONTINUATION, NIL, NIL  can't get return value from the continuation funtion tho.  POPPED UP THE SELECT COLOR WINDOW BELOW.
#|(defun text-callb-func (a b)
  (multiple-value-bind (c d e)
  (CAPI:PROMPT-FOR-COLOR "Select Color")
  (values a b c d e)
  (list a b c d e)
  ))|#


(defun my-select-font ()
  "In U-capi.lisp"
  (CAPI:PROMPT-FOR-FONT "Select Font" )
  )


(defun my-select-color ()
  "In U-capi.lisp"
  (CAPI:PROMPT-FOR-COLOR "Select Color")
  )

(defun my-define-new-color (&optional color-sym)
  "In U-capi.lisp"
  (let
      ((new-color (CAPI:prompt-for-color "Select Color"))
       )
    (when color-sym
    (set color-sym new-color))
    (values new-color new-color-sym)
    ))

;;CENTERING PANES AND WINDOWS
;; ALSO USE :best-x :center and :best-y :center
;;
(defun center-window (element)
  "From LispHUG, Nick Levine for centering a CAPI element OR a window on a screen-see def below"
  (let ((interface  (capi:element-interface element)))
    ;;Camille said to use for centering window on screen must use following:
    ;;  (capi:top-level-interface element) instead
    (multiple-value-bind (i-left i-top i-width i-height)
        (capi:top-level-interface-geometry interface)
      (declare (ignore i-left i-top))
      Multiple-value-bind (s-left s-top s-width s-height)
      (capi:screen-internal-geometry (capi:element-screen interface))
      (let ((s-center-x(+ s-left (/ s-width 2)))
            (s-center-y (+ s-top (/ s-height 2))))
        (capi:set-top-level-interface-geometry interface
                                               :x (- s-center-x (/ i-width 2))
                                               :y (- s-center-y (/ i-height 2)))))))



;;NONSELECTED&SELECTED-BUTTONS
;;
;;ddd
(defun nonselected&selected-buttons (selected-buttons all-selected-buttons)
  "In U-CAPI, from a list of selected buttons eg (\"item2\") and list of all-button-selections eg (\"item2\" \"item2\" \"item3\"), RETURNS (values nonsel-items nonsel-item-nums  sel-item-nums). num begins at 1."
  (let
      ((nonsel-item-nums)
       (nonsel-items)
       (sel-item-nums)
       )
    (loop
     for button in all-selected-buttons
     for n from 1 to (list-length all-selected-buttons)
     do
     (cond
      ((member button selected-buttons :test 'string-equal)
       (setf sel-item-nums (append sel-item-nums (list n))))
      (t
       (setf nonsel-item-nums (append nonsel-item-nums (list n))
             nonsel-items (append nonsel-items (list button)))))
     ;;end loop
     )
    (values nonsel-items nonsel-item-nums  sel-item-nums)
    ;;end let, nonselected&selected-buttons
    ))
;;TEST
;;  (nonselected&selected-buttons '("item2")'("item1"  "item2" "item3"))
      
    


;;SET-RICH-TEXT-PANES-TEXT
;;2017
;;ddd
(defun set-rich-text-panes-text (interface pane text)
  "In U-capi, sets text in pane to reset-text. NOT in-process"   
  (loop
   for pane in panes
   for n from 0 to (list-length panes)
   do
   (cond
    (text
     (setf text1 text))
    (t 
     (setf text1 (nth n textlist))))
   (eval 
    `(with-slots (quote ,pane) ,interface
       (setf (capi:rich-text-pane-text ,pane) ,text)
       )
    ;;end  with, eval
    ))
  ;;end loop
  )



;;SET-PANES-TEXT
;;2017
;;ddd
(defun set-panes-text (interface pane capi-set-function text)
  "In U-capi, sets text in pane to reset-text. Eg 'capi:title-pane-text.  NOT IN-PROCESS"   
  (loop
   for pane in panes
   for n from 0 to (list-length panes)
   do
   (cond
    (text
     (setf text1 text))
    (t 
     (setf text1 (nth n textlist))))
   (eval 
    `(with-slots (quote ,pane) ,interface
       (setf (,capi-set-function  ,pane) ,text)
       )
    ;;end  with, eval
    ))
  ;;end loop
  )



;;SET-TITLE-PANES-TEXT-IN-PROCESS
;;
;;ddd
(defun set-title-panes-text-in-process (interface panes &key text textlist)
  "In U-capi, sets text in title-pane to text (if not nil) or nth in textlist. SETS IN PROCESS-after interface created."
  (loop
   for pane in panes
   for n from 0 to (list-length panes)
   do
   (cond
    (text
     (setf text1 text))
    (t 
     (setf text1 (nth n textlist))))
   (eval 
    `(with-slots (quote ,pane) ,interface
       (capi:apply-in-pane-process ,interface 
                                   #'(setf  capi:title-pane-text) ,text1 ,pane)
       ;;end  with, eval
       ))
   ;;end loop
   )
  ;;*deftest
  ;;end set-title-panes-text-in-process
  )
;;TEST
;;  (progn (setf *tinst2 (make-instance 'EXPLORE-DIRS-INTERFACE) ) (capi:display *tinst2))
;; (set-title-panes-text-in-process *tinst2 '(name-title-pane ) :text "THIS IS A TEST")
;; works, sets  pane to the text



;;SET-RICH-TEXT-PANES-TEXT-IN-PROCESS
;;2017
;;ddd
(defun set-rich-text-panes-text-in-process (interface panes &key text textlist)
  "In U-capi, sets text in pane to text (if not nil) or nth in textlist.SETS IN PROCESS-after interface created."
  (loop
   for pane in panes
   for n from 0 to (list-length panes)
   do
   (cond
    (text
     (setf text1 text))
    (t 
     (setf text1 (nth n textlist))))
   (eval 
    `(with-slots (quote ,pane) ,interface
       (capi:apply-in-pane-process ,interface 
                                   #'(setf  capi:rich-text-pane-text) ,text1 ,pane)
       ;;end  with, eval
       ))
   ;;end loop
   )
  ;;*deftest
  ;;end set-rich-text-panes-text-in-process
  )
;;TEST
;;  (setf *tinst (make-radio-button-choice-instance) )(textlist '("a" "b" "c") ))
;; (set-rich-text-panes-text-in-process *tinst '(title-pane info-pane) :textlist  '("aaa" "bbb"))
;; works, sets both panes to the text




;;SET-PANES-TEXT-IN-PROCESS
;;2017
;;ddd
(defun set-panes-text-in-process (interface panes  capi-set-function &key text textlist)
  "In U-capi, sets text in pane to text (if not nil) or nth in textlist.SETS IN PROCESS-after interface created. Eg.  'capi:rich-text-pane-text for rich-text-pane."
  (loop
   for pane in panes
   for n from 0 to (list-length panes)
   do
   (cond
    (text
     (setf text1 text))
    (t 
     (setf text1 (nth n textlist))))
   (eval
    `(with-slots (quote ,pane) ,interface
       (capi:apply-in-pane-process ,interface 
                                   #'(setf  ,capi-set-function ) ,text1 ,pane)
       ;;end  with, eval
       ))
   ;;end loop
   )
  ;;*deftest
  ;;end set-panes-text-in-process
  )
;;TEST
;;  (setf *tinst (make-radio-button-choice-instance) )(textlist '("a" "b" "c") ))
;; (set-panes-text-in-process *tinst '(title-pane info-pane) 'capi:rich-text-pane-text :textlist  '("aaa" "bbb"))
;; works, sets both panes to the text
;; (progn (setf *restinst (make-instance 'explore-dirs-interface)) (capi:display *restinst))
;;
;; FOR MULTI-COLUMN-LIST-PANELS
;;(progn (setf *restinst (make-instance 'explore-dirs-interface)) (capi:display *restinst))
;;following works, but leaves parens at both ends and only uses first heading "title"
;; (set-panes-text-in-process  *restinst '(dir-list-panel-3) 'capi:collection-items  :TEXT  (quote (quote ((("ROW1 ITEM 1" "ITEM2" "ITEM3" "ITEM4"))(("ROW2 ITEM 1" "ITEM2" "ITEM3" "ITEM4")))))) 
;; (set-panes-text-in-process  *restinst '(dir-list-panel-3) 'capi:collection-items  :TEXT  (quote (quote ((("ROW1 ITEM 1")("ITEM2")( "ITEM3")( "ITEM4"))))))
;;(("ROW2 ITEM 1" "ITEM2" "ITEM3" "ITEM4"))))))
;; TO CLEAR EXCEPT FOR HEADINGS
;; (set-panes-text-in-process  *restinst '(dir-list-panel-3) 'capi:collection-items  :TEXT nil)  ;;





#|
xx CAPI:PROMPT-FOR-COLOR 
Lambda List: (MESSAGE &KEY COLOR COLORS PANE-ARGS POPUP-ARGS OWNER)
xxCAPI:PROMPT-FOR-CONFIRMATION 
xxxCAPI:PROMPT-FOR-DIRECTORY 
xxxCAPI:PROMPT-FOR-FILE 
CAPI::PROMPT-FOR-FILE-GET-CANONICAL-PATH 
xxCAPI:PROMPT-FOR-FILES 
xxCAPI:PROMPT-FOR-FONT 
Lambda List: (CAPI::MESSAGE &KEY CAPI::FONT CAPI::OWNER)

(CAPI::*DEFAULT-PROMPT-LIST-BORDER
Global Variables
* CAPI::*DEFAULT-PROMPT-LIST-HEIGHT-IN-CHARACTERS
* CAPI::*DEFAULT-PROMPT-LIST-MIN-HEIGHT-IN-CHARACTERS
* CAPI::*DEFAULT-PROMPT-LIST-MIN-WIDTH-IN-CHARACTERS
* CAPI::*PROMPT-FOR-FILE-ERROR-ON-BAD-ARG
* CAPI::CALL-PROMPT-WITH-LIST-CALLBACK 
Functions
CAPI::EXIT-RANDOM-TYPEOUT-PROMPT-INFO
xx CAPI::MAKE-CHOICE-FOR-PROMPT-WITH-LIST
xx CAPI:PROMPT-FOR-COLOR 
xxCAPI:PROMPT-FOR-CONFIRMATION 
xxxCAPI:PROMPT-FOR-DIRECTORY 
xxxCAPI:PROMPT-FOR-FILE 
CAPI::PROMPT-FOR-FILE-GET-CANONICAL-PATH 
xxCAPI:PROMPT-FOR-FILES 
xxCAPI:PROMPT-FOR-FONT 
xxCAPI:PROMPT-FOR-FORM 
CAPI:PROMPT-FOR-FORMS 
CAPI:PROMPT-FOR-INTEGER 
CAPI:PROMPT-FOR-ITEMS-FROM-LIST 
CAPI::PROMPT-FOR-LABEL 
CAPI:PROMPT-FOR-NUMBER 
CAPI:PROMPT-FOR-STRING 
CAPI:PROMPT-FOR-SYMBOL 
CAPI:PROMPT-FOR-VALUE 
CAPI::PROMPT-INFO 
CAPI:PROMPT-WITH-LIST 
CAPI:PROMPT-WITH-LIST-NON-FOCUS 
CAPI:PROMPT-WITH-MESSAGE 
CAPI-INTERNALS:CAPI-PROMPT-FOR-COLOR 
CAPI-INTERNALS:CAPI-PROMPT-FOR-DIRECTORY 
CAPI-INTERNALS:CAPI-PROMPT-FOR-FILE 
CAPI-INTERNALS:CAPI-PROMPT-FOR-FONT 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-COLOR 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-DIRECTORY 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-FILE 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-FILES 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-FONT 
CAPI-TOOLKIT:INTERFACE-PROMPT-FOR-FILE 
CAPI-TOOLKIT::INTERFACE-PROMPT-FOR-PACKAGE 
CAPI-TOOLKIT::PROMPT-ARGS 
CAPI-TOOLKIT::PROMPT-FOR-ALTERNATIVE-CLASS 
CAPI-TOOLKIT::PROMPT-FOR-ALTERNATIVE-SYMBOL 
CAPI-TOOLKIT::PROMPT-FOR-ALTERNATIVES 
CAPI-TOOLKIT::PROMPT-FOR-HISTORY 
CAPI-TOOLKIT:PROMPT-FOR-PACKAGE 
CAPI-WIN32-LIB::PROMPT-FOR-FILE-ARGS 
CAPI-WIN32-LIB::REPRESENTATION-PROMPT-FOR-COLOR 
CAPI-WIN32-LIB::WW-PROMPT-FOR-DIRECTORY 
CONDITIONS::PROMPT-FOR-FUNCTION
|#

