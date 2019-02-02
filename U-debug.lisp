;;******************************** U-debug.lisp *************************
;;
;; FUNCTIONS FOR DEBUGGING & PERFORMANCE TESTING
;;
;;EXTENDED-TIME -- TO MEASURE PERFORMANCE SEE INFO BELOW



;;SHOW-TEXT
;;ddd
(defun show-text (text min-height title-or-richtext 
                       &key min-width internal-border (x 20)(y 20))
  "U-debug, For debugging etc: shows text in a container window (title or richtext=anything but 'title)."
  (let ((rich-text-pane-show)
        (title-pane-show)
        )   
    (cond
     ((equal title-or-richtext 'title)
      (capi:contain
       (setf title-pane-show (make-instance 'capi:title-pane :name "title-pane1"
                             :visible-min-height  min-height  :visible-min-width min-width  :x x :y y 
                             :text  (format nil "~A" text)))))
     (t
      (capi:contain
       (setf rich-text-pane-show (make-instance 'capi:rich-text-pane :name "rich-text-pane-show" :visible-min-height  min-height  :visible-min-width min-width  :x x :y y 
                                                :text (format nil "~A" text)))
       ) ))
    ))
;; (show-text "test text" 300 "this is the title" :min-width 500 :internal-border 40)


(in-package "COMMON-LISP-USER")


;;FOUT -- A SUBSTITUTE FOR (FORMAT T USING WINDOWS
;;works
;;ddd

#|(defun tfout ()  
  (fout "test output 1") ;; "test-output1" "Fout OUTPUT PANES" )
    (fout "test output 2" "test-output2" "Fout OUTPUT PANES" )
    (fout (format nil "test fout 3 x= ~A" 99) "Test output pane 2" "OUTPUT PANES 3")
  )|#




;;AFOR
;;2017
;;ddd
(defun afor (fout-var text  &rest format-args) 
  "Use QUOTED VAR--creates a list of texts that are appended by afout onto the var fout-var--even if it's unbound"
  (let
      ((f-text (eval (append `(format nil ,text) (make-quoted-list-items format-args))))
       )
  (cond
   ((not (boundp fout-var))
      (set fout-var f-text))
   (t
    (set fout-var (format nil "~A~%~A~%" (eval fout-var) f-text))))
  #|(when filename
    (with-open-file (out-str filename :direction :output :if-exists :append :if-does-not-exist :create)
      (format out-str  "~A~%~%~A~%" (eval fout-var) text)
      ;;end when, with
      ))|#
  ;;end let,afor
   ))
;;TEST
#|(defun testaf ()
  (let
      ((x 7)
       (y  'this)
       )
    (afor 'out "This is a test x= ~A and y= ~A end" x y)
    ))|#


;;AFOUT
;;
;;ddd
(defun afout (fout-var text &optional filename)
  "Use QUOTED VAR--creates a list of texts that are appended by afout onto the var fout-var--even if it's unbound"
  (cond
   ((not (boundp fout-var))
      (set fout-var text))
   (t
    (set fout-var (format nil "~A~%~%~A~%" (eval fout-var) text))))
  (when filename
    (with-open-file (out-str filename :direction :output :if-exists :append :if-does-not-exist :create)
      (format out-str  "~A~%~%~A~%" (eval fout-var) text)
      ;;end when, with
      ))
   )


#|(defun afout (fout-var text)
  "Use QUOTED VAR--creates a list of texts that are appended by afout onto the var fout-var--even if it's unbound"
  (cond
   ((not (boundp fout-var))
      (set fout-var text))
   (t
    (set fout-var (format nil "~A~%~%~A~%" (eval fout-var) text)))
   ))|#

;;FOUT -- REPLACES (FORMAT NIL IN MY FUNCTIONS)
;;  USE AFOUT to create texts that are added together 
;; then FOUT TO CREATE 2 EDITABLE WINDOWS FOR OUTPUT
;;
;;ddd
(defun fout (pane1-text &optional pane2-text win-title &optional ide-buffer-p)
  "Use UNQUOTED VAR, in U-debug.lisp, An OUTPUT WINDOW for format based debugging--if I can
get it to work"
  (let
      ((fout-instance)
       )
    (if (null pane2-text) (setf pane2-text ""))
    (if (null win-title) (setf win-title ""))
    ;;make the instance and display it
    (capi:display
     (setf fout-instance (make-instance 'output-window)))
    ;;set the slots
     (with-slots (outpane1 outpane2 title-pane) fout-instance
         ;;write the text to rich text windows
         (if ide-buffer-p (setf (capi::output-pane-flags outpane1) NIL
                                (capi::output-pane-flags outpane2) NIL))         
        (setf (capi:editor-pane-text outpane1) pane1-text)
        (setf (capi:editor-pane-text outpane2) pane2-text)
       ;; (setf (capi:rich-text-pane-text outpane1) pane1-text)
        ;; (setf (capi:rich-text-pane-text outpane2) pane2-text)
          (setf (capi:rich-text-pane-text title-pane) win-title)        
     ;;eown't work  (setf (capi:interface-title instance-name) "Fout OUTPUT PANES")
        )
    ))
;;TEST
;;works
#|(defun testfout ()
  (afout 'thyz "this text")
  (afout 'thyz "this text2")
  (fout thyz))|#
   



;;MAKE-QUOTED-LIST-ITEMS
;;2017
;;ddd
(defun make-quoted-list-items (list &key (quote-syms&lists-p T))
  "In U-debug RETURNS list of quoted item. Quotes only symbols unless quote-syms&lists-p."
  (let
      ((newlist)       
       )
    (loop
     for item in list
     do
     (cond
      ((symbolp item)      
       (setf newlist (append newlist `(',item))))
      ((and quote-syms&lists-p (listp item))
       (setf newlist (append newlist `(',item))))
      (t (setf newlist (append newlist  (list item)))))
     ;;end loop
     )
         newlist
     ;;end let,make-quoted-list-items
     ))
;;TEST
;; (make-quoted-list-items '(SYM "STRING" 77 (LIST A)))
;; works=  ((QUOTE SYM) "STRING" 77 (QUOTE (LIST A)))



;;OUTPUT-WINDOW INFERFACE
;;
;;ddd
(capi:define-interface output-window ()
  ()
   (:panes
   (title-pane
    capi:rich-text-pane
    :max-height 20
    :accepts-focus-p t
 ;;   :automatic-resize t
    :enabled t)
;;using this may caused erasure of buffers in THE IDE 
  (outpane1
    capi:editor-pane
    :flag t  ;;new on lw7, stops creation of ide buffer
    :enabled t
    :buffer "my-output-buffer1" :buffer-name "My Output Buffer 1"  
  :documentation  "MUST specify buffer names to avoid crashing regurlar IDE editor buffers")
  (outpane2
    capi:editor-pane
    :flag t
    :enabled t
    :buffer "my-output-buffer2" :buffer-name "My Output Buffer 2"  
    :max-height 30
  :documentation  "MUST specify buffer names to avoid crashing regurlar IDE editor buffers")

;; used editor-pane instead of rich-text-pane to make it searchable with C-F
#|  (outpane1
    capi:rich-text-pane
    :accepts-focus-p t
 ;;   :automatic-resize t
    :enabled t)
   (outpane2
    capi:rich-text-pane
    :accepts-focus-p t
 ;;   :automatic-resize t
    :enabled t)|#

   (option-pane-1
    capi:option-pane
    :items '("Option-Pane-1" "Item" "Another Item")
    :selection 0)
   (radio-button-panel-1
    capi:radio-button-panel
    :items '("Radio-Button-Panel-1" "Button 2" "Button 3")
    :max-width t
    :max-height t))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(title-pane :divider outpane1 :divider outpane2 :divider option-pane-1 radio-button-panel-1)))
#| (:menu-bar menu-1 menu-2)
  (:menus
   (menu-2
    "Menu-2"
    :items  ("item1" "item2"))
   (menu-1
    "Menu-1"
     :items ( "itemA" "itemB")))|#
  (:default-initargs
   :min-height 400
   :min-width 500
   :layout 'column-layout-1
   :default-height 600
   :default-width 300
   ))


;;************************* hhh  HELP *************************


;;EXTENDED-TIME Macro
;;xxx
#|Summary PRINTS USEFUL TIMING INFORMATION, including information on
garbage collection (GC) activity.
Package hcl
Signature EXTENDED-TIME &BODY BODY
Arguments body The forms to be timed.

DESCRIPTION The macro extended-time runs the forms in body. It then prints a summary of the time taken followed by a breakdown of time spent in the GC.
The three columns of the GC breakdown show, respectively,
total time, user time, and system time. The rows of the GC
breakdown indicate the type of activity.
In 32-bit LispWorks these rows begin: 
main promote indicates promotions from generation 0.
internal promote, indicates when an attempt to promote from one generation to the next causes promotion of the higher generation, to make room for the objects from the lower generation.
FIXUP is a part of the compaction and promotion process.
|#
;;MY-EXAMPLE:

#|(defun find-paired-sums (nums1n  nums2n)
  (let
      ((totals)  
       (total-lists)
       (num1)
       (num2)
       )
    (loop
     for n1 from 1 to nums1n
     do
     (loop
      for n2 from 1 to nums2n
      do
      (let
          ((total 1)
           )
        (setf total (+ n1 n2)
              totals (append totals (list total))
              total-lists (append total-lists (list (list (format nil "~A + ~A= ~A" n1 n2 total)))))
        )))
    ;;end loops
    (values totals total-lists)
    ))|#
;;TEST
;;  (extended-time (find-paired-sums 100 100))
;; works= 
#|Timing the evaluation of (PROGN (FIND-PAIRED-SUMS 100 100))
>> TIMING OUTPUT:
User time    =        1.250
System time  =        0.000
Elapsed time =        1.209
Allocation   = 1200512320 bytes
0 Page faults
                                      total    /   user     /   system
total gc activity              =      0.421875 /   0.421875 /   0.000000
main promote (    6 calls)     =      0.031250 /   0.031250 /   0.000000
mark and sweep (  136 calls)   =      0.390625 /   0.390625 /   0.000000
internal promote (    1 calls) =      0.000000 /   0.000000 /   0.000000
promote (    1 calls)          =      0.000000 /   0.000000 /   0.000000
fixup (   12 calls)            =      0.015625 /   0.015625 /   0.000000
compact (    0 calls)          =      0.000000 /   0.000000 /   0.000000

followed by output:
(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20  etc
(("1 + 1= 2") ("1 + 2= 3") ("1 + 3= 4") ("1 + 4= 5") etc.


 (extended-time (list-dos-dir-contents "f:/"))
 (extended-time (list-dos-dir-contents "f:/"))
Timing the evaluation of (PROGN (LIST-DOS-DIR-CONTENTS "f:/"))

User time    =        0.218
System time  =        0.000
Elapsed time =        0.250
Allocation   = 102765680 bytes
0 Page faults
Calls to %EVAL    478505

                                      total    /   user     /   system
total gc activity              =      0.046875 /   0.046875 /   0.000000
main promote (    3 calls)     =      0.000000 /   0.000000 /   0.000000
mark and sweep (   98 calls)   =      0.046875 /   0.046875 /   0.000000
internal promote (    0 calls) =      0.000000 /   0.000000 /   0.000000
promote (    0 calls)          =      0.000000 /   0.000000 /   0.000000
fixup (    3 calls)            =      0.000000 /   0.000000 /   0.000000
compact (    0 calls)          =      0.000000 /   0.000000 /   0.000000
" f:\\"
"  \"f:/\""
"KINGSTON"
"0966-16A2"
"7"
"4"
"3488165"
"9243213824"
((:DIR "Lexus Radio Replace Info" "02/23/2013; 06:45 PM") (:DIR "LOST.DIR" "05/24/2013; 07:30 PM") (:DIR "1 Temp Videos" "05/21/2013; 12:38 PM") (:DIR "Key Current BUs" "05/21/2013; 12:38 PM") (:DIR "CogSys-Model" "05/16/2016; 01:43 PM") (:DIR "2016_06_15 Family Spen Graduation" "06/16/2016; 09:34 AM") (:DIR "2 To MEDIA CENTER" "05/22/2017; 07:59 PM"))
((:FILE "unInstaller.exe" "09/14/2011; 12:07 PM" "361248") (:FILE "urDrive.exe" "09/14/2011; 12:07 PM" "1934624") (:FILE "wtv2tssetup.exe" "07/17/2012; 04:50 PM" "1166935") (:FILE "test-functions2.lisp" "05/18/2016; 10:57 PM" "25358"))
"f:/"
NIL



;;===========

 (extended-time (list-dos-dir-contents "c:/3-TS/"))
Timing the evaluation of (PROGN (LIST-DOS-DIR-CONTENTS "c:/3-TS/"))

User time    =        1.359
System time  =        0.031
Elapsed time =        1.393
Allocation   = 704874432 bytes
0 Page faults
Calls to %EVAL    3226605

                                      total    /   user     /   system
total gc activity              =      0.140625 /   0.140625 /   0.000000
main promote (    3 calls)     =      0.000000 /   0.000000 /   0.000000
mark and sweep (  689 calls)   =      0.140625 /   0.140625 /   0.000000
internal promote (    0 calls) =      0.000000 /   0.000000 /   0.000000
promote (    0 calls)          =      0.000000 /   0.000000 /   0.000000
fixup (    3 calls)            =      0.000000 /   0.000000 /   0.000000
compact (    0 calls)          =      0.000000 /   0.000000 /   0.000000
" c:\\3-TS"
"  \"c:/3-TS/\""
"OS"
"4058-766B"
"54"
"38"
"111149319"
"631904452608"

(extended-time (list-directory-contents  "c:/3-TS/"))
Timing the evaluation of (PROGN (LIST-DIRECTORY-CONTENTS "c:/3-TS/"))

User time    =        0.000
System time  =        0.000
Elapsed time =        0.002
Allocation   = 168572 bytes
0 Page faults
                                      total    /   user     /   system
total gc activity              =      0.000000 /   0.000000 /   0.000000
main promote (    0 calls)     =      0.000000 /   0.000000 /   0.000000
mark and sweep (    0 calls)   =      0.000000 /   0.000000 /   0.000000
internal promote (    0 calls) =      0.000000 /   0.000000 /   0.000000
promote (    0 calls)          =      0.000000 /   0.000000 /   0.000000
fixup (    0 calls)            =      0.000000 /   0.000000 /   0.000000
compact (    0 calls)          =      0.000000 /   0.000000 /   0.000000
|#        

#|
(extended-time (make-tomex-data  nil :default-last-leveln 3 :send-to-file-p NIL :pprint-to-file-p NIL  :scan-drive-letters '(g)))
(extended-time (make-tomex-data  nil :default-last-leveln 3 :send-to-file-p NIL :pprint-to-file-p NIL  :scan-drive-letters '(g)))
Timing the evaluation of (PROGN (MAKE-TOMEX-DATA NIL :DEFAULT-LAST-LEVELN 3 :SEND-TO-FILE-P NIL :PPRINT-TO-FILE-P NIL :SCAN-DRIVE-LETTERS (QUOTE (G))))

User time    =       42.640
System time  =        0.437
Elapsed time =       43.385
Allocation   = 20852008672 bytes
0 Page faults
Calls to %EVAL    94235582

                                      total    /   user     /   system
total gc activity              =      5.343750 /   5.328125 /   0.015625
main promote (   77 calls)     =      0.046875 /   0.046875 /   0.000000
mark and sweep ( 9501 calls)   =      5.296875 /   5.281250 /   0.015625
internal promote (    0 calls) =      0.000000 /   0.000000 /   0.000000
promote (    1 calls)          =      0.000000 /   0.000000 /   0.000000
fixup (   78 calls)            =      0.031250 /   0.031250 /   0.000000
compact (    0 calls)          =      0.000000 /   0.000000 /   0.000000
(USB03-128)

|#

;;

 

#|
(make-instance 'menu
:title "Foo"
:items '("One" "Two" "Three" "Four")
:callback 'test-callback)|#