;;********************************* U-Input-output.lisp *****************************
;;
;; OPEN ARGS
#|direction---one of :INPUT, :OUTPUT, :IO, OR :PROBE. The default is :input.

ELEMENT-TYPE---a type specifier for recognizable subtype of character; or a type specifier for a finite recognizable subtype of integer; or one of the symbols signed-byte, unsigned-byte, or :default. The default is character.

IF-EXISTS---one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE, :OVERWRITE, :APPEND, :SUPERSEDE, OR NIL. The default is :new-version if the version component of filespec is :newest, or :error otherwise.

IF-DOES-NOT-EXIST---one of :error, :create, or nil. The default is :error if direction is :input or if-exists is :overwrite or :append; :create if direction is :output or :io, and if-exists is neither :overwrite nor :append; or nil when direction is :probe.

EXTERNAL-FORMAT---an external file format designator. The default is :default.
If direction is :input or :probe, or if if-exists is not :new-version and the version component of the filespec is :newest, then the file opened is that file already existing in the file system that has a version greater than that of any other file in the file system whose other pathname components are the same as those of filespec.
|#
;;
;;
;;READ-FILE-LINES
;;ddd
(defun read-file-lines (stream &key incl-newline)
  "In U-Input-output.lisp, reads an entire file and returns it as a single string"
  (let
      ((file-string "")
       )
    (loop
     with line
     do
     (setf  line (read-line stream nil :eof)) 
     (when (equal line :eof)
       (return))
     (cond
      (incl-newline
       (setf file-string (format nil "~A~%~A" file-string line)))
      (t 
       (setf file-string (format nil "~A~A" file-string line)))))
    file-string))
;;
;;test -- works
#|(defun testfl ()
  (setf out nil)
  (let
      ((file "C:\\TOM\\My Webs Exp\\~tstevens\\index.html")
       (file-string)
       )
    (with-open-file (stream file :direction :input) 
   (setf file-string (read-file-lines stream   :incl-newline t))
   (afout 'out (format nil "file-string= ~A~%" file-string))
   )))|#


;;READ-FILE-CHARS
;;
;;ddd
(defun read-file-chars  (stream &key incl-newline)
  "In U-Input-output.lisp, reads an entire file by chars? and returns it as a single string"
  (let
      ((file-string)
       )
    #|(format stream "GET / HTTP/1.0~C~C~C~C"
            (code-char 13) (code-char 10)
            (code-char 13) (code-char 10))|#
  ;;  (force-output stream)
  ;;  (write-string "Waiting to reply...")
    (loop for ch = (read-char-no-hang stream nil :eof)
          until ch
          do (write-char #\.)
          (sleep 0.25)
          finally (unless (eq ch :eof)
                    (unread-char ch stream)))
    (terpri)
    (loop for line = (read-line stream nil nil)
          while line
         ;;was do (write-line line)
         do
         (cond
      (incl-newline
       (setf file-string (format nil "~A~%~A" file-string line)))
      (t 
       (setf file-string (format nil "~A~A" file-string line))))
          )
    file-string
    ))
;;test
#|(defun testfc ()
  (setf out nil)
  (let
      ((file "C:\\TOM\\My Webs Exp\\~tstevens\\index.html")
       (file-string)
       )
    (with-open-file (stream file :direction :input) 
   (setf file-string (read-file-chars stream   :incl-newline t))
   (afout 'out (format nil "file-string= ~A~%" file-string))
   )))|#



;;PPRINT-LISTS
;;2020
;;ddd
(defun pprint-lists (nested-list  &key (separator " ") (leveln 1)
                                  (indent " ")(total-indent ""))
  "U-input-output.   RETURNS    INPUT:  
 NOTE: Puts SLASHES before double-quotes and ENCLOSES KEYWORDS in double-quotes.  TO FIX: 1. Copy output text to a new buffer; 2. SEARCH & REPLACE following: [ \"=> simple double-quote], keywords  eg.\":CSS\" to :CSS,etc."

   ;;(afout  'out (format nil "BEGIN (car nested-list= ~A" (car nested-list)))
   ;;NEW INDENT
   (setf total-indent (format nil "~A~A" total-indent indent))
    ;;LOOP THRU NESTED-LIST
  (let*
      ((pptext (cond ((= leveln 1) (format nil "~% (" ))
                     (t (format nil "~%~A  (" total-indent))))
       )
   ;;(afout   'out (format nil "indent= ~A leveln= ~A" indent leveln))
    (loop
     for item in nested-list
     do
     ;;(afout   'out (format nil "item= ~A" item))
    (cond
     ((and item (listp item))
      ;;(setf pptext (format nil "~A ~A" pptext " ("))
      (multiple-value-bind (pptext1)
          (pprint-lists item :leveln (+ leveln 1) :indent indent :total-indent total-indent)
        (setf pptext (format nil "~A~A ~A)~%  ~A" total-indent pptext pptext1
                             total-indent))
      ;;end mvb listp
      ))
     ;;all non-list items
     (T 
      (cond
         ((equal (elt pptext (- (length pptext) 1)) '#\( )
          (cond
           ((or (stringp item)(keywordp item))
            (when (keywordp item) (setf item (format nil ":~A" item)))
            (setf pptext (format nil "~A ~S" pptext item)))
           (T (setf pptext (format nil "~A ~A" pptext item))))
         )
         ;;(setf pptext (format-append-2 pptext item :separator "")))
         (T 
          (cond
             ((or (stringp item)(keywordp item))
              (when (keywordp item) (setf item (format nil ":~A" item)))
              (setf pptext (format nil "~A ~S" pptext item)))
             (T (setf pptext (format nil "~A ~A" pptext item))))))
      ;;end big T,cond
      ))
          ;;(setf pptext (format-append-2 pptext item :separator separator))))))
    ;; format-append-2 worked better SSSS RE-DEFINE IT
    ;;(afout   'out (format nil "pptext= ~A" pptext))
    ;;end loop
    )
    (when (= leveln 1)
      (setf pptext (format nil "~A) " pptext)))
      ;; (when (= leveln 1)(break "LEVEL 1 END"))
     pptext
    ;;end let, pprint-lists
    ))
;;TEST
;;with enclosed strings-NO KEYWORDS
;; (pprint-lists  '(a "b" (1 "2" 3 (x y "z") :key 4 5 6) "c" d) )
;; works= 
#|" 
 ( A \"b\"   
    ( 1 \"2\" 3 
     ( X Y \"z\")
     4 5 6)
    \"c\" D) "|#

 (pprint-lists  '(:a "b" (1 "2" 3 (x y "z") :key 4 5 6) "c" d) )
" 
 ( \":A\" \"b\"   
    ( 1 \"2\" 3 
     ( X Y \"z\")
     \":KEY\" 4 5 6)
    \"c\" D) "
;; With KEYWORDS -- MAKES THEM STRINGS
;; (pprint-lists  '(:a "b" (1 "2" 3 (x y "z") :key 4 5 6) "c" d) )
;;RESULT
#|" 
 ( \":A\" \"b\"   
    ( 1 \"2\" 3 
     ( X Y \"z\")
     \":KEY\" 4 5 6)
    \"c\" D) "|#
;; (pprint-lists *CS-CAT-DB-CSYMS)
;;no strings
;; (pprint-lists  '(a b (1 2 3 (x y z) 4 5 6) c d) )
;;
#| " 
  ( A B   
    ( 1 2 3 
     ( X Y Z)
     4 5 6)
    C D) "
|#
    


;;TEST-FORMAT/WRITE-SLASHES-TO-FILE
;;2020
;;ddd
(defun test-writing-slashes-to-file (testlist &key 
                                              (pathname *pprint-lists-to-file-pathname) )
  "U-input-output   RETURNS    INPUT:  "
  (let*
      ((x)
       )
    (with-open-file (outs pathname :direction :output :if-exists :append
                          :if-does-not-exist :create)
      (format outs "format-A= ~A" testlist)
      (format outs "~% format-S= ~S" testlist)
      (format outs "~% write=")
      (write testlist :stream outs)
      ;;end with
      )
    (values  "TEST DONE"  )
    ;;end let, test-writing-slashes-to-file
    ))
;;TEST
;; (test-writing-slashes-to-file  '(a b (1 "2" 3 ("x" y z) 4 5 "6") c "d") ) ;;  *pprint-lists-to-file-pathname)
;; RESULT 
#|
[INPUT= (a b (1 "2" 3 ("x" y z) 4 5 "6") c "d") ]
 format-A= (A B (1 2 3 (x Y Z) 4 5 6) C d)
 format-S= (A B (1 "2" 3 ("x" Y Z) 4 5 "6") C "d")
 write=(A B (1 "2" 3 ("x" Y Z) 4 5 "6") C "d")
|#




;;MY-PRINT-STRINGS-WO-SLASHES
;;2020
;;ddd
(defun my-print-strings-wo-slashes (nested-list  &key  (max-n 10000) )
  "U-input-output. Returns PROPER STRINGS W/O SLASH before DOUBLE-QUOTES (in list form = list-out)  INPUT: NESTED-LIST can be a STRING, LIST, or any object?   RETURNS: (values  list-out string-out).  STRING-OUT is a list WITH SLASHES." 
  (let*
      ((list-out)
       (string-out)
       )
    (cond
     ((stringp nested-list) 
      (setf string-out nested-list))
     (T (setf string-out (format nil "~S" nested-list))))
    (with-input-from-string (out-string string-out )
      (loop
       for n from 1 to max-n
       do
       (let*
           ((token (read out-string nil 'eof T))
         )
       (cond
        ((equal token 'eof)
         (return))
        (t (setf list-out (append list-out (list token) ))))     
       ;;end let,loop
       ))
    ;;end with
    )
    (when (listp (car list-out))
      (setf list-out (car list-out)))
    (values  list-out string-out)
    ;;end let, my-print-strings-wo-slashes
    ))
;;TEST
;; (my-print-strings-wo-slashes   "this \"is\" a (\"1\" 2) \"test\" string")
;; works= (THIS "is" A ("1" 2) "test" STRING)    "this \"is\" a (\"1\" 2) \"test\" string"
;; (my-print-strings-wo-slashes  '(a b (1 2 3 (x "y" z) 4 "5" 6) "c" d))
;; works= (A B (1 2 3 (X "y" Z) 4 "5" 6) "c" D)        "(A B (1 2 3 (X \"y\" Z) 4 \"5\" 6) \"c\" D)"

;;If have MULTI-LINE STRING
 #|(my-print-strings-wo-slashes   "this \"is\" a 
         (\"1\" 2) \"test\" 
           string")|#
;;works= (THIS "is" A ("1" 2) "test" STRING)
#|"this \"is\" a 
         (\"1\" 2) \"test\" 
           string"|#




;;WRITE-LIST-ITEMS
;;2020  
;;ddd
(defun write-list-items (list &key (stream *standard-output*)  indent-n
                              add-pre-newline-p  add-post-newline-p
                              prestring  poststring  (return-string-p T))
  "U-input-output  RETURNS    INPUT:  "
  (let*
      ((out-string " ")
       (len-list (list-length list))
       (indent "")
       )
    (when indent-n
         (dotimes (n indent-n) 
           (setf indent (string-append indent " "))))
    (loop
     for item in list
     do
     (afout 'out (format nil "item= ~A" item))
       (when add-pre-newline-p
         (format stream "~%"))
       (when indent-n
         (write indent :stream stream))
       (when prestring
         (write prestring :stream stream))
       ;;write item
         (write item :stream stream)
        (when poststring
          (write poststring :stream stream))     
        (when add-post-newline-p
         (format stream "~%"))

        (when return-string-p
          (when add-pre-newline-p
            (setf out-string (format nil "~A~%" out-string)))
          (when indent-n
              (setf out-string (format nil "~A~A" out-string indent)))
          (when prestring
            (setf out-string (format nil "~A ~S" out-string prestring)))
          ;;write item          
          (setf out-string (format nil "~A ~S" out-string item))
          (when poststring
            (setf out-string (format nil "~A ~S" out-string poststring)))
          (when add-post-newline-p
            (setf out-string (format stream "~A~%" out-string)))
          )
     ;;end ,loop
     )
    (values out-string len-list)
    ;;end let, write-list-items
    ))
;;TEST
;; (write-list-items '(a "test" 1 2 ("c" d) more))
;; write list= A"test"12("c" D)MORE    
;; string= " A \"test\" 1 2 (\"c\" D) MORE"     6

;; indent & newline
;; ;; (write-list-items '(a "test" 1 2 ("c" d) more) :indent-n 4 :add-pre-newline-p T)
;; result= 
#|
"    "A
"    ""test"
"    "1
"    "2
"    "("c" D)
"    "MORE
"
     A
     \"test\"
     1
     2
     (\"c\" D)
     MORE"
6|#







;;xxx ================== HELP ===========================
#|
;;FOR SLASHES IN STRINGS

;;works ONLY for list output not string output; but can 
;;FOR A LIST
(MY-PRINT-STRINGS-WO-SLASHES '(this "is" a ("test") ))
(THIS "is" A ("test"))    "(THIS \"is\" A (\"test\"))"
;;FOR A STRING: combine my-print-strings-wo-slashes with WRITE.
;;EG.
1. (MY-PRINT-STRINGS-WO-SLASHES "(this \"is\" a (\"test\") )")
;;list output = (THIS "is" A ("test")), which REMOVES SLASHES 
;; then can use WRITE  (NOT FORMAT) to write W/O SLASHES to file or *standard-output*
;;string-output (same as input)= "(this \"is\" a (\"test\") )"

;;2. use WRITE to write to a FILE, STREAM, etc
;; (WRITE '(THIS "is" A ("test"))) =  (THIS "is" A ("test"))

;;If start with NO slashes (must be a list), [see above]
;; (my-print-strings-wo-slashes (this \"is\" a (\"test\") )")
;;-------------------------------------------------------------------------------------

CL-USER 30 > (WRITE '(this \"is\" a (\"test\") ) :readably T)
(THIS \"IS\" A (\"TEST\"))
(THIS \"IS\" A (\"TEST\"))

CL-USER 31 > (WRITE '(this \"is\" a (\"test\") ) :readably T)
(THIS \"IS\" A (\"TEST\"))
(THIS \"IS\" A (\"TEST\"))

CL-USER 32 > (FORMAT nil "~A" '(this \"is\" a (\"test\") ))
"(THIS \"IS\" A (\"TEST\"))"

CL-USER 33 > (FORMAT nil "~S" '(this \"is\" a (\"test\") ))
"(THIS \\\"IS\\\" A (\\\"TEST\\\"))"

CL-USER 34 > (FORMAT nil "~A" '(this "is" a ("test") ))
"(THIS is A (test))"

CL-USER 35 > (FORMAT nil "~S" '(this "is" a ("test") ))
"(THIS \"is\" A (\"test\"))"
|#




;;MOVED TO U-FILES --very versitle and robust
;;WRITE-TO-FILE
;;2017
;;ddd
#|(defun write-to-file (filename objects &key (if-exists :append) 
                               (if-does-not-exist  :create)
                               (element-type :default) (direction :output) 
                               (pretty *print-pretty*) 
                               incl-nils-p  (newline-after-objects-p T)(final-newline-p T)
                               (array *print-array*) (base *print-base*)  
                               (case *print-case*) (circle *print-circle*) 
                               (escape *print-escape*) (gensym *print-gensym*) 
                               (length *print-length*) (level *print-level*)  
                               (lines *print-lines*) (miser-width *print-miser-width*) 
                               (pprint-dispatch *print-pprint-dispatch*) 
                               (radix *print-radix*)(readably *print-readably*)  
                               (right-margin *print-right-margin*) )
  "In U-Input-output  USE FOR PPRINT TO A FILE,etc. plus lots more varisions using print.  U-Input-output  RETURNS    INPUT:  (if incl-nils-p= NIL, omits NILs) Flexible, works well."
  (let
      ((stream)
       (n-objects 0)
       )
    (with-open-file (stream filename :if-exists  if-exists  :if-does-not-exist if-does-not-exist
                            :element-type  element-type  :direction direction)

      (loop
       for object in objects
       do
       (when (or object incl-nils-p)
         (write  object  :stream stream :array array :base base
                 :case case :circle circle 
                 :escape escape :gensym gensym 
                 :length length :level level  
                 :lines lines :miser-width miser-width 
                 :pprint-dispatch pprint-dispatch :pretty pretty 
                 :radix radix :readably readably  
                 :right-margin right-margin)

         (when newline-after-objects-p
           (format stream "~%")) 

         (incf n-objects)
       
         ;;(break "here")
         ;;end while, loop
         ))
      (when final-newline-p
        (format stream "~%"))
      ;;end with-open
      )
    (format nil ">> N-OBJECTS= ~A WRITTEN TO FILE=> ~A (pprint= ~A)" 
            n-objects filename pretty)
    ;;end let, write-to-file
    ))|#
;;TEST
;;(setf testwobj '(list '(first (a b c (1 2 3 (4 5 6 (7 8 9)(10 11 12) h i j) k l) next-last) last)))
;; (WRITE-TO-FILE   "c:/temp/my-write-test2.lisp" `( (setf testwobj ,(eval testwobj))))
;; (WRITE-TO-FILE   "c:/temp/my-write-test2.lisp" `(this (setf testwobj ,(eval testwobj))) :pretty T)



;;NOTE FOR WRITE PARAMETERS
#|write is the general entry point to the Lisp printer. For each explicitly supplied keyword parameter named in the next figure, the corresponding printer control variable is dynamically bound to its value while printing goes on; for each keyword parameter in the next figure that is not explicitly supplied, the value of the corresponding printer control variable is the same as it was at the time write was invoked. Once the appropriate bindings are established, the object is output by the Lisp printer.

PARAMETER        CORRESPONDING DYNAMIC VARIABLE  
array            *print-array*                   
base             *print-base*                    
case             *print-case*                    
circle           *print-circle*                  
escape           *print-escape*                  
gensym           *print-gensym*                  
length           *print-length*                  
level            *print-level*                   
lines            *print-lines*                   
miser-width      *print-miser-width*             
pprint-dispatch  *print-pprint-dispatch*         
pretty           *print-pretty*                  
radix            *print-radix*                   
readably         *print-readably*                
right-margin     *print-right-margin*  |#



    
                                       






;;HHH USE OF INPUT AND OUTPUT TO STRINGS

#|(with-output-to-string (out)
  (format out "hello, world ")
  (format out "~s" (list 1 2 3)))
|#
;;HHH USE OF ECHO-STREAMS, make-string-output-stream, ETC.
;; 
;;
  ;; (setf *test-out-string (make-string-output-stream))
;;  = #<SYSTEM::STRING-OUTPUT-STREAM 200914F7>
;; (setf *test-echo (make-echo-stream *standard-output* *test-out-string))
;; = #<Echo Stream Input = #<EDITOR::RUBBER-STREAM #<EDITOR:BUFFER CAPI interactive-pane 78> 2B7275F3>, Output = #<SYSTEM::STRING-OUTPUT-STREAM 200914F7>>
;; (format t "TEST OUT")
;; (get-output-stream-string *test-out-string)
;; (echo-stream-output-stream *test-echo)  = #<SYSTEM::STRING-OUTPUT-STREAM 200914F7>

  ;; (setf *test-in-string "        "   *test-in-string2 (make-string-input-stream *test-in-string))
;;#<SYSTEM::STRING-INPUT-STREAM 2F86FDBB>

;; (format t "TEST OUT")
;;

;; (format *test-out-string "FORMAT TEST OUT")
;; (get-output-stream-string *test-out-string) = "FORMAT TEST OUT"
;; NOTE: WITH EACH READ, get-output-stream-string RESETS STRING TO ""

;; (format *standard-output* "FORMAT STANDAND-OUTPUT")
;; = FORMAT STANDAND-OUTPUT   NIL
;; (get-output-stream-string *test-out-string) = "" ;;DOESN'T WORK on *standard-output*

;; (format *TEST-ECHO "FORMAT STANDAND-OUTPUT3")
;;      = NIL
;; ;;works when use ECHO stream= (get-output-stream-string *test-out-string) = "FORMAT STANDAND-OUTPUT3"


;; (get-get-output-stream-string *test-out-string) = "" 




;;MY-PPRINT-IN-LISTS-W/SLASHES
;;2020
;;ddd
(defun my-pprint-in-lists-w/slashes (pp-string  &key  (end-strs '( "\)")  )
                                      (startn 0) endn (max-n 2000))
  "U-input-output   RETURNS pp-out-str.  CODE BETTER THAN W/O-SLASHES. . INPUT: any string (or object to make into a string).  END-STRS must be SINGLE ELT [or must modify function]"
  (when (not (stringp pp-string))
    (setf pp-string (format nil "~S" pp-string)))
  (let*
      ((instr (make-string-input-stream pp-string startn endn))
       (my-pp-string "")    
       (newline "")
       (newline-end-p)
       )
    (loop
     for line-n from 1 to max-n
     do
     (let*
         ((line (read-line instr nil 'eof))
          (len-line (cond ((stringp line)(length line))
                          (T line)))
          )   
       (unless (equal line 'eof)
         (loop
          for n from 0 to (- len-line 1)
          do
          (let*
              ((str (subseq line n (+ n 1)))
               ;;(list-end-p (listp obj))
               (line-end-p (member str end-strs :test 'my-equal))          
               )
            (cond
             ((equal line 'eof)      
              nil)
             ((member str end-strs :test 'my-equal)
              (setf newline (format nil "~A~A" newline str)
                    my-pp-string (format nil "~A~%~A" my-pp-string newline)
                    newline ""))
             (t (setf newline (format nil "~A~A" newline str) )))
            ;;end let,loop, unless
            )))
       ;;end let,loop
       ))
    (values my-pp-string)
    ;;end let, my-pprint-in-lists-w/slashes
    ))
;;TEST
;; (my-pprint-in-lists-w/slashes **new-csym-tree)
;; works, same code as **new-csym-tree except has slashes 
;; PP form is largely BY LISTS, so much more compact than normal pprint.



;;MY-PPRINT-LISTS  -- PRINTS LOTS OF EXTRA NILS?? CHECK LATER?
;;2020
;;ddd
#|(defun my-pprint-lists (nested-list  &key (end-items '(:S)))
  "U-input-output   RETURNS pp-out-str. INPUT: any string (or object to make into a string).  END-STRS must be SINGLE ELT [or must modify function]"
  (when (not (listp nested-list))
    (setf nested-list (list nested-list)))
  (let*
      (;;(instr (make-string-input-stream pp-string startn endn))
       (my-pp-string "")    
       (newlines-list)
       (newline-str "")
       (newline)
       (newline-end-p)
       )
    (loop
     for item in nested-list
     do
     (let*
         ((x)
          )   
       (cond
        ((and item (listp item))
         (multiple-value-bind (sub-my-pp-string sub-newlines-list)
         (my-pprint-lists item)
           ;;ACCUMULATE
           (setf newlines-list (append1 newlines-list sub-newlines-list)
                 my-pp-string (format nil "~A ~A" my-pp-string sub-my-pp-string))
           ))
       ((my-equal item (first nested-list))
        (setf newline (append1 newline item)
              newline-str (format nil "~A (~S" newline-str item)
              newlines-list (append1 newlines-list newline))
        (when (= (length nested-list) 1)
          (setf newline-str (format nil "~A)" newline-str)))
        (setf my-pp-string (format nil "~A~%  (~A" my-pp-string newline-str)))
       ((my-equal item (last1 nested-list))
        (setf newline (append1 newline item)
              newline-str (format nil "~A ~S)" newline-str item)
              newlines-list (append1 newlines-list newline)
              my-pp-string (format nil "~A~%   ~A" my-pp-string newline-str)))
        ((member item end-items :test 'my-equal)
         (setf newline (append1 newline item)
                 newline-str (format nil "~A ~S" newline-str item)
                 newlines-list (append1 newlines-list newline)
                 my-pp-string (format nil "~A~%  ~A" my-pp-string newline-str)))
        (item (setf newline (append1 newline item)
                 newline-str (format nil "~A ~S" newline-str item))))
            ;;end let,loop
            ))
    (values my-pp-string newlines-list)
    ;;end let, my-pprint-lists
    ))|#
;;TEST
;; (my-pprint-lists **new-csym-tree)












#|(defun clean-cs-pp-output (pp-string  &key  (end-objs '(":S")  )
                                      (startn 0) endn (max-n 2000))
  "U-input-output   RETURNS pp-out-str  LINE-ENDS= collect until find end. "
  (when (not (stringp pp-string))
    (setf pp-string (format nil "~S" pp-string)))
  (let*
      ((instr (make-string-input-stream pp-string startn endn))
       (new-list)    
       (newline-list)
       )
    (loop
     for n from 1 to max-n
     do
     (let*
         ((obj (read instr nil 'eof ))
          (list-end-p (listp obj))
          (end-p (member obj end-objs :test 'my-equal))          
          )
    (cond
     ((equal obj 'eof)
      (return))
     (list-end-p
      (setf new-list (append new-list (list obj))))
     (end-p
      (setf newline-list (append1 newline-list obj)
       new-list (append1 new-list newline-list)
       newline-list nil))
     (t (setf newline-list (append1 newline-list obj))))     
     ;;end let,loop
     ))
    

    (values new-list )
    ;;end let, clean-cs-pp-output
    ))|#
