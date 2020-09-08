;;**************************** U-tstring.lisp *********************************
;;FUNCTIONS FOR STRINGS and SEQUENCES
;; List functions are in U-lists.lisp
;;
;;INCLUDES functions formerly in U-sequences.lisp -- that file is depreciated-deleted.
;;some functions originally in TSTRING UTILITIES FOR GWLISP.  See file \gt-util\t-string.lsp for more trans-lisp related functions

(in-package "CL-USER")

;;CL FUNCTIONS W/STRING
#|
base-string
directory-namestring
enough-namestring
string-append &rest (appends strs not in list (&rest)
string-append* (appends strs in a lists)
make-string
make-string-input-stream
make-string-output-stream
string-capitalize produces a copy of string such that, for every word in the copy, the first character of the ``word,'' if it has case, is uppercase 
string-downcase
string-upcase

PARSE-INTEGER string &key start end radix junk-allowed => integer, pos
[converts a STRING (or part) to an INTEGER] parse-integer expects an optional SIGN (+ or -) followed by a a non-empty sequence of digits to be interpreted in the specified radix. Optional leading and trailing WHITESPACE[1] is ignored.


read-from-string

simple-base-string
simple-string
simple-string-p
STRING
 (string "already a string") =>  "already a string"
 (string 'elm) =>  "ELM"
 (string #\c) =>  "c"
string-capitalize
string-downcase
string-equal
string-greaterp
string-left-trim
string-lessp
string-not-equal
string-not-greaterp
string-not-lessp
string-right-trim
string-stream
string-trim
string-upcase
string/=
string<
string<=
string=
string>
string>=
stringp

WITH-INPUT-FROM-STRING
WITH-OUTPUT-TO-STRING
WRITE-TO-STRING
prin1-to-string
princ
princ-to-string

|#;;
;;GLOBAL VARS USED IN U-TSTRING
(defparameter  *new-symbol-type-list nil)
(defparameter  *new-symbols-type-list-of-lists  nil)
(defparameter  *new-symbol-type-spec-list-of-lists nil)
(defparameter  *new-root-list nil)
(defparameter  *new-symbol-type-symbol-string-list-of-lists nil )
(defparameter  *new-seq-nested-lists nil)
(defparameter   *new-symbol-nested-lists nil)
(defparameter   *new-dim-list-nested-lists nil)
;;added
(defparameter *all-new-sym-names-flat-list  NIL)
(defparameter *all-new-symbols-flat-list NIL)
(defparameter *all-new-symdim-lists-flat-list NIL)
(defparameter  *append-node-sep-lists nil)




;;MY-EQUAL-PATHS
;;2020
;;dde
(defun my-equal-paths (path1 path2)
  "U-tstring, uses my-equal :clean-path-str-p T"
  (let*
      ((result (my-equal path1 path2 :clean-path-str-p T))
       )
    result
    ))
;;TEST
;; (my-equal-paths "F:\\2 MAIN BU\\" "F:/2 MAIN BU/") = T


;;MY-EQUAL
;;2020 modified to match substrings if only spaces diff  eg. "1MK" now = "   1MK   ")
;;ddd
(defun my-equal (item1 item2 &key (remove-chars '(#\space))
                       clean-path-str-p)
  "In U-tstring.lisp, items may be any combo of string, number, or symbol types.  Tests to see if are case-insensitive equal. Tests lists with multiple nesting levels. NOT TEST LISTS equal."
  (let
      ((item1-str)
       (item2-str)
       (result)
       (element2)
       )     
    (when (and clean-path-str-p (stringp item1)(stringp item2))
      (setf result
      (string-equal (clean-path-str item1)(clean-path-str item2))))
    (cond 
     ;;IF EITHER IS A PATHNAME, TEST PATHS
     ((or (pathnamep item1)(pathnamep item2))
      (cond
       ((equal item1 item2)
        (setf result T))))
     ;;BOTH NIL
     ((and (null item1)(null item2))
      (setf result T))
     ;;IF BOTH ARE LISTS
     ((and (listp item1)(listp item2)(list-length item1)(list-length item2))
      (loop
       for element1 in item1
       for n from 0 to (- (list-length item1) 1)
       do
       (setf element2 (nth n item2))
       ;;(afout 'out (format nil "n= ~A item1= ~A item2= ~A element1= ~A element2= ~A~% " n item1 item2 element1 element2))

       (cond
        ((my-equal element1 element2) 
         (setf result T))
        (t (setf result nil)
           (return)))       
       ;;end loop
       )
      (when result (setf result item1))
      ;;end listp item1 and item2
      )
      ;;IF ONE A LIST AND ONE NOT, CANNOT BE EQUAL
      ((or (listp item1)(listp item2))  NIL)       
      ;;for NUMBERS
      ((or (numberp item1)(numberp item2))
       (cond
        ((or (numberp item1)(characterp item1))
         (setf item1-str (format nil "~A" item1)))
        (t (setf item1-str (string item1))))
       (cond
        ((or (numberp item2)(characterp item2))
         (setf item2-str (format nil "~A" item2)))
        (t (setf item2-str (string item2))))
       ;; (afout 'out (format nil "~S = ~S" item1-str  item2-str ))
       (setf result (string-equal item1-str item2-str))
       )
      ;;FOR STRINGS and if ONE is a SYMBOL
      ((or (stringp item1)(stringp item2))
       (when (and (stringp item1) remove-chars)
         (setf item1 (delete-chars item1 remove-chars)))
       (when (and (stringp item2) remove-chars)
         (setf item2 (delete-chars item2 remove-chars)))
       (cond
        ((string-equal item1 item2)
         (setf result T))
        ((string-equal (format nil "~A" item1)(format nil "~A" item2))
         (setf result T)))
       )    
      ;;TWO SYMBOLS
      ((equal item1 item2)
       (setf result T))
      (T NIL))
    result
    ))
;;TEST
;;2020
;; (my-equal "F:\\2 MAIN BU\\" "F:/2 MAIN BU/" :clean-path-str-p T)
;; works (not without :clean-path-str-p =T)
;; (my-equal 'this "this") = T
;; (my-equal 'this "  this    ") = T
;;older
;; (my-equal 'this "this") = T
;; (my-equal 3 "3") = T
;; (my-equal 3 "4") = NIL
;; (my-equal #\c "c") = T
;; (my-equal #\c "C") = T
;; (my-equal #\c "d") = NIL
;; (characterp #\c)   = T
;; (characterp  "c")  = NIL   
;; (my-equal 3 "this")   = NIL
;; (my-equal '(this list) "this")
;; (my-equal '(1 3 2) '(1 3 2)) =  (1 3 2)
;; (my-equal '((1 3 2)) '((1 3 2))) = ((1 3 2))
;; (my-equal '(((1 3 2))) '(((1 3 2)))) = (((1 3 2)))
;; (my-equal '((a b c)((x y z))) '((a b c)((x y z)))) = ((A B C) ((X Y Z)))
;; 

;; (substitute  "\\" "/" "\\this\\that\\")
;; (my-

;;ddd ORIGINAL MY-EQUAL DIDN'T TEST LISTS



;;MY-EQUAL-PATH
;;2017
;;ddd
(defun my-equal-path (item1 item2)
  "In U-tstring.lisp, LIKE MY-EQUAL, except it also TESTS PATHS-even if slashes don't match. Uses my-equal: items may be any combo of string, number, or symbol types.  Tests to see if are case-insensitive equal. Tests lists with multiple nesting levels. newitem1 and newitem2 are the revised versions of those items for testing. RETURNS (values result newitem1 and newitem2). When subt-slashes-p, substitutes forward for back slashes in all items."
  (let
      ((result)
       (newitem1 item1) 
       (newitem2 item2)
       )
    (when (pathnamep item1)
      (setf newitem1 (namestring item1)))
    (when (pathnamep item2)
      (setf newitem2 (namestring item2)))

    (when (stringp newitem1)
      (setf newitem1 (my-substitute-char #\/ #\\ newitem1)))
    (when (stringp newitem2)
      (setf newitem2 (my-substitute-char #\/ #\\ newitem2)))

    (setf result (my-equal newitem1 newitem2))
    (values  result   newitem1 newitem2)
    ;;end let, my-equal-path
    ))
;;PRE-TEST
;; (setf testpth (make-pathname :host "c:\\" :directory "this")) = #P"\\\\c:\\\\this\\"
;; (equal "\\\\c:\\\\this\\"  testpth) = NIL
;; (equal "\\\\c:\\\\this\\"  (namestring testpth)) = T
;; (string-equal "\\\\c:\\\\this\\"  (namestring testpth)) = T
;;TEST
;; (my-equal-path testpth "\\\\c:\\\\this\\")
;; works= T  "//c://this/"  "//c://this/"
;; (my-equal-path "F:/LISP PROJECTS TS\\" "F:/LISP PROJECTS TS/")





;;MY-SUBSTITUTE-CHAR
;;2017
;;ddd 
(defun my-substitute-char (new old string &key (begin 0) end)                                      
  "In U-tstring. Substitutes one char for each occurance  from begin to end. RETURNS (values new-string n-str)."
  (let
      ((n-str (length string))
       (newstr "")
       (char)
       (n-end)
       )
    (cond
     ((null end)
      (setf n-end (- n-str 1)))
     (t (setf n-end end)))
    (loop
     for n from begin to n-end
     do
     (setf char (char string n ))
     (cond
      ((char-equal old char)
       (setf newstr (format nil "~A~C" newstr new)))          
      (t (setf newstr (format nil "~A~C" newstr char))))
     ;;end loop
        )
    (values newstr n-str)
    ;;end let, my-substitute-char
    ))
;;TEST
;; (my-substitute-char #\/   #\\ "this\\that\\")
;; works=  "this/that/"  10
;; (my-substitute-char #\/   #\\  "\\\\c:\\\\this\\") = "//c://this/"   11


;;CHARS
;;2017
;;ddd        
(defun chars (string) 
  "In U-tstring, RESULTS (values chars-list chars-str n-chars) Prints a list and string of characters in string"
   (let
       ((char)
        (chars-list)
        (chars-str "")
        (n-chars (length string))
        )
  (loop
   for n from 0 to (- n-chars  1)
   do
   (setf char (char string n )
         chars-list (append chars-list (list char))
      chars-str (format nil  "~A~C" chars-str char))
   )
  (values chars-list chars-str n-chars)
  ))
;; (chars "this\\that\\")
;; works=  (#\t #\h #\i #\s #\\ #\t #\h #\a #\t #\\)   "this\\that\\"   10
;; (chars "this\that\") = WON'T EVAL
;; (chars "this\that") = WON'T EVAL
;; (chars "this\that") 
;;NOTE: BACKSLASH \ ALONE IS IGNORED EXCEPT PREVENTS EVAL OF QUOTES.
;;works= (#\t #\h #\i #\s #\t #\h #\a #\t)   "thisthat"   8


(defparameter *sym-not-found "ERROR in my-make-symbol")



;;MY-MAKE-SYMBOLS
;;2020
;;ddd
(defun my-make-symbols (strings &key csformatp
                                (delete-chars '(#\: #\\ #\/ #\; #\" #\comma #\.  #\tab
                                                #\newline  #\comma #\; #\: #\\ #\? #\[ #\] #\# 
                                                #\< #\>  #\|  #\( #\) #\} ))
                                (string-not-equal '("" ".")) (alt-new-symbol '*sym-not-found)
                                (replace-space-char #\-) pre post)
  "In MyUtilities\\U-tstring.lisp, If string is string, converts to a symbol, if string is really a symbol, just returns it. ALSO  converts a string of a list to a real list of symbols. Will NOT make sym beginning with zero.
   If ALT-NEW-SYMBOLS, sets to these if conditions not met.
   PRE and POST add strings to begin or end of symbols.
   CSFORMATP allows < and > in symbol"
  (let*
      ((symbols)
       )
    (when (or pre post)
      (setf strings (modify-strings strings :pre pre :post post) ))
    (loop
     for str in strings
     do
     (let*
         ((newsym (my-make-symbol str :delete-chars delete-chars
                                  :string-not-equal string-not-equal
                                  :alt-new-symbol alt-new-symbol :csformatp csformatp))
          )
       (setf symbols (append symbols (list newsym)))
       ))
    symbols
    ;;end let,my-make-symbols
    ))
;;TEST
;; (my-make-symbols '("sym1" "sym2") :pre "<" :csformatp T )
;; works= (<SYM1 <SYM2)
     
     

;;MY-MAKE-SYMBOL WORKS EFFICIENTLY
;; Revised 2018-08 to put "-" for spaces by default.
;;ddd
(defun my-make-symbol (string &key (delete-chars '(#\: #\\ #\/ #\; #\" #\comma #\.  #\tab #\newline  #\comma #\; #\: #\\ #\? #\[ #\] #\#  #\< #\>  #\|  #\( #\) #\} ))
                              (string-not-equal '("" ".")) (alt-new-symbol '*sym-not-found)
                              (replace-space-char #\-) csformatp if-num-return-num-p)
   "In MyUtilities\\U-tstring.lisp, If string is string, converts to a symbol, if string is really a symbol, just returns it. ALSO  converts a string of a list to a real list of symbols. Will NOT make sym beginning with zero. IF-NUM-RETURN-NUM-P
   If ALT-NEW-SYMBOLS, sets to these if conditions not met."
   (let
      ((new-symbol)
       (n-chars) 
       (char)
       (new-string "")
       )
     (when csformatp
       (setf delete-chars (delete-list-items  '( #\< #\>) delete-chars)))
     (cond
      ;;in case it's already a symbol
      ((symbolp string)
       (setf new-symbol string))
      ((and (stringp string)
            (not (member string string-not-equal :test 'string-equal)))
       (setf n-chars (length string))
       ;;process chars
       (loop
        for n from 0 to (- n-chars 1)
        do
        (setf char (char string n))
        (when (graphic-char-p char) ;;(graphic-char-p #\.)
          (cond
           ((and replace-space-char (char-equal char #\space))
            (setf new-string (format nil "~A~A" new-string replace-space-char)))
           ((member char  delete-chars   :test 'char-equal)
            NIL)
           (t (setf new-string (format nil "~A~A" new-string char))))
          ;;end when,loop
          ))
       ;;make symbol
        (setf  new-symbol (read (make-string-input-stream new-string) NIL 'EOF))
  ;;(setf  new-symxx (read (make-string-input-stream "0123456") NIL 'EOF)) = 123456
       ;;2017 If its a number MUST include any periods = #\. 
       ;; SO RE-CHECK FOR PERIODS
       (when (numberp new-symbol)
         (setf new-string "")
         (loop
          for n from 0 to (- n-chars 1)
          do
          (setf char (char string n))
          (when (or (equal char #\.) (digit-char-p char))
            (setf new-string (format nil "~A~A" new-string char)))
            ;;end loop
            )
         (setf  new-symbol (read (make-string-input-stream new-string) NIL 'EOF))
         ;;end when numberp
         )
      ;;end (stringp string)
      )
      (t (setf new-symbol alt-new-symbol)))
     ;;IF-NUM-RETURN-NUM-P 
     (when (and (numberp string) if-num-return-num-p)
       (setf new-symbol string))
    new-symbol
    ;;end  let, my-make-symbol
     ))
;;TEST
;; (my-make-symbol 77 :IF-NUM-RETURN-NUM-P T) = 77
;;  (my-make-symbol "test space 2") = TEST-SPACE-2
;;  (my-make-symbol "0123456") = 123456  (not  0123456 as should be)
;;  (my-make-symbol "12:34;5.6") = 12345.6
;; also (+ 3.3 (my-make-symbol "12:34;5.6")) => 12348.899
;; (my-make-symbol "123456") also = 123456
;;works
;; (my-make-symbol 'CS2-1-1-99)

;; (my-make-symbol "this:that") = THISTHAT
;; (my-make-symbol "this.that") = THISTHAT
;;(my-make-symbol "this.that":delete-chars nil) = THIS.THAT
;; (my-make-symbol "this/that") = THISTHAT
;; (my-make-symbol "this/that"  :delete-chars nil)  = THIS/THAT
;; (my-make-symbol "this;that") = THISTHAT
;;(my-make-symbol "this;that" :delete-chars nil) = THIS
;; (my-make-symbol "\"DIR") = DIR
;; (my-make-symbol "0.6132") =0.6132
;; (my-make-symbol "061.32") = 61.32
#|
(defun testmms ()
  "successfully sets this1 to 77, returns 77, x is a memory location?"
  (let
      ((x)
       )
   (setf x  (my-make-symbol "this1"))
   (set x 77)))
|#

;;MY-MAKE<SYMBOL
;;2018 
;;ddd
(defun my-make<symbol (str)
  "U-tstring.  Allows symbols with < and >. Otherwise like my-make-symbol"
  (let ((newsym (my-make-symbol str  :delete-chars '(#\: #\\ #\/ #\; #\" #\, #\. #\Tab #\Newline #\, #\; #\\ #\? #\[ #\] #\#  #\| #\( #\})))
        )
    newsym
    ))


;;MAKE-STRING&SYMBOL
;;2020
;;ddd
(defun make-string&symbol (item &key prefix postfix
                                (delete-chars '(#\: #\\ #\/ #\; #\" #\, #\. #\Tab #\Newline
                                                  #\, #\; #\\ #\? #\[ #\] #\#  #\| #\( #\})))
  "U-tstring  RETURNS: (values newstr newsym ). ITEM can be string or symbol.
  If pre or post already begin or end, will NOT add."
  (let*
      ((newstr (cond
                ((stringp item) item)
                (t (format nil "~A" item))))
       (newsym)
       (len-prefix (length prefix))
       (len-postfix (length postfix))
       (len-item (length newstr))
       )
    (when prefix
      (unless (equal (subseq newstr 0 len-prefix) prefix)
      (setf newstr (format nil "~A~A" prefix newstr))))
    (when postfix
      (unless (equal (subseq newstr (- len-item len-postfix)) postfix)
        (setf newstr (format nil "~A~A" newstr postfix))))
    ;;make symbol
      (setf newsym (my-make-cs-symbol newstr :delete-chars delete-chars))
    (values newstr newsym )
    ;;end let, make-string&symbol
    ))
;;TEST
;; (make-string&symbol 'testsymxx :prefix "<")
;; works= "<TESTSYMXX"   <TESTSYMXX
;; (make-string&symbol "testsymxx2" :prefix "<")
;; works= "<testsymxx2"   <TESTSYMXX2
;; (make-string&symbol 'testsymxx :prefix "<" :postfix ">")
;; works= "<TESTSYMXX>" <TESTSYMXX>
;; IF ALREADY PRESENT
;; ;; (make-string&symbol '<testsymxx> :prefix "<" :postfix ">")
;; WORKS [not add new pre & post] = "<TESTSYMXX>"  <TESTSYMXX>




;;GET-SET-SYMBOL-VALUE
;;2018-08
;;ddd
(defun get-set-symbol-value (symbol  &key  new-value (sym-prefix "")(sym-end "")
                                     replace-old-value-p)
  "U-list  RETURNS  (values symbol (eval symbol) symbol-str old-value)  INPUT: symbol can be string or unbound.  Sets symbol to new-value unless it is already bound AND replace-old-value-p = NIL. Will NOT append new-value only replace old-value. Use get-set-append-delete for that."
  (let*
      ((old-value)
       (testsym)
       (return-sym-str)
       (symbol-str (format nil "~A" symbol))
       )
   (cond
    ((and (symbolp symbol) (boundp symbol))
     (setf old-value (eval symbol))
     (when replace-old-value-p
       (set symbol new-value)))
    ;;if symbol is a string and its symbol is bound
    ((and (stringp symbol) (boundp (setf testsym (my-make-symbol symbol))))
     (setf return-sym-str symbol
           symbol testsym
           old-value (eval symbol))
     (when replace-old-value-p
       (set symbol new-value)))
    ;;If not bound, create a new symbol
    (t
     (let*
         ((symbol-str (format nil "~A" symbol))
          (len-sym (length symbol-str))      
          (len-pre (length sym-prefix))
          (len-end (length sym-end))
          )
       (unless (or (null sym-prefix)
                   (string-equal  (subseq symbol-str 0 (- len-sym 1) ) sym-prefix))
         (setf symbol-str (format nil "~A~A" sym-prefix symbol-str)
               len-sym (length symbol-str)))
       (unless (or (null sym-prefix)
                   (string-equal  (subseq symbol-str (- len-sym len-end) ) sym-end))
         (setf symbol-str (format nil "~A~A"  symbol-str sym-end) ))

       (setf symbol (my-make-symbol symbol-str))
       (when (boundp symbol)
            (setf old-value (eval symbol)))
       (set symbol new-value)
     ;;end let, t, cond
     )))
    (values symbol (eval symbol) symbol-str old-value)
    ;;end let, get-set-symbol-value
    ))
;;TEST
;; (get-set-symbol-value "testgetsetsymX"  :new-value (list :BIPATH (list "old-value" "new-value")) :sym-prefix "*pre-" :sym-end "-end")
;; works= *PRE-TESTGETSETSYMX-END (:BIPATH ("old-value" "new-value"))   "testgetsetsym"   NIL
;;also CL-USER 101 > *PRE-TESTGETSETSYMX-END
;;works= (:BIPATH ("old-value" "new-value"))
;; sym for "testgetsetsym" is UNBOUND, but *PRE-TESTGETSETSYMX-END bound
;; (get-set-symbol-value "testgetsetsym" :new-value "new-value2" :sym-prefix "*pre-" :sym-end "-end" :replace-old-value-p T)
;; works= *PRE-TESTGETSETSYM-ENDX  "new-value2" "testgetsetsym"  (:BIPATH ("old-value" "new-value"))



#| OLD-VERSION, ERRORS WHEN TRIED TO PROCESS WEIRD CHARS IN DOS ETC
(defun my-make-symbol (string &key (delete-chars '(#\: #\\ #\/ #\; #\" #\comma #\.  #\tab #\newline  #\comma #\;  #\\ #\? #\[ #\] #\#  #\< #\>  #\|  #\( #\} ))
                              (string-not-equal '("" ".")))
   "In MyUtilities\\U-tstring.lisp, If string is string, converts to a symbol, if string is really a symbol, just returns it. ALSO
 converts a string of a list to a real list of symbols. Will NOT make sym beginning with zero."
   (let
      ((new-symbol)
       )
         (cond
          ((and (stringp string)(not (member string string-not-equal :test 'string-equal)))
           (setf string (delete-chars  string delete-chars)
            new-symbol (read (make-string-input-stream string) NIL 'EOF)))
          (t (setf new-symbol string)))
    new-symbol
     ))|#


;;DELETE-SYMBOL-STR
;;2020
;;ddd
(defun delete-symbol-str (delete-str sym  &key   (begin-n 0) end-n)
  "U-tstring   RETURNS (values new-sym new-str )
    INPUT: Can be symbol or string. N DELETE-STR can be > 1."
  (let*
      ((str (format nil "~A" sym))
       (teststr (subseq str begin-n end-n))
       (new-str (my-delete-substring delete-str teststr))
       (new-sym (my-make-symbol new-str))
       )  
    (values new-sym new-str )
    ;;end let, delete-symbol-str
    ))
;;TEST
;; (delete-symbol-str "<" '<symbx)
;; works= SYMBX  "SYMBX"



;;MY-MAKE-NEW-SYMBOL
;;
;;ddd
(defun my-make-new-symbol (root num &key (begin-str "")(mid-str "")(end-str ""))
  "In U-Tstring, appends num to a string (optionally with keys). RETURNS (values new-sym new-str"
  (let*
      ((new-str (format nil "~A~A~A~A~A" begin-str root mid-str num end-str))
       (new-sym (my-make-symbol (make-symbol-string new-str)))
       )
    (values new-sym new-str)
    ))
;;TEST
;;  (my-make-new-symbol "root" 6) = ROOT6  "root6"
;;  (my-make-new-symbol "root" 6  :begin-str "*" :mid-str "-" :end-str  "end") 
;; works= *ROOT-6END    "*root-6end"   
       




;;MAKE-LIST-OF-STRINGS
;;
;;ddd
(defun make-list-of-strings (objects &key list-each-item-p)
  "In U-tstring, INPUTS a symbol or object or list of symbols/objects.  RETURNS a list of strings of those symbols.  If list-each-item-p, puts each new string inside a separate list."
  (let
      ((object-str)
       (single-object-list)
       (new-list)
       )
    (cond
     ((listp objects)
      (loop
       for object in objects
       do
       (cond
         (list-each-item-p
          (setf new-list (append new-list (list (list (format nil "~A" object))))))
         (t (setf new-list (append new-list (list (format nil "~A" object))))))
       ;;end loop, listp
      ))
     (t (cond
         (list-each-item-p
          (setf new-list (list (list (format nil "~A" objects)))))
         (t (setf new-list (list (format nil "~A" objects)))))))

    ;;end let, make-list-of-strings
    new-list
    ))
;;TEST
;;  (make-list-of-strings 'this) = ("THIS")
;;  (make-list-of-strings "this is a test") = ("this is a test")
;;  (make-list-of-strings 'this :list-each-item-p t) = (("THIS"))
;;  (make-list-of-strings '(this is a test)) = ("THIS" "IS" "A" "TEST")
;;  (make-list-of-strings '(this is a test)  :list-each-item-p t) = (("THIS") ("IS") ("A") ("TEST"))
;;  (get-PC-inputs  'testpc) = ((IN1 0.2 PC1 FB) (IN2 0.7 FB) (IN3 0.5 SENS1) (IN4 0.1 SENS2))  
;; (make-list-of-strings '((IN1 0.2 PC1 FB) (IN2 0.7 FB) (IN3 0.5 SENS1) (IN4 0.1 SENS2)))
;;  works= ("(IN1 0.2 PC1 FB)" "(IN2 0.7 FB)" "(IN3 0.5 SENS1)" "(IN4 0.1 SENS2)")
;; (make-list-of-strings '(this 88 that (what) xx))
;; works= ("THIS" "88" "THAT" "(WHAT)" "XX")
;; (make-list-of-strings '((IN1 0.2 PC1 FB) (IN2 0.7 FB) (IN3 0.5 SENS1) (IN4 0.1 SENS2))   :list-each-item-p T)
;; works= (("(IN1 0.2 PC1 FB)") ("(IN2 0.7 FB)") ("(IN3 0.5 SENS1)") ("(IN4 0.1 SENS2)"))



;;MAKE-LIST-OF-1NESTED-STRINGS
;;2018
;;ddd
(defun make-list-of-1nested-strings (1nested-lists &key list-each-item-p 
                                                   head-string tail-string)
  "In U-tstring, INPUTS a list of 1nested lists of a symbol or object or list of symbols/objects.  RETURNS a list of strings of those symbols.  If list-each-item-p, puts each new string inside a separate list. If item not a list, then makes string of that item. If head-string or tail-string) adds to each string."
  (let
      ((strlist-lists)
       )
    (loop
     for list in 1nested-lists
     do
     (let
         ((strlist)
          )   
    (cond
     ((listp list)
      (setf strlist 
            (convert-items-to-strings list :list-each-item-p list-each-item-p
                                      :head-string head-string :tail-string tail-string)
            strlist-lists (append strlist-lists (list strlist))))
     (t (when list 
          (setf strlist (convert- list)
                strlist-lists (append strlist-lists (list strlist))))))
    ;;end let,loop
    ))
    ;;end let, make-list-of-1nested-strings
    strlist-lists
    ))
;;TEST
;; (make-list-of-1nested-strings '((b 3)(this & that) 88 (a b c) 55))



;;SET-STRING WORKS EFFICIENTLY
;;
;;(set-string "this" 99) this
;;ddd
(defun set-string (string value)
  "In U-tstring.lisp, converts string to a symbol, then sets the sym to value. RETURNS value."
    (set (my-make-symbol string) value))















;;************** FUNCTIONS TO DIVIDE STRINGS OR LISTS INTO SUBPARTS ******
;;
;; THESE ARE ESPECIALLY USEFUL FOR PRINTING OUT LINES FOR EQUAL LENGTH
;;	FROM LONG UNDIVIDED STRINGS



;;DIVIDE-SEQUENCE
;;
;;ARG: N/SEQ-LIST MUST BE A LIST SPECIFYING THE NUMBER OF PARTS FOR
;;	EACH LIST--IT IS ROBUST, 
;;		IF TOO SHORT IT WILL DROP LAST ITEMS
;;		IF TOO LONG IT WILL IGNORE AND ADD NILS FOR EXTRA ITEMS
;;
;;works w/dif nums
;;(divide-sequence '(a b c d e f g h i j k) '(3 2 4 4 3))
;;dss
(defun divide-sequence (sequence n/seq-list)
   "GWT-UTIL\TSTRING--seq is string or list--n/seq-list 
      eg.is '(4 2 3). Divides seq into list of 3 lists of 4 2 3 items"
  (let ((seq-length (length sequence))
        (begin-n)
        (end-n)
        (new-subseq)
        (list-of-subseqs)
        (N/SEQ)
	)
    (setf end-n 0
                list-of-subseqs nil)

    (dolist (n/seq n/seq-list)
      (setf n/seq n/seq
            begin-n  end-n
            end-n (+ begin-n n/seq))

      (if (> end-n  seq-length )
          (setf end-n  seq-length ))

      (cond
       ((> begin-n seq-length)
        nil)
       (t 

        ;;	  (PRINT `( n/seq ,n/seq begin-n ,begin-n end-n ,end-n))
        ;;CHANGE n/seq ONLY IF COMPLETED
    

        (setf new-subseq (subseq sequence begin-n end-n))
        (unless (null new-subseq)
          (setf list-of-subseqs (append list-of-subseqs (list new-subseq))))
        ))
      )
    list-of-subseqs
    ;;end let, defun
    ))




;;MY-DIVIDE-MULTI-LINE-STRING
;;
;;RETURNS LIST OF STRINGS of MAX length length/line
;; WITHOUT REPEATED SPACES & WITHOUT ORIGINAL CARRIAGE RETURNS
;; MISC CAN BE USED TO ENTER 'NO-TAB 'NO-CR OR ANY CHAR SUCH AS " "
;;	WHICH YOU DO NOT WANT REPEATED 
;; makes  **string-list
;;(setf **print-str "this is a long string to be divided" **string-list nil)
;;three substrings must be set to "" before beginning?

;;  (progn(my-divide-string 10 "or engl100 or its equivalent etc to make this longer " )(print `(,**first-pt ,**last-pt)))
;;WORKS FOR VARIETY OF NUMS(progn(setf **string-list nil **first-pt "" **last-pt "")(my-divide-multi-line-string **print-str 16 ))(print `(,**string-list)))
;;;do following to avoid probs with eval below
;;(setf no-tab 'no-tab no-cr 'no-cr)



;;MY-DIVIDE-MULTI-LINE-STRING
;;2020
;;ddd
(defun my-divide-multi-line-string (string max-line-length 
                                           &key divide-only-on-spaces-p (max-extra-line-length 10)
                                           (return-string-lists-p T))
  "U-tstring.lisp redivides string for equal lengths--deleting CR & NEWLINE--keeping all chars. DIVIDE-ONLY-ON-SPACES-P will make some lines TOO LONG. 
Adjust MAX-EXTRA-LINE-LENGTH and max-line-length to approximate desired effect.
NOTE: works for ONE-LINE strings as well."
  (let*
      ((len-str (length string))
       (new-string "")
       (new-strings-list)
       (new-line "")
       (rest-str "")
       )
    (loop
     for n from 0 to (- len-str 1)
     do
     (let*
         ((char (char string n))
          )
       (cond
        ((and divide-only-on-spaces-p (> n (- max-line-length 3))
              (char-equal char #\space))
         (setf new-string (format nil "~A~%~A" new-string new-line)
               new-strings-list (append new-strings-list (list new-line))
               rest-str (subseq string n))
         (return))  
        ((and (> n max-line-length)
              (or (null divide-only-on-spaces-p)
                  (> n (+ max-extra-line-length max-line-length))))
         (setf new-string (format nil "~A~%~A" new-string new-line)
               new-strings-list (append new-strings-list (list new-line))
               rest-str (subseq string n))
         (return))
        ((char-equal char #\newline)
         (setf new-line (format nil "~A~A" new-line #\space)))
        (t
         (setf new-line (format nil "~A~A" new-line char))))
       ;;end let,loop
       ))
    ;;(break "1")
    (cond
     ((> (length rest-str) 0)
      (multiple-value-bind (new-string1 new-strings-list1) ;; rest-str1)
          (my-divide-multi-line-string rest-str max-line-length 
                                       :divide-only-on-spaces-p divide-only-on-spaces-p)
        (setf new-string (format nil "~A~%~A" new-string new-string1)
                             new-strings-list (append new-strings-list new-strings-list1))))
     ((> (length new-line) 0)
      (setf new-string (format nil "~A~%~A" new-string new-line)
            new-strings-list (append new-strings-list (list new-string))))
     (T nil))
    (values new-string new-strings-list)
    ;;end let, my-divide-multi-line-string
    ))
;;TEST
#|  (my-divide-multi-line-string 
  "This is a test string that is going to be redivided into
parts that are
different than the ones that make it up originally
the lines will be different than this. They will
make up similar length lines." 30 :divide-only-on-spaces-p T)
"
This is a test string that is

 going to be redivided into parts

 that are different than the

 ones that make it up originally the

 lines will be different than

 this. They will make up similar

 length lines."
("This is a test string that is" " going to be redivided into parts" " that are different than the" " ones that make it up originally the" " lines will be different than" " this. They will make up similar" "
 length lines.")
|#
#|
 (my-divide-multi-line-string "This is a test string that is going to be redivided into
parts that are different than the ones that make it up originally."40 :divide-only-on-spaces-p T)
;;works=
"This is a test string that is going to

 be redivided into parts that are different

 than the ones that make it up originally."
("This is a test string that is going to" " be redivided into parts that are different" "
 than the ones that make it up originally.")
|#






;;MY-CONSTANT-LENGTH-STRING
;;
;;RETURNS STRING OF CONSTANT LENGTH 
;;    DROPS OFF ANY PART THAT IS TOO LONG
;;(my-constant-length-string 2 "this")
;;
(defun my-constant-length-string (length string)
  "U-tstring OLD: USE MY-DIVIDE-MULTI-LINE-STRING-works for one-line strings as well. "
  (let
      ((str-length (length string))
       (new-string)
       (fill)
       ) 
;;(print `(str-length ,str-length))  
  (cond
      ((equal str-length length)
       (setf new-string string))
      ((> str-length length)
       (setf new-string (subseq string 0 length)))
      ((< str-length length)
       (setf fill (make-string (- length str-length) :initial-element #\space))
       (setf new-string (format nil "~A~A" string fill)))
      (t nil))
    new-string))



;;(string #\comma) = ","
;;(string #\newline) works
;; (string #\period) = error; 
;; (string #\tab) = "       "

;;DIVIDE-STRING-TO-TOKENS
;;
;;ddd
(defun divide-string-to-all-tokens (string  &key tokens-list token-syms-list 
                                            (return-token-syms-p T) (char-delim-list  '(#\space))  
                                (delim-nth 0) 
                                (ignore-char-list '(#\comma #\.  #\tab #\newline  #\comma #\;  #\\
                                                            #\? #\[ #\] #\#  #\< #\>  #\|  #\( #\}     ))
                                (strs-not-part-of-symbol '("" "." ".." ")" "("))
                                 nth-char nth-word max-length word-delim-list delete-list)
  "U-TSTRING.lisp divides list or string at delim-nth (default 1) occurance of char-delim-list, RETURNS (values all-tokens-list rest-items) The TOKENS are STRINGS of string. nth-char (default 1) divides string at nth-char or after nth-word if not already divided; max-length limits max-length of entire returned string. Chars may want to put in ignore list include #\newline #\space #\tab #\comma. delete-list actively deletes all its items from the new-word-list (default is NIL, ""; set to NIL if no deletes wanted.). Stops after 2000 words unless change function def.  If tokens-list, appends this list with tokens from string (needed in recusion). If want SYMBOLS, use RETURN-TOKEN-SYMS-P."
  (let
      ((first-token)
       (first-sym)
       (rest-string)
       (all-tokens-list)  
       (all-token-syms-list)
      ;; (rest-items)
      ;; (process-string)
     ;;  (rest-str-length)
       )
    ;;ADDED 2019 for problem when first chars are spaces
    (setf string (my-delete-first-spaces string))
    (cond
     ((> (length string) 0)
      (multiple-value-setq (first-token rest-string)
          ;;note: unless no-initial-spaces = T, can cause stack-overflow.
          (divide-string-to-tokens string char-delim-list 
                                    :delim-nth delim-nth
                                   :ignore-char-list ignore-char-list :nth-char nth-char
                                   :nth-word nth-word :max-length max-length 
                                   :word-delim-list word-delim-list :delete-list delete-list))
      ;;make new all-tokens-list
      (when tokens-list (setf all-tokens-list tokens-list))
      #|was, if first item is token causes stack overflow (unless (string-equal first-token "")
        (setf all-tokens-list (append all-tokens-list (list first-token))))|#
      (cond
       ((string-equal first-token "")
        (setf rest-string (subseq rest-string 1)))
       (t (setf all-tokens-list (append all-tokens-list (list first-token)))))
      ;;added 2017
      (when return-token-syms-p 
        (when token-syms-list 
          (setf  all-token-syms-list token-syms-list))
        (setf first-sym (my-make-symbol first-token :string-not-equal 
                                        strs-not-part-of-symbol))
        (unless (stringp first-sym)
               (setf all-token-syms-list (append all-token-syms-list (list first-sym)))))
    ;;  (afout 'out (format nil "first-token= ~A rest-string= ~A~% all-tokens-list= ~A~%" first-token rest-string all-tokens-list))
  ;;CCC
    (cond
     ((> (length rest-string) 0)
      (multiple-value-setq (all-tokens-list rest-string all-token-syms-list)
          (divide-string-to-all-tokens rest-string :tokens-list all-tokens-list
                                       :token-syms-list all-token-syms-list
                                       :char-delim-list char-delim-list  :delim-nth delim-nth
                                       :ignore-char-list ignore-char-list :nth-char nth-char
                                       :nth-word nth-word :max-length max-length 
                                       :word-delim-list word-delim-list :delete-list delete-list)))
     (t nil))
    ;;end clause string > 0
    )
    (t nil))
    (values all-tokens-list rest-string all-token-syms-list)
    ))
;;TEST
;;PROBLEM 2019
;; (DIVIDE-STRING-TO-ALL-TOKENS " Volume in drive C is OS" :IGNORE-CHAR-LIST '(#\,  #\TAB)) 
;; RESULT = ("olume" "in" "drive" "C" "is" "OS")   ""    (*SYM-NOT-FOUND OLUME IN DRIVE C IS OS)
;; IF NO SPACE BEFORE FIRST WORKS
;; (DIVIDE-STRING-TO-ALL-TOKENS "Volume in drive C is OS" :IGNORE-CHAR-LIST '(#\,  #\TAB)) = ("Volume" "in" "drive" "C" "is" "OS")   ""   (VOLUME IN DRIVE C IS OS)

;; (DIVIDE-STRING-TO-ALL-TOKENS   ") :dir \"CogSys-Model\")" )
;; works = (")" ":dir" "\"CogSys-Model\")")   ""    (*SYM-NOT-FOUND DIR COGSYS-MODEL)



;;  (divide-string-to-all-tokens    "              51 File(s)      4,826,878 bytes")
;; works= ("51" "File(s)" "4826878" "bytes")  ""  (51 FILE 4826878 BYTES)
;;  (divide-string-to-all-tokens "This is a test of xxxxxxx")
;; works= ("This" "is" "a" "test" "of" "xxxxxxx")  ""    (THIS IS A TEST OF XXXXXXX)
;; CL-USER 6 > (divide-string-to-all-tokens "Another,  test of, this function")
;;works= ("Another" "test" "of" "this" "function")   ""   (ANOTHER TEST OF THIS FUNCTION)
;;
;; (divide-string-to-all-tokens "11/17/2013  05:54 PM            15,150 permutation.lisp")
;; works= ("11/17/2013" "05:54" "PM" "15150" "permutationlisp")  ""   (|11/17/2013| 554 PM 15150 PERMUTATIONLISP)
;;
;;  (divide-string-to-all-tokens  "\"DIR   c: \"" )
;;works= ("\"DIR" "c:" "\"")   ""    (DIR C EOF) ;;fix 
;;
;;  (divide-string-to-all-tokens "This is a test of xxxxxxx")
;; works= ("This" "is" "a" "test" "of" "xxxxxxx")  ""    (THIS IS A TEST OF XXXXXXX)
;; CL-USER 6 > (divide-string-to-all-tokens "Another,  test of, this function")
;;works= ("Another" "test" "of" "this" "function")   ""   (ANOTHER TEST OF THIS FUNCTION)
;;  (divide-string-to-all-tokens "/TEMP/PART/SUB/" :char-delim-list '(#\\ #\/ ))
;; works=  ("TEMP" "PART" "SUB")  "" (*SYM-NOT-FOUND TEMP *SYM-NOT-FOUND PART *SYM-NOT-FOUND SUB *SYM-NOT-FOUND)
;; NOTE: "/TEMP/PART/SUB/" caused stack overflow bec first item was a token and kept returning "" as first token.  Fixed with code modification noted above.

;; ;; (divide-string-to-all-tokens "1.2.3.4" :char-delim-list '(#\.) :ignore-char-list nil)
;; works = ("1" "2" "3" "4")    ""  (1 *SYM-NOT-FOUND 2 *SYM-NOT-FOUND 3 *SYM-NOT-FOUND 4)




;;DIVIDE-STRING-TO-TOKENS
;;
;;ddd
(defun divide-string-to-tokens (string char-delim-list 
                                       &key (no-initial-spaces T)
                                       (delim-nth 0) ignore-char-list nth-char 
                                       (nth-word 2000) 
                                       max-length 
                                       (word-delim-list '(" " #\space #\tab #\comma "," ";" "." "?" "-" "!" "'" )) 
                                       (delete-list '("" NIL #\\ "\\")))
  "U-TSTRING.lisp divides list or string at delim-nth (default 1) occurance of char-delim-list, RETURNS (values first-pt last-pt). of string. nth-char (default 1) divides string at nth-char or after nth-word if not already divided; max-length limits max-length of entire returned string. Chars may want to put in ignore list include #\newline #\space #\tab #\comma.
delete-list actively deletes all its items from the new-word-list (default is NIL, ""; set to NIL if no deletes wanted.). RETURNS (values first-pt last-pt new-word-list). Stops after 2000 words unless change function def."
  (let
      ((str-length (length string))
       (cur-char-num 0 )
       (delim-n 0)
       (char-delimiter-found-p)
       (word-delimiter-found-p)       
       (first-pt "")
       (last-pt "")
       (end-word-list "")
       (cur-word "" ) ;;was "")
       (word-n 1)
       (new-word-list )
      ;; (fin? 'no)
      (char-divide-fin-p)
       (cur-place 0)
       (cur-char "")
       (cur-char-string)
       (cur-place-num 0)
       (len-first+word)
       )
    ;;the default value = 1

    (dotimes (n  str-length)
      (setf cur-char (char string n)
            cur-char-string (string cur-char))

      ;;if cur-char is member of ignore-char-list, ignore it
      (if (or (member cur-char ignore-char-list  :test 'equal)
                  (member cur-char-string ignore-char-list :test 'string-equal))
          (setf cur-char-string ""))

      ;;if cur-char or cur-char-string member of char-delim-list, set flag char-delimiter-found-p
      (if  (or (member cur-char char-delim-list :test 'equal)
               (member cur-char-string char-delim-list :test 'string-equal))
          (setf  char-delimiter-found-p t
                 delim-n (+ delim-n 1)))    
                 
      ;;if cur-char or cur-char-string member of word-delim-list, set flag word-delimiter-found-p
      (if  (or (member cur-char word-delim-list :test 'equal)
               (member cur-char-string word-delim-list :test 'string-equal))
          (setf  word-delimiter-found-p t))
      
      ;;unless fin? = yes, append the first-pt string
      ;;was (unless (equal fin? 'yes)
        (cond
         ((and char-delimiter-found-p (>= delim-n delim-nth) (null char-divide-fin-p))
          (setf char-divide-fin-p t
           last-pt (subseq string n)))
         ((null char-divide-fin-p)
          (unless (string-equal cur-char-string "") ;;added
          (setf  first-pt (format nil "~A~A" first-pt cur-char-string))))
         (t nil))

        (cond
         ((= n  (- str-length 1))
          (setf cur-char-string (subseq string n)
                cur-word (format nil "~A~A" cur-word cur-char-string)
           new-word-list (append new-word-list (list cur-word))))
         (word-delimiter-found-p 
          (setf new-word-list (append new-word-list (list cur-word))
              word-n (+ word-n 1)
              word-delimiter-found-p nil
              cur-word "")
          (cond
           ((and nth-word (>= word-n nth-word ))
            (setf end-word-list (subseq string n)
                  new-word-list (append  new-word-list (list end-word-list)))
             (return))
           (t nil)))
           (t (setf  cur-word (format nil "~A~A" cur-word cur-char-string)))
         ;;end fist cond
         )
      ;;FILTER OUT CHAR REPEATS AS INDICATED IN MISC (& REST LIST)	
      ;;NO-LATER MAKE OPTIONAL??   (setf cur-char (my-delete-char-repeats cur-char misc)) 

    ;;  (afout 'out (format nil "n= ~A   cur-char= ~A cur-char-string= ~A cur-word= ~A ~%string= ~A~%first-pt= ~A~%last-pt= ~A~%new-word-list=~A~%" n   cur-char cur-char-string cur-word  string first-pt last-pt new-word-list  ))

      ;;end dotimes
      )
    ;;filter spaces if no-initial-spaces
    (if  no-initial-spaces
      (setf first-pt (my-delete-first-spaces first-pt)
            last-pt (my-delete-first-spaces last-pt)))
    ;;delete delete-list items
    (if delete-list
        (setf new-word-list (delete-list-items delete-list new-word-list)))

      ;;end divide-string-into-tokens
    (values first-pt last-pt new-word-list)
  ;;end let, divide-string-to-tokens
  ))
;;TEST 
;; ;;   (divide-string-to-tokens  "1. 81 to 100 percent;    2.  61 to 80 percent;    3.  41 to 60 percent;    4.  21 to 40 percent;    5.  1 to 20 percent;    6.  never/almost never;" NIL :word-delim-list '(";"))
;;works= 
#|"1. 81 to 100 percent;    2.  61 to 80 percent;    3.  41 to 60 percent;    4.  21 to 40 percent;    5.  1 to 20 percent;    6.  never/almost never;"
""
("1. 81 to 100 percent" "    2.  61 to 80 percent" "    3.  41 to 60 percent" "    4.  21 to 40 percent" "    5.  1 to 20 percent" "    6.  never/almost never;")
|#
;;
;;   (divide-string-to-tokens    "              51 File(s)      4,826,878 bytes" '(#\space))
;; works= ""  "51 File(s)      4,826,878 bytes"   ("51" "File(s)" "4" "826" "878" "bytes")
;; (divide-string-to-tokens "\"DIR   c: \""  '(#\space))
;; works= "\"DIR"    "c: \""    ("\"DIR" "c:" "\"")

;; (divide-string-to-tokens "(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" '(",") :no-initial-spaces T  :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";"))
;;
;; (divide-string-to-tokens "this is a test" '(" ")  :no-initial-spaces T  :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";")) 
;;results=  "thisisatest"  ""  ("thisisatest")
;;test
;; (divide-string-to-tokens "this, is a test" '(",")  :no-initial-spaces T  :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";"))
;;results= "this"   ", is a test"   ("this" "isatest")
;;test  (divide-string-to-tokens "this, is a test" '(#\space) :no-initial-spaces T  :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";"))
;;result = "this,"  "is a test"  ("this" "isatest")
;;
;; (divide-string-to-tokens "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"  '()  :word-delim-list '( #\\) :no-initial-spaces T  :ignore-char-list nil)
;; works= "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"   ""   ("C:" "3-TS" "LISP PROJECTS TS" "MyUtilities" "U-lists.lisp")
;; (divide-string-to-tokens "\\3-TS\\LISP PROJECTS TS\\MyUtilities\\"  '()  :word-delim-list '( #\\) :no-initial-spaces T  :ignore-char-list '(#\\))  
;; (divide-string-to-tokens "ACT-R-support:goal-style-module"  '()  :word-delim-list '( #\- #\:) :no-initial-spaces T  :ignore-char-list nil)
;;works = "ACT-R-support:goal-style-module"  ""  ("ACT" "R" "support" "goal" "style" "module")


#|(defun testnd ()
  (setf out nil)
  (let
      ((string "  public static final String intSrq8TellAllQ = \"I have told my partner almost everything about myself.\";")
        ;;"tknowmor	t-Want to know more of self")
       (no-initial-spaces T )
       ( char-nth 1) 
       ( char-delim-list '(" " #\space #\tab))
       (delim-nth nil)
       (ignore-char-list )
       (nth-char 1)
       (nth-word)
       (max-length 100)
       (word-delim-list '(" " #\tab))
       )
    (divide-string-to-tokens string char-delim-list :no-initial-spaces no-initial-spaces
                             :delim-nth delim-nth
                             :ignore-char-list ignore-char-list :nth-char nth-char 
                             :nth-word nth-word  :max-length max-length :word-delim-list word-delim-list)
;;word-delim-li
    ))|#
;;works, returns=
#|"tknowmor"
 "	t-Want to know more of self"
("tknowmor" "t" "Want" "to" "know" "more" "of" "self")
|#



;;MAKE-SYMBOL-STRING
;;2020
;;ddd
;;NAME
;;2020
;;ddd
(defun make-symbol-string (string  &key (not-incl-chars '(#\comma
                                                          #\tab #\newline #\space
                                                          #\comma #\;  #\\ #\? #\[ #\] #\# #\( #\) #\} ))
                                   (prefix ""))
  "U-tstring. RETURNS: String as input to my-make-symbol.  RETURNS string if doesn't include any of  not-incl-chars OR NIL"
  (let*
      ((new-string prefix)
       (xtra-chars)
       (len-string (length string))
       )
    (loop
     for  n from 0 to (- len-string 1)
     do
     (let*
         ((char (char string n) )
          )
       (cond
        ((not (member char not-incl-chars :test 'char-equal))
         (setf new-string (format nil "~A~A" new-string char)))
        (t (setf xtra-chars (append xtra-chars (list char)))))
     ;;end let,loop
     ))
    (values new-string xtra-chars)
    ;;end let, make-symbol-string
    ))
;;TEST
;; (make-symbol-string "this)")
;; works= "this"  (#\))





;;DIVIDE-LISTS-OF-STRINGS-TO-TOKENS
;;
;;SSS modify version below to take all these args??
;;(defun divide-lists-of-strings-to-tokens (string-list char-delim-list &key no-initial-spaces
;;                       delim-nth ignore-char-list nth-char nth-word max-length word-delim
;;;;NOTE--TAKES A REALLY LONG TIME TO WORK
;;
;;ddd
(defun divide-lists-of-strings-to-tokens (string-list &key (no-initial-spaces T)    
       (char-nth 1)   (char-delim-list '(" " #\space #\tab)) delim-nth
       ignore-char-list  (nth-char 1)  nth-word  (max-length 100)
       (word-delim-list '(" " #\tab)))
  "In U-tstring"
  (let*
      ((list)
       (string)
       (new-string-list)
       (first-pt)
       (last-pt)
  #|     (no-initial-spaces T)
       (char-nth 1) 
       (char-delim-list '(" " #\space #\tab))
       (delim-nth nil)
       (ignore-char-list )
       (nth-char 1)
       (nth-word)
       (max-length 100)|#
       (word-delim-list '(" " #\tab))
       )
    (dolist (list string-list)      
      (setf  string (car list))
     ;; (afout 'out (format nil "string= ~A~%" string))
      (multiple-value-setq (first-pt last-pt)
          (divide-string-to-tokens string char-delim-list :no-initial-spaces no-initial-spaces
                                   :delim-nth delim-nth
                                   :ignore-char-list ignore-char-list :nth-char nth-char 
                                   :nth-word nth-word  :max-length max-length
                                   :word-delim-list word-delim-list))
      (setf new-string-list (append  new-string-list (list (list first-pt last-pt))))
      ;;end dolist
      )
    ;;word-delim-list
    (setf  *shaq-sym-label-list2 new-string-list)
    ))

#|
;;test and actual use to change SHAQ lists--results-worked
;;NOTE--Takes a really long time to work
;;  (divide-lists-of-strings-to-tokens *part3 ) ;;*SHAQ-sym-label-list) ;; '( *test-shaq-list) ;;*SHAQ-sym-label-list) ;; '( ( "tknowmor	t-Want to know more of self") ("tknowmor1	    t-Want to know more of self1")))
|#

#|
(defun divide-lists-of-strings-to-tokens (string-list char-delim-list &key no-initial-spaces
                       delim-nth ignore-char-list nth-char nth-word max-length word-delim-list)
  (let
      ((new-string-list)
       (first-pt)
       (last-pt)
       (new-word-list)
       (string) ;; "tknowmor	t-Want to know more of self")
       (string-list)
       )
    ;;(afout 'out (format nil "string-list= ~A~%" string-list))
    ;;    (loop
    ;;     for list in string-list
    ;;     do
    (dolist (list string-list)
      (setf string (car list))
     ;; (afout 'out (format nil "string= ~A~%" string))
      (multiple-value-setq (first-pt last-pt new-word-list)
          (divide-string-to-tokens string char-delim-list :no-initial-spaces no-initial-spaces
                                   :delim-nth delim-nth
                                   :ignore-char-list ignore-char-list :nth-char nth-char 
                                   :nth-word nth-word  :max-length max-length :word-delim-list word-delim-list))
      (setf  string-list (list fist-pt last-pt)
             new-string-list (append new-string-list (list string-list)))
      ;;(afout 'out (format nil "new-string-list= ~A~%" new-string-list))
      ;;end loop
      )
    new-string-list
    ))

(defun runwss ()
  (setf out nil)
  (write-SHAQ-sym-label-list-divided))

;;ddd
(defun write-SHAQ-sym-label-list-divided ()
  (let*
      ((string-list1 *SHAQ-sym-label-list)
       ;;  (string "tknowmor	t-Want to know more of self")
       (no-initial-spaces T )
       ( char-nth 1) 
       ( char-delim-list '(#\tab));;for this list ;; '(" " #\space #\tab))
       (delim-nth nil)
       (ignore-char-list  '(" "))
       (nth-char 1)
       (nth-word)
       (max-length 100)
       (word-delim-list nil) ;; '(" " #\tab))
       )
   (format t "string-list1= ~A~%" string-list1)
    (setf  *SHAQ-sym-label-list-divided
          (divide-lists-of-strings-to-tokens string-list1 char-delim-list
                                             :no-initial-spaces no-initial-spaces
                                             :delim-nth delim-nth
                                             :ignore-char-list ignore-char-list :nth-char nth-char 
                                             :nth-word nth-word  :max-length max-length
                                             :word-delim-list word-delim-list))
    ))

|#
;;MY-DELETE-FIRST-SPACES
;;
;;ddd
(defun my-delete-first-spaces (string &key (delete-chars '(#\space  #\TAB))
                                      (delete-strs '(" " #\TAB)) delete-others)                                     
  "In U-tstring.lisp,  deletes all spaces including TABS before first non-space char. If  DELETE-OTHERS, appends the list of chars in delete-others to the other chars.  Any other modifications can be made by modifying the delete-chars and/or delete-strs lists."

  (when delete-others
      (setf delete-chars (append delete-chars delete-others)))
  (let
      ((cur-char)
       (cur-char-string)
       (str-length (length string))
       (new-string string)       
       )
    (dotimes (n str-length)
      (setf cur-char (char string n)
            cur-char-string (string cur-char))
      (cond
       ((or (member cur-char delete-chars  :test 'equal)
            (member cur-char-string delete-strs :test 'string-equal))
        (setf new-string (subseq string (+ n 1))))
       (t (return)))
      )
    new-string))
;;test
;;   (my-delete-first-spaces "     test string")
;;      (my-delete-first-spaces  "tknowmor")
;; works, returns "test string";; also works removing initial TABs
;; (equal " " #\space)
;;  (member #\b '(#\space " " #\TAB) :test 'char-equal) = ERROR " " NOT A CHAR
;;    BUT IF USE EQUAL, WON'T MATCH DIF CASES
;;  (member #\c  '(" " #\TAB) :test 'string-equal)  works= NIL
;;
;; TO DELETE BOTH FIRST AND LAST CHARS
;; (my-delete-first-spaces  (my-delete-last-spaces  "  \"C:/\"  " :delete-others '(#\")) :delete-others '(#\"))   
;; works= "C:/"

#| (string-trim "abc" "abcaakaaakabcaaa") =>  "kaaak"
 (string-trim '(#\Space #\Tab #\Newline) " garbanzo beans
        ") =>  "garbanzo beans"
 (string-trim " (*)" " ( *three (silly) words* ) ")
=>  "three (silly) words"

 (string-left-trim "abc" "labcabcabc") =>  "labcabcabc"
 (string-left-trim " (*)" " ( *three (silly) words* ) ")
=>  "three (silly) words* ) "

 (string-right-trim " (*)" " ( *three (silly) words* ) ") 
=>  " ( *three (silly) words"|#


;;DELETE-BEGIN-STRING
;;2020
;;ddd
(defun delete-begin-string (begin-strings string &key del-chars)
  "In U-tstring.lisp, USE CL STRING-LEFT-TRIM INSTEAD? -replaces older functions. DEL-CHARS must actually be a list of STRINGS, Works taking \\ or / off end of dirs"
  (let
      ((new-string) ;;(my-delete-last-spaces string :delete-strs final-strings :delete-chars del-chars ))
       (len-str (length string))
       )
    (loop
     for target-str in begin-strings
     do
     (let*
         ((len-target-str (length target-str))
          (len-dif (- len-str len-target-str))
          (begin-str (when (>= len-dif 0) (subseq string 0 len-target-str)))
          (result (string-equal begin-str target-str))
          )
       ;;(break)
       (when result
         (setf new-string (subseq string len-target-str ))
         (return))
       ;;end let,loop
       ))
    (unless new-string
      (setf new-string string))
    new-string
  ;;let,my-delete-begin-string
  ))
;;TEST
;; (delete-begin-string  '("begin.") "begin.a.bb.ccc.testcsym")
;; works= "a.bb.ccc.testcsym"
;; (delete-begin-string '("\\" "/") "\\subdir\\")
;; works= "subdir\\"
;; (delete-begin-string '("\\" "/") "/subdir/")
;; works= "subdir/"
;; (delete-begin-string '("<") "THIS")





;;DELETE-FINAL-STRING
;;2020
;;ddd
(defun delete-final-string (final-strings string &key del-chars)
  "U-tstring.lisp, USE CL STRING-RIGHT-TRIM INSTEAD? replaces older functions. DEL-CHARS must actually be a list of STRINGS, Works taking \\ or / off end of dirs"
  (let
      ((new-string) ;;(my-delete-last-spaces string :delete-strs final-strings :delete-chars del-chars ))
       (len-str (length string))
       )
    (loop
     for target-str in final-strings
     do
     (let*
         ((len-target-str (length target-str))
          (len-dif (- len-str len-target-str))
          (end-str (when (>= len-dif 0) (subseq string len-dif)))
          (result (string-equal end-str target-str))
          )
        (when result
         (setf new-string (subseq string 0 len-dif))
         (return))
        
       ;;end let,loop
       ))
  (when (null new-string)  ;;2020, was returning nil, no del-chars matched
    (setf new-string string))
    new-string
  ;;let,my-delete-final-string
  ))
;;TEST
;; (delete-final-string  '(".testcsym")   "a.bb.ccc.testcsym")
;; works= "a.bb.ccc"
;; (delete-final-string '("\\" "/") "\\subdir\\")
;; works= "\\subdir"
;; (delete-final-string '("\\" "/") "\\subdir/")
;; works= "\\subdir"
;; (delete-final-string '("\\" "/") "\\subdir")



;;DELETE-FINAL-STRINGS
;;2019
;;ddd
(defun delete-final-strings (final-strings strings &key del-chars)
  "In U-tstring.lisp, replaces older function, uses my-delete-last-spaces. Deletes final string in all members of a list."
  (let
      ((new-strings)
       )
    (loop
     for string in strings
     do
  (let
      ((new-string (my-delete-last-spaces string :delete-strs final-strings
                                          :delete-chars del-chars ))
       )
    (setf new-strings (append new-strings (list new-string)))
    ;;end let,loop
    ))
    new-strings
  ;;let,my-delete-final-strings
  ))
;;TEST
;; (delete-final-strings '("\\" "/") '("\\subdir\\" "G:/"))
;; works= ("\\subdir" "G:")


;;ENSURE-FINAL-STRING
;;2019
;;ddd
(defun ensure-final-string (string &key (delete-strings '("/" "\\"))
                                   (final-str "/"))
  "In U-tstring.lisp, makes sure a final string from final-strings) is at end. RETURNS. IF NOT, adds replacement-str (values new-string orig-end-ok)"
  (let
      ((new-string)
       (str-base (delete-final-string delete-strings string))
#|       (len-str (length string))
       (n-last)
       (orig-end-ok)|#
       )
    (setf new-string (format nil "~A~A" str-base final-str))
    
    #|(loop
     for finalstr in final-strings
     do
     (when finalstr
       (let*
           ((lenf (length finalstr))          
            ;;(n (- len 1))
            (endstr (subseq string (- len-str lenf)))
            )
         (setf orig-end-ok (string-equal endstr finalstr))
         ;;end let,when
         ))          
     (when orig-end-ok
       (return))
     ;;end loop
     )|#
    #|(cond
     (orig-end-ok
      (setf new-string string))
     (t (setf new-string (format nil "~A~A" string replacement-str))))|#       
    ;;(values new-string orig-end-ok)
    new-string
    ;;let,ensure-final-string
    ))
;;TEST
;; (ensure-final-string  "\\subdir\\")
;; works= "\\subdir/"
;; (ensure-final-string  "\\subdir/")
;; works= "\\subdir/"  
;; (ensure-final-string  "\\subdir")
;;works= "\\subdir/"  
;;;; (ensure-final-string  "\\subdir\\" :final-str "")
;; works = "\\subdir"



;;MY-DELETE-LAST-SPACES
;;2017
;;ddd
(defun my-delete-last-spaces (string &key (delete-chars '(#\space  #\TAB))
                                      (delete-strs '(" " #\TAB))  delete-others)
  "In U-tstring.lisp,  deletes all spaces including TABS AT END of string. If  delete-others, appends the list of chars in delete-others to the other chars.  Any other modifications can be made by modifying the delete-chars and/or delete-strs lists."
  (let
      ((cur-char)
       (cur-char-string)
       (n-str (length string))
       (new-string string)       
       )
    (when delete-others
      (setf delete-chars (append delete-chars delete-others)))
    (loop
       for n from 1 to  n-str
       do
      (setf cur-char (char string (- n-str n))
            cur-char-string (string cur-char))
      ;;(when (= n (- n-str 2))(break "cur-str"))
      (cond
       ((or (member cur-char delete-chars :test 'char-equal)  ;;was 'equal
            (member cur-char-string delete-strs :test 'string-equal))
        (setf new-string (subseq string 0 (- n-str n))))
       (t (return)))
      )
    new-string))
;;TEST
;; (my-delete-last-spaces  "a b c d this    ")
;;works = "a b c d this"
;; (my-delete-last-spaces  "  \"C:/\"" :delete-others '(#\")) = "  \"C:/"
;; on orig-end-oks of above, use my-delete-first-spaces
;; (my-delete-first-spaces  "  \"C:/" :delete-others '(#\"))  = "C:/"




;;MATCH-CHAR-LIST-TO-STRING
;;
;;ddd
(defun match-char-list-to-string (char-list string &key start end case-sensitive)
  "In U-tstring.lisp, start = 0, end = str-length by default. Seaches betw start and end. RETURNS (values rest-string first-string result-string). Char can be a symbol, string, or char. NUMBERS MUST be in form of a string or char. RETURNS (values result  rest-string first-string )"
  (let
      ((str-length (length string))
       (cur-char)
       (str-char)
       (result "")
       (first-string)
       (rest-string)
       )
    ;;set default part of string to search (all)
    (unless start
      (setf start 0))      
    (unless end
      (setf end  (- str-length 1 )))

  ;;test each char from start to en
    (loop
      for n from start to end
      do
      (setf cur-char (char string n)
            str-char (string cur-char))
      (cond
       ((null case-sensitive)
        (cond
         ((member str-char char-list :test 'string-equal)
          (setf result str-char)
          ;;  (afout 'out (format nil " n= ~A result= ~A~%" n result))
          (setf first-string (subseq string 0 n)
                rest-string (subseq string n))
          (return))
         (t (setf result "")))
        ;;end null case-sensitive
        )
       (t
        (cond
         ((member str-char char-list :test 'equal) 
          (setf result str-char)
          ;;  (afout 'out (format nil " n= ~A result= ~A~%" n result))
          (setf first-string (subseq string 0 n)
                rest-string (subseq string n))
          (return))
         (t (setf result "")))))
        ;;end loop
        )
    (if (equal result "")(setf result nil))
    ;;end match-substring-char-list
    (values result  rest-string first-string )
    ))
;;test
;;  (match-char-list-to-string '(c #\1 x) "  this abcd 123 ux")
;;works, returns "c" "cd 123 ux" "  this ab" 



;;FIND-MATCHED-STRINGS-FROM-SUBSTRINGS
;;2020
;;ddd
(defun find-matched-strings-from-substrings (match-str-list strings
                                                            &key (start 0) end case-sensitive)
  "In U-tstring.lisp, start = 0, end = str-length by default. Seaches betw start and end. RETURNS (values  found-items other-items found-ns)  for FIRST SUBSTRING MATCHED for each.  USE find-list-item-by-substrings to search list of strings. NOTE: first-string INCLUDES THE SUBSTRING at end. FOUND-N starts at 0"
  (let*
      ((found-items)
       (other-items)
       (found-ns)
       )
    (loop
     for str in strings
     for n from 0 to 5000
     do
     (cond  ;;(values rest-string first-string result-string)
            ((match-substrings match-str-list str :start start :end end
                               :case-sensitive case-sensitive)
             (setf found-items (append found-items (list str))
                   found-ns (append found-ns (list n))))
            (T (setf other-items (append other-items (list str)))))
     ;;end ,loop
     )
    (values  found-items other-items found-ns)
    ;;end let, find-matched-strings-from-substrings
    ))
;;TEST
;; (find-matched-strings-from-substrings '("ef" "22") '("abcdefgh" "1111e" "123" "abc22de")) 
;;works= ("abcdefgh" "abc22de")   ("1111e" "123") (0 3)
;; (find-matched-strings-from-substrings '("tom") '("C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\1 TOM-All-CSQ-DATA-2020-03-19.lisp" ))
;;works= ("C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\1 TOM-All-CSQ-DATA-2020-03-19.lisp")   NIL


;;MATCH-ALL-SUBSTRINGS
;;2019
;;ddd
(defun match-all-substrings (substring-list string &key start end case-sensitive
                                        (max-parts 100))
  "In U-tstring.lisp, start = 0, end = str-length by default. Seaches betw start and end. RETURNS  (values matched-substrs  first-strings n-matched)"
  (let
      ((matched-substrs)
       (n-matched)
       (first-strings)
       (test-string string)
       )
    (loop
     for n from 1 to max-parts
     do
    (multiple-value-bind (rest-str first-string result-string)
        (match-substrings substring-list test-string :start start :end end
                          :case-sensitive case-sensitive)
     (cond
      (first-string
       (setf matched-substrs (append matched-substrs (list result-string))
             first-strings (append first-strings (list first-string))
             test-string rest-str))
      (t (return)))
    ;; (break)
      ;;end mvb,loop
      ))
    (setf n-matched (list-length matched-substrs))
      (values matched-substrs  first-strings n-matched)
    ;;end let,match-all-substrings
    ))
;;TEST
;; (match-all-substrings  '(";") "  abc de;  lmno? ;  third one;  end" )
;; works= (";" ";" ";")  ("  abc de;" "  lmno? ;" "  third one;")  3


;;MATCH-SUBSTRINGS
;;
;;ddd
(defun match-substrings (substring-list string &key start end case-sensitive)
  "In U-tstring.lisp, start = 0, end = str-length by default. Seaches betw start and end. RETURNS (values rest-string first-string result-string) for FIRST SUBSTRING MATCHED.  USE find-list-item-by-substrings to search list of strings. NOTE: first-string INCLUDES THE SUBSTRING at end."
  (let
      (( rest-string)
       (first-string)
       (result-string)
       )
    (dolist (item substring-list)
      (setf item (string item))
      (multiple-value-setq  (rest-string first-string result-string)
          (match-substring item string :start start :end  end :case-sensitive case-sensitive))
      (if rest-string (return))
      ;;end dolist
      )
      (values rest-string first-string result-string)  
      ))
;;test 
;;  (match-substrings '(a x m ) "mthisa bx")
;; works returns "thisa bx" "m" "m"
;;(match-substrings '(a x ) "mthisa bx") =  "" "mthisa bx" "x"
;; (match-substrings '(f  g ) "mthisa bx") = NIL NIL ""
;;  ;; (match-substrings  '( = +)    "public static final String romSrq1SurpriseQ ="  ) ; :start 20)  
;; (match-substrings '(";") "  abc de;  lmno? ;  third one;  end" )
;; works= "  lmno? ;  third one;  end"    "  abc de;"   ";"
#|  (multiple-value-bind (a b c)
     (match-substrings '(";") "  abc de;  lmno? ;  third one;  end" )
    (values a b c))|#
;;works= "  lmno? ;  third one;  end"    "  abc de;"   ";"





;;MATCH-SUBSTRING
;;
;;ddd
(defun match-substring (substring string &key start end case-sensitive)
  "In U-tstring.lisp, start = 0, end = str-length by default. Seaches betw start and end. RETURNS (values rest-string first-string result-string) USE find-list-item-by-substrings to search list of strings. NOTE: first-string INCLUDES THE SUBSTRING at end."
  (let
      ((str-length (length string))
       (sub-length (length substring))
       (ss-char)
       (str-char)
       (result-string "")
       (first-string)
       (rest-string)
       (test-n -1)
       )
    ;;set default part of string to search (all)
    (unless start
      (setf start 0))      
    (unless end
      (setf end  (- str-length 1 )))

  ;;test each char from start to en
    (loop
      for n from start to end
      do
      (incf test-n)
      (setf ss-char (char substring test-n)
            str-char (char string n))
      (cond
       ((null case-sensitive)
      (cond
       ((char-equal ss-char str-char)
        (setf result-string (format nil "~A~A" result-string str-char))
      ;;  (afout 'out (format nil " n= ~A result-string= ~A~%" n result-string))
        (cond
         ((string-equal substring result-string)
          (setf first-string (subseq string  0  (+ n 1))
                rest-string (subseq string  (+ n 1)))
          (return))
         (t nil)))
       (t (setf result-string ""
                test-n -1)))
      ;;end null case-sensitive
      )
       (t
      (cond
       ((string-equal ss-char str-char)
        (setf result-string (format nil "~A~A" result-string str-char))
      ;;  (afout 'out (format nil " n= ~A result-string= ~A~%" n result-string))
        (cond
         ((string-equal substring result-string)
          (setf first-string (subseq string  0  (+ n 1))
                rest-string (subseq string  (+ n 1)))
          (return))
         (t nil)))
       (t (setf result-string ""
                test-n -1)))
        ;;end loop
        )))
    (values rest-string first-string result-string)
    ))
;;TEST
;; (match-substring "tom" "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\1 TOM-All-CSQ-DATA-2020-03-19.lisp" )
;; (match-substring "reverse" "LikeMe7Reverse")
;;works "" "LikeMe7Reverse"  "Reverse"
;; (match-substring "Reverse" "LikeMe7")
;;works (note "" above= T, NIL here)
;; NIL  NIL  ""
;; (match-substring "!"  "xx   this! that") 
;; = " that"  "xx   this!"  "!"
;;  (match-substring  "PCategory"   "PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" :start 10)    
;;works, returns:
#|"(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);"
"intSrq6Extra = new PCategory"
"PCategory"|#
;; (match-substring "input" "RESET-NINPUTS1")
;; = "S1"  "RESET-NINPUT"  "INPUT"
;; (match-substring  "RESET-NINPUTS" "RESET-NINPUTS1")
;; = "1"  "RESET-NINPUTS"  "RESET-NINPUTS"


#|  
  (match-substring  "PUBLIC"  " public static final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";")
;;RETURNS
" static final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";"
" public"
"public"
;; following also works
  (match-substring  "static"  " static final String intSrq6ExtraQ = ETC...

  (match-substring  "static" "  static final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";")
;;RETURNS
" final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";"
"static"
"static"
|#








;;MATCH-TOKEN-PHRASE
;;  
;;yyy
;;ddd
(defun match-token-phrase (phrase-token-list string &key start)
  "In U-tstring.lisp, searches for a match between a list of string symbols or token-strings and a string.  Eg.   public static final String intSrq6ExtraQ = . NOTE: CAN have intervening tokens, but tokens in list must be found in SAME ORDER as in list. RETURNS (values rest-string first-string result-string-list token-matched-p)"
  (let
      ((rest-string)
       (first-string)
       (result-string)
       (str-length (length string))
       (token-matched-p T)
       (result-string-list)
       )
    (unless start
      (setf start 0))
#|    (unless end
      (setf end (- str-length 1)))|#
 ;;    (afout 'out (format nil "phrase-token-list= ~A~% string= ~A~%" phrase-token-list string))
    (setf rest-string (subseq string start ))  ;;caused error (- end 2)))
    ;;check each token for match.  Must match IN ORDER.
    (loop
     for token in phrase-token-list
     do
;;     (afout 'out (format nil "1  result-string= ~A token= ~A~%rest-string= ~A" result-string token rest-string))
     (unless (null token-matched-p)       
       (setf token (format nil "~A" token)
             rest-string (format nil "~A" rest-string))
       (multiple-value-setq (rest-string first-string result-string)
           (match-substring token rest-string))  ;;:start start ))  ;;caused error (- end 1)))
       (setf start (- str-length (length first-string) 2))
       ;;  end (- (length rest-string) 3))
   ;;     (afout 'out (format nil "2  result-string= ~A token= ~A~%rest-string= ~A" result-string token rest-string))
       ;;check if the current token is matched
       (cond
        ((string-equal result-string token)
         (setf result-string-list (append result-string-list (list result-string))))
        (t (setf token-matched-p nil)
           (return)))
 ;;      (afout 'out (format nil "3  result-string= ~A token= ~A~%rest-string= ~A~%result-string-list= ~A~% token-matched-p= ~A~%" result-string token rest-string result-string-list token-matched-p))
       ;;end unless, loop
       ))    
     (values rest-string first-string result-string-list token-matched-p)
     ))
;;test
;;  (testmtp)
#|(defun testmtp ()
  (setf out nil)
  (let
      ((X) 
              )
 ;; works,   (match-token-phrase  '(public static)     "  public static String frameTitle = \"Romantic Relationship\";") = " String frameTitle = \"Romantic Relationship\";" " static" ("public" "static") T
;;also weren't being found 
 "  public static int frameDimWidth = 805;"
 "  public static int frameDimHeight = 460;"

;;    (match-token-phrase  '(public static final string) "public static final String romSrq5CelebrQ = \"My partner and I celebrate special days together almost once a month.\";")
  (match-token-phrase  '(public static final string) " public static final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";")  ;; :start 0)
  ))|#
;;works, returns:
#|" intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";"
" String"
("public" "static" "final" "String")|#
;;  (match-token-phrase  '(public static final string) "public class bsSelfManagement extends iExecSelf")

;; (match-token-phrase  '(in drive)     "VOL  E:



;;FUZZY-NESTED-LIST-SEARCHER
;;
;;ddd
(defun fuzzy-nested-list-searcher (search-item  nested-lists 
                                                &key return-symbols-p auto-cutoff  
                                                n-matched-cutoff   min-seq-length )
  "In U-tstring.lisp, matches search-item (symbol or string) to strings or symbols in a list that partially match.  RETURNS (values result-string-list result-sym-list result-outer-list) (incls search-item, matched-string-list).  If return-symbols, returns list of symbols as second value. If n-matched-cutoff not NIL and > number of matched chars, then NIL not string1 is returned. n-matched-cutoff simply requires that that many chars in seq in string1 match SOME chars in string2.    min-seq-length requires a SUBSTRING match of at least that many chars.  Works on pathnames, numbers, symbols, strings. Takes nested-lists of any? depth."
  (let
      ((result-string-list)
       (result-level-string-list)
       (result-string2)
       (result-string1)
       (result-sym-list)
       (result-level-sym-list)
       (result-sym2)
       (result-string-list1)
       (result-sym-list1) 
       (result-outer-list)
       (result-level-outer-list)
       (result-outer-list1)
       )
    (loop
     for item in nested-lists
     do
     #|     (when (and incl-nonlist-items-p
                (null (listp list)))
       (setf list (list list)))|#
     (cond
      ;;ITEM IS A LIST
      ((listp item)
       (multiple-value-setq (result-string-list1 result-sym-list1 result-outer-list1)
           (fuzzy-nested-list-searcher search-item item
                                       :return-symbols-p return-symbols-p
                                       :auto-cutoff auto-cutoff
                                       :n-matched-cutoff  n-matched-cutoff  
                                       :min-seq-length  min-seq-length))
         (when result-string-list1
           (setf result-level-string-list (append result-level-string-list (list result-string-list1))))
         (when result-sym-list1
                 result-level-sym-list (append result-level-sym-list (list  result-sym-list1)))
         (when result-outer-list1
           (setf result-level-outer-list (append result-level-outer-list (list result-outer-list1))))
                ;;not here result-outer-list (append result-outer-list (list item))))
         ;;(afout 'out (format nil "In listp item= ~A result-string-list1= ~A~%result-outer-list= ~A" item result-string-list1 result-outer-list))
         ;;(when result-sym-list1  (afout 'out (format nil "In listp item= ~A result-string-list1= ~A~%result-level-string-listt= ~A" item result-string-list1 result-level-string-list)))
         )
      ;; ITEM IS NOT A LIST
      (t
       ;;if item is a pathname
       (when (pathnamep item)
         (setf item (namestring item)))

       (setf result-string2 (fuzzy-matcher search-item item   :auto-cutoff auto-cutoff
                                       :n-matched-cutoff  n-matched-cutoff  
                                       :min-seq-length  min-seq-length ))
       (when result-string2
         (setf result-level-string-list (append result-level-string-list (list result-string2))
                              result-level-outer-list (append result-level-outer-list (list nested-lists)))
         (when return-symbols-p
           (setf result-sym2 (my-make-symbol result-string2)
                 result-level-sym-list (append result-level-sym-list (list result-sym2)))))
          ;;(when result-string2   (afout 'out (format nil "In NON-listp  item= ~A  search-item= ~A ~%RESULT-STRING2 ~A result-level-string-list= ~A" item search-item result-string2 result-level-string-list)))
         ;;end  t, cond
         ))
     ;;end  loop 
     )
    (when result-level-string-list
      (setf result-string-list (append result-string-list (list result-level-string-list))
            result-outer-list (append result-outer-list (list result-level-outer-list)))
      (when result-level-sym-list
        (setf result-sym-list (append result-sym-list (list result-level-sym-list)))))

    ;;(when result-level-string-list  (afout 'out (format nil "END: OUTSIDE LOOP: ~% result-level-string-list= ~a ~%result-string-list= ~a  "result-level-string-list result-string-list)))
    (values result-string-list result-sym-list result-outer-list)
    ;;let, fuzzy-nested-list-searcher
    ))
;;TEST
;;  (progn (setf out nil) (fuzzy-nested-list-searcher "lvietnam"  '((this "vietna" "other"  "etnam") (LANG "bio7lang" "English" "Spanish" "Vietnamese" "Cambodian" "Chinese" "Korean" "Portuguese" "German" "French" "Other Asian" "Other European" "Other"))  :min-seq-length 4))
;; works=  (("vietna" "etnam") ("Vietnamese"))   ((VIETNA ETNAM) (VIETNAMESE))   ((LANG "bio7lang" "English" "Spanish" "Vietnamese" "Cambodian" "Chinese" "Korean" "Portuguese" "German" "French" "Other Asian" "Other European" "Other") (LANG "bio7lang" "English" "Spanish" "Vietnamese" "Cambodian" "Chinese" "Korean" "Portuguese" "German" "French" "Other Asian" "Other European" "Other"))    
;;
;; (progn (setf out nil)(fuzzy-nested-list-searcher  "midsomer"   *resx :min-seq-length 4))

;; (fuzzy-nested-list-searcher "Steffens" '(#P"G:/0 Main OUR PHOTOS-VIDEOS/Steffens/"    ("0 Main OUR PHOTOS-VIDEOS" "Steffens")) :incl-nonlist-items-p nil)
;; works= ("Steffens")  NIL
#|
 (progn (setf out nil)(fuzzy-nested-list-searcher "Steffe" 
 '(:SUBDIR-INFO
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Xmas Photos/"
    ("0 Main OUR PHOTOS-VIDEOS" "Xmas Photos"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Tom Family/"
    ("0 Main OUR PHOTOS-VIDEOS" "Tom Family"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Tim & Trina/"
    ("0 Main OUR PHOTOS-VIDEOS" "Tim & Trina"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Steffens/"
    ("0 Main OUR PHOTOS-VIDEOS" "Steffens"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Snyders/"
    ("0 Main OUR PHOTOS-VIDEOS" "Snyders"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Sherry family/"
    ("0 Main OUR PHOTOS-VIDEOS" "Sherry family"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Sherry & Tom/"
    ("0 Main OUR PHOTOS-VIDEOS" "Sherry & Tom"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Maui/" ("0 Main OUR PHOTOS-VIDEOS" "Maui"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Home PalmDesert/"
    ("0 Main OUR PHOTOS-VIDEOS" "Home PalmDesert"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Friends - Family Unsorted/"
    ("0 Main OUR PHOTOS-VIDEOS" "Friends - Family Unsorted"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/Desert Photos/"
    ("0 Main OUR PHOTOS-VIDEOS" "Desert Photos"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/ALL PHOTOS-VIDEOS BY DATE/"
    ("0 Main OUR PHOTOS-VIDEOS" "ALL PHOTOS-VIDEOS BY DATE"))
   (#P"G:/0 Main OUR PHOTOS-VIDEOS/0-Our HEALTH IMAGES/"
    ("0 Main OUR PHOTOS-VIDEOS" "0-Our HEALTH IMAGES"))) :min-seq-length 4))
;;
;;works = ((("Steffens")))    ((NIL))    (((("0 Main OUR PHOTOS-VIDEOS" "Steffens"))))
|#




;;FUZZY-LIST-SEARCHER
;;
;;ddd
(defun fuzzy-list-searcher (search-item  list &key return-symbols-p
                                         auto-cutoff  n-matched-cutoff   min-seq-length )
  "In U-tstring.lisp, matches search-item (symbol or string) to strings or symbols in a list that partially match.  RETURNS (values matched-string-list matched-sym-list)(incls search-item, matched-string-list).  If return-symbols, returns list of symbols as second value. If n-matched-cutoff not NIL and > number of matched chars, then NIL not string1 is returned. n-matched-cutoff simply requires that that many chars in seq in string1 match SOME chars in string2.    min-seq-length requires a SUBSTRING match of at least that many chars. The returned matched-seq is the LAST matched seq."
  (let
      ((search-item-string)
       (item-string)
       (result)
       (matched-string)
       (matched-string-list)
       (result-string-list)
       (matched-sym)
       (matched-sym-list)
       (result-sym-list)
       )
    (when 
    (setf search-item-string (format nil "~A" search-item))

    (dolist (item list)
      (when (null (listp item))
          (setf item-string (format nil "~A" item)))
        (setf result  (fuzzy-matcher search-item-string  item-string
                                     :auto-cutoff auto-cutoff  :n-matched-cutoff n-matched-cutoff                                     :min-seq-length  min-seq-length))        
        ;;(afout 'out (format nil "search-item-string= ~A item-string= ~A~% result= ~A~%" search-item-string item-string  result))

        (when result
          ;;if match, make matched-string-list
          (setf matched-string-list (append matched-string-list (list result)))

          (when  return-symbols-p
            (setf matched-sym (my-make-symbol result)
                  matched-sym-list (append matched-sym-list (list matched-sym))
                  result nil)))
        ;;(afout 'out (format nil "matched-string-list= ~A" matched-string-list))
        ;;end when,dolist
        ))
#|    (setf result-string-list (list search-item matched-string-list))
    (if return-symbols-p 
        (setf result-sym-list (list search-item matched-sym-list)))    |# 
   
    ;;end fuzzy-list-searcher
    (values matched-string-list matched-sym-list)
    ))
;;TEST
;; (fuzzy-list-searcher "abcthisxyz" '(defmno lmthis lmxyzghi mywhat dexyzfghijk)  :auto-cutoff 0.5 :return-symbols-p t)
;;works, returns ("abcthisxyz" ("LMXYZGHI" "DEXYZFGHIJK")) ("abcthisxyz" (LMXYZGHI DEXYZFGHIJK))
;; (fuzzy-list-searcher 'abcthisxyz '(defmno lmthis  mywhat dexyzfghijk) :min-seq-length 4 :return-symbols-p t)
;;works, returns (ABCTHISXYZ ("LMTHIS" "DEXYZFGHIJK")) (ABCTHISXYZ (LMTHIS DEXYZFGHIJK))
;;(fuzzy-list-searcher 'abcthisxyz '(defmno lmthisu  mywhat dexyzfghijk)  :min-seq-length  3 :return-symbols-p t)     
;; (progn (setf out nil)(fuzzy-list-searcher "lvietnam"  '(LANG "bio7lang" "English" "Spanish" "Vietnamese" "Cambodian" "Chinese" "Korean" "Portuguese" "German" "French" "Other Asian" "Other European" "Other")  :min-seq-length 4))

;; ;; (fuzzy-list-searcher "Serial"  '(" this " "serially" " SERIAL " "NON-SERIAL"))
;; result (not work) = NIL  NIL




;;FUZZY-MATCH-IN-STRING-LIST
;;2019
;;ddd
(defun fuzzy-match-in-string-list (string string-list  &key return-symbol2-p auto-cutoff 
                                                n-matched-cutoff min-seq-length return-all-matches-p)
  "U-tstring,   RETURNS ( first-matched-string all-matched-strings)   Where all-matched-strings is a list of lists consisting of the return list above. "
  (let
      ((all-matched-strings)
       (all-matched-results)
       (first-matched-string)
       )
    (loop
     for list-str in string-list
     do
     (multiple-value-bind (string2-result string1  matched-seq matched-char-strings
                                          unmatched-char-strings  matched-chars unmatched-chars 
                                          matched-chars-n symbol2-result  )
         (fuzzy-matcher string list-str  :return-symbol2-p  return-symbol2-p 
                        :auto-cutoff auto-cutoff :n-matched-cutoff n-matched-cutoff 
                        :min-seq-length min-seq-length )
       ;;IF FOUND, RETURN OR CONTINUE
       (when string2-result
         (unless first-matched-string
           (setf first-matched-string string2-result))
         (setf  all-matched-strings (append all-matched-strings (list string2-result))
                all-matched-results (append all-matched-results
                                            (list (list string2-result string1  matched-seq 
                                                        matched-char-strings
                                                        unmatched-char-strings  matched-chars 
                                                        unmatched-chars 
                                                        matched-chars-n symbol2-result)))))
     (when (and string2-result (null return-all-matches-p))
         (return))     
       ;;(afout 'out (format nil "END LOOP first-matched-string= ~A string2-result= ~A" first-matched-string string2-result))
 
       ;;end mvb,loop
       ))
    (values first-matched-string all-matched-strings all-matched-results)
    ;;end let, fuzzy-match-in-string-list
    ))
;;TEST
;; (fuzzy-match-in-string-list "Serial"  '("ser" "SERIA" "serial"  "seral" "seriAL" "serially") :return-all-matches-p T)
;; works = "serial"   ("serial" "seriAL")  (("serial" "Serial" "" NIL NIL NIL NIL 0 NIL) ("seriAL" "Serial" "" NIL NIL NIL NIL 0 NIL))  
;; (fuzzy-match-in-string-list "serial" '("Volume" "Serial" "Number" "is" "4058-766B"))
;; (fuzzy-matcher "serial" "Serial") = "Serial" "serial"
;; (fuzzy-match-in-string-list "serial" '("Serial"))

;;FUZZY-MATCHER
;;
;;NOTE:  In U-lists.lisp this function is added to 
;;    COMPARE-NESTED-LIST-ITEMS to get fuzzy mataches of strings in lists
;;
;;ddd
(defun fuzzy-matcher (item1 item2 &key return-symbol2-p
                                      auto-cutoff  n-matched-cutoff   min-seq-length)
  "In U-tstring.lisp, matches items (strings or symbols) that partially match.  RETURNS 
   (values string2-result string1  matched-seq matched-char-strings unmatched-char-strings  matched-chars unmatched-chars  matched-chars-n symbol2-result) . If n-matched-cutoff not NIL and > number of matched chars, then NIL not string1 is returned. n-matched-cutoff simply requires that that many chars in seq in string1 match SOME chars in string2.    min-seq-length requires a SUBSTRING match of at least that many chars. The returned matched-seq is the LAST matched seq. If return-symbol2-p, then the 9th return is the symbol of item2--or NIL if no match."
  (let*
      ((string1 (format nil "~A" item1))
       (string2 (format nil "~A" item2))
       (length-str1 (length string1))
       (length-str2 (length string2))
       (char1)
       (char2)
       (char1-str)
       (char2-str)
       (matched-chars)
       (unmatched-chars)
       (matched-char-strings)
       (unmatched-char-strings)
       (matched-chars-n 0)
       (result-string)
       (matched-p)
       (matched-seq)
       (matched-seq-list) 
       (end-str)
       (match-str)
       (n-start 0)
       (n-end 0)
       (last-cycle-p)
       (seq-start-end-dif 0)
       (string2-result)
       (symbol2-result)
       )
    (cond
     ;;Try simple string-equal match for strings first
     ((and (stringp item1)(stringp item2) (string-equal item1 item2))
      (setf string2-result item2 
            string1 item1))
     (t
    ;;(afout   'out (format nil "AA string1= ~A String2=~A~% length-str1= ~A length-str2=~A~%" string1 String2 length-str1 length-str2))
    ;;if  auto-cutoff not nil, then either use default or auto-cufoff value
    (cond
     (auto-cutoff
      (cond
       ;;if numberp, auto-cutoff, leave alone
       ((and (numberp auto-cutoff)) NIL)
       ;;if not numberp, set auto-cutoff to default 
       (t (setf auto-cutoff  0.6)))
      ;;in either case use the auto-cutoff to calc the n-matched-cutoff for string1
      (setf n-matched-cutoff  (round (* length-str1 auto-cutoff))))
     (t nil))
     ;;(break "n-matched-cutoff")
    ;;compare the chars of the two strings.
    (dotimes (n1  length-str1) 
      (setf char1 (char string1 n1)
            char1-str (string char1))
      ;;to allow full processing of last char in string1
      (if (= n1 (- length-str1 1))
          (setf last-cycle-p T))

      (dotimes (n2 length-str2)  ;; (- length-str2 1))
        (setf char2 (char string2  n2)
              char2-str (string char2))
        ;;(afout  'out (format nil "BB char1=~A~% char2=~A~% n1=~A   n2=~A~%"   char1 char2 n1 n2 ))
        (cond
         ((or (string-equal char1-str char2-str) (char-equal char1 char2))
          (setf matched-p t )
          (cond
           ((and (= n-start 0) (not (= n2 0))
                 (setf n-start n2
                       n-end n-start)))
           (t (incf n-end)))
          (return)
          )
         (t nil ))
        ;;end inner dotimes
        )  
      ;;(afout 'out (format nil "CC  min-seq-length= ~A~% matched-seq-list= ~A~%  seq-start-end-dif = ~A~%  n-start 1= ~A  n-end 1= ~A~%matched-p= ~A~%" min-seq-length matched-seq-list  seq-start-end-dif  n-start n-end matched-p))
      (cond
       ;;if matched keep track of sequences of matched chars
       (matched-p
        (setf matched-chars-n (+ matched-chars-n 1)
              matched-chars (append matched-chars (list char1))
              matched-char-strings (append matched-char-strings (list char1-str))
              matched-seq-list (append matched-seq-list (list char1))))
        (t nil))
      
      ;;if not matched OR last char in string1, marks end of a matched-seq.
      (cond 
       ((or (not matched-p) (and last-cycle-p matched-p))
        (setf unmatched-chars (append unmatched-chars (list char1))
              unmatched-char-strings (append unmatched-char-strings (list char1-str))
              seq-start-end-dif (+ (- n-end  n-start) 1)) 
        ;; do return here
        (cond
         ;;to continue testing the seq must be longer than min-seq-length
         ((and min-seq-length matched-seq-list 
               (or (>= seq-start-end-dif min-seq-length)
                   (if (and last-cycle-p (>= seq-start-end-dif (- min-seq-length 1)))
                       T)))
          ;;make the chars into a string for test            
          (setf matched-seq  (format nil "~{~A~}" matched-seq-list))
          (multiple-value-setq (end-str match-str result-string)
              (match-substring matched-seq  string2)) 
;;works (match-substring "lvietnam" "Vietnamese") 
          ;;(afout 'out (format nil "DD- MATCH-SUBSTRING  n-start= ~A n-end= ~A~%matched-seq= ~A~% seq-start-end-dif= ~A~% result-string= ~A~%" n-start n-end matched-seq seq-start-end-dif  result-string  ))

          ;;if this the substring matches a substring in string2, 
          ;;       return MATCH = do nothing more (otherwise, string1 is set ot NIL)
          (cond
           ;;must search entire string2, since could have more than one occurance of beginning char
           ((string-equal matched-seq  result-string)
            (setf string2-result string2)
            (return))
           (t (setf string2-result nil)))
          ;;end seq length test clause
          )
         (t nil))
        ;;reset values if seq doesn't meet min standards (and the loop isn't broken by return)
        (setf matched-seq-list nil
              matched-seq ""
              seq-start-end-dif  0
              n-start 0
              n-end 0)
        ;;end of not matched-p
        )
       (t )) ;;never reset this?? (setf matched-chars-n 0)))
      
       ;;reset  matched-p for new inner do cycle
        (setf matched-p nil)

      ;;(afout   'out (format nil "EE  char1= ~A~% char2= ~A~%  char1-str= ~A~% matched-char-strings= ~A~% unmatched-char-strings= ~A~%  matched-seq-list= ~A~% n-start= ~A n-end= ~A~%" char1 char2  char1-str matched-char-strings unmatched-char-strings  matched-seq-list   n-start n-end))
      ;;end outer dotimes
      ) 
    ;;if these matched-cutoff not nil, and num chars matched < n-matched-cutoff, set string2= nil
    (cond
     (n-matched-cutoff
      (cond
       ((>  n-matched-cutoff  (length matched-chars))
        (setf string2-result nil))
       (t (setf string2-result string2))))
     (t nil)) 

    ;;if  n-matched-cutoff not nil, the max sequence must be > n-matched-cutoff or set string2=NIL
    (cond
     ((and n-matched-cutoff (>= matched-chars-n  n-matched-cutoff))
      (setf string2-result string2))
     (t NIL))  ;;no?? was (setf string1-result nil)))
    ;;end t, cond
    ))
    
    ;;if appropriate, make a symbol2-result, NIL if no match
    (if (and string2-result return-symbol2-p)
        (setf symbol2-result (my-make-symbol string2)))

    ;;(afout  'out (format nil "FF-END matched-chars-n= ~A~%  n-matched-cutoff= ~A~%string1-result= ~A~%" matched-chars-n  n-matched-cutoff string1-result ))
    ;;end my-fuzzy-matcher
    (values string2-result string1 matched-seq matched-char-strings unmatched-char-strings  matched-chars unmatched-chars  matched-chars-n symbol2-result)
    ))
;;TEST
;;2019
;; (fuzzy-matcher "Serial"  "  SERIAL ")
;;works = "Serial" "" ("S" "e" "r" "i" "a" "l") ("l") (#\S #\e #\r #\i #\a #\l) (#\l) 6 NIL
;; (fuzzy-matcher "Serial"  "  SERI  ")
;; results= NIL  "Serial"  ""  ("S" "e" "r" "i")  ("a" "l") 
;; (fuzzy-matcher "Serial"  "  SERIALLY  ")
;; results= NIL  "Serial" "" ("S" "e" "r" "i" "a" "l")  ("l")


;;(apply 'max '(1 2 3))
;;(apply 'max nil));; '(1 2 3))
;;ttt test 
;;(fuzzy-matcher string1 string2 (&key auto-cutoff  n-matched-cutoff n-matched-cutoff )
;;
;;NOTE: CHANGED ORDER OF VALUES RETURNED AFTER THESE TO
;;  (values result-string1 string2 MATCHED-SEQ MATCHED-CHAR-STRINGS UNMATCHED-CHAR-STRINGS  matched-chars unmatched-chars  max-matched-chars-n )
;; (fuzzy-matcher "Steffens" "Steffens")
;; (fuzzy-matcher "abcde" "xdey" :n-matched-cutoff  5)
;; works, returns NIL "xdey" (#\d #\e) (#\a #\b #\c) ("d" "e") ("a" "b" "c") 2
;;(fuzzy-matcher "abcde" "xdey" :auto-cutoff  0.5)
;; returns NIL"xdey" (#\d) (#\a #\b #\c) ("d") ("a" "b" "c") 0
;; (fuzzy-matcher "abcde" "xdey" :n-matched-cutoff 2) 
;; works= "xdey" "abcde" "" ("d" "e") ("a" "b" "c" "e") (#\d #\e) (#\a #\b #\c #\e) 2 NIL
;;(progn (setf out nil)(fuzzy-matcher "abcdefg" "xcdey" :auto-cutoff  t))
;;works, NIL "xcdey" (#\c #\d #\e) (#\a #\b #\f #\g) ("c" "d" "e") ("a" "b" "f" "g") 3
;;(fuzzy-matcher "abcde" "xdey" :auto-cutoff  0.2)
;; works, returns "xdey"  "abcde"  ""("d" "e")  ("a" "b" "c" "e")  (#\d #\e)  (#\a #\b #\c #\e)  2  NIL
;;(progn (setf out nil)(fuzzy-matcher "abcdefg" "xcdey" :n-matched-cutoff 2)) 
;;works, returns  "abcdefg" "xcdey" (#\c #\d #\e) (#\a #\b #\f #\g) ("c" "d" "e") ("a" "b" "f" "g") 3
;;;(progn (setf out nil)(fuzzy-matcher "CaseNum" "TColFacAd"  :n-matched-cutoff 4)) 
;;works, returns NIL "TCol(FacAd" (#\C #\C #\a #\a) (#\s #\e #\N #\u #\m) ("C" "C" "a" "a") ("s" "e" "N" "u" "m") 2
;;;(progn (setf out nil)(fuzzy-matcher "CaseNumFacTest" "TColFacAd" :min-seq-length 3  ))
;;result=  "TColFacAd"   "CaseNumFacTest"   "t"   ("C" "a" "F" "a" "c" "T" "t")  ("s" "e" "N" "u" "m" "e" "s" "t") (#\C #\a #\F #\a #\c #\T #\t)   (#\s #\e #\N #\u #\m #\e #\s #\t)  7  NIL
;;(progn (setf out nil)(fuzzy-matcher "lrnwrpap" "lrnskls" :min-seq-length 3 ))
;;works, result "lrnwrpap" "lrnskls" (#\l #\r #\n) (#\w) ("l" "r" "n") ("w") 0 "lrn"
;;(progn (setf out nil)(fuzzy-matcher "lrnwrpap" "lrnskls" :min-seq-length 4 ))
;; works, result NIL "lrnskls"(#\l #\r #\n #\r) (#\w #\p #\a #\p) ("l" "r" "n" "r") ("w" "p" "a" "p") 0 ""
;;(fuzzy-matcher "abcthisxyz" "lmthis"  :min-seq-length 3) (fuzzy-matcher "abcthisxyz" "lmthis"  :min-seq-length 3)
;;works,= "lmthis"  "abcthisxyz"  "this"  ("t" "h" "i" "s")  ("a" "b" "c" "x") (#\t #\h #\i #\s)  (#\a #\b #\c #\x) 4 NIL
;;(progn (setf out nil)(fuzzy-matcher "abcthisxyz" "dexyzfghijk"  :min-seq-length 3))
;; result= "dexyzfghijk"  "abcthisxyz"  "xyz" ("h" "i" "x" "y" "z")  ("a" "b" "c" "t" "s" "z")  (#\h #\i #\x #\y #\z)  (#\a #\b #\c #\t #\s #\z)  5 NIL
;;  (fuzzy-matcher "abcthisxyz"  "dexyzfghijk" :min-seq-length 4)
;;  (progn (setf out nil)(fuzzy-matcher "lvietnam" "Vietnamese"  :min-seq-length 4))





;;MATCH-FIRST-TOKEN 
;;
;;ddd
(defun match-first-token (token string &key delimiter-list case-sensitive)
  "In U-string.lisp, tests to see if token matches first word (as defined by delimiter #/space or " " OR chars/string in delimiter-list. Returns  (values first-token rest-string)"
  (let
      ((first-token)
       (rest-string)
       )
  (multiple-value-setq (first-token rest-string)
      (find-first-word  string :delimiter-list delimiter-list))
  (cond
   ((null case-sensitive)
    (if (string-equal token first-token)
        (values first-token rest-string)
      ))
   (t
    (if (string-equal token first-token)
        (values first-token rest-string))
      ))
  ))
    
;; (match-first-token  "PCategory"   "PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);")
;;works, returns the 2 parts.    
;;   (match-first-token "PCategory" " PCategory romSrq5Celebr = new PCategory(\"romSrq5Celebr\",3, romSrq5CelebrQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);")



;;FIND-WORDS-NUMBERS
;;2019
;;ddd
(defun find-words-numbers (string &key letter-only-p integer-only-p added-chars 
                                  (max-words 100))
  "In tstring, RETURNS (values found-item-list rest-str all-first-strings n-found-items)"
  (let
      ((found-item-list)
       (all-first-strings)
       (n-found-items 0)
       (rest-string)
       )
    (loop
     for n from 1 to max-words
     do
     (let
         ((first-str)
          ;; (rest-str)
          )
     (multiple-value-bind (found-item found-n rest-str word-len)
         (find-first-word-number string :letter-only-p letter-only-p 
                                 :integer-only-p integer-only-p :added-chars added-chars)
       ;;RETURN TEST
       (when (or (= word-len 0)(string-equal found-item "")(null found-item))
         (setf rest-string rest-str)
         (return))

       ;;append all-first-strings?
       (when (> found-n 0)
         (setf first-str (subseq string 0 found-n))
         (unless (or (string-equal first-str "") (null first-str))
           (setf    all-first-strings (append all-first-strings (list first-str)))))
       ;;append found-item-list
       (setf found-item-list (append found-item-list (list found-item))
             found-item nil
             string rest-str)
       ;;end mvb,let,loop
       )))
    (values found-item-list rest-string all-first-strings n-found-items)
     ;;end let,find-words-numbers   
    ))
;;TEST
;; words only
;; (find-words-numbers "   1. these are words! and nums= 12344; the end." :letter-only-p T)
;; works= ("these" "are" "words" "and" "nums" "the" "end")   "."   ("   1. " " " " " "! " " " "= 12344; " " ")   0  
;;integers only
;; ;; (find-words-numbers  "this is a3456bc 789  +567." :integer-only-p T)
;; works= ("3456" "789" "567")   "."  ("this is a" "bc " "  +")  0  
;;letters & added
;; (find-words-numbers "   1. These are words! and nums. (whatever) = 12344;  two-with-hyphens! the end." :letter-only-p T :added-chars '( #\! #\; #\. #\(  #\- #\)  ))
;; works= ("These" "are" "words!" "and" "nums." "whatever)" "two-with-hyphens!" "the" "end.")    "."     ("   1. " " " " " " " " " " (" " = 12344;  " " " " ")    0






;;FIND-FIRST-WORD-NUMBER
;;2019
;;ddd
(defun find-first-word-number (string &key letter-only-p integer-only-p added-chars)
  "In U-tstring, finds first letter or integer in a string. If letter-only-p, only letters, if integer-only-p, only numbers. RETURNS (values first-found-item found-n rest-string length). NEW VERSION older (more flexible?) is find-first-word."
  (let
      ((first-found-item)
       (end-n)
       (item-len)
       (delim)
       (found-char)
       (found-item)
       (first-found-char)
       )
    (multiple-value-bind (rest-string1 found-n first-found-char)
 ;; ( item length rest-string1)
        (find-first-letter/integer string :letter-only-p letter-only-p 
                                   :integer-only-p integer-only-p)
      (multiple-value-bind ( first-found-item length rest-string) 
          (find-word-integer-end rest-string1 :word letter-only-p
                                 :integer integer-only-p
                                 :added-chars added-chars) 
      (values  first-found-item found-n rest-string length)
    ;;end mvb,let, find-first-word-number
    ))))
;;TEST
;; letters-only-p
;; (find-first-word-number  "  ==>  1 2 crazy-word!  rest of words 22" :letter-only-p T)
;; letters-only-p with added chars = '(#\-)
;; (find-first-word-number  "  ==>  1 2 crazy-word!  rest of words 22" :letter-only-p T :added-chars  '(#\-))
;; works= "crazy-word"   11   "!  rest of words 22"  10
;; number-only-p
;; (find-first-word-number  "  ==>  crazy-word! 345 rest of words 22" :integer-only-p T)
;; works = "345"  19  " rest of words 22"  3
;; (find-first-word-number  "  ==>  crazy-word! 345! rest of words 22" :integer-only-p T)
;; when string has no more words
;; ;; (find-first-word-number  " +  .")
;; result = ""  4  "."  0
;; (find-first-word-number  "345. rest of words 22" :integer-only-p T)
;; works= "345"   0  ". rest of words 22"  3



;;FIND-WORD-INTEGER-END
;;2019
;;ddd
(defun find-word-integer-end  (string &key (word T) integer added-chars)
  "In U-tstring, String is a string that STARTS with the first letter of a word. This finnds the word end and RETURNS (values item length rest-string). If neither word nor integer, finds items that have BOTH letters and integers. ADDED-CHARS (CHARS only) is a list of additional chars that can be included in the item."
  (let
      ((item "")
       (length 0)
       (str-len (length string))
       (rest-string)
       )
    (loop
     for n from 0 to (- str-len 1)
     do
     (let
         ((char (char string n))
          (found-p)
          )
     (cond
      ((and added-chars
            (member char added-chars :test 'char-equal))
       (setf found-p T))
      (word
       (setf found-p (alpha-char-p char)))
      (integer
       (setf found-p (digit-char-p char)))
      (t
       (setf found-p (alphanumericp char))))
     (cond
      (found-p
       (setf item (format nil "~A~A" item char)))
      (t (return)))
            (incf length)
     ;;end let, loop
       ))
    (setf rest-string (subseq string  length))
    (values item length rest-string)
  ;;end find-word-integer-end
  ))
;;TEST
;; for letters only
;; (find-word-integer-end "THISIS  THE END")
;; works= "THISIS"  7  "  THE END"
;;for integers only
;; (find-word-integer-end "2340ab c " :integer T :word nil)
;; works= "2340"  4  "ab c "
;; for letters or integers combined
;; (find-word-integer-end  "2ab3de!the end" :word nil)
;; works= "2ab3de"   6  "!the end"
;; for added and words
;; (find-word-integer-end "this-word! abc 123 " :added-chars '(#\-))
;; works= "this-word"  9  "! abc 123 "



;;RETURN-FIRST-LETTERS/INTEGERS
;;NAME
;;2020
;;ddd
(defun return-first-letters/integers (string-list &key (omit-nils-p T) 
                                         letter-only-p integer-only-p)
  "U-tstring   RETURNS (values first-letters/integers rest-strs-list) INPUT:  "
  (let*
      ((first-letters/integers)
       (rest-strs-list)       
       )
    (loop
     for str in string-list
     do
     (let*
         ((first-let)
          )
     (multiple-value-bind (rest-str found-n first-found-char)
          (find-first-letter/integer str :letter-only-p letter-only-p 
                                            :integer-only-p integer-only-p)
       (setf first-let (format nil "~A" first-found-char)
             rest-strs-list (append rest-strs-list (list rest-str)))
          )
     (when (or first-let (null omit-nils-p))
       (setf first-letters/integers (append first-letters/integers (list first-let))))
     ;;end let,loop
     ))
    (values rest-strs-list first-letters/integers)
    ;;end let, return-first-letters/integers
    ))
;;TEST
;; (return-first-letters/integers '("F:\\" "    G:/") )
;;works=  ("F:\\" "G:/")  ("F" "G")

;; 
;;




;;FIND-FIRST-LETTER/INTEGER
;;2019
;;ddd
(defun find-first-letter/integer (string &key letter-only-p integer-only-p)
  "In U-tstring, finds first letter or integer in a string. If letter-only-p, only letters, if integer-only-p, only numbers. RETURNS (values rest-string found-n first-found-char )."
  (let
      ((first-found-char)
       (rest-string)
       (found-n)
       (str-len (length string))
       )
    (loop
     for n from 0 to (- str-len 1)
     do
     (let*
         ((char (char string n))
          (result)
          )
       (cond
        (letter-only-p
          (setf result (alpha-char-p char)
                first-found-char char
                found-n n)
         (when result
           (return)))        
        (integer-only-p
           (setf result (digit-char-p char)
                first-found-char char
                found-n n)
         (when result
           (return)))   
        (t
         (setf result (alphanumericp char) 
                first-found-char char
                found-n n)
         (when result
           (return))))
       ;;end let,loop
       ))
    (when found-n
      (setf rest-string (subseq string found-n)))
    (values rest-string found-n first-found-char )
    ;;end let, find-first-letter/integer
    ))
;;TEST
;;  (find-first-letter/integer "     THIS")
;; works = "THIS"  5   #\T
;;  (find-first-letter/integer "    =123XY93   end" :letter-only-p T)
;; works= "XY93   end"  8  #\X
;;  (find-first-letter/integer "    =>  ab %& 123XY93   end" :integer-only-p T)      
;; works= "123XY93   end" 14  #\1
;; for integers
;;  (find-first-letter/integer "a b 7. this isn't one" :integer-only-p T)


;;go here



;;FIND-FIRST-WORD
;; OLDER VERSION
;;ddd
(defun find-first-word (string &key delimiter-list)
  "In U-tstring.lisp, finds first word followed by a space in a string of various words, returns word and rest of string (minus that space). If delimiter list contains other chars to use as delimiters. OLDER VERSION, may have some extra FLEXIBILITY."
  (let*
      ((cur-char)
       (cur-char-string)
       (new-string "")
       (str-length (length string))
       (rest-string)
       (non-delimit-char-p)
       (found-first-char-p)
       )
    ;;default delimiter-list
    (unless delimiter-list
        (setf delimiter-list  `(#\space #\; #\, #\. #\( #\) #\tab #\+ #\[ #\] #\|    #\\ )))  
              ;;was (" " #\space "," ";" ")" "(" "]" "[" "?" #\tab "+")))
    ;;check each char for delimiter, accumulate rest into first and last string parts
    (dotimes  (n str-length)
      (setf cur-char (char string n)
            cur-char-string (string cur-char))
      ;;test for delimiter for each do item
      (cond
       ((or (member cur-char-string delimiter-list  :test 'string-equal))
            (member cur-char delimiter-list :test 'char-equal)
        (setf non-delimit-char-p nil))
       (t (setf non-delimit-char-p t)))
      ;;this only set once
      (if  non-delimit-char-p
          (setf found-first-char-p t))
      ;;look for space and return word and rest of string (without the space)
      (cond
       ((null found-first-char-p)  
        ;;if not ever found first char, do nothing yet
        nil)
       ;;if found first char,  test for delimiter= end first word
       ((null non-delimit-char-p)
        (setf rest-string (subseq string (+ n 1)))
        (return))
       ;;otherwise keep adding chars to first word
       (t (setf new-string (format nil "~A~A" new-string cur-char-string))))
    ;;  (afout 'out (format nil "cur-char-string= ~A~%new-string= ~A~%non-delimit-char-p= ~A~%found-first-char-p= ~A~%" cur-char-string new-string non-delimit-char-p found-first-char-p))
      ;;end dotimes
      )
    (values new-string rest-string)
    ;;end let, find-first-word-number
    ))
;;TEST
;;test
;;works, returns "this" "is a test"
#|(defun tm ()
  (let*
      ((string "   this is a test")
        )
    (setf out nil)
    (find-first-word string)
    ))|#

#|
         ("Email	")
         ("ZipCode")	
         ("Nation	")
         ("HrsWork")	
         ("UserRate")	
         ("tknowmor	t-Want to know more of self")
         ("texperie	t-Experienced self-help user")
         ("twanttho	t-Want thorough assessment")
         ("twantspe	t-Want specific help")
         ("tworknga	t-worknga")
|#


;;MY-FIND-STRING
;;2020
;;ddd
(defun my-find-string (match-string string  &key end-item (trim-items 0)
                                    trim-end-items match-all-from-end)
  "U-tstring   RETURNS    INPUT:  "
  (let*
      ((result (match-substring match-string string  :end-item end-item 
                       :trim-items trim-items   :trim-end-items  trim-end-items 
                        :match-all-from-end match-all-from-end))
       )
    (values result   )
    ;;end let, my-find-string
    ))
;;TEST
;; (match-substring "this"  "what is xxthisyy next")



;;FIND-STRINGS-IN-STRING
;;
;;ddd
(defun find-strings-in-string (string)
  "In U-tstring.lisp, finds all strings embedded in strings. RETURNS a list of them."
  (let
      ((string-length (length string))
       (char-str)
       (new-string)
       (string-list)
       (new-non-string)
       (non-string-list)
       (string-p)
       (non-string-p)
       (char)
       )
    (loop
     for n from 0 to (- string-length 1)
     do
     (setf char (char string n)
           char-str (string char))
     (cond
      ;;Is the char a double-quote--begin or end of inner string??
      ;;note (string-equal "\"" #\") = T
      ((or (char-equal char  #\") (string-equal char-str "\""))    
       (cond
        ;;quote is beginning of new string
        ((null string-p)
         (setf new-string char
               string-p t))
        ;;quote is end of new-string
        (t         
         (setf new-string (format nil "~A~A" new-string (string char))
          string-list (append string-list (list new-string)))))
       ;;FINISH SSS STARTING HERE
       )
      (t 
       (cond
        (string-p
         (setf new-string (format nil "~A~A" new-string (string char))))
        (t nil))))
     ;;end loop
     )
     string-list
     ))
;;TEST
;; (

;;test SSS
;;  (testsis)
#|(defun testsis ()
  (let
      ((string "  public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";")
       )
  (find-strings-in-string string)      
  ))|#
;;works, returns: ("\"A long term commitment (would) cause(s) me to feel trapped.\"")
     







;;MY-DIVIDE-STRING (Largely REPLACED BY DIVIDE-STRING-TO-TOKENS, 2014, ABOVE)
;;
;;(list *first-pt **last-pt)
;;WORKS ON ALL NUMBERS (my-divide-string 8  "this string is long")
;;(my-divide-string 8  "this        
;;string          is long" 'no-cr " ")
;;DDD
;;new version-SETS ABSOLUTE LIMIT ON WIDTH
;; ALSO IT CAN IGNORES TABS AND/OR CARRIAGE RETURNS
;;    JUST ADD 'NO-TAB OR 'NO-CR TO &REST ARGS
;;latest version
;;THIS FUNCTION IS INEFFICIENT IF THERE IS A LONG SECOND PART, BECAUSE
;;	IT READS EACH SECOND PART CHARACTER UNNCESSECARILY
;;
(defun my-divide-string (place string &rest misc)
   "In MyUtilities\\U-tstring.lisp sets absolute max width at place, returns values first-pt last-pt"
  (let
        ((str-length (length string))
	(cur-char 0)
	(first-pt "")
	(last-pt "")
	(last-char! "")
	(current-word "")
	(fin? 'no)
        (old-char)
        (cur-place)
        (cur-char-num)
        (len-first+word)
        )

  (cond
;;IF TOTAL STRING IS LESS THAN PLACE, JUST USE ENTIRE STRING
     ((<= str-length place) 
     (setf first-pt string))
;;OTHERWISE MUST DIVIDE IT
      (t
      (dotimes (n  str-length)
;;(print `(cur-char ,*cur-char **first-pt ,**first-pt  **current-word ,**current-word **last-pt ,**last-pt))	
         (setf cur-place n ;;(- n 1)
		  old-char cur-char
	          cur-char-num (char string cur-place)
		  cur-char (string cur-char-num))

;;FILTER OUT CHAR REPEATS AS INDICATED IN MISC (& REST LIST)	
         (setf cur-char (my-delete-char-repeats cur-char misc)) 


  ;;if it is a return, ignore it
	 (if (and (member 'no-cr misc :test 'equal)
	         (string-equal cur-char #\newline)) 
	    (setf cur-char ""))

  ;;if it is a TAB, ignore it
	 (if (and (member 'NO-TAB misc :test 'equal)
	         (string-equal cur-char #\tab))
	    (setf cur-char ""))

	(cond
	   ((equal fin? 'no)
;;(print 'no)
              (setf current-word (format nil "~A~A"
					current-word cur-char))
	      
	   (cond
	      ((or (string-equal cur-char " ")(= cur-place (- str-length 1)))
;;(print `(cur-place ,cur-place))
	      (setf len-first+word
  		 (+ (length first-pt)
			(length current-word)))
	      (cond
;;IF THE TOTAL STRING IS TOO LONG, 
		   ((> len-first+word place)
;;(print '>)	
		   (setf fin? 'yes
		      last-pt current-word))		
;;IF THE TOTAL STRING WITH BOTH WORD ISN'T TOO LONG, DONT EXIT
;;				 set first-pt = first plus BOTH WORDS
	           ((< len-first+word place)
	           (setf  first-pt (format nil "~A~A"
				first-pt current-word )
		      current-word ""))
;;IF EQUAL
	           ((= len-first+word place)
;;(print '=)
	           (setf fin? 'yes
		           first-pt (format nil "~A~A"
				first-pt  current-word)
		          current-word ""		
	                  last-pt ""))
	           (t (print `(ERROR? ON cur-char  ,cur-char )))))
	;;if cur-char not = " "
	       (t nil)))
;;IF FIN? 'YES JUST CHANGE LAST-PT
	   ((equal fin? 'yes)
;;(print 'Yes)
	    (setf last-pt (format nil "~A~A"
					last-pt cur-char)))
	   (t (print `(ERROR? cur-char ,cur-char)))))))
  (values first-pt last-pt)
  ;;end let,my-divide-string
  ))
;;TEST
;; ( my-divide-string  8 "THIS IS A TEST STRING TO SEE IF THIS WORKS")
;; works?  results = "THIS IS " "A TEST STRING TO SEE IF THIS WORKS"




;;FIND-STRING-AFTER-TOKEN-PHRASE
;;2017
;;ddd
(defun find-string-after-token-phrase ( phrase-token-list  string 
                                                           &key (start 0) n-target-string (n-betw 0))
  "In U-tstring  RETURNS "
  (let
      ((target-string)
       (length-tokens )
       (n-tokens)
       (length-token-phrase)
       (length-first-string)
       (n-to-string 0)
       )    
    (multiple-value-bind (rest-string first-string result-string-list 
                                      token-matched-p)
        (match-token-phrase  '(in drive E is)  string :start start)

      (multiple-value-setq ( length-tokens n-tokens)
          (find-total-strings-length phrase-token-list))
#|      (setf length-token-phrase (+ length-tokens (- n-tokens 1))
            length-first-string (length first-string)
            n-to-string (+ length-first-string length-token-phrase n-betw))|#
      (setf length-token-phrase (+ length-tokens (- n-tokens 1)))
      (cond
       (n-target-string
        (setf target-string (subseq rest-string n-betw 
                                    (+ n-betw  n-target-string))))
       (t 
        (setf target-string (subseq rest-string n-betw))))
      (values  target-string )
      ;;end mvb, let,  find-string-after-token-phrase
      )))
;;TEST
;; (find-string-after-token-phrase '("in" "drive" "e:") "VOL  E:  Volume in drive E is SD01-128  Volume Serial Number is 3634-6637" :n-betw 1 :n-target-string 10)
;; works= "SD01-128  "
        



;;FIND-TOTAL-STRINGS-LENGTH
;;2017
;;ddd
(defun find-total-strings-length (strings)
  "In U-tstring, RETURNS (values total-length n-strs)"
  (let
      ((n-strs 0)
       (total-length 0)
       )
    (loop
     for string in strings
     do
     (setf total-length (+ total-length (length string)))
           (incf n-strs)
     )
    (values total-length n-strs)
    ;;end let,find-total-strings-length
    ))
;;TEST
;;  (find-total-strings-length '("THIS" "NOW" "STEVENS"))
;; works= 14   3

   





;;MY-SIMPLE-CONCAT
;;
;;TAKES A MIXTURE OF SYMBOLS, NUMBERS, STRINGS IN A LIST &
;;    RETURNS A STRING OF THEM ALL
;;works(my-simple-concat '(this "test quote list" 999))
;;ddd
(defun my-simple-concat (string-symbol-list)
  "In U-tstring.lisp, GWT-UTIL\TSTRING"
  (let
      ((ss-list string-symbol-list)
       (print-str "")
       )
    (cond
     ((listp ss-list)
      (dolist (item ss-list)
        (setf print-str (format nil "~A ~A"  print-str item))))
     (t (setf print-str (format nil "~A ~A"  print-str ss-list))))
    print-str))



;;MY-DELETE-CHAR-REPEATS-IN-STRING
;;
;; (my-delete-char-repeats-in-string '("a" "c") "baccaaadccc")
;;(my-delete-char-repeats-in-string '(" " ) " this    is a   that")
;;
(defun my-delete-char-repeats-in-string (char-list sequence)
  "gwt-util\\tstring"
  (let
      ((new-str "")
       (last-char! "")
       (char)
       (new-char)
       )
    (dotimes (n (length sequence))
      (setf char (string (char sequence n))
            new-char (my-delete-char-repeats char char-list ))
      (setf new-str (format nil "~A~A" new-str new-char)))
    new-str
    ))


;;MY-DELETE-CHAR-REPEATS 
;;
;;DELETES ANY REPEATS OF ANY OF THE CHARS IN CHAR-LIST (in string form)
;;--MUST SETF *THIS-CHAR "" BEFORE USING THIS FUNCTION
;;(progn (setf *last-char! "")(my-delete-char-repeats "c" `("a" "c"))  "bacccdaagc"))
;;(member "c" '("a" "c") :test 'equal)
(defun my-delete-char-repeats (char char-list)
  (list "gwt-util\tstring--MUST SETF *last-char! = double-quotes first")
 ;;(print `(,*last-char! ,char ,char-list))
  (cond ((and (member char char-list :test 'string-equal) (string-equal char *last-char!))
	(setf *last-char! char) "")
	(t
	(setf *last-char! char)
	char)))

;;  STRING-EQUAL = BEST TEST??
;; (member #\b '(a b c) :test 'string-equal) = (B C)
;; (member #\b '(a "b" c) :test 'string-equal) = ("b" C)
;; (member #\b '(a  #\b c) :test 'string-equal) = (#\b C)
;; (member #\b '(a  "B" c) :test 'string-equal) = ("B" C)

;;MY-DELETE
;;
;; THIS DELETES ANY CHAR-STRING IN DELETE LIST FROM A STRING
;;
;;(my-delete "-" "- ENGL100")
;;(my-delete (string 10) "THIS IS A END")
;;ddd
(defun my-delete (delete-char-list string)
  "U-tstring, deletes a single char-string  in delete-char-list from string.delete-char-list can actually be a single non-list char-string."
  (let
      ((new-char-str "")
       (char)
       (char-str)
       (new-char-str "")
       )
  (dotimes (n (length string))
     (setf char (char string n)
           char-str (string char))
     (cond 
	((or
          (and (listp delete-char-list)
              (not (member char-str delete-char-list :test 'string-equal)))
          ;;if delete-char-list not a list
          (and (not (listp delete-char-list))
               (not (string-equal char-str delete-char-list))))
	 (setf new-char-str (format nil "~A~A"	
			new-char-str char-str)))
	(t nil))
     ;;end dotimes
     )
  new-char-str
  ))
;; (my-delete "-" "-ENGL100") = "ENGL100"
;; (member "a"  '("b" "A" "d") :test 'string-equal) = ("A" "d")
;;
;; (my-delete  '(";" "+" "=") "This = \"a test of whatever.\" + ;")
;; works, returns   "This  \"a test of whatever.\"  "
;; SSS
;;  (format t "~A~%" (my-delete '("," ";") "{\"12 or more\",\"11\",\"10\",\"9\",\"8\",\"7\",\"6\",\"5\",\"4\",\"3\",\"2\",\"1\",\"0\"};") ) ;;NIL 'EOF-FOUND)))  ))
;;  (my-delete '( #\space  #\newline) "This is a test        of 
;;my-delete        .")  =  "Thisisatestofmy-delete."
;;  (my-delete '("    "  #\newline) "This is a test        of  my-delete        .") ;;NOT WORK

;;MY-SEARCH
;;
;;DESIGN LATER, USE MY-SUBSTITUTE AS A BEGINNNING ALGORITHM??
;;

;;DELETE-CHARS
;;2017
;;ddd
(defun delete-chars (string chars)
  "U-tstring use STRING-TRIM INSTEAD?"
  (let
      ((new-string "")
       (deleted-chars-str "")
       (n-chars (- (length string) 1))
       )
    (loop
     for n from 0 to n-chars
     do
     (let
         ((char (char string n))
          )
      (cond
       ((member char  chars :test 'char-equal)
        ;;(break "here")
        (setf deleted-chars-str
              (format nil "~A ~A" deleted-chars-str char)))
       (t
        (setf new-string (format nil "~A~A" new-string char))))              
      ;;end let,loop
      ))
    (values new-string deleted-chars-str)
    ))
;;TEST
;;   ( delete-chars "this;that is: \dir a string"   '(#\/ #\\  #\: #\;))
;; works= "thisthat is dir a string"   " ; :"

#|
 ;;NOT NEEDED my-delete-substring WORKS FOR ALL IF DELETE-ALL-P
;;MY-DELETE-SUBSTRINGS -- DELETE 
;;2020
;;ddd
(defun my-delete-substrings (string  &key (delete-all-p t) from-end-p
                                     (test 'my-equal) (max-deletes 100))
  "U-tstring   RETURNS    INPUT:  "
  (let*
      ((new-string)
        (rest-string string)
        (result)
       )
    (loop
     for n from 1 to max-deletes
     do
     (let*
         ((x)
          )
      (multiple-value-bind (new-str first-string rest substring
                                        STRING-FOUND-P)
           (my-delete-substring substring rest-string :delete-all-p delete-all-p
                       :from-end-p from-end-p :test test)
         (setf new-string (format nil "~A~A" new-string first-string)
               rest-string rest)
    (cond
     ((or (null result)(null rest-str)(equal resto-str ""))
      (return))
     (
      )
     (t nil))
  

     ;;end mvb,let,loop
     )))

    (values    )
    ;;end let, my-delete-substrings
    ))
;;TEST
;; (my-delete-substrings

|#



;;MY-DELETE-SUBSTRING
;;2020 added string-found-p
;;ddd
;;new version using my-equal
(defun my-delete-substring (substring string &key (delete-all-p t) from-end-p (test 'my-equal))
  "In U-tstring.lisp, Deletes the first substring. RETURNS (values new-string first-string rest-string substring STRING-FOUND-P) new-string = nil if no match."
  (if from-end-p
      (setf string (reverse string)
            substring (reverse substring)))      
  (let
      ((length-str (length string))
       (length-ss (length substring))
       (begin-n 0)
       (last-match-n 0)
       (first-string "")
       (rest-string "")
       (new-string "")
       (string-found-p)
       (test-ss)
       (end-n)
       )
    (loop
     for n from 0 to length-str
     do
     (setf end-n (+ n length-ss))
     (cond
      ((and (>= length-str (+ last-match-n length-ss))
            (>= length-str  end-n)
            (>= n last-match-n))
       (setf begin-n n)
       (setf test-ss (subseq string begin-n end-n))
       (cond
        ((funcall test substring test-ss) ;;was(my-equal substring test-ss)
         (setf string-found-p T
               first-string (format nil "~A~A" first-string
                                    (subseq string  last-match-n begin-n))
               rest-string (subseq string end-n   length-str)
               new-string (format nil "~A"  first-string) 
               last-match-n end-n)
         ;;return after first match?
         (unless delete-all-p 
           (setf new-string (format nil "~A~A" first-string rest-string))
           (return)))
        (t ;;(/= n end-n)
           (setf new-string (format nil "~A~A" new-string (subseq string n (+ n 1))))))
       )
      ((<= length-str (+ last-match-n length-ss ))
       (setf new-string (format nil "~A~A" new-string 
                                (subseq string  last-match-n  length-str)))
       (return))      
      ((<= length-str  (+ begin-n length-ss ))
       (setf new-string (format nil "~A~A" new-string 
                                (subseq string  (+ begin-n 1) length-str )))
       (return))
      (t nil))
     ;;(afout 'out (format nil "new-string= ~A begin-n=~A end-n=~A~%" new-string begin-n end-n))
     ;;end loop
     )
    ;;in case no match found
    (if (null string-found-p)
        (setf  new-string string))
    (if from-end-p
        (setf new-string (reverse new-string)
              first-string  (reverse rest-string)
              rest-string (reverse first-string)))
    ;;end my-delete-substring
    (values new-string first-string rest-string substring string-found-p)
    ))
;;TEST
;; (my-delete-substring "old"  "thisoldstuff")
;; works= "thisstuff"  "this" "stuff" "old"  T

;; (my-delete-substring "-"  "-2.73")   
;; works= "2.73"  "" "2.73" "-" T
;; (my-delete-substring "-"  "2.73")   
;;works= "2.73"  "" "" "-" NIL
;; PRE 2020--no string-found-p returned -------------------------------------
;; (progn (setf out nil) (my-delete-substring "this"  "123this4567890"))
;; works "1234567890" "123"  "4567890" "this"
;; (progn (setf out nil) (my-delete-substring "this"  "123this4567this890"))
;;works "1234567890" "1234567" "890" "this"
;; (progn (setf out nil) (my-delete-substring "this"  "123567this" ))
;; works "123567" "123567" "" "this"
;; (progn (setf out nil) (my-delete-substring "this"  "  xxxthis what "))
;;works, returns "  xxx what " "  xxx" " what ""this"
;; (progn (setf out nil) (my-delete-substring "this"  "123this4567this890" :delete-all-p nil)) "1234567this890" "123" "4567this890" "this"
;; (progn (setf out nil) (my-delete-substring "this"  "123this4567890"))
;; works "1237890" "123" "7890" "this"
;;  (my-delete-substring "  "  "  xxx  this is                 what            .")
;; works= "xxxthis is what."
;; (progn (setf out nil) (my-delete-substring "this"  "this"))
;;works = "" "" "" "this" 


;;MY-SUBSTITUTE
;;
;;ddd
(defun my-substitute (newitem olditem sequence &key (start 0) end 
                                           match-case-p  (count 0)  from-end-p)
  "In U-tstring.lisp. If count, substitutes newitem for only count occurramces of olditem in sequence. Note: number of spaces in newitem and olditem need NOT be the same.  start and end refer to orig sequence even if  from-end-p = t. NOTE: olditem CAN BE A LIST of items which can substitute new-item for. RETURNS (values new-seq match-n-list). match-n-list lists matches starting with beginning at 0."
  (let
      ((newitem-str (string newitem))
       (new-char)
       (new-char-str "")
       (olditem-str "")
       (old-char)
       (old-char-str "")
       (olditem-length)
       (newitem-length)
       (sequence-str "")
       (seq-char)
       (seq-char-str "")
       (seq-length (length sequence))
       (match-n 0)
       (count-n 0)
       (seq-first-pt "")
       (seq-last-pt "")
       (new-seq "")
       (last-matched-end-n)
       (match-n-list) ;;list of ns for matching elements
       )
    (cond
     ((and (setf newitem-str (string newitem)
                 olditem-str (string olditem)
                 sequence-str (string sequence)))
      (setf  olditem-length (length olditem-str)
             newitem-length (length newitem-str))

      (unless end
        (setf end (- seq-length 1)))

      (loop
       for i from start to end
     ;;  with n 
       do
       (let*
           ((n)
            )
       ;;from-end-p?
       (cond
        (from-end-p
         (setf n (- end  i)
                 match-n (- olditem-length 1)))
        (t (setf  n  i)))
       ;;set chars and char strings
       (setf seq-char (char sequence-str n)
             seq-char-str (string seq-char)
             new-char (char newitem-str match-n)
             new-char-str (string new-char)
             old-char (char olditem-str match-n)
             old-char-str (string old-char))
          ;;   seq-first-pt (format nil "~A~A" seq-first-pt seq-char-str))

          ;;(afout 'out (format nil "newitem-str= ~A~% olditem-str= ~A~% sequence-str= ~A~% old-char-str= ~A seq-char-str=~A~% newitem-str= ~A~%new-seq= ~A~% seq-length= ~A olditem-length= ~A~% n= ~A~% match-n= ~A~%"newitem-str olditem-str sequence-str  old-char-str seq-char-str newitem-str new-seq seq-length olditem-length n match-n))	
       (cond
        ;;DO THE CURRENT CHARS-STRS MATCH?
        ((or (and (null match-case-p) ;;if don't need to match case
                  (string-equal old-char-str seq-char-str))
             ;;if do need to match case
             (equal old-char-str seq-char-str))

         ;;IF CHAR-STRS MATCH, THEN 
         (incf match-n)
         (setf match-n-list (append match-n-list (list n)))
         ;;(afout 'out (format nil "MATCHED match-n= ~A~%" match-n))
         ;;is end of old-item reached? Length of new-item not important.
         (cond
          ((= match-n  olditem-length)
           (incf count-n)
           (cond
            (from-end-p
             (setf  new-seq (format nil "~A~A" newitem-str new-seq ) ;; seq-last-pt)
                    match-n 0))
            (t          
             (setf  new-seq (format nil "~A~A" new-seq newitem-str) ;; seq-last-pt)
                    match-n 0)))
           ;;(afout 'out (format nil "MATCHED ALL match-n= ~A count-n= ~A~% new-seq= ~A~%" match-n count-n new-seq))
           ;;if count, return or increment count-n, otherwise nothing
           (cond
            (count
             (cond
              ((= count-n count)
               ;;if count limit reached, return new-seq w/ rest of seq attached
               (cond
                (from-end-p
                 ;;here new-seq is starting from end, accumulating gradually, 
                 ;;   seq-last-pt is part left over -- which is first part of sequence
                 (setf seq-last-pt (subseq sequence start (+ n  1))
                                       ;;was  (string  (subseq sequence start (+ n  1))
                       new-seq (format nil "~A~A"  seq-last-pt new-seq)))
                 (t 
                  (setf seq-last-pt (string  (subseq sequence (+ n 1)))
                        new-seq (format nil "~A~A" new-seq seq-last-pt))))
               (return))
              (t nil)))
             (t nil))
           ;;end of matched whole seq.
           )
          ;;if matched, but not end of olditem-str incf match-n
          (t 
           ;;end matched clause
           )))
           ;;CHARS DON'T MATCH
           (t (setf  match-n 0)
              (cond
               (from-end-p t
                 (setf  new-seq (format nil "~A~A" seq-char-str  new-seq )))
               (t (setf  new-seq (format nil "~A~A" new-seq  seq-char-str))))
            ))
          ;;end loop and set items to strings clause
          )))
     (t (format t "ERROR--ONE OF THE OBJECTS==> ~A~%, ~A~%   OR ~A  IS NOT A STRING--IN QUOTES" newitem-str olditem-str sequence-str)))
         
    ;;what to return
    (cond
     ((> (length new-seq) 0)
      (values new-seq match-n-list))
     (t nil))
    ))
;;TEST
;;    (my-substitute   #\newline   (read-from-string "\\n")  "anbncndnee\n") = subs for all n's
;;   (read-from-string "\\n") = \n
;;  (my-substitute   #\newline (format nil "~C~A" #\\  "n")  "anbncndnee\n") = "anbncndneen"
;;  NOTE: "anbncndnee\n" EVALS to "anbncndneen" (drops \)
;;  not-work (my-substitute  (my-substitute  (format nil "~%") (format nil "\\n")  "anbncndnee\n") "\\n"  "anbncndnee\n") = "anbncndneen" (only eliminates \)
;; not-work (my-substitute  #\newline "\n"  "anbncndnee\n") = newline sub for every n
;;works, (my-substitute  "xx" "n"  "anbncndneen") =  "axxbxxcxxdxxeexx" (1 3 5 7 10)
;;  (my-substitute  "xx" "n"  "anbncndneen" :count 2)
;; works = "axxbxxcndneen" (1 3)
;; works (progn (setf out nil) (my-substitute "xx" "n"  "anbncndnee\n" :from-end-p t))
;; works (progn (setf out nil) (my-substitute "xx" "n"  "anbncndnee\n" :from-end-p t :count 2))
;;"anbncndnxxeexx"
;;  (my-substitute "xx" "n"  "anbncndnee\n" :from-end-p t :count 1)
;; works= "anbncndneenxx"
;;  (my-substitute "xx" "n"  "anbncndnee" :from-end-p t :count 1)
;; works= "anbncndnxxee"
;; (my-substitute  #\newline (format t "\\~A" #\n)  (format t "~A" "anbncndnee\n"))
;; (my-substitute  #\newline #\\ "anbncndnee\n")
;;works (progn (setf out nil) (my-substitute  "xyz" "abc" "xyabcde")) = "xyxyzde"
;;not work: (progn (setf out nil) (my-substitute  "xyz" "abc" "xyabcde" :from-end-p t))
;;works (my-substitute  "and" "&" "&xyabcde") = "andxyabcde"
;;works (my-substitute  'and "&" "xya & bcde") = "xya AND bcde"
;;Following only works if num spaces matches in newitem and olditem
;;  works (my-substitute  (format nil "~% ") "\\n" "ab cn de \\n  x yn z") 
;; works (my-substitute (string #\newline) "   " "this is a    this is second" ) 
;;works = "thisisa
;; thisissecond"
;;  (char  (string #\newline) 0) = #\Newline
;; (print "\\n" :escape :ESCAPE *PRINT-ESCAPE*)



;;MY-SUBSTITUTE-IN-STRINGS
;;2019
;;ddd
(defun my-substitute-in-strings (new-item old-item strings &key (start 0)
                                        match-case-p (count 0))
  "U-tstring. Substitutes new-str for old-str each time it finds it in strings list. Uses my-stubstitue: . If count, substitutes newitem for only count occurramces of olditem in sequence. Note: number of spaces in newitem and olditem need NOT be the same.  start and end refer to orig sequence even if  from-end-p = t. NOTE: olditem CAN BE A LIST of items which can substitute new-item for. RETURNS (values new-seq match-n-list). match-n-list lists matches starting with beginning at 0."
  (let
      ((new-strings)
       )
    (loop
     for string in strings
     do
     (let 
         ((new-str (my-substitute new-item old-item string :start start :match-case-p 
                                  match-case-p :count count))
          )
       (setf new-strings (append new-strings (list new-str)))
       ;;end let,loop
       ))
    new-strings
    ;;end let, my-substitute-in-strings     
    ))
;;TEST
;; (my-substitute-in-strings "" "/"  '("g:/" "h:/"))  = ("g:" "h:")
       


;;
;;CONVERTS ANY KIND OF STRING WITH ANY MIXTURE TO INITIAL CAPS-REST LOWER CASE
;;--PRECEDING SPACE SIGNALS NEXT LETTER TO BE A CAPITAL
;;(char " " 0)
;;works (convert-to-upper&lower-case "TEST STRING of letters" t)
;;
(defun convert-to-upper&lower-case (string roman-num?)
  (list "gwt-util\\tstring")
  (setf *char #\newline
	*newstring "")
  (dotimes (n (length string))
    (setf *oldchar *char 
	*char (char string n))
    (cond
	((or (string-equal *newstring "")
	   (string-equal *oldchar #\space )  ;(code-char 73)#\I
         (and (equal roman-num? 'yes)(string-equal *oldchar #\I)
		(string-equal *char #\I)))
	(setf *newchar (string (char-upcase *char))
		*oldchar *newchar))
	(t  (setf *newchar (string (char-downcase *char)))))
    (setf *newstring (string-append *newstring (string *newchar))))
   *newstring)	



;;CONVERT-STRING-TO-INTEGER
;;2020
;;ddd
(defun convert-string-to-integer (str &optional (radix 10))
  "In U-tstring, from Steel, p236, given a digit string & optional radix, return an integer uses CL PARSE-INTEGER."
  (parse-integer str)
  )
;;TEST
;;  (convert-string-to-integer "568") = 568
;; (parse-integer "568")  = 568  3
;;  (convert-string-to-integer "-568") = -568  4

#| DOES NOT WORK
(defun convert-string-to-integer (str &optional (radix 10))
 "In U-tstring, from Steel, p236, given a digit string & optional radix, return an integer use CL PARSE-INTEGER."
   (do ((j 0 (+ j 1))
	(n 0 (+ (* n radix)
		(or (digit-char-p (char str j) radix)
			(error "Bad radix-~D digit: ~C"
				radix (char str j))))))
	((= j (length str)) n)))|#





;;CONVERT-STRING-SUBSEQ-TO-INTEGER
;;
;;ddd
(defun convert-string-subseq-to-integer (string start &optional end)
  "In U-tstring, converts begin-n to end-n chars in string to an integer (if they are numbers). To find integers in the string, combine with find-integers-in-string function."
  (let
      ((seq)
       (str-n (length string))
       )
    (when (and end (>= end str-n 1))
      (setf end (- str-n 1)))
    (setf seq (subseq string start end))
    (convert-string-to-integer seq)
    ))
;;TEST
;; (convert-string-subseq-to-integer "this123that" 4 5)  = 1
;; (convert-string-subseq-to-integer "this123that" 5 7)    = 23
;; ;; (convert-string-subseq-to-integer "this123that" 5 8) 
   



;;FIND-INTEGERS-IN-STRING
;;
;;ddd
(defun find-integers-in-string (str &key return-ints-as-strings-p)
  "In U-tstring, RETURNS (values integer-list token-list alpha-list), token-list= a list of integers and connecting strings in order appear in a string, alpha-list= all chars except integers. Returns units, eg. \"132\" = 132 and  abc = abc not a b c. Returns all integer strings as integers unless return-ints-as-strings-p. "
  (let
      ((char)
       (last-char-int-p)
       (int-str "")
       (int)
       (integer-list)
       (alpha-str "")
       (alpha-list)
       (token-list)
       (str-length (length str))
       )
    (loop
     for n from 0 to (- str-length 1)
     do
     (setf char (char str n))
    ;; (format T "char= ~A~%digit-char-p= ~A~%" char (digit-char-p char))
     (cond
      ((digit-char-p char)
       (setf int-str (format nil "~A~A" int-str char))
       (when  (and (null last-char-int-p)(not (string-equal alpha-str "")))
         (setf token-list (append token-list (list alpha-str))
               alpha-list (append alpha-list (list alpha-str))
               alpha-str ""))
       (when (= n (- str-length 1))
         (cond
          (return-ints-as-strings-p
           (setf integer-list (append integer-list (list int-str))
                 token-list (append token-list (list int-str))))
          (t
           (setf int (convert-string-to-integer int-str)
                 integer-list (append integer-list (list int))
                 token-list (append token-list (list int)))))
         ;;end when
         )         
       (setf  last-char-int-p T))
      (t
       (setf alpha-str (format nil "~A~A" alpha-str char))
       (when  (and last-char-int-p (not (string-equal int-str ""))) 
         (cond
          (return-ints-as-strings-p
           (setf integer-list (append integer-list (list int-str))
                 token-list (append token-list (list int-str))))
          (t
           (setf int (convert-string-to-integer int-str)
                 integer-list (append integer-list (list int))
                 token-list (append token-list (list int)))))
         (setf  last-char-int-p nil  int-str "")
         ;;end when
         )
       (when (= n (- str-length 1))
         (setf token-list (append token-list (list alpha-str))
               alpha-list (append alpha-list (list alpha-str)))
         ;;end when
         )
       ;;end t, cond
       ))
    ;; (format T " integer-list=~A~% token-list=~A~% alpha-list= ~A~%"  integer-list token-list alpha-list)
     ;;end loop
     )
    (values integer-list token-list alpha-list)
    ;;end let, find-integers-in-string
    ))

;;NOTE: CAN ALSO USE PARSE-INTEGER FOR SOME APPLICATIONS
;;  (parse-integer "x33y" :junk-allowed t) = nil 0
;; (parse-integer "33 " :junk-allowed t) = 33  2
;;  (parse-integer "-33- " :junk-allowed t) =  -33  3

;;TEST
;;  (find-integers-in-string "this232is a 56 test33")
;;  works= (232 56)  ("this" 232 "is a " 56 " test")  ("this" "is a " " test")
;;  (find-integers-in-string "62this232is a 56 test33yu")
;;   works= (62 232 56 33)  (62 "this" 232 "is a " 56 " test" 33 "yu")  ("this" "is a " " test" "yu")
;;  (find-integers-in-string "62this232is a 56 test33yu" :return-ints-as-strings-p T)
;;  works= ("62" "232" "56" "33")  ("62" "this" "232" "is a " "56" " test" "33" "yu")  ("this" "is a " " test" "yu")
;;  (find-integers-in-string "Wup4-2")
;;  works= (4 2)  ("Wup" 4 "-" 2) ("Wup" "-")
;;  (find-integers-in-string  "mov_2345.mp4")





;;CONVERT-STRING-TO-FLOAT
;;
;;ddd
(defun convert-string-to-float (float-str &optional (radix 10))
  "In U-tstring, based on Steel, p236, given a float digit string & optional radix, return an integer. If float-str is number, returns it, otherwise returns nil."
  (let
      ((length-str) 
       (digit-list)
       (decimal-digit-list)
       (decimal-digit-p)
       (num-sign '+)
       (number)
       (element)
       (digit)
       )
    (cond
     ((numberp float-str)
      (setf  number float-str))
     ((stringp float-str)
      (setf length-str (length float-str))
      (loop
       for i from 0 to (- length-str 1)
       do
       (setf element (char float-str i))
       (cond
        ((char-equal element #\-)
         (setf num-sign '-))
        ((char-equal element #\.)
         (setf decimal-digit-p T))
        (T
         (setf digit (digit-char-p element radix))
         (cond
          (decimal-digit-p
           (setf decimal-digit-list (append decimal-digit-list (list digit))))
          (t
           (setf digit-list  (append digit-list (list digit)))))
         ))
       ;;end loop
       )
      (setf number (convert-digit-list-to-float digit-list decimal-digit-list))
      ;;end stringp clause
      )   
     (t nil))
    number
    ;;end let, convert-string-to-float
    ))
  

;;TEST
;; (convert-string-to-float "123.45")



;;CONVERT-DIGIT-LIST-TO-FLOAT
;;
;;ddd
(defun convert-digit-list-to-float (digit-list &optional decimal-digit-list)
  "In U-tstring"
  (let*
      ((number (convert-digit-list-to-integer digit-list))
       (decimal-num (convert-digit-list-to-integer decimal-digit-list))
       (num-decimals (list-length decimal-digit-list))
       (decimal-multiplier (/ 1.0 (expt 10 num-decimals)))
       )
    (setf number (+ number (* decimal-num  decimal-multiplier)))
    ;;end let, convert-digit-list-to-float
  ))
;;TEST
;;  (convert-digit-list-to-float '(2 3 4) '(9 8 7 6))
;; works =  234.9876

;;CONVERT-DIGIT-LIST-TO-INTEGER
;;
;;ddd
(defun convert-digit-list-to-integer (digit-list) 
  "In U-tstring"
  (let
      ((multiplier 1)
       (new-num 0)
       (i -1)
       (num)
       )
    (loop
     for digit in (reverse digit-list)
     do
     (incf i)
     (if (> i 0) (setf multiplier (* multiplier 10)))
      (setf num (* multiplier digit)
           new-num (+ new-num num))
     ;;end loop
     )
    new-num
    ;;end let, convert-digit-list-to-number
    ))
;;TEST
;;  (convert-digit-list-to-integer  '(1 2 3))
;; works = 123






;;SEARCH-FOR-MEMBER
;;
;;ddd
(defun search-for-member (sequence list &key return-first-match-p
                                   reverse-search-p use-my-equal-p)
  "In U-Tstring,  DEPRECIATED, USE MATCH-SUBSTRINGS INSTEAD. Searches sequence to see if any member of list is found as part of the sequence using CL search. List cannot contain numbers (unless strings). RETURNS (values result begin-index end-index matches). Unless return-first-match-p, returns best match and all other matches. When reverse-search-p, searches for list item as subseq of sequence. If use-my-equal-p, uses my-equal instead of string-equal for test. ALSO SEE find-best-match."
  (let
      ((result)
       (begin-index)
       (end-index)
       (best-begin-index)
       (best-end-index)
       (matches)
       (seq1)
       (seq2)
       (best-match) 
       (length-best-match 0)
       (length-item)
       )
    (when (listp list)
    (loop
     for item in list
     do
     (setf  length-item (length item))
     ;;(afout 'out (format nil "item= ~A sequence= ~A" item sequence))
     ;;reverse-search-p?
     (cond
      ((null reverse-search-p)
       (setf seq1 sequence
             seq2 item))
      (t (setf seq1 item
               seq2 sequence)))

     (cond
      ((null use-my-equal-p)
       (setf begin-index
             (search  seq1 seq2  :test 'string-equal)))
      (t (setf begin-index
               (search  seq1 seq2  :test 'my-equal))))

     (when begin-index
       (setf end-index (-  length-item  begin-index 1))

       (setf matches (append matches (list item)))
       (cond
        (return-first-match-p
         (setf result item
                 best-begin-index begin-index
                 best-end-index end-index
                 length-best-match length-item)
         (return))
        (t 
         (cond
          ((>= length-best-match length-item) 
           NIL)
          (t 
           (setf result item
                 best-begin-index begin-index
                 best-end-index end-index
                 length-best-match length-item)))))
       ;;end when match
       )
     ;;end loop, when listp
     ))
    (values result begin-index end-index matches)
    ;;end let, search-for-member
    ))
;;TEST
;; (search-for-member "RESETI-3-6" '("X" "Y" "RESET" "WUP")) 
;; = NIL NIL NIL NIL
;; (search-for-member "RESETI-3-6" '("X" "Y" "RESET" "WUP") :reverse-search-p T)
;;WORKS= "RESET"  NIL 4 ("RESET")
;; (search-for-member '("THIS" "THAT" "33") '("77" "THAT" "X")) = NIL
;;   (search-for-member "WUPI-3-3TOI-L-6" *all-art-symbol-strings)
;; works= "Wup"  0  2
;;  (search-for-member "RESET-NINPUTS1" '("Input" "X" "Y" "R" "Wup" "Wdn" "Uup" "Udn" "Y-Output" "RESET" "RESET-NINPUTS" "RESET-NOUTPUTS" "RESET-COUNTER" "N-CATS" "TEMP" "TEMP2") :reverse-search-p t) zzzz
;; WORKS = "RESET-NINPUTS"  NIL 12 ("Input" "R" "RESET" "RESET-NINPUTS")
;;Won't search within strings:
;;  (search-for-member "a bb c; xxxyz; this is the end; more" '(";"))

;; (search-for-member "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\1 TOM-All-CSQ-DATA-2020-03-19.lisp" '("tom") :use-my-equal-p T)




;;FIND-BEST-MATCH
;;
;;ddd
(defun find-best-match (string str-list &key reverse-find-p)
  "In U-tstring.  Finds best match (by num of consequtive char matches) of string with items in a list. string must be the substring. If reverse-find-p, reverses--looks for the items in the string. RETURNS (values best-match length-best-match best-match-n all-matches best-matched-item all-matched-items) .   If a tie, first is returned. NOTE: If string or item not strings, converts them."
  (let
      ((subseq-match)
       (begin-index)
       (end-index)
       (match-length 0)
       (best-match-n)
       (best-matched-item)
       (n-items (list-length str-list))
       (string-str string)
       (item-str)
       (str1 )
       (str2)
       (rest-str)
       (first-str)
       (matched-str)
       (best-match)
       (length-best-match 0)
       (matched-item)
       (all-matched-items)
       (all-matches)
       )
    (unless (stringp string)
      (setf string-str (format nil "~A" string)))
    (loop
     for item in str-list
     for n from 0 to (- n-items 1)
     do
     (cond
      ((stringp item)
       (setf item-str item))
      (t
      (setf item-str (format nil "~A" item))))

     ;;which is the substring?
     (cond
      ((null reverse-find-p)
       (setf str1 string-str
             str2 item-str))
      (t (setf str1 item-str
               str2 string-str)))
     
     ;;match
     (multiple-value-setq (rest-str first-str matched-str)
                (match-substring str1 str2))
    ;;(afout 'out (format nil "str1= ~A str2= ~A matched-str= ~A" str1 str2 matched-str))
     (when (not (equal matched-str ""))
       (setf match-length (length matched-str)
             matched-item item
             all-matched-items (append all-matched-items (list matched-item))
             all-matches (append all-matches (list matched-str)))
       ;;compare to others
       (when matched-str
         (cond
          ((>= length-best-match  match-length)
           NIL)
          (t (setf best-match matched-str
                   best-matched-item item
                   length-best-match match-length
                   best-match-n n))))
       ;;end when match
       )
     ;;end loop
     )
    (values best-match length-best-match best-match-n all-matches 
            best-matched-item all-matched-items)
    ;;end let, find-best-match
    ))
;;TEST
;; problem 2019
;; (find-best-match "Serial" '("Directory" "of" "C:\\3-TS\\LISP" "PROJECTS" "TS\\MYUTILITIES" )) = "S"   1 3 ("S" "S")  "PROJECTS"  ("PROJECTS" "TS\\MYUTILITIES")


;;  (find-best-match "RESET-INPUTS"  '("Input" "X" "Y" "R" "Wup" "Wdn" "Uup" "Udn" "Y-Output" "RESET" "RESET-ININPUTS" "RESET-NOUTPUTS" "RESET-COUNTER" "N-CATS" "TEMP" "TEMP2"))
;; RESULT= "RESET" 5 9 ("R" "RESET" "R") "RESET" ("R" "RESET" "RESET-COUNTER")
;;  (find-best-match "RESET-ININPUTS1"  '("Input" "X" "Y" "R" "Wup" "Wdn" "Uup" "Udn" "Y-Output" "RESET" "RESET-ININPUTS" "RESET-NOUTPUTS" "RESET-COUNTER" "N-CATS" "TEMP" "TEMP2") :reverse-find-p T)
;; WORKS= "RESET-ININPUTS"  14 10 ("R" "RESET" "RESET-ININPUTS") "RESET-ININPUTS" ("R" "RESET" "RESET-ININPUTS")
;; (find-best-match 'xi-2-2 '(reset-points X-2-2-I-points y-points)) 
;; works= "XI-2-2"  6  1 ("XI-2-2") X-2-2-I-points (X-2-2-I-points)
;; (nth-value 4 (find-best-match 'xi-2-2 '(reset-points X-2-2-I-points y-points)))
;; works= X-2-2-I-points



; CONVERT-STRING-TO-OBJECTS
;;
;;ddd
(defun convert-string-to-objects (string &key object-list (start 0) string-list
                                         object-separator-list separate-root-digits-p )
  "In U-tstring.lisp, converts a string to objects. Returns object-list. If separate-root-digits-p, then it removes digits from end of root into separate object (eg. root23 = root  23
  RETURNS (values object-list first-object rest-string string-list) object-list is objects string-list is strings."
  (let 
      ((length-string) 
       (begin-n start)
       (first-object)
       (rest-string)
       (new-string)
       (first-object-list) 
       (word)
       (word-str)
       (number)
       )      
    (when  object-separator-list
      (dolist (sep object-separator-list)
        (setf new-string (my-substitute " " sep string)))
      (setf string new-string)
      ;;here string= "root1 2 3 4 5 6"
      ;;end when
      )   
    (setf  length-string (length string))

    (when (> (length string) 0)
      ;;line > 1

      ;; basic processing to objects is here--process it first here
         (multiple-value-setq (first-object rest-string begin-n)
             (convert-object-in-string string :start start))
         ;;(convert-object-in-string "  0.43") = 0.43
         (when first-object
             (setf object-list (append object-list (list first-object))
                   string-list (append string-list (list (format nil "~A" first-object)))))

         (when separate-root-digits-p
           ;;Divide first-object into (root num) list and substitute for first-object   
           (multiple-value-bind (root-num-list nonnum-list num-list root-str-num-list)
               (convert-object-to-root-num-list first-object)
             ;;(convert-object-to-root-num-list 
             (when root-num-list
               (setf word (car root-num-list)
                     number (car (second root-num-list)))

             (when (member word  '("" " " nil) :test 'equal)
               (setf word nil))
             (when (member number  '("" " " nil) :test 'equal)
               (setf number nil))

             ;;If either is true, remove last object from list, but don't do it twice
             (cond
              (word
               (setf object-list  (butlast object-list)
                     string-list (butlast string-list)))
              (number
               (setf object-list  (butlast object-list)
                     string-list (butlast string-list)))
              (t nil))

             (when (not (member word  '("" " " nil) :test 'equal))
               (setf object-list (append object-list (list word))
                     string-list (append  string-list (list (car root-str-num-list)))))
             (when (not (member number  '("" " " nil) :test 'equal))
               (setf object-list (append object-list (list number))
                     string-list (append string-list (list (format nil "~A" number )))))
               ;;end when, mvb, when separate-root-digits-p
               )))

             ;;RECURSE FOR REST-STRING?
             (when (> (length rest-string) 0)
               (multiple-value-bind (object-list1 first-object1 rest-string1 string-list1)
                   (convert-string-to-objects  rest-string
                                               :object-list object-list :string-list string-list)
               
                 ;;append recursed string-list
                 (setf string-list string-list1
                       object-list object-list1
                       first-object first-object1 
                       rest-string rest-string1)

                 ;;was :start begin-n))
                 ;;(setf outX (append outX (list (format nil "n= ~A first-object= ~A~%  rest-string= ~A~%  object-list= ~A~%  "n first-object rest-string object-list ))))   
                 ;;end mvb,when
                 ))

             ;;end when >0
             )             
    (values object-list first-object rest-string string-list)
    ;;end let,convert-string-to-objects
    ))
;;test  
;;  (convert-string-to-objects "this is a 1 2 3 test")
;;  (THIS IS A 1 2 3 TEST)  TEST  ""
;; (convert-string-to-objects "root1-2-3-4-5-6" :separate-root-digits-p T :object-separator-list '("-"))
;; WORKS= ("ROOT" 1 2 3 4 5 6)   6  "" ("ROOT" "1" "2" "3" "4" "5" "6")
;;  (convert-string-to-objects "this that 2 3 4 then 5 6")
;; WORKS= (THIS THAT 2 3 4 THEN 5 6)  6 "" ("THIS" "THAT" "2" "3" "4" "THEN" "5" "6")
;;
;; (convert-string-to-objects "1.2.3.4" :object-separator-list '( #\.))
;; works= (1 2 3 4)  4  ""   ("1" "2" "3" "4")
;; (convert-string-to-objects "1.2.3.4" :object-separator-list '( "."))
;; works= (1 2 3 4)  4  ""   ("1" "2" "3" "4")
;; (convert-string-to-objects "  0.43  " ) 
;; works=  (0.43)  NIL  ""  ("0.43")


;;CONVERT-OBJECTS-TO-STRINGS
;;2017
;;ddd
(defun convert-objects-to-strings (objects &key (string-head "") (string-tail ""))
  "In U-tstring, RETURNS list of strings.  Adds string-head and string-tail to each string. SAME AS  convert-items-to-strings, almost like make-list-of-strings "
  (let
      ((strings)
       (string "")
       (n-strs 0)
       )
    (loop
     for object in objects
     do
     (when object
       (incf n-strs)
       (setf string (format nil "~A~A~A" string-head object string-tail)
             strings (append strings (list string))))
           
     ;;end loop
     )
    (values strings n-strs)
   ;;end let, convert-objects-to-strings
  ))
;;TEST
;;   (convert-objects-to-strings '( 1 3.22 THIS "this" NIL (list) a b))
;;  works=  ("1" "3.22" "THIS" "this" "(LIST)" "A" "B")   7




#|
(setf *test-out9 (my-delete '("+" "=" ";")
" intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";"))
 (convert-string-to-objects  *test-out9)
|#





;;CONVERT-OBJECT-IN-STRING
;;   
;;ddd
(defun convert-object-in-string (string &key (start 0) object-separator-list return-list-p)
  "In U-tstring.lisp, converts a string to objects. If return-list-p, returns (values (first-object rest-string begin-n) (first-string rest-string begin-n)). When object-separator-list, will divide by any object/string on list. RETURNS (values first-object rest-string begin-n first-string)"
  (let*
      ((first-object)
       (rest-string )
       (begin-n)
       (end-string (- (length string) start 1))
       (first-string "")
       (elt-n)
       ;;had to include this because read-from-string didn't work right--wouldn't search only bounded region of some strings
       (new-string)
       )

    (when (not (= start 0))
      (setf string (subseq string start)))

    (setf new-string (my-substitute "\\(" "\(" string ))

    ;;REPLACE items on object-separator-list with a blank " "
    (when object-separator-list
      (dolist  (sep object-separator-list)
        ;;replace sep with a blank space
        (setf new-string (my-substitute " " (format nil "~A" sep)  string))))

    (unless (> start end-string) 
      ;;sss
      ;;added to get string      
      (when (setf elt-n (search " " new-string )) ;;(search " " "test ") = 4
        (setf first-string (subseq new-string 0 elt-n)))

          (multiple-value-setq (first-object begin-n)
              (read-from-string new-string nil 'eof  :start 0)) ;; :end end-string ))            
      (setf rest-string (subseq new-string begin-n))
      (if (equal first-object 'eof)
          (setf first-object nil))
      ;;end unless
      )
    ;;end convert-object-in-string
    (cond
     ((null return-list-p)
      (values first-object rest-string begin-n first-string))
     (t  (values (list first-object rest-string begin-n) 
                 (list first-string rest-string begin-n))))    
    ))
;;TEST
;; (convert-object-in-string "  7.2  ")
;; (convert-object-in-string  "root1-2-3-4-5-6" :object-separator-list '(-))
;; WORKS= ROOT1  "2 3 4 5 6"  6  "root1"
;;
;; to return lists instead
;; (convert-object-in-string  "root1-2-3-4-5-6" :object-separator-list '(-) :return-list-p T)
;; WORKS =  (ROOT1 "2 3 4 5 6" 6)  ("root1" "2 3 4 5 6" 6)
;;
;; (convert-object-in-string  "    System.out.println(\"questionFramesArray[0] to setVisible(true) \"") ;;:start 22 :return-list-p  t))
;;;; (convert-object-in-string  "(true) \"" :return-list-p t) = ((TRUE) "\"" 7)
;;(char "    System.out.println(\"questionFramesArray[0] to setVisible(true) \""22)
;; works returns (SYSTEM.OUT.PRINTLN "(\"questionFramesArray[0] to setVisible(true) \"" 22)
;;
;;  (convert-object-in-string "this is a test") 
;; works, returns= THIS  "is a test"  5  "this"
;;
;; (convert-object-in-string "this is a test" :return-list-p  t)
;;works, returns=  (THIS "is a test" 5)  ("this" "is a test" 5)
;;
;;(convert-object-in-string "this is a test" :start 5)
;;works, returns=  IS  "a test" 3  "is"

;;(length "(\"questionFramesArray[0] to setVisible(true) ")
;;(read-from-string "(\"questionFramesArray[0] to setVisible(true) \"" nil 'eof :start 0 :end 44)
;;(length "(\"questionFramesArray[0] to setVisible(true) \"") = 46
;;(read-from-string "(\"questionFramesArray[0] to setVisible(true) \"") ;; nil 'eof :start 0 :end 44)
;; (unwind-protect (read-from-string "(\"questionFramesArray[0] to setVisible(true) \"" :start 10 )) = same error
;;(read-from-string "\(\"questionFramesArray[0] to setVisible(true) \")") 

;;;(read-from-string (my-substitute "\\(" "\("  "(\"questionFramesArray[0] to setVisible(true) \""))
;;(read-from-string   "    System.out.println(\"questionFramesArray[0] to setVisible(true) \"" nil 'eof  :start 22) =  Error: End of file while reading stream #<SYSTEM::STRING-INPUT-STREAM 23FCE687>.
;;  (read-from-string   "    System.out.println(\"questionFramesArray[0] to setVisible(true) \"")  = SYSTEM.OUT.PRINTLN 22
;;(read-from-string   "    System.out.println(\"questionFramesArray[0] to setVisible(true) \""  :start 33) = SYSTEM.OUT.PRINTLN 22

;; (read-from-string "    System.out.println (\"questionInstancesArray[0] \" questionInstancesArray[0]);" nil 'eof :start 50 :end 79 ) = Error: End of file while reading stream 
;; (length "    System.out.println (\"questionInstancesArray[0] \" questionInstancesArray[0]);")


;;CONVERT-STRING-W-SEPARATORS-TO-LIST
;;2019
;;ddd
(defun convert-string-w-separators-to-list (string &key (separators '("." "-"))
                                                   delete-chars (return-symbols-p T)
                                                    (string-not-equal (quote ("" ))) 
                                                    (alt-new-symbol (quote *sym-not-found)) 
                                                    (replace-space-char #\-))
  "U-tstring   RETURNS (values string-list symbol-list) list of separated strings and/or symbols.    INPUT: string with separators eg. CS.HS.1 => (\"C\" \"S\" \"1\") or (C S 1).  Can be a single number. "
  (let
      ((string-list)
       (symbol-list)
       (len-string)  ;; (length string))
       (symstr "")
       )
    (cond
     ((numberp string)
      (setf string-list (list (format nil "~A" string))
            symbol-list (list string)))
     ;;string not a number
     (T  
      (setf len-string (length string))
      (loop
       for n from 0 to (- len-string 1)
       do
       (let*
           ((char  (char string n))
            )
         ;;(BREAK)
         (cond
          ((= n (- len-string 1))
           (setf symstr (format nil "~A~A" symstr char)
                 string-list (append string-list (list symstr)))
           (when return-symbols-p
             (setf symbol-list (append symbol-list 
                                       (list (my-make-symbol symstr
                                                             :string-not-equal string-not-equal
                                                             :delete-chars delete-chars
                                                             :alt-new-symbol alt-new-symbol
                                                             :replace-space-char replace-space-char
                                                             ))))))         
          ((member char separators :test 'string-equal)
           (setf string-list (append string-list (list symstr)))     
           (when return-symbols-p
             (setf symbol-list (append symbol-list 
                                       (list (my-make-symbol symstr :delete-chars delete-chars)))))
           (setf symstr ""))
          (t (setf symstr (format nil "~A~A" symstr char))))
         ;;end unless, loop,clause, cond
         ))))
    (values string-list symbol-list)
    ;;end let, convert-string-w-separators-to-list
    ))
;;TEST
;; (convert-string-w-separators-to-list "CS.HS.1.2")
;; works= ("CS" "HS" "1" "2")   (CS HS 1 2)
;; (convert-string-w-separators-to-list 1) = ("1")  (1)



;;CONVERT-STRINGS-LIST-TO-OBJECTS
;;2017
;;ddd
(defun convert-strings-list-to-objects (strings-list &key (start 0) 
                                                     object-separator-list return-lists-p)
  "In U-tstring.lisp, converts a list of strings to a list of objects (or object lists if return-lists-p. If return-list-p, returns (values (first-object rest-string begin-n) (first-string rest-string begin-n)). When object-separator-list, will divide by any object/string on list. RETURNS (values first-object rest-string begin-n first-string)"
;;;;here1
   (let
       ((objects-list)
        (n-strings (list-length strings-list))
        )
     (loop
      for string in strings-list
      do
      (let
          ((object (convert-object-in-string string  :start start 
                                             :object-separator-list object-separator-list))
           )
        (cond
         (return-lists-p
          (setf objects-list (append objects-list (list (list object)))))
         (t  (setf objects-list (append objects-list (list object)))))
        ;;end let, loop
        ))
        (values objects-list n-strings)
        ;;end let, convert-strings-list-to-objects
        ))
;;TEST
;;  (convert-strings-list-to-objects '(" 2.3 ""  3.6  " "  1.88 "))
;; works=  (2.3 3.6 1.88)   3
;;  (convert-strings-list-to-objects '(" 2.3 ""  3.6  " "  1.88 ") :return-lists-p T)
;; works= ((2.3) (3.6) (1.88))    3

 


;;CENTER-TEXT
;;2020 revised to be more accurate
;;ddd
(defun center-text (string-list n-line-pix &key stream (left-margin-pix 0) left-margin-spaces
                                 incl-rest-string-p (add-pre-lines-n 0)
                                 n-line-chars  (default-pix/char 7.0)
                                 (my-font-name '12-nonbold) font-size bold-p font-style  
                                 (string-too-short-lengthen-p t)
                                 (add-post-lines-n 0))
  "U-tstring Centers string between left-margin-pix and end of n-line-pix and SENS TO STREAM.   RETURNS (values line-string begin-string-char-n rest-str-n). Note: margin is part of string length, but string is centered betw margin and end. Uses my-center-string to output to a stream (or not).
  FONT: estimate-string-pix used to center--must have a pre-defined my-font-name (12-nonbold, 14-bold, ???)  OR a font-size, boldp, and font-style (default) with est-pix-char. MY-FONT-NAMES: (14-bold, 14-nonbold, 12-bold, 12-nonbold, 111-bold, 11-nonbold, 10-bold, 10-nonbold)"     
  (let*
      ((centered-line-strings)
       (n-strings 0)
       )
    (loop
     for string in string-list
     do
     (when (stringp string)
       (incf n-strings)
       (multiple-value-bind (line-string begin-string-char-n n-string-pix-est rest-str-n n-line-pix new-line-pix)
           (my-center-string string :left-margin-pix left-margin-pix :n-line-pix n-line-pix
                             :incl-rest-string-p incl-rest-string-p :my-font-name my-font-name
                             :font-size font-size :bold-p bold-p :font-style font-style)
         (when (> add-pre-lines-n 0)
           (setf line-string (format nil "~V%~A" add-pre-lines-n line-string)))
         (when (> add-post-lines-n 0)
           (setf line-string (format nil "~A~V%" line-string add-post-lines-n )))
         (setf centered-line-strings (append centered-line-strings (list line-string)))
         (format stream "~A" line-string)
         ;;end when,mvb,loop
         )))
    (values centered-line-strings n-strings)
    ;;end mvb, center-text
    ))
;;TEST
;; (center-text  '("center this text 1" "center the very next text" "center this") 300)
;; works= ("          center this text 1" "       center the very next text" "              center this")  3

#|"
                            center this text                            
"
28 28|#


#|OLD-DOESN'T WORK RIGHT
(defun center-text (string line-width &key stream)
  "Approx centers text in line of stream with width of line-width"
  (format stream "~V<~;~A~;~;~>" line-width string)
  )|#




;;MY-CENTER-STRING
;;2020
;;ddd
(defun my-center-string (string  &key (left-margin-pix 0) left-margin-spaces
                                 (n-line-pix 880) n-line-chars
                                 incl-rest-string-p   (my-font-name '12-nonbold)
                                  font-size  bold-p  font-style (default-pix/char 7.0)
                                  (string-too-short-lengthen-p T))
  "U-tstring Centers string between left-margin and end of n-line-pix  RETURNS (values line-string begin-string-char-n n-string-pix-est rest-str-n n-line-pix new-line-pix).  
  MY-FONT-NAMES: (14-bold, 14-nonbold, 12-bold, 12-nonbold, 111-bold, 11-nonbold, 10-bold, 10-nonbold)
  Note: All calculations in pixels, when chars/spaces provided, they are CONVERTED TO PIXEL ESTIMATES BASED UPON THE FONT.
   2. margin is part of string length, but string is centered betw margin and end."
  (multiple-value-bind (n-string-pix-est n-str-chars pix/char-est)
      (estimate-string-pixels string :my-font-name my-font-name 
                              :font-size font-size :bold-p bold-p :font-style font-style
                              :default-pix/char default-pix/char)
    ;;If only n-line-chars given, calc n-line-pix and use it
    (when (and (null n-line-pix) n-line-chars)
      (setf n-line-pix (convert-to-integer (* n-line-chars pix/char-est))))
    ;;left-margin-spaces converted
    (when left-margin-spaces
      (setf left-margin-pix (convert-to-integer (* left-margin-spaces pix/char-est))))
    (when (null left-margin-spaces)
      (setf left-margin-spaces (convert-to-integer (/ left-margin-pix pix/char-est))))
    (let*
        ((new-line-pix n-line-pix)
         (line-minus-margin-pix (- n-line-pix left-margin-pix))
         (net-line-pix  (- line-minus-margin-pix n-string-pix-est))
         (line-string "")
         (half-line-plus-half-string-pix (cond
                                          ((> net-line-pix 0)
                                           (convert-to-integer
                                                           (/ (- line-minus-margin-pix n-string-pix-est) 2)))
                                          (string-too-short-lengthen-p 
                                           (setf new-line-pix (+ n-string-pix-est left-margin-pix)))
                                          (T (setf line-string "ERROR: STRING TOO SHORT FOR LINE")
                                             (setf new-line-pix (+ n-string-pix-est left-margin-pix)))))
         (begin-string-pix (+ left-margin-pix half-line-plus-half-string-pix))
         (begin-string-char-n (convert-to-integer (/ begin-string-pix pix/char-est)))
         (fill-string (make-string (+ left-margin-spaces begin-string-char-n)
                                   :initial-element #\space))
         (rest-str-n 0) 
         (rest-string "")
         (line-string) 
         )
     (unless (equal line-string "ERROR: STRING TOO SHORT FOR LINE")
      (setf  line-string  (format nil "~A~A" fill-string string ))
      ;;incl-rest-string-p?
      (when incl-rest-string-p 
        (setf rest-str-n (- begin-string-char-n left-margin-spaces))
        (when (> rest-str-n 0)
              (setf rest-string (make-string rest-str-n  :initial-element #\space)
              line-string (format nil "~A~A" line-string rest-string))))
      ;;end unless
      )
      (values line-string begin-string-char-n n-string-pix-est rest-str-n n-line-pix new-line-pix)
      ;;end let, mvb,my-center-string
      )))
;;TEST
;; for n-line-pix TOO SHORT
;; (my-center-string   "this is a test string" :n-line-pix 100 )
;; works= "                                       this is a test string"  39 147 39 100 161
;; for n-line-pix TOO SHORT
;; (my-center-string   "this is a test string" :left-margin-pix 20 :n-line-pix 100  :incl-rest-string-p T)
;; works= "                            this is a test string                        "  26 161 24 100 181
;; (my-center-string   "this is a test string" :left-margin-pix 20 :n-line-pix 210 )
;; works= "      this is a test string"  4  161  0 210 210 



;;TEST-FONT-PIXELS/CHAR
;;2020
;;ddd
(defun test-font-pixels/char (font-size bold-p 
                                        &key (string1 "abcdefghijklmnopqrstuvwxyz")(string2 "abcdef"))
  "U-tstring (MOVE?)   RETURNS (values pixels/char instr-pane-width-pixels n-chars-instr-pane-text instr-text  *instr-pane-font-size *simple-input-instr-boldp)    INPUT: Change font attributes temporarily to calc pixels per char. STRING = string1 string1 string2)"
  (let*
      ((frame-inst) 
       (orig-font-size *instr-pane-font-size)
       (orig-bold-p *simple-input-instr-boldp)
       (instr-text (format nil "~A ~A ~A" string1 string1 string2))
       (n-chars-instr-pane-text (length instr-text))
       (instr-pane-width-pixels)
       (pixels/char)
       )
    ;;set the instance FONT-SIZE and BOLD
    (setf *instr-pane-font-size font-size
          *simple-input-instr-boldp bold-p)
     ;;make simple-text-input-frame
    (setf frame-inst (make-instance 'simple-text-input-frame))

    ;;SSS FIX GO-FRAME-BUTTON LATER
    (with-slots (column-layout title-pane  instr-pane text-input-pane 
                               quest-pane  go-fr-button ) 
        frame-inst
      (setf (capi:layout-description column-layout)
            (list title-pane  instr-pane quest-pane
                  text-input-pane  go-fr-button)) ;; terminate-button na-none-button ))
    (capi:display frame-inst)
      (capi:apply-in-pane-process instr-pane
                                  #'(setf capi:rich-text-pane-text) 
                                  instr-text instr-pane )
     (setf instr-pane-width-pixels  (capi::pane-width instr-pane)
           pixels/char (float (/ instr-pane-width-pixels n-chars-instr-pane-text)))                            
      ;;end with slots
      )
      ;;reset global vars to original
      (setf *instr-pane-font-size orig-font-size
            *simple-input-instr-boldp orig-bold-p)
      (values pixels/char instr-pane-width-pixels n-chars-instr-pane-text instr-text  *instr-pane-font-size *simple-input-instr-boldp)
      ;;end let, test-font-pixels/char
      ))
;;TEST
;; (setf *instr-pane-font-size 12 *simple-input-instr-boldp nil)
;; (test-font-pixels/char  14 T :string2 "a" )
;; works= 10.363636  570 55 "abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz a" 12 NIL
;; (test-font-pixels/char  14 NIL :string2 "abcdefg" )
;;works= 9.344262  570  61  "abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdefg"  12 NIL
;; (test-font-pixels/char  12 T :string2 "abcdefghijkl" )  ;; works= 8.636364  570 66 "abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdefghijkl" 12 NIL
;; (test-font-pixels/char  12 NIL :string2 "abcdefghijklmnopqrst" )
;;works= 7.7027025  570  74  "abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrst"  12 NIL
;; (test-font-pixels/char  11 T :string2 "abcdefghijklmnopqrstuv" ) 
;; works= 7.5  570   76  "abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuv"  12 NIL
;; (test-font-pixels/char  11 NIL :string2 "abcdefghijklmnopqrstuvwxyz abcdef" ) 
;; 6.551724  570 87 "abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdef" 12 NIL
;; (test-font-pixels/char  10 T :string2 "abcdefghijklmnopqrstuv") 
;; NOTE: same as font-size 11 = 7.5 570 76 "abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuv" 12 NIL
;; (test-font-pixels/char  10 nil :string2 "abcdefghijklmnopqrstuvwxyz abcdef") 
;; works= 6.551724 570  87  "abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz abcdef"  12 NIL ;;same as 11pt




;;FORMAT-STRING-LIST
;;2020 modified to eval objects 
;;ddd
(defun format-string-list (string-list &key stream (line-width 100)  
                                       (add-newlines 0) add-top-lines add-bottom-lines 
                                       add-pre-to-all add-post-to-all
                                       (justify-type :left ) (left-margin-spaces 0) 
                                       (ignore-strings '("" " " "  " "   "))
                                       (remove-pre-spaces-on-first-str-p T)
                                       remove-pre-spaces-on-all-strs-p (eval-bound-symbols-p T))
  "In U-tstring.lisp, INPUT:  list of items (can be string, symbol (if boundp & eval-bound-symbols-p, evals), list, EVALS FORMAT lists, and creates ONE formated string.ADD-NEWLINES adds add-newlines newlines between strings. RETURNS (values all-string-text  n-strings. justify-type :center, :left, :right. Should use LINE-WIDTH if justify. RETURNS the formated string.  If stream, sends string to stream first. LEFT-MARGIN-SPACES adds that many spaces to left of each line."
  (let
      ((all-strings-text "")
       (new-string)
       (new-strings-list)
       (left-margin-str (format nil "~vA" left-margin-spaces #\space)) 
       (n 0)
       )  
    (loop
     for item in string-list
     do
     (let*
         ((len-str 0)
          (string)
          )
       (unless (member item ignore-strings :test 'equal)
         ;;IF EVAL ITEM FIRST
         (cond
          ((stringp item)
           (setf string item))
          ((and (symbolp item) eval-bound-symbols-p (boundp item))
           (setf string (eval item)))
          ((listp item)
           (cond
            ((equal (car item) 'format)
             (setf string (eval item)))
            (t (setf string item))))
          (t (setf string (format nil "~A" item))))
         (when (stringp string)
           (setf len-str (length string)))
         (incf n)
         ;;remove string initial spaces?
         (when (or remove-pre-spaces-on-all-strs-p
                   (and remove-pre-spaces-on-first-str-p (= n 1)))
           (setf string (my-delete-first-spaces string)))
         ;;add-pre-to-all and/or add-post-to-all
         (when add-pre-to-all
           (setf string (format nil "~A~A" add-pre-to-all string)))
         (when add-post-to-all
           (setf string (format nil "~A~A"  string add-post-to-all)))

         ;;1-JUSTIFY EACH LINE or not
         (cond
          ((equal justify-type :center)
           (setf new-string
             (car    (center-text (list string) line-width 
                              :left-margin-pix (* left-margin-spaces 5)
                              :add-post-lines-n  add-newlines))))
          ((equal justify-type :left)
           (cond
            ((and (= n 1)(=  left-margin-spaces 0))
             (setf new-string (format nil "~A~V%"
                                      string add-newlines)))
            (t  (setf new-string (format nil "~V<~A~A~;~;~>~V%" 
                                         line-width left-margin-str string add-newlines))))
           )
          ((equal justify-type :right)
           (setf new-string (format nil "~V<~A~A~>~V%"
                                    line-width left-margin-str string add-newlines)))
          ;; (format nil "~V<~A~>~V%" 50  "this test string" 0)
          (t  (setf new-string (format nil "~A~A" left-margin-str string))))
         ;;(break "after justify")

         ;;add the string to a new-strings-list (justified or not)
         (setf new-strings-list (append new-strings-list (list new-string)))
         ;;end let,unless,loop
         )))
    ;;(break "new-strings-list")
    ;;ADD LINES AT TOP OR BOTTOM
    (if add-top-lines
        (setf new-strings-list
              (append (list (format nil "~V%" add-top-lines  ))
                      new-strings-list)))
    ;;works (format t "~V%~A" 2 "this is a test")
    (if add-bottom-lines
        (setf new-strings-list (append new-strings-list
                                       (list  (format nil "~V%"  add-bottom-lines )))))
    ;;(format nil "~V%"  3 )
    (if stream
        (format stream "~{~A~}" new-strings-list))
    (setf  all-strings-text (format nil "~{~A~}" new-strings-list))
    ;;end format-string-list
    ))
;;TEST
;; (format-string-list  (list "THIS IS THE TITLE")  :add-top-lines 0 :add-newlines 0 :justify-type :center   :line-width  (- *fr-visible-min-width 80)   :left-margin-spaces 0) ;; *title-text-left-margin-spaces)
;; (format-string-list '("THIS IS A TITLE") :line-width 80 :justify-type :center :add-newlines 1)
;; (format-string-list '("THIS IS A TITLE" "second-string  here") :line-width 70 :justify-type :center :add-newlines 1 :add-pre-to-all "Pre:" :add-post-to-all "POST")
;; works=
;;"                                                THIS IS THE TITLE"                    
;; (setf  teststr222  "evaled symbol string")
;;  (format-string-list '("THIS IS TEST"  teststr222 (format nil "~% rest of  ~A" 'test)))
#|works= "THIS IS TEST                                                                      evaled symbol string
 rest of  TEST                                                                                    " |#
;;older
;;  (format-string-list (list "this one" "1 3 5 7" "final one") :line-width 1)
;;  works = "this one1 3 5 7final one" 3
;; remove initial spaces on first
;; (format-string-list (list "  this one" "  1 3 5 7" "   final one") :line-width 1)
;; works= "this one   1 3 5 7    final one"
;; remove-pre-spaces-on-all-strs-p
;; (format-string-list (list "  this one" "  1 3 5 7" "   final one") :line-width 1 :remove-pre-spaces-on-all-strs-p T)
;;works= "this one 1 3 5 7 final one"
;;  (format-string-list (list "this one" "1 3 5 7" "final one") :add-newlines 1)
;; works =
#|"this one
1 3 5 7
final one"
3|#
;; (format-string-list (list "this one" "1 3 5 7" "final one") :add-newlines 1 :left-margin-spaces 10)
;;works =
#|"          this one                                                                                  
          1 3 5 7                                                                                   
          final one                                                                                 
"|#
;; also works with 1,2 etc add-newlines
;;  (format-string-list (list "this one" "1 3 5 7" "final one") :add-newlines 1 :add-top-lines 1 :add-bottom-lines 1)
;;  (format-string-list (list "this one" "1 3 5 7" "final one") :add-newlines 2 :justify-type :left)




;;ESTIMATE-STRING-PIXELS
;;2020
;;ALSO, COMPARE TO SEE HOW WELL FORMAT or PARAGRAPH (in rich-text-pane) CENTERS text.
#|
NOTE: IN MOST FRAMES, USE  :PARAGRAPH-FORMAT INSTEAD in CAPI:RICH-TEXT-PANE
    :character-format  (list ;; :face *instr-pane-font-face   :size  *instr-pane-font-size   :color *instr-pane-font-color  :bold nil :italic nil :underline nil )
    :PARAGRAPH-FORMAT '(:ALIGNMENT :CENTER  ;; :LEFT :RIGHT
                         :start-indent 20  :offset-indent 20  ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs :tab-stops  '(5 10 15 20)  :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman. )
;; STRATEGIES FOR ESTIMATING PIXELES/CHAR
;;STRATEGY 1:  CAPI::ELEMENT-FONT-WIDTH.  One strategy would be to set line widths in NUM-CHARS which fall within a safe number of pixels for the pane. Tho should be close or centering looks funny.
;;NOTE:   (CAPI::ELEMENT-FONT-WIDTH ELEMENT) should work AFTER creating a pane.  Could create the pane, measure with this, then use that fixed number to help with estimates made BEFORE the pane creation. (Since reuse same panes many times).
;;STRATEGY 2: CREATE PANE of known pixel width and count max chars using alphabet, etc.  Used this strategy below with MAKE-QUESTION-FRAME 14 bold & 12 regular default font.
  q-title was "LINKS TO OTHER NODES" in font size 14, bold, def style.
  *temp-instr=  "abcdefghijklmnopqrstuvwxyz" in font size 12, not bold, def style.
  *temp-instr2=  "abcdefgh"   
  USED MAKE-QUESTION-FRAME :SINGLE-TEXT below to run test.
  Inside frame (text area) width was 880 pixels.
  REPLACE make-text-input-frame with following:
        (make-text-input-frame (format nil "~A ~A ~A ~A" q-title q-title q-title q-title)
                               (format nil "~A ~A ~A ~A ~A ~A" *temp-instr *temp-instr *temp-instr *temp-instr  *temp-instr2 *temp-instr2) 
                               quest-text-formated text-input-box1-instrs   text-input-box2-instrs 'FRAME-CSQ    :num-input-boxes num-input-boxes   :pane1-title csq-frame-pane1-title    :pane2-title csq-frame-pane2-title )
;;TO CALCULATE PIXELS PER CHAR:
;;1. FOR  TITLE PANE: q-title=20chars+space x 63 + (20 - 5) = 78 TITLE PANE CHARS font 14 bold, default style 78  PER 880 PIXELS = 
    (float 880/78) = 11.282051 PIXELS/CHAR
;;2. FOR INSTR PANE: *temp-instr= 26+space X 4= 108 + (9 x 2)= 18 = 126 INSTR PANE CHARS font 12 NOT-bold, default style PER 880 PIXELS = 
    (float 880/126) = 6.984127 PIXELS/CHAR|#
;;
;;ESTIMATE-STRING-PIXELS (see above strategy)
;;2020
;;ddd
(defun estimate-string-pixels (string &key  (font-size 12) bold-p (font-style 'default)
                                      (my-font-name '12-nonbold) (default-pix/char 7.0) )
  "U-tstring. Can't find exact pixels/font-char until AFTER display using CAPI::ELEMENT-FONT-WIDTH;  therefore use this to estimate pixels/font-char BEFORE creating strings (esp :center justification).  RETURNS: (values  n-string-pixels-est n-chars pixels/char-est)   INPUT: So far only have values for 14, bold, default and 12,null bold, default  (used in make-question-frame for title and instructions/questions/etc.   Defined FONT NAMES= (14-bold, 14-nonbold, 12-bold, 12-nonbold, 111-bold, 11-nonbold, 10-bold, 10-nonbold)"
  (let*
      ((pixels/char-est)
       (n-chars (length string))
       (n-string-pixels-est)
       )
    ;;FIND PIXELS/CHAR
    (cond
     ;;Title 14, bold, default
     ((or (equal my-font-name '14-bold) 
          (and (equal font-size 14) bold-p (equal font-style 'default)))
            (setf pixels/char-est 11.282051)) ;;10.363636 for simple-input-frame instr-pane
     ((or (equal my-font-name '14-nonbold) 
          (and (equal font-size 14) (null bold-p) (equal font-style 'default)))
            (setf pixels/char-est  9.344262 ))
     ((or (equal my-font-name '12-bold) 
          (and (equal font-size 12) bold-p (equal font-style 'default)))
            (setf pixels/char-est 6.984127))
     ((or (equal my-font-name '12-nonbold) 
          (and (equal font-size 12) (null bold-p) (equal font-style 'default)))
      (setf pixels/char-est 7.7027025 ))
     ((or (equal my-font-name '11-bold) 
          (and (equal font-size 11) bold-p (equal font-style 'default)))
            (setf pixels/char-est 7.5))
     ((or (equal my-font-name '11-nonbold) 
          (and (equal font-size 11) (null bold-p) (equal font-style 'default)))
            (setf pixels/char-est 6.551724))
     ((or (equal my-font-name '10-bold) 
          (and (equal font-size 10) bold-p (equal font-style 'default)))
            (setf pixels/char-est 7.5))
     ((or (equal my-font-name '10-nonbold) 
          (and (equal font-size 10) (null bold-p) (equal font-style 'default)))
            (setf pixels/char-est 6.551724)) ;;yes, same as 11 pt
    (t nil))
    ;;FIND N-STRING-PIXELS-EST
    (cond
     (pixels/char-est
      (setf n-string-pixels-est  (convert-to-integer (* n-chars pixels/char-est))))
     (default-pix/char
      (setf n-string-pixels-est  (convert-to-integer (* n-chars default-pix/char))))
      (t nil))

    (values  n-string-pixels-est n-chars pixels/char-est)
    ;;end let, estimate-string-pixels
    ))
;;TEST
;; (estimate-string-pixels "this is a test string")
;; works= 146  21  6.984127 for make-question-frame
;;  161 21 7.7027025 for simple-input-text-frame



;;CHECK-STRING-NUMBER-RANGE
;;
;;ddd
(defun check-string-number-range (num-string lo-num hi-num)
  "In U-Tstring.lisp, returns (values T or NIL if num is >= lo-num and <= hi-num AND num or the original num-string or nil if not a number."
  (let
      ((length-num) ;; )
       (num)
       (result)
       )
    ;;if its not blank or a number, make it a number
    (cond
     ((stringp num-string)
      (setf length-num (length num-string))
     (if (> length-num 0)
         (setf num (my-make-symbol num-string)))
     )
     ((numberp num-string)
      (setf num num-string))
     (t  nil))
    ;;check on its value    
    (cond
     ((and (numberp num) (>= num lo-num) 
           (<= num hi-num))
      (setf result T))
     (t nil))
    (values result num)
    ))
;;TEST
;; (check-string-number-range "44" 30 99) = T 44
;; (check-string-number-range "22" 30 99)  = NIL 44
;; (check-string-number-range "" 30 99)   = NIL ''"
;; (check-string-number-range  45 30 99)
;; (check-string-number-range  11 30 99)


;;
;;ddd
(defun greatest-string-length (list-of-strings)
  "In U-tstring, returns VALUES 1-length of longest string and the 2-longest string and 3-position in list--starting with 0."
  (let
      ((return-string)
       (return-length 0)
       (return-n 0)
       (n -1)
       (str-length)
       )
    (loop
     for string in list-of-strings
     do
     (incf n)
       (setf str-length (length string))
       (cond
        ((> str-length return-length)
         (setf return-string string
               return-length str-length
               return-n n))         
        (t nil))
       ;;end loop
       )
    (values return-length return-string return-n)
    ;;end let, greatest-string-length
    ))
;;TEST, works
;;   (greatest-string-length '("ABC" "cde" "lmnop" "np")) = 5  "lmnop"  2
;;  (greatest-string-length '("lmnop" "ABC" "cde" "lmnop" "np")) = 5 "lmnop" 0
;;  (greatest-string-length '("lmnop" "ABC" "cde" "lmnop" "np" "1234567"))  = 7 "1234567"  5           


;;CONVERT-STRING-TO-CHARS
;;
;;ddd
(defun convert-string-to-char-strings (string &key list-words-p omit-separators-p
                                              (separators '(#\Space #\, #\. #\; #\- #\\(  #\\) 
                                                                    #\  #\   #\  #\   #\_  #\?   #\!  #\:   #\"  #\'  ))
                                              (return-char-strings-p T))
  "In t-string,  Works for any sequence. RETURNS (values  charstr-list charlist word-list). If list-words-p, puts words divided by separators into lists with the separator included between each list (unless omit-separators-p)."
  (let
      ((char)
       (char-str)
       (charlist)
       (charstr-list)
       (len-str) 
       (cur-word)
       (word-list)
       )
  (when (stringp string)
    (setf  len-str (length string))
    (loop
     for nth from 0 to (- len-str 1)
     do
     (setf char (elt string nth)
           char-str (format nil "~A" char))
     (setf charlist (append charlist (list char))
           charstr-list (append charstr-list (list char-str)))

     ;;PUT INTO LISTS?
     (when list-words-p
       (cond
        ((member char separators)
         (when (null omit-separators-p)
           (cond
            (return-char-strings-p
             (setf word-list (append word-list (list (list char-str)))))
            (t 
             (setf word-list (append word-list (list (list char))))))
           ;;end when char separator
           )
         (when cur-word
           (setf word-list (append word-list (list cur-word))
                 cur-word nil)))
        ((null cur-word)
         (cond
          (return-char-strings-p
           (setf cur-word (list char-str)))
          (t (setf cur-word (list char )))))
        (t 
         (setf cur-word (append cur-word (list char-str))))
        ;;end cond, when
        ))
     ;;end loop, when
     ))
  (values  charstr-list charlist word-list)
  ;;end let, convert-string-to-char-strings
  ))

;;TEST
;; (convert-string-to-char-strings "This is a Test 1 2 3 &" :list-words-p T)
;; works = ("T" "h" "i" "s" " " "i" "s" " " "a" " " "T" "e" "s" "t" " " "1" " " "2" " " "3" " " "&")   (#\T #\h #\i #\s #\Space #\i #\s #\Space #\a #\Space #\T #\e #\s #\t #\Space #\1 #\Space #\2 #\Space #\3 #\Space #\&)   ((" ") ("T" "h" "i" "s") (" ") ("i" "s") (" ") ("a") (" ") ("T" "e" "s" "t") (" ") ("1") (" ") ("2") (" ") ("3"))   
;;  (convert-string-to-char-strings "This is a Test 1 2 3 &")
;; works=   ("T" "h" "i" "s" " " "i" "s" " " "a" " " "T" "e" "s" "t" " " "1" " " "2" " " "3" " " "&")     (#\T #\h #\i #\s #\Space #\i #\s #\Space #\a #\Space #\T #\e #\s #\t #\Space #\1 #\Space #\2 #\Space #\3 #\Space #\&)
;;  (convert-string-to-char-strings "This is a Test 1 2 3 &" :list-words-p T)
;; works=
;;  (convert-string-to-char-strings "This is a Test 1 2 3 &" :list-words-p T :omit-separators-p T)
;;works=("T" "h" "i" "s" " " "i" "s" " " "a" " " "T" "e" "s" "t" " " "1" " " "2" " " "3" " " "&")  (#\T #\h #\i #\s #\Space #\i #\s #\Space #\a #\Space #\T #\e #\s #\t #\Space #\1 #\Space #\2 #\Space #\3 #\Space #\&)   ((" ") ("T" "h" "i" "s") (" ") ("i" "s") (" ") ("a") (" ") ("T" "e" "s" "t") (" ") ("1") (" ") ("2") (" ") ("3"))
;;  (convert-string-to-char-strings "This is a Test 1 2 3 &" :list-words-p T :omit-separators-p T)
;; works= ("T" "h" "i" "s" " " "i" "s" " " "a" " " "T" "e" "s" "t" " " "1" " " "2" " " "3" " " "&")  (#\T #\h #\i #\s #\Space #\i #\s #\Space #\a #\Space #\T #\e #\s #\t #\Space #\1 #\Space #\2 #\Space #\3 #\Space #\&)  (("T" "h" "i" "s") ("i" "s") ("a") ("T" "e" "s" "t") ("1") ("2") ("3"))

;;works= (#\T #\h #\i #\s #\Space #\i #\s #\Space #\a #\Space #\T #\e #\s #\t #\Space #\1 #\Space #\2 #\Space #\3 #\Space #\&)   (("T" "h" "i" "s") ("i" "s") ("a") ("T" "e" "s" "t") ("1") ("2") ("3"))
;;  (convert-string-to-char-strings "This is a Test 1 2 3 &" :return-char-strings-p NIL)
;;works= ("T" "h" "i" "s" " " "i" "s" " " "a" " " "T" "e" "s" "t" " " "1" " " "2" " " "3" " " "&")  #\T #\h #\i #\s #\Space #\i #\s #\Space #\a #\Space #\T #\e #\s #\t #\Space #\1 #\Space #\2 #\Space #\3 #\Space #\&)   NIL 



;;COMBINE-LISTS-INTO-STRING
;;
;;ddd
(defun combine-lists-into-sequence (list1 list2 &key insert-str insert-n-list)
  "In U-tstring, combines elements of list1 with list2 into a string by alternating elements from list1 and list2. RETURNS (values new-string  new-list rest-list1 rest-list2) (of alternating elements).Limited by length of shortest list. If insert-str-n-list eg. (\"To\" 3) inserts \"To\" after member of insert-n-list."
  (let
      ((elem2)
       (new-list)
       (rest-list1)
       (rest-list2)
       (new-string "")
       (length-list1 (list-length list1))
       (length-list2 (list-length list2))
       )
    (loop
     for elem1 in list1
     for n from 0 to (- length-list1 1)
     do
     (cond
      ((< n  (list-length list2))
       (setf elem2 (nth n list2)))
      (t (setf elem2 "")))

       (cond
        ((and insert-n-list (member n insert-n-list))
         (setf new-string (format nil "~A~A~A" new-string elem1 insert-str)
          new-list (append new-list (list elem1 elem2))
          rest-list1 (nthcdr (+ n 1) list1)
          rest-list2 (nthcdr (+ n 1) list2))
         )
        (t 
         (setf
          new-string (format nil "~A~A~A" new-string elem1 elem2)
          new-list (append new-list (list elem1 elem2))
          rest-list1 (nthcdr (+ n 1) list1)
          rest-list2 (nthcdr (+ n 1) list2))))
     ;;end loop
     )

    (values new-string  new-list rest-list1 rest-list2)
    ;;end let, combine-lists-into-sequence
    ))
;;TEST
;;  (combine-lists-into-sequence '(1 2 3 4 5 6) '(- - To - -) )
;;
;;  (combine-lists-into-sequence '(A b c D E) '(1 2 3 4)) = "A1B2C3D4" (A 1 B 2 C 3 D 4) (E)  NIL
;; (combine-lists-into-sequence '(A b c D E) '(1 2 3 4 5 6 7)) = "A1B2C3D4E5" (A 1 B 2 C 3 D 4 E 5) NIL (6 7)
;;   '(1 3 1 5 1 2) 
;;  (combine-lists-into-sequence '(1 2 3 4 5 6) '(a b c d e f) :insert-str "To" :insert-n-list '(2 4)) 
;; works??= "1A2B3To4D5To6F"  (1 A 2 B 3 C 4 D 5 E 6 F)
;;
;;  (combine-lists-into-sequence '(1 2 3 4 5 6) '(- - To - -) ) ;;:insert-str "To" :insert-n-list '(2 4))

;;  (combine-lists-into-sequence '(1 2 3 4 5 6) '(- - To - -) )
;; works=  "1-2-3TO4-5-6"  (1 - 2 - 3 TO 4 - 5 - 6 "")  NIL NIL









;;CONVERT-OBJECT-TO-ROOT-NUM-LIST
;;
;;ddd
(defun convert-object-to-root-num-list (object &key (start-n 0) (separator-chars '(#\-  #\=)) separator-str (convert-to-string-nums-p T) (nums-in-list-p T))
  " Finds and returns a list of all numbers in a string from start-n. RETURNS (values root-num-list  nonnum-list num-list root-str-num-list begin-str str-loc-n). OMITS separator-chars. convert-to-string-nums-p causes the chars (incl begin-str) to be converted to one string and numbers before adding to the lists. object root MUST NOT have embedded numbers after start-n. nums-in-list-p returns (root-str num-list).  If alpha chars after root, they are put third in the list. str-loc-n is the number of numbers before the separator-str if it is midway."
  (let
      ((string)
       (new-string)
       (char)
       (num)
       (num-list)
       (nonnum-str "")
       (root-num-list)
       (root-str-num-list)
       (root-sym)
       (length-newstr) 
       (begin-str "")
       (root)
       ;;new
       (root-str "")
       (rest-str "")
       (root-list)
        ;;(rest-str-list)
       (add-to-root-p)
       (fullnum-str "")
       (newnum-p T)  
       ;;WHERE IS THIS IN BREAK VAR LIST??
       (str-loc-n)
       (match-n-list)
       )
    (cond
     ((not (stringp object))
      (setf string (format nil "~A" object)))
     (t (setf string object)))

    (when separator-str
      ;;was (setf str-loc-n (search separator-str string :test 'string-equal))
      (multiple-value-setq (string match-n-list)
             (my-substitute  "-" separator-str string  :from-end-p T :count 1)))
     ;;(setf str-loc-n (length num-list)))
   
    (cond
     ((> start-n 0)
      (setf begin-str (subseq string 0 start-n)
            new-string (subseq string start-n)))
     (t (setf new-string string)))
     
    (setf length-newstr (length new-string)
          add-to-root-p T)
           
    (dotimes (n  (+ length-newstr 1))
      ;;dotimes an extra cycle to add final number to list
      (unless (= n length-newstr)
        (setf char (char new-string n)))
      (cond
       ;;if end of a multi-digit number
       ((or (member char separator-chars) (char-equal char #\space) (= n length-newstr))
        (when (> (length fullnum-str) 0)
          (setf num-list (append num-list (list (my-make-symbol fullnum-str)))
                fullnum-str "")))
       ;;if char is a digit
       ((setf num (digit-char-p char)) ;;returns a num if char = a digit-char
        (setf  add-to-root-p nil)
        (cond
         ;;if the number is multi-digit num
         (convert-to-string-nums-p
          (setf fullnum-str (format nil "~A~A" fullnum-str num))

          ;;add to previous nums and put in list?
          (when (or (member char separator-chars) (char-equal char #\space) 
                    (= n length-newstr))
            (when (> (length fullnum-str) 0)
              (setf num-list (append num-list (list (my-make-symbol fullnum-str)))
                    fullnum-str ""))
            ;;end whens, convert-to-string-nums-p clause
            ))
         (t   (setf num-list (append num-list (list num)))))
        ;;end digit-char-p clause
        )
       ;; separator-chars may be used above for separators, but not included in list
       ((member char separator-chars)  NIL )
       (add-to-root-p
        (setf root-str (format nil "~A~A" root-str char)
                root-list (append root-list (list char))))
       (t (setf rest-str (format nil "~A~A" rest-str char))))
      ;;(afout 'out (format nil "n= ~A char= ~A num-list= ~A fullnum-str= ~A"n char num-list  fullnum-str))
      ;;end dotimes
      )
    (cond
     (convert-to-string-nums-p
      (setf root-str  (format nil "~A~A" begin-str root-str)
            root-sym (my-make-symbol root-str))
      (cond
       (nums-in-list-p 
        (setf  root-num-list (append (list root-sym) (list num-list))
               root-str-num-list (append (list root-str) (list num-list))))
       (t  (setf  root-num-list (append (list root-sym)  num-list)
                  root-str-num-list  (append (list root-str) num-list)))))
     (t  (setf root-num-list 
               (append root-num-list (list root-str num-list begin-str )))))

#|    (setf  root-num-list (append root-num-list (list rest-str))
           root-str-num-list (append root-str-num-list (list rest-str)))|#
    (when separator-str
    (setf str-loc-n (floor (/ (length num-list) 2))))

    (values root-num-list  root-str num-list root-str-num-list begin-str rest-str str-loc-n)
    ;;end let, find-nums-in-string
    ))
;;TEST  (start-n 0) (separator-chars '(#\-  #\=)) (convert-to-string-nums-p T))
;;
;;  (convert-object-to-root-num-list "this1-2-3To4-5-6" :separator-str "to")
;; works (THIS (1 2 3 4 5 6))  "this"  (1 2 3 4 5 6)  ("this" (1 2 3 4 5 6))  ""  "To"
;;  (convert-object-to-root-num-list 'this1-2-3To4-5-6 :separator-str "to")
;; works = (THIS (1 2 3 4 5 6))   "THIS"  (1 2 3 4 5 6)  ("THIS" (1 2 3 4 5 6))  ""  "TO"
;; (convert-object-to-root-num-list "this1-2-3-4")
;;works= (THIS (1 2 3 4)) (#\t #\h #\i #\s) (1 2 3 4) ("this" (1 2 3 4))
""
;; (convert-object-to-root-num-list "this1-2-3-4" :convert-to-string-nums-p NIL)
;; result= (#\t #\h #\i #\s 1 2 3 4 . "")  (#\t #\h #\i #\s)  (1 2 3 4) NIL "" ""

;; (convert-object-to-root-num-list "this1-2-3-4" :nums-in-list-p NIL)
;;  result= (THIS 1 2 3 4)  (#\t #\h #\i #\s)  (1 2 3 4)  ("this" 1 2 3 4)  ""
;;
;; (convert-object-to-root-num-list "this1-2-3To4-5-6" :nums-in-list-p NIL)


;;FROM ART
;; (convert-object-to-root-num-list 'root1-2-33-444)
;; works= ("ROOT" (1 2 33 444))  (#\R #\O #\O #\T)  (1 2 33 444)  ""
;;
;; (convert-object-to-root-num-list 'rooted12-333-2-444-77)
;; works= ("ROOTED" (12 333 2 444 77))   (#\R #\O #\O #\T #\E #\D)   (12 333 2 444 77)   ""
;; (convert-object-to-root-num-list  'Zup33-66-2-3-51-51-9-9)
;; works= ("ZUP" (33 66 2 3 51 51 9 9))  (#\Z #\U #\P)  (33 66 2 3 51 51 9 9) ""
;;
;; (convert-object-to-root-num-list "thisone1-2-3-4" :start-n 2)
;; works= ("thisone" (1 2 3 4))  (#\i #\s #\o #\n #\e)  (1 2 3 4)  "th"
;;
;;  (convert-object-to-root-num-list "thisone1-2-3-4" :start-n 2 :separator-chars nil) 
;;works= ("thisone---" (1234))  (#\i #\s #\o #\n #\e #\- #\- #\-)  (1 2 3 4)  "th"
;;
;; (convert-object-to-root-num-list "thisone1-2-3-4" :start-n 2 :separator-chars nil :convert-to-string-nums-p nil)
;;works=  (#\i #\s #\o #\n #\e #\- #\- #\- 1 2 3 4 . "th")  (#\i #\s #\o #\n #\e #\- #\- #\-)  (1 2 3 4)  "th"





;;MY-DO-STRINGS
;; Just a sham function to call make me aware of and/or use the built-in function below
;;ddd
#|(defmacro my-do-strings (string-var value-var table &optional result &body forms)
  "JUST USE LW BUILT-IN FUNCTION DO-STRINGS INSTEAD = Do-Strings (String-Var Value-Var Table [Result]) {declaration}* {form}*
  Iterate over the strings in a String Table.  String-Var and Value-Var
  are bound to the string and value respectively of each successive entry
  in the string-table Table in alphabetical order.  If supplied, Result is
  a form to evaluate to get the return value."

  (editor:do-strings (list string-var value-var table result) forms)
  )|#

;;; DO-STRINGS  --  PUBLIC  (from src/editor  user-macros.lisp)
;;; LW BUILT-IN FUNCTION, DO NO RE-EVALUATE UNLESS CHANGE NAME
;;; Iterate over the entries in a string table using the NEXT link.
;;;
#|(defmacro do-strings ((string-var value-var table &optional result) &body forms)
  "Do-Strings (String-Var Value-Var Table [Result]) {declaration}* {form}*
  Iterate over the strings in a String Table.  String-Var and Value-Var
  are bound to the string and value respectively of each successive entry
  in the string-table Table in alphabetical order.  If supplied, Result is
  a form to evaluate to get the return value."
  (let ((tab (gensym))
	(current (gensym))
	(i (gensym))
	(vec (gensym)))
    `(let* ((,tab ,table)
	    (,vec (string-table-table ,tab)))
       (dotimes (,i (string-table-num-entries ,tab) ,result)
	 (declare (fixnum ,i))
	 (let* ((,current (svref ,vec ,i))
		(,string-var (string-table-entry-proper ,current))
		(,value-var (string-table-entry-value ,current)))
	   (declare (simple-string ,string-var))
	   ,@forms)))))|#





;;******************************* WERE IN U-sequences.lisp ****************************
;;
;;
;;
(in-package "CL-USER")




;;COERCE object result-type => result
;;Examples:
#| 
;;list to vector,string,
(coerce '(a b c) 'vector) =>  #(A B C)
(coerce '(a b c) 'string) => error
(string #\A) = "A"
;;symbol to char
 (coerce 'a 'character) =>  #\A
;;numbers
 (coerce 4.56 'complex) =>  #C(4.56 0.0)
 (coerce 4.5s0 'complex) =>  #C(4.5s0 0.0s0)
 (coerce 7/2 'complex) =>  7/2
 (coerce 0 'short-float) =>  0.0s0
 (coerce 3.5L0 'float) =>  3.5L0
 (coerce 7/2 'float) =>  3.5
;; cons
 (coerce (cons 1 2) t) =>  (1 . 2)
|#



;;MY-COERCE-LIST-TO-STRING
;;
;;ddd
(defun my-coerce-list-to-string (list &key include-parens-p)
  "In U-sequences.lisp, returns list as string with or without parens via key."
  (let
      ((item)
       (new-char)
       (new-string "")
       (list-length (list-length list))
       )
    ;;include the first parens in new-string?
    (if include-parens-p
        (setf new-string "("))

    ;;coerce each element/char into a string and add it to new-string
    (dotimes (n list-length)
      ;;note- dotimes... for each integer from 0 up to but not including the value of count-form,
      (setf item (nth n list)
            new-char (string (coerce item 'character ))
            new-string (format nil "~A~A" new-string new-char))
      )
    ;;include the last parens in new-string?
    (if include-parens-p
        (setf new-string (format nil "~A)" new-string)))
    new-string))
;;test, works   returns "(ABC)" if  include-parens-p T or "ABC" if  include-parens-p NIL
;;
#|(defun testls ()
  (let
      ((list '(a b c))
       (include-parens-p nil)
       )
  (my-coerce-list-to-string list :include-parens-p include-parens-p)))|#


;;MATCH-SUBSEQ
;;   For string to match all chars in an item to a substring in a string, 
;;           USE MATCH-SUBSTRING (In U-tstring.lisp)
;;
;;ddd
(defun match-subseq (item seq &key end-item trim-items
                          trim-end-items match-all-from-end )
  "In U-sequences.lisp, returns a sub-sequence. end-item specifies end-item in sequence (nil or chars/string; trim-items (integer) indicates to trim from beginning. trim-end-items (integer) indicates to trim from end; match-all-from-end (boolean) indicates that both beginning and end items are matched starting from the seq end. Set item to 0 or nil to begin at seq beginning.NOTE: MATCH-SUBSTRING WORKS BETTER FOR STRINGS??"
;;  (afout 'out (format nil ">>>> ARGS~% item=~A~% seq=~A~%  end-item=~A~% trim-items=~A~% trim-end-items=~A~% match-all-from-end=~A~%"item seq  end-item trim-items trim-end-items match-all-from-end))
  (let ((matched-item-elt) 
        (matched-end-item-elt) 
        (subseq seq)
        )
    ;;Can set item to 0 or nil to begin at beginning
    (when (or (null item)(equal item 0))
      (setf matched-item-elt 0))
    ;; (when (or (null end-item)(equal end-item 0))
    ;;  (setf matched-end-item-elt (leng))

    ;;if NOT match-all-from-end
    (cond
     ((null match-all-from-end)
      (if (null matched-item-elt)
          (setf matched-item-elt (position item seq)))
      (setf subseq (subseq seq matched-item-elt))
      (when end-item
        (setf matched-end-item-elt (position end-item subseq))
        (setf subseq (subseq subseq 0 matched-end-item-elt)))
      ;;end first cond clause
      )
     ;;if match-all-from-end
     (match-all-from-end
      (cond
       ((null item)
        (setf matched-item-elt 0))
       (t (setf matched-item-elt (position item seq :from-end t))))
    ;;(afout 'out (format nil "==> PRE BEGIN ANEW **~% subseq= ~A~%matched-item-elt= ~A~%  matched-end-item-elt= ~A" subseq matched-item-elt matched-end-item-elt))
       (if matched-item-elt
          (setf subseq (subseq  seq matched-item-elt))) ;;otherwise subseq = seq
      (when end-item
        (setf matched-end-item-elt (position end-item subseq :from-end t))
        (if matched-end-item-elt
            (setf subseq (subseq subseq 0 matched-end-item-elt))))
      ;;end second cond clause
      )      
     (t nil))
    ;;(afout 'out (format nil "==> BEGIN ANEW **~% subseq= ~A~% matched-item-elt= ~A~%  matched-end-item-elt= ~A" subseq matched-item-elt matched-end-item-elt))

    ;;find the original subseq from matched item (whether matched from beginning or from end)
    ;;  (if  matched-item-elt  (setf subseq (subseq seq matched-item-elt)))
    ;;(afout 'out (format nil "3 matched-item-elt= ~A~% subseq= ~A~%" matched-item-elt subseq))
    ;;(fout out)

    ;; trim-items trim-end-items
    (when (and trim-items (<= trim-items (length subseq)))
      (setf subseq (subseq subseq  trim-items))) 
    (when (and trim-end-items (<= trim-end-items (length subseq)))
      (setf subseq (subseq subseq 0 (- (length subseq) trim-end-items))))     

    ;;(afout 'out (format nil "4 subseq= ~A~%" subseq))

    ;; in case no matches were found, must reset subseq to nil
    (if (equal subseq seq)
        (setf subseq nil))
    ;;(afout 'out (format nil "5 subseq= ~A~%" subseq))
    ;; (fout out)
    (values subseq matched-item-elt matched-end-item-elt)
    ))
;;test
;;All tests appear to work
;;
#|
(defun itest ()
(setf out nil)
;;OK(match-subseq '#\9 "2012:11:19 19:15:06") ;; = "9 19:15:06" 9 NIL
;;(setf out nil)
;;OK(match-subseq '#\9 "2012:11:19 19:15:06" :end-item '#\0 ) ;;= "9 19:15:" 9 8
;;(setf out nil)
;;OK(match-subseq '#\9 "2012:11:19 19:15:06" :trim-items 1) ;;= " 19:15:06" 9 nil
;;(setf out nil)
;;OK (match-subseq 0 "2012:11:19 19:15:06":end-item '#\space :trim-end-items 1)  ;;  = "2012:11:1" 
;;(setf out nil)
;;OK (match-subseq '#\space "19:15:06":end-item '#\: :trim-end-items 2 :match-all-from-end t) ;; =  "19:"
;;(setf out nil)
;;OK (match-subseq '#\space "19:15:06":end-item '#\:  :match-all-from-end t) ;;= "19:15" (note: eliminates seconds)
(setf out nil)
;;OK (match-subseq  '#\: "19:15:06" :match-all-from-end  t )  ;; = ":06"
(setf out nil)
;;OK (match-subseq  nil "19:15:06" :end-item '#\:  :match-all-from-end t) ;;= ":06" 5 NIL  (returns time w/o seconds)
;;(setf out nil)
;;OK (match-subseq  nil "19:15:06":end-item '#\:) = "19:15" 0 5
)
;; (match-subseq     item seq &key  end-item trim-items trim-end-items match-all-from-end )
;;  (match-subseq 
;; (position  "PCategory"   "PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);")
|#














;;MAKE-CONDENSED-SYM
;;
;;added 2016-04
;;ddd
(defun make-condensed-sym (string &key (min-length 3) (max-length 12)
                                     (extra-chars "xxxxxxxxxxxx") (if-exists-change-p T)
                                     (if-null-string-abort-p T)
                                     (set-to-value-p T)(set-to-value nil) (word-separator "")
                                     cond-words-p (min-word-len 3)         
                                     (phrase-min 8) (phr-word-max 5))
  "In U-Tstring,  makes a shorter string attempting based upon  main word or words in string. When word-separator, makes it a separator betw words.
  When cond-words-p, condenses each word using min-word-len & phr-word-max.
  When min-word-len, omits words too short.
  RETURNS: (values newsym newstr similar-sym set-to-value)  similar-sym is the first sym it tried to use."
  (let
      ((newsym)
       (newstr)
       (similar-sym)
       )
    (setf newstr (make-condensed-string string :min-length min-length
                                        :word-separator word-separator
                                        :max-length max-length :extra-chars extra-chars
                                        :cond-words-p cond-words-p :min-word-len min-word-len
                                        :if-nil-return-nil-p if-null-string-abort-p
                                        :phrase-min phrase-min :phr-word-max phr-word-max))       
    (unless (and (null newstr) if-null-string-abort-p)
    ;;MAKE NEW SYMBOL
    (setf newsym (my-make-symbol newstr))
    ;;if exists, change it
    (when (boundp newsym)
      (setf similar-sym newsym)
      (setf newstr (format nil "~A~A" newstr (random 9))
            newsym (my-make-symbol newstr))
      ;;end when
      )
    ;;SET TO SET-TO-VALUE ?
    (when (and set-to-value-p 
               (null (boundp newsym)))
      (set newsym set-to-value))
    (values newsym newstr similar-sym set-to-value)
    ;;end when, let, make-condensed-sym
    )))
;;TEST
;;  (make-condensed-sym "very funny")
;; (make-condensed-sym "a little old")
;; works=   LITTLEOLD  "littleold" NIL
;;if done again
;;(make-condensed-sym "a little old") = LITTLEOLD4   "littleold4"  LITTLEOLD  ;;; (make-condensed-sym  "mother")
;; (make-condensed-sym  "most independent")
;;testing previous symvals
;; (setf VERYFUNNY '(this list)) = (THIS LIST)
;;CL-USER 139 > (make-condensed-sym "very funny") = VERYFUNNY8 "  veryfunny8"  VERYFUNNY  NIL
;;CL-USER 140 > VERYFUNNY (THIS LIST)
;;end test
;; ;;  (make-condensed-sym "smarter than my mother")
;; works= SMARTERTHANMOTHER  "smarterthanmother" NIL NIL
;; ALSO: SMARTERTHANMOTHER = NIL

;;  (make-condensed-sym "")    :if-null-string-abort-p T) = NIL
;;  (make-condensed-sym NIL)    :if-null-string-abort-p T) = NIL
;;  (make-condensed-sym "" :if-null-string-abort-p NIL) 
;; works = XXX  "xxx" NIL NIL
;; ALSO  XXX= NIL
;;  (make-condensed-sym NIL :if-null-string-abort-p NIL) = NIL
;; works= XXX8  "xxx8"  XXX  NIL
;; ALSO  XXX8= NIL



;;MAKE-CONDENSED-STRING
;;
;;added 2016-04
;;ddd
(defun make-condensed-string (string &key (min-length 3) (max-length 14)
                                     (min-word-len 3) (if-nil-return-nil-p T)
                                     (extra-chars "xxxxxxxxxxxx") (word-separator "")
                                     cond-words-p
                                     (phrase-min 3) (phr-word-max 5))
  "In U-Tstring,  makes a shorter string attempting based upon  main word or words in string. When word-separator, makes it a separator betw words.
  When cond-words-p, condenses each word using min-word-len & phr-word-max.
  When min-word-len, omits words too short.
  When word-separator= \"\" runs words together, if NIL, leaves a space.
  RETURNS newstr"
  (unless (and (or (equal string "")(null string)) if-nil-return-nil-p)
  (let*
      ((newstr "")
      (newstr-len 0) 
       (words)
       (rest)
       (dif)
       (str-len (length string))
      ;; (phrase-min (car long-phrase-words-spec))
       ;;(phr-str-max (second long-phrase-words-spec))
       (set-max-word-p (and cond-words-p
                            ;;not needed (< phrase-min str-len)
                            (> str-len max-length)))
       )
    (cond
     ;;STRING < MIN-LENGTH
     ((and min-length (< str-len min-length ))
         (setf dif (- min-length str-len))
        (setf newstr (format nil "~A~A" string (subseq extra-chars 0 dif)))
        )
     ;;STRING <  MAX-LENGTH, KEEP AS IS if unll word-separator
     ((and (<=  str-len  max-length) (null word-separator))
      (setf newstr string))
     ;;STRING > MAX-LENGTH SO SHORTEN BY CUTOFF OR BY SHORTER WORDS
        ;;cond-words-p  (phrase-min 8) (phr-word-min 3)(phr-word-max 5))
     (T
      (multiple-value-setq (words rest)
          (divide-string-to-all-tokens string))
      (loop
       for word in words
       do
       (let*
           ((word-len (length word))
            )
         (setf  newstr-len (length newstr))
       (cond
        ;;string < max-length, condense by remove spacesl only
        ((< str-len max-length) NIL)         
        ;;newstr less= max AND word > min-word, proceed
        ((and (<= newstr-len  max-length)
              (>= word-len min-word-len))
         ;;set-max-word-p AND word > nax, shorten word
         (when (and set-max-word-p (> word-len phr-word-max))
           (setf word (subseq word 0 phr-word-max))))
        (t nil))
         ;;incl word-separator or not in newstr
         (cond
          ((or (equal newstr "")(null word-separator))
           (setf newstr (format nil "~A~A" newstr word)))
          (word-separator
           (setf newstr (format nil "~A~A~A" newstr word-separator word))
           )
          (t nil ))
        (when (>= newstr-len max-length)
         (return))
       ;;end let,loop
       ))
     ;;end clause, cond
     ))
      ;;Adjust lengths?
      (setf newstr-len (length newstr))
      (cond
       ((> newstr-len max-length)
        (setf newstr (subseq newstr 0 max-length)))
       ;;if min-length nil or if > min, don't add anything
       ((and min-length (< newstr-len min-length ))
         (setf dif (- min-length newstr-len))
        (setf newstr (format nil "~A~A" newstr (subseq extra-chars 0 dif)))
        )
       (t nil))
     newstr
     ;;end let,unless, make-condensed-string
     )))
;;TEST
;;  (make-condensed-string "very funny") 
;;  (make-condensed-string "a little older")   = "littleold"
;;  (make-condensed-string  "smart") = "smart"
;;  (make-condensed-string  "b")     
;;  (make-condensed-string  "b in this place now")   = "thisplacenow"
;; ;;  (make-condensed-string "helper")
;; ;;  
;;2019 added features
;;  (make-condensed-string  "go to school") = "gotoschool"
;; (make-condensed-string  ""  :if-nil-return-nil-p NIL) = "xxx"
;; (make-condensed-string  nil  :if-nil-return-nil-p NIL) = "xxx"
;; (make-condensed-string  "")  :if-nil-return-nil-p T) = NIL
;; (make-condensed-string  nil ) :if-nil-return-nil-p T) = NIL
;; (make-condensed-string "this is a supercalaf string"  :word-separator "-" :
;; works = "this-superc"
;;
;; max-length longer
;; (make-condensed-string "this is a supercalaf string"  :word-separator "-" :cond-words-p T) = "this-super-strin"
;; (make-condensed-string "this is a supercalaf string"  :WORD-SEPARATOR NIL :cond-words-p T)  = "thissuperstrin"
;; (make-condensed-string "this is a supercalaf string"  :WORD-SEPARATOR NIL :cond-words-p T :MAX-LENGTH 20 :PHR-WORD-MAX 7)
;; works= "thissupercastring"
;; (make-condensed-string "this is a supercalaf string"  :WORD-SEPARATOR "-" :cond-words-p T :MAX-LENGTH 20 :PHR-WORD-MAX 7)
;; works= "this-superca-string"
;; (make-condensed-STRING "a little new") = "littlenew"
;; STRING TOO SHORT (< min-length = 3)
;; (make-condensed-STRING "2s") = "2sx"




;;CONVERT-NILS-TO-STRINGS
;;2017
;;ddd
(defun convert-nils-to-strings (list)
  "In U-tstring, sets every symbol that evals to NIL  in sym-list to a string= \"\".   RETURNS empty-string-syms =  a list of  the changed syms for each   INPUT:  "
  (let
      ((empty-string-syms)
       )
    (loop
     for item in list
     do     
     (when (and (symbolp item)
           (boundp item)
           (null (eval item)))
       (set  item "")
       (setf empty-string-syms (append empty-string-syms (list item))))
     ;;end loop
     )
    empty-string-syms
    ;;end let, convert-nils-to-strings
    ))
;;TEST
;;  (progn (setf xxx1 7 xxx2 nil  xxx3 '(a b c) xxx4 nil) (convert-nils-to-strings '(xxx1 xxx2 xxx3 xxx4)))
;;works=  (XXX2 XXX4)
;;CL-USER 9 > XXX2 = ""



;;REMOVE-OUTSIDE-STRING-DBL-QUOTES
;;2020
;;ddd
(defun remove-outside-string-dbl-quotes (string &key (begin-p T)(end-p T)
                                                add-backslash-p  (delete-chars '(#\space)))
  "U-tstring--uses read Eg from \"\"F:\\ \" to \"F: \"--removes \\ after F:
  add-backslash-p to return  \"F:/\" Use CLEAN-PATH-STR too? "
  (let*
      ((new-str) ;;(my-delete-first-spaces (my-delete-last-spaces string :delete-others '(#\")) :delete-others '(#\")))))
       )
    (when end-p
      (setf new-str (my-delete-last-spaces string :delete-others '(#\"))))
    (when begin-p
      (setf new-str (my-delete-first-spaces new-str  :delete-others '(#\"))))
    (when add-backslash-p
      (setf new-str (format nil "~A~A" new-str "/")))
    (when delete-chars
      (setf new-str (delete-chars new-str delete-chars)))
    new-str
  ;;end let, remove-outside-string-dbl-quotes
  ))
;;TEST
;; (remove-outside-string-dbl-quotes " \"F:\\ \" ")
;; works= "F:\\"
;; note:  (clean-path-str " \"F:\\ \" " :remove-extra-quotes-p T) =  "F:/"



;;WHEN-KEYS-ADD-VALUES
;;2017
;;ddd
(defun when-keys-add-values (match-key key-value-lists 
                                       &key begin-str end-str (test 'my-equal))
  "In U-tstring, when match-key matches a key, RETURNS (values return-str return-key return-value) which is one of the key-value-lists = (key value begin end)"
  (let
      ((return-str)
       (return-key)
       (return-value)
       ) 
    (loop
     for list in key-value-lists
     do 
     (when (listp list)
       (let
           ((key (car list))
            (value (second list))
            (begin (third list))
            (end (fourth list))
            (list-return-str)
            )
         (when (funcall test match-key key) ;;was(my-equal match-key key)
           (setf return-key key
                 return-value value
                 return-str 
                 (when-key-add-value match-key key value :begin-str begin :end-str end))
           (when begin-str (setf return-str (format nil "~A~A" begin-str return-str)))
           (when end-str (setf return-str (format nil "~A~A"  return-str end-str)))              
           (return))
         ;;end let,when,loop
         )))
    (values return-str return-key return-value)
    ;;end let, when-keys-add-values
    ))
;;TEST
;;  (when-keys-add-values 'key3 '((key1 1)(key2 2)(key3 3 "begin " " end")(key4 4)) :begin-str "begin-str " :end-str " end-str")
;;works=  "begin-str begin 3 end end-str"  KEY3   3





;;WHEN-KEY-ADD-VALUE
;;2017
;;ddd
(defun when-key-add-value (item key new-value &key begin-str end-str (test 'my-equal))
  "In U-tstring.  When key or key+value is present, make a string of new-value (only)"
  (let
      ((return-str "")
       )
    (when (funcall test item key) ;;was(my-equal item key)
      (cond
       ((stringp new-value)
        (setf return-str new-value))
       (t  (setf return-str (format nil "~A" new-value)))))

    (when begin-str (setf return-str (format nil "~A~A" begin-str return-str)))
    (when end-str (setf return-str (format nil "~A~A"  return-str end-str)))
    return-str
    ;;end let, when-key-add-value
    ))
;;TEST
;; (when-key-add-value 'this 'this 'new-value :begin-str "begin " :end-str " end.")
;; works=  "begin NEW-VALUE end."




;;CONVERT-ITEM-TO-STRING
;;2018
;;ddd
(defun convert-item-to-string (item &key head-string tail-string )
  "In U-tstring.  Converts a list of items to a list of strings using format. If head or tail string, inserts it into each string. SAME AS convert-objects-to-strings almost like make-list-of-strings"
  (let
      ((string)
       )
     (cond
      ((and head-string tail-string)
       (setf string (format nil "~A~A~A" head-string item tail-string)))
      (head-string
       (setf string (format nil "~A~A"head-string item)))
      (tail-string
       (setf string (format nil "~A~A" item tail-string)))
      (t (setf string (format nil "~A" item))))
    ;;end let,convert-item-to-string
    ))
;;TEST
;; (convert-item-to-string 'a) = "A"
;; (convert-item-to-string  99 :head-string "head" :tail-string "tail") = "head99tail"
;; (convert-item-to-string  '(this list)) = "(THIS LIST)"
;; (convert-item-to-string  '#\comma) = ","
;; (convert-item-to-string "a") = "a"



;;CONVERT-ITEMS-TO-STRINGS
;;2017
;;ddd
(defun convert-items-to-strings (list &key head-string tail-string 
                                      list-each-item-p no-nils-p)
  "In U-tstring.  Converts a list of items to a list of strings using format. If head or tail string, inserts it into each string. SAME AS convert-objects-to-strings almost like make-list-of-strings If no-nils-p, omits nils. If list-each-item-p, puts each item in a new list."
  (let
      ((newlist)
       )
    (loop
     for item in list
     do
     (cond
      ((and no-nils-p (null item))
       NIL)
      (list-each-item-p
       (setf newlist (append newlist (list (list (convert-item-to-string item 
                                                                 :head-string head-string :tail-string tail-string))))))  
      (t(setf newlist (append newlist (list (convert-item-to-string item 
                                                                 :head-string head-string :tail-string tail-string))))))
     ;;end loop
     )
     newlist
    ;;end let,convert-items-to-strings
    ))
;;TEST
;; (convert-items-to-strings '(1 2 3 4 5) :head-string "   " :tail-string "   ")
;; ("   1   " "   2   " "   3   " "   4   " "   5   ")
;; (convert-items-to-strings '(1 2 3 4 5))
;; works= ("1" "2" "3" "4" "5")
;; (convert-items-to-strings '(1 2 3 4 5) :head-string "head==" :tail-string "==tail==")
;;works= ("head==1==tail==" "head==2==tail==" "head==3==tail==" "head==4==tail==" "head==5==tail==")
;; (convert-items-to-strings '(1 2 3 4 5) :head-string "head==")
;; = ("head==1" "head==2" "head==3" "head==4" "head==5")
;; (convert-items-to-strings '(1 2 3 4 5)  :tail-string "==tail==")
;;= ("1==tail==" "2==tail==" "3==tail==" "4==tail==" "5==tail==")
;; list-each-item-p
;; (convert-items-to-strings '(1 2 3 4 5) :list-each-item-p T) 
;; works = (("1") ("2") ("3") ("4") ("5"))


;;CONVERT-LIST-ITEMS-TO-STRING
;;2018, 2019 added separator
;;ddd
(defun convert-list-items-to-string (list &key (exclude-nil-items-p t)
                                          (separator " "))
  "In   RETURNS new-string  INPUT: a list."
  (let
      ((n-items 0)
       (new-string "")
       )
    (when (listp list)
    (loop
     for item in list
     do
     (cond
      ((and exclude-nil-items-p (null item))
       NIL)
      ((equal item (car list))
       (incf n-items)
       (setf new-string (format nil "~A"  item)))
      (t (setf new-string (format nil "~A~A~A" new-string separator item))
                (incf n-items)))
     ;;end let,loop
     ))
    (values new-string n-items)
    ;;end let, convert-list-items-to-string
    ))
;;TEST
;;  (convert-list-items-to-string '(this "that" NIL  88))
;; works= "THIS that 88"  3
;; 2019 ;;  (convert-list-items-to-string '(this "that" NIL  88) :separator ".")
;; works= "THIS.that.88"  3
;; NOT exclude NIL
;;  (convert-list-items-to-string '(this "that" NIL  88) :exclude-nil-items-p NIL)
;; works= "THIS that NIL 88" 4
;; EXCLUDE NIL
;;  (convert-list-items-to-string '(this "that" NIL  88)) = "THIS that 88"  3


;;CONVERT-LIST-ITEMS-TO-STRINGS
;;2018, 2019 added separator
;;ddd
(defun convert-list-items-to-strings (list &key (exclude-nil-items-p t)
                                          (separator " "))
  "In RETURNS list of strings.  INPUT: If not a list, makes item into list."
  (let
      ((string-list)
       (n-items 0)
       )
    (unless (listp list)
      (setf list (list list)))
      (loop
       for item in list
       do
       (cond
        ((and exclude-nil-items-p (null item))
         NIL)
        ((stringp item)
         (setf string-list (append string-list (list item)))
         (incf n-items))
        (T
         (setf string-list (append string-list (list (format nil "~A" item))))
         (incf n-items)))
       ;;end loop
       )
     (values string-list n-items)
    ;;end let, convert-list-items-to-strings
    ))
;;TEST
;; (convert-list-items-to-strings '(a 22 (this) "what" (a b c) now))
;; works= ("A" "22" "(THIS)" "what" "(A B C)" "NOW") 6



;;PROCESS-TEXT-LIST
;;2018
;;ddd
(defun process-text-list (textlist &key (default-text "")
                                   return-nil-p)
  "U-Tstring, Processes MIXED string lists. RETURNS (values text-outlist n-list) EG. (\"Test THIS\" \"testxxx evalled\" \"Inside a list\" \"not in a list\" \"*NOT-BOUND-SYM\")  6"
  (let
      ((text-outlist)
       (n-list) 
       (text)
       )
    (cond
     ((listp textlist)
      (setf n-list (list-length textlist)))
     (t (setf n-list 1)))

    (cond
     ;;IF NOT A LIST
     ((and (not (listp textlist))(= n-list 1))
      (cond
       ((stringp textlist)
        (setf text textlist))
       ((symbolp textlist)
             (cond
              ((boundp textlist)
               (setf text (eval textlist)))
              (t (setf text (format nil "~A" textlist))))
             )
        (t (setf text default-text)))
      (when (or  text return-nil-p)
        (setf text-outlist (append text-outlist (list text))))
      )
     ;;OTHERWISE LOOP EACH ITEM
     (t
      (loop
       for item in textlist
       for n from 1 to n-list
       do
       (let ((text)
             )
         (cond
          ((and (listp item) (equal (car item) 'format))
           (setf text (eval item)))
          ((and item (listp item))
           (setf text (string-append* item)))
          ((stringp item)
           (setf text item))
          ((symbolp item)
           (cond
            ((boundp item)
             (setf text (eval item)))
            (t (setf text (format nil "~A" item))))
           )
           (t (setf text default-text)))


         ;;(break)
         (when (or  text return-nil-p)
           (setf text-outlist (append text-outlist (list text))))
         ;;end let,loop
         ))
      ;;end listp
      )
     (t nil))
  (values text-outlist n-list)
  ;;end let, process-text-list
  ))
;;TEST
;; (setf *testxxx "testxxx evalled")
;; (process-text-list '((format nil "Test ~A" 'this) *testxxx NIL ("Inside a list") "not in a list" *not-bound-sym))
;;works= ("Test THIS" "testxxx evalled" "Inside a list" "not in a list" "*NOT-BOUND-SYM")  6
;;
;; (process-text-list '((format nil "Test ~A" 'this) *testxxx NIL ("Inside a list") "not in a list") :default-text NIL)        
;; works= ("Test THIS" "testxxx evalled" "Inside a list" "not in a list")   5
;; FOR NON-LISTS
;; (process-text-list "this is only a string")
;; works= ("this is only a string") 1
;; (process-text-list *testxxx)
;; works= ("testxxx evalled") 1
;; ;; (process-text-list (QUOTE *testxxx))
;; works= ("testxxx evalled")  1
;; FOR NON-BOUND SYM
;; (process-text-list '*NOT-BOUND)
;; works = ("*NOT-BOUND")  1
;; (process-text-list '(("this list") a))
;; works= ("this list" "A")  2
;; ;; (process-text-list '("this list"))




;;MODIFY-STRINGS
;;2020
;;ddd
(defun modify-strings (strings  &key  pre post)
  "U-tstring   RETURNS new-strings "
  (let*
      ((new-strings)
       )
    (loop
     for str in strings
     do
       (when pre
         (setf str (format nil "~A~A" pre str)))
       (when post
         (setf str  (format nil "~A~A"  str post)))
     (setf new-strings (append new-strings (list str)))
     ;;end let,loop
     )
  new-strings
    ;;end let, modify-strings
    ))
;;TEST
;; (modify-strings  '("str1" "str2") :pre "pre" :post "post")



;;FORMAT-APPEND-2-STRING
;;2020
;;ddd
(defun format-append-2-string (item1 item2 &key if-list-use-S-p
                                     not-stringp1 not-stringp2 
                              (separator " ") (nil1 "")(nil2 "")  use-all-As-p use-all-Ss-p)
  "U-tstring  RETURNS: a string of 2 items--used ~S if string, ~A if not. If strings INSIDE of list, PRINTS NON-STRING (though caps right): "
  (let*
      ((textout)    
       (stringp1 (stringp item1))
       (stringp2 (stringp item2))
       (nilp1 (equal item1 nil1))
       (nilp2 (equal item2 nil2))
       )
  (cond
   (use-all-As-p
     (setf textout (format nil "~A~A~A" item1 separator item2)))
   (use-all-Ss-p
    (setf textout (format nil "~S-A-S" item1 separator item2)))
   (T
    ;;If list, process as ~S?
    (when (and (listp item1) if-list-use-S-p)
      (setf stringp1 T))
    (when (and (listp item1) if-list-use-S-p)
      (setf stringp2 T))
    ;;over-ride other settings?
    (when not-stringp1
      (setf stringp1 NIL))
    (when not-stringp2
      (setf stringp2 NIL))
    ;;to append a mixed string (eg long first part) to string, 
    (cond
     ((and nilp1 stringp2)
      (setf textout (format nil "~S"  item2)))
     ((and stringp1 nilp2 )
      (setf textout (format nil "~S"  item1)))
     ((and nilp1 (null stringp2))
      (setf textout (format nil "~A"  item2)))
     ((and (null stringp1) nilp2 )
      (setf textout (format nil "~A"  item1)))
     ((and stringp1 stringp2) 
      (setf textout (format nil "~S~A~S" item1 separator item2)))
     ((and (null stringp1)(null stringp2))
      (setf textout (format nil "~A~A~A" item1 separator item2)))
     ((and  stringp1 (null stringp2))
      (setf textout (format nil "~S~A~A" item1 separator item2)))
     ((and (null stringp1) stringp2)
      (setf textout (format nil "~A~A~S" item1 separator item2)))
     (T (setf textout (format nil "~A~A~A" item1 separator item2))))
    ;;end T,cond
    ))
    textout
    ;;end let, format-append-2-string
    ))
;;TEST
;; (format-append-2-string '("this")  '(that) :if-list-use-S-p T)
;;works= "(\"this\") (THAT)"
;;doesn't work on strings inside lists
;; (format-append-2-string '("this")  '(that)) = "(this) (THAT)"
;; If string inside of list, prints non-string (though caps right):
;; (format-append-2-string '("this")  '(that)) = "(this) (THAT)" 
;; ==> (format-append-2-string  "this" 'that) = "\"this\" THAT"
;; ;; (format-append-2 '("this")  '(that) :use-all-As-p T) = "(this) (THAT)"
;; 







;; hhh ------------------------------------------- HELP -------------------------------------------------

;;  (length "abcedfgh") = 8
;;   gp::*default-font* = #<GRAPHICS-PORTS::GENERIC-FONT {:SYSTEM :SYSTEM * * * * *} 20E42DB7>
;;  (gp::string-width "this is a string" gp::*default-font*) = error
;;  gp::port

;;  STRING-EQUAL = BEST TEST??
;; (member #\b '(a b c) :test 'string-equal) = (B C)
;; (member #\b '(a "b" c) :test 'string-equal) = ("b" C)
;; (member #\b '(a  #\b c) :test 'string-equal) = (#\b C)
;; (member #\b '(a  "B" c) :test 'string-equal) = ("B" C)


;;from Seibel
#|(count 1 #(1 2 1 2 3 1 2 3 4)) = 3
(remove 1 #(1 2 1 2 3 1 2 3 4)) = #(2 2 3 2 3 4)
(remove 1 '(1 2 1 2 3 1 2 3 4)) = (2 2 3 2 3 4)
(remove #\a "foobarbaz") = "foobrbz"
(substitute 10 1 #(1 2 1 2 3 1 2 3 4)) = #(10 2 10 2 3 10 2 3 4)
(substitute 10 1 '(1 2 1 2 3 1 2 3 4)) = (10 2 10 2 3 10 2 3 4)
(substitute #\x #\b "foobarbaz") = "fooxarxaz"
(find 1 #(1 2 1 2 3 1 2 3 4))  = 1
(find 10 #(1 2 1 2 3 1 2 3 4)) = NIL
(position 1 #(1 2 1 2 3 1 2 3 4)) = 0
|#
;; SSS PLAY HERE-- LEARN ABOUT CHARS AND STRINGS WITH BLANKS
;;   TO FIX MY-SUBSTITUTE
;;  (equal "this" "THIS") = NIL
;;  (string-equal  "this" "THIS")  = T
;;  (equal  #\a #\A) = NIL
;;  (char-equal  #\a #\A) = T
;;  (string-equal #\space " ") = T
;;  (string-equal #\c "C")  = T

;; (setf **a "a"  **b "  ")
;; (length (format t "~A~A" **a **b)) ;;" "" " ")
;; (char " " 0)

;;  #\a#\b#\c = error
;;  #\a #\b #\c  = values of same 
;; (length #\a) = error
;; (length (string #\a)) = 1

;;USE OF CHAR AND SCHAR -- PREFER CHAR
#| (setq my-simple-string (make-string 6 :initial-element #\A)) =>  "AAAAAA"
 (schar my-simple-string 4) =>  #\A
 (setf (schar my-simple-string 4) #\B) =>  #\B|#

#|STRING x => string
ARGUMENTS and Values:
x---a string, a symbol, or a character.
string---a string.
DESCRIPTION: Returns a string described by x; specifically:
If x is a string, it is returned.
If x is a symbol, its name is returned.
If x is a character, then a string containing that one character is returned.
|#

;;  FORMAT USING ~S TO PRINT STRINGS AND READ READABLE
;;  (setf tilde-info "tilde S prints in (read) able output--eg strings are surrounded with quotes")
;;  (format t "This is tilde-info= ~S" tilde-info) = 
;; returns:  This is tilde-info= "tilde S prints in (read) able output--eg strings are surrounded with quotes"
;;  (format t "~S"  #\space) = #\Space
;;  (format t "~S"  (string #\space)) = " "
;;  (format nil "~S"  #\space) = "#\\Space"
;;  (format nil "~S"  (string #\space)) = "\" \""
;;
;;STRING ELT or CHAR
;;returns char
;; (elt "already a string" 7) = #\Space
;; (char "already a string" 7) = #\Space
;;eeturns string
;;(string (elt "already a string" 7)) = " "

;;POSITION
;; (position "bcd" "xabcdefg") = nil
;; (position #\b "xabcdefg") = 2
;; CHAR
;; (char "abc def" 3) = #\Space
;; (elt "abc def" 3) = #\Space
;; POSITION
;; (position " " "abc def") = NIL
;; (position #\Space "abc def") = 3
;;xxx SEARCH
;; (search " "   "abc def") = 3
;; (search #\Space "abc def") = error not of type sequence
;; (search (string #\Space) "abc def") = 3
;; (search "A B"  "xyz a bcda b c") = NIL
;; (search "A B"  "xyz a bcda b c" :test 'string-equal)  = 4
;; (search "A B"  "xyz a bcda b c" :test 'string-equal :from-end t) = 9
;;FIND
;; (find #\b "abcbe b f" :test 'equal) = #\b 
;; (find #\b  '(a b  c d)) = NIL
;;
;;Function REPLACE
;;  replace sequence-1 sequence-2 &key start1 end1 start2 end2 => sequence-1
;;Destructively modifies sequence-1 by replacing the elements of subsequence-1 bounded by start1 and end1 with the elements of subsequence-2 bounded by start2 and end2.
;; NO (replace 'string "\\n" (concatenate  (string #\\)(string #\n)) "this is an ntest\\n" :test 'equal)

;;xxx SUBSTITUTE
;;THESE WORK
;; (substitute  #\x  #\d  "abcdef g h" :test 'equal) = "abcxef g h"
;; (substitute  #\space  #\d  "abcdef g h" :test 'equal) = "abc ef g h"
;; (substitute  #\newline  #\n "abcdef g h \n  x y z" :test 'equal) = "abcdef g h 
;;  x y z"

;;THESE DON'T WORK
;; (substitute  #\x#\y  #\d#\e  "abcdef g h" :test 'equal)
;; (substitute "xy"  "bc"  "abcdef g h" :test 'equal) = 
;;  (substitute (string #\newline) "\n"  "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+" :test 'equal) = doesn't work
;;  (substitute  "this" "all"  "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+" :test 'equal) = doesn't work
;;  (substitute  "this" "all "  "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+" :test 'equal) = doesn't work


;;xxx NUMBERS
;; (string-equal 7 "7")  Error: Cannot coerce 7 to type STRING.
;;  (equal 7 "7") = NIL

;;xxx READ-LINE
;;  (test-read-line)
#|(defun test-read-line ()
  (let
      (( lines "line1 \" \"   a b #\space
            line2    ")
       (input1)
       (input2)
       )
  (setf input1 (read-line (setq input-stream (make-string-input-stream lines))))
  (setf input2 (read-line  input-stream (make-string-input-stream lines)))
  (values input1 input2)
        ))|#
;;works, returns:
#|   "line1 \" \"   a b #\space"
"            line2    "|#



;;XXX
;;
;;COERCE object result-type => result
;;Examples:
#| 
;;list to vector,string,
(coerce '(a b c) 'vector) =>  #(A B C)
(coerce '(a b c) 'string) => error
(string #\A) = "A"
;;symbol to char
 (coerce 'a 'character) =>  #\A
;;numbers
 (coerce 4.56 'complex) =>  #C(4.56 0.0)
 (coerce 4.5s0 'complex) =>  #C(4.5s0 0.0s0)
 (coerce 7/2 'complex) =>  7/2
 (coerce 0 'short-float) =>  0.0s0
 (coerce 3.5L0 'float) =>  3.5L0
 (coerce 7/2 'float) =>  3.5
;; cons
 (coerce (cons 1 2) t) =>  (1 . 2)
 (coerce 7 'string) = Error: Cannot coerce 7 to type STRING.
|#



;;000---------------------------------------------------- OLDER ---------------------------------------------


#|
;;(DEFINE-INSTANCE *engl100 (:is engl100))
;;(DEFINE-INSTANCE *math122 (:is math122))


;;ADD-MARGINS moved to r-intro because it wouldn't compile right


;;ADD-MARGINS
;;
;;NEW 10/90
;;
;;-->NOTE: IF RIGHT-MARGIN-PLACE IS NIL, THEN WILL USE "NATURAL" RIGHT MARGINS
;;
;;(add-left-margins (string left-margin-spaces  pstream 
;;			&key right-margin-place new-string print-to-screen)

;; note: ADD-LEFT-MARGINS in planner uses this function wholly
;;(add-margins "(Note: FIN464 is not required for the Real Estate concentration.)" 5 t :right-margin-place 64)
;;
;(defun add-margins (string left-margin-spaces  pstream 
;			&key right-margin-place new-string print-to-screen)
;
;   (list "TSTRING--ADDS RIGHT MARGINS AT NEAR SPACE TOO")
;   (let* 
;	 ((prestring 
;	    (make-string  left-margin-spaces  :initial-element #\space ))
;	 ;;(string-stream (make-string-input-stream string))
;	 (line-string)
;	 (end-place)
;	 (string-length (length string))
;	 (rest-string)
;	 (space-place)
;	 (current-right-margin right-margin-place)
;	)
;
 ;    (cond
;       (current-right-margin
;	 (setf string (substitute #\space #\newline string :test 'equal))
;	 
;	 ;;MAKE SURE THE RIGHT MARGIN IS NOT PAST THE STRING LENGTH
;	 (cond
;	   ((<= string-length current-right-margin)
;	     (setf current-right-margin string-length)
;	     )
;
;	 ;;OTHERWISE--FIND A BLANK-SPACE NEAR THE RIGHT MARGIN
;	   ((setf space-place (search " "  string 
;				       :start2 (- current-right-margin 6)
;				       :end2 (+ current-right-margin 6)))
;
;	     (setf current-right-margin space-place)
;	     )
;	   (t nil))
;
;;; (print `(rigtht-margin-place ,right-margin-place str-lenth ,string-length))
;
;	 (setf line-string (subseq string 0 current-right-margin))
;
;	 (unless (<= string-length current-right-margin)
;	       (setf  rest-string (subseq string (+ current-right-margin 1) )))
;	 )
 ;     ((setf end-place  (search (string #\newline) string))
;	(setf line-string (subseq string 0 end-place)
;	      rest-string (subseq string (+ end-place 1)))
;	)
;       (t
;	(setf end-place string-length
;	      line-string (subseq string 0 end-place)   
;	      rest-string nil)
;	))
;
;
;	(setf line-string (format nil "~A~A" prestrying line-string))
;
;;;  (print `(end-place ,end-place line-string ,line-string rest-string ,rest-string))
;
;	(cond
;	    (new-string
;	      (if line-string
;	        (setf new-string (format nil "~A~A~A" 
;					  new-string #\newline line-string)))
;	      )
;	    (t (setf new-string (format nil "~A"  line-string ))))
;
;	(cond
;	  (rest-string 
;	  (setf new-string
;	     (add-left-margins rest-string left-margin-spaces t :new-string new-string
;			       :right-margin-place right-margin-place))
;;;;	  (my-substitute (string #\newline) "   " "this is a      this is second" )
;
;	  )
;	  (t nil))
;
;	(unless (equal pstream t)
;	  (setf new-string (format nil "~A~A" new-string  #\newline))
;	  (format pstream "~A" new-string)
;	  ;;done in larger function
;	  (if print-to-screen
;	      (format t "~A" new-string))
;
;	  )
;	new-string
;	))
; 

 |#

#|
(setf *test-shaq-list '(
   ("tknowmor	t-Want to know more of self")
         ("texperie	t-Experienced self-help user")
         ("twanttho	t-Want thorough assessment")
         ("twantspe	t-Want specific help")
         ("tworknga	t-worknga")
         ("tu100stu	t-CSULB U100 student")
         ("tcsulbst	t-CSULB other student")
         ("totherst	t-Other student")
         ("tcolstu	t-Other college student")
         ("tinstruc	t-Instructor")
         ("tcolfaca	t-College faculty-admin")
         ("twanthel	t-Want help with problem")
         ("wantspq	g-Specific questionnaire")
         ("gsuchap	g-Success-happiness")
         ("gacadsuc	g-Academic success")
         ("gemocop	g-Emotional coping")
         ("gslfest	g-Self-esteem")
         ("gprocrst	g-Procrastination")
         ("gtimeman	g-Time Management")
         ("grelat	g-Relationships")
         ))

 (setf *test-shaq-list2
       '(
                  ("Email	")
         ("ZipCode")	
         ("Nation	")
         ("HrsWork")
         ))
|#


#|
;;OLD VERSION
(defun my-substitute2 (newitem olditem sequence)
  "In U-tstring.lisp. Note: number of spaces in newitem and olditem MUST? BE SAME--add spaces, etc if necessary?"
  (let
      ((newitem-str " ")
       (olditem-str " ")
       (olditem-length)
       (sequence-str " ")
       (seq-length (length sequence))
       (n-match 0)
       (new-seq "")
       (new-char "")
       ;; (old-char0 "")
       (old-char "")
       )
    (cond
     ((and (setf newitem-str (string newitem)
                 olditem-str (string olditem)
                 sequence-str (string sequence)))
      (setf  olditem-length (length olditem-str))
      (dotimes (n seq-length)
        ;;was (setf old-char0 (char sequence-str n))
        (setf old-char (string (char sequence-str n)))
     ;;   (afout 'out (format nil "newitem-str= ~A~% olditem-str= ~A~% sequence-str= ~A~% old-char= ~A~% newitem-str= ~A~%new-seq= ~A~% seq-length= ~A olditem-length= ~A~% n= ~A~% "newitem-str olditem-str sequence-str  old-char newitem-str new-seq seq-length olditem-length n))	
        (cond
         ((string-equal old-char (char olditem-str n-match))  ;;was old-char0
          (setf new-char (string (char newitem-str n-match)))
          (incf n-match )	
          (cond
           ((= olditem-length n-match)  ;;was  (length olditem-str) n-match)
            (setf new-seq (format nil "~A~A" new-seq newitem-str)
                  n-match 0))
           (t nil)))
         (t (setf n-match 0
                  new-char old-char
                  new-seq (format nil "~A~A" new-seq new-char))))
        )
      new-seq)
     (t (print `(ERROR--ONE OF THE OBJECTS==> ,newitem-str ,olditem-str OR ,sequence-str  IS NOT A STRING--IN QUOTES))))
    ))

|#;; (setf **a "a"  **b "  ")
;; (length (format t "~A~A" **a **b)) ;;" "" " ")
;; (char " " 0)


;; OLD VERSION -- MOSTLY WORKS
#|(defun my-delete-substring (substring string)
  "In U-tstring.lisp, Deletes the first substring. RETURNS (values new-string first-string rest-string substring) new-string = nil if no match."
  (let
      ((char)
       (char-str)
       (length-str (length string))
       (length-ss (length substring))
       (ss-n 0)
       (ss-char-str "")
       (matched-str "")
       (continuous-match-p)
       (first-string "")
       (rest-string "")
       (new-string)
       )
    (loop
     for n from 0 to length-str
     do
     (unless (and (> ss-n 0)(>= ss-n length-ss))
       (setf ss-char-str (string (char substring ss-n))
             char (char string n)
             char-str (string char)))
#|     (setf last-string (format nil "~A~A" first-string char-str
      rest-string|#
     (cond
      ((and (= ss-n 0) (string-equal char-str ss-char-str) )
       (setf continuous-match-p t
             ss-n 1
        matched-str (format nil "~A~A" matched-str char-str)))
     ((= length-ss (+ ss-n 1))
       (setf rest-string (subseq string (+ n 1))
             first-string (subseq string 0  (- length-str (+ n 2)))  ;;  length-ss  1))
             new-string (format nil "~A~A" first-string rest-string))
       (return))
      ((and continuous-match-p (string-equal char-str ss-char-str))
      (if (< ss-n length-ss) (incf ss-n))
       (setf matched-str (format nil "~A~A" matched-str char-str)))
     (t  (setf ss-n 0
               matched-ss ""
               continuous-match-p nil)))   
  ;;  (afout 'out (format nil "char-str= ~A~% length-str= ~A~% length-ss= ~A~% ss-n= ~A~% ss-char-str= ~A~% matched-str= ~A~%  continuous-match-p= ~A~% first-string= ~A~% rest-string= ~A~% new-string= ~A~%" char-str length-str length-ss ss-n ss-char-str matched-str  continuous-match-p first-string rest-string new-string))
          
     ;;end loop
     )
    ;;end my-delete-substring
    (values new-string first-string rest-string substring)
    ))|#

#| CONFUSING:  WILL ACTUALLY MAKE A SET OF SYMBOLS OFF OF A ROOT VARYING ONLY THE FIRST INDEX OF A LIST OF INDEXES
;; REPLACED BY MAKE-NEW-INDEX-SYMBOLS
(defun make-new-index-symbol ( index-spec-list  &key (change-only-first-index-p T)) 
  ;;&key   ;;end  betw-symbol)
  "In U-tstring.lisp, Makes  symbol.  Each spec-list  (prefix (spec-sublist) betw-symbol end). The last 2 can be empty.The spec-sublist is a list of  (begin-num increment-num) used to create the array index values. CHANGE-ONLY-FIRST-INDEX-P is used for ART2 so can change other indexes from outside this function. RETURNS (values new-symbol new-index-spec-list new-symbol-string). INDEX-SPEC-LIST=(symbol-root (index1 str index2 str ... last-index end-str))"
  (cond
   ((listp index-spec-list)
    (let*
        ((prefix (car index-spec-list))
         (new-symbol)
         (spec-sublist (second index-spec-list ))
         (betw-symbol)
         (end)
         ;; (next-digit 0)
         (digit-n -1)
         (current-digit 0)
         (incr-digit 0)
         (new-digit 0)
         (new-spec-sublist spec-sublist)
         ;;do so have record of original spec-list
         (new-index-spec-list  index-spec-list)
         (length-spec-list (length index-spec-list))
         (new-spec)
         (new-symbol-string)
         )
      ;;set optional parameters
      (if (> length-spec-list 2) (setf betw-symbol (third index-spec-list))
        (setf betw-symbol ""))
      (if (> length-spec-list 3) (setf end (fourth index-spec-list))
        (setf end ""))
      (cond
       (change-only-first-index-p
        ;;ONLY USE THE FIRST DIGIT/SUBLIST--rest must be changed from outside of function
        (loop
         for index-specs in  spec-sublist
         for n from 1 to (length spec-sublist)
         do
         (setf current-digit (car index-specs)
               incr-digit (second index-specs))
         (cond
          ;;only change specs for first index/sublist
          ((= n 1)          
           (incf digit-n)
           (setf new-digit (+ current-digit incr-digit)
                 new-spec (list new-digit incr-digit)
                 new-spec-sublist (replace-list new-spec-sublist digit-n new-spec)))
          (t nil))
         (cond 
          ((= n 1)
           (setf new-symbol (format nil "~A~A" prefix  current-digit) ))
          (t 
           (setf new-symbol (format nil "~A~A~A" 
                                    new-symbol betw-symbol current-digit))))
         ;;end loop, change-only-first-index-p
         ))
       (t 
        ;;process the index items in sublist --caused changing ALL sublists,
        (dolist (spec-sublist spec-sublist)
          (incf digit-n)
          (setf current-digit (car spec-sublist)
                incr-digit (second spec-sublist))

          (setf new-digit (+ current-digit incr-digit)
                new-spec (list new-digit incr-digit)
                new-spec-sublist (replace-list new-spec-sublist digit-n new-spec))
          (cond 
           ((= digit-n 0)
            (setf new-symbol (format nil "~A~A" 
                                     prefix  current-digit) ))
           (t 
            (setf new-symbol (format nil "~A~A~A" 
                                     new-symbol betw-symbol current-digit))))
          ;;end dolist, change-only-first-index-p= NIL, cond
          )))
      (setf new-symbol-string (format nil "~A~A" new-symbol end)
            new-symbol (my-make-symbol  new-symbol-string)
            new-index-spec-list (list prefix new-spec-sublist betw-symbol end))
      
      (values new-symbol new-index-spec-list new-symbol-string)
      ))
   (t nil))
  )|#




;;xxx----------------- REPLACED BUT MOSTLY WORK ----------------------------------
;;MAKE-NEW-INDEX-SYMBOLS
;;
;;mmm
;;ddd
#|(defun make-new-index-symbols (symbol-index-spec-list 
                                         &key  no-type-symbol-p  ;;make-sublists-for-each-dim-p
                                         (restart-numbers-each-dim-p T))
  "In U-tstring.lisp, Makes a series of  n-symbols related symbols in a list. Each spec-list  (prefix (spec-sublist) betw-symbol end). The last 2 can be empty.The SPEC-SUBLIST is a list of  (begin-num increment-num) used to create the array index values. See make-new-index-symbol also. Makes number of indexes = lengthh of spec-sublist.  RETURNS (values new-symbol-type new-symbols-list new-symbol-index-spec-list-of-lists new-symbol-string-list)
:(\"cell-root-str\" ((N begin-n incr-n end-str &key ) (N begin-n incr-n end-str &key) etc)  &key doc) &key = keys can be later added
 SYMBOL-INDEX-SPEC-LIST= (\"Wup\" ((8 1 1 \"-\" :doc \"doc1\")(1 1 1 \"=\")(5 1 1 \"-\")(1 2  1 \"\")) :doc \"whole doc\") NOTE:: use N=1 and incr=0 if all cells in same field."
  (let*
      ((new-symbol)              ;;eg symbol-index-spec-list=  ;;old("varx" ((1 1)(1 1)(1 1)) "-" "end")
       (var-root (car symbol-index-spec-list))  ;;eg "varx"
       (cur-index-specs (second symbol-index-spec-list))
       (n-dims (length cur-index-specs))          ;;eg  3
       (first-index-spec (car cur-index-specs))   ;;eg (1 1 1 "-")
       ;;for each dim
       (n-dim-elements)
       (begin-index)
       (incr-index)
       (end-index-str) 
       (index-keys)
       (cur-index)
       (cur-index-spec)
       (cur-index-list)
       (first-dim-index-spec (car cur-index-specs))
       (cur-dim1-spec first-dim-index-spec)
       (n-dim1-elements)
       (dim1-cur-index)
       (1-incr-index)
       (dim1-end-index-str)
       (new-index-spec-list)
       (new-index-spec-list-of-lists)
       (new-symbol-index-spec)
       ;;other spec-list vars
       (sym-keys (cddr symbol-index-spec-list))
       ;;rest check???

       (new-dim-symbols-list)
       (new-dim-index-spec-list)
       (new-dim-symbol-string-list)       
       (new-symbol-string)
       ;;returns
       (new-symbol-type)    
       (new-symbols-list)
       (new-symbol-index-spec-list  symbol-index-spec-list) ;;why start with this??
       (new-symbol-index-spec-list-of-lists)
       (new-symbol-string-list)
       )
    ;;first make the new symbol-type symbol that the list is set to
    (unless no-type-symbol-p
      (setf new-symbol-type
            (my-make-symbol
             (format nil "~A" var-root ))))    

))|#

;;EG OF COMPLETED LIST
#|  ((((X1111 X2111 X3111)(X1211 X2211 X3211))
  ((X1121 X2121 X3121)(X1221 X2221 X3221)))
  (((X1112 X2112 X3112)(X1212 X2212 X3212))
  ((X1122 X2122 X3122)(X1222 X2222 X3223)))
  (((X1113 X2113 X3112)(X1212 X2212 X3213))
  ((X1123 X2123 X3122)(X1222 X2222 X3223))))

(defun make-fill-indexes (root dim-spec-list &optional root-list)
   "(first last)(first last)(first last)))"
   (let*
       ((first)
        (last)
        (new-str)
        (dim-list)
        (return-lists)
        (dim-length (length dim-spec-list))
      ;;  (reverse-dim-spec-list (reverse dim-spec-list))
        ) 
    ;; (setf *out (append *out (list (list (format nil "NEW CALL,  dim-spec-list= ~A" dim-spec-list)))))
     (loop
      for dim in dim-spec-list
      for n from 1 to dim-length
      do
      (setf first (car dim)
            last (second dim))
      ;;(afout 'out (format nil "NEW-DIM,  dim= ~A" dim))
     ;; (setf *out (append *out (list  (format nil "NEW-DIM,  dim= ~A" dim))))
        (cond
         (root-list
          (multiple-value-setq (new-dim-list root-list)
              (make-fill-indexes  root dim root-list))
          (setf return-lists (append return-lists (list new-dim-list)))       
          ;;(afout 'out (format nil "return-lists=  ~A~%root-list= ~A~%" return-lists root-list))
          )
         ((= n 1)
          (multiple-value-setq (new-dim-list root-list)
              (make-fill-indexes  root dim root-list))
          )
         (t nil))     
        (setf return-lists (append return-lists (list dim-list)))
      ;;end dim loop
      )    
  (values return-lists root-list)
  ))
;;  (make-fill-indexes "root" '((11 15)(21 24)(31 35)))


;;MAKE-NEXT-DIM-INDEX-SYMBOLS
;;
;;ddd
(defun make-next-dim-index-symbols (symbol-string dim-index-specs  &key begin-index)
  "In U-tstring.lisp, "
  (let*
      ((new-symbol)              ;;eg symbol-index-spec-list=  ;;old("varx" ((1 1)(1 1)(1 1)) "-" "end")
       ;;for each dim
       (n-dim-elements)
       (begin-index)
       (incr-index)
       (end-index-str) 
       (index-keys)
       (cur-index)
       (cur-index-spec)
       (cur-index-list)
    ;;   (first-dim-index-spec (car cur-index-specs))
    ;;   (cur-dim1-spec first-dim-index-spec)
   ;;    (n-dim1-elements)
   ;;    (dim1-cur-index)
       (1-incr-index)
       (dim1-end-index-str)
       (new-index-spec-list)
       (new-index-spec-list-of-lists)
       (new-symbol-index-spec)
       ;;other spec-list vars
       (sym-keys (cddr symbol-index-spec-list))
       ;;rest check???

       (new-dim-symbols-list)
       (new-dim-index-spec-list)
       (new-dim-symbol-string-list)       
       (new-symbol-string)
       ;;returns
       (new-symbol-type)    
       (new-symbols-list)
       (new-symbol-index-spec-list  symbol-index-spec-list) ;;why start with this??
       (new-symbol-index-spec-list-of-lists)
       (new-symbol-string-list)
       )

 ))
|#



;;MAKE-NEW-INDEX-SYMBOLS
;;
;;mmm
;;ddd
#|(defun make-new-index-symbols (symbol-index-spec-list 
                                         &key  no-type-symbol-p  ;;make-sublists-for-each-dim-p
                                         (restart-numbers-each-dim-p T))
  "In U-tstring.lisp, Makes a series of  n-symbols related symbols in a list. Each spec-list  (prefix (spec-sublist) betw-symbol end). The last 2 can be empty.The SPEC-SUBLIST is a list of  (begin-num increment-num) used to create the array index values. See make-new-index-symbol also. Makes number of indexes = lengthh of spec-sublist.  RETURNS (values new-symbol-type new-symbols-list new-symbol-index-spec-list-of-lists new-symbol-string-list)
:(\"cell-root-str\" ((N begin-n incr-n end-str &key ) (N begin-n incr-n end-str &key) etc)  &key doc) &key = keys can be later added
 SYMBOL-INDEX-SPEC-LIST= (\"Wup\" ((8 1 1 \"-\" :doc \"doc1\")(1 1 1 \"=\")(5 1 1 \"-\")(1 2  1 \"\")) :doc \"whole doc\") NOTE:: use N=1 and incr=0 if all cells in same field."
  (let*
      ((new-symbol)              ;;eg symbol-index-spec-list=  ;;old("varx" ((1 1)(1 1)(1 1)) "-" "end")
       (var-root (car symbol-index-spec-list))  ;;eg "varx"
       (cur-index-specs (second symbol-index-spec-list))
       (n-dims (length cur-index-specs))          ;;eg  3
       (first-index-spec (car cur-index-specs))   ;;eg (1 1 1 "-")
       ;;for each dim
       (n-dim-elements)
       (begin-index)
       (incr-index)
       (end-index-str) 
       (index-keys)
       (cur-index)
       (cur-index-spec)
       (cur-index-list)
       (first-dim-index-spec (car cur-index-specs))
       (cur-dim1-spec first-dim-index-spec)
       (n-dim1-elements)
       (dim1-cur-index)
       (1-incr-index)
       (dim1-end-index-str)
       (new-index-spec-list)
       (new-index-spec-list-of-lists)
       (new-symbol-index-spec)
       ;;other spec-list vars
       (sym-keys (cddr symbol-index-spec-list))
       ;;rest check???

       (new-dim-symbols-list)
       (new-dim-index-spec-list)
       (new-dim-symbol-string-list)       
       (new-symbol-string)
       ;;returns
       (new-symbol-type)    
       (new-symbols-list)
       (new-symbol-index-spec-list  symbol-index-spec-list) ;;why start with this??
       (new-symbol-index-spec-list-of-lists)
       (new-symbol-string-list)
       )
    ;;first make the new symbol-type symbol that the list is set to
    (unless no-type-symbol-p
      (setf new-symbol-type
            (my-make-symbol
             (format nil "~A" var-root ))))                                      

     ;;TO MAKE SUBLISTS FOR EACH DIM
     ;;SSS START HERE     
     (loop          
      for dim-n from 1 to n-dims  ;;eg n-dims= 3 in eg below
      for cur-index-spec  in cur-index-specs     ;;eg index-spec= (8 1 1 "-" :doc "doc1")
      do
      ;;get current index info
      (multiple-value-setq (n-dim-elements begin-index incr-index
                                           end-index-str index-keys)
          (values-list cur-index-spec))
      (when  restart-numbers-each-dim-p 
        (setf cur-index begin-index))
      ;;;(setf cur-index-list nil) ;;needed or not??
        
      (cond
       ;;for first dim, do nothing
       ((= dim-n 1)
        (setf cur-index (- cur-index 1))
        )
       ;;CHANGE/REPLACE cur-index-spec AFTER FIRST DIM
       ;;the INNERMOST NESTED LISTS WILL CONTAIN THE SYMBOLS VARYING ONLY THE FIRST DIM.
       ((> dim-n 1)

       ;;increment the cur-index
       (setf cur-index-spec (replace-list cur-index-spec 1 cur-index))
       ;; (if index-keys (setf cur-index-spec (append cur-index-spec index-keys))) 
        
        ;;replace cur-index-spec in cur-index-specs
        (setf cur-index-specs (replace-list cur-index-specs (- dim-n 1) cur-index-spec))

        ;;when want to restart indexe numbers with each dim
      ;;needed or not?? (when restart-numbers-each-dim-p
      ;;    (setf  cur-index-specs (replace-list cur-index-specs 0 first-index-spec)))          

        ;;make the new new-symbol-index-spec-list to be used in loop below
        (setf new-symbol-index-spec-list 
              (list var-root cur-index-specs  sym-keys ))
        ;;end  > dim-n 1, cond
        ;;NOT END, CONTINUE ))

         ;;note: make the new symbols by varing only first dim at most nested list

          ;;NOW CREATE THE NEW FIRST DIM ELEMENTS FOR EACH DIM
           ;;;yyyy
      (multiple-value-setq (n-dim1-elements dim1-cur-index dim1-incr-index
                                           dim1-end-index-str) ;;  dim1-index-keys)
          (values-list first-dim-index-spec))
      ;;subtract 1 bec add 1 below
      (setf dim1-cur-index (- dim1-cur-index 1))

      (loop
       for  dim1-element-n from 1 to  n-dim1-elements
       do  
       ;; increment the dim1-cur-index
       (setf dim1-cur-index (+ dim1-cur-index dim1-incr-index))

       ;;replace the cur-dim1-spec with revised one (modified after first cycle)
       (setf cur-dim1-spec (list n-dim1-elements dim1-cur-index dim1-incr-index
                                 dim1-end-index-str))
       (setf cur-index-specs (replace-list cur-index-specs 0 cur-dim1-spec)
             cur-symbol-index-spec (list var-root cur-index-specs))

       ;;MAKE THE NEW-SYMBOL, ETC
       (multiple-value-setq (new-symbol new-index-spec-list new-symbol-string)
           (make-new-index-symbol cur-symbol-index-spec))

       ;;(afout 'out (format nil "new-symbol= ~A~% new-index-spec-list= ~A~%  new-symbol-string= ~A~%" new-symbol new-index-spec-list new-symbol-string))

       ;;append the accumulation return lists
       (setf new-dim-symbols-list 
             (append new-dim-symbols-list  (list new-symbol))
             new-dim-symbol-string-list 
             (append new-dim-symbol-string-list (list new-symbol-string))
             new-dim-index-spec-list
             (append  new-dim-index-spec-list  (list cur-symbol-index-spec)))

         ;;append the 
        (setf new-index-spec-list-of-lists
              (append  new-index-spec-list-of-lists  (list new-dim-index-spec-list)))

        (setf new-symbols-list (append new-symbols-list (list new-dim-symbols-list))   
              new-symbol-string-list
              (append new-symbol-string-list (list new-dim-symbol-string-list)))

        ;;increment the dim variables
        (setf cur-index (+ cur-index incr-digit)
              new-index-spec (list n-dim-elements cur-index incr-index end-index-str)
              new-symbol-index-spec-list (list symbol-root new-index-spec))

        ;;end loop n-dim-elements
        ) 

        ;;reinitialize the dim variables
        (setf new-dim-symbols-list nil
              new-dim-index-spec-list nil
              new-dim-symbol-string-list  nil)
        ;;end t, cond
        ;;)
 

        ;;end n-dim > 1 cond
        ))



       #|       ;;REPLACE CURRENT DIM SPEC AFTER FIRST RUN
       (when  (> n-dim 1)
         (setf current-digit (car index-spec)
               incr-digit (second index-spec))
         (setf new-digit (+ current-digit incr-digit)
               new-spec (list new-digit incr-digit)
               new-index-specs (replace-list index-specs (- n-dim 1) new-spec)))

         ;;when want to restart indexe numbers with each dim
         (when restart-numbers-each-dim-p
           (setf  new-index-specs (replace-list new-index-specs 0 first-index-spec)))          

         ;;make the new new-index-spec-list to be used in loop below
         (setf new-index-spec-list (list var-root new-index-specs betw-index-str end-index-str))|#
       ;;MAKE EACH SYMBOL, ETC IN THE CURRENT DIM
#|      (loop
        for n-sym from 1 to  n-dim-elements
        do
        (setf new-dim-index-spec-list
              (append  new-dim-index-spec-list  (list new-index-spec-list)))

        ;;MAKE THE NEW-SYMBOL, ETC
        (multiple-value-setq (new-symbol new-index-spec-list new-symbol-string)
            (make-new-index-symbol new-index-spec-list))

        ;;(afout 'out (format nil "new-symbol= ~A~% new-index-spec-list= ~A~%  new-symbol-string= ~A~%" new-symbol new-index-spec-list new-symbol-string))

        (setf new-dim-symbols-list (append new-dim-symbols-list    (list new-symbol))
              new-dim-symbol-string-list 
              (append new-dim-symbol-string-list (list new-symbol-string)))

        ;;end inner loop
        )
       (setf new-index-spec-list-of-lists
             (append  new-index-spec-list-of-lists  (list new-dim-index-spec-list)))

       (setf new-symbols-list (append new-symbols-list (list new-dim-symbols-list))      
             new-symbol-string-list
             (append new-symbol-string-list (list new-dim-symbol-string-list)))

       ;;reinitialize the dim variables
       (setf new-dim-symbols-list nil
             new-dim-index-spec-list nil
             new-dim-symbol-string-list  nil)|#

       ;;end outer loop  n-dims   
       )
      (set new-symbol-type new-symbols-list)
      ;;end , make-sublists-for-each-dim-p = T, cond 
      
    (values new-symbol-type new-symbols-list new-index-spec-list-of-lists new-symbol-string-list)
    ))|#
;;TEST
;;  (make-new-index-symbols  '("wXX" ((5 1 1 "-")(1 1 0 ">") (3 1 1 "-")(1 2 0 ""))"test" "test"))



;; ART2 MOST REALISTIC TEST IS TESTNS4  BELOW:
#|(defun testns4 ()
  (setf out nil)
  ( make-new-index-symbols 5  '("wXX" ((1 1)(1 1)) "" "") :make-sublists-for-each-dim-p T  ))
  (testns4) |#
#| RETURNS= WXX
((WXX11 WXX21 WXX31 WXX41 WXX51) (WXX12 WXX22 WXX32 WXX42 WXX52))
((("wXX" ((1 1) (1 1)) "" "") ("wXX" ((2 1) (1 1)) "" "") ("wXX" ((3 1) (1 1)) "" "") ("wXX" ((4 1) (1 1)) "" "") ("wXX" ((5 1) (1 1)) "" "")) (("wXX" ((1 1) (2 1)) "" "") ("wXX" ((2 1) (2 1)) "" "") ("wXX" ((3 1) (2 1)) "" "") ("wXX" ((4 1) (2 1)) "" "") ("wXX" ((5 1) (2 1)) "" "")))
(("wXX11" "wXX21" "wXX31" "wXX41" "wXX51") ("wXX12" "wXX22" "wXX32" "wXX42" "wXX52"))|#
;; ( make-new-index-symbols 5  '("wXX" ((1 1)(1 1) (1 1)) "" "") :make-sublists-for-each-dim-p T  )



#|(defun testns2 ()
  (setf out nil)
  ( make-new-index-symbols 5  '("pre" ((0 1)(1 3)(2 5)) "-" "end")  ))|#
;;works, returns=>
#|PRE-END
(PRE0-1-2END PRE1-4-7END PRE2-7-12END PRE3-10-17END PRE4-13-22END)
(("pre" ((0 1) (1 3) (2 5)) "-" "end") ("pre" ((1 1) (4 3) (7 5)) "-" "end") ("pre" ((2 1) (7 3) (12 5)) "-" "end") ("pre" ((3 1) (10 3) (17 5)) "-" "end") ("pre" ((4 1) (13 3) (22 5)) "-" "end"))
("pre0-1-2end" "pre1-4-7end" "pre2-7-12end" "pre3-10-17end" "pre4-13-22end")
 pre-end returns=>
(PRE0-1-2END PRE1-4-7END PRE2-7-12END PRE3-10-17END PRE4-13-22END)
|#
#|(defun testns3 ()
  (setf out nil)
  ( make-new-index-symbols 5  '("pre" ((0 1)(1 3)(2 5)) "-" "end") :make-sublists-for-each-dim-p T  ))
  (testns3) |#
;;returns=> 
#|PRE-END
((PRE0-1-2END PRE1-4-7END PRE2-7-12END) (PRE3-10-17END PRE4-13-22END PRE5-16-27END) (PRE6-19-32END PRE7-22-37END PRE8-25-42END) (PRE9-28-47END PRE10-31-52END PRE11-34-57END) (PRE12-37-62END PRE13-40-67END PRE14-43-72END))
((("pre" ((0 1) (1 3) (2 5)) "-" "end") ("pre" ((1 1) (4 3) (7 5)) "-" "end") ("pre" ((2 1) (7 3) (12 5)) "-" "end")) (("pre" ((3 1) (10 3) (17 5)) "-" "end") ("pre" ((4 1) (13 3) (22 5)) "-" "end") ("pre" ((5 1) (16 3) (27 5)) "-" "end")) (("pre" ((6 1) (19 3) (32 5)) "-" "end") ("pre" ((7 1) (22 3) (37 5)) "-" "end") ("pre" ((8 1) (25 3) (42 5)) "-" "end")) (("pre" ((9 1) (28 3) (47 5)) "-" "end") ("pre" ((10 1) (31 3) (52 5)) "-" "end") ("pre" ((11 1) (34 3) (57 5)) "-" "end")) (("pre" ((12 1) (37 3) (62 5)) "-" "end") ("pre" ((13 1) (40 3) (67 5)) "-" "end") ("pre" ((14 1) (43 3) (72 5)) "-" "end")))
(("pre0-1-2end" "pre1-4-7end" "pre2-7-12end") ("pre3-10-17end" "pre4-13-22end" "pre5-16-27end") ("pre6-19-32end" "pre7-22-37end" "pre8-25-42end") ("pre9-28-47end" "pre10-31-52end" "pre11-34-57end") ("pre12-37-62end" "pre13-40-67end" "pre14-43-72end"))|#



;;MAKE-NEW-INDEX-SYMBOL
;;
;;ddd
#|(defun make-new-index-symbol ( symbol-index-spec-list ) 
  ;;&key   ;;end  betw-symbol)
  "In U-tstring.lisp, Makes ONE symbol. INDEX-SPEC-LIST=(symbol-root (sublist1 sublist2 etc)).  Each index sublist =  (num-elements  cur-index index-incr string).  RETURNS (values new-symbol index-spec-list new-symbol-string). "
  (cond
   ((listp symbol-index-spec-list)
    (let*
        ((symbol-root (car symbol-index-spec-list))
         (new-symbol)
         (index-spec-lists (second symbol-index-spec-list ))
         (cur-index)
         (cur-str)
         (new-symbol-string)
         )
        (loop
         for index-specs in  index-spec-lists
         for n from 1 to (length index-spec-lists)
         do
         (cond
          ((= n 1)   
           (setf cur-index (second index-specs)
                 cur-str (fourth index-specs)
                 new-symbol-string (format nil "~A~A~A" symbol-root cur-index cur-str)) )   
          (t
           (setf cur-index (second index-specs)
                 cur-str (fourth index-specs)
                 new-symbol-string (format nil "~A~A~A" new-symbol-string cur-index cur-str))
           ))
         ;;end loop
         )
        (setf new-symbol (my-make-symbol new-symbol-string))
      
      (values new-symbol symbol-index-spec-list new-symbol-string)
      ))
  ))
|#
;;TEST
;;  (make-new-index-symbol '("prefix" ((5 6 1 "-")(3 9 1 ">")(2 2 1 "-") (2 7 1 "end"))))
;; results= PREFIX6-9>2-7END  ("prefix" ((5 6 1 "-") (3 9 1 ">") (2 2 1 "-") (2 7 1 "end")))  "prefix6-9>2-7end"


;;xxx----------------- END REPLACED BUT MOSTLY WORK ----------------------------------




;;--------------------------- DELETE FOLLOWING ------------------------
;;  (testmst 5 3)
;; creates cell types, cells, and arrays for EACH CELL (not each cell type).
#|(defun testmst (nInputs nOutputs)
  (setf out nil)
  (let 
      ((symbol-spec-lists
        `((,nInputs ("Input" ((0 1)) "" ""))
          (,nInputs ("X-Activity" ((0 1)) "" ""))
          (,nInputs ("V" ((0 1)) "" ""))
          (,nInputs ("R" ((0 1)) "" ""))
          (,nInputs ("U" ((0 1)) "" ""))
          (,nInputs ("Q" ((0 1)) "" ""))
          (,nInputs ("P" ((0 1)) "" ""))
          ;;NOTE: FOR CONNECTIONS TO SECOND FIELD, THE NUMBER OF DIMS = N CELLS IN FIELD 2
          (,nInputs ("Wup" ((0 1)(1 0 )(2 0)) "" "")) ;;for 3 cells in field 2
          (,nInputs ("Wdn" ((0 1)(1 0)(2 0)) "" "")) 
          (,nOutputs ("Y-Outputs" ((0 1)) "" ""))
          ;;others
          (,nInputs ("Temp" ((0 1)) "" ""))
          (1 ("reset-Val" ((0 1)) "" ""))
          (,nOutputs ("reset" ((0 1)) "" ""))
          (,nOutputs ("reset-cnt" ((0 1)) "" ""))
          (,nOutputs ("n-cats" ((0 1)) "" ""))
          (,nOutputs ("Temp2" ((0 1)) "" ""))
          ))
       ;;end let vars
         )

  (make-new-dim-symbol-types  symbol-spec-lists)
  ))|#
;;works results=>
#|
 new-symbol-type-list  new-symbols-type-list-of-lists
            new-symbol-type-spec-list-of-lists 
            new-symbol-type-symbol-string-list-of-lists)
;;new-symbol-type-list=
(INPUTS X-ACTIVITY V R U Q P WUP WDN Y-OUTPUTS TEMP RESET-VAL RESET RESET-CNT N-CATS TEMP2)
;;;; new-symbols-type-list-of-lists= 
((INPUTS0 INPUTS1 INPUTS2 INPUTS3 INPUTS4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP10 WUP20 WUP30 WUP40) (WDN01 WDN11 WDN21 WDN31 WDN41) (Y-OUTPUTS0 Y-OUTPUTS1 Y-OUTPUTS2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2) (RESET-CNT0 RESET-CNT1 RESET-CNT2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22))
;; new-symbol-type-spec-list-of-lists= 
((("Inputs" ((0 1)) "" "") ("Inputs" ((1 1)) "" "") ("Inputs" ((2 1)) "" "") ("Inputs" ((3 1)) "" "") ("Inputs" ((4 1)) "" "")) (("X-Activity" ((0 1)) "" "") ("X-Activity" ((1 1)) "" "") ("X-Activity" ((2 1)) "" "") ("X-Activity" ((3 1)) "" "") ("X-Activity" ((4 1)) "" "")) (("V" ((0 1)) "" "") ("V" ((1 1)) "" "") ("V" ((2 1)) "" "") ("V" ((3 1)) "" "") ("V" ((4 1)) "" "")) (("R" ((0 1)) "" "") ("R" ((1 1)) "" "") ("R" ((2 1)) "" "") ("R" ((3 1)) "" "") ("R" ((4 1)) "" "")) (("U" ((0 1)) "" "") ("U" ((1 1)) "" "") ("U" ((2 1)) "" "") ("U" ((3 1)) "" "") ("U" ((4 1)) "" "")) (("Q" ((0 1)) "" "") ("Q" ((1 1)) "" "") ("Q" ((2 1)) "" "") ("Q" ((3 1)) "" "") ("Q" ((4 1)) "" "")) (("P" ((0 1)) "" "") ("P" ((1 1)) "" "") ("P" ((2 1)) "" "") ("P" ((3 1)) "" "") ("P" ((4 1)) "" "")) (("Wup" ((0 1) (0 0)) "" "") ("Wup" ((1 1) (0 0)) "" "") ("Wup" ((2 1) (0 0)) "" "") ("Wup" ((3 1) (0 0)) "" "") ("Wup" ((4 1) (0 0)) "" "")) (("Wdn" ((0 1) (1 0)) "" "") ("Wdn" ((1 1) (1 0)) "" "") ("Wdn" ((2 1) (1 0)) "" "") ("Wdn" ((3 1) (1 0)) "" "") ("Wdn" ((4 1) (1 0)) "" "")) (("Y-Outputs" ((0 1)) "" "") ("Y-Outputs" ((1 1)) "" "") ("Y-Outputs" ((2 1)) "" "")) (("Temp" ((0 1)) "" "") ("Temp" ((1 1)) "" "") ("Temp" ((2 1)) "" "") ("Temp" ((3 1)) "" "") ("Temp" ((4 1)) "" "")) (("reset-Val" ((0 1)) "" "")) (("reset" ((0 1)) "" "") ("reset" ((1 1)) "" "") ("reset" ((2 1)) "" "")) (("reset-cnt" ((0 1)) "" "") ("reset-cnt" ((1 1)) "" "") ("reset-cnt" ((2 1)) "" "")) (("n-cats" ((0 1)) "" "") ("n-cats" ((1 1)) "" "") ("n-cats" ((2 1)) "" "")) (("Temp2" ((0 1)) "" "") ("Temp2" ((1 1)) "" "") ("Temp2" ((2 1)) "" "")))
;;new-symbol-type-symbol-string-list-of-lists=
(("Inputs0" "Inputs1" "Inputs2" "Inputs3" "Inputs4") ("X-Activity0" "X-Activity1" "X-Activity2" "X-Activity3" "X-Activity4") ("V0" "V1" "V2" "V3" "V4") ("R0" "R1" "R2" "R3" "R4") ("U0" "U1" "U2" "U3" "U4") ("Q0" "Q1" "Q2" "Q3" "Q4") ("P0" "P1" "P2" "P3" "P4") ("Wup00" "Wup10" "Wup20" "Wup30" "Wup40") ("Wdn01" "Wdn11" "Wdn21" "Wdn31" "Wdn41") ("Y-Outputs0" "Y-Outputs1" "Y-Outputs2") ("Temp0" "Temp1" "Temp2" "Temp3" "Temp4") ("reset-Val0") ("reset0" "reset1" "reset2") ("reset-cnt0" "reset-cnt1" "reset-cnt2") ("n-cats0" "n-cats1" "n-cats2") ("Temp20" "Temp21" "Temp22"))
|#



#| SOMETHING IN THESE DOESN'T WORK  -------- DELETE? ---------------
(defun make-multi-dim-sequence-list  (root all-dims-spec-list &key (recurse-flag 99)
                                           (unbind-global-vars-p T) (return-flat-lists-p T) 
                                           (node-separator "To") )
 "In U-tstring. Unless make-list-p, returns a list of new sequences of root,begin-str,dim-element,end-str for each value of dim-element from begin-n to n-dim-elements. RETURNS (values  new-seq-nested-lists  new-symbol-nested-lists new-dim-list-nested-lists new-seq-flat-list new-symbol-flat-list).  ALL-DIMS-SPEC-LIST (sublist1 sublist2 etc).  Each dim sublist =  (n-elements begin-n/or/cur-dim-n  dim-incr  begin-str end-str. If unbind-global-vars-p, then unbinds global vars needed to return right lists. If return-flat-lists-p, then returns unnested lists instead of nested ones. NOTE: Nested-lists are nested by all Dim1 items together.  Use function resort-nested-lists for 2-level/2-dim nested lists with all Dim2 items in same list."
   ;;note the global vars are used ONLY in the called functions, initialized by defparameters
    (setf   *new-seq-nested-lists nil  *new-symbol-nested-lists nil  
            *new-dim-list-nested-lists nil
            *append-node-sep-list nil  ) ;; *new-seq-flat-list nil)
    (let
        ((new-seq-nested-lists)
         (new-symbol-nested-lists)
         (new-dim-list-nested-lists)
         (new-seq-flat-list)
         (new-symbol-flat-list)
         (return-lists '( *new-seq-nested-lists  *new-symbol-nested-lists *new-dim-list-nested-lists))  ;; *new-seq-flat-list))
         )
  ;;CALL THE MAIN WORK FUNCTION          
   (multiple-value-setq (new-seq-nested-lists  new-symbol-nested-lists new-dim-list-nested-lists) ;;  new-seq-flat-list)
       (make-multi-dim-sequence-list1  root  all-dims-spec-list  :recurse-flag recurse-flag 
                                   :return-flat-lists-p return-flat-lists-p
                                   :node-separator node-separator))

   ;;set the symbol-vals done in bottom-function? (set-sym-to-root+dims new-symbol-nested-lists new-sym-vals-list) 
   
   (when unbind-global-vars-p
     (loop
      for list in return-lists
      do    
     (makunbound list)
     ;;end loop, when
     )
     ;;needed bec makunbound requires to be re-declared as special
     (setf  *new-seq-nested-lists nil  
             *new-symbol-nested-lists nil *new-dim-list-nested-lists nil) ;; *new-seq-flat-list nil)
     )

   ;;make  flat symbol and symbol-string lists?
   (when return-flat-lists-p
    (loop
     for strlist in new-seq-nested-lists
     do
           (when (and (listp strlist) (= (list-length strlist) 1))
             (setf new-seq-flat-list (append new-seq-flat-list strlist)))
           ;;end loop, when
         ) 
    (loop
     for symlist in new-symbol-nested-lists
     do
           (when (and (listp symlist) (= (list-length symlist) 1))
             (setf new-symbol-flat-list (append new-symbol-flat-list symlist)))
           ;;end loop, when
         ))

   (values  new-seq-nested-lists  new-symbol-nested-lists new-dim-list-nested-lists new-seq-flat-list new-symbol-flat-list)
   ;;end when, make-multi-dim-sequence-list
   ))

(defun make-new-dim-symbol-types (symbol-spec-lists
                                    &key make-sublists-for-each-dim-p
                                    (set-global-vars-p T)  (return-flat-lists-p T)
                                    (path-indicator-string "To") nested-inner-list-size)
 "In U-tstring.  Returns new sequences of root, begin-str,dim-element,end-str for each value of dim-element from begin-n to n-dim-elements. RETURNS (values new-symbol-type-list  new-symbols-type-list-of-lists new-symbol-type-spec-list-of-lists  new-root-list   new-symbol-type-symbol-string-list-of-lists all-new-sym-names-list).  INPUTS:  For SYMBOL-SPEC-LISTS, EACH symbol-spec-list= (ROOT all-dims-spec-list). ALL-DIMS-SPEC-LIST= (sublist1 sublist2 etc).  Each dim sublist =  (n-elements begin-n/or/cur-dim-n  dim-incr  begin-str end-str. Eg. (\"root\" '((4 1 1 \"C\" \"F\")(3 1 1 \"C\" \"F\"))).
KEYS: If set-global-vars-p, sets global * versions of all return vars.  If return-flat-lists-p, then returns unnested lists instead of nested ones. NOTE: Nested-lists are nested by all Dim1 items together.  Use function resort-nested-lists for 2-level/2-dim nested lists with all Dim2 items in same list."
 (when set-global-vars-p
  (setf   *new-symbol-type-list nil  *new-symbols-type-list-of-lists  nil
            *new-symbol-type-spec-list-of-lists nil  *new-root-list nil
            *new-symbol-type-symbol-string-list-of-lists nil ))
  (let*    
      ((n-symbol-types  (length symbol-spec-lists))
      ;; (n-symbols 1)
      ;; (dim-i-end)
      ;; (dim-j-end)
       ;;outputs
       (root-sym)
       (new-symbols-list)
       (new-symbol-type)
       (new-symbol-type-list)
       (new-sym-names-flat-list)  ;;new
       (new-symbols-flat-list) ;;new
       (all-new-sym-names-flat-list)  ;;new
       (all-new-symbols-flat-list) ;;new
       (new-symbol-type-spec-list)
       (new-symbol-type-symbol-string-list)
       (new-symbols-type-list-of-lists)
       (new-symbol-type-spec-list-of-lists)
       (new-symbol-type-symbol-string-list-of-lists)       
       (new-root-list)
       (inner-list)
       (outer-list)
       )
    ;;(afout 'out (format nil "2 symbol-spec-lists= ~A~%" symbol-spec-lists))
    (loop
     for symbol-spec-list in symbol-spec-lists
     ;;for dim-spec-list in symbol-spec-lists
     ;;for symbol-n from 1 to  n-symbol-types
     ;;    with n-symbols = (car dim-spec-list)
     do
     (setf root (car symbol-spec-list)
           all-dims-spec-list (second symbol-spec-list)
           root-sym (my-make-symbol root)
           dimspecs-n 0)
     ;;(afout 'out (format nil "dim-spec-list= ~A~%" dim-spec-list))
     ;;eg SingleDim (,nInputs ("Input" ((1 1)) "" ""))
     ;;eg DoubleDim (,nOutputs ("Wup" ((1 1)(1 1)) "" ""))

     ;;added 2015-09
       ;;all-dims-spec-list=  ((3 1 1"")(1 3 1  "-")(1 1 1 "-"  ) (3  1 1  "To" )(1 1 1   "-")(1 2 1 "-"))
       ;;dimspecs= (3 1 1"")  or (1 3 1  "-") or  (3  1 1  "To" )

      ;;If path-indicator-string, find nested-inner-list-size
     (when path-indicator-string
       (dolist (dimspecs all-dims-spec-list)
         (incf dimspecs-n) ;;first = 1
         (cond 
          ;;if  "To" is begin-str, uses N of same dimspecs
          ((string-equal (fourth dimspecs) path-indicator-string)
           (setf nested-inner-list-size (car dimspecs)))
          ;;if  "To" is end-str, uses N of NEXT dimspecs
          ((string-equal (fifth dimspecs) path-indicator-string)
           (setf nested-inner-list-size (car (nth dimspecs-n dimspecs))))   ;;next list      
        (t  NIL ))  ;;was (setf nested-inner-list-size nil)))
       ;;end  dolist,when
       ))

     ;;SSS FIX new-seq-nested-lists RETURNING NIL
     ;;FIND THE NEW SYMBOLS, ETC
     (multiple-value-setq (new-seq-nested-lists  new-symbol-nested-lists new-dim-list-nested-lists  new-sym-names-flat-list new-symbols-flat-list)
         (make-multi-dim-sequence-list  root  all-dims-spec-list
                                        :return-flat-lists-p  return-flat-lists-p))
        ;;old (make-new-dim-symbols  n-symbols (second dim-spec-list)
          ;;                        :make-sublists-for-each-dim-p make-sublists-for-each-dim-p))

     (setf  new-root-list (append new-root-list (list root))
           new-symbol-type-list (append new-symbol-type-list 
                                         (list root-sym))            
           new-symbols-type-list-of-lists (append new-symbols-type-list-of-lists
                                                  (list new-symbol-nested-lists))
           new-symbol-type-spec-list-of-lists (append new-symbol-type-spec-list-of-lists
                                                      (list new-dim-list-nested-lists))
           new-symbol-type-symbol-string-list-of-lists 
                (append new-symbol-type-symbol-string-list-of-lists 
                      (list new-seq-nested-lists))
           all-new-sym-names-flat-list (append all-new-sym-names-flat-list 
                                           (list new-sym-names-flat-list))
           all-new-symbols-flat-list  (append all-new-symbols-flat-list
                                            (list new-symbols-flat-list)))

   ;;sss
;; (values new-symbol-type-list  Xnew-symbols-type-list-of-lists             new-symbol-type-spec-list-of-lists  new-root-list             Xnew-symbol-type-symbol-string-list-of-lists  all-new-sym-names-list)       

     ;;added 2015-09 may be elsewhere also??
     ;;make a flat list if list is eg. ((WUP1-3-2TO1-1-3) (WUP1-3-2TO2-1-3) (WUP2-3-2TO1-1-3) (WUP2-3-2TO2-1-3) (WUP3-3-2TO1-1-3) (WUP3-3-2TO2-1-3))
#|     (dolist (item  new-symbol-nested-lists)
       (cond 
        ((listp item)
         (setf new-symbol-flat-list (append new-symbol-flat-list  item)))
        (t setf  new-symbol-flat-list (append new-symbol-flat-list (list item))))
       ;;end dolist
       )|#
     ;;make a nested list 
     (when  nested-inner-list-size
       (loop
        for item in new-symbols-flat-list
        do
        (loop
         for n from 1 to nested-inner-list-size
         do
         (setf inner-list (append inner-list (list item)))
         ;;end inner-loop
         )
        (setf outer-list (append outer-list (list inner-list)))
        ;;reset inner-list
        (setf inner-list nil)
        ;;end outer loop
        )         
       (setf new-symbol-nested-lists outer-list)
       ;;end when
       )

     ;;(break)
     ;;SET NEW SYMBOLS TO LISTS 
     (cond
     ;;If want to manually control the nested-inner-list-size
      (nested-inner-list-size
       (set (my-make-symbol (format nil "~A-flat" root))   new-symbols-flat-list)
       (set  root-sym  new-symbol-nested-lists))
      (t
       (set  root-sym  new-symbols-flat-list)
       (set (my-make-symbol (format nil "~A-flat" root))  new-symbols-flat-list)))

     ;;end BIG OUTER symbol-spec-lists loop
     )
    
    (when set-global-vars-p 
      (setf *new-symbol-type-list new-symbol-type-list
            *new-symbols-type-list-of-lists  new-symbols-type-list-of-lists
            *new-symbol-type-spec-list-of-lists  new-symbol-type-spec-list-of-lists
            *new-root-list new-root-list
            *new-symbol-type-symbol-string-list-of-lists 
            new-symbol-type-symbol-string-list-of-lists))

    (values new-symbol-type-list  new-symbols-type-list-of-lists
            new-symbol-type-spec-list-of-lists  new-root-list
            new-symbol-type-symbol-string-list-of-lists  all-new-sym-names-flat-list 
            all-new-symbols-flat-list)
    ;;end let, make-new-dim-symbol-types
    ))


(defun make-dim-sequence-list (root n-dim-elements &key sym-vals-list
                                    (begin-n 1)(dim-incr 1)
                                          (begin-str "") (end-str "")
                                          dim-elements-list  (make-string-p T) (make-list-p T)
                                           set-sym-to-vals-p
                                          (make-symbol-p T)  append-node-sep-list                                      
                                          )
  "In  Unless make-list-p, returns a list of new sequences of root,begin-str,dim-element,end-str for each value of dim-element (either begin-n to n-dim-elements or from the list dim-elements-list. RETURNS (values new-seq-list new-symbol-list new-dim-list-list ).  If set-sym-to-vals-p, then sets the new-symbol = (init-root dims-list n-dim-elements), eg. (\"Wup\" (2 5) 7)."
  (let*
      ((cur-dim-element)
       ;SSS IS THIS RIGHT??
       (end-n (floor (/  (+ (- n-dim-elements 1) begin-n) dim-incr)))
    ;;was (floor (/ (+ (- n-dim-elements begin-n) 1) dim-incr)) couldn't begin with other than 1
       (dim-n)
       (pre-seq)
       (new-seq)
       (new-dim-list)
       (new-symbol)
       (symbol-vals)  ;; (initial-root dim-list n-elements value(to be added))
       (return-sym-vals-list)
       (new-seq-list)
       (new-symbol-list)
       (new-dim-list-list)
       (node-sep-list)
       )
    ;;(floor (/  (+ (- n-dim-elements 1) begin-n) dim-incr))))
    ;;test for above
   ;; (+ (- n-dim-elements 1) begin-n)
   ;;  (+ (- 1 1) 3) = 3 ;  (+ (- 3 1)  1)
  ;;(end-n  (/ (+  (- 1 1)  3) 1)) = 3;; (/ (+  (- 2 1)  3) 2)
;;  

    (if (null begin-str) (setf begin-str ""))
    (if (null end-str) (setf end-str ""))

    (loop
     for n from begin-n to end-n
     do
     (setf dim-end (* n dim-incr))
     
     (cond
      (dim-elements-list (setf dim-element (nth n dim-elements-list)))
      (t (setf dim-element n)))

     (when make-string-p      ;;was (or make-string-p  set-sym-to-list-p)
       (setf new-seq (format nil "~A~A~A~A" root  begin-str dim-element end-str))
       (when  (or make-symbol-p  set-sym-to-list-p)
         (setf new-symbol (my-make-symbol new-seq)))
       (setf new-seq-list (append new-seq-list (list new-seq))
             new-symbol-list (append new-symbol-list (list new-symbol))))

       ;;for recursion?
     (when  make-list-p 
       (setf new-dim-list 
             (list new-seq (list n-dim-elements dim-element dim-incr begin-str end-str))
             new-dim-list-list (append new-dim-list-list (list new-dim-list))))
       
     ;;setting the symbol to sym-vals ONLY WHEN LAST DIM.
     (when set-sym-to-vals-p
       (multiple-value-bind (dim-list token-list alpha-list)
           (find-integers-in-string new-seq)
         ;;added
         (cond
          (*append-node-sep-list
           (setf sym-vals (append (list (car alpha-list) dim-list  n-dim-elements nil ) 
                         *append-node-sep-list)))
          (t (setf sym-vals (list (car alpha-list) dim-list  n-dim-elements))))
       ;;was (setf sym-vals (list (car alpha-list) dim-list  n-dim-elements))

       ;;SETS THE NEW-SYMBOL TO SYM-VALS
       (set new-symbol sym-vals)))
     ;;end loop
     )
    
    (values new-seq-list new-symbol-list new-dim-list-list)
    ;;end let, make-dim-sequence-list      
    ))


;;THIS ONE OK??
(defun make-multi-dim-sequence-list1  (root all-dims-spec-list &key  (recurse-flag 99)
                                            return-flat-lists-p)
  "In  Unless make-list-p, returns a list of new sequences of root,begin-str,dim-element,end-str for each value of dim-element (either begin-n to n-dim-elements or from the list dim-elements-list. RETURNS (values new-seq-nested-lists new-symbol-nested-lists new-dim-list-nested-lists ).  ALL-DIMS-SPEC-LIST (sublist1 sublist2 etc).  Each dim sublist =  (n-elements cur-dim-n  dim-incr  begin-str end-str)."
  (let*
      ((dim-sublist )
       (n-elements)
       (cur-dim-n)
       (dim-incr)
       (begin-str)
       (end-str)
       (dim-spec-list)
       (dim-subspec-list)
       (new-root-list)
       (new-seq-list)
       (new-symbol-list)
       (new-dim-list-list)
       ;;done here bec only set on last dim
       (set-sym-to-vals-p)
      ;; (new-seq-nested-lists)
     ;;  (new-symbol-nested-lists)
      ;;(new-dim-list-nested-lists)
       (new-all-dims-spec-list)
      ;; (test-list)
       )
     ;;COND TO STOP THE RECURSION
     (cond
      ((or (= recurse-flag 0) (null all-dims-spec-list))
       NIL)
      ;;((= recurse-flag 99)
      (t
       ;;get the dim parameters
       (setf dim-spec-list (car all-dims-spec-list))             
       (multiple-value-setq (n-elements cur-dim-n dim-incr begin-str end-str)
           (values-list dim-spec-list))

       ;;on last cycle, set the symbol to the sym-vals  (sym-to-vals-p= t)
       (when (= (length all-dims-spec-list) 1)
         (setf set-sym-to-vals-p T))

       ;;ADD THE DIM TO THE ROOT AND GET NESTED LISTS
       (multiple-value-setq (new-seq-list new-symbol-list new-dim-list-list)
           (make-dim-sequence-list root  n-elements :begin-n cur-dim-n 
                                   :set-sym-to-vals-p set-sym-to-vals-p
                                   :dim-incr dim-incr  :begin-str begin-str :end-str end-str))

       ;;make lists for new recursion--don't append old because want only latest version??
       (setf new-all-dims-spec-list (cdr all-dims-spec-list)
             new-root-list new-seq-list)

       ;;SSS START HERE,  HOW TO GET THIS TO TOP WITH A NON-GLOBAL???
       ;; COULD I USE CATCH--THROW ETC.
       (cond
        #|(return-flat-lists-p
         (when (<  (length new-all-dims-spec-list) 1)
           (setf  *new-seq-nested-lists (append *new-seq-nested-lists new-seq-list)
                  *new-symbol-nested-lists  (append *new-symbol-nested-lists new-symbol-list )
                  *new-dim-list-nested-lists (append *new-dim-list-nested-lists new-dim-list-list))
           ;;added 2015-09
           (when (and (listp (car new-seq-list)) (= (list-length (car new-seq-list) 1)))
             (setf *new-seq-flat-list (append *new-seq-flat-list (caar new-seq-list))))
         ) )|#
        ;;return nested lists
        (t
         (when (<  (length new-all-dims-spec-list) 1)
           (setf  *new-seq-nested-lists (append *new-seq-nested-lists (list new-seq-list))
                  *new-symbol-nested-lists  (append *new-symbol-nested-lists (list new-symbol-list ))
                  *new-dim-list-nested-lists (append *new-dim-list-nested-lists (list new-dim-list-list))))))

       ;;(afout 'out (format nil "AT #1 new-all-dims-spec-list= ~A~% *new-seq-nested-lists= ~A~%new-root-list= ~A~%"  new-all-dims-spec-list    *new-seq-nested-lists new-root-list ))
       ;;end recurse-flag 99
       )
      (t nil))


     ;; IF  new-all-dims-spec-list = nil, THEN SKIP REST AND RETURN  LAST LISTS FROM ABOVE?
     (cond
      ((> (setf recurse-flag (length new-all-dims-spec-list))  0)

       ;;RECURSE CALL ON CDR OF ORIGINAL ALL-DIMS-SPEC-LIST FOR EACH NEW ROOT IN new-seq-list   
       (loop
        for root in new-root-list
        do                             

        (multiple-value-setq (seq-nested-lists1   symbol-nested-lists1
                                                  dim-list-nested-lists1)
            (make-multi-dim-sequence-list1 root  new-all-dims-spec-list
                                          :recurse-flag recurse-flag))

        ;;end loop root
        )
       ;;end recurse-flag > 0
       )
      ;;if  recurse-flag = 0 no recurse
      (t nil))
       (values *new-seq-nested-lists   *new-symbol-nested-lists   *new-dim-list-nested-lists  *new-seq-nested-lists recurse-flag) ;;  *new-seq-flat-list)
       
       ;;end let, make-multi-dim-sequence-list
       ))

;;older version?
#|(defun make-new-dim-symbol-types (symbol-spec-lists
                                    &key make-sublists-for-each-dim-p
                                    (set-global-vars-p T)  (return-flat-lists-p T)
                                    (path-indicator-string "To") nested-inner-list-size)
 "In U-tstring.  Returns new sequences of root, begin-str,dim-element,end-str for each value of dim-element from begin-n to n-dim-elements. RETURNS (values new-symbol-type-list  new-symbols-type-list-of-lists new-symbol-type-spec-list-of-lists  new-root-list   new-symbol-type-symbol-string-list-of-lists all-new-sym-names-list).  INPUTS:  For SYMBOL-SPEC-LISTS, EACH symbol-spec-list= (ROOT all-dims-spec-list). ALL-DIMS-SPEC-LIST= (sublist1 sublist2 etc).  Each dim sublist =  (n-elements begin-n/or/cur-dim-n  dim-incr  begin-str end-str. Eg. (\"root\" '((4 1 1 \"C\" \"F\")(3 1 1 \"C\" \"F\"))).
KEYS: If set-global-vars-p, sets global * versions of all return vars.  If return-flat-lists-p, then returns unnested lists instead of nested ones. NOTE: Nested-lists are nested by all Dim1 items together.  Use function resort-nested-lists for 2-level/2-dim nested lists with all Dim2 items in same list."
 (when set-global-vars-p
  (setf   *new-symbol-type-list nil  *new-symbols-type-list-of-lists  nil
            *new-symbol-type-spec-list-of-lists nil  *new-root-list nil
            *new-symbol-type-symbol-string-list-of-lists nil ))
  (let*    
      ((n-symbol-types  (length symbol-spec-lists))
      ;; (n-symbols 1)
      ;; (dim-i-end)
      ;; (dim-j-end)
       ;;outputs
       (root-sym)
       (new-symbols-list)
       (new-symbol-type)
       (new-symbol-type-list)
       (new-symbol-flat-list)
       (new-sym-names-flat-list)  ;;new
       (all-new-sym-names-list)  ;;new
       (new-symbol-type-spec-list)
       (new-symbol-type-symbol-string-list)
       (new-symbols-type-list-of-lists)
       (new-symbol-type-spec-list-of-lists)
       (new-symbol-type-symbol-string-list-of-lists)       
       (new-root-list)
       (inner-list)
       (outer-list)
       )
    ;;(afout 'out (format nil "2 symbol-spec-lists= ~A~%" symbol-spec-lists))
    (loop
     for symbol-spec-list in symbol-spec-lists
     ;;for dim-spec-list in symbol-spec-lists
     ;;for symbol-n from 1 to  n-symbol-types
     ;;    with n-symbols = (car dim-spec-list)
     do
     (setf root (car symbol-spec-list)
           all-dims-spec-list (second symbol-spec-list)
           root-sym (my-make-symbol root)
           dimspecs-n 0)
     ;;(afout 'out (format nil "dim-spec-list= ~A~%" dim-spec-list))
     ;;eg SingleDim (,nInputs ("Input" ((1 1)) "" ""))
     ;;eg DoubleDim (,nOutputs ("Wup" ((1 1)(1 1)) "" ""))

     ;;added 2015-09
       ;;all-dims-spec-list=  ((3 1 1"")(1 3 1  "-")(1 1 1 "-"  ) (3  1 1  "To" )(1 1 1   "-")(1 2 1 "-"))
       ;;dimspecs= (3 1 1"")  or (1 3 1  "-") or  (3  1 1  "To" )

      ;;If path-indicator-string, find nested-inner-list-size
     (when path-indicator-string
       (dolist (dimspecs all-dims-spec-list)
         (incf dimspecs-n) ;;first = 1
         (cond 
          ;;if  "To" is begin-str, uses N of same dimspecs
          ((string-equal (fourth dimspecs) path-indicator-string)
           (setf nested-inner-list-size (car dimspecs)))
          ;;if  "To" is end-str, uses N of NEXT dimspecs
          ((string-equal (fifth dimspecs) path-indicator-string)
           (setf nested-inner-list-size (car (nth dimspecs-n dimspecs))))   ;;next list      
        (t  NIL ))  ;;was (setf nested-inner-list-size nil)))
       ;;end  dolist,when
       ))

     ;;SS FIX new-seq-nested-lists RETURNING NIL
     ;;FIND THE NEW SYMBOLS, ETC
     (multiple-value-setq (new-seq-nested-lists  new-symbol-nested-lists new-dim-list-nested-lists  new-sym-names-flat-list)
;;was (new-symbol-type new-symbols-list new-symbol-type-spec-list  new-symbol-type-symbol-string-list)
         (make-multi-dim-sequence-list  root  all-dims-spec-list
                                        :return-flat-lists-p  return-flat-lists-p))
        ;;old (make-new-dim-symbols  n-symbols (second dim-spec-list)
          ;;                        :make-sublists-for-each-dim-p make-sublists-for-each-dim-p))

     (setf  new-root-list (append new-root-list (list root))
           new-symbol-type-list (append new-symbol-type-list 
                                         (list root-sym))            
           new-symbols-type-list-of-lists (append new-symbols-type-list-of-lists
                                                  (list new-symbol-nested-lists))
           new-symbol-type-spec-list-of-lists (append new-symbol-type-spec-list-of-lists
                                                      (list new-dim-list-nested-lists))
           new-symbol-type-symbol-string-list-of-lists 
                (append new-symbol-type-symbol-string-list-of-lists 
                     (list new-seq-nested-lists))
                ;;SS fix
           all-new-sym-names-list (append all-new-sym-names-list 
                                          (list new-sym-names-flat-list)))
           

     ;;added 2015-09 may be elsewhere also??
     ;;make a flat list if lis is eg. ((WUP1-3-2TO1-1-3) (WUP1-3-2TO2-1-3) (WUP2-3-2TO1-1-3) (WUP2-3-2TO2-1-3) (WUP3-3-2TO1-1-3) (WUP3-3-2TO2-1-3))
     (dolist (item  new-symbol-nested-lists)
       (cond 
        ((listp item)
         (setf new-symbol-flat-list (append new-symbol-flat-list  item)))
        (t setf  new-symbol-flat-list (append new-symbol-flat-list (list item))))
       ;;end dolist
       )
     ;;make a nested list 
     (when  nested-inner-list-size
       (loop
        for item in new-symbol-flat-list
        do
        (loop
         for n from 1 to nested-inner-list-size
         do
         (setf inner-list (append inner-list (list item)))
         ;;end inner-loop
         )
        (setf outer-list (append outer-list (list inner-list)))
        ;;reset inner-list
        (setf inner-list nil)
        ;;end outer loop
        )         
       (setf new-symbol-nested-lists outer-list)
       ;;end when
       )

     ;;(break)
     ;;SET NEW SYMBOLS TO LISTS 
     (cond
     ;;If want to manually control the nested-inner-list-size
      (nested-inner-list-size
       (set (my-make-symbol (format nil "~A-flat" root))   new-symbol-flat-list)
       (set  root-sym  new-symbol-nested-lists))
      (t
       (set  root-sym  new-symbol-flat-list)
       (set (my-make-symbol (format nil "~A-flat" root))  new-symbol-flat-list)))

     ;;end BIG OUTER symbol-spec-lists loop
     )
    
    (when set-global-vars-p 
      (setf *new-symbol-type-list new-symbol-type-list
            *new-symbols-type-list-of-lists  new-symbols-type-list-of-lists
            *new-symbol-type-spec-list-of-lists  new-symbol-type-spec-list-of-lists
            *new-root-list new-root-list
            *new-symbol-type-symbol-string-list-of-lists 
            new-symbol-type-symbol-string-list-of-lists))

    (values new-symbol-type-list  new-symbols-type-list-of-lists
            new-symbol-type-spec-list-of-lists  new-root-list
            new-symbol-type-symbol-string-list-of-lists  all-new-sym-names-list)
    ;;end let, make-new-dim-symbol-types
    ))|#




|#
;; --------------------------------------- end delete ----------------------------------