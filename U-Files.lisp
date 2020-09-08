;; ******************************* U-Files.lisp ***********************************
;; *****>> SEE FILE END FOR FILE-NAMESTRING AND PATHNAME functions
;;
;; 
;;

#|
CL LISP FUNCTIONS

NAMESTRING CONVERTS PATH TO STRING 
(eg (namestring #P"C:/3-TS/LISP PROJECTS TS/ART3/ART3-simpleReset.lisp")
;;= "C:\\3-TS\\LISP PROJECTS TS\\ART3\\ART3-simpleReset.lisp")
directory-namestring
file-namestring
host-namestring

logical-pathname
logical-pathname-translations
parse-namestring
PATHNAME CONVERTS STRINGS OR STREAMS TO PATHNAMES
pathname-device
pathname-directory
pathname-host
pathname-match-p
pathname-name:  (pathname-name "c:\\temp\\apple farm.jpg") = "apple farm"
pathname-type
pathname-version
pathnamep
translate-logical-pathname
translate-pathname
wild-pathname-p

delete-file
file-author
file-error
file-error-pathname
file-length
file-namestring
file-position
file-stream
file-string-length
file-write-date
probe-file
rename-file

FILE-AUTHOR
file-error
file-error-pathname
FILE-LENGTH

;;NAMESTRINGS
NAMESTRING returns string of  real pathname
file-namestring  (file-namestring  "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp") = "U-lists.lisp"
directory-namestring (directory-namestring  "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp") = "/3-TS/LISP PROJECTS TS/MyUtilities/"
host-namestring  (host-namestring  "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp") = "C"
enough-namestring   (enough-namestring  "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp")) = "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp"

;;SIMILAR FOR PATHNAMES
pathname-host pathname &key case => host
pathname-device pathname &key case => device
pathname-directory pathname &key case => directory
pathname-name pathname &key case => name
pathname-type pathname &key case => type
pathname-version pathname => version
;;EXAMPLES
 (setq q (make-pathname :host "KATHY"  :directory "CHAPMAN" :name "LOGIN" :type "COM"))
=> #P"//KATHY/CHAPMAN/LOGIN.COM"
 (pathname-host q) =>  "KATHY"
 (pathname-name q) =>  "LOGIN"
 (pathname-type q) =>  "COM"
 (pathname-directory q) => (:ABSOLUTE "CHAPMAN")
;;using pathname and subdirs
also (setf qqp (PATHNAME "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp")) = #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp"
 (pathname-directory qqp) = (:ABSOLUTE "3-TS" "LISP PROJECTS TS" "MyUtilities")

file-position
file-stream
file-string-length
FILE-WRITE-DATE

PARSE-NAMESTRING thing &optional host default-pathname &key start end junk-allowed
=> pathname, position
[CONVERTS THING TO REAL PATHNAME]
 EG (parse-namestring "C:\3-TS\LISP PROJECTS TS\MyUtilities/U-lists.lisp")
  RESULT= #P"C:3-TSLISP PROJECTS TSMyUtilities/U-lists.lisp"   49
thing---a string, a pathname, or a stream associated with a file.
host---a valid pathname host, a logical host, or nil.
default-pathname---a pathname designator. The default is the value of *default-pathname-defaults*.
junk-allowed---a generalized boolean. The default is false.
* If host is a logical host then thing is parsed as a logical pathname namestring on the host.
* If host is nil and thing is a syntactically valid logical pathname namestring containing an explicit host, then it is parsed as a logical pathname namestring.
* If host is nil, default-pathname is a logical pathname, and thing is a syntactically valid logical pathname namestring without an explicit host, then it is parsed as a logical pathname namestring on the host that is the host component of default-pathname.

user-homedir-pathname
|#


;; *******> BASIC FILE OPERATIONS, READ, WRITE, + SAVE and LOAD A Database ********
;;;
#|
ARGUMENT KEYWORDS FOR OPEN WITH-OPEN-FILE ETC.
DIRECTION ---one of :input, :output, :io, or :probe. The default is :input.

ELEMENT-TYPE ---a type specifier for recognizable subtype of character; or a type specifier for a finite recognizable subtype of integer; or one of the symbols signed-byte, unsigned-byte, or :default. The default is character.

IF-EXISTS ---one of :error, :new-version, :rename, :rename-and-delete, :OVERWRITE, :APPEND, :supersede, or nil. The DEFAULT IS :NEW-VERSION if the version component of filespec is :newest, or :error otherwise.

IF-DOES-NOT-EXIST ---one of :error, :CREATE, or nil.
 The default is :error 
if direction is :input or if-exists is :overwrite or :append; :CREATE IF DIRECTION is
 :output or :io, and if-exists is neither :overwrite nor :append; or nil when direction is :probe. 
|#


;;XXX =============FROM HCL PACKAGE ===================

#|
FILE-STRING   
Summary RETURNS THE CONTENTS OF A FILE AS A STRING.
Package hcl
Signature file-string file &key length external-format => string
Arguments file A pathname, string or file-stream, designat
ing a file.
length The number of characters to return in string,
or nil (the default).
(file-string pathname &key length (external-format :default))
;;eg
 (FILE-STRING "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-capi.lisp")
;; works perfectly



;;FAST-DIRECTORY-FILES
;;xxx
fdf-handle-directory-p
fdf-handle-directory-string
fdf-handle-last-access
fdf-handle-last-modify
fdf-handle-link-p
fdf-handle-size
fdf-handle-writable-p Functions

SUMMARY Maps a callback on the names of files in a specified directory and 
RETURNS a list of those for which the callback returned true. 
The callback can RETRIEVE INFORMATION ABOUT THE FILES.

PACKAGE HCL

SIGNATURE:
  FAST-DIRECTORY-FILES DIR-PATHNAME CALLBACK => RESULT
fdf-handle-directory-p fdf-handle => directory-p
fdf-handle-directory-string fdf-handle => directory-string
fdf-handle-last-access fdf-handle => last-access
fdf-handle-last-modify fdf-handle => last-modify
fdf-handle-link-p fdf-handle => link-p
FDF-HANDLE-SIZE fdf-handle => size
fdf-handle-writable-p fdf-handle => writable-p

ARGUMENTS 
  DIR-PATHNAME A pathname designator without wild characters in its directory path.
  CALLBACK A function designator.
fdf-handle An opaque object used to retrieve information about a file in dir-pathname.38 The HCL Package
804
VALUES RESULT A list of strings.
directory-p, link-p, writable-p
Booleans.
directory-string A string.
last-access, last-modify, size
Integers.

DESCRIPTION 
   The function FAST-DIRECTORY-FILES MAPS the function CALLBACK on the names of the files in directory specified by dirpathname, and
 RETURNS a LIST OF THE NAMES for which callback returned non-nil.
dir-pathname must be a pathname designator, which does not contain wild characters in its directory path. To be useful, it should either be a DIRECTORY (with no name and type),
   or WITH WILD NAME AND/OR TYPE.

CALLBACK must be a function of TWO ARGUMENTS, 
   the name of the file and an OPAQUE OBJECT which is referred to as the fdf-handle.
The FDF-HANDLE can be used to retrieve information about the file, by calling any of the fdf-handle-* functions documented on this page.
fast-directory-files traverses the files that match dirpathname in an undefined way, and for each file calls the callback with the file’s name (not including the directory) and a fdf-handle. 
IF CALLBACK RETURNS NON-NIL IT ADDS THE NAME TO A LIST.
It returns the list of names for which the callback returned non-nil. 
Note that the names do NOT contain the directory name.

FDF-HANDLE READERS
The fdf-handle can be accessed by the following READERS. 
Functions named in parentheses would return the same value when called on the full path of the file:
FDF-HANDLE-SIZE returns the size of the file in bytes.
FDF-HANDLE-LAST-MODIFY returns the universal time of the last modification of the file (cl:file-write-date).
FDF-HANDLE-LAST-ACCESS returns the universal time of the last access of the file.
FDF-HANDLE-DIRECTORY-P is a predicate for whether the file is a directory (file-directory-p)
FDF-HANDLE-WRITABLE-P is a predicate for whether the file is writable (file-writable-p).
FDF-HANDLE-DIRECTORY-STRING returns a string with the directory path followed by a separator. Therefore the FULL PATH OF THE FILE CAN BE CONSTRUCTED by:
(string-append (fdf-handle-directory-string fdf-handle) name)
Notes The fdf-handle can be used only within the dynamic scope of
the callback to which it was passed.
See also directory
  EG. CL-USER 29 > (directory "c:/temp/")  =  (#P"c:/temp/switchsetup.exe" #P"c:/temp/setup_xrecode3_win_64bit_1.56.exe" #P"c:/temp/netbeans-8.2-windows.exe" ETC.

FROM  27.13.1 Fast access to files in a directory
fast-directory-files gives a faster way to access files than directory, especially in situations when you need to filter based on simple features such as size and access time, or filter based on the name in a more complex way than directory can.

Instead of creating a list of pathnames and returning it, fast-directory-files traverses the files and calls a callback function on each file with its name and an opaque handle, which is referred to as fdf-handle. From this handle, you can retrieve the size, last-access time and last-modify time, and query whether the file is a directory, whether it is a link (for platforms other than Windows), and whether it is writable. The implementation makes the access to the fdf-handle much faster than doing the same by calling directory and then calling cl:file-write-date and similar functions on the result.

When the callback returns non-nil, fast-directory-files collects the filename, otherwise it ignores it. Hence the callback can be used both as a filter and to actually do some work. In many cases, the callback will always return nil, and the call will be used just to map the callback on the file for the "side-effects" of the callback.

fast-directory-files is restricted to one directory level, that is it cannot deal with wild directories.
|#
;;EXAMPLES 
;;eee
;;  (fast-directory-files "c:/temp/" 'fdf-handle-size)

;; (fdf-handle-size "c:/temp/")






;;WRITE-FORMAT-TO-FILE ------------------------
;;
;;ddd
#|(defmacro write-format-to-file (format-list pathname &key if-exists-keyword )
  "In U-files.lisp, takes any standard format-list= '("string here ~A ~A" var1 var2) and writes it to a file at pathname. Takes if-exists-keyword s and also creates a file if non-existent"
  `(progn
     ,(with-open-file (out pathname :direction :output
                          :if-exists if-exists-keyword :if-does-not-exist :create)
        (eval (append (list 'format out) format-list)) 
       )))|#
;;TEST
;;tw8 test works for many variants of the if-exists-keyword and format-list etc
;;see H-macros.lisp to see development of this function as an example for develop macros
;;;;note: "~/WRITE-TO-FILE-TEST.TXT" writes to C:\USERS\SHERRY BENE STEVENS




;;PPRINT-TO-FILE
;;2020
;;ddd
(defun pprint-to-file (filename objects &key return-output-p
                                (if-exists :append) 
                               (if-does-not-exist  :create))
  "U-files uses my func write-to-file"
  (let*
      ((results)
       )
    (setf results
          (write-to-file filename objects :if-exists if-exists
                         :if-does-not-exist if-does-not-exist :pretty T
                         :return-output-p return-output-p))
    ;;end let, pprint-to-file
    ))
;;TEST
;; (setf **test-ppobj '("this" (a (b b (c) dd (1 2 (3 4)) ee)) that))
;; (pprint-to-file "C:\\TEMP\\new-pprint-file.lisp" **test-ppobj :return-output-p T)
;;works, creates and prints following to above file:
#|"this"
(A (B B (C) DD (1 2 (3 4)) EE))
THAT|#






;;WRITE-TO-FILE
;;2017
;;ddd
(defun write-to-file (filename objects &key (if-exists :append) 
                               (if-does-not-exist  :create) (return-output-p T)
                               (element-type :default) (direction :output) 
                               (pretty *print-pretty*) 
                               incl-nils-p  (newline-after-objects-p T)
                               (final-newline-p T)
                               (array *print-array*) (base *print-base*)  
                               (case *print-case*) (circle *print-circle*) 
                               (escape *print-escape*) (gensym *print-gensym*) 
                               (length *print-length*) (level *print-level*)  
                               (lines *print-lines*) (miser-width *print-miser-width*)
                               (pprint-dispatch *print-pprint-dispatch*) 
                               (radix *print-radix*)(readably *print-readably*)  
                               (right-margin *print-right-margin*) )
  "In U-files,  USE FOR PPRINT TO A FILE,etc. plus lots more varitions using print.  U-Input-output  RETURNS    INPUT:  (if incl-nils-p= NIL, omits NILs) Flexible, works well."
  (let
      ((stream)
       (n-objects 0)
       )
    (with-open-file (stream filename :if-exists  if-exists
                            :if-does-not-exist if-does-not-exist
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
    (when return-output-p
    (format nil ">> N-OBJECTS= ~A WRITTEN TO FILE=> ~A (pprint= ~A)" 
            n-objects filename pretty))
    ;;end let, write-to-file
    ))
;;TEST
;;(setf testwobj '(list '(first (a b c (1 2 3 (4 5 6 (7 8 9)(10 11 12) h i j) k l) next-last) last)))
;; (WRITE-TO-FILE   "c:/temp/my-write-test2.lisp" `( (setf testwobj ,(eval testwobj))))
;; (WRITE-TO-FILE   "c:/temp/my-write-test2.lisp" `(this (setf testwobj ,(eval testwobj))) :pretty T)
;; (WRITE-TO-FILE   "f:/1-LW-CUM/MyUtilities/my-write-test234.lisp" `(this (setf testwobj ,(eval testwobj))) :pretty T)
;;results= ">> N-OBJECTS= 2 WRITTEN TO FILE=> f:/1-LW-CUM/MyUtilities/my-write-test2.lisp (pprint= T)"



;;WRITE-FORMAT-TO-FILE
;;
;;ddd
(defun write-format-to-file (format-list pathname &key if-exists-keyword )
  "In U-files.lisp, takes any standard format-list= '(\"string here ~A ~A\" var1 var2) and writes it to a file at pathname. Takes if-exists-keyword s and also creates a file if non-existent. MUST USE BACKQUOTE-COMMA FOR LOCAL VARS IN FORMAT-LIST"
     (with-open-file (out pathname :direction :output
                          :if-exists if-exists-keyword :if-does-not-exist :create)
        (eval (append (list 'format out)  (list (first format-list))  (cdr format-list )))
       ))
;;TEST
;;  
;; (write-format-to-file '("string here ~A ~A" 5 7) "c:/temp/formatfile.lisp")
;; works:  Prints    string here 5 7  to that file.
;; (write-format-to-file '("string here ~A ~A" 5 7) "c:/temp3/temp/formatfile.lisp")

;;another test
(defun tw7 ()
  (setf out nil)
  (let
      ((var1 19)
       (var2 "TEST99") 
       )
   (write-format-to-file  `("Test-string ~A ~A~%" ,var1 ,var2)  "c:/temp/write-to-file-test.txt" :if-exists-keyword :append)
;;  (write-format-to-file2  '("Test-string ~A ~A~%" var1 var2)  "c:/temp/write-to-file-test.txt" :if-exists-keyword :append) = ERROR VAR1 VAR2 UNBOUND
  ))
;;  (tw7)
;; works:   writes    Test-string 19 TEST99    to the above file.

#|
THE MACRO DOES ABOUT SAME AS DEFUN ABOVE--BUT MUST USE EVAL
  IN THIS VERSION IF TRY TO PASS LOCAL VARS.
(defmacro write-format-to-file2 (format-list pathname &key if-exists-keyword )
  "In U-files.lisp, takes any standard format-list= '("string here ~A ~A" var1 var2) and writes it to a file at pathname. Takes if-exists-keyword s and also creates a file if non-existent.
 MUST USE EVAL-BACKQUOTE-COMMA to get preset local values eg. for var1 var2 in ex:  (EVAL `(write-format-to-file  (\"~%Test-string ~A ~A~%\" ,var1 ,var2)  \"c:/temp/write-to-file-test.txt\" :if-exists-keyword :append)) "
  `(progn
     ,(with-open-file (out pathname :direction :output
                          :if-exists if-exists-keyword :if-does-not-exist :create)
        (eval (append (list 'format out) format-list)) 
       )))

;;NOTE TWO IMPORTANT POINTS ABOUT MACROS BELOW
(defun tw8 ()
  (setf out nil)
  (let
      ((var1 19)
       (var2  "TEST5")  ;; (1) `THAT5) SYM DOESN'T WORK--TRYS TO EVAL THAT5 INSIDE THE MACRO--BUT WILL NOT EVAL A VAR SET TO OUTER LOCAL VALUE BY LET -- MUST USE A STRING WHICH EVALS TO ITSELF.
       )
    ;;(2) NOTE: MUST BACKQUOTE-COMMA FORMAT VARIABLES--LOCAL VARS
    ;;  DON'T CARRY INTO MACROS WHEN EVALED LATER
  (EVAL `(write-format-to-file2  ("~%Test-string ~A ~A~%" ,var1 ,var2)  "c:/temp/write-to-file-test.txt" :if-exists-keyword :append)) 
  ))
;;WORKS--WRITES TO FILE = Test-string 19 THAT5
|#





;;STORE-SYMS&EVALED-SYMS
;;2020
;;ddd
(defun store-syms&evaled-syms (filename  &key PPRINT-P 
                                         (dir "C:\\3-TS\\LISP PROJECTS TS\\CogsysOutputs\\") 
                                         (listnames '( *ALL-STORED-SYS-CSYMS ))
                                         (eval-nth-in-symlist 2)
                                         (file-head-message 
                                          ";;TOM ALL STORED CSYMS & CSYM-VALS")
                                         (symlist-sym "*ALL-CSYMS")
                                         (evaled-sym-sym "*ALL-CSYMVALS-LISTS")
                                         (evaled-nth-sym "*EVALED-NTH-CSYMS")
                                         )
  "U-Files, Prints objects, evaled syms, and evaled nth syms to filename."
  (let*
      ((pathname (format nil "~A~A" dir filename))
       (all-biglists)
       (all-item&result-lists)
       (date (date-string))
       (all-nth-evaled-lists)
       )  
    (with-open-file (instr pathname :direction :output :if-does-not-exist :create 
                           :if-exists :append)    
      (loop
       for listname in listnames
       do
       (let*
           ((biglist (eval listname))
            (item-lists)
            (item&result-lists)
            (nth-evaled-lists)
            )
         ;;FOR LISTS
         (setf all-biglists (append all-biglists (list biglist)))

         ;;FOR ITEMS & EVALED ITEMS
         (loop
          for item in biglist
          do
          (let*
              ((result)
               (item&result)
               (nth-sym)
               (nth-evaled-result)
               )
            (when (and (symbolp item)(boundp item))
              (setf result (eval item))
              (when eval-nth-in-symlist
                (setf nth-sym (nth eval-nth-in-symlist result))
                (when (and (symbolp nth-sym)(boundp nth-sym))
                  (setf nth-evaled-result  (list nth-sym (eval nth-sym)))))
              ;;end when and
              )
            (setf item&result (list item result)
                  item&result-lists (append item&result-lists (list item&result))
                  item-lists (append item-lists (list item)))
            (when eval-nth-in-symlist
              (setf nth-evaled-lists (append nth-evaled-lists  (list nth-evaled-result))))

            ;;end let,loop
            ))
         (setf all-biglists (append all-biglists (list item-lists))
               all-item&result-lists (append all-item&result-lists (list item&result-lists)))
         (when eval-nth-in-symlist
           (setf all-nth-evaled-lists (append all-nth-evaled-lists (list nth-evaled-lists))))
         ;;end let,loop
         ))
      ;;WRITE TO THE FILE
      (cond
       (pprint-p
        (format instr "~A~%  ;;DATE: ~A   PPRINTED LISTS:" file-head-message date)
        (format instr ";;ALL SYMS:~%  (setf ~A " symlist-sym)
        (princ " '" instr) (pprint all-biglists instr)
        (format instr "~%;;ALL SYM-SYMLISTS:~%  (setf ~A" evaled-sym-sym)
        (princ " '" instr) (pprint all-item&result-lists instr)
        (format instr "~%;;ALL SYMS by BIGLIST:~%  (setf ~A" evaled-nth-sym)
        (princ " '" instr)  (pprint all-nth-evaled-lists instr))
       (T      
        (format instr "~A~%  ;;DATE: ~A
   ;;ALL SYMS by BIGLIST:
    (setf ~A '( ~A ))~%
   ;;ALL SYM-SYMLISTS by BIGLIST: 
    (setf ~A '( ~S ))~%
    (setf ~A '(~S))~%
    "  file-head-message date symlist-sym all-biglists 
                evaled-sym-sym all-item&result-lists evaled-nth-sym all-nth-evaled-lists)))
      ;;end with-open
      )
    (values all-biglists all-item&result-lists all-nth-evaled-lists)
    ;;end let, store-syms&evaled-syms
    ))
;;TEST
;; (store-syms&evaled-syms "Tom-stored-csyms.lisp")

;; (store-syms&evaled-syms"2020-TOM-CSQ-CSYM-OUTPUT-FILE.lisp" :pprint-p T  :dir  "C:\\3-TS\\LISP PROJECTS TS\\CogsysOutputs\\"   :listnames '( *ALL-STORED-SYS-CSYMS  )  :eval-nth-in-symlist NIL   :file-head-message  ";;TOM ALL STORED CSYMS & CSYM-VALS")




;;Using WITH-OPEN-FILE (filename)
;;
;;To save a DB etc.
;;from PSeibel PCL-- see H-files.lisp for complete info
;;
;;SAVE-DB 
;;ddd
(defun save-db (db filename &key (show-text-p T) (if-not-exist :create))
  "In U-files.lisp, doc in H-files.lisp"
  (when (stringp filename)
    (let
        ((dir (directory-namestring filename))
         )
      (make-directory dir)
      (with-open-file (out filename
                           :direction :output :if-does-not-exist if-not-exist
                           :if-exists :supersede)
        (with-standard-io-syntax
          (print db out)))
      (when show-text-p 
        (show-text (format nil "Settings saved to: ~A~%" *settings-filename) 30 nil))
      ;;end when, let, save-db
      )))



;;
;;
;;THE FUNCTION TO LOAD-EVAL THE DATABASE back in is similar.
;;
;;LOAD-EVAL-DB 
;;ddd
(defun load-eval-db (filename &key (eval-value T) if-does-not-exist )
  "In U-files.lisp, doc in H-files.lisp. If eval-value is T, sets the key = (eval value), otherwise sets key = value."
  (let
      ((plist-list-db)
       )
    (cond
     ((probe-file filename)
      (with-open-file (in filename :direction :input :if-does-not-exist if-does-not-exist) ;;2018
          ;;(afout 'out (format nil ">>>> In load-eval-db, filename= ~A" filename))
        (with-standard-io-syntax
          (setf plist-list-db (read in  NIL '("eof")  T))
          ;;set the key in each pair equal the evaluated value
          (set-key-to-value-in-plists plist-list-db :eval-value eval-value)
           ;;(afout 'out (format nil "plist-list-db= ~A~%" plist-list-db))
          )))
     (t  
         (cond
          (if-does-not-exist
           (with-open-file (out filename :direction :output :if-does-not-exist if-does-not-exist)
             (format out ";;FILE FOR DIR-BU-SETTINGS  ~%"))
             (show-text (format nil "FILENAME= ~A CREATED~%" filename) 60 nil))
          (t (show-text (format nil "FILENAME= ~A IS NOT A VALID PATHNAME~%"
                                filename) 60 nil)))
         ))
    plist-list-db
    ))
;;TEST
;; (testdb)
#|(defun testdb ()
  (setf out nil)
  (load-eval-db "~/settings.db");; don't eval value :eval-value t)
    ;;(afout 'out (format nil "*PHOTO-DISPLAY-SECONDS= ~A~%" *PHOTO-DISPLAY-SECONDS))
  (fout out))|#

;;(probe-file  "C:/3-TS/MyDrives/tomex-settings-db.lisp")
#|(defun load-db (filename)
  (let ((db)
        )
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf db (read in))))
  db))|#

;;test
;;(defun myset (mysym in)
;;  (set mysym in))
;;  (testsv)
;;following tests both save-db and load-eval-db;; it works (incl with STRINGS)
#|(defun testsv ()
  (setf out nil)
  (setf testx ' ((*VISIBLE-BORDER-P NIL) (*TRANSFORM-CORRECTION-FACTOR 1.0) (*RECURSION-LIMIT 50) (*MAIN-PHOTO-DIRECTORY "C:\\OUR PARTIAL PHOTOS-VIDEOS\\MY PHOTOS\\") (*PHOTO-DIRECTORY "C:\\OUR PARTIAL PHOTOS-VIDEOS\\MY PHOTOS\\") (*PHOTO-COPY-DIR "C:\\temp\\") (*SORT-TYPE RANDOMP) (*BY-DIR-OR-FILE DIR) (*PHOTO-DISPLAY-SECONDS 12) (*REVERSE-ORDER-P NIL) (*CURRENT-PHOTO-INFO-LIST NIL) (*CURRENT-PHOTO-TITLE Current Photo)  (*CURRENT-PHOTO-DATE "Current photo date")))
  (save-db testx "c:\\temp\\mytest.db")
  (load-eval-db "c:\\temp\\mytest.db");; don't eval value :eval-value t)
  (afout 'out (format nil "*PHOTO-DISPLAY-SECONDS= ~A~%" *PHOTO-DISPLAY-SECONDS))
  (set-key-to-value-in-plists testx)
  (values *PHOTO-DISPLAY-SECONDS *PHOTO-DIRECTORY)
  )|#




;;LOAD-EVAL-SYMS&VALS
;;2020
;;ddd
(defun load-eval-syms&vals (pathname   &key dir  (max-items 100)
                                       reset-gloval-vars)
  "U-files  RETURNS (values all-syms-w/vals all-global-vars other-value-items other-list-items other-nonlist-items )   INPUT: end dir w/ slash.
 For files that include either 1. a var set to a list of syms AND/OR 2. a global var set to a list of (sym symvals) pairs AND/OR  3. a simple list of (sym symvals) pairs. 
  SETS the SYMs to SYMVALS in every case. and the global vars for the lists to the lists.  RESET-GLOVAL-VARS can be :UNBIND to unbind or T to set to nil"
  (let*
      ((all-syms-w/vals)
       (other-value-items)
       (other-list-items)
       (other-nonlist-items)
       (all-global-vars)
       )
    (when dir
      (setf pathname (format nil "~A~A" dir pathname)))   
    (with-open-file (in pathname :direction :input)
      (loop
       for n from 1 to max-items
       do
       (let*
           ((item (read in nil 'eof))
            (global-var)
            (sym&symvals-lists)
            (new-syms-w/vals)
            (third-item)
            )
         (unless (equal item 'eof)
           (cond
            ((listp item)
             (cond
              ;;items using (setf defparameter defvar)
              ((member (car item) '(setf defparameter defvar))
               (eval item)
               (setf global-var (second item)
                     all-global-vars (append1 all-global-vars global-var)
                     third-item (third item))
               (cond
                ((and (listp third-item)
                      (equal 'quote (car third-item))
                      (listp (second third-item))
                      (listp (second (second third-item))))
                 (setf sym&symvals-lists (second third-item)
                       new-syms-w/vals
                       (make-syms-from-symvalists sym&symvals-lists )
                       all-syms-w/vals (append1 all-syms-w/vals new-syms-w/vals))
                 (break "all-syms-w/vals")
                 )
                ;;list items not lists
                (T (setf other-value-items (append1 other-value-items item)))))
              ;;NOT (setf defparameter defvar) items
              (T
               (setf other-list-items (append1 other-list-items item)))))
            ;;NON-LIST ITEMS
            (T
             (setf other-nonlist-items (append1 other-nonlist-items item))))
           ;;end let,loop, unless
           )))
      ;;end with
      )
    (when reset-gloval-vars
      (cond
       ((equal reset-gloval-vars :UNBIND)
        (makunbound-vars all-global-vars))
       (T (set-vars-to-value NIL all-global-vars))))
    (values all-syms-w/vals all-global-vars other-value-items other-list-items 
            other-nonlist-items )
    ;;end let, load-eval-syms&vals
    ))
;;TEST
;; (load-eval-syms&vals "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\CS-netviewer-test-data.lisp") 
;; works= ((CAREFOROTHERSX INTIMATEX FLEXIBLEX CASUALX IMPULSIVEX ENTERTAINERX INSPIREOTHERSX) (MOTHERX FATHERX BEST-M-FRIENDX BEST-F-FRIENDX))      (PC-TESTDATA ELM-TESTDATA)   NIL  NIL  NIL
;;also:  CAREFOROTHERSX  =  ("CAREFOROTHERSX" "CARE FOR OTHERS vs SELFISH" $CS.$PC.CAREFOROTHERSX NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHERX NIL) (POLE1 NIL BEST-M-FRIENDX NIL) (POLE2 NIL FATHERX NIL) (POLE1 NIL (POLE1 INTIMATEX)) (POLE1 NIL (POLE1 INSPIREOTHERSX))) :CSVAL "0.917" :CSRANK 3)
;; also global var: PC-TESTDATA = ((CAREFOROTHERSX ("CAREFOROTHERSX" "CARE FOR OTHERS vs SELFISH" $CS.$PC.CAREFOROTHERSX NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHERX NIL) (POLE1 NIL BEST-M-FRIENDX NIL) (POLE2 NIL FATHERX NIL) (POLE1 NIL (POLE1 INTIMATEX)) (POLE1 NIL (POLE1 INSPIREOTHERSX))) :CSVAL "0.917" :CSRANK 3)) ..... ETC --- (INSPIREOTHERSX ("INSPIREOTHERSX" "INSPIRE OTHERS vs NOT INSPIRE" $CS.$PC.INSPIREOTHERSX NIL NIL :PC ("INSPIRE OTHERS" "NOT INSPIRE" 1 NIL) :POLE1 "INSPIRE OTHERS" :POLE2 "NOT INSPIRE" :BESTPOLE 1 :BIPATH ((POLE1 NIL BEST-M-FRIEND NIL) (POLE1 NIL F-ADMIRE NIL) (POLE2 NIL M-DISLIKE NIL) (POLE1 NIL (POLE1 CAREFOROTHERSX)) (POLE1 NIL (POLE1 ENTERTAINERX))) :CSVAL "0.750" :CSRANK 2)))




;;SEARCH&REPLACE-IN-FILE
;;2020
;;ddd
(defun search&replace-in-file (new-item match-substrings pathname  &key 
                                        (file-pos-start 0) file-pos-end output-path
                                        (return-extra-lines-n 3)
                                        (max-lines 25000) (make-file-bu-p T))
  "U-files. CAUTION: This func may modify files in unknown ways; it re-writes entire file. Creates original file BU if MAKE-FILE-BU-P w/ .lisp.lisp end.   RETURNS (values edited-line file-position found-line found-line-n last-line extra-lines)   INPUT:  If match-substrings can be a single string.  Returns line matching ANY string in match-substrings.  If new-string = :DELETE-ITEM or :DELETE-LINE. Uses PRIN1 to write lines back to file."
  (when (stringp match-substrings)
    (setf match-substrings (list match-substrings)))
  (unless output-path
    (setf output-path pathname))

  (let*
      ((input-path (format nil "~A.lisp" pathname))
       (last-line "")
       (found-line)
       (found-line-n)
       (edited-line)
       (file-position)       
       (extra-lines nil)
       (result)
       )
    ;;make input-path file a copy of pathname file
    (my-dos-copy pathname input-path)
    ;;INPUT
    (with-open-file (instr input-path :direction :input)
      (with-open-file (outstr output-path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
      (loop
       for n from 1 to max-lines
       do
       (let*
           ((line (read-line instr nil 'eof))
            (new-line "")
            )
         ;;(afout 'out (format nil "line= ~A" line))
         (when (equal line 'eof)
           (return))
         ;;MATCH?
         (setf result (match-substrings match-substrings line)
               last-line line)
         ;;(afout 'out (format nil "RESULT= ~A" result))
         (cond
          (result
           (setf found-line line
                 found-line-n n
                 file-position (file-position instr))
           ;;EDIT FOUND LINE
           (cond
            ((equal new-item :delete-item)
             (setf new-line result))
            ((equal new-item :delete-line)
             (setf new-line :delete-line))
            (T
             (setf new-line (format nil "~A~A~%" new-item result))))
           ;;SET EDITED-LINE
           (setf edited-line new-line)

           ;;WRITE TO FILE
           (unless (equal new-line :delete-line)
             (format outstr "~A~%" new-line))
             ;;(prin1 new-line outstr))
           ;;end  result
           )
          (T (format outstr "~A~%" line))) ;;(princ line outstr)))  ;;))

         ;;return extra lines?
         (when (and return-extra-lines-n found-line-n
                    (not (= n found-line-n))
                    (<=  n (+ return-extra-lines-n found-line-n)))
           (setf extra-lines (append extra-lines (list line))))    
         (when (null instr)
           (return))
         ;;end let,loop
         ))
      ;;end with-opens
      ))
    (values edited-line file-position found-line found-line-n last-line extra-lines)
    ;;end let, search&replace-in-file
    ))
;;TEST
;; (search&replace-in-file ";;7MK" ";;2MK" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\BUFFER-TEST-1.lisp")
;;works= ";;7MK       "  6938   ";;2MK"    213    
;;last-line:   "    ))"       
;;extra-lines:     ("  (values chars-list chars-str n-chars)" "  ))" ";; (chars \"this\\\that\\\\")")
;; (search&replace-in-file :DELETE-ITEM ";;3MK" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\BUFFER-TEST-1.lisp")
;;works= 
;; edited-line: "--REST OF LINE"
;; 4623    ";;3MK--REST OF LINE"   133    "    ))"
;;xtra-lines("    (when (pathnamep item1)" "      (setf newitem1 (namestring item1)))" "    (when (pathnamep item2)")
;;DELETE-LINE
;; (search&replace-in-file :DELETE-LINE "OF LINE" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\BUFFER-TEST-1.lisp")
;;works= :DELETE-LINE   4618   "--REST OF LINE"  133   "    ))"         ("    (when (pathnamep item1)" "      (setf newitem1 (namestring item1)))" "    (when (pathnamep item2)")

   



;;SEARCH-FILE
;;2020
;;ddd
(defun search-file (string pathname  &key match-substrings
                           (file-pos-start 0) file-pos-end 
                           (return-extra-lines 3)
                           (max-lines 25000))
  "U-files   RETURNS (values file-position found-line last-line extra-lines)   INPUT:  If null string & match-substrings, returns line matching ANY string in match-substrings. "
  (let*
      ((last-line "")
       (found-line)
       (file-position)
       (extra-lines nil)
       )
    (with-open-file (instream pathname :direction :input)
      (loop
       for n from 1 to max-lines
       do
       (let*
           ((line (read-line instream nil 'eof))
            (result)
            )
         ;;(afout 'out (format nil " N= ~A line= ~A" n line))
         (when (equal line 'eof)
           (return))
         (cond
          ((and (stringp string)
                (setf result (match-substring string line)))  NIL)
          ((and (listp string)
                (setf result (match-substrings match-substrings line))) NIL)
          (t (setf last-line line)))
         (when result
           (setf found-line line
                 file-position (file-position instream))
           (when return-extra-lines
             (loop
              for n from 1 to return-extra-lines
              do
              (setf extra-lines (append extra-lines 
                                        (list (read-line instream nil 'eof))))))
           (return)
           ;;end when result
           )
         (when (null instream)
           (return))
         ;;end let,loop
         ))
      ;;end with
      )
    (values file-position found-line last-line extra-lines)
    ;;end let, defun
    ))
;;TEST
;; (search-file "MK" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\cs-memory-test.lisp")
;;works= 403  ";;2MK"    ";;last line"     (";;RESULTS" ";;SHERRY 2019-06-24" ";; *memtest-list1 LIST")
;; (search-file ";;CSQ-SYMS" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\CSQ-MANAGE.LISP")



;;FILE-READ-LINES
;;
;;ddd
(defun read-file-lines (pathname &key (start-line 0) end-line
                                 (file-max-lines 25000) add-newline-p )
  "In U-files.lisp, uses read-line to create a string that is the contents of the file. Normally terminates at eof, file-max-lines is a limiting number. add-newline-p puts a newline at end of each line.  Returns (values all-lines n-lines), all-lines is a list of strings. Function  makes the output lisp readable, eg. \" for quotes."
  (let
      ((all-lines)
       (n-lines 0)
       )
    (unless start-line
      (setf start-line 0))
    (unless end-line
      (setf end-line 'eof))
    ;;READ THE FILE LINES
    (with-open-file (instream pathname :direction :input)
      (loop 
       for n from start-line to file-max-lines
       with line
       ;;YYY 
       do
       (incf n-lines)
       (setf line (read-line instream nil 'eof )) 
       ;;(afout 'out (format nil "n= ~A line= ~A~% all-lines= ~A" n line all-lines ))
       (cond
        ((and line (not (equal line  'eof)))
         (if add-newline-p
             (setf line (format nil "~A~%" line)))
         (setf all-lines (append all-lines (list line))))
        (t (return)))
       ;;termination test 2
       (if (equal line end-line) ;;can be 'eof
           (return))
       ;;end loop, with-
       ))
    (values all-lines n-lines)
    ))
;;TEST
;;(read-file-lines "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\cs-memory-test.lisp") 

;;
#|(defun testread ()
  (setf out nil)
  (let
      ((pathname "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\test-read-java-file.lisp")
       (file-lines)
       (n-lines)
       )
    (multiple-value-setq (file-lines n-lines)
        (read-file-lines pathname )) ;;:add-newline-p t)) ;;NOT NEEDED :read-readable-p t ))
    (values file-lines n-lines)
    ))|#
;;test
;; SAMPLE OF RETURN WITH NEWLINE WHICH ADDS " " BEFORE EACH LINE
;; NOTE HOW IT ADDS \ BEFORE "  ETC TO BE LISP/READ READABLE
#|
(";;********************** test-read-java-file.lisp ************************" ";;" "" ";;IDEAS" ";; 1- USE FORMAT CONDITIONAL TO MAKE \\N = #\\NEWLINE " ";; 2-USE + TO SIGNAL CONCAT, MERGE OR WHATEVER WORKS" ";; 3- MAKE ; INTO A NEWLINE OR IGNOR
... SKIPPED SOME HERE
  //SRQ--INTIMACY" "   public static final String intSrq6ExtraQ =" "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+" "   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\\n\\n\"+" "   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";" "   public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";" "     //same as crSrq30RealQ" "   public static final String intSrq30RealQ = \"We usually discuss what is really bothering us (the underlying issues) instead of the surface issues.\";" "   public static final String intSrq8TellAllQ = \"I have told my partner almost everything about myself.\";" "" "" "" "    PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" "    questionInstancesArray[0] = intSrq6Extra;" "    PCategory intSrq7Commit = new PCategory(\"intSrq7Commit\",2, intSrq7CommitQ, \"int\",  FrAnswerPanel.LikeUs7Reverse,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" "    questionInstancesArray[1] = intSrq7Commit;")
41
|#
;;SAMPLE RESULTS WITH ADD-NEWLINE-P = T
#| (" "     //same as crSrq30RealQ
" "   public static final String intSrq30RealQ = \"We usually discuss what is really bothering us (the underlying issues) instead of the surface issues.\";
" "   public static final String intSrq8TellAllQ = \"I have told my partner almost everything about myself.\";
" "
" "
" "
" "    PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);
" "    questionInstancesArray[0] = intSrq6Extra;
" "    PCategory intSrq7Commit = new PCategory(\"intSrq7Commit\",2, intSrq7CommitQ, \"int\",  FrAnswerPanel.LikeUs7Reverse,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);
" "    questionInstancesArray[1] = intSrq7Commit;
")
41
|#



;;READ-EVAL-SELECT-FILE-LINES
;;2020
;;ddd
(defun read-eval-select-file-lines (search-strings pathname 
                                                   &key (start-line 0) end-line (file-max-lines 25000)
                                                   add-newline-p)
  "In U-files.lisp. When any string found, reads that line--which evals it. 
  RETURNS: (values matched-lines eval-results found-objects unevaled-found-lines 
            all-lines n-lines)"
  (let
      ((all-lines)
       (n-lines 0)
       (matched-lines)
       (found-objects)
       (eval-result)
       (eval-results)
       (unevaled-found-lines)
       )
    (unless start-line
      (setf start-line 0))
    (unless end-line
      (setf end-line 'eof))
    ;;read the file lines
    (with-open-file (instream pathname :direction :input)
      (loop 
       for n from start-line to file-max-lines
       do
       (let*
           ((line (read-line instream nil 'eof ))
            (found-object)
            )
       (incf n-lines)
       ;;(afout 'out (format nil "n= ~A line= ~A~% all-lines= ~A" n line all-lines ))
       (cond
        ((and line (not (equal line  'eof)))
         (if add-newline-p
             (setf line (format nil "~A~%" line)))
         (setf all-lines (append all-lines (list line)))
         (when (match-substrings search-strings line)
           (setf matched-lines (append matched-lines (list line)))
           (setf found-object (read (make-string-input-stream line))
                 found-objects (append found-objects (list found-object)))
           (cond
            ((or (and (symbolp found-object) (boundp found-object))
                     (listp found-object))
               (setf eval-result (eval found-object)
                 eval-results (append eval-results (list eval-result))))
            (T (setf unevaled-found-lines (append unevaled-found-lines (list line))))))
         )
        (t (return)))
       ;;termination test 2
       (if (equal line end-line) ;;can be 'eof
           (return))
       ;;end let,loop, with-
       )))
    (values matched-lines eval-results found-objects unevaled-found-lines 
            all-lines n-lines)
    ;;end let,read-eval-select-file-lines
    ))
;;TEST
;; (read-eval-select-file-lines  '("setf")    "C:/3-TS/LISP PROJECTS TS/CogSysOutputs/read-eval-test-file.lisp")
;;works= 
#|("  (setf xxthis 'that)" "SECOND TEST LINE (setf xxsecond '(a b c))")
(THAT)
((SETF XXTHIS (QUOTE THAT)) SECOND)
("SECOND TEST LINE (setf xxsecond '(a b c))")
(";;************************ read-eval-test-file.lisp ****************" ";;" "THIS IS A TEST LINE" "  (setf xxthis 'that)" "SECOND TEST LINE (setf xxsecond '(a b c))" "what is this" "the end" "")
9|#




;;READ-FILE-OBJECTS&LINES
;;2020
;;ddd
(defun read-file-objects&lines (pathname &key (max-obj-n 2000) 
                                         make-objs-strings-p return-nils-p
                                         return-only-all-objs-p)
  "U-files   RETURNS (values all-objects objs-no-strings strings)"
  (let*
      ((all-objects)
       (objs-no-strings)
       (strings)
       )
    (with-open-file (instr pathname :direction :input)
      (loop
       for n from 1 to max-obj-n
       do       
      (let*
          ((obj (read instr nil 'eof nil))
           (str (when (stringp obj) obj ))
           )
        (when (equal obj 'eof)
          (return))
        (when make-objs-strings-p
          (setf obj (format nil "~A~%" obj)))
        (unless (and return-nils-p (null obj))
          (setf all-objects (append all-objects (list obj))))
        (when str
          (setf strings (append strings str)))
        (when (and (null str) obj)
          (setf objs-no-strings (append objs-no-strings (list obj))))
        ;;end let,loop,with
        )))
    (when return-only-all-objs-p
      (setf objs-no-strings nil 
            strings nil))
    (values all-objects objs-no-strings strings)
    ;;end let, read-file-objects&lines
           ))
;;TEST
;; (read-file-objects&lines "c:\\tomex\\PD-MEDIA.lisp")
;; works






;;MY-PROMPT-FOR-FILE 
;;2017
;;ddd
(defun my-prompt-for-file (message &key poke-process return-name-too-p
                                   load-file-p (verbose *load-verbose*) 
                                   (package *package*) (print *load-print*) 
                                    (if-does-not-exist t) (external-format (quote :default)) 
                                    source-pathname fasl-data)
  "In U-files RETURNS real path or (values realpath pathname). If load-file-p, will also load the file with args same as load."
  (let ((realpath (capi:prompt-for-file message))
        )
    (when poke-process
      (mp:process-poke poke-process))
    (when load-file-p 
      (load realpath  :verbose verbose :package package :print print 
                                    :if-does-not-exist if-does-not-exist  :external-format external-format 
                                    :source-pathname source-pathname :fasl-data fasl-data))

    (cond
     (return-name-too-p (values realpath (namestring realpath)))
     (t realpath))
    ;;fend let,my-prompt-for-file
    ))
;;TEST
;;   (my-prompt-for-file "Test prompt" :return-name-too-p T)
;;works= #P"C:/3-TS/LISP PROJECTS TS/ART3/ART3-simpleReset.lisp""C:\\3-TS\\LISP PROJECTS TS\\ART3\\ART3-simpleReset.lisp"
;; (my-prompt-for-file "Test prompt" :return-name-too-p T   :load-file-p T)
;;works= ; Loading text file C:\3-TS\LISP PROJECTS TS\CogSysOutputs\Tom-AllData2017-01-copy.lisp
;;Warning: Setting unbound variable *FILE-ELMSYMS
;;Warning: Setting unbound variable *FILE-ELMSYMVALS  
;; ETC
;; #P"C:/3-TS/LISP PROJECTS TS/CogSysOutputs/Tom-AllData2017-01-copy.lisp"
;; "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\Tom-AllData2017-01-copy.lisp"

;;(my-prompt-for-file :poke-process (mp:get-current-process))





;;IF SETTINGS FILE EXISTS, READ IT AND USE SETTINGS
;;
;;READ-SETTINGS-FILE
;;ddd
;;bbb
(defun read-settings-file (db-sym pathname)
  "In ScreensaveMP,"
  (cond
   ((pathnamep pathname)
    (load-db db-sym pathname))
   (t (set db-sym nil)))
  db-sym)
;;bbb





;;LOAD-DB 
;;ddd
(defun load-db (db-sym filename)
  "In U-files.lisp, doc in H-files.lisp"
  (with-open-file (in filename)
    (with-standard-io-syntax
      (set db-sym (read in)))))
;;my version (above) allows setting database to any symbol you choose
#|(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))|#
;;



;;(FP 599 *cd *filenameXX)

(defparameter *files-out nil  "The text from U-files used in fout for debugging, etc")

;;USE THE FUNCTION (CALL-SYSTEM DOS-COMAND-STRING)  FOR MANY FILE OPERATIONS ETC  -- SEE MY HELP FILE H-SYSTEM.LISP

(setf *cdroot "C:\\3-TS\\LISP PROJECTS TS"
      *cdbranch "\\MyUtilities"
      *cd (format nil "~a~a" *cdroot *cdbranch))
(setf *filenameXX "U-Files.lsp")
;; 
;;
;;MY-DOS-COMMAND2
;;CALL-SYSTEM COMMAND &KEY CURRENT-DIRECTORY WAIT SHELL-TYPE 
;;works (my-dos-command 'copy "C:\\3-TS\\Temp\\ArtsCulture.pdf" :to-path "c:\\temp") 
;;ddd
(defun my-dos-command2 (dos-command  filename &key to-path )
  "in U-files, OLD-VERSION PRIMARILY FOR FILES. Use copyfiles, ETC  SEE NEWER MY-RUN-SYSTEM in U-system.  "
  ;;note:  MUST HAVE EXTRA QUOTES ON FILENAMES OR DOS WON'T READ IT RIGHT
  (let
      ((command)
       (rest-arg (format nil "\"~A\"" filename))
       )
    (if to-path (setf rest-arg (format nil "~A  \"~A\"" rest-arg to-path)))
    (setf command  (format nil "~A ~A" dos-command rest-arg))
    (show-text command 30 t)
    (format t "~A" command)
    (system:call-system command))
  )
;;TEST
;; (my-dos-command :dir  '*.*)
;; this doesn't work


;;MY-DOS-COMMAND
;;
;;ddd
(defun my-dos-command (dos-command show-text-p &rest rest-args  )
  "in U-files,  RETURNS (values dos-out-string  dos-out). show-text-p causes a separate popup to display results. NOTE:  MUST HAVE EXTRA QUOTES ON FILENAMES or dos won't read it right -- SEE NEWER MY-RUN-SYSTEM in U-system."
  (let
      ((command)
       (rest-args-str "") 
       (prefix "")
       )
    (loop
     for arg in rest-args
     do
     (setf rest-args-str (format nil  "~A ~A" rest-args-str arg))
     ;;end loop
     )
    (setf command  (format nil "~A ~A" dos-command rest-args-str))

    (multiple-value-bind (dos-out-string dos-out)
        (CALL-WITH-SHOW-DOS-OUTPUT command :prefix prefix)    
          
      (when show-text-p
        (show-text (format nil "Command= ~A~% dos-out-string= ~A~%dos-out= ~A" command dos-out-string dos-out) 30 t  :min-width 60))
    (values dos-out-string dos-out)
    ;;end mvb,let,my-dos-command
    )))
;;TEST
;; (my-dos-command 'vol nil "E:")
;; (my-dos-command 'LABEL nil "E:" "NEW-E" ) = works
;; (show-text "text" 30 "title" :min-width 60)
;; (my-dos-command "dir c:\\temp" T) = works
;; note: HAD TO USE DOUBLE QUOTES BECAUSE A SPACE IN PATH
;; (my-dos-command "dir  \"C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\" " T) = works
;; (my-dos-command "dir \"C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\*.lisp\" " T) 
;; works "dir \"C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\*.lisp\"  
#|" Volume in drive C is OS
 Volume Serial Number is 4058-766B
 Directory of C:\\3-TS\\LISP PROJECTS TS\\MyUtilities
08/03/2018  08:31 PM               339 0-CODE-TEMPLATES.lisp
05/08/2018  06:48 PM            15,903 ARC-curve-function-tests.lisp
08/11/2010  02:36 PM             2,837 space-show-arglist.lisp
11/26/2013  03:48 PM            23,466 U-Arrays.lisp
02/27/2019  07:05 PM            10,138 U-BASIC-functions.lisp
04/22/2016  03:06 PM             4,527 U-capi - Copy.lisp
04/25/2014  08:14 PM            48,236 U-capi-buttons-etc-old.lisp
10/12/2016  03:04 PM           112,977 U-capi-buttons-etc.lisp
02/23/2018  06:37 PM            10,892 U-capi-graphics.lisp
06/26/2019  05:18 PM           113,374 U-capi-input-interfaces.lisp
01/25/2017  12:19 PM             6,840 U-capi-multi-column-list-panels.lisp
04/25/2018  05:23 PM            16,536 U-capi.lisp
02/10/2014  07:46 PM            42,417 U-clos-wDefunMyDefclass.lisp
04/14/2018  07:20 PM            90,253 U-clos.lisp
05/29/2014  02:19 PM            27,773 U-colors.lisp
03/26/2015  04:56 PM             9,965 U-Comm.lisp
04/10/2018  02:13 PM           149,864 U-CS-ART.lisp
04/10/2018  02:13 PM            51,039 U-CS-data-results-functions.lisp
04/14/2018  07:20 PM             9,837 U-CSQ-shaq-web.lisp
  ... MANY IN MIDDLE DELETED ...
09/20/2013  06:01 PM            54,813 U-photo-infoX.lisp
10/22/2013  05:11 PM             5,690 U-photos.lisp
03/17/2017  05:55 PM             7,510 U-sequences.lisp
03/16/2016  04:14 PM            13,692 U-sexp.lisp
04/16/2018  09:00 PM             2,667 U-symbol-info.lisp
08/08/2019  09:02 PM           170,425 U-symbol-trees.lisp
03/27/2019  09:46 AM            22,096 U-system.lisp
08/29/2019  05:48 PM            87,058 U-trees-art-dims.lisp
08/22/2019  07:54 PM            88,173 U-trees-art-dims1.lisp
08/06/2017  05:22 PM            15,563 U-trees-flatlists.lisp
07/23/2019  05:54 PM            81,732 U-trees-old.lisp
08/09/2019  08:48 PM            93,451 U-trees.lisp
04/16/2015  05:18 PM           121,474 U-tstring-OLD-make-index-funs.lisp
08/10/2019  03:35 PM           255,774 U-tstring.lisp
              62 File(s)      4,443,093 bytes
               0 Dir(s)  381,221,978,112 bytes free
"|#




;;MY-DOS-LABEL
;;2017
;;ddd
(defun my-dos-label (drive new-label &optional show-text-p)
  "In U-files, changes drive label name.  WILL NOT RETURN CURRENT LABEL Returns label 0 if changed, 1 if error or not chanaged."
  (cond
   ((symbolp drive)
    (setf drive (format nil "~A:"  drive)))
   ((> (length drive) 2)
    (setf drive (subseq drive 0 2)))
   (t nil))
  ;;(break)
  (my-dos-command  'LABEL show-text-p drive new-label)
  )
;;TEST
;; All of following work
;; (my-dos-label 'G "F01-128" t)
;; (my-dos-label 'E "SD01-128" t)
;; (my-dos-label "F:/" "F05-64A" t)
;; (my-dos-label "F:\\"  "F06-128") = "LABEL  F: F06-128  "   0







;;MY-DOS-VOL
;;2017
;;ddd
(defun my-dos-vol (drive &optional show-text-p)
  "In U-files, RETURNS (values vol-label host serial-num drive-let)"
  (let
      ((vol-label "")
       (serial-num "")
       (tokens)
       (drive-str)
       )
  (when (symbolp drive)
    (setf drive-str (format nil "~A:"  drive)))
    (multiple-value-bind (string result)
        (my-dos-command 'VOL show-text-p drive-str )
      (setf tokens (divide-string-to-all-tokens string))
      (multiple-value-bind (v host v2 in d drive-let is vol-label v3 ser num i2 serial-num)
          (values-list tokens)
     ;;(BREAK "HERE")
#|;;"VOL  E:
 Volume in drive E is SD01-128
 Volume Serial Number is 3634-6637
"|#
      (values vol-label host serial-num drive-let)
      ;;end mvbs, let, my-dos-vol
      ))))
;;TEST
;; (my-dos-vol 'E nil)
;; works= "SD01-128"   "E:"    "3634-6637"   "E"



;;MY-DOS-COPY
;;
;;ddd
(defun my-dos-copy (from-path to-path &key show-text-p new-filename 
                              (use-backslashes-p T) )
  "U-files.lisp. INPUT: to-path can be a dir or full pathname. If to-path only a dir-path and new-filename, copies to the new-filename in to-path dir. RETURNS (values dos-string dos-result copied-p)
  NOTE: NO EXTRA SPACES before root, etc-> error. Replaces forward-slashes with backslashes--requlred by DOS?"
  ;;note:  must have extra quotes or dos won't read it right
  ;;MUST HAVE PATH WITH BACKSLASHES TO WORK WITH DOS?
  (when use-backslashes-p
    (setf to-path (my-substitute "\\" "/" to-path)
          from-path (my-substitute "\\" "/" from-path)))
  (let ((command (cond (new-filename  
                        (format nil "copy \"~A\" \"~A/~A\"" from-path to-path new-filename))
                       (t (format nil "copy \"~A\"  \"~A\"" from-path to-path))))
        (copied-p)
    )
    (multiple-value-bind (dos-string dos-result)
    (my-dos-command command show-text-p)
      (when (= dos-result 0)
        (setf copied-p T))
      (values dos-string dos-result copied-p)
      ;;end mvb, let, my-dos-copy
      )))
;;TEST
;; (my-dos-copy "C:/temp1/test-file.txt"  "c:/temp2/test-file-renamed5.txt")
;; works= "copy \"C:\\Temp1\\test-file.txt\" \"C:\\Temp/test-newnameX4.txt\"    1 file(s) copied.  " T
;; (my-dos-copy "C:/temp1/test-file.txt"  "e:/test-file-renamed5.txt")
;; ; (my-dos-copy  "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-files.lisp" "c:/dropbox/1-lw-cum")
;;CAN COPY WITH CMD, BUT NOT USING LISP TO DROPBOX
;; results= ACCESS DENIED "copy \"C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-files.lisp\"  \"c:\\dropbox\\1-lw-cum\"   Access is denied.           0 file(s) copied." 1  NIL
;;  (my-dos-copy  "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-files.lisp" "c:/dropbox-ADD/1-lw-cum")
;; works= "copy \"C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-files.lisp\"  \"c:\\dropbox-ADD\\1-lw-cum\"         1 file(s) copied.  "  0  T
;; (my-dos-copy "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp" "E:\\U-listsbu.lisp") = works
;; (my-dos-copy "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"   "E:\\0 LW-CUM-dated\\MyUtilities\\U-lists-test.lisp") = works


;;USING CMD WINDOW
;;;; >copy "C:\temp1\test-file.txt"  "c:\temp2\test-file-renamed.txt" WORKS
;;  COPY & DIR WORK MOST OF TIME WITH EITHER \ OR /
;;copy "c:/temp1/test-file.txt" "c:/temp2/test-newX.txt" = not work
;; copy "c:/temp1\test-file.txt" "c:/temp2/test-newX.txt" = works
;; dir "c:/dropbox/1-lw-cum/myutilities" = works (w /)
;;  copy "c:/temp1/test-file.txt" "c:/temp2/test-newX2.txt" = not work
;; copy "c:/temp1\test-file.txt" "c:/temp2/test-newX2.txt"  = works



;; (my-dos-copy "C:\\temp1\\test-file.txt"  "c:\\temp2\\test-file-renamed3.txt")
;;(my-dos-copy "C:\\3-TS\\Temp\\ArtsCulture.pdf" "c:\\temp")
;;(my-dos-copy "C:\\3-TS\\Temp\\YAHOO spam.txt" "c:/dropbox-ADD/temp" :new-filename "yahoo-new-spamname.txt")
;;works= "copy \"C:\\3-TS\\Temp\\YAHOO spam.txt\" \"c:/dropbox-ADD/temp/yahoo-new-spamname.txt\"         1 file(s) copied.  "  0
;; results= "copy \"C:\\3-TS\\Temp\\YAHOO spam.txt\" \"c:/dropbox/temp/yahoo-new-spamname.txt\"  Access is denied.          0 file(s) copied.
;; (my-dos-copy "C:\\3-TS\\Temp\\YAHOO spam.txt" "c:/dropbox-ADD/temp/yahoo-new-spamname222.txt")
;; works = "copy \"C:\\3-TS\\Temp\\YAHOO spam.txt\" \"c:/dropbox-ADD/temp/yahoo-new-spamname222.txt\"           1 file(s) copied. " 0




#|
;;DOS COMMANDS ----------------------------------------------------------------
ASSOC          Displays or modifies file extension associations.
ATTRIB         Displays or changes file attributes.
BREAK          Sets or clears extended CTRL+C checking.
BCDEDIT        Sets properties in boot database to control boot loading.
CACLS          Displays or modifies access control lists (ACLs) of files.
CALL           Calls one batch program from another.
CD             Displays the name of or changes the current directory.
CHCP           Displays or sets the active code page number.
CHDIR          Displays the name of or changes the current directory.
CHKDSK         Checks a disk and displays a status report.
CHKNTFS        Displays or modifies the checking of disk at boot time.
CLS            Clears the screen.
CMD            Starts a new instance of the Windows command interpreter.
COLOR          Sets the default console foreground and background colors.
COMP           Compares the contents of two files or sets of files.
COMPACT        Displays or alters the compression of files on NTFS partitions.
CONVERT        Converts FAT volumes to NTFS.  You cannot convert the
               current drive.
COPY           Copies one or more files to another location.
DATE           Displays or sets the date.
DEL            Deletes one or more files.
DIR            Displays a list of files and subdirectories in a directory.
DISKCOMP       Compares the contents of two floppy disks.
DISKCOPY       Copies the contents of one floppy disk to another.
DISKPART       Displays or configures Disk Partition properties.
DOSKEY         Edits command lines, recalls Windows commands, and
               creates macros.
DRIVERQUERY    Displays current device driver status and properties.
ECHO           Displays messages, or turns command echoing on or off.
ENDLOCAL       Ends localization of environment changes in a batch file.
ERASE          Deletes one or more files.
EXIT           Quits the CMD.EXE program (command interpreter).
FC             Compares two files or sets of files, and displays the
               differences between them.
FIND           Searches for a text string in a file or files.
FINDSTR        Searches for strings in files.
FOR            Runs a specified command for each file in a set of files.
FORMAT         Formats a disk for use with Windows.
FSUTIL         Displays or configures the file system properties.
FTYPE          Displays or modifies file types used in file extension
               associations.
GOTO           Directs the Windows command interpreter to a labeled line in
               a batch program.
GPRESULT       Displays Group Policy information for machine or user.
GRAFTABL       Enables Windows to display an extended character set in
               graphics mode.
HELP           Provides Help information for Windows commands.
ICACLS         Display, modify, backup, or restore ACLs for files and
               directories.
IF             Performs conditional processing in batch programs.
LABEL          Creates, changes, or deletes the volume label of a disk.
MD             Creates a directory.
MKDIR          Creates a directory.
MKLINK         Creates Symbolic Links and Hard Links
MODE           Configures a system device.
Configures system devices.
Serial port:       MODE COMm[:] [BAUD=b] [PARITY=p] [DATA=d] [STOP=s]
                                [to=on|off] [xon=on|off] [odsr=on|off]
                                [octs=on|off] [dtr=on|off|hs]
                                [rts=on|off|hs|tg] [idsr=on|off]
Device Status:     MODE [device] [/STATUS]
Redirect printing: MODE LPTn[:]=COMm[:]
Select code page:  MODE CON[:] CP SELECT=yyy
Code page status:  MODE CON[:] CP [/STATUS]
Display mode:      MODE CON[:] [COLS=c] [LINES=n]
Typematic rate:    MODE CON[:] [RATE=r DELAY=d]

MORE           Displays output one screen at a time.
MOVE           Moves one or more files from one directory to another
               directory.
OPENFILES      Displays files opened by remote users for a file share.
PATH           Displays or sets a search path for executable files.
PAUSE          Suspends processing of a batch file and displays a message.
POPD           Restores the previous value of the current directory saved by
               PUSHD.
PRINT          Prints a text file.
PROMPT         Changes the Windows command prompt.
PUSHD          Saves the current directory then changes it.
RD             Removes a directory.
RECOVER        Recovers readable information from a bad or defective disk.
REM            Records comments (remarks) in batch files or CONFIG.SYS.
REN            Renames a file or files.
RENAME         Renames a file or files.
REPLACE        Replaces files.
RMDIR          Removes a directory.
ROBOCOPY       Advanced utility to copy files and directory trees
SET            Displays, sets, or removes Windows environment variables.
SETLOCAL       Begins localization of environment changes in a batch file.
SC             Displays or configures services (background processes).
SCHTASKS       Schedules commands and programs to run on a computer.
SHIFT          Shifts the position of replaceable parameters in batch files.
SHUTDOWN       Allows proper local or remote shutdown of machine.
SORT           Sorts input.
START          Starts a separate window to run a specified program or command.
SUBST          Associates a path with a drive letter.
SYSTEMINFO     Displays machine specific properties and configuration.
TASKLIST       Displays all currently running tasks including services.
TASKKILL       Kill or stop a running process or application.
TIME           Displays or sets the system time.
TITLE          Sets the window title for a CMD.EXE session.
TREE           Graphically displays the directory structure of a drive or
               path.
TYPE           Displays the contents of a text file.
VER            Displays the Windows version.
VERIFY         Tells Windows whether to verify that your files are written
               correctly to a disk.
VOL            Displays a disk volume label and serial number.
XCOPY          Copies files and directory trees.
WMIC           Displays WMI information inside interactive command shell.
[global switches] <command>
The following WMIC global switches are available:
/NAMESPACE           Path for the namespace the alias operate against.
/ROLE                Path for the role containing the alias definitions.
/NODE                Servers the alias will operate against.
/IMPLEVEL            Client impersonation level.
/AUTHLEVEL           Client authentication level.
/LOCALE              Language id the client should use.
/PRIVILEGES          Enable or disable all privileges.
/TRACE               Outputs debugging information to stderr.
/RECORD              Logs all input commands and output.
/INTERACTIVE         Sets or resets the interactive mode.
/FAILFAST            Sets or resets the FailFast mode.
/USER                User to be used during the session.
/PASSWORD            Password to be used for session login.
/OUTPUT              Specifies the mode for output redirection.
/APPEND              Specifies the mode for output redirection.
/AGGREGATE           Sets or resets aggregate mode.
/AUTHORITY           Specifies the <authority type> for the connection.
/?[:<BRIEF|FULL>]    Usage information.

For more information on a specific global switch, type: switch-name /?
The following alias/es are available in the current role:
ALIAS                    - Access to the aliases available on the local system
BASEBOARD                - Base board (also known as a motherboard or system board) management.
BIOS                     - Basic input/output services (BIOS) management.
BOOTCONFIG               - Boot configuration management.
CDROM                    - CD-ROM management.
COMPUTERSYSTEM           - Computer system management.
CPU                      - CPU management.
CSPRODUCT                - Computer system product information from SMBIOS.
DATAFILE                 - DataFile Management.
DCOMAPP                  - DCOM Application management.
DESKTOP                  - User's Desktop management.
DESKTOPMONITOR           - Desktop Monitor management.
DEVICEMEMORYADDRESS      - Device memory addresses management.
DISKDRIVE                - Physical disk drive management.
;;---------------------------------------------------------
|#



#|
(defun my-save-file (file pathname)
  (let
      ((command (format nil "SAVE ~A" )
       (call-system 
 )))))

(defun my-open-file (filename)
  (let
      ((command (format nil "OPEN"
)))))
|#

(DEFUN FP1 (FILE-POSITION pathname)
  "In U-Files.lisp, finds file-postion and prints line point lies on."
  (LET ((PRINT-STRING "")
        (file-length 0)
        )
    (WITH-OPEN-FILE (IN-STREAM PATHNAME :DIRECTION :INPUT) 
      (setf file-length (file-length in-stream))
      (cond
       ((< file-position file-length)
        (file-position in-stream file-position)
        (setf print-string (read-line in-stream))
        (format t "FILE-POSITION= ~A LINE=> ~A" file-position print-string))
       (t (format t "FILE-POSITION GREATER THAN FILE-LENGTH= ~A" file-length)))
      )))

(defun FP (file-position directory filename)
  "In U-Files.lisp; uses FP"
  (let ((pathname (format nil "~A~A" directory filename))
        )
    (FP1 file-position pathname)))



;;MY-LOAD-DIRECTORIES
;;2018
;;ddd
(defun my-load-directories (dirs operation &optional load-print-p )
  "In U-Files.lisp, for all lisp files If operation= 'print-filename 'print-path 'compile-load, compiles and loads all. Returns all-source-files list"
  (let
      ((all-source-files)
       )
    (loop
     for dir in dirs
     do
     (let
         ((source-files (my-load-directory dir operation  load-print-p ))
          )
       (setf all-source-files (append all-source-files (list source-files)))
       ;;end let,loop
     ))
    ;;return source-files list
    all-source-files
    ;;end let, my-load-directories
    ))
;;TEST
;; (my-load-directories '("C:\\3-TS\\LISP PROJECTS TS\\ACT-R TS\\tutorial\\unit1")  'print-filename T)



;;(defparameter *plotter-directory "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master")
;;  (my-load-directory *plotter-directory 'load t)
;;MY-LOAD-DIRECTORY
;;
;;ddd
(defun my-load-directory (dir operation &optional load-print-p file-list)
  "In U-Files.lisp, for all lisp files If operation= 'print-filename 'print-path 'compile-load, compiles and loads all. If file-list, then loads it instead of files found in directory.
    Returns source-files list"
  (let
      ((source-files)
       (source-pathname)
       (compile-pathname)
       )
    (cond
     (file-list
      (setf source-files file-list))
     (t (setf source-files  (directory (format nil "~A/\*.*" dir)))))
    (format t "Lisp files in directory= ~A~%" dir)
    (format t "source-files= ~A~%" source-files)
    (cond
     ;;print only
     ((member operation '(print print-path print-filename print-file))
      (dolist (file source-files)
        (cond
         ((member (pathname-type file) '( "lisp" "lsp" "cl") :test 'equal)
            (setf file (file-namestring file))
            )
           (t nil ))      
          (format t "\"~A\"~%"  file)
          ))
     ;;compile and load
     ((equal operation 'compile-load)
      (dolist (file source-files)
        (cond
         ((member (pathname-type file) '( "lisp" "lsp" "cl") :test 'equal)
          (setf source-pathname (format nil "~A/~A" dir file))
          (compile-file source-pathname)
          (setf compile-pathname (compile-file-pathname source-pathname))
          (load compile-pathname :print load-print-p))
         ((equal (pathname-type source-pathname) "fas")
          (load source-pathname :print load-print-p))
         (t nil))
        ))
     ;;load
     ((equal operation 'load)
      (dolist (file source-files)
        (setf source-pathname (format nil "~A\\~A" dir file))
        (format t "1 load source-pathname= ~A~% pathname-type= ~A~%"
                source-pathname (pathname-type source-pathname))
        (cond
         ((member (pathname-type source-pathname) '( "lisp" "lsp" "cl") :test 'equal)
          (load source-pathname :print load-print-p)
          (format t "loading file= ~A~%" source-pathname))
         (t nil))
        ))
     )
    ;;return source-files list
    source-files
    ))

;;LOAD-FILES
;;2019
;;ddd
(defun load-files (files  &key  dir  )
  "U-files   RETURNS    INPUT: files=list of pathnames or filenames. dir=string. If dir, only use filenames not paths for files. Also compiles files."
  (let
      ((loaded-paths)
       (n-paths)
       )   
    (loop
     for file in files
     do
     (let*
         ((path)
          )
       (cond
        (dir
         (setf dir (delete-final-string '("\\" "/") dir))
         (setf path (format nil "~A/~A" dir file)))
        (t (setf path file)))
       (compile-file path :load T)
       (setf loaded-paths (append loaded-paths (list path)))
       ;;end let,loop
       ))     
    (setf n-paths (list-length loaded-paths))
    (values loaded-paths n-paths)
    ;;end let, load-files
    ))
;;TEST
;; (load-files '("C:/3-TS/LISP PROJECTS TS/MyUtilities/U-BASIC-functions.lisp"  "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-Files.lisp"))
;; (load-files '("U-BASIC-functions.lisp"  "U-Files.lisp") :dir "C:/3-TS/LISP PROJECTS TS/MyUtilities/")



;;LIST-DOS-DIR-CONTENTS (REPLACES list-directory-contents in LIST-DRIVE-INFO
;;2017
;;ddd
(defun list-dos-dir-contents (dirname &key print-lists-p incl-hostless-subdir-names-p)
  "In U-files.lisp, uses Seibel function. RETURNS S (values cur-dir host drive-name dos-volname serial-num n-dirs n-files used-size free-size  dirlists filelists  host-str hostless-dirstrs)  MUST INCL HOST except on drive C: (returns host-str= \"ERROR: NO HOST in dirname\".  "
  (let*
    ((host (host-namestring dirname))
       (host-str (format nil "~A:/" host))
#|       (hostless-dirstr)
       (hostless-dirstrs)
       (drive-name)
       (dos-volname)|#
       )
    (when (equal host "")
      (setf host-str "ERROR: NO HOST in dirname"))
    ;;SSSS FINISH list-dos-dir-contents FIRST IF NEEDED??/
    (multiple-value-bind (cur-dir host1 volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out) ;;omit all-line-lists)
        (my-dos-dir dirname)        
      ;;older   (values filenamestrs file-pathnamestrs subdir-namestrs file-paths subdir-paths host-str hostless-dirstrs)
      (values cur-dir host volname volname serial-num n-dirs n-files 
                                  used-size free-size  dirlists filelists  host-str) ;; hostless-dirstrs)
     ;;end, list-dos-dir-contents
     )))




;;LIST-DIRECTORY-CONTENTS (not neeed -- use MY-DOS-DIR instead??
;; modified 2019
;;ddd
(defun list-directory-contents (dirname &key print-lists-p 
                                        incl-filetypes omit-filetypes
                                        incl-hostless-subdir-names-p)
  "not neeed -- use MY-DOS-DIR instead?? 
  In U-files.lisp, uses Seibel function. RETURNS S (values filenamestrs file-pathnamestrs subdir-namestrs file-paths subdir-paths host-str hostless-dirstrs)  MUST INCL HOST except on drive C: (returns host-str= \"ERROR: NO HOST in dirname\".  "
  (let*
      ((file-paths)
       (dir-paths)
       (filenamestr)
       (filenamestrs)
       (file-pathnamestr)
       (file-pathnamestrs)
       (subdir-namestr)
       (subdir-namestrs)
       (subdir-paths)
       (host (host-namestring dirname))
       (host-str (format nil "~A:/" host))
       (hostless-dirstr)
       (hostless-dirstrs)
       )
    (when (equal host "")
      (setf host-str "ERROR: NO HOST in dirname"))

    ;;DIR-PATHS are REAL LISP DIR PATHS
    (setf dir-paths (list-directory dirname ))
    (loop
     for path in dir-paths
     do
     (when path
     (let*
         ((pathnamestr (namestring path))
          (filenamestr (file-namestring path))
          (filetype) 
          )
       (when (stringp filenamestr)
         (setf filetype (pathname-type filenamestr)))
       (when
           (and filetype
                (or (null incl-filetypes) 
                     (member filetype incl-filetypes :test 'string-equal))
                 (or (null omit-filetypes) 
                     (not (member filetype omit-filetypes :test 'string-equal))))
         (cond      
          (filenamestr
           (setf  filenamestrs (append  filenamestrs (list filenamestr))
                  file-pathnamestrs (append file-pathnamestrs (list pathnamestr))
                  file-paths (append file-paths (list path))))
          (t
           (setf subdir-namestrs (append subdir-namestrs (list pathnamestr))
                 subdir-paths (append subdir-paths (list path)))))    

         (when incl-hostless-subdir-names-p
           (loop
            for name in subdir-namestrs
            do
            (setf hostless-dirstr (directory-namestring name)
                  hostless-dirstrs (append hostless-dirstrs (list hostless-dirstr)))
            ;;end loop, inner when
            ))
         ;;end when path, when filetype ok, let, loop
         ))))
    (values filenamestrs file-pathnamestrs subdir-namestrs file-paths subdir-paths host-str hostless-dirstrs)
    ;;end let, list-directory-contents
    ))
#| OLD--CAN'T CONTROL FILETYPE
(defun list-directory-contents (dirname &key print-lists-p 
                                        incl-hostless-subdir-names-p)
  "not neeed -- use MY-DOS-DIR instead?? 
  In U-files.lisp, uses Seibel function. RETURNS S (values filenamestrs file-pathnamestrs subdir-namestrs file-paths subdir-paths host-str hostless-dirstrs)  MUST INCL HOST except on drive C: (returns host-str= \"ERROR: NO HOST in dirname\".  "
  (let*
      ((file-paths)
       (filenamestr)
       (filenamestrs)
       (file-pathnamestr)
       (file-pathnamestrs)
       (subdir-namestr)
       (subdir-namestrs)
       (subdir-paths)
       (host (host-namestring dirname))
       (host-str (format nil "~A:/" host))
       (hostless-dirstr)
       (hostless-dirstrs)
       )
    (when (equal host "")
      (setf host-str "ERROR: NO HOST in dirname"))

    ;;DIR-PATHS are REAL LISP DIR PATHS
    (setf dir-paths (list-directory dirname ))
    (loop
     for path in dir-paths
     do
     (setf pathnamestr (namestring path)
           filenamestr (file-namestring path))
     (cond
      (filenamestr
       (setf  filenamestrs (append  filenamestrs (list filenamestr))
              file-pathnamestrs (append file-pathnamestrs (list pathnamestr))
              file-paths (append file-paths (list path))))
      (t
       (setf subdir-namestrs (append subdir-namestrs (list pathnamestr))
             subdir-paths (append subdir-paths (list path)))))
     )

    (when incl-hostless-subdir-names-p
      (loop
       for name in subdir-namestrs
       do
       (setf hostless-dirstr (directory-namestring name)
             hostless-dirstrs (append hostless-dirstrs (list hostless-dirstr)))
       ;;end loop, when
       ))
       (values filenamestrs file-pathnamestrs subdir-namestrs file-paths subdir-paths host-str hostless-dirstrs)
    ;;end let, list-directory-contents
    ))|#
;;TEST
;; (list-directory-contents "c:\\temp\\")
;; (list-directory-contents "c:\\temp\\" :incl-hostless-subdir-names-p T)
;; (NAMESTRING "C:/THIS/THAT/OTHER.LISP") = "C:\\THIS\\THAT\\OTHER.LISP"
;;"c:\\"
;;subdir-namestrs= ("\\temp\\Editorial Staff Directory - LA Times_files\\")
;; filepath-namestrs= ("\\temp\\netbeans-8.2-windows.exe" "\\temp\\jdk-8u121-windows-x64.exe" "\\temp\\Editorial Staff Directory - LA Times.htm" "\\temp\\coral test.cdr" "\\temp\\Chrome Setup.exe" "\\temp\\bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe")
;;filenamestrs= ("netbeans-8.2-windows.exe" "jdk-8u121-windows-x64.exe" "Editorial Staff Directory - LA Times.htm" "coral test.cdr" "Chrome Setup.exe" "bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe")
;;subdir-paths= (#P"c:/temp/Editorial Staff Directory - LA Times_files/")
;;file-paths= (#P"c:/temp/netbeans-8.2-windows.exe" #P"c:/temp/jdk-8u121-windows-x64.exe" #P"c:/temp/Editorial Staff Directory - LA Times.htm" #P"c:/temp/coral test.cdr" #P"c:/temp/Chrome Setup.exe" #P"c:/temp/bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe")

;;  (list-directory-contents "\\temp")

;;(list-directory-contents "c:\\")p186?
;;(list-directory-contents "c:\\temp")
;; (pathname-directory "c:\\this\\that\\file.lisp") 
;; = (:ABSOLUTE "this" "that")



;;2016-03
;;SORT-DIRECTORY-FILENAMES
;;
;;ddd
(defun sort-directory-filenames (dirname &key file-list (by-embedded-num-p T)  (ascending-p T)) 
  "In U-files.lisp, If fi;;le-list, finds sorted filenames and pathnames in dirname.  If (null file-list), gets file and pathnames from dirname.  If by-embedded-num-p, then sorts files by an embeded number (as in most photo files).  (Uses Seibel function. Returns values filename-list and pathname-list, but NOT subdirectories) If file-list, DIRNAME should end in \\ "
  (let ((filename)
        (embedded-num)
        (filename-list)
        (pathname)
        (pathname-list)
        (numfile-sublist)
        (numfile-list)
        (sorted-filenames)
        (sorted-pathnames)
        (sorted-sublists)
        )
    
    (unless file-list
      (when dirname
        (setf file-list (list-directory dirname))))

    ;;MAKE A LIST OF ALL (embedded-num filename pathname)
    (when file-list
    (loop
     for file in file-list 
     do
     (when file
       (cond
        ;;FIND PATHNAME
        ((and dirname file-list)
         (setf pathname (format nil "~A~A" dirname file)))
        ((null dirname)
         (setf pathname file))
        (dirname
         (setf pathname (namestring file)))
        (t nil))

       ;;FIND REST 
       (setf  filename (file-namestring file)
              embedded-num (car (find-integers-in-string filename))
              numfile-sublist (list embedded-num filename pathname)
              numfile-list (append numfile-list (list numfile-sublist)))
       ;;end loop, when
       ))

    ;;SORT THE SUBLISTS
    (cond
     (ascending-p
      (setf sorted-sublists (nth-value 1 (my-sort-lists 0 numfile-list :ascending-p T))))
     (t (setf sorted-sublists (my-sort-lists 0 numfile-list ))))
 
    ;;BREAK INTO SORTED-FILENAME-LIST AND SORTED-PATHNAME-LIST
    (loop
     for sublist in sorted-sublists
     do
     (multiple-value-bind (num filename pathname)
         (values-list sublist)
       (when (stringp filename)
       (setf sorted-filenames (append sorted-filenames (list filename))))
       (when (stringp filename)
         (setf sorted-pathnames (append sorted-pathnames (list pathname))))
       ;;end mvb, loop
       ))
    ;;end when file-list
    )
  (values sorted-filenames sorted-pathnames)
  ;;end let,sort-directory-filenames
  ))
;;TEST
;;  (sort-directory-filenames "c:\\test\\"  :file-list  '("IMG_7563.JPG" "IMG_7561.JPG" "MVI_7562.MP4" "IMG_7569.JPG" "IMG_7567.JPG"  "MVI_7566.MP4"  "MVI_7568.MP4"))
;;   (sort-directory-filenames "F:\ALL PHOTOS-VIDEOS BY DATE") = works
;;  (sort-directory-filenames   "F:\\ALL PHOTOS-VIDEOS BY DATE\\2016_03_08-10 DEATH VALLEY TRIP")
;; RESULTS = ("IMG_7561.JPG" "IMG_7562.JPG" "IMG_7563.JPG" "IMG_7564.JPG" "IMG_7565.JPG" "IMG_7566.JPG" "IMG_7567.JPG" "IMG_7568.JPG" "IMG_75....   ....  " "F:\\ALL PHOTOS-VIDEOS BY DATE\\2016_03_08-10 DEATH VALLEY TRIP\\IMG_7729.JPG" ... ETC...  "F:\\ALL PHOTOS-VIDEOS BY DATE\\2016_03_08-10 DEATH VALLEY TRIP\\IMG_7743.JPG" "F:\\ALL PHOTOS-VIDEOS BY DATE\\2016_03_08-10 DEATH VALLEY TRIP\\MVI_7746.MP4" "F:\\ALL PHOTOS-VIDEOS BY DATE\\2016_03_08-10 DEATH VALLEY TRIP\\IMG_7747.JPG" ... ETC...  "F:\\ALL PHOTOS-VIDEOS BY DATE\\2016_03_08-10 DEATH VALLEY TRIP\\IMG_7904.JPG" "F:\\ALL PHOTOS-VIDEOS BY DATE\\2016_03_08-10 DEATH VALLEY TRIP\\IMG_7905.JPG")



;;  (find-integers-in-string  "mov_2345.mp4") 
;;  RESULTS = (2345 4)   ("mov_" 2345 ".mp" 4)   ("mov_" ".mp")
;;  (my-sort-lists 0  '((5 x y)(2 a b)(4  l m)(1 a b))  :ascending-p T)
;;  = ((5 X Y) (4 L M) (2 A B) (1 A B))  ((1 A B) (2 A B) (4 L M) (5 X Y))  NIL

  



;;WALK-DIRECTORY -- applies a function to all members of a directory that pass a test :test
;;
;;from PSeibel Practical Common Lisp p187
;;ddd   
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
          ((directory-pathname-p name)
           (when (and directories (funcall test name))
             (funcall fn name))
           (dolist (x (list-directory name)) (walk x)))
          ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))



;;sss START TESTING HERE -- LATER FIX THE ASCEND SORT PROBLEM IN U-LISTS
#|
(defun testos ()
  (setf out nil)
  (let ((dir  "C:\\3-TS\\LISP PROJECTS TS\\screensaver")
        )
    (organize-subdirs dir  :lessp :dir)
    ;;    (organize-subdirs dir  :randomp :dir)
    (fout out)
    ))|#
;;all these work? :greaterp :dir)))  :lessp :file))) :lessp :dir))) :randomp :file)))  :randomp :dir)))




;;FIND-SUBDIR-PATHS
;;2020
;;ddd
(defun find-subdir-paths (subdir-lists dirpathname &key (incl-last-slashes-p T))
  "U-files   RETURNS (values subdir-pathnames n-paths) INPUT: INCL LAST SLASHES IN dirpathname"
  (let*
      ((n-paths)
       (subdir-pathnames)
       )
    (loop
     for list in subdir-lists
     do
     (let*
         ((subdir (second list))
          (subdir-path (cond (incl-last-slashes-p
                              (format nil "~A~A\\" dirpathname subdir))
                             (T (format nil "~A~A" dirpathname subdir))))
          )
       (setf subdir-pathnames (append subdir-pathnames (list subdir-path)))
     ;;end let,loop
     ))
    (setf n-paths (list-length subdir-pathnames))
    (values  subdir-pathnames n-paths  )
    ;;end let, find-subdir-paths
    ))
;;TEST
;; (find-subdir-paths '((:DIR "0 LW-CUM-dated" "03/29/2020; 02:59 PM") (:DIR "AI-related" "06/26/2019; 10:36 PM") (:DIR "ANDY FOLDERS" "06/26/2019; 10:37 PM") (:DIR "BKHAP 2010" "06/26/2019; 10:44 PM") (:DIR "BKHAP FILES" "06/26/2019; 10:49 PM") (:DIR "csulb" "06/26/2019; 10:49 PM") (:DIR "JAVA Literature-Help" "06/26/2019; 10:49 PM") (:DIR "JAVA SUCCESS-DO NOT USE" "06/26/2019; 11:00 PM") (:DIR "JAVA SUCCESS-NB" "06/26/2019; 11:14 PM") (:DIR "JAVA SUCCESS-NB Xtra" "06/26/2019; 11:17 PM") (:DIR "JAVA SUN PROJECTS" "06/26/2019; 11:25 PM") (:DIR "LISP EXAMPLES-CODE" "06/26/2019; 11:41 PM") (:DIR "LISP PROJECTS TS" "06/26/2019; 11:55 PM") (:DIR "LISP PROJECTS TS-RUN" "06/26/2019; 11:56 PM") (:DIR "LISP PROJECTS2" "06/27/2019; 12:00 AM") )  "E:\\")
;;works= ("E:\\0 LW-CUM-dated\\" "E:\\AI-related\\" "E:\\ANDY FOLDERS\\" "E:\\BKHAP 2010\\" "E:\\BKHAP FILES\\" "E:\\csulb\\" "E:\\JAVA Literature-Help\\" "E:\\JAVA SUCCESS-DO NOT USE\\" "E:\\JAVA SUCCESS-NB\\" "E:\\JAVA SUCCESS-NB Xtra\\" "E:\\JAVA SUN PROJECTS\\" "E:\\LISP EXAMPLES-CODE\\" "E:\\LISP PROJECTS TS\\" "E:\\LISP PROJECTS TS-RUN\\" "E:\\LISP PROJECTS2\\")     15



;;LIST-FILES
;;2017, added ext-list 2019
;;DDD
(defun list-files (dir &key incl-exts omit-exts)
  "in u-files, returns (values filenames filepaths file-realpaths). uses separate-files-from-dirs"
  (let*
      ((paths (list-directory dir))       
       )
    (multiple-value-bind (file-realpaths sdpaths filenames sdnames
                                         filepaths sdpathnames)
        ;;ile-list subdir-list file-name-list subdir-name-list
        (separate-files-from-dirs dir :return-subdir-info-p nil
                                  :incl-exts incl-exts :omit-exts omit-exts)

      (values filenames filepaths file-realpaths)
      ;;end mvb, let, list-files
      )))
;;TEST
;; (LIST-FILES "C:\\TEMP\\")
;; WORKS=
;; FILENAMES= ("SWITCHSETUP.EXE" "SETUP_XRECODE3_WIN_64BIT_1.56.EXE" "NETBEANS-8.2-WINDOWS.EXE" "JDK-8U121-WINDOWS-X64.EXE" "EDITORIAL STAFF DIRECTORY - LA TIMES.HTM" "CORAL TEST.CDR" "CHROME SETUP.EXE" "BITDEFENDER_WINDOWS_BC3DB0E0-1194-4117-97EC-46995EDD53A7.EXE")
;;FILEPATHS= ("C:\\TEMP\\SWITCHSETUP.EXE" "C:\\TEMP\\SETUP_XRECODE3_WIN_64BIT_1.56.EXE" "C:\\TEMP\\NETBEANS-8.2-WINDOWS.EXE" "C:\\TEMP\\JDK-8U121-WINDOWS-X64.EXE" "C:\\TEMP\\EDITORIAL STAFF DIRECTORY - LA TIMES.HTM" "C:\\TEMP\\CORAL TEST.CDR" "C:\\TEMP\\CHROME SETUP.EXE" "C:\\TEMP\\BITDEFENDER_WINDOWS_BC3DB0E0-1194-4117-97EC-46995EDD53A7.EXE")
;;FILE-REALPATHS= (#P"C:/TEMP/SWITCHSETUP.EXE" #P"C:/TEMP/SETUP_XRECODE3_WIN_64BIT_1.56.EXE" #P"C:/TEMP/NETBEANS-8.2-WINDOWS.EXE" #P"C:/TEMP/JDK-8U121-WINDOWS-X64.EXE" #P"C:/TEMP/EDITORIAL STAFF DIRECTORY - LA TIMES.HTM" #P"C:/TEMP/CORAL TEST.CDR" #P"C:/TEMP/CHROME SETUP.EXE" #P"C:/TEMP/BITDEFENDER_WINDOWS_BC3DB0E0-1194-4117-97EC-46995EDD53A7.EXE")
;;FILTERING OUT FILES
;; (list-files "C:/3-TS/LISP PROJECTS TS/CogSys-Model" :incl-exts '("lisp") :omit-exts '("lisp~"))
;; (equal "lisp" "lisp~")
;; (pathname-type #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-net-view-Utils.lisp~")
;;TEST
;; (LIST-FILES "C:\\TEMP\\")
;; WORKS=
;; FILENAMES= ("SWITCHSETUP.EXE" "SETUP_XRECODE3_WIN_64BIT_1.56.EXE" "NETBEANS-8.2-WINDOWS.EXE" "JDK-8U121-WINDOWS-X64.EXE" "EDITORIAL STAFF DIRECTORY - LA TIMES.HTM" "CORAL TEST.CDR" "CHROME SETUP.EXE" "BITDEFENDER_WINDOWS_BC3DB0E0-1194-4117-97EC-46995EDD53A7.EXE")
;;FILEPATHS= ("C:\\TEMP\\SWITCHSETUP.EXE" "C:\\TEMP\\SETUP_XRECODE3_WIN_64BIT_1.56.EXE" "C:\\TEMP\\NETBEANS-8.2-WINDOWS.EXE" "C:\\TEMP\\JDK-8U121-WINDOWS-X64.EXE" "C:\\TEMP\\EDITORIAL STAFF DIRECTORY - LA TIMES.HTM" "C:\\TEMP\\CORAL TEST.CDR" "C:\\TEMP\\CHROME SETUP.EXE" "C:\\TEMP\\BITDEFENDER_WINDOWS_BC3DB0E0-1194-4117-97EC-46995EDD53A7.EXE")
;;FILE-REALPATHS= (#P"C:/TEMP/SWITCHSETUP.EXE" #P"C:/TEMP/SETUP_XRECODE3_WIN_64BIT_1.56.EXE" #P"C:/TEMP/NETBEANS-8.2-WINDOWS.EXE" #P"C:/TEMP/JDK-8U121-WINDOWS-X64.EXE" #P"C:/TEMP/EDITORIAL STAFF DIRECTORY - LA TIMES.HTM" #P"C:/TEMP/CORAL TEST.CDR" #P"C:/TEMP/CHROME SETUP.EXE" #P"C:/TEMP/BITDEFENDER_WINDOWS_BC3DB0E0-1194-4117-97EC-46995EDD53A7.EXE")
;;FILTERING OUT FILES
;; (list-files "C:/3-TS/LISP PROJECTS TS/CogSys-Model" :incl-exts '("lisp") :omit-exts '("lisp~"))
;; (equal "lisp" "lisp~")
;; (pathname-type #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-net-view-Utils.lisp~")
;;WHEN FILTER OUT FILE TYPES
;; (LIST-FILES "C:\\TEMP\\" :incl-exts '("jpg" "lisp"))
;; 



;;GET-FILES-W-EXTS
;;2018, 2019 added omit-exts
;;DDD
(defun get-files-w-exts (files &key (exts '("lisp" "txt"))
                               omit-exts)
  "U-Files. If exts, only includes files with those extensions.  If exts=NIL, and omit-exts, incls all files except omit-exts. Works on string and real pathnames."
  (LET
      ((RETURN-FILES)
       )
    (LOOP
     FOR FILE IN FILES
     DO
     (LET
         ((EXT (PATHNAME-TYPE FILE))
          )
       (cond
        ;;exclude omit-exts
        ((and omit-exts (member ext omit-exts :test 'string-equal)) NIL)
        ;;exclude if exts and not a member of exts
       ((and exts (null (MEMBER EXT EXTS :TEST 'STRING-EQUAL)))
        NIL)
       ;;otherwise include the file
       (t 
         (SETF RETURN-FILES (APPEND RETURN-FILES (LIST FILE)))))
       ;;END LET,LOOP
       ))
    RETURN-FILES
    ;;END LET,GET-FILES-W-EXTS
    ))
;;TEST
;; (GET-FILES-W-EXTS '("F1.LISP" "F2.DOC" "F3.TXT" "F4.LISP~" "F5.FASL" "F6.LISP"))
;;WORKS= ("F1.LISP" "F3.TXT" "F6.LISP")
;; (GET-FILES-W-EXTS '("F1.LISP" "F2.DOC" "F3.TXT" "F4.LISP~" "F5.FASL" "F6.LISP") :exts NIL :omit-exts '("doc"))
;; works= ("F1.LISP" "F2.DOC" "F3.TXT" "F4.LISP~" "F5.FASL" "F6.LISP")
;;FOR PATHNAMES
;; ;; (GET-FILES-W-EXTS   '(#P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-net-view-utils.lisp" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CSQ.lisp~" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CS.lisp~" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CS.lisp" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CS-symbol-trees.lisp~" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CS-symbol-trees.lisp" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CS-OLD.lisp~" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CS-OLD.lisp")  :EXTS '(LISP))                       
;; WORKS = (#P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-net-view-utils.lisp" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CS.lisp" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CS-symbol-trees.lisp" #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/U-CS-OLD.lisp")
;;FILTER FILE TYPES
;; (LIST-FILES "C:\\TEMP\\" :incl-exts '("jpg" "lisp"))
;; WORKS= ("settings-X.lisp" "my-write-test2.lisp" "IMG_6815.JPG" "IMG_6813.JPG" "IMG_6162.JPG" "IMG_5938.JPG" "dir-db-settings.lisp" "2018-09-29 19.53.04.jpg" "2018-09-09 19.21.59.jpg" "2018-06-12 19.15.25.jpg")                ("C:\\TEMP\\settings-X.lisp" "C:\\TEMP\\my-write-test2.lisp" "C:\\TEMP\\IMG_6815.JPG" "C:\\TEMP\\IMG_6813.JPG" "C:\\TEMP\\IMG_6162.JPG" "C:\\TEMP\\IMG_5938.JPG" "C:\\TEMP\\dir-db-settings.lisp" "C:\\TEMP\\2018-09-29 19.53.04.jpg" "C:\\TEMP\\2018-09-09 19.21.59.jpg" "C:\\TEMP\\2018-06-12 19.15.25.jpg")                 (#P"C:/TEMP/settings-X.lisp" #P"C:/TEMP/my-write-test2.lisp" #P"C:/TEMP/IMG_6815.JPG" #P"C:/TEMP/IMG_6813.JPG" #P"C:/TEMP/IMG_6162.JPG" #P"C:/TEMP/IMG_5938.JPG" #P"C:/TEMP/dir-db-settings.lisp" #P"C:/TEMP/2018-09-29 19.53.04.jpg" #P"C:/TEMP/2018-09-09 19.21.59.jpg" #P"C:/TEMP/2018-06-12 19.15.25.jpg")
;; ;; (LIST-FILES "C:\\TEMP\\" :omit-exts '("jpg" "lisp" "gif" "exe" "pdf" "html"))
;; WORKS= ("test-newnameX4.txt" "test-newnameX3.txt" "test-newname.txt" "RunSHAQ.exe.lwheap" "Paris-Lisa etc.jpeg" "Paris-2017-Sher-Centre du Monde.jpeg" "My SHAQ Results2.txt" "My SHAQ Results.txt" "MediaMonkey Scan Log (7-27-2019).txt" "Editorial Staff Directory - LA Times.htm" "coral test.cdr")                 ("C:\\TEMP\\test-newnameX4.txt" "C:\\TEMP\\test-newnameX3.txt" "C:\\TEMP\\test-newname.txt" "C:\\TEMP\\RunSHAQ.exe.lwheap" "C:\\TEMP\\Paris-Lisa etc.jpeg" "C:\\TEMP\\Paris-2017-Sher-Centre du Monde.jpeg" "C:\\TEMP\\My SHAQ Results2.txt" "C:\\TEMP\\My SHAQ Results.txt" "C:\\TEMP\\MediaMonkey Scan Log (7-27-2019).txt" "C:\\TEMP\\Editorial Staff Directory - LA Times.htm" "C:\\TEMP\\coral test.cdr")                 (#P"C:/TEMP/test-newnameX4.txt" #P"C:/TEMP/test-newnameX3.txt" #P"C:/TEMP/test-newname.txt" #P"C:/TEMP/RunSHAQ.exe.lwheap" #P"C:/TEMP/Paris-Lisa etc.jpeg" #P"C:/TEMP/Paris-2017-Sher-Centre du Monde.jpeg" #P"C:/TEMP/My SHAQ Results2.txt" #P"C:/TEMP/My SHAQ Results.txt" #P"C:/TEMP/MediaMonkey Scan Log (7-27-2019).txt" #P"C:/TEMP/Editorial Staff Directory - LA Times.htm" #P"C:/TEMP/coral test.cdr")



;;FILTER-FILE-EXT
;;2019
;;ddd
(defun filter-file-ext (path &key incl-exts omit-exts)
  "U-files. Works on strings or real-paths. the exts must be strings w/o period."
  (let*
      ((ext (pathname-type path))
       (result)
       )
    (cond
        ;;exclude omit-exts
        ((and omit-exts (member ext omit-exts :test 'string-equal)) nil)
        ;;exclude if exts and not a member of exts
       ((and incl-exts (null (member ext incl-exts :test 'string-equal)))
        NIL)
       ;;otherwise include the file
       (t (setf result path)))
    result
    ;;end let, filter-file-ext
    ))
;;TEST
;; (filter-file-ext #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-explore-Questions.lisp~" :incl-exts '("lisp")) 
;; works= NIL
;; (filter-file-ext #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-explore-Questions.lisp~")  = #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-explore-Questions.lisp~"
;; (filter-file-ext #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-explore-Questions.lisp~" :omit-exts '("lisp~")) = NIL



;;ORGANIZE-SUBDIRS
;;
;;ddd
(defun organize-subdirs (main-dir sort-type by-dir-or-file)
  "In U-files, used by screensaver, sorts subdirs GIVEN FLAT OR TREE DIR, uses flatten-list-tree function in U-lists.lisp. Returns organized-dir-list flat-list simple-list. sort-type randomp, lessp or greaterp, by-dir-or-file is as-is dir or file"
  (let
      ((flat-subdirs-list)
       (flat-subdirs-list2)
       (simple-all-files-list)
       (simple-all-files-list2)
       (organized-file-list)
       (last-sub-files-list)
       (ufiles-out)
       (r)
       (a)
       (d)
       )
    (cond
     ((directory main-dir)
    ;;first flatten the main dir tree
    (setf flat-subdirs-list 
        (make-flat-dirs-list main-dir *reverse-order-p))
    ;;(afout  '*ufiles-out (format nil "In organize-subdirs, flat-subdirs-list= ~A~%" flat-subdirs-list))   ;;400 t)

    ;;first, determine type of organiztion 
    (cond
     ((equal sort-type :randomp)
      (setf r t))
     ((equal sort-type :lessp)
      (setf d t))
     ((equal sort-type :greaterp)
      (setf a t)))

    ;;then organize the flatten-list-tree by whatever means is specified by sort-type
    ;;if organize by dir
    (cond
     ;;To get a flat random all files list, first find the simple-all-files-list
     ((equal by-dir-or-file :file)
      (multiple-value-setq (flat-subdirs-list2 simple-all-files-list)
          (flatten-list-tree flat-subdirs-list))
      (setq organized-file-list  ;;xxx added list to avoid error of not deep enough lists
            (list (organize-sublists simple-all-files-list :randomize  r  :descend-sort d  :ascend-sort a))))
     ;;organize by dir is the default
     (t   
      (if (equal by-dir-or-file :as-is)
          (setf d t a nil r nil))
      (setq organized-file-list 
            (organize-sublists flat-subdirs-list :randomize  r  :descend-sort d  :ascend-sort a))))
    ;;return 3 lists
  ;;  (afout '*ufiles-out (format nil  "organized-file-list= ~A~%~%" organized-file-list))
  ;;  (fout *ufiles-out)
  )
     (t (show-text (format nil "DIRECTORY= ~A DOES NOT EXIST~%" main-dir) 60 nil))
     )
    (values organized-file-list flat-subdirs-list simple-all-files-list)
    ;;end let, organize-subdirs
    ))
;;TEST
;;(testmfd)
;; Test  works
#|(defun testmfd ()
  (setf *recursion-limit 5
        *reverse-order-p nil)
  (make-flat-dirs-list  "C:\\3-TS\\LISP PROJECTS TS\\screensaver"*reverse-order-p)
  (fout  *ufiles-out)
  ;;was "c:\\temp")
  )|#




;;MAKE-FLAT-DIRS-LIST
;;
;;;;USE AS A MODEL FOR RECURSION???
;;works
;;ddd
(defun make-flat-dirs-list (dir &optional reverse-order-p &key return-namestrings-p
                                (recursion-limit 10) return-flat-filelists-p n)
  "In U-Files.lisp, will take dir tree with scattered files at all levels. It separates files IN EACH DIR INTO OWN LIST FOR EACH DIR-BRANCH in the file-tree.  RETURNS (values flat-dirs-paths flat-dirs-filenames flat-filepath-lists  all-flat-filenames) if RETURN-NAMESTRINGS-P  Returns flat-dirs-paths (path OBJECTS)= a flat list of all these DIRS (each with own FILES-only list) irrespective of their level in the dir tree. "
  (setf *ufiles-out nil
        n 0)
  (unless (boundp '*recursion-limit)
    (setf *recursion-limit recursion-limit))
  (let
      ((flat-dirs-filenames)
       (all-flat-filenames)
       (flat-dirs-paths)
       (flat-filepath-lists)
       (sub-dir-list  (list-directory dir))
       )
    (cond
     ((directory dir)
    ;;make list of sub-dirs of files and completely flat list of all files
     (multiple-value-setq (flat-dirs-paths sub-dir-list) 
        (make-flat-dirs-list1 sub-dir-list 0 flat-dirs-paths :reverse-order-p reverse-order-p))
;; (afout '*ufiles-out (format nil "In make-flat-dirs-list, FINAL, flat-dirs-paths= ~A~% sub-dir-list= ~A~% ~% simple-all-files-list= ~A~%"flat-dirs-paths sub-dir-list simple-all-files-list ))
    ;;now print out the output to a output-window 
  ;;  (fout *ufiles-out)
    )
     (t (show-text (format nil "In make-flat-dirs-list, DIRECTORY= ~A DOES NOT EXIST" dir) 60 nil)))
    
    ;;FLATTEN FILE-LISTS?
    (when return-flat-filelists-p
      (setf flat-filepath-lists (flatten-1-level flat-dirs-paths)))

    ;;RETURN FILE NAMESTRINGS (for both grouped files and flat list)
    (when return-namestrings-p 
      (let ((dirfilenames))
        (dolist (filelist flat-dirs-paths)
          (setf dirfilenames (append dirfilenames (namestrings-list filelist)))
          (setf flat-dirs-filenames (append flat-dirs-filenames (list dirfilenames)))))
      ;;end when
      )
    (when (and return-namestrings-p return-flat-filelists-p)
      (setf all-flat-filenames (namestrings-list flat-filepath-lists)))
    (values flat-dirs-paths flat-dirs-filenames
          flat-filepath-lists  all-flat-filenames) 
    ;;end let, make-flat-dirs-list
    ))
;;TEST
;; (make-flat-dirs-list  "C:\\Dropbox-ADD\\DOCS" nil :return-flat-filelists-p T :return-namestrings-p T)
;; WORKS= 
;;FLAT-DIRS-PATHS=  ((#P"C:/Dropbox-ADD/DOCS/cancer stopper_files/arrow.gif" #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/bnr_overstock_020905.gif" #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/button_search.gif" #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/de.gif" #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/du.gif"... ETC...     #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/title_daily_news.gif" #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/us.gif")            (#P"C:/Dropbox-ADD/DOCS/2018 tax year/2018 861 Aston Year Data.pdf" #P"C:/Dropbox-ADD/DOCS/2018 tax year/2018 861 Aston Year Data_1.pdf"   ... ETC...     #P"C:/Dropbox-ADD/DOCS/2018 tax year/ToMaui Hertz E-Return 830889010.pdf" #P"C:/Dropbox-ADD/DOCS/2018 tax year/TomTiaa-5498.pdf") (#P"C:/Dropbox-ADD/DOCS/cancer stopper.htm" #P"C:/Dropbox-ADD/DOCS/TomSherryLetterHead.docx"))
;;FLAT-DIRS-FILENAMES=  (("C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\arrow.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\bnr_overstock_020905.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\button_search.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\de.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\du.gif"   ... ETC .... "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\sp.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\spacer.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\title_daily_news.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\us.gif")          ("C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\arrow.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\bnr_overstock_020905.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\button_search.gif"     .ETC...  "C:\\Dropbox-ADD\\DOCS\\2018 tax year\\ToMaui Hertz E-Return 830889010.pdf" "C:\\Dropbox-ADD\\DOCS\\2018 tax year\\TomTiaa-5498.pdf")         ("C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\arrow.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\bnr_overstock_020905.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\button_search.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\de.gif"   ... ETC...      "C:\\Dropbox-ADD\\DOCS\\TomSherryLetterHead.docx"))
;;FLAT-FILEPATH-LISTS=   (#P"C:/Dropbox-ADD/DOCS/cancer stopper_files/arrow.gif" #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/bnr_overstock_020905.gif" #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/button_search.gif" #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/de.gif" #P"C:/Dropbox-ADD/DOCS/cancer stopper_files/du.gif"    ...  ETC ....  #P"C:/Dropbox-ADD/DOCS/2018 tax year/ToMaui Hertz E-Return 830889010.pdf" #P"C:/Dropbox-ADD/DOCS/2018 tax year/TomTiaa-5498.pdf" #P"C:/Dropbox-ADD/DOCS/cancer stopper.htm" #P"C:/Dropbox-ADD/DOCS/TomSherryLetterHead.docx")
;;ALL-FLAT-FILENAMES=    ("C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\arrow.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\bnr_overstock_020905.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\button_search.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\de.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\du.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\es.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\footer_disclaimer.gif" "C:\\Dropbox-ADD\\DOCS\\cancer stopper_files\\fr.gif"      ...  ETC..     "C:\\Dropbox-ADD\\DOCS\\2018 tax year\\Tom Tiaa1099-Rstate.pdf" "C:\\Dropbox-ADD\\DOCS\\2018 tax year\\ToMaui Hertz E-Return 830889010.pdf" "C:\\Dropbox-ADD\\DOCS\\2018 tax year\\TomTiaa-5498.pdf" "C:\\Dropbox-ADD\\DOCS\\cancer stopper.htm" "C:\\Dropbox-ADD\\DOCS\\TomSherryLetterHead.docx")
;;  (make-flat-dirs-list  "C:\\3-TS\\LISP PROJECTS TS\\h-help" NIL return-flat-filelists-p)
;; (make-flat-dirs-list  "C:\\3-TS\\LISP PROJECTS TS\\h-help")

;;  (make-flat-dirs-list  "C:\\3-TS\\LISP PROJECTS TS\\h-help" NIL :RETURN-FLAT-FILELISTS-P T  :RETURN-NAMESTRINGS-P T)
;;works=
;;FLAT-DIRS-PATHS: ((#P"C:/3-TS/LISP PROJECTS TS/h-help/Math-circle CENTER from radius and 2pts_files/610a8a95f861678ab9f72ee483d1c1a7" #P"C:/3-TS/LISP PROJECTS TS/h-help/Math-circle CENTER from radius and 2pts_files/analytics.js.download" #P"C:/3-TS/LISP PROJECTS TS/h-help/Math-circle CENTER from radius and 2pts_files/bbca0e07ecdf80994bcebc07561bd643" ...   ETC ...   #P"C:/3-TS/LISP PROJECTS TS/h-help/Math-circle CENTER from radius and 2pts_files/yrJ91.jpg")           (#P"C:/3-TS/LISP PROJECTS TS/h-help/1-clhelp-notes.lisp" #P"C:/3-TS/LISP PROJECTS TS/h-help/1-KEY BINDINGS" #P"C:/3-TS/LISP PROJECTS TS/h-help/1-KEY BINDINGS~" #P"C:/3-TS/LISP PROJECTS TS/h-help/2-UsefulFunctionsFromLW.lisp" #P"C:/3-TS/LISP PROJECTS TS/h-help/2-UsefulFunctionsFromLW.lisp~....   ETC    ...    #P"C:/3-TS/LISP PROJECTS TS/h-help/test-loop.lisp" #P"C:/3-TS/LISP PROJECTS TS/h-help/U-Files.lisp - Shortcut.lnk" #P"C:/3-TS/LISP PROJECTS TS/h-help/U-Format-Print-Read-IOetc.lisp" #P"C:/3-TS/LISP PROJECTS TS/h-help/U-Format-Print-Read-IOetc.lisp~" #P"C:/3-TS/LISP PROJECTS TS/h-help/X-Y TRANSFORMATION.pdf"))
;; FLAT-DIRS-FILENAMES    =  (("C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\610a8a95f861678ab9f72ee483d1c1a7" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\analytics.js.download" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\bbca0e07ecdf80994bcebc07561bd643" . . . ETC . . .   "C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\yrJ91.jpg")                     ("C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\610a8a95f861678ab9f72ee483d1c1a7" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\analytics.js.download" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\bbca0e07ecdf80994bcebc07561bd643" . . . ETC . . .  "C:\\3-TS\\LISP PROJECTS TS\\h-help\\test-loop.lisp" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\U-Files.lisp - Shortcut.lnk" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\U-Format-Print-Read-IOetc.lisp" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\U-Format-Print-Read-IOetc.lisp~" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\X-Y TRANSFORMATION.pdf"))
;; FLAT-FILEPATH-LISTS =   (#P"C:/3-TS/LISP PROJECTS TS/h-help/Math-circle CENTER from radius and 2pts_files/610a8a95f861678ab9f72ee483d1c1a7" #P"C:/3-TS/LISP PROJECTS TS/h-help/Math-circle CENTER from radius and 2pts_files/analytics.js.download" #P"C:/3-TS/LISP PROJECTS TS/h-help/Math-circle CENTER from radius and 2pts_files/bbca0e07ecdf80994bcebc07561bd643"  . . .  ETC . . .     #P"C:/3-TS/LISP PROJECTS TS/h-help/Per-cell-X.lisp~" #P"C:/3-TS/LISP PROJECTS TS/h-help/test-loop.lisp" #P"C:/3-TS/LISP PROJECTS TS/h-help/U-Files.lisp - Shortcut.lnk" #P"C:/3-TS/LISP PROJECTS TS/h-help/U-Format-Print-Read-IOetc.lisp" #P"C:/3-TS/LISP PROJECTS TS/h-help/U-Format-Print-Read-IOetc.lisp~" #P"C:/3-TS/LISP PROJECTS TS/h-help/X-Y TRANSFORMATION.pdf")
;; ALL-FLAT-FILENAMES=  ("C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\610a8a95f861678ab9f72ee483d1c1a7" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\analytics.js.download" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\Math-circle CENTER from radius and 2pts_files\\bbca0e07ecdf80994bcebc07561bd643" . . . ETC . . .  "C:\\3-TS\\LISP PROJECTS TS\\h-help\\test-loop.lisp" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\U-Files.lisp - Shortcut.lnk" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\U-Format-Print-Read-IOetc.lisp" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\U-Format-Print-Read-IOetc.lisp~" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\X-Y TRANSFORMATION.pdf")






;;works after much work 
;;USE AS A MODEL FOR RECURSION???
;;MAKE-FLAT-DIRS-LIST1
;;
;;ddd
(defun make-flat-dirs-list1 (dir-list N &optional  dir-flat-list &key reverse-order-p)
  "In U-files.lisp, works with make-flat-dirs-list as recursor- USE AS MODEL?"
  (let
      ((new-dir-list)
       (sub-dir-list)
       (return-sub-dir-list)
        )   
    (incf n)
    ;;   (afout '*ufiles-out (format nil "0-IN MAKE-FLAT-DIRS-LIST1, dir-list ~A~%" dir-list))

    ;;for each element in the list -- may be file or dir or subdir
    (dolist (element dir-list)
      ;;keeps from appending extra first dir element files extra time
      (setf return-sub-dir-list nil
            new-dir-list nil)
      
      #|     (afout '*ufiles-out
             (format nil "1-IN MAKE-FLAT-DIRS-LIST1,element= ~A~%~%dir-list= ~A~%~% sub-dir-list= ~A~%~%"  element dir-list sub-dir-list)) ;; dir-flat-list ))|#

      (cond
       ((null (setf new-dir-list (list-directory element)))
        (cond
         ((and (null reverse-order-p)
               (setf sub-dir-list (append (list element) sub-dir-list))))
        (t (setf sub-dir-list (append sub-dir-list (list element)))))
        ;;end of null clause
        )          
       ;;cut off process if limit reached
       ((>  n *recursion-limit) nil)
       ;;tests to see if it is a directory or a file- returns file list if a dir        
       ((setf new-dir-list (list-directory element))        
        (multiple-value-setq (dir-flat-list return-sub-dir-list)
            (make-flat-dirs-list1 new-dir-list  n dir-flat-list :reverse-order-p reverse-order-p))
        ;;moved from end to here to avoid append extra list at bottom of recursion
        ;;adding the unless clause here and at end stopped the extra list being appended at end
        (unless (equal return-sub-dir-list (car  (last dir-flat-list)))
          (cond
           ((and (null reverse-order-p) return-sub-dir-list)
            (setf dir-flat-list (append (list return-sub-dir-list)  dir-flat-list)))

           ;;was  (if return-sub-dir-list (setf dir-flat-list (append dir-flat-list (list return-sub-dir-list))))
           (return-sub-dir-list
             (setf dir-flat-list (append dir-flat-list (list return-sub-dir-list)))
             )))
        ;;end setf clause
        )      
       (t nil))
      ;;end dolist
      )
    ;;append the dir-flat-list for this level (end of dolist when all elements processed)
    ;;prevent repeating append of last appended list
    (unless (equal sub-dir-list (car (last dir-flat-list)))
      (if sub-dir-list (setf dir-flat-list (append dir-flat-list (list sub-dir-list)))))
    ;;set it to nil??  sub-dir-list nil)
    ;;   (afout '*ufiles-out  (format nil "3-At END OF make-flat-dirs-list1 N= ~A~%~% dir-flat-list= ~A~%~% sub-dir-list= ~A~%~%return-sub-dir-list= ~a~%" N dir-flat-list sub-dir-list return-sub-dir-list) )
    (values dir-flat-list sub-dir-list) 
    ;;end let, make-flat-dirs-list1
    ))
    


;;MY-DOS-DRIVES-INFO
;;2020
;;ddd
(defun my-dos-drives-info (top-dir-pathnames &key  
                                             popup-drive-info-p (clean-host-path-p T)
                                             (location "TomPC") type brand dsize
                                             (last-leveln *tomex-data-last-level) (leveln 0)
                                             (scandate (date-string))
                                             set-dirsym-to-symlist-p 
                                             not-return-all-info-symvals-p
                                             set-all-drive-info-to-sym  ;;*tomex-drive-symlists
                                             set-all-dirsyms-to-symvals-p 
                                             (drive-letters '(D E F G H I J K L M N O P Q R S T U V))
                                             (omit-drive-letters '(C)) 
                                             wildcard-p attributes format  sort-order
                                             directories-p lowercase-p  
                                             read-only-p hidden-p system-files-p not-content-indexed-p 
                                             archive-ready-p 
                                             bare-p owner-p  4-digit-years-p reparse-points-p 
                                             (show-cmd-p t)
                                             return-dos-output-t return-all-lines-p  return-line-lists-p
                                             (max-lines 25000) dos-out-prefix)
  "U-files   RETURNS (values level1-dirsyms level1-dir-paths found-drive-letters all-top-path-keylists)  INPUT:  Will 1. FIND all DRIVES then 2 return extensive drive & dir info.  CAN SET INFO TO DIRSYMS and/or return it all as keylists in form of: (list dirsym cur-dir host volname serial-num  :n-dirs  n-dirs :n-files n-files  :used-size used-size :free-size free-size  :dirlists dirlists :filelists filelists  :dos-out-string dos-out-string :dos-out dos-out :all-line-lists :all-line-lists) DEPENDING ON ARGS.
   INPUT: note:  must incl \\ eg  c:\\ (not c: ) or scans current dir.
   SUBDIRS LISTED TWICE (once in :DIRLIST with only minimal info, and in :SUBDIRS (if <= last-leveln) which includes complete info--DIRS and FILES).
   WILDCARD-P  is when path includes eg. \"*.lisp\".
   ATTRIBUTES [d=directories, r=read-only files, h=hidden files, a=files ready for archiving, s=system files, i=not content indexed files, l=reparse points,  - =prefix meaning not.] 
   FORMAT: is :wide, :col (same as wide, but by cols), or nil.
   SUBDIR-FILES-P: displays files in SPECIFIED DIRECTORY AND ALL SUBDIRECTORIES.   
   BARE-P= (no headings), /-c=no thousand sep, /d=sorted by col,      
   SORT-ORDER:  :NAME, :size (smallest first), :extension, :dir-first, :create-date, :last-access,  :last-access,:last-write [date/time (oldest first)]   -  =prefix to reverse order.
   NOTE: following not used:  /r=display alternate data streams of the file,   /x=this displays the short names generated for non-8dot3 file,  /p=pauses after each screenful of information, 
   RETURN-LINE-LISTS-P returns all lines as lists of tokens.   
   LIST FORMAT: (top-dirsym (name full-path etc for leveln= 1)(( level 2 subdirlist 1)(level 2 subdir list2)) etc."
  ;;FIND ALL DRIVES, syms, paths, letters OR MAKE ONE PATH SYM per top path.
  (let*
      ((level1-dirsyms)
       (level1-dir-paths)
       (dir-paths)
       (dirpath-all-level-info)
       (all-top-path-keylists)
       (subdirs-lists)
       (dirsyms)
       (found-drive-paths)
       (found-drive-letters)
       (n-drives)
       )
    ;;INCF LEVELN
    (incf leveln)
    ;;EXPLORE GIVEN PATHNAMES OR DRIVES?
    (cond
     (top-dir-pathnames
      (setf dir-paths top-dir-pathnames
            found-drive-letters (nth-value 1 (return-first-letters/integers dir-paths))))
     ;;dirsyms (make-dir-syms-from-paths dir-paths)))
     (T  (multiple-value-bind (dirsyms1 found-drive-paths found-drive-letters 
                                        n-drives)
             (list-all-drives-info :drive-letters drive-letters :last-leveln 0)
           (setf dir-paths found-drive-paths))))

    ;;EXPLORE ALL DRIVES/DIRS
    (loop
     for dir-path in dir-paths
     ;;for dirsym in dirsyms
     ;;no for drive-letter in found-drive-letters
     do
     (let*
         ((path-info-keylist)
          (dirsym)
          (subdir-paths)     
          (popup-keylist)
          )
       ;;(break "dir-paths")
       (multiple-value-bind  (cur-dir host volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out all-line-lists)
           (my-dos-dir dir-path :wildcard-p wildcard-p :attributes  attributes 
                       :format format :sort-order sort-order
                       :clean-host-path-p clean-host-path-p
                       :directories-p directories-p :lowercase-p lowercase-p 
                       :read-only-p read-only-p :hidden-p  hidden-p 
                       :system-files-p system-files-p 
                       :not-content-indexed-p not-content-indexed-p 
                       :archive-ready-p archive-ready-p :bare-p  bare-p 
                       :owner-p owner-p :4-digit-years-p  4-digit-years-p 
                       :reparse-points-p reparse-points-p :show-cmd-p show-cmd-p
                       :return-dos-output-t return-dos-output-t
                       :return-all-lines-p return-all-lines-p 
                       :return-line-lists-p return-line-lists-p
                       :max-lines max-lines :dos-out-prefix dos-out-prefix)

         ;;LEVEL1: MAKE DIRSYM? DIR-PATHS
         (cond
          (volname
           (setf dirsym (my-make-symbol volname)
                 level1-dirsyms (append level1-dirsyms (list dirsym))))
          (T (setf dirsym (car (make-dirsyms (list dir-path)))
                   level1-dirsyms (append level1-dirsyms (list dirsym)))))
         ;;accumulate level1-dir-paths
         (setf level1-dir-paths (append level1-dir-paths (list dir-path)))

         ;;(break "after my-dos-dir")
         ;;for later
         (setf subdirs-lists dirlists)
         
         (cond
          ((= leveln 1)
           (when popup-drive-info-p
             (loop
              for key-title in '(" DRIVE LOCATION " " DRIVE TYPE " 
                                 " DRIVE BRAND "  " DRIVE SIZE ")
              for key in '(:location :type :brand :dsize)
              do
             (my-call-input-text "type here" :title key-title
                                :win-title "INPUT WINDOW" :close-interface-p NIL
                                :poke-special-process-p T)
             (setf  popup-keylist (append popup-keylist 
                          (list key *TEXT-INPUT-OR-BUTTON-INTERFACE-TEXTDATA)))
             ;;end loop,when                                
             ))
           ;;SET LEVEL1 PATH-INFO-KEYLIST
           (setf path-info-keylist
                 (list dirsym cur-dir :host host 
                       :path (directory-namestring dir-path)
                       :volname volname 
                       :location location ;;get
                       :type type ;;get
                       :brand brand ;;get
                       :dsize dsize ;;get
                       :scandate scandate ;;get
                       :last-leveln last-leveln ;;get
                       :serial-num serial-num
                       :n-dirs  n-dirs :n-files n-files 
                       :used used-size :free free-size  
                       :dirlists dirlists :filelists filelists :scandate scandate
                       :dos-out-string dos-out-string :dos-out dos-out
                       :all-line-lists all-line-lists))
           ;;end leveln = 1
           )
          (T (setf path-info-keylist
                   (list dirsym cur-dir :host host 
                         :path (directory-namestring dir-path)
                         :volname volname 
                         :n-dirs  n-dirs :n-files n-files 
                         :used used-size :free free-size  
                         :dirlists dirlists :filelists filelists :scandate scandate
                         :dos-out-string dos-out-string :dos-out dos-out
                         :all-line-lists all-line-lists))))
           

         ;;SET A SYMBOL TO path-info-keylist
         (when (and set-all-dirsyms-to-symvals-p 
                    dirsym (symbolp dirsym))
           (set dirsym path-info-keylist))
         ;;(break "AFTER path-info-keylist")
         ;;(afout 'out (format nil "PATH-INFO-KEYLIST= ~S~% SUBDIRS-LIST= ~S" path-info-keylist subdirs-lists))

         ;;ACCUMULATE? path-info-keylist
         #|         (when (and (= leveln 1) path-info-keylist) ;;dirpath-all-level-info
             (setf dirpath-all-level-info (list dirsym :LEVELN= leveln)))|#
         ;;START LEVELN = 1 dirpath-all-level-info with DIRSYM
         #|         (when path-info-keylist  DO LATER??
           (setf dirpath-all-level-info (append dirpath-all-level-info path-info-keylist)))|#
         ;;(break "after dirpath-all-level-info")
         #|         (when path-info-keylist  
           (cond
            ((= leveln 1)
             (setf dirpath-all-level-info (list dirsym path-info-keylist)));; :LEVELN= leveln)))
            (T (setf dirpath-all-level-info (append dirpath-all-level-info
                                                    (list path-info-keylist)))))) |#
         ;;(break "after my-dos-dir")

         ;;RECURSE ON SUBDIRS?  [level 1 means no recurse]
         (when (and subdirs-lists (>  last-leveln leveln)) 
           (setf subdir-paths (find-subdir-paths subdirs-lists dir-path)) 
           ;;(afout 'out (format nil "BEFORE RECURSE: leveln= ~A SUBDIR-PATHS= ~S" leveln subdir-paths))
           ;;(break "subdirs-paths")
           (multiple-value-bind (dirsyms1 dir-paths1 found-drive-letters1 
                                          dirpath-all-level-info1) ;; dirpath-all-level-info)
               ;;  (my-dos-drives-info  ("0 LW-CUM-dated" "0 Main OUR PHOTOS-VIDEOS" "1 Move to MEDIA CENTER")  :last-leveln 2)
               (my-dos-drives-info  subdir-paths :clean-host-path-p clean-host-path-p
                                    :last-leveln  last-leveln :leveln leveln ;;incf each level
                                    :set-dirsym-to-symlist-p NIL ;;? set-dirsym-to-symlist-p
                                    :not-return-all-info-symvals-p NIL ;;not-return-all-info-symvals-p
                                    :set-all-drive-info-to-sym set-all-drive-info-to-sym
                                    :wildcard-p wildcard-p :attributes  attributes 
                                    :format format :sort-order sort-order 
                                    :directories-p directories-p :lowercase-p lowercase-p 
                                    :read-only-p read-only-p :hidden-p  hidden-p 
                                    :system-files-p system-files-p 
                                    :not-content-indexed-p not-content-indexed-p 
                                    :archive-ready-p archive-ready-p :bare-p  bare-p 
                                    :owner-p owner-p :4-digit-years-p  4-digit-years-p 
                                    :reparse-points-p reparse-points-p :show-cmd-p show-cmd-p
                                    :return-dos-output-t return-dos-output-t
                                    :return-all-lines-p return-all-lines-p 
                                    :return-line-lists-p return-line-lists-p
                                    :max-lines max-lines :dos-out-prefix dos-out-prefix)
             
             ;;APPEND SUBDIRS?
             (when dirpath-all-level-info1
               (setf path-info-keylist (append path-info-keylist
                                               (list :SUBDIRS) (list dirpath-all-level-info1))))

             #|             (when path-info-keylist
               (setf dirpath-all-level-info (append dirpath-all-level-info
                                                    (list path-info-keylist))))|#
             ;;(break "after recurse dirpath-all-level-info1")
             ;;(afout 'out (format nil "AFTER RECURSE: path-info-keylist= ~S ~% dirpath-all-level-info= ~S  ~%ALSO: RECURSE OUT: dirpath-all-level-info1= ~S  " path-info-keylist dirpath-all-level-info dirpath-all-level-info1 ))
             ;;(break "end of mvb,when")
             ;;end mvb, when
             ))
         ;;ACCUMULATE AFTER EACH TOP PATH= dir-path PROCESSED
         (when path-info-keylist
           (setf ALL-TOP-PATH-KEYLISTS (append all-top-path-keylists 
                                               (list path-info-keylist))))
         ;;SET DRIVE-SYM = (level 1 DIRSYM) TO ALL ITS DATA?
         (when (and  set-dirsym-to-symlist-p (= leveln 1)) 
           (set dirsym path-info-keylist))

         ;;end mvb, let,loop
         )))
    ;;SET dirpath-all-level-info TO GLOBAL VAR?
    (when (and set-all-drive-info-to-sym dirpath-all-level-info)
      (set set-all-drive-info-to-sym dirpath-all-level-info))
   
    ;;WHEN not-return-all-path-info-keylists-p
    (when not-return-all-info-symvals-p
      (setf all-top-path-keylists nil))  
    (values level1-dirsyms level1-dir-paths found-drive-letters all-top-path-keylists)
    ;;end let, my-dos-drives-info
    ))
;;TEST
;;   (my-dos-drives-info  '("F:/") :last-leveln 2 :location "TomPC" :type "FD" :brand "Sams" :dsize "128GB")
;;   (my-dos-drives-info  '("F:/") :last-leveln 2)
;; (TOM-SE5TBHD-MAIN)   ("F:/")  ("F")
;; ((TOM-SE5TBHD-MAIN "F:\\" :HOST "  \"F:/\"" :PATH "\\" :VOLNAME "Tom-Se5tbHD-Main" :LEVELN 1 :SERIAL-NUM "B8F3-358D" :N-DIRS "9" :N-FILES "1" :USED "0" :FREE "3117112082432" :DIRLISTS ((:DIR "0 LW-CUM-dated" "03/29/2020; 02:59 PM") (:DIR "0 Main OUR PHOTOS-VIDEOS" "12/31/2019; 01:55 AM") (:DIR "1 Move to MEDIA CENTER" "03/26/2020; 07:58 AM") (:DIR "1 PHONE BUs" "12/31/2019; 11:25 AM") (:DIR "2 MAIN BU" "01/01/2020; 02:18 PM") (:DIR "2 To MEDIA CENTER" "12/30/2019; 01:03 PM") (:DIR "3 Our MUSIC-m4a [master]" "12/30/2019; 01:47 PM") (:DIR "My Software-Manuals" "12/30/2019; 01:13 PM") (:DIR "Video-watch" "12/31/2019; 11:38 AM")) :FILELISTS ((:FILE "TEST-FILE.txt" "04/13/2020; 10:41 AM" "0")) :DOS-OUT-STRING NIL :DOS-OUT 0 :ALL-LINE-LISTS NIL                                                                            :SUBDIRS ((TOM-SE5TBHD-MAIN "F:\\0 LW-CUM-dated" :HOST "  \"F:/0 LW-CUM-dated/\"" :PATH "\\0 LW-CUM-dated\\" :VOLNAME "Tom-Se5tbHD-Main" :LEVELN 2 :SERIAL-NUM "B8F3-358D" :N-DIRS "28" :N-FILES "1" :USED "144070" :FREE "3117112082432" :DIRLISTS ((:DIR "1-LW-INIT" "04/18/2020; 07:45 PM") (:DIR "ACT-R TS" "12/30/2019; 11:08 AM") (:DIR "ACTR TS OUTPUTS" "11/14/2018; 05:23 PM") (:DIR "AndyCares" "12/30/2019; 11:09 AM") (:DIR "ART-LW" "12/30/2019; 11:09 AM") (:DIR "ART-Utilities" "12/30/2019; 11:09 AM") (:DIR "ART3" "12/30/2019; 11:10 AM") (:DIR "CogSys-Model" "04/18/2020; 07:45 PM") (:DIR "CogSysOutputs" "04/18/2020; 07:45 PM") (:DIR "commands" "04/18/2020; 07:45 PM") (:DIR "H-Capi" "04/18/2020; 07:45 PM") (:DIR "H-HELP" "04/18/2020; 07:45 PM") (:DIR "LISP PROJECTS TS" "12/30/2019; 11:14 AM") (:DIR "Math" "12/30/2019; 11:14 AM") (:DIR "MyInterfaces" "12/30/2019; 11:14 AM") (:DIR "MyLispWebs" "12/30/2019; 11:14 AM") (:DIR "MyLWex-Utilities" "12/30/2019; 11:15 AM") (:DIR "MyUtilities" "04/18/2020; 07:45 PM") (:DIR "screensaver" "12/30/2019; 11:17 AM") (:DIR "screensaver2013" "12/30/2019; 11:17 AM") (:DIR "SHAQ" "12/30/2019; 11:17 AM") (:DIR "SHAQ - Copy 2017-10-02" "12/30/2019; 11:17 AM") (:DIR "Toms-HDrive-Explorer" "04/18/2020; 07:45 PM") (:DIR "TomsFolderBackups" "12/30/2019; 11:17 AM") (:DIR "video-playback-utility" "12/30/2019; 11:17 AM") (:DIR "VideoPlayback" "12/30/2019; 11:18 AM")) :FILELISTS ((:FILE ".LISPWORKS" "03/03/2019; 08:14 PM" "144070")) :DOS-OUT-STRING NIL :DOS-OUT 0 :ALL-LINE-LISTS NIL)                              (TOM-SE5TBHD-MAIN "F:\\0 Main OUR PHOTOS-VIDEOS" :HOST "  \"F:/0 Main OUR PHOTOS-VIDEOS/\"" :PATH "\\0 Main OUR PHOTOS-VIDEOS\\" :VOLNAME "Tom-Se5tbHD-Main" :LEVELN 2 :SERIAL-NUM "B8F3-358D" :N-DIRS "18" :N-FILES "0" :USED "0" :FREE "3117112082432" :DIRLISTS ((:DIR "0-Our HEALTH IMAGES" "12/30/2019; 03:33 PM") (:DIR "1 Compressed PHOTOS" "12/30/2019; 03:36 PM") (:DIR "1 TO BE SORTED" "12/30/2019; 03:36 PM") (:DIR "ALL PHOTOS-VIDEOS BY DATE" "12/31/2019; 01:53 AM") (:DIR "Desert Photos" "12/31/2019; 01:53 AM") (:DIR "Friends - Family Unsorted" "12/31/2019; 01:53 AM") (:DIR "Home PalmDesert" "12/31/2019; 01:54 AM") (:DIR "Maui" "12/31/2019; 01:54 AM") (:DIR "New folder" "12/20/2016; 06:17 PM") (:DIR "Sherry & Tom" "12/31/2019; 01:54 AM") (:DIR "Sherry family" "12/31/2019; 01:54 AM") (:DIR "Snyders" "12/31/2019; 01:54 AM") (:DIR "Steffens" "12/31/2019; 01:54 AM") (:DIR "Tim & Trina" "12/31/2019; 01:55 AM") (:DIR "Tom Family" "12/31/2019; 01:55 AM") (:DIR "Xmas Photos" "12/31/2019; 01:55 AM")) :FILELISTS NIL :DOS-OUT-STRING NIL :DOS-OUT 0 :ALL-LINE-LISTS NIL)                            ETC ETC ETC   ) :DOS-OUT-STRING NIL :DOS-OUT 0 :ALL-LINE-LISTS NIL))))


;; (pprint (nth-value 3  (my-dos-drives-info  '("G:/") :last-leveln 2)))
;;works= 
;;(PD-MEDIA)
;; (MY-DOS-DRIVES-INFO  '("E:\\") :LAST-LEVELN 2)
;; works= (C-MAIN-BU)  ("E:\\")  ("E")
;;((C-MAIN-BU (C-MAIN-BU "E:\\" :HOST "  \"E:/\"" :PATH "\\" :VOLNAME "C-MAIN-bu" :SERIAL-NUM "F4A2-4D95" :N-DIRS "43" :N-FILES "7" :USED "159428" :FREE "63673704448" :DIRLISTS ((:DIR "0 LW-CUM-dated" "03/29/2020; 02:59 PM") (:DIR "AI-related" "06/26/2019; 10:36 PM") (:DIR "ANDY FOLDERS" "06/26/2019; 10:37 PM") (:DIR "BKHAP 2010" "06/26/2019; 10:44 PM") (:DIR "BKHAP FILES" "06/26/2019; 10:49 PM") (:DIR "csulb" "06/26/2019; 10:49 PM") (:DIR "JAVA Literature-Help" "06/26/2019; 10:49 PM") (:DIR "JAVA SUCCESS-DO NOT USE" "06/26/2019; 11:00 PM") (:DIR "JAVA SUCCESS-NB" "06/26/2019; 11:14 PM") (:DIR "JAVA SUCCESS-NB Xtra" "06/26/2019; 11:17 PM") (:DIR "JAVA SUN PROJECTS" "06/26/2019; 11:25 PM") (:DIR "LISP EXAMPLES-CODE" "06/26/2019; 11:41 PM") (:DIR "LISP PROJECTS TS" "06/26/2019; 11:55 PM") (:DIR "LISP PROJECTS TS-RUN" "06/26/2019; 11:56 PM") (:DIR "LISP PROJECTS2" "06/27/2019; 12:00 AM") (:DIR "LISP- incl downloads" "06/26/2019; 11:42 PM") (:DIR "LISP-CLOJURE" "06/27/2019; 12:00 AM") (:DIR "LISP-Literature Info and Help" "06/27/2019; 12:30 AM") (:DIR "LISPWORKS-misc" "06/27/2019; 12:30 AM") (:DIR "LITERATURE AI NN CogSci" "06/27/2019; 12:49 AM") (:DIR "Literature-COMP WIN INTERNET" "06/27/2019; 12:53 AM") (:DIR "Literature-DATABASES" "06/27/2019; 12:54 AM") (:DIR "Literature-Gov-Organizations-Legal" "06/27/2019; 12:54 AM") (:DIR "Literature-HEALTH" "06/27/2019; 01:15 AM") (:DIR "Literature-KINDLE" "06/27/2019; 01:18 AM") (:DIR "Literature-PSYCH etc" "06/27/2019; 01:28 AM") (:DIR "Literature-Subscriptions etc" "06/26/2019; 06:42 PM") (:DIR "Literature-Temp" "06/26/2019; 06:43 PM") (:DIR "MyArch" "06/26/2019; 07:01 PM") (:DIR "MyDat" "06/26/2019; 07:12 PM") (:DIR "MyDrives" "06/26/2019; 07:12 PM") (:DIR "MyTemplates" "06/26/2019; 07:12 PM") (:DIR "MyWebsArchive" "06/26/2019; 08:34 PM") (:DIR "MyWEBSexp" "06/26/2019; 08:41 PM") (:DIR "MyWebsINFO-DOCS" "06/26/2019; 08:45 PM") (:DIR "NIL" "06/26/2019; 06:37 PM") (:DIR "Our PRODUCTS" "06/26/2019; 09:17 PM") (:DIR "OurPHONES BU" "06/26/2019; 09:21 PM") (:DIR "rtfs" "06/26/2019; 09:33 PM") (:DIR "TEACH-My Papers+Questionaires" "06/26/2019; 09:35 PM") (:DIR "Temp" "06/26/2019; 09:35 PM") (:DIR "Travel" "06/26/2019; 09:57 PM") (:DIR "TS-PER" "06/26/2019; 10:30 PM")) :FILELISTS ((:FILE "0 MY BLANK DOCX 10pt.docx" "03/27/2019; 10:44 AM" "12163") (:FILE "0 MY BLANK DOCX 11pt.docx" "03/27/2019; 10:43 AM" "12156") (:FILE "2918-09 Oklahoma Sooner Sports bill problem.html" "09/21/2018; 02:21 PM" "40679") (:FILE "Backup of 0 MY BLANK DOCX 10pt.wbk" "01/31/2019; 07:12 PM" "12669") (:FILE "Backup of 0 MY BLANK DOCX 11pt.wbk" "03/23/2019; 11:29 AM" "12104") (:FILE "Backup of MY-BLANK-DOC.wbk" "10/21/2018; 06:34 PM" "35164") (:FILE "MY-BLANK-DOC.docx" "10/30/2018; 04:51 PM" "34493")) :DOS-OUT-STRING NIL :DOS-OUT 0 :ALL-LINE-LISTS NIL)) ((C-MAIN-BU "E:\\0 LW-CUM-dated" :HOST "  \"E:/0 LW-CUM-dated/\"" :PATH "\\0 LW-CUM-dated\\" :VOLNAME "C-MAIN-bu" :SERIAL-NUM "F4A2-4D95" :N-DIRS "28" :N-FILES "1" :USED "144070" :FREE "63673704448" :DIRLISTS ((:DIR "1-LW-INIT" "04/16/2020; 05:40 PM") (:DIR "ACT-R TS" "06/26/2019; 06:30 PM") (:DIR "ACTR TS OUTPUTS" "11/14/2018; 05:23 PM") (:DIR "AndyCares" "06/26/2019; 06:31 PM") (:DIR "ART-LW" "06/26/2019; 06:31 PM") (:DIR "ART-Utilities" "12/05/2019; 06:04 PM") (:DIR "ART3" "06/26/2019; 06:31 PM") (:DIR "CogSys-Model" "04/16/2020; 05:40 PM") (:DIR "CogSysOutputs" "04/16/2020; 05:40 PM") (:DIR "commands" "04/16/2020; 05:40 PM") (:DIR "H-Capi" "04/16/2020; 05:40 PM") (:DIR "H-HELP" "04/16/2020; 05:40 PM") (:DIR "LISP PROJECTS TS" "12/05/2019; 06:04 PM") (:DIR "Math" "06/26/2019; 06:31 PM") (:DIR "MyInterfaces" "06/26/2019; 06:31 PM") (:DIR "MyLispWebs" "06/26/2019; 06:31 PM") (:DIR "MyLWex-Utilities" "06/26/2019; 06:31 PM") (:DIR "MyUtilities" "04/16/2020; 05:40 PM") (:DIR "screensaver" "06/26/2019; 06:32 PM") (:DIR "screensaver2013" "06/26/2019; 06:32 PM") (:DIR "SHAQ" "12/05/2019; 06:04 PM") (:DIR "SHAQ - Copy 2017-10-02" "06/26/2019; 06:32 PM") (:DIR "Toms-HDrive-Explorer" "04/16/2020; 05:40 PM") (:DIR "TomsFolderBackups" "06/26/2019; 06:32 PM") (:DIR "video-playback-utility" "06/26/2019; 06:32 PM") (:DIR "VideoPlayback" "06/26/2019; 06:33 PM")) :FILELISTS ((:FILE ".LISPWORKS" "03/03/2019; 08:14 PM" "144070")) :DOS-OUT-STRING NIL :DOS-OUT 0 :ALL-LINE-LISTS NIL) NIL (C-MAIN-BU "E:\\AI-related" :HOST "  \"E:/AI-related/\"" :PATH "\\AI-related\\" :VOLNAME "C-MAIN-bu" :SERIAL-NUM "F4A2-4D95" :N-DIRS "15" :N-FILES "1" :USED "906" :FREE "63673704448" :DIRLISTS ((:DIR "ACL 8 Projects" "06/26/2019; 10:34 PM") (:DIR "AI Code-SpecificSoftware" "06/26/2019; 10:35 PM") (:DIR "AI related - OLD" "06/26/2019; 10:35 PM") (:DIR "allegro" "06/26/2019; 10:35 PM") (:DIR "CMU" "06/26/2019; 10:35 PM") (:DIR "ICS171" "06/26/2019; 10:35 PM") (:DIR "Lisp Info" "06/26/2019; 10:35 PM") (:DIR "LITERATURE" "06/26/2019; 10:35 PM") (:DIR "MIT" "06/26/2019; 10:35 PM") (:DIR "PERSIM" "06/26/2019; 10:36 PM") (:DIR "PROD" "06/26/2019; 10:36 PM") (:DIR "RULE" "06/26/2019; 10:36 PM") (:DIR "UCI" "06/26/2019; 10:36 PM")) :FILELISTS ((:FILE "LITERATURE AI NN CogSci - Shortcut.lnk" "03/25/2015; 03:00 PM" "906")) :DOS-OUT-STRING NIL :DOS-OUT 0 :ALL-LINE-LISTS NIL) NIL   ETC, ETC.





;;FILTER-FILE-EXT
;;2019
;;ddd
(defun filter-file-ext (path &key incl-exts omit-exts)
  "U-files. Works on strings or real-paths. the exts must be strings w/o period."
  (let*
      ((ext (pathname-type path))
       (result)
       )
    (cond
        ;;exclude omit-exts
        ((and omit-exts (member ext omit-exts :test 'string-equal)) nil)
        ;;exclude if exts and not a member of exts
       ((and incl-exts (null (member ext incl-exts :test 'string-equal)))
        NIL)
       ;;otherwise include the file
       (t (setf result path)))
    result
    ;;end let, filter-file-ext
    ))
;;TEST
;; (filter-file-ext #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-explore-Questions.lisp~" :incl-exts '("lisp")) 
;; works= NIL
;; (filter-file-ext #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-explore-Questions.lisp~")  = #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-explore-Questions.lisp~"
;; (filter-file-ext #P"C:/3-TS/LISP PROJECTS TS/CogSys-Model/CS-explore-Questions.lisp~" :omit-exts '("lisp~")) = NIL




;;SEPARATE-FILES-FROM-DIRS
;;
;;ddd
(defun separate-files-from-dirs (dir-or-dir-list
                                 &key (return-strings-p t) omit-exts incl-exts
                                                 (return-file-info-p t)  
                                                 (return-subdir-info-p t))
  "U-files.lisp, removes and returns (values file-realpaths subdir-realpaths filename-list subdir-name-list filepath-names subdirpath-names) from a dir.  if return-strings-p returns dir and file namestrings too.  if return-drive-p, returns the drive letter with the dir. use list-directory-contents to list all subdir and file paths and strings separately.
  also see less derailed separate-subdirs-files function.  
    OMIT-EXTS INCL-EXTS added to filter file types/exts."
  ;;not needed or use in cond below?  (setf dir (pathname-as-directory dir-or-dir-list))
  ;;  (show-text (format nil "in separate-files-from-dirs, dir-or-dir-list ~a~%" dir-or-dir-list) 20 t)
  (let
      ((dir-list) 
       (file-realpaths)
       (filename-list)
       (subdir-realpaths)
       (subdir-name-list)
       (filepath-names)
       (subdirpath-names)
       )
    ;;create a dir-list of file-realpaths and subdir-realpaths
    (cond
     ((listp dir-or-dir-list) 
      (setf dir-list dir-or-dir-list))
     (t (setf  dir-or-dir-list (list-directory dir-or-dir-list))))
    ;;  (show-text (format nil "2-in separate-files-from-dirs, dir-list ~a~%" dir-list) 200 t)
    (dolist (element dir-or-dir-list)
      ;; (show-text (format nil "element= ~a~%" element) 20 t)
      (cond
       ((directory-pathname-p element)
        (when return-subdir-info-p
          (setf subdir-realpaths (append subdir-realpaths (list element)))
          (when return-strings-p
            (setf subdir-name-list (append subdir-name-list 
                                           (list (directory-namestring  element)))
                  subdirpath-names (append subdirpath-names 
                                           (list (namestring element)))))))     
       ((file-pathname-p element)
        ;;added 2019 TO FILTER by FILE EXTS/TYPES
        (setf element (filter-file-ext element :incl-exts incl-exts :omit-exts omit-exts))
        (when element
        ;;end added
        (when  return-file-info-p
          (setf file-realpaths (append file-realpaths (list element)))
          (when return-strings-p
            (setf filename-list (append filename-list  
                                        (list (file-namestring element)))
                  filepath-names (append filepath-names
                                         (list (namestring element))))))))
       ;;  (show-text (format nil "file-name= ~a~%" element) 20 t)  
       (t nil))
      ;;end dolist
      )
    (values file-realpaths subdir-realpaths filename-list subdir-name-list 
            filepath-names subdirpath-names) 
    ;;end let, separate-files-from-dirs
    ))
;;TEST
;; (SEPARATE-FILES-FROM-DIRS "C:\\TEMP\\")
;; WORKS=
;;FILE-REALPATHS= (#P"C:/TEMP/SWITCHSETUP.EXE" #P"C:/TEMP/SETUP_XRECODE3_WIN_64BIT_1.56.EXE" #P"C:/TEMP/NETBEANS-8.2-WINDOWS.EXE" #P"C:/TEMP/JDK-8U121-WINDOWS-X64.EXE" #P"C:/TEMP/EDITORIAL STAFF DIRECTORY - LA TIMES.HTM" #P"C:/TEMP/CORAL TEST.CDR" #P"C:/TEMP/CHROME SETUP.EXE" #P"C:/TEMP/BITDEFENDER_WINDOWS_BC3DB0E0-1194-4117-97EC-46995EDD53A7.EXE")
;;SUBDIR-REALPATHS= (#P"C:/TEMP/EDITORIAL STAFF DIRECTORY - LA TIMES_FILES/")
;;FILENAME-LIST= ("SWITCHSETUP.EXE" "SETUP_XRECODE3_WIN_64BIT_1.56.EXE" "NETBEANS-8.2-WINDOWS.EXE" "JDK-8U121-WINDOWS-X64.EXE" "EDITORIAL STAFF DIRECTORY - LA TIMES.HTM" "CORAL TEST.CDR" "CHROME SETUP.EXE" "BITDEFENDER_WINDOWS_BC3DB0E0-1194-4117-97EC-46995EDD53A7.EXE")
;;SUBDIR-NAME-LIST= ("\\TEMP\\EDITORIAL STAFF DIRECTORY - LA TIMES_FILES\\")
;;FILEPATH-NAMES= ("C:\\TEMP\\SWITCHSETUP.EXE" "C:\\TEMP\\SETUP_XRECODE3_WIN_64BIT_1.56.EXE" "C:\\TEMP\\NETBEANS-8.2-WINDOWS.EXE" "C:\\TEMP\\JDK-8U121-WINDOWS-X64.EXE" "C:\\TEMP\\EDITORIAL STAFF DIRECTORY - LA TIMES.HTM" "C:\\TEMP\\CORAL TEST.CDR" "C:\\TEMP\\CHROME SETUP.EXE" "C:\\TEMP\\BITDEFENDER_WINDOWS_BC3DB0E0-1194-4117-97EC-46995EDD53A7.EXE")
;;SUBDIRPATH-NAMES= ("C:\\TEMP\\EDITORIAL STAFF DIRECTORY - LA TIMES_FILES\\")


;;  EG (MY-DOS-COMMAND "DIR \"C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP\" " T)  OTHER LIST-DIRECTORY ETC 


;;SEPARATE-FILE-INFO-IN-DIR-OUTPUT -- 
;; NOW REDUNDANT? DELETE?
;;2019
;;ddd
#|(defun separate-file-info-in-dir-output (dir-output-string &key
                                                          ;; (return-strings-p T) (return-file-info-p T) 
                                                          ;;(return-subdir-info-p T)
                                                 (max-pre-lines 2) (max-lines 200))
  "In U-files.lisp,  FINISH--CAN BE USED WITH MY-DOS-COMMAND TO MAKE DIR LISTS WHEN HAVE  *.LISP IN DIR ARG"
  ;;not needed or use in cond below?  (setf dir (pathname-as-directory dir-or-dir-list))
  ;;  (show-text (format nil "In separate-files-from-dirs, dir-or-dir-list ~A~%" dir-or-dir-list) 20 t)
  (let
      ((first-lines)
       (last-lines)
       (filenames)
       (file-lists)
       (instr (make-string-input-stream dir-output-string))
       (last-lines-p)
#|       (dir-list) 
       (file-realpaths)
       (filename-list)
       (subdir-realpaths)
       (subdir-name-list)
       (filepath-names)
       (subdirpath-names)|#
       )
    (loop
     for n from 0 to max-lines
     do
     (cond
      ((and (>= n 0)(<  n max-pre-lines))
       (setf first-lines (append first-lines (list (read-line instr nil 'eof)))))
      (t     
     (let*
         ((line (read-line instr nil 'eof))
#|          (line-strs  )
          (date )
          (time )
          (size )
          (filename )  |#   
          )
       (unless (equal line 'eof)
         (setf line-strs  (divide-string-to-all-tokens line))
         ;;(values object-list first-object rest-string string-list
         (multiple-value-bind (date time ampm size filename )  
             (values-list line-strs)   
           (cond
            ((or last-lines-p (= (list-length line-strs) 4))
             (setf last-lines-p T
                   last-lines (append last-lines (list line-strs))))
            (t
             (setf filenames (append filenames (list filename))
                   file-lists (append file-lists (list line-strs)))))
           ))
     ;;end let,t,cond,loop
     ))))
    (values filenames file-lists first-lines last-lines)
    ;;end let, separate-file-info-in-dir-output
    ))|#
;;TEST
;; (separate-file-info-in-dir-output *test-dir)
 ;; (my-dos-command "dir \"C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\*.lisp\" " T) 
;; works "dir \"C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\*.lisp\"  
#| (setf *test-dir " Volume in drive C is OS
 Volume Serial Number is 4058-766B
 Directory of C:\\3-TS\\LISP PROJECTS TS\\MyUtilities
08/03/2018  08:31 PM               339 0-CODE-TEMPLATES.lisp
05/08/2018  06:48 PM            15,903 ARC-curve-function-tests.lisp
08/11/2010  02:36 PM             2,837 space-show-arglist.lisp
11/26/2013  03:48 PM            23,466 U-Arrays.lisp
02/27/2019  07:05 PM            10,138 U-BASIC-functions.lisp
04/22/2016  03:06 PM             4,527 U-capi - Copy.lisp
04/25/2014  08:14 PM            48,236 U-capi-buttons-etc-old.lisp
10/12/2016  03:04 PM           112,977 U-capi-buttons-etc.lisp
03/27/2019  09:46 AM            22,096 U-system.lisp
08/29/2019  05:48 PM            87,058 U-trees-art-dims.lisp
08/22/2019  07:54 PM            88,173 U-trees-art-dims1.lisp
08/06/2017  05:22 PM            15,563 U-trees-flatlists.lisp
07/23/2019  05:54 PM            81,732 U-trees-old.lisp
08/09/2019  08:48 PM            93,451 U-trees.lisp
04/16/2015  05:18 PM           121,474 U-tstring-OLD-make-index-funs.lisp
08/10/2019  03:35 PM           255,774 U-tstring.lisp
              62 File(s)      4,443,093 bytes
               0 Dir(s)  381,221,978,112 bytes free
") |#


;; WORKS
;; filenames= ("TSMyUtilities" "0-CODE-TEMPLATESlisp" "ARC-curve-function-testslisp" "space-show-arglistlisp" "U-Arrayslisp" "U-BASIC-functionslisp" "U-capi" "U-capi-buttons-etc-oldlisp" "U-capi-buttons-etclisp" "U-systemlisp" "U-trees-art-dimslisp" "U-trees-art-dims1lisp" "U-trees-flatlistslisp" "U-trees-oldlisp" "U-treeslisp" "U-tstring-OLD-make-index-funslisp" "U-tstringlisp")
;;string lists= (("irectory" "of" "C:3-TSLISP" "PROJECTS" "TSMyUtilities") ("08/03/2018" "08:31" "PM" "339" "0-CODE-TEMPLATESlisp") ("05/08/2018" "06:48" "PM" "15903" "ARC-curve-function-testslisp") ("08/11/2010" "02:36" "PM" "2837" "space-show-arglistlisp") ("11/26/2013" "03:48" "PM" "23466" "U-Arrayslisp") ("02/27/2019" "07:05" "PM" "10138" "U-BASIC-functionslisp") ("04/22/2016" "03:06" "PM" "4527" "U-capi" "-" "Copylisp") ("04/25/2014" "08:14" "PM" "48236" "U-capi-buttons-etc-oldlisp") ("10/12/2016" "03:04" "PM" "112977" "U-capi-buttons-etclisp") ("03/27/2019" "09:46" "AM" "22096" "U-systemlisp") ("08/29/2019" "05:48" "PM" "87058" "U-trees-art-dimslisp") ("08/22/2019" "07:54" "PM" "88173" "U-trees-art-dims1lisp") ("08/06/2017" "05:22" "PM" "15563" "U-trees-flatlistslisp") ("07/23/2019" "05:54" "PM" "81732" "U-trees-oldlisp") ("08/09/2019" "08:48" "PM" "93451" "U-treeslisp") ("04/16/2015" "05:18" "PM" "121474" "U-tstring-OLD-make-index-funslisp") ("08/10/2019" "03:35" "PM" "255774" "U-tstringlisp"))
;;first-lines= (" Volume in drive C is OS" " Volume Serial Number is 4058-766B")
;;last-lines= (("2" "Files)" "4443093" "bytes") ("irs)" "381221978112" "bytes" "free"))



;;LIST-DIRECTORY
;;from PSeibel Practical Common Lisp 
;;(list-directory "C:\\") ;;works--lists pathnames not strings for FILES ONLY
;;(list-directory "C:\\3-TS\\LISP PROJECTS\\MyProjects\\MyUtilities\\U-lists.lisp")==>
;; returns nil if it is a file and not a directory
;; (list-directory "C:\\TOM\LISP PROJECTS\\MyProjects") => NIL  DOESN[T WORK
;;ddd
(defun list-directory (dirname)
  "In U-files.lisp, USE MORE POWERFUL MY-DOS-DIR?  Seibel advises to use this instead of DIRECTORY function"
  (when  (or (stringp dirname)(pathnamep dirname)(streamp dirname))
    (when (wild-pathname-p dirname)
      (error "Can only list concrete directory names."))
    (directory (directory-wildcard dirname)))
  )
;;TEST
;; (list-directory "C:/System Recovery/") = NIL (because special system file??)
;; (list-directory "c:\\temp\\") = works
;; (list-directory "c:\\temp") = works
;; (list-directory "c:\\tem*") Error: Can only list concrete directory names.
;; (list-directory nil) = nil
;; (setf  *dir-listx  (list-directory "c:\\"))
;; works= (#P"c:/{EFC301B3-00A7-423C-9480-6DB44167507B}" #P"c:/{ECDA7218-E3FF-4E71-BBD0-93281D8387D5}" #P"c:/{C95264FD-4F34-425F-8A0F-341958847E3E}" #P"c:/{870662A9-8D22-4C2E-ADEF-D510E242528F}" #P"c:/{726BA7D1-CA33-40C0-B7A5-95809BC48BC8}" #P"c:/{572F6D24-8FB6-4146-82B9-19CE53895E28}" #P"c:/{204F89D3-51EF-435F-9CFB-E2F05427680B}" #P"c:/{1CE490CF-C766-42FC-8A51-728809ED1EB1}" #P"c:/write-to-file-test.txt" #P"c:/WirelessDiagLog.csv" #P"c:/Windows/" #P"c:/Users/" #P"c:/TOM/" #P"c:/Temp2/" #P"c:/Temp/" #P"c:/System Volume Information/" #P"c:/System Recovery/" #P"c:/special folders/" #P"c:/SHERRY/"  etc ... )
;; (list-directory "a:\\") = NIL



;;NAMESTRINGS-LIST
;;2019
;;ddd
(defun namestrings-list (pathnames  &key (test-pathnamep T) )
  "U-files   RETURNS (values namestrings failed-pathnames)    INPUT: list of file or dir path objects. If namestring, eturns as is. "
  (let
      ((namestrings)
       (failed-pathnames)
       )
    (loop
     for pathname in pathnames
     do
     (let*
         ((namestr) 
          )
       (cond
        ((and pathname (or (null test-pathnamep)
                           (and (or (pathnamep pathname)(stringp pathname)(streamp pathname)))))
         (setf namestr (namestring pathname)
               namestrings (append namestrings (list namestr))))
        (t (setf failed-pathnames (append failed-pathnames (list pathname)))))
     ;;end let,loop
     ))
    (values namestrings failed-pathnames)
    ;;end let, namestrings-list
    ))
;;TEST
;; (namestrings-list '(#P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-system.ofasl~" #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-trees-flatlists.lisp" #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-trees-flatlists.lisp~" #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-trees.lisp" #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-trees.lisp~" #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-tstring-OLD-make-index-funs.lisp" 'THIS NIL "WHAT" "\\THIS\\THAT"))
;; works= ("C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-system.ofasl~" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-trees-flatlists.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-trees-flatlists.lisp~" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-trees.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-trees.lisp~" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-tstring-OLD-make-index-funs.lisp" "WHAT" "\\THIS\\THAT")  ((QUOTE THIS) NIL)
;; (namestrings-list '(   #P"C:/3-TS/LISP PROJECTS TS/h-help/X-Y TRANSFORMATION.pdf" #P"C:/3-TS/LISP PROJECTS TS/h-help/my-multi-threading.lisp~"))
;;works= ("C:\\3-TS\\LISP PROJECTS TS\\h-help\\X-Y TRANSFORMATION.pdf" "C:\\3-TS\\LISP PROJECTS TS\\h-help\\my-multi-threading.lisp~")



;;LIST-DIRECTORY-NAMESTRINGS
;;
;;ddd
(defun list-directory-namestrings (dirname &key paths) 
  "In U-files.lisp, RETURNS (values  path-namestrings paths) =  list of  path-namestrings and real paths using Seibel advises to use this instead of DIRECTORY function; NOTE: USE LIST-DIRECTORY-CONTENTS to return all file and subdir paths and strings. IF PATHS, saves step of finding paths in dirname. "
  (let
      ((path-namestrings)
       (namestr)
       (list-directory dirname)
       )
  (unless paths
    (setf paths (list-directory dirname)))

    (loop
     for path in paths
     do
     (setf namestr (namestring path)
           path-namestrings (append path-namestrings (list namestr)))
     )
    (values path-namestrings paths)
    ))
;;TEST
;;  (list-directory-namestrings "C:\\3-TS\\")
;; WORKS=
;;  ("C:\\3-TS\\TS-PER\\" "C:\\3-TS\\Travel\\" "C:\\3-TS\\Template-Narrow 9pt-10ptlineSpace.dotx" "C:\\3-TS\\Temp MS BU\\" "C:\\3-TS\\Temp\\" "C:\\3-TS\\TEACH-My Papers+Questionaires\\" "C:\\3-TS\\statement-2.pdf" "C:\\3-TS\\SherryBoatPainting.jpg" "C:\\3-TS\\Shared Albums Archive\\" "C:\\3-TS\\rtfs\\" "C:\\3-TS\\Our PRODUCTS\\" "C:\\3-TS\\MyWebsINFO-DOCS\\" "C:\\3-TS\\MyWEBSexp\\"  ETC )  
;; (#P"C:/3-TS/TS-PER/" #P"C:/3-TS/Travel/" #P"C:/3-TS/Template-Narrow 9pt-10ptlineSpace.dotx" #P"C:/3-TS/Temp MS BU/" #P"C:/3-TS/Temp/" #P"C:/3-TS/TEACH-My Papers+Questionaires/" #P"C:/3-TS/statement-2.pdf" #P"C:/3-TS/SherryBoatPainting.jpg" #P"C:/3-TS/Shared Albums Archive/" #P"C:/3-TS/rtfs/" #P"C:/3-TS/Our PRODUCTS/" #P"C:/3-TS/MyWebsINFO-DOCS/" #P"C:/3-TS/MyWEBSexp/" #P"C:/3-TS/MyWebsArchive/" #P"C:/3-TS/MyTemplates/"  ETC   )    



;;SEPARATE-SUBDIRS-FILES
;;2019
;;ddd
(defun separate-subdirs-files (dir  &key  )
  "U-files,   RETURNS (values subdirs-paths file-pathnames other)   INPUT: Dir path or list of dir namestrings. SUBDIRS and FILES are FULL PATHNAMES. "
  (let
      ((dir-list)
       (subdirs)
       (files)
       (other)
       )
    (cond
     ((listp dir) (setf dir-list dir))
     (t (setf dir-list (list-directory-namestrings dir))))
    (loop
     for item in dir-list
     do
     (let
         ((type (nth-value 1 (dir-or-file-test item)))
          )
       (cond
        ((equal :dir type)
         (setf subdirs (append subdirs (list item))))
        ((equal :file type)
         (setf files (append files (list item))))
        ((equal :other type)
         (setf other (append other (list item))))
        (t nil))
       ;;end let,loop
       ))
    (values  subdirs files other)
    ;;end let, separate-subdirs-files
    ))
;;TEST
;; (separate-subdirs-files "c:\\3-ts\\lisp projects ts\\")
;; works= ("c:\\3-ts\\lisp projects ts\\z-LISPWORKS files\\" "c:\\3-ts\\lisp projects ts\\X-OLD Misc\\" "c:\\3-ts\\lisp projects ts\\VideoPlayback\\" "c:\\3-ts\\lisp projects ts\\video-playback-utility\\" "c:\\3-ts\\lisp projects ts\\Toms-HDrive-Explorer\\"  ETC . )
;; ("c:\\3-ts\\lisp projects ts\\test-spss-output1.txt" "c:\\3-ts\\lisp projects ts\\test-spss-output0.txt" "c:\\3-ts\\lisp projects ts\\test-spss-output.txt~" "c:\\3-ts\\lisp projects ts\\test-shaq-text.lisp~" "c:\\3-ts\\lisp projects ts\\test-shaq-text.lisp" "c:\\3-ts\\lisp projects ts\\test-output-1.lisp~" "c:\\3-ts\\lisp projects ts\\test-output-1.lisp" "c:\\3-ts\\lisp projects ts\\test-object.lisp" "c:\\3-ts\\lisp projects ts\\test-functions2.lisp~"  ETC ...)




;;DIR-OR-FILE-TEST
;;2019
;;ddd
(defun dir-or-file-test (item &key (dot-in-last 7))
 "U-files Tests if dir, file, or other. RETURNS (values dir-p type[:dir :file :other]"
 (let*
     ((dir-p)
      (type)
      (len-item (length item))
      (item-end (subseq item (- len-item 1)))
      (item-last (cond ((>= len-item  dot-in-last) (subseq item (- len-item dot-in-last)))   (t  0)))    
      )
    (cond
     ((member item-end '("/" "\\") :test 'string-equal)
      (setf type :dir  dir-p T))     
     ((and item-last
           (search  "."  item-last))
      (setf type :file))
     ((or (search "\\" item)
          (search "/" item))
      (setf type :dir  dir-p T))
     (t (setf item :other)))
    (values dir-p type)
    ;;end let, dir-or-file-test
    ))
;;TEST
;; (dir-or-file-test "c:\\this\that\\file.doc") = NIL :FILE
;; (dir-or-file-test "file.doc") = NIL :FILE
;; (dir-or-file-test "c:\\this\that\\") = T :DIR
;; (dir-or-file-test "/that") = T :DIR
;;;; (dir-or-file-test  "c:\\3-ts\\lisp projects ts\\test-functions1.lisp~") = NIL :FILE



;;LIST-ALL-DRIVES-INFO - DEPRECIATED
;;
;;ddd
#|(defun list-all-drives-info (&key (last-leveln 2) 
                                  (drives '(a b      d e f g h i j k l m n o p q r s t u v w x y z)) 
                                     omit-dirs
                                   ;;fix later omit-dirs '("$RECYCLE.BIN" "System Volume Information"))   
                                   incl-subdir-namelists-p   (remove-final-slashes-p t)                           
                                  (drive-key :drive)
                                  (level-key :level)
                                  (return-namestrings-p T)
                                  not-return-strings-p)
  "In U-files. DEPRECIATED-USE ????? SSSSSS  RETURNS (values found-drives all-drive-info) where each drive-list is a list (drive dirs files) for top level of drice only. Explores all subdirs up to N-LEVELS for each. [Note: drive can be any dir path]
  IF NOT-RETURN-STRINGS-P, returns only the actual paths for the drive. When INCLUDE-ONLY-DIRS, then only includes subdirs of those titles DIRECTLY under root. When  INCL-SUBDIR-NAMELISTS-P, sets each list in subdir-info-lists to (path subdirnames), if nil = (path).  If  remove-final-slashes-p, removes final slashes for each subdirname. subdir-info-lists is included as part of every all-inclusive all-root-info list. If paths-as-strings-p, puts all paths in data as strings not real pathnames."
  (let
      ((dir-list)  
       (found-drives)
       (drive-info)
       (all-drive-info)       
       (newdrives)
       (drive-n 0)
       (dirnames-list)
       (filename-list)
       (all-pathnames)
       (leveln 0)
       )
    (cond
     ;fix later ;(omit-dirs  (setf newdrives (delete-items-from-list omit-dirs drives)))
     (t (setf newdrives drives)))

    (loop
     for drive-letter in newdrives
     for n from 1 to  (list-length newdrives) 
     do
     (multiple-value-bind (rootstr all-root-info all-pathnames dir-list)
         (list-drive-info drive-letter :last-leveln last-leveln :level-key level-key
                           :leveln leveln 
                          :omit-dirs omit-dirs
                          :incl-subdir-namelists-p incl-subdir-namelists-p
                         ;;fix later?  :omit-dirs omit-dirs           
                          :incl-subdir-namelists-p incl-subdir-namelists-p
                          :remove-final-slashes-p remove-final-slashes-p            
                          :not-return-strings-p not-return-strings-p)

     ;;(when (equal drive 'c)  (BREAK))
     (when all-root-info
       (incf drive-n)
       (setf found-drives (append found-drives (list rootstr))
             drive-info (list drive-n drive-key  rootstr all-root-info)
             all-drive-info (append all-drive-info (list drive-info)))
       ;;end when
       )
     (break)
     ;;end mvb, loop
     )) 
   
    (values found-drives all-drive-info)
    ;;end let, list-all-drives-info
    ))|#
;;TEST
;; (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info :omit-dirs '(c)))
;;  (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info  :last-leveln 2 )) ;;later? :omit-dirs '(c)))
;;  (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info :drives '(g)  :last-leveln 2 ))

;;  (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info :drives '(g) :include-only-dir '"\\0 Main OUR PHOTOS-VIDEOS\\"   :incl-subdir-namelists-p T :last-leveln 5 ))

;; BELOW IS SHORT FULL TEST
;;  (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info :drives '(g)  :last-leveln 1   :incl-subdir-namelists-p T))



;;LIST-ALL-DRIVES-INFO2
;; OLDER VERSION BASED UPON LIST-DRIVE-INFO
;; works ok, but newer verwion, based upon my-dos-dir provides more info.
;;
;;ddd
#|(defun list-all-drives-info2 (&key (last-leveln 2) 
                                  (drives '(a b      d e f g h i j k l m n o p q r s t u v w x y z)) 
                                     omit-dirs
                                   ;;fix later omit-dirs '("$RECYCLE.BIN" "System Volume Information"))   
                                   incl-subdir-namelists-p   (remove-final-slashes-p t)                           
                                  (drive-key :drive)
                                  (level-key :level)
                                  (return-namestrings-p T)
                                  not-return-strings-p)
  "In U-files. RETURNS (values found-drives all-drive-info) where each drive-list is a list (drive dirs files) for top level of drice only. Explores all subdirs up to N-LEVELS for each. [Note: drive can be any dir path]
  IF NOT-RETURN-STRINGS-P, returns only the actual paths for the drive. When INCLUDE-ONLY-DIRS, then only includes subdirs of those titles DIRECTLY under root. When  INCL-SUBDIR-NAMELISTS-P, sets each list in subdir-info-lists to (path subdirnames), if nil = (path).  If  remove-final-slashes-p, removes final slashes for each subdirname. subdir-info-lists is included as part of every all-inclusive all-root-info list. If paths-as-strings-p, puts all paths in data as strings not real pathnames."
  (let
      ((dir-list)  
       (found-drives)
       (drive-info)
       (all-drive-info)       
       (newdrives)
       (drive-n 0)
       (dirnames-list)
       (filename-list)
       (all-pathnames)
       (leveln 0)
       )
    (cond
     ;fix later ;(omit-dirs  (setf newdrives (delete-items-from-list omit-dirs drives)))
     (t (setf newdrives drives)))

    (loop
     for drive-letter in newdrives
     for n from 1 to  (list-length newdrives) 
     do
     (multiple-value-bind (rootstr all-root-info all-pathnames dir-list)
         (list-drive-info drive-letter :last-leveln last-leveln :level-key level-key
                           :leveln leveln 
                          :omit-dirs omit-dirs
                          :incl-subdir-namelists-p incl-subdir-namelists-p
                         ;;fix later?  :omit-dirs omit-dirs           
                          :incl-subdir-namelists-p incl-subdir-namelists-p
                          :remove-final-slashes-p remove-final-slashes-p            
                          :not-return-strings-p not-return-strings-p)

     ;;(when (equal drive 'c)  (BREAK))
     (when all-root-info
       (incf drive-n)
       (setf found-drives (append found-drives (list rootstr))
             drive-info (list drive-n drive-key  rootstr all-root-info)
             all-drive-info (append all-drive-info (list drive-info)))
       ;;end when
       )
     ;;end mvb, loop
     )) 
   
    (values found-drives all-drive-info)
    ;;end let, list-all-drives-info2
    ))|#
;;TEST
;; (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info :omit-dirs '(c)))
;;  (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info  :last-leveln 2 )) ;;later? :omit-dirs '(c)))
;;  (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info :drives '(g)  :last-leveln 2 ))

;;  (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info :drives '(g) :include-only-dir '"\\0 Main OUR PHOTOS-VIDEOS\\"   :incl-subdir-namelists-p T :last-leveln 5 ))

;; BELOW IS SHORT FULL TEST
;;  (multiple-value-setq (*drivesx *dirinfox)  (list-all-drives-info :drives '(g)  :last-leveln 1   :incl-subdir-namelists-p T))
;; works= 
#|(1
  :DRIVE
  "G:\\"
  ((:LEVEL
    1
    "G:\\"
    (:dirpaths
     "\\TRAVEL VIDEOS-audials\\"
     "\\TEMP VIDEOS\\"
     "\\System Volume Information\\"
     "\\Seagate\\"
     "\\PHONE BUs\\"
     "\\My 1420 Software and Manuals-Deleted WhenMOVED\\"
     "\\MOVIES TO MAUI\\"
     "\\From XPS\\"
     "\\3-TS\\"
     "\\Acronis Backups\\"
     "\\5 TO HOME Netflix and Audials\\"
     "\\3 Our New Music\\"
     "\\2-CURRENT XPS MISC BUs\\"
     "\\2 Video Databases-Collectorz\\"
     "\\1 MOVE TO MEDIA CENTER\\"
     "\\1 LW CUMLATIVE BUS\\"
     "\\1 Current LW BUs\\"
     "\\0 Main OUR PHOTOS-VIDEOS\\"
     "\\0 Book Article for Academic Websites\\"
     "\\$RECYCLE.BIN\\")
    (:FILENAMES
     "Warranty.pdf"
     "Set_Up.exe"
     "Set_Up.dmg"
     "PP11Thumbs.ptn2"
     "PP11Thumbs.ptn"
     "maxdesk.ini2"
     "BackupPlus.ico"
     "Autorun.inf")
    (:SUBDIR-INFO
     ("\\TRAVEL VIDEOS-audials\\" ("TRAVEL VIDEOS-audials"))
     ("\\TEMP VIDEOS\\" ("TEMP VIDEOS"))
     ("\\System Volume Information\\" ("System Volume Information"))
     ("\\Seagate\\" ("Seagate"))
     ("\\PHONE BUs\\" ("PHONE BUs"))
     ("\\My 1420 Software and Manuals-Deleted WhenMOVED\\"
      ("My 1420 Software and Manuals-Deleted WhenMOVED"))
     ("\\MOVIES TO MAUI\\" ("MOVIES TO MAUI"))
     ("\\From XPS\\" ("From XPS"))
     ("\\3-TS\\" ("3-TS"))
     ("\\Acronis Backups\\" ("Acronis Backups"))
     ("\\5 TO HOME Netflix and Audials\\" ("5 TO HOME Netflix and Audials"))
     ("\\3 Our New Music\\" ("3 Our New Music"))
     ("\\2-CURRENT XPS MISC BUs\\" ("2-CURRENT XPS MISC BUs"))
     ("\\2 Video Databases-Collectorz\\" ("2 Video Databases-Collectorz"))
     ("\\1 MOVE TO MEDIA CENTER\\" ("1 MOVE TO MEDIA CENTER"))
     ("\\1 LW CUMLATIVE BUS\\" ("1 LW CUMLATIVE BUS"))
     ("\\1 Current LW BUs\\" ("1 Current LW BUs"))
     ("\\0 Main OUR PHOTOS-VIDEOS\\" ("0 Main OUR PHOTOS-VIDEOS"))
     ("\\0 Book Article for Academic Websites\\"
      ("0 Book Article for Academic Websites"))
     ("\\$RECYCLE.BIN\\" ("$RECYCLE.BIN")))
    :LAST-LEVELN
    1)))
|#




;;LIST-ALL-DRIVES-INFO--CURRENT VERSION
;;2020-revised
;;ddd list-all-drives-info
(defun list-all-drives-info (&key (LAST-LEVELN  1) ;;*tomex-data-last-level=3?
                             popup-drive-info-p
                             (drive-letters '(D E F G H I J K L M N O P Q R S T U V))
                             (omit-drive-letters '(C))
                             (set-drivesym-to-symlist-p T)  confirm-drive-vol-names-p 
                             (dirsyms-sym '*tomex-drive-syms)(save-to-file-p T)
                             (data-dir-location (cond ((boundp '*tomex-data-dir)
                                                       *tomex-data-dir)
                                                      (t (setf *tomex-data-dir "c:\\temp\\"))))
                             (if-exists NIL) CHECK-EXISTING-P
                             set-dirsym-to-symlist-p
                             not-return-all-info-symvals-p
                             set-all-drive-info-to-sym (set-all-dirsyms-to-symvals-p T)
                             wildcard-p attributes format  sort-order
                             directories-p lowercase-p  
                             read-only-p hidden-p system-files-p not-content-indexed-p 
                             archive-ready-p 
                             bare-p owner-p  4-digit-years-p reparse-points-p 
                             (show-cmd-p t)
                             return-dos-output-t return-all-lines-p  return-line-lists-p
                             (max-lines 25000) dos-out-prefix)
  "In U-lists RETURNS: (values dirsyms found-drive-paths found-drive-letters n-drives top-dirpaths-keylists) . ALSO SEE MY-DOS-DIRS-INFO [funcs use each other]. IF-EXISTS for data output files is normally :overwrite, :new-version, or NIL  here. CREATES DATA FILES FOR EACH DRIVE LETTER (DIRSYM = FILENAME) in  data-dir-location if save-to-file-p . dirsyms-sym not changed unless save-to-file-p.ONLY SCANS BASIC INFO, NO DIRS OR FILES. Use tomex functions for more data (eg. go-tomex). When confirm-drive-vol-names-p, asks to confirm each name."
  (let
      ((drive-paths)
       (path)
       (list-all-drives-process (mp:get-current-process))
       (found-drive-paths)
       (found-drive-letters)
       (n-drives 0)
       (dirsym-boundp)
       (top-dirpaths-keylists)
       (dirpath-all-level-keylists)
       (leveln 0)
       )
  (setf dirsym-boundp (boundp 'dirsyms-sym))

    ;;1. WHICH DRIVES EXIST ON PC?
      (loop
       for drive in drive-letters
       do
       (let
           ((path)
            )
         (cond
          ((symbolp drive)
           (setf path (format nil "~A:/" drive)))
          (t (setf path drive)))

         (when (directory-p path)
           (incf n-drives)
           (setf found-drive-paths (append found-drive-paths (list path))
                 found-drive-letters (append found-drive-letters
                                             (list (my-make-symbol (subseq path 0 1))))))
         ;;end let,loop
         ))
      ;;(break "after found-drive-paths")

        ;;2. USE LIST-ALL-DRIVES-INFO TO GET MORE INFO & SUBDIRS
          (multiple-value-bind (dirsyms found-drive-paths1 found-drive-letters1
                                           top-dirpaths-keylists1) ;;dirpath-all-level-keylists1)
              (MY-DOS-DRIVES-INFO found-drive-paths
               :popup-drive-info-p popup-drive-info-p
               :LAST-LEVELN last-leveln  :leveln leveln
               :set-dirsym-to-symlist-p  T 
               :not-return-all-info-symvals-p NIL
               :set-all-drive-info-to-sym set-all-drive-info-to-sym 
               :wildcard-p T :attributes  NIL 
               :format format :sort-order sort-order 
               :directories-p directories-p :lowercase-p lowercase-p 
               :read-only-p read-only-p :hidden-p  hidden-p 
               :system-files-p system-files-p 
               :not-content-indexed-p not-content-indexed-p 
               :archive-ready-p archive-ready-p :bare-p  bare-p 
               :owner-p owner-p :4-digit-years-p  4-digit-years-p 
               :reparse-points-p reparse-points-p :show-cmd-p show-cmd-p
               :return-dos-output-t return-dos-output-t
               :return-all-lines-p return-all-lines-p 
               :return-line-lists-p return-line-lists-p
               :max-lines max-lines :dos-out-prefix dos-out-prefix)

            (setf top-dirpaths-keylists top-dirpaths-keylists1)
            ;;(break "after my-dos-drives-info")

          ;;SSSS CHECK TO SEE IF WANT TO USE ANY OF THIS OLDER PART
          #|(when check-existing-p
         ;;OLDER: USE MY-DOS-DIR 
#|         (multiple-value-bind (cur-dir path volname serial-num n-dirs n-files 
                                       used-size free-size  dirlists filelists  dos-out-string dos-out)
             (my-dos-dir path)|#       

           ;;SSS ADDED EDIT DRIVE-VOL NAMES HERE
           ;;initially set drive-name to dos-volname
           (setf dos-volname volname
                  drive-name dos-volname)
           ;;(break "dos-volname")
           ;;CONFIRM NAMES?
           (when confirm-drive-vol-names-p
             (let
                 ((con-inst (make-text-input-or-button-interface-instance 
                             :title "    ENTER DRIVE SYMBOL NAME BELOW  " 
                             :instr-text   "Replace text below for different drive-name"
                             :input-pane-text (format nil "  ~A" drive-name)
                             :confirm-input-p T
                             :current-process list-all-drives-process
                             :poke-process list-all-drives-process
                             ))
                  )                  
                  ;;pause-process
                  (mp:current-process-pause 20)
                  (unless (null *text-input-or-button-interface-textdata)
                    (setf drive-name *text-input-or-button-interface-textdata
                          *text-input-or-button-interface-textdata nil))
                  (capi:destroy con-inst)
                  ;;end let
                  )
             (when (null (my-equal drive-name dos-volname))
               (let
                   ((con-inst2 (make-text-input-or-button-interface-instance 
                                :title "    ENTER DRIVE (VOLUME) NAME BELOW  " 
                                :instr-text   "Replace text below for different drive-name"
                                :input-pane-text (format nil "  ~A" dos-volname)
                                :confirm-input-p T
                                :current-process list-all-drives-process
                                :poke-process list-all-drives-process
                                ))  
                    )
                 ;;pause-process
                 (mp:current-process-pause 20)
                 (unless (null *text-input-or-button-interface-textdata)
                   (setf dos-volname *text-input-or-button-interface-textdata
                         *text-input-or-button-interface-textdata nil))

                 ;;RESET ORIGINAL VOLUME NAME
                 (my-dos-label drive dos-volname  T)
                 (capi:destroy con-inst2)
                 ;;end let
                 )              
               ;;end when,when 
               ))
           ;;MAKE NEW SYMLIST?          
           (setf dirsym (my-make-symbol drive-name)
                   dirsyms (append dirsyms (list dirsym)))
           
           ;;IF PREVIOUS DIRSYM, THEN CHECK IF-EXISTS 
           ;; (is it in dirsyms-sym list, if so omit, unless redo it.
           (when (OR  (and dirsyms-sym dirsym-boundp
                           (not (member dirsym (eval dirsyms-sym) :test 'my-equal)))
                      (member if-exists '(:overwrite :new-version :supercede)))
             (setf host (clean-path-str path :remove-extra-quotes-p T))
             (setf path (directory-namestring path)
                   dirsymlist
                   (list  dirsym  :NAME drive-name  :HOST host  :PATH path
                          :LOCATION data-dir-location :VOLNAME dos-volname
                          :TYPE "??" :BRAND "??" 
                          :SIZE "??"  :USED used-size    :FREE free-size
                          :SERIAL-NUM serial-num :SCANDATE scandate  :LAST-LEVELN last-leveln
                          :CUR-DATE cur-date)
                   all-drive-info-keylists (append all-drive-info-keylists (list dirsymlist)))
             (when dirsym 
               ;;set sym to symlist?
               (when set-drivesym-to-symlist-p
                 (set dirsym dirsymlist))
               ;;(break "dirsym")
               ;;save data to file?
               (when save-to-file-p
                 (save-tomex-drive-data-to-file dirsyms
                                                :data-dir data-dir-location :if-exists if-exists ))
               ;;end when dirsym, when or members,  
               ))
           ;;end check-existing-p
           )|#
          (values dirsyms found-drive-paths found-drive-letters n-drives 
                  top-dirpaths-keylists) ;;level1-info-keylists)
    ;;mvb,let, list-all-drives
    )))
;;TEST
;;( list-all-drives-info :LAST-LEVELN  2 :drive-letters '(F)  :omit-drive-letters '(C) :set-drivesym-to-symlist-p NIL :dirsyms-sym '*tomex-drive-syms :save-to-file-p NIL :if-exists :overwrite )
;; works= (TOM-SE5TBHD-MAIN)  ("F:/")    (F)  1
;; (((TOM-SE5TBHD-MAIN "F:\\" :HOST "  \"F:\\"" :PATH "\\" :VOLNAME "Tom-Se5tbHD-Main" :LEVELN 0 :SERIAL-NUM "B8F3-358D" :N-DIRS "9" :N-FILES "1" :USED "0" :FREE "3169664110592" :DIRLISTS ((:DIR "0 LW-CUM-dated" "03/29/2020; 02:59 PM") (:DIR "0 Main OUR PHOTOS-VIDEOS" "12/31/2019; 01:55 AM") (:DIR "1 Move to MEDIA CENTER" "03/26/2020; 07:58 AM") (:DIR "1 PHONE BUs" "12/31/2019; 11:25 AM") (:DIR "2 MAIN BU" "01/01/2020; 02:18 PM") (:DIR "2 To MEDIA CENTER" "12/30/2019; 01:03 PM") (:DIR "3 Our MUSIC-m4a [master]" "12/30/2019; 01:47 PM") (:DIR "My Software-Manuals" "12/30/2019; 01:13 PM") (:DIR "Video-watch" "12/31/2019; 11:38 AM")) :FILELISTS ((:FILE "TEST-FILE.txt" "04/13/2020; 10:41 AM" "0")) :DOS-OUT-STRING NIL :DOS-OUT 0 :ALL-LINE-LISTS NIL) (TOM-SE5TBHD-MAIN (TOM-SE5TBHD-MAIN "F:\\0 LW-CUM-dated" :HOST "  \"F:\\0 LW-CUM-dated\\"" :PATH "\\0 LW-CUM-dated\\" :VOLNAME "Tom-Se5tbHD-Main" :LEVELN 1 :SERIAL-NUM "B8F3-358D" :N-DIRS "28" :N-FILES "1" :USED "144070" :FREE "3169664110592" :DIRLISTS ((:DIR "1-LW-INIT   ETC ETC ETC
;;
;;  (list-all-drives-info :if-exists :overwrite)
;;  works= ("C:\\" "E:\\" "F:\\" "G:\\")   (C E F G)    4
;; (list-all-drives-info :drive-letters '(f) :confirm-drive-vol-names-p T)
;;(makunbound-vars '(OS C-MAIN-BU TOM-SE5TBHD-MAIN PD-MEDIA ))
;;(setf *tomex-drive-syms NIL)
;;2020-04-12
;;  (list-all-drives-info :if-exists :overwrite)
#|>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/OS.lisp>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/OS.lisp>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/C-MAIN-BU.lisp>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/OS.lisp>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/C-MAIN-BU.lisp>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/TOM-SE5TBHD-MAIN.lisp>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/OS.lisp>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/C-MAIN-BU.lisp>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/TOM-SE5TBHD-MAIN.lisp>>>   SAVING TOMEX DATA FILE:  C:/3-TS/MyDrives/PD-MEDIA.lisp
(OS C-MAIN-BU TOM-SE5TBHD-MAIN PD-MEDIA)
("C:/" "E:/" "F:/" "G:/")  (C E F G)  4
((OS :NAME "OS" :HOST "C:/" :PATH "\\" :LOCATION "C:/3-TS/MyDrives/" :VOLNAME "OS" :TYPE "??" :BRAND "??" :SIZE "??" :USED "122782" :FREE "268774731776" :SERIAL-NUM "4058-766B" :DATE "Date: 4.12.2020  Time: 11:16" :LAST-LEVELN 1) (C-MAIN-BU :NAME "C-MAIN-bu" :HOST "E:/" :PATH "\\" :LOCATION "C:/3-TS/MyDrives/" :VOLNAME "C-MAIN-bu" :TYPE "??" :BRAND "??" :SIZE "??" :USED "159428" :FREE "63729889280" :SERIAL-NUM "F4A2-4D95" :DATE "Date: 4.12.2020  Time: 11:16" :LAST-LEVELN 1) (TOM-SE5TBHD-MAIN :NAME "Tom-Se5tbHD-Main" :HOST "F:/" :PATH "\\" :LOCATION "C:/3-TS/MyDrives/" :VOLNAME "Tom-Se5tbHD-Main" :TYPE "??" :BRAND "??" :SIZE "??" :USED "0" :FREE "3169726525440" :SERIAL-NUM "B8F3-358D" :DATE "Date: 4.12.2020  Time: 11:16" :LAST-LEVELN 1) (PD-MEDIA :NAME "PD-Media" :HOST "G:/" :PATH "\\" :LOCATION "C:/3-TS/MyDrives/" :VOLNAME "PD-Media" :TYPE "??" :BRAND "??" :SIZE "??" :USED "0" :FREE "153501859840" :SERIAL-NUM "064E-B394" :DATE "Date: 4.12.2020  Time: 11:16" :LAST-LEVELN 1))|#
;;ALSO:  C-MAIN-BU =
#|(C-MAIN-BU :NAME "C-MAIN-bu" :HOST "E:/" :PATH "\\" :LOCATION "C:/3-TS/MyDrives/" :VOLNAME "C-MAIN-bu" :TYPE "??" :BRAND "??" :SIZE "??" :USED "159428" :FREE "63729889280" :SERIAL-NUM "F4A2-4D95" :DATE "Date: 4.12.2020  Time: 11:8" :LAST-LEVELN 1)|#



;;DRIVE-EXISTS-P
;;2019
;;ddd
(defun drive-exists-p (drive &key return-all-drives-info-p)
     "U-files, drive can be sym or string with w or w/o colon=\"C:\", \"C\".  Uses find-all-drives.  RETURN-ALL-DRIVES-INFO-P RETURNS (values drive-exists-p drive-syms drive-paths found-drive-letters n-drives) (eg = G  (OS USBN1-128 SD01-128)  (\"C:/\" \"F:/\" \"G:/\")  (C F G)  3)"
  (let*
      ((drive-exists-p)
      ;; (drive-noslash (delete-final-string '("\\" "/")))
       (drive-let-sym (cond  ((symbolp drive) drive)
                             (t (my-make-symbol (subseq drive 0 1)))))
       )
  (multiple-value-bind ( drive-syms drive-paths found-drive-letters n-drives)
      (list-all-drives)
    (when (member drive-let-sym found-drive-letters :test 'equal)
      (setf drive-exists-p drive-let-sym))
    (cond
     (return-all-drives-info-p              
      (values drive-exists-p drive-syms drive-paths found-drive-letters n-drives))
     (t drive-exists-p))
    ;;end mvb,let, drive-exists-p
    )))
;;TEST
;; (drive-exists-p 'G :return-all-drives-info-p T)
;; works = G  (OS USBN1-128 SD01-128)  ("C:/" "F:/" "G:/")  (C F G)  3
;; (drive-exists-p "f:" ) = F
;; (drive-exists-p "C:\\") = C
;; (drive-exists-p 'j) = NIL



;;LIST-DOS-DRIVE-INFO--DEPRECIATED, BUT WORKS?
;;
;;ddd
(defun list-dos-drive-info (root &key (leveln 0) (last-leveln 3) (level-key :LEVEL) 
                             (find-dos-info-p  T) omit-dirs
                             ;;(return-namestrings-p T)
                             ;; incl-subdir-namelists-p 
                             (remove-final-slashes-p T)
                             list-hostless-subdirs-p
                             not-return-strings-p )
  "U-lists USE MY-DOS-INFO INSTEAD? RETURNS (values rootstr all-root-info  host-str).  IF NOT-RETURN-STRINGS-P, returns only the actual paths for the root.  When  INCL-SUBDIR-NAMELISTS-P, sets each list in subdir-info-lists to (path subdirnames), if nil = (path).  If  remove-final-slashes-p, removes final slashes for each subdirname. subdir-info-lists is included as part of every all-inclusive all-root-info list. If return-namestrings-p, returns namestrings instead of paths--takes additional processing. If FIND-DOS-INFO-P, also lists :used :free :name :serial-num :n-dirs :n-files"
  (let
      ((dirlists)
       (dirs)
       ;;(realdirpaths)
       (realfilepaths)
       (file-pathnames)
       (filenames)
       (dir-names)
       (dir-names1)
       (all-pathnames )
       (rootstr)
       (root-info1)
       (newroot)
       (all-root-info)
       (subdir-names)
       (subdir-info)
       (level-subdir-info)

       (sub-rootstr) 
       (sub-root-info)
       (level-subroot-info)
#|       (volname)
       (serial-num)
       (dos-n-dirs)
       (dos-n-files)
       (used)
       (free)
       (dirlists)
       (filelists)|#
       (dirlists)
       ;;from mvs
       (cur-dir)
       (host) 
       (host-str )
       ;;(hostless-subdirs)
       )
    (incf leveln)

    (cond
     ((symbolp root)
      (setf rootstr (format nil "~A:/" root)))  ;;was (format nil "~A:\\" root)))
     ((pathnamep root)
      (setf rootstr (namestring root)))
     (t (setf rootstr root)))

    (setf rootstr (my-delete-first-spaces rootstr))
    
    (when (null (member rootstr omit-dirs))

    ;;THIS LEVEL DIR LIST TO BE EXPLORED
    ;;(break "dir-list")
    
      ;;FIND NEXT LEVEL ROOT-INFO: 
      (multiple-value-bind (cur-dir host volname serial-num dos-n-dirs dos-n-files 
                                    used free  dirlists1  filelists )
          (my-dos-dir rootstr)

        (setf host-str host ;;is this right??
              dirlists  dirlists1)
        ;;(BREAK "mvs")
        ;;dirlist= (list :dir dirname date time am-pm)
        ;; filelist= (list :file filename date time am-pm size)
        #| was     (multiple-value-setq (filenames file-pathnames dir-names realfilepaths  
                                      realdirpaths  host-str hostless-subdirs) 
          (list-directory-contents rootstr :incl-hostless-subdir-names-p list-hostless-subdirs-p))|#
        ;;(when (and (<= leveln last-leveln) realdirpaths)

        (cond
         ((= leveln 1)
          (setf  root-info1 
                 (list level-key leveln  
                                 :DIRPATH rootstr 
                                 (append (list :DIRS) dirlists) ;;was dirnames)
                                 (append (list :FILENAMES) filelists)
                                 ;;(append (list :SUBDIR-INFO) level-subdir-info) 
                                 :SERIAL-NUM serial-num :N-DIRS dos-n-dirs :N-FILES dos-n-files
                                 :USED used :FREE free
                                 :LAST-LEVELN last-leveln)))
         (t (setf  root-info1 
                 (list level-key leveln 
                                 :DIRPATH rootstr 
                                 (append (list :DIRS) dirlists) ;;was dirnames)
                                 (append (list :FILENAMES) filelists)
                                 :LAST-LEVELN last-leveln))))
          
          ;;(break "root-info1")
          ;;end when not member omit-dirs
          ))

        ;;(break "after set root-info")

        ;;RECURSE IF DIRS?
        (when  dirlists ;;was dir-names  ;;was dirnames     ;;was dirs
          (loop
           for dirlist in dirlists
           ;;was for newroot in dir-names ;;was dirnames   ;;was dirs
           do
           (setf dirname (second dirlist)
                 newroot  (format nil "~A~A/" rootstr dirname)) 
                 ;;was (format nil "~A~A\\" rootstr dirname))
           ;;(break "newroot")

           (when  (< leveln  last-leveln)
             (multiple-value-setq (sub-rootstr sub-root-info)  ;;not needed?? all-pathnames dir-list host-str)
                 (list-dos-drive-info newroot :last-leveln last-leveln :level-key level-key
                                  :leveln leveln
                                  :omit-dirs omit-dirs))
                                ;;  :list-hostless-subdirs-p list-hostless-subdirs-p
                               ;;   :incl-subdir-namelists-p incl-subdir-namelists-p))
             (when sub-root-info
               (setf level-subroot-info (append level-subroot-info (list sub-root-info))))

             ;;(when (null sub-root-info)(break "end loop"))
             ;;(break "end loop")
          ;end inner when, mvb, loop
             ))
          (when  level-subroot-info
            (setf all-root-info (list  level-subroot-info)))    
          #|was (cond
           (level-subroot-info
            (setf all-root-info (list root-info level-subroot-info)))
           (t (setf all-root-info (list root-info ))))|#
          ;;(break "end")
          ;;end  when dirlists
          )
        (when root-info1
          (setf all-root-info (append (list root-info1) all-root-info)))
    (values rootstr all-root-info)  ;;was all-pathnames  realdirpaths host-str) ;;was dirnames)  ;was dir-list)
    ;;end let, list-dos-drive-info
    ))
;;TEST
;; (list-dos-drive-info "E:/")
;; (list-dos-drive-info "E:\\")
;; works=  rootstr= "E:\\"
#|all-root-info= (:LEVEL  1
  :NAME   "SD01-128"
  :DIRPATH   "E:\\"
  (:DIRS
   (:DIR "1 LW FOLDERS CUM" "10/17/2016; 09:21 PM")
   (:DIR "2 LW Folders" "11/29/2016; 07:40 PM")
   (:DIR "5 Selected XPS Folder BUs" "12/15/2016; 09:52 AM")
   (:DIR "0-Our Health Info" "11/23/2016; 06:16 PM"))
  (:FILENAMES
   (:FILE "OS (C) - Shortcut.lnk" "12/13/2016; 01:14 PM" "444"))
  :SERIAL-NUM
  "3634-6637"
  :N-DIRS
  "4"
  :N-FILES
  "1"
  :USED
  "444"
  :FREE
  "122435141632"
  :LAST-LEVELN
  3) ((((:LEVEL 2 :DIRPATH "E:\\1 LW FOLDERS CUM\\" (:DIRS (:DIR "CogSys-Model" "10/17/2016; 09:18 PM") (:DIR "CogSysOutputs" "10/12/2016; 05:46 PM") (:DIR "MyUtilities" "10/17/2016; 09:01 PM") (:DIR "ART2-Output Data" "08/08/2015; 08:42 PM") (:DIR "ART3" "04/23/2016; 05:49 PM") (:DIR "ART3-output-data" "02/12/2016; 08:38 PM") (:DIR "ART-LW" "09/26/2015; 04:23 PM") (:DIR "ARTMAP-JavaVersion" "08/08/2015; 09:27 PM") (:DIR "ART-Utilities" "05/02/2016; 06:51 PM") (:DIR "CL-Utilities" "10/27/2014; 01:27 PM") (:DIR "Toms-HDrive-Explorer" "01/20/2017; 01:35 PM") (:DIR "H-Capi" "04/11/2017; 02:53 PM")) (:FILENAMES) :LAST-LEVELN 3) (((:LEVEL 3 :DIRPATH "E:\\1 LW FOLDERS CUM\\CogSys-Model\\" (:DIRS (:DIR "Older" "09/08/2016; 06:28 PM") (:DIR "CSQ-config (StevensXPS's conflicted copy 2016-12-25).lisp" "12/17/2016; 02:14 PM")   
ETC   ETC  END FOLLOWS
((:LEVEL 3 :DIRPATH "E:\\0-Our Health Info\\Prescriptions & Info\\" (:DIRS (:DIR "Sherry Prescription Med Info" "05/24/2016; 07:17 PM") (:DIR "Tom Prescription Med Info" "07/03/2012; 05:54 PM")) (:FILENAMES) :LAST-LEVELN 3)) ((:LEVEL 3 :DIRPATH "E:\\0-Our Health Info\\Sherry Surgery\\" (:DIRS (:DIR "Laparoscopic and Robotic Surgery" "05/15/2013; 07:25 PM") (:DIR "Mat Clark MD UroGynocological Center" "05/15/2013; 03:58 PM") (:DIR "Sling Info" "05/15/2013; 06:45 PM") (:DIR "UCLA Urology Shlomo Raz MD" "05/15/2013; 07:23 PM")) (:FILENAMES (:FILE "2014-06-14 Dr Raz letter.docx" "06/14/2014; 02:27 PM" "11194") (:FILE "2016-02-24 ToDrRaz email.docx" "03/03/2016; 03:47 PM" "16551") (:FILE "Urologists.docx" "05/15/2013; 06:57 PM" "20918")) :LAST-LEVELN 3)) ((:LEVEL 3 :DIRPATH "E:\\0-Our Health Info\\Shlomo Raz MD UCLA\\" (:DIRS) (:FILENAMES (:FILE "2014-03-02 Email to Dr Raz.docx" "03/02/2014; 07:35 PM" "12277") (:FILE "2015-02-9 email to Dr Raz.docx" "02/09/2015; 02:22 PM" "12238")) :LAST-LEVELN 3)))))))
|#





;;MY-DOS-DIR
;;2017
;;ddd
(defun my-dos-dir (path &key 
                        wildcard-p attributes format  sort-order
                        directories-p lowercase-p  (clean-host-path-p T)
                        return-slash-type 
                        read-only-p hidden-p system-files-p not-content-indexed-p 
                        archive-ready-p 
                        bare-p owner-p  4-digit-years-p reparse-points-p (show-cmd-p t)
                        ;;(file-names-first-p t) doesn't work
                        return-dos-output-t return-all-lines-p  return-line-lists-p
                        (max-lines 25000) dos-out-prefix)
  "U-files,  RETURNS  (values cur-dir host volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out all-line-lists)
   INPUT: note:  must incl \\ eg  c:\\ (not c: ) or scans current dir.
   WILDCARD-P  is when path includes eg. \"*.lisp\".
   ATTRIBUTES [d=directories, r=read-only files, h=hidden files, a=files ready for archiving, s=system files, i=not content indexed files, l=reparse points,  - =prefix meaning not.] 
   FORMAT: is :wide, :col (same as wide, but by cols), or nil.
   SUBDIR-FILES-P: displays files in specified directory and all subdirectories.   
   BARE-P= (no headings), /-c=no thousand sep, /d=sorted by col,      
   SORT-ORDER:  :name, :size (smallest first), :extension, :dir-first, :create-date, :last-access,  :last-access,:last-write [date/time (oldest first)]   -  =prefix to reverse order.
   NOTE: following not used:  /r=display alternate data streams of the file,   /x=this displays the short names generated for non-8dot3 file,  /p=pauses after each screenful of information, 
   RETURN-LINE-LISTS-P returns all lines as lists of tokens. "
  (let
      ((command-str "dir ")
       (com-rest-str " ")
       (attribute-str "/a: ")
       (rest-attr-str "")
       (format-str " ")
       (sort-order-str " /o: ")
       )

    ;;STEP 1: MAKE THE DIR COMMAND STRING

    #|       (attribute-keys-values '((directories-p "d")(read-only-p "r")(hidden-p "h")
                                (archive-ready-p "a")(system-files-p "s")(not-content-indexed-p "i")
                                (reparse-points-p "l")))|#
    ;;(format-keys-values '((:wide "/w")(:col "/d")))
              
    #|subdir-files-p: displays files in specified directory and all subdirectories.   
   bare-p= (no headings), /-c=no thousand sep, /d=sorted by col,      
   sort-order:  n=by name (alphabetic), s=by size (smallest first), e=by extension (alphabetic), d=by date/time (oldest first), g=group directories first,  -  =prefix to reverse order.
   sort-time-field: :create :last-access, or :last-write (tme field used for sorting).      
|#    
    ;;make attribute-str
    (setf attribute-str
          (when-key-add-value directories-p t " d " :begin-str attribute-str)
          attribute-str
          (when-key-add-value read-only-p t " r " :begin-str attribute-str)
          attribute-str
          (when-key-add-value hidden-p t " h " :begin-str attribute-str)   
          attribute-str
          (when-key-add-value archive-ready-p t " a " :begin-str attribute-str)
          attribute-str
          (when-key-add-value system-files-p t " s " :begin-str attribute-str)
          attribute-str
          (when-key-add-value not-content-indexed-p t " i " :begin-str attribute-str)
          attribute-str
          (when-key-add-value reparse-points-p t " l " :begin-str attribute-str))

    ;;make format-str 
    (cond
     ((equal format :wide)
      (setf format-str " /w "))
     ((equal format :col)
      (setf format-str " /d "))
     )
    ;;make sort-order-str
    ;;n=by name (alphabetic), s=by size (smallest first), e=by extension (alphabetic), d=by date/time (oldest first), g=group directories first
    (when sort-order
      (cond
       ((equal sort-order :name)
        (setf sort-order-str  " /o: n "))
       ((equal sort-order :size)
        (setf sort-order-str  " /o: s "))
       ((equal sort-order :extension)
        (setf sort-order-str  " /o: e "))
       ((equal sort-order :dir-first)
        (setf sort-order-str  " /o: g "))
       ;;sort-time-field: :create :last-access, or :last-write (tme field used for sorting).
       ;;should i incl /o too??
       ((equal sort-order :create-date)
        (setf sort-order-str  " /t: c "))
       ((equal sort-order :last-access)
        (setf sort-order-str  " /t: a "))
       ((equal sort-order :last-write)
        (setf sort-order-str  " /t: w "))
       (t nil)))

    ;;lowercase-p   
    (when lowercase-p (setf rest-attr-str (format nil "~a /l " rest-attr-str)))
    ;;bare-p
    (when bare-p (setf rest-attr-str (format nil "~a /b " rest-attr-str)))
    ;;owner-p
    (when owner-p (setf rest-attr-str (format nil "~a /q " rest-attr-str)))
    ;;4-digit-years-p
    (when 4-digit-years-p (setf rest-attr-str (format nil "~a /4 " rest-attr-str)))
    ;; /n=new long list format where filenames are on the far right.
    ;;file-names-first-p doesn't work, tried various combos
   ;;(when 4-digit-years-p (setf rest-attr-str (format nil "~a /-n " rest-attr-str)))

   ;;must use / not \\ for paths.  if spaces in path, or if filespec? must have quotes around all   
   (setf path (clean-path-str path :wildcard-p wildcard-p :enclose-in-quotes-p t :remove-extra-quotes-p NIL :slash-type :forward))
 ;; " \"e:/\""
    ;;(break "path f")
    ;;make final command-str
    (setf command-str (format nil "~a  ~a"  command-str path))  ;;2nd wat ~s
    (when (> (length attribute-str) 8)
      (setf command-str (format nil "~a  ~a"  command-str attribute-str)))
    (when (> (length sort-order-str) 5)
      (setf command-str (format nil "~a  ~a"  command-str sort-order-str)))
    (when (> (length format-str) 1)
      (setf command-str (format nil "~a  ~a"  command-str format-str)))

    ;;STEP 2: RUN THE NEW DIR COMMAND
  ;;SSSSS USE AS MODEL FOR NEW FUNCTION -- 
       ;;1-MVB CAPTURE OUT-STRING and OUTPUT results of function call
    (multiple-value-bind (DOS-OUT-STRING dos-out)
        (CALL-WITH-SHOW-DOS-OUTPUT command-str
                                   :prefix dos-out-prefix :show-cmd show-cmd-p)    
      ;;(break "1 mvb,command-str")

      ;;STEP 3: SORT THE DOS-OUT-STRING
      ;;2- Use above OUT-STRING as INPUT to function to sort the string.
      (multiple-value-bind (cur-dir host volname n-dirs 
                                    n-files used-size free-size 
                                     serial-num  dirlists filelists 
                                     all-lines all-line-lists)  
          (MAKE-DIR-INFOLISTS-FROM-DOS-DIR  DOS-OUT-STRING 
                                            :return-all-lines-p return-all-lines-p
                                            :return-line-lists-p return-line-lists-p
                                           :max-lines max-lines)
        (unless return-dos-output-t
          (setf dos-out-string nil))
        ;;(break "host")
        (when clean-host-path-p
          (setf host (clean-path-str host :slash-type return-slash-type)))
      (values cur-dir host volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out all-lines all-line-lists)
      ;;end mvb,mvb,let, my-dos-dir
      ))))
;;TEST
;;HERE1
;;2019 TO TEST WILDCARDS
;; (MY-DOS-DIR "C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP" :wildcard-p T :return-dos-output-t T  :return-line-lists-p T)
;;RETURNS=> CUR-DIR HOST VOLNAME SERIAL-NUM N-DIRS N-FILES-X USED-SIZE-X  FREE-SIZE  DIRLISTS FILELISTS  DOS-OUT-STRING DOS-OUT all-line-lists)
;; RESULTS = " C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES"  "  \"C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP\""  "OS"   "4058-766B"  "0"  "62"   "4465221"   "381897928704"  NIL   ((:FILE "0-CODE-TEMPLATES.lisp" "08/03/2018; 08:31 PM" "339") (:FILE "ARC-curve-function-tests.lisp" "05/08/2018; 06:48 PM" "15903") (:FILE "space-show-arglist.lisp" "08/11/2010; 02:36 PM" "2837") (:FILE "U-Arrays.lisp" "11/26/2013; 03:48 PM" "23466") (:FILE "U-BASIC-functions.lisp" "02/27/2019; 07:05 PM" "10138") (:FILE "U-capi - Copy.lisp" "04/22/2016; 03:06 PM" "4527")  ETC   
;; DOS-OUT-STRING
 #|"DIR   \"C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP\"
 Volume in drive C is OS
 Volume Serial Number is 4058-766B

 Directory of C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES

08/03/2018  08:31 PM               339 0-CODE-TEMPLATES.lisp
05/08/2018  06:48 PM            15,903 ARC-curve-function-tests.lisp|#
;; DOS-OUT = 0   ALL-LINE-LISTS= NIL
;; 
#|0 ("DIR" "\"C:\\3-TS\\LISP" "PROJECTS" "TS\\MYUTILITIES\\*.LISP\"")
1 ("Volume" "in" "drive" "C" "is" "OS")
2 ("Volume" "Serial" "Number" "is" "4058-766B")
3 NIL
4 ("Directory" "of" "C:\\3-TS\\LISP" "PROJECTS" "TS\\MYUTILITIES")
5 NIL
6 ("08/03/2018" "08:31" "PM" "339" "0-CODE-TEMPLATES.lisp")
7 ("05/08/2018" "06:48" "PM" "15903" "ARC-curve-function-tests.lisp")

5 NIL
6 ("08/03/2018" "08:31" "PM" "339" "0-CODE-TEMPLATES.lisp")
7 ("05/08/2018" "06:48" "PM" "15903" "ARC-curve-function-tests.lisp")
ETC
63 ("08/06/2017" "05:22" "PM" "15563" "U-trees-flatlists.lisp")
64 ("07/23/2019" "05:54" "PM" "81732" "U-trees-old.lisp")
65 ("08/09/2019" "08:48" "PM" "93451" "U-trees.lisp")
66 ("04/16/2015" "05:18" "PM" "121474" "U-tstring-OLD-make-index-funs.lisp")
67 ("09/21/2019" "08:03" "PM" "259590" "U-tstring.lisp")
68 ("62" "File(s)" "4463962" "bytes")
69 ("0" "Dir(s)" "381893693440" "bytes" "free")
;;ALL-LINE-LISTS = (("DIR" "\"C:\\3-TS\\LISP" "PROJECTS" "TS\\MYUTILITIES\\*.LISP\"") ("Volume" "in" "drive" "C" "is" "OS") ("Volume" "Serial" "Number" "is" "4058-766B") NIL ("Directory" "of" "C:\\3-TS\\LISP" "PROJECTS" "TS\\MYUTILITIES") NIL ("08/03/2018" "08:31" "PM" "339" "0-CODE-TEMPLATES.lisp") ("05/08/2018" "06:48" "PM" "15903" "ARC-curve-function-tests.lisp") ("08/11/2010" "02:36" "PM" "2837" "space-show-arglist.lisp") ("11/26/2013" "03:48" "PM" "23466" "U-Arrays.lisp") ("02/27/2019" "07:05" "PM" "10138" "U-BASIC-functions.lisp") ("04/22/2016" "03:06" "PM" "4527" "U-capi" "-" "Copy.lisp")
|#




;; (MY-DOS-DIR  "F:/1-LW-CUM/")
;;WORKS= CL-USER 9 > (MY-DOS-DIR  "F:/1-LW-CUM/")   " F:\\1-LW-CUM"  "  \"F:/1-LW-CUM/\""  "USBN1-128"  "7C21-483B"  "IR(S)"  NIL  NIL  "BYTES" 
;; ((:DIR "MYUTILITIES" "02/26/2019; 09:47 AM"))  ((:FILE "MY-WRITE-TEST2.LISP" "02/26/2019; 09:46 AM" "189"))  NIL  0
;; (MY-DOS-DIR  "E:\\1 LW FOLDERS CUM")
;; (MY-DOS-DIR (FORMAT NIL "~S" "E:\\1 LW FOLDERS CUM\\"))
;; (MY-DOS-DIR "C:\\")
;; WORKS=  " C:\\"   "  C:\\ "   "OS"   "4058-766B"   "26"   "2"  "6493"  "659432439808"
;;((:DIR " AMD" "02/23/2017; 12:47 PM") (:DIR " APPS" "02/04/2017; 02:23 PM") (:DIR " BROTHER" "03/22/2017; 11:41 AM") (:DIR " DELL" "02/04/2017; 03:17 PM") (:DIR " DRIVERS" "02/04/2017; 12:58 PM") (:DIR " 3-TS" "04/30/2017; 08:33 AM") (:DIR " EECOLORTABLES" "02/04/2017; 02:12 PM") (:DIR " INTEL" "02/04/2017; 02:06 PM") (:DIR " MY SOFTWARE-MANUALS" "03/22/2017; 11:52 AM") (:DIR " OUR NEW MUSIC" "02/09/2017; 04:32 AM")  ETC  (:DIR " TOMEX" "04/15/2017; 06:01 PM") (:DIR " USERS" "02/08/2017; 11:31 AM") (:DIR " VIDEOS-TEMP" "02/10/2017; 08:40 PM") (:DIR " WINDOWS" "04/12/2017; 06:18 PM") :N-DIRS "26" :N-FILES "2" :SIZE "6493" :FREE "659432439808")
;;((:FILE "BDLOG.TXT" "04/13/2017; 07:53 PM" "6291") (:FILE "STARTMENU.INI" "02/04/2017; 02:22 PM" "202") :N-FILES "2")
;;NIL
;;0

#| ("DIRECTORY OF C:\\3-TS\\LISP PROJECTS TS

04/13/2017  09:49 AM    <DIR>          .
04/13/2017  09:49 AM    <DIR>          ..
05/03/2016  05:26 PM           125,375 .LISPWORKS
05/03/2016  05:23 PM           125,283 .LISPWORKS~
09/16/2014  12:51 PM             1,422 1-LISP 7 NOTES.LISP
08/17/2014  03:29 PM               562 1-LISP 7 NOTES.LISP~
01/12/2014  11:10 PM                78 1-LISP-TEST.TXT
02/08/2017  07:39 PM    <DIR>          AI-NLP
02/08/2017  05:43 PM    <DIR>          ANDYCARES
02/09/2017  10:48 AM    <DIR>          ART-LW
02/08/2017  06:24 PM    <DIR>          ART-PRENESTSYMBU
02/23/2017  07:57 PM    <DIR>          ART-UTILITIES
06/05/2015  04:22 AM            36,777 ART2-MULTIPANE-INTERFACE-SCROLL.LISP
02/09/2017  03:38 PM    <DIR>          ART2-OUTPUT DATA
02/08/2017  06:00 PM    <DIR>          ART3
02/08/2017  06:24 PM    <DIR>          ART3-BU
02/09/2017  07:07 AM    <DIR>          ART3-OUTPUT-DATA
02/08/2017  07:47 PM    <DIR>          ARTMAP-JAVAVERSION
02/08/2017  07:50 PM    <DIR>          CL-UTILITIES
02/11/2017  11:05 PM    <DIR>          COGSYS-MODEL
02/08/2017  08:26 PM    <DIR>          COGSYSOUTPUTS
03/26/2016  05:55 PM           168,115 COGTEST-DELIVER LOG.LISP
03/28/2016  12:45 PM             8,594 CSQ-PC-FRAMES.LISP
05/02/2016  07:00 PM           123,809 CURRENT LISPWORKS 7.0 COPY.LISP
01/22/2017  02:22 PM                 0 FAP4B66.TMP
03/24/2015  03:43 PM           148,904 FOUT-OUT-20CYCLES.LISP
03/23/2015  05:40 PM           159,102 FOUT-OUT-20CYCLES.LISP~
04/13/2017  03:50 PM    <DIR>          H-CAPI
02/20/2017  03:56 PM    <DIR>          H-CAPIEXAMPLES
04/21/2017  08:44 AM    <DIR>          H-HELP
05/03/2016  05:29 PM           125,375 LISPWORKS-7-0 BU.LISP
05/02/2016  05:51 PM           108,264 LISPWORKS-LAST 6.1.LISP
02/08/2017  06:20 PM    <DIR>          LW6-EXAMPLES
02/08/2017  06:20 PM    <DIR>          LW7-EXAMPLES
02/08/2017  05:04 PM    <DIR>          LWSTART
02/08/2017  04:56 PM    <DIR>          MATH
11/24/2014  01:13 PM           108,180 MY SHAQ RESULTSALL-PT1.TXT
11/24/2014  01:14 PM           504,495 MY SHAQ RESULTSALL-PT2.TXT
02/09/2017  06:24 AM    <DIR>          MYCONFIGFILES
02/25/2016  09:04 PM                 0 MYFILE.TXT
02/09/2017  05:23 AM    <DIR>          MYINTERFACES
02/09/2017  02:33 AM    <DIR>          MYLISPWEBS
02/08/2017  08:35 PM    <DIR>          MYLWEX-UTILITIES
02/08/2017  06:01 PM    <DIR>          MYPROJECTS2
04/21/2017  08:36 AM    <DIR>          MYUTILITIES
02/09/2017  11:53 AM    <DIR>          NATURAL LANGUAGE CODE ETC
11/17/2013  05:43 PM             2,076 PACKAGE.LISP
11/17/2013  05:33 PM             1,996 PACKAGE.LISP~
02/08/2017  06:29 PM    <DIR>          PCDB
11/17/2013  05:47 PM             7,868 PERMUTATION-EXAMPLES.LISP
11/17/2013  05:45 PM             7,892 PERMUTATION-EXAMPLES.LISP~
11/17/2013  05:54 PM            15,150 PERMUTATION.LISP
11/17/2013  05:50 PM            15,150 PERMUTATION.LISP~
09/13/2013  03:06 PM            92,043 PREVIOUSBU.LISPWORKS
09/16/2013  06:40 PM           100,069 PREVIOUSCOPY.LISPWORKS
02/22/2017  01:21 PM                55 SAMSUNG-128USB.LISP
02/09/2017  06:05 AM    <DIR>          SCREENSAVER
02/09/2017  07:52 AM    <DIR>          SCREENSAVER2013
02/12/2017  12:22 AM    <DIR>          SECURITYSIGNING INFO-ISSUES
01/12/2012  04:00 PM             7,858 SELECTION-KEY-BINDS.LISP
02/09/2017  01:59 AM    <DIR>          SHAQ
02/09/2017  10:48 AM    <DIR>          SHAQ - COPY2015-03-30
02/09/2017  12:09 PM    <DIR>          SHAQ - COPY2015-09-08
02/09/2017  09:50 AM    <DIR>          SHAQ-COPY2014-12-05
02/09/2017  09:37 AM    <DIR>          SHAQ-COPY2015-01-13
11/29/2014  06:21 PM           124,731 SHAQ-DATA-LIST-ALL.LISP
11/29/2014  06:04 PM           124,004 SHAQ-DATA-LIST-ALL.LISP~
11/29/2014  02:29 PM            32,441 SHAQ-DATA-LIST-ANGER.LISP
11/28/2014  07:18 PM            32,428 SHAQ-DATA-LIST-ANGER.LISP~
11/26/2014  09:06 PM            26,396 SHAQ-DATA-LIST-EG1.LISP
09/26/2014  05:16 PM           122,866 SHAQ-DATA-LIST-EG1.LISP~
09/11/2014  07:16 PM            65,002 SHAQ-DATA-LIST-EQ2.LISP
09/11/2014  05:23 PM            54,128 SHAQ-DATA-LIST-EQ2.LISP~
11/19/2014  05:28 PM            94,829 SHAQ-DATA-LIST-FOR-SPSS.LISP
10/29/2014  12:42 PM           117,958 SHAQ-DATA-LIST-FOR-SPSS.LISP~
10/18/2014  07:17 PM            50,600 SHAQ-DATA-LIST-NOACADCAR.LISP
10/18/2014  06:24 PM            12,203 SHAQ-DATA-LIST-OUTOMCESONLY.LISP
09/29/2015  04:29 PM         1,684,199 SHAQ-DELIVER LOG.LISP
02/08/2017  11:29 PM    <DIR>          SHAQ-PLUS
02/09/2017  05:43 PM    <DIR>          SHAQ-RESULTSTEXTETC
02/09/2017  06:22 AM    <DIR>          SHAQ-WEB SIGNED-EXE PHP
02/09/2017  01:35 AM    <DIR>          SHAQXNOTUSED
11/16/2014  11:58 PM            74,081 SPSS-TEST-DATAFILE.SAV
08/15/2013  03:07 PM                66 TAGS
11/26/2014  12:11 PM                 0 TEST-BUFFER.LISP
11/19/2014  04:13 PM            16,448 TEST-BUFFER.LISP~
11/18/2014  03:55 PM           125,541 TEST-COMBINE.LISP
05/15/2016  08:02 PM            16,860 TEST-FUNCTIONS2.LISP
05/15/2016  03:21 PM             9,880 TEST-FUNCTIONS2.LISP~
03/24/2014  02:40 PM               390 TEST-OBJECT.LISP
11/16/2014  09:15 AM            19,683 TEST-SPSS-OUTPUT.TXT~
11/25/2014  06:24 PM            12,093 TEST-SPSS-OUTPUT0.TXT
11/16/2014  11:59 PM            15,559 TEST-SPSS-OUTPUT1.TXT
04/21/2017  11:51 AM    <DIR>          TOMS-HDRIVE-EXPLORER
02/09/2017  04:34 PM    <DIR>          VIDEO-PLAYBACK-UTILITY
02/08/2017  09:24 PM    <DIR>          VIDEOPLAYBACK
02/08/2017  06:06 PM    <DIR>          Z-LISPWORKS FILES
              51 FILE(S)      4,826,878 BYTES
              46 DIR(S)  665,718,132,736 BYTES FREE
")
|#

;;FOR WILDCARD FILENAMES at path end [eg.  /*.lisp"
;; (my-dos-dir "  C:/3-TS/LISP PROJECTS TS/MYUTILITIES/*.LISP   " :WILDCARD-P T)

;;MAKE-DIR-INFOLISTS-FROM-DOS-DIR
;;
;;DDD
(defun make-dir-infolists-from-dos-dir (dos-output-str 
                                        &key (return-all-lines-p t) 
                                        return-line-lists-p
                                                       (max-lines 25000))
  "U-files, returns  (values cur-dir host volname dos-n-dirs dos-n-files used-size free-bytes  serial-num  all-dirs-info all-files-info  all-lines all-line-lists).  DOS-DIR is the dir being scanned, IF NOT SUPPLIED, just lists 'root' return-all-lines-p prints a string of the exact dos output."
  (let
      ((dirlists)
       (filelists)
       (dos-n-dirs)
       (dos-n-files)
       ;;(n-dirs)
       ;;(n-files)
       (used-size)
       (free-bytes)
       (host)
       (volname)
       (serial-num)
       (cur-dir)
       (dos-input-stream (make-string-input-stream dos-output-str))
       (all-lines "")
       (all-files-info)
       (all-dirs-info)
       (all-dos-filelines-info)
       (all-line-lists)
       )
    ;;(break "dos-output-str")
    (with-open-stream (dos-str dos-input-stream)
      (loop
       for line-n from 1 to max-lines
       do
       (let
           ((len-line)
            (line1 "")
            (line "")
            (file-line-info) 
            (line-list '(""))
            (date-time)
            (dir-infolist)
            (file-infolist)
            (test-out)
            (dirname)
            (filename)
            )
         ;;read the current line
         (setf line (read-line dos-str  nil 'eof)
               ;;line (format nil "~a" line1)
               all-lines (format nil "~a~%~a" all-lines line))

         ;;return from loop?
         (when  (equal line 'eof)
           (return))

         ;;line length
         (setf len-line (length line))

         ;;if first line = "", add 1 to line-n  ;;2019
         (when (and (= line-n 1)(< len-line 2))
           (setf line-n (- line-n 1)))

         ;;divide string into token strings
         (setf line-list (divide-string-to-all-tokens line :ignore-char-list '(#\,  #\tab))) ;;not #\.
         ;;(divide-string-to-all-tokens " volume in drive c is os" :ignore-char-list '(#\,  #\tab))
         (when return-line-lists-p
           (setf all-line-lists (append all-line-lists (list line-list))))

         ;;(afout 'out (format nil ">>> line= ~a~%line-list= ~a~%test-out= ~a" line line-list  test-out))

         ;;check each line
         (cond
          ;;lines 1-5
          ((= line-n 1)
           (setf host (subseq line 4)))
          ((= line-n 2)
           (setf volname (cond ((match-substring "is" (car (last line-list))))
                               (T (car (last line-list))))))   ;; (subseq line 21)))
          ((and (> line-n 2)(< line-n 7)        
           (fuzzy-match-in-string-list "serial" line-list))
           (setf serial-num (car (last line-list))))
            ;; (find-best-match "serial' 
            ((and (> line-n 2)(< line-n 10)
                  (fuzzy-match-in-string-list "Directory" line-list))
             (setf cur-dir  (subseq line 13)))
            #|    was, caused some errors      ((= line-n 3)
           (setf serial-num (car (last line-list)))) ;;(subseq line 24)))
          ((= line-n 5)
           (setf cur-dir  (subseq line 13)))   ;; (subseq line 12)))
          ;;((break "find-best-match"))
          ;;for num files and bytes line
          ;;              51 file(s)      4,826,878 bytes|#
          
          ((fuzzy-match-in-string-list "file(s)" line-list)   
           (setf dos-n-files  (first line-list)
                 used-size  (third line-list))
           ;;   redundant   all-files-info (append all-files-info  (list :n-files dos-n-files )))
           ;;(break "files")
           )
          ;;for num dirs and free space line
          ;;bytes free line
          ;;              46 dir(s)  665,718,132,736 bytes fre
          ((and (fuzzy-match-in-string-list "dir(s)" line-list) 
                (fuzzy-match-in-string-list "free" line-list))     
           (setf dos-n-dirs (first line-list)
                 free-bytes (third line-list))
           ;;was, but caused redundant listing of this info
#|                 info-list
                 (list :n-dirs dos-n-dirs :n-files dos-n-files :size used-size :free free-bytes)
                 all-dirs-info (append all-dirs-info  info-list))|#
           ;;(break "free")
           )
          ;;dir lines
          ((fuzzy-match-in-string-list "<DIR>"  line-list)  ;; "<dir>" "dir" DIDN'T WORK
 ;; (fuzzy-match-in-string-list  "<DIR>" '("06/26/2019" "11:55" "PM" "<DIR>" "LISP" "PROJECTS" "TS"))
           ;;make (list :dir dirname date time am-pm)
           (multiple-value-bind (date time am-pm dir )
               (values-list line-list)
             ;;here
             (setf dirname (format-string-list  (nthcdr 4 line-list) :line-width 0))
             (unless (member dirname '( "" "." "..") :test 'string-equal)      
               (setf date-time (format nil "~a; ~a ~a" date time am-pm)
                     dir-infolist  (list :dir dirname date-time )
                     all-dirs-info (append all-dirs-info (list dir-infolist)))
               ;;(afout 'out (format nil "line= ~a line-list= ~a dir-infolist= ~a~% all-dirs-info= ~a" line line-list dir-infolist all-dirs-info))
               ;;end unless,mvb, find-
               )))
          
          ;;file lines
          ((or (find-best-match "am"  line-list)
               (find-best-match "pm" line-list))
           ;;make (list :file filename date time am-pm size)
           (multiple-value-bind (date time am-pm size) ;; filename)
               (values-list line-list)           
             (setf filename (format-string-list  (nthcdr 4 line-list) :line-width 0))
             (setf date-time (format nil "~a; ~a ~a" date time am-pm)
                   file-infolist (list :file filename date-time size)
                   all-files-info (append all-files-info (list file-infolist)))
             ;;end mvb, or
             ))
          (t nil))
         ;;end let, loop
         ))
      ;;end with-open
      )
    (unless return-all-lines-p
      (setf all-lines nil))
    ;;remove extra spaces
    (setf cur-dir (my-delete-first-spaces cur-dir)
          volname (my-delete-first-spaces volname))
             
    ;;(break "end make-dir-infolists-from-dos-dir")
    (values cur-dir host volname dos-n-dirs dos-n-files used-size free-bytes
            serial-num  all-dirs-info all-files-info  all-lines  all-line-lists)
    ;;end let, make-dir-infolists-from-dos-dir
    ))
;;TEST
;; HERE2
;;  ;; (FIND-BEST-MATCH "Serial' '("Directory" "of" "C:\\3-TS\\LISP" "PROJECTS" "TS\\MYUTILITIES"))
;; 
#|0 ("DIR" "\"C:\\3-TS\\LISP" "PROJECTS" "TS\\MYUTILITIES\\*.LISP\"")
1 ("Volume" "in" "drive" "C" "is" "OS")
2 ("Volume" "Serial" "Number" "is" "4058-766B")
3 NIL
4 ("Directory" "of" "C:\\3-TS\\LISP" "PROJECTS" "TS\\MYUTILITIES")
5 NIL
6 ("08/03/2018" "08:31" "PM" "339" "0-CODE-TEMPLATES.lisp")
7 ("05/08/2018" "06:48" "PM" "15903" "ARC-curve-function-tests.lisp")
ETC
63 ("08/06/2017" "05:22" "PM" "15563" "U-trees-flatlists.lisp")
64 ("07/23/2019" "05:54" "PM" "81732" "U-trees-old.lisp")
65 ("08/09/2019" "08:48" "PM" "93451" "U-trees.lisp")
66 ("04/16/2015" "05:18" "PM" "121474" "U-tstring-OLD-make-index-funs.lisp")
67 ("08/10/2019" "03:35" "PM" "255774" "U-tstring.lisp")
68 ("2" "File(s)" "4459657" "bytes")
69 ("ir(s)" "382527213568" "bytes" "free")
|#























;;FORMAT-DIRLISTS-FOR-CAPI
;;2020
;;ddd
(defun format-dirlists-for-capi (dir-dirlists &key begin-with-dirsym-p)
  "U-files RETURNS: keylists beginning with DIRNAME-SYM instead of :DIR"
  (let*
      ((new-dirlists)
       )
    (loop
     for dirlist in dir-dirlists
     do
     (let*
         ((new-dirlist (when (equal (car dirlist) :DIR) (cdr dirlist)))
          (dirname (second dirlist))
          (dirsym (my-make-symbol dirname))
          )
       (cond
        ((null dirsym) NIL)
        (begin-with-dirsym-p
         (setf new-dirlist (append (list dirsym) (cdr dirlist))
               new-dirlists (append new-dirlists (list new-dirlist))))
        (T (setf new-dirlist (cdr dirlist)
               new-dirlists (append new-dirlists (list new-dirlist)))))
       ;;end let,loop
       ))
    new-dirlists
  ;;end let,format-dirlists-for-capi
  ))
;;TEST
;; (format-dirlists-for-capi '((:DIR ".readyDLNA" "12/31/1969; 05:00 PM") (:DIR "0 LW-CUM-dated" "04/10/2020; 07:57 PM") (:DIR "1 Copy to MEDIA CENTER" "12/08/2019; 06:16 PM") (:DIR "Misc" "10/16/2019; 11:42 AM") (:DIR "Movies" "10/16/2019; 11:41 AM") (:DIR "Music" "10/16/2019; 01:59 PM") (:DIR "NIL" "11/25/2019; 03:12 PM") (:DIR "TV" "03/09/2020; 09:43 PM")) :begin-with-dirsym-p T)
;;works= ((READYDLNA ".readyDLNA" "12/31/1969; 05:00 PM") (0-LW-CUM-DATED "0 LW-CUM-dated" "04/10/2020; 07:57 PM") (1-COPY-TO-MEDIA-CENTER "1 Copy to MEDIA CENTER" "12/08/2019; 06:16 PM") (MISC "Misc" "10/16/2019; 11:42 AM") (MOVIES "Movies" "10/16/2019; 11:41 AM") (MUSIC "Music" "10/16/2019; 01:59 PM") (TV "TV" "03/09/2020; 09:43 PM"))
;; (format-dirlists-for-capi '((:DIR ".readyDLNA" "12/31/1969; 05:00 PM") (:DIR "0 LW-CUM-dated" "04/10/2020; 07:57 PM") (:DIR "1 Copy to MEDIA CENTER" "12/08/2019; 06:16 PM") (:DIR "Misc" "10/16/2019; 11:42 AM") (:DIR "Movies" "10/16/2019; 11:41 AM") (:DIR "Music" "10/16/2019; 01:59 PM") (:DIR "NIL" "11/25/2019; 03:12 PM") (:DIR "TV" "03/09/2020; 09:43 PM")))
;;works= ((".readyDLNA" "12/31/1969; 05:00 PM") ("0 LW-CUM-dated" "04/10/2020; 07:57 PM") ("1 Copy to MEDIA CENTER" "12/08/2019; 06:16 PM") ("Misc" "10/16/2019; 11:42 AM") ("Movies" "10/16/2019; 11:41 AM") ("Music" "10/16/2019; 01:59 PM") ("TV" "03/09/2020; 09:43 PM"))




;;LIST-DRIVE-INFO   [Use only if my-dos-dir doesn't work??]
;; NOTE: MY-DOS-DIR PROVIDES MUCH  MORE INFO (SIZE OF EACH SUBDIR, ETC.  --though this uses some of that info if use find-dos-info-p (which is also calls my-dos-dir)
;;
;;ddd
(defun list-drive-info (root &key (leveln 0) (last-leveln 3) (level-key :LEVEL) 
                             (find-dos-info-p  T) omit-dirs
                             (return-namestrings-p T)
                             incl-subdir-namelists-p (remove-final-slashes-p T)
                             list-hostless-subdirs-p
                             not-return-strings-p )
  "U-lists USE MY-DOS-INFO INSTEAD? RETURNS (values rootstr all-root-info all-pathnames  realdirpaths host-str).  IF NOT-RETURN-STRINGS-P, returns only the actual paths for the root.  When  INCL-SUBDIR-NAMELISTS-P, sets each list in subdir-info-lists to (path subdirnames), if nil = (path).  If  remove-final-slashes-p, removes final slashes for each subdirname. subdir-info-lists is included as part of every all-inclusive all-root-info list. If return-namestrings-p, returns namestrings instead of paths--takes additional processing. If FIND-DOS-INFO-P, also lists :used :free :name :serial-num :n-dirs :n-files"
  (let
      ((dir-list) 
       (dirs)
       (realdirpaths)
       (realfilepaths)
       (file-pathnames)
       (filenames)
       (dir-names)
       (dir-names1)
       (all-pathnames )
       (rootstr)
       (root-info)
       (all-root-info)
       ;;(all-root-info)
       (subdir-names)
       (subdir-info)
       (level-subdir-info)

       (sub-rootstr) 
       (sub-root-info)
       (level-subroot-info)
       ;;from mvs
       (host-str )
       (hostless-subdirs)
       )
    (incf leveln)

    (cond
     ((symbolp root)
      (setf rootstr (format nil "~A:\\" root)))
     ((pathnamep root)
      (setf rootstr (namestring root)))
     (t (setf rootstr root)))
    
    (when (null (member rootstr omit-dirs))

    ;;THIS LEVEL DIR LIST TO BE EXPLORED
    ;;(break "dir-list")

    
      ;;FIND NEXT LEVEL ROOT-INFO: 
      (multiple-value-setq (filenames file-pathnames dir-names realfilepaths  
                                      realdirpaths  host-str hostless-subdirs) 
          (list-directory-contents rootstr :incl-hostless-subdir-names-p list-hostless-subdirs-p))

      (when (and (<= leveln last-leveln) realdirpaths)

        ;;MAKE SUBDIR-INFO
        (loop
         for dirname in dir-names
         do
         (cond
          (incl-subdir-namelists-p
           (setf subdir-names (find-subdir-namestrings dirname
                                                       :remove-final-slashes-p remove-final-slashes-p)
                 subdir-info (list dirname subdir-names)
                 level-subdir-info (append level-subdir-info (list subdir-info)))
           ;;(break "in incl-subdir-namelists-p")
           )
          (T (setf  level-subdir-info (append level-subdir-info (list  (list dirname))))))
         ;;end loop
         )
        ;;APPEND LEVEL ROOT-INFO
        ;;list dir-names w/o host info
        (cond
         (list-hostless-subdirs-p
            (setf dir-names1 hostless-subdirs))
         (t (setf dir-names1 dir-names)))

        ;;(break "append dirnames1")
        (cond
         ((null find-dos-info-p)
          (setf  root-info (list level-key leveln rootstr 
                                 (append (list :DIRNAMES) dir-names1) ;;was dirnames)
                                 (append (list :FILENAMES) filenames)
                                 (append (list :SUBDIR-INFO) level-subdir-info) 
                                 :LAST-LEVELN last-leveln)))
         ;;if find-dos-info-p
         (t   ;;here
              (multiple-value-bind (cur-dir host volname serial-num n-dirs n-files used free)
                  (my-dos-dir rootstr)
                ;;(break "my-dos-dir")
                ;;dirlists filelists  dos-out-string dos-out)
                (setf  root-info (list level-key leveln rootstr :NAME drive-name
                                       :VOLNAME volname 
                                       :LOCATION "??"
                                       :TYPE "??" :BRAND "??"
                                       :SIZE "??"  :USED used :FREE free
                                       :SERIAL-NUM serial-num
                                       :N-DIRS n-dirs :N-FILES n-files
                                       :LAST-LEVELN last-leveln
                                       (append (list :DIRNAMES) dir-names1) ;;was dirnames)
                                       (append (list :FILENAMES) filenames)
                                       (append (list :SUBDIR-INFO) level-subdir-info) 
                                       ))
                ;;end mvb, t, cond
                )))

        ;;(break "after set root-info")

        ;;RECURSE IF DIRS?
        (when dir-names  ;;was dirnames     ;;was dirs
          (loop
           for newroot in dir-names ;;was dirnames   ;;was dirs
           do

           (when  (< leveln  last-leveln)
             (multiple-value-setq (sub-rootstr sub-root-info) ;;not needed all-pathnames dir-list)
                 (list-drive-info newroot :last-leveln last-leveln :level-key level-key
                                  :leveln leveln
                                  :omit-dirs omit-dirs
                                  :list-hostless-subdirs-p list-hostless-subdirs-p
                                  :incl-subdir-namelists-p incl-subdir-namelists-p))
             (when sub-root-info
               (setf level-subroot-info (append level-subroot-info (list sub-root-info))))

             ;;(when (null sub-root-info)(break "end loop"))
             ;;(break "end loop")
          ;end inner when, mvb, loop
             ))
          
          (cond
           (level-subroot-info
            (setf all-root-info (list root-info level-subroot-info)))
           (t (setf all-root-info (list root-info ))))
          ;;(break "end")
          ;;end  when dirs, when (and , when not on omit-dirs
          )))
    (values rootstr all-root-info all-pathnames  realdirpaths host-str) ;;was dirnames)  ;was dir-list)
    ;;end let, list-drive-info
    ))
;;TEST
;; (setf  *resx (nth-value 1 (list-drive-info 'G  :last-leveln 4 :incl-subdir-namelists-p T)))
;; (setf  *resx (nth-value 1 (list-drive-info 'G  :last-leveln 4 :incl-subdir-namelists-p T :list-hostless-subdirs-p T)))
;; works= pprinted= 

;;   (list-drive-info "C:/System Recovery/"  :last-leveln 1) = NIL (BEC SYSTEM PROTECTED?)
;; (progn (setf  *resx (nth-value 1 (list-drive-info 'E  :last-leveln 2 :incl-subdir-namelists-p T))) (PPRINT *RESX))
;;works= PPRINTED (SOME INNER CONTENTS OMITTED)
;;(progn (setf  *resx (nth-value 1 (list-drive-info 'E  :last-leveln 2 :incl-subdir-namelists-p T))) (PPRINT *RESX))
#|((:LEVEL
  1
  "E:\\"
  :NAME
  "SD01-128"
  (:DIRNAMES
   "E:\\System Volume Information\\"
   "E:\\0-Our Health Info\\"
   "E:\\5 Selected XPS Folder BUs\\"
   "E:\\2 LW Folders\\"
   "E:\\1 LW FOLDERS CUM\\")
  (:FILENAMES "OS (C) - Shortcut.lnk")
  (:SUBDIR-INFO
   ("E:\\System Volume Information\\" ("System Volume Information"))
   ("E:\\0-Our Health Info\\" ("0-Our Health Info"))
   ("E:\\5 Selected XPS Folder BUs\\" ("5 Selected XPS Folder BUs"))
   ("E:\\2 LW Folders\\" ("2 LW Folders"))
   ("E:\\1 LW FOLDERS CUM\\" ("1 LW FOLDERS CUM")))
  :SERIAL-NUM
  "3634-6637"
  :N-DIRS
  "4"
  :N-FILES
  "1"
  :USED
  "444"
  :FREE
  "122468433920"
  :LAST-LEVELN
  2)
 (((:LEVEL
    2
    "E:\\0-Our Health Info\\"
    :NAME
    "specified."
    (:DIRNAMES
     "E:\\0-Our Health Info\\Shlomo Raz MD UCLA\\"
     "E:\\0-Our Health Info\\Sherry Surgery\\"
     "E:\\0-Our Health Info\\Prescriptions & Info\\"
     "E:\\0-Our Health Info\\Our supplements\\"
     "E:\\0-Our Health Info\\Old our health records\\"
     "E:\\0-Our Health Info\\Andrea Natale MD records-info\\"
     "E:\\0-Our Health Info\\1-Lab Results\\")
    (:FILENAMES
     "Toms Post Ablation Log.docx"
     "Stevens 3 Med Cards.pdf"
     "Post Ablation Log.docx"
     "Our Suppliments Jan 2004.xls"
     "Our Supplements.wpd"
     "Our Supplements compared to experts.wpd"
     "Our Supplements 3.doc"
     "Our Supplements 2.wpd"
     "My Atrial Fib Summary.docx"
     "My Atrial Fib Log-Info.docx"
     "CPR certificate-Tom.pdf"
     "Backup of My Atrial Fib Summary.wbk"
     "Backup of My Atrial Fib Log-Info.wbk"
     "2016 LIFE EXPECTANCY TOM SHERRY 100.docx")
    (:SUBDIR-INFO
     ("E:\\0-Our Health Info\\Shlomo Raz MD UCLA\\"
      ("0-Our Health Info" "Shlomo Raz MD UCLA"))
     ("E:\\0-Our Health Info\\Sherry Surgery\\"
      ("0-Our Health Info" "Sherry Surgery"))
     ("E:\\0-Our Health Info\\Prescriptions & Info\\"
      ("0-Our Health Info" "Prescriptions & Info"))
     ("E:\\0-Our Health Info\\Our supplements\\"
      ("0-Our Health Info" "Our supplements"))
     ("E:\\0-Our Health Info\\Old our health records\\"
      ("0-Our Health Info" "Old our health records"))
     ("E:\\0-Our Health Info\\Andrea Natale MD records-info\\"
      ("0-Our Health Info" "Andrea Natale MD records-info"))
     ("E:\\0-Our Health Info\\1-Lab Results\\"
      ("0-Our Health Info" "1-Lab Results")))
    :SERIAL-NUM
    NIL
    :N-DIRS
    NIL
    :N-FILES
    NIL
    :USED
    NIL
    :FREE
    NIL
    :LAST-LEVELN
    2))
  ((:LEVEL
    2
    "E:\\5 Selected XPS Folder BUs\\"
    :NAME
    "specified."
    (:DIRNAMES
     "E:\\5 Selected XPS Folder BUs\\PWSf\\"
     "E:\\5 Selected XPS Folder BUs\\0 CURRENT DejaOffice-CompanionLink BU\\")
    (:FILENAMES)
    (:SUBDIR-INFO
     ("E:\\5 Selected XPS Folder BUs\\PWSf\\"
      ("5 Selected XPS Folder BUs" "PWSf"))
     ("E:\\5 Selected XPS Folder BUs\\0 CURRENT DejaOffice-CompanionLink BU\\"
      ("5 Selected XPS Folder BUs" "0 CURRENT DejaOffice-CompanionLink BU")))
    :SERIAL-NUM
    NIL
    :N-DIRS
    NIL
    :N-FILES
    NIL
    :USED
    NIL
    :FREE
    NIL
    :LAST-LEVELN
    2))
  ((:LEVEL
    2
    "E:\\2 LW Folders\\"
    :NAME
    "specified."
    (:DIRNAMES
     "E:\\2 LW Folders\\Toms-HDrive-Explorer\\"
     "E:\\2 LW Folders\\MyUtilities\\"
     "E:\\2 LW Folders\\CogSys-Model\\"
     "E:\\2 LW Folders\\MyUtilities 3\\"
     "E:\\2 LW Folders\\MyUtilities 2\\"
     "E:\\2 LW Folders\\CogSys-Model 2\\"
     "E:\\2 LW Folders\\ART3-output-data\\"
     "E:\\2 LW Folders\\ART3-BU\\"
     "E:\\2 LW Folders\\ART3\\"
     "E:\\2 LW Folders\\ART2-Output Data\\"
     "E:\\2 LW Folders\\AndyCares\\"
     "E:\\2 LW Folders\\AI-NLP\\"
     "E:\\2 LW Folders\\z-LISPWORKS files\\"
     "E:\\2 LW Folders\\video-playback-utility\\"
     "E:\\2 LW Folders\\VideoPlayback\\"
     "E:\\2 LW Folders\\SHAQxNotUsed\\"
     "E:\\2 LW Folders\\SHAQxJava2000\\"
     "E:\\2 LW Folders\\SHAQ-Web signed-exe php\\"
     "E:\\2 LW Folders\\SHAQ-ResultsTEXTetc\\"
     "E:\\2 LW Folders\\SHAQ-PLUS\\"
     "E:\\2 LW Folders\\SHAQ-Copy2015-01-13\\"
     "E:\\2 LW Folders\\SHAQ-Copy2014-12-05\\"
     "E:\\2 LW Folders\\SHAQ - Copy2015-09-08\\"
     "E:\\2 LW Folders\\SHAQ - Copy2015-03-30\\"
     "E:\\2 LW Folders\\SHAQ\\"
     "E:\\2 LW Folders\\SecuritySIGNING Info-Issues\\"
     "E:\\2 LW Folders\\screensaver2013\\"
     "E:\\2 LW Folders\\screensaver\\"
     "E:\\2 LW Folders\\PCDB\\"
     "E:\\2 LW Folders\\Natural Language Code etc\\"
     "E:\\2 LW Folders\\MyUtilities 1\\"
     "E:\\2 LW Folders\\MyProjects2\\"
     "E:\\2 LW Folders\\MyLispWebs\\"
     "E:\\2 LW Folders\\MyInterfaces\\"
     "E:\\2 LW Folders\\MyConfigFiles\\"
     "E:\\2 LW Folders\\Math\\"
     "E:\\2 LW Folders\\LwStart\\"
     "E:\\2 LW Folders\\LW6-examples\\"
     "E:\\2 LW Folders\\H-HELP\\"
     "E:\\2 LW Folders\\H-CapiExamples\\"
     "E:\\2 LW Folders\\H-Capi\\"
     "E:\\2 LW Folders\\CogSysOutputs\\"
     "E:\\2 LW Folders\\CogSys-Model 1\\"
     "E:\\2 LW Folders\\CL-Utilities\\"
     "E:\\2 LW Folders\\ART-Utilities\\"
     "E:\\2 LW Folders\\ART-preNestSymBU\\"
     "E:\\2 LW Folders\\ARTMAP-JavaVersion\\"
     "E:\\2 LW Folders\\ART-LW\\"
     "E:\\2 LW Folders\\MyLWex-Utilities\\"
     "E:\\2 LW Folders\\LW7-EXAMPLES\\")
    (:FILENAMES
     "test-spss-output1.txt"
     "test-spss-output0.txt"
     "test-spss-output.txt~"
     "test-object.lisp"
     "test-functions2.lisp~"
     "test-functions2.lisp"
     "test-combine.lisp"
     "test-buffer.lisp~"
     "test-buffer.lisp"
     "spss-test-datafile.sav"
     "SHAQ-Deliver LOG.lisp"
     "shaq-data-list-OUTOMCESonly.lisp"
     "shaq-data-list-NoAcadCAR.lisp"
     "shaq-data-list-for-spss.lisp~"
     "shaq-data-list-for-spss.lisp"
     "shaq-data-list-eq2.lisp~"
     "shaq-data-list-eq2.lisp"
     "shaq-data-list-eg1.lisp~"
     "shaq-data-list-eg1.lisp"
     "shaq-data-list-anger.lisp~"
     "shaq-data-list-anger.lisp"
     "shaq-data-list-all.lisp~"
     "shaq-data-list-all.lisp"
     "myfile.txt"
     "My SHAQ ResultsAll-pt2.txt"
     "My SHAQ ResultsAll-pt1.txt"
     "LISPWORKS-LAST 6.1.LISP"
     "LISPWORKS-7-0 BU.lisp"
     "fout-out-20cycles.lisp~"
     "fout-out-20cycles.lisp"
     "Current LISPWORKS 7.0 copy.lisp"
     "CSQ-PC-Frames.lisp"
     "CogTest-Deliver LOG.lisp"
     "ART2-multipane-interface-SCROLL.lisp"
     "1-LISP 7 NOTES.lisp~"
     "1-LISP 7 NOTES.lisp"
     ".LISPWORKS~"
     ".LISPWORKS"
     "tags"
     "selection-key-binds.lisp"
     "PreviousCopy.LISPWORKS"
     "PreviousBU.LISPWORKS"
     "permutation-examples.lisp~"
     "permutation-examples.lisp"
     "permutation.lisp~"
     "permutation.lisp"
     "package.lisp~"
     "package.lisp"
     "1-lisp-test.txt")
    (:SUBDIR-INFO
     ("E:\\2 LW Folders\\Toms-HDrive-Explorer\\"
      ("2 LW Folders" "Toms-HDrive-Explorer"))
     ("E:\\2 LW Folders\\MyUtilities\\" ("2 LW Folders" "MyUtilities"))
     ("E:\\2 LW Folders\\CogSys-Model\\" ("2 LW Folders" "CogSys-Model"))
     ("E:\\2 LW Folders\\MyUtilities 3\\" ("2 LW Folders" "MyUtilities 3"))
     ("E:\\2 LW Folders\\MyUtilities 2\\" ("2 LW Folders" "MyUtilities 2"))
     ("E:\\2 LW Folders\\CogSys-Model 2\\" ("2 LW Folders" "CogSys-Model 2"))
     ("E:\\2 LW Folders\\ART3-output-data\\"
      ("2 LW Folders" "ART3-output-data"))
     ("E:\\2 LW Folders\\ART3-BU\\" ("2 LW Folders" "ART3-BU"))
     ("E:\\2 LW Folders\\ART3\\" ("2 LW Folders" "ART3"))
     ("E:\\2 LW Folders\\ART2-Output Data\\"
      ("2 LW Folders" "ART2-Output Data"))
     ("E:\\2 LW Folders\\AndyCares\\" ("2 LW Folders" "AndyCares"))
     ("E:\\2 LW Folders\\AI-NLP\\" ("2 LW Folders" "AI-NLP"))
     ("E:\\2 LW Folders\\z-LISPWORKS files\\"
      ("2 LW Folders" "z-LISPWORKS files"))
     ("E:\\2 LW Folders\\video-playback-utility\\"
      ("2 LW Folders" "video-playback-utility"))
     ("E:\\2 LW Folders\\VideoPlayback\\" ("2 LW Folders" "VideoPlayback"))
     ("E:\\2 LW Folders\\SHAQxNotUsed\\" ("2 LW Folders" "SHAQxNotUsed"))
     ("E:\\2 LW Folders\\SHAQxJava2000\\" ("2 LW Folders" "SHAQxJava2000"))
     ("E:\\2 LW Folders\\SHAQ-Web signed-exe php\\"
      ("2 LW Folders" "SHAQ-Web signed-exe php"))
     ("E:\\2 LW Folders\\SHAQ-ResultsTEXTetc\\"
      ("2 LW Folders" "SHAQ-ResultsTEXTetc"))
     ("E:\\2 LW Folders\\SHAQ-PLUS\\" ("2 LW Folders" "SHAQ-PLUS"))
     ("E:\\2 LW Folders\\SHAQ-Copy2015-01-13\\"
      ("2 LW Folders" "SHAQ-Copy2015-01-13"))
     ("E:\\2 LW Folders\\SHAQ-Copy2014-12-05\\"
      ("2 LW Folders" "SHAQ-Copy2014-12-05"))
     ("E:\\2 LW Folders\\SHAQ - Copy2015-09-08\\"
      ("2 LW Folders" "SHAQ - Copy2015-09-08"))
     ("E:\\2 LW Folders\\SHAQ - Copy2015-03-30\\"
      ("2 LW Folders" "SHAQ - Copy2015-03-30"))
     ("E:\\2 LW Folders\\SHAQ\\" ("2 LW Folders" "SHAQ"))
     ("E:\\2 LW Folders\\SecuritySIGNING Info-Issues\\"
      ("2 LW Folders" "SecuritySIGNING Info-Issues"))
     ("E:\\2 LW Folders\\screensaver2013\\"
      ("2 LW Folders" "screensaver2013"))
     ("E:\\2 LW Folders\\screensaver\\" ("2 LW Folders" "screensaver"))
     ("E:\\2 LW Folders\\PCDB\\" ("2 LW Folders" "PCDB"))
     ("E:\\2 LW Folders\\Natural Language Code etc\\"
      ("2 LW Folders" "Natural Language Code etc"))
     ("E:\\2 LW Folders\\MyUtilities 1\\" ("2 LW Folders" "MyUtilities 1"))
     ("E:\\2 LW Folders\\MyProjects2\\" ("2 LW Folders" "MyProjects2"))
     ("E:\\2 LW Folders\\MyLispWebs\\" ("2 LW Folders" "MyLispWebs"))
     ("E:\\2 LW Folders\\MyInterfaces\\" ("2 LW Folders" "MyInterfaces"))
     ("E:\\2 LW Folders\\MyConfigFiles\\" ("2 LW Folders" "MyConfigFiles"))
     ("E:\\2 LW Folders\\Math\\" ("2 LW Folders" "Math"))
     ("E:\\2 LW Folders\\LwStart\\" ("2 LW Folders" "LwStart"))
     ("E:\\2 LW Folders\\LW6-examples\\" ("2 LW Folders" "LW6-examples"))
     ("E:\\2 LW Folders\\H-HELP\\" ("2 LW Folders" "H-HELP"))
     ("E:\\2 LW Folders\\H-CapiExamples\\" ("2 LW Folders" "H-CapiExamples"))
     ("E:\\2 LW Folders\\H-Capi\\" ("2 LW Folders" "H-Capi"))
     ("E:\\2 LW Folders\\CogSysOutputs\\" ("2 LW Folders" "CogSysOutputs"))
     ("E:\\2 LW Folders\\CogSys-Model 1\\" ("2 LW Folders" "CogSys-Model 1"))
     ("E:\\2 LW Folders\\CL-Utilities\\" ("2 LW Folders" "CL-Utilities"))
     ("E:\\2 LW Folders\\ART-Utilities\\" ("2 LW Folders" "ART-Utilities"))
     ("E:\\2 LW Folders\\ART-preNestSymBU\\"
      ("2 LW Folders" "ART-preNestSymBU"))
     ("E:\\2 LW Folders\\ARTMAP-JavaVersion\\"
      ("2 LW Folders" "ARTMAP-JavaVersion"))
     ("E:\\2 LW Folders\\ART-LW\\" ("2 LW Folders" "ART-LW"))
     ("E:\\2 LW Folders\\MyLWex-Utilities\\"
      ("2 LW Folders" "MyLWex-Utilities"))
     ("E:\\2 LW Folders\\LW7-EXAMPLES\\" ("2 LW Folders" "LW7-EXAMPLES")))
    :SERIAL-NUM
    NIL
    :N-DIRS
    NIL
    :N-FILES
    NIL
    :USED
    NIL
    :FREE
    NIL
    :LAST-LEVELN
    2))
  ((:LEVEL
    2
    "E:\\1 LW FOLDERS CUM\\"
    :NAME
    "specified."
    (:DIRNAMES
     "E:\\1 LW FOLDERS CUM\\H-Capi\\"
     "E:\\1 LW FOLDERS CUM\\Toms-HDrive-Explorer\\"
     "E:\\1 LW FOLDERS CUM\\CL-Utilities\\"
     "E:\\1 LW FOLDERS CUM\\ART-Utilities\\"
     "E:\\1 LW FOLDERS CUM\\ARTMAP-JavaVersion\\"
     "E:\\1 LW FOLDERS CUM\\ART-LW\\"
     "E:\\1 LW FOLDERS CUM\\ART3-output-data\\"
     "E:\\1 LW FOLDERS CUM\\ART3\\"
     "E:\\1 LW FOLDERS CUM\\ART2-Output Data\\"
     "E:\\1 LW FOLDERS CUM\\MyUtilities\\"
     "E:\\1 LW FOLDERS CUM\\CogSysOutputs\\"
     "E:\\1 LW FOLDERS CUM\\CogSys-Model\\")
    (:FILENAMES)
    (:SUBDIR-INFO
     ("E:\\1 LW FOLDERS CUM\\H-Capi\\" ("1 LW FOLDERS CUM" "H-Capi"))
     ("E:\\1 LW FOLDERS CUM\\Toms-HDrive-Explorer\\"
      ("1 LW FOLDERS CUM" "Toms-HDrive-Explorer"))
     ("E:\\1 LW FOLDERS CUM\\CL-Utilities\\"
      ("1 LW FOLDERS CUM" "CL-Utilities"))
     ("E:\\1 LW FOLDERS CUM\\ART-Utilities\\"
      ("1 LW FOLDERS CUM" "ART-Utilities"))
     ("E:\\1 LW FOLDERS CUM\\ARTMAP-JavaVersion\\"
      ("1 LW FOLDERS CUM" "ARTMAP-JavaVersion"))
     ("E:\\1 LW FOLDERS CUM\\ART-LW\\" ("1 LW FOLDERS CUM" "ART-LW"))
     ("E:\\1 LW FOLDERS CUM\\ART3-output-data\\"
      ("1 LW FOLDERS CUM" "ART3-output-data"))
     ("E:\\1 LW FOLDERS CUM\\ART3\\" ("1 LW FOLDERS CUM" "ART3"))
     ("E:\\1 LW FOLDERS CUM\\ART2-Output Data\\"
      ("1 LW FOLDERS CUM" "ART2-Output Data"))
     ("E:\\1 LW FOLDERS CUM\\MyUtilities\\"
      ("1 LW FOLDERS CUM" "MyUtilities"))
     ("E:\\1 LW FOLDERS CUM\\CogSysOutputs\\"
      ("1 LW FOLDERS CUM" "CogSysOutputs"))
     ("E:\\1 LW FOLDERS CUM\\CogSys-Model\\"
      ("1 LW FOLDERS CUM" "CogSys-Model")))
    :SERIAL-NUM
    NIL
    :N-DIRS
    NIL
    :N-FILES
    NIL
    :USED
    NIL
    :FREE
    NIL
    :LAST-LEVELN
    2))))|#






;;FIND-DIRNAMES-FROM-PATHS
;;
;;ddd
(defun find-dirnames-from-paths (pathlist  &key (pathlist-is-root-p T)) 
  "In U-files For a list of paths RETURNS (values all-pathnames  dirname-list filename-list) . If PATHLIST-IS-ROOT-P, then finds pathlist from the pathlist= root. USE MORE USEFUL FUNCTION LIST-DRIVE-INFO?"
  (let
      ((pathname)
       (pathname-list)
       (filename)
       (dirname)
       (filename-list)
       (dirname-list)
       (all-pathnames)
       )
    (when pathlist-is-root-p
      (setf pathlist (list-directory pathlist)))
    (loop
     for path in pathlist
     do
     (when (pathnamep path)
       (setf pathname (namestring path))
       (when pathname 
         (setf all-pathnames (append all-pathnames (list pathname))))
       
       (setf filename (file-namestring path))
         (cond
          (filename
           (setf filename-list (append filename-list (list filename))))
          ((setf dirname (directory-namestring path)
               dirname-list (append dirname-list (list pathname))))
          (t nil))         
     ;;end when, loop
    ))
    (values all-pathnames  dirname-list filename-list)
    ;;end let, find-names-from-paths
    ))
;;TEST
;; (find-dirnames-from-paths "G:\\" :pathlist-is-root-p T)
;; works= 
#|("F:\\Video-watch\\" "F:\\TEST-FILE.txt" "F:\\System Volume Information\\" "F:\\My Software-Manuals\\" "F:\\3 Our MUSIC-m4a [master]\\" "F:\\2 To MEDIA CENTER\\" "F:\\2 MAIN BU\\" "F:\\1 PHONE BUs\\" "F:\\1 Move to MEDIA CENTER\\" "F:\\0 Main OUR PHOTOS-VIDEOS\\" "F:\\0 LW-CUM-dated\\" "F:\\$RECYCLE.BIN\\")
("F:\\Video-watch\\" "F:\\System Volume Information\\" "F:\\My Software-Manuals\\" "F:\\3 Our MUSIC-m4a [master]\\" "F:\\2 To MEDIA CENTER\\" "F:\\2 MAIN BU\\" "F:\\1 PHONE BUs\\" "F:\\1 Move to MEDIA CENTER\\" "F:\\0 Main OUR PHOTOS-VIDEOS\\" "F:\\0 LW-CUM-dated\\" "F:\\$RECYCLE.BIN\\")
("TEST-FILE.txt")|#

#|
;;mistakingly modified older with following.  USE OLDER VERSION??
(defun find-dirnames-from-paths (pathlist  &key incl-subdir-namelists-p
                                           (pathlist-is-root-p T) (remove-final-slashes-p T)) 
  "In U-files For a list of paths RETURNS (values all-pathnames  dirnames-list filename-list) . If PATHLIST-IS-ROOT-P, then finds pathlist from the pathlist= root. If incl-subdir-namelists-p, each dirnames-list  in dirnames-list = (path subdir-names)  otherwise  = (path) only."
  (let
      ((pathname)
       (pathname-list '(:dirpaths))
       (filename)
       (dirname)
       (filename-list '(:FILENAMES))
       (dirname-list)
       (dirnames-list '(:PATH-SUBDIRNAMES))
       (all-pathnames)
       (subdir-names)
       )
    (when pathlist-is-root-p
      (setf pathlist (list-directory pathlist)))
    (loop
     for path in pathlist
     do
     (when (pathnamep path)
       (setf pathname (namestring path)
             dirname-list (list pathname))

       (when pathname 
         (setf all-pathnames (append all-pathnames (list pathname)))

         (when incl-subdir-namelists-p
           (setf subdir-names (find-subdir-namestrings  pathname
                                                        :remove-final-slashes-p remove-final-slashes-p)
                 dirname-list (list pathname subdir-names)
                 dirnames-list (append dirnames-list  (list dirname-list))))
         ;;end outer when pathname
         )
       
       (setf filename (file-namestring path))

       (when filename
           (setf filename-list (append filename-list (list filename))))

         #|was, wrong? (cond
          (filename
           (setf filename-list (append filename-list (list filename))))
          ((setf dirname (directory-namestring path)
               dirnames-list (append dirnames-list (list pathname))))
          (t nil))|#    
      
     ;;end when, loop
    ))
    (values all-pathnames  dirnames-list filename-list)
    ;;end let, find-dirnames-from-paths
    ))|#







;;FIND-SUBDIR-NAMESTRINGS
;;modified for / replace \\ 2017
;; ALSO USE: (pathname-directory "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-files-backup.lisp" )  => (:ABSOLUTE "3-TS" "LISP PROJECTS TS" "MyUtilities")
;; (pathname-directory "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\") =>
;;       (:ABSOLUTE "3-TS" "LISP PROJECTS TS" "MyUtilities")
;;ddd
(defun find-subdir-namestrings (path &key (leveln 1) (remove-final-slashes-p  T)
                                     return-pathname-path-p)
  "In U-files.  Finds the subdir string-names in a multi-lovel path. leveln specifies the level with 1= first level after drive root. Path can be path or path namestring RETURNS (values subdirs target-subdir pathname-path). PATH must be a real pathname or a pathname string."
  (let
      ((subdirs)
       (subdir-str)
       (target-subdir)
       (whole-dir)
       (last-path-char (subseq path (- (length path) 1)))
       )
    (cond
     ((pathnamep path)
      (setf subdirs (cdr (pathname-directory path)))
      ;;(break "a")
      )
     ((or (string-equal last-path-char "\\")(string-equal last-path-char "/"))
      (setf subdir-str (directory-namestring path))
      (when remove-final-slashes-p
        (setf subdir-str (subseq subdir-str 0 (- (length subdir-str) 1))))
      (setf subdirs  (nth-value 2 (divide-string-to-tokens  subdir-str  '()
                     :word-delim-list '( #\\) :no-initial-spaces T  :ignore-char-list nil))))
     ((stringp path)
      (setf subdirs  (nth-value 2 (divide-string-to-tokens  path  '()  :word-delim-list '( #\\) :no-initial-spaces T  :ignore-char-list nil))))
      (t nil))   
      (setf target-subdir (nth (- leveln 1)  subdirs)
            whole-dir (make-whole-dir-from-subdirs subdirs))
      (values subdirs target-subdir)
    ;;end let, find-subdir-namestrings
    ))
;;TEST
;;  (find-subdir-namestrings  "thisdir")
;; works= ("thisdir")  "thisdir"
;; multi-level w/ last slashes
;; (find-subdir-namestrings  "thisdir\\that\\other\\last\\")
;; works= ("thisdir" "that" "other" "last")    "thisdir"
;; multi-level  no last slashes
;; (find-subdir-namestrings  "thisdir\\that\\other\\last")
;; works= ("thisdir" "that" "other" "last")    "thisdir"

;;  (find-subdir-namestrings "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp" :leveln 1)
;; works= ("3-TS" "LISP PROJECTS TS" "MyUtilities")   "3-TS"
;;
;;  (setf xxp(pathname  "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"))
;;  (find-subdir-namestrings xxp :leveln 1)
;; works= ("3-TS" "LISP PROJECTS TS" "MyUtilities")   "3-TS"

;; based upon
;; (divide-string-to-tokens "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"  '()  :word-delim-list '( #\\) :no-initial-spaces T  :ignore-char-list nil)
;; works= "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"   ""   ("C:" "3-TS" "LISP PROJECTS TS" "MyUtilities" "U-lists.lisp")
;; (divide-string-to-tokens "\\3-TS\\LISP PROJECTS TS\\MyUtilities\\"  '()  :word-delim-list '( #\\) :no-initial-spaces T  :ignore-char-list '(#\\))  



;;MAKE-FULL-PATHS
;;ddd
(defun make-full-paths (root directories)
  "In U-files, takes mixed list of full pathnames or only dirs directly subord to root and returns a list of all full pathnames"
  (let 
      ((newpath)
       (newpaths)
       )
    (loop
     for item in directories
     do     
     (setf newpaths (append newpaths (list newpath)))
     ;;end loop
     )
    newpaths
    ;;end let, make-full-paths
    ))




;;MAKE-PATH-FROM-SUBDIRS
;;
;;ddd
(defun make-path-from-subdirs (&key root dir-path subdirs filename add-filename-p
                                    also-return-real-path-p add-end-slash-p
                                    (slashstr "/") keep-dir-path-root-p)
  "In U-files, takes either a full directory pathname (dir-path) or a list of subdir strings (subdirs) and RETURNS (values  newpath real-path subdirs filename). root can be nil. If filename, adds it to end. SUBDIRS is a list of subdir strings. Adds root and/or filename if present. If add-filename-p, keeps filename in dir-path and/or adds filename."
  (let*
      ((newdir "") ;; (car (find-subdir-namestrings directory)))
       (newpath)
       (newpaths)
       (real-path)
       (dir-path-root-sym (cond ((null dir-path) "") (t (host-namestring dir-path))))
       (dir-path-root (when (not (string-equal dir-path-root-sym ""))
                        (format nil "~A:" dir-path-root-sym)))
       (path-filename (cond ((null dir-path) NIL) (t (file-namestring dir-path))))
       )
    (unless (or filename (null path-filename))
      (setf filename path-filename))
    (cond
     ;;If root=nil, check for root in dir-path
     ((and (null root) (not (string-equal dir-path-root  "")))
      (setf root dir-path-root))
     ((null root)
      (setf root ""))
     ((and (stringp root) (= (length root) 1))
      (setf root (format nil "~A:" root)))
     ((symbolp root)
      (setf root (format nil "~A:" root)))
     ((null root)
      (setf root ""))
     (t nil))

    (when (and (null subdirs)
               dir-path)
      (setf subdirs (cdr (pathname-directory dir-path)))
    )

    (when subdirs
      (loop
       for subdir in subdirs
       do
       (setf newdir (format nil "~A~A~A" newdir  slashstr subdir))
       ;;end loop
       )
      (setf dir-path newdir)
      ;;end when
      )
    ;;ADD ROOT?
    (cond
     ((and keep-dir-path-root-p dir-path-root)
      (setf newpath (format nil "~A~A" dir-path-root dir-path )))
     (root
      (setf newpath (format nil "~A~A" root dir-path )))
     (t (setf newpath dir-path)))
   ;; (break "newpath")

    (when filename
      (setf newpath (format nil "~A~A~A" newpath slashstr filename)))

    (when also-return-real-path-p
      (setf real-path (pathname newpath)))

    (when (or add-end-slash-p add-filename-p)
      (setf newpath (format nil "~A~A" newpath slashstr)))

    (when add-filename-p
      (when (and (null filename) path-filename)
        (setf filename path-filename))
      (when filename
      (setf newpath (format nil "~A~A" newpath slashstr filename))))

      (values  newpath real-path subdirs filename)
      ;;end let,make-path-from-subdirs
      ))
;;TEST
;; (make-path-from-subdirs  :dir-path "\\dir1\\dir2\\dir3\\filename.txt")
;;  works=  "//dir1/dir2/dir3"  NIL  ("dir1" "dir2" "dir3") NIL
;; (make-path-from-subdirs  :dir-path "\\dir1\\dir2\\dir3\\filename.txt" :add-filename-p T)
;;works= "/dir1/dir2/dir3/filename.txt//"  NIL  ("dir1" "dir2" "dir3")  "filename.txt"
;; (make-path-from-subdirs  :dir-path "\\dir1\\dir2\\dir3\\filename.txt" :root "E:" :also-return-real-path-p T)
;; works= "E://dir1/dir2/dir3"     #P"E:/dir1/dir2/dir3"   ("dir1" "dir2" "dir3") "filename.txt"
;; (make-path-from-subdirs  :dir-path "c:\\dir1\\dir2\\dir3\\filename.txt" :keep-dir-path-root-p T)
;; works= "c:/dir1/dir2/dir3"  NIL   ("dir1" "dir2" "dir3")  "filename.txt"
;;  (make-path-from-subdirs :root "c" :dir-path "\\thisdir\\" :add-end-slash-p T) 
;; works= "c:/thisdir/"   NIL  ("thisdir")   NIL    
;;  no root
;; (make-path-from-subdirs  :dir-path "\\thisdir\\") = "/thisdir"  NIL   ("thisdir")  NIL
;;  filename
;; (make-path-from-subdirs :root "c" :dir-path "\\thisdir\\" :filename "thisfile.lisp")
;; works= "c:/thisdir/thisfile.lisp"   NIL  ("thisdir")   "thisfile.lisp"
;; multi-subdirs plus :also-return-real-path-p T
;;  (make-path-from-subdirs :root "c" :dir-path "\\thisdir\\that\\other\\" :also-return-real-path-p T) = "c:\\thisdir\\that\\other\\"   #P"c:/thisdir/that/other/"
;;  SUBDIRS 
;;  (make-path-from-subdirs :root "c" :subdirs  '("thisdir"  "that" "other" "last"))
;; works=  "c:/thisdir/that/other/last"  NIL  ("thisdir" "that" "other" "last")  NIL
;; same w no root
;; (make-path-from-subdirs  :subdirs  '("thisdir"  "that" "other" "last"))  = "\\thisdir\\that\\other\\last\\"  NIL
;; a pathname w/ all but last subdir or filename
;;


;; (host-namestring "c:\\this\\help.lisp") = "c" ;; If no host, then returns = ""

;;COMPONENT-PRESENT-P
;;
;;ddd
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))


;;DIRECTORY-PATHNAME-P
;;
;;ddd
(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))
;;(directory-pathname-p "c:\\temp\\") 
;;(directory-pathname-p "c:\\temp\\test-photo1.jpg") => nil
;;(directory-pathname-p "c:\\temp") = nil
;;(directory-pathname-p "c:\\temp\\*.*") = nil

  


;;PATHNAME-AS-DIRECTORY
;;
;;ddd
(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
      pathname)))
;;TEST
;;(pathname-as-directory "C:\\3-TS\\LISP PROJECTS\\MyProjects\\1 TEST OUTPUT.docx") => converts it to a real FILE path
;;(pathname-as-directory "C:\\3-TS\\LISP PROJECTS\\MyProjects") 
;;   => CONVERTS STRING TO A REAL DIRECTORY PATH
#|
(defun test-pad ()
  (let ((pad (pathname-as-directory "C:\\TOM\LISP PROJECTS\\MyProjects"))
        )
    (show-text (format nil "pathname-as-directory= ~A~%" pad) 30 t)
    ))
|#



;;----------------------------- From Peter Seibel PCL Ch-15 ----------------------------
;;functions not duplicated or modified above

;;DIRECTORY-WILDCARD
;;ppp
;;ddd
(defun directory-wildcard (dirname)
  (make-pathname 
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))
;;TEST
;;  (directory-wildcard "C:\\TEMP\\") =  #P"C:/TEMP/*.*"



;;FILE-PATHNAME-P
;; modified 2019
;;ddd
(defun file-pathname-p (path &key  return-details-p)
  "In U-files.lisp, Returns a file path string name from either real path or string name.  If RETURN-DETAILS-P, RETURNS If return-details-p= (values result filename filetype dir host) if filename at end. Note: If filename or filetype= *, returns :WILD."
  (let*
       ((pathstr (namestring path))
        (file-period-n (search "." pathstr))
        (result (when file-period-n pathstr))
        )
    (cond
     (return-details-p
      (multiple-value-bind (dir filename filetype host)
          (get-file-dir-name-type pathstr)
        (values result filename filetype dir host)))
     (t result))
  ;;end mvb, let, file-pathname-p
  ))
;;TEST
;;  (file-pathname-p"F:\\4-TS\\2 TEST-DIR2") = NIL
;;  (file-pathname-p"F:\\4-TS\\2 TEST-DIR2" :return-details-p T)
;;  (file-pathname-p "C:\\3-TS\\LISP PROJECTS\\MyProjects\\1 TEST OUTPUT.docx")
;; works=  "1 TEST OUTPUT"  "docx"  "\\3-TS\\LISP PROJECTS\\MyProjects\\" 
;;(file-pathname-p "c:\\temp\\") = NIL
;;(file-pathname-p "c:\\temp\\test-photo1.jpg") => "test-photo1"  "jpg"  "\\temp\\"
;;(file-pathname-p "c:\\temp") = NIL
;;(file-pathname-p "c:\\temp\\*.*") :WILD :WILD "\\temp\\"


;;note: (directory-pathname-p "F:\\4-TS\\2 TEST-DIR2") = NIL
;; (namestring "c:\\this\\that") = "c:\\this\\that"
;; (namestring "c:\\this\\that\\") = "c:\\this\\that\\"
;; (namestring "c:\\this\\that\\filename.txt") = "c:\\this\\that\\filename.txt"


;;CONVERTS A STRING INTO A REAL FILE PATH
;;ddd
(defun pathname-as-file (name)
  "In U-files.lisp, Return a pathname reperesenting the given pathname in `file form',
i.e. with the name elements in the name and type component. Can't
convert wild pathnames because of problems mapping wild directory
component into name and type components. Returns its argument if
it is already in file form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname 
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))
;;TEST
;; ;;(pathname-as-file "C:\\3-TS\\LISP PROJECTS\\MyProjects\\1 TEST OUTPUT.docx")




;;FILE-EXISTS-P
;;modified 2017, 2019
;;ddd
(defun file-exists-p (pathname)
  "RETURNS (values file-path filename-st). Similar to CL:PROBE-FILE except it always returns directory names in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory.  NOTE: probe-file is not reliable now, so wrote work-a-round."
  (let*
      ((filename-str)
       (filepath)
       (filetype (pathname-type pathname))
      )
    ;;make sure there is a filetype ext.
    (when filetype
       (setf filepath (pathname pathname)
         filename-str (file-namestring pathname)))
#|   was, but probe-file didn't always work   (setf filepath (probe-file pathname)
            filename-str (file-namestring pathname)))|#
    (values filepath filename-str)
    ;;end let, file-exists-p
    ))
;;TEST
;;  (file-exists-p "c:\\temp\\this.lisp") = #P"c:/temp/this.lisp" "this.lisp"
;;  (file-exists-p "c:\\3-TS\\MyDrives\\saved\\FO5-64.lisp") = 
;; works= #P"c:/3-TS/MyDrives/saved/FO5-64.lisp"  "FO5-64.lisp"
;;  (file-exists-p "c:\\temp\\") = NIL NIL
;;  (file-exists-p "c:\\temp") = NIL NIL



;;THESE SHOULD PRODUCE POSITIVE RESULTS ??
;;  (probe-file "c:\\3-TS\\MyDrives\\saved\\FO5-64.lisp") = NIL
;; (let ((path1 (pathname "c:\\3-TS\\MyDrives\\saved\\FO5-64.lisp"))) (probe-file  path1)) = NIL
;; for "C:\\temp1\\test-file.txt"
;; (probe-file "C:\\temp1\\test-file.txt") = #P"C:/temp1/test-file.txt"
;;  (pathname-type #P"C:/temp1/test-file.txt") = "txt"
;; (pathname-type "C:\\temp1\\test-file") = NIL




#|weird problem below in parens
(defun file-exists-p (pathname)
  "Returns (values file-path filename-st). Similar to CL:PROBE-FILE except it always returns directory names in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."

  #+(or sbcl lispworks openmcl)
  ;; These implementations do "The Right Thing" as far as we are
  ;; concerned. They return a truename of the file or directory if it
  ;; exists and the truename of a directory is in directory normal
  ;; form.
  (let
      ((filename-str)
       (filepath)
       )
  (cond
   ((setf filepath (probe-file pathname))
    (setf filename-str (file-namestring pathname)))
   (t (setf filepath nil)))
  ((

  #+(or allegro cmu)
  ;; These implementations accept the name of a directory in either
  ;; form and return the name in the form given. However the name of a
  ;; file must be given in file form. So we try first with a directory
  ;; name which will return NIL if either the file doesn't exist at
  ;; all or exists and is not a directory. Then we try with a file
  ;; form name.
  ;;(or (probe-file (pathname-as-directory pathname))
   ;;   (probe-file pathname))

#|  (or (ignore-errors
        ;; PROBE-FILE will return the truename if file exists and is a
        ;; file or NIL if it doesn't exist at all. If it exists but is
        ;; a directory PROBE-FILE will signal an error which we
        ;; ignore.
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        ;; PROBE-DIRECTORY returns T if the file exists and is a
        ;; directory or NIL if it doesn't exist at all. If it exists
        ;; but is a file, PROBE-DIRECTORY will signal an error.
        (let ((directory-form (pathname-as-directory pathname)))

   ;;what is package ext in ext:probe-directory? It creates compile error.
  ;;        (when (ext:probe-directory directory-form)
   ;;         directory-form))))
|#

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    ;;(error "list-directory not implemented")
    (values filepath filename-str)
    ;;end let, file-exists-p
    ))|#



;;DIRECTORY-P
;;2019 modified
;;ddd
(defun directory-p (name)
  "Is `name' the name of an existing directory. RETURNS (values dir-path dir-paths) dir-paths are real objects. "
#| old  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))|#
  (let
      ((dir-paths  (list-directory name))
       (dir-path (probe-file name))
       )  
       (values dir-path dir-paths)
    ;;end let,directory-p
  ))
;;TEST
;; (directory-p "C:\\3-TS\\") =
;; (directory-p "c:\\nosuchdir\\") = NIL NIL
;; ;; (directory-p "C:/TEMP" :GET-DIR-LIST-P T)
;; an EMPTY DIR
;; ;; (directory-p "C:/TEMP4/")
;; works = #P"C:/TEMP4/"  NIL

;;(ensure-directories-exist "c:/3-TS/MyDrives/" :verbose T)
;; works tells exist (NIL not create) = "c:/3-TS/MyDrives/"   NIL
;;(ensure-directories-exist "c:/3-TS/MyDrives/saved/" :verbose T)
;; works creates directory= ;; Created directory c:\\3-TS\\MyDrives\saved\   "c:/3-TS/MyDrives/saved/"  T


;;FILE-P
;;
;;ddd
(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))
    ;;end let, file-p
    ))



;;MY-ENSURE-PATHS-EXIST
;;2019
;;ddd
(defun my-ensure-paths-exist (pathnames-list &key tested-dirs host
                                           (return-filenames-p T)  checked-dirs
                                            (chk-host-exist-p T) (slashstr "\\") 
                                            ensure-pathobjects-exists-p)
  "U-files, RETURNS    INPUT:  "
  (let
      ((hosts)
       (new-dirs)
       (fulldirpaths)
       )
    (loop
     for pathname in pathnames-list
     do
     (let*
         ((fulldirpath "")
           (tested-pathname)
           (tested-dir)
          )       
       (multiple-value-bind (tested-dir filename filetype pathobject new-dir)
           (my-ensure-path-exists pathname :host host
                                       :return-filename-p return-filenames-p
                                     :slashstr slashstr  :chk-host-exist-p chk-host-exist-p
                                     :return-pathobject-p ensure-pathobjects-exists-p)
         ;;add to tested-dirs if not a member
         (unless (member tested-dir tested-dirs :test 'string-equal)
           (setf tested-dirs (append tested-dirs (list tested-dir))))

         ;;MAKE FULLDIRPATH
         (cond
          (filename
           (setf fulldirpath (format nil "~A~A~A.~A" tested-dir slashstr filename
                                            filetype)))
          (tested-dir
           (setf fulldirpath tested-dir))
          (t nil))
         ;;append fulldirpaths
         (when fulldirpath
           (setf fulldirpaths (append fulldirpaths (list fulldirpath))))

       ;;directory-p
       (unless  (member fulldirpath tested-dirs :test 'string-equal)
         (setf fulldirpath (ensure-final-string fulldirpath :final-str slashstr))
         (multiple-value-bind (dirpath1 new-p)
             (ensure-directories-exist fulldirpath)  

           (setf tested-dirs (append tested-dirs (list fulldirpath)))
           (when new-p
             (setf new-dirs (append new-dirs (list fulldirpath))))
           ;;end when,mvb,unless, let,loop
           )))))
     (values tested-dirs new-dirs)
     ;;(tested-dirs filenames filetype pathobject new-dir)
    ;;end let, my-ensure-paths-exist
    ))
;;TEST
;; (my-ensure-paths-exist '("c:/dropbox-add/MyUtilities/" "c:/dropbox-add/help/"))
;; (my-ensure-paths-exist '("c:/temp1/test"))
;; for non-existant drive H:
;; (my-ensure-paths-exist '("h:/")) = NIL NIL




;;MY-ENSURE-PATH-EXISTS
;;2019
;;ddd
(defun my-ensure-path-exists (pathname &key  host (return-filename-p T)
                                       add-filename-end checked-dirs (ensure-final-str "\\")
                                       (chk-host-exist-p T) (slashstr "\\") return-pathobject-p)
  "U-files  RETURNS (values dir-exists filename filetype pathobject new-dir)   INPUT: pathname or pathobject. ADD-FILENAME-END is a string (eg. a save-date). If dir not exist, CREATES NEW DIR. "
  (multiple-value-bind (pathname1 filename filetype dir host1)
      (file-pathname-p pathname :return-details-p T)
    (unless host (setf host host1))
    (let*
        ((new-dir)
         (fulldirpath (format nil "~A:~A" host dir))
         (file-path-p (when filename pathname))
         (host-exists-p T) ;;assumed unless changed below
         (dir-exists)
         (newfilename)
         (pathobject)
         )
      (when file-path-p
        (cond
         (add-filename-end
          (setf newfilename (format nil "~A~A.~A" filename add-filename-end filetype)))
         (t (setf newfilename (format nil "~A~A" filename  filetype)))))

      ;;CHECK PATHS
      (cond
       ;;already checked, get info
       ((and checked-dirs (member fulldirpath checked-dirs :test 'string-equal))
        ;;(member (delete-final-string '("\\") dirpath) checked-dirs :test 'string-equal))
        (setf dir-exists fulldirpath)
          ;;not needed?    filename (pathname-name pathname)
             ;;not needed?   filetype (pathname-type pathname))
        )
       ;;NOT in checked-dirs, get info and CHECK
       (t       
        (when chk-host-exist-p
          (setf host-exists-p (drive-exists-p host)))
        ;;check dirs
        (when host-exists-p
          ;;WHEN DIR NOT EXIST, CREATE IT!!
          (setf dir-exists (my-ensure-dir-exists fulldirpath))
        ;;end when, t, cond
        )))
      (when return-pathobject-p
        (setf pathobject (pathname pathname)))
      (values dir-exists filename filetype pathobject) 
      ;;end mvb,let, my-ensure-path-exists
      )))
;;TEST
;; (my-ensure-path-exists "E:\\4-TS\\2 TEST-DIR2" :return-pathobject-p T)
;; works= "F:\\4-TS\\2 TEST-DIR2\\"  NIL  NIL  #P"F:/4-TS/2 TEST-DIR2"
;; above creates new dirs
;; (my-ensure-path-exists "C:\\3-TS\\LISP PROJECTS TS\\Math\\Permutations\\readme.lisp" :return-pathobject-p T)
;; works = "C:\\3-TS\\LISP PROJECTS TS\\Math\\Permutations\\"  "readme"  "lisp"  #P"C:/3-TS/LISP PROJECTS TS/Math/Permutations/readme.lisp"  NIL
;; for a path object INPUT
;; (setf **testpth2 (pathname "C:\\3-TS\\LISP PROJECTS TS\\Math\\Permutations\\readme.lisp"))
;; #P"C:/3-TS/LISP PROJECTS TS/Math/Permutations/readme.lisp"
;; (my-ensure-path-exists **testpth2  :return-pathobject-p T))
;; works= "C:\\3-TS\\LISP PROJECTS TS\\Math\\Permutations\\"    "readme"  "lisp" #P"C:/3-TS/LISP PROJECTS TS/Math/Permutations/readme.lisp"  NIL
;;IF PREVIOUSLY TESTED
;; (my-ensure-path-exists **testpth2  :return-pathobject-p T :checked-dirs '("C:\\3-TS\\LISP PROJECTS TS\\Math\\Permutations\\")  :return-pathobject-p T)
;; works= "C:\\3-TS\\LISP PROJECTS TS\\Math\\Permutations\\"  "readme"  "lisp"  #P"C:/3-TS/LISP PROJECTS TS/Math/Permutations/readme.lisp"  NIL
;;



;;MY-ENSURE-DIR-EXISTS
;;2019
;;ddd
;;NAME
;;2019
;;ddd
(defun my-ensure-dir-exists (path  &key (chk-is-filepath-p T) alt-path )
  "U-files    RETURNS: (values dirpath-exists-str new-p  ))  INPUT: String: Can be dir-pathname that ends in no-slash or slash, or can end in filename. Does NOT create new file."
  (let
      ((filepath-p (filepathname-p path))
       (path1)
       (result)
       )
    (cond
     ((null filepath-p)
      (setf path1 (ensure-end-slash path)))
     (t (setf path1 path)))
    (when (and path1 (not (match-substring "NIL" path1)))
     (setf result (ensure-directories-exist path1)))
    (when (and (null result) alt-path)
      (setf result (format nil  "ALT PATH= ~A" alt-path)
            path1 alt-path))
    (values result path1)
    ;;end let, my-ensure-dir-exists
    ))
;;TEST
;; (my-ensure-dir-exists "F:\\1 NEW3\\NEWX")
;; works= "F:\\1 NEW3\\NEWX/"  "F:\\1 NEW3\\NEWX/"
;; ;; (my-ensure-dir-exists "F:\\1 NEW4\\NEWX\\testfile.txt")
;; "F:\\1 NEW4\\NEWX\\testfile.txt" "F:\\1 NEW4\\NEWX\\testfile.txt"
;; ;; (my-ensure-dir-exists "F:\\0 LW-CUM-dated\\CogSys-Model")



;;FILEPATHNAME-P
;;2019
;;ddd
(defun filepathname-p (pathstr  &key (chk-if-exists-p T)  )
  "U-files    RETURNS: N of period char or NIL.  INPUT: string"
 (search "." pathstr))
;;TEST
;; (filepathname-p "\\this\\that\\filename.longext")




;;ENSURE-SAVE-FILE-TO-DIR
;;2019
;;ddd
(defun ensure-save-file-to-dir (from-filepath to-path)
  "U-files  RETURNS (values dos-string dos-result copied-p)   INPUT: dos-ok path stringsto-path can be a dir or a NEW FILENAME."
  (let
      ((dir-exists (my-ensure-dir-exists to-path))
       (dos-string)( dos-result)( copied-p)
       )
      (when dir-exists
        (multiple-value-setq (dos-string dos-result copied-p)
            (my-dos-copy from-filepath to-path))
    (values dos-string dos-result copied-p)    
    ;;end mvb,let, ensure-save-file-to-dir
    )))
;;TEST
;; (ensure-save-file-to-dir "c:\\temp2\\test-newx.txt" "c:\\temp3\\test-newnamex.txt")
;; works= "copy \"c:\\temp2\\test-newx.txt\"  \"c:\\temp3\\test-newnamex.txt\"         1 file(s) copied.   "  0   T




;;PPRINT-OBJECT-TO-FILE
;;
;;ddd
(defun pprint-object-to-file (lisp-object pathname 
                                              &key (incl-label 'list) 
                                              (if-exists-keyword :append))
  "In U-files, pprints a lisp-object to a file. If append-p, appends current file if exists.  If incl-label, puts nested-list inside a new list with the symbol beside the keyword first item. (Also works for non-nested lists). Does NOT properly print nested lists."
;;SSS
 (with-open-file (out pathname :direction :output
                          :if-exists if-exists-keyword :if-does-not-exist :create)
   (format t ">>>>>>>> PRINTING LISP-OBJECT TO FILE: ~A~%" pathname)
   (if incl-label
       (format out "~%  (~A~%" incl-label))
   (pprint lisp-object out) 
   (if incl-label
       (format out "~%   )%" ))
   (format t ">>>>>>>> END PRINTING LISP-OBJECT TO FILE: ~A~%" pathname)
   ;;end with, pprint-object-to-file
   ))
;;TEST
;; (pprint-object-to-file '(this (that  (nested (list) a ) b ) c) "C:\\3-TS\\LISP PROJECTS TS\\test-object.lisp" :incl-label 'my-label)




;;PRINT-LIST-TO-FILE
;;
;;ddd
(defun print-list-to-file   (list pathname &key (incl-label 'list) 
                                  (incl-quotes-p t)
                                   (no-newline-p nil)
                                  (if-exists-keyword :append))
  "In U-files, pprints a lisp-object to a file. If append-p, appends current file if exists.  If incl-label, puts nested-list inside a new list with the symbol beside the keyword first item. (Also works for non-nested lists). print-fewer-newlines-p puts additional newlines after double-nested items. "
  ;;SSS
  (with-open-file (out pathname :direction :output
                       :if-exists if-exists-keyword :if-does-not-exist :create)
    (format t ">>>>>>>> PRINTING LIST TO FILE: ~A~%" pathname)

      (print-list list :stream out :incl-quotes-p  incl-quotes-p
                                         :no-newline-p no-newline-p)
    (if incl-label   
        (format out "~%  )~%" ))
    (format t ">>>>>>>> END PRINTING LIST TO FILE: ~A~%" pathname)
    ;;end with-open,  print-list-to-file
    ))
 ;;  (print-list-to-file  '(test (a (b 1 2 3)(c 4 5 6) d (e 7 ("x" 1 2 3) f  "g")))  "C:\\3-TS\\LISP PROJECTS TS\\test.lisp" :print-nested-list-to-file;;PRINT-NESTED-LIST-TO-FILE



;;PPRINT-NESTED-LIST-TO-FILE
;;
;;ddd
(defun pprint-nested-list-to-file (nested-list pathname &key incl-label 
                                              (incl-quotes-p t)
                                              (print-fewer-newlines-p nil)
                                              (no-newline-p nil)
                                              (if-exists-keyword :append))
  "In U-files, pprints a lisp-object to a file. If append-p, appends current file if exists.  If incl-label, puts nested-list inside a new list with the symbol beside the keyword first item. (Also works for non-nested lists). print-fewer-newlines-p puts additional newlines after double-nested items. "
  ;;SSS
  (with-open-file (out pathname :direction :output
                       :if-exists if-exists-keyword :if-does-not-exist :create)
    (format t ">>>>>>>> PRINTING NESTED-LIST TO FILE: ~A~%" pathname)
    (if incl-label
        (format out "~% (~A~%" incl-label))
    (cond
     ((null print-fewer-newlines-p)
      (print-double-nested-list  nested-list :stream out :incl-label incl-label
                                 :incl-quotes-p  incl-quotes-p :no-outer-parens-p t))
     ;;if want double-nested-list printed with more newlines 
     (t
      (print-nested-list nested-list :stream out :incl-quotes-p  incl-quotes-p
                         :no-newline-p no-newline-p :incl-label incl-label)))

    (if incl-label   
        (format out "~%  )~%" ))
    (format t ">>>>>>>> END PPRINTING NESTED-LIST TO FILE: ~A~%" pathname)
    ;;let, print-nested-list-to-file
    ))
;;TEST
;; (pprint-nested-list-to-file '(this (that  (nested (list) a ) b ) c)   "C:\\3-TS\\LISP PROJECTS TS\\test-object.lisp"  :incl-label  'my-label)
;;(print-nested-list-to-file '(double (this (that  (nested (list) a ) b ) c) d)   "C:\\3-TS\\LISP PROJECTS TS\\test-object.lisp"  :incl-label  'my-label :print-fewer-newlines-p t)
;;  (pprint-nested-list-to-file '(xxx  (a 1 2 3) (b 4 5 6) ( c 7 8 9) d)   "C:\\3-TS\\LISP PROJECTS TS\\test-object.lisp"  :incl-label  'my-label :print-fewer-newlines-p t)








;;------------------------------------------- FROM GW TSTRING.LSP IN GW-UTILITIES -----------

;;MY-FILE-POSITION
;;
;;This works analagos to file-position, but not keywords
;;	1) NO ARG=> RETURNS FILE-POSITION
;;	2) 'START=> MOVES POINTER TO 0
;;	3) '(end path)=> MOVES POINTER TO EXACT END (note: often may
;;			want to start next entry with a space)
;; 
;;EXAMPLE (my-file-position '(end fl1)) works with all args
;;
;;(setf path "c:\\temp\\test.lsp")
;;(with-open-file (stream path :direction :io)(file-position stream :start)(file-position stream))
;;(with-open-file (stream path :direction :io)(my-file-position stream `(end ,path))(file-position stream)(print (gensym "foo") stream)(file-position stream))




;;MY-FILE-POSITION
;;
;;ddd   
(defun my-file-position (stream &optional  my-key)
  "\gwt-util\tstring"
"1-no/arg 2-'start 3-'(end path) If end is not in exactly right position, items will be written to file past eof mark and not readable by editor until eof mark removed--this works"
	(cond
	 ((equal my-key 'start)
	 (send stream :set-pointer 0))
	 ((and (listp my-key) (equal (car my-key) 'end))
	 (setf *path (eval (second my-key)))
	 (file-size *path) ;(function file-size) *path)
	 (send stream :set-pointer (- **file-size 3))   ;;or should it be 1??
	 (find-file-end stream)

	 )
	 ((not (null my-key))
	 (send stream :set-pointer my-key))
	 (t (send stream :read-pointer)))
        ;;my-file-position
        )
;;TEST
;;(my-file-position os ) works as it should, but needs fix on end





;;FIND-FILE-END
;;
;;ddd
(defun find-file-end (stream)
  (list "U-files.lisp")
  (send stream :set-pointer (- **file-size 5))   ;;or should it be 1??
  (do
      ((end? (read-char stream nil 'eof-found)
	     (read-char stream nil 'eof-found))
       )
      ((equal end? 'eof-found)
       (return t))
    ;;end let,find-file-end
    ))
;;TEST




;;IS-FILE-POSITION-NOT-EOF
;;
;;ddd
(defun is-file-position-not-eof (stream)
  (list "U-files.lisp")
  (let ((file-length (file-length stream))
         (file-position (file-position stream))
         )
    (if (> file-length file-position)
        t nil)
    ))
;;TEST


;(setf tf "c:\\andy\\temp.lsp")
;(setf os (open tf :direction :io))
;(send os :set-pointer 99999)
;(send os :read-pointer)
;(send os :write-char #\newline)
;(send os :write-char #\~)

;;FROM ELI AT GOLD HILL TO FIND END OF FILE
;;(1) slow, but simple, read file to get to end
;;(2) FUNCTION BELOW USING FILE-INFO FUNCTION (I had to guess at last part)
;;		he had said something like (+ * hi 65536 lo)



;;MY-PATH-P
;;
;;ddd
(defun my-path-p (path &key create-file)
 (list "gwt-util tstring checks to see existence of file")
 (cond
   ((and (pathnamep (pathname path))
          (not (null (file-info path)))))  ;;(file-info "c:\\temp\\xxx.lsp")
   (create-file
    (with-open-file (stream path :direction :output)
      (format stream "          test
 "))
   (values nil "created a new empty file"))
   (t (values nil "NOT A PATHNAME")))
 ;;end my-path-p
 )
;;TEST

;;MY-PATH-P
;;-> RETURNS T IF A FILE EXISTS & NIL IF NOT
;;(null (my-path-p *class-filename))
;; (my-path-p "c:\\andy\\f-person.lsp"))
;;(my-path-p "c:\\a-print\\ot2.lsp" :create-file t)
;;(my-path-p "c:\\a-print\\ot3.lsp")
;;gw1 version
;(defun file-size (path)
;  (list "\\gwt-util\\tstring")
;  (multiple-value-bind (ignore lo hi) (file-info path)
;  (setf **file-size (- hi lo))))
;;NOTE:: CL FUNCTION (file-length path) is preferred version --not in gw


;;SET-DRIVE-LABEL
;;2017
;;ddd
(defun set-drive-label (drive new-label)
  "In U-files renames the drive label for eg. E, with new-label. RETURNS label."
  ;;HERE
  

)


;;FILE-SIZE
;;2017
;;ddd
(defun file-size (path)
  "In U-files, uses with-open.  If not exist,returns nil"
  (with-open-file (instr path :direction :input :if-does-not-exist nil)
   (file-length instr)
   ))
;;TEST
;; (file-size "c:\\3-TS\\MyDrives\\3-TS\\MyDrives-settings-db.lisp") = 112
;; (file-size "c:\\3-TS\\MyDrives\\file-not-exist.lisp") = NIL





;;FILE-DATE
;;
;;ddd
(defun file-date (path &key (main-sep ";")(sep":"))
  "In U-files SEP is separator. RETURNS (values date-string year month  date hour minute second) "
  (let
      ((date-string)       
       )
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (file-write-date path))
    (setf date-string (format nil "~A~A~A~A~A~A~A~A~A~A~A"
        year main-sep month sep date main-sep hour sep minute sep second))
   (values date-string year month  date hour minute second)
   )))
;;TEST
;;  (file-date  "c:\\3-TS\\MyDrives\\3-TS\\MyDrives-settings-db.lisp")
;;works=  "2017;4:17;14:11:17" 2017 4 17 14 11 17


;;  (sys:call-system-showing-output command :prefix "; " :show-cmd t :output-stream *standard-output*  :extra-environ  nil)
;; (multiple-value-setq (xx1  xx2 xx3 xx4) (sys:call-system-showing-output "vol e:" :prefix "; " :show-cmd t :output-stream *standard-output*  :extra-environ  nil))






;;GET-FILE-INFO
;;2017
;;ddd
(defun get-file-info (path &key (get-vol-info-p T))
  "In U-files, RETURNS   (values filename dir host date size vol-info)  "
  (let
      ((date (file-date path))
       (size (file-size path))
       (host (format nil "~A:"  (host-namestring path)))
       (dir (directory-namestring path))
       (filename (file-namestring path))
       (vol-info)
       )
    (when get-vol-info-p
      (setf vol-info (my-dos-vol host)))

    (values filename dir host date size vol-info)
    ;;end let, defun
    ))
;;TEST
;;  (get-file-info "c:\\3-TS\\MyDrives\\3-TS\\MyDrives-settings-db.lisp")
;; works=  
#|"tomex-settings-db.lisp"
"\\3-TS\\MyDrives\\"
"c:"
"2017;4:17;14:11:17"
112
"VOL  c:
 Volume in drive C is OS
 Volume Serial Number is 4058-766B
"|#






;;COUNT-READS
;;
;;ddd
(defun count-reads (path &key (max-reads 10000))
  "In U-files, counts number of reads it takes to get to eof. Useful for eg. NUMBER OF CASES in a file"
  (let 
      ((num-reads 0)
       )
          (with-open-file (in path  :direction :input :if-does-not-exist nil) 
        ;; (afout 'out (format nil ">>>> In read-shaq-data, shaq-all-datafiles-path ~A" shaq-all-datafiles-path))
        
        ;;   (with-open-file (out spss-datafile-path :direction :output :if-does-not-exist :create)
        ;;first write the spss variable list to the file
        ;;  (when (= n 1) (format out "~A~%" spss-all-varnames))

        ;;INPUT from shaq-data-file  
        (with-standard-io-syntax
          (loop
           for n from 1 to max-reads
           do  
           (let
               ((item)
                )
           (setf item (read in nil 'eof))
           ;;(afout 'out (format nil ">>>> In read-shaq-data, item ~A" item)) 
             
           (cond
            ((equal item  'eof)
             (return))
            (t (incf num-reads)))
           ;;end loop, with, with
           ))))
          num-reads
          ;;end let, count-reads
          ))
;;TEST
;;  (count-reads "C:\\3-TS\\LISP PROJECTS TS\\shaq-data-list-for-spss.lisp" :max-reads 100)






;;COMBINE-LISP-OBJECT-FILES
;;
;;ddd
(defun combine-lisp-object-files (infile-list target-file &optional in-dir-root  &rest write-keys ) 
  "In U-files, combines a list of files. Writes first file to target-file, then appends the rest. Uses CL READ and WRITE. Ideal for most data files. If in-dir-root, appends root to filename string. pretty-p causes it to pretty-print to file. write-keys should be write keys plus value eg. :pretty T. NOTE: MUST include NIL or a path for in-dir-root if use write-keys."
  (let
      ((object)
       )
    (with-open-file (out  target-file :direction :output :if-exists :append
                          :if-does-not-exist :create)
      (loop
       for infile in infile-list
       do
       (if in-dir-root
           (setf infile (format nil "~A~A" in-dir-root infile)))
       (with-open-file (in infile :direction :input)
         (loop
          (setf object (read in nil 'eof))
          (cond
           ((equal object 'eof) 
            (return) infile)
           (t
            (cond 
             (write-keys
               (apply #'write object :stream out write-keys))
             (t (write object :stream out )))
            (terpri out)
            ;;end outer t, cond
            ))
          ;;end loop, with
          ))
       ;;end outer loop, with, let, combine-lisp-object-files
       ))))
;;TEST
;;  (combine-lisp-object-files '( "shaq-data-list-for-spss.lisp")  "C:\\3-TS\\LISP PROJECTS TS\\test-combine.lisp"  "C:\\3-TS\\LISP PROJECTS TS\\" ) = works
;; (combine-lisp-object-files '( "shaq-data-list-for-spss.lisp")  "C:\\3-TS\\LISP PROJECTS TS\\test-combine.lisp"  "C:\\3-TS\\LISP PROJECTS TS\\" :pretty T ) = works.
;;  (combine-lisp-object-files '( "shaq-data-list-for-spss.lisp")  "C:\\3-TS\\LISP PROJECTS TS\\test-combine.lisp"   :pretty T ) = ERROR, MUST HAVE NIL IN OPTIONAL SPOT HERE.
;;  (combine-lisp-object-files '( "shaq-data-listn-for-spss.lisp")  "C:\\3-TS\\LISP PROJECTS TS\\test-combine.lisp" NIL   :pretty T ) = ,WORKS,  MUST HAVE NIL IN OPTIONAL SPOT HERE.
;; NOTE: USING &KEY. Must put &key AFTER &REST.  Causes the &key key-vars to be included in the &rest list.  If using Apply, causes error unless want it there.

#|(let
 ((x 0) )
 (loop
  (unless (> (incf x) 4)
    (format t "x= ~A~%" x)
    )))|#
;;works, prints x=1 thru x=4


;;COMBINE-TEXT-FILES
;;
;;ddd
(defun combine-text-files (infile-list outfile &key in-dir-root (use-format-A-p T))
  "In files"
  (let
      ((textline)
       )
    (with-open-file (out outfile :direction :output :if-exists :append :if-does-not-exist :create)
      (loop
       for infile in infile-list
       do
       (with-open-file (in infile :direction :input)
         (loop
         (setf textline (read-line in nil 'eof))
         (cond
          ((equal textline 'eof)
           (return))
          (t
           (cond
            (use-format-A-p
           (format out "~A~%" textline))
            (t
             (format out "~S~%" textline)))
           ;;end outer t, cond, inner loop
           )))
         ;;end with, outer loop
         ))
      ;;end with, let, combine-text-files
      )))
;;TEST
;;  (combine-text-files   '("SHAQ-Deliver LOG.lisp" "shaq-results-text-eg1.txt") "C:\\3-TS\\LISP PROJECTS TS\\test-text-combine.txt" :in-dir-root "C:\\3-TS\\LISP PROJECTS TS\\")
;; ABOVE WORKS, prints text data from each file to the outfile.


;;MAKE-DIRECTORY
;;2017
;;ddd
(defun make-directory (dirpath &key (show-results-p T)  slashchar)
  "In U-files.  RETURNS (values dirpath1 dirpath2 dir-created-p)  dirpath can end in \\ or nothing.    SHOW-RESULTS => popup showing results. dirpath1 has forward slashes.  dirpath2 same as original dirpath. Uses CL ensure-directories-exist"
  (let
      ((dirpath1 (namestring dirpath))  ;;(filename-no-ext "this.txt")      
       )
    (when dirpath1
      (setf dirpath1 (ensure-end-slash dirpath1)))

    (multiple-value-bind (dirpath2 dir-created-p)
        (ensure-directories-exist dirpath1) ;;must END IN SLASHES to work
      (when slashchar
        (cond
         ((char-equal slashchar #\\)
          (setf dirpath1 (substitute slashchar #\/ dirpath2)))
         (t (setf  dirpath1 (substitute slashchar #\\  dirpath2)))))
      (values dirpath1 dirpath2 dir-created-p)
      )))
;;TEST
;; (make-directory "c:\\temp2X\\")
;; works = "c:\\temp2X\\"  "c:\\temp2X\\"  T [created new dirs]
;; (make-directory "c:/temp2X/temp/new/") 
;; works= "c:\\temp2X\\temp\\new\\"  "c:\\temp2X\\temp\\new\\" T [created new di

;; (make-directory "F:\\1 NEW\\2 NEW\\test-file.txt")
;; works= makes dir "F:\\1 NEW\\2 NEW\\" and not file test-file.txt
;; (make-directory "F:/1 NEW2/2 NEW")




;;GET-FILE-DIR-NAME-TYPE
;;2019
;;ddd
(defun get-file-dir-name-type (filepath)
  "U-files  RETURNS (values dir filename filetype host fullpath).INPUT: string or path."
  (let*
      ((fullpath (ensure-end-slash filepath))
       (dir (directory-namestring fullpath))
       (filename (pathname-name fullpath))
       (filetype (pathname-type fullpath))
       (host (pathname-host fullpath))
       )
    (values dir filename filetype host fullpath)
    ;;end let, get-file-dir-name-type
    ))
;;TEST
;; (get-file-dir-name-type "C:\\this\\that\\other\\testfile.txt")
;; works= "\\this\\that\\other\\"    "testfile"  "txt" "C"  "C:\\this\\that\\other\\testfile.txt"
;; for path object
;; (setf **testpathh (pathname "C:\\this\\that\\other\\testfile.txt"))
;; works= #P"C:/this/that/other/testfile.txt"
;; (get-file-dir-name-type  **testpathh)
;; works= "\\this\\that\\other\\"  "testfile"  "txt"  "C"  "C:\\this\\that\\other\\testfile.txt"
;; ;; (get-file-dir-name-type "C:\\this\\that\\other\\")
;; works= "\\this\\that\\other\\"  NIL  NIL "C" "C:\\this\\that\\other\\"
;; ;; (get-file-dir-name-type "C:\\this\\that\\other")
;; works= "\\this\\that\\other\\"  NIL  NIL  "C"  "C:\\this\\that\\other/"





;;MY-PARSE-PATHNAME
;;2017
;;ddd
(defun my-parse-pathname (pathname &key pathnamestr  (convert-backslashes-p T))
  "In   RETURNS  (values  all-subdirs filename host directory pathnamestr type)  INPUT: A string or real path.  Note: host returned as only a letter."
  (cond
   ((stringp pathname) 
    (setf pathnamestr pathname ))
   ;;if pathnamestr is a string
   ((stringp pathnamestr) NIL)
   ;;if pathaname is a real path
   ((pathnamep pathname)
    (setf pathnamestr (namestring pathname)))
   (t nil))

  (when convert-backslashes-p
    (setf pathnamestr (substitute #\/  #\\ pathnamestr)))

  (let*
      ((directory (directory-namestring pathnamestr))
       (next-level-dir)
       (cur-level-subdir)
       (all-subdirs (cdr (pathname-directory pathnamestr)))
       (filename (pathname-name pathnamestr))
       (host (pathname-host pathnamestr))
       (type (pathname-type pathnamestr))
       )
    (when directory
      (setf directory (substitute #\/  #\\ directory)
       all-subdirs (divide-string-to-all-tokens directory :char-delim-list '(#\\ #\/ ))))

    (values  all-subdirs filename host directory pathnamestr type)
    ;;end let, my-parse-pathname
    ))
;;TEST
;;  (my-parse-pathname "c:/this/temp/sub/file.lisp")
;; works= ("this" "temp" "sub")   "file"   "c"  "/this/temp/sub/"   "c:/this/temp/sub/file.lisp"
;;help
;;  (divide-string-to-all-tokens "/TEMP/PART/SUB/" :char-delim-list '(#\\ #\/ ))
;; works=  ("TEMP" "PART" "SUB")  ""
;; (PARSE-NAMESTRING "C:/FOO/TEMP/DIR/THIS.LISP")
;; CONVERTS TO A REAL PATHNAME=  #P"C:/FOO/TEMP/DIR/THIS.LISP"   25
;; (pathname-directory (parse-namestring "C:/foo/bar/baz.lisp") :CASE :LOCAL)
;; = (:ABSOLUTE "foo" "bar")
;; (pathname-directory (parse-namestring "/foo/bar/baz.lisp") :case :local)
;; = (:ABSOLUTE "foo" "bar")




;;MY-DIRECTORY-NAMESTRING
;;
;;ddd
(defun my-directory-namestring (pathstring &key incl-host-p host replace-host-p
                                           (ensure-final-str "\\") (slashstr "\\")
                                           incl-path-no-type-p)
  "In U-files, like directory-namestring, but returns dirname with SLASHSTR INSTEAD OF BACKSLASH if use-backslash-p.  
  RETURNS (incl-host-p (values host-dir-namestr dir-namestring))   (t (values dir-namestring host-dir-namestr))). If no host, returns NIL for host-dir-namestr"
  (let
      ((dir-namestring)
       (dir-namestring (directory-namestring pathstring))
       (host-namestr (host-namestring pathstring))
       (host-dir-namestr)
       (path-no-filetype)
       (filename (pathname-name pathstring))
       )
    (when ensure-final-str
      (setf dir-namestring (ensure-final-string dir-namestring
                                                :delete-strings '("/" "\\") 
                                                :final-str ensure-final-str)))
    (cond
     ((and replace-host-p host)
      (setf host-dir-namestr (format nil "~A:~A" host  dir-namestring)))
     ((and host-namestr (not (string-equal "" host-namestr)))
      (setf host-dir-namestr (format nil "~A:~A" host-namestr
                                      dir-namestring)))
     ((and host (not (string-equal "" host)))
      (setf host-dir-namestr (format nil "~A:~A" host dir-namestring))))
    (when incl-path-no-type-p ;;added 2020
      (setf path-no-filetype (format nil "~A~A" host-dir-namestr filename  )))
    (cond
     (incl-host-p 
      (values host-dir-namestr dir-namestring path-no-filetype ))
     (t (values dir-namestring host-dir-namestr path-no-filetype)))
    ;;end let, defun
    ))
;;TEST
;; (my-directory-namestring "C:/3-TS/LISP PROJECTS TS/CogSysOutputs/ALL-CSQ-USER-DATA.lisp" :incl-path-no-type-p T)
;; works=  "\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\"     "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\"    "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\ALL-CSQ-USER-DATA"

;; (my-directory-namestring "C:/3-TS/LISP PROJECTS TS/CogSysOutputs/ALL-CSQ-USER-DATA.lisp")
;;works=  "\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\"  "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\"
;;  (my-directory-namestring "C:\\TEMPX\\THIS\\THAT\\FILE.TXT")
;; works= "\\TEMPX\\THIS\\THAT\\"  "C:\\TEMPX\\THIS\\THAT\\"
;;  (my-directory-namestring "C:\\TEMPX\\THIS\\THAT\\FILE.TXT" :incl-host-p T)
;; works= "C:\\TEMPX\\THIS\\THAT\\"  "\\TEMPX\\THIS\\THAT\\"
;; FOR HOSTLESS DIR PATHS
;;  (my-directory-namestring "\\temp\\test\\") 
;; works= "\\temp\\test\\"  NIL
;;  (my-directory-namestring "C:\\TEMPX\\THIS\\THAT\\FILE.TXT" :host 'h :incl-host-p T :replace-host-p T)
;; works= "H:\\TEMPX\\THIS\\THAT\\"  "\\TEMPX\\THIS\\THAT\\"

;;    (substitute #\/  #\\    "C:\\TEMPX\\THIS\\THAT\\FILE.TXT")
;; (substitute #\/  #\\  "this\\that\\other\\") = "this/that/other/" WORKS
;; (substitute  #\x  #\c  "abcde") = "abxde" WORKS
;;Following don't work:
;; (substitute  "x"  "c"  "abcde") = "abcde"
;; (subst  "x"  "c"  "abcde") = "abcde"
;; (subst  "x"  "c"  '("a" "b" "c" "de") ) = ("a" "b" "c" "de")


#|DELET (defun my-directory-namestring (pathstring &key incl-host-p (ensure-final-str "/"))
  "In U-files, like directory-namestring, but returns dirname with SLASHSTR INSTEAD OF BACKSLASH if use-backslash-p.   RETURNS (values  dir-namestring  backslash-dirname)"
  (let
      ((dir-namestring)
       (backslash-dirname (directory-namestring pathstring))
       (host)
       )
    (when ensure-final-str
      (setf dir-namestring (ensure-final-string dir-namestring
                                                :delete-strings '("/" "\\") 
                                                :final-str ensure-final-str)))
    ;;(setf dir-namestring (substitute #\/  #\\ backslash-dirname))
    (when (and incl-host-p (setf host (pathname-host pathstring)))
      (setf dir-namestring (format nil "~A:~A" host dir-namestring)))   
    (values  dir-namestring  backslash-dirname)
    ;;end let, defun
    ))|#


;;MAKE-DATED-PATHNAME
;;
;;ddd
(defun make-dated-pathname (pathname
                &key  (root "C:\\3-TS\\LISP PROJECTS TS\\")
                         (file-ext "lisp") no-date-p incl-time-p append-begin append-end
                         (convert-backslashes-p T))
  "In U-files,  filename string can be a full pathname, a filename w/ or w/o ext. If filename only adds ROOT diretory string before and the date plus period file-ext onto the end. If no-date-p, doesn't include date. FILE-EXT is used whether or not filename has a file-ext (unless nil uses filename ext) APPEND-BEGIN appended to filename.  AUTO-detects if filename = ENTIRE PATH."   
  (when (symbolp pathname)
    (setf pathname (eval pathname)))

  (let
       ((date)
       )
    (multiple-value-bind (all-subdirs filename-base host directory pathnamestr type)
        (my-parse-pathname pathname :convert-backslashes-p convert-backslashes-p)
      
      ;;IF HOST, assume pathname is complete!
      (cond
       ((and host directory)
        (setf root (format nil "~A:~A" host directory)))
       (host
        (setf root (format nil "~A:")))
       (T
        (when (null root)
          (setf root (nth-value 1 (my-directory-namestring filename))))
        (unless file-ext
          (setf file-ext type))))

    ;;ADD BEGIN?
    (when append-begin
      (setf filename-base (format nil "~A~A" append-begin filename-base)))
    (cond
     ((null no-date-p)
      (multiple-value-bind (date-time1 date1 time year mo day hour min)
          (my-get-date-time :date-separator "-")
        (setf date date-time1)
        (cond
         (incl-time-p
          (setf pathname 
                (format nil "~A~A~A-~A-~A-~Ah~A" root filename-base year mo day hour min )))
         (t (setf pathname 
                  (format nil "~A~A~A-~A-~A" root filename-base year mo day ))))
        ))       
     (t (setf date (my-get-date-time) 
              pathname (format nil "~A~A" root filename-base  ))))

    ;;ADD END to filename
    (when append-end
      (setf pathname (format nil "~A-~A" pathname append-end )))
    ;;ADD FILE-EXT
     (setf pathname (format nil "~A.~A" pathname  file-ext))   
     (values pathname date  )
     ;;end mvb,let, make-dated-pathname
    )))
;;TEST
;; (make-dated-pathname *CS-CAT-DB-TREE-file)
;; works= "C:/3-TS/LISP PROJECTS TS/CogSysOutputs/CS-CAT-DB-TREE2020-7-18.lisp"      "Date: 7-18-2020  Time: 13:8"
;;AUTO-DETECT that filename is a full pathname
;; (make-dated-pathname"C:/3-TS/LISP PROJECTS TS/CogSysOutputs/ALL-CSQ-USER-DATA.lisp")
;;works= "C:/3-TS/LISP PROJECTS TS/CogSysOutputs/ALL-CSQ-USER-DATA2020-7-18.lisp"  "Date: 7-18-2020  Time: 13:8"
;; (make-dated-pathname  "ALL-CSQ-USER-DATA" :append-begin "Tom-" :root "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\" :file-ext "lisp")
;; works= "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\Tom-ALL-CSQ-USER-DATA2020-6-6-11.lisp"
;; (make-dated-pathname "testfile")
;; works= "C:\\3-TS\\LISP PROJECTS TS\\testfile2020-7-18.lisp"   "Date: 7-18-2020  Time: 13:9"
;;  (make-dated-pathname "testfile.lisp")   
;; works = "C:\\3-TS\\LISP PROJECTS TS\\SHAQ-PLUS\\testfile2014-12-4.lisp"
;;  (make-dated-pathname "testfile" :incl-time-p T) 
;; works="C:\\3-TS\\LISP PROJECTS TS\\SHAQ-PLUS\\testfile2014-12-4-10h25.lisp"
;;  (make-dated-pathname "testfile" :no-date-p T)
;; works= "C:\\3-TS\\LISP PROJECTS TS\\SHAQ-PLUS\\testfile.lisp"
;;  (make-dated-pathname "testfile2.docx" :file-ext nil)   
;; works=  "C:\\3-TS\\LISP PROJECTS TS\\testfile22020-5-23.docx"
;; using append-end
;; (make-dated-pathname "testfile" :append-end  9)
;; works= "C:\\3-TS\\LISP PROJECTS TS\\testfile2020-6-4-9.lisp"
;; (make-dated-pathname "testfile.lisp"  :append-begin "begin" :append-end "end")
;;works= "C:\\3-TS\\LISP PROJECTS TS\\begintestfile2020-6-4-end.lisp"



#|  Function PEEK-CHAR
SYNTAX: peek-char &optional peek-type input-stream eof-error-p eof-value recursive-p =SEE HELP SECTION BELOW
;;also
 (with-input-from-string (is "0123")
    (do ((c (read-char is) (read-char is nil 'the-end)))
        ((not (characterp c)))
     (format t "~S " c)))
>>  #\0 #\1 #\2 #\3
=>  NIL
111,459.30 vanguard
|#

;;MY-READ-FILES-APPEND-FILE
;;
;;ddd
(defun my-read-files-append-file (infile-list outfile &key root-inpath
                                              (eof 'eof) (if-exists :append)
                                    (if-does-not-exist :create)  collect-chars-list omit-chars-list)
  (let
      ((all-last-objects)
       (all-collected-chars)
       (all-omitted-chars)
       )
    (loop
     for infile in infile-list
     do
     (when (stringp infile)
       (when  root-inpath
         (setf infile (format nil "~A~A" root-inpath infile)))
       (multiple-value-bind (last-object collected-chars omitted-chars)
           (my-read-append-file infile outfile  :eof  eof  :if-exists if-exists
                                :if-does-not-exist if-does-not-exist
                                :collect-chars-list collect-chars-list
                                :omit-chars-list omit-chars-list)
         (setf all-last-objects (append all-last-objects (list last-object))
               all-collected-chars (append all-collected-chars (list collected-chars))
               all-omitted-chars (append all-omitted-chars (list omitted-chars)))
         ;;end mvb, when, loop
         )))
    (values all-last-objects all-collected-chars all-omitted-chars)
    ;;end let, my-read-files-append-file
    ))
;;TESTED IN SHAQ-PLUS, U-spss-data-analysis.
    



;;MY-READ-APPEND-FILE
;;
;;ddd
(defun my-read-append-file  (infile outfile &key (eof 'eof) (if-exists :append)
                                    (if-does-not-exist :create)  collect-chars-list omit-chars-list)
  "In U-files, reads infile chars, if a parens, peforms a read of the lisp-object and writes it to outfile. If not a parens, reads and writes the char to the outfile."
  (let
      ((inchar)
       (outchar)
       (object)
       (collected-chars)
       (omitted-chars)
       )  
    (with-open-file (instream  infile :direction :input)
      (with-open-file (outstream outfile :direction :output :if-exists if-exists
                                 :if-does-not-exist if-does-not-exist)
        (loop
         until (equal inchar 'eof)
         do
         (setf inchar (peek-char nil instream  nil 'eof))
         (cond
          ((equal inchar 'eof)
           (return))
          ;;if begins with paren, " read and write as a lisp object
          ((member inchar  '( #\(  #\" ) :test 'char-equal)
           (setf object (read instream nil 'eof))
           (write object :stream outstream))
          ((member inchar collect-chars-list)
           (setf inchar (read-char  instream nil 'eof)
                 collected-chars (append collected-chars (list inchar)))
           (write-char inchar  outstream))
          ((member inchar omit-chars-list)
           (setf inchar (read-char instream nil 'eof)
                 omitted-chars (append omitted-chars (list inchar))))
          (t (setf inchar (read-char instream nil 'eof))
             (write-char inchar  outstream)))
         ;;zzzz
         ;;end loop
         )
     ;;end with, with
    ))
 ;;returns last object with collected and omitted chars
 (values object collected-chars omitted-chars)
  ;;end let, 
  ))
;;TEST
;;  (my-read-append-file "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\test-read-file.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\test-write-file.lisp")
;;results, seemed to work on all kinds of objects and non-objects.
;;NOTE--DIDN'T TEST THE COLLECTED AND OMITTED ITEMS.
       
                                    


;;FIND-GLOBAL-VARIABLE-IN-FILE
;;2016
;;
;;ddd
(defun find-global-variable-in-file (var-sym pathname &key root 
                                             (set-var-to-value-p T)
                                             (max-exprs-n 1000)
                                             (test-car-list '(defparameter setf setq defvar)))
  "In U-files, opens (read-only) path and searches for a top-level defparameter, setq, or setf command and compares the variable to variable-sym.  If equal, then RETURNS (values var-value var-doc) if found, nil otherwise. When root, sets pathname to root-pathname."
  (let
      ((expr)
       (test-car)
       (var-value)
       (var-doc)
       ) 
    (when root
      (setf pathname (format nil "~A~A" root pathname)))
 
    (with-open-file ( in pathname :direction :input :if-does-not-exist nil)
      ;;(break "with")
      (loop
       for n from 1 to max-exprs-n
       do
       (setf expr (read in  nil 'eof-found))
       (cond
        ((equal expr 'eof-found)
         (return))
        ((listp expr) 
         (setf test-car (car expr))
         (when (and (member test-car test-car-list :test 'equal)
                    (equal (second expr) var-sym))
           (setf var-value (third expr)
                 var-doc (fourth expr))
          ;;(break "found")
           (return)
           ;;end when,cond
           ))
        (t nil))
       ;;end  loop, with-open-file
       ))

    (when (and var-value set-var-to-value-p)
      (cond
       ((equal (car var-value) 'quote)
        (setf var-value (second var-value))
        (set var-sym var-value))
       (t (set var-sym var-value))))

    (values var-value  var-doc)
    ;;end let, find-global-variable-in-file
    ))
;;TEST
;; (find-global-variable-in-file '*test-data  "csq-permdata.lisp"   :root "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\" )
;;works  (THIS IS TEST DATA)  "This is test docs"






;;MAKE-FORMATED-SAVE-TO-FILE-DATA
;;2018
;;ddd
(defun make-formated-save-to-file-data (symlist &key title
                                                (incl-date-p T) (return-formated-data-p T)
                                                (return-data-p T))
       "U-files. EVALS SYMS in symlist to created symval-lists.] RETURNS (values formated-datafile-string all-sym-data-lists)   If title, incl title.   Makes formated data with (setf sym '( data )) for symbols which eval to their data. Eg. ;;FOR DATE: Date: 2018/08/29 16:29:02
;; ELMSYMS=
 (setf *file-elmsyms  '(MOTHER FATHER BEST-M-FRIEND ...)) "
       (let
           ((all-sym-data-lists)
            (formated-datafile-string title)
            (date "")
            )
         (when incl-date-p (setf date ())
           (setf formated-datafile-string 
                 (format nil ";;DATE: ~A~%~A~%" (date-string) title)))
         (loop
          for dbsym in symlist
          do
          (let*
              ((data)
               (formated-data)
               )
            (when (and (symbolp dbsym) (boundp dbsym) )
              (setf data (eval dbsym))
              (when return-data-p
                (setf all-sym-data-lists (append all-sym-data-lists (list (list dbsym data)))))
              (when return-formated-data-p
                (setf formated-datafile-string
                      (format nil "~%~A  (setf ~A '~A)~% "  formated-datafile-string dbsym data)))
              ;;end outer when
              )
            ;;end let,loop
            ))
         (values formated-datafile-string all-sym-data-lists)
         ;;end let, make-formated-save-to-file-data
         ))
;;TEST
;; (setf  varlistx '(VX1 VX2) VX1 '(DATA 1) VX2 '(DATA 2))
;; (make-formated-save-to-file-data varlistx :title ";;VARLIST DATA")
;; works=
#|
"
;;DATE: 2018/08/29 17:06:01
;;VARLIST DATA
  (setf VX1 '(DATA 1))
   (setf VX2 '(DATA 2))
 "
((VX1 (DATA 1)) (VX2 (DATA 2)))
|#




;;MAKE-DIR-SYMS-FROM-PATHS
;;2020
;;ddd
(defun make-dir-syms-from-paths (pathnames  &key (not-from-files-p T ))
  "U-files   RETURNS    INPUT: can have slashes or not, can end with actual filename "
  (let*
      ((dirsyms)
       (dirsym-strs)
       (file-strs)
       )
    (loop
     for path in pathnames
     do
     (let*
         ((dir-path)          
          (path-no-slash)   
          (dirsym-str)
          (dirsym )
          )
       (cond
        ((or (null not-from-files-p) (dir-or-file-test path))
         (setf dir-path (directory-namestring  path)
               path-no-slash (car (delete-final-strings '("\\" "/") (list dir-path)))
               dirsym-str (file-namestring path-no-slash)
               dirsym (my-make-symbol dirsym-str))
       (setf dirsyms (append dirsyms (list dirsym))
             dirsym-strs (append dirsym-strs (list dirsym-str))))
        (T (setf file-strs (append file-strs (list path)))))
       ;;end let,loop
       ))
    (values  dirsyms dirsym-strs file-strs)
    ;;end let, make-dir-syms-from-paths
    ))
;;TEST
;; (make-dir-syms-from-paths '( "c:\\this\\sym1\\" "c:\\this\\sym2" "c:\\this\\sym3.lisp"))
;;works= (SYM1 THIS)   ("sym1" "this")   ("c:\\this\\sym3.lisp")




;;SET-VAR-FROM-FILE-OR-VALUE
;;2016
;;
;;ddd
(defun set-var-from-file-or-value (var-sym &optional pathname value
                                           &key root
                                           (max-exprs-n 1000)
                                           (test-car-list '(defparameter defvar setf setq)))
  "In U-files, If value, sets var-sym to value.  Otherwise sets var-sym to a top-level global variable (if found) in pathname [if on set-syms-list] RETURNS return-value).  If root, uses pathname as filename and appends to root."
  (let
      ((return-value)
       )
   
    (cond
     (value
      (setf return-value value))
     (t  (setf return-value 
            (find-global-variable-in-file var-sym  pathname
                                          :root root
                                          :max-exprs-n max-exprs-n
                                          :test-car-list test-car-list))))
    return-value
    ;;end let, set-var-from-file-or-value
    ))
;;TEST
;;  (set-var-from-file-or-value '*test-data  "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\csq-permdata.lisp")


;;ENSURE-END-SLASH
;;2019
;;ddd
(defun ensure-end-slash (pathname &key (not-filenames-p T))
  "U-files,  Ends string or path object with foreward slash. Uses win32::check-trailing-slash. When not-filenames-p, doesn't put at end of filenames. RETURNS: newpathstr"
    (let*
        ((string (namestring pathname))
         (period-n (search "." string))
         (newpathstr (cond ((and period-n  not-filenames-p)
                            string)
                           (t (win32::check-trailing-slash string))))                            
         )
     newpathstr
    ;;end let,ensure-end-slash
    ))
;;TEST
;; (ensure-end-slash "\\THIS\\THAT\\filename.txt")
;; works= "\\THIS\\THAT\\filename.txt"  
;; (ensure-end-slash "\\THIS\\THAT") 
;; works= "\\THIS\\THAT/"
;; (ensure-end-slash "\\THIS\\THAT\\") = "\\THIS\\THAT\\"
;;  (setf **testpathh (pathname "C:\\this\\that\\other\\testfile.txt"))
;;  = #P"C:/this/that/other/testfile.txt"
;; (ensure-end-slash  **testpathh)
;; works= "\\this\\that\\other\\" 




;;CLEAN-PATH-STR
;;2017
;;ddd
(defun clean-path-str (pathstr &key slash-type  enclose-in-quotes-p  
                               (remove-extra-quotes-p T) wildcard-p)
  "In U-files.  Substitutes #\/for  #\\  and removes any extra spaces at beginning or end of pathstr, which is necessary for proper paths in dos eg my-dos-dir. ENCLOSE-IN-QUOTES-P is for use in DOS-related situations where there is a space between parts of a dir or filename. NOTE: pathstr can end in a wild-card filename: eg \"*.lisp\" 
  SLASH-TYPE :BACK, :FORWARD, or NIL = no change.  When wildcard-p, sets all slashes to :back and enclose-in-quotes-p to T.
ALSO SEE: remove-outside-string-dbl-quotes func"
  (cond
   ((and wildcard-p (equal slash-type :back))
   (setf pathstr (substitute  #\\ #\/ pathstr)))
   ((and wildcard-p (equal slash-type :forward))
   (setf pathstr (substitute #\/  #\\  pathstr)))
   ((equal slash-type :forward)
   (setf pathstr (substitute #\/ #\\  pathstr)))
   (t nil))
  ;;delete spaces
   (setf pathstr (my-delete-first-spaces pathstr)
         pathstr (my-delete-last-spaces pathstr))
   (when  enclose-in-quotes-p 
     (setf pathstr (format nil "\"~A\"" pathstr)))
   (when remove-extra-quotes-p
     (setf pathstr (my-delete-first-spaces  (my-delete-last-spaces  pathstr :delete-others '(#\")) :delete-others '(#\"))))
   pathstr
   ;;end clean-path-str
   )
;;TEST
;; (clean-path-str " \"F:\\ \" " :remove-extra-quotes-p T :slash-type :forward)
;; works= "F:/"
;; (clean-path-str " \"F:\\ \" " :remove-extra-quotes-p T)
;; works= "F:\\"
;; (clean-path-str "  D:\\   ")  =  "D:/"
;; (clean-path-str "    D:\\1 THIS DIR\\2 THAT DIR\\MY FILE.LISP    ")
;; works= "D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP"
;;enclose-in-quotes-p
;; (clean-path-str "    D:\\1 THIS DIR\\2 THAT DIR\\MY FILE.LISP    " :enclose-in-quotes-p T)
;; works= "\"D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP\""
;; ;; (clean-path-str    "C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP" :slash-type :forward)
;;works= "C:/3-TS/LISP PROJECTS TS/MYUTILITIES/*.LISP"
;; (clean-path-str "C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP" :wildcard-p T :enclose-in-quotes-p t :remove-extra-quotes-p NIL :slash-type :forward)
;;works= "\"C:/3-TS/LISP PROJECTS TS/MYUTILITIES/*.LISP\""
;; (clean-path-str "C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP" :wildcard-p T :enclose-in-quotes-p t :remove-extra-quotes-p NIL :slash-type NIL)
;;works= "\"C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP\""


;; NOW REMOVE EXTRA QUOTES 
;; (clean-path-str " \"F:\\ \" " :remove-extra-quotes-p T)
;; works= "F:/"
;; OK for normal paths: (clean-path-str "F:\\ " :remove-extra-quotes-p T)
;; works= "F:/"
;;  (clean-path-str    "  \"C:/\"" :remove-extra-quotes-p T)  =  "C:/"

;;   (clean-path-str    "\"D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP\"" :remove-extra-quotes-p T)
;;  works=  "D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP"
;; what if no extra quotes? still works, see below.
;; ;;   (clean-path-str "D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP" :remove-extra-quotes-p T)  = "D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP"
;;
;;FOR WILDCARD PATHS
;;  (clean-path-str "  C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP   " :wildcard-path-p T)
;;works=  "\"C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP\""

;;TO REVERSE SLASHES
;;;; (clean-path-str "C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP")
;;works= "C:/3-TS/LISP PROJECTS TS/MYUTILITIES/*.LISP"

;; (MY-DOS-COMMAND (format nil "DIR \"~A\"" (clean-path-str "  C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP   " :wildcard-p T)) T)
;; WORKS:
#|"DIR \"C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES\\*.LISP\" 
 Volume in drive C is OS
 Volume Serial Number is 4058-766B

 Directory of C:\\3-TS\\LISP PROJECTS TS\\MYUTILITIES

08/03/2018  08:31 PM               339 0-CODE-TEMPLATES.lisp
05/08/2018  06:48 PM            15,903 ARC-curve-function-tests.lisp
08/11/2010  02:36 PM             2,837 space-show-arglist.lisp
.... ETC
08/10/2019  03:35 PM           255,774 U-tstring.lisp
              62 File(s)      4,451,956 bytes
               0 Dir(s)  381,029,384,192 bytes free
"     0|#


;; NO (MY-DOS-COMMAND (format nil "DIR \"~A\"" (clean-path-str "  C:/3-TS/LISP PROJECTS TS/MYUTILITIES/*.LISP   " )) T)
;;DOES NOT WORK DIR \"C:/3-TS/LISP PROJECTS TS/MYUTILITIES/*.LISP\" 

;;(in-package #:com.gigamonkeys.pathnames)
#|
(defun list-directory (dirname)
  "Return a list of the contents of the directory named by dirname.
Names of subdirectories will be returned in `directory normal
form'. Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept
wildcard pathnames; `dirname' should simply be a pathname that
names a directory. It can be in either file or directory form."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))

  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
    ;; form just the way we want.
    (directory wildcard)
    
    #+openmcl
    ;; OpenMCl by default doesn't return subdirectories at all. But
    ;; when prodded to do so with the special argument :directories,
    ;; it returns them in directory form.
    (directory wildcard :directories t)
            
    #+allegro
    ;; Allegro normally return directories in file form but we can
    ;; change that with the :directories-are-files argument.
    (directory wildcard :directories-are-files nil)
            
    #+clisp
    ;; CLISP has a particularly idiosyncratic view of things. But we
    ;; can bludgeon even it into doing what we want.
    (nconc 
     ;; CLISP won't list files without an extension when :type is
     ;; wild so we make a special wildcard for it.
     (directory wildcard)
     ;; And CLISP doesn't consider subdirectories to match unless
     ;; there is a :wild in the directory component.
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))




(defun file-exists-p (pathname)
  "Similar to CL:PROBE-FILE except it always returns directory names
in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."

  #+(or sbcl lispworks openmcl)
  ;; These implementations do "The Right Thing" as far as we are
  ;; concerned. They return a truename of the file or directory if it
  ;; exists and the truename of a directory is in directory normal
  ;; form.
  (probe-file pathname)

  #+(or allegro cmu)
  ;; These implementations accept the name of a directory in either
  ;; form and return the name in the form given. However the name of a
  ;; file must be given in file form. So we try first with a directory
  ;; name which will return NIL if either the file doesn't exist at
  ;; all or exists and is not a directory. Then we try with a file
  ;; form name.
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  ;; Once again CLISP takes a particularly unforgiving approach,
  ;; signalling ERRORs at the slightest provocation.

  ;; pathname in file form and actually a file      -- (probe-file file)      ==> truename
  ;; pathname in file form and doesn't exist        -- (probe-file file)      ==> NIL
  ;; pathname in dir form and actually a directory  -- (probe-directory file) ==> truename
  ;; pathname in dir form and doesn't exist         -- (probe-directory file) ==> NIL

  ;; pathname in file form and actually a directory -- (probe-file file)      ==> ERROR
  ;; pathname in dir form and actually a file       -- (probe-directory file) ==> ERROR
  (or (ignore-errors
        ;; PROBE-FILE will return the truename if file exists and is a
        ;; file or NIL if it doesn't exist at all. If it exists but is
        ;; a directory PROBE-FILE will signal an error which we
        ;; ignore.
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        ;; PROBE-DIRECTORY returns T if the file exists and is a
        ;; directory or NIL if it doesn't exist at all. If it exists
        ;; but is a file, PROBE-DIRECTORY will signal an error.
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))
    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))

|#


#|
(defun directory-pathname-p (p)
  "Is the given pathname the name of a directory? This function can
usefully be used to test whether a name returned by LIST-DIRECTORIES
or passed to the function in WALK-DIRECTORY is the name of a directory
in the file system since they always return names in `directory normal
form'."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))
|#



#|
(defun pathname-as-directory (name)
  "Return a pathname reperesenting the given pathname in
`directory normal form', i.e. with all the name elements in the
directory component and NIL in the name and type components. Can
not be used on wild pathnames because there's not portable way to
convert wildcards in the name and type into a single directory
component. Returns its argument if name and type are both nil or
:unspecific."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname 
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))
|#



#|
;;(walk-directory "

;;(setf xdirx "C:\\3-TS\\LISP PROJECTS\\MyProjects\\screensaver\\images")

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "Walk a directory invoking `fn' on each pathname found. If `test' is
supplied fn is invoked only on pathnames for which `test' returns
true. If `directories' is t invokes `test' and `fn' on directory
pathnames as well."
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
|#


;;hhh ------------------------------------------ HELP -----------------------------------------------------------

;;xxx  CL    SYSTEM:  FUNCTIONS    HELP ==================
#|
CALL-SYSTEM Function
Package system
Signature 
CALL-SYSTEM COMMAND &KEY CURRENT-DIRECTORY WAIT SHELL-TYPE =>
status
Arguments
 COMMAND   A string, a list of strings, a simple-vector of strings, or nil.
CURRENT-DIRECTORY  A string. Implemented only on Microsoft Windows.
WAIT A boolean.
SHELL-TYPE A string or nil.
|#



#|
(directory "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\*.lisp")
=> LISTS all files matching *.lisp in directory; output format is a PATH,
last in output list is ... #P"C:/TOM/LISP PROJECTS/lispworks projects/LispPlotter-master/fastregex.lisp") |#


;;----------------------------------- USING THE NAMESTRING FUNCTIONS ----------------------
;;(NAMESTRING  #P"C:/TOM/LISP PROJECTS/lispworks projects/LispCodeLibraries/DMcClain-vmath-master/matrix.lisp")
;;=>"C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispCodeLibraries\\DMcClain-vmath-master\\matrix.lisp" 
;;(FILE-NAMESTRING  "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp") => "plotter.lisp" 
;;(DIRECTORY-NAMESTRING  "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp")
;;note: no drive letter C: or filename in return
;;=> "\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\" 

;;(ENOUGH-NAMESTRING  "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp") => "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp" 
#|in all cases, and the result of enough-namestring is the shortest reasonable string that will satisfy this criterion.|#


;;;-------using the PATHNAME FUNCTIONS ------------------------
;;(PATHNAME-NAME  "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master")
;;   => "LispPlotter-master" 
;;(pathname-name "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp")  
;;=> "plotter" 

;;(PATHNAME-TYPE "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp")
;;=> "lisp" 
;;(PATHNAME-DIRECTORY "C:\\3-TS\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp")
;;=> (:ABSOLUTE "TOM" "LISP PROJECTS" "lispworks projects" "LispPlotter-master") 

#|
 (setq q (make-pathname :host "KATHY"
                        :directory "CHAPMAN" 
                        :name "LOGIN" :type "COM"))
=>  #P"KATHY::[CHAPMAN]LOGIN.COM"
 (PATHNAME-HOST q) =>  "KATHY"
 (PATHNAME-NAME q) =>  "LOGIN"
 (PATHNAME-TYPE q) =>  "COM"
|#

#|
(DIRECTORY "c:\\temp\\*.*")
 => (#P"c:/temp/test-photo1.jpg" #P"c:/temp/screensaverMP-problems notes.docx" #P"c:/temp/Backup of screensaverMP-problems notes.wbk" #P"c:/temp/ArtsCulture.pdf")
ARGUMENTS: 
(SYSTEM::PATHSPEC &REST SYSTEM::OPTIONS &KEY SYSTEM::TEST SYSTEM::DIRECTORIES SYSTEM::NON-ACCESSIBLE-AS-EMPTY SYSTEM::FLAT-FILE-NAMESTRING SYSTEM::RELATIVE-TO-COMMON-PATH (SYSTEM::LINK-TRANSPARENCY SYSTEM:*DIRECTORY-LINK-TRANSPARENCY*) SYSTEM::NON-EXISTENT-LINK-DESTINATIONS)
|#

#|(append '(a b c) (list '(d e f))) => (A B C (D E F)) 
(append (list  '(a b c)) (list '(d e f))) => ((A B C) (D E F)) 
(cdr '(a b c d)) = (B C D) 
(cadr  '(a b c d)) = b
(cadr '((a b c d))) = nil
(cdr '((a b c d))) = nil|#

#|
MACRO ARGUMENTS  -- DEFSTRUCTURING  = 
    SUBSTITUTING A LIST FOR A SINGLE ARGMENT-- SEE BELOW:
  IN EX 1 VAR-AND-RANGE is a list that is later separated into VAR START AND END; but can
  do same thing IN DEFMACRO BY 
  EX 2 just SUBSTITUTING A LIST (VAR START END) for VAR-AND-RANGE

EXAMPLE 1:
    (defmacro do-primes (var-and-range &rest body)
      (let ((var (first var-and-range))
            (start (second var-and-range))
            (end (third var-and-range)))
        `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
             ((> ,var ,end))
           ,@body)))
In a moment I’ll explain how the body generates the correct expansion; for now you can
just note that the variables var, start, and end each hold a value, extracted from var-and-range,
that’s then interpolated into the backquote expression that generates do-primes’s expansion.
However, you don’t need to take apart var-and-range “by hand” because macro parameter
lists are what are called destructuring parameter lists. Destructuring, as the name suggests,
involves taking apart a structure—in this case the list structure of the forms passed to a macro.
Within a destructuring parameter list, a simple parameter name can be replaced with a
nested parameter list. The parameters in the nested parameter list will take their values from
the elements of the expression that would have been bound to the parameter the list replaced.
For instance, you can replace var-and-range with a list (var start end), and the three elements
of the list will automatically be destructured into those three parameters.
Another special feature of macro parameter lists is that you can use &body as a synonym
for &rest. Semantically &body and &rest are equivalent, but many development environments
will use the presence of a &body parameter to modify how they indent uses of the macro—
typically &body parameters are used to hold a list of forms that make up the body of the macro.
So you can streamline the definition of do-primes and give a hint to both human readers
and your development tools about its intended use by defining it like this:
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
|#

;;---------------  USING PEEK-CHAR and SIMILAR FUNCTIONS ------
#|
Function PEEK-CHAR
SYNTAX: peek-char &optional peek-type input-stream eof-error-p eof-value recursive-p => char
ARGUMENTS AND VALUES:
peek-type---a character or t or nil.
input-stream---input stream designator. The default is standard input.
eof-error-p---a generalized boolean. The default is true.
eof-value---an object. The default is nil.
recursive-p---a generalized boolean. The default is false.
char---a character or the eof-value.
DESCRIPTION:
peek-char obtains the next character in input-stream without actually reading it, thus leaving the character to be read at a later time. It can also be used to skip over and discard intervening characters in the input-stream until a particular character is found.

If PEEK-TYPE IS NOT SUPPLIED OR NIL, peek-char returns the next character to be read from input-stream, WITHOUT ACTUALLY REMOVING IT FROM INPUT-STREAM. The next time input is done from input-stream, the character will still be there.
 IF PEEK-TYPE IS T, THEN PEEK-CHAR SKIPS OVER WHITESPACE[2] CHARACTERS, but NOT COMMENTS, and then performs the peeking operation on the next character. The last character examined, the one that starts an object, is not removed from input-stream.
 IF PEEK-TYPE IS A CHARACTER, THEN PEEK-CHAR SKIPS OVER INPUT CHARACTERS UNTIL A CHARACTER THAT IS CHAR= TO THAT CHARACTER IS FOUND; that character is left in input-stream.

If an end of file[2] occurs and eof-error-p is false, eof-value is returned.

If recursive-p is true, this call is expected to be embedded in a higher-level call to read or a similar function used by the Lisp reader.

When input-stream is an echo stream, characters that are only peeked at are not echoed. In the case that peek-type is not nil, the characters that are passed by peek-char are treated as if by read-char, and so are echoed unless they have been marked otherwise by unread-char.

EXAMPLES:
 (with-input-from-string (input-stream "    1 2 3 4 5")
    (format t "~S ~S ~S" 
            (peek-char t input-stream)
            (peek-char #\4 input-stream)
            (peek-char nil input-stream)))
>>  #\1 #\4 #\4
=>  NIL
|#

#|
;; LISP DIRCTORY FUNCTIONS
HARLEQUIN-COMMON-LISP CHANGE-DIRECTORY
LISPWORKS DELETE-DIRECTORY
COMMON-LISP DIRECTORY
COMMON-LISP DIRECTORY-NAMESTRING
COMMON-LISP-USER DIRECTORY-P
COMMON-LISP-USER DIRECTORY-PATHNAME-P
COMMON-LISP-USER DIRECTORY-WILDCARD
HARLEQUIN-COMMON-LISP FAST-DIRECTORY-FILES
HARLEQUIN-COMMON-LISP FDF-HANDLE-DIRECTORY-P
HARLEQUIN-COMMON-LISP FDF-HANDLE-DIRECTORY-STRING
LISPWORKS FILE-DIRECTORY-P
HARLEQUIN-COMMON-LISP GET-TEMP-DIRECTORY
HARLEQUIN-COMMON-LISP GET-WORKING-DIRECTORY

;;XXX   SYSTEM DIRECTORY FUNCTIONS
SYSTEM %FILE-DIRECTORY-P
SYSTEM %FILE-EXIST-AND-DIRECTORY-P
SYSTEM %GET-CURRENT-DIRECTORY-OF-DISK
SYSTEM %GET-CURRENT-DISK-AND-DIRECTORY
SYSTEM %PATHNAME-DIRECTORY
SYSTEM ADD-DIRECTORY-COMPONENT
SYSTEM ADD-NAME-TO-DIRECTORY
SYSTEM CHECK-FOR-DIRECTORY
SYSTEM CHECK-LISPWORKS-DIRECTORY
SYSTEM CONVERT-LOGICAL-PATHNAME-DIRECTORY-COMPONENT
SYSTEM CREATE-DIRECTORY
SYSTEM CURRENT-DIRECTORY
SYSTEM DIRECTORY-FILES
SYSTEM DIRECTORY-LINK-TRUENAME
SYSTEM DIRECTORY-LIST-FIRST-WILD-POSITION
SYSTEM DIRECTORY-NAMESTRING-1
SYSTEM DIRECTORY-PATHNAME
SYSTEM DIRECTORY-PATHNAME-P
SYSTEM DIRECTORY-TO-FILE-HACK
SYSTEM DO-DIRECTORY-WILD-INFERIORS
SYSTEM ENOUGH-NAMESTRING-DIRECTORY
SYSTEM FILTER-DIRECTORY-OUTPUT
SYSTEM GET-CURRENT-DIRECTORY-OF-DISK
SYSTEM GET-CURRENT-DISK-AND-DIRECTORY
SYSTEM GET-USER-PROFILE-DIRECTORY
SYSTEM IN-DIRECTORY
SYSTEM INTERNAL-FAST-DIRECTORY-FILES
SYSTEM IO-DIRECTORY
SYSTEM IO-GET-CURRENT-DIRECTORY-OF-DISK
SYSTEM IO-GET-CURRENT-DISK-AND-DIRECTORY
SYSTEM LICENSING-DIRECTORY
SYSTEM LISP-LIBRARY-DIRECTORY
SYSTEM LOGICAL-PATHNAME-DIRECTORY
SYSTEM LOOP-DIRECTORY-FILES-WINDOWS
SYSTEM LOW-IO-GET-CURRENT-DIRECTORY-OF-DISK
SYSTEM LOW-IO-GET-CURRENT-DISK-AND-DIRECTORY
SYSTEM MAKE-DIRECTORY
SYSTEM MAYBE-ADD-DIRECTORY
SYSTEM MAYBE-RESET-LISPWORKS-DIRECTORY
SYSTEM MERGE-ANY-NAME-TO-DIRECTORY
SYSTEM MERGE-DIRECTORY-TO-DIRECTORY
SYSTEM MERGE-NAME-TO-DIRECTORY
SYSTEM MERGE-WITH-LIBRARY-DIRECTORY
SYSTEM PARSE-LOGICAL-PATHNAME-DIRECTORY?
SYSTEM PROBE-FILE-NOT-DIRECTORY-P
SYSTEM RECURSE-SUB-MATCH-DIRECTORY-WILD-INFERIORS
SYSTEM REMOVE-REDUNDANT-BACKS-FROM-PATHNAME-DIRECTORY
SYSTEM REPLACE-WILDCARDS-IN-DIRECTORY
SYSTEM SET-TEMP-DIRECTORY
SYSTEM STRUCTURED-DIRECTORY-NAME
SYSTEM TRANSLATE-PATHNAME-DIRECTORY
SYSTEM WILD-DIRECTORY-DIRECTORY
SYSTEM WILD-DIRECTORY-NOT-DIRECTORY
SYSTEM WIN32-RECURSE-SUB-MATCH-DIRECTORY-DIRECTORY
SYSTEM WIN32-REMOVE-DIRECTORY
SYSTEM set %PATHNAME-DIRECTORY
SYSTEM set LOGICAL-PATHNAME-DIRECTORY
|#

#|
;;xxx ALL FUNCTIONS WITH "FILE" IN NAME =======================
HARLEQUIN-COMMON-LISP ADD-SYMBOL-PROFILER
LISPWORKS APPEND-FILE
COMMON-LISP-USER CLOSE-FILE-CALLBACK
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-CALLED
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-COUNTERS-COUNT
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-COUNTERS-EXECUTED
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-COUNTERS-HIDDEN
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-FULLY-COVERED
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-HIDDEN-COVERED
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-LAMBDAS-COUNT
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-NOT-CALLED
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-PARTIALLY-COVERED
HARLEQUIN-COMMON-LISP CODE-COVERAGE-FILE-STATS-SOURCE-FILE
COMMON-LISP-USER COMBINE-LISP-OBJECT-FILES
COMMON-LISP-USER COMBINE-TEXT-FILES
COMMON-LISP COMPILE-FILE
HARLEQUIN-COMMON-LISP COMPILE-FILE-IF-NEEDED
COMMON-LISP COMPILE-FILE-PATHNAME
LISPWORKS COPY-FILE
HARLEQUIN-COMMON-LISP CREATE-TEMP-FILE
COMMON-LISP DELETE-FILE
COMMON-LISP-USER DIR-OR-FILE-TEST
HARLEQUIN-COMMON-LISP DUMP-FORMS-TO-FILE
COMMON-LISP-USER ENSURE-SAVE-FILE-TO-DIR
LISPWORKS EXAMPLE-COMPILE-FILE
LISPWORKS EXAMPLE-EDIT-FILE
LISPWORKS EXAMPLE-FILE
LISPWORKS EXAMPLE-LOAD-BINARY-FILE
LISPWORKS EXIT-COMPILE-FILE
HARLEQUIN-COMMON-LISP FAST-DIRECTORY-FILES
COMMON-LISP FILE-AUTHOR
HARLEQUIN-COMMON-LISP FILE-BINARY-BYTES
COMMON-LISP-USER FILE-DATE
LISPWORKS FILE-DIRECTORY-P
COMMON-LISP FILE-ERROR-PATHNAME
COMMON-LISP-USER FILE-EXISTS-P
COMMON-LISP FILE-LENGTH
COMMON-LISP FILE-NAMESTRING
COMMON-LISP-USER FILE-P
COMMON-LISP-USER FILE-PATHNAME-P
COMMON-LISP FILE-POSITION
HARLEQUIN-COMMON-LISP FILE-READABLE-P
COMMON-LISP-USER FILE-SIZE
HARLEQUIN-COMMON-LISP FILE-STRING
COMMON-LISP FILE-STRING-LENGTH
HARLEQUIN-COMMON-LISP FILE-WRITABLE-P
COMMON-LISP FILE-WRITE-DATE
COMMON-LISP-USER FILEPATHNAME-P
COMMON-LISP-USER FILTER-FILE-EXT
COMMON-LISP-USER FIND-FILE-END
COMMON-LISP-USER FIND-GLOBAL-VARIABLE-IN-FILE
COMMON-LISP-USER FIND-MULTIPLE-FILES-COMMAND
COMMON-LISP-USER GET-FILE-DIR-NAME-TYPE
COMMON-LISP-USER GET-FILE-INFO
COMMON-LISP-USER GET-FILES-W-EXTS
COMMON-LISP-USER IS-FILE-POSITION-NOT-EOF
LISPWORKS LISPWORKS-FILE
COMMON-LISP-USER LIST-ALL-FILE-OBJECTS
COMMON-LISP-USER LIST-FILE-NESTED-LISTS
COMMON-LISP-USER LIST-FILE-OBJECTS
COMMON-LISP-USER LIST-FILES
COMMON-LISP-USER LOAD-CS-FILES
HARLEQUIN-COMMON-LISP LOAD-DATA-FILE
COMMON-LISP-USER LOAD-FILES
COMMON-LISP-USER LOAD-TOM-MAIN-UFILES
COMMON-LISP-USER MAKE-FILE-MENU1
COMMON-LISP-USER MAKE-FORMATED-SAVE-TO-FILE-DATA
HARLEQUIN-COMMON-LISP MAKE-TEMP-FILE
COMMON-LISP-USER MY-EDIT-DIR-FILES
COMMON-LISP-USER MY-EDIT-FILE
COMMON-LISP-USER MY-EDIT-FILES
COMMON-LISP-USER MY-FILE-POSITION
COMMON-LISP-USER MY-PROMPT-FOR-FILE
COMMON-LISP-USER MY-READ-APPEND-FILE
COMMON-LISP-USER MY-READ-FILES-APPEND-FILE
COMMON-LISP-USER OPEN-FILE-TO-EDIT-CALLBACK
COMMON-LISP-USER OPEN-LISP-OBJECT-FILE-TO-EDIT-CALLBACK
HARLEQUIN-COMMON-LISP OPEN-TEMP-FILE
COMMON-LISP-USER PATHNAME-AS-FILE
COMMON-LISP-USER PPRINT-OBJECT-TO-FILE
COMMON-LISP-USER PRINT-LIST-TO-FILE
COMMON-LISP-USER PRINT-NESTED-LIST-TO-FILE
COMMON-LISP PROBE-FILE
HARLEQUIN-COMMON-LISP PROFILE
COMMON-LISP-USER READ-FILE-CHARS
COMMON-LISP-USER READ-FILE-LINES
COMMON-LISP-USER READ-SETTINGS-FILE
COMMON-LISP-USER READ-TCP-FILE
HARLEQUIN-COMMON-LISP REMOVE-SYMBOL-PROFILER
COMMON-LISP RENAME-FILE
HARLEQUIN-COMMON-LISP RESET-PROFILER
COMMON-LISP-USER SAVE-CS-EXPLORE-DATA-TO-FILE
COMMON-LISP-USER SAVE-CSQ-DATA-TO-FILE
COMMON-LISP-USER SAVE-FILE-CALLBACK
COMMON-LISP-USER SAVE-REVISED-USER-FILE-DATA
COMMON-LISP-USER SELECT-DIR-FILE-CALLBACK
COMMON-LISP-USER SEPARATE-FILES-FROM-DIRS
COMMON-LISP-USER SEPARATE-SUBDIRS-FILES
COMMON-LISP-USER SET-DEFAULT-FIND-FILES-PATTERN
HARLEQUIN-COMMON-LISP SET-PROFILER-THRESHOLD
HARLEQUIN-COMMON-LISP SET-UP-PROFILER
COMMON-LISP-USER SET-VAR-FROM-FILE-OR-VALUE
COMMON-LISP-USER SORT-DIRECTORY-FILENAMES
COMMON-LISP-USER STORE-VAR-LISTS-TO-FILE
COMMON-LISP-USER STORE-VARS-TO-FILE
COMMON-LISP WITH-OPEN-FILE
HARLEQUIN-COMMON-LISP WITH-OUTPUT-TO-FASL-FILE
COMMON-LISP-USER WRITE-FORMAT-TO-FILE
COMMON-LISP-USER WRITE-TO-FILE
CAPI %WITH-METAFILE
CAPI CALL-FILE-PROMPTER
CAPI CAN-USE-METAFILE-P
CAPI CAPI-ABORT-EXTERNAL-METAFILE
CAPI CAPI-CLOSE-EXTERNAL-METAFILE
CAPI CAPI-CLOSE-INTERNAL-METAFILE
CAPI CAPI-OPEN-EXTERNAL-METAFILE
CAPI CAPI-OPEN-INTERNAL-METAFILE
CAPI CHECKED-DEFAULT-EXTERNAL-METAFILE-FORMAT
CAPI CHECKED-DEFAULT-INTERNAL-METAFILE-FORMAT
CAPI COPY-TIP-FILE-COMPLETION
CAPI DEFINE-FILE-PROMPTER
CAPI DELETE-INTERNAL-METAFILE
CAPI DRAW-METAFILE
CAPI DRAW-METAFILE-TO-IMAGE
CAPI ERROR--NOT-SUPPORTING-METAFILE
CAPI FREE-METAFILE
CAPI INTERFACE-UPDATE-DRAG-IMAGE-AND-FILENAME
CAPI MAKE-TIP-FILE-COMPLETION
CAPI METAFILE-COMPUTE-BOUNDS
CAPI PRINT-FILE
CAPI PROMPT-FOR-FILE
CAPI PROMPT-FOR-FILE-GET-CANONICAL-PATH
CAPI PROMPT-FOR-FILES
CAPI READ-SOUND-FILE
CAPI SAVE-INTERNAL-METAFILE
CAPI TEXT-INPUT-PANE-FILE-COMPLETE
CAPI TEXT-INPUT-PANE-FILE-COMPLETION-PREPARE
CAPI TEXT-INPUT-PANE-NON-FOCUS-FILE-COMPLETE
CAPI TIP-FILE-COMPLETION-CARET-POSITION
CAPI TIP-FILE-COMPLETION-DIRECTORIES-ONLY
CAPI TIP-FILE-COMPLETION-DIRECTORY
CAPI TIP-FILE-COMPLETION-IGNORE
CAPI TIP-FILE-COMPLETION-LOCATION
CAPI TIP-FILE-COMPLETION-P
CAPI TIP-FILE-COMPLETION-POSITION
CAPI WITH-EXTERNAL-METAFILE
CAPI WITH-INTERNAL-METAFILE
CAPI set TIP-FILE-COMPLETION-CARET-POSITION
CAPI set TIP-FILE-COMPLETION-LOCATION
CAPI set TIP-FILE-COMPLETION-POSITION
CAPI-LIBRARY ABORT-EXTERNAL-METAFILE
CAPI-LIBRARY ABORT-INTERNAL-METAFILE
CAPI-LIBRARY CLOSE-EXTERNAL-METAFILE
CAPI-LIBRARY CLOSE-INTERNAL-METAFILE
CAPI-LIBRARY CONVERT-AND-DELETE-INTERNAL-METAFILE
CAPI-LIBRARY DEFAULT-EXTERNAL-METAFILE-FORMAT
CAPI-LIBRARY DEFAULT-INTERNAL-METAFILE-FORMAT
CAPI-LIBRARY EXTERNAL-METAFILE-MODE
CAPI-LIBRARY INTERNAL-METAFILE-FORMAT
CAPI-LIBRARY LIBRARY-CAN-USE-METAFILE-P
CAPI-LIBRARY LIBRARY-DELETE-INTERNAL-METAFILE
CAPI-LIBRARY LIBRARY-PROMPT-FOR-FILE
CAPI-LIBRARY LIBRARY-PROMPT-FOR-FILES
CAPI-LIBRARY LIBRARY-SAVE-INTERNAL-METAFILE
CAPI-LIBRARY LIBRARY-VERSION-FOR-NON-SUPPORTING-METAFILE
CAPI-LIBRARY METAFILE-REQUIRE-BOUNDS-P
CAPI-LIBRARY OPEN-EXTERNAL-METAFILE
CAPI-LIBRARY OPEN-INTERNAL-METAFILE
CAPI-LIBRARY REPRESENTATION-DRAW-METAFILE
CAPI-LIBRARY REPRESENTATION-METAFILE-TO-IMAGE-REPRESENTATION
CAPI-LIBRARY REPRESENTATION-WITH-EXTERNAL-METAFILE
CAPI-LIBRARY REPRESENTATION-WITH-INTERNAL-METAFILE
CAPI-TOOLKIT COMPILE-FILE-LIST
CAPI-TOOLKIT FILE-INTERFACE-COMPILE-AND-LOAD-TITLE
CAPI-TOOLKIT FILE-INTERFACE-COMPILE-TITLE
CAPI-TOOLKIT FILE-INTERFACE-LOAD-TITLE
CAPI-TOOLKIT FILE-INTERFACE-PROMPTING-FOR-FILES
CAPI-TOOLKIT INTERFACE-COMPILE-AND-LOAD-FILES
CAPI-TOOLKIT INTERFACE-COMPILE-FILES
CAPI-TOOLKIT INTERFACE-FILE
CAPI-TOOLKIT INTERFACE-FILES
CAPI-TOOLKIT INTERFACE-LOAD-FILE
CAPI-TOOLKIT INTERFACE-LOAD-FILES
CAPI-TOOLKIT INTERFACE-OPERATION-ON-FILES
CAPI-TOOLKIT INTERFACE-PASTE-FILE
CAPI-TOOLKIT INTERFACE-PASTE-FILE-P
CAPI-TOOLKIT INTERFACE-PROMPT-FOR-FILE
CAPI-TOOLKIT INTERFACE-RECENT-FILES
CAPI-TOOLKIT INTERFACE-RECENT-FILES-ITEMS
CAPI-TOOLKIT INTERFACE-RECORD-FILE-OPEN-RECENT-FILE
CAPI-TOOLKIT INTERFACE-REVERT-FILE
CAPI-TOOLKIT INTERFACE-REVERT-FILE-P
CAPI-TOOLKIT MAKE-FILE-COMPILING-COMPONENT
CAPI-TOOLKIT MAKE-FILE-EDITING-FILE-COMPONENT
CAPI-TOOLKIT MAKE-FILE-MENU-COMPONENT
CAPI-TOOLKIT MAKE-FILE-PRINT-COMPONENT
CAPI-TOOLKIT MAKE-INTERFACE-FILE-COMPONENT
CAPI-TOOLKIT RECORD-NAMESTRING-IN-LIST-OF-FILES
CAPI-WIN32-LIB %FOREIGN-CALLABLE/browse_file_wndproc
CAPI-WIN32-LIB ALLOCA-OLD-METAFILE-DATA
CAPI-WIN32-LIB ALLOCA-PLACEABLE-METAFILE-DATA
CAPI-WIN32-LIB BROWSE-FILE-CALLBACK
CAPI-WIN32-LIB CLOSE-GDIP-METAFILE
CAPI-WIN32-LIB COPY-GDIP-METAFILE
CAPI-WIN32-LIB COPY-METAFILEPICT
CAPI-WIN32-LIB COPY-R-METAFILE-DEVICE
CAPI-WIN32-LIB COPY-R-METAFILE-PORT
CAPI-WIN32-LIB COPY-WINDOWS-ENHANCED-METAFILE
CAPI-WIN32-LIB CREATE-COPY-OF-HENHMETAFILE
CAPI-WIN32-LIB ENHANCED-METAFILE-TO-METAFILEPICT
CAPI-WIN32-LIB GDIP-METAFILE-P
CAPI-WIN32-LIB GET-CLIPBOARD-METAFILE
CAPI-WIN32-LIB GET-FILE-ICON-BITMAP
CAPI-WIN32-LIB GET-METAFILE-CLIPBOARD-TYPE-AND-HANDLE
CAPI-WIN32-LIB MAKE-GDIP-METAFILE
CAPI-WIN32-LIB MAKE-METAFILEPICT
CAPI-WIN32-LIB MAKE-R-METAFILE-DEVICE
CAPI-WIN32-LIB MAKE-R-METAFILE-PORT
CAPI-WIN32-LIB MAKE-WINDOWS-ENHANCED-METAFILE
CAPI-WIN32-LIB METAFILEPICT-P
CAPI-WIN32-LIB OPEN-FILE-DIRECTORY
CAPI-WIN32-LIB PROMPT-FOR-FILE-ARGS
CAPI-WIN32-LIB R-METAFILE-DEVICE-P
CAPI-WIN32-LIB R-METAFILE-OPEN-GDI-METAFILE
CAPI-WIN32-LIB R-METAFILE-PORT-BOUNDS
CAPI-WIN32-LIB R-METAFILE-PORT-CLIP-CACHE
CAPI-WIN32-LIB R-METAFILE-PORT-CURSOR
CAPI-WIN32-LIB R-METAFILE-PORT-DC
CAPI-WIN32-LIB R-METAFILE-PORT-DEVICE
CAPI-WIN32-LIB R-METAFILE-PORT-ELEMENT
CAPI-WIN32-LIB R-METAFILE-PORT-FILENAME
CAPI-WIN32-LIB R-METAFILE-PORT-FLAGS
CAPI-WIN32-LIB R-METAFILE-PORT-FORMAT
CAPI-WIN32-LIB R-METAFILE-PORT-GDIP-METAFILE
CAPI-WIN32-LIB R-METAFILE-PORT-GDIP-STUFF
CAPI-WIN32-LIB R-METAFILE-PORT-HDC
CAPI-WIN32-LIB R-METAFILE-PORT-HWND
CAPI-WIN32-LIB R-METAFILE-PORT-MESSAGE-TABLE
CAPI-WIN32-LIB R-METAFILE-PORT-NEXT-WND-PROC
CAPI-WIN32-LIB R-METAFILE-PORT-OWNER-REP
CAPI-WIN32-LIB R-METAFILE-PORT-P
CAPI-WIN32-LIB R-METAFILE-PORT-PARENT
CAPI-WIN32-LIB R-METAFILE-PORT-REFERENCE-HDC
CAPI-WIN32-LIB R-METAFILE-PORT-REFERENCE-HDC-FOR-GDIP
CAPI-WIN32-LIB R-METAFILE-PORT-X-ORG
CAPI-WIN32-LIB R-METAFILE-PORT-Y-ORG
CAPI-WIN32-LIB SAVE-INTERNAL-METAFILE-AS-WMF
CAPI-WIN32-LIB SET-APMFILEHEADER-CHECKSUM
CAPI-WIN32-LIB WIN32-CLOSE-METAFILE
CAPI-WIN32-LIB WIN32-DELETE-METAFILE
CAPI-WIN32-LIB WIN32-OPEN-METAFILE
CAPI-WIN32-LIB WINDOWS-ENHANCED-METAFILE-HDC-REF
CAPI-WIN32-LIB WINDOWS-ENHANCED-METAFILE-HENHMETAFILE
CAPI-WIN32-LIB WINDOWS-ENHANCED-METAFILE-P
CAPI-WIN32-LIB WRAP-METAFILE-IN-METAFILEPICT
CAPI-WIN32-LIB set R-METAFILE-PORT-BOUNDS
CAPI-WIN32-LIB set R-METAFILE-PORT-CLIP-CACHE
CAPI-WIN32-LIB set R-METAFILE-PORT-CURSOR
CAPI-WIN32-LIB set R-METAFILE-PORT-DC
CAPI-WIN32-LIB set R-METAFILE-PORT-DEVICE
CAPI-WIN32-LIB set R-METAFILE-PORT-ELEMENT
CAPI-WIN32-LIB set R-METAFILE-PORT-FILENAME
CAPI-WIN32-LIB set R-METAFILE-PORT-FLAGS
CAPI-WIN32-LIB set R-METAFILE-PORT-FORMAT
CAPI-WIN32-LIB set R-METAFILE-PORT-GDIP-METAFILE
CAPI-WIN32-LIB set R-METAFILE-PORT-GDIP-STUFF
CAPI-WIN32-LIB set R-METAFILE-PORT-HDC
CAPI-WIN32-LIB set R-METAFILE-PORT-HWND
CAPI-WIN32-LIB set R-METAFILE-PORT-MESSAGE-TABLE
CAPI-WIN32-LIB set R-METAFILE-PORT-NEXT-WND-PROC
CAPI-WIN32-LIB set R-METAFILE-PORT-OWNER-REP
CAPI-WIN32-LIB set R-METAFILE-PORT-PARENT
CAPI-WIN32-LIB set R-METAFILE-PORT-X-ORG
CAPI-WIN32-LIB set R-METAFILE-PORT-Y-ORG
CAPI-WIN32-LIB set WINDOWS-ENHANCED-METAFILE-HDC-REF
CAPI-WIN32-LIB set WINDOWS-ENHANCED-METAFILE-HENHMETAFILE
COMM BIO-NEW-FILE
COMM FREE-CACHED-PEM-THINGS-FILE
COMM RAND-LOAD-FILE
COMM READ-CERTIFICATE-FILE
COMM READ-PRIVATE-KEY-FILE
COMM READ-X509-INFO-FILE
COMM SELECT-FROM-FILE-LIST
COMM SSL-CTX-USE-CERTIFICATE-CHAIN-FILE
COMM SSL-CTX-USE-CERTIFICATE-FILE
COMM SSL-CTX-USE-PRIVATEKEY-FILE
COMM SSL-CTX-USE-RSAPRIVATEKEY-FILE
COMM SSL-LOAD-CLIENT-CA-FILE
COMM SSL-USE-CERTIFICATE-FILE
COMM SSL-USE-PRIVATEKEY-FILE
COMM SSL-USE-RSAPRIVATEKEY-FILE
COMPILER %MAKE-CODE-COVERAGE-FILE-OBJECT
COMPILER ADD-FILE-TO-CODE-COVERAGE-DATA
COMPILER CHECK-CODE-COVERAGE-FILE-OBJECTS-ARE-COMPATIBLE
COMPILER CODE-COVERAGE-COMPILE-TIME-OBJECT-FILE-OBJECT
COMPILER CODE-COVERAGE-DATA-GET-FILE
COMPILER CODE-COVERAGE-FILE-OBJECT-COMPILE-UNIVERSAL-TIME
COMPILER CODE-COVERAGE-FILE-OBJECT-FLAGS
COMPILER CODE-COVERAGE-FILE-OBJECT-FORMS-MAP
COMPILER CODE-COVERAGE-FILE-OBJECT-P
COMPILER CODE-COVERAGE-FILE-OBJECT-TRUENAME
COMPILER CODE-COVERAGE-FILE-STATS-FUNCTIONS
COMPILER CODE-COVERAGE-FILE-STATS-LAMBDAS
COMPILER CODE-COVERAGE-FILE-STATS-MACROS
COMPILER CODE-COVERAGE-FILE-STATS-ONE-SHOT
COMPILER CODE-COVERAGE-FILE-STATS-P
COMPILER CODE-COVERAGE-FILE-STATS-SUM
COMPILER CODE-COVERAGE-FILE-STATS-TRUENAME
COMPILER COMBINE-CODE-COVERAGE-FILE-OBJECTS
COMPILER COMBINE-FILE-COUNTER-MAPS
COMPILER COMPILE-FILE-VALUES
COMPILER COPY-CODE-COVERAGE-FILE-OBJECT
COMPILER COPY-CODE-COVERAGE-FILE-STATS
COMPILER DESTRUCTIVE-COPY-CODE-COVERAGE-FILE-OBJECT
COMPILER DESTRUCTIVE-COPY-FILE-COUNTER-MAPS
COMPILER DO-FILE-COMPILATION
COMPILER DUMPED-XREF-TABLES-FILE
COMPILER ENSURE-*CODE-EXECUTION-FILE-TABLE*
COMPILER IN-PROCESS-FORMS-IN-FILE
COMPILER INSTALL-VARIABLE-BOUND-PER-COMPILE-FILE
COMPILER INVOKE-COMPILE-FILE-PROGRESS-HOOK
COMPILER LOAD-TIME-SETUP-CODE-EXECUTION-FILE-OBJECT
COMPILER MAKE-CODE-COVERAGE-FILE-OBJECT
COMPILER MAKE-CODE-COVERAGE-FILE-STATS
COMPILER MAP-FILE-OBJECT-CCLDS
COMPILER NOTE-START-OF-FILE
COMPILER PRINT-CODE-COVERAGE-FILE-OBJECT
COMPILER PRINT-CODE-COVERAGE-FILE-STATS
COMPILER PROCESS-FORMS-IN-FILE
COMPILER RAW-COPY-CODE-COVERAGE-FILE-OBJECT
COMPILER REMOVE-VARIABLE-BOUND-PER-COMPILE-FILE
COMPILER RESET-CODE-COVERAGE-FILE-OBJECT
COMPILER RESTORE-XREF-FROM-FILE
COMPILER set CODE-COVERAGE-FILE-OBJECT-COMPILE-UNIVERSAL-TIME
COMPILER set CODE-COVERAGE-FILE-OBJECT-FILE-WRITE-DATE
COMPILER set CODE-COVERAGE-FILE-OBJECT-FLAGS
COMPILER set CODE-COVERAGE-FILE-OBJECT-FORMS-MAP
COMPILER set CODE-COVERAGE-FILE-STATS-FUNCTIONS
COMPILER set CODE-COVERAGE-FILE-STATS-LAMBDAS
COMPILER set CODE-COVERAGE-FILE-STATS-MACROS
COMPILER set CODE-COVERAGE-FILE-STATS-ONE-SHOT
COMPILER set CODE-COVERAGE-FILE-STATS-TRUENAME
CONDITIONS COMPILER-ERROR-FILE
CONDITIONS FILE-ENCODING-RESOLUTION-ERROR-PRINTER
CONDITIONS FILE-OPERATION-ERROR
CONDITIONS FILE-OPERATION-ERROR-ERRNO
CONDITIONS FILE-OPERATION-ERROR-OPERATION
CONDITIONS FILE-STREAM-ERROR-ERRNO
CONDITIONS FILE-STREAM-ERROR-READ
CONDITIONS ILLEGAL-FILE-STREAM-OPERATION-FUNCTION
CONDITIONS REPORT-FILE-STREAM-ERROR
DBG EXECUTABLE-LOG-FILE
DBG LISPWORKS-DEBUG-INFO-FILE
DBG READ-CFO-PC-DEBUG-INFO-FROM-FILE
DSPEC SAME-SOURCE-FILE-P
EDITOR ADD-TO-*PATHNAMES-IN-FIND-FILE-BUFFER*
EDITOR ALL-SOURCE-FILES-IN-DIRECTORY
EDITOR APPEND-REGION-TO-FILE
EDITOR APPEND-TO-FILE-COMMAND
EDITOR APPEND-TO-WORD-ABBREV-FILE-COMMAND
EDITOR BACKGROUND-LOAD-FILE
EDITOR BACKUP-FILE-COMMAND
EDITOR BUFFER-FILE-WRITABLE-P
EDITOR CCO-HTML-FILE-CALLBACK
EDITOR CCO-HTML-OUTPUT-FILE-LINE
EDITOR CCO-HTML-WRITE-ALL-FILE-LINES
EDITOR CCO-MAYBE-CALL-FILE-CALLBACK
EDITOR CHECK-FILENAME-FEASIBILITY
EDITOR CHECKPOINT-FILE-EXTERNAL-FORMAT
EDITOR CLEAR-CKP-FILES
EDITOR CODE-COVERAGE-FILE-COMMAND
EDITOR CODE-COVERAGE-FILE-OBJECT-COLOR-BUFFER
EDITOR CODE-COVERAGE-GENERATE-HTML-FOR-FILE-OBJECT
EDITOR CODE-COVERAGE-WRITE-INDEX-FILE-HEADER
EDITOR COMPARE-FILE-AND-BUFFER-COMMAND
EDITOR COMPILE-AND-LOAD-BUFFER-FILE
EDITOR COMPILE-AND-LOAD-BUFFER-FILE-COMMAND
EDITOR COMPILE-AND-LOAD-FILE-COMMAND
EDITOR COMPILE-BUFFER-FILE-COMMAND
EDITOR COMPILE-FILE-COMMAND
EDITOR COMPILE-FILE-INTERNAL
EDITOR COMPILE-FILE-WITH-MESSAGE
EDITOR COMPLETE-FILE-INFO-P
EDITOR COMPLETE-FILE-TRY-IF-ALREADY-COMPLETING
EDITOR COMPLETE-FILENAME-WITH-NON-FOCUS
EDITOR COMPLETE-FILENAME-WITH-NON-FOCUS-COMPLETER
EDITOR COMPLETE-FILENAME-WITH-NON-FOCUS-UPDATER
EDITOR COPY-COMPLETE-FILE-INFO
EDITOR DEFINE-FILE-TYPE-HOOK
EDITOR DEFINITION-LINES-IN-FILE
EDITOR DELETE-CKP-FILES
EDITOR DELETE-FILE-AND-KILL-BUFFER-COMMAND
EDITOR DELETE-FILE-COMMAND
EDITOR DIALOG-PROMPT-FOR-FILE
EDITOR DIRECTORY-MODE-ADVANCE-TO-FILENAME
EDITOR DIRECTORY-MODE-DELETE-DELETED-LINES-FILES
EDITOR DIRECTORY-MODE-DO-DELETE-FILES
EDITOR DIRECTORY-MODE-DO-MOVE-OR-COPY-FILES
EDITOR DIRECTORY-MODE-EDIT-FILE-COMMAND
EDITOR DIRECTORY-MODE-EDIT-FILE-IN-OTHER-WINDOW-COMMAND
EDITOR DIRECTORY-MODE-MOVE-OR-COPY-MARKED-LINES-FILES
EDITOR DIRECTORY-MODE-POINT-FILENAME
EDITOR DISPLAY-COMPILE-FILE-STATUS
EDITOR ECHO-COMPLETE-FILE
EDITOR ECHO-NON-FOCUS-COMPLETE-FILE
EDITOR ECHO-NON-FOCUS-COMPLETE-FILE-1
EDITOR EDITOR-COMPLETE-FILE
EDITOR EDITOR-FILE-MODE
EDITOR EDITOR-PRINT-FILE
EDITOR EMERGENCY-SAVE-ALL-FILES
EDITOR EXPAND-FILE-NAME-COMMAND
EDITOR EXPAND-FILE-NAME-WITH-SPACE-COMMAND
EDITOR FILE-BUFFER-P
EDITOR FILE-COMPLETEION-NON-MATCH-REGEXP
EDITOR FILE-IN-TAGS-BUFFER
EDITOR FILE-PROMPT-VERIFY
EDITOR FILES-IN-TAGS-FILE
EDITOR FILTER-INTERESTING-SEARCH-FILES
EDITOR FIND-ALTERNATE-FILE-COMMAND
EDITOR FIND-DSPEC-IN-FILE
EDITOR FIND-FILE-BUFFER
EDITOR FIND-FILE-BUFFER-VERBOSE
EDITOR FIND-FILE-COMMAND
EDITOR FIND-FILE-IN-BUFFER-LIST
EDITOR FIND-FILE-WITH-EXTERNAL-FORMAT-COMMAND
EDITOR FIND-LINE-IN-FILE
EDITOR FIND-TAGGED-FILE
EDITOR GET-FILE-LENGTH
EDITOR I-COMPLETE-FILENAME-WITH-NON-FOCUS-COMPLETER
EDITOR IN-PLACE-EXPAND-FILE-NAME-COMMAND
EDITOR IN-PLACE-EXPAND-FILE-NAME-WITH-SPACE-COMMAND
EDITOR IN-WRITE-FILE
EDITOR INSERT-FILE-COMMAND
EDITOR INTERNAL-DIRECTORY-MODE-EDIT-FILE
EDITOR LOAD-FILE-COMMAND
EDITOR MAKE-BACKUP-FILENAME
EDITOR MAKE-COMPLETE-FILE-INFO
EDITOR MAKE-FILES-FOR-DIFF
EDITOR MAYBE-BACKUP-FILE
EDITOR NON-IGNORED-FILE-P
EDITOR NON-IGNORED-FILES-IN-DIRECTORY
EDITOR OCCURRENCES-OF-STRING-IN-FILE
EDITOR OCCURRENCES-OF-STRING-IN-FILES
EDITOR PARSE-CODE-COVERAGE-FILE-OBJECT
EDITOR PRINT-FILE-COMMAND
EDITOR PROCESS-FILE-OPTIONS
EDITOR PROCESS-FILE-OPTIONS-COMMAND
EDITOR PROMPT-FOR-FILE
EDITOR PROMPT-FOR-FILE-IN-DEFAULT-DIRECTORY
EDITOR PROMPT-FOR-TAGS-FILE
EDITOR QUERY-DELETE-FILE
EDITOR QUERY-REPLACE-FILES
EDITOR READ-BIG-FILE
EDITOR READ-CHECKPOINT-FILE
EDITOR READ-DA-FILE
EDITOR READ-FILE
EDITOR READ-FILE-HANDLER-TRY-ANOTHER-FORMAT
EDITOR READ-FILE-INTO-BUF
EDITOR READ-FILE-LIST
EDITOR READ-WORD-ABBREV-FILE-COMMAND
EDITOR REMOVE-FROM-*PATHNAMES-IN-FIND-FILE-BUFFER*
EDITOR RENAME-FILE-COMMAND
EDITOR SAVE-ALL-FILES-AND-EXIT-COMMAND
EDITOR SAVE-ALL-FILES-COMMAND
EDITOR SAVE-FILE-COMMAND
EDITOR SEARCH-FILES
EDITOR SEARCH-FILES-COMMAND
EDITOR SEARCH-FILES-MATCHING-PATTERNS-COMMAND
EDITOR SET-BUFFER-WRITABLE-FROM-FILE
EDITOR SOURCE-FILE-P
EDITOR STRING-DIRECTORY-MODE-FILENAME
EDITOR TAGS-FILE-P
EDITOR TAGS-PARSE-ONE-FILE
EDITOR UN-KILL-AS-FILENAME-COMMAND
EDITOR UPDATE-BUFFER-FILE-WRITE-INFO
EDITOR UPDATE-EVAL/COMPILE-RECORD-AFTER-COMPILE-FILE
EDITOR VERIFY-FILE-NAME-FOR-CODE-COVERAGE
EDITOR VISIT-FILE-COMMAND
EDITOR VISIT-TAGS-FILE-COMMAND
EDITOR WFIND-FILE-COMMAND
EDITOR WINDOW-SAVE-ALL-FILE
EDITOR WITH-TILDE-COMPLETE-FILE
EDITOR WRITE-CHECKPOINT-FILE
EDITOR WRITE-DA-FILE
EDITOR WRITE-FILE-COMMAND
EDITOR WRITE-OR-APPEND-REGION-TO-FILE
EDITOR WRITE-REGION-TO-FILE
EDITOR WRITE-WORD-ABBREV-FILE-COMMAND
ENVIRONMENT PROMPT-FOR-FILE
ENVIRONMENT-INTERNALS ENVIRONMENT-PROMPT-FOR-FILE
FLI CONVERT-MODULE-FILE-NAME-TO-STRING
FLI GET-MODULE-FILE-NAME
FLI LOW-GET-MODULE-FILE-NAME
FLI MODULE-FILE-NAME
FOREIGN PROCESS-C-FILE
FOREIGN PROCESS-CPP-FILE
FOREIGN PROCESS-FOREIGN-FILE
GRAPHICS-PORTS CALL-GDIP-CREATE-BITMAP-FROM-FILE
GRAPHICS-PORTS CALL-GDIP-CREATE-METAFILE-FROM-EMF
GRAPHICS-PORTS CALL-GDIP-RECORD-METAFILE
GRAPHICS-PORTS GDIP-CREATE-BITMAP-FROM-FILE
GRAPHICS-PORTS GDIP-CREATE-METAFILE-FROM-EMF
GRAPHICS-PORTS GDIP-GET-HEMF-FROM-METAFILE
GRAPHICS-PORTS GDIP-GET-METAFILE-HEADER-FROM-METAFILE
GRAPHICS-PORTS GDIP-LOAD-IMAGE-FROM-FILE
GRAPHICS-PORTS GDIP-RECORD-METAFILE
GRAPHICS-PORTS GDIP-RECORD-METAFILE-FILE-NAME
GRAPHICS-PORTS GDIP-SAVE-IMAGE-TO-FILE
GRAPHICS-PORTS INITIALIZE-GDIP-METAFILE
GRAPHICS-PORTS METAFILE-POINTER-GET-HEMF-COPY
GRAPHICS-PORTS METAFILE-RESET-REPRESENTATION-GDIP-STUFF-GRAPHICS
GRAPHICS-PORTS READ-DIB-FILE
GRAPHICS-PORTS READ-IMAGE-FILE
GRAPHICS-PORTS REPRESENTATION-DRAW-METAFILE-POINTER
GRAPHICS-PORTS WRITE-GDIP-IMAGE-TO-FILE
HQN-WEB BUILD-LIBRARY-INTRO-FILE
HQN-WEB BUILD-LOCATION-INTRO-FILE
HQN-WEB DELETE-ALL-TEMP-WEB-FILES
HQN-WEB FILE-PREFIX-END
HQN-WEB FILE-TO-VIEW
HQN-WEB LIBRARY-INTRO-FILE
HQN-WEB LIST-PAGE-FILES
HQN-WEB MAKE-TEMP-WEB-FILE
HQN-WEB RESULT-LINE-FILE
HQN-WEB SEARCH-EXAMPLES-FILES-FOR-STRING
HQN-WEB SEARCH-RESULT-FILE
HQN-WEB SIMPLIFY-FILE-URL
LISPWORKS-TOOLS %PRINT-ERROR-BROWSER-FILE
LISPWORKS-TOOLS AD-ASK-FOR-OUTPUT-FILE-AND-ARGS
LISPWORKS-TOOLS AD-UPDATE-SAVED-FILE-SIZE
LISPWORKS-TOOLS ADD-CODE-COVERAGE-ITEM-FILE-TO-CCD
LISPWORKS-TOOLS ADD-FILE-BUFFER-BREAKPOINT
LISPWORKS-TOOLS ADD-FILE-BUFFER-REGION-POTENTIAL-BREAKPOINT
LISPWORKS-TOOLS ADD-FILE-TO-DIRECTORY-SEARCH
LISPWORKS-TOOLS CALL-WITH-BREAKPOINT-AT-FILE-BUFFER-POINT
LISPWORKS-TOOLS CALL-WITH-BREAKPOINT-AT-VALID-FILE-BUFFER-POINT
LISPWORKS-TOOLS CALL-WITH-FILE-BUFFER-BREAKPOINT-REGION
LISPWORKS-TOOLS CAPTURE-USER-FILES-TO-EDIT
LISPWORKS-TOOLS CHECK-KILL-STEPPER-FILE-BUFFER
LISPWORKS-TOOLS CHECK-STEPPER-FILE-REDEFINITION
LISPWORKS-TOOLS COPY-DSI-FILE-SPEC
LISPWORKS-TOOLS COPY-DSI-TV-FILE-SPEC
LISPWORKS-TOOLS COPY-ERROR-BROWSER-FILE
LISPWORKS-TOOLS COPY-LIST-PROFILE-INFO
LISPWORKS-TOOLS COPY-PROFILE-INFO
LISPWORKS-TOOLS COPY-PROFILER-STATE
LISPWORKS-TOOLS COPY-TREE-PROFILE-INFO
LISPWORKS-TOOLS CREATE-ROOTS-FROM-MATCHED-FILES
LISPWORKS-TOOLS DIRECTORY-SEARCH-FILES-EDIT
LISPWORKS-TOOLS DIRECTORY-SEARCH-UPDATE-ALL-FILES-P
LISPWORKS-TOOLS DSI-ALL-FILES-P
LISPWORKS-TOOLS DSI-ALL-FILES1-BUTTON
LISPWORKS-TOOLS DSI-ALL-FILES2-BUTTON
LISPWORKS-TOOLS DSI-FILE-SPEC-P
LISPWORKS-TOOLS DSI-MATCHED-FILES
LISPWORKS-TOOLS DSI-MATCHED-FILES-P
LISPWORKS-TOOLS DSI-NEW-FILES
LISPWORKS-TOOLS DSI-TV-FILE-SPEC-P
LISPWORKS-TOOLS DSI-TV-FILE-SPEC-PATHNAME
LISPWORKS-TOOLS EDIT-SEARCH-FILES-CALLBACK
LISPWORKS-TOOLS EDIT-USER-FILES
LISPWORKS-TOOLS ENSURE-FILE-BUFFER-BREAKPOINT-CONTEXT-FOR-DSPEC
LISPWORKS-TOOLS ERROR-BROWSER-FILE-P
LISPWORKS-TOOLS ERROR-TREE-UNHIDE-FILE-CONDITIONS
LISPWORKS-TOOLS EXPAND-MOST-RECENT-ERROR-TREE-FILES
LISPWORKS-TOOLS FILE-BUFFER-BREAKPOINT-AT-POINT
LISPWORKS-TOOLS FILE-BUFFER-EDIT-BREAKPOINTS
LISPWORKS-TOOLS FILES-ERROR-TREE-DELETE-CALLBACK
LISPWORKS-TOOLS FILES-ERROR-TREE-POPUP-MENU
LISPWORKS-TOOLS FILES-ERRORS-TREE-CHILDREN
LISPWORKS-TOOLS FILES-ERRORS-TREE-PRINT
LISPWORKS-TOOLS FIND-FILE-BUFFER-CONTEXT-FOR-DSPEC
LISPWORKS-TOOLS I-PROFILER-MAKE-TREE-INFO-A
LISPWORKS-TOOLS I-PROFILER-SET-CURRENT-FUNCTION-AS-ROOT
LISPWORKS-TOOLS IDE-SAVE-IMAGE-MAKE-FILE-NAME
LISPWORKS-TOOLS INTERFACE-FILE-FOR-OBJECT
LISPWORKS-TOOLS INTERFACE-INSERT-FILE
LISPWORKS-TOOLS INTERFACE-INSERT-FILE-P
LISPWORKS-TOOLS INTERFACE-PROMPT-FOR-FILE-AND-CALL
LISPWORKS-TOOLS LISPWORKS-WIN32-MDI-PODIUM-FILES-TO-EDIT
LISPWORKS-TOOLS LIST-PROFILE-INFO-P
LISPWORKS-TOOLS LOCATE-FILE-BUFFER-POINT
LISPWORKS-TOOLS MAKE-DSI-FILE-SPEC
LISPWORKS-TOOLS MAKE-DSI-TV-FILE-SPEC
LISPWORKS-TOOLS MAKE-EDITOR-FILE-MISC-MENU-COMPONENT
LISPWORKS-TOOLS MAKE-ERROR-BROWSER-FILE
LISPWORKS-TOOLS MAKE-FILE-EXIT-MENU
LISPWORKS-TOOLS MAKE-FILES-ERROR-TREE-POPUP-MENU
LISPWORKS-TOOLS MAKE-LIST-PROFILE-INFO
LISPWORKS-TOOLS MAKE-PROFILE-INFO
LISPWORKS-TOOLS MAKE-PROFILER-ROOT-MENU-COMPONENT
LISPWORKS-TOOLS MAKE-PROFILER-STATE
LISPWORKS-TOOLS MAKE-PROFILER-TREE--MENU
LISPWORKS-TOOLS MAKE-SYSTEM-BROWSER-SHOW-HIDE-FILE-MENU-ITEM
LISPWORKS-TOOLS MAKE-TREE-PROFILE-INFO
LISPWORKS-TOOLS MAP-FILE-BUFFER-BREAKPOINTS
LISPWORKS-TOOLS MAYBE-UPDATE-SEARCH-FILES-INTERFACE
LISPWORKS-TOOLS MODULE-IS-FILE-P
LISPWORKS-TOOLS NEW-REGISTERED-INIT-FILENAME-P
LISPWORKS-TOOLS NOTE-USER-FILE-TO-EDIT
LISPWORKS-TOOLS POPUP-PROFILER-RESULTS-MENU
LISPWORKS-TOOLS PRINC-PROFILE-COUNT
LISPWORKS-TOOLS PRINT-PROFILE-INFO
LISPWORKS-TOOLS PRINT-PROFILER-STATE
LISPWORKS-TOOLS PROFILE-INFO-CALL-COUNT
LISPWORKS-TOOLS PROFILE-INFO-CHILDREN
LISPWORKS-TOOLS PROFILE-INFO-DSPEC
LISPWORKS-TOOLS PROFILE-INFO-FILTERED-CHILDREN
LISPWORKS-TOOLS PROFILE-INFO-NODE
LISPWORKS-TOOLS PROFILE-INFO-OBJECT
LISPWORKS-TOOLS PROFILE-INFO-ON-STACK-COUNT
LISPWORKS-TOOLS PROFILE-INFO-P
LISPWORKS-TOOLS PROFILE-INFO-SEEN-PROMIL
LISPWORKS-TOOLS PROFILE-INFO-TOP-OF-STACK-COUNT
LISPWORKS-TOOLS PROFILE-UPDATE-DESCRIPTION
LISPWORKS-TOOLS PROFILER-AFTER-PROFILE-CODE
LISPWORKS-TOOLS PROFILER-CALL-TREE-ROOT
LISPWORKS-TOOLS PROFILER-CAN-DO-NODE-AS-ROOT
LISPWORKS-TOOLS PROFILER-CAN-SET-FUNCTION-AS-ROOT
LISPWORKS-TOOLS PROFILER-CODE
LISPWORKS-TOOLS PROFILER-COERCE-TO-STATE
LISPWORKS-TOOLS PROFILER-COERCE-TO-STATE-P
LISPWORKS-TOOLS PROFILER-COLLAPSE-SINGLETONS
LISPWORKS-TOOLS PROFILER-COLUMN-FUNCTION
LISPWORKS-TOOLS PROFILER-CUTOFF
LISPWORKS-TOOLS PROFILER-DEFAULT-STATE
LISPWORKS-TOOLS PROFILER-EDIT-FUNCTION
LISPWORKS-TOOLS PROFILER-ENSURE-SET-UP-PROFILER-UP-TO-DATE
LISPWORKS-TOOLS PROFILER-GET-PROFILER-RESULTS
LISPWORKS-TOOLS PROFILER-GET-START-PROFILING-ARGS-FROM-DIALOG
LISPWORKS-TOOLS PROFILER-INFO-COMPUTE-DSPEC
LISPWORKS-TOOLS PROFILER-INSIDE-PROFILING-P
LISPWORKS-TOOLS PROFILER-KW-IS-LOADED-P
LISPWORKS-TOOLS PROFILER-MAKE-TREE-INFO-A
LISPWORKS-TOOLS PROFILER-MARK-TREES-NEED-UPDATE
LISPWORKS-TOOLS PROFILER-NAME-THE-CURRENT-TREE
LISPWORKS-TOOLS PROFILER-NEW-LIST-SELECTED-ITEM
LISPWORKS-TOOLS PROFILER-NEW-SELECTED-ITEM
LISPWORKS-TOOLS PROFILER-NEW-TREE-P
LISPWORKS-TOOLS PROFILER-NEW-TREE-SELECTED-ITEM
LISPWORKS-TOOLS PROFILER-NOT-PROFILING-P
LISPWORKS-TOOLS PROFILER-PRINT-FUNCTION
LISPWORKS-TOOLS PROFILER-PRINT-PERCENTAGE
LISPWORKS-TOOLS PROFILER-PROFILE-CODE
LISPWORKS-TOOLS PROFILER-PROFILE-CODE-AUX
LISPWORKS-TOOLS PROFILER-PROFILING
LISPWORKS-TOOLS PROFILER-RAISE-SET-UP-PROFILER-DIALOG
LISPWORKS-TOOLS PROFILER-RAISE-START-PROFILING-DIALOG
LISPWORKS-TOOLS PROFILER-RESULTS
LISPWORKS-TOOLS PROFILER-SAMPLE-COUNT
LISPWORKS-TOOLS PROFILER-SAVE-CURRENT-TREE
LISPWORKS-TOOLS PROFILER-SELECTED-FUNCTIONS
LISPWORKS-TOOLS PROFILER-SET-CALL-TREE-ROOT
LISPWORKS-TOOLS PROFILER-SET-CALL-TREE-ROOT-TO-NODE
LISPWORKS-TOOLS PROFILER-SET-CALLS-TO-CURRENT-FUNCTION-AS-ROOT
LISPWORKS-TOOLS PROFILER-SET-CURRENT-FUNCTION-AS-ROOT
LISPWORKS-TOOLS PROFILER-SET-ENABLED-STATE
LISPWORKS-TOOLS PROFILER-SET-FROM-FROM-FILE
LISPWORKS-TOOLS PROFILER-SET-FROM-FROM-INTERNAL-TREE
LISPWORKS-TOOLS PROFILER-SET-FROM-PROMPTED-FILENAME
LISPWORKS-TOOLS PROFILER-SET-TREE-SHOWING-STRING
LISPWORKS-TOOLS PROFILER-SHOWING-PROPER-TREE-P
LISPWORKS-TOOLS PROFILER-STACKED-GRAPH-NODE-FUNCTION
LISPWORKS-TOOLS PROFILER-START-PROFILING-ARGS
LISPWORKS-TOOLS PROFILER-START-PROILING-DIALOG-CHECK
LISPWORKS-TOOLS PROFILER-STATE
LISPWORKS-TOOLS PROFILER-STATE-CODE
LISPWORKS-TOOLS PROFILER-STATE-FILENAME
LISPWORKS-TOOLS PROFILER-STATE-LIST-SELECTED
LISPWORKS-TOOLS PROFILER-STATE-NAME
LISPWORKS-TOOLS PROFILER-STATE-P
LISPWORKS-TOOLS PROFILER-STATE-PROFILER-SETUP
LISPWORKS-TOOLS PROFILER-STATE-RESULTS
LISPWORKS-TOOLS PROFILER-STATE-SAMPLE-COUNT
LISPWORKS-TOOLS PROFILER-STATE-STYLE
LISPWORKS-TOOLS PROFILER-STATE-SUMMARY
LISPWORKS-TOOLS PROFILER-STATE-TREE
LISPWORKS-TOOLS PROFILER-STATE-TREE-RESULTS
LISPWORKS-TOOLS PROFILER-STATE-TREE-SELECTED
LISPWORKS-TOOLS PROFILER-STOP-PROFILE-CODE
LISPWORKS-TOOLS PROFILER-STOP-PROFILING-AND-IMPORT
LISPWORKS-TOOLS PROFILER-SUMMARY-STRING
LISPWORKS-TOOLS PROFILER-TOGGLE-DESCRIPTION
LISPWORKS-TOOLS PROFILER-TOOL-EXTENDED-TREE-NODE-STRING
LISPWORKS-TOOLS PROFILER-TOOL-P
LISPWORKS-TOOLS PROFILER-TOOL-STACKED-TREE-COLOR-FUNCTION
LISPWORKS-TOOLS PROFILER-TREE-EDGE-PANE-FUNCTION
LISPWORKS-TOOLS PROFILER-TREE-FILE-CALLBACK
LISPWORKS-TOOLS PROFILER-TREE-RESULTS
LISPWORKS-TOOLS PROFILER-UPDATE-MESSAGE
LISPWORKS-TOOLS PROFILER-UPDATE-VIEW
LISPWORKS-TOOLS PROFILER-UPDATED-VIEWS
LISPWORKS-TOOLS PROFILER-VIEW-DESCRIPTION-P
LISPWORKS-TOOLS PROMPT-FOR-CONCATENATED-FASL-FILENAME
LISPWORKS-TOOLS REGISTERED-INIT-FILENAME-OR-DEFAULT
LISPWORKS-TOOLS REMOVE-ALL-FILE-BUFFER-BREAKPOINTS
LISPWORKS-TOOLS REMOVE-ALL-FILE-BUFFER-REGION-BREAKPOINTS
LISPWORKS-TOOLS REMOVE-FILE-BUFFER-BREAKPOINT
LISPWORKS-TOOLS REMOVE-FILE-BUFFER-REGION-POTENTIAL-BREAKPOINT
LISPWORKS-TOOLS REMOVE-RECENT-FILES
LISPWORKS-TOOLS RESET-RECENT-FILES
LISPWORKS-TOOLS REVERT-FILES-IN-EDITOR
LISPWORKS-TOOLS SAFE-PROBE-FILE
LISPWORKS-TOOLS SAVE-BUFFER-OF-FILE
LISPWORKS-TOOLS SAVE-RECENT-FILES
LISPWORKS-TOOLS SB-TV-ITEM-IS-FILE-P
LISPWORKS-TOOLS SB-TV-SYSTEM-BROWSER-OPEN-FILE
LISPWORKS-TOOLS SEARCH-A-SINGLE-FILE
LISPWORKS-TOOLS SET-PROFILER-TOOL-PROFILING-PARAMETERS-DIALOG-PROCESSES
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-OK-CHECK
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-PACKAGES-OPTIONS-CALLBACK
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-SELECT-CONTEXT
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-SELECT-PACKAGES
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-SELECT-SYMBOLS
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-SET-ARG-LIST
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-SETUP-CONTEXTS
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-SETUP-PACKAGES
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-SETUP-SYMBOLS
LISPWORKS-TOOLS SET-UP-PROFILER-DIALOG-UPDATE
LISPWORKS-TOOLS SHOW-STEPPER-BREAKPOINTS-IN-FILE-BUFFER
LISPWORKS-TOOLS STEPPER-BUFFER-FILE-BUFFER
LISPWORKS-TOOLS SYSTEM-BROWSER-CONCATENTATE-SYSTEMS-TO-FILE
LISPWORKS-TOOLS SYSTEM-BROWSER-OPEN-MODULE-FILE
LISPWORKS-TOOLS SYSTEM-BROWSER-POPUP-FILES-MENU
LISPWORKS-TOOLS SYSTEM-BROWSER-SELECTED-FILE-NODES
LISPWORKS-TOOLS SYSTEM-BROWSER-SHOW-FILES
LISPWORKS-TOOLS TREE-PROFILE-INFO-P
LISPWORKS-TOOLS TREE-PROFILE-INFO-SEEN-PROMIL-STRING
LISPWORKS-TOOLS UPDATE-DSI-TV-FILE-SPEC
LISPWORKS-TOOLS UPDATE-PROFILER-STATE
LISPWORKS-TOOLS UPDATE-SEARCH-FILES-INTERFACE
LISPWORKS-TOOLS UPDATE-SEARCH-FILES-INTERFACE-AND-RECORD
LISPWORKS-TOOLS set PROFILE-INFO-CHILDREN
LISPWORKS-TOOLS set PROFILE-INFO-NODE
LISPWORKS-TOOLS set PROFILE-INFO-SEEN-PROMIL
LISPWORKS-TOOLS set PROFILER-STATE-CODE
LISPWORKS-TOOLS set PROFILER-STATE-FILENAME
LISPWORKS-TOOLS set PROFILER-STATE-LIST-SELECTED
LISPWORKS-TOOLS set PROFILER-STATE-NAME
LISPWORKS-TOOLS set PROFILER-STATE-PROFILER-SETUP
LISPWORKS-TOOLS set PROFILER-STATE-RESULTS
LISPWORKS-TOOLS set PROFILER-STATE-SAMPLE-COUNT
LISPWORKS-TOOLS set PROFILER-STATE-STYLE
LISPWORKS-TOOLS set PROFILER-STATE-SUMMARY
LISPWORKS-TOOLS set PROFILER-STATE-TREE
LISPWORKS-TOOLS set PROFILER-STATE-TREE-RESULTS
LISPWORKS-TOOLS set PROFILER-STATE-TREE-SELECTED
LW-GT COPY-OD-METAFILE-CACHE
LW-GT DESTROY-PINBOARD-OBJECTS-DISPLAYER-METAFILE
LW-GT MAKE-OD-METAFILE-CACHE
LW-GT OD-METAFILE-CACHE-P
SCM ADD-FILE-TYPE
SCM BINARY-SCM-FILE-P
SCM C-COMPILE-FILE
SCM C-FILE-EMBEDDED-MODULE
SCM C-FILE-FLL-EXPORTS
SCM C-LOAD-FILE
SCM C-LOAD-OR-GRAB-FILE
SCM CANONICALIZE-FILE-NAME
SCM COMPILE-TEMP-OBJECT-FILE
SCM CONCATENATE-C-OBJECT-FILE
SCM CONCATENATE-OBJECT-FILE
SCM CONCATENATE-OBJECT-FILE-1
SCM CONCATENATE-SOURCE-FILE
SCM FILE-DIRECTORY
SCM FILE-HOST
SCM FILE-LOAD-TIME
SCM FILE-LOADED-AS
SCM FILE-ONCE-ONLY
SCM FILE-PATHNAME-TYPE
SCM FILE-WRITE-TIME
SCM GET-FILE-TIME
SCM GET-FILE-TYPE
SCM INVOKE-WITH-CONCATENATED-FASL-FILE
SCM LOAD-PATCH-FILE
SCM MAKE-SCM-FILE-LOAD-PLAN
SCM SCM-COMPILE-FILE
SCM SYSTEM-MODULES-FOR-FILE
SCM SYSTEMS-CONTAINING-FILE
SCM WRITE-FILE-INTO-FASL-IN
SETF "HQN-WEB" "SEARCH-RESULT-FILE"
SETF "LISPWORKS-TOOLS" "DSI-ALL-FILES-P"
SETF "LISPWORKS-TOOLS" "DSI-MATCHED-FILES"
SETF "LISPWORKS-TOOLS" "DSI-MATCHED-FILES-P"
SETF "LISPWORKS-TOOLS" "DSI-NEW-FILES"
SETF "LISPWORKS-TOOLS" "PROFILER-CALL-TREE-ROOT"
SETF "LISPWORKS-TOOLS" "PROFILER-COLLAPSE-SINGLETONS"
SETF "LISPWORKS-TOOLS" "PROFILER-CUTOFF"
SETF "LISPWORKS-TOOLS" "PROFILER-DEFAULT-STATE"
SETF "LISPWORKS-TOOLS" "PROFILER-PROFILING"
SETF "LISPWORKS-TOOLS" "PROFILER-SHOWING-PROPER-TREE-P"
SETF "LISPWORKS-TOOLS" "PROFILER-START-PROFILING-ARGS"
SETF "LISPWORKS-TOOLS" "PROFILER-UPDATED-VIEWS"
SETF "LISPWORKS-TOOLS" "SYSTEM-BROWSER-SHOW-FILES"
SETF "SCM" "C-FILE-EMBEDDED-MODULE"
SETF "SCM" "C-FILE-FLL-EXPORTS"
SETF "SCM" "FILE-DIRECTORY"
SETF "SCM" "FILE-HOST"
SETF "SCM" "FILE-LOAD-TIME"
SETF "SCM" "FILE-LOADED-AS"
SETF "SCM" "FILE-ONCE-ONLY"
SETF "SCM" "FILE-PATHNAME-TYPE"
SETF "STREAM" "%L1FILE-STREAM-FILE-HANDLE"
SETF "STREAM" "EF-STREAM-INPUT-BUFFER-FILE-POSITIONS"
SETF "STREAM" "EF-STREAM-INPUT-EOL-FILE-POSITION"
SETF "STREAM" "FILE-STREAM-PATH"
SETF "STREAM" "L1FILE-STREAM-BYTE-SIZE"
SETF "STREAM" "L1FILE-STREAM-EOL-STYLE"
SETF "STREAM" "L1FILE-STREAM-EXTERNAL-FORMAT"
SETF "STREAM" "L1FILE-STREAM-FILE-INDICES"
SETF "STREAM" "L1FILE-STREAM-FLAG"
SETF "STREAM" "L1FILE-STREAM-PATH"
SETF "STREAM" "L1FILE-STREAM-TRANSACTION-TYPE"
SETF "STREAM" "L1FILE-STREAM-UNDERLYING-FILE-POSITION"
SETF "STREAM" "OS-FILE-HANDLE-STREAM-BYTE-SIZE"
SETF "STREAM" "OS-FILE-HANDLE-STREAM-FILE-HANDLE"
SETF "STREAM" "OS-FILE-HANDLE-STREAM-TRANSACTION-TYPE"
SETF "STREAM" "STREAM-FILE-POSITION"
SETF "SYSTEM" "REGISTERED-INIT-FILENAME"
STREAM %CLOSE-FILE
STREAM %CREATE-FILE
STREAM %FILE-LENGTH
STREAM %L1FILE-STREAM-FILE-HANDLE
STREAM %OPEN-FILE
STREAM %PROBE-FILE
STREAM %READ-FILE
STREAM %SET-FILE-POSITION
STREAM %WRITE-FILE
STREAM CANONICALIZE-FILE-ELEMENT-TYPE
STREAM CHANGE-BINARY-FILE-STREAM-ELEMENT-TYPE
STREAM CONVERT-BINARY-FILE-STREAM-TO-LATIN-1
STREAM EF-FILE-STRING-LENGTH
STREAM EF-STREAM-INPUT-BUFFER-FILE-POSITIONS
STREAM EF-STREAM-INPUT-EOL-FILE-POSITION
STREAM FILE-STREAM-PATH
STREAM FILE-STREAM-STRING
STREAM FILE-STRING*
STREAM GUESS-FILE-EXTERNAL-FORMAT
STREAM INIT-LATIN-1-FILE-STREAM-EOL-STYLE
STREAM L1CLOSE-FILE-STREAM-HANDLE
STREAM L1FILE-READ-BYTE-16BIT
STREAM L1FILE-READ-BYTE-32BIT
STREAM L1FILE-READ-BYTE-8BIT
STREAM L1FILE-READ-BYTE-SIGNED-32BIT
STREAM L1FILE-STREAM-BYTE-SIZE
STREAM L1FILE-STREAM-EOL-STYLE
STREAM L1FILE-STREAM-EXTERNAL-FORMAT
STREAM L1FILE-STREAM-FILE-INDICES
STREAM L1FILE-STREAM-FLAG
STREAM L1FILE-STREAM-PATH
STREAM L1FILE-STREAM-TRANSACTION-TYPE
STREAM L1FILE-STREAM-UNDERLYING-FILE-POSITION
STREAM L1FILE-WRITE-BYTE-16BIT
STREAM L1FILE-WRITE-BYTE-32BIT
STREAM L1FILE-WRITE-BYTE-8BIT
STREAM LATIN-1-FILE-STREAM-P
STREAM LATIN-1-LF-FILE-STRING
STREAM OS-FILE-HANDLE-STREAM-BYTE-SIZE
STREAM OS-FILE-HANDLE-STREAM-FILE-HANDLE
STREAM OS-FILE-HANDLE-STREAM-P
STREAM OS-FILE-HANDLE-STREAM-TRANSACTION-TYPE
STREAM PATHNAME-DESIGNATOR-FILE-HANDLE
STREAM SET-FILE-STREAM-DELETE-ON-CLOSE
STREAM SIGNAL-FILE-ERROR
STREAM SIGNAL-FILE-STREAM-ERROR
STREAM STREAM-BINARY-INPUT-FAST-FILE-POSITION-FUNCTION
STREAM STREAM-FILE-POSITION
STREAM STREAM-FILE-STRING-LENGTH
STREAM WIN-FILE-WRITE-ERROR
SYSTEM %FILE-DIRECTORY-P
SYSTEM %FILE-EXIST-AND-DIRECTORY-P
SYSTEM ASK-THE-USER-FOR-FILE
SYSTEM BAD-SET-FILE-POSITION
SYSTEM BLOCK-LOAD-INIT-FILES
SYSTEM CHECK-FILE-ATTRIBUTES
SYSTEM CLOSE-C-FILE-STREAM
SYSTEM CLOSE-FASL-FILE
SYSTEM CLOSE-FILE-HANDLE
SYSTEM CODE-PAGE-FILE-ENCODING
SYSTEM COMPLETE-FILE
SYSTEM COND-CLOSE-C-FILE-STREAM
SYSTEM CREATE-FILE-STREAM
SYSTEM CREATE-FILE-STREAM-FOR-TERMINAL
SYSTEM CREATE-LWUPDATER-COMMAND-FILE
SYSTEM DEMAND-LOAD-OBJECT-FILE
SYSTEM DETECT-JAPANESE-ENCODING-IN-FILE
SYSTEM DIRECTORY-FILES
SYSTEM DIRECTORY-TO-FILE-HACK
SYSTEM DOCUMENTATION-FILENAME
SYSTEM ENSURE-TEMP-FILE-BASE
SYSTEM EXAMPLE-TARGET-FILE
SYSTEM FASL-IN-FILE
SYSTEM FILE-COMPARE
SYSTEM FILE-INFO-EF-FOREIGN-SIZE
SYSTEM FILE-LINK-P
SYSTEM FILE-NAMESTRING-STRING
SYSTEM FILE-POSITION-ERROR
SYSTEM FILE-SIZE
SYSTEM FILE-STREAM-P
SYSTEM FILE-STREAM-SEEK
SYSTEM FILETIME-TO-INTERNAL-TIME
SYSTEM FIND-DOCUMENTATION-IN-FILE
SYSTEM FIND-FILENAME-PATTERN-ENCODING-MATCH
SYSTEM FIND-LISP-FASL-FILE
SYSTEM FIND-LISP-TEXT-FILE
SYSTEM FLI-SET-FILE-POSITION
SYSTEM FORCE-CREATE-AND-OPEN-NEW-FILE
SYSTEM GET-PROFILE-INFO
SYSTEM GET-PROFILER-RESULTS
SYSTEM GET-RUN-FILE-NAME
SYSTEM GET-USER-PROFILE-DIRECTORY
SYSTEM IN-COPY-FILE
SYSTEM INITIALISE-FASL-FILE
SYSTEM INTERNAL-CREATE-TEMP-FILE
SYSTEM INTERNAL-DYNAMIC-MODULE-DELETE-FILE
SYSTEM INTERNAL-FAST-DIRECTORY-FILES
SYSTEM INTERNAL-OPEN-TEMP-FILE
SYSTEM INVOKE-WITH-OPEN-FASL-FILE
SYSTEM INVOKE-WITH-OPEN-FILE
SYSTEM IO-CHECK-FILE-ATTRIBUTE
SYSTEM IO-CLOSE-FILE
SYSTEM IO-CREATE-FILE
SYSTEM IO-FILE-LENGTH
SYSTEM IO-GET-FILE-WRITE-DATE
SYSTEM IO-GET-FILE-WRITE-DATE-FROM-NAME
SYSTEM IO-GET-TIME-AS-FILE-TIME-FROM-OFFSET
SYSTEM IO-GET-TIME-AS-FILE-TIME-FROM-OFFSET-1
SYSTEM IO-OPEN-FILE
SYSTEM IO-RENAME-FILE
SYSTEM IO-SET-FILE-DATES
SYSTEM IO-SET-FILE-POSITION
SYSTEM LOAD-FASL-FILE
SYSTEM LOAD-FILE-WITH-RESTARTS
SYSTEM LOAD-INIT-FILE
SYSTEM LOAD-INIT-FILES
SYSTEM LOAD-TEXT-FILE
SYSTEM LOCALE-FILE-ENCODING
SYSTEM LOOKUP-UNICODE-NAME-IN-FILE
SYSTEM LOOP-DIRECTORY-FILES-WINDOWS
SYSTEM LOW-IO-CHECK-FILE-ATTRIBUTE
SYSTEM LOW-IO-CLOSE-FILE
SYSTEM LOW-IO-OPEN-FILE
SYSTEM LWUPDATER-CREATE-FILES
SYSTEM MATCH-FILE-NAMESTRING-WITH-WILD-NAME
SYSTEM MATCH-FILE-NAMESTRING-WITH-WILD-TYPE
SYSTEM MODULE-FILE-NAMES
SYSTEM OPEN-FASL-FILE
SYSTEM OPEN-FILE
SYSTEM OPEN-FILE-STREAM-HANDLE
SYSTEM PROBE-FILE-NOT-DIRECTORY-P
SYSTEM PROMPT-FOR-FILE
SYSTEM RECORD-FILE-STREAM
SYSTEM REGISTERED-INIT-FILENAME
SYSTEM REPLACE-THE-FILE-NAME
SYSTEM RESET-ALL-STACK-PROFILER-IDS
SYSTEM SAFE-LOCALE-FILE-ENCODING
SYSTEM SAVE-IMAGE-DEFAULT-EXE-FILE
SYSTEM SEARCH-FOR-FILE
SYSTEM SEARCH-FOR-FILE-WITH-TYPES
SYSTEM SET-FILE-DATES
SYSTEM SET-FILE-OWNERS
SYSTEM SPECIFIC-VALID-FILE-ENCODING
SYSTEM TERMINATE-FASL-FILE
SYSTEM TOP-LOOP-END-OF-FILE
SYSTEM TRANS-WITH-OPEN-FILE-INTERNAL
SYSTEM UNRECORD-FILE-STREAM
SYSTEM WIN32-DELETE-FILE
SYSTEM WIN32-SUB-MATCH-A-FILE
SYSTEM WITH-OPEN-FILE-INTERNAL
WIN32 %EF-GET-MODULE-FILE-NAME
WIN32 %FOREIGN-CALLABLE/prompt_for_file_wnd_proc
WIN32 CALL-CREATE-FILE
WIN32 CLOSE-ENH-META-FILE
WIN32 CREATE-ENH-META-FILE
WIN32 CREATE-FILE
WIN32 DECODE-WIN32-FILE-TIME
WIN32 DELETE-ENH-META-FILE
WIN32 DELETE-FILE-OR-DIRECTORY
WIN32 EF-GET-MODULE-FILE-NAME
WIN32 EXISTING-FILE-WRITABLE-P
WIN32 FILE-DIALOG
WIN32 FILE-DIALOG-RESULT-FN
WIN32 FILE-IS-GUI-APPLICATION-P
WIN32 FILE-TIME-TO-SYSTEM-TIME
WIN32 FILETIME-TO-UNIVERSAL-TIME
WIN32 FIND-FIRST-FILE
WIN32 FIND-FIRST-FILE-EX
WIN32 FIND-NEXT-FILE
WIN32 FLUSH-FILE-BUFFERS
WIN32 GET-ENH-META-FILE-BITS
WIN32 GET-ENH-META-FILE-HEADER
WIN32 GET-FILE-ATTRIBUTES
WIN32 GET-FILE-ATTRIBUTES-EX
WIN32 GET-FILE-INFORMATION-BY-HANDLE
WIN32 GET-FILE-INODE-FROM-HANDLE
WIN32 GET-FILE-INODE-FROM-NAME
WIN32 GET-FILE-SIZE
WIN32 GET-FILE-SIZE-BY-ATTRIBUTES
WIN32 GET-FILE-SUBSYSTEM-AND-MAGIC
WIN32 GET-FILE-TIME
WIN32 GET-FILE-TYPE
WIN32 GET-OPEN-FILE-NAME
WIN32 GET-PRIVATE-PROFILE-INT
WIN32 GET-PRIVATE-PROFILE-STRING
WIN32 GET-PROFILE-STRING
WIN32 GET-PROFILE-STRINGS
WIN32 GET-SAVE-FILE-NAME
WIN32 GET-SYSTEM-TIME-AS-FILE-TIME
WIN32 GET-USER-PROFILE-DIRECTORY
WIN32 GET-WIN-META-FILE-BITS
WIN32 GETPRIVATEPROFILESTRING
WIN32 GETPROFILESTRING
WIN32 LOAD-CURSOR-FROM-FILE
WIN32 LOOP-ON-FILES
WIN32 MOVE-FILE
WIN32 MOVE-FILE-EX
WIN32 MULTIPLE-FILES-RESULT-FN
WIN32 PLAY-ENH-META-FILE
WIN32 PROMPT-FOR-FILE
WIN32 PROMPT-FOR-FILES
WIN32 READ-FILE
WIN32 READ-ICON-FILE-DATA
WIN32 RICH-TEXT-READ-BYTES-FROM-FILE
WIN32 RICH-TEXT-READ-FILE
WIN32 RICH-TEXT-WRITE-BYTES-TO-FILE
WIN32 RICH-TEXT-WRITE-FILE
WIN32 SET-ENH-META-FILE-BITS
WIN32 SET-FILE-POINTER
WIN32 SET-FILE-TIME
WIN32 SET-META-FILE-BITS-EX
WIN32 SYSTEM-TIME-TO-FILE-TIME
WIN32 UNIVERSAL-TIME-TO-FILETIME
WIN32 WRITE-FILE
WIN32 WRITE-PRIVATE-PROFILE-STRING
|#


;;OLD VERSION, WOULD NOT RECURSE ON SUBDIRS
#|(defun my-dos-dir (path &key (last-leveln 0)
                        wildcard-p attributes format  sort-order
                        directories-p lowercase-p  
                        read-only-p hidden-p system-files-p not-content-indexed-p 
                        archive-ready-p 
                        bare-p owner-p  4-digit-years-p reparse-points-p (show-cmd-p t)
                        ;;(file-names-first-p t) doesn't work
                        return-dos-output-t return-all-lines-p  return-line-lists-p
                        (max-lines 25000) dos-out-prefix)
  "U-files,  RETURNS  (values cur-dir host volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out all-line-lists)
   INPUT: note:  must incl \\ eg  c:\\ (not c: ) or scans current dir.
   WILDCARD-P  is when path includes eg. \"*.lisp\".
   ATTRIBUTES [d=directories, r=read-only files, h=hidden files, a=files ready for archiving, s=system files, i=not content indexed files, l=reparse points,  - =prefix meaning not.] 
   FORMAT: is :wide, :col (same as wide, but by cols), or nil.
   SUBDIR-FILES-P: displays files in specified directory and all subdirectories.   
   BARE-P= (no headings), /-c=no thousand sep, /d=sorted by col,      
   SORT-ORDER:  :name, :size (smallest first), :extension, :dir-first, :create-date, :last-access,  :last-access,:last-write [date/time (oldest first)]   -  =prefix to reverse order.
   NOTE: following not used:  /r=display alternate data streams of the file,   /x=this displays the short names generated for non-8dot3 file,  /p=pauses after each screenful of information, 
   RETURN-LINE-LISTS-P returns all lines as lists of tokens. "
  (let
      ((command-str "dir ")
       (com-rest-str " ")
       (attribute-str "/a: ")
       (format-str " ")
       (sort-order-str " /o: ")
       )

    ;;STEP 1: MAKE THE DIR COMMAND STRING

    #|       (attribute-keys-values '((directories-p "d")(read-only-p "r")(hidden-p "h")
                                (archive-ready-p "a")(system-files-p "s")(not-content-indexed-p "i")
                                (reparse-points-p "l")))|#
    ;;(format-keys-values '((:wide "/w")(:col "/d")))
              
    #|subdir-files-p: displays files in specified directory and all subdirectories.   
   bare-p= (no headings), /-c=no thousand sep, /d=sorted by col,      
   sort-order:  n=by name (alphabetic), s=by size (smallest first), e=by extension (alphabetic), d=by date/time (oldest first), g=group directories first,  -  =prefix to reverse order.
   sort-time-field: :create :last-access, or :last-write (tme field used for sorting).      
|#    
    ;;make attribute-str
    (setf attribute-str
          (when-key-add-value directories-p t " d " :begin-str attribute-str)
          attribute-str
          (when-key-add-value read-only-p t " r " :begin-str attribute-str)
          attribute-str
          (when-key-add-value hidden-p t " h " :begin-str attribute-str)   
          attribute-str
          (when-key-add-value archive-ready-p t " a " :begin-str attribute-str)
          attribute-str
          (when-key-add-value system-files-p t " s " :begin-str attribute-str)
          attribute-str
          (when-key-add-value not-content-indexed-p t " i " :begin-str attribute-str)
          attribute-str
          (when-key-add-value reparse-points-p t " l " :begin-str attribute-str))

    ;;make format-str 
    (cond
     ((equal format :wide)
      (setf format-str " /w "))
     ((equal format :col)
      (setf format-str " /d "))
     )

    ;;make sort-order-str
    ;;n=by name (alphabetic), s=by size (smallest first), e=by extension (alphabetic), d=by date/time (oldest first), g=group directories first
    (when sort-order
      (cond
       ((equal sort-order :name)
        (setf sort-order-str  " /o: n "))
       ((equal sort-order :size)
        (setf sort-order-str  " /o: s "))
       ((equal sort-order :extension)
        (setf sort-order-str  " /o: e "))
       ((equal sort-order :dir-first)
        (setf sort-order-str  " /o: g "))
       ;;sort-time-field: :create :last-access, or :last-write (tme field used for sorting).
       ;;should i incl /o too??
       ((equal sort-order :create-date)
        (setf sort-order-str  " /t: c "))
       ((equal sort-order :last-access)
        (setf sort-order-str  " /t: a "))
       ((equal sort-order :last-write)
        (setf sort-order-str  " /t: w "))
       (t nil)))

    ;;lowercase-p   
    (when lowercase-p (setf rest-attr-str (format nil "~a /l " rest-attr-str)))
    ;;bare-p
    (when bare-p (setf rest-attr-str (format nil "~a /b " rest-attr-str)))
    ;;owner-p
    (when owner-p (setf rest-attr-str (format nil "~a /q " rest-attr-str)))
    ;;4-digit-years-p
    (when 4-digit-years-p (setf rest-attr-str (format nil "~a /4 " rest-attr-str)))
    ;; /n=new long list format where filenames are on the far right.
    ;;file-names-first-p doesn't work, tried various combos
   ;;(when 4-digit-years-p (setf rest-attr-str (format nil "~a /-n " rest-attr-str)))

   ;;must use / not \\ for paths.  if spaces in path, or if filespec? must have quotes around all   
   (setf path (clean-path-str path :wildcard-p wildcard-p :enclose-in-quotes-p t))
 ;; " \"e:/\""

    ;;make final command-str
    (setf command-str (format nil "~a  ~a"  command-str path))  ;;2nd wat ~s
    (when (> (length attribute-str) 8)
      (setf command-str (format nil "~a  ~a"  command-str attribute-str)))
    (when (> (length sort-order-str) 5)
      (setf command-str (format nil "~a  ~a"  command-str sort-order-str)))
    (when (> (length format-str) 1)
      (setf command-str (format nil "~a  ~a"  command-str format-str)))

    ;;STEP 2: RUN THE NEW DIR COMMAND
  ;;SSSSS USE AS MODEL FOR NEW FUNCTION -- 
       ;;1-MVB CAPTURE OUT-STRING and OUTPUT results of function call
    (multiple-value-bind (DOS-OUT-STRING dos-out)
        (CALL-WITH-SHOW-DOS-OUTPUT command-str
                                   :prefix dos-out-prefix :show-cmd show-cmd-p)    
      ;(break "1 mvb")

      ;;STEP 3: SORT THE DOS-OUT-STRING
      ;;2- Use above OUT-STRING as INPUT to function to sort the string.
      (multiple-value-bind (cur-dir host volname n-dirs 
                                    n-files used-size free-size 
                                     serial-num  dirlists filelists 
                                     all-lines all-line-lists)  
          (MAKE-DIR-INFOLISTS-FROM-DOS-DIR  DOS-OUT-STRING 
                                            :return-all-lines-p return-all-lines-p
                                            :return-line-lists-p return-line-lists-p
                                           :max-lines max-lines)
        (unless return-dos-output-t
          (setf dos-out-string nil))
      (values cur-dir host volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out all-lines all-line-lists)
      ;;end mvb,mvb,let, my-dos-dir
      ))))|#

;;OLD--NOT RECURSE ON LOWER SUBDIRS
#|(defun my-dos-dir (path &key (last-leveln 0)
                        wildcard-p attributes format  sort-order
                        directories-p lowercase-p  
                        read-only-p hidden-p system-files-p not-content-indexed-p 
                        archive-ready-p 
                        bare-p owner-p  4-digit-years-p reparse-points-p (show-cmd-p t)
                        ;;(file-names-first-p t) doesn't work
                        return-dos-output-t return-all-lines-p  return-line-lists-p
                        (max-lines 25000) dos-out-prefix)
  "U-files,  RETURNS  (values cur-dir host volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out all-line-lists)
   INPUT: note:  must incl \\ eg  c:\\ (not c: ) or scans current dir.
   WILDCARD-P  is when path includes eg. \"*.lisp\".
   ATTRIBUTES [d=directories, r=read-only files, h=hidden files, a=files ready for archiving, s=system files, i=not content indexed files, l=reparse points,  - =prefix meaning not.] 
   FORMAT: is :wide, :col (same as wide, but by cols), or nil.
   SUBDIR-FILES-P: displays files in specified directory and all subdirectories.   
   BARE-P= (no headings), /-c=no thousand sep, /d=sorted by col,      
   SORT-ORDER:  :name, :size (smallest first), :extension, :dir-first, :create-date, :last-access,  :last-access,:last-write [date/time (oldest first)]   -  =prefix to reverse order.
   NOTE: following not used:  /r=display alternate data streams of the file,   /x=this displays the short names generated for non-8dot3 file,  /p=pauses after each screenful of information, 
   RETURN-LINE-LISTS-P returns all lines as lists of tokens. "
  (let
      ((command-str "dir ")
       (com-rest-str " ")
       (attribute-str "/a: ")
       (format-str " ")
       (sort-order-str " /o: ")
       )

    ;;STEP 1: MAKE THE DIR COMMAND STRING

    #|       (attribute-keys-values '((directories-p "d")(read-only-p "r")(hidden-p "h")
                                (archive-ready-p "a")(system-files-p "s")(not-content-indexed-p "i")
                                (reparse-points-p "l")))|#
    ;;(format-keys-values '((:wide "/w")(:col "/d")))
              
    #|subdir-files-p: displays files in specified directory and all subdirectories.   
   bare-p= (no headings), /-c=no thousand sep, /d=sorted by col,      
   sort-order:  n=by name (alphabetic), s=by size (smallest first), e=by extension (alphabetic), d=by date/time (oldest first), g=group directories first,  -  =prefix to reverse order.
   sort-time-field: :create :last-access, or :last-write (tme field used for sorting).      
|#    
    ;;make attribute-str
    (setf attribute-str
          (when-key-add-value directories-p t " d " :begin-str attribute-str)
          attribute-str
          (when-key-add-value read-only-p t " r " :begin-str attribute-str)
          attribute-str
          (when-key-add-value hidden-p t " h " :begin-str attribute-str)   
          attribute-str
          (when-key-add-value archive-ready-p t " a " :begin-str attribute-str)
          attribute-str
          (when-key-add-value system-files-p t " s " :begin-str attribute-str)
          attribute-str
          (when-key-add-value not-content-indexed-p t " i " :begin-str attribute-str)
          attribute-str
          (when-key-add-value reparse-points-p t " l " :begin-str attribute-str))

    ;;make format-str 
    (cond
     ((equal format :wide)
      (setf format-str " /w "))
     ((equal format :col)
      (setf format-str " /d "))
     )

    ;;make sort-order-str
    ;;n=by name (alphabetic), s=by size (smallest first), e=by extension (alphabetic), d=by date/time (oldest first), g=group directories first
    (when sort-order
      (cond
       ((equal sort-order :name)
        (setf sort-order-str  " /o: n "))
       ((equal sort-order :size)
        (setf sort-order-str  " /o: s "))
       ((equal sort-order :extension)
        (setf sort-order-str  " /o: e "))
       ((equal sort-order :dir-first)
        (setf sort-order-str  " /o: g "))
       ;;sort-time-field: :create :last-access, or :last-write (tme field used for sorting).
       ;;should i incl /o too??
       ((equal sort-order :create-date)
        (setf sort-order-str  " /t: c "))
       ((equal sort-order :last-access)
        (setf sort-order-str  " /t: a "))
       ((equal sort-order :last-write)
        (setf sort-order-str  " /t: w "))
       (t nil)))

    ;;lowercase-p   
    (when lowercase-p (setf rest-attr-str (format nil "~a /l " rest-attr-str)))
    ;;bare-p
    (when bare-p (setf rest-attr-str (format nil "~a /b " rest-attr-str)))
    ;;owner-p
    (when owner-p (setf rest-attr-str (format nil "~a /q " rest-attr-str)))
    ;;4-digit-years-p
    (when 4-digit-years-p (setf rest-attr-str (format nil "~a /4 " rest-attr-str)))
    ;; /n=new long list format where filenames are on the far right.
    ;;file-names-first-p doesn't work, tried various combos
   ;;(when 4-digit-years-p (setf rest-attr-str (format nil "~a /-n " rest-attr-str)))

   ;;must use / not \\ for paths.  if spaces in path, or if filespec? must have quotes around all   
   (setf path (clean-path-str path :wildcard-p wildcard-p :enclose-in-quotes-p t))
 ;; " \"e:/\""

    ;;make final command-str
    (setf command-str (format nil "~a  ~a"  command-str path))  ;;2nd wat ~s
    (when (> (length attribute-str) 8)
      (setf command-str (format nil "~a  ~a"  command-str attribute-str)))
    (when (> (length sort-order-str) 5)
      (setf command-str (format nil "~a  ~a"  command-str sort-order-str)))
    (when (> (length format-str) 1)
      (setf command-str (format nil "~a  ~a"  command-str format-str)))

    ;;STEP 2: RUN THE NEW DIR COMMAND
  ;;SSSSS USE AS MODEL FOR NEW FUNCTION -- 
       ;;1-MVB CAPTURE OUT-STRING and OUTPUT results of function call
    (multiple-value-bind (DOS-OUT-STRING dos-out)
        (CALL-WITH-SHOW-DOS-OUTPUT command-str
                                   :prefix dos-out-prefix :show-cmd show-cmd-p)    
      ;(break "1 mvb")

      ;;STEP 3: SORT THE DOS-OUT-STRING
      ;;2- Use above OUT-STRING as INPUT to function to sort the string.
      (multiple-value-bind (cur-dir host volname n-dirs 
                                    n-files used-size free-size 
                                     serial-num  dirlists filelists 
                                     all-lines all-line-lists)  
          (MAKE-DIR-INFOLISTS-FROM-DOS-DIR  DOS-OUT-STRING 
                                            :return-all-lines-p return-all-lines-p
                                            :return-line-lists-p return-line-lists-p
                                           :max-lines max-lines)
        (unless return-dos-output-t
          (setf dos-out-string nil))
      (values cur-dir host volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out all-lines all-line-lists)
      ;;end mvb,mvb,let, my-dos-dir
      ))))|#