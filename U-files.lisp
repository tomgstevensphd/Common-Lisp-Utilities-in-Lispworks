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


;;WRITE-TO-FILE
;;2017
;;ddd
(defun write-to-file (filename objects &key (if-exists :append) 
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
  "In U-files,  USE FOR PPRINT TO A FILE,etc. plus lots more varisions using print.  U-Input-output  RETURNS    INPUT:  (if incl-nils-p= NIL, omits NILs) Flexible, works well."
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
    ))
;;TEST
;;(setf testwobj '(list '(first (a b c (1 2 3 (4 5 6 (7 8 9)(10 11 12) h i j) k l) next-last) last)))
;; (WRITE-TO-FILE   "c:/temp/my-write-test2.lisp" `( (setf testwobj ,(eval testwobj))))
;; (WRITE-TO-FILE   "c:/temp/my-write-test2.lisp" `(this (setf testwobj ,(eval testwobj))) :pretty T)




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
(defun load-eval-db (filename &key (eval-value T))
  "In U-files.lisp, doc in H-files.lisp. If eval-value is T, sets the key = (eval value), otherwise sets key = value."
  (let
      ((plist-list-db)
       )
    (cond
     ((probe-file filename)
      (with-open-file (in filename :direction :input :if-does-not-exist :create) ;;2018
          ;;(afout 'out (format nil ">>>> In load-eval-db, filename= ~A" filename))
        (with-standard-io-syntax
          (setf plist-list-db (read in))
          ;;set the key in each pair equal the evaluated value
          (set-key-to-value-in-plists plist-list-db :eval-value eval-value)
           ;;(afout 'out (format nil "plist-list-db= ~A~%" plist-list-db))
          )))
     (t  (show-text (format nil "FILENAME= ~A IS NOT A VALID PATHNAME~%" filename) 60 nil)
         ))
    plist-list-db
    ))
;;TEST
;; (testdb)
#|(defun testdb ()
  (setf out nil)
  (load-eval-db "~/settings.db");; don't eval value :eval-value t)
    (afout 'out (format nil "*PHOTO-DISPLAY-SECONDS= ~A~%" *PHOTO-DISPLAY-SECONDS))
  (fout out))|#

;;(probe-file  "C:/tomex/tomex-settings-db.lisp")
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






;;FILE-READ-LINES
;;
;;ddd
(defun read-file-lines (pathname &key start-line end-line file-max-lines
                                 add-newline-p )
  "In U-files.lisp, uses read-line to create a string that is the contents of the file. Normally terminates at eof, file-max-lines is a limiting number. add-newline-p puts a newline at end of each line.  Returns (values all-lines n-lines), all-lines is a list of strings. Function  makes the output lisp readable, eg. \" for quotes."
  (let
      (;;(line)
       (all-lines)
       (n-lines 0)
       )
    (unless start-line
      (setf start-line 0))
    (unless end-line
      (setf end-line 'eof))
    (unless file-max-lines
      (setf file-max-lines 1000))
    ;;read the file lines
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
;;read-file-lines (pathname &key start-line end-line file-max-lines)
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


;;MY-PROMPT-FOR-FILE 
;;2017
;;ddd
(defun my-prompt-for-file (message &key poke-process return-name-too-p load-file-p
                                    (verbose *load-verbose*) (package *package*) (print *load-print*) 
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
;;(my-dos-copy "C:\\3-TS\\Temp\\ArtsCulture.pdf" "c:\\temp")
;;ddd
(defun my-dos-copy (from-path to-dir &key show-text-p )
  "U-files.lisp"
  ;;note:  must have extra quotes or dos won't read it right
  (let ((command (format nil "copy \"~A\" \"~A\"" from-path to-dir))
    )
    (multiple-value-bind (dos-string result)
    (my-dos-command command show-text-p)
      (values dos-string result)
      ;;end mvb, let, my-dos-copy
      )))



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




;;LIST-DOS-DIR-CONTENTS (REPLACES list-directory-contents in LIST-DRIVE-INFO
;;2017
;;ddd
(defun list-dos-dir-contents (dirname &key print-lists-p incl-hostless-subdir-names-p)
  "In U-files.lisp, uses Seibel function. RETURNS S (values cur-dir host drive-name dos-volname serial-num n-dirs n-files used-size free-size  dirlists filelists  host-str hostless-dirstrs)  MUST INCL HOST except on drive C: (returns host-str= \"ERROR: NO HOST in dirname\".  "
  (let*
    (
#|       (file-paths)
       (filenamestr)
       (filenamestrs)
       (file-pathnamestr)
       (file-pathnamestrs)
       ;;(subdir-namestr)
       (subdir-namestrs)
       (subdir-paths)|#
       (host (host-namestring dirname))
       (host-str (format nil "~A:/" host))
       (hostless-dirstr)
       (hostless-dirstrs)
       )
    (when (equal host "")
      (setf host-str "ERROR: NO HOST in dirname"))
    ;;SSSS FINISH list-dos-dir-contents FIRST IF NEEDED??/
    (multiple-value-bind (cur-dir host volname serial-num n-dirs n-files 
                                  used-size free-size  dirlists filelists  dos-out-string dos-out)
        (my-dos-dir dirname)        
                     

      ;;older   (values filenamestrs file-pathnamestrs subdir-namestrs file-paths subdir-paths host-str hostless-dirstrs)
      (values cur-dir host drive-name dos-volname serial-num n-dirs n-files 
                                  used-size free-size  dirlists filelists  host-str hostless-dirstrs)
     ;;end, list-dos-dir-contents
     )))




;;LIST-DIRECTORY-CONTENTS (not neeed -- use MY-DOS-DIR instead??
;;
;;ddd
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
    ))
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
  "In U-files.lisp, If fi;;le-list, finds sorted filenames and pathnames in dirname.  If (null file-list), gets file and pathnames from dirname.  If by-embedded-num-p, then sorts files by an embeded number (as in most photo files).  (Uses Seibel function. Returns values filename-list and pathname-list, but NOT subdirectories) If file-list, DIRNAME should end in \\\ "
  (let ((filename)
        (embedded-num)
        (filename-list)
        (pathname)
        (pathname-list)
        (numfile-sublist)
        (numfile-list)
        (sorted-filenames)
        (sorted-pathnames)
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




;;ORGANIZE-SUBDIRS
;;
;;ddd
(defun organize-subdirs (main-dir sort-type by-dir-or-file)
  "In U-files, used by screensaver, sorts subdirs GIVEN FLAT OR TREE DIR, uses flatten-list-tree function in U-lists.lisp. Returns organized-dir-list flat-list simple-list. sort-type randomp, lessp or greaterp, by-dir-or-file is as-is dir or file"
  (let
      ((flat-subdirs-list)
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
    (multiple-value-setq (flat-subdirs-list last-sub-files-list)
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
(defun make-flat-dirs-list (dir &optional reverse-order-p &key paths-as-strings-p)
  "In U-Files.lisp, will take dir tree with scattered files at all levels, separate files in each dir into own list for each dir-branch in the file-tree.  Returns flat list of all these dirs irrespective of their level in the dir tree.  If randomize-dirs, randomizes the dirs in overall list"
  (setf *ufiles-out nil
        n 0)
  (let
      ((dir-almost-flat-list)
       (simple-all-files-list)
       (dir-list)
       (sub-dir-list  (list-directory dir))
       (sub-dir-name-list)
       (sub-file-list)
       (file-list)
       )
    (cond
     ((directory dir)
    ;;make list of sub-dirs of files and completely flat list of all files
     (multiple-value-setq (dir-flat-list sub-dir-list )  ;;had dir-almost-flat-list here
        (make-flat-dirs-list1 sub-dir-list 0 reverse-order-p))

;; (afout '*ufiles-out (format nil "In make-flat-dirs-list, FINAL, dir-flat-list= ~A~% sub-dir-list= ~A~% ~% simple-all-files-list= ~A~%"dir-flat-list sub-dir-list simple-all-files-list ))
    ;;now print out the output to a output-window 
  ;;  (fout *ufiles-out)
    )
     (t (show-text (format nil "In make-flat-dirs-list, DIRECTORY= ~A DOES NOT EXIST" dir) 60 nil)))
    (values dir-flat-list simple-all-files-list )
    ;;end let, make-flat-dirs-list
    ))


;;works after much work 
;;USE AS A MODEL FOR RECURSION???
;;
;;ddd
(defun make-flat-dirs-list1 (dir-list N &optional dir-flat-list reversed-order-p)
  "In U-files.lisp, works with make-flat-dirs-list as recursor- USE AS MODEL?"
  (let
      ((sub-file-list)
       (new-dir-list)
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
         ((and (null reversed-order-p)
               (setf sub-dir-list (append (list element) sub-dir-list))))
         (setf sub-dir-list (append sub-dir-list (list element))))
        ;;end of null clause
        )          
       ;;cut off process if limit reached
       ((>  n *recursion-limit) nil)
       ;;tests to see if it is a directory or a file- returns file list if a dir
        
       ((setf new-dir-list (list-directory element))
        (multiple-value-setq (dir-flat-list return-sub-dir-list)
            (make-flat-dirs-list1 new-dir-list  n dir-flat-list reversed-order-p))
        ;;moved from end to here to avoid append extra list at bottom of recursion
        ;;adding the unless clause here and at end stopped the extra list being appended at end
        (unless (equal return-sub-dir-list (car  (last dir-flat-list)))
          (cond
           ((and (null reversed-order-p) return-sub-dir-list)
            (setf dir-flat-list (append (list return-sub-dir-list)  dir-flat-list)))
           ;;was  (if return-sub-dir-list (setf dir-flat-list (append dir-flat-list (list return-sub-dir-list))))
           ( return-sub-dir-list
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
    



;;MY-DOS-DIR
;;2017
;;ddd
(defun my-dos-dir (path &key attributes format  sort-order directories-p lowercase-p  
                        read-only-p hidden-p system-files-p not-content-indexed-p archive-ready-p 
                        bare-p owner-p  4-digit-years-p reparse-points-p (show-cmd-p T)
                        ;;(file-names-first-p T) doesn't work
                        return-dos-output-t  (max-lines 1000)
                         dos-out-prefix)
  "In U-files,  RETURNS  (values cur-dir host volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out)
   INPUT: Note:  MUST incl \\ eg  c:\\ (not c: ) or scans current dir.
   ATTRIBUTES [D=Directories, R=Read-only files, H=Hidden files, A=Files ready for archiving, S=System files, I=Not content indexed files, L=Reparse Points,  - =Prefix meaning not.] 
   FORMAT: is :wide, :col (same as wide, but by cols), or NIL.
   SUBDIR-FILES-P: Displays files in specified directory and all subdirectories.   
   BARE-P= (no headings), /-C=NO THOUSAND SEP, /D=sorted by col,      
   SORT-ORDER:  :name, :size (smallest first), :extension, :dir-first, :create-date, :last-access,  :last-access,:last-write [date/time (oldest first)]   -  =Prefix to reverse order.
   Note: Following not used:  /R=Display alternate data streams of the file,   /X=This displays the short names generated for non-8dot3 file,  /P=Pauses after each screenful of information, "
  (let
      ((command-str "DIR ")
       (com-rest-str " ")
       (attribute-str "/A: ")
       (format-str " ")
       (sort-order-str " /O: ")
       )

    ;;STEP 1: MAKE THE DIR COMMAND STRING

    #|       (attribute-keys-values '((directories-p "D")(read-only-p "R")(hidden-p "H")
                                (archive-ready-p "A")(system-files-p "S")(not-content-indexed-p "I")
                                (reparse-points-p "L")))|#
    ;;(format-keys-values '((:wide "/W")(:col "/D")))
              
    #|SUBDIR-FILES-P: Displays files in specified directory and all subdirectories.   
   BARE-P= (no headings), /-C=NO THOUSAND SEP, /D=sorted by col,      
   SORT-ORDER:  N=By name (alphabetic), S=By size (smallest first), E=By extension (alphabetic), D=By date/time (oldest first), G=Group directories first,  -  =Prefix to reverse order.
   SORT-TIME-FIELD: :create :last-access, or :last-write (tme field used for sorting).      
|#    
    ;;make attribute-str
    (setf attribute-str
          (when-key-add-value directories-p T " D " :begin-str attribute-str)
          attribute-str
          (when-key-add-value read-only-p T " R " :begin-str attribute-str)
          attribute-str
          (when-key-add-value hidden-p T " H " :begin-str attribute-str)   
          attribute-str
          (when-key-add-value archive-ready-p T " A " :begin-str attribute-str)
          attribute-str
          (when-key-add-value system-files-p T " S " :begin-str attribute-str)
          attribute-str
          (when-key-add-value not-content-indexed-p T " I " :begin-str attribute-str)
          attribute-str
          (when-key-add-value reparse-points-p T " L " :begin-str attribute-str))

    ;;make format-str 
    (cond
     ((equal format :wide)
      (setf format-str " /W "))
     ((equal format :col)
      (setf format-str " /D "))
     )

    ;;make sort-order-str
    ;;N=By name (alphabetic), S=By size (smallest first), E=By extension (alphabetic), D=By date/time (oldest first), G=Group directories first
    (when sort-order
      (cond
       ((equal sort-order :name)
        (setf sort-order-str  " /O: N "))
       ((equal sort-order :size)
        (setf sort-order-str  " /O: S "))
       ((equal sort-order :extension)
        (setf sort-order-str  " /O: E "))
       ((equal sort-order :dir-first)
        (setf sort-order-str  " /O: G "))
       ;;SORT-TIME-FIELD: :create :last-access, or :last-write (tme field used for sorting).
       ;;SHOULD I INCL /O TOO??
       ((equal sort-order :create-date)
        (setf sort-order-str  " /T: C "))
       ((equal sort-order :last-access)
        (setf sort-order-str  " /T: A "))
       ((equal sort-order :last-write)
        (setf sort-order-str  " /T: W "))
       (t nil)))

    ;;lowercase-p   
    (when lowercase-p (setf rest-attr-str (format nil "~A /L " rest-attr-str)))
    ;;bare-p
    (when bare-p (setf rest-attr-str (format nil "~A /B " rest-attr-str)))
    ;;owner-p
    (when owner-p (setf rest-attr-str (format nil "~A /Q " rest-attr-str)))
    ;;4-digit-years-p
    (when 4-digit-years-p (setf rest-attr-str (format nil "~A /4 " rest-attr-str)))
    ;; /N=New long list format where filenames are on the far right.
    ;;file-names-first-p DOESN'T WORK, TRIED VARIOUS COMBOS
   ;;(when 4-digit-years-p (setf rest-attr-str (format nil "~A /-N " rest-attr-str)))

   ;;MUST use / not \\ for paths.  If spaces in path, must have quotes around all   
   (setf path (clean-path-str path :enclose-in-quotes-p T))
 ;; " \"E:/\""

    ;;make final command-str
    (setf command-str (format nil "~A  ~A"  command-str path))  ;;2nd wat ~S
    (when (> (length attribute-str) 8)
      (setf command-str (format nil "~A  ~A"  command-str attribute-str)))
    (when (> (length sort-order-str) 5)
      (setf command-str (format nil "~A  ~A"  command-str sort-order-str)))
    (when (> (length format-str) 1)
      (setf command-str (format nil "~A  ~A"  command-str format-str)))

    ;;STEP 2: RUN THE NEW DIR COMMAND

    (multiple-value-bind (dos-out-string dos-out)
        (CALL-WITH-SHOW-DOS-OUTPUT command-str
                                   :prefix dos-out-prefix :show-cmd-p show-cmd-p)    
      ;;(BREAK "1 MVB")
      ;;STEP 3: SORT THE DOS-OUT-STRING
      (multiple-value-bind (cur-dir host volname n-dirs n-files used-size free-size 
                                     serial-num  dirlists filelists)  
          (make-dir-infolists-from-dos-dir  dos-out-string   :return-all-lines-p nil
                                           :max-lines max-lines)

        (unless return-dos-output-t
          (setf dos-out-string nil))

      (values cur-dir host volname serial-num n-dirs n-files used-size free-size  dirlists filelists  dos-out-string dos-out)
      ;;end mvb,mvb,let, my-dos-dir
      ))))
;;TEST
;; (my-dos-dir  "e:\\1 LW FOLDERS CUM\\")
;; (my-dos-dir  "e:\\1 LW FOLDERS CUM")
;; (my-dos-dir (format nil "~S" "e:\\1 LW FOLDERS CUM\\"))
;; (my-dos-dir "c:\\")
;; works=  " c:\\"   "  c:\\ "   "OS"   "4058-766B"   "26"   "2"  "6493"  "659432439808"
;;((:DIR " AMD" "02/23/2017; 12:47 PM") (:DIR " Apps" "02/04/2017; 02:23 PM") (:DIR " Brother" "03/22/2017; 11:41 AM") (:DIR " Dell" "02/04/2017; 03:17 PM") (:DIR " Drivers" "02/04/2017; 12:58 PM") (:DIR " 3-TS" "04/30/2017; 08:33 AM") (:DIR " eeColorTables" "02/04/2017; 02:12 PM") (:DIR " Intel" "02/04/2017; 02:06 PM") (:DIR " My Software-Manuals" "03/22/2017; 11:52 AM") (:DIR " Our New Music" "02/09/2017; 04:32 AM")  ETC  (:DIR " tomex" "04/15/2017; 06:01 PM") (:DIR " Users" "02/08/2017; 11:31 AM") (:DIR " VIDEOS-temp" "02/10/2017; 08:40 PM") (:DIR " Windows" "04/12/2017; 06:18 PM") :N-DIRS "26" :N-FILES "2" :SIZE "6493" :FREE "659432439808")
;;((:FILE "bdlog.txt" "04/13/2017; 07:53 PM" "6291") (:FILE "StartMenu.ini" "02/04/2017; 02:22 PM" "202") :N-FILES "2")
;;NIL
;;0

#| ("Directory of C:\\3-TS\\LISP PROJECTS TS

04/13/2017  09:49 AM    <DIR>          .
04/13/2017  09:49 AM    <DIR>          ..
05/03/2016  05:26 PM           125,375 .LISPWORKS
05/03/2016  05:23 PM           125,283 .LISPWORKS~
09/16/2014  12:51 PM             1,422 1-LISP 7 NOTES.lisp
08/17/2014  03:29 PM               562 1-LISP 7 NOTES.lisp~
01/12/2014  11:10 PM                78 1-lisp-test.txt
02/08/2017  07:39 PM    <DIR>          AI-NLP
02/08/2017  05:43 PM    <DIR>          AndyCares
02/09/2017  10:48 AM    <DIR>          ART-LW
02/08/2017  06:24 PM    <DIR>          ART-preNestSymBU
02/23/2017  07:57 PM    <DIR>          ART-Utilities
06/05/2015  04:22 AM            36,777 ART2-multipane-interface-SCROLL.lisp
02/09/2017  03:38 PM    <DIR>          ART2-Output Data
02/08/2017  06:00 PM    <DIR>          ART3
02/08/2017  06:24 PM    <DIR>          ART3-BU
02/09/2017  07:07 AM    <DIR>          ART3-output-data
02/08/2017  07:47 PM    <DIR>          ARTMAP-JavaVersion
02/08/2017  07:50 PM    <DIR>          CL-Utilities
02/11/2017  11:05 PM    <DIR>          CogSys-Model
02/08/2017  08:26 PM    <DIR>          CogSysOutputs
03/26/2016  05:55 PM           168,115 CogTest-Deliver LOG.lisp
03/28/2016  12:45 PM             8,594 CSQ-PC-Frames.lisp
05/02/2016  07:00 PM           123,809 Current LISPWORKS 7.0 copy.lisp
01/22/2017  02:22 PM                 0 FAP4B66.tmp
03/24/2015  03:43 PM           148,904 fout-out-20cycles.lisp
03/23/2015  05:40 PM           159,102 fout-out-20cycles.lisp~
04/13/2017  03:50 PM    <DIR>          H-Capi
02/20/2017  03:56 PM    <DIR>          H-CapiExamples
04/21/2017  08:44 AM    <DIR>          H-HELP
05/03/2016  05:29 PM           125,375 LISPWORKS-7-0 BU.lisp
05/02/2016  05:51 PM           108,264 LISPWORKS-LAST 6.1.LISP
02/08/2017  06:20 PM    <DIR>          LW6-examples
02/08/2017  06:20 PM    <DIR>          LW7-EXAMPLES
02/08/2017  05:04 PM    <DIR>          LwStart
02/08/2017  04:56 PM    <DIR>          Math
11/24/2014  01:13 PM           108,180 My SHAQ ResultsAll-pt1.txt
11/24/2014  01:14 PM           504,495 My SHAQ ResultsAll-pt2.txt
02/09/2017  06:24 AM    <DIR>          MyConfigFiles
02/25/2016  09:04 PM                 0 myfile.txt
02/09/2017  05:23 AM    <DIR>          MyInterfaces
02/09/2017  02:33 AM    <DIR>          MyLispWebs
02/08/2017  08:35 PM    <DIR>          MyLWex-Utilities
02/08/2017  06:01 PM    <DIR>          MyProjects2
04/21/2017  08:36 AM    <DIR>          MyUtilities
02/09/2017  11:53 AM    <DIR>          Natural Language Code etc
11/17/2013  05:43 PM             2,076 package.lisp
11/17/2013  05:33 PM             1,996 package.lisp~
02/08/2017  06:29 PM    <DIR>          PCDB
11/17/2013  05:47 PM             7,868 permutation-examples.lisp
11/17/2013  05:45 PM             7,892 permutation-examples.lisp~
11/17/2013  05:54 PM            15,150 permutation.lisp
11/17/2013  05:50 PM            15,150 permutation.lisp~
09/13/2013  03:06 PM            92,043 PreviousBU.LISPWORKS
09/16/2013  06:40 PM           100,069 PreviousCopy.LISPWORKS
02/22/2017  01:21 PM                55 SAMSUNG-128USB.lisp
02/09/2017  06:05 AM    <DIR>          screensaver
02/09/2017  07:52 AM    <DIR>          screensaver2013
02/12/2017  12:22 AM    <DIR>          SecuritySIGNING Info-Issues
01/12/2012  04:00 PM             7,858 selection-key-binds.lisp
02/09/2017  01:59 AM    <DIR>          SHAQ
02/09/2017  10:48 AM    <DIR>          SHAQ - Copy2015-03-30
02/09/2017  12:09 PM    <DIR>          SHAQ - Copy2015-09-08
02/09/2017  09:50 AM    <DIR>          SHAQ-Copy2014-12-05
02/09/2017  09:37 AM    <DIR>          SHAQ-Copy2015-01-13
11/29/2014  06:21 PM           124,731 shaq-data-list-all.lisp
11/29/2014  06:04 PM           124,004 shaq-data-list-all.lisp~
11/29/2014  02:29 PM            32,441 shaq-data-list-anger.lisp
11/28/2014  07:18 PM            32,428 shaq-data-list-anger.lisp~
11/26/2014  09:06 PM            26,396 shaq-data-list-eg1.lisp
09/26/2014  05:16 PM           122,866 shaq-data-list-eg1.lisp~
09/11/2014  07:16 PM            65,002 shaq-data-list-eq2.lisp
09/11/2014  05:23 PM            54,128 shaq-data-list-eq2.lisp~
11/19/2014  05:28 PM            94,829 shaq-data-list-for-spss.lisp
10/29/2014  12:42 PM           117,958 shaq-data-list-for-spss.lisp~
10/18/2014  07:17 PM            50,600 shaq-data-list-NoAcadCAR.lisp
10/18/2014  06:24 PM            12,203 shaq-data-list-OUTOMCESonly.lisp
09/29/2015  04:29 PM         1,684,199 SHAQ-Deliver LOG.lisp
02/08/2017  11:29 PM    <DIR>          SHAQ-PLUS
02/09/2017  05:43 PM    <DIR>          SHAQ-ResultsTEXTetc
02/09/2017  06:22 AM    <DIR>          SHAQ-Web signed-exe php
02/09/2017  01:35 AM    <DIR>          SHAQxNotUsed
11/16/2014  11:58 PM            74,081 spss-test-datafile.sav
08/15/2013  03:07 PM                66 tags
11/26/2014  12:11 PM                 0 test-buffer.lisp
11/19/2014  04:13 PM            16,448 test-buffer.lisp~
11/18/2014  03:55 PM           125,541 test-combine.lisp
05/15/2016  08:02 PM            16,860 test-functions2.lisp
05/15/2016  03:21 PM             9,880 test-functions2.lisp~
03/24/2014  02:40 PM               390 test-object.lisp
11/16/2014  09:15 AM            19,683 test-spss-output.txt~
11/25/2014  06:24 PM            12,093 test-spss-output0.txt
11/16/2014  11:59 PM            15,559 test-spss-output1.txt
04/21/2017  11:51 AM    <DIR>          Toms-HDrive-Explorer
02/09/2017  04:34 PM    <DIR>          video-playback-utility
02/08/2017  09:24 PM    <DIR>          VideoPlayback
02/08/2017  06:06 PM    <DIR>          z-LISPWORKS files
              51 File(s)      4,826,878 bytes
              46 Dir(s)  665,718,132,736 bytes free
")
|#


;;MAKE-DIR-INFOLISTS-FROM-DOS-DIR
;;
;;ddd
(defun make-dir-infolists-from-dos-dir (dos-output-str &key (return-all-lines-p T) 
                                                       (max-lines 1000))
  "In U-files, RETURNS  (values cur-dir host volname dos-n-dirs dos-n-files used-size free-bytes  serial-num  all-dirs-info all-files-info  all-lines).  dos-dir is the dir being scanned, if not supplied, just lists 'root' RETURN-ALL-LINES-P prints a string of the exact dos output."
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
       )
    (with-open-stream (dos-str dos-input-stream)
      (loop
       for line-n from 1 to max-lines
       do
       (let
           ((len-line)
            (line1 "")
            (line "")
            (file-line-info) 
            (line-list)
            (date-time)
            (dir-infolist)
            (file-infolist)
            (test-out)
            )
         ;;READ THE CURRENT LINE
         (setf line (read-line dos-str  nil 'eof)
               ;;line (format nil "~A" line1)
               all-lines (format nil "~A~%~A" all-lines line))

         ;;return from loop?
         (when  (equal line 'eof)
           (return))

         ;;line length
         (setf len-line (length line))

         ;;DIVIDE STRING INTO TOKEN STRINGS
         (setf line-list (divide-string-to-all-tokens line :ignore-char-list '(#\,  #\Tab))) ;;not #\.

         ;;(afout 'out (format nil ">>> line= ~A~%line-list= ~A~%test-out= ~A" line line-list  test-out))

         ;;CHECK EACH LINE
         (cond
          ;;Lines 1-5
          ((= line-n 1)
           (setf host (subseq line 4)))
          ((= line-n 2)
           (setf volname (car (last line-list)))) ;; (subseq line 21)))
          ((= line-n 3)
           (setf serial-num (car (last line-list)))) ;;(subseq line 24)))
          ((= line-n 5)
           (setf cur-dir  (subseq line 13)))   ;; (subseq line 12)))
          ;;((break "find-best-match"))
          ;;FOR Num Files and Bytes line
          ;;              51 File(s)      4,826,878 bytes
          ((find-best-match "File(s)" line-list)           
           (setf dos-n-files  (first line-list)
                 used-size  (third line-list))
           ;;   redundant   all-files-info (append all-files-info  (list :n-files dos-n-files )))
           ;;(break "files")
           )
          ;;FOR Num dirs and free space line
          ;;Bytes free line
          ;;              46 Dir(s)  665,718,132,736 bytes fre
          ((find-best-match "free" line-list)           
           (setf dos-n-dirs (first line-list)
                 free-bytes (third line-list))
           ;;was, but caused redundant listing of this info
#|                 info-list
                 (list :n-dirs dos-n-dirs :n-files dos-n-files :size used-size :free free-bytes)
                 all-dirs-info (append all-dirs-info  info-list))|#
           ;;(break "free")
           )

          ;;DIR lines
          ((find-best-match "DIR" line-list)  ;; "<DIR>"
           ;;MAKE (list :DIR dirname date time am-pm)
           (multiple-value-bind (date time am-pm dir )
               (values-list line-list)
             ;;here
             (setf dirname (format-string-list  (nthcdr 4 line-list) :line-width 0))
             (unless (member dirname '( "" "." "..") :test 'string-equal)      
               (setf date-time (format nil "~A; ~A ~A" date time am-pm)
                     dir-infolist  (list :dir dirname date-time )
                     all-dirs-info (append all-dirs-info (list dir-infolist)))
               ;;(afout 'out (format nil "line= ~A line-list= ~A dir-infolist= ~A~% all-dirs-info= ~A" line line-list dir-infolist all-dirs-info))
               ;;end unless,mvb, find-
               )))
          
          ;;FILE lines
          ((or (find-best-match "AM"  line-list)
               (find-best-match "PM" line-list))
           ;;make (list :FILE filename date time am-pm size)
           (multiple-value-bind (date time am-pm size) ;; filename)
               (values-list line-list)           
             (setf filename (format-string-list  (nthcdr 4 line-list) :line-width 0))
             (setf date-time (format nil "~A; ~A ~A" date time am-pm)
                   file-infolist (list :file filename date-time size)
                   all-files-info (append all-files-info (list file-infolist)))
             ;;end mvb, or
             ))
          (T NIL))
         ;;end let, loop
         ))
      ;;end with-open
      )
    (unless return-all-lines-p
      (setf all-lines nil))

    (values cur-dir host volname dos-n-dirs dos-n-files used-size free-bytes  serial-num  all-dirs-info all-files-info  all-lines)
    ;;end let, make-dir-infolists-from-dos-dir
    ))     
;;TEST
;;(my-dos-dir "g:\\")
#|" g:\\"
"  g:\\ "
"4tbSeagate"
"A8EB-A38B"
"21"
"4"
"49266900"
"1925239435264"
((:DIR " 0 Book Article for Academic Websites" "08/03/2016" "03:00" "PM") (:DIR " 0 Main OUR PHOTOS-VIDEOS" "03/22/2017" "07:34" "PM") (:DIR " 1 Current LW BUs" "02/23/2017" "07:58" "PM") (:DIR " 1 LW CUMLATIVE BUS" "02/21/2017" "10:10" "AM") (:DIR " 1 MOVE TO MEDIA CENTER" "03/27/2017" "12:10" "PM") (:DIR " 2 Video Databases-Collectorz" "07/30/2016" "04:41" "PM") (:DIR " 2-CURRENT XPS MISC BUs" "06/07/2016" "07:52" "AM") (:DIR " 3 Our New Music" "09/10/2016" "01:49" "AM") (:DIR " 3 Selected Music" "04/20/2017" "07:39" "PM") (:DIR " 5 TO HOME Netflix and Audials" "09/26/2016" "03:10" "PM") (:DIR " 9b0dc53b1249130458213666" "02/12/2017" "11:15" "AM") (:DIR " Acronis Backups" "02/26/2017" "01:56" "PM") (:DIR " 3-TS" "02/08/2017" "06:40" "PM") (:DIR " FileHistory" "02/09/2017" "08:35" "AM") (:DIR " From XPS" "12/15/2016" "01:47" "PM") (:DIR " From XPS laptop" "02/09/2017" "11:49" "AM") (:DIR " MOVIES TO MAUI" "09/26/2016" "03:07" "PM") (:DIR " My 1420 Software and Manuals-Deleted WhenMOVED" "12/15/2016" "03:01" "PM") (:DIR " PHONE BUs" "06/16/2016" "08:31" "AM") (:DIR " TEMP VIDEOS" "04/25/2017" "06:08" "AM") (:DIR " TRAVEL VIDEOS-audials" "08/30/2016" "11:47" "PM") :N-DIRS "21" :N-FILES "4" :SIZE "49266900" :FREE "1925239435264")
((:FILE "msdia80.dll" "12/02/2006" "12:37" "AM" "904704") (:FILE "Set_Up.dmg" "06/02/2015" "05:05" "PM" "27929012") (:FILE "Set_Up.exe" "06/02/2015" "05:03" "PM" "19286784") :N-FILES "01/08/2015" :N-FILES "4")
NIL
0|#
;; (make-dir-infolists-from-dos-dir *test-dos-str :return-all-lines-p nil)
;; WORKS=  " C:\\3-TS\\LISP PROJECTS TS"
;; "  c: "  "OS"   "46"   "51"   "4826878"   "665718132736"   "4058-766B"
;; DIRS= ((:DIR "AI-NLP" "02/08/2017" "07:39" "PM") (:DIR "AndyCares" "02/08/2017" "05:43" "PM") (:DIR "ART-LW" "02/09/2017" "10:48" "AM") (:DIR "ART-preNestSymBU" "02/08/2017" "06:24" "PM") (:DIR "ART-Utilities" "02/23/2017" "07:57" "PM") (:DIR "ART2-Output" "02/09/2017" "03:38" "PM") (:DIR "ART3" "02/08/2017" "06:00" "PM") (:DIR "ART3-BU" "02/08/2017" "06:24" "PM") (:DIR "ART3-output-data" "02/09/2017" "07:07" "AM") (:DIR "ARTMAP-JavaVersion" "02/08/2017" "07:47" "PM") (:DIR "CL-Utilities" "02/08/2017" "07:50" "PM") (:DIR "CogSys-Model" "02/11/2017" "11:05" "PM") (:DIR "CogSysOutputs" "02/08/2017" "08:26" "PM")  ETC.
;;FILES=  ((:FILE ".LISPWORKS" "05/03/2016" "05:26" "PM" "125375") (:FILE ".LISPWORKS~" "05/03/2016" "05:23" "PM" "125283") (:FILE "1-LISP" "09/16/2014" "12:51" "PM" "1422") (:FILE "1-LISP" "08/17/2014" "03:29" "PM" "562") (:FILE "1-lisp-test.txt" "01/12/2014" "11:10" "PM" "78") (:FILE "ART2-multipane-interface-SCROLL.lisp" "06/05/2015" "04:22" "AM" "36777") (:FILE "CogTest-Deliver" "03/26/2016" "05:55" "PM" "168115") (:FILE "CSQ-PC-Frames.lisp" "03/28/2016" "12:45" "PM" "8594") (:FILE "Current" "05/02/2016" "07:00" "PM" "123809") (:FILE "FAP4B66.tmp" "01/22/2017" "02:22" "PM" "0") (:FILE "fout-out-20cycles.lisp" "03/24/2015" "03:43" "PM" "148904") (:FILE "fout-out-20cycles.lisp~" "03/23/2015" "05:40" "PM" "159102") (:FILE "LISPWORKS-7-0" "05/03/2016" "05:29" "PM" "125375") (:FILE "LISPWORKS-LAST" "05/02/2016" "05:51" "PM" "108264")  ETC )
          ;;(multiple-value-setq (date time am-pm dir dirname) ;; filename size)
          ;;(values-list line-list))
          ;;SSS  PUT LINE ITEMS IN TOMEX ORDER??
          ;;(values cur-dir all-dirs-info all-files-info dos-n-dirs dos-n-files used-size free-bytes host volname serial-num cur-dir all-lines) 
          ;;(make-dir-infolists-from-dos-dir *test-dos-str :return-all-lines-p nil)
          ;; (setf *xline (divide-string-to-all-tokens "11/29/2014  06:21 PM           124,731 shaq-data-list-all.lisp"))
          ;; works= ("11/29/2014" "06:21" "PM" "124731" "shaq-data-list-alllisp")
          ;; (find-best-match "PM" *xline) returns "PM" 2 2 etc
      ;; (divide-string-to-tokens *xline  '()  :word-delim-list '( #\\) :no-initial-spaces T  :ignore-char-list nil)



;;LIST-FILES
;;2017
;;ddd
(defun list-files (dir)
  "In U-files, RETURNS (values filenames filepaths file-realpaths). Uses separate-files-from-dirs"
  (let*
      ((paths (list-directory dir))       
       )
    (multiple-value-bind (file-realpaths sdpaths filenames sdnames
                                         filepaths sdpathnames)
        ;;ile-list subdir-list file-name-list subdir-name-list
        (separate-files-from-dirs dir :return-subdir-info-p nil)

      (values filenames filepaths file-realpaths)
      ;;end mvb, let, list-files
      )))
;;TEST
;; (list-files "c:\\temp\\")
;; works=
;; filenames= ("switchsetup.exe" "setup_xrecode3_win_64bit_1.56.exe" "netbeans-8.2-windows.exe" "jdk-8u121-windows-x64.exe" "Editorial Staff Directory - LA Times.htm" "coral test.cdr" "Chrome Setup.exe" "bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe")
;;filepaths= ("c:\\temp\\switchsetup.exe" "c:\\temp\\setup_xrecode3_win_64bit_1.56.exe" "c:\\temp\\netbeans-8.2-windows.exe" "c:\\temp\\jdk-8u121-windows-x64.exe" "c:\\temp\\Editorial Staff Directory - LA Times.htm" "c:\\temp\\coral test.cdr" "c:\\temp\\Chrome Setup.exe" "c:\\temp\\bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe")
;;file-realpaths= (#P"c:/temp/switchsetup.exe" #P"c:/temp/setup_xrecode3_win_64bit_1.56.exe" #P"c:/temp/netbeans-8.2-windows.exe" #P"c:/temp/jdk-8u121-windows-x64.exe" #P"c:/temp/Editorial Staff Directory - LA Times.htm" #P"c:/temp/coral test.cdr" #P"c:/temp/Chrome Setup.exe" #P"c:/temp/bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe")



;;GET-FILES-W-EXTS
;;2018
;;ddd
(defun get-files-w-exts (files &key (exts '("lisp" "txt")))
  "U-files Returns list of only files from files with extensions in exts"
  (let
      ((return-files)
       )
    (loop
     for file in files
     do
     (let
         ((ext (pathname-type file))
          )
       (when (member ext exts :test 'string-equal)
         (setf return-files (append return-files (list file))))
       ;;end let,loop
       ))
    return-files
    ;;end let,get-files-w-exts
    ))
;;TEST
;; (get-files-w-exts '("F1.lisp" "F2.doc" "F3.txt" "F4.lisp~" "F5.fasl" "F6.lisp"))
;;works= ("F1.lisp" "F3.txt" "F6.lisp")
                               




;;SEPARATE-FILES-FROM-DIRS
;;
;;ddd
(defun separate-files-from-dirs (dir-or-dir-list &key (return-strings-p T) 
                                                 (return-file-info-p T)  (return-subdir-info-p T))
  "In U-files.lisp, removes and RETURNS (values file-realpaths subdir-realpaths filename-list subdir-name-list filepath-names subdirpath-names) from a dir.  If RETURN-STRINGS-P returns dir and file namestrings too.  If return-drive-p, returns the drive letter with the dir. Use LIST-DIRECTORY-CONTENTS to list ALL subdir and file paths and strings SEPARATELY."
  ;;not needed or use in cond below?  (setf dir (pathname-as-directory dir-or-dir-list))
  ;;  (show-text (format nil "In separate-files-from-dirs, dir-or-dir-list ~A~%" dir-or-dir-list) 20 t)
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
    ;;  (show-text (format nil "2-In separate-files-from-dirs, dir-list ~A~%" dir-list) 200 t)
    (dolist (element dir-or-dir-list)
      ;; (show-text (format nil "element= ~A~%" element) 20 t)

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
        (when return-file-info-p
          (setf file-realpaths (append file-realpaths (list element)))
          (when return-strings-p
            (setf filename-list (append filename-list  
                                        (list (file-namestring element)))
                  filepath-names (append filepath-names
                                         (list (namestring element)))))))
       ;;  (show-text (format nil "file-name= ~A~%" element) 20 t)  
       (t nil))
      ;;end dolist
      )
    (values file-realpaths subdir-realpaths filename-list subdir-name-list 
            filepath-names subdirpath-names) 
    ;;end let, separate-files-from-dirs
    ))
;;TEST
;; (separate-files-from-dirs "c:\\temp\\")
;; works=
;;file-realpaths= (#P"c:/temp/switchsetup.exe" #P"c:/temp/setup_xrecode3_win_64bit_1.56.exe" #P"c:/temp/netbeans-8.2-windows.exe" #P"c:/temp/jdk-8u121-windows-x64.exe" #P"c:/temp/Editorial Staff Directory - LA Times.htm" #P"c:/temp/coral test.cdr" #P"c:/temp/Chrome Setup.exe" #P"c:/temp/bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe")
;;subdir-realpaths= (#P"c:/temp/Editorial Staff Directory - LA Times_files/")
;;filename-list= ("switchsetup.exe" "setup_xrecode3_win_64bit_1.56.exe" "netbeans-8.2-windows.exe" "jdk-8u121-windows-x64.exe" "Editorial Staff Directory - LA Times.htm" "coral test.cdr" "Chrome Setup.exe" "bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe")
;;subdir-name-list= ("\\temp\\Editorial Staff Directory - LA Times_files\\")
;;filepath-names= ("c:\\temp\\switchsetup.exe" "c:\\temp\\setup_xrecode3_win_64bit_1.56.exe" "c:\\temp\\netbeans-8.2-windows.exe" "c:\\temp\\jdk-8u121-windows-x64.exe" "c:\\temp\\Editorial Staff Directory - LA Times.htm" "c:\\temp\\coral test.cdr" "c:\\temp\\Chrome Setup.exe" "c:\\temp\\bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe")
;;subdirpath-names= ("c:\\temp\\Editorial Staff Directory - LA Times_files\\")






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


;;LIST-DIRECTORY-NAMESTRINGS
;;
;;ddd
(defun list-directory-namestrings (dirname &key paths)
  "In U-files.lisp, RETURNS (values  path-namestrings paths) =  list of  path-namestrings and real paths using Seibel advises to use this instead of DIRECTORY function; NOTE: USE LIST-DIRECTORY-CONTENTS to return all file and subdir paths and strings. IF PATHS, saves step of finding paths in dirname."
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


;;LIST-ALL-DRIVES-INFO
;;
;;ddd
(defun list-all-drives-info (&key (last-leveln 2) 
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
    ;;end let, list-all-drives-info
    ))
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
(defun list-all-drives-info2 (&key (last-leveln 2) 
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
    ))
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




;;LIST-ALL-DRIVES
;;
;;ddd
(defun list-all-drives (&key (drive-letters '(a b C d e f g h i j k l m n o p q r s t u))
                             (set-drivesym-to-symlist-p T)  confirm-drive-vol-names-p 
                             (drive-syms-sym '*tomex-drive-syms)
                             (save-to-file-p T)(data-dir-location *tomex-data-dir)
                             (if-exists NIL))
  "In U-lists RETURNS (values drive-syms drive-paths found-drive-letters n-drives). Only checks to see if drive exists. INPUT can be drive letter or drive-path. IF-EXISTS for data output files is normally :overwrite, :new-version, or NIL  here. Creates data files for EACH drive letter (drive-sym = filename) in  data-dir-location if save-to-file-p . drive-syms-sym not changed unless save-to-file-p.ONLY SCANS BASIC INFO, NO DIRS OR FILES. Use tomex functions for more data (eg. go-tomex). When confirm-drive-vol-names-p, asks to confirm each name."
  (let
      ((drive-paths)
       (path)
       (list-all-drives-process (mp:get-current-process))
       (found-drive-letters)
       (n-drives 0)
       (drive-syms)
       (last-leveln 1)
       )
    (loop
     for drive in drive-letters
     do
     (let
         ((drive-sym)
          (drive-symlist)
          (drive-name)
          (dos-volname)
          (location)
          (date (my-get-date-time))
          )
       (cond
        ((symbolp drive)
         (setf path (format nil "~A:/" drive)))
        (t (setf path drive)))

       (when (directory-p path)
         (incf n-drives)
         (setf drive-paths (append drive-paths (list path))
               found-drive-letters (append found-drive-letters
                                           (list (my-make-symbol (subseq path 0 1)))))

         ;;added-from MY-DOS-DIR TO GET MORE INFO--VOL NAME SER-NUM ETC
         (multiple-value-bind (cur-dir host volname serial-num n-dirs n-files 
                                       used-size free-size  dirlists filelists  dos-out-string dos-out)
             (my-dos-dir path)

           ;;SSS ADDED EDIT DRIVE-VOL NAMES HERE
           ;;initially set drive-name to dos-volname
           (setf  dos-volname volname
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
           (setf drive-sym (my-make-symbol drive-name)
                   drive-syms (append drive-syms (list drive-sym)))
           
           ;;IF PREVIOUS DRIVE-SYM, THEN CHECK IF-EXISTS, do what?
           (when (or  (and drive-syms-sym
                           (not (member drive-sym (eval drive-syms-sym) :test 'my-equal)))
                      (member if-exists '(:overwrite :new-version :supercede)))
             (setf host (clean-path-str host :remove-extra-quotes-p T))
             (setf location (directory-namestring path)
                   drive-symlist
                   (list :DRIVE drive-sym (list :NAME drive-name  :HOST host 
                                                :VOLNAME dos-volname
                                                :LOCATION data-dir-location
                                                :TYPE "??" :BRAND "??"
                                                :SIZE "??"  :USED used-size    :FREE free-size
                                                :SERIAL-NUM serial-num
                                                :DATE date  :LAST-LEVElN last-leveln)))

             (when drive-sym 
               ;;set sym to symlist?
               (when set-drivesym-to-symlist-p
                 (set drive-sym drive-symlist))
               ;;(break "drive-sym")
               ;;save data to file?
               (when save-to-file-p
                 (save-tomex-drive-data-to-file drive-syms
                                                :data-dir data-dir-location :if-exists if-exists ))
               ;;end when drive-sym, when or members,  
               ))

           ;;end mvb,when, let, loop
           ))))            
    (values drive-syms drive-paths found-drive-letters n-drives)
    ;;mvb,let, list-all-drives
    ))
;;TEST
;;  (list-all-drives)
;;  works= ("C:\\" "E:\\" "F:\\" "G:\\")   (C E F G)    4
;; (list-all-drives :drive-letters '(f) :confirm-drive-vol-names-p T)


;;LIST-DOS-DRIVE-INFO
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
;; (find-dirnames-from-paths "C:\\" :pathlist-is-root-p T  :incl-subdir-namelists-p T)
;; works= 

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
;;ddd
(defun find-subdir-namestrings (path &key (leveln 1) (remove-final-slashes-p  T))
  "In U-files.  Finds the subdir string-names in a multi-lovel path. leveln specifies the level with 1= first level after drive root. Path can be path or path namestring RETURNS (values subdirs target-subdir). PATH must be a real pathname or a pathname string."
  (let
      ((subdirs)
       (subdir-str)
       (target-subdir)
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
      (setf subdirs  (nth-value 2 (divide-string-to-tokens  subdir-str  '()  :word-delim-list '( #\\) :no-initial-spaces T  :ignore-char-list nil))))
     ((stringp path)
      (setf subdirs  (nth-value 2 (divide-string-to-tokens  path  '()  :word-delim-list '( #\\) :no-initial-spaces T  :ignore-char-list nil))))
      (t nil))
      (setf target-subdir (nth (- leveln 1)  subdirs))

      (values subdirs target-subdir)
    ;;end let, find-subdir-name-from-path
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
(defun make-path-from-subdirs (&key root dir-path subdirs filename also-return-real-path-p)
  "In U-files, takes either a full directory pathname (dir-path) or a list of subdir strings (subdirs) and returns al full pathname. root can be nil. If filename, adds it to end."
  (let 
      ((newdir "\\") ;; (car (find-subdir-namestrings directory)))
       (newpath)
       (newpaths)
       (real-path)
       )
    (cond
     ((null root)
      (setf root ""))
     ((equal (length (string root)) 1)
      (setf root (format nil "~A:" root)))
     (t (setf root (format nil "~A:" (host-namestring root)))))

    (when subdirs
      (loop
       for subdir in subdirs
       do
       (setf newdir (format nil "~A~A/" newdir subdir))
       ;;end loop
       )
      (setf dir-path newdir)
      ;;end when
      )

    ;;ADD ROOT?
    (cond
       ;;host not included
       ((string-equal (host-namestring dir-path) "")
        (setf newpath (format nil "~A~A" root dir-path)))
       (t (setf newpath dir-path)))

    (when filename
      (setf newpath (format nil "~A~A" newpath filename)))

    (when also-return-real-path-p
      (setf real-path (pathname newpath)))

      (values  newpath real-path)
      ;;end let,make-path-from-subdirs
      ))
;;TEST
;;  (make-path-from-subdirs :root "c" :dir-path "\\thisdir\\")  =  "c:\\thisdir\\"   NIL    
;;  no root
;; (make-path-from-subdirs  :dir-path "\\thisdir\\") = "\\thisdir\\" NIL
;;  filename
;; (make-path-from-subdirs :root "c" :dir-path "\\thisdir\\" :filename "thisfile.lisp") = "c:\\thisdir\\thisfile.lisp"   NIL
;; multi-subdirs plus :also-return-real-path-p T
;;  (make-path-from-subdirs :root "c" :dir-path "\\thisdir\\that\\other\\" :also-return-real-path-p T) = "c:\\thisdir\\that\\other\\"   #P"c:/thisdir/that/other/"
;;  subdirs 
;;  (make-path-from-subdirs :root "c" :subdirs  '("thisdir"  "that" "other" "last")) = "c:\\thisdir\\that\\other\\last\\"   NIL
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



;;FILE-PATHNAME
;;ddd
(defun file-pathname-p (p)
  "In U-files.lisp, Returns a file path string name from either real path or string name"
  (unless (directory-pathname-p p) p))
;;TEST
;;  (file-pathname-p "C:\\3-TS\\LISP PROJECTS\\MyProjects\\1 TEST OUTPUT.docx")
;;(file-pathname-p "c:\\temp\\") = NIL
;;(file-pathname-p "c:\\temp\\test-photo1.jpg") => "c:\\temp\\test-photo1.jpg"
;;(file-pathname-p "c:\\temp") = "c:\\temp"
;;(file-pathname-p "c:\\temp\\*.*") = "c:\\temp\\*.*"


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
;;modified 2017
;;ddd
(defun file-exists-p (pathname)
  "RETURNS (values file-path filename-st). Similar to CL:PROBE-FILE except it always returns directory names in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."
  (let
      ((filename-str)
       (filepath)
       )
  (when (setf filepath (probe-file pathname))
    (setf filename-str (file-namestring pathname)))

  (unless filename-str 
    (setf filepath nil))

    (values filepath filename-str)
    ;;end let, file-exists-p
    ))
;;TEST
;;  (file-exists-p "c:\\temp\\this.lisp") = NIL NIL
;;  (file-exists-p "c:\\tomex\\tomex-settings-db.lisp")
;; works= #P"c:/tomex/tomex-settings-db.lisp"   "tomex-settings-db.lisp"
;;  (file-exists-p "c:\\temp\\") = NIL NIL


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
;;
;;ddd
(defun directory-p (name)
  "Is `name' the name of an existing directory. RETURNS realpaths of dir contents.
  USE ensure-directories-exist INSTEAD?
  If directory is EMPTY, does not work! Returns nil even if it exists."
#| old  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))|#
  (let
      ((dirlist (list-directory name))
       )
   dirlist
    ;;end let,directory-p
  ))
;;TEST
;; (directory-p "C:\\TOMEX\\") = (#P"C:/TOMEX/tomex-settings-db.lisp")
;; (directory-p "c:\\nosuchdir\\") = NIL

;;(ensure-directories-exist "c:/tomex/" :verbose T)
;; works tells exist (NIL not create) = "c:/tomex/"   NIL
;;(ensure-directories-exist "c:/tomex/saved/" :verbose T)
;; works creates directory= ;; Created directory c:\tomex\saved\   "c:/tomex/saved/"  T


;;FILE-P
;;
;;ddd
(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))
    ;;end let, file-p
    ))



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


;;PRINT-NESTED-LIST-TO-FILE
;;
;;ddd
(defun print-nested-list-to-file (nested-list pathname &key incl-label 
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
    (format t ">>>>>>>> END PRINTING NESTED-LIST TO FILE: ~A~%" pathname)
    ;;let, print-nested-list-to-file
    ))
;;TEST
;; (print-nested-list-to-file '(this (that  (nested (list) a ) b ) c)   "C:\\3-TS\\LISP PROJECTS TS\\test-object.lisp"  :incl-label  'my-label)
;;(print-nested-list-to-file '(double (this (that  (nested (list) a ) b ) c) d)   "C:\\3-TS\\LISP PROJECTS TS\\test-object.lisp"  :incl-label  'my-label :print-fewer-newlines-p t)
;;  (print-nested-list-to-file '(xxx  (a 1 2 3) (b 4 5 6) ( c 7 8 9) d)   "C:\\3-TS\\LISP PROJECTS TS\\test-object.lisp"  :incl-label  'my-label :print-fewer-newlines-p t)








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
;; (file-size "c:\\tomex\\tomex-settings-db.lisp") = 112
;; (file-size "c:\\tomex\\file-not-exist.lisp") = NIL





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
;;  (file-date  "c:\\tomex\\tomex-settings-db.lisp")
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
;;  (get-file-info "c:\\tomex\\tomex-settings-db.lisp")
;; works=  
#|"tomex-settings-db.lisp"
"\\tomex\\"
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
           (setf item (read in nil 'eof))
           ;;(afout 'out (format nil ">>>> In read-shaq-data, item ~A" item)) 
             
           (cond
            ((equal item  'eof)
             (return))
            (t (incf num-reads)))
           ;;end loop, with, with
           )))
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
    (with-open-file (out  target-file :direction :output :if-exists :append :if-does-not-exist :create)
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
;;  (combine-lisp-object-files '( "shaq-data-list-for-spss.lisp")  "C:\\3-TS\\LISP PROJECTS TS\\test-combine.lisp" NIL   :pretty T ) = ,WORKS,  MUST HAVE NIL IN OPTIONAL SPOT HERE.
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
(defun make-directory (dirpath &key (show-results-p T))
  "In U-files.  RETURNS (values dirpath1 dirpath2 dir-created-p)  dirpath can end in \\ or nothing.    SHOW-RESULTS => popup showing results. dirpath1 has forward slashes.  dirpath2 same as original dirpath. Uses CL ensure-directories-exist"
  (let
      ((dirpath1)
       )
    (multiple-value-bind (dirpath2 dir-created-p)
        (ensure-directories-exist dirpath)
      (setf dirpath1 (substitute #\/  #\\ dirpath2))

      (values dirpath1 dirpath2 dir-created-p)
      )))
;;TEST
;; (make-directory "c:\\temp2\\") = "c:/temp2/"  "c:\\temp2\\"  NIL
;; (make-directory "c:/temp3/temp/new/") = "c:/temp3/temp/new/"   "c:/temp3/temp/new/"   T




;;MY-PARSE-PATHNAME
;;2017
;;ddd
(defun my-parse-pathname (pathname &key pathnamestr  (convert-backslashes-p T))
  "In   RETURNS  (values  all-subdirs filename host directory pathnamestr)  INPUT: A string or real path.  Note: host returned as only a letter."
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
       )
    (when directory
      (setf directory (substitute #\/  #\\ directory)
       all-subdirs (divide-string-to-all-tokens directory :char-delim-list '(#\\ #\/ ))))

    (values  all-subdirs filename host directory pathnamestr)
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
(defun my-directory-namestring (pathstring)
  "In U-files, like directory-namestring, but returns dirname with FORWARD SLASHES INSTEAD OF BACKSLASHES  RETURNS (values  dir-namestring  backslash-dirname)"
  (let
      ((dir-namestring)
       (backslash-dirname (directory-namestring pathstring))
       )
    (setf dir-namestring (substitute #\/  #\\ backslash-dirname))
    (values  dir-namestring  backslash-dirname)
    ;;end let, defun
    ))
;;TEST
;;  (my-directory-namestring "C:\\TEMPX\\THIS\\THAT\\FILE.TXT")

;;    (substitute #\/  #\\    "C:\\TEMPX\\THIS\\THAT\\FILE.TXT")
;; (substitute #\/  #\\  "this\\that\\other\\") = "this/that/other/" WORKS
;; (substitute  #\x  #\c  "abcde") = "abxde" WORKS
;;Following don't work:
;; (substitute  "x"  "c"  "abcde") = "abcde"
;; (subst  "x"  "c"  "abcde") = "abcde"
;; (subst  "x"  "c"  '("a" "b" "c" "de") ) = ("a" "b" "c" "de")



;;MAKE-DATED-PATHNAME
;;
;;ddd
(defun make-dated-pathname (filename-base 
                &key (root "C:\\3-TS\\LISP PROJECTS TS\\")
                         (file-ext "lisp") no-date-p incl-time-p)
  "In U-files, takes a filename-base string and adds the root string before and the date plus period file-ext onto the end. If no-date-p, doesn't include date."
  (let
      ((pathname)
       (date)
       )
    (cond
     ((null no-date-p)
      (multiple-value-bind (date-time date time year mo day hour min)
          (my-get-date-time :date-separator "-")
        (cond
         (incl-time-p
          (setf pathname 
                (format nil "~A~A~A-~A-~A-~Ah~A.~A" root filename-base year mo day hour min file-ext)))
         (t (setf pathname 
                  (format nil "~A~A~A-~A-~A.~A" root filename-base year mo day file-ext))))
        ))       
     (t (setf date (my-get-date-time) 
              pathname (format nil "~A~A.~A" root filename-base  file-ext))))
    ))
;;TEST
;;  (make-dated-pathname "testfile") 
;; works = "C:\\3-TS\\LISP PROJECTS TS\\SHAQ-PLUS\\testfile2014-12-4.lisp"
;;  (make-dated-pathname "testfile" :incl-time-p T) 
;; works="C:\\3-TS\\LISP PROJECTS TS\\SHAQ-PLUS\\testfile2014-12-4-10h25.lisp"
;;  (make-dated-pathname "testfile" :no-date-p T)
;; works= "C:\\3-TS\\LISP PROJECTS TS\\SHAQ-PLUS\\testfile.lisp"
   
    
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
       "U-files.  RETURNS (values formated-datafile-string all-sym-data-lists)   If title, incl title.   Makes formated data with (setf sym '( data )) for symbols which eval to their data. Eg. ;;FOR DATE: Date: 2018/08/29 16:29:02
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


;;CLEAN-PATH-STR
;;2017
;;ddd
(defun clean-path-str (pathstr &key enclose-in-quotes-p  remove-extra-quotes-p)
  "In U-files.  Substitutes #\/for  #\\  and removes any extra spaces at beginning or end of pathstr, which is necessary for proper paths in dos eg my-dos-dir. ENCLOSE-IN-QUOTES-P is for use in DOS-related situations where there is a space between parts of a dir or filename."
   (setf pathstr (substitute #\/ #\\  pathstr)
         pathstr (my-delete-first-spaces pathstr)
         pathstr (my-delete-last-spaces pathstr))
   (when enclose-in-quotes-p
     (setf pathstr (format nil "~S" pathstr)))
   (when remove-extra-quotes-p
     (setf pathstr (my-delete-first-spaces  (my-delete-last-spaces  pathstr :delete-others '(#\")) :delete-others '(#\"))))
   pathstr
   ;;end clean-path-str
   )
;;TEST
;; (clean-path-str "  D:\\   ")  =  "D:/"
;; (clean-path-str "    D:\\1 THIS DIR\\2 THAT DIR\\MY FILE.LISP    ")
;; works= "D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP"
;;enclose-in-quotes-p
;; (clean-path-str "    D:\\1 THIS DIR\\2 THAT DIR\\MY FILE.LISP    " :enclose-in-quotes-p T)
;; works= "\"D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP\""
;;
;; NOW REMOVE EXTRA QUOTES 
;;  (clean-path-str    "  \"C:/\"" :remove-extra-quotes-p T)  =  "C:/"

;;   (clean-path-str    "\"D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP\"" :remove-extra-quotes-p T)
;;  works=  "D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP"
;; what if no extra quotes? still works, see below.
;; ;;   (clean-path-str "D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP" :remove-extra-quotes-p T)  = "D:/1 THIS DIR/2 THAT DIR/MY FILE.LISP"

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
(DIRECTORY "c:\\temp\\*.*"
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