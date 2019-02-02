;; ***************************** U-java-to-lisp.lisp *****************************
;;
;; FUNCTIONS TO CONVERT JAVA CODE TO LISP OR LISP-READABLE CODE
;;
;;IDEAS
;; 1- USE FORMAT CONDITIONAL TO MAKE \N = #\NEWLINE 
;; 2-USE + TO SIGNAL CONCAT, MERGE OR WHATEVER WORKS
;; 3- MAKE ; INTO A NEWLINE OR IGNORE??
;; 4- EARLY  * OR // TO IGNORE LINE
;; 5-  X = Y   TO   (SETQ X Y) -- see below
;; 6-  public static [String/int/etc] variable = value;   TO  (defparameter varible value)
;; 7- USE PCategory and question processing functions for those specific instances     
;;

;;PROCESS-JAVA-LINE-STRING
;;
;;newer than PROCESS-JAVA-STRINGS below
;;
;;ddd
(defun process-java-line-string (line &key convert-to-objects-p force-line-process-p
                                      (delete-str-list '("+" "=")))
  "In U-java-to-lisp.lisp, returns a list of lisp objects from the string.RETURNS (values  new-line   extra-line).  force-line-process-p (eg. when want to process a java line combo of symbol = a string) Use convert-to-objects-p = t then."
  (let*
      ((new-line)
       (line-objects)
       (extra-line)
       (length-line (length line))
       (start-n 0)
       (end-n)
     ;;  (continuouts-string-p)
      (process-line-p)
       )
    (cond
          ((> length-line 9)
           (setf start-n (- length-line 4)))
          (t setf start-n 0))
    (cond
          ((> length-line 10)
           (setf end-n 10))
          (t setf end-n length-line))
;;yyy
    (cond
     ((match-substring "//" line  :end end-n ) 
      ;;quoted line, add to extra-line
      (setf extra-line (append extra-line (list line))) )
     ((match-char-list-to-string '("+" "=" "\"") line :start start-n)
      (setf continuouts-string-p t
            process-line-p t))
     ;;are one of first 3 chars a \" ?
     ((and (> length-line 3)(match-substring "\"" line :start start-n))
      (setf continuouts-string-p nil
            process-line-p t))
     ;;may want to process the line as a combination of java symbols and strings
     (force-line-process-p 
      (setf process-line-p t))
     (t nil))

    (cond
     ((and process-line-p )
      ;;delete +'s, = and substitute newlines for \n here
      (setf new-line (my-delete delete-str-list line)
            new-line (replace-java-newline new-line))
 
      (if convert-to-objects-p ;;Note: must do after replace newlines or error
          (setf new-line  (convert-string-to-objects  new-line))) )
     (t (setf extra-line (append extra-line (list line)))))
#|    (if (or continued-string-list continuouts-string-p)
        (setf continued-string-list (append continued-string-list (list new-line))))|#

#|    (setf  continued-string-list (append continued-string-list (list input-continuous-string-line)))|#
    ;;(afout 'out (format nil "line= ~A~% length-line= ~A~% continuouts-string-p= ~A~% process-line-p= ~A~% continued-string-list= ~A~%  "line  length-line continuouts-string-p process-line-p continued-string-list))
    ;;end process-java-line-string
   (values  new-line  extra-line)
   ))

;;test
#|"
;; (process-java-line-string   "       \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n\"+   "  :convert-to-objects-p t :force-line-process-p t)
;;works, returns ("INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n")  (("INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n"))  T
NIL
;; (process-java-line-string " smtBALanceQ =
    \"I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.\" +  " :convert-to-objects-p t :force-line-process-p t)
;;works, returns (SMTBALANCEQ "I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.") [second copy in a list] T NIL
;;   (my-substitute  "
" "\\n"  "INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n") ;; :test 'string-equal)
; ;(process-java-line-string " \"I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.\";")
;;works--returns " \"I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.\""
;; (process-java-line-string " \"I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.\";" :convert-to-objects-p t)
;;works, returns ("I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.")
|#
 
#|
    //SRQ--ROMANCE
public static final String romSrq1SurpriseQ =
       "INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n"+
       "  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n"+
       "Question 1. I do something different to surprise my partner such as buy flowers, leave a love note, or buy a present for no special event at least once a week.";
public static final String romSrq4FantasizeQ = "I frequently fantasize about my partner.";
|#

;;REPLACE-JAVA-NEWLINE
;;
;;ddd
;;SSS START HERE DEBUG
(defun replace-java-newline (java-line &optional  (start 3))
  "In U-java-to-list. Replaces all end java newlines with lisp newlines. Also deletes end   end pluses on java lines."
  (let
      ((readline)
       (length-line (length java-line))
       )
      (cond
       ((> length-line  start)
        (setf start  (- length-line  start)))
       (t (setf start 0)))
    ;;(afout 'out (format nil "start= ~A length-line= ~A~%" start length-line))
     ;;work done here
  (setf newline (substitute #\newline #\n java-line :start start :end nil
                    :from-end t :count 2   :test 'char-equal))
   ;;gets error  (setf newline (substitute #\newline #\n readline :start start ))
     ))
;; (replace-java-newline *java-test-line)
;; (string #\newline)
;; (format nil "
;; (substitute #\newline #\n "abncdnef gh\n" :start 0 :end nil :from-end nil :count nil   :test 'char-equal) = replaces ALL n's.
;; (my-substitute (format nil " ~%") (format nil "\n")  "  \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n " :from-end-p t :start 4) :count 2)
;; (find #\\  (read-from-string "  \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n \" ") :test 'char-equal))
;; (find  (format nil "~A~A" #\\ #\n) "  \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n ")
;; (setf *java-test-line (read-from-string " \" Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\\n\n\"+ ")) = .nn"
 ;;  (substitute #\newline #\n *java-test-line :start 340 :from-end t) replaces all end n's
;;  (concatenate 'string "\\" "n")
;;(progn (setf out nil)(replace-java-newline "public static final String romSrq4FantasizeQ = \"I frequently fantasize about my partner.\n\";"))


;;CONVERT-JAVA-FILE
;;
;;ddd
;;SSS FIX THIS LATER???
#|(defun convert-java-file (pathname &optional add-newline-p)
  "In U-java-to-lisp.lisp. Default for add-newline-p = T."
  (let
      ((all-lines)
       (new-line-list)
       (n-lines 0)
       (file-max-lines 1000)
       )
    (unless (null add-newline-p)
      (setf add-newline-p T))
    
    (with-open-file (instream pathname :direction :input)
      (loop 
       for n from 0 to file-max-lines
       with line
       ;;YYY 
       do
       (incf n-lines)
       (setf line (read-line instream nil 'eof )) 
     ;;  (afout 'out (format nil "n= ~A line= ~A~%" n line ))
     ;;  (afout 'out (format nil "n= ~A line= ~A~% all-lines= ~A" n line all-lines ))
       (cond
        ((and line (not (equal line  'eof)))
         (unless (null add-newline-p)
             (setf line (format nil "~A~%" line)))

         ;;SSS WRITE LINE PROCESSING HERE --USE FUNCTIONS?

          (setf new-line-list (process-static-variable line))
      ;;doesn't work well   (setf new-line (process-static-SHAQ-variable line))

         (setf all-lines (append all-lines (list new-line-list))))
        (t (return)))
       ;;termination test 2
       (if (equal line 'eof) ;;can be 'eof
           (return))
       ;;end loop, with-
       ))
    all-lines
    ))
|#
;;test
;;  (convert-java-file "C:\\TOM\\LISP PROJECTS TS\\SHAQ\\calc-scale-scores.lisp")



     
;;delete  =  +  ;
;;  (read-from-string " \"this substring.\" ")
;; returns  "this substring."   19
;; (read-from-string "that  \"this substring.\"; this ")
;;returns  THAT   5

(defun testds2 ()
  (let
      ((string " public static final String smtBALanceQ =
    \"I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.\";")
       (a)(b)(c)(d)(e)
       )
  (multiple-value-setq (a b c d e)
    (match-token-phrase  '( public static final String) string))
#|    ;;;; :no-initial-spaces t
                             :ignore-char-list  '("+" ";" = #\= #\+ #\; )                ;;nth-char nth-word 
                            ;;max-length word-delim-list 
                            :delete-list '("+" ";" = #\= #\+ #\; )))|#
  (values a b c d e)
    ))
;; (my-delete '("+" "=" ";") " smtBALanceQ =     \"I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.\";")
;;works, returns=   " smtBALanceQ      \"I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.\""
;;use read-from-string
;;(read-from-string "smtBALanceQ  \"I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.\"" t nil  :start 15)



;;PROCESS-JAVA-STRING-LIST
;;
;;ddd
(defun process-java-string-list (string-list)
      "In U-java-to-lisp.lisp,"
      (let
          ((all-processed-string-lines)
           (processed-lines)
           (continuous-string-lines)
           (process-next-p)
           (extra-lines)
           (all-extra-lines)
           (all-lines)
           )
        (loop
         for line in string-list
         with length-line 
         with start-n = 0
         with n = 0
         do
         (incf n)
         (setf length-line (length line))
         (cond
          ((> length-line 9)
           (setf start-n (- length-line 7)))
          (t setf start-n 0))
         ;;(afout 'out (format nil "at line n= ~A~%length-line= ~A start-n= ~A~% line= ~A~%" n length-line start-n line))

         (cond
          ;;continuous line 
          ((match-substrings '("=" "+") line :start start-n)           
           (multiple-value-setq (processed-lines process-next-p extra-lines)
             (process-multiline-javastring line continuous-string-lines))
           (setf continuous-string-lines  processed-lines)
           ;;(afout 'out (format nil "FOR CONTINUOUS LINE process-next-p= ~A~% processed-lines= ~A~%" process-next-p processed-lines))
           )
          ;;final line in continuous
          ((and (match-substring  ";" line :start start-n)  process-next-p)
           (multiple-value-setq (processed-lines process-next-p extra-lines)
               (process-multiline-javastring line continuous-string-lines))
           (setf all-processed-string-lines
                 (append all-processed-string-lines (list processed-lines)
                         process-next-p nil)
                 continuous-string-lines nil)
            ;;(afout 'out (format nil "FOR LAST CONTINUOUS LINE process-next-p= ~A~% processed-lines= ~A~%" process-next-p processed-lines))
           )
          ;;single termnated line
          ((match-substring  ";"  line :start start-n)
           ;;(afout 'out (format nil "FOR  SINGLE LINE  process-next-p= ~A~%  processed-lines= ~A~%" process-next-p processed-lines))
           (multiple-value-setq (processed-lines process-next-p extra-lines)
             (process-multiline-javastring line nil))
           (setf  all-processed-string-lines 
                  (append all-processed-string-lines  (list processed-lines)))
           )
          (t nil))

         (if extra-lines
             (setf all-extra-lines (append all-extra-lines (list extra-lines))))
         ;;all lines no matter what
         (setf  all-lines (append all-lines (list processed-lines)))
         ;;end loop
         )
         (values all-processed-string-lines  all-extra-lines) ;; all-lines)
         ))

;;test
;;(progn (setf out nil) (print-list (process-java-string-list *test-java-str1) :stream t :incl-parens-p t :incl-quotes-p t))
;;
 (defparameter *test-java-str1
   '("public static final String romSrq1SurpriseQ ="
     "  \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n\"+ "
     "  \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n\"+"
    "   \"Question 1. I do something different to surprise my partner such as buy flowers, leave a love note, or buy a present for no special event at least once a week.\"; "
 "//this is a test"
 "public static final String romSrq4FantasizeQ = \"I frequently fantasize about my partner.\";"
 ))

 ;; (match-substring  "="    "public static final String romSrq1SurpriseQ ="   :start 20)  
;;   (match-substrings ";"  "   \"Question 1. I do something different to surprise my partner such as buy flowers, leave a love note, or buy a present for no special event at least once a week.\"; " :start 60)

#|
;;REDUNDANT--
;;  SUPERCEDED BY CONVERT-SHAQ-QUESTION-FILE FUNCTION
;;
;;PROCESS-MULTILINE-JAVASTRING
;;
;;ddd
(defun process-multiline-javastring (line previous-lines)
    "In U-java-to-lisp.lisp, takes a single multi-java-line java String and converts to a lisp string. RETURNS a lisp string. Also marks beginning and end of the single string from  inputed lines from java-line-list."
    (let*
        ((length-line (length line))
         (last5chars-n (- length-line 6))
      ;;  (process-nextline-p)
       ;;  (continuouts-string-p)  ;;not really used here
         (all-processed-lines)
         )
#|      (if (> last5chars-n  length-line)
          (setf last5chars-n 0))      
      
       ;;is the line NOT the end line of the string?
       (if (match-substrings '("+" "=") line :start last5chars-n)
        (setf  process-nextline-p t))  |#

       ;;if it is the end line, should end with a semi-colon (java) THIS GOES LAST?
#|       (if (match-substring ";" line :start last5chars-n)
        (setf process-nextline-p nil))|#

      ;;in any case process this line and add to list
      (multiple-value-setq (new-line extra-line)
          (process-java-line-string line :convert-to-objects-p t :force-line-process-p t ))
      ;;add to previous lines
        (setf all-processed-lines (append previous-lines  new-line))         

        (values all-processed-lines  extra-line)
      ))
|#
;;test
;;  (process-multiline-javastring "  \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n\"+ " '("public static final String romSrq1SurpriseQ ="))

#|
;;older version contains redundant tests

(defun process-multiline-javastring (line previous-lines)
    "In U-java-to-lisp.lisp, takes a single multi-java-line java String and converts to a lisp string. RETURNS a lisp string. Also marks beginning and end of the single string from  inputed lines from java-line-list."
    (let*
        ((length-line (length line))
         (last5chars-n (- length-line 6))
         (process-nextline-p)
         (continuouts-string-p)  ;;not really used here
         (all-processed-lines)
         )
      (if (> last5chars-n  length-line)
          (setf last5chars-n 0))      
      
       ;;is the line NOT the end line of the string?
       (if (match-substrings '("+" "=") line :start last5chars-n)
        (setf  process-nextline-p t))  

       ;;if it is the end line, should end with a semi-colon (java) THIS GOES LAST?
       (if (match-substring ";" line :start last5chars-n)
        (setf process-nextline-p nil))

      ;;in any case process this line and add to list
      (multiple-value-setq (new-line continued-string-list continuouts-string-p extra-lines)
          (process-java-line-string line :convert-to-objects-p t :force-line-process-p t ))
      ;;add to previous lines
        (setf all-processed-lines (append previous-lines  new-line))         

        (values all-processed-lines process-nextline-p extra-lines)
      ))
|#
;;test
;;  (process-multiline-javastring "  \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n\"+ " '("public static final String romSrq1SurpriseQ ="))


;;PROCESS-JAVA-STRINGS  SSS NEEDS WORK
;;  USE MAIN PART OF CONVERT-SHAQ-QUESTION-FILES
;;
;;ddd
(defun process-java-strings (string-list)
  "In U-java-to-lisp.lisp. For a list of actual java Strings to convert to lisp strings."
  (let
      ((new-string)
       (current-lines "")
       (add-next-string-p)
       (subtract-next-string-p)
       (all-lines)
       )
    (loop
     for string in string-list
     do
     (cond
      ;;if line end ;, ignore it
      ((member string '(";" ";;" " " "  ") :test 'equal) NIL)
      ;;next string is quoted off
      ((member string '("/" "//" "///" "*" "*/" "/*") :test 'equal)
               (setf subtract-next-string-p T))
      (t
       ;;sub llisp newline for java newline
       (setf new-string  (my-substitute  (format nil " ~%") "\n" string))
       ;;current-lines beginning string set here
       (setf current-lines new-string)

       ;;if  java line has a +, add next line to current-lines, then when no + add current-lines to all-lines
       (cond
        (subtract-next-string-p
         (setf subtract-next-string-p nil))
        ((find #\+ new-string)
         (setf add-next-string-p t))
        (t
         (cond
          ;;if there was a + on previous string SSS- MAY WANT TO DELETE ~%
          (add-next-string-p ;;may not need this (not (equal current-lines ""))
                             (setf current-lines (format nil "~A~A" current-lines new-string)
                                   add-next-string-p nil))
          (t 
           (setf all-lines (append all-lines (list current-lines))
                 current-lines "")))))
       ;;end cond
       )) 
     ;;(afout 'out (format nil "new-string= ~A~% add-next-string-p= ~A~% current-strings= ~A~% all-lines= ~A~%" new-string add-next-string-p  current-lines all-lines))
     ;;end loop
     )
    all-lines
    ))
;;tests
;; ;;(my-substitute  (format nil " ~%") "\n" "this n is an  test \n")  = replaces all n's

;;SSS START HERE DEBUGGING
;;  (testsl)
;; (print-list (testsl)  :stream t :incl-quotes-p t :incl-parens-p t)
(defun testsl ()
  (setf out nil)
  (process-java-strings   *test-output) ;;or *test-java-NL
  )
;;works, returns:
(setf *test-output
 '( "
//   public static final String x10Label = " ";"

"    //SRQ--ROMANCE
public static final String romSrq1SurpriseQ =
       \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n\"+
       \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n\"+
       \"Question 1. I do something different to surprise my partner such as buy flowers, leave a love note, or buy a present for no special event at least once a week.\";
public static final String romSrq4FantasizeQ = \"I frequently fantasize about my partner.\";
public static final String romSrq5CelebrQ = \"My partner and I celebrate special days together almost once a month.\";
"))
`(" ;;********************** test-read-java-file.lisp ************************" " " " ;;IDEAS" " ;; 1- USE FORMAT CONDITIONAL TO MAKE  
 = # 
EWLINE " " ;; 4- EARLY  * OR // TO IGNORE LINE" " ;; 5-  X = Y   TO   (SETQ X Y) -- see below" " ;; 6-  public static [String/int/etc] variable = value;   TO  (defparameter varible value)" " ;; 7- USE PCategory and question processing functions for those specific instances     " " " "    public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";" " " " " " " " " " " " " " " " " "     //SRQ--INTIMACY" "    public static final String intSrq6ExtraQ =" "    public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";" "      //same as crSrq30RealQ" "    public static final String intSrq30RealQ = \"We usually discuss what is really bothering us (the underlying issues) instead of the surface issues.\";" "    public static final String intSrq8TellAllQ = \"I have told my partner almost everything about myself.\";" " " " " " " "     PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" "     questionInstancesArray[0] = intSrq6Extra;" "     PCategory intSrq7Commit = new PCategory(\"intSrq7Commit\",2, intSrq7CommitQ, \"int\",  FrAnswerPanel.LikeUs7Reverse,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" "     questionInstancesArray[1] = intSrq7Commit;")
;;TO SEE THIS WITH NEWLINES, USE PRINT-STRING-LIST BELOW


;;TEST READING ACTUAL FILES WITH  test-read-java-file.lisp  IN MyUtilities

;;    public static final String intSrq8TellAllQ = "I have told my partner almost everything about myself.";

(defun process-static-variable (string)
  "U-java-to-lisp.lisp."
  (let
      ((rest-string)
       (first-string)
       (result-string-list)
       (token-matched-p)
       (set-symbol)
       (string-value)
       (var-type)       
       (last-pt)
       (new-word-list)
       (new-line)
       (new-lines-list)
       )

    (multiple-value-setq (rest-string first-string result-string-list token-matched-p)
          (match-token-phrase '(public static) string))

    (cond
     ;;test--is line setting a static variable (ie does line start with   public static final)
     (token-matched-p
      ;;if so, break into tokens, first token tells type of  variable being set
      (multiple-value-setq (first-pt last-pt new-word-list)
          (divide-string-to-tokens rest-string '(" ") :ignore-char-list '(";")))
      ;;find the name of  set-var
      (setf  var-type (first new-word-list)
             set-var (second new-word-list))
      ;;find the string
      (setf value (car (find-strings-in-string last-pt)))
      ;;from type of variable, process differently
      ;;(afout 'out (format nil "var-type= ~A~%set-var= ~A~% value= ~A~%first-part=~A~% last-pt= ~A~% new-word-list= ~A~%" var-type set-var value first-pt last-pt new-word-list))
      (cond
       ;;for string and int variables, create the new line =  (SETQ SET-VAR VALUE)
       ((member var-type '("string" "int") :test 'string-equal)
        (setf  new-line (format nil "(setf  ~A ~A)~%" set-var value)
               new-lines-list (append new-lines-list (list new-line)))
        ;;(afout 'out (format nil "new-line= ~A~%" new-line))
        )       
       (t
        ;;SSS START HERE
        ;;PROCESS THE INSTANCE ETC VARIABLES -- TURN INTO CLASSES


               
        ))
      ;;delete and "" items SSS CHECK
      ( delete-list-items '(nil "") new-lines-list)
      ;;(afout 'out (format nil "new-line= ~A~% new-lines-list= ~A~%" new-line new-lines-list))
      ;;end past static var test clause
      )
     (t nil))
    ;;end print-string-list
    new-lines-list
    ))

;;SSS FIRST, START HERE DEBUGGING
;;test
;;  (testpsv)
(defun testpsv ()
  (setf out nil)
  (let
      ((test-line1 (fourth *test-output))
       (test-line2 " public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";")
       )
    (setf result  (process-static-variable test-line2))
    ))

;;(multiple-value-setq (first-pt last-pt new-word-list)
;;    (divide-string-to-tokens test-line2 '(" ") :ignore-char-list '(";" "") :no-initial-spaces T)) ;;works, returns:
#|"public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";"
("public" "static" "final" "String" "intSrq7CommitQ" "=" "\"A" "long" "term" "commitment" "(would)" "cause(s)" "me" "to" "feel" "trapped" "\";")|#
    

;; CONVERT-JAVA-STRING-ARRAY
;;
;;ddd
(defun convert-java-string-array (sting-array-line)
  "In U-java-to-lisp.lisp, converts the actual string array: {\"x\", \"y\", \"z\"}; after slashes manually or otherise are added."
  (let
      ((newlist-str (my-delete '("," ";") sting-array-line))
       (newlist)
       )
    (setf newlist-str (my-substitute "'(" "{" newlist-str)
          newlist-str (my-substitute ")" "}" newlist-str))
    (if (stringp newlist-str)
        (setf newlist (read (make-string-input-stream newlist-str))))
    (values newlist newlist-str)
    ))
;;test
;;  (convert-java-string-array "{\"12 or more\",\"11\",\"10\",\"9\",\"8\",\"7\",\"6\",\"5\",\"4\",\"3\",\"2\",\"1\",\"0\"};")
;;works, returns (QUOTE ("12 or more" "11" "10" "9" "8" "7" "6" "5" "4" "3" "2" "1" "0"))    "'(\"12 or more\"\"11\"\"10\"\"9\"\"8\"\"7\"\"6\"\"5\"\"4\"\"3\"\"2\"\"1\"\"0\")"

 

;;SSS START HERE  -- ALSO RE-WRITE MY-SUBSTITUTE?
;; WRITE A MY-SUBSTITUTE  MATCHED PAIRS (EG  #\NEWLINE FOR  \N ,
;;    "  " OR #\SPACE FOR  " +  "  ??
;;
;;   SOMEHOW USE (FORMAT ??? --- LOOK IT UP??
;;
#|  "INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n"+
   "  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n"+
   "Question 1.If I am under more stress than usual, my partner will usually do extra things for me.";|#



(defun java-to-lisp-pathname (java-pathname)
  "In U-java-to-lisp, changes path extension from java to lisp"
  (let
      ((lisp-pathname)
       (path-no-extension)
       )
    (setf path-no-extension (my-substitute "lisp" "java" java-pathname))))
;;test
;;  (java-to-lisp-pathname "C:\\TOM\\LISP TS\\testpath.java")
