;; ************************* U-java-to-lisp-shaq.lisp ***********************
;;
;;;;put in all key files
  (my-config-editor-after-start)

;; ---------------------  SHAQ LISP FILE PROCESSING -----------------------
#|
from q-extra-lines.lisp
 "  public static String frameTitle = \"Romantic Relationship\";"
 "  public static int frameDimWidth = 805;"
 "  public static int frameDimHeight = 460;"
 "  public static int totalQuestions = 3;"
 "  public static String helpInfo = \"\";"
 "  public static HelpResource helpResource;"

 " public static String[] rpeCommitTextArray ="
 "  {\"Married--extremely high commitment\","
 "  \"Living together--extremely high commitment\","
 "  \"Not living together--extremely high commitment\","
 "  \"Married--moderate commitment\","
 "  \"Living together--moderate commitment\","
 "  \"Not living together--moderate commitment\","
 "  \"Married--low commitment\","
 "  \"Living together--low commitment\","
 "  \"Not living together--low commitment\","
 "  \"Dating one person regularly, but not in a committed relationship\","
 "  \"Previously married, but not now in a committed relationship\","
 "  \"Previously in a committed relationship, but not now\","
 "  \"Never in a committed relationship\"};"
 "  public static String helpInfo = \"\";"
 "  public static HelpResource helpResource;"

 "    rhlDrugs.helpInfo = \"Continued use of drugs usually causes avoidance of coping with problems, guilt, and failure in relationships, work, etc.  It can also lead to serious health problems.\";"
 "    rhlDrugs.helpLink = HelpLink.substanceAbuse;"
|#

;;PROCESS-STATIC-VAR-LIST
;;
;;ddd
(defun process-static-var-list  (extra-line-list)
  (let
      ((all-static-var-lists '(STATIC-VARS))
       (all-other-var-lists '(INFO-VARS))
       )
    (loop
     for file-list in extra-line-list
     with filename
     with line-list 
     with file-other-info-list
     with file-static-vars
     do
     (setf filename (second file-list)
           line-list (cddr file-list)
           file-static-vars (list 'STATIC-VARS filename)
           file-other-info-list (list 'OTHER-INFO filename))

     (loop
      for line in line-list
      with var 
      with value
      with static-var-list
      do
     
      (cond
#|       ((multiple-value-setq (rest-string first-string result-string-list token-matched-p)
                 (match-token-phrase  '(public static) line))
             (setf static-var-list  (process-java-line-string rest-string)
                   file-static-vars   (append file-static-vars (list static-var-list)))
             ;;end NON-STRING STATIC VARIABLE
             )|#
       ((multiple-value-setq (value-str first-pt var-str)
        (match-substrings  '( "frameTitle""helpInfo" "helplink" "HelpResource" "frameTitle" ".\"" "String[]" "frameDimWidth" "frameDimHeight" "totalQuestions") line))
        (setf static-var-list (list :var  var-str :value value-str :filename filename)
                   file-static-vars   (append file-static-vars  (list static-var-list))))
       (t 
        (setf  file-other-info-list (append file-other-info-list (list line)))))
        
      ;;end inner loop
      )
     (setf all-static-var-lists (append all-static-var-lists (list file-static-vars))
            all-other-var-lists  (append all-other-var-lists (list file-other-info-list)))
     ;;end outer loop
     )      
      ;;end process-static-var-list
      (values all-static-var-lists  all-other-var-lists)
      ))
       
;;test
;; step 1: (multiple-value-setq (*all-static-vars1 *all-info-vars2)  (process-static-var-list *shaq-extra-lines-list))
;; step 2: (print-nested-list-to-file *all-static-vars1 "C:\\TOM\\LISP PROJECTS TS\\SHAQ\\Q-static-vars2.lisp" :print-fewer-newlines-p t)

 ;;(match-substrings '( "frameTitle""helpInfo" "helplink" "HelpResource" "frameTitle" ".\"" "String[]" "frameDimWidth" "frameDimHeight" "totalQuestions")  "  public static String frameTitle = \"Romantic Relationship\";")
#|
;;GET-EXTRA-LINES-VARS
;;
;;ddd
(defun get-extra-lines-vars (shaq-extra-lines-vars)
  "In U-java-to-list-shaq.lisp, gets some important info from Q-extra-lines.lisp file using defparameter it's set to."
  (let
      ((ex-file-list)
    ;;   (q-pc-name-list)
    ;;   (q-string-list)
       (extra-items-list)
  ;;     (return-sym-list)
       (return-ex-list)
       (return-ex-file-list)
       )
    (loop
     for ex-file-list  in shaq-extra-lines-vars
     with ex-filename 
     with file-ex-list 
     with new-ex-list
     do


     (afout 'out (format nil "1 ex-file-list= ~S~% Length= ~A~%" ex-file-list (list-length ex-file-list)))
     (cond
      ((and (listp  ex-file-list) (> (list-length ex-file-list) 2))
       (setf ex-filename (second ex-file-list)
             file-ex-list (cddr ex-file-list))
     ;;  (afout 'out (format nil "2 file-ex-list= ~S~%" file-ex-list))

     (if match-filename-list
         (cond
          ((member ex-filename match-filename-list :test 'equal)
           (setf return-ex-file-list (append return-ex-file-list (list ex-file-list))))
          (t nil)))

       (cond
        ((and (listp file-ex-list) (> ( list-length file-ex-list) 0))

         (loop
          for  ex-list in file-q-list
          with ex-name-str
          with ex-name
          with ex-num
          with ex-q-name
          with ex-type
          with ex-q-array
          with fr-title
          with fr-width
          with fr-height
          do
          (setf ex-name (first ex-list)
                q-string (second ex-list))
          (afout 'out (format nil "3 ex-name= ~A~% ex-list= ~A~%" ex-name ex-list))
          (if match-q-ex-name-list
              (setf ex-name-str (string ex-name))
              (cond
               ((member ex-name-str match-ex-name-list :test 'equal)
                (setf new-ex-list  (list ex-name q-string ex-filename)
                return-ex-list (append return-ex-list (list new-ex-list))
                return-sym-list (append return-sym-list (list ex-name)))
                )
               (t nil)))
          ;;end inner loop, listp
          ))
        (t nil ))  ;;(setf extra-items-list (append extra-items-list (list ex-file-list)))))
       ;;end listp
       )
      (t nil))  ;;(setf extra-items-list (append extra-items-list (list ex-file-list)))))
     ;;end outer loop
     )
     ;;end 
     (values return-ex-list extra-items-list return-sym-list return-ex-file-list)
     ))
|#


;;XXX ------------------ FOR Q-PC-INSTANCES.LISP FILE -----------------

;;ALL-SHAQ-PCINSTANCES-LIST
;;
;;ddd
(defun all-shaq-pcinstances-list (&key match-pc-name-list
                                                   match-filename-list)
  "In U-java-to-lisp-shaq.lisp, returns list of all pc-instance lists matching match-pc-name-list ."
  (let
      ((q-file-list)
       (q-pc-name-list)
       (q-string-list)
       (extra-items-list)
       (return-sym-list)
       (return-q-list)
       (return-q-file-list)
       )
    (loop
     for q-file-list  in all-shaq-quest-list
     with q-filename 
     with file-q-list 
     with new-q-list
     do
     (if match-filename-list
         (cond
          ((member pc-name match-filename-list :test 'equal)
           (setf return-q-file-list (append return-q-file-list (list q-file-list))))
          (t nil)))

     (afout 'out (format nil "1 q-file-list= ~S~% Length= ~A~%" q-file-list (list-length q-file-list)))
     (cond
      ((and (listp  q-file-list) (> (list-length q-file-list) 2))
       (setf q-filename (second q-file-list)
             file-q-list (cddr q-file-list))
     ;;  (afout 'out (format nil "2 file-q-list= ~S~%" file-q-list))
       (cond
        ((and (listp file-q-list) (> ( list-length file-q-list) 0))

         (loop
          for  q-list in file-q-list
          with pc-name-str
          with pc-name
          with pc-num
          with pc-q-name
          with pc-type
          with pc-q-array
          with fr-title
          with fr-width
          with fr-height
          do
          (setf pc-name (first q-list)
                q-string (second q-list))
          (afout 'out (format nil "3 pc-name= ~A~% q-list= ~A~%" pc-name q-list))
          (if match-q-pc-name-list
              (setf pc-name-str (string pc-name))
              (cond
               ((member pc-name-str match-pc-name-list :test 'equal)
                (setf new-q-list  (list pc-name q-string q-filename)
                return-q-list (append return-q-list (list new-q-list))
                return-sym-list (append return-sym-list (list pc-name)))
                )
               (t nil)))
          ;;end inner loop, listp
          ))
        (t nil ))  ;;(setf extra-items-list (append extra-items-list (list q-file-list)))))
       ;;end listp
       )
      (t nil))  ;;(setf extra-items-list (append extra-items-list (list q-file-list)))))
     ;;end outer loop
     )
     ;;end 
     (values return-q-list extra-items-list return-sym-list return-q-file-list)
     ))
;;test
;; (get-pcinstance-var-lists *all-shaq-pc-instances  :match-pc-name-list '(
                                                 ;;  match-filename-list)




;;xx ---------------------- FOR Q-QUESTION.LISP FILE -------------------------

#|
;;Format within each q-questions.lisp file sublist is:
   (STRING-VARS
    Filename-string
;; then for each question
    (Question symbol (note: some don't have a Q or may miss part of ending??)
    Question String)
    ;;REST OF FILE QUESTIONS
    )
   (cddr '(STRING-VARS
    Filename-string
    (Question symbol 
    Question String)
    (Question symbol 
    Question String)
    (Question symbol 
    Question String)
    ))
|#
;;
;;GET-QUESTION-VAR-LISTS
;;
;;ddd
;;SSS START HERE -- TEST DEBUG
(defun get-question-var-lists (all-shaq-quest-list &key match-q-sym-list
                                                   match-filename-list)
  "In U-java-to-lisp-shaq.lisp, returns list of all question symbols. match-q-sym-list must be symbols--or modify function."
  (let
      ((q-file-list)
       (q-sym-list)
       (q-string-list)
       (extra-items-list)
       (return-sym-list)
       (return-q-list)
       (return-q-file-list)
       )
    (loop
     for q-file-list  in all-shaq-quest-list
     with q-filename 
     with file-q-list 
     with new-q-list
     do
     (if match-filename-list
         (cond
          ((member q-sym match-filename-list :test 'equal)
           (setf return-q-file-list (append return-q-file-list (list q-file-list))))
          (t nil)))

     (afout 'out (format nil "1 q-file-list= ~S~% Length= ~A~%" q-file-list (list-length q-file-list)))
     (cond
      ((and (listp  q-file-list) (> (list-length q-file-list) 2))
       (setf q-filename (second q-file-list)
             file-q-list (cddr q-file-list))
     ;;  (afout 'out (format nil "2 file-q-list= ~S~%" file-q-list))
       (cond
        ((and (listp file-q-list) (> ( list-length file-q-list) 0))

         (loop
          for  q-ex-list in file-q-list
          with q-sym
          with q-string
          do
          (setf q-sym (first q-ex-list)
                q-string (second q-ex-list))
          (afout 'out (format nil "3 q-sym= ~A~% q-ex-list= ~A~%" q-sym q-ex-list))
          (if match-q-sym-list
              (cond
               ((member q-sym match-q-sym-list :test 'equal)
                (setf new-q-list  (list q-sym q-string q-filename)
                return-q-list (append return-q-list (list new-q-ex-list))
                return-sym-list (append return-sym-list (list q-sym)))
                )
               (t nil)))
          ;;end inner loop, listp
          ))
        (t nil ))  ;;(setf extra-items-list (append extra-items-list (list q-file-list)))))
       ;;end listp
       )
      (t nil))  ;;(setf extra-items-list (append extra-items-list (list q-file-list)))))
     ;;end outer loop
     )
     ;;end 
     (values return-q-list extra-items-list return-sym-list return-q-file-list)
     ))
;;test
;;works, returns the 4 lists (progn (setf out nil) (get-question-var-lists *all-shaq-questions  :match-q-sym-list '(SMTBUSYQ  CARIJOURNALISMQ  ACMESOCSTUDYQ CRSRQ26FINISHQ))) ;;  :match-filename-list)





;;ddd
#|
(defun get-Q-file-vars (java-filename q-symbol  all-shaq-quest-list)
  "In U-java-to-lisp-shaq.lisp, returns (list q-string q-symbol filename-sym) for the given question symbol."
  )|#



;;xxx ----------------- JAVA FILE CONVERSION FUNCTIONS ----------------

;;CONVERT-SHAQ-QUESTION-FILES 
;;
;;ddd
(defun convert-shaq-question-files (pathname-list &key  print-to-sum-files-p  
                                                         print-to-match-file-p)
  "In U-SHAQ, converts shaq java question files to lisp nested list format.  Separates questioninstance property lists, question string lists, and other static variables. RETURNS (values pc-all-instances all-string-object-lists all-class-static-var-lists). max-file-lines is 500 by default and is a protection against problems. results-list-p returns a list  of same instead of values. Note: large string processing section copied from process-java-string-list in U-java-to-lisp. Uses convert-shaq-question-file"
  (let
      ((x)
       )
    (loop
     for pathname in pathname-list
     do
     ;;process each file
     (convert-shaq-question-file pathname :results-list-p t
                                 :print-to-sum-files-p print-to-sum-files-p
                                 :print-to-match-file-p print-to-match-file-p ) ;;; out-pathname)

     ;;end loop
     )
         (format t ">>>>>> ALL FILE PROCESSING COMPLETED~%~%")
    ))
;;  (convert-shaq-question-files *all-shaq2000-question-files  :print-to-sum-files-p t  :print-to-match-file-p nil ) 



;;CONVERT-SHAQ-QUESTION-FILE
;;
;;SSS  MAKE FUNCTION OF MAIN SECTION 
;;           TO ALSO USE ON JAVA STRING LISTS?
;;
;;ddd
(defun convert-shaq-question-file (pathname &key max-file-lines results-list-p 
                                            print-to-sum-files-p print-to-match-file-p out-pathname)
  "In U-SHAQ, converts shaq java question files to lisp nested list format.  Separates questioninstance property lists, question string lists, and other static variables. RETURNS (values pc-all-instances all-string-object-lists all-class-static-var-lists). max-file-lines is 500 by default and is a protection against problems. results-list-p returns a list  of same instead of values. Note: large string processing section copied from process-java-string-list in U-java-to-lisp"
  (let* 
      ;;from convert-shaq
      ((input)
       (line)
       (eof-p)
       ;;question instances
       (pc-inst-list)
       (file-namestring  (file-namestring pathname))
       (all-pc-instances (list  'PC-INSTANCES file-namestring))
       ;;question strings
       (all-processed-string-lines (list  'STRING-VARS file-namestring))
       (processed-line)
       ;;new
       (string-array-list)
       (string-array-string)
       (all-string-array-lists)
        ;;end new
       (continuous-string-lines)
       (process-next-p)
       (extra-lines)
       (all-extra-lines (list 'EXTRA-LINES file-namestring))
       (all-print-lines (list 'PRINT-LINES file-namestring))
       ;;from convert-shaq
       ;;other static variables
       (class-static-var-list)
       (all-class-static-var-lists (list 'STATIC-VARS file-namestring))
       (all-deleted-lines (list 'DELETED-LINES file-namestring))
       )
    (unless max-file-lines
      (setf max-file-lines 500))
   
    (cond
     ((probe-file pathname)
      (with-open-file (input pathname :direction :input :if-does-not-exist nil) 
        (format t ">>>>>> PROCESSING FILE: ~A~%~%" pathname)
        ;;(afout 'out     (format nil ">>>>>> PROCESSING FILE: ~A~%~%" pathname))
        (setf file-namestring (file-namestring pathname))

        (loop
         for n from 0 to max-file-lines
         until (equal line 'eof)
         with rest-string
         with first-string 
         with result-string-list 
         with token-matched-p
         with length-line
         ;;from new
         with start-n = 0
         with qstart = 0
         with qlength-line = 0
         do

         ;;read the input line
         (multiple-value-setq (line missing-newline)
             (read-line input nil 'eof))

         ;;find length-line and set start-n to start near line end looking for terminators
         (unless (equal line 'eof)(setf length-line (length line)))
         (cond
          ;;set start-n (place to start checking for type of line end)
          ((> length-line 9)
           (setf start-n (- length-line 7)))
          (t setf start-n 0))

        ;; (afout 'out (format nil "line-n= ~A == ~A~% length-line= ~A start-n=~A~%" n line length-line start-n))
         ;;TEST EACH LINE
         ;;yyy
         (unless (or (equal line 'eof) (< length-line  2))  
           (cond
            ;;LINE IS DELETED LINE = quoted off with //
            ((and (> length-line 8) ( match-substring "//" line :end 8))
             (setf all-deleted-lines (append all-deleted-lines (list line))))

            ;;LINE IS A QUEST INSTANCE
            ((match-substring  "PCategory" line)
             (setf pc-inst-list (process-pcategory line)
                   all-pc-instances (append all-pc-instances (list pc-inst-list))))   

            ;;line is a print line = SYSTEM.OUT.PRINTLN
            ((match-substrings  '("PRINTLN" "SYSTEM.OUT") line)
             (setf all-print-lines (append all-print-lines (list line))))s

            ;;SSS  NEW ADDED - LINE IS A STRING ARRAY
#|            ((match-substrings "String[]" line)

          (multiple-value-setq (rest-string first-string result-string-list token-matched-p)
                      (match-token-phrase  '(public static final string[]) line))
             (multiple-value-setq (string-array-list string-array-string)
              (convert-java-string-array rest-string))

             ;;test to see if this line is end of array
             (setf qline-length (length rest-string))
             (if (> qline-length 6)
                 (setf qstart (- qline-length 5))
               (setf qstart 0))
             (cond
              ((match-substring  ";"  line :start qstart)
               (setf  all-string-array-lines
                      (append all-string-array-lines  (list string-array-list))
                      process-next-p nil))
              (t  (setf continuous-string-array-lines  string-array-list
                        string-array-list nil
                        process-next-p t)))

             ;;(afout 'out (format nil "FIRST STRING QUESTION line n= ~A~%qlength-line= ~A qstart= ~A~% rest-string= ~A~%process-next-p= ~A~%" n qlength-line qstart rest-string process-next-p))
             ;;end first (or only) line of question-string
             )|#
  ;;END OF NEW ADDED

            ;;SSS   LINE IS FIRST (or only) QUESTION STRING
            ;;first (or only) line of question-string:
            ((and (multiple-value-setq 
                      (rest-string first-string result-string-list token-matched-p)
                      (match-token-phrase  '(public static final string) line))
                  (match-substring "Q" line ))
             (multiple-value-setq (processed-line extra-line)
                 (process-java-line-string  rest-string
                                            :convert-to-objects-p t :force-line-process-p t))
             (setf qline-length (length rest-string))
             (if (> qline-length 6)
                 (setf qstart (- qline-length 5))
               (setf qstart 0))

             ;;end of string test.  set process-next-p T or NIL
             (cond
              ((match-substring  ";"  line :start qstart)
               (setf  all-processed-string-lines 
                      (append all-processed-string-lines  (list processed-line))
                      process-next-p nil))
              (t  (setf continuous-string-lines  processed-line
                        processed-line nil
                        process-next-p t)))

             ;;(afout 'out (format nil "FIRST STRING QUESTION line n= ~A~%qlength-line= ~A qstart= ~A~% rest-string= ~A~%process-next-p= ~A~%" n qlength-line qstart rest-string process-next-p))
             ;;end first (or only) line of question-string
             )
            ;;LINE IS MID-CONTINUED QUESTION STRING LINE
            ;;test to see the type of string line -- first-continuous, continued, or terminal
            ;;first continuous line?
            ((match-substring "+" line :start start-n)           
             (multiple-value-setq (processed-line extra-line)
                 (process-java-line-string  line
                                            :convert-to-objects-p t :force-line-process-p t))
             ;;note: 
             (setf continuous-string-lines 
                   (append continuous-string-lines  processed-line)
                   all-extra-lines (append all-extra-lines (list extra-line))
                   process-next-p t)
          ;;   (afout 'out (format nil "FOR CONTINUOUS LINE process-next-p= ~A~% processed-line= ~A~%continuous-string-lines= ~A~%start-n= ~A~%" process-next-p processed-line continuous-string-lines start-n))
             )
            ;;FINAL LINE IN CONTINUOUS STRING LINE
            ;;          (process-next-p t means continuous)
            ((and (match-substring "\"" line) (match-substring  ";" line :start start-n) 
                  process-next-p)
             (multiple-value-setq (processed-line extra-line)
                 (process-java-line-string  line
                                            :convert-to-objects-p t :force-line-process-p t))
             (setf continuous-string-lines 
                   (append continuous-string-lines processed-line)
                   all-processed-string-lines
                   (append all-processed-string-lines (list continuous-string-lines))
                   process-next-p nil
                   processed-line nil
                   continuous-string-lines nil)
           ;;  (afout 'out (format nil "FOR LAST CONTINUOUS LINE process-next-p= ~A~% processed-line= ~A~%" process-next-p processed-line))
             ;;end  final line in continuous clause
             )

            ;;LINE IS NON-STRING STATIC VARIABLE 
            ((multiple-value-setq (rest-string first-string result-string-list token-matched-p)
                 (match-token-phrase  '(public static) line))
             (setf class-static-var-list  (process-java-line-string rest-string)
                   all-class-static-var-lists 
                   (append all-class-static-var-lists (list class-static-var-list))) 
             ;;end NON-STRING STATIC VARIABLE
             )
            ;;If line is none of above, append to separate EXTRA-LINES list
            (t  (setf all-extra-lines (append all-extra-lines (list line)))))

           ;;end unless, loop
           ))
        ;;end with-open, probe-file, cond
        )))
    ;;end convert-shaq-question-file

    ;;PRINT RESULTS TO MATCHING .LISP FILES
    (cond
     (print-to-match-file-p
      (unless out-pathname
        (setf out-pathname (java-to-lisp-pathname pathname)))
      ;;this didn't hellp (pathname (java-to-lisp-pathname pathname)

                
      (with-open-file (out-stream out-pathname :direction :output :if-exists :supersede
                                  :if-does-not-exist :create) 
        (format t ">>>> PRINTING OUTPUT TO FILE: ~A~%~%" out-pathname)

        ;;print to file
        ;;Part 1
        (format out-stream 
                ";;**************** ~A ****************
;;
;;Tom G. Stevens PhD
;;  Success and Happiness Attributes Questionnaire (SHAQ) File
;;
;;  Converted from original SHAQ java file of same name (with .java extension)
;;  Originally written my Dr. Stevens in @ 2000
;;  Converted to lisp file in 2014
;;

 '(
" file-namestring)
        ;;Part 2
        (format out-stream "~%~A~%~% ~%" 
                (print-nested-list  
                 (list all-pc-instances all-processed-string-lines 
                       all-class-static-var-lists  all-extra-lines all-deleted-lines   
                       all-print-lines) :stream out-stream  :incl-quotes-p t ))
        ;;Part 3
        (format out-stream "~%   ) ~%~% ;;end of file~%")
        ;;end with-open-file, print-to-file clause
        ))
     (t nil))

    ;;PRINT-TO-SUM-FILES-P
    (if print-to-sum-files-p
        (let*
            ((results-lists 
              (LIST  all-processed-string-lines all-class-static-var-lists
                     all-extra-lines)) ;;omit all-deleted-lines   all-print-lines))
             (path-list '( "C:\\\TOM\\LISP PROJECTS TS\\SHAQ\\Q-questions.lisp"
                           "C:\\\TOM\\LISP PROJECTS TS\\SHAQ\\Q-static-vars.lisp"
                           "C:\\\TOM\\LISP PROJECTS TS\\SHAQ\\Q-extra-lines.lisp"))
             (pc-instance-path "C:\\\TOM\\LISP PROJECTS TS\\SHAQ\\Q-pc-instances.lisp")
             ;;   (pc-path-name (file-namestring pc-instance-path))
             ;; (label-list '(PC-INSTANCES SHAQ-QUESTIONS STATIC-VARIABLES EXTRA-LINES))
             )
          (loop
           for list in results-lists
           for sum-pathname in path-list
           ;; for list-label in label-list
           do
         ;;was (print-nested-list-to-file  list  sum-pathname
          (print-list-to-file list sum-pathname                            
                                       :incl-label file-namestring) 
           (print-nested-list  all-pc-instances  :stream t :incl-quotes-p t   :incl-label 'MY-TEST-LABEL)
           ;;end loop
           )
          (print-list-to-file  all-pc-instances pc-instance-path 
                                :incl-label file-namestring)
           ))      

    ;;end convert-shaq-question-file
    (if results-list-p
        (list all-pc-instances all-processed-string-lines all-class-static-var-lists
              all-extra-lines all-deleted-lines   all-print-lines all-string-array-lists)
      (values all-pc-instances all-processed-string-lines all-class-static-var-lists
              all-extra-lines all-deleted-lines   all-print-lines all-string-array-lists)) 
    ))
;;test
;;
;;SSS 
;; PRINT TO FILE
;; (convert-shaq-question-file "C:\\TOM\\LISP PROJECTS TS\\SHAQ2000\\bsRomantic.java":results-list-p t  :print-to-match-file-p t :out-pathname "C:\\TOM\\LISP PROJECTS TS\\SHAQ2000\\bsRomantic.lisp")
;;  (convert-shaq-question-file "C:\\TOM\\LISP PROJECTS TS\\SHAQ2000\\studentdatafile.java" :results-list-p t  :print-to-match-file-p t :out-pathname "C:\\TOM\\LISP PROJECTS TS\\SHAQ2000\\student-data-file-edited.lisp")
;; PRINT OUT A LIST AND organize it
;;  (progn (setf out nil) (setf *test-out1 (convert-shaq-question-file "C:\\TOM\\LISP PROJECTS TS\\SHAQ2000\\bsRomantic.java":results-list-p t)) (print-nested-list  *test-out1 :stream t  :incl-quotes-p t ))
;;  (write-format-to-file  `("Test-string ~A ~A~%" ,var1 ,var2)  "c:\\temp\\write-to-file-test.txt" :if-exists-keyword :append)
 ;; (file-namestring "c:\\tom\\lisp\\thisfile.lisp") = "thisfile.lisp"

;;TRY IT ON A NON-QUESTION FILE
;;  (convert-shaq-question-file "C:\\TOM\\LISP PROJECTS TS\\SHAQ\\FrAnswerPanel.lisp":results-list-p t  :print-to-match-file-p t :out-pathname "C:\\TOM\\LISP PROJECTS TS\\SHAQ\\Frame-answer-panels.lisp")

#|
 (process-java-line-string " smtBALanceQ =
    \"I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.\";")
;;works, returns (SMTBALANCEQ "I would say that I lead a very balanced lifestyle. I have time and energy for my school, my work, friends and family, the opposite sex, relaxation, physical activity, my spiritual life, and recreation.  In addition, almost all of these life areas are providing me with a great deal of satisfaction.")
|#

;;SSS START HERE PROCESSING EACH LINE
;;   CREATE SEPARATE FUNCTIONS IF COMPLEX??
;;
#|
IDEA--Try 1-creating name-sym and all data from java file
   2-SEARCH FOR name-sym in *shaq-sym-labels-list
   3a--IF MATCH, COPY LABEL INTO NEW CLASS
     b--IF NO MATCH, PUT "NO MATCH" ETC IN LABEL SLOT
  4--MANUALLY UPDATE NAME-SYM AND LABEL IN NO MATCH
  CLASSES.|#


#| PCategory intSrq6Extra = new PCategory("intSrq6Extra",1, intSrq6ExtraQ, "int",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);|#
       

;;PROCESS-STATIC-SHAQ-VARIABLE
;;
;;COPIED/MODIFIED FROM U-JAVA-TO-LISP.LISP
;;
;;ddd
(defun process-static-SHAQ-variable (string)
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
      (q-inst)
       (q-num)
       (q-text-inst)
       (q-data-type)
       (q-ans-panel)
       (q-inst-array)
       (q-frame-title)
       (q-fr-width)
       (q-fr-height)
       )

    (multiple-value-setq (rest-string first-string result-string-list token-matched-p)
          (match-token-phrase '(public static final) string))

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
       ;;FOR STRING AND INT VARIABLES, 
       ;;   create the new line =  (SETF SET-VAR VALUE)
       ((member var-type '("string" "int") :test 'string-equal)
        (setf  new-line (format nil "(setf  ~A ~A)~%" set-var value)
               new-lines-list (append new-lines-list (list new-line)))
        ;;(afout 'out (format nil "new-line= ~A~%" new-line))
        )       
       (t
        ;;PROCESS THE INSTANCE ETC VARIABLES 
        (multiple-value-setq (q-inst q-num q-text-inst q-data-type q-ans-panel q-inst-array q-frame-title q-fr-width q-fr-height)
            (process-PCategory string))
        ;;SSS START HERE
        ;;PROCESS THESE Q-VARS TO MAKE A NEW CLASS  HERE


        ))
      ;;delete  "" and NIL items in the new-lines-list
      ( delete-list-items '(nil "") new-lines-list)
      ;;(afout 'out (format nil "new-line= ~A~% new-lines-list= ~A~%" new-line new-lines-list))
      ;;end past static var test clause
      )
     (t nil))
    ;;end print-string-list
    new-lines-list
    ))








;;PROCESS-PCATEGORY  
;;  SSS SEE NOTE-- FINISH PROCESSING THE TOKEN LIST BELOW??
;;
;;ddd
(defun process-PCategory (line &optional return-values-p)  
  "In U-java-to-lisp-shaq.lisp, Example: PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight).  RETURNS (values-list token-list) EX: \"intSrq6Extra\"
\"1\"
\"intSrq6ExtraQ\"
\"\"int\"\"
\"FrAnswerPanel.LikeUs7\"
\"questionInstancesArray\"
\"frameTitle\"
\"frameDimWidth\"
\"frameDimHeight;\""
  (let*
      ((result) 
       (first-pt)
       (last-pt)
       (token-list)
       (first-pt2)
#|       (q-inst)
       (q-num)
       (q-text-inst)
       (q-data-type)
       (q-ans-panel)
       (q-inst-array)
       (q-frame-title)
       (q-fr-width)
       (q-fr-height)|#
       )
    (multiple-value-setq ( last-pt first-pt) 
        (match-substring  "PCategory" line :start 10))

    (cond
     (first-pt
      (multiple-value-setq (first-pt2 last-pt token-list) 
          (divide-string-to-tokens last-pt '(",") :no-initial-spaces T
                                   :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";")))
      ;;not needed??(multiple-values-setq (q-inst  q-num  q-text-inst q-data-type q-ans-panel
                               ;;      q-inst-array q-frame-title  q-fr-width q-fr-height)
                          ;;  (values-list  token-list))
                          )
;;(divide-string-to-tokens "(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" '(",") :no-initial-spaces T  :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";"))

    ;;could also return first-part last-part
     (t nil))   
 
  ;;end process-PCategory
    (cond
     (return-values-p
      (values-list  token-list first-pt2 last-pt))
     (t  token-list))
    ))
;;test--works
#| (process-PCategory   "PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);") 
values token-list first-pt last-pt) returns
("\"intSrq6Extra\"" "1" "intSrq6ExtraQ" "\"int\"" "FrAnswerPanel.LikeUs7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight;")
"\"intSrq6Extra\""
",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);"|#
#|(values-list token-list) RETURNS:
"\"intSrq6Extra\""
"1"
"intSrq6ExtraQ"
"\"int\""
"FrAnswerPanel.LikeUs7"
"questionInstancesArray"
"frameTitle"
"frameDimWidth"
"frameDimHeight;"|#




;;CONVERT-SYMS-TO-CLASSES
;;
;; SSS FINISH THIS FROM DATA AFTER READ JAVA FILES???
;;ddd
(defun convert-syms-to-classes (sim-label-list superclasses)
  "In U-SHAQ, converts sim-label-lists into SHAQ classes."
  (let
      ((class-name)
       (class-label)
       (long-symbol)
       (class-question)
       ;;the question and q-frames
       (q-frame-title)
       (q-frame)
       (q-frame-created-p)
       (q-frame-width)
       (q-frame-height)
       (frame-instances)
       (q-answer)
       (q-answer-type)
       ;;vars used below
       (slot-name)
       (slot)
       (added-slot-list)
       (slot-doc)
       (new-slot-list)
       (default-initargs)
       (class-documentation)
       (class-def)
       )
  (loop
   for element in sim-label-list
   do
    (setf class-name (first element)
          class-label (second element)
          help-info
          help-link
          added-slot-list 
          
    (setf new-slot-list `((name :initform ,class-name)
                      (label :initform ,class-label)))
    )

  ;;("\"intSrq6Extra\"" "1" "intSrq6ExtraQ" "\"int\"" "FrAnswerPanel.LikeUs7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight;")        

    ;;end loop
    )

;;end let, defun
))



;;ddd
;;

;;;;
;;STEP 1:  (directory "C:\\TOM\\LISP PROJECTS TS\\SHAQ2000\\*.java")
;;STEP 2: I search and replaced #P at front of each
;;STEP 3: (print-list *all-shaq-2000-pathnames :stream t :incl-quotes-p t :incl-parens-p t)
(defparameter *all-shaq2000-question-files
'(;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/StudentBasicData2.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/StudentBasicData.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/Student.java"
#| "C:/TOM/LISP PROJECTS TS/SHAQ2000/Resources.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Reflection.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Referals1.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RecProbation.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RecommendationRules.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RecommendationFile.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealWorld.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealResources.java"|#
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealPeopleSex.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealPeople.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealHealth.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealHappy.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealEmotions.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealEducation.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealDepression.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealAnxiety.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealAnger.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealAchievement.java"
#| "C:/TOM/LISP PROJECTS TS/SHAQ2000/RandomIntGenerator.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/ProblemsSymptoms.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PersonDataFrame.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PersonBio.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PersonBasicData.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Person.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PCatQuestFileTemplate.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PCategoryList.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PCategory.java"
|#
;;?? "C:/TOM/LISP PROJECTS TS/SHAQ2000/OutcomeStudents.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/Outcomes.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/OutcomeRelationships.java"
#| "C:/TOM/LISP PROJECTS TS/SHAQ2000/OutcomeCareer.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/ObjectConverter.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/ObjectAnalyzer.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/MyMethods.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/MixedArray.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/MakeFrQuJRadioButtons.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/MakeFrameQuestions.java"
|#
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iWorldviewFears.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iWorldview.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iTopBV2.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iTopBV.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iSelf2.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iSelf.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/iRomantic.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/iRelationship.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/iReasoning.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/iProblemSolving.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/iPeopleview.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/iPeers.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/iParents.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/iOpatsm.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inWrite.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inWoEthnicSt.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inSocSci.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inNatSci.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inMiltCrim.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inMed.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inLang.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inHelp.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inFineArt.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inEngr.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inCareer.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inBus.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iLifeThemesSoc.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iLifeThemesNeg.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iLifeThemesAch.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iLifeThemes.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iIEcontrol2.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iIEcontrol.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/iFamilyview.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/iExecSelf.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/iEmotion.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/iCognitiveSystem.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/iCognition.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iCareer.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/iAuthorities.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iAchievement.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iAcademicMotivation.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/iAbundance.java"
#| "C:/TOM/LISP PROJECTS TS/SHAQ2000/HelpResource.java"
#| "C:/TOM/LISP PROJECTS TS/SHAQ2000/HelpLink.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/HappinessQuotient.java"|#
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrAnswerPanel.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameUserType.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameUserGoals.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameUserFeedback.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameU100Credit.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameTestTextOutput.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameTestServerData.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameSuper.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameSpecificQuestionnaire.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameServerData.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsTextHelp.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsText.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsServerProblem.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsScales.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsHelp.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResearchSubject.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameQuestionJRadioButtons.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FramePermission.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameNonU100Credit.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameHelpLinks.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameExitWarning.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameDisplayLinks.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameDeBugText.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameCourseCredit.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameCaresTest.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameCareerGo.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameBasicQuestionBackground.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Faculty.java"|#
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/eHappy.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/eDepression.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/eAnxiety.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/eAnger.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/CaresDataSender.java"
;; "C:/TOM/LISP PROJECTS TS/SHAQ2000/CalcScaleScores.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsWorkhabits.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsSelfManagement.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsRomantic.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsPersuasion.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsMeetingPeople.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsManagement.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsLiberatedRole.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsLearningDisable.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsLearningAreas.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsLearning.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsIntimacy.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsInterpersonal.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsIndependentRel.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsEquality.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsEmotionManagement.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsAssertiveCR.java"
;;? "C:/TOM/LISP PROJECTS TS/SHAQ2000/AssessPriorities.java"
 ))


(defparameter *all-shaq2000-pathnames
'( "C:/TOM/LISP PROJECTS TS/SHAQ2000/StudentBasicData2.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/StudentBasicData.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Student.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Resources.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Reflection.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Referals1.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RecProbation.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RecommendationRules.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RecommendationFile.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealWorld.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealResources.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealPeopleSex.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealPeople.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealHealth.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealHappy.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealEmotions.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealEducation.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealDepression.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealAnxiety.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealAnger.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RealAchievement.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/RandomIntGenerator.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/ProblemsSymptoms.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PersonDataFrame.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PersonBio.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PersonBasicData.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Person.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PCatQuestFileTemplate.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PCategoryList.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/PCategory.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/OutcomeStudents.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Outcomes.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/OutcomeRelationships.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/OutcomeCareer.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/ObjectConverter.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/ObjectAnalyzer.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/MyMethods.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/MixedArray.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/MakeFrQuJRadioButtons.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/MakeFrameQuestions.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iWorldviewFears.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iWorldview.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iTopBV2.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iTopBV.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iSelf2.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iSelf.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iRomantic.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iRelationship.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iReasoning.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iProblemSolving.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iPeopleview.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iPeers.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iParents.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iOpatsm.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inWrite.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inWoEthnicSt.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inSocSci.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inNatSci.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inMiltCrim.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inMed.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inLang.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inHelp.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inFineArt.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inEngr.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inCareer.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/inBus.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iLifeThemesSoc.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iLifeThemesNeg.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iLifeThemesAch.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iLifeThemes.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iIEcontrol2.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iIEcontrol.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iFamilyview.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iExecSelf.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iEmotion.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iCognitiveSystem.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iCognition.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iCareer.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iAuthorities.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iAchievement.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iAcademicMotivation.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/iAbundance.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/HelpResource.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/HelpLink.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/HappinessQuotient.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrAnswerPanel.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameUserType.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameUserGoals.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameUserFeedback.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameU100Credit.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameTestTextOutput.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameTestServerData.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameSuper.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameSpecificQuestionnaire.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameServerData.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsTextHelp.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsText.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsServerProblem.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsScales.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResultsHelp.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameResearchSubject.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameQuestionJRadioButtons.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FramePermission.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameNonU100Credit.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameHelpLinks.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameExitWarning.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameDisplayLinks.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameDeBugText.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameCourseCredit.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameCaresTest.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameCareerGo.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/FrameBasicQuestionBackground.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/Faculty.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/eHappy.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/eDepression.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/eAnxiety.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/eAnger.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/CaresDataSender.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/CalcScaleScores.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsWorkhabits.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsSelfManagement.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsRomantic.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsPersuasion.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsMeetingPeople.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsManagement.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsLiberatedRole.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsLearningDisable.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsLearningAreas.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsLearning.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsIntimacy.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsInterpersonal.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsIndependentRel.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsEquality.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsEmotionManagement.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/bsAssertiveCR.java"
 "C:/TOM/LISP PROJECTS TS/SHAQ2000/AssessPriorities.java"
 ))



;;ttt ----------------------------------- INFO AND MISC TESTS ----------------------


;;
#|    (SETF BeAlone   '("BeAlone" 1  BeAloneQ  "int" inrSrq9OwnFriends  FrAnswerPanel-LikeUs7Reverse questionInstancesArray  frameTitle  frameDimWidth  frameDimHeight) |#

#|
;;IN JAVA FILES--READ AND CONVERT THE FOLLOWING
;;CLASS-SCALE NAMES
;;public class bsIntimacy  extends bsInterpersonal
;;STATIC VARIABLES
  public static String frameTitle = "Intimacy Skills and Habits";
  public static int frameDimWidth = 805;
  public static int frameDimHeight = 460;
  public static int totalQuestions = 15;

;;QUESTION INSTANCES
#|    PCategory intSrq6Extra = new PCategory("intSrq6Extra",1, intSrq6ExtraQ, "int",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);
    questionInstancesArray[0] = intSrq6Extra;|#

;;QUESTIONS
    //SRQ--INTIMACY
   public static final String intSrq6ExtraQ =
   "INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n"+
   "  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n"+
   "Question 1.If I am under more stress than usual, my partner will usually do extra things for me.";
   public static final String intSrq7CommitQ = "A long term commitment (would) cause(s) me to feel trapped.";
     //same as crSrq30RealQ
   public static final String intSrq30RealQ = "We usually discuss what is really bothering us (the underlying issues) instead of the surface issues.";
   public static final String intSrq8TellAllQ = "I have told my partner almost everything about myself.";
|#

#|
" intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";"
|#

;;ddd
#|(defun process-java-question-string (question-string)
  
)|#
#|
(defun convert-shaq-question-file-list (input-file-list output-file-list &key max-file-lines)
  "In U-SHAQ, converts shaq java question files to lisp nested list format.  Separates questioninstance property lists, question string lists, and other static variables. RETURNS (values pc-all-instances all-string-object-lists all-class-static-var-lists). max-file-lines is 500 by default and is a protection against problems."
  (let
      ((result)
       )
    (dolist 

 )))

|#