;; ************************ U-data-functions.lisp **************************
;;
;; WILL NEED TO MODIFY THESE IF ADD NEW QUEST NAMES OR MAKE
;;    OTHER CHANGES IN *SHAQ-question-variable-lists


;;GET-CATEGORY-QVARS
;;
;;ddd
(defun get-category-qvars (category &key (all-category-lists *shaq-question-variable-lists))
  "In U-data-functions.lisp, RETURNS (values category-qvarlist = a list of all the qvarlists for qvar category AND category)."
  (let 
      ((category-list)
       (category-qvarlist)
       (return-qvar)
       )
    (multiple-value-setq (category-list return-qvar)
        (get-key-value-in-nested-lists (list (list category 0))
                                       all-category-lists :return-list-p t
                                       :no-return-extra-p t))
    (setf category-qvarlist (cdr category-list))   
    (values category-qvarlist category)
    ;;end let, get-category-qvars
    ))
;;TEST
;;  (get-category-qvars 'bio4job) = works
;;   (get-category-qvars 'stu-data) = works
#|(("stpared" "b-Highest parents educ level" "spss-match" ("stuParentsEduc" 1 STUPARENTSEDUCQ "int" STUPARENTSEDUC "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "StudentBasicData.java")) ("stuclass" "st-Class level" "spss-match" ("stuClassLevel" "3" STUCLASSLEVELQ "int" STUCLASSLEVEL "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "StudentBasicData.java")) ("studegre" "st-Educ objective level"... ETC. SOME DELETED|#



(defun get-all-shaq-qvar-categories (&key (all-category-lists *shaq-question-variable-lists))
      "In U-data-functions.lisp, RETURNS all-categories-list."
      (let
          ((all-categories-list)
           )
        (loop
         for cat-list in all-category-lists
         with cat-sym
         do
         (setf cat-sym (car cat-list)
               all-categories-list (append all-categories-list (list cat-sym)))
         ;;end loop
         )
        all-categories-list
        ;;end get-all-shaq-qvar-categories
        ))
;;TEST
;;  (get-all-shaq-qvar-categories) = 
;;works, returns (ID UTYPE UGOALS BIO3EDUC BIO4JOB BI07LANG BIO1ETHN BIORELAF STUCOLLE STUMAJOR STUSPECI STURESID STU-DATA STGPATRE STUAPT STUFEEL STURESOURCE STUACMOTIV STU-LRN SM COPE THEMES TBV IE WORLDVIEW SELF-CONF ASSERTCR INTIMACY INDEP-INT ROM LIBROLE HAP RHEALTH RPEOPLE RDEP RANX RANG REMOT TB2 CARGEN CARBUS CARENGR CARFINE CARHELP CARLANG CARMED CARLAW CARNATSCI CARBESCI CARETHNIC CARWRITE)


;;GET-QVARLIST
;;
;;ddd
(defun get-qvarlist (qvar &key  (category T) (all-qvarlists *shaq-question-variable-lists))
  "In U-data-functions.lisp. RETURNS  (values qvarlist return-qvar). Used to create *shaq-all-question-categories-list, a global shaq variable."
  (let
      ((qvarlist)
       (return-qvar)
       )
    (multiple-value-setq (qvarlist return-qvar)
        (get-key-value-in-nested-lists (list (list category 0)(list qvar 0)) all-qvarlists :return-list-p t  :no-return-extra-p t))
    ;;end get-qvarlist
    (values qvarlist return-qvar)
    ))
;;test
;;  (get-qvarlist "lrntxout" *SHAQ-question-variable-lists)
;;   (get-qvarlist "lrntxout" *SHAQ-question-variable-lists 'STU-LRN)
;; both work, return ("lrntxout" "la-Outline textbooks" "spss-match" "lrnTEXToutline" ("lrnTEXToutline" "2" "lrnTEXToutlineQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "bsLearningAreas.java"))
;;FOR MULTI-SELECTION ITEMS
;;  (get-qvarlist "BIO4JOB") = ("bio4job" "b-Primary occupation" "spss-match" ("bio4job") ("INSTRUCTIONS: Select ALL of the following best describe your primary OCCUPATION." "If you have multiple occupations, choose all of them.") NIL :MULTI-ITEM)
;; (get-key-value-in-nested-lists '(( "BIO4JOB" 0))  *SHAQ-question-variable-lists :return-list-p T) works, returns entire list of lists under BIO4JOB CATEGORY

;;GET-ALL-SHAQ-QVARNAMES
;;
;;ddd
(defun get-all-shaq-qvarnames ( &key return-var&label-p (add-quote "") (all-qvarlists *shaq-question-variable-lists))
  "In U-data-functions.lisp. RETURNS (values all-qvars-list all-categories-list  problems-list). If return-var&label-p, then returns \"qvar  label\" for each. If add-quote, puts whatever add-quote is set to on both ends of the label (for spss add single quote)."
  (let
      ((all-qvars-list)
       (all-categories-list)
       (problems-list)
       )
    (loop
     for catlist in all-qvarlists
     with catname = nil
     do
     (cond
      ((listp catlist)
       (setf catname (car catlist))
       (loop 
        for qvarlist in (cdr catlist)
        with qvar = nil
        do
        (let
            ((label)
             )
        (cond
         ((null return-var&label-p)
        (cond
         ((listp qvarlist)
          (setf qvar (car qvarlist)                
                all-qvars-list (append all-qvars-list (list qvar))))
          (t (setf problems-list (append problems-list (list qvarlist)))))
        )
         ;;if return-var&label-p
         (t
          (cond
         ((listp qvarlist)
          (setf qvar (car qvarlist)     
                label (second qvarlist)
                all-qvars-list (append all-qvars-list (list 
                                                       (format nil "~A  ~A~A~A" qvar add-quote label add-quote)))))
          (t (setf problems-list (append problems-list (list qvarlist)))))))       
        ;;end inner loop, clause
        )))
      (t (setf problems-list (append problems-list (list catlist)))))
         ;;end outer loop
         )
    (values all-qvars-list all-categories-list  problems-list)
  ;;end let, get-all-shaq-qvars
  ))
;;TEST
;; (setf *all-shaq-qvarnames-list (get-all-shaq-qvarnames))
;; ;; (setf *num-qvars (list-length *all-shaq-qvarnames-list)) = 713
;;  (setf *all-shaq-qvar&labels (get-all-shaq-qvarnames :return-var&label-p T))
;;  (setf *all-shaq-qvar&labels (get-all-shaq-qvarnames :return-var&label-p T :add-quote "\'"))


;;MY-GET-DATE-TIME
;;
;;ddd
(defun my-get-date-time (&key (date-separator ".") reverse-year-p
                                              return-unseparated-date-str-p use-zeros-p )
  "In U-data-functions.lisp, RETURNS (values date-string  date time year month day hour min sec. USE-ZEROS-P returns 02 for day and 08 for hour. See keys too. )"
  (let
      ((date-string)
       (time)
       (unseparated-date-str)
       (date)
       (unsep-time)
       )
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time (get-universal-time))
      ;;use zeros-p
      (when use-zeros-p
        (when (< day 10)
          (setf day (format nil "0~A" day)))
        (when (<  hour 10)
          (setf hour (format nil "0~A" hour)))
        (when (<  month 10)
          (setf month (format nil "0~A" month))))

      (setf time (format nil "~a:~a" hour min)
            unsep-time (format nil "~a~a" hour min))
     ;; (break "time")
      (cond
       (reverse-year-p
        (setf date 
              (format nil "~a~a~a~a~a" year date-separator month date-separator day)))
       (t (setf date 
                (format nil "~a~a~a~a~a" month date-separator day date-separator year))))
      (setf date-string (format nil "Date: ~A  Time: ~A" date time))

    (when return-unseparated-date-str-p
      (setf unseparated-date-str 
            (format nil "~a~a~a~a" year month  day unsep-time)))
    (values date-string  date time year month day hour min sec unseparated-date-str)
    ;;end mvb, let, my-get-date-time
    )))
;;TEST
;; (my-get-date-time)
;; works= "Date: 10.29.2014  Time: 11:15"  "10.29.2014"  "11:15"  2014  10  29  11  15  7
;;;; (my-get-date-time  :use-zeros-p T :return-unseparated-date-str-p T)
;;works= "Date: 02.26.2019  Time: 08:45"   "02.26.2019"  "08:45"  2019  "02" 26 "08"  45  58  "201902260845"
;; ;; (my-get-date-time :reverse-year-p T  :use-zeros-p T :return-unseparated-date-str-p T)
;;works= "Date: 2019.02.26  Time: 08:47"  "2019.02.26"  "08:47"  2019  "02" 26 "08"  47 23 "201902260847"
;; (format nil "~S" "
 



;;GET-QUEST-VAR-VALUES
;;
;;ddd
(defun get-quest-var-values (qvar &key qvarlist  return-values-p  (category T) label spss-match java-var qnum q-name data-type answer-panel array frame-title fr-width fr-height java-file help-info help-links)
  "In U-data-functions.lisp  qvar is the NEW-spss variable sym or string. RETURNS (values return-keylist qvar) =  a list of key-value pairs with keys=  label spss-match java-var qnum q-name data-type answer-panel array frame-title fr-width fr-height java-file qvar).  For a key = T, returns the first T value (values return-var-value return-var  return-keylist qvar help-info help-links). Category is the category of the qvar.  If qvarlist supplied, then uses it INSTEAD of search long list for qvar--saves time."
  (let
      ((return-var-value)  
       (sublist)
       (arg-keylist-vals (list  label spss-match java-var qnum q-name data-type answer-panel array frame-title fr-width fr-height java-file))
       (return-var)
       (key)
       (val)
       (return-qvar)
       (return-keylist)
       (qvarlist)
       (help-info-links-list)
       (help-info)
       (help-links)
       ( var1)
        (n2)
       )
    ;;(afout 'out (format nil "arg-keylist-vals= ~A~%" arg-keylist-vals))

    ;;see if use return-var (set to T?) before change their values below
        (loop
         for n from 0 to 11  
         do
         (setf key (nth n '(label spss-match java-var qnum q-name  data-type answer-panel array frame-title fr-width fr-height java-file))
               val (nth n arg-keylist-vals))
          (cond
           (val
            (setf  return-var key)
            (return))
           (t nil))
          ;;end loop
          )
        ;;get the list of values
        (cond
         ;;if the qvarlist is supplied, saves a search
         (qvarlist nil)
         (t (multiple-value-setq (qvarlist return-qvar)
                (get-qvarlist qvar :category category))))
    ;;set the values
    (setf  label (second qvarlist)
           spss-match (third qvarlist)
           java-var (fourth qvarlist)
           help-info-links-list (sixth qvarlist))
    (if (listp (setf sublist (fifth qvarlist)))
        (setf qnum (second sublist) 
              q-name (third sublist) 
              data-type (fourth sublist) 
              answer-panel (fifth sublist)
              array (sixth sublist)
              frame-title  (seventh sublist)
              fr-width (eighth sublist)
              fr-height  (ninth sublist)
              java-file  (tenth sublist )))

    ;;for help-info-link-list
    (setf  help-info (second help-info-links-list)
           help-links (third help-info-links-list))

    ;;make the return-keylist
       (setf return-keylist (list  :label label :spss-match spss-match :java-var java-var :qnum qnum :q-name q-name :data-type data-type :answer-panel answer-panel :array array :frame-title frame-title :fr-width  fr-width :fr-height fr-height :java-file java-file :help-info help-info :help-links help-links))
      ;;(afout 'out (format nil "q-name= ~A  return-var= ~A~% " q-name return-var))  
#|  DOESN'T WORK BUT SHOULD -- (eval return-var) = unbound var
   ;;get the return-var-value  set above
    (if return-var
        (setf return-var-value (eval return-var)))|#
      ;;SSS TEMP SOLUTION
      (loop
         for n from 0 to 12  
         do
         (setf n2 (+ (* n 2) 1)
          var1 (nth n '(label spss-match java-var qnum q-name  data-type answer-panel array frame-title fr-width fr-height java-file help-info-link-list)))
         (cond
          ((equal key var1)
           (setf return-var-value (nth n2 return-keylist)
                 return-var key)
           (return))
          (t nil))                 
          ;;end loop
          )

    ;;what to return?
      (cond
       (return-values-p 
        (values   label  spss-match  java-var  qnum  q-name  data-type  answer-panel  array  frame-title   fr-width  fr-height  java-file help-info  help-links))
       ( return-var 
        (values return-var-value return-var  return-keylist qvar))
       (t (values return-keylist qvar)))
        ))
;;TEST 
;; (get-quest-var-values 'acmfinan) 
;;modified version incl help
#|"iAcademicMotivation.java"
JAVA-FILE
(:LABEL "am-Confidence school finances" :SPSS-MATCH "spss-match" :JAVA-VAR "acmFinancial" :QNUM "19" :Q-NAME "acmFinancialQ" :DATA-TYPE "int" :ANSWER-PANEL "Confidence7" :ARRAY "questionInstancesArray" :FRAME-TITLE "frameTitle" :FR-WIDTH "frameDimWidth" :FR-HEIGHT "frameDimHeight" :JAVA-FILE "iAcademicMotivation.java" :HELP-INFO NA :HELP-LINKS NA)
ACMFINAN|#

   ;; label spss-match java-var qnum q-name data-type answer-panel array frame-title fr-width fr-height java-file)
;;works, returns= (:LABEL "am-Confidence school finances" :SPSS-MATCH "spss-match" :JAVA-VAR "acmFinancial" :QNUM "19" :Q-NAME "acmFinancialQ" :DATA-TYPE "int" :ANSWER-PANEL "FrAnswerPanel.Confidence7" :ARRAY "questionInstancesArray" :FRAME-TITLE "frameTitle" :FR-WIDTH "frameDimWidth" :FR-HEIGHT "frameDimHeight" :JAVA-FILE "iAcademicMotivation.java")       ACMFINAN
;;   (get-quest-var-values 'acmfinan :q-name t) 
;; works, returns= "acmFinancialQ"  Q-NAME  (:LABEL "am-Confidence school finances" :SPSS-MATCH "spss-match" :JAVA-VAR "acmFinancial" :QNUM "19" :Q-NAME "acmFinancialQ" :DATA-TYPE "int" :ANSWER-PANEL "FrAnswerPanel.Confidence7" :ARRAY "questionInstancesArray" :FRAME-TITLE "frameTitle" :FR-WIDTH "frameDimWidth" :FR-HEIGHT "frameDimHeight" :JAVA-FILE "iAcademicMotivation.java")   ACMFINAN
;;(setf  xx 1 yy 2 zz 3)
;; (sort (list zz xx yy) '<) = (1 2 3)
 ;;






;;GET-MULTI-SELECTION-QUEST-VAR-VALUES
;;
;;ddd
(defun get-multi-selection-quest-var-values (qvar-category &key qvarlist) ;;
   ;;should work, but gets error &key (qvar-master-list *SHAQ-question-variable-lists))
  "In U-data-functions.lisp  RETURNS PLIST ( :qvar-category :primary-qvar-sym  :primary-qvar-label :primary-title-text :primary-instr-text :quest-text-list :q-spss-name :ans-name-list (SPSS VAR NAME each item) :ans-text-list :num-answers  :primary-title-text :primary-instr-text :quest-text-list  :qnum  :data-type :ans-data-list  :primary-java-var :primary-spss-match :spss-match-list :java-var-list :ans-xdata-lists).  If qvarlist is supplied, saves the search for the qvar category list."
  ;;qvar-category  is the overall sym not used  (eg BIO4JOB)
  (let*
      ((multi-items-list 
        (if qvarlist  qvarlist ;;if provided in key
          ;;otherwise, find the qvarlist
          (eval `(get-key-value-in-nested-lists (quote (( ,qvar-category 0))) *SHAQ-question-variable-lists :return-list-p t))))
       ;;test (get-key-value-in-nested-lists (quote (( ugoals 0))) *SHAQ-question-variable-lists :return-list-p t)
       ;; (find-qvar-selection-type 'ugoals)
       (length-multi-list (list-length multi-items-list))
       (primary-qvarlist) ;;later (car multi-items-list))
       (primary-qvar-sym)  ;; later(car primary-qvarlist))
       (primary-qvar-label)
       (primary-spss-match)
       (primary-java-var)
       (primary-qvar-sublist)
       ;;indiv item spss names
       (q-spss-name) ;;for whole category (eg. occupation)
       (ans-name-list)  ;;spss var name for each item (eg. sales, teacher, etc)
       (ans-text-list) ;;actual text of answer items
       (num-answers (- length-multi-list 2)) ;;don't count category name + primary list
       ;;question text title, instrs, overall question

       ;;THESE MAY BE MODIFIED BELOW
       (primary-title-text *default-multi-choice-title)
       (primary-instr-text *default-multi-choice-instr)
       (primary-help-info "")
       (primary-help-links)
       (quest-text-list) ;;PUT IN AS INSTR TEXT
       (qnum 0)
       (q-name)
       (answer-panel)
       ;;data info -- generally each item scored 0 or 1(checked)
       (data-type)       ;;usually integer
       (ans-data-list)   ;;list of  1's (1 1 1 ... 1)        
       ;;following rarely used with multi-selection
       (array)
       (frame-title)
       (java-var-list)
       (spss-match-list)
       (fr-width)
       (fr-height)
       (java-file)
       (ans-xdata-lists)
       (qvar)
        (label)
        (spss-match)
        (java-var)
        (sublist)
        (special-title)
        (special-instr)
        (ans-xdata-list)
        ;;added
        (ans-text-sublist)
        (return-multi-selection-keylist)
        (q-instr-sym)
        (answer)


      ;;for helplinks
      (primary-help-list)
      (special-quest-list)
;;yyyy
        )
  
   ;;FOR EACH LIST IN MULTI-SELECT LIST GROUP
   (loop
    for qvarlist in multi-items-list
    for i from 0 to (-  length-multi-list 1)
#|    with qvar
    with label
    with spss-match
    with java-var
    with sublist
    with special-title
    with special-instr
    with ans-xdata-list|#
    do
    ;;set the values
 
    ;;from the primary-qvar get more data for the title, instrs, question
    ;; most ARE NOT SCORED EXCEPT 1 (checked) or 0 (not)  
    (cond
     ((= i 1) 
      (setf  primary-qvar-sym (first qvarlist)
             ;; qvar-syms-list (append qvar-syms-list (list qvar))
             primary-qvar-label (second qvarlist)
             primary-spss-match (third qvarlist)
             primary-java-var (fourth qvarlist) ;;actually usually a list
             primary-qvar-sublist (fifth qvarlist)
             primary-help-list (sixth qvarlist))
      ;;for helplinks
      (if (equal (car primary-help-list) :help)
          (setf primary-help-info (second primary-help-list)
                primary-help-links (third primary-help-list)))
      ;;check for special title and instructions for each category/multi question
      (cond
       ((listp primary-qvar-sublist)
        (setf special-title (getf primary-qvar-sublist :title)
              special-quest-list (getf primary-qvar-sublist :quest))
        (if special-title (setf primary-title-text  (car special-title))
          ;;may cause weird error if nil instead of  a string? After checking, probably not.
          (setf primary-title-text " "))
      ;;NO USED AS QUEST  (if special-instr (setf primary-instr-text  special-instr))
        (setf  QUEST-TEXT-LIST special-quest-list) ;; primary-qvar-sublist)
        ;;(afout 'out (format nil "2 QUEST-TEXT-LIST ~A~%"QUEST-TEXT-LIST))
        )
       (t nil))      
  ;;OLD FORM THAT MAY STILL BE USED??
     ;;(afout 'out (format nil "primary-qvar-sublist= ~A~%"primary-qvar-sublist))
      (cond
#|       ((and (listp primary-qvar-sublist)
             (<  (list-length primary-qvar-sublist) 5))
        (setf  QUEST-TEXT-LIST primary-instr-text) ;; primary-qvar-sublist)
        ;;(afout 'out (format nil "2 QUEST-TEXT-LIST ~A~%"QUEST-TEXT-LIST))
        )   |#     
       ((and (listp primary-qvar-sublist)
             (> (list-length primary-qvar-sublist) 4))
        (if (numberp (second sublist))
            (setf qnum (second sublist)))
        (setf q-name (third sublist) 
              data-type (fourth sublist) 
              answer-panel (fifth sublist)
              array (sixth sublist)
              frame-title  (seventh sublist)
              fr-width (eighth sublist)
              fr-height  (ninth sublist)
              java-file  (tenth sublist ))
        )
       (t   nil     ))
     ;;end (= i 1) clause
      )
     ;;FOR REST OF QVAR ITEMS IN THE MULT-LIST
     ((> i 1) 
      ;;get answer text and other info here
      (cond
       ((listp qvarlist)
        (setf  qvar (first qvarlist)
               ans-name-list (append ans-name-list (list qvar))
               label (second qvarlist)
               spss-match (third qvarlist)
               ;;later comment off these below??
               spss-match-list (append spss-match-list (list spss-match))
               java-var (fourth qvarlist)
               java-var-list (append java-var-list (list java-var))
               ans-text-sublist (fifth qvarlist)
              ans-xdata-list (car (last qvarlist)))
        (unless  (and (listp ans-xdata-list)(equal (car ans-xdata-list) :xdata))
          (setf ans-xdata-list nil))
        (setf ans-xdata-lists (append ans-xdata-lists (list ans-xdata-list)))
             
        (if (stringp (car ans-text-sublist))
            (setf ans-text-list (append ans-text-list  ans-text-sublist))
          ;;else
          (setf  ans-text-list (append ans-text-list (list label))))             
             
        ;;(afout 'out (format nil "label= ~A ans-text-list= ~A~%"   label ans-text-list))
        )
       (t nil))
      ;;end (> i 1),cond
      )
     (t nil))
     ;;end loop
     )
   ;;MAKE QNUM AND ANS-DATA-LIST
     ;;SSS  DEFPARAMETER THIS VARIABLE
     (unless (and (numberp qnum) (> qnum 0))
       (setf qnum (incf *current-multi-selection-qnum)))
     (unless ans-data-list
       (setf data-type 'integer
             ans-data-list (make-sequence 'list (-  length-multi-list 2) :initial-element 1))) ;;don't count cat-sym and primary-var

         ;;MAKE THE RETURN-KEYLIST
         (setf return-multi-selection-keylist
               (list :qvar-category qvar-category
                 :primary-qvar-sym  primary-qvar-sym  ;; later(car primary-qvarlist))
                 :primary-qvar-label  primary-qvar-label
                 ;;question text title, instrs, overall question
                 :primary-title-text primary-title-text
                 :primary-instr-text primary-instr-text
                 :qnum  qnum
                 :quest-text-list quest-text-list
                ;;;indiv item spss names
                 :q-spss-name  q-spss-name ;;for whole category (eg. occupation)
                 :ans-name-list  ans-name-list  ;;SPSS VAR NAME each item 
                 :ans-text-list ans-text-list  ;;actual text of answer items
                 :num-answers  num-answers
                 ;;question text title, instrs, overall question
                 ;;data info -- generally each item scored 0 or 1(checked)
                 :data-type   data-type      ;;usually integer
                 :ans-data-list   ans-data-list  ;;list of  1's (1 1 1 ... 1)       
                 :primary-java-var  primary-java-var 
                 :primary-spss-match  primary-spss-match
                 :spss-match-list spss-match-list
                 :java-var-list java-var-list
                 :ans-xdata-lists ans-xdata-lists
                 :help-info primary-help-info
                 :help-links primary-help-links
                 ))

     ;;end let, defun get-multi-selection-quest-var-values
     ))
(defparameter @SSS-TEST nil)
;;TEST
;;  (get-multi-selection-quest-var-values 'bio4job)
;; works, result= (:QVAR-CATEGORY BIO4JOB :PRIMARY-QVAR-SYM "bio4job" :PRIMARY-QVAR-LABEL "b-Primary occupation" :PRIMARY-TITLE-TEXT "MULTIPLE-SELECTION QUESTION" :PRIMARY-INSTR-TEXT "Select ALL that apply to you" :QNUM 10 :QUEST-TEXT-LIST ("INSTRUCTIONS: Select ALL of the following best describe your primary OCCUPATION." "If you have multiple occupations, choose all of them.") :Q-SPSS-NAME NIL :ANS-NAME-LIST ("student" "manager" "propeop" "protech" "consulta" "educator" "sales" "technici" "clerical" "service" "ownbus10" "othrsfem") :ANS-TEXT-LIST ("1-Student" "2-Manager" "3-People professional" "4-Technical professional" "5-Consultant" "6-Educator" "7-Sales" "8-Other technical" "9-Clerical" "10-Service" "11-Own business" "12-othrsfem") :NUM-ANSWERS 13 :DATA-TYPE INTEGER :ANS-DATA-LIST (1 1 1 1 1 1 1 1 1 1 1 1) :PRIMARY-JAVA-VAR ("bio4job") :PRIMARY-SPSS-MATCH "spss-match" :SPSS-MATCH-LIST ("spss-match" "spss-match" "spss-match" "spss-match" NIL "spss-match" "spss-match" "spss-match" "spss-match" "spss-match" "spss-match" "spss-match") :JAVA-VAR-LIST (("Student") ("Manager/executive") NIL ("Technician") ("Consultant") ("Educator") ("Sales") ("Technician") ("Clerical") ("Service") ("Own business +10 employees") (("Other self-employed"))))
;; 
;; (get-key-value-in-nested-lists '(( "BIO4JOB" 0))  *SHAQ-question-variable-lists :return-list-p T) works, returns entire list of lists under BIO4JOB CATEGORY
;; (defun testx (a &key (b 7))   b) ;; (testx 9) = 7
;;
;;  (get-multi-selection-quest-var-values 'scalessel)



   
;;GET-QUESTION-TEXT
;;
;;ddd
(defun get-question-text (quest-name &optional (category T))
  "In U-data-functions.lisp,quest-name = qvar  given quest-symbol RETURNS (values q-text-sym q-text-list q-title q-instr) LIST of question text from nested list--arg string or symbol, will also search for question sym w/o Q if gets nil. "
  (let*
      ((q-text-name (format nil "~AQ" quest-name))
       (q-text-sym (my-make-symbol q-text-name))
       (q-var-list 
        (get-key-value-in-nested-lists  (list (list category 0) (list q-text-sym 0))
                                       *all-shaq-questions :return-list-p T))
       (q-instr-list)
       (q-title)
       (q-instr)
       (q-instr-list)
       (q-instr-sym)
       (q-text-list)     
       )
    #|    (if
        (null q-var-list)
        (setf new-q-name (my-make-symbol (format nil "~AQ" quest-sym))
              q-var-list  (get-key-value-in-nested-lists 
                           (list (list category 0) (list new-q-name 0))
                           *all-shaq-questions :return-list-p T)))|#
    (cond
     (q-var-list    
      (setf q-text-list (second q-var-list)
            q-instr-sym (third q-var-list))

      (setf q-instr-list (get-key-value-in-nested-lists 
                          (list (list category 0) (list q-instr-sym 0))
                          *all-shaq-questions :return-list-p T))

      (if (listp q-instr-list)
          (setf q-title (first (second  q-instr-list))
                q-instr (second  (second  q-instr-list))))
      )
     (t (setf q-text-list '("NO QUESTION TEXT FOUND"))))

    (values q-text-sym q-text-list q-title q-instr)  
    ;;end get-question-text
    ))
;;test
;; (get-question-text    "thm1ach")
;;works, = (" Being the best at whatever I do (example: making top grades). Achieving more than most other people.")  "INSTRUCTIONS FOR Life Themes, Dreams, and Values:"  "Answer each question according to how important that theme is to you."   
;; (get-question-text  'SLFCOPE)
;;  (list (list 3 0) (list (my-make-symbol "this") 0))
;; (my-make-symbol "thisone")


;;GET-QUESTION-TEXT-FORMATED
;;
;;ddd
(defun get-question-text-formated (qvar &key add-instrs-p)
  "In U-data-functions RETURNS (values formated-qtext q-text-sym q-title q-instr) formated text from question-text list in the definition of the question in Q-questions.lisp"
  (let
      ((formated-qtext)
       )
    (multiple-value-bind (q-text-sym q-text-list q-title q-instr)
        (get-question-text qvar)
      (setf formated-qtext (print-list q-text-list :no-newline-p t))

      ;;add the question instructions?
      (when add-instrs-p 
        (setf formated-qtext (format nil  "~A~%   INSTRUCTIONS: ~A" formated-qtext q-instr )))
      (values formated-qtext q-text-sym q-title q-instr)
      )))
;;TEST
;; (get-question-text-formated 'SLFCOPE)
;; (get-question-text-formated 'SLFCOPE :add-instrs-p T )
        


;;XXX -------------------------------- OLDER -- LATER DELETE?? ----------------
;;
#|  OLDER NOT-WORKING WITH CURRENT SCHEME 
(defun get-multi-question-vars-text (quest-sym)
  "In U-data-functions.lisp, "
  (let*
      ((q-text-name (format nil "~AQ" quest-name))
       (q-text-sym (my-make-symbol q-text-name))
       (q-multi-var-list 
        (get-key-value-in-nested-lists  (list (list quest-name 0)) 
                                        *all-shaq-questions :return-list-p T))
       (length-q-multi-list (list-length q-multi-var-list))
       (q-instr-list)
       (q-title)
       (q-instr)
       (q-instr-list)
       (q-item-text-list)
       (n -1)
       )
    #|    (if
        (null q-var-list)
        (setf new-q-name (my-make-symbol (format nil "~AQ" quest-sym))
              q-var-list  (get-key-value-in-nested-lists 
                           (list (list category 0) (list new-q-name 0))
                           *all-shaq-questions :return-list-p T)))|#
    (loop
     for q-item-var-list in q-multi-var-list
     do
     (incf n)
     
     (cond
      ((= n 1)
       (setf quest-name (car q-item-var-list)
             quest-label (second q-item-var-list)
             quest-instr-list (fifth q-item-var-list)
             quest-title (first quest-instr-list)
             quest-instr (second  quest-instr-list)
             quest-type (sixth quest-instr-list))
       (cond
        ((listp q-instr-list)
         (setf q-title (first (second  q-instr-list))
               q-instr (second  (second  q-instr-list))
            quest-text (third q-instr-list)))
        ;;unless the q-instr text is specified, use default
        (t (setf quest-title "INSTRUCTIONS: Select ALL that apply to you."
           quest-instr ("Finish this SSS"))))
       ;;end n = 1
       )
      ((listp q-item-var-list)
       (setf q-item-str (first q-item-var-list)
             q-item-sym (my-make-symbol q-item-str)
             q-item-label (second q-item-var-list)
             q-text-list (fourth q-var-list))

       ;;appending the list to pass to the check-button-panel for items text
       (setf q-item-text-list (append q-item-text-list (list (car q-text-list))))
       )
       (t (setf q-text-list '("NO MULTI-QUESTION INFORMATION FOUND"))))

     #|      (setf q-instr-list (get-key-value-in-nested-lists 
                          (list (list category 0) (list q-instr-sym 0))
                          *all-shaq-questions :return-list-p T))|#

     ;;end loop      
     )
      

      (values q-text-sym q-text-list q-title q-instr)  
      ;;end get-question-text
      ))|#





;;xxx      ------------------   TEST DATA -------------------------
;;example date:
#|(setf  *test-quests1 
  '((STRING-VARS
     "RealPeopleSex.java"
     (RPESATISFIEDRELQ "Overall, I am extremely satisfied with my relationship with my partner.")
     (RPEFU "My partner and I have have fun together:")
     (RPEARGUEQ "My partner and I have arguments for at least several minutes:")
     )
    (STRING-VARS
     "RealPeople.java"
     (RPECOMMITQ "Degree of commitment to an intimate(romantic) relationship (lasting at least 3 months)")
     (RPEHAPFRIENDSQ "Almost all of my good friends are very successful and happy in almost every area of their lives including school and interpersonal relationships.")
     (RPEHMARRQ "I have (or have had) a very happy marital -- or marital-like relationship with someone for an extended period of time.")
     (RPENETWQ "I have developed an extensive, close network of friends and career-related persons with whom I share support and information.")
     (RPECLFR "In my life I have had a number of extremely close friends with whom I could discuss my innermost secrets, weaknesses, and problems.")
     (RPECLFAMQ "I feel extremely close with the members of the family I grew up in.")
     (RPENUMFRIENDSQ "Approximate number of friends in general with whom you interact socially -- outside of work or school settings--at least once a month.")
     (RPENUMCLOSEFRIENDSQ "Approximate number of EXTREMELY CLOSE friendships with which you are VERY SATISFIED."))))
|#


;;xxx ---------------------- CONVERSION FUNCTION (No longer needed?) ----------

#|  WORKS,  QUOTED OFF BECAUSE DON'T NEED IT ANYMORE??
;;CONVERT-JAVA-TO-SPSS-QUEST-NAMES
;;
;;ddd
(defun convert-java-to-spss-quest-names (&key (all-qvarlists *shaq-question-variable-lists))
  "In U-data-functions.lisp, A TEMP FUNCTION USED TO CHANGE THE NAMES IN THE LIST OF QUESTION  FROM THE OLD JAVA NAMES TO THE NEW SPSS NAMES"
  (let
      ((old-q-sym)
       (new-q-sym)
       (q-string)
       (new-qlist)
       (cat-sym)
       (new-cat-list)
       (new-shaq-quest-list)
       )
    (loop
     for cat-list in all-qvarlists
     with cat-sym
     do
     (setf cat-sym (first cat-list)
           cat-list (cdr cat-list)
           new-cat-list (list cat-sym))
     (loop
      for sl-list in cat-list
      with qvar
      with sublist
      with q-name
      with q-list
      with q-strings
      with new-q-sym
      do
      (cond
       ((listp sl-list)
        (setf qvar (first sl-list)
              sublist (fifth sl-list))
        (cond
         ((listp sublist)
          (setf q-name (third sublist))
       ;;  (afout 'out (format nil "q-name= ~A~% (list '(T 0)(list q-name 0))= ~A~%" q-name (list '(T 0)(list q-name 0))))
      ;; (afout 'out (format nil "q-name= ~A~%sl-list= ~A~%" q-name sl-list))
         (multiple-value-setq (q-list q-name)
               (get-key-value-in-nested-lists 
                (list '(T 0)(list q-name 0)) *all-shaq-questions :return-list-p t))     
         (setf new-q-sym (my-make-symbol (format nil "~AQ" qvar)))
       ;;  (afout 'out (format nil "q-strings= ~A~%" q-strings))
         )
        (t nil)))
       (t nil))     
      (cond
       (q-list
        (setf  q-strings (cdr q-list)
               new-qlist (list  new-q-sym q-strings q-name)
               new-cat-list (append new-cat-list (list new-qlist))))
       (t (setf new-q-sym (my-make-symbol (format nil "~AQ" qvar))
                new-qlist (list  new-q-sym 'NO-QUEST-STRING-FOUND)
                new-cat-list (append new-cat-list (list new-qlist)))))
        ;;end inner loop
        )
       (setf new-shaq-quest-list (append new-shaq-quest-list (list new-cat-list)))
       ;;end outer loop
       )
      ;;end
      new-shaq-quest-list
      ))
|#
;;test
;; SSS START TESTING HERE--TO RENAME QUESTS
#|  (defun print-new-shaq-questions ()
    (let
       ((new-shaq-qlist)
        )
      (setf new-shaq-qlist (convert-java-to-spss-quest-names))
      (print-nested-list new-shaq-qlist :stream t :incl-quotes-p t)
      ))|#
;; (print-new-shaq-questions)


;;  
;; (get-key-value-in-nested-lists (list '(T 0)(list 'smtExercizeQ 1)) *all-shaq-questions)


;;CALC-IS-QUEST-REVERSE-SCORED
;;
;;ddd
(defun calc-is-quest-reversed  (qvar &key answer-array)
  "In U-data-functions. Checks the scoring array Eg. LikeMe7Reverse vs LikeMe7 to see if question is reverse or normall scored.  Returns (values reversed-item-p item-norm-or-rev) REVERSED-ITEM  if finds word reverse in the name. item-norm-or-rev = NORMAL-ITEM if not reversed/  NOTE: answer-array can be a symbol. "
  (let*
      ((reversed-item-p )
       (item-norm-or-rev 'NORMAL-ITEM)
       )

    (unless answer-array
             (setf answer-array (fifth (fifth (get-qvarlist qvar)))))

    (when (symbolp answer-array)
      (setf answer-array (format nil "~A" answer-array)))

    ;;see if answer-array contains word "reverse"
    (when (search "reverse" answer-array :test 'char-equal)
      (setf reversed-item-p 'REVERSED-ITEM
            item-norm-or-rev 'REVERSED-ITEM))

            item-norm-or-rev 
    ;;return result NIL or REVERSED-ITEM
    (values reversed-item-p item-norm-or-rev)
    ))
;;TEST
;;  (calc-is-quest-reversed 'TBVWEAK)  works = REVERSED-ITEM
;;  (calc-is-quest-reversed 'THM10OTH)  works = nil
;;  (calc-is-quest-reversed 'COPBLAME)  works = REVERSED-ITEM
;;  (calc-is-quest-reversed 'COPBLAME :answer-array "PerCent6CopeReverse") = REVERSED-ITEM
;;  (calc-is-quest-reversed nil :answer-array 'FREQ7REVERSE) = REVERSED-ITEM
;; (calc-is-quest-reversed nil :answer-array 'FREQ7) = NIL
    