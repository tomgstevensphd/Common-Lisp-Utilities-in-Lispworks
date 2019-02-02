;;********************** U-misc-print.lisp ************************

;;ALSO SEE:  U-HQ&composite-scales.lisp



(DEFPARAMETER *HQ-SCALES-PRINT-INTRO-TEXT 
"SUCCESS AND HAPPINESS ATTRIBUTES QUESTIONNAIRE (SHAQ) SCORING INFORMATION 
Tom G. Stevens PhD
      
 NOTE:  It is highly recommended that you use the downloadable SHAQ APP to administer and score SHAQ.  It provides instant results.  Go to: http://www.csulb.edu/~~tstevens/success    

    ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 INFORMATION ABOUT YOUR SCALE AND QUESTION RESULTS 

 COMPLETE HQ SCALE/SUBSCALES SCORING INFORMATION
 (Note: There are no outcome, academic, or interest scales included here.)

  ")
(DEFPARAMETER  *HQ-BETA-WEIGHT-INFO "
BETA WEIGHTS:  Each scale score is multiplied by a BETA WEIGHT to get a weighted score [scales vary in how much they influence the overall HQ by their beta weights].  The beta weights are the value of the correlation coefficients (r) between that scale and the overall happiness outcome variable in our research study.

TO CALCULATE YOUR HQ SCORE: 
    1. Write YOUR SCALE/SUBSCALE RELATIVE-SCORE on the blank space beside each HQ scale. (From you scored answer sheet)
    2. Multiply the beta weight X your scale score = weighted-score. 
    3. Add all the weighted scores together to get a TOTAL-WEIGHTED-SCORE.  
    4. Use the following formula to calculate your HQ score:
     HQ-SCORE = 100 + [(total-weighted-score. -  13.3727)   X   (10 / 2.60388)]  
     (See above for explanation of the meaning of the HQ-score.)
    ")
(DEFPARAMETER *ALL-HQ-SCALES-INTRO
  " ---------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                                                   
                                                                    HQ SCALE/SUBSCALES SCORING INFORMATION
                                                             (Note: There are no outcome, academic, or interest scales included)  ")

(DEFPARAMETER *SCALES-PRINT-INTRO-TEXT "

   Success and Happiness Attributes Questionnaire (SHAQ):
   Complete HQ Scale/Subscales Scoring Information
   (Note: There are no outcome, academic, or interest scales included.)
   Tom G. Stevens PhD, tom.stevens@csulb.edu
")


(DEFPARAMETER  *NON-HQ-SCALE-RESULTS-EXPLAINATION-TEXT  "
                     
TO CALCULATE EACH SCALE SCORE: 
    1.  (From you scored answer sheet)

     (See above for explanation of the meaning of the HQ-score.)
    ")



(DEFPARAMETER *SHAQ-MANUAL-TEST-RESULTS-INTRO-pt1
           (format nil "SUCCESS AND HAPPINESS ATTRIBUTES QUESTIONNAIRE (SHAQ) SCORING INFORMATION 
Tom G. Stevens PhD
      
 NOTE:  It is highly recommended that you use the downloadable SHAQ APP to administer and score SHAQ.  It provides instant results.  Go to: http://www.csulb.edu/~~tstevens/success    

    ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

                                             INFORMATION ABOUT YOUR SCALE AND QUESTION RESULTS "))


(DEFPARAMETER *SHAQ-MANUAL-TEST-RESULTS-INTRO-pt2
           (format nil "~%                                                 
                                                                PART 1: YOUR MAIN SCALE RESULTS SUMMARY

    Your results are presented in two main parts: PART 1 lists all your MAIN SCALE SCORES ONLY.  Part 2 (in a separate window) lists ALL your results including all the scales, subscales, and question results, along with self-help internet links. Part 1 is meant to be an overview of your results.  Part 2 is a detailed view.

    PART 1: INFORMATION
    Part 1 gives your score on all main shaq scales you completed. each scale  consists of questions related to a common value or belief area, a life skill area, or other personal attribute that research has been shown to relates to success, happiness, health, relationship success, or other important life outcomes. 

    EACH SCALE INCLUDES THE FOLLOWING INFORMATION:
              * SCALE NAME AND SCALE DESCRIPTION. Often correlations with happiness or other outcomes are given. Correlations range from 0.0 to 1.000; 0.3 to 0.4 are good, 0.4 to 0.6 very good, above 0.6 unusually high for this type of research. (Statistical note: all quoted correlations are significant at the p < .001 level and sample N > 1500.) 
              * YOUR DATA. Your RELATIVE SCORE ranges from 0.0 to 1.000.  Normally the higher the score, the better.  The SD (standard deviation) is a measure of how much variation you had among the scale items.  For example, if you answered all the questions the same, the standard deviation would be about 0.0, meaning no variation/deviation at all.
              * GRAPHIC SCORE LINE: A graph line is drawn that reflects your relative score (see #2). Normally, the LONGER THE GRAPH, THE BETTER THE SCORE.
              * COMPARISON TO OTHERS' RESULTS.  Where possible, each scale also includes the Mean and Standard Deviation (SD) of all (3400) users in our research sample.

    ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"    ))


(DEFPARAMETER *SHAQ-MANUAL-TEST-RESULTS-INTRO-pt3
            (format nil "~%
                                                    ALL RESULTS--YOUR SCALE, SUBSCALE, AND QUESTION RESULTS
                                                                                         WITH SELF-HELP INFORMATION

    INFORMATION 
    Part 2 provides a detailed look at ALL your SHAQ results.  Each subsection contains each of the following:
         1. MAIN SCALE RESULTS  In part 2 each scale is listed with all it's subscales (if any) and all its questions. (Scale questions divided and listed under each subscale section).
         2. SUBSCALE RESULTS.  Many main scales have been subdivided further into subscales which provide you with more detailed information about yourself related to the larger, more general category represented by the main scale.  The scores are presented the same way the scale scores are. (Scores only included in downloaded SHAQ app version.)

            UNDERSTANDING YOUR SCALE and SUBSCALE SCORES: Each scale/subscale includes the following:
              * SCALE NAME AND SCALE DESCRIPTION. Often correlations with happiness or other outcomes are given. Correlations range from 0.0 to 1.000; 0.3 to 0.4 are good, 0.4 to 0.6 very good, above 0.6 unusually high for this type of research. (Statistical note: all quoted correlations are significant at the p < .001 level and sample N > 1500.) 
              * YOUR DATA. Your RELATIVE SCORE ranges from 0.0 to 1.000.  Normally the higher the score, the better.  The SD (standard deviation) is a measure of how much variation you had among the scale items.  For example, if you answered all the questions the same, the standard deviation would be about 0.0, meaning no variation/deviation at all. (On downloaded SHAQ APP ONLY. For manual paper version, the scores must be manually calculated using the paper answer sheet and HQ scoring instructions.)
              * GRAPHIC SCORE LINE: A graph line is drawn that reflects your relative score (see #2). Normally, the LONGER THE GRAPH, THE BETTER THE SCORE (On downloaded SHAQ APP ONLY)
              * COMPARISON TO OTHERS' RESULTS.  Where possible, each scale also includes the Mean and Standard Deviation (SD) of all (3400) users in our research sample.

         3. QUESTION RESULTS.  Your results for EVERY SCALE QUESTION are presented.  If the main scale has subscales, the questions are listed under the appropriate subscale.  The main scale includes all the questions of its' subscales.  Your individual question results lists every question that you answered. They are grouped by the scale of which they are members. Questions that are not part of any scale are listed separately, and questions that are part of more than one scale are listed under each scale. Each question lists essentially the same information listed above under the scales (except SD). Each question result includes the following:
               * The EXACT QUESTION you answered.
               * Your EXACT ANSWER to the question.
               * NORMAL OR REVERSED QUESTION. Relative-scores are generally scored so that a higher score is more desirable. For example, suppose a question read, \"I read poorly.\" and you answered \"I strongly agree.\"  Instead of giving a high relative score of 1.0 for the answer, SHAQ REVERSES the scoring so that the relative score would be a very low score. A note indicates when an item was a reverse-scored question.

         4. SELF-HELP SUGGESTIONS: In some cases, brief suggestions are printed out under the scale or question.
         5. INTERNET HELP LINKS: Our web site often contains specific suggestions on how to improve yourself in most of the areas covered by SHAQ. Some other internet self-help links are also provided.
          ==> WE STRONGLY SUGGEST THAT YOU FOLLOW and STUDY THE INTERNET HELP LINKS!

        UNDERSTANDING THE FACTORS UNDERLYING YOUR DEGREE OF HAPPINESS, SUCCESS, and OTHER OUTCOMES.  Correlations* from research results are included in information about many scales. The correlations are important for helping you determine the strength of the relationship between your scale scores and the emotional outcomes of happiness, low depression, low anxiety, and low anger/aggression. Other outcomes such as income, health, relationship outcomes, and academic success also have many moderate to strong correlations with SHAQ scales, but were only occassionally included with your results. For more information about these success factors, see the SHAQ research research summaries.

   *[UNDERSTANDING CORRELATIONS:  Correlations range from minus 1.0 to plus 1.0. A correlation of zero means there is no relationship between the variables (e.g. scale and happiness).  Generally correlations from 0.2 to 0.4 are moderate,  correlations of 0.4 to 0.6 are moderately high, and correlations above 0.6 are higher than usual in this type of research.
   Correlations ARE NOT ADDITIVE--that means that if  3 variables X1, X2, and X3 all are correlated with another variable (e.g. happiness), then their SUM CAN BE GREATER THAN 1.0.  (E.G. X1 could correlate 0.6 with happiness, X2  0.5 with happiness, and X3 .0.3 with happiness. The sum is 1.40, yet the maximum correlation for EACH individual factor is 1.0.) The reason for this apparent oddity is that X1, X2, and X3 can correlate with each other so that all 3 variables share some common factor with happiness.  You will see that many scales correlated 0.3 to 0.6 with happiness. These scales also correlated in similar ranges with each other. Put in a simpler way, more generally GOOD CORRELATES WITH GOOD!  People high in one positive factor tended to be high in others as well.  Similarly, people who scored low on one factor tended to score low on others as well. This was predicted by the theory behind SHAQ.  Part of the good news is that developing yourself on one important factor may have positive effects on other factors as well. For example, changing a top value (such as valuing honesty, integrity, or  happiness more) can have a wide effect on many other values, goals, and habits.]

   * TO LEARN MORE ABOUT SUCCESS OUTCOMES OR THE SHAQ RESEARCH, GO TO:  http://www.csulb.edu/~tstevens/h10conclusions.htm 
[Note: Correlations were included only when moderate and statistically significant, (normally p < .001).  SHAQ research included over 3400 people of a wide variety of ages, occupations, locations, religions, etc.]"))


(DEFPARAMETER   *NON-HQ-SCALES-INTRO 
  (format nil "~A~%~A~%
  ---------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                                                   
                                                                          
                                     OUTCOME, ACADEMIC-RELATED, AND CAREER-INTEREST SCALES
                                                                            (These are NON-HQ scales and subscales.)
       " *SHAQ-MANUAL-TEST-RESULTS-INTRO-pt1
         ;;no--for main scales only version *SHAQ-MANUAL-TEST-RESULTS-INTRO-pt2
          *SHAQ-MANUAL-TEST-RESULTS-INTRO-pt3))


(DEFPARAMETER   *BRIEF-NON-HQ-INTRO-TEXT "
                                                        INFORMATION

       Each SHAQ scale  consists of questions related to a certain important life area.

EACH SCALE INCLUDES THE FOLLOWING INFORMATION:
              * SCALE NAME AND SCALE DESCRIPTION. Often correlations with happiness or other outcomes are given. Correlations range from 0.0 to 1.000; 0.3 to 0.4 are good, 0.4 to 0.6 very good, above 0.6 unusually high for this type of research. (Statistical note: all quoted correlations are significant at the p < .001 level and sample N > 1500.) 
              * YOUR DATA. Your RELATIVE SCORE ranges from 0.0 to 1.000.  Normally the higher the score, the better.  The SD (standard deviation) is a measure of how much variation you had among the scale items.  For example, if you answered all the questions the same, the standard deviation would be about 0.0, meaning no variation/deviation at all. 
         ==> NOTE: If you did NOT take the online, downloaded app version of SHAQ, you must calculate your own scores using information provided on the manual answer sheet and below.
              * COMPARISON TO OTHERS' RESULTS.  Where possible, each scale also includes the Mean and Standard Deviation (SD) of all (3400) users in our research sample.
    " )
 

(DEFPARAMETER *HQ-SCALE-BETA-LIST
          `((VALUES-THEMES (sT1HigherSelf  0.380 )
          ( sT2SocIntimNoFam 0.256)
          (sT3FamCare 0.406 )
          (sT4SuccessStatusMater 0.295 )
          (sT5-OrderPerfectionGoodness 0.310 )
          (sT6GodSpiritRelig 0.302 )
          (sT7ImpactChallengeExplor 0.347 )
          (sT8AttentionFunEasy 0.311 )
          (sT9ValueSelfAllUncond 0.461 )
          (sT10OvercmProbAcceptSelf 0.185 )
          (sT11DutyPunctual 0.265 ))
          (BELIEFS
       (sworldview  not-hq-scale )
          (sswvgratpt  0.722 )
          (sswvoptims  0.543 )
          (sswventit  0.157 )
       (stbslfwo  not-hq-scale. )
          (ssswNonCont  0.364 )
          (ssswHapAllGrat  0.567 )
          (ssswAcAllSelf  0.160 )
       (siecontr  not-hq-scale)
          (ssieautony  0.553 )
          (ssiencodep  0.213 )
          (ssienother  0.246 )
       (sethbel   not-hq-scale);;new label
          (ssb2Ethic  0.346 )
          (ssb2Forgiv  0.282 )
          (ssb2IDgrnd  0.258 )
          (ssb2GrndMng  0.256 )
          (ssb2InrGood  0.327 )
          ;; (ssb2noAstr  0.087 )
          ;; (ssb2lifad  0.263 )
       (sgrfears   not-hq-scale)
          (sswfsocial  0.492 )
          (sswfself  0.574 )
          (sswfpovfai  0.380 )
          (sswfilldea  0.221 ))
     (SKILLS-CONFIDENCE
       (sslfconf   not-hq-scale)
          (sssclearn  0.408 )
          (sssccopopt  0.740 )
          (ssscsmsmsd  0.668 )
          (ssscinterp  0.577)
          (ssscallhelp  0.480 )
          (ssscscience  0.318 )
          (ssscartcre  0.349 )
       (sselfman   not-hq-scale)
          (sssmTimeManGoalSet 0.411 )
          (sssmAccompLoRush 0.553 )
          (sssmSelfDevel 0.558 )
          (sssmHealthHabs 0.460 )
       (semotcop   not-hq-scale)
          (sscpProbSolv  0.491 )
          (sscpPosThoughts  0.577 )
          (sscpPosActs  0.362 )
          (sscpNoBlameAngerWDraw  0.492 )
          (sscpNotSmokDrugMed  0.192 )
          (sscpNotEat  0.200 ))
         (INTERPERSONAL
          (IntSS1aAssertCR  0.463 )
          (IntSS1bOpenHon  0.503 )
          (IntSS2Romantc   0.394  )
          (IntSS3LibRole  0.166 )
          (IntSS4LoveRes  0.476 )
          (IntSS5Indep  0.379 )
          (IntSS6PosSup  0.421)
          (IntSS7Collab  0.406 )))
          "List of all HQ component scales/subscales with beta weights lists."
          )




;;PRINT-HQ-SCALE-INFO
;;2019  
;;ddd
(defun print-hq-scale-info (&key (hq-scale-beta-lists *HQ-SCALE-BETA-LIST)
                                 (include-explain-text-p T)(include-beta-weight-info-p T)
                                 (include-intro-in-brief-text-p T)
                                 (include-score-lines-p T))
  "In U-HQ&composite-scales.lisp. RETURNS (values scale-lists formated-scale-info) "
  (let
      ((brief-formated-text (format nil " HQ SCALES/SUBSCALES WITH BETA-WEIGHTS~%"))
       (formated-scale-info "") 
       (intro-text (format nil "~A~%" *HQ-SCALES-PRINT-INTRO-TEXT))
       (scale-info-lists  '("HQ SCALE/SUBSCALES WITH BETA WEIGHTS:"))
       ;;end let vars
       )
    (when include-explain-text-p
      (setf intro-text (format nil "~A~%~A" intro-text *HQResultsExplainationText)))
    ;;
    (when include-beta-weight-info-p
      (setf intro-text (format nil "~A~%~A~%" intro-text *HQ-BETA-WEIGHT-INFO)))

    (setf  intro-text (format nil "~A~%~A~%" intro-text *ALL-HQ-SCALES-INTRO))

    (setf  formated-scale-info  intro-text)
    (when include-intro-in-brief-text-p
      (setf brief-formated-text intro-text)
      )S
    ;;FOR EACH CATEGORY
    (loop
     for cat-list in hq-scale-beta-lists
     for n from 1 to 100
     do
     (let
         ((cat (car cat-list))
          (scale-beta-lists (cdr cat-list))
          (cat-inst-sym)
          (cat-title)
          (scale-group-description)
          (scale-group-info)
          )
       (setf cat-inst-sym (my-make-symbol (format nil "*~A-inst" cat))               
             cat-title (get-slot-value cat-inst-sym 'title)
             scale-group-description (get-slot-value cat-inst-sym  'scale-group-description)
             scale-group-info (format nil " 

                                               SCALE GROUP: ~A
              DESCRIPTION:  ~A  
                      =============================================================================================== "  cat-title scale-group-description)
             formated-scale-info (format nil "~A~A
" formated-scale-info
                                            scale-group-info)
             brief-formated-text (format nil "~A ~A~%" brief-formated-text scale-group-info))
       ;;FOR EACH SCALE-BETA SUBLIST  
       (loop
        for list in scale-beta-lists
        for n from 1 to 200
        do
        (let*
            ((scale-name) 
             (beta) 
             (non-hq-scale-p)
             (superclass-names)
             (superclasses)
             (super-scale-name)
             (super-scale-class)
             (super-scale-label) 
             )
          (setf scale-name (car list)
                beta (second list))
          (when (equal beta 'not-hq-scale)
            (setf non-hq-scale-p T))           
            
          ;;IS SCALE A SUBSCALE?
          (multiple-value-setq (superclass-names  superclasses)
              (find-direct-superclass-names scale-name))
          (when (string-equal (second superclass-names) "SUBSCALE")
            (setf super-scale-name (first superclass-names)
                  super-scale-class (first superclasses))
            (multiple-value-bind (sc-name-string sc-label)
                ;;not used sc-description sc-num-questions sc-scale-questions sc-mean-score )
                (get-scale-slot-values super-scale-name)
              (setf super-scale-label sc-label) )) ;;end mvb,when subscale
  
          (multiple-value-bind (name-string label  description num-questions 
                                            scale-questions mean-score )
              (get-scale-slot-values scale-name)

            ;;SET THE RETURN VALUES
            ;;Superscale?
           (when super-scale-name
              (setf formated-scale-info (format nil "~A~%  >> SUBSCALE of SCALE: ~A; SCALE-SYMBOL: ~A" formated-scale-info super-scale-label super-scale-name)
                    brief-formated-text (format nil "~A  >> SUBSCALE of SCALE: ~A; SCALE-SYMBOL: ~A~%" brief-formated-text super-scale-label super-scale-name)))
           ;;REST
            (setf scale-info-lists (append scale-info-lists (list name-string :label label
                                              :description description :num-questions   num-questions  
                                           :scale-questions scale-questions :mean-score  mean-score)
                                           (when super-scale-name (list :superscale super-scale-name)))

                  formated-scale-info (format nil "~A~%  SCALE SYMBOL: ~A~% LABEL: ~A    >>HQ-BETA: ~A~%   DESCRIPTION: ~A~%  NUM-QUESTIONS: ~A SCALE-QUESTIONS: ~A~%  MEAN-SCORE: ~A " formated-scale-info  name-string  label  BETA  description num-questions scale-questions mean-score)

                  brief-formated-text (format nil "~A    SCALE SYMBOL: ~A~% LABEL: ~A   >>HQ-BETA: ~A~%" brief-formated-text name-string label BETA))
            ;;Include score lines?
            (when include-score-lines-p
              (cond
               ;;hq-scale printout
               ((null non-hq-scale-p)
                (setf formated-scale-info (format nil "~A    WEIGHTED-SCORE [weight x  scale-score] = ~A  X  SCORE (0-1.0): ______ = _______.
 -----------------------------------------------------------------------------------------------------------------------------------------------------------" formated-scale-info beta)
                      brief-formated-text (format nil "~A    WEIGHTED-SCORE [weight x  scale-score] = ~A  X  SCORE (0-1.0): ______ = _______.~%  ---------------------------------------------------------------------------------------------------------------------------------------------------------- ~%" brief-formated-text beta))
                ;;end null non-hq-scale-p
                )
               ;;non-hq-scale-p printout
               (t  
                (setf formated-scale-info (format nil "~A    RELATIVE-SCALE SCORE (0-1.0): ______(Information only: Only this scale's SUBSCALES BELOW used in HQ calculations.)
 -----------------------------------------------------------------------------------------------------------------------------------------------------------" formated-scale-info beta)
                      brief-formated-text (format nil "~A    RELATIVE-SCALE SCORE (0-1.0): ______(Information only: Only this scale's SUBSCALES BELOW used in HQ calculations.)
 -----------------------------------------------------------------------------------------------------------------------------------------------------------" formated-scale-info beta))
                ;;end null non-hq-scale-p,cond,when include-score-lines-p
                ) ))
            ;;end mvb,let,scale-beta loop
            )))
       ;;end let, cat loop
       ))
    (values  formated-scale-info  brief-formated-text  scale-info-lists)
    ;;end let, print-hq-scale-info
    ))
;;TEST
;; (print-hq-scale-info)
      

;;USED ABOVE
;; (FIND-DIRECT-SUPERCLASS-NAMES "SSWVGRATPT") 
;; result= #<STANDARD-CLASS SWORLDVIEW 222FAAD3>  #<STANDARD-CLASS SUBSCALE 222AE0CF>   ("SWORLDVIEW" "SUBSCALE")  (#<STANDARD-CLASS SWORLDVIEW 222FAAD3> #<STANDARD-CLASS SUBSCALE 222AE0CF>)   SSWVGRATPT   #<STANDARD-CLASS SSWVGRATPT 2232F51B>
;;NOTE: (clos:class-direct-superclasses  *sswvgratpt-inst) ;; *IntSS4LoveRes-inst)
;;= Error: No applicable methods for #<STANDARD-GENERIC-FUNCTION CLASS-DIRECT-SUPERCLASSES 20BA35B2> with args (#<SSWVGRATPT 2CB57973>)


;; CL-USER 66 > (get-subscale-names 'acad-learning)
;;works= ("SCOLLEGE" "SSL1CONFIDEFFICSTUDYTEST" "SSL1BCONFIDNOTAVOIDSTUDY" "SSL2SATISCAMPUSFACFRIENDSGRDES" "SSL3WRITEREADSKILLS" "SSL4BLDMENTALSTRUCT" "SSL5BASICSTUDYSKILLS" "SSL6SELFMANACADGOALS" "SSL7MATHSCIPRINC" "SSL8STUDYENVIR" "SSL9ATTENDHW" "SSL10MEMNOTANX" "SSL11NOTNONACADMOT" "SSL12STDYTMAVAIL" "SSL13VERBALAPT" "SSL14MATHAPT")





;;PRINT-NON-HQ-SCALE-INFO
;;2019
;;ddd
(defun print-NON-HQ-scale-info (&key (scale-class-cats '(outcome 
                                                         acad-learning career-interest))
                                     (include-explain-text-p T) ;;(include-beta-weight-info-p T)
                                     (include-intro-in-brief-text-p T)
                                     (include-score-lines-p T))
  "In U-misc-print.lisp.lisp. RETURNS (values scale-lists formated-scale-info) "
  (let
      ((formated-scale-info "")
       (brief-formated-text (format nil " HQ SCALES/SUBSCALES S~%"))
       (intro-text (format nil "~A~%" *NON-HQ-SCALES-INTRO))
       (scale-info-lists  '("HQ SCALE/SUBSCALES "))
       ;;end let vars
       )
    (setf  formated-scale-info  intro-text)
    (when include-intro-in-brief-text-p
      (setf brief-formated-text *BRIEF-NON-HQ-INTRO-TEXT)
      )
    (loop
     for scale-cat in scale-class-cats
     for n from 1 to 100
     do
     (let*
          ((scale-names)
          (scale-insts) 
         (superclass-names)
          (superclasses)
          (super-scale-name)
          (super-scale-class)
          (super-scale-label)
          (scalelist-info)
                    (cat-title)
          (scale-group-description)
          (scale-group-info)
          (cat-inst-sym (my-make-symbol (format nil "*~A-inst" scale-cat)))       
          (cat-title (get-slot-value cat-inst-sym 'title))
          (scale-group-description (get-slot-value cat-inst-sym  'scale-group-description))
          (scale-group-info)
          )
     (multiple-value-setq (scale-insts  scale-names)
         (get-subscale-names scale-cat))
      
         (setf  scale-group-info (format nil " 

                                               SCALE GROUP: ~A
              DESCRIPTION:  ~A  
                      =============================================================================================== "  cat-title   scale-group-description)
            formated-scale-info (format nil "~A~A
"   formated-scale-info scale-group-info)
             brief-formated-text (format nil "~A ~A~%" brief-formated-text  scale-group-info))
              
       (loop
        for scale-name in scale-names
        for n from 1 to 100
        do
        (let
            ((x)
             )
          ;;IS SCALE A SUBSCALE?
          (multiple-value-setq (superclass-names  superclasses)
              (find-direct-superclass-names scale-name))
          (when (string-equal (second superclass-names) "SUBSCALE")
            (setf super-scale-name (first superclass-names)
                  super-scale-class (first superclasses))
            (multiple-value-bind (name-string label  description num-questions 
                                              scale-questions mean-score )
                (get-scale-slot-values super-scale-name)
              (setf super-scale-label label)))
  
          (multiple-value-bind (name-string label  description num-questions 
                                            scale-questions mean-score )
              (get-scale-slot-values scale-name)

            ;;SET THE RETURN VALUES
            (setf scale-info-lists (append scale-info-lists (list name-string :label label
                                              :description description :num-questions   num-questions  
                                           :scale-questions scale-questions :mean-score  mean-score)
                                           (when super-scale-name (list :superscale super-scale-name)))  
   
                  formated-scale-info (format nil "~A ~%  SCALE SYMBOL:  ~A~% LABEL: ~A %   DESCRIPTION: ~A~%  NUM-QUESTIONS: ~A SCALE-QUESTIONS: ~A~%  MEAN-SCORE: ~A " formated-scale-info  name-string  label  description num-questions scale-questions mean-score)

                  brief-formated-text (format nil "~A ~%  SCALE SYMBOL:  ~A~% LABEL:  ~A" brief-formated-text name-string label ))
            (when super-scale-name
              (setf formated-scale-info (format nil "~A~%  >> SUBSCALE of SCALE: ~A; SCALE-SYMBOL: ~A~%" formated-scale-info super-scale-label super-scale-name)

                    brief-formated-text (format nil "~A~%  >> SUBSCALE of SCALE: ~A; SCALE-SYMBOL: ~A~%" brief-formated-text super-scale-label super-scale-name)))

            (when include-score-lines-p
              (setf formated-scale-info (format nil "~A    
   SCALE-SCORE:  Sum-of-scale-question-scores: ________ /  Number-of-scale-questions:  ~A  = SCALE-RELATIVE-SCORE (0-1.0: ________
 ---------------------------------------------------------------------------------------------------------------------------------------------------------- " formated-scale-info num-questions)

                    brief-formated-text (format nil "~A    
   SCALE-SCORE:  Sum-of-scale-question-scores: ________ /  Number-of-scale-questions:  ~A  = SCALE-RELATIVE-SCORE (0-1.0: ________
 ---------------------------------------------------------------------------------------------------------------------------------------------------------- " brief-formated-text num-questions)))
            ;;end mvb,let, scales loop
            )))
       ;;end let, cats loop
       ))
    (values  formated-scale-info brief-formated-text  scale-info-lists)
    ;;end let, print-non-HQ-scale-info
    ))
;;TEST 
;; (print-non-HQ-scale-info)




;;XXX============ FUNCTIONS FOR CREATING THE SHAQ MANUAL QUESTION AND ANSWER FORMS (including listing reverse scored, rel-scores for each answer, etc) ======================


;;PRINT-NEW-SCALE-SUBSC-QUESTS-W-ANSFORM-QNUMS
;;2019
;;ddd
(defun print-new-scale-subsc-quests-w-ansform-qnums
       (&key (filepath  
              "C:/3-TS/LISP PROJECTS TS/SHAQ-print-version/Q-quests-by-scale-subscales.lisp")
             (max-file-lines 5000)
             (line-pt1-end 50)
             use-fuzzy-matcher-p
             search-only-part-p
             (add-newline-before-nonkey-lines-p T))
  "In  U-misc-print.lisp"
  (let
      ((all-text)
           (scale-info)
           (subscale-info)
           (subscale-scale-info)
           (new-scale-p)
           (new-subscale-p)
           (new-instrs-p T)
       )
    (with-open-file (instr filepath :direction :input)
      (loop
       for line-n from 1 to max-file-lines
       do
      (let*
          ((line (read-line  instr  nil "eof")) 
           (line-len (length line))
           (line-pt1)
           (begin-n)
           (numline)
           (end-n)
           (newline)
           (match-quest-text)
           )
        (cond
         ((equal line "eof")
          (return))
         (t
          (cond
           ;;FOR    >>>>>>>>>>>   QUESTIONS FOR SCALE:
           ((and (> line-len 20)
                 (search "FOR SCALE:" line  :start2 2  :test 'string-equal))
            (setf scale-info line
                  new-instrs-p T
                  new-scale-p T
                   all-text (format nil "~A~%~A" all-text scale-info))
                  ;;not needed?(find-words-numbers (subseq line 18) :letter-only-p T))
            ;;end scale 1
            )
           ;;        ==>  QUESTIONS FOR SUBSCALE:  Achievement Confidence   
            ;;      (SUBSCALE OF SCALE: Self-Confidence and Life Skill Areas)
           ((and (> line-len 38)
                 (search "FOR SUBSCALE:" line  :start2 2 
                         :test 'string-equal))
            (setf scale-info line
                  new-instrs-p T
                  all-text (format nil "~A~A
         ====> SCALE SCORE(0-1.0): _____[sum/num-quests]" all-text line)
                  new-scale-p NIL
                  new-subscale-p T)
            )
           ;;SCALE FOR SUBSCALES
           ((and (> line-len 38)
                 (search "SUBSCALE OF SCALE:" line  :start2 0 :end2 38
                         :test 'string-equal))
            (setf all-text (format nil "~A~%~A
           ===> SUBSCALE SCORE(0-1.0): _____[sum/num-quests]" all-text line))
            )
           ((and (> line-len 20)
                 (search ">> QUESTION:" line  ;; :start2 0 :end2 25
                         :test 'string-equal))
                 ;; >> QUESTION: 1.  "Your Highest Education Completed:  
      ;;  (search ">> QUESTION:" ">> QUESTION: 1.  \"Your Highest Education Completed:  " :test 'string-equal)  = 0
            (cond
             ((and search-only-part-p (> line-len 70))
              (setf match-quest-text (subseq line 40 (- line-len 15))))
             ((and search-only-part-p (> line-len 50))
              (setf match-quest-text (subseq line 30 (- line-len 10))))
             ((> line-len 40)
              (setf match-quest-text (subseq line 24 (- line-len 5))))
             ((> line-len 25)
              (setf match-quest-text (subseq line 22 (- line-len 2))))
             (t (setf match-quest-text (subseq line 21))))
                 ;; qnum (find-first-letter/integer (subseq line 18) :integer-only-p T)
                 ;;quest (subseq line 17 )
             (setf quest-info line)
            ;;(break "ans-scale")
            
            ;;MATCH THIS QUEST TO QUEST ON QUESTIONS FORM TO GET SCALE AND NUMBER INFO
            (multiple-value-bind (ans-scale ans-quest ans-qnum)
                (get-answer-form-print-info match-quest-text
                                            :use-fuzzy-matcher-p use-fuzzy-matcher-p)
              (when (null ans-quest)
                (setf ans-quest "**NO MATCHED QUEST"))
            (setf all-text (format nil 
 "~A~%  NEW: ~A
    ====>> GET SCORE from OLD SCALE:~%   ~A
         ====>> MATCHED-OLD-QUESTION:
         ===OLD QUEST NUMBER=> ~A  REL-SCORE(0-1.0): ______ <=====
         ==> ~A  <<====" 
                                   all-text quest-info ans-scale ans-qnum ans-quest))
            (setf match-quest-text "")
            ;;end mvb,quest line
            ))
           ((and new-scale-p (> line-len 20)
                 (search "INSTRUCTIONS:" line  :start2 0 :end2 20 
                         :test 'string-equal))
            (when new-instrs-p
              (setf all-text (format nil "~A~%==> A" 
                                     all-text line)
                    new-subscale-p NIL
                    new-scale-p NIL
                    new-instrs-p NIL))
            )            
             ;;SSSSS FINISH WRITING THIS FUNCT BELOW
             (t (setf all-text (format nil "~A~%~A" all-text line))))
          ;;end read loop
          ))
      
      ;;end let,with-open
      ))
      (values all-text)
    ;;end let, print-new-scale-subsc-quests-w-ansform-qnums
    )))
;;TEST
;; (print-new-scale-subsc-quests-w-ansform-qnums)
;; (print-new-scale-subsc-quests-w-ansform-qnums :search-only-part-p T)
;; (print-new-scale-subsc-quests-w-ansform-qnums :search-only-part-p T  ;use-fuzzy-matcher-p use-fuzzy-matcher-p)



;;GET-ANSWER-FORM-PRINT-INFO
;;2019
;;ddd "C:/3-TS/LISP PROJECTS TS/SHAQ-print-version/0 SHAQ ALL QUESTS2-docx.lisp"
(defun get-answer-form-print-info   (match-quest-text &key (filepath  
                                                            "C:/3-TS/LISP PROJECTS TS/SHAQ-print-version/0 SHAQ ALL QUESTS2-docx.lisp") 
                                                      (match-begin-n 0)(match-from-end-n 1)
                                                      (max-file-lines 5000) 
                                                      (not-found-return "**QUESTION NOT FOUND")
                                                      use-fuzzy-matcher-p
                                                      )
  "In   U-misc-print.lisp  RETURNS (values ans-scale ans-qnum)"
  (let*
      ((all-text)
       (scale-info)
       (quest-info)
       (qnum)
       (match-q-len (length match-quest-text))
       (match-substr (subseq match-quest-text 
                             match-begin-n (- match-q-len  match-from-end-n)))
       )
    (with-open-file (instr filepath :direction :input)
      (loop
       for line-n from 1 to max-file-lines
       do
       (let*
           ((line (read-line  instr  nil "eof")) 
            (line-len (length line))         
            )
         (cond
          ((equal line "eof")
           (return))
          (t
           (cond
            ;;FOR    >>>>  QUESTIONS FOR SCALE:
            ((and (> line-len 32)
                  (search "QUESTIONS FOR SCALE:" line  :start2 5 :test 'string-equal))
             ;;(break)
             (setf scale-info (subseq line 28))
             ;;not needed?(find-words-numbers (subseq line 18) :letter-only-p T))
             ;;end scale 1
             )
            ;;MATCH THIS QUEST TO QUEST ON QUESTIONS FORM TO GET SCALE AND NUMBER INFO
            ;;try non fuzzy matcher first (if use-fuzzy-matcher-p) ;;DIDN'T HELP
            ((and (>= line-len (- match-q-len 1))
                  (search match-substr line))
             (setf qnum (find-first-word-number (subseq line 0 9)
                                                                       :integer-only-p T)
                   quest-info line)
             (return)
             ;;end match
             )
            ((and use-fuzzy-matcher-p
                  (>= line-len (- match-q-len 1))
                  (fuzzy-matcher match-substr line))
             (setf qnum (find-first-word-number (subseq line 0 9)
                                                                       :integer-only-p T)
                   quest-info line)
             (return)
             ;;end match
             )   
                      
            ;;otherwise ignore line
            (t nil))
           ;;end t,cond,let,loop
           ))))
      (when (null quest-info)
        (setf scale-info nil
              quest-info not-found-return))
      (values scale-info quest-info qnum)
      ;;end with,let,get-answer-form-print-info
      )))
;;TEST
;; (get-answer-form-print-info " I spend a lot of time thinking about the future, making plans, and working toward completing distant goals.")
;; result=" Management Skills  <<<<<   Number of Questions = 15"  "2. I spend a lot of time thinking about the future, making plans, and working toward completing distant goals."     "2"
;; (get-answer-form-print-info   "INDEPENDENCE: Being independent, and living according to my own values and dreams.")




;;PRINT-TEXT-LISTS-W-ADDED-NUMS
;;2019
;;ddd
(defun print-text-lists-w-added-nums (filepath &key (begin-numline '("{"))
                                               (end-numline "}") (max-file-lines 1000)
                                               (add-newline-before-nonkey-lines-p T))
  "In  U-misc-print.lisp"
  (let
      ((all-text)
       )
    (with-open-file (instr filepath :direction :input)
      (loop
       for line-n from 1 to max-file-lines
       do
      (let
          ((line (read-line  instr  nil 'eof)) 
           (begin-n)
           (numline)
           (end-n)
           (newline)
           )
        (cond
         ((equal line 'eof)
          (return))
         (t
          (loop
           for key in begin-numline
           do
            (setf begin-n (search key line ))
           (when (numberp begin-n)
             (setf numline (subseq line begin-n))
             (return))
           ;;end loop
           )
          (cond
           ((null begin-n)
            (when add-newline-before-nonkey-lines-p
              (setf all-text (format nil "~A~%" all-text)))
            (setf all-text
                  (format nil "~A~%~A" all-text line)))
           (t (setf newline (add-nums-to-string numline :end-numline end-numline)
               all-text (format nil "~A~%~A" all-text newline))))
          ;;end cond, read loop
          )))
      
      ;;end let,with-open
      ))
      (values all-text)
    ;;end let, print-text-lists-w-added-nums
    ))
;;TEST
;; (print-text-lists-w-added-nums "c:/3-ts/lisp projects ts/test-shaq-text.lisp")
;; (print-text-lists-w-added-nums "c:/3-ts/lisp projects ts/1 SHAQ QUESTIONS FORM.lisp")
;; (print-text-lists-w-added-nums "c:/3-ts/lisp projects ts/1 SHAQ Outcome-Interest Quest-Ans.lisp")


;;ADD-NUMS-TO-STRING
;;2019
;;ddd
(defun add-nums-to-string (string &key (numkey '( "{" "," )) (start-n 0)
                                               (str-betw-nums "; ") (end-numline "}")
                                               newline-p  (max-n 30))
  "In  U-misc-print.lisp"
  (let
      ((newline "")
          (teststr string)
       )
    (loop
     for n from 1 to max-n
     do
     (let
         ((begin-n)
          (end-n)
          (numstr)
          (new-numstr)
          )
          (loop
           for key in numkey
           do
            (setf begin-n (search key teststr ))
            ;;(afout  'out (format nil "1. begin-n= ~A end-n= ~A numstr= ~A" begin-n end-n numstr))
            
           (when (numberp begin-n)
             (setf teststr (subseq teststr (+ begin-n 1)))
             ;;(afout   'out (format nil "1b.  teststr= ~A" teststr))
             (loop
              for key in numkey
              do
             (setf end-n (search key (subseq teststr 1))) ;; end-n) ))
             (when (numberp end-n)   
               (setf end-n (+ end-n 1))
               ;;go past old begin key
               (cond
                ((> end-n 1)
                 (setf numstr (subseq teststr 0 end-n)))
                (t (setf numstr "")))
               (setf numstr (remove end-numline numstr :test 'string-equal))
               ;;done later  teststr (subseq teststr end-n))
               (return))
             ;;end inner loop
             )
             ;;if next key never found, assume numstr = teststr
             (unless numstr 
               (setf numstr (remove end-numline teststr :test 'string-equal)
                     teststr ""))                                

             ;;(afout   'out (format nil "2. begin-n= ~A end-n= ~A numstr= ~A" begin-n end-n numstr))

             ;;WRITE NEW-NUMSTR
             (when numstr
               (incf start-n)
               (setf new-numstr (format nil "~A. ~A" start-n numstr))
               (when str-betw-nums
                 (setf new-numstr (format nil "~A~A" new-numstr str-betw-nums)))
               (cond
                (newline-p
                 (setf newline (format nil "~A~%~A"  newline new-numstr)))
                (t (setf newline (format nil "~A   ~A"  newline new-numstr))))

          ;;(afout   'out (format nil "3. teststr= ~A~% new-numstr= ~A end-n= ~A" teststr new-numstr end-n ))

               ;;reset vars
               (cond
                ;;go past last key if long enough
                ((and end-n (> (length teststr) end-n))
                  (setf  teststr (subseq teststr end-n )))
                (t (setf  teststr "")))
               (setf  begin-n nil end-n nil numstr nil new-numstr nil)
               ;;(afout   'out (format nil "END teststr= ~A" teststr))
               ;;return newline
               (return)
               ;;end when numstr
               )
          ;;end when numberp begin-n, let, key loop
           ))
          (when (equal teststr "")
            (return))
          ;;end let, line loop
          ))
    newline
    ;;end let, add-nums-to-string
    ))
;;TEST
;; (add-nums-to-string   "{Student, Manager/executive, Professional, People-oriented, Professional, Technical, Consultant, Educator, Sales, Technician, Clerical, Service, Own business +10 employees, Other self-employed, Other}")
;;(add-nums-to-string  "{3.75-4.00, 3.50-3.74, 3.25-3.49, 3.00-3.24, 2.75-2.99, 2.50-2.74, 2.25-2.49, 2.00-2.24, 1.75-1.99, 1.50-1.74, 1.25-1.49, 1.00-1.24, Below 1.00, No GPA yet}")
;;works= "   1. 3.75-4.0;    2.  3.50-3.7;    3.  3.25-3.4;    4.  3.00-3.2;    5.  2.75-2.9;    6.  2.50-2.7;    7.  2.25-2.4;    8.  2.00-2.2;    9.  1.75-1.9;    10.  1.50-1.7;    11.  1.25-1.4;    12.  1.00-1.2;    13.  Below 1.0;    14.  No GPA yet; "
;; (add-nums-to-string   "{EXTREMELY accurate / like me, MODERATELY accurate / like me, MILDLY accurate / like me, UNCERTAIN, neutral, or midpoint, MILDLY inaccurate / unlike me, MODERATELY inaccurate / unlike me, EXTREMELY inaccurate / unlike me}")
;;works= "   1. EXTREMELY accurate / like me;    2.  MODERATELY accurate / like me;    3.  MILDLY accurate / like me;    4.  UNCERTAIN;    5.  neutral;    6.  or midpoint;    7.  MILDLY inaccurate / unlike me;    8.  MODERATELY inaccurate / unlike me;    9.  EXTREMELY inaccurate / unlike me; "

  


;;PRINT-ALL-Q-REL-SCORES
;;2019
;;ddd
(defun print-all-q-rel-scores (filepath &key (max-file-lines 1000)
                                        add-rest-lines-p (default-quest-n 'q-num-error)
                                        add-newline-before-non-lines-p
                                        (min-first-word-length 1)
                                        max-qnum-test-n  (min-ans-semi-colons 3))
  "In  U-misc-print.lisp. Reads a SHAQ quest & answer text file and makes an ANSWER FORM with each line =  Eg. 1____(1.0);  2____(.67); etc Looks for *REV in line for reverse scored items, and start with lowest score instead of highest."
  (let
      ((all-text "")
       (quest-num default-quest-n)  ;;
       (rev-scored-p)
       (removed-text-lines)
       )
    (with-open-file (instr filepath :direction :input)
      (loop
       for line-n from 1 to max-file-lines
       do
      (let*
          ((line (read-line  instr  nil 'eof)) 
           (line-integers) 
           (Qline-integers)
           (n-integers)
           (end-n)
           (begin-n)
           (test-str)
           (numline)
           (ans-nums)
           (ans-line-text)
           (answers)
           (n-answers)
           (line-len)
           (line-qnum-n max-qnum-test-n)
           (test-qnum-str)  ;;(subseq line 0  max-qnum-n
           (qtest-integers-n 0)
           (removed-text-line)
           (n-semi-colons 0)
           (answer-line-p)
           )
        ;;(afout 'out (format nil "BEGIN line= ~A" line))
        (cond
         ((equal line 'eof)
          (return))
         ((search "QUESTIONS FOR SCALE" line)
          (setf all-text (format nil "~A~%  ~A" all-text line)))
         ;;NOT EOF, A GOOD LINE
         (t 
          ;;FIND QUEST NUMBER
          ;;pre find integers, find part of line to test for quest number 
          (setf line-len (length line))
          (cond
           ;;WORKS BETTER IF CHECKS WHOLE LINE--LESS ERRORS?
           ;;SET max-qnum-test-n NIL
           ((null max-qnum-test-n)
            (setf line-qnum-n line-len))
           ((< line-len max-qnum-test-n) 
            (setf line-qnum-n line-len))
           (t (setf line-qnum-n max-qnum-test-n)))

          ;;FIND ALL INTEGERS IN THE FIRST PART OF STRING
          (setf test-qnum-str (subseq line 0  line-qnum-n)
                Qline-integers (find-integers-in-string test-qnum-str)
                qtest-integers-n (list-length Qline-integers ))
           (when (=  qtest-integers-n 1)
              (setf quest-num (car Qline-integers)))

          ;;FIND ALL INTEGERS IF AN ANSWER LINE
          (setf  line-integers   (find-integers-in-string  line)
                 n-integers (list-length line-integers))
          ;; Find "1."; "2." and "3." to see if is an answer line
          ;; Also, look for quest number at begin of line
          (cond
           ;;IF LINE IS A QUESTION , FIND IF REV SCORED?
           ((<  n-integers 2)

            ;;START TEXT LINE WITH BEGINNING TEXT (not other chars)
            (multiple-value-bind (first-found-item found-n rest-string word-len )
                (find-first-word-number line :letter-only-p T)

              (when (>= word-len min-first-word-length)
                (setf test-str (format nil "~A~A" first-found-item rest-string))
                ;;(break "first line")
             
                ;;TEST REVERSE ITEM TEXT FOR MATCH W Q TEXT
                ;;MUST COMPILE FILE: "Q-shaq-reversed-Qs.lisp" 
                ;;       to get *shaq-reverse-scored-quests            
                ;;old (setf test-str (subseq line begin-n end-n))
                (setf rev-scored-p
                      ;;how much of line to match?? try whole question
                      ;;won't find "eat" or other very short?
                      (find-list-item-by-substrings test-str *shaq-reverse-scored-quests))

                ;;(afout  'out (format nil "AT end n-integers, line= ~A~%test-str= ~A~%rev-scored-p= ~A" line test-str rev-scored-p))
                ;;(when (string-equal test-str "eat") (break "eat"))
                ;;end n-integers
                )               
              ;;END OF QUEST-TEXT LINE (< n-integers 2)
              ))
           ;;IF LINE IS AN ANSWER LINE
           ((> n-integers 1)
             ;;IF IT ALSO HAS SEMI-COLONS
             (multiple-value-bind (semi-colons subitems n-scs)
                 (match-all-substrings '(";") line)
               (setf n-semi-colons n-scs))
             (cond
              ((>= n-semi-colons min-ans-semi-colons)
               (setf answer-line-p T))
              (t (setf answer-line-p NIL)))
               
             (cond
             ;;FOR ANSWER LINES (answer-line-p = T)
             (answer-line-p                
                 ;;make 
            ;;Is line reverse scored
            ;;USE FOLLOWING IF "*REV"  WRITTEN ON LINES
#|            (setf numline line
                  rev-scored-p (search line "*REV"))|#
            (setf answers (nth-value 2 (divide-string-to-tokens  line  NIL
                                                                      :word-delim-list '(";") :delete-list '(" " "  ")))
                       n-answers (list-length answers))
            ;;CALC REL-SCORES
            (cond
             ;;If not rev scored, start with largest rel-score
             ((null rev-scored-p)
              (setf ans-line-text
               (print-all-rel-scores :n quest-num :max-score-list 
                                     (list n-answers) :reverse-rel-scores-p T)))
             ;;If reverse scored, start with smallest rel-score
             (rev-scored-p
              (setf ans-line-text
               (print-all-rel-scores  :n quest-num :max-score-list (list n-answers))))
             (t nil))
            ;;(afout 'out (format nil "rev-scored-p= ~A, ans-line-text= ~A~%" rev-scored-p ans-line-text))
            ;;ADD THE NEW LINE TO ALL-TEXT
            (setf all-text (format nil "~A~A" all-text ans-line-text))
#|   before used semi-colons to ID answer line         (cond
             ((quest-num
               (setf all-text (format nil "~A~A" all-text ans-line-text))))
             ;;no quest-num probably meeans misc number in the question text itself
             ;;   so remove previous item on all-text list (probably a false answer line)
             (t (setf removed-text-line (last all-text)
                      all-text (butlast all-text)
                      all-text (format nil "~A~A" all-text ans-line-text)
                            removed-text-lines (append removed-text-lines 
                                                       (list removed-text-line)))))|#
            ;;reset the quest-num
            (setf quest-num  default-quest-n)
            ;;(break "END ans line")
            ;;reset rev-scored-p
            (setf rev-scored-p nil)
            ;;END ANSWER LINE (> n-integers 1)
            )
             ;;MULTI-INTEGERS, BUT NOT ANSWER LINE
             (add-rest-lines-p
              (when add-newline-before-non-lines-p
                (setf all-text (format nil "~A~%" all-text)))
              (setf all-text (format nil "~A~A" all-text  line))  ;;was ~% 
              )
             (t nil))
             ;;end (> n-integers 1)
             )           
           ;;IF ADD-REST-LINES-P , JUST ADD THE LINE TO ALL-TEXT
           (add-rest-lines-p
            (when add-newline-before-non-lines-p
              (setf all-text (format nil "~A~%" all-text)))
            (setf all-text (format nil "~A~A" all-text  line))  ;;was ~% 
         )
           (t  nil))
          ;;end good line cond
          ) )
             ;;end let, loop
             ))
      all-text
            ;;XXX
            ;;end with, let, print-all-q-rel-scores
          )))
;;TEST  XXX
;; 
;; (print-all-q-rel-scores "c:/3-ts/lisp projects ts/test-output-1.lisp")
;; (print-all-q-rel-scores "c:/3-ts/lisp projects ts/0 SHAQ ALL SCORED  QUESTS-docx.lisp")
;; WORKED, COPIED TO" "0 SHAQ ANSWER FORM ALL-PARTS.docx"
;; (print-all-q-rel-scores "c:/3-ts/lisp projects ts/0 SHAQ QUESTS-INTEREST.lisp")
;; (print-all-q-rel-scores "c:/3-ts/lisp projects ts/0 SHAQ QUESTS-OUTCOME.lisp")
;;WORKS EG. BELOW:
#|  >>>>>  QUESTIONS FOR SCALE: Academic Achievements and Aptitudes Scale  <<<<<  Number of Questions = 13         
 1.  1___(1.0);  2___(0.88);  3___(0.74);  4___(0.62);  5___(0.5);  6___(0.38);  7___(0.24);  8___(0.12);             
 2.  1___(1.0);  2___(0.91);  3___(0.83);  4___(0.75);  5___(0.66);  6___(0.58);  7___(0.5);  8___(0.41);  9___(0.33);  10___(0.25);  11___(0.16);  12___(0.08);           
 3.  1___(1.0);  2___(0.9);  3___(0.8);  4___(0.7);  5___(0.6);  6___(0.5);  7___(0.4);  8___(0.3);  9___(0.2);  10___(0.1);        
 4.  1___(1.0);  2___(0.85);  3___(0.71);  4___(0.57);  5___(0.43);  6___(0.28);  7___(0.14);   ETC.  |#  
;;EXAMPLE OF REVERSE-SCORED ITEM:
;; 8.  1___(0.12);  2___(0.25);  3___(0.38);  4___(0.5);  5___(0.62);  6___(0.75);  7___(0.88);  8___(1.0);       
#|(nth-value 2 (divide-string-to-tokens  "1. 81 to 100 percent;    2.  61 to 80 percent;    3.  41 to 60 percent;    4.  21 to 40 percent;    5.  1 to 20 percent;    6.  never/almost never;" NIL :word-delim-list '(";")))
("1. 81 to 100 percent" "    2.  61 to 80 percent" "    3.  41 to 60 percent" "    4.  21 to 40 percent" "    5.  1 to 20 percent" "    6.  never/almost never;")|#
;;to fix a problem
;; (print-all-q-rel-scores "c:/3-ts/lisp projects ts/1 SHAQ-temp-missing-Qs.lisp")




;;PRINT-ALL-REL-SCORES
;;2019
;;ddd
(defun print-all-rel-scores (&key (N "X") (stream nil)  reverse-rel-scores-p 
                                    reverse-nums&scores-p
                                    (max-score-list '(14 13 12 11 10 9 8 7 6 5 4 3))(digits 2)
                                    (intro-text "  SHAQ RELATIVE SCHORES   ")
                                    output-score-list-p  (0-score 0.1))            
  "In U-misc-print.lisp, prints possible relative scores given a max-score. Sets rel-score = 0 if less than 0-score. RETURNS: for each max-score returns a line with numbers blank for answer, and rel-score in parens."
  (let
      ((all-rel-score)
       (output-text "")
       (all-scores-list)
       )
    (loop
     for max-score in max-score-list
     ;;for n from 1 to (car max-score-list)
     do
     (let
         ((scores-text (format nil "~% ~A." n))
          (scores-list)
          (min-score  (cond ((my-greaterp (/ 1.0 max-score)  0))(t 0)))
          )
       (loop
        for score from 1 to max-score
        do        
        (let*
            ((score1  (cond
                       (reverse-nums&scores-p   (+ (- max-score score) 1))
                       (t score)))           
             (rel-score (cond ((> score1 0) 
                               (my-round  (/  score1  max-score ) :dec digits))
                              (t 0)))
             (rel-score1 (cond ((> rel-score 0-score) rel-score)
                               (t 0)))
             (score-text)
             )
                   
            ;;(when (= score1 1)(break "score1"))
          ;;reverse-rel-scores-p
          (when reverse-rel-scores-p
            (cond
             ((> rel-score1 0) 
              (setf rel-score1 (my-round (- (+ 1.0 min-score) rel-score1) :dec digits)))
             (t (setf rel-score1 1.0))))

         ;; (when (= score1 1)(break "score1"))
          ;;make the score text
          (setf score-text (format nil " ~A___(~A);" score1 rel-score1))

          ;;(format t "max-score= ~A score= ~A" max-score score1)
          (when output-score-list-p
            (setf scores-list (append scores-list (list (list max-score score1 rel-score1)))))
          (setf scores-text (format nil " ~A ~A" scores-text score-text))
          ;;(break "here")
          ;;end let,loop
          ))
       (when output-score-list-p
         (setf all-scores-list (append all-scores-list (list scores-list))))
       (setf output-text (format nil "~A ~A"
                                 output-text scores-text))
       ;;end outer let, loop
       ))
    (format stream "~A~%~A" intro-text output-text)
    (values output-text all-scores-list)
    ;;end let, print-all-rel-scores
    ))
;;TEST
;; (print-all-rel-scores :max-score-list '(6) :reverse-rel-scores-p T)
;; (print-all-rel-scores :max-score-list '(6) )
;; (print-all-rel-scores :reverse-rel-scores-p T)
;;works =            



;; (print-all-rel-scores)
;; works =              
#| "               
 X.  1___(0);  2___(0.14);  3___(0.21);  4___(0.29);  5___(0.36);  6___(0.43);  7___(0.5);  8___(0.57);  9___(0.64);  10___(0.71);  11___(0.79);  12___(0.86);  13___(0.93);  14___(1.0);              
 X.  1___(0);  2___(0.15);  3___(0.23);  4___(0.31);  5___(0.38);  6___(0.46);  7___(0.54);  8___(0.62);  9___(0.69);  10___(0.77);  11___(0.85);  12___(0.92);  13___(1.0);             
 X.  1___(0);  2___(0.17);  3___(0.25);  4___(0.33);  5___(0.42);  6___(0.5);  7___(0.58);  8___(0.67);  9___(0.75);  10___(0.83);  11___(0.92);  12___(1.0);            
 X.  1___(0);  2___(0.18);  3___(0.27);  4___(0.36);  5___(0.45);  6___(0.55);  7___(0.64);  8___(0.73);  9___(0.82);  10___(0.91);  11___(1.0);           
 X.  1___(0);  2___(0.2);  3___(0.3);  4___(0.4);  5___(0.5);  6___(0.6);  7___(0.7);  8___(0.8);  9___(0.9);  10___(1.0);          
 X.  1___(0.11);  2___(0.22);  3___(0.33);  4___(0.44);  5___(0.56);  6___(0.67);  7___(0.78);  8___(0.89);  9___(1.0);         
 X.  1___(0.12);  2___(0.25);  3___(0.38);  4___(0.5);  5___(0.62);  6___(0.75);  7___(0.88);  8___(1.0);        
 X.  1___(0.14);  2___(0.29);  3___(0.43);  4___(0.57);  5___(0.71);  6___(0.86);  7___(1.0);       
 X.  1___(0.17);  2___(0.33);  3___(0.5);  4___(0.67);  5___(0.83);  6___(1.0);      
 X.  1___(0.2);  2___(0.4);  3___(0.6);  4___(0.8);  5___(1.0);     
 X.  1___(0.25);  2___(0.5);  3___(0.75);  4___(1.0);    
 X.  1___(0.33);  2___(0.67);  3___(1.0);"
|#


;; (print-all-rel-scores :reverse-nums&scores-p T) 
;; works= "               
#| "               
 X.  14___(1.0);  13___(0.93);  12___(0.86);  11___(0.79);  10___(0.71);  9___(0.64);  8___(0.57);  7___(0.5);  6___(0.43);  5___(0.36);  4___(0.29);  3___(0.21);  2___(0.14);  1___(0);              
 X.  13___(1.0);  12___(0.92);  11___(0.85);  10___(0.77);  9___(0.69);  8___(0.62);  7___(0.54);  6___(0.46);  5___(0.38);  4___(0.31);  3___(0.23);  2___(0.15);  1___(0);             
 X.  12___(1.0);  11___(0.92);  10___(0.83);  9___(0.75);  8___(0.67);  7___(0.58);  6___(0.5);  5___(0.42);  4___(0.33);  3___(0.25);  2___(0.17);  1___(0);            
 X.  11___(1.0);  10___(0.91);  9___(0.82);  8___(0.73);  7___(0.64);  6___(0.55);  5___(0.45);  4___(0.36);  3___(0.27);  2___(0.18);  1___(0);           
 X.  10___(1.0);  9___(0.9);  8___(0.8);  7___(0.7);  6___(0.6);  5___(0.5);  4___(0.4);  3___(0.3);  2___(0.2);  1___(0);          
 X.  9___(1.0);  8___(0.89);  7___(0.78);  6___(0.67);  5___(0.56);  4___(0.44);  3___(0.33);  2___(0.22);  1___(0.11);         
 X.  8___(1.0);  7___(0.88);  6___(0.75);  5___(0.62);  4___(0.5);  3___(0.38);  2___(0.25);  1___(0.12);        
 X.  7___(1.0);  6___(0.86);  5___(0.71);  4___(0.57);  3___(0.43);  2___(0.29);  1___(0.14);       
 X.  6___(1.0);  5___(0.83);  4___(0.67);  3___(0.5);  2___(0.33);  1___(0.17);      
 X.  5___(1.0);  4___(0.8);  3___(0.6);  2___(0.4);  1___(0.2);     
 X.  4___(1.0);  3___(0.75);  2___(0.5);  1___(0.25);    
 X.  3___(1.0);  2___(0.67);  1___(0.33);"
(((14 14 1.0) (14 13 0.93) (14 12 0.86) (14 11 0.79) (14 10 0.71) (14 9 0.64) (14 8 0.57) (14 7 0.5) (14 6 0.43) (14 5 0.36) (14 4 0.29) (14 3 0.21) (14 2 0.14) (14 1 0)) ((13 13 1.0) (13 12 0.92) (13 11 0.85) (13 10 0.77) (13 9 0.69) (13 8 0.62) (13 7 0.54) (13 6 0.46) (13 5 0.38) (13 4 0.31) (13 3 0.23) (13 2 0.15) (13 1 0)) ((12 12 1.0) (12 11 0.92) (12 10 0.83) (12 9 0.75) (12 8 0.67) (12 7 0.58) (12 6 0.5) (12 5 0.42) (12 4 0.33) (12 3 0.25) (12 2 0.17) (12 1 0)) ((11 11 1.0) (11 10 0.91) (11 9 0.82) (11 8 0.73) (11 7 0.64) (11 6 0.55) (11 5 0.45) (11 4 0.36) (11 3 0.27) (11 2 0.18) (11 1 0)) ((10 10 1.0) (10 9 0.9) (10 8 0.8) (10 7 0.7) (10 6 0.6) (10 5 0.5) (10 4 0.4) (10 3 0.3) (10 2 0.2) (10 1 0)) ((9 9 1.0) (9 8 0.89) (9 7 0.78) (9 6 0.67) (9 5 0.56) (9 4 0.44) (9 3 0.33) (9 2 0.22) (9 1 0.11)) ((8 8 1.0) (8 7 0.88) (8 6 0.75) (8 5 0.62) (8 4 0.5) (8 3 0.38) (8 2 0.25) (8 1 0.12)) ((7 7 1.0) (7 6 0.86) (7 5 0.71) (7 4 0.57) (7 3 0.43) (7 2 0.29) (7 1 0.14)) ((6 6 1.0) (6 5 0.83) (6 4 0.67) (6 3 0.5) (6 2 0.33) (6 1 0.17)) ((5 5 1.0) (5 4 0.8) (5 3 0.6) (5 2 0.4) (5 1 0.2)) ((4 4 1.0) (4 3 0.75) (4 2 0.5) (4 1 0.25)) ((3 3 1.0) (3 2 0.67) (3 1 0.33)))
|#




;;FIND-MATCHED-QUESTIONS
;;2019
;;ddd
(defun find-matched-questions (search-item &key reverse-scored-p
                                           (all-qvars-list *SHAQ-question-variable-lists)
                                                 (question-list  *all-SHAQ-questions)
                                            only-search-cat return-all-quests-p)
  "In U-misc-print. Can find reverse-scored, all quests in a cat, or all quests. OR modify to find whatever matchees in qvar list. RETURNS (values all-matched-quests all-matched-qvars all-matched-quests-by-cat cat-matched-qvars  all-cats). NOTE: SEARCH-ITEM can be String to search strings for OR symbol."
  (let      ((all-matched-quests-by-cat)
       (all-matched-quests)
       (all-matched-qvars)
       (cat-matched-qvars)
       (all-cats)
       )
    (when reverse-scored-p
      (setf match-string "reverse"))
    ;;LOOP THRU CATS
    (loop
     for catlist in all-qvars-list
     do
     (let*
         ((cat-quests)
          (cat (car catlist))
          (qvarlists (cdr catlist))
          (n-catqvars (list-length qvarlists))
          )
       (setf all-cats (append all-cats (list cat)))
       ;;LOOP THRU QVARLISTS
       (loop
        for qvarlist in qvarlists
        for n from 1 to n-catqvars
        do
        ;;(afout 'out (format nil "qvarlist= ~A" qvarlist))
       (cond
        (only-search-cat
         (cond
          ;;search only matched cat?
          ((my-equal only-search-cat cat)
           (setf search-catp T))
          (t (setf search-catp NIL))))
         ;;PROCESS EACH QVARLIST
         ((listp qvarlist)
          ;;(break "1")
          (let
              ((qvar (car qvarlist))
               (foundp)
               (quest-text)
               (quest-text-list)
               (quest)
               (cat-qvars)
               )
            ;;(afout 'out (format nil "qvar= ~A" qvar))
            (cond
             ;;when return-all-quests-p
             (return-all-quests-p
              (multiple-value-setq (quest quest-text-list)
                  (get-question-text qvar T :all-questions-list *all-shaq-questions
                                     :eval-text-input-box-sym-p NIL))
              (when (listp quest-text-list)
                (setf quest-text (car quest-text-list)))
              (setf all-matched-quests (append all-matched-quests (list quest-text))
                    cat-quests (append cat-quests (list quest))))
             (t
              ;;LOOP THRU QVARLIST FOR SEARCH-ITEM
              (loop
               for item in qvarlist
               do
               ;;(afout 'out (format nil "item= ~A" item))
               (cond
                ;;search nested lists too
                ((listp item)
                 (loop 
                  for subitem in item
                  do
                  (when (or (and (symbolp search-item)(symbolp subitem)
                                 (my-equal search-item subitem))
                            (and (stringp search-item)(stringp subitem)
                                 (match-substring search-item subitem)))
                    (setf foundp T))))
                ;;if item not a list
                ((and (symbolp search-item)
                      (my-equal search-item item))
                 (setf  foundp T))
                ((and (stringp search-item)(stringp item)
                      (match-substring search-item item))
                 (setf foundp T))
                (t nil))
               ;;IF FOUNDP, APPEND
               (when foundp
                 (multiple-value-setq (quest quest-text-list)
                     (get-question-text qvar T :all-questions-list *all-shaq-questions
                                        :eval-text-input-box-sym-p NIL))
                 (when (listp quest-text-list)
                   (setf quest-text (car quest-text-list)))
                 (setf all-matched-quests (append all-matched-quests (list quest-text))
                       all-matched-qvars (append all-matched-qvars (list qvar))
                       cat-quests (append cat-quests (list quest-text))
                       cat-qvars (append cat-qvars (list qvar)))
                ;;(afout 'out (format nil "FOUNDP qvarlist= ~A  qvar= ~A%quest-text= ~A"  qvarlist qvar quest-text))
                 ;;(break "foundp")   
                 (return)
                 ;;end foundp
                 )
               ;;(afout 'out (format nil "FOUND item= ~A" item))
               ;;end qvarlist loop
               )
              ;;append 
              (when cat-qvars
                (setf all-matched-quests-by-cat 
                      (append all-matched-quests-by-cat (list (list cat cat-quests)))
                      cat-matched-qvars (append cat-matched-qvars (list (list cat cat-qvars)))))
              ;;end t=listp qvarlist-not just process one cat, cond
              ))
            ;;end  cat item is a qvarlist,cond
            )))
       ;;end qvarlists loop
         )
       ;;(break "END OUTER LOOP")
       ;;end let, outer cats loop
       ))
    (values all-matched-quests all-matched-qvars all-matched-quests-by-cat cat-matched-qvars  all-cats)
    ;;end let, find-matched-questions
    ))
;;TEST
;; (find-matched-questions "reverse" )
;; (find-matched-questions "reverse"  :all-qvars-list **test-qvars)
 #|(setf **test-qvars '((test-cat ( "stulikei"
            "sa-Like instructors-can talk"
            "spss-match"
            ("stuLikeInstr")
            ("stuLikeInstr" "22" "stuLikeInstrQ" "int" "LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "StudentBasicData.java")
            (:help *CONNECT-COL  na)
            )
           ( "stuextmo"
            "sa-NotParents expectations main motive"
            "spss-match"
            ("stuExtMotiv")
            ("stuExtMotiv" "25" "stuExtMotivQ" "int" "LikeMe7Reverse" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "StudentBasicData.java")
            (:help nil nil)
            )
           ( "stucomfo"
            "sa-Comfortable w/ area fac & students"
            "spss-match"
            ("stuComfortable" )
            ("stuComfortable" "23" "stuComfortableQ" "int" "LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "StudentBasicData.java")
            (:help nil nil)
            )
           ( "stufrien"
            "sa-Current school friends"
            "spss-match"
            ("stuFriends")
            ("stuFriends" "27" "stuFriendsQ" "int" "LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "StudentBasicData.java")
            (:help nil nil)
            )
           ( "stuenjoy"
            "sa-Enjoy learning, classes, homework"
            "spss-match"
            ("stuEnjoyLearn")
            ("stuEnjoyLearn" "21" "stuEnjoyLearnQ" "int" "LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "StudentBasicData.java")
            (:help nil nil)
            ))))|#



;;PRINT-FILE-LINES
;;2019
;;ddd
(defun print-file-lines (pathname &key (max-lines 5000))
  " "
  (let
      ((all-text)
       (n-lines)
       (line)
       )
    (with-open-file  (instr pathname :direction :input )
      (loop
       for n from 1 to max-lines
       do
       (cond
        ((equal line 'eof )(return))
        (t
       (setf n-lines n       
         line (read-line instr nil 'eof)
       all-text (format nil "~A~A" all-text line))
         ))))
    (values all-text n-lines)
    ;;end let,print-file-lines
    ))
;;TEST
;; ( print-file-lines "C:/3-TS/LISP PROJECTS TS/SHAQ-print-version/Q-quests-by-scale-subscales.lisp")
     
#|original (DEFPARAMETER *HQ-SCALE-BETA-LIST
          `((sT1HigherSelf  0.380 )
          ( sT2SocIntimNoFam 0.256)
          (sT3FamCare 0.406 )
          (sT4SuccessStatusMater 0.295 )
          (sT5-OrderPerfectionGoodness 0.310 )
          (sT6GodSpiritRelig 0.302 )
          (sT7ImpactChallengeExplor 0.347 )
          (sT8AttentionFunEasy 0.311 )
          (sT9ValueSelfAllUncond 0.461 )
          (sT10OvercmProbAcceptSelf 0.185 )
          (sT11DutyPunctual 0.265 )
          #| ;;redundant bec of subscales
       (sworldview  0. )
       (stbslfwo  0. )
       (siecontr  0. )
       (sethbel  0. ) ;;new label
       (sgrfears  0. )
       (sslfconf  0. )
       (sselfman  0. )
       (semotcop  0. )|#
          (IntSS1aAssertCR  0.463 )
          (IntSS1bOpenHon  0.503 )
          (IntSS2Romantc   0.394  )
          (IntSS3LibRole  0.166 )
          (IntSS4LoveRes  0.476 )
          (IntSS5Indep  0.379 )
          (IntSS6PosSup  0.421)
          (IntSS7Collab  0.406 )
          ;;subscales only
          (sswvgratpt  0.722 )
          (sswvoptims  0.543 )
          (sswventit  0.157 )
          (ssswNonCont  0.364 )
          (ssswHapAllGrat  0.567 )
          (ssswAcAllSelf  0.160 )
          (ssieautony  0.553 )
          (ssiencodep  0.213 )
          (ssienother  0.246 )
          (ssb2Ethic  0.346 )
          (ssb2Forgiv  0.282 )
          (ssb2IDgrnd  0.258 )
          (ssb2GrndMng  0.256 )
          (ssb2InrGood  0.327 )
          ;; (ssb2noAstr  0.087 )
          ;; (ssb2lifad  0.263 )
          (sswfsocial  0.492 )
          (sswfself  0.574 )
          (sswfpovfai  0.380 )
          (sswfilldea  0.221 )
          (sssclearn  0.408 )
          (sssccopopt  0.740 )
          (ssscsmsmsd  0.668 )
          (ssscinterp  0.577)
          (ssscallhelp  0.480 )
          (ssscscience  0.318 )
          (ssscartcre  0.349 )
          (sssmTimeManGoalSet 0.411 )
          (sssmAccompLoRush 0.553 )
          (sssmSelfDevel 0.558 )
          (sssmHealthHabs 0.460 )
          (sscpProbSolv  0.491 )
          (sscpPosThoughts  0.577 )
          (sscpPosActs  0.362 )
          (sscpNoBlameAngerWDraw  0.492 )
          (sscpNotSmokDrugMed  0.192 )
          (sscpNotEat  0.200 ))
          "List of all HQ component scales/subscales with beta weights lists."
          )|#