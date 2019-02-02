;;********************** test-read-java-file.lisp ************************
;;

;;IDEAS
;; 1- USE FORMAT CONDITIONAL TO MAKE \N = #\NEWLINE 
;; 2-USE + TO SIGNAL CONCAT, MERGE OR WHATEVER WORKS
;; 3- MAKE ; INTO A NEWLINE OR IGNORE??
;; 4- EARLY  * OR // TO IGNORE LINE
;; 5-  X = Y   TO   (SETQ X Y) -- see below
;; 6-  public static [String/int/etc] variable = value;   TO  (defparameter varible value)
;; 7- USE PCategory and question processing functions for those specific instances     

;;ACTUAL OUTPUT FROM MY READ-FILE-LINES FUNCTION
;; WITH ADD-NEWLINE-P =  T
(defparameter *test-java-NL
`(";;********************** test-read-java-file.lisp ************************
" ";;
" "
" ";;IDEAS
" ";; 1- USE FORMAT CONDITIONAL TO MAKE \\N = #\\NEWLINE 
" ";; 2-USE + TO SIGNAL CONCAT, MERGE OR WHATEVER WORKS
" ";; 3- MAKE ; INTO A NEWLINE OR IGNORE??
" ";; 4- EARLY  * OR // TO IGNORE LINE
" ";; 5-  X = Y   TO   (SETQ X Y) -- see below
" ";; 6-  public static [String/int/etc] variable = value;   TO  (defparameter varible value)
" ";; 7- USE PCategory and question processing functions for those specific instances     
" "
" "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+
" "   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\\n\\n\"+
" "   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";
" "   public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";
" "
" "
" "
" "
" "
" "
" "
" "
" "    //SRQ--INTIMACY
" "   public static final String intSrq6ExtraQ =
" "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+
" "   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\\n\\n\"+
" "   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";
" "   public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";
" "     //same as crSrq30RealQ
" "   public static final String intSrq30RealQ = \"We usually discuss what is really bothering us (the underlying issues) instead of the surface issues.\";
" "   public static final String intSrq8TellAllQ = \"I have told my partner almost everything about myself.\";
" "
" "
" "
" "    PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);
" "    questionInstancesArray[0] = intSrq6Extra;
" "    PCategory intSrq7Commit = new PCategory(\"intSrq7Commit\",2, intSrq7CommitQ, \"int\",  FrAnswerPanel.LikeUs7Reverse,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);
" "    questionInstancesArray[1] = intSrq7Commit;
"))

;;WITH ADD-NEWLINE-P  NIL 
(defparameter *test-java
  '(";;********************** test-read-java-file.lisp ************************" ";;" "" ";;IDEAS" ";; 1- USE FORMAT CONDITIONAL TO MAKE \\N = #\\NEWLINE " ";; 2-USE + TO SIGNAL CONCAT, MERGE OR WHATEVER WORKS" ";; 3- MAKE ; INTO A NEWLINE OR IGNORE??" ";; 4- EARLY  * OR // TO IGNORE LINE" ";; 5-  X = Y   TO   (SETQ X Y) -- see below" ";; 6-  public static [String/int/etc] variable = value;   TO  (defparameter varible value)" ";; 7- USE PCategory and question processing functions for those specific instances     " "" "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+" "   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\\n\\n\"+" "   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";" "   public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";" "" "" "" "" "" "" "" "" "    //SRQ--INTIMACY" "   public static final String intSrq6ExtraQ =" "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+" "   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\\n\\n\"+" "   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";" "   public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";" "     //same as crSrq30RealQ" "   public static final String intSrq30RealQ = \"We usually discuss what is really bothering us (the underlying issues) instead of the surface issues.\";" "   public static final String intSrq8TellAllQ = \"I have told my partner almost everything about myself.\";" "" "" "" "    PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" "    questionInstancesArray[0] = intSrq6Extra;" "    PCategory intSrq7Commit = new PCategory(\"intSrq7Commit\",2, intSrq7CommitQ, \"int\",  FrAnswerPanel.LikeUs7Reverse,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" "    questionInstancesArray[1] = intSrq7Commit;"))












#\
   "INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n"+
   "  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n"+
   "Question 1.If I am under more stress than usual, my partner will usually do extra things for me.";
   public static final String intSrq7CommitQ = "A long term commitment (would) cause(s) me to feel trapped.";








    //SRQ--INTIMACY
   public static final String intSrq6ExtraQ =
   "INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n"+
   "  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n"+
   "Question 1.If I am under more stress than usual, my partner will usually do extra things for me.";
   public static final String intSrq7CommitQ = "A long term commitment (would) cause(s) me to feel trapped.";
     //same as crSrq30RealQ
   public static final String intSrq30RealQ = "We usually discuss what is really bothering us (the underlying issues) instead of the surface issues.";
   public static final String intSrq8TellAllQ = "I have told my partner almost everything about myself.";



    PCategory intSrq6Extra = new PCategory("intSrq6Extra",1, intSrq6ExtraQ, "int",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);
    questionInstancesArray[0] = intSrq6Extra;
    PCategory intSrq7Commit = new PCategory("intSrq7Commit",2, intSrq7CommitQ, "int",  FrAnswerPanel.LikeUs7Reverse,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);
    questionInstancesArray[1] = intSrq7Commit;
