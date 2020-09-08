;;********************* U-trees-art-dims *******************************
;;see U-trees for more general tree viewer


;;== ARTSYM TREES & OTHER  DIMLIST TREES-----------------------------
;;NOTATION
;;
;; In U-TREES, ROOTN = DIM, ROOTNLIST = DIMLIST, ROOTNLISTS = DIMLISTS


#|Copy, modify make nodes list.
For each node not an int or value,
Instead, keys in any order.Can be no keys. Note, puttng happy CSYM is redundant, since CS-HS-1 evals to list w CSYM. Also value redundant.
For viewing tree, the tree viewer could eval ARTSYM and fill in CSYM and value (also keeps non redundant DB).
Could write a function to fill in csyms and values w csyms from eval artsyms. 
(NodeID :v Value :s sublist :c csym) 
Eg.  (CS :s (HS :s (1 :c happy :v .917 :s (2 :c integrity))(2 :c honest)))
MAKES=> 
((CS) CS :S ((CS HS) CS-HS :S ((CS HS 1) CS-HS-1 :s happy :v .917 :S ((CS HS 1 2)CS-HS-1-2 :s integrity)) ((CS HS 2) CS-HS-2 :s honest)))|#

;;make-treeviewer-options-interface first
(defparameter  *make-treeviewer-options-interface T)

;;dim-treeviewer frame options
(defparameter *dim-treeviewer-frame-title "Tom's Cognitive Systems TreeViewer")
(defparameter  *incl-csymlist-in-treeviewer-p NIL "Includes csymvals in each node printout")
(defparameter *dim-treeviewer-frame-ht 800 )
(defparameter *dim-treeviewer-frame-width (cond (*incl-csymlist-in-treeviewer-p 1300) (t 800)))
(defparameter *dim-treeviewer-frame-x 10 )
(defparameter *dim-treeviewer-frame-y 30)
(defparameter *dim-tree-list NIL "Main dim-tree-list incls all sublists/subtrees. Includes *tree-dimroots")
(defparameter *tree-dimroots  '(($CS)( $VER) ($IMG)( $SND$)( $SML)( $TST)($EMT)( $MOT)) "The tree root(s) above the subtrees") ;;was '(1 2 3 4 5)) 
(defparameter  *tree-dim-parent-list NIL "Child complete lists/subtrees of a given ROOT including other keys & values at that level.")
(defparameter  *tree-child-sublists NIL "The child subtreekey value= list/subtree w/dims=ROOT.")

;;parameters for adjusting dims-treeviewer node info
(defparameter  *print-all-node-info-p NIL "Prints all node info for each node in frame")
(defparameter *omit-sublists-in-node-info T "Omits printing sublist info in node info")
(defparameter *expanded-node-list-p NIL "Leaves only minimal info in each node")
(defparameter *new-dim-sublist-value "See +" "Replaces sublist in dimlist node info")
(defparameter *tree-dim-parent-list-w-sublists nil "Temp reset of dim sublists including actual sublists")

(defparameter *treeview-flat-tree-list NIL)
(defparameter *child-root-value NIL"keyvalue value")
(defparameter *child-dimlists NIL)

(defparameter *tree-root-dimlists  NIL)
(defparameter *tree-flat-dimlists NIL)
(defparameter *tree-dimroot-leveln-list NIL)
(defparameter *totaln-nodes  NIL)
(defparameter  *tree-dimroot-sublists NIL )  
(defparameter *treeview-expand-level  99 "Default= 99, expand up to that level")
(defparameter *my-dim-tree NIL)
(defparameter *my-dims-expand-function   'my-dims-expandp-function )


;;XXX FOR TESTING
#| (setf *tree-dimroots '((CS))
  *dim-tree-list
 '(((CS)   "CS"  :S
  (((CS HS)    "CS.HS"    :S
    (((CS HS 1)      "CS.HS.1"  :C CAREFOROTHERS     :S
      (((CS HS 1 1) "CS.HS.1.1" :C INTIMATE) ((CS HS 1 2) "CS.HS.1.2" :C UNDERSTANDING)))
     ((CS HS 2)      "CS.HS.2"  :C INTIMATE    :S
      (((CS HS 2 1) "CS.HS.2.1" :C FLEXIBLE )
       ((CS HS 2 2) "CS.HS.2.2" :C EXUBERANT)
       ((CS HS 2 3) "CS.HS.2.3" :C LOVEDANCE)))
     ((CS HS 3) "CS.HS.3")))
   ((CS MS)    "CS.MS"    :S
    (((CS MS MS1)      "CS.MS.MS1"      :S
      (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))))|#


(defparameter *treeviewer-options-interface-title " Cognitive Systems TreeViewer Options")
(defparameter *treeviewer-options-title " Cognitive Systems TreeViewer Options")
(defparameter  *treeviewer-options-instrs "Check the item below for viewing the Cognitive Systems Tree")


;;XXX ============ MAKING CSART (EMPTY) TREES =================

;;MAKE-CSARTDIMS-TREE
;;2019    ;;*CSARTSYS-TREE-SPEC-LIST moved to CS-config
;;ddd
(defun make-csartdims-tree (csartroot csartspecs &key csartdims-tree
                                      ;;(cs-catlist *CS-categories) 
                                      (sublistkey :S)  put-sym-first-p
                                      (separator ".") delete-chars (set-sym-to-vals-p T))
  "U-trees-art-dims Makes an artdims treelist (viewable from dims-treeviewer) from a categories (eg CS HS) and a list of CSYMS using csym values & ranks, cs-category, and abstraction number (currently manually assigned?).   RETURNS csartdims-tree  
  INPUT: eg ''(($CS (($TBV ($WV (($SLF) ($REFG) ($FAM)))) ($KNW etc"
  (let*
      ((rootstr (when csartroot  (format nil "~A" csartroot)))
       (n-items (list-length csartspecs))  ;;was n-speclists
       (new-sublist)
       )
    ;;(afout 'out (format nil "BEGIN: rootstr= ~A" rootstr))
    (loop
     for item in csartspecs
     for item-n from 1 to n-items
     do
     (let*
         ((subrootstr)
          )
    ;;(afout 'out (format nil "BEGIN LOOP: item= ~A" item))  
       (cond
        ;;item is a list--therefore another level of sublists, RECURSE,
        ;;then append (:S ((   )(  ))) to new-sublist
        ;; if item-n=1, append to ((CS 1) CS.1) =>new-sublist= ((CS 1) CS.1 :S (( )( )) )
        ;; otherwise append to last (list) in new-sublist 
        ((and item (listp item))
         ;;(break "before mvb, rootstr")
         (multiple-value-bind (subtree sublevel-list)
             (make-csartdims-tree rootstr  item  ;; :cs-catlist cs-catlist 
                                  :put-sym-first-p put-sym-first-p )
           ;;(afout 'out (format NIL "AFTER RECURSE: subtree= ~A" subtree))          
           (when  subtree
             (cond   
              ;;appends the sublist after :S
              ((and new-sublist (listp (car (last new-sublist))))
               ;;(afout 'out (format NIL "BEFORE BUTLASTetc: new-sublist=~A~%" new-sublist ))          
               (setf new-sublist (append (butlast new-sublist)
                                           (append (last new-sublist) (list  subtree))))
               ;;(afout 'out (format NIL "AFTER BUTLASTetc: new-sublist=~A~%subtree= ~A" new-sublist subtree))
               )
             ((or (= item-n 1) (null new-sublist))
               (setf new-sublist   (list subtree))
               ;;(afout 'out (format NIL "WHEN item-n= 1:  new-sublist= ~a" new-sublist))
               )
              (new-sublist 
               (setf new-sublist  (append new-sublist (list sublistkey subtree)))
                          ;;(afout 'out (format NIL "WHEN new-sublist:  new-sublist= ~a" new-sublist))
                          )
             (t (setf new-sublist (list sublistkey (list subtree))) ;;added list here55
                ;;(afout 'out (format NIL "WHEN T:  new-sublist= ~a" new-sublist))
                ))
             ;;end when subtree
             )
           ;;(afout 'out (format NIL "LISTP ITEM= ~a~% new-sublist= ~a" item new-sublist))    
           ;;end mvb, listp
           ))
        ;;first item of new-sublist is either csartsym or symdims, create them 
         ((equal item-n 1)
          (multiple-value-bind (rootstr1 new-sym new-dims new-sublist1 
                                         new-artsymvals)
              (make-csartsym-simple-node rootstr item :separator separator 
                                         :set-sym-to-vals-p set-sym-to-vals-p)
           ;; (break "new-dims")
           (setf rootstr rootstr1)
           (cond
            (put-sym-first-p
             (setf new-sublist (list new-sym new-dims)))
            (t (setf new-sublist (list new-dims new-sym )))  )
            ;;(afout 'out (format NIL "when ITEM= ~a~% rootstr1= ~A new-sublist= ~a " item rootstr1 new-sublist))
            ;;here33
            ))
         ;;otherise, just add item to new-sublist
         (t (setf  new-sublist (append new-sublist (list item)))))
           ;;(afout 'out (format NIL "T  ITEM= ~a~% csartdims-tree= ~a" item csartdims-tree))
        ;;end let,inner loop
        ))
     ;;end let,outer loop
    ;; ))
       (cond
        (csartdims-tree
         (setf csartdims-tree (append csartdims-tree (list new-sublist))))
        (new-sublist 
         (setf csartdims-tree   new-sublist))
        (t nil))
    ;;(afout 'out (format NIL "AT END: csartdims-tree= ~A" csartdims-tree))        
      csartdims-tree
    ;;end let, make-csartdims-tree
    ))
;;TEST
;; (setf *CSARTSYS-TREE-SPEC-LIST  '(($CS (($TBV ($WV (($SLF) ($REFG) ($FAM)))) ($KNW (($NSC) ($BSC) ($SSC) ($ART) ($BUS) ($SPT) ($REC))) ($CSK (($LRN) ($RESN ($MTH) ($LOG)))) ($SM (($PS) ($DM) ($PLN) ($COP) ($HTH) ($FIN))) ($CAR) ($BSK (($PEOP) ($MECH) ($MAIN) ($ATH) ($MAN))) ($SCR ($ROLE)) ($EVT (($PER) ($HIST) ($HYP))) ($ELM (($PEO) ($OBJ) ($ANM))))) ($VER (($GRA) ($SEN) ($WRD))) ($IMG) ($SND$) ($SML) ($TST) ($EMT (($HAP) ($LOV) ($ANX) ($ANG) ($DEP))) ($MOT ($SPH))))
;; (make-csartdims-tree NIL  *CSARTSYS-TREE-SPEC-LIST :set-sym-to-vals-p nil)
;; WORKS 
#|((($CS)   $CS  :S
  ((($CS $TBV)    $CS.$TBV    :S
    (($CS $TBV $WV)     $CS.$TBV.$WV     :S
     ((($CS $TBV $WV $SLF) $CS.$TBV.$WV.$SLF)
      (($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG)
      (($CS $TBV $WV $FAM) $CS.$TBV.$WV.$FAM))))
   (($CS $KNW)    $CS.$KNW    :S
    ((($CS $KNW $NSC) $CS.$KNW.$NSC)
     (($CS $KNW $BSC) $CS.$KNW.$BSC)
     (($CS $KNW $SSC) $CS.$KNW.$SSC)
     (($CS $KNW $ART) $CS.$KNW.$ART)
     (($CS $KNW $BUS) $CS.$KNW.$BUS)
     (($CS $KNW $SPT) $CS.$KNW.$SPT)
     (($CS $KNW $REC) $CS.$KNW.$REC)))
   (($CS $CSK)    $CS.$CSK    :S
    ((($CS $CSK $LRN) $CS.$CSK.$LRN)
     (($CS $CSK $RESN)      $CS.$CSK.$RESN      :S
      (($CS $CSK $RESN $MTH) $CS.$CSK.$RESN.$MTH)
      (($CS $CSK $RESN $LOG) $CS.$CSK.$RESN.$LOG))))
   (($CS $SM)    $CS.$SM    :S
    ((($CS $SM $PS) $CS.$SM.$PS)
     (($CS $SM $DM) $CS.$SM.$DM)
     (($CS $SM $PLN) $CS.$SM.$PLN)
     (($CS $SM $COP) $CS.$SM.$COP)
     (($CS $SM $HTH) $CS.$SM.$HTH)
     (($CS $SM $FIN) $CS.$SM.$FIN)))
   (($CS $CAR) $CS.$CAR)   
   (($CS $BSK)  $CS.$BSK     :S
    ((($CS $BSK $PEOP) $CS.$BSK.$PEOP)
     (($CS $BSK $MECH) $CS.$BSK.$MECH)
     (($CS $BSK $MAIN) $CS.$BSK.$MAIN)
     (($CS $BSK $ATH) $CS.$BSK.$ATH)
     (($CS $BSK $MAN) $CS.$BSK.$MAN)))
   (($CS $SCR) $CS.$SCR :S (($CS $SCR $ROLE) $CS.$SCR.$ROLE))
   (($CS $EVT)    $CS.$EVT    :S
    ((($CS $EVT $PER) $CS.$EVT.$PER)
     (($CS $EVT $HIST) $CS.$EVT.$HIST)
     (($CS $EVT $HYP) $CS.$EVT.$HYP)))
   (($CS $ELM)    $CS.$ELM    :S
    ((($CS $ELM $PEO) $CS.$ELM.$PEO)
     (($CS $ELM $OBJ) $CS.$ELM.$OBJ)
     (($CS $ELM $ANM) $CS.$ELM.$ANM)))))
 (($VER)   $VER  :S
  ((($VER $GRA) $VER.$GRA) (($VER $SEN) $VER.$SEN) (($VER $WRD) $VER.$WRD)))
 (($IMG) $IMG)
 (($SND$) $SND$)
 (($SML) $SML)
 (($TST) $TST)
 (($EMT)  $EMT  :S
  ((($EMT $HAP) $EMT.$HAP)
   (($EMT $LOV) $EMT.$LOV)
   (($EMT $ANX) $EMT.$ANX)
   (($EMT $ANG) $EMT.$ANG)
   (($EMT $DEP) $EMT.$DEP)))
 (($MOT) $MOT :S (($MOT $SPH) $MOT.$SPH)))
|#


;;OLDER
;; (defparameter *CS-CATEGORIES '((CS (BV (TOPV (HS () ))(TOPB (WV ()  )))  (ROLES ( ))  (LBS (SM ()) (COPE ()) (LRN ()) (INTR ()) (HLTH ()) (CAR ()) (MANU ()) (ATHL ())))(EMOT (HAP)(CARE)(ANX)(DEP)(ANG))  (SENS (VIS)(AUD)(PROP))(MOT))     "WV= worldview, LBS=LifeBeliefsSkills, SM COPE LRN INTR=Interpers, HLTH CAR MANU ATHL")
;; (make-csartdims-tree NIL *CS-categories :set-sym-to-vals-p nil)
;; works? =
#| |#
;; 
;; FOR SYM FIRST
;; (make-csartdims-tree NIL *CS-categories :set-sym-to-vals-p nil :put-sym-first-p T)
;; works= 


;;MAKE-LEVEL-CSARTSYMS
;;2019
;;ddd
(defun make-level-csartsyms (symroot min-n max-n
                               &key CSCATS value rank ABSTN ncon leveln (separator ".")
                              (csarttree *dim-tree-list)(level-range '(1 100))(set-sym-to-vals-p T)
                               (cs-catlist *CS-categories) (abstn-range '(1 10)) )
  "U-trees-art-dims For a new tree level (at subtree bottom) makes new artsyms and artdims with artsyms set to artsymvals.  INPUT: symroot eg (CS-HS-2 is the symbol a new set of dims at next level is added to eg. CS-HS-2-1 ... CS-HS-2-5.
     RETURNS (values  csartsyms tree-level-node-list csartsymstr csartdims leveln)"
  (let*
      ((symrootstr (cond ((stringp symroot) symroot)
                         (t (format nil "~A" symroot))))
       (csartsyms)
       (tree-level-node-list)
       (csartsymstrs)
       (csartdims)
       (n-new-nodes (+ (- max-n min-n) 1))
       )
    (loop
     for n from min-n to max-n
     do
     (multiple-value-bind (new-symstr new-sym new-dims new-node-list new-artsymvals)
         (make-csartsym-simple-node symrootstr  n :separator separator
                                    :set-sym-to-vals-p set-sym-to-vals-p)
       ;;append lists
       (setf tree-level-node-list (append tree-level-node-list (list new-node-list))
             csartsyms (append csartsyms (list new-sym))
             csartsymstrs (append csartsymstrs  (list new-symstr))
             csartdims (append csartdims (list new-dims)))
       ;;end mvb,loop
       ))
       ;;end let, make-level-csartsyms
      (values  csartsyms csartsymstrs csartdims n-new-nodes)
  ))
;;TEST
;; (make-level-csartsyms 'CS.HS.1 1 3)
;; (CS.HS.1.1 CS.HS.1.2 CS.HS.1.3)   ("CS.HS.1.1" "CS.HS.1.2" "CS.HS.1.3")   ((CS HS 1 1) (CS HS 1 2) (CS HS 1 3))  3
;; ALSO:  CS.HS.1.2 = > (CS.HS.1.2 (CS HS 1 2))





;;MAKE-CSARTSYM-SIMPLE-NODE
;;2019
;;ddd
(defun make-csartsym-simple-node (csart-rootstr new-dim &key (separator ".")
                                                (set-sym-to-vals-p T))
  "U-trees-art-dims RETURNS: (values new-symstr new-sym new-dims new-node-list new-artsymvals)"
  (let*
      ((new-symstr (cond
                    (csart-rootstr (format nil "~A~A~A" csart-rootstr  separator new-dim))
                    (t (format nil "~A"  new-dim))))
       (new-sym (my-make-symbol new-symstr  :delete-chars nil))
       (new-dims (find-artsym-dims new-sym))
       (new-node-list (list new-dims new-sym))
       (new-artsymvals (list new-sym new-dims))
       )
    ;;(break "new")
    (when set-sym-to-vals-p
      (set new-sym new-artsymvals))
    (values new-symstr new-sym new-dims new-node-list new-artsymvals)
    ;;end let, make-csartsym-simple-node
    ))
;;TEST
;; (make-csartsym-simple-node "XX.1.2" "y")
;; works="XX.1.2.y"  XX.1.2.Y (XX 1 2 Y) ((XX 1 2 Y) XX.1.2.Y) (XX.1.2.Y (XX 1 2 Y))
;;also  XX.1.2.Y => (XX.1.2.Y (XX 1 2 Y))

;; (make-csartsym-simple-node nil "y")
;; works= "y"  Y (Y) ((Y) Y) (Y (Y))
;;also CL-USER 116 > Y = (Y (Y))







;;CALC-CSYM-ART-LEVEL
;;2019
;;ddd
(defun calc-csym-art-level (csym &key CSCATS value rank ABSTN ncon leveln
                                 ;;not used level-pts (csarttree *dim-tree-list)(cs-catlist *CS-categories)
                                 (value-abstn-leveln-ratio 1.5) 
                                 (level-range '(0 10))(value-range '(0 1.0))
                                  (abstn-range '(0 10)) use-rel-ranks-p
                                  (n-rank-intvs 20) (max-rank-val 20) ;;1.0 if use-rel-ranks-p
                                 (n-val-intervals 12))
  "U-trees-art-dims Makes an artdims (viewable from dims-treeviewer) from INPUT: categories (eg CS HS) and a list of CSYMS using csym values & ranks, cs-category, and abstraction? number= abstn [1-10, later digits?] (currently manually assigned?), num-connections= ncon, [later?rest-sort-vars].  When level-pts, overides calculated level. 
     RETURNS (values leveln level-pts). Note: n-val-intervals always= 12, n-rank-vals varies/unknown. 20 causes less pts deduct-so is conservative for unknown rank."
  (when use-rel-ranks-p
    (setf max-rank-val 1.0))
  (let*
      ((csymstr (when (stringp csym) csym))
       (csymvals (progn (when csymstr
                          (setf csym (my-make-symbol csym :delete-chars nil)))
                                (when (boundp csym)(eval csym))))                      
       (csartsymstr)
       (csartsym)
       (csartdims)
       (val-level)
       (val-width)
       (rank-level)
       (val-rank-level)
       (abstn-level)
       (return-int-n)
       (len-list)
       (val-rank-ratio)
       )
    (multiple-value-bind (val-intervals-list val-num-intervals val-interval-width)
        (calc-num-intervals-list (second level-range) 0 1.0)
      (multiple-value-bind (rank-intervals-list rank-num-intervals  
                                                rank-interval-width)
;;(multiple-value-bind (rank-intervals-list rank-num-intervals  rank-interval-width)  (calc-num-intervals-list  20  0  1.0) (values rank-intervals-list rank-num-intervals  rank-interval-width))
          (calc-num-intervals-list  n-rank-intvs  0  max-rank-val)
        (multiple-value-bind  (abstn-intervals-list abstn-num-intervals  
                                                    abstn-interval-width)
            (calc-num-intervals-list (second level-range) 0 100)
          (unless value
            (setf value (get-key-value :csval csymvals)))
          (unless rank
            (setf rank (get-key-value :csrank csymvals)))
          (when (and use-rel-ranks-p (> rank 1.0))
            (setf rank nil))
          ;;FORMULA FOR EACH LEVEL VARIABLE DIVIDING INTO RANK-GROUPS
          (unless leveln
            ;;FOR VALUES (and ranked values)
            (when value 
              (multiple-value-setq (val-level return-int-n len-list val-width)
                  (calc-intervaln-for-num value :intervals-list val-intervals-list))
              ;;calc val-rank-level? Note: values within each value interval are ranked WITHIN
              (cond
               (rank
                (setf rank-level (calc-intervaln-for-num rank :intervals-list rank-intervals-list)
                      ;;formula for combining values & ranks within value levels
                      val-rank-ratio (/  val-interval-width rank-interval-width )
                      val-rank-level (- val-level (* val-rank-ratio rank-level))))
               (t (setf val-rank-level val-level))))
            ;;(break "data")
            ;;FOR ABSTRACT LEVEL = ABSTN-LEVEL
            (when abstn
              (setf abstn-level (calc-intervaln-for-num abstn  :intervals-list abst-intervals-list)))
            ;;COMBINE THE VARIABLE RANKINGS
            ;;overall leveln
            (cond
             ((and val-rank-level abstn-level)
              (setf leveln (* value-abstn-leveln-ratio val-rank-level abstn-level)))
             (val-rank-level (setf leveln val-rank-level))
             (abstn-level (setf leveln abstn-level))
             (t nil))  
            (values leveln val-rank-level abstn-level val-level rank-level)
            ;;end let, 3mvbs, calc-csym-art-level
            ))))))
;;TEST
;; (calc-csym-art-level  'careforothers)
;; results= 9.6 9.6 NIL 10 4



;;xxx =================== VIEWING, USING CSART TREES ==============


;;TREEVIEWER-OPTIONS-INTERFACE
;;2019
;;ddd
(capi:define-interface treeviewer-options-interface ()
  (
   (calling-interface
    :initarg :calling-interface
    :accessor calling-interface
    :initform NIL
    :documentation  "calling-interface")
   )
  (:panes
   (title-rich-text-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :character-format (list ;; :face *title-pane-font-face
                            :size  *title-pane-font-size 
                            :color *title-pane-font-color
                            :bold T :italic nil :underline nil )
    :paragraph-format  (list :alignment :center  ;; :left :right
                             ;;no effect?  :start-indent 20
                             ;;no effect? :offset-indent 20
                             ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                             :tab-stops  '(5 10 15 20)
                             :numbering nil 
                             ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                             )
    :text *treeviewer-options-title
    :visible-border T
    ;;only adds at bottom when use format? 
    :internal-border 8
    :visible-min-height *title-pane-height  :visible-max-height *title-pane-height
    :external-min-width *title-pane-width  ;; :external-max-width *title-pane-width
 ;;    :foreground *title-pane-foreground 
    :background *title-pane-background
    ;;text done elsewhere  :text *title-area-text 
    ;;doesn't work :y 10
    )
   (instr-rich-text-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :character-format  (list ;; :face *instr-pane-font-face 
                             :size  *instr-pane-font-size  
                             :color *instr-pane-font-color
                             :bold nil :italic nil :underline nil )
    :paragraph-format '(:alignment :center  ;; :left :right
                        :start-indent 20
                        :offset-indent 20
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops  '(5 10 15 20)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :text *treeviewer-options-instrs
   ;;doesn't work :enabled nil
    :visible-border T
    :internal-border 8
    :visible-min-height *instr-pane-height :visible-max-height *instr-pane-height
    :external-min-width *instr-pane-width ;; :external-max-width *instr-pane-width
    ;;   :foreground *instr-pane-foreground 
    :background *instr-pane-background
    ;;text done elsewhere   :text  (format nil "~%   ~A  " *instr-area-text)
    )
   (rich-text-pane-1
    capi:rich-text-pane    :accepts-focus-p NIL
    :character-format (list  ;; :face *quest-pane-font-face
                             :size *quest-pane-font-size
                             :color *quest-pane-font-color
                             :bold *quest-pane-font-weight
                             :italic nil  :underline nil )
    :paragraph-format '(:alignment :left ;; :center  ;; :left :right
                        ;;  :start-indent 5
                       ;; :offset-indent 10
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops nil   ;;eg  (10 20 10)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :text "Print how much information at EVERY tree node."
    :visible-border NIL
    :accepts-focus-p NIL
    ;;:internal-border 20
    ;;  :visible-min-height *quest-pane-height
    :visible-max-height 40 ;; *quest-pane-height
    :visible-min-width *quest-pane-width ;; :visible-max-width *quest-pane-width
    :foreground *answer-pane-foreground 
    :background *answer-pane-background
    ;;   :text *quest-area-text 
    )
   (radio-button-panel-1
    capi:radio-button-panel
    :items '("Brief info" "Expanded info" "Include CS detailed info")
    :max-width t
    :max-height t)
   (rich-text-pane-2
    capi:rich-text-pane    :accepts-focus-p NIL
    :character-format (list  ;; :face *quest-pane-font-face
                             :size *quest-pane-font-size
                             :color *quest-pane-font-color
                             :bold *quest-pane-font-weight
                             :italic nil  :underline nil )
    :paragraph-format '(:alignment :left ;; :center  ;; :left :right
                        ;;  :start-indent 5
                       ;; :offset-indent 10
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops nil   ;;eg  (10 20 10)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :visible-border NIL
    :accepts-focus-p NIL
    ;;:internal-border 20
    ;;  :visible-min-height *quest-pane-height
    :visible-max-height 40 ;; *quest-pane-height
    :visible-min-width *quest-pane-width ;; :visible-max-width *quest-pane-width
    :foreground *answer-pane-foreground 
    :background *answer-pane-background
    ;;   :text *quest-area-text 
    )
   (radio-button-panel-2
    capi:radio-button-panel
    :items '("Radio-Button-Panel-2" "Button 2" "Button 3")
    :max-width t
    :max-height t)
   (rich-text-pane-3
    capi:rich-text-pane    :accepts-focus-p NIL
    :character-format (list  ;; :face *quest-pane-font-face
                             :size *quest-pane-font-size
                             :color *quest-pane-font-color
                             :bold *quest-pane-font-weight
                             :italic nil  :underline nil )
    :paragraph-format '(:alignment :left ;; :center  ;; :left :right
                        ;;  :start-indent 5
                       ;; :offset-indent 10
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops nil   ;;eg  (10 20 10)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :visible-border NIL
    :accepts-focus-p NIL
    ;;:internal-border 20
    ;;  :visible-min-height *quest-pane-height
    :visible-max-height 40 ;; *quest-pane-height
    :visible-min-width *quest-pane-width ;; :visible-max-width *quest-pane-width
    :foreground *answer-pane-foreground 
    :background *answer-pane-background
    ;;   :text *quest-area-text 
    )
   (radio-button-panel-3
    capi:radio-button-panel
    :items '("Radio-Button-Panel-3" "Button 2" "Button 3")
    :max-width t
    :max-height t)
   (rich-text-pane-4
    capi:rich-text-pane    :accepts-focus-p NIL
    :character-format (list  ;; :face *quest-pane-font-face
                             :size *quest-pane-font-size
                             :color *quest-pane-font-color
                             :bold *quest-pane-font-weight
                             :italic nil  :underline nil )
    :paragraph-format '(:alignment :left ;; :center  ;; :left :right
                        ;;  :start-indent 5
                       ;; :offset-indent 10
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops nil   ;;eg  (10 20 10)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :text "Select ALL that you want"
    :visible-border NIL
    :accepts-focus-p NIL
    ;;:internal-border 20
    ;;  :visible-min-height *quest-pane-height
    :visible-max-height 40 ;; *quest-pane-height
    :visible-min-width *quest-pane-width ;; :visible-max-width *quest-pane-width
    :foreground *answer-pane-foreground 
    :background *answer-pane-background
    ;;   :text *quest-area-text 
    )
   (check-button-panel-1
    capi:check-button-panel
    :items '("CHECK-Button-Panel-1" "Button 2" "Button 3")
    :max-width t
    :max-height t)
   (push-button-1
    capi:push-button
    :text "Push-Button-1")
   (go-frame-button
        capi:push-button
        :background *go-frame-button-background
        :foreground *go-frame-button-foreground
        :text  "GO to NEXT Frame >>"
        :visible-min-width  *go-frame-button-width
        :visible-max-width  *go-frame-button-width
        :visible-min-height  *go-frame-button-height
        :visible-max-height  *go-frame-button-height
        ;;  :external-max-height  *go-frame-button-height   
#|        :default-x *go-frame-button-x
        :default-y *go-frame-button-y|#
        :font  *go-frame-button-font 
        ;;   :color-requirements 
        ;;   :selected T
        :default-p T  ;;means if return hit, selects this button
        ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
        :callback 'treeviewer-go-callback
        :callback-type :data-interface
        :data *text-go-button-callback
        ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
        ;;doesn't work    :character-format *title-pane-char-format
        ;;doesn't work?    :x 20   :y 20
        ))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(title-rich-text-pane :separator instr-rich-text-pane :separator rich-text-pane-1 
     radio-button-panel-1 :separator
      rich-text-pane-2 radio-button-panel-2 :separator 
      rich-text-pane-3 radio-button-panel-3 :separator
      rich-text-pane-4 check-button-panel-1 :separator row-layout-1))
   (row-layout-1
    capi:row-layout
    '(push-button-1 go-frame-button)))
  (:menu-bar menu-1 menu-2)
  (:menus
   (menu-2
    "Menu-2"
    ("Item-3"
     "Item-4"
     (:component
      ())))
   (menu-1
    "Menu-1"
    ("Item-1"
     "Item-2")))
  (:default-initargs
   :best-height 600
   :best-width 600
   :layout 'column-layout-1
   :title *treeviewer-options-interface-title)
  ;;end treeviewer-options-interface
  )


;;MAKE-TREEVIEWER-OPTIONS-INTERFACE
;;2019
;;ddd
(defun make-treeviewer-options-interface (calling-interface)
  "U-trees-art-dims "
  (let*
      ((inst (make-instance 'treeviewer-options-interface))
       )
    (setf (slot-value inst 'calling-interface) calling-interface)
    (capi:display inst)
    inst
    
    ;;end let,make-treeviewer-options-interface
    ))
;;TEST
;; (make-treeviewer-options-interface)




;;DIM-TREEVIEWER-INTERFACE
;;2019
;;ddd
(capi:define-interface DIM-TREEVIEWER-INTERFACE ()
  (
#|   (tree-nodes-list
    :initarg :tree-nodes-list
    :accessor tree-nodes-list
    :initform NIL
    :type  :list
    :documentation  "tree-nodes-list")
   (tree-roots-list
    :initarg :tree-roots-list
    :accessor tree-roots-list
    :initform NIL
    :documentation  "tree-roots-list")
   (tree-dimlists
    :initarg :tree-dimlists
    :accessor tree-dimlists
    :initform NIL
    :documentation  "tree-rootn-list")
   (treeview-expand-level
    :initarg :treeview-expand-level
    :accessor treeview-expand-level
    :initform NIL
    :documentation  "Data from text input")
   (totaln-nodes
    :initarg :totaln-nodes
    :accessor totaln-nodes
    :initform NIL
    :documentation  "totaln-nodes")
  |#
   ;;end SLOT VARIABLES
   ) 
  (:PANES
   (tree capi:tree-view
         :roots  *tree-dimroots  ;;was '(1 2 3 4) 
         :leaf-node-p-function 'MY-LEAF-NODE-P-FUNCTION
         :children-function 'DIM-TREEVIEW-CHILDREN-FUNCTION
         ;; :image-lists (list :normal *my-image-list*)
         ;; :image-function #'(lambda (x) (mod (1- x) 4))
         :checkbox-status 0  ;;or  2=checked initially; NIL= no checkbox
      ;;   :visible-min-width 500
       ;;  :visible-min-height 800
         :has-root-line T
         :retain-expanded-nodes T
         :expandp-function  *MY-DIMS-EXPAND-FUNCTION
         ;;set in the make-instance :expandp-function 'my-expandp-function
         ;; :expandp-function USE TO FIND NESTED-LIST ITEMS TO DISPLAY AT EACH SUB CHILD MODE?? (it is called each time expand a node)
         ;;OR try to design a function for :children-function that auto fills all in at the beginning instead of calling a function to search (possibly deep in a tree) at each node
         :print-function 'MY-DIM-PRINT-FUNCTION  ;;was #'(lambda(x) (format nil "~d : ~r" x x ))
         :selection-callback #'(lambda (item self)
                                 (declare (ignore self))
                                 (format t "~&Select item ~S~%" item))
         :action-callback 'action-callback ;;was  #'(lambda (item self)    (declare (ignore self))   (format t "~&Activate item ~S~%" item))
         :delete-item-callback #'(lambda (self item)
                                   (declare (ignore self))
                                   (format t "~&Delete item ~S~%" item))))
  (:layouts
   (default-layout
    capi:simple-layout
    '(tree)))
  (:default-initargs
   :title *dim-treeviewer-frame-title)
  ;;end interface
  )





;;MAKE-DIMS-TREEVIEWER
;;2019
;;ddd
(defun make-dims-treeviewer (dimstree 
                             &key (frame-title *dim-treeviewer-frame-title)
                             tree-nodes  tree-nodelists
                             (make-treeviewer-options-interface 
                              *make-treeviewer-options-interface)
                             group-by-val-p  order-by-rank-p
                             last-list=value-p (valkey :V) (sublistkey :S)
                             (min-treeview-pane-height *dim-treeviewer-frame-ht)
                             (min-treeview-pane-width *dim-treeviewer-frame-width)
                             (x *dim-treeviewer-frame-x )(y *dim-treeviewer-frame-y)            
                             frame-init-args (expand-tree-p 99)
                             (initial-y 20))
  "In U-trees, makes a tree-view frame that uses make-tree-nodes to create a tree view of ANY dimstree to any degree of nesting. expand-tree-p= T for initial expansion of all nodes. last-list=value-p puts the value after the sublists."

  ;;CALL *make-treeviewer-options-interface FIRST
  (when make-treeviewer-options-interface
    (make-treeviewer-options-interface (mp:get-current-process))
    ;;pause current                                      
   (mp:current-process-pause 30))

  ;;FIND *tree-dimroots
  (setf *dim-tree-list dimstree)
  (let*
      ((frame-call-args `(make-instance 'dim-treeviewer-interface :title ,frame-title :x ,x :y ,y))
       (tree-inst)
       (expand-function)
       )
    (cond
     (expand-tree-p
      (setf *my-expand-function 'my-expandp-function)
      (when (numberp (setf  *treeview-expand-level expand-tree-p))))
     (t (setf *my-expand-function 'my-not-expandp-function)))
    #| doesn't work   (setf frame-call-args (append frame-call-args 
                                  `(:expandp-function (quote ,expand-function))))|#
    ;;herego
    (setf frame-init-args (append frame-init-args 
                                  (list :y initial-y :visible-min-height 
                                        min-treeview-pane-height
                                        :visible-min-width min-treeview-pane-width)))       
    (setf frame-call-args (append frame-call-args frame-init-args))
    ;;(break "before make frame")
    (setf tree-inst (eval frame-call-args))
    ;;(break "after make-frame")
    
    ;;SET THE VARIABLE VALUES
    #|    (setf (slot-value tree-inst 'totaln-nodes) *totaln-nodes
          (slot-value tree-inst 'treeview-expand-level) expand-tree-p
          (slot-value tree-inst 'tree-nodes-list) *dim-tree-list
          (slot-value tree-inst 'tree-roots-list) *tree-roots-list
          (slot-value tree-inst 'tree-rootn-list) *tree-rootn-list
          ;;(slot-value tree-inst ' 
          )|#
    (capi:display tree-inst)
    ;;end let, make-dims-treeviewer
    ))
;;TEST
;; ;; (make-dims-treeviewer *CSART-CATS-TREE)
;; WORKS!!


;; (setf *tree-dimroots '((CS)))
;; (setf  *dim-tree-list1  '(((CS) CS :S ((CS HS) "CS.HS"  :S ((((CS HS 1) "CS.HS.1"  :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))) ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))))
;;
;; (make-dims-treeviewer *dim-tree-list1 )

;;FOR REAL CSART TREE
;; (make-dims-treeviewer *CSART-CATS-TREE)

;;where already set
;; (setf *tree-dimroots '(($CS))) ;;( $VER)( $IMG)( $SND$)( $SML)( $TST)( $EMT)( $MOT)))
;; NO:  '(($CS $VER $IMG $SND$ $SML $TST $EMT $MOT)))
;;   ') 
;;  no ')
;; *CSART-CATS-TREE=>
;; (setf *CSART-CATS-TREE  '((($CS) $CS :S (($CS $TBV) $CS.$TBV :S (($CS $TBV $WV) $CS.$TBV.$WV :S ((($CS $TBV $WV $SLF) $CS.$TBV.$WV.$SLF) (($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG) (($CS $TBV $WV $FAM) $CS.$TBV.$WV.$FAM)))) (($CS $KNW) $CS.$KNW :S ((($CS $KNW $NSC) $CS.$KNW.$NSC) (($CS $KNW $BSC) $CS.$KNW.$BSC) (($CS $KNW $SSC) $CS.$KNW.$SSC) (($CS $KNW $ART) $CS.$KNW.$ART) (($CS $KNW $BUS) $CS.$KNW.$BUS) (($CS $KNW $SPT) $CS.$KNW.$SPT) (($CS $KNW $REC) $CS.$KNW.$REC))) (($CS $CSK) $CS.$CSK :S ((($CS $CSK $LRN) $CS.$CSK.$LRN) (($CS $CSK $RESN) $CS.$CSK.$RESN :S (($CS $CSK $RESN $MTH) $CS.$CSK.$RESN.$MTH) (($CS $CSK $RESN $LOG) $CS.$CSK.$RESN.$LOG)))) (($CS $SM) $CS.$SM :S ((($CS $SM $PS) $CS.$SM.$PS) (($CS $SM $DM) $CS.$SM.$DM) (($CS $SM $PLN) $CS.$SM.$PLN) (($CS $SM $COP) $CS.$SM.$COP) (($CS $SM $HTH) $CS.$SM.$HTH) (($CS $SM $FIN) $CS.$SM.$FIN))) (($CS $CAR) $CS.$CAR) (($CS $BSK) $CS.$BSK :S ((($CS $BSK $PEOP) $CS.$BSK.$PEOP) (($CS $BSK $MECH) $CS.$BSK.$MECH) (($CS $BSK $MAIN) $CS.$BSK.$MAIN) (($CS $BSK $ATH) $CS.$BSK.$ATH) (($CS $BSK $MAN) $CS.$BSK.$MAN))) (($CS $SCR) $CS.$SCR :S (($CS $SCR $ROLE) $CS.$SCR.$ROLE)) (($CS $EVT) $CS.$EVT :S ((($CS $EVT $PER) $CS.$EVT.$PER) (($CS $EVT $HIST) $CS.$EVT.$HIST) (($CS $EVT $HYP) $CS.$EVT.$HYP))) (($CS $ELM) $CS.$ELM :S ((($CS $ELM $PEO) $CS.$ELM.$PEO) (($CS $ELM $OBJ) $CS.$ELM.$OBJ) (($CS $ELM $ANM) $CS.$ELM.$ANM)))) (($VER) $VER :S ((($VER $GRA) $VER.$GRA) (($VER $SEN) $VER.$SEN) (($VER $WRD) $VER.$WRD))) (($IMG) $IMG) (($SND$) $SND$) (($SML) $SML) (($TST) $TST) (($EMT) $EMT :S ((($EMT $HAP) $EMT.$HAP) (($EMT $LOV) $EMT.$LOV) (($EMT $ANX) $EMT.$ANX) (($EMT $ANG) $EMT.$ANG) (($EMT $DEP) $EMT.$DEP))) (($MOT) $MOT :S (($MOT $SPH) $MOT.$SPH))))




;;MAKE-TREE-FROM-DIMLISTS
;; REPLACED (ENTIRELY?) BY MAKE-CSARTDIMS-TREE ?
;;2019
;;ddd
#|(defun make-tree-from-dimlists  (tree &key   (cur-dim-n 0) 
                                   group-by-val-p  order-by-rank-p 
                                   (root-separator ".")
                                   last-list=value-p (csymkey :C) (valkey :V)(sublistkey :S)
                                   add-other-keys (default-key-value NIL)
                                   (tree-leveln 0) tree-leveln-list
                                     parent-dimlist parent-dimsymstr
                                   frame-init-args)
#|;; group-by-val-p  order-by-rank-p (root-separator ".")
                                    last-list=value-p (valkey :V=)(sublistkey :S)
                                    parent-rootn parent-rootnlist  (root-leveln 0)
                                    root-leveln-list frame-init-args)|#
  "In U-trees-art-dims, MAKES A COMPLEX TREE FROM A SIMPLIER INPUT FORMULA. Use in CS & ART.
   INPUT: All level-nodelist items (branch) must be a dim, key from keyslist, or following key-value.  Any can be list or not. (Each branch = level-nodelist can have leaves= nodelists within that level) Input eg= (cs :s (hs :s (1 :s (1 2)  2 :s (1  2  3)   3))  (ms :s (ms1 :s (1 2)))).  
  RETURNS (values return-tree. Eg. (((CS)\"CS\" :S (((CS HS) \"CS.HS\":S (((CS HS 1) \"CS.HS.1\":S (((CS HS 1 1) \"CS.HS.1.1\") ((CS HS 1 2) \"CS.HS.1.2\"))) ETC.
  USE IN CS/ART: artloc=dimlist (incl prefix); CSYM can either be found from eval artsym or csym can be stored as a value to csymkey.  Tree can be ART-CS DATABASE partially or completely filled.  If start empty, can add artsym/csym to prexisting dimlists. Or could add whole subtrees. Could either check boundp for dimsymstr or value for csymkey."
  #|All level-nodelist items (branch) must be dim ,key from keyslist, or Val.  Any can be list or not. (Each branch = level-nodelist can have leaves= nodelists).
Make new nodelist of dimlist, dimsynstr on item if it passes thru cond keys/keyvalues.
(when NULL NODELIST) and find new NONKEYORVALUE ITEM or on ITEM
when  last-level-item-p.
Append nodelist to LEVEL-MODELIST after last level item( in cur items loop) or
find new nonkeyorvalue item.
Append return-tree after loop end when nonnil level-nodelist.
Make new nodelist on lastitem when find key or levelist end.
|#
  (let*
      ((len-tree (list-length tree))
       (return-tree) 
       (nodelist)
       (level-nodelists)
       (totaln 0)
       (all-keys (append (list csymkey valkey sublistkey) add-other-keys))
       (previous-item=key-p)
       (next-sublist-p)
       (previous-item)
       (last-level-item-p)
       (root-level-dimlist parent-dimlist)
       (root-level-dimsymstr parent-dimsymstr)
       ;;added from old
       (flat-dims-list)
       (tree-leveln-list)
       (bottom-dimlist)
       )
    (incf tree-leveln)
    ;;WHEN TREE IS NON-NIL LIST
    (when (and tree (listp tree))
      ;;LOOP THRU ITEMS AT THIS LEVEL
      (LOOP
       for item in tree
       for item-n from 1 to len-tree
       do
       (let*
           ((dim)
            (dimlist)
            (dimsymstr)  
            (last-level-item-p (= item-n len-tree))          
            )
         ;;(afout 'out (format nil "NEW LOOP ITEM= ~A~% nodelist= ~A level-nodelists= ~A return-tree= ~A" item nodelist level-nodelists return-tree))
         (cond   
          (next-sublist-p
           (setf next-sublist-p NIL
                 previous-item=key-p NIL)       
           ;;RECURSE 
           (multiple-value-bind (subtree  dimrootns
                                                   flat-dimrootns dimroot-leveln-list
                                                   totaln-dimnodes  dimroots)
               ;;was  flat-dims-list1 tree-leveln-list1 totaln1 bottom-dimlist1)
               ;;tree-leveln-list1 totaln) ;; bottom-dimlist)
               (make-dim-tree-nodes-list item  ;;:n (- n 1) 
                                     :parent-dim dim
                                     :parent-dimlist parent-dimlist
                                     :parent-dimsymstr parent-dimsymstr
                                     :root-separator root-separator
                                     :tree-leveln  tree-leveln
                                     :last-list=value-p  last-list=value-p
                                     ;; :tree-num-list tree-num-list
                                     )
             ;;here22
;;*tree-dimnodes-list *tree-dimrootn-list *tree-flat-dimrootn-list 
                         ;;            *tree-dimroot-leveln-list *totaln-dimnodes  *tree-dimroots)
             ;;add current level info OLD
#|             (setf subtree-parent-node (list rootnlist rootn sublistkey subtree-nodes)
                   tree-nodes (append tree-nodes  (list subtree-parent-node))
                   rootn-list (append rootn-list (list rootn-list1))
                   flat-rootn-list (append flat-rootn-list flat-rootn-list1)
                   ;;flat-rootnlist-list (append flat-rootnlist-list flat-rootnlist-list1)
                   root-leveln-list (append root-leveln-list (list root-leveln-list1)))
             (setf totaln (+ totaln return-n)) |#
          ;;(subtree  dimrootns flat-dimrootns dimroot-leveln-list  totaln-dimnodes  dimroots)
             ;;APPEND NODELIST 
             (when nodelist
               (setf nodelist (append nodelist (list subtree))
                     subtree-parent-dimnode (list dimrootns dim sublistkey subtree);;dimliststr??
                   tree-nodes (append tree-nodes  (list subtree-parent-node))
                   rootn-list (append rootn-list (list rootn-list1))
                   flat-rootn-list (append flat-rootn-list flat-rootn-list1)
                   ;;flat-rootnlist-list (append flat-rootnlist-list flat-rootnlist-list1)
                   root-leveln-list (append root-leveln-list (list root-leveln-list1))
                 ;;end revised

                     flat-dims-list (append flat-dims-list (list flat-dims-list1))
                     tree-leveln-list (append tree-leveln-list (list tree-leveln-list1))
                    totaln (+ totaln totaln1)))
             ;;(afout 'out (format nil "END RECURSE: item= ~A~% level-nodelists= ~A return-tree= ~A~%  subtree= ~A~%  nodelist= ~A~% " item level-nodelists return-tree subtree nodelist))
             ;;(BREAK "END RECURSE")
             ;;end mvb, sublist-recurse clause
             ))
          ;;first item past dim must be a key or end of list
          ((equal item sublistkey)
           (setf next-sublist-p T
                 previous-item=key-p T
                 nodelist (append nodelist (list item)))
           )
          ((member item all-keys :test 'equal)
           (setf previous-item=key-p T
                 nodelist (append nodelist (list item)))
           ;;end member key
           )
          (previous-item=key-p
           (setf previous-item=key-p NIL)
           (setf nodelist (append nodelist (list item)))
           )
          ;;IF NOT A KEY OR KEYVALUE (filtered out above), MUST BE NEW DIM
          ((or item last-level-item-p)        
           ;;CLOSE PREVIOUS NODELIST AND APPEND LEVEL-NODELISTS
           (when nodelist
             (setf level-nodelists (append level-nodelists (list nodelist)))
             ;;(afout 'out (format nil "1-APPENDED LEVEL-NODELISTS= ~A ITEM= ~a" level-nodelists item))
             )         
           ;;START NEW NODELIST, SINCE ITEM MUST BE A DIM (if end of list or no)
           (cond
            (root-level-dimlist
             (setf dim item 
                   dimsymstr (format nil "~A~A~A" root-level-dimsymstr
                                     root-separator item)
                   dimlist (append root-level-dimlist (list  item)))
             (setf parent-dimlist dimlist
                   parent-dimsymstr (format nil "~A~A~A" root-level-dimsymstr 
                                            root-separator item))
             )
            (t (setf dim item
                     dimsymstr (format nil "~A" item)
                     dimlist (list  item)
                     parent-dimlist dimlist
                     parent-dimsymstr (format nil "~A" item)
                     )))
           ;;NEW NODELIST           
           (setf nodelist (list dimlist dimsymstr))
           ;;end T cond
           ))
         ;;(afout 'out (format nil "END LOOP: item= ~A nodelist= ~A~% dimlist= ~A~%dimsymstr= ~A dim=~A~% level-nodelists= ~A "item nodelist dimlist dimsymstr dim level-nodelists))
         ;;end let, loop, when listp clause
         ))
      ;;APPEND RETURN-TREE WITH LEVEL-NODELISTS
      ;;UPDATE PARENT DIMLISTS
      (when  nodelist        
        (setf level-nodelists (append level-nodelists (list nodelist))))            
      (setf return-tree level-nodelists)
      ;;end when listp
      )
    (setf tree-leveln-list (append tree-leveln-list (list tree-leveln)))
#|  *tree-dimnodes-list *tree-dimrootn-list *tree-flat-dimrootn-list 
                                         *tree-dimroot-leveln-list *totaln-dimnodes  *tree-dimroots)|#
    (values  return-tree  flat-dims-list tree-leveln-list totaln bottom-dimlist)
 ;;to set frame values: *tree-rootn-list *tree-flat-rootn-list *tree-dimroot-leveln-list *totaln-nodes  *tree-roots-list)
    ;;end let, make-tree-from-dimlists
    ))|#
;;TEST
;; SSSS TEST ON REAL CS/ART SYM TREES
;;  (setf *test-artdims-tree1 '(cs :s (hs :s (1 :s (1 2)  2 :s (1  2  3)   3)  ms :s (ms1 :s (1 2)))))
;; (make-tree-from-dimlists *test-artdims-tree1)
;; WORKS=
#| (setf csart-tree-dimsXX '(((CS)   "CS"  :S
  (((CS HS)    "CS.HS"    :S
    (((CS HS 1)      "CS.HS.1"      :S
      (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2")))
     ((CS HS 2)      "CS.HS.2"      :S
      (((CS HS 2 1) "CS.HS.2.1")
       ((CS HS 2 2) "CS.HS.2.2")
       ((CS HS 2 3) "CS.HS.2.3")))
     ((CS HS 3) "CS.HS.3")))
   ((CS MS)    "CS.MS"    :S
    (((CS MS MS1)      "CS.MS.MS1"      :S
      (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))))) |#

;;MAKE-TREE
;; (make-treeview-frame CSART-TREE-DIMSXX)
#|
;;FOLLOWING MANUALLY SET FOR TESTING MAKE-TREE-VIEWER-FRAME  REMOVED :S
 (setf *tree-root-dimslist '((CS)))
 (setf *dim-tree-list
   '(((CS)   "CS"  :S
  (((CS HS)    "CS.HS"    :S
    (((CS HS 1)      "CS.HS.1"      :S
      (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2")))
     ((CS HS 2)      "CS.HS.2"      :S
      (((CS HS 2 1) "CS.HS.2.1")
       ((CS HS 2 2) "CS.HS.2.2")
       ((CS HS 2 3) "CS.HS.2.3")))
     ((CS HS 3) "CS.HS.3")))
   ((CS MS)    "CS.MS"    :S
    (((CS MS MS1)      "CS.MS.MS1"      :S
      (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))))

       *tree-rootn-list   '("CS"  ("CS.HS"  ("CS.HS.1"  ("CS.HS.1.1"  "CS.HS.2")  "CS.HS.2" ("CS.HS.2.1" "CS.HS.2.2" "CS.HS.2.3")   "CS.HS.3")  "CS.MS"  ("CS.MS.MS1"          ("CS.MS.MS1.1"  "CS.MS.MS1.2"))))
       *tree-flat-rootn-list '("CS"  "CS.HS"  "CS.HS.1"  "CS.HS.1.1"  "CS.HS.2"  "CS.HS.2" "CS.HS.2.1" "CS.HS.2.2" "CS.HS.2.3"   "CS.HS.3"  "CS.MS"  "CS.MS.MS1"          "CS.MS.MS1.1"  "CS.MS.MS1.2")
       *tree-dimroot-leveln-list '(cs :s (hs :s (1 :s (1 2)  2 :s (1  2  3)   3)  ms :s (ms1 :s (1 2))))
       *totaln-nodes 14
       *tree-roots-list '(CS))  ;;tree-viewer auto changes this to (1)
 |#

#| (setf *tree-dimroots '((CS)))
 (setf *dim-tree-list
  '(((CS HS)    "CS.HS"    :S
    (((CS HS 1)      "CS.HS.1"      :S
      (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2")))
     ((CS HS 2)      "CS.HS.2"      :S
      (((CS HS 2 1) "CS.HS.2.1")
       ((CS HS 2 2) "CS.HS.2.2")
       ((CS HS 2 3) "CS.HS.2.3")))
     ((CS HS 3) "CS.HS.3")))
   ((CS MS)    "CS.MS"    :S
    (((CS MS MS1)      "CS.MS.MS1"      :S
      (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))|#


;; OLD TEST (make-treeview-frame *dim-tree-list)
;; RESULTS= INACURATE & OVERLY COMPLEX TREE with extra nodes from :S, several extra "CS" nodes, etc. NOT EVEN CLOSE. ALL CS DIMS WERE INCLUDED AS VALUES :V.
;; THE INTERFACE CHANGED THE INPUT LIST (ABOVE) TO *dim-tree-list
;;  (((1) "1" :SL (((1 1) "1.1" :SL (((1 1 1) "1.1.1" :V= CS))) ((1 2) "1.2" :V= "CS") ((1 3) "1.3" :V= :S) ((1 4) "1.4" :SL (((1 4 1) "1.4.1" :SL (((1 4 1 1) "1.4.1.1" :SL (((1 4 1 1 1) "1.4.1.1.1" :V= CS) ((1 4 1 1 2) "1.4.1.1.2" :V= HS))) ((1 4 1 2) "1.4.1.2" :V= "CS.HS") ((1 4 1 3) "1.4.1.3" :V= :S) ((1 4 1 4) "1.4.1.4" :SL (((1 4 1 4 1) "1.4.1.4.1" :SL (((1 4 1 4 1 1) "1.4.1.4.1.1" :SL (((1 4 1 4 1 1 1) "1.4.1.4.1.1.1" :V= CS) ((1 4 1 4 1 1 2) "1.4.1.4.1.1.2" :V= HS) ((1 4 1 4 1 1 3) "1.4.1.4.1.1.3" :V= 1))) ((1 4 1 4 1 2) "1.4.1.4.1.2" :V= "CS.HS.1") ((1 4 1 4 1 3) "1.4.1.4.1.3" :V= :S) ((1 4 1 4 1 4) "1.4.1.4.1.4" :SL (((1 4 1 4 1 4 1) "1.4.1.4.1.4.1" :SL (((1 4 1 4 1 4 1 1) "1.4.1.4.1.4.1.1" :SL (((1 4 1 4 1 4 1 1 1) "1.4.1.4.1.4.1.1.1" :V= CS).)   ETC  ETC  ETC  

       


;;DIM-TREEVIEW-CHILDREN-FUNCTION
;;2019
;;ddd
(defun dim-treeview-children-function (root)
  "U-trees-art-dims - same as U-trees treeview-children-function? RETURNS *child-dimlists"
  (when root  ;;was 100,  limits node number (larger N means more depth levels).
    (unless (listp root)
      (setf root (list root)))
            
         ;;ORIG--LIKE U-TREES
         (multiple-value-setq (*TREE-CHILD-SUBLISTS 
                               *tree-dim-parent-list *CHILD-DIMLISTS 
                               *child-root-value )
             (get-dim-node-childlists root *dim-tree-list))  
;; works: (get-dim-node-childlists  '($CS $TBV $WV) *CSART-CATS-TREE)
;; CHILD-SUBLISTS=  ((($CS $TBV $WV $SLF) $CS.$TBV.$WV.$SLF) (($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG) (($CS $TBV $WV $FAM) $CS.$TBV.$WV.$FAM))
;;return-parent (($CS $TBV $WV) $CS.$TBV.$WV :S ((($CS $TBV $WV $SLF) $CS.$TBV.$WV.$SLF) (($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG) (($CS $TBV $WV $FAM) $CS.$TBV.$WV.$FAM)))
;;child-rootnlists= (($CS $TBV $WV $SLF) ($CS $TBV $WV $REFG) ($CS $TBV $WV $FAM))  root-value= NIL
         ;;*omit-sublists-in-node-info (default = T)
        (when *omit-sublists-in-node-info 
          (setf *tree-dim-parent-list-w-sublists *tree-dim-parent-list
                *tree-dim-parent-list 
                (nth-value 1 (set-singlekey-value-in-nested-lists 
                              ;;SSSS FIX THIS 
                              *child-root-value :S *tree-dim-parent-list))))
                            ;;WAS  *new-dim-sublist-value :S *tree-dim-parent-list))))
        ;;(break "in callback *tree-child-sublists")
        ;;(afout 'out (format nil "AFTER mvs for root= ~A~%*tree-dim-parent-list= ~A~%*CHILD-DIMLISTS= ~A~% *TREE-CHILD-SUBLISTS= ~A" root *tree-dim-parent-list *child-dimlists *tree-child-sublists))
         ;;(values *tree-dim-parent-list *child-dimlists *tree-dim-parent-list-w-sublists)
         *child-dimlists
       ;;end let,when,dim-treeview-children-function
       ))
;;TEST
;; (setf *tree-dimroots  '((CS)))
;; (setf  *dim-tree-list '(((CS) "CS"  :S (((CS HS) "CS.HS"  :S (((CS HS 1) "CS.HS.1"  :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))) ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))))
;;;; (dim-treeview-children-function '(cs))
;; 
;; (dim-treeview-children-function '(cs hs))
;; works= ((CS HS 1) (CS HS 2) (CS HS 3))
;; (dim-treeview-children-function '(cs hs 2))
;; works= ((CS HS 2 1) (CS HS 2 2) (CS HS 2 3))

;;(get-dim-node-childlists '(cs hs 2) *dim-tree-list)
;; works= (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))          ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3")))          ((CS HS 2 1) (CS HS 2 2) (CS HS 2 3))   NIL



;;GET-DIM-NODE-CHILDLISTS
;; CAN USE U-trees get-node-childlists instead=
;;ddd
(defun get-dim-node-childlists (target-rootnlist tree-node-list &key (target-leveln 1)
                                         (valkey :V)(sublistkey :S))
  "U-trees  RETURNS: (values return-parent child-rootnlists root-value child-sublists)  Set parent-rootlist= 0 or NIL to find base roots.  "
  (when (equal target-rootnlist 0)
    (setf rootnlist NIL))
  (unless (listp target-rootnlist)
    (setf target-rootnlist (list target-rootnlist)))
  (let*
      ((n-levels (list-length target-rootnlist))
        (speclist (list (list target-rootnlist 0 0)))
       (return-parent (get-set-append-delete-keyvalue-in-nested-list
                       :get speclist tree-node-list :return-list-p T))
        (root-value (get-key-value valkey return-parent))
        (child-sublists (get-key-value sublistkey return-parent))
        (child-rootnlists  (get-level-dimlists  child-sublists))
       )
    (when (null target-rootnlist)
      (setf parent-leveln 0))
     ;;(break "get-dim-node-childlists")
     (values CHILD-SUBLISTS return-parent child-rootnlists root-value )
      ;;end let, get-dim-node-childlists
      ))
;;TEST 
;; ;; (get-dim-node-childlists  '($CS) *CSART-CATS-TREE)
;;; works
;; ;; (get-dim-node-childlists  '($CS $TBV $WV) *CSART-CATS-TREE)
;; works= 
;; CHILD-SUBLISTS=  ((($CS $TBV $WV $SLF) $CS.$TBV.$WV.$SLF) (($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG) (($CS $TBV $WV $FAM) $CS.$TBV.$WV.$FAM))
;;return-parent (($CS $TBV $WV) $CS.$TBV.$WV :S ((($CS $TBV $WV $SLF) $CS.$TBV.$WV.$SLF) (($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG) (($CS $TBV $WV $FAM) $CS.$TBV.$WV.$FAM)))
;;child-rootnlists= (($CS $TBV $WV $SLF) ($CS $TBV $WV $REFG) ($CS $TBV $WV $FAM))  root-value= NIL


;; (setf *tree-dimroots'((CS)))
;; (setf  *dim-tree-list1  '(((CS) CS :S ((CS HS) "CS.HS"  :S ((((CS HS 1) "CS.HS.1"  :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))) ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))))
;; (get-dim-node-childlists  '(CS) *dim-tree-list1)
;;CHILD-SUBLISTS ((CS HS) "CS.HS" :S ((((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))) ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2"))))))
;;return-parent ((CS) CS :S ((CS HS) "CS.HS" :S ((((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))) ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))
;;child-rootnlists (CS (((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3")) (CS MS))
;;root-value NIL



;; (get-dim-node-childlists  '(CS HS 1) *dim-tree-list)
;; works=
;;return-parent= ((CS HS 1) "CS.HS.1" :V (QUOTE VALUE) :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2")))
;;CHILD-ROOTNLISTS= ((CS HS 1 1) (CS HS 1 2))
;;root-value= (QUOTE VALUE)
;;child-sublists= (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))
;; (get-dim-node-childlists  '(CS HS)  *dim-tree-list)
;; works= 
;;return-parent= ((CS HS) "CS.HS" :V 99 :S (((CS HS 1) "CS.HS.1" :V (QUOTE VALUE) :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3")))
;;children= (((CS HS 1) "CS.HS.1" :V (QUOTE VALUE) :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))
;;flat-children= ((((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2") NIL NIL) (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3") NIL NIL NIL) NIL)


;;USING get-node-childlists
;; (get-node-childlists '((CS HS 2)) *dim-tree-list)
;; results= NOT WORK = NIL NIL NIL NIL
;; NON-CS
;; (setf *testcl33 '(((1) "1" :VA A) ((2) "2" :SL (((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3) "2.3" :SL (((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6) "2.3.6" :SL (((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))) ((2 3 6 3) "2.3.6.3" :VA W))) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y))) ((2 4) "2.4" :VA Z))) ((3) "3" :VA END))  )
;; for end in value
;; (get-node-childlists  '(2 3 5)  *testcl33 :sublistkey :SL :valkey :VA)
;; works = ((2 3 5) "2.3.5" :VA M) NIL  M   NIL
;; for end in sublists
;; (get-node-childlists  '(2 3 4)  *testcl33 :sublistkey :SL :valkey :VA)
;; works= ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) 
;;child-dimlists= ((2 3 4 1) (2 3 4 2))
;;   rootvalue= NIL sublists= (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))
;; (get-node-childlists  '(2 3 1) *dim-tree-list)



;;GET-DIMLIST-CHILDIMS     was GET-DIMLIST-SUBLISTS
;;
;;ddd
(defun get-dimlist-childims (parent-dimlist &key (sublistkey :S))
  "U-trees-art-dims RETURNS: (values  child-dimlists children )"
  (let
      ((child-dimlists)
       (children)
       )
    (cond
     ((listp parent-dimlist)
      (setf children (get-key-value sublistkey parent-dimlist))
      ;;(break)
      (loop
       for child in children
       do
       (let
           ((dimlist)
            )
         (when (listp child)
           (setf dimlist (car  child)
                 child-dimlists (append child-dimlists (list dimlist))))
         ;;end let,loop
         ))
      )
     (t nil))
     (values  children child-dimlists  )    ;;end let,get-dimlist-sublists
     ;;end let, get-dimlist-childims
    ))
 ;;TEST
;; (get-dimlist-childims    '((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))))        =  ((2 3 4 1) (2 3 4 2))
;;  
;; (get-dimlist-childims   '((CS HS 2)      "CS.HS.2"      :S        (((CS HS 2 1) "CS.HS.2.1")        ((CS HS 2 2) "CS.HS.2.2")       ((CS HS 2 3) "CS.HS.2.3")))  :SUBLISTKEY :S )
;; works= ((CS HS 2 1) (CS HS 2 2) (CS HS 2 3))            (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))     
;; (get-dimlist-childims



;;SIMPLE-GET-NESTED-CHILDLISTS
;;2019
;;ddd
(defun simple-get-nested-childlists (parents  &key (n-dims 3) (sublistkey :s) (totaln 0)
                                           (return-sublists-in-children-p T))
  "U-trees-art-dims For use making dimlist trees  RETURNS (values  new-parents dimlists flat-children flat-dimlists totaln)   INPUT:  "
     (let*
         ((new-parents)
          (childlists)
          (dimlists)
          (flat-children)
          (flat-dimlists)
          (totaln 0)
          (n-parents (list-length parents))          
          )  
    (loop
     for parent in parents
     for n from 1 to n-parents 
     do
     (when (listp parent)
       (let*
           ((parent-childlists)
            (parent-dimlists)
            (parent-flat-children)
            (parent-flat-dimlists)
            (parent-totaln 0)
            (len-parent (list-length parent))
            (new-parent)
            (next-is-sublist-p)   
            (child-totaln 0)
            )
         (loop
          for item in parent
          for n from 1 to len-parent
          do
          (let*
              ((dimlist) 
               )
            ;;NOTE: NOT necessary to loop through parent and create new-parent
            ;;however, gives opportunity to modify in future--changining or adding parts.
            (cond
             ;;if sublist key
             ((equal item sublistkey)
              (setf next-is-sublist-p T)
             ;;(break "1")
              (setf new-parent (append new-parent (list item))))
             ;;if sublist
             (next-is-sublist-p
              (setf next-is-sublist-p nil)
              ;;add sublist to new-parent 
              (when (and parent return-sublists-in-children-p)
                (setf new-parent (append new-parent (list item))))
              ;;recurse on children
              (multiple-value-bind (chlldlists1 dimlists1 flat-return-children1 
                                                flat-dimlists1 totaln1)
                  (simple-get-childlists item :totaln totaln)
                (when chlldlists1
                  (setf parent-childlists (append parent-childlists (list chlldlists1))))
                (when dimlists1
                        parent-dimlists (append parent-dimlists (list dimlists1)))
                (when flat-dimlists1
                        parent-flat-dimlists (append parent-flat-dimlists flat-dimlists1))
                (when totaln1
                  (setf child-totaln (+ child-totaln totaln1)))
                (cond
                 ((and item flat-return-children1)
                  (setf parent-flat-children (append parent-flat-children item flat-return-children1)))
                 (item (setf parent-flat-children (append parent-flat-children item )))
                 (flat-return-children1
                  (setf parent-flat-children (append parent-flat-children flat-return-children1 ))))
                ;;(break "after mvb")
                ;;end mvb,recurse
                ))
             ;;for dimlist
             ((and (= n 1)(listp item))
              (setf new-parent  (list item)
                    child-totaln (+ child-totaln 1)
                    dimlist item
                    parent-dimlists (append parent-dimlists (list dimlist))
                    parent-flat-dimlists (append parent-flat-dimlists (list dimlist)))
              ;;(break "dimlist")
              )
             ;;otherwise 
             (t  (setf new-parent (append new-parent (list item)))))
            ;;end let,loop, 
            ))
           (setf new-parents (append new-parents (list new-parent))
                 dimlists (append dimlists (list parent-dimlists))
                 flat-children (append flat-children  (list parent-flat-children))
                 flat-dimlists (append flat-dimlists parent-flat-dimlists)
                 totaln (+ totaln child-totaln))          
       ;;end let, loop,when listp
       )))
    (values  new-parents dimlists flat-children flat-dimlists totaln) ;;redundant? flat-children
    ;;end let, simple-get-nested-childlists
    ))
;;TEST
;; (setf *dim-tree-list '(((CS HS) "CS.HS" :S (((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))) ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))
;;
;; (simple-get-nested-childlists *dim-tree-list)
;;works?=  new-parents dimlists flat-children flat-dimlists totaln
;;new-parents=  (((CS HS) "CS.HS" :S (((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))) ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2"))))))
;;dimlists= (((CS HS) (((CS HS 1) (((CS HS 1 1)) ((CS HS 1 2)))) ((CS HS 2) (((CS HS 2 1)) ((CS HS 2 2)) ((CS HS 2 3)))) ((CS HS 3)))) ((CS MS) (((CS MS MS1) (((CS MS MS1 1)) ((CS MS MS1 2)))))))
;;flat-children= ((((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2")))      ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3")))     ((CS HS 3) "CS.HS.3")    (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2") NIL NIL)      (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3") NIL NIL NIL)   NIL)      (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2"))) (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2") NIL NIL)))
;;flat-dimlists= ((CS HS) (CS HS 1) (CS HS 1 1) (CS HS 1 2) (CS HS 2) (CS HS 2 1) (CS HS 2 2) (CS HS 2 3) (CS HS 3) (CS MS) (CS MS MS1) (CS MS MS1 1) (CS MS MS1 2))
;;totaln= 13



;;MY-PRINT-FUNCTION
;;2019
;;ddd
(defun my-print-function (rootdims)
  "works"
  ;;(break)
  (let
      ((root-list)
       )   
    (unless (listp rootdims)
      (setf rootnlist (list rootdims)))
    ;;SSSSS HERE4
    (multiple-value-setq (*TREE-CHILD-SUBLISTS *tree-dim-parent-list 
                                               *child-dimslists *child-root-value    )  
        (get-dim-node-childlists rootdims *dim-tree-list))
    ;;(break "my-print-function: *tree-dim-parent-list")
  ;; (get-node-childlists '(2) *tree-nodes-list)
   
    (setf root-list *child-rootnlists) ;;was *tree-dim-parent-list)
    (print-node-info root-list :omit-root-info-p NIL :valkey :v :sublistkey :s)
    (format nil "~A: ~A" (make-root-string rootdims) (print-node-info root-list))
    ;;was "Node: ~A: Nodelist: ~A" (make-root-string root) root)
    ;;end let, my-print-function
    ))
;;TEST
;; (print-node-info *tree-dim-parent-list :omit-root-info-p T :valkey :v :sublistkey :s)
;; (print-node-info *tree-dim-parent-list :omit-root-info-p NIL :valkey :v :sublistkey :s)
;; works= (((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))

  ;; (get-dim-node-childlists '($CS $TBV $WV $REFG) *dim-tree-list)
  ;;WORKS= (($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG)
 ;;(print-node-info '(($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG) :omit-root-info-p NIL :valkey :v :sublistkey :s) 
;; works= (($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG)



;;PRINT-NODE-INFO
;;
;;ddd
(defun print-node-info (tree-root-childlists &key (omit-root-info-p T) omit-rootnlist-p
                                             (valkey :V) (sublistkey :S) (csymkey :C) (csymvalkey :CV)
                                             (csymdesckey :CD))
  (let*
      ((sublists-p (member sublistkey tree-root-childlists :test 'equal))
       (value (get-key-value valkey  tree-root-childlists))
       (rootinfo) 
       )
    (cond
     (sublists-p
      (setf rootinfo (butlast tree-root-childlists 1)
            rootinfo (append rootinfo (list  " Select +"))))
     (t (setf rootinfo  tree-root-childlists)))
    (cond
     (omit-root-info-p
      (setf rootinfo (cddr rootinfo))
      )
     (omit-rootnlist-p
      (setf rootinfo (cdr rootinfo))))
    ;;(break "rootinfo")
    rootinfo
    ;;end let, get-node-info
    ))
;;TEST
;; (print-node-info '((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) :omit-rootnlist-p NIL :valkey :VA :sublistkey :SL)
;; works= ((2 3 4) "2.3.4"  " Select +")
;

;; (find-keys-and-values-in-list '(:C :CV :CD :V) '((2 3 4) "2.3.4" :C "HAPPY" :V "ARTVAL" :S (((2 3 4 1) "2.3.4.1" ) )) :return-keys-not-found-p T)
;; works= (:C "HAPPY" :V "ARTVAL")    (:C :V)    (:CV :CD)


(defun my-dims-expandp-function (roots)
  "In U-trees"
  (let
      ((expandp T)
       )
   (when (and (listp roots)
              (> (length roots) *treeview-expand-level))
     (setf expandp NIL))
   expandp
  ;;end  let, my-expandp-function
  ))

;;MY-NOT-EXPANDP-FUNCTION
;;
;;ddd
(defun my-NOT-expandp-function (roots)
  "In U-trees"
   NIL
  ;;end  my-expandp-function
  )



;;MY-DIM-PRINT-FUNCTION
;;
;;ddd
(defun my-dim-print-function (rootdims)
  "U-trees-art-dims."
  ;;(break)
  (let
      ((node-print-string "")
       )   
    (unless (listp rootdims)
      (setf rootdims (list rootdims)))

    (multiple-value-bind (node-info-string  all-info-string)
        (get-artsymnode-info rootdims)
     
      (cond
       (*print-all-node-info-p 
        (setf node-print-string all-info-string))
       (t (setf node-print-string node-info-string)))

    ;;SSSSS HERE4
#|    (multiple-value-setq (*tree-dim-parent-list *child-dimslists  *child-root-value *tree-child-sublists   )  
        (get-dim-node-childlists rootdims *dim-tree-list))
    ;;(break "my-print-function: *tree-dim-parent-list")
  ;; (get-node-childlists '(2) *tree-nodes-list)
    
    (setf root-list *child-rootnlists) ;;was *tree-dim-parent-list)
    (print-node-info root-list :omit-root-info-p NIL :valkey :v :sublistkey :s)
    (format nil "~A: ~A" (make-root-string rootdims) (print-node-info root-list))|#
    ;;was "Node: ~A: Nodelist: ~A" (make-root-string root) root)
    ;;(break "node-print-string")
    node-print-string
    ;;end mvb,let, my-dim-print-function
    )))


;;GET-ARTSYMNODE-INFO
;;2019
;;ddd
(defun get-artsymnode-info (artdims  &key (artdimstree *dim-tree-list)
                                     (omit-some-nil-p *expanded-node-list-p) 
                                     (incl-csymlist-p *incl-csymlist-in-treeviewer-p)
                                     (cskey :C) (artvalkey :V)
                                     (csvalkey :CV)(cs-csvalkey :CSVAL)
                                     (rankey :R) (cs-rankey :CSRANK))
  "U-trees-art-dims   RETURNS (values node-info-string  all-info-string csym csval artnodelist)   INPUT: Can use artsym string or sym instead of artdims."
  (unless (listp artdims)
          artdims (find-artsym-dims artdims))
  (let*
      ((artnodelist (get-set-append-delete-keyvalue-in-nested-list :get
                     (list (list artdims 0 0)) artdimstree :return-list-p T))
       ;;(get-set-append-delete-keyvalue-in-nested-list :get (list (list '($CS $TBV) 0 0)) *CSART-CATS-TREE :return-list-p T)
       (artsymstr (second artnodelist))
       (csym (get-key-value cskey artnodelist))
       (artval (get-key-value artvalkey artnodelist))
       (csymlist (eval (my-make-symbol csym)))
       (csval (get-key-value cs-csvalkey csymlist))
       (csrank (get-key-value cs-rankey csymlist))
       (csdesc (second  csymlist))
       (all-info-string "")
       (node-info-string "")
       )
    (cond
     (omit-some-nil-p ;;artval, csval, csdesc,csymlist
       (setf all-info-string (format nil "~A ~S" ARTSYMSTR CSYM))
       (setf node-info-string (format nil "~A ~S" artsymstr csym)) 
       (when csval (setf all-info-string (format nil "~A :~A ~S"all-info-string csvalkey csval)
                         node-info-string (format nil "~A :~A ~S"node-info-string csvalkey csval))
         (when csrank
           (setf all-info-string (format nil "~A :~A ~A"all-info-string rankey csrank)
                         node-info-string (format nil "~A :~A ~A"node-info-string rankey csrank))))
       (when csdesc (setf all-info-string (format nil "~A ~S"all-info-string csdesc)
                  node-info-string (format nil "~A ~S"node-info-string csdesc)))       
       (when artval (setf all-info-string (format nil "~A :~A ~S"all-info-string artvalkey artval)
                  node-info-string (format nil "~A :~A ~S"node-info-string artvalvalkey artval)))
       ;;always include
       (setf all-info-string (format nil "~A ~A"all-info-string artdims)
                  node-info-string (format nil "~A ~A"node-info-string artdims))
       (when artval (setf all-info-string (format nil "~A :~A ~S"all-info-string artvalkey artval)
                  node-info-string (format nil "~A :~A ~S"node-info-string artvalkey artval)))

      )
     (t
       (setf all-info-string (format nil "~A ~S :~A ~S ~S  ~S :~A ~S" artsymstr csym csvalkey csval csdesc  artdims artvalkey artval))
       (setf node-info-string (format nil "~A ~S :~A ~S ~S :~A ~S" artsymstr csym csvalkey  csval csdesc artvalkey artval))))       
    (when incl-csymlist-p 
      (setf all-info-string (format nil "~A; ~A"all-info-string csymlist)
                  node-info-string (format nil "~A; ~A"node-info-string csymlist)))
      
    (values node-info-string  all-info-string csym csval artnodelist)
    ;;end let, get-artsymnode-info
    )) ;;HERE222
;;TEST
;; ;; (get-artsymnode-info '($CS $TBV $WV) :artdimstree *CSART-CATS-TREE)
;; returns:
#|"$CS.$TBV.$WV NIL :CV NIL NIL :V NIL"
"$CS.$TBV.$WV NIL :CV NIL NIL  ($CS $TBV $WV) :V NIL"
NIL
NIL
(($CS $TBV $WV) $CS.$TBV.$WV :S ((($CS $TBV $WV $SLF) $CS.$TBV.$WV.$SLF) (($CS $TBV $WV $REFG) $CS.$TBV.$WV.$REFG) (($CS $TBV $WV $FAM) $CS.$TBV.$WV.$FAM)))|#

;; from info below
;; (get-artsymnode-info  '(CS HS 1))
;; works= 
;;node-info-string= "CAREFOROTHERS :CV \"0.917\"; \"CARE FOR OTHERS vs SELFISH\"; CS.HS.1 :V \"ARTVALUE\""
;; [if use ~A instead of ~S get= "CAREFOROTHERS :CV 0.917; CARE FOR OTHERS vs SELFISH" :V ARTVALUE"]
;; all-info-string=  "CS.HS.1 CAREFOROTHERS :CSVAL \"0.917\"; \"CARE FOR OTHERS vs SELFISH\";  (CS HS 1) :V \"ARTVALUE\""         
;;csym=  CAREFOROTHERS    csval=     "0.917"        
;;artnodelist=   ((CS HS 1) "CS.HS.1" :C CAREFOROTHERS :V "ARTVALUE" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2")))  




;;TREEVIEWER-GO-CALLBACK
;;2019
;;ddd
(defun treeviewer-go-callback (data interface)
  "U-trees-art-dims Sets options for dim-treeviewer-interface"
    (with-slots (calling-interface radio-button-panel-1 radio-button-panel-2 
                                   radio-button-panel-3 check-button-panel-1) interface
      ;;note:  (capi:choice-selected-item radio-button-panel-1) => item text output
  (let
      ((radio1-selection (capi:choice-selection radio-button-panel-1))
      (radio2-selection (capi:choice-selected-item radio-button-panel-2))
      (radio3-selection (capi:choice-selected-item radio-button-panel-3))
       )
      ;;FOR radio-button-panel-1
    (cond
     ((= radio1-selection 0)
      (setf  *incl-csymlist-in-treeviewer-p NIL   *expanded-node-list-p NIL))
     ((= radio1-selection 1)
      (setf  *incl-csymlist-in-treeviewer-p NIL  *expanded-node-list-p T))
     ((= radio1-selection 2)
      (setf  *incl-csymlist-in-treeviewer-p T   *expanded-node-list-p T
             *dim-treeviewer-frame-width 1300)))
    ;;(break "*incl-csymlist-in-treeviewer-p")
    (mp:process-poke calling-interface)
    (capi:destroy interface)
    (values  radio1-selection radio2-selection radio3-selection  )
    ;;end let, with, let, treeviewer-go-callback
    )))
;;TEST
;; (treeviewer-go-callback



;;MAKE-DIM-TREE-NODES-LIST
;; USED IN DEPRECIATED FUNCTION ABOVE
;; REPLACED BY MAKE-CSARTDIMS-TREE
;;2019
;;ddd
#|(defun make-dim-tree-nodes-list  (csarttree-specs)
  "In U-trees, INPUT any list w/any nesting.  RETURNS (values tree-nodes rootn-list flat-rootn-list root-leveln-list totaln bottom-root-list). Each node (rootnlist rootn value) where rootnlist eg (2 3 4 6) of each level node; rootn eg. \"2.3.4.6\" and key value = valkey  the original item in list OR for a parent node sublistkey (...) = key sublist of child nodes.  If  last-list=value-p, then the last level list is considered a value for the node in level below it. USED FOR CREATING CAPI:LIST-VIEW PANES in its :chidren-function used to create root trees."
  (let
      ((len-tree (list-length tree))
       (bottom-root-list)
       (tree-nodes) 
       (csyms)
       (leveln 0)
       (totaln 0)
       )
    (incf root-leveln)
    (setf root-leveln-list (append root-leveln-list (list root-leveln)))
    (cond
     ((and tree (listp tree))
      (loop
       for item in tree
       for item-n from 1 to len-tree
       do
       (let
           ((itemlist)
            (subtree-parent-node)
            )
       (incf leveln)       
       ;;(incf rootbase)
       (cond
        (parent-rootn
         (setf rootn (format nil "~A.~A" parent-rootn item-n)
               rootnlist (append parent-rootnlist (list  item-n))))
        ;;  itemlist (append itemlist (list (list rootnlist rootn item
        (t (setf  bottom-root-list (append bottom-root-list (list item-n))
                  rootn (format nil "~A" item-n)               
                  rootnlist (list item-n))))
       ;;was  (+ (* item-n rootmult) parent-rootn)
       (setf rootn-list (append rootn-list (list rootn))
             flat-rootn-list (append flat-rootn-list (list rootn))
             rootnlist-list (append rootnlist-list (list rootnlist)))     
       (cond
        ((and tree (listp item))
         (cond
          ;;if want to use the last non-nested list as the value for the parent node
          ((or (null item)
               (and last-list=value-p (null (nested-list-p item))))
           (setf itemlist  (list rootnlist rootn valkey item)
                 tree-nodes (append tree-nodes  (list itemlist))))
          ;;otherwise each item is used as a value for that node
          (t
;;(values tree-nodes rootn-list flat-rootn-list root-leveln-list totaln)
           (multiple-value-bind (subtree-nodes rootn-list1 flat-rootn-list1 
                                                 root-leveln-list1 return-n)
               (make-dim-tree-nodes-list item  ;;:n (- n 1) 
                                       :parent-rootn rootn :parent-rootnlist rootnlist                                    
                                       :root-leveln  root-leveln
                                       :last-list=value-p  last-list=value-p
                                       :tree-num-list tree-num-list)
             ;;add current level info
             (setf subtree-parent-node (list rootnlist rootn sublistkey subtree-nodes)
                   tree-nodes (append tree-nodes  (list subtree-parent-node))
                   rootn-list (append rootn-list (list rootn-list1))
                   flat-rootn-list (append flat-rootn-list flat-rootn-list1)
                   ;;flat-rootnlist-list (append flat-rootnlist-list flat-rootnlist-list1)
                   root-leveln-list (append root-leveln-list (list root-leveln-list1)))
             (setf totaln (+ totaln return-n))
             ;;end mvb, clause
             ))
          ;;end inner cond, listp item
          ))
          ;;FINAL (NON-LIST NODE)
        (t
           (setf itemlist  (list rootnlist rootn valkey item)
                 tree-nodes (append tree-nodes (list itemlist)))
         ;;was (setf tree-nodes (append tree-nodes (list n)))
         ))
       ;;end let, loop, listp clause
       )))
     (t (setf tree-nodes (list tree))))
    (setf tree-num-list (append tree-num-list (list tree-nodes))
          totaln (+ totaln leveln))
    (values tree-nodes rootn-list flat-rootn-list root-leveln-list totaln bottom-root-list)
    ;;end let, make-dim-tree-nodes-list
    ))|#
;;TEST
;;TO MAKE A CSART-TREE FROM ONLY CS MODEL TYPES
;; INPUT SHOULD LOOK LIKE:
;; (((CS HS 2 1) "CS.HS.2.1" :C FLEXIBLE) ((CS HS 2 2) "CS.HS.2.2" :C EXUBERANT) ((CS HS 2 3) "CS.HS.2.3" :C LOVEDANCE))
;; *ALL-CS-DB-ITEM-TYPES = ($TOPBV $WRLDVW $SELF $REFGRP $VALUE  $BELIEF  $KNOWB $COGSK $BEHSK  $SCRIPT $EVENT $ELM) )
;; *ALL-CS-BRAIN-SYSTEMS = '($CS $VERBAL        $IMAGE       $SOUND       $SMELL       $TASTE       $TACTILE       $EMOTION       $MOTOR))
;; SUBTYPES EG.  $COGSK =  '(:SUBTP ($PROBS $DEC $PLAN $LEARN $COPE $CRITTHK)) 





;;GET-LEVEL-DIMLISTS
;;2019
;;ddd
(defun get-level-dimlists (level-csart-tree-list  &key (sublistkey :S) )
  "U-trees-art-dims   RETURNS    INPUT:  "
  (let
      ((level-dimslists)
       )
    (loop
     for sublist in level-csart-tree-list
     do
     (when (and sublist (listp sublist))
       (let*
           ((sublist-p)
            )
         (loop
          for item in sublist
          do
          (let*
              ((dimlist)
               )
            (cond
             ((equal item sublistkey)
              (setf sublist-p T))
             (sublist-p NIL)
             ((and (null sublist-p) (listp item))
              (setf dimlist item
                    level-dimslists (append level-dimslists (list item))
                    sublist-p nil))
             (t (setf sublist-p NIL)))    
            ;;end let,loop
            ))
         ;;end when, let,loop
         )))
     (values  level-dimslists  )
     ;;end let, get-level-dimlists
     ))
;;TEST
;; (get-level-dimlists '(((CS HS) "CS.HS" :S ((((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3")))   ) (   ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2"))))))))


;;wrong output '(((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3")))
;; works= ((CS HS 1) (CS HS 2) (CS HS 3))





;; (make-tree-nodes-list *dim-tree-list)
;; (setf *testartlist22 '(CS (HS (1 (1 (1 ) 2 (1 (1)2 (1 2)) 3 (1 (1 2) 2 3)))) (VB (1 2 (1 (1 2) 2) 3))))
;; (make-tree-nodes-list *TESTARTLIST22)
;; OLD RESULT= (((1) "1" :V= CS) ((2) "2" :SL (((2 1) "2.1" :V= HS) ((2 2) "2.2" :SL (((2 2 1) "2.2.1" :V= 1) ((2 2 2) "2.2.2" :SL (((2 2 2 1) "2.2.2.1" :V= 1) ((2 2 2 2) "2.2.2.2" :SL (((2 2 2 2 1) "2.2.2.2.1" :V= 1))) ((2 2 2 3) "2.2.2.3" :V= 2) ((2 2 2 4) "2.2.2.4" :SL (((2 2 2 4 1) "2.2.2.4.1" :V= 1) ((2 2 2 4 2) "2.2.2.4.2" :SL (((2 2 2 4 2 1) "2.2.2.4.2.1" :V= 1))) ((2 2 2 4 3) "2.2.2.4.3" :V= 2) ((2 2 2 4 4) "2.2.2.4.4" :SL (((2 2 2 4 4 1) "2.2.2.4.4.1" :V= 1) ((2 2 2 4 4 2) "2.2.2.4.4.2" :V= 2))))) ((2 2 2 5) "2.2.2.5" :V= 3) ((2 2 2 6) "2.2.2.6" :SL (((2 2 2 6 1) "2.2.2.6.1" :V= 1) ((2 2 2 6 2) "2.2.2.6.2" :SL (((2 2 2 6 2 1) "2.2.2.6.2.1" :V= 1) ((2 2 2 6 2 2) "2.2.2.6.2.2" :V= 2))) ((2 2 2 6 3) "2.2.2.6.3" :V= 2) ((2 2 2 6 4) "2.2.2.6.4" :V= 3))))))))) ((3) "3" :SL (((3 1) "3.1" :V= VB) ((3 2) "3.2" :SL (((3 2 1) "3.2.1" :V= 1) ((3 2 2) "3.2.2" :V= 2) ((3 2 3) "3.2.3" :SL (((3 2 3 1) "3.2.3.1" :V= 1) ((3 2 3 2) "3.2.3.2" :SL (((3 2 3 2 1) "3.2.3.2.1" :V= 1) ((3 2 3 2 2) "3.2.3.2.2" :V= 2))) ((3 2 3 3) "3.2.3.3" :V= 2))) ((3 2 4) "3.2.4" :V= 3))))))
;; ("1" "2" ("2.1" "2.2" ("2.2.1" "2.2.2" ("2.2.2.1" "2.2.2.2" ("2.2.2.2.1") "2.2.2.3" "2.2.2.4" ("2.2.2.4.1" "2.2.2.4.2" ("2.2.2.4.2.1") "2.2.2.4.3" "2.2.2.4.4" ("2.2.2.4.4.1" "2.2.2.4.4.2")) "2.2.2.5" "2.2.2.6" ("2.2.2.6.1" "2.2.2.6.2" ("2.2.2.6.2.1" "2.2.2.6.2.2") "2.2.2.6.3" "2.2.2.6.4")))) "3" ("3.1" "3.2" ("3.2.1" "3.2.2" "3.2.3" ("3.2.3.1" "3.2.3.2" ("3.2.3.2.1" "3.2.3.2.2") "3.2.3.3") "3.2.4")))
;; ("1" "2" "2.1" "2.2" "2.2.1" "2.2.2" "2.2.2.1" "2.2.2.2" "2.2.2.2.1" "2.2.2.3" "2.2.2.4" "2.2.2.4.1" "2.2.2.4.2" "2.2.2.4.2.1" "2.2.2.4.3" "2.2.2.4.4" "2.2.2.4.4.1" "2.2.2.4.4.2" "2.2.2.5" "2.2.2.6" "2.2.2.6.1" "2.2.2.6.2" "2.2.2.6.2.1" "2.2.2.6.2.2" "2.2.2.6.3" "2.2.2.6.4" "3" "3.1" "3.2" "3.2.1" "3.2.2" "3.2.3" "3.2.3.1" "3.2.3.2" "3.2.3.2.1" "3.2.3.2.2" "3.2.3.3" "3.2.4")
;; (1 (2 (3 (4 (5) (5 (6) (6)) (5 (6))))) (2 (3 (4 (5)))))   38   (1 2 3)


