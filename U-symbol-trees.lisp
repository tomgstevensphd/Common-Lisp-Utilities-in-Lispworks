;;*********************** U-symbol-trees.lisp ***********************
;;
;;THESE ARE USED PRIMARILY FOR GROSSBERG ART NETWORK MODELS
;;
;;THEY MAY BE USEFUL OR ADAPTED TO CS MODELS
;;
;; THESE FUNCTIONS MAKE A SYMBOL TREE FOR EACH SYMBOL
;;
;; SEE U-ART, AND U-TSTRING FOR FUNCTIONS USED BY THIS FILE
;; These functions were developed for use with ART3, but create a powerful database structure that integrates well with math, networks (with nodes and paths, etc);
;;
;; SEE DOCS WITHIN EACH FUNCTION AND TEST EXAMPLES FOR DETAILED INFO ABOUT HOW THESE WORK.
;;
;;ALSO SEE U-sexp.lisp for functions to print a whole tree of any kind.

(setf *out1 nil)

;;  ILFTOILF
;;  IL1TOILF, IL2TOILF;   ILFTOIL1, ILFTOIL2
;;  IL1TOIL1, IL1TOIL2,    IL2TOIL1, IL2TOIL2
;;  I11TOILIL1, I21TOIL1;  IL1TOI11 IL1TOI12 etc.

;;MAKE-PATH-DIMSYM-TREE
;;
;;ddd
(defun make-path-dimsym-tree (rootstr topdims dimspec-lists1 dimspec-lists2 
                                      &key (top-combo '((I L F)(I L F))) default-graph-slot
                                      (node-separator 'TO) (index-syms *art-index-syms) )
  "In U-symbol-trees. Makes path symbols (eg WupI-L-FTOI-L-F) and sets them to symvals (eg (\"Wup\" (I L F TO I L F) NIL NIL subsims).  RETURNS all-labeled-newsyms."
  (let*
      ((n-dims)
       (n-subdims1)
       (n-subdims2)
       (labeled-dims1)
       (labeled-dims2)
       (all-labeled-dimlists)
       (level1)
            (level2) 
            (dimlists)
       ;;combos
       (all-combos)
       (TOP-TOP-combos)(TOP-M-combos)(M-TOP-combos)
       (MM-combos)(MF-combos)(FM-combos)
       (FF-combos)(FL-combos)(LF-combos)
       (LL-combos)(LI-combos)( IL-combos)
       (II-combos) ;;these are the bottom combos eg ((1 2 2 TO 1 1 3)...)
       (rest-combos)
       ;;syms
       (topsym (make-dim-symbol rootstr topdims))
       (labeled-newsyms)
       (all-labeled-newsyms)
       (all-newsyms)       
       (current-level-syms )
       (superord-syms (list topsym))
       (1dim-tree-dims (car top-combo))
       (n-1dims (list-length 1dim-tree-dims))
       ;;(new-syms)
       (class-sym (my-make-symbol rootstr))
       )
    ;;make set class sym to symvals dim-specs and top-sym
    (set class-sym 
         (list rootstr (list dimspec-lists1 'TO dimspec-lists2) default-graph-slot  nil (list topsym)))

    ;;MAKE DIMS TREES FOR THE FROM and TO DIMLISTS
    ;;FOR FROM DIMS
    (setf  labeled-dims1 (make-1dimtree  1dim-tree-dims  n-1dims dimspec-lists1)
           labeled-dims2 (make-1dimtree  1dim-tree-dims  n-1dims  dimspec-lists2))
    (afout 'out (format nil "AT 1: labeled-dims1= ~A~% labeled-dims2= ~A~% " labeled-dims1 labeled-dims2))
    #|AT 1: labeled-dims1= ((F ((I L 2) (I L 3))) (L ((I 3 2) (I 4 2) (I 5 2)) ((I 3 3) (I 4 3) (I 5 3))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2)) ((1 4 2) (2 4 2) (3 4 2) (4 4 2)) ((1 5 2) (2 5 2) (3 5 2) (4 5 2)) ((1 3 3) (2 3 3) (3 3 3) (4 3 3)) ((1 4 3) (2 4 3) (3 4 3) (4 4 3)) ((1 5 3) (2 5 3) (3 5 3) (4 5 3))))
 labeled-dims2= ((F ((I L 3) (I L 4))) (L ((I 1 3) (I 2 3)) ((I 1 4) (I 2 4))) (I ((1 1 3) (2 1 3) (3 1 3)) ((1 2 3) (2 2 3) (3 2 3)) ((1 1 4) (2 1 4) (3 1 4)) ((1 2 4) (2 2 4) (3 2 4))))|#

    ;;MAKE THE 2-DIM COMBOS from all combos of labeled-dims1 X labeled-dims2
    (multiple-value-setq (all-labeled-dimlists all-combos  
                                               TOP-TOP-combos TOP-M-combos M-TOP-combos  
                                               MM-combos MF-combos FM-combos 
                                               FF-combos FL-combos LF-combos 
                                               LL-combos LI-combos IL-combos 
                                               II-combos  rest-combos)
        (make-path-tree-combos labeled-dims1 labeled-dims2 :top-combo top-combo))
    (if (> *print-details 2)  (afout 'out (format nil "AT 2: all-labeled-dimlists= ~A~%" all-labeled-dimlists)))
    #|all-labeled-dimlists= ((TOP TOP (((I L F) (I L F)))) (TOP M NIL) (M TOP NIL) (M M NIL) (M F NIL) (F M NIL) (F F ((((I L 2) (I L 3)) ((I L 2) (I L 4)) ((I L 3) (I L 3)) ((I L 3) (I L 4))))) (F L NIL) (L F NIL) (L L ((((I 3 2) (I 1 3)) ((I 3 2) (I 2 3)) ((I 4 2) (I 1 3)) ((I 4 2) (I 2 3)) ((I 5 2) (I 1 3)) ((I 5 2) (I 2 3))))) (L I NIL) (I L NIL) (I I ((((1 3 2) (1 1 3)) ((1 3 2) (2 1 3)) ((1 3 2) (3 1 3)) ((2 3 2) (1 1 3)) ((2 3 2) (2 1 3)) ((2 3 2) (3 1 3)) ((3 3 2) (1 1 3)) ((3 3 2) (2 1 3)) ((3 3 2) (3 1 3)) ((4 3 2) (1 1 3)) ((4 3 2) (2 1 3)) ((4 3 2) (3 1 3))))) (REST-COMBOS NIL))|#

    ;;CREATE SYMBOLS and SET SUBSYMS FOR EACH LEVEL
    ;;LOOP THRU LEVELS
    (loop
     for labeled-dimlists in all-labeled-dimlists
     do
     (setf  level1 (first labeled-dimlists)
            level2 (second labeled-dimlists)
            dimlists (third labeled-dimlists))
     (if (> *print-details 3)  (afout 'out (format nil "AT 3: level1= ~A level2= ~A dimlists= ~A~% " level1 level2 dimlists)))
     ;;eg level1= L level2= L dimlists= ((((I 3 2) (I 1 3)) ((I 3 2) (I 2 3)) ((I 4 2) (I 1 3)) ((I 4 2) (I 2 3)) ((I 5 2) (I 1 3)) ((I 5 2) (I 2 3))))

     ;;MAKE NEW SYMS and SET TO SUBSYMS OF HIGHER LEVEL 
     ;;since current-level-syms is often nil, use last level not nil
     (when  (and current-level-syms  labeled-dimlists)
       (setf superord-syms current-level-syms))  
 ;;ZZZZ  
     (setf current-level-syms
           (make-artsyms-from-dims rootstr dimlists :superord-artsyms superord-syms)
           all-labeled-newsyms (append all-labeled-newsyms
                                       (list (list level1 level2 current-level-syms)))
           all-newsyms (append all-newsyms (list current-level-syms)))
     ;; (make-artsyms-from-dims  "Test1"  '((((I 4 3) (1 2 4)) ((I 4 3) (2 2 4)) ((I 4 3) (3 2 4)))))
   (if (> *print-details 3)      (afout 'out (format nil "AT 4: superord-syms= ~A~%current-level-syms= ~A~%dimlists= ~A~%all-labeled-newsyms= ~a~%" superord-syms current-level-syms dimlists all-labeled-newsyms)))
     ;;end loop
     )

    (values all-labeled-newsyms all-newsyms)
    ;;end let, make-path-dimsym-tree
    ))
;;TEST
;; (PROGN (SETF OUT NIL)(make-path-dimsym-tree  "Tstx1" '(I L F TO I L F)  '((4 1 1)(3 3 1)(2 2 1))  '((3 1 1)(2 1 1)(2 3 1))))
#|((TOP TOP (TSTX1I-L-FTOI-L-F)) (TOP M NIL) (M TOP NIL) (M M NIL) (M F NIL) (F M NIL) (F F (TSTX1I-L-2TOI-L-3 TSTX1I-L-2TOI-L-4 TSTX1I-L-3TOI-L-3 TSTX1I-L-3TOI-L-4)) (F L NIL) (L F NIL) (L L (TSTX1I-3-2TOI-1-3 TSTX1I-3-2TOI-2-3 TSTX1I-4-2TOI-1-3 TSTX1I-4-2TOI-2-3 TSTX1I-5-2TOI-1-3 TSTX1I-5-2TOI-2-3)) (L I NIL) (I L NIL) (I I (TSTX11-3-2TO1-1-3 TSTX11-3-2TO2-1-3 TSTX11-3-2TO3-1-3 TSTX12-3-2TO1-1-3 TSTX12-3-2TO2-1-3 TSTX12-3-2TO3-1-3 TSTX13-3-2TO1-1-3 TSTX13-3-2TO2-1-3 TSTX13-3-2TO3-1-3 TSTX14-3-2TO1-1-3 TSTX14-3-2TO2-1-3 TSTX14-3-2TO3-1-3)) (REST-COMBOS NIL NIL))
((TSTX1I-L-FTOI-L-F) NIL NIL NIL NIL NIL (TSTX1I-L-2TOI-L-3 TSTX1I-L-2TOI-L-4 TSTX1I-L-3TOI-L-3 TSTX1I-L-3TOI-L-4) NIL NIL (TSTX1I-3-2TOI-1-3 TSTX1I-3-2TOI-2-3 TSTX1I-4-2TOI-1-3 TSTX1I-4-2TOI-2-3 TSTX1I-5-2TOI-1-3 TSTX1I-5-2TOI-2-3) NIL NIL (TSTX11-3-2TO1-1-3 TSTX11-3-2TO2-1-3 TSTX11-3-2TO3-1-3 TSTX12-3-2TO1-1-3 TSTX12-3-2TO2-1-3 TSTX12-3-2TO3-1-3 TSTX13-3-2TO1-1-3 TSTX13-3-2TO2-1-3 TSTX13-3-2TO3-1-3 TSTX14-3-2TO1-1-3 TSTX14-3-2TO2-1-3 TSTX14-3-2TO3-1-3) NIL)
CL-USER 8 > Tstx1
("Tstx1" (((4 1 1) (3 3 1) (2 2 1)) TO ((3 1 1) (2 1 1) (2 3 1))) NIL NIL (TSTX1I-L-FTOI-L-F))
CL-USER 9 > TSTX1I-L-FTOI-L-F
("Tstx1" (I L F TO I L F) NIL NIL ((TSTX1I-L-FTOI-L-F) (TSTX1I-L-2TOI-L-3 TSTX1I-L-2TOI-L-4 TSTX1I-L-3TOI-L-3 TSTX1I-L-3TOI-L-4)))|#
;;
;; (PROGN (SETF OUT NIL)(make-path-dimsym-tree  "Test1" '(I L F TO I L F)  '((4 1 1)(3 3 1)(2 2 1))  '((3 1 1)(2 1 1)(2 3 1))))
;;works=
;; all-labeled-newsyms: (TOP TOP (TEST1I-L-FTOI-L-F) TOP M NIL M TOP NIL M M NIL M F NIL F M NIL F F (TEST1I-L-2TOI-L-3 TEST1I-L-2TOI-L-4 TEST1I-L-3TOI-L-3 TEST1I-L-3TOI-L-4) F L NIL L F NIL L L (TEST1I-3-2TOI-1-3 TEST1I-3-2TOI-2-3 TEST1I-4-2TOI-1-3 TEST1I-4-2TOI-2-3 TEST1I-5-2TOI-1-3 TEST1I-5-2TOI-2-3) L I NIL I L NIL I I (TEST11-3-2TO1-1-3 TEST11-3-2TO2-1-3 TEST11-3-2TO3-1-3 TEST12-3-2TO1-1-3 TEST12-3-2TO2-1-3 TEST12-3-2TO3-1-3 TEST13-3-2TO1-1-3 TEST13-3-2TO2-1-3 TEST13-3-2TO3-1-3 TEST14-3-2TO1-1-3 TEST14-3-2TO2-1-3 TEST14-3-2TO3-1-3) REST-COMBOS NIL NIL)
;; all-newsyms:  ((TEST1I-L-FTOI-L-F) NIL NIL NIL NIL NIL (TEST1I-L-2TOI-L-3 TEST1I-L-2TOI-L-4 TEST1I-L-3TOI-L-3 TEST1I-L-3TOI-L-4) NIL NIL (TEST1I-3-2TOI-1-3 TEST1I-3-2TOI-2-3 TEST1I-4-2TOI-1-3 TEST1I-4-2TOI-2-3 TEST1I-5-2TOI-1-3 TEST1I-5-2TOI-2-3) NIL NIL (TEST11-3-2TO1-1-3 TEST11-3-2TO2-1-3 TEST11-3-2TO3-1-3 TEST12-3-2TO1-1-3 TEST12-3-2TO2-1-3 TEST12-3-2TO3-1-3 TEST13-3-2TO1-1-3 TEST13-3-2TO2-1-3 TEST13-3-2TO3-1-3 TEST14-3-2TO1-1-3 TEST14-3-2TO2-1-3 TEST14-3-2TO3-1-3) NIL)
#| PPRINT all-labeled-newsyms:
(TOP  TOP:  
      (TEST1F-L-ITOF-L-I) = ("Test1" (I L F TO I L F) NIL NIL ((TEST1I-L-FTOI-L-F) (TEST1I-L-2TOI-L-3 TEST1I-L-2TOI-L-4 TEST1I-L-3TOI-L-3 TEST1I-L-3TOI-L-4)))
 TOP  M  NIL
 M  TOP  NIL
 M M  NIL
 M F  NIL
 F M  NIL
 F F:
  (TEST1I-L-2TOI-L-3 
   TEST1I-L-2TOI-L-4 = ("Test1" (I L 2 TO I L 4) NIL NIL ((TEST1I-3-2TOI-1-3 TEST1I-3-2TOI-2-3 TEST1I-4-2TOI-1-3 TEST1I-4-2TOI-2-3 TEST1I-5-2TOI-1-3 TEST1I-5-2TOI-2-3)))
   TEST1I-L-3TOI-L-3 = ("Test1" (I L 3 TO I L 3) NIL NIL ((TEST1I-3-2TOI-1-3 TEST1I-3-2TOI-2-3 TEST1I-4-2TOI-1-3 TEST1I-4-2TOI-2-3 TEST1I-5-2TOI-1-3 TEST1I-5-2TOI-2-3)))
   TEST1I-L-3TOI-L-4)
 F L  NIL
 L F  NIL
 L L: 
 (TEST1I-3-2TOI-1-3
  TEST1I-3-2TOI-2-3
  TEST1I-4-2TOI-1-3 = ("Test1" (I 4 2 TO I 1 3) NIL NIL ((TEST11-3-2TO1-1-3 TEST11-3-2TO2-1-3 TEST11-3-2TO3-1-3 TEST12-3-2TO1-1-3 TEST12-3-2TO2-1-3 TEST12-3-2TO3-1-3 TEST13-3-2TO1-1-3 TEST13-3-2TO2-1-3 TEST13-3-2TO3-1-3 TEST14-3-2TO1-1-3 TEST14-3-2TO2-1-3 TEST14-3-2TO3-1-3)))
  TEST1I-4-2TOI-2-3
  TEST1I-5-2TOI-1-3
  TEST1I-5-2TOI-2-3)
 L I NIL
 I L  NIL
 I I:
 (TEST11-3-2TO1-1-3
  TEST11-3-2TO2-1-3
  TEST11-3-2TO3-1-3 = ("Test1" (1 3 2 TO 3 1 3) NIL NIL)
  TEST12-3-2TO1-1-3
  TEST12-3-2TO2-1-3
  TEST12-3-2TO3-1-3
  TEST13-3-2TO1-1-3
  TEST13-3-2TO2-1-3 = ("Test1" (3 3 2 TO 2 1 3) NIL NIL)
  TEST13-3-2TO3-1-3
  TEST14-3-2TO1-1-3
  TEST14-3-2TO2-1-3
  TEST14-3-2TO3-1-3)
 REST-COMBOS:  NIL
 NIL)|#






;;MAKE-PATH-TREE-COMBOS
;;
;;ddd
(defun make-path-tree-combos (labeled-list1 labeled-list2 
                                            &key (top-combo '((I L F)(I L F))))
  "In U-symbol-trees. RETURNS (values new-labeled-combos   new-combos MM-combos MF-combos FM-combos FF-combos FL-combos LF-combos LL-combos LI-combos IL-combos II-combos IB-combos BI-combos BB-combos rest)"
  (let*
      ((label1)
       (label2)
       (group1)
       (group2)
       ;;(combos1)
       ;;(combos2)
       (new-combos  (list top-combo))
       (new-labeled-combos `((TOP TOP ,top-combo )))
       (all-combos (list (list top-combo) ))
       (TOP-TOP-combos (list top-combo ))
       (TOP-M-combos)
       (M-top-combos)
       (MM-combos)
       (MF-combos)
       (FM-combos)
       (FF-combos)
       (FL-combos)
       (LF-combos)
       (LL-combos)
       (LI-combos)
       (IL-combos)
       (II-combos)
       (rest-combos)  ;;NOTE:  Above only include 1 level dif. eg ML, MI, MB etc not included.
       ;;(higher-combos '(within-level-combos betw-2-level-combos betw-3-level-combos))
       ( labeled-combos-by-level)
       )
    (loop
     for combo-group1 in labeled-list1 ;;(I L F M)
     for combo-group2 in labeled-list2
     do
     (setf label1 (car combo-group1)
           group1 (second combo-group1)
           label2 (car combo-group2)
           group2 (second combo-group2))

     (setf new-combos (make-possible-2-list-combos group1 group2) ;;eg ( ((I L 2)(I L 3))..)
           new-labeled-combos (append new-labeled-combos
                                        (list label1 label2 new-combos));;eg (L (((I L 2)(I L 3)) ...))
           all-combos (append all-combos (list new-combos)))

     ;;(afout 'out (format nil "label1= ~A label2= ~A~% group1= ~A~% group2= ~A~% new-labeled-combos= ~A~%" label1 label2 group1 group2 new-labeled-combos))


     ;;make lists for returning values
     (cond
      ((and (equal label1 'TOP)(equal label2 'TOP))
       (setf TOP-TOP-combos (append TOP-TOP-combos  new-combos)))
      ((and (equal label1 'TOP)(equal label2 'M))
       (setf TOP-M-combos (append TOP-M-combos  new-combos)))
      ((and (equal label1 'M)(equal label2 'TOP))
       (setf M-TOP-combos (append M-TOP-combos  new-combos)))
      ((and (equal label1 'M)(equal label2 'M))
       (setf MM-combos (append MM-combos  new-combos)))
      ((and (equal label1 'M)(equal label2 'F))
       (setf MF-combos (append MF-combos  new-combos)))
      ((and (equal label1 'F)(equal label2 'M))
       (setf FM-combos (append FM-combos  new-combos)))
      ((and (equal label1 'F)(equal label2 'F))
       (setf FF-combos (append FF-combos  new-combos)))
      ((and (equal label1 'F)(equal label2 'L))
       (setf FL-combos (append FL-combos  new-combos)))
      ((and (equal label1 'L)(equal label2 'F))
       (setf LF-combos (append LF-combos  new-combos)))
      ((and (equal label1 'L)(equal label2 'L))
       (setf LL-combos (append LL-combos  new-combos)))
      ((and (equal label1 'L)(equal label2 'I))
       (setf LI-combos (append LI-combos  new-combos)))
      ((and (equal label1 'I)(equal label2 'L))
       (setf IL-combos (append IL-combos  new-combos)))
      ((and (equal label1 'I)(equal label2 'I))
       (setf II-combos (append II-combos  new-combos)))
      (t 
       (setf rest-combos (append rest-combos  new-combos))))

      ;;end combo-groups loop
      )
    (setf labeled-combos-by-level `((TOP TOP ,TOP-TOP-combos)(Top M ,top-m-combos)(M TOP ,m-top-combos)(M M ,MM-combos)(M F ,MF-combos)(F M ,FM-combos)(F F ,FF-combos)(F L ,FL-combos) (L F ,LF-combos)(L L ,LL-combos)(L I ,LI-combos)(I L ,IL-combos) (I I ,II-combos)(rest-combos ,rest-combos)))

    (values labeled-combos-by-level   all-combos 
             TOP-TOP-combos   TOP-M-combos 
            M-top-combos  MM-combos MF-combos FM-combos FF-combos FL-combos LF-combos LL-combos LI-combos IL-combos II-combos  rest-combos) 
   ;;end let, make-path-tree-combos
   ))
;;TEST
;; (make-path-tree-combos  '((F ((I L 2) (I L 3))) (L ((I 3 2) (I 4 2) (I 5 2)) ((I 3 3) (I 4 3) (I 5 3))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2)) ((1 4 2) (2 4 2) (3 4 2) (4 4 2)) ((1 5 2) (2 5 2) (3 5 2) (4 5 2)) ((1 3 3) (2 3 3) (3 3 3) (4 3 3)) ((1 4 3) (2 4 3) (3 4 3) (4 4 3)) ((1 5 3) (2 5 3) (3 5 3) (4 5 3))))       '((F ((I L 3) (I L 4))) (L ((I 1 3) (I 2 3)) ((I 1 4) (I 2 4))) (I ((1 1 3) (2 1 3) (3 1 3)) ((1 2 3) (2 2 3) (3 2 3)) ((1 1 4) (2 1 4) (3 1 4)) ((1 2 4) (2 2 4) (3 2 4)))))
;;WORKS=
;; labeled-combos-by-level:  ((TOP TOP (((I L F) (I L F))) (TOP M NIL) (M TOP NIL) (M M NIL) (M F NIL) (F M NIL) (F F (((I L 2) (I L 3)) ((I L 2) (I L 4)) ((I L 3) (I L 3)) ((I L 3) (I L 4)))) (F L NIL) (L F NIL) (L L (((I 3 2) (I 1 3)) ((I 3 2) (I 2 3)) ((I 4 2) (I 1 3)) ((I 4 2) (I 2 3)) ((I 5 2) (I 1 3)) ((I 5 2) (I 2 3)))) (L I NIL) (I L NIL) (I I (((1 3 2) (1 1 3)) ((1 3 2) (2 1 3)) ((1 3 2) (3 1 3)) ((2 3 2) (1 1 3)) ((2 3 2) (2 1 3)) ((2 3 2) (3 1 3)) ((3 3 2) (1 1 3)) ((3 3 2) (2 1 3)) ((3 3 2) (3 1 3)) ((4 3 2) (1 1 3)) ((4 3 2) (2 1 3)) ((4 3 2) (3 1 3)))) (REST-COMBOS NIL))
;; labeled-combos-by-level: (((I L F) (I L F)) (((I L 2) (I L 3)) ((I L 2) (I L 4)) ((I L 3) (I L 3)) ((I L 3) (I L 4))) (((I 3 2) (I 1 3)) ((I 3 2) (I 2 3)) ((I 4 2) (I 1 3)) ((I 4 2) (I 2 3)) ((I 5 2) (I 1 3)) ((I 5 2) (I 2 3))) (((1 3 2) (1 1 3)) ((1 3 2) (2 1 3)) ((1 3 2) (3 1 3)) ((2 3 2) (1 1 3)) ((2 3 2) (2 1 3)) ((2 3 2) (3 1 3)) ((3 3 2) (1 1 3)) ((3 3 2) (2 1 3)) ((3 3 2) (3 1 3)) ((4 3 2) (1 1 3)) ((4 3 2) (2 1 3)) ((4 3 2) (3 1 3))))
;;VALUES BY LEVEL:  (((I L F) (I L F)))  NIL  NIL NIL NIL NIL (((I L 2) (I L 3)) ((I L 2) (I L 4)) ((I L 3) (I L 3)) ((I L 3) (I L 4))) NIL NIL (((I 3 2) (I 1 3)) ((I 3 2) (I 2 3)) ((I 4 2) (I 1 3)) ((I 4 2) (I 2 3)) ((I 5 2) (I 1 3)) ((I 5 2) (I 2 3))) NIL NIL (((1 3 2) (1 1 3)) ((1 3 2) (2 1 3)) ((1 3 2) (3 1 3)) ((2 3 2) (1 1 3)) ((2 3 2) (2 1 3)) ((2 3 2) (3 1 3)) ((3 3 2) (1 1 3)) ((3 3 2) (2 1 3)) ((3 3 2) (3 1 3)) ((4 3 2) (1 1 3)) ((4 3 2) (2 1 3)) ((4 3 2) (3 1 3))) NIL
#|;;PPRINTED LABELED
((TOP TOP (((I L F) (I L F))))
 (TOP M NIL)
 (M TOP NIL)
 (M M NIL)
 (M F NIL)
 (F M NIL)
 (F F (((I L 2) (I L 3)) ((I L 2) (I L 4)) ((I L 3) (I L 3)) ((I L 3) (I L 4))))
 (F L NIL)
 (L F NIL)
 (L L
  (((I 3 2) (I 1 3))
   ((I 3 2) (I 2 3))
   ((I 4 2) (I 1 3))
   ((I 4 2) (I 2 3))
   ((I 5 2) (I 1 3))
   ((I 5 2) (I 2 3))))
 (L I NIL)
 (I L NIL)
 (I I
  (((1 3 2) (1 1 3))
   ((1 3 2) (2 1 3))
   ((1 3 2) (3 1 3))
   ((2 3 2) (1 1 3))
   ((2 3 2) (2 1 3))
   ((2 3 2) (3 1 3))
   ((3 3 2) (1 1 3))
   ((3 3 2) (2 1 3))
   ((3 3 2) (3 1 3))
   ((4 3 2) (1 1 3))
   ((4 3 2) (2 1 3))
   ((4 3 2) (3 1 3))))
 (REST-COMBOS NIL))|#

;;INPUTS ARE FROM
;;For FROM DIMS from make-1dimtree
;; (progn (setf out1 nil)(make-1dimtree  '(I L F)  3  '((4 1 1)(3 3 1)(2 2 1)))) 
;; LABELED= ((M ((I L F))) (F ((I L 2))) (L ((I 3 2))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2))) (L ((I 4 2))) (I ((1 4 2) (2 4 2) (3 4 2) (4 4 2))) (L ((I 3 2) (I 4 2) (I 5 2))) (L ((I 5 2))) (I ((1 5 2) (2 5 2) (3 5 2) (4 5 2))) (F ((I L 2) (I L 3))) (F ((I L 3))) (L ((I 3 3))) (I ((1 3 3) (2 3 3) (3 3 3) (4 3 3))) (L ((I 4 3))) (I ((1 4 3) (2 4 3) (3 4 3) (4 4 3))) (L ((I 3 3) (I 4 3) (I 5 3))) (L ((I 5 3))) (I ((1 5 3) (2 5 3) (3 5 3) (4 5 3))))
;;For TO DIMS from make-1dimtree
;; ;; (progn (setf out1 nil)(make-1dimtree  '(I L F)  3  '((3 1 1)(2 1 1)(2 3 1)))) 
;;LABELED= ((M ((I L F))) (F ((I L 3))) (L ((I 1 3))) (I ((1 1 3) (2 1 3) (3 1 3))) (L ((I 1 3) (I 2 3))) (L ((I 2 3))) (I ((1 2 3) (2 2 3) (3 2 3))) (F ((I L 3) (I L 4))) (F ((I L 4))) (L ((I 1 4))) (I ((1 1 4) (2 1 4) (3 1 4))) (L ((I 1 4) (I 2 4))) (L ((I 2 4))) (I ((1 2 4) (2 2 4) (3 2 4))))
 
        





;;MAKE-1DIMTREE  
;;
;;ddd
(defun make-1dimtree (dims target-dim-n dimspecs) 
  "Makes only dimlist trees--not symbols. RETURNS  (values  labeled-all-newdims-by-level all-newdims). Labels are index syms in dims. eg (I ((I 2 3)...)).  NOTE that the bottom level I dimlists are grouped by their level L. If group-by-subord-sym-p, groups dims in labeled-all-newdims by subord level sym"
  (let*
      ((target-dim-nth (- target-dim-n 1))
       (index (nth target-dim-nth dims))
       (dimspec (nth target-dim-nth dimspecs))
       (n-indexs (first dimspec))
       (begin-index (second dimspec))
       (incr (third dimspec))
       (new-index begin-index)
       (newdims)
       ;; (newdims1)
       (all-newdims)  ;;below (list (list dims)))
       (all-newdims1)
       (sub-newdims)
       (labeled-all-newdims) ;;below (list (list 'TOP  dims)))
       (indexs)
       (sup-label (find-super-subord-index index))
       (label)
       (labeled-all-newdims-by-level)
       (labeled-all-newdims-by-level1)( labeled-all-newdims1)( all-newdims1 )
       ) 

    (cond
      ;;PROCESS THE SUPERORDINATE LETTER INDEX
     ((not (integerp index))

      ;;LOOP THRU EACH SUBORDINATE INTEGER INDEX
      (setf  label index)
      (loop
       For n from 1 to n-indexs 
       do                 ;;eg. newdims= (I L 2)      ;;eg ((I L 1)(I L 2))    
       (setf newdims (replace-nth dims target-dim-nth  new-index) ;;starts w begin-index
            sub-newdims (append sub-newdims (list newdims))) 
          ;;(afout 'out1 (format nil "AT LOOP BEGIN,~%  newdims= ~A~% sub-newdims= ~A~%" newdims sub-newdims))

         ;;RECURSE ON EACH NEW NEWDIMS, going to next less dim eg (I L 2)
         (when (> target-dim-nth 0)
           ;;(afout 'out1 (format nil "IN LOOP, WHEN target-dim-nth > 0; RECURSE~%on  newdims= ~A  (- target-dim-n=~A 1) " newdims target-dim-n))
           (multiple-value-setq 
               (labeled-all-newdims-by-level1 labeled-all-newdims1 all-newdims1 )
               (make-1dimtree  newdims  (- target-dim-n 1)  dimspecs))

           (setf all-newdims (append all-newdims (list all-newdims1))
                 ;;labeled-all-newdims labeled-all-newdims1)
                 labeled-all-newdims (append labeled-all-newdims  labeled-all-newdims1))
           ;;(afout 'out1 (format nil "AT 2 AFTER RECURSE: newdims= ~A target-dim-nth= ~A ~%labeled-all-newdims-by-level= ~A labeled-all-newdims-by-level1= ~a~%all-newdims= ~A~%all-newdims1= ~A~%labeled-all-newdims= ~a~%" newdims target-dim-nth labeled-all-newdims-by-level labeled-all-newdims-by-level1 labeled-all-newdims all-newdims all-newdims1))
           ;;end when
           )
       ;;RE-INITIATE
       (setf new-index (+ new-index incr))
       ;;end loop n-indexs
       )
       ;;WHEN INDEXES LOOP COMPLETE, ADD TO OVERALL LISTS
      ;;eg (((I L F))((I L 1)..) eg. ((M((I L F)))(L ((I L 1)(I L 2))))
      (setf all-newdims (append all-newdims (list sub-newdims))
            labeled-all-newdims 
            (append labeled-all-newdims (list (list label sub-newdims))))
      ;;(afout 'out1 (format nil "IN LOOP, WHEN N= N-NINDEXES:~% label= ~A newdims= ~A target-dim-nth= ~A ~%labeled-all-newdims= ~A~%all-newdims1= ~A~%" label newdims target-dim-nth  labeled-all-newdims all-newdims1))      
      ;;end not integerp
      )
     ((< target-dim-nth 0)
      NIL)
     (t nil ))
   ;;(afout 'out1 (format nil "AT 3:labeled-all-newdims-by-level= ~A~%labeled-all-newdims= ~a~%"labeled-all-newdims-by-level labeled-all-newdims))

    ;;GROUP ALL NEWDIMS BY LEVELS
    (when labeled-all-newdims
      (setf labeled-all-newdims-by-level (sort-keylists-by-begin   labeled-all-newdims :sort-within-groups 'descending)))
;; eg  (((M ((I L F))) (F ((I L 2)) ((I L 3))) (L ((I 3 2)) ((I 4 2)) ((I 5 2)) ((I 3 3)) ((I 4 3)) ((I 5 3))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2)) ((1 4 2) (2 4 2) (3 4 2) (4 4 2)) ((1 5 2) (2 5 2) (3 5 2) (4 5 2)) ((1 3 3) (2 3 3) (3 3 3) (4 3 3)) ((1 4 3) (2 4 3) (3 4 3) (4 4 3)) ((1 5 3) (2 5 3) (3 5 3) (4 5 3)))))
     ;;PUT IN FIRST VALUE labeled-all-newdims-by-level
    (values labeled-all-newdims-by-level  labeled-all-newdims all-newdims)
    ;;end let, make-1dimtree
    ))
;;TEST
;;(PROGN (SETF OUT1 NIL) (make-1dimtree  '(I L F) 3 '((4 1 1)(3 3 1)(2 2 1))))
;;ALL GROUPED BY LEVEL: (((F ((I L 2) (I L 3))) (L ((I 3 2) (I 4 2) (I 5 2)) ((I 3 3) (I 4 3) (I 5 3))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2)) ((1 4 2) (2 4 2) (3 4 2) (4 4 2)) ((1 5 2) (2 5 2) (3 5 2) (4 5 2)) ((1 3 3) (2 3 3) (3 3 3) (4 3 3)) ((1 4 3) (2 4 3) (3 4 3) (4 4 3)) ((1 5 3) (2 5 3) (3 5 3) (4 5 3)))))
;; INDIVIDUAL GROUPS: ((I ((1 3 2) (2 3 2) (3 3 2) (4 3 2))) (I ((1 4 2) (2 4 2) (3 4 2) (4 4 2))) (I ((1 5 2) (2 5 2) (3 5 2) (4 5 2))) (L ((I 3 2) (I 4 2) (I 5 2))) (I ((1 3 3) (2 3 3) (3 3 3) (4 3 3))) (I ((1 4 3) (2 4 3) (3 4 3) (4 4 3))) (I ((1 5 3) (2 5 3) (3 5 3) (4 5 3))) (L ((I 3 3) (I 4 3) (I 5 3))) (F ((I L 2) (I L 3))))
;;ALL RAW DIMS: (((((1 3 2) (2 3 2) (3 3 2) (4 3 2))) (((1 4 2) (2 4 2) (3 4 2) (4 4 2))) (((1 5 2) (2 5 2) (3 5 2) (4 5 2))) ((I 3 2) (I 4 2) (I 5 2))) ((((1 3 3) (2 3 3) (3 3 3) (4 3 3))) (((1 4 3) (2 4 3) (3 4 3) (4 4 3))) (((1 5 3) (2 5 3) (3 5 3) (4 5 3))) ((I 3 3) (I 4 3) (I 5 3))) ((I L 2) (I L 3)))
;;PPRINTED ALL GROUPED BY LEVEL
#|(((F ((I L 2) (I L 3)))
  (L ((I 3 2) (I 4 2) (I 5 2)) ((I 3 3) (I 4 3) (I 5 3)))
  (I
   ((1 3 2) (2 3 2) (3 3 2) (4 3 2))
   ((1 4 2) (2 4 2) (3 4 2) (4 4 2))
   ((1 5 2) (2 5 2) (3 5 2) (4 5 2))
   ((1 3 3) (2 3 3) (3 3 3) (4 3 3))
   ((1 4 3) (2 4 3) (3 4 3) (4 4 3))
   ((1 5 3) (2 5 3) (3 5 3) (4 5 3)))))|#









;;SORT-DIM-LEVELS-FROM-DIMS
;;
;;ddd
(defun sort-dim-levels-from-dims (dimlists)  
  "In U-symbol-trees. RETURNS (values  level-M level-F level-L level-I  level-nested-dimlists) from dimlists which may be nested or flat.  Works well with make-1dimtree."
  (let
      ((top-level)
       (level-M)
       (level-F) 
       (level-L) 
       (level-I)
       (bottom-level)
       (level-vals)
       (flat-dimlists (flatten-list-tree dimlists))
       (level-nested-dimlists)
       )

   ;;(1 2 3) (I 1 2) (I L 1)(I L F) 
   (loop
    for dims in flat-dimlists
    do
     (cond
      ((member 'M dims)
       (setf level-M (append level (list dims))
             top-level '(I L F M)))
      ((member 'F dims)
       (unless top-level  (setf top-level '(I L F)))
       (unless (member dims level-F :test 'equal)
         (setf level-F (append level-F (list dims)))))
      ((member 'L dims)
       (unless top-level  (setf top-level '(I L)))   
       (unless (member dims level-L :test 'equal)
         (setf level-L (append level-L (list dims)))))
      ((member 'I dims)
       (unless top-level  (setf top-level '(I))) 
       (unless (member dims level-I :test 'equal)  
         (setf level-I (append level-I (list dims)))))  
      ((integers-list-p dims)
       (setf bottom-level (append bottom-level (list dims))))
      (t nil))
     ;;end loop
     )
    (setf  level-nested-dimlists  
           `((top-level ,top-level)(level-m ,level-m)(level-f ,level-f)
             (level-l ,level-l)(level-i ,level-i) (bottom-level ,bottom-level)))
    ;;end loop
;;    )         
    (values top-level level-M level-F level-L level-I bottom-level  level-nested-dimlists)
    ;;end let, sort-dim-levels-from-dims
    ))
;;TEST
;;(sort-dim-levels-from-dims  '(((I L F)) ((I L 2)) ((I 3 2)) ((1 3 2) (2 3 2) (3 3 2) (4 3 2)) ((I 4 2)) ((1 4 2) (2 4 2) (3 4 2) (4 4 2)) ((I 3 2) (I 4 2) (I 5 2)) ((I 5 2)) ((1 5 2) (2 5 2) (3 5 2) (4 5 2)) ((I L 2) (I L 3)) ((I L 3)) ((I 3 3)) ((1 3 3) (2 3 3) (3 3 3) (4 3 3)) ((I 4 3)) ((1 4 3) (2 4 3) (3 4 3) (4 4 3)) ((I 3 3) (I 4 3) (I 5 3)) ((I 5 3)) ((1 5 3) (2 5 3) (3 5 3) (4 5 3))))
;;TOP (I L F)
;;M NIL
;;F ((I L F))
;;L ((I L 2) (I L 3))
;;I  ((I 3 2) (I 4 2) (I 5 2) (I 3 3) (I 4 3) (I 5 3))
;;BOTTOM
;;((1 3 2) (2 3 2) (3 3 2) (4 3 2)
;; (1 4 2) (2 4 2) (3 4 2) (4 4 2)
;; (1 5 2) (2 5 2) (3 5 2) (4 5 2) 
;; (1 3 3) (2 3 3) (3 3 3) (4 3 3) 
;; (1 4 3) (2 4 3) (3 4 3) (4 4 3) 
;; (1 5 3) (2 5 3) (3 5 3) (4 5 3))

;;
;((TOP-LEVEL (I L F)) 
;;(LEVEL-M NIL) 
;;(LEVEL-F ((I L F))) 
;;(LEVEL-L ((I L 2) (I L 3)))
;;(LEVEL-I ((I 3 2) (I 4 2) (I 5 2) (I 3 3) (I 4 3) (I 5 3))) 
;;(BOTTOM-LEVEL ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (1 4 2) (2 4 2) (3 4 2) (4 4 2) (1 5 2) (2 5 2) (3 5 2) (4 5 2) (1 3 3) (2 3 3) (3 3 3) (4 3 3) (1 4 3) (2 4 3) (3 4 3) (4 4 3) (1 5 3) (2 5 3) (3 5 3) (4 5 3))))

;;WHEN DUPLICATES NOT ELIMINATED RESULTS=
;;TOP (I L F)
;;M NIL
;;F ((I L F))
;;L ((I L 2)    (I L 2) (I L 3) (I L 3))
;;I ((I 3 2) (I 4 2)    (I 3 2) (I 4 2)    (I 5 2) (I 5 2) 
;;   (I 3 3) (I 4 3) (I 3 3) (I 4 3) (I 5 3) (I 5 3))
;;BOTTOM  
#|((1 3 2) (2 3 2) (3 3 2) (4 3 2) 
(1 4 2) (2 4 2) (3 4 2) (4 4 2) 
(1 5 2) (2 5 2) (3 5 2) (4 5 2) 
(1 3 3) (2 3 3) (3 3 3) (4 3 3) 
(1 4 3) (2 4 3) (3 4 3) (4 4 3) 
(1 5 3) (2 5 3) (3 5 3) (4 5 3))
((TOP-LEVEL (I L F)) (LEVEL-M NIL) (LEVEL-F ((I L F))) (LEVEL-L ((I L 2) (I L 2) (I L 3) (I L 3))) (LEVEL-I ((I 3 2) (I 4 2) (I 3 2) (I 4 2) (I 5 2) (I 5 2) (I 3 3) (I 4 3) (I 3 3) (I 4 3) (I 5 3) (I 5 3))) (BOTTOM-LEVEL ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (1 4 2) (2 4 2) (3 4 2) (4 4 2) (1 5 2) (2 5 2) (3 5 2) (4 5 2) (1 3 3) (2 3 3) (3 3 3) (4 3 3) (1 4 3) (2 4 3) (3 4 3) (4 4 3) (1 5 3) (2 5 3) (3 5 3) (4 5 3))))|#

;;(sort-dim-levels-from-dims '(((I L F)) ((I L 2)) ((I 1 2)) ((1 1 2) (2 1 2) (3 1 2) (4 1 2)) ((I 1 2) (I 2 2)) ((I 2 2)) ((1 2 2) (2 2 2) (3 2 2) (4 2 2)) ((I L 2) (I L 3)) ((I L 3)) ((I 1 3)) ((1 1 3) (2 1 3) (3 1 3) (4 1 3)) ((I 1 3) (I 2 3)) ((I 2 3)) ((1 2 3) (2 2 3) (3 2 3) (4 2 3))))
;;RESULTS=








;;SORT-DIMLISTS-BY-LEVEL
;; eg. ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (1 4 2) (2 4 2) (3 4 2))
;;ddd

(defun sort-dimlists-by-level (level dimlists &key (sort-dim-n 1)
                                     (sort-groups 'ascending))
  "In U-symbol-trees groups dimlists by same dim-n number or letter. RETURNS (values ascending-lists descending-lists ). level can be a dim-n (1-4) or  level I-M.  sort-groups-p causes groups to be sorted either 'ascending or 'descending by sort-dim-n."
  (let
      ((labels-list)
       (grouped-dimlists)
       (nth) 
       )
    (cond
     ((setf nth (find-list-element-n level *art-index-syms)) NIL)
     (t (setf nth (- level 1))))

    (multiple-value-setq (grouped-dimlists labels-list)
        (sort-keylists-by-key nth dimlists  :sort-dim-n sort-dim-n
                                     :sort-groups sort-groups))


      (values grouped-dimlists  labels-list)
    ;;end let, sort-dimlists-by-level
    )) 
;;TEST
;; (sort-dimlists-by-level 2 '((2 3 2) (2 4 2)(3 3 2) (4 3 2) (1 4 2) (1 3 2) (3 4 2)))
;;works= (((1 3 2) (2 3 2) (3 3 2) (4 3 2)) ((1 4 2) (2 4 2) (3 4 2)))  (3 4 3 3 4 3 4)

;; (sort-dimlists-by-level 2 '((1 3 2) (2 3 2) (3 3 2) (4 3 2) (1 4 2) (2 4 2) (3 4 2)(4 4 2) (1 5 2) (2 5 2) (3 5 2) (4 5 2) (1 3 3) (2 3 3) (3 3 3) (4 3 3) (1 4 3) (2 4 3) (3 4 3) (4 4 3) (1 5 3) (2 5 3) (3 5 3) (4 5 3)))
;;RESULTS= (((1 3 3) (1 3 2) (2 3 3) (2 3 2) (3 3 3) (3 3 2) (4 3 3) (4 3 2)) ((1 4 3) (1 4 2) (2 4 3) (2 4 2) (3 4 3) (3 4 2) (4 4 3) (4 4 2)) ((1 5 3) (1 5 2) (2 5 3) (2 5 2) (3 5 3) (3 5 2) (4 5 3) (4 5 2)))   (3 3 3 3 4 4 4 4 5 5 5 5 3 3 3 3 4 4 4 4 5 5 5 5)
;;NIL
;; (sort-dimlists-by-level 1 '((L (1 2 3))(F (3 4 5))(L (6 78))))
;; works= (((L (6 78)) (L (1 2 3))) ((F (3 4 5))))   (L F L)




;;SORT-DIMLISTS-BY-DIM
;;
;;ddd
(defun SORT-DIMLISTS-BY-DIM (dim-n dimlists);; &key (all-labels *art-index-syms))
  "In U-symbol-trees sorts dimlists by dim-n. RETURNS (values ascending-lists descending-lists ). Does NOT GROUP BY DIMS--use sort-dimlists-by-level."
  (let
      ((ascending-lists)
       (descending-lists)  
       (n-dims (list-length (car dimlists)))
       ;;(labels (butlast all-labels (- 
       )
    (multiple-value-setq (descending-lists ascending-lists)
        (my-sort-lists (- dim-n 1) dimlists :ascending-p T))
    (values ascending-lists descending-lists )
    ;;end let, sort-dimlists
    ))
;;TEST
;; (sort-dimlists 2 '((1 3 2) (2 3 2) (3 3 2) (4 3 2) (1 4 2) (2 4 2) (3 4 2) (4 4 2) (1 5 2) (2 5 2) (3 5 2) (4 5 2) (1 3 3) (2 3 3) (3 3 3) (4 3 3) (1 4 3) (2 4 3) (3 4 3) (4 4 3) (1 5 3) (2 5 3) (3 5 3) (4 5 3)))



;;TEST
;; (sort- 2 '((1 3 2) (2 3 2) (3 3 2) (4 3 2) (1 4 2) (2 4 2) (3 4 2) (4 4 2) (1 5 2) (2 5 2) (3 5 2) (4 5 2) (1 3 3) (2 3 3) (3 3 3) (4 3 3) (1 4 3) (2 4 3) (3 4 3) (4 4 3) (1 5 3) (2 5 3) (3 5 3) (4 5 3)))




;;OLD
;;(sort-dim-levels-from-dims '(((I L F))((I L 2)(I L 3)) ((I 3 2)(I 1 3)(I 2 3)) ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2)   (1 1 3) (2 1 3) (3 1 3)  (1 2 3)(2 2 3)(3 2 3)(4 2 3) )))
;;TOP= (I L F)
;;M= NIL
;;F=((I L F))
;;L= ((I L 2) (I L 3))
;;I= ((I 3 2) (I 1 3))
;;BOTTOM= ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2)   (1 1 3) (2 1 3) (3 1 3)   (1 2 3) (2 2 3) (3 2 3) (4 2 3))
;;LIST= ((TOP-LEVEL (I L F)) (LEVEL-M NIL) (LEVEL-F ((I L F))) (LEVEL-L ((I L 2) (I L 3))) (LEVEL-I ((I 3 2) (I 1 3) (I 2 3))) (BOTTOM-LEVEL ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2) (1 1 3) (2 1 3) (3 1 3) (1 2 3) (2 2 3) (3 2 3) (4 2 3))))

;;  (sort-dim-levels-from-dims '(((I L 2)) ((I 3 2)) ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2))))
;; works= 
;;TOP-LEVEL= (I L)
;;M= NIL
;;F=  NIL
;;L= ((I L 2)) 
;;I=  ((I 3 2)) 
;;BOTTOM-LEVEL= ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2)) 
;;LISTS= ((TOP-LEVEL (I L)) (LEVEL-M NIL) (LEVEL-F NIL) (LEVEL-L ((I L 2))) (LEVEL-I ((I 3 2))) (BOTTOM-LEVEL ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2))))





;;SORT-DIM-LEVELS-FROM-NESTED-LISTS
;;
;;ddd
(defun sort-dim-levels-from-nested-lists (nested-dimlist1 nested-dimlist2)
  (let
      ((levelM-1)
       (levelF-1)
       (levelL-1)
       (levelI-1)
       (levelM-2)
       (levelF-2)
       (levelL-2)
       (levelI-2)
       (n-levels1 (list-length nested-dimlist1))
       (n-levels2 (list-length nested-dimlist2))       
       )
    (multiple-value-setq (levelI-1 levelL-1 levelF-1 levelM-1)
        (values-list (reverse nested-dimlist1)))  ;;extra levels = nil

    (multiple-value-setq (levelI-2 levelL-2 levelF-2 levelM-2)
        (values-list (reverse nested-dimlist2)))
     
    (values  levelI-1 levelL-1 levelF-1 levelM-1 levelI-2 levelL-2 levelF-2 levelM-2)
    ;;end let, sort-dim-levels
    ))
;;TEST
;; (sort-dim-levels '(((I L 2)) ((I 3 2)) ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2))) '(((I L 2)) ((I 3 2)) ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2))))
;; works= ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2))  ((I 3 2)) ((I L 2)) NIL  ((1 3 2) (2 3 2) (3 3 2) (4 3 2) (5 3 2))  ((I 3 2)) ((I L 2)) NIL






;;MAKE-DIMSYMS-TREES
;;
;;ddd
(defun make-dimsyms-trees (all-syms-spec-list 
                           &key (node-separator  *art-node-separator)
                           (separator-str *art-index-separator)
                           (index-syms *art-index-syms)
                           parse-dimlist1-p (parse-dimlist2-p T))
  "U-ART. TOP ARTSYM CREATING FUNCTION. Makes one symbol tree for EACH sym-spec-list in all-syms-spec-list. Sets each symbol to a list (root dimlist nil subsym-list).  RETURNS  (values all-root-syms all-node-trees-syms all-path-syms-by-levels). Either makes node symbols eg XI-L-F, X9-2-3 OR path symbols eg. WUPI-L-FTOI-L-F  WUP7-3-2TO4-1-3. Sets  higher level symbols' value to subsyms."
  (let
      ((sym)
       (dimlist)
       (new-subsyms)
       (new-subsym-strs)
       (node-tree-syms)       
       (all-node-trees-syms)
       (sym-spec-list)
       (symroot)
       (dims-spec-list)
       (sym-default-graph-slot)
       (root-sym)
       (string)
       (dimslist)
       (top-path-combos)
       (path-syms-by-levels)
       (all-path-syms-by-levels)
       (n-dims)(n-items)(sublists)
       (n-dim1)(n-dim2)
       (dimspec-lists1)
       (dimspec-lists2)
       (topdims)
       (all-root-syms)
       (sublist-Ns)
       )

    ;;FOR EACH NEW SYM; topsym-spec-list:
    ;;NODE/VAR  eg (XI-L-F ("X" ((,*n-inputs 1 1 )(3 1 1    )(3 1 1 ))))
    ;;PATH eg. (WDNI-L-FTOI-L-F ("Wdn" ((,*n-outputs 1 1  )(1 1 1   )(1 3 1   )TO (,*n-inputs 1 1  )(1 3 1  )(1 2 1 ))))
    (loop
     for topsym-spec-list in all-syms-spec-list
     do
     ;;find parts of topsym-spec-list
     (setf sym (car topsym-spec-list)
           sym-spec-list (second topsym-spec-list)
           sym-default-graph-slot (third topsym-spec-list)
           symroot (car sym-spec-list)
           dims-spec-list (second sym-spec-list) 
           ;;eg ((,*n-outputs 1 1  )(1 1 1   )(1 3 1   )TO (,*n-inputs 1 1  )(1 3 1  )(1 2 1 ))
           root-sym (my-make-symbol symroot)
           all-root-syms (append all-root-syms (list root-sym)))

     ;;SEPARATE PATHS FROM NODES/VARS
     (cond
      ;;If a PATH 
      ((member *art-node-separator dims-spec-list :test 'my-equal)
       ;;find the dim spec info
       (multiple-value-setq (n-dims n-items sublist-Ns dimspec-lists n-dim1 n-dim2 dimspec-lists1 dimspec-lists2 )
           (find-dim-spec-info dims-spec-list))
       ;;eg (find-dim-spec-info '((5 1 1)(2 3 1)(2 2 1) TO (3 1 1)(1 1 1)(1 3 1)))

       ;;find the top-path-combos
       (cond
        ((= n-dims 6)
         (setf top-path-combos  '((I L F)(I L F))
               topdims '(I L F TO I L F)))
        ((= n-dims 8)
         (setf top-path-combos  '((I L F M )(I L F M ))
               topdims '(I L F M TO I L F M)))
        (t nil))

       ;;MAKE THE PATH NEWSYMS
       (setf path-syms-by-levels
             (make-path-dimsym-tree  symroot topdims dimspec-lists1 dimspec-lists2 
                                     :top-combo top-path-combos
                                     :default-graph-slot sym-default-graph-slot
                                     :node-separator node-separator  :index-syms index-syms)
             all-path-syms-by-levels 
             (append all-path-syms-by-levels (list path-syms-by-levels)))
       ;;end path clause
       )
      (t 
       ;;FOR NODES ONLY
       ;;find the dimlist of sym
       (setf dimlist (find-art-dims sym))

       ;;set the root-sym (eg X WUP  to the sym-spec-list; sym= XI-2-2 
       (when (and root-sym sym-spec-list (not (equal sym root-sym)))
         (set  root-sym (append sym-spec-list (list sym-default-graph-slot))))
       ;;set the sym to its symvals
       (cond
        ;;if sym exists (eg sym=XI-L-F), do nothing
        ((boundp sym) NIL)        
        ((and sym dimlist) 
         (set sym (list symroot dimlist)))
        (t nil))
      ;;above was (when (and sym dimlist) (set sym (list symroot dimlist)))

       ;;add sym to the symroot subsymvals (if not already there)
       (setsubsyms  root-sym  sym)
       ;;eg root-sym= X  sym=XI-L-F
       (loop
        for topsym-spec-list in all-syms-spec-list
        do
        ;;find parts of topsym-spec-list
        ;;(setf *out1 (append *out1 (list (list  (format nil "AT FOR NODES ONLY setsubsyms X= ~A~% root-sym= ~A sym=~A sym-spec-list= ~A~%  sym-default-graph-slot= ~A symroot= ~A dims-spec-list= ~A"  x root-sym sym sym-spec-list  sym-default-graph-slot symroot dims-spec-list)))))
        ;;(values all-node-tree-syms path-syms-by-levels new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p )
       
        (multiple-value-setq  (node-tree-syms new-subsyms new-subsym-strs )
            ;;not needed? subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p )
            (make-node-dimsym-tree (list sym) sym-spec-list 
                                   :separator-str  separator-str
                                   :node-separator  node-separator    :index-syms index-syms))
        ;;(setf *out1 (append *out1 (list (list (format nil "AFTER RECURSE, X= ~A~%  root-sym= ~A sym= ~A dimlist= ~A sym-spec-list= ~A  node-tree-syms= ~A~% "  x root-sym sym dimlist sym-spec-list  node-tree-syms )))))
        (setf all-node-trees-syms 
              (append all-node-trees-syms (list node-tree-syms)))
        ;;was (append all-node-trees-syms (list (list sym sym-spec-list node-tree-syms)))) 
        ;;was all-path-syms-by-levels (append all-path-syms-by-levels (list path-syms-by-levels)))
        ;;(BREAK) ;;ZZZZ

        ;;end NODE/VAR CLAUSE, cond
        )))
     ;;end all-syms-spec-list loop
     )
    (values all-root-syms all-node-trees-syms all-path-syms-by-levels)
    ;;end let, make-dimsyms-trees
    ))
;;TEST
;; TEST MAKING FIELD 1 AND FIELD 2 NODES W/ DIF Ns
;; (make-dimsyms-trees  '((TestXI-L-F  ("TestX" ((5 1 1 )(2 1 1)(2 1 1 )))) (TestXI-L-F  ("TestX" ((3 1 1 )(1 3 1)(1 3 1 ))))))
;;RESULTS=
;;(TESTX TESTX)
#|PPRINTED: 
(((TESTXI-L-1 TESTXI-L-2)
  (TESTXI-1-1 TESTXI-2-1)
  (TESTX1-1-1 TESTX2-1-1 TESTX3-1-1 TESTX4-1-1 TESTX5-1-1)
  (TESTX1-2-1 TESTX2-2-1 TESTX3-2-1 TESTX4-2-1 TESTX5-2-1)
  (TESTXI-1-2 TESTXI-2-2)
  (TESTX1-1-2 TESTX2-1-2 TESTX3-1-2 TESTX4-1-2 TESTX5-1-2)
  (TESTX1-2-2 TESTX2-2-2 TESTX3-2-2 TESTX4-2-2 TESTX5-2-2))
 ((TESTXI-L-1 TESTXI-L-2)
  (TESTXI-1-1 TESTXI-2-1)
  (TESTX1-1-1 TESTX2-1-1 TESTX3-1-1 TESTX4-1-1 TESTX5-1-1)
  (TESTX1-2-1 TESTX2-2-1 TESTX3-2-1 TESTX4-2-1 TESTX5-2-1)
  (TESTXI-1-2 TESTXI-2-2)
  (TESTX1-1-2 TESTX2-1-2 TESTX3-1-2 TESTX4-1-2 TESTX5-1-2)
  (TESTX1-2-2 TESTX2-2-2 TESTX3-2-2 TESTX4-2-2 TESTX5-2-2))
 ((TESTXI-L-3) (TESTXI-3-3) (TESTX1-3-3 TESTX2-3-3 TESTX3-3-3))
 ((TESTXI-L-3) (TESTXI-3-3) (TESTX1-3-3 TESTX2-3-3 TESTX3-3-3)))
NIL|#
#|;;SAMPLE SUBSYMS
 TESTX = ("TestX" ((3 1 1) (1 3 1) (1 3 1)) NIL NIL (TESTXI-L-F))
 TESTXI-L-F = ("TestX" (I L F) NIL NIL (TESTXI-L-1 TESTXI-L-2 TESTXI-L-3))
;;From 5 node FIELD 1:
 TESTXI-L-1 = ("TestX" (I L 1) NIL NIL (TESTXI-1-1 TESTXI-2-1))
 TESTXI-1-1 = ("TestX" (I 1 1) NIL NIL (TESTX1-1-1 TESTX2-1-1 TESTX3-1-1 TESTX4-1-1 TESTX5-1-1))
;;From 3 node FIELD 3:
TESTXI-L-3 = ("TestX" (I L 3) NIL NIL (TESTXI-3-3))
TESTXI-3-3 = ("TestX" (I 3 3) NIL NIL (TESTX1-3-3 TESTX2-3-3 TESTX3-3-3))
|#


;;
;;FOR NODES
;; (progn (setf *out1 nil *out2 nil) (make-dimsyms-trees '((INPUTI-L-F ("Input" ((5 1 1)(1 1 1 )(1 1 1)))))))
;; WORKS= (((INPUTI-I-L-1) (INPUTI-I-1-1) (INPUTI-1-1-1 INPUTI-2-1-1 INPUTI-3-1-1 INPUTI-4-1-1 INPUTI-5-1-1)))  NIL
;; INPUT = ("Input" ((5 1 1) (1 1 1) (1 1 1)) NIL NIL ((INPUTI-L-F)))
;;
;; (progn (setf *out1 nil *out2 nil) (make-dimsyms-trees '((XI-L-F  ("X" ((5 1 1 )(3 1 1)(3 1 1 ))) x-points))))
;;works= (((XI-L-1 XI-L-2 XI-L-3) (XI-1-1 XI-2-1 XI-3-1) (X1-1-1 X2-1-1 X3-1-1 X4-1-1 X5-1-1) (X1-2-1 X2-2-1 X3-2-1 X4-2-1 X5-2-1) (X1-3-1 X2-3-1 X3-3-1 X4-3-1 X5-3-1) (XI-1-2 XI-2-2 XI-3-2) (X1-1-2 X2-1-2 X3-1-2 X4-1-2 X5-1-2) (X1-2-2 X2-2-2 X3-2-2 X4-2-2 X5-2-2) (X1-3-2 X2-3-2 X3-3-2 X4-3-2 X5-3-2) (XI-1-3 XI-2-3 XI-3-3) (X1-1-3 X2-1-3 X3-1-3 X4-1-3 X5-1-3) (X1-2-3 X2-2-3 X3-2-3 X4-2-3 X5-2-3) (X1-3-3 X2-3-3 X3-3-3 X4-3-3 X5-3-3)))    NIL
;;CL-USER 22 > X  = ("X" ((5 1 1) (3 1 1) (3 1 1)) X-POINTS NIL ((XI-L-F)))
;;
;;FOR PATHS
;;SS? FIX;;;; (progn (setf *out1 nil *out2 nil)  (make-dimsyms-trees '((UUPI-L-FTOI-L-F ("Uup" ((5 1 1"")(1 3 1)(1 2 1)TO (3 1 1 )  (1 1 1)(1 3 1)))))) )
;; works= (UUP) NIL   (((TOP TOP (UUPI-L-FTOI-L-F)) (TOP M NIL) (M TOP NIL) (M M NIL) (M F NIL) (F M NIL) (F F (UUPI-L-2TOI-L-3)) (F L NIL) (L F NIL) (L L (UUPI-3-2TOI-1-3)) (L I NIL) (I L NIL) (I I (UUP1-3-2TO1-1-3 UUP1-3-2TO2-1-3 UUP1-3-2TO3-1-3 UUP2-3-2TO1-1-3 UUP2-3-2TO2-1-3 UUP2-3-2TO3-1-3 UUP3-3-2TO1-1-3 UUP3-3-2TO2-1-3 UUP3-3-2TO3-1-3 UUP4-3-2TO1-1-3 UUP4-3-2TO2-1-3 UUP4-3-2TO3-1-3 UUP5-3-2TO1-1-3 UUP5-3-2TO2-1-3 UUP5-3-2TO3-1-3)) (REST-COMBOS NIL NIL)))
;; CL-USER 57 > UUP =  ("Uup" (((5 1 1 "") (1 3 1) (1 2 1)) TO ((3 1 1) (1 1 1) (1 3 1))) NIL NIL (UUPI-L-FTOI-L-F))


;;THE LEVELS
;;
;; (setf *testsymspecs '( (XI-L-F ("X" ((5 1 1 )(3 1 1)(3 1 1)))) (UUPI-L-FTOI-L-F ("Uup" ((5 1 1"")(1 3 1)(1 2 1)TO (3 1 1 )  (1 1 1)(1 3 1))))))
;; (make-dimsyms-trees *testsymspecs)
;;RESULTS= [all one list]
;; ((XI-L-F ("X" ((5 1 1) (3 1 1) (3 1 1))) ((XI-L-1 XI-L-2 XI-L-3) (XI-1-1 XI-2-1 XI-3-1) (X1-1-1 X2-1-1 X3-1-1 X4-1-1 X5-1-1) (X1-2-1 X2-2-1 X3-2-1 X4-2-1 X5-2-1) (X1-3-1 X2-3-1 X3-3-1 X4-3-1 X5-3-1) (XI-1-2 XI-2-2 XI-3-2) (X1-1-2 X2-1-2 X3-1-2 X4-1-2 X5-1-2) (X1-2-2 X2-2-2 X3-2-2 X4-2-2 X5-2-2) (X1-3-2 X2-3-2 X3-3-2 X4-3-2 X5-3-2) (XI-1-3 XI-2-3 XI-3-3) (X1-1-3 X2-1-3 X3-1-3 X4-1-3 X5-1-3) (X1-2-3 X2-2-3 X3-2-3 X4-2-3 X5-2-3) (X1-3-3 X2-3-3 X3-3-3 X4-3-3 X5-3-3))) 
;;ALSO
;; FOR NODE:
;; level 1: XI-L-F = ("X" (I L F) NIL (XI-L-1 XI-L-2 XI-L-3))
;; level 2: eg XI-L-2 = ("X" (I L 2) NIL (XI-1-2 XI-2-2 XI-3-2))
;; level 3: eg  XI-1-2 = ("X" (I 1 2) NIL (X1-1-2 X2-1-2 X3-1-2 X4-1-2 X5-1-2))
;; level 4-values: eg.  X3-1-2 = ("X" (3 1 2) NIL NIL)
;;
;;FOR PATH -- pprint
#|((UUPI-L-FTOI-L-F
  ("Uup" ((5 1 1 "") (1 3 1) (1 2 1) TO (3 1 1) (1 1 1) (1 3 1)))
  ((UUPI-L-FTOI-L-3)
   (UUPI-L-FTOI-1-3)
   (UUPI-L-FTO1-1-3 UUPI-L-FTO2-1-3 UUPI-L-FTO3-1-3)
   (UUPI-L-2TO1-1-3)
   (UUPI-3-2-TO1-1-3)
   (UUP1-3-2TO1-1-3 UUP2-3-2TO1-1-3 UUP3-3-2TO1-1-3 UUP4-3-2TO1-1-3 UUP5-3-2TO1-1-3)
   (UUPI-L-2TO2-1-3)
   (UUPI-3-2-TO2-1-3)
   (UUP1-3-2TO2-1-3 UUP2-3-2TO2-1-3 UUP3-3-2TO2-1-3 UUP4-3-2TO2-1-3 UUP5-3-2TO2-1-3)
   (UUPI-L-2TO3-1-3)
   (UUPI-3-2-TO3-1-3)
   (UUP1-3-2TO3-1-3
    UUP2-3-2TO3-1-3
    UUP3-3-2TO3-1-3
    UUP4-3-2TO3-1-3
    UUP5-3-2TO3-1-3))))|#
;;
;; FOR SUBDIMS2 -------------------------
;;LEVEL 1:  UUPI-L-FTOI-L-3 = ("Uup" (I L F TO I L 3) NIL (UUPI-L-FTOI-1-3))
;;LEVEL 2:  UUPI-L-FTOI-1-3 = ("Uup" (I L F TO I 1 3) NIL (UUPI-L-FTO1-1-3 UUPI-L-FTO2-1-3 UUPI-L-FTO3-1-3))
;;LEVEL 3:  UUPI-L-FTO1-1-3 = ("Uup" (I L F TO 1 1 3) NIL (UUPI-L-2TO1-1-3))
;;                   UUPI-L-FTO2-1-3 = ("Uup" (I L F TO 2 1 3) NIL (UUPI-L-2TO2-1-3))
;;                   UUPI-L-FTOI-1-3  = ("Uup" (I L F TO I 1 3) NIL (UUPI-L-FTO1-1-3 UUPI-L-FTO2-1-3 UUPI-L-FTO3-1-3))
;; Note no values above bec must be at bottom level 
;;
;;FOR SUBDIMS1 ---------------------------
;;LEVEL 4:subdim2-1: 
;;    UUPI-L-2TO1-1-3 = ("Uup" (I L 2 TO 1 1 3) NIL (UUPI-3-2-TO1-1-3))
;;    UUPI-L-2TO2-1-3 = ("Uup" (I L 2 TO 2 1 3) NIL (UUPI-3-2-TO2-1-3))
;;    UUPI-L-2TO3-1-3 = ("Uup" (I L 2 TO 3 1 3) NIL (UUPI-3-2-TO3-1-3))
;;LEVEL 5:subdim2-2: 
;;    UUPI-3-2-TO1-1-3 = ("Uup" (I 3 2 TO 1 1 3) NIL (UUP1-3-2TO1-1-3 UUP2-3-2TO1-1-3 UUP3-3-2TO1-1-3 UUP4-3-2TO1-1-3 UUP5-3-2TO1-1-3))
;;    UUPI-3-2-TO2-1-3 = ("Uup" (I 3 2 TO 2 1 3) NIL (UUP1-3-2TO2-1-3 UUP2-3-2TO2-1-3 UUP3-3-2TO2-1-3 UUP4-3-2TO2-1-3 UUP5-3-2TO2-1-3))
;;   UUPI-3-2-TO3-1-3  = ("Uup" (I 3 2 TO 3 1 3) NIL (UUP1-3-2TO3-1-3 UUP2-3-2TO3-1-3 UUP3-3-2TO3-1-3 UUP4-3-2TO3-1-3 UUP5-3-2TO3-1-3))
;;LEVEL 6:subdim2//VALUE LEVEL:
;;  eg:   UUP1-3-2TO3-1-3 = ("Uup" (1 3 2 TO 3 1 3) NIL NIL)
;; END PATH EXAMPLE/TEST --------------------------------------------------


;;MORE TESTING FOR ALL  zzzz
;; ;; (progn (setf out nil) (make-dimsyms-trees    `((INPUTI-L-F ("Input" ((5 1 1)(1 1 1 )(1 1 1)))  input-points) (XI-L-F   ("X" ((5 1 1 )(3 1 1)(3 1 1 ))) x-points)     (YI-L-F  ("Y"((5 1 1 )(3 1 1   )(3 1 1 ))) y-points) )))
;;add these??
         ;; ("X-Activity" ((,*n-inputs 1 1 )) )  ;;was X-Activity
         ;; ("V-Activity" ((,*n-inputs 1 1 )) )  ;;V-Activity
          ;;(RI-L-F  ("R" ((,*n-inputs 1 1 )(1 1 1)(1 1 1)) ))
          ;;("Q-Activity" ((,*n-inputs 1 1 )) )
          ;;("P" ((,*n-inputs 1 1 )) )
          ;; (var-root (fromcelldim fromfielddim  tocelldim tofielddim))   EACH DIM SPEC= (N  begin incr end-str) 
#|          (WUPI-L-FTOI-L-F  ("Wup" ((,*n-inputs  1 1)(1 3 1)(1 2 1   ) TO (,*n-outputs 1 1) (1 1 1   )(1 3 1 ))) wup-points)            ;;was ((,*n-inputs 1 1 ) (,*n-outputs 1 1) ))
          (WDNI-L-FTOI-L-F ("Wdn" ((,*n-outputs 1 1  )(1 1 1   )(1 3 1   )TO (,*n-inputs 1 1  )(1 3 1  )(1 2 1 ))) wdn-points)
          (UUPI-L-FTOI-L-F ("Uup" ((,*n-inputs 1 1)(1 3 1  )(1 2 1   )TO (,*n-outputs 1 1   )(1 1 1   )(1 3 1 ))) uup-points)        
          (UDNI-L-FTOI-L-F ("Udn" ((,*n-outputs 1 1  )(1 1 1   )(1 3 1   ) TO (,*n-inputs 1 1  )(1 3 1  )(1 2 1 ))) udn-points)
         ;; ("Y-Output" ((,*n-outputs 1 1)) )
          ;;others
         ;;("Temp" ((1 1 1 )) )
          (RESETI-L-F ("reset" ((,*n-inputs 1 1  )(1 2 1 ) (2 2 1 ))) reset-points)
          ;;for ART3 F2
          ;;SSS  CHECK CREATION OF THE SYMVALS ETC
          ;;(RESET-NINPUTSI  ("reset-ninputs" ((,*n-inputs 1 1))))
          ;;for ART3 F3
          ;;(RESET-NOUTPUTSI ("reset-noutputs" ((,*n-outputs 1 1)) ))
          (RESET-CNTRI-L-F ("reset-cntr" ((,*n-outputs 1 1)(1 2 1)(1 2 1) )))
        ;;  (N-CATSI ("n-cats" ((,*n-outputs 1 1 ))))   ;;was(1 2 1)) )
          ;;("Temp2" ((,*n-outputs 1 1 ))))  ;;was(1 2 1)) )
          ;;end list, symbol-spec-lists
        ))|#








;;MAKE-NODE-DIMSYM-TREE 
;;works well on node trees (uses make-path-dimsym-tree);; not complete for paths, replaced.
;;
;;ddd
(defun make-node-dimsym-tree (symlist sym-spec-list  
                                        &key all-syms-list
                                        (node-separator  *art-node-separator)
                                        (separator-str *art-index-separator)
                                         (index-syms *art-index-syms)
                                         parse-dimlist1-p (parse-dimlist2-p T)
                                         subdims1-done-p subdims2-done-p
                                         all-node-tree-syms)
       "U-ART. Makes one symbol tree from one sym-spec-list. Sets each symbol to a list (artsym dimlist nil subsym-list).  RETURNS  (values all-artsyms path-syms-by-levels new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p ). Either makes node symbols eg X.F.L.I, X9-2-3 OR path symbols eg. WUP.F.L.ITO.F.L.I  WUP7-3-2TO4-1-3. Sets  higher level symbols' value to subsyms. Works well on both node & path trees (uses make-path-dimsym-tree). NO LONGER USED FOR PATHS.

OLD: Makes one symbol tree from one sym-spec-list. Sets each symbol to a list (root dimlist nil subsym-list).  RETURNS  (values all-node-tree-syms path-syms-by-levels new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p ). Either makes node symbols eg X.F.L.I, X9-2-3 OR path symbols eg. WUP.F.L.ITO.F.L.I  WUP7-3-2TO4-1-3. Sets  higher level symbols' value to subsyms. Works well on both node & path trees (uses make-path-dimsym-tree). NO LONGER USED FOR PATHS."
   (let*
       ((orig-root (car sym-spec-list))
         (dim-spec-lists (second sym-spec-list))
         (n-syms (length symlist))
         (new-subsyms)
         (new-subsym-strs)
         ;;added
         (path-sym-p)
         (symvals)
         (symdims)
         (new-spec-sublists)
         (subdimspecs1) 
         (subdimspecs2)
         (curdims1)
         (curdims2)
         (top-dimlist)
         (toppathsym)
         (new-n-dims)
         (new-n-items)
         (sublist-ns)
         (new-sublist-Ns)
         (path-syms-by-levels)
        )

     (loop
      for sym in symlist
      for n from 1 to n-syms
      do

      ;;NODE OR PATH SYM
      (setf symvals (eval sym)
            symdims (second symvals))
      (multiple-value-setq (new-n-dims new-n-items new-sublist-Ns new-spec-sublists)
          (find-dim-spec-info  symdims))
        (when (> (list-length new-spec-sublists) 1)
          (setf path-sym-p T
                subdimspecs1 (car new-spec-sublists)
                subdimsspecs2 (second new-spec-sublists))
          ;;WRONG PLACE?, last not done yet: check to see if subdims is all integers (meaning it's done)
         ;; (setf subdims1-done-p (dimlist-is-ints-p subdims1) 
          ;;      subdims2-done-p (dimlist-is-ints-p  subdims2)) 

          ;;(setf *out1 (append *out1 (list (format nil "~%IN make-node-dimsym-tree, X= ~A  new-spec-sublists= ~A subdims1-done-p= ~A subdims2-done-p= ~A  subdimspecs1= ~A subdimspecs2= ~A"X new-spec-sublists subdims1-done-p subdims2-done-p   subdimspecs1 subdimspecs2))))
          ;;end when
          )

      (cond
       ;;NODE SYM -- just do subsym, no subdims         ;;zzzz
       ((null path-sym-p)

        (multiple-value-setq (new-subsyms new-subsym-strs) ;;no subdims
            (make-node-dimsym-subtree  sym  sym-spec-list 
                                  :separator-str separator-str :node-separator node-separator
                                  :index-syms index-syms
                                  :new-subsyms new-subsyms :new-subsym-strs new-subsym-strs
                                  ))
        (when new-subsyms
          (setf all-node-tree-syms (append all-node-tree-syms (list new-subsyms))))
        )
       ;;PATH SYM -- must find both sublist syms
       ;;eg INPUT eg. (WUPI-L-FTOI-L-F  (\"Wup\" ((,*n-inputs  1 1)(1 3 1)(1 2 1   ) TO (,*n-outputs 1 1 (1 1 1   )(1 3 1 )))))
       (t     
#|        (setf target-dim-n1 (list-length subdimspecs1)
              target-dim-n2 (list-length subdimspecs2) 
              curdims *art-index-syms ;;(I L F M)
              n-artsyms (list-length *art-index-syms)
              n-dif (- n-artsyms target-dim-n1))                                 
               
        (when (> n-dif  0)
          (setf  curdims (butlast curdims n-dif))) 

        (unless (and (symbolp orig-root) (boundp orig-root))
          (setf top-dimlist (append  curdims  (list 'TO) curdims)
                toppathsym (make-dim-symbol orig-root top-dimlist)))

        (multiple-value-setq (M-syms L-syms F-syms I-syms  path-syms-by-level)
            (make-path-dimsym-tree toppathsym curdims target-dim-n1 subdimspecs1 
                                   curdims target-dim-n2 subdimspecs2))|#

        ;;end t,cond
        ))

      ;;TRY TEST FOR SUBDIMS DONE HERE -- DONE IN SUBTREE?
      ;;check to see if subdims is all integers (meaning it's done)
       ;;  (setf subdims1-done-p (dimlist-is-ints-p subdims1) 
        ;;        subdims2-done-p (dimlist-is-ints-p  subdims2))     |   

      ;;WHAT NEXT--RECURSE?
      (cond
       ;;last item at last level with all dims being integers, therefore returning nil?
       ((null new-subsyms)  NIL)
       (t
         (multiple-value-setq (all-node-tree-syms path-syms-by-levels new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p)
             (make-node-dimsym-tree  new-subsyms sym-spec-list 
                                :all-node-tree-syms all-node-tree-syms
                                :separator-str  separator-str
                                   :node-separator  node-separator    :index-syms index-syms
                                   :subdims1-done-p subdims1-done-p
                                   :subdims2-done-p subdims2-done-p
                                   :parse-dimlist1-p parse-dimlist1-p 
                                   :parse-dimlist2-p parse-dimlist2-p))

         (when new-subsyms
           (setf all-node-tree-syms (append all-node-tree-syms (list new-subsyms))))
         ;;end t,cond
         ))
         ;;end loop
         )
       
    ;;(setf *out1 (append (list *out1) (list (format nil "AFTER TREE RECURSE: X= ~A  new-subsyms=~A~%all-node-tree-syms= ~A~%path-syms-by-levels= ~A~%  subdims1-done-p= ~A subdims2-done-p= ~A~%"   X new-subsyms all-node-tree-syms path-syms-by-levels subdims1-done-p subdims2-done-p))))

     (values all-node-tree-syms path-syms-by-levels new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p )
        ;;end let, make-node-dimsym-tree
        ))
;;TEST
;;ZZZZ
;;  (progn (setf *out1 nil *out2 nil  INPUTI-L-F '("Input" (I L F) ))(make-node-dimsym-tree '(INPUTI-L-F) '("Input" ((5 1 1)(1 1 1 )(1 1 1)))))
;;works= ((INPUTI-L-1) (INPUTI-1-1) (INPUT1-1-1 INPUT2-1-1 INPUT3-1-1 INPUT4-1-1 INPUT5-1-1))   NIL   NIL  3  0  NIL  NIL
;;  
;; (make-node-dimsym-tree  '(XI-L-F)  '("X" ((5 1 1 )(3 1 1)(3 1 1 ))))
;; results= 
;;  ((XI-L-1 XI-L-2 XI-L-3) (XI-1-1 XI-2-1 XI-3-1) (X1-1-1 X2-1-1 X3-1-1 X4-1-1 X5-1-1) (X1-2-1 X2-2-1 X3-2-1 X4-2-1 X5-2-1) (X1-3-1 X2-3-1 X3-3-1 X4-3-1 X5-3-1) (XI-1-2 XI-2-2 XI-3-2) (X1-1-2 X2-1-2 X3-1-2 X4-1-2 X5-1-2) (X1-2-2 X2-2-2 X3-2-2 X4-2-2 X5-2-2) (X1-3-2 X2-3-2 X3-3-2 X4-3-2 X5-3-2) (XI-1-3 XI-2-3 XI-3-3) (X1-1-3 X2-1-3 X3-1-3 X4-1-3 X5-1-3) (X1-2-3 X2-2-3 X3-2-3 X4-2-3 X5-2-3) (X1-3-3 X2-3-3 X3-3-3 X4-3-3 X5-3-3))  NIL NIL NIL NIL NIL NIL T
#|;;PPRINT
((XI-L-1 XI-L-2 XI-L-3)
 (XI-1-1 XI-2-1 XI-3-1)
 (X1-1-1 X2-1-1 X3-1-1 X4-1-1 X5-1-1)
 (X1-2-1 X2-2-1 X3-2-1 X4-2-1 X5-2-1)
 (X1-3-1 X2-3-1 X3-3-1 X4-3-1 X5-3-1)
 (XI-1-2 XI-2-2 XI-3-2)
 (X1-1-2 X2-1-2 X3-1-2 X4-1-2 X5-1-2)
 (X1-2-2 X2-2-2 X3-2-2 X4-2-2 X5-2-2)
 (X1-3-2 X2-3-2 X3-3-2 X4-3-2 X5-3-2)
 (XI-1-3 XI-2-3 XI-3-3)
 (X1-1-3 X2-1-3 X3-1-3 X4-1-3 X5-1-3)
 (X1-2-3 X2-2-3 X3-2-3 X4-2-3 X5-2-3)
 (X1-3-3 X2-3-3 X3-3-3 X4-3-3 X5-3-3))|#
 ;; X = NIL ERROR






;;
;;  FOR NODE VARIABLES------------------------------------------
;; (setf  YI-L-F  '("Y" (I L F)))
;;  (make-node-dimsym-tree '(YI-L-F) '("Y" ((4 1 1 )(3 1 1)(3 1 1))))
#| returns = ((YI-L-1 YI-L-2 YI-L-3) (YI-1-1 YI-2-1 YI-3-1) (Y1-1-1 Y2-1-1 Y3-1-1 Y4-1-1) (Y1-2-1 Y2-2-1 Y3-2-1 Y4-2-1) (Y1-3-1 Y2-3-1 Y3-3-1 Y4-3-1) (YI-1-2 YI-2-2 YI-3-2) (Y1-1-2 Y2-1-2 Y3-1-2 Y4-1-2) (Y1-2-2 Y2-2-2 Y3-2-2 Y4-2-2) (Y1-3-2 Y2-3-2 Y3-3-2 Y4-3-2) (YI-1-3 YI-2-3 YI-3-3) (Y1-1-3 Y2-1-3 Y3-1-3 Y4-1-3) (Y1-2-3 Y2-2-3 Y3-2-3 Y4-2-3) (Y1-3-3 Y2-3-3 Y3-3-3 Y4-3-3))  NIL NIL NIL NIL NIL NIL T
Y = ("Y" ((9 1 1) (3 1 1) (3 1 1)) Y-POINTS NIL ((YI-L-F)))
;;pprint
((YI-L-1 YI-L-2 YI-L-3)
 (YI-1-1 YI-2-1 YI-3-1)
 (Y1-1-1 Y2-1-1 Y3-1-1 Y4-1-1)
 (Y1-2-1 Y2-2-1 Y3-2-1 Y4-2-1)
 (Y1-3-1 Y2-3-1 Y3-3-1 Y4-3-1)
 (YI-1-2 YI-2-2 YI-3-2)
 (Y1-1-2 Y2-1-2 Y3-1-2 Y4-1-2)
 (Y1-2-2 Y2-2-2 Y3-2-2 Y4-2-2)
 (Y1-3-2 Y2-3-2 Y3-3-2 Y4-3-2)
 (YI-1-3 YI-2-3 YI-3-3)
 (Y1-1-3 Y2-1-3 Y3-1-3 Y4-1-3)
 (Y1-2-3 Y2-2-3 Y3-2-3 Y4-2-3)
 (Y1-3-3 Y2-3-3 Y3-3-3 Y4-3-3))|#

;;SAMPLE VALUES at each level
;;LEVEL 1:(TOP)  YI-L-F = ("Y" (I L F) NIL (YI-L-1 YI-L-2 YI-L-3))
;;LEVEL 2:  YI-L-1 = ("Y" (I L 1) NIL (YI-1-1 YI-2-1 YI-3-1))
;;LEVEL 3:(BOTTOM)  YI-3-1 = ("Y" (I 3 1) NIL (Y1-3-1 Y2-3-1 Y3-3-1 Y4-3-1))
;;
;;  (progn (setf *out1 nil *out2 nil  INPUTI-L-F '("Input" (I L F) ))(make-node-dimsym-tree '(INPUTI-L-F) '("Input" ((5 1 1)(1 1 1 )(1 1 1)))))
;;((INPUTI-L-1) (INPUTI-1-1) (INPUT1-1-1 INPUT2-1-1 INPUT3-1-1 INPUT4-1-1 INPUT5-1-1))  NIL NIL NIL NIL
;; (progn (setf  *out1 nil)(make-node-dimsym-tree  '(XI-L-F)  '("X" ((5 1 1 )(3 1 1)(3 1 1 )))))
;; ((XI-L-1 XI-L-2 XI-L-3) (XI-1-1 XI-2-1 XI-3-1) (X1-1-1 X2-1-1 X3-1-1 X4-1-1 X5-1-1) (X1-2-1 X2-2-1 X3-2-1 X4-2-1 X5-2-1) (X1-3-1 X2-3-1 X3-3-1 X4-3-1 X5-3-1) (XI-1-2 XI-2-2 XI-3-2) (X1-1-2 X2-1-2 X3-1-2 X4-1-2 X5-1-2) (X1-2-2 X2-2-2 X3-2-2 X4-2-2 X5-2-2) (X1-3-2 X2-3-2 X3-3-2 X4-3-2 X5-3-2) (XI-1-3 XI-2-3 XI-3-3) (X1-1-3 X2-1-3 X3-1-3 X4-1-3 X5-1-3) (X1-2-3 X2-2-3 X3-2-3 X4-2-3 X5-2-3) (X1-3-3 X2-3-3 X3-3-3 X4-3-3 X5-3-3))  NIL NIL NIL NIL NIL NIL T
;; X = ("X" ((9 1 1) (3 1 1) (3 1 1)) X-POINTS NIL (XI-L-F))
;;XI-L-1 =  ("X" (I L 1) NIL NIL (XI-1-1 XI-2-1 XI-3-1))
;; pprint
#|((XI-L-1 XI-L-2 XI-L-3)
 (XI-1-1 XI-2-1 XI-3-1)
 (X1-1-1 X2-1-1 X3-1-1 X4-1-1 X5-1-1)
 (X1-2-1 X2-2-1 X3-2-1 X4-2-1 X5-2-1)
 (X1-3-1 X2-3-1 X3-3-1 X4-3-1 X5-3-1)
 (XI-1-2 XI-2-2 XI-3-2)
 (X1-1-2 X2-1-2 X3-1-2 X4-1-2 X5-1-2)
 (X1-2-2 X2-2-2 X3-2-2 X4-2-2 X5-2-2)
 (X1-3-2 X2-3-2 X3-3-2 X4-3-2 X5-3-2)
 (XI-1-3 XI-2-3 XI-3-3)
 (X1-1-3 X2-1-3 X3-1-3 X4-1-3 X5-1-3)
 (X1-2-3 X2-2-3 X3-2-3 X4-2-3 X5-2-3)
 (X1-3-3 X2-3-3 X3-3-3 X4-3-3 X5-3-3))|#
;; ((XI-L-3) (XI-3-3) (X1-3-3 X2-3-3 X3-3-3))
;; (make-node-dimsym-tree  '(YI-L-F)  '("Y" ((3 1 1 )(1 3 1)(1 3 1 ))))
;; ((YI-L-3) (YI-3-3) (Y1-3-3 Y2-3-3 Y3-3-3))






;;MAKE-NODE-DIMSYM-SUBTREE
;;
;;ddd
(defun make-node-dimsym-subtree (sym  sym-spec-list 
                                    &key parse-dimlist1-p (parse-dimlist2-p T) 
                                    (separator-str "-")(node-separator "TO") 
                                    (index-syms *art-index-syms)
                                    subdims1-done-p  subdims2-done-p graph-slot
                                    new-subsyms new-subsym-strs)
  "In U-ART.  Sets new syms= symval lists (root dims nil subsyms) RETURNS (values new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p. Note: sym can be string or symbol. This is the workhorse of the new sym tree system."
  (let*
      ((sym (my-make-symbol sym))
       (symvals  (eval sym)) ;;eg (root dimlist place-for-subsyms)  ("X" (i l f))
       (topdims (second symvals))
       (target-dims)
       (rev-target-dims)
       (n-target-dims)
       (target-dimspecs)
       (orig-root) 
       (dim-spec-lists (second sym-spec-list))
       (dimspec-sublist1)
       (dimspec-sublist2)
       (other-dimspecs)
       (subsym)
       (subsymval)
       (subsymvals)
       (subsymdims)
       (begin-index) 
       (incr-index) 
       (begin-index-str "") 
       (end-index-str "")
       (begin-indecies)
       (begin-indecies-str "")
       (end-indecies)
       (end-indecies-str "")
       (sublist1-indecies-str)
       (sublist2-indecies-str)
       (n-dims)(n-items)(sublists)
       (found-index-sym-p)
       (node-separator-n 0)
       (parse-sublist)
       (node-sym-spec-list)
       (subdims1)
       (subdims2)
       (n-tdims)(n-sub1-dims)(n-sub2-dims)
       (dimspec-sublists)
       (sublist1-n)
       (node-sep)
       (sublist2-n)
       (target-index-n)
       (new-indecies)
       (subsym-str)
       ;;(preset-vals)
       (preset-subsubsyms)
       )

    ;;find info for artsym topdims dim-lists
    (multiple-value-setq (dimslist subdims1 subdims2 n-tdims n-sub1-dims n-sub2-dims)
        (find-symdim-info sym))
         ;; (setf *out1 (append *out1 (list (format nil "DIMS INFO  topdims=~A~% subdims1= ~A subdims2= ~A n-tdims= ~A  n-sub1-dims= ~A  n-sub2-dims= ~A~%"  topdims subdims1 subdims2 n-tdims n-sub1-dims n-sub2-dims))))

     ;;find overall info in dim-spec-lists
    (multiple-value-setq (n-dims n-items sublist-Ns dimspec-sublists) ;;eg 3  3  (3)  sublists
         (find-dim-spec-info dim-spec-lists))
     ;;find the (3 to 3) sublist
     (multiple-value-setq (sublist1-n node-sep sublist2-n)  ;;eg 3 0 0
         (values-list sublist-Ns))
     ;;find the two dimspec-sublists
     (setf dimspec-sublist1 (first dimspec-sublists)
           dimspec-sublist2 (second dimspec-sublists)
           ;;test to see if subdims are all integers (ie done)
           subdims1-done-p (integers-list-p subdims1)
           subdims2-done-p (integers-list-p subdims2))

     ;;SET WHICH SUBDIMS (IF ANY) TO PARSE NEXT
     (cond
      ;;1 done, 2 nil; parse 2
      ((and subdims1-done-p (null subdims2-done-p))
       (setf parse-dimlist2-p T
             parse-dimlist1-p NIL))
       ;;2 done, 1 nil; parse 1
      ((and subdims2-done-p (null subdims1-done-p))
       (setf parse-dimlist1-p T
             parse-dimlist2-p nil))
      ;;both done; parse none
      ((and subdims1-done-p subdims2-done-p)
       (setf parse-dimlist1-p nil
             parse-dimlist2-p nil))
      ;;neither done, go by arg key var settings; Default is parse-dimlist2-p = T
      (t  nil))

          ;;(setf *out1 (append *out1 (list (format nil "DONE-P & PARSE-P SET: dimspec-sublists= ~A;INFO: sublist1-n= ~A node-sep= ~A sublist2-n= ~A~%dimspec-sublist1= ~A dimspec-sublist2= ~A~%subdims1-done-p= ~A subdims2-done-p= ~A~%parse-dimlist1-p= ~A parse-dimlist2-p= ~A~%" dimspec-sublists sublist1-n node-sep sublist2-n dimspec-sublist1 dimspec-sublist2 subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p ))))

     ;;If sym is a path, choose which sublist to parse (before TO or after TO)
     ;;If a node-separator, find the sym-spec-list sublist to process

     ;;Find target-dims & target-dimspecs ;;eg (i l 2) & ((5 1 1)(1 2 1)(1 2 1))  
     (cond
      ;;if path symbol
      (node-sep
       (cond
        (parse-dimlist2-p
        ;; (setf subdims2-done-p T)
         (setf target-dimspecs dimspec-sublist2
               other-dimspecs dimspec-sublist1
               target-dims subdims2
               sublist1-indecies-str (make-dims-string subdims1)))
        (parse-dimlist1-p
         ;;(setf subdims1-done-p T)
         (setf target-dimspecs dimspec-sublist1
                 other-dimspecs dimspec-sublist2
                  target-dims subdims1
                  sublist2-indecies-str (make-dims-string subdims2))
                  )
        (t NIL))
       ;;end if path (node-sep)
        )
      ;;if node symbol
      (t  (setf target-dims topdims
                target-dimspecs dim-spec-lists)))

     ;;then set target-dims info
     (setf rev-target-dims (reverse target-dims)
           n-target-dims (list-length target-dims)
           orig-root (car sym-spec-list))
     ;;(setf *out1 (append *out1 (list (format nil "AT 1 target-dimspecs= ~A target-dims= ~A  rev-target-dims= ~A" target-dimspecs target-dims rev-target-dims))))

     ;;PROCESS EACH INDEX IN TARGET-DIMS
    (loop
     ;;for index in target-dims
     for index-n from 1 to n-target-dims
     for target-index in (reverse target-dims)  ;;eg f
     ;;find dimspec of LAST dimspec list first
     for target-dimspec in (reverse target-dimspecs)  ;;eg (3 1 1 -)
     do
     (setf target-index-n (- n-target-dims (- index-n 1))) ;; eg 3
     
     ;;(setf *out1 (append *out1 (list (format nil "IN OUTER LOOP 1, target-index= ~A target-dimspec= ~A target-index-n(from target-dims)= ~A ~%" target-index target-dimspec target-index-n ))))

     (cond
      ;;work on first reverse index that is a symbol (not number)
      ((member target-index index-syms :test 'my-equal)  ;;eg F is an ART index sym
       ;; (make-one-dim-indecies '(3 1 1 -))
       (setf new-indecies (make-one-dim-indecies target-dimspec) ;; eg (1 2 3)
             begin-indecies (butlast target-dims  index-n)
             begin-indecies-str 
             (print-list begin-indecies :no-newline-p t :separator-str separator-str)
             end-indecies (nthcdr target-index-n target-dims)
             end-indecies-str
             (print-list end-indecies :no-newline-p t  :separator-str separator-str)
             found-index-sym-p T)

      ;;(setf *out1 (append *out1 (list (format nil "~%~%IN OUTER LOOP 1B new-indecies= ~A begin-indecies= ~A end-indecies= ~A"     new-indecies   begin-indecies  end-indecies ))))
       
       ;;FOR EACH NEW-INDEX, MAKE A NEW SYBSYM
       (loop
        for new-index in new-indecies
        do
        ;;make the subsymdims and subsymdims-str for current new subsym
        (cond
         (end-indecies
          (setf subsymdims (append begin-indecies (list new-index) end-indecies)))
         (t (setf subsymdims (append begin-indecies (list new-index)))))
        
        ;;so cond below works right

        ;;MAKE THE NEW SUBSYM-STR AND SUBSYM
        (cond
         ;;If artsym is a NODE
         ((null node-sep)
          (cond
           ((and end-indecies begin-indecies)
            (setf subsym-str (format nil "~A~A~A~A~A~A" orig-root begin-indecies-str 
                                     separator-str  new-index separator-str end-indecies-str)))
           (begin-indecies
            (setf subsym-str (format nil "~A~A~A~A" orig-root begin-indecies-str 
                                     separator-str  new-index)))
           (end-indecies
            (setf subsym-str (format nil "~A~A~A~A" orig-root 
                                     new-index separator-str end-indecies-str))))
          ;;(BREAK)
          ;;end node-sep clause
          )
         ;;If artsym is a PATH
         (t
          ;;end t, cond
          ))
     
        (when subsym-str  ;;was subsym
          (setf subsym (my-make-symbol subsym-str)
                subsymvals (list orig-root  subsymdims nil nil) ;;didn't incl nil nil
                new-subsym-strs (append new-subsym-strs (list subsym-str))
                new-subsyms (append new-subsyms (list subsym)))

          ;;(setf *out1 (append *out1 (list (format nil "IN INNER LOOP, subsymdims= ~A~%subsym= ~A, subsymvals= ~A~%" subsymdims     subsym   subsymvals))))
          ;;(setf *out1 (append *out1 (list (format nil "ALSO: begin-indecies-str= ~A  end-indecies-str= ~A"  begin-indecies-str  end-indecies-str))))
            ;;end when
          )        

        (when (and subsym subsymvals)
          (set subsym subsymvals))                  

        ;;end inner loop
        )
       ;;AFTER MAKING NEW SUBSYMS, RETURN
       (return)
       ;;end member
       )
      ((integerp target-index)
       ;;DO NOTHING??
        )
        ;;not needed?
        (t 
         ;;DO NOTHING
         ))

     ;;end outer loop
     )
   
    ;;ADD NEW SUBSYMS TO ORIGINAL SYM SYMVALS LIST ZZZZ
    (setsubsyms  sym new-subsyms :graph-slot graph-slot)
    ;;First see if subsym previously put in list of parent subsyms, don't add again!
#|  not needed?  (setf preset-subsubsyms (getsubsyms sym))     ;;was (setf preset-vals (getsymval sym))
    (cond
     ((member subsym preset-subsubsyms)
      NIL)
     (t                                        ;;WAS (setsymval sym nil new-subsyms)))|#

    (values new-subsyms new-subsym-strs subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p)
     ;;end let, make-node-dimsym-subtree
     ))
;;TEST  
;;CURRENT FOR NODES ONLY ;;ZZZZ
;; (make-node-dimsym-subtree 'INPUTI-L-F  '("Input" ((5 1 1)(1 1 1 )(1 1 1))))
;; (make-node-dimsym-subtree  'XI-L-F '("TX" ((51 1 )(2 1 1)(2 1 1 ))) ) = ERRORS
;; (make-node-dimsym-subtree  'XI-L-F '("TX" ((3 1 1 )(1 3 1)(1 3 1 ))))  = ERRORS

;; (PROGN (SETF *OUT1 NIL) (make-node-dimsym-subtree  'XI-L-F  '("X" ((5 1 1 )(3 1 1)(3 1 1 )))))
;; (make-node-dimsym-subtree    'YI-L-F  '("Y" ((9 1 1) (3 1 1) (3 1 1))))


;; FOR NODE SYMBOLS ---------------------------------------------
;;  FOR LEVEL 1
;;  (setf Xi-l-f '("X" (i l f))) = ("X" (I L F))
;;
;; FOR NEXT LEVEL 2
;; (make-dimsym-subtree 'Xi-l-f  '("x" ((5 1 1 )(3 1 1   "-" )(3 1 1 "-"))))
;; works= (XI-L-1 XI-L-2 XI-L-3)   ("xI-L-1" "xI-L-2" "xI-L-3") NIL 0 T NIL
;;  also:   CL-USER 50 > XI-L-2  =  ("X" (I L F) NIL (XI-L-1 XI-L-2 XI-L-3))
;;
;; FOR NEXT LEVEL 3
;;   (make-dimsym-subtree 'XI-L-2  '("x" ((5 1 1 )(3 1 1   "-" )(3 1 1 "-"))))
;;  works=  (XI-1-2 XI-2-2 XI-3-2)  ("xI-1-2" "xI-2-2" "xI-3-2")
;;  also:   CL-USER 56 > XI-2-2 =   ("x" (I 2 2))
;; ALSO: topsym= TXI-L-2 now=  = ("x" (I L 2) NIL (XI-1-2 XI-2-2 XI-3-2))
;;
;; FOR NEXT LEVEL 4 (BOTTOM, VALUE LEVEL)
;; (make-dimsym-subtree 'XI-2-2  '("x" ((5 1 1 )(3 1 1   "-" )(3 1 1 "-"))))
;; works=  (X1-2-2 X2-2-2 X3-2-2 X4-2-2 X5-2-2)    ("x1-2-2" "x2-2-2" "x3-2-2" "x4-2-2" "x5-2-2")
;; also:  CL-USER 69 > X4-2-2  ("x" (4 2 2))
;; also topsym= XI-2-2 now =    = ("x" (I 2 2) NIL (X1-2-2 X2-2-2 X3-2-2 X4-2-2 X5-2-2));;
;; AT BOTTOM IF TRY TO RUN FUNCTION = NIL
;;   (make-dimsym-subtree 'X2-2-2  '("x" ((5 1 1 )(3 1 1   "-" )(3 1 1 "-")))) = NIL







;;FIND-BOTTOM-ART-INSTANCES
;;
;;ddd
(defun find-bottom-art-instances (topsym  &key all-subsyms-list 
                                          all-bottom-syms-list (create-sym-if-unbound-p T)
                                             all-errors-list default-value default-subsyms)
  "In U-symbol-trees.  Finds all bottom level instances (ones with real values and digits for all indecies.  To find all, start with topsym in a tree.  But will find all bottom level starting with any higher level artsym. Errors-list is all syms without all dims= integers, but no subsyms. RETURNS (values all-bottom-syms-list all-subsyms-list  all-errors-list). If create-sym-if-unbound-p, makes new topsym if unbound and sets to new symvals with default-value and default-subsyms."
  (let*
      ((topsym-symvals) ;; (eval topsym))
       (topsym-dimlist) ;; (second topsym-symvals))
       (topsym-val) ;; (fourth topsym-symvals))
       (topsym-subsyms)
       (bottom-syms-list)
       (bottom-syms-list1)
       (subsyms-list) 
       (subsyms-list1) 
       (errors-list)
       (errors-list1)
       (dims)(root)(symdims-str)
       )

    (when (and create-sym-if-unbound-p
               (not (and (symbolp topsym)(boundp topsym))))
      (multiple-value-setq (dims root)
          (find-art-dims topsym))

      ;;makes new sym and sets to a default symvals
      (multiple-value-setq (topsym symdims-str topsym-symvals)
          (make-art-topsym root dims :value default-value
                           :subsyms default-subsyms))
       ;;end when
      ) 

      ;;If the dims aren't all letters, then finds the top topsym.
      ;; Then  uses top-topsym to get ALL bottom syms--NOT JUST THOSE THAT
      (cond
       ((and (symbolp topsym)(boundp topsym))
        (setf topsym-symvals (eval topsym)
              topsym-dimlist (second topsym-symvals)
              topsym-val (fourth topsym-symvals)
              topsym-subsyms (fifth topsym-symvals))

        (cond
         ;;is the topsym actually a bottom sym?
         ((integers-list-p topsym-dimlist :exceptions (list *art-node-separator))
          (setf all-bottom-syms-list (append all-bottom-syms-list (list topsym)))
          (setf *out3 (append *out3 (list (format nil "AFTER INTEGERS-LIST-P all-bottom-syms-list= ~A" all-bottom-syms-list))))
          )
         #|     ((null (listp topsym-val))
      (setf errors-list (append errors-list (list topsym)))
      ;;(BREAK)
      )|#
         ;;if not, process it's sublist of syms
         (t
          (setf subsyms-list1 (append subsyms-list1 (list topsym)))
          (loop
           for sym in topsym-subsyms
           do
           (unless (equal sym topsym)
           ;;RECURSE ON EACH SUB SYM
           (multiple-value-setq (bottom-syms-list subsyms-list  errors-list)
               (find-bottom-art-instances sym :all-subsyms-list all-subsyms-list
                                          :all-bottom-syms-list  all-bottom-syms-list  :all-errors-list all-errors-list))
           (setf *out3 (append *out3 (list (format nil "AFTER RECURSE sym= ~A bottom-syms-list= ~A~% subsyms-list= ~A~%  errors-list= ~A~%" sym bottom-syms-list subsyms-list  errors-list))))

           (when bottom-syms-list
             (setf bottom-syms-list1 (append bottom-syms-list1 bottom-syms-list)))
           (when subsyms-list
             (setf subsyms-list1 (append subsyms-list1 subsyms-list)))
           (when errors-list
             (setf errors-list1 (append errors-list1 errors-list)))

           ;;end unless, loop
           ))
          (when bottom-syms-list1
            (setf all-bottom-syms-list (append all-bottom-syms-list bottom-syms-list1)))
          (when subsyms-list1
            (setf all-subsyms-list (append all-subsyms-list subsyms-list1)))
          (when errors-list1
            (setf all-errors-list (append all-errors-list errors-list1)))

          (setf *out3 (append *out3 (list (format nil "AFTER LOOP all-bottom-syms-list= ~A~% all-subsyms-list= ~A~%  all-errors-list= ~A~%" all-bottom-syms-list all-subsyms-list  all-errors-list))))

          ;;end t, cond
          ))
        ;;end symbolp and boundp topsym
        )
       (t nil))

    (values all-bottom-syms-list all-subsyms-list  all-errors-list)
    ;;end let, find-bottom-art-instances
    ))
;; SSS START HERE TESTING
;;TEST
;;  (progn (setf *out3 nil)(find-bottom-art-instances 'wupi-l-ftoi-l-f))
;; (find-bottom-art-instances 'RESETI-L-F) 
;; works = (RESET1-2-2 RESET2-2-2 RESET3-2-2 RESET4-2-2 RESET5-2-2)   (RESETI-L-F RESETI-L-2 RESETI-2-2)  NIL
;; ;; (find-bottom-art-instances 'wdnI-L-FTOI-L-F) ;;for ninputs=5 noutputs=3
;; works= (WDN1-1-3TO1-3-2 WDN2-1-3TO1-3-2 WDN3-1-3TO1-3-2 WDN1-1-3TO2-3-2 WDN2-1-3TO2-3-2 WDN3-1-3TO2-3-2 WDN1-1-3TO3-3-2 WDN2-1-3TO3-3-2 WDN3-1-3TO3-3-2 WDN1-1-3TO4-3-2 WDN2-1-3TO4-3-2 WDN3-1-3TO4-3-2 WDN1-1-3TO5-3-2 WDN2-1-3TO5-3-2 WDN3-1-3TO5-3-2)      
;;(WDNI-L-FTOI-L-F WDNI-L-FTOI-L-2 WDNI-L-FTOI-3-2 WDNI-L-FTO1-3-2 WDNI-L-3TO1-3-2 WDNI-1-3-TO1-3-2 WDNI-L-FTO2-3-2 WDNI-L-3TO2-3-2 WDNI-1-3-TO2-3-2 WDNI-L-FTO3-3-2 WDNI-L-3TO3-3-2 WDNI-1-3-TO3-3-2 WDNI-L-FTO4-3-2 WDNI-L-3TO4-3-2 WDNI-1-3-TO4-3-2 WDNI-L-FTO5-3-2 WDNI-L-3TO5-3-2 WDNI-1-3-TO5-3-2)
;;NIL
;; (progn (setf *out3 nil)(find-bottom-art-instances 'wupI-L-FTOI-L-F))




;;REPLACED BY make-new-dimsymbol-types IN U-CS-ART
;; 
;;MAKE-NEW-DIM-SYMBOL-TYPES
;; MOSTLY REPLACED BY MAKE-SYMBOL-TREES -not used in art
;;
;;  THESE 3 MAKE FUNCTIONS ARE ONLY USED IN ART--SO CAN CHANGE
;;   ALSO, U-ART HAS RELATED SET, GET FUNCTIONS TO OPERATE 
;;   ON THE SYMBOLS AND SYMVALS LISTS CREATED
;;
;;ddd
#|(defun make-new-dim-symbol-types (symbol-spec-lists
                                    &key make-sublists-for-each-dim-p
                                    (set-global-vars-p T)  (return-flat-lists-p T)
                                    (path-indicator-string "To") nested-inner-list-size)
 "In U-symbol-trees.  Returns new sequences of root, begin-str,dim-element,end-str for each value of dim-element from begin-n to n-dim-elements. RETURNS (values new-symbol-type-list  new-symbols-type-list-of-lists   new-symbol-type-spec-list-of-lists  new-root-list   new-symbol-type-symbol-string-list-of-lists  all-new-sym-names-flat-list  all-new-symbols-flat-list all-new-symdim-lists-flat-list).  INPUTS:  For SYMBOL-SPEC-LISTS, EACH symbol-spec-list= (ROOT all-dims-spec-list). ALL-DIMS-SPEC-LIST= (sublist1 sublist2 etc).  Each dim sublist =  (n-elements begin-n/or/cur-dim-n  dim-incr  begin-str end-str. Eg. (\"root\" '((4 1 1 \"C\" \"F\")(3 1 1 \"C\" \"F\"))).
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
       (new-symdim-lists-flat-list) ;;new
       (all-new-sym-names-flat-list)  ;;new
       (all-new-symbols-flat-list) ;;new
       (all-new-symdim-lists-flat-list) ;;new
       (all-dims-spec-list) ;;new
       (new-seq-nested-lists)
       (new-symbol-nested-lists)
       (new-dim-list-nested-lists)
       (new-symbol-type-spec-list)
       (new-symbol-type-symbol-string-list)
       (new-symbols-type-list-of-lists)
       (new-symbol-type-spec-list-of-lists)
       (new-symbol-type-symbol-string-list-of-lists)       
       (new-root-list)
       (dimspecs-n)
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
     (multiple-value-setq (new-seq-nested-lists  new-symbol-nested-lists new-dim-list-nested-lists  new-sym-names-flat-list new-symbols-flat-list new-symdim-lists-flat-list)
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
                                            (list new-symbols-flat-list))
           all-new-symdim-lists-flat-list (append all-new-symdim-lists-flat-list
                                            (list new-symdim-lists-flat-list)))


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

     ;;SET NEW SYMBOLS TO LISTS 
 ;;  causes errors
#|   (cond
     ;;If want to manually control the nested-inner-list-size
      (nested-inner-list-size
       (set (my-make-symbol (format nil "~A-flat" root))   new-symbols-flat-list)
       (set  root-sym  new-symbol-nested-lists))
      (t
       (set  root-sym  new-symbols-flat-list)
       (set (my-make-symbol (format nil "~A-flat" root))  new-symbols-flat-list)))|#

     ;;end BIG OUTER symbol-spec-lists loop
     )
    
    (when set-global-vars-p 
      (setf *new-symbol-type-list new-symbol-type-list
            *new-symbols-type-list-of-lists  new-symbols-type-list-of-lists
            *new-symbol-type-spec-list-of-lists  new-symbol-type-spec-list-of-lists
            *new-root-list new-root-list
            *new-symbol-type-symbol-string-list-of-lists new-symbol-type-symbol-string-list-of-lists
            *all-new-sym-names-flat-list  all-new-sym-names-flat-list
            *all-new-symbols-flat-list all-new-symbols-flat-list
            *all-new-symdim-lists-flat-list all-new-symdim-lists-flat-list))

    (values new-symbol-type-list  new-symbols-type-list-of-lists
            new-symbol-type-spec-list-of-lists  new-root-list
            new-symbol-type-symbol-string-list-of-lists  all-new-sym-names-flat-list 
            all-new-symbols-flat-list all-new-symdim-lists-flat-list)
    ;;end let, make-new-dim-symbol-types
    ))|#
;;TEST
;;FOR  OLD ART3--------------------------------------------------------------
;; (make-new-dim-symbol-types  `(("Wup" ((3 1 1"")(1 3 1  "-")(1 1 1 "-"  ) (3  1 1  "To" )(1 1 1   "-")(1 2 1 "-")))))
;;new-symbol-type-list= (WUP)
;;new-symbols-type-list-of-lists= (((WUP1-3-1TO1-1-2) (WUP1-3-1TO2-1-2) (WUP1-3-1TO3-1-2) (WUP2-3-1TO1-1-2) (WUP2-3-1TO2-1-2) (WUP2-3-1TO3-1-2) (WUP3-3-1TO1-1-2) (WUP3-3-1TO2-1-2) (WUP3-3-1TO3-1-2)))
;;new-symbol-type-spec-list-of-lists= [note: dims not correct] (((("Wup1-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup1-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup1-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup2-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup2-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup2-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup3-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup3-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup3-3-1To3-1-2" (1 2 1 "-" "")))))
;;new-root-list= ("Wup")
;;new-symbol-type-symbol-string-list-of-lists= ((("Wup1-3-1To1-1-2") ("Wup1-3-1To2-1-2") ("Wup1-3-1To3-1-2") ("Wup2-3-1To1-1-2") ("Wup2-3-1To2-1-2") ("Wup2-3-1To3-1-2") ("Wup3-3-1To1-1-2") ("Wup3-3-1To2-1-2") ("Wup3-3-1To3-1-2")))
;;all-new-sym-names-flat-list= (("Wup1-3-1To1-1-2" "Wup1-3-1To2-1-2" "Wup1-3-1To3-1-2" "Wup2-3-1To1-1-2" "Wup2-3-1To2-1-2" "Wup2-3-1To3-1-2" "Wup3-3-1To1-1-2" "Wup3-3-1To2-1-2" "Wup3-3-1To3-1-2"))
;;all-new-symbols-flat-list= ((WUP1-3-1TO1-1-2 WUP1-3-1TO2-1-2 WUP1-3-1TO3-1-2 WUP2-3-1TO1-1-2 WUP2-3-1TO2-1-2 WUP2-3-1TO3-1-2 WUP3-3-1TO1-1-2 WUP3-3-1TO2-1-2 WUP3-3-1TO3-1-2))
;;all-new-symdim-lists-flat-list= ((("Wup1-3-1To1-1-2" (1 2 1 "-" "")) ("Wup1-3-1To2-1-2" (1 2 1 "-" "")) ("Wup1-3-1To3-1-2" (1 2 1 "-" "")) ("Wup2-3-1To1-1-2" (1 2 1 "-" "")) ("Wup2-3-1To2-1-2" (1 2 1 "-" "")) ("Wup2-3-1To3-1-2" (1 2 1 "-" "")) ("Wup3-3-1To1-1-2" (1 2 1 "-" "")) ("Wup3-3-1To2-1-2" (1 2 1 "-" "")) ("Wup3-3-1To3-1-2" (1 2 1 "-" ""))))
;;ALSO
;;CL-USER 16 > WUP3-3-1TO1-1-2  =  ("Wup" (3 3 1 1 1 2) 1)
;;
;;  (make-new-dim-symbol-types  `(("X" ((3 1 1"")(1 2 1  "-")(1 1 1 "-"  )))))
;;  WORKS=
;; (X)
;; (((X1-2-1) (X2-2-1) (X3-2-1)))
;; (((("X1-2-1" (1 1 1 "-" ""))) (("X2-2-1" (1 1 1 "-" ""))) (("X3-2-1" (1 1 1 "-" "")))))
;; ("X")
;; ((("X1-2-1") ("X2-2-1") ("X3-2-1")))
;; (("X1-2-1" "X2-2-1" "X3-2-1"))
;; ((X1-2-1 X2-2-1 X3-2-1))
;; ((("X1-2-1" (1 1 1 "-" "")) ("X2-2-1" (1 1 1 "-" "")) ("X3-2-1" (1 1 1 "-" ""))))
;;
;; (make-new-dim-symbol-types  `(("INPUT" ((3 1 1"")))))
;; (make-new-dim-symbol-types  '(("X" ((5 1 1 )(3 1 1   "-" )(3 1 1 "-")))))
;;
;;  FROM INIT
;; (make-new-dim-symbol-types '(("Wup" ((5 1 1"")(1 3 1  "-")(1 2 1 "-"  ) (3 1 1  "To" )(1 1 1   "-")(1 3 1 "-")))))





;;FOR ART2---------------------------------------------------------------
;;  (make-new-dim-symbol-types  '(("input" ((7 1  1 "C" ""))) ( "wup" ((4 1 1 "C" "")(3 1 1 "F" "")))))
;;  WORKS: new-symbol-type-list= (INPUT WUP)
;;new-symbols-type-list-of-lists= (((INPUTC1 INPUTC2 INPUTC3 INPUTC4 INPUTC5 INPUTC6 INPUTC7)) ((WUPC1F1 WUPC1F2 WUPC1F3) (WUPC2F1 WUPC2F2 WUPC2F3) (WUPC3F1 WUPC3F2 WUPC3F3) (WUPC4F1 WUPC4F2 WUPC4F3)))
;;new-symbol-type-spec-list-of-lists= (((("inputC1" (7 1 1 "C" "")) ("inputC2" (7 2 1 "C" "")) ("inputC3" (7 3 1 "C" "")) ("inputC4" (7 4 1 "C" "")) ("inputC5" (7 5 1 "C" "")) ("inputC6" (7 6 1 "C" "")) ("inputC7" (7 7 1 "C" "")))) ((("wupC1F1" (3 1 1 "F" "")) ("wupC1F2" (3 2 1 "F" "")) ("wupC1F3" (3 3 1 "F" ""))) (("wupC2F1" (3 1 1 "F" "")) ("wupC2F2" (3 2 1 "F" "")) ("wupC2F3" (3 3 1 "F" ""))) (("wupC3F1" (3 1 1 "F" "")) ("wupC3F2" (3 2 1 "F" "")) ("wupC3F3" (3 3 1 "F" ""))) (("wupC4F1" (3 1 1 "F" "")) ("wupC4F2" (3 2 1 "F" "")) ("wupC4F3" (3 3 1 "F" "")))))
;;new-root-list= ("input" "wup")
;;new-symbol-type-symbol-string-list-of-lists=  ((("inputC1" "inputC2" "inputC3" "inputC4" "inputC5" "inputC6" "inputC7")) (("wupC1F1" "wupC1F2" "wupC1F3") ("wupC2F1" "wupC2F2" "wupC2F3") ("wupC3F1" "wupC3F2" "wupC3F3") ("wupC4F1" "wupC4F2" "wupC4F3")))
;;ALSO
;; CL-USER 2 > INPUTC2  =  ("inputC2" (7 2 1 "C" ""))
;; CL-USER 3 > WUPC3F3 =  ("wupC3F3" (3 3 1 "F" ""))

;;(make-new-dim-symbol-types '((Input ((5 1 1))) (X-Activity ((5 1 1))) (V ((5 1 1))) (R ((5 1 1))) (U ((5 1 1))) (Q ((5 1 1))) (P ((5 1 1))) (W ((5 1 1))) (Wup ((5 1 1  -) (3 1 1))) (Wdn ((3 1 1  -) (5 1 1))) (Y-Output ((3 1 1))) (Temp ((1 1 1))) (reset-Val ((1 1 1))) (reset ((3 1 1))) (reset-cntr ((3 1 1))) (n-cats ((3 1 1) (1 2 1))) (Temp2 ((3 1 1) (1 2 1)))))
;;above works, but is too long



#|(defun testns ()
 (setf out nil)
  (let
      ((x)
       )
    (make-new-symbols "Prefix" 7  '((0 1)(1 7))
                                        :hyphen-p t  :end-string "Ending")
    ))|#

;; IS THERE A WAY TO CATCH AND THROW THESE LISTS INSTEAD OF USING GLOBAL VARS???


;;REPLACED BY make-multi-dim-symbol-list IN U-CS-ART?
;;
;;MAKE-MULTI-DIM-SEQUENCE-LIST
;;
;;ddd
#|(defun make-multi-dim-sequence-list  (root all-dims-spec-list 
                                           &key (recurse-flag 99)
                                           (unbind-global-vars-p T) (return-flat-lists-p T) 
                                           (node-separator "To") (set-root-to-flat-list-p T))
 "In U-symbol-trees. Unless make-list-p, returns a list of new sequences of root,begin-str,dim-element,end-str for each value of dim-element from begin-n to n-dim-elements. RETURNS (values  new-seq-nested-lists  new-symbol-nested-lists new-dim-list-nested-lists new-seq-flat-list new-symbol-flat-list new-symdim-flat-list).  ALL-DIMS-SPEC-LIST (sublist1 sublist2 etc).  Each dim sublist =  (n-elements begin-n/or/cur-dim-n  dim-incr  begin-str end-str. If unbind-global-vars-p, then unbinds global vars needed to return right lists. If return-flat-lists-p, then returns unnested lists instead of nested ones. NOTE: Nested-lists are nested by all Dim1 items together.  Use function resort-nested-lists for 2-level/2-dim nested lists with all Dim2 items in same list."
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
         (new-symdim-flat-list)
         (return-lists '( *new-seq-nested-lists  *new-symbol-nested-lists *new-dim-list-nested-lists))  ;; *new-seq-flat-list))
         ;;added
         (main-root-sym (my-make-symbol (format nil "~A" root)))
         )
  ;;CALL THE MAIN WORK FUNCTION          
   (multiple-value-setq (new-seq-nested-lists  new-symbol-nested-lists new-dim-list-nested-lists) ;;  new-seq-flat-list)
       (make-multi-dim-sequence-list1  root  all-dims-spec-list  :recurse-flag recurse-flag 
                                   :return-flat-lists-p return-flat-lists-p))
                                  ;; :node-separator node-separator))

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
        (setf new-seq-flat-list (append new-seq-flat-list strlist))
      ;;end  when, loop
      ))
   (loop
    for symlist in new-symbol-nested-lists
    do
    (when (and (listp symlist) (= (list-length symlist) 1))
      (setf new-symbol-flat-list (append new-symbol-flat-list symlist))
      ;;end  when, loop
      ))
   (loop
    for symdim-list  in new-dim-list-nested-lists
    do
    ;;(break)
    (when (and (listp symdim-list) (= (list-length symdim-list) 1))       
       (when (listp symdim-list)
         (setf new-symdim-flat-list (append new-symdim-flat-list symdim-list)))
         ;;end  whens, loops
         ))
   ;;if lists don't begin as nested, just set flat lists = other lists
   (when (null new-seq-flat-list)
     (setf new-seq-flat-list  new-seq-nested-lists))
   (when (null new-symbol-flat-list)
     (setf new-symbol-flat-list  new-symbol-nested-lists))
   (when (null new-symdim-flat-list)
     (setf new-symdim-flat-list new-dim-list-nested-lists))

    ;;end outer when return-flat-lists-p
    )

   ;;SET ROOT TO LIST OF SYMS--flat or nested?
   (cond
    (set-root-to-flat-list-p 
     (set main-root-sym (list root new-symbol-flat-list)))
    (t
     (set  main-root-sym (list root new-symbol-nested-lists))))    
   
   (values  new-seq-nested-lists  new-symbol-nested-lists new-dim-list-nested-lists new-seq-flat-list new-symbol-flat-list new-symdim-flat-list)
   ;;end when, make-multi-dim-sequence-list
   ))|#
;;TEST
;; (make-multi-dim-sequence-list "Wup" `((5 1 1"")(1 3 1  "-")(1 1 1 "-"  ) (3 1 1  "To" )(1 1 1   "-")(1 2 1 "-")) :return-flat-lists-p T)
;;ALSO WUP= (WUP1-3-1TO1-1-2 WUP1-3-1TO2-1-2 WUP1-3-1TO3-1-2 WUP2-3-1TO1-1-2 WUP2-3-1TO2-1-2 WUP2-3-1TO3-1-2 WUP3-3-1TO1-1-2 WUP3-3-1TO2-1-2 WUP3-3-1TO3-1-2 WUP4-3-1TO1-1-2 WUP4-3-1TO2-1-2 WUP4-3-1TO3-1-2 WUP5-3-1TO1-1-2 WUP5-3-1TO2-1-2 WUP5-3-1TO3-1-2)
;; WORKS=
;;new-seq-nested-lists= (("Wup1-3-1To1-1-2") ("Wup1-3-1To2-1-2") ("Wup1-3-1To3-1-2") ("Wup2-3-1To1-1-2") ("Wup2-3-1To2-1-2") ("Wup2-3-1To3-1-2") ("Wup3-3-1To1-1-2") ("Wup3-3-1To2-1-2") ("Wup3-3-1To3-1-2") ("Wup4-3-1To1-1-2") ("Wup4-3-1To2-1-2") ("Wup4-3-1To3-1-2") ("Wup5-3-1To1-1-2") ("Wup5-3-1To2-1-2") ("Wup5-3-1To3-1-2"))
;;new-symbol-nested-lists= ((WUP1-3-1TO1-1-2) (WUP1-3-1TO2-1-2) (WUP1-3-1TO3-1-2) (WUP2-3-1TO1-1-2) (WUP2-3-1TO2-1-2) (WUP2-3-1TO3-1-2) (WUP3-3-1TO1-1-2) (WUP3-3-1TO2-1-2) (WUP3-3-1TO3-1-2) (WUP4-3-1TO1-1-2) (WUP4-3-1TO2-1-2) (WUP4-3-1TO3-1-2) (WUP5-3-1TO1-1-2) (WUP5-3-1TO2-1-2) (WUP5-3-1TO3-1-2))
;;new-dim-list-nested-lists=[note: the dims are NOT right]= ((("Wup1-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup1-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup1-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup2-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup2-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup2-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup3-3-1To1-1-2" (1 2 1 "-" ""))) ;;(("Wup3-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup3-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup4-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup4-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup4-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup5-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup5-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup5-3-1To3-1-2" (1 2 1 "-" ""))))
;;new-seq-flat-list= ("Wup1-3-1To1-1-2" "Wup1-3-1To2-1-2" "Wup1-3-1To3-1-2" "Wup2-3-1To1-1-2" "Wup2-3-1To2-1-2" "Wup2-3-1To3-1-2" "Wup3-3-1To1-1-2" "Wup3-3-1To2-1-2" "Wup3-3-1To3-1-2" "Wup4-3-1To1-1-2" "Wup4-3-1To2-1-2" "Wup4-3-1To3-1-2" "Wup5-3-1To1-1-2" "Wup5-3-1To2-1-2" "Wup5-3-1To3-1-2")
;;new-symbol-flat-list=  (WUP1-3-1TO1-1-2 WUP1-3-1TO2-1-2 WUP1-3-1TO3-1-2 WUP2-3-1TO1-1-2 WUP2-3-1TO2-1-2 WUP2-3-1TO3-1-2 WUP3-3-1TO1-1-2 WUP3-3-1TO2-1-2 WUP3-3-1TO3-1-2 WUP4-3-1TO1-1-2 WUP4-3-1TO2-1-2 WUP4-3-1TO3-1-2 WUP5-3-1TO1-1-2 WUP5-3-1TO2-1-2 WUP5-3-1TO3-1-2)
;;new-symdim-flat-list = (("Wup1-3-1To1-1-2" (1 2 1 "-" "")) ("Wup1-3-1To2-1-2" (1 2 1 "-" "")) ("Wup1-3-1To3-1-2" (1 2 1 "-" "")) ("Wup2-3-1To1-1-2" (1 2 1 "-" "")) ("Wup2-3-1To2-1-2" (1 2 1 "-" "")) ("Wup2-3-1To3-1-2" (1 2 1 "-" "")) ("Wup3-3-1To1-1-2" (1 2 1 "-" "")) ("Wup3-3-1To2-1-2" (1 2 1 "-" "")) ("Wup3-3-1To3-1-2" (1 2 1 "-" "")) ("Wup4-3-1To1-1-2" (1 2 1 "-" "")) ("Wup4-3-1To2-1-2" (1 2 1 "-" "")) ("Wup4-3-1To3-1-2" (1 2 1 "-" "")) ("Wup5-3-1To1-1-2" (1 2 1 "-" "")) ("Wup5-3-1To2-1-2" (1 2 1 "-" "")) ("Wup5-3-1To3-1-2" (1 2 1 "-" "")))


;;FOR ART2 ------------------------------------------------------------------------
;;OLDER TEST RETURNING FLAT LISTS
;;   (progn (setf out nil *NEW-SEQ-NESTED-LISTS nil) (make-multi-dim-sequence-list "test" '((4 1 1 "C" "F")(3 1 1 "C" "F"))))
;;RESULTS, WORK=
#|("testC1FC1F" "testC1FC2F" "testC1FC3F" "testC2FC1F" "testC2FC2F" "testC2FC3F" "testC3FC1F" "testC3FC2F" "testC3FC3F" "testC4FC1F" "testC4FC2F" "testC4FC3F")
(TESTC1FC1F TESTC1FC2F TESTC1FC3F TESTC2FC1F TESTC2FC2F TESTC2FC3F TESTC3FC1F TESTC3FC2F TESTC3FC3F TESTC4FC1F TESTC4FC2F TESTC4FC3F)
(("testC1FC1F" (3 1 1 "C" "F")) ("testC1FC2F" (3 2 1 "C" "F")) ("testC1FC3F" (3 3 1 "C" "F")) ("testC2FC1F" (3 1 1 "C" "F")) ("testC2FC2F" (3 2 1 "C" "F")) ("testC2FC3F" (3 3 1 "C" "F")) ("testC3FC1F" (3 1 1 "C" "F")) ("testC3FC2F" (3 2 1 "C" "F")) ("testC3FC3F" (3 3 1 "C" "F")) ("testC4FC1F" (3 1 1 "C" "F")) ("testC4FC2F" (3 2 1 "C" "F")) ("testC4FC3F" (3 3 1 "C" "F")))|#
;;TEST RETURNING NESTED LISTS
;;   (progn (setf out nil *NEW-SEQ-NESTED-LISTS nil) (make-multi-dim-sequence-list "test" '((4 1 1 "C" "F")(3 1 1 "C" "F"))))
;;works= (("testC1FC1F" "testC1FC2F" "testC1FC3F") ("testC2FC1F" "testC2FC2F" "testC2FC3F") ("testC3FC1F" "testC3FC2F" "testC3FC3F") ("testC4FC1F" "testC4FC2F" "testC4FC3F"))
;;((TESTC1FC1F TESTC1FC2F TESTC1FC3F) (TESTC2FC1F TESTC2FC2F TESTC2FC3F) (TESTC3FC1F TESTC3FC2F TESTC3FC3F) (TESTC4FC1F TESTC4FC2F TESTC4FC3F))
;;((("testC1FC1F" (3 1 1 "C" "F")) ("testC1FC2F" (3 2 1 "C" "F")) ("testC1FC3F" (3 3 1 "C" "F"))) (("testC2FC1F" (3 1 1 "C" "F")) ("testC2FC2F" (3 2 1 "C" "F")) ("testC2FC3F" (3 3 1 "C" "F"))) (("testC3FC1F" (3 1 1 "C" "F")) ("testC3FC2F" (3 2 1 "C" "F")) ("testC3FC3F" (3 3 1 "C" "F"))) (("testC4FC1F" (3 1 1 "C" "F")) ("testC4FC2F" (3 2 1 "C" "F")) ("testC4FC3F" (3 3 1 "C" "F"))))
;;
;;
;; REPLACED BY make-multi-dim-symbol-list1 IN U-CS-ART?
;;MAKE-MULTI-DIM-SEQUENCE-LIST1
;;
;;ddd
#|(defun make-multi-dim-sequence-list1  (root all-dims-spec-list &key  (recurse-flag 99)
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
       (new-seq-nested-lists1)
       (new-symbol-nested-lists1)
       (new-dim-list-nested-lists1)
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
       ))|#
;;TEST
;;  MUST TEST ABOVE MAIN FUNCTION , make-multi-dim-sequence-list






;;MAKE-DIM-SEQUENCE-LIST
;;
;;ddd
(defun make-dim-sequence-list (root n-dim-elements &key sym-vals-list
                                    (begin-n 1)(dim-incr 1)
                                          (begin-str "") (end-str "")
                                          dim-elements-list  (make-string-p T) (make-list-p T)
                                           set-sym-to-vals-p
                                          (make-symbol-p T))
  "In  Unless make-list-p, returns a list of new sequences of root,begin-str,dim-element,end-str for each value of dim-element (either begin-n to n-dim-elements or from the list dim-elements-list. RETURNS (values new-seq-list new-symbol-list new-dim-list-list ).  If set-sym-to-vals-p, then sets the new-symbol = (init-root dims-list n-dim-elements), eg. (\"Wup\" (2 5) 7)."
  (let*
      ((cur-dim-element)
       ;SSS IS THIS RIGHT??
       (end-n (floor (/  (+ (- n-dim-elements 1) begin-n) dim-incr)))
    ;;was (floor (/ (+ (- n-dim-elements begin-n) 1) dim-incr)) couldn't begin with other than 1
       (dim-n)
       (dim-end)
       (pre-seq)
       (new-seq)
       (new-dim-list)
       (new-symbol)
       (sym-vals)
         ;; (initial-root dim-list n-elements value(to be added))
       (return-sym-vals-list)
       (new-seq-list)
       (new-symbol-list)
       (new-dim-list-list)
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
       (setf sym-vals (list (car alpha-list) dim-list  n-dim-elements))
       ;;SETS THE NEW-SYMBOL TO SYM-VALS
       (set new-symbol sym-vals)))
     ;;end loop
     )
    
    (values new-seq-list new-symbol-list new-dim-list-list)
    ;;end let, make-dim-sequence-list      
    ))
;;TEST
;;FOR ART3 --------------------------------------------------------
;; (make-dim-sequence-list "wup1-2" 4 :begin-str "-"  :end-str "To")
;; works= ("wup1-2-1To" "wup1-2-2To" "wup1-2-3To" "wup1-2-4To")
;;(WUP1-2-1TO WUP1-2-2TO WUP1-2-3TO WUP1-2-4TO)
;;(("wup1-2-1To" (4 1 1 "-" "To")) ("wup1-2-2To" (4 2 1 "-" "To")) ("wup1-2-3To" (4 3 1 "-" "To")) ("wup1-2-4To" (4 4 1 "-" "To")))
;;following args have no useful effects? :sym-vals-list '("wup" (7 8 9)) :dim-elements-list '(11 12))


;; (make-multi-dim-sequence-list "Wup" `((5 1 1"")(1 3 1  "-")(1 1 1 "-"  ) (3 1 1  "To" )(1 1 1   "-")(1 2 1 "-")))
;; WORKS= (("Wup1-3-1To1-1-2") ("Wup1-3-1To2-1-2") ("Wup1-3-1To3-1-2") ("Wup2-3-1To1-1-2") ("Wup2-3-1To2-1-2") ("Wup2-3-1To3-1-2") ("Wup3-3-1To1-1-2") ("Wup3-3-1To2-1-2") ("Wup3-3-1To3-1-2") ("Wup4-3-1To1-1-2") ("Wup4-3-1To2-1-2") ("Wup4-3-1To3-1-2") ("Wup5-3-1To1-1-2") ("Wup5-3-1To2-1-2") ("Wup5-3-1To3-1-2"))
;;  ((WUP1-3-1TO1-1-2) (WUP1-3-1TO2-1-2) (WUP1-3-1TO3-1-2) (WUP2-3-1TO1-1-2) (WUP2-3-1TO2-1-2) (WUP2-3-1TO3-1-2) (WUP3-3-1TO1-1-2) (WUP3-3-1TO2-1-2) (WUP3-3-1TO3-1-2) (WUP4-3-1TO1-1-2) (WUP4-3-1TO2-1-2) (WUP4-3-1TO3-1-2) (WUP5-3-1TO1-1-2) (WUP5-3-1TO2-1-2) (WUP5-3-1TO3-1-2))
;;  ((("Wup1-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup1-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup1-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup2-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup2-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup2-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup3-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup3-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup3-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup4-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup4-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup4-3-1To3-1-2" (1 2 1 "-" ""))) (("Wup5-3-1To1-1-2" (1 2 1 "-" ""))) (("Wup5-3-1To2-1-2" (1 2 1 "-" ""))) (("Wup5-3-1To3-1-2" (1 2 1 "-" ""))))

;;FOR GENERAL USE -----------------------------------------
;;  (make-dim-sequence-list "root" 1 :begin-str "C" :end-str "F" :set-sym-to-vals-p T)
;; 
;;  (make-dim-sequence-list "root" 3 :begin-str "C" :end-str "F" :set-sym-to-vals-p T)
;;  = ("rootC1F" "rootC2F")  (ROOTC1F ROOTC2F)  (("rootC1F" (3 1 1 "C" "F")) ("rootC2F" (3 2 1 "C" "F")) ("rootC3F" (3 3 1 "C" "F")))
;;  also: CL-USER 18 > ROOTC2F  = ("root" (2) 3)
;; SSS START HERE, NOTE: RETURNS (4 1 2 "C" "F") IN ("rootC2F" (4 1 2 "C" "F"))
;;   WHAT DO I WANT IT TO RETURN FOR TOP FUNCTION?
;;  (make-dim-sequence-list "root4" 3 :begin-str "C" :end-str "F" :set-sym-to-vals-p T)
;;
;;  NOTE: returns only LAST dim values = ("root4C1F" "root4C2F" "root4C3F")  (ROOT4C1F ROOT4C2F ROOT4C3F)  (("root4C1F" (3 1 1 "C" "F")) ("root4C2F" (3 2 1 "C" "F")) ("root4C3F" (3 3 1 "C" "F")))
;;
;;  (make-dim-sequence-list "root" 4 :begin-str "C" :end-str "F" :dim-incr 2)
;;   works= ("rootC1F" "rootC2F")  (ROOTC1F ROOTC2F)  (("rootC1F" (4 1 2 "C" "F")) ("rootC2F" (4 2 2 "C" "F")))
;;  




;;MAKE-ARTSYMS-FROM-DIMS
;;
;;ddd
(defun make-artsyms-from-dims (rootstr dimslists 
                                       &key superord-artsyms (node-separator *art-node-separator))
  "In U-symbol-trees.  Makes a list of artsyms from dimslists (not dimspec-lists) eg. (I L 2) (3 2 2).  Also, appends list of superord-artsyms values to include these new artsyms. INPUT: Can be in form of  (I L F), ((I L F)), (((I L F)(I L F))), or (I L F TO I L F)  RETURNS (values newsym-list newsymstr-list) RETURNS (values newsym-list newsymstr-list). "
  (let
      ((newsym)
       (newsymstr)
       (dims)
       (newsym-list)
       (newsymstr-list)
       )
    (loop
     for dimlist in dimslists
     do
     (when (listp dimlist)
     (cond
      ;;is a dimlist (eg (I L F) or (I L F TO I L F)
      ((not (listp (car dimlist)))
       (setf dims dimlist))            
      ;;is double list of above
      ((not (listp (caar dimlist)))
       (cond
         ;; is of form (((I L F)(I L F)))
        ((= (list-length dimlist) 2)
         (setf dims (append (car dimlist) (list node-separator) (second dimlist))))
         (t 
          (setf dims (car dimlist)))))
      (t nil))

     (when dims
       (multiple-value-setq (newsym newsymstr)
            (make-dim-symbol rootstr dims))
       (setf newsym-list (append newsym-list (list newsym))
              newsymstr-list (append newsymstr-list (list newsymstr)))
       (set newsym (list rootstr dims nil nil))
       ;;end when
       )

     ;;end outer when, loop
     ))
    ;;when superord-artsyms
    (when (and superord-artsyms newsym-list)
      (loop
       for supersym in superord-artsyms
       do
       (setsubsyms supersym newsym-list)
       ;;end loop, when
       ))
    (values newsym-list newsymstr-list)
    ;;end let, make-artsyms-from-dims
    ))
;;TEST
;;  (make-artsyms-from-dims  "Wup" '(((I L 1)(I L 2))((I L 2)(I L 1))))
;; works=  (WUPI-L-1TOI-L-2 WUPI-L-2TOI-L-1)  ("WupI-L-1TOI-L-2" "WupI-L-2TOI-L-1")
;; also: WUPI-L-2TOI-L-1  = ("Wup" (I L 2 TO I L 1) NIL NIL)
;; (make-artsyms-from-dims  "Test1"  '(((I 4 3) (1 2 4)) ((I 4 3) (2 2 4)) ((I 4 3) (3 2 4)))) ;;was ((((I 4 3) (1 2 4)) ((I 4 3) (2 2 4)) ((I 4 3) (3 2 4))))

;;LATEST
;;  (make-artsyms-from-dims "TEST1"  '( ((2 3 2) (1 1 3)) ((2 3 2) (2 1 3)) ((2 3 2) (3 1 3))) :superord-artsyms  '(TEST1I-3-2TOI-1-3 TEST1I-3-2TOI-2-3))
;;superord-syms= (TEST1I-3-2TOI-1-3 TEST1I-3-2TOI-2-3 TEST1I-4-2TOI-1-3 TEST1I-4-2TOI-2-3 TEST1I-5-2TOI-1-3 TEST1I-5-2TOI-2-3)
;;dimlists=  ((2 3 2) (1 1 3)) ((2 3 2) (2 1 3)) ((2 3 2) (3 1 3)) ((3 3 2) (1 1 3)) ((3 3 2) (2 1 3)) ((3 3 2) (3 1 3)) ((4 3 2) (1 1 3)) ((4 3 2) (2 1 3)) ((4 3 2) (3 1 3)))
;;RESULTS=
;;(TEST12-3-2TO1-1-3 TEST12-3-2TO2-1-3 TEST12-3-2TO3-1-3)   ("test12-3-2TO1-1-3" "test12-3-2TO2-1-3" "test12-3-2TO3-1-3")
;;also: TEST12-3-2TO1-1-3 = ("test1" (2 3 2 TO 1 1 3) NIL NIL)
;; and superord: TEST1I-3-2TOI-2-3 = ("Test1" (I 3 2 TO I 2 3) NIL NIL ((TEST12-3-2TO1-1-3 TEST12-3-2TO2-1-3 TEST12-3-2TO3-1-3)))






;;MAKE-ART-TOPSYM
;;
;;ddd
(defun make-art-topsym (root dims &key (set-symvals-if-unbound-p T) 
                             value subsyms (art-index-syms *art-index-syms) 
                             value subsyms)
  "In U-symbol-trees. Makes a  topsym symbol eg. WUPI-L-FTOI-L-F and if it is unbound, sets it to symvals = (root dims nil default-value default-subsyms) if set-symvals-if-unbound-p. If already bound, returns OLD value and subsyms."
  (let
      ((topsym)
       (symvals)
       (n-dims)(n-items )(sublists)( n-ints)
       (n-topdims (list-length art-index-syms))
       (pathsym-p)
       (subdims-ns)
       (n-subdims 0)
       (topsubdims)
       (topsym-str)
       )

    (multiple-value-setq (n-dims  n-items subdims-ns n-ints)
        (dimlist-length dims))

    (when (> (list-length subdims-ns) 1)
      (setf pathsym-p T
            n-subdims (car subdims-ns)))

    (cond
     (pathsym-p
      (setf topsubdims (butlast art-index-syms (- n-topdims n-subdims) )
            topdims (append  topsubdims (list 'TO) topsubdims)))
      (t
       (setf topdims (butlast art-index-syms (- n-topdims n-dims) ))))

    (multiple-value-setq (topsym topsym-str symvals)
        (make-dim-symbol root topdims :value value :subsyms subsyms))

    (values topsym symvals topsym-str)
    ;;end let, make-art-topsym
    ))
;;TEST
;; (make-art-topsym "test" '(1 2 3) :value 0.66 :subsyms '(this that))
;; works= TESTI-L-F  ("test" (I L F) NIL 0.66 (THIS THAT))  "testI-L-F"
;; 
;; (make-art-topsym "test2d" '(1 2 3 to 4 5 6) :value 0.99 :subsyms '(this that))
;; works= TEST2DI-L-FTOI-L-F  ("test2d" (I L F TO I L F) NIL 0.99 (THIS THAT))  "test2dI-L-FTOI-L-F" 
;;also: TEST2DI-L-FTOI-L-F = ("test2d" (I L F TO I L F) NIL 0.99 (THIS THAT))






;;FIND-ART-CLASS-TOPSYM
;;
;;ddd
(defun find-art-class-topsym (sym &optional dimspecs-list 
                                  &key (art-index-syms *art-index-syms)
                                  (node-separator *art-node-separator) 
                                  (all-art-rootsyms *all-art-class-rootsyms)
                                  ;;not needed??(all-art-syms *all-art-symbols)
                                  )
  "In U-symbol-trees, RETURNS (values class-topsym n-dims path-var-p  n-dims1 n-dims2) (eg. XI-L-F or WUPI-L-FTOI-L-F) dim-specs-list required if sym is just the root str or sym."
  (let*
      ((symvals)
       (dims )
       (rootstr)
       (class-topsymstr)
       (class-topsym)
       (topdims)
       (dims-list)
       (dims1)
       (dims2)
       (n-indsyms)
       (class-topsym)( n-dims)( n-ints)( path-var-p)(n-dims1 0)( n-dims2 0)
       (n-ints1)( n-ints2)(n-items)
       ) 
    (cond
     ;;if sym is a class sym (eg. INPUT X WUP)
     ((member sym all-art-rootsyms)
      (setf symvals (eval sym)
            rootstr (first symvals)
            dimspecs-list (second symvals))
      (multiple-value-setq (dims  n-dims  path-var-p  n-dims1 n-dims2)     
          (make-top-dims-from-dimspecs dimspecs-list))
      (setf class-topsym (make-dim-symbol sym dims)))
     ;;If sym is an unbound sym or root.
     ((or (stringp sym)(not (symbolp sym))(not (boundp sym)))
      (setf sym (my-make-symbol sym)
            dims (make-top-dims-from-dimspecs dimspecs-list)
            class-topsym (make-dim-symbol sym dims)) )
     ;;for sym that evals to symvals
     (t
      (setf symvals (eval sym)
            dims (second symvals)
            rootstr (first symvals)
            n-indsyms (list-length art-index-syms))

      (multiple-value-setq (n-dims n-ints path-var-p  n-dims1 n-dims2 n-ints1 n-ints2  n-items)
          (find-dims-info dims))
      
      (cond
       (path-var-p
        (setf  dims1 (butlast art-index-syms (- n-indsyms n-dims1))
               dims2 (butlast art-index-syms (- n-indsyms n-dims2))
               topdims   (format nil "~{~a~^-~}~A~{~a~^-~}" dims1 node-separator dims2)))
       (t (setf  dims1 (butlast art-index-syms  (- n-indsyms n-dims))
                 topdims  (format nil "~{~a~^-~}" dims1 ))))

      (setf class-topsymstr (format nil "~A~A" rootstr topdims)
            class-topsym (my-make-symbol class-topsymstr))
      ;;(BREAK)    
      ;;end t, cond
      ))
            
    (values class-topsym n-dims path-var-p  n-dims1 n-dims2)
    ;;end let, find-art-class-topsym
    ))
;;TEST
;;  (find-art-class-topsym 'XI-2-2) = XI-L-F 3 NIL 0 0
;;  (find-art-class-topsym  'WUPI-3-2TOI-1-3) = 6 T 3 3
;;  (find-art-class-topsym 'X '((5 1 1)(2 1 1)(3 2 1))) = XI-L-F 3 NIL 0 0
;;  (find-art-class-topsym 'WUP  '((5 1 1)(2 1 1)(3 2 1) TO ((3 1 1)(2 1 1)(3 2 1))))
;;  =  WUPI-L-FTOI-L-F  6 T 3 3


;;Help
;; (format nil "~{~a, ~}" (list 1 2 3))
;; (format nil "~{~a~^-~}TO~{~a~^-~}" '(1 2 3) '(4 5 6)) = "1-2-3TO4-5-6"




;;FIND-ART-CLASS-ROOTSYM
;; Egs X, Y, WUP, WDN  second is DIM-SPECS-LIST NOT DIMS-LIST
;;
;;ddd
(defun find-art-class-rootsym (sym &optional dimspecs-list 
                                  &key (make-sym-if-not-found-p T)
                                  (art-index-syms *art-index-syms)
                                  (node-separator *art-node-separator) 
                                  (all-art-rootstrs *all-art-symbol-strings)
                                  (all-art-rootsyms *all-art-class-rootsyms)
                                  (all-art-syms *all-art-symbols))
  "In U-symbol-trees, RETURNS (values class-rootsym symvals) (eg. X or WUP)   Second in symvals is DIM-SPECS-LIST NOT DIMS-LIST"
  (let*
      ((symvals)
       (dims )
       (rootstr)
       (class-rootsymstr)
       (class-rootsym)
       (topdims)
       (dims1)
       (dims2)
       (n-indsyms)
       (class-rootsym)( n-dims)( n-ints)( path-var-p)(n-dims1 0)( n-dims2 0)
       (n-ints1)( n-ints2)(n-items)
       ) 
    (cond
     ;;if sym is a class sym (eg. INPUT X WUP)
     ((member sym all-art-rootsyms)
      (setf class-rootsym  sym))
     ;;If sym is an unbound sym or root.
     ;;((or (stringp sym)(not (symbolp sym))(not (boundp sym)))
     (t
      (setf  rootstr (find-best-match  sym all-art-rootstrs)
             class-rootsym (my-make-symbol rootstr))))     

    (cond
     ((boundp class-rootsym)
      (setf  symvals (eval class-rootsym)))
     ((and make-sym-if-not-found-p dimspecs-list)
      (unless rootstr (setf rootstr (format nil "~A" class-rootsym)))
      (setf symvals (list rootstr dimspecs-list))
      (set class-rootsym symvals))
     (t nil))
  
    (values class-rootsym symvals) 
    ;;end let, find-art-class-rootsym
    ))
;;TEST
;;  (find-art-class-rootsym 'XI-2-2) 
;; works= X   ("X" ((5 1 1) (1 3 1) (1 3 1)) XI-L-3-POINTS NIL (XI-L-F))
;;  (find-art-class-rootsym  'WUPI-3-2TOI-1-3) 
;; works= WUP    ("Wup" (((9 1 1) (1 3 1) (1 2 1)) TO ((5 1 1) (1 1 1) (1 3 1))) WUP-POINTS NIL (WUPI-L-FTOI-L-F))
;;  (find-art-class-rootsym 'X '((5 1 1)(2 1 1)(3 2 1)))
;; works= X  ("X" ((5 1 1) (1 3 1) (1 3 1)) XI-L-3-POINTS NIL (XI-L-F
;;  (find-art-class-rootsym 'WUP  '((5 1 1)(2 1 1)(3 2 1) TO ((3 1 1)(2 1 1)(3 2 1))))
;; works= WUP     ("Wup" (((9 1 1) (1 3 1) (1 2 1)) TO ((5 1 1) (1 1 1) (1 3 1))) WUP-POINTS NIL (WUPI-L-FTOI-L-F))
;; MAKE NEW SYM
;;  (find-art-class-rootsym 'TEMP    '((5 1 1)(2 1 1)(3 2 1)) )
;;works= TEMP    ("TEMP" ((5 1 1) (2 1 1) (3 2 1)))

;; 



;;FIND-CLASS-DIMSPECS
;;
;;ddd
(defun find-class-dimspecs (sym)
  "In U-symbol-trees,  Finds class-dimspecs--which apply to all instances in the class. INPUT= any sym in the class. RETURNS (values class-dimspecs rootsym). Finds/uses class-rootsym."
  (let
      ((class-dimspecs)
       )
  (multiple-value-bind (rootsym symvals)
       (find-art-class-rootsym sym)
    (setf class-dimspecs (second symvals))

    (values class-dimspecs rootsym)
    ;;end let,mvb, find-class-dimspecs
    )))
;;TEST
;;  (find-class-dimspecs 'xi-2-2)
;; works= ((5 1 1) (1 3 1) (1 3 1))   X
    
    

        




;;MAKE-TOP-DIMS-FROM-DIMSPECS
;;
;;ddd
(defun make-top-dims-from-dimspecs (dimspecs-list
                                    &key (art-index-syms *art-index-syms)
                                  (node-separator *art-node-separator))
  "In U-symbol-trees, RETURNS (values (dims  n-dims  path-var-p  n-dims1 n-dims2) dims= a class top dimslist  (eg. (I L F) or (I L F M)."
  (let 
      ((n-dims (list-length dimspecs-list))
       (path-sym-p (member node-separator dimspecs-list :test 'equal ))
       (dimslist)
       (n-dims1 0)
       (n-dims2 0)
       (dims2)
       )
    (cond
     (path-sym-p
      (setf path-sym-p T
           n-dims1 (list-length (first dimspecs-list))
            n-dims2 (list-length (third dimspecs-list))     
            n-dims (+ n-dims1 n-dims2)
            dims2 (butlast art-index-syms (- 4 n-dims2))
            dimslist (append dims2 (list node-separator) dims2)))
     (t
      (setf dimslist (butlast art-index-syms (- 4 n-dims)))))    
    (values dimslist  n-dims  path-sym-p  n-dims1 n-dims2)
    ;;end let, make-top-dims-from-dimspecs
    ))
;;TEST
;;  (make-top-dims-from-dimspecs '((5 1 1)(2 1 1)(3 2 1))) = (I L F)
;;  (make-top-dims-from-dimspecs '(((5 1 1)(2 1 1)(3 2 1)) TO ((3 1 1)(2 1 1)(3 2 1)))) = (I L F TO I L F)


;;DIMLIST-LENGTH
;;modified for art3
;;
;;ddd
(defun dimlist-length (dimlist &key n-ints-first-p  (dim-letters *art-index-syms) 
                               (dim-separators (list *art-node-separator -)))
  "In U-symbol-trees, RETURNS (values n-dims  n-items sublist-ns n-ints) for a dimlist (eg. (1 2 3 TO 4 5 6)). SUBLIST-NS is a list of  n-subitems key n-subitems, etc. NOTE: N-DIMS at end is total n-dims incl letters. N-INTS-FIRST-P puts n-ints first and n-dims last in values returned."
  (let
      ((n-dims 0)
       (n-ints 0)
       (n-items (list-length dimlist))
       (n-subs 0)
       (sublist-ns)
       (sublist-indexes)
       )
    (dolist (item dimlist)
      (cond
       ((integerp item)
        (incf n-dims)
        (incf n-ints)
        (incf n-subs)
        )
       ((my-member item dim-letters)
        (incf n-dims)
        (incf n-subs))
       (t (setf sublist-ns (append sublist-ns (list n-subs item))
                n-subs 0)))
      )
    (setf  sublist-ns (append sublist-ns (list n-subs)))

    (cond
     ((null n-ints-first-p)
      (values n-dims   n-items sublist-ns n-ints))
     (t 
      (values  n-ints n-items sublist-ns n-dims)))
    ;;end let, dimlist-length
    ))
;;TEST
;;works
;;  (dimlist-length '(1 2 3 TO 4 5 6)) = 6 7 (3 TO 3) 6
;;  (dimlist-length '(I 2 3 TO I 5 6)) =  6 7 (3 TO 3) 4
;;  (dimlist-length '(I 2 3 TO I 5 6) :n-ints-first-p T)  = 4 7 (3 TO 3) 6
;;  (dimlist-length '(1 2 3)) = 3 3 (3) 3







;;FIND-SUPERORD-INDEX
;;
;;ddd
(defun find-super-subord-index (sym &key (superord-subord-lists '((Top M)(M F)(F L)(L I)(I B))) return-subord-index-p)
  "In U-symbol-trees. RETURNS If :return-subord-index-p returns subord-sym otherwise superord-sym."
  (let
      ((superord-sym)
       (subord-sym)
       (return-sym)
       )
    (loop
     for list in superord-subord-lists
     do
     (setf  superord-sym (first list)
           subord-sym (second list))
     (cond
      (return-subord-index-p
       (when (equal superord-sym sym)
         (setf return-sym subord-sym)
         (return)))
      (t 
       (when (equal subord-sym sym)
         (setf return-sym superord-sym)
         (return))))
     ;;end loop
     )

    return-sym
    ))
;;TEST
;; (find-sub-superord-index 'i)
;; (find-sub-superord-index 'i :return-subord-index-p nil)       
                                                                                                                                       







;;=================== INFO TEST AREA =================
;;
;;CURRENT RESULTS
;; FOR WUPI-L-FTOI-L-F  SPEC= ((5 1 1)(1 3 1)(1 2 1) TO (3 1 1)(1 1 1)(1 3 1))
;;
#|CL-USER 47 > (progn (setf *out1 nil *out2 nil  WUPI-L-FTOI-L-F '("Wup" (I L F TO I L F)))(make-node-dimsym-tree '(WUPI-L-FTOI-L-F) '("Wup" ((5 1 1)(1 3 1)(1 2 1) TO (3 1 1)(1 1 1)(1 3 1)))))
;;RESULT
((WUPI-L-FTOI-L-3)
 (WUPI-L-FTOI-1-3)
 (WUPI-L-FTO1-1-3 WUPI-L-FTO2-1-3 WUPI-L-FTO3-1-3) 
(WUPI-L-2TO1-1-3) 
(WUPI-3-2-TO1-1-3) 
(WUP1-3-2TO1-1-3 WUP2-3-2TO1-1-3 WUP3-3-2TO1-1-3 WUP4-3-2TO1-1-3 WUP5-3-2TO1-1-3)
 (WUPI-L-2TO2-1-3) 
(WUPI-3-2-TO2-1-3) 
(WUP1-3-2TO2-1-3 WUP2-3-2TO2-1-3 WUP3-3-2TO2-1-3 WUP4-3-2TO2-1-3 WUP5-3-2TO2-1-3) 
(WUPI-L-2TO3-1-3) 
(WUPI-3-2-TO3-1-3) 
(WUP1-3-2TO3-1-3 WUP2-3-2TO3-1-3 WUP3-3-2TO3-1-3 WUP4-3-2TO3-1-3 WUP5-3-2TO3-1-3))
NIL NIL 3 3 NIL NIL

VALUES FOR EACH SYMBOL:---------------------------------------
CL-USER 48 > WUPI-L-FTOI-L-F
("Wup" (I L F TO I L F) NIL (WUPI-L-FTOI-L-3))
CL-USER 49 > WUPI-L-FTOI-L-3
("Wup" (I L F TO I L 3) NIL (WUPI-L-FTOI-1-3))
CL-USER 50 > WUPI-L-FTOI-1-3
("Wup" (I L F TO I 1 3) NIL (WUPI-L-FTO1-1-3 WUPI-L-FTO2-1-3 WUPI-L-FTO3-1-3))
CL-USER 51 > WUPI-L-FTO1-1-3
("Wup" (I L F TO 1 1 3) NIL (WUPI-L-2TO1-1-3))
CL-USER 52 > WUPI-L-FTO2-1-3
("Wup" (I L F TO 2 1 3) NIL (WUPI-L-2TO2-1-3))
CL-USER 53 > WUPI-L-FTO3-1-3
("Wup" (I L F TO 3 1 3) NIL (WUPI-L-2TO3-1-3))
CL-USER 54 > WUPI-L-2TO1-1-3
("Wup" (I L 2 TO 1 1 3) NIL (WUPI-3-2-TO1-1-3))
CL-USER 55 > WUPI-L-2TO2-1-3
("Wup" (I L 2 TO 2 1 3) NIL (WUPI-3-2-TO2-1-3))
CL-USER 56 > WUPI-L-2TO3-1-3
("Wup" (I L 2 TO 3 1 3) NIL (WUPI-3-2-TO3-1-3))
CL-USER 57 > WUPI-3-2-TO1-1-3
("Wup" (I 3 2 TO 1 1 3) NIL (WUP1-3-2TO1-1-3 WUP2-3-2TO1-1-3 WUP3-3-2TO1-1-3 WUP4-3-2TO1-1-3 WUP5-3-2TO1-1-3))
CL-USER 58 > WUPI-3-2-TO2-1-3
("Wup" (I 3 2 TO 2 1 3) NIL (WUP1-3-2TO2-1-3 WUP2-3-2TO2-1-3 WUP3-3-2TO2-1-3 WUP4-3-2TO2-1-3 WUP5-3-2TO2-1-3))

CL-USER 59 > WUPI-3-2-TO3-1-3
("Wup" (I 3 2 TO 3 1 3) NIL (WUP1-3-2TO3-1-3 WUP2-3-2TO3-1-3 WUP3-3-2TO3-1-3 WUP4-3-2TO3-1-3 WUP5-3-2TO3-1-3))
|#


#|
EG OF A 2-DIM ART TREE      I32TOI13

            WUPI-L-FTOI-L-F SPEC= ((5 1 1)(1 3 1)(1 2 1) TO (3 1 1)(1 1 1)(1 3 1))
             ILF TO ILF
    ILF TO IL3
    IL2TO IL3
    I32TOI13
    I32TO113  I32TO213  I32TO313
    132TO113 232TO213  332TO313 432TO113  532TO213  

FOR:  WUPI-L-FTOI-L-F SPEC= ((5 1 1)(1 3 1)(1 2 1) TO (3 1 1)(1 1 1)(1 3 1))
((WUPI-L-FTOI-L-3)
 (WUPI-L-FTOI-1-3) 
 (WUPI-L-FTO1-1-3 WUPI-L-FTO2-1-3 WUPI-L-FTO3-1-3) 
 (WUPI-L-2TO1-1-3) 
    MISSING IS (WUPI-L-2TOI-L-3)
    MISSING IS (WUPI-3-2TOI-1-3)
 (WUPI-3-2-TO1-1-3)
 (WUP1-3-2TO1-1-3 WUP2-3-2TO1-1-3 WUP3-3-2TO1-1-3 WUP4-3-2TO1-1-3 WUP5-3-2TO1-1-3) 
(WUPI-L-2TO2-1-3) 
(WUPI-3-2-TO2-1-3) 
(WUP1-3-2TO2-1-3 WUP2-3-2TO2-1-3 WUP3-3-2TO2-1-3 WUP4-3-2TO2-1-3 WUP5-3-2TO2-1-3) 
(WUPI-L-2TO3-1-3) 
(WUPI-3-2-TO3-1-3) 
(WUP1-3-2TO3-1-3 WUP2-3-2TO3-1-3 WUP3-3-2TO3-1-3 WUP4-3-2TO3-1-3 WUP5-3-2TO3-1-3))


ALL POSSIBLE FOR SPEC:  ((5 1 1)(1 3 1)(1 2 1) TO (3 1 1)(1 1 1)(1 3 1))
NOTE:  MAKES FOR VERY TOPHEAVY HIERARCHY FOR JUST 15 INSTANCES

TOP LEVEL:
ILFTOILF  (ILFTOIL3 IL2TOILF)
SUBLEVELS:  VALS
ILFTOIL3  (IL2TOIL3) NOTE: instances REDUNDANT bec only one field for each
IL2TOILF  (IL2TOI13)

IL2TOIL3 balanced 2,1 (IL2TOI13 I32TOIL3)

IL2TOI13   2,1TO1,2 (I32TOI13)  NOTE: REDUNDANT one layer each
I32TOIL3   1,2TO2,1 (I32TOI13)

I32TOI13 balanced 1,2 (incl all 8: I32TO113 I32TO213 I32TO213  
132TOI13  232TOI13 332TOI13 432TOI13 532TOI13)

I32TO113 (132TO113  232TO113 332TOI13 432TO113 532TO113)
I32TO213 (132TO213  232TO213 332TO213 432TO213 532TO213)
I32TO213 (132TO313  232TO313 332TO313 432TO313 532TO313)

132TOI13 (132TO113 132TO213 132TO313)
232TOI13 (232TO113 232TO213 232TO313)
332TOI13 (332TOI13 332TO213 332TO313)
432TOI13 (432TO113 432TO213 432TO313)
532TOI13 (532TO113 532TO213 532TO313)

BOTTOM LEVEL: balanced 0,3
132TO113  232TO113 332TOI13 432TO113 532TO113
132TO213  232TO213 332TO213 432TO213 532TO213
132TO313  232TO313 332TO313 432TO313 532TO313

GOAL 
IF 4 TO 4 DIMS (SUB1 = S11 S12 S13 S14)  TO  (SUB2= S21 S22 S23 S24)
ORDER:  S24, S14;  S23, S13;  S22, S12;  S21, S11  (OR REVERSE S1 FIRST)
CREATE LETTER FOR EACH STAGE (NOT JUST INTEGERS)

STEP1: MATCH index-letter with corresponding DIM-SPEC 
S11=(I (5 1 1))
S12=(L (1 3 1))
S13=(F (1 2 1))
S21=(I (3 1 1))
S22=(L (1 1 1))
S23=(F (1 3 1))
STEP2:
START WITH ROOT  I-L-FTOI-L-F
1.replace S23, keep rest, (append all to I-L-FTOI-L-F)
2.replace S13
|#  





;;PRE VERSION THAT USED SORT-KEYLISTS-BY-KEY -----------
;;
;;
#|(defun sort-dimlists-by-level (level dimlists &key (sort-dim-n 1)
                                     (sort-groups 'ascending))
  "In U-symbol-trees groups dimlists by same dim-n number or letter. RETURNS (values ascending-lists descending-lists ). level can be a dim-n (1-4) or  level I-M.  sort-groups-p causes groups to be sorted either 'ascending or 'descending by sort-dim-n."
  (let
      ((label)
       ;; (labels-list)
       (new-group)
       (grouped-dimlists)
       (new-grouped-dimlists)
       (labeled-groups)
       (nth) 
       (group-found-p)
       (ascending-p)
       )
    (cond
     ((setf nth (find-list-element-n level *art-index-syms)) NIL)
     (t (setf nth (- level 1))))

    (loop
     for dimlist in dimlists
     do
     (setf  label (nth nth dimlist))
     ;;(afout 'out (format nil "label= ~A" label))
 
     (cond
      (grouped-dimlists
       (loop
        for group in grouped-dimlists
        do
        ;;(afout 'out (format nil "group= ~A" group))

        (cond
         ((get-key-value-in-nested-lists `((,label ,nth)) group)
          ;;(break)
          (setf new-group (append group (list dimlist))   ;;eg ((1 3 2)(2 3 2))
                grouped-dimlists (replace-list-item  new-group group grouped-dimlists)
                group-found-p T)
          )
         (t nil))
         ;;  (replace-list-item  '((1 3 2)(2 3 2)) '((1 3 2)) '(((1 3 2))))
#|         (t (setf new-group   (list dimlist)
                  grouped-dimlists (append grouped-dimlists (list new-group)))))|#

         ;;(afout 'out (format nil "group= ~A new-group= ~A grouped-dimlists= ~A" group new-group grouped-dimlists))               
        ;;(break)
        (setf new-group nil)
        ;;end inner loop
        )
       ;;end grouped-dimlists
       )
      (t nil))
         ;(setf grouped-dimlists (list (list dimlist)))))
    (cond
     ((null group-found-p)
      (setf grouped-dimlists (append grouped-dimlists (list (list dimlist)))))
     (t (setf group-found-p nil)))
            
       
       ;;(break)
     ;;end outer loop
     )   
    ;;TO SORT WITHIN EACH GROUP (by sort-dim-n) ascending or descending
    (when sort-groups
      (when (equal sort-groups 'ascending)
        (setf ascending-p T))
      (loop
       for group in grouped-dimlists
       do
       (multiple-value-setq (descending-group ascending-group)
            (my-sort-lists  (- sort-dim-n 1) group :ascending-p ascending-p))
       (cond
        ((equal sort-groups 'ascending)
             (setf new-grouped-dimlists
                   (append new-grouped-dimlists (list ascending-group))))
        (t (setf new-grouped-dimlists
                   (append new-grouped-dimlists (list descending-group)))))
       )
      (setf grouped-dimlists new-grouped-dimlists))

      grouped-dimlists  
    ;;end let, sort-dimlists-by-level
    ))|#


#| PRE 11-12
(defun make-1dimtree (dims target-dim-n dimspecs &key (group-by-subord-sym-p T)
                           (topdims '(I L F)))
  "Makes only dimlist trees--not symbols. RETURNS  (values all-newdims labeled-all-newdims). Labels are index syms in dims. eg (I ((I 2 3)...)).  NOTE that the bottom level I dimlists are grouped by their level L. If group-by-subord-sym-p, groups dims in labeled-all-newdims by subord level sym"

  (let*
      ((target-dim-nth (- target-dim-n 1))
       (index (nth target-dim-nth dims))
       (dimspec (nth target-dim-nth dimspecs))
       (n-indexs (first dimspec))
       (begin-index (second dimspec))
       (incr (third dimspec))
       (new-index begin-index)
       (newdims)
       ;; (newdims1)
       (all-newdims (list (list dims)))
       (all-newdims1)
       (sub-newdims)
       (labeled-all-newdims (list (list dims)))
       (indexs)
       (label)
       ) 
    
    ;;on top-dim, list dims
  ;; (when (= (list-length dims) target-dim-n)
      (setf all-newdims (list (list dims)))

    (cond
     ((not (integerp index))

      ;;want to list by subord index or index?
      (cond
       (group-by-subord-sym-p
        (setf label (find-sub-superord-index  index)))
       (t (setf label index)))

      (loop
       For n from 1 to n-indexs 
       do
       (setf newdims (replace-nth dims target-dim-nth  new-index)
            sub-newdims (append sub-newdims (list newdims)))            

       (when (= n n-indexs)
         (setf all-newdims (append all-newdims (list sub-newdims))
               labeled-all-newdims (append labeled-all-newdims (list (list label sub-newdims)))))
       ;;(afout 'out1 (format nil "AT 1: newdims= ~A target-dim-nth= ~A ~%all-newdims= ~A~%all-newdims1= ~A~%" newdims target-dim-nth  all-newdims all-newdims1))
       ;;after, incr index
       (setf new-index (+ new-index incr))

       ;;RECURSE ON EACH NEW newdims, going to next less dim
       (when (> target-dim-nth 0)
         (multiple-value-setq (all-newdims1 labeled-all-newdims1)
             (make-1dimtree  newdims  (- target-dim-n 1)  dimspecs)) 
                            ;;:all-newdims all-newdims))
         (setf all-newdims (append all-newdims all-newdims1)
               labeled-all-newdims (append labeled-all-newdims labeled-all-newdims1))
         ;;(afout 'out1 (format nil "AT 2:  ~A target-dim-nth= ~A ~%all-newdims= ~A~%all-newdims1= ~A~%" newdims target-dim-nth  all-newdims all-newdims1))
         ;;end when
         )
       ;;end loop
       )
      ;;end not integerp
      )
     ((< target-dim-nth 0)
      NIL)
     )

    (values all-newdims labeled-all-newdims)
    ;;end let, make-1dimtree
    ))|#

#|(defun make-1dimtree (dims target-dim-n dimspecs) 
  "Makes only dimlist trees--not symbols. RETURNS  (values  labeled-all-newdims-by-level all-newdims). Labels are index syms in dims. eg (I ((I 2 3)...)).  NOTE that the bottom level I dimlists are grouped by their level L. If group-by-subord-sym-p, groups dims in labeled-all-newdims by subord level sym"

  (let*
      ((target-dim-nth (- target-dim-n 1))
       (index (nth target-dim-nth dims))
       (dimspec (nth target-dim-nth dimspecs))
       (n-indexs (first dimspec))
       (begin-index (second dimspec))
       (incr (third dimspec))
       (new-index begin-index)
       (newdims)
       ;; (newdims1)
       (all-newdims)  ;;below (list (list dims)))
       (all-newdims1)
       (sub-newdims)
       (labeled-all-newdims) ;;below (list (list 'TOP  dims)))
       (indexs)
       (sup-label (find-super-subord-index index))
       (label)
       (labeled-all-newdims-by-level)
       ) 

    (cond
     ((not (integerp index))

      ;;FIRST CONCENTRATE ON THE SUPERORDINATE LETTER INDEX

      ;;add these dims to overall lists
      (setf  all-newdims  (list (list dims)) ;;WAS (list
             labeled-all-newdims (list (list sup-label (list  dims))))
       ;;(setf  all-newdims (append all-newdims (list (list dims))) ;;WAS (list
            ;; labeled-all-newdims (append labeled-all-newdims   (list sup-label (list  dims)))) ;was list
            ;;(afout 'out1 (format nil "AT INTERGERP, before LOOP~%    all-newdims= ~A~% labeled-all-newdims= ~A~%" all-newdims labeled-all-newdims))
      ;;LOOP THRU EACH SUBORDINATE INTEGER INDEX
        (setf  label index)

      (loop
       For n from 1 to n-indexs 
       do
       (setf newdims (replace-nth dims target-dim-nth  new-index)
            sub-newdims (append sub-newdims (list newdims)))            

          ;;(afout 'out1 (format nil "AT LOOP BEGIN,~%  newdims= ~A~% sub-newdims= ~A~%" newdims sub-newdims))

       (when (= n n-indexs)
         (setf all-newdims (append all-newdims (list sub-newdims))
          labeled-all-newdims (append labeled-all-newdims (list (list label sub-newdims))))
         ;;dont use     (setf labeled-all-newdims-by-level (append labeled-all-newdims-by-level (list (list label sub-newdims))) here because can't just add lists to sorted list.
         ;; MUST USE ANOTHER FUNCTION TO PLACE IT CORRECTLY
         ;; MAKE A SET-BEGIN-LIST FUNCTION 
         ;; SS RIGHT ARGS???
         ;;sort at end instead???
         (setf labeled-all-newdims-by-level 
               (append-groups-with-same-begin (list label) labeled-all-newdims
                                              labeled-all-newdims-by-level))

;;         (afout 'out1 (format nil "IN LOOP, WHEN N= N-NINDEXES:~% label= ~A newdims= ~A target-dim-nth= ~A ~%labeled-all-newdims-by-level= ~A~%labeled-all-newdims= ~A~%all-newdims1= ~A~%" label newdims target-dim-nth labeled-all-newdims-by-level labeled-all-newdims all-newdims1))
         ;;after, incr index
         

         ;;RECURSE ON EACH NEW newdims, going to next less dim
         (when (> target-dim-nth 0)
         ;;(afout 'out1 (format nil "IN LOOP, WHEN target-dim-nth > 0; RECURSE~%on  newdims= ~A  (- target-dim-n=~A 1) " newdims target-dim-n))
           (multiple-value-setq (labeled-all-newdims-by-level1 labeled-all-newdims1 all-newdims1 )
               (make-1dimtree  newdims  (- target-dim-n 1)  dimspecs)) 
           ;;Gets rid of  top-level list that is redundant below??
           (setf labeled-all-newdims1 (second labeled-all-newdims1)
                 labeled-all-newdims-by-level1 (second (car labeled-all-newdims-by-level1)))
           ;;:all-newdims all-newdims))
           (setf all-newdims (append all-newdims all-newdims1)
                 labeled-all-newdims (append labeled-all-newdims  labeled-all-newdims1))
               ;;no??  labeled-all-newdims-by-level  (append-groups-with-same-begin label 
                ;;was labeled-all-newdims-by-level (append labeled-all-newdims-by-level labeled-all-newdims-by-level1))
           ;;(afout 'out1 (format nil "AT 2: newdims= ~A target-dim-nth= ~A ~%labeled-all-newdims-by-level= ~A labeled-all-newdims-by-level1= ~a~%all-newdims= ~A~%all-newdims1= ~A~%labeled-all-newdims= ~a~%" newdims target-dim-nth labeled-all-newdims-by-level labeled-all-newdims-by-level1 labeled-all-newdims all-newdims all-newdims1))
           ;;end when, when
           ))
       ;;RE-INITIATE
       (setf new-index (+ new-index incr))

       ;;end loop n-indexs
       )
      ;;end not integerp
      )
     ((< target-dim-nth 0)
      NIL)
     (t nil ))
   ;;(afout 'out1 (format nil "AT 3:labeled-all-newdims-by-level= ~A~%labeled-all-newdims= ~a~%"labeled-all-newdims-by-level labeled-all-newdims))

    ;;GROUP ALL NEWDIMS BY LEVELS
    (setf labeled-all-newdims-by-level (sort-keylists-by-begin labeled-all-newdims-by-level :sort-within-groups 'descending))
;; eg  (((M ((I L F))) (F ((I L 2)) ((I L 3))) (L ((I 3 2)) ((I 4 2)) ((I 5 2)) ((I 3 3)) ((I 4 3)) ((I 5 3))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2)) ((1 4 2) (2 4 2) (3 4 2) (4 4 2)) ((1 5 2) (2 5 2) (3 5 2) (4 5 2)) ((1 3 3) (2 3 3) (3 3 3) (4 3 3)) ((1 4 3) (2 4 3) (3 4 3) (4 4 3)) ((1 5 3) (2 5 3) (3 5 3) (4 5 3)))))

    (values labeled-all-newdims-by-level  labeled-all-newdims all-newdims)
    ;;end let, make-1dimtree
    ))|#








;;OLD DELETE?
#|(defun make-node-dimsym-tree (symlist sym-spec-list  
                                        &key all-syms-list
                                        (node-separator  *art-node-separator)
                                        (separator-str *art-index-separator)
                                         (index-syms *art-index-syms)
                                         parse-dimlist1-p (parse-dimlist2-p T)
                                         subdims1-done-p subdims2-done-p
                                         all-node-tree-syms)
       "U-ART. Makes one symbol tree from one sym-spec-list. Sets each symbol to a list (root dimlist nil subsym-list).  RETURNS  (values all-node-tree-syms path-syms-by-levels new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p ). Either makes node symbols eg XI-L-F, X9-2-3 OR path symbols eg. WUPI-L-FTOI-L-F  WUP7-3-2TO4-1-3. Sets  higher level symbols' value to subsyms. Works well on both node & path trees (uses make-path-dimsym-tree). NO LONGER USED FOR PATHS."
   (let*
       ((orig-root (car sym-spec-list))
         (dim-spec-lists (second sym-spec-list))
         (n-syms (length symlist))
         (new-subsyms)
         (new-subsym-strs)
         ;;added
         (path-sym-p)
         (symvals)
         (symdims)
         (new-spec-sublists)
         (subdimspecs1) 
         (subdimspecs2)
         (curdims1)
         (curdims2)
         (top-dimlist)
         (toppathsym)
         (new-n-dims)
         (new-n-items)
         (sublist-ns)
         (new-sublist-Ns)
         (path-syms-by-levels)
        )

     (loop
      for sym in symlist
      for n from 1 to n-syms
      do

      ;;NODE OR PATH SYM
      (setf symvals (eval sym)
            symdims (second symvals))
      (multiple-value-setq (new-n-dims new-n-items new-sublist-Ns new-spec-sublists)
          (find-dim-spec-info  symdims))
        (when (> (list-length new-spec-sublists) 1)
          (setf path-sym-p T
                subdimspecs1 (car new-spec-sublists)
                subdimsspecs2 (second new-spec-sublists))
          ;;WRONG PLACE?, last not done yet: check to see if subdims is all integers (meaning it's done)
         ;; (setf subdims1-done-p (dimlist-is-ints-p subdims1) 
          ;;      subdims2-done-p (dimlist-is-ints-p  subdims2)) 

          ;;(setf *out1 (append *out1 (list (format nil "~%~%PATH CHECK new-spec-sublists= ~A subdims1-done-p= ~A subdims2-done-p= ~A  subdimspecs1= ~A subdimspecs2= ~A"new-spec-sublists subdims1-done-p subdims2-done-p   subdimspecs1 subdimspecs2))))
          ;;end when
          )

      (cond
       ;;NODE SYM -- just do subsym, no subdims         ;;zzzz
       ((null path-sym-p)

        (multiple-value-setq (new-subsyms new-subsym-strs) ;;no subdims
            (make-node-dimsym-subtree  sym  sym-spec-list 
                                  :separator-str separator-str :node-separator node-separator
                                  :index-syms index-syms
                                  :new-subsyms new-subsyms :new-subsym-strs new-subsym-strs
                                  ))
        ;;(BREAK)
        (when new-subsyms
          (setf all-node-tree-syms (append all-node-tree-syms (list new-subsyms))))
        )
       ;;PATH SYM -- must find both sublist syms
       ;;eg INPUT eg. (WUPI-L-FTOI-L-F  (\"Wup\" ((,*n-inputs  1 1)(1 3 1)(1 2 1   ) TO (,*n-outputs 1 1 (1 1 1   )(1 3 1 )))))
       (t     
        (setf target-dim-n1 (list-length subdimspecs1)
              target-dim-n2 (list-length subdimspecs2) 
              curdims *art-index-syms ;;(I L F M)
              n-artsyms (list-length *art-index-syms)
              n-dif (- n-artsyms target-dim-n1))                                 
               
        (when (> n-dif  0)
          (setf  curdims (butlast curdims n-dif))) 

        (unless (and (symbolp orig-root) (boundp orig-root))
          (setf top-dimlist (append  curdims  (list 'TO) curdims)
                toppathsym (make-dim-symbol orig-root top-dimlist)))

        (multiple-value-setq (M-syms L-syms F-syms I-syms  path-syms-by-level)
            (make-path-dimsym-tree toppathsym curdims target-dim-n1 subdimspecs1 
                                   curdims target-dim-n2 subdimspecs2))

        ;;end t,cond
        ))

      ;;TRY TEST FOR SUBDIMS DONE HERE -- DONE IN SUBTREE?
      ;;check to see if subdims is all integers (meaning it's done)
       ;;  (setf subdims1-done-p (dimlist-is-ints-p subdims1) 
        ;;        subdims2-done-p (dimlist-is-ints-p  subdims2))     |   

      ;;WHAT NEXT--RECURSE?
      (cond
       ;;last item at last level with all dims being integers, therefore returning nil?
       ((null new-subsyms)  NIL)
       (t
         (multiple-value-setq (all-node-tree-syms path-syms-by-levels new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p)
             (make-node-dimsym-tree  new-subsyms sym-spec-list 
                                :all-node-tree-syms all-node-tree-syms
                                :separator-str  separator-str
                                   :node-separator  node-separator    :index-syms index-syms
                                   :subdims1-done-p subdims1-done-p
                                   :subdims2-done-p subdims2-done-p
                                   :parse-dimlist1-p parse-dimlist1-p 
                                   :parse-dimlist2-p parse-dimlist2-p))

         (when new-subsyms
           (setf all-node-tree-syms (append all-node-tree-syms new-subsyms))) ;;was list new-subsyms
         ;;end t,cond
         ))
         ;;end loop
         )
       
    ;; (setf *out2 (append (list *out2) (list (format nil "AFTER TREE RECURSE: new-subsyms=~A~%all-node-tree-syms= ~A~%path-syms-by-levels= ~A~%  subdims1-done-p= ~A subdims2-done-p= ~A~%"   new-subsyms all-node-tree-syms path-syms-by-levels subdims1-done-p subdims2-done-p))))

     (values all-node-tree-syms path-syms-by-levels new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p )
        ;;end let, make-node-dimsym-tree
        ))

(defun make-node-dimsym-subtree (sym  sym-spec-list 
                                    &key parse-dimlist1-p (parse-dimlist2-p T) 
                                    (separator-str "-")(node-separator "TO") 
                                    (index-syms *art-index-syms)
                                    subdims1-done-p  subdims2-done-p
                                    new-subsyms new-subsym-strs)
  "In U-ART.  Sets new syms= symval lists (root dims nil subsyms) RETURNS (values new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p. Note: sym can be string or symbol. This is the workhorse of the new sym tree system."
  (let*
      ((sym (my-make-symbol sym))
       (symvals  (eval sym)) ;;eg (root dimlist place-for-subsyms)  ("X" (i l f))
       (topdims (second symvals))
       (target-dims)
       (rev-target-dims)
       (n-target-dims)
       (target-dimspecs)
       (orig-root) 
       (dim-spec-lists (second sym-spec-list))
       (dimspec-sublist1)
       (dimspec-sublist2)
       (other-dimspecs)
       (subsym)
       (subsymval)
       (subsymvals)
       (subsymdims)
       (begin-index) 
       (incr-index) 
       (begin-index-str "") 
       (end-index-str "")
       (begin-indecies)
       (begin-indecies-str "")
       (end-indecies)
       (end-indecies-str "")
       (sublist1-indecies-str)
       (sublist2-indecies-str)
       (n-dims)(n-items)(sublists)
       (found-index-sym-p)
       (node-separator-n 0)
       (parse-sublist)
       (node-sym-spec-list)
       (subdims1)
       (subdims2)
       (n-tdims)(n-sub1-dims)(n-sub2-dims)
       (dimspec-sublists)
       (sublist1-n)
       (node-sep)
       (sublist2-n)
       (target-index-n)
       (new-indecies)
       (subsym-str)
       (preset-vals)
       )

    ;;find info for artsym topdims dim-lists
    (multiple-value-setq (dimslist subdims1 subdims2 n-tdims n-sub1-dims n-sub2-dims)
        (find-symdim-info sym))
         ;; (setf *out1 (append *out1 (list (format nil "DIMS INFO  topdims=~A~% subdims1= ~A subdims2= ~A n-tdims= ~A  n-sub1-dims= ~A  n-sub2-dims= ~A~%"  topdims subdims1 subdims2 n-tdims n-sub1-dims n-sub2-dims))))

     ;;find overall info in dim-spec-lists
    (multiple-value-setq (n-dims n-items sublist-Ns dimspec-sublists) ;;eg 3  3  (3)  sublists
         (find-dim-spec-info dim-spec-lists))
     ;;find the (3 to 3) sublist
     (multiple-value-setq (sublist1-n node-sep sublist2-n)  ;;eg 3 0 0
         (values-list sublist-Ns))
     ;;find the two dimspec-sublists
     (setf dimspec-sublist1 (first dimspec-sublists)
           dimspec-sublist2 (second dimspec-sublists)
           ;;test to see if subdims are all integers (ie done)
           subdims1-done-p (integers-list-p subdims1)
           subdims2-done-p (integers-list-p subdims2))

     ;;SET WHICH SUBDIMS (IF ANY) TO PARSE NEXT
     (cond
      ;;1 done, 2 nil; parse 2
      ((and subdims1-done-p (null subdims2-done-p))
       (setf parse-dimlist2-p T
             parse-dimlist1-p NIL))
       ;;2 done, 1 nil; parse 1
      ((and subdims2-done-p (null subdims1-done-p))
       (setf parse-dimlist1-p T
             parse-dimlist2-p nil))
      ;;both done; parse none
      ((and subdims1-done-p subdims2-done-p)
       (setf parse-dimlist1-p nil
             parse-dimlist2-p nil))
      ;;neither done, go by arg key var settings; Default is parse-dimlist2-p = T
      (t  nil))

          ;;(setf *out1 (append *out1 (list (format nil "DONE-P & PARSE-P SET: dimspec-sublists= ~A;INFO: sublist1-n= ~A node-sep= ~A sublist2-n= ~A~%dimspec-sublist1= ~A dimspec-sublist2= ~A~%subdims1-done-p= ~A subdims2-done-p= ~A~%parse-dimlist1-p= ~A parse-dimlist2-p= ~A~%" dimspec-sublists sublist1-n node-sep sublist2-n dimspec-sublist1 dimspec-sublist2 subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p ))))

     ;;If sym is a path, choose which sublist to parse (before TO or after TO)
     ;;If a node-separator, find the sym-spec-list sublist to process

     ;;Find target-dims & target-dimspecs ;;eg (i l 2) & ((5 1 1)(1 2 1)(1 2 1))  
     (cond
      ;;if path symbol
      (node-sep
       (cond
        (parse-dimlist2-p
        ;; (setf subdims2-done-p T)
         (setf target-dimspecs dimspec-sublist2
               other-dimspecs dimspec-sublist1
               target-dims subdims2
               sublist1-indecies-str (make-dims-string subdims1)))
        (parse-dimlist1-p
         ;;(setf subdims1-done-p T)
         (setf target-dimspecs dimspec-sublist1
                 other-dimspecs dimspec-sublist2
                  target-dims subdims1
                  sublist2-indecies-str (make-dims-string subdims2))
                  )
        (t NIL))
       ;;end if path (node-sep)
        )
      ;;if node symbol
      (t  (setf target-dims topdims
                target-dimspecs dim-spec-lists)))

     ;;then set target-dims info
     (setf rev-target-dims (reverse target-dims)
           n-target-dims (list-length target-dims)
           orig-root (car sym-spec-list))
     ;;(setf *out1 (append *out1 (list (format nil "AT 1 target-dimspecs= ~A target-dims= ~A  rev-target-dims= ~A" target-dimspecs target-dims rev-target-dims))))

     ;;PROCESS EACH INDEX IN TARGET-DIMS
    (loop
     ;;for index in target-dims
     for index-n from 1 to n-target-dims
     for target-index in (reverse target-dims)  ;;eg f
     ;;find dimspec of LAST dimspec list first
     for target-dimspec in (reverse target-dimspecs)  ;;eg (3 1 1 -)
     do
     (setf target-index-n (- n-target-dims (- index-n 1))) ;; eg 3
     
     ;;(setf *out1 (append *out1 (list (format nil "IN OUTER LOOP 1, target-index= ~A target-dimspec= ~A target-index-n(from target-dims)= ~A ~%" target-index target-dimspec target-index-n ))))

     (cond
      ;;work on first reverse index that is a symbol (not number)
      ((member target-index index-syms :test 'my-equal)  ;;eg F is an ART index sym
       ;; (make-one-dim-indecies '(3 1 1 -))
       (setf new-indecies (make-one-dim-indecies target-dimspec) ;; eg (1 2 3)
             begin-indecies (butlast target-dims  index-n)
             begin-indecies-str 
             (print-list begin-indecies :no-newline-p t :separator-str separator-str)
             end-indecies (nthcdr target-index-n target-dims)
             end-indecies-str
             (print-list end-indecies :no-newline-p t  :separator-str separator-str)
             found-index-sym-p T)

      ;;(setf *out1 (append *out1 (list (format nil "~%~%IN OUTER LOOP 1B new-indecies= ~A begin-indecies= ~A end-indecies= ~A"     new-indecies   begin-indecies  end-indecies ))))
       
       ;;FOR EACH NEW-INDEX, MAKE A NEW SYBSYM
       (loop
        for new-index in new-indecies
        do
        ;;make the subsymdims and subsymdims-str for current new subsym
        (cond
         (end-indecies
          (setf subsymdims (append begin-indecies (list new-index) end-indecies)))
         (t (setf subsymdims (append begin-indecies (list new-index)))))
        
        ;;so cond below works right

        ;;MAKE THE NEW SUBSYM-STR AND SUBSYM
        (cond
         ;;If artsym is a NODE
         ((null node-sep)
          (cond
           ((and end-indecies begin-indecies)
            (setf subsym-str (format nil "~A~A~A~A~A~A" orig-root begin-indecies-str 
                                     separator-str  new-index separator-str end-indecies-str)))
           (begin-indecies
            (setf subsym-str (format nil "~A~A~A~A" orig-root begin-indecies-str 
                                     separator-str  new-index)))
           (end-indecies
            (setf subsym-str (format nil "~A~A~A~A" orig-root 
                                     new-index separator-str end-indecies-str))))
          ;;(BREAK)
          ;;end node-sep clause
          )
         ;;If artsym is a PATH
         (t
         #|          (cond
           (parse-dimlist2-p
            (cond                 
             ((and end-indecies begin-indecies)
              (setf subsym-str (format nil "~A~ATO~A~A~A~A~A" orig-root
                                       sublist1-indecies-str begin-indecies-str 
                                       separator-str  new-index separator-str end-indecies-str)))
             (begin-indecies
              (setf subsym-str (format nil "~A~ATO~A~A~A" orig-root 
                                       sublist1-indecies-str  begin-indecies-str 
                                       separator-str  new-index)))
             (end-indecies
              (setf subsym-str (format nil "~A~ATO~A~A~A" orig-root 
                                       sublist1-indecies-str  
                                       new-index separator-str end-indecies-str))))
           ;;FINISH?? PROCESS OTHER SUBLIST HERE??

           ;;end parse-dimlist2-p
            )
           ;;parse first node (before TO)
           (parse-dimlist1-p
            (cond
             ((and end-indecies begin-indecies)
              (setf subsym-str (format nil "~A~A~A~A~A~A~ATO~A" orig-root
                                       begin-indecies-str separator-str  new-index 
                                       separator-str end-indecies-str
                                       separator-str sublist2-indecies-str)))
             (begin-indecies
              (setf subsym-str (format nil "~A~A~A~ATO~A" orig-root 
                                       begin-indecies-str separator-str  new-index
                                       sublist2-indecies-str)))
             (end-indecies
              (setf subsym-str (format nil "~A~A~A~ATO~A" orig-root 
                                       new-index separator-str 
                                       end-indecies-str sublist2-indecies-str  ))))
            )
          ;;end both t, conds for MAKE THE NEW SUBSYM-STR AND SUBSYM
           (t nil)))|#
          ;;end t, cond
          ))
     
        ;;If a PATH, must put the new subdims with the orig subdims 
#|        (when node-sep
          (cond
           (parse-dimlist1-p 
            (setf subsymdims (append subsymdims (list 'TO) subdims2)))
           (parse-dimlist2-p 
                      (setf subsymdims (append  subdims1 (list 'TO) subsymdims )))
           (t nil))
          ;;end when
          )|#

        (when subsym-str  ;;was subsym
          (setf subsym (my-make-symbol subsym-str)
                subsymvals (list orig-root  subsymdims))

          (setf *out1 (append *out1 (list (format nil "IN INNER LOOP, subsymdims= ~A~%subsym= ~A, subsymvals= ~A~%" subsymdims     subsym   subsymvals))))
          (setf *out1 (append *out1 (list (format nil "ALSO: begin-indecies-str= ~A  end-indecies-str= ~A"  begin-indecies-str  end-indecies-str))))

          (set subsym subsymvals)

          (setf new-subsym-strs (append new-subsym-strs (list subsym-str))
                new-subsyms (append new-subsyms (list subsym)))             
          ;;end when
          )

        ;;end inner loop
        )
       ;;AFTER MAKING NEW SUBSYMS, RETURN
       (return)
       ;;end member
       )
      ((integerp target-index)
       ;;DO NOTHING??
        )
        ;;not needed?
        (t 
         ;;DO NOTHING
         ))

     ;;end outer loop
     )
    ;;ADD NEW SUBSYMS TO ORIGINAL SYM SYMVALS LIST
    ;;First see if subsym previously put in list of parent subsyms, don't add again!
    (setf preset-vals (getsymval sym))
    (cond
     ((member subsym preset-vals)
      NIL)
     (t
      (cond
       ;;for paths, must append the value?
       (node-sep
        (setsymval sym nil new-subsyms :append-value-p T)
        ;;end paths
        )
       ;;for nodes, just set the value
       (t 
        (setsymval sym nil new-subsyms)))
      ;;end subsym not in values, outer cond
      ))

    (values new-subsyms new-subsym-strs) 
     ;;end let, make-node-dimsym-subtree
     ))


;;NEW-NOT WORK-DELETE??
(defun make-node-dimsym-tree (symlist sym-spec-list  
                                        &key all-syms-list
                                        (node-separator  *art-node-separator)
                                        (separator-str *art-index-separator)
                                         (index-syms *art-index-syms)
                                         all-node-tree-syms)
       "U-ART. Makes one symbol tree from one sym-spec-list. Sets each symbol to a list (root dimlist nil subsym-list).  RETURNS  (values all-node-tree-syms  new-subsyms new-subsym-strs). MAKES ONLY node symbols eg XI-L-F, X9-2-3  Sets  higher level symbols' subsyms value to subsyms."
   (let*
       ((orig-root (car sym-spec-list))
         (dim-spec-lists (second sym-spec-list))
         (n-syms (length symlist))
         (new-subsyms)
         (new-subsym-strs)
         ;;added         
         (symvals)
         (symdims)
         (new-spec-sublists)
         ;;(subdimspecs1) 
         ;;(subdimspecs2)
         ;;(curdims1)
         ;;(curdims2)
         (top-dimlist)
         (toppathsym)
         (new-n-dims)
         (new-n-items)
         (sublist-ns)
         (new-sublist-Ns)
         )

     (loop
      for sym in symlist
      for n from 1 to n-syms
      do

      ;;NODE OR PATH SYM
      (setf symvals (eval sym)
            symdims (second symvals))
      (multiple-value-setq (new-n-dims new-n-items new-sublist-Ns new-spec-sublists)
          (find-dim-spec-info  symdims))
      ;;IF NODE SYM
      (when (> (list-length new-spec-sublists) 1)

        ;;NODE SYM -- just do subsym, no subdims         ;;zzzz
        (multiple-value-setq (new-subsyms new-subsym-strs) ;;no subdims
            (make-node-dimsym-subtree  sym  sym-spec-list 
                                       :separator-str separator-str :node-separator node-separator
                                       :index-syms index-syms
                                       :new-subsyms new-subsyms
                                       :new-subsym-strs new-subsym-strs
                                       ))

        (when new-subsyms
          (setf all-node-tree-syms (append all-node-tree-syms (list new-subsyms))))
        )
      ;;TRY TEST FOR SUBDIMS DONE HERE -- DONE IN SUBTREE?
      ;;check to see if subdims is all integers (meaning it's done)
       ;;  (setf subdims1-done-p (dimlist-is-ints-p subdims1) 
        ;;        subdims2-done-p (dimlist-is-ints-p  subdims2))     |   

      ;;WHAT NEXT--RECURSE?
      (cond
       ;;last item at last level with all dims being integers, therefore returning nil?
       ((null new-subsyms)  NIL)
       (t
         (multiple-value-setq (new-subsyms   new-subsym-strs) 
             (make-node-dimsym-tree  new-subsyms sym-spec-list 
                                :all-node-tree-syms all-node-tree-syms
                                :separator-str  separator-str
                                   :node-separator  node-separator    :index-syms index-syms))
         (when new-subsyms
           (setf all-node-tree-syms (append all-node-tree-syms new-subsyms))) ;;was list new-subsyms
         ;;end t,cond
         ))
         ;;end loop
         )
       
     (setf *out2 (append (list *out2) (list (format nil "AFTER TREE RECURSE: new-subsyms=~A~%all-node-tree-syms= ~A~%  subdims1-done-p= ~A "   new-subsyms all-node-tree-syms  subdims1-done-p ))))

     (values all-node-tree-syms  new-subsym-strs)  
        ;;end let, make-node-dimsym-tree
        ))


;;NEW-NOT WORK-DELETE??
(defun make-node-dimsym-subtree (sym  sym-spec-list 
                                    &key parse-dimlist1-p (parse-dimlist2-p T) 
                                    (separator-str "-")(node-separator "TO") 
                                    (index-syms *art-index-syms)
                                    new-subsyms new-subsym-strs)
  "In U-ART.  Sets new syms= symval lists (root dims nil subsyms) RETURNS (values new-subsyms new-subsym-strs  subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p. Note: sym can be string or symbol. This is the workhorse of the new sym tree system."
  (let*
      ((sym (my-make-symbol sym))
       (symvals  (eval sym)) ;;eg (root dimlist place-for-subsyms)  ("X" (i l f))
       (topdims (second symvals))
       (target-dims)
       (rev-target-dims)
       (n-target-dims)
       (target-dimspecs)
       (orig-root) 
       (dim-spec-lists (second sym-spec-list))
       (dimspec-sublist1)
       (dimspec-sublist2)
       (other-dimspecs)
       (subsym)
       (subsymval)
       (subsymvals)
       (subsymdims)
       (begin-index) 
       (incr-index) 
       (begin-index-str "") 
       (end-index-str "")
       (begin-indecies)
       (begin-indecies-str "")
       (end-indecies)
       (end-indecies-str "")
       (sublist1-indecies-str)
       (sublist2-indecies-str)
       (n-dims)(n-items)(sublists)
       (found-index-sym-p)
       (node-separator-n 0)
       (parse-sublist)
       (node-sym-spec-list)
       (subdims1)
       (subdims2)
       (n-tdims)(n-sub1-dims)(n-sub2-dims)
       (dimspec-sublists)
       (sublist1-n)
       (node-sep)
       (sublist2-n)
       (target-index-n)
       (new-indecies)
       (subsym-str)
       (preset-vals)
       )

    ;;find info for artsym topdims dim-lists
    (multiple-value-setq (dimslist subdims1 subdims2 n-tdims n-sub1-dims n-sub2-dims)
        (find-symdim-info sym))
         ;; (setf *out1 (append *out1 (list (format nil "DIMS INFO  topdims=~A~% subdims1= ~A subdims2= ~A n-tdims= ~A  n-sub1-dims= ~A  n-sub2-dims= ~A~%"  topdims subdims1 subdims2 n-tdims n-sub1-dims n-sub2-dims))))

     ;;find overall info in dim-spec-lists
    (multiple-value-setq (n-dims n-items sublist-Ns dimspec-sublists) ;;eg 3  3  (3)  sublists
         (find-dim-spec-info dim-spec-lists))
     ;;find the (3 to 3) sublist
     (multiple-value-setq (sublist1-n node-sep sublist2-n)  ;;eg 3 0 0
         (values-list sublist-Ns))
     ;;find the two dimspec-sublists
     (setf dimspec-sublist1 (first dimspec-sublists))
           ;;dimspec-sublist2 (second dimspec-sublists)

     ;;ARE SUBDIMS ALL PARSED?
     ;;test to see if subdims are all integers (ie done)
     (setf  subdims1-done-p (integers-list-p subdims1))
           ;;subdims2-done-p (integers-list-p subdims2))
     (when subdims1-done-p 
       (setf  parse-dimlist1-p NIL))
          ;;(setf *out1 (append *out1 (list (format nil "DONE-P & PARSE-P SET: dimspec-sublists= ~A;INFO: sublist1-n= ~A node-sep= ~A sublist2-n= ~A~%dimspec-sublist1= ~A dimspec-sublist2= ~A~%subdims1-done-p= ~A subdims2-done-p= ~A~%parse-dimlist1-p= ~A parse-dimlist2-p= ~A~%" dimspec-sublists sublist1-n node-sep sublist2-n dimspec-sublist1 dimspec-sublist2 subdims1-done-p subdims2-done-p parse-dimlist1-p parse-dimlist2-p ))))

     ;;If sym is a path, choose which sublist to parse (before TO or after TO)
     ;;If a node-separator, find the sym-spec-list sublist to process

     ;;Find target-dims & target-dimspecs ;;eg (i l 2) & ((5 1 1)(1 2 1)(1 2 1))  
  
      ;;works ONLY for NODE syms
      (unless subdims1-done-p
        (setf target-dims topdims
              target-dimspecs dim-spec-lists)

        ;;then set target-dims info
        (setf rev-target-dims (reverse target-dims)
              n-target-dims (list-length target-dims)
              orig-root (car sym-spec-list))
        ;;(setf *out1 (append *out1 (list (format nil "AT 1 target-dimspecs= ~A target-dims= ~A  rev-target-dims= ~A" target-dimspecs target-dims rev-target-dims))))

        ;;PROCESS EACH INDEX IN TARGET-DIMS
        (loop
         ;;for index in target-dims
         for index-n from 1 to n-target-dims
         for target-index in (reverse target-dims)  ;;eg f
         ;;find dimspec of LAST dimspec list first
         for target-dimspec in (reverse target-dimspecs)  ;;eg (3 1 1 -)
         do
         (setf target-index-n (- n-target-dims (- index-n 1))) ;; eg 3
     
         ;;(setf *out1 (append *out1 (list (format nil "IN OUTER LOOP 1, target-index= ~A target-dimspec= ~A target-index-n(from target-dims)= ~A ~%" target-index target-dimspec target-index-n ))))

         (cond
          ;;work on first reverse index that is a symbol (not number)
          ((member target-index index-syms :test 'my-equal)  ;;eg F is an ART index sym
           ;; (make-one-dim-indecies '(3 1 1 -))
           (setf new-indecies (make-one-dim-indecies target-dimspec) ;; eg (1 2 3)
                 begin-indecies (butlast target-dims  index-n)
                 begin-indecies-str 
                 (print-list begin-indecies :no-newline-p t :separator-str separator-str)
                 end-indecies (nthcdr target-index-n target-dims)
                 end-indecies-str
                 (print-list end-indecies :no-newline-p t  :separator-str separator-str)
                 found-index-sym-p T)

           ;;(setf *out1 (append *out1 (list (format nil "~%~%IN OUTER LOOP 1B new-indecies= ~A begin-indecies= ~A end-indecies= ~A"     new-indecies   begin-indecies  end-indecies ))))
       
           ;;FOR EACH NEW-INDEX, MAKE A NEW SYBSYM
           (loop
            for new-index in new-indecies
            do
            ;;make the subsymdims and subsymdims-str for current new subsym
            (cond
             (end-indecies
              (setf subsymdims (append begin-indecies (list new-index) end-indecies)))
             (t (setf subsymdims (append begin-indecies (list new-index)))))
        
            ;;so cond below works right

            ;;MAKE THE NEW SUBSYM-STR AND SUBSYM
            (cond
             ((and end-indecies begin-indecies)
              (setf subsym-str (format nil "~A~A~A~A~A~A" orig-root begin-indecies-str 
                                       separator-str  new-index separator-str end-indecies-str)))
             (begin-indecies
              (setf subsym-str (format nil "~A~A~A~A" orig-root begin-indecies-str 
                                       separator-str  new-index)))
             (end-indecies
              (setf subsym-str (format nil "~A~A~A~A" orig-root 
                                       new-index separator-str end-indecies-str))))
          
            (when subsym-str  ;;was subsym
              (setf subsym (my-make-symbol subsym-str)
                    subsymvals (list orig-root  subsymdims))

              (setf *out1 (append *out1 (list (format nil "IN INNER LOOP, subsymdims= ~A~%subsym= ~A, subsymvals= ~A~%" subsymdims     subsym   subsymvals))))
              (setf *out1 (append *out1 (list (format nil "ALSO: begin-indecies-str= ~A  end-indecies-str= ~A"  begin-indecies-str  end-indecies-str))))

              (set subsym subsymvals)

              (setf new-subsym-strs (append new-subsym-strs (list subsym-str))
                    new-subsyms (append new-subsyms (list subsym)))             
              ;;end when subsym-str
              )

            ;;end inner loop new-indecies
            )
           ;;AFTER MAKING NEW SUBSYMS, RETURN
           (return)
           ;;end member
           )
          ((integerp target-index)
           ;;DO NOTHING??
           )
          ;;not needed?
          (t 
           ;;DO NOTHING
           ))

         ;;end outer loop  for  n-target-dims
         )

        ;;ADD NEW SUBSYMS TO ORIGINAL SYM SYMVALS LIST
        ;;First see if subsym previously put in list of parent subsyms, don't add again!
        (setf preset-vals (getsymval sym))
        (cond
         ((member subsym preset-vals)
          NIL)
         (t
          (cond
           ;;for paths, must append the value?
           (node-sep
            (setsymval sym nil new-subsyms :append-value-p T)
            ;;end paths
            )
           ;;for nodes, just set the value
           (t 
            (setsymval sym nil new-subsyms)))
          ;;end subsym not in values, outer cond
          ))

        ;;END UNLESS DONE
        )
     (values new-subsyms  new-subsym-strs) 
     ;;end let, make-node-dimsym-subtree
     ))
|#