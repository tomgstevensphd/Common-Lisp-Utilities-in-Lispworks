;;*************************** U-trees-flatlists.lisp *******************************
;; THESE NOT USED BUT WORK, and might be useful for sume purpose
;; Use all flat lists to represent trees--see below

;;============= METHOD USING FLAT LISTS TO REP TREES (depreciated, but fnnctions may be useful for other purposes?? ===============

;;MAKE-TREE-NUMBERS-LIST
;;
;;ddd
(defun make-tree-numbers-list (tree &key (n 0) tree-num-list ;;(rootmult 1)
                                   parent-rootn parent-rootnlist  (root-leveln 0) root-leveln-list)
  " Works, but depreciated, HOWEVER fnnctions may be useful for other purposes??"
  (let
      ((len-tree (list-length tree))
       (subtree-list)       
       (final-n 0)
       (rootn-list)
       (rootbase 0)
       (flat-rootn-list)
       (flat-rootnlist-list)
       (rootn 0)
       (rootnlist)
       )
    (incf root-leveln)
    (setf root-leveln-list (append root-leveln-list (list root-leveln)))
    (loop
     for item in tree
     do
     (incf n)
     (incf rootbase)
     (cond
      (parent-rootn
       (setf rootn (format nil "~A.~A" parent-rootn rootbase)
             rootnlist (append parent-rootnlist (list  rootbase))))
      (t (setf rootn rootbase
               rootnlist (list rootbase))))
     ;;was  (+ (* rootbase rootmult) parent-rootn)
     (setf rootn-list (append rootn-list (list rootn))
           flat-rootn-list (append flat-rootn-list (list rootn))
           flat-rootnlist-list (append flat-rootnlist-list (list rootnlist)))
     (cond
      ((listp item)
       (multiple-value-bind (subsubtree-list rootn-list1 flat-rootn-list1 
                                             flat-rootnlist-list1 root-leveln-list1
                                              return-n)
           (make-tree-numbers-list item  :n (- n 1) ;;:rootmult (* 0.1 rootmult) 
                                   :parent-rootn rootn :parent-rootnlist rootnlist                                    
                                   :root-leveln root-leveln
                                   :tree-num-list tree-num-list)
         (setf subtree-list (append subtree-list  subsubtree-list)
               rootn-list (append rootn-list (list rootn-list1))
               flat-rootn-list (append flat-rootn-list flat-rootn-list1)
               flat-rootnlist-list (append flat-rootnlist-list flat-rootnlist-list1)
               root-leveln-list (append root-leveln-list (list root-leveln-list1)))
         (setf n return-n)
         ;;end mvb, clause
         ))
      (t
       (setf subtree-list (append subtree-list (list n)))
       ))
     ;;end loop
     )
    (setf tree-num-list (append tree-num-list (list subtree-list)))
    (values tree-num-list rootn-list flat-rootn-list flat-rootnlist-list root-leveln-list n)
    ))
;;TEST
;; (make-tree-numbers-list '(a (b c (d e (f g (h i j)) (k l) m (n (o (p q (r (s)) t) u v) w) x y ) z) end))
;; works= ((1 (2 3 (4 5 (6 7 (8 9 10)) (11 12) 13 (14 (15 (16 17 (18 (19)) 20) 21 22) 23) 24 25) 26) 27))   27
;; (make-tree-numbers-list '(a (b c (d e (f g (h i j)) (k l) m (n (o (p q (r (s)) t) u v) w) x y ) z) end))
;;works=
;; tree-num-list= ((1 (2 3 (4 5 (6 7 (8 9 10)) (11 12) 13 (14 (15 (16 17 (18 (19)) 20) 21 22) 23) 24 25) 26) 27))
;; rootn-list= (1 2 ("2.1" "2.2" "2.3" ("2.3.1" "2.3.2" "2.3.3" ("2.3.3.1" "2.3.3.2" "2.3.3.3" ("2.3.3.3.1" "2.3.3.3.2" "2.3.3.3.3")) "2.3.4" ("2.3.4.1" "2.3.4.2") "2.3.5" "2.3.6" ("2.3.6.1" "2.3.6.2" ("2.3.6.2.1" "2.3.6.2.2" ("2.3.6.2.2.1" "2.3.6.2.2.2" "2.3.6.2.2.3" ("2.3.6.2.2.3.1" "2.3.6.2.2.3.2" ("2.3.6.2.2.3.2.1")) "2.3.6.2.2.4") "2.3.6.2.3" "2.3.6.2.4") "2.3.6.3") "2.3.7" "2.3.8") "2.4") 3)
;;flat-rootn-list  = (1 2 "2.1" "2.2" "2.3" "2.3.1" "2.3.2" "2.3.3" "2.3.3.1" "2.3.3.2" "2.3.3.3" "2.3.3.3.1" "2.3.3.3.2" "2.3.3.3.3" "2.3.4" "2.3.4.1" "2.3.4.2" "2.3.5" "2.3.6" "2.3.6.1" "2.3.6.2" "2.3.6.2.1" "2.3.6.2.2" "2.3.6.2.2.1" "2.3.6.2.2.2" "2.3.6.2.2.3" "2.3.6.2.2.3.1" "2.3.6.2.2.3.2" "2.3.6.2.2.3.2.1" "2.3.6.2.2.4" "2.3.6.2.3" "2.3.6.2.4" "2.3.6.3" "2.3.7" "2.3.8" "2.4" 3)
;;flat-rootnlist-list= ((1) (2) (2 1) (2 2) (2 3) (2 3 1) (2 3 2) (2 3 3) (2 3 3 1) (2 3 3 2) (2 3 3 3) (2 3 3 3 1) (2 3 3 3 2) (2 3 3 3 3) (2 3 4) (2 3 4 1) (2 3 4 2) (2 3 5) (2 3 6) (2 3 6 1) (2 3 6 2) (2 3 6 2 1) (2 3 6 2 2) (2 3 6 2 2 1) (2 3 6 2 2 2) (2 3 6 2 2 3) (2 3 6 2 2 3 1) (2 3 6 2 2 3 2) (2 3 6 2 2 3 2 1) (2 3 6 2 2 4) (2 3 6 2 3) (2 3 6 2 4) (2 3 6 3) (2 3 7) (2 3 8) (2 4) (3))
;;root-level-list = (1 (2 (3 (4 (5)) (4) (4 (5 (6 (7 (8))))))))  N = 27


;;MAKE-TREE-ROOTN-LIST-FROM-ROOTNLIST
;;
;;ddd
(defun make-tree-rootn-list-from-rootnlist (flat-rootnlist-list &key (max-n-levels 10)
                                                                flat-rootnstr-list )
" Works, but depreciated, HOWEVER fnnctions may be useful for other purposes??"
  (let*
      ((tree-level-list)
       (tree-level-str-list)
       (new-tree-level-lists)
       (new-tree-level-str-lists)
       (tree-level-lists (my-make-list max-n-levels :initial-element NIL))
       (tree-level-str-lists (my-make-list max-n-levels :initial-element NIL))
       (len-flat-rootnlist-list (list-length flat-rootnlist-list))
       (child-level-list)
       (treeview-child-level-lists)
       )
    (loop
     for level-list in tree-level-lists
     for level-str-list in tree-level-str-lists
     for leveln from 1 to max-n-levels
     do
     (loop
      for rootnlist in flat-rootnlist-list
      for rootnlist-n from 0 to len-flat-rootnlist-list
      for rootnstr in flat-rootnstr-list
      do
      (setf level-digit (nth (- leveln 1) rootnlist)
            len-rootnlist (list-length rootnlist))
      (when  (and level-digit 
                  (equal leveln len-rootnlist))
        (setf child-level-list (append child-level-list (last rootnlist)))
        (cond
         ((null level-list)
          (setf level-list (list rootnlist)
                level-str-list (list rootnstr))) 
         (t  (setf level-list (append level-list (list rootnlist))
                   level-str-list (append  level-str-list (list rootnstr)))))
        ;;end lwhen evel-digit
        )
      ;;end inner loop
      )
     (when level-list
       (setf new-tree-level-lists (append new-tree-level-lists (list level-list))))
     (when level-str-list
       (setf  new-tree-level-str-lists (append new-tree-level-str-lists  (list level-str-list))))
     (when child-level-list
       (setf treeview-child-level-lists (append treeview-child-level-lists (list child-level-list))
             child-level-list nil))
     ;;end outer loop
     )
    (setf max-tree-depthn (list-length new-tree-level-lists))
    (values new-tree-level-str-lists new-tree-level-lists  treeview-child-level-lists
            max-tree-depthn)
    ;;end let, make-tree-rootn-list-from-rootnlist
    ))
;;TEST
;; (make-tree-rootn-list-from-rootnlist '((1) (2) (2 1) (2 2) (2 3) (2 3 1) (2 3 2) (2 3 3) (2 3 3 1) (2 3 3 2) (2 3 3 3) (2 3 3 3 1) (2 3 3 3 2) (2 3 3 3 3) (2 3 4) (2 3 4 1) (2 3 4 2) (2 3 5) (2 3 6) (2 3 6 1) (2 3 6 2) (2 3 6 2 1) (2 3 6 2 2) (2 3 6 2 2 1) (2 3 6 2 2 2) (2 3 6 2 2 3) (2 3 6 2 2 3 1) (2 3 6 2 2 3 2) (2 3 6 2 2 3 2 1) (2 3 6 2 2 4) (2 3 6 2 3) (2 3 6 2 4) (2 3 6 3) (2 3 7) (2 3 8) (2 4) (3))  :flat-rootnstr-list   '(1 2 "2.1" "2.2" "2.3" "2.3.1" "2.3.2" "2.3.3" "2.3.3.1" "2.3.3.2" "2.3.3.3" "2.3.3.3.1" "2.3.3.3.2" "2.3.3.3.3" "2.3.4" "2.3.4.1" "2.3.4.2" "2.3.5" "2.3.6" "2.3.6.1" "2.3.6.2" "2.3.6.2.1" "2.3.6.2.2" "2.3.6.2.2.1" "2.3.6.2.2.2" "2.3.6.2.2.3" "2.3.6.2.2.3.1" "2.3.6.2.2.3.2" "2.3.6.2.2.3.2.1" "2.3.6.2.2.4" "2.3.6.2.3" "2.3.6.2.4" "2.3.6.3" "2.3.7" "2.3.8" "2.4" 3))
;; works= 
;;new-tree-level-str-lists= ((1 2 3) ("2.1" "2.2" "2.3" "2.4") ("2.3.1" "2.3.2" "2.3.3" "2.3.4" "2.3.5" "2.3.6" "2.3.7" "2.3.8") ("2.3.3.1" "2.3.3.2" "2.3.3.3" "2.3.4.1" "2.3.4.2" "2.3.6.1" "2.3.6.2" "2.3.6.3") ("2.3.3.3.1" "2.3.3.3.2" "2.3.3.3.3" "2.3.6.2.1" "2.3.6.2.2" "2.3.6.2.3" "2.3.6.2.4") ("2.3.6.2.2.1" "2.3.6.2.2.2" "2.3.6.2.2.3" "2.3.6.2.2.4") ("2.3.6.2.2.3.1" "2.3.6.2.2.3.2") ("2.3.6.2.2.3.2.1"))
;;new-tree-level-lists= (((1) (2) (3)) ((2 1) (2 2) (2 3) (2 4)) ((2 3 1) (2 3 2) (2 3 3) (2 3 4) (2 3 5) (2 3 6) (2 3 7) (2 3 8)) ((2 3 3 1) (2 3 3 2) (2 3 3 3) (2 3 4 1) (2 3 4 2) (2 3 6 1) (2 3 6 2) (2 3 6 3)) ((2 3 3 3 1) (2 3 3 3 2) (2 3 3 3 3) (2 3 6 2 1) (2 3 6 2 2) (2 3 6 2 3) (2 3 6 2 4)) ((2 3 6 2 2 1) (2 3 6 2 2 2) (2 3 6 2 2 3) (2 3 6 2 2 4)) ((2 3 6 2 2 3 1) (2 3 6 2 2 3 2)) ((2 3 6 2 2 3 2 1)))
;;DONT WORK? treeview-child-level-lists= ((1 2 3) (1 2 3 4) (1 2 3 4 5 6 7 8) (1 2 3 1 2 1 2 3) (1 2 3 1 2 3 4) (1 2 3 4) (1 2) (1))    max-tree-depthn= 8
;;; HEREX


;;CONVERT-TREE-TO-ROOTLISTS
;;
;;ddd
(defun CONVERT-NESTED-LIST-TO-ROOTLISTS (nested-list)
 
  ;;SS  NEVER FINISHED, BEC REPLACED FLAT APPROACH WITH NESTED LIST APPROACH TO NODES MAKING OVERALL FUNCTION TO CREATE THE TREE-VIEW AND ADD VALUES FROM ANY NESTED-LIST 
  (multiple-value-bind ( )
      (make-tree-numbers-list nested-lists)

    (multiple-value-bind ( )
     (make-tree-rootn-list-from-rootnlist  xxx)
  

    )))

;;PRINT-FUNCTION  FUNCTIONS --------------------------------------------
(defun my-print-function (root)
  "works"
  (format nil "~A: ~A" (make-root-string root) root)
          ;;was "Node: ~A: Nodelist: ~A" (make-root-string root) root)
  )

;;FIND-CHILD-ROOT-LISTS-FROM-FLAT-NODELISTS
;;
;;ddd
(defun find-child-root-lists-from-flat-nodelists (parent-rootlist tree-level-lists)
  "   RETURNS: List of rootlists.  Set parent-rootlist= 0 or NIL to find base roots.
   Works, but depreciated, HOWEVER fnnctions may be useful for other purposes??"
  (when (equal parent-rootlist 0)
    (setf parent-rootlist NIL))
  (let*
      ((parent-leveln-digit (car (last parent-rootlist)))
       (parent-leveln (list-length parent-rootlist))
       (return-rootlists)
       ;;eg (2 3  5), pld= 5, pln= 3
       )
    (when (null parent-rootlist)
      (setf parent-leveln 0))
    (loop
     for rootlist in tree-level-lists
     do
     (let
         ((len-rootlist (list-length rootlist))
          (leveln-digit)
          )
     (when (= len-rootlist (+ parent-leveln 1))     
       (cond
        ((null parent-rootlist)
         (setf return-rootlists (append return-rootlists (list rootlist))))
        (t 
         (setf leveln-digit (nth (- parent-leveln 1) rootlist))
         (when (= leveln-digit parent-leveln-digit)
           (setf return-rootlists (append return-rootlists  (list rootlist))))))
       ;;end outer when
       )
     ;;end let, loop
     ))
    return-rootlists
    ;;end let*, find-child-root-lists
    ))
 ;;TEST
;;  (setf *test-flat-tree-list '((1) (2) (2 1) (2 2) (2 3) (2 3 1) (2 3 2) (2 3 3) (2 3 3 1) (2 3 3 2) (2 3 3 3) (2 3 3 3 1) (2 3 3 3 2) (2 3 3 3 3) (2 3 4) (2 3 4 1) (2 3 4 2) (2 3 5) (2 3 6) (2 3 6 1) (2 3 6 2) (2 3 6 2 1) (2 3 6 2 2) (2 3 6 2 2 1) (2 3 6 2 2 2) (2 3 6 2 2 3) (2 3 6 2 2 3 1) (2 3 6 2 2 3 2) (2 3 6 2 2 3 2 1) (2 3 6 2 2 4) (2 3 6 2 3) (2 3 6 2 4) (2 3 6 3) (2 3 7) (2 3 8) (2 4) (3)))
;; BOTH OF FOLLOWING WORK
;;  (find-child-root-lists-from-flat-nodelists   '(2 3 2) *test-flat-tree-list)  = NIL ;;works, no children 
;;   (find-child-root-lists-from-flat-nodelists   '(2 3 3) *test-flat-tree-list)   = ((2 3 3 1) (2 3 3 2) (2 3 3 3))
;;  (find-child-root-lists-from-flat-nodelists   NIL *test-flat-tree-list)  = ((1) (2) (3))
;;  (find-child-root-lists-from-flat-nodelists   0 *test-flat-tree-list) = ((1) (2) (3))




;;MAKE-ROOT-STRING
;;
;;ddd
(defun make-root-string (rootlist)
  (let
      ((rootstr "")
       (len-rootlist (list-length rootlist))
       )
    (loop
     for root in rootlist
     for n from 0 to len-rootlist
     do
     (cond
      ((and rootlist (= n 0))
       (setf rootstr (format nil "~A" (car rootlist))))
      (rootlist
       (setf rootstr (format nil "~A.~A" rootstr root))
       )
      (t nil))
     ;;end loop
     )
    rootstr
    ;;end let, make-root-string
    ))
;;TEST
;; (make-root-string '(2)) = "2"
;; (make-root-string '(2  3 5 2 1)) = "2.3.5.2.1"



;;MAKE-TREE-VIEW-FOR-FLAT-LISTS
;;
;;ddd
(defun make-tree-view-for-flat-lists (treeview-flat-tree-list)
  (setf *treeview-flat-tree-list treeview-flat-tree-list
        *tree-roots-list (find-child-root-lists 0 treeview-flat-tree-list))
  (capi:display (make-instance 'my-tree-view-interface :title "My Test Tree view")))
;;TEST
;; (setf  *treeview-flat-test-list '((1) (2) (2 1) (2 2) (2 3) (2 3 1) (2 3 2) (2 3 3) (2 3 3 1) (2 3 3 2) (2 3 3 3) (2 3 3 3 1) (2 3 3 3 2) (2 3 3 3 3) (2 3 4) (2 3 4 1) (2 3 4 2) (2 3 5) (2 3 6) (2 3 6 1) (2 3 6 2) (2 3 6 2 1) (2 3 6 2 2) (2 3 6 2 2 1) (2 3 6 2 2 2) (2 3 6 2 2 3) (2 3 6 2 2 3 1) (2 3 6 2 2 3 2) (2 3 6 2 2 3 2 1) (2 3 6 2 2 4) (2 3 6 2 3) (2 3 6 2 4) (2 3 6 3) (2 3 7) (2 3 8) (2 4) (3))  ) 
;; (make-tree-view-for-flat-lists *treeview-flat-test-list)


(defun find-nodes-n-at-each-level (list  &key n-items-list)
  (let
      ((n-level-items)
       (len-list (list-length list))
       (nitems-list)
       )
    (loop
     for item in list
     for n from 1 to len-list
     do
     (cond
      ((listp item)
       (multiple-value-bind (sub-n-items-list sub-n-level-items)
           (find-nodes-n-at-each-level item :n-items-list n-items-list)
         (setf n-level-items (append n-level-items (list sub-n-items-list)))
       ))
      (t
       (setf nitems-list      xxx)

       ))
     (setf n-items-list (append n-items-list  (list n-level-items)))
     ;;end loop
     )
    (values n-items-list  n-level-items)
    ;;end let, find-nodes-n-at-each-level
    ))
;;TEST


(defun children-function-for-flat-nodelists (root)
  (when root  ;;was 100,  limits node number (larger N means more depth levels).
       (let*
           ((base (* root 10)) ;;next level node num begins at 1 + number
             ;; should be 1 + (length *tree-roots-list) 
             (rootlist (get-node-childlists root *tree-nodes-list))
             )
         rootlist
       ;;end let,when,chidren-function
       )))
;; (setf *treeview-flat-tree-list '((1) (2) (2 1) (2 2) (2 3) (2 3 1) (2 3 2) (2 3 3) (2 3 3 1) (2 3 3 2) (2 3 3 3) (2 3 3 3 1) (2 3 3 3 2) (2 3 3 3 3) (2 3 4) (2 3 4 1) (2 3 4 2) (2 3 5) (2 3 6) (2 3 6 1) (2 3 6 2) (2 3 6 2 1) (2 3 6 2 2) (2 3 6 2 2 1) (2 3 6 2 2 2) (2 3 6 2 2 3) (2 3 6 2 2 3 1) (2 3 6 2 2 3 2) (2 3 6 2 2 3 2 1) (2 3 6 2 2 4) (2 3 6 2 3) (2 3 6 2 4) (2 3 6 3) (2 3 7) (2 3 8) (2 4) (3)))

;; (setf  *treeview-levels-lists  '((1 2 3) (1 2 3 4) (1 2 3 4 5 6 7 8) (1 2 3 1 2 1 2 3) (1 2 3 1 2 3 4) (1 2 3 4) (1 2) (1)))
;; (setf  *treeview-levels-lists  '(((1)(2) (3)) (1 2 3 4) (1 2 3 4 5 6 7 8) (1 2 3 1 2 1 2 3) (1 2 3 1 2 3 4) (1 2 3 4) (1 2) (1)))
;; ;; (my-test-tree-view *treeview-levels-lists)
;;'(((2 1) (2 2) (2 3) (2 4)) ((2 3 1) (2 3 2) (2 3 3) (2 3 4) (2 3 5) (2 3 6) (2 3 7) (2 3 8)) ((2 3 3 1) (2 3 3 2) (2 3 3 3) (2 3 4 1) (2 3 4 2) (2 3 6 1) (2 3 6 2) (2 3 6 3)) ((2 3 3 3 1) (2 3 3 3 2) (2 3 3 3 3) (2 3 6 2 1) (2 3 6 2 2) (2 3 6 2 3) (2 3 6 2 4)) ((2 3 6 2 2 1) (2 3 6 2 2 2) (2 3 6 2 2 3) (2 3 6 2 2 4)) ((2 3 6 2 2 3 1) (2 3 6 2 2 3 2)) ((2 3 6 2 2 3 2 1))))
;; '( (1 2 :leaf 4 5) (11 22 33 44)(21 22 23 24)(31 32)(101 102 103 104)(201 202 203 204 205) (333 444 555 )(1111 2222 3333 4444 5555 6666)))

;;XXX========== END DEPRECIATED FLAT-LIST TREE APPROACH=====


