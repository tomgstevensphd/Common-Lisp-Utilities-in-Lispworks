;;********************** U-trees.lisp ********************************
;;2017-07

(defparameter *tree-roots-list  '(1 2 3)) ;;was '(1 2 3 4 5)) 
(defparameter *my-tree '(a (b c (d e (f g (h i j)) (k l) m (n (o (p q (r (s)) t) u v) w) x y ) z) end))
(defparameter *treeview-flat-tree-list NIL)
(defparameter *tree-nodes-list NIL)
(defparameter *child-root-value NIL)
(defparameter  *tree-root-childlists NIL)
(defparameter *child-rootnlists NIL)
(defparameter  *tree-root-sublists NIL )  
(defparameter *tree-rootn-list  NIL)
(defparameter *tree-flat-rootn-list NIL)
(defparameter *tree-root-leveln-list NIL)
(defparameter *totaln-nodes  NIL)
(defparameter *tree-roots-list NIL)
(defparameter *treeview-expand-level  99 "Default= 99, expand up to that level")



;;MY-TREE-VIEW-INTERFACE
;;
;;ddd
(capi:define-interface my-tree-view-interface ()
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
   (tree-rootn-list
    :initarg :tree-rootn-list
    :accessor tree-rootn-list
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
         :roots *tree-roots-list  ;;was '(1 2 3 4) 
         :leaf-node-p-function 'my-leaf-node-p-function
         :children-function 'treeview-children-function
         ;; :image-lists (list :normal *my-image-list*)
         ;; :image-function #'(lambda (x) (mod (1- x) 4))
         :checkbox-status 0  ;;or  2=checked initially; NIL= no checkbox
      ;;   :visible-min-width 500
       ;;  :visible-min-height 800
         :has-root-line T
         :retain-expanded-nodes T
         :expandp-function  *my-expand-function
         ;;set in the make-instance :expandp-function 'my-expandp-function
         ;; :expandp-function USE TO FIND NESTED-LIST ITEMS TO DISPLAY AT EACH SUB CHILD MODE?? (it is called each time expand a node)
         ;;OR try to design a function for :children-function that auto fills all in at the beginning instead of calling a function to search (possibly deep in a tree) at each node
         :print-function 'my-print-function  ;;was #'(lambda(x) (format nil "~d : ~r" x x ))
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
   :title "Tree View Test")
  ;;end interface
  )



;;CALLBACKS

;;ACTION-CALLBACKS ---------------------------------------------------
(defun action-callback (selected-item interface)
  (declare (ignore interface))
  (format t "~&Activate selected-item ~S~%" selected-item)
  )


;;NOTE:  The relationship between the root num and any node MUST BE A CONSISTENT MATH FUNCTION TO BE ABLE TO PUT RIGHT SUBLIST WITH THE PROPER NODE??
;;EG.  level-n = 1  11 101 1001




;;TREEVIEW-CHILDREN-FUNCTION
;;
;;ddd  
(defun treeview-children-function (root)
  (when root  ;;was 100,  limits node number (larger N means more depth levels).
    (unless (listp root)
      (setf root (list root)))
       (let*
           (;;(base (* root 10)) ;;next level node num begins at 1 + number
             ;; should be 1 + (length *tree-roots-list) 
             (root-childlists) 
             ;;couldn't find how to access interface-tried everything
              ;;(interface (capi::find-active-interface :this))
           ;;(capi:element-interface-for-callback :children-function))
              ;;(capi:top-level-interface :this))
             ;;(tree-nodes-list (slot-value interface 'tree-nodes-list))
             )
        (multiple-value-setq (*tree-root-childlists *child-rootnlists 
                                                    *child-root-value *tree-root-sublists )  
             (get-node-childlists root *tree-nodes-list))
        ;;(break "in callback *tree-root-sublists")
         *child-rootnlists
       ;;end let,when,chidren-function
       )))
;;TEST
;; (make-treeview-frame *testlist22)
#| older, works, using global vars
   (defun treeview-children-function (root)
  (when root  ;;was 100,  limits node number (larger N means more depth levels).
    (unless (listp root)
      (setf root (list root)))
       (let*
           (;;(base (* root 10)) ;;next level node num begins at 1 + number
             ;; should be 1 + (length *tree-roots-list) 
             (root-childlists) 
             )
         ;;moved to print-function
        (multiple-value-setq (*tree-root-childlists *child-rootnlists *child-root-value *tree-root-sublists )  
             (get-node-childlists root (slot-value *tree-nodes-list))
        ;;(break "in callback *tree-root-sublists")
         *child-rootnlists
       ;;end let,when,chidren-function
       ))))|#



;;LEAF-NODE-P-FUNCTION
;;
(defun my-leaf-node-p-function (root)
  (let ((result))
   (when (or (null root) (equal root :leaf))
     (setf result T)
     )))



;;EXPANDP-FUNCTION FUNCTIONS

#|expandp-function controls automatic expansion of NODES
(items) in the tree-view. By default, initially only the items
specified by the ROOTS argument are displayed. 
This initial display can be altered by supplying a function expandp-function
which allows further items to be displayed. 
If supplied,
expandp-function should be a function which is 
CALLED ON THE ROOTS AND IS CALLED RECURSIVELY ON THE CHILDREN IF IT RETURNS TRUE.
When the USER EXPANDS A NODE, 
expandp-function is CALLED ON EACH NEWLY CREATED CHILD NODE, which is expanded if this call returns true, and so on RECURSIVELY. 
The default value of expandp-function is NIL so that there is NO AUTOMATIC EXPANSION and only the root nodes are visible initially|#

;;MY-EXPANDP-FUNCTION
;;
;;ddd
(defun my-expandp-function (roots)
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


;;PRINT-FUNCTION  FUNCTIONS --------------------------------------------
(defun my-print-function (root)
  "works"
  ;;(break)
  (let
      ((rootnlist root)
       (root-list)
       )   
    (unless (listp rootnlist)
      (setf rootnlist (list rootnlist)))
    (multiple-value-setq (*tree-root-childlists *child-root-value *tree-root-sublists )  
             (get-node-childlists rootnlist *tree-nodes-list))
  ;; (get-node-childlists '(2) *tree-nodes-list)
    
    (setf root-list *tree-root-childlists)
    (print-node-info root-list)
    (format nil "~A: ~A" (make-root-string rootnlist) (print-node-info root-list))
    ;;was "Node: ~A: Nodelist: ~A" (make-root-string root) root)
    ))

  ;;was #'(lambda(x) (format nil "~d : ~r" x x ))



;;PRINT-NODE-INFO
;;
;;ddd
(defun print-node-info (tree-root-childlists &key (omit-root-info-p T) omit-rootnlist-p
                                             (valkey :VA) (sublistkey :SL))
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
;; (print-node-info '((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) :omit-rootnlist-p NIL)
;; works= ((2 3 4) "2.3.4" :SL " Select +")
;; (print-node-info '((2 3 4 1) "2.3.4.1" :VA K)  :omit-root-info-p NIL)
;; works= ((2 3 4 1) "2.3.4.1" :VA K)
;;  (print-node-info '((2 3 4 1) "2.3.4.1" :VA K)  :omit-rootnlist-p T)
;; works= ("2.3.4.1" :VA K)
;;  (print-node-info '((2 3 4 1) "2.3.4.1" :VA K) :omit-root-info-p T) 


;;MAKE-ROOT-STRING
;;
;;ddd
(defun make-root-string (rootnlist)
  (let
      ((rootstr "")
       (len-rootnlist) 
       )
    (cond
     ((listp rootnlist)
      (setf len-rootnlist (list-length rootnlist))
      (loop
       for root in rootnlist
       for n from 0 to len-rootnlist
       do
       (cond
        ((and rootnlist (= n 0))
         (setf rootstr (format nil "~A" (car rootnlist))))
        (rootnlist
         (setf rootstr (format nil "~A.~A" rootstr root))
         )
        (t nil))
       ;;end loop
       )
      )
     (t (setf rootstr (format nil "~A" rootstr))))
    rootstr
    ;;end let, make-root-string
    ))
;;TEST
;; (make-root-string '(2)) = "2"
;; (make-root-string '(2  3 5 2 1)) = "2.3.5.2.1"




;;MAKE-TREEVIEW-FRAME
;;
;;ddd
(defun make-treeview-frame (list &key (frame-title "Tom's List Tree View")
                                 group-by-val-p  order-by-rank-p
                                 last-list=value-p (valkey :V=) (sublistkey :SL)
                                 (min-treeview-pane-height 800)
                                 (min-treeview-pane-width 500)
                                 frame-init-args (expand-tree-p 99)
                                 (initial-y 20))
  #|  background   checkbox-change-callback   checkbox-initial-status   delete-item-callback    has-root-line     expandp-function    font  capi:help-key    image-lists   internal-border   keep-selection-p    leaf-node-p-function   retain-expanded-nodes   visible-border|#
  "In U-trees, makes a tree-view frame that uses make-tree-nodes to create a tree view of ANY list to any degree of nesting. expand-tree-p= T for initial expansion of all nodes. last-list=value-p puts the value after the sublists."
  (multiple-value-setq (*tree-nodes-list *tree-rootn-list *tree-flat-rootn-list 
                                         *tree-root-leveln-list *totaln-nodes  *tree-roots-list)
      (make-tree-nodes-list list :group-by-val-p group-by-val-p  
                            :order-by-rank-p  order-by-rank-p
                            :last-list=value-p last-list=value-p
                            :valkey valkey :sublistkey sublistkey))
  (let*
      ((frame-call-args `(make-instance 'my-tree-view-interface :title ,frame-title))
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
                                    (list  :y initial-y :visible-min-height min-treeview-pane-height
                                          :visible-min-width min-treeview-pane-width)))            
            
      (setf frame-call-args (append frame-call-args frame-init-args))

    (setf tree-inst (eval frame-call-args))
    ;;(break "y")
    
    ;;SET THE VARIABLE VALUES
#|    (setf (slot-value tree-inst 'totaln-nodes) *totaln-nodes
          (slot-value tree-inst 'treeview-expand-level) expand-tree-p
          (slot-value tree-inst 'tree-nodes-list) *tree-nodes-list
          (slot-value tree-inst 'tree-roots-list) *tree-roots-list
          (slot-value tree-inst 'tree-rootn-list) *tree-rootn-list
          ;;(slot-value tree-inst ' 
          )|#
    (capi:display tree-inst)
    ))
;;TEST
;;  (setf *testlist22 '(a (b c (d e (f g (h i j)) (k l) m (n (o (p q (r (s)) t) u v) w) x y ) z) end))
;; (make-treeview-frame *testlist22)
;; (make-treeview-frame *file-elmsymvals)
;; (make-treeview-frame *file-elmsymvals  :expand-tree-p 1)
;; (setf *valtest1 '(happy love health (truth knowledge integrity (self-man (time-man goalset) interp (romance friends family others) career (contribute income security fun) impact productivity))))
;; (make-treeview-frame *valtest1)

;;OLD DELETE AFTER TEST NEW ---------------------
(defun make-treeview-frame (list &key (frame-title "Tom's List Tree View")
                                 group-by-val-p  order-by-rank-p
                                 last-list=value-p (valkey :V=) (sublistkey :SL)
                                 (min-treeview-pane-height 800)
                                 (min-treeview-pane-width 500)
                                 frame-init-args (expand-tree-p 99)
                                 (initial-y 20))
  #|  background   checkbox-change-callback   checkbox-initial-status   delete-item-callback    has-root-line     expandp-function    font  capi:help-key    image-lists   internal-border   keep-selection-p    leaf-node-p-function   retain-expanded-nodes   visible-border|#
  "In U-trees, makes a tree-view frame that uses make-tree-nodes to create a tree view of ANY list to any degree of nesting. expand-tree-p= T for initial expansion of all nodes. last-list=value-p puts the value after the sublists."
  (multiple-value-setq (*tree-nodes-list *tree-rootn-list *tree-flat-rootn-list 
                                         *tree-root-leveln-list *totaln-nodes  *tree-roots-list)
      (make-tree-nodes-list list :last-list=value-p last-list=value-p
                            :valkey valkey :sublistkey sublistkey))
  (let*
      ((frame-call-args `(make-instance 'my-tree-view-interface :title ,frame-title))
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
                                    (list  :y initial-y :visible-min-height min-treeview-pane-height
                                          :visible-min-width min-treeview-pane-width)))            
            
      (setf frame-call-args (append frame-call-args frame-init-args))

    (setf tree-inst (eval frame-call-args))
    ;;(break "y")
    
    ;;SET THE VARIABLE VALUES
#|    (setf (slot-value tree-inst 'totaln-nodes) *totaln-nodes
          (slot-value tree-inst 'treeview-expand-level) expand-tree-p
          (slot-value tree-inst 'tree-nodes-list) *tree-nodes-list
          (slot-value tree-inst 'tree-roots-list) *tree-roots-list
          (slot-value tree-inst 'tree-rootn-list) *tree-rootn-list
          ;;(slot-value tree-inst ' 
          )|#
    (capi:display tree-inst)
    ))

(defun make-tree-nodes-list  (tree &key  tree-num-list ;;(rootmult 1)
                                   group-by-val-p  order-by-rank-p
                                    last-list=value-p (valkey :V=)(sublistkey :SL)
                                    parent-rootn parent-rootnlist  (root-leveln 0) root-leveln-list
                                    frame-init-args)
  "In U-trees, INPUT any list w/any nesting.  RETURNS (values tree-nodes rootn-list flat-rootn-list root-leveln-list totaln bottom-root-list). Each node (rootnlist rootn value) where rootnlist eg (2 3 4 6) of each level node; rootn eg. \"2.3.4.6\" and key value = valkey  the original item in list OR for a parent node sublistkey (...) = key sublist of child nodes.  If  last-list=value-p, then the last level list is considered a value for the node in level below it. USED FOR CREATING CAPI:LIST-VIEW PANES in its :chidren-function used to create root trees."
  (let
      ((len-tree (list-length tree))
       (bottom-root-list)
       (tree-nodes) 
       (rootn-list)
       (rootbase 0)
       (flat-rootn-list)
       (flat-rootnlist-list)
       (rootn 0)
       (rootnlist)
       (rootnlist-list)
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
               (make-tree-nodes-list item  ;;:n (- n 1) 
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
    ;;end let, make-tree-nodes-list
    ))
;;END OLD -------------------------------------------------



;;MAKE-TREE-NODES-LIST
;;2017
;;ddd
(defun make-tree-nodes-list  (tree &key  tree-num-list ;;(rootmult 1)
                                   group-by-val-p  order-by-rank-p
                                    last-list=value-p (valkey :V=)(sublistkey :SL)
                                    parent-rootn parent-rootnlist  (root-leveln 0) root-leveln-list
                                    frame-init-args)
  "In U-trees, INPUT any list w/any nesting.  RETURNS (values tree-nodes rootn-list flat-rootn-list root-leveln-list totaln bottom-root-list). Each node (rootnlist rootn value) where rootnlist eg (2 3 4 6) of each level node; rootn eg. \"2.3.4.6\" and key value = valkey  the original item in list OR for a parent node sublistkey (...) = key sublist of child nodes.  If  last-list=value-p, then the last level list is considered a value for the node in level below it. USED FOR CREATING CAPI:LIST-VIEW PANES in its :chidren-function used to create root trees."
  (let
      ((len-tree (list-length tree))
       (bottom-root-list)
       (tree-nodes) 
       (rootn-list)
       (rootbase 0)
       (flat-rootn-list)
       (flat-rootnlist-list)
       (rootn 0)
       (rootnlist)
       (rootnlist-list)
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
               (make-tree-nodes-list item  ;;:n (- n 1) 
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
    ;;end let, make-tree-nodes-list
    ))
;;TEST
;;HEREB
;;  (setf *testlist22 '(a (b c (d e (f g (h i j)) (k l) m (n (o (p q (r (s)) t) u v) w) x y ) z) end))
;; (make-tree-nodes-list *testlist22)
;; works= 
;;tree-nodes= 
#| (((1) "1" :VA A)
 ((2)
  "2"
  :SL
  (((2 1) "2.1" :VA B)
   ((2 2) "2.2" :VA C)
   ((2 3)
    "2.3"
    :SL
    (((2 3 1) "2.3.1" :VA D)
     ((2 3 2) "2.3.2" :VA E)
     ((2 3 3)
      "2.3.3"
      :SL
      (((2 3 3 1) "2.3.3.1" :VA F)
       ((2 3 3 2) "2.3.3.2" :VA G)
       ((2 3 3 3)
        "2.3.3.3"
        :SL
        (((2 3 3 3 1) "2.3.3.3.1" :VA H)
         ((2 3 3 3 2) "2.3.3.3.2" :VA I)
         ((2 3 3 3 3) "2.3.3.3.3" :VA J)))))
     ((2 3 4)
      "2.3.4"
      :SL
      (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L)))
     ((2 3 5) "2.3.5" :VA M)
     ((2 3 6)
      "2.3.6"
      :SL
      (((2 3 6 1) "2.3.6.1" :VA N)
       ((2 3 6 2)
        "2.3.6.2"
        :SL
        (((2 3 6 2 1) "2.3.6.2.1" :VA O)
         ((2 3 6 2 2)
          "2.3.6.2.2"
          :SL
          (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P)
           ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q)
           ((2 3 6 2 2 3)
            "2.3.6.2.2.3"
            :SL
            (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R)
             ((2 3 6 2 2 3 2)
              "2.3.6.2.2.3.2"
              :SL
              (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S)))))
           ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T)))
         ((2 3 6 2 3) "2.3.6.2.3" :VA U)
         ((2 3 6 2 4) "2.3.6.2.4" :VA V)))
       ((2 3 6 3) "2.3.6.3" :VA W)))
     ((2 3 7) "2.3.7" :VA X)
     ((2 3 8) "2.3.8" :VA Y)))
   ((2 4) "2.4" :VA Z)))
 ((3) "3" :VA END))
 ((3) "3" END))|#
;;rootn-list= ("1" "2" ("2.1" "2.2" "2.3" ("2.3.1" "2.3.2" "2.3.3" ("2.3.3.1" "2.3.3.2" "2.3.3.3" ("2.3.3.3.1" "2.3.3.3.2" "2.3.3.3.3")) "2.3.4" ("2.3.4.1" "2.3.4.2") "2.3.5" "2.3.6" ("2.3.6.1" "2.3.6.2" ("2.3.6.2.1" "2.3.6.2.2" ("2.3.6.2.2.1" "2.3.6.2.2.2" "2.3.6.2.2.3" ("2.3.6.2.2.3.1" "2.3.6.2.2.3.2" ("2.3.6.2.2.3.2.1")) "2.3.6.2.2.4") "2.3.6.2.3" "2.3.6.2.4") "2.3.6.3") "2.3.7" "2.3.8") "2.4") "3")
;;flat-rootn-list= ("1" "2" "2.1" "2.2" "2.3" "2.3.1" "2.3.2" "2.3.3" "2.3.3.1" "2.3.3.2" "2.3.3.3" "2.3.3.3.1" "2.3.3.3.2" "2.3.3.3.3" "2.3.4" "2.3.4.1" "2.3.4.2" "2.3.5" "2.3.6" "2.3.6.1" "2.3.6.2" "2.3.6.2.1" "2.3.6.2.2" "2.3.6.2.2.1" "2.3.6.2.2.2" "2.3.6.2.2.3" "2.3.6.2.2.3.1" "2.3.6.2.2.3.2" "2.3.6.2.2.3.2.1" "2.3.6.2.2.4" "2.3.6.2.3" "2.3.6.2.4" "2.3.6.3" "2.3.7" "2.3.8" "2.4" "3")     root-leveln-list= (1 (2 (3 (4 (5)) (4) (4 (5 (6 (7 (8))))))))  
;; totaln= 37  bottom-level-roots=  (1 2 3)
;;
;; TEST WHEN VALUES ARE LISTS
;; (setf *testlist33 '(a (k1 1 2) b (k2 3 4) (c (k3 5 6) d (k4 7 8) (e (k5 9) f (k6 10 11))) (d (k7 1 2))))
;; (make-tree-nodes-list *testlist33 :last-list=value-p T)
;; works= (values tree-nodes rootn-list flat-rootn-list root-leveln-list totaln bottom-root-list)
;; tree-nodes= 
#|(((1) "1" :VA A)
 ((2) "2" :VA (K1 1 2))
 ((3) "3" :VA B)
 ((4) "4" :VA (K2 3 4))
 ((5)
  "5"
  :SL
  (((5 1) "5.1" :VA C)
   ((5 2) "5.2" :VA (K3 5 6))
   ((5 3) "5.3" :VA D)
   ((5 4) "5.4" :VA (K4 7 8))
   ((5 5)
    "5.5"
    :SL
    (((5 5 1) "5.5.1" :VA E)
     ((5 5 2) "5.5.2" :VA (K5 9))
     ((5 5 3) "5.5.3" :VA F)
     ((5 5 4) "5.5.4" :VA (K6 10 11))))))
 ((6) "6" :SL (((6 1) "6.1" :VA D) ((6 2) "6.2" :VA (K7 1 2)))))|#
;;root-leveln-list=  (1 (2 (3)) (2))         totaln= 17  bottom-root-list=  (1 2 3 4 5 6)
;;rootn-list= ("1" "2" "3" "4" "5" ("5.1" "5.2" "5.3" "5.4" "5.5" ("5.5.1" "5.5.2" "5.5.3" "5.5.4")) "6" ("6.1" "6.2"))
;;flat-rootn-list=  ("1" "2" "3" "4" "5" "5.1" "5.2" "5.3" "5.4" "5.5" "5.5.1" "5.5.2" "5.5.3" "5.5.4" "6" "6.1" "6.2")




;;GET-NODE-CHILDLISTS
;;
;;ddd
(defun get-node-childlists (target-rootnlist tree-node-list &key (target-leveln 1)
                                         (valkey :VA)(sublistkey :SL))
  "   RETURNS: (values return-childlists child-rootnlists root-value child-sublists)  Set parent-rootlist= 0 or NIL to find base roots.
   "
  (when (equal target-rootnlist 0)
    (setf rootnlist NIL))
  (unless (listp target-rootnlist)
    (setf target-rootnlist (list target-rootnlist)))
  (let*
      ((n-levels (list-length target-rootnlist))
       (return-childlists)
        (root-value)
        (child-sublists)
        (child-rootnlists)
       ;;eg (2 3  5), pld= 5, pln= 3
       )
    (when (null target-rootnlist)
      (setf parent-leveln 0))
    ;;(break "target-rootnlist")
    (loop
     for  node-n in target-rootnlist
     for leveln from 1 to n-levels
     do
     (when (equal target-leveln leveln)
       ;;(break "(equal target-leveln leveln")
       (loop
        for item in tree-node-list
        do
        ;;MUST FIRST DIFFERENTIATE BETWEEN A NODE-LIST AND A NESTED-LIST
        ;; BEFORE DOING NEXT COND
        (let*
            ((test-rootnlist)
             (test-noden)
             (len-item)
             (target-parentlist)
             (parent-sublists)
             )
          (when (and item 
                     (listp item))
            (setf  test-rootnlist (car item)
                   test-noden (nth (- leveln 1) test-rootnlist))

            (when (and (equal (setf len-item (list-length test-rootnlist)) target-leveln)
                       (equal test-noden node-n))
              (setf target-parentlist item
                    parent-sublists (get-key-value sublistkey target-parentlist)
                    child-rootnlists (get-rootnlists-sublists parent-sublists))
            ;;(break "before recurse")

              (multiple-value-bind (return-childlists1 child-rootnlists1) 
                  (get-node-childlists target-rootnlist parent-sublists 
                                                     :target-leveln (+ target-leveln 1)
                                                     :valkey valkey :sublistkey sublistkey)
              ;;(break "after recurse")
                (cond
                 (return-childlists1
                  (setf return-childlists return-childlists1
                        child-rootnlists child-rootnlists1)
                  (return))
                 ((equal target-rootnlist test-rootnlist)
                  (setf return-childlists item)
                  (return))
                 (t nil))
                  ;;end mvb, whens
                  )))
          ;;end let, inner loop
          ))
       (when  return-childlists
         (return))
       ;;end when,, outer
       ))
  (when (and (equal target-leveln 1) return-childlists)
      (setf  child-sublists (get-key-value sublistkey return-childlists)
             child-rootnlists (get-nth-in-all-lists 0  child-sublists)
             root-value (get-key-value valkey return-childlists)))

    (values return-childlists child-rootnlists root-value child-sublists)
    ;;end let, get-node-childlists
    ))
;;TEST
;; (setf *testcl33 '(((1) "1" :VA A) ((2) "2" :SL (((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3) "2.3" :SL (((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6) "2.3.6" :SL (((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))) ((2 3 6 3) "2.3.6.3" :VA W))) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y))) ((2 4) "2.4" :VA Z))) ((3) "3" :VA END))  )
;; for end in value
;; (get-node-childlists  '(2 3 5)  *testcl33)
;; works = ((2 3 5) "2.3.5" :VA M) NIL  M   NIL
;; for end in sublists
;; (get-node-childlists  '(2 3 4)  *testcl33)
;; works= ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) 
;;child-rootnlists= ((2 3 4 1) (2 3 4 2))
;;   rootvalue= NIL sublists= (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))
;; (get-node-childlists  '(2 3 1) *tree-nodes-list)


;;GET-ROOTNLISTS-SUBLISTS
;;
;;ddd
(defun get-rootnlists-sublists (parent-rootlist &key (sublistkey :SL))
  (let
      ((child-rootnlists)
       (child-rootlists)
       )
    (cond
     ((listp parent-rootlist)
      (setf child-rootlists (get-key-value sublistkey parent-rootlist))
      ;;(break)
      (loop
       for sublist in child-rootlists
       do
       (let
           ((rootnlist)
            )
         (when (listp sublist)
           (setf rootnlist (car  sublist)
                 child-rootnlists (append child-rootnlists (list rootnlist))))
         ;;end let,loop
         ))
      )
     (t nil))
    child-rootnlists
    ;;end let,get-rootnlists-sublists
    ))
 ;;TEST
;; (get-rootnlists-sublists    '((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))))        =  ((2 3 4 1) (2 3 4 2))
;;  (  

#|  WORKS--NOT NEEDED
;;FIND-CHILDLIST-FOR-LEVELN
;;
;;ddd
(defun find-childlist-for-leveln (parent-leveln parent-rootn parent-sublists &key (sublistkey :SL))
  "In U-trees, "
  (let
      ((return-childlist)       
       )
    (loop
     for child in parent-sublists
     do
     (let*
         ((rootnlist (car child))
          (test-rootn (nth (- parent-leveln 1) rootnlist))
          (test-leveln (list-length rootnlist))
          )
       (when (and (equal parent-leveln test-leveln)
                  (equal parent-rootn test-rootn))
         (setf return-childlist  child)
              (return))
       ;;end let,loop
       ))
    return-childlist 
    ;;end let, find-childlist
    ))|#
;;TEST
;; (setf *testcl11 '(((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R)  ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S)))) )
;; (find-childlist-for-leveln 7 2 *testcl11)
;; works=  ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S)))

;;HEREA
#|((2)
  "2"
  :SL
  (((2 1) "2.1" :VA B)
   ((2 2) "2.2" :VA C)
   ((2 3)
    "2.3"
    :SL
    (((2 3 1) "2.3.1" :VA D)
     ((2 3 2) "2.3.2" :VA E)
     ((2 3 3)
      "2.3.3"
      :SL
      (((2 3 3 1) "2.3.3.1" :VA F)
       ((2 3 3 2) "2.3.3.2" :VA G)
       ((2 3 3 3)
        "2.3.3.3"
        :SL
        (((2 3 3 3 1) "2.3.3.3.1" :VA H)
         ((2 3 3 3 2) "2.3.3.3.2" :VA I)
         ((2 3 3 3 3) "2.3.3.3.3" :VA J)))))
     ((2 3 4)
      "2.3.4"
      :SL
      (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L)))
     ((2 3 5) "2.3.5" :VA M)
     ((2 3 6)
      "2.3.6"
      :SL
      (((2 3 6 1) "2.3.6.1" :VA N)
       ((2 3 6 2)
        "2.3.6.2"
        :SL
        (((2 3 6 2 1) "2.3.6.2.1" :VA O)
         ((2 3 6 2 2)
          "2.3.6.2.2"
          :SL
          (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P)
           ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q)
           ((2 3 6 2 2 3)
            "2.3.6.2.2.3"
            :SL
            (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R)
             ((2 3 6 2 2 3 2)
              "2.3.6.2.2.3.2"
              :SL
              (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S)))))
           ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T)))
         ((2 3 6 2 3) "2.3.6.2.3" :VA U)
         ((2 3 6 2 4) "2.3.6.2.4" :VA V)))
       ((2 3 6 3) "2.3.6.3" :VA W)))
     ((2 3 7) "2.3.7" :VA X)
     ((2 3 8) "2.3.8" :VA Y)))
   ((2 4) "2.4" :VA Z)))
|#


;; TEST-FUNCTIONS2.lisp 



;;GET-SET-APPEND-DELETE-TREE-NODELIST
;;
;;ddd
(defun get-set-append-delete-tree-nodelist (new-value key  target-rootnlist tree-node-list 
                                                        &key (keyloc-n 2) (val-nth 1) (target-leveln 1)
                                                        (valkey :VA)(sublistkey :SL)  (test 'my-equal)
                                                        append-value-p add-value-p
                                                        put-key-after-items  put-value-after-items 
                                                        new-begin-items new-end-items
                                                        paren-after-begin-items-p
                                                        splice-key-value-in-list-p
                                                        break-if-keys-not-match-p
                                                        splice-old-new-values-p 
                                                        parens-after-begin-items-p)
  "   RETURNS: (values return-nodelist child-rootnlists root-value childsublists new-childlist
            return-new-value old-value)  Set parent-rootlist= 0 or NIL to find base roots.
   "
  (when (equal target-rootnlist 0)
    (setf rootnlist NIL))
  (unless (listp target-rootnlist)
    (setf target-rootnlist (list target-rootnlist)))
  (let*
      ((n-levels (list-length target-rootnlist))
       (return-nodelist)
       (new-tree-node-list)
       (return-value)
       (root-value)
       (childsublists)
       (child-rootnlists)
       (return-keylist)(new-keylist)
       (return-new-value)(old-value)
       (new-keylist)
       (len-tree-node-list (list-length tree-node-list))
       (list-tail)
       (list-head)
       ;;eg (2 3  5), pld= 5, pln= 3
       )
    (when (null target-rootnlist)
      (setf target-leveln 0))
    
    (cond
     ;;FOR COMPLEX CASES USE GET-SET-APPEND-DELETE-NESTED-LIST
     ((or  append-value-p add-value-p put-key-after-items  put-value-after-items                              new-begin-items new-end-items parens-after-begin-items-p
           splice-key-value-in-list-p break-if-keys-not-match-p splice-old-new-values-p )
      (let
          ((key-spec-lists `((,target-rootnlist 0)(,key T)))
           )
 ;;(return-keylist new-return-nested-lists new-keylist return-value  old-keylist last-key-found-p )
;; (return-nodelist new-tree-node-list child-rootnlists root-value childsublists    return-new-value old-value)
        (multiple-value-setq (return-nodelist new-tree-node-list new-keylist
                                              return-value old-keylist last-key-found-p)
            (get-set-append-delete-keyvalue-in-nested-list new-value 
                                                           key-spec-lists tree-node-list
                                                           :append-value-p append-value-p
                                                           :add-value-p add-value-p
                                                           :put-key-after-items put-key-after-items
                                                           :put-value-after-items put-value-after-items
                                                           :new-begin-items new-begin-items
                                                           :new-end-items new-end-items
                                                           :parens-after-begin-items-p parens-after-begin-items-p
                                                           :splice-key-value-in-list-p splice-key-value-in-list-p
                                                           :splice-old-new-values-p splice-old-new-values-p ))
        ;;end let, or
        ))
     (t

      (loop
       for  node-n in target-rootnlist
       for leveln from 1 to n-levels
       do
       (when (equal target-leveln leveln)
         ;;(break "(equal target-leveln leveln")
         (loop
          for item in tree-node-list
          for item-n from 0 to len-tree-node-list ;;not needed?
          do
          (afout 'out (format nil "item= ~A item-n= ~A" item item-n))
          ;;NO? Fixed in get-set- functions;; must first differentiate between a node-list and a nested-list before doing next cond
          (let*
              ((test-rootnlist)
               (test-noden)
               (len-item)
               (target-parentlist)
               (parent-sublists)
               (new-tree-node-sublist)
               )
            (cond
             ((and item (listp item))
              (setf  test-rootnlist (car item)
                     test-noden (nth (- leveln 1) test-rootnlist)
                     len-item (list-length item))

              (cond
               ;;TARGET-ROOTNLIST FOUND
               ((equal target-rootnlist test-rootnlist)
                (setf keyloc-n  (nth-value 1 (find-list-item key item :test test)))

                ;;GET LIST-HEAD AND LIST-TAIL (items before and after key and value)
                (cond
                 ;;when key not found
                 ((null keyloc-n)
                  (cond
                   ;;already has a sublistkey, put new key BEFORE sublistkey
                   ((find-list-item sublistkey item :test 'equal)
                    (setf keyloc-n (- len-item 3)
                          list-head (butlast item  (- len-item 2))
                          list-tail (nthcdr  keyloc-n item))
                    )
                   ;;already has a valkey, put new key & value AFTER value
                   (t (setf keyloc-n (- len-item 1)
                            list-head item
                            list-tail NIL) ))
                  ;;end (null keyloc-n)
                  )
                 (t 
                  ;;SO ADD KEY AND VALUE TO TARGET LIST
                  (setf list-head (butlast item  (- len-item keyloc-n))
                        list-tail (nthcdr (+ keyloc-n val-nth 1) item))))

                ;;(BREAK "item-n val-nth 1 keyloc-n 2")
                (multiple-value-setq (return-keylist new-keylist return-new-value old-value)
                    (get-set-append-delete-keyvalue key new-value :keyloc-n keyloc-n :val-nth val-nth  
                                                    :old-keylist ITEM
                                                    :append-value-p append-value-p :add-value-p add-value-p
                                                    :put-key-after-items put-key-after-items
                                                    :put-value-after-items put-value-after-items
                                                    :new-begin-items new-begin-items :new-end-items new-end-items
                                                    :parens-after-begin-items-p parens-after-begin-items-p
                                                    :splice-key-value-in-list-p splice-key-value-in-list-p
                                                    :break-if-keys-not-match-p break-if-keys-not-match-p
                                                    :splice-old-new-values-p splice-old-new-values-p ))       
                ;;    (values return-nodelist child-rootnlists root-value childsublists)
                ;; (get-set-append-delete-keyvalue  :SL '(NEW SL VALUE) :KEYLOC-N 2 :VAL-NTH 1 :OLD-KEYLIST '((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))))

                (cond
                 ;;to splice new value into list, not add as a list
                 ((and (or add-value-p splice-key-value-in-list-p splice-old-new-values-p)
                       (listp root-value))  ;;not add-value-p
                  (setf  return-nodelist  (append list-head
                                                  return-keylist   ;;was return-keylist
                                                  list-tail)))
                 ;;to append as a list
                 (t (setf  return-nodelist  (append list-head
                                                    return-keylist    ;;was return-keylist
                                                    list-tail))))
                (setf new-tree-node-list (append new-tree-node-list (list return-nodelist)))
                ;;(break "after get-set-append-delete-keyvalue")
                (return)                      
                ;;end found rootnlist clause
                )  
               ;;IS THE TARGET NODE SAME AS THIS NODE W/IN THE ROOTNLIST?
               ;;IF SO RECURSE
               ((and (equal (length (car item)) target-leveln)
                     ;; (setf len-item (list-length test-rootnlist)) target-leveln)
                     (equal test-noden node-n))
                ;;target-parentlist= ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L)))
                (setf target-parentlist item
                      parent-sublists (get-key-value sublistkey target-parentlist)
                      child-rootnlists (get-rootnlists-sublists parent-sublists))
                ;;(break "before recurse")

                (multiple-value-setq (return-nodelist new-tree-node-sublist child-rootnlists 
                                                      root-value childsublists return-new-value old-value) 
                    (get-set-append-delete-tree-nodelist new-value key target-rootnlist parent-sublists 
                                                         :target-leveln (+ target-leveln 1)
                                                         :valkey valkey :sublistkey sublistkey
                                                         :keyloc-n keyloc-n :val-nth val-nth  
                                                         :append-value-p append-value-p :add-value-p add-value-p
                                                         :put-key-after-items put-key-after-items
                                                         :put-value-after-items put-value-after-items
                                                         :new-begin-items new-begin-items :new-end-items new-end-items
                                                         :parens-after-begin-items-p parens-after-begin-items-p
                                                         :splice-key-value-in-list-p splice-key-value-in-list-p
                                                         :break-if-keys-not-match-p break-if-keys-not-match-p
                                                         :splice-old-new-values-p splice-old-new-values-p ))
               

                ;;end equal length and recurse
                )
               (t 
                ;;in any case append new-tree-node-list
                (setf new-tree-node-sublist (append new-tree-node-sublist (list item)))))
              ;;at end 
              (setf new-tree-node-list (append new-tree-node-list  new-tree-node-sublist))

              ;;end clause (and item
              )
             ;;ITEM NOT A LIST
             (t
              (setf new-tree-node-list (append new-tree-node-list (list item)))))
            ;;end let, inner loop
            ))
         (when  return-nodelist
           (return))
         ;;end when,, outer loop
         ))
      ;;end for non-complex tree cases not using nested-lists
      ))
    ;;(return-keylist new-keylist root-value old-value)
    (when (and (equal target-leveln 1) return-nodelist)
      (setf  childsublists   (get-key-value sublistkey return-nodelist)
             child-rootnlists (get-nth-in-all-lists 0  childsublists)
             root-value (get-key-value valkey return-nodelist))
      ;;(break "end target-leveln= 1 recurse")
      )
    ;; (get-key-value :SL '((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))))

    ;;(break "end recurse")
    (values return-nodelist new-tree-node-list child-rootnlists root-value childsublists 
             return-new-value old-value)
    ;;end let, get-set-append-delete-tree-nodelist
    ))
;;TEST
;; (setf *testcl33 '(((1) "1" :VA A) ((2) "2" :SL (((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3) "2.3" :SL (((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6) "2.3.6" :SL (((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S)))))    ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))) ((2 3 6 3) "2.3.6.3" :VA W))) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y))) ((2 4) "2.4" :VA Z))) ((3) "3" :VA END))  )
;; for end in value
;; (get-node-childlists  '(2 3 5)  *testcl33)
;; works = ((2 3 5) "2.3.5" :VA M) NIL  M   NIL
;; for end in sublists
;; (get-node-childlists  '(2 3 4)  *testcl33)
;; works= ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) 
;;child-rootnlists= ((2 3 4 1) (2 3 4 2))
;;   rootvalue= NIL sublists= (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))
;;
;; (get-set-append-delete-keyvalue-in-nested-list :GET '(('(2 3 6 2) 0 2) ) *testcl33)
;; FOR GET
;; (get-set-append-delete-tree-nodelist  :GET  :SL '(2 3 6 2) *testcl33)
;; works=
;;(values return-nodelist new-tree-node-list child-rootnlists root-value childsublists  return-new-value old-value)
;;return-nodelist= ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V)))
;;new-tree-node-list=  (((1) "1" :VA A) ((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6 1) "2.3.6.1" :VA N) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y) ((2 4) "2.4" :VA Z) ((3) "3" :VA END))
;; child-rootnlists= ((2 3 6 2 1) (2 3 6 2 2) (2 3 6 2 3) (2 3 6 2 4))
;;root-value= NIL (correct)
;;childsublists= (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))
;; return-new-value old-value all = childsublists
;; FOR SET NEW-VALUE
;;(values return-nodelist child-rootnlists root-value childsublists new-childlist  return-new-value old-value)
;; ;; (get-set-append-delete-tree-nodelist  '(NEW SL VALUE)  :SL '(2 3 6 2) *testcl33)
;;works= 
;;return-nodelist= ((2 3 6 2) "2.3.6.2" :SL (NEW SL VALUE))
;;  child-rootnlists= NIL  root-value= NIL 
;; childsublists= (NEW SL VALUE)  new-childlist= NIL 
;; return-new-value= (NEW SL VALUE)
;;old-value= (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))
;; with a real sublist
;; (get-set-append-delete-tree-nodelist 'NEW-VALUE   :VA  '(2 3 6 2 2 4)  *testcl33)
;; works= ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA NEW-VALUE)
;; new-tree-node-list=  (((1) "1" :VA A) ((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA NEW-VALUE) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V) ((2 3 6 3) "2.3.6.3" :VA W) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y) ((2 4) "2.4" :VA Z) ((3) "3" :VA END))
;; NIL NEW-VALUE  NIL   NIL  NEW-VALUE   T

;;
;;(get-set-append-delete-tree-nodelist '(NEW SUBLISTS GO HERE) :SL '(2 3 6 2 2 4)       *testcl33)
;; works= 
;;return-nodelist== ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T :SL (NEW SUBLISTS GO HERE) T)
;;new-tree-node-lists= (((1) "1" :VA A) ((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T :SL (NEW SUBLISTS GO HERE) T) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V) ((2 3 6 3) "2.3.6.3" :VA W) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y) ((2 4) "2.4" :VA Z) ((3) "3" :VA END))
;;NIL  T   (NEW SUBLISTS GO HERE)   (NEW SUBLISTS GO HERE)   NIL

;;SSS TEST THE NESTED-LIST SUBFUNCTION
;;(get-set-append-delete-tree-nodelist '(NEW SUBLISTS GO HERE) :SL '(2 3 6 2 2 3)       *testcl33 :append-value-p T)
;; works= (:SL ((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))) (NEW SUBLISTS GO HERE))
;;       (((1) "1" :VA A) ((2) "2" :SL (((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3) "2.3" :SL (((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6) "2.3.6" :SL (((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) (((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))) (NEW SUBLISTS GO HERE)))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))) ((2 3 6 3) "2.3.6.3" :VA W))) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y))) ((2 4) "2.4" :VA Z))) ((3) "3" :VA END))
;;     (2)   NIL   ((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R)    NIL   NIL








;;DECODE-DOT-STRING
;;
;;ddd
(defun decode-dot-string (dot-string)
  (let
      ((digit-str)
       (digit)
       (digit-str-list)
       (all-items-list)
       )
    (setf digit-str-list
          (divide-string-to-all-tokens dot-string :char-delim-list '(#\.) :ignore-char-list nil))
    ))








;;==================== COPIED FROM USERMAN, CAPI TREES =======

#|
;;works
(capi:contain
 (make-instance
  'capi:graph-pane
  :roots '(1)
  :children-function
  #'(lambda (x)
      (when (< x 8)
        (list (* 2 x) (1+ (* 2 x)))))))
;;me
(capi:contain
 (make-instance
  'capi:graph-pane
  :roots '(1)
  :children-function
  #'(lambda (x)
      (when (< x 8)
        (list (+ 2 x) (1+ (* 2 x)))))))
;; works, produces odd shaped with multiple lines to same numbers (7, 
(capi:contain
 (make-instance
  'capi:graph-pane
  :roots '(1 2)
  :children-function
  #'(lambda (x)
      (when (< x 10)
        (list (+ 20 x) (1+ (* 2 x)))))))
|#






;; ================== COPIED FROM tree-view.lisp in LW7.1 examples======
;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:tree-view.lisp,v 1.8.3.1 2017/01/19 11:50:03 martin Exp $" -*-

;; Copyright (c) 1987--2017 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/tree-view.lisp
;;
;; This example demonstrates the uses of the tree-view in the CAPI.
;; Shows how to setup the images for a tree-view.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-TREE-VIEW)
;;
;;  Can also try:
;;
;;     (CL-USER::TEST-TREE-VIEW-WITH-STANDARD-IMAGES)
;;
;; Same as above, but uses standard images for some of the items. 

;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Create an image list for use by the tree-view
;;----------------------------------------------------------------------------

#|(defvar *my-image-list*
  (make-instance 'capi:image-list
		  :image-sets (list (capi:make-general-image-set
				     :id #.(gp:read-external-image
                                            (current-pathname"tree.bmp"))
				     :image-count 4))
		  :image-width 16
		  :image-height 16))|#


;;----------------------------------------------------------------------------
;; Define the interface option-pane-test
;;----------------------------------------------------------------------------

#|(capi:define-interface tree-view-test ()
  () 
  (:panes
   (tree capi:tree-view
         :roots '(1 2 3 4)
         :children-function #'(lambda (x)
                                (and (< x 100)
                                     (let ((base (* x 4)))
                                       (list (+ base 1)
                                             (+ base 2)
                                             (+ base 3)
                                             (+ base 4)))))
         :image-lists (list :normal *my-image-list*)
         :image-function #'(lambda (x) (mod (1- x) 4))
         :visible-min-width 200
         :visible-min-height 200
         :retain-expanded-nodes t
         :print-function #'(lambda(x) (format nil "~d : ~r" x x ))
         :selection-callback #'(lambda (item self)
                                 (declare (ignore self))
                                 (format t "~&Select item ~S~%" item))
         :action-callback #'(lambda (item self)
                              (declare (ignore self))
                              (format t "~&Activate item ~S~%" item))
         :delete-item-callback #'(lambda (self item)
                                   (declare (ignore self))
                                   (format t "~&Delete item ~S~%" item))))
  (:layouts
   (default-layout
    capi:simple-layout
    '(tree)))
  (:default-initargs
   :title "Tree View Test"))|#


;;----------------------------------------------------------------------------
;; A simple test function
;;----------------------------------------------------------------------------

(defun test-tree-view ()
  (capi:display (make-instance 'tree-view-test :title "Simple Tree view")))
;;TEST
;; (test-tree-view)


;;----------------------------------------------------------------------------
;; Add also standard images
;;----------------------------------------------------------------------------

;;; The image function for th etree view with standard images. 
;;; For some of the items return a symbol specifying a standard image. 

(defun my-image-function-with-standard-images (x)
  (case (mod x 9)
    (1 :std-cut)
    (2 :std-file-save)
    #+mswindows (3 :view-parent-folder)
    #+mswindows (4 :hist-favorites)
    (t  (mod (1- x) 4))))

;;; The print function.
;;; For items that use standard image, also print the image name. 

(defun my-print-function-with-standard-images (x)
  (format nil "~d : ~r ~@[ [ ~a ]~]" x x 
          (case (mod x 9)
            (1 "Cut")
            (2 "Save")
            #+mswindows (3 "Parent Folder")
            #+mswindows (4 "Favorites"))))
   

(defun test-tree-view-with-standard-images ()
  (let ((interface (make-instance 'tree-view-test :title "Tree view with standard images")))
    (with-slots (tree) interface
      (setf (capi:tree-view-image-function tree) 'my-image-function-with-standard-images
            (capi:collection-print-function tree) 'my-print-function-with-standard-images))
    (capi:display interface)))

;;====================== end copied from tree-view.lisp ================



;;XXX =============== COPIED FROM LW7.1 CAPI UM ====================

#|
TREE-VIEW CLASS
Summary A tree view is a pane that displays a hierarchical list of items.
Each item may optionally have an image and a checkbox.
Package capi PAGE 973

Superclasses choice
titled-object
simple-pane
Initargs :roots A list of the root nodes.

:CHILDREN-FUNCTION
Returns the children of A NODE and hence DEFINES THE HIERARCHY IN THE TREE.

:leaf-node-p-function
Optional function which determines
whether a node is a leaf node (that is, has no
children). This is useful if it can be computed faster than the children-function.
:retain-expanded-nodes
Specifies if the tree view remembers
whether hidden nodes were expanded.

:EXPANDP-FUNCTION
A designator for a function of one argument, or nil.

:ACTION-CALLBACK-EXPAND-P
A boolean. The default value is nil.

:delete-item-callback
A function designator for a function of two
arguments.

:right-click-extended-match
Controls the area within which selection by
the mouse right button occurs. Default t.

:has-root-line
Controls whether the line and expanding
boxes of the root nodes are drawn. Default t.21 CAPI Reference Entries
974
Initargs for handling check boxes. Note that these do not
work on Cocoa:
;;CHECKBOXES 
:CHECKBOX-STATUS
Controls whether the tree has checkboxes,  except on Cocoa. If non-nil, the value should
be a non-negative integer less than the length of the image-list, or t.
An integer specifies the default initial status, and t means the same as 2 (that is, by default the checkboxes are checked initially).
The default is nil, meaning no checkboxes.

:checkbox-next-map
Controls the change in status when the user clicks on a checkbox. Can be an array, a function or an integer. Default #(2 2 0).

:checkbox-parent-function
Controls the changes in the ancestors when the status of an item is changed.

:checkbox-child-function
Controls the changes in the descendants
when the status of an item is changed.
:checkbox-change-callback
A function called when the status of an item
is changed interactively.
:checkbox-initial-status
Specifies the initial status of specific items.

;;IMAGES
Initargs for handling images:
:image-function  Returns an image for a node.
:state-image-function    Returns a state image for a node.975
:image-lists  A plist of keywords and image-list objects.
:use-images Flag to specify whether items have images. Defaults to t.
:use-state-images   Flag to specify whether items have state images. Defaults to nil.
:image-width Defaults to 16.
:image-height Defaults to 16.
:state-image-width  Defaults to image-width.
:state-image-height  Defaults to image-height. 

ACCESSORS 
tree-view-roots
tree-view-children-function
tree-view-image-function
tree-view-state-image-function
tree-view-leaf-node-p-function
tree-view-retain-expanded-nodes
tree-view-expandp-function
tree-view-action-callback-expand-p
tree-view-right-click-extended-match
tree-view-has-root-line
tree-view-checkbox-next-map
tree-view-checkbox-parent-function
tree-view-checkbox-status
tree-view-checkbox-child-function
tree-view-checkbox-change-callback
tree-view-checkbox-initial-status

READERS 
tree-view-checkbox-status21 CAPI Reference Entries
976

DESCRIPTION 
The tree view pane allows the user to select between items displayed in a hierarchical list. Although it is a choice, only single selection interaction is supported. Use extendedselection-tree-view if you need other selection interaction styles.

CHIDREN-FUNCTION
The HIERARCHY OF ITEMS IN THE TREE IS DEFINED BY THE CHILDREN FUNCTION, which must be a function taking a single argument
and returning A LIST OF CHILDREN. 
When an item is expanded, whether programmatically, automatically, or in response to a
user gesture, 
the system calculates what children this item has BY CALLING THE CHILDREN-FUNCTION ON IT.
What children the children-function returns for an item is up to you. 
However, the list must not include an object which is cl:eql to another object in the tree. To work
 sensibly it also needs to be consistent over time, that is return the same objects each time it is called, unless the state of the entity that the tree represents changes. It should also be reasonably fast, as the user will be waiting to see the items.

If the TREE IS SUPPOSED TO DISPLAY ITEMS THAT ARE "THE SAME" IN DIFFERENT PARTS OF THE TREE, you can DEFINE A "WRAPPER", typically cl:defstruct with a few slots, and return a list of these wrappers (each pointing to the actual object). 
This wrapping is also useful for keeping other information related to the display in the tree, for example the string or the image to display, and maybe cache the children.

If leaf-node-p-function is not supplied, the children-function is also used to decide whether unexpanded nodes are leaf nodes or not (and hence whether to display the expanding box). If the children-function is slow, this may slow significantly the display of large trees. If it is possible TO CHECK FOR THE EXISTENCE OF CHILDREN FASTER, YOU SHOULD SUPPLY LEAF-NODE-PFUNCTION to avoid this slow down.

The default value of children-function is (constantly false), that is no children, and hence only the roots are displayed.977

EXPANDP-FUNCTION 
expandp-function controls automatic expansion of nodes (items) in the tree-view. 
By default, initially only the items specified by the roots argument are displayed. 
This INITIAL DISPLAY CAN BE ALTERED by supplying a function expandp-function which allows further items to be displayed. 
If supplied, expandp-function should be a function which is called on the roots and is called recursively on the children if it returns true. 
When the user expands a node, expandp-function is called on each newly created child node, which is expanded if this call returns true, and so on recursively. The default value of
expandp-function is nil so that there is no automatic expansion and only the root nodes are visible initially.

The default value of RETAIN-EXPANDED-NODES is t.
Any item which has children has a small expansion button next to it to indicate that it can be expanded. When the user clicks on this button, the children nodes (as determined by the children function) are displayed.

If ACTION-CALLBACK-EXPAND-P is true, then the activate gesture expands a collapsed node, and collapses an expanded node.
This expansion and contraction of the node is additional to any supplied action-callback.

DELETE-ITEM-CALLBACK is called when the user presses the Delete
key. Two arguments are passed: the tree-view and the
selected item item. Note that, apart from calling the callback,
the system does nothing in response to the Delete key. In particular, if you want to remove the selected item, delete-itemcallback needs to do it by changing what the children-function
returns when called on the parent of item. Normally you also
need to to call tree-view-update-item with in-parent = t to
actually update the tree on the screen.
Note also that in extended-selection-tree-view (a subclass of tree-view), if the interaction was not explicitly
changed to :single-selection, the second argument to
delete-item-callback is a list of the selected items (even when
only one item is selected)
Summary A tree view is a pane that displays a hierarchical list of items.
Each item may optionally have an image and a checkbox.
Package capi973
Superclasses choice
titled-object
simple-pane
Initargs :roots A list of the root nodes.
:children-function
Returns the children of a node and hence
defines the hierarchy in the tree.
:leaf-node-p-function
Optional function which determines
whether a node is a leaf node (that is, has no
children). This is useful if it can be computed faster than the children-function.
:retain-expanded-nodes
Specifies if the tree view remembers
whether hidden nodes were expanded.
:expandp-function
A designator for a function of one argument,
or nil.
:action-callback-expand-p
A boolean. The default value is nil.
:delete-item-callback
A function designator for a function of two
arguments.
:right-click-extended-match
Controls the area within which selection by
the mouse right button occurs. Default t.
:has-root-line
Controls whether the line and expanding
boxes of the root nodes are drawn. Default t.21 CAPI Reference Entries
974
Initargs for handling check boxes. Note that these do not
work on Cocoa:
:checkbox-status
Controls whether the tree has checkboxes,
except on Cocoa. If non-nil, the value should
be a non-negative integer less than the
length of the image-list, or t.
An integer specifies the default initial status,
and t means the same as 2 (that is, by
default the checkboxes are checked initially).
The default is nil, meaning no checkboxes.
:checkbox-next-map
Controls the change in status when the user
clicks on a checkbox. Can be an array, a function or an integer. Default #(2 2 0).
:checkbox-parent-function
Controls the changes in the ancestors when
the status of an item is changed.
:checkbox-child-function
Controls the changes in the descendants
when the status of an item is changed.
:checkbox-change-callback
A function called when the status of an item
is changed interactively.
:checkbox-initial-status
Specifies the initial status of specific items.
Initargs for handling images:
:image-function
Returns an image for a node.
:state-image-function
Returns a state image for a node.975
:image-lists
A plist of keywords and image-list
objects.
:use-images Flag to specify whether items have images.
Defaults to t.
:use-state-images
Flag to specify whether items have state
images. Defaults to nil.
:image-width Defaults to 16.
:image-height Defaults to 16.
:state-image-width
Defaults to image-width.
:state-image-height
Defaults to image-height.
Accessors tree-view-roots
tree-view-children-function
tree-view-image-function
tree-view-state-image-function
tree-view-leaf-node-p-function
tree-view-retain-expanded-nodes
tree-view-expandp-function
tree-view-action-callback-expand-p
tree-view-right-click-extended-match
tree-view-has-root-line
tree-view-checkbox-next-map
tree-view-checkbox-parent-function
tree-view-checkbox-status
tree-view-checkbox-child-function
tree-view-checkbox-change-callback
tree-view-checkbox-initial-status
Readers tree-view-checkbox-status21 CAPI Reference Entries
976
Description The tree view pane allows the user to select between items
displayed in a hierarchical list. Although it is a choice, only
single selection interaction is supported. Use extendedselection-tree-view if you need other selection interaction styles.
The hierarchy of items in the tree is defined by the childrenfunction, which must be a function taking a single argument
and returning a list of children. When an item is expanded,
whether programmatically, automatically, or in response to a
user gesture, the system calculates what children this item
has by calling the children-function on it.
What children the children-function returns for an item is up
to you. However, the list must not include an object which is
cl:eql to another object in the tree. To work sensibly it also
needs to be consistent over time, that is return the same
objects each time it is called, unless the state of the entity that
the tree represents changes. It should also be reasonably fast,
as the user will be waiting to see the items.
If the tree is supposed to display items that are "the same" in
different parts of the tree, you can define a "wrapper", typically cl:defstruct with a few slots, and return a list of these
wrappers (each pointing to the actual object). This wrapping
is also useful for keeping other information related to the display in the tree, for example the string or the image to display, and maybe cache the children.
If leaf-node-p-function is not supplied, the children-function is
also used to decide whether unexpanded nodes are leaf
nodes or not (and hence whether to display the expanding
box). If the children-function is slow, this may slow significantly the display of large trees. If it is possible to check for
the existence of children faster, you should supply leaf-node-pfunction to avoid this slow down.
The default value of children-function is (constantly
false), that is no children, and hence only the roots are displayed.977
expandp-function controls automatic expansion of nodes
(items) in the tree-view. By default, initially only the ite|#



#|

;;CHILDREN-FUNCTION  FUNCTIONS FOR TESTING -------------------------- 
(defun children-function2 (root)
  (let
      ((base (* root 1))
       )
    (make-tree-numbers-list *my-tree)
    ))
  
(defun children-function1 (root)
 ;;x must be = each root plugged in 
  (when (< root 500)  ;;was 100,  limits node number (larger N means more depth levels).
       (let ((base (* root 2))) ;;next level node num begins at 1 + number
                                        ;; should be 1 + (length *tree-roots-list)
         (list (+ base 1) 
               (+ base 2)
               (+ base 3)
               (+ base 4)
               (+ base 5) ;; N children per EACH node (at each level)
               )))
   )

(defun children-function3 (root)
  "Only seems to expand to SAME NUM OF NODES FOR EACH SUBLEVEL"
 ;;x must be = each root plugged in 
  ;;(break)
  (when (< root 500)  ;;was 100,  limits node number (larger N means more depth levels).
       (let ((base (* root 2))) ;;next level node num begins at 1 + (* root number)
                                        ;; should be 1 + (length *tree-roots-list)
         (list (+ base 1) 
               (+ base 2)
               (+ base 3)
               (+ base 4)
               (+ base 5) ;; N sublists =  N children per EACH node (at EACH level)
               )))
   )

(defparameter *my-base 3)
(defparameter *my-n-nodes  2)

(defun children-function4 (root)
  (when (< root 500)  ;;was 100,  limits node number (larger N means more depth levels).
  (setf *my-base (+ 2)
      *my-n-nodes (+ *my-n-nodes 1))
       (let 
           ((base (* root 2)) ;;next level node num begins at 1 + number
            )   ;; should be 1 + (length *tree-roots-list)    
         (my-make-list *my-n-nodes :initial-element base :incf-by-n 3)
       ;;end let,when,chidren-function
       )))

(defun children-function5 (root)
  (when (< root 500)  ;;was 100,  limits node number (larger N means more depth levels).
  (setf *my-base (+ 2)
      *my-n-nodes (+ *my-n-nodes 1))
       (let 
           ((base (* root 2)) ;;next level node num begins at 1 + number
            )   ;; should be 1 + (length *tree-roots-list)    
         (list (- base 2) (+ base 2) 3)

       ;;end let,when,chidren-function
       )))


(defun children-function6 (root)
  (cond
   ((null root)
    NIL)
   ((< root 500)  ;;was 100,  limits node number (larger N means more depth levels).
         '(1 2 3 :leaf 5)))
       ;;end let,when,chidren-function
       )

(defun children-function8 (root)
  (when (< root 1000)  ;;was 100,  limits node number (larger N means more depth levels).
       (let 
           ((base (* root 10)) ;;next level node num begins at 1 + number
            )   ;; should be 1 + (length *tree-roots-list)    
         (my-make-list *my-n-nodes :initial-element base :incf-by-n 1)
       ;;end let,when,chidren-function
       )))

;;(setf *nth-rootlist 0)
(defun children-function9 (root)
  (when root  ;;was 100,  limits node number (larger N means more depth levels).
       (let*
           ((base (* root 10)) ;;next level node num begins at 1 + number
             ;; should be 1 + (length *tree-roots-list) 
             (rootlist (nth *nth-rootlist *rest-treeview-levels))
             )
         (setf *nth-rootlist (+ *nth-rootlist 1))
         rootlist
       ;;end let,when,chidren-function
       )))

(defparameter *nth-root-list 0)

(defun children-function7 (root)
  "DOES NOT EVAL THE INCF OR BREAK AFTER FIRST FIRING OF FUNCTION EVEN when I move them to within the let phrase."
  (when (< root 10000)  ;;was 100,  limits node number (larger N means more depth levels).
       (let 
           (;;(base (* root 10)) ;;next level node num begins at 1 + number
              ;; should be 1 + (length *tree-roots-list)  
              (root-lists *rest-treeview-levels)
              (nth *nth-root-list)
              ) 
         (setf *nth-root-list  (+ *nth-root-list 1))
         (nth  root  root-lists)
;;
         ;;(my-make-list *my-n-nodes :initial-element base :incf-by-n 1)
       ;;end let,when,chidren-function
       )))

|#


#|;; 
;;  '(a b (c (d (e f) g) h) i (j k))  => '(1 2 3 4 5)  '(21 22)  '(33 34 35)
;; works= ((1 (2 3 (4 5 (6 7 (8 9 10)) (11 12) 13 (14 (15 (16 17 (18 (19)) 20) 21 22) 23) 24 25) 26) 27 (28 29) 30))   27 
;; FOR ABOVE LIST, FOLLOWING 1. Adequately represents all possibilities, but 2. Does not allow id of node where number came from. (However tree will do that). 3. Does allow easier translation from root to nth to find the lists for each level.
;; ROOT-N LEVEL-N ITEM-N
;; 1 level 1 item 1 = 1
;; 2 level 1 item 2 = (2 3 (4 5 (6 7 (8 9 10)) (11 12) 13 (14 (15 (16 17 (18 (19)) 20) 21 22) 23) 24 25) 26)
;; 3 level 1 item 3 = 27
;; 4 level 1 item 4 = (28 29) 
;; 5 level 1 item 5 = 30
;;  2.1 level 2.2 item 1 = 2
;;  2.2 level 2.2 item 2 = 3
;; 2.3 level 2.2 item 3 = (4 5 (6 7 (8 9 10)) (11 12) 13 (14 (15 (16 17 (18 (19)) 20) 21 22) 23) 24 25)
;; 2.4 Level 2.2 item 4 = 26
;; 4.1  level 2.4 item 5 = 28
;; 4.2  level 2.4 item 6 = 29
;; 2.3.1 level 3.2.3 item 1 = (6 7 (8 9 10))
;; 2.3.2 level 3.2.3 item 2 = (11 12)
;; 2.3.3 level 3.2.3 item 3 = 13
;; 2.3.4 level 3.2.3 item 4 =  (14 (15 (16 17 (18 (19)) 20) 21 22) 23)
;; 2.3.5 level 3.2.3 item 5 = 24
;; 2.3.6 level 3.2.3 item 6 = 25
;; 2.3.1.1 level 4.3.2.3 item 1 = 6
;; 2.3.1.2 level 4.3.2.3 item 2 = 7
;; 2.3.1.3 level 4.3.2.3 item 3 = (8 9 10)
;; 2.3.2.1 level 4 item 4 = 11
;; 2.3.2.2 level 4 item 3 = 12
;; 2.3.4.1 level 4 item 4 = 14
;; 2.3.4.2 level 4 item 5 = (15 (16 17 (18 (19)) 20) 21 22)
;; 2.3.4.3 level 4 item 6 = 23
;; 2.3.1.3.1 level 5 item 1 = 8
;; 2.3.1.3.2 level 5 item 2 = 9
;; 2.3.1.3.3 level 5 item 3 = 10
;; 2.3.4.2.1 level 5 item 4 = 15
;; 2.3.4.2.2 level 5 item 5 = (16 17 (18 (19)) 20)
;; 2.3.4.2.3 level 5 item 6 =  21
;; 2.3.4.2.4 level 5 item 7 =  22
;; 2.3.4.2.2.1 level 6 item 1 = 16
;; 2.3.4.2.2.2 level 6 item 2 = 17
;; 2.3.4.2.2.3 level 6 item 3 = (18 (19))
;; 2.3.4.2.2.4 level 6 item 4 = 20
;; 2.3.4.2.2.3.1 level 7 item 1 = 18
;; 2.3.4.2.2.3.2 level 7 item 1 = (19)
;; 2.3.4.2.2.3.2.1  level 8 item 1 = 19
|#
