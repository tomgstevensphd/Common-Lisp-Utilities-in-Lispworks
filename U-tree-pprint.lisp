;;************************** U-tree-pprint.lisp ********************************


#|  From LispHug
------ Original message------
From: Roy Anderson 
Date: Mon, Mar 14, 2016 7:12 PM
To: lisp-hug@lispworks.com;
Cc: 
Subject:Re: Symbolic expression printing (Lisp code)
Roy Anderson <reanz1959 <at> gmail.com> writes:
|#

;;(in-package :TORONTO)

#|

I have fixed a couple of serious bugs I hadn't caught in the original post.

PRINTING SYMBOLIC-EXPRESSIONS (lisp trees).

Symbolic-expressions[https://en.wikipedia.org/wiki/S-expression], sexprs or 
sexps (for "symbolic expressions")
are a notation for nested lists (tree-structured) data, invented for and 
popularized by the programming language
Lisp, which uses them for source code as well as data. 

In the usual parenthesized syntax of Lisp, an s-expression is classically 
defined recursively as:

1.an atom, or

2.an expression of the form (x . y) where x and y are s-expressions.


I spent quite a while googling "PRETTY PRINTING LISP TREES" but I failed to 
find anything even remotely like 
the code I wanted. So, rather reluctantly, I decided to cut the code fresh. 
My goal is to print any symbolic 
expression including natural language parse trees with robust code to 
traverse down the source expression 
and consistently print connections between nodes using Unicode line drawing 
glyphs.

Their are a few test expressions at the end of this file. My favourite is 
the defun f and its reduced derivatives: g and h.

To test this code enter (p g) and you should see a graph appear in your 
Listener.

I am interested if you find a Lisp expression that doesn't print correctly 
or any other anomalous output.

The key entry points are print-expression for general symbolic expressions, 
and print-association-list.

|#
;;
;; Author: Roy Anderson, reanz1959@gmail.com
;;
;; This code was developed using LispWorks Personal Edition 6.1.1

(export `(print-expression print-tree-indented print-association-list 
chart))

;; define line drawing characters...
(defconstant *horizontal-line* (code-char #x2500))
(defconstant *vertical-line* (code-char #x2502))
(defconstant *top-corner* (code-char #x250c))
(defconstant *bottom-corner* (code-char #x2514))
(defconstant *t-intersection* (code-char #x251c))
(defconstant *circle* (code-char #x25cf)) ;; solid black circle
(defconstant *t-junction* (code-char #x252c))

(defun print-line-glyphs ()
  (dolist (ch '(*horizontal-line*
                *vertical-line*
                *top-corner*
                *bottom-corner*
                *t-intersection*
                *circle*
                *t-junction*))
    (format t "~a ~a~%" ch (symbol-value ch))))

;; S-expressions are rewritten with nodes to accomodate NIL valued leaves and duplicate node values.
(defclass node ()
  ((id :initarg :id :accessor node-id)
   ;;(indices :accessor node-indices :documentation "node coordinates")
   (joints :initform "" :accessor node-joints)
   (parent :initform nil :accessor node-parent)
   (children :initform nil :accessor node-children)
   (youngest :initform nil :accessor youngest)
   (value :initform nil :initarg :value :accessor node-value)))

(defclass symbolic-expression ()
  ((counter :initform 0 :accessor counter)
   (source :accessor source)
   (nodes :accessor nodes)
   (tree :initarg :tree :accessor tree)))

(defmethod root ((sexpr symbolic-expression))
  (first (nodes sexpr)))

(defmethod render-nodes ((s symbolic-expression))
  (dolist (x (nodes s))
    (node-joint s x)))

(defun node-list (source)
  (let ((nodes ()))
    (mapleaves source #'(lambda(n) (push n nodes)))
    (nreverse nodes)))

(defmethod initialize-instance :after ((sexpr symbolic-expression) &key)
  (setf (source sexpr) (make-nodes sexpr (copy-tree (tree sexpr))))
  (families sexpr)
  (setf (nodes sexpr) (node-list (source sexpr)))
  (render-nodes sexpr))

(defmethod depth ((node node))
  (length (ancestors node)))

(defmethod reference ((node node))
  (format nil "~d. ~a" (node-id node) (node-value node)))

(defmethod print-object ((node node) stream)
  (format stream "~d. " (node-id node))
  (format stream "~s" (node-value node))
  ;;(format stream "/~{~(~a~) /~}" (mapcar #'node-id (ancestors node)))
  (format stream " >>~{ ~a~}" (mapcar #'node-id (node-children node)))
  (terpri stream))

(defun make-node (&rest args)
  (apply 'make-instance 'node args))

(defun node-p (x) (typep x 'node))

(defmethod make-nodes ((sexpr symbolic-expression) tree)
  "Replace all atoms with a node so that we can print NIL values"
  (when (and tree (listp tree))
    (cond
     ((consp (car tree))
      (make-nodes sexpr (car tree)))
     (t
      (setf (car tree) (make-instance 'node :id (incf (counter 
sexpr)) :value (car tree)))))
    (make-nodes sexpr (cdr tree)))
  tree)

(defun head (tree)
  ;; Return the NODE at the head of TREE
  (if (node-p tree)
      tree
    (if (and tree (listp tree))
        (head (car tree)))))

(defun maptrees (tree callback)
  "Funcall CALLBACK passing each subtree (including sybolic expressions)"
  (when tree
    (funcall callback tree)
    (when (consp (car tree))
      (maptrees (car tree) callback))
    (maptrees (cdr tree) callback)))

(defun mapleaves (tree callback)
  "Perform a breadth first traversal of TREE calling CALLBACK with each 
NODE"
  (when (and tree (listp tree))
    (let ((subtree (car tree)))
      (cond
       ((node-p subtree)
        (funcall callback subtree))
       ((consp subtree)
        (mapleaves subtree callback))))
    (mapleaves (cdr tree) callback)))

(defmethod crawl-print ((sexpr symbolic-expression))
  (mapleaves (source sexpr) #'(lambda(x) (format t "cawler node ~a" x))))

(defmethod adopt ((h node) (c node))
  (unless (node-parent c)
    ;;(setf (node-children h) (append (node-children h) (list c)))
    (push c (node-children h))
    (setf (node-parent c) h)))

(defmethod families ((sexpr symbolic-expression))
  (maptrees
   (source sexpr)
   #'(lambda(x)
       (let ((h (head x)))
         (dolist (node (cdr x))
           (adopt h (head node)))))))

(defun concat (&rest args)
  (format nil "~{~a~}" args))

(defmethod joints-complete ((node node))
  (>= (length (node-joints node))
      (* 2 (depth node))))

(defmethod join ((node node) &rest args)
  ;;(if (joints-complete node) (break "join Overflow ~a" node))
  ;(unless (joints-complete node)
  (setf (node-joints node) (apply #'concat (or (node-joints node) "") 
args)))

(defmethod below ((node1 node) (node2 node))
  "Return T if NODE1 is lower in the symbolic expression than NODE2"
  (> (node-id node1) (node-id node2)))

(defmethod last-child ((node node))
  "Return the youngest/lowest NODE amongst the children of NODE"
  (or
   (youngest node)
   (let* ((children (node-children node))
          (youngest (car children)))
     (dolist (c (cdr children))
       (if (> (node-id c) (node-id youngest))
           (setf youngest c)))
     (setf (youngest node) youngest))))
#|

    (car children)))


    ;;(format t "last-child node ~a children ~a youngest ~a~%" (reference 
node) (mapcar #'reference children) (reference youngest))

|#

(defmethod last-child-p ((node node) parent)
  "Return T if child is the last node in the direct children of NODE"
  (and (eq (node-parent node) parent)
       (eq node (last-child parent))))

(defmethod ancestors ((node node))
  "Return a list of parent nodes closest to the root first"
  (let ((parent (node-parent node)))
    (if parent
        (append (ancestors parent) (list parent)))))

(defun join-branches (x list)
  (let ((last (last-child (car list))))
    (cond
     ((null (cdr list))
      (if (eq last x)
          (join x *bottom-corner* *horizontal-line*)
        (join x *t-intersection* *horizontal-line*)))
     ((below x last)
      (join x #\space #\space))
     (t
      (join x *vertical-line* #\space)))))

(defmethod node-joint ((s symbolic-expression) (x node))
  ;;(format t "node-joint ~s parent: ~a/~{~(~s ~)~}~%" (node-value x) (and parent (node-value parent)) (mapcar #'node-value (ancestors x)))
  (cond
   ((null (node-parent x)) ; X is the root NODE
    (join x *top-corner* *horizontal-line*))
   ((eq (root s) (node-parent x)) ; X is a child of the root NODE
    (if (last-child-p x (node-parent x))
        (join x *bottom-corner* *horizontal-line*)
      (join x *t-intersection* *horizontal-line*)))
   (t
    (maplist #'(lambda(list) (join-branches x list)) (ancestors x)))))

(defun print-expression (tree &optional (stream *standard-output*) &key 
verbose)
  ;; recursively print tree with indentation
  (let ((sexpr (make-instance 'symbolic-expression :tree tree)))
    (dolist (node (nodes sexpr))
      (cond 
       (verbose
        (format stream "~a~a ~d. ~s" (node-joints node) *circle* (node-id 
node) (node-value node))
        (let ((anc (ancestors node)))
          (if anc (format stream " / ~{~a ~}"  (mapcar #'node-id anc))))
        (let ((cn (node-children node)))
          (if cn (format stream " >> ~{~a ~}" (mapcar #'node-id cn)))))
       (t
        (format stream "~a~a ~s" (node-joints node) *circle* (node-value 
node))))
      (terpri stream)))
  (values))

(defun print-tree-with-indentation (tree stream)
  ;; recursively print tree with indentation
  (typecase tree
    (cons
     (mapc #'(lambda(x) (print-tree-with-indentation x stream)) tree))
    (node
     (format stream "~v@a~a" (* 2 (depth tree)) "" tree))))

(defun print-tree-indented (tree &optional (stream *standard-output*))
  ;; entry point for printing a tree with indentation for leaves
  (let ((sexpr (make-instance 'symbolic-expression :tree tree)))
    (print-tree-with-indentation (source sexpr) stream))
  (values))

(defun print-association-list (alist &optional (stream *standard-output*))
  "Print an association list with alist keys padded to maximum key width."
  (let ((width (apply #'max (mapcar #'(lambda(s) (length (string (car s)))) 
alist))))
    (mapcar
     #'(lambda (expr) (format stream "~va~{ ~(~a~)~}~%" width (pop expr) 
(if (atom expr) (list expr) expr)))
     alist))
  (values))

;; what have got to work with in our pallette of graphics?
(defun chart (&optional (page #x2500))
  (format t "~5a" "")
  (dotimes (i 16) (format t "~2x" i))
  (terpri)
  (dotimes (i 16)
    (format t "~4x " (+ page (* 16 i)))
    (dotimes (j 16)
      (format t " ~a" (code-char (+ page (* i 16) j))))
    (terpri)))

;; test data
(defparameter treefern '(a a2 (b r) a3 (c (d e))))
(defparameter *tree1* '(3 (2 (1 a b) c) (1 x y) (5 50 51)))
(defparameter *tree2* `(40
                        (30 
                         25
                         nil
                         nil)
                        (35
                         (nil
                          nil))
                        (60
                         50
                         nil)
                        nil))

(defparameter *tree3* `(40
                        (30
                         (25 () ()))
                        (3 () ())
                        (60 (50) t () ())))

(defvar *association-list* '((a 1) (b 2) (c 3 4)))

(defvar *tree* '(s (subj (np (mods angry grey) (head (word thief))))
                   (vp (aux do have) (v won))))

(defparameter f '(DEFUN AP-C.AP (STRING STACK REGS PATH)
  "DEFARC AP-C.AP"
  (FORMAT T "~s ~s~%" 'AP-C.AP '*DONE-IF)
  (LET ()
    (WHEN (TRACK-ARC-TEST (^ > AP))
      (UNLESS (OR STRING STACK) (RETURN-FROM AP-C.AP (RECORD-PARSE PARSE0 
(APPEND `(,(IF (EQ 'AJ (^ > TYPE)) 'AJP 'AVP)) (^ ? AP)) STRING)))
      (WHEN STACK
        (LET ((! (APPEND `(,(IF (EQ 'AJ (^ > TYPE)) 'AJP 'AVP))
                         (^ ? AP))) (PS (POP STACK))) (RECORD-PARSE (THIRD 
PS) ! STRING) (FUNCALL (POP PS) ! STRING STACK (CAR PS) PATH)))))))

(defparameter i '(DEFUN AP-C.AP (STRING STACK REGS PATH)
  "DEFARC AP-C.AP"
  (FORMAT T "~s ~s~%" 'AP-C.AP '*DONE-IF)
  (LET ()
    (WHEN (TRACK-ARC-TEST (^ > AP))
      (UNLESS (OR STRING STACK) (RETURN-FROM AP-C.AP (RECORD-PARSE PARSE0 
(APPEND `(,(IF (EQ 'AJ (^ > TYPE)) 'AJP 'AVP)) (^ ? AP)) STRING)))
      (WHEN STACK
        (LET ((! (APPEND `(,(IF (EQ 'AJ (^ > TYPE)) 'AJP 'AVP))
                         (^ ? AP))) (PS (POP STACK)))
          (RECORD-PARSE (THIRD PS) ! STRING)))))))


(defparameter h '(DEFUN AP-C.AP (STRING STACK REGS PATH)
  "DEFARC AP-C.AP"
  (FORMAT T "~s ~s~%" 'AP-C.AP '*DONE-IF)
  (LET ()
    (WHEN (TRACK-ARC-TEST (^ > AP))
      (UNLESS (OR STRING STACK) (RETURN-FROM AP-C.AP (RECORD-PARSE PARSE0 
(APPEND `(,(IF (EQ 'AJ (^ > TYPE)) 'AJP 'AVP)) (^ ? AP)) STRING)))
      (WHEN STACK
        (LET ((! (APPEND `(,(IF (EQ 'AJ (^ > TYPE)) 'AJP 'AVP))
                         (^ ? AP))))))))))

(defparameter g
  '(DEFUN
          AP-C.AP
          (STRING
           STACK
           REGS
           PATH)
     "DEFARC AP-C.AP"
     (FORMAT
      T
      "~s ~s~%"
      'AP-C.AP
      '*DONE-IF)
     (WHEN
         STACK
       nil)))

(defun p (x) (print-expression x))

(defconstant alist '((a 1) (b 2) (c 3) (d e f g) (h 0) (i nil)))

#|
(crawl-print (make-instance 'symbolic-expression :tree g))
(crawl-print (make-instance 'symbolic-expression :tree h))
|#

#|_______________________________________________
Lisp Hug - the mailing list for LispWorks users
lisp-hug@lispworks.com
http://www.lispworks.com/support/lisp-hug.html
|#