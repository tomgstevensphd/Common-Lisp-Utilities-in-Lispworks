;;***************************** U-Arrays.lisp ********************
;;
;;

(defparameter *print-detail  0 "Make = 2 for fout printing in this file " )

;;MY-AREF
;;
;;ddd
(defun my-aref (array dim-list)
  "In U-Arrays.lisp, looks up the value of an array with  indices in a LIST instead of a &rest set"
  (let
      ((value)
       )
    (setf value (apply #'aref  (append (list array) dim-list)))
    value))

(defun my-set-aref (array dim-list value)
  "In U-Arrays.lisp, sets the value of an array with  indices in a LIST instead of a &rest set"
    (setf  (apply #'aref  (append (list array) dim-list))  value)
    array)

;;tests--------------------------
#|(defun testmaf ()
  (let ((array (make-array '(2 3 5) :initial-element 0))
        (value)
        )
    (setf value (my-aref array '(0 2 4))))) ;;note these dim-elements MUST be LESS than (2 3 5)
|#
;; (testmaf)  works?   => 0

#|(defun testmsa ()
  (let ((array (make-array '(2 2)))
        (dim-list '(1 1))
        (value 6))
  ( my-set-aref array dim-list value)))
|#
;;works returns #2A((NIL NIL) (NIL 6))


;;my-gen-perm-lists (end-num)



;----- FUNCTIONS TO GET/SET/COPY DATA TO/FROM LISTS  AND ARRAYS -----------


;;COPY-FROM-ARRAY-TO-ARRAY
;;
;;ddd
(defun copy-from-array-to-array (from-array to-array &key array-startlist)
  "In U-arrays.lisp.  All dims in from-array MUST be smaller than to-array.  array-startlist gives indices in to-array where start copying the data from list."
  (let*
      ((from-list)
       (new-to-array)
       )
    (setf from-list (array-to-list from-array))
    (afout 'out (format nil "In copy-from-array-to-array 1, from-array= ~A~%from-list= ~A~%to-array= ~A~%" from-array from-list to-array))
    (setf new-to-array (copy-list-to-array from-list to-array :array-startlist array-startlist))
    (if (> *print-detail 1)(afout 'out (format nil "In copy-from-array-to-array 2, to-array= ~A~%"  new-to-array)))
    new-to-array
    ))

;;TEST
;;  (testcaa)
#|(defun testcaa ()
  (setf out nil)
  (declare (special *testout))
    (let*
        ((from-array (make-array '(3 4 3) :initial-contents  '(((1 2 3) (4 5 6) (7 8 9) (10 11 12)) ((A B C) (D E F) (G H I) (J K L)) ((M N O) (P Q R) (S T U) (W X Y)))))
         (to-array (make-array '(5 6 6) :initial-element 0))
         )
     (setf *testout (copy-from-array-to-array from-array to-array))
     (fout out)
     *testout
      ))|#
;;works WITH DIMS (3 4 3), RETURNS  #3A(((1 2 3) (4 5 6) (7 8 9) (10 11 12)) ((A B C) (D E F) (G H I) (J K L)) ((M N O) (P Q R) (S T U) (W X Y))) 
;;works WITH DIMS (5 6 6), RETURNS  #3A(((1 2 3 0 0 0) (4 5 6 0 0 0) (7 8 9 0 0 0) (10 11 12 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((A B C 0 0 0) (D E F 0 0 0) (G H I 0 0 0) (J K L 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((M N O 0 0 0) (P Q R 0 0 0) (S T U 0 0 0) (W X Y 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)))


;;LIST-TO-ARRAY
;;
;;ddd
(defun list-to-array (list dimlist)
  "In U-arrays.lisp. Uses (make-array dimlist :initial-contents list)."
   (make-array dimlist :initial-contents list))

;;COPY-LIST-TO-ARRAY
;;
;;ddd
(defun copy-list-to-array (list array &key array-startlist)
  "In U-arrays.lisp.  All dims MUST be smaller than array. array-startlist gives indices in array where start copying the data from list."
  (let*
      ((array-dimlist (array-dimensions array))
       (array-setlist)
       ;(num-dims (list-length array-dimlist))
      ;; (set-dimlist-length (list-length get-dimlist ))
      ;; (list-length (list-length list))
      (dim-n 0)
       (new-array)
       )
     (setf new-array (copy-list-to-array1 list array array-dimlist array-setlist dim-n
                                          :array-startlist array-startlist))
     ))

;;copy-list-to-array1 (see calling function)
;;
;;ddd
(defun copy-list-to-array1 (list array array-dimlist array-setlist  dim-n &key array-startlist)
 "doc here"
  (let*
      ((array-setlist-length (list-length array-setlist ))
       (list-length (list-length list))
       )
    (if (> *print-detail 1)(afout 'out  (format nil "dim-n= ~A" dim-n)))
    (loop
     for list-element in list
     for n from 0 to (- list-length 1)
     with element-list-length
     with array-setdims
     with array-setlist1
     with to-dim
     do
     (setf element-list-length (nth dim-n array-dimlist ))
           
     (cond
      ((>= element-list-length  list-length)
       (cond
        ;;if  item is a list, then must recurse until get non-list element
        ((listp list-element)
         (cond
          (array-startlist
           (setf array-setlist1 (append array-setlist (list  (+ n (nth dim-n array-startlist))))))
          (t (setf array-setlist1 (append array-setlist (list n)))))
         (if (> *print-detail 1)(afout 'out  (format nil "IN LOOP LISTP  n= ~A list-element= ~A~% array-dimlist= ~A~%array-setlist1= ~A~%"  n list-element array-dimlist array-setlist1)))

         ;;recurse on next dimension
         (copy-list-to-array1 list-element array array-dimlist array-setlist1  
                              (+ dim-n 1) :array-startlist array-startlist)
         )
        (t
         ;;since element isn't a list, store its value in array
         (cond
          (array-startlist
           (setf array-setlist1 (append array-setlist (list  (+ n (nth dim-n array-startlist))))))
          (t (setf array-setlist1 (append array-setlist (list n)))))
         (my-set-aref array array-setlist1 list-element)
         (if (> *print-detail 1)(afout 'out  (format nil "IN LOOP LISTP  n= ~A list-element= ~A~% array-dimlist= ~A~%array-setlist1= ~A~%"  n list-element array-dimlist array-setlist1)))         ;;end inner cond
         ))
       )
      (t (return "ARRAY DIMENSIONS SMALLER THAN LIST")))
     ;;end loop
     )
    array
    ))

;;TEST
#|(defun testcl ()
  (setf out nil)
  (declare (special *testout))
    (let*
        ((list '(((1 2 3) (4 5 6) (7 8 9) (10 11 12)) ((A B C) (D E F) (G H I) (J K L)) ((M N O) (P Q R) (S T U) (W X Y))))
         (array (make-array '(6 5 6) :initial-element 0))
         )
     (setf *testout (copy-list-to-array list array :array-startlist '(1 1 1)))
     (fout out)
     *testout
      ))|#
;;works WITH ARRAY DIMS (5 6 6) returns array of those dims with values= #3A(((1 2 3 0 0 0) (4 5 6 0 0 0) (7 8 9 0 0 0) (10 11 12 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((A B C 0 0 0) (D E F 0 0 0) (G H I 0 0 0) (J K L 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((M N O 0 0 0) (P Q R 0 0 0) (S T U 0 0 0) (W X Y 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)))
;;works WITH ARRAY DIMS (3 4 3) = SAME AS LIST returns array= #3A(((1 2 3) (4 5 6) (7 8 9) (10 11 12)) ((A B C) (D E F) (G H I) (J K L)) ((M N O) (P Q R) (S T U) (W X Y)))
;;works WITH ARRAY DIMS (5 6 6) and :array-startlist '(1 1 1) returns #3A(((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((0 0 0 0 0 0) (0 1 2 3 0 0) (0 4 5 6 0 0) (0 7 8 9 0 0) (0 10 11 12 0 0)) ((0 0 0 0 0 0) (0 A B C 0 0) (0 D E F 0 0) (0 G H I 0 0) (0 J K L 0 0)) ((0 0 0 0 0 0) (0 M N O 0 0) (0 P Q R 0 0) (0 S T U 0 0) (0 W X Y 0 0)) ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)))

;;ARRAY-TO-LIST
;;modified 2020
;;ddd
(defun array-to-list (array &optional get-dimlist  X)
  "In U-arrays.lisp. Gets all values from multi-dim array of any rank? and returns in list form. Outermost list is the first dim, innermost is last dim. &optional incl for Depreciated versions."
  (let*
      ((array-dimlist (array-dimensions array))
       (n-dims (list-length array-dimlist))
      (return-list)
       )
    (when (null get-dimlist)
      (setf get-dimlist array-dimlist))
    (cond
     ((= n-dims 1)
      (setf return-list
            (get-array-dim-values array '(2) 0)) ;; (- num-dims 1))  ;;(+ dim-n 1)
      )
     (T (setf return-list 
           (array-to-list1 array get-dimlist  0 return-list))))
     (values return-list array-dimlist)
     ))
;;TEST
;; (array-dimensions #((" 1MK  " " SS? " "   U-BUFFER-TEST-1.lisp       " "  my    notes   here   " "CogSys-Model" "(defun get-nth-in-all-2nested-lists ~% (nth list-of-1nested-lists)  In U-lists RETURNS (values nths-1nested-lists non-nth-items). ~%NTHS-1NESTED-LISTS list of all items at nth place~% for each list in each group of 1nested lists") ("  2MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  3MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  4MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ")))
;;note tested above with arrayp = T array-dimensions = 4
;;



;;ARRAY-TO-LIST1  (SEE CALLING FUNCTION ABOVE)
;;
;;ddd
(defun array-to-list1  (array get-dimlist dim-n return-list)
  "In U-arrays.lisp. A sub-recursive function of array-to-lisp (array)."
  (let*
      ((array-dimlist (array-dimensions array))
       (num-dims (list-length array-dimlist))
       (element-list-length (nth dim-n array-dimlist ))
       (get-dimlist-length (cond ((null get-dimlist) 1)
                                 (T (list-length get-dimlist ))))
       (new-return-list)
       )
    (break "1")
   ;;(if (> *print-detail 1)(afout 'out  (format nil "STARTING array-to-list1,get-dimlist= ~A~% dim-n= ~A~%return-list= ~A~%" get-dimlist dim-n return-list )))
    (loop
     for n from 0 to (- element-list-length 1)  ;; 1) ;; left out last one (- element-list-length 1)
     do
     (let*
         ((new-get-dimlist)
          )
     (cond
      ((= get-dimlist-length (- num-dims 2)) 
       (setf new-get-dimlist (append get-dimlist (list  n  0))  
             new-return-list (append new-return-list
                                          (list (get-array-dim-values array new-get-dimlist (- num-dims 1)))))
   ;;(if (> *print-detail 1)(afout 'out  (format nil "END OF DIMS array-to-list1,get-dimlist= ~A~% dim-n= ~A~%return-list= ~A~% new-return-list= ~A~% " get-dimlist dim-n return-list new-return-list )))
        )
       ((and ( < get-dimlist-length (- num-dims  2))(<  n element-list-length)) ;;was - 1
         (setf  new-get-dimlist (append get-dimlist (list  n))  
              ;;  dim-n (list-length new-get-dimlist) ;;  (+ dim-n 1) 
                new-return-list
                (append new-return-list (list  (array-to-list1 array new-get-dimlist (+ dim-n 1) new-return-list))))
        ;;(if (> *print-detail 1)(afout 'out  (format nil "RECURSIVE-CALL array-to-list1,get-dimlist= ~A~% dim-n= ~A~%return-list= ~A~% new-return-list= ~A~%  " get-dimlist dim-n return-list new-return-list )))
         )
       ;;(and ( < get-dimlist-length (- num-dims  2))(<  n element-list-length))
#|       ((and (< num-dims 2)  (<  n element-list-length))
        )|#
       (t  NIL ))
     (break "2")
     ;;end let,loop
     ))
    new-return-list))


;;TEST
#|(defun testcad ()
  (setf out nil)
  (let* ((from-array (make-array '(3 4 3)
                                 :initial-contents `(((1 2 3)(4 5 6)(7 8 9)(10 11 12))((A B C)(D E F)(G H I)(J K L))((m n o)(p q r)(s t u)(w x y)))))
         )
    (setf  *values-list-of-lists (array-to-list from-array))
    (fout out)
    *values-list-of-lists
    ))|#
;;TEST                                       indices     1,0       1,1         1,2       1,3
;;  for only dims 1 and 2, works  returns((1 A I) (4 D L) (7 G O) (10 J R))
;; works returns (((1 2 3) (4 5 6) (7 8 9) (10 11 12)) ((A B C) (D E F) (G H I) (J K L)) ((M N O) (P Q R) (S T U) (W X Y)))



;;GET-ARRAY-DIM-VALUES (used with above functions or stand-alone)
;;
;;ddd
(defun get-array-dim-values (array get-dimlist dim-n)
  "In U-arrays.lisp, Gets the values of a specified array dimension. ARGs: get-dimlist is the list specifying the indices of the lookup dimlist--except the dim-n dim is varied from element 0 to its max-length. "
  (let*
      ((values-list)
       (array-dimlist (array-dimensions array))  ;;num elements for each dim
       (num-dims (list-length array-dimlist))  ;;num dims
       (element-list-length (nth dim-n array-dimlist )) ;;num elements for target dim
       ;;not needed (get-n (nth dim-n get-dimlist))
       (new-dimlist)
       )
    (if (> *print-detail 1)(afout 'out  (format nil "FINDING VALUES get-array-dim-values,get-dimlist= ~A~% dim-n= ~A~%*values-list-of-lists= ~A~% " get-dimlist dim-n *values-list-of-lists)))   
    (loop
     for n from 0 to (- element-list-length 1)
     with value
     do
     (setf ;;;New-Dimlist (mymakenlonglist Num-Dims :default-value 0)
           new-dimlist (replace-list get-dimlist dim-n n));;; New-Dimlist))
     (setf value (my-aref array new-dimlist )
           values-list (append values-list (list value)))
     )
    (values values-list)
    ;;end let, get-array-dim-values
    ))
;;TEST
;; (setf **test-arr (make-array '(3 4 3)  :initial-contents `(((1 2 3)(4 5 6)(7 8 9)(10 11 12))((A B C)(D E F)(G H I)(J K L))((i j k)(l m n)(o p q)(r s t)))))
;; (array-dimensions **test-arr) = (3 4 3)
;;   (get-array-dim-values **test-arr '(1 2 0)  2)   = (G H I)

;;note It doesn't matter what the get-dimlist value for the dim-n item is!! It returns ALL values for that item
;; get-dimlist (0 0 0), dim-n 0 works returns (1 A I)  
;;  get-dimlist (0 0 0), dim-n 1 works returns (1 4 7 10)
;;  get-dimlist (0 0 0), dim-n 2 works returns (1 2 3)
;;  get-dimlist (1 0 0), dim-n 1 works returns (A D G J)
;;  get-dimlist (2 0 0), dim-n 1 works returns (I L O R)
;;  get-dimlist (1 0 0), dim-n 2 works returns (A B C)
;;  get-dimlist (1 2 0), dim-n 2 works returns (G H I)


 
;; --------------------------------- END GET/SET/COPY  TO/FROM ARRAYS LISTS -----------



;;TEST-RECURSIVE-RETURN
;;
;;ddd
(defun test-recursive-return (return-value cycle-n)
  "In U-arrays.lisp.  A model for setting up a recursive function.  Could be used to make a macro that applies a function?"
  (incf cycle-n)
  (let*
      ((new-return-value)
        (letters '(A B C D E F G H I J K L))
        (letter  (nth cycle-n letters))
       )
  (if (> *print-detail 1)(afout 'out  (format nil "1-return-value= ~A cycle-n= ~A~% " return-value cycle-n)))
  (cond
   ((= cycle-n 5)
   (setf new-return-value  (append return-value (list LETTER "RETURN-VALUE" cycle-n)))
    (if (> *print-detail 1)(afout 'out  (format nil "3-new-return-value= ~A cycle-n= ~A~% " new-return-value cycle-n))))
   ((< cycle-n 5)
    (setf  new-return-value (append return-value (list LETTER (test-recursive-return new-return-value cycle-n) cycle-n)))
        (if (> *print-detail 1)(afout 'out  (format nil "2-new-return-value= ~A cycle-n= ~A~% " new-return-value cycle-n))))
  (t (cerror "ERROR IN TEST-RECURSIVE-RETURN")))
  new-return-value
  ))

;;TEST
#|
(defun testrv ()
  (let ((rv)
        )
  (setf out nil)
  (setf rv (test-recursive-return  NIL -1))
  (fout out)
  rv
  ))
 |#    



;;---------------------------------- TESTING ------------------------------------
#|
6 Current Project NOTES
(10 (pattern. .)) num-cycles = num-cycles + 10
Track all resets, print only resets option.

For dims 4 2 4, num-dims 3
0,,  0.  0123
      1.  0123

1,,   0.  0123
       1.  0123

2,,   0   0123
       1.  0123

3,,   0.  0123
       1,  0123
4 x 2 x 4 = 32 ok

3 x3 x 4 = 36 ok

(Cond
( dims-remaining-list
(multiple-value-setq (new-to-array cur-dims-list dims-remaining-list ...)
(Copy-array-values from-array to-array cur-dims-list dims-remaining-list ...))
(t. Use the copy dim function here.

Consider a macro that walks a list and applies a function. 
|#
;;RESULT OF BELOW
#|#3A(((1 0 0) (0 0 0) (0 0 0) (0 0 0)) ((A 0 0) (0 0 0) (0 0 0) (0 0 0)) ((I 0 0) (0 0 0) (0 0 0) (0 0 0)))
#3A(((1 2 3) (4 5 6) (7 8 9) (10 11 12)) ((A B C) (D E F) (G H I) (J K L)) ((I J K) (L M N) (O P Q) (R S T)))|#

;;mmm
;;(testcadv)
#|(defun testcadv ()
  (setf out nil)
  (let ((from-array (make-array '(3 4 3)
              :initial-contents `(((1 2 3)(4 5 6)(7 8 9)(10 11 12))((A B C)(D E F)(G H I)(J K L))((i j k)(l m n)(o p q)(r s t)))))
        (to-array (make-array  '(3 4 3) :initial-element 0))
        (from-dim-list '(0 0 0))
        (to-dim-list '(0 0 0))
        (from-dim-n 1)
        (to-dim-n 1) 
        )
    (copy-array-dim-values from-array to-array from-dim-list to-dim-list 
                           from-dim-n to-dim-n) 
        ))|#
#|
;;testcav1            0,0      0,1       0,2        0,3                L1 L2 use dim-funct
;;from-array '(((1 2 3) (4 5 6) (7 8 9) (10 11 12)) ;0  (0 0-2 0-3)
                     ((A B C) (D E F) (G H I) (J K L))   ;1  (1 0-2 0-3)  note: DEF is 1,1; E is 1,1,1;
                     ((I J K) (L M N) (O P Q) (R S T))) ;2  (2 0-2 0-3)
Number of values 3 x 3 x 4 = 36 
 (0-2 0 0)(0-2 1 0)(0-2 2 0)
 (0-2 0 1)(0-2 0 2)(0-2 0 3)
 (0-2 1 1)(0-2 1 2)(0-2 1 3)
 (0-2 2 1)(0-2 2 2)(0-2 2 3)
|#
;;works (from-dim-n 0)(to-dim-n 0)  returns A(((1 0 0) (0 0 0) (0 0 0) (0 0 0)) ((A 0 0) (0 0 0) (0 0 0) (0 0 0)) ((I 0 0) (0 0 0) (0 0 0) (0 0 0)))
;;works (from-dim-n 2)(to-dim-n 2) returns (((1 2 3) (0 0 0) (0 0 0) (0 0 0)) ((0 0 0) (0 0 0) (0 0 0) (0 0 0)) ((0 0 0) (0 0 0) (0 0 0) (0 0 0)))
;;works  (from-dim-n 1) (to-dim-n 1);
#| new-from-dim-list= (0 0 0)  new-to-dim-list= (0 0 0) value= 1
  new-from-dim-list= (0 1 0)  new-to-dim-list= (0 1 0) value= 4
 new-from-dim-list= (0 2 0)  new-to-dim-list= (0 2 0) value= 7
 new-from-dim-list= (0 3 0)  new-to-dim-list= (0 3 0) value= 10
 returns   (((1 0 0) (4 0 0) (7 0 0) (10 0 0)) ((0 0 0) (0 0 0) (0 0 0) (0 0 0)) ((0 0 0) (0 0 0) (0 0 0) (0 0 0)))|#
;;test
;;  (testcav)
#|(defun testcav ()
  (setf out nil)
  (let ((from-array (make-array '(3 4) :initial-contents '((a b c d) (1 2 3 4) (x y z w)))) ;; :initial-element 1))
        (to-array (make-array  '(3 5) :initial-contents  '((a b c d 0) (1 2 3 4 0) (x y z w 0))))
        (from-dim-list '(0 0))
        (to-dim-list '(0 0))
        (from-dim-n 0)
        (to-dim-n 0) 
        )
    (copy-array-dim-values from-array to-array from-dim-list to-dim-list 
                           from-dim-n to-dim-n) 
        ))|#
;;above works returns
#|CL-USER 13 > (testcav)
#2A((A B C D 0) (1 2 3 4 0) (X Y Z W 0))
#2A((A B C D) (1 2 3 4) (X Y Z W))|#
;;
#|(defun testcav0 ()
  (setf out nil)
  (let ((from-array (make-array '(3 4) :initial-contents '((a b c d) (1 2 3 4) (x y z w)))) ;; :initial-element 1))
        (to-array (make-array '(3 5) :initial-element 0))
        (from-dim-list '(1 1))
        (to-dim-list '(2 2))
        (from-dim-n 0)
        (to-dim-n 1) 
        )
    (copy-array-dim-values from-array to-array from-dim-list to-dim-list 
                           from-dim-n to-dim-n) 
        ))
;;works returnts #2A((0 0 0 0 0) (0 0 0 0 0) (B 2 Y 0 0))
;;#2A((A B C D) (1 2 3 4) (X Y Z W))
  |#
;;--------------------------------------------------- END TESTING --------------------------------------------   


;;ARRAY-TO-LIST
;;ddd
(defun array-to-list2 (array)
  "In U-Arrays.lisp, makes a one-dim array into a list"
  (let
      ((list)
       (element)
       )
    (loop 
     for n from 0 to (- (car (array-dimensions array)) 1)
     do
     (setf element (aref  array n))
     ;;(afout 'out (format nil "element= ~A~%" element))
      (setf list (append list (list element))))
       list))
;;
;;following works
#|(defun testrl ()
  (setf out nil)
  (let 
      ((array (make-array 4 :adjustable t   :initial-contents '(1 2 4 5)))
       (new-list)
       )
    (setf new-list (array-to-list array))
    (afout 'out (format nil "array= ~A new-list= ~A~%" array new-list))))|#

;;example
#| (make-array '(4 2 3) :initial-contents
             '(((a b c) (1 2 3))
              ((d e f) (3 1 2))
              ((g h i) (2 3 1))
              ((j k l) (0 0 0))))|#


;;MAKE-ARRAY-SYMBOL
;;
;;ddd -- works, see below
(defun make-array-symbol (array prefix field cell &optional dim-list)
  "In ART2.lisp, sets a string from prefix, field, cell and opt dim-list to an array so can generate symbols to represent the arrays"
  (let 
      ((new-string (format nil "~A~A-~A" prefix field cell))
       (new-sym)
       )
    (if dim-list
        (dolist (item dim-list)
          (setf new-string  (format nil "~A-~A" new-string item))))
    (setf new-sym (my-make-symbol new-string))
    (if (arrayp array)
        (set new-sym array))
    (values new-sym array)))

;;following works--makes new string and sets it's symbol to an array
#|(defun testmas ()
  (setf out nil)
  (let
      ((x)
       (y)
       (array1 (make-array '(2)))
       )
  (multiple-value-setq (x y) (make-array-symbol array1 "Input" "F1" 3))
 ;;also works (multiple-value-setq (x y) (make-array-symbol array1 "Input" "F1" 3 '(4 5 6)))
 ;;   (afout 'out (format nil "x= ~A INPUTF1-3= ~A y= ~A~%" x  INPUTF1-3 y))
    ))|#
    
(defun make-arrays (symbol-list dimensions-list
                                &key initial-element initial-contents-list prefix)
  "In U-Arrays.lisp, sets symbols in symbol-list to new arrays of dimensions in a sublist from dimensions-list. default-contents is for all arrays. Sets GLOBAL variable of each symbol in symbol-list to an array."
  (let
      ((num-symbols (length symbol-list))
       (new-symbol-list)
       (array)
       (array-list)
       )
    (loop
     for symbol in symbol-list
     for n from 0 to num-symbols
     with new-symbol
     with new-array
     with dims = (nth n dimensions-list)
     with initial-contents

     do
     (if prefix
         (setf new-symbol (my-make-symbol (format nil "~A~A" prefix symbol)))
       (setf new-symbol symbol))

     (if initial-contents-list 
         (setf initial-contents (nth n initial-contents-list)))


     ;;(intern (format nil "~A" symbol))
     (setf new-array (make-array dims :initial-element initial-element
                                 :initial-contents initial-contents))
     (set new-symbol new-array)

     (afout 'out (format nil "new-array= ~A~% symbol= ~A array1=~A~% symbol-value symbol= ~A~%"  new-array new-symbol array1 (symbol-value new-symbol)))

     (setf new-symbol-list (append new-symbol-list (list new-symbol))
           array-list (append array-list (list (eval new-symbol))))

     ;;end loop
     )
    (values symbol-list new-symbol-list array-list)))


;;was
#|(defun make-arrays (symbol-list dimensions-list
                                &key initial-element initial-contents-list)
  "In U-Arrays.lisp, sets symbols in symbol-list to new arrays of dimensions in a sublist from dimensions-list. default-contents is for all arrays."
  (let
      ((num-symbols (length symbol-list))
       (array-list)
       )
    (loop
     for symbol in symbol-list
     for n from 0 to num-symbols
     with new-array
     with dims = (nth n dimensions-list)
     with initial-contents

     do
     (if initial-contents-list 
       (setf initial-contents (nth n initial-contents-list)))

     (setf symbol (make-array dims :initial-element initial-element
                              :initial-contents initial-contents)
           array-list (append array-list (list (eval symbol))))
     ;;end loop
     )
    (values symbol-list array-list)))|#

#|(defun testma ()
  (let
      ((symbol-list '(array1 array2))
       (dim-list '((5)(5 2)))
       )
  ;;  (make-arrays symbol-list dim-list)
 ;; (make-arrays symbol-list  dim-list :initial-contents-list '((3 5 2 6 7) (( 1 2)(3 4)(5 6)(7 8) (9 10))))
 (make-arrays symbol-list dim-list :initial-element 0 :prefix "*")
    ))|#

;;works returns=>
#|(ARRAY1 ARRAY2)
(#(NIL NIL NIL NIL NIL) #(NIL NIL NIL NIL NIL))|#
;;with initial-contents returns=>
#|(ARRAY1 ARRAY2)
(#(3 5 2 6 7) #((1 2) (3 4) (5 6) (7 8) (9 10)))|#
;;with initial-element and :prefix "*" returns=>
#|(ARRAY1 ARRAY2)
(*ARRAY1 *ARRAY2)
(#(0 0 0 0 0) #(0 0 0 0 0))
|#
           
     
    
