;;************************** U-lists.lisp ****************************
;;= n
;;SSS NOTE:  INSERT VALUE EITHER 1.AFTER INNER KEY, 2. NTH IN LIST, OR 3. APPEND VALUE TO LIST CONTAINING KEY=FIRST
;;(KEY  XXX :KEY VALUE  ...) OR (KEY .. NTH= VALUE..) OR (KEY .. (APPEND VALUE)
;;
;; ** ALSO SEE  U-symbol-trees.lisp FOR LIST TREE CREATING AND MODIFYING FUNCTIONS-- uses ART conventions, but generalizable
;;



;;SOME UTILITY FUNCTIONS BY PAUL GRAHAM p45ff
;;from Figure 4.1: Small functions which operate on lists.
;;(proclaim ’(inline last1 single append1 conc1 mklist))

;;LAST1
;;2019 from PGraham
;;ddd
(defun last1 (lst)
  "U-lists RETURNS last item in a list"
  (car (last lst)))
;;TEST
;; (last1 '(a b c)) = C

;; 1-ITEM-LIST-P
;;2019 from PGraham's single func
;;ddd
(defun 1-item-list-p (lst)
  "U-lists, tests whether something is a list of one element"
  (and (consp lst) (not (cdr lst))))
;;TEST
;; (1-item-list-p '(a b c)) = NIL


;;APPEND1
;;2019 from PGraham
;;ddd
(defun append1 (lst obj)
  "U-lists, appends one item to a list w/o adding (list item)"
  (append lst (list obj)))
;;TEST
;; (append1 '(a b) 'c)  = (A B C)


;;MY-APPEND1
(defun my-append1 (list obj)
  "U-lists, appends one item to a list [NOT= (NIL etc)] w/o adding (list item)"
  (cond
   ((not (null-list-p list))
    (append list (list obj)))
   (T (setf list (list obj))))
  )
;;TEST
;; (my-append1 '(NIL) 'THIS)  = (THIS)
;; (my-append1 '(NIL NIL) 'THIS) = (THIS)
;; (my-append1 '(A B) 'C) = (A B C)



;;LAST1
;;from PGraham
;;ddd
(defun last1 (lst)
  (car (last lst)))
;;TEST
;; (last1 '(a b c))

;;SINGLE
;;from PGraham
;;ddd
(defun single (lst)
  (and (consp lst) (not (cdr lst))))

;;MKLIST
;;from PGraham
;;ddd
(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;ASSURE-LIST
;;2019 from PGraham mklist
;;ddd
(defun assure-list (obj &key not-nil-p)
  "U-lists, makes sure obj is a list"
  (if (listp obj) obj (list obj)))
;;TEST
;; (assure-list 'a) = (A)
;; (assure-list nil) = NIL

;;from Figure 4.2: Larger functions that operate on lists.

;;LIST-LONGER-P
;;2019 from PGraham's list-longer-p function
;;ddd
(defun list-longer-p (x y)
  "U-lists, tests to see if list X is list-longer-p than Y"
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
      (> (length x) (length y)))))
;;TEST
;; (list-longer-p '(a b c) '(d e f g h)) = NIL


;;FILTER-BY-FUNC
;;2019 from PGraham's filter
;;ddd 
(defun filter-by-func (func lst)
  "U-lists,  get back a list of whatever non-nil values are returned by the function as it is applied to the elements of the list."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall func x)))
        (if val (push val acc))))
    (nreverse acc)))
;;TEST
;; (filter-by-func 'values  '(a b c d e f g h))
;; works= (A B C D E F G H)
;; (filter-by-func 'numberp   '(a b c d e f g h))  = NIL



;;MAKE-SUBLISTS
;;2019 from PGraham called his version group
;;ddd
(defun make-sublists (source n &key return-only-n-long-p)
  "U-lists, groups list members into sublists n long (last list remainder)"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source))
                   (remain) ;;was a global var
                   (result)   ;;was a global var
                   )
               (when (consp rest)
                 (setf result
                       (cond
                        ((or (null return-only-n-long-p)
                             (>= (length rest) n))
                         (rec rest (cons (subseq source 0 n) acc)))
                        (rest (setf remain rest))
                        (t (nreverse (cons source acc))))
                       ;;end setf
                       )
               (break)
               (values result acc source)  
                       ;;end , when, 
                       )
           (if source (rec source nil) nil)
           (break)
               )))
           ))
;;TEST
;; (make-sublists ’(a b c d e f g) 2) = 

;;GROUP
;;PGraham
;;ddd
(defun group (source n &key return-only-n-long-p)
  "U-lists, groups list members into sublists n long (last list remainder)"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                 (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
;;TEST
;; (group '(a b c d e f g h i j) 3) = ((A B C) (D E F) (G H I) (J))


;;FLATTEN
;;PGraham
;;ddd
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
;;TEST
;; (flatten '(a b (1 2 (x y (z)) 3 4 (aa bb) 5 6) c d))
;;works= (A B 1 2 X Y Z 3 4 AA BB 5 6 C D)


;;PRUNE
;;PGraham
;;ddd
(defun prune (test tree)
  "U-lists. Every leaf for which the function returns true is removed."
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                             (cons (car tree) acc)))))))
    (rec tree nil)))
;;PRUNE
;;(prune ’evenp ’(1 2 (3 (4 5) 6) 7 8 (9))) = (1 (3 (5)) 7 (9))
;; 


;;GET-KEY-LIST
;;
;;ddd
(defun get-key-list (key list)
  "In U-lists, searches a flat list for a sublist that begins with key, RETURNS (values list  key) if found, nil if not. Uses my-equal which will even match symbols and strings."
  (let
      ((result)
       (first-item)
       )
    (loop
     for sublist in list
     do
     (cond
      ((and (listp sublist)(my-equal key (car sublist)))
       (return (setf result sublist)))
      (t nil))
     ;;end loop
     )
    (values result key)
    ;;end let, get-key-list
    ))
;;TEST
;;  (get-key-list 'this '(a (that b c) (4 5 6)("this" l m)(w m 2))) = ("this" L M)


;;GET-NTHS-IN-LISTS
;;
;;ddd
(defun get-nths-in-lists (nth-list list-of-lists)
  "In U-lists, finds corresponding nth-n nth to list-n instead of same nth for all lists. Uses mapcar."
    (mapcar #'nth nth-list list-of-lists)
    ;;end let, get-nths-in-lists
    )
;;TEST
;; (get-nths-in-lists '(0 1 2) '((a b c)(d e f)(g h i))) = (A E I)
;; following have unmatched list lengths
;; (get-nths-in-lists '(0 1 2) '((a b c)(d e f)(g h i) (j k l))) = (A E I)
;; (get-nths-in-lists '(0 1 3) '((a b c)(d e f)))  = (A E)


;;GET-NTH-IN-ALL-2NESTED-LISTS
;;2017
;;ddd
(defun get-nth-in-all-2nested-lists (nth list-of-1nested-lists)
  "In U-lists RETURNS (values nths-1nested-lists non-nth-items). NTHS-1NESTED-LISTS list of all items at nth place for each list in each group of 1nested lists"
  (let
      ((nths-1nested-lists)
       (item-n)
       (all-non-nth-items)
       )
    (loop
     for 1nested-lists in list-of-1nested-lists
     do
     (cond
      ((and 1nested-lists (listp 1nested-lists) )
       (multIple-value-bind (nthslist non-nth-items)
           (get-nth-in-all-lists nth 1nested-lists)
         (setf nths-1nested-lists (append nths-1nested-lists (list nthslist)))
         (when non-nth-items
           (setf all-non-nth-items (append all-non-nth-items  non-nth-items)))
         ;;end mvb, clause
         ))
      (t   (setf all-non-nth-items (append all-non-nth-items (list 1nested-lists)))))
     ;;end loop
     )        
    (values nths-1nested-lists all-non-nth-items)
    ;;end let, get-nth-in-all-2nested-lists
    ))
;;TEST
;;  (get-nth-in-all-2nested-lists 1 '(((D 4)(A 1)(C 3)(B 2))((X 9)(M 4) NON (Z 2))))
;;  works= ((4 1 3 2) (9 4 2))    (NON)



;;GET-NTH-IN-ALL-LISTS
;;
;;ddd
(defun get-nth-in-all-lists (nth list-of-lists)
    "In U-lists RETURNS (values nths-list non-nth-items). NTHS-LIST--A list of all items at nth place for each list in lists"
    (let
        ((nths-list)
         (item-n)
         (non-nth-items)
         )
      (loop
       for list in list-of-lists
       do
       (cond
        ((and list (listp list)
              (setf item-n (nth nth list)))
         (setf nths-list (append nths-list (list item-n))))
        (t  (when list
              (setf non-nth-items (append non-nth-items (list list))))))
       ;;end loop
       )
      (values nths-list non-nth-items)
      ;;end let, get-nth-in-all-lists
      ))
;;TEST
;;  (get-nth-in-all-lists 1 '(a  (1 2 3)(4 5 6) b (7) (8 9 0) 77))
;; works= (2 5 9)  (A B (7) 77)
;;  (get-nth-in-all-lists 0  '((a b c)(1 2 3)(m n o p) x)) = (A 1 M) (X)
;;  (get-nth-in-all-lists 3  '((a b c)(1 2 3)(m n o p) x)) = (P)  ((A B C) (1 2 3) X)
;;  (get-nth-in-all-lists 1    '((X 9)(M 4) NON (Z 2))) = (9 4 2)    (NON)
      


;;GET-BUTLAST-IN-LISTS
;;2020
;;ddd
(defun get-butlast-in-lists (n lists)
  "U-lists RETURNS list of (butlast N) for each list."
  (let*
      ((new-list)
       )
    (loop
     for list in lists
     do
     (setf new-list (append new-list (list (butlast list n))))
     )
    new-list
  ;;end letget-butlast-in-lists
  ))
;;TEST
;; (get-butlast-in-lists 2 '((1 2 3 4 5 6)(a b c d e f)))
;; works= ((1 2 3 4) (A B C D))



;;MAKE-LISTS-FROM-ITEMS
;;2017
;;ddd
(defun make-lists-from-items (items &rest rest-items-lists)
  "In U-lists RETURNS new-lists; List of (list item) in items. If there are n-items in final lists, then rest-items-lists must be (- n-items 1) lists each with items in order to match items in items list."
  (let
      ((new-lists)
       (n-items (list-length items))
       ;;(n-rest-lists (list-length rest-items-lists))
       )
    (loop
     for item in items
     for n from 0 to n-items
     do
     (cond
      ((null rest-items-lists)
       (setf new-lists (append new-lists (list (list item)))))
      (t 
       (let*
           ((newlist (list item))
            )
         (loop
          for list in rest-items-lists
         ;; for n-rest from 1 to n-rest-lists
          do
          (setf newlist (append newlist (list (nth n list))))
          ;;end loop
          )
         (setf new-lists (append new-lists (list newlist)))
         ;;end inner let, t cond
         )))
     ;;end outer loop
     )
    new-lists
    ;;end let,make-lists-from-items
    ))
;;TEST
;;  (make-lists-from-items '(A "this" 77 c))  = ((A) ("this") (77) (C))
;;   (make-lists-from-items '(A "this" 77 c) '(1 1 1 1) '(2 2 2 2) '(3 3 3 3))
;;  works =  ((A 1 2 3) ("this" 1 2 3) (77 1 2 3) (C 1 2 3))
;;   '((X 9)(M 4) NON (Z 2)))

;; *tomex-drive-syms = (C-MAIN-BU TOM-SE5TBHD-MAIN PD-MEDIA)
;; (make-lists-from-items *tomex-drive-syms) 
;; works= ((C-MAIN-BU) (TOM-SE5TBHD-MAIN) (PD-MEDIA))



;;GET-ALL-NTHS-IN-LISTS--DEPRECIATED, but works
;;
;;ddd
(defun get-all-nths-in-lists (nth list-of-lists)
  "In U-lists, DEPRECITED, REPLACED BY get-nth-in-all-lists RETURNS all nth items from lists in list-of-lists"
  (get-nth-in-all-lists nth list-of-lists)
  )
;;TEST
;;  (get-all-nths-in-lists 0  '((a b c)(1 2 3)(m n o p) x)) = (A 1 M)
;;  (get-all-nths-in-lists 3  '((a b c)(1 2 3)(m n o p) x)) = (P)



;;GROUP-ITEMS-BY-NTHS
;;2016
;;ddd
(defun group-items-by-nths (list-of-lists &key return-items-by-listlength-p)
  "In U-lists. Re-groups items by groups putting all items with same nth together, etc. Eg  all nth=0 in same new list. RETURN-ITEMS-BY-LISTLENGTH-P puts items in lists by order of original list length."
  (let*
      ((newlist)
       (item)
       (grouped-list)
       (longest-lists (find-longest-lists list-of-lists))
       (longest-list-n (list-length (car longest-lists)))
       (nth-grouped-lists (my-make-list  longest-list-n :initial-element nil))
       ;;(n-lists (list-length list-of-lists))
       )
    ;;(BREAK "1")
    (when RETURN-ITEMS-BY-LISTLENGTH-P
      (setf list-of-lists longest-lists))
        (loop
     for n from 0 to longest-list-n
     do
     (loop
      for list in list-of-lists
      do
      (setf item (nth n list))

      (cond
       (item
        (setf nth-grouped-lists 
              (append-nth-list item n nth-grouped-lists :if-not-found-create-p T)))
       (t  NIL))

     ;;(BREAK "2")
      ;;end inner loop
      )
     ;;end outer loop
     )
    (values nth-grouped-lists)
    ;;end let, group-items-by-nths
    ))
;;TEST
;; (group-items-by-nths  '((1 2 3 4 5 6 7)(A B C D E F G H I)(91 92 93 94 95)))
;; works= ((1 A 91) (2 B 92) (3 C 93) (4 D 94) (5 E 95) (6 F) (7 G) (H) (I))
;; (group-items-by-nths  '((1 2 3 4 5 6 7)(A B C D E F G H I)(91 92 93 94 95)))
;; :return-items-by-listlength-p
;;  (group-items-by-nths  '((1 2 3 4 5 6 7)(A B C D E F G H I)(91 92 93 94 95)) :RETURN-ITEMS-BY-LISTLENGTH-P T)
;; works= ((A 1 91) (B 2 92) (C 3 93) (D 4 94) (E 5 95) (F 6) (G 7) (H) (I))
;; (group-items-by-nths  '((1 2 )(A B C)(91 92)))
;;  (append-nth-list 1 0 nil)


;;GROUP-LISTS-BY-NTHS
;;2017
;;ddd
(defun group-lists-by-nths (nth list-of-lists  &key (group-matched-lists-p T))
  "In U-lists  RETURNS  (values grouped-lists non-grouped-lists)  INPUT:  If group-matched-lists-p, puts each group in a list. Does NOT SORT lists."
  (let*
      ((grouped-lists)
       (non-grouped-lists)
       (match-list)
       ;;(len-lols (list-length list-of-lists))
         (nth-val) 
          (rest-lists)
          )
    (cond
     ((and (listp list-of-lists)(listp (car list-of-lists)))
      (setf  match-list (car list-of-lists)
             rest-lists (cdr list-of-lists)
             nth-val (nth nth match-list))

      ;;GET EACH LIST THAT MATCHES NTH-VAL
      (multiple-value-bind (nth-matched-lists non-matched-lists)
          (get-lists-with-equal-nth-items nth nth-val rest-lists)
#|        (setf grouped-lists (append grouped-lists
                                    (list (append (list match-list) nth-matched-lists)))
              non-grouped-lists non-matched-lists)|#
        (cond
         (group-matched-lists-p
          (setf grouped-lists (append grouped-lists (LIST (append (list match-list) nth-matched-lists)))
              non-grouped-lists non-matched-lists)
          )
         (T (setf grouped-lists (append grouped-lists (list match-list) nth-matched-lists)
              non-grouped-lists non-matched-lists)))
        ))
     (t nil))

      ;;RECURSE ON NON-MATCHED-LISTS
      (when (and non-grouped-lists (listp non-grouped-lists))
        (multiple-value-bind (nth-matched-lists non-matched-lists)                                               
            (group-lists-by-nths nth non-grouped-lists 
                                 :group-matched-lists-p group-matched-lists-p)                                 

            (setf grouped-lists (append grouped-lists  nth-matched-lists)
                    non-grouped-lists non-matched-lists)

          #|(cond
           (group-matched-lists-p
            (setf grouped-lists (append grouped-lists (list nth-matched-lists))
                  non-grouped-lists non-matched-lists))
           (T (setf grouped-lists (append grouped-lists  nth-matched-lists)
                    non-grouped-lists non-matched-lists)))|#
          ;;end mvb, when
          ))
    (values grouped-lists non-grouped-lists)
    ;;end let, group-lists-by-nths
    ))
;;TEST
;; GROUP-MATCHED-LISTS-P = T
;; (group-lists-by-nths  1  '((a 1)(b 2)(c 3)(x 1)(y 2)(z 3)(w 4)))
;; works=(((A 1) (X 1)) ((B 2) (Y 2)) ((C 3) (Z 3)) ((W 4)))  NIL
;; GROUP-MATCHED-LISTS-P = NIL
;; (group-lists-by-nths  1  '((a 1)(b 2)(c 3)(x 1)(y 2)(z 3)(w 4)) :GROUP-MATCHED-LISTS-P NIL)
;; works= ((A 1) (X 1) (B 2) (Y 2) (C 3) (Z 3) (W 4))  NIL
;; supporting function:
;; (get-lists-with-equal-nth-items 1 2 '((a 1)(b 2)(c 3)(x 1)(y 2)(z 3)(w 4)))
;; ((B 2) (Y 2))  ((A 1) (C 3) (X 1) (Z 3) (W 4))  NIL
;; ;; (group-lists-by-nths  1  '((a .1)(b .2)(c 3)(x .1)(y .2)(z 3)(w 4)) )
;; (((A 0.1) (X 0.1)) ((B 0.2) (Y 0.2)) ((C 3) (Z 3)) ((W 4)))   NIL




;;GROUP-ITEMS-BY-EQUAL-NTH-ITEMS
;;2017
;;ddd
(defun group-items-by-equal-nth-items (nth list-of-lists &key (put-nth-first-p T) 
                                           appendp  (test 'my-equal) 
                                           remove-matchitem-p
                                           (cycle-n 1) (max-cycles 100))
  "In  U-lists, RETURNS (values  grouped-lists other-items);  where if put-nth-first-p AND appendp=NIL grouped-list items are (nth (rest of list)), appendp=T (nth rest of list). If  put-nth-first-p = NIL, simply makes  lists of all matching sublists. INPUT: NOTE: If :remove-matchitem-p=T and put-nth-first-p=T, removes inner matchitems, but leaves matchitem at first position in list.  SEE ALSO group-items-by-nths and group-lists-by-nths"
  (let
      (;;(len-lol (list-length list-of-lists))
       (grouped-lists)
       (non-grouped-lists)
       (other-items)
       (list (car list-of-lists))
       )

    (cond
     ((and (listp list)
           (>= (list-length list) nth))
      (let*
          ((nthitem (nth nth list))
           (list-group)            
           )
        (multiple-value-setq (list-group non-grouped-lists other-items)
            (get-lists-with-equal-nth-items nth nthitem list-of-lists
                                            :appendp appendp
                                            :remove-matchitem-p remove-matchitem-p :test test))
        ;;put nthitem first?
        (cond
         (put-nth-first-p
          (setf grouped-lists  (list (append (list nthitem)  list-group))))
         (t (setf grouped-lists  (list list-group))))
        ;;end let, listp
        ))
     (t (setf other-items (list list)
              non-grouped-lists (cdr list-of-lists))))

    ;;(break "1")
    ;;RECURSE ON NON-GROUP-LISTS
    (when (and non-grouped-lists (< cycle-n max-cycles))
      (multiple-value-bind (grouped-lists1 other-items1)
          (group-items-by-equal-nth-items  nth non-grouped-lists :appendp appendp
                                            :remove-matchitem-p remove-matchitem-p
                                           :put-nth-first-p put-nth-first-p :test test                    
                                           :cycle-n (incf cycle-n) :max-cycles (- max-cycles 1))
        (setf grouped-lists (append grouped-lists  grouped-lists1)
              other-items (append other-items other-items1))
        ;;end mvb,when
        ))
    ;;(afout 'out (format nil "grouped-lists= ~A~%non-grouped-lists= ~A" grouped-lists non-grouped-lists))
    (values  grouped-lists other-items) ;; non-grouped-lists)
    ;;end let, group-items-by-equal-nth-items 
    ))
;;TEST
;; (group-items-by-equal-nth-items 0 '((2 a b c) 88 (1 m n)(2 d e)(3 11 12)(1 o p)(3 13 14 15) aa (2 f g h i)) :max-cycles 10)
;; works=  ((2 (2 A B C) (2 D E) (2 F G H I)) (1 (1 M N) (1 O P)) (3 (3 11 12) (3 13 14 15)))     (88 AA)
;; remove-matchitem-p = T
;; (group-items-by-equal-nth-items  0 '((2 a b c) 88 (1 m n)(2 d e)(3 11 12)(1 o p)(3 13 14 15) aa (2 f g h i))    :remove-matchitem-p T :appendp NIL)
;; works= ((2 (A B C) (D E) (F G H I)) (1 (M N) (O P)) (3 (11 12) (13 14 15)))     (88 AA)
;; appendp = T
;; (group-items-by-equal-nth-items  0 '((2 a b c) 88 (1 m n)(2 d e)(3 11 12)(1 o p)(3 13 14 15) aa (2 f g h i))    :remove-matchitem-p T :appendp T)
;;works= ((2 A B C D E F G H I) (1 M N O P) (3 11 12 13 14 15))    (88 AA)
;; (group-items-by-equal-nth-items  1 '((a 1)( b 2)(c 3)(x 1)(y 2)(z 3)(w 4))  :remove-matchitem-p T :appendp T)
;; works= ((1 A X) (2 B Y) (3 C Z) (4 W))   NIL
;;  (group-items-by-equal-nth-items  1 '((a 1)( b 2)(c 3)(x 1)(y 2)(z 3)(w 4))  :remove-matchitem-p T :appendp nil)
;; works= ((1 (A) (X)) (2 (B) (Y)) (3 (C) (Z)) (4 (W)))  NIL
;; (group-items-by-equal-nth-items  1 '((a 1)( b 2)(c 3)(x 1)(y 2)(z 3)(w 4))  :remove-matchitem-p NIL :appendp T)
;; works= ((1 A 1 X 1) (2 B 2 Y 2) (3 C 3 Z 3) (4 W 4))  NIL
;; (group-items-by-equal-nth-items  1 '(((a 1)( b 2)(c 3))((x 1)(y 2)(z 3)(w 4)))  :remove-matchitem-p T :appendp T)
;;doesn't work with 2-nested lists = (((B 2) (A 1) (C 3)) ((Y 2) (X 1) (Z 3) (W 4)))  NIL
;;
;;(group-items-by-equal-nth-items 1 '((A 0.4) (B 0.2) (C 0.1) (D 0.7) (E 0.3) (L 0.4) (M 0.7) (O 0.2) (P 0.7) (Q 0.4) (R 0.1) (S 0.7)))
;;works= ((0.4 (A 0.4) (L 0.4) (Q 0.4)) (0.2 (B 0.2) (O 0.2)) (0.1 (C 0.1) (R 0.1)) (0.7 (D 0.7) (M 0.7) (P 0.7) (S 0.7)) (0.3 (E 0.3)))   NIL
;; IF DON'T WANT VALUE LISTED IN CAR OF LIST
;; (group-items-by-equal-nth-items 1 '((A 0.4) (B 0.2) (C 0.1) (D 0.7) (E 0.3) (L 0.4) (M 0.7) (O 0.2) (P 0.7) (Q 0.4) (R 0.1) (S 0.7)) :put-nth-first-p NIL)
;; works= (((A 0.4) (L 0.4) (Q 0.4)) ((B 0.2) (O 0.2)) ((C 0.1) (R 0.1)) ((D 0.7) (M 0.7) (P 0.7) (S 0.7)) ((E 0.3)))  NIL

;;GROUP-ITEMS-BY-


;;GET-LISTS-WITH-EQUAL-NTH-ITEMS
;;2017
;;ddd
(defun get-lists-with-equal-nth-items (nth matchitem list-of-lists   
                                          &key appendp  (test 'my-equal) remove-matchitem-p)
  "In  U-lists, RETURNS (values matched-lists nonmatched-lists other-items)   where if put-nth-first-p AND appendp=NIL grouped-list items are (nth (rest of list)), appendp=T (nth rest of list). If  put-nth-first-p = NIL, simply makes  lists of all matching sublists. INPUT:  "
  (let
      ((len-lol (list-length list-of-lists))
       (matched-lists)
       (nonmatched-lists)  
       (other-items)
       )
    (loop
     for list in list-of-lists
     do
     (let
         ((testitem) 
          )
       (cond
        ((and (listp list) (> (list-length list) nth))
         (setf testitem (nth nth list))
         (cond
          ;;if items match
          ((funcall test matchitem testitem)
           (when remove-matchitem-p
             (setf list (delete-nth list nth)))
           (cond
            (appendp
             (setf matched-lists (append matched-lists list)))
            (t (setf matched-lists (append matched-lists (list list))))))
          ;;not match
          (t
           (setf nonmatched-lists (append nonmatched-lists (list list)))))
         ;;end listp
         )
        ;;testitem not a list or too short list
        (t (setf other-items (append other-items (list list)))))
       ;;end let,loop
       ))
    (values matched-lists nonmatched-lists other-items)
    ;;end let, get-lists-with-equal-nth-items
    ))
;;TEST
;;  (get-lists-with-equal-nth-items 1  'X  '((1 X 3)(a y b)(c x d f) 77 (3 x 4)))
;; works = ((1 X 3) (C X D F) (3 X 4))   ((A Y B))   (77)
;;
;;(get-lists-with-equal-nth-items 1  'X  '((1 X 3)(a y b)(c x d f) 77 (3 x 4)) :remove-matchitem-p T)
;; works= ((1 3) (C D F) (3 4))    ((A Y B))   (77)
;; appendp
;; (get-lists-with-equal-nth-items  0 2 '((2 a b c) 88 (1 m n)(2 d e)(3 11 12)(1 o p)(3 13 14 15) aa (2 f g h i))    :remove-matchitem-p T :appendp T)
;; works= (A B C D E F G H I)      ((1 M N) (3 11 12) (1 O P) (3 13 14 15))     (88 AA)
;; :remove-matchitem-p NIL :appendp T
;; (get-lists-with-equal-nth-items  0  2 '((2 a b c) 88 (1 m n)(2 d e)(3 11 12)(1 o p)(3 13 14 15) aa (2 f g h i))    :remove-matchitem-p NIL :appendp T)
;; works= (2 A B C 2 D E 2 F G H I)     ((1 M N) (3 11 12) (1 O P) (3 13 14 15))    (88 AA)






;;GET-NTH-IN-KEYLIST
;;
;;ddd
(defun get-nth-in-keylist (key nth list)
  "In U-lists, searches a flat list for a sublist beginning with key, RETURNS (values nth-element keylist) where nth is the nth item in the sublist."
  (let*
      ((keylist (get-key-list key list))
       (nth-element (nth nth keylist))
       )
    (values nth-element keylist)
    ;;end let get-nth-in-keylist
    ))
;;TEST
;;  (get-nth-in-keylist 'key1  2  '(a b (1 2 3) x (key1 9 10 11 12) x y))
;; works = 10    (KEY1 9 10 11 12)    


;;(nth 3 '(1 2 3 4 5))

;;GET-NTH-IN-2NESTED-LIST
;;
;;ddd
(defun get-nth-in-nth-2nested-list (nth nth-item 2nested-list &key match-item)
  "In U-lists. RETURNS (values all-found-nested-lists  all-found-matched-lists) using nth item in a single nested list. nth-item must be a list. Both nth starts with 0. 2nested-list is a list of nested lists.  eg. ((a b c)(d e (1 X 3))(g h (1 Y 3))) can find x, y."
  (let
      ((found-item)
       (matched-sublist)
       (matched-nested-list)
       (found-list)
       (found-nested-list)
       (all-found-nested-lists)
       (all-found-matched-lists)
       )
    (loop
     for nested-list in 2nested-list
     do
     (multiple-value-setq (found-item matched-sublist)
         (get-nth-in-nth-nested-list nth nth-item nested-list :match-item match-item))
     ;;(afout 'out1 (format nil "nested-list= ~A~% found-item= ~A~% matched-sublist= ~A~%" nested-list found-item matched-sublist))
     (cond
      ((and match-item matched-sublist)
       (setf found-nested-list nested-list             
             matched-nested-list nested-list
       all-found-nested-lists (append all-found-nested-lists (list nested-list))
       all-found-matched-lists (append all-found-matched-lists (list nested-list)))
       )
      (found-item
       (setf found-nested-list nested-list             
       all-found-nested-lists (append all-found-nested-lists (list nested-list)))
       )
      (t nil))

     ;;end loop
     )
    (values all-found-nested-lists  all-found-matched-lists)
    ;;end let, get-nth-in-nth-2nested-list
    ))
;;TEST
;; (progn (setf out1 nil) (get-nth-in-nth-2nested-list 4  3 '((1 (40 NIL) "Wup1-3" (1 3 2 TO 1 1 3)) (1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (1 (120 NIL) "Wup1-3" (1 3 2 TO 3 1 3)) (1 (160 NIL) "Wup1-3" (1 3 2 TO 4 1 3)) (1 (200 NIL) "Wup1-3" (1 3 2 TO 5 1 3)) (2 (240 NIL) "Wup1-3" (2 3 2 TO 1 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3)))))
;;result= ((1 (40 NIL) "Wup1-3" (1 3 2 TO 1 1 3)) (1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (1 (120 NIL) "Wup1-3" (1 3 2 TO 3 1 3)) (1 (160 NIL) "Wup1-3" (1 3 2 TO 4 1 3)) (1 (200 NIL) "Wup1-3" (1 3 2 TO 5 1 3)) (2 (240 NIL) "Wup1-3" (2 3 2 TO 1 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3)))  NIL
;; USE :MATCH-ITEM 2
;; (progn (setf out1 nil) (get-nth-in-nth-2nested-list 4  3 '((1 (40 NIL) "Wup1-3" (1 3 2 TO 1 1 3)) (1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (1 (120 NIL) "Wup1-3" (1 3 2 TO 3 1 3)) (1 (160 NIL) "Wup1-3" (1 3 2 TO 4 1 3)) (1 (200 NIL) "Wup1-3" (1 3 2 TO 5 1 3)) (2 (240 NIL) "Wup1-3" (2 3 2 TO 1 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3))) :match-item 2))
;;RESULTS= ((1 (40 NIL) "Wup1-3" (1 3 2 TO 1 1 3)) (1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (1 (120 NIL) "Wup1-3" (1 3 2 TO 3 1 3)) (1 (160 NIL) "Wup1-3" (1 3 2 TO 4 1 3)) (1 (200 NIL) "Wup1-3" (1 3 2 TO 5 1 3)) (2 (240 NIL) "Wup1-3" (2 3 2 TO 1 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3)))
;;((1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3)))



;;GET-NTH-IN-NTH-NESTED-LIST
;;
;;ddd
(defun get-nth-in-nth-nested-list (nth nth-item nested-list &key match-item (test 'my-equal))
  "In U-lists. Returns the nth item in a single nested list. nth-item must be a list. Both nth starts with 0. RETURNS (values found-item matched-list). If match-item, returns matched-list if found-item my-equal match-item."
  (let
      ((found-item)
       (found-list (nth nth-item nested-list))
       (matched-sublist)
       )
    (when (listp found-list)
      (setf found-item (nth nth found-list)))
    (when (and match-item (funcall test match-item found-item)) ;; (my-equal match-item found-item))
      (setf matched-sublist found-list))
    ;;(break)
    (values found-item matched-sublist)
    ;;end let, get-nth-in-nth-nested-list
    ))
;;TEST
;; (get-nth-in-nth-nested-list  3  2  '(1 2 (a b c d) 4 5)) = D
;; (get-nth-in-nth-nested-list  3  2  '(1 2 (a b c d) 4 5) :match-item  "d")    
;; works= D   (A B C D    




;;GET-KEY-VALUE-IN-SINGLE-NESTED-LIST
;;
;;ddd
(defun get-key-value-in-single-nested-list (key single-nested-list &key return-list-p)
  "In U-lists, uses get-key-value-in-nested-lists. RETURNS (values return-value return-key extra-return-nth-items extra-items-list) OR entire list with key if return-list-p. KEY must be first item in target list."
   (get-key-value-in-nested-lists (list (list key 0))  single-nested-list :return-list-p return-list-p)
   )
;;TEST
;; (get-key-value-in-single-nested-list 'key1 '((x x x)b (key0 a b)(key1 d e f)(key2 1 2)))
;;works= D  KEY1  NIL  (B)
;;  (get-key-value-in-single-nested-list 'key1 '((x x x)b (key0 a b)(key1 d e f)(key2 1 2)) :return-list-p t) 
;;works= (KEY1 D E F)  KEY1  NIL  (B)

;;GET-KEY-VALUE-IN-NESTED-LISTS
;; gets values in all sorts of lists and nested lists
;;
;;ddd
(defun get-key-value-in-nested-lists (key-spec-lists nested-lists 
                                                      &key return-list-p no-return-extra-p (test 'my-equal)
                                                      final-key-inside-keylist-p) ;; return-nth (now in spec)
  "In U-lists.lisp, DEPRECIATED--(Replaced by get-keyvalue-in-nested-list ) RETURNS first value that matches key (values return-value key extra-return-nth-items extra-items). The spec-lists is a list of 2 or 3 item spec lists. (key find-nth return-nth)  If  key = T, searches entire list of lists for key if find-nth = T, searches entire sublist for the key.  If return-list-p, RETURNS entire key-list as return-value. Extra items are extra non-list items on all level lists that might contain important info. Can process infinite? levels of key lists. Can match with equal or string-equal automatically. The spec-lists are arranged from outer-most list to inner-most list. final-key-inside-keylist-p means that the last key is inside a list--so items in ourer list won't cause a false match."
  (let*
      ((return-value)
       (return-key)
       (extra-items-list)
       (extra-return-nth-items)
       (spec-list (car key-spec-lists))
       (key (first spec-list))
       (find-nth (second spec-list))
       (return-nth (third spec-list))
       (new-spec-lists (cdr key-spec-lists))
        (extra-items1 )
        (extra-return-nth-items1 )
        (current-level-return-extra-value1 )
        ( match-item2 )
        (extra-items1 )
        (length-item2 )
       ( extra-return-nth-items1 )
       ( extra-items1)
       ( list-length3)
       ( match-item3)
       )
    (unless  find-nth (setf find-nth 0))
    (unless (or return-nth (equal find-nth T))
                (setf return-nth (+ find-nth 1)))
 
    (cond
     ;;SPEC IS TO SEARCH EVERY LIST AT THIS LEVEL
     ;;yyy
     ((or (equal key t) (equal find-nth t))
      ;;(afout 'out (format nil "NEW CALL TO T  key= ~S~% (car of list= ~S~%" key (car nested-lists)))   
      (loop
       for item  in nested-lists
       do
       ;;(afout 'out (format nil "T OUTER-LOOP new-spec-lists= ~S~%  item= ~S~%" new-spec-lists item))
       ;;Should it return a  value from this level (eg name))
       (if (and return-nth (listp item))
           (setf current-level-return-extra-value1 (nth return-nth item)))

       ;;test to see what the spec-list indicates
       (cond
        ((and item (listp item))
         ;;note: this may call other recursive calls
         (multiple-value-setq (return-value return-key extra-return-nth-items1 extra-items1)
             (get-key-value-in-nested-lists   new-spec-lists  item
                                              :return-list-p return-list-p
                                              #|:return-nth return-nth|# 
                                              :no-return-extra-p no-return-extra-p
                                              :final-key-inside-keylist-p final-key-inside-keylist-p))

         ;;these are the extra items want to return (bec inside of target containing list)         
         (cond
          (return-key  
           ;;add extra items from list of item contaning the matched key
           (unless no-return-extra-p
           (if extra-items1 (setf extra-items-list (append extra-items-list (list extra-items1))
                                  extra-return-nth-items (append extra-return-nth-items (list extra-return-nth-items1)))))
           (return))
          (t nil))
         ;;end item clause
         )
        ;;may be non-list items such as other keys providing info re: found outer list.
        (t nil)) ;; (if item (setf extra-items-list (append extra-items-list (list item))))))
       ;;end loop, find-nth = t
       ))
     ;;SSS
     ;;AT LOWEST LIST, SEARCH FOR KEY-VALUE
     ((and (null new-spec-lists) key)
      (loop
       for item in nested-lists
       do
       ;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~S  match-item= ~S~% find-nth= ~S~% " key match-item find-nth ))
       ;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~S  match-item= ~S~% find-nth= ~S~%nested-lists= ~S~% IN find-key-value-in-lists " key match-item find-nth nested-lists))

       (cond
        ;;SEARCH INSIDE A NESTED LIST -- NOT IN OUTER LIST FOR KEY
        ((and (listp item) (null final-key-inside-keylist-p))
         (setf  length-item2 (list-length item))       
         (unless (>= find-nth  length-item2))
         (setf match-item2 (nth find-nth item))
         (cond
          ((funcall test key match-item2)                  ;;was (my-equal key match-item2)
           (cond
            (return-list-p
             (setf return-value item
                   return-key match-item2))
            (t
             (setf return-value (nth return-nth item)
                   return-key key)))
           (return))
          (t nil))
         ;;end listp item
         )
        ;;if not list, search current list for item, just check to see if item = key
        ;;problem if an item matches key but real key is inside a later list
        ;;SEARCHES OUTER LIST FOR KEY MATCH--NOT AN INNER LIST
        (final-key-inside-keylist-p
         (cond
          ((funcall test key item)                ;;was (my-equal key item)
           ;;was (or (equal key item) (if (stringp item) (string-equal key item)))
           (cond
            (return-list-p 
             (setf return-value nested-lists
                   return-key item))
            (t
             (setf return-value (nth return-nth nested-lists)
                   return-key key)))
           (return))
          (t (if  item (setf extra-items-list (append extra-items-list (list item)))))))
         ;;problem
        (t (if item (setf extra-items-list (append extra-items-list (list item)))))
        ;;end cond, loop, equal null new-spec-lists
        )))
     ;;SPEC IS TO SEARCH THIS LEVEL (not last level) BY KEY at FIND-NTH
     ((and  new-spec-lists key)
      ;;(afout 'out (format nil "OUTER-LOOP SEARCH new-spec-lists= ~S~%" new-spec-lists))         
      ;;check each list at this level lll
      (loop
       for item  in nested-lists
       ;;  with new-spec-list1
#|       with extra-return-nth-items1 )
       with extra-items1 
       with list-length3  
       with match-item3|#
       do
       ;;for each sublist, check find-nth
       ;;(afout 'out (format nil "KEY OUTER-LOOP key= ~S~% item= ~S~%new-spec-lists= ~S~%" key item new-spec-lists))
       (cond
        ((listp item)
         (setf  list-length3 (list-length item))     
         (unless (>= find-nth  list-length3))
         (setf match-item3 (nth find-nth item))
         ;;(afout 'out (format  nil "OUTER LOOP TESTING key= ~S  match-item3= ~S~%  IN find-key-value-in-lists" key match-item3))
         (cond
          ((funcall test key match-item3)    ;;was (my-equal key match-item3)
           ;;(or (equal key match-item) (if (stringp match-item) (string-equal key match-item)))
           (multiple-value-setq (return-value return-key extra-return-nth-items1 extra-items1)
               (get-key-value-in-nested-lists  new-spec-lists item
                                              #| :return-nth return-nth|#
                                               :return-list-p return-list-p
                                               :no-return-extra-p no-return-extra-p))
           ;;(afout 'out (format nil "ON RECURSE RETURN return-value= ~S~% return-key=~S~%" return-value return-key))
           (unless no-return-extra-p
             (if extra-items1 
                 (setf extra-items-list (append extra-items-list (list extra-items1))))
             (if extra-return-nth-items1
                  (setf extra-return-nth-items (append extra-return-nth-items 
                                                      (list extra-return-nth-items1)))))          
           (return))
          (t nil))
         ;;end listp item
         )
        (t (if item (setf extra-items-list (append extra-items-list (list item))))))
       ;;end loop, equal new-spec-lists
       ))
     ;;IF TOP LEVEL NONE-OF-ABOVE
     (t nil)) ;;no items at top level 
    ;;end find-key-value-in-lists
    (if  no-return-extra-p
        (setf extra-items-list nil))
    (values return-value return-key extra-return-nth-items extra-items-list)
    ;;end let, get-key-value-in-nested-lists
    ))
;;TEST
;; (get-key-value-in-nested-lists '((T 0) (THVSELFAQ 0)) *all-shaq-questions  :return-list-p t)
;;works =  (THVSELFAQ ("There are one or more aspects (or parts) of myself that I have a hard time accepting or do not like.") TBV-INSTR THVSELFACCEPTQ)     THVSELFAQ      (NIL)     ((TBV))
;;TEST
;;(get-key-value-in-nested-lists '((T 0) ("iecicont" 0)) *shaq-question-variable-lists :return-list-p t)
;;WORKS =  ("iecicont" "ie-I control life-happiness" "spss-match" "iecILOFCiVSe" ("iecILOFCiVSe" "2" "iecILOFCiVSeQ" "int" "Agree7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "iIEcontrol.java") (:HELP NA NA))     "iecicont"   (NIL)    ((IE))
;;(get-key-value-in-nested-lists '((T 0) ("userid"  0)) *shaq-question-variable-lists :return-list-p t)  =  ("UserID" "UserID" "spss-match?" NO-PC-INST-MATCH (:HELP NA NA))    "UserID"    (NIL)   ((ID))
;; (get-key-value-in-nested-lists  (list (list 1 1))  '((:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)))) (:DIMSIM 2 ((2 (80 200) WUP1-2 (2 1)))))   :return-list-p T)
;;works = (:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1))))

;;yyy
;; (progn (setf out nil) (get-key-value-in-nested-lists '((t 0)("smtsdevelopment" 0)) *all-shaq-pc-instances :return-list-p t))
;;  (progn (setf out nil)  (get-key-value-in-nested-lists  '(("iAcademicMotivation.java" 1)( "acmESOCSTudy" 0)) '((PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight"))))) ;;(fout out)) ;; :return-list-p  t))
;;works, returns"acmESOCSTudyQ"  "acmESOCSTudy"  ((PC-INSTANCES "iAcademicMotivation.java"))
;;
;; (progn (setf out nil) (get-key-value-in-nested-lists (list '(T 0)'(smtExercizeQ 0)) *testq-vars3)));; (fout out) *all-shaq-questions)))
;; (progn (setf out nil) (get-key-value-in-nested-lists '((T 0) (thvUncondCareQ 0)) *all-shaq-questions))
;;(get-key-value-in-nested-lists '((T 0) (ugoals 0)) *shaq-question-variable-lists :return-list-p t)

;;  (get-key-value-in-nested-lists '((5 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))))
;;  works= 1 5 NIL NIL
;;  (get-key-value-in-nested-lists '((this 0))  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL
;;  (get-key-value-in-nested-lists '((x 0))  '((a b)(c (1 2))(this (3 4 5))(x y)) )
;; works, returns Y X NIL
;;   (get-key-value-in-nested-lists '((5 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL
;; (get-key-value-in-nested-lists  '((5 0)) '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL
;; SSS
;; (progn (setf out nil) (get-key-value-in-nested-lists  '((5 0)(1 0)) '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t))(fout out))
;;works, returns  (1 6 (QUOTE (A B)))  1 NIL
;; (get-key-value-in-nested-lists  '((5 0)(1 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  1 NIL
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))
;; (progn (setf out nil)  ( get-key-value-in-nested-lists '(( "iWorldviewFears.java" 1)("wovNoLove" 0)) *all-shaq-pc-instances  :return-list-p t))  
;;works, returns= ("wovNoLove" "16" "wovNoLoveQ" "int" "FrAnswerPanel.Fear7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")"wovNoLove"  NIL     (PC-INSTANCES "iWorldviewFears.java")
;; (progn (setf out nil)  (get-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances :find-outer-key "iWorldviewFears.java" :find-nth-first 1)) ;;  :return-list-p t))
;;works, = "16"  "wovNoLove"  NIL  (PC-INSTANCES "iWorldviewFears.java")
;;
;; (progn (setf out nil) (multiple-value-setq (*testfn1 *testfn2 *testfn3 *testfn4) (get-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances  :find-outer-key  t   :return-list-p t) )(fout out)))
 
;;  (progn (setf out nil)  (get-key-value-in-nested-lists    "acmESOCSTudy" '(PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")) :return-list-p  t))
;;works, returns ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")  "acmESOCSTudy"  NIL  (PC-INSTANCES "iAcademicMotivation.java")

;;  (get-key-value-in-nested-lists 'this  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL NIL
;;  (get-key-value-in-nested-lists 'x '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns Y X NIL NIL
;;   (get-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL NIL
;; (get-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL NIL
;; (get-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  5 NIL NIL
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))
;; (get-key-value-in-nested-lists  '((2 1)) '((dim 1 xx)(dim 2 bb)(dim 3 yy)) :return-list-p T) 
;;works = (DIM 2 BB) 2 nil nil



;;REPLACE-KEYLIST
;;
;;ddd
(defun replace-keylist (key nested-lists replacement &key (key-n 0) (test 'my-equal))
  "In U-lists, Replaces the first matched keylist in nested-lists (matches key to key-n item) with replacement (list or nonlist). RETURNS (values new-nested-lists matched-list. Nested list may contain non-list items. Uses my-equal to test."
  (let
      ((matched-keylist)
       (new-nested-lists)
       )
    (loop
     for item in nested-lists
     do
     (cond
      ((and (listp item)
                 (funcall test key (nth key-n item)))  ;;was (my-equal key (nth key-n item)))
       (setf matched-keylist item
             new-nested-lists (append new-nested-lists (list replacement))))
      (t (setf new-nested-lists (append new-nested-lists (list item)))))
     ;;end loop
     )
    ;;end let, replace-keylist
    (values new-nested-lists  matched-keylist)
    ))
;;TEST
;;  (replace-keylist 'key1 '(x (key0 a b c)((nested))(key1 d e f) 77 (key2 l l)) '(replacement-list))
;; (my-equal 'key1 (nth 0 '(key1 d e f)))
;;works = (X (KEY0 A B C) ((NESTED)) (REPLACEMENT-LIST) 77 (KEY2 L L))    (KEY1 D E F)


  

;;APPEND-GROUP-WITH-SAME-BEGIN
;;
;;ddd
(defun append-groups-with-same-begin (begin-keys  keylist  groups 
                                                &key append-nth-match-only (append-nested-p T) (test 'my-equal)
                                                 (append-last-group-list-p T) (append-if-not-found T))
  "In U-lists. Appends the group within groups with same begin keys.  If no group exists and append-if-not-found-p, then makes a new group with keylist. RETURNS (values new-groups matched groups). Only appends first group found. Unless append-nth-match-only, appends ALL matching occurances; if append-nth-only appends only the nth item. append-nested-p removes extra parens in lists to append keylist. Processes LARGE VARIETY OF APPEND TYPES."
  (let*
      ((new-group)
       (new-groups)
       (matched-group)
       (matched-groups)
       (begin)
       (rest-group)
       (nth-match 0)
       (begin-n (list-length begin-keys))
       (rest-keylist (nthcdr begin-n keylist))
       (n-groups (list-length groups))
       (non-modified-groups)
       (group-found-p)
       ;;(old-group)( new-group)
       )
    (loop
     for group in groups
     for n from 1 to n-groups
     do
     (setf begin (butlast group (- (list-length group) begin-n))
           rest-group (nthcdr begin-n group))
     ;;(afout 'out (format nil "AT 1 begin= ~A rest-group= ~A%group= ~A~%  begin-keys= ~A rest-keylist= ~A~% begin-n= ~A"  begin  rest-group group begin-keys rest-keylist begin-n))
     (cond
      ((funcall test begin-keys begin)    ;;was (my-equal begin-keys begin)
       (setf group-found-p T)
       (cond
        ((or (null append-nth-match-only)
                 (and (= append-nth-match-only nth-match)))
         (cond
          ((and append-nested-p append-last-group-list-p)
           (setf matched-groups (append matched-groups (list group))
                 new-group (list (append  begin-keys (butlast rest-group)
                                      (list  (append-1nested-lists nil (car (last rest-group)) rest-keylist))))
                 new-groups (append new-groups (list new-group))))
;;(append-lists nil '(B C (WHAT)) '((NEW LIST))) = (B C (WHAT) (NEW LIST))
;;(append '(bc) (list (append-1nested-lists nil  ' (WHAT) '((NEW LIST))) = (B C (WHAT) (NEW LIST)))) = (BC (WHAT NEW LIST))
          (append-nested-p
           (setf matched-groups (append matched-groups (list group))
          ;;       new-group (list (append  begin-keys (butlast rest-group)
          ;;                              (append-lists NIL  rest-group rest-keylist)))
                new-group (append-1nested-lists group rest-keylist)
                 new-groups (append new-groups (list new-group)))
           )
          (t 
           (setf matched-groups (append matched-groups (list group))
                 new-group 
                 (append-lists begin-keys  rest-group rest-keylist)
                 new-groups (append new-groups (list new-group)))))
              ;;(afout 'out (format nil "new-groups= ~A~%" new-groups))
         ;;end append
         )
        (t (setf non-modified-groups (append non-modified-groups (list group))
                 new-groups (append new-groups (list group)))))
       ;;end my-equal
       )

       (t (setf non-modified-groups (append non-modified-groups (list group))
                 new-groups (append new-groups (list group)))))
      ;;(afout 'out (format nil "AT XX group= ~A~% matched-groups~A~% begin-keys~A~% rest-keylist~A~%new-groups= ~A~%"  group matched-groups begin-keys rest-keylist new-groups))
     ;;end loop
     )
    (when  (and append-if-not-found (null group-found-p))
       (setf new-groups (append new-groups (list keylist))))
         
    (values new-groups matched-groups non-modified-groups)
    ;;end let, append-group-with-same-begin
    ))
;;TEST
;;:append-nested-p nil :append-last-group-list-p nil
;; Appends the new-list to end of old group
;; NOTE:  to get (B C (WHAT) (NEW LIST))
;; (progn (setf out nil) (append-groups-with-same-begin  '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))  :append-nested-p nil :append-last-group-list-p nil))
;; works= ((D YES) (A (THIS)) (B C (WHAT) (NEW LIST)) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO) (NEW LIST)))   ((B C (WHAT)) (B C X (TOO)))   ((D YES) (A (THIS)) (X Y (THAT)) (A (ANOTHER)))
;;
;;For  :append-nested-p T :append-last-group-list-p T;
;; note how LAST ITEM IN OLD IS APPENDED WITH NEW
;; appends the new list items inside and removes parens around old last item
;; to get ((B C (WHAT NEW LIST)))
;;(progn (setf out nil) (append-groups-with-same-begin  '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))  :append-nested-p T :append-last-group-list-p T))
;;works= ((D YES) (A (THIS))    ((B C (WHAT NEW LIST)))    (X Y (THAT)) (A (ANOTHER))  ((B C X (TOO NEW LIST))))        ((B C (WHAT)) (B C X (TOO)))   ((D YES) (A (THIS)) (X Y (THAT)) (A (ANOTHER)))
;;
;;  :append-nested-p T :append-last-group-list-p nil
;; all parens removed on ONLY NEW ITEMS--not appended to last item in the old list.
;;to get (B C (WHAT) NEW LIST)
;; To remove ALL parens, use ??
;;(progn (setf out nil) (append-groups-with-same-begin  '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))  :append-nested-p T :append-last-group-list-p nil))
;; works= ((D YES) (A (THIS)) (B C (WHAT) NEW LIST) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO) NEW LIST))   ((B C (WHAT)) (B C X (TOO)))   ((D YES) (A (THIS)) (X Y (THAT)) (A (ANOTHER)))
;;
;; append-if-not-found T (if not found puts it at end>
;; IF matching group not found, appends keylist to END of group.
;; (progn (setf out nil) (append-groups-with-same-begin  '(m n) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))  :append-nested-p T :append-last-group-list-p nil :append-if-not-found T))
;; works= ((D YES) (A (THIS)) (B C (WHAT)) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO)) (B C (NEW LIST)))      NIL    ((D YES) (A (THIS)) (B C (WHAT)) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO)))
;;extra
;; (progn (setf out nil) (append-lists  '(B C (WHAT)) '(X Y (THAT))))
;; (progn (setf out nil testx nil new-matched-group1 nil)(append-lists 'new-matched-group1 (append-lists 'testx '((NEW LIST)) '((WHAT)) )  '(b c)))



;;REPLACE-LIST-IF-SAME-BEGIN
;;
;;ddd
(defun replace-list-if-same-begin (begin-keys replacement groups 
                                              &key append-if-not-found-p replace-nth-only (test 'my-equal))
  "In U-lists, Replaces all matched keylists in groups (matches begin-keys to first part of each list) with replacement (list or nonlist) UNLESS replace-nth-only (begins w 1). RETURNS (values new-groups matched-keylists).  Uses my-equal to test. begin-items can be almost any type. If append-if-not-found-p, appends replacement to the list NOTE: replacement must have same begin-keys."
  (let*
      ((matched-keylists)
       (new-groups)
       (begin-n (list-length begin-keys))
       (item-n)
       (dif-n)
       (replacement-rest (nthcdr begin-n replacement))
       (nth-match 0)
       (begin-item)
       )
    (loop
     for item in groups
     ;;for nth-item from 1 to (list-length groups)
     do
     (cond
      ((listp item)
       (setf item-n (list-length item))             
       (cond
        ((>= item-n begin-n)
         (setf dif-n (- item-n begin-n)
               begin-item (butlast item dif-n))
         (cond
          ((funcall test begin-keys begin-item) ;;was (my-equal begin-keys begin-item)
           (incf nth-match)
           (cond
            ((or (null replace-nth-only)
                 (and replace-nth-only (= replace-nth-only nth-match)))
             (setf matched-keylists (append matched-keylists (list item))
                   new-groups (append new-groups (list replacement)))
             )
            (t (setf new-groups (append new-groups (list item)))))
           ;;end my-equal
           )
          (t (setf new-groups (append new-groups (list item)))))
         ;;end >= item-n begin-n
         )
        (t (setf new-groups (append new-groups (list item)))))
       ;;end listp
       )
      (t (setf new-groups (append new-groups (list item)))))
     ;;end loop
     )
    (when (and append-if-not-found-p (null matched-keylists))
      (setf new-groups (append new-groups (list (append begin-keys replacement-rest)))))
    ;;end let, replace-list-if-same-begin
    (values new-groups  matched-keylists)
    ))
;;TEST
;; Replace ALL occurances
;;  (replace-list-if-same-begin '(b) '(B NEW LIST)  '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO))))
;; works=((D YES) (A (THIS)) (B C (NEW LIST)) (X Y (THAT)) (A (ANOTHER)) (B C (NEW LIST)))      ((B C (WHAT)) (B C X (TOO)))

;; Replace 1st occurance
;; (replace-list-if-same-begin '(b) '(B NEW LIST)  '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO))) :replace-nth-only 1)
;;works = ((D YES) (A (THIS)) (B NEW LIST) (X Y (THAT)) (A (ANOTHER)) (B X (TOO)))     ((B (WHAT)))
;;for n = 2
;;(replace-list-if-same-begin '(b) '(B NEW LIST)  '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO))) :replace-nth-only 2)
;;works= ((D YES) (A (THIS)) (B (WHAT)) (X Y (THAT)) (A (ANOTHER)) (B NEW LIST))     ((B X (TOO)))
;;
;;(replace-list-if-same-begin '(y) '(Y (NEW LIST))  '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO))) :append-if-not-found-p t)
;; works= ((D YES) (A (THIS)) (B (WHAT)) (X Y (THAT)) (A (ANOTHER)) (B X (TOO)) (Y (NEW LIST)))   NIL
;;
;;(replace-list-if-same-begin '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO))))




        
       

;;FIND-LIST-ELEMENT-N
;;
;;ddd
(defun find-list-element-n (item list 
                                 &key from-end (test 'my-equal))
  "In U-lists.lisp, . RETURNS (values n result), NIL NIL if not found. Uses my-equal for test."
  (let
      ((testlist)
       (result)
       (n -1)
       )
    (if from-end
        (setf testlist (reverse list)))
    (loop
     for element in list
     do
     (incf n)
     (cond
      ((and element (funcall test item element)) ;;was (my-equal item element)) 
       (setf result element)
       (return) )
      (t nil))
     ;;end loop
     )
    (if from-end
        (setf n (- (list-length list) n)))

    (when (null result)
      (setf n nil))

    (values n result)
    ;;end let, find-list-element-n
    ))
;;TEST
;;  (find-list-element-n "this" '(a b x 33 this 4 that))
;; works =  4  THIS
;;  (find-list-element-n "thm33goa"  (get-all-shaq-variables)) works= 343  "thm33goa" 
   
;;FIND-NTH-IN-LIST
;;
;;ddd
(defun find-nth-in-list (nth list &key from-end)
  "In U-lists.lisp, finds the nth element of a list"
  (let
      ((element)
       (search-list list)
       )
    (if from-end
        (setf search-list (reverse list)))
    (loop
     for n from 0 to (- (list-length list) 1)
     for item in search-list
     do
     (when (= nth n)
       (setf element item)
       (return))
     ;;end loop
     )
    element
    ;;end let, find-nth-in-list
    ))
;;TEST
;;  (find-nth-in-list 148 *test-var-list)   = "stucolle" works




;;FIND
(defun find-item-n (item list &key from-end-p (test 'my-equal) (max-list-n 1000))
  "In U-lists.lisp, finds ONLY FIRST symbol or string item in a list  RETURNS (values nth found-item) (from 0) from-end-p begins at end."
  (let 
      ((nth)
       (testlist list)
       (found-item)
       )
    (when from-end-p
      (setf testlist (reverse list)))
    
    (loop
     for testitem in testlist
     for n from 0 to max-list-n
     do
     (when  (funcall test  item testitem)
       (setf nth n
             found-item testitem)
       (return))
     ;;end loop
     )
    (values nth found-item)
    ))
;;TEST
;; (find-item-n 'this  '(a b d 99 "this" c  88)) = 4   "this"
;; (find-item-n 'this  '(a b d 99 "this"   88) :from-end-p T) = 1 "this"






;;FIND-LIST-ITEM
;;
;;ddd
(defun find-list-item (item list &OPTIONAL return-first-p use-my-equal-p)
  "In U-lists.lisp, finds a symbol or string item in a list (returns list of all matched items or NIL and found-ns. If return-first-p, then only returns first item (not a list) and found-ns is an integer. If USE-MY-EQUAL-P, uses MY-EQUAL. RETURNS (values return-item found-ns)."
  (let 
      ((return-item)
       (found-ns)
       (eqfunc)
       )
    (cond
     (use-my-equal-p
      (setf eqfunc 'my-equal))
     (t (setf eqfunc 'equal)))
    (loop
     for test-item in list
     for n from 0 to (list-length list)
     do
     (cond
      ((eval `(,eqfunc (quote ,item) (quote ,test-item)))
       (cond
        (return-first-p
         (setf return-item test-item
               found-ns n)
         (return))
        (t (setf return-item (append return-item (list test-item))
                 found-ns (append found-ns (list n))))))
      (t nil))
      ;;end dolist
      )
    (values return-item found-ns)
    ))
;;test
;;  (find-list-item 'this  '(a b c this x y this z)) =  (THIS THIS)  (3 6)
;;  (find-list-item 'this  '(a b c this x y this z)  T) = THIS   3
;; IF use-my-equal-p (note string equal sym)
;;  (find-list-item "this"  '(a b c this x y z) t T) = THIS  3
;; (find-list-item  "4"  '( 1 2 3 4 5 6)) = NIL  NIL
;; (find-list-item  "4"  '( 1 2 3 4 5 6) NIL T) = (4)  (3)
;; (find-list-item  "4"  '( 1 2 3 4 5 6) T T)  = 4  3


;;FIND-LIST-ITEM-BY-SUBSTRINGS
;;2019
;;ddd
(defun find-list-item-by-substrings (substrings list &key return-first-p )
  "In U-lists.lisp, finds a symbol or string item in a list (returns list of all matched items or NIL and found-ns. If return-first-p, then only returns first item (not a list) and found-ns is an integer. If substrings is a list, uses MATCH-SUBSTRINGS, if not uses MATCH-SUBSTRING."
  (let 
      ((return-item)
       (found-ns)
       (eqfunc)
       )
    (loop
     for test-item in list
     for n from 0 to (list-length list)
     do
     (cond
      ((listp substrings)
       (cond
        ((match-substrings  substrings test-item)
         (cond
          (return-first-p
           (setf return-item test-item
                 found-ns n)
           (return))
          (t (setf return-item (append return-item (list test-item))
                   found-ns (append found-ns (list n))))))
        ;;end listp substrings
        ))
      ((> (length substrings) 0)
       (cond
        ((match-substring substrings test-item)
         (cond
          (return-first-p
           (setf return-item test-item
                 found-ns n)
           (return))
          (t (setf return-item (append return-item (list test-item))
                   found-ns (append found-ns (list n))))))
        ;;end listp substrings
        ))
      (t nil))
      ;;end dolist
      )
    (values return-item found-ns)
    ;;end let, find-list-item-by-substrings
    ))
;;TEST
;; NONLIST
;;  (find-list-item-by-substrings "this"  '("that" "nomatchhere" "nope""abcthis"  "thisxyz" "midthisend"))
;; works = ("abcthis" "thisxyz" "midthisend")   (3 4 5)
;; LIST
;;  (find-list-item-by-substrings '("mat" "abc" "end") '("that" "nomatchhere" "nope""abcthis"  "thisxyz" "midthisend"))
;; works= ("nomatchhere" "abcthis" "midthisend")  (1 3 5)
;; NO MATCH
;; (find-list-item-by-substrings "what"  '("that" "nomatchhere" "nope""abcthis"  "thisxyz" "midthisend")) 
;;works= NIL




;;PRINT-LIST
;;2020 modified to work with format 
;;ddd
(defun print-list (list &key stream no-newline-p incl-quotes-p incl-parens-p incl-label sort-string-test (num-spaces 1) separator-str )
  "in U-lists.lisp, Prints each list item on newline (or not) with quotes (or not) for strings (only). If sort-string-test, use it (eg. #'sting<) to sort the list. ALSO see FORMAT-STRING-LIST--maybe a better choice. separator-str puts that string in place of spaces between elements.
  NOTE: 2020-Will EVAL & PRINT FORMAT STATEMENTS."
  (let
      ((string "")
       (space " " )
       (space1)
       )
    (cond
     (separator-str
      (setf space separator-str))
     ((> num-spaces 0)
      (dotimes (n  num-spaces)
        (setf space (format nil "~A " space))))
     (t (setf space "")))

    (if sort-string-test
        (setf list (sort list sort-string-test)))
    (if incl-parens-p
        (setf string (format nil "~A" #\( )))
    (if incl-label
        (setf string (format nil "~A~A "string incl-label)))
    ;;for each item
    (loop
     for item in list
     for n from 0 to (list-length list)
     do
     (let*
         ((item-string "")
          )
     ;; (format t "item= ~A item-string= ~A~%" item item-string)
     ;;incl-quotes-p -- Include quotes or not?
       (cond
        ((> n 0) (setf space1 space))
        (t (setf space1 "")))
     (cond
      ((null (listp item))
       (cond
        (incl-quotes-p
         (setf item-string (format nil "~A~A~S" item-string space1 item)))
        (t
         (setf item-string (format nil "~A~A~A" item-string space1 item))))
       ;;end stringp
       )
      ;;for FORMAT lists within the list
      ((and (listp item) (equal (car item) 'format))
       (setf item-string (format nil "~A~A" space1 (eval item)) ))
      (t nil))                
     ;;newline-p--newline at end of each item?
     (if (null no-newline-p)
         (setf item-string (format nil "~A~%" item-string))) 
     (cond
      (incl-parens-p
       (setf string (format nil "~A~A~A"string item-string  #\))))
      (t (setf string (format nil "~A~A"string item-string ))))
     ;;end let,loop
     ))
    (format stream "~A" string)
    ))
;;TEST
;;2020 (print-list '(one "two" (format nil "This is a ~A" 'TEST) "what") :no-newline-p T)
;;works= "ONE  two  This is a TEST  what"
;; (print-list '(one "two" (format nil "This is a ~A" 'TEST) "what"))
#|"ONE
  two
  This is a TEST
  what
"|#
;;  (print-list '(M F L I) :no-newline-p t :separator-str "-") = "M-F-L-I"
;;
;;  (print-list '("a" "b" C "d") :no-newline-p t :num-spaces 0)
;;  works = "abCd"
;;  (print-list '("a" "b" C "d") :no-newline-p t :separator-str "-")
;;  works= "a-b-C-d"
;;(print-list '("a" "b" C "d") :no-newline-p t :num-spaces 2) = "a   b   C   d"
;; (print-list '("a" "b" c "d"))
;; (print-list '(z a l m) :sort-string-test #'string< :stream T :no-newline-p t ) 
;; works = ALMZ
;; works =  A  L  M  Z NIL
;; (print-list '("a" "b" c "d") :stream t :incl-quotes-p t)
;;works
#|"a"
"b"
C
"d"|#
;; (print-list '("a" "b" c "d") :stream t :incl-quotes-p t :incl-parens-p t)
#| ("a"
  "b"
  C
  "d"
)
NIL|#
;;  (print-list  '(test (a (b 1 2 3)(c 4 5 6) d (e 7 ("x" 1 2 3) f  "g")))  :stream t :incl-quotes-p t :incl-parens-p t :incl-label 'MY-TEST-LABEL)
#| (MY-TEST-LABEL TEST
(A (B 1 2 3) (C 4 5 6) D (E 7 ("x" 1 2 3) F "g"))
)
NIL|#
;; (setf xxyy '(a b c d))
;; (print-list xxyy)
#|"A
  B
  C
  D
"|#
;; (print-list 
;; (make-one-dim-indecies '(3 1 1 1))
;;

#|
;;OLD DEFINITION
(defun print-list (list &key stream no-newline-p incl-quotes-p incl-parens-p incl-label sort-string-test)
  "in U-lists.lisp, Prints each list item on newline (or not) with quotes (or not) for strings (only). If sort-string-test, use it (eg. #'sting<) to sort the list."
  (if sort-string-test
      (setf list (sort list sort-string-test)))
  (if incl-parens-p
      (format stream " ~A" #\( ))
  (if incl-label
      (format stream "~A~%" incl-label))
  (unless stream
    (setf stream nil))
  ( dolist (item list)
    ;;incl-quotes-p -- Include quotes or not?
    (cond
     (incl-quotes-p
      (format stream " ~S" item))
     (t
      (format stream " ~A" item)))
    ;;newline-p--newline at end of each item?
    (if (null no-newline-p)
        (format stream "~%"))
    ;;end dolist
    )
  (if incl-parens-p
      (format stream " ~A" #\) ))
  )|#
(defun print-list2 (list)
  (let
      ((string "")
       )
    (loop
     for item in list
     do
     (setf string (format nil "~A~A" string item))
     )
    (format nil "~A" string)
    ))
;; (print-list2 '("a" "b" C "d")) 
;; works = "abCd"



;;PRINT-NESTED-LIST
;;
;;ddd
(defun print-nested-list (list-of-lists &key stream no-newline-p
                                        incl-quotes-p no-outer-parens-p  incl-label)
  "in U-lists.lisp, Prints each list item on newline (or not) with quotes (or not) for strings (only). If incl-label = a label, it is printed as first item in list."
    (unless no-outer-parens-p  (format stream " ~A" #\( ))
    (if incl-label
        (format stream "~A~%" incl-label))
  ( dolist (item list-of-lists)
    (cond
     ((listp item)
      (format stream " ~A" #\( )
      (print-list item :stream stream :no-newline-p no-newline-p
                  :incl-quotes-p incl-quotes-p)
      (format stream "  ~A~%" #\)))
     (t 
      ;;incl-quotes-p -- Include quotes or not?
      (cond
       (incl-quotes-p
        (format stream " ~S" item))
       (t
        (format stream " ~A" item)))
      ;;newline-p--newline at end of each item?
      (if (null no-newline-p)
          (format stream "~%"))))
    ;;  (format stream " ~A" #\)
    ;;end dolist
    )
   (unless no-outer-parens-p   (format stream " ~A" #\) ))
 )
;;test
;;   (print-nested-list '("a" ("b" c) "d" (e (x "y" z) f)) :stream t :incl-quotes-p t)
;;  (print-nested-list  '(test (a (b 1 2 3)(c 4 5 6) d (e 7 ("x" 1 2 3) f  "g" ) (x y z)(l m n)))  :stream t :incl-quotes-p t   :incl-label 'MY-TEST-LABEL)
;;  


;;PRINT-DOUBLE-NESTED-LIST
;;
;;ddd
(defun print-double-nested-list (list-of-nested-lists &key stream 
                              incl-label no-newline-p incl-quotes-p (no-outer-parens-p t))
                 ;; LATER ADD? pre-spaces)
  "in U-lists.lisp, Prints each list item on newline (or not) with quotes (or not) for strings (only). Incl-label includes the label given with the keyword as first list item."
  (format stream " ~A" #\( )
  ( dolist (nested-list  list-of-nested-lists)
    (cond
     ((listp nested-list)
     ;; (format stream " ~A" #\( )
       (print-nested-list nested-list  :stream stream  :no-newline-p no-newline-p
         :incl-label incl-label :incl-quotes-p incl-quotes-p :no-outer-parens-p no-outer-parens-p)
#|      (print-list item :stream stream :no-newline-p no-newline-p
                  :incl-quotes-p incl-quotes-p)|#
     ;; (format stream "  ~A~%" #\))
              )
     (t 
      ;;incl-quotes-p -- Include quotes or not?
      (cond
       (incl-quotes-p
        (format stream " ~S" nested-list))
       (t
        (format stream " ~A" nested-list)))
      ;;newline-p--newline at end of each item?
      (if (null no-newline-p)
          (format stream "~%"))))
    ;;  (format stream " ~A" #\)
    ;;end dolist
    )
      (format stream " ~A" #\) )
 )
;;test
;;   (print-double-nested-list '("a" ("b" c) "d" (e (x "y" z) f)) :stream t :incl-quotes-p t)


#|
;;PRINT-STRING-LIST
;;
;;ddd
(defun print-string-list (string-list &optional stream)
  "in U-lists.lisp,"
  (unless stream
    (setf stream nil))
  ( dolist (string string-list)
    (format stream "~A~%" string)
    ))
;;test
;; (print-string-list *test-output T)
|#

;;MY-LESSP
;;modified 2019 to return first item
;;ddd
(defun my-lessp (item1 item2 &optional only-nums-p)
  "In U-lists, items can be numbers, strings, or symbols. If both numbers, compares using >, if symbols, converts to strings, then compares by string-greaterp. 
  IF TRUE,RETURNS FIRST ITEM When ONLY-NUMS-P, if either arg not a number, then returns NIL"
  (let
      ((result)
       (item1-str)
       (item2-str)
       )
    (cond
     ((and (numberp item1)(numberp item2))
      (setf result (< item1 item2)))
     ((null only-nums-p)
      (cond
       ((and (stringp item1)(stringp item2))
        (setf result (string-lessp item1 item2)))
       (t (setf item1-str (format nil "~A" item1)
                item2-str (format nil "~A" item2)
                result (string-lessp item1-str item2-str)))))
     (t nil))
    (when result
      (setf result item1))
    result
    ;;end let, my-lessp
    ))
;;TEST
;;  (my-lessp 7  3) = NIL
;;  (my-lessp 3 7) = 3  ;was T
;;  (my-lessp "THIS" "apple") = NIL
;;  (my-lessp "APPLE" "this")  = "APPLE" ;;was 0
;;  (my-lessp 'this 'apple) = NIL
;;  (my-lessp 'apple 'this) = APPLE ;;was 0




;;MY-GREATERP
;;
;;ddd
(defun my-greaterp (item1 item2 &optional only-nums-p)
  "In U-lists, items can be numbers, strings, or symbols. If both numbers, compares using >, if symbols, converts to strings, then compares by string-greaterp. RETURNS item1 if greater, otherwise NIL. When ONLY-NUMS-P, if either arg not a number, then returns NIL"
  (let
      ((result)
       (item1-str)
       (item2-str)
       )
    (cond
     ((and (null item1)(null item2)) nil)
     ((null item2)(setf result item1))
     ((null item1) nil) ;;bec item2 NOT greater than item1
     ((and (numberp item1)(numberp item2))
      (setf result (> item1 item2)))
     ((null only-nums-p)
      (cond
       ((and (stringp item1)(stringp item2))
       (setf result (string-greaterp item1 item2)))
      (t
       (setf item1-str (format nil "~A" item1)
             item2-str (format nil "~A" item2)
             result (string-greaterp item1-str item2-str)))))
     (t nil))
    (when result
      (setf result item1))
     result
    ;;end let, my-greaterp
    ))
;;TEST
;; (my-greaterp 'a nil) = A
;; ;; (my-greaterp nil 'a ) = nil
;; (my-greaterp nil nil) = nil
;; (my-greaterp 7 2) = 7
;; (my-greaterp "apple" "orange") = NIL
;; (my-greaterp  "orange" "apple") = "orange"
;; (my-greaterp 'xyz 'abc) = XYZ
;;  (my-greaterp 'abc  'xyz) = NIL
;;2019
;;  (my-greaterp "A" 7) = "A"
;;  (my-greaterp  7 "A") = NIL
;; (my-greaterp  7 "A" T)
 


;;MY-GREATERP-NUM
;;2019
;;ddd
(defun my-greaterp-num (num1 num2 &key (if-non-num NIL))
  "U-lists. For use as a test on numbers only.   RETURNS T or NIL. Non-numbers return NIL  "
  (cond
   ((and (numberp num1)(numberp num2))
    (let
        ((result (> num1 num2))
         )
      ;;end let, my-greaterp-num
      result))
   (t if-non-num))
  )
;;TEST
;; (my-greaterp-num 5  6) = NIL
;; (my-greaterp-num 6  3) = T
;; (my-greaterp-num 'X  2 :if-non-num 'non-num) = NON-NUM


;;MY-LESSP-NUM
;;2019
;;ddd
(defun my-lessp-num (num1 num2 &key (if-non-num NIL))
  "U-lists. For use as a test on numbers only.   RETURNS T or NIL. Non-numbers return NIL  "
  (cond
   ((and (numberp num1)(numberp num2))
    (let
        ((result (< num1 num2))
         )
      ;;end let, my-lessp-num
      result))
   (t if-non-num))
  )
;;TEST
;; (my-lessp-num 5  6) = T
;; (my-lessp-num 6  3) = NIL
;; (my-lessp-num 'X  2 :if-non-num 'non-num) = NON-NUM











;;MANUAL SORTED LIST
;; testlist= (3 9 2 6 1) sorted= nil
;; -greatest  ;; 3 > 9 nil, S=NIL; S= (9 3) T=(2 6 1)
;; -2 (car T) >  9(carS) nil, recurse
;; 2 > 3 (2nd/last S) nil (last S); S= (9 3 2) T=(6 1)
;; 6(car T) > 9(carS) nil; 
;; 6 > 3(2nd S) T;  S = [head= (9) cur=6 tail=(3 2)] = (9 6 3 2)   T= (1) 
;; 1(car/last T) > 9(carS)  nil; 
;; 1(car/last T) > 6 2ndS  nil;
;; 1(car/last T) > 3 3rdS nil;
;; 1(car/last T) > 2 (last S); nil S= (9 6 3 2 1)  T= NIL

;;SSSSSS START HERE LEARN HOW RECCURSE WITH sort-x
(defun sort-x (testlist &key sortedlist (sortn 0) (test 'my-greaterp) (delete-nils-p T))
  "U-lists"
  (let*
      ((curitem (car testlist))
       (testitem (cond 
                  (sortedlist (car sortedlist))
                  (t (second testlist))))
       (result (funcall test curitem testitem))
       (len-sortedlist (list-length sortedlist))
       )
    (when delete-nils-p
      (setf sortedlist (delete NIL  sortedlist)))

    ;;(afout 'out (format nil "BEGIN curitem= ~A testitem= ~A testlist= ~A sortedlist= ~A sortn= ~A" curitem testitem testlist  sortedlist sortn))
    ;;TESTING ONLY
    ;;(incf **stop-n)
    ;;(when (= **stop-n 50) (break "STOP"))
    (when (and  curitem testitem)
      (cond
       (result
        (cond
         (sortedlist
          (setf sortedlist (append (list curitem) sortedlist)))
         ;;null sortedlist
         (t (setf sortedlist  (list curitem testitem)
                  testlist (cdr testlist))))
        ;;end result
        )
       ((null curitem) NIL)
       ;;NULL RESULT
       (t
        (cond
         (sortedlist
          (loop
           for sorteditem in sortedlist
           for n from 0 to len-sortedlist
           for left-n from len-sortedlist downto  0
           do
           (let*
               ((result1 (funcall test curitem sorteditem))
                (but-n n) ;; (- n 1))
                )
             ;;(afout 'out (format nil "BEGIN LOOP, curitem= ~A sorteditem= ~A  sortedlist= ~A result1= ~A"  curitem sorteditem sortedlist result1))
             (cond
              (result1
               ;;but-n and n are right to use below--checked
               (setf sortedlist (append (butlast sortedlist  (- but-n 1))
                                        (list curitem sorteditem) (nthcdr (+ n 1) sortedlist)))
               ;;test only (when (and (numberp curitem)(= curitem 3))(break "IN result 3"))
               (return))
              (t
               (when (= left-n 1)
               ;;(afout 'out (format nil "IN LOOP when left-n=1, curitem= ~A sorteditem= ~A  sortedlist= ~A "  curitem sorteditem sortedlist))
                 (setf sortedlist (append1 sortedlist curitem))
                 (return))
               ;;(when (and (numberp curitem)(= curitem 3))(break "RESULT NIL 3"))
               ))
             ;;(afout 'out (format nil "IN LOOP, curitem= ~A sorteditem= ~A  sortedlist= ~A "  curitem sorteditem sortedlist))
             ;;END LET, LOOP 
             ))
          ;;end sortedlist
          )
         ;;null sortedlist
         (t (setf sortedlist  (list  testitem curitem)
                  testlist (cdr testlist)))
         ))

       )
      ;;(when (and (numberp curitem)(= curitem 3))(break "END 3"))
      (when testlist
        (multiple-value-bind (sortedlist1 testlist1)  ;;if don't use (cdr testlist) stack overfl
            (sort-x  (cdr testlist)  :sortedlist sortedlist :sortn (incf sortn)  :test test)
          (setf sortedlist  sortedlist1
                testlist  testlist1)
          ;;end mvb,when 
          ))
      ;;(afout 'out (format nil "END curitem= ~A testitem= ~A result= ~A testlist= ~A sortedlist= ~A sortn= ~A" curitem testitem result testlist  sortedlist sortn))
      (when delete-nils-p
        (setf sortedlist (delete NIL  sortedlist)))
      ;;end when (and testitem curitem)
      )
    (values sortedlist testlist)
    ;;end let, when, sort-x
    ))
;;TEST
;; (sort-x '(4 7 3 9 3 5))
;; (progn (setf out nil) (sort-x  '(5 6 7 8)))
;; works= (8 7 6 5)  NIL
;; (progn (setf out nil)  (sort-x '(4 5 6 7 3 9 3 5)))
;; works= (9 7 6 5 5 4 3 3)  NIL

                       

#|(defun sort-lists (nth  testlists  &key sortedlists  (sort-n 0)
                        (test 'my-greaterp))
  "   RETURNS    INPUT:  "
  (let*
      ((curlist (car testlists))
       (curitem (nth nth curlist))
       (testlist  (cond ((null sortedlists) (second testlists))
                        (t (car sortedlists))))
       (testitem (nth nth testlist))
       (result (funcall test curitem testitem))
       (restlists (cond ((null sortedlists) (cddr testlists))
                        (t (cdr testlists))))
       (sorthead)
       (sorttail)
       (len-sorted (list-length sortedlists))       
       )
    (incf sort-n)
    ;; (when (equal curitem 2) (break "2"))
    (cond
     (result 
      (cond
       (sortedlists
        (setf sortedlists (append (list curlist) sortedlists))
        )
       ;;null sortedlists 
       (t
        (setf sortedlists (list curlist testlist))
        ;;(break "null sortedlists result")
        )))
     ;;null result
     (t   
      (cond
       (sortedlists
        (let*
            ((len-sortedlists1 (list-length sortedlists))
             (sorthead1  (butlast sortedlists  (- len-sorted sort-n)))
             (sorttail1 (nthcdr sort-n sorted-lists))
             ;;not needed?
             (sortedsublists (append (list curlist) (cdr sortedlists)))
             )
          (break "t")
          (multiple-value-setq (sortedlists testlists restlists) ;;should not be restlists?
              (sort-lists nth  sorttail1    :sortedlists nil  :test test))
          (break "null result, after recurse")
          ;;end let, sortedlists
          ))
       ;;null sortedlists
       (t
        (setf sortedlists (list curlist testlist ))
        ;;end t,cond/t,cond
        ))))
 
    ;;FOR TESTING ONLY
   ;; (setf *alltests (append *alltests (list (list curlist testlist RESULT  ))))
    (incf **stop-n)
    (when (= **stop-n 20) (break "STOP"))

    (when restlists (multiple-value-setq (sortedlists testlists restlists) 
                        (sort-lists nth  restlists    :sortedlists sortedlists  :test test)))     
    ;;(afout 'out (format nil "sortedlists= ~A restlists= ~A~% testlists= ~A "sortedlists restlists testlists ))
    (values   sortedlists testlists restlists)
    ;;end let, sort-lists
    ))|#


;;SORT-LISTS
;;2019
;;ddd
(defun sort-lists (nth  testlists  &key sortedlists  (sort-n 0)
                        (test 'my-greaterp))
  " USE sort-1nested-lists INSTEAD   RETURNS    INPUT:  "
  (let*
      ((curlist (car testlists))
       (curitem (nth nth curlist))
       (testlist  (cond ((null sortedlists) (second testlists))
                        (t (car sortedlists))))
       (testitem (nth nth testlist))
       (result (funcall test curitem testitem))
       (restlists (cond ((null sortedlists) (cddr testlists))
                        (t (cdr testlists))))
       (sorthead)
       (sorttail)
       (len-sorted (list-length sortedlists))
       )
    (incf sort-n)
    ;; (when (equal curitem 2) (break "2"))
    (cond
     (result 
      (cond
       (sortedlists
        (setf sortedlists (append (list curlist) sortedlists))
        )
       ;;null sortedlists 
       (t
        (setf sortedlists (list curlist testlist))
        ;;(break "null sortedlists result")
        )))
     ;;null result
     (t   
      (cond
       (sortedlists
        (let*
            ((len-sortedlists1 (list-length sortedlists))
             (sorthead1  (butlast sortedlists  (- len-sorted sort-n)))
             (sorttail1 (nthcdr sort-n sortedlists))
             ;;not needed?
             (sortedsublists (append (list curlist) (cdr sortedlists)))
             )
          ;;(break "t")
          (multiple-value-setq (sortedlists testlists restlists) ;;should not be restlists?
              (sort-lists nth  sorttail1    :sortedlists nil  :test test))
          (break "null result, after recurse")
          ;;end let, sortedlists
          ))
       ;;null sortedlists
       (t
        (setf sortedlists (list curlist testlist ))
        ;;end t,cond/t,cond
        ))))
 
    ;;FOR TESTING ONLY
   ;; (setf *alltests (append *alltests (list (list curlist testlist RESULT  ))))
#|    (incf **stop-n)
    (when (= **stop-n 20) (break "STOP"))|#

    (when restlists (multiple-value-setq (sortedlists testlists restlists) 
                        (sort-lists nth  restlists    :sortedlists sortedlists  :test test)))     
    ;;(afout 'out (format nil "sortedlists= ~A restlists= ~A~% testlists= ~A "sortedlists restlists testlists ))
    (values   sortedlists testlists restlists)
    ;;end let, sort-lists
    ))
;;TEST
;; (setf *alltests nil)
;;  (sort-lists 1 '((d 4)(f 6)(b 2)(e 5)(c 3)))
;;  SSSSS START HERE DEBUGGING -- WHY DOES THE TEST YIELD WRONG RESULTS??




;;SORT-LIST
;;2019
;;ddd
(defun sort-list (testlist &key sortedlist (sortn 0) (test 'my-greaterp) (delete-nils-p T))
  "U-lists. Sorts list with different options than CL sort and my-sort based on it."
    (when (and delete-nils-p (null sortedlist))
      (setf testlist (delete NIL  testlist)))
  (unless sortedlist
    (setf sortedlist (list (car testlist))
          testlist (cdr testlist)))
  (let*
      ((curitem (car testlist))
       (len-sortedlist (list-length sortedlist))
       (new-sortedlist sortedlist) ;;already sorted (incl testlist items)
       (next-testlist (cdr testlist))
       )
    ;;(afout 'out (format nil "BEGIN curitem= ~A  testlist= ~A sortedlist= ~A sortn= ~A" curitem  testlist  sortedlist sortn))
    ;;TESTING ONLY
    ;;(incf **stop-n)
    ;;(when (= **stop-n 50) (break "STOP"))
    (cond
     ;;TEXT CURITEM VS EACH ITEM IN SORTEDLIST, IF FAIL TEST, PUT BEFORE NEXT ITEM
     ((and curitem sortedlist)
      ;;(break "curitem sortedlist")
      (loop
       for sorteditem in sortedlist
       for n from 0 to len-sortedlist ;;only (- len-sortedlist 1) will loop
       for left-n from len-sortedlist downto  0  
       do
       (let*
           ((result (funcall test curitem sorteditem))
            ;;(but-n  n) ;; (- n 1))
            )
         ;;(afout 'out (format nil "BEGIN LOOP, curitem= ~A sorteditem= ~A  sortedlist= ~A result= ~A"  curitem sorteditem sortedlist result))

         (cond
          ((null result)
          ;; (break "result0")
           ;;but-n and n are right to use below--checked
           (setf new-sortedlist (append  (butlast sortedlist left-n)  (list curitem ) (last sortedlist left-n)))
           ;;(break "result")
           ;;test only (when (and (numberp curitem)(= curitem 3))(break "IN result 3"))
         ;;(afout 'out (format nil "END LOOP, curitem= ~A sorteditem= ~A  new-sortedlist= ~A left-n= ~A n= ~A "  curitem sorteditem sortedlist left-n n))
           (return))
          ((= n (- len-sortedlist 1))
           (setf new-sortedlist (append sortedlist (list curitem)))
         ;;(afout 'out (format nil "END LOOP, curitem= ~A sorteditem= ~A  new-sortedlist= ~A "  curitem sorteditem sortedlist))
           (return))
          (t nil))

         ;;END LET, LOOP 
         ))
      ;;end (and curitem
      )
     ;;null sortedlist
     (curitem
      (setf new-sortedlist (list curitem)))
     ;;null curitem and sortedlist
     (t nil))

    ;;(break "1")
    (when next-testlist    
        (setf new-sortedlist  ;;if don't use (cdr testlist) stack overfl
            (sort-list  next-testlist  :sortedlist new-sortedlist :sortn (incf sortn)  :test test)))

      ;;(afout 'out (format nil "END curitem= ~A  result= ~A testlist= ~A next-testlist= ~A new-sortedlist= ~A sortn= ~A" curitem  result testlist next-testlist  new-sortedlist sortn))
      (when delete-nils-p
        (setf new-sortedlist (delete NIL  new-sortedlist))) 
    new-sortedlist
    ;;end unless, let, when, sort-list
    ))
;;TEST
;; (progn (setf out nil)  (sort-list '(4 5 6 7 3 9 3 5)))
;;works = (3 3 4 5 5 6 7 9)
;; (progn (setf out nil)  (sort-list '(6 7 3 9 3 5) :sortedlist '(4 5)))
;; works= (3 3 4 5 5 6 7 9)
;; (progn (setf out nil)  (sort-list '(6 3 5) :sortedlist '(4 5)))
;; works= (3 4 5 5 6)
;; (progn (setf out nil)  (sort-list '(4 5 6 7 3 9 3 5) :test 'my-lessp))
;; works= (9 7 6 5 5 4 3 3)

          

;;MY-SORT-LISTS
;;
;;ddd
(defun my-sort-lists (nth lists  &key sorted-lists test (ascending-p T) descending-p from-end reduce-duplicates-p ) 
  "In U-lists.  Sorts by largest item first only by NTH in the lists.  RETURNS (values sorted-lists ascending-lists descending-lists other-items length-newlist) If  DESCENDING-P, both sorted-lists and descending-lists contain descending lists. Othewise SORTED-LIST is ASCENDING.  ascending-lists is always listed.  If from-end, counts from end. If sorted-lists, begins with presorted (ascending) lists to append lists to.  REDUCE-DUPLICATES-P may get rid of all duplicates.
 DEPRECIATED but work ascending-p descending-p. TEST overides them--uses funcall."
  (let*
      ((ascending-lists)
       (descending-lists)
       (temp-list)
       (other-items)
       (greaterp)
       (length-list)
       (length-newlist)
       (nth-list )
       (new-nth)
       )
    (loop
     for list in lists
     do
     ;;ignore  if not a list
     (cond
      ((listp list)
       (setf  length-list (list-length list))
       (setf new-nth nth) ;;not needed??
                   
       (cond
        ((> length-list new-nth)
         ;;(setf nth-list (nth new-nth list))
         (multiple-value-setq (sorted-lists other-items length-newlist)
             (append-sorted-lists new-nth list sorted-lists  :other-items other-items :from-end from-end  :no-add-duplicates-p reduce-duplicates-p))
         ;;(break "after append-sorted-lists")
         )
        (t (setf other-items (append other-items (list list)))))     
       ;;end listp
       )
      (t (setf other-items (append other-items (list list)))))
     ;;end loop
     )
    (cond
     (descending-p
      (setf ascending-lists  sorted-lists
            sorted-lists (reverse sorted-lists)
            descending-lists sorted-lists))
     (t (setf ascending-lists  sorted-lists)))
    (values sorted-lists ascending-lists descending-lists other-items length-newlist)
    ;;end let, my-sort-lists
    ))
#|OLD
(defun my-sort-lists (nth lists  &key sorted-lists (ascending-p T) descending-p from-end reduce-duplicates-p) 
  "In U-lists.  Sorts by largest item first only by NTH in the lists.  RETURNS (values sorted-lists ascending-lists descending-lists other-items length-newlist) If  DESCENDING-P, both sorted-lists and descending-lists contain descending lists. Othewise SORTED-LIST is ASCENDING.  ascending-lists is always listed.  If from-end, counts from end. If sorted-lists, begins with presorted (ascending) lists to append lists to.  REDUCE-DUPLICATES-P may get rid of all duplicates."
  (let*
      ((ascending-lists)
       (descending-lists)
       (temp-list)
       (other-items)
       (greaterp)
       (length-list)
       (length-newlist)
       (nth-list )
       (new-nth)
       )
    (loop
     for list in lists
     do
     ;;ignore  if not a list
     (cond
      ((listp list)
       (setf  length-list (list-length list))
       (setf new-nth nth) ;;not needed??

#|     done in append-sorted-list??   (cond
         (from-end 
          (setf new-nth (- length-list nth 1)))
         (t (setf new-nth nth)))|#
                     
       (cond
        ((> length-list new-nth)
         ;;(setf nth-list (nth new-nth list))
         (multiple-value-setq (sorted-lists other-items length-newlist)
             (append-sorted-lists new-nth list sorted-lists  :other-items other-items :from-end from-end  :no-add-duplicates-p reduce-duplicates-p))
         ;;(break "after append-sorted-lists")
         )
        (t (setf other-items (append other-items (list list)))))     
       ;;end listp
       )
      (t (setf other-items (append other-items (list list)))))

     ;;end loop
     )

    (cond
     (descending-p
      (setf ascending-lists  sorted-lists
            sorted-lists (reverse sorted-lists)
            descending-lists sorted-lists))
     (t (setf ascending-lists  sorted-lists)))

    (values sorted-lists ascending-lists descending-lists other-items length-newlist)
    ;;end let, my-sort-lists
    ))|#
;;TEST
;; (my-sort-lists 0 '((a 3  x)(m 1 mm)(z 9 l)(m 4 llm)(a  5  nn)) :DESCENDING-p t)
;; works= ((Z 9 L) (M 1 MM) (M 4 LLM) (A 3 X) (A 5 NN))     ((A 5 NN) (A 3 X) (M 4 LLM) (M 1 MM) (Z 9 L))      ((Z 9 L) (M 1 MM) (M 4 LLM) (A 3 X) (A 5 NN))    NIL 5
;;
;; (my-sort-lists 0 '((a 3  x)(m 1 mm)(z 9 l)(m 4 llm)(a  5  nn)) :ascending-p nil)
;;works= ((A 5 NN) (A 3 X) (M 4 LLM) (M 1 MM) (Z 9 L))    ((A 5 NN) (A 3 X) (M 4 LLM) (M 1 MM) (Z 9 L))    NIL    NIL  5
;;
;;  (my-sort-lists 1 '((R 7 M)(X 1) (2 5 U)  (2 0 6)  (A 3) (A 4 C)  ))
;; works= ((2 0 6) (X 1) (A 3) (A 4 C) (2 5 U) (R 7 M))    ((2 0 6) (X 1) (A 3) (A 4 C) (2 5 U) (R 7 M))    NIL   NIL   6
;;  (my-sort-lists 1 '((R 7 M) 7 (X 1) (this) (2 5 U)  (2 0 6)  (A 3) (A 4 C)  ))
;; works= ((2 0 6) (X 1) (A 3) (A 4 C) (2 5 U) (R 7 M))  ((2 0 6) (X 1) (A 3) (A 4 C) (2 5 U) (R 7 M))     NIL    (7 (THIS))   6

;;  (my-sort-lists 1 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)))  
;; works = ((M 1 MM) (A 3 X) (A 5 NN) (Z 9 L))    ((M 1 MM) (A 3 X) (A 5 NN) (Z 9 L))   NIL   NIL  4
;;  (my-sort-lists 0 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)))  
;; works ((A 5 NN) (A 3 X) (M 1 MM) (Z 9 L))   ((A 5 NN) (A 3 X) (M 1 MM) (Z 9 L))   NIL NIL  4
;;  (my-sort-lists 1 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)) :ascending-p t) 
;; works = ((M 1 MM) (A 3 X) (A 5 NN) (Z 9 L))  ((M 1 MM) (A 3 X) (A 5 NN) (Z 9 L))  NIL  NIL  4

;;  (my-sort-lists 0 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)))  
;; works= ((A 5 NN) (A 3 X) (M 1 MM) (Z 9 L))    (A 5 NN) (A 3 X) (M 1 MM) (Z 9 L))   NIL    NIL   4
;;
;;FROM-END
;;  (my-sort-lists 0 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)) :from-end t)  
;;works = ((Z 9 L) (M 1 MM) (A 5 NN) (A 3 X))   ((Z 9 L) (M 1 MM) (A 5 NN) (A 3 X))   NIL  NIL  4
;;  (my-sort-lists 0 '((a 3  x b)(z 9 l)(m 1 mm)(a  5  nn)) :from-end t)  
;; works = ((A 3 X B) (Z 9 L) (M 1 MM) (A 5 NN))     ((A 3 X B) (Z 9 L) (M 1 MM) (A 5 NN))  NIL  NIL  4
;;  (my-sort-lists 0 '((a 3  x b)(z 9 l)(m 1 mm)(a  5  nn)) :from-end t :descending-p T)
;; works= ((A 5 NN) (M 1 MM) (Z 9 L) (A 3 X B))   ((A 3 X B) (Z 9 L) (M 1 MM) (A 5 NN))   ((A 5 NN) (M 1 MM) (Z 9 L) (A 3 X B))    NIL   4
;; (my-sort-lists 0 '((a 3  x b)(z 9 l)(m 1 mm)(a  5  nn)) :from-end t :ascending-p t)
;; works= ((A 5 NN) (M 1 MM) (Z 9 L) (A 3 X B))  ((A 3 X B) (Z 9 L) (M 1 MM) (A 5 NN))
;;what if a nil in the list??
;;  ;;  (my-sort-lists 1 '((a 3  x)(z 9 l)(m nil  mm)(a  5  nn)))  
;;works = ((Z 9 L) (A 5 NN) (A 3 X) (M NIL MM)) NIL

;;  (setf *textbl (my-sort-lists 0 '((("life_goals_and_meaning.htm" "Life Goals-Values" "Life Goals and Meaning ") ("test_anxiety.htm"  "Perform Anxiety" "Reducing Test or Performance Anxiety") .5)(("assert req.html" "Assertive Request" "How to Make an Assertive Request for a Behavior Change" )(("life_goals_and_meaning.htm" "Life Goals-Values" "Life Goals and Meaning ") ("test_anxiety.htm"  "Perform Anxiety" "Reducing Test or Performance Anxiety") .5) ("c14-lisn.htm" "Intimacy" "Assertive Communication Skills to Create Understanding and Intimacy" ) .2)) :from-end t))
;; works
;; (delete-all-duplicate-nth-lists 0  *textbl);; NO, ONLY WORKS ON UNNESTED LISTS
;;
;;DO IN SEPARATE FUNCTION
;;TEST delete-duplicates-nth    delete-largest-p
;;  (my-sort-lists 0 '((a 3  x)(m 1 mm)(z 9 l)(m 4 llm)(a  5  nn))) 
;;works = ((Z 9 L) (M 1 MM) (M 4 LLM) (A 3 X) (A 5 NN))
;;  (my-sort-lists 0 '((a 3  x)(m 1 mm)(z 9 l)(m 4 llm)(a  5  nn)) :ascending-p t) 
;; works = ((Z 9 L) (M 1 MM) (M 4 LLM) (A 3 X) (A 5 NN)) ((A 5 NN) (A 3 X) (M 4 LLM) (M 1 MM) (Z 9 L))

#|
  (pprint (setf *newly-sorted-pcs (my-sort-lists 2 *unsorted-pc-bestvals-lists2 :descending-p T)))
 (setf *unsorted-pc-bestvals-lists2
 '(("CARE FOR OTHERS" "SELFISH" "0.917" CAREFOROTHERS)
 ("INTIMATE" "NOT INTIMATE" "0.750" INTIMATE)
 ("FLEXIBLE" "RIGID" "0.750" FLEXIBLE)
 ("CASUAL" "FORMAL" "0.500" CASUAL)
 ("HUMBLE" "EGOTISTICAL" "0.667" EGOTISTICAL)
 ("EXUBERANT" "INHIBITED" "0.750" EXUBERANT)
 ("ACCOMPLISHED THEORIST" "NOT THEORIST" "0.833" NOTTHEORIST)
 ("LOVE ME" "INDIFFERENT" "0.750" LOVEX)
 ("LOVE DANCE" "NOT LOVE DANCE" "0.667" LOVEDANCE)
 ("HELPING CAREER" "NOT HELPING CAREER" "0.917" HELPINGCAREER)
 ("HIGH IMPACT" "LO IMPACT" "0.917" HIGHIMPACT)
 ("PSYCHOLOGISTS" "NOT PSYCH" "0.583" PSYCHOLOGISTS)
 ("UNDERSTANDING" "NOT UNDERSTANDING" "0.833" UNDERSTANDING)
 ("NOT IMPULSIVE" "IMPULSIVE" "0.833" IMPULSIVE)
 ("ENTERTAINER" "NOT ENTERTAINER" "0.583" ENTERTAINER)
 ("CALM-ASSERTIVE" "AGGRESSIVE" "0.833" AGGRESSIVE)
 ("CARE ABOUT OTHERS FEELINGS" "NOT CARE" "0.917" CAREABOUTOTHERSFEE)
 ("INSPIRE OTHERS" "NOT INSPIRE" "0.750" INSPIREOTHERS)
 ("BEST FRIEND" "ENEMY" "0.833" BESTFRIEND)
 ("DIRECT-HONEST" "SNEAKY-GAMES" "0.917" DIRECT-HONEST)))
|#
;;WORKS, RESULTS = 
#|(("CARE FOR OTHERS" "SELFISH" "0.917" CAREFOROTHERS)
 ("HELPING CAREER" "NOT HELPING CAREER" "0.917" HELPINGCAREER)
 ("HIGH IMPACT" "LO IMPACT" "0.917" HIGHIMPACT)
 ("CARE ABOUT OTHERS FEELINGS" "NOT CARE" "0.917" CAREABOUTOTHERSFEE)
 ("DIRECT-HONEST" "SNEAKY-GAMES" "0.917" DIRECT-HONEST)
 ("ACCOMPLISHED THEORIST" "NOT THEORIST" "0.833" NOTTHEORIST)
 ("UNDERSTANDING" "NOT UNDERSTANDING" "0.833" UNDERSTANDING)
 ("NOT IMPULSIVE" "IMPULSIVE" "0.833" IMPULSIVE)
 ("CALM-ASSERTIVE" "AGGRESSIVE" "0.833" AGGRESSIVE)
 ("BEST FRIEND" "ENEMY" "0.833" BESTFRIEND)
 ("INTIMATE" "NOT INTIMATE" "0.750" INTIMATE)
 ("FLEXIBLE" "RIGID" "0.750" FLEXIBLE)
 ("EXUBERANT" "INHIBITED" "0.750" EXUBERANT)
 ("LOVE ME" "INDIFFERENT" "0.750" LOVEX)
 ("INSPIRE OTHERS" "NOT INSPIRE" "0.750" INSPIREOTHERS)
 ("HUMBLE" "EGOTISTICAL" "0.667" EGOTISTICAL)
 ("LOVE DANCE" "NOT LOVE DANCE" "0.667" LOVEDANCE)
 ("PSYCHOLOGISTS" "NOT PSYCH" "0.583" PSYCHOLOGISTS)
 ("ENTERTAINER" "NOT ENTERTAINER" "0.583" ENTERTAINER)
 ("CASUAL" "FORMAL" "0.500" CASUAL))|#




;;SORT-LISTS-BY-LIST1
;;2017
;;ddd
(defun sort-lists-by-list1 (list1 list2 &key  (ascending-p t) descending-p from-end reduce-duplicates-p make-new-sublists-p)
  "In U-lists Sorts both lists by SAME nth item in first list.  RETURNS (values sorted-list1 sorted-list2)"
  (let*
      ((dual-list (combine-2-lists list1 list2))
       (ordered-dual-list (my-sort-lists 0 dual-list :ascending-p ascending-p
                                            :descending-p descending-p :from-end from-end
                                            :reduce-duplicates-p reduce-duplicates-p))
       )
    (multiple-value-bind (sorted-list1 sorted-list2)
        (separate-lists-by-sublists 0 ordered-dual-list :make-new-sublists-p make-new-sublists-p)
      (values sorted-list1 sorted-list2)
      ;;end mvb, let, sort-lists-by-list1
      )))
;;TEST
;;  (sort-lists-by-list1 '(3 2 5 4 1) '(c b e d a ))
;; works=  (1 2 3 4 5)   (A B C D E)
;;  (sort-lists-by-list1 '(3 2 5 4 1) '(c b e d a ) :make-new-sublists-p T)
;; work=  ((1) (2) (3) (4) (5))  ((A) (B) (C) (D) (E))





;;GET-LISTS-WITH-EQUAL-KEYVALUES
;;2018-01
;;ddd
(defun get-lists-with-equal-keyvalues (key matchval keylists   
                                          &key appendp  (test 'my-equal) )
  "In  U-lists, RETURNS (values matched-lists nonmatched-lists other-items)   where if put-key-first-p AND appendp=NIL grouped-list items are (key (rest of list)), appendp=T (key rest of list). If  put-key-first-p = NIL, simply makes  lists of all matching sublists. INPUT:  NOTE: If MATCHVAL= :ALL, returns all keylists with the key and a value."
  (let
      ((len-lol (list-length keylists))
       (matched-lists)
       (nonmatched-lists)  
       (other-items)
       )
    (loop
     for keylist in keylists
     do
     (cond
      ((and (listp keylist) (> (list-length keylist) 1))
       (multiple-value-bind (testval key1)
           (get-key-value key keylist)
         (cond
          ;;if items match OR return all with that key
          ((or (funcall test matchval testval)
               ;;when matchval = :all
               (and (equal matchval :all) key1))
           (cond
            (appendp
             (setf matched-lists (append matched-lists keylist)))
            (t (setf matched-lists (append matched-lists (list keylist)))))
           ;;end (or (funcall
           )
          ;;not match
          (t
           (setf nonmatched-lists (append nonmatched-lists (list keylist)))))
         ;;end mvb, listp
         ))
      ;;testval not a list or too short list
      (t (setf other-items (append other-items (list keylist)))))
     ;;end loop
     )
    (values matched-lists nonmatched-lists other-items)
    ;;end let, get-lists-with-equal-keyvalues
    ))
;;TEST
;;  (get-lists-with-equal-keyvalues 'k2 1 '((k1 3)(k2 4)(k1 6 k2 1)(k3 3 k2 1)(k1 2)(k2 4)(k2 0)))
;;works= ((K1 6 K2 1) (K3 3 K2 1))    ((K1 3) (K2 4) (K1 2) (K2 4) (K2 0))   NIL
;; using :all to return all lists with key in them
;;  (get-lists-with-equal-keyvalues 'k2 :all '((k1 3)(k2 4)(k1 6 k2 1)(k3 3 k2 1)(k1 2)(k2 4)(k2 0)))
;; works= ((K2 4) (K1 6 K2 1) (K3 3 K2 1) (K2 4) (K2 0))    ((K1 3) (K1 2))   NIL



;;GROUP-KEYLISTS-BY-KEYVALUES
;;2018-01
;;ddd
(defun group-keylists-by-keyvalues (groupkey keylists 
                                             &key sortkey sort-betw-groups-key
                                             splice-matched-lists-p 
                                             only-group-numbers-p (only-sort-numbers-p T) 
                                     descending-groups-p descending-sorts-p test)
  "In  U-lists,     INPUT: keylists. Keylists are first grouped into nested lists by groupkey. If sortkey, then sorted within that group by sortkey values. RETURNS (values sorted-groupkey-lists other-items). If splice-matched-lists-p, then does NOT group matched lists within separate lists. NOTE: If either groupkey or sortkey is a NUMBER, then if will find NTH value in list instead of key and value. SORT-BETW-GROUPS-KEY if a number, sorts between groups by that nth, if a key sorts by keyvalue (next item). TEST can be my-greater-p or any test."
  (let
      ((groupkey-lists)
       (sorted-wkeyval) 
       (other-items1)
       (sorted-groupkey-lists)
       (non-grouped-lists)
       (match-list)
       ;;(len-lols (list-length keylists))
       ;;(nth-val) 
       (keyvalue)
       (rest-keylists)
       (non-matched-lists)
       (other-items)
       ;;(sorted-by-groupkey-lists)
       )
    ;;SORT LISTS BY GROUPKEY
    ;;Function puts lists in order by keyvalues (so same values are together)

    (cond
     ((numberp groupkey)
      (multiple-value-setq (groupkey-lists other-items1)
          (group-lists-by-nths groupkey keylists
                                                :group-matched-lists-p (null splice-matched-lists-p))))
     (t
      (multiple-value-setq (groupkey-lists sorted-wkeyval other-items1)
          (sort-keylists-by-keyvalue groupkey keylists 
                                     :only-numbers-p only-group-numbers-p
                                     :ascending-p (null descending-groups-p) 
                                     :descending-p descending-groups-p
                                     :group-sorted-lists-p T))))
     ;;append lists
     (setf other-items (append other-items (list other-items1)))
     ;;(break "groupkey-lists")

    ;;SORT BY SORTKEY?
    (cond
     (sortkey    
      (loop
       for grouplist in groupkey-lists
       do
       (let
           ((sorted-group)
            )
         (cond
          ((numberp sortkey)
           (setf sorted-group
                 (my-sort-lists sortkey grouplist :descending-p descending-sorts-p)))
          (t
           (setf sorted-group 
                 (sort-keylists-by-keyvalue sortkey grouplist 
                                            :only-numbers-p only-sort-numbers-p
                                            :ascending-p (null descending-sorts-p) 
                                            :descending-p descending-sorts-p
                                            :group-sorted-lists-p NIL))))     
         ;;append lists
           (setf sorted-groupkey-lists (append sorted-groupkey-lists (list sorted-group)))
         ;;end let, loop,  sortkey
         )))
     (t (setf sorted-groupkey-lists groupkey-lists)))

    ;;SORT BETWEEN GROUPS
    (when sort-betw-groups-key
      (cond
       ((numberp sort-betw-groups-key)
        (cond
         (descending-groups-p 
          (setf test 'my-greaterp))
         (t (setf test 'my-lessp)
            (setf sorted-groupkey-lists 
                  (sort-1nested-lists sorted-groupkey-lists :test test))))
        ;;end numberp
        )
       (t 
        (setf sorted-groupkey-lists
              (sort-keylists-by-keyvalue
               ))
        ))
      ;;end when sort-betw-groups-key
      )
      ;;SSS FIX SPLICE LATER HERE--SPLICE GROUPS INTO OVERALL LISTS??
    (values sorted-groupkey-lists other-items)
    ;;end mvb,let, group-keylists-by-keyvalues
    ))
;;TEST
;; (group-keylists-by-keyvalues 'K2  '((7 K1 5)(3 K2 7 K3 8)(K2 3 K3 1)(K2 7 K3 3)(K2 3 K3 6)(8 K2 3 K3 9)) :sortkey 'K3 )
;; note that the groups are in order of ascending as are the rankings within groups
;;works= (((K2 3 K3 1) (K2 3 K3 6) (8 K2 3 K3 9)) ((K2 7 K3 3) (3 K2 7 K3 8)))   (((7 K1 5)))
;; FOR NTHS
;; ;; (group-keylists-by-keyvalues 1 '((7 K1 5)(K2 7 K3 8)(K2 3 K3 1)(K2 7 K3 3)(K2 3 K3 6)(K2 3 K3 9)) :sortkey NIL)
;;note that the groups are unsorted, since :sortkey=NIL
;; works= (((7 K1 5)) ((K2 7 K3 8) (K2 7 K3 3)) ((K2 3 K3 1) (K2 3 K3 6) (K2 3 K3 9)))   (NIL)



;;GROUP-KEYLISTS-BY-KEYVALUES
;;2018-01
;;ddd
#|(defun group-keylists-by-keyvalues (groupkey keylists 
                                             &key sortkey splice-matched-lists-p 
                                             only-group-numbers-p (only-sort-numbers-p T) 
                                     descending-groups-p descending-sorts-p)
  "In  U-lists,     INPUT: keylists. Keylists are first grouped into nested lists by groupkey. If sortkey, then sorted within that group by sortkey values. RETURNS (values sorted-groupkey-lists other-items). If splice-matched-lists-p, then does NOT group matched lists within separate lists."
  (let
      (;;(groupkey-lists)
       (sorted-groupkey-lists)
       (non-grouped-lists)
       (match-list)
       ;;(len-lols (list-length keylists))
       ;;(nth-val) 
       (keyvalue)
       (rest-keylists)
       (non-matched-lists)
       (other-items)
       ;;(sorted-by-groupkey-lists)
       )
    ;;SORT LISTS BY GROUPKEY
    ;;Function puts lists in order by keyvalues (so same values are together)

    (multiple-value-bind (groupkey-lists1 sorted-wkeyval1 other-items1)
        (sort-keylists-by-keyvalue groupkey keylists 
                                   :only-numbers-p only-group-numbers-p
                                     :ascending-p (null descending-groups-p) 
                                     :descending-p descending-groups-p
                                     :group-sorted-lists-p T)
     ;;append lists
     (setf other-items (append other-items (list other-items1)))

    ;;SORT BY SORTKEY?
    (when sortkey
    (loop
     for grouplist in groupkey-lists1
     do
     (let
         ((sorted-group (sort-keylists-by-keyvalue sortkey grouplist 
                                                   :only-numbers-p only-sort-numbers-p
                                                   :ascending-p (null descending-sorts-p) 
                                                   :descending-p descending-sorts-p
                                                   :group-sorted-lists-p NIL))
          )
       ;;append lists
       (cond
        ((null splice-matched-lists-p)
         (setf sorted-groupkey-lists (append sorted-groupkey-lists (list sorted-group))))
        (t (setf sorted-groupkey-lists (append sorted-groupkey-lists sorted-group))))
       ;;end let, loop, when
       )))
    (values sorted-groupkey-lists other-items)
    ;;end mvb,let, group-keylists-by-keyvalues
    )))|#
;;TEST
;; (group-keylists-by-keyvalues 'K2  '((7 K1 5)(3 K2 7 K3 8)(K2 3 K3 1)(K2 7 K3 3)(K2 3 K3 6)(8 K2 3 K3 9)) :sortkey 'K3 )
;; note that the groups are in order of ascending as are the rankings within groups
;;works= (((K2 3 K3 1) (K2 3 K3 6) (8 K2 3 K3 9)) ((K2 7 K3 3) (3 K2 7 K3 8)))   (((7 K1 5)))





;;SORT-KEYLISTS-BY-KEYVALUE
;;2018-01
;;ddd
(defun sort-keylists-by-keyvalue (key keylists &key (only-numbers-p T)
                                      (ascending-p T) descending-p group-sorted-lists-p
                                      return-only-keylists-by-keyval-p)
  "In U-lists  RETURNS  (values  sorted-keylists sorted-keylists-by-keyval  other-items)     If return-only-keylists-by-keyval-p, sorted-keylists = lists of  ((value keylist), otherwise returns that list as second value. When GROUP-SORTED-LISTS-P, groups each sorted lists within a list with same keyvalues."
  (when (listp keylists)
    (let
        ((sorted-keylists)
         (sorted-keylists-by-keyval)
         (other-items)
         (unsorted-keylists)
         (keylist (car keylists))
         (rest-keylists (cdr keylists))
         (value&keylist)
         (value)
         )
      (loop
       for keylist in keylists
       do
       (cond
        ((listp keylist)
         (multiple-value-bind (value key1)
             (get-key-value key keylist)
           ;;filter if no key or non-numbers if only-numbers-p 
           (cond
            ((or (null key1)
                 (and only-numbers-p
                      (null (numberp value))))
             (setf other-items (append other-items (list keylist))))
            ;;if found key, process
            (t 
             (setf value&keylist (list value keylist)
                   unsorted-keylists (append unsorted-keylists (list value&keylist)))))
           ;;end mvb,listp
           ))
        (t (setf other-items (append other-items (list keylist)))))
       ;;end loop
       )

      ;;SORT THE FINAL LISTS (in form of (keyval keylist)
      (setf sorted-keylists-by-keyval
            (my-sort-lists 0 unsorted-keylists :ascending-p ascending-p
                           :descending-p descending-p))
      (cond
       (group-sorted-lists-p
        (setf sorted-keylists-by-keyval (group-lists-by-nths 0 sorted-keylists-by-keyval))
        (loop
         for list in sorted-keylists-by-keyval
         do
         (setf sorted-keylists (append sorted-keylists 
                                       (list (get-nth-in-all-lists 1 list))))
         ;;end loop, group..
         ))
       (t 
      (unless return-only-keylists-by-keyval-p
        (setf sorted-keylists (get-nth-in-all-lists 1 sorted-keylists-by-keyval)))))

      (values  sorted-keylists sorted-keylists-by-keyval  other-items)
      ;;end mvb, when, let, sort-keylists-by-keyvalue
      )))
;;TEST
;; (sort-keylists-by-keyvalue 'K2 '((K1 3) (K2 4 K1 3) (K1 6 K2 1) (K3 3 K2 1) (K1 2 K2 1) (K2 4 K1 7) (K2 1 K1 5)))
;; works= ((K2 1 K1 5) (K1 2 K2 1) (K3 3 K2 1) (K1 6 K2 1) (K2 4 K1 7) (K2 4 K1 3))  ((1 (K2 1 K1 5)) (1 (K1 2 K2 1)) (1 (K3 3 K2 1)) (1 (K1 6 K2 1)) (4 (K2 4 K1 7)) (4 (K2 4 K1 3)))    ((K1 3))
;; :group-sorted-lists-p
;; (sort-keylists-by-keyvalue 'K2 '((K1 3) (K2 4 K1 3) (K1 6 K2 1) (K3 3 K2 1) (K1 2 K2 1) (K2 4 K1 7) (K2 1 K1 5)) :group-sorted-lists-p T)
;;works= (((K2 1 K1 5) (K1 2 K2 1) (K3 3 K2 1) (K1 6 K2 1)) ((K2 4 K1 7) (K2 4 K1 3)))  (((1 (K2 1 K1 5)) (1 (K1 2 K2 1)) (1 (K3 3 K2 1)) (1 (K1 6 K2 1))) ((4 (K2 4 K1 7)) (4 (K2 4 K1 3))))   ((K1 3))


;;SORT-SYMS-BY-KEYVALUE
;;2020
;;ddd
(defun sort-syms-by-keyvalue (key symlist &key omit-nil-vals-p (test 'my-greaterp) )
  "U-lists RETURNS (values sorted-syms sorted-symval-lists nilval-list)"
  (let*
      ((sorted-symval-lists)
       (nilval-list)
       (symval-lists)
       )
    (loop
     for sym in symlist
     do
     (let*
         ((val (eval-sym sym))
          (symval-pair (list sym (get-key-value key val)))
          )
       (cond
        ((or val (null omit-nil-vals-p))
         (setf symval-lists (append1 symval-lists symval-pair )))
        (T (setf nilval-list (append1 nilval-list sym))))
     ;;end let,loop
     ))
    ;;ORDER THE LISTS
    (setf sorted-symval-lists (sort-1nested-lists 1 symval-lists :test test)
          sorted-syms (get-nth-in-all-lists 0 sorted-symval-lists))   
    (values sorted-syms sorted-symval-lists nilval-list)
    ;;end let, sort-syms-by-keyvalue
    ))
;;TEST
;; (sort-syms-by-keyvalue :CSRANK '(spiritualintegrati careaboutothersfee careforothers  enlightenworld spreadknowledge valuegrouphappines  seekultimatetruth highimpact ) :TEST 'MY-LESSP)
;;works= (VALUEGROUPHAPPINES CAREABOUTOTHERSFEE CAREFOROTHERS ENLIGHTENWORLD SPREADKNOWLEDGE SPIRITUALINTEGRATI SEEKULTIMATETRUTH HIGHIMPACT)
;; ((VALUEGROUPHAPPINES 1) (CAREABOUTOTHERSFEE 2) (CAREFOROTHERS 3) (ENLIGHTENWORLD 4) (SPREADKNOWLEDGE 4.5) (SPIRITUALINTEGRATI 5) (SEEKULTIMATETRUTH 6) (HIGHIMPACT 7))     NIL




;;COMBINE-2-LISTS
;;2017
;;ddd
(defun combine-2-lists (list1 list2 &key (make-item1-item2-lists-p T))
  "In U-lists. If make-item1-item2-lists-p from (a b)+(1 2) to ((a 1)(b 2)). NOTE: If lists unequal length, then does NOT include extra items in longer list."
  (let
      ((newlist)
       )
    (loop
     for item1 in list1
     for item2 in list2
     do
     (cond
      (make-item1-item2-lists-p 
       (setf newlist (append newlist (list (list item1 item2)))))             
      (t (setf newlist (append newlist (list item1 item2)))))
     ;;end loop
     )
    newlist
    ;;endl let, combine-2-lists
    ))
;;TEST
;;  (combine-2-lists '(1 2 3 4) '(a b c d e f))
;; works= ((1 A) (2 B) (3 C) (4 D))



;;SEPARATE-LISTS-BY-SUBLISTS
;;2017
;;ddd
(defun separate-lists-by-sublists (after-nth list-of-lists  &key (make-new-sublists-p T))
  "In U-lists, From eg. ((1 1 a b)(2 2 c d)) if after-nth= 0 ((1 1)(2 2)) ((a b)(c d)) if make-new-sublists-p= T; if NIL, (1 1 2 2)(a b c d))"
  (let
      ((newlist1)
       (newlist2)
       (n-lists 0)
       )
    (loop
     for list in list-of-lists
     do
     (when (and list (listp list))
       (incf n-lists)
     (let*
         ((butn (- (list-length list) after-nth 1))
          (list1 (butlast list butn))
          (list2 (nthcdr (+ after-nth 1) list))
          )
       (cond
        (make-new-sublists-p
         (setf newlist1 (append newlist1 (list list1))
               newlist2 (append newlist2 (list list2))))
        (t (setf newlist1 (append newlist1 list1)
               newlist2 (append newlist2 list2))))
       ;;end let, when, loop
       )))
    (values newlist1 newlist2)
    ;;end let, separate-lists-by-sublists
    ))
;;TEST
;;  (separate-lists-by-sublists 1 '((1  1  A B)(2  2 C D)(3 3 E F) (4 X (NEW) 77 88)))
;; works=  ((1 1) (2 2) (3 3) (4 X))    ((A B) (C D) (E F) ((NEW) 77 88))
;; (separate-lists-by-sublists 1 '((1  1  A B)(2  2 C D)(3 3 E F) (4 X (NEW) 77 88)) :make-new-sublists-p NIL)
;; works=  (1 1 2 2 3 3 4 X)    (A B C D E F (NEW) 77 88)


;;SORT-LISTS-BY-FUNCALL
;;2018-02
;;ddd
(defun sort-lists-by-funcall (function funcargs append-n unsorted-lists 
                                       &key splice-args-p
                                funcargs2 append-n2 splice-args2-p
                                (sorttest 'my-greaterp) (eqtest 'my-equal))
  "In   RETURNS (values  sorted-lists other-items)   Sorts unsorted-lists by items returned by function with unsorted-lists appended to funcargs at append-n.  If funcargs2, then appends them at append-n2.  If a splice, the splices an arglist into the args. eqtest is the test for matching the items for sorting."
  (let
      ((other-items)
       (sorted-lists )
       (other-items)
       (sorted-list-length1)  
       )       
    ;;1-If 
    (loop
     for sortlist in unsorted-lists
     do
     (cond
      ((and sortlist (listp sortlist))
       (multiple-value-bind (sorted-lists1 other-items1 sorted-list-length1)
             (append-sorted-lists-by-funcall function funcargs append-n 
                                             sortlist  sorted-lists :splice-args-p splice-args-p
                                :funcargs2 funcargs2 :append-n2  append-n2 
                                :splice-args2-p splice-args2-p
                                         ;;    :from-end from-end
                                         ;;    :no-add-duplicates-p no-add-duplicates-p
                                :sorttest sorttest :eqtest eqtest )
       (setf sorted-lists sorted-lists1)  ;;(append sorted-lists sorted-lists1))
       ;;(break "1")

       ;;(append-sorted-lists-by-funcall #'NTH '(1) 1 '(C 3)'((E 5)) :sorttest 'my-lessp) ;;(A 1)(B 2)(F 6)))
       ;;(append-sorted-lists-by-funcall #'NTH '(1) 1  '(B 2) '((E 5)  (C 3) (A 1)))
    
       ;;end mvb, and
       ))
      (t (setf other-items (append other-items (list sortlist)))))
     ;;end  loop
     )
#|    (setf sorted-lists sorted-lists1
          other-items other-items1)|#

    (values  sorted-lists other-items)
    ;;end let, defun
    ))
;;TEST
;; (sort-lists-by-funcall #'NTH '(1) 1 '((E 5)(C 3)(A 1)(B 2)(F 6)))
;; works= ((F 6) (E 5) (C 3) (B 2) (A 1))   NIL 
;; (sort-lists-by-funcall #'NTH '(1) 1 '((E 5)(C 3)(A 1)(B 2)(F 6)) :sorttest 'my-lessp)
;; works= ((A 1) (B 2) (C 3) (E 5) (F 6))  NIL






;;SORT-1NESTED-LISTS-BY-SUBLIST-FUNCALL
;;2018-02
;;ddd
(defun sort-1nested-lists-by-sublist-funcall (function funcargs append-n
                                                       list-of-1nested-lists
                                                       &key splice-args-p
                                                       funcargs2 append-n2 splice-args2-p
                                                       (sorttest 'my-greaterp) (eqtest 'my-equal)
                                                       ;;nthlist nth list-of-1nested-lists
                                                       sorted-list unsorted-list other-items)
  "In  U-lists. INPUTS a list of 1nested-lists, RETURNS (values  sorted-lists sorted-groupdata-lists n-groups other-items) new list sorted by order of nth-list, nth item nested-list in each list."
  (let
      ((n-groups (list-length list-of-1nested-lists))
       (groupdata-lists)
       (sorted-groupdata-lists)
       (sorted-lists)
       (selected-grp-n)
       (other-items)
       )
    (when (and list-of-1nested-lists (listp  list-of-1nested-lists))
      ;;1-MAKE GROUPDATA-LISTS ((groupn selectedn selected-value) group) ;;selectedn by 
      (loop
       for nested-group in list-of-1nested-lists
       for n from 0 to n-groups
       do
       (let*
           ((groupdata)
            #|       (testlist (nth nthlist testgroup))
       (testitem)
       (matchitem (nth nth (nth nthlist (car (last sorted-list)))))|#
            )
         (multiple-value-bind (selected ret-value ret-n restlists uncked-lists nthlist)
             (get-greatest/least-list-by-funcalled-items function funcargs append-n
                                                         nested-group  :splice-args-p splice-args-p
                                                       :funcargs2 funcargs2 :append-n2  append-n2 
                                                       :splice-args2-p splice-args2-p  :test sorttest)
           (setf groupdata (list n ret-value ret-n selected)
                 groupdata-lists (append groupdata-lists (list groupdata))
                 other-items (append other-items (list uncked-lists)))

           ;;(break "1")
           ;;(afout 'out (format nil "nested-group= ~A~%groupdata= ~A~%other-items= ~A~%" nested-group groupdata other-items ))

           ;;end mvb, let, loop, when
           )))
      ;;2- SORT THE GROUPDATA-LISTS BY ret-values
      (multiple-value-setq (sorted-groupdata-lists     )
          (sort-lists-by-funcall #'NTH '(1) 1 groupdata-lists   :sorttest sorttest))
#|                                 :splice-args-p splice-args-p
                                 :funcargs2 funcargs2 :append-n2  append-n2 
                                 :splice-args2-p splice-args2-p  :test sorttest))|#

      ;;3-MAKE SORTED-LISTS from sorted-groupdata-lists (using list n's)
      (loop
       for groupdata in sorted-groupdata-lists
       do
       (let*
           ((listn (car groupdata))
            (list (nth listn list-of-1nested-lists))
            )
         (setf sorted-lists (append sorted-lists (list list)))
         ;;end let, loop
         ))
      ;;end when
      )
    ;;(afout 'out (format nil "END sorted-lists= ~A~% unested-group=~A~%  other-items=~A" sorted-lists nested-group other-items))
    (values  sorted-lists sorted-groupdata-lists n-groups other-items)
    ;;end let, sort-1nested-lists-by-sublist-funcall
    ))
;;TEST
;; with identical inner lists
;;  (sort-1nested-lists-by-sublist-funcall #'NTH '(1) 1 '(((E 0.3)) ((D 0.7) (M 0.7) (P 0.7) (S 0.7)) ((C 0.1) (R 0.1))  ((B 0.2) (O 0.2)) ((A 0.4) (L 0.4) (Q 0.4))))
;; works= (((D 0.7) (M 0.7) (P 0.7) (S 0.7)) ((A 0.4) (L 0.4) (Q 0.4)) ((E 0.3)) ((B 0.2) (O 0.2)) ((C 0.1) (R 0.1)))            ((1 0.7 6 (S 0.7)) (4 0.4 4 (Q 0.4)) (0 0.3 0 (E 0.3)) (3 0.2 2 (O 0.2)) (2 0.1 2 (R 0.1)))      5    (NIL NIL NIL NIL NIL)
;; with different inner lists 
;; (sort-1nested-lists-by-sublist-funcall #'NTH '(1) 1 '(((E 0.3)) ((D 0.3) (M 0.7) (P 0.5) (S 0.4)) ((C 0.1) (R 0.0))  ((B 0.3) (O 0.2)) ((A 0.3) (L 0.1) (Q 0.4))))
;; works= (((D 0.3) (M 0.7) (P 0.5) (S 0.4)) ((A 0.3) (L 0.1) (Q 0.4)) ((E 0.3)) ((B 0.3) (O 0.2)) ((C 0.1) (R 0.0)))          ((1 0.7 1 (M 0.7)) (4 0.4 -2 (Q 0.4)) (0 0.3 0 (E 0.3)) (3 0.3 -2 (B 0.3)) (2 0.1 -2 (C 0.1)))          5     (NIL NIL NIL NIL NIL)
;; ;; with different inner lists and my-lessp
;; (sort-1nested-lists-by-sublist-funcall #'NTH '(1) 1 '(((E 0.3)) ((D 0.3) (M 0.7) (P 0.5) (S 0.4)) ((C 0.1) (R 0.0))  ((B 0.3) (O 0.2)) ((A 0.3) (L 0.1) (Q 0.4))) :sorttest 'my-lessp)
;; works= (((C 0.1) (R 0.0)) ((A 0.3) (L 0.1) (Q 0.4)) ((B 0.3) (O 0.2)) ((E 0.3)) ((D 0.3) (M 0.7) (P 0.5) (S 0.4)))      ((2 0.0 2 (R 0.0)) (4 0.1 0 (L 0.1)) (3 0.2 2 (O 0.2)) (0 0.3 0 (E 0.3)) (1 0.3 0 (D 0.3)))      5      (NIL NIL NIL NIL NIL)





;;APPEND-SORTED-LISTS-BY-FUNCALL
;;2018-02 here3
;;ddd
(defun append-sorted-lists-by-funcall (function funcargs append-n list sorted-lists 
                                &key splice-args-p
                                funcargs2 append-n2 splice-args2-p
                                (sorttest 'my-greaterp) (eqtest 'my-equal) 
                                other-items  from-end no-add-duplicates-p )
  "In U-lists, Appends a list to a  list pre-sorted by function (eg #'nth or #'get-key-value) funcargs are the function args and append-n is the nth place where list goes in funcargs.   RETURNS (values new-sorted-lists other-items sorted-list-length). CHECK/FIX  from-end. Test must be one of (my-greaterp > >= greaterp) or (my-lessp lessp < <=) NOTE: lessp makes ascending and greaterp descending lists. (first item is less or greater)"
  (let
      ((nth-list) 
       (nth-testlist)
      ;; (last-newlists)
       ;;(rev-sorted-lists (reverse sorted-lists))
       (sorted-list-length (list-length sorted-lists))
       (new-sorted-lists)
       (list-length (list-length list))
       (testitem)
       (listitem)
       (n-dupls 0)
       (newargs1) 
       (newargs2)
       )
    ;;find listitem using my-funcall
    (setf newargs1 
          (append-nth list append-n funcargs :splice-list-p splice-args-p ))
    (when funcargs2
      (setf newargs1 
            (append-nth list append-n funcargs :splice-list-p splice-args2-p )))
    (setf listitem (my-funcall-arglist function newargs1 :use-quoted-args-p T)) 

       ;;(break "begin")
    (cond
     ((null sorted-lists)
      (setf new-sorted-lists (list list)))
     (t
      (loop
       for testlist in sorted-lists  ;;rev-sorted-lists
       for n from 0 to sorted-list-length
       do
       (let
           ((testargs (append-nth testlist append-n funcargs))
            )
    ;;find testitem funcargs
    (setf newargs2
          (append-nth testlist append-n funcargs :splice-list-p splice-args-p ))
    (when funcargs2
      (setf newargs2
            (append-nth testlist append-n funcargs :splice-list-p splice-args2-p )))
    ;;find testitem and new uncked-lists
    (setf  testitem (my-funcall-arglist function newargs2 :use-quoted-args-p T))
         ;;(break "testitem")
         (cond
          ((or (not (listp testlist))(null testitem))
           (setf other-items (append other-items (list testlist))))
          ;;IF GREATER, PUT AT END OF LIST
          ((and no-add-duplicates-p (funcall eqtest list testlist)) ;;was (my-equal list testlist))
           (incf n-dupls)
           NIL)
          ((member sorttest '(my-greaterp  >  >= greaterp))
           (cond
            ((funcall sorttest listitem testitem )       ;;was (my-greaterp listitem testitem)
             ;;(break "greaterp")
             (setf new-sorted-lists (append new-sorted-lists (butlast new-sorted-lists n) 
                                            (list list )
                                            (nthcdr n sorted-lists)))
             (return)
             ;;(break "in append 1")
             )
            (t
             (setf new-sorted-lists (append new-sorted-lists (list testlist)))
             (when  (= n (- sorted-list-length 1))
               (setf new-sorted-lists (append new-sorted-lists (list list))))))
               ;;end member greaterp
               )
          ;;IF LESSP, PUT AHEAD IN LIST
          ((member sorttest '(my-lessp lessp <  <=))
           (cond
            ((funcall sorttest listitem testitem)       ;;was (my-greaterp listitem testitem)
             ;;(break "lessp")
             (cond
              ((> (list-length new-sorted-lists) 0)
               (setf new-sorted-lists (append  new-sorted-lists
                                               (list list )
                                               (nthcdr n sorted-lists)))
               ;;(break "2a")
               (return))
              (t (setf new-sorted-lists (append  (list list) sorted-lists))
                 ;;(break "2b")
                 (return)))
             ;;(break "in append 1")
             ;;end funcall
             )
            (t
             (cond
              ((= n (- sorted-list-length 1))
               (setf new-sorted-lists (append  new-sorted-lists 
                                              (list testlist list)))
              ;;(break "3")
               )
              (t 
             (setf new-sorted-lists (append new-sorted-lists (list testlist)))
             ;;(break "4")
             ))))
               ;;end member
               )        
          (t  (setf new-sorted-lists (list "ERROR: TEST MUST BE ONE OF (my-greaterp > >= greaterp) OR (my-lessp lessp < <=)"))))
         ;;(afout 'out (format nil "testlist= ~A~%new-sorted-lists= ~A%other-items= ~A" testlist new-sorted-lists other-items))
         ;;end let, loop, t, cond
         ))))
#|    (when last-newlists
      (setf  new-sorted-lists (append new-sorted-lists last-newlists)))|#
    (values new-sorted-lists other-items (+ sorted-list-length  (- 1 n-dupls)))
    ;;end let, append-sorted-lists-by-funcall
    ))
;;TEST
;; (setf testxxx '(c 3) sortedtestlistxx '((a 1)(b 2)(d 4))) )
;; (append-sorted-lists-by-funcall #'nth '(1) 1 testxxx sortedtestlistxx :sorttest 'my-lessp)
;; works= ((A 1) (B 2) (C 3) (D 4))   NIL   4
;; (append-sorted-lists-by-funcall #'nth '(1) 1 '(c 3)  '((a 1)(b 2)(d 4)) :sorttest 'my-lessp)
;; works= ((A 1) (B 2) (C 3) (D 4))   NIL   4
;;(append-sorted-lists-by-funcall #'nth '(1) 1 '(a 1)  '((b 2)(c 3)(d 4)) :sorttest 'my-lessp)
;; (append-sorted-lists-by-funcall #'nth '(1) 1 '(c 3)  '((e 5)(d 4)(b 2)(a 1)))
;; works= ((E 5) (D 4) (C 3) (B 2) (A 1))  NIL  5
;; ;; (append-sorted-lists-by-funcall #'nth '(1) 1 '(a 1)  '((e 5)(c 3))) ;;(d 4)(b 2)(a 1)))
;; works= ((E 5) (C 3) (A 1))  NIL  3
;; (append-sorted-lists-by-funcall #'NTH '(1) 1  '(B 2) '((E 5)  (C 3) (A 1)))
;; works= ((E 5) (C 3) (B 2) (A 1))  NIL  4

;; (append-sorted-lists-by-funcall #'NTH '(1) 1  '(F 6) '((E 5) (C 3) (B 2)(A 1))  )  
;; works= ((F 6) (E 5) (C 3) (B 2) (A 1))  NIL  5
;; (append-sorted-lists-by-funcall #'NTH '(1) 1 '(B 2)    '((A 1) (C 3) (E 5)) :sorttest 'my-lessp)
;;works= ((A 1) (B 2) (C 3) (E 5))  NIL  4
;; (append-sorted-lists-by-funcall #'NTH '(1) 1  '(F 6)   '(  (A 1) (B 2)(C 3) (E 5)) :sorttest 'my-lessp)
;;works = ((A 1) (B 2) (C 3) (E 5) (F 6))   NIL   5
;; (append-sorted-lists-by-funcall #'NTH '(1) 1 '(A 4 C) '((2 0 6)(X 1)(A 3)))


;;OLDER VERSION??? ===================
;; COMPARE TO ABOVE IF IT NOT RIGHT
#|(defun append-sorted-lists-by-funcall (function funcargs append-n 
                                                list sorted-lists 
                                &key other-items  from-end no-add-duplicates-p 
                                (sorttest 'my-greaterp) (eqtest 'my-equal) )
  "In U-lists, Appends a list to a  list pre-sorted by function (eg #'nth or #'get-key-value) funcargs are the function args and append-n is the nth place where list goes in funcargs.   RETURNS (values new-sorted-lists other-items sorted-list-length). CHECK/FIX  from-end. Test must be one of (my-greaterp > >= greaterp) or (my-lessp lessp < <=)"
  (let
      ((nth-list) 
       (nth-testlist)
      ;; (last-newlists)
       ;;(rev-sorted-lists (reverse sorted-lists))
       (sorted-list-length (list-length sorted-lists))
       (new-sorted-lists)
       (list-length (list-length list))
       (testitem)
       (listitem)
       (n-dupls 0)
       (newargs (append-nth list append-n funcargs))
       )
    (setf listitem (my-funcall-arglist function newargs :use-quoted-args-p T)) ;; (nth nth list)))
    ;; (t (setf listitem (nth (- list-length nth 1) list))))
       ;;(break "begin")
    (cond
     ((null sorted-lists)
      (setf new-sorted-lists (list list)))
     (t
      (loop
       for testlist in sorted-lists  ;;rev-sorted-lists
       for n from 0 to sorted-list-length
       do
       (let
           ((testargs (append-nth testlist append-n funcargs))
            )
         (setf testitem (my-funcall-arglist function testargs :use-quoted-args-p T))
         ;;(break "testitem")
         (cond
          ((or (not (listp testlist))(null testitem))
           (setf other-items (append other-items (list testlist))))
          ;;If greater, put at end of list
          ((and no-add-duplicates-p (funcall eqtest list testlist)) ;;was (my-equal list testlist))
           (incf n-dupls)
           NIL)
          ((member sorttest '(my-greaterp > >= greaterp))
           (cond
            ((funcall sorttest listitem testitem)       ;;was (my-greaterp listitem testitem)
           ;;(break "greaterp")
           (setf new-sorted-lists (append  (butlast sorted-lists  n) (list list)
                                            (nthcdr n  sorted-lists)))
           ;;(break "1")
           (return)
           )
            (t
             (setf new-sorted-lists (append new-sorted-lists (list list)))))
               ;;end member
               )
          ((member sorttest '(my-lessp lessp < <=))
           (cond
            ((funcall sorttest listitem testitem)       ;;was (my-greaterp listitem testitem)
           ;;(break "greaterp")
           (setf new-sorted-lists (append  (butlast sorted-lists (- n 1)) (list list)
                                            (nthcdr  n sorted-lists)))
           ;;(break "1")
           (return)
           )
            (t
             (setf new-sorted-lists (append new-sorted-lists (list list)))))
               ;;end member
               )
          
          (t  (setf new-sorted-lists (list "ERROR: TEST MUST BE ONE OF (my-greaterp > >= greaterp) OR (my-lessp lessp < <=)"))))
         ;;(afout '*out (format nil "testlist= ~A~%new-sorted-lists= ~A%other-items= ~A" testlist new-sorted-lists other-items))
         ;;end let, loop, t, cond
         ))))
#|    (when last-newlists
      (setf  new-sorted-lists (append new-sorted-lists last-newlists)))|#
    (values new-sorted-lists other-items (+ sorted-list-length  (- 1 n-dupls)))
    ;;end let, append-sorted-lists-by-funcall
    ))|#




;;GET-GREATEST/LEAST-LIST-BY-FUNCALLED-ITEMS
;;2018
;;ddd
(defun get-greatest/least-list-by-funcalled-items (function funcargs list-n 
                                                            uncked-lists
                                                            &key itemlist restlists (nthlist  0) ret-n 
                                                            (test 'my-greaterp) 
                                                            splice-args-p 
                                                            funcargs2 list-n2 splice-args2-p)
  "In U-lists In uncked-lists, compares items found by [function funcargs list-nth] at list-n with uncked-lists.  The test used to compare item is test.  RETURNS  (values selected ret-value ret-n  restlists uncked-lists nthlist) ret-n is n from 0 of the selected list. nthlist is length of restlists.  Note: Funcargs2 (unlike funcargs) is a LIST of args that is appended at list-n2."
  (let*
      ((testlist)   
       (testitem) 
       (item)
       (selected)
       (newargs1)
       (newargs2)
       (ret-value)
       )
    (when (null itemlist)
      (setf itemlist (car uncked-lists)
            uncked-lists (cdr uncked-lists)))

    ;;find item using my-funcall
    (setf newargs1 
          (append-nth itemlist list-n funcargs :splice-list-p splice-args-p ))
    (when funcargs2
      (setf newargs1 (append-nth itemlist list-n funcargs :splice-list-p splice-args2-p )))
    (setf item (my-funcall-arglist function newargs1 :use-quoted-args-p T))
    
    ;;FIND TESTLIST
    (cond
     (uncked-lists
      (setf  testlist (car uncked-lists))
      ;;find testitem funcargs
      (setf newargs2
            (append-nth testlist list-n funcargs :splice-list-p splice-args-p ))
      (when funcargs2
        (setf newargs2
              (append-nth testlist list-n funcargs :splice-list-p splice-args2-p )))
      ;;find testitem and new uncked-lists
      (setf  testitem (my-funcall-arglist function newargs2 :use-quoted-args-p T)
             ;;(nth nthitem testlist)
             uncked-lists (cdr uncked-lists))
      ;;(break "1")
      (cond
#|   not needed    ((null testlist)
        (setf selected itemlist
              ret-value item))|#
       ((funcall test item testitem)
        ;;(afout 'out (format nil "1 PASSED-TEST testitem= ~A item= ~A" testitem item))
        (setf restlists (append restlists (list testlist))
              ret-n (- nthlist 2))
        ;; uncked-lists (cdr uncked-lists)
        (multiple-value-setq (selected ret-value ret-n restlists uncked-lists nthlist)
            (get-greatest/least-list-by-funcalled-items  function funcargs 
                                                         list-n  uncked-lists
                                                         :itemlist  itemlist :restlists restlists :nthlist (incf nthlist)
                                                         :ret-n ret-n :test test)))
       ;;(find-greatest/least-1nested-list  nthitem uncked-lists :test test :itemlist itemlist :restlists restlists :nthlist (incf nthlist) :ret-n ret-n)))
       (t (setf restlists (append restlists (list itemlist)))
          (incf nthlist)
          ;;ret-n (+ nthlist 1))
          ;;(afout 'out (format nil "2 FAILED-TEST testitem= ~A item= ~A" testitem item))
          ;;uncked-lists (cdr uncked-lists)
          (multiple-value-setq (selected ret-value ret-n restlists uncked-lists nthlist)
              (get-greatest/least-list-by-funcalled-items  function funcargs 
                                                           list-n  uncked-lists
                                                           :itemlist  testlist :restlists restlists :nthlist (incf nthlist)
                                                           :ret-n ret-n :test test))))
      #|(multiple-value-setq (selected ret-n restlists uncked-lists nthlist)
               (find-greatest/least-1nested-list nthitem  uncked-lists :test test
                                                 :itemlist testlist :restlists restlists :nthlist (incf nthlist) :ret-n ret-n))))|#
      ;;(afout 'out (format nil "END selected= ~A ret-value=~A ret-n= ~A~% restlists= ~A ~% uncked-lists nthlist= ~A " selected ret-value ret-n restlists uncked-lists nthlist))
      ;;end clause
      )
     (t (setf selected itemlist
              ret-value item)
        (unless ret-n (setf ret-n nthlist))))
    (values selected  ret-value ret-n restlists uncked-lists nthlist)
    ;;end let, get-greatest/least-list-by-funcalled-items
    ))
;;TEST
;;For 1nested list
;; (get-greatest/least-list-by-funcalled-items #'nth '(1) 1  '((C 3)(D 4)(A 1)(B 2)(X 9)(Y 7)(P 1)(O 8)) :test 'my-lessp)
;; works= (P 1)  1  6   ((D 4) (C 3) (B 2) (X 9) (Y 7) (A 1) (O 8))  NIL    9
;;(get-greatest/least-list-by-funcalled-items #'nth '(1) 1  '((C 3)(D 4)(A 1)(B 2)(X 9)(Y 7)(P 1)(O 8)))
;;works= (X 9) 9 4   ((C 3) (A 1) (B 2) (D 4) (Y 7) (P 1) (O 8))   NIL  7
;;(get-greatest/least-list-by-funcalled-items #'nth '(1) 1  '((C 3)))
;; for a key
;; ;;(get-greatest/least-list-by-funcalled-items #'get-key-value '(k2) 1 '((k1 8 k2 4)(k1 5 k2 7)(k1 2)(k2 6)(k1 5 k2 9)(k1 6 k2 3)))
;;works= (K1 5 K2 9) 9  4   ((K1 8 K2 4) (K1 2) (K2 6) (K1 5 K2 7) (K1 6 K2 3))   NIL  7
;;For 2nested list 
;;NOTE: FOLLOWING ONLY CHECKS FIRST OCCURANCE OF K2 if several k2 lists in same overall list???
;; (get-greatest/least-list-by-funcalled-items #'get-keyvalue-in-nested-list '(((k2 T))) 1 '(((k1 8 k2 4)(k1 5 k2 7))((k1 2)(k2 6))((k1 5 k2 9)(k1 6 k2 3))))
;; works= ((K1 5 K2 9) (K1 6 K2 3))  NIL  (((K1 8 K2 4) (K1 5 K2 7)) ((K1 2) (K2 6)))   NIL  4




;;APPEND-SORTED-LISTS
;;
;;ddd
(defun append-sorted-lists (append-n list sorted-lists &key (sorttest 'my-greaterp)
                                    (eqtest 'my-equal) other-items from-end no-add-duplicates-p)
  "U-lists, USES append-sorted-lists-by-funcall. Older version did not.
In U-lists, Appends a list to a  pre-sorted list where the list is compared to the other lists by the nth append-n item.  RETURNS (values new-sorted-lists other-items sorted-list-length). CHECK/FIX  from-end. Test must be one of (my-greaterp > >= greaterp) or (my-lessp lessp < <=) NOTE: lessp makes ascending and greaterp descending lists. (first item is less or greater)"
  (multiple-value-bind (sorted-lists other-items length-newlist)
      (append-sorted-lists-by-funcall #'nth (list append-n)  2  list sorted-lists 
                                      :splice-args-p  NIL :funcargs2 NIL 
                                     :sorttest sorttest :eqtest eqtest 
                                     :other-items other-items :from-end from-end
                                     :no-add-duplicates-p no-add-duplicates-p)
    (values sorted-lists other-items length-newlist)
    ;;end mvb, append-sorted-lists
    ))
;;TEST
;; (append-sorted-lists 1 '(A 4 C) '((2 0 6)(X 1)(A 3)) :sorttest 'my-lessp)
;; works= ((2 0 6) (X 1) (A 3) (A 4 C))   NIL 4
;; (append-sorted-lists 1 '(A 4 C) '((2 0 6)(X 1)(A 3)(2 5 u)(r 7 m)))
;; works= ((2 0 6) (X 1) (A 3) (A 4 C) (2 5 U) (R 7 M))  NIL  6
;; ;; (append-sorted-lists 1 '(A 4 C)  NIL)
;; works= ((A 4 C))  NIL  1
;; (append-sorted-lists 1 '(X 1)  '((R 7 M)) ) 
;; works= ((X 1) (R 7 M))  NIL  2
;; from-end
;; (append-sorted-lists 1 '(A 4 C) '((2 0 6)(2 5 u)(r 7 m)(A 3)(X 1)) :from-end T )
;; works= ((2 0 6) (A 4 C) (2 5 U) (R 7 M) (A 3) (X 1))    NIL   6
;;
;; (append-sorted-lists  0  '(z 9  l) '((a 3  x)) :from-end t)   (z 9 l)(m 1 mm)(a  5  nn))
;; works= ((Z 9 L) (A 3 X))   NIL  2
;;NO-ADD-DUPLICATES-P
;; (append-sorted-lists 1 '(A 4 C) '((2 0 6)(X 1)(A 3)(A 4 C)(2 5 u)(r 7 m)) :NO-ADD-DUPLICATES-P t)
;; works= ((2 0 6) (X 1) (A 3) (A 4 C) (2 5 U) (R 7 M)) NIL 6





;;SORT-KEYLISTS-BY-KEY
;;
;;ddd
(defun sort-keylists-by-key (key-nth keylists &key (sort-dim-n 1)
                                     (sort-groups 'ascending))
  "In U-lists GROUPS keylists by same dim-n number or letter. RETURNS (values ascending-lists descending-lists labels-list). (key-nth begins w/ 0).  sort-groups-p causes groups to be sorted either 'ascending or 'descending by sort-dim-n. "
  (let
      ((label)
       (labels-list)
       (new-group)
       (grouped-keylists)
       (new-grouped-keylists)
       (labeled-groups)
       (group-found-p)
       (ascending-p)
       (descending-group)(ascending-group)
       )
    (loop
     for dimlist in keylists
     do
     (setf  label (nth key-nth dimlist)
            labels-list (append labels-list (list label)))
     ;;(afout 'out (format nil "label= ~A" label))
 
     (cond
      (grouped-keylists
       (loop
        for group in grouped-keylists
        do
        ;;(afout 'out (format nil "group= ~A" group))

        (cond
         ((get-key-value-in-nested-lists `((,label ,key-nth)) group)
          ;;(break)
          (setf new-group (append group (list dimlist))   ;;eg ((1 3 2)(2 3 2))
                grouped-keylists (replace-list-item  new-group group grouped-keylists)
                group-found-p T)
          )
         (t nil))
         ;;  (replace-list-item  '((1 3 2)(2 3 2)) '((1 3 2)) '(((1 3 2))))
#|         (t (setf new-group   (list dimlist)
                  grouped-keylists (append grouped-keylists (list new-group)))))|#

         ;;(afout 'out (format nil "group= ~A new-group= ~A grouped-keylists= ~A" group new-group grouped-keylists))               
        ;;(break)
        (setf new-group nil)
        ;;end inner loop
        )
       ;;end grouped-keylists
       )
      (t nil))
         ;(setf grouped-keylists (list (list dimlist)))))
    (cond
     ((null group-found-p)
      (setf grouped-keylists (append grouped-keylists (list (list dimlist)))))
     (t (setf group-found-p nil)))
            
       
       ;;(break)
     ;;end outer loop
     )   
    ;;TO SORT WITHIN EACH GROUP (by sort-dim-n) ascending or descending
    (when sort-groups
      (when (equal sort-groups 'ascending)
        (setf ascending-p T))
      (loop
       for group in grouped-keylists
       do
       (multiple-value-setq (descending-group ascending-group)
            (my-sort-lists  (- sort-dim-n 1) group :ascending-p ascending-p))
       (cond
        ((equal sort-groups 'ascending)
             (setf new-grouped-keylists
                   (append new-grouped-keylists (list ascending-group))))
        (t (setf new-grouped-keylists
                   (append new-grouped-keylists (list descending-group)))))
       )
      (setf grouped-keylists new-grouped-keylists))

      (values grouped-keylists  labels-list)
    ;;end let, sort-keylists-by-key
    ))
;;TEST
;; key nth= 0 sort-dim-n= 1 (begins at 1?)
;;  (sort-keylists-by-key 0  '((1 3 2) (2 3 2) (3 3 2) (4 3 2) (1 4 2) (2 4 2) (3 4 2)))
;; works = (((1 4 2) (1 3 2)) ((2 4 2) (2 3 2)) ((3 4 2) (3 3 2)) ((4 3 2)))   (1 2 3 4 1 2 3)
;;  (sort-keylists-by-key 0 '((L (1 2 3))(F (3 4 5))(L (6 78))))
;; works= (((L (6 78)) (L (1 2 3))) ((F (3 4 5))))   (L F L)





;;SORT-LIST-INTO-NESTED-LISTS
;;
;;ddd
(defun sort-list-into-nested-lists  (flat-list items-per-list &key preitems postitems
                                               pre-add-dim-n-p double-quote-nested-item-p reset-x-pixs)
  "In U-lists, if items-per-list= integer, sorts list into lists with that many items (remainder is in last list).  If items-per-list = list of integers, sort according to integers, if not enough integers puts rest of items in last list. Appends preitems to beginning of each dimlist, postitems to end of each dimlist. pre-add-dim-n-p adds dim-n at end of preitems and reset-x-pixs  (both ONLY work on ART formated flat lists).  reset-x-pixs is a list of begin-x-pix incr-x-pix"
  (let*
      ((nested-lists)
       (n-items (list-length flat-list))
       (n-lists)
       (sublist)
       (nth 0)
       (nth-sublist 0)
       (sublist-nth  0)
       (length-sublist)
       (x-pix)
       (x-pix0)
       (x-pix-incr)
       (dim-n)
       (length-nested-lists)
       ) 
    ;;convert  items-per-list = integer into a list of integers
    (when
        (integerp items-per-list)
      (setf n-lists (ceiling (/ n-items  items-per-list))
            ;;note: last list may have too many elements
            items-per-list (make-list n-lists :initial-element items-per-list)))

    (setf  length-sublist (nth nth-sublist items-per-list))

    (when reset-x-pixs
      (setf  x-pix-incr (second reset-x-pixs)
             x-pix0 (car reset-x-pixs)
             x-pix x-pix0))
          
    ;;SORT THE FLAT-LIST
    (loop
     for item in flat-list
     do
     (incf nth)
     (incf sublist-nth)

     ;;reset item pix values; eg.  item= (1 (40 110) WUP1-1 (1 2 2 to 4 1 3))
     (when reset-x-pixs  
       (multiple-value-bind (itemn xy  var vdims rest)
           (values-list item)
           (setf x-pix (+ x-pix x-pix-incr))
           (if rest (setf item (list itemn (list x-pix (second xy)) var vdims rest))
             (setf item (list itemn (list x-pix (second xy)) var vdims)))))

     ;;PREITEMS? (also set below)
     (when (= nth 1)
       (cond
        ((and preitems pre-add-dim-n-p)
         (setf dim-n (car item))
         (setf sublist (append preitems (list dim-n))))
        (preitems
         (setf sublist (list preitems)))
        (t (setf sublist nil))))
     ;;(afout 'out (format nil "1 sublist=~A~%nested-lists= ~A~%item= ~A~% " sublist nested-lists item))

     (cond
      ;;at last item in flat-list
      ((= nth  n-items)
       (if double-quote-nested-item-p
           (setf sublist (append-double-quoted-sublist sublist  item))
         ;;otherwise
       (setf sublist (append sublist (list item))))
       (setf nested-lists (append nested-lists (list sublist)))
       (return))
      ;;at last item in sublist
      ((= sublist-nth  length-sublist)
       (if double-quote-nested-item-p
           (setf sublist (append-double-quoted-sublist sublist  item))
         ;;otherwise
         (setf sublist (append sublist (list item))))
       (when postitems
         (setf sublist (append sublist postitems)))
       (setf nested-lists (append nested-lists (list sublist)))
   ;;(afout 'out (format nil "SET NESTED HERE  sublist=~A~%nested-lists= ~A~%item= ~A~%" sublist nested-lists item))    
  ;; (append-double-quoted-sublist   '(DIMLIST 1 ((1 (40 110) WUP1-1 (1 1))))  '(1 (40 110) WUP1-1 (1 1)))
       ;;reset sublist values
       (incf  nth-sublist)
       (setf  length-sublist (nth (- nth-sublist 1) items-per-list)
              sublist-nth 0)
       (setf x-pix x-pix0)
       ;;Appends preitems to beginning of each dimlist, postitems to end of each dimlist. pre-add-dim-n-p adds dim-n at end of preitems (ONLY works on ART formated flat lists).
       (cond
        ((and preitems pre-add-dim-n-p)
         (setf dim-n (car item))
         (setf sublist (append preitems (list (+ dim-n 1 ))))
         ;;(break)
               ) 
        (preitems
         (setf sublist (append preitems)))
        (t (setf sublist nil)))
       ;;end last item clause
       )
      ;;otherwise append sublist
      (t 
       (if double-quote-nested-item-p
           (setf sublist (append-double-quoted-sublist sublist  item))
         ;;otherwise
       (setf sublist (append sublist (list item))))
       ;;end last t, cond
       ))
     ;;(afout 'out (format nil "2-end LOOP  sublist=~A~%nested-lists= ~A~%" sublist nested-lists))
     ;;end loop
     )
    nested-lists
    ;;end let, sort-list-into-nested-lists
    ))
;;TEST
;;  (sort-list-into-nested-lists '(1 2 3 4 5 6 7 8 9 10 11)  3) = ((1 2 3) (4 5 6) (7 8 9) (10 11))
;;   (sort-list-into-nested-lists '(1 2 3 4 5 6 7 8 9 10 11 12)  3) = ((1 2 3) (4 5 6) (7 8 9) (10 11 12))    
;;   (sort-list-into-nested-lists '(1 2 3 4 5 6 7 8 9 10 11)  '(2 4 1 3)) = ((1 2) (3 4 5 6) (7) (8 9 10) (11))
;;FOR ART FORMAT TO make-graph-line
;;newest   old (1 (40 110) WUP1-1 (1 1))
;;  
;; ;;   (sort-list-into-nested-lists  '((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1))(1 (40 50) WUP1-2 (1 2)) (2 (80  20) WUP2-2 (2 2))) 2 :pre-add-dim-n-p T :preitems '(:DIMSIM)  :double-quote-nested-item-p T )
;;works= ((:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1)))) (:DIMSIM 2 ((1 (40 50) WUP1-2 (1 2)) (2 (80 20) WUP2-2 (2 2)))))
;; (sort-list-into-nested-lists  '((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1))(1 (40 50) WUP1-2 (1 2)) (2 (80  20) WUP2-2 (2 2))) 2 :pre-add-dim-n-p T :preitems '(:DIMSIM)  :double-quote-nested-item-p T :reset-x-pixs  '(0 40))
;;((:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1)))) (:DIMSIM 2 ((1 (40 50) WUP1-2 (1 2)) (2 (80 20) WUP2-2 (2 2)))))

;; ;;   (sort-list-into-nested-lists  '((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1))(1 (40 50) WUP1-2 (1 2)) (2 (80  20) WUP2-2 (2 2))) 2)
;; works= (((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1))) ((1 (40 50) WUP1-2 (1 2)) (2 (80 20) WUP2-2 (2 2))))


#|
CL-USER 65 > (truncate (/ 9  4 )) = 2   1/4
CL-USER 66 > (truncate (/ 9 (* 4 1.0))) = 2  0.25
CL-USER 68 > (ceiling (/ 9  4)) = 3  -3/4
|#


;;RESORT-NESTED-LISTS-BY-N
;;
;;ddd
(defun resort-nested-lists-by-n (nested-list &key n-lists n-items-per-list)
  "In U-lists, RETURNS (values resorted-list n-nested-lists). Any degree of nesting ok. Sorts EITHER BY n-lists OR n-items-per-list. If both specified, sorts by n-lists with n-items-per-list and puts rest items in rest-list. If left over items in any sorting, puts items in rest-list. RETURNS  sorted-list.  "
  (let*
      ((resorted-list)
       (flat-list (flatten-count-nested-lists nested-list))
       (flat-list-length (list-length flat-list))
       (n-nested-lists)
       (n-items)
       )
   (when (listp flat-list)
   (cond
    (n-lists
     (cond
      (n-items-per-list
       (setf n-items n-items-per-list))
      (t (setf n-items  (ceiling (/ flat-list-length  n-lists)))))
     ;;(afout 'out (format nil "n-items= ~A" n-items))
     (setf resorted-list (sort-list-into-nested-lists flat-list n-items)
           n-nested-lists (list-length resorted-list))
     ;;end n-lists
     )
    (n-items-per-list
     (setf resorted-list (sort-list-into-nested-lists flat-list n-items-per-list)
           n-nested-lists (list-length resorted-list)))
    (t nil))
   ;;end when
   )
   (values resorted-list n-nested-lists)
   ;;end let, resort-nested-lists-by-n
   ))
;;TEST
;;  (resort-nested-lists-by-n '((1 2 3)((4 5) 6 7) 8 9 (10 11)) :n-lists 3)
;; works= ((1 2 3 4) (5 6 7 8) (9 10 11))  3
;;  (resort-nested-lists-by-n '((1 2 3)((4 5) 6 7) 8 9 (10 11)) :n-items-per-list 5)
;; works= ((1 2 3 4 5) (6 7 8 9 10) (11))  3



;;RESORT-NESTED-LISTS
;;
;;ddd
(defun resort-nested-lists (nested-lists &key (max-n-items 500))
  "In U-lists, takes a list of 2-level nested lists and resorts them so that all first items are in one list, all second items in another, etc. max-n-items means max length of longest sublist."
  (let*
      ((item)
       (newlist)
       (sorted-list)
       (length-nested-lists (list-length nested-lists))
       )
    (loop
     for n from 0 to max-n-items
     do     
    (loop
     for list in nested-lists
     ;;for listn from 1 to length-nested-lists
     do
     (setf item (nth n list))
     (when item
       (setf newlist (append newlist (list item))))
     ;;end inner loop
     )    
    ;;reset newlist or append to sorted-list
    (cond
     ((null newlist)
      (return))
     (t (setf sorted-list (append sorted-list (list newlist))
              newlist nil)))
    ;;end outer loop
    )
    sorted-list
    ))
;;TEST
;; ;; (resort-nested-lists '(((LIST1 1 1)(LIST11 11 11)(LIST111 111 111 111))  ((LIST2 2 2 2)(LIST22 22 22))    ((LIST3 3 3 3 3)(LIST33 33 33)(LIST333 333 333 333))  ) )
;; works= (((LIST1 1 1) (LIST2 2 2 2) (LIST3 3 3 3 3)) ((LIST11 11 11) (LIST22 22 22) (LIST33 33 33)) ((LIST111 111 111 111) (LIST333 333 333 333))) 
;;
;;   (resort-nested-lists '(("testC1FC1F" "testC1FC2F" "testC1FC3F") ("testC2FC1F" "testC2FC2F" "testC2FC3F") ("testC3FC1F" "testC3FC2F" "testC3FC3F") ("testC4FC1F" "testC4FC2F" "testC4FC3F")))
;; WORKS= (("testC1FC1F" "testC2FC1F" "testC3FC1F" "testC4FC1F") ("testC1FC2F" "testC2FC2F" "testC3FC2F" "testC4FC2F") ("testC1FC3F" "testC2FC3F" "testC3FC3F" "testC4FC3F"))

;;NOTE: MUST RESORT POST-DIMLIST REMOVED LISTS
;;;;   (resort-nested-lists  '(((1 (40 0.070434004) "Wup1-1" (1 1)) (1 (80 0.06954) "Wup1-2" (1 2)) (1 (120 0.026330002) "Wup1-3" (1 3)) (1 (160 0.038548) "Wup1-4" (1 4)) (1 (200 0.112005) "Wup1-5" (1 5))) ((2 (40 0.08712201) "Wup2-1" (2 1)) (2 (80 0.0074070008) "Wup2-2" (2 2)) (2 (120 0.082205005) "Wup2-3" (2 3)) (2 (160 0.017092) "Wup2-4" (2 4)) (2 (200 0.063878) "Wup2-5" (2 5))) ((3 (40 0.100085005) "Wup3-1" (3 1)) (3 (80 0.009195) "Wup3-2" (3 2)) (3 (120 0.13808) "Wup3-3" (3 3)) (3 (160 0.096807) "Wup3-4" (3 4)) (3 (200 0.08280101) "Wup3-5" (3 5))) ((4 (40 0.032587998) "Wup4-1" (4 1)) (4 (80 0.076096006) "Wup4-2" (4 2)) (4 (120 0.022009) "Wup4-3" (4 3)) (4 (160 0.097701006) "Wup4-4" (4 4)) (4 (200 0.05166) "Wup4-5" (4 5))) ((5 (40 0.107535005) "Wup5-1" (5 1)) (5 (80 0.009791001) "Wup5-2" (5 2)) (5 (120 0.057769) "Wup5-3" (5 3)) (5 (160 0.046295997) "Wup5-4" (5 4)) (5 (200 0.056875) "Wup5-5" (5 5))) ((6 (40 0.046445) "Wup6-1" (6 1)) (6 (80 0.07848) "Wup6-2" (6 2)) (6 (120 0.024542002) "Wup6-3" (6 3)) (6 (160 0.048978) "Wup6-4" (6 4)) (6 (200 0.104704) "Wup6-5" (6 5))) ((7 (40 0.023946) "Wup7-1" (7 1)) (7 (80 0.11111101) "Wup7-2" (7 2)) (7 (120 0.07505301) "Wup7-3" (7 3)) (7 (160 0.011728) "Wup7-4" (7 4)) (7 (200 0.022009) "Wup7-5" (7 5))) ((8 (40 0.0033840002) "Wup8-1" (8 1)) (8 (80 0.019178002) "Wup8-2" (8 2)) (8 (120 0.12347801) "Wup8-3" (8 3)) (8 (160 0.012324) "Wup8-4" (8 4)) (8 (200 0.047934998) "Wup8-5" (8 5))) ((9 (40 0.026181002) "Wup9-1" (9 1)) (9 (80 0.046147) "Wup9-2" (9 2)) (9 (120 0.126458) "Wup9-3" (9 3)) (9 (160 0.080417) "Wup9-4" (9 4)) (9 (200 0.13361001) "Wup9-5" (9 5)))))
;;WORKS= (((1 (40 0.070434004) "Wup1-1" (1 1)) (2 (40 0.08712201) "Wup2-1" (2 1)) (3 (40 0.100085005) "Wup3-1" (3 1)) (4 (40 0.032587998) "Wup4-1" (4 1)) (5 (40 0.107535005) "Wup5-1" (5 1)) (6 (40 0.046445) "Wup6-1" (6 1)) (7 (40 0.023946) "Wup7-1" (7 1)) (8 (40 0.0033840002) "Wup8-1" (8 1)) (9 (40 0.026181002) "Wup9-1" (9 1))) ((1 (80 0.06954) "Wup1-2" (1 2)) (2 (80 0.0074070008) "Wup2-2" (2 2)) (3 (80 0.009195) "Wup3-2" (3 2)) (4 (80 0.076096006) "Wup4-2" (4 2)) (5 (80 0.009791001) "Wup5-2" (5 2)) (6 (80 0.07848) "Wup6-2" (6 2)) (7 (80 0.11111101) "Wup7-2" (7 2)) (8 (80 0.019178002) "Wup8-2" (8 2)) (9 (80 0.046147) "Wup9-2" (9 2))) ((1 (120 0.026330002) "Wup1-3" (1 3)) (2 (120 0.082205005) "Wup2-3" (2 3)) (3 (120 0.13808) "Wup3-3" (3 3)) (4 (120 0.022009) "Wup4-3" (4 3)) (5 (120 0.057769) "Wup5-3" (5 3)) (6 (120 0.024542002) "Wup6-3" (6 3)) (7 (120 0.07505301) "Wup7-3" (7 3)) (8 (120 0.12347801) "Wup8-3" (8 3)) (9 (120 0.126458) "Wup9-3" (9 3))) ((1 (160 0.038548) "Wup1-4" (1 4)) (2 (160 0.017092) "Wup2-4" (2 4)) (3 (160 0.096807) "Wup3-4" (3 4)) (4 (160 0.097701006) "Wup4-4" (4 4)) (5 (160 0.046295997) "Wup5-4" (5 4)) (6 (160 0.048978) "Wup6-4" (6 4)) (7 (160 0.011728) "Wup7-4" (7 4)) (8 (160 0.012324) "Wup8-4" (8 4)) (9 (160 0.080417) "Wup9-4" (9 4))) ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (200 0.063878) "Wup2-5" (2 5)) (3 (200 0.08280101) "Wup3-5" (3 5)) (4 (200 0.05166) "Wup4-5" (4 5)) (5 (200 0.056875) "Wup5-5" (5 5)) (6 (200 0.104704) "Wup6-5" (6 5)) (7 (200 0.022009) "Wup7-5" (7 5)) (8 (200 0.047934998) "Wup8-5" (8 5)) (9 (200 0.13361001) "Wup9-5" (9 5))))



;;SORT-LISTS-BY-LENGTH
;;2016
;;ddd
(defun sort-lists-by-length (lists &key ascending-p not-return-sorted-newlist-p)
  "In U-lists RETURNS (values sorted-lists sorted-newlists first-list-length) where  NEWLIST= (length-list list)"
  (let
      ((sorted-newlists)
       (sorted-lists)
       (longest-list)
       (len-longest)
       (len-list)
       (newlists)
       )
    ;;CREATE NEW-LISTS of (len-list list)
    (loop
     for list in lists
     do
     (setf len-list (list-length list)
           newlists (append newlists (list (list len-list list))))
     )
    ;;SORT BY len-list
    (setf sorted-newlists (my-sort newlists #'test-greaterp :key 'car))
    (dolist (item sorted-newlists)
      (setf sorted-lists (append sorted-lists (cdr item))))
    
    (when ascending-p
      (setf sorted-lists (reverse sorted-lists)
            sorted-newlists (reverse sorted-newlists)))
    (cond
     (not-return-sorted-newlist-p 
      (values sorted-lists (caar sorted-newlists)))
     ;;end let, sort-lists-by-length
     (t (values sorted-lists sorted-newlists (caar sorted-newlists))))
    ))
;;TEST
;;  (sort-lists-by-length '((1 2 3 4) (a b c)(x y z w m)(7 8 9)))    
;; works= ((X Y Z W M) (1 2 3 4) (A B C) (7 8 9))      ((5 (X Y Z W M)) (4 (1 2 3 4)) (3 (A B C)) (3 (7 8 9)))  5
;; :ascending-p
;; (sort-lists-by-length '((1 2 3 4) (a b c)(x y z w m)(7 8 9)) :ascending-p T)
;; works= ((7 8 9) (A B C) (1 2 3 4) (X Y Z W M))  ((3 (7 8 9)) (3 (A B C)) (4 (1 2 3 4)) (5 (X Y Z W M)))   3
;; not-return-sorted-newlist-p
;; (sort-lists-by-length '((1 2 3 4) (a b c)(x y z w m)(7 8 9)) :not-return-sorted-newlist-p T)
;; works= ((X Y Z W M) (1 2 3 4) (A B C) (7 8 9))   5






;;APPEND-DOUBLE-QUOTED-SUBLIST
;;
;;ddd
(defun append-double-quoted-sublist  ( list item)
  "In U-lists. Appends a list of form (item1 item2 SUBLIST) in which sublist is a double-quoted list.  Appends only the sublist. Eg list= (:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)))). If last item NOT a list, the appends list with double-quoted item. "
  (let
      ((new-sublist)
       (new-list)
       (begin)
       (sublist)
       )
    (setf begin (butlast list)
          sublist (car (last list)))
    (cond
     ((listp sublist)
       (setf  new-sublist (append sublist (list item))
          new-list (append begin (list new-sublist))))
     (t (setf new-list (append list (list (list item))))))
    new-list
    ;;end let, append-double-quoted-sublist
    ))
;;TEST
;; (append-double-quoted-sublist '(:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)))) '(this list))
;; works= (:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)) (THIS LIST)))
;; (append-double-quoted-sublist '(:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)))) '(2 (80 210) WUP2-1 (2 1)))
;; works= (:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)) (2 (80 210) WUP2-1 (2 1))))
;; (append-double-quoted-sublist '(a b) '(this list)) = (A B ((THIS LIST)))

;;(append-double-quoted-sublist   '(DIMLIST 1 ((1 (40 110) WUP1-1 (1 1))))  '(1 (40 110) WUP1-1 (1 1)))

    




;;REMOVE-NTH-DUPLICATES
;;
;;ddd
(defun remove-nth-duplicates (nth lists &key  delete-largest-p (test 'my-equal))
  "In U-lists.  DELETE-DUPLICATES-NTH (if duplicate by delete-duplicates-nth item, deletes the item with smallest nth item (unless delete-largest-p). RETURNS (values new-list duplicates-list."
  (let
      ((new-list)
       (sorted-list.)
       (greaterp)
       (first-lists)
       ( item-deleted-p)
       (sorted-lists)
       (last-item)
       (ordered-list)
       (delete-duplicates-nth)
           )
    (loop
     for list in lists
     do
     (cond
      (sorted-list.
       (setf first-lists (butlast sorted-lists)
             last-item (car (last sorted-lists)))
       (multiple-value-setq (ordered-list greaterp)
           (my-list-nth-greaterp nth list last-item))

       ;;delete-duplicates-nth (if duplicate by delete-duplicates-nth item,
       ;; deletes the item with smallest nth item (unless delete-largest-p)
       (when delete-duplicates-nth
         (cond
          ((funcall test  (nth delete-duplicates-nth list)          ;;was (my-equal 
                     (nth delete-duplicates-nth last-item))
           (setf item-deleted-p t)
           (cond
            (delete-largest-p
             (setf ordered-list (second ordered-list)))
            (t  (setf ordered-list (first ordered-list))))
           ;;end my-equal clause
           )
          (t  nil))
         ;;end when delete-duplicates-nth
         )           

       ;;(afout 'out (format nil "1 list= ~A~% sorted-lists= ~A~%first-lists= ~A~%" list sorted-lists first-lists))
       ;;in any case
       (setf sorted-lists (append first-lists ordered-list))
       (cond
        (item-deleted-p NIL)
        (t
         (cond
          ((null greaterp) NIL)
          (t (setf sorted-lists (my-sort-lists nth sorted-lists)))))
        ;;end outer cond, sorted-lists clause
        ))
      (t (setf sorted-lists (list list))))
     ;;end loop
     )
    sorted-lists
    ;;end let, my-sort-lists
    ))


;;MY-LIST-NTH-LESSP
;;
;;ddd
(defun my-list-nth-lessp (nth list1 list2)
  "In U-lists RETURNS (values sorted-list result) where result is T if the nth item in list1 is my-lessp than the one in list2."
  (let
      ((sorted-list)
       (result)
       )
    (cond
     ((my-lessp (nth nth list1) (nth nth list2))
      (setf sorted-list (list list1 list2)
            result T))
     (t (setf sorted-list (list list2 list1)
              result NIL)))
    (values sorted-list result)
  ;;end let, my-list-nth-lessp
  ))
;;; TEST
;; (my-list-nth-lessp 1 '(A 2 XX) '(B 1 MM N)) = ((B 1 MM N) (A 2 XX)) NIL
;; (my-list-nth-lessp 0 '(A 2 XX) '(B 1 MM N)) = ((A 2 XX) (B 1 MM N))  T

;;MY-LIST-NTH-GREATERP
;;
;;ddd
(defun my-list-nth-greaterp (nth list1 list2 &key from-end)
  "In U-lists RETURNS (values sorted-list result) where result is T if the nth item in list1 is my-greater than the nth one in list2. ROBUST, works on even lists = nil or lists too short or not lists."
  (let
      ((sorted-list)
       (result)
       (nth1 nth)
       (nth2 nth)
       )
    (when (and from-end (and (listp list1)(listp list2)))
      (setf nth1 (- (list-length list1) nth 1)
            nth2 (- (list-length list2) nth 1)))
    ;;in case of nil lists
    (cond
     ((or (and (null list1)(null list2))
          (and (< nth1 0)(< nth2 0))
          (and (null (listp list1))(null (listp list2))))
      (setf result nil sorted-list (list nil nil)))
     ((or (null list1)(< nth1 0)(null (listp list1)))
      (setf result nil sorted-list (list list2 nil)))
     ((or (null list2)(< nth1 0) (null (listp list2)))
      (setf result list1 sorted-list (list list1)))
     (t
      ;;otherwise process normal lists    
      (cond
       ((my-greaterp (nth nth1 list1) (nth nth2 list2))
        (setf sorted-list (list list1 list2)
              result list1))
       (t (setf sorted-list (list list2 list1)
                result NIL)))
      ;;end T, cond
      ))
    (values sorted-list result)
  ;;end T, cond, let, my-list-nth-greaterp
  ))
;;; TEST
;; (my-list-nth-greaterp 1 '(A 2 XX) '(B 1 MM N)) = ((A 2 XX) (B 1 MM N)) T
;; (my-list-nth-greaterp 0 '(A 2 XX) '(B 1 MM N)) = ((B 1 MM N) (A 2 XX)) NIL
;; (my-list-nth-greaterp 2 '(A 2 XX) '(B 1 MM y)) = ((A 2 XX) (B 1 MM Y))  T
;; (my-list-nth-greaterp 0 '(A 2 XX) '(B 1 MM Y) :from-end t) = ((B 1 MM Y) (A 2 XX)) NIL
;;;; if one is nil
;;(my-list-nth-greaterp 2 '(A 2 XX) '(B 1 nil N)) = ((A 2 XX) (B 1 NIL N)) 
;;  (my-list-nth-greaterp 1 '(A 2 XX) nil) =((A 2 XX)) (A 2 XX)
;; (my-list-nth-greaterp 1 '(A 2 XX) nil :from-end t) = ((A 2 XX)) (A 2 XX)
;;  (my-list-nth-greaterp 0 nil  '(B 1 MM Y) :from-end t) = ((B 1 MM Y) NIL) NIL
;; (my-list-nth-greaterp 0 nil  '(B 1 MM Y)) = ((B 1 MM Y) NIL)  NIL
;; (my-list-nth-greaterp 3 '(A 2 XX) '(B 1 MM Y) :from-end t) = ((B 1 MM Y) NIL)  NIL
;;(my-list-nth-greaterp 3 '(A 2 XX) '(B 1 MM Y)) = ((B 1 MM Y) (A 2 XX)) NIL
;; (my-list-nth-greaterp 3 '(A 2 XX LL) '(B 1 MM)) = ((A 2 XX LL) (B 1 MM))   (A 2 XX LL)
;; (my-list-nth-greaterp 0 '(A 2 XX LL) 7) = ((A 2 XX LL)) (A 2 XX LL)
;; (my-list-nth-greaterp 2 '(A 2 XX LL) 7 :from-end t) = ((A 2 XX LL)) (A 2 XX LL)
;; (my-list-nth-greaterp 2 "ab" '(A 2 XX LL)  :from-end t) = ((A 2 XX LL) NIL)  NIL

;;MY-COMPARE-LIST-NTH
;;
;;ddd
(defun my-compare-list-nth (nth list1 list2 &key (test 'my-greaterp))
  "In U-lists RETURNS (values sorted-list result) where result is T if the nth item in list1 is TEST (usually 'my-greaterp or 'my-lessp than the one in list2. list1 and list2 MUST be 1-lists and 2-at least nth long. This is a SIMPLE,NON-ROBUST function. Use my-list-nth-greaterp or my-list-nth-lessp for unknown lists that may have nils, non-list items, or lists < nth long."
  (let
      ((sorted-list)
       (result)
       )
    (cond
     ((funcall test (nth nth list1) (nth nth list2))
      (setf sorted-list (list list1 list2)
            result T))
     (t (setf sorted-list (list list2 list1)
              result NIL)))
    (values sorted-list result)
  ;;end let, my-list-nth-greaterp
  ))
;;TEST
;; (my-compare-list-nth 1 '(a b c) '(x y z)) 
;; works = ((X Y Z) (A B C)) NIL
;; (my-compare-list-nth 1 '(a b c) '(x y z) :test 'my-lessp)
;; works = ((A B C) (X Y Z)) T
;; (my-compare-list-nth 1 '(1 2 3) '(x 0 z) :test 'my-lessp)
;; works = ((X 0 Z) (1 2 3)) NIL
;;
;; (funcall  #'my-greaterp "this" "apple") = 0
;;  (setf xx 'car) (funcall xx '(a b c)) = a






;;DELETE-ALL-DUPLICATE-NTH-LISTS
;;
;;ddd
(defun delete-all-duplicate-nth-lists (nth lists  &key compare-nth return-lessp)
  "In U-lists"
  (let
      ((return-list)
       (duplicates)
       (first-list)
       (rest-lists)
       (new-lists)       
       (newlist)
       ( result-list)
       (list1)
       (duplicate)
       (matchp)
     ;;;SSS START HERE FINISH THIS (NEED RECURSIVE ??)
       )
    (loop
     for n from 1 to (list-length lists)
     do
     (setf list1 (car lists)
           rest-lists (cdr lists))
     ;;(afout 'out (format nil "LOOP1 n= ~A list1= ~A~%, rest-lists=~A~%" n list1 rest-lists))
     (loop
      for list2  in rest-lists
      do 
      (multiple-value-setq (result-list duplicate matchp)
          (delete-duplicate-nth nth list1 list2  :compare-nth compare-nth
                                :return-lessp return-lessp)) 
     ;;(afout 'out (format nil "After mvs, result-list= ~A~% duplicate= ~A~%" result-list duplicate))
      (setf list1 (car result-list))
      (if duplicate (setf duplicates (append duplicates  duplicate)))
            
      ;;eliminate duplicates from original lists above
      (unless matchp (setf new-lists (append new-lists  (list (second result-list)))))
     ;;(afout 'out (format nil "LOOP2 list1= ~A~%matchp= ~A  new-lists= ~A~%" list1 matchp new-lists))                      
      ;;end loop
      )
     (if list1
         (setf newlist (append newlist (list list1))
               lists new-lists
               new-lists nil))
          ;;(afout 'out (format nil "AFTER LOOP2, newlist= ~A~%lists= ~A~%" newlist lists))
     ;;end loop
     )
    (values newlist duplicates)
    ;;end let, delete-all-duplicate-nth-lists
    ))
;;TEST
;;SSS START HERE DEBUGGING USE TO DELETE EXTRA HELP-LINKS
;;  (delete-all-duplicate-nth-lists 1 '((x a  3)(a a 4)(b u 3)(m d 6)(x m 0)))
;;  (delete-all-duplicate-nth-lists 1 '((x a  3)(a a 4)(b u 3)(m d 6)(x m 0)):compare-nth 2)
;;works= ((A A 4) (B U 3) (M D 6) (X M 0))    ((X A 3))

;;DELETE-ALL-DUPLICATE-NTH-LISTS
;;
;;ddd
#|(defun delete-all-duplicate-nth-listsOLD (nth lists  &key compare-nth return-lessp)
  "In U-lists"
  (let
      ((return-list)
       (duplicates-list)
       (first-list)
       (last-list)
       (newlist)
       )
    (loop
     for list in lists
     do
     (cond
      (return-list
       (loop
        for r-list in return-list
        do
        (multiple-value-setq (newlist duplicate matchp)
            (delete-duplicate-nth nth list r-list  :compare-nth compare-nth
                                  :return-lessp return-lessp))
          (setf return-list (append return-list newlist)
                duplicates-list (append duplicates-list duplicate))
          ;;end inner loop
          ))
       (t (setf return-list (list list))))
     ;;end outer loop
     )
    (values return-list duplicates-list)
    ;;end let, delete-all-duplicate-nth-lists
    ))|#

        
      




;;DELETE-DUPLICATE-NTH
;;
;;ddd
(defun delete-duplicate-nth (nth list1 list2 &key compare-nth return-lessp (test 'my-equal))
  "In U-lists. Compares same nth item in list of lists and deletes lists with duplicates. RETURNS (values new-list duplicate-list duplicatep) new-list is a list of both lists unless nth item is my-equal.  If compare-nth, then the one with the largest value of compare-nth will be returned (unless return-less-p)."
  (let
      ((new-list)
       (result)
       (duplicate-list)
       (duplicatep)
       (ordered-list)
       )
    (cond
     ((funcall test (nth nth list1)(nth nth list2)) ;;was(my-equal (nth nth list1)(nth nth list2))
      (setf duplicatep t)
      (cond
       (compare-nth
        (setf ordered-list  (my-list-nth-lessp compare-nth list1 list2))
        (cond
         (return-lessp
          (setf new-list (list (car ordered-list))
                duplicate-list (list (second ordered-list))))
         (t (setf new-list (list (second ordered-list))
                  duplicate-list (list (first ordered-list)))))
        ;;end compare-nth
        )
       (t (setf new-list (list list1)
                duplicate-list (list list2))))
      ;;ene my-equal
      )
     (t (setf new-list (list list1 list2))))
    (values new-list duplicate-list duplicatep)
    ;;end let, delete-duplicate-nth
    ))
;;TEST
;;  (delete-duplicate-nth 1 '(a b c d) '(one b 2 x))  = ((A B C D)) ((ONE B 2 X))  T
;;  (delete-duplicate-nth 0  '(a b c d) '(one b 2 x)) = ((A B C D) (ONE B 2 X))  NIL NIL
;;  (delete-duplicate-nth 1 '(a b c d) '(one b 2 x) :compare-nth 3) = (ONE B 2 X)  (A B C D) T
;;  (delete-duplicate-nth 1 '(a b c d) '(one b 2 x) :compare-nth 3 :return-lessp t) = (A B C D)  (ONE B 2 X) T


;;COMPARE-LISTS
;;
;;ddd
(defun compare-lists (list1 list2 &key return-unmatched-list2-p
                            (test 'my-equal))
  "In U-lists, compares lists.  Checks all items from list1 to see if they are members  to items in list2. RETURNS (values matched-items unmatched-list1-items"
  (let
      ((matched-items)
       (unmatched-list1-items)
       (matched2)
       (unmatched-list2-items)
       )
    (dolist (item list1)
      (cond
       ((member item list2 :test test)
        (setf matched-items (append matched-items (list item))))
       (t
        (setf unmatched-list1-items (append unmatched-list1-items 
                                            (list item)))))
      ;;end dolist
      )
    (when return-unmatched-list2-p
      (multiple-value-setq (matched2 unmatched-list2-items)
          (compare-lists list2 list1)))      
    (values matched-items  unmatched-list1-items unmatched-list2-items)
    ))
;;TEST
;;  (compare-lists '(a b c d e) '(a x b y e g h))
;; works= (A B E)   (C D)   NIL
;; find non-matched for BOTH LISTS
;;  (compare-lists '(a b c d e) '(a x b y e g h) :return-unmatched-list2-p t)
;;  (A B E)   (C D)   (X Y G H)




;;COMPARE-NESTED-LIST-ITEMS
;;
;;ddd
(defun compare-nested-list-items (list-of-lists1 nth-item1 list-of-lists2 nth-item2 
                          &key fuzzy-match-p auto-cutoff n-matched-cutoff n-match-length)
  "In U-lists.lisp, compares the nth-item1 in each sublist in the  list-of-lists1 (OR if nth-item1 = nil to plain items) to each of nth-item2 in sublists (or simple items) of list-of-lists2. RETURNS (values match-items matched-sublists unmatched-items unmatched-sublists). Compares using 'equal  If items 1 and 2 are strings, can use fuzzy-match-p. n-matched-cutoff  = num of chars must have in common to specifiy a fuzzy match.  If NIL, test = 'equal is used. auto-cutoff default is 0.6 means 0.6 of the length of the item1 is the cutoff. Must be betw 0 < 1.0.  If want auto-cutoff default must set it to T or 0.6. n-match-length specifies that a substring match of at least that many chars MUST happen or no match."
  (let
      ((item1)
       (item2)
       (matched-items)
       (matched-sublists)
       (item-matchlist)
       (sublist-matchlist)
       (unmatched-items)
       (unmatched-sublists)
       (matched-p)
       (item1-return)
       (fuzzy-item)
       (fuzzy-chars)
       (fuzzy-list)
       )
    (if (or auto-cutoff n-matched-cutoff n-match-length)
        (setf fuzzy-match-p T))
    ;;find item1 -- either it is the next item in simple list or nth item in nested list
    (dolist (list1 list-of-lists1)
      (cond
       ((null nth-item1)
        (setf  item1 list1))
       (t (setf item1 (nth nth-item1 list1))))
      ;;now find item2 as nth item in list-of-lists2 -- which can also be a simple list
      (dolist (list2 list-of-lists2)
        (cond
         ((null nth-item2)
          (setf  item2 list2))
         (t (setf item2 (nth nth-item2 list2))))
        ;;see if item1 is equal to item2
        (cond
         ;;if use a fuzzy match insert a fuzzy-list with matched item plus matched chars
         (fuzzy-match-p
          (multiple-value-setq (item1-return fuzzy-item fuzzy-chars)
              (fuzzy-matcher  item1 item2 :auto-cutoff auto-cutoff
                       :n-matched-cutoff  n-matched-cutoff :n-match-length n-match-length ))
          (if  item1-return
          (setf item-matchlist (append item-matchlist (list fuzzy-item))))
                ;;not needed  sublist-matchlist (append sublist-matchlist (list (list fuzzy-item))))))
          #|   (setf  fuzzy-list (list fuzzy-item fuzzy-chars)
                 matched-items (append matched-items (list item1 fuzzy-list))
                matched-sublists (append matched-sublists (list (append list1 (list fuzzy-list)))))|#
          (unless (null item1-return)
            (setf matched-p T)))
         ;;if not using fuzzy match, use test = equal and don't add any list
         ((equal item1 item2)
          (setf matched-items (append matched-items (list item1))
                matched-sublists (append matched-sublists (list list1))
                matched-p T))
         (t  NIL))
        ;;end nested dolist
        )
      ;;if fuzzy match, must add item and sublist matchlists to overall lists
      (if (and matched-p fuzzy-match-p)
          (setf   matched-items (append matched-items (list item1 item-matchlist))
                  matched-sublists  (append matched-sublists (list (append  list1 (list item-matchlist))))
                  ;;reset item-matchlist
                  item-matchlist nil))

      ;;if no match, include the unmatched item and sublist on unmatched lists
      (unless matched-p
        (setf unmatched-items (append unmatched-items (list item1))
              unmatched-sublists  (append unmatched-sublists (list list1))))
      ;;reset the matched-p to nil
      (setf matched-p nil)
      ;;end outer dolist
      )
    ;;return from compare-nested-list-items
    (values matched-items matched-sublists unmatched-items unmatched-sublists)
    ))
;;tests
#|(defun testcl2 ()
  (let
      ((list1 '((1 c 11)(2 b 6 7 8)(3 x)))   ;; '(a b c d e))
       (list2 '((1 b 2 3)(2 c 4 5)(3 d y e g h)))
       (v1)
       (v2)
       (v3)
       (v4)
       )
    (multiple-value-setq (v1 v2 v3 v4)
    (compare-nested-list-items list1 1 list2 1))
    (values v1 v2 v3 v4)
    )) |#
;;works for (list1 '(a b c d e)), returns (B C D) (B C D) (A E)
;;works for (list1 '((1 c 11)(2 b 6 7 8)(3 x))), returns   (C B)  ((1 C 11) (2 B 6 7 8))  (X) ((3 X))

;;test for fuzzy match
#|
(defun testcl3 ()
  (setf out nil)
  (let
      ((list1  '(("CaseNum" "CaseNumOrigFile") ("CaseType" "")))
#| ("SourceFile" "Source files PARTnum") ("FileDate" "") ("Instr" "Instructor") ("Resr" "Researcher") ("Name" "") ("IDnum" "") ("Sex" "Sex 1=M 2=F") ("Age" "") ("Email" "") ("ZipCode" "") ("Nation" "") ("HrsWork" "") ("UserRate" "") ("tknowmor" "t-Want to know more of self") ("texperie" "t-Experienced self-help user") ("twanttho" "t-Want thorough assessment") ("twantspe" "t-Want specific help") ("tworknga" "t-worknga") ("tu100stu" "t-CSULB U100 student") ("tcsulbst" "t-CSULB other student") ("totherst" "t-Other student") ("tcolstu" "t-Other college student") ("tinstruc" "t-Instructor") ("tcolfaca" "t-College faculty-admin") ("twanthel" "t-Want help with problem") ("wantspq" "g-Specific questionnaire") ("gsuchap" "g-Success-happiness") ("gacadsuc" "g-Academic success") ("gemocop" "g-Emotional coping") ("gslfest" "g-Self-esteem") ("gprocrst" "g-Procrastination") ("gtimeman" "g-Time Management") ("grelat" "g-Relationships") ("gmeetpeo" "g-Meeting people")))|#
       (list2    '( "CaseNum" "CaseType" "Group" "Var2" "FileDate" "Instr" "Resr" "Name" "SSN"   
       "Sex" "Age" "Email" "ZipCode" "Nation" "HrsWork" "UserFBra" "TKnowMor" "TExperie" "TWantTho" "TWantSpe" "TWorkngA"   
       "TU100Stu" "TCSULBSt" "TOtherSt" "TColStu" "TInstruc" "TColFacAd" "TWantHel" "WantSpQ" "GSucHap" "GAcadSuc" "GEmoCop"   
       "GSlfEst" "GProcrstn" "GTimeMan" "GRelat" "GMeetPeo" "GLonelyF" "GExValus" "GDepres" "GAnxFear" "GAggrAng" "GCompltA"   
       "GCarPlan" "GNotTake" "GCarOnly"   
          ;;  43 scales
        "SPersBio" "SAAchApt" "SAMotSat" "SAMotiv"   
        "SLrnSkls" "SLrnArea" "SLrnDisb" "SSelfMan" "SEmotCop" "SLiThAch" "SLiThSoc" "SLiThNeg" "SLiThInt" "StbSlfWo" "SIEcontr"   
        "SWorldVi" "SGrFears" "SSlfConf" "SAssert" "SIntimat" "SIndepRe" "SRomantc" "SLibRole" "SeHappy" "SRelHlth" "SRPeople"   
        "SrDepres" "SrAnxiet" "SrAngAgg" "SrEmotPr" "Sb2Ethic" "SinCar" "SinBus" "SinEngr" "SinFineA" "SinHelp" "SinLang" "SinMed"   
        "SinMiltC" "SinNatSc" "SinSocSc" "SinWoEth" "SinWrite"   
         ;;   questions
         "bio3educ" "bioHSGPA" "bioColle" "bio4job" "Student" "Manager" "ProPeop" "ProTech"   
         "Consulta" "Educator" "Sales" "Technici" "Clerical" "Service" "OwnBus10" "OthrSfEm" "Other"   
        "bio5inco" "bio7lang" "LEnglish" "LSpanish" "LVietname" "LCambodn" "LChinese" "LKorean" "LPortugue" "LGerman" "LFrench"   
        "LOthrAsn" "LOthrEur" "LOther" "bio1ethn" "ENorthAm" "EAfrica" "ENorEur" "ESouEur" "ECambodn" "EChina" "EKorea" "EJapan"   
        "EVietnam" "EOthrAsn" "EMexico" "ECentrAm" "ESouthAm" "EPacific" "EOther" "bioRelAf" "Catholic" "Jewish" "Islam"   
        "LatterD" "Buddhist" "Baptist" "Methodst" "Episcop" "Lutheran" "Presbyte" "PrOLiber" "PrOFunda" "NoAffil" "Agnostic"   
         "OthrNoAn" "stParEd" "stuColle" "CoCSULB" "CcCSU" "CoUCal" "CoOPublc" "CoPrivCA" "CoPrivOt" "CoCAComC"   
         "CoOthCC" "CoOthNAT" "CoPrGrad" "CoTech" "HighSch" "CoOther" "stuClass" "stuDegre" "stuMajor" "MLibArt"   
         "MSocSci" "MBiolSci" "MArt" "MNatSci" "MBus" "MEnginr" "MEducat" "MMedical" "MOtCompu" "MOthTech" "MRecrPE"   
         "MDoesNA" "MUndecid" "stuSpeci" "STranCC" "STran4yr" "SAdultRe" "SEOP" "SUSImmig" "SVisa" "SHonor"   ))
  
       (v1)
       (v2)
       (v3)
       (v4)
       )
    (multiple-value-setq (v1 v2 v3 v4)
        (compare-nested-list-items list1 0 list2 nil :n-match-length 4))  ;;:auto-cutoff 0.5 )) ;; n-matched-cutoff 6
    (values v1 v2 v3 v4)
    ))
|#
;;(compare-nested-list-items list1 list2 nil :auto-cutoff 0.7 )
;;works, returns NIL NIL plus 2 unmatched lists
;;



;;REPLACE-LIST-ITEM
;;
;;ddd
(defun replace-list-item (new-item old-item list &key delete-instead-p (test 'my-equal))
  "In U-lists.lisp, replaces old-item with new-item unless delete-instead-p is T, then old item is
    simply deleted (put nil or anything for new-item). RETURNS (values  new-list item-found-p)"
  (let
      ((new-list)
       (post-list)
       (item-found-p)
       )
    (dolist (item list)
      (cond
       ((funcall test  item old-item)      ;;was (my-equal item old-item)
        (cond
         ((null delete-instead-p)
          (setf  item-found-p t
                 new-list (append new-list (list new-item))))
         (t nil)))
       (t (setf new-list (append new-list (list item)))))
      ;;(afout 'out (format nil "item= ~A old-item= ~A new-item= ~A~% new-list= ~A~%" item old-item new-item new-list))
      )     
   (values  new-list item-found-p)
    ))
;;TEST
;;(replace-list-item  '(very long list)  '((list1 a b c d e)(list2 1 2 3 4 5)(list3 x y z))
;;(replace-list-item '((1 3 2)(2 3 2))  '((1 3 2)) '(((2 5 1))((1 3 2))((1 4  3))))
;; works= (((2 5 1)) ((1 3 2) (2 3 2)) ((1 4 3)))   T
#|(defun testrli ()
 ;; (replace-list-item 'x NIL '(a b c d NIL e) :delete-instead-p t) ;;works, returns (A B C D E)
  (replace-list-item 'x "" '(a b c d "" e) :delete-instead-p t) ;;works, returns (A B C D E)
  )|#
  ;;(replace-list-item '(new group)   '((L (((I 5 2))))) '(((M (((I L F))))) ((F (((I L 2))))) ((L (((I 3 2))))) ((I (((1 3 2) (2 3 2) (3 3 2) (4 3 2))))) ((L (((I 4 2))))) ((I (((1 4 2) (2 4 2) (3 4 2) (4 4 2))))) ((L (((I 3 2) (I 4 2) (I 5 2))))) ((L (((I 5 2))))) ((I (((1 5 2) (2 5 2) (3 5 2) (4 5 2))))) ((F (((I L 2) (I L 3))))) ((F (((I L 3))))) ((L (((I 3 3))))) ((I (((1 3 3) (2 3 3) (3 3 3) (4 3 3))))) ((L (((I 4 3))))) ((I (((1 4 3) (2 4 3) (3 4 3) (4 4 3))))) ((L (((I 3 3) (I 4 3) (I 5 3))))) ((L (((I 5 3))))) ((I (((1 5 3) (2 5 3) (3 5 3) (4 5 3))))))) = works
;;result= (((M (((I L F))))) ((F (((I L 2))))) ((L (((I 3 2))))) ((I (((1 3 2) (2 3 2) (3 3 2) (4 3 2))))) ((L (((I 4 2))))) ((I (((1 4 2) (2 4 2) (3 4 2) (4 4 2))))) ((L (((I 3 2) (I 4 2) (I 5 2))))) (NEW GROUP) ((I (((1 5 2) (2 5 2) (3 5 2) (4 5 2))))) ((F (((I L 2) (I L 3))))) ((F (((I L 3))))) ((L (((I 3 3))))) ((I (((1 3 3) (2 3 3) (3 3 3) (4 3 3))))) ((L (((I 4 3))))) ((I (((1 4 3) (2 4 3) (3 4 3) (4 4 3))))) ((L (((I 3 3) (I 4 3) (I 5 3))))) ((L (((I 5 3))))) ((I (((1 5 3) (2 5 3) (3 5 3) (4 5 3))))))   T




;;DELETE-ITEMS-IN-LISTS
;;2017
;;ddd
(defun delete-items-in-lists (items lists &key (test 'my-equal) (begin 0) end
                                not-delete-nil-p )
  "In U-lists. Deletes same item from all lists. RETURNS newlists. begin and end apply to each list."
  (let
      ((newlists)
       (deleted-item-lists)
       )
    (loop
     for list in lists
     do
     (cond
      ((listp list)
       (multiple-value-bind (newlist deleted-items)
           (delete-list-items items list :test test :begin begin :end end
                              :not-delete-nil-p not-delete-nil-p )
         (when (or newlist not-delete-nil-p)
           (setf newlists (append newlists (list newlist))))
         (setf deleted-item-lists (append deleted-item-lists (list deleted-items)))
         ))
      (t (setf newlists (append newlists (list list)))))
     ;;end loop
     )
    (values newlists deleted-item-lists)
    ;;end let, delete-items-in-lists
    ))
;;TEST
;; (delete-items-in-lists '(K1 this) '((K1 1 2)(3 4 k1)(5 6 7)(THIS K1 7 8)))
;; works= ((1 2) (3 4) (5 6 7) (7 8))    ((K1) (K1) NIL (THIS K1))




;;DELETE-LIST-ITEMS
;;2017 version
;;ddd
(defun delete-list-items (items list &key (test 'my-equal) (begin 0) end
                                not-delete-nil-p )
  "In U-lists. Deletes same item from all lists. RETURNS (values newlist deleted-items)NOT WORK with nested-lists; will work with non-nested list items."
  (let
      ((newlists)
       (deleted-items)
       (newlist)
       (element)
       (begin-list)
       (end-list)
       (n-list (list-length list))
       )
    (cond
     (end
      (setf end-list (nthcdr end list)))
     (t (setf end (- n-list 1))))
    (when (> begin 0)
      (setf begin-list (butlast list begin)))

    (when (listp list)
      (loop
       for n from begin to end
       do
       (setf element (nth n list))
       (cond
        ((member element items :test test)
         (setf deleted-items (append deleted-items (list element))))
        (t
         (when (or element not-delete-nil-p)
           (setf newlist (append newlist (list element))))))
       ;;end loop,when
       ))
    (when begin-list
      (setf newlist (append begin-list newlist)))
    (when end-list
      (setf newlist (append newlist end-list)))

    (values newlist deleted-items)
    ;;end let, delete-item-in-lists
    ))
;;TEST
;; (delete-list-items '(K1) '(1 2 3 k1 THIS 4 5 K1 7 8))
;; works = (1 2 3 THIS 4 5 7 8)  (K1 K1)
;; (delete-list-items '(K1) '(1 2 3 k1 THIS 4 5 K1 7 8) :begin 5)
;; works= (1 2 3 K1 THIS 4 5 7 8)   (K1)
;; (delete-list-items '(K1) '(1 2 3 k1 THIS 4 5 K1 7 8) :end 6)
;; works= (1 2 3 THIS 4 5 5 K1 7 8)  (K1)
;;  (delete-list-items '(c  NIL ""  " "  ) '(a b c NIL d "" x yz))
 ;;works=  (A B D X YZ)  (C "")
;; (delete-list-items '(a (x Y z)) '(c a (a b) d (x y z)  7 (X Y Z)))
;; works= (C (A B) D 7)    (A (X Y Z) (X Y Z))
;;
;; (delete-list-items '(9 (a b c))  '((8 (a b c))(9 (a b c)) x y ))
;; NOT WORK = ((8 (A B C)) (9 (A B C)) X Y)   NIL




#|(get-set-append-delete-keyvalue-in-nested-list new-value key-spec-lists nested-lists &key append-value-p list-first-item-in-append-p add-value-p splice-newvalue-at-nth-p key-in-prev-keylist-p (test (quote my-equal)) return-list-p key=list-p put-key-after-items put-value-after-items new-begin-items new-end-items splice-key-value-in-list-p splice-old-new-values-p parens-after-begin-items-p (return-nested-list-p t) (max-list-length 1000) (if-not-found-append-key-value-p t) if-not-found-append-keyvalue-list-p if-not-found-add-item-in-last-nested-list-p orig-list-items-leftn (recurse-for-key-p t) (bottom-level-p t) (if-get-no-return-old-keylist-p t) last-key-found-p oldkey)

>>>> Documentation : 
In U-lists. KEYSPEC= (key keyloc-n val-nth).  FOR KEY = :NTH (to find nth in list without key) :  (1) KEYLOC-N NOT USED--searches entire list at that level  for key (as before if keyloc-n = T) [In which case will check entire list at that level for key].  Use depreciated old function for that.
   (2) Each new level does NOT need a key in the key-spec-lists (it can be a list with lists containing keys; and (3) VALUE MUST either be an item next to key (val-nth = 1) OR val-nth [the optional 3rd item in the level spec-list].  If :NR member spec-list, sets RECURSE-FOR-KEY-P to NIL If :R member, sets it to T, then key MUST be IN LIST on FIRST LEVEL of current recursion (doesn't recurse on inner lists for that spec-list). 
   If in  NOT LAST KEYSPEC VAL-NTH is a NUMBER  searches that location [can be VALUE LIST] for the nested list with next key.  IF VAL-NTH not a number (eg aT), searches ALL NESTED LISTS for the NEXT KEY.
   To APPEND KEY & VALUE TO TOP LIST, set :IF-NOT-FOUND-APPEND-KEYVALUE-LIST-P to NIL. (To append whole list, set to T)
    RETURNS (values return-keylist return-nested-lists new-keylist return-value    
              return-old-keylist last-key-found-p old-value new-key-spec-lists)
     If RETURN-LIST-P, return-keylist is the ENTIRE list containing key, otherwise same as new-keylist. 
     KEY-SPEC-LISTS are lists of (key keyloc-n val-n) [val-nth optional, default= 0) from outermost to innermost (last) key. Eg of proper list (:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key5 OLD-VALUE)...)); key-spec-list= ((:key1 2)(:key2  2)(:key3 4)(:key5 0)) .
     KEY-IN-PREV-KEYLIST-P Adds or appends (depending on &key above) the last key and new-value (if not found in the previous flat keylist) to that previous keylist). If found, acts normally. PREFERRED if last key is same flat keylist as previous key.
     IF-NOT-FOUND-APPEND-KEY-VALUE-P adds a key and keyvalue to innermost list if not found (but preceding key must be found). If last keyloc-n = 0, puts old previous keyvalue at end of new keylist. If last keyloc-n > 0, puts it first, then fills in nils before new key in last list which is new value of previous key. 
     IF KEY = :NTH, then gets, sets, or appends number in KEYLOC-N PLACE. Note: If second item in spec-list isn't number, uses default keyloc-n.  
     PUT-KEY-AFTER-ITEMS is a list of items which is used if keyloc-n > 0 to fill in items between key and value [done automatically if old items are there]. splice-key-value-in-list-p does similar except includes items after the value. 
    If SPLICE-OLD-NEW-VALUES-P, puts or splices (list key value) into put or splice list. Otherwise puts or splices key value [not a list]. COMPATIBIILITY PROBLEMS: get-key-value-in-nested-lists, val-nth starts with 0 there, with 1 here. APPEND-VALUE-P puts old-value & new-value in a list.  
   ADD-VALUE-P just adds new-value after old-value.   Only with add-value-p, SPLICE-NEWVALUE-AT-NTH-P, Also use :NTH and :KEYLOC-N for location (:NTH 99) = eg (:k1 1 :k2 2 :newkey newval) instead of (:k1 1 :k2 2  (:newkey newval))
   LAST-KEY-FOUND-P must be NIL (set only in recursive calls), or keeps from adding new values sometimes.          
   NOTE: IF KEYS NOT FOUND, Can NOT reliably put new key and value in an innermost list IF LAST KEY NOT FOUND, because without a rigid orderly key system, could end up putting it inside the last of any or multiple nested lists. Therefore putting it in lowest level list.  Use the get-set-append-delete-keyvalue-in-orderly-nested-list function to put it INSIDE an inner nested list.
   return-nested-list-p often makes no difference.
   LIST-FIRST-ITEM-IN-APPEND-P to set keylist to key (newitem) not key newitem.
   * This version DOES NOT USE THE KEYLOC-N to search lists
  * THIS CAN BE USED AS A GENERAL SEARCH FUNCTION|#



;;APPLY-FUNC-TO-NESTEDLIST-ITEMS
;;2019
;;ddd
(defun apply-func-to-nestedlist-items (nested-list func &key arglist return-nil-p)
  "U-liists.   RETURNS result-list [same form as nested-list, but with every new item the result of applying func with optional arglist. Works on any kind of nested-list? "
  (let
      ((result-list)
       (item-result)
       )
    ;;FOR ALL ITEMS IN NESTED-LIST
    (cond
     ((listp nested-list)
      (loop
       for item in nested-list
       do
       (let*
           ((result)
            )
         (cond
          ((listp item)
           (multiple-value-bind (result1)
               (apply-func-to-nestedlist-items  item func :arglist arglist)
             (setf result result1)       
             ;;end mvb, listp item
             ))
          (t
           (cond 
            (arglist 
             (setf result (eval (append `(funcall ,func ,item) arglist))))
            (t (setf result (eval (append `(funcall ,func ,item))))))
           ;;end t,cond
           ))
         ;;accumlate
         (when (or result return-nil-p)
           (setf result-list (append result-list (list result))))
         ;;end let,loop
         ))
      ;;end listp nested-list
      )
     ;;NESTED-LIST NOT LIST
     (T
      (cond 
       (arglist 
        (setf item-result (eval (append `(funcall ,func ,nested-list) arglist))))
       (t (setf item-result (eval `(append (funcall ,func ,nested-list))))))
      (when (or item-result return-nil-p)
        (setf result-list (append result-list (list item-result)))) 
      ;;end t,cond
      ))
    result-list
    ;;end let, apply-func-to-nestedlist-items
    ))
;;TEST
;; func = #'+  arglist '(1 1) ;; adds 2 to each item
;;  (apply-func-to-nestedlist-items '(1 (2 3 (4 5) 6 (99 999)) 7 (8 (9 10 (11 12)) (1))) #'+ :arglist '( 1 1))
;; works = (3 (4 5 (6 7) 8 (101 1001)) 9 (10 (11 12 (13 14)) (3)))
;; no arglist
;; (apply-func-to-nestedlist-items '(1 (2 3 (4 5) 6 (99 999)) 7 (8 (9 10 (11 12)) (1))) #'sqrt )
;; works = (1.0 (1.4142135 1.7320508 (2.0 2.236068) 2.4494899 (9.949874 31.606963)) 2.6457513 (2.828427 (3.0 3.1622777 (3.3166249 3.4641016)) (1.0)))





;;SEARCH/DELETE-ITEM-IN-NESTED-LIST
;;2019
;;ddd   
(defun search/delete-item-in-nested-list (item nested-list  
                                               &key new-nested-list search-only-p (test 'my-equal) 
                                               from-end  (start 0) end stop-after-first-p (n 0)) 
                                                 ;;added (n 0), was undefined
  "U-list   RETURNS (values  new-nested-list matched-sublists matched-items matched-item)   INPUT: item can be a list, string, symbol, etc.   STOP-AFTER-FIRST-P Only finds/deletes FIRST INSTANCE of item. If nil, all instances will be deleted and/or found  When SEARCH-ONLY-P, does NOT delete any items. Returns full nested-list and found sublists. MATCHED-SUBLISTS  includes nested sublists.  If match is in main list, only the matched-item is included."
  (let
      ((result)
       (found-item)
       (matched-item)
       (matched-items)
       (matched-sublist)
       (matched-sublists)
       (sub-new-nested-list)
       (len-nested-list (cond ((listp nested-list) (list-length nested-list))(t 0)))       
       )
    ;;1. TEST TO SEE IF ITEM = NESTED-LIST
    (cond
     ;;BOTH ARE LISTS
     ((and (listp item)(listp nested-list))
      (cond
       ;;list = nested-list
       ((setf result 
              (my-equal-nested-lists  item nested-list :test test))
        (setf found-item result))
       (t (setf result nil)))
      ;;end both lists
      )
     ;;BOTH NOT LISTS AND PASS TEST
     ((and (null (listp item))(null (listp nested-list))
           (setf result (funcall test item nested-list))
           (setf found-item nested-list))
      ;;end both not lists, pass test
      )
     (t NIL))
    
    ;;WHEN ITEM = NESTED-LIST (either item or list)
    (when result
      (setf matched-item nested-list
        matched-items (list matched-item))
      ;;matched-sublist nested-list)      
      ;;otherwise, omit item from sub-new-nested-list
      )
    ;;NOT FIND MATCH ABOVE
    (when (null result)
      (cond
       ;;IF LISTP NESTED-LIST, RECURSE ON EACH NESTED-LIST ITEM
       ((listp nested-list)           
        (loop
         for nested-item in nested-list
         for n from 0 to len-nested-list
         do
         (let
             ((nestedlist-tail)
              (loop-matched-sublist)
              (loop-matched-sublists)
              )
           (cond
            ;;non-list ITEM = non-list NESTED-ITEM?
            ((and (not (listp item)) (not (listp nested-item))
                  (funcall test item nested-item))
             (setf matched-item  nested-item
                   matched-items (append matched-items (list nested-item)))
             ;;(break "not lists")
             ;;otherwise, omit item from sub-new-nested-list
             ;;(afout 'out (format nil "AFTER ITEM=NESTED-ITEM; NESTED-LIST- ~a sub-new-nested-list= ~A  "NESTED-LIST sub-new-nested-list ))
             )
            ;;list ITEM; non-list NESTED-ITEM [prevents need to recurse in this case]
            ((and (listp item) (not (listp nested-item)))
             ;;since no match, add to sub-new-nested-list
             (setf sub-new-nested-list (append sub-new-nested-list (list nested-item)))
             )                
            ;;NESTED-ITEM = LIST; item = list or not
            ((listp nested-item)
             ;; HERE2
             ;;(afout 'out (format nil "BEFORE RECURSE; NESTED-LIST- ~a sub-new-nested-list= ~A  "NESTED-LIST sub-new-nested-list ))
             (multiple-value-bind (sub-new-nested-list1 recurse-matched-sublists
                                                        matched-items1 matched-item1)
                 (search/delete-item-in-nested-list item nested-item
                                                    :test test 
                                                    :from-end from-end  :start start :end end 
                                                    :stop-after-first-p stop-after-first-p)
               (when recurse-matched-sublists
                 (setf loop-matched-sublists 
                       (append loop-matched-sublists recurse-matched-sublists)))
               (when matched-items1
                 (setf matched-items (append matched-items matched-items1)))

               ;;IF FOUND LIST (and final-new-list1 =  :empty-item)
               (cond
                ;;FOUND, BUT DON'T APPEND sub-new-nested-list--must include this!
                (matched-item1 
                 (when nested-item
                   (setf loop-matched-sublists 
                         (append loop-matched-sublists (list nested-item))))  ;;matched-sublists1))        
                 ;;OMIT FOLLOWING???
                 (when sub-new-nested-list1
                   (setf sub-new-nested-list 
                         (append sub-new-nested-list (list sub-new-nested-list1))))

                 ;;include following?
                 ;;end matched-item1
                 )
                (sub-new-nested-list1
                 (setf sub-new-nested-list
                       (append sub-new-nested-list (list sub-new-nested-list1)))
                 )
                (t NIL))
               ;;mvb, listp nested-item
               ))
            ;;NESTED-ITEM NOT A LIST and/or NOT MATCH
            (t
             (when nested-item
               (setf sub-new-nested-list (append sub-new-nested-list (list nested-item))))))
             
           ;;AT LOOP END
           ;;(afout 'out (format nil " END LOOP;  NESTED-LIST- ~a ~%nested-item=  ~A~%sub-new-nested-list= ~A  " NESTED-LIST nested-item  sub-new-nested-list ))  
           (when loop-matched-sublists
             (setf matched-sublists (append matched-sublists loop-matched-sublists))
             )
           ;;RETURN IF MATCH & stop-after-first-p
           (when (and stop-after-first-p loop-matched-sublists)
               (setf nestedlist-tail (nthcdr (+ n 1) nested-list))
               (when nestedlist-tail
                 (setf sub-new-nested-list (append sub-new-nested-list nestedlist-tail)))
               (return))
           ;;end let, loop, nested-list = list
           )))
       ;;ITEM = NESTED-LIST not lists
       ((funcall test item nested-list)
        (let*
            ((nestedlist-tail)
             )
        (setf matched-sublists (append matched-sublists (list nested-list)))

        (when stop-after-first-p
          (setf nestedlist-tail (nthcdr (+ n 1) nested-list))
          (when nestedlist-tail
            (setf sub-new-nested-list (append sub-new-nested-list nestedlist-tail))))
        ;;leave sub-new-nested-list as is
        ))
       ;;OTHERWISE NO MATCH IN NESTED LIST so add item to sub-new-nested-list
       (t  (setf sub-new-nested-list (append sub-new-nested-list (list nested-list)))))
      ;;end when null result
      )
    ;;ACCUMLATE
    (when sub-new-nested-list 
      (setf new-nested-list (append new-nested-list  sub-new-nested-list)))
    (when matched-sublist
      (setf matched-sublists (append matched-sublists (list matched-sublist 444))))

    ;;When search-only-p, return original nested-list; 
    (when search-only-p
      (setf new-nested-list nested-list))

    ;;(afout 'out (format nil "AT END NESTED-LIST ~a sub-new-nested-list= ~A~%==> NEW-NESTED-LIST= ~A " NESTED-LIST sub-new-nested-list new-nested-list ))
    (values  new-nested-list matched-sublists matched-items matched-item)
    ;;end let, search/delete-item-in-nested-list
    ))
;;TEST
;;  (search/delete-item-in-nested-list  'a  '(1 (2 3 (4 5) 6 (a b c)) 7 (8 (9 10 (11 12)) (1 a))))
;;  works =(1 (2 3 (4 5) 6 (B C)) 7 (8 (9 10 (11 12)) (1)))     ((A B C) (1 A))  (A A)   NIL
;;  (search/delete-item-in-nested-list  'a  '(1 (2 3 (4 5) 6 (a b c)) 7 (8 (9 10 (11 12)) (1 a))) :stop-after-first-p T)
;; works=   (1 (2 3 (4 5) 6 (B C)) 7 (8 (9 10 (11 12)) (1 A)))   ((A B C))     (A)   NIL
;;  (search/delete-item-in-nested-list  'a  '(1 (2 3 (4 5) 6 (a b c)) 7 a (8 (9 10 (11 12)) (1 a))))
;;  works= (1 (2 3 (4 5) 6 (B C)) 7 (8 (9 10 (11 12)) (1)))    ((A B C) (1 A)) (A A A)  A
;; FOR ITEM = a LIST
;;   (search/delete-item-in-nested-list '(a ("this" 7) b)  '(1 2 (3 (a ("this" 7) d)  4) 5 (e (a ("this" 7) b) f) 8 9)   )
;; works= (1 2 (3 (A ("this" 7) D) 4) 5 (E F) 8 9)   ((A ("this" 7) B))   ((A ("this" 7) B))  NIL
;;   (search/delete-item-in-nested-list '(a ("this" 7) b)  '(1 2 (3 (a ("this" 7) b)  4) 5 (e (a ("this" 7) b) f) 8 9)   )
;; works=  (1 2 (3 4) 5 (E F) 8 9)    ((A ("this" 7) B) (A ("this" 7) B))   ((A ("this" 7) B) (A ("this" 7) B))   NIL





;;DELETE-CONSEQ-REPEATS
;;2018
;;ddd
(defun delete-conseq-repeats (item list   &key from-end  (test 'my-equal) (start 0) end
                                (stop-on-first-miss-p T))
  "In U-lists Deletes consequitive repeated items with seq start to end. If from end reverses list.  RETURNS  (values newlist  matched-items)  Could be used to separate a list into 2 parts if one is conseq items. "
  (let
      ((newlist)
       (testlist list)
       (n-list (list-length list))
       (matched-items)
       )
    (when (null end)
      (setf end n-list))

    (when from-end 
      (setf testlist (reverse list)))   

    (setf testlist (subseq testlist start end))   
   ;;(break)
    (loop
     for testitem in testlist
     for n from 0 to (+ n-list 1)
     do
       (cond
        ((funcall test item testitem)
         (setf matched-items (append matched-items (list item)))) 
        (stop-on-first-miss-p
         ;;FIX 
         (setf newlist  (subseq testlist n))
         (return))
        (t (setf matched-items (append matched-items (list item)))))    
       ;;(break "1")
     ;;end let,loop
     )
    (when from-end
      (setf newlist (reverse newlist)))
    (values  newlist  matched-items)
    ;;end let, DELETE-CONSEQ-REPEATS
    ))
;;TEST
;; (delete-conseq-repeats  'x   '(a b c x x x x) :from-end t)
;; works = (A B C)   (X X X X)
;; ;; (delete-conseq-repeats  'nil   '(a b c nil nil nil) :from-end t)
;; works = (A B C)   (NIL NIL NIL)





;;DELETE-LAST-ITEMS
;;2018
;;ddd
(defun delete-last-items (item list  &key  last-n (test 'my-equal))
  "U-lists   RETURNS (values newlist n-rest n-del)   INPUT:  Unless last-n, deletes all last MULTIPLE OCCURANCES of item."
  (let*
      ((len-list (list-length list))
       (n-rest 0)
       (n-del 0)
       (newlist)
       (del-items)
       (maybe-list)
       )
    (loop
     for test-item in list
     for n from 1 to len-list
     do
     (cond
      (last-n
       (cond
        ((and (> n (- len-list last-n ))
          (apply test (list item test-item)))
           (setf del-items (append del-items (list test-item)))
           (incf n-del))
          (t (setf newlist (append newlist (list test-item)))
             (incf n-rest))))
      (t
       (cond
        ((apply test (list item test-item))
         (cond
          ((null maybe-list)
           (setf maybe-list (list item)))
          (t
           (when maybe-list
             (setf del-items (append del-items (list test-item)))
             (incf n-del)))))
        (maybe-list
         (setf newlist (append newlist maybe-list (list test-item))
               maybe-list nil
               n-rest (+ n-rest 2)))
        (t (setf newlist (append newlist  (list test-item)))
           (incf n-rest)))))
     ;;end ,loop
     )
    (when maybe-list 
      (setf del-items (append del-items maybe-list))
      (incf n-del))
    (values newlist del-items n-rest n-del)
    ;;end let, delete-last-items
    ))
;;TEST
;; (delete-last-items 'xx  '(a b c xx d e f  xx xx xx))
;; works= (A B C XX D E F)  (XX XX XX)  7 3
;; (delete-last-items nil  '(a b c NIL d e f  nil nil nil))
;; works= (A B C NIL D E F)  (NIL NIL NIL)  7  3
;; (delete-last-items nil  '(a b c NIL d e f  nil nil nil) :last-n 2)
;; works= (A B C NIL D E F NIL)  (NIL NIL)  8  2


;;
;; FOR CHANGING LIST TO STRING, OR MATCHING SUBSEQ, GO TO U-SEQUENCES.LISP  AND/OR U-TSTRING.LISP
;;
;;REPLACE-LIST 
;;replaces nth list item
;;ddd
(defun replace-list (list n new-item)
  "In U-lists.lisp, replaces nth item in a list with new-item--item can be sublist or not. begin n= 0."
  (let 
      ((nth -1)
        (new-list)
        )
    (cond 
     ((listp list)
      (dolist (item list)
        (incf nth)
        (cond
         ((= nth n)
          (setf  new-list (append new-list (list new-item))))
         (t (setf new-list (append new-list (list item)))))
        ;;end dolist
        ))
      (t  (setf new-list "ERROR-NOT A LIST")))
    new-list
    ))
;;TEST
;;  (replace-list '
;;works
#|(defun testnl ()
  (values
  (replace-list '((a b)(c d)(e f)(g (1 2) h)(i j))  3 '(x y))
  (replace-list '((a b)(c d)(e f)(g (1 2) h)(i j))  3 "this"))
  )|#




;;REPLACE-LIST-INTEGERS
;;
;;ddd
(defun replace-list-items (list new-items &key if-integerp)
  "Lists must be same length to replace each item with an item in same place in new-items.  If if-integer-p, then only replaces integers."
  (let
      ((new-list)
       )
    (loop
     for item in list
     for new-item in new-items
     do
     (cond
      ((and if-integerp (integerp item))
       (setf new-list (append new-list (list new-item))))
      (if-integerp ;;if not an integer, don't replace item
       (setf new-list (append new-list (list item))))
      (t 
       (setf new-list (append new-list (list new-item)))))
     ;;end loop
     )
    new-list
     ;;end let, replace-list-items
     ))
;;TEST
;; (replace-list-items '(I  2 3 to 4 5 F) '(I L F TO I L F) :if-integerp T) = (I L F TO I L F)
;; (replace-list-items '(X  2 3 to 4 5 F) '(I L F TO I L F) :if-integerp T) = (X L F TO I L F)

     





;;REPLACE-NTH
;;
;;ddd
(defun replace-nth (list nth value 
                         &key (append-if-no-nth-p T) delete-nth-p)
  "U-lists, replace nth item in a list with value. RETURNS (values new-list old-item value). If n greater than list length, appends it to end if append-if-no-nth-p.  if nth = :last, replaces last. If DELETE-NTH-P, deletes nth instead of replacing it. nth begin= 0"
  (let 
      ((new-list)
       (n -1)
       (old-item)
       )
     (when (equal nth :last)
      (setf nth (- (list-length list) 1)))
    (cond
     ((>=  nth (list-length list))
      (cond
       (append-if-no-nth-p 
        (setf new-list (append list (list value))))
       (t  (setf value nil
                 new-list list)))
      )
     (t
      (loop
       for item in list
       do
       (incf n)
       (cond
        ((= n nth)
         (setf old-item item)
         (cond
          ((null delete-nth-p)
              (setf new-list (append new-list (list value))))
          (t nil)))
        (t (setf new-list (append new-list (list item)))))
       ;;end loop, t, cond
       )))
    (values new-list old-item value)
    ;;end let, replace-nth
    ))
;;TEST
;;  (replace-nth '(A B C D E) 2  'THIS) = (A B THIS D E) C THIS
;;  (replace-nth '(A B C D E) 9  'THIS) = (A B C D E THIS)  NIL  THIS
;;  (replace-nth '(A B C D E) 9  'THIS :append-if-no-nth-p nil) = (A B C D E) NIL NIL
;;  (setf  listxx1 '(1 2 3 4 5))
;;  (setf listxx1 (replace-nth listxx1 9 'this)) = (1 2 3 4 5 THIS)
;;  (replace-nth '(a (b c) d) 3 'this) = (A (B C) D THIS)  NIL THIS
;;  (replace-nth '(A B C D E) :last  'THIS) = (A B C D THIS)  E  THIS
;;
;;  (progn (setf out nil) (replace-nth '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))   2   '(b c (NEW LIST))))  
;; works= ((D YES) (A (THIS)) (B C (NEW LIST)) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO)))      (B C (WHAT))   (B C (NEW LIST))      
;; (replace-nth '(1 2 3 4 5 6) 2 nil)  = (1 2 NIL 4 5 6)   3   NIL
;; (replace-nth '(1 2 3 4 5 6)  2 nil  :delete-nth-p T)  = (1 2 4 5 6)  3  NIL




;;DELETE-NTH
;;
;;ddd
(defun delete-nth (list nth)
  "In U-lists, deletes nth, RETURNS (new-list old-item). Uses replace-nth. nth begin= 0"
  (multiple-value-bind (new-list old-item)
      (replace-nth list nth nil  :delete-nth-p T)

    (values new-list old-item)
    ;;end mvb, delete-nth
    ))
;;TEST
;;  (delete-nth '(1 2 3 4 5) 3) = (1 2 3 5)  4




;;FIND-KEY-VALUES-IN-LIST 
;;
;;ddd
(defun find-keys-and-values-in-list  (key-list list 
                                               &key return-keys-not-found-p)
  "In U-list.lisp,RETURNS (values matching-key-values-list keys-found keys-not-found). In a list of misc placed key-value pairs, will return  the keys and values in key-list in the form of a list. . Then use values-list if want to convert to values list"
  (let*
      ((value)
       (n 0)
       (matching-key-values-list)
       (keys-found)
       (keys-not-found)
       )
    (dolist (item list)
      (incf n)
      (when (member item key-list :test 'equal)
        (setf value (nth n list)
              keys-found (append keys-found (list item))
              matching-key-values-list 
              (append matching-key-values-list (list item value))))                  
      ;;end dolist
      )
    (when return-keys-not-found-p
      (setf keys-not-found 
            (nth-value 1 (compare-lists key-list keys-found))))
    (values matching-key-values-list keys-found keys-not-found)
    ;;end let,
    ))
;;TEST
;;  (find-keys-and-values-in-list  '(:k2 :k3 :k-none1 :k9 :k-none2) '(:k4 99 :k2 'this :k5 'that :k3 67))
;; works, returns (:K2 (QUOTE THIS) :K3 67)
;;TO RETURN KEYS-NOT-FOUND
;; (find-keys-and-values-in-list  '(:k2 :k3 :k-none1 :k9 :k-none2) '(:k4 99 :k2 'this :k5 'that :k3 67) :RETURN-KEYS-NOT-FOUND-P T)
;; works=  (:K2 (QUOTE THIS) :K3 67)   (:K2 :K3)   (:K-NONE1 :K9 :K-NONE2)
;; (find-keys-and-values-in-list '(:C :CV :CD :V) '((2 3 4) "2.3.4" :C "HAPPY" :V "ARTVAL" :S (((2 3 4 1) "2.3.4.1" ) )) :return-keys-not-found-p T)
;;works= (:C "HAPPY" :V "ARTVAL")  (:C :V)  (:CV :CD)

;;TO RETURN VALUES INSTEAD
;;   (values-list '(:K2 (QUOTE THIS) :K3 67)) returns :K2 (QUOTE THIS) :K3 67
;; doesn't work (find-keys-and-values-in-list '(2)  '((1 (a b))(2 (c d)))) = NIL 



;;SET-KEYS-AND-VALUES-IN-LIST
;;
;;ddd
(defun set-keys-and-values-in-list  (keys-values old-keylist 
                                               &key  return-keys-not-found-p 
                                               (test 'my-equal))
  "In U-list.lisp,RETURNS . In a list of misc placed key-value pairs, will return  the keys and values in key-list in the form of a list. . Then use values-list if want to convert to values list.  OLD-KEYLIST can be a NESTED list?"
  (let*
      ((new-keys-values  old-keylist)
       (key)
       (keys-found)
       (keys-not-found)
       (n-keys-values (list-length keys-values))
       (n-old-keylist (list-length old-keylist))
       )
    (loop
     for item in keys-values
     for n from 0 to (- n-keys-values 1)
     do
     (let
         ((value)
          )
       ;;(afout 'out (format nil "item= ~A n= ~A" item n))
       (cond
        ((evenp n)
         (when item
           (setf key item)))
        ((and (oddp n) key)
         (setf value item)
         (multiple-value-bind (return-keylist new-return-nested-lists new-keylist 
                                              return-value  old-keylist last-key-found-p)
             (get-set-append-delete-keyvalue-in-nested-list  value `((,key T)) new-keys-values 
                                                             :if-not-found-append-key-value-p T :test test)    
           (setf new-keys-values new-return-nested-lists)
           ;;(afout 'out (format nil "new-keys-values= ~A key= ~A value= ~A" new-keys-values key value))
           (cond
            (last-key-found-p
             (setf keys-found (append keys-found (list key))))
            (t (setf keys-not-found (append keys-not-found (list key)))))
           (setf key nil)
           ;;(break "here")
           ;;end ,mvb,oddp
           ))
        (t nil))
       ;;end let, loop
       ))
 #|(dolist (item list)
      (incf n)
      (when (member item key-list :test 'equal)
        (setf value (nth n list)
              keys-found (append keys-found (list item))
              matching-key-values-list 
              (append matching-key-values-list (list item value))))                  
      ;;end dolist
      )
    (when return-keys-not-found-p
      (setf keys-not-found 
            (nth-value 1 (compare-lists key-list keys-found))))
    (values matching-key-values-list keys-found keys-not-found)|#

     (values new-keys-values  keys-found keys-not-found)
    ;;end let, set-keys-and-values-in-list
    ))
;;TEST
;;  (set-keys-and-values-in-list '(k1 new1 k3 new3 k5 new5) '(k2 222 k5 555 k6 666 k1 111 k7 777)))
;;works=  (K2 222 K5 NEW5 K6 666 K1 NEW1 K7 777 K3 NEW3)   (K1 K5)   (K3)


;;NOTE: RANDOMIZE WORKS WELL, BUT THE 'LESSP AND 'GREATERP ARE
;; UNRELIABLE--SOMETHING TO DO WITH HOW THE STRINGS ARE COMPARED??


;;FIND-KEYS-IN-LISTS
;;
;;ddd
(defun find-keys-in-lists (key-lists)
  "In U-lists. RETURNS list of keys/cars of all first-order lists in key-lists."
  (let
      ((keys-list)
       ( key)
       )
    (loop
     for list in key-lists
     do
     (if (listp list)
         (setf key (car list)
               keys-list (append keys-list (list key))))
     ;;end loop
     )
    keys-list
    ;;end let, find-keys-in-lists
    ))
;;TEST
;;  (find-keys-in-lists *select-shaq-scales-keylist)


;;FIND-KEYS-IN-NESTED-LISTS
;;
;;ddd
(defun find-keys-in-nested-lists (nested-list &key sublist-newline-p)
  "In U-lists, finds 1st and 2nd-order keys (initial objects in sublists) and returns a list of only the keys and nested keys. RETURNS A STRING of all keys. When sublist-newline-p = t, adds newlines."
  (let
      ((return-list-string (format nil "~%LIST OF KEYS~%"))
       ( new-sublist-string)
       (key2)
       (key2-next)
       )
    ;;eliminate extra quote--added for use with read
    (if (and (listp nested-list) (equal (car nested-list) 'quote))
        (setf nested-list (second nested-list)))

    (loop
     for sublist in nested-list
     do
     (setf  new-sublist-string  "(")
     (cond
      ((listp sublist)
       (loop
        for item in sublist
        do
        (cond
         ((listp item)
          (setf  key2 (car item))
          (if (> (list-length item) 1) 
              (setf key2-next (second item))
            (setf key2-next ""))
          (cond 
           (sublist-newline-p
            (setf new-sublist-string  
                  (format nil "~A~%(~A  ~A )" new-sublist-string key2 key2-next)))
           (t (setf new-sublist-string  
                    (format nil "~A  (~A   ~A)" new-sublist-string key2 key2-next))))
          ;;end inner listp
          )
         (t (setf new-sublist-string
                  (format nil "~A~A" new-sublist-string item))))
        ;;end inner loop
        )
       (setf return-list-string
             (format nil "~A~%~A~%)"return-list-string new-sublist-string))
       ;;end outer listp clause
       )
      ;;sublist is not a list
      (t (setf return-list-string
               (format nil "~A~A)" return-list-string sublist))))
     ;;end outer loop
     )
    return-list-string
    ;;end let, 
    ))
;;TEST
;;  (find-keys-in-nested-lists *SHAQ-question-variable-lists)
;; works SAMPLE = (IE(iecselfs)(iecicont)(iecgenet)(iecpeopl)(iecdepen)(ieccofee)(ieccoprb)
;;  (find-keys-in-nested-lists *SHAQ-question-variable-lists :sublist-newline-p t)
#| works SAMPLE =
(IE
(iecselfs)
(iecicont)
(iecgenet)
(iecpeopl)
(iecdepen)
(ieccofee)
(ieccoprb)
)|#
;;  (find-keys-in-nested-lists *select-shaq-scales-keylist)



;;FIND-LIST-WITH-ITEM
;;2017
;;ddd
(defun find-list-with-item (item lists &key (test 'my-equal))
  "In U-lists, RETURNS (values found-list nth) nth begin=0"
  (let 
      ((found-list)
       (len-lists (list-length lists))
       (nth)
       )
    (loop
     for testlist in lists
     for n from 0 to len-lists
     do
     (when (and (listp testlist)
             (member item testlist :test test))
       (setf nth n
             found-list testlist)
       (return))
     ;;end loop
     )
    (values found-list nth)
    ;;end let, find-list-with-item
    ))
;;TEST
;;  (find-list-with-item  'x '((1 2)(a b)("x" y)(4 5)))
;;  works= ("x" Y)  2




;; (setf  plist '(a b c :key1 99  :key2 nil))
;;
;;APPEND-KEY-VALUE-IN-LIST
;;  at times APPEND-KEY-VALUE used as name instead (error?)
;;
;; revised 2017, 2019
;;ddd
(defun append-key-value-in-list (key value listsym
                                     &key (key-n T)  (val-nth 1) (test 'my-equal)
                                     set-listsym-to-newlist-p  not-duplicate-p
                                     (append-value-p T)  ;;puts new-value in SAME list as old-value
                                     (delete-nils-p T) csym symvals
                                     list-first-item-in-append-p
                                     add-value-p ;;puts new-value in keylist after old-value
                                     splice-key-value-in-list-p
                                     splice-old-new-values-p
                                     ;;rest-depreciated, do nothing
                                     append-as-flatlist-p append-as-keylist-p 
                                     append-first-as-flatlist-p
                                     list-first-value-p    
                                     recursive-call-p)
  "In U-lists,  In a NESTED-list set to listsym, finds key and replaces val-nth element with value at key-n.  Can set either to T to search entire list.  Works for any flat or nested list.  If key not found, appends key and value to list. RETURNS (values new-list return-keylist old-keylist return-new-keylist return-key-found-p)  New 2016, sets listsym to new list.
    LISTSYM can be a list with first item = sym that is used for  name
   APPEND-VALUE-P puts new-value in SAME list as old-value when new-value=list
   SPLICE-OLD-NEW-VALUES-P MUST be used for nonlist new-value or it REPLACES old-value.
   ADD-VALUE-P puts new-value in keylist after old-value
  Revised 2017: revised. DEPRECIATED:  append-as-flatlist-p, append-first-as-flatlist-p, list-first-value-p, recursive-call-p.  Based upon get-set-append-delele- "

  ;;SO CAN USE SYMVALS INSTEAD OF CSYM, but can't set csym to csymlist.
  (when (listp listsym)
    (unless symvals
      (setf symvals listsym))
    (unless csym 
      (setf csym (my-make-symbol (car listsym)))))
  (let
      ((target-list) 
       )
    (cond
     ((symbolp listsym)
      (setf target-list (eval listsym)))
     ((listp listsym)
      (setf target-list listsym)))
    ;;If list-first-item-in-append-p, append-value-p must be T 
    (when list-first-item-in-append-p
      (setf append-value-p T))
    (multiple-value-bind (return-keylist new-list return-new-keylist return-value old-keylist return-key-found-p)
        ;;was (new-list return-key-found return-old-value return-new-keylist)
        ;;returns (return-keylist new-return-nested-lists new-keylist return-value  old-keylist return-key-found-p ) 
        (get-set-append-delete-keyvalue-in-nested-list value 
                                                       (list (list key key-n val-nth))  target-list 
                                                       :add-value-p  add-value-p 
                                                       :append-value-p append-value-p
                                                       :splice-key-value-in-list-p  splice-key-value-in-list-p
                                                       :splice-old-new-values-p splice-old-new-values-p
                                                       :list-first-item-in-append-p list-first-item-in-append-p
                                                       :not-duplicate-p not-duplicate-p
                                                       :test test :return-list-p T)
      ;;old (append-key-value-in-list  key value target-list :append-as-flatlist-p append-as-flatlist-p)

      (when set-listsym-to-newlist-p
        (set listsym new-list))
      ;;was (values new-list return-key-found return-old-value return-new-keylist)
      ;;(break "end")
      (values new-list return-keylist old-keylist return-new-keylist return-key-found-p)
      ;;end mvb, let, set-key-value
      )))
;;TEST
;  FOR KEYS  RANDOMLY IN A LIST WITH OWN SUBLIST/VALUE
;; (setf testlist-ky '(a b :key1 (:key2 x y) :key3 nil :key4 (1 2 3) :key5 m))
;;  (append-key-value-in-list :key3 'this 'testlist-ky  :set-listsym-to-newlist-p T)
;; works= (A B :KEY1 (:KEY2 X Y) :KEY3 (THIS) :KEY4 (1 2 3) :KEY5 M)
;; append same key again
;; APPENDS A VALUE=LIST
;; (append-key-value-in-list :key3 'that 'testlist-ky :append-value-p t :set-listsym-to-newlist-p T))
;; works= (A B :KEY1 (:KEY2 X Y) :KEY3 (THIS THAT) :KEY4 (1 2 3) :KEY5 M)
;; (append-key-value-in-list :key4 'that 'testlist-ky :append-value-p t)
#|(A B :KEY1 (:KEY2 X Y) :KEY3 NIL :KEY4 (1 2 3 THAT) :KEY5 M)
(A B :KEY1 (:KEY2 X Y) :KEY3 NIL :KEY4 (1 2 3 THAT) :KEY5 M)
(A B :KEY1 (:KEY2 X Y) :KEY3 NIL :KEY4 (1 2 3) :KEY5 M)
(A B :KEY1 (:KEY2 X Y) :KEY3 NIL :KEY4 (1 2 3 THAT) :KEY5 M)   T|#
;; DON'T USE TO APPEND, BUT TO REPLACE VALUE (must include :append-value-p T to append a value list)
;;   (append-key-value-in-list :key3 'that 'testlist-ky)
;; REPLACES OLD VALUE!!= (A B :KEY1 (:KEY2 X Y) :KEY3 (THAT) :KEY4 (1 2 3) :KEY5 M)
;;
;; WHEN NOT-DUPLICATE-P
;; (append-key-value-in-list  :S '$SLF   '(:S ($CS $MIS $EXC $TBV $WV $SLF $REFG $FAM)) :not-duplicate-p T)
;; (:S ($CS $MIS $EXC $TBV $WV $SLF $REFG $FAM))   (:S ($CS $MIS $EXC $TBV $WV $SLF $REFG $FAM))  (:S ($CS $MIS $EXC $TBV $WV $SLF $REFG $FAM))  (:S $CS $MIS $EXC $TBV $WV $SLF $REFG $FAM)

;; FOR KEYS FIRST IN OWN LIST
;; (setf testlistY '(a b (:key1 x y) c (:key2 (a))))
;;   (append-key-value-in-list :key2 '(new-value-list) 'testlistY)
;; = (A B (:KEY1 X Y) C (:KEY2 (A (NEW-VALUE-LIST)))) .. ETC
;; (setf testlisty2 '(A B (:KEY1 X Y) C (:KEY2 (A (NEW-VALUE-LIST)))))
;;   (append-key-value-in-list :key2 '(new-value-list2) 'testlisty2)
;; = (A B (:KEY1 X Y) C (:KEY2 (A (NEW-VALUE-LIST) (NEW-VALUE-LIST2)))) ETC
;;APPEND SYMBOL WITH LIST= key (sym list)
;; (setf testlistY3 '(a b (:key1 x y) c (:key2 A)))
;;   (append-key-value-in-list :key2 '(new-value-list) 'testlistY3)
;; = (A B (:KEY1 X Y) C (:KEY2 (A (NEW-VALUE-LIST))))
;; (append-key-value-in-list :key2 'A 'testlistY3)
;;= (A B (:KEY1 X Y) C (:KEY2 (A A)))
;;APPEND KEY NIL WITH SYM= key sym
;; (setf testlistY4 '(a b (:key1 x y) c (:key2)))
;;   (append-key-value-in-list :key2 'A 'testlistY4)
;; = (A B (:KEY1 X Y) C (:KEY2 A))
;;APPEND NESTED-LIST WITH LIST= sym (list1 list2)
;; (setf testlistY '(a b (:key1 x y) c (:key2 ((A)))))
;; (append-key-value-in-list :key2 '(new-value-list) 'testlistY)
;; = (A B (:KEY1 X Y) C (:KEY2 ((A) (NEW-VALUE-LIST))))  ETC
;; TO APPEND KEY NIL WITH ITEM TO MAKE IT key (item) not key item
;; use  list-first-item-in-append-p
;; (setf testlistY4 '(a b (:key1 x y) c (:key2)))
;;  (append-key-value-in-list :key2 '(new-value-list) 'testlistY4 :list-first-item-in-append-p T)
;; works= (A B (:KEY1 X Y) C (:KEY2 ((NEW-VALUE-LIST))))   (:KEY2 ((NEW-VALUE-LIST)))   (:KEY2)   (:KEY2 ((NEW-VALUE-LIST)))   T
;; (setf testlistY4b '(A B (:KEY1 X Y) C (:KEY2 ((NEW-VALUE-LIST)))))
;; (append-key-value-in-list :key2 '(new-value-list2) 'testlistY4b :list-first-item-in-append-p T)
;; (A B (:KEY1 X Y) C (:KEY2 ((NEW-VALUE-LIST) (NEW-VALUE-LIST2))))
;; works= (:KEY2 ((NEW-VALUE-LIST) (NEW-VALUE-LIST2)))    (:KEY2 ((NEW-VALUE-LIST)))    (:KEY2 ((NEW-VALUE-LIST) (NEW-VALUE-LIST2)))   

;; (setf testlistY5 '(a b (:key1 x y) c (:key2 ((A)))))

;; (setf listsymX '(a b (:key1 x y) c (:key2 1 2 3) d e :key3 11 12  :key4 (l m)))
;;   (append-key-value-in-list :key2 "NEW VALUE" 'listsymX  :append-as-keylist-p nil )
;; works= (A B (:KEY1 X Y) C (:KEY2 1 "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))  :KEY2  1  (:KEY2 1 "NEW VALUE")
;;  SSSS START HERE, NEW DOESN'T SPLICE OLD NEW
;; RESULT= (A B (:KEY1 X Y) C (:KEY2 (1 "NEW VALUE") 2 3) D E :KEY3 11 12 :KEY4 (L M)) COMPARE TO OLDER ABOVE
;; 
;;
;; key in flatlist, also :set-listsym-to-newlist-p = t
;; (append-key-value :key3 "NEWEST VALUE" 'listsymX  :set-listsym-to-newlist-p T )
;; works= (A B (:KEY1 X Y) C (:KEY2 1 2 3) D E :KEY3 11 "NEWEST VALUE" 12 :KEY4 (L M))  :KEY3  11  (:KEY3 11 "NEWEST VALUE")
;;also
;; CL-USER 44 > listsymX  = (A B (:KEY1 X Y) C (:KEY2 1 2 3) D E :KEY3 11 "NEWEST VALUE" 12 :KEY4 (L M))
;;
;;
;;FOR MORE EXAMPLES, SEE  APPEND-KEY-VALUE-IN-LIST TEST SECTION
;; OLDER--CHECK OLDER U-LISTS.LISP
;;APPEND-KEY-VALUE-IN-LIST
;;  COMPARE TO set-key-value-in-list and later revise one?
;;
;;ddd









;;GET-SET-APPEND-NESTED-POST-KEY&VALUE
;;2017
;;ddd
;; ******************** test-function1.lisp **********************

(defun get-set-append-nested-post-key&matchkey (key-value-spec-lists nested-lists
                                                                  &key set-new-value append-new-value
                                                                  append-value-p (test 'my-equal)
                                                                  (return-sublist-p T)
                                                                  return-list-p
                                                                   use-fuzzy-matcher-p
                                                                   auto-cutoff  n-matched-cutoff   
                                                                  min-seq-length)   
   "In U-lists.  A speclist = (key matchkey keylocn val-locn postval-n)  If key is nil, only searches for matchkey, if matchkey nil, only searches for key. If both specified, both must match to go to next speclist OR return item at postval-n). If POSTVAL-N = an integer, returns (nth postval-n item). If postval-n =  NIL sets target-n to same as val-locn or searchs list for val-locn.
  RETURNS (values found-item new-sublist new-list found-n new-item matched-key2)  If  SET-NEW-VALUE, sets found spec item to the set-new value.  If APPEND-NEW-VALUE,  appends it to last found list. NOTES: 1. key and matchkey must be on same list (matchkey not nested).  NOTES:  1. key, matchkey, and postval-n must refer to the SAME LIST. 
   If  matchkey does NOT MATCH item at val-locn, dos NOT CHANGE the value of postval-n.  If MATCHKEY = NIL, then it WILL change the value at postval-n."
  (let
      ((speclist)
       (final-speclist-p)
       (key)
       (matchkey)
       (matched-key2)
       (keylocn)
       (val-locn)
       (postval-n)
       (testitem)
       (found-key)
       (found-item)
       (found-list)
       (found-n)
       (last-found-n)
       (return-item)
       (new-item)
       (new-sublist)
       (new-list)
       (set-new-value-now)
       (append-new-value-now)
       (list-length (list-length nested-lists))
       )
    ;;speclist
    (setf speclist (car key-value-spec-lists))
    ;;speclist items
    (multiple-value-setq (key matchkey keylocn val-locn postval-n)
        (values-list speclist))
    ;;(BREAK "(key matchkey keylocn val-locn postval-n)")
    ;;final-speclist-p ? 
    (when (null (cdr key-value-spec-lists))
      (setf final-speclist-p T
           set-new-value-now set-new-value
            append-new-value-now append-new-value
            ))

    ;;MAIN CONDITIONS
    (cond
     ;;key not nil
     (key
      (cond
       ;;keylocn not nil
       (keylocn
        (setf testitem (nth keylocn nested-lists)
              found-key (funcall test  testitem key)) ;;was (my-equal testitem key))
        (when found-key
          (setf found-key testitem)))
       ;;keylocn nil 
        (t (multiple-value-setq (found-key keylocn)
               (find-list-item key nested-lists  T T))))
      ;;(break "found-key") 
        ;;key not nil

        (cond
         ;;IF key and not found, do not proceed
         (found-key
          (cond
           (matchkey
             ;;val-locn not nil
             (unless val-locn
               ;;val-locn is null (but matchkey isn't); and find it
               (setf val-locn (nth-value 1 (find-list-item matchkey nested-lists T T))))

             ;;get-set-append the postval-n
             ;;sss ADDED WHEN HERE
             (when val-locn
               (multiple-value-setq (found-item new-sublist found-n new-item matched-key2)  ;was new-list
                   (get-set-append-nth-post-key&matchkey matchkey keylocn val-locn 
                                                      postval-n nested-lists
                                                      :set-new-value set-new-value-now 
                                                      :append-new-value append-new-value-now
                                                      :append-value-p append-value-p
                                                      :use-fuzzy-matcher-p use-fuzzy-matcher-p
                                                      :auto-cutoff   auto-cutoff 
                                                      :n-matched-cutoff n-matched-cutoff   
                                                      :min-seq-length min-seq-length)))

             ;;(afout 'out (format nil "FOUND-KEY= ~S  MATCHKEY= ~S ~%found-item= ~S~% new-list= ~%  found-n= ~S  new-item= ~S  " found-key matchkey found-item  found-n new-item))
            ;;(BREAK "val-locn (found-item new-list found-n)")
            ;;end matchkey
            )
           (t nil))
          ;;still in keylocn
          ;;WHEN MATCHKEY = NIL and found-key,  never found matchkey
          ;;matchkey is null, same as having key plus returning nth postval-n
          (when (null matchkey)
            (setf  val-locn 0)
            (multiple-value-setq (found-item new-sublist found-n new-item matched-key2)
                (get-set-append-nth-post-key&matchkey  NIL  keylocn  0  postval-n nested-lists
                                                    :set-new-value set-new-value-now
                                                    :append-new-value append-new-value-now
                                                    :append-value-p append-value-p
                                                    :use-fuzzy-matcher-p use-fuzzy-matcher-p
                                                    :auto-cutoff   auto-cutoff 
                                                    :n-matched-cutoff n-matched-cutoff   
                                                    :min-seq-length min-seq-length)))
          ;;(BREAK "(when (null matchkey)")
            
          ;;end found-key
          )

         ;;null keylocn, but find key in non-nested lists
         #|((setf found-ns (find-list-item key nested-lists  T  T))
          (when found-ns
            (setf keylocn 0  
                  postval-n found-ns)
            (multiple-value-setq (found-item new-list found-n new-item)
                (get-set-append-nth-post-key&matchkey NIL  keylocn  0  postval-n nested-lists
                                                   :set-new-value set-new-value-now 
                                                   :append-new-value append-new-value-now
                                                   :append-value-p append-value-p))))|#
         ;;end cond,key
         ))
     ;; key = nil, keep going, see below
     (t nil))

    ;;REDUNDANT TO BELOW
     ;;IF KEY NOT FOUND= (NULL KEY) OR  (NULL FOUND-KEY) ,             
     ;; SEARCH LIST FOR LISTS TO RECURSE ON
     #|(when  (null found-key)
       (loop
        for item in nested-lists
        for n from 0 to (- list-length 1)
        do
        (cond
         ((listp item)
          (multiple-value-bind (found-item1 new-list1 found-n1 new-item1)
              (get-set-append-nested-post-key&matchkey key-value-spec-lists ITEM
                                                    :set-new-value set-new-value-now 
                                                    :append-new-value append-new-value
                                                    :append-value-p append-value-p
                                                    :use-fuzzy-matcher-p use-fuzzy-matcher-p
                                                    :auto-cutoff   auto-cutoff 
                                                    :n-matched-cutoff n-matched-cutoff   
                                                    :min-seq-length min-seq-length)

            (cond
             (found-item1
              (setf found-item found-item1
                    new-list (append new-list (list new-list1)) ;; 'FOUND-NULL-KEYFND))
                    new-item new-item1
                    found-n found-n1)
              ;;(BREAK "FOUND-NULL-KEYFND")
              ;;added
              (return)
              )
             (t (setf new-list (append new-list (list item))))) ;; 'NOT-FOUND-NULL-KEYFND)))))
            
            ;;end mvb, listp item
            ))
         (t   NIL)) ;;(setf  new-list (append new-list (list item 'NOT-A-LIST)))

        ;;end loop for nested list
        )
       ;;end  (null found-key ) 
       )|#
    ;;(BREAK "BEFORE NULL-FOUND-ITEM LOOP")
     ;;IF DIDN'T FIND KEY OR ITEM ON CURRENT LEVEL, SEARCH NESTED
     (when (or (null found-key)
               (null found-item))      
       (loop
        for item in nested-lists
        for listn from 0 to list-length
        do
        (cond
         ((listp item) 
          (multiple-value-bind (found-item1 new-sublist1 new-list1  found-n1 new-item1 matched-key21)
              (get-set-append-nested-post-key&matchkey KEY-VALUE-SPEC-LISTS
                                                    ITEM
                                                    :set-new-value set-new-value 
                                                    :append-new-value append-new-value
                                                    :append-value-p append-value-p
                                                    :use-fuzzy-matcher-p use-fuzzy-matcher-p
                                                    :auto-cutoff   auto-cutoff 
                                                    :n-matched-cutoff n-matched-cutoff   
                                                    :min-seq-length min-seq-length)
            (cond
             (found-item1
              (setf found-item found-item1
                    new-item new-item1
                    found-n found-n1
                    matched-key2 matched-key21
                    )
              ;;(break "before butlast")
              (setf  found-list item
                     new-sublist new-sublist1
                     new-list 
                     (append (butlast nested-lists found-n)(list new-list1)(nthcdr (+ found-n 1) nested-lists)))
              ;;(afout 'out (format nil "AFTER RECURSIVE (NULL FOUND-ITEM) loop~%FOUND-KEY= ~S  MATCHKEY= ~S ~%found-item= ~S~% new-list= ~%  found-n= ~S  new-item= ~S  " found-key matchkey found-item  found-n new-item))
              ;;(BREAK "after (NULL FOUND-ITEM) loop mvb (found-item1 new-list1 found-n1 new-item1)")
              (return))
             (t 
              ;;was (setf  new-list (append new-list (list item 'LOOPITEM-NOT-FOUND )))
              ))
            ;;end mvb,listp
            ))
         (t  
          ;;(setf new-list (append new-list (list item 'inside-loop)))
          ;;  (setf new-list (append new-list (list item))) ;; 'NOT-LIST)))
          ))
            
        ;;end loop
        )
       ;;end (when (null found-item)
       )
 
    ;;IF NOT FOUND-ITEM, RECURSE ON NEW-LIST WITH NEXT SPECLIST
     ;;IF NOT LAST SPECLIST
     (when (null final-speclist-p)
       (cond
        ;;found key, not  found-item, and new-list is a list (all from above results)
        ((and found-key (null found-item) (listp new-list))
         (multiple-value-bind (found-item1 new-sublist1 new-list1 found-n1 new-item1 matched-key21)
             (get-set-append-nested-post-key&matchkey  (CDR KEY-VALUE-SPEC-LISTS)
                                                    NEW-LIST
                                                    :set-new-value set-new-value
                                                    :append-new-value append-new-value
                                                    :append-value-p append-value-p
                                                    :use-fuzzy-matcher-p use-fuzzy-matcher-p
                                                    :auto-cutoff   auto-cutoff 
                                                    :n-matched-cutoff n-matched-cutoff   
                                                    :min-seq-length min-seq-length)

           (cond
            (found-item1
             (setf found-item found-item1
                   found-list NEW-LIST
                   new-sublist new-sublist1                  
                   new-list (append new-list (list new-list1 'IN-FINAL))
                   found-n found-n1
                   new-item new-item1
                   matched-key2 matched-key21
                   ))
           ;; (t  NIL ))     ;; (setf new-list (append new-list (list item 'IN-FINAL)))))
           ;;(BREAK "(null final-speclist-p) new-list found-item")
           ;;end found-key, mvb,  (null found-item)
           ))) 

        ;;WHEN FOUND-ITEM and NOT FINAL SPECLIST, 
        ;;          RECURSE ON FOUND-ITEM WITH NEXT SPECLIST
        ((and found-item  (listp found-item))
         (setf last-found-n found-n)
         (multiple-value-bind (found-item1 new-sublist1 new-list1 found-n1 new-item1)
             (get-set-append-nested-post-key&matchkey  (CDR KEY-VALUE-SPEC-LISTS)
                                                    FOUND-ITEM
                                                    :set-new-value set-new-value
                                                    :append-new-value append-new-value
                                                    :append-value-p append-value-p
                                                    :use-fuzzy-matcher-p use-fuzzy-matcher-p
                                                    :auto-cutoff   auto-cutoff 
                                                    :n-matched-cutoff n-matched-cutoff   
                                                    :min-seq-length min-seq-length)

           (cond
            (found-item1
             (setf found-item found-item1
                   new-sublist new-sublist1
                   new-item new-item1
                   found-n found-n1)
             (setf new-list 
                   (append (butlast nested-lists last-found-n) (list new-list1)
                           ;; 'MATCHKEY-FOUND)
                           (nthcdr (+ last-found-n 1) nested-lists)))
           ;;(BREAK "after FOUND-ITEM loop mvb (found-item1 new-list1 found-n1 new-item1)")
             ;;end found-item1
             )
            (t (setf new-list  ;;HERENOW
                   (append (butlast nested-lists last-found-n)
                           (list (format nil "~A  NOT FOUND--CHANGE THIS OUTPUT?" 
                                         found-item))
                           ;; 'MATCHKEY-NOT-FOUND)
                           (nthcdr (+ last-found-n 1) nested-lists)))))
           ;;end mvb,listp
           ))
        ;;what happens when found-item and NOT A LIST??
        (T NIL))

       ;; END WHEN (NULL FINAL-SPECLIST-P)
       )
     ;;(afout 'out (format nil "AT END~%FOUND-KEY= ~S  MATCHKEY= ~S ~%found-item= ~S~% new-sublist= ~S~%  found-n= ~S  new-item= ~S  " found-key matchkey found-item  new-sublist found-n new-item))
     ;;(BREAK "END")
     (unless return-sublist-p (setf new-sublist NIL))
     (unless return-list-p (setf new-list NIL))

    (values found-item new-sublist new-list found-n new-item matched-key2)
    ;;end let, get-set-append-nested-post-key&matchkey
    ))
;;TEST
;;  (progn (setf out nil) (multiple-value-setq (*found-item *new-sublist *new-list *found-n *new-item *matched-key2) (get-set-append-nested-post-key&matchkey '((:LEVEL "G:\\LISP PROJECTS TS\\screensaver2013\\ARTMAP-JavaVersion\\"  0 1 NIL)) SAMSUNG-128USB)))
;; works= = *found-item= "G:\\LISP PROJECTS TS\\screensaver2013\\ARTMAP-JavaVersion\\"
;; *new-sublist= (:LEVEL 4 "G:\\LISP PROJECTS TS\\screensaver2013\\ARTMAP-JavaVersion\\" (:DIRPATHS "G:\\LISP PROJECTS TS\\screensaver2013\\ARTMAP-JavaVersion\\artmap\\") (:FILENAMES "JavaArtmap.zip" "artmapTest.java") (:SUBDIR-INFO ("G:\\LISP PROJECTS TS\\screensaver2013\\ARTMAP-JavaVersion\\artmap\\" ("LISP PROJECTS TS" "screensaver2013" "ARTMAP-JavaVersion" "artmap"))) :LAST-LEVELN 4)
;; *NEW-LIST = NIL   *FOUND-N = 2  *NEW-ITEM= NIL  *MATCHED-KEY2=NIL
;;
;; (progn (setf out nil) (multiple-value-setq (*found-item *new-sublist *new-list *found-n *new-item ) (get-set-append-nested-post-key&matchkey '(( :LEVEL "G:\\TEMP VIDEOS\\" 0 1 0 )) *test-tomex-info)) (pprint *found-item))

;; *found-item= (:dirpaths  "G:\\TEMP VIDEOS\\Poldark\\" "G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\"   "G:\\TEMP VIDEOS\\3 OTHER MOVIES\\"     "G:\\TEMP VIDEOS\\2 TCM MOVIES\\"    "G:\\TEMP VIDEOS\\1 MYSTERIES\\")
;; *new-sublist=  (:LEVEL 2 "G:\\TEMP VIDEOS\\" (:dirpaths "G:\\TEMP VIDEOS\\Poldark\\" "G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\" "G:\\TEMP VIDEOS\\3 OTHER MOVIES\\" "G:\\TEMP VIDEOS\\2 TCM MOVIES\\" "G:\\TEMP VIDEOS\\1 MYSTERIES\\") (:FILENAMES) (:SUBDIR-INFO ("G:\\TEMP VIDEOS\\Poldark\\" ("TEMP VIDEOS" "Poldark")) ("G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\" ("TEMP VIDEOS" "Jenny-Lady Randolph Churchill 1974")) ("G:\\TEMP VIDEOS\\3 OTHER MOVIES\\" ("TEMP VIDEOS" "3 OTHER MOVIES")) ("G:\\TEMP VIDEOS\\2 TCM MOVIES\\" ("TEMP VIDEOS" "2 TCM MOVIES")) ("G:\\TEMP VIDEOS\\1 MYSTERIES\\" ("TEMP VI3DEOS" "1 MYSTERIES"))) :LAST-LEVELN 2)
;; *found-n= 3
;; *new-list = NIL

;; (progn (setf out nil) (multiple-value-setq (*found-item *new-sublist *new-list *found-n *new-item) (get-set-append-nested-post-key&matchkey '(( :LEVEL "G:\\TEMP VIDEOS\\" 0 1 0 )) *test-tomex-info :append-new-value 'MY-NEW-VALUE  :append-value-p T)) (pprint *found-item))





;;GET-SET-APPEND-NTH-POST-KEY&MATCHKEY
;;2017
;;ddd
(defun get-set-append-nth-post-key&matchkey (old-value keylocn val-locn postval-n list
                                                &key set-new-value append-new-value (test 'my-equal)
                                                append-value-p (test 'my-equal)
                                                use-fuzzy-matcher-p
                                                auto-cutoff  n-matched-cutoff   min-seq-length)
  "In U-lists.  Finds a specific key at keylocn (or anyplace on list), a specific old-value at val-locn  after the key (or anyplace after key), and then a TARGET ITEM/OLD-VALUE at postval-n. All locns start at 0. If VAL-LOCN=NIL , searches entire list for old-value. If postval-n = NIL, uses val-locn for target-n.
RETURNS (values old-target new-list target-n new-item matched-key2).  keylocn= non-nil and val-locn must be integer. If  set-new-value, sets old-target to that; if appened-new-vn=alue, appends new value to list after old-target; if  append-value-p appends old-target if  it's a list.  If  old-value=NIL (and val-locn = 0, all functions work on postval-n. If  old-value does NOT MATCH item at val-locn, dos NOT CHANGE the old-value of postval-n.  If OLD-VALUE = NIL, then it WILL change the old-value at postval-n. If USE-FUZZY-MATCHER-P, keys for auto-cutoff , n-matched-cutoff,  min-seq-lengt apply to OLD-VALUE ONLY and STRINGS ONLY--setf val-locn = nil if want to search whole list. Note: val-locn can be NIL, but NOT key-locn or postval-n"
  (let*
      ((old-target)
       (new-item)
       (new-list)
       ;; only call function if this true (last-spec-p (null (cdr key-spec-lists)))
       (val-list-n)  
       (testval)
       (matched-key2)
       (target-n)  
       (appended-sublist)
       )
    ;;val-locn can be NIL, but NOT key-locn or
    (when (numberp val-locn)
      (setf val-list-n (+ keylocn val-locn 1))
      (cond
       ((null postval-n )
        (setf target-n val-list-n))
       (t (setf  target-n (+ val-list-n postval-n 1))))
      ;;end when?
      )
;;  (get-set-append-nth-post-key&matchkey  'val1  0  1  2   '(key1 xx val1 a b oldtargX 1 2 3 4 5 6 7))
     ;;(break "(my-equal old-value (nth val-list-n list))")
    (cond
     ;;if  set-new-value
     ((and set-new-value  val-list-n   ;;last-spec-p
           (or (funcall test old-value (setf testval (nth val-list-n list)))
               ;;was (my-equal old-value (setf testval (nth val-list-n list)))
                (null old-value)
                (and  use-fuzzy-matcher-p (stringp old-value)
                     (setf matched-key2
                           (fuzzy-matcher old-value testval :auto-cutoff  auto-cutoff  
                                :n-matched-cutoff n-matched-cutoff 
                                :min-seq-length min-seq-length)))
                ;;end  and condition
                ))
      (multiple-value-setq (new-list old-target new-item)
          (replace-nth list  target-n  set-new-value)))  
     ;;if append-new-value
     ((and append-new-value  val-list-n 
           (or (funcall test old-value (setf testval (nth val-list-n list)))
               ;;was  (my-equal old-value (setf testval (nth val-list-n list)))
                (null old-value)
                (and  use-fuzzy-matcher-p (stringp old-value)
                     (setf matched-key2
                           (fuzzy-matcher old-value testval :auto-cutoff  auto-cutoff  
                                :n-matched-cutoff n-matched-cutoff 
                                :min-seq-length min-seq-length)))
                ))
      (multiple-value-setq (new-list old-target new-item)
          (append-nth-item target-n append-new-value list 
                           :append-nth-item-p append-value-p))
      ;;(break "append")
      )
     ;;if (null val-locn)
     ((null val-locn)
      ;;(break "(null val-locn)")
      (loop
       for item in (nthcdr (+ keylocn 1) list)
       for n from 0 to (list-length list)
       do
       ;;(afout  'out (format nil "item= ~A old-value= ~A" item old-value))
       (when 
           (or (funcall test   old-value item)   ;;was (my-equal old-value item)
               (and  use-fuzzy-matcher-p (stringp old-value)
                     (setf matched-key2
                           (fuzzy-matcher old-value item :auto-cutoff  auto-cutoff  
                                :n-matched-cutoff n-matched-cutoff 
                                :min-seq-length min-seq-length))))
         ;;(BREAK "fuzzy")
         ;;set the n's
         (setf val-locn n)

         ;;RECURSE USING NEW VAL-LOCN
         (multiple-value-setq (old-target new-list target-n new-item matched-key2)
             (get-set-append-nth-post-key&matchkey old-value keylocn val-locn postval-n list
                                                :set-new-value set-new-value 
                                                :append-new-value  append-new-value 
                                                :append-value-p  append-value-p
                                                :use-fuzzy-matcher-p use-fuzzy-matcher-p
                                                :auto-cutoff   auto-cutoff 
                                                :n-matched-cutoff n-matched-cutoff   
                                                :min-seq-length min-seq-length))
         ;;(break "after recurse")
         (return)
         ;;end when, loop
         ))
       ;;end (null val-locn)
       )
      ;;if GET (neither of above)
      (T     
       (setf testval (nth val-list-n list))
       (when 
           (or (funcall test old-value testval)  ;;was (my-equal old-value testval)
               (and  use-fuzzy-matcher-p (stringp old-value)
                     (setf matched-key2
                           (fuzzy-matcher old-value testval :auto-cutoff  auto-cutoff  
                                :n-matched-cutoff n-matched-cutoff 
                                :min-seq-length min-seq-length))))
         (setf old-target (nth target-n list)           
               new-list list))))
    ;;(break "end")
    (values old-target new-list target-n new-item matched-key2)
    ;;end let,get-set-append-nth-post-key&matchkey
    ))
;;TEST
;; FOR GET (null set-new-value append-new-value )
;;  (get-set-append-nth-post-key&matchkey  'val1  0  1  2   '(key1 xx val1 a b oldtargX 1 2 3 4 5 6 7))
;;  works= OLDTARGX   (KEY1 XX VAL1 A B OLDTARGX 1 2 3 4 5 6 7)   5 NIL NIL
;; IF OLD-VALUE DOESN'T MATCH THE val-locn  testval
;;  (get-set-append-nth-post-key&matchkey  'val1  0  1  2   '(key1 xx NOTVAL1 a b oldtargX 1 2 3 4 5 6 7))
;;  works= NIL   NIL   5   NIL   NIL
;; with val-locn = NIL
;; (get-set-append-nth-post-key&matchkey  'val1  0  NIL  2   '(key1 xx val1 a b oldtargX 1 2 3 4 5 6 7))
;; works= OLDTARGX  (KEY1 XX VAL1 A B OLDTARGX 1 2 3 4 5 6 7)  5   NIL  NIL
;; FOR postval-n = NIL
;;  (get-set-append-nth-post-key&matchkey  'val1  0  1  NIL  '(key1 xx val1 a b oldtargX 1 2 3 4 5 6 7))
;; works (returns VALUE if val-locn a number) = VAL1   (KEY1 XX VAL1 A B OLDTARGX 1 2 3 4 5 6 7)    2   NIL   NIL
;; IF BOTH val-locn and postval-n are NIL (searches whole list for VALUE)
;;  (get-set-append-nth-post-key&matchkey  'val1  0  NIL  NIL  '(key1 xx val1 a b oldtargX 1 2 3 4 5 6 7))
;; works=  VAL1    (KEY1 XX VAL1 A B OLDTARGX 1 2 3 4 5 6 7)  2   NIL   NIL
;; FOR set-new-value
;; (get-set-append-nth-post-key&matchkey  'val1  0  1  2   '(key1 xx val1 a b oldtargX 1 2 3 4 5 6 7) :set-new-value 99)
;; works= OLDTARGX  (KEY1 XX VAL1 A B 99 1 2 3 4 5 6 7)   5  99 NIL
;;IF VALUE DOESN'T MATCH ITEM AT VAL-LOCN, DON'T SET!!
;; (get-set-append-nth-post-key&matchkey  'val1  0  1  2   '(key1 xx NOTVAL a b oldtargX 1 2 3 4 5 6 7) :set-new-value 99)
;; works= (does NOT CHANGE postval-n value) = OLDTARGX  (KEY1 XX NOTVAL A B OLDTARGX 1 2 3 4 5 6 7)   5    NIL NIL
;; IF VALUE = NIL, 
;; (get-set-append-nth-post-key&matchkey  NIL  0  1  2   '(key1 xx NOTVAL a b oldtargX 1 2 3 4 5 6 7) :set-new-value 99)
;; works (changes postval-n when value=NIL)= OLDTARGX   (KEY1 XX NOTVAL A B 99 1 2 3 4 5 6 7)   5   99 NIL
;; FOR append-new-value
;; (get-set-append-nth-post-key&matchkey  'val1  0  1  2   '(key1 xx val1 a b oldtargX 1 2 3 4 5 6 7) :append-new-value  99)
;; works=  OLDTARGX  (KEY1 XX VAL1 A B OLDTARGX 99 1 2 3 4 5 6 7)   5 99 NIL
;; append, if item is a list; but  APPEND-VALUE-P = nil
;;  (get-set-append-nth-post-key&matchkey  'val1  0  1  2   '(key1 xx val1 a b (oldtargX) 1 2 3 4 5 6 7) :append-new-value  99)
;; works= (OLDTARGX)    (KEY1 XX VAL1 A B (OLDTARGX) 99 1 2 3 4 5 6 7)  5 99 NIL
;; append, if item is a list; but  APPEND-VALUE-P = T
;;  (get-set-append-nth-post-key&matchkey  'val1  0  1  2   '(key1 xx val1 a b (oldtargX) 1 2 3 4 5 6 7) :append-new-value  99 :append-value-p T)
;; works=  (OLDTARGX)   (KEY1 XX VAL1 A B (OLDTARGX 99) 1 2 3 4 5 6 7)   5  (OLDTARGX 99) NIL
;;   :append-new-value  99 :APPEND-VALUE-P=T  except OLD-VALUE NOT A LIST  
;; (get-set-append-nth-post-key&matchkey  'val1 0  1  2    '(key1 xx val1 a b oldtargX 1 2 3 4 5 6 7) :append-new-value  99 :append-value-p T)
;; works=  OLDTARGX   (KEY1 XX VAL1 A B (OLDTARGX 99) 1 2 3 4 5 6 7)   5  (OLDTARGX 99) NIL
;;  IF VALUE = NIL (and val-locn = 0), works to find with just KEY and TARGET-N
;;  (get-set-append-nth-post-key&matchkey   NIL 0  0  3   '(key1 xx val1 a b oldtargX 1 2 3 4 5 6 7) :append-new-value  99 :append-value-p T) 
;;  works=  OLDTARGX   (KEY1 XX VAL1 A B (OLDTARGX 99) 1 2 3 4 5 6 7)  5  (OLDTARGX 99) NIL
;;
;;USING MATCHER with val-locn  VALUE MUST BE A STRING
;;  (get-set-append-nth-post-key&matchkey  "VALUE1"  0  1  2    '(key1 xx "VALUE22"  a b oldtargX 1 2 3 4 5 6 7) :append-new-value  99 :append-value-p T :use-fuzzy-matcher-p T :n-matched-cutoff  3 :min-seq-length 3)
;; works =  OLDTARGX   (KEY1 XX "VALUE22" A B (OLDTARGX 99) 1 2 3 4 5 6 7)   5   (OLDTARGX 99)   "VALUE22"
;;USING MATCHER with val-locn=NIL  VALUE MUST BE A STRING
;; (get-set-append-nth-post-key&matchkey  "VALUE1"  0  NIL  2    '(key1 xx "VALUE22"  a b oldtargX 1 2 3 4 5 6 7) :append-new-value  99 :append-value-p T :use-fuzzy-matcher-p T :n-matched-cutoff  3 :min-seq-length 3)
;; works=  OLDTARGX     (KEY1 XX "VALUE22" A B (OLDTARGX 99) 1 2 3 4 5 6 7)    5    (OLDTARGX 99)     "VALUE22"







;;;; ******************* test-input1.lisp ***********************
#|
 (setf *testg '(:LEVEL
  1
  "G:\\"
  (:dirpaths
   "G:\\TRAVEL VIDEOS-audials\\"
   "G:\\TEMP VIDEOS\\"
   "G:\\System Volume Information\\"
   "G:\\Seagate\\"
   "G:\\PHONE BUs\\"
 
   "G:\\1 Current LW BUs\\"
   "G:\\0 Main OUR PHOTOS-VIDEOS\\"
   "G:\\0 Book Article for Academic Websites\\"
   "G:\\$RECYCLE.BIN\\")
  (:FILENAMES
   "Warranty.pdf"
   "Set_Up.exe"
   "BackupPlus.ico"
   "Autorun.inf")
  (:SUBDIR-INFO
   ("G:\\TRAVEL VIDEOS-audials\\" ("TRAVEL VIDEOS-audials"))
   ("G:\\TEMP VIDEOS\\" ("TEMP VIDEOS"))
   ("G:\\System Volume Information\\" ("System Volume Information"))
   ("G:\\Seagate\\" ("Seagate"))
   ("G:\\PHONE BUs\\" ("PHONE BUs"))

   ("G:\\0 Main OUR PHOTOS-VIDEOS\\" ("0 Main OUR PHOTOS-VIDEOS"))
   ("G:\\0 Book Article for Academic Websites\\"
    ("0 Book Article for Academic Websites"))
   ("G:\\$RECYCLE.BIN\\" ("$RECYCLE.BIN")))
  :LAST-LEVELN
  2))
 (setf  *testgx 
   '(:LEVEL
    2  ;;XXX
    "G:\\TEMP VIDEOS\\"
    (:dirpaths
     "G:\\TEMP VIDEOS\\Poldark\\"
     "G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\"
     "G:\\TEMP VIDEOS\\3 OTHER MOVIES\\"
     "G:\\TEMP VIDEOS\\2 TCM MOVIES\\"
     "G:\\TEMP VIDEOS\\1 MYSTERIES\\")
    (:FILENAMES)
    (:SUBDIR-INFO
     ("G:\\TEMP VIDEOS\\Poldark\\" ("TEMP VIDEOS" "Poldark"))
     ("G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\"
      ("TEMP VIDEOS" "Jenny-Lady Randolph Churchill 1974"))
     ("G:\\TEMP VIDEOS\\3 OTHER MOVIES\\" ("TEMP VIDEOS" "3 OTHER MOVIES"))
     ("G:\\TEMP VIDEOS\\2 TCM MOVIES\\" ("TEMP VIDEOS" "2 TCM MOVIES"))
     ("G:\\TEMP VIDEOS\\1 MYSTERIES\\" ("TEMP VIDEOS" "1 MYSTERIES")))
    :LAST-LEVELN
    2))|#
;; MORE TESTING
;; (fuzzy-matcher "value22" "value1" :auto-cutoff  nil    :n-matched-cutoff 3   :min-seq-length 3) = "value1" "value22" etc
;; TESTING FOR DIR SEARCH
;;  (get-set-append-nth-post-key&matchkey  "G:\\TEMP VIDEOS\\"  0  1  0 *test-xx)
;; (setf *test-xx '(:LEVEL 2 "G:\\TEMP VIDEOS\\" (:dirpaths "G:\\TEMP VIDEOS\\Poldark\\" "G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\" "G:\\TEMP VIDEOS\\3 OTHER MOVIES\\" "G:\\TEMP VIDEOS\\2 TCM MOVIES\\" "G:\\TEMP VIDEOS\\1 MYSTERIES\\") (:FILENAMES) (:SUBDIR-INFO ("G:\\TEMP VIDEOS\\Poldark\\" ("TEMP VIDEOS" "Poldark")) ("G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\" ("TEMP VIDEOS" "Jenny-Lady Randolph Churchill 1974")) ("G:\\TEMP VIDEOS\\3 OTHER MOVIES\\" ("TEMP VIDEOS" "3 OTHER MOVIES")) ("G:\\TEMP VIDEOS\\2 TCM MOVIES\\" ("TEMP VIDEOS" "2 TCM MOVIES")) ("G:\\TEMP VIDEOS\\1 MYSTERIES\\" ("TEMP VIDEOS" "1 MYSTERIES"))) :LAST-LEVELN 2))

;;   (progn (setf out nil) (multiple-value-setq (*found-item *new-sublist *new-list *found-n *new-item) (get-set-append-nested-post-key&matchkey '(( :LEVEL "G:\\TEMP VIDEOS\\" 0 1 0 )) *test-xx)) (pprint *found-item))

;;  (progn (setf out nil) (multiple-value-setq (*found-item *new-sublist *new-list *found-n *new-item) (get-set-append-nested-post-key&matchkey '(( :LEVEL "G:\\TEMP VIDEOS\\" 0 1 0 )) *test-xx :append-new-value 'MY-NEW-VALUE  :append-value-p T)) (pprint *found-item))

;;  (get-set-append-nth-post-key&matchkey  "G:\\TEMP VIDEOS\\"  0  1  0 *test-xx    :append-new-value 'MY-NEW-VALUE  :append-value-p T)
;; works= found-item (:dirpaths "G:\\TEMP VIDEOS\\Poldark\\" "G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\" "G:\\TEMP VIDEOS\\3 OTHER MOVIES\\" "G:\\TEMP VIDEOS\\2 TCM MOVIES\\" "G:\\TEMP VIDEOS\\1 MYSTERIES\\")
;;new-list (:LEVEL 2 "G:\\TEMP VIDEOS\\" (:dirpaths "G:\\TEMP VIDEOS\\Poldark\\" "G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\" "G:\\TEMP VIDEOS\\3 OTHER MOVIES\\" "G:\\TEMP VIDEOS\\2 TCM MOVIES\\" "G:\\TEMP VIDEOS\\1 MYSTERIES\\" MY-NEW-VALUE) (:FILENAMES) (:SUBDIR-INFO ("G:\\TEMP VIDEOS\\Poldark\\" ("TEMP VIDEOS" "Poldark")) ("G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\" ("TEMP VIDEOS" "Jenny-Lady Randolph Churchill 1974")) ("G:\\TEMP VIDEOS\\3 OTHER MOVIES\\" ("TEMP VIDEOS" "3 OTHER MOVIES")) ("G:\\TEMP VIDEOS\\2 TCM MOVIES\\" ("TEMP VIDEOS" "2 TCM MOVIES")) ("G:\\TEMP VIDEOS\\1 MYSTERIES\\" ("TEMP VIDEOS" "1 MYSTERIES"))) :LAST-LEVELN 2)
;;found-n 3
;; new-item  (:dirpaths "G:\\TEMP VIDEOS\\Poldark\\" "G:\\TEMP VIDEOS\\Jenny-Lady Randolph Churchill 1974\\" "G:\\TEMP VIDEOS\\3 OTHER MOVIES\\" "G:\\TEMP VIDEOS\\2 TCM MOVIES\\" "G:\\TEMP VIDEOS\\1 MYSTERIES\\" MY-NEW-VALUE)
;; matched-key2 nil




     




        
;;GET-SET-APPEND-FOUND-VALUE
;; 2017         
;;ddd
(defun get-set-append-found-value (value keylocn val-locn 
                                         key-spec-lists list
                                         &key set-new-value append-new-value (test 'my-equal)
                                         append-value-p)
  "In U-lists. RETURNS (values old-value new-list).  keylocn= non-nil and val-locn must be integer. If  set-new-value, sets value to that; if append-new-value, appends new value to list after value; if  append-value-p appends value if  it's a list."
  (let
      ((old-value)
       (new-list)
       (value (nth (+ keylocn val-locn) list))
       (last-spec-p (null (cdr key-spec-lists)))
       (val-list-n (+ keylocn val-locn))
       )
    (cond
     ;;if  set-new-value
     ((and set-new-value  last-spec-p
           (or (funcall test value (nth val-list-n list)) ;;was (my-equal value (nth val-list-n list))
                (null value)))
      (multiple-value-setq (new-list old-value)
          (replace-nth list  val-list-n  set-new-value)))
     ;;if append-new-value
     ((and append-new-value  last-spec-p
           (or  (funcall test value (nth val-list-n list))  ;;was(my-equal value (nth val-list-n list))
                (null value)))
      (multiple-value-setq (new-list old-value)
          (append-nth-item val-list-n append-new-value list)))
     ;;if get (neither of above)
     (t (setf old-value value
              new-list list)))
    (values old-value new-list)
    ;;end let,get-set-append-found-value
    ))
;;TEST
;; SSSS START HERE DEBUGGING -- use new append-nth-item function1
;;  (change-found-value   'val1  0  2 '((key1 val1 0  2))  '(key1 xx val1 rest of list))
;;  works= VAL1   (KEY1 XX VAL1 REST OF LIST)
;;  (change-found-value 'key1  'val1  0  2 '((key1 val1 0  2))  '(key1 xx yy rest of list))











;;SET-NTH-IN-LIST
;;2016
;;ddd
(defun set-nth-in-list (value nth list)
  "In U-list, sets nth item in list to value. begin=0. RETURNS new list."
  (let
      ((new-list)
       )
    (loop
     for item in list
     for n from 0 to (list-length list)
     do
     (cond
      ((= n nth)
       (setf new-list (append new-list (list value))))
      (t (setf new-list (append new-list (list item)))))
     ;;end loop
     )
    new-list
    ;;end let, set-nth-in-list
    ))
;;TEST
;; (set-nth-in-list 'new-value 2  '(1 2 (3 a) 4 5)) = (1 2 NEW-VALUE 4 5)

     



;;SET-KEY-VALUE-IN-LISTS
;;2020
;;ddd
(defun set-key-value-in-lists (key value list-of-lists
                          &key append-values-p (replace-old-value-p T)
                          (set-listsym-to-newlist-p T)
                          (append-as-keylist-p T) append-as-flatlist-p no-dupl-p
                           (test 'my-equal))
"In U-lists, SETS SAME VALUE FOR ALL NON-NESTED. LISTS WITH KEY. EACH list-sym can be a SYMBOL OR LIST, but only symbol can be set to new list. In a list which LISTSYM evals to, finds key and replaces next element with value. If key not found, appends key and value) to list. Uses equal. If APPEND-VALUES-P, makes a value list of old & new vals. append-values-p has precedence over replace-old-value-p.
  RETURNS (values newlists old-values symlist). APPEND-AS-KEYLIST-P causes a keylist to be appended if no key found, APPEND-AS-FLATLIST-P (has priority) causes key value to be appended. If both are NIL, then nothing appended if key not found.  NOTE: RECURSIVE function, may replace set-key-value-in-nested-lists (this newer, doesn't need key-specs, better for simplier cases?). Use set-key-value-in-list if start with list instead of listsym. NO-DUPL-P will not append if already in value-list."
  (let*
      ((newlists)
       (old-values)
       (symlist)
       )
    (loop
     for sym/list in list-of-lists
     do
     (multiple-value-bind (newlist  new-key-val old-value)
         (set-key-value key value sym/list
                        :append-values-p append-values-p
                        :replace-old-value-p replace-old-value-p
                        :append-as-keylist-p append-as-keylist-p
                        :append-as-flatlist-p append-as-flatlist-p 
                        :no-dupl-p no-dupl-p :test test )
       ;;If symbol & reset it
       (when (and set-listsym-to-newlist-p
                  (symbolp sym/list))
         (set sym/list newlist)
         (setf symlist (append symlist (list sym/list))))
       ;;in any case
       (setf newlists (append newlists (list newlist))
             old-values (append old-values (list old-value)))
       ;;end mvb,loop
       ))
    (values newlists old-values symlist)
    ;;end let, set-key-value-in-lists
    ))
;;TEST
;; (set-key-value-in-lists :k2 'newval '((:k2 old)(:k1 33 :k2 44) (:k2 '(old))))
;; works= ((:K2 NEWVAL) (:K1 33 :K2 NEWVAL) (:K2 NEWVAL))      (OLD 44 (QUOTE (OLD)))  NIL
;; (setf   ls1 '(:k2 old) ls2 '(:k1 33 :k2 44))
;; (set-key-value-in-lists :k2 'newval '(ls1 ls2))
;; works= ((:K2 NEWVAL) (:K1 33 :K2 NEWVAL))  (OLD 44)  (LS1 LS2)
;;also:  LS1 = (:K2 NEWVAL) and LS2 = (:K1 33 :K2 NEWVAL)




;;SET-KEY-VALUE
;;2020 revised
;;  loops thru entire list even if key found. May want to use rest-list later, tho that must keep calc cdrs for every element
;;ddd
(defun set-key-value (key value list-or-sym
                          &key append-values-p (replace-old-value-p T)
                          (append-as-keylist-p T) append-as-flatlist-p no-dupl-p
                          set-listsym-to-newlist-p  (test 'my-equal))
  "In U-lists, USE SET-KEY-VALUE-IN-LIST for NESTED LISTS.  Can be a symbol or list, but only symbol can be set to new list. In a list which LISTSYM evals to, finds key and replaces next element with value. If key not found, appends key and value) to list. Uses equal. If APPEND-VALUES-P, makes a value list of old & new vals. append-values-p has precedence over replace-old-value-p.
  RETURNS (values newlist  new-key-val old-value)  . APPEND-AS-KEYLIST-P causes a keylist to be appended if no key found, APPEND-AS-FLATLIST-P (has priority) causes key value to be appended. If both are NIL, then nothing appended if key not found.  NOTE: RECURSIVE function, may replace set-key-value-in-nested-lists (this newer, doesn't need key-specs, better for simplier cases?). Use set-key-value-in-list if start with list instead of listsym. NO-DUPL-P will not append if value or member of value if (listp value).
  NOTE: ONLY WORKS ON SYMS FOR GLOBAL VARS, NOT LOCAL VARS.
  For SET-LISTSYM-TO-NEWLIST-P to work list-or-sym must be a sym."
  (let
      ((target-list) 
       (key-found-p)
       (old-value)
       (newlist)
       (new-key-val)
       )
    ;;SYMBOL OR LIST?
    (cond
     ((listp list-or-sym)
      (setf target-list list-or-sym))
     (t (setf target-list (eval list-or-sym))))

    (loop
     for item in target-list
     for n from 0 to 5000
     do
     (let
         ((rest-list)
          )
     (cond
      (key-found-p
       (setf old-value item
             rest-list (nthcdr (+ n 1) target-list))
       ;;(afout 'out (format nil "rest-list= ~A" rest-list))
       (cond
        (append-values-p
         (cond
          ((and no-dupl-p (listp old-value))
           (cond
            ;;both lists, union
            ((listp value)
             (setf new-key-val (list key (union value old-value :test 'my-equal))))
            ;;old list, new not, but dup
            ((member value old-value :test 'my-equal)
             ;;don't change target-list if value is already in old-value list
             (setf new-key-val (list key old-value)))
            ;;old list, new not, not dup
            (T (setf new-key-val (list key (append1 old-value value))))))
        ;;(return))
        ;;DUPS OK
        ;;old list, new not
        ((and (listp old-value)(null (listp value)))
         (setf new-key-val (list key (append1 old-value value))))
        ;;both lists
        ((and (listp old-value) (listp value))
         (setf new-key-val (list key (append old-value  value))))
        ;;old not list, new is
        ((and old-value (listp value))
         (setf new-key-val (list key (append (list old-value) value))))
        ;;both not lists
        ((and old-value value)
         (setf new-key-val (list key (list old-value value))))       
        (T NIL))
         ;;end append-values-p
         )
        ;;NOT APPEND-P
        (replace-old-value-p
         (setf new-key-val (list key value)))
        ;;just join in a list--note if value is list will be (key old-val (new-val))
        (T (setf new-key-val (list key (list old-value value)))))
       ;;SET THE NEWLIST
       (setf newlist (append newlist new-key-val rest-list))
       (return)
       ;;end key-found-p
       )
      ((funcall test item key)
       (setf key-found-p T))
      (T
       (setf newlist (append newlist (list item)))))
     ;;end let, loop
     ))
    (unless key-found-p
      (cond
       ((and append-values-p (not (listp value)))
         (setf new-key-val (list key (list value))))
       (T (setf new-key-val (list key value))))
      ;;append newlist
      (cond
       (append-as-flatlist-p 
        (setf newlist (append newlist  new-key-val)))
       (append-as-keylist-p
        (setf newlist (append newlist (list new-key-val))))
       ;;default = append-as-flatlist-p
       (T (setf newlist (append newlist  new-key-val))))
       ;;(afout 'out (format nil  "in set-key-value, key-found-p NIL END: key= ~A value= ~A list-or-sym= ~A " key value list-or-sym))
        ;;end unless key found
        )
    (when (and set-listsym-to-newlist-p (symbolp list-or-sym))
      (when list-or-sym
        (set list-or-sym newlist)))
    ;;(afout 'out (format nil "SET-KEY-VALUE END, list-or-sym= ~A key= ~A value= ~A~% newlist= ~A " list-or-sym key value  newlist))
    (values newlist  new-key-val old-value)
    ;;(values new-list return-key-found return-old-value return-new-keylist)
    ;;end , let, set-key-value
    ))
;;TEST
;;SET-KEY-VALUE END, list-or-sym= ($P Persim Model NIL Person Simulation Model S ($CS) CLEV 0 QT SYS) key= S value= $CS
;; newlist= ($P Persim Model NIL Person Simulation Model S ($CS) CLEV 0 QT SYS S ($CS) CLEV 0 QT SYS) 
;; (set-key-value :S '$CS  '($P "Persim Model" NIL "Person Simulation Model" :S ($CS) :CLEV 0 :QT :SYS)  :replace-old-value-p NIL  :append-values-p T :set-listsym-to-newlist-p T :append-as-flatlist-p T :no-dupl-p T)
;;works = ($P "Persim Model" NIL "Person Simulation Model" :S ($CS) :CLEV 0 :QT :SYS)    (:S ($CS))   ($CS)
;;(set-key-value :S '($CS)  '($P "Persim Model" NIL "Person Simulation Model" :S ($CS) :CLEV 0 :QT :SYS)  :replace-old-value-p NIL  :append-values-p T :set-listsym-to-newlist-p T :append-as-flatlist-p T :no-dupl-p T)
;;works= ($P "Persim Model" NIL "Person Simulation Model" :S ($CS) :CLEV 0 :QT :SYS)   (:S ($CS))  ($CS)
;;  (set-key-value :S '($CS $T)  '($P "Persim Model" NIL "Person Simulation Model" :CLEV 0 :QT :SYS) :replace-old-value-p NIL  :append-values-p T :set-listsym-to-newlist-p NIL :append-as-flatlist-p T :no-dupl-p T)
;;works= ($P "Persim Model" NIL "Person Simulation Model" :CLEV 0 :QT :SYS :S ($CS $T))   (:S ($CS $T))  NIL

;;; (set-key-value :k1 '(new-val x y z) '(1 2 3 :k1 (oldvals x y m n) a b))
;; works= (1 2 3 :K1 (NEW-VAL X Y Z) A B)   (:K1 (NEW-VAL X Y Z))  (OLDVALS X Y M N)
;;; (set-key-value :k1 'new-val '(1 2 3 :k1 old-val a b))
;; (1 2 3 :K1 NEW-VAL A B)  (:K1 NEW-VAL)  OLD-VAL
;; test omit dupls inside a newvalue list:
;; (set-key-value :S '(sym1 oldsym2 sym2) '("supsysX" "sup phrase" $MIS.supsysX "this" :S (oldsym1 oldsym2))    :replace-old-value-p NIL  :append-values-p T :set-listsym-to-newlist-p T :append-as-flatlist-p T :no-dupl-p T)
;;works= ("supsysX" "sup phrase" $MIS.SUPSYSX "this" :S OLDSYM1 SYM1 OLDSYM2 SYM2)   (OLDSYM1 SYM1 OLDSYM2 SYM2)   (OLDSYM1 OLDSYM2)
;; (set-key-value  :S '<UTYPE  '("$BIO" "Bio Info" NIL NIL NIL :CSS $SLF :CLEV 4 :QT :SYS)  :append-values-p T  :replace-old-value-p NIL   :set-listsym-to-newlist-p T :no-dupl-p T :append-as-flatlist-p T)
;; works=  ("$BIO" "Bio Info" NIL NIL NIL :CSS $SLF :CLEV 4 :QT :SYS :S (<UTYPE))   (:S (<UTYPE))   NIL
;; add another
;; (set-key-value  :S '<UGOALS  '("$BIO" "Bio Info" NIL NIL NIL :CSS $SLF :CLEV 4 :QT :SYS :S (<UTYPE))  :append-values-p T  :replace-old-value-p NIL   :set-listsym-to-newlist-p T :no-dupl-p T :append-as-flatlist-p T)
;; works= ("$BIO" "Bio Info" NIL NIL NIL :CSS $SLF :CLEV 4 :QT :SYS :S (<UTYPE <UGOALS))   (:S (<UTYPE <UGOALS))   (<UTYPE)
;; try to add duplicate
;; (set-key-value  :S '<UGOALS  '("$BIO" "Bio Info" NIL NIL NIL :CSS $SLF :CLEV 4 :QT :SYS :S (<UTYPE <UGOALS))  :append-values-p T  :replace-old-value-p NIL   :set-listsym-to-newlist-p T :no-dupl-p T :append-as-flatlist-p T)
;;works= ("$BIO" "Bio Info" NIL NIL NIL :CSS $SLF :CLEV 4 :QT :SYS :S (<UTYPE <UGOALS))   (:S (<UTYPE <UGOALS))  (<UTYPE <UGOALS)
;; (set-key-value :k1 'new-val '(1 2 3 :k1 old-val a b))
;; works=  (set-key-value :k1 'new-val '(1 2 3 :k1 old-val a b))    (1 2 3 :K1 NEW-VAL A B)  (:K1 NEW-VAL)  OLD-VAL
;;for :append-values-p;; old-val list, value not
;;(set-key-value :k1 'new-val '(1 2 3 :k1 old-val a b) :append-values-p T)
;;works= (1 2 3 :K1 (OLD-VAL NEW-VAL) A B)    (:K1 (OLD-VAL NEW-VAL))   OLD-VAL
;; ;;for :append-values-p;; both vals are lists
;; (set-key-value :k1 '(new-val xx)  '(1 2 3 :k1 (old-val 1 2) a b) :append-values-p T)
;; works= (1 2 3 :K1 (OLD-VAL 1 2 NEW-VAL XX) A B)   (:K1 (OLD-VAL 1 2 NEW-VAL XX))   (OLD-VAL 1 2)
;; if-no-dupl-p
;; (set-key-value :k1 'val-xx  '(1 2 3 :k1 (old-val 1 val-xx 2) a b) :append-values-p T :no-dupl-p T)
;; works (returns orig list & value): (1 2 3 :K1 (OLD-VAL 1 VAL-XX 2) A B)  (OLD-VAL 1 VAL-XX 2)   (OLD-VAL 1 VAL-XX 2)
;; if-no-dupl-p & not lists

;; for adding a value in flatlist
;; (set-key-value :S  '(a b c) '(1 2 3 4) :append-as-flatlist-p T)
;; (1 2 3 4 :S (A B C))  T  (OLD VALUE)
;; for replacing with a keylist
;; (set-key-value :S  '(a b c) '(1 2 3 4)) => 
;; works= (1 2 3 4 (:S (A B C)))  NIL  NIL  (:S (A B C))
;; for replacing a value in flatlist
;; (set-key-value :S  '(a b c) '(1 2 3 4 :S (old value)) :append-as-flatlist-p T)
;; works= (1 2 3 4 :S (A B C))    :S (OLD VALUE)     (:S (A B C))
;; THIS FUNCTION ONLY WORKS ON SYMS FOR GLOBAL VARS, NOT LOCAL VARS SEE BELOW:
;; (LET ((keylist1 '(a b :key1 99 c d :key2 66 e :key3 '(this))))  (set-key-value :key2 '(new-value) keylist1 :set-listsym-to-newlist-p T) keylist1)
;; DOES NOT SET KEYLIST1 to new var, if use 'KEYLIST1,
;; get UNBOUND VAR ERROR.  
;; (setf keylist1 '(a b :key1 99 c d :key2 66 e :key3 '(this)))  
;; (set-key-value :key2 '(new-value)  'keylist1)
;; works= (A B :KEY1 99 C D :KEY2 (NEW-VALUE) E :KEY3 (QUOTE (THIS)))  :KEY2  66   (:KEY2 (NEW-VALUE))
;;append-as-flatlist-p
;; for a new key
;; (set-key-value :key5 '(new-value)  'keylist1 :append-as-flatlist-p T :append-as-keylist-p NIL :set-listsym-to-newlist-p T)
;; works= (A B :KEY1 99 C D :KEY2 66 E :KEY3 (QUOTE (THIS)) :KEY5 (NEW-VALUE))
;; keylist1= (A B :KEY1 99 C D :KEY2 66 E :KEY3 (QUOTE (THIS)) :KEY5 (NEW-VALUE))
;; (set-key-value :key4 '(new-value) 'keylist1 
;; works= (A B :KEY1 99 C D :KEY2 66 E :KEY3 (QUOTE (THIS)) (:KEY4 (NEW-VALUE)))  NIL  NIL  (:KEY4 (NEW-VALUE))
;;




;;replace value for key in nested list
;;  (setf keylist2 '(a b :key1 99 c d ( :key2 66 ) e :key3 '(this)))
;;  (set-key-value :key2 '(new-value) 'keylist2)
;; ;;works= (A B :KEY1 99 C D (:KEY2 (NEW-VALUE)) E :KEY3 (QUOTE (THIS)))   :KEY2   66    (:KEY2 (NEW-VALUE))
;;IF LISTSYM IS A LIST
;; (setf keylist1 '(a b :key1 99 c d :key2 66 e :key3 '(this)))
;; (set-key-value :key2 'this keylist1)
;; works=  (A B :KEY1 99 C D :KEY2 THIS E :KEY3 (QUOTE (THIS)))    :KEY2    66   (:KEY2 THIS)   
;; FOR MORE EXAMPLES, SEE TEST AREA BELOW set-key-value-in-list





;;SET-KEYS-VALUES-IN-LIST
;;2020 modified/tested
;;ddd
(defun set-keys-values-in-list (key-value-pairs keylist 
                                                &key append-values-p 
                                                omit-if-null-value-p  no-dupl-p
                                                (replace-old-value-p T)
                                                (append-as-flatlist-p T) append-as-keylist-p)
  "U-lists Sets keys & values in NON-NESTED list from key-value lists using set-key-value. Useful for adding only keys with NON-NIL VALUES."
  (let*
      ((new-keylist keylist)
       (old-key-vals)
       (keys-found)
       )
    (loop
     for k-v-pair in key-value-pairs 
     do
     (let*
         ((key (car k-v-pair))
          (val (second k-v-pair))
          )
       (unless (and omit-if-null-value-p (null val))
         (multiple-value-bind (new-keylist1 kf1 old-val1) ;;(newlist kf old-val)
             (set-key-value key val new-keylist :no-dupl-p no-dupl-p
                            :append-values-p append-values-p
                            :replace-old-value-p replace-old-value-p
                            :append-as-keylist-p append-as-keylist-p
                            :append-as-flatlist-p append-as-flatlist-p)
           (setf new-keylist new-keylist1) 
           (setf old-key-vals (append old-key-vals (list (list key old-val1))))
           (when kf1
             (setf keys-found (append keys-found (list key))))
           ;;end let,,loop
           ))))
     (values  new-keylist keys-found old-key-vals)
   ;;end let, set-keys-values-in-list
  ))
;;TEST
;; (set-keys-values-in-list '((:k1 new1)(:k2 new2)) '(:a a :k1 old1 :c c :k2 old2 :d d))
;; works= (:A A :K1 NEW1 :C C :K2 NEW2 :D D)   (:K1 :K2)  ((:K1 OLD1) (:K2 OLD2))
;; If add new keys & some have nil vals & append-values-p
;; (set-keys-values-in-list '((:k1 new1)(:k2 (old2 new2))(:kx1 add1)(:kx2 nil)) '(:a a :k1 old1 :c c :k2 (old2 xx) :d d) :append-values-p T :replace-old-value-p NIL  :omit-if-null-value-p T :append-as-flatlist-p T :no-dupl-p T))
;;works= (:A A :K1 (OLD1 NEW1) :C C :K2 (XX OLD2 NEW2) :D D :KX1 (ADD1))   (:K1 :K2 :KX1)   ((:K1 OLD1) (:K2 (OLD2 XX)) (:KX1 NIL))
;;don't append values
;;(set-keys-values-in-list '((:k1 new1)(:k2 (old2 new2))(:kx1 add1)(:kx2 nil)) '(:a a :k1 old1 :c c :k2 (old2 xx) :d d) :replace-old-value-p NIL  :omit-if-null-value-p T :append-as-flatlist-p T :no-dupl-p T)
;;works= (:A A :K1 (OLD1 NEW1) :C C :K2 ((OLD2 XX) (OLD2 NEW2)) :D D :KX1 ADD1)   (:K1 :K2 :KX1)   ((:K1 OLD1) (:K2 (OLD2 XX)) (:KX1 NIL))
;;normally add keys&vals when know not on list with (NULL append-values-p)?
;; (set-keys-values-in-list '((:k1 new1)(:k2 (new2))(:kx1 add1)(:kx2 nil)) '(:a a  :d d) :append-values-p NIL :replace-old-value-p NIL  :omit-if-null-value-p T :append-as-flatlist-p T :no-dupl-p T)
;;works= (:A A :D D :K1 NEW1 :K2 (NEW2) :KX1 ADD1)   (:K1 :K2 :KX1)    ((:K1 NIL) (:K2 NIL) (:KX1 NIL))



;;add new keys&vals to a list with APPEND-VALUES-P 
;; (set-keys-values-in-list '((:k1 new1)(:k2 (new2))(:kx1 add1)(:kx2 nil)) '(:a a  :d d) :append-values-p T :replace-old-value-p NIL  :omit-if-null-value-p T :append-as-flatlist-p T :no-dupl-p T)
;; works= (:A A :D D :K1 (NEW1) :K2 (NEW2) :KX1 (ADD1))   (:K1 :K2 :KX1)   ((:K1 NIL) (:K2 NIL) (:KX1 NIL))



;;SET-KEY-VALUE-IN-LIST 
;;
;;ddd
(defun set-key-value-in-list (key value list 
                          &key (append-as-keylist-p T) append-as-flatlist-p recursive-call-p)
  "In U-lists, In a NESTED? LIST (not LIST-SYM), finds key and replaces next element with value. If key not found, appends key and value) to list. Uses equal. RETURNS (values new-list key-found old-value) . APPEND-AS-KEYLIST-P causes a keylist to be appended if no key found, APPEND-AS-FLATLIST-P (has priority) causes key value to be appended. If both are NIL, then nothing appended if key not found.  NOTE: RECURSIVE function, may replace set-key-value-in-nested-lists (this newer, doesn't need key-specs, better for simplier cases?)"
  (let
      ((key-found-p)
       (key-found)
       (new-element)
       (old-value)
       (new-keylist)
       (new-list) 
       (new-sublist)
       (return-new-keylist)
       (return-old-value)
       (return-key-found)
       (rest-list)
       )
    (when (listp list)
      (loop
       for element in list
       do
       ;;FOR LIST ELEMENT, recurse
       (cond
        ((and (listp element)(null key-found-p))
         ;;Use to prevent adding new keylist to every level
#|   not needed?      (when recursive-call-p
           (setf append-as-keylist-p nil
                 append-as-flatlist-p nil))|#

         (multiple-value-setq (new-sublist key-found old-value new-keylist)
             (set-key-value-in-list key value element  :append-as-keylist-p append-as-keylist-p
                            :append-as-flatlist-p append-as-flatlist-p 
                            :recursive-call-p T))      
         (when key-found 
           (setf return-new-keylist new-keylist
                 return-key-found key-found
                 return-old-value old-value
                 append-as-keylist-p nil
                 append-as-flatlist-p nil))
        ;;(when (equal (car element)  :key2  )(break "key-found?"))

         ;;If new-sublist found append it, otherwise append element to new-list
         (cond
          (new-sublist 
           (setf new-list (append new-list (list new-sublist))))
           (t (setf new-list (append new-list (list element)))))
         )
        (T
         ;;FOR NON-LIST ELEMENT
         (cond
          (key-found-p
           (setf return-key-found  key
                 return-old-value element
                 return-new-keylist (list key value)
                 old-value element
                 new-element value
                 key-found key
                 new-keylist (append new-keylist (list new-element))
                 key-found-p nil))
          ((equal key element)
           (setf key-found-p T
                 new-keylist (list key)
                 new-element key))
          (t (setf new-element element)))
         (setf new-list (append new-list (list new-element)))
         ;;end T, cond
         ))  
       ;;end loop
       )
   ;;IF KEY NOT FOUND APPEND KEY TO LIST
    ;;(when (null recursive-call-p) (break "at append key"))

    (when (and (null return-key-found)
               (null key-found)(null key-found-p) (null recursive-call-p))
      (cond
       (append-as-flatlist-p
        (setf  return-new-keylist (list key value)
               new-list (append new-list return-new-keylist)))
       (append-as-keylist-p
        (setf  return-new-keylist (list key value)
         new-list (append new-list (list return-new-keylist)))   )        
       (t nil))) 
    ;;(break "At end of append")
      ;;end when listp 
       )

    ;;avoids problem of  new-keylist being reset after recurse 
    (when new-keylist 
      (setf return-new-keylist  new-keylist))
    (when  key-found
      (setf  return-key-found  key-found))
    (when old-value
      (setf  return-old-value old-value))

    (values new-list return-key-found return-old-value return-new-keylist)
    ;;end let, set-key-value-in-list
    ))
;;TEST
;;for flat key list
;;(setf keylist1 '(a b :key1 99 c d :key2 66 e :key3 '(this)))  
;; (set-key-value-in-list :key2 '(new-value)  '(a b :key1 99 c d :key2 66 e :key3 '(this)))
;; works = (A B :KEY1 99 C D :KEY2 (NEW-VALUE) E :KEY3 (QUOTE (THIS)))   :KEY2  66  (:KEY2 (NEW-VALUE))
;;for new key
;;if want whole keylist appended
;; (set-key-value-in-list :key4 '(new-value) '(a b :key1 99 c d :key2 66 e :key3 '(this)))
;; works= (A B :KEY1 99 C D :KEY2 66 E :KEY3 (QUOTE (THIS)) (:KEY4 (NEW-VALUE)))   NIL  NIL  (:KEY4 (NEW-VALUE))
;;for adding a new flatlist key & value
;;  (set-key-value-in-list :key4 '(new-value) '(a b :key1 99 c d :key2 66 e :key3 '(this)) :append-as-flatlist-p T)
;;works= (A B :KEY1 99 C D :KEY2 66 E :KEY3 (QUOTE (THIS)) :KEY4 (NEW-VALUE))   NIL  NIL  (:KEY4 (NEW-VALUE))
;;
;;for keylist in list
;; (set-key-value-in-list :key2 '(new-value) '(a b (:key1 99) c d (:key2 66) e (:key3 '(this))))
;;works= (A B (:KEY1 99) C D (:KEY2 (NEW-VALUE)) E (:KEY3 (QUOTE (THIS))))   :KEY2   66   (:KEY2 (NEW-VALUE))
;;
;;for keylist in middle of nested list
;; (set-key-value-in-list :key1 '(new-value) '(a b (x y :key1 99  w z) c d (:key2 66) e (:key3 '(this))))
;; works= (A B (X Y :KEY1 (NEW-VALUE) W Z) C D (:KEY2 66) E (:KEY3 (QUOTE (THIS))))   :KEY1   99   (:KEY1 (NEW-VALUE))
;;
;;for keylist inside a double-nested-list
;; (set-key-value-in-list :key1 '(new-value) '(a (b) (list1 2 3 (x y :key1 99  w z) 4 5) c d (:key2 66) e (:key3 '(this))))
;; works= (A (B) (LIST1 2 3 (X Y :KEY1 (NEW-VALUE) W Z) 4 5) C D (:KEY2 66) E (:KEY3 (QUOTE (THIS))))  :KEY1  99  (:KEY1 (NEW-VALUE))

;;OTHER
;; (set-key-value-in-list :key1 'new-keyvalue '(6 "newdata nth=1" :KEY1 (C D E) "newdata nth=4"))
;; works= (6 "newdata nth=1" :KEY1 NEW-KEYVALUE "newdata nth=4")   :KEY1   (C D E)   (:KEY1 NEW-KEYVALUE)

;;flatlist
;;  (set-key-value-in-list :info "NEW-FLAT-VALUE" '("MOTHER" "mother" CS1-1-1-1 (DATALIST THIS :KEY1 "that") NIL :INFO "this is info" :KEY3 (A B C)))
;; works= ("MOTHER" "mother" CS1-1-1-1 (DATALIST THIS :KEY1 "that") NIL :INFO "NEW-FLAT-VALUE" :KEY3 (A B C))  :INFO "this is info"  (:INFO "NEW-FLAT-VALUE")

;; simpliest case?
;; (set-key-value-in-list :key2 '(new-value) '(:key2 66))
;; works= (:KEY2 (NEW-VALUE))  :KEY2  66  (:KEY2 (NEW-VALUE))





;;GET-KEY-VALUE
;;
;;modified 2016-04, 2019 so key could be a nth
;;ddd
(defun get-key-value (key list &key (nth-item 1) (test 'my-equal))
  "In U-lists. gets key value in a flat list of keys or list including keylists. nth-item =1 is value following key.In U-lists, searches a flat list for a key, RETURNS (values value key keylist) the value following key if found, nil if not. Uses my-equal which will even match symbols and strings. NOTE: Differs from GETF which requires a keyword. Any string or symbol can be used for key in get-key-value. IF TEST = 'NTH, finds nth value instead of a key."
  (let
      ((value)
       (key-foundp)
       (nth 0)
       (keylist)
       )
    (when (listp list)
      (cond
       ((equal test 'nth)
        (setf value (NTH (+ key nth-item) list)
                  key-foundp T
                  keylist list))
       (t
      (loop
       for element in list
       do
       ;;FOR LISTS (check for key, return if found)
       (when (listp element)
         (when  (my-equal (car element) key)
           (setf keylist element
                 key-foundp key
            value (nth nth-item element))
           (return)))
       ;;(break "in key-value")
          
       ;;OTHERWISE continue processing whether list or not
       (cond
        ((funcall test element key) ;;was(my-equal element key)
         (setf  key-foundp key)
         (incf nth))
        ((= nth nth-item)
         (setf value element)
         (return))
        ((> nth 0) (incf  nth))
        (t nil))
       ;;end loop
       )
       ;;end t, cond, when
       )))
    (values value key-foundp keylist)
    ;;end let, get-key-value
    ))
;;TEST
;; for non keylists
;; (get-key-value  'key2 '(a b 77 key1 99 key2 a b c key3 99))
;; works=  A  KEY2  NIL
;; (get-key-value  'key2 '(a b 77 key1 99 key2 a b c key3 99) :nth-item 2)
;; works= B  KEY2 NIL
;;FOR KEYLISTS
;;(get-key-value  'key2 '(a b 77 (key1 99) (key2 a b c) (key3 99)))
;;works= A  KEY2  (KEY2 A B C)
;;(get-key-value  'key2 '(a b 77 (key1 99) (key2 a b c) (key3 99)) :nth-item 2))
;; works= B  KEY2  (KEY2 A B C)
;;from older version
;;  (get-key-value "this" '(a b c this (1 2 3) d e f))  =  (1 2 3)   "this" NIL
;;note:
;;  (getf  '(a b c this (1 2 3) d e f) "this") = NIL
;;  (getf  '(a b c this (1 2 3) d e f) 'this) = nil
;;  (getf  '(a b c :this (1 2 3) d e f) :this) = nil
;;  (getf  '(a b c :this (1 2 3) d e f)  1) = NIL

;; (get-key-value 'k2 '(k1 a b (k2 xx y) d))
;; works= XX  K2  (K2 XX Y)

;;FOR KEY = NTH & nth-item 1
;;(get-key-value 2 '(A B C D E F) :TEST 'NTH)
;; workd = D    T   (A B C D E F)
;;FOR KEY = NTH & nth-item 0
;;(get-key-value 2 '(A B C D E F) :TEST 'NTH :nth-item 0)
;; C  T  (A B C D E F)




;;GET-KEY-VALUES
;;
;;ddd
(defun get-key-values (key list  &key end-key (test 'my-equal))
  "In U-lists. gets key values in a flat list of keys searches a flat list for a key, RETURNS (values values key keylist) Returns ALL flat list values following key if found UP TO END-KY (or any key if null end-key), nil if not found. Uses my-equal which will even match symbols and strings. NOTE:  Any string or symbol can be used for key in get-key-value. ALSO works on NESTED LIST."
  (let
      ((key-values)
       (key-foundp)
       (keylist)
       )
    (when (listp list)
      (loop
       for element in list
       do
       ;;(break "b1")
       (cond
        ((funcall test element key) ;;was(my-equal element key)
         (setf  key-foundp key))
        ((and key-foundp (keywordp element))       
         (return))
        (key-foundp
         (setf key-values (append key-values (list element))))
        ((listp element)
#|         (when  (my-equal (car element) key)
           (setf key-foundp key)|#
           (multiple-value-setq (key-values key-foundp keylist)
                 (get-key-values key element :end-key end-key))
           (when key-foundp (return))
           )
        (t nil))
       ;;(BREAK "end loop")
       ;;end loop, when
       ))
    (when key-values
      (setf keylist (append (list key) key-values)))
    (values key-values key-foundp keylist)
    ;;end let, get-key-values
    ))
;;TEST
;; (get-key-values :key2 '(list :key1 a b c (1 2 3) :key2 x (y z) u v :key3 11 22 (this list)))
;; works= (X (Y Z) U V)   :KEY2  (:KEY2 X (Y Z) U V)
;;
;;(get-key-values :key2 '(list :key1 a b c (1 2 3) :key2 x (y z) u v))
;; works= (X (Y Z) U V)  :KEY2  (:KEY2 X (Y Z) U V)
;;
;; (get-key-values :key2 '(list :key1 a b c (1 2 3) ( :key2 x (y z) u v :key3 11 22 (this list)) ex1 ex2))
;; works= (X (Y Z) U V)  :KEY2  (:KEY2 X (Y Z) U V)
;;
;;IN A NESTED LIST
;; (get-key-values :key2 '(list1 this ( :key1 a b c (1 2 3)  :key2 x (y z) u v :key3 11 22 (this list)) ex1 ex2))
;; works= (X (Y Z) U V)  :KEY2   (:KEY2 X (Y Z) U V)
;;
;;DOUBLE-NESTED-LIST
;; (get-key-values :key2 '(list1 that (list2 this ( :key1 a b c (1 2 3)  :key2 x (y z) u v :key3 11 22 (this list)) ex1 ex2)))
;; works = (X (Y Z) U V)  :KEY2  (:KEY2 X (Y Z) U V)






;;SET-KEY-TO-VALUE-IN-PLISTS
;;
;;ddd
(defun set-key-to-value-in-plists (plist-list &key eval-value)
  "In U.lists.lisp, sets key to value eg. '((a 7)(b 'this)....), If eval-value is T, sets the key = (eval value), otherwise sets key = value. RETURNS (values plist-list plist).
  MAKES GLOBAL VARS ONLY."
  (let ((key)
        (value)
        (plist)
        )
    (dolist (plist plist-list)
      (cond
       ((= (length plist) 2)
        (cond
         (eval-value 
          (set (car plist) (eval (second plist))))
         (t (set (car plist) (second plist))))))
   ;; (afout 'out (format nil "In    (set (car plist)= ~A (second plist))= ~A (eval (car plist)= ~A~%"  (car plist) (second plist) (eval (car plist))))
    ;;end dolist
    )
    (values plist-list plist)
    ))
;;TEST
;; ;;works
#| (defun testsk ()
  (setf  xxyy 'this)
  (set-key-to-value-in-plists '((a 7)(b 'this)(c xxyy)))  ;; NOTE: b = 'this c= xxyy
  (set-key-to-value-in-plists '((d 8)(e 'that)(f xxyy)) :eval-value t)) ;; e= that, f= this|# 
;;TEST GLOBAL ONLY
;; (let ((k11)(k22)) (set-key-to-value-in-plists '((k11 'this)(k22 'that))) (list 'k11 k11 'k22 k22))
;; returns: (K11 NIL K22 NIL) therefore LOCAL vars NOT set.
;; However, GLOBAL vars ARE set: CL-USER 25 > k11 = (QUOTE THIS)





;;SET-SYM&SUBSYMS
;;2020
;;ddd
(defun set-sym&subsyms (list-sym nested-symlists 
                                 &key (eval-nested-syms-p T)
                                 (return-symlists-p T) (sym-nth 0)(val-nth 1))
  "U-lists  RETURNS (values evaled-syms non-evaled-items return-symlists) INPUT: nested-lists is lists of syms and items/lists to set sym-nth to val-nth  "
  (let*
      ((evaled-syms)
       (return-symlists)
       (non-evaled-items)
       )
    (cond
     ((symbolp list-sym) NIL)
     ((stringp list-sym)
      (setf list-sym (my-make-symbol list-sym)))
     (t nil))
    ;;SET LIST-SYM
      (cond
       ((symbolp list-sym)
        (set list-sym nested-symlists)
        (setf evaled-syms (list list-sym))
        ;;EVAL-NESTED-SYMS-P?
        (when eval-nested-syms-p
          (loop
           for item in nested-symlists
           do
           (cond
            ((listp item)
             (let*
                 ((sym (my-make-symbol (nth sym-nth item)))
                  (symvals (nth val-nth item))
                  )
               (set sym symvals)
               (setf evaled-syms (append evaled-syms (list sym)))  
               (when return-symlists-p
                 (setf return-symlists (append return-symlists (list symvals))))
               ;;end let, listp,
               ))
            (T (setf non-evaled-items (append non-evaled-items (list item)))))
           ;;end loop,when,symbolp
           )))
        (T (setf non-evaled-items (append non-evaled-items (list list-sym)))))
      (values evaled-syms non-evaled-items return-symlists)
      ;;end let, set-sym&subsyms
      ))
;;TEST
;;  (set-sym&subsyms 'testlistX  '((sym1xx (a b c))("sym2xx" (x y z))))
;; works= (TESTLISTX SYM1XX SYM2XX)  NIL
;;also:  SYM1XX = (A B C)    SYM2XX = (X Y Z)
;;also: TESTLISTX = ((SYM1XX (A B C)) ("sym2xx" (X Y Z)))





;;SET-SYMS-TO-SECOND
;;2019
;;ddd
(defun set-syms-to-second (sym-val-pairs  &key  convert-keys-p
                                          (set-sym-to-val-p T))
  "U-lists   RETURNS (values syms vals)  sims  INPUT:eg. ((key val) etc) Unless know keyword present, don't use convert-keys-p, slower. MAKES GLOBAL VARS ONLY. Can UNBIND SYMS LATER."
  (let
      ((syms)
       (vals)
       )
    (loop
     for sym-val-pair in sym-val-pairs 
     do
     (let*
         ((sym (car sym-val-pair))
          (val (second sym-val-pair))
          )
       (when convert-keys-p
         (setf sym (convert-keyword sym)))
       (setf syms (append syms (list sym))
             vals (append vals (list val)))
       (when set-sym-to-val-p
         (set sym (second sym-val-pair)))
     ;;end let,loop
     ))
    (values syms vals)
    ;;end let, set-syms-to-second
    ))
;;TEST
;; (set-syms-to-second '((a '(x))( b 22)))
;; works= (A B) 
  ;;also CL-USER 21 > a  (QUOTE (X))  CL-USER 22 > b 22
;; TO UNBIND, (makunbound-vars '(A B)) = unbinds a and b
;;FOR CONVERT-KEYS-P
;; (set-syms-to-second '((:keyy7 33)(:keyy9 99)) :convert-keys-p T)
;; works = (KEYY7 KEYY9)  
;; also:  KEYY7 = 33
;; CAN'T SET LOCAL VARS--ONLY GLOBAL ONES (tried macros etc)
;; (let*  ((a)(b))  (set-syms-to-second '((a '(x))( b 22))))
;; SETS GLOBAL a = '(x) b = 22 NOT JUST LOCAL SCOPE
;; (let*  ((a)(b))  (set-syms-to-second ((a '(x))( b 22))))
;; (defun my-settest (var val) (set var val))
;; (let*  ((a)(b))  (my-settest 'a 22))
;; result= SETS GLOBAL, not local VAR A = 22



;;SET-SYMS-TO-VALUES
;;
;;ddd
(defun set-syms-to-values (symlist value-list &key (set-sym-to-val-p T)
                                   convert-keys-p)
  "In U-lists, RETURNS (values sym-value-pairs sym-value-flatlist)
 If  SET-SYM-TO-VAL-P sets each symbol in symlist to the corresponding value in value-list. Unless know keyword present, don't use convert-keys-p, slower.
  MAKES GLOBAL VARS ONLY. Can UNBIND SYMS LATER."
  (let
      ((sym-value-flatlist)
       (sym-value-pairs)
       )
    (loop
     for sym in symlist
     for value in value-list
     do
     (when convert-keys-p
         (setf sym (convert-keyword sym)))
     (when set-sym-to-val-p
       (set sym value))
     (setf sym-value-flatlist (append sym-value-flatlist (list sym value))
           sym-value-pairs (append sym-value-pairs (list (list sym value))))
     )
    (values sym-value-pairs sym-value-flatlist)
    ))
;;TEST
;; (set-syms-to-values '(X1 X2 X3) '(99 "THIS" '(WHAT)))
;; works= ((X1 99) (X2 "THIS") (X3 (QUOTE (WHAT))))    (X1 99 X2 "THIS" X3 (QUOTE (WHAT)))
;; also X1 = 99,  x2="THIS",  x3= (QUOTE (WHAT))
;; Using CONVERT-KEYS-P
;; (set-syms-to-values '(:X1 X2 :X3) '(99 "THIS" '(WHAT)) :SET-SYM-TO-VAL-P T :CONVERT-KEYS-P T)
;; works= ((X1 99) (X2 "THIS") (X3 (QUOTE (WHAT))))    (X1 99 X2 "THIS" X3 (QUOTE (WHAT)))
;;also:  x1 = 99    x2 = "THIS"
;; TO UNBIND (makunbound-vars '(X1 X2)) = unbinds x1 and x2
;
;;TEST LOCAL VARS
;; (let* ((kk1)( kk2)) (set-syms-to-values '(kk1 kk2) '(44 66)) (list 'kk1 kk1 'kk2 kk2))
;; returns (KK1 NIL KK2 NIL) therefore LOCAL VARS NOT SET
;; also: GLOBAL VARS ARE SET: KK1 = 44   CL-USER 30 > KK2 = 66



;;SET-SYMS-TO-EVALED-SYMS
;;
;;ddd
(defun set-syms-to-evaled-syms (symlist1 symlist2 &key convert-keys-p
                                         (not-set-when-no-sym1 T))
  "In U-lists, RETURNS (values sym-value-pairs sym-value-flatlist)
 If  SET-SYM-TO-VAL-P sets each symbol in symlist to the corresponding value in value-list. Unless know keyword present, don't use convert-keys-p, slower.
  MAKES GLOBAL VARS ONLY. Can UNBIND SYMS LATER.
  UNLESS-NIL-P, sets to value ONLY if value is bound and not nil.
  NOT-SET-WHEN-NO-SYM1 If BOTH (boundp sym1) AND (NOT boundp sym2),
  uses sym1 for vals."
  (let
      ((sym-value-flatlist)
       (sym-value-pairs)
       )
    (loop
     for sym1 in symlist1
     for sym2 in symlist2
     do
     (let*
         ((val (eval-sym sym2))
          )
     (when convert-keys-p
       (setf sym1 (convert-keyword sym)))
     (cond
      (not-set-when-no-sym1
       (cond
        ((and (null val)
              (setf val (eval-sym sym1))) NIL)
        (T (set sym1 val))))
      (T (set sym1 val)))
     (setf sym-value-flatlist (append sym-value-flatlist (list sym1 val))
           sym-value-pairs (append sym-value-pairs (list (list sym1 val))))
     ;;(break "end set-syms-to-evaled-syms loop")
     ))
    (values sym-value-pairs sym-value-flatlist)
    ;;end set-syms-to-evaled-syms
    ))
;;TEST
;;2020
;; (setf boundx 'newval)
;; (set-syms-to-evaled-syms '(X1 X2 X3 X4 X5) '(99 NIL 'UNBOUND BOUNDX '(WHAT))) 
;; works= ((X1 99) (X2 NIL) (X3 NIL) (X4 NEWVAL) (X5 NIL))
;;               (X1 99 X2 NIL X3 NIL X4 NEWVAL X5 NIL)
;;ALSO: X1 = 99  ; X2 = NIL ; X4 = NEWVAL
;;
;; (set-syms-to-evaled-syms   '(*ALL-MAKE-CSYMS-SYMS&VALS) '(*file-*ALL-MAKE-CSYMS-SYMS&VALS) :not-set-when-no-sym1 T)
;;works: keeps *ALL-MAKE-CSYMS-SYMS&VALS set to its vals.




;;SSS START HERE MAKING THIS NON-DISTRUCTIVE, also see Seibel p145 and before on destructive modifs.  It seems to work, but ORGANIZE-SUBLISTS STILL DOESN'T EVEN THO I BASE IT ON THIS FUNCTION
;;MY-SORT
;;  A function to NON-DESTRUCTIVELY sort lists (REPLACES SORT for lists)
;;ddd
(defun my-sort (sequence predicate &key key)
  "In U-lists.lisp, function to NON-DESTRUCTIVELY sorts sequences. REPLACES SORT.Eg.  (my-sort list-of-lists #'test-greaterp  :key 'car) also (my-sort \"xylmno\" #'char>) RETURNS  (values new-sequence sequence). Inner lists less than symbols."
  (let
      ((new-sequence (copy-seq sequence))
       )
    (setf new-sequence (sort  new-sequence predicate :key key))
    (values new-sequence sequence)
    ))
;;TEST
;;   (my-sort "xylmno" #'char>) =  "yxonml" "xylmno"
;;
;;  (my-sort '(this a 7 0 and) #'test-lessp)
;; works= (0 7 A AND THIS)  (THIS A 7 0 AND)
;;  (my-sort '(this (mand) 7 (1 2 3)(and)) #'test-lessp)
;; results=  ((1 2 3) (AND) (MAND) 7 THIS)   (THIS (MAND) 7 (1 2 3) (AND))


  
#|(defun testsl2 ()
  (setf out nil)
  (let
      ;;must use flat lists
      ((sublists '((x 1 2)(f 3)(2 6 5)(b 3 2) (99 6 2)))
       (sublists1 '((4 3)(1 6)(8 7 3)(5 a b)))

       (y1)(y2)(y3)(y4)(y5)(o1)(o2)
       )
    ;;   (sort sublists #'test-lessp :key 'car)))
    (multiple-value-setq (y1 o1)   (my-sort sublists1 #'test-greaterp  :key 'car))
;;   (setf y1 (organize-sublists sublists1 :descend-sort t))
   ;;(afout  'out (format nil "sublist1= ~A~% y1= ~A~%o1= ~A~% " sublists1 y1 o1))
   ;;works-- afout=> sublist1= ((4 3) (1 6) (8 7 3) (5 A B))
    ;; y1= ((8 7 3) (5 A B) (4 3) (1 6))
    ;; o1= ((4 3) (1 6) (8 7 3) (5 A B))
;;    (setf y1  (my-sort sublists #'test-lessp  :key 'car))
;;   (setf y2 (organize-sublists sublists1 :ascend-sort t))
;;  (afout 'out (format nil "sublists1= ~A~% o2= ~A~%" sublists1 o2))
;;    (setf  y3 (organize-sublists sublists1   :randomize t))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
;;    (setf y4 (sort sublists1 #'test-lessp :key 'car))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
;;    (setf y5 (sort sublists1 #'test-greaterp :key 'car))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
    (fout out)
    (values y1 y2 y3 y4 y5)
    ))|#





;;SORT-1NESTED-LISTS
;;2017
;;ddd
(defun sort-1nested-lists  (nthitem uncked-lists &key ret-n (test 'my-greaterp))
  "In U-lists  Sorts list of 1nested-lists by nth item in each list. RETURNS (values sorted-lists len-uncked non-lists) by item passing test first. "
  (let
      ((sorted-lists)
       (itemlist)
       (restlists)
       (non-lists)
       (ret-n)
       (len-uncked (list-length uncked-lists))
       )    
    (loop
     for n from 0 to (- len-uncked 1)
     do
     (let
         ((selected1)
          (uncked-lists1)
          (restlists1)
          (ret-n1)
          (nthlist1)
          )
       ;;(afout 'out (format nil "uncked-lists1= ~A~% sorted-lists= ~A" uncked-lists1  sorted-lists))
       (cond
        ((and uncked-lists (listp uncked-lists))
         (multiple-value-setq (selected1 ret-n1 restlists1 uncked-lists1 nthlist1)
             (find-greatest/least-1nested-list nthitem uncked-lists :test test ))
         ;;append sorted-lists
         (setf sorted-lists (append sorted-lists (list selected1))
               uncked-lists restlists1)
         ;;(break "1")
         )
       (t (setf non-lists (append non-lists (list uncked-lists1)))))
     ;;end let,loop
     ))
    (values sorted-lists len-uncked non-lists)
    ;;end let, sort-1nested-lists
    ))
;;TEST
;; (sort-1nested-lists 1 '((C 3)(B 2)(A 1)(F 6)(D 4)(E 5)))
;; works= ((F 6) (E 5) (D 4) (C 3) (B 2) (A 1))  6  NIL
;; (sort-1nested-lists 1 '((C 3)(B 2)(A 1)(F 6)(D 4)(E 5)) :test 'my-lessp)
;; works= ((A 1) (B 2) (C 3) (D 4) (E 5) (F 6))  6  NIL
;; (my-sort-lists nth lists &key sorted-lists (ascending-p t) descending-p from-end reduce-duplicates-p)
;; (my-sort-lists 1 '((C 3)(B 2)(A 1)(F 6)(D 4)(E 5)))
;; works = ((A 1) (B 2) (C 3) (D 4) (E 5) (F 6))   ((A 1) (B 2) (C 3) (D 4) (E 5) (F 6))  NIL    NIL 6
;; (sort-1nested-lists 4 '(((A 3 K1 5 X)(A 3 K1 5 Y)(A 3 K1 5 Z))((B 1 K1 3 X)(B 1 K1 3 Y)(B 1 K1 3 Z))((C 7 K1 8 X)(C 7 K1 8 Y))))
;; works= (((C 7 K1 8 X) (C 7 K1 8 Y)) ((B 1 K1 3 X) (B 1 K1 3 Y) (B 1 K1 3 Z)) ((A 3 K1 5 X) (A 3 K1 5 Y) (A 3 K1 5 Z)))   3   NIL


;;SORT-INNER-LISTS
;;2019
;;ddd
(defun sort-inner-lists (nthitem 2nested-lists  &key (test 'my-greaterp) )
  "U-lists   RETURNS    INPUT:  "
  (let
      ((sorted-lists)
       )
    (loop
     for nested-list in 2nested-lists
     do
     (let*
         ((sorted-list (sort-1nested-lists nthitem nested-list :test test))
          )
       (setf sorted-lists (append sorted-lists (list sorted-list)))
       ;;end let,loop
       ))
    sorted-lists
    ;;end let, defun
    ))
;;TEST
 ;; (setf **sort-testx (sort-inner-lists 2 '(((INTIMATE "0.750" 7.5) (FLEXIBLE "0.750" 7) (EXUBERANT "0.750" 8) (LOVEX "0.750" 4) (PATERNAL "0.750" 23) (INSPIREOTHERS "0.750" 2) (COURTEOUS "0.750" 22) (COGNITIVEPSYCHS "0.750" 12) (GREATLEADER "0.750" 1) (CLOSE "0.750" 18) (PLAYFUL "0.750" 10) (INDUSTRIOUS "0.750" 5) (LIBERALCHRISTIAN "0.750" 8) (EMPATHETICLISTENER "0.750" 9) (LAWFUL "0.750" 17) (VERBALSKILLS "0.750" 16) (SOCIALCONSTRAINTS "0.750" 21) (RESPECTED "0.750" 20) (INDIVIDUALISTIC "0.750" 15) (PERFORMANCE-ORIENT "0.750" 6) (INFLUENCIAL "0.750" 11) (NOTHISTORY-ORIENTE "0.750" 19) (NOTANALYTIC "0.750" 14) (MATERIALISTIC "0.750" 13) (SKILLEDMOVEMENTS "0.750" 16))    ((CASUAL 0.5 5) (UNBRIDLEDHUMOR 0.5 7) (FANTASYWORLD 0.5 6) (CRITICAL 0.5 9) (PUBLICSERVANT 0.5 2) (SEEKATTENTION 0.5 7) (PERFORMER 0.5 4) (STARPERFORMER 0.5 1) (CATHOLIC 0.5 8) (PEOPLE-JOB 0.5 3))) :test 'my-lessp))
;; works = (((GREATLEADER "0.750" 1) (INSPIREOTHERS "0.750" 2) (LOVEX "0.750" 4) (INDUSTRIOUS "0.750" 5) (PERFORMANCE-ORIENT "0.750" 6) (FLEXIBLE "0.750" 7) (INTIMATE "0.750" 7.5) (LIBERALCHRISTIAN "0.750" 8) (EXUBERANT "0.750" 8) (EMPATHETICLISTENER "0.750" 9) (PLAYFUL "0.750" 10) (INFLUENCIAL "0.750" 11) (COGNITIVEPSYCHS "0.750" 12) (MATERIALISTIC "0.750" 13) (NOTANALYTIC "0.750" 14) (INDIVIDUALISTIC "0.750" 15) (SKILLEDMOVEMENTS "0.750" 16) (VERBALSKILLS "0.750" 16) (LAWFUL "0.750" 17) (CLOSE "0.750" 18) (NOTHISTORY-ORIENTE "0.750" 19) (RESPECTED "0.750" 20) (SOCIALCONSTRAINTS "0.750" 21) (COURTEOUS "0.750" 22) (PATERNAL "0.750" 23)) ((STARPERFORMER 0.5 1) (PUBLICSERVANT 0.5 2) (PEOPLE-JOB 0.5 3) (PERFORMER 0.5 4) (CASUAL 0.5 5) (FANTASYWORLD 0.5 6) (SEEKATTENTION 0.5 7) (UNBRIDLEDHUMOR 0.5 7) (CATHOLIC 0.5 8) (CRITICAL 0.5 9)))


;;SORT-2NESTED-LISTS-BY-CARLIST
;;2018-02
;;ddd
;;here now 1
(defun sort-2nested-lists-by-carlist  (nthitem 2nested-lists &key ret-n 
                                               (test 'my-greaterp))
  "In U-lists  Sorts list of 1nested-lists by nth item in each list. RETURNS (values sorted-lists len-uncked non-lists) by item passing test first. "
  (let*
      ((sorted-lists)
       (itemlist)
       (restlists)
       (non-lists)
      (ret-n)
       (uncked-lists 2nested-lists)
       (len-uncked (list-length uncked-lists)) 
       )    
    (loop
     for list in 2nested-lists
     do
     (let
         ((selected1)
          (uncked-lists1)
          (restlists1)
          (ret-n1)
          (nthlist1)
          )
       ;;(afout 'out (format nil "uncked-lists1= ~A~% sorted-lists= ~A" uncked-lists1  sorted-lists))
       (cond
        ((and uncked-lists (listp uncked-lists))
         (multiple-value-setq (selected1 ret-n1 restlists1 uncked-lists1 nthlist1)
             (find-greatest/least-1nested-list nthitem uncked-lists :test test ))
         ;;append sorted-lists
         (setf sorted-lists (append sorted-lists (list selected1))
               uncked-lists restlists1)
         ;;(break "1")
         )
       (t (setf non-lists (append non-lists (list uncked-lists1)))))
     ;;end let,loop
     ))
    (values sorted-lists len-uncked non-lists)
    ;;end let, sort-2nested-lists-by-carlist
    ))
;;TEST
;; (sort-2nested-lists-by-carlist 1 '(((A 3 K1 5 X)(A 3 K1 5 Y)(A 3 K1 5 Z))((B 1 K1 3 X)(B 1 K1 3 Y)(B 1 K1 3 Z))((C 7 K1 8 X)(C 7 K1 8 Y))))




;;FIND-GREATEST/LEAST-1NESTED-LIST
;;2017
;;ddd
(defun find-greatest/least-1nested-list  (nthitem uncked-lists &key itemlist restlists 
                                                  (nthlist  0) ret-n (test 'my-greaterp))
  "In U-lists In uncked-lists, finds selected itemlist RETURNS  (values selected ret-n restlists uncked-lists nthlist) ret-n is nthlist of selected nthlist is the length of restlists."
    (let*
        ((testlist)   
         (testitem) 
         (item)
         (selected)
         )
    (when  (null itemlist)                        
      (setf itemlist (car uncked-lists))
      (when (and itemlist (listp itemlist))
         (setf   item (nth nthitem itemlist)
            ret-n nthlist
            uncked-lists (cdr uncked-lists))))
    (when (and itemlist (listp itemlist))
      (setf  item (nth nthitem itemlist)
             testlist (car uncked-lists)
             testitem (nth nthitem testlist)
             uncked-lists (cdr uncked-lists)))
      (cond
       ((null testlist)
        (setf selected itemlist))
       ((and (listp testlist)(funcall test item testitem))  ;;added and (listp testlist)
        ;;(afout 'out (format nil "1 testitem= ~A item= ~A" testitem item))
        (setf restlists (append restlists (list testlist)))
        ;; uncked-lists (cdr uncked-lists)
        (multiple-value-setq (selected ret-n restlists uncked-lists nthlist)
             (find-greatest/least-1nested-list  nthitem uncked-lists :test test
                                                :itemlist itemlist :restlists restlists :nthlist (incf nthlist) :ret-n ret-n)))
       (t (setf restlists (append restlists (list itemlist))
                ret-n (+ nthlist 1))
         ;;(afout 'out (format nil "2 testitem= ~A item= ~A" testitem item))
          ;;uncked-lists (cdr uncked-lists)
          (multiple-value-setq (selected ret-n restlists uncked-lists nthlist)
               (find-greatest/least-1nested-list nthitem  uncked-lists :test test
                                                 :itemlist testlist :restlists restlists :nthlist (incf nthlist) :ret-n ret-n))))
      ;;(afout 'out (format nil "END selected= ~A ret-n= ~A~% restlists= ~A ~% uncked-lists nthlist= ~A " selected ret-n restlists uncked-lists nthlist))
      (values selected ret-n restlists uncked-lists nthlist)
      ;;end let, find-greatest/least-1nested-list
      ))
;;TEST
;; (find-greatest/least-1nested-list 1 '((C 3)(B 2)(A 1)(F 6)(D 4)(E 5)))
;; works= (F 6)  3   ((B 2) (A 1) (C 3) (D 4) (E 5))  NIL  5
;; (find-greatest/least-1nested-list 1 '((C 3)(B 2)(A 1)(F 6)(D 4)(E 5)) :test 'my-lessp)
;; works= (A 1)  2  ((C 3) (B 2) (F 6) (D 4) (E 5))  NIL  5




;;FIND-GREATEST
;;2017
;;ddd
(defun find-greatest  (uncked-list &key item restlist (nth  0) ret-n)
  "In U-lists In uncked-list, finds greatest item RETURNS  (values greatest ret-n restlist uncked-list nth) ret-n is nth of greatest."
    (when (null item)
      (setf item (car uncked-list)
            ret-n nth
            uncked-list (cdr uncked-list)))
    (let*
        ((testitem (car uncked-list))
         (greatest)
         )
      (setf uncked-list (cdr uncked-list))
      (cond
       ((null testitem)
        (setf greatest item))
       ((my-greaterp item testitem)
        (setf restlist (append restlist (list testitem)))
        ;; uncked-list (cdr uncked-list)
        (multiple-value-setq (greatest ret-n restlist uncked-list nth)
             (find-greatest  uncked-list :item item :restlist restlist :nth (incf nth) :ret-n ret-n)))
       (t (setf restlist (append restlist (list item))
                ret-n (+ nth 1))
          ;;uncked-list (cdr uncked-list)
          (multiple-value-setq (greatest ret-n restlist uncked-list nth)
               (find-greatest  uncked-list :item testitem :restlist restlist :nth (incf nth) :ret-n ret-n))))
      (values greatest ret-n restlist uncked-list nth)
      ;;end let, find-greatest
      ))
;;TEST
;; (find-greatest '(C D A F  B)) = F 3 (C A D B) NIL 4
;; (find-greatest '(4 5 1 3 0)) = 5  1  (4 1 3 0)  NIL  4


;;FIND-LEAST
;;2017
;;ddd
(defun find-least  (uncked-list &key item restlist (nth  0) ret-n)
  "In U-lists In uncked-list, finds least item RETURNS  (values least ret-n restlist uncked-list ret-n)  ret-n is nth of least."
    (when (null item)
      (setf item (car uncked-list)
            ret-n nth
            uncked-list (cdr uncked-list)))
    (let*
        ((testitem (car uncked-list))
         (least)
         )
      (setf uncked-list (cdr uncked-list))
      (cond
       ((null testitem)
        (setf least item))
       ((my-lessp item testitem)
        (setf restlist (append restlist (list testitem)))
        ;; uncked-list (cdr uncked-list)
        (multiple-value-setq (least ret-n restlist uncked-list nth)
             (find-least  uncked-list :item item :restlist restlist :nth (incf nth) :ret-n ret-n)))
       (t (setf restlist (append restlist (list item))
                ret-n (+ nth 1))
          ;;uncked-list (cdr uncked-list)
          (multiple-value-setq (least ret-n restlist uncked-list nth)
               (find-least  uncked-list :item testitem :restlist restlist :nth (incf nth) :ret-n ret-n))))
      (values least ret-n restlist uncked-list nth)
      ;;end let, find-least
      ))
;;TEST
;; (find-least '(4 5 1 3 0 7 8 9))
;; works= 0  4 (5 4 3 1 7 8 9) NIL  7
;; (find-least  '(b x d a f m)) 
;; works= A 3 (X D B F M)  NIL  5
      
      
       


;;ORGANIZE-SUBLISTS
;;
;;ddd
(defun organize-sublists (sublists &key randomize descend-sort ascend-sort)
  "In U-lists.lisp, used by screensaver, sorts subdirs. MUST FLATTEN TREE DIRS FIRST--returns orgnaized list"
  (let*
      ((list-length (length sublists))
       (n)
       (sublist)
       (organized-sublists)
       )
    (cond
     (randomize 
      (setf organized-sublists (my-randomize-list sublists)))
     (ascend-sort
      (setf organized-sublists  (my-sort sublists #'test-lessp  :key 'car)))
     ;;2013-12 was (sort sublists #'test-lessp :key 'car))) destructively modifies old list
     (descend-sort
      (setf organized-sublists  (my-sort sublists #'test-greaterp :key 'car)))
     ;;2013-12 was  (sort sublists #'test-greaterp :key 'car)))  
     )
    ;;  (afout 'out (format nil "randomize= ~A descend-sort=~A ascend-sort=~A~%   organized-sublists= ~A~%"  randomize descend-sort ascend-sort organized-sublists))
    organized-sublists))

;;test organize-sublists
#|(defun testsl ()
  (setf out nil)
  (let
      ;;must use flat lists
      ((sublists '((x 1 2)(f 3)(2 6 5)(b 3 2) (99 6 2)))
       (sublists1 '((4 3)(1 6)(8 7 3)(5 a b)))

       (y1)(y2)(y3)(y4)(y5)
       )
    ;;   (sort sublists #'test-lessp :key 'car)))
  (setf y1 (organize-sublists sublists1 :descend-sort t))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
   (setf y2 (organize-sublists sublists1 :ascend-sort t))
;;  (afout 'out (format nil "sublists1= ~A~%" sublists1))
    (setf  y3 (organize-sublists sublists1   :randomize t))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
    (setf y4 (my-sort sublists1 #'test-lessp :key 'car))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
    (setf y5 (my-sort sublists1 #'test-greaterp :key 'car))
   ;;(afout 'out (format nil "sublists1= ~A~%" sublists1))
    (fout out)
    (values y1 y2 y3 y4 y5)
    ))|#
;;works returns
#| ((8 7 3) (5 A B) (4 3) (1 6))  ;;>
((1 6) (4 3) (5 A B) (8 7 3))    ;;<
((8 7 3) (4 3) (1 6) (5 A B))    ;;random
((1 6) (4 3) (5 A B) (8 7 3))    ;;<
((8 7 3) (5 A B) (4 3) (1 6))    ;;>
|#



;;MY-RANDOMIZE-LIST
;;
;;ddd
(defun my-randomize-list (list)  
  "In U-lists.lisp, takes a list or non-nested list-of-lists and non-destructively randomizes it's elements.  Serves as default element in case somehow replacement with real item not made."
  (let*
      ((list-length (list-length list))
       (new-list)  
       (rest-list list)
       (rest-n  list-length)
       ( item)
       (n)
       )
    ;;  (afout 'out (format nil "rest-n= ~A~%" rest-n))
    (loop
     for i from 1 to list-length
     do
     (cond
      ((<  i list-length)
       (setf n  (random (- rest-n 1)))
       (setf item (nth n rest-list))
       (setf new-list (append new-list (list item)))
       ;;may remove wrong item if multiple identical items, tho for randomization, that is ok
       (setf rest-list (remove-list-nth n rest-list))    ;;was (remove item rest-list))
       (decf  rest-n)
       )
      (t 
       (setf item (car rest-list)
             new-list (append new-list (list item)))))
     ;;(afout 'out (format nil "n= ~A new-list= ~A~%rest-list= ~A~%" n new-list rest-list))
     )
    new-list
    ;;end let, my-randomize-list
    ))
;;TEST
;; (my-randomize-list '(a b c d e f g)) 
;;  works= (F B D A E C G)
#|(defun testmr ()
  (setf out nil)
  (let
      ((list '(x a (1 2) (l m n) 99  3))
       )
  ;;  (fout out)
    (my-randomize-lists list)))|#
;;works returns (A (L M N) X 99 (1 2) 3),  ((L M N) 99 X (1 2) A 3), then ((1 2) A 99 (L M N) X 3)




;;MY-RANDOM-SELECTION
;;2016
;;ddd
(defun my-random-selection (list percent &key total-n selections)
  "In U-lists, RETURNS (values selections rest-list)"
  (let
      ((list-n (list-length list))
       (selection)
       (selected-n)
       (percent-used)
       (new-list)
       )
    (cond
     ((> list-n 5000)
      (extend-current-stack 400))
     ((> list-n 3000)
      (extend-current-stack 300))
     ((> list-n 1000)
      (extend-current-stack 100))
     (t nil))      
    (cond
     ((null total-n) 
      (setf total-n list-n
            percent-used 0))
      (t (setf percent-used (* 100 (/ (- total-n list-n) total-n)))))

    (when (and (< percent-used  percent)(> list-n 0))
      (setf selected-n (random list-n))
      (multiple-value-setq (new-list selection)
            (delete-nth list selected-n ))
      (setf selections (append selections (list selection)))
      ;;recurse
      (multiple-value-setq (selections list)
          (my-random-selection new-list percent :total-n total-n
                               :selections selections))
      ;;end when
      )
    (when (= list-n 0)(break))
    (values selections list)
    ;;end let, my-random-selection
    ))
;;TEST
;;  (my-random-selection '(1 2 3 4 5 6 7)  100) = (2 3 6 4 1 7 5)  NIL
;;  (my-random-selection '(1 2 3 4 5 6 7)  0) = NIL  (1 2 3 4 5 6 7)
;;  (my-random-selection '(1 2 3 4 5 6 7)  30) = (7 3 5)   (1 2 4 6)



;;DELETE-ITEMS-FROM-LIST
;;
;;ddd
(defun delete-items-from-list (item-list list 
                             &key from-end (test  'equal)  (start 0) end remove-only-first-p )
  "Uses CL delete.  Inefficient to use because searches entire list for each item in the list. Here count is the TOTAL of all items removed--not separate count for each item removed."
  (let*
      (;;(new-list list)
       (test-list list)
       (newlist-length) 
       (butlast-n 0)
       (result-list)
       (removed-items)
       (removed-items1)
       )
    (if from-end
        (setf test-list (reverse test-list)))
    (setf test-list (nthcdr start test-list))
    (when end
        (setf  newlist-length (list-length test-list)
               butlast-n (- newlist-length end)
               test-list (butlast  test-list butlast-n)))

#|    (loop
     for item in item-list
     do|#
    ;;(afout 'out (format nil "test-list= ~A~%" test-list))
     (loop
      for testitem in test-list
      for n from 0 to (list-length test-list)
      do
     (cond
      ((member testitem item-list :test test)
       (cond
        (remove-only-first-p
         (setf removed-items (append removed-items (list testitem))
               item-list (delete testitem item-list))
         )
        (t  ;;do nothing to result-list
         (setf removed-items (append removed-items (list testitem)))))
       )
      ;;testitem not a member of the item-list
      (t (setf result-list (append result-list (list testitem)))))
         ;;(afout 'out (format nil "result-list= ~A~%" result-list))
     ;;end loop
     )
    (if from-end
        (setf result-list (reverse result-list)))

    (values result-list removed-items)
    ;;end let,delete-items-from-list
    ))
;;TEST
;; (setf out nil)
;;(delete-items-from-list '("SUBSCALE" "COMPOSITE-SCALE" "BIO-TEXT") '("SUBSCALE" "COMPOSITE-SCALE" "BIO-TEXT" "ST1HIGHERSELF" "ST2SOCINTIMNOFAM" "ST3FAMCARE" "ST4SUCCESSSTATUSMATER" "ST5-ORDERP") :test  'equal)
;;works= ("ST1HIGHERSELF" "ST2SOCINTIMNOFAM" "ST3FAMCARE" "ST4SUCCESSSTATUSMATER" "ST5-ORDERP")
;; (delete-items-from-list '(c d x)  '(a b c d e f c c l m x bb  x y z))
;; works = (A B E F L M BB Y Z)  (C D C C X X)
;; (delete-items-from-list '(c d x)  '(a b c d e f c c l m x bb  x y z) :remove-only-first-p T)
;; works = (A B E F C C L M BB X Y Z)  (C D X)
;; (delete-items-from-list '(c d x)  '(a b c d e f c c l m x bb  x y z) :remove-only-first-p T :from-end T)
;;works = (A B C E F C L M X BB Y Z)  (X C D)
;;  ;; (delete-items-from-list '(3  6  8)  '(0 1 2 3 4 5 6 7 8 9  10 11) :start 3 :end 7)
;; works = (4 5 7 9)  (3 6 8)



;;DELETE-KEYS&VALS-FROM-LIST
;;2020
;;ddd
(defun delete-keys&vals-from-list (flat-list &key keys vals (test 'my-equal)  )
  "U-lists. If key or val is member of keys or vals, removes BOTH key & val (assumes value next item).  RETURNS (values new-list deleted-pairs)"
  (let*
      ((new-list)
       (deleted-pair)
       (deleted-pairs)
       (last-item)
       (remove-val-p)
       (len-flat-list (list-length flat-list))
       )
    (loop
     for item in flat-list
     for n from 1 to len-flat-list
     do
     (let*
         ((key)
          (val)
          )
       ;;(afout 'out (format nil "BEGIN: item= ~A last-item= ~A" item last-item))
     (cond
      (remove-val-p
       (setf deleted-pair (append deleted-pair (list item))
            deleted-pairs (append deleted-pairs (list deleted-pair))
            deleted-pair nil
            remove-val-p nil))
     ((member item keys :test 'my-equal)
      (setf remove-val-p T
            deleted-pair (list item)))     
     ((member item vals :test 'my-equal)
      (setf deleted-pair (list (last1 new-list) item)
            deleted-pairs (append deleted-pairs (list deleted-pair))
            new-list (butlast new-list)))
     (T (setf new-list (append new-list (list item)))))
;;    (afout 'out (format nil "new-list= ~A item= ~A last-item= ~A" new-list item last-item))
     ;;end let,loop
     ))
    (values new-list deleted-pairs )
    ;;end let, delete-keys&vals-from-list
    ))
;;TEST
;; (delete-keys&vals-from-list '(:a 1 :b 2 :c nil :d 4 :e nil) :keys '(:b) :vals '(nil 4))
;; works= (:A 1)    ((:B 2) (:C NIL) (:D 4) (:E NIL))



;;DELETE-ITEMS-FROM-LIST-COUNT
;;
;;ddd
(defun delete-items-from-list-COUNT (item-list list 
                             &key from-end (test  'equal) test-not (start 0) end count )
  "Uses CL delete.  Inefficient to use because searches entire list for each item in the list. However, this version has SOME KEYS NOT IN BETTER VERSION."
  (let
      ((new-list list)
       )
    (loop
     for item in item-list
     do
     (setf new-list (delete item new-list  :from-end from-end  :test test  :test-not test-not :start start :end end :count count ))
     ;;end loop
     )
    new-list
    ;;end let,delete-items-from-list
    ))
;;TEST
;;(delete-items-from-list '("SUBSCALE" "COMPOSITE-SCALE" "BIO-TEXT") '("SUBSCALE" "COMPOSITE-SCALE" "BIO-TEXT" "ST1HIGHERSELF" "ST2SOCINTIMNOFAM" "ST3FAMCARE" "ST4SUCCESSSTATUSMATER" "ST5-ORDERP") :test  'equal)
;;works= ("ST1HIGHERSELF" "ST2SOCINTIMNOFAM" "ST3FAMCARE" "ST4SUCCESSSTATUSMATER" "ST5-ORDERP")
;;



;;REMOVE-LIST-NTH
;;
;;ddd
(defun remove-list-nth (nth list &key from-end)
  "Like Remove for lists only, but removes (nondestructively) the nth item. Returns nil if nth > list-length."
  (let
      ((list-length (list-length list))
       (new-list)
       )
    (if  from-end
        (setf nth (- list-length (+ nth 1))))
    (loop
     for element in list
     for i from 0 to (- list-length 1)
     do
     (if (> nth list-length) (return nil))
     (unless (= i nth)
       (setf new-list (append new-list (list element))))
     )
    new-list))

;;test

(defun testmrl ()
  (let ((list '(a b c (1 2 3) d (4 5) e))
        )
    (remove-list-nth 2 list :from-end t)
    ))
;;  (testmrl) nth= 3 works returns (A B C D (4 5) E)
;; nth= 9 returns nil as it should
;; nth= 2 from-end t   returns (A B C (1 2 3) (4 5) E)
    
  ;;  (length '(a b c d))

#|
 (remove 2 '(1 2 3 4 5))
(let ((list '(a b c d))
      (new-list))
      (setf new-list (setf (nth 2 list)  'this))
  list)  ;;replaces 2nd => (A B THIS D)
|#
;;  (testsl)
       ;;NOTE--these functions DESTRUCTIVELY CHANGE THE LISTS, 
       ;; therefore, y1-y5 point to underlying lists THAT HAVE CHANGED??
       ;; SO THEY ALL REPORT THE SAME VALUES??
       ;;--EVIDENCE-- FOUT SHOWS CHANGES, THE VALUES Y1-Y5 DON'T
       ;;  ALSO, these functions WORK IF DONE ALONE, but when repeated with same
      ;;    sublist arg they DO NOT WORK.


;;------- UNDERSTANDING THE SORT FUNCTION ----
;; It DESCRUCTIVELY MODIFIES LISTS, ETC.
;; EXAMPLES
#|
(defun testsort ()
  (let
      ((list '(a x y  b  cat))
       (result)
       )
    (setf result (sort list #'string-lessp))
    (setf rx2 list)
    (values list result)))
;;returns (A B CAT X Y)  (A B CAT X Y);; so changes value of list  (also sets rx2 to changed list)
|#

;;works (sort '(c  x 99 d 3 f 1) #'test-lessp)

;;(sort '((x 1 2)(f 3)(2 6 5)(b 3 2) (99 6 2))  #'test-lessp :key 'car )
#|
(char "xyz" 0) = #\x
(char  897 0) = error
(char  (format nil "~A" 897) 0) = #\8 
;; works (test-car 'this '(this that))
|#

;;test works
#|
(defun tcl ()
  (test-lessp 3 (car '(5 a b))))
;;
(defun tcl2 ()
  (test-lessp 5  (car '(9 21))))
(car '(11 1))

(defun tcl3 ()
  (test-lessp "this" (car '("what" "apple"))))
|#

;;ddd
(defun test-lessp (item1 item2)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'lessp"
  (test-any-type item1 item2 'lessp))

;;ddd
(defun test-greaterp (item1 item2)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'greaterp"
   (test-any-type item1 item2 'greaterp))
;;didn't help  (string-test-any-type item1 item2 'greaterp))

;;ddd
(defun test-equal (item1 item2)
  "In U-lists.lisp, tests item vs (car list) for any type items for  'equalp"
  (test-any-type item1 item2 'equal))  
#|
(test-any-type 9 (car '(11 21)) 'lessp)
(test-any-type 9 11 'lessp)  ;;me, may only evaluate the FIRST char so 11 <  9 
(test-any-type 'y 'x 'lessp)
(string-lessp "31" "21")
(
|#
;;
(defun test-any-type (item1 item2 test)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'lessp 'greaterp or 'equalp, case-insensitive."
  (let
      ((item1-char)   ;;(format nil "~A" item1))
       (item2-char)   ;; (format nil "~A" item2))
       (result)
       )
    ;;test to see if both are numbers--use number tests
    (cond 
     ((and (numberp item1) (numberp item2))
      (setf item1-char item1
            item2-char item2)
      (format t "1= ~A   2=  ~A" item1-char item2-char)
      (cond
       ((equal test 'lessp)
        (setf result (<  item1-char item2-char)))
       ((equal test 'greaterp)
        (setf result  (> item1-char item2-char)))
       ((equal test 'equal)
        (setf result (= item1-char item2-char)))
       (t nil))
      result
      )
     ;;if not convert both to strings--use string tests
     (t 
      (setf item1-char (format nil "~A" item1)
            item2-char (format nil "~A" item2))

      (format t "1= ~A   2=  ~A" item1-char item2-char)
      (cond
       ((equal test 'lessp)
        (setf result (string-lessp item1-char item2-char)))
       ((equal test 'greaterp)
        (setf result  (string-greaterp item1-char item2-char)))
       ((equal test 'equal)
        (setf result (if (stringp item1) (string-equal item1-char item2-char))))
       (t nil))
      ))
    result
    ))

(defun string-test-any-type (item1 item2 test)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'lessp 'greaterp or 'equalp, by CONVERTING ALL TO STRINGS FIRST"
  (let
      ((item1-char)   ;;(format nil "~A" item1))
       (item2-char)   ;; (format nil "~A" item2))
       (result)
       )
      (setf item1-char (format nil "~A" item1)
            item2-char (format nil "~A" item2))
      (format t "1= ~A   2=  ~A" item1-char item2-char)
      (cond
       ((equal test 'lessp)
        (setf result (string-lessp item1-char item2-char)))
       ((equal test 'greaterp)
        (setf result  (string-greaterp item1-char item2-char)))
       ((equal test 'equal)
        (setf result (string-equal item1-char item2-char))) ;;was string-equal
       (t nil))
    result
    ))

#|
(string-lessp "abc" "xyz") = 0
(string-lessp  "xyz" "abc") = nil
(string-lessp  "9" "2") = nil
(string-lessp   "2" "9") = 0
|#
;;works



;;INTEGERS-LIST-P
;;
;;ddd
(defun integers-list-p (list &key exceptions)
 "U-LISTS,  RETURNS N-ITEMS if it is a list with all integers. If exceptions, excepts any item in exceptions list."
 (let
     ((n-items 0)
      )
   (when (listp list)
     (dolist (item list)
       (cond
        ((my-member item exceptions) NIL)
        ((integerp item)
         (incf n-items))
        (t (setf n-items nil)
           (return)))))
   n-items
   ))
;;TEST
;; (integers-list-p '(1 2 3 4 5)) = 5
;; (integers-list-p '(1 2 3  a 4 5))  = NIL
;; (integers-list-p '(1 2 3 TO 4 5 6) :exceptions '(THIS TO)) = 6
;; (integers-list-p '(1 2 3 TO 4 5 6) :exceptions '(THIS "TO")) = 6


;;MAKE-INTEGER-LIST
;;2019
;;ddd
(defun make-integer-list (min-n &optional max-n &key n-ints  reverse-p)
  "U-lists   RETURNS    INPUT:  "
  (unless max-n
    (setf max-n (- (+ min-n n-ints) 1)))                   ;; (+ (- max-n min-n) 1)))
  (let
      ((int-list)
       )
    (loop
     for n from min-n to max-n
     do
     (setf int-list (append int-list (list n)))     
     ;;end let,loop
     )
  (when reverse-p
    (setf int-list (reverse int-list)))
  int-list
    ;;end let, defun
    ))
;;TEST
;; (make-integer-list 3  11)
;;works= (3 4 5 6 7 8 9 10 11)
 















#|(defun testflt ()
  (flatten-list-tree '(1 2 3 4 (a b c) (d (e f (g)) h) (i (j) k))))  |#
#|=> FINAL, flat-list= ((A B C) (D) (E F) (G H) (I) (J K))
 flat-sublist= (J K)
 simple-list= (A B C D E F G H I J K)|#

;;works  (flatten-list-tree '(a b c (d) (e f (g h)))))
;;
;;FLATTEN-LIST-TREE
;;2019 copied from PSeibel's collect-leaves function
;;ddd
(defun flatten-list-tree (tree)
  "Me RETURNS a  flattened tree list"
  (let ((flatlist ()))
    (labels ((walk (tree)
               (cond
                ((null tree))
                ((atom tree) (push tree flatlist))  ;;atom is any nono-cons object (not  X.Y)
                (t (walk (car tree))
                   (walk (cdr tree))))
             ;;end WALK def
             ))
      ;;NOTE: THE FUNCTION CALL IS OUTSIDE THE FUNCT DEF, 
      ;;   but INSIDE LABELS
      (walk tree)     
   ;;end  labels, flatten-list-tree
    (values (nreverse flatlist) tree))))
;;TEST
;; (flatten-list-tree '(a b c (d) (e f (g h))))
;; works = (A B C D E F G H)   (A B C (D) (E F (G H)))
;; ;; (flatten-list-tree '(a (b c (d (e (f ) g ) h) i (j (k) l) m (n)) p))
;; works = (A B C D E F G H I J K L M N P)
;; (A (B C (D (E (F) G) H) I (J (K) L) M (N)) P)
;; ;; (flatten-list-tree '(a (b c (d '(e (f ) g ) h) i (j '(k) l) m (n)) p))


;;FLATTEN-LIST-TREE-DEPRECIATED, less efficient, but works
;;ddd
#|(defun flatten-list-tree (list-tree)
  "In U-lists.lisp, takes a list tree and removes extra parens of sublists to return values
    a flat list ol sublists and single list of all elements"
  (let 
      ((flat-list)
       (flat-sublist)
       (simple-list)
       (sublist)
       (element)
       )
  (multiple-value-setq (flat-list flat-sublist simple-list)
      (flatten-list-tree1 list-tree flat-list simple-list))
  (if flat-sublist (setf flat-list (append flat-list (list flat-sublist))))
 ;;(show-text (format nil "FINAL, flat-list= ~A~% simple-list= ~A~%"flat-list  simple-list) 200 t)
  (values flat-list simple-list)))|#
;;ddd
#|(defun flatten-list-tree1 (list-tree &optional flat-sublist simple-list)
  (let 
      ((last-sublist)
       (flat-list)
       (return-flat-sublist)
       (simple-sublist)
       )
    (cond
     ((> (length list-tree) 0)
      (dolist (element list-tree)
        (cond
         ((null element) nil)
         ((not (listp element))
          ;;   (show-text
          ;;           (format nil "element= ~A~% flat-list= ~A~%" element flat-list) 100 t)
          (setf flat-sublist (append flat-sublist (list element))
                simple-list (append  simple-list (list element))))
         (t
          (if flat-sublist  (setf flat-list (append flat-list (list flat-sublist))))
          (multiple-value-setq (return-flat-sublist flat-sublist simple-sublist)
              (flatten-list-tree1 element)) ;; flat-sublist))  ;; flat-list simple-list)
          ;;either of above returns a list             
          (if  return-flat-sublist  
              (setf flat-list  (append  flat-list  return-flat-sublist)))  ;;was (list flat-sublist))
          (setf  simple-list (append simple-list simple-sublist))))))
     ;;if no more items in dir list, append the flat-sublist to flat-list
     (t nil ))
    #| (show-text (format nil "1 FLATTEN, flat-list= ~a~% flat-sublist= ~a~% simple-list= ~a~% "    flat-list flat-sublist simple-list) 200 T)|#
    (values flat-list flat-sublist simple-list)))|#



;;FLAT-LIST-EQUAL
;;
;;(flat-list-equal '(a b c) '(a b c)) = (A B C) A
;;(flat-list-equal '(a b c) '(a d  c)) = nil B
;;(flat-list-equal '(a b c) '(a b  c d)) = nil nil
;;(flat-list-equal '(a b c d e) '(a b  c d)) = nil E
(defun flat-list-equal (list1 list2)
  "In U-lists.lisp, tests to see if two flat lists are identical; returns values list1 & first item OR nil and first item not equal, "
  (let
      ((item (car list1))
       (return-item)
       (return-list)
       )
    ;;(afout  'eout (format nil "item= ~A  list1= ~A~%list2= ~A~%" item list1 list2))
      (cond
       ((equal item (car list2))
        (setf return-item item
              return-list (cdr list1))
        (unless (null item)
          (multiple-value-setq (return-list return-item) (flat-list-equal return-list (cdr list2)))))
       (t (setq return-list nil 
                return-item item)))
      (values return-list return-item)))



;;FLAT-LIST-EQUAL
;;
;;(flat-list-equal '(a b c) '(a b c)) = (A B C) A
;;(flat-list-equal '(a b c) '(a d  c)) = nil B
;;(flat-list-equal '(a b c) '(a b  c d)) = nil nil
;;(flat-list-equal '(a b c d e) '(a b  c d)) = nil E
(defun flat-list-equal (list1 list2)
  "In U-lists.lisp, tests to see if two flat lists are identical; returns values list1 & first item OR nil and first item not equal, "
  (let
      ((item (car list1))
       (return-item)
       (return-list)
       )
    ;;(afout  'eout (format nil "item= ~A  list1= ~A~%list2= ~A~%" item list1 list2))
      (cond
       ((equal item (car list2))
        (setf return-item item
              return-list (cdr list1))
        (unless (null item)
          (multiple-value-setq (return-list return-item) (flat-list-equal return-list (cdr list2)))))
       (t (setq return-list nil 
                return-item item)))
      (values return-list return-item)))



;;MY-MAKE-LIST
;;
;;ddd
(defun my-make-list (length &key (initial-element 0) incf-by-n delta-element-function fun-rest-args)
  "In U-list.lisp, makes a list length long. If incf-by-n, adds incf-by-n to each element after initial-element.  If delta-element-function = NIL, just uses CL make-list.  If delta-element-function (eg. 'incf), then applies delta-element-function to the previous element= first arg with fun-rest-args as the rest of the args to make the new computed value for the next element."
  (let
      ((element initial-element)
       ;;(new-element)
       (new-list)
       (times-n (- length 1))
       )
    (cond
     ((and (numberp incf-by-n) initial-element)
      (setf new-list (list element))
      (dotimes (n times-n)
        (setf element (+ element incf-by-n)
         new-list (append new-list (list element)))
        ))
     (delta-element-function
      (cond
       (initial-element
        (setf new-list (append new-list (list initial-element))))
        (t (setf times-n length)))
      (dotimes (n times-n)
        (cond
         (fun-rest-args
          (setf element (eval (append `(,delta-element-function ,element) fun-rest-args))))
         (t (setf element (eval `(,delta-element-function ,element)))))
              ;;(apply delta-element-function  (list 'element)))
        (setf new-list (append new-list (list element)))
        ))
     (element
      (setf new-list (make-list length :initial-element element)))
     (t (setf new-list (make-list length))))
    new-list
    ;;end let, my-make-list
    ))
;;TEST
;;SSSS START HERE
;;  (my-make-list 7 :initial-element NIL) = (NIL NIL NIL NIL NIL NIL NIL)
;; (my-make-list 7) = (0 0 0 0 0 0 0)
;;  (my-make-list 7 :initial-element 'a) = (A A A A A A A)
;;  (my-make-list 7 :initial-element 0 :delta-element-function  '+ :fun-rest-args '(1) ) = (1 2 3 4 5 6 7)
;;  (progn (defun myadd3 (x) (+ x 3)) (my-make-list 4 :initial-element 0 :delta-element-function  'myadd3)) = (0 3 6 9)
;;  (my-make-list 7  :delta-element-function  '+ :fun-rest-args '(1) ) = (0 1 2 3 4 5 6)
;; (my-make-list 7 :initial-element 1 :delta-element-function  '+ :fun-rest-args '(1) ) = (1 2 3 4 5 6 7)
;; (my-make-list 7 :initial-element 1    :incf-by-n 2) = (1 3 5 7 9 11 13)



;;MAKE-LIST-N-LONG
; 
;;ddd
(defun make-list-n-long ( length list &key default-element second-list)
  "In U-list.lisp, makes a list length long by starting with list and either adding a second-list or default-element (s) or both to fill in extra length.  NIL used otherwise.  If  length < list length, then it cuts off items. Also can use MAKE-LIST or MY-MAKE-LIST"
  (let
      ((new-list (make-list length :initial-element default-element))
       (list-length (list-length list))
       (second-list-length 0)
       (n2 0)
       (item)
       )
    (if second-list
               (setf second-list-length (list-length second-list)))
    (loop
     for n from 0 to length
     do
     (cond
      ((< n list-length)
       (setf item (nth n list)
        new-list (replace-list new-list n item)))
      ((and (>= n list-length) second-list (< n (+ list-length 1)))  ;; second-list-length -1)))
         (loop
          for item2 in second-list
          for n2 from n to (+ n second-list-length)
          do
           (setf new-list (replace-list new-list n2 item2))
           ;;end 2nd loop and (>= n list-length)
           ))
      (t  (return new-list)))
     ;;(afout  'out (format nil "In make-list-n-long, new-list= ~A~%  n= ~A  n2= ~A list-length= ~A second-list-length= ~A~%" new-list n n2 list-length second-list-length))
     ;;end loop
     )
    new-list
    ))

 ;;test
#|
(defun testmnl ()
  (setf out nil)
  (let
      ((new-list)
       (list '(a b c d))
       (length 9)
       )
 ;; (setf new-list (make-list-n-long length list)) 
  (setf new-list (make-list-n-long length list :default-element 0 :second-list '(1 2 3)))
   ;;works returns (A B C D 1 2 3 0 0)
  (fout out)
  new-list
   ))
|#
;;  (testmnl)
;; with no keys returns (A B C D NIL NIL NIL NIL NIL)
;;with 



;;MAKE-LIST-SYMBOL
;;2019
;;ddd
(defun make-list-symbol (list  &key (separator-str ".") )
  "U-lists  RETURNS (values listsym liststr list-n)    INPUT:   Eg (this a b 22 C x)) =  THIS.A.B.22.C.X "
  (let
      ((listsym)
       (liststr "")
       (list-n (list-length list))
       )
    (loop 
     for item in list
     for n from 1 to list-n
     do
     (cond
      ((= n 1)
       (setf liststr (format nil "~A" item)))
      (t (setf liststr (format nil "~A~A~A" liststr separator-str item))))
     ;;end loop
     )
    (setf listsym (my-make-cs-symbol liststr))
    (values listsym liststr list-n)
    ;;end let, make-list-symbol
    ))
;;TEST
;; (make-list-symbol '(this a b 22 C x)) 
;; THIS.A.B.22.C.X  "THIS.A.B.22.C.X"  6
;; NOTE: (my-make-cs-symbol '(this a b 22 C x)) = *SYM-NOT-FOUND



;;PRINT-LIST-WITH-NEWLINES
;;
;;ddd
(defun print-list-with-newlines (list out-stream &key without-quotes)
  "In U-files.lisp. Prints list with newlines to out-stream -- can be NIL or T. without-quotes only works for lists of strings. NOTE: Prints symbols to CAPS."
  (let
      ((new-list)
       )
    (dolist (item list)
      (cond
       (without-quotes
        (format out-stream "~A" item)(format out-stream "~%"))
       (t 
        (cond
         ((stringp item)
          (format out-stream "~S" item)(format out-stream "~%"))
         (t
          (format out-stream "~C~A~C" #\" item #\")(format out-stream "~%")))))
      )
    new-list
    ))
;; (print-list-with-newlines *spss-var-names8 t :without-quotes nil)
;;(print-list-with-newlines   '(CaseNum  CaseType  Group  var2  FileDate  Instr  Resr  Name  SSN  Sex  Age  Email  ZipCode  Nation  HrsWork  UserRate  tknowmor  texperie  twanttho  twantspe  tworknga  tu100stu) t :without-quotes nil)
;;works, returns; NOTE: ALL ARE IN CAPS
#|"CASENUM"
"CASETYPE"
"GROUP"
"VAR2"
"FILEDATE"
... etc NIL|#
;; (print-list-with-newlines   '(CaseNum  CaseType  Group  var2  FileDate  Instr  Resr  Name  SSN  Sex  Age  Email  ZipCode  Nation  HrsWork  UserRate  tknowmor  texperie  twanttho  twantspe  tworknga  tu100stu) t :without-quotes t)
;;works, returns NOTE: ALL ARE IN CAPS
#|CASENUM
CASETYPE
GROUP
VAR2
FILEDATE
INSTR|#
;;(print-list-with-newlines   '("CaseNum"  "CaseType"  "Group"  "var2")  t :without-quotes nil) = works, returns:
#|"CaseNum"
"CaseType"
"Group"
"var2"
NIL|#
;;  (print-list-with-newlines   '("CaseNum"  "CaseType"  "Group"  "var2")  t :without-quotes t) = works, returns
#|CaseNum
CaseType
Group
var2
NIL|#



;;FIND-GREATEST-MAX-SUBTRACT-LIST
;;
;;ddd
;; NOT USED?--- FIX LATER-DOESN'T RETURN WHAT IT'S SUPPOSED TO
(defun find-greatest-max-subtract-list (list-of-lists)
  "In U-lists.lisp, subtracts each first-item from second-item and returns the entire list from the list-of-lists. RETURNS (values results-list max-dif). Used in fuzzy-matcher."
  (let
      ((item1 0)
       (item2 0)
       (dif 0)
       (new-results-lists)       
       (n -1)
       (max-dif)
       (list-n)
       (result-list)
       )
    (dolist (list list-of-lists)
      (incf n)
      (setf item1 (first list)
            item2 (second list)
            dif  (- item2 item1)
            new-results-lists (append new-results-lists (list (list dif  n list))))
      ;;(afout  'out (format nil "item1= ~A item2= ~A~%  dif= ~A new-results-lists= ~A~%"  item1 item2  dif  new-results-lists))
      ;;end dolist
      )
    (setf max-dif (apply 'max (mapcar #'car new-results-lists)))
    (setf  list-n (get-key-value-in-nested-lists `((,max-dif  0 )) 
                                                 new-results-lists :return-list-p t)
           result-list (third (nth n list-of-lists)))
 ;;end find-greatest-max-subtract-list  
 (values result-list max-dif)
 ))
;;test
;; NOT USED?--- FIX LATER-DOESN'T RETURN WHAT IT'S SUPPOSED TO
;; (progn (setf out nil)  (find-greatest-max-subtract-list '((1 2 a)(1 6 '(a b))(2 5 x))))
;; works, returns
;;  (find-greatest-max-subtract-list '((1 2 a)(1 6 '(a b))(2 5 x)))

  ;;   (nth 1 '(a b c))
  ;;works (apply 'max (mapcar #'car '((1 a)(2 b)(3 c)))) = 3
;;  (apply 'max (mapcar #'car   '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))) (3 2 (2 5 X)))))

#|(defun find-key-value-in-nested-lists (key list-of-lists &key return-list-p
                                           (find-nth 0) (return-nth 0) ) 
  "In U-lists.lisp, RETURNS first value that matches key (values value key). find-nth searches for nth item in the list for the first key. if return-nth, returns nth instead of second item in list. :find-key-in-nested looks for the key in the nth-key of the second order nested lists--not in first list set.  If  return-list-p, returns entire sublist. "
  (let
      ((item)
       (value)
       (list-length (list-length list-of-lists))
       )
    (dolist (list list-of-lists)
      (unless (>= find-nth)
      (setf item (nth find-nth list))
      
      (cond
       ((equal key item)
        (cond
         (return-list-p
          (setf value list))
         ((numberp return-nth)
          (setf value (nth return-nth list)))
         (t (setf value (second list))))
        (return))
       (t nil))
      ;;end do, unless
       ))
    (values value key)
    ))
|#

#| REPLACED BY GET-KEY-VALUE-IN-NESTED-LISTS
(defun find-key-value-in-lists2  (key list-of-lists &key return-list-p
                                           (find-nth 0) (return-nth 1) ) 
  "In U-lists.lisp, RETURNS first value that matches key (values value key). find-nth searches for nth item in the list for the first key. if return-nth, returns nth instead of second item in list. :find-outer-key looks for the key in the nth-key of the second order nested lists--not in first list set.  If  return-list-p, returns entire sublist. "
  (let
      ((result-value)
       (result-key)
       (extra-items)
       )
      (loop
       for item in list-of-lists
       with match-item
       with item-length
       do
       ;;(afout 'out (format nil "LOOP item ~A~%" item))   
       (cond
        ((listp item)
         (setf item-length (list-length item))       
         (unless (>= find-nth  item-length))
         (setf match-item (nth find-nth item))
       ;; (afout 'out (format  nil "TESTING key= ~A  match-item= ~A~%  IN find-key-value-in-lists" key match-item))
         (cond
          ((or (equal key match-item) (if (stringp match-item) (string-equal key match-item)))
           (cond
            (return-list-p
             (setf result-value item
                   result-key match-item))
            (t
             (setf result-value (nth return-nth item)
                   result-key key)))
           (return))
          (t nil))
         ;;end listp item
         )
        ;;if item is not a list, then item may be outer keys or info want to keep
        (t (if  item (setf extra-items (append extra-items (list item))))))
       ;;end loop
       )
  (values result-value result-key extra-items)
  ))|#
;;test
;;  (progn (setf out nil)  (find-key-value-in-lists    "acmESOCSTudy" '(PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")) :return-list-p  t))
;;works, returns ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")  "acmESOCSTudy"  NIL  (PC-INSTANCES "iAcademicMotivation.java")

;;  (find-key-value-in-lists 'this  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL NIL
;;  (find-key-value-in-lists 'x '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns Y X NIL NIL
;;   (find-key-value-in-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL NIL
;; (find-key-value-in-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL NIL
;; (find-key-value-in-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  5 NIL NIL
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))



#| REPLACED BY GET-KEY-VALUE-IN-NESTED-LISTS
(defun find-key-value-in-nested-lists-old (key list-of-lists &key return-list-p
                                 (find-nth 0) (return-nth 1) find-key-in-nested (find-nth-first 0))
  "In U-lists.lisp, RETURNS first value that matches key (values value key outer-items). FIND-NTH searches for nth item in the list for the first key. if RETURN-NTH, returns nth instead of second item in list. :find-outer-key looks for the first key in the nth-key of the second order nested lists--not in first list set. It IGNORES keys in the first list order if = T,  otherwise it searchs only the list with the key set to :find-outer-key.  If  RETURN-LIST-P, returns entire sublist.  :FIND-NTH-FIRST is used only if :find-outer-key is set to a value."
  (let
      ((search-list list-of-lists)
       (list-length (list-length list-of-lists))
       (result-value)
       (result-key)
       (outer-extra-items)
       (inner-extra-items)
       )
    ;;  (afout 'out (format nil "NEW CALL TO FUNCTION key= ~A~% list~A~%" key list-of-lists))
    ;;finds a list to search in outer/first order set of lists (by key)
    ;;SSS debug here
    (cond
     ;;if  T all first-order lists must be searched
     ((equal find-key-in-nested t)
    ;;  (afout 'out (format nil "NEW CALL TO T  key= ~A~% (car of list= ~A~%" key (car list-of-lists)))   
      (loop
       for item  in list-of-lists
       with inner-extra-items1
       with outer-extra-items1
       do
      ;;(afout 'out (format nil "T OUTER-LOOP key= ~A~% first-order-item= ~A~%" key first-order-item))
       (cond
        ((and item (listp item))
         (multiple-value-setq (result-value result-key inner-extra-items1 outer-extra-items1)
             (find-key-value-in-lists  key  item
                                         :find-nth find-nth  :return-list-p t))
         ;;these are the extra items want to return (bec inside of target containing list)
         (if inner-extra-items1
             (setf inner-extra-items (append inner-extra-items (list inner-extra-items1))))
         (if outer-extra-items
             outer-extra-items (append outer-extra-items (list outer-extra-items1)))
         (if result-key
             (return))
         )
        ;;may be non-list items such as other keys providing info re: found outer list.
        (t (setf outer-extra-items (append outer-extra-items (list first-order-item)))))
       ;;end loop equal
       ))

     ;;if first-order key is specified by find-key-in-nested, find that sublist first
     (find-key-in-nested
      (loop
       for first-order-item in list-of-lists
       with match-key1
       with result-key1
       with outer-extra-items1
       with inner-extra-items1
       with search-list
       do
       ;;(afout 'out (format nil "OUTER-LOOP #2 first-order-item= ~A~%" first-order-item))
       (multiple-value-setq (search-list result-key1 outer-extra-items1 inner-extra-items1)
           (find-key-value-in-lists  find-key-in-nested  list-of-lists
                                            :find-nth find-nth-first :return-list-p t))
      


         (if inner-extra-items1
             (setf inner-extra-items (append inner-extra-items (list inner-extra-items1))))
         (if outer-extra-items
             outer-extra-items (append outer-extra-items (list outer-extra-items1)))
         ;;if successful finding match with outer-key in item
         (cond
          (result-key1
           (multiple-value-setq (result-list key search-list inner-extra-items1)
               (find-key-value-in-lists  key  search-list
                                                :find-nth find-nth :return-list-p return-list)))
           (t nil))
         ;;end loop, find-key-in-nested
         ))
     ;;otherwise, normal call to find-key-value-in-lists
     (t 
      (multiple-value-setq (result-list key search-list inner-extra-items1)
               (find-key-value-in-lists  key  search-list 
                                                :find-nth find-nth :return-nth return-nth
                                                :return-list-p return-list))
      ))
 
    ;;end find-key-value-in-nested-lists
    (values result-value  result-key outer-extra-items inner-extra-items)
    ))|#

;;SSS START DEBUG HERE
;;test
;; (progn (setf out nil)  ( find-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances :find-outer-key "iWorldviewFears.java" :find-nth-first 1  :return-list-p t))  
;;works, returns= ("wovNoLove" "16" "wovNoLoveQ" "int" "FrAnswerPanel.Fear7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")"wovNoLove"  NIL     (PC-INSTANCES "iWorldviewFears.java")
;; (progn (setf out nil)  (find-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances :find-outer-key "iWorldviewFears.java" :find-nth-first 1)) ;;  :return-list-p t))
;;works, = "16"  "wovNoLove"  NIL  (PC-INSTANCES "iWorldviewFears.java")
;;
;; (progn (setf out nil) (multiple-value-setq (*testfn1 *testfn2 *testfn3 *testfn4) (find-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances  :find-outer-key  t   :return-list-p t) )(fout out)))
 
;;  (progn (setf out nil)  (find-key-value-in-nested-lists    "acmESOCSTudy" '(PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")) :return-list-p  t))
;;works, returns ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")  "acmESOCSTudy"  NIL  (PC-INSTANCES "iAcademicMotivation.java")

;;  (find-key-value-in-nested-lists 'this  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL NIL
;;  (find-key-value-in-nested-lists 'x '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns Y X NIL NIL
;;   (find-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL NIL
;; (find-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL NIL
;; (find-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  5 NIL NIL
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))



;;JOIN-PARALLEL-LIST-ITEMS
;;
;;ddd
(defun join-parallel-list-items (list1 list2 &key list1-start  list2-start)
  "In U-lists.lisp, takes items in order from 2 lists that match starting with list2-start (default = 0).  Then makes a new list by creating sublists of the matched parallel items. Cuts off extra list1 items; Puts nil in last list2 item if extra list2 items."
  (let
      ((item1)
       (item2)
       (newlist)
       (length-list2 (list-length list2))
       (length-list1 (list-length list1))
       )
    (unless list1-start
      (setf list1-start 0))
    (unless list2-start
      (setf list2-start 0))

    (loop
     for n-item1 from list1-start to length-list1
     for n-item2 from list2-start to length-list2
     do
     (setf item1 (nth n-item1 list1)
           item2 (nth n-item2 list2))
     (unless (null item1)
           (setf newlist (append newlist (list (list item1 item2)))))
     ;;end loop
     )
    newlist
    ))
;;test
;;  (join-parallel-list-items '(a b c d e f) '(x y z))
;;works, returns ((A X) (B Y) (C Z) (D NIL)) ;;NOTE EXTRA ITEM = NIL
;;  (join-parallel-list-items '(a b c d) '(x y z l m n o))    
;;works, returns ((A X) (B Y) (C Z) (D L))


;; JOIN-NESTED-PARALLEL-LIST-ITEMS
;;
;;ddd
(defun join-nested-parallel-list-items (nlist1 nlist2 &key nlist1-start  nlist2-start)
  (let
      ((item1)
       (item2)
       (sublist)
       (new-nlist)
       (length-nlist2 (list-length nlist2))
       (length-nlist1 (list-length nlist1))
       )
    (unless nlist1-start
      (setf nlist1-start 0))
    (unless nlist2-start
      (setf nlist2-start 0))

    (loop
     for n-item1 from nlist1-start to length-nlist1
     for n-item2 from nlist2-start to length-nlist2
     do
     (setf item1 (nth n-item1 nlist1)
           item2 (nth n-item2 nlist2))
     ;;
     (setf sublist (join-parallel-list-items item1 item2))
     (setf new-nlist (append new-nlist (list sublist)))
     ;;end loop
     )
     new-nlist
     ))
;;test
;; (join-nested-parallel-list-items '((a b  c)(d e f)) '((x y z)(l m n)))
;;works, returns  (((A X) (B Y) (C Z)) ((D L) (E M) (F N)) NIL)



;;ADD-SYMS-FROM-PARALLEL-LIST
;;
;;ddd
(defun add-syms-from-parallel-list (list-receive list-send)
  "In U-lists.lisp, Adds an item from one nested list to another nested-list-of-lists. Appends the item to the end of each sublist.  LISTS MUST BE PERFECTLY PARALLEL--except one has items the other sublists parallel to the items."
  (let
      ((send-sym)
       (n1 -1)
       (n2 -1)
       (rec-sublist)
       (rec-sublist-list)
       (new-rec-sublist)
       (new-rec-sublist-list)
       (new-rec-list)
       )
    ;;get group lists
    (dolist (send-sublist list-send)
      (incf n1)
      (setf rec-sublist (nth n1 list-receive))
      ;;get sublists and specific items
      (dolist (send-sublist-item send-sublist)
        (incf n2)
          (setf rec-sublist-list (nth n2 rec-sublist))
        (cond
         ((> n2 0) 
          (setf new-rec-sublist-list (append  rec-sublist-list  (list send-sublist-item))
                new-rec-sublist (append new-rec-sublist (list new-rec-sublist-list))))
         (t (setf new-rec-sublist (append new-rec-sublist (list send-sublist-item)))))
        ;;(afout 'out (format nil "new-rec-sublist= ~A~%" new-rec-sublist))
        ;;end inner dolist
        )
           
      (setf new-rec-list  (append new-rec-list (list new-rec-sublist)))
            (setf new-rec-sublist-list nil
                  new-rec-sublist nil
                  n2 -1)
      ;;end outer dolist
      )
    new-rec-list
    ))
;; test
;;  (progn (setf out nil) (setf *outputx2 (add-syms-from-parallel-list *add-to-this *from-here  )))
;;  (setf *output2p (print-nested-list *outputx2  :stream t :incl-quotes-p t :no-outer-parens-p t))



;;GET-LIST2-ITEM-FROM-LIST1-MATCHED-ITEM
;;2019
;;ddd
(defun get-list2-item-from-list1-matched-Nth-item (item list1 list2
                                                    &key (test 'my-equal))
  "   RETURNS (values  matched-list2-item  matched-list1-item  nth-item)  nth-item begins w/ 0."
  (let
      ((matched-list1-item)
       (matched-list2-item)
       (nth-item)
       )
    (multiple-value-setq (matched-list1-item nth-item)
        (find-list-item item list1 :return-first-p T))
    (setf matched-list2-item (nth nth-item list2)) 
    (values  matched-list2-item  matched-list1-item  nth-item)
    ;;end let, get-list2-item-from-list1-matched-Nth-item
    ))
;;TEST
;;  (get-list2-item-from-list1-matched-Nth-item "this" '(a b "this" "what" d) '(11 22 33 44 55))
;; works=  33  "this"   2





;;MAKE-ASCENDING-LIST
;;
;;ddd
(defun make-ascending-list (lowest-num list-length)
  "In U-lists.lisp"
    (loop
     for n from lowest-num to (+ lowest-num list-length -1)
     collect n)
    )
;;test, works
;;  (make-ascending-list 5 10) = (5 6 7 8 9 10 11 12 13 14)

;;MAKE-DESCENDING-LIST
;;
;;ddd
(defun make-descending-list (highest-num list-length)
  "In U-lists.lisp"
     (loop
      for n downfrom highest-num  downto (+ (- highest-num list-length) 1)
      collect n)
    ) 
;;test, works
;;  (make-descending-list 14 10) = (14 13 12 11 10 9 8 7 6 5)





;;APPEND-NTH-ITEM
;;ddd
(defun append-nth-item (nth new-item list &key (append-to-short-list-p t) append-nth-item-p)
  "In U-lists, NOTE: first item= 1. If append-to-short-list-p = T, appends new-item to the end of a list that is too short. nth can be keyword :last. RETURNS (values new-list old-value new-appended-item). If APPEND-NTH-ITEM-P, appends the nth item in list if it a list.and makes a new sublist of (old-value new-item) in nth place."
  (let
      ((length-list (list-length list))
       (new-list)
       (new-appended-item)
       (old-value)
       )
    ;;Is it a simple add to end of list?
  (cond
        ((and (or (<= length-list nth) (equal nth :last))
              append-to-short-list-p)
         (setf old-value (car (last list))
               new-appended-item new-item
               new-list (append list (list new-item))))
        ;;otherwise must loop
        (append-nth-item-p
         (setf old-value (nth  nth list))
         (cond
          ((listp old-value)
           (setf new-appended-item (append old-value (list new-item))))
          (t (setf new-appended-item (list old-value new-item))))
         (setf new-list 
               (append (butlast list (- length-list nth))(list new-appended-item)(nthcdr (+ nth 1) list))))
        (t
         (setf old-value (nth nth list)
               new-appended-item new-item
               new-list (append (butlast list (- length-list (+ nth 1)))(list new-item)(nthcdr (+ nth 1)  list)))
         ;;(break "old-value")
         ))
    (values new-list old-value new-appended-item)
    ;;end let,append-nth-item
    ))
;;TEST
;;  (append-nth-item 3 "this" '(a b c d e f g h))
;; works = (A B C D "this" E F G H)  D    "this"
;;  (append-nth-item 3 "this" '(a b c d e f g h) :APPEND-NTH-ITEM-P T) 
;;works=(A B C (D "this") E F G H)  D  (D "this")
;; ;;  (append-nth-item 3 "this" '(a b  (c)  d e f g h) :APPEND-NTH-ITEM-P T) 
;; works =(A B (C) (D "this") E F G H)   D   (D "this")
;; (append-nth-item 4 "this" '(a b c d )) = (A B C D "this")   D   "this"
;;  (append-nth-item 7 "this" '(a b c d )  :append-to-short-list-p T) 
;;   works = (A B C D "this")   D   "this"
;;   not append item but list
;; (append-nth-item 5 99 '(KEY1 XX VAL1 A B OLDTARGX 1 2 3 4 5 6 7))
;;  (KEY1 XX VAL1 A B OLDTARGX 99 1 2 3 4 5 6 7)   OLDTARGX  99
;; append item in list
;;  (append-nth-item 5 99 '(KEY1 XX VAL1 A B OLDTARGX 1 2 3 4 5 6 7) :append-nth-item-p T)
;; works= (KEY1 XX VAL1 A B (OLDTARGX 99) 1 2 3 4 5 6 7)   OLDTARGX (OLDTARGX 99)
;; append list-item in list
;; (append-nth-item 5 99 '(KEY1 XX VAL1 A B (OLDTARGX) 1 2 3 4 5 6 7) :append-nth-item-p T)
;; works=  (KEY1 XX VAL1 A B (OLDTARGX 99) 1 2 3 4 5 6 7)    (OLDTARGX)  (OLDTARGX 99)





;;APPEND-NTH-ITEM-IN-NESTED-LISTS
;;ddd
(defun append-nth-item-in-nested-lists (nth new-item nested-list)
  "In U-lists, first item= 0. Appends ALL lists with new-item at nth position."
  (let
      ((new-nested-list)
       )
    (loop
     for list in nested-list
     do
     (cond
      ((listp list)
     (setf new-nested-list
           (append new-nested-list (list (append-nth-item nth new-item list)))))
      (t (setf new-nested-list
           (append new-nested-list (list list)))))
     ;;end loop
     )
    new-nested-list
    ;;end let, append-nth-item-in-nested-lists
    ))
;;TEST
;;  (append-nth-item-in-nested-lists  2  "this" '((1 2 3 4)nonlist(a b c d e f g h)(m n o p)))
;; works = ((1 2 "this" 3 4) NONLIST (A B "this" C D E F G H) (M N "this" O P))
;;


;;APPEND-NTH-ITEM-IN-2ND-NESTED-LISTS
;;
;;ddd
(defun append-nth-item-in-2nd-nested-lists (nth new-item double-nested-list) 
  "In U-lists, first item= 0. Appends ALL 2nd-nested lists with the item at position nth."
  (let
      ((new-2nested-list)
       )
    (loop
     for nested-list in double-nested-list
     do
     (cond
      ((listp nested-list)
       (setf new-2nested-list 
             (append new-2nested-list 
                     (list (append-nth-item-in-nested-lists nth new-item nested-list)))))
      (t (setf new-2nested-list 
               (append new-2nested-list (list nested-list)))))
     ;;end loop
     )
    new-2nested-list
    ;;end let, append-nth-item-in-2nd-nested-lists
    ))
;;TEST
;;  (append-nth-item-in-2nd-nested-lists  2 "TEST" '((A (1 2 3 4)) (B (5 6 7) ) ((9 10 11) NO)(C (a b c d e))))
;;works = ((A (1 2 "TEST" 3 4)) (B (5 6 "TEST" 7)) ((9 10 "TEST" 11) NO) (C (A B "TEST" C D E)))
;; (append-nth-item-in-2nd-nested-lists 2 '(this is a test) '((A (1 2 3 4)) (B (5 6 7) ) ((9 10 11) NO)(C (a b c d e))))
;;works = ((A (1 2 (THIS IS A TEST) 3 4)) (B (5 6 (THIS IS A TEST) 7)) ((9 10 (THIS IS A TEST) 11) NO) (C (A B (THIS IS A TEST) C D E)))
;;
     


;;APPEND-NTH-NESTED-LIST
;;
;;ddd
(defun append-nth-nested-list (new-item nth  nested-list &key (nth-item :last) 
                                        nth-list)  ;;create-new-lists-p)
  "In U-lists, In a nested-list, puts an item in the nth-list in position nth-item. Both nth-list and nth-item can use keyword :last. Some items can be non-lists, but still count in the nth count. nth begins with 1 not 0. If NTH-LIST = integer, counts lists instead of items." ;; If CREATE-NEW-LISTS-P, creates new lists if none existed."
  (let
      ((new-nested-list)
       (n 0)
       (listn 0)
       (nth-item1)
       )
    (multiple-value-bind ( list-length n-lists n-symbols n-strings n-numbers) 
        (my-list-length nested-list)

#|      (when (and (null nested-list) create-new-lists-p)
        (loop
         for n from 1 to nth
         do
         (setf nested-list (append nested-list (list nil))
         )))
      (break)|#
    
    (loop
     for item in nested-list
     do
     (incf n)
     (cond
      ((listp item)
       (incf  listn)
       (cond
        ((or (and nth-list (= nth-list listn))
             (and (equal nth-list :last) (= nth-list listn))
             (and (equal nth :last)(= listn n-lists))
             (and (numberp nth)(= nth n)))
         (if (equal nth-item :last)
             (setf nth-item1 :last)
           (setf nth-item1 (- nth-item 1)))
         ;;changes item
         (setf  item (append-nth-item nth-item1 new-item   item))
         ;;SSS START HERE
         )
        (t nil)))
      (t nil))
     ;;append new-nested-list with modified or unmodified item
      (setf new-nested-list (append new-nested-list (list item)))
      ;;end loop
      )
    new-nested-list
    ;;end let,mvb,append-nth-nested-list
    )))
;;TEST
;; (append-nth-nested-list '(xxxx) 2  '((list) a (a b c) d (1 2) 7 b))
;; result= ((LIST) A (A B C) D (1 2) 7 B), not append bec nth 2 not a list
;; (append-nth-nested-list '(xxxx) 3  '((list) a (a b c) d (1 2) 7 b))
;; works= ((LIST) A (A B C (XXXX)) D (1 2) 7 B)
;;  (append-nth-nested-list '(xxxx) 3  '((list) a (a b c) d (1 2) 7 b) :nth-item 2)
;; works= ((LIST) A (A (XXXX) B C) D (1 2) 7 B)
;; (append-nth-nested-list '(xxxx) nil  '((list) a (a b c) d (1 2) 7 b) :nth-item 2 :nth-list 3)
;; works= ((LIST) A (A B C) D (1 (XXXX) 2) 7 B)  
;; IF LIST = NIL
;; (append-nth-nested-list 'THIS 0 NIL :create-new-lists-p T)
;;   
;;
;;From ART
;;  (append-nth-nested-list   '(3 (280 0.08280101)  "Wup3-5" (3 5)) :last  '(:DIMSIM 5 ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (240 0.063878) "Wup2-5" (2 5)))))
;; works= (:DIMSIM 5 ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (240 0.063878) "Wup2-5" (2 5)) (3 (280 0.08280101) "Wup3-5" (3 5))))

;;  (append-nth-nested-list  '(4 (320 0.05166) "Wup4-5" (4 5)) :last  '(:DIMSIM 5 ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (240 0.063878) "Wup2-5" (2 5)) (3 (280 0.08280101) "Wup3-5" (3 5)))))
;;works= (:DIMSIM 5 ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (240 0.063878) "Wup2-5" (2 5)) (3 (280 0.08280101) "Wup3-5" (3 5)) (4 (320 0.05166) "Wup4-5" (4 5))))



;;APPEND-NTH-LIST
;;2016
;;ddd
(defun append-nth-list (item nth list-of-lists &key if-not-found-create-p)
  "In U-lists, nth begins with 0 RETURNS (values newlists newlist). If :IF-NOT-FOUND-CREATE-P, creates a list of nils to nth."
  (let
      ((newlists)
       (newlist)
       (len-lists (list-length list-of-lists))
       )
    ;;IF NEED TO MAKE LIST OR MAKE IT LONGER
    (when if-not-found-create-p
      (cond     
       ((null list-of-lists)
        (setf list-of-lists (my-make-list (+ nth 1) :initial-element NIL)))      
       ((< len-lists nth)
        (setf list-of-lists (append list-of-lists 
                                    (my-make-list (+ (- nth len-lists) 1) :initial-element NIL))))
       (t nil)))    

    (loop
     for list in list-of-lists
     for n from 0 to (list-length list-of-lists)
     do
     (cond
      ((= nth n)
       (setf newlist (append list (list item))
             newlists (append newlists (list newlist) (nthcdr (+ n 1) list-of-lists)))
       (return))
      (t (setf newlists (append newlists (list list)))))
     ;;end loop
     )
    (values newlists newlist)
    ;;end let, append-nth-list
    ))
;;TEST
;;  (append-nth-list  'THIS 1 '((A B C)(D E F)(H I J)))
;;  ((A B C) (D E F THIS) (H I J))     (D E F THIS)
;;  if nil
;;  (append-nth-list  'THIS 1 nil) = NIL
;;  (append-nth-list  'THIS 1 nil :if-not-found-create-p T) = (NIL (THIS))   (THIS)
;; (append-nth-list  'THIS 2 '((a b c)) :if-not-found-create-p T)
;; works= ((A B C) NIL (THIS))    (THIS)



;;APPEND-GREATEST-KEYVALUE-NESTED-LIST
;;2017
;;ddd
(defun append-greatest-keyvalue-nested-list (match-spec-list test-spec-list 
                                                           all-nested-lists new-nested-list 
                                                           &key (replace-test 'my-greaterp))
  "In U-lists. Searches all-nested-lists for lists with MATCHING spec-lists (eg keyvalues).  If two lists match on that keyvalue (etc), then KEEPS list with GREATEST keyvalue (etc) from test-spec-list.  REPLACES APPEND-NO-DUPLICATES for nested-lists, but can do much more also. If no matches, found simply appends the new-nested-list to the all-nested-lists.  (all-nested-lists items can be of any type).
  RETURNS (values new-list rejected-items)"
  (let
      ((new-list)
       (rejected-items)
       )
    (loop
     for testlist in all-nested-lists
     do
     ;;(afout 'out (format nil "testlist= ~A" testlist))
     (cond
      ((listp testlist)
     ;;FIND THE  MATCH KEYVALUES?
     (multiple-value-bind ( T-match-value) ;; Treturn-keylist Tnew-keylist 
         ;;Treturn-nested-lists T-last-key-found-p )
         (get-keyvalue-in-nested-list match-spec-list testlist)   

       (multiple-value-bind ( N-match-value) ;;Nreturn-keylist Nnew-keylist 
           ;;Nreturn-nested-lists N-last-key-found-p )
           (get-keyvalue-in-nested-list match-spec-list new-nested-list)   

         ;;(afout 'out (format nil "T-match-value= ~A VS  N-match-value= ~A" T-match-value N-match-value))
         ;;ARE THE TWO MATCH KEYVALUES MY-EQUAL?
         (cond
          ((my-equal T-match-value N-match-value)
           ;;For testlist
           (multiple-value-bind ( test-value1 return-keylist1 new-keylist1
                                              return-nested-lists1  last-key-found-p1)
               (get-keyvalue-in-nested-list test-spec-list testlist)
             ;;For new-nested-list
             (multiple-value-bind ( test-value2 return-keylist2 new-keylist2 
                                                return-nested-lists2  last-key-found-p2 )
                 (get-keyvalue-in-nested-list test-spec-list new-nested-list)
               ;;(afout 'out (format nil "For testlist= ~A~%vs new-nested-list= ~A~%  test-value1= ~A vs test-value2= ~A"         testlist new-nested-list test-value1 test-value2))
               ;;SSSS START HERE TESTING, TEST-VALUE1 SEEMS TO MATCH NON-MATCH TEST-VALUE2

               ;;TEST TO SEE IF new-nested-list IS MY-GREATER-P, etc THAN TESTLIST 
               ;;  on the (second) test

               (when (symbolp 'test-value1)
                 ;;(break "1")
                 (setf test-value1 (quote test-value1)))
               (when (symbolp test-value2)
                 (setf test-value2 (quote test-value2)))
               ;;(break "here")
               (cond
                ((funcall replace-test test-value2 test-value1)
                 ;;was (replace-test test-value2 test-value1))
                 (setf new-list (append new-list (list new-nested-list))
                       rejected-items (append rejected-items (list testlist))))
                (t (setf new-list (append new-list (list testlist))
                         rejected-items (append rejected-items (list new-nested-list)))))
               ;;end mvb, mvb,MY-EQUAL
               )))
          ;;not equal, so append item to new-list
          (t
           (setf new-list (append new-list (list testlist)))))
         ;;(break "end loop")
         ;;end mvbs
         ))
       ;;end listp testlist
         )
      (t (setf new-list (append new-list (list testlist)))))
     ;;end loop
     )
    (values new-list rejected-items)
    ;;end let, append-greatest-keyvalue-nested-list
    ))
;;TEST
;; (append-greatest-keyvalue-nested-list '((:key2 0)) '((:key3 t)) '(symbol1  (oldlist1 a b (:key2 val1 1 2) c d (1 2 3 :key3  testval1 4 5) x y z)    (old-list2 a b (:key2 matchval 1 2) c d (1 2 3 :key3  testval1 4 5) x y z) (non match list)  symbol)  '(new list l m n (:key2 matchval 1 2) c d (1 2 3 :key3  testval2 6 7) x y z))
;;works= (SYMBOL1 (OLDLIST1 A B (:KEY2 VAL1 1 2) C D (1 2 3 :KEY3 TESTVAL1 4 5) X Y Z) (NEW LIST L M N (:KEY2 MATCHVAL 1 2) C D (1 2 3 :KEY3 TESTVAL2 6 7) X Y Z) (NON MATCH LIST) SYMBOL)
;;((OLD-LIST2 A B (:KEY2 MATCHVAL 1 2) C D (1 2 3 :KEY3 TESTVAL1 4 5) X Y Z))
;; (my-greaterp 'test-value2 'test-value1) = 10
;; (my-greaterp 'test-value1 'test-value2) = NIL



;;APPEND-NO-DUPLS SSSSS-REPLACE APPENDS IN GLOBAL LISTS
;;2020
;;ddd
(defun append-no-dupls (list &rest items )
  "In U-lists.lisp.  Appends list with all sublist items and item if each is NOT a member of list [test= 'my-equal]. APPENDS LIKE normal APPEND.   RETURNS (values new-list dupl-items)"
  (let*
      ((new-list list)
       (dupl-items)
       )
    (loop
     for item in items
     do
     (cond
      ((listp item)
       (loop
        for subitem in item
        do        
        (cond
         ((member subitem new-list :test 'my-equal)
          (setf dupl-items (append1 dupl-items subitem)))
         (T (setf new-list (append1 new-list subitem))))
       ;;end loop, listp
       ))
       (T 
        (multiple-value-bind (new-list1 dupl-items1)
           (append-item-no-dupls new-list item)
         (setf new-list  new-list1
               dupl-items (append dupl-items dupl-items1)))))
     ;;end loop
     )
    (values new-list dupl-items)
    ;;end append-no-dupls
    ))
;;TEST
;;  (append-no-dupls '(a b c d e)  '(b) '(x) '(d) '(y))
;; works=  (A B C D E X Y)    (B D)
;;  (append-no-dupls '(A B C D)  '(B D X))
;; works= (A B C D X)  (B D) [like normal append]
;; (append-no-dupls '(A B C D)  '(B D X)  'Y  'A)
;; makes dotted list = (A B C D X . Y)   (B D A)
;; (append-no-dupls '(A B C D)  '((B D X))  (list 'Y  'A) (list '(a b z)))
;; works like normal append=
;; (A B C D (B D X) Y (A B Z))    (A)  [note: A in last list is double nested]




;; APPEND-NO-NTH-SUBLIST-DUPLS
;;2020
;;ddd
(defun append-no-nth-sublist-dupls (nth list1 list2 &key (test 'my-equal)
                                        (append-only-lists-p T))
  "In U-lists.lisp.  USE for appending SYM&SYMVALS lists w/ no dupls. Appends list1 with all sublist items in list2 if each sublist does NOT contain an nth value that is the same as the nth value of any list1 sublist. [test= 'my-equal]. APPENDS LIKE normal APPEND.   RETURNS (values new-list dupl-items)"
  (let*
      ((newlist list1)
       (rejected-items)
       (list1-sublist-nth-items)
       )
    ;;First make list of all list1 nth items
    (loop
     for item in list1
     do
     (when (listp item)
       (setf list1-sublist-nth-items (append1 list1-sublist-nth-items  (nth nth item))))
     )
   ;;Test list2 nth item to see if append to list1
    (loop
     for item in list2
     do
     (cond
      ((listp item)
       (cond
        ((member (nth nth item) list1-sublist-nth-items :test test)
         (setf rejected-items (append1 rejected-items item)))
        (T (setf newlist (append1 newlist item)))))
       ((null append-only-lists-p)
        (setf newlist (append1 newlist item)))
       (T (setf rejected-items (append1 rejected-items item))))
       ;;end loop
       )
    (values newlist rejected-items)
    ;;end append-no-nth-sublist-dupls
    ))
;;TEST
;; (append-no-nth-sublist-dupls 0 '((a b  c)(d e f)(g h i)) '((l m n)(d h i)(p q r) 77 88))
;; works= ((A B C) (D E F) (G H I) (L M N) (P Q R))   ((D H I) 77 88)
;; if 
;; (append-no-nth-sublist-dupls 0 '((a b  c)(d e f)(g h i)) '((l m n)(d h i)(p q r) 77 88) :append-only-lists-p NIL)
;;works= ((A B C) (D E F) (G H I) (L M N) (P Q R) 77 88)    ((D H I))




;;APPEND-ITEM-NO-DUPLS
;;2020
;;ddd
(defun append-item-no-dupls (list &rest items )
  "In U-lists.lisp.  Appends list only if item isn NOT a member [test= 'my-equal] Also see APPEND-NO-DUPLICATES (which also checks sublist items to see any items are a member of list). Do not add (list item) to append a list."
  (let*
      ((new-list list)
       (dupl-items)
       )
    (loop
     for item in items
     do
     (cond
      ((member item list :test 'my-equal)
       (setf dupl-items (append1 dupl-items item)))
      (T (setf  new-list (append new-list  item))))
     ;;end loop
     )
    (values new-list dupl-items)
    ;;end append1-item-no-dupls
    ))
;;TEST
;; (append-item-no-dupls '(a b c d e)  '(b) '(x) '(d) '(y))
;; works [note: b NOT= (b) etc] = (A B C D E B X D Y)  NIL
;; ;; (append-item-no-dupls '(a b c d e)  'b 'x 'd 'y)  = ERROR
;;works = ERROR (like normal append)
;; (append-item-no-dupls '(a b c d e)  '(b c x))
;; works= (A B C D E B C X)    NIL


;;APPEND1-NO-DUPLS
;;2020
;;ddd
(defun append1-no-dupls (list &rest items )
  "In U-lists.lisp.  Appends list with all sublist items and item if each is NOT a member of list [test= 'my-equal]. APPENDS LIKE APPEND1.
  RETURNS (values new-list dupl-items)"
  (let*
      ((new-list list)
       (dupl-items)
       )
    (loop
     for item in items
     do
     (cond
      ((listp item)
       (loop
        for subitem in item
        do
       (multiple-value-bind (new-list1 dupl-items1)
           (append1-item-no-dupls new-list subitem)
         (setf new-list  new-list1
               dupl-items (append dupl-items dupl-items1)))
       ;;end loop, listp
       ))
       (T 
        (multiple-value-bind (new-list1 dupl-items1)
           (append1-item-no-dupls new-list item)
         (setf new-list  new-list1
               dupl-items (append dupl-items dupl-items1)))))
     ;;end loop
     )
    (values new-list dupl-items)
    ;;end append1-no-dupls
    ))
;;TEST
;; (append1-no-dupls '(A B C D)  '(B D X)  'Y  'A)
;;works= (A B C D X Y)   (B D A)
;;(append1-no-dupls '(A B C D)  '(B D X)  99 )
;; works= (A B C D X 99)  (B D)
;; note traditional append main-list (list sublist) to created (main
;; ((append1-no-dupls '(A B C D)  '((B D X))  (list 'Y  'A) (list '(a b z)))
;; works= (A B C D (B D X) Y (A B Z))   (A)
;; (append1-no-dupls '(a b c d e)  '(b) '(x) '(d) '(y))
;; works= (A B C D E X Y)   (B D)



;;APPEND1-ITEM-NO-DUPLS
;;2020
;;ddd
(defun append1-item-no-dupls (list &rest items )
  "In U-lists.lisp.  Appends list only if item isn NOT a member [test= 'my-equal] Also see APPEND-NO-DUPLICATES (which also checks sublist items to see any items are a member of list). Do not add (list item) to append a list."
  (let*
      ((return-list list)
       (dupl-items)
       )
    (loop
     for item in items
     do
     (cond
      ((member item list :test 'my-equal)
       (setf dupl-items (append1 dupl-items item)))
      (T (setf  return-list (append1 return-list  item))))
     ;;end loop
     )
    (values return-list dupl-items)
    ;;end append1-item-no-dupls
    ))
;;TEST
;; (append1-item-no-dupls '(a b c d e)  '(b) '(x) '(d) '(y))
;; works [note: b NOT= (b) etc] = (A B C D E (B) (X) (D) (Y))   NIL
;; ;; (append1-item-no-dupls '(a b c d e)  'b 'x 'd 'y)
;; works = (A B C D E X Y)    (B D)
;; (append1-item-no-dupls '(a b c d e)  '(b c x) 'a )
;; works= (A B C D E (B C X))    (A)




;;APPEND-NO-DUPLICATES
;;  NOTE: use APPEND-GREATEST-KEYVALUE-NESTED-LIST for nested-lists to get no duplicates OR to append the list with 1-matched keyvalue, 2-replace if greater than keyvalue
;;
;;ddd
(defun append-no-duplicates (list &rest items)
  "In U-lists.lisp. REPLACED BY APPEND-NO-DUPLS  Use adjoin instead? or use instead of append sometimes. Appends items to list that NEITHER THE ITEM OR ANY SUBITEM match an item in list. Note: subitems of LIST are NOT searched. Uses string-equal for strings. Otherwise works like append. NOTE: use APPEND-GREATEST-KEYVALUE-NESTED-LIST for nested-lists."
  (let
      ((search-item)
       (return-list list)
       (duplicate-items-list)
       (found-list)
       )
    (loop
     for subitem in items
     do
     (cond
      ((listp subitem)
       (cond
        ;;is whole list same as another whole list?
        ((setf found-list (find-item-or-subitem-in-list subitem list))
         (cond
          ((equal (car found-list) subitem)
           (setf duplicate-items-list (append duplicate-items-list (list subitem))))
          ;;not equal a whole list
          (t
           (loop
            for item1 in subitem
            do
            (cond 
             ((find-item-or-subitem-in-list item1 list)
              (setf duplicate-items-list (append duplicate-items-list (list item1))))
             (t (setf  return-list (append return-list (list item1)))))
            ;;end inner loop, setf found-list
            ))))
        (t
         (loop
          for item in subitem
          do
          (cond
           ((find-item-or-subitem-in-list item list)
            (setf duplicate-items-list (append duplicate-items-list (list item))))
           (t (setf  return-list (append return-list (list item)))))  
          ;;end loop, cond, listp
          ))))
      ;;subitem not a list
      (t
       (cond
        ((find-item-or-subitem-in-list subitem list)
         (setf duplicate-items-list (append duplicate-items-list (list subitem))))
        (t (setf  return-list (append return-list (list subitem)))))
       ;;end outer t, cond
       ))
     ;;end outer loop
     )
    (values return-list duplicate-items-list)
    ;;end append-no-duplicates
    ))
;;TEST works
;;  (append-no-duplicates '(a b c) "this" '(x b) '(4 "that") 'c) 
;; works= (A B C "this" X 4 "that")   (B C)
   ;;old, wrong= (A B C "this" (4 "that"))   ((X B) C) 
;;  (append-no-duplicates '((a b c) (x y z)(1 2 3)) '(x y z)) = ((A B C) (X Y Z) (1 2 3))      ((X Y Z))
;;  (append-no-duplicates '((a b c) (x y z)(1 2 3)) '(x b z)) =  ((A B C) (X Y Z) (1 2 3) (X B Z))   NIL
;; ;;  (append-no-duplicates   '((:DRIVE SAMSUSB-128 (:NAME "SAMSUSB-128" :HOST "G:\\"))) (list '(:DRIVE SAMSUSB-128 (:NAME "SAMSUSB-128" :HOST "G:\\"))))
;; works = ((:DRIVE SAMSUSB-128 (:NAME "SAMSUSB-128" :HOST "G:\\")))   ((:DRIVE SAMSUSB-128 (:NAME "SAMSUSB-128" :HOST "G:\\")))





;;APPEND-LISTS
;;
;;ddd
(defun append-lists (list &rest new-items) 
  "In U-lists, If new-istems is a list, then it APPENDS EACH ITEM separately to the list (not the list of items). eg. result is  (orig-list A B C D) not  (orig-list (A) (B)(C)(D)) for new-lists '((A)(B)(C)(D)). NOTE: If  an item in new-items is not a list still appends it--removes parens if they exist, not attempt if they don't. "
  (let
      ((newlist list)
       )
    ;;what kind of objects are in new-items?  append appropriately
    (cond
     ((listp new-items)
      (loop
       for item in new-items
       do
       (cond
        ((listp item)
         (setf newlist (append newlist item)))
        (t (setf newlist (append newlist (list item)))))
       ;;end loop, listp
       ))
     (t (setf newlist (append newlist (list new-items)))))

    newlist
    ;;end let, append-lists
    ))
;;TEST
;;  (append-lists '(orig-list (a b c))  '(list1) 'a  '(list2 1 2 3) 'b '(list3 4 5))
;; works = (ORIG-LIST (A B C) LIST1 A LIST2 1 2 3 B LIST3 4 5)
;;
;; ;;  (append-lists (list (car '(orig-list (a b c)))) (list (second '(orig-list (a b c)))) '(list1) 'a '(list2 1 2 3) 'b '(list3 4 5) '(also this))




;;APPEND-1NESTED-LISTS
;;
;;ddd
(defun append-1nested-lists (list &rest new-items) 
  "In U-lists, If new-istems is a list, then it APPENDS EACH ITEM separately to the list (not the list of items). Also, removes parens from items 1 nesting level inside. eg. result is  (orig-list (1 2 3) A B C D E) not (orig-list (1 2 3) (A)(B)(C)(D) E)  for new-lists  '((A))((B C)) '(D) 'E . NOTE: If  an item in new-items is not a list still appends it--removes parens if they exist, not attempt if they don't. NOTE: to APPEND NESTED ITEMS IN ORIGINAL LIST use NIL first"
  (let
      ((newlist list)
       )
    ;;what kind of objects are in new-items?  append appropriately
    (cond
     ((listp new-items)
      (loop
       for item in new-items
       do
       (cond
        ((listp item)
         (loop
          for initem in item
          do
          (cond
           ((listp initem)
            (setf newlist (append newlist initem)))
           (t (setf newlist (append newlist (list initem)))))
        ;;end inner loop, listp
        ))
        (t (setf newlist (append newlist (list item)))))
       ;;end loop, listp
       ))
     (t (setf newlist (append newlist (list new-items)))))

    newlist
    ;;end let, append-1nested-lists
    ))
;;TEST
;; (append-1nested-lists '(orig list (1 2 3))   '((A)) '((B C)) '(D) 'E)
;; works= (ORIG LIST (1 2 3) A B C D E)
;; TO APPEND NESTED ITEMS IN ORIGINAL LIST USE NIL
;; (append-1nested-lists NIL  '(orig list (1 2 3))   '((A)) '((B C)) '(D) 'E)
;; works= (ORIG LIST 1 2 3 A B C D E)
;; (append-1nested-lists '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO))))
;; works= (B C B C NEW LIST D YES A (THIS) B C (WHAT) X Y (THAT) A (ANOTHER) B C X (TOO))
;; ;; (append-1nested-lists NIL '((A B C)(D E F)(G H I J)))
;; works= (A B C D E F G H I J)
;; (append-1nested-lists  '(A B C)  '((D E F)(G H I J)))
;; works= (A B C D E F G H I J)
;;
;; (append-1nested-lists '(this (list 1)(list 2))) = (THIS (LIST 1) (LIST 2))
;; (append-1nested-lists NIL '(this (list 1)(list 2))) = (THIS LIST 1 LIST 2)
;; (append-1nested-lists NIL '((this (list 1)) ((list 2) (list 3))))
;; (append-1nested-lists NIL  '(((a 1)(a 2))((b 11)(c 22)))) = ((A 1) (A 2) (B 11) (C 22))




;;APPEND-MULTI-LISTS
;;2017
;;ddd
(defun append-multi-lists (list-of-lists1 append-lists &key  list-2-lists-p append-list-p
                                          (return-lists-as-values-p T) not-append-nonlists-p)
  "In U-lists, USE INSTEAD OF SEVERAL APPENDS!   Appends EACH list in list-of-lists1 with items from corresponding SINGLE LIST in append-lists. Only 1 list is appended to 1 list. RETURNS: (values appended-list-of-lists extra-items total-n-lists)  UNLESS RETURN-LISTS-AS-VALUES-P, RETURNS  each each list in list-of-lists1 as a value plus total-n-lists. 
   INPUTING SYMBOLS: If list1 is a list of symbols, use (list sym1... symn). Use mvsetq (sym1 ... symn) to the values output (return-lists-as-values-p= T)."
  (let
      ((appended-list-of-lists)
       (extra-items)
       (len-lol 0)
       (total-n-lists 0)
       )
    (cond
    ;;if list-of-lists = nil
    ((null list-of-lists1)
      (setf appended-list-of-lists append-lists))
    ;;list-of-lists not nil
    (t
    (setf len-lol (list-length list-of-lists1))

    (loop
     for list in list-of-lists1
     for n from 0 to len-lol
     do
     (let
         ((newlist)
          (append-list (nth n append-lists))
          )       
       (cond
        (append-list
         (cond
          (list-2-lists-p
           (setf newlist (list list append-list)))
          ((and not-append-nonlists-p (null (listp list))) 
           (setf extra-items (append extra-items (list list))))
          ((or append-list-p (null (listp list)))
           (setf newlist (append list (list append-list))))
          (t (setf newlist (append list  append-list))))
         )
        (t (setf newlist list)))

       ;;append appended-list-of-lists
       (setf appended-list-of-lists (append appended-list-of-lists (list newlist)))      
       ;;end let, loop, t  ,cond
       ))))
    (setf total-n-lists (list-length appended-list-of-lists))
    ;;RETURN
    (cond
     (return-lists-as-values-p
      (values-list  (append appended-list-of-lists extra-items (list total-n-lists))))
     (t (values appended-list-of-lists extra-items total-n-lists)))
    ;;end let, append-multi-lists
    ))
;;TEST
;; append-list-p
;;  (append-multi-lists '((LIST1 1 1 1)(LIST2 2 2 2 2)(LIST3 3 3)) '((LIST11 11)(LIST22 22 22)(LIST33 33)(LIST44 4 44)) :append-lists-p T)
;; works= (LIST1 1 1 1 (LIST11 11))   (LIST2 2 2 2 2 (LIST22 22 22))  (LIST3 3 3 (LIST33 33))  3
;; list-2-lists-p
;;  (append-multi-lists '((LIST1 1 1 1)(LIST2 2 2 2 2)(LIST3 3 3)) '((LIST11 11)(LIST22 22 22)(LIST33 33)(LIST44 4 44)) :list-2-lists-p T)
;; works= ((LIST1 1 1 1) (LIST11 11))   ((LIST2 2 2 2 2) (LIST22 22 22))  ((LIST3 3 3) (LIST33 33))  3
;; IF also return-lists-as-values-p =NIL
;;  (append-multi-lists '((LIST1 1 1 1)(LIST2 2 2 2 2)(LIST3 3 3)) '((LIST11 11)(LIST22 22 22)(LIST33 33)(LIST44 4 44)) :list-2-lists-p T :return-lists-as-values-p nil) 
;; works=  (((LIST1 1 1 1) (LIST11 11)) ((LIST2 2 2 2 2) (LIST22 22 22)) ((LIST3 3 3) (LIST33 33)))   NIL  3
;; IF JUST APPEND W/O LIST
;; (append-multi-lists '((LIST1 1 1 1)(LIST2 2 2 2 2)(LIST3 3 3)) '((LIST11 11)(LIST22 22 22)(LIST33 33)(LIST44 4 44)) )
;; works= (LIST1 1 1 1 LIST11 11)  (LIST2 2 2 2 2 LIST22 22 22)  (LIST3 3 3 LIST33 33)  3

;;COMPLEX CASE
#| (append-multi-lists NIL  '(
(((B 0.2) (E 0.3) (A 0.4) (D 0.7) (C 3)) ((M 0.2) (Q 0.22) (P 0.3) (L 0.41) (O 0.7) (N 3)))
((3 0.2 0.3 0.4 0.7) (3 0.2 0.22 0.3 0.41 0.7))
(("3 " "  0.2  " "  0.3  " "  0.4  " "  0.7  ") ("3 " "  0.2  " "  0.22  " "  0.3  " "  0.41  " "  0.7  "))
((0.2 0.3 0.4 0.7 3) (0.2 0.22 0.3 0.41 0.7 3))
(("0.2" "0.3" "0.4" "0.7" "3") ("0.2" "0.22" "0.3" "0.41" "0.7" "3"))
((B E A D C) (M Q P L O N))
) :list-2-lists-p T)
|#
;;works= (((B 0.2) (E 0.3) (A 0.4) (D 0.7) (C 3)) ((M 0.2) (Q 0.22) (P 0.3) (L 0.41) (O 0.7) (N 3)))   ((3 0.2 0.3 0.4 0.7) (3 0.2 0.22 0.3 0.41 0.7))   (("3 " "  0.2  " "  0.3  " "  0.4  " "  0.7  ") ("3 " "  0.2  " "  0.22  " "  0.3  " "  0.41  " "  0.7  "))   ((0.2 0.3 0.4 0.7 3) (0.2 0.22 0.3 0.41 0.7 3))   (("0.2" "0.3" "0.4" "0.7" "3") ("0.2" "0.22" "0.3" "0.41" "0.7" "3"))   ((B E A D C) (M Q P L O N))   6






;;APPEND-LISTS1-WITH-OTHER-LISTS
;;2017
;;ddd
(defun append-lists1-with-other-lists (list-of-lists1 other-lists-of-lists
                                                      &key append-list-p (return-lists-as-values-p T) 
                                                      (if-list1-nil-use-len-other-p T)
                                                      if-list1-nil-use-N)
  "In U-lists. INPUTS: list-of-lists1 and  other-lists-of-lists. Appends each list in list-of-lists1 with list with the same Nth list EACH list-of-lists in other-lists-of-lists  To INPUT SYMS instead of lists, (list local1 local2 local3) and catch appended lists with mvsetq (local1 local2 local3).
   IF SYMS ARE= NIL,  RETURNS  (values all-appended-lists extra-lists n-appended-lists) APPEND-LIST-P appends each list AS A LIST not items ((a b)(1 2)) vs (a b 1 2). DO NOT USE INSTEAD OF SEVERAL APPENDS--use append-multi-lists.
  NESTING OF OTHER-LISTS- nesting must be correct--NEED EXTRA SET OF PARENS to append lists together.
  INPUTING SYMBOLS: If list1 is a list of symbols, use (list sym1... symn). Use mvsetq (sym1 ... symn) to the values output (return-lists-as-values-p= T). If SYMS = NIL, then list1 should be list of NILs.   Can IF-LIST1-NIL-USE-N= N if list1= NIL. "
  (let
      ((all-appended-lists)
       (other-items)
       (n-appended-lists 0)
       (len-list1) 
       ;;not needed? (len-other (list-length other-lists-of-lists))
       (longest-other 0)
       )
    ;;FOR EACH LIST IN LIST-OF-LISTS1
    (when  (null list-of-lists1)
      (cond
       (if-list1-nil-use-len-other-p
        (setf longest-other (nth-value 1 
                                       (sort-lists-by-length other-lists-of-lists :not-return-sorted-newlist-p T)))
        (setf list-of-lists1 (make-list longest-other :initial-element NIL)))
       (if-list1-nil-use-N
        (setf list-of-lists1 (make-list if-list1-nil-use-N :initial-element NIL)))
       (t (setf list-of-lists1 (make-list (list-length (car other-lists-of-lists ) :initial-element NIL))))))    
    (setf len-list1 (list-length list-of-lists1))

    (loop
     for list1 in list-of-lists1
     for n from 0 to len-list1
     do 
     (let
         ((appended-list)
          )
       ;;start appended-list with first list
       (when  (and list1 (listp list1))
         (cond
          (append-list-p
           (setf appended-list (list list1)))
          (t (setf appended-list list1))))

       (cond
        ((listp list1)
         ;;FIND OTHER LIST-OF-LISTS2,... N AND APPEND TO LIST1
         (loop
          for list-of-listn in other-lists-of-lists
          do
          (let
              ((listn (nth n list-of-listn))
               )
            (cond
             ((and listn (listp listn))
              (cond
               (append-list-p
                (setf appended-list (append appended-list (list listn))))
               (t (setf appended-list (append appended-list  listn))))
              )
             ;;listn not a list
             (t (setf other-items (append other-items (list listn)))))
            ;;(break "In append-list1etc listn loop")
            ;;end let, loop
            ))
         ;;APPEND ALL, incf n-appended-lists
#|         (when appended-list
           (setf all-appended-lists (append all-appended-lists (list appended-list)))
           (incf n-appended-lists))|#
         ;;end when listp list1
         )
        ;;LIST1 NOT A LIST
        (t (setf other-items (append other-items (list list1)))))

         ;;APPEND ALL, incf n-appended-lists
         (when appended-list
           (setf all-appended-lists (append all-appended-lists (list appended-list)))
           (incf n-appended-lists))
       ;;end outer let,loop
       ))
   ;; (BREAK "end append-lists1-with-other-lists")
    (cond
     (return-lists-as-values-p
      (values-list (append all-appended-lists (list other-items) (list n-appended-lists))))
     (t (values all-appended-lists other-items n-appended-lists)))
    ;;end let, append-lists1-with-other-lists
    ))
;;TEST
;; (append-lists1-with-other-lists '((list1 1 1)(list2 2 2)(list3 3 3 3)) '(((list11 11)(list22 22 22 22)(list33 33))((list 111 111)(list222 222 222 222)(list333 333)))  :append-list-p T)
;; works= ((LIST1 1 1) (LIST11 11) (LIST 111 111))   ((LIST2 2 2) (LIST22 22 22 22) (LIST222 222 222 222))   ((LIST3 3 3 3) (LIST33 33) (LIST333 333))   NIL  3
;;
;; for :return-lists-as-values-p NIL, returns a big list of lists
;; (append-lists1-with-other-lists '((list1 1 1)(list2 2 2)(list3 3 3 3)) '(((list11 11)(list22 22 22 22)(list33 33))((list 111 111)(list222 222 222 222)(list333 333)))  :append-list-p T :return-lists-as-values-p NIL)
;;If first list= NIL
;; (append-lists1-with-other-lists NIL  '(((list11 11)(list22 22 22 22)(list33 33))((list 111 111)(list222 222 222 222)(list333 333)))  :append-list-p T)
;; works=  ((LIST22 22 22 22) (LIST222 222 222 222)) ((LIST33 33) (LIST333 333))   NIL  3
;; if NIL and first list is shorter
;; (append-lists1-with-other-lists NIL  '(((list11 11)(list22 22 22 22))((list 111 111)(list222 222 222 222)(list333 333)))  :append-list-p T)
;; works= ((LIST11 11) (LIST 111 111))    ((LIST22 22 22 22) (LIST222 222 222 222))   ((LIST333 333))   (NIL)  3


;;for nested lists
;;  (append-lists1-with-other-lists '(((a a)(b b)(c c))((d d)(e e)(f f)(g g))) '(((1 2)(3 4 5)) ((11 22)(33 44 55)(66 77))((88 88)(99 99))))
;;results= ((A A) (B B) (C C) 1 2 11 22 88 88)  ((D D) (E E) (F F) (G G) 3 4 5 33 44 55 99 99)   NIL  2   ;;note, no (66 77)
;; result= ((A A) (B B) (C C) 1 2 11 22)    ((D D) (E E) (F F) (G G) 3 4 5 33 44 55)  NIL  2
;; (append-lists1-with-other-lists '(((a a)(b b)(c c))((d d)(e e)(f f)(g g))) '((((1 1)(2 2))((3 3)(4 4)(5 5))) ((11 22)(33 44 55)(66 77))))  ;;note, no (66 77)
;; results= ((A A) (B B) (C C) (1 1) (2 2) 11 22)   ((D D) (E E) (F F) (G G) (3 3) (4 4) (5 5) 33 44 55)  NIL   2 ;;note, no (66 77)
;; (append-lists1-with-other-lists '(((a a)(b b)(c c))((d d)(e e)(f f)(g g))) '((((1 1)(2 2))((3 3)(4 4)(5 5))) ((11 22)(33 44 55)(66 77))) :append-list-p T)
;; results= (((A A) (B B) (C C)) ((1 1) (2 2)) (11 22))    (((D D) (E E) (F F) (G G)) ((3 3) (4 4) (5 5)) (33 44 55))   NIL   2 ;;note, no (66 77)
;; (append-lists1-with-other-lists '((a b c)(d e)(f g h i)) '(((1 2)(3 4 5)) ((11 22)(33 44 55)(66 77))))
;;  works= (A B C 1 2 11 22)  (D E 3 4 5 33 44 55)   (F G H I 66 77)   (NIL)  3
;; :append-list-p
;; (append-lists1-with-other-lists '((a b c)(d e)(f g h i)) '(((1 2)(3 4 5)) ((11 22)(33 44 55)(66 77)))  :append-list-p T)
;;works=  ((A B C) (1 2) (11 22))  ((D E) (3 4 5) (33 44 55))  ((F G H I) (66 77))  (NIL)  3
;; RETURN LIST INSTEAD OF VALUES
;; (append-lists1-with-other-lists '((a b c)(d e)(f g h i)) '(((1 2)(3 4 5)) ((11 22)(33 44 55)(66 77))) :RETURN-LISTS-AS-VALUES-P NIL)
;; works= ((A B C 1 2 11 22) (D E 3 4 5 33 44 55) (F G H I 66 77))   (NIL)   3
;;
;; WHEN LIST1 IS LIST OF NILs (representing evaled symbols in the list)
;;  (append-lists1-with-other-lists NIL '(((1 2)(3 4 5)) ((11 22)(33 44 55)(66 77))))
;;works= (1 2 11 22)  (3 4 5 33 44 55)  NIL  2
;; (append-lists1-with-other-lists '(nil nil nil) '(((1 2)(3 4 5)) ((11 22)(33 44 55)(66 77))))
;; works = (1 2 11 22)   (3 4 5 33 44 55)   (66 77)  (NIL)  3
;; When LIST1=NIL,
;;  (append-lists1-with-other-lists NIL '(((1 2)(3 4 5)) ((11 22)(33 44 55)(66 77))) :if-list1-nil-use-N 3)
;; works= (1 2 11 22)  (3 4 5 33 44 55)   (66 77)  (NIL)  3
;; (append-lists1-with-other-lists NIL'(((list1 1 2 3 4 5)) ((list2 11 22 33 44 55))))
;; works= 
;;
;; (append-lists1-with-other-lists NIL'(((1 2)(3 4 5)) (((11 22))((33 44 55))((66 77))((88 88)(99 99)))) :append-list-p T)



;;(append-lists1-with-other-lists '((a b c)(d e) 'x1 (f g h i)) '(((1 2) 'x2 (3 4 5)) ((11 22)(33 44 55)(66 77) 'x3))  :append-list-p T)
;; note: appends the quoted items (lists): (((A B C) (1 2) (11 22)) ((D E) (QUOTE X2) (33 44 55)) ((QUOTE X1) (3 4 5) (66 77)) ((F G H I) (QUOTE X3)))    (NIL)   4
;;(append-lists1-with-other-lists '((a b c)(d e) 111 (f g h i)) '(((1 2) 222 (3 4 5)) ((11 22)(33 44 55)(66 77) 333))  :append-list-p T)
;; finds non-quoted non-list and NIL or missing items and lists as other-items= ((A B C) (1 2) (11 22))   ((D E) (33 44 55))  ((F G H I))   (222 111 NIL 333)  3
;;
;; (append-lists1-with-other-lists '((1 2) (3 4)) '(((11 22) (33 44)) ((111 222) (333 444)))  :APPEND-LIST-P T)
;; WORKS= ((1 2) (11 22) (111 222))   ((3 4) (33 44) (333 444))   NIL  2
;; (append-lists1-with-other-lists '((1 2) (3 4)) '(((11 22) (33 44)(111 222) (333 444)))  :APPEND-LIST-P T)
;; works= ((1 2) (11 22))   ((3 4) (33 44))  NIL  2 ;;note: 111 222) (333 444) are unmatched lists



;;APPEND-LISTS2 -- WORKS, BUT PROBLEM SEE BELOW
;;
;;ddd
#|(defun append-lists2 (listname new-items &optional default-list)
  "In U-lists, appends list= (evaled listname--should be quoted) with lists. If listname is BOUND and its evaled list is not nil, then it first appends default-list to the evaled list. If  listname is UNBOUNDP, then it FIRST sets listname to default-list, then appends new-lists to default-list. NOTE: If  new-lists is not a list, appends the new item to the original list. If new-istems is a list, then it APPENDS EACH ITEM separately to the list (not the list of items). eg. result is  (orig-list A B C D) not  (orig-list (A) (B)(C)(D)) for new-lists '((A)(B)(C)(D)). NOTE: If  an item in new-items is not a list still appends it--removes parens if they exist, not attempt if they don't. PROBLEM: It makes listname a GLOBAL variable even if local in context."
  (let
      ((listname-vals)
       )
    ;;if listname unboundp, set to default-list, otherwise if default-list append it
    (cond
     ((not (and (symbolp listname)(boundp listname)))
      (set listname default-list))
     (default-list
      (set listname (append (eval listname) default-list)))
     (t nil))
    
    ;;set 
    (setf  listname-vals (eval listname))
    ;;(afout 'out (format nil "listname= ~A listname-vals= ~A~%"listname listname-vals))
    ;;what kind of objects are in new-items?  append appropriately
    (cond
     ((listp new-items)
      (loop
       for item in new-items
       do
       (cond
        ((listp item)
         (setf listname-vals (append listname-vals item)))
        (t (setf listname-vals (append listname-vals (list item)))))
       ;;end loop, listp
       ))
     (t (setf listname-vals (append listname-vals (list new-items)))))

    (set listname listname-vals)
    ;;end let, append-lists
    ))|#
;;TEST
;; when originally unboundp
;;  (append-lists 'testlistx2 'this '(default-list))  = (DEFAULT-LIST THIS)
;;  append a symbol
;;  (append-lists 'testlistx2 'that) = (DEFAULT-LIST THIS THAT)
;;  also  testlistx2 = (DEFAULT-LIST THIS THAT)
;;  append a single list
;;  (append-lists 'testlistx2 '(list1)) = (DEFAULT-LIST THIS THAT LIST1)
;;  also teslistx2 = (DEFAULT-LIST THIS THAT LIST1)
;; append mixed multiple-lists and non-lists
;;  (append-lists 'testlistx2 '((list2)(list3) non-list1 (list4)))
;;  works= (DEFAULT-LIST THIS THAT LIST1 LIST2 LIST3 NON-LIST1 LIST4)
;;  also: (DEFAULT-LIST THIS THAT LIST1 LIST2 LIST3 NON-LIST1 LIST4)
;;
;;FOR USE IN 
;;  (setf new-matched-group1 nil)
;;  (append-lists 'new-matched-group1 (list '(b c)  (append '((WHAT)) '((NEW LIST)))))
;;  results= (B C (WHAT) (NEW LIST))
;;  (append-lists 'new-matched-group1  (list '(b c) (append  '((WHAT)) '((NEW LIST)))))
;;  (progn (setf out nil testx nil new-matched-group1 nil)(append-lists 'new-matched-group1 (append-lists 'testx '((NEW LIST)) '((WHAT)) )  '(b c))) 
;;  works= (B C WHAT NEW LIST)
;;  also new-matched-group1 = (B C WHAT NEW LIST)


;;MAKE-VALUES-LIST
;;2018
;;ddd
(defmacro make-values-list (values-funcall &key (nvars 8) (trim-nils-p T))
  "U-lists   RETURNS a list of the values   INPUT: NVARS= num of vars want to put in list--up to 52 values or other items "
  (let*
      ((allvars '(a b c d e f g h i j k l m n o p q r s t1 u v w x y z a2 b2 c2 d2 e2 
                    f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2))
       (vars (subseq allvars 0 nvars))    
       (letlist) ;; `((return-list)))
       (return-list  (append (list 'list) `,vars))
       )  
    (loop
     for v in vars
     do
     (setf letlist (append letlist (list (list v)))))
    (setf *macout
    `(progn
       (let
           ,letlist            
         (multiple-value-setq ,vars
             ,values-funcall)
       ,return-list       
    (when ,trim-nils-p
      (delete-conseq-repeats NIL ,return-list :from-end T))
    ))
    )    
    ;;end  make-values-list
    ))
;;(make-values-list (values 1 2 3))
;;works =(1 2 3)   (NIL NIL NIL NIL NIL)
;; (setf ***testout (make-string-input-stream (make-values-list (DESCRIBE-OBJECT *SWORLDVIEW-INST t) :nvars 25)))
#| (with-open-stream (stream :direction :output)
  (let*
      ((outstring (make-string-input-stream (make-values-list (DESCRIBE-OBJECT *SWORLDVIEW-INST stream ) :nvars 25)))
       )
    (setf ***teststr outstring)
    ))
(DOS-INPUT-STREAM (MAKE-STRING-INPUT-STREAM DOS-OUTPUT-STR))
|#



#| SIMPLE VERSION RETURNS EXTRA NIL'S
(defun make-values-list (&rest valueslist)
  "U-lists   RETURNS a list of the values   INPUT: values or other items  "
   valueslist
    ;;end  make-values-list
    )|#
;;SSSS DO LATER??
#|(defmacro make-values-list (values-funcall &key (nvars 8))
  ;;(setf *macout
   `(progn
      (multiple-value-bind (a b c d e f)
      ,values-funcall
       (list a b c d e f))
      )
   )|#
;; (make-values-list (values 1 2 'x)) 
;; works=  (1 2 X NIL NIL NIL)

;; (delete-conseq-repeats  'x   '(a b c x x x x) :from-end t)
;; works = (A B C)   (X X X X)
;; ;; (delete-conseq-repeats  'nil   '(a b c nil nil nil) :from-end t)
;; works = (A B C)   (NIL NIL NIL)
;; (let ((x 1)(y 2))  (make-values-list (values x y)))
;; works= (1 2)  (NIL NIL NIL NIL NIL NIL)




;;APPEND-NTH
;;2016
;;ddd
#| WORKS -- BUT USES REVERSE TWICE--seems inefficient
 (defun append-nth (newitem nth  list &key from-end-p splice-list-p)
  "In U-lists, places (not appends) newitem to the nth place in a list, moves later items down one. If SPLICE-LIST-P, splices newitem, if list, into list at nth.If not list, places item there--not an error."   ;;0 1 2 3 4 ;; nth= 0   from-end nth (- 5 0 1) = 4
  (let
      ((newlist)
       (list-n (list-length list))
       (append-item)
       (testlist list)
       )
    (when from-end-p
      (setf testlist (reverse list)))

     (cond
       ((and (listp newitem)
             splice-list-p)
        (setf append-item newitem))
       (t (setf append-item (list newitem))))
  
    (loop
     for item in testlist
     for n from 0 to list-n
     do
     (cond
      ((= n nth)
       (cond
        (from-end-p
         (setf newlist (append newlist append-item (nthcdr n testlist) )))
        (t (setf newlist (append newlist append-item (nthcdr n testlist)))))
        ;;(afout 'out (format nil "n= ~A item= ~A newlist= ~A" n item newlist))
        (return))
      (t (setf newlist (append newlist (list item )))))
     ;;(afout 'out (format nil "n= ~A item= ~A newlist= ~A" n item newlist))
     ;;end loop
     )
    (when from-end-p
      (setf newlist (reverse newlist)))
    newlist
    ;;end let, append-nth
    ))|#

;;APPEND-NTH
;;2016
;;ddd
(defun append-nth (newitem nth  list &key from-end-p splice-list-p
                           (if-out-of-range-append-p T))
  "In U-lists, places (not appends) newitem to the nth place in a list, moves later items down one. If SPLICE-LIST-P, splices newitem, if list, into list at nth.If not list, places item there--not an error. Works if nth is out of range, puts at begin or end of list if include-out-of-range-p."
   ;;0 1 2 3 4 ;; nth= 0   from-end nth (- 5 0 1) = 4
  (let
      ((newlist)
       (list-n (list-length list))
       (append-item)
       (testlist  list)
       )
    (cond
     ((and (listp newitem)
           splice-list-p)
      (setf append-item newitem))
     (t (setf append-item (list newitem))))

    (when from-end-p
      (setf nth (- list-n nth 1 )))

    ;;IF NTH IS OUT OF RANGE, 
    (cond
     ((and if-out-of-range-append-p
           (>= nth list-n))
      (setf newlist (append list append-item)))
     ((and if-out-of-range-append-p
           (< nth 0))
      (setf newlist (append  append-item list)))
     ((or (>= nth list-n) (< nth 0))
      (setf newlist list))
     (t  
      (loop
       for item in testlist
       for n from 0 to list-n
       do
       (cond
        ((= n nth)
         (cond
          (from-end-p
           (setf newlist (append newlist (list (nth (+ n) testlist)) append-item (nthcdr (+ n 1) testlist  ))))
          (t (setf newlist (append newlist append-item (nthcdr n testlist)))))
         ;;(afout 'out (format nil "1 n= ~A nth= ~A item= ~A newlist= ~A" n nth item newlist))
         (return))
        (t (setf newlist (append newlist (list item )))))
       ;;(afout 'out (format nil "2 n= ~A nth= ~A item= ~A newlist= ~A" n nth item newlist))
       ;;end loop, when in range
       )))
    newlist
    ;;end let, append-nth
    ))
;;TEST
;; (append-nth 'this 0 '(0 1 2 3 4 5 6 7 8 9))) ;; THIS IN 0th place
;; works= (THIS 0 1 2 3 4 5 6 7 8 9)
;; (progn (setf out nil)(append-nth 'this 5 '(0 1 2 3 4 5 6 7 8 9)))
;; works=  (0 1 2 3 4 THIS 5 6 7 8 9) ;;5 in 5th place
;; (progn (setf out nil)(append-nth 'this 9 '(0 1 2 3 4 5 6 7 8 9)))
;; works= (0 1 2 3 4 5 6 7 8 THIS 9) NOTE: THIS is put in 9th place (list now 11 long)
;; FROM-END-P
;;  (append-nth 'this 0 '(0 1 2 3 4 5 6 7 8 9) :from-end-p T))
;; (0 1 2 3 4 5 6 7 8 9 THIS) ; 0th place from end
;; (progn (setf out nil)(append-nth 'this 9 '(0 1 2 3 4 5 6 7 8 9) :from-end-p T))
;;  works=  (0 THIS 1 2 3 4 5 6 7 8 9);;  9th place from end (list now 11 long).
;; SPLICE-LIST-P
;; (progn (setf out nil)(append-nth 'this 5 '(0 1 2 3 4 5 6 7 8 9) :splice-list-p T))
;;  works= (0 1 2 3 4 THIS 5 6 7 8 9) ;;note: appended a NON-LIST NEWITEM OK
;; (progn (setf out nil)(append-nth '(this) 5 '(0 1 2 3 4 5 6 7 8 9) :splice-list-p T))
;; works= (0 1 2 3 4 THIS 5 6 7 8 9)  ;;note same result as if non-list item
;; (progn (setf out nil)(append-nth '(this) 9 '(0 1 2 3 4 5 6 7 8 9)))
;;  (0 1 2 3 4 5 6 7 8 (THIS) 9)  IF NOT SPLICE-LIST-P
;; IF OUT OF RANGE (:if-out-of-range-append-p T)
;; (append-nth '(this) 11 '(0 1 2 3 4 5 6 7 8 9)) = (0 1 2 3 4 5 6 7 8 9 (THIS))
;;  (append-nth '(this) -2 '(0 1 2 3 4 5 6 7 8 9)) = ((THIS) 0 1 2 3 4 5 6 7 8 9)
;;  If  :if-out-of-range-append-p = NIL
;;  (append-nth '(this) 11 '(0 1 2 3 4 5 6 7 8 9) :if-out-of-range-append-p NIL) =
;; = (0 1 2 3 4 5 6 7 8 9)
;; (append-nth '(this) -1 '(0 1 2 3 4 5 6 7 8 9) :if-out-of-range-append-p NIL)
;;  = (0 1 2 3 4 5 6 7 8 9)

;;  (append-nth 'this 2 '(0 1 2 3 4 5 6 7 8 9)) = (0 1 THIS 2 3 4 5 6 7 8 9)
;;  (append-nth 'this 2 '(0 1 2 3 4 5 6 7 8 9) :from-end-p T)
;;   works = (0 1 2 3 4 5 6 7 THIS 8 9)
;;
;; (append-nth '(a b c) 4 '(0 1 2 3 4 5 6 7 8 9) :splice-list-p T)
;; works=  (0 1 2 3 4 A B C 5 6 7 8 9)

;; (progn (setf out nil)(append-nth 'this 0 '(0 1 2 3 4 5 6 7 8 9) :from-end-p T))
  






;;APPEND-LIST -- original
;;
;;ddd
(defun append-list (listname item  list)
  "In U-lists, sets  symbol listname. Eg (append-list 'xx-list 'this)  =  (ITEM THIS) and (append-list 'xx-list 'that '(test)) = (TEST THAT) "
  (cond
   ((null list)
      (set listname (append (eval listname) (list item))))
   (t (set listname (append list (list item)))))
  ;;end append-list
      )
;;TEST
;;  (setf  xx-list nil) (setf xx-list (append xx-list (list 'item)))
;;  (append-list 'xx-list 'this xx-list)  =  (ITEM THIS)
;;  also: xx-list = (ITEM THIS)
;;   (append-list 'xx-list 'that '(test)) = (TEST THAT)
;;  xx-list = (TEST THAT)


;;APPEND-KEYVALS-IF-NONIL
;;2019
;;ddd
(defun append-keyvals-if-nonil (keyvals-list list &key eval-value-p)
  "U-lists   RETURNS (values new-keylist appended-items INPUT: keyvals-list= lists of key-value pairs. (eg. ((:k1 val1)etc) When eval-value-p, evals the val variable. "
  (let
      ((new-keylist list)
       (appended-items)
       )
   (loop
     for keyval in keyvals-list
     do
     (let*
         ((key (car keyval))
          (val (second keyval))
          )
       (when (and eval-value-p (symbolp val)(boundp val))
         (setf val (eval val)))
       (when val
         (setf appended-items (append appended-items (list key val))))     
      ;; (break "loop")
     ;;end let,loop
     ))
   (when appended-items
     (setf new-keylist (append new-keylist appended-items)))
    (values  new-keylist appended-items)
    ;;end let, append-keyvals-if-nonil
    ))
;;TEST
;; (append-keyvals-if-nonil '((:k2 nil) (:k3 77)) '(a b c))
;; works = (A B C :K3 77)   (:K3 77)



;;APPEND-NO-EVAL
;;2018
;;ddd
(defmacro append-no-eval  (&rest list)
  "U-lists Doesn't eval list items before append"
  (let
      ((newlist)
       )
    (loop
     for item in list
     do
     (let*
         ((newitem)
          )
       ;;(break "item")
       (cond
        ((listp item)
         (loop
          for subitem in item
          do
            (setf newitem (eval `(append (quote ,newitem)  (list (quote ,subitem)))))
          ;;(break "eval")
          ;;end loop
          )
         (setf newlist `(append ,newlist  (quote ,newitem))))
        (t (setf newlist `(append ,newlist  (quote ,item)))))
       ;was (setf newlist `(append ,newlist  (list ,item)))
       ;;end let, outer loop
       ))
    newlist
    ;;end let, append-no-eval
    ))
;;TEST
;; (append-no-eval (this a b c) (that x y))
;;works=  (THIS A B C THAT X Y)
 ;; (append-no-eval (this a b c) (that x y) (list yy zz))
;; result= (THIS A B C THAT X Y LIST YY ZZ) ;;note LIST not evaluated either
 ;; (append-no-eval (this a b c) (that x y) xx)
;; works= (THIS A B C THAT X Y . XX)

 
;;FIND-ITEM-OR-SUBITEM-IN-LIST
;;
;;ddd
(defun find-item-or-subitem-in-list (item list &key (match-subitems-p T))
  "In U-lists.lisp, searches list. Finds item (or first-order subitem in item (if listp item) and RETURNS item and rest of list (like member does) if found. NOTE: Doesn't test inner parts of lists in LIST."
  (let
      ((result)
       )
    (cond
     ((setf result (my-member item list)))
     ((and (listp item) match-subitems-p)
      (loop
       for subitem in item
       do
       ;;(setf out1 (format nil "subitem= ~A" subitem))
       (cond       
        ((setf result (my-member subitem list))
         (return))              
        (t nil))
       ;;end loop, clause
       ))
     (t nil))
    result
    ;;end find-item-or-subitem-in-list
    ))
;;TEST
;;  (find-item-or-subitem-in-list '(b) '(a b c))
 ;;doesn't test inner parts of lists in list
;; (find-item-or-subitem-in-list '(m y) '(a b c 3 "this" 7 (x y)))  = NIL
;; (find-item-or-subitem-in-list '(m "this") '(a b c 3 "this" 7 (x y))) = ("this" 7 (X Y))



;;MY-EQUAL-LISTS
;;
;;ddd
(defun my-equal-lists (list1 list2 &key exclusion-items (test 'my-equal))
  "In U-lists.  Tests 2 items in 2 lists using my-equal in order. Omits exclusion-items on either list. List items MUST BE IN SAME ORDER--including exclusion items. 
  RETURNS (values result matched-items unmatched-items unmatched-ns excluded-items)"
  (let
      ((matched-items)
       (unmatched-items)
       (excluded-items)
       (unmatched-ns)
       (result T)
       )
    (loop
     for item1 in list1
     for item2 in list2
     for n from 1 to (length list1)
     do
     (cond
      ((my-member item1 exclusion-items)
       (setf excluded-items (append excluded-items (list item1 item2))))
      ((funcall test item1 item2) ;;was(my-equal item1 item2)
       (setf matched-items (append matched-items (list (list item1 item2)))))
      (t (setf unmatched-items (append unmatched-items (list (list item1 item2)))
               unmatched-ns (append unmatched-ns (list n))
               result nil)))
     )
    (values result matched-items unmatched-items unmatched-ns excluded-items)
    ;;end let, my-equal-lists
    ))
;;TEST
;;  (my-equal-lists '(L 2 3 TO 2 3 3) '(l 2 3 "to"  2 3 3) :exclusion-items '(TO))
;; works= T  ((L L) (2 2) (3 3) (2 2) (3 3) (3 3))   NIL   NIL  (TO "to")
;;  (my-equal-lists '(L 2 3 TO 1 3 3) '(l 2 3 "to"  2 3 3) :exclusion-items '(TO))
;; works= NIL  ((L L) (2 2) (3 3) (3 3) (3 3))  ((1 2))  (5)  (TO "to")
;;
;; (my-equal-lists '(a b x c 7 e) '(a b x c 7 e))
;; works = T    ((A A) (B B) (X X) (C C) (7 7) (E E))   NIL  NIL  NIL
;;  (my-equal-lists '("a" "b") '(a b)) = T    (("a" A) ("b" B))   NIL  NIL NIL


;;MY-EQUAL-ITEM-LIST-OR-MEMBER
;;2019
;;ddd
(defun my-equal-item-list-or-member (item item-or-list  &key (test 'my-equal) )
  "U-lists. Is item equal or a member of item-or-list  RETURNS  (values return-equal compare-type)  compare-type= item-item, item-list, list-list, or NIL if can't compare.   INPUT:  Only ITEM-OR-LIST is searhed for list MEMBERSHIP. Otherwise, either can be a symbol, string, number, or list. "
  (let
      ((return-equal)
       (compare-type)
       )
    (cond
     ;;neither is a list
     ((and (null (listp item))
           (null (listp item-or-list)))
      (setf return-equal (funcall test item item-or-list)
            compare-type 'item-item))
     ;;item not-list, item-or-list=list
     ((and (null (listp item))
           (listp item-or-list))
      (setf return-equal (car (member item item-or-list :test test))
            compare-type 'item-list))
     ;;both are lists
     ((and (listp item) (listp item-or-list))
      (setf return-equal (my-equal-lists item item-or-list :test test)
            compare-type 'list-list))
     ;;redundant below ((and (listp item)(null (listp item-or-list))) NIL)      
     (t nil))
     (values return-equal compare-type)
    ;;end let, my-equal-item-list-or-member
    ))
;;TEST
;; (my-equal-item-list-or-member 'a  '(d a g))    = A  ITEM-LIST
;; (my-equal-item-list-or-member '(d a g)  '(d a g)) =   T  LIST-LIST
;; (my-equal-item-list-or-member '(d a g)  '(d 1 g)) = NIL   LIST-LIST
;; (my-equal-item-list-or-member '(d a g)  '("d" "A" "G")) = T  LIST-LIST
;; (my-equal-item-list-or-member "S" "s")   =   T   ITEM-ITEM
;; (my-equal-item-list-or-member 'S "s") = T  ITEM-ITEM



;;MY-EQUAL-NESTED-LISTS
;;2019
;;ddd
(defun my-equal-nested-lists (list1 list2  &key (test 'my-equal))
  " U-lists  RETURNS (values result unmatched-items tested-list1 tested-list2 )   INPUT: Any degree of nested lists can be tested eg. for inequality as well. Uses test-nested-lists."
  (multiple-value-bind (result unmatched-items tested-list1 tested-list2 )
      ( test-nested-lists list1 list2  :test test)
    (values result unmatched-items tested-list1 tested-list2 )
    ;;end mvb, my-equal-nested-lists
    ))
;;TEST
;; SEE BELOW TEST-NESTED-LISTS




;;TEST-NESTED-LISTS
;;2019
;;ddd
(defun test-nested-lists (list1 list2  &key (test 'my-equal) )
  " U-lists  RETURNS (values result unmatched-items tested-list1 tested-list2 )   INPUT: Any degree of nested lists can be tested eg. for inequality as well."
  (let
      ((tested-list1)
       (tested-list2)
       (result)
       (unmatched-items)
       )
    (loop
     for item1 in list1
     for item2 in list2
     do
        ;;(afout 'out (format nil "BEGIN item1= ~A item2= ~A" item1 item2))
     (let*
         ((cur-result)
          )
       (cond
        ((and (listp item1)(listp item2))
         (cond
          ((= (list-length item1)(list-length item2))
           (multiple-value-bind (recurse-result unmatched-items1)
               (test-nested-lists item1 item2 :test test )
             ;;(afout 'out (format nil "RECURSE item1= ~A item2= ~A recurse-result= ~A" item1 item2 recurse-result))
             (setf cur-result recurse-result
                   unmatched-items unmatched-items1)))
          (t (setf cur-result NIL
                   unmatched-items (list item1 item2))))
         )
        (t (setf cur-result (funcall test item1 item2))
           (when (null cur-result (setf unmatched-items (list item1 item2))))
   ;;(afout 'out (format nil "NON-RECURSE item1= ~A item2= ~A CUR-result= ~A" item1 item2 cur-result))
           ))
       ;;summary
       (setf tested-list1 (append tested-list1 (list item1))
             tested-list2 (append tested-list2 (list item2)))
          ;;(afout 'out (format nil "item1= ~A item2= ~A" item1 item2))
       (cond
        (cur-result
         (setf result T))
        (t
         (setf result nil)
         (return)))
     ;;end let,loop
     ))
    (values result unmatched-items tested-list1 tested-list2 )
    ;;end let, test-nested-lists
    ))
;;TEST
;; (test-nested-lists '(a (1 2 (3 4 (6 6 7) 8) 9 10) c (x y (z) w)) '(a (1 2 (3 4 (6 6 7) 8) 9 10) c (x y (z) w)))
;; works= T    NIL   (A (1 2 (3 4 (6 6 7) 8) 9 10) C (X Y (Z) W))    (A (1 2 (3 4 (6 6 7) 8) 9 10) C (X Y (Z) W))
;; ;; (test-nested-lists '(a (1 2 (3 4 (6 6 7) 8) 9 10) c (x y (z) w)) '(a (1 2 (3 4 (6 6 7) 8) 9 10) c (x y (z NO-MATCH) w)))
;; works= NIL   ((Z) (Z NO-MATCH))     (A (1 2 (3 4 (6 6 7) 8) 9 10) C (X Y (Z) W))  (A (1 2 (3 4 (6 6 7) 8) 9 10) C (X Y (Z NO-MATCH) W))
;; ;; (test-nested-lists '(a (1 2 (3 4 (6 999 7) 8) 9 10) c (x y (z) w)) '(a (1 2 (3 4 (6 6 7) 8) 9 10) c (x y (z) w)))
;; works= NIL  (999 6)    (A (1 2 (3 4 (6 999 7) 8) 9 10))   (A (1 2 (3 4 (6 6 7) 8) 9 10))












;;MY-MEMBER
;;
;;ddd
(defun my-member (item list &key (ignore-case-p T))
   "In U-lists.lisp, REPLACES MEMBER,  tests item whether it is a string, number, symbols, or list to see if it is a member of list. Good for UNKNOWN items. Matches strings with symbols if same (eg \"TO\" = TO)."
   (let
       ((result)
        (str1)
        (str2)
        (test1)
        )
     (cond
      ((and (listp item)(setf test1 (member item list :test 'equal)))
         (setf result test1)
         )
      ((listp item)
       (loop
        for item1 in item
        do
        (setf result 
              (my-member item1 list))
        (when result
          (return)))
       ;;end listp
       )
      ;;item not a list, use most liberal matching via format
      (t
       (loop
        for element in list
        for n from 0 to (- (list-length list) 1)
        do
       (setf str1 (format nil "~A" item)
             str2 (format nil "~A" element))
       (when (string-equal str1 str2)
         (setf result  (nthcdr n list))
         (return))
       ;; end 2nd loop, t, cond
       )))
      result
      ;;end my-member
      ))
;;TEST works
;; (my-member 7  '(a b c 3 "this" 7 (a b c))) = (7 (A B C))
;; (my-member '(x y) '(a b c 3 "this" 7 (x y))) = ((X Y))
;;  (my-member "this"  '(a b c 3 "this" 7 (x y)))
;; (my-member "to"  '(a b c 3 "this" TO 7 (x y))) = (TO 7 (X Y))




;;MY-MEMBER-LIST
;;
;;ddd
(defun my-member-list (list1 list2)
  "In U-lists.lisp, For each item of list1tests item whether it is a string, number, symbols, or list to see if it is a member of list2. Good for UNKNOWN items. Matches strings with symbols if same (eg \"TO\" = TO). RETURNS (members non-members) of list2. If list1 not a list, checks to see if the item is a member of list2."
  (let
      ((members)
       (non-members)
       )
    (cond
     ((listp list1)
      
      (loop
       for item in list1
       do
       (cond
        ((my-member item list2)
         (setf members (append members (list item))))
        (t (setf non-members (append non-members (list item)))))
       ;;end loop, listp
       ))
     (t 
      (cond
        ((my-member list1  list2)
         (setf members (append members (list list1 ))))
        (t (setf non-members (append non-members (list list1)))))
      ))
    
    (values members non-members)
    ;;end let, my-member-list
    ))
;;TEST
;;  (my-member-list '(a 7 (this) "the" 77 x (that)) '((this) 7 the "a"))
;;  works= (A 7 (THIS) "the")  (77 X (THAT))
;; (my-member-list  'the  '((this) 7 the "a")) = (THE)  NIL
;; (my-member-list  99  '((this) 7 the "a")) = NIL  (99)












;;MAKE-LIST-TREE-FROM-EVALED-TREE
;;  THIS IS A GOOD RECURSE DEMO 
;;
;;ddd
(defun make-list-tree-from-evaled-tree (topx)
 "In U-lists, INPUT= top node of a tree in which one symbol evals to a list of other symbols, each of which evals to other lists of symbols.  Process stops at and returns final symbol that does not eval to a list. RETURNS a tree of lists, each list is the node with sub-symbols in the second list. EG.  (setf  xx '(x1 x2) x1 '(xx1 xx2) xx1 '(xxx1 xxx2) xx2 '(xxx3 xxx4)) RETURNS ((XX ((X1 ((XX1 (XXX1)) (XX1 (XXX2)))) (X1 ((XX2 (XXX3)) (XX2 (XXX4)))))) (XX (X2)))"
  (let 
      ((sublist)
       (result-list)
       (all-result-lists)
       )
   ;; (incf *cycle-n)
    ;;(afout 'out1 (format nil "*cycle-n= ~A~%topx= ~A~%" *cycle-n topx))
    (cond
     ((boundp topx) (setf  sublist (eval topx))
      (loop
       for subvar in sublist
       do
      ;; (afout 'out1 (format nil "2 subvar= ~A~%" subvar))
       (cond
        ((and (listp sublist)(> (list-length sublist) 0))
         (setf result-list (simple-recurse subvar)
               all-result-lists (append all-result-lists (list (list topx result-list))))      
         )
        (t (setf all-result-lists (append all-result-lists (list subvar)))))
      ;; (afout 'out1 (format nil "*cycle-n= ~A~%result-list= ~A~%all-result-list= ~A~%" *cycle-n result-list all-result-lists))
       ;;end loop
       ))
     (t (setf all-result-lists (append all-result-lists (list topx)))))

    all-result-lists
    ))
;;TEST
;;  (setf  xx '(x1 x2)    x1 '(xx1 xx2)    xx1 '(xxx1 xxx2)   xx2 '(xxx3 xxx4))
;; (progn (setf  *cycle-n 0) (simple-recurse 'xx))
;; works=  ((XX ((X1 ((XX1 (XXX1)) (XX1 (XXX2)))) (X1 ((XX2 (XXX3)) (XX2 (XXX4)))))) (XX (X2)))
           





;;FLATTEN-COUNT-NESTED-LISTS
;;
;;ddd
(defun flatten-count-nested-lists (nested-list)
  "In U-lists, RETURNS (values flat-list length). Any degree of nesting ok."
  (let
      ((element)
       (rest)
       (flat-list)
       (length)
       (new-flat-list)
       )
    (cond
     ((not (listp nested-list))
      (setf flat-list (append flat-list (list nested-list))))
     ((null nested-list) NIL)
     ((listp nested-list)
      (setf element (car nested-list)
            rest (cdr nested-list))
      (multiple-value-setq (new-flat-list)
          (flatten-count-nested-lists element))
      (setf flat-list (append flat-list new-flat-list))
      )
     (t nil))

    (cond
     ((and (listp rest)(> (list-length rest) 0))
      (multiple-value-setq (new-flat-list)
          (flatten-count-nested-lists rest))
      (setf flat-list (append flat-list new-flat-list)))
     (t nil))
    (setf  length (list-length flat-list))

    (values flat-list  length)
    ;;end let, flatten-nested-lists
    ))
;;TEST
;;  (flatten-count-nested-lists '(1 (2 3)((4 5)((6 7)) 8) 9 (10 11))) 
;; works = (1 2 3 4 5 6 7 8 9 10 11)  11
;;  (flatten-count-nested-lists '((0 1) (2 3)((4 5)((6 7)) 8) 9 (10 11)12))  12
;;  works = (0 1 2 3 4 5 6 7 8 9 10 11 12)



;;FLATTEN-1-LEVEL
;;2016
;;ddd
(defun flatten-1-level (lists)
  "In U-lists. "
  (let 
      ((flattened-list)
       )
    (loop
     for item in lists
     do     
     (cond
      ((listp item)
       (setf flattened-list (append flattened-list item)))
      (t (setf flattened-list (append flattened-list (list item)))))
     ;;end loop
     )
    flattened-list
    ;;let,flatten-1-level
    ))
;;TEST
;; (flatten-1-level '((A B) C (((D E F))) (G H)((1 2 3))))
;; works= (A B C ((D E F)) G H (1 2 3))
;; (flatten-1-level '((A B) C (('(D E F))) '(G H)((1 2 3))))
;; WORKS W SOME QUOTES = (A B C ((QUOTE (D E F))) QUOTE (G H) (1 2 3))

;;MY-LIST-LENGTH
;;
;;ddd
(defun my-list-length (list)
  "In U-lists, RETURNS (values length n-lists n-symbols n-strings n-numbers)"
  (let
      ((length 0)
       (n-lists 0)
       (n-symbols 0)
       (n-strings 0)
       (n-numbers 0)
       )
    (loop
     for item in list
     do
     (incf length)
     (if (listp item)(incf n-lists))
     (if (symbolp item)(incf n-symbols))
     (if (stringp item)(incf  n-strings))
     (if (numberp item)(incf n-numbers))
     )
    (values length n-lists n-symbols n-strings n-numbers)
    ))
;;TEST
;;  (my-list-length '(a 7 "this"(a b) 9 (d) "that"))
;; works= 7  2  1  2  2


;;GET-NESTED-LIST-LEVELN-ITEMS
;;2019
;;ddd
(defun get-nested-list-leveln-items (list &key (nest-level-n 1)(cur-level 0)
                (n-higher-level-items 0) non-list-items-only-p)
  "U-lists Counts all items in list that are at NEST-LEVEL-N ONLY.  RETURNS    (values leveln-items n-leveln-items higher-level-items) where HIGHER-LEVEL-ITEMS is a list of items at each preceding level."
  (let*
      ((leveln-items)
       (n-leveln-items 0)
       (higher-level-items)
       )
    (cond
     ;;listp list
     ((and list (listp list))
      (loop
       for item in list
       do
       ;;(afout 'out (format nil "LOOP BEGIN item= ~A cur-level= ~A" item cur-level))
       (cond
        ;;if nest-level-n, then count/add if list or not
        ((= cur-level nest-level-n)
         (unless (and non-list-items-only-p (listp item))
           (incf n-leveln-items)
           (setf leveln-items (append leveln-items (list item)))  ))
        ;;if not right level, and is list recurse
        ((and (listp item)(< cur-level nest-level-n))
         (multiple-value-bind (leveln-items1 n-leveln-items1 higher-level-items1)
             ;;n-higher-level-items1)
             (get-nested-list-leveln-items item :nest-level-n nest-level-n
                                           :cur-level  (+ cur-level 1) 
                                           :non-list-items-only-p  non-list-items-only-p)
           (setf leveln-items (append leveln-items leveln-items1)
                 n-leveln-items (+ n-leveln-items n-leveln-items1)
                 higher-level-items (append higher-level-items 
                                            (list higher-level-items1)))
           ;;n-higher-level-items (+ n-higher-level-items n-higher-level-items1))
           ;;(afout 'out (format nil "MVB,  leveln-items= ~A cur-level=" leveln-items cur-level))
           ;;end mvb, listp
           ))
        ;;if less than right level, and NOT list, add to other
        (t (setf higher-level-items (append higher-level-items (list item)))))
       ;;end let,loop
       )
     ;;end and list listp
     )
    ;;not list, but at level-n, add to leveln-items
    ((= cur-level nest-level-n)
     (incf n-leveln-items)
     (setf leveln-items (append leveln-items (list list))))
    ;;not list, but not at level-n, incf n-higher-level-items 
    (t  (incf n-higher-level-items)))

  (values leveln-items n-leveln-items higher-level-items)
  ;;end let,get-nested-list-leveln-items
  ))
;;TEST
;; (get-nested-list-leveln-items '(a (aa bb) c (cc (ddd (ffff) ddd) ee) g))
;; works= (AA BB CC (DDD (FFFF) DDD) EE)  5   (A NIL C NIL G)
;; (get-nested-list-leveln-items '(a (aa bb) c (cc (ddd (ffff) ddd) ee) g) :nest-level-n 2)
;; works= (DDD (FFFF) DDD)   3  (A (AA BB) C (CC NIL EE) G)
;; (get-nested-list-leveln-items '(a (aa bb) c (cc (ddd (ffff) ddd) ee) g) :nest-level-n 0)
;; works = (A (AA BB) C (CC (DDD (FFFF) DDD) EE) G)   5  NIL
;;for NON-LIST-ITEMS-ONLY-P
;; (get-nested-list-leveln-items '(a (aa bb) c (cc (ddd (ffff) ddd) ee) g) :NON-LIST-ITEMS-ONLY-P T)
;; works= (AA BB CC EE)   4  (A NIL C NIL G)




;;FIND-LONGEST-LISTS
;;2016
;;ddd
(defun find-longest-lists (lists)
  "U-list. Puts lists in order by length--longest first. RETURNS (values ordered-lists  list-ns ordered-info-lists)"
  (let
      ((ordered-lists)
       (ordered-info-lists)
       (new-ordered-info-lists)
       (ordered-list-ns)
       (list-ns)
       (listn)
       (lists-info)
       (list-len)
       (num-lists (list-length lists))
       )
    ;;FIND LENGTHS AND ID BY NTH
    (loop
     for list in lists
     for nth-list from 1 to num-lists
     do
     (when (and list (listp list))
       (setf list-len (list-length list)
             lists-info (append lists-info (list (list :list nth-list :length list-len )))
             list-ns (append list-ns (list  list-len)))
     ;;end when, loop
     ))
    (setf ordered-info-lists (my-sort-lists 3 lists-info)
          ordered-list-ns (my-sort list-ns  #'test-greaterp))
    
    (loop
     for info-list in ordered-info-lists
     for i from 1 to num-lists
     do
     (setf info-list (append  (list :order i) info-list)
           new-ordered-info-lists (append new-ordered-info-lists (list info-list)  )
           listn (fourth info-list)
           ordered-lists (append ordered-lists (list (nth (- listn 1) lists))))
     )    
    (values ordered-lists ordered-list-ns new-ordered-info-lists)
    ;;end let, find-longest-lists
    ))
;;TEST
;; (find-longest-lists '((1 2 3 4)(A B C D E F)(l m n o p q r s t)))
;; works= ((L M N O P Q R S T) (A B C D E F) (1 2 3 4))  (9 6 4)  ((:ORDER 1 :LIST 3 :LENGTH 9) (:ORDER 2 :LIST 2 :LENGTH 6) (:ORDER 3 :LIST 1 :LENGTH 4))









;;FILL-LISTS
;;2016
;;ddd
(defun fill-lists (lists fill-list)
  "In U-lists. Takes items from fill-list to fill in lists that are shorter than the longest list in lists. RETURNS filled-lists of equal length."
  (let
      ((filled-lists)
       (list-ns)
       (ordered-lists)
       (ordered-info-lists)
       (longest-listn)
       (num-lists (list-length lists))
       )
    (multiple-value-setq (ordered-lists list-ns ordered-info-lists)
        (find-longest-lists lists))

    (setf longest-listn (car list-ns))
    (loop
     for list in ordered-lists
     for listn in list-ns
     do
     (loop
      for item in fill-list
      do
      (cond
       ((= listn longest-listn)
        (setf filled-lists (append filled-lists (list list)))
        (return))
       (t (incf listn)
          (setf list (append list (list item)))))
      ;;end inner loop
      )
     (when (= (list-length filled-lists) num-lists)
       (return))
     ;;(break "after when")
     ;;end outer loop
     )
    filled-lists

    ;;end let, fill-lists
    ))
;;TEST
;; (fill-lists '((A B C D)(1 2 3 4 5 6)(X Y Z)) '(F1 F2 F3 F4 F5 F6 F7 F8 F9))
;; works= ((1 2 3 4 5 6) (A B C D F1 F2) (X Y Z F1 F2 F3))







;;MY-2COMBOS
;;
;;ddd
(defun my-2combos (list)
  "In U-lists RETURNS all-lists of possible combos of any 2 items in list (even if items are lists."
  (let
      ((item1 (car list))
       (restlist (cdr list))
       (newlist)
       (all-lists)
       )
  (loop
   for item2 in restlist
   do
   (setf newlist (list item1 item2)
         all-lists (append all-lists (list newlist)))
   )

   (when restlist
     (setf all-lists (append all-lists (my-combos  restlist))))
   
   all-lists
   ;;end let, all-possible-combos
   ))
;;TEST
;; (my-2combos '(a b c)) = ((A B) (A C) (B C))
;; (my-2combos  '((A B) (A C) (B C))) = (((A B) (A C)) ((A B) (B C)) ((A C) (B C)))
;; (my-2combos '(I L F))



                            


;;MAKE-POSSIBLE-2-LIST-COMBOS
;;
;;ddd
(defun make-possible-2-list-combos (list1 list2)
  "In U-lists.INPUT 2 lists. Makes new lists of all combos of items across list1 and list2. RETURNS (values all-combos all-n-combos). Used by ART to create PATH symbols."
  (let
      ((list1-n (list-length list1))
       (list2-n (list-length list2))
       (all-combos)
       (all-n-combos)
       )
    (loop
     for item1 in list1
     for n1 from 1 to list1-n
     do

     (loop
      for item2 in list2
      for n2 from 1 to list2-n
      do
      (setf all-combos (append all-combos (list (list item1 item2)))
            all-n-combos (append all-n-combos (list (list n1 n2))))
    ;;(afout 'out (format nil "item2= ~A all-combos= ~A~%" item2  all-combos))
      ;;end inner loop
      )

     ;;end outer loop
     )
    (values all-combos all-n-combos)
    ;;end let, make-possible-2-list-combos
    ))
;;TEST
;;  (make-possible-2-list-combos '((I  3 2)) '((I 1 3))) = (((I 3 2) (I 1 3)))
;;  (make-possible-2-list-combos '((I  3 2)) '((1 1 3)(2 1 3)(3 1 3)))
;; results= (((I 3 2) (1 1 3)) ((I 3 2) (2 1 3)) ((I 3 2) (3 1 3)))   ((1 1) (1 2) (1 3))
;;  (I ((1 4 3) (2 4 3) (3 4 3) (4 4 3)))
;;


;;  (make-possible-2-list-combos '((I L 1)(I L 2)) '((I L 1)(I L 2)(I L 3)))
;;  result= (((I L 1) (I L 1)) ((I L 1) (I L 2)) ((I L 1) (I L 3)) ((I L 2) (I L 1)) ((I L 2) (I L 2)) ((I L 2) (I L 3)))    ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3))
;;  (make-possible-2-list-combos '(A B C) '(D E F))
;;results= ((A D) (A E) (A F) (B D) (B E) (B F) (C D) (C E) (C F))    ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
;;FOR F
;;  (make-possible-2-list-combos '(F 2)'(F 3))
;; results= ((F F) (F 3) (2 F) (2 3))
;;  ((1 1) (1 2) (2 1) (2 2))
;;FOR L
;;results= ((L L) (L 1) (3 L) (3 1))
;;  ((1 1) (1 2) (2 1) (2 2))
;;FOR I
;;  (make-possible-2-list-combos '(I 1 2 3 4 5 ) '(I 1 2 3))
;; RESULTS= ((I I) (I 1) (I 2) (I 3) (1 I) (1 1) (1 2) (1 3) (2 I) (2 1) (2 2) (2 3) (3 I) (3 1) (3 2) (3 3) (4 I) (4 1) (4 2) (4 3) (5 I) (5 1) (5 2) (5 3))
;;   ((1 1) (1 2) (1 3) (1 4) (2 1) (2 2) (2 3) (2 4) (3 1) (3 2) (3 3) (3 4) (4 1) (4 2) (4 3) (4 4) (5 1) (5 2) (5 3) (5 4) (6 1) (6 2) (6 3) (6 4))


;;MY-3COMBOS
;;
;;ddd
#|(defun my-3combos (list)
  (let
      ((item1 (car list))
       (item2 (second list))
       (restlist (nthcdr 2 list))
       (newlist)
       (all-lists)
       )
  (loop
   for item3 in restlist
   do
  
   (setf newlist (list item1 item2 item3)
         all-lists (append all-lists (list newlist)))
   )

   (when restlist
     (setf all-lists (append all-lists (my-combos  restlist))))
   
   all-lists
   ;;end let, all-possible-combos
   ))|#
;;TEST
;; (my-3combos '(1 2 3 4 5 6 7 8 9 10))
;; RESULT= ((1 2 3) (1 2 4) (1 2 5) (1 2 6) (1 2 7) (1 2 8) (1 2 9) (1 2 10) (3 4) (3 5) (3 6) (3 7) (3 8) (3 9) (3 10) (4 5) (4 6) (4 7) (4 8) (4 9) (4 10) (5 6) (5 7) (5 8) (5 9) (5 10) (6 7) (6 8) (6 9) (6 10) (7 8) (7 9) (7 10) (8 9) (8 10) (9 10))
 ;;SSS START HERE FINISH THIS TO TEST WHAT ALL COMBOS LOOK LIKE


#|  DELETE -- MAKE SYMBOL TREE INSTEAD IN U-SYMBOL-TREES
;;MAKE-HIERARCHY-2-LIST-COMBOS
;;
;;ddd
(defun make-hierarchy-2-list-combos (topdims dimspecs1 dimspecs2 
                                             &key (separator-sym *art-node-separator))
  "In U-lists. Each item on topdims is a symbol (eg (M F L I) representing a different level (last level is highest).  dimspecs = ((n-items begin-index incr) for each level).  RETURNS (values all-combos-by-level flat-all-combos ). Used to make ART path names (eg WUP-F-L-I-TO-F-L-I). Topdims divided by separator-sym"
  (let
      ((x)
       (level)
       (flat-all-combos)
       (all-combos-by-level)
       (target-dim-n1)
       (target-dim-n1)
       (target-dim-nth1) 
       (target-dim-nth2) 
       )

    ;;find needed Ns
    (multiple-value-setq (n-dims  n-items sublist-ns n-ints)
        (dimlist-length topdims))
    (setf target-dim-n1 (first sublist-ns)
          target-dim-n2 (second sublist-ns)
          target-dim-nth1 (- target-dim-n1 1)
          (target-dim-nth2 (- target-dim-n2 1)))

    (multiple-value-setq (topdims topsubdims1 topsubdims2 
                                  n-topdims n-sub1-dims n-sub2-dims rootstr)
        (find-symdim-info topsym))
      (setf topsymvals (list rootstr topdims nil nil nil ))
      (set topsym topsymvals)))


          (values all-combos-by-level flat-all-combos )
          ;;end let, make-hierarchy-2-list-combos
          ))  
  |#            




;;SORT-LISTS-BY-BEGIN 
;;
;;ddd
(defun sort-keylists-by-begin (keylists &key (n-grouping-syms 1) 
                                        (sort-within-groups 'ascending) (test 'my-equal))
  "In U-lists, groups together all lists with the same beginning n-grouping-syms in a new list beginning  with those syms followed by a list of all the sublists.  If sort-within-groups, then sorted by ascending or descending.  RETURNS grouped-lists."
  (let
      ((labels)
       (labels-list)
       (labels-lists)
       (new-group)
       (matched-group)
       (matched-keylist)
       (grouped-keylists)
       (new-grouped-keylists)
       (labeled-groups)
       (ascending-p)
       (length-keylist)
       (other-items)
       (descending-group)
       (ascending-group)
       (sublist)
       (group)
       (keylist)
       )
  
    (cond
     ((listp keylists)
      (loop
       for keylist in keylists
       do
       (setf  length-keylist (list-length keylist)
              labels (butlast keylist (- length-keylist n-grouping-syms))
              labels-lists (append labels-lists (list labels))
              sublist (nthcdr n-grouping-syms keylist))
       ;;(afout 'out (format nil "AT 1 labels= ~A sublist= ~A~%" labels sublist))
       
       ;;should just be one match within the grouped-keylists
       (setf matched-group (car (get-keylists-by-begin labels grouped-keylists)))

       (cond
        (matched-group
         (setf new-group (append matched-group sublist))
         (multiple-value-setq (grouped-keylists group)
             (replace-list-if-same-begin labels new-group grouped-keylists)))             
        (t (setf new-group keylist
                 grouped-keylists (append grouped-keylists (list keylist)))))

       ;;(afout 'out (format nil "AT 2 matched-group= ~A new-group= ~A grouped-keylists= ~A" matched-group new-group grouped-keylists))

       ;;end loop keylists
       )
       ;;end listp keylists
       )
     ;;if not a list, add to the other-items list
     (t (setf other-items (append other-items (list keylist)))))

      ;;TO SORT WITHIN EACH GROUP (by sort-dim-n) ascending or descending
      (when sort-within-groups
        (when (equal sort-within-groups 'ascending)
          (setf ascending-p T))
         (multiple-value-setq (descending-group ascending-group)
             (my-sort-lists  n-grouping-syms  grouped-keylists :ascending-p ascending-p))
         (cond
          ((equal sort-within-groups 'ascending)
           (setf new-grouped-keylists
                 (append new-grouped-keylists (list ascending-group))))
          (t (setf new-grouped-keylists
                   (append new-grouped-keylists (list descending-group)))))
       
         (setf grouped-keylists (car new-grouped-keylists))
         ;;end loop,when sort groups, listp
         )

      (values grouped-keylists other-items  labels-lists )
      ;;end let, sort-keylists-by-begin
      ))
;;TEST
;; ;;(progn (setf out nil) (sort-keylists-by-begin '
;;(progn (setf out nil) (sort-keylists-by-begin  '((M ((I L F))) (F ((I L 2))) (L ((I 3 2))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2))) (L ((I 4 2))) (I ((1 4 2) (2 4 2) (3 4 2) (4 4 2)))  (L ((I 5 2))) (I ((1 5 2) (2 5 2) (3 5 2) (4 5 2)))  (F ((I L 3))) (L ((I 3 3))) (I ((1 3 3) (2 3 3) (3 3 3) (4 3 3))) (L ((I 4 3))) (I ((1 4 3) (2 4 3) (3 4 3) (4 4 3)))  (L ((I 5 3))) (I ((1 5 3) (2 5 3) (3 5 3) (4 5 3)))) :sort-within-groups 'descending))
;;WORKS=
;;((M ((I L F))) (F ((I L 2)) ((I L 3))) (L ((I 3 2)) ((I 4 2)) ((I 5 2)) ((I 3 3)) ((I 4 3)) ((I 5 3))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2)) ((1 4 2) (2 4 2) (3 4 2) (4 4 2)) ((1 5 2) (2 5 2) (3 5 2) (4 5 2)) ((1 3 3) (2 3 3) (3 3 3) (4 3 3)) ((1 4 3) (2 4 3) (3 4 3) (4 4 3)) ((1 5 3) (2 5 3) (3 5 3) (4 5 3))))    NIL    ((M) (F) (L) (I) (L) (I) (L) (I) (F) (L) (I) (L) (I) (L) (I))
#|PPRINTED
((M ((I L F)))
 (F ((I L 2)) ((I L 3)))
 (L ((I 3 2)) ((I 4 2)) ((I 5 2)) ((I 3 3)) ((I 4 3)) ((I 5 3)))
 (I
  ((1 3 2) (2 3 2) (3 3 2) (4 3 2))
  ((1 4 2) (2 4 2) (3 4 2) (4 4 2))
  ((1 5 2) (2 5 2) (3 5 2) (4 5 2))
  ((1 3 3) (2 3 3) (3 3 3) (4 3 3))
  ((1 4 3) (2 4 3) (3 4 3) (4 4 3))
  ((1 5 3) (2 5 3) (3 5 3) (4 5 3))))
|#
;; (progn (setf out nil) (sort-keylists-by-begin '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO)))))
;; RESULT= (((D (YES))) ((A ((THIS)))) ((B ((WHAT)))) ((X (Y (THAT)))) ((A ((ANOTHER)))) ((B (X (TOO)))))  ((B))
;;  ;; (progn (setf out nil) (sort-keylists-by-begin '((x (9 7 6))(a (f c x))(x 3 2)(b 8 9 9)(a (7 8 9)))))









;;GET-KEYLISTS-BY-BEGIN
;;
;;ddd
(defun get-keylists-by-begin (begin-syms keylists &key (test 'my-equal))
  "In U-lists.  Looks for syms in begin-syms in beginning of each keylist in keylists. RETURNS (values all-matched-keylists non-matched-items)."
  (let
      ((return-keylist)
       (all-matched-keylists)
       (non-matched-items)
       (begin-n (list-length begin-syms))
       (keylist-n)
       (begin-syms2)
       (result)
       )
    (loop
     for keylist in keylists
     do
     (cond
      ((listp keylist)
       (setf  keylist-n (list-length keylist))
       (when (> keylist-n begin-n)
         (setf  begin-syms2 (butlast keylist (- keylist-n begin-n))
                result (funcall test begin-syms begin-syms2))
         ;;was (my-equal begin-syms begin-syms2)))
         ;;if found append and reset or add to non-matched-items
         (cond
          (result
           (setf all-matched-keylists (append all-matched-keylists (list keylist))
                 result nil))
          (t (setf non-matched-items (append non-matched-items (list keylist)))))

         ;;end when,listp
         ))
      (t (setf non-matched-items (append non-matched-items (list keylist)))))
     ;;end loop
     )
    (values all-matched-keylists non-matched-items)
    ;;end let,get-keylists-by-begin
    ))
;;TEST
;; (get-keylists-by-begin '(a "b") '((1 2 3)(a b 3 4 5 (6)) (A B (this)) x  33))
;; works= ((A B 3 4 5 (6)) (A B (THIS)))   ((1 2 3) X 33)




;;INSERT-AT-N
;;modified 2020
;;added 2016-04
;;ddd
(defun insert-at-n (item nth list)
  "In U-lists, in list, inserts item at nth place. nth begins with 0."
  (let
      ((newlist)
       (list-n (list-length list))
       )
    (cond
     ;;if nth >= list length, append it to end
     ((>= nth list-n)
      (setf newlist (append list (list item))))
     ;;otherwise
     (T
      (loop
       for n from 0 to list-n
       for elem in list
       do
       (cond
        ((= n nth)
         (setf newlist (append newlist (list item elem))))
        (t (setf newlist (append newlist (list  elem)))))
       )))
    newlist
    ;;end let, insert-at-n
    ))
;;TEST
;; (insert-at-n 'b 1 '(a c d e)) = (A B C D E)
;; (insert-at-n 'this 2 '(a b)) = (A B THIS)
;; (insert-at-n 'x 0 '(a c d e)) = (X A C D E)



;;new 2016 --both work

;;((1 2) (1 3) (1 4) (1 5) 
;; (2 3) (2 4) (2 5)
;; (3 4) (3 5) (4 5))

;;(U(1 2) U(1 3) U(1 4) X(1 5) 
;; U(2 3) U(2 4)  X(2 5)
;; U(3 4) X(3 5) X(4 5))

;used  1 2,     1 2      1  2      1 3      1  3       1 4
;;     ((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5)
;used 2  3      2 3      2 4
;;      (2 3 4) (2 3 5) (2 4 5) 
;used 3 4
;;      (3 4 5))

;;




;;MAKE-COMBOS
;;2016
;;ddd
(defun make-combos (list1 items-per-combo
                          &key 
                          return-every-nth
                           random-sample-percent
                          randomize-p  return-all-combos-p
                          (prefix "")(postfix "")
                          global-combos-sym 
                          (randomize-min-combo-n 20))
  "In U-LISTS, creates combo-lists of items-per-combo elements and can return a randomized sample of random-sample-percent of the total. RETURNS  If return-all-combos-p, (values global-combos-sym return-combos all-combos) otherwise, (values  return-combos  n-combos new-global-combos-sym all-combos). "
  (let
      ((randomized-combos)
       (n-combos)
       (qnames)
       (nested-combos)
       (all-combos)
       (return-combos)
       (n-all-combos)
       (new-cat-combo-global-sym)
       (new-global-combos-sym)
       )

    ;;MAKE THE COMBOS
    (setf nested-combos (make-nested-combos list1 items-per-combo)
          all-combos (flatten-1-level nested-combos)
          n-all-combos (list-length all-combos))   
    ;;FUNCTION WORKS FAST TO THIS POINT WITH LARGE NUMBERS
    ;;(break "1")   

    (cond      
    ;;RANDOM SELECT COMBOS FROM EACH CATEGORY?
    ;;SSS PROBLEM WITH HANGING UP WITH LARGE NUMBERS
     ((and  (or randomize-p random-sample-percent)
            (> n-all-combos randomize-min-combo-n))
      (setf return-combos 
            (my-random-selection all-combos random-sample-percent)
            n-combos (list-length return-combos))
      (break "2")
      )
     (return-every-nth
      (multiple-value-setq (return-combos n-combos)
          (find-every-nth-item return-every-nth all-combos))  )

     (t (setf return-combos all-combos
              n-combos (list-length return-combos))))
    

    ;;SET A cat-combo-global-sym TO THE RETURN-COMBOS?
    (when global-combos-sym
      (cond
       ((or (not (equal prefix ""))(not (equal postfix ""))(stringp global-combos-sym))
        (setf new-global-combos-sym (my-make-symbol 
                                     (format nil "~A~A~A" prefix  global-combos-sym postfix))))
       ((symbolp global-combos-sym)
        (setf new-global-combos-sym global-combos-sym)
        (set new-global-combos-sym return-combos))
       (t nil)))

    (unless return-all-combos-p
      (setf all-combos  'all-combos-not-returned))

    (values  return-combos  n-combos new-global-combos-sym all-combos)
    ;;end let, make-combos
    ))
;;TEST
;; (make-combos   '(A B C D E F G H I J K L M N O P) 3 :return-every-nth 10 :global-combos-sym  'test-glsym4)
;; works= ((A B C) (A B M) (A C J) (A D H) (A E G) (A F G) (A G H) (A H J) (A I M) (A K L) (A M O) (B C I) (B D G) (B E F) (B E P) (B F P) (B H I) (B I L) (B J P) (B M N) (C D I) (C E H) (C F H) (C G I) (C H K) (C I N) (C K M) (C M P) (D E L) (D F L) (D G M) (D H O) (D J L) (D L M) (E F G) (E G H) (E H J) (E I M) (E K L) (E M O) (F G M) (F H O) (F J L) (F L M) (G H I) (G I L) (G J P) (G M N) (H I N) (H K M) (H M P) (I K L) (I M O) (J L M) (K L M) (L M N))   56   TEST-GLSYM4    ((A B C) (A B D) (A B E) (A B F) etc
;; (make-combos   '(A B C D E F G H I J K L M N O P) 3 :global-combos-sym  'test-glsym4)
;; works makes 56 of 560, sym evals to list -- like below

;; (make-combos   '(A B C D E F G H I J K L M) 3 :global-combos-sym  'test-glsym4)
;; works= ((A B M) (H I K) (C F M) (A G K) (B G J) (C G M) (J K M) (A B D) (A C J) (D F J) (A J L) (D G H) (C D H) (D L M) (G H K) (B D K) (C E I) (B E J) (H K L) (C G J) (C H I) (B F H) (A H M) (A F M) (F H M) (F I J) (J L M) (B F M) (B H M))   29   TEST-GLSYM4         ((A B C) (A B D) (A B E) (A B F) (A B G) (A B H) (A B I) (A B J) (A B K) (A B L) (A B M) (A C D) (A C E) (A C F) (A C G) (A C H) (A C I) (A C J) (A C K) (A C L) (A C M) (A D E) (A D F) (A D G) (A D H) (A D I) (A D J) (A D K) (A D L) (A D M) (A E F) (A E G) (A E H) (A E I) (A E J) (A E K) (A E L) (A E M) (A F G) (A F H) (A F I) (A F J) (A F K).... entire all-combos.
;;also TEST-GLSYM4  = ((A B M) (H I K) (C F M) (A G K) (B G J) (C G M) (J K M) (A B D) (A C J) (D F J) (A J L) (D G H) (C D H) (D L M) (G H K) (B D K) (C E I) (B E J) (H K L) (C G J) (C H I) (B F H) (A H M) (A F M) (F H M) (F I J) (J L M) (B F M) (B H M))


;; (make-combos '(testsym1 testsym2 testsym3 testsym4)  3 :global-combos-sym  'test-glsym2)
;;works= ((TESTSYM1 TESTSYM2 TESTSYM3) (TESTSYM1 TESTSYM2 TESTSYM4) (TESTSYM1 TESTSYM3 TESTSYM4) (TESTSYM2 TESTSYM3 TESTSYM4))   4   TEST-GLSYM2
;;also CL-USER 33 > TEST-GLSYM2 =  ((TESTSYM1 TESTSYM2 TESTSYM3) (TESTSYM1 TESTSYM2 TESTSYM4) (TESTSYM1 TESTSYM3 TESTSYM4) (TESTSYM2 TESTSYM3 TESTSYM4))
;;for csq
;; (make-combos *all-elmsyms 3 :global-combos-sym 'test3-glsym)
;; works=  ((MOTHER FATHER BEST-M-FRIEND) (MOTHER FATHER BEST-F-FRIEND) (MOTHER BEST-M-FRIEND BEST-F-FRIEND) (FATHER BEST-M-FRIEND BEST-F-FRIEND))    4   TEST3-GLSYM
;;also CL-USER 39 > TEST3-GLSYM  = ((MOTHER FATHER BEST-M-FRIEND) (MOTHER FATHER BEST-F-FRIEND) (MOTHER BEST-M-FRIEND BEST-F-FRIEND) (FATHER BEST-M-FRIEND BEST-F-FRIEND))





;;MAKE-COMBOS-FROM-LISTS
;;2016
;;ddd
(defun make-combos-from-lists (lists &key randomize-p 
                                     (return-remainders-p T) (group-rems-by-origlists-p T))
  "In U-lists. Takes item from each list and makes a new combo-list, and appends combo-lists. If not equal lengths, either makes lists from remainder or ends. RETURNS (values combo-lists remainders). Takes lists in order to create combos."
  (let*
      ((combo)
       (combos)
       (sorted-len-lists)
       (sorted-lists)
       (list)
       (num-lists (list-length lists))
       (max-n-combos)
       (max-len-combos)
       (remainder)
       (remainders)
       )
    (multiple-value-setq (sorted-lists sorted-len-lists)
        (sort-lists-by-length lists :ascending-p T))

    ;;MAX NUMBER OF COMBOS
    (setf max-n-combos (caar sorted-len-lists)
          max-len-combos (caar (last sorted-len-lists)))
  ;;(BREAK)
    ;;MAKE COMBOS AND REMAINDERS 
    (loop
     for nth from 0 to (- max-len-combos 1)
     do
     (setf combo nil
           remainder nil)
     (loop
      for list in lists
      do
      (cond
       ((<  nth max-n-combos)
        (setf combo (append combo (list (nth nth list))))
        )
       (t
        (setf remainder (append remainder (list (nth nth list))))))
   
      ;;end loop
      )
     ;;(afout 'out (format nil "remainder= ~A" remainder))
     
      ;;RANDOMIZE?
      (when (and randomize-p combo)
        (setf combo (my-randomize-list combo)))

     (when combo 
       (setf combos (append combos (list combo))))
     (when remainder
       (setf remainders (append remainders (list remainder))))
     ;;(break)
     (setf combo nil remainder nil)
     ;;end outer loop
     )
    ;;GROUP REMAINDERS BY ORIGINAL-LISTS
   (when group-rems-by-origlists-p
      (setf remainders  (group-items-by-nths remainders)))

    (values combos remainders num-lists)
    ))
;;TEST
;; (make-combos-from-lists  '((1 2 3 4 5)(a b c d e)(91 92 93 94 95)))
;; works= ((1 A 91) (2 B 92) (3 C 93) (4 D 94) (5 E 95))  NIL   3
;; (make-combos-from-lists '((1 2 3 4 5)(a b c d e)(91 92 93 94 95 96 97))) 
;; works= ((A 1 91) (B 2 92) (C 3 93) (D 4 94) (E 5 95))   (NIL NIL (96 97))    3
;; (make-combos-from-lists '((1 2 3 4 5 6 7 8)(a b c d e)(91 92 93 94 95 96 97))) 
;;works = ((1 A 91) (2 B 92) (3 C 93) (4 D 94) (5 E 95))    ((6 7 8) NIL (96 97))   3
;; (make-combos-from-lists '((1 2 3 4 5 6 7 8)(a b c d e)(91 92 93 94 95 96 97)) :randomize-p T)   
;; works= ((A 1 91) (B 2 92) (C 3 93) (4 D 94) (E 5 95))   ((6 7 8) NIL (96 97))   3




;;MAKE-FLAT-COMBOS
;;2016
;;ddd
(defun make-flat-combos (list1 items-per-combo &optional list2)
"In U-lists. Creates a list of all combinations of items taken items-per-combo at a time.  RETURNS (values all-combos  n-combos). Uses make-combos, then returns FLAT list of all new combos."
   (let
       ((all-flat-combos)
        (allcombos-n)
        (nested-combos)
        )
     (setf nested-combos 
           (make-nested-combos list1 items-per-combo list2))

     (setf all-flat-combos (flatten-list-tree nested-combos)
           allcombos-n (list-length all-flat-combos))
     (values all-flat-combos allcombos-n)
     ;;end let, make-flat-combos
     ))
;;TEST
;;  (make-flat-combos '(1 2 3 4 5) 3)
;; works= ((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5))   10


;;MAKE-NESTED-COMBOS
;;
;;ddd
(defun  make-nested-combos (list1 items-per-combo &optional list2)  
  "In U-lists. Creates a list of all combinations of items taken items-per-combo at a time.  RETURNS (values all-combos  n-combos). Note: Must return NESTED lists. Flatten after final return if want flat list. (old make-combos)."
  (let
      ((list2n) 
       (newcombos)
       (allnewcombos)
       (allcombos-n)
       (newlist1)
       (newlist2)
       (subcombos)
       (combos-n)
       (subcombo-n)
       (allcombos)
       )
    (when (null list2)
      (setf list2  list1))
    (setf  list2n (list-length list2))

    (loop
     for item1 in (butlast list1 ) 
     for j from 1 to list2n
     do
     (setf subcombos
            (append-item-to-lists  (nthcdr j list2)  item1   :to-front-p T)) 
     (setf subcombo-n (list-length (car subcombos)))

     ;;(BREAK "before recurse")
    
     ;;RECURSE TO LENGTHEN LIST1-N?
     (cond
      ((and subcombo-n (> items-per-combo subcombo-n))

       (setf newlist2 (nthcdr  j  list2)
             newlist1 subcombos) ;; (butlast subcombos)) ;;  j))

       (multiple-value-setq (newcombos combos-n) 
           (make-nested-combos newlist1  items-per-combo  newlist2))
       (when newcombos
         (setf allnewcombos (append allnewcombos (list newcombos))))
       ;;clause
       )
      (t (when subcombos (setf allnewcombos (append allnewcombos subcombos)))))
     ;;end loop
     )
    
    (when allnewcombos
      (setf  allcombos (append allcombos allnewcombos)
             allcombos-n (list-length allcombos)))
        
    (values allcombos allcombos-n) 
    ;;end let, make-nested-combos
    ))
;;TEST
;; (make-nested-combos '(1 2 3 4 5) 2)
;; = (((1 2) (1 3) (1 4) (1 5)) ((2 3) (2 4) (2 5)) ((3 4) (3 5)) ((4 5)))
;; (make-nested-combos '(1 2 3 4 5) 3)
;;  = (((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5)) ((2 3 4) (2 3 5) (2 4 5)) ((3 4 5)))    3
;; (make-nested-combos '(1 2 3 4 5) 4)
;; = ((((1 2 3 4) (1 2 3 5) (1 2 4 5)) ((1 3 4 5))) (((2 3 4 5))))   2
;; (make-nested-combos '(1 2 3 4 5) 5)
;; = (((((1 2 3 4 5)))))   1
;; (make-nested-combos '(1 2 3 4 5 6 7 8) 4)
;; works= ((((1 2 3 4) (1 2 3 5) (1 2 3 6) (1 2 3 7) (1 2 3 8) (1 2 4 5) (1 2 4 6) (1 2 4 7) (1 2 4 8) (1 2 5 6) (1 2 5 7) (1 2 5 8) (1 2 6 7) (1 2 6 8) (1 2 7 8)) ((1 3 4 5) (1 3 4 6) (1 3 4 7) (1 3 4 8) (1 3 5 6) (1 3 5 7) (1 3 5 8) (1 3 6 7) (1 3 6 8) (1 3 7 8)) ((1 4 5 6) (1 4 5 7) (1 4 5 8) (1 4 6 7) (1 4 6 8) (1 4 7 8)) ((1 5 6 7) (1 5 6 8) (1 5 7 8)) ((1 6 7 8))) (((2 3 4 5) (2 3 4 6) (2 3 4 7) (2 3 4 8) (2 3 5 6) (2 3 5 7) (2 3 5 8) (2 3 6 7) (2 3 6 8) (2 3 7 8)) ((2 4 5 6) (2 4 5 7) (2 4 5 8) (2 4 6 7) (2 4 6 8) (2 4 7 8)) ((2 5 6 7) (2 5 6 8) (2 5 7 8)) ((2 6 7 8))) (((3 4 5 6) (3 4 5 7) (3 4 5 8) (3 4 6 7) (3 4 6 8) (3 4 7 8)) ((3 5 6 7) (3 5 6 8) (3 5 7 8)) ((3 6 7 8))) (((4 5 6 7) (4 5 6 8) (4 5 7 8)) ((4 6 7 8))) (((5 6 7 8))))    5
;; (setf *all-combos (make-nested-combos *all-elmsyms 3))
;; (setf *all-flat-combos (flatten-1-level *all-combos))
;; (list-length *flat-all-combos) = 35990








;;DIVIDE-LIST
;;2016
;;ddd
(defun divide-list (list num-lists &key (equal-parts-p t)  percent-part-list group-n-list)
  "In U-lists, divides list into num-lists equal parts (if equal-parts-p) or if PART-LIST, then divides according to percents in percent-part-list"
  (let*
      ((list-n (list-length list))
       (percent-per-group)
       (n-per-group)
       (group-n)
       (group-nth 0)
       (new-lists)
       (sublist-n 0)
       (sublist-nth 0)
       (sublist)
       (restlist)
       )
    ;;CONVERT PERCENTS and NUMBERS TO A LIST OF SUBLIST Ns
    (cond
     ((and equal-parts-p (null  group-n-list)(null percent-part-list))
      (setf percent-per-group (ceiling (*(/ (/  list-n  num-lists ) list-n) 100.00))
            n-per-group (ceiling (/ list-n num-lists)))

      ;;(BREAK)
      (loop
       for n from 1 to num-lists
       do
       (setf percent-part-list (append percent-part-list (list percent-per-group))
             group-n-list (append group-n-list (list n-per-group)))
             
       ;;end loop, equal parts
       ))
     (percent-part-list
      (setf num-lists (list-length percent-part-list))
      (loop
       for percent in percent-part-list
       do
       (setf group-n (ceiling (/ (* percent list-n) 100))
             group-n-list (append group-n-list (list group-n)))
       ;;end loop,percent-part-list
       ))
     (group-n-list
      (setf num-lists (list-length group-n-list))
      )
     (t nil))
      ;;(break "after percent")
    ;;initialize sublist-n
    (setf sublist-n (car group-n-list))

    ;;MAIN  DIVIDER LOOP USING GROUP-N-LIST
    (loop
     for item in list
     for n from 0 to list-n
     do     
     (cond
      ;;in case left over items
      ((and (> group-nth num-lists)(>  sublist-nth   sublist-n))
       (setf sublist (append sublist (list item))
             new-lists (append new-lists (list sublist))
             restlist  (nthcdr (+ n 1) list))
       ;;(BREAK "IN (> group-nth num-lists)")
       )
      ((or (=   sublist-nth  (- sublist-n 1) )
           (= n list-n))
       (incf group-nth)
       (setf sublist (append sublist (list item))
             new-lists (append new-lists (list sublist))
             sublist-n (nth group-nth  group-n-list)
             sublist nil
             sublist-nth 0)
       ;;(BREAK "IN (=   sublist-nth  (- sublist-n 1) )")
       (unless (> group-nth num-lists)
         sublist-n (nth group-nth group-n-list))
       )
      (t (setf sublist (append sublist (list item)))         
         (incf sublist-nth)))
     ;;(BREAK "END OF LOOP")
     ;;end  loop
     ) 
    (when sublist
      (setf new-lists (append new-lists (list sublist))
            restlist nil))
    
    (values new-lists restlist)
    ;;end let, divide-list
    ))
;;TEST
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) 3) =  ((1 2 3 4) (5 6 7 8) (9 10))  NIL
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) 5) = ((1 2) (3 4) (5 6) (7 8) (9 10))  NIL
;; (equal-parts-p t)  percent-part-list group-n-list)
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) nil :group-n-list '(5 2 3))
;; works= ((1 2 3 4 5) (6 7) (8 9 10)) NIL
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) nil :percent-part-list '(50 20 30))
;; works= ((1 2 3 4 5) (6 7) (8 9 10)) NIL
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) nil :percent-part-list '(33 25 50))
;; works= ((1 2 3 4) (5 6 7) (8 9 10))  NIL


;;DIVIDE-LIST-INTO-PARTS
;;2020
;;ddd
(defun divide-list-into-parts (n-items-list list)
  "U-lists   RETURNS  (values  divided-list rest)  INPUT: N-ITEMS-LIST Eg, (1 3 2 REST) If REST included, returns rest of list.  If not extra items included in REST value. "
  (let*
      ((divided-list)
       (rest list)
       (rest-n (list-length rest))
       )
    (loop 
     for n-items in n-items-list
     do
     (cond
      ((equal n-items 'rest)
       (setf divided-list (append divided-list (list rest))
             rest nil))
      ((< n-items rest-n)
       (let*
           ((part (subseq rest 0 n-items))
            )
         (setf rest-n (- rest-n  n-items)
               divided-list (append divided-list  (list part)))
         (when (> rest-n 0)
           (setf rest (subseq rest n-items)))
         ;;end let, >
         ))
      (t nil))
     ;;end ,loop
     )
     (values  divided-list rest)
     ;;end let, divide-list-into-parts
     ))
;;TEST
;; (divide-list-into-parts '(2 1 rest) '(a b c d e f g h))
;;works= ((A B) (C) (D E F G H))  NIL
;;  (divide-list-into-parts '(2 1 3) '(a b c d e f g h))
;;works = ((A B) (C) (D E F))   (G H)



;;DIVIDE-LIST-IN-ORDER
;;2016
;;ddd
(defun divide-list-in-order (list num-lists)
  "In U-lists, divides a list into num-lists, taking items in order and putting item1 in list1, item2 in list2, etc."
  (let*
      ((newlists)
       (list-n (list-length list))
       (listn  0)
       )
    ;;
    (loop
     for item in list
     for nth from 0 to list-n
     do
     (incf listn)

     (cond
      ((< nth num-lists)
       (setf newlists (append newlists (list (list item)))))
      (t
       (setf newlists (append-nth-nested-list item listn  newlists))
       ))
       (when (= listn num-lists)
         (setf listn 0))
     ;;end loop
     )
    ;;vvvv
    newlists
    ;;end let, divide-list-in-order
    ))
;;TEST
;; (divide-list-in-order '(1 2 3 4 5 6 7 8 9 10 11 12) 3) 
;; works= ((1 4 7 10) (2 5 8 11) (3 6 9 12))
;;  (divide-list-in-order '(1 2 3 4 5 6 7 8 9 10 11)  3)
;; works= ((1 4 7 10) (2 5 8 11) (3 6 9))






;;GET-PERCENT-LIST
;;2016
;;ddd
(defun get-percent-list (percent list &key (begin-n 1) return-restlist-p)
  "In U-lists, RETURNS list of first percent of items from begin-n in list. It returns the number of items higher than odd percent. When RETURN-RESTLIST-P, RETURNS (values newlist restlist)."
  (let*
      ((newlist)
       (list-n (list-length list))
       (begin-nth (- begin-n 1))
       (target-nth (- (+ (ceiling (* list-n (/ percent 100.00)))  begin-nth) 1))
       (restlist)
       )
    (when (>= target-nth list-n)
      (setf target-nth (- list-n 1)))

    (loop
     for nth from begin-nth to target-nth
     do
     (setf newlist (append newlist (list (nth nth list))))
     ;;end loop
     )
    
    (when  return-restlist-p 
      (setf restlist (nthcdr (+ target-nth 1) list)))

    (values newlist restlist)
    ;;end let, get-percent-list
    ))
;;TEST
;;  (get-percent-list 30 '(1 2 3 4 5 6 7 8 9 10))  = (1 2 3 4) NIL
;;  (get-percent-list 30 '(1 2 3 4 5 6 7 8 9 10) :return-restlist-p T) 
;;   works= (1 2 3 4)   (5 6 7 8 9 10)
;; (get-percent-list 34 '(1 2 3 4 5 6 7 8 9 10) :return-restlist-p T)





;;DIVIDE-LISTS-IN-LIST
;;2017
;;ddd
(defun divide-lists-in-list (list n-sublists)
  "In U-lists, Divides the sublists of a list into n-sublists (generally n-sublists = num items in each sublist)  RETURNS  all-item-sublists"
  (let
      ((item-sublists (make-list n-sublists :initial-element NIL))
       (all-item-sublists)
       )
    (loop
     for item-sublist in item-sublists
     for n from 0 to n-sublists
     do
     (loop
      for sublist in list
      do
      (when (listp sublist)
        (setf item-sublist (append item-sublist (list (nth n sublist)))))
      ;;end loop
      )
     (setf all-item-sublists (append all-item-sublists (list item-sublist)))
     ;;end outer loop
     )
    all-item-sublists
    ;;end let, divide-lists-in-list
    ))
;;TEST
;; (divide-lists-in-list '((a b c)(1 2 3)(x y z w)(1st 2nd)) 3)
;; works= ((A 1 X 1ST) (B 2 Y 2ND) (C 3 Z NIL))








;;MAKE-ORDERED-ITEMS-SUBLISTS
;;2017
;;ddd
(defun make-ordered-items-sublists (list &key ascending-p (nth-item 1)
                                         (n-ordered-lists 2) (initial-element " ") 
                                         return-all-sublists-p)
  "In U-lists. In a list of lists, makes a sublist for each 1st, 2nd, etc up to n-ordered-lists  list items and orders ALL sublists by the lists' nth-item. Fills in missing items with NIL. If list is a simple list, fills in ratings-list with all initial-element.  RETURNS (values items-list  ratings-list all-sublists)"
  (let
      ((items-ranks-list)
       (ascending-list)
       (descending-list)
       (ordered-items-list)
       (all-sublists)
       (items-list)
       (ratings-list)
       (n-items (list-length list))
       ;;(n-items (list-length list))
       )
    (cond
     ;;IS IT A SIMPLE LIST OR LIST OF LISTS?
     ((listp (car list))
      ;;SORT BY RATINGS
      (multiple-value-setq (ascending-list descending-list)
          (my-sort-lists nth-item list))
      (cond
       (ascending-p
        (setf ordered-items-list ascending-list))
       (t (setf ordered-items-list descending-list)))
      ;;DIVIDE INTO n-ordered-lists LISTS
      (setf all-sublists (divide-lists-in-list ordered-items-list n-ordered-lists)
            items-list (car all-sublists)
            ratings-list (nth nth-item all-sublists))
      ;;end clause
      )
     ;;otherwise, make ratings-list of all empty strings
     (t (setf items-list list
              ratings-list (make-list n-items :initial-element initial-element))))
    (cond
     (return-all-sublists-p
      (values items-list  ratings-list all-sublists))
     (t (values items-list  ratings-list)))
    ;;end let, make-ordered-items-sublists
    ))
;;TEST
;; (make-ordered-items-sublists '(A B C D E))
;; works= (A B C D E)  ("  " "  " "  " "  " "  ") 
;;
;; (make-ordered-items-sublists '((A .4) (B .2)( C .1)  (D .7)( E .3)) :return-all-sublists-p T)
;; works= (C B E A D)    (0.1 0.2 0.3 0.4 0.7)  ((C B E A D) (0.1 0.2 0.3 0.4 0.7))
;; (make-ordered-items-sublists '((A  a1 .4) (B b1 .2)( C c1 .1)  (D  d1 .7)( E e1 .3)) :nth-item 2 :n-ordered-lists 3 :return-all-sublists-p T)
;;works=  (C B E A D)   (0.1 0.2 0.3 0.4 0.7)   ((C B E A D) (C1 B1 E1 A1 D1) (0.1 0.2 0.3 0.4 0.7))











;;APPEND-ITEM-TO-LISTS
;;2016
;;ddd
(defun append-item-to-lists  (lists  item &key to-front-p)
  "Appends an item (symbol, string, number, list) to EACH LIST in lists. If TO-FRONT-P, puts item in front of each returned list. RETURNS newcombos. Does NOT test to see if item is in list."
  (let
      ((newcombo)
       (newcombos)
       (len-lists (list-length lists))
       )
    (loop
     for list in lists
     do
     (cond
      ((listp item)
       (cond
        (to-front-p
         (setf newcombo (append  item (list  list))))
         (t 
          (setf newcombo (append (list list) item))))
       )
      (t (cond
          (to-front-p
           (setf newcombo (list item list)))
          (t 
           (setf newcombo (list list item))))))

     (setf newcombos (append newcombos (list newcombo)))
     ;;end loop
     )
    newcombos
    ))
;;TEST
;; (append-item-to-lists '(1 2 3 4)  5) = ((1 5) (2 5) (3 5) (4 5))
;; (append-item-to-lists '((1 5) (2 5) (3 5) (4 5))  4) 
;;  = ((1 5 4) (2 5 4) (3 5 4) (4 5 4))
;; ;; (append-item-to-lists '(3 4 5) '(1 2) :to-front-p t);;  
;;  = ((1 2 3) (1 2 4) (1 2 5))






;;APPEND-NEW-COMBOS
;;2016
;;ddd
(defun append-new-combos (newitem  lists)
  "In U-lists.  Appends newitem to BEGIN of all lists if NOT IN THE LIST. RETURNS newlists."
  (let
      ((newlist)
       (newlists)
       )
    (loop
     for list in lists
     do
     (when (not (my-member newitem list))
       (setf newlist (append (list newitem) list)))
     (when newlist
       (setf newlists (append newlists (list newlist))))
     ;;reset 
     (setf newlist nil)           
     ;;(afout 'out (format nil "list= ~A newlist= ~A newlists= ~A~%" list newlist newlists))
     )
    newlists
    ;;end let, append-combos
    ))
;;TEST
;; (append-new-combos 1  '((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5)))
;; WORKS = ((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5)) 
 ;;NOTE:can make less 3ple lists that don't already include 1
;;
;;(append-new-combos 2  '((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5)))
;; works= ((2 1 3) (2 1 4) (2 1 5) (2 1 5) (2 1 5) (2 1 5) (2 3 4) (2 3 5) (2 4 5))







;;APPEND-ITEMS-TO-LIST
;;
;;2016
;;ddd     
(defun append-items-to-list (list items &key (items-per-append 1))
  "In U-lists, appends each item in items ITEMS-PER-APPEND at a time to items in list. Good foundation for making combinations."
  (let
      ((newlist list)
       (newlists)
       (n-items (list-length items))
       (num-added 0)
       (rest-items)
       )
    (loop
     for item in items
     for n from 1 to n-items
     do
     (incf num-added)
     (cond
      ((= n n-items)
       (cond
        ((= num-added items-per-append)
         (setf newlist (append newlist (list item))
          newlists (append newlists (list newlist))))
        (t
         (setf rest-items (append rest-items (list item))))))
      ((= num-added  items-per-append)           
       (setf newlist (append newlist (list item))
               newlists (append newlists (list newlist))
               newlist list
              rest-items  nil             
             num-added 0))
      ((< n n-items)
       (setf newlist (append newlist (list item))
              rest-items (append rest-items (list item))))
      (t nil))
    
     ;;end loop
     )
    (values newlists rest-items)
    ;;end let, append-items-to-list
    ))
;;TEST
;; (append-items-to-list '(A B C) '(1 2 3 4 5 6  7))
;; works= ((A B C 1) (A B C 2) (A B C 3) (A B C 4) (A B C 5) (A B C 6) (A B C 7))      NIL
;; (append-items-to-list '(A B C) '(1 2 3 4 5 6  7) :items-per-append 2)
;; works = ((A B C 1 2) (A B C 3 4) (A B C 5 6))    (7)
;; (append-items-to-list '(A B C) '(1 2 3 4 5 6  7 8) :items-per-append 3)
;;  works= ((A B C 1 2 3) (A B C 4 5 6))   (7 8)
;; (append-items-to-list '(A B C) '(1 2 3 4 5 6  7 8) :items-per-append 5)
;; works= ((A B C 1 2 3 4 5))  (6 7 8)



;;MY-LISTP
;;2016
;;ddd
(defmacro my-listp (item &key (min-length 1))
  "In U-lists, test item to see if it is a list.  Unlike listp, item can be unbound and must be a list of at least MIN-LENGTH (default = 1). NOTE: If min-length = 0, then NIL RETURNS 0 NOT NIL. RETURNS LIST-LENGTH OR NIL."
  `(let 
      ((len-list)
       (result)
       )
     (cond
      ((and (symbolp (quote ,item))
            (boundp (quote ,item))
            (listp ,item))
       (setf len-list (list-length ,item))
      (cond
       ((and (= len-list 0)(= ,min-length 0))
        (setf result len-list))
       ((>= len-list ,min-length)
        (setf result len-list))
       (t nil))
      ;;end and clause
      )
      ;;a symbol, string, number, unboundp etc.
      ((or (and (symbolp (quote ,item))
                (null (boundp (quote ,item))))
            (numberp (quote ,item))
            (stringp (quote ,item)))
       NIL)
      ((listp  ,item)
       (setf result (list-length ,item)))
      (t nil))
     (when (and (equal result 0)
                (not (= ,min-length 0)))
       (setf result nil))
     (values result (quote ,item))
    ;;end let, my-listp
    ))
;;TEST
;; (my-listp (third '(EXTERNALCONT))) = NIL  (THIRD (QUOTE (EXTERNALCONT)))
;;  (my-listp 'axx) = NIL (QUOTE AXX)
;;  (my-listp 22) = NIL  22
;;  (my-listp "axx") = NIL  "axx"
;;  (my-listp NIL) = NIL NIL
;; (my-listp NIL :min-length 0)  = 0
;;  (my-listp '(a b c)) = 3   (QUOTE (A B C))
;;  mother = ("MOTHER" "mother" ELM2-1-1-99 NIL NIL :BIPATH (((MOTHER NIL (TALL3 (POLE2) NIL))) ((MOTHER NIL (GREEN (POLE2) NIL)))))
;; (my-listp 'mother) = nil (QUOTE MOTHER)
;; (my-listp mother) = 7  MOTHER
;; (setf  short-list '(1))
;; (my-listp short-list) = 1 short-list
;; (my-listp short-list   :min-length 2) = NIL SHORT-LIST
;; (my-listp  'short-list) = NIL  (QUOTE SHORT-LIST)
;; (setf long-list '(a b c d e))
;; (my-listp long-list   :min-length 2) = 5 LONG-LIST
;; (my-listp 'long-list   :min-length 2) = NIL  (QUOTE LONG-LIST)


;;SYMBOL-LISTP
;;2016
;;ddd
(defmacro symbol-listp (sym &key (min-length 1) (return-listp t))
  "In U-lists.  Tests to see if symbol is bound and evals to a list.  RETURNS (values result list len-list) RESULT = nil or list length.  LIST = evaled symbol list or nil. SYM must be  bound or error."
  `(let
       ((result)
        (len-list)
        (list)
        )
     (when (symbolp ,sym)
       (cond
        ((and  (boundp  ,sym))      
         (setf len-list (list-length (eval  ,sym)))
         (cond
          ((and (= len-list 0)(= ,min-length 0))
           (setf result len-list)
           (when ,return-listp
             (setf list (eval ,sym))))
          ((>= len-list ,min-length)
           (setf result len-list)
           (when ,return-listp
             (setf list (eval ,sym))))
          (t nil))
         ;;end and clause
         )
        (t nil))
       ;;end when
       )
     (values result list)
     ;;end let, symbol-listp
     ))
;;TEST
;; (symbol-listp 'mother) = 7   ("MOTHER" "mother" ELM2-1-1-99 NIL NIL :BIPATH (((MOTHER NIL (TALL3 (POLE2) NIL))) ((MOTHER NIL (GREEN (POLE2) NIL)))))
;; (symbol-listp mother) = nil nil



;;UNQUOTED-LISTP
;;2016
;;ddd
(defmacro unquoted-listp (x)
  "In U-lists. Tests whether the the UNQUOTED symbol evals to a list. Also, see my-listp, which does much more."
  `(cond
   ((and (symbolp (quote ,x))
         (boundp (quote ,x)))
    (listp ,x))
   (t nil))
  )
;;TEST
;; (unquoted-listp 'this) = nil  ;;this is unbound
;; (unquoted-listp 'mother) = nil
;; (unquoted-listp mother) = T



        


 
;;FIND-SYMBOLS-FOR-NAMES
;;2016
;;ddd
(defun find-symbols-for-names (namelist)
  "In U-lists, uses my-make-symbol to return a list of symbols from a list of strings/names."
        (loop
         for name in namelist
         do
         collect (my-make-symbol name)
         )
    ;;end find-symbols-for-names
    )    
;;TEST
;; (find-symbols-for-names '("mom" "dad" "best-m-friend")) 
;;works = (MOM DAD BEST-M-FRIEND)
     


;;FIND-SYMBOLS-IN-LIST
;;2020
;;ddd
(defun find-symbols-in-list (list &key (omit-nils-p T) omit-keywords-p)
  "In U-lists, uses my-make-symbol to return a list of symbols from a list of strings/names."
  (let*
      ((symbol-list)
       )
        (loop
         for item in list
         do
         (let
             ((omit-item-p)
              )
         (when (and omit-nils-p (equal item NIL))
           (setf omit-item-p T))
         (when (and omit-keywords-p (keywordp item))
           (setf omit-item-p T))
         (when (and (symbolp item) (not omit-item-p))
           (setf symbol-list (append symbol-list (list item))))
         ))
        symbol-list
    ;;end find-symbols-in-list
    ))
;;TEST
;; (find-symbols-in-list '(A "this" that 99 nil :key (list)))  = (A THAT :KEY)
;; (find-symbols-in-list '(A "this" that 99 nil :key (list)) :omit-nils-p NIL)
;;  = (A THAT NIL :KEY)
;; (find-symbols-in-list '(A "this" that 99 nil :key (list)) :omit-keywords-p T)
;; = (A THAT)



;;SET-CATSYM-TO-NESTED-NAMES
;;2016
;;ddd
(defun set-catsym-to-nested-names (all-catlists 
                                   &key (return-all-nested-names-p T) (append-prefix "*")
                                   (append-postfix ""))
  "In U-lists, sets global vars by name of  * + pce-cat to the qvar-names within the cat--for all pce-cats, where the lists are the CDR of the catlist, and the nested-names are the CAR of each sublist. RETURNS (values catvars globalcatvars all-nested-names) if return-all-nested-names-p."
  (let
      ((catlists)
       (catlist)
       (catvar) 
       (globalcatvar)
       (globalcatvars)
       (catvars)
       (catsublists)
       (nested-names)
       (all-nested-names)
       )
    (loop
     for catlist in all-catlists
     do
     (when (listp catlist)
     (setf catvar (car catlist)
           globalcatvar (my-make-symbol 
                         (format nil "~A~A~A" append-prefix catvar append-postfix))
           globalcatvars (append globalcatvars (list globalcatvar))
           catvars (append catvars (list catvar))
          catsublists (cdr catlist))

     (when (listp catsublists)
       (setf nested-names (get-all-nths-in-lists 0 catsublists)
             all-nested-names (append all-nested-names (list nested-names))))

     ;;set the catvar to nested-names
     (set globalcatvar nested-names)
     ;;end when,loop
     ))

    (cond
     (return-all-nested-names-p
      (values catvars globalcatvars all-nested-names))
     (t (values catvars globalcatvars)))
     ;;end let, set-catsym-to-nested-names
     ))
;;TEST
;; (set-catsym-to-nested-names '((CA1 ("name1" b c)("name2" d e))(CA2 ("name3" f g)("name4" x y z)) sym1 "this"))
;;works= 
;;(CA1 CA2)   (*CA1 *CA2)   (("name1" "name2") ("name3" "name4"))
;;CL-USER 18 > *ca1 =  ("name1" "name2")
;;
;;  (set-catsym-to-nested-names '((CA1 ("name1" b c)("name2" d e))(CA2 ("name3" f g)("name4" x y z)) sym1 "this") :append-postfix "-post")
;; works= (CA1 CA2)   (*CA1-POST *CA2-POST)   (("name1" "name2") ("name3" "name4"))
;; CL-USER 26 > *CA2-POST  =  ("name3" "name4")







;;GET-KEYVALUE-IN-NESTED-LIST
;;2016-08
(defun get-keyvalue-in-nested-list (key-spec-lists  nested-lists 
                                                    &key  return-list-p return-value&keylist-p
                                                    (test 'my-equal) (max-list-length 1000))
  "In U-lists, NEW: Can use KEY for key-spec-lists [searches all). RETURNS (values return-value return-keylist new-keylist return-nested-lists  last-key-found-p ) unless RETURN-LIST-P then (values return-keylist return-value new-keylist return-nested-lists  last-key-found-p) .
    In U-lists. KEYSPEC= (key keyloc-n val-nth).  FOR KEY = :NTH (to find nth in list without key) :  (1) Can have exact loc of all keys, keyloc-n as a number OR can be NIL or T [In which case will check entire list at that level for key]. 
   If the KEY = T, searches entire list.  DOES NOT check for keys in any other location than keyloc-n (unless T, NIL);   (2) Each new level does NOT need a key in the key-spec-lists (it can be a list with lists containing keys; and (3) VALUE MUST either be an item next to key (val-nth = 1) OR val-nth [the optional 3rd item in the level spec-list].  If :NR member spec-list, sets RECURSE-FOR-KEY-P to NIL If :R member, sets it to T, then key MUST be IN LIST on FIRST LEVEL of current recursion (doesn't recurse on inner lists for that spec-list). 
   If  :NEXT-KEY-IN-VALUE is a member of  spec-list,  searches the VALUE LIST for list with next key. See doc for main function, get-set-append-delete-keyvalue-in-nested-list Eg, ((:key3 T 2)(:key4 0))"
  ;;2020
  (when (symbolp key-spec-lists)
    (setf key-spec-lists (list (list key-spec-lists T))))
  (multiple-value-bind (return-keylist return-nested-lists new-keylist
                                       return-value old-keylist last-key-found-p )
      (get-set-append-delete-keyvalue-in-nested-list :get  key-spec-lists  nested-lists 
                                              :return-list-p return-list-p :test test
                                              :append-value-p nil 
                                              :max-list-length max-list-length)
  
    (cond
     ;;when return-list-p, switch return-value and return-keylist
     (return-list-p (values return-keylist return-value new-keylist return-nested-lists  last-key-found-p))
     (return-value&keylist-p
      (values return-value return-keylist))
     (t
      (values return-value return-keylist new-keylist return-nested-lists  last-key-found-p )))
    ;;end mvb, get-keyvalue-in-nested-list
    ))
;;TEST
;; (get-keyvalue-in-nested-list "Sex" '(("sID" :TEXT-DATA ("Name" " " :SINGLE " ")("UserID" "222222" :SINGLE "222222") ("Sex" "Male" :SINGLE "Male" 1) ("Age" 22 :SINGLE 22 22) ("Email" "" :SINGLE "") ("USA?" "USA" :SINGLE "USA" 1) ("Nation" "USA" :SINGLE "USA") ("ZipCode" 44444 :SINGLE 44444) ("HrsWork" 22 :SINGLE 22 22))))
;; works= "Male"  ("Sex" "Male" :SINGLE "Male" 1)  ("Sex" "Male" :SINGLE "Male" 1)  (("sID" :TEXT-DATA ("Name" " " :SINGLE " ") ("UserID" "222222" :SINGLE "222222") ("Sex" "Male" :SINGLE "Male" 1) ("Age" 22 :SINGLE 22 22) ("Email" "" :SINGLE "") ("USA?" "USA" :SINGLE "USA" 1) ("Nation" "USA" :SINGLE "USA") ("ZipCode" 44444 :SINGLE 44444) ("HrsWork" 22 :SINGLE 22 22)))  T
;;
;; (get-keyvalue-in-nested-list 'utype *all-csq-qvars :return-value&keylist-p T)
;; works=
;;return-value= 
#|("utype" "UserType" NIL NIL (:TITLE ("SHAQ CARES:
 Selection of Your Questionnaires-Part 1") :QUEST ("  ==> FIND YOUR HAPPINESS QUOTIENT (HQ). 
         Research shows that 75% of people's overall happiness score is accounted for by the SHAQ HQ score (Stevens, 2009). Your HQ values, beliefs, and life skills may be powerful influences on your past, current, and future happiness. 
         HQ factors are CONTROLLABLE factors: you can choose to be happy by improving them. 
         ==> Why are you taking SHAQ?")) (:HELP NIL NIL) NIL :MULTI-ITEM)|#
;;return-keylist=
  #|(UTYPE ("utype" "UserType" NIL NIL (:TITLE ("SHAQ CARES:
 Selection of Your Questionnaires-Part 1") :QUEST ("  ==> FIND YOUR HAPPINESS QUOTIENT (HQ). 
         Research shows that 75% of people's overall happiness score is accounted for by the SHAQ HQ score (Stevens, 2009). Your HQ values, beliefs, and life skills may be powerful influences on your past, current, and future happiness. 
         HQ factors are CONTROLLABLE factors: you can choose to be happy by improving them. 
         ==> Why are you taking SHAQ?")) (:HELP NIL NIL) NIL :MULTI-ITEM) ("twanttho" "t-Want thorough assessment" "spss-match" NIL ("Want a thorough assessment and/or my Happiness Quotient (HQ) Score.") (:HELP NIL NIL) :MULTI-ITEM (:XDATA :SCALES (HQ)))   ETC ETC
|#
;; (get-keyvalue-in-nested-list  '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx))  )  
;; works= 0.95    (:KEY4 0.95)   (:KEY4 0.95)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))    T
;;
;;return-list-p
;;  (get-keyvalue-in-nested-list  '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 rest of list) :key5 (xxx))  :return-list-p T )
;; works= 0.95    (:KEY4 0.95 REST OF LIST)    (:KEY4 0.95)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 REST OF LIST) :KEY5 (XXX))   T
;;USE OF T AND LISTS INSIDE LIST WITH NO KEY
;;  (progn (setf out nil) (GET-KEYVALUE-IN-NESTED-LIST  (list (list T  0) (list 'motherq  0)) '(PCE-PEOPLE (PCE-PEOPLE-INSTR (People Important To You *INSTR-NAME-ELEMENT)) (MOTHERQ (Your Mother [or person most like a mother to you]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ (Your Father [or person most like a father to you]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ (A best male friend) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-F-FRIENDQ (A best female friend) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) ) :return-list-p T))
;;works= 
#|(MOTHERQ (YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)
(YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU])
(MOTHERQ (YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU]))
((MOTHERQ (YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (PCE-PEOPLE (PCE-PEOPLE-INSTR (PEOPLE IMPORTANT TO YOU *INSTR-NAME-ELEMENT)) (MOTHERQ (YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ (YOUR FATHER [OR PERSON MOST LIKE A FATHER TO YOU]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ (A BEST MALE FRIEND) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-F-FRIENDQ (A BEST FEMALE FRIEND) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)))
T|#

  


;;GET-LISTS-W-KEYVALUE
;;2019
;;ddd
(defun get-lists-w-keyvalue (key value nested-lists &key (test 'equal) (chk-main-list-p t))
  "U-lists   RETURNS (values found-keyval-lists found-values)    INPUT: keylists. For nested lists. or non-nested lists. Finds every instance of key in nested lists.  If value= :get, returns ALL lists with key. Otherwise only returns those whose value matches value.  CHK-MAIN-LIST-P returns main list if it has key and? value."
  (let
      ((found-keyval-lists)
       (main-return-value)
       (found-values)
       )  
    ;;CHECK MAIN LIST?
    (when (and chk-main-list-p
               (setf main-return-value (get-key-value key nested-lists :test test)))
      (when (or (equal value :get)
                (funcall test value main-return-value))
        (setf found-values (append found-values (list main-return-value))
               found-keyval-lists (list nested-lists))))
    ;;CHECK NESTED LISTS
    (loop
     for list in nested-lists
     do
     (let*
         ((return-value  (get-keyvalue-in-nested-list (list (list key T)) list))
          )
       ;;PROBLEM HERE RETURN-VALUE = NIL
       (cond
        ((equal value :get)
         (setf found-keyval-lists (append found-keyval-lists (list list))
               found-values (append found-values (list return-value))))
        ((funcall test value return-value)
         (setf found-keyval-lists (append found-keyval-lists (list list))
               found-values (append found-values (list return-value)))))
     ;;end let,loop
     ))
    (values found-keyval-lists found-values)
    ;;end let, get-lists-w-keyvalue
    ))
;;TEST
;; keys in sublists
;; (get-lists-w-keyvalue :k2  'C  '((this (a (1 2 :k2 C))) (x y :k2 B 3)(3 4 :k2 c)))
;; works= ((THIS (A (1 2 :K2 C))) (3 4 :K2 C))  (C C)
;; key in main list
;; (get-lists-w-keyvalue :lntp :isa   '((POLE1 CAREFOROTHERS :KEY22 DATA22) :LNTP :ISA :KEY1 DATA1))
;; works= (((POLE1 CAREFOROTHERS :KEY22 DATA22) :LNTP :ISA :KEY1 DATA1))   (:ISA)
;; Using :get to get lists with same key and any value
;; (get-lists-w-keyvalue :k2  :get '((this (a (1 2 :k2 C))) (x y :k2 B 3)(3 4 :k2 c)))
;; works= ((THIS (A (1 2 :K2 C))) (X Y :K2 B 3) (3 4 :K2 C))   (C B C)

    
;;GET-SET-APPEND-DELETE-KEYVALUE-IN-NESTED-LIST--MAIN FUNCTION
;;2017 version
;;  
#|;;LOGIC FOR get-set-append-delete-keyvalue-in-nested-list
;; NESTED-LISTS MUST BE A LIST
;;1. IF LAST-KEY-FOUND-P
   USE GET-ADD AND PASS FINAL LISTS UP THRU RECURSE
;;2. IF  NULL  NESTED-LISTS, NIL 
;;3. IF NULL KEY-SPEC-LISTS  & KEY-NOT-FOUND-P
            RECURSE PASS UP NESTED-LISTS TO ADD
;;4. LOOP THRU  NESTED-LISTS
;;      FOR ITEM IN NESTED-LISTS, FOR EACH ITEM
     4,1 IF LISTP ITEM: 
;;         4.1.1 IF NUMBERP KEYLOC-N, use NTH on nested-lists 
;;                  4.1.1.1 IF KEY FOUND, (setf new-key-spec-lists (cdr key-spec-lists))
                          4.1.1.1.1 If last-key-found-p, GO TO build final lists (get-set..)
                          4.1.1.1.2 If not last key, Use new-key-spec-lists
                          RECURSE on that list and put head and tail aside for appending later.
                    4.1.1.2 IF KEY NOT FOUND
                                     Add list to return-nested lists, do not recurse.
          4,1,2 IF NOT NUMBERP, loop thru every item on nested-lists
                   LOOP,  FOR EACH ITEM
                  4.2.1 IF ITEM NOT LIST
                        4.2.1.1 ITEM = KEY (then find value and in betw items)
                                (setf new-key-spec-lists (cdr key-spec-lists))
                               4.2.1.1.1 If last-key-found-p, GO TO build final lists (get-set..)
                               4.2.1.1.2 key-spec-lists, RECURSE ON THIS NESTED-LISTS
                                      USING new-key-spec-lists.
                        4.2.1.2 ITEM NOT= KEY,
                                      Add to a return sublist?
                  4.2.2 ITEM IS A LIST, 
                         RECURSE on that ITEM (a list)
                                    and put head and tail aside for appending later.
          5.  AT BOTTOM If  NULLlast-key-found-p ever found , 
                    and ITEM NOT LIST, ADD TO FINAL LISTS.
;;end get-set-append-delete-keyvalue-in-nested-list LOGIC|# 






;;GET-MULTI-KEYVALUES-IN-NESTED-LISTS
;;2017, 2019 added add-flat-keylist-p, if-val-nil; added found-lists
;;ddd
(defun get-multi-keyvalues-in-nested-lists (keys nested-list 
                                                 &key add-flat-keylist-p (if-val-nil 'omit)
                                                 (return-found-lists-p T))
  "In U-lists RETURNS (values  keyvalue-lists  n-found-keys rest-keys flat-keylist found-lists).  Works in multi-level lists, with key anyplace in list.  Value must be next to key. EFFICIENT way to get values for several keys at once in a complex list structure. If ADD-FLAT-KEYLIST-P, adds a flat key-value list. 
   IF-VAL-NIL use 'OMIT, NIL, or any value."
  (let
      ((key)
       (value)
       (keyvalue-list)
       (keyvalue-lists)
       (n-found-keys 0)
       (n-nested-list (list-length nested-list))
       (rest-keys keys)
       (flat-keylist)
       (found-lists)
       )
    (loop
     for item in nested-list
     for n from 0 to n-nested-list
     do
     (cond
      ((listp item)
       (multiple-value-bind (keyvalue-lists1  n-found-keys1 rest-keys1 flat-keylist found-lists1)
           (get-multi-keyvalues-in-nested-lists rest-keys item :return-found-lists-p
                                                return-found-lists-p)
         (setf keyvalue-lists (append keyvalue-lists keyvalue-lists1)
               n-found-keys (+ n-found-keys n-found-keys1)
               rest-keys rest-keys1)  ;;is this right??
         (when (not (empty-list-p found-lists1))
           (setf found-lists (append found-lists (list found-lists1))))
         (when (null rest-keys)
           (return))
         ;;end mvb, listp
         ))
      ((member item rest-keys :test 'my-equal)
       (setf value (nth (+ n 1) nested-list))
       (cond
        ((and (null value) (equal if-val-nil 'omit))
         NIL)
        ;;otherwise use key and value
        (T
         ;;when value=NIL, set it to if-val-nil value and proceed
         (when (and (null value) (not (equal if-val-nil 'omit)))
           (setf value if-val-nil))
         (incf n-found-keys)
         (setf key item
               rest-keys (delete key rest-keys :test 'my-equal)
               keyvalue-list (list key value)
               ;;added 2019               
               keyvalue-lists (append keyvalue-lists (list keyvalue-list)))
         (when (not (empty-list-p nested-list))
           (setf found-lists (append found-lists (list nested-list))))
         (when (null rest-keys)
           (return))))
       ;;end member
       )
      (t nil))
     ;;end loop
     )
    (when add-flat-keylist-p
      (setf flat-keylist (flatten-list-tree keyvalue-lists)))
    (values  keyvalue-lists  n-found-keys rest-keys flat-keylist found-lists)
    ;;end let, get-multi-keyvalues-in-nested-lists
    ))
;;TEST
;; NON-NESTED LIST
;; also :add-flat-keylist-p = T
;; (get-multi-keyvalues-in-nested-lists '(key1 key3) '(a b key1 val1 c d key2 val2 e f g key3 val3  l m key4 val4 x y z) :add-flat-keylist-p T)
;; works= ((KEY1 VAL1) (KEY3 VAL3))  2  NIL  (KEY1 VAL1 KEY3 VAL3)
;; added found-lists= ((A B KEY1 VAL1 C D KEY2 VAL2 E F G KEY3 VAL3 L M KEY4 VAL4 X Y Z) (A B KEY1 VAL1 C D KEY2 VAL2 E F G KEY3 VAL3 L M KEY4 VAL4 X Y Z))
;; (get-multi-keyvalues-in-nested-lists '(key1 key3) '(a b key1 val1 c d key2 val2 e f g key3 val3  l m key4 val4 x y z))
;; works= ((KEY1 VAL1) (KEY3 VAL3))  2  NIL
;; nested-list
;;  (get-multi-keyvalues-in-nested-lists '(key1 key3) '(a b key1 val1 c d key2 val2 e f g (1 2 key3 val3 3 4) l m key4 val4 x y z))
;; works= ((KEY1 VAL1) (KEY3 VAL3))   2 NIL
;; 2-nested list w/ :add-flat-keylist-p
;; (get-multi-keyvalues-in-nested-lists '(key1 key2 key3) '(a b key1 val1 ( c d key2 val2 e f g (1 2 key3 val3 3 4) l) m key4 val4 x y z) :add-flat-keylist-p T)
;; works= ((KEY1 VAL1) (KEY2 VAL2) (KEY3 VAL3))   3   NIL  (KEY1 VAL1 KEY2 VAL2 KEY3 VAL3)
;;USE OF IF-VAL-NIL  if-val-nil= 'omit
;; (get-multi-keyvalues-in-nested-lists '(key1 key2 key3) '(a b key1 NIL ( c d key2 val2 e f g (1 2 key3 val3 3 4) l) m key4 val4 x y z) :add-flat-keylist-p T)
;; works, omits key1 and its nil value (by default if-val-nil= 'omit
;; if-val-nil = NIL
;; (get-multi-keyvalues-in-nested-lists '(key1 key2 key3) '(a b key1 NIL ( c d key2 val2 e f g (1 2 key3 val3 3 4) l) m key4 val4 x y z) :add-flat-keylist-p T :if-val-nil NIL)
;; works ((KEY1 NIL) (KEY2 VAL2) (KEY3 VAL3))   3  NIL  (KEY1 KEY2 VAL2 KEY3 VAL3)
;; if-val-nil = 'THIS
;;(get-multi-keyvalues-in-nested-lists '(key1 key2 key3) '(a b key1 NIL ( c d key2 val2 e f g (1 2 key3 val3 3 4) l) m key4 val4 x y z) :add-flat-keylist-p T :if-val-nil 'THIS)
;; works= ((KEY1 THIS) (KEY2 VAL2) (KEY3 VAL3))  3   NIL  (KEY1 THIS KEY2 VAL2 KEY3 VAL3)
;;NAME
;;2019
;;ddd



;;EMPTY-LIST-P
;;2019
;;ddd
(defun empty-list-p (list )
  "U-lists   RETURNS T or NIL "
  (not (find-if-not #'null list))
  ;;end let, empty-list-p
  )
;;TEST
;; (empty-list-p nil) = T
;; (empty-list-p '(nil nil b)) = NIL
;; (empty-list-p '(nil nil NIL)) = T
;; (empty-list-p '()) = T

;; (find-if-not #'null nil)




;;
;;GET-SET-APPEND-DELETE-KEYVALUE-IN-NESTED-LIST
;;--MAIN FUNCTION
;;2020 modified to add :DELETE-LIST
;; NOTE: FOR LOGIC/STEPS/DOCS for this funct to to this file end in help
;;ddd
(defun get-set-append-delete-keyvalue-in-nested-list (new-value  key-spec-lists  nested-lists                                                           &key use-simple-key-p append-value-p 
                                                                 list-first-item-in-append-p
                                                                 add-value-p
                                                                 ;;added 2020
                                                                 simple-splice-key-value-in-keylist-p
                                                                 splice-newvalue-at-nth-p 
                                                                 ;;added 2018-01
                                                                 KEY-IN-PREV-KEYLIST-P
                                                                 (test 'my-equal)
                                                                 return-list-p 
                                                                 key=list-p
                                                                 put-key-after-items
                                                                 put-value-after-items
                                                                 new-begin-items  new-end-items
                                                                 splice-key-value-in-list-p 
                                                                 splice-old-new-values-p
                                                                 parens-after-begin-items-p
                                                                 (return-nested-list-p T) ;;was return-orig-nested-list-p
                                                                 ;;added 2019-07
                                                                 (simple-keylist-ok T)
                                                                 (max-list-length 1000)  
                                                                 (if-not-found-append-key-value-p T)
                                                                 ;;splice takes presidence
                                                                 if-not-found-splice-keyvalue-list-p 
                                                                 if-not-found-append-keyvalue-list-p 
                                                                 ;;doesn't work well unless finds lower keys
                                                                 if-not-found-add-item-in-last-nested-list-p 
                                                                 orig-list-items-leftn
                                                                 (recurse-for-key-p T)
                                                                 (bottom-level-p T)
                                                                 (if-get-no-return-old-keylist-p T)
                                                                 last-key-found-p oldkey
                                                                 ;;added 2019-10
                                                                 not-duplicate-p
                                                                 )
  "In U-lists. RETURNS: (values return-keylist return-nested-lists new-keylist return-value return-old-keylist last-key-found-p old-value new-key-spec-lists old-key&value)
  New KEY-SPEC-LISTS can be a simple KEY or LIST OF KEYS (from outer to inner lists) eg. (:k1 key 6) and searches as if each is a (key T) sublist. If use-simple-key-p, key an be a list. Use :GET :SET-KEYLIST=NIL :DELETE-VALUE :DELETE-KEY&VALUE :DELETE-LIST keyword for new-value for those options.
  KEYSPEC= (key keyloc-n val-nth).  FOR KEY = :NTH (to find nth in list without key) :  (1) KEYLOC-N NOT USED--searches entire list at that level  for key (as before if keyloc-n = T) [In which case will check entire list at that level for key].  Use depreciated old function for that. Note: to search ALL NESTED LISTS for key at Nth position (including DIMLIST KEYS,  use eg '(((CS HS 2 1) 0 0))
   (2) Each new level does NOT need a key in the key-spec-lists (it can be a list with lists containing keys; and (3) VALUE MUST either be an item next to key (val-nth = 1) OR val-nth [the optional 3rd item in the level spec-list].  If :NR member spec-list, sets RECURSE-FOR-KEY-P to NIL If :R member, sets it to T, then key MUST be IN LIST on FIRST LEVEL of current recursion (doesn't recurse on inner lists for that spec-list). 
   If in  NOT LAST KEYSPEC VAL-NTH is a NUMBER  searches that location [can be VALUE LIST] for the nested list with next key.  IF VAL-NTH not a number (eg aT), searches ALL NESTED LISTS for the NEXT KEY.
   To APPEND KEY & VALUE TO TOP LIST, set :IF-NOT-FOUND-APPEND-KEYVALUE-LIST-P to NIL. (To append whole list, set to T)
    RETURNS (values return-keylist return-nested-lists new-keylist return-value    
              return-old-keylist last-key-found-p old-value new-key-spec-lists)
     If RETURN-LIST-P, return-keylist is the ENTIRE list containing key, otherwise same as new-keylist. 
     KEY-SPEC-LISTS are lists of (key keyloc-n val-n) [val-nth optional, default= 0) from outermost to innermost (last) key. Eg of proper list (:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key5 OLD-VALUE)...)); key-spec-list= ((:key1 2)(:key2  2)(:key3 4)(:key5 0)) .
     KEY-IN-PREV-KEYLIST-P Adds or appends (depending on &key above) the last key and new-value (if not found in the previous flat keylist) to that previous keylist). If found, acts normally. PREFERRED if last key is same flat keylist as previous key.
     IF-NOT-FOUND-APPEND-KEY-VALUE-P adds a key and keyvalue to innermost list if not found (but preceding key must be found). If last keyloc-n = 0, puts old previous keyvalue at end of new keylist. If last keyloc-n > 0, puts it first, then fills in nils before new key in last list which is new value of previous key. 
     IF KEY = :NTH, then gets, sets, or appends number in KEYLOC-N PLACE. Note: If second item in spec-list isn't number, uses default keyloc-n.  
     PUT-KEY-AFTER-ITEMS is a list of items which is used if keyloc-n > 0 to fill in items between key and value [done automatically if old items are there]. splice-key-value-in-list-p does similar except includes items after the value. 
    If :SIMPLE-SPLICE-KEY-VALUE-IN-KEYLIST-P (use MOST), If value listp,  appends list contents, if value not list, appends value.
    If SPLICE-OLD-NEW-VALUES-P, puts or splices (list key value) into put or splice list. Otherwise puts or splices key value [not a list]. COMPATIBIILITY PROBLEMS: get-key-value-in-nested-lists, val-nth starts with 0 there, with 1 here. APPEND-VALUE-P puts old-value & new-value in a list.   
   ADD-VALUE-P just adds new-value after old-value.   Only with add-value-p, SPLICE-NEWVALUE-AT-NTH-P, Also use :NTH and :KEYLOC-N for location (:NTH 99) = eg (:k1 1 :k2 2 :newkey newval) instead of (:k1 1 :k2 2  (:newkey newval))
   LAST-KEY-FOUND-P must be NIL (set only in recursive calls), or keeps from adding new values sometimes.          
   NOTE: IF KEYS NOT FOUND, Can NOT reliably put new key and value in an innermost list IF LAST KEY NOT FOUND, because without a rigid orderly key system, could end up putting it inside the last of any or multiple nested lists. Therefore putting it in lowest level list.  Use the get-set-append-delete-keyvalue-in-orderly-nested-list function to put it INSIDE an inner nested list.
   return-nested-list-p often makes no difference.
   LIST-FIRST-ITEM-IN-APPEND-P to set keylist to key (newitem) not key newitem.
   * This version DOES NOT USE THE KEYLOC-N to search lists
  * THIS CAN BE USED AS A GENERAL SEARCH FUNCTION"

  ;;Added 2019-07
  (when (or (symbolp key-spec-lists) (stringp key-spec-lists)
            (and simple-keylist-ok (not (listp (car key-spec-lists)))))
    (setf key-spec-lists  (make-get-set-delete-keylist-from-keys key-spec-lists)))
    ;;note:  simple-keylist-ok = NIL in recursive calls, so this new key-spec-lists will be used
  (let*
      ((return-nested-lists)
       (return-old-keylist)
       (new-return-nested-lists)
       (match-item)
       (spec-list  (car key-spec-lists))
       (KEY  (cond ((listp spec-list)(first spec-list))
                   (t key-spec-lists)))
       (keyloc-n (cond ((listp spec-list) (second spec-list))
                       (t T)))
       (val-nth )
       (new-nested-lists)
       (new-key-spec-lists)
       (add-new-value-p)
       (cur-level-list)
       (list-head)
       (list-tail)
       (length-nnl) 
       (old-value)
       (key-found-p)
       (last-key-found-p1)
       (added-key-value)
       (return-keylist)(new-keylist)(old-keylist)( return-value)
       (testitem)
       (item)
       (new-key)
       (found-keylist)
       (found-top-n)
       (oldkeylocn)(oldvalnth)
       (old-key&value) ;;2020
       (old-value1) ;;for return of get-set- ;not sure needed, check?
       ;;(keyloc-n2)(val-nth2)
       )
    (when (listp key)
      (setf key=list-p key))
    ;;new


    ;;2018
    ;;to make sure entire list is searched for keys in last nested list
    (when (and key-in-prev-keylist-p
               (= (list-length key-spec-lists) 1))
      (setf keyloc-n T))
    
    ;;LISTP NESTED-LISTS--Function only processes list inputs.
    (cond           
     ((listp nested-lists)
      (setf length-nnl (list-length nested-lists))

      ;;SPEC-LIST VARIABLES ----------------------------
      (cond
       ;;KEY = T  (for compatibity with older and is useful)
       ((or  (equal key 'T)(equal key T))
        (setf  ;;no? key-spec-lists  (cdr key-spec-lists)
               spec-list (car key-spec-lists)
               key (car spec-list)
               keyloc-n (second spec-list)
               val-nth (third spec-list))
        ;;always recurse-for-key-p T)
        (when (null keyloc-n) 
          (setf keyloc-n 1))
        
        ;;(afout 'out (format nil ">>KEY= T, THEN KEY= ~A,~% spec-list= ~A keyloc-n= ~A  KEY-SPEC-LISTS= ~A~% recurse-for-key-p= ~A"key  spec-list keyloc-n key-spec-lists     recurse-for-key-p))
        )
       ;;FOR KEY = :NTH (to find nth in list without key)
       ((equal key :NTH)
        (setf val-nth 0))
       (t nil))
 
      ;;KEYLOC-N (position of the key in nested-lists)
      (when (not (numberp keyloc-n))
        (setf  recurse-for-key-p T))
      ;;VAL-NTH, place for value after key
      (when  (third spec-list)
        (setf val-nth (third spec-list)))
      (when (null val-nth)
        (setf val-nth 1))   
      ;;(afout 'out (format nil ">>>>>>>> 1-NESTED-LISTS= ~A~%SPEC-LIST= ~A~%KEYLOC-N= ~a  VAL-NTH= ~a~% recurse-for-key-p= ~A"  nested-lists spec-list keyloc-n  val-nth    recurse-for-key-p))

      ;;TOPLEVEL LOOP  AAA
      (loop
       for topitem in nested-lists
       for top-n from 0 to length-nnl
       do
       (let
           ((key-spec-list)
            )
       ;;(afout 'out  (format nil "***** TOPLEVEL LOOP, TOPITEM= ~A~% TOP-N=~a~%SPEC-LIST= ~A" TOPITEM TOP-N spec-list))

       ;;TOP LEVEL COND: Is item a list or not?
       (cond
        ;;TOPITEM IS A LIST (but key is not), RECURSE
        ((and (listp topitem) 
              (null (and key=list-p
                         (member  key=list-p nested-lists :test test))))

         ;;Set head and tail before recurse to append after recurse
         (setf list-head (butlast nested-lists (- length-nnl top-n)) ;;lll
               list-tail (nthcdr (+ top-n 1) nested-lists))
         (setf old-keylist topitem) ;;added is this right?
         #|     What is this for?    
           (cond
            ((equal key :any-key)
             (setf key-spec-lists1 (cdr key-spec-lists)))
            (t (setf key-spec-lists1 key-spec-lists)))|#
         (multiple-value-setq (return-keylist new-return-nested-lists
                                              new-keylist return-value    
              return-old-keylist last-key-found-p1 old-value new-key-spec-lists 
              old-key&value) ;;old-key&value 2020
             (get-set-append-delete-keyvalue-in-nested-list new-value  key-spec-lists  
                                                            TOPITEM 
                                                            :return-list-p return-list-p :test test
                                                            :append-value-p append-value-p
                                                            :add-value-p add-value-p
                                                            :simple-splice-key-value-in-keylist-p
                                                                       simple-splice-key-value-in-keylist-p
                                                            :splice-newvalue-at-nth-p splice-newvalue-at-nth-p
                                                            :new-begin-items new-begin-items 
                                                            :new-end-items new-end-items
                                                            :max-list-length max-list-length
                                                            :last-key-found-p   last-key-found-p
                                                            ;;      :new-keylist new-keylist
                                                            ;;     :old-keylist old-keylist
                                                            ;;      :return-value return-value
                                                            :key-in-prev-keylist-p 
                                                            key-in-prev-keylist-p
                                                            :if-not-found-append-key-value-p NIL
                                                            :RECURSE-FOR-KEY-P  recurse-for-key-p
                                                            :bottom-level-p nil
                                                            ;; :put-key-after-items put-key-after-items
                                                            ;; :splice-key-value-in-list-p splice-key-value-in-list-p
                                                            ;; :put-value-after-items put-value-after-items
                                                            :splice-old-new-values-p splice-old-new-values-p
                                                            :parens-after-begin-items-p parens-after-begin-items-p
                                                            :list-first-item-in-append-p list-first-item-in-append-p
                                                            ;;  :return-keylist return-keylist
                                                            :oldkey oldkey
                                                            :not-duplicate-p not-duplicate-p
                                                            ))
         (setf  return-nested-lists (append list-head 
                                            put-key-after-items  ;; (list key)  ;;list key added
                                            (list  new-return-nested-lists)   list-tail))
         ;;(break "after (setf  return-nested-lists")
         ;;(my-equal-path
         ;;(afout 'out (format nil "IN TOP NUMBERP LOOP, AFTER RECURSE, ITEM=new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a" TOPITEM  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items))
         
         ;;last-key-found-p1 from recurse, If T, RETURN
         (when last-key-found-p1
           (setf last-key-found-p T)
           (return))
         ;;end listp topitem, not numberp keyloc-n,  listp item, recurse-for-key-p
         ;;)
         ;;END (LISTP TOPITEM)
         )
        ;;TOPITEM NOT A LIST or BOTH TOPITEM AND KEY ARE LISTS
        ((or (null (listp topitem))
             (and key=list-p (listp topitem)))

         (cond
          ;;TOPITEM = KEY ( or NTH if :NTH)
          ((or (FUNCALL TEST  TOPITEM KEY) ;;was (my-equal topitem key)
               (and (equal key :nth) (equal top-n keyloc-n))
               (and KEY-IN-PREV-KEYLIST-P
                    (= (list-length key-spec-lists) 2) 
                    ;;first key is in the list
                    (find-item-n key  nested-lists)))

           ;;(BREAK "TOPITEM FOUND")
           ;;(afout 'out (format nil ">>>> 1. TOPITEM NOT A LIST, KEY= ~a  FOUND~% NESTED-LISTS=old-keylist= ~A, ~%FOR OLD: SPEC-LIST= ~a, TOP-N= ~a  " key nested-lists spec-list top-n ))
           (setf key-found-p T              
                 new-key-spec-lists (cdr key-spec-lists))
           (when (not (numberp keyloc-n))
             (setf keyloc-n top-n)) 
           ;;NOT FOR THIS VERSION, THE NEXT KEY DOEN'T NEED TO BE IN THE VALUE-LIST
           ;;THE SPEC-LIST IS NEW, SO RE-SEARCH ENTIRE LIST, COULD BE BEFORE IT EVEN
           ;;(BREAK "TOPITEM NOT LIST, FOR NEW RECURSE:")

           (cond
            ;;LAST-KEY-FOUND-P OR NOT FOUND INSIDE CURRENT LIST
            ((OR (null new-key-spec-lists)
                 ;;KEY-IN-PREV-KEYLIST-P and NOT in old nested-lists (w last found key)
                 ;;(break "key")
                 ;;KEY-IN-PREV-KEYLIST-P conds must be BOTH here and above
                 (and KEY-IN-PREV-KEYLIST-P
                      (= (list-length new-key-spec-lists) 1) 
                      ;;first key is in the list
                      (find-item-n key  nested-lists)))  
             ;;(BREAK "LAST KEY FOUND")

             (cond
              (key-in-prev-keylist-p
               (setf last-key-found-p T)
               ;;FIRST KEY IN KEYLIST
               (multiple-value-setq (oldkey oldkeylocn oldvalnth)
                   (values-list spec-list))
               (when (null oldvalnth)
                 (setf oldvalnth 1))             
               ;;1 key ago = right one?
               ;;keyloc-n & val-nth set above for oldkey?
               ;;(setf oldkey (car old-keylist)) = 2 keys ago

               ;;set new key-spec-list for same keylist
               (setf spec-list (car new-key-spec-lists))                   
               ;;SECOND KEY IN SAME KEYLIST
               (multiple-value-setq (key keyloc-n val-nth) 
                   (values-list spec-list))
               ;;check for second key in same keylist--replace keyloc-n
               (setf keyloc-n (find-item-n  key nested-lists))

               (when (null val-nth)
                 (setf val-nth 1))
                 ;;sets keyloc-n to NIL if not found                   

               (cond
                (keyloc-n
                 (setf 
                       ;;NO old-value (nth (+ keyloc-n2 val-nth2) nested-lists)
                       list-head (butlast nested-lists (- length-nnl  (+ keyloc-n val-nth)))
                       list-tail (nthcdr (+ keyloc-n val-nth 1) nested-lists)
                       old-value (nth (+ keyloc-n val-nth) nested-lists)
                       old-keylist nested-lists))
                (t 
                 (setf 
                       ;;NO old-value (nth (+ keyloc-n2 val-nth2) nested-lists)
                       list-head  nested-lists
                       list-tail (list key)
                       old-keylist nested-lists)))

              ;;(break "after mvs (key keyloc-n val-nth)")
               ;;END KEY-IN-PREV-KEYLIST-P
               )
              ;; (find-item-n :csval '("KIND" "kind vs ukind" CS2-1-1-99 NIL NIL :PC ("kind" "ukind" 1 NIL) :POLE1 "kind" :POLE2 "ukind" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL)) :CSVAL "0.917"))

              ;;IF NOT KEY-IN-PREV-KEYLIST-P (original code)
              (T
               ;;GET-SET
               (setf last-key-found-p T
                     old-value (nth (+ keyloc-n val-nth) nested-lists)
                     list-head (butlast nested-lists (- length-nnl (+ top-n val-nth)))
                     list-tail (nthcdr (+ top-n val-nth 1) nested-lists)
                     old-keylist nested-lists)))

             ;;HERE NOW 2
             ;;(afout 'out (format nil ">>>> 2. LAST-KEY-FOUND. OLD-VALUE= ~A~%list-head= ~A~%list-tail= ~A" old-value list-head list-tail))    
             ;;(BREAK "LAST KEY FOUND--BEFORE GET-SET-OR-APPEND")

             ;;LAST KEY FOUND, SO USE get-set-append-delete-keyvalue
             ;;
             ;;USE GET-SET-APPEND-DELETE-KEYVALUE 
             (multiple-value-setq (return-keylist new-keylist return-value
                                                  old-value1 old-key&value)
                 ;;NOS old-value) ;;2020 old-value1 old-key&value added above
                 (get-set-append-delete-keyvalue key  new-value
                                                 :keyloc-n keyloc-n :val-nth val-nth :test test
                                                 :old-keylist old-keylist
                                                 :append-value-p append-value-p
                                                 :list-first-item-in-append-p list-first-item-in-append-p
                                                 :add-value-p add-value-p
                                                 :simple-splice-key-value-in-keylist-p
                                                                       simple-splice-key-value-in-keylist-p
                                                 :splice-newvalue-at-nth-p splice-newvalue-at-nth-p
                                                 :put-key-after-items put-key-after-items
                                                 :put-value-after-items put-value-after-items
                                                 :new-begin-items new-begin-items
                                                 :new-end-items new-end-items
                                                 :splice-key-value-in-list-p splice-key-value-in-list-p
                                                 :splice-old-new-values-p splice-old-new-values-p                 
                                                 :parens-after-begin-items-p parens-after-begin-items-p
                                                 :not-duplicate-p not-duplicate-p
                                                 ))
             ;; (get-set-append-delete-keyvalue :csval "newval" :old-keylist '("KIND" "kind vs ukind" :BESTPOLE 1 (POLE1 X X) :CSVAL "0.917") :keyloc-n 7 :val-nth 1)
             ;;(break "after get-set-append-delete-keyvalue")
             ;;modified 2017-07
             ;;RETURN-OLD-KEYLIST
             (setf return-old-keylist old-keylist)

             ;;RETURN-KEYLIST, RETURN-VALUE, RETURN-NESTED-LISTS
             (cond
              ;;2020 added simple-splice-key-value-in-keylist-p & :delete-list
              ((equal new-value :delete-list)
               (setf return-keylist :LIST-DELETED
                     return-nested-lists :LIST-DELETED-HERE))
              ((OR simple-splice-key-value-in-keylist-p
                   (member new-value '(:delete :delete-key&value ) :test 'equal))  ;;hereAA
               ;;(break "in simple-splice-key-value-in-keylist-p")
               (setf  return-nested-lists return-keylist)
               )
              ;;to splice new value into list, not add as a list              
              ((and (listp return-value)
                    (or add-value-p splice-key-value-in-list-p))              
                     ;;not add-value-p
               (cond
                (key-in-prev-keylist-p
                 (setf  return-nested-lists (append list-head list-tail
                          return-value)))
                (t
                 (setf  return-nested-lists  (append list-head
                                                     return-value   ;;was return-keylist
                                                     list-tail))))
               )
              ;;TO APPEND AS A LIST eg splice-old-new-values-p
              (t
               (cond
                (key-in-prev-keylist-p
                 (setf  return-nested-lists (append list-head list-tail
                          (list return-value))))
                (t
                 (setf  return-nested-lists  (append list-head
                                                     (list return-value)   ;;was return-keylist
                                                     list-tail))))))

             ;;(BREAK "before return-list-p")
             (when return-list-p
               (setf return-keylist return-nested-lists)) 

             ;;Set return values above
             ;;(values return-keylist return-nested-lists new-keylist return-value    
             ;;      return-old-keylist last-key-found-p old-value new-key-spec-lists )

             ;;(afout 'out (format nil "BEFORE RETURN AFTER GET-SET TOPITEM= ~A NOT A LIST AFTER LAST-KEY-FOUND:~%GET-SET-APPEND; top-n= ~A list-head= ~A~%return-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A OLD-VALUE= ~a" TOPITEM top-n list-head return-keylist list-tail return-nested-lists old-value))

             ;;RETURN
             (RETURN)
             ;;end when (or (null new-key-spec-lists) and clause (or (null new-key-spec-lists)
             )
            ;;LAST KEY NOT FOUND, RECURSE ON NEW-NESTED-LISTS
            (t
             ;;(BREAK "KEY-FOUND, RECURSE, TOPITEM NOT A LIST")
             ;;Arguments set above after key found

             ;;SET VALUES FOR RECURSE
             (setf new-nested-lists nested-lists
                   old-keylist  nested-lists 
                   key-spec-list (car key-spec-lists))
             (multiple-value-setq (key keyloc-n val-nth)
                 (values-list key-spec-list))

             ;;end COND
             ))
           ;;If NOT (NULL KEY-SPEC-LISTS)
           ;;RECURSE W NEW-KEY-SPEC-LISTS -- USE CURRENT NESTED-LIST
           (cond
            ((and (listp new-nested-lists) ;;same as nested-lists set above
                  ;;added for KEY-IN-PREV-KEYLIST-P condition
                  ;;don't recurse if final key in next to last keylist
                  (or (null KEY-IN-PREV-KEYLIST-P)
                      (not (= (list-length new-key-spec-lists) 1))
                      ;;first key is in the list
                      (null (find-item-n key  nested-lists))))
             (multiple-value-setq (return-keylist new-return-nested-lists new-keylist 
                                                  return-value  return-old-keylist last-key-found-p1
                                                  old-value new-key-spec-lists old-key&value)
                 (get-set-append-delete-keyvalue-in-nested-list new-value  
                                                                NEW-KEY-SPEC-LISTS  
                                                                NEW-NESTED-LISTS ;;same as nested-lists
                                                                :return-list-p return-list-p :test test
                                                                :append-value-p append-value-p
                                                                :add-value-p add-value-p
                                                                :simple-splice-key-value-in-keylist-p
                                                                simple-splice-key-value-in-keylist-p
                                                                :splice-newvalue-at-nth-p 
                                                                splice-newvalue-at-nth-p
                                                                :new-begin-items new-begin-items
                                                                :new-end-items new-end-items
                                                                :max-list-length max-list-length
                                                                :last-key-found-p   last-key-found-p
                                                                ;;      :new-keylist new-keylist
                                                                ;;     :old-keylist old-keylist
                                                                ;;      :return-value return-value
                                                                :KEY-IN-PREV-KEYLIST-P 
                                                                KEY-IN-PREV-KEYLIST-P
                                                                :if-not-found-append-key-value-p NIL
                                                                :bottom-level-p nil
                                                                :list-first-item-in-append-p 
                                                                list-first-item-in-append-p
                                                                ;; :put-key-after-items put-key-after-items
                                                                ;; :splice-key-value-in-list-p splice-key-value-in-list-p
                                                                ;; :splice-old-new-values-p splice-old-new-values-p
                                                                :splice-old-new-values-p splice-old-new-values-p
                                                                :parens-after-begin-items-p parens-after-begin-items-p
                                                                ;;  :return-keylist return-keylist
                                                                :oldkey oldkey
                                                                ))
             ;;caused error sometimes (setf return-old-keylist old-keylist)
             (when last-key-found-p1
               (setf last-key-found-p T))
                          ;;(afout 'out (format nil "IN LOOP, TOPITEM=KEY (NOT A LIST),, AFTER RECURSE, TOPITEM=new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a~%old-keylist= ~A last-key-found-p= ~A" TOPITEM  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items old-keylist last-key-found-p))
             )
            ;;MUST BE ABLE TO CONTINUE PROCESSING
            (t 
             ;;RECURSE ON CDR AFTER LAST FOUND KEY  OR WHOLE LIST HERE??
             (BREAK "SSS FIX?? ERROR: NEW-NESTED-LISTS= [~A  ]  IS NOT A LIST [May have wrong keyloc-n]" new-nested-lists)
             ))
           (cond
            ((numberp keyloc-n)
             (setf  return-nested-lists (append list-head 
                                                put-key-after-items  ;; (list key)  ;;list key added
                                                (list  new-return-nested-lists)   list-tail)))
            ;;If keyloc-n = T, return whole nested list
            (t (setf return-nested-lists new-return-nested-lists)))                         

           ;;(BREAK "IN LOOP, TOPITEM=KEY (NOT A LIST), RETURN? if  last-key-found-p")
           ;;test this
           (when (or last-key-found-p
                     (null (cdr new-key-spec-lists))
                     (and  key-in-prev-keylist-p
                           (null (cddr new-key-spec-lists))))
             ;;(break "test")
             (return))              
           ;;END KEY FOUND
           )

          ;;KEY NOT FOUND  AS ITEM IN TOPLIST
          ((null last-key-found-p)
           (setf return-nested-lists (append return-nested-lists (list topitem)))
           ;;(afout 'out (format nil "AT LOOP END, NO KEY FOUND as item in toplist, top-n= ~A length-nnl= ~A topitem= ~A" top-n length-nnl topitem))
           ))

         ;;END TOPITEM NOT A LIST, COND
         )  )

       ;;END TOPLEVEL LOOP
       ))
      ;;END LISTP NESTED-LISTS
      )
     ;;NESTED-LISTS NOT A LIST
     (T ;;was (break "ERROR: NESTED-LISTS IS NOT A LIST")))
        ;;TRY RECURSING ON THE CDR OF LIST
        ;;MODIFY RETURN VALS??
        (when  (funcall test key nested-lists)
          (setf new-key-spec-lists (cdr key-spec-lists)
                return-value nested-lists
                return-keylist nested-lists))
        ;;here
        (when (null new-key-spec-lists)
          (setf return-old-keylist old-keylist)
          (setf last-key-found-p T)
          ;;(afor 'out "NESTED-LISTS NOT A LIST;; KEY= ~A EQUALS NESTED-LISTS= ~A" key nested-lists)
          ;;(break "after nested-list= key")
          ;;end when
          )

        ;;END NESTED-LIST NOT A LIST
        )  )

    ;;IF-NOT-FOUND-APPEND-KEY-VALUE-P
    ;;NOTE: Can NOT reliably put new key and value in an innermost list IF LAST KEY NOT FOUND, because without a rigid orderly key system, could end up putting it inside the last of any or multiple nested lists. Therefore putting it in lowest level list.  Use the get-set-append-delete-keyvalue-in-orderly-nested-list function to put it INSIDE an inner nested list.
    (when (and bottom-level-p
               (or if-not-found-append-key-value-p
                   if-not-found-splice-keyvalue-list-p
                   if-not-found-append-keyvalue-list-p)  
               (null if-not-found-add-item-in-last-nested-list-p)
               (null last-key-found-p) (null new-keylist) 
               (not (member new-value
                            '(:get :set-keylist=nil :delete-value :delete-key&value
                              :delete-list))))
      (setf  added-key-value T
             key (caar (last key-spec-lists))
             new-keylist (list key new-value)
             return-keylist new-keylist
             return-value new-value)
      (cond
       (if-not-found-splice-keyvalue-list-p
        (break "if-not-found-splice-keyvalue-list-p")
        (cond  ;;added cond 2020
               ((and (listp new-keylist)(listp (car new-keylist)))
                (setf return-nested-lists (append nested-lists (car new-keylist))))
               (t (setf return-nested-lists (append nested-lists new-keylist)))))
       (if-not-found-append-keyvalue-list-p
        (setf return-nested-lists (append nested-lists 
                                          (list new-keylist))))
       (t (setf return-nested-lists (append nested-lists 
                                            new-keylist))))
      
      ;;(break "new-value if not found")      
      ;;(afout 'out (format nil "KEY NEVER FOUND, return-nested-lists= ~A" return-nested-lists))
      ;;end if-not-found-append-key-value-p clause
      )

    (when (and bottom-level-p (null return-nested-list-p)) 
      (setf return-nested-lists  (list "return-nested-list-p=NIL")))  
    ;;(afout 'out (format nil "END RETURN-VALUES  return-keylist= ~A~% return-nested-lists= ~A~%                new-keylist= ~A~%  return-value= ~A~% return-old-keylist= ~A~%  last-key-found-p new-key-spec-lists= ~A" return-keylist return-nested-lists new-keylist return-value    return-old-keylist last-key-found-p new-key-spec-lists ))

    ;;(BREAK "AT END")

    ;;WHEN :DELETE LIST remove identified list
    (when :DELETE-LIST
      (unless (equal return-nested-lists :list-deleted-here)
        (member :LIST-DELETED-HERE return-nested-lists :test 'equal)
        (setf return-nested-lists 
              (delete-items-from-list '(:list-deleted-here) return-nested-lists))))
    
    (cond
     ;;to reduce extra output for long return-old-keylist
     ((and bottom-level-p  if-get-no-return-old-keylist-p
           (equal new-value :get))
      (values return-keylist return-nested-lists new-keylist 
              return-value 'SEE-RETURN-NESTED-LISTS-FOR-GET   
              last-key-found-p old-value new-key-spec-lists old-key&value ))
     (T
      #|(when (and KEY-IN-PREV-KEYLIST-P (null return-list-p))
        (setf return-keylist (list (caar (last key-spec-lists)) (second return-keylist))))|# 
      (values return-keylist return-nested-lists new-keylist return-value    
              return-old-keylist last-key-found-p old-value new-key-spec-lists 
              old-key&value)))     ;;2020 old-key&value added
    ;;end let, get-set-append-delete-keyvalue-in-nested-list
    ))
;;NEW TESTS
;; on multi-nested key-specs
;; (get-set-append-delete-keyvalue-IN-NESTED-LIST :GET '((k2 0)(k3 0))  '((k1 a)(k2 c d (k3 oldval 2 3))(k4 5 6)))
;; works= (K3 OLDVAL 2 3)   ((K1 A) ((K2 C D (K3 OLDVAL 2 3))) (K4 5 6))   (K3 OLDVAL 2 3)   OLDVAL
;; (get-set-append-delete-keyvalue-IN-NESTED-LIST :GET '(k2 k3)  '((k1 a)(k2 c d (k3 oldval 2 3))(k4 5 6)) :use-simple-key-p T)
;; works= (K3 OLDVAL 2 3)  ((K1 A) (K2 C D (K3 OLDVAL 2 3)) (K4 5 6))  (K3 OLDVAL 2 3)  OLDVAL
;; (get-set-append-delete-keyvalue-IN-NESTED-LIST :GET '(k1 k3)  '((k1 a b c (k2 1 2 3) d e (k3 oldval  (l)(m)(n) o p) (k5  2 3))(k4 5 6)) :use-simple-key-p T)
;; works= (K3 OLDVAL (L) (M) (N) O P)    ((K1 A B C (K2 1 2 3) D E (K3 OLDVAL (L) (M) (N) O P) (K5 2 3)) (K4 5 6))   (K3 OLDVAL (L) (M) (N) O P)  OLDVAL
;; WORKS 
;;  (nth-value 6  (get-set-append-delete-keyvalue-IN-NESTED-LIST :GET '(("UTYPE" T 1)) *SHAQ-ALL-DATA-LIST :RETURN-NESTED-LIST-P NIL))
;; works= ("UTYPE" :MULTI "utype" "UserType" 1 ("twanttho" "1" 1 T 1 1 (:XDATA :SCALES (HQ))) ("tknowmor" "2" 1 NIL 0 1 (:XDATA :SCALES (HQ))) ("twanthel" "3" 1 NIL 0 1 (:XDATA :SCALES (HQ))) ("twantspe" "4" 1 NIL 0 1 (:XDATA :SCALES NIL)) ("texperie" "5" 1 NIL 0 1 (:XDATA :SCALES NIL)) ("tprevshaq" "6" 1 NIL 0 1 (:XDATA :SCALES (PREVIOUS-USER))) ("wantspq" "7" 1 NIL 0 1 (:XDATA :SCALES (SPECIFIC-QUESTS))) ("tu100stu" "8" 1 NIL 0 1 (:XDATA :SCALES (HQ ACAD-LEARNING))) ("tcsulbst" "9" 1 NIL 0 1 (:XDATA :SCALES (ACAD-LEARNING))) ("tcolstu" "10" 1 NIL 0 1 (:XDATA :SCALES (ACAD-LEARNING))) ("totherst" "11" 1 NIL 0 1 (:XDATA :SCALES (ACAD-LEARNING))) ("tressub" "12" 1 NIL 0 1 (:XDATA :SCALES NIL)) ("tcolfaca" "13" 1 NIL 0 1 (:XDATA :SCALES NIL)) ("u-none" "14" 1 NIL 0 1 (:XDATA :SCALES NIL)))

;;NOTE:
;; (get-key-value "utype" (car *SHAQ-ALL-DATA-LIST) :nth-item 1)
;; this works = :MULTI    "utype"  ("UTYPE" :MULTI "utype" "UserType" 1 ("twanttho" "1" 1 T 1 1 (:XDATA :SCALES (HQ))) ("tknowmor" "2" 1 NIL 0 1 (:XDATA :SCALES (HQ))) ("twanthel" "3" 1 NIL 0 1 (:XDATA :SCALES (HQ))) ("twantspe" "4" 1 NIL 0 1 (:XDATA :SCALES NIL)) ("texperie" "5" 1 NIL 0 1 (:XDATA :SCALES NIL)) ("tprevshaq" "6" 1 NIL 0 1 (:XDATA :SCALES (PREVIOUS-USER))) ("wantspq" "7" 1 NIL 0 1 (:XDATA :SCALES (SPECIFIC-QUESTS))) ("tu100stu" "8" 1 NIL 0 1 (:XDATA :SCALES (HQ ACAD-LEARNING))) ("tcsulbst" "9" 1 NIL 0 1 (:XDATA :SCALES (ACAD-LEARNING))) ("tcolstu" "10" 1 NIL 0 1 (:XDATA :SCALES (ACAD-LEARNING))) ("totherst" "11" 1 NIL 0 1 (:XDATA :SCALES (ACAD-LEARNING))) ("tressub" "12" 1 NIL 0 1 (:XDATA :SCALES NIL)) ("tcolfaca" "13" 1 NIL 0 1 (:XDATA :SCALES NIL)) ("u-none" "14" 1 NIL 0 1 (:XDATA :SCALES NIL)))



;; ;; (get-set-append-delete-keyvalue-IN-NESTED-LIST  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1  (OLD VALUE 1) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p T :not-duplicate-p T)
;; works= (:KEY2 (OLD VALUE 1 "NEW VALUE") 2 3)
;; (A B (:KEY1 X Y) C (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3) D E :KEY3 11 12 :KEY4 (L M))
;; (:KEY2 (OLD VALUE 1 "NEW VALUE") 2 3)   (OLD VALUE 1 "NEW VALUE")   :KEY2 1 (OLD VALUE 1) 2 3)  T  (OLD VALUE 1)  NIL  (:KEY2 (OLD VALUE 1))
;;NOT-DUPLICATE-P when IS duplicate.
;; (get-set-append-delete-keyvalue-IN-NESTED-LIST  "NEW VALUE" '((:key2 0 2)) '(A B (:KEY1 X Y) C (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3) D E :KEY3 11 12 :KEY4 (L M))  :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p T :not-duplicate-p T)
;;works=  (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3)
;; (A B (:KEY1 X Y) C (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3) D E :KEY3 11 12 :KEY4 (L M))
;; (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3)  (OLD VALUE 1 "NEW VALUE")   (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3)   T  (OLD VALUE 1 "NEW VALUE")  NIL  (:KEY2 (OLD VALUE 1 "NEW VALUE"))

;; DELETE-LIST
;; (get-set-append-delete-keyvalue-in-nested-list :delete-list '(("2MK")) '(("1MK" A B C)("2MK" D E F)("3MK" G H I)))
;; works=
;; (("1MK" A B C) ("3MK" G H I));
;; ("2MK" :DELETE-LIST E F)
;; :DELETE-LIST   ("2MK" D E F)  T  D  NIL  ("2MK" D)

;;NEW-TEST
;; ;; (get-set-append-delete-keyvalue-in-nested-list 'test  ':s '("$KNW" "Knowledge" NIL NIL NIL :S ($NSC $SSC $BSC $ART $BUS $SPT $REC) :CLEV 1 )  :SIMPLE-SPLICE-KEY-VALUE-IN-KEYLIST-P T  :not-duplicate-p T)
;;NOT WORKING = ("$KNW" "Knowledge" NIL NIL NIL :S ($NSC $SSC $BSC $ART $BUS $SPT $REC) :CLEV 1 TEST)


;; (get-set-append-delete-keyvalue-in-nested-list :delete-key&value '(( :CSVAL)) '(CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :CSVAL "0.917" :RNK 3))
;;works= 
;;return-keylist= (CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :RNK 3)
;;return-nested-lists= (CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :RNK 3)
;;new-keylist= (CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :RNK 3)
      
                  
              
;;return-value= NIL
;;return-old-keylist= (CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :CSVAL "0.917" :RNK 3)
;;last-key-found-p= T  old-value= "0.917" new-key-spec-lists= NIL 
;;old-key&valu=  (:CSVAL "0.917")


;; KEY-SPEC-LIST for searching for dimslist (only 1st item searched): works
;; (get-set-append-delete-keyvalue-in-nested-list :GET '(((CS HS 2 2) 0 0))    '(((CS HS) "CS.HS" :S (((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))) ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2")))))))
;;WORKS= ((CS HS 2 2) "CS.HS.2.2")    (((CS HS) "CS.HS" :S (((CS HS 1) "CS.HS.1" :S (((CS HS 1 1) "CS.HS.1.1") ((CS HS 1 2) "CS.HS.1.2"))) ((CS HS 2) "CS.HS.2" :S (((CS HS 2 1) "CS.HS.2.1") ((CS HS 2 2) "CS.HS.2.2") ((CS HS 2 3) "CS.HS.2.3"))) ((CS HS 3) "CS.HS.3"))) ((CS MS) "CS.MS" :S (((CS MS MS1) "CS.MS.MS1" :S (((CS MS MS1 1) "CS.MS.MS1.1") ((CS MS MS1 2) "CS.MS.MS1.2"))))))      ((CS HS 2 2) "CS.HS.2.2")     (CS HS 2 2)
;; (get-set-append-delete-keyvalue-in-nested-list  "newval" '((:key3 T 1))  '(A B :KEY1 (:KEY2 X Y) :KEY3 (THIS) :KEY4 (1 2 3) :KEY5 M) :append-value-p T)
;; works= (A B :KEY1 (:KEY2 X Y) :KEY3 (THIS "newval") :KEY4 (1 2 3) :KEY5 M)
;;;; ;; (get-set-append-delete-keyvalue-in-nested-list  "newval2" '((:key3 T 1))    '(A B :KEY1 (:KEY2 X Y) :KEY3 (THIS "newval") :KEY4 (1 2 3) :KEY5 M) :append-value-p T)
;;works= (A B :KEY1 (:KEY2 X Y) :KEY3 (THIS "newval" "newval2") :KEY4 (1 2 3) :KEY5 M)
;; (get-set-append-delete-keyvalue-in-nested-list  '(newval2) '((:key3 T 1))    '(A B :KEY1 (:KEY2 X Y) :KEY3 (THIS "newval") :KEY4 (1 2 3) :KEY5 M) :append-value-p T)
;;works= (A B :KEY1 (:KEY2 X Y) :KEY3 (THIS "newval" (NEWVAL2)) :KEY4 (1 2 3) :KEY5 M)

;;FOR FINDING A LEAF IN A TREE
;; (setf *testorgdimslist '(:ORGDIMS (CS (1 (1 (1 2 3))(2 1 1) etc (HS (1 (1 (1 2 3)(2 (1(2 1 1))))(2 (3)(4 (1)(5)))))    (PS (1 (1 2))(2 (1 2 3)) etc) etc))  (NONCS etc)))
;;using new simple speckeylists 2019-07
;; (get-set-append-delete-keyvalue-in-nested-list '(NEW 1 2 3) '(CS HS 2 4 5 ) *testorgdimslist )
;; works= (5 (NEW 1 2 3))     (:ORGDIMS (CS (1 (1 (1 2 3)) (2 1 1) ETC (HS (1 (1 (1 2 3) (2 (1 (2 1 1)))) (2 (3) (4 (1) (5 (NEW 1 2 3)))))) (PS (1 (1 2)) (2 (1 2 3)) ETC) ETC)) (NONCS ETC))   (5 (NEW 1 2 3))  (NEW 1 2 3)  (5)  T NIL NIL
;;using old style speckeylists
;; (get-set-append-delete-keyvalue-in-nested-list '(NEW 1 2 3) '((CS T)(HS T)(2 T)(4 T) (5 T)) *testorgdimslist :APPEND-VALUE-P T)
;; works= (5 ((NEW 1 2 3)))    (:ORGDIMS (CS (1 (1 (1 2 3)) (2 1 1) ETC (HS (1 (1 (1 2 3) (2 (1 (2 1 1)))) (2 (3) (4 (1) (5 ((NEW 1 2 3))))))) (PS (1 (1 2)) (2 (1 2 3)) ETC) ETC)) (NONCS ETC))  omitted rest values
;; (get-set-append-delete-keyvalue-in-nested-list '(NEW 1 2 3) '((CS T)(HS T)(2 T)(4 T) (5 T)) *testorgdimslist)
;; works= (5 (NEW 1 2 3))     (:ORGDIMS (CS (1 (1 (1 2 3)) (2 1 1) ETC (HS (1 (1 (1 2 3) (2 (1 (2 1 1)))) (2 (3) (4 (1) (5 (NEW 1 2 3)))))) (PS (1 (1 2)) (2 (1 2 3)) ETC) ETC)) (NONCS ETC))  omitted rest values

;; FOR ADDITIONAL KEYS IN LAST NESTED LIST --------------------
;;;FOR :KEY-IN-PREV-KEYLIST-P
;; (get-set-append-delete-keyvalue-in-nested-list  "newval" '((:key3 T)("kind" T)(:csval 99)) '(a b c (:key1 d e (:key2 2 2 2) (:key3 ("KIND" "kind vs ukind" :BESTPOLE 1 (pole1 x x ) :CSVAL "0.917" ) 11 22) 333 444) end)  :KEY-IN-PREV-KEYLIST-P t)
;;works= ("KIND" "kind vs ukind" :BESTPOLE 1 (POLE1 X X) :CSVAL "newval")
;;(A B C (:KEY1 D E (:KEY2 2 2 2) (:KEY3 ("KIND" "kind vs ukind" :BESTPOLE 1 ;;(POLE1 X X) :CSVAL "newval") 11 22) 333 444) END)
;;("KIND" "kind vs ukind" :BESTPOLE 1 (POLE1 X X) :CSVAL "newval")
;;"newval"
;;("KIND" "kind vs ukind" :BESTPOLE 1 (POLE1 X X) :CSVAL "0.917")
;; T   "0.917"  ((:CSVAL 99))  
;; WITH DOUBLE NESTED LISTS
;; (get-set-append-delete-keyvalue-in-nested-list ' NEWVAL '((:K2  T)(:NEWKEY 99))   '(A B (C D E (F)) (:K1 ((:K2 VAL2  1 2 3 4) 77) X Y) END1 END2) :ADD-VALUE-P T :splice-newvalue-at-nth-p T :KEY-IN-PREV-KEYLIST-P T :if-not-found-append-key-value-p nil)
;;works= (:K2 VAL2 1 2 3 4 :NEWKEY NEWVAL)   (A B (C D E (F)) (:K1 ((:K2 VAL2 1 2 3 4 NEWVAL :NEWKEY) 77 77) X Y X Y) END1 END2 END1 END2)    (:K2 VAL2 1 2 3 4 :NEWKEY NEWVAL)    NEWVAL    (:K2 VAL2 1 2 3 4)   T   NIL   ((:NEWKEY 99))
;;  :KEY-IN-PREV-KEYLIST-P, APPEND-VALUE-T (adds value as a list)
;;  (get-set-append-delete-keyvalue-in-nested-list ' NEWVAL '((:K2  T)(:NEWKEY 99))   '(A B (C D E (F)) (:K1 (:K2 VAL2  1 2 3 4) X Y) END1 END2) :APPEND-VALUE-P T :splice-newvalue-at-nth-p T :KEY-IN-PREV-KEYLIST-P T :if-not-found-append-key-value-p nil)
;;works=(:K2 VAL2 1 2 3 4 :NEWKEY (NEWVAL))     (A B (C D E (F)) (:K1 (:K2 VAL2 1 2 3 4 :NEWKEY (NEWVAL)) X Y) END1 END2) (:K2 VAL2 1 2 3 4 :NEWKEY (NEWVAL))   (NEWVAL)   (:K2 VAL2 1 2 3 4)   T   NIL   ((:NEWKEY 99))
;;:KEY-IN-PREV-KEYLIST-P, APPEND-VALUE-T (adds value as a list) WHEN KEY EXISTS
;; (get-set-append-delete-keyvalue-in-nested-list ' NEWVAL '((:K2  T)(:NEWKEY 99))   '(A B (C D E (F)) (:K1 (:K2 VAL2  1 2  :NEWKEY OLDVAL  3 4) X Y) END1 END2) :APPEND-VALUE-P T :splice-newvalue-at-nth-p T :KEY-IN-PREV-KEYLIST-P T :if-not-found-append-key-value-p nil)
;;works= (:K2 VAL2 1 2 :NEWKEY (OLDVAL NEWVAL) 3 4)                                         (A B (C D E (F)) (:K1 (:K2 VAL2 1 2 :NEWKEY 3 4 (OLDVAL NEWVAL)) X Y) END1 END2)   (:K2 VAL2 1 2 :NEWKEY (OLDVAL NEWVAL) 3 4)    (OLDVAL NEWVAL)   (:K2 VAL2 1 2 :NEWKEY OLDVAL 3 4)   T   OLDVAL   ((:NEWKEY 99))
;;:KEY-IN-PREV-KEYLIST-P, ADD-VALUE-T (adds value as a list) WHEN KEY EXISTS
;; (get-set-append-delete-keyvalue-in-nested-list ' NEWVAL '((:K2  T)(:NEWKEY 99)) '(A B (C D E (F)) (:K1 (:K2 VAL2  1 2 :newkey oldval 3 4) X Y) END1 END2) :ADD-VALUE-P T :splice-newvalue-at-nth-p T :KEY-IN-PREV-KEYLIST-P T)
;; works= (:K2 VAL2 1 2 :NEWKEY OLDVAL NEWVAL 3 4)                                          (A B (C D E (F)) (:K1 (:K2 VAL2 1 2 :NEWKEY 3 4 OLDVAL NEWVAL) X Y) END1 END2)   (:K2 VAL2 1 2 :NEWKEY OLDVAL NEWVAL 3 4)    (OLDVAL NEWVAL)   (:K2 VAL2 1 2 :NEWKEY OLDVAL 3 4)   T   OLDVAL  ((:NEWKEY 99))
;;FOR :GET
;; (get-set-append-delete-keyvalue-in-nested-list :GET  '((:K2  T)(:OLDKEY T)) '(A B (C D E (F)) (:K1 (:K2 VAL2  1 2 :OLDKEY oldval 3 4) X Y) END1 END2) :ADD-VALUE-P T :splice-newvalue-at-nth-p T :KEY-IN-PREV-KEYLIST-P T)
;; works=  (:K2 VAL2 1 2 :OLDKEY OLDVAL 3 4)     (A B (C D E (F)) (:K1 (:K2 VAL2 1 2 :OLDKEY 3 4 OLDVAL) X Y) END1 END2)   (:K2 VAL2 1 2 :OLDKEY OLDVAL 3 4)   OLDVAL   SEE-RETURN-NESTED-LISTS-FOR-GET  T   OLDVAL  ((:OLDKEY T))
;; end for additional keys in last nested list -----------------------------------------------------

;;OTHER TESTS -- SEE OTHER TESTS AFTER OLD VERSION BELOW (DONE ON IT)
;;A FEW TESTS
;; SIMPLE, NON-NESTED CASES
;; (get-set-append-delete-keyvalue-in-nested-list 'NEWVAL '((K3 T)) '(K3 33 K5 55 K1 11 K7 77 K8 88))
;; works= (K3 NEWVAL K5 55 K1 11 K7 77 K8 88) (K3 NEWVAL K5 55 K1 11 K7 77 K8 88)  (K3 NEWVAL K5 55 K1 11 K7 77 K8 88)  NEWVAL  (K3 33 K5 55 K1 11 K7 77 K8 88)  T  33 NIL

;; key not on list
;; (get-set-append-delete-keyvalue-in-nested-list 'NEWVAL '((K2 T)) '(K3 33 K5 55 K1 11 K7 77 K1))
;;NEWworks?= (K2 NEWVAL)  (K3 33 K5 55 K1 11 K7 77 K1 K2 NEWVAL)  (K2 NEWVAL)  NEWVAL NIL NIL NIL NIL
;; OLDworks= (K2 NEWVAL)  (K3 33 K5 55 K1 11 K7 77 K1 K2 NEWVAL)  (K2 NEWVAL)      NEWVAL   NIL   NIL
;; on Tom directory
;; (get-set-append-delete-keyvalue-in-nested-list :GET '(("F:/LISP PROJECTS TS\\")) F02-128 :return-list-p T :TEST 'MY-EQUAL-PATH)
;; WORKS mostly, too long to write.
;; (get-tomex-dos-dir-info 'F02-128  "F:/LISP PROJECTS TS/" )
;; (get-set-append-delete-keyvalue-in-nested-list :GET '(("F:/LISP PROJECTS TS\\" T) ) F02-128 :return-list-p T :TEST 'MY-EQUAL-PATH)
;;  (multiple-value-setq (tst-retkl tst-retnls tst-newkl retval    )(get-set-append-delete-keyvalue-in-nested-list :GET '(("F:/LISP PROJECTS TS\\" T) (:DIRS T)) F02-128 :return-list-p T :TEST 'MY-EQUAL-PATH))
;; works = (:DIRS (:DIR "AI-NLP" "02/08/2017; 07:39 PM") (:DIR "AndyCares" "02/08/2017; 05:43 PM") (:DIR "ART-LW" "02/09/2017; 10:48 AM") (:DIR "ART-preNestSymBU" "02/08/2017; 06:25 PM") (:DIR "ART-Utilities" "02/23/2017; 07:58 PM") (:DIR "ART2-Output Data" "02/09/2017; 03:38 PM") (:DIR "ART3" "02/08/2017; 06:00 PM") (:DIR "ART3-BU" "02/08/2017; 06:25 PM") (:DIR "ART3-output-data" "02/09/2017; 07:07 AM") (:DIR "ARTMAP-JavaVersion" "02/08/2017; 07:47 PM") (:DIR "CL-Utilities" "02/08/2017; 07:50 PM") (:DIR "CogSys-Model" "02/11/2017; 11:05 PM") (:DIR "CogSysOutputs" "02/08/2017; 08:26 PM") (:DIR "H-Capi" "04/13/2017; 03:50 PM") (:DIR "H-CapiExamples" "02/20/2017; 03:56 PM") (:DIR "H-HELP" "04/21/2017; 08:44 AM") (:DIR "LW6-examples" "02/08/2017; 06:20 PM") (:DIR "LW7-EXAMPLES" "02/08/2017; 06:20 PM") (:DIR "LwStart" "02/08/2017; 05:04 PM")  ETC ETC
;;(:DRIVE F02-128 (:NAME "F02-128" :HOST "F:/" :LOCATION "C:/3-TS/MyDrives/TomPC/" :TYPE "??" :BRAND "??" :SIZE "??" :USED "16428451" :FREE "122851950592" :SERIAL-NUM "B78B-E44F" :DATE "Date: 5.7.2017  Time: 18:46" :LAST-LEVELN 2 :DATA ((:LEVEL 1 :NAME "F02-128" :DIRPATH "F:/" (:DIRS (:DIR "SanDiskSecureAccess" "02/24/2015; 10:35 AM") (:DIR "LISP PROJECTS TS" "04/13/2017; 09:49 AM") (:DIR "1 LW CUMMULATIVE BUS" "04/28/2017; 05:33 PM") (:DIR "Acronis Backups" "05/02/2017; 08:20 PM") ETC ETC
;;
;; (get-set-append-delete-keyvalue-in-nested-list :GET '(("F:/LISP PROJECTS TS\\") (:LEVEL t)) F02-128 :return-list-p T :TEST 'MY-EQUAL-PATH)
;; worked  return-value was the correct leveln = 2


;;2020  WHEN KEY VALUE IS A LIST (and want to change value or append it)
;; ALL NULL: :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1  (OLD VALUE 1) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;;RESULT: REPLACES OLD VALUE LIST with "NEW VALUE"
;; (:KEY2 1 "NEW VALUE" 2 3) 
;; (A B (:KEY1 X Y) C (:KEY2 1 "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M));; (:KEY2 1 "NEW VALUE" 2 3)  "NEW VALUE"  (:KEY2 1 (OLD VALUE 1) 2 3)T (OLD VALUE 1)  NIL  (:KEY2 (OLD VALUE 1))
;; ONLY :APPEND-VALUE-P = T
;;
;;ONLY :SPLICE-OLD-NEW-VALUES-P = T  ;;HERENOW 1
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1  (OLD VALUE 1) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p T)
;;WORKS= (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3)
;; (A B (:KEY1 X Y) C (:KEY2 1 OLD VALUE 1 "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))  
;; (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3) (OLD VALUE 1 "NEW VALUE")  (:KEY2 1 (OLD VALUE 1) 2 3)  T  (OLD VALUE 1)  NIL  (:KEY2 (OLD VALUE 1))
;;
;; ONLY :SIMPLE-SPLICE-KEY-VALUE-IN-KEYLIST-P = T
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1  (OLD VALUE 1) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL :SIMPLE-SPLICE-KEY-VALUE-IN-KEYLIST-P T)
;;WORKS= (:KEY2 1 (OLD VALUE 1) 2 3 "NEW VALUE")
;;(A B (:KEY1 X Y) C (:KEY2 1 (OLD VALUE 1) 2 3 "NEW VALUE") D E :KEY3 11 12 :KEY4 (L M))
;; (:KEY2 1 (OLD VALUE 1) 2 3 "NEW VALUE")   "NEW VALUE"    :KEY2 1 (OLD VALUE 1) 2 3)  T  (OLD VALUE 1)  NIL   (:KEY2 (OLD VALUE 1))

;;SSSSS FIX THIS PROBLEM -- USE either of :splice-old-new-values-p or :simple-splice-key-value-in-keylist-p INSTEAD?
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1  (OLD VALUE 1) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p T :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;;ERROR: DOT CONS WITHIN DOUBLE LIST  --- SSSS FIX??
;; (:KEY2 1 ((OLD VALUE 1 . "NEW VALUE")) 2 3)
;;(A B (:KEY1 X Y) C (:KEY2 1 ((OLD VALUE 1 . "NEW VALUE")) 2 3) D E :KEY3 11 12 :KEY4 (L M))   
;; (:KEY2 1 ((OLD VALUE 1 . "NEW VALUE")) 2 3)    ((OLD VALUE 1 . "NEW VALUE"))   (:KEY2 1 (OLD VALUE 1) 2 3)   T  (OLD VALUE 1)  NIL  (:KEY2 (OLD VALUE 1))
;;ONLY :ADD-VALUE-P = T
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1  (OLD VALUE 1) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p T  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;; ADDS "NEW VALUE" AFTER old value list 
;; (:KEY2 1 (OLD VALUE 1) "NEW VALUE" 2 3)
;; (A B (:KEY1 X Y) C (:KEY2 1 (OLD VALUE 1) "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))
;; (:KEY2 1 (OLD VALUE 1) "NEW VALUE" 2 3)   ((OLD VALUE 1) "NEW VALUE")  (:KEY2 1 (OLD VALUE 1) 2 3)   T  (OLD VALUE 1)  NIL  (:KEY2 (OLD VALUE 1))
;; ONLY :SPLICE-KEY-VALUE-IN-LIST-P = T
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1  (OLD VALUE 1) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p T :splice-old-new-values-p NIL)
;; ERROR?  REMOVES PARENS, REPLACES OLD VALUE but keeps first item ini list. SSSS FIX???
;;(:KEY2 1 "NEW VALUE" 2 3)
;;(A B (:KEY1 X Y) C (:KEY2 1 "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))
;; (:KEY2 1 "NEW VALUE" 2 3)     "NEW VALUE"  (:KEY2 1 (OLD VALUE 1) 2 3)   T  (OLD VALUE 1)   NIL  (:KEY2 (OLD VALUE 1))



;;TESTING APPEND, ADD, SPLICE, ETC ------------------------------------------------
;; GOAL= (A B (:KEY1 X Y) C (:KEY2 1 "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))  :KEY2  1  (:KEY2 1 "NEW VALUE")
;; VARIATIONS
;; 1. All NILs, FOR VALUE= "OLD-VALUE"
;;  (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 "OLD VALUE" 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;; works= (:KEY2 1 "NEW VALUE" 2 3)   (A B (:KEY1 X Y) C (:KEY2 1 "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))   (:KEY2 1 "NEW VALUE" 2 3)   "NEW VALUE"   (:KEY2 1 "OLD VALUE" 2 3)   T   NIL   "OLD VALUE"
;; 2. All NILs, FOR VALUE= (x "OLD-VALUE" y)
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 (x "OLD-VALUE" y) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;; works= (:KEY2 1 "NEW VALUE" 2 3)   (A B (:KEY1 X Y) C (:KEY2 1 "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))   (:KEY2 1 "NEW VALUE" 2 3)    "NEW VALUE"    (:KEY2 1 (X "OLD-VALUE" Y) 2 3)    T    NIL   (X "OLD-VALUE" Y)
;; 3.  :append-value-p = T, rest NIL FOR VALUE= "OLD-VALUE"
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 "OLD VALUE" 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p T :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;; works= (:KEY2 1 ("OLD VALUE" "NEW VALUE") 2 3)    (A B (:KEY1 X Y) C (:KEY2 1 ("OLD VALUE" "NEW VALUE") 2 3) D E :KEY3 11 12 :KEY4 (L M))     (:KEY2 1 ("OLD VALUE" "NEW VALUE") 2 3)         ("OLD VALUE" "NEW VALUE")       (:KEY2 1 "OLD VALUE" 2 3)       T       NIL       "OLD VALUE"
;; FOR SERIES OF APPENDING LISTS TO ORIG LIST WHERE :KEY NIL
;; For :key2 NIL
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 T)) '(a b (:key1 x y) c (:key2 NIL 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p T :list-first-item-in-append-p T :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL :RETURN-LIST-P T)
;; works= (:KEY2 ("NEW VALUE") 2 3)    (A B (:KEY1 X Y) C (:KEY2 ("NEW VALUE") 2 3) D E :KEY3 11 12 :KEY4 (L M))   (:KEY2 ("NEW VALUE") 2 3)  ("NEW VALUE")  (:KEY2 NIL 2 3)  T  NIL  NIL
;; For :key2 ("NEW VALUE")
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE2" '((:key2 T)) '(a b (:key1 x y) c (:key2 ("NEW VALUE") 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p T :list-first-item-in-append-p T :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL :RETURN-LIST-P T)
;; works= (:KEY2 ("NEW VALUE" "NEW VALUE2") 2 3)    (A B (:KEY1 X Y) C (:KEY2 ("NEW VALUE" "NEW VALUE2") 2 3) D E :KEY3 11 12 :KEY4 (L M))   (:KEY2 ("NEW VALUE" "NEW VALUE2") 2 3)   ("NEW VALUE" "NEW VALUE2")   (:KEY2 ("NEW VALUE") 2 3)   T  NIL  ("NEW VALUE")


;; 4.  :append-value-p = T, rest NIL FOR VALUE (x "OLD-VALUE" y)
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 (x "OLD-VALUE" y) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p T :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;; works= (:KEY2 1 (X "OLD-VALUE" Y "NEW VALUE") 2 3)      (A B (:KEY1 X Y) C (:KEY2 1 (X "OLD-VALUE" Y "NEW VALUE") 2 3) D E :KEY3 11 12 :KEY4 (L M))        (:KEY2 1 (X "OLD-VALUE" Y "NEW VALUE") 2 3)     (X "OLD-VALUE" Y "NEW VALUE")      (:KEY2 1 (X "OLD-VALUE" Y) 2 3)    T    NIL   (X "OLD-VALUE" Y)
;; 5.  :add-value-p= T, rest NIL FOR VALUE= "OLD-VALUE"
;; (get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 "OLD VALUE" 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p T  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;; result= (:KEY2 1 "OLD VALUE" "NEW VALUE" 2 3)     (A B (:KEY1 X Y) C (:KEY2 1 "OLD VALUE" "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))   (:KEY2 1 "OLD VALUE" "NEW VALUE" 2 3)      ("OLD VALUE" "NEW VALUE")      (:KEY2 1 "OLD VALUE" 2 3)     T   NIL   "OLD VALUE"
;; 6.  :add-value-p= T, rest NIL FOR VALUE (x "OLD-VALUE" y)
;;(get-set-append-delete-keyvalue-in-nested-list  "NEW VALUE" '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 (x "OLD-VALUE" y) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p T  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;; works= (:KEY2 1 "OLD VALUE" "NEW VALUE" 2 3)   (A B (:KEY1 X Y) C (:KEY2 1 "OLD VALUE" "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))    (:KEY2 1 "OLD VALUE" "NEW VALUE" 2 3)   ("OLD VALUE" "NEW VALUE")   (:KEY2 1 "OLD VALUE" 2 3)   T   NIL   "OLD VALUE"
;; 6B.   if new-value is list
;; (get-set-append-delete-keyvalue-in-nested-list  '(11 22 "NEW VALUE" 33 44) '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 (x "OLD-VALUE" y) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p T  :splice-key-value-in-list-p NIL :splice-old-new-values-p NIL)
;; works=  (:KEY2 1 (X "OLD-VALUE" Y) (11 22 "NEW VALUE" 33 44) 2 3)   (A B (:KEY1 X Y) C (:KEY2 1 (X "OLD-VALUE" Y) (11 22 "NEW VALUE" 33 44) 2 3) D E :KEY3 11 12 :KEY4 (L M))    (:KEY2 1 (X "OLD-VALUE" Y) (11 22 "NEW VALUE" 33 44) 2 3)    ((X "OLD-VALUE" Y) (11 22 "NEW VALUE" 33 44))   (:KEY2 1 (X "OLD-VALUE" Y) 2 3)    T   NIL  (X "OLD-VALUE" Y)
;; 7. :append-value-p= T & :splice-key-value-in-list-p = T, rest nil FOR VALUE= "OLD-VALUE"
;; (get-set-append-delete-keyvalue-in-nested-list  '( 11 22 "NEW VALUE" 33 44) '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 "OLD VALUE" 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p T :add-value-p NIL  :splice-key-value-in-list-p T :splice-old-new-values-p NIL)
;; works= (:KEY2 1 ("OLD VALUE" (11 22 "NEW VALUE" 33 44)) 2 3)    (A B (:KEY1 X Y) C (:KEY2 1 ("OLD VALUE" (11 22 "NEW VALUE" 33 44)) 2 3) D E :KEY3 11 12 :KEY4 (L M))    (:KEY2 1 ("OLD VALUE" (11 22 "NEW VALUE" 33 44)) 2 3)     ("OLD VALUE" (11 22 "NEW VALUE" 33 44))    (:KEY2 1 "OLD VALUE" 2 3)    T    NIL    "OLD VALUE"

;; 8. :append-value-p= T &  :splice-key-value-in-list-p = T, rest nil FOR VALUE (x "OLD-VALUE" y)
;; (get-set-append-delete-keyvalue-in-nested-list  '( 11 22 "NEW VALUE" 33 44) '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 (x "OLD-VALUE" y) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p T :add-value-p NIL  :splice-key-value-in-list-p T :splice-old-new-values-p NIL)
;; works=  (:KEY2 1 (X "OLD-VALUE" Y (11 22 "NEW VALUE" 33 44)) 2 3)    (A B (:KEY1 X Y) C (:KEY2 1 (X "OLD-VALUE" Y (11 22 "NEW VALUE" 33 44)) 2 3) D E :KEY3 11 12 :KEY4 (L M))    (:KEY2 1 (X "OLD-VALUE" Y (11 22 "NEW VALUE" 33 44)) 2 3)   (X "OLD-VALUE" Y (11 22 "NEW VALUE" 33 44))   (:KEY2 1 (X "OLD-VALUE" Y) 2 3)   T   NIL   (X "OLD-VALUE" Y)
;; 9. :add-value-p= T & :splice-key-value-in-list-p = T, rest nil FOR VALUE= "OLD-VALUE"
;; (get-set-append-delete-keyvalue-in-nested-list  '( 11 22 "NEW VALUE" 33 44) '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 "OLD VALUE" 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p T  :splice-key-value-in-list-p T :splice-old-new-values-p NIL)
;; works=   (:KEY2 1 "OLD VALUE" (11 22 "NEW VALUE" 33 44) 2 3)   (A B (:KEY1 X Y) C (:KEY2 1 "OLD VALUE" (11 22 "NEW VALUE" 33 44) 2 3) D E :KEY3 11 12 :KEY4 (L M))    (:KEY2 1 "OLD VALUE" (11 22 "NEW VALUE" 33 44) 2 3)     ("OLD VALUE" (11 22 "NEW VALUE" 33 44))     (:KEY2 1 "OLD VALUE" 2 3)    T    NIL   "OLD VALUE"
;; 9. :add-value-p= T & :splice-key-value-in-list-p = T, rest nil FOR VALUE= (x "OLD-VALUE" y)
;; (get-set-append-delete-keyvalue-in-nested-list  '( 11 22 "NEW VALUE" 33 44) '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 (x "OLD-VALUE" y) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p T  :splice-key-value-in-list-p T :splice-old-new-values-p NIL)
;;result=  (:KEY2 1 (X "OLD-VALUE" Y) (11 22 "NEW VALUE" 33 44) 2 3)   (A B (:KEY1 X Y) C (:KEY2 1 (X "OLD-VALUE" Y) (11 22 "NEW VALUE" 33 44) 2 3) D E :KEY3 11 12 :KEY4 (L M))     (:KEY2 1 (X "OLD-VALUE" Y) (11 22 "NEW VALUE" 33 44) 2 3)    ((X "OLD-VALUE" Y) (11 22 "NEW VALUE" 33 44))    (:KEY2 1 (X "OLD-VALUE" Y) 2 3)   T    NIL   (X "OLD-VALUE" Y)

;; 11. :add-value-p= T & :splice-old-new-values-p = T, rest nil FOR VALUE= "OLD-VALUE"
;; (get-set-append-delete-keyvalue-in-nested-list  '( 11 22 "NEW VALUE" 33 44) '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 "OLD VALUE" 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p T  :splice-key-value-in-list-p NIL :splice-old-new-values-p T)
;; NOT WORK = (:KEY2 1 "OLD VALUE" (11 22 "NEW VALUE" 33 44) 2 3)
#|(A B (:KEY1 X Y) C (:KEY2 1 "OLD VALUE" (11 22 "NEW VALUE" 33 44) 2 3) D E :KEY3 11 12 :KEY4 (L M))
(:KEY2 1 "OLD VALUE" (11 22 "NEW VALUE" 33 44) 2 3)
("OLD VALUE" (11 22 "NEW VALUE" 33 44))|#

;; 12. :add-value-p= T & :splice-old-new-values-p = T , rest nil FOR VALUE= (x "OLD-VALUE" y)
;; (get-set-append-delete-keyvalue-in-nested-list  '( 11 22 "NEW VALUE" 33 44) '((:key2 0 2)) '(a b (:key1 x y) c (:key2 1 (x "OLD-VALUE" y) 2 3) d e :key3 11 12  :key4 (l m))  :append-value-p NIL :add-value-p T  :splice-key-value-in-list-p NIL :splice-old-new-values-p T)
;;works=  (:KEY2 1 X "OLD-VALUE" Y 11 22 "NEW VALUE" 33 44 2 3)      (A B (:KEY1 X Y) C (:KEY2 1 X "OLD-VALUE" Y 11 22 "NEW VALUE" 33 44 2 3) D E :KEY3 11 12 :KEY4 (L M))         (:KEY2 1 X "OLD-VALUE" Y 11 22 "NEW VALUE" 33 44 2 3)    (X "OLD-VALUE" Y 11 22 "NEW VALUE" 33 44)    (:KEY2 1 (X "OLD-VALUE" Y) 2 3)    T   NIL   (X "OLD-VALUE" Y)

;;TESTING for treeview functions
;; (setf *testcl33 '(((1) "1" :VA A) ((2) "2" :SL (((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3) "2.3" :SL (((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6) "2.3.6" :SL (((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))) ((2 3 6 3) "2.3.6.3" :VA W))) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y))) ((2 4) "2.4" :VA Z))) ((3) "3" :VA END "3" :VA END)))
;; (get-set-append-delete-keyvalue-in-nested-list :GET '(('(2 3 6 2) 0 2) ) *testcl33)
#|NIL
(((1) "1" :VA A) ((2) "2" :SL (((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3) "2.3" :SL (((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6) "2.3.6" :SL (((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))) ((2 3 6 3) "2.3.6.3" :VA W))) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y))) ((2 4) "2.4" :VA Z))) ((3) "3" :VA END "3" :VA END))
NIL
NIL
SEE-RETURN-NESTED-LISTS-FOR-GET
NIL
NIL
NIL|#


;; RESULT= (A B (:KEY1 X Y) C (:KEY2 (1 "NEW VALUE") 2 3) D E :KEY3 11 12 :KEY4 (L M)) COMPARE TO OLDER ABOV


;;NEW TEST
;; (progn (setf out nil) (get-keyvalue-in-nested-list  (list (list 'PCE-PEOPLE 0) (list "mother" 0))  '((FIRST-LIST (K1 (A))(K2 B)) (PCE-PEOPLE  (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ ("A best male friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-F-FRIENDQ ("A best female friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (M-DISLIKEQ ("A male you very strongly dislike") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (F-DISLIKEQ ("A female you very strongly dislike") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (M-ADMIREQ ("A male who you most admire (not parent)") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)) (SECOND-LIST (K1 (A))(K2 B))(THIRD-LIST (K3 (A))(K4 B)))   :return-list-p T))


;; (progn (setf out nil) (get-keyvalue-in-nested-list  (list (list T 0) (list "mother" 0))  (eval  *cur-all-questions)  :return-list-p T))
;; (progn (setf out nil) (get-keyvalue-in-nested-list  (list (list 'PCE-PEOPLE 0) (list "mother" 0))  (eval  *cur-all-questions)  :return-list-p T))


;;TESTING FOR TREEVIEW FUNCTIONS
;; (setf *testcl33 '(((1) "1" :VA A) ((2) "2" :SL (((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3) "2.3" :SL (((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6) "2.3.6" :SL (((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))) ((2 3 6 3) "2.3.6.3" :VA W))) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y))) ((2 4) "2.4" :VA Z))) ((3) "3" :VA END "3" :VA END)))
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list :GET '(((2 3 6 2) 0 2) ) *testcl33))
#|NIL
;;       ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V)))
;;      (((1) "1" :VA A) ((2) "2" :SL (((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3) "2.3" :SL (((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6) "2.3.6" :SL (((2 3 6 1) "2.3.6.1" :VA N) ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))) ((2 3 6 3) "2.3.6.3" :VA W))) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y))) ((2 4) "2.4" :VA Z))) ((3) "3" :VA END))
;;      ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V)))
;;OLD-VALUE=   :SL
;;      SEE-RETURN-NESTED-LISTS-FOR-GET   T    NIL   NIL |#
;;
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list :GET '(((2 3 6 2) 0 2)(:SL T 1) ) *testcl33))
;; WORKS= FIRST 3 RETURNED VALUES SAME AS ABOVE
;;OLD-VALUE=           (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))
;;

;;FOR SET :SL TO NEW-VALUE
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list '(NEW-VALUE A B C)  '(((2 3 6 2) 0 2)(:SL T 1) ) *testcl33))
;;works=  (return-keylist new-return-nested-lists new-keylist return-value  old-keylist
;;return-keylist= (:SL (NEW-VALUE A B C))
;; [ if :return-list-p T then return-keylist=  ((2 3 6 2) "2.3.6.2" :SL (NEW-VALUE A B C))  ]
;;new-return-nested-lists= (((1) "1" :VA A) ((2) "2" :SL (((2 1) "2.1" :VA B) ((2 2) "2.2" :VA C) ((2 3) "2.3" :SL (((2 3 1) "2.3.1" :VA D) ((2 3 2) "2.3.2" :VA E) ((2 3 3) "2.3.3" :SL (((2 3 3 1) "2.3.3.1" :VA F) ((2 3 3 2) "2.3.3.2" :VA G) ((2 3 3 3) "2.3.3.3" :SL (((2 3 3 3 1) "2.3.3.3.1" :VA H) ((2 3 3 3 2) "2.3.3.3.2" :VA I) ((2 3 3 3 3) "2.3.3.3.3" :VA J))))) ((2 3 4) "2.3.4" :SL (((2 3 4 1) "2.3.4.1" :VA K) ((2 3 4 2) "2.3.4.2" :VA L))) ((2 3 5) "2.3.5" :VA M) ((2 3 6) "2.3.6" :SL (((2 3 6 1) "2.3.6.1" :VA N)         (((2 3 6 2) "2.3.6.2" :SL (NEW-VALUE A B C)))         ((2 3 6 3) "2.3.6.3" :VA W))) ((2 3 7) "2.3.7" :VA X) ((2 3 8) "2.3.8" :VA Y))) ((2 4) "2.4" :VA Z))) ((3) "3" :VA END))
;;new-keylist= (:SL (NEW-VALUE A B C))
;;return-value= (NEW-VALUE A B C)
;;old-keylist=  ((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V)))   T  NIL   NIL


;; ;; ==>> TESTED TO HERE  --- SEE AFTER OLD VERSION FOR TESTS ON IT





;;EDIT-MULTI-LIST-LIST- MOVE TO U-LISTS
;;2020
;;ddd
(defun edit-multi-list-list (list-of-lists &key match-elm
                               new-item edited-item new-list-items new-item-elm
                              append-new-item  item-n item-elm-n delete-item-n
                              (if-new-item-replace-p T) (default-match-item 'first)
                              (delete-item-w/elm) (null-elm "")
                              (delete-elm-if "DEL")) 
  "U-lists. For EDITING multi-column-list-panels & similar lists 2-nested lists.   RETURNS (values new-list-items new-item1 new-item-elm1  matched-elm matched-item-elm)  If NEW-ITEM = OR (:DELETE-ITEM :delete-list), deletes list/item"
  (let*
      ((n-items (list-length list-of-lists))
        (matched-item)
       (matched-elm)
       (matched-item-elm)
       (new-item-elm1)
       (new-item1)
       )
    ;;(break "new-item")
    (cond
     ;;REPLACE ALL PANE ITEMS?
     (new-list-items NIL)
#|      (when (null match-elm) (setf match-item 
                                    (eval `(,default-match-item ,new-list-items)))))|#
     ;;APPEND NEW ITEM
     (append-new-item
      (setf new-list-items (append list-of-lists (list append-new-item))
            new-item1 append-new-item)
      ;;(break "new-list-items")
      )
     ;;DELETE NTH ITEM
     (delete-item-n 
      (setf new-list-items (delete-nth new-list-items delete-item-n)))
     ;;EDITor DELETE ITEM OR ITEM-ELM
     ((or new-item-elm new-item edited-item match-elm)
      (loop
       for item in list-of-lists
       for n from 1 to n-items
       do
       ;;Does ITEM contain MATCHED-ELM?
          ;;matched match-elm (substr in any elm in item) 
       (setf matched-item (find-list-item-by-substrings match-elm item))
       ;;(break "matched-item")
         ;;MAKE THE NEW ITEM (if found)
         (cond
          ;;EDIT elm at ITEM-ELM-N in item list.
          ((and matched-item match-elm  item-elm-n)
                ;; (setf matched-item (find-list-item-by-substrings match-elm item)))
           (setf matched-elm (nth item-elm-n matched-item))
           (cond
            (new-item
             (setf new-item1 new-item))
            (new-item-elm
             (setf new-item1 (replace-nth item item-elm-n new-item-elm)))
           )
           (setf new-list-items (append new-list-items (list new-item1)))
           )
          ;;USE ITEM-N instead of matched-elm to find right item.
          ((and item-n (= n item-n))
           (setf matched-item item)
           (cond
            (item-elm-n
             (setf matched-item-elm (nth item-elm-n item)
                   new-item1 (replace-nth item item-elm-n new-item-elm)))
            (t (setf new-item1 new-item)))
           (setf new-list-items (append new-list-items (list new-item1)))
           )
          ;;FOR EDITING EXISTING ITEMS
          ;;For new-item= :delete-list or :delete-item; don't add item to new-list-items
          ((and matched-item 
                 (member new-item '(:delete-list :delete-item) :test 'equal))
           NIL)
          ;;COMPARE MATCHED-LIST [old list] with NEW-LIST-ITEMS
          ((and matched-item edited-item if-new-item-replace-p)
           ;;set new to old item, then modify with edited-item
           (unless (my-equal item edited-item)
             (setf new-item1 item)
             (loop
              for elm1 in edited-item
              for n from 0 to (list-length edited-item)
              do
              (cond
               ;;when elm = "", keep old item.
               ((my-equal elm1 null-elm) NIL)
               ;;delete old item? (replace with null-item)
               ((or (my-equal elm1 delete-elm-if) (null elm1))
                (setf new-item1 (replace-nth new-item1 n "")))
               ;;otherwise replace old with new
               (T
                (setf new-item1 (replace-nth new-item1 n elm1))))
             ;;end loop
             )
           ;;(setf new-list-items (append new-list-items (list new-item1)))
           
           ;;REPLACE MATCHED-ITEM WITH NEW-ITEM1?
           (when (and new-item1 matched-item)
            (setf new-list-items (nth-value 1
                  (get-set-append-delete-keyvalue-in-nested-list :delete-list 
                                                                 (list (list match-elm)) list-of-lists))
                  new-list-items (append new-list-items (list new-item1))))
           ;;end unless, editing existing items
           ))         
          ;;OTHERWISE if item, ADD ITEM TO LIST
         (item
          (setf new-list-items (append new-list-items (list item))))
         (T NIL))
         ;;end list-of-listsloop
         )
      ;;end (edit or delete new-item or new-item-elm )
      )
     (t nil))   
    (values new-list-items new-item1 new-item-elm1  matched-elm 
            matched-item-elm)
    ;;end let, edit-multi-list-list
    ))
;;TEST
;;EDITED ITEM
;; (edit-multi-list-list **test-mll :MATCH-ELM  "1MK" :edited-item '("7MK" "DEL" ""  "MODIFIED-ELM  " "ADDED " ""))
;;works= (("  2MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  3MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  4MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("7MK" "" "   U-BUFFER-TEST-1.lisp       " "MODIFIED-ELM  " "ADDED " "(defun get-nth-in-all-2nested-lists ~% (nth list-of-1nested-lists)  In U-lists RETURNS (values nths-1nested-lists non-nth-items). ~%NTHS-1NESTED-LISTS list of all items at nth place~% for each list in each group of 1nested lists") ("  2MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  3MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  4MK  " "      " "    buffer              " "     notes   " " dir  " "  info   "))        ("7MK" "" "   U-BUFFER-TEST-1.lisp       " "MODIFIED-ELM  " "ADDED " "(defun get-nth-in-all-2nested-lists ~% (nth list-of-1nested-lists)  In U-lists RETURNS (values nths-1nested-lists non-nth-items). ~%NTHS-1NESTED-LISTS list of all items at nth place~% for each list in each group of 1nested lists")  NIL NIL NIL

;; (SETF **TEST-MLL '((" 1MK  " " SS? " "   U-BUFFER-TEST-1.lisp       " "  my    notes   here   "    "CogSys-Model"    "(defun get-nth-in-all-2nested-lists ~% (nth list-of-1nested-lists)  In U-lists RETURNS (values nths-1nested-lists non-nth-items). ~%NTHS-1NESTED-LISTS list of all items at nth place~% for each list in each group of 1nested lists"  ) ("  2MK  " "      " "    buffer              " "     notes   "   " dir  "  "  info   ")  ("  3MK  " "      " "    buffer              " "     notes   "  " dir  "  "  info   ") ("  4MK  " "      " "    buffer              "  "     notes   " " dir  "  "  info   ")))
;;
;; (edit-multi-list-list **test-mll :match-elm "2MK" :item-elm-n 3 :new-item-elm " NEW ITEM ELM 1  ")
;; works= ((" 1MK  " " SS? " "   U-BUFFER-TEST-1.lisp       " "  my    notes   here   " "CogSys-Model" "(defun get-nth-in-all-2nested-lists ~% (nth list-of-1nested-lists)  In U-lists RETURNS (values nths-1nested-lists non-nth-items). ~%NTHS-1NESTED-LISTS list of all items at nth place~% for each list in each group of 1nested lists") ("  2MK  " "      " "    buffer              " " NEW ITEM ELM 1  " " dir  " "  info   ") ("  3MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  4MK  " "      " "    buffer              " "     notes   " " dir  " "  info   "))
;;("  2MK  " "      " "    buffer              " " NEW ITEM ELM 1  " " dir  " "  info   ")  NIL  NIL  NIL
;;
;; (edit-multi-list-list **test-mll :item-n 1 :item-elm-n 3 :new-item-elm " NEW ITEM ELM 1  ")
;;works= ((" 1MK  " " SS? " "   U-BUFFER-TEST-1.lisp       " " NEW ITEM ELM 1  " "CogSys-Model" "(defun get-nth-in-all-2nested-lists ~% (nth list-of-1nested-lists)  In U-lists RETURNS (values nths-1nested-lists non-nth-items). ~%NTHS-1NESTED-LISTS list of all items at nth place~% for each list in each group of 1nested lists") ("  2MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  3MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  4MK  " "      " "    buffer              " "     notes   " " dir  " "  info   "))
;;(" 1MK  " " SS? " "   U-BUFFER-TEST-1.lisp       " " NEW ITEM ELM 1  " "CogSys-Model" "(defun get-nth-in-all-2nested-lists ~% (nth list-of-1nested-lists)  In U-lists RETURNS (values nths-1nested-lists non-nth-items). ~%NTHS-1NESTED-LISTS list of all items at nth place~% for each list in each group of 1nested lists")  NIL  NIL    "  my    notes   here   "
;;FOR APPEND-NEW-ITEM
;; ;; (edit-multi-list-list '(("  2MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ")) :item-n nil :item-elm-n nil :new-item-elm NIL :append-new-item '("  4MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ")  )
;;works= (("  2MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") ("  4MK  " "      " "    buffer              " "     notes   " " dir  " "  info   "))("  4MK  " "      " "    buffer              " "     notes   " " dir  " "  info   ") NIL NIL NIL
;;DELETE ELM
;;EDITED ITEM
;; (edit-multi-list-list **test-mll :MATCH-ELM  "1MK" :edited-item '("7MK" "DEL" ""  "MODIFIED-ELM  " "ADDED "))







;;ORIGINAL 2017 VERSION
#|(defun get-set-append-delete-keyvalue-in-nested-list-OLD (new-value  key-spec-lists  nested-lists                                                           &key append-value-p  add-value-p (test 'my-equal)
                                                          return-list-p (test 'my-equal)
                                                          put-key-after-items
                                                          put-value-after-items
                                                          new-begin-items  new-end-items
                                                          splice-key-value-in-list-p 
                                                          splice-old-new-values-p
                                                          (return-nested-list-p T) ;;was return-orig-nested-list-p
                                                          (max-list-length 1000)  
                                                          (if-not-found-append-key-value-p T)
                                                          if-not-found-append-keyvalue-list-p 
                                                          ;;doesn't work well unless finds lower keys
                                                          if-not-found-add-item-in-last-nested-list-p 
                                                          orig-list-items-leftn
                                                          (recurse-for-key-p T)
                                                          (bottom-level-p T)
                                                          last-key-found-p )
  "In U-lists. KEYSPEC= (key keyloc-n val-nth).  FOR KEY = :NTH (to find nth in list without key) :  (1) Can have exact loc of all keys, keyloc-n as a number OR can be NIL or T [In which case will check entire list at that level for key]. Also, if the KEY = T, searches entire list.  DOES NOT check for keys in any other location than keyloc-n (UNLESS T, NIL);   (2) Each new level does NOT need a key in the key-spec-lists (it can be a list with lists containing keys; and (3) VALUE MUST either be an item next to key (val-nth = 1) OR val-nth [the optional 3rd item in the level spec-list].  If :NR member spec-list, sets RECURSE-FOR-KEY-P to NIL If :R member, sets it to T, then key MUST be IN LIST on FIRST LEVEL of current recursion (doesn't recurse on inner lists for that spec-list). 
   If in  NOT LAST KEYSPEC VAL-NTH is a NUMBER  searches that location [can be VALUE LIST] for the nested list with next key.  IF VAL-NTH not a number (eg aT), searches ALL NESTED LISTS for the NEXT KEY.
   To APPEND KEY & VALUE TO TOP LIST, set :IF-NOT-FOUND-APPEND-KEYVALUE-LIST-P to NIL. (To append whole list, set to T)
    RETURNS (return-keylist new-return-nested-lists new-keylist return-value  old-keylist last-key-found-p ) If return-list-p, return-keylist is the ENTIRE list containing key, otherwise same as new-keylist. KEY-SPEC-LISTS are lists of (key keyloc-n val-n) [val-nth optional, default= 0) from outermost to innermost (last) key. Eg of proper list (:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key5 OLD-VALUE)...)); key-spec-list= ((:key1 2)(:key2  2)(:key3 4)(:key5 0)) .IF-NOT-FOUND-APPEND-KEY-VALUE-P adds a key and keyvalue to innermost list if not found (but preceding key must be found). If last keyloc-n = 0, puts old previous keyvalue at end of new keylist. If last keyloc-n > 0, puts it first, then fills in nils before new key in last list which is new value of previous key. IF KEY = :NTH, then gets, sets, or appends number in KEYLOC-N PLACE. Note: If second item in spec-list isn't number, uses default keyloc-n.  PUT-KEY-AFTER-ITEMS is a list of items which is used if keyloc-n > 0 to fill in items between key and value [done automatically if old items are there]. splice-key-value-in-list-p does similar except includes items after the value. If SPLICE-OLD-NEW-VALUES-P, puts or splices (list key value) into put or splice list. Otherwise puts or splices key value [not a list]. COMPATIBIILITY PROBLEMS: get-key-value-in-nested-lists, val-nth starts with 0 there, with 1 here. APPEND-VALUE-P puts old-value & new-value in a list.  ADD-VALUE-P just adds new-value after old-value.
   LAST-KEY-FOUND-P must be NIL (set only in recursive calls), or keeps from adding new values sometimes.          
   NOTE: IF KEYS NOT FOUND, Can NOT reliably put new key and value in an innermost list IF LAST KEY NOT FOUND, because without a rigid orderly key system, could end up putting it inside the last of any or multiple nested lists. Therefore putting it in lowest level list.  Use the get-set-append-delete-keyvalue-in-orderly-nested-list function to put it INSIDE an inner nested list.
   return-nested-list-p often makes no difference.
  THIS CAN BE USED AS A GENERAL SEARCH FUNCTION"
  (let*
      ((return-nested-lists)
       (return-old-keylist)
       (new-return-nested-lists)
       (match-item)
       (spec-list (car key-spec-lists))
       (KEY (first spec-list))
       (keyloc-n (second spec-list))
       (val-nth )
       (new-spec-lists)
       (new-nested-list1)
       (new-nested-lists)
       (new-key-spec-lists)
       (key-spec-lists1)
       (loop-return-list)
       (add-new-value-p)
       (cur-level-list)
       (list-head)
       (list-tail)
       (length-nnl) 
       (old-value)
       (key-found-p)
       (added-key-value)
       (return-keylist)(new-keylist)(old-keylist)( return-value)
       (testitem)
       (item)
       (new-key)
       (found-keylist)
       (found-top-n)
       )
    
    ;;LISTP NESTED-LISTS--Function only processes list inputs.
    (cond           
     ((listp nested-lists)
      (setf length-nnl (list-length nested-lists))

      ;;SPEC-LIST VARIABLES ----------------------------
      (cond
       ;;KEY = T  (for compatibity with older and is useful)
       ((or  (equal key 'T)(equal key T))
        (setf  key-spec-lists  (cdr key-spec-lists)
               spec-list (car key-spec-lists)
               key (car spec-list)
               keyloc-n (second spec-list)
               val-nth (third spec-lists)
               recurse-for-key-p T)
        (when (null keyloc-n) 
          (setf keyloc-n 1))
        
        ;;(afout 'out (format nil ">>KEY= T, THEN KEY= ~A,~% spec-list= ~A keyloc-n= ~A  KEY-SPEC-LISTS= ~A~% recurse-for-key-p= ~A"key  spec-list keyloc-n key-spec-lists     recurse-for-key-p))
        )
       ;;FOR KEY = :NTH (to find nth in list without key)
       ((equal key :NTH)
        (setf val-nth 0))
       (t nil) ) 
      ;;KEYLOC-N (position of the key in nested-lists)
      (when (not (numberp keyloc-n))
        (setf  recurse-for-key-p T))
      ;;VAL-NTH, place for value after key
      (when  (third spec-list)
        (setf val-nth (third spec-list)))
      (when (null val-nth)
        (setf val-nth 1))
      ;;SET RECURSE-FOR-KEY-P (Causes search all lists in top list)
      (cond
       ((member :R spec-list)
        (setf  recurse-for-key-p T))
       ((member :NR spec-list)
        (setf  recurse-for-key-p NIL)))   
      ;;(afout 'out (format nil ">>>>>>>> 1-NESTED-LISTS= ~A~%SPEC-LIST= ~A~%KEYLOC-N= ~a  VAL-NTH= ~a~% recurse-for-key-p= ~A"  nested-lists spec-list keyloc-n  val-nth    recurse-for-key-p))

      ;;TOPLEVEL LOOP  AAA
      (loop
       for topitem in nested-lists
       for top-n from 0 to length-nnl
       do
       ;;(afout 'out  (format nil "***** TOPLEVEL LOOP, TOPITEM= ~A TOP-N=~a~%SPEC-LIST= ~A" TOPITEM TOP-N spec-list))

       ;;TOP LEVEL COND: Is item a list or not?
       (cond
        ((listp topitem)
         (cond
          ;;IF (NUMBERP KEYLOC-N), check (nth keyloc-n nested-lists) for key
          ((and (numberp keyloc-n) (not (equal key :any-key)))

           ;;First test key at this level
           (setf item (nth keyloc-n topitem))
           ;;(afout 'out (format nil "NUMBERP KEYLOC-N= ~A, found ITEM= ~A key-spec-lists= ~A~%TOPITEM list= ~A" keyloc-n item  key-spec-lists topitem ))

#|(defun testtesttt (a b &key (test 'equal))
 (eval `(,test a b))
  ;;(list test a b)
  )
  (testtesttt  'this 'this)
|#

           (cond
            ;;TOPITEM = KEY
            ((or (eval `(,test key item)) (equal key :nth)) 
             ;;was (or (my-equal key item) (equal key :nth))
             (setf key-found-p T
                   list-head (butlast nested-lists (- length-nnl top-n ))
                   list-tail (nthcdr (+ top-n 1) nested-lists)
                 new-key-spec-lists (cdr key-spec-lists))
             ;;(afout 'out (format nil ">>>In numberp, HEAD-TAIL:KEY= ~A FOUND-P= ~A keyloc-n ~A top-n= ~A  length-nnl= ~A~% new-nested-lists= ~A~%list-head= ~A ~%list-tail= ~A"key key-found-p keyloc-n top-n  length-nnl new-nested-lists list-head    list-tail))
             (cond
              ((null new-key-spec-lists)
               ;;copied from old
               (setf old-value (nth (+ keyloc-n  val-nth) topitem)
                     old-keylist topitem
                     last-key-found-p T)

               ;;IS KEY= :NTH?
               (cond 
                ((not (equal key :NTH)) 
                 (multiple-value-setq (return-keylist new-keylist return-value)
                     (get-set-append-delete-keyvalue key  new-value
                                              :keyloc-n keyloc-n :val-nth val-nth :test test
                                              :old-keylist TOPITEM
                                              :append-value-p append-value-p
                                              :add-value-p add-value-p
                                              :put-key-after-items put-key-after-items
                                              :put-value-after-items put-value-after-items
                                              :new-begin-items new-begin-items 
                                              :new-end-items new-end-items
                                              :splice-key-value-in-list-p splice-key-value-in-list-p
                                              :splice-old-new-values-p splice-old-new-values-p))
                 (setf return-old-keylist old-keylist)

                 ;;Inserts items betw keyloc-n and val-nth into final returned list
                 (cond
                  ((> keyloc-n 0)
                   (setf  put-key-after-items (subseq topitem 0  keyloc-n) ;; (+ keyloc-n val-nth))
                          ;;was (+ keyloc-n 1) (+ keyloc-n val-nth))
                          ;;was splice-old-new-values-p NIL ;;SSS or should be NIL?
                          splice-key-value-in-list-p nil))
                  ;;added
                  (t (setf put-key-after-items NIL)))
                 ;;copied from old end

                 ;;(afout 'out (format nil "LAST KEY FOUND= key= ~A  old-keylist= ~a~%put-key-after-items= ~a old-value= ~A" key old-keylist put-key-after-items old-value))
                 (setf return-nested-lists  (append list-head
                                                    (list return-keylist)
                                                    list-tail))
                 ;;(BREAK "before return-list-p")
                 (when return-list-p
                   (setf return-keylist return-nested-lists))
                 ;;(afout 'out (format nil "AFTER LAST-KEY-FOUND: list-head= ~A~%return-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A" list-head return-keylist list-tail return-nested-lists))   
                 (when last-key-found-p
                   (return)) 
                 ;;(BREAK "RETURN? if  last-key-found-p")
                 ;;END (not (equal key :NTH))
                 )
                ;;was (setf old-keylist (list key old-value)))
                (t  nil)) ;;(setf old-keylist (list old-value))))
               ;;end null spec-list
               )
              ;;RECURSE TO NEXT KEY-SPEC
              ( T 
                ;;RECURSE ON  new-key-spec-lists new-nested-lists ;;same as nested lists
                ;; ADD TO RETURN NESTED-LISTS ETC
                ;;VARS SET ABOVE
                ;;(afout 'out (format nil "RECURSE TO NEXT KEY-SPEC: list-head= ~A~%return-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A next-key-in-value-p= ~a" list-head return-keylist list-tail return-nested-lists next-key-in-value-p))
                (cond
                 ;;WHEN VAL-NTH IS A NUMBER
                 ((numberp val-nth)              ;;was next-key-in-value-p
                  (setf found-top-n top-n)
                  ;;set above  list-tail  (nthcdr top-n))
                  )
                 ;;OTHERWISE SEARCH ENTIRE CURRENT LIST 
                 ;; for nested-lists that contain (at next level ONLY) the next key
                 ;; Set it as the new 
                 (T
                  (multiple-value-setq (topitem found-top-n)
                      (find-list-with-item key nested-lists))
                  ))
                ;;in either case
                (setf topitem (nth (+ val-nth top-n) nested-lists)
                      list-head (butlast nested-lists (- length-nnl (+ top-n val-nth)))  
                      ;;was (+ (- length-nnl top-n) val-nth))
                      list-tail (nthcdr  (+ found-top-n val-nth)  nested-lists))    
                ;;(break "XX BEFORE RECURSE")

                ;;RECURSE ON SAME LIST WITH CDR  KEY-SPEC-LIST (set above)
                (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                                     return-value   return-old-keylist last-key-found-p )
                    (get-set-append-delete-keyvalue-in-nested-list new-value  new-key-spec-lists  
                                                            TOPITEM 
                                                            :return-list-p return-list-p :test test
                                                            :append-value-p  append-value-p
                                                            :add-value-p add-value-p
                                                            :new-begin-items new-begin-items
                                                            :new-end-items new-end-items
                                                            :max-list-length max-list-length
                                                            :last-key-found-p   last-key-found-p
                                                            ;;      :new-keylist new-keylist
                                                            ;;     :old-keylist old-keylist
                                                            ;;      :return-value return-value
                                                            :if-not-found-append-key-value-p NIL
                                                            :bottom-level-p nil
                                                            ;; :put-key-after-items put-key-after-items
                                                            ;; :put-value-after-items put-value-after-items
                                                            ;; :splice-key-value-in-list-p splice-key-value-in-list-p
                                                            :splice-old-new-values-p splice-old-new-values-p
                                                            ;;  :return-keylist return-keylist
                                                            ))

                (setf  return-nested-lists (append list-head 
                                                   put-key-after-items  ;; (list key)  ;;list key added
                                                   (list  new-return-nested-lists)   list-tail))

                ;;(afout 'out (format nil "IN TOP NUMBERP, AFTER RECURSE ON TOPITEM= ~a, top-n= ~A new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a" TOPITEM top-n new-keylist  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items))

                (when return-list-p
                  (setf  return-keylist  new-return-nested-lists))
                ;;(BREAK "BEFORE RETURN")
                (when last-key-found-p
                  (return))
                ;;end key found, but not last key, cond
                ))
             ;;END KEY=ITEM
             )
            ;;KEY NOT= ITEM, and  (null recurse-for-key-p)
            ((null recurse-for-key-p)
             (setf old-keylist topitem) ;;added is this right?
             (setf return-nested-lists (append return-nested-lists (list topitem)))
             ;;(afout 'out (format nil "IN TOP NUMBERP, NO-RECURSE, KEY= ~a NOT= ITEM= ~a~%topitem= ~A~% (append return-nested-lists (list topitem))= ~A"  key item topitem return-nested-lists))
             ;;end (null recurse-for-key-p)
             )
            ;;KEY NOT=ITEM, RECURSE (recurse-for-key-p)
            (recurse-for-key-p
             ;;RECURSE ON  new-key-spec-lists new-nested-lists ;;same as nested lists
             ;; ADD TO RETURN NESTED-LISTS ETC
             ;;don't loop, done in recurse?    
             (setf list-head (butlast nested-lists (- length-nnl top-n))
                   list-tail (nthcdr (+ top-n 1) nested-lists))
             (setf old-keylist topitem) ;;added is this right?

             ;;(BREAK "XX BEFORE TOPITEM LIST RECURSE")
             ;;RECURSE ON SAME LIST WITH SAME  KEY-SPEC-LIST (set above)
             ;;  WAS ON CDR KEY-SPEC-LIST, BUT IF NOT FOUND, NO
             (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                                  return-value   return-old-keylist last-key-found-p )
                 (get-set-append-delete-keyvalue-in-nested-list new-value  key-spec-lists  
                                                         TOPITEM 
                                                         :return-list-p return-list-p :test test
                                                         :append-value-p  append-value-p
                                                         :add-value-p add-value-p
                                                         :new-begin-items new-begin-items :new-end-items new-end-items
                                                         :max-list-length max-list-length
                                                         :last-key-found-p   last-key-found-p
                                                         ;;      :new-keylist new-keylist
                                                         ;;     :old-keylist old-keylist
                                                         ;;      :return-value return-value
                                                         :put-value-after-items put-value-after-items
                                                         :if-not-found-append-key-value-p NIL
                                                         :RECURSE-FOR-KEY-P T
                                                         :bottom-level-p nil
                                                         ;; :put-key-after-items put-key-after-items
                                                         ;; :splice-key-value-in-list-p splice-key-value-in-list-p
                                                         :splice-old-new-values-p splice-old-new-values-p
                                                         ;;  :return-keylist return-keylist
                                                         ))
             (setf  return-nested-lists (append list-head 
                                                put-key-after-items  ;; (list key)  ;;list key added
                                                (list  new-return-nested-lists)   list-tail))

             ;;(afout 'out (format nil "IN TOP NUMBERP, AFTER RECURSE ON TOPITEM LIST= ~a, top-n= ~A new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a" TOPITEM top-n new-keylist  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items))

             (when return-list-p
               (setf  return-keylist  new-return-nested-lists))
             ;;(BREAK "BEFORE RETURN")
             (when last-key-found-p
               (return))
             ;;end recurse-for-key-p,cond
             ))

           ;;END NUMBERP KEYLOC-N
           )
          ;;(LISTP TOPITEM), BUT  KEYLOC-N = T, NIL or other non-number
          ;;Recurse on this list to search for key?
          (recurse-for-key-p
           (setf list-head (butlast nested-lists (- length-nnl top-n)) ;;lll
                 list-tail (nthcdr (+ top-n 1) nested-lists))
           (setf old-keylist topitem) ;;added is this right?

           ;;put this here?
           (cond
            ((equal key :any-key)
             (setf key-spec-lists1 (cdr key-spec-lists)))
            (t (setf key-spec-lists1 key-spec-lists)))
           
           (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                                return-value   return-old-keylist last-key-found-p )
               (get-set-append-delete-keyvalue-in-nested-list new-value  key-spec-lists1  
                                                       TOPITEM 
                                                       :return-list-p return-list-p :test test
                                                       :append-value-p append-value-p
                                                       :add-value-p add-value-p
                                                       :new-begin-items new-begin-items 
                                                       :new-end-items new-end-items
                                                       :max-list-length max-list-length
                                                       :last-key-found-p   last-key-found-p
                                                       ;;      :new-keylist new-keylist
                                                       ;;     :old-keylist old-keylist
                                                       ;;      :return-value return-value
                                                       :if-not-found-append-key-value-p NIL
                                                       :RECURSE-FOR-KEY-P  recurse-for-key-p
                                                       :bottom-level-p nil
                                                       ;; :put-key-after-items put-key-after-items
                                                       ;; :splice-key-value-in-list-p splice-key-value-in-list-p
                                                       ;; :put-value-after-items put-value-after-items
                                                       :splice-old-new-values-p splice-old-new-values-p
                                                       ;;  :return-keylist return-keylist
                                                       ))
           (setf  return-nested-lists (append list-head 
                                              put-key-after-items  ;; (list key)  ;;list key added
                                              (list  new-return-nested-lists)   list-tail))

           ;;(afout 'out (format nil "IN TOP NUMBERP LOOP, AFTER RECURSE, ITEM=new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a" ITEM  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items))

           (when last-key-found-p
             (return))
           ;;end listp topitem, not numberp keyloc-n,  listp item, recurse-for-key-p
           ))
         ;;END (LISTP TOPITEM)
         )
        ;;TOPITEM NOT A LIST
        (t
         (cond
          ;;TOPITEM = KEY ( or NTH if :NTH)
          ((or (funcall test  topitem key) ;;was (my-equal topitem key)
               (and (equal key :nth) (equal top-n keyloc-n)))
           ;;(afout 'out (format nil ">>>> 1. TOPITEM NOT A LIST, KEY= ~a  FOUND~% NESTED-LISTS=old-keylist= ~A, ~%FOR OLD: SPEC-LIST= ~a, TOP-N= ~a  " key nested-lists spec-list top-n ))
           (setf key-found-p T              
                 new-key-spec-lists (cdr key-spec-lists))
           (when (not (numberp keyloc-n))
             (setf keyloc-n top-n)) 
           (cond
            ((numberp val-nth)
             (setf new-nested-lists (nth (+ top-n val-nth) nested-lists)
                   old-keylist  nested-lists                  
                   list-head (butlast nested-lists (- length-nnl (+ top-n val-nth)))
                   list-tail (nthcdr (+ top-n val-nth 1) nested-lists)))
            (t (setf new-nested-lists nested-lists
                      old-keylist  nested-lists )))  ;;could cause infinite loop?? NEW
           ;;(afout 'out (format nil ">>>> 2. TOPITEM NOT A LIST, KEY= ~a  FOUND~% NESTED-LISTS= ~A, ~%FOR NEW RECURSE: new-key-spec-lists ~a, TOP-N= ~a ~%LIST-HEAD= ~a~%LIST-TAIL= ~a" key nested-lists new-key-spec-lists top-n  list-head list-tail))
           ;;(BREAK "TOPITEM NOT LIST, FOR NEW RECURSE:")

           (cond
            ;;LAST-KEY-FOUND-P
            ((null new-key-spec-lists)
             ;;GET-SET
             (setf last-key-found-p T
                   old-value (nth (+ keyloc-n val-nth) nested-lists)
                   old-keylist nested-lists)
            ;;(afout 'out (format nil ">>>> 2. LAST-KEY-FOUND. OLD-VALUE= ~A" old-value))      
#|    not needed, causes extra addition of these items in front of return-keylist??
        (cond
             ((and (numberp keyloc-n) (> keyloc-n 0))
              (setf  put-key-after-items (subseq nested-lists 0  keyloc-n) 
                     ;;was (+ keyloc-n 1) (+ keyloc-n val-nth))
                     ;;was  splice-old-new-values-p NIL ;;SSS or should be NIL?
                     splice-key-value-in-list-p nil))
            ;;added
            (t (setf put-key-after-items NIL)))|#

             ;;(BREAK "BEFORE GET-SET-OR-APPEND")

             ;;HERE
             ;;USE GET-SET-OR-APPEND
             (multiple-value-setq (return-keylist new-keylist return-value)
                 (get-set-append-delete-keyvalue key  new-value
                                          :keyloc-n keyloc-n :val-nth val-nth :test test
                                          :old-keylist old-keylist
                                          :append-value-p append-value-p
                                          :add-value-p add-value-p
                                          :put-key-after-items put-key-after-items
                                          :put-value-after-items put-value-after-items
                                          :new-begin-items new-begin-items
                                          :new-end-items new-end-items
                                          :splice-key-value-in-list-p splice-key-value-in-list-p
                                          :splice-old-new-values-p splice-old-new-values-p))
             (setf return-old-keylist old-keylist)
             (setf return-nested-lists  (append list-head
                                                (list return-value)   ;;was return-keylist
                                                list-tail))

             ;;(BREAK "before return-list-p")
             (when return-list-p
               (setf return-keylist return-nested-lists))

             ;;(afout 'out (format nil "TOPITEM= ~A NOT A LIST AFTER LAST-KEY-FOUND:~%GET-SET-APPEND; top-n= ~A list-head= ~A~%return-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A OLD-VALUE= ~a" TOPITEM top-n list-head return-keylist list-tail return-nested-lists old-value))
             (when last-key-found-p
               (return))
             ;;end (null new-key-spec-lists)
             )
            ;;LAST KEY NOT FOUND, RECURSE ON NEW-NESTED-LISTS
            (t
             ;;(BREAK "KEY-FOUND, RECURSE, TOPITEM NOT A LIST")
             ;;Arguments set above after key found
             ;;to fix errors, redundant?
             (setf key-spec-list (car key-spec-lists))
             (multiple-value-setq (key keyloc-n val-nth)
                 (values-list key-spec-list))
             ;;test added
             (cond
              ((not (numberp val-nth))
               (setf new-key (caar new-key-spec-lists))    
                     
               ;;when val-nth is not a number search entire current list 
               (cond
                ((member new-key nested-lists :test 'my-equal)
                 (setf new-nested-lists nested-lists) )
                ( t 
                  (multiple-value-setq (found-keylist found-top-n)
                        (find-list-with-item new-key nested-lists))
                    (when found-keylist
                      (setf new-nested-lists  found-keylist)
                      ))
                )
               ;;add??       new-key-spec-lists (cdr new-spec-lists)
               ;;end whens
               ))
              (cond
               ((listp new-nested-lists)
                (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                                     return-value   return-old-keylist last-key-found-p )
                    (get-set-append-delete-keyvalue-in-nested-list new-value  new-key-spec-lists  
                                                            NEW-NESTED-LISTS
                                                            :return-list-p return-list-p :test test
                                                            :append-value-p append-value-p
                                                            :add-value-p add-value-p
                                                            :new-begin-items new-begin-items :new-end-items new-end-items
                                                            :max-list-length max-list-length
                                                            :last-key-found-p   last-key-found-p
                                                            ;;      :new-keylist new-keylist
                                                            ;;     :old-keylist old-keylist
                                                            ;;      :return-value return-value
                                                            :if-not-found-append-key-value-p NIL
                                                            :bottom-level-p nil
                                                            ;; :put-key-after-items put-key-after-items
                                                            ;; :splice-key-value-in-list-p splice-key-value-in-list-p
                                                            ;; :splice-old-new-values-p splice-old-new-values-p
                                                            :splice-old-new-values-p splice-old-new-values-p
                                                            ;;  :return-keylist return-keylist
                                                            ))
                ;;(afout 'out (format nil "IN LOOP, TOPITEM=KEY (NOT A LIST),, AFTER RECURSE, TOPITEM=new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a~%old-keylist= ~A last-key-found-p= ~A" TOPITEM  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items old-keylist last-key-found-p))
                )
               (t NIL))
              ;;WAS (BREAK "ERROR: NEW-NESTED-LISTS= [~A  ]  IS NOT A LIST [May have wrong keyloc-n]" new-nested-lists), BUT OK FOR IT TO NOT BE A NESTED LIST.
              ;;SSS DOES MORE CODE NEED TO BE WRITTEN TO PROCEED IF THIS NON-LIST?
              (cond
               ((numberp keyloc-n)
                (setf  return-nested-lists (append list-head 
                                                   put-key-after-items  ;; (list key)  ;;list key added
                                                   (list  new-return-nested-lists)   list-tail)))
               ;;If keyloc-n = T, return whole nested list
               (t (setf return-nested-lists new-return-nested-lists)))                         

              ;;(BREAK "IN LOOP, TOPITEM=KEY (NOT A LIST), RETURN? if  last-key-found-p")
              ;;test this
              (when (or last-key-found-p (null (cdr new-key-spec-lists)))
                ;;(break "test")
                (return))
              ))
           ;;end key found
           )
          ;;added 2017-03
          ;;AAA          ;;KEY NOT FOUND AS LAST ITEM IN FINAL TOPLIST
          ((and (not (numberp keyloc-n))
                (= top-n (- length-nnl 1)) ;;last item in nested-lists
                if-not-found-add-item-in-last-nested-list-p ;;PUT IN KEYS
                (or if-not-found-append-key-value-p 
                    if-not-found-append-keyvalue-list-p))
           (setf old-keylist nested-lists) ;;SSS OR topitem or new-return-nested-lists?
           (multiple-value-setq (return-keylist new-keylist return-value)
               (get-set-append-delete-keyvalue key  new-value
                                        :keyloc-n keyloc-n :val-nth val-nth
                                        :old-keylist old-keylist :test test
                                        :append-value-p append-value-p ;;was if-not-found-append-keyvalue-list-p
                                        :add-value-p add-value-p ;;was if-not-found-append-key-value-p
                                        :put-key-after-items put-key-after-items
                                        :put-value-after-items put-value-after-items
                                        :new-begin-items new-begin-items :new-end-items new-end-items
                                        :splice-key-value-in-list-p splice-key-value-in-list-p
                                        :splice-old-new-values-p splice-old-new-values-p))
           (setf return-old-keylist old-keylist)
           ;;(break "NO KEY FOUND, APPEND")
           ;;end when, = top-n
           )
          ;;end added
          ;;KEY NOT FOUND  AS ITEM IN TOPLIST
          (T
           (setf return-nested-lists (append return-nested-lists (list topitem)))
           ;;(afout 'out (format nil "AT LOOP END, NO KEY FOUND as item in toplist, top-n= ~A length-nnl= ~A topitem= ~A" top-n length-nnl topitem))
           ))
         ;;END TOPITEM NOT A LIST, COND
         ))

       ;;END TOPLEVEL LOOP
       )
      ;;END LISTP NESTED-LISTS
      )
     ;;NESTED-LISTS NOT A LIST
     (T NIL )) ;;was (break "ERROR: NESTED-LISTS IS NOT A LIST")))

    ;;IF-NOT-FOUND-APPEND-KEY-VALUE-P
    ;;NOTE: Can NOT reliably put new key and value in an innermost list IF LAST KEY NOT FOUND, because without a rigid orderly key system, could end up putting it inside the last of any or multiple nested lists. Therefore putting it in lowest level list.  Use the get-set-append-delete-keyvalue-in-orderly-nested-list function to put it INSIDE an inner nested list.
    (when (and bottom-level-p
               (or if-not-found-append-key-value-p
                   if-not-found-append-keyvalue-list-p)  
               (null if-not-found-add-item-in-last-nested-list-p)
               (null last-key-found-p) (null new-keylist) 
               (not (member new-value
                            '(:get :set-keylist=nil :delete-value :delete-key&value))))
      (setf  added-key-value T
             key (caar (last key-spec-lists))
             new-keylist (list key new-value)
             return-keylist new-keylist
             return-value new-value)
      (cond
       (if-not-found-append-keyvalue-list-p
        (setf return-nested-lists (append nested-lists 
                                          (list new-keylist))))
       (t (setf return-nested-lists (append nested-lists 
                                            new-keylist))))
      
      ;;(break "new-value if not found")      
      ;;(afout 'out (format nil "KEY NEVER FOUND, return-nested-lists= ~A" return-nested-lists))
      ;;end if-not-found-append-key-value-p clause
      )

    (when (and bottom-level-p (null return-nested-list-p)) ;;was (null return-orig-nested-list-p)
      (setf return-nested-lists  (list "return-nested-list-p=NIL")))  ;;was (list "return-orig-nested-list-p=NIL"))))

    ;;(BREAK "AT END")
    (values return-keylist return-nested-lists new-keylist return-value    
            return-old-keylist last-key-found-p )
    ;;end let, get-set-append-delete-keyvalue-in-nested-list
    ))|#
;;TEST
;; SIMPLE, NON-NESTED CASES
;; (get-set-append-delete-keyvalue-in-nested-list 'NEWVAL '((K3 T)) '(K3 33 K5 55 K1 11 K7 77 K8 88))
;; works= (K3 33 K5 NEWVAL K1 11 K7 77 K8 88)   (K3 33 K5 NEWVAL K1 11 K7 77 K8 88)   (K3 33 K5 NEWVAL K1 11 K7 77 K8 88)   NEWVAL  (K3 33 K5 55 K1 11 K7 77 K8 88)   T
;; key not on list
;; (get-set-append-delete-keyvalue-in-nested-list 'NEWVAL '((K2 T)) '(K3 33 K5 55 K1 11 K7 77 K1))
;; works= (K2 NEWVAL)  (K3 33 K5 55 K1 11 K7 77 K1 K2 NEWVAL)  (K2 NEWVAL)      NEWVAL   NIL   NIL
;;    (THIRD G-DRIVE)
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list "NEW-LOCATION"  '((:LOCATION T)) (THIRD G-DRIVE) :if-not-found-append-keyvalue-list-p NIL  :recurse-for-key-p nil))
;; works= (:LOCATION "NEW-LOCATION")
;;return-keylist= (:LOCATION "NEW-LOCATION")
;;return-nested-lists= (:NAME "G-drive" :HOST "G:\\" :DATE "Date: 3.19.2017  Time: 9:37" :NAME "G-drive" :HOST "G:\\" :DATE "Date: 3.19.2017  Time: 9:37" :LAST-LEVELN 1 :DATA ((:LEVEL 1 "G:\\" (:DIRNAMES "\\System Volume Information\\" "\\From XPS\\" "\\MyUtilities 1\\" "\\CogSys-Model\\" "\\PWSf\\" "\\0 CURRENT DejaOffice-CompanionLink BU\\" "\\2016-12 Friends - Christmas letter\\" "\\0 Book Article for Academic Websites\\" "\\2 LW Folders\\" "\\1 LW CUMMULATIVE Folder BUs\\" "\\LISP PROJECTS TS\\") (:FILENAMES "2016 Merry Christmas.docx" "How To Be Happy-book article.docx") (:SUBDIR-INFO ("G:\\System Volume Information\\" ("System Volume Information")) ("G:\\From XPS\\" ("From XPS")) ("G:\\MyUtilities 1\\" ("MyUtilities 1")) ("G:\\CogSys-Model\\" ("CogSys-Model")) ("G:\\PWSf\\" ("PWSf")) ("G:\\0 CURRENT DejaOffice-CompanionLink BU\\" ("0 CURRENT DejaOffice-CompanionLink BU")) ("G:\\2016-12 Friends - Christmas letter\\" ("2016-12 Friends - Christmas letter")) ("G:\\0 Book Article for Academic Websites\\" ("0 Book Article for Academic Websites")) ("G:\\2 LW Folders\\" ("2 LW Folders")) ("G:\\1 LW CUMMULATIVE Folder BUs\\" ("1 LW CUMMULATIVE Folder BUs")) ("G:\\LISP PROJECTS TS\\" ("LISP PROJECTS TS"))) :LAST-LEVELN 1)) :LOCATION "NEW-LOCATION")
;;return-value= (:LOCATION "NEW-LOCATION")
;;new-keylist= "NEW-LOCATION"
;;old-keylist= NIL  
;;last-key-found-p= NIL

;;SEARCH FOR ONE KEY IN COMPLEX NESTING with keyloc-n = number
;;CAN BE USED AS A GENERAL SEARCH FUNCTION
;;     (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (l m ( x y z (:KEY2 (0.55) ))( inside this list (and this one (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 OLD-VALUE end1 end2) post3)) also outside ) end l m)  :KEY5 (xxx)) :append-value-p t :return-list-p T :splice-old-new-values-p NIL ))
;;works= 
;;return-list [outside-list]= (L M (X Y Z (:KEY2 (0.55))) (INSIDE THIS LIST (AND THIS ONE (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (OLD-VALUE NEW-VALUE) END1 END2) POST3)) ALSO OUTSIDE) END L M)
;; new-nested-lists ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (L M (X Y Z (:KEY2 (0.55))) (INSIDE THIS LIST (AND THIS ONE (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (OLD-VALUE NEW-VALUE) END1 END2) POST3)) ALSO OUTSIDE) END L M) :KEY5 (XXX))
;;new-keylist= (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (OLD-VALUE NEW-VALUE) END1 END2)
;;new-value=(OLD-VALUE NEW-VALUE)
;;old-keylist= (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 OLD-VALUE END1 END2)
;;last-key-found-p= T
;;


;; COMPLEX TESTS (with added items, etc) -----------------------------------------
;;added items in all cases, append-list-p T, :append-value-p T, splice-old-new-values-p NIL
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 2)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 0.95 end1 end2) post3) :KEY5 (xxx)) :append-value-p t :return-list-p T :splice-old-new-values-p NIL))
;;works=  (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2) POST3)             ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2) POST3) :KEY5 (XXX))               (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2)                       (0.95 NEW-VALUE)                       (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3)      T
;;;;added items in all cases, append-list-p T, :append-value-p T, splice-old-new-values-p T
;;(progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 2)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 0.95 end1 end2) post3) :KEY5 (xxx)) :append-value-p t :return-list-p T :splice-old-new-values-p NIL)) 
;;works= (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2) POST3)              ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2) POST3) :KEY5 (XXX))                (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2)                (0.95 NEW-VALUE)                  (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3)  T

;; with T as keyloc-n AND  added items in all cases, append-list-p T, :append-value-p T, splice-old-new-values-p T [NOTE: first keyspeclist = (:key3 T 2) bec next is in its value.]
;;   (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 OLD-VALUE end1 end2) post3) :KEY5 (xxx)) :append-value-p t :return-list-p T :splice-old-new-values-p T))
;;works=  (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (OLD-VALUE NEW-VALUE) END1 END2)                ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (OLD-VALUE NEW-VALUE) END1 END2) POST3) :KEY5 (XXX))           (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 OLD-VALUE NEW-VALUE END1 END2)                (OLD-VALUE NEW-VALUE)                 (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 OLD-VALUE END1 END2)    T

;;USES :NEXT-KEY-IN-VALUE (key3 not in a separate list here)
;;(progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2 :next-key-in-value)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 0.95 end1 end2) post3 :KEY5 (xxx)) :append-value-p t :return-list-p T :splice-old-new-values-p T))
;;works=  (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2)            ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2) POST3 :KEY5 (XXX))                (:KEY4 (0.95 NEW-VALUE))                (0.95 NEW-VALUE)              (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2)            T   

;;FIND NEXT KEY IF NOT IN VALUE LOCATION OF PREVIOUS
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T T)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 OLD-VALUE end1 end2) post3) :KEY5 (xxx)) :append-value-p t :return-list-p T :splice-old-new-values-p T))

;;WHEN VAL-NTH = T, searches same list and ONE-LEVEL of all lists within the current nested-lists.
;;(progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T T)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 OLD-VALUE end1 end2) post3) :KEY5 (xxx)) :append-value-p t :return-list-p T :splice-old-new-values-p T :recurse-for-key-p T))
;;works= (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (OLD-VALUE NEW-VALUE) END1 END2)           ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (OLD-VALUE NEW-VALUE) END1 END2) :KEY5 (XXX))                (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 OLD-VALUE NEW-VALUE END1 END2)             (OLD-VALUE NEW-VALUE)            (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 OLD-VALUE END1 END2)    T


;;SSS START TESTING HERE,  TESTED TO HERE   

;; ADDED ITEMS, KEY= T, NEXT-KEY-IN-VALUE
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-nested-list 'NEW-VALUE  '((:PC-VALUES T 1 :NR :NEXT-KEY-IN-VALUE)(A3 T)) '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)(A4 "this is A4") x y) (list z))))
;;works=  (A3 NEW-VALUE)          (:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 NEW-VALUE :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12) (A4 "this is A4") X Y) (LIST Z))               (A3 NEW-VALUE)            NEW-VALUE    (A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)    T

;USES :NEXT-KEY-IN-VALUE
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-nested-list 'NEW-VALUE  '((:PC-VALUES T 1 :NR :NEXT-KEY-IN-VALUE)(A3 T)) '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)(A4 "this is A4") x y) (list z))))
;;works=  (A3 NEW-VALUE)                  ((A3 NEW-VALUE "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12) (A4 "this is A4") X Y)              (A3 NEW-VALUE)             NEW-VALUE               (A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)           T 
;;
;; :NEXT-KEY-IN-VALUE
;; (get-set-append-delete-keyvalue-in-nested-list "new location"  '((:DRIVE 0 2 :NEXT-KEY-IN-VALUE )(:location   0  ))  f-drive)



;;xxx SIMPLIER TESTS ---------------------------------------------------
;; normal key-spec-lists
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key2 0)(:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95))) :KEY5 (xxx)) :append-value-p t))
;;works= (:KEY4 (0.95 NEW-VALUE))          ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55) (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE)))) :KEY5 (XXX))               (:KEY4 (0.95 NEW-VALUE))        (0.95 NEW-VALUE)        (:KEY3 (44) (:KEY4 0.95))           T
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95)) :KEY5 (xxx)) :append-value-p t))
;; works= (:KEY4 (0.95 NEW-VALUE))    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) :KEY4 (0.95 NEW-VALUE)) :KEY5 (XXX))    (:KEY4 (0.95 NEW-VALUE))    (0.95 NEW-VALUE)   (:KEY4 0.95)   T
;; set with return-list-p
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95)) :KEY5 (xxx)) :return-list-p T))
;; works= (:KEY3 (44) :KEY4 NEW-VALUE)    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) :KEY4 NEW-VALUE) :KEY5 (XXX))    (:KEY4 NEW-VALUE)    NEW-VALUE    (:KEY4 0.95)    T
;; keyloc-n > 0
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 2)(:key4 4)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (A  B :KEY3 (44) (0 1 2 3 :KEY4 0.95)) :KEY5 (xxx)) :return-list-p T )) ;; :recurse-for-key-p T))
;;works= (A B :KEY3 (44) (0 1 2 3 :KEY4 NEW-VALUE))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (A B :KEY3 (44) (0 1 2 3 :KEY4 NEW-VALUE)) :KEY5 (XXX))    (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)   T
;; keyloc-n = T, keys not nested
;; IF KEY4 NOT NESTED IN SAME LIST AS KEY3, then must be specified in key3's VAL-NTH and include :NEXT-KEY-IN-VALUE as 4th key-spec item.
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:KEY3 T 2 :next-key-in-value)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL))
;;works= (:KEY4 NEW-VALUE)    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE) :KEY5 (XXX)) (44) (:KEY4 0.95) :KEY5 (XXX))    (:KEY4 NEW-VALUE)     NEW-VALUE    (:KEY4 0.95)     T
;; keyloc-n = T, keys nested, set/not append.
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:KEY3 T)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) ( :KEY3 (44) (:KEY4 0.95) :key6 'this) :key5 (xxx)) :append-value-p NIL))
;;works=  (:KEY4 NEW-VALUE)    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) (:KEY3 (44) (:KEY4 NEW-VALUE) :KEY6 (QUOTE THIS)) :KEY5 (XXX))    (:KEY4 NEW-VALUE)    NEW-VALUE   (:KEY4 0.95)   T
;;   :return-list-p :append-value-p with above  keyloc-n = T, keys nested
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:KEY3 T)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) ( :KEY3 (44) (:KEY4 0.95) :key6 'this) :key5 (xxx)) :append-value-p T :return-list-p T))
;;works= (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE)) :KEY6 (QUOTE THIS))     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE)) :KEY6 (QUOTE THIS)) :KEY5 (XXX))      (:KEY4 (0.95 NEW-VALUE))     (0.95 NEW-VALUE)    (:KEY4 0.95)   T

;; If KEY = T, keyloc-n = 0; key-spec = (T 0)
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((T 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) ( (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)) :key5 (xxx)) :append-value-p NIL))
;;works= (:KEY4 NEW-VALUE)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) ((:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE)) :KEY5 (XXX))   (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)   T
;; :GET
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  :GET '((:KEY3 t)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL))
;;works= (:KEY4 0.95)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))  (:KEY4 0.95)  0.95  (:KEY4 0.95)  T
;; :get with return-list-p
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  :GET '((:KEY3 t)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL :return-list-p T))
;;works= ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))   (:KEY4 0.95)   0.95  (:KEY4 0.95)   T

;; if key not found, append
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key2 0) (:KEY7  0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL))
;;work=  (:KEY7 NEW-VALUE)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX) (:KEY7 NEW-VALUE))    (:KEY7 NEW-VALUE)    NEW-VALUE    NIL   NIL
;;
;;(progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key2 0) (:KEY7  0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL :if-not-found-append-keyvalue-list-p NIL))
;;works= (:KEY7 NEW-VALUE)    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX) :KEY7 NEW-VALUE)   (:KEY7 NEW-VALUE)   NEW-VALUE   NIL   NIL

;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 A)) :KEY5 (xxx)) :append-value-p t))
;;works=  (:KEY4 (A NEW-VALUE))     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) (:KEY4 (A NEW-VALUE))) :KEY5 (XXX))     (:KEY4 (A NEW-VALUE))     (A NEW-VALUE)    (:KEY4 A)    T
;;with items  after value, key3 = T
;;(progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 A B C D E)) :KEY5 (xxx)) :append-value-p t)))
;;works= (:KEY4 (A NEW-VALUE) B C D E)     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) (:KEY4 (A NEW-VALUE) B C D E)) :KEY5 (XXX))             (:KEY4 (A NEW-VALUE) B C D E)            (A NEW-VALUE)         (:KEY3 (44) (:KEY4 A B C D E))       T
;;nested keys
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T )(:key4 1)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44 (:KEY4 0.95))  :KEY5 (xxx)) :append-value-p t))
;;works= (:KEY4 (0.95 NEW-VALUE))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44 (:KEY4 (0.95 NEW-VALUE))) :KEY5 (XXX))   (:KEY4 (0.95 NEW-VALUE))    (0.95 NEW-VALUE)   (:KEY4 0.95)   T
;;no append/replace old-value
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL))
;;works=(:KEY4 NEW-VALUE)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE) :KEY5 (XXX))  (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)  T
;;If key = T
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((T 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL))

;;ORIG TESTS
;; append
;;(progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (xxx)) :append-value-p t))
;; works= (:KEY4 (0.95 NEW-VALUE))     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 (0.95 NEW-VALUE)) :KEY5 (XXX))    (:KEY4 (0.95 NEW-VALUE))    (0.95 NEW-VALUE)   (:KEY4 0.95)   T
;;no append/replace old-value
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL))
;;works=(:KEY4 NEW-VALUE)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE) :KEY5 (XXX))  (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)  T
;;If key = T
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((T 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL))
;;works=  (:KEY4 NEW-VALUE)        ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE) :KEY5 (XXX))          (:KEY4 NEW-VALUE)                NEW-VALUE          ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))           T
;;return-list-p NIL
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95)) :KEY5 (xxx)) :append-value-p t) :return-list-p T)
;;works= (:KEY4 (0.95 NEW-VALUE))       ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE))) :KEY5 (XXX))        (:KEY4 (0.95 NEW-VALUE))       (0.95 NEW-VALUE)     (:KEY3 (44) (:KEY4 0.95))     T
;;return-list-p T
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95)) :KEY5 (xxx)) :append-value-p t :return-list-p T))
;;works= (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE)))          ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE))) :KEY5 (XXX))     (:KEY4 (0.95 NEW-VALUE))     (0.95 NEW-VALUE)    (:KEY3 (44) (:KEY4 0.95))    T
;; ;;return-list-p T with added
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 2)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 0.95 end1 end2) post3) :KEY5 (xxx)) :append-value-p t :return-list-p T))
;; works= (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2) POST3)             ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2) POST3) :KEY5 (XXX))                    (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2)              (0.95 NEW-VALUE)                   (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3)              T

;;  keyloc-n = T, val-nth 2, where next key is in the top list in value slot
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 a b (c) d) :key5 (xxx)) :return-list-p NIL :append-value-p NIL))
;;works=  (:KEY4 NEW-VALUE A B (C) D)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE A B (C) D) :KEY5 (XXX))   (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)   T
;; when return-list-p returns whole nested-list with new value
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 a b (c) d) :key5 (xxx)) :return-list-p T :append-value-p NIL))
;;works= ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE A B (C) D) :KEY5 (XXX))                        ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE A B (C) D) :KEY5 (XXX))                 (:KEY4 NEW-VALUE A B (C) D)                 NEW-VALUE    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 A B (C) D) :KEY5 (XXX))       T
;;
;;TRY SETTING VAL-NTH WITH :NTH
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2 :NEXT-KEY-IN-VALUE)(:NTH  2)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 a b (c) d) :key5 (xxx)) :return-list-p NIL :append-value-p NIL))
;; NOTE: MUST USE :RETURN-LIST-P TO GET FULL NEW KEYLIST W/END
;;works= (:KEY4 0.95 NEW-VALUE)  ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 NEW-VALUE B (C) D) :KEY5 (XXX))    (NEW-VALUE)    NEW-VALUE   (:KEY4 0.95 A B (C) D)    T
;;works= (:KEY4 0.95 NEW-VALUE)              ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 NEW-VALUE B (C) D) :KEY5 (XXX))           (NEW-VALUE)            NEW-VALUE   (:KEY4 0.95 A B (C) D)   T
;;
;; NTH with :RETURN-LIST-P
;;   (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2 :NEXT-KEY-IN-VALUE)(:NTH  2)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 a b (c) d) :key5 (xxx)) :RETURN-LIST-P t :append-value-p NIL))
;;works= (:KEY4 0.95 NEW-VALUE B (C) D)             ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 NEW-VALUE B (C) D) :KEY5 (XXX))            (NEW-VALUE)     NEW-VALUE       (:KEY4 0.95 A B (C) D)    T

;;GET
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  :GET '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-value-p NIL))
;; works= (:KEY4 0.95)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))    (:KEY4 0.95)    0.95   (:KEY4 0.95)  T

;; IF NO LAST KEY FOUND AND IF-NOT-FOUND-APPEND-KEY-VALUE-P
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:keyX 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (xxx)) :append-value-p t  :if-not-found-append-key-value-p T))
;;works=  (:KEYX NEW-VALUE)      ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX) (:KEYX NEW-VALUE))          (:KEYX NEW-VALUE)           NEW-VALUE           NIL         NIL
;; NO KEY FOUND FOR :GET
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  :GET '((:key3 T 2)(:keyX 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (xxx)) :append-value-p t  :if-not-found-append-key-value-p T))
;; works (:keyx not in list)=  NIL  ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) NIL :KEY5 (XXX))   NIL   NIL   NIL  NIL
;;
;;  
;; 2 equal higher-keys A3, must get first key :PC-VALUES right first
;; Also, double-list with NO key ((A3....)(A4 ...))
;;  (progn (setf out nil) (get-set-append-delete-keyvalue-in-nested-list 'NEW-VALUE  '((:PC-VALUES T)(A3 T)) '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)(A4 "this is A4") x y) (list z))))
;;works= (A3 NEW-VALUE)      (:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 NEW-VALUE :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12) (A4 "this is A4") X Y) (LIST Z))      (A3 NEW-VALUE)   NEW-VALUE    (A3 "a vs not a")   T




;;xxx
;;FOR TEST ON CSQ-RELATED SEARCHES ==================
;; (get-keyvalue-in-nested-list  (list (list T  0) (list 'mother  0)) (eval *pce-test-list) :return-list-p T)

;;WORKS
;;SEARCH ALL LISTS--time consuming BUT WORK & SIMPLE SPEC-LIST
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  :get  (list '((PCE-PEOPLE  (list 'motherQ  T))  *pce-test-list :return-list-p T :append-value-p t :RECURSE-FOR-KEY-P T)) 
;;works= (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)   [REST OF VALUES WORK TOO]
;;
;;FOR APPEND NEW-VALUE
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE (list  (list 'motherQ  T))  *pce-test-list :return-list-p T :append-value-p t :RECURSE-FOR-KEY-P T))
;;works= 
#|(MOTHERQ ("Your Mother [or person most like a mother to you]" NEW-VALUE) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)
((PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]" NEW-VALUE) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)   ETC )
(MOTHERQ ("Your Mother [or person most like a mother to you]" NEW-VALUE))
("Your Mother [or person most like a mother to you]" NEW-VALUE)
(MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)   T|#
;; VAL-NTH 3
;;  (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE (list  (list 'motherQ  T 3))  *pce-test-list :return-list-p T :append-value-p t :RECURSE-FOR-KEY-P T))
#|works=  (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR (*INPUT-BOX-INSTRS NEW-VALUE))
((PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR (*INPUT-BOX-INSTRS NEW-VALUE))  ETC )
(MOTHERQ (*INPUT-BOX-INSTRS NEW-VALUE))
(*INPUT-BOX-INSTRS NEW-VALUE)
(MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)   T|#
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  'NEW-VALUE (list  (list 'motherQ  0))  *pce-test-list :return-list-p T :append-value-p t :RECURSE-FOR-KEY-P T)) = DOESN'T WORK


;;  (setf  *pce-test-list ' ((PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ ("A best male friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-F-FRIENDQ ("A best female friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)  (DIS-TEACHERQ ("A teacher you disliked the most") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS))                       (PCE-GROUPS (PCE-GROUPS-INSTR ("Significant Groups" *INSTR-NAME-ELEMENT)) (TEACHERQ ("Teachers") PCE-GROUPS-INSTR *INPUT-BOX-INSTRS) (POLICEMANQ ("Policepersons") PCE-GROUPS-INSTR *INPUT-BOX-INSTRS)  PCE-GROUPS-INSTR *INPUT-BOX-INSTRS)                        (PCE-SELF (PCE-SELF-INSTR ("Important Self-Related" *INSTR-NAME-ELEMENT)) (MOST-IMPORTANT-VALUEQ ("Your most important goal or value") PCE-SELF-INSTR *INPUT-BOX-INSTRS) (MOST-IMPORTANT-ABILITYQ ("Your most important ability") PCE-SELF-INSTR *INPUT-BOX-INSTRS)  (YOUR-WORST-CHARACTERISTICQ ("Your worst characteristic") PCE-SELF-INSTR *INPUT-BOX-INSTRS PCE-SELF-INSTR *INPUT-BOX-INSTRS))))

;;  :return-list-p T
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  :get (list (list 'PCE-PEOPLE 0) (list 'motherQ  T))  *pce-test-list :return-list-p T :append-value-p t :RECURSE-FOR-KEY-P T))
;; works=  note: :return-list-p T
#|(PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ ("A best male friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-F-FRIENDQ ("A best female friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (DIS-TEACHERQ ("A teacher you disliked the most") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)) 
;;  :return-list-p NIL
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  :get (list (list 'PCE-PEOPLE 0) (list 'motherQ  T))  *pce-test-list :RETURN-LIST-P NIL :append-value-p t :RECURSE-FOR-KEY-P T))
;; works= (MOTHERQ ("Your Mother [or person most like a mother to you]"))          ((PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) ..... ETC WHOLE LIST HERE          ) PCE-SELF-INSTR *INPUT-BOX-INSTRS PCE-SELF-INSTR *INPUT-BOX-INSTRS)))
(MOTHERQ ("Your Mother [or person most like a mother to you]"))
("Your Mother [or person most like a mother to you]")
(MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)   T
|#


;;THESE WON'T WORK
;;  Reason: For :next-key-in-value to work must be a LIST of lists or keys in val-nth place--only looks in that one place.
;; (progn (setf out nil)(get-set-append-delete-keyvalue-in-nested-list  :get (list (list 'PCE-PEOPLE 0 1  :next-key-in-value) (list 'motherQ  T))  *pce-test-list :return-list-p T :append-value-p t :RECURSE-FOR-KEY-P T))
;;
;;SSS TEST HERE
;; (get-set-append-delete-keyvalue-in-nested-list :get  (list (list 'pce-people 0 :R)                                              (list 'motherq  0 :R))      (eval  *cur-all-questions))  :return-list-p T)
;;works= MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)                    ("return-nested-list-p=NIL")              (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)             ("Your Mother [or person most like a mother to you]")                (PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ ("A best male friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)  etc   ....   etc   (FAV-SPIRITUALQ ("A favorite spiritual or inspirational person") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (DIS-TEACHERQ ("A teacher you disliked the most") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS))    T
;;
;;NOTE; return-list-p = T  not needed, returns too much
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-nested-list :get  (list (list T 0) (list 'motherq  0 :R))      (eval  *cur-all-questions)))
;;works= (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)                ("return-orig-nested-list-p=NIL")                    (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)               ("Your Mother [or person most like a mother to you]")     (PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)  ETC  ETC    (FAV-TEACHERQ ("A favorite teacher") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FAV-SPIRITUALQ ("A favorite spiritual or inspirational person") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (DIS-TEACHERQ ("A teacher you disliked the most") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS))       T


;;MUST HAVE :R when multiple lists to be searched at same level?
;; (get-keyvalue-in-nested-list (list (list t 0 )(list 'PCE-PEOPLE-INSTR 0 :R) ) (eval *cur-all-questions)) 
;; works= ("People Important To You" *INSTR-NAME-ELEMENT)   (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT))              (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT))              ("return-orig-nested-list-p=NIL")          T

;;TESTING FOR ADDING KEY & VALUE WHEN KEY NOT FOUND
;; when (null if-not-found-add-item-in-last-nested-list-p)
;; (setf *test-yyylist '(:key1 (a :key2  b  (a b c d (1 2 3 4) e f) mm nn oo pp)))
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-nested-list "NEW LOCATION"  '((:key1 0)(:key2 1 2)(:location T)) *test-yyylist      :append-value-p t   :add-value-p NIL     :return-list-p  T     :return-nested-list-p T          :max-list-length 1000        :if-not-found-append-key-value-p T       :if-not-found-append-keyvalue-list-p nil      :bottom-level-p t    :put-key-after-items NIL                                                   :put-value-after-items NIL      :new-begin-items NIL    :new-end-items NIL            :splice-key-value-in-list-p NIL     :splice-old-new-values-p T     :recurse-for-key-p NIL      :last-key-found-p nil))
;;works= (:LOCATION "NEW LOCATION")       ((:KEY1 (A :KEY2 B (A B C D (1 2 3 4) E F) MM NN OO PP)) :LOCATION "NEW LOCATION")    (:LOCATION "NEW LOCATION")   "NEW LOCATION"    NIL   NIL
;;when if-not-found-add-item-in-last-nested-list-p
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-nested-list "NEW LOCATION"  '((:key1 0)(:key2 1 2)(:location T)) *test-yyylist      :append-value-p t   :add-value-p NIL     :return-list-p  T     :return-nested-list-p T          :max-list-length 1000   :if-not-found-add-item-in-last-nested-list-p T     :if-not-found-append-key-value-p T       :if-not-found-append-keyvalue-list-p nil      :bottom-level-p t    :put-key-after-items NIL         :put-value-after-items NIL      :new-begin-items NIL    :new-end-items NIL            :splice-key-value-in-list-p NIL     :splice-old-new-values-p T     :recurse-for-key-p NIL      :last-key-found-p nil))


;;MAKE-ROOTNS-KEY-FROM-ARTDIMS
;;2019
;;ddd
(defun make-rootns-key-from-artdims (artdims+pre)
  "U-lists  RETURNS get-set-rootns   INPUT: a list of keys to search L-R = TOP-BOTTOM. Converts each to a (key T) sublist for get-set-keylists"
  (let
      ((get-set-rootn-keys)
       (keysn (list-length artdims+pre))
       ;;for old (new-artdims+pre (append (list (first artdims+pre)) (reverse (cdr artdims+pre))))
       )
    (loop
     for n from (- keysn 1) downto 0
     do
     (let*
         ((keylist (list (butlast artdims+pre n) T))
          )
       (setf get-set-rootn-keys (append get-set-rootn-keys (list keylist)))     
       ;;end let,loop
       ))
     get-set-rootn-keys
    ;;end let, make-rootns-key-from-artdims
    ))
;;TEST
;; (make-rootns-key-from-artdims '(CS HS 1 2 3 4 ))
;;works= (((CS) T) ((CS HS) T) ((CS HS 1) T) ((CS HS 1 2) T) ((CS HS 1 2 3) T) ((CS HS 1 2 3 4) T))



;;MAKE-GET-SET-DELETE-KEYLIST-FROM-KEYS
;;2019
;;ddd
(defun make-get-set-delete-keylist-from-keys (keys &key prefix
                                                   reverse-keys-p)
  "U-lists  RETURNS get-set-keylist   INPUT: a list of keys to search L-R = TOP-BOTTOM. Converts each to a (key T) sublist for get-set-keylists"
  (let
      ((get-set-keylists)
       )
    (cond
     ((or (symbolp keys) (stringp keys))
      (setf get-set-keylists (list (list keys))))
     (T
      (when  prefix
        (setf keys (append (list prefix) keys)) )
      (when reverse-keys-p (setf keys (reverse keys)))
      (loop
       for key in keys
       do
       (let*
           ((keylist (list key T))
            )
         (setf get-set-keylists (append get-set-keylists (list keylist)))     
         ;;end let,loop
         ))))    
     get-set-keylists
    ;;end let, make-get-set-delete-keylist-from-keys
    ))
;;TEST
;; (make-get-set-delete-keylist-from-keys '(k1  k2  1 2 3))
;; works= ((K1 T) (K2 T) (1 T) (2 T) (3 T))
;; (make-get-set-delete-keylist-from-keys '( HS 1 2 3 4 ) :prefix 'CS)
;; works= ((CS T) (HS T) (1 T) (2 T) (3 T) (4 T))
;; (make-get-set-delete-keylist-from-keys '(1 2 3 4 5) :reverse-keys-p T)
;; works= ((5 T) (4 T) (3 T) (2 T) (1 T))




;;GET-SET-APPEND-DELETE-KEYVALUE
;;2018-01-26, 2019-07
;;ddd
(defun get-set-append-delete-keyvalue (key  new-value   ;;second was old-value
                                       &key (keyloc-n T) ;;2019-06 changed from 0 
                                       (val-nth 1) (test 'my-equal)
                                       old-keylist
                                       append-value-p  
                                       list-first-item-in-append-p ;;NEW
                                       add-value-p       (delete-old-nil-p T) ;;added 2020
                                       simple-splice-key-value-in-keylist-p
                                       splice-newvalue-at-nth-p ;;used with add-value--p
                                       put-key-after-items
                                       add-nils-p ;;use to fill space before key with nils if needed
                                       put-value-after-items
                                       new-begin-items
                                       new-end-items
                                       parens-after-begin-items-p
                                       splice-key-value-in-list-p
                                       break-if-keys-not-match-p
                                       splice-old-new-values-p
                                       not-duplicate-p ;;added 2019-10
                                       old-key&value  
                                       )
  "In U-lists, if new-value = :get, just returns old-value. Otherwise replaces or appends old value.  If listp old:- value, appends list; if not sets value to (list old-value new-value). RETURNS (values return-keylist new-keylist return-value old-value old-key&value). 
  When KEYLOC-N=T or NIL or > old-keylist length, finds keyloc-n if key present or sets to list length if not.
  When KEYLOC-N > 0, If  PUT-KEY-AFTER-ITEMS is a list, puts put-key-after-items  before key. If a single object, puts keyloc-n objects key in keylist. If SPLICE-KEY-VALUE-IN-LIST-P, splices key and new-value at keyloc-n in that list which becomes the new-keylist. If SPLICE-OLD-NEW-VALUES-P, then makes a keylist that contains the old and new value(s) in same list even if the new value is list (it then appends the list to the old-value list (eg (old-value 1 2 new-value x y) instead of adding it to the old-value list as a list eg (old-value 1 2 (new-value x y)),.  RETURN-KEYLIST is the list possibly modified by outside items. The NEW-KEYLIST is the innermost keylist.
   If splice-key-value-in-list-p length < prev-valloc-n, then fills places with  put-key-after-items object to make new-keylist. If KEY = :NTH (keyloc-n becomes the number for nth and val-nth=0, then just ignores all keys and operates on old-value--otherwise like above. NOTE: Used by nesting functions to get, set, or append values in FINAL NESTED LIST. If want filled with NIL, put :NIL in put-... 
  ADD-VALUE-P adds new-value after old-value without being in a list. 
  only with add-value-p, SPLICE-NEWVALUE-AT-NTH-P = eg (:k1 1 :k2 2 :newkey newval) instead of (:k1 1 :k2 2  (:newkey newval))
 TO APPEND OLD-VALUE LIST WITH NEW-VALUE LIST, MUST USE BOTH  :APPEND-VALUE-P  and :SPLICE-OLD-NEW-VALUES-P
  APPEND-VALUE-P LEAVES OLD-VALUE (unless old-value= NIL)   Puts new-value in AS A LIST with old-value (even if old-value wasn't a list). [Note: IF OLD-KEYLIST, then don't need key or old-value.]  USE OLD-KEYLIST WITH NO-KEY .  With append-value-p & LIST-FIRST-ITEM-IN-APPEND-P causes ((1)(2)(3)..) instead of (1 (2)(3)) if 1 was 1 or (1).
  The arg BEGIN-ITEMS only works if there are no items before the key and END-ITEMS only works if there are no items after the value (in the old-keylist).  
  PUT-KEY-AFTER-ITEMS is a list of items to put before key.   ADD-NILS-P used to fill space before key with nils if needed.
Also PUT-VALUE-AFTER-ITEMS only works if there are no items between the key and value in the old-keylist.
   NOTE: re IF KEY NOT EQUAL item at keyloc-n in old-keylist, normally INSERTS the arg key for the item at that location.UNLESS BREAK-IF-KEYS-NOT-MATCH-P, then creates a warning or break.
   When new-value= :SET-KEYLIST=NIL,RETURNS :SET-KEYLIST=NIL :DELETE-VALUE, sets old-value to NIL?. When new-value= :DELETE-KEY&VALUE, deletes both."  
  (let
      ((new-keylist)
       (return-keylist)
       (return-value)
       (splice-list-n)
       (put-n 0)
       (add-n 0)
       (key+val-n  0) 
       (old-key )
       (old-value)
       (len-old-keylist (cond (old-keylist (list-length old-keylist))
                              (t (break "old-keylist"))))
       (begin-items)
       (end-items)
       (new=old-val-do-not-add-p)
       )
    ;;PRE-PROCESSING
    ;;WHEN KEYLOC-N = T OR GREATER THAN OLD-KEYLIST LENGTH
    (when  (or (equal keyloc-n T)(null keyloc-n)
               (> keyloc-n len-old-keylist))
      (setf  keyloc-n  (find-item-n key old-keylist))
      (when (null keyloc-n)
        (setf keyloc-n  len-old-keylist)))

    ;;for splice-old-new-values-p to work
    (when splice-old-new-values-p 
      (setf append-value-p T))

     ;;(BREAK "get-set-append-delete-keyvalue")

    ;;WHEN :NTH
    (cond
     ((equal key :NTH)
      (setf val-nth 0  
            key+val-n (+ keyloc-n val-nth)
            old-value (nth keyloc-n old-keylist)))
     ;;otherwise if numberp
     ((and (numberp keyloc-n)(numberp val-nth))
      (setf  key+val-n  (+ keyloc-n val-nth)
             old-key (nth keyloc-n old-keylist))
      (cond
       ((not (funcall test key old-key))
        (cond
         ((null break-if-keys-not-match-p )
          (setf old-keylist (append-nth  key keyloc-n old-keylist)
                old-keylist (append-nth NIL key+val-n old-keylist)
                len-old-keylist (list-length old-keylist)))
         (t (break (format nil "ERROR:  OLD-KEY= ~A does NOT match KEY= ~A" old-key key))))
        ;;end (not (equal key old-key))
        )
       (t  NIL))       
      ;;end (and (numberp keyloc-n)(numberp val-nth))
      )   
     ;;IF KEYLOC-N or VAL-NTH NOT A NUMBER find them if there is a key anywhere on that level.
     ((listp old-keylist)
      (loop
       for item in old-keylist
       for n from 0 to len-old-keylist 
       do
       (when  (funcall test item key) ;;was (my-equal item key)
         (setf keyloc-n n
               old-key item)
         (unless (numberp val-nth)
           (setf val-nth 1))   ;;WAS-ERROR? (+ keyloc-n 1)))
         (setf key+val-n  (+ keyloc-n val-nth)
               len-old-keylist (list-length old-keylist))
         (when (> len-old-keylist key+val-n)  ;;was >=
           (setf old-value (nth key+val-n old-keylist)))
         (return))              
       ;;end loop
       )
      ;;end (listp old-keylist)
      )
     (t nil))

    ;;WHEN OLD-KEYLIST 
    (cond
     (old-keylist 
      (when (>= len-old-keylist key+val-n)
        (setf old-value (nth key+val-n old-keylist)))
      ;;not needed herer?? added 2018 SSS TEST THIS list-first-item-in-append-p
      #|      (when (and list-first-item-in-append-p
                 (or (not (listp old-value))
                     (= (list-length old-value) 1)))
        (setf old-value (list (list old-value))))|#
      ;;(break "old-value")
      ;;CHECK FOR NEW-VALUE DUPLICATES?
      (when (and old-value not-duplicate-p)
        (when (or (my-equal new-value old-value)
                  (and (listp old-value)(member new-value old-value :test 'my-equal)))
          (setf new=old-val-do-not-add-p T)))
        
      ;;(BREAK "OLD-VALUE")
      (unless new=old-val-do-not-add-p
        (when  (and (> keyloc-n 0)(< keyloc-n len-old-keylist))
          (setf begin-items (subseq old-keylist 0  keyloc-n )))  ;;was keyloc-n)))
        (when (and (> val-nth 1) (not (equal key :nth)))
          (setf put-value-after-items (subseq old-keylist  (+ keyloc-n 1)  key+val-n)))
        (when (and (>  len-old-keylist (+ key+val-n 1)) (not (equal key :nth))) ;;added nth    
          (setf end-items (subseq old-keylist  (+ key+val-n 1))))
        ;;(break "old-value2")    
        ;;was -caused error if no old key (setf end-items (subseq old-keylist (+ key+val-n 1) len-old-keylist)))
        ;;was below, but leave old-value = nil if there is none
        #|      (when (null old-value) 
        (setf old-value (nth  (+ keyloc-n val-nth) old-keylist)))|#
        ))
     ;;OLD-KEYLIST = NIL, therefore CREATES ENTIRELY NEW KEYLIST
     ;;If no old-keylist, no old-value
     (t
      (when put-value-after-items
        ;;(when (equal key :NTH)
        (setf begin-items (append begin-items put-value-after-items)))))
    ;;(break "after if old-keylist")

    ;;STEP 1: MAKE NEW-KEYLIST
    (cond
     ;;1. FOR DELETES
     ((equal new-value :set-keylist=nil)
      (setf new-keylist :set-keylist=nil
            begin-items nil  end-items nil))
     ((equal new-value :delete-value)
      (cond
       ((equal key :NTH)
        (setf new-keylist nil))
       (t (setf new-keylist (list key))))
      (when (< (+ keyloc-n 1) key+val-n)
        put-value-after-items (subseq old-keylist (+ keyloc-n 1) key+val-n)) ;;was (butlast old-keylist (- len-old-keylist key+val-n)))
      ;;(break "new-keylist")
      ) ;;WAS (delete-nth old-keylist key+val-n )))
     ((equal new-value :DELETE-KEY&VALUE) ;;2020 
      ;;(break ":delete-key&value")
      (setf new-keylist (append begin-items end-items)
            ;;not work? new-keylist NIL
            old-key&value (list old-key old-value)) )
      ;;      (setf new-keylist (delete-nth (+ keyloc-n 1) old-keylist)
       ;;  new-keylist  (delete-nth keyloc-n new-keylist)       
     ;; (butlast old-keylist (- len-old-keylist key+val-n))
     ;;was new-keylist (delete-nth keyloc-n new-keylist )))
     ;;2. FOR GET
     ((equal new-value :GET)
      (setf  new-keylist old-keylist)
      #|       (when (and (equal key :NTH) old-value)
          (setf  new-keylist (list old-value)))|#
      ;;also
      (setf return-value  old-value)
      ;;end get
      )
     ;;3. WHEN OLD=NEW VAL & NOT-DUPLICATE-P, DO NOTHING ELSE
     (new=old-val-do-not-add-p
      (setf return-value old-value
            new-keylist old-keylist)
      ;;(break "new=old-val-do-not-add-p")
      )
     ;;4. SPLICE-OLD-NEW-VALUES-P
     (SPLICE-OLD-NEW-VALUES-P        
        (cond
         (old-value
          ;;added
          (cond
           ((and  (listp old-value) (listp new-value))
            (setf  return-value (append  old-value  new-value)
                   new-keylist (append  (list key)  (list return-value))))
           ((listp new-value)
            (setf  return-value (append  (list old-value)  new-value)
                   new-keylist (append  (list key)  (list return-value))))
           ((listp old-value) ;;2020
            (setf  return-value (append  old-value (list new-value))
                   new-keylist (append  (list key)  (list return-value)))
            ;;(break "1-new-keylist")
            )
           (t
            (setf  return-value (list old-value  new-value)
                   new-keylist (append  (list key)  put-value-after-items
                                        return-value))))
          ;;end old-value
          )
         (t (setf return-value (list  new-value)
                  new-keylist (append  (list key)  put-value-after-items  return-value))))
        ;;(break "end splice-old-new-values-p")
        ;;end splice-old-new-values-p
        )
     ;;4. FOR APPEND-VALUE-P  
     ;;ALSO HERE list-first-item-in-append-p
     (append-value-p  ;;was (and append-value-p (not (equal key :NTH)))
                      (cond
                       ;;OLD-VALUE IS LIST OR NIL 
                       ((listp old-value) 
                        (cond
                         ((or (null not-duplicate-p)
                              (null (my-equal-item-list-or-member new-value old-value
                                                                  :test test)))
                          ;;(break "new vs old")
            ;OLD-VALUE IS A NON-NIL LIST
                          #|    old   ((and old-value (listp old-value)) ;;was just (listp old-value)
        ;;list-first-item-in-append-p  added 2018 
        (when (and list-first-item-in-append-p
          (= (list-length old-value) 1))
          (setf old-value (list old-value)))|#
                          (cond
                           (put-value-after-items
                            (cond
                             ((equal key :NTH)                            
                              (setf  return-value (append old-value  (list new-value))
                                     new-keylist (append  put-value-after-items return-value )))
                             ;;make key plus  a list of value(s) eg.  key  (old-value new-value)
                             (splice-old-new-values-p
                              ;;(break "splice-old-new-values-p")
                              (cond
                               ;;NEW-VALUE IS A LIST
                               ((listp new-value)
                                (setf  return-value (append old-value  new-value)
                                       new-keylist (append  (list key)  put-value-after-items (list return-value))))
                               ;;new-value not a list
                               (t (setf  return-value (append old-value  (list new-value))
                                         new-keylist (append  (list key)  put-value-after-items
                                                              (list return-value)))))
                              ;;(break "splice-old-new-values-p, LIST")
                              )
                             ;;splice-old-new-values-p = NIL
                             ;;make key plist value(s) not in a list
                             (t (setf  return-value (list (append  old-value new-value))
                                       new-keylist (append  (list key)  put-value-after-items (list return-value))))) ;;2017-07      
                            ;;end put-value-after-items
                            )
                           ;; put-value-after-items = NIL
                           (t
                            (cond
                             ;;KEY= :NTH
                             ((equal key :NTH)
                              (setf  return-value (append old-value  (list new-value))
                                     new-keylist (append  put-value-after-items return-value)))
                             ;;make key plus  a list of value(s) eg.  key  (old-value new-value)
                             ;;SPLICE-OLD-NEW-VALUES-P
                             (splice-old-new-values-p                            
                              (cond
                               ;;NEW-VALUE IS A LIST
                               ((listp new-value)
                                (setf  return-value (list (append old-value  new-value))
                                       new-keylist (append  (list key) (list return-value )) ))
                               ;;new-value not a list
                               (t (setf  return-value (list (append old-value  (list new-value)))
                                         new-keylist (append  (list key) return-value)))))
                             ;;SPLICE-OLD-NEW-VALUES-P = NIL
                             ;;make key plust value(s) not in a list
                             (t (setf  return-value (list  old-value new-value)
                                       new-keylist (append  (list key)  (list return-value )))))))
                          ;;END NO PROBLEM WITH DUPLICATE
                          )                         
                         ;;IF DUPLICATE AND NOT-DUPLICATE-P
                         ;;I THINK I FIXED IT
                         ;;PROBLEM = (setf $KNW (get-set-append-delete-keyvalue-in-nested-list 'test  '(:S) $KNW :splice-old-new-values-p T  :not-duplicate-p T)) =
                         ;;WRONG= ("$KNW" "Knowledge" NIL NIL NIL :S $NSC $SSC $BSC $ART $BUS $SPT $REC TEST :CLEV 1 1)
                         ;;SHOULD BE= ("$KNW" "Knowledge" NIL NIL NIL :S ($NSC $SSC $BSC $ART $BUS $SPT $REC TEST) :CLEV 1 1)
                         (T
                          (setf  return-value old-value
                                 new-keylist (append  (list key)  old-value))))
                        ;;2018 added (list
                        (break "end old-value is a list")
                        ;;end  OLD-VALUE IS A LIST
                        )
                       ;;OLD VALUE NOT A LIST
                       (T
                        ;;added 2018
                        (when  list-first-item-in-append-p                     
                          (setf old-value (list old-value)))
                        ;;NOT DUPLICATE or null not-duplicate-p
                        (cond
                         ((or (null not-duplicate-p)
                              (null (my-equal-item-list-or-member old-value new-value
                                                                  :test test)))
                          (cond
                           (put-value-after-items
                            ;;(break "old-value= nil, put-value-after-items=")
                            ;;(afout 'out (format NIL "old-value= ~A put-value-after-items= ~A" old-value put-value-after-items))
                            (cond
                             ((equal key :NTH)
                              (cond 
                               (old-value
                                (setf  return-value (list old-value new-value)
                                       new-keylist (append  put-value-after-items  return-value)))
                               (t (setf  return-value  new-value
                                         new-keylist (append  put-value-after-items  (list return-value))))))
                             ;;make key plus  a list of value(s) eg.  key  (old-value new-value)
                             ((null splice-old-new-values-p)
                              (cond 
                               (old-value
                                (setf  return-value  (list old-value  new-value)
                                       new-keylist (append (list key)  put-value-after-items (list return-value))))
                               (t (setf  return-value  (list   new-value)
                                         new-keylist (append (list key)  put-value-after-items (list return-value))))))
                             ;;splice-old-new-values-p
                             ;;make key plust value(s) not in a list
                             (T
                              (cond 
                               (old-value
                                (setf  return-value (list old-value  new-value)
                                       new-keylist (append (list key)  put-value-after-items  return-value)))
                               (t (setf  return-value (list new-value)
                                         new-keylist (append (list key)  put-value-after-items  return-value))))))
                            ;;(break "splice-old-new-values-p, NOT-LIST")
                            ;;end put-value-after-items
                            )
                           ;;NOT 
                           (T
                            (cond
                             ((or old-value (null delete-old-nil-p))  ;;HERENOW
                              (setf return-value  (list old-value  new-value)
                                    new-keylist (list key  return-value)))
                             (t (setf return-value  new-value
                                      new-keylist (list key return-value) )))))
                          ;;end NO DUPLICATE PROBLEM
                          )
                         ;;DUPLICATE PROBLEM
                         (T (setf return-value  old-value
                                  new-keylist (list key return-value) )))))
                      ;;END APPEND-VALUE-P
                      )
                      ;;later??(when begin-items (setf new-keylist (append begin-items new-keylist)
     ;;5. FOR ADD-VALUE-P
     (add-value-p 
      (cond
       ;;4.1. KEY= NTH
       ((equal key :NTH)
        (cond
         (splice-newvalue-at-nth-p  ;;here now
                                    (setf return-value  new-value;;eg (:newkey newval)
                                          ;;is keyloc-n right?
                                          new-keylist (append-nth new-value keyloc-n old-keylist :splice-list-p T ))
                                    ;;(break "splice-list-p")
                                    ;;was new-keylist return-value)
                                    (when put-value-after-items
                                      (setf new-keylist (append put-value-after-items new-keylist))))
         (old-value
          (setf  return-value (list  old-value  new-value)
                 new-keylist return-value ))
         ;;splice-newvalue-at-nth-p (eg (k1 1 k2 2 :newkey newval)
              
         ;;for non-splice add-value-p     
         (t (setf return-value (list new-value)
                  new-keylist (append-nth new-value keyloc-n old-keylist :splice-list-p NIL))))
        ;;was new-keylist return-value)))
        (when put-value-after-items
          (setf new-keylist (append put-value-after-items new-keylist)))
        ;;end key nth
        )
       ;;4.2 PUT-VALUE-AFTER-ITEMS
       (PUT-VALUE-AFTER-ITEMS             
        (cond
         (old-value
          ;;added
          (cond
           ((and SPLICE-OLD-NEW-VALUES-P (listp old-value) (listp new-value))
            (setf  return-value (append  old-value  new-value)
                   new-keylist (append  (list key)  put-value-after-items
                                        return-value)))
           ((and splice-old-new-values-p (listp new-value))
            (setf  return-value (append  (list old-value)  new-value)
                   new-keylist (append  (list key)  put-value-after-items
                                        return-value)))
           (t
            (setf  return-value (list old-value  new-value)
                   new-keylist (append  (list key)  put-value-after-items
                                        return-value))))
          ;;end old-value
          )
         (t (setf return-value (list  new-value)
                  new-keylist (append  (list key)  put-value-after-items  return-value))))
        ;;end put-value-after-items
        )
       ;;4.3 SPLICE-OLD-NEW-VALUES-P
       #|(SPLICE-OLD-NEW-VALUES-P             
        (cond
         (old-value
          ;;added
          (cond
           ((and  (listp old-value) (listp new-value))
            (setf  return-value (append  old-value  new-value)
                   new-keylist (append  (list key)  (list return-value))))
           ((listp new-value)
            (setf  return-value (append  (list old-value)  new-value)
                   new-keylist (append  (list key)  (list return-value))))
           ((listp old-value) ;;2020
            (setf  return-value (append  old-value (list new-value))
                   new-keylist (append  (list key)  (list return-value))))
           (t
            (setf  return-value (list old-value  new-value)
                   new-keylist (append  (list key)  put-value-after-items
                                        return-value))))
          ;;end old-value
          )
         (t (setf return-value (list  new-value)
                  new-keylist (append  (list key)  put-value-after-items  return-value))))
        (when not-duplicate-p
          (setf return-value (delete-duplicates return-value)
                new-keylist (delete-duplicates new-keylist)))
        ;;end splice-old-new-values-p
        )|#
       (t 
        (cond
         (old-value
          (setf return-value (list  old-value  new-value)
                new-keylist (append (list key) return-value)))
         (t (setf return-value  new-value
                  new-keylist (list key new-value))))))

      ;;later? (when begin-items (setf new-keylist (append begin-items new-keylist)))
      ;;end add-value-p
      )
     ;;6. FOR SET/REPLACE OLD VALUE
     (T
      #|    above  (when (numberp keyloc-n)
        (setf old-value (nth key+val-n old-keylist)))|#
      (cond
       ((equal key :NTH)
        (setf  return-value new-value
               new-keylist (list new-value)))
       ;;SET KEY NOT NTH
       (t
        (cond
         (put-value-after-items
          (cond
           ((not (equal key :NTH))
            (setf  return-value   new-value
                   new-keylist (append (list key) put-value-after-items (list return-value))))
           (t (setf  return-value  new-value
                     new-keylist (append  put-value-after-items (list return-value)))))
          ;;(break "put-value-after-items 1")
          ;;end put-value-after-items
          )         
         (t (setf return-value  new-value
                  new-keylist (list key new-value))))
        ;;end t, key not nth, cond
        ))
      ;;later? (when begin-items (setf new-keylist (append begin-items new-keylist)))
    
      ;;end of  T REPLACE OLD-VALUE, cond
      ))
    ;;end step 1
    ;;(break "AFTER STEP 1")
    ;;(afout 'out (format nil "AFTER STEP 1 new-keylist= ~A~% return-keylist= ~A" new-keylist return-keylist))

    ;;STEP 2: FINAL CHANGES TO KEYLIST?
    ;;was (unless (equal key :NTH)
    (unless (or  (member new-value '( :get :delete-key&value) :test 'equal)                
                 (equal new-keylist :set-keylist=nil))
      ;;when delete
      (when (and put-value-after-items
                 (member new-value  '(:delete-value))) ;; :delete-key&value)))
        (cond
         (new-keylist
          (setf new-keylist (append new-keylist put-value-after-items)))
         (t
          (setf new-keylist put-value-after-items))))
      ;;(break "before begin-items")
      ;;for calculated begin and end items
      (when begin-items
        (cond
         ;;if listp old-value, keep list in tact for append
         ((and parens-after-begin-items-p (listp old-value))
          ;;use this??(setf return-keylist (append begin-items (list new-keylist))))
          (setf new-keylist (append begin-items (list new-keylist))))         
         ;;previously-put extra parens before key in all new lists
         (t 
          ;;use this?(setf return-keylist (append begin-items new-keylist)))))
          (setf new-keylist (append begin-items new-keylist)))))
      ;;(break "before end-items")
      ;;END-ITEMS
      (when end-items         
          (cond
           ;;if listp old-value, keep list in tact for append
           ((listp old-value)
            (cond
             ;;for cases where end-items are added to a new appended new-keylist
             ((listp new-keylist)
              (setf  new-keylist (append  new-keylist  end-items)))
             ;;otherwise
             (t (setf  new-keylist (append (list new-keylist)  end-items))))
            )
           (t (setf  new-keylist (append new-keylist  end-items)))))
      ;;NEW-BEGIN-ITEMS or NEW-END-ITEMS
      ;;for new ones from args
      (when new-begin-items
        (setf new-keylist (append new-begin-items new-keylist)))
      (when new-end-items
        (setf  new-keylist (append new-keylist  new-end-items)))
      ;;END UNLESS  DELETE, :DELETE-KEY&VALUE, ETC      
      )
    ;;(afout 'out (format nil "AFTER STEP 2 new-keylist= ~A~% return-keylist= ~A" new-keylist return-keylist))
    ;;(break "AFTER STEP 2")

    ;;STEP 3: INSERT NEW-KEYLIST IN A LIST?
    (cond
     ;;3.1. If NILS [added 2019-04 bec :set-keylist=nil put NIL in its place instead of EMPTY]
     ((or (equal new-value :set-keylist=nil)(equal new-keylist :set-keylist=nils))
      NIL ;;??
      )
     ;;3.2. SIMPLE-SPLICE-KEY-VALUE-IN-KEYLIST-P      ;;added 2020
     (simple-splice-key-value-in-keylist-p
      (cond 
       ((listp new-value)
          (setf return-keylist (append old-keylist new-value)
                new-keylist return-keylist))
       ((listp old-keylist)
        (setf return-keylist (append old-keylist (list new-value))
                new-keylist return-keylist))))
     ;;3.3. SPLICE-KEY-VALUE-IN-LIST-P
     (splice-key-value-in-list-p 
      ;;is splice-list-n > keyloc-n?
      (cond 
       ((and old-keylist)
        (>=  (setf splice-list-n (list-length old-keylist))    keyloc-n)
        NIL )
       ;;if not, must modify new-keylist by putting in new elments 
       (t (setf return-keylist (append (make-list (- keyloc-n splice-list-n)  :initial-element put-key-after-items)  new-keylist))))
      ;;In either case, make new-keylist
      (cond ;;HERE1
       (splice-old-new-values-p  ;;here now
                                 (setf return-keylist (append-nth (list new-keylist)
                                                           keyloc-n splice-key-value-in-list-p :splice-list-p T)) 
        ;;(append-nth '(:key value) 1 '(1 2  3 4 5 6) :splice-list-p T) = (1 2 :KEY VALUE 3 4 5 6)
        )
       (t  
        (setf  return-keylist (append-nth (list new-keylist) keyloc-n splice-key-value-in-list-p :splice-list-p NIL))))   
      ;;(break "END splice-key-value-in-list-p")
      ;;end splice-key-value-in-list-p clause
      )
     ;;3.4. IF PUT-KEY-AFTER-ITEMS and NULL SPLICE-KEY-VALUE-IN-LIST-P
     (put-key-after-items
      (when (> keyloc-n 0)
        ;;if not a list, use put-key-after-items to fill a new list; IF :NIL fills with NIL
        (cond
         ((not (listp put-key-after-items))
          (cond
           ((equal put-key-after-items :NIL)
            (setf put-key-after-items (make-list keyloc-n :initial-element  NIL)))
           (t (make-list keyloc-n :initial-element  put-key-after-items))))
         ((listp put-key-after-items)
          ;;keyloc-n is num of items needed to fill in keyloc-n > 0
          ;;put-n is number of  items put-key-after-items is short
          (setf put-n (list-length put-key-after-items)
                add-n (- keyloc-n put-n))           
          ;;cccc
          (cond
           ;; if keyloc-n = put-n, use entire list
           ((= add-n 0) NIL)
           ;;if put-n is negative, must only use part of list
           ((< add-n 0)
            (setf put-key-after-items (butlast put-key-after-items (abs add-n))))
           ;;keyloc-n > list length, must add elements
           ((and (> put-n 0) add-nils-p)
            (setf  put-key-after-items (append put-key-after-items 
                                                    (make-list add-n :initial-element  NIL))))
         (t nil))
        ;;(afout 'out (format nil "AFTER STEP 3: PUT new-keylist= ~A~% return-keylist= ~A" new-keylist return-keylist))
        ;;(BREAK "AFTER PUT")
        ;;IN ANY CASE PUT KEYLIST AFTER PUT-KEY-AFTER-ITEMS
        (when (> (list-length put-key-after-items) 0)
          (cond
           (splice-old-new-values-p
            (setf  return-keylist (append  put-key-after-items  (list new-keylist))))
           (t (setf  return-keylist (append  put-key-after-items  new-keylist))))
          ;;end when
          )
        ;;end  listp clause
        )
        (t nil))
        ;; end  when keyloc-n > 0
        )
      ;;(break "END put-key-after-items")
      ;;end put-key-after-items clause
      )
     ;;end cond
     (t nil))

    ;;4.  IF ALL OF ABOVE ARE NIL OR THEY SET RETURN-KEYLIST TO NIL
    (unless old-key&value
      (setf old-key&value (list old-key old-value)))
    ;;4.1. OFTEN THE RETURN-KEYLIST = NEW-KEYLIST
    ;;(break "END return-keylist")
    (when (and (null return-keylist) (equal new-keylist :set-keylist=nil))
      (setf new-keylist NIL)) ;;1st and term was (null return-set-keylist=nilp)=>error
    (when (null return-keylist)
      (setf return-keylist new-keylist))
    
    (values return-keylist new-keylist return-value old-value old-key&value)
    ;;end let, get-set-append-delete-keyvalue
    ))
;;TEST
;;2020
;; (get-set-append-delete-keyvalue :key2 "NEW VALUE"  :keyloc-n t :val-nth 1 :old-keylist'(:key2 1  (OLD VALUE 1) 2 3) :append-value-p NIL :list-first-item-in-append-p NIL :add-value-p NIL :delete-old-nil-p T :simple-splice-key-value-in-keylist-p NIL :splice-newvalue-at-nth-p NIL :put-key-after-items NIL :add-nils-p NIL :put-value-after-items NIL :new-begin-items NIL :new-end-items NIL :parens-after-begin-items-p NIL :splice-key-value-in-list-p NIL :break-if-keys-not-match-p NIL :splice-old-new-values-p T :not-duplicate-p T :old-key&value  NIL)
;;works= (:KEY2 (OLD VALUE 1 "NEW VALUE") 2 3)
;; (:KEY2 (OLD VALUE 1 "NEW VALUE") 2 3)    (OLD VALUE 1 "NEW VALUE")  (OLD VALUE 1)   (:KEY2 (OLD VALUE 1))
;;For NOT-DUPLICATE-P 
;; (get-set-append-delete-keyvalue :key2 "NEW VALUE"  :keyloc-n t :val-nth 2 :old-keylist'(:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3) :append-value-p NIL :list-first-item-in-append-p NIL :add-value-p NIL :delete-old-nil-p T :simple-splice-key-value-in-keylist-p NIL :splice-newvalue-at-nth-p NIL :put-key-after-items NIL :add-nils-p NIL :put-value-after-items NIL :new-begin-items NIL :new-end-items NIL :parens-after-begin-items-p NIL :splice-key-value-in-list-p NIL :break-if-keys-not-match-p NIL :splice-old-new-values-p T :not-duplicate-p T :old-key&value  NIL)
;; (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3)   (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3)    (OLD VALUE 1 "NEW VALUE")   (OLD VALUE 1 "NEW VALUE")    (:KEY2 (OLD VALUE 1 "NEW VALUE"))
;;

;;:SIMPLE-SPLICE-KEY-VALUE-IN-KEYLIST-P
;; (get-set-append-delete-keyvalue  :key2  "NEW VALUE" :old-keylist '(:key2 1  (OLD VALUE 1) 2 3) :val-nth 2 :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :splice-old-new-values-p nil :SIMPLE-SPLICE-KEY-VALUE-IN-KEYLIST-P T)
;;WORKS= (:KEY2 1 (OLD VALUE 1) 2 3 "NEW VALUE")
;; (:KEY2 1 (OLD VALUE 1) 2 3 "NEW VALUE")
;; "NEW VALUE"  "OLD VALUE 1)  (:KEY2 (OLD VALUE 1))
;; :SPLICE-OLD-NEW-VALUES-P
;; (get-set-append-delete-keyvalue  :key2  "NEW VALUE" :old-keylist '(:key2 1  (OLD VALUE 1) 2 3) :val-nth 2 :append-value-p NIL :add-value-p NIL  :splice-key-value-in-list-p NIL :SPLICE-OLD-NEW-VALUES-P T)   
;; works = (:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3)
;;(:KEY2 1 (OLD VALUE 1 "NEW VALUE") 2 3)
;; (OLD VALUE 1 "NEW VALUE") (OLD VALUE 1)  (:KEY2 (OLD VALUE 1))
;;TEST
;; :SIMPLE-SPLICE-KEY-VALUE-IN-KEYLIST-P
;; (get-set-append-delete-keyvalue  ':PC '(:CSVAL "0.917")  :OLD-KEYLIST  '("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL)))  :SIMPLE-SPLICE-KEY-VALUE-IN-KEYLIST-P  T)
;;works= ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL)) :CSVAL "0.917")
;; ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC (:CSVAL "0.917") :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL)))
;; (:CSVAL "0.917")   ("CARE FOR OTHERS" "SELFISH" 1 NIL)   (:CSVAL "0.917")
;;:DELETE-KEY&VALUE
;; (get-set-append-delete-keyvalue  ':CSVAL :DELETE-KEY&VALUE  :OLD-KEYLIST  '(CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :CSVAL "0.917" :RNK 3))
;;works= (CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :RNK 3)
;; (CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :RNK 3)     NIL    "0.917"




;; IF NO :APPEND-VALUE-P, REPLACES OLD VALUE
;; (get-set-append-delete-keyvalue :key1 '(1 2 3) :old-keylist '(a b c :key1 NIL d e f))
;; result=   (A B C :KEY1 (1 2 3) D E F) etc
;; (get-set-append-delete-keyvalue :key1 '(1 2 3) :old-keylist '(a b c :key1 OLD-VAL d e f))
;; result= (A B C :KEY1 (1 2 3) D E F)
;;
;; IF :APPEND-VALUE-P, lists (old-value (LIST new-value))
;; (get-set-append-delete-keyvalue :key1 '(1 2 3) :old-keylist '(a b c :key1 NIL d e f) :append-value-p T)) = (A B C :KEY1 ((1 2 3)) D E F)
;; (get-set-append-delete-keyvalue :key1 '(1 2 3) :old-keylist '(a b c :key1 OLD-VAL  d e f) :append-value-p T)
;; result= (A B C :KEY1 (OLD-VAL (1 2 3)) D E F)
;; (get-set-append-delete-keyvalue :key1 '(1 2 3) :old-keylist '(a b c :key1 (OLD1 OLD2)  d e f) :append-value-p T)
;; result= (A B C :KEY1 (OLD1 OLD2 (1 2 3)) D E F)
;; TO APPEND OLD-VALUE LIST WITH NEW-VALUE LIST
;; MUST USE BOTH  :APPEND-VALUE-P T  :SPLICE-OLD-NEW-VALUES-P T)
;; ;; (get-set-append-delete-keyvalue :key1 '(1 2 3) :old-keylist '(a b c :key1(OLD1 OLD2)  d e f) :append-value-p T  :splice-old-new-values-p T)
;; result= (A B C :KEY1 (OLD1 OLD2 1 2 3) D E F)
;;USE OF NOT-DUPLICATE-P
;; (get-set-append-delete-keyvalue :key1 'OLD-VAL :old-keylist '(a b c :key1 (11 22 OLD-VAL 33 44)  d e f) :append-value-p T  :NOT-DUPLICATE-P T)
;; works - (A B C :KEY1 11 22 OLD-VAL 33 44 D E F)    (A B C :KEY1 11 22 OLD-VAL 33 44 D E F)    (11 22 OLD-VAL 33 44)    (11 22 OLD-VAL 33 44)

;; (get-set-append-delete-keyvalue :key1 '(1 2 3) :old-keylist '(a b c :key1 NIL d e f) :append-value-p T)
;; (get-set-append-delete-keyvalue` :BIPATH '(new-path info) :old-keylist '("KIND" "kind vs ukind" :BESTPOLE 1 (POLE1 X X) :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL)) :CSVAL "0.917") :APPEND-VALUE-P T)
;;works=> ("KIND" "kind vs ukind" :BESTPOLE 1 (POLE1 X X) :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL) (NEW-PATH INFO)) :CSVAL "0.917")

;;(get-set-append-delete-keyvalue :csval "newval" :old-keylist '("KIND" "kind vs ukind" :BESTPOLE 1 (POLE1 X X) :CSVAL "0.917") :keyloc-n 7 :val-nth 1)
;; works = ("KIND" "kind vs ukind" :BESTPOLE 1 (POLE1 X X) :CSVAL "newval")   ("KIND" "kind vs ukind" :BESTPOLE 1 (POLE1 X X) :CSVAL "newval")   "newval"   "0.917"
;;(get-set-append-delete-keyvalue :csval "newval" :old-keylist  '(:K2 VAL2 1 2 3 4) :add-value-p T :keyloc-n 6 :val-nth 1)
;; works= (:K2 VAL2 1 2 3 4 :CSVAL "newval")   (:K2 VAL2 1 2 3 4 :CSVAL "newval")   "newval"  NIL
;;(get-set-append-delete-keyvalue :newkey    'new-value  :OLD-KEYLIST '(1 2 OLD-VALUE 3 4   5 (6 7)  8 9 ) :KEYLOC-N 5  :append-value-p nil :add-value-p T :splice-newvalue-at-nth-p T)
;; works= (1 2 OLD-VALUE 3 4 :NEWKEY NEW-VALUE 5 (6 7) 8 9)  (1 2 OLD-VALUE 3 4 :NEWKEY NEW-VALUE 5 (6 7) 8 9)   NEW-VALUE  NIL
;; APPEND A NEW KEY AND VALUE to end (use a LARGE :keyloc-n 9 :val-nth 1)
;; (get-set-append-delete-keyvalue :csval "newval" :old-keylist  '(:K2 VAL2 1 2 3 4) :append-value-p T :keyloc-n 9 :val-nth 1)
;;works= (:K2 VAL2 1 2 3 4 :CSVAL ("newval"))   (:K2 VAL2 1 2 3 4 :CSVAL ("newval"))  ("newval")  NIL
;; then append a second value using same args:
;; (get-set-append-delete-keyvalue :csval "newval2" :old-keylist  '(:K2 VAL2 1 2 3 4 :CSVAL ("newval")) :append-value-p T :keyloc-n 9 :val-nth 1)
;;works= (:K2 VAL2 1 2 3 4 :CSVAL ("newval" "newval2"))   (:K2 VAL2 1 2 3 4 :CSVAL ("newval" "newval2"))    ("newval" "newval2")  ("newval")
;; old-keylist = NIL
;; (get-set-append-delete-keyvalue :csval "newval2" :old-keylist  NIL :append-value-p T :keyloc-n 9 :val-nth 1)
;;works= (:CSVAL ("newval2")) (:CSVAL ("newval2")) ("newval2") NIL

;;--------------- Use of APPEND-KEY-P, ADD-KEY-P and just REPLACE OLD-VALUE-----
;;APPEND-KEY-P T AND key doesn't exist
;; (get-set-append-delete-keyvalue :newkey    'new-value  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4   5 (6 7)  8 9 ) :KEYLOC-N 5  :append-value-p T)
;;works= (A B C 1 2 OLD-VALUE 3 4 :NEWKEY (NEW-VALUE) 5 (6 7) 8 9)  (1 2 OLD-VALUE 3 4 :NEWKEY (NEW-VALUE) 5 (6 7) 8 9)  (NEW-VALUE)  NIL
;;APPEND-KEY-P T AND key exists
;;(get-set-append-delete-keyvalue :newkey    'new-value  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4 :newkey old-value2  5 (6 7)  8 9 ) :KEYLOC-N 5  :append-value-p T)
;;works= (A B C 1 2 OLD-VALUE 3 4 :NEWKEY (OLD-VALUE2 NEW-VALUE) 5 (6 7) 8 9)  (1 2 OLD-VALUE 3 4 :NEWKEY (OLD-VALUE2 NEW-VALUE) 5 (6 7) 8 9)  (OLD-VALUE2 NEW-VALUE)  OLD-VALUE2
;;ADD-KEY-P T  :append-value-p nil AND key doesn't exist
;; (get-set-append-delete-keyvalue :newkey    'new-value  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4   5 (6 7)  8 9 ) :KEYLOC-N 5  :append-value-p nil :add-value-p T)
;; works= (A B C 1 2 OLD-VALUE 3 4 :NEWKEY NEW-VALUE 5 (6 7) 8 9)  (1 2 OLD-VALUE 3 4 :NEWKEY NEW-VALUE 5 (6 7) 8 9)  NEW-VALUE  NIL
;;ADD-KEY-P T  :append-value-p nil AND key exists
;; (get-set-append-delete-keyvalue :newkey    'new-value  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4 :NEWKEY OLD-VALUE2  5 (6 7)  8 9 ) :KEYLOC-N 5  :append-value-p nil :add-value-p T)
;; works = (A B C 1 2 OLD-VALUE 3 4 :NEWKEY OLD-VALUE2 NEW-VALUE 5 (6 7) 8 9)   (1 2 OLD-VALUE 3 4 :NEWKEY OLD-VALUE2 NEW-VALUE 5 (6 7) 8 9)   (OLD-VALUE2 NEW-VALUE)   OLD-VALUE2
;;REPLACE KEY-VALUE add-key-p nil  :append-value-p nil and key doesn't exist
;; (get-set-append-delete-keyvalue :newkey    'new-value  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4 :NEWKEY OLD-VALUE2  5 (6 7)  8 9 ) :KEYLOC-N 5  :append-value-p nil :add-value-p NIL)
;; works= (A B C 1 2 OLD-VALUE 3 4 :NEWKEY NEW-VALUE 5 (6 7) 8 9)  (1 2 OLD-VALUE 3 4 :NEWKEY NEW-VALUE 5 (6 7) 8 9)   NEW-VALUE  OLD-VALUE2
;; end append, add, replace --------------------------------------------------------------------------------

;; MORE SIMPLE CASES
;; (get-set-append-delete-keyvalue  'K5 'NEWVAL :KEYLOC-N T :OLD-KEYLIST '(K3 33 K5 55 K1 11 K7 77 K8 88))
;; works= (K3 33 K5 NEWVAL K1 11 K7 77 K8 88)   (K3 33 K5 NEWVAL K1 11 K7 77 K8 88)   NEWVAL   55
;; KEY = :NTH
;; (get-set-append-delete-keyvalue :NTH    'new-value  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4   5 (6 7)  8 9 ) :KEYLOC-N 2  )
;; works= (A B (1 2 NEW-VALUE 3 4 5 (6 7) 8 9))    (1 2 NEW-VALUE 3 4 5 (6 7) 8 9)     NEW-VALUE   OLD-VALUE
;; nth with a new-value keylist
;; (get-set-append-delete-keyvalue :NTH    '(:newkey new-value)  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4   5 (6 7)  8 9 ) :KEYLOC-N 2  )
;;works [but replaces old-value with insert keyvalue LIST]= (A B 1 2 (:NEWKEY NEW-VALUE) 3 4 5 (6 7) 8 9)   (1 2 (:NEWKEY NEW-VALUE) 3 4 5 (6 7) 8 9)   (:NEWKEY NEW-VALUE)   OLD-VALUE
;; NTH :KEYLOC-N 2 (same as old-value)
;; (get-set-append-delete-keyvalue :NTH    '(:newkey new-value)  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4   5 (6 7)  8 9 ) :KEYLOC-N 2  :append-value-p T)
;; result= (A B 1 2 :NTH (OLD-VALUE (:NEWKEY NEW-VALUE)) 3 4 5 (6 7) 8 9)    (1 2 :NTH (OLD-VALUE (:NEWKEY NEW-VALUE)) 3 4 5 (6 7) 8 9)   (OLD-VALUE (:NEWKEY NEW-VALUE))   OLD-VALUE
;; :NTH, :KEYLOC-N 9
;; (get-set-append-delete-keyvalue :NTH    '(:newkey new-value)   :KEYLOC-N 9 :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4   5 (6 7)  8 9 ))
;;result= (A B C NIL NIL NIL NIL NIL NIL 1 2 OLD-VALUE 3 4 5 (6 7) 8 9 (:NEWKEY NEW-VALUE))   (1 2 OLD-VALUE 3 4 5 (6 7) 8 9 (:NEWKEY NEW-VALUE))   (:NEWKEY NEW-VALUE)  NIL
;;
;;ADD-VALUE-P & :NTH  to add a new (:newkey newval) to a keylist
;; (get-set-append-delete-keyvalue :NTH    '(:newkey new-value)  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4   5 (6 7)  8 9 ) :KEYLOC-N 99 :add-value-p T )
;;works= (A B C 1 2 OLD-VALUE 3 4 5 (6 7) 8 9 (:NEWKEY NEW-VALUE))  (1 2 OLD-VALUE 3 4 5 (6 7) 8 9 (:NEWKEY NEW-VALUE))  ((:NEWKEY NEW-VALUE))   NIL
;;here now
;;ADD-VALUE-P & :NTH  to add a new :newkey newval  to a keylist
;; plus :SPLICE-NEWVALUE-AT-NTH-P to eliminate parens
;; (get-set-append-delete-keyvalue :NTH    '(:newkey new-value)  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 OLD-VALUE 3 4   5 (6 7)  8 9 ) :KEYLOC-N 99 :ADD-VALUE-P T :SPLICE-NEWVALUE-AT-NTH-P T)
;; works= (A B C 1 2 OLD-VALUE 3 4 5 (6 7) 8 9 :NEWKEY NEW-VALUE)  (1 2 OLD-VALUE 3 4 5 (6 7) 8 9 :NEWKEY NEW-VALUE)  (:NEWKEY NEW-VALUE)  NIL
;;  (get-set-append-delete-keyvalue :K2 '(:NEWKEY NEWVAL) :OLD-KEYLIST '(:K2 VAL2 1 2 3 4) :ADD-VALUE-P T :SPLICE-NEWVALUE-AT-NTH-P T :KEYLOC-N 0)
;; results = (:K2 VAL2 (:NEWKEY NEWVAL) 1 2 3 4)    (:K2 VAL2 (:NEWKEY NEWVAL) 1 2 3 4)    (VAL2 (:NEWKEY NEWVAL))    VAL2 
;; (get-set-append-delete-keyvalue :NTH '(:NEWKEY NEWVAL) :OLD-KEYLIST '(:K2 VAL2 1 2 3 4) :ADD-VALUE-P T :SPLICE-NEWVALUE-AT-NTH-P T :KEYLOC-N 0)
;;works= (:NEWKEY NEWVAL :K2 VAL2 1 2 3 4)   (:NEWKEY NEWVAL :K2 VAL2 1 2 3 4)   (:NEWKEY NEWVAL)   :K2
;;  (get-set-append-delete-keyvalue :NTH '(:NEWKEY NEWVAL) :OLD-KEYLIST '(:K2 VAL2 1 2 3 4) :ADD-VALUE-P T :SPLICE-NEWVALUE-AT-NTH-P T :KEYLOC-N 99)
;; results/works= (:K2 VAL2 1 2 3 4 :NEWKEY NEWVAL)

;;  (append-nth  '(:NEWKEY NEW-VALUE) 99   '(1 2 OLD-VALUE 3 4 5 (6 7) 8 9 ) :splice-list-p T) = (1 2 OLD-VALUE 3 4 5 (6 7) 8 9 :NEWKEY NEW-VALUE)
;; ;GET
;;  (get-set-append-delete-keyvalue :key4  :get :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4 old-value  a b c d e) :append-value-p T :splice-old-new-values-p NIL )
;;  works=  (:KEY4 OLD-VALUE A B C D E)    (:KEY4 OLD-VALUE A B C D E)     OLD-VALUE    OLD-VALUE
;;
;;
;; OLD-VALUE, :APPEND-VALUE-P T
;; (get-set-append-delete-keyvalue :key4  'new-value :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  old-value a b c d e) :append-value-p T :splice-old-new-values-p NIL )
;;works=  (:KEY4 (OLD-VALUE NEW-VALUE) A B C D E)    (:KEY4 (OLD-VALUE NEW-VALUE) A B C D E)    (OLD-VALUE NEW-VALUE)  OLD-VALUE
;;
;; ;; 2018 OLD-VALUE, :APPEND-VALUE-P T AND :LIST-FIRST-ITEM-IN-APPEND-P
;;for old item = NIL
 ;; (get-set-append-delete-keyvalue :key4  "newvalue" :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  nil a b c d e) :append-value-p T :LIST-FIRST-ITEM-IN-APPEND-P T  :splice-old-new-values-p NIL )
;; works= (:KEY4 ("newvalue") A B C D E)   (:KEY4 ("newvalue") A B C D E)  ("newvalue")  NIL
;;for old item = ("newvalue")
;; (get-set-append-delete-keyvalue :key4  "newvalue2" :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  ("newvalue") a b c d e) :append-value-p T :LIST-FIRST-ITEM-IN-APPEND-P T  :splice-old-new-values-p NIL )
;; works= (:KEY4 ("newvalue" "newvalue2") A B C D E)    (:KEY4 ("newvalue" "newvalue2") A B C D E)   ("newvalue" "newvalue2")   ("newvalue")  

;; (get-set-append-delete-keyvalue :key4  '(new-value) :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  old-value a b c d e) :append-value-p T :LIST-FIRST-ITEM-IN-APPEND-P T  :splice-old-new-values-p NIL )
;;works (:KEY4 ((OLD-VALUE) (NEW-VALUE)) A B C D E)   (:KEY4 ((OLD-VALUE) (NEW-VALUE)) A B C D E)   ((OLD-VALUE) (NEW-VALUE))   (OLD-VALUE)
;;for appending last return keylist
;; (get-set-append-delete-keyvalue :key4  '(new-value2) :KEYLOC-N T :VAL-NTH 1 :old-keylist  '(x y z  :KEY4 ((OLD-VALUE) (NEW-VALUE)) A B C D E) :append-value-p T :LIST-FIRST-ITEM-IN-APPEND-P T  :splice-old-new-values-p NIL )
;;works=(X Y Z :KEY4 ((OLD-VALUE) (NEW-VALUE) (NEW-VALUE2)) A B C D E)   (X Y Z :KEY4 ((OLD-VALUE) (NEW-VALUE) (NEW-VALUE2)) A B C D E)   ((OLD-VALUE) (NEW-VALUE) (NEW-VALUE2))   ((OLD-VALUE) (NEW-VALUE))

;;  KEYLOC-N =T, :append-value-p=T, (:splice-old-new-values-p MAKES NO DIFF)
;; (get-set-append-delete-keyvalue :location  "location" :KEYLOC-N T :VAL-NTH 1 :old-keylist '(A B C :LOCATION OLD-VALUE D (1 2 3 4) E F) :append-value-p T :splice-old-new-values-p NIL )
;;works= (A B C :LOCATION (OLD-VALUE "location") D (1 2 3 4) E F)   (A B C :LOCATION (OLD-VALUE "location") D (1 2 3 4) E F)    (OLD-VALUE "location")  OLD-VALUE
;; (get-set-append-delete-keyvalue :location  "location2" :KEYLOC-N T :VAL-NTH 1 :old-keylist '(A B C :LOCATION (OLD-VALUE "location") D (1 2 3 4) E F) :append-value-p T :splice-old-new-values-p NIL )
;; works= (A B C :LOCATION (OLD-VALUE "location" "location2") D (1 2 3 4) E F) ETC
;;
;; REPLACE OLD-VALUE, :append-value-p NIL 
;; (OLD-VALUE 1 2) is a list, :append-value-p T
;; (get-set-append-delete-keyvalue :key4  'new-value :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  (OLD-VALUE 1 2) a b c d e) :append-value-p T :splice-old-new-values-p NIL )
;; works= (:KEY4 (OLD-VALUE 1 2 NEW-VALUE) A B C D E)    (:KEY4 (OLD-VALUE 1 2 NEW-VALUE) A B C D E)      (OLD-VALUE 1 2 NEW-VALUE)    (OLD-VALUE 1 2)
;; :append-value-p T :splice-old-new-values-p NIL   
;;(get-set-append-delete-keyvalue :key4  '(NEW-VALUE X Y) :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  (OLD-VALUE 1 2) a b c d e) :append-value-p T :splice-old-new-values-p NIL )
;; works= (:KEY4 (OLD-VALUE 1 2 (NEW-VALUE X Y)) A B C D E)   (:KEY4 (OLD-VALUE 1 2 (NEW-VALUE X Y)) A B C D E)   (OLD-VALUE 1 2 (NEW-VALUE X Y))   (OLD-VALUE 1 2)

;; :APPEND-VALUE-P T     :SPLICE-OLD-NEW-VALUES-P T 
;; (get-set-append-delete-keyvalue :key4  '(NEW-VALUE X Y) :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  (OLD-VALUE 1 2) a b c d e) :append-value-p T :splice-old-new-values-p T )
;; works= (:KEY4 (OLD-VALUE 1 2 NEW-VALUE X Y) A B C D E)   (:KEY4 (OLD-VALUE 1 2 NEW-VALUE X Y) A B C D E)  ((OLD-VALUE 1 2 NEW-VALUE X Y))  OLD-VALUE 1 2)
;;
;; NEW-BEGIN-ITEMS, NEW-END-ITEMS, :PUT-VALUE-AFTER-ITEMS
;; SET (REPLACE OLD-VALUE WITH NEW-VALUE) W/ new-begin-items new-end-items
;; (get-set-append-delete-keyvalue :key4  'new-value :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  old-value) :append-value-p NIL :splice-old-new-values-p NIL :new-begin-items '(begin items) :new-end-items '(end items) :put-value-after-items '(items between key and value))
;; works= (BEGIN ITEMS :KEY4 ITEMS BETWEEN KEY AND VALUE NEW-VALUE END ITEMS)    (BEGIN ITEMS :KEY4 ITEMS BETWEEN KEY AND VALUE NEW-VALUE END ITEMS)  (NEW-VALUE)  OLD-VALUE
;;
;; :ADD-VALUE-P when NEW-VALUE IS LIST
;; (get-set-append-delete-keyvalue :key4   '(NEW-VALUE X Y) :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  old-value a b c d e) :ADD-VALUE-P T :append-value-p NIL :splice-old-new-values-p NIL )
;;works= (:KEY4 OLD-VALUE (NEW-VALUE X Y) A B C D E)   (:KEY4 OLD-VALUE (NEW-VALUE X Y) A B C D E)  (OLD-VALUE (NEW-VALUE X Y))   OLD-VALUE
;; :ADD-VALUE-P when NEW-VALUE IS NOT A LIST
;; (get-set-append-delete-keyvalue :key4   'NEW-VALUE  :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  old-value a b c d e) :ADD-VALUE-P T :append-value-p NIL :splice-old-new-values-p NIL )
;; works=  (:KEY4 OLD-VALUE NEW-VALUE A B C D E)       (:KEY4 OLD-VALUE NEW-VALUE A B C D E)   (OLD-VALUE NEW-VALUE)   OLD-VALUE

;;WHEN OLD-KEY, KEY DON'T MATCH (or there is no old-key in old-keylist)
;;when  :break-if-keys-not-match-p = NIL
;; (get-set-append-delete-keyvalue :key4   'NEW-VALUE  :KEYLOC-N 0 :VAL-NTH 3 :old-keylist '( :not-key4  old-value a b c d e) :ADD-VALUE-P T :break-if-keys-not-match-p NIL  :append-value-p NIL :splice-old-new-values-p NIL )
;; works= (:KEY4 :NOT-KEY4 OLD-VALUE NEW-VALUE B C D E)       (:KEY4 :NOT-KEY4 OLD-VALUE NEW-VALUE B C D E)            (NEW-VALUE)              NIL
;; when  :break-if-keys-not-match-p T 
;;  (get-set-append-delete-keyvalue :key4   'NEW-VALUE  :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :not-key4  old-value a b c d e) :ADD-VALUE-P T :break-if-keys-not-match-p T  :append-value-p NIL :splice-old-new-values-p NIL )
;;works= causes break plus message: ERROR:  OLD-KEY= NOT-KEY4 does NOT match KEY= KEY4
;;
;FOR  NEW-BEGIN-ITEMS, NEW-END-ITEMS
;;  (get-set-append-delete-keyvalue :key3   'new-value :keyloc-n 2 :val-nth 3 :old-keylist '(PRE3A PRE3B :KEY3 (44) A OLD-VALUE B C  (D) E POST3)  :append-value-p T :splice-old-new-values-p NIL :NEW-BEGIN-ITEMS '(NEW BEGIN ITEMS) :NEW-END-ITEMS '(NEW END ITEMS))
;;  works=  (NEW BEGIN ITEMS PRE3A PRE3B :KEY3 (44) A (OLD-VALUE NEW-VALUE) B C (D) E POST3 NEW END ITEMS)  (NEW BEGIN ITEMS PRE3A PRE3B :KEY3 (44) A (OLD-VALUE NEW-VALUE) B C (D) E POST3 NEW END ITEMS)   (OLD-VALUE NEW-VALUE)   OLD-VALUE
;;
;; IF key is in a NESTED LIST, WILL NOT FIND IT, so acts AS IF NO KEY FOUND
;;(get-set-append-delete-keyvalue :key4  'new-value :keyloc-n 3 :val-nth 3 :old-keylist '(PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3)  :append-value-p T :splice-old-new-values-p T)
;;works=[ note there are 2 key4's bec can't find inner one]= (PRE3A PRE3B :KEY3 :KEY4 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3 NEW-VALUE)    (PRE3A PRE3B :KEY3 :KEY4 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3 NEW-VALUE)    (POST3 NEW-VALUE)    POST3
;
;;COMPLEX FORMS WHEN ADD ITEMS TO RESULT
;;  :splice-old-new-values-p NIL 
;; (get-set-append-delete-keyvalue :key4  'new-value :KEYLOC-N 3 :VAL-NTH 4 :old-keylist '(old begin items :key4  old mid items OLD-VALUE  old end items) :append-value-p T :splice-old-new-values-p NIL )
;;works=  (OLD BEGIN ITEMS :KEY4 OLD MID ITEMS (OLD-VALUE NEW-VALUE) OLD END ITEMS)        (OLD BEGIN ITEMS :KEY4 OLD MID ITEMS (OLD-VALUE NEW-VALUE) OLD END ITEMS)            (OLD-VALUE NEW-VALUE)              OLD-VALUE
;;  :SPLICE-OLD-NEW-VALUES-P T, :append-value-p T
;; (get-set-append-delete-keyvalue :key4  'new-value :KEYLOC-N 3 :VAL-NTH 4 :old-keylist '(old begin items :key4  old mid items OLD-VALUE  old end items) :append-value-p T :splice-old-new-values-p T )
;;works= (OLD BEGIN ITEMS :KEY4 OLD MID ITEMS OLD-VALUE NEW-VALUE OLD END ITEMS)               (OLD BEGIN ITEMS :KEY4 OLD MID ITEMS OLD-VALUE NEW-VALUE OLD END ITEMS)                (OLD-VALUE NEW-VALUE)                OLD-VALUE


;; ;; (progn (setf out nil) (get-set-append-delete-keyvalue :location   "new location" :OLD-KEYLIST '(A B C D (1 2 3 4) E F) :KEYLOC-N  2  :append-value-p T :splice-old-new-values-p nil))
;; works= (A B :LOCATION "new location" C D (1 2 3 4) E F)            (A B :LOCATION "new location" C D (1 2 3 4) E F)          "new location"          NIL
;;WHEN KEYLOC-N = T, NIL etc [MUST FIND KEY IN KEYLIST]
;; (get-set-append-delete-keyvalue :location   "new location" :OLD-KEYLIST '(A B C :LOCATION D OLD-VALUE (1 2 3 4) E F) :KEYLOC-N  T :val-nth 2 :append-value-p T :splice-old-new-values-p nil)
;;works=  (A B C :LOCATION D (OLD-VALUE "new location") (1 2 3 4) E F)          (A B C :LOCATION D (OLD-VALUE "new location") (1 2 3 4) E F)           (OLD-VALUE "new location")                 OLD-VALUE
;;
;;TESTING :DELETE-VALUE AND :SET-KEYLIST=NIL  TTT
;; :DELETE-VALUE
;; (get-set-append-delete-keyvalue :key4  :DELETE-VALUE  :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  old-value a b c d e) )
;;works= (:KEY4 A B C D E)     (:KEY4 A B C D E)     NIL    OLD-VALUE
;; (get-set-append-delete-keyvalue :key4  :SET-KEYLIST=NIL  :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  old-value a b c d e) )
;; (values return-keylist new-keylist return-value old-value). 
;; NIL NIL NIL OLD-VALUE

;;NTH, 
;; (get-set-append-delete-keyvalue :NTH    :DELETE-VALUE    :OLD-KEYLIST '(1 2 OLD-VALUE 3 4  5 (6 7)  8 9 ) :KEYLOC-N 2 )
;; works= (1 2 3 4 5 (6 7) 8 9)   (1 2 3 4 5 (6 7) 8 9)   NIL   OLD-VALUE
;;for complex :delete-value
;; (get-set-append-delete-keyvalue :key4  :DELETE-VALUE  :KEYLOC-N 2 :VAL-NTH 3 :old-keylist '(x y :key4 m n  old-value a b c d e) )
;; works= (X Y :KEY4 M N A B C D E)   (X Y :KEY4 M N A B C D E)   NIL   OLD-VALUE
;; :SET-KEYLIST=NIL
;; (get-set-append-delete-keyvalue :key4  :SET-KEYLIST=NIL  :KEYLOC-N 2 :VAL-NTH 3 :old-keylist '(x y :key4 m n  old-value a b c d e) )
;;works= NIL  NIL  NIL  OLD-VALUE
;; :DELETE-KEY&VALUE
;; (get-set-append-delete-keyvalue :key4  :DELETE-KEY&VALUE  :KEYLOC-N 2 :VAL-NTH 3 :old-keylist '(x y :key4 m n  old-value a b c d e) )
;; works= (X Y M N A B C D E)    (X Y M N A B C D E)    NIL   OLD-VALUE
;;


;; OLD-VALUE, :APPEND-VALUE-P T
;; (get-set-append-delete-keyvalue :key4  'new-value :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  old-value a b c d e) :append-value-p T :splice-old-new-values-p NIL )
;;works=  (:KEY4 (OLD-VALUE NEW-VALUE) A B C D E)    (:KEY4 (OLD-VALUE NEW-VALUE) A B C D E)    (OLD-VALUE NEW-VALUE)  OLD-VALUE
;;
;;  KEYLOC-N =T, :append-value-p=T, (:splice-old-new-values-p MAKES NO DIFF)
;; (get-set-append-delete-keyvalue :location  "location" :KEYLOC-N T :VAL-NTH 1 :old-keylist '(A B C :LOCATION OLD-VALUE D (1 2 3 4) E F) :append-value-p T :splice-old-new-values-p NIL )
;;
;; REPLACE OLD-VALUE, :append-value-p NIL 
;; (OLD-VALUE 1 2) is a list, :append-value-p T
;; (get-set-append-delete-keyvalue :key4  'new-value :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  (OLD-VALUE 1 2) a b c d e) :append-value-p T :splice-old-new-values-p NIL )
;; works= (:KEY4 (OLD-VALUE 1 2 NEW-VALUE) A B C D E)    (:KEY4 (OLD-VALUE 1 2 NEW-VALUE) A B C D E)      (OLD-VALUE 1 2 NEW-VALUE)    (OLD-VALUE 1 2)
;; :append-value-p T :splice-old-new-values-p NIL   
;;(get-set-append-delete-keyvalue :key4  '(NEW-VALUE X Y) :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  (OLD-VALUE 1 2) a b c d e) :append-value-p T :splice-old-new-values-p NIL )
;; works= (:KEY4 (OLD-VALUE 1 2 (NEW-VALUE X Y)) A B C D E)   (:KEY4 (OLD-VALUE 1 2 (NEW-VALUE X Y)) A B C D E)   (OLD-VALUE 1 2 (NEW-VALUE X Y))   (OLD-VALUE 1 2)
;; :append-value-p T :splice-old-new-values-p T 
;; (get-set-append-delete-keyvalue :key4  '(NEW-VALUE X Y) :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4  (OLD-VALUE 1 2) a b c d e) :append-value-p T :splice-old-new-values-p T )

;;ON TREEVIEW LISTS
;;  (setf *treevtl1 '((2 3 6 2) "2.3.6.2" :SL (((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))))
;;(get-set-append-delete-keyvalue :SL '(NEW-VALUE A B) :KEYLOC-N 2  :OLD-KEYLIST *treevtl1)
;; works= 
#|((2 3 6 2) "2.3.6.2" (:SL (NEW-VALUE A B)))
((2 3 6 2) "2.3.6.2" (:SL (NEW-VALUE A B)))
(NEW-VALUE A B)
(((2 3 6 2 1) "2.3.6.2.1" :VA O) ((2 3 6 2 2) "2.3.6.2.2" :SL (((2 3 6 2 2 1) "2.3.6.2.2.1" :VA P) ((2 3 6 2 2 2) "2.3.6.2.2.2" :VA Q) ((2 3 6 2 2 3) "2.3.6.2.2.3" :SL (((2 3 6 2 2 3 1) "2.3.6.2.2.3.1" :VA R) ((2 3 6 2 2 3 2) "2.3.6.2.2.3.2" :SL (((2 3 6 2 2 3 2 1) "2.3.6.2.2.3.2.1" :VA S))))) ((2 3 6 2 2 4) "2.3.6.2.2.4" :VA T))) ((2 3 6 2 3) "2.3.6.2.3" :VA U) ((2 3 6 2 4) "2.3.6.2.4" :VA V))|#



;;
;; SSS START HERE TESTING FOLLOWING OLD TESTS
;;==========OLD TESTS ===========
;;
;;when keyloc-n = 0
;; (progn (setf out nil)(get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :append-value-p T :new-begin-items '(begin items) :new-end-items '(end items) :put-value-after-items '(a b c) :put-key-after-items '(put key after items) :splice-old-new-values-p T ))
;;works= (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS)    (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS)   (OLD-VALUE NEW-VALUE)    >>> If keyloc-n = 0, no (put key after items) included. 
;;WHEN KEYLOC-N > 0
;; (progn (setf out nil)(get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :keyloc-n 3 :append-value-p T :new-begin-items '(begin items) :new-end-items '(end items) :put-value-after-items '(a b c) :put-key-after-items '(put key after items) :splice-old-new-values-p T ))
;; works=  (PUT KEY AFTER (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS))    (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS)   (OLD-VALUE NEW-VALUE)
;; same with  :splice-old-new-values-p = NIL
;; (progn (setf out nil)(get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :keyloc-n 3 :append-value-p T :new-begin-items '(begin items) :new-end-items '(end items) :put-value-after-items '(a b c) :put-key-after-items '(put key after items) :splice-old-new-values-p nil ))
;; works= (PUT KEY AFTER BEGIN ITEMS :K2 A B C OLD-VALUE NEW-VALUE END ITEMS)      (BEGIN ITEMS :K2 A B C OLD-VALUE NEW-VALUE END ITEMS)      (OLD-VALUE NEW-VALUE)
;; with :splice-key-value-in-list-p instead of :put-key-after-items, :splice-old-new-values-p T
;; 2017-06, works
;;(get-set-append-delete-keyvalue :k4  'new-value :keyloc-n 3 :append-value-p T :old-keylist '(1 2 3 4 5 6)  :new-begin-items '(begin items) :new-end-items '(end items) :put-value-after-items '(a b c) :splice-key-value-in-list-p '(splice key value in list) :splice-old-new-values-p T )
;;(SPLICE KEY VALUE (BEGIN ITEMS 1 2 3 :K4 A B C NEW-VALUE 4 5 6 END ITEMS) IN LIST)    (BEGIN ITEMS 1 2 3 :K4 A B C NEW-VALUE 4 5 6 END ITEMS)   (NEW-VALUE)    NIL
;; with :splice-key-value-in-list-p instead of :put-key-after-items, :splice-old-new-values-p NIL
;; (progn (setf out nil)(get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :keyloc-n 3 :append-value-p T :new-begin-items '(begin items) :new-end-items '(end items) :put-value-after-items '(a b c) :splice-key-value-in-list-p '(splice key value in list) :splice-old-new-values-p NIL ))
;;works=  (SPLICE KEY VALUE IN BEGIN ITEMS :K2 A B C OLD-VALUE NEW-VALUE END ITEMS LIST)   (BEGIN ITEMS :K2 A B C OLD-VALUE NEW-VALUE END ITEMS)    (OLD-VALUE NEW-VALUE)


;;SIMPLIER FORMS
;;replace old-value
;;  (get-set-append-delete-keyvalue :k2  'new-value)
;;works= (:K2 NEW-VALUE)  (:K2 NEW-VALUE)  NEW-VALUE
;;  (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value)
;; works= (:K2 NEW-VALUE) (:K2 NEW-VALUE)  NEW-VALUE
;;append old-value= nonlist
;; (get-set-append-delete-keyvalue :k2  'new-value :append-value-p T)
;; works= (:K2 (OLD-VALUE NEW-VALUE)) (:K2 (OLD-VALUE NEW-VALUE))   (OLD-VALUE NEW-VALUE)
;;append old-value=list
;;  (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :append-value-p T)
;; works= (:K2 (OLD-VALUE NEW-VALUE)) (:K2 (OLD-VALUE NEW-VALUE))  (OLD-VALUE NEW-VALUE)
;; add-value-p
;; (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :add-value-p T)
;; works= (:K2 (OLD-VALUE) NEW-VALUE)     (:K2 (OLD-VALUE) NEW-VALUE)     ((OLD-VALUE) NEW-VALUE)
;; :splice-old-new-values-p has no effect on :add-value-p
;; (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :add-value-p T :splice-old-new-values-p NIL)
;; result= (:K2 (OLD-VALUE) NEW-VALUE)      (:K2 (OLD-VALUE) NEW-VALUE)    ((OLD-VALUE) NEW-VALUE)
;; put-value-after-items
;; (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :append-value-p T :put-value-after-items '(a b c) )
;; works= (:K2 A B C (OLD-VALUE NEW-VALUE))    (:K2 A B C (OLD-VALUE NEW-VALUE))    (OLD-VALUE NEW-VALUE)
;; ::put-value-after-items WITH :splice-old-new-values-p NIL, 
;;(get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :append-value-p T :put-value-after-items '(a b c) :splice-old-new-values-p NIL )
;; works= (:K2 A B C OLD-VALUE NEW-VALUE)    (:K2 A B C OLD-VALUE NEW-VALUE)     (OLD-VALUE NEW-VALUE)
;;
;; (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :append-value-p T :put-value-after-items '(a b c) :NEW-BEGIN-ITEMS '(BEGIN 1 2 ) :NEW-END-ITEMS '(END X Y))
;; works=  (BEGIN 1 2 :K2 A B C (OLD-VALUE NEW-VALUE) END X Y)     (BEGIN 1 2 :K2 A B C (OLD-VALUE NEW-VALUE) END X Y)    (OLD-VALUE NEW-VALUE)


;; :GET
;; (get-set-append-delete-keyvalue :k2 '(old-value) :get)
;; works= (:K2 (OLD-VALUE))  (:K2 (OLD-VALUE)) (OLD-VALUE)
;;
;; If :put-key-after-items, with NIL
;; (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :append-value-p T :keyloc-n 4 :put-key-after-items '(x) :splice-old-new-values-p NIL)
;; works= (X NIL NIL NIL :K2 (OLD-VALUE NEW-VALUE))   (:K2 (OLD-VALUE NEW-VALUE))   (OLD-VALUE NEW-VALUE)
;; (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :append-value-p T :keyloc-n 4 :put-key-after-items '(x) :splice-old-new-values-p T)
;; works= (X NIL NIL NIL (:K2 (OLD-VALUE NEW-VALUE)))   (:K2 (OLD-VALUE NEW-VALUE))  (OLD-VALUE NEW-VALUE)
;; If :put-key-after-items, with list
;;   (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :append-value-p T :keyloc-n 2 :put-key-after-items '(a b c) :splice-old-new-values-p t)
;;  works= (A B (:K2 (OLD-VALUE NEW-VALUE)))  (:K2 (OLD-VALUE NEW-VALUE))  (OLD-VALUE NEW-VALUE)
;; (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :append-value-p T :keyloc-n 2 :put-key-after-items '(a b c) :SPLICE-OLD-NEW-VALUES-P NIL)
;; works= (A B :K2 (OLD-VALUE NEW-VALUE))   (:K2 (OLD-VALUE NEW-VALUE))   (OLD-VALUE NEW-VALUE)
;;NOTE: keyloc-n = 0, so A B C shouldn't be added
;; (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :put-key-after-items '(a b c))
;; works= (:K2 NEW-VALUE)  (:K2 NEW-VALUE) NEW-VALUE

;; if set not append
; ; (get-set-append-delete-keyvalue :k2 '(old-value) 'new-value :keyloc-n 4 :put-key-after-items '(a b c) )
;; works= (A B C NIL (:K2 NEW-VALUE))  (:K2 NEW-VALUE)  NEW-VALUE
;; :NTH  TESTS
;;set
;; (get-set-append-delete-keyvalue :NTH  NIL  'new-value :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 3 4 5 (6 7)  8 9 ) :KEYLOC-N 2 )
;;works= (A B (1 2 NEW-VALUE 4 5 (6 7) 8 9))      (1 2 NEW-VALUE 4 5 (6 7) 8 9)     NEW-VALUE
;; append
;; (get-set-append-delete-keyvalue :NTH  NIL  'new-value :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 3 4 5 (6 7)  8 9 ) :KEYLOC-N 2 :APPEND-VALUE-P T)
;;works= (A B (1 2 :NTH (3 NEW-VALUE) 4 5 (6 7) 8 9))    (1 2 :NTH (3 NEW-VALUE) 4 5 (6 7) 8 9)         (3 NEW-VALUE)
;;get
;; (get-set-append-delete-keyvalue :NTH  NIL  :get  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 3 4 5 (6 7)  8 9 ) :KEYLOC-N 2 )
;; works= (A B (1 2 3 4 5 (6 7) 8 9))     (1 2 3 4 5 (6 7) 8 9)     3

;; (get-set-append-delete-keyvalue :key2  NIL  :get  :OLD-KEYLIST '(1 (:key1 2 3 4) 5 (:key2 6 7)  8 9 ) :KEYLOC-N 1 )

;;TO PUT NEW KEY AND VALUE AT SPECIFIC PLACE OR END, USE KEYLOC-N
;; (get-set-append-delete-keyvalue :key3 'new-value :keyloc-n 3  :old-keylist '(begin :key1 a :key2 '(x y) this end) :keyloc-n 3 ) 
;;works= (BEGIN :KEY1 A :KEY3 NEW-VALUE :KEY2 (QUOTE (X Y)) THIS END)    (BEGIN :KEY1 A :KEY3 NEW-VALUE :KEY2 (QUOTE (X Y)) THIS END)    NEW-VALUE   NIL
;; (get-set-append-delete-keyvalue :key3 'new-value :keyloc-n 9  :old-keylist '(begin :key1 a :key2 '(x y) this end) :keyloc-n 3 )
;; works, with keyloc-n = 9, puts new key and value at end
;; (BEGIN :KEY1 A :KEY2 (QUOTE (X Y)) THIS END :KEY3 NIL :KEY3 NEW-VALUE)    (BEGIN :KEY1 A :KEY2 (QUOTE (X Y)) THIS END :KEY3 NIL :KEY3 NEW-VALUE)         NEW-VALUE  

;;GET-SET-APPEND-DELETE-KEYVALUE-IN-ORDERLY-NESTED-LIST
;;2016
;;ddd
(defun get-set-append-delete-keyvalue-in-orderly-nested-list (new-value  key-spec-lists  
                                                                  nested-lists 
                                                        &key append-keyvalue-p 
                                                        return-list-p add-value-p
                                                        test return-nested-list-p 
                                                        if-not-found-append-keyvalue-list-p
                                                        put-value-after-items 
                                                        begin-items    end-items 
                                                        splice-old-new-values-p
                                                        (max-list-length 1000)  
                                                        (if-not-found-append-key-value-p T)
                                                        ;;cur-level-list
                                                        (keyloc-n 0)
                                                        (val-loc-n 0)
                                                        put-key-after-items
                                                        splice-key-value-in-list-p
                                                        last-key-found-p 
                                                        new-keylist old-keylist return-value) 
  "In U-lists. Efficient way to get or set keyvalue for ORDERLY LISTS; but (1) MUST have exact loc of all keys keyloc-n MUST be a number.  Does not check for keys in any other location; (2) each new level MUST BE INSIDE THE VALUE PLACE OF THE PREVIOUS KEY;  (3) MUST be a key AT EVERY LEVEL; and (4) VALUE MUST be item in list following key.  RETURNS (values return-keylist return-nested-lists new-keylist return-value old-keylist last-key-found-p). If RETURN-LIST-P, return-keylist is the ENTIRE list containing key, otherwise same as new-keylist. KEY-SPEC-LISTS are lists of (key keyloc-n [optional val-loc-n]) from outermost to innermost (last) key. Eg of proper list (:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key5 OLD-VALUE)...)); key-spec-list= ((:key1 2)(:key2  2)(:key3 4)(:key5 0)) .IF-NOT-FOUND-APPEND-KEY-VALUE-P adds a key and keyvalue to innermost list if not found (but preceding key must be found). If last keyloc-n = 0, puts old previous keyvalue at end of new keylist. If last keyloc-n > 0, puts it first, then fills in nils before new key in last list which is new value of previous key. IF KEY = :NTH, then gets, sets, or appends number in keyloc-n place. Note: If second item in spec-list isn't number, uses default keyloc-n"

  (let*
      ((return-nested-lists)
       (new-return-nested-lists)
       (match-item)
       (spec-list (car key-spec-lists))
       (KEY (first spec-list))
       (new-spec-lists)
       (new-nested-list1)
       (new-nested-lists)
       (add-new-value-p)
       (cur-level-list)
       (list-head)
       (list-tail)
       (length-nnl)
       (key-not-found-p)
       (return-keylist)
       (item)
       (old-value)
       (new-keylist)
       (return-value)
       (old-keylist) 
       )
    ;;If second item in spec-list isn't number, use default keyloc-n
    (when  (second spec-list)
      (setf  keyloc-n (second spec-list)))
    ;;If third item in spec-list  use default val-loc-n
    (when  (third spec-list)
      (setf  val-loc-n (third spec-list)))
    
    ;;(afout 'out (format nil "key-spec-lists= ~A~%nested-lists= ~A~% keyloc-n= ~A val-loc-n= ~A"   key-spec-lists nested-lists keyloc-n val-loc-n))

#|    (when (and nested-lists (listp nested-lists) (numberp keyloc-n))
      (when (setf item (nth keyloc-n nested-lists))|#
    (cond
     ;;IN LAST LEVEL USE NON-ORDERLY FUNCTION IF keyloc-n = NIL OR T, etc
      ((or (not (numberp val-loc-n))(not (numberp val-loc-n)))
      (multiple-value-setq (return-keylist return-nested-lists new-keylist return-value  
                                           old-keylist last-key-found-p )
            (get-set-append-delete-keyvalue-in-nested-list new-value key-spec-lists nested-lists
                                                    :append-keyvalue-p append-keyvalue-p
                                                    :add-value-p add-value-p :test test
                                                    :return-list-p  return-list-p 
                                                    :return-nested-list-p return-nested-list-p 
                                                    (max-list-length 1000) 
                                                    :if-not-found-append-key-value-p
                                                    if-not-found-append-key-value-p 
                                                    :if-not-found-append-keyvalue-list-p
                                                    if-not-found-append-keyvalue-list-p 
                                                    (bottom-level-p t) ;;orig-list-items-leftn 
                                                    :put-key-after-items put-key-after-items
                                                    :put-value-after-items put-value-after-items 
                                                    :begin-items begin-items
                                                    :end-items end-items
                                                     :splice-key-value-in-list-p splice-key-value-in-list-p 
                                                     :splice-old-new-values-p splice-old-new-values-p
                                                     :recurse-for-key-p T
                                                     :last-key-found-p T))
      ;;(BREAK "end not numberp val-loc-n")
      ;;end not numberp val-loc-n
      )
      ((and nested-lists (listp nested-lists) (numberp keyloc-n))

       (when (setf item (nth keyloc-n nested-lists))
         (setf length-nnl (list-length nested-lists)  ;;5
               list-head  (butlast nested-lists  (-  length-nnl  val-loc-n)) ;; 5-2= 3 = (1 2 )
               list-tail (nthcdr  (+ keyloc-n  val-loc-n 1) nested-lists)) ;; 0 + 2 + 1 = 2 ;   (4 5)
                   
         ;;(afout 'out (format nil "HEAD-TAIL:keyloc-n ~A val-loc-n= ~A length-nnl= ~A~% item= ~A~%new-nested-lists= ~A~%list-head= ~A ~%list-tail= ~A" keyloc-n val-loc-n length-nnl item new-nested-lists list-head    list-tail))
        
         (cond
          ((or (equal key item)(equal key :NTH))
           ;;(afout 'out (format nil "ITEM=~A = KEY=~A" item key))

           (setf new-spec-lists (cdr key-spec-lists))
           ;;(afout 'out (format nil "new-spec-lists= ~A" new-spec-lists  ))

           (cond
            (new-spec-lists
             (setf  new-nested-lists (car (nthcdr (+ val-loc-n keyloc-n) nested-lists)))
             ;;(afout 'out (format nil " new-nested-lists= ~A" new-nested-lists ))
             (cond
              ((listp new-nested-lists)
               (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                                    return-value   old-keylist last-key-found-p )
                   (get-set-append-delete-keyvalue-in-orderly-nested-list new-value  new-spec-lists  
                                                                   new-nested-lists 
                                                                   :return-list-p return-list-p :test test
                                                                   :append-keyvalue-p append-keyvalue-p
                                                                   :max-list-length max-list-length
                                                                   :last-key-found-p   last-key-found-p
                                                                   :new-keylist new-keylist
                                                                   :old-keylist old-keylist
                                                                   :return-value return-value))

               (setf  return-nested-lists (append  ;;return-nested-lists 
                                                   list-head (list key)
                                                   (list new-return-nested-lists)
                                                   list-tail))

               ;;(afout 'out (format nil "After RECURSE, new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A~%FINAL: return-nested-lists= ~A" new-keylist  return-value new-return-nested-lists list-head list-tail new-keylist return-nested-lists))
               )
              (t (BREAK "ERROR-- KEY-VALUE  new-nested-lists in recurse set to key-value here] IS NOT A LIST")))
             ;;end new-spec-lists
             )
            ;;LAST KEY-LIST
            ;;last-key-found-p
            (t
             (setf old-value (nth (+ keyloc-n 1) nested-lists)
                   last-key-found-p T)
             (cond 
              ((not (equal key :NTH))
               (setf old-keylist (list key old-value)))
              (t (setf old-keylist (list old-value))
             ;;(afout 'out (format nil "LAST KEY FOUND= key= ~A  old-keylist= ~A~%old-value= ~A" key old-keylist old-value))
             (multiple-value-setq (new-keylist return-value)
                 (get-set-append-delete-keyvalue key old-value new-value  :keyloc-n keyloc-n
                                          :append-value-p append-keyvalue-p :test test
                                          :put-key-after-items put-key-after-items
                                          :splice-key-value-in-list-p splice-key-value-in-list-p))
             (setf return-nested-lists  (append list-head
                                                new-keylist
                                                list-tail))
             (when return-list-p
               (setf return-keylist return-nested-lists))
             ;;(afout 'out (format nil "AFTER LAST-KEY-FOUND: list-head= ~A~%new-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A" list-head new-keylist list-tail return-nested-lists))    
             ;;end  cond
             ))))  ;;HERENOW
           ;;end key = item
           )
          ;; (( "IS ITEM LIST"))
          ;;ITEM IS A LIST and 
          ((and (listp item)
                (not (member key item :test  test))
                (= (list-length new-spec-lists) 1)  if-not-found-append-key-value-p)
           (setf   new-keylist (append item (list key new-value))
                   return-nested-lists (append  ;;return-nested-lists 
                                                list-head 
                                                new-keylist
                                                (list new-return-nested-lists)
                                                list-tail))
           ;;(afout 'out (format nil "AFTER ITEM IS A LIST LAST-KEY-FOUND: list-head= ~A~%new-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A" list-head new-keylist list-tail return-nested-lists))         
           )
          ;;KEY NOT= ITEM
          ((and (= (list-length new-spec-lists) 1)  if-not-found-append-key-value-p)
           (multiple-value-setq (new-keylist return-value)
               (get-set-append-delete-keyvalue key old-value new-value :keyloc-n keyloc-n
                                        :append-value-p append-keyvalue-p :test test
                                        :put-key-after-items put-key-after-items
                                        :splice-key-value-in-list-p splice-key-value-in-list-p))

           ;;(BREAK "new-value if not found")
           (setf return-nested-lists  (append list-head 
                                              (list item key return-value)
                                              list-tail))
           (when return-list-p
             (setf return-keylist return-nested-lists))
           )
          (t
           ;;(BREAK "NADA")
           (setf key-not-found-p T)
           ;;end not=item, cond
           ))
         ;;end when item= list (and should be because it is in the place of the key value for less than final key)
         )
       ;;end cond  
       ))

    (when (and (not (and (listp nested-lists)(numberp keyloc-n)))
               (= (list-length key-spec-lists) 1))
      (multiple-value-setq (new-keylist return-value)
          (get-set-append-delete-keyvalue key old-value new-value :keyloc-n keyloc-n
                                   :append-value-p append-keyvalue-p :test test
                                   :put-key-after-items put-key-after-items
                                   :splice-key-value-in-list-p splice-key-value-in-list-p))

      ;;(BREAK "(and (not (and (listp nested-lists)(numberp keyloc-n)))(= (list-length key-spec-lists) 1))")
      (cond 
       ((and (numberp keyloc-n)(= keyloc-n 0))
        (setf new-keylist (append new-keylist (list nested-lists))))
       (t (setf new-keylist (replace new-keylist (list nested-lists)))))

      (setf return-nested-lists  (append list-head
                                         new-keylist
                                         list-tail))
      ;;end when
      )      

    (values return-keylist return-nested-lists new-keylist return-value    
            old-keylist last-key-found-p )      
    ;;end let, get-set-append-delete-keyvalue-in-orderly-nested-list
    ))
;;TEST
;;  (progn (setf out nil) (get-set-append-delete-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE) k5 e) k6 f) xx) :if-not-found-append-key-value-p nil ))
;; works= 
;;  (:KEY4 NEW-VALUE)    (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 NEW-VALUE) K5 E) K6 F) XX)    NEW-VALUE   (:KEY4 OLD-VALUE)    T
;; for return-list-p
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE rest of list) k5 e) k6 f) xx) :return-list-p T :if-not-found-append-key-value-p nil ))
;; works= (:KEY4 NEW-VALUE REST OF LIST)    (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 NEW-VALUE REST OF LIST) K5 E) K6 F) XX)   (:KEY4 NEW-VALUE)   (:KEY4 NEW-VALUE)   (:KEY4 OLD-VALUE)   T
;; append-keyvalue-p
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE) k5 e) k6 f) xx) :if-not-found-append-key-value-p nil  :append-keyvalue-p T))
;; works= 
;;  (:KEY4 (OLD-VALUE NEW-VALUE))   (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 (OLD-VALUE NEW-VALUE)) K5 E) K6 F) XX)    (OLD-VALUE NEW-VALUE)   (:KEY4 OLD-VALUE)    T
;; if no old key or value and :if-not-found-append-key-value-p
;; keyloc-n > 0
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 3))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3  k5 e) k6 f) xx) :if-not-found-append-key-value-p T ))
;; keyloc-n = 0
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3  k5 e) k6 f) xx) :if-not-found-append-key-value-p T ))
;; TEST FOR :NTH
;; append
;; (progn (setf out nil) (get-set-append-delete-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:NTH 2)(:NTH  2)(:NTH 4)(:NTH 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE) k5 e) k6 f) xx) :if-not-found-append-key-value-p nil  :append-keyvalue-p T))
;; works=  (OLD-VALUE NEW-VALUE)     (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 (OLD-VALUE NEW-VALUE)) K5 E) K6 F) XX)     (OLD-VALUE NEW-VALUE)      (OLD-VALUE)     T
;; set
;;  (progn (setf out nil) (get-set-append-delete-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:NTH 2)(:NTH  2)(:NTH 4)(:NTH 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE) k5 e) k6 f) xx) :if-not-found-append-key-value-p nil  :append-keyvalue-p NIL))
;; works= (NEW-VALUE)    (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 NEW-VALUE) K5 E) K6 F) XX)   NEW-VALUE   (OLD-VALUE)   T
;;
;; (get-set-append-delete-keyvalue-in-orderly-nested-list "NEW LOCATION"   '((:DRIVE 2)(:LOCATION  2))  E-DRIVE :if-not-found-append-key-value-p T :return-list-p T) ;;  :append-keyvalue-p NIL)







;SET-KEY-SINGLEVALUE-IN-NESTED-LISTS
;;  (partially replaced by set-key-value and append-key-value) 
;;   This function can search deep nested lists 
;;
;;ddd
(defun set-singlekey-value-in-nested-lists (new-value  key  nested-lists 
                                                          &key append-keyvalue-p  (set-nth 1)
                                                          return-list-p (test 'my-equal)
                                                          (max-list-length 1000)
                                                          (keyloc-n 0)
                                                          splice-key-value-in-list-p
                                                          put-key-after-items
                                                          new-keylist old-keylist return-value key-found-p) 
  "In U-lists.lisp, SETS key-value FOR EVERY OCCURANCE OF KEY. Use set-key-value-in-nested-lists for key-spec-lists.   RETURNS (values new-keylist return-nested-lists  return-value  old-keylist) that matches key.  If RETURN-LIST-P, returned new-keylist is the ENTIRE list containing key..If APPEND-KEYVALUE-P, appends the new-value to the value following the key [If old value not a list, makes a list with both values.]  If set-nth, sets nth item after key to value (starts with 1).  [If  KEY NOT FOUND, RETURNS NILS for all values except return-nested-lists. [Note: this version does exhaustive search of tree including branches and items past the found key.  Not efficient if know lower level keys.]. SPLICE-KEY-VALUE-IN-LIST-P is a list to put the key/new-value (or if :NTH) at position keyloc-n.  put-key-after-items is a list of items to put before key/value. IF KEY = :NTH, then uses keyloc-n to find the item located at that position in the last list in recursion to operate on (not very useful?)."
  (let*
      ((return-nested-lists)
       (new-nested-lists)
       (new-nested-list)
       (new-nested-lists)
       (add-new-value-p)
       (old-value)
       )
    (cond
     ((listp nested-lists)
      (loop
       for item in nested-lists
       for item-n from 0 to max-list-length
       do
       ;;(afout 'out (format nil "item=  ~A key-found-p= ~A" item key-found-p))
       (cond
        ;;ADD-NEW-VALUE-P  SET VALUE FOR ITEM FOLLOWING LAST KEY  [only for next item after last key]
        (add-new-value-p  
         (setf add-new-value-p nil
               old-value item)
         (cond
          ((not (equal key :NTH))
           (setf old-keylist (list key item)))
          (t (setf old-keylist (list item))))

         (multiple-value-setq (new-keylist return-value)
             (get-set-append-delete-keyvalue key  new-value
                                         :keyloc-n keyloc-n :test test
                                         :append-value-p append-keyvalue-p
                                         :put-key-after-items put-key-after-items
                                         :splice-key-value-in-list-p splice-key-value-in-list-p))

         (setf  return-nested-lists (append return-nested-lists 
                                            new-keylist))
         ;;(BREAK "after add new value")
         ;;end add-new-value-p
         )
        ((listp item)
         (multiple-value-setq (new-keylist new-nested-lists 
                                           return-value   old-keylist key-found-p)
             (set-singlekey-value-in-nested-lists new-value  key  item
                                                  :append-keyvalue-p append-keyvalue-p
                                                  :max-list-length max-list-length
                                                  :key-found-p key-found-p
                                                  :new-keylist new-keylist 
                                                  :old-keylist old-keylist :return-value return-value
                                                  ))
         (setf return-nested-lists (append return-nested-lists (list  new-nested-lists)))
         ;;end listp item
         )
        ;;NOT A LIST 
        (t (cond
            ((or (funcall test item key) ;;was (my-equal item key) 
                 (and (equal key :NTH)
                      (= item-n keyloc-n)))
             (setf key-found-p T
                   add-new-value-p T)
             ;;(break "1b EQUAL")
             ;;(afout 'out (format nil "ITEM=KEY= ~a" ITEM))
             ;;end item = key
             )
            ;;not item = key
            (t (setf return-nested-lists (append return-nested-lists (list  item)))))
           ;;end item not listp, cond
           ))
       ;;end loop, nested-lists clause
       ))
     ;;NULL NESTED-LISTS (end recursions)
     (t  NIL
         ;;end null nested-lists
         ))
    ;;(afout 'out (format nil "return-nested-lists= ~A~%key-found-p= ~A" return-nested-lists key-found-p))
    (values  new-keylist return-nested-lists return-value    
            old-keylist key-found-p)         
    ;;end let, set-singlekey-value-in-nested-lists
    ))
;;TEST
;; APPEND
;; (set-singlekey-value-in-nested-lists 'NEW-VAL :KEY '((A B :KEY 11 C D)(D :KEY 22 E)(:KEY 33 E F)) :return-list-p T)
;; works= (:KEY NEW-VAL)         ((A B :KEY NEW-VAL C D) (D :KEY NEW-VAL E) (:KEY NEW-VAL E F))           (:KEY NEW-VAL)          (:KEY 33)          T
;;(progn (setf out nil)(set-singlekey-value-in-nested-lists  'NEW-VALUE ':key2 '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)) :append-keyvalue-p t))
;;works=(:KEY2 (0.55 NEW-VALUE))    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55 NEW-VALUE)) :KEY3 (44) (:KEY4 0.95))    (:KEY2 (0.55 NEW-VALUE))   (:KEY2 (0.55))    T
;;SET
;;   (set-singlekey-value-in-nested-lists  'NEW-VALUE ':key2 '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)))
;;works=  (:KEY2 NEW-VALUE)     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 NEW-VALUE) :KEY3 (44) (:KEY4 0.95))     (:KEY2 NEW-VALUE)    (:KEY2 (0.55))   T 
;; GET
;;  (set-singlekey-value-in-nested-lists  :get  ':key2 '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)))
;; works= (:KEY2 (0.55))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95))    (:KEY2 (0.55))   (:KEY2 (0.55))   T
;; 2 SAME KEYS, PICKS FIRST KEY ONLY--Use get-set-append-delete-keyvalue-in-nested-list instead
;; set not append
;;(progn (setf out nil) (set-singlekey-value-in-nested-lists 'NEW-VALUE  'A3  '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)(A4 "this is A4") x y) (list z))))
;;NOTE:-SETS BOTH A3 =  (A3 NEW-VALUE)    (:PCSYM-ELM-LISTS ((A3 NEW-VALUE) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 NEW-VALUE :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12) (A4 "this is A4") X Y) (LIST Z))    (NEW-VALUE)    (A3 "a vs not a")    T
;;
;;with 2 keys in nested list, sets BOTH VALUES.
;;WORKS= (A3 NEW-VALUE)    (:PCSYM-ELM-LISTS ((A3 NEW-VALUE) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 NEW-VALUE :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)))    (NEW-VALUE)     (A3 "a vs not a")    T




;;SET-KEY-VALUE-IN-NESTED-LISTS
;; DEPRECIATED--REPLACED BY GET-SET-APPEND-DELETE-KEYVALUE-IN-NESTED-LIST
;;  (partially replaced by set-key-value and append-key-value) 
;;   This function can more specifically search deep nested lists 
;;  ESPECIALLY GOOD FOR MULTI-LEVEL KEYS
;;
;;ddd
(defun set-key-value-in-nested-lists (new-value  key-spec-lists  nested-lists 
                                                &key append-item  append-keyvalue-p (test 'my-equal)
                                                final-key-inside-keylist-p 
                                                subst-new-keylist-p no-return-extra-p) 
  "In U-lists.lisp, DEPRECIATED (but works ok?)--use get-set-append-delete-keyvalue-in-nested-list SETS key-value. USE SET-KEY-VALUE (newer) or APPEND-KEY-VALUE for many cases. USE THIS FOR MULTI-LEVEL KEYS.  RETURNS (values new-keylist return-nested-lists  return-value  final-spec-list old-keylist) that matches key-spec. The spec-lists is a list of 2 item spec lists. (key set-nth)  If  key = T, searches entire list of lists for key  If set-nth = T, searches entire sublist for the key.. Extra items are extra non-list items on all level lists that might contain important info. Can process infinite? levels of key lists. Can match with equal or string-equal automatically. The spec-lists are arranged FROM OUTER-MOST LIST to inner-most list. FINAL-KEY-INSIDE-KEYLIST-P means that the last key is inside a list--so items in outer list won't cause a false match. APPEND-ITEM is appended to the end of the list containing the final key.If APPEND-KEYVALUE-P, appends the new-value to the value following the key [If old value not a list, makes a list with both values.]     NOTE: final keylists should contain a value--even if nil in the set-nth position so it can be replaced UNLESS the new-value is a new keylist and subst-new-keylist-p = t. If value is in place, replaces it with new-value. "
  (let*
      ((new-keylist)
       (return-key)
       (return-value)
       (return-nested-lists)
       (old-keylist)
       (match-item)
       (spec-list (car key-spec-lists))
       (KEY (first spec-list))
       (SET-NTH (second spec-list))
       (new-spec-lists (cdr key-spec-lists))
       (final-spec-list)
       (nested-list-length (list-length nested-lists))
       (new-nested-lists)
       (match-item-lo)
       (new-value-lo  )
       (length-item-lo)
       (new-nested-lists)
       (item-loval)
       )
    (unless  set-nth (setf set-nth 0))
    (cond
     (nested-lists
      (cond
       ;;SPEC IS TO SEARCH EVERY LIST AT THIS LEVEL
       ((or (equal key t) (equal set-nth t))
        ;;(afout 'out (format nil "NEW CALL TO T  key= ~A~% (car of list= ~A~%" key (car nested-lists)))   
        (loop
         for item  in nested-lists
         ;;with new-value2 
         with new-nested-lists 
         do
         ;;(afout 'out (format nil "T OUTER-LOOP key= ~A~%  item= ~A~%" key item))
         ;;test to see what the spec-list indicates
         (cond
          ((and item (listp item))

           ;;note: this may call other recursive calls
           (multiple-value-setq (new-keylist  new-nested-lists return-value 
                                              final-spec-list old-keylist)
               (set-key-value-in-nested-lists new-value new-spec-lists item
                                              :append-item append-item 
                                              :final-key-inside-keylist-p final-key-inside-keylist-p 
                                              :subst-new-keylist-p subst-new-keylist-p
                                              :no-return-extra-p no-return-extra-p
                                              ))
           ;;works?
                (setf return-nested-lists (append return-nested-lists (list  new-nested-lists)))
           )
          ;;may be non-list items such as other keys providing info re: found outer lis.
          (t (setf return-nested-lists (append return-nested-lists (list  item))))) 
         ;;end loop set-nth = t
         ))
       ;;AT LOWEST LIST, SEARCH FOR KEY-VALUE
       ((and (null new-spec-lists) key)
        (loop
         for item-lo in nested-lists
         for n from 0 to nested-list-length
         with match-item-lo 
         with new-value-lo  
         with length-item-lo 
         do
         ;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~A  match-item-lo= ~A~% set-nth= ~A~%nested-lists= ~A~% IN find-key-value-in-lists " key match-item-lo set-nth nested-lists))
         (cond
          ;;SEARCH INSIDE A NESTED LIST -- NOT IN OUTER LIST FOR KEY
          ((and (listp item-lo) (null final-key-inside-keylist-p))
           (setf  length-item-lo (list-length item-lo))       
           (unless (>= set-nth  length-item-lo))
           (setf match-item-lo (nth set-nth item-lo))
           (cond
            ((funcall test key match-item-lo) ;;was(my-equal key match-item-lo)
             ;;was (or (equal key match-item-lo) (if (stringp match-item-lo) (string-equal key match-item-lo)))

             ;;(afout 'out (format  nil "2 LOWEST LEVEL TEST key= ~A  match-item-lo= ~A~% set-nth= ~A~%nested-lists= ~A~% item-lo= ~A~%IN find-key-value-in-lists " key match-item-lo set-nth nested-lists item-lo))

             ;;use RETURN-VALUE because will be NIL IF NO MATCH (and return NIL)
             ;;  also set old-keylist to item-lo
             (setf return-value new-value
                   old-keylist item-lo)
             ;;PUT NEW-VALUE IN PROPER PLACE AND/OR ADD APPEND-ITEM-LO
             ;;do I replace an item-lo or just add it.
             (cond
              ;;if subst-new-keylist-p subst new-value (a list) for old keylist.
              ((and subst-new-keylist-p (listp new-value))
               (setf new-keylist new-value))
              ;;if  set-nth, replace that item-lo with new-value
              (set-nth
               ;;problem with replace modifying item-lo permenantly and therefore old-keylist
               ;;zzzz
               ;;modified 2016-04 to add append-keyvalue-p option
               (cond
                (append-keyvalue-p
                 (setf item-loval (second item-lo))
                 (cond
                  ((listp item-loval)
                   (setf new-value-lo (append item-loval (list new-value))))
                  (t (setf new-value-lo (list item-loval new-value)))))
                (t (setf  new-value-lo new-value)))

               ;;2015-05 was (list new-value)= extra parens
               (setf new-keylist (replace-list item-lo (+ set-nth 1) new-value-lo)))
              (t nil))  ;;was (setf new-keylist nil)))
             ;;if append-item (= a value) append to the new-keylist
             (if append-item
                 (setf final-spec-list (append final-spec-list (list append-item)))) 
             ;;set final return items
             (setf final-spec-list (list key set-nth)
                   return-nested-lists (replace-list nested-lists n new-keylist)) 
             ;;2015 was (list new-keylist))) => extra parens around list
             ;;  (return)
             )
            (t nil))
           ;;end listp item-lo
           )
          ;;if not list, search current list for item-lo, just check to see if item-lo = key
          ;;problem if an item-lo matches key but real key is inside a later list
          ;;SEARCHES OUTER LIST FOR KEY MATCH--NOT AN INNER LIST
          (final-key-inside-keylist-p
           (cond
            ((funcall test key item-lo) ;;was(my-equal key item-lo)
             ;;was (or (equal key item-lo) (if (stringp item-lo) (string-equal key item-lo)))
             ;;PUT NEW-VALUE IN PROPER PLACE AND/OR ADD APPEND-ITEM-LO
             ;;do I replace an item-lo or just add it.
             (cond
              ;;if subst-new-keylist-p subst new-value (a list) for old keylist.
              ((and subst-new-keylist-p (listp new-value))
               (setf new-keylist new-value))
              ;;if  set-nth, replace that item-lo with new-value
              (set-nth
               (setf  new-value-lo (list new-value)
                      ;;xxx causes a problem perm changing item-lo value??
                      new-keylist (replace-list item-lo    (+ set-nth 1) new-value-lo)))
              #|                    (replace item-lo new-value-lo :start1
                                         (+ set-nth 1) :end1 (+ set-nth 2))))|#
              (t (setf new-keylist nil)))

             ;;if append-item (= a value) append to the new-keylist
             (if append-item
                 (setf new-keylist (append new-keylist (list append-item))))
             ;;added works
             (setf return-nested-lists (append return-nested-lists (list  new-keylist)))
             (return))
            (t  nil)))    ;; (setf new-nested-list (append new-nested-list (list item-lo))))))
          (t nil) ;; (setf new-nested-list (append new-nested-list (list item-lo))))
          ;;end cond, lo loop, equal null new-spec-lists
          )))
       ;;SPEC IS TO SEARCH THIS LEVEL (not last level) BY KEY at SET-NTH
       ((and  new-spec-lists  key)
        ;;(afout 'out (format nil "OUTER-LOOP SEARCH new-spec-lists= ~A~%" new-spec-lists))         
        ;;check each list at this level
        (loop
         for item in nested-lists
         with list-length  
         with match-item 
         with new-nested-lists 
         do
         ;;for each sublist, check set-nth
         ;;(afout 'out (format nil "KEY OUTER-LOOP key= ~A~% item= ~A~%new-spec-lists= ~A~%" key item new-spec-lists))
         (cond
          ((listp item)
           (setf  list-length (list-length item))     
           (unless (>= set-nth  list-length))
           (setf match-item (nth set-nth item))
          ;; (setf *out (append *out  (format  nil "OUTER LOOP TESTING key= ~A  match-item= ~A~%  IN find-key-value-in-lists" key match-item)))
           (cond
            ((funcall test key match-item) ;;was(my-equal key match-item)
             ;;was (or (equal key match-item) (if (stringp match-item) (string-equal key match-item)))
             ;;;yyy
             (multiple-value-setq (new-keylist  new-nested-lists return-value 
                                                final-spec-list old-keylist)
                 (set-key-value-in-nested-lists new-value new-spec-lists item
                                                :append-item append-item 
                                                :final-key-inside-keylist-p final-key-inside-keylist-p 
                                                :subst-new-keylist-p subst-new-keylist-p
                                                :no-return-extra-p no-return-extra-p))
             ;;works
             (setf return-nested-lists (append return-nested-lists (list new-nested-lists)))
   ;;zzzz sss start here
             ;;(setf *out (append *out  (format nil "ON RECURSE RETURN return-value= ~A~% return-key=~A~%" return-value return-key)))
             ;;was   (return)
             )
            (t (setf return-nested-lists (append return-nested-lists (list item)))))
           ;;end listp item
           )
          (t 
           (setf return-nested-lists (append return-nested-lists (list (list item))))))
         ;;end loop, equal new-spec-lists
         ))
       ;;IF TOP LEVEL NONE-OF-ABOVE
       (t (setf return-nested-lists (append return-nested-lists (list (list :NTH new-value))))))
      ;;end nested-lists
      )
     (t (cond
         (key-spec-lists
          (if (listp key-spec-lists)
              (setf new-keylist (list (caar  key-spec-lists) new-value)
                    return-nested-lists (list new-keylist))
            (setf new-keylist (list  key-spec-lists  new-value)
                  nested-lists (list new-keylist))))
         (t (setf new-keylist (list :NTH new-value)
              return-nested-lists (list new-keylist))))
        ;;end outer t, cond
        ))
    ;;end find-key-value-in-lists

;;2015-05 added bec wouldn't add keylist to a simple list with no key already in list
   (if (and (null return-value) (null new-keylist) key new-value )
        (setf new-keylist  (list key new-value)
              return-nested-lists (append nested-lists (list new-keylist))))
             ;; return-nested-lists nil)
    ;;end set-key-value-in-nested-lists
    (values new-keylist return-nested-lists  return-value  final-spec-list old-keylist) 
    )) 
;;TEST
;;2016-04 added :append-keyvalue-p functionality
;; (set-key-value-in-nested-lists  77 '((:key2 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)) :append-keyvalue-p t)
;;works= (:KEY2 (0.55 77))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55 77)) :KEY3 (44) (:KEY4 0.95))   77   (:KEY2 0)   (:KEY2 (0.55))
;;
;; (set-key-value-in-nested-lists 0.50 '((:PCSYM-ELM-LISTS  0)(B3 0)) '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12))) :final-key-inside-keylist-p T)

;;SSS START HERE MAKING IT APPEND INSIDE FIRST LEVEL OR
;;  USE ANOTHER FUNCTION (APPEND-KEY-VALUE OR SET-KEY-VALUE)
;; (setf mother '("MOTHER" "mother" CS1-1-1-1 (datalist this :key1 "that") NIL :info "this is info" :key3 (a b c)))
;; (set-key-value-in-nested-lists  "new-value"  `((:key1  0 T)) mother :append-keyvalue-p T :final-key-inside-keylist-p T) =
;;doesn't work right, results =  (:KEY1 "new-value")  ("MOTHER" "mother" CS1-1-1-1 (DATALIST THIS :KEY1 "that") NIL :INFO "this is info" :KEY3 (A B C) (:KEY1 "new-value"))


;;  (set-key-value-in-nested-lists 77 '((:key1 0)) '("classX1end" 0.22 (:NTH 0.55) (:NTH 0.55) (:NTH 0.55)))

;;test append-item -- only replaces items after the key??
;; (set-key-value-in-nested-lists  77 '((:key2 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)) :append-item t)
;; (set-key-value-in-nested-lists  77 '((:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 0.55) :KEY3 (44) (:KEY4 0.95)) :append-item t)
;;(:KEY4 77)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 0.55) :KEY3 (44) (:KEY4 77))   77   (:KEY4 0)  (:KEY4 0.95)

;;note key-spec-list wrong below, so appends list with key1; MUST BE A NESTED LIST
;; (set-key-value-in-nested-lists  77 '((:key1 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 0.55) :KEY3 (44) (:KEY4 0.95)))
;; result= ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 0.55) :KEY3 (44) (:KEY4 0.95) (:KEY1 77))

;;first key NOT in main list
;;  (progn (setf out nil) (set-key-value-in-nested-lists 'this-value '((a 0)(b 0)(c 0)) '( (s 1 2 (t 3 4 (u 5 6 7 )))(a (n1 2) (b (mm 33 ) (c 1 2 3  (l i s t )) (nn 22)) ( extra ))))  )
;;works, returns 1- (C (THIS-VALUE) 2 3 (L I S T))  2-  ((S 1 2 (T 3 4 (U 5 6 7))) (A (N1 2) (B (MM 33) ((C (THIS-VALUE) 2 3 (L I S T))) (NN 22)) (EXTRA)))  3-  THIS-VALUE  4-  (C 0)    5- (C 1 2 3 (L I S T))
;; first key IN MAIN list
;; (progn (setf out nil) (set-key-value-in-nested-lists 'this-value '((a T)(b 0)(c 0)) '( s 1 2 (t 3 4 (u 5 6 7 )) a ((n1 2) (b (mm 33 ) (c 1 2 3  (l i s t ))) (nn 22)) ( extra )))  )  
;;SSS = doesn't work (ME-USE GET-SET-APPEND-DELETE-KEYVALUE-IN-NESTED-LIST.
;;
;;  (progn (setf out nil)  (set-key-value-in-nested-lists 'this-value  '(("iAcademicMotivation.java" 1)( "acmESOCSTudy" 0)) '((PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight"))))) ;;(fout out)) ;; :return-list-p  t))
;;WORKS, returns 1-("acmESOCSTudy" (THIS-VALUE) "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")   2-((PC-INSTANCES "iAcademicMotivation.java" ("[]questionInstancesArray1)") ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight") (("acmESOCSTudy" (THIS-VALUE) "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight"))))   3- THIS-VALUE  4- ("acmESOCSTudy" 0)   5-("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")
;;
;;  (set-key-value-in-nested-lists 'this '((5 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))))
;; returns (THIS 1 (1 6 (QUOTE (A B))))  ((THIS 1 (1 6 (QUOTE (A B))))) (THIS)  (5 0) NIL

;; (progn (setf out nil) (set-key-value-in-nested-lists 'that-value '((this 0))  '((a b)(c (1 2))(this (3 4 5))(x y))))
;;  (set-key-value-in-nested-lists '((x 0))  '((a b)(c (1 2))(this (3 4 5))(x y)) )
;; (set-key-value-in-nested-lists  '((5 0)) '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;;  use (replace-list  new-value2   (+ set-nth 1)) instead
;;(replace   '(5 1 (1 6 (QUOTE (A B)))) '(this) :start1 1)
;; (progn (setf out nil) (set-key-value-in-nested-lists  '((5 0)(1 0)) '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t))(fout out))
;; (set-key-value-in-nested-lists  '((5 0)(1 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))
;; (progn (setf out nil)  ( set-key-value-in-nested-lists '(( "iWorldviewFears.java" 1)("wovNoLove" 0)) *all-shaq-pc-instances  :return-list-p t))  
;; (progn (setf out nil)  (set-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances :find-outer-key "iWorldviewFears.java" :find-nth-first 1)) ;;  :return-list-p t))
;;
;; (progn (setf out nil) (multiple-value-setq (*testfn1 *testfn2 *testfn3 *testfn4) (set-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances  :find-outer-key  t   :return-list-p t) )(fout out)))
 
;;  (progn (setf out nil)  (set-key-value-in-nested-lists    "acmESOCSTudy" '(PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")) :return-list-p  t))
;;works, returns ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")  "acmESOCSTudy"  NIL  (PC-INSTANCES "iAcademicMotivation.java")

;;  (set-key-value-in-nested-lists 'this  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL NIL
;;  (set-key-value-in-nested-lists 'x '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns Y X NIL NIL
;;   (set-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL NIL
;; (set-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL NIL
;; (set-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  5 NIL NIL
;;(replace  '(0 1 2 3 4 5 6 7 8 9) '(a b c) :start1 5 :end1 7 :start2 1 :end2 3) = (0 1 2 3 4 B C 7 8 9)
;; (replace  '(0 1 2 3 4 5 (x y z) 7 8 9) '(a b (l m)) :start1 5 :end1 7 :start2 1 :end2 3) = (0 1 2 3 4 B (L M) 7 8 9)
;; (replace  '(0 1 2 3 4 5 (x y z) 7 8 9) '(a b (l m)) :start1 6 :end1 9 :start2 1 :end2 3) = (0 1 2 3 4 5 B (L M) 8 9)
;; (set-class-symval "classXX" .22 :key1)
;; ;;    (set-key-value-in-nested-lists 55  '((key1 T))  nil) = (KEY1 55) ((KEY1 55)) NIL NIL NIL
;;  (set-key-value-in-nested-lists 55 nil  nil) = (:NTH 55) ((:NTH 55)) NIL NIL NIL
;;  (set-key-value-in-nested-lists 55  '((key2 0)) '(1 2 (key1 22)(key2 33)(key3 0))) 
;; works= (KEY2 55)  (1 2 (KEY1 22) (KEY2 55) (KEY3 0))   55  (KEY2 0)  (KEY2 33)



;;MY-NTH
;; 2017
;;ddd
(defun my-nth (nth list &key (startn 0))
  "In U-lists. NTH can be negative relative to list-startn. 0 = startn.  RETURNS my-nth item or NIL STARTN is nth (startinf from 0) of reference point in list for my-nth."
  (let
      ((abs-nth)
       (my-nth-item)
       )
    (when (listp list)
      (setf my-nth-item (nth (+ startn nth) list))
      #|(setf abs-nth (- (list-length list) startn 1)
            my-nth-item (nth (- abs-nth nth) list))|#
      )
    my-nth-item
    ))
;;TEST
 ;; (my-nth  0  '(0 1 2 3 4 5 6 7 8) :startn 0)  = 0
 ;; (my-nth  0  '(0 1 2 3 4 5 6 7 8) :startn 1) = 1
;; (my-nth  -1 '(0 1 2 3 4 5 6 7 8) :startn 3) = 2
;; (my-nth  -1 '(a b) :startn 1) = A



;NTH-NTH
;;2018-02
;;ddd
(defun nth-nth (nth1 nth2 nested-list)
  "In U-lists RETURNS nth2 in the nth1 list of nested-lists"
  (when (listp nested-list)
    (let*
        ((nthlist (nth nth1 nested-list))
         (nthitem (nth nth2 nthlist))
         )
      nthitem
      ;;end nth-nth
      )))
;;TEST
;; (nth-nth 2 1 '((a)(b 2)(c 3))) = 3



;;EVAL-SYM
;;2020
;;ddd
(defun eval-sym (sym &key (convert-str-p T) eval-sym-value-p (nums-ok-p T))
  "In U-lists RETURNS (values  evaled-syms  evaled-values  non-evaled-syms rest-items)"
  (let
      ((sym-val)
       (sym-val-val)
       )
    ;;convert to sym??
    (when (and (stringp sym) convert-str-p)
      (setf sym (my-make-symbol  sym :if-num-return-num-p nums-ok-p )))
    (when (and (numberp sym) nums-ok-p )
      (setf sym sym
            sym-val sym
            sym-val-val sym))
    ;;eval the sym twice if both bound
    (when (and (symbolp sym)
           (boundp sym))
      (setf sym-val (eval sym))
      (when eval-sym-value-p
        (setf sym-val-val (eval sym-val)))
      )
    (values sym-val  sym-val-val sym) 
    ;;end let, eval-sym
    ))
;;TEST
;;new for nums
;; (eval-sym "99"  :eval-sym-value-p T) = 99 99 99
;; (setf test-val1 '(this)  test-sym1 'test-val1)
;; (eval-sym 'test-sym1 :eval-sym-value-p T)
;; works= TEST-VAL1  (THIS)  TEST-SYM1
;;for string
;; (eval-sym "test-sym1" :eval-sym-value-p T))
;; works= TEST-VAL1  (THIS)  TEST-SYM1
;; for number
;;  (eval-sym 99  :eval-sym-value-p T))
;; works= 99  99  99
;;  (eval-sym "99"  :eval-sym-value-p T))


;;EVAL-SYMS
;;2017
;;ddd
(defun eval-syms (list)
  "In U-lists RETURNS (values  evaled-syms  evaled-values  non-evaled-syms rest-items)"
  (let
      ((non-evaled-syms) 
       (evaled-sym)
       (evaled-syms)
       (evaled-value)
       (evaled-values)
       (rest-items)
       )
    (loop
     for item in list
     do   
    (cond
     ((and (symbolp item)
           (boundp item))
      (setf evaled-sym item
            evaled-value (eval evaled-sym)
            evaled-syms (append evaled-syms (list evaled-sym))
            evaled-values (append evaled-values (list evaled-value))))
     ((symbolp item)
      (setf non-evaled-syms (append non-evaled-syms (list item))))
     (t (setf rest-items (append rest-items (list item)))))
     ;;end loop
     )
    (values  evaled-syms  evaled-values  non-evaled-syms rest-items)
    ;;end let, eval-syms
    ))
;;TEST
;;  (setf  g1 '(this) g2 '(that))
;;  (eval-syms '(1  a b g1 xx g2  4 5 6 '(what)))
;; works=  (G1 G2)     ((THIS) (THAT))    (A B XX)    (1 4 5 6 (QUOTE (WHAT)))




;;NESTED-LIST-P
;; 2017
;;ddd
(defun nested-list-p (list &key return-all-nested-lists-p )
  "In U-lists, RETURNS (values return-value list-length all-nested-lists nested-list-nths )  If return-all-nested-lists-p returns last2. return-value=boolean"
  (let 
      ((return-value)
       (all-nested-lists)
       (nested-list-nths)
       (list-length (list-length list))
       )
    (loop
     for item in list
     for n from 0 to list-length
     do
     (cond
      ((listp item)
       (setf return-value T)
       (cond
        (return-all-nested-lists-p 
         (setf all-nested-lists (append all-nested-lists (list item))
               nested-list-nths (append nested-list-nths (list n))))
        (t (return))))
      (t nil))
     ;;end loop
     )
     (values return-value list-length all-nested-lists nested-list-nths )
     ;;end let, nested-list-p
     ))
;;TEST
;;works
;; (nested-list-p '(A B C D)) = NIL 4 NIL NIL
;; (nested-list-p '(A B (D E) F (G) H (I (J)K) L)) = T 8 NIL NIL
;; (nested-list-p '(A B (D E) F (G) H (I (J)K) L) :return-all-nested-lists-p T) 
;; = T    8   ((D E) (G) (I (J) K))    (2 4 6)
;; (nested-list-p '(A B C D) :return-all-nested-lists-p T) = NIL 4 NIL NIL
      



;;NUMBER-LIST-P
;;2018
;;ddd
(defun number-list-p (list &key stop-if-not-all-numbers-p  )
  "In   RETURNS (values list-is-all-numbers-p numbers non-numbers)  If stop-if-not-all-numbers-p, returns only items up to first non-number."
  (let
      ((non-numbers)
       (numbers)
       (list-is-all-numbers-p)
       )
    (when (and list (listp list))
      (loop
       for item in list
       do
       (cond
        ((numberp item)
         (setf numbers (append numbers (list item))))
        (t (setf non-numbers (append non-numbers (list item)))
           (when stop-if-not-all-numbers-p
             (return))))                 
       ;;end loop
       )
      (when (null non-numbers)
        (setf list-is-all-numbers-p T))
      ;;end when
      )
    (values list-is-all-numbers-p numbers non-numbers)
    ;;end let, number-list-p
    ))
;;TEST
;; (number-list-p '(1 2 3 99)) = T  (1 2 3 99)  NIL
;; (number-list-p '(1 2 B 99)) = NIL  (1 2 99)  (B)
;; (number-list-p '(1 2 NIL 99)) = NIL (1 2 99) (NIL)
;; (number-list-p  99) = NIL NIL NIL
;; (number-list-p  NIL) = NIL NIL NIL




;;LIST-EVALED-LIST-ITEMS
;;2018-02, modified 2019
;;ddd
(defun list-evaled-list-items (list)
  "In U-lists, RETURNS (newlist evaled-items notevaled-items) Newlist= list of appended evaled bound items in list. Use for COMPOSITE LISTS OF LISTS. Reduces memory, keeps composite list updated if sublists have changed."
  (let
      ((newlist)
       (evaled-items)
       (not-evaled-items)
       )
    (loop
     for item in list
     do
     (cond
      ((and item (symbolp item)(boundp item))
       (setf newlist (append newlist (eval item))
             evaled-items (append evaled-items (list item))))
      (t (setf not-evaled-items (append not-evaled-items (list  item)))))
     ;;end loop
     )
    (values newlist evaled-items not-evaled-items)
    ;;end let, list-evaled-list-items
    ))
;;TEST
;; (setf  a1 '(1 2) a2 '(3 4) a3 '(a b c))
;; (list-evaled-list-items '(a1 nil a2 bb a3 cc))
;; works= (1 2 3 4 A B C)    (A1 A2 A3)   (NIL BB CC)
                 



;;NULL-LIST-P
;;2020
;;ddd
(defun null-list-p (list &key (null-strings '("" " " "  ")))
  "U-lists If a list = (nil) or any number of nils or (nil) with NO NON-NIL elements,  RETURNS T Note: To consider any string non-null, set null-strings to nil."
  (let*
      ((result T)
       (first-non-nil-item)
       )
    (loop
     for item in list
     do
     (let*
         ((item-result)
          )
   (cond
     ((listp item)
      (multiple-value-setq (item-result first-non-nil-item)
          (null-list-p item)))
     ((and (stringp item)(member item null-strings :test 'string-equal))
      (setf item-result T))      
     ((null item) 
      (setf item-result T))
     (t (setf first-non-nil-item item)))
      (when (null item-result)
        (setf result nil)
        (return))
     ;;end let,loop
     ))
    (values result first-non-nil-item)
    ;;end let, null-list-p
    ))
;;TEST
;; (null-list-p '(nil (nil (nil (nil (7)))) nil nil))
;; works= NIL  7
;; (null-list-p '(nil (nil (nil (nil (nil)))) nil nil))
;; works = T  NIL


;;APPEND-INCL-NIL
;;2019
;;ddd
(defun append-incl-nil (list item &key omit-if-nil-p) 
  "U-lists Eg.  If list = NIL item= '(1 2), sets new-list = (NIL 1 2), otherwise appends as append does.  RETURNS  new-list  OMIT-IF-NIL-P acts same as append, returns (1 2). 
 Uses key so can easily change whether or not to include NIL programatically."
  (let*
      ((new-list)
       )
    (cond
     ((and (null list) (null omit-if-nil-p))
      (setf new-list (append (list NIL) item)))
     (t (setf new-list (append list item))))
    ;;end let, append-incl-nil
    ))
;;TEST
;; (append-incl-nil '(a b c) '(1 2 3)) = (A B C 1 2 3)
;; (append-incl-nil NIL '(1 2 3)) = (NIL 1 2 3)
;; (append-incl-nil NIL '(1 2 3)  :omit-if-nil-p T)  = (1 2 3)



;;GET-ITEMS-BETW-KEYS
;;2020
;;ddd
(defun get-items-betw-keys (key1-list key2-list nested-list  &key (test 'my-equal) )
  "U-lists  Gets items between any member of  key1-list and any member of key2-list. RETURNS (values  found-items found-nth) found-nth is in final list, starting with 1. NOTE: Keys should be IN ORDER of PREFERENCE. Uses first found key neach list."
  (let*
      ((found-key1-p)
       (found-items)
       (found-nth)
       )
    (loop
     for item in nested-list
     for n from 1 to 1000
     do
    (cond
     ((and (listp item)(null found-key1-p))
      (multiple-value-setq (found-items found-nth)
            (get-items-betw-keys key1-list key2-list item))
      (when found-items
        (return)))
     (found-key1-p
      (cond
       ((member item key2-list :test test)
        (return))
       (t (setf found-items (append found-items (list item)))))
      )
     ((member item key1-list  :test test)
      (setf found-key1-p T
            found-nth n))
     (t nil))
     ;;end loop
     )
    (values  found-items found-nth)
    ;;end let, get-items-betw-keys
    ))
;;TEST
;; (get-items-betw-keys '(k1) '(k2) '(a b k1 1 2 3 4 k2 c d))
;; works= (1 2 3 4)   3
;; (get-items-betw-keys '(k1) '(k2)  '(m d o (22 33 44) ((a b k1 1 2 3 4 k2 c d) aa bb) ))
;; works= (1 2 3 4)  3
;; (get-items-betw-keys '(k1) '( xx  k2 d)  '(m d o (22 33 44) ((a b k1 1 2 3 4 k2 c d) aa bb) ))
;; works= (1 2 3 4)  3





;;GET-LIST-WITH-MULTI-KEYS
;;2020
;;ddd
(defun get-list-with-multi-keys (keys  nested-list  &key (test 'my-equal) 
                                           (max-items 1000))
  "U-lists  Returns a complete LIST that includes ALL KEYS IN ORDER. RETURNS: (values  found-items found-nth) found-nth is in final list, starting with 1. NOTE: Keys should be IN ORDER of PREFERENCE. Uses first found key neach list."
  (let*
      ((found-all-keys-p)
       (n-found-keys 0)
       (found-list)
       (n-keys (list-length keys))
       )
    ;;SEARCH MAIN LIST FOR ALL KEYS
    (loop
     for key in keys
     for n from 1 to n-keys
     do
     (cond
      ((member key nested-list :test test)
       (incf n-found-keys))
      (t (setf n-found-keys 0)))
     (when  (= n-found-keys n-keys)
       (setf found-all-keys-p T
             found-list nested-list)
       (return))
     ;;end first loop
     )
    ;;IF NOT FOUND IN MAIN LIST, FIND LISTS and RECURSE ON THEM
    (unless found-all-keys-p
      (loop
       for item in nested-list
       for n from 1 to max-items
       do
       (when (listp item)
         (setf found-list
               (get-list-with-multi-keys keys item :max-items max-items)))
       (when found-list
         (setf found-all-keys-p T)
         (return))
       ;;(afout 'out (format nil "FOR item= ~A in nested-list= ~A ~% found-list= ~A" item nested-list found-list))
       ;;end ,loop,unless
       ))
    ;;(break "end funct")
    (values  found-list )
    ;;end let, get-list-with-multi-keys
    ))
;; one level
;; (get-list-with-multi-keys '(k1 k2 k3) '(a b k1 d e k2  f  k3  g h i))
;; works= (A B K1 D E K2 F K3 G H I)
;; 3 levels
;; ;; (get-list-with-multi-keys '(k1 k2 k3) '(zz (a b k1 ( d (x (1 2 k1 3 4 k2 5 k3 6) y) e ) xx k2)  f  k3  g h i))
;; works = (1 2 K1 3 4 K2 5 K3 6)




;;MOVE-NESTED-LIST-ITEMS
;;2020
;;ddd
(defun move-nested-list-items (move-key  to-keylist-key 
                                        nested-lists-list &key from-keylist-key
                                        (return-all-return-nested-lists-p T)
                                        (move-key&value-p T)
                                        from-keylist to-keylist )
  "   RETURNS    INPUT:  "
  (let*
      ((all-moved-value-lists)
       (all-return-nested-lists)
       )
    (loop
     for nested-list in nested-lists-list
     do
     ;;(break "before")
     (multiple-value-bind (return-nested-lists  return-keylist  moved-value)
         (move-nested-list-item move-key  to-keylist-key nested-list 
                                :from-keylist from-keylist :to-keylist  to-keylist 
                                :move-key&value-p move-key&value-p 
                                :from-keylist-key from-keylist-key )
       (setf all-moved-value-lists (append all-moved-value-lists 
                                          (list moved-value)))
       (when return-all-return-nested-lists-p
         (setf all-return-nested-lists 
               (append all-return-nested-lists (list return-nested-lists))))
       ;;end mvb, loop
       ))
    (values all-moved-value-lists all-return-nested-lists)
    ;;end let, move-nested-list-items
    ))
;;TEST
;; (multiple-value-setq (*revised-value-lists *revised-file-pcsymval-lists) (move-nested-list-items  :CSVAL :PC   *file-pcsymval-lists))
;; (multiple-value-setq (*revised-value-lists *revised-file-pcsymval-lists2) (move-nested-list-items  :RNK :PC   *revised-file-pcsymval-lists))
;; (setf *file-pcsymval-lists *revised-file-pcsymval-lists2)
;; (setf *test-pc-lists1 '((CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :CSVAL "0.917" :RNK 3)     (INTIMATE ("INTIMATE" "INTIMATE vs NOT INTIMATE" CS2-1-1-99 NIL NIL :PC ("INTIMATE" "NOT INTIMATE" 1 NIL) :POLE1 "INTIMATE" :POLE2 "NOT INTIMATE" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-F-FRIEND NIL) (POLE2 NIL BEST-M-FRIEND NIL))) :CSVAL "0.750" :RNK 7.5) ))
;; (move-nested-list-items  :CSVAL :PC   *test-pc-lists1)
;; works= 
;;all-moved-value-lists=   ((:CSVAL "0.917") (:CSVAL "0.750"))
;;all-return-nested-lists= ((CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL)) :CSVAL "0.917") :RNK 3) (INTIMATE ("INTIMATE" "INTIMATE vs NOT INTIMATE" CS2-1-1-99 NIL NIL :PC ("INTIMATE" "NOT INTIMATE" 1 NIL) :POLE1 "INTIMATE" :POLE2 "NOT INTIMATE" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-F-FRIEND NIL) (POLE2 NIL BEST-M-FRIEND NIL)) :CSVAL "0.750") :RNK 7.5))



;;MOVE-NESTED-LIST-ITEM
;;2020
;;ddd
(defun move-nested-list-item (move-key  to-keylist-key 
                                        nested-lists &key from-keylist-key
                                        (move-key&value-p T)
                                        from-keylist to-keylist  )
  "U-lists   RETURNS (values return-nested-lists  return-keylist  moved-value)   INPUT:  from-keylist-key finds from-keylist in EVERY list in nested-list that contains it. Within that from-keylist. Then gets MOVE-KEY and IF MOVE-KEY&VALUE-P, its value,  and moves it to  to-keylist (which contains to-keylist-key.
  If (NULL FROM-KEYLIST-KEY), uses only move-key for finding original key&value"    
  (let*
      ((move-value)
       (new-from-list)
       (new-to-list)
       (new-nested-list)
       (delete-spec-list (cond 
                          (from-keylist-key (list (list from-keylist-key)(list move-key)))
                        (t (list move-key))))
       (append-spec-list  (list (list to-keylist-key)(list move-key)))
       (new-value1 (cond (move-key&value-p :DELETE-KEY&VALUE)
                         (t :DELETE)))
       (moved-value2)       
       (return-value)
       )
    ;;GET AND DELETE THE FROM-KEY&VALUE FROM FROM-KEYLIST
    (multiple-value-bind ( RETURN-KEYLIST1 return-nested-lists1 new-keylist1
                                          return-value1 return-old-keylist1 last-key-found-p1 
                                          old-value1)
        (get-set-append-delete-keyvalue-in-nested-list :DELETE-KEY&VALUE 
                                                      delete-spec-list nested-lists)
       (cond 
        (move-key&value-p 
         (setf  moved-value2 (list move-key old-value1)))
        ((listp return-value)
         (setf moved-value2 (second old-value1)))
        ;;is this right??
        (T moved-value2 old-value1))
      ;;(break "after delete") ;;HERE99

      ;;APPEND TO-KEYLIST WITH SAME (OLD-VALUE1)
    (multiple-value-bind (RETURN-KEYLIST return-nested-lists new-keylist
                                          return-value return-old-keylist last-key-found-p
                                          old-value append-spec-list MOVED-KEY&VALUE )
            (get-set-append-delete-keyvalue-in-nested-list moved-value2
                                                        to-keylist-key  return-nested-lists1
                                                   :simple-splice-key-value-in-keylist-p T)
      (values return-nested-lists  return-keylist  moved-value2)
    ;;end let, mvb,move-nested-list-item
    ))))
;;TEST
;; (move-nested-list-item :CSVAL   :PC    '(CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL))) :CSVAL "0.917" :RNK 3)) 
;;works= 
;;return-nested-lists= (CAREFOROTHERS ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL)) :CSVAL "0.917") :RNK 3)
;;return-keylist= ("CAREFOROTHERS" "CARE FOR OTHERS vs SELFISH" CS2-1-1-99 NIL NIL :PC ("CARE FOR OTHERS" "SELFISH" 1 NIL) :POLE1 "CARE FOR OTHERS" :POLE2 "SELFISH" :BESTPOLE 1 :BIPATH ((POLE1 NIL MOTHER NIL) (POLE1 NIL BEST-M-FRIEND NIL) (POLE2 NIL FATHER NIL)) :CSVAL "0.917")    ;;moved-value=  (:CSVAL "0.917")




;;APPEND-WHEN-BOUNDP-OR-LISTP
;;2020
;;ddd
(defun append-when-boundp-or-listp (list &key incl-symbols-p append-list-list-p)
  "U-lists [another copy in CSQ-config--later delete it]  Only appends items in list that are either a boundp symbol that evals to a list or is a listp itself. "
   (let*
       ((appended-list)
        (rejected-items)
        (evaled-symbol-result)
        )
  (loop
   for item in list
   do
   (cond
    ((listp item)
     (cond
      (append-list-list-p
       (setf appended-list (append appended-list (list item))))
      (t  (setf appended-list (append appended-list  item)))))
    ((and (symbolp item)(boundp item))
     (setf evaled-symbol-result (eval item))
     (cond 
      ((listp evaled-symbol-result)
       (cond
        (append-list-list-p
         (setf appended-list (append appended-list (list evaled-symbol-result))))
        (t  (setf appended-list (append appended-list evaled-symbol-result)))))
      (incl-symbols-p
       (cond
        (append-list-list-p
         (setf appended-list (append appended-list (list (list evaled-symbol-result)))))
        (t (setf appended-list (append appended-list (list evaled-symbol-result))))))    
      ;;end incl-symbols-p
      (t (setf rejected-items (append rejected-items `(,item))))))
    ;;end bound symbolp item
    (t (setf rejected-items (append rejected-items `(,item)))))
   ;;end loop
   )
   (values appended-list rejected-items)
   ;;end let, append-when-boundp-or-listp
   ))
;;TEST
;; (SETF LIST1 '(1 2 3) LIST2 '(4 5 6)  sym1 'not-list)
;; (append-when-boundp-or-listp '(list1 list3 list2 sym1 (a b c d) ))
;; works =(1 2 3 4 5 6 A B C D)   (LIST3 SYM1)
;; :INCL-SYMBOLS-P
;; (append-when-boundp-or-listp '(list1 list3 list2 sym1 (a b c d) ) :incl-symbols-p T)
;; works= (1 2 3 4 5 6 NOT-LIST A B C D)    (LIST3)
;; :append-list-list-p
;; (append-when-boundp-or-listp '(list1 list3 list2 sym1 (a b c d) ) :incl-symbols-p T :append-list-list-p T)
;; ((1 2 3) (4 5 6) (NOT-LIST) (A B C D))   (LIST3)



;;SET-VARS-TO-VALUE
;;2020
;;ddd
(defun set-vars-to-value (value vars &key (func 'setf))
  "U-lists   RETURNS vars"
  (dolist (var vars)
    (eval `(,func ,var ,value)))
  vars
  ;;end set-vars-to-value
  )
;;TEST
;; (set-vars-to-value NIL '(testx1 testx2) :func 'defparameter)
;; works= (TESTX1 TESTX2) 
;; also: TESTX1 = NIL     TESTX2 = NIL


;;xxx
;; hhh ***************************** HELP ****************************8
;;
;;USEFUL CL FUNCTIONS
;; FIND-SYMBOL
;;Lambda List: (STRING &OPTIONAL (PACKAGE *PACKAGE*))
#|Returns the symbol named String in Package.  If such a symbol is found
  then the second value is :intern, :external or :inherited to indicate
  how the symbol is accessible.  If no symbol is found then both values
  are NIL.|#
;;TEST
;;  (find-symbol "setf") = nil  ;;doesn't work
;;  (find-symbol "newtest") = nil ;;doesn't work



#|
(append nil (list 5)) = (5)
(tx '(1 2 3 (4) 5))  = (1 2 3 (4) 5)
(tx '(a b c)) = (A B C)
(defun tx (list)
  (let
      ((newlist)
       )
  (dolist (x list)
    (setf newlist (append newlist (list x)))    
    )
      newlist))
|#



#|(random 10)
(nth 3 '(a b c d e))
(elt  '(a b c d e) 2) = C
(elt  "abcde" 2) = #\c 
(elt  '((a)(b)(c) d) 1) = (B) |#

#|;;SSS START HERE ON LOGIC, ADDED LOOP, SHOULDN'T HAVE?
;; BUT NOT RECURSING ON LISTS INSIDE OF LISTS IN MAIN LIST.

;;LOGIC FOR get-set-append-delete-keyvalue-in-nested-list

;; NESTED-LISTS MUST BE A LIST

;;1. IF LAST-KEY-FOUND-P
   USE GET-ADD AND PASS FINAL LISTS UP THRU RECURSE
;;2. IF  NULL  NESTED-LISTS, NIL 
;;3. IF NULL KEY-SPEC-LISTS  & KEY-NOT-FOUND-P
            RECURSE PASS UP NESTED-LISTS TO ADD

;;4. LOOP THRU  NESTED-LISTS
;;      FOR ITEM IN NESTED-LISTS, FOR EACH ITEM

     4,1 IF LISTP ITEM: 
;;         4.1.1 IF NUMBERP KEYLOC-N, use NTH on nested-lists 
;;                  4.1.1.1 IF KEY FOUND, (setf new-key-spec-lists (cdr key-spec-lists))
                          4.1.1.1.1 If last-key-found-p, GO TO build final lists (get-set..)
                          4.1.1.1.2 If not last key, Use new-key-spec-lists
                          RECURSE on that list and put head and tail aside for appending later.
                    4.1.1.2 IF KEY NOT FOUND
                                     Add list to return-nested lists, do not recurse.

          4,1,2 IF NOT NUMBERP, loop thru every item on nested-lists
                   LOOP,  FOR EACH ITEM
                  4.2.1 IF ITEM NOT LIST
                        4.2.1.1 ITEM = KEY (then find value and in betw items)
                                (setf new-key-spec-lists (cdr key-spec-lists))
                               4.2.1.1.1 If last-key-found-p, GO TO build final lists (get-set..)
                               4.2.1.1.2 key-spec-lists, RECURSE ON THIS NESTED-LISTS
                                      USING new-key-spec-lists.

                        4.2.1.2 ITEM NOT= KEY,
                                      Add to a return sublist?

                  4.2.2 ITEM IS A LIST, 
                         RECURSE on that ITEM (a list)
                                    and put head and tail aside for appending later.

          5.  AT BOTTOM If  NULLlast-key-found-p ever found , 
                    and ITEM NOT LIST, ADD TO FINAL LISTS.

       



;;AT END, IF key not found and append-if-not-found-p, add (key value) to overall list
|#
