;;******************************* U-symbol-info.lisp *********************
;;
;;Functions for writing code etc to find functions, function args, etc. to speed writing code
;;
;;SOME FUNCTIONS THAT MIGHT HELP
#|
do-symbols, do-external-symbols, and do-all-symbols iterate over the symbols of packages. For each symbol in the set of packages chosen, the var is bound to the symbol, and the statements in the body are executed. When all the symbols have been processed, result-form is evaluated and returned as the value of the macro.

do-symbols iterates over the symbols accessible in package. Statements may execute more than once for symbols that are inherited from multiple packages.

do-all-symbols iterates on every registered package. do-all-symbols will not process every symbol whatsoever, because a symbol not accessible in any registered package will not be processed. do-all-symbols may cause a symbol that is present in several packages to be processed more than once.

do-external-symbols iterates on the external symbols of package.

When result-form is evaluated, var is bound and has the value nil.
|#

;;ALSO SEE U-sexp.lisp




;;MAKUNBOUND-NESTED-VARS
;;2019
;;ddd
(defun makunbound-nested-vars (nested-varlists &key (unbind-level 99) 
                                               (convert-strings-p T)
                                               (level  0))
  "U-symbol-info,  RETURNS: (values unbound-vars  not-unbound-vars)   UNBIND-LEVEL usually either 0 :1, or 99"
  (let*
      ((unbound-vars)
       (not-unbound-vars)
       (itemsym)
       )
    (cond
     ((listp nested-varlists)
      (loop
       for item in nested-varlists
       do
      (cond
       ;;FOR LIST ITEMS
       ((listp item)
        (cond
         ((> unbind-level level)
          (multiple-value-bind ( unbound-vars1 not-unbound-vars1)
              (makunbound-nested-vars item :unbind-level unbind-level 
                                      :convert-strings-p convert-strings-p :level (+ level 1))
            (setf unbound-vars (append unbound-vars unbound-vars1)
                  not-unbound-vars (append not-unbound-vars not-unbound-vars1))) )
         (t (setf not-unbound-vars (append not-unbound-vars item))))
        ;;end listp item
        )
       ;;FOR NON-LIST ITEMS
       (T
            (when (and (stringp item) convert-strings-p)
              (setf item (my-make-symbol item)))
          (cond
           ((and (symbolp item)(not (constantp item)))
            (setf unbound-vars (append unbound-vars
                                      (list (makunbound item)))))
           (t (setf not-unbound-vars (append not-unbound-vars (list item)))))
          ;;end T, cond,loop
          )))
      ;;end listp
      )
     (t (cond
         ((and (stringp nested-varlists) convert-strings-p)
          (setf itemsym (my-make-symbol nested-varlists)))
         ((and (symbolp nested-varlists)(not (constantp nested-varlists)))
          (setf itemsym nested-varlists))
         (t nil))
        (when itemsym
          (setf unbound-vars (append unbound-vars (list (makunbound nested-varlists)))))
        ))
    (values unbound-vars not-unbound-vars)
    ;;let, makunbound-nested-vars
    ))
;;TEST
;; (makunbound-nested-vars '(v1 (v11 v22 (v111 v222 "strvl333"))))
;; works= (V1 V11 V22 V111 V222 STRVL333)   NIL
;; FOR UNBIND-LEVEL = 1
;; (makunbound-nested-vars '(v1 (v11 v22 (v111 v222 "strvl333"))) :UNBIND-LEVEL 1)
;; works= (V1 V11 V22)    (V111 V222 "strvl333")
;; (makunbound-nested-vars '(v1 (v11 v22 (v111 v222 "strvl333") v33)  v2 (v44 v55 (v444 v555) v66)) :UNBIND-LEVEL 1)
;;works=  (V1 V11 V22 V33 V2 V44 V55 V66)       (V111 V222 "strvl333" V444 V555)
        




;;MAKUNBOUND-VARS
;; modified 2018, 2020
;;ddd
(defun makunbound-vars (varlist &key (convert-strings-p t))
  "U-symbol-info converts a list of  symbols or strings to a list of UNBOUND VARS--even if they were previously bound. RETURNS (values unbound-vars varlist)"
  (let
      ((unbound-vars)
       )
  (dolist (var varlist)
    (when  (and convert-strings-p (stringp var))
      (setf var (my-make-symbol var)))
    (unless (or (constantp var)(stringp var)
                (not (symbolp var))(not (boundp var)))
      ;;(break)
      (makunbound `,var))
      (setf unbound-vars (append unbound-vars (list var)))
      )
  (values unbound-vars varlist) 
  ))
;;TEST
;;  (setf mubtestx1 '(mubtestx1 list)) = (MUBTESTX1 LIST)
;;  (makunbound-vars  '( "mubtestx1")) = ("mubtestx1")
;; CL-USER 37 > MUBTESTX1
;; WORKS= Error: The variable MUBTESTX1 is unbound.
;; for unbound var
;;  (makunbound-vars '(unboundvar))
;;works= (UNBOUNDVAR)  (UNBOUNDVAR)

;;(makunbound-vars  '("bb") :convert-strings-p T)
;; works= (BB)  ("bb")
;; (constantp :this) = T
;; (setf thisxx '(a b))   (constantp 'thisxx) = NIL

;; (makunbound-varlist



;;UNQUOTED-SYMBOLP
;;2016
;;ddd
(defmacro unquoted-symbolp (x)
  "In U-symbol-info. Tests whether the the UNQUOTED symbol is a symbol"
  `(symbolp (quote ,x)))
;;TEST
;; (unquoted-symbolp 'this) = NIL
;; (unquoted-symbolp this) = T



;;UNQUOTED-BOUNDP
;;2016
;;ddd
(defmacro unquoted-boundp (x)
  "In U-symbol-info, Tests whether the the UNQUOTED symbol is boiundp"
  `(boundp (quote ,x)))
;;TEST
;; (unquoted-boundp this) = NIL
;; (unquoted-boundp mother) = T



;;CONVERT-KEYWORD
;;2019
;;ddd
(defun convert-keyword (keyword )
  "U-symbol-info  RETURNS (values sym str)"
  (let*
      ((str (format nil "~A" keyword))
       (sym (my-make-symbol str))
       )
    (values sym str)
    ;;end let, convert-keyword
    ))
;;TEST
;; (convert-keyword :this)
;; works = THIS   "THIS"


;;MY-MAKE-KEYWORD
;;2019
;;ddd
(defun my-make-keyword (object)
  "U-symbol-info object= str or sym. RETURNS keyword."
  (let*
      ((str (format nil "~A" object))
       (keyword (intern  str "KEYWORD"))
       )
    (values keyword str)
    ;;end let, my-make-keyword
    ))
;;TEST
;; (my-make-keyword "THIS1") 
;; works= :THIS1  "THIS1"



;;MY-SYMBOLP
;;2020
;;ddd
(defun my-symbolp (item &key (not-keywordp T)(not-nil-p T))
  "U-symbol-info.lisp. Filters out keywords and/or NILs & 'NILS"
  (let
      ((result (symbolp item))
       )
    (when (and not-keywordp (keywordp item))
      (setf result NIL))
    (when (and not-nil-p (equal  item 'NIL))
      (setf result NIL))
    result
    ;;end let,my-symbolp
    ))
;;TEST
;; (my-symbolp NIL) = nil
;; (my-symbolp 'NIL) = NIL
;; (my-symbolp :NEWKEY) = NIL
;; (my-symbolp :NEWKEY  :not-keywordp NIL) = T
;; (my-symbolp 'NIL  :not-nil-p NIL) = T
;; (my-symbolp NIL  :not-nil-p NIL) = T
    


;;GET-SYM-DIMS
;;2020
;;ddd
(defun get-sym-dims  (symbol &key (separators  '("."))) ;;caused probs "-")))
  "In U-symbol, Converts a symbol (or string) into dims list. RETURNS (values dimslist rootstr dims-list dims-str)."
  (let
      ((symstr (cond ((stringp symbol) symbol)
                     (T (format nil "~A" symbol))))
       ;;(dims-str-list)
       (n-dims)
       )
  (multiple-value-bind ( dims-str  dimslist)
      (convert-string-w-separators-to-list symstr :separators separators)

    ;;(setf dims-str-list (format nil "~{~a~^-~}" dimslist ))
     (values dimslist dims-str)  
     ;;end mvb,let, get-sym-dims
     )))
;;TEST
;; (get-sym-dims 'a.b.c)
;; works = (A B C)    ("A" "B" "C")

