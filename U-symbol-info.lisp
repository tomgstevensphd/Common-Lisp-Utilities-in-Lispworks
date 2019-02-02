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

;;MAKUNBOUND-VARS
;; modified 2018
;;ddd
(defun makunbound-vars (varlist &key (convert-strings-p t))
  "U-symbol-info converts a list of  symbols or strings to a list of UNBOUND VARS--even if they were previously bound. RETURNS (values new-varlist varlist)"
  (let
      ((new-varlist)
       )
  (dolist (var varlist)
    (when  (and convert-strings-p (stringp var))
      (setf var (my-make-symbol var)))
      (unless (or (constantp var)(stringp var))
             ;;(break)
        (makunbound `,var))
      (setf new-varlist (append new-varlist (list var)))
      )
  (values new-varlist varlist) 
  ))
;;TEST
;;  (setf mubtestx1 '(mubtestx1 list)) = (MUBTESTX1 LIST)
;;  (makunbound-vars  '( "mubtestx1")) = ("mubtestx1")
;; CL-USER 37 > MUBTESTX1
;; WORKS= Error: The variable MUBTESTX1 is unbound.

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



