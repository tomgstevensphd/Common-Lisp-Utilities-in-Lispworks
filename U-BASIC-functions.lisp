;;******************** U-basic-functions.lisp ********************
;;MISC BASIC FUNCTIONS
;;
;;LOAD WITH ALL MAIN CONFIG FILES


;;MY-FUNCALL
;;2018-02
;;ddd
(defun my-funcall (function  &rest args)  
  "In    REPLACES FUNCALL with easier to use version just put args wanted in args and #'function in function. RETURNS  up to five values from the call. Can't use a macro such as setf. NOTE: use-quoted-args-p in my-funcall-arglist= T. "
  (multiple-value-bind (a b c d e)
      ;;NOTE: :use-quoted-args-p = NIL always caused errors in my-funcall, 
      (my-funcall-arglist function args :use-quoted-args-p use-quoted-args-p)
    (values a b c d e)
    ;;end let, my-funcall
    ))
;;TEST
;; (my-funcall  T #'nth-nth 2 1 '((a)(b 2)(c 3))) ;;if use-quoted-args-p= NIL = ERROR
;; works= 3 NIL NIL NIL NIL
;;If use-quoted-args-p= NIL = ERROR
;; (my-funcall T #'get-nths-in-lists '(0 1 0) '((a 2)(b 2)(c 3))) 
;; works= (A 2 C)  NIL  NIL NIL NIL
;; (setf  mftestlist '((a 2)(b 2)(c 3)))
;;If use-quoted-args-p= NIL = ERROR
;; (my-funcall T #'get-nths-in-lists '(0 1 0) mftestlist) 
;; works= (A 2 C) NIL NIL NIL NIL
;; IF ARGS ARE UNQUOTED
;; (my-funcall  T #'nth-nth 2 1 ((a)(b 2)(c 3))) 

;;USE IN A FUNCTION CALL
#|(defun testmyfunc1 (function key list)
  (multiple-value-bind (a b c d e)
      ;;NOTE USE-QUOTED-ARGS-P = T
      (my-funcall T function key list)
    ;;(break)
    (values a b c d e)
    ))|#
;;TEST
;;(setf functestlist '(k1 5 88 k2 value))
;; (testmyfunc1 #'get-key-value 'k2 functestlist)
;; works= VALUE   K2 NIL  NIL NIL
;; (testmyfunc1 #'get-key-value 'k2  '(k1 5 88 k2 value))
;; works= VALUE  K2 NIL NIL NIL



;;MY-FUNCALL-ARGLIST
;;2018-02
;;ddd
(defun my-funcall-arglist (function arglist &key use-quoted-args-p)
  "In    REPLACES FUNCALL with easier to use version just put args wanted in arglist and #'function in function. RETURNS  up to five values from the call. Can't use a macro such as setf.  Note: :USE-QUOTED-ARGS-P= T usually caused errors in my-funcall-arglist for quoted args, ok for nonquoted ones; but was necessary for example below def--USE-MY-FUNCALL-ARGLIST-TEST function."
   ;; NOTE: :use-quoted-args-p = NIL always caused errors in my-funcall, 
  (let
      ((newargs)
       (qarg)
       )
    (cond
     (use-quoted-args-p
      (loop 
       for arg in arglist
       do
       (setf qarg arg
             newargs (append newargs (list `(quote ,qarg))))
       ;;end loop, use-quoted
       ))
     (t (setf newargs arglist)))
    ;;(break)
    (multiple-value-setq (a b c d e f)
        (eval (append (list 'funcall function) newargs)))     
    (values a b c d e f)
    ;;end let, my-funcall-arglist
    ))
;;TEST
;; (my-funcall-arglist  #'nth-nth '(2 1 ((a)(b 2)(c 3))) :use-quoted-args-p T)
;;works= 3 NIL NIL NIL NIL

;;USE IN A FUNCTION CALL
#|(defun testmyfunc (function arglist &key use-quoted-args-p)
  (multiple-value-bind (a b c d e)
      (my-funcall-arglist function arglist :use-quoted-args-p use-quoted-args-p)
    ;;(break)
    (values a b c d e)
    ))|#
;;TEST
;;(setf functestlist '(k1 5 88 k2 value))
;;note: items in arglist are in quotes, with ::use-quoted-args-p= NIL evaled first.
;; (testmyfunc #'get-key-value  '('k2 functestlist) :use-quoted-args-p NIL)
;; works= VALUE  K2  NIL  NIL  NIL
;;note: items in arglist are in quotes, with ::use-quoted-args-p= NIL evaled first.
;; (testmyfunc #'get-key-value  '('k2 '(k1 5 88 k2 value)) :use-quoted-args-p NIL)
;; works= VALUE  K2  NIL  NIL  NIL
;; iF iftems in arglist are NOT IN QUOTES, use :USE-QUOTED-ARGS-P T)
;;(testmyfunc #'get-key-value  '(k2 (k1 5 88 k2 value)) :use-quoted-args-p T)
;; works= VALUE  K2  NIL  NIL  NIL

;;PROBLEM OF HAVING ARGS IN A FUNCALL THAT CHANGE INSIDE THE CALLING DEFUN FUNCTION
;;SOLUTION EGS:
;; 1. Using func-args as just one arg in normal arg list
#|(defun USE-MY-FUNCALL-ARGLIST-TEST (function funcargs append-n list1)
  "IMPORTANT: FUNCARGS used as funcargs for MY-FUNCALL-ARGLIST.  But FUNCARGS is modified WITHIN the defun USE-MY-FUNCALL-ARGLIST-TEST by using append-nth  with a value specified in the main function arg list, so it can be varied with any value of FUNCTION."
  (let*
      ((newargs (append-nth list1 append-n funcargs))
       (item1 (my-funcall-arglist function newargs :use-quoted-args-p T))
       (item2)
       )
    ;;(break)
    ;;NOTE: CHANGING ARGLIST VAR= listx INSIDE FUNCTION WORKED
    ;; BY CHANGING NEWARGS for function
    (setf newargs (set-nth-in-list (cdr list1) append-n newargs)
          item2 (my-funcall-arglist function newargs :use-quoted-args-p T))
    ;;(BREAK)
       (values item1 item2 list1 newargs)
      ))|#
;;TEST
;;(setf listx '(1 2 3 4 5 6 7 8 9 10 11 12))
;; (USE-MY-FUNCALL-ARGLIST-TEST #'nth '(3) 1 listx)
;; WORKS= 4   5   (1 2 3 4 5 6 7 8 9 10 11 12)    (3 (2 3 4 5 6 7 8 9 10 11 12))
;;2. Using &rest args
#|(defun USE-MY-FUNCALL-ARGLIST-TEST2 (function list1 append-n &rest args)
  "IMPORTANT: ARGS used as args for MY-FUNCALL-ARGLIST.  But ARGS is modified WITHIN the defun USE-MY-FUNCALL-ARGLIST-TEST2 by using append-nth  with a value specified in the main function arg list, so it can be varied with any value of FUNCTION."
  (let*
      ((newargs (append-nth list1 append-n args))
       (item1 (my-funcall-arglist function newargs :use-quoted-args-p T))
       (item2)
       )
    ;;(break)
    ;;NOTE: CHANGING ARGLIST VAR= listx INSIDE FUNCTION WORKED
    ;; BY CHANGING NEWARGS for function
    (setf newargs (set-nth-in-list (cdr list1) append-n newargs)
          item2 (my-funcall-arglist function newargs :use-quoted-args-p T))
       (values item1 item2 list1 newargs)
       ))|#
;;(setf listx '(1 2 3 4 5 6 7 8 9 10 11 12))
;; (USE-MY-FUNCALL-ARGLIST-TEST2 #'nth listx 1 3)
;; WORKS= 4   5   (1 2 3 4 5 6 7 8 9 10 11 12)    (3 (2 3 4 5 6 7 8 9 10 11 12))

;;USED IN MY-KEYBINDS below
(capi:define-interface interface-keybinds ()
    ()
    ;;Had to make new editor WINDOW then later close it to get CAPI:CALL-EDITOR to stay open and close window called to open it.
    (:layouts
     (column-layout-1
      capi:column-layout ()))
    (:default-initargs
     :layout 'column-layout-1
     :title "KEY-BINDS"))

;;MY-KEYBINDS
;;2018-8
;;ddd
(defun my-keybinds ()
  "U-BASIC-functions Lists all key binds in separate window"
  (let*
      ((kbwin (make-instance 'interface-keybinds))
       ;;NOTE: This creates a separate window that works as an editor window
       (editor (make-instance 'capi:editor-pane  :text "MY-KEYBINDS"))
       )
    (with-slots (column-layout-1) kbwin
      (setf (capi:layout-description column-layout-1) (list editor))
      )
    (capi:display kbwin)

    ;;NOTE: CAPI:CALL-EDITOR is able to CALL BASIC EDITOR FUNCTIONS
    (capi:apply-in-pane-process  editor 'CAPI:CALL-EDITOR editor "DESCRIBE BINDINGS")
    (sleep 3)
    (capi:destroy kbwin)
    ))
;;TEST
 ;;(MY-KEYBINDS)
;; works, opens a help editor buffer and does not leave any other windows open

#| OLD, works, but leaves a container window open
(defun my-keybinds ()
  "U-BASIC-functions Lists all key binds in separate window"
  (let
      ;;NOTE: This creates a separate window that works as an editor window
      ((editor (capi:contain
                (make-instance 'capi:editor-pane
                               :text "MY-KEY-BINDS")))
       )     
    ;;NOTE: CAPI:CALL-EDITOR is able to CALL BASIC EDITOR FUNCTIONS
    (capi:apply-in-pane-process  editor 'CAPI:CALL-EDITOR editor "DESCRIBE BINDINGS")
    ))|#








;;MY-EDIT-FILE
;;2018-08
;;ddd
(defun my-edit-file (pathname)
  "In U-BASIC-functions opens a new editor buffer from pathname.  If not exist, creates buffer, but contents must be saved."
  (let
      ((path (pathname pathname))
       )
  (editor:find-file-command nil path)
  ))
;;TEST
;; (my-edit-file "C:/3-TS/LISP PROJECTS TS/1-LW-INIT/0-CODE-TEMPLATES.lisp") 
;; works: opens a new editor buffer with that name
  

;;MY-COLORS
;;
;;ddd
(defun my-colors (&optional sym red green blue)
  "U-Basic-functions, RETURNS COLOR INFO, all color syms. Sets color-sym to new RGB color."
  (let
      ((colors (color:get-all-color-names))
       (colors2)
       )
  (format t "==== SELECTING/MAKING COLORS INFO =======
  ==> TO MAKE NEW RGB COLORS
    EVAL: 
    (my-make-rgb red green blue   &optional alpha &key sym)
            ;where each color-val varies from 0 to 1.0 
            ;when sym, sets sym to color

  ==> TO SELECT NEW COLORS FROM A PALLET:
    EVAL: 
    (my-define-new-color  &optional color-sym)
    sets color-sym to the selected color and returns both. ~%
  ==> ALL DEFINED LW COLOR SYMS FOLLOW: ~%")
  (when (and sym red green blue)
    (set sym (color:make-rgb red green blue)))
   (format t "(ALL-LW-COLOR-SYMS~%")
  (loop
   for color in colors
   do
   (format t "    :~A~%" color)
   )
   (format t "END ALL-LW-COLOR-SYMS )")
  (values sym (eval sym))
  ))
;;TEST
;; (my-get-colors) = works
;; (my-get-colors :color-sym 'newcol '(.7  0 0))
     


;;MY-DEFINE-NEW-COLOR
;;
;;ddd
(defun my-define-new-color (&optional color-sym)
  "U-Basic-funcytions RETURNS (values color color-sym) where color-sym is set to the new color eg. #(:RGB 0.0 0.5019608 0.2509804 1.0)"
  (let
      ((new-color (CAPI:prompt-for-color "Select Color"))
       )
    (when color-sym
      (set color-sym new-color))
    (values new-color color-sym)
    ))
;;works= #(:RGB 0.0 0.5019608 0.2509804 1.0)  MY-COL



;;MY-MAKE-RGB
;;
;;ddd
(defun my-make-rgb (red green blue &optional alpha &key sym)
  "U-BASIC-functions, input 0-1.0 colors and if sym, sets new color to sym"
  (let
      ((color (color:make-rgb red green blue alpha))
       )
    (when sym
      (set sym color))
    (values sym color)
  ;;end my-make-rgb
  ))
;; (my-make-rgb .5 .3  .2 nil :sym 'newcol)
;; works= NEWCOL   #(:RGB 0.5 0.3 0.2)
;; (my-make-rgb .5 .3  .2  .8 :sym 'newcol1)
;; works= NEWCOL1    #(:RGB 0.5 0.3 0.2 0.8)


;;PPMX  (PRETTY PRINT MACRO EXPANSION)
;;From David Touretzky, Common Lisp: A Gentle .. Kindle
;;ddd
(defmacro ppmx (form )
  "U-BASIC-functions. Pretty Print Macro Expansion of form "
  `(let* 
      ((exp1 (macroexpand-1 ',form))
       (exp (macroexpand exp1))
       (*print-circle* nil)   
       )
    (cond
     ((equal exp exp1)
      (format t "~&Macro expansion:")
      (pprint exp))
     (t (format t "~&First step of expansion:")
        (pprint exp1)
        (format t "~%~%Final expansion:")
        (pprint exp)))
    (format t "~%~%")
    (values)))
;;TEST
#| (defmacro lengthy-incf (var)
 `(setf ,var (+ ,var 1)))|#
;; (PPMX (lengthy-incf a))
;;works=
#|CL-USER 43 > (ppmx (lengthy-incf a))
First step of expansion:
(SETF A (+ A 1))
Final expansion:
(LET* ((#:|Store-Var-409352| (+ A 1))) (SETQ A #:|Store-Var-409352|))|#


;;DEFINED IN .LISPWORKS ===========================
;;
#|;;MY-LW-INIT
;;2019, 2020
;;ddd
(defun MY-LW-INIT (&optional projects-list  &key (default-projects-list 
                                                  '(**CS **UCS  **SHAQ **U-ETC))  
                             (my-keybinds-p T))
  "Projects include **U-ETC **CS **UCS **ACTR **ART **SHAQ **SCRSAV projects-list is a LIST of projects."
  (when (null projects-list)
    (setf projects-list default-projects-list))
  (my-open-buffers *open-editor-buffers*)
  (compile-file "C:/3-TS/LISP PROJECTS TS/1-LW-INIT/0 MY-LW-INIT.lisp" :load T)
  (my-open-buffers *open-edit-buffers-new-window :open-new-windows-p T)
  (eval  `(my-edit-projects (quote ,projects-list)))
  (my-keybinds)
  )
;; (my-lw-init (**cs))
;;MLI
;;2019
;;ddd
(defmacro  MLI (&rest projects-list)
  "In .LISPWORKS, can just put proj syms eg. **U-ETC **CS **UCS **ACTR **ART **SHAQ **SCRSAV after mli"
  (setf **mactest
  `(MY-LW-INIT (quote ,projects-list))
  ;;"C:/3-TS/LISP PROJECTS TS/0 MY-LW-INIT.lisp"
  ))
|#

