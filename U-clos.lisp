;;**************************** U-clos.lisp **********************************
;;
;;
;;
;;put in all key files
;;  (my-config-editor-after-start)
;;  IF *make-defs-file-p = T, THEN WRITES TO FILES
;;(defparameter  *make-defs-file-p T) 
(defparameter *make-defs-file  "C:\\3-TS\\LISP PROJECTS TS\\SHAQ\\shaq-new-scales-compile.lisp" "File for writing class defs, instances in dif file.")
(defparameter *make-insts-file "C:\\3-TS\\LISP PROJECTS TS\\SHAQ\\shaq-new-scales-insts.lisp" "File for writing class instances, put into defun, put defun into run file")

;;MY-DEFCLASS
;;SSS -- CHANGE MAKE-INSTANCE STREAM BACK TO SAME AS DEFCLASS TO AVOID 2 FILES BEING CREATED, OR SIMPLY MOVE THE NEW FUNCTION BACK TO THE COMPILE FILE MANUALLY.
;;
;;ddd
(defmacro  my-defclass (class-sym superclasses slot-spec-list &rest defclass-option-list) ;;no use global variable above &key (make-defs-file *make-defs-file) (make-insts-file *make-insts-file))
  "In u-clos,lisp, like defclass, except adds slots, parents, children. makes a class instance called *classname-inst and FILLS IN parents slot with superclass; also it auto defines accessors for all slots in slot-list. default-args is used to set the class instance slot-values. it makes a slot by creating a slot-spec that writes in (:initarg :name  :accessor name ) and appends any added slot-keys such as   :initform initial top-class name :documentation name. :instance-initargs is args for the *class-inst. These CLASS-OPTIONS should be standard format for initiating instances. When *MAKE-defs-file, writes the new class definitions to the file named in *make-defs-file and *make-insts-file for instances, because when try to deliver or compile file, it creates error otherwise. {Must put insts in a defun and call it inside the run app function TO PREVENT DELIVER ERROR."

  (let*
      (;;(class-sym (car class-definition-list))
       (class)
       (defclass-option-list-n (list-length defclass-option-list))
     ;;  (superclasses  (second class-definition-list))
    ;;   (slot-spec-list (first class-definition-list))
      ;; (slot-name)
     ;;  (slot-list)
    ;;   (defclass-option-list)
       ;;     (class-options)
       (default-initargs)
    ;;   (new-default-initargs)
       (documentation)
       (metaclass)
       (class-inst-sym)
       (class-inst)
       (defclass-call)
       (instance-initargs)
       (parents-list  `(:parents (quote ,superclasses))) ;; `(:parents ,superclasses-syms))
       (parent-class-inst-syms)
       ;;following 2 may not be needed but are returned in multiple-value-setq below
       (parent-class-inst-objects)
      (parent-class-inst-strings)
       ;;  (superclasse-objects) 
       )

  ;;   (afout 'out (format nil "defclass-option-list= ~A~%" defclass-option-list))
 (if (> defclass-option-list-n 0) 
    (dolist (element defclass-option-list)
      (if  (listp element)
          (cond
           ((equal (car element) :default-initargs)   
            (setf default-initargs element))
           ((equal (car element) :documentation)
            (setf documentation element))
           ((equal (car element) :metaclass)
            (setf metaclass element)) 
           ((equal (car element) :instance-initargs)
            (setf instance-initargs element)))
        ;;end if, dolist
        )))
    (cond
     (default-initargs   
      ;;add parents to :default-initargs in the default-initargs list
      ;;sss add 
      (setf  defclass-option-list
             (list (append default-initargs parents-list))))
     (t     (setf  defclass-option-list (list (append (list :default-initargs) parents-list)))))
    
    (if documentation
        (setf defclass-option-list (append defclass-option-list (list documentation))))
    (if metaclass
        (setf defclass-option-list  (append defclass-option-list (list metaclass))))
     
    ;;(afout 'out (format nil "defclass-option-list= ~A~%" defclass-option-list))

    ;;add the accessor parts to each slot spec item
    (setf slot-spec-list (add-accessors slot-spec-list))

    ;;(afout 'out (format nil "2-superclasses= ~A~%" superclasses))

    ;;PUT THIS 'CHILD CLASS INTO 'CHILDREN SLOTS OF EACH OF SUPERCLASS INSTS -- NOT NEEDED & DOESN'T WORK WHEN MAKE-defs-file
    ;;find parent-class-instance-list = the superclass class instances
    ;;yyy
#|    (multiple-value-setq (parent-class-inst-syms parent-class-inst-objects parent-class-inst-strings)
        (get-class-insts superclasses))
    ;;add to the children slot of superclasses
    (append-slot-lists parent-class-inst-syms 'children class-sym) |#
 
    ;;MAKE THE NEW CLASS WITH DEFCLASS
    (setf defclass-call 
          (append  `(defclass ,class-sym ,superclasses ,slot-spec-list) defclass-option-list))

    (cond
     ((null *make-defs-file-p)
      (setf class (eval defclass-call))
   
    ;;setf *[CLASSNAME]-INST to a new class instance with init-args and adds its sym to 
    ;;  a list in the class instance class-instances list
  ;;  (afout 'out 
 ;;  (setf *out (format t "class-sym= ~A~%instance-initargs= ~A~%" ,class-sym ,instance-initargs))
    (cond
     (instance-initargs
      (multiple-value-setq (class-inst class-inst-sym)
           (my-make-instance  class-sym nil :instance-initargs instance-initargs)))
     (t 
      (multiple-value-setq (class-inst class-inst-sym)
         (my-make-instance   class-sym nil))))
       ;;(afout 'out (format nil  "class= ~A~% class-sym= ~A~%  class-inst= ~A~% class-inst-sym= ~A~%  " class class-sym  class-inst class-inst-sym))
       
       ;;end null *make-defs-file
   `(values (quote ,class) (quote ,class-sym) (quote  ,class-inst) (quote ,class-inst-sym))
       )
     ;;otherwise send the defs to a file
     (*make-defs-file-p
      (with-open-file (defout *make-defs-file :direction :output :if-does-not-exist :create 
                           :if-exists :append)
      (with-open-file (instout *make-insts-file :direction :output :if-does-not-exist :create 
                           :if-exists :append)
        (terpri defout)
        (write defclass-call :stream defout :pretty t)
        (terpri defout)
        (cond
         (instance-initargs
          (multiple-value-setq (class-inst class-inst-sym)
              (my-make-instance  class-sym out :instance-initargs instance-initargs  )))
         (t  
          (multiple-value-setq (class-inst-def class-inst-sym)
              (my-make-instance   class-sym instout  ))))
      ;;  (terpri instout)
        ;;values for *make-defs-file
        `(values (quote ,defclass-call) (quote ,class-sym) (quote  ,class-inst) (quote ,class-inst-sym))
        ;;end with-opens, *make-defs-file clause, cond
        ))))

    `(values (quote ,class) (quote ,class-sym) (quote  ,class-inst) (quote ,class-inst-sym))
    ;;end let, my-defclass
    ))
;; (setf cx-inst (make-instance 'class1 :object-docs "this one"  :parents '(this-one that-one)))
;;test
;;SSS RETEST  MY-DEFCLASS WITH NEW ADDING CLASS TO PARENTS 'CHILDREN SLOTS 
;;(class-sym  superclasses slot-spec-list class-options )
#|(defun testdcm ()
  (setf out nil)
  (let
      ( (class)
        (class-sym)
        (class-inst)
        (class-inst-sym)
       (result)
        )
    ;;NOTE: APPARENTLY THE MACRO WON'T RETURN MULTIPLE VALUES!!
        (multiple-value-setq (class class-sym  class-inst class-inst-sym)
              (my-defclass new-class4 
                           (class1 class2) 
                           ((new-slot1 :documentation "test docs1" :initform 99) 
                            (new-slot2  :documentation "test docs2"))
                           (:default-initargs :new-slot2 'init-value2) 
                           (:documentation "this is the class doc")))

    (values class class-sym class-inst class-inst-sym (slot-value *new-class4-inst 'new-slot1))
    ))|#
;;TESTING ----------------------------------------------------------------------------
;;WORKS
;; RETURNS VALUES:
#|#<STANDARD-CLASS NEW-CLASS4 22533D63>
NEW-CLASS4
#<NEW-CLASS4 200B057B>
*NEW-CLASS4-INST
99|#

;;   (slot-value *new-class4-inst 'class-instances) = (*NEW-CLASS4-INST)
;; MAKES A NEW CLASS INSTANCE
;;  *new-class4-inst =  #<NEW-CLASS4 200A1DA7>
;;ADDS PARENTS TO SELF PARENTS SLOT
;;   (slot-value *NEW-CLASS4-INST 'parents) = (CLASS1 CLASS2)
;; ADDS SELF  TO PARENTS' CHILDREN SLOT
;;    (slot-value *class1-inst 'children) = (NEW-CLASS4)
;;  (slot-value *class2-inst 'children) =  (NEW-CLASS4)

#|(defun testdc ()
  (let
      ((class-sym 'new-class3)
       (superclasses '(class1 class2))
       (slot-spec-list '((new-slot1 :documentation "test docs1" :initform 99)
                         (new-slot2  :documentation "test docs2")))
       ;;can't use :documentation as initial 
       (default-initargs '(:default-initargs :new-slot2 'init-value2))
       (documentation  '(:documentation "this is the class doc"))
        (meta-class '(:metaclass T))
       (instance-initargs '(:new-slot1 101 :object-docs "Instance docs"))
       (result)
       )
   ;;args (class-sym superclasses slot-spec-list default-initargs instance-initargs)
    (setf result  (my-defclass class-sym superclasses slot-spec-list
                  default-initargs documentation   instance-initargs))
    (values result (slot-value *new-class3-inst 'new-slot1))
    ))|#

;;appears to work well, returns (*NEW-CLASS2-INST)  101
;;ADDS PARENTS TO SELF PARENTS SLOT
;;  (slot-value *NEW-CLASS2-INST 'parents) = (top-class)
;; ADDS SELF  TO PARENTS' CHILDREN SLOT
;; (slot-value *class1-inst 'children) = (NEW-CLASS2)
;;  END TESTING ----------------------------------------------------------------------------




;;ADD-ACCESSORS
;;
;;ddd
(defun add-accessors (defclass-slot-spec-list)
  (let
      ((new-defclass-slot-spec-list)
       (slot-name)
       (slot-name-string)
       (rest-slot-list)
       (accessor-list-part)
       )
    (loop
     for slot-list in defclass-slot-spec-list
     do
     (setf slot-name (car slot-list)
           slot-name-string (format nil "~A" slot-name)
           rest-slot-list (cdr slot-list))
     (setf accessor-list-part `(,slot-name   
                                ;;NO  :name ,slot-name-string
                                :initarg ,(my-make-symbol (format nil ":~A" slot-name))
                                :accessor ,slot-name))
     (setf slot-list (append accessor-list-part rest-slot-list))
     (setf new-defclass-slot-spec-list
           (append new-defclass-slot-spec-list (list slot-list)))
     ;;end loop, let, defun                                              
     )
    new-defclass-slot-spec-list
    ))
;;TEST
#|(defun testaa ()
  (let
      ((defclass-slot-spec-list '((slot-x1  :documentation "slot-x1 doc")(slot-x2   :documentation "slot-x2 doc")))
       )
       (add-accessors defclass-slot-spec-list)
       ))|#
;;works, returns:
#|((SLOT-X1 :NAME "SLOT-X1" :INITARG :SLOT-X1 :ACCESSOR SLOT-X1 :DOCUMENTATION "slot-x1 doc") (SLOT-X2 :NAME "SLOT-X2" :INITARG :SLOT-X2 :ACCESSOR SLOT-X2 :DOCUMENTATION "slot-x2 doc"))|#
                                       



;;MY-MAKE-INSTANCE
;;
;;ddd 
(defun my-make-instance (class-sym  make-def-stream &rest initargs &key instance-sym class-instances-slot ) ;; init-args instance-sym class-instances-slot)
  "In U-clos.lisp, sets instance-sym to make-instance, and adds to class-instances slot-value. Default instance-sym= *class-sym-inst. If make-def-stream, sends def to that stream instead of evaling it."
  (let
      ((make-instance-call)
       ;;(new-initargs)  ;;use with below later??       
     ;;  (class-instances-slot) ;;fix later, makes default only  option
      ;;  (instance-sym)  ;;fix later, makes default only  option
       )
    ;;make default instance-sym
    (unless instance-sym
      (setf instance-sym 
            (my-make-symbol (format nil "*~A-inst" class-sym))))

    ;;set default class-instances-slot
    (unless class-instances-slot
      (setf class-instances-slot  'class-instances))
;;KKK
    (cond
     (initargs
      (setf  make-instance-call (append `(make-instance (quote ,class-sym)) initargs))
      )
     (t
      (setf  make-instance-call `(make-instance (quote ,class-sym)))))

    (cond
     ;;if make-def-stream, send def to that stream
     (make-def-stream
      (setf  write-form `(defparameter ,instance-sym ,make-instance-call))
      (terpri make-def-stream)
      (write write-form :stream make-def-stream  :pretty t)
      (terpri make-def-stream)
     (values write-form instance-sym)
      )
    ;;if null make-def-stream
    ;;now set the instance-sym to new made instance
     ((null make-def-stream)
      (set instance-sym (eval make-instance-call))

      ;;(afout 'out (format nil "instance-sym= ~A~%" instance-sym))
      ;;APPEND-SLOT-LIST GOES HERE TO ADD INSTANCE TO LIST
      (append-slot-list (eval instance-sym) class-instances-slot instance-sym)
      (values (eval  instance-sym) instance-sym)
      ;;end null clause, rest
      ))))

#|(defun testmm2 ()
  (let
      ((instance)
       (inst-sym)
       (name-slot)
       (value2)
       )
    (multiple-value-setq (instance inst-sym) ;;  instance-sym class-instances-slot)
        (my-make-instance  'class2  :name "test-name"))
    (setf name-slot  (slot-value *class2-inst 'name))
    (setf value2 (slot-value *class2-inst 'class-instances))
    (values instance inst-sym name-slot value2) ;;  instance-sym (quote class-instances-slot))
    ))|#
;;works, returns #<CLASS2 22ECDA03> *CLASS2-INST "test-name" (*CLASS2-INST)

;;older works, returns:
;;#<CLASS2 22BC2573> *CLASS2-INST "Initial top-class name" (*CLASS2-INST)




 ;; PRE-MACRO VERSION
#|(defun my-make-instance (class-sym &key init-args instance-sym class-instances-slot)
  "In U-clos.lisp, sets instance-sym to make-instance, and adds to class-instances slot-value. Default instance-sym= *class-sym-inst."
  (let
      ((make-instance-call)
       (slot 'class-instances)
       (new-init-args)
       )
    ;;make default instance-sym
    (unless instance-sym
      (setf instance-sym 
            (my-make-symbol (format nil "*~A-inst" class-sym))))

    ;;set default class-instances-slot
    (unless class-instances-slot
      (setf class-instances-slot  'class-instances))

    (cond
     (init-args
      (setf  make-instance-call (append `(make-instance (quote ,class-sym)) init-args))
      )
     (t
      (setf  make-instance-call `(make-instance (quote ,class-sym)))))

    ;;now set the instance-sym to new made instance
   (set instance-sym (eval make-instance-call))

    ;;APPEND-SLOT-LIST GOES HERE TO ADD INSTANCE TO LIST
    (append-slot-list (eval instance-sym) class-instances-slot instance-sym)
   (values (eval  instance-sym) instance-sym)
    ))|#


;;APPEND-SLOT-LISTS
;;
;;
(defun append-slot-lists (instance-list slot new-value &key match-instance-to-value)
  "U-clos.lisp, appends slot LISTS in all instances with new-value (can be a list).  If match-instance-to-value = T, appends items in order of items in new-value  list.  If new-value list is shorter than num of instances, puts first new value in rest of  instances."
  ;;(afout 'out (format nil "1 instance-list= ~A new-value= ~A ~%"  instance-list new-value))
  (let
      ((new-value-length 0) 
       (i  -1)
      ;; (nth-value 0)
       (cur-new-value)
       )
    (cond
     ((listp instance-list)
      (if (listp new-value)
          (setf new-value-length (list-length new-value)))
      (dolist (instance instance-list)
        (incf i)
       ;;(afout 'out (format nil "2 instance= ~A new-value= ~A ~%"  instance new-value))
       (cond
        (match-instance-to-value
         ;;are there enough values to go in each separate instance, if not, use first new-value in value list
         (cond
          ((plusp  (-  new-value-length (+ i 1)))
           (setf cur-new-value (nth i new-value))
           )
          (t (setf cur-new-value (car new-value))))
         ;;for match-instance-to-value only add one value to the slot
         (append-slot-list  instance slot cur-new-value)) 
    
        ;;If adding entire new-value to each instance, append the slot with the new-value whether item or list 
        (t (append-slot-list (eval instance) slot new-value)))
       ;;end dolist, clause
       ))
      (t  (print "ARGUMENT INSTANCE-LIST IS NOT A LIST")))

    ;;   (afout 'out (format nil "3 instance= ~A new-value= ~A ~%"  instance new-value))
    ;; let, defun
    ))

;;test
#|(defun testas2 ()
  (setf out nil)
  (let
      ((instance-list '(*class1-inst1 *class1-inst2 *class1-inst3))
       (slot 'children)
       (new-value  '(nv1 nv2 nv3)) ;; 'test-new-value)
       )
    (append-slot-lists instance-list slot new-value)
     (slot-value *class1-inst1 'children)
    ))|#
;;  (slot-value *class1-inst2 'children) 
;; works, for  'test-new-value)  returns (TEST-NEW-VALUE)
;;works, for  (new-value  '(nv1 nv2 nv3)), 
;;(slot-value *class1-inst3 'children) = (NV1 NV2 NV3)
;;works for 'children slot  in all 3 instances in'(*class1-inst1 *class1-inst2 *class1-inst3)

;;APPEND-SLOT-LIST
;;
;;ddd
(defun append-slot-list (instance slot new-value)
  "U-clos.lisp,"
  (let
      ((current-slot-list)
       )
    (setf current-slot-list (slot-value instance slot))
    (cond
     ((listp current-slot-list)
      (cond
       ((listp new-value)
        (setf current-slot-list (append current-slot-list  new-value)))
       (t   
        (setf current-slot-list (append current-slot-list (list new-value)))))
      ;;replace the original slot-value
      (setf (slot-value instance slot) current-slot-list))
     (t  (values  nil  "slot-value NOT a list")))
    ))
;;test
 ;;from above   (append-slot-list (eval instance-sym) class-instances-slot instance-sym)
#|(defun testup ()
  (setf out nil)
    (setf (slot-value  *class1-inst1  'class-instances) '(*class1-inst1 *class1-inst2 *class1-inst2))
  (let
      ((x)
       )
    (append-slot-list *class1-inst1 'class-instances '*new-inst)
   ;;(append-slot-list *class1-inst1 'class-instances '(*new-inst3 *new-inst4))
    ))|#
;;works with single new-value, returns:
;;(*CLASS1-INST1 *CLASS1-INST2 *CLASS1-INST2 *NEW-INST)
;;works with new-value list, returns:
;;(*CLASS1-INST1 *CLASS1-INST2 *CLASS1-INST2 *NEW-INST3 *NEW-INST4)  

;;not needed??
#|;;MODIFY-DEFAULT-INITARGS
;;
;;ddd
(defun modify-default-initargs (default-initargs  initargs-addition-list)
  "in U-clos.lisp, used to modify defclass default-initargs to include parents,children, etc."
  (let
      (;;(default-initargs)
       (new-default-initargs)
       (element-n -1)
       )
    (dolist (element default-initargs)
      (incf element-n)
      (cond
       ((equal element :default-initargs)
       (setf new-default-initargs default-initargs))
       (t
       
       (evenp element-n 
            (setf new-default-initargs (append element initargs-addition-list)
                  new-default-initargs-list (append new-default-initargs-list (list  default-initargs))))
       (t (setf new-default-initargs-list (append new-default-initargs-list (list element)))))
      ;;end dolist
      )
    new-default-initargs-list
    ))|#
;;test 
#|(defun testmdi ()
  (let
      ((default-initargs '(:default-initargs :description "description" :documentation "docs"))
        ;;was'((:documentation "docs")(:default-initargs :description "descrip")))
        (initargs-addition-list '(:parents '(class1 class2)))
         ;;was '(:parents '(class1 class2)))
        )
    (modify-default-initargs default-initargs-list initargs-addition-list)
    ))
|#;;works, returns:
#|((:DOCUMENTATION "docs") (:DEFAULT-INITARGS :DESCRIPTION "descrip" :PARENTS (QUOTE (CLASS1 CLASS2))))|#

;;SET-SLOT-VALUES
;;
;;ddd
(defun set-slot-values (instance  slot-list  slot-value-list)
  "U-clos"
  (loop
   for slot-value in slot-list
   for slot in slot-list
   do
   (setf (slot-value  instance slot) slot-value)
   ;;end loop, defun
   ))
;;TEST 
;;  (set-slot-values *sT1HigherSelf-inst '(raw-score) '(11))


;;  (let ((inst  *sT1HigherSelf-inst)) (setf (slot-value (eval inst) 'raw-score) 20))
;; works (slot-value  *sT1HigherSelf-inst 'raw-score) = 20

#|CL-USER 12 > (setf (slot-value  *sT1HigherSelf-inst 'raw-score) 10)
10
CL-USER 13 > (slot-value  *sT1HigherSelf-inst 'raw-score)
10|#



;;XXX------------------- FUNCTIONS TO ACCESS CLASSES AND INSTANCES  ----------------



;;FIND-CLASSNAME
;;
;;ddd
(defun find-classname (class-object &key is-class-p)
  "In U-clos.lisp, finds the classname string in a class INSTANCE/OBJECT (eg. CLASS1 in #<CLASS1 22B2538B>.  If  is-class-p, finds class in  #<STANDARD-CLASS CLASS1 21E8A5C3>.  Returns (values class-sym string-string)."
  (let
      ((class-string)
       (class-sym)
       )
    (setf class-string (format nil "~A~%" class-object))
    (cond
     (is-class-p
      ;;find from class object
      (setf class-string (subseq class-string 17)
            class-string (match-subseq  nil  class-string  :end-item '#\space)))
     ;;find from instance object
     (t  (setf class-string (subseq class-string 2 )
               class-string (match-subseq  nil  class-string  :end-item '#\space))))
    ;;make class symbol
    (setf class-sym (my-make-symbol class-string))
    (values class-sym class-string)
    ))
;;test, works:
;;(find-classname *class1-inst) = CLASS1 "CLASS1"



;;XXX -------     MY CLASS *CLASS-INST AND OTHER CLASS INSTANCES ----------

;;GET-CLASS-INST
;;
;;ddd
(defun get-class-inst (class)
  "In U-clos.lisp, class can be string or symbol. Returns   (values class-inst-sym class-inst-object class-inst-string  class-string)"
  (let
      ((class-string)
       (class-inst-string)
       (class-inst-sym)
       (class-inst-object)
       )
    (setf class-string (format nil "~A" class)
          class-inst-string (format nil "*~A-inst" class-string)
          class-inst-sym (my-make-symbol class-inst-string))
    (if  (and (symbolp (quote class-inst-sym))(boundp class-inst-sym))
      (setf  class-inst-object (eval class-inst-sym)))
    (values class-inst-sym class-inst-object class-inst-string   class-string)
    ))
;;test
;;  (get-class-inst 'outcome) =  *OUTCOME-INST  #<OUTCOME 25C9EBFF>  "*OUTCOME-inst"  "OUTCOME"
;;  (get-class-inst 'class1)
;;works, returns: *CLASS1-INST   #<CLASS1 22FAC627> "*CLASS1-inst"  "CLASS1"
;;also           
;;(my-make-symbol "*CLASS1-inst")
;;  (get-class-inst 'sbiog)
;; try on unbound symbol
;; (get-class-inst 'unboundsym) = *UNBOUNDSYM-INST  NIL "*UNBOUNDSYM-inst" "UNBOUNDSYM"
;; works, note that class-inst-object = NIL 
;;  (get-class-inst 'INTSS2ROMANTC)



;;GET-CLASS-INSTS
;;ddd
(defun get-class-insts (class-list)
  ;;  (setf out nil)
  (let
      ((class-inst)
       (class-inst-syms)
       (class-inst-objects)
       (class-inst-strings)
       )
    (dolist (class class-list)
      (multiple-value-bind (class-inst-sym class-inst-object class-inst-string class-string)
          (get-class-inst  class)
        ;;(afout 'out (format nil "~A ~A ~A~% " class-inst-sym class-inst-object class-inst-string))
        (setf  class-inst-syms (append class-inst-syms (list class-inst-sym))
               class-inst-objects (append class-inst-objects (list class-inst-object))
               class-inst-strings (append class-inst-strings (list class-inst-string)))
        ;;end mvb, dolist
        ))
    (values class-inst-syms class-inst-objects class-inst-strings)
    ))
;;test
;;(get-class-insts '(class1 class2))
;;works, returns: 
;;(*CLASS1-INST *CLASS2-INST) (#<CLASS1 226E77EF> #<CLASS1 226DF2E7>) ("*CLASS1-inst" "*CLASS2-inst")


;;SSS FINISH MAKING NEW METHODS BY WRAPPING AROUND SOME BELOW DEFUNS?
#|(defgeneric get-instances-method (class &optional slot-name)
  (:documentation "returns a list of all instances in the class. default slot-name is class-instances"))|#


;;GET-CLASS-INSTANCES
;;
;;ddd
(defun get-class-instances (class-instance &key class-instances-slot)
  "in u-clos.lisp, default class instance slot is class-instances. returns (values all-instance-syms class-instances-slot).RETURNS (values all-instance-syms class-instances-slot)"
  (let
      ((all-instance-syms)
       )
    (unless class-instances-slot
      (setf class-instances-slot 'class-instances.))

    (setf all-instance-syms (slot-value  class-instance  class-instances-slot))
    (values all-instance-syms class-instances-slot)
    ))
;;test
#|(defun testgi ()
  (let
      ((class-instance *class1-inst1) ;;*class1-inst1 can't be quoted, use actual object in slot-value
       (class-instances-slot 'class1-instances)
       )
    (setf  (slot-value  *class1-inst1  class-instances-slot) '(*class1-inst1 *class1-inst2 *class1-inst2))
    (multiple-value-setq (all-instance-syms class-instances-slot)
        (get-class-instances class-instance :class-instances-slot class-instances-slot ))
    (values all-instance-syms class-instances-slot)
    ))|#
;;works, returns:
#|cl-user 8 > (testgi)
(*class1-inst1 *class1-inst2 *class1-inst2)
class1-instances|#


;;GET-CLASS-INSTANCE-OBJECTS
;;
;;ddd
(defun get-class-instance-objects  (class-instance &key class-instances-slot)
  "in u-clos.lisp"
  (unless class-instances-slot
    (setf class-instances-slot 'class-instances))

  (let
      ((class-instance-syms (get-class-instances (eval class-instance)
                                                 :class-instances-slot class-instances-slot))
       (class-instance-objects)
       (object)
       )
    ;;(afout 'out (format nil "class-instance-syms= ~a~%" class-instance-syms))
    (loop
     for instance in class-instance-syms
     do
     (setf object (eval instance)
           class-instance-objects (append class-instance-objects (list object)))
     ;;end loop
     )
    (values class-instance-objects  class-instance-syms)
    ))
;;test
#|(defun testgo ()
  (setf out nil)
  (setf  (slot-value  *class1-inst1 'class-instances) '(*class1-inst1 *class1-inst2 *class1-inst2))
  (let
      ((class-instance '*class1-inst1)
       )
    (get-class-instance-objects  class-instance)
    ))|#
    


;;CLASS-INSTANCE-P
;;
;;ddd
(defun class-instance-p (instance &optional class)
  "In U-clos.lisp, returns T if instance is an instance, T T if instance of class. Returns (values is-instance-p is-class-instance-p instance-class-string instance-class-sym  class-string. NOTE: use QUOTED symbol for instance. Works on unbound symbols.)"
  (let
      ((instance-class-string)
       (class-string)
       (instance-class-sym)
       (is-instance-p)
       (is-class-instance-p)
       )
  ;;uses typep to do the work
  (cond
   ((and (symbolp instance)(boundp instance))
    (multiple-value-setq (instance-class-sym  instance-class-string)
        (find-classname (eval instance)))
    (setf  class-string (format nil "~A" class)
           is-instance-p T)
    (if class
        (setf is-class-instance-p (equal instance-class-string  class-string))))
   (t nil))   
    ;;(setf result (typep (eval instance) class))
  (values is-instance-p is-class-instance-p instance-class-string instance-class-sym  class-string)
  ))
;;test
;;test
;;  (class-instance-p '*class1-inst) ;;class not listed in args
;;works, returns: 
;;T NIL "CLASS1"CLASS1"NIL"
;;  (class-instance-p '*class1-inst  'class1)  ;;in class1  = T T
;;works, returns T T "CLASS1" CLASS1 "CLASS1"
;; (class-instance-p *class1-inst 'class2)  ;;not in class2
;;works, returns T NIL "CLASS1" CLASS1 "CLASS2"
;; (class-instance-p '*class6-inst  'class1)
;; works, returns NIL NIL NIL NIL NIL
;;(symbolp '*class1-inst) = T
;; (symbolp '*class6-inst)

;;
;; (symbolp 'xxu)
;; (boundp 'xxz)




;;XXX ----------------------------- PARENTS AND CHILD CLASSES ----------------------------------


;;GET-PARENT-SYMS
;;sss
(defun get-parent-syms (class-inst &key nth)
  "In U-clos.lisp, from class-inst,  returns (values list of parents and if nth,, nth parent)"
  (let
      ((parents)
       (nth-parent)
;;;*** Warning in GET-PARENT-SYMS: CLASS-INST assumed special
; Analyse; Transform
;;;*** Warning in GET-PARENT-SYMS: CLASS is bound but not referenced
       )
    ;;  (setf class-inst  (my-make-symbol (format nil "*~A-inst" class)))

    (setf parents (slot-value class-inst 'parents))
    (cond
     ((and nth (>= nth (list-length parents)))
      (setf nth-parent (nth nth parents)))
     (t nil))
    (values parents nth-parent)))
;;test
#|(defun testgps ()
  (get-parent-syms 'class1 :nth 0))
|#






;;XXX --------------------- ACCESSING MY-DEFCLASS SLOT VALUES -----------------------


;;GET-ALL-CLASS-INSTANCES-SLOT-VALUE
;;
;;defmethod causes unbound error in test1 below
;;ddd
(defun get-all-class-instances-slot-value (class-generic-inst slot  &key nth class-instances-slot)
  "in u-clos.lisp, returns values list= slot-values-list all instance symbols and all instance objects. by default returns a list of slot-value for all instances, if nth is a number, returns only that one value, instance-sym and instance-object--use ordinary slot value if know instance-sym."
  (let
      ((slot-value)
       (all-instance-syms)
       (all-instance-objects)
       (instance-sym)
       (instance-object)
       (slot-values-list)
       (class-generic-inst-object   (eval class-generic-inst))
       )

   ;;(afout 'out (format nil "class-generic-inst-object ~a~%" class-generic-inst-object))

   (unless class-instances-slot
     (setf class-instances-slot 'class-instances))

   (setf all-instance-syms (slot-value  class-generic-inst-object  class-instances-slot))
 
   ;;(afout 'out (format nil "all-instance-syms ~a~%" all-instance-syms))
   (cond
    (nth
     (setf instance-sym (nth nth all-instance-syms))
     (multiple-value-setq (slot-value instance-sym instance-object)
         (get-slot-value instance-sym slot))
     (setf slot-values-list (append slot-values-list (list slot-value))))
    (t
     (loop
      for instance in all-instance-syms
      do
      ;;change to accumulate values
      (multiple-value-setq (slot-value instance-sym instance-object)
          (get-slot-value instance slot))
   ;;(afout 'out (format nil "slot-value=~a  instance-sym= ~a instance-object" slot-value instance-sym  instance-object))
      (setf slot-values-list (append slot-values-list (list slot-value))
            all-instance-objects (append all-instance-objects (list instance-object)))
      ))
    ) 
      (values slot-values-list all-instance-syms all-instance-objects)
      ))
;;test

#|(defun test1 ()
  (setf out nil)
  (setf (slot-value  *class1-inst1  'class-instances) '(*class1-inst1 *class1-inst2 *class1-inst2))
  (afout 'out (format nil "(slot-value  *class1-inst1  'class1-instances) = ~a~%"(slot-value  *class1-inst1  'class1-instances)))
  (multiple-value-setq (slot-values-list all-instances all-instance-objects)
  (get-all-class-instances-slot-value *class1-inst1 'name :class-instances-slot 'class1-instances))
  (fout out)
  (values slot-values-list all-instances all-instance-objects)
  )|#
;;works, returns values:
;;cl-user 48 > (test1)
#|("new-name" "inst2" "inst2")
(*class1-inst1 *class1-inst2 *class1-inst2)
(#<class1 23348ec7> #<class1 2335444f> #<class1 2335444f>)|#

;;(list-length nil)





;;GET-SUPERCLASS-MULTI-SLOT-VALUES
;;
;;ddd
(defun get-superclass-multi-slot-values (superclass-name slot-list 
                          &key (omit-subclasses-list '( "SUBSCALE" "COMPOSITE-SCALE" 
                                                       "BIO-TEXT" "ACAD-ACH" "NO-SCALE"))
                                   (add-separator-line "--------------------------------------------------") 
                                   convert-classvar-p )
  "In U-clos, lists all the subclasses in a superclass and finds the slot-values for slots in slot-list for each subclass.  If convert-classvar-p, then only need class name, not class-instance."
  (let
      ((superclass-inst)
       (subclass-namelist)
       (subclass-inst)
       (slot-name)
       (slot-value)
       ;;(name-value-list)
       (subclass-slotname-value-list)
       (all-subclass-slotname-value-lists)
       (all-subclass-name-slotname-value-lists)
       )
#|    (cond
     (convert-classvar-p
      (setf superclass-sym1 (my-make-symbol (format nil "*~A" superclass-sym))))
     (t (setf superclass-sym1 superclass-sym1)))|#

    (setf subclass-namelist
          (find-all-direct-subclass-names (list superclass-name)))
    ;;(afout 'out (format nil "subclass-namelist= ~A" subclass-namelist))

    (loop
     for subclass-name in subclass-namelist
     do
     (cond
      ;;do not use these subclasses
      ((member subclass-name  omit-subclasses-list :test 'string-equal) NIL)
      (t
       (setf subclass-inst (my-make-symbol (format nil "*~A-INST" subclass-name)))
       (setf subclass-slotname-value-list
             (get-multi-slot-values subclass-inst slot-list))
     ;;(afout 'out (format nil "subclass-slotname-value-list= ~A" subclass-slotname-value-list))

     (cond
      (add-separator-line
       (setf all-subclass-name-slotname-value-lists 
             (append all-subclass-name-slotname-value-lists  
                     (list (list add-separator-line subclass-name subclass-slotname-value-list)))))
       (t 
        (setf all-subclass-name-slotname-value-lists 
             (append all-subclass-name-slotname-value-lists  
                     (list (list subclass-name subclass-slotname-value-list))))))
       ;;end t, cond
       ))
     ;;end loop
     )
    all-subclass-name-slotname-value-lists
    ;;end let,  get-superclass-multi-slot-values
    ))
;;TEST
;;  (setf  *all-scale-slotvals1 (get-superclass-multi-slot-values  "SCALE" '(name-string label scale-name description scale-group-name scale-questions mean-score help-links) ))
;; (pprint *all-scale-slotvals1)
;; works Example:
#|("--------------------------------------------------"
  "SGRFEARS"
  ((NAME-STRING "sgrfears")
   (LABEL "s-Low greatest fears")
   (SCALE-NAME "Low Greatest Fears")
   (DESCRIPTION "A key HQ scale. Low degree of common major fears such as illness, poverty, death, failure, rejection, and confidence in ability to overcome fears or circumstances. Fears are interesting to people per se.  Your greatest fears reflect your top values and goals in life and are related to your happiness and success. The underlying fear is usually that one's greatest values/goals will be unsatisfied. Your underlying fears can be powerful sources of pain and avoidance motivation. Underlying fears give your little jolts of fear or anxiety daily as you get a thought that is related to them. 
      Overcoming these underlying fears can help you be less fearful and anxious the rest of your life! This scale correlates .462 with happiness, .417 with low depression, .375 with low anxiety, .241 with low anger/aggression. (12 items)")
   (SCALE-GROUP-NAME "beliefs")
   (SCALE-QUESTIONS (WOVHAPPY
                     WOVPOOR 
                     WOVILL
                     WOVDEATH
                     WOVALONE
                     WOVNOLOV
                     WOVLIKED
                     WOVPERSO
                     WOVPROBL
                     WOVDISCO
                     WOVSUCCE
                     WOVOVERC))
   (MEAN-SCORE ".601")
   (HELP-LINKS ("h43dark.htm"))))|#

;;TEST SUBSCALE INFO PRINTING
;;  (setf  *all-subscale-slotvals1 (get-superclass-multi-slot-values  "SUBSCALE" '(name-string label scale-name description scale-group-name scale-questions mean-score help-links) ))
;; (pprint *all-scale-slotvals1)




;;GET-MULTI-SLOT-VALUES
;;
;;ddd
(defun get-multi-slot-values (instance-sym slot-list &key convert-classvar-p )
  "In U-clos, gets slot-values for each slot on slot-list. RETURNS multi-slot-value-list"
  (let
      ((slot)
       (slot-name)
       (slot-value)
       (multi-slot-value-list)
       (instance-sym1)
       (instance-object)
       (n 0)
       )
    (loop
     for slot-name in slot-list
     do
     (incf n)
     (cond  ;;(car `(,slot-name))
      ((= n 1)
       (multiple-value-setq ( slot-value  instance-sym1 instance-object)
             (get-slot-value instance-sym slot-name :convert-classvar-p convert-classvar-p)))
      (t  (setf slot-value  (get-slot-value instance-sym1 slot-name ))))
     (setf multi-slot-value-list (append multi-slot-value-list (list (list slot-name slot-value))))
     ;;end loop
     )
     ;;end let, get-multi-slot-values
     (values multi-slot-value-list instance-sym1)
     ))
;;TEST
;;  (get-multi-slot-values 'ssl11NotNonAcadMot '(name-string label scale-name description scale-group-name scale-questions mean-score help-links) :convert-classvar-p T)
;; works= ((NAME-STRING "ssl11NotNonAcadMot") (LABEL "ssl11NotNonAcadMot") (SCALE-NAME "College Internal Motivation") (DESCRIPTION "Internal Motivation--to be in college. Internal motives versus pleasing parents, making money, or being confused why in school. Financially self-supporting. Internal motivation for accomplishing any task--including a college degree--is associated with greater success and happiness. (4 items)") (SCALE-GROUP-NAME "acad-learning") (SCALE-QUESTIONS (STUEXTMO STUMONEYNEW STUCONFU STUFINDE STUCAREE)) (MEAN-SCORE 0.593) (HELP-LINKS ("c15-carp.htm" "time_management.htm" "life_goals_and_meaning.htm")))      *SSL11NOTNONACADMOT-INST










;;GET-SLOT-VALUES
;;
;;ddd
(defun get-slot-values (instance-sym-list slot-name-list &key convert-classvar-p)
  "in u-clos.lisp, differs from normal (slot-value ...)  in that it evals the instance-sym arg first. If  convert-classvar-p, instance-sym can be a classvar which is converted to a class-instance of form *classname-inst. If  slot-name-list= a slot-name symbol, uses that symbol for EVERY instance.  RETURNS (values slot-values-list class-inst-list)."
  (let
      ( (slot-values-list)
        (class-inst-list)
        (n -1)
        (slot-name slot-name-list)
        (slot-value)
        )
    (loop
     for instance in instance-sym-list
     do
     (incf n)
     (when (listp slot-name-list)
       (setf slot-name (nth n slot-name-list)))
     (when convert-classvar-p
       (setf instance (get-class-inst instance)))
     (setf slot-value (slot-value  (eval instance) slot-name)
           slot-values-list (append slot-values-list (list slot-value))
           class-inst-list (append class-inst-list (list instance)))
     ;;end loop
     )
    (values slot-values-list class-inst-list)
    ;;end let, get-slot-values
    ))
;;TEST
;;  (get-slot-values '( sscpPosThoughts  ssieautony) 'relative-score :convert-classvar-p t))
;; works = (0.5835 0.71433336)      (*SSCPPOSTHOUGHTS-INST *SSIEAUTONY-INST)


#|(let ((xx))
(loop
 for n from 1 to 10
 for i from 1 to 5
 do
 (setf xx (append xx (list n i)))
 ) xx) = (1 1 2 2 3 3 4 4 5 5) NOTE STOPS AFTER SHORTEST FOR|#



;;GET-SLOT-VALUE
;;
;;ddd
(defun get-slot-value (instance-sym slot-name &key convert-classvar-p)
  "in u-clos.lisp, differs from normal (slot-value ...)  in that it evals the instance-sym arg first. If  convert-classvar-p, instance-sym can be a classvar which is converted to a class-instance of form *classname-inst. RETURNS (values slot-value  instance-sym instance-object). ALSO checks SLOT-BOUNDP"
  (if convert-classvar-p 
      (setf instance-sym (get-class-inst instance-sym)))
  (let
      ((slot-value)
       (instance-object (eval instance-sym))
       )
    (if (slot-boundp instance-object slot-name)
        (setf slot-value (slot-value instance-object slot-name)))

    ;;(afout 'out (format nil "in  get-slot-value, slot-value= ~a~%"   slot-value))
    (values slot-value  instance-sym instance-object)
    ))
;;TEST
;; (get-slot-value 'sselfman 'relative-score  :convert-classvar-p T)
;; works 1.0  *SSELFMAN-INST #<SSELFMAN 23F9CC8F>
;; (get-slot-value 'sselfman 'range  :convert-classvar-p T) 
;; (note: range unbound) works = NIL *SSELFMAN-INST #<SSELFMAN 23F9CC8F>
;;works
#|(defun test2 ()
  (let
      ((list '(1 2 3 4))
       (new-list)
       (x)
       )
    (loop
      for i in list
      do
      (setf i (+  i 2)
            new-list (append new-list (list i))) 
      )
    new-list))|#



;;MY-OBJECT-TO-STRING
;;
;;ddd
(defun my-object-to-string (object)
  "In U-clos, INPUT= object-symbol(not quoted) or object output that would normally be like #<STANDARD-CLASS SSL14MATHAPT 21C06BF7>.  RETURNS a string of that object eg. \"#<STANDARD-CLASS SSL14MATHAPT 21C06BF7>\". Can pass to functions operating on strings to get eg CLASS-NAME from it. IS THERE A BETTER WAY?"
  (let
      ((object-string (format nil "~A" (print-object object NIL)))
       )
    object-string
    ))
;;TEST (setf  ***ots (my-object-to-string *SCALE-INST))

;;CCC

;;FIND-OBJECT-NAME
;;
;;ddd
(defun find-object-info (object)
   "In U-clos, INPUT= object-symbol(not quoted) or object output that would normally be like #<STANDARD-CLASS SSL14MATHAPT 21C06BF7>.  RETURNS (values object-name superclass instance-id  object-name-list)"
   (let
       ((object-string) ;;(format nil "~A" (print-object object NIL)))
      ;;  (object-name (divide-string-to-tokens object-string   '("  " #\space) :ignore-char-list '(#\# #\< )));;:word-delim-list '(" ") :nth-word 2 :delete-list '("\#" "\<")))
      (object-name-list)
      (length-obj-list) ;; (list-length object-name-list))
      (superclass) ;;(first object-name-list))
      (object-name)
      (instance-id)
        )
     (setf object-string (format nil "~A" (print-object object NIL))
           object-name-list  (divide-string-to-all-tokens object-string  )
           length-obj-list (list-length object-name-list)
           superclass (first object-name-list))

     (cond
      ((= length-obj-list 2)
       (setf instance-id (second  object-name-list)))
      ((= length-obj-list 3)
       (setf object-name (second  object-name-list)
             instance-id (third object-name-list)))
      (t  (setf object-name (second  object-name-list))))
   (values object-name superclass instance-id  object-name-list)
     ))
;;THIS VERSION GOT WIERD ERROR MESSAGES ABOUT BINDING TOO EARLY
#|(defun find-object-info (object)
   "In U-clos, INPUT= object-symbol(not quoted) or object output that would normally be like #<STANDARD-CLASS SSL14MATHAPT 21C06BF7>.  RETURNS (values object-name superclass instance-id  object-name-list)"
   (let*
       ((object-string (format nil "~A" (print-object object NIL)))
      ;;  (object-name (divide-string-to-tokens object-string   '("  " #\space) :ignore-char-list '(#\# #\< )));;:word-delim-list '(" ") :nth-word 2 :delete-list '("\#" "\<")))
      (object-name-list
       (divide-string-to-all-tokens object-string  ))
      (length-obj-list (list-length object-name-list))
      (superclass (first object-name-list))
      (object-name)
      (instance-id)
        )
     (cond
      ((= length-obj-list 2)
       (setf instance-id (second  object-name-list)))
      ((= length-obj-list 3)
       (setf object-name (second  object-name-list)
             instance-id (third object-name-list)))
      (t  (setf object-name (second  object-name-list))))
   (values object-name superclass instance-id  object-name-list)
     ))|#
;;TEST
;;  (setf  *acad-subclasses (CLASS-DIRECT-SUBCLASSES (FIND-CLASS 'ACAD-LEARNING)) 
;;   (progn (setf out nil) (find-object-info (car *acad-subclasses)))
;; WORKS, RETURNS
#|"SSL14MATHAPT"
"STANDARD-CLASS"
"21ABC667"
("STANDARD-CLASS" "SSL14MATHAPT" "21ABC667")|#
;; TEST2 
;;   (find-object-info *SCALE-INST)
;;works, results= "SCALE" "21A9F7FF" ("SCALE" "21A9F7FF")


;;FIND-ALL-DIRECT-SUBCLASS-NAMES
;;
;;ddd
(defun find-all-direct-subclass-names (classname-list &key (delete-duplicates-p T) reverse-subclass-order-p) 
  "In U-clos, For each class in classname-list, 1-finds subclasses, 2-If delete-duplicates-p, deletes all duplcates RETURNS (values all-subclass-names  all-subclasses  class-sym-list class-list). reverse-cat-scales-p causes classes to be listed in order opposite in class definitions."
  (let*
      (
      ;;(class-sym (my-make-symbol class-name))
      ;; (class (find-class class-sym))
      ;;  (subclasses (class-direct-subclasses class))
       (all-subclass-names)
       (all-subclasses)
       (class-sym-list)
       (class-list)
       (subclass-names)
       (subclasses) 
       )
    (loop
     for classname  in classname-list
     do     
     (multiple-value-bind (subclass-names subclasses class-sym class)
         (find-direct-subclass-names classname
                                     :reverse-subclass-order-p reverse-subclass-order-p)
       (setf all-subclass-names (append all-subclass-names  subclass-names)
             all-subclasses (append all-subclasses  subclasses)
             class-sym-list (append class-sym-list (list class-sym))
             class-list (append class-list (list class)))
       ;;end mvb,loop
       ))
    ;;remove duplicates?
    (if  delete-duplicates-p
        (setf all-subclass-names (remove-duplicates all-subclass-names)
              all-subclasses (remove-duplicates all-subclasses)))
                

    (values all-subclass-names  all-subclasses  class-sym-list class-list)
    ;;end let, find-all-direct-subclass-names
    ))
;;TEST HQ
;; (find-all-direct-subclass-names '(HQ)) 
;; RESULT = ("INTSS7COLLAB" "INTSS6POSSUP" "INTSS5INDEP" "INTSS4LOVERES" "INTSS3LIBROLE" "INTSS2ROMANTC" "INTSS1BOPENHON" "INTSS1AASSERTCR" "SEMOTCOP" "SSELFMAN" "SSLFCONF" "SGRFEARS" "SETHBEL" "SIECONTR" "SWVGRATENT" "STBSLFWO" "SWORLDVIEW" "ST11DUTYPUNCTUAL" "ST10OVERCMPROBACCEPTSELF" "ST9VALUESELFALLUNCOND" "ST8ATTENTIONFUNEASY" "ST7IMPACTCHALLENGEEXPLOR" "ST6GODSPIRITRELIG" "ST5-ORDERPERFECTIONGOODNESS" "ST4SUCCESSSTATUSMATER" "ST3FAMCARE" "ST2SOCINTIMNOFAM" "ST1HIGHERSELF")
;;TEST
;;  (find-all-direct-subclass-names '(outcome  acad-learning))
;;works, returns= ("SRPEOPLE" "SRELHLTH" "SRANGAGG" "SRANXIET" "SRDEPRES" "SEHAPPY" "SSL14MATHAPT" "SSL13VERBALAPT" "SSL12STDYTMAVAIL" "SSL11NOTNONACADMOT" "SSL10MEMNOTANX" "SSL9ATTENDHW" "SSL8STUDYENVIR" "SSL7MATHSCIPRINC" "SSL6SELFMANACADGOALS" "SSL5BASICSTUDYSKILLS" "SSL4BLDMENTALSTRUCT" "SSL3WRITINGSKILLS" "SSL2SATISCAMPUSFACFRIENDSGRDES" "SSL1BCONFIDNOTAVOIDSTUDY" "SSL1CONFIDEFFICSTUDYTEST")  also, sample= (#<STANDARD-CLASS SRPEOPLE 21D54FF3> #<STANDARD-CLASS SRELHLTH 21A8C913> #<STANDARD-CLASS SRANGAGG 21A60317> #<STANDARD-CLASS SRANXIET 21D54BC3> #<STANDARD-CLASS SRDEPRES 21A8BA47> #<STANDARD-CLASS SEHAPPY 21AC904B> #<STANDARD-CLASS SSL14MATHAPT 21A84FFF>
;; and finally  (OUTCOME ACAD-LEARNING)    (#<STANDARD-CLASS OUTCOME 21A5AFFF> #<STANDARD-CLASS ACAD-LEARNING 21A8E74F>)


;;  (setf xxrd (remove-duplicates '(a b c b e a g h))) = (C B E A G H)

;;FIND-DIRECT-SUBCLASS-NAMES
;;
;;ddd
(defun find-direct-subclass-names (class-name
                                   &key reverse-subclass-order-p)
  "In U-clos, RETURNS (values subclass-names subclasses class-sym class)"
  (let*
      ((class-sym (my-make-symbol class-name))
       (class (find-class class-sym))
       (subclasses (class-direct-subclasses class))
       (subclass-names)
       (subclass-name)
       (superclass)
       (instance-id)
       (subclass-info-list)
       )
    (loop
     for subclass in subclasses
     do     
     ;; (setf subclass-string (format nil "~A" (print-object subclass NIL)))
     (multiple-value-setq (subclass-name superclass instance-id
                                         subclass-info-list)  
         (find-object-info subclass))
     (setf subclass-names (append subclass-names (list subclass-name)))
     ;;end loop
     ) 
    ;;class-direct-subclasses above returns subclasses in reverse order
    (unless reverse-subclass-order-p
      (setf subclass-names (reverse subclass-names)
            subclasses (reverse subclasses)))
    (values subclass-names subclasses class-sym class)
    ;;mvb, let, find-direct-subclass-names
    ))
;;TEST
;;(find-direct-subclass-names "ACAD-LEARNING")
;;works, returns = ("SSL14MATHAPT" "SSL13VERBALAPT" "SSL12STDYTMAVAIL" "SSL11NOTNONACADMOT" "SSL10MEMNOTANX" "SSL9ATTENDHW" "SSL8STUDYENVIR" "SSL7MATHSCIPRINC" "SSL6SELFMANACADGOALS" "SSL5BASICSTUDYSKILLS" "SSL4BLDMENTALSTRUCT" "SSL3WRITINGSKILLS" "SSL2SATISCAMPUSFACFRIENDSGRDES" "SSL1BCONFIDNOTAVOIDSTUDY" "SSL1CONFIDEFFICSTUDYTEST")
;;sample of subclasses (#<STANDARD-CLASS SSL14MATHAPT 21A84FFF> #<STANDARD-CLASS SSL13VERBALAPT 21A8A33B> #<STANDARD-CLASS SSL12STDYTMAVAIL 21A8FE8F> #<STANDARD-CLASS SSL11NOTNONACADMOT 21D8B2B3> #<STANDARD-CLASS SSL10MEMNOTANX 21D7572B> #<STANDARD-CLASS SSL9ATTENDHW 221A11C7
;;class-sym, class = ACAD-LEARNING    #<STANDARD-CLASS ACAD-LEARNING 21A8E74F>
;;(find-direct-subclass-names )


;;GET-SUBSCALE-NAMES
;;
;;ddd
(defun get-subscale-names (scale)
  "In U-clos, RETURNS (values subscale-names non-subscales). Non-subscales are subclasses that are not subscales."
  (let
      ((subscale-names)
       (subclasses (find-direct-subclass-names scale))
       (non-subscales)
       (superclass-names)
       )
    (loop
     for subclass in subclasses
     do
     (setf superclass-names (find-direct-superclass-names subclass))
     (cond
      ((member subclass superclass-names)
       (setf subscale-names (append subscale-names (list subclass))))
      (t (setf non-subscales (append non-subscales (list subclass)))))
     ;;end loop
      )
    (values subscale-names non-subscales)
    ;;end let, get-subscale-names
    ))
;;TEST
;;  (get-subscale-names 'sworldview)
;; works = ("SSWVGRATPT" "SSWVOPTIMS" "SSWVENTIT")
;;
;;(member "subclass" (find-direct-superclass-names 'sswvoptims))



;;FIND-DIRECT-SUPERCLASS-NAMES
;;
;;ddd
(defun find-direct-superclass-names (subclass-name)
  "In U-clos, RETURNS (values class-names superclasses subclass-sym subclass)"
  (let*
      ((subclass-sym (my-make-symbol subclass-name))
       (subclass (find-class subclass-sym))
       (superclasses (class-direct-superclasses subclass))
       (class-names)
       )
    (loop
     for class in superclasses
     do     
     ;; (setf superclass-string (format nil "~A" (print-object superclass NIL)))
     (multiple-value-bind (class-name superclass instance-id
                                      class-info-list)  
         (find-object-info class)
       (setf class-names (append class-names (list class-name)))
       ;;end mvb,loop
       ))
    (values class-names superclasses subclass-sym subclass)
    ;;mvb, let, find-direct-superclass-names
    ))
;;TEST
;;(find-direct-superclass-names "ACAD-LEARNING")
;;works, returns= ("ASSESSMENT" "PER-SYSTEM")   (#<STANDARD-CLASS ASSESSMENT 21E90D3F> #<STANDARD-CLASS PER-SYSTEM 21E900A3>)   ACAD-LEARNING   #<STANDARD-CLASS ACAD-LEARNING 21A8E74F>

;;test:  (class-direct-superclasses (find-class  'ACAD-LEARNING))
;;results = (#<STANDARD-CLASS ASSESSMENT 21E90D3F> #<STANDARD-CLASS PER-SYSTEM 21E900A3>)

;; xxx ----------------------------------------- TESTS ------------------------------------------------------


;;TO FIND IF INSTANCE IS IN A CLASS
;;NOTE: NO QUOTE ON *class1-inst, QUOTE ON class1
;;  (typep *class1-inst 'class1)  = T
;;   (typep *class1-inst 'class2)  = NIL
;;    (typep *class1-inst 'top-class) = T
;;   (typep *class1-inst 'subclass1)
;;   (typep *class1-inst  T)
;;   (boundp 'this)
;;   (typep class1 'top-class) = unbound error
;;    (symbolp 'class1)

   

#|
;;USE AS A MODEL FOR MY TOP CLASS IN ANY PROJECT
;;ddd
(defclass top-class ()
  ((name
    :initarg :name
    :accessor name
    :initform "Initial top-class name"
    :documentation "name")
   (description
      :initarg :description
    :accessor description
    :initform "Initial description"
    :documentation "description")
   (object-docs
     :initarg :object-docs
    :accessor object-docs
    :initform "Put docs here."
    :documentation "Specific class or instance object-docs")
   (parents
     :initarg :parents
    :accessor parents
    :type :list
     :initform nil
    :documentation "Parent/superclass classes")
   (children
     :initarg :children
    :accessor children
    :type :list
     :initform nil
    :documentation "Children classes")
   (class-instances
    :initarg :class-instances
    :type :list
    :initform nil
    :accessor class-instances
    :documentation "class-instances-must use this slot for all subclass instances or can't use my accessor methods/functions")
   (top-class-var1
    :initarg :top-class-var1
    :accessor top-class-var1
    :initform nil
    :documentation "Misc var to be used only by top-class.")
   ))


(defclass class1 (top-class)
  ((class1-name
    :initarg :class1-name
    :accessor class1-name
    :initform "initial class1 name"
    :documentation "class1-name")
   (class1-var1
    :initarg :class1-var1
    :initform 99
    :accessor class1-var1
    :documentation "class1-var1")
   )
  (:default-initargs  :parents '(top-class))(:documentation "class1")
  )
 

(defclass class2 (top-class)
  ((class2-name
    :initarg :class2-name
    :accessor class2-name
    :initform "class2-name"
    :documentation "class2-name")
   (class2-var1
    :initarg :class2-var1
    :initform 99
    :accessor class2-var1
    :documentation "class2-var1")
   ))

(defclass class3 (top-class)
  ((class3-name
    :initarg :class2-name
    :accessor class2-name
    :initform "class2-name"
    :documentation "class2-name")
   (class3-var1
    :initarg :class2-var1
    :initform 299
    :accessor class2-var1
    :documentation "class2-var1")
   ))

(defclass subclass1 (class1 class2)
  ((subclass1-name
    :initarg :subclass1-name
    :initform "must supply a name."
    :accessor subclass1-name
    :documentation "subclass1-name")
   (subclass1-var1
    :initarg :subclass1-var1
    :initform 99
    :accessor subclass1-var1
    :documentation "subclass1-var1")
   ))

(defclass subclass2 (class2)
  ((subclass2-name
    :initarg :subclass2-name
    :initform "initial name"
    :accessor subclass2-name
    :documentation "subclass2-name")
   (subclass2-var1
    :initarg :subclass2-var1
    :initform 99
    :accessor subclass2-var1
    :documentation "subclass2-var1")
   ))


(defparameter *class1-inst (make-instance 'class1 :name "*class1-inst" :class1-name "*class1-inst"))
(defparameter *class2-inst (make-instance 'class1 :name "*class1-inst" :class1-name "*class1-inst"))
(defparameter *class3-inst (make-instance 'class1 :name "*class1-inst" :class1-name "*class1-inst"))

(defparameter *class1-inst1 (make-instance 'class1 :name "inst1" :class1-name "class1-inst-name1"))
(defparameter *class1-inst2  (make-instance 'class1 :name "inst2" :class1-name "class1-inst-name2"))
(defparameter *class1-inst3  (make-instance 'class1 :name "inst3" :class1-name "class1-inst-name3"))

#|(slot-value  *class1-inst1  'name) ;; = "inst1"
(setf (slot-value  *class1-inst1  'name)  "new-name") 
(slot-value  *class1-inst1  'name)  ;;=  "new-name"|#
|#   
    





;;HHH---------------------------------------------- HELP ---------------------------------------------------
;;
#|
defclass class-name ({superclass-name}*) ({slot-specifier}*) [[class-option]]
=> new-class
slot-specifier::= slot-name | (slot-name [[slot-option]])
slot-name::= symbol
slot-option::= {:reader reader-function-name}* | 
               {:writer writer-function-name}* | 
               {:accessor reader-function-name}* | 
               {:allocation allocation-type} | 
               {:initarg initarg-name}* | 
               {:initform form} | 
               {:type type-specifier} | 
               {:documentation string} 
function-name::= {symbol | (setf symbol)}
class-option::= (:default-initargs . initarg-list) | 
                (:documentation string) | 
                (:metaclass class-name) 
|#
;;
;;FROM P SEIBEL, PCL -- EXAMPLES, ETC FROM SECTIONS ON CLOS
;;
;;
#|  BEGIN COMMENT CHAR
#|(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than amount."))|#

#| (defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))     |#



(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than amount."))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

(defmethod withdraw ((proxy proxy-account) amount)
  (withdraw (proxied-account proxy) amount))

(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))
    (call-next-method)))

(defmethod withdraw :before ((account checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))

(defgeneric priority (job)
(:documentation "Return the priority at which the job should be run.")
(:method-combination +))
;;By default all these method combinations combine the primary methods in most-specificfirst

(defgeneric priority (job)
  (:documentation "Return the priority at which the job should be run.")
  (:method-combination + :most-specific-last))
;;The primary methods on a generic function that uses


#|The primary methods on a generic function that uses one of these combinations must be
qualified with the name of the method combination. Thus, a primary method defined on
priority might look like this:|#
(defmethod priority + ((job express-job)) 10)

#|(defgeneric beat (drum stick)
(:documentation
"Produce a sound by hitting the given drum with the given stick."))
Then you can define various multimethods to implement beat for the combinations you
care about. For example:
(defmethod beat ((drum snare-drum) (stick wooden-drumstick)) ...)
(defmethod beat ((drum snare-drum) (stick brush)) ...)
(defmethod beat ((drum snare-drum) (stick soft-mallet)) ...)
(defmethod beat ((drum tom-tom) (stick wooden-drumstick)) ...)
(defmethod beat ((drum tom-tom) (stick brush)) ...)
(defmethod beat ((drum tom-tom) (stick soft-mallet)) ...)|#


#|(defclass bank-account () ...)
(defclass checking-account (bank-account) ...)
(defclass savings-account (bank-account) ...)
|#
(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))))
;;Most of the time the combination of :initarg


;;Now you can create an account and specify the slot values at the same time.
(defparameter *account*
  (make-instance 'bank-account :customer-name "John Doe" :balance 1000))

#|(slot-value *account* 'customer-name) ÌØÄÌ±Ø "John Doe"
(slot-value *account* 'balance) ÌØÄÌ±Ø 1000
If you don‚Äôt supply a :balance argument to MAKE-INSTANCE,
|#
(defparameter *account* (make-instance 'bank-account)) ÌØÄÌ±Ø *ACCOUNT*
(setf (slot-value *account* 'customer-name) "John Doe") ÌØÄÌ±Ø "John Doe"
(setf (slot-value *account* 'balance) 1000) ÌØÄÌ±Ø 1000
Now you can access the value of the slots.
(slot-value *account* 'customer-name) ÌØÄÌ±Ø "John Doe"

#|Most of the time the combination of :initarg and :initform options will be sufficient to
properly initialize an object. However, while an initform can be any Lisp expression, it has no
access to the object being initialized, so it can‚Äôt initialize one slot based on the value of another.
For that you need to define a method on the generic function INITIALIZE-INSTANCE.
The primary method on INITIALIZE-INSTANCE specialized on STANDARD-OBJECT takes
care of initializing slots based on their :initarg and :initform options. Since you don‚Äôt want to
disturb that, the most common way to add custom initialization code is to define an :after
method specialized on your class.6 For instance, suppose you want to add a slot account-type
that needs to be set to one of the values :gold, :silver, or :bronze based on the account‚Äôs
initial balance. You might change your class definition to this, adding the account-type slot
with no options:|#

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))
#|Then you can define an :after method on INITIALIZE-INSTANCE that sets the
account-type slot based on the value that has been stored in the balance slot.7|#
;;xxx
#|INITIALIZE-INSTANCE seems VERY IMPORTANT to use as both a generic function and method. NOTE it is the ONLY? way to initialize an object/instance based upon data within other class-instance slots.|#

(defmethod initialize-instance :after ((account bank-account) &key)
(let ((balance (slot-value account 'balance)))
(setf (slot-value account 'account-type)
(cond
((>= balance 100000) :gold)
((>= balance 50000) :silver)
(t :bronze)))))

#|On the other hand, if an INITIALIZE-INSTANCE method specialized on a particular class
does specify a &key parameter, that parameter becomes a legal parameter to MAKE-INSTANCE
when creating an instance of that class. For instance, if the bank sometimes pays a percentage
of the initial balance as a bonus when an account is opened, you could implement that using a
method on INITIALIZE-INSTANCE that takes a keyword argument to specify the percentage of
the bonus like this:|#
(defmethod initialize-instance :after ((account bank-account)
                                       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
          (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))
#|By defining this INITIALIZE-INSTANCE method, you make :opening-bonus-percentage a
legal argument to MAKE-INSTANCE when creating a bank-account object.
CL-USER>|#
(defparameter *acct* (make-instance 'bank-account
                                    :customer-name "Sally Sue"
                                    :balance 1000
                                    :opening-bonus-percentage 5))
#|*ACCT*
CL-USER> (slot-value *acct* 'balance)
1050
7.|#
;;You can remove the unwanted primary method
;;  using the functions REMOVE-METHOD and FIND-METHOD.
#|(remove-method #'initialize-instance
               (find-method #'initialize-instance () (list (find-class 'bank-account))))|#
#|
Finally, using accessor functions makes your code tidier since it helps you avoid lots of
uses of the rather verbose SLOT-VALUE function.
It‚Äôs trivial to define a function that reads the value of the balance slot.
(defun balance (account)
  (slot-value account 'balance))
However, if you know you‚Äôre going to define subclasses of bank-account, it might be a good
idea to define balance as a generic function. That way, you can provide different methods on
balance for those subclasses or extend its definition with auxiliary methods. So you might write
this instead:|#
;;(defgeneric balance (account)) ...)

#|(defmethod balance ((account bank-(defmethod balance ((account bank-account))
  (slot-value account 'balance))|#

#|A SETF function is a way to extend SETF, defining a new kind of place that it knows how to
set. The name of a SETF function is a two-item list whose first element is the symbol setf and
whose second element is a symbol, typically the name of a function used to access the place the
SETF function will set. A SETF function can take any number of arguments, but the first argument  is always the value to be assigned to the place.9 You could, for instance, define a SETF function to set the customer-name slot in a bank-account like this:|#
                             (defun (setf customer-name) (name account)
                               (setf (slot-value account 'customer-name) name))
;;After evaluating that definition, an expression like the following one:
;;(setf (customer-name my-account) "Sally Sue")
#|will be compiled as a call to the SETF function you just defined with ‚ÄúSally Sue‚Äù as the first argument and the value of my-account as the second argument.
Of course, as with reader functions, you‚Äôll probably want your SETF function to be generic,
so you‚Äôd actually define it like this:|#
(defgeneric (setf customer-name) (value account))
(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))
;;And of course you‚Äôll also want to define a reader function for customer-name.
(defgeneric customer-name (account))
(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))
;;This allows you to write the following:
(setf (customer-name *account*) "Sally Sue") ;;= Sally Sue"
(customer-name *account*) ;;= Sally Sue"

#|Thus, instead of explicitly writing the balance generic function and method as shown previously, you could change the slot specifier for the BALANCE SLOT in the definition of bank-account to this:|#
#|(customer-name
 :initarg :customer-name
 :initform (error "Must supply a customer name.")
 :accessor customer-name)|#
;;FINAL DEF
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name.")
    :accessor customer-name
    :documentation "Customer's name")
   (balance
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, or :bronze.")))

;;HARDER WAY TO ACCESS
#|
(defmethod assess-low-balance-penalty ((account bank-account))
  (when (< (balance account) *minimum-balance*)
    (decf (slot-value account 'balance) (* (balance account) .01))))
And if you decide you want to directly access the slot value in order to avoid running auxiliary
methods, it gets even more cluttered.
(defmethod assess-low-balance-penalty ((account bank-account))
  (when (< (slot-value account 'balance) *minimum-balance*)
    (decf (slot-value account 'balance) (* (slot-value account 'balance) .01))))
|#
;;EASIER WAY (WITH-SLOTS  OR (WITH
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))

#|also use WITH-ACCESSORS. The form of WITH-ACCESSORS is the same as WITH-SLOTS except each element of the slot list is a two-item list containing a variable name and the name of an accessor function. Within the body of WITH-ACCESSORS, a reference to one of the variables is equivalent to a call to the corresponding accessor function. If the accessor function is SETFable, then so is the variable.|#
(defmethod assess-low-balance-penalty ((account bank-account))
  (with-accessors ((balance balance)) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))
#|The first balance is the name of the variable, and the second is the name of the accessor
function; they don‚Äôt have to be the same. You could, for instance, write a method to merge two
accounts using two calls to WITH-ACCESSORS, one for each account.|#
(defmethod merge-accounts ((account1 bank-account) (account2 bank-account))
  (with-accessors ((balance1 balance)) account1
    (with-accessors ((balance2 balance)) account2
      (incf balance1 balance2)
      (setf balance2 0))))
#|The choice of whether to use WITH-SLOTS versus WITH-ACCESSORS is the same as the
choice between SLOT-VALUE and an accessor function: low-level code that provides the basic
functionality of a class may use SLOT-VALUE or WITH-SLOTS to directly manipulate slots in
ways not supported by accessor functions or to explicitly avoid the effects of auxiliary methods
that may have been defined on the accessor functions. But you should generally| use with-accessors
|#

#|
xxx SOME DEFMACRO HELP ------------------------------

;;FOUND WAY TO USE MULTIPLE VALUES WITH DEFMACRO
(defmacro test4 (v1 &rest body)
  (setf out nil)
  (let
      ((x (first v1))
       (y)
       )
    (setf y `(quote ,body))
  ;;AFOUT DOES WORK HERE
    (afout 'out (format nil "TEST Y= ~A~% X= ~A" y X))
;;THIS VALUES WORKS USE BACKQUOTE WITH (QUOTE ,VAR) INSIDE
   `(values (quote ,X)  (quote ,y)  99)
   ))
;;works 
;;  (test4 (a b) (x y z)  b c d) = A (QUOTE ((X Y Z) B C D))  99

(defmacro test5 (v1 &rest body)
  (let
      ((x (first v1))
       (y)
       )
    (setf y `(quote ,body))
  ;;AFOUT DOES WORK HERE
    (afout 'out (format nil "TEST= ~A~%" y))
;;values DOESN'T SEEM TO WORK AT ALL -- ONLY RETURNS FIRST ITEM
   (values `(quote ,x) `(quote ,y)  99)))
;; FOLLOWING WORKS, BUT NOT RETURN REST OF VALUES
;; (test5 (a b) (x y z)  b c d)  =  A


(defmacro test7 (v1 &rest body)
  (let
      ((x (first v1))
       (y)
       )
    (setf y `(quote ,body))))
;;works
(test7 '(a b) a b c d)
;;works
(test7 '(a b) (x y z)  b c d)  ;;= ((X Y Z) B C D)
|#
#|
SOME USEFUL CLOS FUNCTIONS

FIND-CLASS
Function: #<Function FIND-CLASS 20B190D2>
Lambda List: (CLASS-NAME &OPTIONAL (ERRORP T) ENVIRONMENT)
RETURNS ACTUAL CLASS from CLASS-NAME--like (eval inst-sym = actual instance)

FIND-PROPER-CLASS
Function
LambdaL: (CLOS::NAME &REST CLOS::FIND-CLASS-ARGS)


 (DEFGENERIC  CLASS-DIRECT-SUBCLASSES):
#<The HARLEQUIN-COMMON-LISP package, 5/8 internal, 271/512 external>
Arguments: (INSTANCE)
;;TEST
(CLASS-DIRECT-SUBCLASSES (FIND-CLASS 'SCALE))
;;WORKS, SAMPLE OUTPUT
(#<STANDARD-CLASS SRPEOPLE 21F02F13>
 #<STANDARD-CLASS SRELHLTH 21C0E88B>
 #<STANDARD-CLASS SRANGAGG 21BD7B6F>
 #<STANDARD-CLASS SRANXIET 21F02AE3>
 #<STANDARD-CLASS SRDEPRES 21C0D9BF>
 #<STANDARD-CLASS SEHAPPY 21C5D4DB>
 #<STANDARD-CLASS SINWRITE 21F020D3>
 #<STANDARD-CLASS SINWOETH 21F0315B>, ETC, LISTS ALL OF THEM
TEST (setf  *acad-subclasses (CLASS-DIRECT-SUBCLASSES (FIND-CLASS 'ACAD-LEARNING)))
;;CCC



SOME  SUBCLASS FUNCTIONS:
xxx HARLEQUIN-COMMON-LISP CLASS-DIRECT-SUBCLASSES
CLOS ADD-CLASS-FINALIZING-HOOK-FOR-SUBCLASSES
xxx CLOS ADD-DIRECT-SUBCLASS
CLOS GET-CLASS-POTENTIAL-INITARGS-FOR-SUBCLASS
CLOS KEEP-STREAM-SUBCLASSES
CLOS REFINALIZE-SUBCLASSES
xxx CLOS REMOVE-DIRECT-SUBCLASS
CLOS SKIP-SUBCLASSES-IN-MAP-ALL-CLASSES
xxx CLOS UPDATE-CLASS-AND-SUBCLASSES

SOME SUPERCLASS FUNCTIONS
xxx clos CLASS-ALL-SUPERCLASSES:
xxx harlequin-common-lisp CLASS-DIRECT-SUPERCLASSES
HARLEQUIN-COMMON-LISP VALIDATE-SUPERCLASS
CLOS COERCE-CLASS-DIRECT-SUPERCLASSES
CLOS FIND-CLASS-SLOT-DEFINED-BY-SUPERCLASSES
CLOS METACLASS-DEFAULT-DIRECT-SUPERCLASSES
CLOS UPDATE-CLASS-DIRECT-SUPERCLASSES

TEST 
(CLOS::CLASS-ALL-SUPERCLASSES  (FIND-CLASS 'SCALE))
;;WORKS, RETURNS
(#<BUILT-IN-CLASS T 202E5F83> #<STANDARD-CLASS STANDARD-OBJECT 201256D3> #<STANDARD-CLASS TOP-CLASS 22043667> #<STANDARD-CLASS PERSIM 2205C593> #<STANDARD-CLASS ASSESSMENT 2205EB77> #<STANDARD-CLASS SCALE 220261E3>)

Name: CLOS::CLASS-INSTANCE-SLOTS
Name: CLOS::GET-CLASS-EFFECTIVE-SLOTS
(these 2 functions seem to have same output--see test)
Home Package: CLOS [Internal]
Function: #<Function CLOS::GET-CLASS-EFFECTIVE-SLOTS 209A5B6A>
Lambda List: (CLASS)
;;TEST
 (CLOS::GET-CLASS-EFFECTIVE-SLOTS (find-class 'scale))
;;works, returns a list of all-slots in sample form below:
(#<STANDARD-EFFECTIVE-SLOT-DEFINITION NAME 21F20C27>
 #<STANDARD-EFFECTIVE-SLOT-DEFINITION NAME-STRING 21F20C8F>
 #<STANDARD-EFFECTIVE-SLOT-DEFINITION LABEL 21F20CD3>
 #<STANDARD-EFFECTIVE-SLOT-DEFINITION DESCRIPTION 21F20D17> etc )
(STRING  #<STANDARD-EFFECTIVE-SLOT-DEFINITION NAME-STRING 21F20C8F>)

UPDATE-INSTANCE-FOR-DIFFERENT-CLASS 
UPDATE-INSTANCE-FOR-REDEFINED-CLASS


CLOS::CLASS-PLIST
;;TEST
(CLOS::CLASS-PLIST (find-class 'scale))
;;WORKS = (CLOS::COPYABLE-INSTANCE #<SCALE 21F2040F>)

;;SSS 
xxx SOME OBJECTS FUNCTIONS
(DEFGENERIC DESCRIBE-OBJECT):
Arguments: (OBJECT STREAM)
CL
TEST
;;XXX THIS WORKS FOR CHANGING OBJECT INTO A STRING
;;TEST (PRINT-OBJECT *SCALE-INST T)
;;WORKS=    #<SCALE 21BE283F>
 (stringp (PRINT-OBJECT *SCALE-INST NIL)) = #<SCALE 21BE283F>

TEST  (subseq (format nil "~A" (PRINT-OBJECT *SCALE-INST NIL)) 5 9)
WORKS, = #<SCALE 21BE283F>  "LE 2"

XXX
;;PARTIALL SOLVED ERROR PROBLEM WITH function MY-OBJECT-TO-STRING
All of following get same ERROR= Error while reading: Subcharacter #\< not defined for dispatch char #\#.  (Adding quotes didn't help)
(PRINT-OBJECT #<STANDARD-CLASS SRPEOPLE 21F02F13>) 
(DESCRIBE-OBJECT #<STANDARD-CLASS SRPEOPLE 21F02F13>) = ERROR
(class-name #<STANDARD-CLASS SRPEOPLE 21F02F13>) 
PRINT-UNREADABLE-OBJECT
(PRINT-UNREADABLE-OBJECT #<STANDARD-CLASS SRPEOPLE 21F02F13> T)
(format nil "~A" (quote #<STANDARD-CLASS SRPEOPLE 21F02F13>))
(PRINT-NOT-READABLE-OBJECT  #<STANDARD-CLASS SRPEOPLE 21F02F13>)
(CLOS::OBSOLETE-OBJECT-REPRESENTATION-P #<STANDARD-CLASS SRPEOPLE 21F02F13>) 
 (print-object  (car *acad-subclasses) T) = #<STANDARD-CLASS SSL14MATHAPT 21C06BF7>
(format nil "Object= ~A" (print-object  (car *acad-subclasses) T))
(FUNCALL #<STANDARD-CLASS SRPEOPLE 21F02F13>)


(CLASS-SLOTS (FIND-CLASS 'SCALE))
;;WORKS, return sample:
(#<STANDARD-EFFECTIVE-SLOT-DEFINITION NAME 21F20C27> 
#<STANDARD-EFFECTIVE-SLOT-DEFINITION NAME-STRING 21F20C8F> 
#<STANDARD-EFFECTIVE-SLOT-DEFINITION LABEL 21F20CD3>  ETC.

VALIDATE-SUPERCLASS
TEST:
(VALIDATE-SUPERCLASS (find-class 'scale) (find-class 'assessment)) works = T



;; FUNCTIONS WITH "SLOT" IN THEM (FROM SYMBOL BROWSER) ------------------

XXX ME COMMON-LISP-USER APPEND-SLOT-LIST
XXX ME COMMON-LISP-USER APPEND-SLOT-LISTS
HARLEQUIN-COMMON-LISP CLASS-DIRECT-SLOTS
HARLEQUIN-COMMON-LISP CLASS-EFFECTIVE-SLOTS
HARLEQUIN-COMMON-LISP CLASS-SLOTS
COMMON-LISP-USER GET-ALL-CLASS-INSTANCES-SLOT-VALUE
XXX ME COMMON-LISP-USER GET-SLOT-VALUE
COMMON-LISP MAKE-LOAD-FORM-SAVING-SLOTS
HARLEQUIN-COMMON-LISP METHOD-SLOT-NAME
HARLEQUIN-COMMON-LISP OPTIMIZE-SLOT-VALUE
XXX ME COMMON-LISP-USER SET-SLOT-VALUES
XXX COMMON-LISP SLOT-BOUNDP
HARLEQUIN-COMMON-LISP SLOT-DEFINITION-ALLOCATION
HARLEQUIN-COMMON-LISP SLOT-DEFINITION-INITARGS
HARLEQUIN-COMMON-LISP SLOT-DEFINITION-INITFORM
HARLEQUIN-COMMON-LISP SLOT-DEFINITION-INITFUNCTION
HARLEQUIN-COMMON-LISP SLOT-DEFINITION-LOCATION
HARLEQUIN-COMMON-LISP SLOT-DEFINITION-NAME
HARLEQUIN-COMMON-LISP SLOT-DEFINITION-READERS
HARLEQUIN-COMMON-LISP SLOT-DEFINITION-TYPE
HARLEQUIN-COMMON-LISP SLOT-DEFINITION-WRITERS
XXX COMMON-LISP SLOT-EXISTS-P
COMMON-LISP SLOT-MAKUNBOUND
COMMON-LISP SLOT-MISSING
COMMON-LISP SLOT-UNBOUND
XXX COMMON-LISP SLOT-VALUE
COMMON-LISP UNBOUND-SLOT-INSTANCE
XXX COMMON-LISP WITH-SLOTS
CLOS ACCESSOR-METHOD-SLOT-DEFINITION
;;XXX CLOS ADD-SLOT-NEW-ACCESSORS
CLOS ALLOCATE-FUNCALLABLE-INSTANCE-WRAPPER-SLOTS
CLOS ATOMIC-EXCHANGE-SLOT-VALUE
  CLOS ATOMIC-POP-SLOT-VALUE
  CLOS ATOMIC-PUSH-SLOT-VALUE
CLOS ATOMIC-SLOT-VALUE-FIXNUM-ADD
CLOS BIND-WRAPPER-AND-STATIC-SLOTS-FSC
CLOS BIND-WRAPPER-AND-STATIC-SLOTS-STD-CLASS
CLOS BINDING-SLOT-ACCESS
CLOS CALL-GET-SLOT-VALUE-CACHE-MISS
CLOS CALL-SET-SLOT-VALUE-CACHE-MISS
CLOS CALL-SLOT-UNBOUND-FUNCTION
CLOS CANONICALIZE-DEFCLASS-SLOT
CLOS CLASS-FIXED-LOCATION-ACCESSOR-FOR-SLOT
  CLOS CLASS-INSTANCE-SLOTS
CLOS CLASS-INSTANCE-SLOTS-NUMBER
CLOS CLASS-SLOT-INITFUNCTION-VALUE
CLOS CLASS-SLOTS-AUX
CLOS CLASS-SLOTS-PV-HIT
CLOS CLASS-SLOTS-PV-MISS
CLOS CLASS-SLOTS-PV-MISS-1
CLOS COMPARE-AND-SWAP-SLOT-VALUE
CLOS COMPUTE-CLASS-SLOT-CONSES
CLOS COMPUTE-EFFECTIVE-SLOT-DEFINITION
CLOS COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS
CLOS COMPUTE-SLOT-DEFINITION-FIXED-LOCATION
XX  CLOS COMPUTE-SLOTS
CLOS COMPUTE-WRAPPER-CLASS-SLOTS
CLOS COPY-DYNAMIC-SLOT-DUMMY
CLOS COPY-SLOT-MISSING-DUMMY
CLOS DELETE-WRAPPER-FROM-GLOBAL-SLOT-CACHE
CLOS DIRECT-SLOT-DEFINITION-CLASS
CLOS DYNAMIC-SLOT-BOUNDP-USING-SLOTD
CLOS DYNAMIC-SLOT-DUMMY-P
CLOS DYNAMIC-SLOT-VALUE-USING-SLOTD
CLOS EFFECTIVE-SLOT-DEFINITION-CLASS
CLOS ENSURE-AFTER-SLOT-STORE-FOR-RECHECKING
CLOS ENSURE-SLOT-VALUE-ATOMICALLY
CLOS FAST-CLASS-SLOT-ACCESS
CLOS FAST-METHOD-SLOT-NAME
CLOS FIND-CLASS-SLOT-DEFINED-BY-SUPERCLASSES
CLOS FIND-EFFECTIVE-SLOT-DEFINITION
CLOS FIND-EFFECTIVE-SLOT-DEFINITION-IN-SLOT-DEFS
CLOS FIND-SLOT-DEFINITION
CLOS FIND-SLOT-LOCATION
CLOS FIND-SLOT-NAME-IN-ISL
CLOS FIXED-LOCATION-OPTIMIZE-SET-SLOT-VALUE
CLOS FIXED-LOCATION-OPTIMIZE-SET-SLOT-VALUE-1
CLOS FIXED-LOCATION-OPTIMIZE-SLOT-VALUE
CLOS FIXED-LOCATION-OPTIMIZE-SLOT-VALUE-1
CLOS FIXED-LOCATION-OPTIMIZE-SLOT-VALUE-LOCATION
CLOS FUNCALLABLE-INSTANCE-STATIC-SLOTS
CLOS GENERATE-SLOT-INITFUNCTION-FROM-INITFORM
CLOS GET-CLASS-EFFECTIVE-SLOTS
CLOS GET-OR-SETUP-SLOT-VALUE-ATOMICALLY
CLOS GET-SLOT-SPEC-FOR-ACCESSOR
CLOS GET-SLOT-VALUE-1
CLOS GET-SLOT-VALUE-2
CLOS GET-SLOT-VALUE-CACHE-MISS-FSC
CLOS GET-SLOT-VALUE-CACHE-MISS-STD
CLOS I-ATOMIC-EXCHANGE-SLOT-VALUE
CLOS I-ATOMIC-POP-SLOT-VALUE
CLOS I-ATOMIC-PUSH-SLOT-VALUE
CLOS I-ATOMIC-SLOT-VALUE-FIXNUM-ADD
CLOS I-LOCK-INSTANCE-SLOT
CLOS INSTANCE-SLOT-ACCESS
CLOS INSTANCE-SLOT-POSITION
CLOS INTERNAL-SLOT-DEFINITION-INITFUNCTION
CLOS LOCK-INSTANCE-SLOT
CLOS MAKE-DYNAMIC-SLOT-DUMMY
CLOS MAKE-SLOT-MISSING-DUMMY
CLOS MAKE-SLOTS-LISTS
CLOS MAXIMIZE-BITS-FROM-WRAPPER-NO-FOR-SLOT-CACHE
CLOS MAYBE-WRAPPER-DYNAMIC-SLOTS
CLOS MLF-BOUND-SLOTS
CLOS MLF-SET-INSTANCE-SLOTS
CLOS OPTIMIZE-SET-SLOT-VALUE
CLOS PRINT-SLOT-MISSING-DUMMY
CLOS PROCESS-A-SLOT-OPTION
CLOS READER-UNBOUND-INSTANCE-SLOT-ERROR
CLOS RECHECK-STATIC-SLOTS
CLOS RECOVER-DYNAMIC-SLOTS
CLOS REPEATED-DEFCLASS-SLOT-OPTION-ERROR
CLOS RESET-SLOT-VALUE-CACHE
CLOS SELECT-CLASS-SLOTS
CLOS SET-FUNCALLABLE-INSTANCE-STATIC-SLOTS
XXX CLOS SET-SLOT-VALUE
CLOS SET-SLOT-VALUE-1
CLOS SET-SLOT-VALUE-2
CLOS SET-SLOT-VALUE-CACHE-MISS-FSC
CLOS SET-SLOT-VALUE-CACHE-MISS-STD
CLOS SET-SLOT-VALUE-MISSING
CLOS SLOT-ACCESSORS-USING-CLASS
CLOS SLOT-BOUNDP-FSC
CLOS SLOT-BOUNDP-MISSING
CLOS SLOT-BOUNDP-USING-CLASS
CLOS SLOT-DEFINITION-ELIDE-ACCESS-METHOD-P
CLOS SLOT-DEFINITION-FIXED-LOCATION-P
CLOS SLOT-DEFINITION-FLAGS
CLOS SLOT-EXISTS-P-USING-CLASS
CLOS SLOT-FIND-PV-VECTOR
CLOS SLOT-FLAGS-ALLOCATION
CLOS SLOT-MAKUNBOUND-USING-CLASS
CLOS SLOT-MISSING-DUMMY-P
CLOS SLOT-MISSING-DUMMY-SLOT
CLOS SLOT-UNBOUND-VALUE
CLOS SLOT-VALUE-CACHE-ACCESS
CLOS SLOT-VALUE-CACHE-RAW-OFFSET
CLOS SLOT-VALUE-IF-BOUND
CLOS SLOT-VALUE-MISSING
CLOS SLOT-VALUE-USING-CLASS
CLOS SLOW-ATOMIC-EXCHANGE-SLOT-VALUE
CLOS SLOW-ATOMIC-POP-SLOT-VALUE
CLOS SLOW-ATOMIC-PUSH-SLOT-VALUE
CLOS SLOW-ATOMIC-SLOT-VALUE-FIXNUM-ADD
CLOS SLOW-LOCK-INSTANCE-SLOT
CLOS STANDARD-INSTANCE-STATIC-SLOTS
CLOS STRUCTURE-INSTANCE-SLOT-POSITION
CLOS UNLOCK-INSTANCE-SLOT
CLOS WITH-FUNCALLABLE-INSTANCE-STATIC-SLOTS
CLOS WITH-INSTANCE-STATIC-SLOTS
CLOS WITH-SLOT-VALUE-CACHE-CHECK-FOR-MODIFICATION
CLOS WITH-STANDARD-INSTANCE-STATIC-SLOTS
CLOS WITH-STATIC-SLOTS-RECHECKING
CLOS WITH-STATIC-SLOTS-RECHECKING-WITH-TAG
CLOS WITH-STATIC-SLOTS-RECHECKING-X
;;END search for  SLOT --------------------------------------------------------------


Symbol FUNCTION search for CLASS ----------------------------------------------------
ONLY SELECTED RESULTS:
CHANGE-CLASS
CLASS-DEFAULT-INITARGS 
CLASS-DIRECT-DEFAULT-INITARGS 
CLASS-DIRECT-METHODS 
xxx CLASS-DIRECT-SLOTS
xxx CLASS-DIRECT-SUBCLASSES 
xxx CLASS-DIRECT-SUPERCLASSES
CLASS-EFFECTIVE-SLOTS 
CLASS-FINALIZED-P 
xxx CLASS-NAME 
CLASS-OF 
CLASS-PRECEDENCE-LIST 
CLASS-PROTOTYPE 
xxx CLASS-SLOTS  
xxx FIND-CLASS ;; use to find actual class from class symbol
HARLEQUIN-COMMON-LISP GENERIC-FUNCTION-METHOD-CLASS
COMMON-LISP UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
COMMON-LISP UPDATE-INSTANCE-FOR-REDEFINED-CLASS
HARLEQUIN-COMMON-LISP VALIDATE-SUPERCLASS
xxx CLOS::CHECK-IS-A-CLASS 
xxx CLASS-SLOTS  
xxx CLOS::CLASS-ALL-SUPERCLASSES
CLOS::CLASS-DIRECT-GENERIC-FUNCTIONS 
CLOS:CLASS-EXTRA-INITARGS 
xxx CLOS::CLASS-INSTANCE-SLOTS 
CLOS:CLASS-INSTANCE-SLOTS-NUMBER
CLOS::CLASS-LOCK 
xxx CLOS::CLASS-PLIST 
CLOS::CLASS-PROPERTY
CLOS::FIND-PROPER-CLASS 
CLOS::GET-CLASS-EFFECTIVE-SLOTS 
CLOS:MAP-CLASSES lamdaL: function
CLOS:REMOVE-DIRECT-SUBCLASS
(CLOS::SELECT-CLASS-SLOTS (CLASS CLOS::ALLOCATION-TYPE))
(CLOS::SET-CLASS-PROPERTY (CLASS CLOS::PROPERTY CLOS::VALUE))
(CLOS::SLOT-ACCESSORS-USING-CLASS CLOS:SLOT-BOUNDP-USING-CLASS CLOS:SLOT-EXISTS-P-USING-CLASS CLOS:SLOT-MAKUNBOUND-USING-CLASS CLOS:SLOT-VALUE-USING-CLASS)
(CLOS::UPDATE-CLASS-AND-SUBCLASSES CLOS::UPDATE-CLASS-DIRECT-SUPERCLASSES CLOS::VALIDATE-CLASS-CHANGE CLOS:VALIDATE-METACLASS-CHANGE CLOS:WRITER-METHOD-CLASS)



;;ORIGINAL VERSION -- DIDN'T WRITE TO A FILE OPTION
;; PROBLEM WITH MAKE-INSTANCE IN MACROEXPANSION
;;Hi Tom,
#|
Sorry for the delay -- I was on vacation.  Please use the address lisp-support@lispworks.com for support issues so they are seen by the whole team.

The error occurs because an instance of a CLOS class appears in the macroexpansion of my-defclass (the variable class-inst).  This will work when you load the uncompiled source or when you use the Editor's Compile Definition or Compile Buffer commands, but it won't work when you compile the file.

What are you trying to achieve with that?

Another major problem with my-defclass is that it uses eval to define the class at macroexpansion time.  This should be done at load time, i.e. the (defclass ...) form must be in the expansion of my-defclass so that it is seen by the compiler.

Regards,

Martin Simmons
LispWorks Technical Support
http://www.lispworks.com/support/
|#
#|
(defmacro  my-defclass (class-sym superclasses slot-spec-list &rest defclass-option-list)
  "In u-clos,lisp, like defclass, except adds slots, parents, children. makes a class instance called *classname-inst and FILLS IN parents slot with superclass; also it auto defines accessors for all slots in slot-list. default-args is used to set the class instance slot-values. it makes a slot by creating a slot-spec that writes in (:initarg :name  :accessor name ) and appends any added slot-keys such as   :initform initial top-class name :documentation name. :instance-initargs is args for the *class-inst. These CLASS-OPTIONS should be standard format for initiating instances. "

  (let*
      (;;(class-sym (car class-definition-list))
       (class)
       (defclass-option-list-n (list-length defclass-option-list))
     ;;  (superclasses  (second class-definition-list))
    ;;   (slot-spec-list (first class-definition-list))
      ;; (slot-name)
     ;;  (slot-list)
    ;;   (defclass-option-list)
       ;;     (class-options)
       (default-initargs)
    ;;   (new-default-initargs)
       (documentation)
       (metaclass)
       (class-inst-sym)
       (class-inst)
       (defclass-call)
       (instance-initargs)
       (parents-list  `(:parents (quote ,superclasses))) ;; `(:parents ,superclasses-syms))
       (parent-class-inst-syms)
       ;;following 2 may not be needed but are returned in multiple-value-setq below
       (parent-class-inst-objects)
      (parent-class-inst-strings)
       ;;  (superclasse-objects) 
       )

  ;;   (afout 'out (format nil "defclass-option-list= ~A~%" defclass-option-list))
 (if (> defclass-option-list-n 0) 
    (dolist (element defclass-option-list)
      (if  (listp element)
          (cond
           ((equal (car element) :default-initargs)   
            (setf default-initargs element))
           ((equal (car element) :documentation)
            (setf documentation element))
           ((equal (car element) :metaclass)
            (setf metaclass element)) 
           ((equal (car element) :instance-initargs)
            (setf instance-initargs element)))
        ;;end if, dolist
        )))
    (cond
     (default-initargs   
      ;;add parents to :default-initargs in the default-initargs list
      ;;sss add 
      (setf  defclass-option-list
             (list (append default-initargs parents-list))))
     (t     (setf  defclass-option-list (list (append (list :default-initargs) parents-list)))))
    
    (if documentation
        (setf defclass-option-list (append defclass-option-list (list documentation))))
    (if metaclass
        (setf defclass-option-list  (append defclass-option-list (list metaclass))))
     
    ;;(afout 'out (format nil "defclass-option-list= ~A~%" defclass-option-list))

    ;;add the accessor parts to each slot spec item
    (setf slot-spec-list (add-accessors slot-spec-list))

    ;;(afout 'out (format nil "2-superclasses= ~A~%" superclasses))

    ;;PUT THIS 'CHILD CLASS INTO 'CHILDREN SLOTS OF EACH OF SUPERCLASS INSTS
    ;;find parent-class-instance-list = the superclass class instances
    ;;yyy
    (multiple-value-setq (parent-class-inst-syms parent-class-inst-objects parent-class-inst-strings)
        (get-class-insts superclasses))
    ;;add to the children slot of superclasses
    (append-slot-lists parent-class-inst-syms 'children class-sym) 
 
    ;;MAKE THE NEW CLASS WITH DEFCLASS
    (setf defclass-call 
          (append  `(defclass ,class-sym ,superclasses ,slot-spec-list) defclass-option-list))
    (setf class (eval defclass-call))
   
    ;;setf *[CLASSNAME]-INST to a new class instance with init-args and adds its sym to 
    ;;  a list in the class instance class-instances list
  ;;  (afout 'out 
 ;;  (setf *out (format t "class-sym= ~A~%instance-initargs= ~A~%" ,class-sym ,instance-initargs))
    (cond
     (instance-initargs
      (multiple-value-setq (class-inst class-inst-sym)
           (my-make-instance  class-sym :instance-initargs instance-initargs)))
     (t  
      (multiple-value-setq (class-inst class-inst-sym)
         (my-make-instance   class-sym))))
       ;;(afout 'out (format nil  "class= ~A~% class-sym= ~A~%  class-inst= ~A~% class-inst-sym= ~A~%  " class class-sym  class-inst class-inst-sym))

    `(values (quote ,class) (quote ,class-sym) (quote  ,class-inst) (quote ,class-inst-sym))
    ;;end let, my-defclass
    ))|#


#|(defun my-make-instance (class-sym  make-def-stream &rest initargs &key instance-sym class-instances-slot ) ;; init-args instance-sym class-instances-slot)
  "In U-clos.lisp, sets instance-sym to make-instance, and adds to class-instances slot-value. Default instance-sym= *class-sym-inst. If make-def-stream, sends def to that stream instead of evaling it."
  (let
      ((make-instance-call)
       ;;(new-initargs)  ;;use with below later??       
     ;;  (class-instances-slot) ;;fix later, makes default only  option
      ;;  (instance-sym)  ;;fix later, makes default only  option
       )
    ;;make default instance-sym
    (unless instance-sym
      (setf instance-sym 
            (my-make-symbol (format nil "*~A-inst" class-sym))))

    ;;set default class-instances-slot
    (unless class-instances-slot
      (setf class-instances-slot  'class-instances))

    (cond
     (initargs
      (setf  make-instance-call (append `(make-instance (quote ,class-sym)) initargs))
      )
     (t
      (setf  make-instance-call `(make-instance (quote ,class-sym)))))

    (cond
     ;;if make-def-stream, send def to that stream
     (make-def-stream
      (setf  write-form `(defparameter ,instance-sym ,make-instance-call))
      (terpri make-def-stream)
      (write write-form :stream make-def-stream  :pretty t)
      (terpri make-def-stream)
     (values write-form instance-sym)
      )
    ;;if null make-def-stream
    ;;now set the instance-sym to new made instance
     ((null make-def-stream)
      (set instance-sym (eval make-instance-call))

      ;;(afout 'out (format nil "instance-sym= ~A~%" instance-sym))
      ;;APPEND-SLOT-LIST GOES HERE TO ADD INSTANCE TO LIST
      (append-slot-list (eval instance-sym) class-instances-slot instance-sym)
      (values (eval  instance-sym) instance-sym)
      ;;end null clause, rest
      ))))|#


;;END  Symbol FUNCTION search for CLASS ----------------------------------------------------
|#




;;END HELP SECTION ---------------------------------------------------------------------
|#

