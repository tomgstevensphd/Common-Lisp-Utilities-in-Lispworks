;;**************************** U-clos.lisp **********************************
;;
;;
;;

;;put in all key files
  (my-config-editor-after-start)

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


;;SSS ADD AUTO CREATION OF PARENTS AND CHILDREN PUT INTO SLOTS
;;    AS CLASSES ARE CREATED.

;;MY-DEFCLASS
;;
;;ddd
(defun my-defclass (class-sym superclasses slot-spec-list 
                              &rest class-options )
  "in u-clos,lisp, like defclass, except adds slots, parents, children. makes a class instance called *classname-inst and fills in parents slot with superclass; also it auto defines accessors for all slots in slot-list. default-args is used to set the class instance slot-values. it makes a slot by creating a slot-spec that writes in (:initarg :name  :accessor name ) and appends any added slot-keys such as   :initform initial top-class name :documentation name. instance-initargs adds these CLASS-OPTIONS should be standard format for  "

  (let
      ((slot-name)
       (slot-list)
       ;;(defclass-slot-list)
       (default-initargs)
       (documentation)
       (metaclass)
       (defclass-option-list)
       (inst-sym)
       (inst-arglist)
       (defclass-call)
       (new-default-initargs)
       ;;SSS IS BUG HERE??? (ADDED (QUOTE ...
       (parents-list  `(:parents (quote,superclasses))) ;; `(:parents ,superclasses-syms))
       (parent-class-inst-syms)
       ;;following 2 may not be needed but are returned in multiple-value-setq below
       (parent-class-inst-objects)
       (parent-class-inst-strings)
       ;;  (superclasse-objects) 
       )
    (dolist (element class-options)
      (if  (listp element)
      (cond
       ((equal (car element) :default-arglist)
        (setf default-arglist element))
       ((equal (car element) :documentation)
        (setf documentation element))
       ((equal (car element) :metaclass)
        (setf metaclass element))  
       (t (setf instance-initargs element)))
        ;;end if, dolist
        ))
    (cond
     (default-initargs   
    ;;add parents to :default-initargs in the default-initargs list
    ;;sss add 
    (setf  new-class-options
           (append default-initargs parents-list)))
     (t     (setf  defclass-option-list (list (append (list :default-initargs) parents-list)))))
    
    (if documentation
        (setf defclass-option-list (append defclass-option-list (list documentation))))
    (if metaclass
        (setf defclass-option-list  (append defclass-option-list (list metaclass))))
     
    (afout 'out (format nil "defclass-option-list= ~A~%" defclass-option-list))

    ;;add the accessor parts to each slot spec item
    (setf slot-spec-list (add-accessors slot-spec-list))

    ;;PUT THIS 'CHILD CLASS INTO 'CHILDREN SLOTS OF EACH OF SUPERCLASS INSTS
    ;;find parent-class-instance-list = the superclass class instances
    (multiple-value-setq (parent-class-inst-syms parent-class-inst-objects parent-class-inst-strings)
        (get-class-insts superclasses))
    ;;add to the children slot of superclasses
    (append-slot-lists parent-class-inst-syms 'children class-sym) 
 
    ;;MAKE THE NEW CLASS WITH DEFCLASS
    (setf defclass-call 
        (append  `(defclass ,class-sym ,superclasses ,slot-spec-list) defclass-option-list))
    (eval defclass-call)
   
    ;;setf *CLASSNAME-INST to a new class instance with init-args and adds its sym to 
    ;;  a list in the class instance class-instances list
    (cond
     (instance-initargs
      (my-make-instance class-sym :init-args instance-initargs))
     (t   (my-make-instance class-sym)))

    ;;end let, defun
    ))
;; (setf cx-inst (make-instance 'class1 :object-docs "this one"  :parents '(this-one that-one)))
;;test
;;SSS RETEST  MY-DEFCLASS WITH NEW ADDING CLASS TO PARENTS 'CHILDREN SLOTS 
;;
(defun testdc ()
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
    ))
;;appears to work well, returns (*NEW-CLASS2-INST)  101
;;ADDS PARENTS TO SELF PARENTS SLOT
;;  (slot-value *NEW-CLASS2-INST 'parents) = (top-class)
;; ADDS SELF  TO PARENTS' CHILDREN SLOT
;; (slot-value *class1-inst 'children) = (NEW-CLASS2)





;;ADD-ACCESSORS
;;
;;ddd
(defun add-accessors (defclass-slot-spec-list)
  (let
      ((new-defclass-slot-spec-list)
       )
    (loop
     for slot-list in defclass-slot-spec-list
     with slot-name
     with slot-name-string
     with rest-slot-list
     with accessor-list-part
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
;;Test
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
;;ddd  (my-make-instance class-sym :init-args instance-initargs)
(defun my-make-instance (class-sym &key init-args instance-sym class-instances-slot)
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
    ))
;;test
#|(defun testmm ()
  (let
      ((name-slot)
       (value2)
       )
    (my-make-instance  'class1 :init-args '(:name "test-name"))
    (setf name-slot  (slot-value *test-inst 'name))
    (setf value2 (slot-value *test-inst 'class-instances))
    (values name-slot value2)
    ))|#
;;works, returns:
#|"test-name"
(*TEST-INST)|#



;;APPEND-SLOT-LISTS
;;
;;
(defun append-slot-lists (instance-list slot new-value &key match-instance-to-value)
  "U-clos.lisp, appends slot LISTS in all instances with new-value (can be a list).  If match-instance-to-value = T, appends items in order of items in new-value  list.  If new-value list is shorter than num of instances, puts first new value in rest of  instances."
  (afout 'out (format nil "1 instance-list= ~A new-value= ~A ~%"  instance-list new-value))
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
       (afout 'out (format nil "2 instance= ~A new-value= ~A ~%"  instance new-value))
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
(defun testas2 ()
  (setf out nil)
  (let
      ((instance-list '(*class1-inst1 *class1-inst2 *class1-inst3))
       (slot 'children)
       (new-value  '(nv1 nv2 nv3)) ;; 'test-new-value)
       )
    (append-slot-lists instance-list slot new-value)
     (slot-value *class1-inst1 'children)
    ))
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
(defun testup ()
  (setf out nil)
    (setf (slot-value  *class1-inst1  'class-instances) '(*class1-inst1 *class1-inst2 *class1-inst2))
  (let
      ((x)
       )
    (append-slot-list *class1-inst1 'class-instances '*new-inst)
   ;;(append-slot-list *class1-inst1 'class-instances '(*new-inst3 *new-inst4))
    ))
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



      
;;XXX------------------- FUNCTIONS TO ACCESS CLASSES AND INSTANCES  ----------------



;;FIND-CLASSNAME
;;
;;ddd
(defun find-classname (class-object &key is-class-p)
  "In U-clos.lisp, finds the classname string in a class INSTANCE/OBJECT (eg. CLASS1 in #<CLASS1 22B2538B>.  If  is-class-p, finds class in  #<STANDARD-CLASS CLASS1 21E8A5C3>.  Returns (values class-sym string-string)."
  (let
      ((class-string)
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
    (if  (symbolp (quote class-inst-sym))
      (setf  class-inst-object (eval class-inst-sym)))
    (values class-inst-sym class-inst-object class-inst-string   class-string)
    ))
;;test
;;  (get-class-inst 'class1)
;;works, returns: *CLASS1-INST   #<CLASS1 22FAC627> "*CLASS1-inst"  "CLASS1"
;;also           
;;(my-make-symbol "*CLASS1-inst")



;;GET-CLASS-INSTS
;;ddd
(defun get-class-insts (class-list)
  (setf out nil)
  (let
      ((class-inst)
       (class-inst-syms)
       (class-inst-objects)
       (class-inst-strings)
       )
    (dolist (class class-list)
      (multiple-value-setq (class-inst-sym class-inst-object class-inst-string class-string)
           (get-class-inst  class))
      (afout 'out (format nil "~A ~A ~A~% " class-inst-sym class-inst-object class-inst-string))
      (setf  class-inst-syms (append class-inst-syms (list class-inst-sym))
             class-inst-objects (append class-inst-objects (list class-inst-object))
             class-inst-strings (append class-inst-strings (list class-inst-string)))
      )
    (values class-inst-syms class-inst-objects class-inst-strings)
    ))
;;test
;;(get-class-insts '(class1 class2))
;;works, returns: 
;;(*CLASS1-INST *CLASS2-INST) (#<CLASS1 226E77EF> #<CLASS1 226DF2E7>) ("*CLASS1-inst" "*CLASS2-inst")



;;SSS FINISH MAKING NEW METHODS BY WRAPPING AROUND SOME BELOW DEFUNS?
(defgeneric get-instances-method (class &optional slot-name)
  (:documentation "returns a list of all instances in the class. default slot-name is class-instances"))


;;GET-CLASS-INSTANCES
;;
;;ddd
(defun get-class-instances (class-instance &key class-instances-slot)
  "in u-clos.lisp, default class instance slot is class-instances. returns (values all-instance-syms class-instances-slot)."
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
       )
    (afout 'out (format nil "class-instance-syms= ~a~%" class-instance-syms))
    (loop
     for instance in class-instance-syms
     with object
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
(defun get-parent-syms (class &key nth)
  "In U-clos.lisp, from class or class-inst,  returns (values list of parents and if nth,, nth parent)"
  (let
      ((parents)
       (nth-parent)
       )
   ;; (unless (typep class
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

   (afout 'out (format nil "class-generic-inst-object ~a~%" class-generic-inst-object))

   (unless class-instances-slot
     (setf class-instances-slot 'class-instances))

   (setf all-instance-syms (slot-value  class-generic-inst-object  class-instances-slot))
 
   (afout 'out (format nil "all-instance-syms ~a~%" all-instance-syms))
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
   (afout 'out (format nil "slot-value=~a  instance-sym= ~a instance-object" slot-value instance-sym  instance-object))
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




;;GET-SLOT-VALUE
;;
;;ddd
(defun get-slot-value (instance-sym slot-name)
  "in u-clos.lisp, differs from normal (slot-value ...)  in that it evals the instance-sym arg first"
  (let
      ((slot-value)
       (instance-object (eval instance-sym))
       )
   (setf slot-value (slot-value instance-object slot-name))

   (afout 'out (format nil "in  get-slot-value, slot-value= ~a~%"   slot-value))
   (values slot-value instance-sym instance-object)
   ))
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
;;END HELP SECTION ---------------------------------------------------------------------
|#