;;*************************** U-IDE-tools.lisp *******************************

;;from Lisp HUG, 
;;owner-lisp-hug@lispworks.com; on behalf of; DENIS BUDYAK [budden73@gmail.com]
#|Hi Mitch, List!   I wanted that for a long time too. Your question inspired me to play around with Works/Interface/Inspect menu item. This seem to work for me.|#
;;
;;NOTE: Only the (let ...) was original, I added the defun around it
;;USE FOLLOWING WHEN STARTING ALL LISP?? works great!!
;; EVALUATED BELOW
;;  (set-default-find-files-pattern "C:\\TOM\\LISP PROJECTS TS\\*" "*.l*sp")

(defun set-default-find-files-pattern (root-directory-pane-name &optional pattern-string)
      "Sets hard-coded values for root directory and patterns in search files tools initialize-instance. Also, LISPWORKS-TOOLS::*last-search-files-interface* might be useful"
  (let ((*handle-warn-on-redefinition* nil))

; non-toplevel
    (defmethod initialize-instance
               :around ((i LISPWORKS-TOOLS:search-files) 
                        &rest initargs &key &allow-other-keys)
      "Sets hard-coded values for root directory and patterns in search files tools initialize-instance. Also, LISPWORKS-TOOLS::*last-search-files-interface* might be useful"
      (call-next-method)
      (setf
       (capi:text-input-pane-text
        (slot-value i 'LISPWORKS-TOOLS::root-directory-pane))
       root-directory-pane-name)
      (unless pattern-string
        (setf
         (capi:text-input-pane-text
          (slot-value i 'lispworks-tools::patterns-pane))
         "sw/*.l*sp;sw/fb2/*.l*sp;sw/fb2/sp/*.l*sp;sw/fb2/admin/*.l*sp;sw/fb2/app/*.l*sp")
        (setf
         (capi:text-input-pane-text
          (slot-value i 'lispworks-tools::patterns-pane)) pattern-string))
      )
    );let
  )


;;EVALUATED HERE
 (set-default-find-files-pattern "C:\\TOM\\LISP PROJECTS TS\\*" "*.l*sp")

;;ORIGINAL VERSION
#|
(let ((*handle-warn-on-redefinition* nil))

; non-toplevel
(defmethod initialize-instance :around ((i
LISPWORKS-TOOLS:search-files) &rest initargs &key &allow-other-keys)
  "Sets hard-coded values for root directory and patterns in search files tools initialize-instance. Also,
LISPWORKS-TOOLS::*last-search-files-interface* might be useful"
  (call-next-method)
  (setf
   (capi:text-input-pane-text
    (slot-value i 'LISPWORKS-TOOLS::root-directory-pane))
   "c:/lisp")
  (setf
   (capi:text-input-pane-text
    (slot-value i 'lispworks-tools::patterns-pane))
   "sw/*.l*sp;sw/fb2/*.l*sp;sw/fb2/sp/*.l*sp;sw/fb2/admin/*.l*sp;sw/fb2/app/*.l*sp")
  )

);let
|#