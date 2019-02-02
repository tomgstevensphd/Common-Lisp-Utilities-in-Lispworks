;;********************* U-capi-multi-column-list-panels.lisp ***********

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/multi-column-list-panels.lisp
;;
;; This example demonstrates the uses of multi-column list-panels in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-MULTI-COLUMN-LIST-PANELS)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


  
;;----------------------------------------------------------------------------
;; The data
;;----------------------------------------------------------------------------

;;; Each item in the MULTI-COLUMN-LIST-PANELs is an object
;;; of this type.

(defstruct multi-column-test-item
  name
  priority
  status)

(defvar *multi-column-list-panel-test-items*
  (loop for (name priority status) in 
        '(("bug fixing" 50  "ongoing")
          ("release 1.0" 1000000  "waiting for QA")
          ("release 2.0" 50  "waiting for design")
          ("design" -1  "ongoing")
          ("tea making" 100000000  "ongoing")
          )
        collect (make-multi-column-test-item :name  name 
                                             :priority priority 
                                             :status status)))

;;;; -----------------------------------------------------------------
;;; Images and image functions 
;;; ------------------------------------------------------------------

;;; Pick up the image that is used by the TREE-VIEW example and
;;; create a capi:IMAGE-SET from it. 

(defvar *multi-column-list-panel-image-set*
  (capi:make-general-image-set :id #.(gp:read-external-image
                                      (current-pathname"tree.bmp"))
                               :image-count 4
                               :width 16
                               :height 16))

;;; Create capi:IMAGE-LOCATORs to show sub-images

(defvar *multi-column-list-panel-image-locators*
  (apply 'vector 
         (loop for  x below 4 collect 
               (capi:make-image-locator 
                :image-set *multi-column-list-panel-image-set*
                :index x))))

;;; The image-function just return some capi:IMAGE-LOCATOR based on the priority
(defun multi-column-image-function(item)
  (when-let (pr (multi-column-test-item-priority item))
    (svref *multi-column-list-panel-image-locators*
           (cond ((> pr 1000) 0)
                 ((> pr 0) 1)
                 (t 2)))))

;;;  The state-image-function returns an capi:IMAGE-LOCATOR only if it "ongoing"
(defun multi-column-state-image-function (item)
  (if (equal "ongoing" (multi-column-test-item-status item))
      (svref *multi-column-list-panel-image-locators* 3)))

;;;; ------------------------------------------------------------------
;;; Definine sorting-description
;;; ------------------------------------------------------------------

;;; *multi-column-test-sorting-types* defines the sorting descriptions
;;; that are passed to the multi-column-list-panel by the keywords
;;; :SORT-DESCRIPTIONS. These are searched by capi:SORTED-OBJECT-SORT-BY
;;; to find the description that matches its NEW-SORT-TYPE argument. 
;;;  Because we are passing :SELECTION-CALLBACK :SORT in the header-args,
;;; clicking on the header invokes capi:SORTED-OBJECT-SORT-BY
;;; with the title of the column (defined by *multi-column-list-panel-test-columns* 
;;; below) as the NEW-SORT-TYPE. This finds the sorting description in this 
;;; list with the matching type, and  sorts accordingly. 

(defvar *multi-column-test-sorting-types*
  (list
   (capi:make-sorting-description :type "name"
                                  :key 'multi-column-test-item-name
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   (capi:make-sorting-description :type "priority"
                                  :key 'multi-column-test-item-priority
                                  :sort '> 
                                  :reverse-sort '<)
   (capi:make-sorting-description :type "status"
                                  :key 'multi-column-test-item-status
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   ))


;;; The columns in the MULTI-COLUMN-LIST-PANELs. Each item in this
;;; list defines a column. 
(defvar *multi-column-list-panel-test-columns*
  '((:title "Name")
    (:title "Priority" :adjust :right :gap 15)
    (:title "status")))

;;; The "column" function, takes an items and return a list of
;;; objects to display in each column. 

(defun multi-column-list-panel-test-column-items (item)
  (list (multi-column-test-item-name item)
        (multi-column-test-item-priority item)
        (multi-column-test-item-status item)))


(capi:define-interface multi-column-list-panel-test ()
  ((sortable-lp-reverse :initform nil))
  (:panes
   (fixed-lp
    capi:multi-column-list-panel
    :title "Not sortable:"
    :interaction :single-selection
    :column-function 'multi-column-list-panel-test-column-items
    :columns *multi-column-list-panel-test-columns*
    :image-function 'multi-column-image-function
    :keep-selection-p t
    :items *multi-column-list-panel-test-items*)
   (sortable-lp
    capi:multi-column-list-panel
    :title "Sortable:"
    :interaction :single-selection
    :column-function 'multi-column-list-panel-test-column-items
    :columns *multi-column-list-panel-test-columns*
    :image-function 'multi-column-image-function
     :state-image-function 'multi-column-state-image-function
     :use-state-images t

    :sort-descriptions *multi-column-test-sorting-types*
    :header-args (list 
                  :print-function 'string-capitalize
                  :selection-callback :sort ;;; "magic" callback tells it to use the sort descriptions
                  )
    :keep-selection-p t
    :items *multi-column-list-panel-test-items*))
  (:layouts
   (main-layout
    capi:row-layout
    '(fixed-lp sortable-lp)))
  (:default-initargs
   :title "Multi-column List Panel Test"
   :best-width 500
   :best-height 300))

;;----------------------------------------------------------------------------
;; A simple test function
;;----------------------------------------------------------------------------

(defun test-multi-column-list-panels ()
  (capi:display (make-instance 'multi-column-list-panel-test)))
;; (test-multi-column-list-panels)
