;;********************* U-file&buffers-frames.lisp *********************
;;
;;;; *** ALSO SEE: U-files&buffers.lisp


(defparameter  *mark-info-action-checkboxes '("  AUTO-ADD  " "  UPDATE-LIST  " "  REPLACE-ALL  "  "  APPEND-LIST  ") "For use in Text-Mark-Interface" )
(defparameter   *save-mark-info-settings-path "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\3-MARK-INFO-SETTINGS.lisp" "For get-mark-info settings")
(defparameter  *INFO-EXTRA-LINES-N 3 "For use in get-mark-info-callback")
(defparameter   *INFO-INCL-PREVIOUS-LINE-P T "For use in get-mark-info-callback")
(defparameter    *default-info-extra-lines-n 3 "Extra lines printed in Mark info")


(unless (boundp '*all-mark-info-items)
  (defparameter *all-mark-info-items NIL "Main list for all mark items"))

;;TEXT-MARK-INTERFACE
;;2020
;;ddd
(capi:define-interface Text-Mark-Interface ()
  ((markstr
    :initarg :markstr
    :accessor markstr
    :type  :string
    :initform ""
    :documentation  "Mark String")
   (mark-pos
    :initarg :mark-pos
    :accessor mark-pos
    :type  :integer
    :initform 0
    :documentation  "Mark file position")
   (selected-item-list
    :initarg :selected-item-list
    :accessor selected-item-list
    :type  NIL
    :initform NIL
    :documentation  "Selected item list")
   (modified-item-list
    :initarg :modified-item-list
    :accessor modified-item-list
    :type  NIL
    :initform NIL
    :documentation  "modified text list")
   (new-item-list
    :initarg :new-item-list
    :accessor new-item-list
    :type  nil
    :initform NIL
    :documentation  "New item list")
   (priority
    :initarg :priority
    :accessor priority
    :type  :string
    :initform ""
    :documentation  "My priority")
   (buffer-name
    :initarg :buffer-name
    :accessor buffer-name
    :type  :string
    :initform ""
    :documentation  "buffer-name")
   (buffer-obj
    :initarg :buffer-obj
    :accessor buffer-obj
    :initform NIL
    :documentation  "buffer-obj")
   (mark-notes
    :initarg :mark-notes
    :accessor mark-notes
    :type  :string
    :initform ""
    :documentation  "My notes")
   (mark-info
    :initarg :mark-info
    :accessor mark-info
    :type  :string
    :initform ""
    :documentation  "Lines from buffer.")   
   (list-edit-action
    :initarg :list-edit-action
    :accessor list-edit-action
    :type  NIL
    :initform NIL
    :documentation  "actions: :replace-all :append-list :replace-list :replace-elm" )
   (selected-item-n
    :initarg :selected-item-n
    :accessor selected-item-n
    :type  :integer
    :initform NIL
    :documentation  "selected item integer")
   (info-incl-previous-line-p
    :initarg :info-incl-previous-line-p
    :accessor info-incl-previous-line-p
    :type  :boolean
    :initform NIL
    :documentation  "Include file info previous line?")
   (info-extra-lines-n
    :initarg :info-extra-lines-n
    :accessor info-extra-lines-n
    :type  :string
    :initform NIL
    :documentation  "Include file info N extra lines")
   #|   (text-input-ratings
    :initarg :text-input-ratings
    :accessor text-input-ratings
    :type  :string
   :initform NIL
    :documentation  "Data from text input ratings")|#
   )
  (:PANES
   (multi-column-list-panel-1
    capi:multi-column-list-panel
    ;;HERE11
    :items '((" ")(" ")(" ")(" ")(" ")(" "))
    :title NIL
    :vertical-scroll T
    :selection 0
    :message NIL
    :callback-type '(:DATA :ELEMENT :INTERFACE :COLLECTION :ITEM)
    :selection-callback 'SELECTED-MARK-ITEM-CALLBACK
    :columns '((:title " MARK ")(:title "PRIORITY") (:title "       BUFFER       ") ( :title "      NOTES         ")(:title "                      FILE LINES AT MARK                        ")(:title "        DIR        " ) )
    :font *font-title-times11b
    :item-print-functions 'MY-PRINT-CHOICE-LIST-ITEMS
   :reorderable-columns T
    ;; :visible-max-width 500
    ;;end multi-col
    )
   (markstr-input-pane
    capi:text-input-pane   
    :visible-max-width 40
    :title "    Type NEW MARK NAME: "
    )
   (priority-input-pane
    capi:text-input-pane   
    :title  " *PRIORITY "
    :visible-max-width 30
    )
   (buffer-name-input-pane
    capi:text-input-pane   
    :title "    Type NEW BUFFER NAME: "
    :visible-max-width 200
    )
   (mark-notes-input-pane
    capi:text-input-pane   
    :title "    Type MY NOTES: "
    :width nil
    )
   (action-check-button-panel
    capi:check-button-panel
    :items *mark-info-action-checkboxes 
    :title ">>  Select ACTION for GET MARK INFO:  "
    :callback-type '(:DATA :ELEMENT :INTERFACE  :ITEM)
    :selection-callback 'MARK-ACTION-CHECKBOX-CALLBACK
    )
   (display-pane-1
    capi:display-pane
    :text "  BUFFER LINES BEFORE/AFTER SELECTED MARK:   "
    :default-width nil) ;; :visible-min-width 900)
   
   (goto-mark-button
    capi:push-button
    :text "GO to MK"
    :callback-type '(:DATA :ELEMENT :INTERFACE  :ITEM)
    :selection-callback 'GOTO-MARK-CALLBACK 
    )
   (mark-info-button
    capi:push-button
    :text "ADD MK"
    :callback-type '(:data :element :interface :collection :item)
    :selection-callback 'GET-MARK-INFO-CALLBACK
    )
   (search-for-info-button
    capi:push-button
    :text "SEARCH for MK"
    :callback-type :data-interface
    :selection-callback 'SEARCH-FOR-BUF-INFO-CALLBACK
    )
   (clear-input-button
    capi:push-button
    :text "Clear Input"
    :callback-type :data-interface
    :selection-callback 'CLEAR-MARK-INPUT-CALLBACK
    )
   (delete-mark-button
    capi:push-button
    :text "DEL MK"
    :callback-type '(:element :interface)
    :selection-callback 'DELETE-MARK-CALLBACK
    )
   (delete-all-marks-button
    capi:push-button
    :text "DEL ALL"
    :callback-type '(:element :interface)
    :selection-callback 'DELETE-ALL-MARKS-CALLBACK
    )
   (save-settings&info-button
    capi:push-button
    :text "SAVE ALL"
    :callback 'SAVE-MARK-INFO-SETTINGS-CALLBACK
    :callback-type :data-interface
    )
   ;;end panes
   )

  (:LAYOUTS
   (column-layout-1
    capi:column-layout
    '(multi-column-list-panel-1   row-layout-1 row-layout-2 
                                input-column-layout check-button-row-layout))
   (row-layout-1
    capi:row-layout
    '(display-pane-1))
   (row-layout-2
    capi:row-layout
    '(goto-mark-button mark-info-button  save-settings&info-button
                       delete-mark-button delete-all-marks-button
                       search-for-info-button clear-input-button ))
   (input-column-layout
    capi:column-layout
    '(  input-row-layout  mark-notes-input-pane))
   (input-row-layout
    capi:row-layout
    '(markstr-input-pane priority-input-pane buffer-name-input-pane))
   (check-button-row-layout
    capi:row-layout
    '(action-check-button-panel)
    )
   ;;end layouts
   )
  ;;MENUS
  (:menu-bar settings-menu )
  (:menus
   (menu-info-incl-previous-line-p
    ;;no?   "Photo Options"
    :component
    (("Include Previous File Line"
      :data T)
     ("NOT Include Previous File Line"
      :data NIL)
     )
    :interaction :single-selection
    :callback-type :data-interface
    :callback 'INFO-INCL-PREVIOUS-LINE-P-CALLBACK
    )
   (menu-info-extra-lines-n
    "Select File Info EXTRA LINES."
    (menu-info-extra-lines-n1))
   (menu-info-extra-lines-n1
    ;; "Select SlideShow Display Time"
    :component
    ;;If CHANGE THIS LIST, MUST CHANGE LIST IN 
    (("1" :data 1)("2" :data 2 )("3" :data 3  )("4" :data  4)("5" :data  5)("6" :data  6)("7" :data 7)("8" :data  8)("9" :data  9)("10":data 10))
    :interaction :single-selection
    ;;  :selection-function 'get-display-seconds-index
    :callback-type :data-interface
    :selection-callback 'FILE-MARK-EXTRA-LINES-N-CALLBACK
    ) 
   (settings-menu
    "SETTINGS MENU"
    (menu-info-incl-previous-line-p
     menu-info-extra-lines-n
     ("SAVE ALL SETTINGS"
      :callback 'SAVE-MARK-INFO-SETTINGS-CALLBACK
      :callback-type :data-interface)))
  ;;end new menus
   ) 
  ;;initargs ;;1MK
  (:default-initargs
   :best-height 580
   :best-width 500
   :visible-max-width 500
   :x 0
   :y 550
   :internal-border 10
   :background :yellow
   :layout 'column-layout-1
   :title "BUFFER MARKS:  Can GOTO or MODIFY MARKS >> MUST SAVE BUFFER BEFORE LOOKUP"))





;; USE IN ABOVE TEST
;; (setf **test-mll '((" 1MK  " " SS? " "   U-BUFFER-TEST-1.lisp       " "  my    notes   here   "    "CogSys-Model"    "(defun get-nth-in-all-2nested-lists ~% (nth list-of-1nested-lists)  In U-lists RETURNS (values nths-1nested-lists non-nth-items). ~%NTHS-1NESTED-LISTS list of all items at nth place~% for each list in each group of 1nested lists"  ) ("  2MK  " "      " "    buffer              " "     notes   "   " dir  "  "  info   ")  ("  3MK  " "      " "    buffer              " "     notes   "  " dir  "  "  info   ") ("  4MK  " "      " "    buffer              "  "     notes   " " dir  "  "  info   ")))



;;MINF
;;2020
;;ddd
(defun MINF (&optional pathlist)
  "U-file&buffers-frames.lisp. Calls make-text-mark-frame"
  (make-text-mark-frame pathlist))



;;MAKE-TEXT-MARK-FRAME
;;2020
;;ddd
(defun make-text-mark-frame (&optional pathlist)
  "U-file&buffers-frames.lisp. File buffers must EXIST (or use pathlist to open)USE TMF abbrev."
  (let*
      ((inst (make-instance 'Text-Mark-Interface))
       (new-pane-items) ;;  **test-mll  )
       )
    ;;open files into buffers, if pathlist
    (when pathlist
      (my-edit-files pathlist))

    ;;GET PERMANENT SETTNGS
    (when (and (boundp '*save-mark-info-settings-path)
               (probe-file *save-mark-info-settings-path))
      (load *save-mark-info-settings-path))
    ;;3-MARK-INFO-SETTINGS

    ;;update the pane items & slot-values for first item (default item?)
    (setf new-pane-items *all-mark-info-items
          (slot-value inst 'markstr) (caar *all-mark-info-items)
          (slot-value inst 'buffer-name)(third (car *all-mark-info-items))
          (slot-value inst 'info-incl-previous-line-p) *info-incl-previous-line-p
          (slot-value inst 'info-extra-lines-n) *info-extra-lines-n)
  
    (with-slots (multi-column-list-panel-1 )  inst
      (capi:apply-in-pane-process multi-column-list-panel-1
                                  #'(setf capi:collection-items) 
                                  new-pane-items  multi-column-list-panel-1)
    (capi:display inst)
  ;;end let, make-text-mark-frame
  )))
;;TEST
;;SSSSSS START HERE make-text-mark-frame
;; (make-text-mark-frame)



   ;; #<STANDARD-READER-METHOD CAPI:COLLECTION-ITEMS NIL (CAPI:COLLECTION) 216F881F>
    ;;#<STANDARD-METHOD CAPI:CHOICE-SELECTED-ITEM NIL (CAPI:CHOICE) 20FFFA67>

;;GET-MARK-INFO-CALLBACK
;;2020
;;ddd
(defun get-mark-info-callback (data element interface collection item)
  "U-file&buffers-frames,  RETURNS    INPUT:  
    ACTION: :REPLACE-ALL :APPEND-LIST :REPLACE-LIST :REPLACE-ELM"
  ;;(break "callback input") data=nil, coll=nil, item=unknown elem & inter OK.
  (with-slots (multi-column-list-panel-1 markstr-input-pane
   priority-input-pane  buffer-name-input-pane  mark-notes-input-pane                                 ) interface
  (let*
      ((selected-item (CAPI:CHOICE-SELECTED-ITEM multi-column-list-panel-1))
       (markstr (capi::pane-text markstr-input-pane))
       (priority  (capi::pane-text  priority-input-pane))
       (input-pane-buffer-name (capi::pane-text  buffer-name-input-pane))
       (buffer-name (cond ((not (string-equal input-pane-buffer-name ""))
                           input-pane-buffer-name)
                          (T (slot-value interface 'buffer-name))))
       ;;doesn't work SSSS fix? so I can get cur buffer w/o typing it in?
#|(cond  ((my-equal input-pane-buffer-name "")    (editor:current-buffer))
                          (input-pane-buffer-name) (T (slot-value interface 'buffer-name)))|#
       (buf-obj (editor:buffer-from-name buffer-name))
       (buf-path (editor:buffer-pathname buf-obj))
       (buf-pathname (namestring buf-path))
       (mark-notes (capi::pane-text  mark-notes-input-pane))
       (mark-info)
       (extra-lines-str)
       (incl-last-line-p (slot-value interface 'info-incl-previous-line-p))
       (info-extra-lines-n   (slot-value interface 'info-extra-lines-n))  

       ;;most not used below??
       (ans data)
       (item-n (car data))
       (text (second data))
       (file-pos (cond ((stringp ans) (convert-string-to-integer (third ans)))
                       (t 0)))
       ;;FOR TESTING
       (match-elm)
       (item-elm-n)
       (new-item-elm)  
       (modified-item)
       (new-item)
       (new-pane-items) ;; **test-mcb-info)
       (append-new-item)
       ;;'("  6MK  " "      " "    buffer              "  "     NEW ITEM 2   " " dir  "  "  info   "))
       (delete-item-n) 
       (list-edit-action) 
       )
    ;;(setf **cur-let-vars (format nil "selected-item ~A priority= ~A buffer-name= ~A mark-notes= ~A" selected-item  priority buffer-name mark-notes))
    ;;(BREAK "after let")

    ;;MODIFY THE ITEM--OR ADD NEW FROM INPUT

    (setf *goto-mark-callback-data ans ;;("Item" "Two")
          *goto-mark-callback-item-n item-n
          *goto-mark-callback-text text
          *goto-mark-callback-file-pos (second ans))
#|    (with-slots ( mark-pos selected-item-list modified-item-list new-item-list
                         selected-item-n
                         MULTI-COLUMN-LIST-PANEL-1)  interface|#
      (setf (slot-value interface 'selected-item-n) item-n
            (slot-value interface 'selected-item-list) selected-item
            (slot-value interface 'mark-pos) file-pos)
      ;;SSSS CHECK THIS
      (setf (slot-value interface 'buffer-name) buffer-name)
      ;;markstr
      (unless markstr
        (setf markstr (slot-value interface 'markstr)))
      ;;(setf append-new-item '("5MK" "  " "TEST-BUFFER.lisp" "     notes   "   " dir  "  "  info   "))

      ;;STEP ?: UPDATE THE FRAME MULTI-LIST DATA
      (setf list-edit-action (slot-value interface 'list-edit-action)
            mark-info (slot-value interface 'mark-info))
      (cond
       ((and (or (equal list-edit-action :auto-add)(null list-edit-action) ;;auto-add
                 (equal list-edit-action ""))
             markstr buf-pathname)
        ;;(break "markstr")

        ;;GET MARK-INFO FROM FILE
        ;;(setf pathname (editor:buffer-pathname buf-obj)) ;;NOT NEEDED?
        (unless (numberp *info-extra-lines-n)
          (setf *info-extra-lines-n *default-info-extra-lines-n))
        ;;(break "1")
        (multiple-value-bind (file-position found-line last-line extra-lines)
            (search-file markstr buf-pathname  ;;move to menu
                         :return-extra-lines *info-extra-lines-n)
;; (search-file "(defun get-mark-info" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-file&buffers-frames.lisp" :return-extra-lines *info-extra-lines-n)
          ;;(break "after search-file")
          ;;SET THE MARK INFO
          (setf mark-info (format nil "MK> ~A" found-line))
          ;;incl previous line &/or extra-lines in file info?
          (setf  extra-lines-str  (format-string-list extra-lines :line-width 20
                                             :add-newlines 1     
                                              :add-pre-to-all "LN>" :left-margin-spaces 1
                                              :eval-bound-symbols-p NIL))
          ;;(break "mark-info")
          (when incl-last-line-p ;;menu
            (setf mark-info (format nil " PRE> ~A~%*~A" last-line mark-info )))
          (when extra-lines-str
            (setf mark-info (format nil "~A~% ~A" mark-info extra-lines-str)))
          ;;update slot-value
          (setf (slot-value interface 'mark-info) mark-info)
          ;;append new item to big list
          (setf append-new-item
                (list markstr priority buffer-name mark-notes mark-info))
          ;;(break "auto append-new-item")
          (multiple-value-bind (new-pane-items1 new-item1 new-item-elm1
                                              pane-items matched-elm matched-item-elm) 
            (edit-list-pane-items 'MULTI-COLUMN-LIST-PANEL-1
                                  MULTI-COLUMN-LIST-PANEL-1 interface
                                  :match-elm match-elm :item-n  item-n
                                  :item-elm-n item-elm-n
                                  :new-item-elm  new-item-elm :new-item new-item 
                                  :new-pane-items new-pane-items
                                  :append-new-item append-new-item 
                                  :delete-item-n delete-item-n) 
          (setf new-pane-items new-pane-items1
                *all-mark-info-items new-pane-items
                new-item new-item1  new-item-elm new-item-elm1)         
          ;;end 2 mvb, list-edit-action
          )))
       ;;UPDATE FRAME FROM USER INPUT DATA?
       ((not (equal list-edit-action :auto-add))
        (cond
         ((equal list-edit-action :update-list)
          (setf new-item (list markstr priority buffer-name mark-notes mark-info)))
         ((equal list-edit-action :append-list)
          (setf append-new-item 
                (list markstr priority buffer-name mark-notes mark-info)))
         ((equal list-edit-action :delete-item)
          (setf new-item :delete-item))
         ((equal list-edit-action :replace-all)
          (setf new-pane-items1 (list new-item)))
         (T nil))
        (break "before edit-list-pane-items")
        ;;EDIT: 1-looks up old info, 2-compares to new-item or appends or deletes
        (multiple-value-bind (new-pane-items1 new-item1 new-item-elm1
                                              pane-items matched-elm matched-item-elm) 
            (edit-list-pane-items 'MULTI-COLUMN-LIST-PANEL-1
                                  MULTI-COLUMN-LIST-PANEL-1 interface
                                  :match-elm match-elm :item-n  item-n
                                  :item-elm-n item-elm-n
                                  :new-item-elm  new-item-elm :new-item new-item 
                                  :new-pane-items new-pane-items
                                  :append-new-item append-new-item 
                                  :delete-item-n delete-item-n) 
          (setf new-pane-items new-pane-items1
                new-item new-item1  new-item-elm new-item-elm1)
          ;;end mvb, list-edit-action
          ))
       ;;OTHERWISE SEARCH FILE FOR MARK & RELATED INFO
       ;;MARK & ACCURATE BUFFER-NAME MUST BE IN INPUT
              (T NIL))
      ;;UPDATE SLOT-VALUES
      (setf (slot-value interface 'markstr) (car new-pane-items)
            (slot-value interface 'priority) (second new-pane-items)
            (slot-value interface 'buffer-name) (third new-pane-items)    
            (slot-value interface 'buffer-obj) (editor:buffer-from-name buffer-name) 
            (slot-value interface 'mark-notes) (fourth new-pane-items)
            (slot-value interface 'mark-info) (fifth new-pane-items))
            
      ;;update global var for saving info, etc
      (setf *all-mark-info-items new-pane-items)
        (values new-pane-items new-item new-item-elm)
        ;;end ,with-s,let, with-slots,get-mark-info-callback
        )))
;;TEST
;; (setf **test-mcb-info '((" 1MK  " " SS? " "   BUFFER-TEST-1.lisp       " "  my    notes   here   "    "CogSys-Model"    "(defun get-nth-in-all-2nested-lists ~% (nth list-of-1nested-lists)  In U-lists RETURNS (values nths-1nested-lists non-nth-items). ~%NTHS-1NESTED-LISTS list of all items at nth place~% for each list in each group of 1nested lists"   ) ("  2MK  " "      " "    BUFFER-TEST-1.lisp              " "     notes   "   " dir  "  "  info   ")   ("  3MK  " "      " "    BUFFER-TEST-1.lisp              " "     notes   "  " dir  "  "  info   ")))
;;  ("  5MK  " "      " "    BUFFER-TEST-1.lisp              "  "     NEW ITEM   " " dir  "  "  info   ")))

;; (make-text-mark-interface-TEST)




;;GOTO-MARK-CALLBACK
;;2020
;;ddd
(defun goto-mark-callback (data element interface  item)
  "U-file&buffers-frames,  RETURNS markstr "
  (let*
      ((markstr (slot-value interface 'markstr))     
       (buffer-name (slot-value interface 'buffer-name))
       (buffer-obj (editor:buffer-from-name buffer-name))
       )           
    ;;GO TO BUFFER
    (multiple-value-bind (search-result buf-obj start-pt-obj end-pt-obj
                                        new-temp-pt)
        (my-search-in-buffer markstr buffer-name) 
      ;;(my-search-in-buffer ";;MK1" "BUFFER-TEST-1.lisp")             
      markstr
      ;;end mvb,let, goto-mark-callback
      )))
;;TEST
;; (goto-mark-callback



 
;;MARK-ACTION-CHECKBOX-CALLBACK
;;2020
;;ddd
(defun mark-action-checkbox-callback (data element interface  item)
  "U-file&buffers-frames   RETURNS    INPUT: 
"
;; "actions: :replace-all :append-list :replace-list :replace-elm" )
  ;;works is data = " APPEND LIST " ELEM, INT, NOT ITEM
  (let*
      ((edit-action)
       )
    (cond
     ((string-equal data (car *mark-info-action-checkboxes))
      (setf edit-action :auto-add))
     ((string-equal data (second *mark-info-action-checkboxes))
      (setf edit-action :update-list ))
     ((string-equal data (third *mark-info-action-checkboxes))
      (setf edit-action :replace-all )) 
     ((string-equal data (fourth *mark-info-action-checkboxes))
      (setf edit-action :append-list))
    ;;((string-equal data (fifth *mark-info-action-checkboxes))
    ;;  (setf edit-action :delete-item))
    ;;DEFAULT IS USE :AUTO-ADD
     (T (setf edit-action :auto-add)))  ;;ED-ACT
    ;;set slot val
    (setf (slot-value interface 'list-edit-action) edit-action)
    edit-action
    ;;end let, mark-action-checkbox-callback
    ))
;;TEST
;; (mark-action-checkbox-callback




;;SELECTED-MARK-ITEM-CALLBACK
;;2020
;;ddd
(defun selected-mark-item-callback (data element interface collection item)
  "U-file&buffers-frames   RETURNS    INPUT: 
"
  (let*
      ((mark-item-list data)
       (markstr (car data)) ;; (capi::pane-text markstr-input-pane))
       (priority (second data)) ;;   (capi::pane-text  priority-input-pane))
       (buffer-name (third data)) ;;  (capi::pane-text  buffer-name-input-pane))
       (buf-obj) ;;  (editor:buffer-from-name buffer-name))
       (buf-pathname) ;;  (editor:buffer-pathname buf-obj))
       (mark-notes (fourth data)) ;;  (capi::pane-text  mark-notes-input-pane))
       (mark-info (fifth data))
       )
    (setf (slot-value interface 'selected-item-list) mark-item-list
          (slot-value interface 'markstr) markstr
          (slot-value interface 'priority) priority
           (slot-value interface 'buffer-name) buffer-name
          (slot-value interface 'mark-notes) mark-notes
          (slot-value interface 'mark-info) mark-info)
    ;;display-pane-1
    (with-slots (display-pane-1 buffer-name-input-pane )  interface
    (capi:apply-in-pane-process display-pane-1
                                #'(setf capi:display-pane-text) 
                                   mark-info  display-pane-1)
    (capi:apply-in-pane-process buffer-name-input-pane
                                #'(setf capi:text-input-pane-text) 
                                   buffer-name  buffer-name-input-pane)
    (capi:display interface)
  ;;end let, make-text-mark-interface
  )

    ;;(break "after let & set")

    (values data interface )
    ;;end let, selected-mark-item-callback
    ))
;;TEST
;; (selected-mark-item-callback



;;NOT USED--INCORPORATED INTO 
;;UPDATE-TEXT-MARK-FRAME
;;2020
;;ddd
#|(defun update-text-mark-frame (data interface)
  " "
  (with-slots (do-val-column-layout  do-rank-column-layout  
                                     display-val-list-panel  display-rank-list-panel) interface
  (let*
      ((val-panes (capi:layout-description  do-val-column-layout))
             ;;no (get-multiple-input-pane-text  val-panes  :return-objects-p NIL))
       (rating-panes (capi:layout-description  do-rank-column-layout))
       (val-pane-strs  (slot-value interface 'value-items))
       (calling-process (slot-value interface 'calling-process))
       (ordered-rating-strs)
       (ordered-ratings)
       (ordered-val-strs )
       (new-frame-inst)
       (n-ranking-groups (slot-value interface 'n-ranking-groups))
       )
    (multiple-value-bind (rating-strs ratings)
        (get-multiple-input-pane-text  rating-panes  :return-objects-p T)

      (multiple-value-bind (ordered-ratings ordered-val-strs)
                  (sort-lists-by-list1 ratings  val-pane-strs)

        (setf ordered-rating-strs (make-list-of-strings ordered-ratings))
      ;;(break "ordered-val-strs")  
   
       ;;SET TEMP GLOBAL VARS
      (setf *temp-ratings ratings
            *temp-ratings-strs rating-strs)

      (multiple-value-setq (*temp-ordered-ratings *temp-ordered-items)
          (sort-lists-by-list1 ratings val-pane-strs ))
      (setf  *temp-ordered-ratings-strs 
             (make-list-of-strings *temp-ordered-ratings) 
             *temp-ordered-items-strs
             (make-list-of-strings *temp-ordered-items)
             *temp-ordered-items-ratings-lists
             (make-lists-from-items *temp-ordered-items
                                    *temp-ordered-ratings)
             *temp-ordered-items-ratings-strs
             (make-lists-from-items *temp-ordered-items-strs  
                                    *temp-ordered-ratings-strs))
        (when (> n-ranking-groups 0)
          (setf *csq-value-rank-temp-instrs *temp-group-items-instrs)
          ;;end when
          )       

        ;;MAKE NEW FRAME (with new values), CLOSE OLD
        (make-value-rank-frame NIL :items-list ordered-val-strs
                               :ratings-list ordered-rating-strs
                               :ascending-p T
                               :frame-title *csq-value-rate-frame-title
                               :calling-process calling-process
                               :display-initial-ratings-p T)

        (capi:destroy interface)
 
        (values ordered-ratings ordered-rating-strs)
        ;;end mvb,mvb, with, update-rankings-callback
        )))))|#



;;SAVE-MARK-INFO-SETTINGS-CALLBACK
;;2020
;;ddd
(defun save-mark-info-settings-callback (data interface)
  "U-file&buffers-frames, saves settings and *ALL-MARK-INFO-ITEMS to *SAVE-MARK-INFO-SETTINGS-PATH"
  (let*
      ((info-incl-previous-line-p (slot-value interface 'info-incl-previous-line-p))
       (info-extra-lines-n (slot-value interface 'info-extra-lines-n))
       (date (my-get-date-time))
       )
    (unless (numberp info-extra-lines-n)
      (setf info-extra-lines-n *default-info-extra-lines-n))
                     
    (with-open-file (outstr *SAVE-MARK-INFO-SETTINGS-PATH
                            :direction :output
                            :if-exists :supersede  :if-does-not-exist :create)
      (cond
       (*all-mark-info-items  ;;savset
        (format outstr ";;DATE: ~S~%;;SETTINGS:~% 
   (setf *info-extra-lines-n ~S
          *mark-incl-last-line-p ~S)
   (unless (and (boundp '*all-mark-info-items) *all-mark-info-items)
      (defparameter *all-mark-info-items (quote ~S)  \"Main list for all mark items\"))
      "   date info-incl-previous-line-p info-extra-lines-n *all-mark-info-items))
       (T
        (format outstr ";;DATE: ~S~%;;SETTINGS:~% 
   (setf *info-extra-lines-n ~S
          *mark-incl-last-line-p ~S)
   (unless (boundp '*all-mark-info-items)
      (defparameter *all-mark-info-items  NIL  \"Main list for all mark items\"))
      "   date info-incl-previous-line-p info-extra-lines-n )))
      ;;end let, save-mark-info-settings-callback
      )))
;;TEST
;;




;;DELETE-MARK-CALLBACK
;;2020
;;ddd
(defun delete-mark-callback (element interface)
  "U-file&buffers-frames,  Deletes selected list item."
  ;;(break "callback input") ;;data=nil, coll=nil, item=unknown elem & inter OK.
  (with-slots (multi-column-list-panel-1  ) interface
  (let*
      ((selected-item (CAPI:CHOICE-SELECTED-ITEM multi-column-list-panel-1))
       (match-elm (slot-value interface 'markstr))
       (all-items)
       )

      ;;UPDATE THE FRAME MULTI-LIST DATA
        (multiple-value-bind (new-pane-items1 new-item1 new-item-elm1
                                              pane-items matched-elm matched-item-elm) 
            (edit-list-pane-items 'MULTI-COLUMN-LIST-PANEL-1
                                  MULTI-COLUMN-LIST-PANEL-1 interface
                                  :match-elm match-elm
                                  :new-item :DELETE-ITEM )
          (setf new-pane-items new-pane-items1
                *all-mark-info-items new-pane-items
                new-item new-item1  new-item-elm new-item-elm1)  
          ;;SSSSS MODIFY TO DELETE MARKS IN FILES AS WELL??
        (values new-pane-items new-item new-item-elm)
        ;;end ,mvb, with-slots,delete-mark-callback
        ))))
;;TEST
;;


;;DELETE-ALL-MARKS-CALLBACK
;;2020
;;ddd
(defun delete-all-marks-callback (element interface)
  "U-file&buffers-frames,  Deletes ALL mark items in Frame (only)"
  (with-slots (multi-column-list-panel-1) interface
    (setf (slot-value interface 'new-item-list) NIL
          *all-mark-info-items NIL) 
    (capi:apply-in-pane-process MULTI-COLUMN-LIST-PANEL-1
                                #'(setf capi:collection-items) 
                                NIL  MULTI-COLUMN-LIST-PANEL-1)
    ;;end with-slots,delete-all-marks-callback
    ))


;;CLEAR-MARK-INPUT-CALLBACK
;;2020
;;ddd
(defun clear-mark-input-callback (data  interface)
  "U-file&buffers-frames,  Clears input pane text boxes."
  ;;(break "callback input") data=nil, coll=nil, item=unknown elem & inter OK.
  (with-slots ( markstr-input-pane
   priority-input-pane  buffer-name-input-pane  mark-notes-input-pane                                 ) interface

    (setf (capi:text-input-pane-text markstr-input-pane) ""
          (capi:text-input-pane-text  priority-input-pane) ""
          (capi:text-input-pane-text  buffer-name-input-pane) ""
          (capi:text-input-pane-text  mark-notes-input-pane) "")
                ;; (values new-pane-items new-item new-item-elm)
        ;;end ,with-s,let, with-slots,clear-mark-input-callback
        ))





;;INFO-INCL-PREVIOUS-LINE-P-CALLBACK
;;2020
;;ddd
(defun info-incl-previous-line-p-callback (data interface)
  "U-files&buffers-frames, menu callback"
    (setf *info-incl-previous-line-p data
          (slot-value interface 'info-incl-previous-line-p) data)
    )

;;FILE-MARK-EXTRA-LINES-CALLBACK
;;2020
;;ddd
(defun file-mark-extra-lines-n-callback (data interface)
  "U-files&buffers-frames, menu callback"
    (setf *info-extra-lines-n data
          (slot-value interface 'file-mark-extra-lines-n) data)
    )


      

;;SEARCH-FOR-BUF-INFO-CALLBACK
;;2020
;;ddd
(defun search-for-buf-info-callback (data interface)
  "U-file&buffers  "
  (with-slots (markstr-input-pane buffer-name-input-pane ) interface
  (let*
      ((search-str (capi:text-input-pane-text buffer-name-input-pane))
       (markstr (capi:text-input-pane-text markstr-input-pane))
       )
    (multiple-value-bind ( buf-name  buf-path  buf-obj buf-n)
        (search-for-buffer-info search-str)
      (setf (slot-value interface 'buffer-name) buf-name
            (slot-value interface 'buffer-obj) buf-obj
            (slot-value interface 'pathname) buf-path 
            (slot-value interface 'markstr) markstr)
      (capi:apply-in-pane-process buffer-name-input-pane
                                  #' (setf capi:text-input-pane-text) buf-name
                                  buffer-name-input-pane)
    ;;end let, search-for-buf-info-callback
    ))))
