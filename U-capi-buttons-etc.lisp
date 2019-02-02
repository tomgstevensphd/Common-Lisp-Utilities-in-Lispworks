;;**********************U-capi-buttons-etc.lisp **************************;;
;;

#|(capi:define-interface radio-buttons ()
  ;;sets the button/item data values
  ((data :initform (list 1 2 3)))
  (:layouts
   (panel;
    capi:column-layout)))|#





;;XPER VERSION USING CHECK BUTTONS FOR MULTI-SELECTION
;;MAKE-MY-VERTICAL-BUTTON-PANEL
;; NOTE: Use of   (eval `(defun ... (quote ,variable) .. below
;;ddd
(defun make-my-vertical-button-panel (parent-layout-name
                                      interface-name 
                                      button-text-list  button-data-list            
                                      &key   (close-interface-on-selection-p T)
                                      (selection-type 'single)
                                      button-arglist
                                      button-layout-arglist
                                      button-text-pane-arglist
                                      button-text-layout-arglist
                                      (last-button-on-top-p NIL)
                                      (send-datalist-to-instance-slot 'selected-item-datalist)  
                                      (max-ans-text-line-length *max-ans-text-line-length)
                                      (button-text-pane-height *button-text-pane-height)
                                      )    ;;&rest button-arglist) caused error
  "In U-capi-buttons-etc.lisp.  DOC may be less than 100% up to date. Makes a button layout, 'button-layout-name', which is a customized column layout of buttons that allows more options than the standard radio-button panel. Uses radio buttons as the buttons. All regular initargs to the layout is accessed via button-layout-arglist, and all radio-button initargs thru the &rest list. Starts with NO DEFAULT PRESELECTED BUTTON-a major change over capi.  Use to create buttons and add to interfaces. Makes a  layout and puts inside parent-layout. REQUIRES a pre-defined INTERFACE  a layout named parent-layout-name. The arglists must be preceded by double quotes. EXAMPLE: (make-my-button-panel  'row-layout-1 'test-my-radio-buttons  '(list 'A 'B 'C)  '(list 10 20 30)  :button-layout-arglist   '(list :visible-min-width 200 etc)) ETC. Creates a GLOBAL VAR, (format nil \"*~A-selected-values-list\" parent-layout-name) = (list selected-text selected-data selected-n[begins with 1]  selected-button-instance last-button-on-top-p) of last selected button. IF MULIPLE-SELECTION, RETURNS (list selected-text-LIST selected-data-LIST selected-n-LIST[begins with 1]  selected-button-instance-LIST  last-button-on-top-p). Single selection also calls append-my-vertical-button-panel-single-selection-callback multiple-selection calls append-my-vertical-button-panel-multi-selection-callback. button-text-pane-height is the height of the answer-buttons and panes."
  (let
      ((global-selected-values-list (my-make-symbol 
                                     (format nil "*~A-selected-values-list" parent-layout-name)))
       (button-list)
       (button-layout-inst)
       (button-text-pane-list)
       (button-text-layout-inst)
       )

    (cond
     ((or  *single-selection-item (member selection-type
            (list 'single  :single  :single-selection "single" "SINGLE" "Single"  :test 'equal)))
      (setf selection-type :single-selection))
     (t (setf selection-type :multiple-selection)))

#|    (setf out1  (format nil "IN MAKE-MY-VERTICAL-BUTTON-PANEL~%
parent-layout-name= ~A~% interface-name= ~A~%button-text-list= ~A~%  button-data-list = ~A~%selection-type= ~A~% button-arglist= ~A~%button-layout-arglist= ~A~%button-text-pane-arglist= ~A~%button-text-layout-arglist= ~A~% send-datalist-to-instance-slot= ~A~%"parent-layout-name  interface-name  button-text-list  button-data-list  selection-type button-arglist button-layout-arglist button-text-pane-arglist button-text-layout-arglist send-datalist-to-instance-slot ))|#

    ;;Make the SELECTION CALLBACK
    ;;ddd
    (eval 
     `(defun my-button-panel-selection-callback (item interface)  
        "In U-capi-buttons-etc.lisp, MUST use with my-button-panels.  MODIFY THIS CALLBACK to get data or actions from the button selection."

        ;;(afout 'out (format nil "FIRING my-button-panel-selection-callback"))
        (let
            ((button-value)
             (button-data)
             (selected-text-n)
             (selected-text)
             (selected-data)
             (selected-button)
             (selection-type1 ,selection-type)
             (button-layout)
             (button-text-layout)
             ;;  (datalist-instance)
             ;;   (datalist-slot (quote ,send-datalist-to-instance-slot))
             (last-button-on-top-p1 ,last-button-on-top-p)
             )
       ;;   (setf out3  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-name) ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-name)))
             ;;(afout 'out (format nil "selection-type1= ~A~%" selection-type1))
          (with-slots (,parent-layout-name ,send-datalist-to-instance-slot) interface 
            #|(afout 'out (format nil ",parent-layout-name= ~A~% ,send-datalist-to-instance-slot= ~A~%  item= ~A~%, ",parent-layout-name ,send-datalist-to-instance-slot   item  ))|#
         ;;   (setf out4  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-name) ~A~%item= ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-name) item))
           
            ;;TO CHANGE SELECTION, MUST UNSET PREVOUSLY SELECTED BUTTONS
            (setf  BUTTON-LAYOUT (CAR (capi:layout-description ,parent-layout-name)))    
            (cond
             ((equal selection-type1 :multiple-selection)
              ;;DON'T DO ANYTHING.  CALLBACK IS FROM GO BUTTON INSTEAD
              )
             ((equal selection-type1 :single-selection)
                 (loop
                  for button in (capi:layout-description BUTTON-LAYOUT) ;; ,button-layout-name)
                  unless (eq item button) ;;  (or (equal item button) (null item))
                  do 
                  (setf (capi:item-selected button) nil)
                  ;;(afout 'out (format nil "item= ~A~%button= ~A (capi:item-selected button)= ~A"  item button (capi:item-selected button)))
                  ;;end loop, if
                  
            ;;FIND SELECTED BUTTON N, TEXT, DATA, and BUTTON-INSTANCE
            (setf selected-n  (my-make-symbol (capi:item-text item))
                  selected-text (nth (- selected-n 1) ,button-text-list)
                  selected-data (capi:item-data item)
                  selected-button item)   
            ;;end loop
            )
              
            ;;SET GLOBAL VARIABLE TO DATALIST VALUES
            (setf ,global-selected-values-list 
                  (list selected-text selected-data selected-n  selected-button last-button-on-top-p1))
            ;; SET SLOT-VALUE IN QUESTION FRAME INSTANCE TO DATALIST
            (cond
             ((quote ,send-datalist-to-instance-slot)
              (setf (slot-value interface (quote ,send-datalist-to-instance-slot ))
                    (list selected-text selected-data selected-n  
                          selected-button last-button-on-top-p1)))
             (t nil))
            ;;   (afout 'out (format nil "IN CALLBACK, (slot-value interface (quote ,send-datalist-to-instance-slot) )= ~A~%  (quote ,send-datalist-to-instance-slot)= ~a~%" (slot-value interface (quote ,send-datalist-to-instance-slot) )   (quote ,send-datalist-to-instance-slot)))
            ;;CLOSE THE INTERFACE INSTANCE ONCE SELECTION MADE?
            (if ,close-interface-on-selection-p
                (capi:destroy interface))

            ;;end single-selection
            )
             (t nil))
            ;;IF *CALL-SHAQ-CALLBACK-P, THEN DATA PROCESSING AND
            ;;   CALLING NEXT FRAME
            (cond
             ((equal selection-type1 :single-selection)
                (append-my-vertical-button-panel-single-selection-callback item  interface))
             ((equal selection-type1 :multiple-selection)
              ;;DO NOTHING SINCE USING CHECK-BUTTONS
              ;;Must deselect button if checked even number of times
              
             ;; data actions, etc initiated by GO Button for Multi-selection items
             ;;(append-my-vertical-button-panel-multi-selection-callback item  interface)

             ;;end multi-selection type
              )
             (t nil))

        ;;end  with-slots, defun callback,eval
        ))))

    ;;SSS START HERE DEBUGGING  (CAN REALLY USE THIS ON RADIO BUTTONS?)  
    ;;RETRACT-CALLBACK TO UNMARK BUTTON ON MULTI-SEL
    ;;ddd
    ;;APPARENTLY RADIO-BUTTON DOESN'T CALL RETRACT-CALLBACK
#|    (eval
     `(defun my-button-panel-retract-callback (item interface)
        (cond
         ((equal selection-type1 :single-selection)
          ;;DO NOTHING TO RETRACT, WILL RETRACT WHEN CHOOSE OTHER
          (setf out4(format nil "IN my-button-panel-retract-callback, ITEM= ~A" item))
          )
         (t
          (setf out4(format nil "IN my-button-panel-retract-callback, ITEM= ~A" item))
;;(setf (capi:item-selected button) nil)
          (setf (capi:item-selected  item) nil)))
        ))
|#
    ;; MAKE BUTTON FUNCTION
    ;;ddd
    (defun make-a-button (text data button-arglist 
                               &key (button-type 'capi:radio-button) 
                               (ans-button-height button-text-pane-height))
      (apply #'make-instance button-type  
             :text (format nil "~A" text)
             :data data
             :selected nil
             :selection-callback 'my-button-panel-selection-callback
             :callback-type :item-interface
             ;;added
             :visible-min-height ans-button-height
             :visible-max-height ans-button-height
             ;;next only works for check buttons?
            ;;not needed? :retract-callback 'my-button-panel-retract-callback
             button-arglist)
      )
    ;;MAKE A BUTTON-TEXT-PANE FUNCTION
    ;;ddd
    (defun make-a-button-text-pane (text text-arglist 
                                         &key (ans-button-pane-height button-text-pane-height)) 
      ;; &optional
      ;;   fill-button-text-space-p)
      #|      (if fill-button-text-space-p
          (setf text (format nil "~A" text))
        (format nil "~%~A" text))|#
      (apply #'make-instance 'capi:rich-text-pane  
             :text (format nil "~A" text)
             :visible-min-height ans-button-pane-height
             :visible-max-height ans-button-pane-height
             text-arglist)
      )
    ;; (format nil "outer ~A" (format nil "INNER"))  = "outer INNER"                        :  
    
    ;;Make the :BEFORE METHOD TO MAKE THE BUTTONS and BUTTON-LAYOUT
    ;;ddd
    ;; (setf *mytestvar
    (eval
     `(defmethod capi:interface-display :before ((self  ,interface-name))
        (let
            ((button-list)
             (button-text-pane-list)
             (datalist-length (list-length ,button-data-list))
             (button-type 
              (if (equal ,selection-type :single-selection) 'capi:radio-button
                'capi:check-button))
            ;; (ans-button-height *button-text-pane-height)
             )
          ;;used for multi-selection items only
     ;;not needed aith check-buttons?     (defparameter *multi-selection-button-selection-list nil)

     (with-slots (,parent-layout-name) self
       ;;make the button-list and button-text-pane-list
       (loop 
        for data in  ,button-data-list
        for text in  ,button-text-list
        for n from 1 to datalist-length
        with button 
        with button-text-pane
        with ans-button-height
        with ans-button-pane-height
        do
        ;;added, check text length, if > one line, increase button height
        (cond
         ((<= (length text) ,max-ans-text-line-length)
          (setf ans-button-height  ,button-text-pane-height
                ans-button-pane-height  ,button-text-pane-height))
         (t ;;AAA
          (setf ans-button-height (* 2.0 ,button-text-pane-height)
                ans-button-pane-height  (* 2.0 ,button-text-pane-height))))

        (setf outx ans-button-height)
        (setf button (make-a-button n data ,button-arglist :button-type button-type 
                                    :ans-button-height ans-button-height)
              button-list (append button-list (list button))  
              ;;next used for retracting button selections in multi-selection 
              ;; makes list = ((button-inst 0 nil)....) 0 is num of times clicked upon
              ;;  nil means not selected (reset to nil on even num of clicks)
              #|     not needed with check buttons     *multi-selection-button-selection-list 
                   (append *multi-selection-button-selection-list (list (list button 0 nil)))|#
              button-text-pane (make-a-button-text-pane text ,button-text-pane-arglist
                                                        :ans-button-pane-height ans-button-pane-height)
              button-text-pane-list (append button-text-pane-list                                                                                                         (list button-text-pane)))
        ;;end loop,setf
        )
       ;;SSS 
       ;;DOES HEIGHT PROBLEM COME FROM TOO SHORT A PANE OR LAYOUT??  READ DOCS AND/OR TEST WITH SIMPLE TEXT-PANE, LAYOUT

       ;;make buttons so LAST IN LIST  is on TOP?
       (if  ,last-button-on-top-p
           (setf button-list (reverse button-list)
                 button-text-pane-list (reverse button-text-pane-list)))

       ;;MAKE ,BUTTON-LAYOUT-NAME INSTANCE AND PUT IN LAYOUT
       ;;To adjust height for text lines > 1;; added 2014-06
       #|            (if (> greatest-string-length button-text-pane-list *max-ans-text-width)
                (setf |#
            
       (setf  button-layout-inst  
              (apply  #'make-instance  'capi:column-layout 
                      :description  button-list ,button-layout-arglist))
       ;;make button-text-layout instance
       (setf button-text-layout-inst
             (apply #'make-instance 'capi:column-layout
                    :description button-text-pane-list ,button-text-layout-arglist))  
       ;;put into layout 
       (setf (capi:layout-description  ,parent-layout-name)
             (list button-layout-inst button-text-layout-inst)) 

       ;;(afout 'out (format nil " ,button-layout-inst=~A~%(eval button-layout-inst= ~A~%(capi:layout-description  ,parent-layout-name)= ~A~%"    ,button-layout-inst  (eval ,button-layout-inst )  (capi:layout-description  ,parent-layout-name) ))
#|       (setf  out6 (format nil "(quote ,parent-layout-name)=~A~%(eval parent-layout-name= ~A~%(capi:layout-description  ,parent-layout-name)= ~A~%"  (quote   ,parent-layout-name)  (eval ,parent-layout-name )  (capi:layout-description  ,parent-layout-name) ))|#
     
       ;;end with-slots,let, defmethod, eval
       ))))
    ;;end defun make-my-vertical-button-panel
    ))





(defparameter @SSS-DEBUG-DoubleVERT nil)
;;MAKE-MY-DOUBLE-VERTICAL-BUTTON-PANEL

;;SSS TRY USING ONLY ONE PANEL WITH BUTTONS IN TWO ROWS
;;
;;ddd

;;OLD--BEYOND REPAIR?? TO COMPLEX?/
#|(defun make-my-double-vertical-button-panel (parent-layout-sym1 
                                             parent-layout-sym2
                                      interface-name 
                                      button-text-list1  button-data-list1
                                      button-text-list2  button-data-list2
                                      &key   (close-interface-on-selection-p T)
                                      (selection-type 'single)
                                      button-arglist
                                      button-layout-arglist
                                      button-text-pane-arglist
                                      button-text-layout-arglist
                                      (last-button-on-top-p NIL)
                                      (send-datalist-to-instance-slot 'selected-item-datalist)            
                                      )    ;;&rest button-arglist) caused error
  "In U-capi-buttons-etc.lisp.  DOC may be less than 100% up to date. Makes a button layout, 'button-layout-name', which is a customized column layout of buttons that allows more options than the standard radio-button panel. Uses radio buttons as the buttons. All regular initargs to the layout is accessed via button-layout-arglist, and all radio-button initargs thru the &rest list. Starts with NO DEFAULT PRESELECTED BUTTON-a major change over capi.  Use to create buttons and add to interfaces. Makes a  layout and puts inside parent-layout. REQUIRES a pre-defined INTERFACE  a layout named parent-layout-sym. The arglists must be preceded by double quotes. EXAMPLE: (make-my-button-panel  'row-layout-1 'test-my-radio-buttons  '(list 'A 'B 'C)  '(list 10 20 30)  :button-layout-arglist   '(list :visible-min-width 200 etc)) ETC. Creates a GLOBAL VAR, (format nil \"*~A-selected-values-list\" parent-layout-sym) = (list selected-text selected-data selected-n[begins with 1]  selected-button-instance last-button-on-top-p) of last selected button. IF MULIPLE-SELECTION, RETURNS (list selected-text-LIST selected-data-LIST selected-n-LIST[begins with 1]  selected-button-instance-LIST  last-button-on-top-p). Single selection also calls append-my-vertical-button-panel-single-selection-callback multiple-selection calls append-my-vertical-button-panel-multi-selection-callback"
  (let
      ((global-selected-values-list (my-make-symbol 
                                     (format nil "*~A-selected-values-list" parent-layout-sym1)))
       (button-list1)
       (button-layout-inst1)
       (button-text-pane-list1)
       (button-text-layout-inst1)
       (button-list2)
       (button-layout-inst2)
       (button-text-pane-list2)
       (button-text-layout-inst2)
       (button-panel-list '(button-list1 button-list2))
    ;;   (button-layout-insts '(button-layout-inst1 button-layout-inst2))
       (button-text-panes '(button-text-pane-list1 button-text-pane-list2))
       (button-text-layout-insts '(button-text-layout-inst1 button-text-layout-inst2))   
       (parent-layout-syms (list parent-layout-sym1 parent-layout-sym2))
       )

    (cond
     ((or  *single-selection-item (member selection-type
            (list 'single  :single  :single-selection "single" "SINGLE" "Single"  :test 'equal)))
      (setf selection-type :single-selection))
     (t (setf selection-type :multiple-selection)))

   (setf out1  (format nil "IN MAKE-MY-VERTICAL-BUTTON-PANEL~%parent-layout-syms= ~A~% interface-name= ~A~%button-text-list1= ~A~%  button-data-list1 = ~A~%selection-type= ~A~% button-arglist= ~A~%button-layout-arglist= ~A~%button-text-pane-arglist= ~A~%button-text-layout-arglist= ~A~% send-datalist1-to-instance-slot= ~A~%"parent-layout-syms  interface-name  button-text-list1  button-data-list1  selection-type button-arglist button-layout-arglist button-text-pane-arglist button-text-layout-arglist send-datalist-to-instance-slot ))

    ;;Make the SELECTION CALLBACK
    ;;ddd
    #|(eval 
     `(defun my-button-panel-selection-callback (item interface)  
        "In U-capi-buttons-etc.lisp, MUST use with my-button-panels.  MODIFY THIS CALLBACK to get data or actions from the button selection."

        (afout 'out (format nil "FIRING my-button-panel-selection-callback"))
        (let
            ((button-value)
             (button-data)
             (selected-text-n)
             (selected-text)
             (selected-data)
             (selected-button)
             (selection-type1 ,selection-type)
       (button-panel-list) ;; '(button-list1 button-list2))
       (button-layout-insts) ;; '(button-text-layout-inst1 button-text-layout-inst2))
       (button-text-panes) ;; '(button-text-pane-list1 button-text-pane-list2))
       (button-text-layout-insts) ;; '(button-text-layout-inst1 button-text-layout-inst2))   
             ;;  (datalist-instance)
             ;;   (datalist-slot (quote ,send-datalist-to-instance-slot))
             (last-button-on-top-p1 ,last-button-on-top-p)
             (button-layout1)
             (button-layout2)
             )
      ;;   (setf out3  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-syms) ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-syms)))
       (loop
        for button-list in button-panel-list
        for button-text-list in `(,button-text-list1 ,button-text-list2)
        for button-text-layout-inst in button-layout-insts
        for button-text-pane-list in button-text-panes
        for button-text-layout-inst in button-text-layout-insts
        ;;with 
        do
             (afout 'out (format nil "selection-type1= ~A~%" selection-type1))
          (with-slots (,parent-layout-sym1 ,parent-layout-sym2 ,send-datalist-to-instance-slot) interface 
            #|(afout 'out (format nil ",parent-layout-sym1= ~A~% ,send-datalist-to-instance-slot= ~A~%  item= ~A~%, ",parent-layout-sym1 ,send-datalist-to-instance-slot   item  ))|#
         ;;   (setf out4  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-sym1) ~A~%item= ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-sym1) item))
           
            ;;TO CHANGE SELECTION, MUST UNSET PREVOUSLY SELECTED BUTTONS
            (setf  BUTTON-LAYOUT1
                   (CAR (capi:layout-description ,parent-layout-sym1)))    
            (setf  BUTTON-LAYOUT2 
                   (CAR (capi:layout-description ,parent-layout-sym2)))
            (cond
             ((equal selection-type1 :multiple-selection)
              ;;DON'T DO ANYTHING.  CALLBACK IS FROM GO BUTTON INSTEAD
              )
             ((equal selection-type1 :single-selection)
                 (loop
                  for button in (capi:layout-description BUTTON-LAYOUT) ;; ,button-layout-name)
                  unless (eq item button) ;;  (or (equal item button) (null item))
                  do 
                  (setf (capi:item-selected button) nil)
                  ;;(afout 'out (format nil "item= ~A~%button= ~A (capi:item-selected button)= ~A"  item button (capi:item-selected button)))
                  ;;end loop, if
                  
            ;;FIND SELECTED BUTTON N, TEXT, DATA, and BUTTON-INSTANCE
            (setf selected-n  (my-make-symbol (capi:item-text item))
                  selected-text (nth (- selected-n 1) ,button-text-list)
                  selected-data (capi:item-data item)
                  selected-button item)   
            ;;end inner loop
            )
             
            ;;SET GLOBAL VARIABLE TO DATALIST VALUES
            (setf ,global-selected-values-list 
                  (list selected-text selected-data selected-n  selected-button last-button-on-top-p1))
            ;; SET SLOT-VALUE IN QUESTION FRAME INSTANCE TO DATALIST
            (cond
             ((quote ,send-datalist-to-instance-slot)
              (setf (slot-value interface (quote ,send-datalist-to-instance-slot ))
                    (list selected-text selected-data selected-n  
                          selected-button last-button-on-top-p1)))
             (t nil))
            ;;   (afout 'out (format nil "IN CALLBACK, (slot-value interface (quote ,send-datalist-to-instance-slot) )= ~A~%  (quote ,send-datalist-to-instance-slot)= ~a~%" (slot-value interface (quote ,send-datalist-to-instance-slot) )   (quote ,send-datalist-to-instance-slot)))
            ;;CLOSE THE INTERFACE INSTANCE ONCE SELECTION MADE?
            (if ,close-interface-on-selection-p
                (capi:destroy interface))

            ;;end single-selection
            )
             (t nil))
            ;;IF *CALL-SHAQ-CALLBACK-P, THEN DATA PROCESSING AND
            ;;   CALLING NEXT FRAME
            (cond
             ((equal selection-type1 :single-selection)
                (append-my-vertical-button-panel-single-selection-callback item  interface))
             ((equal selection-type1 :multiple-selection)
              ;;DO NOTHING SINCE USING CHECK-BUTTONS
              ;;AAA
              ;;Must deselect button if checked even number of times
              
             ;; data actions, etc initiated by GO Button for Multi-selection items
             ;;(append-my-vertical-button-panel-multi-selection-callback item  interface)
             ;;end multi-selection type
              )
             (t nil))
            ;;end with-slots, outer loop           )

        ;;end  defun callback,eval
        )))))|#

    ;;SSS START HERE DEBUGGING  (CAN REALLY USE THIS ON RADIO BUTTONS?)  
    ;;RETRACT-CALLBACK TO UNMARK BUTTON ON MULTI-SEL
    ;;ddd
    ;;APPARENTLY RADIO-BUTTON DOESN'T CALL RETRACT-CALLBACK
#|    (eval
     `(defun my-button-panel-retract-callback (item interface)
        (cond
         ((equal selection-type1 :single-selection)
          ;;DO NOTHING TO RETRACT, WILL RETRACT WHEN CHOOSE OTHER
          (setf out4(format nil "IN my-button-panel-retract-callback, ITEM= ~A" item))
          )
         (t
          (setf out4(format nil "IN my-button-panel-retract-callback, ITEM= ~A" item))
;;(setf (capi:item-selected button) nil)
          (setf (capi:item-selected  item) nil)))
        ))
|#
    ;; MAKE BUTTON FUNCTION
    ;;ddd
    (defun make-a-button (text data button-arglist &key (button-type 'capi:radio-button))
      (apply #'make-instance button-type  
             :text (format nil "~A" text)
             :data data
             :selected nil
             :selection-callback 'my-button-panel-selection-callback
             :callback-type :item-interface
             ;;next only works for check buttons?
            ;;not needed? :retract-callback 'my-button-panel-retract-callback
             button-arglist)
      )
    ;;MAKE A BUTTON-TEXT-PANE FUNCTION
    ;;ddd
    (defun make-a-button-text-pane (text text-arglist) ;; &optional
      ;;   fill-button-text-space-p)
      #|      (if fill-button-text-space-p
          (setf text (format nil "~A" text))
        (format nil "~%~A" text))|#
      (apply #'make-instance 'capi:rich-text-pane  
             :text (format nil "~A" text)
             text-arglist)
      )
    ;; (format nil "outer ~A" (format nil "INNER"))  = "outer INNER"                        :  
    
    ;;Make the :BEFORE METHOD TO MAKE THE BUTTONS and BUTTON-LAYOUT
    ;;ddd
    ;; (setf *mytestvar
    (eval
     `(defmethod capi:interface-display :before ((self  ,interface-name))
        (let
            ((button-list1 ,button-list1)
             (button-list2 ,button-list2)
             (button-text-pane-list1)
             (button-text-pane-list2)
             (datalist1-length (list-length ,button-data-list1))
             (datalist2-length (list-length ,button-data-list2))
             (button-type 
              (if (equal ,selection-type :single-selection) 'capi:radio-button
                'capi:check-button))
       (parent-layout-syms (list ,parent-layout-sym1 ,parent-layout-sym2))
       (button-panel-list `(,button-list1 ,button-list2))
       (button-layout-insts `(,button-layout-inst1 ,button-layout-inst2))
       (button-text-panes '(button-text-pane-list1 button-text-pane-list2))
       (button-text-layout-insts '(button-text-layout-inst1 button-text-layout-inst2)) 
             )
          ;;used for multi-selection items only
     ;;not needed aith check-buttons?     (defparameter *multi-selection-button-selection-list nil)
     ;;;AAA
        do
        (with-slots (list ,parent-layout-sym1 ,parent-layout-sym2) self
          ;;make the button-list and button-text-pane-list
          (break "break 0")
          (loop ;;for each button panel
                for parent-layout-name in `(,parent-layout-sym1 ,parent-layout-sym2)
                for button-list in button-panel-list
                for button-text-layout-inst in button-layout-insts
                for button-text-pane-list in button-text-panes
                for button-text-layout-inst in button-text-layout-insts
                ;;needed
                for button-data-list in `(,button-data-list1 ,button-data-list2)
                for text in `(,button-text-list1 ,button-text-list2)
                do
                (setf out2 (format nil "parent-layout-name= ~A~% button-list= ~A~% button-text-layout-inst= ~A~% button-text-pane-list= ~A~%" parent-layout-name  button-list button-text-layout-inst button-text-pane-list))
                (loop ;;for each button
                      for data in  button-data-list
                      for text in  button-text-list
                      for n from 1 to datalist-length
                      with button
                      with button-text-pane
                      do
                      (setf button (make-a-button n data ,button-arglist :button-type button-type)
                            button-list (append button-list (list button))  
                            ;;next used for retracting button selections in multi-selection 
                            ;; makes list = ((button-inst 0 nil)....) 0 is num of times clicked upon
                            ;;  nil means not selected (reset to nil on even num of clicks)
                            #|     not needed with check buttons     *multi-selection-button-selection-list 
                   (append *multi-selection-button-selection-list (list (list button 0 nil)))|#
                            button-text-pane 
                            (make-a-button-text-pane text ,button-text-pane-arglist)    
                            button-text-pane-list (append button-text-pane-list 
                                                          (list button-text-pane)))
                      (afout 'out (format nil "FOR MAKE-A-BUTTON n= ~A data= ~A~% button-text-pane TEXT= ~A~% "n  data text))
                      (setf out3 (format nil "FOR MAKE-A-BUTTON n= ~A data= ~A~% button-text-pane TEXT= ~A~% "n  data text))
                      ;;end loop,setf
                      )

                ;;make buttons so LAST IN LIST  is on TOP?
                (if  ,last-button-on-top-p
                    (setf button-list (reverse button-list)
                          button-text-pane-list (reverse button-text-pane-list)))

                ;;MAKE ,BUTTON-LAYOUT-NAME INSTANCE AND PUT IN LAYOUT
                (break "break 1")
                (setf  button-layout-inst  
                       (apply  #'make-instance  'capi:column-layout 
                               :description  button-list ,button-layout-arglist))
                ;;make button-text-layout instance
                (setf button-text-layout-inst
                      (apply #'make-instance 'capi:column-layout
                             :description button-text-pane-list ,button-text-layout-arglist))  
                ;;put into layout 
                (setf (capi:layout-description  parent-layout-name) ;;was comma
                      (list button-layout-inst button-text-layout-inst)) 
                (break "2")
                (afout 'out (format nil " ,button-layout-inst=~A~%(eval button-layout-inst= ~A~%(capi:layout-description parent-layout-name)= ~A~%"    ,button-layout-inst  (eval ,button-layout-inst )  (capi:layout-description parent-layout-name) ))
                  (setf  out6 (format nil "(quote parent-layout-name)=~A~%(eval parent-layout-name= ~A~%(capi:layout-description  parent-layout-name)= ~A~%"  (quote   parent-layout-name)  (eval parent-layout-name )  (capi:layout-description  parent-layout-name) ))

                ;;end outer loop
                )
     
          ;;end with-slots,let, defmethod, eval
          ))))
    ;;end defun make-double-my-vertical-button-panel
    ))|#




#| WORKING PRE-MULTIPLE-SELECTION MODIFICATIONS
(defun make-my-vertical-button-panel (parent-layout-name
                                      interface-name 
                                      button-text-list  button-data-list            
                                      &key   (close-interface-on-selection-p T)
                                      button-arglist
                                      button-layout-arglist
                                      button-text-pane-arglist
                                      button-text-layout-arglist
                                      (last-button-on-top-p NIL)
                                      (send-datalist-to-instance-slot 'selected-item-datalist)            
                                      )    ;;&rest button-arglist) caused error
  "In U-capi-buttons-etc.lisp.  DOC may be less than 100% up to date. Makes a button layout, 'button-layout-name', which is a customized column layout of buttons that allows more options than the standard radio-button panel. Uses radio buttons as the buttons. All regular initargs to the layout is accessed via button-layout-arglist, and all radio-button initargs thru the &rest list. Starts with NO DEFAULT PRESELECTED BUTTON-a major change over capi.  Use to create buttons and add to interfaces. Makes a  layout and puts inside parent-layout. REQUIRES a pre-defined INTERFACE  a layout named parent-layout-name. The arglists must be preceded by double quotes. EXAMPLE: (make-my-button-panel  'row-layout-1 'test-my-radio-buttons  '(list 'A 'B 'C)  '(list 10 20 30)  :button-layout-arglist   '(list :visible-min-width 200 etc)) ETC. Creates a GLOBAL VAR, (format nil \"*~A-selected-values-list\" parent-layout-name) = (list selected-text selected-data selected-n[begins with 1]  selected-button-instance last-button-on-top-p) of last selected button."
  (let
      ((global-selected-values-list (my-make-symbol 
                                     (format nil "*~A-selected-values-list" parent-layout-name)))
       (button-list)
       (button-layout-inst)
       (button-text-pane-list)
       (button-text-layout-inst)
       )
  ;;  (setf out1  (format nil "A send-datalist-to-instance-slot= ~A~%parent-layout-name ~A~%" send-datalist-to-instance-slot parent-layout-name))

    ;;Make the SELECTION CALLBACK
    (eval 
     `(defun my-button-panel-selection-callback (item interface)  
        "In U-capi-buttons-etc.lisp, MUST use with my-button-panels.  MODIFY THIS CALLBACK to get data or actions from the button selection."
        (let
            ((button-value)
             (button-data)
             (selected-text-n)
             (selected-text)
             (selected-data)
             (selected-button)
             (button-layout)
             (button-text-layout)
             ;;  (datalist-instance)
             ;;   (datalist-slot (quote ,send-datalist-to-instance-slot))
             (last-button-on-top-p1 ,last-button-on-top-p)
             )
       ;;   (setf out3  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-name) ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-name)))

          (with-slots (,parent-layout-name ,send-datalist-to-instance-slot) interface 
            #|(afout 'out (format nil ",parent-layout-name= ~A~% ,send-datalist-to-instance-slot= ~A~%  item= ~A~%, ",parent-layout-name ,send-datalist-to-instance-slot   item  ))|#
         ;;   (setf out4  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-name) ~A~%item= ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-name) item))
           
            ;;TO CHANGE SELECTION, MUST UNSET PREVOUSLY SELECTED BUTTONS
            (setf  BUTTON-LAYOUT (CAR (capi:layout-description ,parent-layout-name)))    
            (loop
             for button in (capi:layout-description BUTTON-LAYOUT) ;; ,button-layout-name)
             unless (eq item button) ;;  (or (equal item button) (null item))
             do 
             (setf (capi:item-selected button) nil)
             ;;(afout 'out (format nil "item= ~A~%button= ~A (capi:item-selected button)= ~A"  item button (capi:item-selected button)))
             ;;end loop
             )
            ;;FIND SELECTED BUTTON N, TEXT, DATA, and BUTTON-INSTANCE
            (setf selected-n  (my-make-symbol (capi:item-text item))
                  selected-text (nth (- selected-n 1) ,button-text-list)
                  selected-data (capi:item-data item)
                  selected-button item)                  
            ;;SET GLOBAL VARIABLE TO DATALIST VALUES
            (setf ,global-selected-values-list 
                  (list selected-text selected-data selected-n  selected-button last-button-on-top-p1))
            ;; SET SLOT-VALUE IN QUESTION FRAME INSTANCE TO DATALIST
            (cond
             ((quote ,send-datalist-to-instance-slot)
              (setf (slot-value interface (quote ,send-datalist-to-instance-slot ))
                    (list selected-text selected-data selected-n  
                          selected-button last-button-on-top-p1)))
             (t nil))
            ;;   (afout 'out (format nil "IN CALLBACK, (slot-value interface (quote ,send-datalist-to-instance-slot) )= ~A~%  (quote ,send-datalist-to-instance-slot)= ~a~%" (slot-value interface (quote ,send-datalist-to-instance-slot) )   (quote ,send-datalist-to-instance-slot)))
            ;;CLOSE THE INTERFACE INSTANCE ONCE SELECTION MADE?
            (if ,close-interface-on-selection-p
                (capi:destroy interface))

            ;;IF *CALL-SHAQ-CALLBACK-P, THEN DATA PROCESSING AND
            ;;   CALLING NEXT FRAME
            (if *call-shaq-question-callback-p
                (append-my-vertical-button-panel-callback item  interface))
            ))
        ;;end defun callback,eval
        ))

    ;; MAKE BUTTON FUNCTION
    (defun make-a-button (text data button-arglist )
      (apply #'make-instance 'capi:radio-button  
             :text (format nil "~A" text)
             :data data
             :selected nil
             :selection-callback 'my-button-panel-selection-callback
             :callback-type :item-interface
             button-arglist)
      )
    ;;MAKE A BUTTON-TEXT-PANE FUNCTION
    (defun make-a-button-text-pane (text text-arglist) ;; &optional
      ;;   fill-button-text-space-p)
      #|      (if fill-button-text-space-p
          (setf text (format nil "~A" text))
        (format nil "~%~A" text))|#
      (apply #'make-instance 'capi:rich-text-pane  
             :text (format nil "~A" text)
             text-arglist)
      )
    ;; (format nil "outer ~A" (format nil "INNER"))  = "outer INNER"                        :  
    
    ;;Make the :BEFORE METHOD TO MAKE THE BUTTONS and BUTTON-LAYOUT
    ;; (setf *mytestvar
    (eval
     `(defmethod capi:interface-display :before ((self  ,interface-name))
        (let
            ((button-list)
             (button-text-pane-list)
             (datalist-length (list-length ,button-data-list))
             )
          (with-slots (,parent-layout-name) self
            ;;make the button-list and button-text-pane-list
            (loop 
             for data in  ,button-data-list
             for text in  ,button-text-list
             for n from 1 to datalist-length
             with button
             with button-text-pane
             do
             (setf button (make-a-button n data ,button-arglist)
                   button-list (append button-list (list button))                        
                   button-text-pane 
                   (make-a-button-text-pane text ,button-text-pane-arglist)    
                   button-text-pane-list (append button-text-pane-list 
                                                 (list button-text-pane)))
             ;;end loop,setf
             )

            ;;make buttons so LAST IN LIST  is on TOP?
            (if  ,last-button-on-top-p
                (setf button-list (reverse button-list)
                      button-text-pane-list (reverse button-text-pane-list)))

            ;;MAKE ,BUTTON-LAYOUT-NAME INSTANCE AND PUT IN LAYOUT
            (setf  button-layout-inst  
                   (apply  #'make-instance  'capi:column-layout 
                           :description  button-list ,button-layout-arglist))
            ;;make button-text-layout instance
            (setf button-text-layout-inst
                  (apply #'make-instance 'capi:column-layout
                         :description button-text-pane-list ,button-text-layout-arglist))  
            ;;put into layout 
            (setf (capi:layout-description  ,parent-layout-name)
                  (list button-layout-inst button-text-layout-inst)) 

            ;;(afout 'out (format nil " ,button-layout-inst=~A~%(eval button-layout-inst= ~A~%(capi:layout-description  ,parent-layout-name)= ~A~%"    ,button-layout-inst  (eval ,button-layout-inst )  (capi:layout-description  ,parent-layout-name) ))
;;  (setf  out6 (format nil "(quote ,parent-layout-name)=~A~%(eval parent-layout-name= ~A~%(capi:layout-description  ,parent-layout-name)= ~A~%"  (quote   ,parent-layout-name)  (eval ,parent-layout-name )  (capi:layout-description  ,parent-layout-name) ))
     
            ;;end with-slots,let, defmethod, eval
            ))))
    ;;end defun make-my-vertical-button-panel
    ))|#
#|
;;WORKING VERSION WITHOUT CALLBACK ABIILITY TO CALL NEXT FRAME
(defun make-my-vertical-button-panel (parent-layout-name
                                      interface-name 
                                      button-text-list  button-data-list            
                                      &key   (close-interface-on-selection-p T)
                                      button-arglist
                                      button-layout-arglist
                                      button-text-pane-arglist
                                      button-text-layout-arglist
                                      (last-button-on-top-p NIL)
                                      (send-datalist-to-instance-slot 'selected-item-datalist)            
                                      )    ;;&rest button-arglist) caused error
  "In U-capi-buttons-etc.lisp.  DOC may be less than 100% up to date. Makes a button layout, 'button-layout-name', which is a customized column layout of buttons that allows more options than the standard radio-button panel. Uses radio buttons as the buttons. All regular initargs to the layout is accessed via button-layout-arglist, and all radio-button initargs thru the &rest list. Starts with NO DEFAULT PRESELECTED BUTTON-a major change over capi.  Use to create buttons and add to interfaces. Makes a  layout and puts inside parent-layout. REQUIRES a pre-defined INTERFACE  a layout named parent-layout-name. The arglists must be preceded by double quotes. EXAMPLE: (make-my-button-panel  'row-layout-1 'test-my-radio-buttons  '(list 'A 'B 'C)  '(list 10 20 30)  :button-layout-arglist   '(list :visible-min-width 200 etc)) ETC. Creates a GLOBAL VAR, (format nil \"*~A-selected-values-list\" parent-layout-name) = (list selected-text selected-data selected-n[begins with 1]  selected-button-instance last-button-on-top-p) of last selected button."
  (let
      ((global-selected-values-list (my-make-symbol 
                                     (format nil "*~A-selected-values-list" parent-layout-name)))
       (button-list)
       (button-layout-inst)
       (button-text-pane-list)
       (button-text-layout-inst)
       )
  ;;  (setf out1  (format nil "A send-datalist-to-instance-slot= ~A~%parent-layout-name ~A~%" send-datalist-to-instance-slot parent-layout-name))

    ;;Make the SELECTION CALLBACK
    (eval 
     `(defun my-button-panel-selection-callback (item interface)  
        "In U-capi-buttons-etc.lisp, MUST use with my-button-panels.  MODIFY THIS CALLBACK to get data or actions from the button selection."
        (let
            ((button-value)
             (button-data)
             (selected-text-n)
             (selected-text)
             (selected-data)
             (selected-button)
             (button-layout)
             (button-text-layout)
             ;;  (datalist-instance)
             ;;   (datalist-slot (quote ,send-datalist-to-instance-slot))
             (last-button-on-top-p1 ,last-button-on-top-p)
             )
          (setf out3  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-name) ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-name)))

          (with-slots (,parent-layout-name ,send-datalist-to-instance-slot) interface 
            #|(afout 'out (format nil ",parent-layout-name= ~A~% ,send-datalist-to-instance-slot= ~A~%  item= ~A~%, ",parent-layout-name ,send-datalist-to-instance-slot   item  ))|#
            (setf out4  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-name) ~A~%item= ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-name) item))
           
            ;;TO CHANGE SELECTION, MUST UNSET PREVOUSLY SELECTED BUTTONS
            (setf  BUTTON-LAYOUT (CAR (capi:layout-description ,parent-layout-name)))    
            (loop
             for button in (capi:layout-description BUTTON-LAYOUT) ;; ,button-layout-name)
             unless (eq item button) ;;  (or (equal item button) (null item))
             do 
             (setf (capi:item-selected button) nil)
             ;;(afout 'out (format nil "item= ~A~%button= ~A (capi:item-selected button)= ~A"  item button (capi:item-selected button)))
             ;;end loop
             )
            ;;FIND SELECTED BUTTON N, TEXT, DATA, and BUTTON-INSTANCE
            (setf selected-n  (my-make-symbol (capi:item-text item))
                  selected-text (nth (- selected-n 1) ,button-text-list)
                  selected-data (capi:item-data item)
                  selected-button item)                  
            ;;SET GLOBAL VARIABLE TO DATALIST VALUES
            (setf ,global-selected-values-list 
                  (list selected-text selected-data selected-n  selected-button last-button-on-top-p1))
            ;; SET SLOT-VALUE IN QUESTION FRAME INSTANCE TO DATALIST
            (cond
             ((quote ,send-datalist-to-instance-slot)
              (setf (slot-value interface (quote ,send-datalist-to-instance-slot ))
                    (list selected-text selected-data selected-n  
                          selected-button last-button-on-top-p1)))
             (t nil))
            ;;   (afout 'out (format nil "IN CALLBACK, (slot-value interface (quote ,send-datalist-to-instance-slot) )= ~A~%  (quote ,send-datalist-to-instance-slot)= ~a~%" (slot-value interface (quote ,send-datalist-to-instance-slot) )   (quote ,send-datalist-to-instance-slot)))

            ;;CLOSE THE INTERFACE INSTANCE ONCE SELECTION MADE?
            (if ,close-interface-on-selection-p
                (capi:destroy interface))
            ))
        ;;end defun callback,eval
        ))

    ;; MAKE BUTTON FUNCTION
    (defun make-a-button (text data button-arglist )
      (apply #'make-instance 'capi:radio-button  
             :text (format nil "~A" text)
             :data data
             :selected nil
             :selection-callback 'my-button-panel-selection-callback
             :callback-type :item-interface
             button-arglist)
      )
    ;;MAKE A BUTTON-TEXT-PANE FUNCTION
    (defun make-a-button-text-pane (text text-arglist) ;; &optional
      ;;   fill-button-text-space-p)
      #|      (if fill-button-text-space-p
          (setf text (format nil "~A" text))
        (format nil "~%~A" text))|#
      (apply #'make-instance 'capi:rich-text-pane  
             :text (format nil "~A" text)
             text-arglist)
      )
    ;; (format nil "outer ~A" (format nil "INNER"))  = "outer INNER"                        :  
    
    ;;Make the :BEFORE METHOD TO MAKE THE BUTTONS and BUTTON-LAYOUT
    ;; (setf *mytestvar
    (eval
     `(defmethod capi:interface-display :before ((self  ,interface-name))
        (let
            ((button-list)
             (button-text-pane-list)
             (datalist-length (list-length ,button-data-list))
             )
          (with-slots (,parent-layout-name) self
            ;;make the button-list and button-text-pane-list
            (loop 
             for data in  ,button-data-list
             for text in  ,button-text-list
             for n from 1 to datalist-length
             with button
             with button-text-pane
             do
             (setf button (make-a-button n data ,button-arglist)
                   button-list (append button-list (list button))                        
                   button-text-pane 
                   (make-a-button-text-pane text ,button-text-pane-arglist)    
                   button-text-pane-list (append button-text-pane-list 
                                                 (list button-text-pane)))
             ;;end loop,setf
             )

            ;;make buttons so LAST IN LIST  is on TOP?
            (if  ,last-button-on-top-p
                (setf button-list (reverse button-list)
                      button-text-pane-list (reverse button-text-pane-list)))

            ;;MAKE ,BUTTON-LAYOUT-NAME INSTANCE AND PUT IN LAYOUT
            (setf  button-layout-inst  
                   (apply  #'make-instance  'capi:column-layout 
                           :description  button-list ,button-layout-arglist))
            ;;make button-text-layout instance
            (setf button-text-layout-inst
                  (apply #'make-instance 'capi:column-layout
                         :description button-text-pane-list ,button-text-layout-arglist))  
            ;;put into layout 
            (setf (capi:layout-description  ,parent-layout-name)
                  (list button-layout-inst button-text-layout-inst)) 

            ;;(afout 'out (format nil " ,button-layout-inst=~A~%(eval button-layout-inst= ~A~%(capi:layout-description  ,parent-layout-name)= ~A~%"    ,button-layout-inst  (eval ,button-layout-inst )  (capi:layout-description  ,parent-layout-name) ))
  ;;SSS THE RESULTS SEEM TO BE NORMAL =
#|"(quote ,parent-layout-name)=ANSWER-BUTTON-LAYOUT
(eval parent-layout-name= #<CAPI:COLUMN-LAYOUT ANSWER-BUTTON-LAYOUT  (1,2) 23657B17>
(capi:layout-description  ,parent-layout-name)= (#<CAPI:COLUMN-LAYOUT  (1,10) 200E3867> #<CAPI:COLUMN-LAYOUT  (1,10) 200E26D7>)
"|#
  (setf  out6 (format nil "(quote ,parent-layout-name)=~A~%(eval parent-layout-name= ~A~%(capi:layout-description  ,parent-layout-name)= ~A~%"  (quote   ,parent-layout-name)  (eval ,parent-layout-name )  (capi:layout-description  ,parent-layout-name) ))
     
            ;;end with-slots,let, defmethod, eval
            ))))
    ;;end defun make-my-vertical-button-panel
    ))|#

;;TEST
;; *QA-PARENT-ROW-LAYOUT-1-SELECTED-VALUES-LIST
;;  works= ("CCC" 300 3 #<CAPI:RADIO-BUTTON "3" 23060D27> T)
;;  (slot-value *mb-testinst 'selected-item-datalist)
;; works = ("This is a longer answer to test the width of the text answer panel" 100 1 #<CAPI:RADIO-BUTTON "1" 24AE2F5B> T)
;;
(defun testvb ()
  (setf out nil)
  (capi:define-interface test-my-vertical-button-panel ()
    ;;sets the button/item data values
    ((selected-item-datalist                                  
      :initarg :selected-item-datalist
      :accessor selected-item-datalist
      :initform nil
      :documentation "Store selected button-result list")
     )
    (:layouts
     (qa-parent-row-layout-1
      capi:row-layout
      ()
      :background :pink
      :visible-min-width 400
      :visible-max-width 450
      :visible-min-height 440
      )
     )
    (:default-initargs
     :title "TEST MY INTERFACE"
     :visible-min-width 600
     :visible-min-height 500
     :background :yellow
     :internal-border 20
     ))
  (defun button-press-callback (data interface)
    (setf *test-result-data data)
    (capi:destroy interface)
    )

  ;;SSS CHANGE -PANEL BACK 
  (make-my-vertical-button-panel 'qa-parent-row-layout-1
                                 'test-my-vertical-button-panel 
                                 '(list "This is a longer answer to test the width of the text answer panel" "BBB" "CCC")
                                 '(list 100 200 300) 
                                 :close-interface-on-selection-p nil
                                 :last-button-on-top-p nil
                                 ;; :fill-button-text-space-p t
                                :button-layout-arglist  (quote (list :visible-max-width 10
                                                                      ;;  :visible-min-height 300
                                                                      :background :green
                                                                      :title "  ";;controls button position
                                                                      :title-gap 0  ;;no effect?
                                                                      ;;no effect    :x 0  :y 0
                                                                      :internal-border 0 ;;important
                                                                      :border nil
                                                                      :title-adjust :center
                                                                      ;;no effect        :title-position :bottom
#|                                                                      :title-font (gp:make-font-description 
                                                                                   :family "Times New Roman"
                                                                                   :weight :normal
                                                                                   :slant :italic :size 12)|#
                                                                      ))
                                 :button-arglist (quote   (list                     
                                                           :title nil  ;;adjust left gap within button area
                                                           :title-gap 0  ;;gap betw end of title and button
                                                           :background :light-blue
                                                           :internal-border 0 ;;no effect??
                                                           :visible-max-width 10
                                                           :external-max-height 25
                                                        #| :font (gp:make-font-description 
                                                                  :family "Times New Roman"
                                                                  :weight :bold
                                                                  :slant :italic :size 11)|#
                                                           ))
                                 :button-text-pane-arglist
                                 (quote (list
                                         :background :lightyellow
                                         :internal-border nil
                                         :visible-min-width 300
                                         :visible-max-height 25
                                         ;;  :external-max-height 30
                                        :font   (gp:make-font-description 
                                                  :family "Times New Roman"
                                                  :weight :normal :size 11)
                                         ;; :slant :normal )
                                         ))
                                 :button-text-layout-arglist
                                 (quote (list
                                         :background :purple
                                         :internal-border nil
                                         :visible-min-width nil
                                         :visible-min-height nil
                                         ;;  :visible-max-height 30
                                         :title "Button-text-layout TITLE"
                                         :title-position  :center
                                         :title-font   (gp:make-font-description 
                                                        :family "Times New Roman"
                                                        :weight :bold
                                                        :slant :italic :size 11)
                                         ))                                                   

                                 ;;end make-my-verticalbutton-panel
                                 )
  (capi:display (setf *mb-testinst (make-instance 'test-my-vertical-button-panel)))  
  ;;end test function
  )
;;TEST
;;*button-row-layout-1-selected-values-list
;;  ;;  SSS DEBUGGING WHY CALLBACK NOT SETTING THE SLOT VALUE
;;  (slot-value *mb-testinst 'selected-item-datalist)
;; (setf (slot-value *mb-testinst 'selected-item-datalist) *button-row-layout-1-selected-values-list)








#|
;;MAKE-MY-BUTTON-PANEL
;;WORKS, BUT ONLY PROBLEM, MAY NOT BE PORTABLE TO NON-WINDOWS VERSIONS
;;ddd
(defun make-my-button-panel (button-layout-name parent-layout-name interface-name 
                                               button-text-list  button-data-list            
                                               &key   (close-interface-on-selection-p T)
                                                (button-layout-type 
                                                (quote (quote capi:column-layout)))
                                               button-arglist
                                               button-layout-arglist
                                               )    ;;&rest button-arglist) caused error
  "Makes a button 'button-layout-name' which is a customized row or column layout of buttons that allows more options than the standard radio-button panel. Uses radio buttons as the buttons. All regular initargs to the layout is accessed via button-layout-arglist, and all radio-button initargs thru the &rest list. Starts with NO DEFAULT PRESELECTED BUTTON-a major change over capi.  Use to create buttons and add to interfaces. Returns a  layout. REQUIRES a pre-defined INTERFACE  a layout named button-layout-name. The arglists must be preceded by double quotes. EXAMPLE: (make-my-button-panel 'my-button-panel-1 'row-layout-1 'test-radio-buttons  '(list \"A\" \"B\" \"C\")  '(list 10 20 30)  :button-layout-arglist  (quote (quote (:visible-min-width 200 etc))) "
  (let
      ((global-selected-values-list (my-make-symbol 
                                     (format nil "*~A-selected-values-list" button-layout-name)))
        (button-list)
       )
;;SSS BUTTONS WON'T DESELECT IF CHOOSE MORE THAN ONE--OK IF USE DESTROY INTERFACE, BUT NOT OTHERWISE!!
   ;;Make the SELECTION CALLBACK
    (eval 
     `(defun my-button-panel-selection-callback (item interface)  
        "In U-capi-buttons-etc.lisp, MUST use with my-button-panels.  MODIFY THIS CALLBACK to get data or actions from the button selection."
        (let
            ((button-value)
             (button-data)
             (selected-text)
             (selected-data)
             (selected-button)
             (button-layout)
               )
          (with-slots (,parent-layout-name ) interface
            ;;(afout 'out (format nil "button-layout= ~A~%item= ~A~%,",button-layout-name  item  ))
            ;;TEST TTT
            (setf BUTTON-LAYOUT (CAR (capi:layout-description ,parent-layout-name)))
            

#|  (with-slots (panel) interface
    (loop for button in (capi:layout-description panel)|#

            (loop
             for button in (capi:layout-description BUTTON-LAYOUT) ;; ,button-layout-name)
             unless (eq item button) ;;  (or (equal item button) (null item))
             do 
             (setf (capi:item-selected button) nil)
             ;;(afout 'out (format nil "item= ~A~%button= ~A (capi:item-selected button)= ~A"  item button (capi:item-selected button)))
             ;;end loop
             )
            ;;added
            (setf selected-text (capi:item-text item)
                  selected-data (capi:item-data item)
                  selected-button item)

            (setf ,global-selected-values-list 
                  (list selected-text selected-data  selected-button))

            (if ,close-interface-on-selection-p
                (capi:destroy interface))
            ))
     ;;end defun callback,eval
     ))

    ;; Make button function
    (defun make-a-button (text data)
       (apply #'make-instance 'capi:radio-button  
                   :text (format nil "~A" text)
                     :data data
                     :selected nil
                     :selection-callback 'my-button-panel-selection-callback
                     :callback-type :item-interface
              button-arglist)
       )

    ;;Make the :BEFORE METHOD TO MAKE THE BUTTONS and BUTTON-LAYOUT
    ;; (setf *mytestvar
    (eval
    `(defmethod capi:interface-display :before ((self  ,interface-name))
        (let
            ((button-list)
             )
        (with-slots (,parent-layout-name) self
 ;;BBB
          ;;make the button-list
          (setf  button-list 
                 (loop 
                  for data in  ,button-data-list
                  for text in  ,button-text-list
                  collect (make-a-button text data)
                 ;;end loop,setf
                 ))

          ;;make ,button-layout-name instance and put into the layout
          (setf  ,button-layout-name
                (apply  #'make-instance  ,button-layout-type
                         :description  button-list ,button-layout-arglist))
          ;;put into layout 
         ;; (setf (capi:pane-layout self) ,button-layout-name)

         ;;use only for putting into interface directly
         ;;  (setf (capi:pane-layout self) ,button-layout-name)
             (setf (capi:layout-description  ,parent-layout-name)
                    (list ,button-layout-name))  

             ;;(afout 'out (format nil "button-layout-name= UNBOUND~% ,button-layout-name=~A~%(eval button-layout-name= ~A~%(capi:layout-description  ,parent-layout-name)= ~A~%"    ,button-layout-name  (eval ,button-layout-name )  (capi:layout-description  ,parent-layout-name) ))
;;SSS        
          ;;end with-slots,let, defmethod, eval
          ))))

    ;;end defun make-my-button-panel
    ))
 |#
;;TEST
#|(defun testmb ()
  (setf out nil)
  (let
      ((x)
       )   
    (capi:define-interface test-radio-buttons ()
      ;;sets the button/item data values
      ()    ;;was (data :initform (list 1 2 3))
      (:layouts
      (row-layout-1
        capi:row-layout
        ()
        :background :pink
        :visible-min-width 400
        :visible-min-height 350
        )
#|        (my-button-panel-1
        capi:column-layout
        ()
        )|#
       )
      (:default-initargs
       :title "TEST INTERFACE"
       :visible-min-width 500
       :visible-min-height 400
       :background :yellow
       :internal-border 30
       ))
    (defun button-press-callback (data interface)
      (setf *test-result-data data)
      (capi:destroy interface)
      )

    ;;SSS CHANGE -PANEL BACK 
    (make-my-button-panel 'my-button-panel-1 'row-layout-1
                          'test-radio-buttons 
                          '(list "A" "B" "C")
                          '(list 10 20 30) 
                          :close-interface-on-selection-p nil
                          :button-layout-arglist  (quote (quote (:visible-min-width 200
                                                                 :visible-min-height 300
                                                                 :background :green
                                                                 :title "My Test Buttons"
                                                                 :title-gap 60  ;;no effect?
                                                                 ;;no effect    :x 0  :y 0
                                                                 :internal-border 2 ;;important
                                                                 :border t
                                                                 :title-adjust :center
                                                                 ;;no effect        :title-position :bottom
                                                                 ;; :font won't work
#|                                                                 :font ;;ERROR (gp:find-best-font pane  
                                                                          ;;(gp:make-font-description 
                                                                          ;; :family "Times New Roman"
                                                                           :weight :normal
                                                                           :slant :italic :size 12||#

                                                                 )))
                          :button-arglist (quote   (                        
                                                    :title "Press a button"
                                                    :title-gap 50  ;;gap betw end of title and button
                                                    :background :light-blue
                                                    :internal-border 0 ;;no effect??
                                                    :visible-min-width 100
                                                    :visible-min-height 40
                                                    ;; :FONT HOW TO GET THIS TO WORK SSS
                                                    ))                        

                          ;;end make-my-button-panel
                          )
    (capi:display (setf *mb-testinst (make-instance 'test-radio-buttons)))      
    ))|#
;;TEST RESULT
;;  CL-USER 10 > *my-button-panel-1-selected-values-list
;; ("B" 20 #<CAPI:RADIO-BUTTON "B" 27AA618F>)
    ;;test
    ;; *my-button-panel-1-selected-values-list = ("C" 30 #<CAPI:RADIO-BUTTON "C" 29845217>)
;; (capi:display (make-instance 'test-radio-buttons))
;;  (capi:display *mb-testinst)
;;  (capi-internals:capi-object-plist *mb-testinst) = nil
;;  (with-slots (row-layout1) *mb-testinst (capi:layout-description row-layout1))
;;CL-USER 98 > *mytestvar
#|(DEFMETHOD CAPI:INTERFACE-DISPLAY :BEFORE ((INTERFACE TEST-RADIO-BUTTONS)) (WITH-SLOTS (MY-BUTTON-PANEL-1) INTERFACE (SETF MY-BUTTON-PANEL-1 (LOOP FOR DATA IN # FOR TEXT IN # COLLECT #)) (CAPI:APPLY-IN-PANE-PROCESS MY-BUTTON-PANEL-1 (FUNCTION #) (LIST MY-BUTTON-PANEL-1) MY-BUTTON-PANEL-1))) 
 =  #<STANDARD-METHOD CAPI:INTERFACE-DISPLAY (:BEFORE) (TEST-RADIO-BUTTONS) 200B1B37>|#



 
 #| LAYOUT INITARGS--I MIGHT USE
:ADJUST
:AUTOMATIC-RESIZE
:BACKGROUND
:BORDER
:BUTTONS
:CHILDREN
:COLOR-REQUIREMENTS
:COLUMNS
:DEFAULT-X
:DEFAULT-Y
:DESCRIPTION
:ENABLED
:FILTER
:FIXED-SIZE
:FLAG
:FONT
:GAP
:HAS-TITLE-COLUMN-P
:INITIAL-CONSTRAINTS
:MESSAGE
:MESSAGE-ARGS
:MESSAGE-FONT
:MESSAGE-GAP
:NAME
:PLIST
:PREPROCESS-DESCRIPTION
:RATIOS
:ROWS
:TITLE
:TITLE-ADJUST
:TITLE-ARGS
:TITLE-FONT
:TITLE-GAP
:TITLE-POSITION
:VISIBLE-BORDER
:VISIBLE-HEIGHT
:VISIBLE-MAX-HEIGHT
:VISIBLE-MAX-WIDTH
:VISIBLE-MIN-HEIGHT
:VISIBLE-MIN-WIDTH
:WIDGET-NAME
:WINDOW-STYLES
:X
:X-ADJUST
:X-GAP
:X-RATIOS
:X-UNIFORM-SIZE-P
:Y
:Y-ADJUST
:Y-GAP
:Y-RATIOS
:Y-UNIFORM-SIZE-P
|#
#|  COMMON RADIO-BUTTON INITARGS
:ACTION-CALLBACK
:ALTERNATE-CALLBACK
:ALTERNATIVE-ACTION-CALLBACK
:BACKGROUND
:BEZEL-STYLE
:BUTTON-GROUP
:BUTTONS
:CALLBACK
:CALLBACK-TYPE
:CANCEL-P
:COLLECTION
:COLOR-REQUIREMENTS
:DATA
:DATA-FUNCTION
:ENABLED
:EXTEND-CALLBACK
:FILTER
:FONT
:HELP-KEY
:HORIZONTAL-SCROLL
:IMAGE
:INDICATOR
:INITIAL-CONSTRAINTS
:INTERACTION
:INTERFACE
:INTERNAL-BORDER
:KEY-FUNCTION
:MAX-HEIGHT
:MAX-WIDTH
:MNEMONIC
:MNEMONIC-ESCAPE
:MNEMONIC-TEXT
:MNEMONIC-TITLE
:NAME
:PRESS-CALLBACK
:PRINT-FUNCTION
:RETRACT-CALLBACK
:SELECTED
:SELECTED-IMAGE
:SELECTION-CALLBACK
:TAKE-FOCUS
:TEXT
:TEXT-ALIGNMENT
:TITLE
:TITLE-ADJUST
:TITLE-ARGS
:TITLE-FONT
:TITLE-GAP
:TITLE-POSITION
:VISIBLE-BORDER
:VISIBLE-HEIGHT
:VISIBLE-MAX-HEIGHT
:VISIBLE-MAX-WIDTH
:VISIBLE-MIN-HEIGHT
:VISIBLE-MIN-WIDTH
:VISIBLE-WIDTH
:WIDGET-NAME
:WINDOW-STYLES
:X
:Y
|#

#| 
  I am converting a java questionnaire I wrote to lisp and a crucial   | element is the capi:radio-button-panel or a substitute that looks almost   just like it (not a   | list panel). It is very important that users do not see any checked   | buttons before they make their own choices-NO DEFAULT SELECTION.
 
  The only way is to use the CAPI:CHECK-BUTTON-PANEL like this.
|# 
#|
   (defun force-selection (pane data)
      (setf (capi:choice-selected-items pane) (list data)))
   
   (capi:contain
     (make-instance 'capi:check-button-panel
      :interaction :multiple-selection
      :items '("one" "two" "three")
      :selected-item nil
      :callback-type :collection-data
      :retract-callback 'force-selection
      :selection-callback 'force-selection))
|#


#|
(defun make-radio-button-panel (ans-instruction-text answer-array-list)
  (make-instance 'capi:radio-button-panel
                 :items answer-array-list
                 ;;'(1 2 3 4 5 6 7 8 9 10 11) 
                 :layout-class 'capi:column-layout
                 :layout-args (list :adjust :center :x 25 :y 25
                                    :y-gap *answer-panel-y-gap
                                    :x-gap 20                                                                       
                                    :internal-border 25)
                 :font (gp:make-font-description 
                        :family *answer-pane-font-face
                        :weight :normal  :size *answer-font-size)
                 :visible-border T
                 :visible-min-height *answer-pane-height
                 :visible-max-height *answer-pane-height
                 :visible-min-width *answer-pane-width
                 ;; :visible-max-width *answer-pane-width
                 :background *answer-pane-background
                ;; :selected-items nil  ;; :none
                 :title ans-instruction-text 
                 :title-adjust  :left
                 ;;   :title-args
                 :title-font (gp:make-font-description 
                              :family *answer-pane-font-face
                              :weight :normal  :size *answer-font-size)
                 :title-gap 15
                 :title-position :top
                 :mnemonics nil         ;;only works for each item'(:none)
                 :callback-type :data-interface
                 :selected nil                 
                ;;doesn't work capi:choice-selected-item nil  must use setf outside init-args
                 ;;:selection :none  ;;:nothing works
                 :selection-callback 'single-selection-callback
                 )
  )
;;
;;My version which can allow modifications, etc.

;;MY MODIFIED VERSION OF ABOVE
(capi:define-interface my-button-panel ()
  ((data :initform (list 1 2 3)))
  (:layouts
   (panel
    capi:column-layout)))

(setf *button-list nil
      *item-value-list nil
      *item-data-list nil)
(defun switch-other-buttons-off (item interface)
  (let
      ((button-value)
       (button-data)
       )
  (with-slots (panel) interface
    (loop for button in (capi:layout-description panel)
        unless (eq item button)
        do (setf (capi:item-selected button) nil))
    ;;added
   ;; (setf button-value (capi:item-selected button))
    ;;end added
    (setf *button-list (append *button-list (list item))
                               button-value (capi:item-text item)
                               *item-value-list (append *item-value-list  (list button-value))
                               button-data (capi:item-data item)
                                 *item-data-list (append *item-data-list  (list button-data)))                          
    )))
;;works, creates non-selected button "panel" then after pushing each button, results
#|CL-USER 35 > *button-list
(#<CAPI:RADIO-BUTTON "three" 2010A28B> #<CAPI:RADIO-BUTTON "two" 2010A56F> #<CAPI:RADIO-BUTTON "one" 2010A84F>)
CL-USER 44 > *item-value-list = ("three" "two" "one")
CL-USER 47 > *item-data-list = (3 2 1) |#


(defun make-a-button (i)
  (make-instance 'capi:radio-button 
                 :text (format nil "~R" i)
                 :data i
                 :selected nil
                 :selection-callback 'switch-other-buttons-off
                 :callback-type :item-interface))

(defmethod capi:interface-display :before ((self radio-buttons))
  (with-slots (data panel) self
    (setf (capi:layout-description panel)
          (loop for i in data
                collect (make-a-button i)))))

(capi:display (make-instance 'radio-buttons))
|#




#|
;;XXX
;;CREATING A BUTTON PANEL FROM FUNCTION (WITH NO DEFAULT SELECTION)
;;IF INSIDE A FUNCTION CREATING AN INTERFACE INSTANCE,
      (with-slots (answer-button-panel)  q-frame-inst
        ;;MAKE THE  BUTTON PANEL
      (setf answer-button-panel (make-radio-button-panel ans-instruction-text answer-array-list))
      ;;Put the BUTTON IN THE ALREADY CREATED FRAME
      (capi:apply-in-pane-process answer-column-layout
                                  #'(setf capi:layout-description)
                                          (list answer-button-panel)                                            
                                          answer-column-layout)
      ;;This SETS PREVENTS A DEFAULT SELECTION from being checked
      (capi:apply-in-pane-process answer-column-layout
                                  #'(setf CAPI:CHOICE-SELECTED-ITEM)
                                          (list nil)                                            
                                          answer-button-panel)


It is very important that users do not see any checked buttons before
 they make their own choices—NO DEFAULT SELECTION.
(defun test ()
  (let ((panel (make-instance 'capi:radio-button-panel :items '("He" "She" "They"))))
    (setf (capi:choice-selected-item panel) nil)
    (capi:contain panel)))
|#

#|

;;MAKE-RADIO-BUTTON-PANEL
;;
;;ddd
(defun make-radio-button-panel (ans-instruction-text answer-array-list)
  (make-instance 'capi:radio-button-panel
                 :items answer-array-list
                 ;;'(1 2 3 4 5 6 7 8 9 10 11) 
                 :layout-class 'capi:column-layout
                 :layout-args (list :adjust :center :x 25 :y 25
                                    :y-gap *answer-panel-y-gap
                                    :x-gap 20                                                                       
                                    :internal-border 25)
                 :font (gp:make-font-description 
                        :family *answer-pane-font-face
                        :weight :normal  :size *answer-font-size)
                 :visible-border T
                 :visible-min-height *answer-pane-height
                 :visible-max-height *answer-pane-height
                 :visible-min-width *answer-pane-width
                 ;; :visible-max-width *answer-pane-width
                 :background *answer-pane-background
                ;; :selected-items nil  ;; :none
                 :title ans-instruction-text 
                 :title-adjust  :left
                 ;;   :title-args
                 :title-font (gp:make-font-description 
                              :family *answer-pane-font-face
                              :weight :normal  :size *answer-font-size)
                 :title-gap 15
                 :title-position :top
                 :mnemonics nil         ;;only works for each item'(:none)
                 :callback-type :data-interface
                ;;doesn't work capi:choice-selected-item nil  must use setf outside init-args
                 ;;:selection :none  ;;:nothing works
                 :selection-callback 'single-selection-callback
                 )
  )
|#

;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/buttons:buttons.lisp,v 1.16.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/buttons/buttons.lisp
;;
;; This example demonstrates the use of buttons and button-panels in the
;; CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-BUTTONS)
;;
;; This display two interfaces. One is the BUTTON-TEST, and demonstarte
;; the callbacks. The other one is an IMAGE-BUTTON-EXAMPLE, and demostrates
;; using images in buttons. 

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Define the interface button-test
;;----------------------------------------------------------------------------
#|
(capi:define-interface button-test ()
  ()
  (:panes
   (push-button
    capi:push-button
    :text "Push"
    :data :push-button
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback
    ;; PUSH-BUTTON has alternate callback that can be invoked
    ;; by selecting with CONTROL (on Windows and GTK) or Command
    ;; (on Cocoa)
    :alternate-callback 'button-alternate-callback
    )
   (check-button
    capi:check-button
    :text "Check"
    :data :check-button
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (radio-button
    capi:radio-button
    :text "Radio"
    :data :radio-button
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (push-button-panel
    capi:push-button-panel
    :items '("push 1" "push 2" "push 3")
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (check-button-panel
    capi:check-button-panel
    :items '("check 1" "check 2" "check 3")
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (radio-button-panel
    capi:radio-button-panel
    :items '("Radio 1" "Radio 2" "Radio 3")
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (op
    capi:collector-pane
    :initial-constraints '(:visible-min-width (character 25)))
    )
  (:layouts
   (row
    capi:row-layout
    '(push-button radio-button check-button)
    :y-adjust :centre)
   (default-layout
    capi:column-layout
    '(row push-button-panel check-button-panel radio-button-panel op)))
  (:default-initargs
   :layout 'default-layout
   :title "Button Test"))
|#

;;----------------------------------------------------------------------------
;; A generic callback
;;----------------------------------------------------------------------------

(defun button-callback (type data interface)
  (format (capi:collector-pane-stream(slot-value interface 'op))
          "~S ~a~%" data type))

(defun button-selection-callback (&rest args)
  (apply 'button-callback "selected" args))

(defun button-extend-callback (&rest args)
  (apply 'button-callback "extended" args))

(defun button-retract-callback (&rest args)
  (apply 'button-callback "retracted" args))

(defun button-alternate-callback (&rest args)
  (apply 'button-callback "alternate selected" args))

;;----------------------------------------------------------------------------
;; Image Buttons
;;----------------------------------------------------------------------------

;; These forms were derived via something like:
;; (let ((external-image (gp:read-external-image <file>)))
;;  (gp:compress-external-image external-image)
;;  (setf (gp:external-image-transparent-color-index external-image) 0)
;;  (make-load-form external-image))
;; where <file> contains the DIB format representation of an image.

#|
(defparameter *dont-button*
  (make-instance 
   'gp:external-image
   :data
   #(66 77 66 2 0 0 0 0 0 0 66 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 54 1 0 0 109 11 0 0 109 11 0 0 3 0 0 0 3 0 0 0 0 0 0 0 255 0 0 0 0 255
        0 0 32 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 11 0 0 4 34 32 5 0 2 34
        10 0 0 0 10 0 0 4 34 32 6 0 4 34 8 0 0 0 11 0 0 4 34 32 4 0 0 4 34 32 9 0
        0 0 11 0 0 4 34 32 4 0 0 4 34 32 9 0 0 0 12 0 0 10 34 32 0 34 32 0 10 0 0 0
        2 1 10 17 0 10 34 33 17 34 33 0 7 17 0 3 0 0 0 0 2 0 11 17 0 8 34 33 34 33 9 17
        2 0 0 0 13 0 7 34 12 0 0 0 10 0 0 4 32 0 5 34 13 0 0 0 10 0 0 4 34 0 4 34
        14 0 0 0 10 0 0 4 34 0 4 34 4 0 2 34 8 0 0 0 11 0 0 12 34 2 34 32 0 34 9 0
        0 0 11 0 0 12 34 2 34 32 2 32 9 0 0 0 12 0 6 34 0 4 2 32 10 0 0 0 12 0 8 34
        12 0 0 0 13 0 6 34 13 0 0 0 14 0 4 34 14 0 0 0 15 0 0 4 34 32 13 0 0 0 15 0
        4 34 13 0 0 0 14 0 6 34 12 0 0 0 14 0 6 34 12 0 0 0 15 0 4 34 13 0 0 0 16 0
        2 34 14 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 0 1)
   :transparent-color-index 0))

(defparameter *do-button*
  (make-instance 
   'gp:external-image
   :data
   #(66 77 66 2 0 0 0 0 0 0 66 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 14 1 0 0 109 11 0 0 109 11 0 0 3 0 0 0 3 0 0 0 0 0 0 0 255 0 0 0 0 0
        255 0 32 0 0 0 32 0 0 0 32 0 0 0 2 0 29 17 1 0 0 0 2 0 29 17 1 0 0 0 32 0
        0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32
        34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0
        0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0
        0 0 12 0 0 8 34 32 34 32 12 0 0 0 10 0 11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0
        11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0
        11 34 11 0 0 0 10 0 11 34 11 0 0 0 11 0 9 34 12 0 0 0 14 0 0 4 34 32 14 0 0 0
        13 0 5 34 14 0 0 0 13 0 5 34 14 0 0 0 13 0 5 34 14 0 0 0 14 0 0 4 34 32 14 0
        0 0 32 0 0 0 32 0 0 0 32 0 0 0 0 1) :transparent-color-index 0))

(defparameter *dont-dis-button*
  (make-instance 
   'gp:external-image
   :data
   #(66 77 86 2 0 0 0 0 0 0 86 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 132 1 0 0 109 11 0 0 109 11 0 0 8 0 0 0 8 0 0 0 0 0 0 0 126 84 169 0 169 169
        169 0 212 169 169 0 169 255 169 0 169 89 174 0 84 174 174 0 126 92 177 0 32 0 0 0 32 0 0 0 32 0
        0 0 32 0 0 0 11 0 0 4 85 80 5 0 2 85 10 0 0 0 10 0 0 14 84 68 80 0 5 68 85 0
        8 0 0 0 9 0 0 6 84 4 80 0 4 0 0 6 84 4 69 0 7 0 0 0 10 0 0 14 84 4 80 0
        84 4 85 0 8 0 0 0 10 0 0 14 84 4 80 0 84 4 80 0 8 0 0 0 11 102 0 12 116 4 102 100
        4 22 6 102 0 3 0 6 0 0 2 99 10 51 0 10 64 67 51 64 67 0 7 51 0 3 96 0 0 0 2 6
        11 51 0 8 64 67 64 67 9 51 2 96 0 0 2 0 11 102 0 8 64 4 0 69 10 102 1 0 0 0 9 0
        0 12 84 82 84 0 4 80 11 0 0 0 9 0 0 16 84 69 84 0 69 0 5 80 7 0 0 0 9 0 0 16
        84 5 84 0 69 0 84 69 7 0 0 0 10 0 0 14 80 69 64 4 80 84 69 0 8 0 0 0 10 0 0 14
        84 5 64 4 85 68 80 0 8 0 0 0 11 0 0 12 80 64 0 69 68 80 9 0 0 0 11 0 2 84 6 0
        2 69 11 0 0 0 12 0 2 84 4 0 2 69 12 0 0 0 13 0 0 6 84 0 69 0 13 0 0 0 14 0
        0 6 84 4 80 0 12 0 0 0 14 0 0 6 84 0 69 0 12 0 0 0 13 0 2 84 4 0 2 69 11 0
        0 0 13 0 2 84 4 0 2 69 11 0 0 0 14 0 0 6 84 0 69 0 12 0 0 0 15 0 0 4 84 69
        13 0 0 0 16 0 2 85 14 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 0 1) :transparent-color-index 0))

(defparameter *do-dis-button*
  (make-instance
   'gp:external-image
   :data
   #(66 77 78 2 0 0 0 0 0 0 78 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 106 1 0 0 109 11 0 0 109 11 0 0 6 0 0 0 6 0 0 0 0 0 0 0 255 255 116 0 227 135
        135 0 230 230 138 0 161 207 207 0 135 135 227 0 32 0 0 0 32 0 0 0 2 0 29 68 1 0 0 0 2 4
        29 34 1 64 0 0 2 4 29 34 1 64 0 0 2 0 29 68 1 0 0 0 11 0 0 10 53 85 53 85 48 0
        11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0
        11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10
        53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5
        48 0 11 0 0 0 10 0 0 12 51 80 83 80 83 48 10 0 0 0 9 0 0 14 53 80 0 80 0 85 48 0
        9 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53
        9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0
        9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 10 0 0 12 53 85 0 5
        85 48 10 0 0 0 11 0 0 10 51 53 5 19 48 0 11 0 0 0 12 0 0 8 53 0 5 48 12 0 0 0
        12 0 0 8 53 0 5 48 12 0 0 0 12 0 0 8 53 0 5 48 12 0 0 0 13 0 0 6 53 85 48 0
        13 0 0 0 14 0 0 4 51 48 14 0 0 0 32 0 0 0 32 0 0 0 0 1) :transparent-color-index 0))



;;------------------------------------------------------------

;; Register the images in the image translation table.
;; From henceforth they can be referenced via the ids

(mapc #'(lambda (id image)
          (gp:register-image-translation id image))
      '(:do :dont :do-disabled :dont-disabled)
      (list *do-button* *dont-button* *do-dis-button* *dont-dis-button*))

;; This shows the effects of button classes with and without indicators.
;; Note that :indicator, :disabled-images, :selected-disabled-images are ignored on Windows.
;; The images in the buttons are used shared resources.

(capi:define-interface image-button-example ()
  ()
  (:panes 
   (buttons1 capi:radio-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)              
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled) 
             :indicator T)                                                  
   (buttons2 capi:radio-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)              
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled) 
             :indicator nil)                                                
   (buttons3 capi:check-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)              
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled) 
             :indicator T)                                                  
   (buttons4 capi:check-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled)
             :indicator nil)
   (disabler capi:radio-button-panel :items '("Enable" "Disable")
             :callback-type :none
             :layout-class 'capi:column-layout
             :selection-callback #'(lambda ()
                                     (mapc #'(lambda (b)
                                               (setf (capi:simple-pane-enabled b) (not (capi:simple-pane-enabled b))))
                                           (list buttons1 buttons2 buttons3 buttons4))))
   (image-explainer capi:output-pane
                    :visible-min-height 160
                    :visible-max-height t
                    :title "Key to images"
                    :title-position :frame
                    :visible-border nil
                    :visible-min-width 180
                    :display-callback 
                    'display-images-with-titles))
  (:layouts 
   (one capi:column-layout '(buttons1 buttons2) :title "Radio" :title-position :frame)
   (two capi:column-layout '(buttons3 buttons4) :title "Check" :title-position :frame)
   (interactive-layout  capi:column-layout  '(one two disabler) )
   (main  capi:row-layout  '(interactive-layout image-explainer) :default t))
  (:default-initargs
   :title "Image Button Example 2"))

(defun display-image-with-title (pane top-y title image)
  (gp:draw-string pane title 50 (+ top-y 30))
  (gp:draw-image pane (gp:load-image pane image) 10 (+ top-y 5)))

(defun display-images-with-titles (pane x y width height)
  (declare (ignore x y width height))
  (display-image-with-title pane 0 "Default image" :do)
  (display-image-with-title pane 40 "Selected image" :dont)
  (if (member (capi:default-library) '(:GTK :Cocoa))
      (gp:draw-string pane "Automatic disabling" 10 110)
    (progn 
      (display-image-with-title pane 80 "Default disabled" :do-disabled)
      (display-image-with-title pane 120 "Selcted disabled" :dont-disabled))))
;;----------------------------------------------------------------------------
;; Test button-test
;;----------------------------------------------------------------------------

(defun test-buttons ()
  (let(( first (capi:display (make-instance 'button-test))))
    (multiple-value-bind (x y width height)
        (capi:top-level-interface-geometry first)
      (declare (ignore x width height))
      (let ((second (make-instance 'image-button-example)))
        (capi:display second)
        (capi:set-top-level-interface-geometry second :y (+ y 300))))))

|#

;;xxx --------------------- STARTING WITH NO BUTTONS CHECKED -----------

#|  Thomas Stevens wrote on Sat, 19 Apr 2014 00:41:37 +0000 04:41:
#| 
  | I am converting a java questionnaire I wrote to lisp and a crucial   | element is the capi:radio-button-panel or a substitute that looks almost   just like it (not a   | list panel). It is very important that users do not see any checked   | buttons before they make their own choices-NO DEFAULT SELECTION.
 
  The only way is to use the CAPI:CHECK-BUTTON-PANEL like this.
|# 
  (defun force-selection (pane data)
    (setf (capi:choice-selected-items pane) (list data)))
 
  (capi:contain
   (make-instance 'capi:check-button-panel
    :interaction :multiple-selection
    :items '("one" "two" "three")
    :selected-item nil
    :callback-type :collection-data
    :retract-callback 'force-selection
    :selection-callback 'force-selection))

   (defun force-selection (pane data)
      (setf (capi:choice-selected-items pane) (list data)))
   
    (capi:contain
     (make-instance 'capi:check-button-panel
      :interaction :multiple-selection
      :items '("one" "two" "three")
      :selected-item nil
      :callback-type :collection-data
      :retract-callback 'force-selection
      :selection-callback 'force-selection))
|#
#|You can also do it like this, with all of the buttons unselected initially, and imitating radio-button-panel's :single-selection interaction subsequently by making their callbacks deselect the other
buttons:|#

;;HERE HE DOESN'T USE RADIO-BUTTON PANEL, BUT MAKES HIS OWN!
;;  PROBLEM WITH :SELECTION-CALLBACK ALREADY BEING DEFINED
;; I solved problem?? below
#|(capi:define-interface radio-buttons ()
  ;;sets the button/item data values
  ((data :initform (list 1 2 3)))
  (:layouts
   (panel
    capi:column-layout)))

(defun switch-other-buttons-off (item interface)
  (with-slots (panel) interface
    (loop for button in (capi:layout-description panel)
        unless (eq item button)
        do (setf (capi:item-selected button) nil))))

(defun make-a-button (i)
  (make-instance 'capi:radio-button 
                 :text (format nil "~R" i)
                 :data i
                 :selected nil
                 :selection-callback 'switch-other-buttons-off
                 :callback-type :item-interface))

(defmethod capi:interface-display :before ((self radio-buttons))
  (with-slots (data panel) self
    (setf (capi:layout-description panel)
          (loop for i in data
                collect (make-a-button i)))))

(capi:display (make-instance 'radio-buttons))
|#
#|
;;MY MODIFIED VERSION OF ABOVE
(capi:define-interface my-radio-buttons-interface ()
  ((data :initform (list 1 2 3)))
  (:layouts
   (panel
    capi:column-layout)))

(setf *button-list nil
      *item-value-list nil
      *item-data-list nil)
(defun switch-other-buttons-off (item interface)
  (let
      ((button-value)
       (button-data)
       )
  (with-slots (panel) interface
    (loop for button in (capi:layout-description panel)
        unless (eq item button)
        do (setf (capi:item-selected button) nil))
    ;;added
   ;; (setf button-value (capi:item-selected button))
    ;;end added
    (setf *button-list (append *button-list (list item))
                               button-value (capi:item-text item)
                               *item-value-list (append *item-value-list  (list button-value))
                               button-data (capi:item-data item)
                                 *item-data-list (append *item-data-list  (list button-data)))                          
    )))
|#
;;works, creates non-selected button "panel" then after pushing each button, results
#|CL-USER 35 > *button-list
(#<CAPI:RADIO-BUTTON "three" 2010A28B> #<CAPI:RADIO-BUTTON "two" 2010A56F> #<CAPI:RADIO-BUTTON "one" 2010A84F>)
CL-USER 44 > *item-value-list = ("three" "two" "one")
CL-USER 47 > *item-data-list = (3 2 1) |#


#|(defun make-a-button (i)
  (make-instance 'capi:radio-button 
                 :text (format nil "~R" i)
                 :data i
                 :selected nil
                 :selection-callback 'switch-other-buttons-off
                 :callback-type :item-interface))

(defmethod capi:interface-display :before ((self radio-buttons))
  (with-slots (data panel) self
    (setf (capi:layout-description panel)
          (loop for i in data
                collect (make-a-button i)))))

(capi:display (make-instance 'radio-buttons))
|#

#| This also seems to work on Windows and Cocoa, but probably should not since radio-button-panel is a :single-selection choice. Does not nullify initial selections on GTK+, so definitely not portable.
(let ((rbp (make-instance 'capi:radio-button-panel
                          :items (list 1 2 3) 
                          :print-function (lambda (i) 
                                            (format nil "~R" i))
                          :layout-class 'capi:column-layout)))
  (setf (capi:choice-selection rbp) nil)
  (capi:contain rbp))
|#
#|

--
Dave Fox
LispWorks Ltd
http://www.lispworks.com/

Registered Office: St John's Innovation Centre, Cowley Road, Cambridge CB4 0WS Registered in England: No. 5114963 EC VAT ID: GB 833329531|#




;; VERSION USING ONLY RADIO BUTTONS,  COULDN'T GET RETRACT TO WORK OR GET IT TO FIRE SELECTION-CALLBACK WHEN PRESSED 2ND TIME
#|(defun make-my-vertical-button-panel (parent-layout-name
                                      interface-name 
                                      button-text-list  button-data-list            
                                      &key   (close-interface-on-selection-p T)
                                      (selection-type 'single)
                                      button-arglist
                                      button-layout-arglist
                                      button-text-pane-arglist
                                      button-text-layout-arglist
                                      (last-button-on-top-p NIL)
                                      (send-datalist-to-instance-slot 'selected-item-datalist)            
                                      )    ;;&rest button-arglist) caused error
  "In U-capi-buttons-etc.lisp.  DOC may be less than 100% up to date. Makes a button layout, 'button-layout-name', which is a customized column layout of buttons that allows more options than the standard radio-button panel. Uses radio buttons as the buttons. All regular initargs to the layout is accessed via button-layout-arglist, and all radio-button initargs thru the &rest list. Starts with NO DEFAULT PRESELECTED BUTTON-a major change over capi.  Use to create buttons and add to interfaces. Makes a  layout and puts inside parent-layout. REQUIRES a pre-defined INTERFACE  a layout named parent-layout-name. The arglists must be preceded by double quotes. EXAMPLE: (make-my-button-panel  'row-layout-1 'test-my-radio-buttons  '(list 'A 'B 'C)  '(list 10 20 30)  :button-layout-arglist   '(list :visible-min-width 200 etc)) ETC. Creates a GLOBAL VAR, (format nil \"*~A-selected-values-list\" parent-layout-name) = (list selected-text selected-data selected-n[begins with 1]  selected-button-instance last-button-on-top-p) of last selected button. IF MULIPLE-SELECTION, RETURNS (list selected-text-LIST selected-data-LIST selected-n-LIST[begins with 1]  selected-button-instance-LIST  last-button-on-top-p). Single selection also calls append-my-vertical-button-panel-single-selection-callback multiple-selection calls append-my-vertical-button-panel-multi-selection-callback"
  (let
      ((global-selected-values-list (my-make-symbol 
                                     (format nil "*~A-selected-values-list" parent-layout-name)))
       (button-list)
       (button-layout-inst)
       (button-text-pane-list)
       (button-text-layout-inst)
       )

    (cond
     ((or  *single-selection-item (member selection-type
            (list 'single  :single  :single-selection "single" "SINGLE" "Single"  :test 'equal)))
      (setf selection-type :single-selection))
     (t (setf selection-type :multiple-selection)))

  ;;  (setf out1  (format nil "A send-datalist-to-instance-slot= ~A~%parent-layout-name ~A~%" send-datalist-to-instance-slot parent-layout-name))

    ;;Make the SELECTION CALLBACK
    ;;ddd
    (eval 
     `(defun my-button-panel-selection-callback (item interface)  
        "In U-capi-buttons-etc.lisp, MUST use with my-button-panels.  MODIFY THIS CALLBACK to get data or actions from the button selection."

        (afout 'out (format nil "FIRING my-button-panel-selection-callback"))
        (let
            ((button-value)
             (button-data)
             (selected-text-n)
             (selected-text)
             (selected-data)
             (selected-button)
             (selection-type1 ,selection-type)
             (button-layout)
             (button-text-layout)
             ;;  (datalist-instance)
             ;;   (datalist-slot (quote ,send-datalist-to-instance-slot))
             (last-button-on-top-p1 ,last-button-on-top-p)
             )
       ;;   (setf out3  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-name) ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-name)))
             (afout 'out (format nil "selection-type1= ~A~%" selection-type1))
          (with-slots (,parent-layout-name ,send-datalist-to-instance-slot) interface 
            #|(afout 'out (format nil ",parent-layout-name= ~A~% ,send-datalist-to-instance-slot= ~A~%  item= ~A~%, ",parent-layout-name ,send-datalist-to-instance-slot   item  ))|#
         ;;   (setf out4  (format nil "B (quote ,send-datalist-to-instance-slot)= ~A~%,(quote ,parent-layout-name) ~A~%item= ~A~%" (quote ,send-datalist-to-instance-slot) (quote ,parent-layout-name) item))
           
            ;;TO CHANGE SELECTION, MUST UNSET PREVOUSLY SELECTED BUTTONS
            (setf  BUTTON-LAYOUT (CAR (capi:layout-description ,parent-layout-name)))    
            (cond
             ((equal selection-type1 :multiple-selection)
              ;;DON'T DO ANYTHING.  CALLBACK IS FROM GO BUTTON INSTEAD
              )
             ((equal selection-type1 :single-selection)
                 (loop
                  for button in (capi:layout-description BUTTON-LAYOUT) ;; ,button-layout-name)
                  unless (eq item button) ;;  (or (equal item button) (null item))
                  do 
                  (setf (capi:item-selected button) nil)
                  ;;(afout 'out (format nil "item= ~A~%button= ~A (capi:item-selected button)= ~A"  item button (capi:item-selected button)))
                  ;;end loop, if
                  
            ;;FIND SELECTED BUTTON N, TEXT, DATA, and BUTTON-INSTANCE
            (setf selected-n  (my-make-symbol (capi:item-text item))
                  selected-text (nth (- selected-n 1) ,button-text-list)
                  selected-data (capi:item-data item)
                  selected-button item)   
            ;;end loop
            )
              
            ;;SET GLOBAL VARIABLE TO DATALIST VALUES
            (setf ,global-selected-values-list 
                  (list selected-text selected-data selected-n  selected-button last-button-on-top-p1))
            ;; SET SLOT-VALUE IN QUESTION FRAME INSTANCE TO DATALIST
            (cond
             ((quote ,send-datalist-to-instance-slot)
              (setf (slot-value interface (quote ,send-datalist-to-instance-slot ))
                    (list selected-text selected-data selected-n  
                          selected-button last-button-on-top-p1)))
             (t nil))
            ;;   (afout 'out (format nil "IN CALLBACK, (slot-value interface (quote ,send-datalist-to-instance-slot) )= ~A~%  (quote ,send-datalist-to-instance-slot)= ~a~%" (slot-value interface (quote ,send-datalist-to-instance-slot) )   (quote ,send-datalist-to-instance-slot)))
            ;;CLOSE THE INTERFACE INSTANCE ONCE SELECTION MADE?
            (if ,close-interface-on-selection-p
                (capi:destroy interface))

            ;;end single-selection
            )
             (t nil))
            ;;IF *CALL-SHAQ-CALLBACK-P, THEN DATA PROCESSING AND
            ;;   CALLING NEXT FRAME
            (cond
             ((equal selection-type1 :single-selection)
                (append-my-vertical-button-panel-single-selection-callback item  interface))
             ((equal selection-type1 :multiple-selection)
              ;;AAA
              ;;Must deselect button if checked even number of times
              (loop
               for button-sel-list  in *multi-selection-button-selection-list
               with button1
               with num
               with sel-value
               do
               (setf button1 (car button-sel-list))

#|              (afout 'out (format nil "ITEM= ~A~% button-sel-list= ~A~%(capi:item-selected item)= ~A~%"item button-sel-list (capi:item-selected item)))|#
               (cond
                ((equal item button1)
                 (setf num (+ (second button-sel-list) 1))
                 (if (oddp num) (setf sel-value T)
                      (setf sel-value NIL))
                 (setf button-sel-list (list button1 num sel-value))
                 ;;change the real button selected value
                 (setf (capi:item-selected item) sel-value)

                 (afout 'out (format nil "ITEM= ~A~% button-sel-list= ~A~%(capi:item-selected item)= ~A~%"item button-sel-list  (capi:item-selected item)))
                 )
                (t nil))
               ;;rebuild the main list (possibly adding revised button-sel-list)
               (setf *multi-selection-button-selection-list
                     (append *multi-selection-button-selection-list (list button-sel-list)))             
              ;;end loop
               )
             ;; data actions, etc initiated by GO Button for Multi-selection items
             ;;(append-my-vertical-button-panel-multi-selection-callback item  interface)

             ;;end multi-selection type
              )
             (t nil))

        ;;end  with-slots, defun callback,eval
        ))))

    ;;SSS START HERE DEBUGGING  (CAN REALLY USE THIS ON RADIO BUTTONS?)  
    ;;RETRACT-CALLBACK TO UNMARK BUTTON ON MULTI-SEL
    ;;ddd
    ;;APPARENTLY RADIO-BUTTON DOESN'T CALL RETRACT-CALLBACK
#|    (eval
     `(defun my-button-panel-retract-callback (item interface)
        (cond
         ((equal selection-type1 :single-selection)
          ;;DO NOTHING TO RETRACT, WILL RETRACT WHEN CHOOSE OTHER
          (setf out4(format nil "IN my-button-panel-retract-callback, ITEM= ~A" item))
          )
         (t
          (setf out4(format nil "IN my-button-panel-retract-callback, ITEM= ~A" item))
;;(setf (capi:item-selected button) nil)
          (setf (capi:item-selected  item) nil)))
        ))
|#
    ;; MAKE BUTTON FUNCTION
    ;;ddd
    (defun make-a-button (text data button-arglist )
      (apply #'make-instance 'capi:radio-button  
             :text (format nil "~A" text)
             :data data
             :selected nil
             :selection-callback 'my-button-panel-selection-callback
             :callback-type :item-interface
           ;;doesn't work??  :retract-callback 'my-button-panel-retract-callback
             button-arglist)
      )
    ;;MAKE A BUTTON-TEXT-PANE FUNCTION
    ;;ddd
    (defun make-a-button-text-pane (text text-arglist) ;; &optional
      ;;   fill-button-text-space-p)
      #|      (if fill-button-text-space-p
          (setf text (format nil "~A" text))
        (format nil "~%~A" text))|#
      (apply #'make-instance 'capi:rich-text-pane  
             :text (format nil "~A" text)
             text-arglist)
      )
    ;; (format nil "outer ~A" (format nil "INNER"))  = "outer INNER"                        :  
    
    ;;Make the :BEFORE METHOD TO MAKE THE BUTTONS and BUTTON-LAYOUT
    ;;ddd
    ;; (setf *mytestvar
    (eval
     `(defmethod capi:interface-display :before ((self  ,interface-name))
        (let
            ((button-list)
             (button-text-pane-list)
             (datalist-length (list-length ,button-data-list))
             )
          ;;used for multi-selection items only
          (defparameter *multi-selection-button-selection-list nil)

          (with-slots (,parent-layout-name) self
            ;;make the button-list and button-text-pane-list
            (loop 
             for data in  ,button-data-list
             for text in  ,button-text-list
             for n from 1 to datalist-length
             with button
             with button-text-pane
             do
             (setf button (make-a-button n data ,button-arglist)
                   button-list (append button-list (list button))  
                   ;;next used for retracting button selections in multi-selection 
                   ;; makes list = ((button-inst 0 nil)....) 0 is num of times clicked upon
                   ;;  nil means not selected (reset to nil on even num of clicks)
                   *multi-selection-button-selection-list 
                   (append *multi-selection-button-selection-list (list (list button 0 nil)))
                   button-text-pane 
                   (make-a-button-text-pane text ,button-text-pane-arglist)    
                   button-text-pane-list (append button-text-pane-list 
                                                 (list button-text-pane)))
             ;;end loop,setf
             )

            ;;make buttons so LAST IN LIST  is on TOP?
            (if  ,last-button-on-top-p
                (setf button-list (reverse button-list)
                      button-text-pane-list (reverse button-text-pane-list)))

            ;;MAKE ,BUTTON-LAYOUT-NAME INSTANCE AND PUT IN LAYOUT
            (setf  button-layout-inst  
                   (apply  #'make-instance  'capi:column-layout 
                           :description  button-list ,button-layout-arglist))
            ;;make button-text-layout instance
            (setf button-text-layout-inst
                  (apply #'make-instance 'capi:column-layout
                         :description button-text-pane-list ,button-text-layout-arglist))  
            ;;put into layout 
            (setf (capi:layout-description  ,parent-layout-name)
                  (list button-layout-inst button-text-layout-inst)) 

            ;;(afout 'out (format nil " ,button-layout-inst=~A~%(eval button-layout-inst= ~A~%(capi:layout-description  ,parent-layout-name)= ~A~%"    ,button-layout-inst  (eval ,button-layout-inst )  (capi:layout-description  ,parent-layout-name) ))
;;  (setf  out6 (format nil "(quote ,parent-layout-name)=~A~%(eval parent-layout-name= ~A~%(capi:layout-description  ,parent-layout-name)= ~A~%"  (quote   ,parent-layout-name)  (eval ,parent-layout-name )  (capi:layout-description  ,parent-layout-name) ))
     
            ;;end with-slots,let, defmethod, eval
            ))))
    ;;end defun make-my-vertical-button-panel
    ))|#

;; DELETE THIS-- NOT NEED USING CHECK BUTTONS
#|(loop
               for button-sel-list  in *multi-selection-button-selection-list
               with button1
               with num
               with sel-value
               do
               (setf button1 (car button-sel-list))

#|              (afout 'out (format nil "ITEM= ~A~% button-sel-list= ~A~%(capi:item-selected item)= ~A~%"item button-sel-list (capi:item-selected item)))|#
               (cond
                ((equal item button1)
                 (setf num (+ (second button-sel-list) 1))
                 (if (oddp num) (setf sel-value T)
                      (setf sel-value NIL))
                 (setf button-sel-list (list button1 num sel-value))
                 ;;change the real button selected value
                 (setf (capi:item-selected item) sel-value)

                 (afout 'out (format nil "ITEM= ~A~% button-sel-list= ~A~%(capi:item-selected item)= ~A~%"item button-sel-list  (capi:item-selected item)))
                 )
                (t nil))
               ;;rebuild the main list (possibly adding revised button-sel-list)
               (setf *multi-selection-button-selection-list
                     (append *multi-selection-button-selection-list (list button-sel-list)))             
              ;;end loop
               )|#