;;************************ U-lw-editor.lisp *************************
;;
;;


;;LIST-BUFFER-OBJECTS
;;
;;ddd
(defun list-buffer-objects (file-prename object-type-list &key (pathroot "C:\\TOM\\LISP PROJECTS TS\\")(dir "shaq")(ext ".lisp") return-object-list-string-p)
  "In U-lw-editor.lisp, creates a window and prints a list of object-type items in it"
  (let*
      ((pathname (format nil "~A~A\\~A~A" pathroot dir file-prename ext))
       (buffer-name (format nil "~A defclass buffer" pathroot))
       (object-list-buffer-inst (make-instance 'buffer-list-frame))
       (eof)
       (object-list-string (format nil "In pathname: ~A~% OBJECT-LIST=> ~A~%" pathname  object-type-list))
       )
    ;;object-type loop
    (loop
     for object-type in object-type-list
     with object-string
     ;;  with object-list-string
     do
     (with-open-file (s-input pathname :direction :input) 
       (setf object-string  (format nil "~A" object-type)
             object-list-string (format nil "~A~%~%>>>>>  OBJECT-TYPE: ~A  <<<<<~%" object-list-string object-string))
       (loop
        for n from 0 to 3000
        with line
        with token
        with rest-string
        do
        (multiple-value-setq (line eof)
            (read-line s-input))
        (cond
         (eof
          (return))
         (t
          (multiple-value-setq (token rest-string)
              (match-first-token object-string line  :delimiter-list '( "\(" " ")))
          (cond
           (token
            (setf object-list-string (format nil "~A~%~A" object-list-string line))
            )
           (t nil))
          ))
        ;;end inner loop, with-open-file (reopen on next loop)
        ))      
     ;;end outer loop
     )
    ;;display the buffer
    (capi:display object-list-buffer-inst)

    #|      (with-slots (answer-button-panel)  q-frame-inst
        ;;MAKE THE  BUTTON PANEL
      (setf answer-button-panel (make-radio-button-panel ans-instruction-text answer-array-list))
      ;;Put the BUTTON IN THE ALREADY CREATED FRAME
        (capi:apply-in-pane-process quest-rich-text-pane
                   #'(setf capi:rich-text-pane-text) question-text-formated quest-rich-text-pane |#
    ;;print to the buffer
    (with-slots (text-pane-1) object-list-buffer-inst
      (capi:apply-in-pane-process  text-pane-1
                                   #'(setf capi:rich-text-pane-text)   object-list-string text-pane-1)               
               
      ;;end let,with-open-file list-my-defclasses
      ;;end with-slots
      )
    (if return-object-list-string-p
        object-list-string)
    ;;end let, list-buffer-objects
    ))
;;TEST
;; (list-buffer-objects "SHAQ-new-scales" '("my-defclass"))


;;LIST-ALL-BUFFER-OBJECTS
;;
;;ddd
(defun list-all-buffer-objects (filename-no-ext )
  "In U-LW-editor, lists defparameter, defun, my-defclass, my-make-instance, and first-order nested-list keys"
  (list-buffer-objects filename-no-ext '( defparameter my-defclass my-make-instance defun))
 )
;;TEST 
;;  (list-all-buffer-objects "SHAQ-new-scales")


;;LIST-MY-DEFCLASSES
;;
;;ddd
(defun list-my-defclasses (filename-no-ext)
  "In U-LW-editor, lists my-defclass and my-make-instance objects"
  (list-buffer-objects filename-no-ext (list "my-defclass" "my-make-instance"))
 )

;;LIST-SCALECLASSES
;;
;;ddd
(defun list-scaleclasses ()
  "In U-LW-editor"
   (list-buffer-objects "SHAQ-new-scales" (list "my-defclass"))
       )
;; (list-scaleclasses)








           
;;BUFFER-LIST-FRAME
;;
;;ddd
(capi:define-interface buffer-list-frame ()
  ()
  (:panes
#|   (output-pane-1
    capi:output-pane)
   (collector-pane-1
    capi:collector-pane)|#
   (text-pane-1
    capi:rich-text-pane
   :visible-min-height 700
   :visible-min-width 350
    )
   (radio-button-panel-1
    capi:radio-button-panel
    :items '("Radio-Button-Panel-1" "Button 2" "Button 3")
    :max-width t
    :max-height t))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(text-pane-1 ;;output-pane-1 collector-pane-1  
                  radio-button-panel-1)))
  (:menu-bar menu-1)
  (:menus
   (menu-2
    "Menu-2"
    ())
   (menu-1
    "Menu-1"
    (menu-2
     "Item-1"
     "Item-2"
     "Item-3")))
  (:default-initargs
       :x 10
    :y 60
   :best-height nil
   :best-width nil
   :layout 'column-layout-1
   :title "Buffer List Frame"))
