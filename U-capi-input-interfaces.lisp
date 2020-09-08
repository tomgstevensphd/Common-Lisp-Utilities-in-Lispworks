;;*********************** U-capi-input-interfaces.lisp **************
;;
;;CAPI INTERFACES FOR VARIOUS TYPES OF STANDARD INPUTS

(unless (boundp 'my-confirm)
  (compile-file "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-capi.lisp" :load T))
;;
;; CAN BE USED WITHOUT MAKING SPECIAL NEW DEF FOR EACH USE

;;CONSIDER STANDARD CAPI PROMPT-FOR FUNCTIONS BELOW:
;;******************* H-capi-prompt-for ************
#|  

;;FILES
CAPI:PROMPT-FOR-DIRECTORY 
  (capi:prompt-for-directory "Select A Directory")
CAPI:PROMPT-FOR-FILE 
  (capi:prompt-for-file "Select A File")
  output= #P"C:/3-TS/LISP PROJECTS TS/H-Capi/H-Capi.lisp"   T   "All Files"
  ;;SEE MY-PROMPT-FOR-FILE in U-FILES can poke calling process and load file.

CAPI:PROMPT-FOR-FILES 
  (capi:prompt-for-files "Select Multiple? Files")
;;FONTS,etc
CAPI:PROMPT-FOR-COLOR 
   (capi:prompt-for-color "Select Color")
   output= #(:RGB 1.0 0.0 0.0 1.0)    T
CAPI:PROMPT-FOR-CONFIRMATION 
   (capi:prompt-for-confirmation "Confirm")
   ;;output= T or NIL
CAPI:PROMPT-FOR-FONT 
   (capi:prompt-for-font "Select FONT")
   ;;output= #S(GRAPHICS-PORTS:FONT :FONT-DESCRIPTION #S(GRAPHICS-PORTS:FONT-DESCRIPTION :ATTRIBUTES (:FAMILY "Arial" :WEIGHT :NORMAL :SLANT :ROMAN :SIZE 10 :UNDERLINE NIL :STRIKEOUT NIL :WIDTH 4.5 :CHARSET :ANSI :PITCH :VARIABLE :DEVICEP NIL :TYPE :TRUETYPE :W-FAMILY :SWISS)) :DEVICE-FONT NIL)   T

;;INPUTS
CAPI:PROMPT-FOR-FORM 
   (capi:prompt-for-form "Select A Form")
   ;;output= FORM-NAME-I-TYPED  T
CAPI:PROMPT-FOR-FORMS 
CAPI:PROMPT-FOR-INTEGER 
  (capi:prompt-for-integer  "Type an Integer Below")
  ;;output= 762  T
CAPI:PROMPT-FOR-ITEMS-FROM-LIST  (multiple-selection)
  (capi:prompt-for-items-from-list '("a" "b" "c" d e) "Select Items From List")
  ;;output= ("a" "c")   T
CAPI:PROMPT-WITH-LIST (single-selection)
  (capi:prompt-with-list '(a "b" c 6 7) "Select Items From List")
  ;;output= A  T
CAPI:PROMPT-FOR-NUMBER 
CAPI:PROMPT-FOR-STRING 
CAPI:PROMPT-FOR-SYMBOL 
CAPI:PROMPT-FOR-VALUE 

CAPI:PROMPT-WITH-LIST-NON-FOCUS 
;;This is a complex function with many alternatives--see args
   (capi:prompt-with-list-non-focus ???)
CAPI:PROMPT-WITH-MESSAGE
   (capi:prompt-with-message "This is the message/reminder")
   ;;output= 1
|#
    #|((selected-item-datalist
    :initarg :selected-item-datalist
    :accessor selected-item-datalist
    :initform nil
    :type :list
    :documentation "Data from selected item")
  (quest-num
    :initarg :quest-num
    :accessor quest-num
    :initform nil
    :type  :integer
    :documentation "Question number")
  (input-text-list
       :initarg :input-text-list
    :accessor input-text-list
    :initform nil
    :type  :list
    :documentation "List of input-text answers")
  )|#


;;GLOBAL VARIABLES
(when (or (not (boundp '*go-frame-button-width))
          (not (boundp '*text-input-OR-button-interface-best-height)))
  (defparameter *go-frame-button-height 25)
  (defparameter *go-frame-button-width 50)
  (defparameter *bio-text-button-font nil)
  (defparameter *text-input-OR-button-interface-textdata nil)
  (defparameter  *answer-pane-font-face 'times-roman)
  (defparameter *go-frame-button-font nil)
  (defparameter *current-qvar nil)
  (defparameter *run-csq-p nil)
  (defparameter *run-shaq-p nil)
  (defparameter *choice-list-interface-datalist nil)
  )

(defparameter *text-input-OR-button-interface-TITLE-PANE-BACKGROUND :white)
(defparameter *text-input-max-chars* 200 "Max characters for text-input-OR-button-interface instance")
(defparameter *text-input-OR-button-interface-best-width  600)
(defparameter *text-input-OR-button-interface-best-height  500)
(defparameter *text-input-OR-button-interface-textdata NIL)
(defparameter   *simple-input-instr-boldp NIL)

;; TO TERMINATE CURRENT LOOP-
(defparameter *terminate-current-loop-p NIL "called in (terminate-interface-callback")





;;TEXT-INPUT-OR-BUTTON-INTERFACE
;;
;;ddd
(capi:define-interface text-input-OR-button-interface ()
  ((sv-var1
    :initarg :sv-var1
    :accessor sv-var1
    :initform NIL
    :type  NIL
    :documentation  "Variable 1 to store misc info from calling function, etc"
    )
   (sv-var2
    :initarg :sv-var2
    :accessor sv-var2
    :initform NIL
    :type  NIL
    :documentation  "Variable 2 to store misc info from calling function, etc"
    )(sv-interface-subtype
    :initarg :sv-interface-subtype
    :accessor sv-interface-subtype
    :initform NIL
    :type  :symbol
    :documentation  "Subtype of interface :text-input :radio-button :check-button :info")
   (sv-selection-type
    :initarg :sv-selection-type
    :accessor sv-selection-type
    :initform NIL
    :type  :symbol
    :documentation  ":single-selection, :multiple-selection, or :extended-selection"    )
   (sv-text-input
    :initarg :sv-text-input
    :accessor sv-text-input
    :initform NIL
    :type  :string
    :documentation  "Data from text input")
   (sv-confirm-input-p
    :initarg :sv-confirm-input-p
    :accessor sv-confirm-input-p
    :initform NIL
    :type  :boolean
    :documentation  "Data from confirm-input-p")
   (sv-input-confirmed-p
    :initarg :sv-input-confirmed-p
    :accessor sv-input-confirmed-p
    :initform NIL
    :type  :boolean
    :documentation  "input-confirmed-p")
      ;;NOTE:  4 WAYS TO CLOSE CALLING PROCESS: 1. change it's slot value, 2. Directly close its process; 3. Directly close it's interface; 4. Poke it and let it close itself (can do with change slot-value of 'chose-this-process-p
   (sv-make-func-process
    :initarg :sv-make-func-process
    :accessor sv-make-func-process
    :initform NIL
    :documentation  "MP process from calling function/object.")
   (sv-poke-make-func-process-p
    :initarg :sv-poke-make-func-process-p
    :accessor sv-poke-make-func-process-p
    :type  :boolean
    :initform NIL
    :documentation  "poke-make-func-process-p ")
   (sv-close-make-func-process-p
    :initarg :sv-close-make-func-process-p
    :accessor sv-close-make-func-process-p
    :initform NIL
    :documentation  "If  T, callback closes calling-process at end.")
   (sv-close-interface-p
    :initarg :sv-close-interface-p
    :accessor sv-close-interface-p
    :initform NIL
    :documentation  "If  T, callback closes interface-process at end.")
   ;;sv-na-none-button-output 2019
   (sv-na-none-button-output
    :initarg :sv-na-none-button-output
    :accessor sv-na-none-button-output
    :initform *na-none-button-output ;; =? "NA-NONE"
    :documentation  "NA-NONE-BUTTON-OUTPUT")

#|   (sv-main-interface-process
    :initarg :sv-main-interface-process
    :accessor sv-main-interface-process
    :initform NIL
    :documentation  "Main-interface-process = THIS PROCESS")|#

   (sv-poke-interface-process-p
    :initarg :sv-poke-interface-process-p
    :accessor sv-poke-interface-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-interface-process-p ")
   (sv-destroy-main-interface-p
    :initarg :sv-destroy-main-interface-p
    :accessor sv-destroy-main-interface-p
    :type  :boolean
    :initform NIL
    :documentation  "If  T, callback closes destroy-main-interface-p at end.")
   (sv-special-process
    :initarg sv-special-process
    :accessor sv-special-process
    :initform NIL
    :documentation  "sv-special-process ")
   (sv-poke-special-process-p
    :initarg :sv-poke-special-process-p
    :accessor sv-poke-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-special-process-p ")
   (sv-close-special-process-p
    :initarg :sv-close-special-process-p
    :accessor sv-close-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-close-special-process-p ")
   )

  ;;((the-pane :accessor text-input-pane-test-pane))
  (:panes
      (title-pane
    capi:rich-text-pane
    :character-format  (list ;; :face *instr-pane-font-face 
                             :size  *instr-pane-font-size  
                             :color *instr-pane-font-color
                             :bold nil :italic nil :underline nil )
    :paragraph-format '(:alignment :center  ;; :left :right
                         :start-indent 20
                       :offset-indent 20
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops  '(5 10 15 20)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :accepts-focus-p NIL
    :character-format  (list ;; :face *instr-pane-font-face 
                             :size  *instr-pane-font-size  
                             :color *instr-pane-font-color
                             :bold nil :italic nil :underline nil )
    :paragraph-format '(:alignment :center  ;; :left :right
                         :start-indent 20
                       :offset-indent 20
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops  '(5 10 15 20)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :make-instance-extra-apply-args :title-pane-args
    :visible-border T
    :internal-border 15 
    :accepts-focus-p NIL
    ;;end title-pane
    )
   (instr-pane
    capi:rich-text-pane
    :character-format  (list ;; :face *instr-pane-font-face 
                             :size  *instr-pane-font-size  
                             :color *instr-pane-font-color
                             :bold nil :italic nil :underline nil )
    :paragraph-format '(:alignment :center  ;; :left :right
                         :start-indent 20
                       :offset-indent 20
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops  '(5 10 15 20)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :accepts-focus-p NIL
    :make-instance-extra-apply-args :instr-pane-args
    :visible-border T
    :internal-border 15
    :visible-min-height 40
    :accepts-focus-p NIL
    )
   (quest-pane
    capi:rich-text-pane
    :character-format  (list ;; :face *instr-pane-font-face 
                             :size  *instr-pane-font-size  
                             :color *instr-pane-font-color
                             :bold nil :italic nil :underline nil )
    :paragraph-format '(:alignment :center  ;; :left :right
                         :start-indent 20
                       :offset-indent 20
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops  '(5 10 15 20)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :accepts-focus-p NIL
    :make-instance-extra-apply-args :quest-pane-args
    :visible-border T
    :visible-min-height 40
    :internal-border 15
    :accepts-focus-p NIL
    )
   (text-input-pane
    capi:text-input-pane
    :make-instance-extra-apply-args :input-pane-args
    :max-characters *text-input-max-chars*
    :background :white
    :visible-border T
    :internal-border 15
    :take-focus T
    ) 
   ;;must include in layout programatically
   (radio-button-pane
    capi:radio-button-panel
    ;; :items  '("1"   "Other")
    :make-instance-extra-apply-args :radio-button-pane-args
    :title " Choose the best answer:"
    :title-font *bio-text-button-font
    :layout-class 'capi:column-layout
    ;;  :selected-item nil
    :selection nil
   ;; :callback-type :item-interface
    ;; :choice-selection nil
    :visible-min-width 400
    :background :yellow
    :font  *bio-text-button-font  #|(gp:make-font-description 
                        :family *answer-pane-font-face
                        :weight :normal  :size *answer-font-size)|#    
    )
   ;;must include in layout programatically
   (check-button-pane
    capi:check-button-panel
   :make-instance-extra-apply-args :check-button-pane-args
    ;; :items  '("1"   "Other")
    :title " Choose the best answer:"
    :title-font *bio-text-button-font
    :layout-class 'capi:column-layout
    ;;  :selected-item nil
    :selection nil
    ;; :choice-selection nil
    ;; :callback-type :item-interface
    :visible-min-width 400
    :background :yellow
    :font  *bio-text-button-font 
    )
   (go-fr-button
    capi:push-button
    :background :green
    :text  "       GO to next  >>      "
    :font  *go-frame-button-font 
    ;;   :color-requirements 
    ;;   :selected T
    :default-p T  ;;means if return hit, selects this button
    ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
    ;; choose in calling function
    ;;  :callback 'text-input-OR-button-interface-callback ;;'go-select-scales-frame-callback
    :callback-type :item-interface
    :make-instance-extra-apply-args :go-button-args
    )
   (na-none-button
    capi:push-button
    :background :green
    :text  " NOT APPLICABLE or NONE: GO to next >> "
    :font  *go-frame-button-font 
    :default-p NIL
    :callback-type :item-interface
           ;;NO   :callback 'terminate-interface-callback 
   :make-instance-extra-apply-args :na-none-button-button-args
   :callback 'NA-NONE-BUTTON-CALLBACK  ;;HERE66
  ;;   :make-instance-extra-apply-args :na-none-button-args
    )
   ;;end panes
   )   
  (:layouts
   (column-layout
    capi:column-layout
    '()
    :make-instance-extra-apply-args  :layout-args
    )
   ;;end layouts
   )
  (:default-initargs
   :best-width *text-input-OR-button-interface-best-width
   :best-height *text-input-OR-button-interface-best-height
   :layout 'column-layout
   :background :red
   :internal-border 15
    :visible-min-height 400
   )
  ;;END text-input-OR-button-interface
  )



;;MAKE-TEXT-INPUT-OR-BUTTON-INTERFACE-INSTANCE
;;
;;ddd
(defun make-text-input-OR-button-interface-instance 
       (&key (title "INPUT WINDOW" ) 
                                                          (title-bkgr ::yellow)(title-pane-ht 30) 
                                                          (title-font-size 14)(title-font-color :red)
                                                          (title-align :center)  title-pane-args    
                                                          (instr-text "Type INPUT below:") 
                                                          (multi-instr-text (format nil 
  "   ONLY TYPE ONE ANSWER-ITEM IN EACH INPUT WINDOW,
                Then select 'GO TO NEXT' button
       WHEN FINISHED, Select 'LAST LIST ITEM, goto next question' button."))
                                                          (instr-pane-ht 40) (instr-align :left)
                                                          (instr-font-size 12) (instr-font-color :black)
                                                          (instr-bkgr :light-blue)   instr-pane-args
                                                          (input-pane-text "")  input-pane-args 
                                                          (quest-pane-ht 40) (quest-border 15)
                                                          (quest-font-size 12) (quest-font-color :black)
                                                          (quest-font-color  :black)(quest-boldp T)
                                                          (quest-bkgr :light-blue)   quest-pane-args
                                                          (quest-align :left)
                                                          quest-text ;;leave NIL or creates pane
                                                          (interface-subtype :text-input)
                                                          ;;types=  :text-input :radio-button :check-button :info
                                                          radio-button-items
                                                          radio-button-pane-args
                                                          check-button-items
                                                          check-button-pane-args
                                                          go-button-args
                                                          INCL-NA-NONE-BUTTON-P ;;new 2019
                                                          layout-args   
                                                          (win-title "User Text Input Window")
                                                          (win-border 10)
                                                          (win-background :light-blue)  
                                                          win-args
                                                          confirm-input-p 
                                                          (callback 'text-input-OR-button-interface-callback)
                                                          (callback-type :item-interface)
                                                          use-selection-callback-p
                                                          (close-this-process-p T)
                                                          close-interface-p 
                                                          make-func-process
                                                          close-make-func-process-p 
                                                          (poke-make-func-process-p T)
                                                          special-process
                                                          poke-special-process-p
                                                          poke-interface-process-p 
                                                          terminate-button-p
                                                          (terminate-text
                                                           " LAST LIST ITEM-goto next question  ")
                                                          (terminate-callback 'terminate-interface-callback  )
                                                          na-none-button-p
                                                          (na-none-button-text 
                                                   " NOT APPLICABLE or NONE: GO to next >> ")
                                                          ;;here77
                                                          (na-none-callback
                                                           'terminate-interface-callback)  
                                                          na-button-args
                                                          (term-button-ht 20)
                                                          )
      ;;NOTE:  4 WAYS TO CLOSE CALLING PROCESS: 1. change it's slot value, 2. Directly close its process; 3. Directly close it's interface; 4. Poke it and let it close itself (can do with change slot-value of 'chose-this-process-p
  "In U-capi-input-interfaces,  A general purpose text input popup window.  SETS *TEXT-INPUT-OR-BUTTON-INTERFACE-TEXTDATA to the input. NOTE: If want to continue in a process, set the global var to NIL each time used, and proceed in calling process when global var is no longer nil? data not used "
  ;;FOR MULTI-TEXT INPUTS, CHANGE INSTRS
  (when terminate-button-p
    (setf instr-text multi-instr-text ))

     (let
         ((inst)
          (all-title-pane-args `(:visible-max-height ,title-pane-ht :visible-min-height ,title-pane-ht  :background ,title-bkgr :text ,title   ;;was ,title   
                                 :character-format (:face "times new roman"  :size  ,title-font-size
                                                    :color ,title-font-color  :bold T :italic  NIL :underline nil) 
                                 :paragraph-format  (:alignment ,title-align)))
          (all-instr-pane-args `(:visible-max-height ,instr-pane-ht
                                 :visible-min-height ,instr-pane-ht 
                                 :background ,instr-bkgr :text ,instr-text
                                 :character-format (:face "times new roman"  :size  ,instr-font-size
                                                    :color ,instr-font-color  :bold T :italic nil :underline nil) 
                                 :paragraph-format  (:alignment ,instr-align )))  ;; :offset 20)))
          (all-quest-pane-args)
          (all-args) 
          (this-process (mp:get-current-process))
          (na-none-button-button-args)
          (terminate-button)
          )
       ;;QUEST-PANE-ARGS
       (when quest-text
         (setf all-quest-pane-args
               `(:text ,quest-text :visible-min-height ,quest-pane-ht 
                 :internal-border ,quest-border
                 :character-format (:face "times new roman"  :size  ,quest-font-size 
                                    :color ,quest-font-color  :bold ,quest-boldp :italic nil :underline nil) 
                 :paragraph-format  (:alignment ,quest-align)
               :background ,quest-bkgr)))
       (when quest-pane-args
         (setf all-quest-pane-args (append all-quest-pane-args quest-pane-args)))

       (unless make-func-process
         (setf make-func-process (mp:get-current-process)))
       ;;reset callback output
       ;;(break  "b")
       (setf  *text-input-OR-button-interface-textdata nil
              ;;*make-text-proc  (mp:get-current-process)
              *close-input-proc Nil
              *2-button-input-interface-data  nil)

       ;;for text-input and check-boxs
       (unless (and radio-button-items  use-selection-callback-p)
         (setf go-button-args (list :go-button-args  
                                    (append  go-button-args `(:callback ,callback)))))
       ;;added 2019-12
       (when na-none-button-p
         (setf na-none-button-button-args (list :na-none-button-button-args
                  (append `(:callback ,na-none-callback :text na-none-button-text)
                          na-none-button-args)))) 
       ;;(break  "1")

       ;;COMBINE ARGS 
       (when title-pane-args
         (setf all-title-pane-args  (append all-title-pane-args title-pane-args)))

       (setf all-args (append all-args
                              (list :title-pane-args all-title-pane-args ) go-button-args))

       (when instr-pane-args
         (setf all-instr-pane-args  (append all-instr-pane-args instr-pane-args)))
       (setf all-args (append all-args  (list :instr-pane-args all-instr-pane-args)))
    
       (when all-quest-pane-args
         (setf all-args (append all-args (list :quest-pane-args all-quest-pane-args))))

       (when input-pane-args
         (setf all-args (append all-args (list :input-pane-args input-pane-args))))
    
       (when radio-button-items
         (cond
          (use-selection-callback-p
           (setf  interface-subtype :radio-button
                  radio-button-pane-args (append radio-button-pane-args 
                                                 (list :items radio-button-items :selection-callback callback ))))
          ;;IF NEXT BUTTON--USE ITS CALLBACK instead of :selection-callback??
          (T (setf  interface-subtype :radio-button
                    radio-button-pane-args (append radio-button-pane-args 
                                                   (list :items radio-button-items))))))   
                 
       ;;(break  "2")
       (when radio-button-pane-args
         (setf all-args (append all-args (list :radio-button-pane-args  radio-button-pane-args))))
       ;;(break "all radio args")
                                               
       (when check-button-items
         (setf   interface-subtype :check-button
                 check-button-pane-args (append check-button-pane-args 
                                                (list :items check-button-items))))      
       (when check-button-pane-args
         (setf all-args (append all-args 
                                (list :check-button-pane-args  check-button-pane-args))))

       (when terminate-button
         (setf layout-args (append layout-args (list terminate-button))))

       (when layout-args
         (setf all-args (append all-args  (list :layout-args layout-args))))

       #|      (capi:apply-in-pane-process answer-column-layout
                                  #'(setf capi:layout-description)
                                          (list answer-button-panel)                                            
                                          answer-column-layout)|#
       ;;(break  "3")
       ;;add window args
       (cond
        (win-args
         (setf all-args (append all-args  (list :title win-title :internal-border win-border
                                              :background win-background) win-args)))
        (t (setf all-args (append all-args  (list :title win-title :internal-border win-border
                                              :background win-background )))))

       ;;(BREAK "all-args, poke-special-process")

       (setf  inst (apply  'make-instance  'text-input-OR-button-interface  all-args))
       ;;(break "after setf inst")

       ;;Must use with-slots AFTER making instance
       (with-slots (column-layout title-pane  instr-pane text-input-pane 
                                  quest-pane radio-button-pane check-button-pane  
                                  go-fr-button na-none-button) inst

         ;;SET SLOT VALUES
         (unless interface-subtype
           (setf interface-subtype :text-input))
         (setf (slot-value inst 'sv-interface-subtype) interface-subtype)
         (when confirm-input-p
           (setf (slot-value inst 'sv-confirm-input-p) T))
         (when poke-special-process-p
           (setf (slot-value inst 'sv-poke-special-process-p) T))
         (when special-process
           (setf (slot-value inst 'sv-special-process) special-process))    
         (when close-interface-p
           (setf (slot-value inst 'sv-close-interface-p) T))
         (when make-func-process
           (setf (slot-value inst 'sv-make-func-process) make-func-process))      
         (when close-make-func-process-p 
           (setf (slot-value inst 'sv-close-make-func-process-p) T))
         (when poke-make-func-process-p
           (setf  (slot-value inst 'sv-poke-make-func-process-p) T))    
         (when poke-interface-process-p
           (setf (slot-value inst 'sv-poke-interface-process-p) T))
#|  done above       (when interface-subtype
           (setf (slot-value inst 'sv-interface-subtype) interface-subtype))|#
         ;;(BREAK "sv-confirm-input-p")
         ;;SET PANE ATTRIBUTES
         (cond  ;;here66
          (radio-button-items
           (setf (capi:layout-description column-layout) 
                 (list title-pane  instr-pane  radio-button-pane )))
          (check-button-items
           (setf (capi:layout-description column-layout) 
                 (list title-pane  instr-pane  check-button-pane go-fr-button )))
          (terminate-button-p
           (setf terminate-button (make-instance 'capi:push-button
                                                 :text terminate-text
                                                 :font  *go-frame-button-font 
                                                 :visible-min-height term-button-ht
                                                 :callback 'terminate-interface-callback 
                                                 :callback-type :item-interface ))
           ;; USE?? (capi:element-interface-for-callback terminate-button)
           (cond
            (quest-text  
             (cond
              (incl-na-none-button-p
               (setf (capi:layout-description column-layout)
                     (list title-pane  instr-pane quest-pane
                           text-input-pane go-fr-button terminate-button na-none-button )))
              (t (setf (capi:layout-description column-layout)
                       (list title-pane  instr-pane quest-pane
                             text-input-pane go-fr-button terminate-button  ))))
             )
            (t 
             (cond
              (incl-na-none-button-p
               (setf (capi:layout-description column-layout)
                     (list title-pane  instr-pane text-input-pane go-fr-button
                           terminate-button na-none-button)))
              (t (setf (capi:layout-description column-layout)
                     (list title-pane  instr-pane text-input-pane go-fr-button
                           terminate-button ))))))
           ;;end terminate button
           )
          (t 
           ;; USE?? (capi:element-interface-for-callback terminate-button)
           (cond
            (quest-text  
             (cond 
              (incl-na-none-button-p
               (setf (capi:layout-description column-layout)
                     (list title-pane  instr-pane quest-pane
                           text-input-pane go-fr-button na-none-button )))
              (t (setf (capi:layout-description column-layout)
                       (list title-pane  instr-pane quest-pane
                             text-input-pane go-fr-button))))
             )
            (t 
             (cond 
              (incl-na-none-button-p
               (setf (capi:layout-description column-layout)
                     (list title-pane  instr-pane text-input-pane go-fr-button na-none-button )))
              (t
               (setf (capi:layout-description column-layout)
                     (list title-pane  instr-pane text-input-pane go-fr-button ))))))
           ;;end t, cond
           ))

         (when input-pane-text
           (capi:apply-in-pane-process  inst
                                        #'(setf capi:text-input-pane-text) 
                                        input-pane-text text-input-pane))

         ;;INCLUDE NA-NONE BUTTON?
         (when INCL-NA-NONE-BUTTON-P
           (capi:apply-in-pane-process  inst
                                        #'(setf capi:text-input-pane-text) 
                                        input-pane-text text-input-pane))
         ;;WHEN QUEST-TEXT, add it, done with :text initarg
#|         (when quest-text
           (capi:apply-in-pane-process  inst
                                        #'(setf capi:rich-text-pane-text) 
                                        input-pane-text quest-pane))|#

         ;;include a terminate button (mostly for multiple-inputs)
          
         ;;end with-slots
         )
       ;;DISPLAY INTERFACE
       (capi:display inst)

       (unless close-this-process-p
         (mp:current-process-pause 3600))

       ;;PUT MORE CODE HERE IF WANT TO MANAGE OTHER ASPECTS OF INTERFACES WITHOUT USING ALL CALLBACKS (MUST BE POKED BY A CALLBACK)
       ;;PUT ADDITIONAL CODE-PROCESSES HERE?

       ;;end let, make-text-input-OR-button-interface-instance
       ))
;;TEST
;;  (Make-text-input-OR-button-interface-instance) = WORKS
;; (make-text-input-OR-button-interface-instance :confirm-input-p T)
;;  (make-text-input-OR-button-interface-instance :terminate-button-p T)

;; (Make-text-input-OR-button-interface-instance :incl-na-none-button-p T)
;;  (make-text-input-OR-button-interface-instance :terminate-button-p T  :incl-na-none-button-p T)

;; (make-text-input-OR-button-interface-instance :win-title "NEW TITLE" :title"Title Pane Text"  :title-bkgr :green :title-pane-ht 40 :title-font-size 18 :title-font-color :red  :title-align :right :instr-text "New instr text" :instr-font-color :red :instr-bkgr :white)
;; above works

;; (Make-text-input-OR-button-interface-instance :interface-subtype :radio-button :radio-button-items '("item 1" "item 2 " "item 3"))
;; works   *TEXT-INPUT-OR-BUTTON-INTERFACE-TEXTDATA = ("item 3")
;;
;; (Make-text-input-OR-button-interface-instance :interface-subtype :check-button :check-button-items '("item 1" "item 2 " "item 3"))
;; works   *TEXT-INPUT-OR-BUTTON-INTERFACE-TEXTDATA  = ("item 1" "item 2 ")




;;TEXT-INPUT-OR-BUTTON-INTERFACE-CALLBACK
;;
;;ddd
(defun text-input-OR-button-interface-callback (item interface)
  (let
      ((confirm-call-process (mp:get-current-process))
       (close-window-p)
       (interface-subtype)
       (special-process (slot-value interface 'sv-special-process))
       (poke-special-process-p (slot-value interface  'sv-poke-special-process-p))
       (make-func-process (slot-value interface 'sv-make-func-process))
       (poke-make-func-process-p (slot-value interface  'sv-poke-make-func-process-p))
      (confirm-input-p)
       (yes? T)
       )
    ;;(Break "callback 1-INFINITE LOOP")
    ;;reset
     (setf *text-input-OR-button-interface-textdata NIL)
    (with-slots (text-input-pane instr-pane radio-button-pane check-button-pane) interface

      ;;SET SLOT-VALUES
      (setf  interface-subtype (slot-value interface 'sv-interface-subtype)) 

      ;;SET THE RESULT GLOBAL VARIABLE
      ;;(break "subtype")
      (cond
       ((equal interface-subtype :text-input)
        (setf *text-input-OR-button-interface-textdata
              (capi:text-input-pane-text text-input-pane)))
       ((equal interface-subtype :radio-button)
        ;;not needed? (setf choice-type (slot-value interface 'sv-selection-type))
        (setf *text-input-OR-button-interface-textdata
             (capi:choice-selected-items radio-button-pane) ))
       ((equal interface-subtype :check-button)
        (setf *text-input-OR-button-interface-textdata
              (capi:choice-selected-items check-button-pane)))
       (t nil))
       ;;(break"when")
      ;;CONFIRM THE INPUT?
      ;;(BREAK "sv-confirm-input-p")
      (when (slot-value interface 'sv-confirm-input-p)  
        (setf confirm-input-p T
              yes? (my-confirm (format nil "YOUR TEXT INPUT=  ~A" *text-input-OR-button-interface-textdata) :QUESTION-STRING "IS YOUR TEXT INPUT CORRECT?"))
        ;;end when
        )
      ;;(break "slot-values")
      ;;TAKE ACTIONS SPECIFIED IN INTERFACE SLOTS
        (when (or (null confirm-input-p)
                  (and confirm-input-p yes?))
          (cond
           ((equal interface-subtype :text-input)
            (setf *text-input-OR-button-interface-textdata
                  (capi:text-input-pane-text text-input-pane))
            ;;(break "yes")
            )
           ((equal interface-subtype :radio-button)
            ;;not needed? (setf choice-type (slot-value interface 'sv-selection-type))
            (setf *text-input-OR-button-interface-textdata
                  (capi:choice-selected-items radio-button-pane) ))
           ((equal interface-subtype :check-button)
            (setf *text-input-OR-button-interface-textdata
                  (capi:choice-selected-items check-button-pane)))
           (t nil))
          ;;(break "before poke")
        ;;POKE TO GO TO NEXT QUESTION?
        ;;(format t "In callback, cur-proc= ~A~% make-func-process= ~A" (mp:get-current-process) make-func-process)
        ;;pause, sometimes pokes before *text-input-OR-button-interface-textdata ready??
        (sleep 1)
          (when (and poke-make-func-process-p make-func-process)
          ;; (break "poke here")
            (mp:process-poke make-func-process))
          (when (and poke-special-process-p special-process)
            (mp:process-poke special-process))
          (when (slot-value interface 'sv-poke-interface-process-p)
            (mp:process-poke interface))
    
          ;;(break "after poke, close now")
          (capi:destroy interface)
         ;;end when (or (null confirm-input-p) yes?)
          )
        ;;(break "end text-input-OR-button-interface-callback")
        *TEXT-INPUT-OR-BUTTON-INTERFACE-TEXTDATA
        ;;end with-slots,let, text-input-OR-button-interface-callback
        )))
;;TEST
;;  (Make-text-input-OR-button-interface-instance) = WORKS
;;   *text-input-OR-button-interface-textdata  =  "this"
;; (make-text-input-OR-button-interface-instance :confirm-input-p T) = works

;;  (make-text-input-OR-button-interface-instance  :title "NEW TITLE" :title-bkgr :YELLOW :title-pane-ht 30 :title-gp-font-list  (gp:make-font-description :size 16) :layout-args '(:background :blue) :window-args '(:title "Window Title"  :visible-min-height  400))

;;SUCCESSFUL TEST OF A MP MANAGER FUNCTION THAT CALLS MAKE-INTERFACE FUNCTIONS THAT POKE THE MANAGER TO CONTINUE

;; ( my-call-input-text "THIS IS TEST TEXT" :title "TEST TITLE" :close-interface-p T)
;; THIS CALLED THE MAKE-INTERFACE FUNCTION AND WAS SUCCESSFULLY PAUSED AND POKED by the callbacks from the other function.
;; SO THIS SUCCESSFULLY MODELS A MANAGER FUNCTION THAT YOU WANT TO CONTROL TIMING OF INTERFACES
  



;;NA-NONE-BUTTON-CALLBACK
;; DOESN'T WORK==>USE TEXT-INPUT-OR-BUTTON-INTERFACE-CALLBACK INSTEAD [leave up to calling function what to do with blank answer]
;;2019
;;ddd
(defun na-none-button-callback (item interface)
  (let
      ((confirm-call-process (mp:get-current-process))
       (special-process (when (slot-exists-p   interface 'sv-special-process)
                          (slot-value interface 'sv-special-process)))
       (poke-special-process-p (when (slot-exists-p   interface 'sv-poke-special-process-p)
                                 (slot-value interface  'sv-poke-special-process-p)))
       (make-func-process (when (slot-exists-p   interface 'sv-make-func-process)
                          (slot-value interface 'sv-make-func-process)))
       (poke-make-func-process-p
        (when (slot-exists-p   interface 'sv-poke-make-func-process-p)
                                   (slot-value interface  'sv-poke-make-func-process-p)))
       (na-none-button-output (when (slot-exists-p   interface 'sv-na-none-button-output)
                          (slot-value interface 'sv-na-none-button-output)))
       )
      ;;SET THE RESULT GLOBAL VARIABLE
      ;;FOR NA-NONE OUTPUT
      (setf *text-input-OR-button-interface-textdata na-none-button-output)          
              ;;(break "before poke")
      ;;TERMINATE CURRENT FRAME
      (setf *terminate-current-loop-p T)

        ;;POKE TO GO TO NEXT QUESTION?
        ;;(format t "In callback, cur-proc= ~A~% make-func-process= ~A" (mp:get-current-process) make-func-process)
          (when (and poke-make-func-process-p make-func-process)
          ;;(break "poke here")
            (mp:process-poke make-func-process))
          (when (and poke-special-process-p special-process)
            (mp:process-poke special-process))
          (when (slot-value interface 'sv-poke-interface-process-p)
            (mp:process-poke interface))

     ;;(text-input-OR-button-interface-callback item interface)
          ;;(break "after poke, close now")
          (capi:destroy interface)
        ;;(break "end text-input-OR-button-interface-callback")
        *text-input-OR-button-interface-textdata
        ;;end with-slots,let, na-none-button-callback
        ))
;;TEST
;;  (make-text-input-OR-button-interface-instance :terminate-button-p T  :incl-na-none-button-p T) = NIL
;; works: *text-input-OR-button-interface-textdata  =  "NA-NONE"
;;interface= #<TEXT-INPUT-OR-BUTTON-INTERFACE " Cognitive Systems Questionnaire (CSQ) " 293CBAD3>

;;FROM BUTTON
;; special process= #<MP:PROCESS Name "CAPI Execution Listener 7" Priority 0 State "Paused">
;; make-func-process= #<MP:PROCESS Name "CAPI Execution Listener 7" Priority 0 State "Paused">
;; pokes both T
;; FROM MAIN FUNC
;; MAKE-FUNC-PROC= #<MP:PROCESS Name "CAPI Execution Listener 7" Priority 0 State "Paused">
;; SPECIAL-PROC = #<MP:PROCESS Name "CAPI Execution Listener 7" Priority 0 State "Paused">
;; POKE-SPEC=T, POKE-MAIN= NIL
;; THIS-FUNC-PROC= #<MP:PROCESS Name "CAPI Execution Listener 7" Priority 0 State "Paused">


;;MY-CALL-INPUT-TEXT
;;
;;ddd
(defun my-call-input-text (text &key (title "INPUT WINDOW") 
                                (win-title "INPUT WINDOW") close-interface-p
                                (poke-special-process-p T))
   "U-capi-input-interfaces.  Sets *TEXT-INPUT-OR-BUTTON-INTERFACE-TEXTDATA to input. "
  (let*
      ((cur-process (mp:get-current-process))  ;;SHOWS IF NIL
       (instance  (make-text-input-OR-button-interface-instance
                   :title "TYPE YOUR INPUT BELOW"
                   :instr-text title
                   :win-title win-title
                   :interface-subtype :text-input
                   :callback 'text-input-or-button-interface-callback
                   :callback-type :item-interface
                   :input-pane-text text
                   :confirm-input-p t  
                   :close-this-process-p t :close-interface-p close-interface-p
                   :poke-special-process-p poke-special-process-p
                    :special-process cur-process
                   :poke-make-func-process-p t :poke-interface-process-p nil))
                 ;;OTHER ARGS
       #|(title-bkgr :yellow) (title-pane-ht 30) (title-font-size 14) (title-font-color :red) (title-align :center) title-pane-args 
 (instr-pane-ht 40) (instr-align :left) (instr-font-size 12) (instr-font-color :black) (instr-bkgr :light-blue) instr-pane-args
   input-pane-args  
   radio-button-items
    radio-button-pane-args 
   check-button-items 
   check-button-pane-args   go-button-args 
   layout-args 
   (window-args (quote (:title "User Text Input Window" :internal-border 10 :background :light-blue))) |#
       )
    (print "BEFORE PAUSE")
    (mp:current-process-pause 20)

    ;;(break "after process pause")
    "THIS IS THE END"
    ))


;;  (make-text-input-OR-button-interface-instance :title "INPUT WINDOW"     :title-bkgr :pink  :title-pane-ht 20        :title-font-size 16    :title-font-color :red    :title-align :center  :title-pane-args  NIL  :instr-text "Type INPUT below:"   :instr-pane-ht 60     :instr-align :right           :instr-font-size 14      :instr-font-color :maroon    :instr-bkgr :cyan     :instr-pane-args  NIL :input-pane-text  "Input pane text"  :input-pane-args nil    :layout-args '(:background :green)) ;;ERROR:   :window-args '(:visible-min-width 500 :title "Window New Title"))

;;  FOR RADIO AND CHECK BUTTONS
;;  (make-text-input-OR-button-interface-instance :confirm-input-p T) 
;;  (Make-text-input-OR-button-interface-instance  :check-button-items '("A" "B" "C" "D" 1 2 3 4 5))
;; works  *text-input-OR-button-interface-textdata  = ("B" 4)
;;  (Make-text-input-OR-button-interface-instance  :radio-button-items '("A" "B" "C" "D" 1 2 3 4 5))
;; works:   *text-input-OR-button-interface-textdata = (1)


;;NOTE
;; (funcall 'list  'a 'b 'c) = (a b c)
;; (funcall 'list '(a b c)) = ((a b c))
;; (apply 'list 'a '(b c))  = (a b c)

;; ==== end text-input-OR-button-interface

;;here3



;;MAKE-INPUT-QUESTIONS
;;2018
;;ddd
(defun make-input-questions (qlists &key (default-func 'input-or-buttons)
                                    (default-title "Type you answer below:" ) 
                                    (win-title "  SINGLE ANSWER INPUT")
                                                          (title-bkgr :yellow)(title-pane-ht 30) 
                                                          (title-font-size 14)(title-font-color :red)
                                                          (title-align :center)  title-pane-args    
                                                         ;; (instr-text "Type INPUT below:") 
                                                          (instr-pane-ht 40) (instr-align :left)
                                                          (instr-font-size 12) (instr-font-color :black)
                                                          (instr-bkgr :light-blue)   instr-pane-args
                                                          (input-pane-text "")  input-pane-args 
                                                          (interface-subtype :text-input)
                                                          ;;types=  :text-input :radio-button :check-button :info
#|                                                          radio-button-items
                                                          radio-button-pane-args
                                                          check-button-items
                                                          check-button-pane-args|#
                                                          go-button-args
                                                          layout-args   
                                                          (win-border 10)
                                                          (win-background :light-blue)
                                                          confirm-input-p 
                                                          (callback 'text-input-OR-button-interface-callback)
                                                          (callback-type :item-interface)
                                                          (close-this-process-p T)
                                                          close-interface-p 
                                                          close-make-func-process-p
                                                          (make-func-process (mp:get-current-process))
                                                          (poke-make-func-process-p T)
                                                          poke-special-process-p
                                                          special-process
                                                          poke-interface-process-p 
                                                          )
  "   RETURNS output-list INPUT: qlist= (question title func q-instr ... ) qlists can vary with func. Types of functions: input-or-buttons, rest args= title/nil question);  qlist= question if use default-func and default-title. FUNCS are either input-or-buttons, ....add to list...  POPS UP A QUESTION AND RETURNS  NOTES: 1.No quote on func multi-input, etc "
  (let
      ((answers)
       (n-qlists (list-length qlists))
       (all-outputs)
       )
    (loop
     for qlist in qlists
     for n from 1 to n-qlists
     do
     (let*
         ((question)
          (q-instrs)
          (title)
          (func)
          (qlist-n)
          (result1)
          (result2)
          )
    ;;FIND FUNC
    (cond
     ((listp qlist)
      (setf qlist-n (list-length qlist)
            func (third qlist))
      (when (null func)
        (setf func default-func))
      ;;ADDITIONAL ARGS?
      (when (> qlist-n 3)
        (when (member :confirm qlist)  ;;if don't ck member, sets to NIL
          (setf confirm-input-p (get-key-value :confirm qlist)))
        ;;ADD ADDITIONAL KEY/VALUES IN QLIST HERE
        ))
      ;;end listp
      (t (setf func default-func)))

     ;;FIND FUNC ARGS and EVAL
     (cond
      ;;FOR SINGLE INPUT OR BUTTONS QUESTIONS
      ((equal func 'input-or-buttons)
       (cond
        ((listp qlist)
         (setf question (car qlist))
         (unless title (setf title (second qlist)))
         )
        (t (setf question qlist 
                 title default-title)))
       ;;eval the function
       (multiple-value-setq (result1 result2)
           (make-text-input-or-button-interface-instance :instr-text question  :title title
                                                         :win-title "SINGLE INPUT WINDOW"
                                                         :win-border win-border
                                                         :win-background win-background
                                                         :close-interface-p close-interface-p 
                                                         :callback callback
                                                         :callback-type callback-type
                                                         :confirm-input-p confirm-input-p))
       ;;(break "after mvs")
       (mp:current-process-pause 100)
       (setf all-outputs (append all-outputs (list *text-input-OR-button-interface-textdata)))
       ;;(list result1 result2)))
       )
      ;;FOR MULTI-INPUT QUESTIONS
      ((equal func 'multi-input)
       (cond
        ((listp qlist)
         (setf question (car qlist))
         (unless title (setf title (second qlist)))
         )
        (t (setf question qlist 
                 title default-title)))
         (multiple-value-setq (result1)
             (make-multi-input-frames question :default-title title 
                                      :win-title "TYPE ONLY ONE INPUT BELOW:"
                                      :win-background #(:RGB 0.5019608 1.0 0.0 1.0)
                                      :confirm-input-p confirm-input-p))
         (setf all-outputs (append all-outputs (list result1)))
         ;;end mult-input
         )
      ;;SSS ADD OTHER QUESTION FUNCTIONS HERE
      (t nil))

     ;;TERMINATE LOOP? called with terminate-interface-callback
     ;;NO, cuts off before all lists processed if a multi-input is included (when *terminate-current-loop-p     (return))
     ;;end let loop
     ))
    ;;(break)
    all-outputs
    ;;end let, make-input-questions
    ))
;;TEST
;; (make-input-questions '( "First Quest" "Second Quest"))
;; works= ("first answer" "second answer")
;; (make-input-questions '( "First Quest" "Second Quest") :confirm-input-p T)
;; works= ("yes 1" "yes 2") after answering "no 1" and "no 2" first on each.
;; FOR MULTI-INPUT INCLUDED
;;  (make-input-questions '( "First Quest"  ("Multi input question" "Multi-input Instructions" multi-input) "Third Quest"))
;; works= ("1" ("m1" "last") "3")
;;;; (make-input-questions '( ("Multi input question" "Multi-input Instructions" multi-input)))
;; works = (("m1" "m2" "m3" "last"))
;; (make-input-questions '( "First Quest" "Second Quest" ("Multi input question" "Multi-input Instructions" multi-input) "Third Quest"))
;; works= ("ans1" "ans2" ("m1" "m2" "last") "ans 3")










;;INFO-POPUP-INTERFACE  ----------------------------------------------------------------------------
;;
;;ddd
(capi:define-interface info-popup-interface ()
  ((sv-var1
    :initarg :sv-var1
    :accessor sv-var1
    :initform NIL
    :type  NIL
    :documentation  "Variable 1 to store misc info from calling function, etc"
    )
   (sv-var2
    :initarg :sv-var2
    :accessor sv-var2
    :initform NIL
    :type  NIL
    :documentation  "Variable 2 to store misc info from calling function, etc"
    )(sv-interface-subtype
    :initarg :sv-interface-subtype
    :accessor sv-interface-subtype
    :initform NIL
    :type  :symbol
    :documentation  "Subtype of interface :text-input :radio-button :check-button :info")
   (sv-text-input
    :initarg :sv-text-input
    :accessor sv-text-input
    :initform NIL
    :type  :string
    :documentation  "Data from text input")
   (sv-confirm-input-p
    :initarg :sv-confirm-input-p
    :accessor sv-confirm-input-p
    :initform NIL
    :type  :boolean
    :documentation  "Data from confirm-input-p")
   (sv-input-confirmed-p
    :initarg :sv-input-confirmed-p
    :accessor sv-input-confirmed-p
    :initform NIL
    :type  :boolean
    :documentation  "input-confirmed-p")
      ;;NOTE:  4 WAYS TO CLOSE CALLING PROCESS: 1. change it's slot value, 2. Directly close its process; 3. Directly close it's interface; 4. Poke it and let it close itself (can do with change slot-value of 'chose-this-process-p
      (sv-close-make-func-process-p
    :initarg :sv-close-make-func-process-p
    :accessor sv-close-make-func-process-p
    :initform NIL
    :documentation  "If  T, callback closes calling-process at end.")
      (sv-close-interface-p
    :initarg :sv-close-interface-p
    :accessor sv-close-interface-p
    :initform NIL
    :documentation  "If  T, callback closes interface-process at end.")
   (sv-make-func-process
    :initarg :sv-make-func-process
    :accessor sv-make-func-process
    :initform NIL
    :documentation  "MP process from calling function/object.")
#|   (sv-main-interface-process
    :initarg :sv-main-interface-process
    :accessor sv-main-interface-process
    :initform NIL
    :documentation  "Main-interface-process = THIS PROCESS")|#
   (sv-poke-interface-process-p
    :initarg :sv-poke-interface-process-p
    :accessor sv-poke-interface-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-interface-process-p ")
   (sv-poke-make-func-process-p
    :initarg :sv-poke-make-func-process-p
    :accessor sv-poke-make-func-process-p
    :type  :boolean
    :initform NIL
    :documentation  "poke-make-func-process-p ")
   (sv-destroy-main-interface-p
    :initarg :sv-destroy-main-interface-p
    :accessor sv-destroy-main-interface-p
    :type  :boolean
    :initform NIL
    :documentation  "If  T, callback closes destroy-main-interface-p at end.")
   (sv-poke-special-process-p
    :initarg :sv-poke-special-process-p
    :accessor sv-poke-special-process-p
        :type  :boolean
    :initform NIL
    :documentation  "sv-poke-special-process-p ")
(sv-close-special-process-p
    :initarg :sv-close-special-process-p
    :accessor sv-close-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-close-special-process-p ")
   (sv-special-process
    :initarg sv-special-process
    :accessor sv-special-process
    :initform NIL
    :documentation  "sv-special-process ")

   )
  ;;PANES
  (:panes
   (title-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :make-instance-extra-apply-args :title-pane-args
    :visible-border T
    :internal-border 20 
    ;;end title-pane
    )
   (info-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :make-instance-extra-apply-args :info-pane-args
    :visible-border T
    :internal-border 20
    )
   (go-fr-button
    capi:push-button
    :background :green
    :text  "       GO to next  >>      "
    :font  *go-frame-button-font 
    :default-p T  ;;means if return hit, selects this button
    :make-instance-extra-apply-args :go-button-args
    ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
    ;; choose in calling function
    ;;  :callback 'text-input-OR-button-interface-callback ;;'go-select-scales-frame-callback
    :callback-type :item-interface
    )

   ;;end panes
   )   
  (:layouts
   (column-layout
    capi:column-layout
    '(   title-pane  info-pane    button-row-layout  )
    :make-instance-extra-apply-args :layout-args
    )
   (button-row-layout
    capi:row-layout
    '(go-fr-button))
   ;;end layouts
   )
  (:default-initargs
   :best-width *text-input-OR-button-interface-best-width
   :best-height *text-input-OR-button-interface-best-height
   :layout 'column-layout
   )
  ;;END info-popup-interface
  )




;;MAKE-INFO-POPUP-INTERFACE-INSTANCE
;;
;;ddd
(defun make-info-popup-interface-instance (&key (title "INFORMATION" ) 
                                                (title-bkgr ::yellow)(title-pane-ht 30) 
                                                (title-font-size 14)(title-font-color :red)
                                                (title-align :center)  title-pane-args    
                                                (info-text "Info goes here") 
                                                (info-pane-ht 60) (info-align :left)
                                                (info-font-size 12) (info-font-color :black)
                                                (info-bkgr :light-blue)   info-pane-args
                                                layout-args   
                                                (window-args '(:title "INFORMATION WINDOW"
                                                               :visible-min-width 400 
                                                               :internal-border 10 :background :light-blue))
                                                confirm-input-p
                                                (callback 'text-input-OR-button-interface-callback)
                                                (callback-type  :item-interface)
                                                poke-special-process-p
                                                special-process
                                                interface-subtype
                                                close-make-func-process-p
                                                make-func-process
                                                      (close-this-process-p T)
                                                          close-interface-p 
                                                          (poke-make-func-process-p T)
                                                          poke-interface-process-p 
                                                )                                 
  "In U-capi-input-interfaces,  A general purpose text input popup window.  Sets *text-input-OR-button-interface-textdata to the input. NOTE: If want to continue in a process, set the global var to NIL each time used, and proceed in calling process when global var is no longer nil? data not used . Sets answer to global *TEXT-INPUT-OR-BUTTON-INTERFACE-TEXTDATA"
  (let
      ((inst)
       (all-title-pane-args `(:visible-max-height ,title-pane-ht :visible-min-height ,title-pane-ht  :background ,title-bkgr :text ,title   :character-format (:face "times new roman"  :size  ,title-font-size  :color ,title-font-color  :bold T :italic  NIL :underline nil)  :paragraph-format  (:alignment ,title-align)))
       (all-info-pane-args `(:visible-max-height ,info-pane-ht :visible-min-height ,info-pane-ht  :background ,info-bkgr :text ,info-text :character-format (:face "times new roman"  :size  ,info-font-size  :color ,info-font-color  :bold T :italic nil :underline nil)  :paragraph-format  (:alignment ,info-align)))
       (all-args) 
       (button-args)
       (this-process (mp:get-current-process))
       )

    ;;COMBINE ARGS
    (when title-pane-args
      (setf all-title-pane-args  (append all-title-pane-args title-pane-args)))
    (setf all-args (append all-args  (list :title-pane-args all-title-pane-args)))

    (when info-pane-args
      (setf all-info-pane-args  (append all-info-pane-args info-pane-args)))
    (setf all-args (append all-args  (list :info-pane-args all-info-pane-args)))

    (when layout-args
      (setf all-args (append all-args  (list :layout-args layout-args))))
    (when window-args
      (setf all-args (append all-args  window-args)))   

    ;;here1
    (setf button-args `(:go-button-args  (:callback ,callback :callback-type ,callback-type))
          all-args (append all-args button-args))

    ;;(BREAK "all-args")    
    (setf  inst (apply  'make-instance  'info-popup-interface  all-args))
           
    ;;SET SLOT VALUES
    (when confirm-input-p
      (setf (slot-value inst 'sv-confirm-input-p) T))
    (when poke-special-process-p
      (setf (slot-value inst 'sv-poke-special-process-p) T))
    (when special-process
      (setf (slot-value inst 'sv-special-process) special-process))
    (when close-interface-p
      (setf (slot-value inst 'sv-close-interface-p) T))
    (setf close-make-func-process-p   (slot-value inst 'sv-close-make-func-process-p)
          make-func-process (slot-value inst 'sv-make-func-process))  
    (when poke-interface-process-p
      (setf (slot-value inst 'sv-poke-interface-process-p) T))
    (setf  (slot-value inst 'sv-make-func-process) this-process
           (slot-value inst 'sv-poke-make-func-process-p) T)
    (when interface-subtype
      (slot-value inst 'sv-interface-subtype) interface-subtype)

    (capi:display inst)
    inst
    ;;end let, make-info-popup-interface-instance
    ))
;;TEST
;;  

;;   :info-pane-args '(:internal-border 25))  ;;doesn't add any border to rich text pane





;;2-BUTTON-POPUP-INTERFACE
;;
;;ddd
(capi:define-interface 2-BUTTON-POPUP-INTERFACE ()
  ((sv-var1
    :initarg :sv-var1
    :accessor sv-var1
    :initform NIL
    :type  NIL
    :documentation  "Variable 1 to store misc info from calling function, etc"
    )
(sv-var2
    :initarg :sv-var2
    :accessor sv-var2
    :initform NIL
    :type  NIL
    :documentation  "Variable 2 to store misc info from calling function, etc"
    )
   (sv-interface-subtype
    :initarg :sv-interface-subtype
    :accessor sv-interface-subtype
    :initform NIL
    :type  :symbol
    :documentation  "Subtype of interface :text-input :radio-button :check-button :info")
   (sv-text-input
    :initarg :sv-text-input
    :accessor sv-text-input
    :initform NIL
    :type  :string
    :documentation  "Data from text input")
   (sv-confirm-input-p
    :initarg :sv-confirm-input-p
    :accessor sv-confirm-input-p
    :initform NIL
    :type  :boolean
    :documentation  "Data from confirm-input-p")
   (sv-input-confirmed-p
    :initarg :sv-input-confirmed-p
    :accessor sv-input-confirmed-p
    :initform NIL
    :type  :boolean
    :documentation  "input-confirmed-p")
      ;;NOTE:  4 WAYS TO CLOSE CALLING PROCESS: 1. change it's slot value, 2. Directly close its process; 3. Directly close it's interface; 4. Poke it and let it close itself (can do with change slot-value of 'chose-this-process-p
      (sv-close-make-func-process-p
    :initarg :sv-close-make-func-process-p
    :accessor sv-close-make-func-process-p
    :initform NIL
    :documentation  "If  T, callback closes calling-process at end.")
      (sv-close-interface-p
    :initarg :sv-close-interface-p
    :accessor sv-close-interface-p
    :initform NIL
    :documentation  "If  T, callback closes interface-process at end.")
   (sv-make-func-process
    :initarg :sv-make-func-process
    :accessor sv-make-func-process
    :initform NIL
    :documentation  "MP process from calling function/object.")
#|   (sv-main-interface-process
    :initarg :sv-main-interface-process
    :accessor sv-main-interface-process
    :initform NIL
    :documentation  "Main-interface-process = THIS PROCESS")|#
#|not needed?
   (sv-poke-main-interface-process-p
    :initarg :sv-poke-main-interface-process-p
    :accessor sv-poke-main-interface-process-p
    :type  :boolean
    :initform NIL
    :documentation  "poke-main-interface-process-p ")|#
   (sv-poke-make-func-process-p
    :initarg :sv-poke-make-func-process-p
    :accessor sv-poke-make-func-process-p
    :type  :boolean
    :initform NIL
    :documentation  "poke-make-func-process-p ")
   (sv-destroy-main-interface-p
    :initarg :sv-destroy-main-interface-p
    :accessor sv-destroy-main-interface-p
    :type  :boolean
    :initform NIL
    :documentation  "If  T, callback closes destroy-main-interface-p at end.")
   (sv-poke-interface-process-p
    :initarg :sv-poke-interface-process-p
    :accessor sv-poke-interface-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-interface-process-p ")
   (sv-poke-special-process-p
    :initarg :sv-poke-special-process-p
    :accessor sv-poke-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-special-process-p ")
(sv-close-special-process-p
    :initarg :sv-close-special-process-p
    :accessor sv-close-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-close-special-process-p ")
   (sv-special-process
    :initarg sv-special-process
    :accessor sv-special-process
    :initform NIL
    :documentation  "sv-special-process ")
   )
  
  ;;((the-pane :accessor text-input-pane-test-pane))
  (:panes
   (title-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :make-instance-extra-apply-args :title-pane-args
    :visible-border T
    :internal-border 15 
    ;;end title-pane
    )
   (info-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :make-instance-extra-apply-args :info-pane-args
    :visible-border T
    :internal-border 15
    )
   
   (go-fr-button
    capi:push-button
    :background :green 
    :text  "       GO to next  >>      "
    :font  *go-frame-button-font 
    ;;   :color-requirements 
    ;;   :selected T
    :default-p T  ;;means if return hit, selects this button
    ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
    ;; choose in calling function
    ;;  :callback 'text-input-OR-button-interface-callback ;;'go-select-scales-frame-callback
    :callback-type :item-interface
    :make-instance-extra-apply-args :go-button-args
    )
   (reject-answer-button
    capi:push-button
    :background :green
    :text  "<< Go Back"
    :visible-min-width   130
    :visible-max-width  130
    :visible-min-height  *go-frame-button-height
    :visible-max-height  *go-frame-button-height
    ;;  :external-max-height  *go-frame-button-height   
    #|        :default-x *go-frame-button-x
        :default-y *go-frame-button-y|#
    :font  *go-frame-button-font 
    ;;   :color-requirements 
    ;;   :selected T
    :default-p T  ;;means if return hit, selects this button
    ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
    ;; choose in calling function
    ;;  :callback 'text-input-OR-button-interface-callback ;;'go-select-scales-frame-callback
    :callback-type :item-interface 
    :make-instance-extra-apply-args  :reject-button-args
    )
   ;;end panes
   ) 
  (:layouts
   (column-layout
    capi:column-layout
    '(   title-pane  info-pane   button-layout)
    :make-instance-extra-apply-args  :column-layout-args
    )
    (button-layout
    capi:row-layout     
     '(  reject-answer-button  go-fr-button) 
         :make-instance-extra-apply-args :button-layout
         :adjust :center
     )
   ;;end layouts
   )
  (:default-initargs
   :best-width *text-input-OR-button-interface-best-width
   :best-height *text-input-OR-button-interface-best-height
   :layout 'column-layout
   :background :red
   :internal-border 15
   )
  ;;END 2-BUTTON-POPUP-INTERFACE
  )



;;2-BUTTON-INPUT-INTERFACE-GO-CALLBACK
;;
;;ddd
(defun 2-button-input-interface-GO-callback (item interface)
  "In U-capi-input-interfaces, Sets *2-BUTTON-INPUT-INTERFACE-DATA to the input. NOTE: If want to continue in a process, set the global var to NIL each time used, and proceed in calling process when global var is no longer nil? data not used"
  ;;reset
  (setf *2-BUTTON-INPUT-INTERFACE-DATA NIL)

  (with-slots (go-fr-button) interface
  ;;TAKE ACTIONS SPECIFIED IN INTERFACE SLOTS
  (let
      ((poke-special-process-p (slot-value interface 'sv-poke-special-process-p))
       (special-process (slot-value interface 'sv-special-process))
       (close-make-func-process-p   (slot-value interface 'sv-close-make-func-process-p))
       (make-func-process (slot-value interface 'sv-make-func-process))  
       (yes? T)
       (answer )
       (choice-list-pane)
       ) 
    ;;answer
    (when (equal item go-fr-button)
      (setf answer T))

    ;;CONFIRM THE INPUT?
    (when (slot-value interface 'sv-confirm-input-p)  
      (setf yes? 
            (my-confirm (format nil "ANSWER=  ~A" answer) :QUESTION-STRING "IS YOUR TEXT INPUT CORRECT?"))
      ;;end when
      )
    ;;RE-COPY ANSWER
    (setf answer (capi:choice-selected-item choice-list-pane)
          *2-BUTTON-INPUT-INTERFACE-DATA answer)

    (when (slot-value interface 'sv-poke-make-func-process-p)
      (mp:process-poke (slot-value interface 'sv-make-func-process)))
      
    (when (and poke-special-process-p special-process)
      (mp:process-poke special-process))
    (when (slot-value interface 'sv-poke-interface-process-p)
      (mp:process-poke interface))

    (when yes?  
      (when (slot-value interface 'sv-close-interface-p)
        ;;(break "close now")
        (capi:destroy interface)))

    (when (and close-make-func-process-p make-func-process)
      (mp:process-terminate make-func-process))

    ;;end  let, with-slots
    ))
  ;;end 2-button-input-interface-GO-callback
  )



#|        ;;POKES SHAQ-process-manager running *shaq-main-process to continue (run next frame)
        (capi:destroy interface)
        (when *run-csq-p
          (mp:process-poke *csq-main-process))
        (when *run-shaq-p
          (mp:process-poke *shaq-main-process))   

      ;;end with-slots, let*, text-go-button-callback
      )))|#




;;REJECT-ANSWER-CALLBACK
;;
;;ddd
(defun reject-answer-callback (data interface)
  "In CSQ-SHAQ-Frame-quest-functions, provides signal for main work function to select previous frame"
  (let
      ((poke-special-process-p)
       (special-process)
       )    
    (with-slots (sv-poke-special-process-p sv-special-process) interface
      (setf poke-special-process-p (slot-value interface 'sv-poke-special-process-p)
            special-process (slot-value interface 'special-process-p))
     ;;(break "calling-process in REJECT")
     (setf *2-button-input-interface-data 'NO
           (slot-value interface 'close-make-func-process-p) NIL)

    ;;(setf *test-rich-text-pane-paragraph-format (capi::default-paragraph-format info-pane))
    (when (and poke-special-process-p special-process)
      (mp:process-poke special-process)) ;; *my-current-process)
  ;;for testing
  ;;(setf *test-rich-text-pane-paragraph-format (capi::default-paragraph-format info-pane))
  (capi:destroy interface)
  ;;end reject-answer-callback
  )))


;;MAKE-2-BUTTON-POPUP-INTERFACE-INSTANCE
;;
;;ddd
(defun make-2-button-popup-interface-instance 
       (&key (title "CONFIRM ANSWER" ) 
             (title-bkgr ::yellow)(title-pane-ht 30) 
             (title-font-size 14)(title-font-color :red)
             (title-align :center)  title-pane-args    
             (info-text "Info goes here") 
             (info-pane-ht 60) (info-align :left)
             (info-font-size 12) (info-font-color :black)
             (info-bkgr :light-blue)   info-pane-args
             go-button-args
             reject-button-args
             column-layout-args   
             button-layout-args
             (window-args '(:title "Confirm Your Answer"
                            :visible-min-width 400))
             (go-callback '2-button-input-interface-GO-callback)
             (reject-callback 'reject-answer-callback)
             interface-subtype
             confirm-input-p
             special-process
             poke-special-process-p
             close-special-process-p
             ;;this-process (mp:get-current-process)
             ;;(close-this-process-p T)
             (close-interface-p T)
             poke-interface-process-p 
             make-func-process (mp:get-current-process)
             close-make-func-process-p  
             (poke-make-func-process-p T)
             )                                 
  "In U-capi-input-interfaces,  A general purpose 2-button popup window.  Sets *text-input-OR-button-interface-textdata to the input. NOTE: If want to continue in a process, set the global var to NIL each time used, and proceed in calling process when global var is no longer nil? SETS *2-BUTTON-INPUT-INTERFACE-DATA TO \"YES\" OR \"NO\"  For simplier function USE MY-CONFIRM."
  (let
      ((inst)
       (all-title-pane-args `(:visible-max-height ,title-pane-ht :visible-min-height ,title-pane-ht  :background ,title-bkgr :text ,title   :character-format (:face "times new roman"  :size  ,title-font-size  :color ,title-font-color  :bold T :italic  NIL :underline nil)  :paragraph-format  (:alignment ,title-align)))
       (all-info-pane-args `(:visible-max-height ,info-pane-ht :visible-min-height ,info-pane-ht  :background ,info-bkgr :text ,info-text :character-format (:face "times new roman"  :size  ,info-font-size  :color ,info-font-color  :bold T :italic nil :underline nil)  :paragraph-format  (:alignment ,info-align)))
       (all-args) 
       )
   (setf *2-button-input-interface-data nil)


    ;;COMBINE ARGS
    (when title-pane-args
      (setf all-title-pane-args  (append all-title-pane-args title-pane-args)))
    (setf all-args (append all-args  (list :title-pane-args all-title-pane-args)))

    (when info-pane-args
      (setf all-info-pane-args  (append all-info-pane-args info-pane-args)))
    (setf all-args (append all-args  (list :info-pane-args all-info-pane-args)))

    (when column-layout-args
      (setf all-args (append all-args  (list :column-layout-args column-layout-args))))
    (when button-layout-args
      (setf all-args (append all-args  (list :button-layout-args button-layout-args))))
    (when window-args
      (setf all-args (append all-args  window-args)))   

    (when go-callback
      (setf go-button-args (append go-button-args  `(:callback ,go-callback))))
    (when reject-callback
      (setf reject-button-args (append reject-button-args `(:callback ,reject-callback))))

    (when go-button-args
      (setf all-args (append all-args `( :go-button-args ,go-button-args))))

    (when reject-button-args
      (setf all-args (append all-args  `(:reject-button-args ,reject-button-args))))

    ;;(BREAK "all-args")    

    (setf  inst (apply  'make-instance  '2-button-popup-interface  all-args))

    ;;SET SLOT VALUES
    (when confirm-input-p
      (setf (slot-value inst 'sv-confirm-input-p) T))
    ;;special-process
    (when special-process
      (when poke-special-process-p
        (setf (slot-value inst 'sv-poke-special-process-p) T))
      (when close-special-process-p
        (setf    (slot-value inst 'sv-close-special-process) T)))
    (when close-make-func-process-p
      (setf (slot-value inst 'sv-close-make-func-process-p) T
           (slot-value inst 'sv-make-func-process) make-func-process))
    ;;make-func-process
    (when make-func-process
      (setf  (slot-value inst 'sv-make-func-process) make-func-process)
      (when poke-make-func-process-p
             (slot-value inst 'sv-poke-make-func-process-p) T)
      (when close-make-func-process-p
             (slot-value inst 'sv-close-make-func-process-p) T))
    ;;interface
    (when inst                
      (when poke-interface-process-p
        (setf (slot-value inst 'sv-poke-interface-process-p) T))
      (when close-interface-p
        (setf (slot-value inst 'sv-close-interface-p) T)))
    (when interface-subtype
      (slot-value inst 'sv-interface-subtype) interface-subtype)
 
    (capi:display inst)
    inst
    ;;end let, make-2-button-popup-interface-instance
    ))
;;TEST
;;  (make-2-button-popup-interface-instance :info-text "This is new info" :info-pane-args '(:internal-border 25))  ;;doesn't add any border to rich text pane
;; WORKS 

;; ----------------------- END   make-2-button-popup-interface-instance --------------------------







;;CHOICE-LIST-INTERFACE
;;
;;ddd
(capi:define-interface choice-list-interface ()
  ((sv-var1
    :initarg :sv-var1
    :accessor sv-var1
    :initform NIL
    :type  NIL
    :documentation  "Variable 1 to store misc info from calling function, etc"
    )
   (sv-var2
    :initarg :sv-var2
    :accessor sv-var2
    :initform NIL
    :type  NIL
    :documentation  "Variable 2 to store misc info from calling function, etc"
    )
   (sv-choice-data
    :initarg :sv-choice-data
    :accessor sv-choice-data
    :initform NIL
    :type  :string
    :documentation  "Data from choice list")
  (sv-interface-subtype
    :initarg :sv-interface-subtype
    :accessor sv-interface-subtype
    :initform NIL
    :type  :symbol
    :documentation  "Subtype of interface :text-input :radio-button :check-button :info")
   (sv-text-input
    :initarg :sv-text-input
    :accessor sv-text-input
    :initform NIL
    :type  :string
    :documentation  "Data from text input")
   (sv-confirm-input-p
    :initarg :sv-confirm-input-p
    :accessor sv-confirm-input-p
    :initform NIL
    :type  :boolean
    :documentation  "Data from confirm-input-p")
   (sv-input-confirmed-p
    :initarg :sv-input-confirmed-p
    :accessor sv-input-confirmed-p
    :initform NIL
    :type  :boolean
    :documentation  "input-confirmed-p")
      ;;NOTE:  4 WAYS TO CLOSE CALLING PROCESS: 1. change it's slot value, 2. Directly close its process; 3. Directly close it's interface; 4. Poke it and let it close itself (can do with change slot-value of 'chose-this-process-p
      (sv-close-make-func-process-p
    :initarg :sv-close-make-func-process-p
    :accessor sv-close-make-func-process-p
    :initform NIL
    :documentation  "If  T, callback closes calling-process at end.")
      (sv-close-interface-p
    :initarg :sv-close-interface-p
    :accessor sv-close-interface-p
    :initform NIL
    :documentation  "If  T, callback closes interface-process at end.")
      (sv-make-func-process
    :initarg :sv-make-func-process
    :accessor sv-make-func-process
    :initform NIL
    :documentation  "MP process from calling function/object.")
#|   (sv-main-interface-process
    :initarg :sv-main-interface-process
    :accessor sv-main-interface-process
    :initform NIL
    :documentation  "Main-interface-process = THIS PROCESS")|#
#|not needed?
   (sv-poke-main-interface-process-p
    :initarg :sv-poke-main-interface-process-p
    :accessor sv-poke-main-interface-process-p
    :type  :boolean
    :initform NIL
    :documentation  "poke-main-interface-process-p ")|#
   (sv-poke-make-func-process-p
    :initarg :sv-poke-make-func-process-p
    :accessor sv-poke-make-func-process-p
    :type  :boolean
    :initform NIL
    :documentation  "poke-make-func-process-p ")
   (sv-destroy-main-interface-p
    :initarg :sv-destroy-main-interface-p
    :accessor sv-destroy-main-interface-p
    :type  :boolean
    :initform NIL
    :documentation  "If  T, callback closes destroy-main-interface-p at end.")
   (sv-poke-special-process-p
    :initarg :sv-poke-special-process-p
    :accessor sv-poke-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-special-process-p ")
   (sv-close-special-process-p
    :initarg :sv-close-special-process-p
    :accessor sv-close-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-close-special-process-p ")
   (sv-special-process
    :initarg sv-special-process
    :accessor sv-special-process
    :initform NIL
    :documentation  "sv-special-process ")
   (sv-poke-interface-process-p
    :initarg :sv-poke-interface-process-p
    :accessor sv-poke-interface-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-interface-process-p ")
   )
  ;;PANES
  (:panes
   (title-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :make-instance-extra-apply-args :title-pane-args
    :visible-border T
    :internal-border 20 
    ;;end title-pane
    )
   (info-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :make-instance-extra-apply-args :info-pane-args
    :visible-border T
    :internal-border 20
    )
   (choice-list-pane
    capi:list-panel   
    ;; :selection 0
    :callback-type :item-interface
    :make-instance-extra-apply-args :choice-list-args
    )
   (go-fr-button
    capi:push-button
    :background :green
    :text  "       GO to next  >>      "
    :font  *go-frame-button-font 
    ;;   :color-requirements 
    ;;   :selected T
    :default-p T  ;;means if return hit, selects this button
    ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
    ;; choose in calling function
    ;;  :callback 'text-input-OR-button-interface-callback ;;'go-select-scales-frame-callback
    :make-instance-extra-apply-args :go-button-args
    :callback-type :item-interface
    )

   ;;end panes
   )   
  (:layouts
   (column-layout
    capi:column-layout
    '()
    ;;  '(   title-pane  info-pane  choice-list-pane  go-fr-button  )
    :make-instance-extra-apply-args :layout-args
    )
   ;;end layouts
   )
  (:default-initargs
   :best-width *text-input-OR-button-interface-best-width
   :best-height *text-input-OR-button-interface-best-height
   :layout 'column-layout
   )
  ;;END choice-list-interface
  )



;;MAKE-CHOICE-LIST-INTERFACE-INSTANCE
;;
;;ddd
(defun make-choice-list-interface-instance (&key (title "INFORMATION" )     
                                                (sel-items '(1)) ;;this isn't working right
                                                (title-bkgr :yellow)(title-pane-ht 30) 
                                                (title-font-size 14)(title-font-color :red)
                                                (title-align :center)  title-pane-args    
                                                (info-text "Info goes here")                                          
                                                (info-pane-ht 60) (info-align :left)
                                                (info-font-size 12) (info-font-color :black)
                                                (info-bkgr :light-blue)   info-pane-args
                                                (choice-items  (list  "NO" "YES"))
                                                (choice-type :single-selection)
                                                ;;doesn't work sel-items
                                                choice-list-args
                                                layout-args   
                                                (window-args '(:title "INFORMATION WINDOW"
                                                  :visible-min-width 400 :visible-min-height 340
                                                  :internal-border 10
                                                  :background #(:RGB 0.3882353 0.94509805 0.6392157 1.0)))
                                                (go-fr-button-p T)
                                                (close-interface-p T)
                                                (border-args (list :width 10 :foreground :red))
                                                confirm-input-p
                                                (callback 'choice-list-interface-callback)
                                                menu-bar-items
                                                go-button-args
                                                interface-subtype
                                                poke-special-process-p
                                                special-process
                                                 close-make-func-process-p 
                                                make-func-process 
                                                          (close-this-process-p T)                                                                                             (poke-make-func-process-p T)
                                                          poke-interface-process-p 
                                                )                                 
  "In U-capi-input-interfaces,  A general purpose text input popup window.  Sets *CHOICE-LIST-INTERFACE-DATA to the input. NOTE: If want to continue in a process, set the global var to NIL each time used, and proceed in calling process when global var is no longer nil? data not used. To NOT CLOSE ON GO-TO, set close-make-func-process-p = NIL. SEL-ITEMS Eg '( 1 3 6) begins w/0 . MENU-BAR-ITEMS eg (list menu1 menu2); the menus are menu-instances created OUTSIDE this function."
  (let
      ((inst)
       (all-title-pane-args `(:visible-max-height ,title-pane-ht :visible-min-height ,title-pane-ht  :background ,title-bkgr :text ,title   :character-format (:face "times new roman"  :size  ,title-font-size  :color ,title-font-color  :bold T :italic  NIL :underline nil)  :paragraph-format  (:alignment ,title-align)))
       (all-info-pane-args `(:visible-max-height ,info-pane-ht :visible-min-height ,info-pane-ht  :background ,info-bkgr :text ,info-text :character-format (:face "times new roman"  :size  ,info-font-size  :color ,info-font-color  :bold T :italic nil :underline nil)  :paragraph-format  (:alignment ,info-align)))  
       (all-args) 
       (this-process (mp:get-current-process))
       )
    (setf *choice-list-interface-datalist nil)

    ;;choice-type (affects data)
    (cond
     ((equal choice-type :multiple-selection)
      (setf  choice-list-args (append choice-list-args (list :interaction :multiple-selection))))
     ((equal choice-type :extended-selection)
      (setf choice-list-args (append choice-list-args (list :interaction :extended-selection)))))

    (cond
     (go-fr-button-p
      (setf go-button-args (append go-button-args `(:callback ,callback))))
      ;;   layout-args '(:description  (title-pane  info-pane  choice-list-pane  go-fr-button ))))
     (t 
      (setf choice-list-args (append choice-list-args `(:selection-callback ,callback))))) 
      ;;      layout-args '(:description '( title-pane  info-pane  choice-list-pane )))))

    (setf choice-list-args (append choice-list-args (list :items choice-items)))
             ;;was  `(:items ,choice-items))) didn't work for menu-components

    (when  sel-items 
      (cond
       ((equal choice-type :single)
        (setf choice-list-args (append choice-list-args  `(:selected-item ,sel-items))))
       ((equal choice-type :multiple)
        (setf choice-list-args (append choice-list-args  `(:selected-items ,sel-items))))))       

    ;;COMBINE ARGS
    (when choice-list-args
      (setf all-args (append all-args (list :choice-list-args choice-list-args))))

    (when go-button-args
      (setf all-args (append all-args (list :go-button-args go-button-args))))    

    (when title-pane-args
      (setf all-title-pane-args  (append all-title-pane-args title-pane-args)))
    (setf all-args (append all-args  (list :title-pane-args all-title-pane-args)))

    (when info-pane-args
      (setf all-info-pane-args  (append all-info-pane-args info-pane-args)))
    (setf all-args (append all-args  (list :info-pane-args all-info-pane-args)))

    (when layout-args
      (setf all-args (append all-args  (list :layout-args layout-args))))
    (when window-args
      (setf all-args (append all-args  window-args)))   
    ;;MENUS
   (when menu-bar-items
      (setf all-args (append all-args  (list :menu-bar-items menu-bar-items))))
    ;;(BREAK "all-args")   
    ;;MAKE THE CAPI INSTANCE
    (setf  inst (apply  'make-instance  'choice-list-interface  all-args))
    
    (with-slots (column-layout title-pane  info-pane  choice-list-pane  go-fr-button ) inst
      (cond
       (go-fr-button-p
        (setf (capi:layout-description column-layout) (list title-pane  info-pane  choice-list-pane  go-fr-button )))
       (t 
        (setf (capi:layout-description column-layout) (list title-pane  info-pane  choice-list-pane))))

    ;;SET SLOT VALUES
    (setf (slot-value inst 'sv-interface-subtype) choice-type)
    (when confirm-input-p
      (setf (slot-value inst 'sv-confirm-input-p) T))
    (when (and poke-special-process-p special-process)
      (setf (slot-value inst 'sv-poke-special-process-p) poke-special-process-p
            (slot-value inst 'sv-special-process) special-process))
        (when close-interface-p
      (setf (slot-value inst 'sv-close-interface-p) T))
    (when close-make-func-process-p
      (setf (slot-value inst 'sv-close-make-func-process-p) T
            (slot-value inst 'sv-make-func-process) make-func-process))
    (when poke-interface-process-p
      (setf (slot-value inst 'sv-poke-interface-process-p) T))
    (when poke-make-func-process-p
      (setf  (slot-value inst 'sv-make-func-process) this-process
             (slot-value inst 'sv-poke-make-func-process-p) T))
    (when interface-subtype
      (slot-value inst 'sv-interface-subtype) interface-subtype)
    ;;menu items
#|    (when menu-bar-items
      (setf (slot-value inst 'menu-bar-items) menu-bar-items))|#
      ;;end with-slots
      ) 
    ;;(break "before display")
    ;;DISPLAY, RETURN
    (capi:display inst)

    ;;SET INITIAL SELECTED ITEM(S): LIST 0F SEL ITEMS (EG. 0 1 3 6)
#|  not working  (with-slots (choice-list-pane) inst
      (when sel-items
        (capi:apply-in-pane-process choice-list-pane
                       #'(setf capi:choice-selected-items) sel-items choice-list-pane))
      (setf *test-sels (capi:choice-selected-items choice-list-pane))
      ;;end second with-slots
      )|#
    inst
    #|   (with-slots (title-pane) inst
       (capi:apply-in-pane-process title-pane 
                 #'(setf capi:rich-text-pane-text) title  title-pane )
       )|#
    ;;end let, make-choice-list-interface-instance
    ))
;;TEST
;; (make-choice-list-interface-instance :choice-items '(1 2 3 4 5 6) :close-interface-p T)
;; works *choice-list-interface-data = 4
;; doesn't work --so elim sel-items 
;; (make-choice-list-interface-instance :choice-items '(aa bb cc dd ee ff) :close-interface-p T :sel-items '( 1 2 5) :choice-type :multiple-selection)

;; TO GO WHEN CLICK ON CHOICE ITEM USE:
;; (make-choice-list-interface-instance :choice-items '(1 2 3 4 5 6) :go-fr-button-p NIL)
;; TO CONFIRM ANSWER
;; (make-choice-list-interface-instance :choice-items '(1 2 3 4 5 6) :CONFIRM-INPUT-P T)

;;multiple-selection, etc
;; (make-choice-list-interface-instance :title "New Title" :title-bkgr :green :info-align :center :info-font-color :blue :choice-items (list "A" "Boy" "C" 1 2 3 4) :choice-type :MULTIPLE-SELECTION :confirm-input-p T :go-button-args (list :visible-min-width 300) )
;; works > *CHOICE-LIST-INTERFACE-DATA =   ("Boy" 2)
;;
;;change window args too
;; (make-choice-list-interface-instance :title "New Title" :title-bkgr :green :info-align :center :info-font-color :blue :choice-items (list "A" "Boy" "C" 1 2 3 4) :choice-type :MULTIPLE-SELECTION :confirm-input-p T :go-button-args (list :visible-min-width 200)   :window-args (quote (:title "NEW INTERFACE TITLE" :visible-min-width 600 :visible-min-height 340 :internal-border 15 :background :red)))
;; WORKS = *CHOICE-LIST-INTERFACE-DATA =   ("C" 2 4)





;;CHOICE-LIST-INTERFACE-CALLBACK
;;
;;ddd
(defun choice-list-interface-callback (item interface)
  "In U-capi-input-interfaces, Sets *CHOICE-LIST-INTERFACE-DATA to the input. NOTE: If want to continue in a process, set the global var to NIL each time used, and proceed in calling process when global var is no longer nil? data not used"

  (with-slots (choice-list-pane) interface
  ;;TAKE ACTIONS SPECIFIED IN INTERFACE SLOTS
  (let
      ((poke-special-process-p (slot-value interface 'sv-poke-special-process-p))
       (special-process (slot-value interface 'sv-special-process))
           (close-make-func-process-p   (slot-value interface 'sv-close-make-func-process-p))
           (make-func-process (slot-value interface 'sv-make-func-process))  
       (yes? T)
       (answer) 
       (selection-type (slot-value interface 'sv-interface-subtype))
       (close-interface-p (slot-value interface 'sv-close-interface-p))
       (confirm-answer-p (slot-value interface 'sv-confirm-input-p))
       )     
    ;;reset
    (setf *choice-list-interface-data NIL)

    (cond
     ((equal selection-type :single-selection)
      (setf answer (capi:choice-selected-item choice-list-pane)))
     (t (setf answer (capi:choice-selected-items choice-list-pane))))

    ;;CONFIRM THE INPUT?
    (when confirm-answer-p  
      (setf yes? 
            (my-confirm (format nil "ANSWER=  ~A" answer)
               :QUESTION-STRING "IS YOUR TEXT INPUT CORRECT?"))
      ;;end when
      )
    ;;RE-COPY ANSWER
    (cond
     ((equal selection-type :single-selection)
      (setf answer (capi:choice-selected-item choice-list-pane)))
     (t (setf answer (capi:choice-selected-items choice-list-pane))))

    (setf *CHOICE-LIST-INTERFACE-DATA answer)
    ;;(break "sv-poke-make-func-process-p")
    (when (slot-value interface 'sv-poke-make-func-process-p)
      (mp:process-poke (slot-value interface 'sv-make-func-process)))
      
    (when (and poke-special-process-p special-process)
      (mp:process-poke special-process))
    (when (slot-value interface 'sv-poke-interface-process-p)
      (mp:process-poke interface))

    ;;CLOSE THIS INTERFACE?
    (cond
     (yes?  
      (when close-interface-p
        ;;(break "close now")
        (capi:destroy interface)))
     ((and close-interface-p  (null confirm-answer-p)  )
      (capi:destroy interface)))

    (when close-make-func-process-p
      (mp:process-terminate make-func-process))

    *CHOICE-LIST-INTERFACE-DATA
     ;;no  *text-input-OR-button-interface-textdata
      ;;end let, with, choice-list-interface-callback
      )))
;;TEST
;;  WORKS  *choice-list-interface-data = "item 3"





;;CHOICE-RADIO-BUTTON-INTERFACE
;;
;;ddd
(capi:define-interface choice-radio-button-interface ()
  ((sv-var1
    :initarg :sv-var1
    :accessor sv-var1
    :initform NIL
    :type  NIL
    :documentation  "Variable 1 to store misc info from calling function, etc"
    )
   (sv-var2
    :initarg :sv-var2
    :accessor sv-var2
    :initform NIL
    :type  NIL
    :documentation  "Variable 2 to store misc info from calling function, etc"
    )(sv-choice-data
    :initarg :sv-choice-data
    :accessor sv-choice-data
    :initform NIL
    :type  :string
    :documentation  "Data from choice list")
   (sv-interface-subtype
    :initarg :sv-interface-subtype
    :accessor sv-interface-subtype
    :initform NIL
    :type  :symbol
    :documentation  "Subtype of interface :text-input :radio-button :check-button :info"   )
   (sv-selection-type
    :initarg :sv-selection-type
    :accessor sv-selection-type
    :initform NIL
    :type  :symbol
    :documentation  ":single-selection, :multiple-selection, or :extended-selection"    )
   (sv-text-input
    :initarg :sv-text-input
    :accessor sv-text-input
    :initform NIL
    :type  :string
    :documentation  "Data from text input")
   (sv-confirm-input-p
    :initarg :sv-confirm-input-p
    :accessor sv-confirm-input-p
    :initform NIL
    :type  :boolean
    :documentation  "Data from confirm-input-p")
   (sv-input-confirmed-p
    :initarg :sv-input-confirmed-p
    :accessor sv-input-confirmed-p
    :initform NIL
    :type  :boolean
    :documentation  "input-confirmed-p")
   ;;NOTE:  4 WAYS TO CLOSE CALLING PROCESS: 1. change it's slot value, 2. Directly close its process; 3. Directly close it's interface; 4. Poke it and let it close itself (can do with change slot-value of 'chose-this-process-p
   (sv-close-make-func-process-p
    :initarg :sv-close-make-func-process-p
    :accessor sv-close-make-func-process-p
    :initform NIL
    :documentation  "If  T, callback closes calling-process at end.")
   (sv-close-interface-p
    :initarg :sv-close-interface-p
    :accessor sv-close-interface-p
    :initform NIL
    :documentation  "If  T, callback closes interface-process at end.")
   (sv-make-func-process
    :initarg :sv-make-func-process
    :accessor sv-make-func-process
    :initform NIL
    :documentation  "MP process from calling function/object.")
#|   (sv-main-interface-process
    :initarg :sv-main-interface-process
    :accessor sv-main-interface-process
    :initform NIL
    :documentation  "Main-interface-process = THIS PROCESS")|#
   #|not needed?
   (sv-poke-main-interface-process-p
    :initarg :sv-poke-main-interface-process-p
    :accessor sv-poke-main-interface-process-p
    :type  :boolean
    :initform NIL
    :documentation  "poke-main-interface-process-p ")|#
   (sv-poke-make-func-process-p
    :initarg :sv-poke-make-func-process-p
    :accessor sv-poke-make-func-process-p
    :type  :boolean
    :initform NIL
    :documentation  "poke-make-func-process-p ")
   (sv-destroy-main-interface-p
    :initarg :sv-destroy-main-interface-p
    :accessor sv-destroy-main-interface-p
    :type  :boolean
    :initform NIL
    :documentation  "If  T, callback closes destroy-main-interface-p at end.")
   (sv-poke-special-process-p
    :initarg :sv-poke-special-process-p
    :accessor sv-poke-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-special-process-p ")
(sv-close-special-process-p
    :initarg :sv-close-special-process-p
    :accessor sv-close-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-close-special-process-p ")
   (sv-special-process
    :initarg sv-special-process
    :accessor sv-special-process
    :initform NIL
    :documentation  "sv-special-process ")
   (sv-poke-interface-process-p
    :initarg :sv-poke-interface-process-p
    :accessor sv-poke-interface-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-interface-process-p ")
   )
  ;;PANES
  (:panes
   (title-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :make-instance-extra-apply-args :title-pane-args
    :visible-border T
    :internal-border 20 
    ;;end title-pane
    )
   (info-pane
    capi:rich-text-pane    :accepts-focus-p NIL
    :make-instance-extra-apply-args :info-pane-args
    :visible-border T
    :internal-border 20
    )
   (radio-button-pane
    capi:radio-button-panel 
    :selection nil
    ;;:callback-type :data-interface
    :make-instance-extra-apply-args :choice-radio-button-args
    )
   (go-fr-button
    capi:push-button
    :background :green
    :text  "       GO to next  >>      "
    :font  *go-frame-button-font 
    ;;   :color-requirements 
    ;;   :selected T
    :default-p T  ;;means if return hit, selects this button
    ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
    ;; choose in calling function
    ;;  :callback 'text-input-OR-button-interface-callback ;;'go-select-scales-frame-callback
    :make-instance-extra-apply-args :go-button-args
    :callback-type :data-interface
    )

   ;;end panes
   )   
  ;;LAYOUTS
  (:layouts
   (column-layout
    capi:column-layout
    '()
    ;;  '(   title-pane  info-pane  radio-button-pane  go-fr-button  )
    :make-instance-extra-apply-args :layout-args
    )
   ;;end layouts
   )
  (:default-initargs
   :best-width *text-input-OR-button-interface-best-width
   :best-height *text-input-OR-button-interface-best-height
   :layout 'column-layout
   )
  ;;END choice-radio-button-interface
  )


;;MAKE-RADIO-BUTTON-CHOICE-INSTANCE
;;
;;ddd
(defun make-radio-button-choice-instance (&key (title "INFORMATION" )                    
                                               (title-bkgr ::yellow)(title-pane-ht 30) 
                                               (title-font-size 14)(title-font-color :red)
                                               (title-align :center)  title-pane-args    
                                               (info-text "Info goes here") 
                                               (info-pane-ht 60) (info-align :left)
                                               (info-font-size 12) (info-font-color :black)
                                               (info-bkgr :light-blue)   info-pane-args
                                               (choice-items  (list  "NO" "YES"))
                                               (choice-type :single-selection)
                                               choice-radio-button-args
                                               layout-args   
                                               (window-args '(:title "INFORMATION WINDOW"
                                                              :visible-min-width 400 :visible-min-height 340
                                                              :internal-border 10
                                                              :background #(:RGB 0.3882353 0.94509805 0.6392157 1.0)))
                                               (choice-items '("Item 1" "Item 2" "Item 3"))
                                               (choice-panel-title "Select one, then hit GO button")
                                               (choice-panel-layout 'capi:column-layout)
                                               (choice-panel-interaction :single-selection)
                                               (go-fr-button-p T)
                                               confirm-input-p
                                               (callback 'text-input-OR-button-interface-callback)
                                                ;;was -- not exist'choice-radio-button-interface-callback)
                                               (callback-type :item-interface)
                                               go-button-args
                                               interface-subtype
                                               (close-this-process-p T)
                                               close-interface-p 
                                               close-make-func-process-p   
                                               make-func-process 
                                               (poke-make-func-process-p T)
                                               poke-special-process-p
                                               special-process
                                               poke-interface-process-p 
                                               )                                 
  "In U-capi-input-interfaces,  Single or multiple selection.  A very flexible, general purpose text input popup window.  Sets *text-input-OR-button-interface-textdata to the input. NOTE: If want to continue in a process, set the global var to NIL each time used, and proceed in calling process when global var is no longer nil? data not used "
  (let
      ((inst)
       (all-title-pane-args `(:visible-max-height ,title-pane-ht
                              :visible-min-height ,title-pane-ht  :background ,title-bkgr
                              :text ,title   
                              :character-format (:face "times new roman":size  ,title-font-size 
                                                 :color ,title-font-color  :bold T :italic  NIL :underline nil) 
                              :paragraph-format  (:alignment ,title-align)))
       (all-info-pane-args `(:visible-max-height ,info-pane-ht
                             :visible-min-height ,info-pane-ht  :background ,info-bkgr
                             :text ,info-text :character-format (:face "times new roman"
                                                                 :size  ,info-font-size  :color ,info-font-color
                                                                 :bold T :italic nil :underline nil)
                             :paragraph-format  (:alignment ,info-align)))
       (all-args) 
       (this-process (mp:get-current-process))
       )
    ;;DATALIST
    (setf *text-input-OR-button-interface-textdata nil)
    
    (cond
     ((equal choice-type :single-selection)
      (setf choice-radio-button-args (append choice-radio-button-args (list :interaction :single-selection))))
     ((equal choice-type :multiple-selection)
      (setf choice-radio-button-args (append choice-radio-button-args (list :interaction :multiple-selection))))
     ((equal choice-type :extended-selection)
      (setf choice-radio-button-args (append choice-radio-button-args (list :interaction :extended-selection)))))

    (cond
     (go-fr-button-p
      (setf go-button-args (append go-button-args `(:callback ,callback
                                                    :callback-type ,callback-type))))
      ;;   layout-args '(:description  (title-pane  info-pane  radio-button-pane  go-fr-button ))))
     (t 
      (setf choice-radio-button-args (append choice-radio-button-args `(:selection-callback ,callback))))) 
      ;;      layout-args '(:description '( title-pane  info-pane  radio-button-pane )))))

    (setf choice-radio-button-args (append choice-radio-button-args 
                                           `(:items ,choice-items :title ,choice-panel-title
                                             :layout-class ,choice-panel-layout))
           all-args (append all-args (list :choice-radio-button-args choice-radio-button-args)))

    ;;COMBINE ARGS
    (when go-button-args
      (setf all-args (append all-args (list :go-button-args go-button-args))))    

    (when title-pane-args
      (setf all-title-pane-args  (append all-title-pane-args title-pane-args)))
    (setf all-args (append all-args  (list :title-pane-args all-title-pane-args)))

    (when info-pane-args
      (setf all-info-pane-args  (append all-info-pane-args info-pane-args)))
    (setf all-args (append all-args  (list :info-pane-args all-info-pane-args)))

    (when layout-args
      (setf all-args (append all-args  (list :layout-args layout-args))))
    (when window-args
      (setf all-args (append all-args  window-args)))   

    ;;(BREAK "all-args")    

    (setf  inst (apply  'make-instance  'choice-radio-button-interface  all-args))            

    (with-slots (column-layout title-pane  info-pane  radio-button-pane  go-fr-button ) inst
      (cond
       (go-fr-button-p
        (setf (capi:layout-description column-layout) 
              (list title-pane  info-pane  radio-button-pane  go-fr-button )))
       (t 
        (setf (capi:layout-description column-layout) 
              (list title-pane  info-pane  radio-button-pane))))

    ;;SET SLOT VALUES
    (setf  (slot-value inst 'sv-interface-subtype) :radio-button
           (slot-value inst 'sv-selection-type) choice-type)
    
    (when confirm-input-p
      (setf (slot-value inst 'sv-confirm-input-p) T
            go-fr-button-p T))
    (when (and poke-special-process-p special-process)
      (setf (slot-value inst 'sv-poke-special-process-p) T
            (slot-value inst 'sv-special-process) special-process))
    (when close-interface-p
      (setf (slot-value inst 'sv-close-interface-p) T))
    (when close-make-func-process-p
      (setf (slot-value inst 'sv-close-make-func-process-p) T
            (slot-value inst 'sv-make-func-process) make-func-process)) 
#|    (when poke-interface-process-p
      (setf (slot-value inst 'sv-poke-interface-process-p) T))|#
    (setf  (slot-value inst 'sv-make-func-process) this-process
           (slot-value inst 'sv-poke-make-func-process-p) T)
    (when interface-subtype
      (slot-value inst 'sv-interface-subtype) interface-subtype)
      ;;end with-slots
      ) 
    (capi:display inst)
    inst
    ;;end let, make-radio-button-choice-instance
    ))
;;TEST
;; (make-radio-button-choice-instance :choice-type :multiple-selection :choice-items '("This is a long choice item 1" "This is a long choice item 2" "This is a long choice item 3" "This is a long choice item 4"))
;; WORKS
;; CL-USER 36 > *CHOICE-RADIO-BUTTON-INTERFACE-DATALIST = ("This is a long choice item 1" "This is a long choice item 3")








;;XXX HHH =================== HELP SECTION
#|(capi:contain (make-instance
               'capi:radio-button-panel
               :title "Select a color:"
               :items '(:red :green :blue)
               :print-function 'string-capitalize))
(setq buttons (capi:contain
               (make-instance
                'capi:radio-button-panel
                :title "Select a color vertical:"
                :items '(:red :green :blue)
                :print-function 'string-capitalize
                :layout-class 'capi:column-layout)))
(capi:choice-selected-item buttons)|#
;;There is a further example here:
;;(example-edit-file "capi/buttons/buttons")
;;


;;MAKE-MULTI-INPUT-FRAMES [Actually one input and multi-answers]
;;2018-08
;;ddd 
(defun make-multi-input-frames (q-params &key func (default-func 'input-or-buttons)
                                    title (default-title "INPUT WINDOW" ) (max-inputs 40)
                                                          (title-bkgr ::yellow)(title-pane-ht 30) 
                                                          (title-font-size 14)(title-font-color :red)
                                                          (title-align :center)  title-pane-args    
                                                         ;; (instr-text "Type INPUT below:") 
                                                          (instr-pane-ht 40) (instr-align :left)
                                                          (instr-font-size 12) (instr-font-color :black)
                                                          (instr-bkgr :light-blue)   instr-pane-args
                                                          (instr-text "Type best answer in window below:")
                                                          (input-pane-text "")  input-pane-args 
                                                          ;;quest-pane
                                                          (quest-pane-ht 40) (quest-border 15)
                                                          (quest-font-size 12) (quest-font-color :black)
                                                          (quest-bkgr :light-blue)   quest-pane-args
                                                          quest-text ;;creates pane if present
                                                          (input-pane-text "") 
                                                          (interface-subtype :text-input)
                                                          ;;types= :text-input :radio-button :check-button :info
#|                                                          radio-button-items
                                                          radio-button-pane-args
                                                          check-button-items
                                                          check-button-pane-args|#
                                                          go-button-args
                                                          INCL-NA-NONE-BUTTON-P
                                                          layout-args  
                                                          (win-title "TYPE ONLY ONE INPUT BELOW:")
                                                          (win-border 10)
                                                          (win-background #(:RGB 0.5019608 1.0 0.0 1.0) )
                                                          win-args
                                                          ;;was :light-blue)
                                                          confirm-input-p 
                                                          (callback 'text-input-OR-button-interface-callback)
                                                          (callback-type :item-interface)
                                                          close-interface-p 
                                                          make-func-process 
                                                          (poke-make-func-process-p T)
                                                          poke-special-process-p
                                                          special-process
                                                          poke-interface-process-p 
                                                          (pause-time 100)
                                                          )
  "U-capi-input-interfaces. INPUT: When Q-PARAMS= (Q-TEXTLIST TITLE1 FUNC1 Q-INSTRLIST REST1)      From ONE INPUT creates multi-SAME-popups for MULTI-ANSWERS.    RETURNS output-list INPUT: q-params= (question title func q-instr ... ) q-instrs can vary with func. Types of functions: input-or-buttons, rest args= title/nil question);  q-instr= question if use default-func and default-title. FUNCS are either input-or-buttons, ....add to list...  POPS UP A QUESTION AND RETURNS  all-outputs and *temp-all-multi-input-outputs???? "
  ;;RESET THIS
  (setf *terminate-current-loop-p NIL
        *temp-all-multi-input-outputs NIL)
  (let
      ((rest)
       (answers)
       (all-outputs)
       (this-func-process (mp:get-current-process))
       )
    (unless func
      (setf func default-func))
    (unless title
      (setf title default-title))
    (unless make-func-process
      (setf make-func-process this-func-process))
    

    ;;FIND FUNC, TEXTS, REST
    (cond
     ((listp q-params)
      ;;note: these OVERIDE THE DEFAULT VALUES
      (multiple-value-bind (q-textlist title1 func1 q-instrlist rest1)
          (values-list q-params)
        (when func1
          (setf func func1))
        (when q-textlist
          (setf quest-text (car (process-text-list q-textlist))))
        (when q-instrlist
          (setf instr-text (car (process-text-list q-instrlist))))
        (when title1
          (setf title title1))
        (when rest1
          (setf rest rest1))
        ;;(break "q-params")
        ;;end mvb, listp
        ))                                       
     (t
      (setf func default-func)))  

    ;;LOOP FOR EACH ANSWER (NOT QUESTION)
    (loop
     for n from 1 to max-inputs
     do
       (cond
        ((equal func 'input-or-buttons)
         ;;(break "make-func-process before call")
         ;;eval the function
         (make-text-input-or-button-interface-instance :instr-text instr-text  :title title
                                                       :win-title win-title
                                                       :quest-text quest-text ;;creates text-pane if not nil
                                                       :close-interface-p close-interface-p 
                                                       :terminate-button-p T
                                                       :confirm-input-p confirm-input-p
                                                       :win-border win-border
                                                       :win-background win-background
                                                       :win-args win-args
                                                       :quest-pane-ht 40 :quest-border quest-border
                                                       :quest-font-size 12 :quest-font-color :black
                                                       :quest-bkgr :light-blue
                                                       :quest-pane-args quest-pane-args
                                                       ;;otherwise puts in 3600 mp pause mode
                                                       :close-this-process-p  T   
                                                       :close-interface-p close-interface-p 
                                                       :close-make-func-process-p NIL                                   
                                                       :make-func-process make-func-process
                                                       :poke-make-func-process-p T
                                                       :INCL-NA-NONE-BUTTON-P 
                                                       INCL-NA-NONE-BUTTON-P
                                                       :callback callback
                                                       :callback-type callback-type
                                                       :special-process special-process
                                                       :poke-special-process-p poke-special-process-p
                                                       :poke-interface-process-p poke-interface-process-p)
         ;;(break "after mvs")
         ;;comment off next 12-31-2019
         ;;(format t "cur-proc= ~A" (mp:get-current-process))
         (mp:current-process-pause pause-time)
         ;;(format t "After pause, cur-proc= ~A" (mp:get-current-process))
         (setf all-outputs 
               (append all-outputs (list *text-input-OR-button-interface-textdata))
               *temp-all-multi-input-outputs all-outputs)
         ;;end func
         )
        ;;SSS ADD OTHER QUESTION FUNCTIONS HERE
        (t nil))

       ;;(break "*terminate-current-loop-p")
       ;;TERMINATE LOOP? called with terminate-interface-callback
       (when *terminate-current-loop-p
         (return))
       ;;end loop
       )
    (values all-outputs rest)
    ;;end let, make-multi-input-frames
    ))
;;TEST
;;  (make-multi-input-frames "TEST QUESTION-INSTRS" )
;; works = ("aa" "bb" "cc" "dd" "last item")
;; ;;  (make-multi-input-frames "TEST QUESTION-INSTRS" :incl-na-none-button-p T)
;; (make-multi-input-frames '( ("Q-TEXT" "2") "TITLE" nil ("Q-INSTR" 3) "REST") :incl-na-none-button-p T)


;;TERMINATE-INTERFACE-CALLBACK
;;2018
;;ddd
(defun terminate-interface-callback (item interface)
  "U-capi-input-interfaces. Terminates calling process"
  (setf *terminate-current-loop-p T)
  (text-input-OR-button-interface-callback item interface)
  )



;;MAKE-SIMPLE-MENU-INSTANCE
;;2019
;;ddd
(defun make-simple-menu-instance (title items callback  ;;was first menu-sym
                                    &key (callback-type :item-interface) 
                                    (interaction :single-selection)
                                     other-init-args )
  "U-capi-input-interfaces,  RETURNS: menu-instance
    INPUT: callback types/list  '( :data :element :interface :collection :item) use :data-interface for multi? For COMPONENT-MENUs, or submenus. 
   1. Make component-menu instance and 2. add to ITEMS-LIST= (list \"item1\" comp-menu)"
  (let
      ((make-inst-args  `(:title ,title ;;"Foo"
                     :items ,items ;; '("One" "Two" "Three" "Four")
                     :callback ',callback  ;;'test-callback
                     :callback-type ,callback-type)) 
      )
    (when other-init-args
      (setf make-inst-args (append make-inst-args other-init-args)))
    ;;MAKE MENU INSTANCE
    ;;no--just return the menu instance? (set menu-sym 
    (apply 'make-instance 'capi:menu make-inst-args)
    ;;end let, make-simple-menu-instance
    ))
;;TEST
;;Following works for ordinary menu:
;;STEP 1:MENU  (setf test-menuXX (MAKE-SIMPLE-MENU-INSTANCE   "test-menuXX" '("Item 1" "Item 2")  'test-callback))
;; works= #<CAPI:MENU "test-menuXX" [2 items] 23BB3C07>
;;STEP 2: INTERFACE INST (setq test-interfaceX1  (make-instance 'capi:interface :title "test-interfaceX1"   :menu-bar-items (list test-menuXX)   :best-height 100 :best-width 200))
;;STEP 3: DISPLAY INST  (capi:display test-interfaceX1)
;; works, shows menu as above in interface
;; COMPONENT-MENU TEST
;; STEP 1: MAKE COMPONENT-MENU INSTANCE
;;  (setf comp-menuX (make-instance 'capi:menu-component :title "Menu Component" :items (list "COMPitem1" "COMPitem2") :interaction :multiple-selection :selection 1 :callback 'test-callback))
;;STEP 2:MENU  (setf test-menuCOMPX (MAKE-SIMPLE-MENU-INSTANCE   "test-menuCOMPX" (list "Item 1" comp-menuX "Item 2") 'test-callback   :interaction :multiple-selection))
;; works= 
;;STEP 2: INTERFACE INST  (setq test-interfaceX2  (make-instance 'capi:interface :title "test-interfaceX2"   :menu-bar-items (list test-menuCOMPX)   :best-height 100 :best-width 200)) 
;;STEP 3: DISPLAY INST  (capi:display test-interfaceX2)
;; This works, displays menu with component menu in middle.




;;MAKE-FILE-MENU1
;;2019
;;ddd
(defun make-file-menu1 ( &key (main-menu-title "File")
                              (main-menu-text (format nil "Current FROM Folder= ~A"
                                                      *saved-dir-X))
                              main-menu-callback
                              (sel-dir-title "Select Copy FROM Folder")
                              (sel-dir-text "Folder")      
                              (sel-dir-default-folder *saved-dir-X) ;; "C:\\dropbox-ADD\\")
                              (sel-dir-items  (list "Choose Directory" sel-dir-default-folder))
                              (sel-dir-callback 'select-dir1-callback)
                              (save-settings-title "SAVE ALL SETTINGS")
                              (save-settings-items (list "Save to c:\\temp\\"))
                              (save-settings-callback 'save-generic-settings-callback)                                                         )
  "In U-capi-input-interfaces "
  (let*
      ((sel-dir-menu
        (make-instance 'capi:menu
                            :title sel-dir-title ;;"File"
                            :items sel-dir-items
                           :selection-callback sel-dir-callback
                           :callback-type :data-interface))
         (save-settings-menu
          (make-instance 'capi:menu
                         :title save-settings-title
                         :items save-settings-items
                :callback save-settings-callback
          :callback-type :interface 
          ))
         (main-menu
          (make-instance 'capi:menu
                         :title main-menu-title
                         :items (list sel-dir-menu save-settings-menu)
                         :callback main-menu-callback
                         ))
       ;;end let vars clause
       )
    main-menu
       ;;end let, make-file-menu1
       ))



(defparameter *saved-dir-X "c:\\temp\\" "Used to store value from select-dir1-callback")

;;SELECT-DIR1-CALLBACK
;;2019
;;ddd
(defun select-dir1-callback (data interface)
  "U-capi-input-interfaces, sets *saved-dir-X to selected dir path"
  (cond
   ((string-equal data (namestring *saved-dir-X))
    NIL)
   (t 
    (setf *saved-dir-X
          (capi:prompt-for-directory "Select COPY FROM Directory/Folder."
                                     ;;:if-does-not-exist if-does-not-exist
                                     :pathname *saved-dir-X))))
  )
;;TEMP
;; (select-dir1-callback interface)


;;SAVE-GENERIC-SETTINGS-CALLBACK
;;2019
;;ddd
(defparameter *settings-filename "C:\\temp\\settings-X.lisp" "Default file for saving generic settings from save-settings-callback")
;;
(defun save-generic-settings-callback (interface)
  "U-capi-input-interfaces.lisp"
  ;;make *settings-db-X-list* saving (often reinitializing) all settings
  (setf *settings-db-X-list*
        `(
          ( *visible-border-p nil)
          "PUT SETTINGS VARIABLES HERE"
          (*saved-dir-X ,*saved-dir-X)
         ;; ( *settings-db-X-width ,*settings-db-X-width)
          ))
  (save-db *settings-db-X-list* *settings-filename)
   (show-text (format nil "Settings saved to: ~A~%" *settings-filename) 30 nil)
        )



;; SIMPLE-TEXT-INPUT-FRAME
;;2020
;;ddd
(capi:define-interface simple-text-input-frame ()
  ((sv-var1
    :initarg :sv-var1
    :accessor sv-var1
    :initform NIL
    :type  NIL
    :documentation  "Variable 1 to store misc info from calling function, etc"
    )
#|   (sv-var2
    :initarg :sv-var2
    :accessor sv-var2
    :initform NIL
    :type  NIL
    :documentation  "Variable 2 to store misc info from calling function, etc"
    )(sv-interface-subtype
    :initarg :sv-interface-subtype
    :accessor sv-interface-subtype
    :initform NIL
    :type  :symbol
    :documentation  "Subtype of interface :text-input :radio-button :check-button :info")
   (sv-selection-type
    :initarg :sv-selection-type
    :accessor sv-selection-type
    :initform NIL
    :type  :symbol
    :documentation  ":single-selection, :multiple-selection, or :extended-selection"    )|#
   (sv-text-input
    :initarg :sv-text-input
    :accessor sv-text-input
    :initform NIL
    :type  :string
    :documentation  "Data from text input")
   (sv-confirm-input-p
    :initarg :sv-confirm-input-p
    :accessor sv-confirm-input-p
    :initform NIL
    :type  :boolean
    :documentation  "Data from confirm-input-p")
   (sv-input-confirmed-p
    :initarg :sv-input-confirmed-p
    :accessor sv-input-confirmed-p
    :initform NIL
    :type  :boolean
    :documentation  "input-confirmed-p")
      ;;NOTE:  4 WAYS TO CLOSE CALLING PROCESS: 1. change it's slot value, 2. Directly close its process; 3. Directly close it's interface; 4. Poke it and let it close itself (can do with change slot-value of 'chose-this-process-p
#|   (sv-make-func-process
    :initarg :sv-make-func-process
    :accessor sv-make-func-process
    :initform NIL
    :documentation  "MP process from calling function/object.")
   (sv-poke-make-func-process-p
    :initarg :sv-poke-make-func-process-p
    :accessor sv-poke-make-func-process-p
    :type  :boolean
    :initform NIL
    :documentation  "poke-make-func-process-p ")
   (sv-close-make-func-process-p
    :initarg :sv-close-make-func-process-p
    :accessor sv-close-make-func-process-p
    :initform NIL
    :documentation  "If  T, callback closes calling-process at end.")
   (sv-close-interface-p
    :initarg :sv-close-interface-p
    :accessor sv-close-interface-p
    :initform NIL
    :documentation  "If  T, callback closes interface-process at end.")
   ;;sv-na-none-button-output 2019
   (sv-na-none-button-output
    :initarg :sv-na-none-button-output
    :accessor sv-na-none-button-output
    :initform *na-none-button-output ;; =? "NA-NONE"
    :documentation  "NA-NONE-BUTTON-OUTPUT")|#

#|   (sv-main-interface-process
    :initarg :sv-main-interface-process
    :accessor sv-main-interface-process
    :initform NIL
    :documentation  "Main-interface-process = THIS PROCESS")|#
#|   (sv-poke-interface-process-p
    :initarg :sv-poke-interface-process-p
    :accessor sv-poke-interface-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-interface-process-p ")
   (sv-destroy-main-interface-p
    :initarg :sv-destroy-main-interface-p
    :accessor sv-destroy-main-interface-p
    :type  :boolean
    :initform NIL
    :documentation  "If  T, callback closes destroy-main-interface-p at end.")|#
   (sv-special-process
    :initarg sv-special-process
    :accessor sv-special-process
    :initform NIL
    :documentation  "sv-special-process ")
   (sv-poke-special-process-p
    :initarg :sv-poke-special-process-p
    :accessor sv-poke-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-poke-special-process-p ")
#|   (sv-close-special-process-p
    :initarg :sv-close-special-process-p
    :accessor sv-close-special-process-p
    :type  :boolean
    :initform NIL
    :documentation  "sv-close-special-process-p ")|#
   )

  ;;((the-pane :accessor text-input-pane-test-pane))
  (:panes
   (title-pane
    capi:rich-text-pane
    :character-format  (list ;; :face *instr-pane-font-face 
                             :size  *instr-pane-font-size  
                             :color *instr-pane-font-color
                             :bold nil :italic nil :underline nil )
    :paragraph-format '(:alignment :center  ;; :left :right
                         :start-indent 20
                       :offset-indent 20
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops  '(5 10 15 20)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :accepts-focus-p NIL
    :character-format  (list ;; :face *instr-pane-font-face 
                             :size  *instr-pane-font-size  
                             :color *instr-pane-font-color
                             :bold nil :italic nil :underline nil )
    :paragraph-format '(:alignment :center  ;; :left :right
                         :start-indent 20
                       :offset-indent 20
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops  '(5 10 15 20)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :make-instance-extra-apply-args :title-pane-args
    :visible-border T
    :internal-border 15 
    :accepts-focus-p NIL
    ;;end title-pane
    )
   (instr-pane
    capi:rich-text-pane
    :character-format  (list ;; :face *instr-pane-font-face 
                             :size  *instr-pane-font-size  
                             :color *instr-pane-font-color
                             :bold *simple-input-instr-boldp :italic nil :underline nil )
    :paragraph-format '(:alignment :center  ;; :left :right
                         :start-indent 20
                       :offset-indent 20
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops  '(5 10 15 20)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :accepts-focus-p NIL
    :make-instance-extra-apply-args :instr-pane-args
    :visible-border T
    :internal-border 15
    :visible-min-height 40
    :accepts-focus-p NIL
    )
   (quest-pane
    capi:rich-text-pane
    :character-format  (list ;; :face *instr-pane-font-face 
                             :size  *instr-pane-font-size  
                             :color *instr-pane-font-color
                             :bold nil :italic nil :underline nil )
    :paragraph-format '(:alignment :center  ;; :left :right
                         :start-indent 20
                       :offset-indent 20
                        ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                        :tab-stops  '(5 10 15 20)
                        :numbering nil 
                        ;;OR :bullet, :arabic, :lowercase,:uppercase, :lower-roman or :upper-roman.
                        )
    :accepts-focus-p NIL
    :make-instance-extra-apply-args :quest-pane-args
    :visible-border T
    :visible-min-height 40
    :internal-border 15
    :accepts-focus-p NIL
    )
   (text-input-pane
    capi:text-input-pane
    :make-instance-extra-apply-args :input-pane-args
    :max-characters *text-input-max-chars*
    :background :white
    :visible-border T
    :internal-border 15
    :take-focus T
    ) 
  ;;must include in layout programatically
   (radio-button-pane
    capi:radio-button-panel
    ;; :items  '("1"   "Other")
    :make-instance-extra-apply-args :radio-button-pane-args
    :title " Choose the best answer:"
    :title-font *bio-text-button-font
    :layout-class 'capi:column-layout
    ;;  :selected-item nil
    :selection nil
   ;; :callback-type :item-interface
    ;; :choice-selection nil
    :visible-min-width 400
    :background :yellow
    :font  *bio-text-button-font  #|(gp:make-font-description 
                        :family *answer-pane-font-face
                        :weight :normal  :size *answer-font-size)|#    
    )
   ;;must include in layout programatically
   (check-button-pane
    capi:check-button-panel
   :make-instance-extra-apply-args :check-button-pane-args
    ;; :items  '("1"   "Other")
    :title " Choose the best answer:"
    :title-font *bio-text-button-font
    :layout-class 'capi:column-layout
    ;;  :selected-item nil
    :selection nil
    ;; :choice-selection nil
    ;; :callback-type :item-interface
    :visible-min-width 400
    :background :yellow
    :font  *bio-text-button-font 
    )
   (go-fr-button
    capi:push-button
    :background :green
    :text  "       GO to next  >>      "
    :font  *go-frame-button-font 
    ;;   :color-requirements 
    ;;   :selected T
    :default-p T  ;;means if return hit, selects this button
    ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
    ;; choose in calling function
    ;;  :callback 'text-input-OR-button-interface-callback ;;'go-select-scales-frame-callback
    :callback-type :item-interface
    :make-instance-extra-apply-args :go-button-args
    )
   (na-none-button
    capi:push-button
    :background :green
    :text  " NOT APPLICABLE or NONE: GO to next >> "
    :font  *go-frame-button-font 
    :default-p NIL
    :callback-type :item-interface
           ;;NO   :callback 'terminate-interface-callback 
   :make-instance-extra-apply-args :na-none-button-button-args
   :callback 'NA-NONE-BUTTON-CALLBACK  ;;HERE66
  ;;   :make-instance-extra-apply-args :na-none-button-args
    )
   ;;end panes
   )   
  (:layouts
   (column-layout
    capi:column-layout
    '()
    :make-instance-extra-apply-args  :layout-args
    )
   ;;end layouts
   )
  (:default-initargs
   :best-width *text-input-OR-button-interface-best-width
   :best-height *text-input-OR-button-interface-best-height
   :layout 'column-layout
   :background :white  ;;was :red
   :internal-border 15
    :visible-min-height 400
   )
  ;;END text-input-OR-button-interface
  )