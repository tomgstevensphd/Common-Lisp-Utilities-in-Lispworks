;; ******************* 0-keybinds.lisp *************************
;;
(DEFPARAMETER *KEYBINDS
"TO GET UPDATED LIST, Step 1: Hit CONTROL-1, Step2: Type ?, Step 3: Select  b
'selection-key-binds.lisp' Buffer Bindings:
'Lisp' Major Mode Bindings:
)                   Lisp Insert )
Tab                 Indent Selection or Complete Symbol

Global Mode Bindings:
Backspace           Delete Previous Character
Backspace           Delete Previous Character
* Ctrl+(              Backward List
* Ctrl+)              Forward List
Ctrl+*              Set Prefix Argument
Ctrl++              Down List
Ctrl+-              Negative Argument
Ctrl+.              Go Forward
Ctrl+1              Help
Ctrl+2              Function Documentation
Ctrl+3              Function Argument List
** Ctrl+4              Function Arglist Displayer
Ctrl+5              Function Arglist
** Ctrl+8              Undefine Buffer Defs
** Ctrl+9              Find Source
* Ctrl+<              Backward Form
* Ctrl+=              What Cursor Position
* Ctrl+>              Forward Form
Ctrl+A              Mark Whole Buffer
Ctrl+Backspace      Kill Previous Word
Ctrl+C              Save Region Preserving Selection
Ctrl+Delete         Kill Line
* Ctrl+Down           History Next
Ctrl+E              Emacs Command
Ctrl+End            End Of Buffer Cancelling Selection
Ctrl+F              Forward Search
Ctrl+F11            Circulate Buffers
* Ctrl+F2             Document Command
Ctrl+F4             Kill Buffer
Ctrl+F5             Refresh Screen
Ctrl+F7             Compile Buffer
Ctrl+F8             Evaluate Expression
Ctrl+F9             Walk Form
Ctrl+H              Query Replace
Ctrl+Home           Beginning Of Buffer Cancelling Selection
Ctrl+I              Find Multiple Files
Ctrl+Insert         Save Region Preserving Selection
Ctrl+J              Rotate Kill Ring
* Ctrl+K              Capitalize Word
Ctrl+L              Lowercase Region
Ctrl+Left           Backward Word Cancelling Selection
Ctrl+M              Emacs Command Escape
Ctrl+N              New Buffer
Ctrl+O              Wfind File
Ctrl+P              Print File
Ctrl+Q              Quoted Insert
Ctrl+Q              Quoted Insert
Ctrl+Right          Forward Word Cancelling Selection
** Ctrl+S              Go Forward
** Ctrl+Shift+B        Go Back
Ctrl+Shift+D        Show Documentation
Ctrl+Shift+Down     Forward Paragraph Modifying Selection
Ctrl+Shift+End      End of Buffer Modifying Selection
Ctrl+Shift+F11      List Buffers
Ctrl+Shift+F2       Document Variable
Ctrl+Shift+F7       Compile Region
Ctrl+Shift+F8       Indent Rigidly
Ctrl+Shift+Home     Beginning of Buffer Modifying Selection
Ctrl+Shift+Left     Backward Word Modifying Selection
Ctrl+Shift+R        Compile Region
Ctrl+Shift+Right    Forward Word Modifying Selection
** Ctrl+Shift+S        Save All Files
Ctrl+Shift+Up       Backward Paragraph Modifying Selection
Ctrl+T              Transpose Characters
Ctrl+T              Transpose Characters
Ctrl+U              Uppercase Region
** Ctrl+Up             History Previous
Ctrl+V              Un-Kill
Ctrl+W              Kill Region
Ctrl+W              Kill Region
Ctrl+X Ctrl+X C D   Dynamic Completion
Ctrl+X Ctrl+X F     Expand File Name
Ctrl+X Ctrl+X S R   Rotate Active Finders
Ctrl+X Ctrl+X T C   Continue Tags Search
Ctrl+X E E          Evaluate Expression
Ctrl+X E F          Evaluate Defun
Ctrl+X E R          Evaluate Region
Ctrl+X M E          Macroexpand Form
Ctrl+X W A L K      Walk Form
Ctrl+Y              Un-Kill
Ctrl+Y              Un-Kill
Ctrl+Z              Undo
** Ctrl+[              Beginning of Defun
** Ctrl+]              End of Defun
Delete              Delete Next Character
Delete              Delete Next Character
Down                Next Line Cancelling Selection
End                 End Of Line Cancelling Selection
** F1                  Help
F11                 Select Previous Buffer
F16                 Un-Kill
F18                 Save Region Preserving Selection
F2                  Function Documentation
F20                 Kill Region
F3                  Incremental Search
F7                  Compile Defun
F8                  Evaluate Defun
F9                  Macroexpand Form
Help                Help
Help                Help
Home                Beginning of Line Cancelling Selection
Insert              Overwrite Mode
Insert              Overwrite Mode
Kp-Enter            New Line
Kp-Enter            New Line
Left                Backward Character Cancelling Selection
Meta+,              Continue Tags Search
Meta+,              Continue Tags Search
Meta+.              Find Source
Meta+.              Find Source
Meta+/              Dynamic Completion
Meta+/              Dynamic Completion
Meta+?              Find Tag
Meta+?              Find Tag
Meta+C              Document Command
Meta+Ctrl+.         Rotate Active Finders
Meta+Ctrl+.         Rotate Active Finders
Meta+Ctrl+I         Complete Symbol
Meta+Ctrl+I         Complete Symbol
Meta+End            End Of Buffer Cancelling Selection
Meta+F1             Show Documentation
Meta+F11            Select Buffer
Meta+F4             Delete Window
Meta+Home           Beginning Of Buffer Cancelling Selection
Meta+K              Document Key
Meta+Shift+End      End of Buffer Modifying Selection
Meta+Shift+Home     Beginning of Buffer Modifying Selection
Meta+Shift+Left     Backward Word Modifying Selection
Meta+Shift+Right    Forward Word Modifying Selection
Meta+Tab            Expand File Name
Meta+Tab            Expand File Name
Meta+V              Document Variable
Meta+X              Extended Command
Meta+X              Extended Command
Newline             Indent New Line
Newline             Indent New Line
Next                Scroll Window Down Moving Point
Prior               Scroll Window Up Moving Point
Return              New Line
Return              New Line
Right               Forward Character Cancelling Selection
Shift+Delete        Delete Previous Character
Shift+Down          Next Line Modifying Selection
Shift+End           End Of Line Modifying Selection
Shift+F1            Function Argument List
Shift+F2            Document Key
Shift+F3            Reverse Incremental Search
Shift+F8            Evaluate Last Form
Shift+Home          Beginning of Line Modifying Selection
Shift+Insert        Un-Kill
Shift+Insert        Un-Kill
Shift+Left          Backward Character Modifying Selection
Shift+Next          Scroll Window Down Modifying Selection
Shift+Prior         Scroll Window Up Modifying Selection
Shift+Right         Forward Character Modifying Selection
Shift+Up            Previous Line Modifying Selection
Up                  Previous Line Cancelling Selection
" )



(DEFPARAMETER *MYKEYS  (format nil 
";; USING EMACS COMMANDS  OTHER EMULATION
  ;;; calling an Emacs command starting with escape
  ;;; This makes MSW or Mac( Ctrl-m < ) == Emacs( Escape < )
 \"Emacs Command Escape\" \"Control-m\")
;; FOR OPENING MULTIPLE FILES ------------------
  \"Find Multiple Files\" \"Control-i\" )
;; FOR CLIPBOARD RING
   \"Rotate Kill Ring\" \"Control-j\" ) 
   \"Abort Recursive Edit\" \"Control-\]\")
   \"Evaluate Defun\" #(\"Control-x\"  \"e\"  \"f\"))
   \"Evaluate Expression\" #(\"Control-x\"  \"e\"  \"e\")) 
   \"Evaluate Region\" #(\"Control-x\"  \"e\"  \"r\")) 
   \"Macroexpand Form\" #(\"Control-x\"  \"m\"  \"e\")) 
   \"Walk Form\" #(\"Control-x\"  \"w\"  \"a\" \"l\" \"k\")) 
;; COMPLETION
   \"Complete Symbol\" #(\"Control-x\"  \"Control-x\"  \"c\"))
   \"Dynamic Completion\" #(\"Control-x\"  \"Control-x\"  \"c\" \"d\"))
   \"Expand File Name\" #(\"Control-x\"  \"Control-x\"  \"f\")) 
   \"Rotate Active Finders\"#(\"Control-x\"  \"Control-x\"  \"s\" \"r\")) 
;; TAGS
   \"Find Tag\" \"Meta-\?\")
  \"Continue Tags Search\" \"Meta-\,\")
   \"Find Tag\" #(\"Control-x\"  \"Control-x\"  \"t\"))
   \"Continue Tags Search\"#(\"Control-x\"  \"Control-x\" \"t\"  \"c\"))
   \"Show Documentation\" \"Control-D\" )
 \"Function Documentation\" \"Control-2\" ) 
;;DOCUMENTATION in editor help window
   \"Function Argument List\" \"Control-3\")  
 ;; arglist in editor echo area
   \"Function Arglist Displayer\" \"Control-4\")  
 ;;shows displayer
   \"Function Arglist\" \"Control-5\")   
   \"Reset Echo Area\" #(\"Control-x\" \"e\" \"k\") 
;;SEARCHING
   \"BEGINNING OF DEFUN\"  \"Control-[\" 
   \"END OF DEFUN\" \"Control-]\" ) 
   \"Forward List\" \"Control-\)\" ) 
   \"Backward List\" \"Control-\(\"  ) 
   \"FORWARD FORM\"  \"Control->\" ) 
   \"BACKWARD FORM\" \"Control-<\" ) 
   \"GO BACK\" \"Control-B\" OR? CONTR-SHIFT-B? )  
   \"GO FORWARD\" \"Control-.\" )    
     NONE: \"SELECT GO BACK\" \"Control-B\")  
;;IN LISTENER ONLY
   \"History Previous\" \"Ctrl-Up\"  ) 
   \"History Next\" \"Ctrl-Down\"  ) 
   \"Execute or Yank from Previous Prompt\" \"Return\"
   \"Throw To Top Level\" #(\"Control-d\"  \"Control-d\"  \"t\")  
   \"Insert From Previous Prompt\"#(\"Control-d\" \"Control-d\" \"i\") 
   \"Document Key\" \"Alt-k\")  
   \"Document Variable\" \"Alt-v\")   
  "))



(DEFPARAMETER *MSWKEYS
"
;; Movement
   Forward Character Cancelling Selection           Right   )
   Backward Character Cancelling Selection           Left   )
   Next Line Cancelling Selection       Down   )
   Previous Line Cancelling Selection                  Up   )

   Beginning of Line Cancelling Selection               Home   )
   Beginning of Buffer Cancelling Selection             Control-Home   )
#-mswindows    Beginning of Line Cancelling Selection       Begin   )
   Beginning of Line Modifying Selection   Shift-Home   )
#-mswindows    Beginning of Line Modifying Selection        Shift-Begin   )
   Beginning Of Line After Prompt Cancelling Selection       Home   
#-mswindows    Beginning Of Line After Prompt Cancelling Selection       Begin 
   Beginning Of Line After Prompt Modifying Selection       Shift-Home   
#-mswindows    Beginning Of Line After Prompt Modifying Selection       Shift-Begin  

   End of Line Modifying Selection         Shift-End   )
   End of Line Cancelling Selection        End   )
   End of Buffer Cancelling Selection      Control-End   )
   Beginning Of Buffer Modifying Selection              Control-Shift-Home   )
   Beginning Of Buffer Modifying Selection              Meta-Shift-Home   )
   Beginning Of Buffer Cancelling Selection             Meta-Home   )
   End Of Buffer Modifying Selection       Control-Shift-End   )
   End Of Buffer Modifying Selection       Meta-Shift-End   )
   End Of Buffer Cancelling Selection      Meta-End   )

   Forward Character Modifying Selection   Shift-Right   )
   Forward Word Modifying Selection        Control-Shift-Right   )
   Forward Word Cancelling Selection       Control-Right   )
   Forward Word Modifying Selection        Meta-Shift-Right   )

   Backward Character Modifying Selection               Shift-Left   )
   Backward Word Modifying Selection       Control-Shift-Left   )
   Backward Word Cancelling Selection      Control-Left   )
   Backward Word Modifying Selection       Meta-Shift-Left   )
   Echo Area Backward Character Modifying Selection      Shift-Left    
   Echo Area Backward Word Modifying Selection   Control-Shift-Left 
   Echo Area Backward Word Cancelling Selection        Control-Left   
   Next Line Modifying Selection           Shift-Down   )
   Previous Line Modifying Selection       Shift-Up   )
   Forward Paragraph Cancelling Selection               Control-Down   )
   Forward Paragraph Modifying Selection   Control-Shift-Down   )
   Backward Paragraph Cancelling Selection              Control-Up   )
   Backward Paragraph Modifying Selection               Control-Shift-Up   ) 
   History Previous       Ctrl-Up    :mode    Pc Execute   )
   History Next       Ctrl-Down    :mode    Pc Execute   )
   History Search       Ctrl-Shift-Up    :mode    Pc Execute   )
   Scroll Window Up Moving Point           Prior   )
   Scroll Window Up Modifying Selection    Shift-Prior   )
   Scroll Window Down Moving Point         Next   )
   Scroll Window Down Modifying Selection               Shift-Next   )
   Delete Previous Character               Shift-Delete   )
   Kill Line                  Control-Delete   )
   Un-Kill                    Shift-Insert   )
   Save Region Preserving Selection        Control-Insert   )
   New Line       Kp-Enter   )
;;; Windows 
   Delete Window       Meta-F4   )
   Refresh Screen       Control-F5   )
;;;  CASE
   Capitalize Word       Control-k   )
   Uppercase Region       Control-u   )
   Lowercase Region       Control-l   )
;;; PREFIX ARGUMENTS
   Set Prefix Argument       Control-*   )
;;; ACCELERATED COMMANDS
;; Because we add accelerators for the following commands in PC mode, these
;; editor bindings are not actually reachable. However, they are defined here
;; so that the editor's online help (Help > Command to Key etc) remains
;; consistent 
;;    FILE > NEW   
   New Buffer       ctrl-n   )
;;    File > Open...   
   Wfind File        Control-o   ) ; not strictly correct as menu item calls
                  ;  editor:find-file-command , with arguments. But the
				            ;  the effect is most like Wfind File, ensuring
				            ;  it reuses the current window 
;;    File > Save   
   Save File       Control-s   )   ; not strictly correct as    File > Save    can call
                  ;  editor:save-file-as-command
   Save All Files       Control-S   )
;;    File > Print...   
   Print File       Control-p   )
;;    Edit > Undo   
   Undo       Control-z   )
;;    Edit > Cut   
   Kill Region       Control-x   ) ; not strictly correct as menu item
                  ;  puts text on the LispWorks clipboard, not
				            ;  the editor kill ring, but the effect is the same.
;;    EDIT > COPY   
   Save Region Preserving Selection       Control-c   )
   Save Region Preserving Selection       Control-c    :mode    Pc Execute   ) 
;;    Edit > Paste   
   Un-Kill       Control-v   )
;;    Edit > Find...   
   Forward Search       Control-f   )
;;    Edit > Select All   
   Mark Whole Buffer       Control-a   )
;; SEARCHING
   Incremental Search       F3   )
   Reverse Incremental Search       Shift-F3   )
   Query Replace    #+mswindows    Control-h    #-mswindows    Control-r   )
#-MSWINDOWS
   Delete Window       Control-w   )
#-mswindows
   Save All Files And Exit       Control-q   )
#+mswindows
   Quoted Insert       Control-q   )
;; LISP OPERATIONS
#+mswindows ; Control-r is Query Replace on other platforms
   Transpose Forms       Control-r   )
;; CALLING AN EMACS COMMAND.
;; This makes MSW( Ctrl-e Ctrl-k ) == Emacs( Ctrl-k )
   Emacs Command       Control-e   )
")



