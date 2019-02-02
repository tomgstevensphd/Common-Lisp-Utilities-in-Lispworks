;;************************ U-lw-editor.lisp *************************
;;
;;

;;==>CHANGE CURSOR TO BLOCK RED, PRE-LETTER
(SETF CAPI::*PC-CURSOR-STYLES* 
      '(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR CAPI:*EDITOR-CURSOR-DRAG-STYLE* :LEFT-BAR CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* :RED))

(setf CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR
      CAPI:*EDITOR-CURSOR-COLOR* :RED)



;;==>MY-EDIT-PROJECT
;;2018
;;ddd
(defun my-edit-project (dirnames &key omit-files return-found-paths-p)
  "U-LW-editor Opens all files in all dirs except those matching filenames in omit-files (no dir specified)   RETURNS (values all-found-files total-n-files found-dir-subdir-files all-found-paths)"
  (let
      ((n-files)
       (all-found-files)
       (all-found-paths)
       (found-dir-subdir-files)
       (total-n-files 0)
       )

    (loop
     for dir in dirnames
     do
     (let*
         ((x)
          )
       (multiple-value-bind (found-files n-files found-dirs-files found-paths)
           (my-edit-dir-files dir :omit-files omit-files 
                              :return-found-paths-p return-found-paths-p)
         (setf all-found-files (append all-found-files (list found-files))
               found-dir-subdir-files (append found-dir-subdir-files 
                                              (list found-dirs-files))
               all-found-paths (append all-found-paths (list found-paths))
               total-n-files (+ total-n-files n-files))     
         ;;end let,mvb,loop
         )))

    (values all-found-files total-n-files found-dir-subdir-files all-found-paths)
    ;;end let, my-edit-project
    ))
;;TEST
;; (my-edit-project *actr-edit-subdirs :omit-files *actr-edit-omit-files)



;;MY-EDIT-DIR-FILES
;;2018
;;
(defun my-edit-dir-files (dirname  &key  omit-files return-found-paths-p
                                   (omit-lw-bus-p T))
  "U-LW-editor,  Opens buffers for each file, but not windows if   RETURNS (values found-files n-files found-dirs-files found-paths)     When omit-files, omits files that have filenames ONLY matching omit-files filenames (no dir info).  FOUND-DIRS-FILES= (dir file-list) for each dir." 
  (let
      ((found-pathnames) 
       (found-paths)
       (found-dirs-files)
       (n-files)
       )
    (setf dirname (my-delete-last-spaces dirname :delete-strs '("/" "\\")
                                         :delete-chars nil))

    (multiple-value-bind  (filenamestrs file-pathnamestrs subdir-namestrs
                                        file-paths subdir-paths host-str)
        (list-directory-contents dirname)
      (when omit-files
        (setf filenamestrs (delete-items-from-list omit-files filenamestrs)))

      (multiple-value-bind ( found-files n-files found-paths )
          (my-edit-files filenamestrs :dir dirname 
                         :return-found-paths-p  return-found-paths-p)
        (setf found-dirs-files  (list dirname found-files))

        (values found-files n-files found-dirs-files found-paths)
        ;;end mvbs,let, my-edit-dir-files
        ))))
;;TEST
;;(my-edit-dir-files  "C:\\3-TS\\LISP PROJECTS TS\\ACT-R TS\\support\\")



;;MY-EDIT-FILES
;;2018
;;ddd
(defun my-edit-files (filenames &key dir filenames=paths-p return-found-paths-p
                                (file-exts '("lisp" "txt")))
  "U-LW-editor, Makes buffers for all files in paths. If dir then sets paths to all .lisp files in dir. When filenames=paths-p, saves converting to real paths. RETURNS (values  found-files n-files found-paths)  If RETURN-FOUND-PATHS-P, returns entire fpaths of found-files--otherwise 'Can return found-paths' FILE EXT MUST BE IN FILE-EXTS (unless it is NIL) "
  (let
      ((found-paths)   
       (found-files)
       (n-files (list-length filenames))
       (all-buffers)
       (all-windows)
       )
    (when dir
     (setf dir (delete-final-string '("\\" "/") dir)))

    (loop
     for file in filenames
     do
     (let
         ((path)
          (pathname)
          (cur-buffer)
          (cur-window)
          (file-ext)
          (p)
          )
       (declare (ignore p))
       (cond
        (dir
         (setf pathname (format nil "~A/~A" dir file)
               path (pathname pathname)))
        (filenames=paths-p 
         (setf path file
               pathname (namestring file)))
        (t (setf pathname file
                 path (pathname file))))
       ;;(break "path")                   
       ;;NOTE wfind opens buffer in STARTING/CALLING window--displacing contents
       ;; Each new editor:wfind-file-command brings new file into EXISTING buffer.
       ;; Therefoe with wfind- (unlike find- ) don't need to delete any buffers.(causes error)
       ;;FILE EXT MUST BE IN FILE-EXTS (unless it is NIL)
       (setf file-ext (pathname-type path))
       (when (or (null file-exts)
                  (member file-ext file-exts :test 'string-equal))
          (editor:wfind-file-command p path)
           (setf cur-buffer (editor:current-buffer)
                 all-buffers (append all-buffers (list cur-buffer))
                 found-files (append found-files (list file))
                 found-paths (append found-paths (list path)))
           ;;end when
           )
              ;;(break "file")
         ;;end let,loop
         ))
    ;;222
    (setf n-files (list-length found-files))
    (unless return-found-paths-p
      (setf found-paths "Can return found-paths"))
    (values  found-files n-files found-paths)
    ))
;; (my-edit-files '("conflict-tree.lisp" "p-star-cmd.lisp" "p-star-cmd.lisp~" )   :dir "C:/3-TS/LISP PROJECTS TS/ACT-R TS/commands")



;;DELETE-BUFFER-WINDOWS
;;2018
;;ddd
(defun delete-buffer-windows (buffers &key (omit-n 1) )
  "U-LW-editor, Deletes buffer windows in editor, but keeps buffers. Only works when function called in an editor window. Some problems--finish?"
  (let*
      ((p)
       (delete-buffers )
       (n-buffers (list-length buffers))
       (rest-buffers )
       (windows)
       )
    ;;When buffers not NIL or '(NIL)
    (when (and buffers (car buffers))
    (cond
     ((> (- n-buffers omit-n) 0)
      (setf delete-buffers (nthcdr  omit-n buffers)
            rest-buffers (butlast buffers (- n-buffers omit-n))))
     (t (setf rest-buffers (list (car buffers))
              delete-buffer (cdr buffers))))
    (loop
     for buffer in delete-buffers
     do
     (let
         ((buffer-window)
          (cur-window)
          (cur-buffer)
          (p)
          )
       (declare (ignore p))
       ;;111
       (when buffer
;;         (setf (editor:current-buffer) buffer)
            (setf  cur-buffer (editor:current-buffer))
         ;;(editor:use-buffer buffer)
        ;; (setf (editor:current-window) (editor::find-window-for-buffer cur-buffer))
         (setf (editor:current-window) (editor::find-window-for-buffer buffer))
         ;;above causes error: Setting current window for the wrong process Background execute 1 : #<EDITOR::WM-WINDOW "motor-compilation.lisp" 2320B4E3>.

     ;;SSSSSS DON'T USE DELETE-BUFFERS? DELETE ONE BUFFER AT A TIME WHEN THE NEW ONE IS CREATED??

         ;;IF RUN IN LISTENER, CAUSES ERROR: 
         ;; Error: Setting current window for the wrong process CAPI Execution Listener 1 : #<EDITOR::WM-WINDOW "p-star-cmd.lisp" 271F962B>.
         (setf cur-window (editor:current-window)
               windows (append windows (list cur-window)))
         (break "cur-window=? cur-buffer")
         #|           ;; (editor:current-window) can be set with setf
         (setf buffer-window (editor::find-window-for-buffer buffer))
                  (break "buffer-window")|#
         (editor:delete-window-command p)
         ;;end when
         )
         
       #|       (when buffer
         ;;(editor::set-current-buffer buffer)
         ;;closes windows not buffers  (editor:delete-window-command p)     
         (break "cur-window")
         (setf buffer-window (editor::find-window-for-buffer buffer))
               ;;cur-window (editor::set-current-window buffer-window))
         (editor:delete-window-command p))|#
       ;;end let, loop
       ))
    (when (and rest-buffers (car rest-buffers)) ;;note (when  '(nil) 7) = 7
      (editor::set-current-buffer (car rest-buffers)))
    ;;end initial when
    )
    windows
    ;;end let, delete-buffer-windows
    ))
;;TEST
;; (delete-buffer-windows buffers)
;; WORKS within my-find-files


;;FDF
;;
;;ddd
(defun  fdf  (dspec-sym)
   "In U-LW-editor, finds a definition for dspec-sym in any buffer?"
   (editor:find-source-for-dspec-command t dspec-sym :same-window nil)
   )

;;zzz me --------------  UNDEFINE BUFFER DEFS (CNTR-8) -------------------------
;;WORKS
#|
(editor:defcommand "Undefine Buffer Defs" (p)
   "Undefines all definitions in current buffer. Make sure have right one."     
       "Undefine current buffer defs"
    (declare (ignore p))
    (editor:undefine-buffer-command  p)
    ) 
  (editor:bind-key "Undefine Buffer Defs" "Control-8" :global :pc)

;;zzz ME ------------- FIND SOURCE  (CNTR-9) (asks in echo area) ------------------
;;WORKS
(editor:defcommand "Find Source" (p)
   "Moves cursor to function definition as in Find Source in debugger."     
       "Find Source"
    (declare (ignore p))
    (editor:find-source-command p) ;; NIL nil  :same-window nil)
    ) 
  (editor:bind-key "Find Source" "Control-9" :global :pc)
 


(editor:defcommand "Find Next Mismatched PARENS" (p)
   "Moves cursor to next mismatched parens."     
       "Find Mismatched Parens"
    (declare (ignore p))
    (EDITOR::FIND-NEXT-MISMATCHED-PARENTHESIS p) ;; NIL nil  :same-window nil)
    ) 
  (editor:bind-key "Find Next Mismatched PARENS" "Control-7" :global :pc)
|#

;;SSS doesn't work, fix?
#|(defun udb ()
  "In U-LW-editor. Undefines all definitions in current buffer"
  (editor:undefine-buffer-command T )
  )|#
#|(editor:defcommand "Undefine Buffer Defs" (p)
   "Undefines all definitions in current buffer. Make sure have right one."     
       "Undefine current buffer defs"
    (declare (ignore p))
    (editor:undefine-buffer-command  p)
    ) 
  (editor:bind-key "Undefine Buffer Defs" "Control-8" :global :pc)|#


#| SSS doesn't work, fix?
(defun fds (symbol)
   "In U-LW-editor, finds a definition for a SYMBOL in any buffer?"
   (editor:find-source-command t symbol t :same-window nil)
   )|#
  

;;from Dave Fox, LW
;;There is no built-in way to open more than one file at  > a time.
;;  If you like that, you could put it in your .lispworks file.
;; zzz "Find Multiple Files" from Dave Fox, LW

;; COPIED FROM .LISPWORKS
#| 
  (editor:defcommand "Find Multiple Files" (p)
      "Prompt for multiple files, and open each like WFind File command."
       "Find multiple files"
    (declare (ignore p))
    (loop for file in (capi:prompt-for-files
                       "Select multiple files to open: "
                       :filter "*.lisp;*.lsp"
                       :filters '("Lisp Source Files" "*.lisp;*.lsp" "All files" "*.*"))
          do (editor:wfind-file-command nil file)))
 
  (editor:bind-key "Find Multiple Files" "Control-i" :global :pc)
|#
;;ZZZ FROM MARTIN SIMMONS, LW
;; FOR CLIPBOARD RING
  (editor:bind-key "Rotate Kill Ring" "Control-j" :global :pc) 


;;
;;LIST-FILE-OBJECTS
;;
;;ddd
(defun list-file-objects (file-prename object-type-list &key (pathroot "C:\\3-TS\\LISP PROJECTS TS\\")(dir "shaq")(ext ".lisp") return-object-list-string-p )
  "In U-lw-editor.lisp, creates a window and prints a list of object-type items in it, Uses READ-LINE. Use list-file-nested-lists for complex DB PList objects."
  (let*
      ((pathname (format nil "~A~A\\~A~A" pathroot dir file-prename ext))
       (file-name (format nil "~A defclass file" pathroot))
       (object-list-file-inst (make-instance 'file-list-frame))
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
            (read-line s-input nil 'eof ))
        (cond
         (eof
          (return))
         (t (multiple-value-setq (token rest-string)
              (match-first-token object-string line  :delimiter-list '(#\(   #\space)))
                                                       ;;was '( "\(" " ")))
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
    ;;display the file
    (capi:display object-list-file-inst)

    #|      (with-slots (answer-button-panel)  q-frame-inst
        ;;MAKE THE  BUTTON PANEL
      (setf answer-button-panel (make-radio-button-panel ans-instruction-text answer-array-list))
      ;;Put the BUTTON IN THE ALREADY CREATED FRAME
        (capi:apply-in-pane-process quest-rich-text-pane
                   #'(setf capi:rich-text-pane-text) question-text-formated quest-rich-text-pane |#
    ;;print to the pane
    (with-slots (text-pane-1) object-list-file-inst
      (capi:apply-in-pane-process  text-pane-1
                                   #'(setf capi:rich-text-pane-text)   object-list-string text-pane-1)    (setf (capi:interface-title  object-list-file-inst)
                                           (format nil ">> ~A CONTENTS"  file-prename))         
               
      ;;end let,with-open-file list-my-defclasses
      ;;end with-slots
      )
    (if return-object-list-string-p
        object-list-string)
    ;;end let, list-file-objects
    ))
;;TEST
;; (list-file-objects "SHAQ-new-scales" '("my-defclass"))


;;LIST-ALL-FILE-OBJECTS
;;
;;ddd
(defun list-all-file-objects (filename-no-ext )
  "In U-LW-editor, lists defparameter, defun, my-defclass, my-make-instance, and first-order nested-list keys"
  (list-file-objects filename-no-ext '( defparameter my-defclass my-make-instance defun))
 )
;;TEST 
;;  (list-all-file-objects "SHAQ-new-scales")


;;LIST-MY-DEFCLASSES
;;
;;ddd
(defun list-my-defclasses (filename-no-ext)
  "In U-LW-editor, lists my-defclass and my-make-instance objects"
  (filename-no-ext (list "my-defclass" "my-make-instance"))
 )

;;LIST-SCALECLASSES
;;
;;ddd
(defun list-scaleclasses ()
  "In U-LW-editor"
   (list-ALL-file-objects "SHAQ-new-scales") ;; (list "my-defclass"))
       )
;; (list-scaleclasses)


;;LIST-NESTED-OBJECTS  (lists ALL DEFPARAMETER OBJECTS)
;;
;;ddd
(defun list-file-nested-lists (file-prename  &key object-sym  (pathroot "C:\\TOM\\LISP PROJECTS TS\\")(dir "shaq")(ext ".lisp") return-object-list-string-p add-defpar-newlines-p )
  "In U-lw-editor.lisp, creates a window and prints a list of DEFPARMETER items in it. Can list nested-list items horizontally or vertically using ADD-DEFPAR-NEWLINES-P. RETURNS the list. if return-object-list-string-p. If OBJECT-SYM, only finds keys for it. Uses READ."
  (let*
      ((pathname (format nil "~A~A\\~A~A" pathroot dir file-prename ext))
       (file-name (format nil "~A defclass file" pathroot))
       (object-list-file-inst (make-instance 'file-list-frame))
       (eof)
       (object-list-string (format nil "In pathname: ~A~% OBJECT-LIST=> ~A~%" pathname  "LISP OBJECTS--ESP   DEFPARAMETER NESTED-LISTS"))
       )
    ;;object-type loop
#|    (loop
     for object-type in object-type-list
     with object-string
     ;;  with object-list-string
     do|#
     (with-open-file (s-input pathname :direction :input) 
       (setf  object-list-string (format nil "~A~%~%>>>>>  ALL LISP OBJECTS  <<<<<~%" object-list-string ))
       (loop
        for n from 0 to 3000
        with object
        with length-object
        with object-1st 
        with object-2nd 
        with defpar-list
        with defpar-list-string
        do
#|        (multiple-value-setq (object eof)
            (read s-input  nil  'eof ))|#
        (setf object (read s-input  nil  'eof )) ;;returns 'eof if reaches end
        (if (listp object )(setf  length-object (list-length object)))
        (setf out1 (format nil "object= ~A~%" object ))
        (cond
        ((equal object 'eof)
          (return))
         ((and (listp object)(> length-object 2)  (listp  (third object))
               (equal (car object) 'defparameter)
               (if object-sym (equal object-sym (second object)) T)   )
          (setf object-list-string (format nil "~A
 -----------------------------------------------------------------------------" object-list-string))
            (setf  defpar-list (third object))
           ;; (setf out2 (format nil "~A~%~%~A" out2 defpar-list))
            (cond
             ((> (list-length defpar-list) 1)
              (setf  defpar-list-string
                     (find-keys-in-nested-lists defpar-list
                                                :sublist-newline-p add-defpar-newlines-p)))
             (t (setf defpar-list-string (format nil "~A" defpar-list))))  
            (setf object-list-string 
                  (format nil "~A~%~A" object-list-string defpar-list-string))
            ;;end and clause
            )
           ((listp object)
            (setf object-1st (first object))
            (if (> (list-length object) 1)
                (setf object-2nd (second object))
              (setf object-2nd ""))
            (setf  object-list-string (format nil "~A~%  (~A    ~A)" object-list-string object-1st object-2nd))
            ;;end listp clause
            )
           (t   (setf  object-list-string (format nil "~A~%  ~A " object-list-string object))))
        ;;end loop, with-open-file
        ))
    ;;display the frame
    (capi:display object-list-file-inst)

    #|      (with-slots (answer-button-panel)  q-frame-inst
        ;;MAKE THE  BUTTON PANEL
      (setf answer-button-panel (make-radio-button-panel ans-instruction-text answer-array-list))
      ;;Put the BUTTON IN THE ALREADY CREATED FRAME
        (capi:apply-in-pane-process quest-rich-text-pane
                   #'(setf capi:rich-text-pane-text) question-text-formated quest-rich-text-pane |#
    ;;print to the frame
    (with-slots (text-pane-1) object-list-file-inst
      (capi:apply-in-pane-process  text-pane-1
                                   #'(setf capi:rich-text-pane-text)   object-list-string text-pane-1)      (setf (capi:interface-title object-list-file-inst) 
                                       (format nil ">> ~A CONTENTS" file-prename))
                                                                                                                  

               
      ;;end let,with-open-file list-my-defclasses
      ;;end with-slots
      )
    (if return-object-list-string-p
        object-list-string)
    ;;end let, list-file-objects
    ))
;;TEST
;;  (list-file-nested-lists "shaq-new-scales" :add-defpar-newlines-p nil )
;;  WORKS  EG ( MY-DEFCLASS  ST2SOCINTIMNOFAMSCALE
;;  (list-file-nested-lists "SHAQ-all-scale-and-question-var-lists" :add-defpar-newlines-p nil )


           
;;FILE-LIST-FRAME
;;
;;ddd
(capi:define-interface file-list-frame ()
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
   :enabled T
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
   :title "File List Frame"))







;;********************************OLD FROM U-editor.lisp **********************
;;
;; Utilities to change editor behavior etc


;;***>>> MOST FUNCTIONS ARE IN U-LW-editor.lisp


(SETF CAPI::*PC-CURSOR-STYLES* 
      '(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR CAPI:*EDITOR-CURSOR-DRAG-STYLE* :LEFT-BAR CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* :RED))

(setf CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR
      CAPI:*EDITOR-CURSOR-COLOR* :RED)





#|Go Back Editor Command
Arguments: None
Key sequence: Ctrl+X C
Takes you back to the most recently recorded location. If a prefix argument
count is supplied, it takes you back count locations in the location history. If
count is negative, it takes you forward again count locations in the history,
provided that no more locations have been recorded since you last went
back.
Select Go Back Editor Command
Arguments: None
Key sequence: Ctrl+X M
Takes you back to a previously recorded location, which you select from a
list.
Any prefix argument is ignored.
Go Forward Editor Command
Arguments: None
Key sequence: Ctrl+X P
Takes you back to the next location in the ring of recorded locations. If a
prefix argument count is supplied, it takes you forward count locations in
the location history. If count is negative, it takes you back count locations in
the history
|#


#|
6.4 EDITOR SOURCE CODE -- page 197 eduser-manual
The section does not apply to LispWorks Personal Edition.
LispWorks comes with source code for the editor, which you can refer to when
adding editor extensions.
6.4.1 Contents
The directory lib/6-1-0-0/src/editor/ contains most of the source files of
the LispWorks editor. Some low-level source code is not distributed.
6.4.2 Source location
To enable location of editor definitions by Find Source and related commands,
configure LispWorks as described under "Finding source code" in the LispWorks
User Guide and Reference Manual.
6.4.3 Guidelines for use of the editor source code
Some care is needed when working with the supplied editor source code, to
ensure that you do not compromise the IDE or introduce a dependancy on a
particular release of LispWorks.
In particular please note:
• The editor source code may not match the compiled code in the Lisp-
Works image exactly, for example if editor patches have been loaded.
• Modifications to the EDITOR package definition are not allowed.
6 Advanced Features
198
• Redefining existing definitions is not recommended. It is better to define a
new command to do what you want. If you find a bug or have a useful
extension to an existing definition then please let us know.
• Do not rely on the expansion of exported macros.
• If you use any internal (that is, not exported) EDITOR symbols, please tell
us, so we can consider how to support your requirements. In addition,
some internal macros have been removed from the LispWorks image and
these should not be used.
|#

#|
CAPI::*PC-CURSOR-STYLES* = 
(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE* :CARET CAPI:*EDITOR-CURSOR-DRAG-STYLE* :CARET CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* NIL)

CAPI::*EMACS-CURSOR-STYLES* =
(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE* :INVERSE CAPI:*EDITOR-CURSOR-DRAG-STYLE* :LEFT-BAR CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :OUTLINE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* NIL)
|#


