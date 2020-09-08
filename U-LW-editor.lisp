;;************************ U-lw-editor.lisp *************************
;;
;;

;;==>CHANGE CURSOR TO BLOCK RED, PRE-LETTER
(SETF CAPI::*PC-CURSOR-STYLES* 
      '(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR CAPI:*EDITOR-CURSOR-DRAG-STYLE* :LEFT-BAR CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* :RED))

(setf CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR
      CAPI:*EDITOR-CURSOR-COLOR* :RED)




;;SSSSSS MAKE MY-COMPARE-BUFFERS USING: 
#| "COMPARE BUFFERS" Editor Command
Arguments: buffer1 buffer2
Key sequence: None
Compares the text in the current buffer with that another buffer.
The first argument defaults to the current buffer. The second defaults to
the next editor buffer.
Differences in whitespace are ignored by default, according to the value of
compare-ignores-whitespace.
|#


;;MY-EDIT-PROJECT (see end for older version w/ diff RESULTS values)
;;2018
;;ddd
(defun my-edit-project (dirnames &key omit-files return-found-paths-p
                                 use-my-edit-files-p load-p (incl-filetypes '("lisp"))
                                 omit-filetypes )
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
           (my-edit-dir-files dir :omit-files omit-files :load-p load-p
                              :incl-filetypes  incl-filetypes 
                              :omit-filetypes omit-filetypes
                              :return-found-paths-p return-found-paths-p
                              :use-my-edit-files-p use-my-edit-files-p)
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
                                   (incl-filetypes '("lisp")) omit-filetypes
                                   (omit-lw-bus-p T) use-my-edit-files-p load-p 
                                   external-format check-function)
  "U-LW-editor,  Opens buffers for each file, but not windows if   RETURNS (values found-files n-files found-dirs-files found-paths)     When omit-files, omits files that have filenames ONLY matching omit-files filenames (no dir info).  FOUND-DIRS-FILES= (dir file-list) for each dir. Note: can't use in .LISPWORKS "
  (let
      ((found-pathnames) 
       (found-paths)
       (found-files)
       (found-dirs-files)
       (n-files)
       )
    (setf dirname (my-delete-last-spaces dirname :delete-strs '("/" "\\")
                                         :delete-chars nil))
    (multiple-value-bind  (filenamestrs file-pathnamestrs subdir-namestrs
                                        file-paths subdir-paths host-str)
        (list-directory-contents dirname  :incl-filetypes  incl-filetypes 
                                 :omit-filetypes omit-filetypes)
      (when omit-files
        (setf filenamestrs (delete-items-from-list omit-files filenamestrs)))

      (cond
       (use-my-edit-files-p
        (multiple-value-setq ( found-files n-files found-paths )
            (my-edit-files filenamestrs :dir dirname 
                           :return-found-paths-p  return-found-paths-p)))
       ;;new: uses my-open-buffers, works better if NOT ON .LISPWORKS 
       ;;causes error of can't use until multi-processing enabled.
       (t 
        (multiple-value-setq ( found-files  found-paths n-files)
            (my-open-buffers file-pathnamestrs :load-p load-p
                             :check-function check-function 
                             :external-format external-format))
        (setf found-dirs-files  (list dirname found-files))))
      (values found-files n-files found-dirs-files found-paths n-files)
      ;;end mvbs,let, my-edit-dir-files
      )))
;;TEST
;;(my-edit-dir-files  "C:\\3-TS\\LISP PROJECTS TS\\ACT-R TS\\support\\")



;;MY-OPEN-BUFFERS
;;2019
;;ddd
(defun my-open-buffers (filenames  &key load-p open-new-windows-p 
                                   check-function (external-format :default) )
  "U-lw-editor uses editor:find-file-buffer to open buffers. RETURNS: (values new-buffers paths n-files).  Return a buffer associated with the file Pathname, reading the file into a new buffer if necessary.  The second value is T if we created a buffer, NIL otherwise.  If the file has already been read, we check to see if the file has been modified on disk since it was read, giving the user various recovery options."
  (let*
      ((paths)
       (new-buffers)
       (n-files (list-length filenames))
       )    
    (loop
     for filename in filenames
     do
     (multiple-value-bind (new-buffer path)
         (my-open-buffer filename  :load-p load-p
                         :open-new-window-p open-new-windows-p
                         :check-function check-function
                         :external-format external-format)
       (setf new-buffers (append new-buffers (list new-buffer))
             paths (append paths (list path)))
     ;;end let, loop
     ))
    (values new-buffers paths n-files)
    ))
;;TEST
;; (my-open-buffers *my-lw-init-files*)


;;MY-OPEN-BUFFER
;;2019
;;ddd
(defun my-open-buffer (filename   &key load-p  open-new-window-p
                                  check-function  (external-format :default) )
  "U-lw-editor uses editor:find-file-buffer to open buffers.RETURNS: (values new-buffer path) 
Return a buffer associated with the file Pathname, reading the file into a new buffer if necessary.  The second value is T if we created a buffer, NIL otherwise.  If the file has already been read, we check to see if the file has been modified on disk since it was read, giving the user various recovery options."
  (let*
      ((path (pathname filename))
       (new-buffer (editor:find-file-buffer path check-function external-format))
       (filename (pathname-name path))
       (type (pathname-type path))
       (buffer-name (format nil "~A.~A" filename type))
       )
    (when load-p
      (compile-file filename :load T))
    (when open-new-window-p
      (make-buffer-window buffer-name   :goto-buffer-p T
                          :if-exists-open-p T  :same-window-p T :temporary-p NIL ))
    (values new-buffer path)
    ))
;;TEST
;; (my-open-buffer "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-BASIC-functions.lisp")
;; works = #<EDITOR:BUFFER U-BASIC-functions.lisp>    #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-BASIC-functions.lisp"
;; (my-open-buffer "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp" :open-new-window-p T)
;; works=> creates new buffer from filename, then opens that buffer in a new window
;; (my-open-buffer "C:/3-TS/LISP PROJECTS TS/1-load-actr.lisp")


;;MAKE-BUFFER-WINDOW
;;2019
;;ddd
(defun make-buffer-window (buffer-name  &key  (goto-buffer-p T) 
                                        (if-exists-open-p T)
                                        (same-window-p t)
                                        temporary-p )
  "U-LW-editor makes a new buffer window.   RETURNS new-buffer 
   NOTE: Use my-open-buffer instead to open/eval existing files first.  "
  (let
      ((new-buffer (editor:make-buffer buffer-name :contents if-exists-open-p
                                       :temporary temporary-p ))
       )
    (when goto-buffer-p 
      (editor:goto-buffer new-buffer same-window-p))
    new-buffer
    ;;end let, make-buffer-window
    ))
;;TEST
;; (make-buffer-window  "buffer-test-1.lisp")
;; (make-buffer-window "new-buffer-EMPTY") 
;;WORKS=> makes a new buffer="new-buffer-EMPTY", opens it, makes primary.
;; (make-buffer-window "new-buffer-XX" :goto-buffer-p NIL)
;; works=> makes new buffer, but NOT open a window for it
;; (make-buffer-window "new-buffer-XX") 
;; works=> takes existing buffer (with contents) and opens it and makes it primary
;; (make-buffer-window  "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp")







;;*************** OLDER, WORKS, BUT NOT AS WELL AS OPEN-BUFFER(S) *********
;;MY-EDIT-FILES
;;2018
;;ddd
(defun my-edit-files (filenames &key dir filenames=paths-p return-found-paths-p
                                (file-exts '("lisp" "txt")))
  "U-LW-editor,USE OPEN-BUFFERS INSTEAD. Makes buffers for all files in paths. If dir then sets paths to all .lisp files in dir. When filenames=paths-p, saves converting to real paths. RETURNS (values  found-files n-files found-paths)  If RETURN-FOUND-PATHS-P, returns entire fpaths of found-files--otherwise 'Can return found-paths' FILE EXT MUST BE IN FILE-EXTS (unless it is NIL). Note: can't use in .LISPWORKS "
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
       (delete-buffers)
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
              delete-buffers (cdr buffers))))
    (loop
     for buffer in delete-buffers
     do
     (let
         ((p)
          )
       (declare (ignore p))
       (when buffer
         ;;         (setf (editor:current-buffer) buffer)
         (setf  cur-buffer (editor:current-buffer))
         ;;(editor:use-buffer buffer)
         ;; (setf (editor:current-window) (editor::find-window-for-buffer cur-buffer))
         (setf (editor:current-window) 
               (EDITOR::FIND-WINDOW-FOR-BUFFER BUFFER))
         ;;above causes error: Setting current window for the wrong process Background execute 1 : #<EDITOR::WM-WINDOW "motor-compilation.lisp" 2320B4E3>.

         ;;SSSSS DON'T USE DELETE-BUFFERS? DELETE ONE BUFFER AT A TIME WHEN THE NEW ONE IS CREATED??

         ;;IF RUN IN LISTENER, CAUSES ERROR: 
         ;; Error: Setting current window for the wrong process CAPI Execution Listener 1 : #<EDITOR::WM-WINDOW "p-star-cmd.lisp" 271F962B>.
         (setf cur-window (editor:current-window)
               windows (append windows (list cur-window)))
         ;;(break "cur-window=? cur-buffer")
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




;;DELETE-BUFFERS
;;2018
;;ddd
(defun delete-buffer-windows (buffers)
  "U-LW-editor, Deletes buffers in editor. BUFFERS can be strings or objects"
  (let*
      ((delete-buffers )
       (deleted-buffers)
       )
    (when (my-listp buffers)
      (loop
       for buffer in delete-buffers
       do
         (when buffer
           (when (stringp buffer)
             (setf buffer (editor:buffer-from-name buffer)))
           (editor::delete-buffer buffer)
           (setf deleted-buffers (append deleted-buffers (list buffer)))
           ;;end inner when
           )
         ;;end loop
         )
      ;;end when listp
      )
      ;;end let, delete-buffer
      deleted-buffers
      ))
;;TEST  -- NOT TESTED YET
;; (delete-buffers buffers)








;;FIND-DSPEC
;;
;;ddd
(defun  find-dspec  (dspec-sym)
   "In U-LW-editor, GOES to definition for dspec-sym in any buffer?
  RETURNS: (vales found point obj and buffer obj)"
   (editor:find-source-for-dspec-command t dspec-sym :same-window nil)
   )
;;TEST
;; (find-dspec 'get-set-append-delete-keyvalue)
;; returns: #<EDITOR::I-POINT "U-lists.lisp" 78750 offset 6592 2E2F8843>#<EDITOR:BUFFER U-lists.lisp>  ALSO: opens buffer and goes to that point



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




;;LIST-DEFAULT-FILE-OBJECTS
;;2020
;;ddd
(defun list-default-file-objects (file-prename  &key (pathroot "C:\\3-TS\\LISP PROJECTS TS\\")(dir "shaq")(ext ".lisp")  (default-objects '("defparameter" "defun" "defmacro" "defclass" "my-defclass"  "defvar" "setf" "set-sym&subsyms" "set"  "quote")) (rest-string-max-n 200)   return-object-list-string-p )
  "In U-lw-editor.lisp, creates a window and prints a list of object-type items in it, Uses READ-LINE. Use list-file-nested-lists for complex DB PList objects."
  (let*
      ((object-list-string (list-file-objects file-prename :ALL :pathroot :pathroot :dir :dir :ext ext   :default-objects default-objects :return-object-list-string-p return-object-list-string-p ))
       )
    (if return-object-list-string-p
        object-list-string)
  ))
;;TEST
;; (list-file-objects  "1 TOM-All-CSQ-DATA-2020-03-19" :ALL :dir "CogSysOutputs")
;; WORKS: opens new capi window listing all above objects



;;LFO
;;2020
;;ddd
(defun LFO (filename &key (object-type-list :ALL) (pathroot "C:\\3-TS\\LISP PROJECTS TS\\")(dir "CogSys-Model")(ext ".lisp")  (default-objects '("defparameter" "defun" "defmacro" "defclass" "my-defclass"  "defvar" "setf" "set-sym&subsyms" "set"  "quote")) (rest-string-max-n 200)   return-object-list-string-p )
  "U-LW-editor uses list-file-objects"
  (list-file-objects filename object-type-list  :pathroot pathroot :dir dir  :ext ext
  :default-objects default-objects     :rest-string-max-n rest-string-max-n 
  :return-object-list-string-p return-object-list-string-p )
  )
;; (lfo "U-LW-editor" :dir "MyUtilities")


;;LIST-FILE-OBJECTS 
;;
;;ddd
(defun list-file-objects (filename object-type-list &key (pathroot "C:\\3-TS\\LISP PROJECTS TS\\")(dir "CogSys-Model")(ext ".lisp")  (default-objects '("defparameter" "defun" "defmacro" "defclass" "my-defclass"  "defvar" "setf" "set-sym&subsyms" "set"  "quote")) (rest-string-max-n 200)   return-object-list-string-p )
  "In U-lw-editor.lisp, creates a window and prints a list of object-type items in it, 
 set object-type-list=> :ALL for all object-types.
 If (null pathroot), uses filename for ENTIRE pathname incl ext.
Uses READ-LINE. Use list-file-nested-lists for complex DB PList objects."
  (let*
      ((pathname (cond 
                  (pathroot (format nil "~A~A\\~A~A" pathroot dir filename ext))
                  (T filename)))
       (file-name (format nil "~A defclass file" pathroot))
       (object-list-file-inst (make-instance 'file-list-frame))
       (eof)
       (object-list-string) 
       )
    (when (equal object-type-list :ALL)
      (setf object-type-list default-objects))
    (setf object-list-string (format nil "In pathname: ~A~% OBJECT-LIST=> ~A~%" pathname  object-type-list))
    ;;object-type loop
    (loop
     for object-type in object-type-list
     do
     (let*
         ((object-string)
          )
     (with-open-file (s-input pathname :direction :input) 
       (setf object-string  (format nil "~A" object-type)
             object-list-string (format nil "~A~%~%>>>>>  OBJECT-TYPE: ~A  <<<<<~%" object-list-string object-string))
       (loop
        for n from 0 to 3000
        with line
        with token
        with rest-string
        do
        (let*
            ((str-end)
             (n-str)
             )
        (multiple-value-setq (line eof)
            (read-line s-input nil 'eof ))
        (cond
         (eof
          (return))
         (t (multiple-value-setq (token rest-string)
              (match-first-token object-string line  :delimiter-list '(#\(   #\space)))
                                                       ;;was '( "\(" " ")))
          ;;FOR SETTING MAX LINE LENGTH
          (setf n-str (length line))
          (cond
           ((or (null rest-string-max-n)
                (> rest-string-max-n n-str))
            (setf str-end nil))
           (T (setf str-end rest-string-max-n)))
          (setf line (subseq line 0 str-end))
                           
          (cond
           (token
            (setf object-list-string (format nil "~A~%~A" object-list-string line))
            )
           (t nil))
          ))
        ;;end let, inner loop, with-open-file (reopen on next loop)
        )))
     ;;end let,outer loop
     ))
    ;;display the file
    (capi:display object-list-file-inst)

    #|      (with-slots (answer-button-panel)  q-frame-inst
        ;;MAKE THE  BUTTON PANEL
      (setf answer-button-panel (make-radio-button-panel ans-instruction-text answer-array-list))
      ;;Put the BUTTON IN THE ALREADY CREATED FRAME
        (capi:apply-in-pane-process quest-rich-text-pane
                   #'(setf capi:rich-text-pane-text) question-text-formated quest-rich-text-pane |#
    ;;PRINT TO THE PANE
    (with-slots (text-pane-1) object-list-file-inst
      (capi:apply-in-pane-process  text-pane-1
                                   #'(setf capi:rich-text-pane-text)   object-list-string text-pane-1)  
      (setf (capi:interface-title  object-list-file-inst)
                                           (format nil ">> ~A CONTENTS"  filename))         
               
      ;;end let,with-open-file list-my-defclasses
      ;;end with-slots
      )
    (if return-object-list-string-p
        object-list-string)
    ;;end let, list-file-objects
    ))
;;TEST
;; (list-file-objects "SHAQ-new-scales" '("my-defclass"))
;; (list-file-objects "1-TOM-new-CSYMS-VALS2020-6-11-16h57."  :all :dir "CogSysOutputs") = works



;;LIST-ALL-FILE-OBJECTS
;;
;;ddd
(defun list-all-file-objects (filename-no-ext &key
                                              (pathroot  "C:\\3-TS\\LISP PROJECTS TS\\" ) 
                                              (dir "SHAQ")(ext ".lisp"))
  "In U-LW-editor, lists defparameter, defun, my-defclass, my-make-instance, and first-order nested-list keys"
  (list-file-objects filename-no-ext '( defparameter my-defclass my-make-instance defun) :pathroot  "C:\\3-TS\\LISP PROJECTS TS\\" :dir "SHAQ" :ext ".lisp")
 )
;;TEST 
;;  (list-all-file-objects "SHAQ-new-scales")
;; WORKS


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
(defun list-file-nested-lists (filename  &key object-sym  (pathroot "C:\\TOM\\LISP PROJECTS TS\\")(dir "shaq")(ext ".lisp") return-object-list-string-p add-defpar-newlines-p )
  "In U-lw-editor.lisp, creates a window and prints a list of DEFPARMETER items in it. Can list nested-list items horizontally or vertically using ADD-DEFPAR-NEWLINES-P. RETURNS the list. if return-object-list-string-p. If OBJECT-SYM, only finds keys for it. Uses READ."
  (let*
      ((pathname (format nil "~A~A\\~A~A" pathroot dir filename ext))
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
       do
       (let*
           ((object)
            (length-object)
            (object-1st)
            (object-2nd) 
            (defpar-list)
            (defpar-list-string)
            )
         #|        (multiple-value-setq (object eof)
            (read s-input  nil  'eof ))|#
         (setf object (read s-input  nil  'eof )) ;;returns 'eof if reaches end
         (if (listp object )(setf  length-object (list-length object)))
         ;;(setf out1 (format nil "object= ~A~%" object ))
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
         ;;end let,loop, with-open-file
         )))
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
                                   #'(setf capi:rich-text-pane-text)   object-list-string text-pane-1)
      (setf (capi:interface-title object-list-file-inst) 
                   (format nil ">> ~A CONTENTS" filename))
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





;; (list-all-file-objects "CogSys-Model\\CSQ-SHAQ-new-scales")


;;AUTO-EDIT-FILE-TEMPLATE
;;2020
;;ddd
(defun auto-edit-file-template (inpath outpath  &key line-match-string
                                       next-line-match-string  (if-exists :overwrite)
                                       (new-line-match-string ":scale-group-name") 
                                       file-only-lisp-objects-p 
                                       pprint-objects-p return-file-string-p (max-n 5000))
  "U-LW-editor,  TEMPLATE: Copy and modify this function to modify lines as required to do needed AUTO-EDITING. SAVE modified version with a NEW FILENAME.  RETURNS file-string (if  return-file-string-p)
 EXAMPLE: See AUTO-EDIT-NEW-SCALES-FILE function below [quoted off]."
  (let*
      ((file-positions)
       (file-string "")
       (edit-this-line-p)
       (edit-next-line-p)
       (insert-new-line-p)
       (matched-code1 "my-defclass")
       (matched-line)
       (classname)
       (children)
      )       
    (with-open-file (in inpath :direction :input)
      (with-open-file (out outpath :direction :output :if-exists if-exists 
                           :if-does-not-exist :create)
        (loop
         for n from 1 to max-n
         do
         (let*
             ((infile-position (file-position in))
              (outfile-position out)
              ;;to use (read in  NIL  'eof), file must ONLY contain lisp objects
              (object (when file-only-lisp-objects-p (read in  NIL  'eof)))
              (line-string (read-line in NIL "eof"))
              (new-line-string)
              (rest-matched-line1)       
              )
           ;;EDITING
           ;;MODIFY THIS CODE?
           (when (setf rest-matched-line1 (match-substring matched-code1 line-string))
             (setf classname (first (divide-string-to-all-tokens rest-matched-line1)))
             (when (classname-p classname)
                   (setf children (find-direct-subclass-names classname)))
             ;;(break)
             )
              (when edit-next-line-p
                ;;PUT EDITING CODE HERE
                )
               ;;insert new line?
               (when (and new-line-match-string
                          (match-substring  new-line-match-string line-string))
                 ;;PUT EDITING CODE HERE
                 (setf new-line-string  (format nil 
                                                "                   :subscales '~A" children)
                       insert-new-line-p T)
                 ;;(break "insert-new-line-p")
                 )
               (when (and next-line-match-string
                     (match-substring  next-line-match-string line-string))
                (setf edit-next-line-p T))
               ;;edit this line?
               (when (and line-match-string
                     (match-substring  line-match-string line-string))
                 ;;PUT EDITING CODE HERE
                (setf edit-this-line-p T))
               ;;(break "edit this or next line?")

              ;;WRITING STRINGS
              (when line-string
                (format out "~A~%"  line-string)
                (when return-file-string-p
                  (setf file-string (format nil "~S~%~S" file-string line-string))))
              (when insert-new-line-p
                (setf insert-new-line-p nil)
                (format out "~A~%"  new-line-string)
                ;;(break)
                (when return-file-string-p
                  (setf file-string (format nil "~S~%~S" file-string new-line-string))))       
               
               ;;FOR OBJECTS
               (when object
                 ;;EDIT OBJECTS HERE?
                 (cond
                  (pprint-objects-p
                  (pprint object out))
                 (T (write object :stream out))) ;;:right-margin T)))
                (when return-file-string-p
                  (setf file-string (format nil "~S~%~S" file-string object))))
               ;;eof?
               (when (or (equal line-string "eof")(equal object 'eof))
                 (return))
              ;;end let,loop
              ))
         ;;end with-, with
         ))
      (values file-string)
      ;;end let, auto-edit-new-scales-file
      ))
;;TEST -- WORKS
;;  (auto-edit-file-template "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\IN-FILE-NAME-HERE" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\OUT-FILE-NAME-HERE")




;;AUTO-EDIT-NEW-SCALES-FILE --WORKS
;;2020
;;ddd
;;THIS FUNCTION WORKS WELL--MODIFY OR USE AS EXAMPLE

(defun auto-edit-new-scales-file (inpath outpath  &key line-match-string
                                         next-line-match-string  (if-exists :overwrite)
                                          (new-line-match-string ":scale-group-name") 
                                         pprint-objects-p 
                                       return-file-string-p (max-n 5000))
  "U-LW-editor,  TEMPLATE: Copy and modify this function to modify lines as required to do needed AUTO-EDITING. SAVE modified version with a NEW FILENAME.  RETURNS file-string (if  return-file-string-p) "
  (let*
      ((file-positions)
       (file-string "")
       (edit-this-line-p)
       (edit-next-line-p)
       (insert-new-line-p)
       (matched-code1 "my-defclass")
       (matched-line)
       (classname)
       (children)
      )
     
      
    (with-open-file (in inpath :direction :input)
      (with-open-file (out outpath :direction :output :if-exists if-exists 
                           :if-does-not-exist :create)
        (loop
         for n from 1 to max-n
         do
         (let*
             ((infile-position (file-position in))
              (outfile-position out)
             ;; (object (read in  NIL  'eof))
              (line-string (read-line in NIL "eof"))
              (new-line-string)
              (rest-matched-line1)
              (object)              
              )
           (when (setf rest-matched-line1 (match-substring matched-code1 line-string))
             (setf classname (first (divide-string-to-all-tokens rest-matched-line1)))
             (when (classname-p classname)
                   (setf children (find-direct-subclass-names classname)))
             ;;(break)
             )

              ;;EDITING
              (when edit-next-line-p
                ;;PUT EDITING CODE HERE
                )
               ;;insert new line?
               (when (and new-line-match-string
                          (match-substring  new-line-match-string line-string))
                 ;;PUT EDITING CODE HERE
                 (setf new-line-string  (format nil 
                                                "                   :subscales '~A" children)
                       insert-new-line-p T)
                 ;;(break "insert-new-line-p")
                 )
               (when (and next-line-match-string
                     (match-substring  next-line-match-string line-string))
                (setf edit-next-line-p T))
               ;;edit this line?
               (when (and line-match-string
                     (match-substring  line-match-string line-string))
                 ;;PUT EDITING CODE HERE
                (setf edit-this-line-p T))
               ;;(break)

              ;;WRITING
              (when line-string
                (format out "~A~%"  line-string)
                (when return-file-string-p
                  (setf file-string (format nil "~S~%~S" file-string line-string))))
              (when insert-new-line-p
                (unless (match-substring  ":subscales" line-string)
                  (format nil "                  :subscales NIL"))
                ;;original (setf insert-new-line-p nil)
                ;;original (format out "~A~%"  new-line-string)
                ;;(break)
                (when return-file-string-p
                  (setf file-string (format nil "~S~%~S" file-string new-line-string))))       
               
               ;;not used (yet)?
               (when object
                 (cond
                  (pprint-objects-p
                  (pprint object out))
                 (T (write object :stream out))) ;;:right-margin T)))
                (when return-file-string-p
                  (setf file-string (format nil "~S~%~S" file-string object))))

               (when (equal line-string "eof")
                 (return))
              ;;end let,loop
              ))
         ;;end with-, with
         ))
      (values file-string    )
      ;;end let, auto-edit-new-scales-file
      ))
;;TEST
;; (auto-edit-new-scales-file "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\CSQ-SHAQ-new-scales.lisp" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\CSQ-SHAQ-new-scales-TEST.lisp")

;;  (auto-edit-new-scales-file "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\CSQ-SHAQ-new-scales.lisp" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\CSQ-SHAQ-new-scales-TEST.lisp")





           
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





;;GET-BUFFER-INFO
;;2019,20
;;ddd
(defun get-buffer-info (buffer)          
  "U-LW-editor,  RETURNS (values buffer-name buffer-pathname modified-p  filename filetype  buffer-path  buffer-obj cur-buf-point modified-tick). INPUT buffer can be a buffer name string or a buffer object."
     (let*
         ((buffer-obj)
          (cur-buf-point)
          (buffer-path )
          (buffer-pathname) ;;(namestring buffer-path))
          (buffer-filename) ;; (pathname-name buffer-path))
          (modified-tick)
          (modified-p)
          (buffer-name)
          (filename)
          (filetype)
          )
       (cond
        ((stringp buffer)
         (setf buffer-obj (editor:buffer-from-name buffer)
               buffer-name buffer))     
        (t 
         (setf buffer-obj buffer
               buffer-name (editor:buffer-name buffer))))
       
       (when buffer-obj
         (setf buffer-path (editor:buffer-pathname buffer-obj)
               cur-buf-point (editor:buffer-point buffer-obj)))
         
       (when buffer-path
         (setf buffer-pathname (namestring buffer-path)
               filename (pathname-name buffer-path)
               filetype (pathname-type buffer-path)
               modified-tick (editor::buffer-modified-tick  buffer-obj)
               modified-p (> modified-tick 1))
         ;;(break "inside get info")

         (values buffer-name buffer-pathname modified-p  filename filetype  buffer-path  buffer-obj cur-buf-point modified-tick)
         ;;end when buf path
         )
    ;;end let, get-buffer-info
  ))
;;TEST
;; (get-buffer-info "BUFFER-TEST-1.LISP")
;; (get-buffer-info "U-files.lisp")
;; works = "U-files.lisp"  "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-Files.lisp"  T  #<EDITOR:BUFFER U-Files.lisp>
;;;; (get-buffer-info "U-lists.lisp" :return-details-p T)
;; works = "U-files.lisp"   "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-Files.lisp"  T  #<EDITOR:BUFFER U-Files.lisp> "U-Files"  "lisp"  #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-Files.lisp"   2074

;;TEST ON BUFFER OBJECT--NOT STRING
;;;; (progn (multiple-value-bind ( buffer-filename buffer-pathname modified-p filename filetype  buffer-path buffer modified-tick) (get-buffer-info "U-lists.lisp" :return-details-p T) (get-buffer-info buffer :return-details-p T)))
;; WORKS= "U-lists.lisp"  "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"   T   "U-lists"  "lisp"  #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp"  #<EDITOR:BUFFER U-lists.lisp>  1001


;;SAVED vs UNSAVED; MODIFIED VS UNMODIFIED
;; NOT MODIFIED U-files = 1
;; MODIFIED SAVED & NOT-SAVED
;; ;; (get-buffer-info "U-files-backup.lisp") 
;; WHEN NOT SAVED =  4521, WHEN SAVED = 4521
;; (get-buffer-info "U-LW-editor.lisp")
;; MODIFIED, SAVED OR NOT = 1800




;;BUFFER-MODIFIED-P
;;2019
;;ddd
(defun buffer-modified-p (buffer)
  "U-LW-editor RETURNS (values modified-p modified-tick buffer-name buffer) INPUT: filename or buffer"
  (let*
      ((buffer-name (cond ((stringp buffer) buffer)
                          (t (editor:buffer-name buffer))))
       (modified-tick)  
       (modified-p) 
       )
    (when (stringp buffer)
      (setf buffer (editor:get-buffer buffer)))
    (setf modified-tick   (editor::buffer-modified-tick  buffer)
          modified-p (> modified-tick 1))
    (values modified-p modified-tick buffer-name buffer)
    ;;end let, buffer-modified-p
    ))
;;TEST
;; (buffer-modified-p "U-System.lisp")
;; result = T   1544   "U-System.lisp"  #<EDITOR:BUFFER U-System.lisp>
;; (buffer-modified-p "U-Arrays.lisp")
;; works = NIL  1  "U-Arrays.lisp"   #<EDITOR:BUFFER U-Arrays.lisp>



;;GET-BUFFERS-INFO
;;2019
;;ddd
(defun get-buffers-info (&key return-details-p
                              (return-modified-buffer-names-p T))
  "U-LW-editor,  RETURNS (values  buffer-filenames buffer-pathnames n-buffers modified-buffer-names buffer-paths buffers) if RETURN-DETAILS-P   for ALL BUFFERS IN EDITOR. Otherwise, returns (values  buffer-filenames buffer-pathnames n-buffers modified-buffer-names). If return-modified-buffer-names-p, return-modified-buffer-names is list."
  (let*
      ((buffer-objs editor:*buffer-list*)
       ;;doesn't give buffer names (buffer-names editor:*buffer-names*)
       (buffer-paths)
       (buffer-pathnames)
       (buffer-filenames)
       (n-buffers)
       (modified-buffer-names)
       )
    (loop
     for buffer-obj in buffer-objs
     do     
     (multiple-value-bind (buffer-filename buffer-pathname modified-p  filename filetype  buffer-path  buffer-obj cur-buf-point modified-tick)
         (get-buffer-info buffer-obj) 
       (when buffer-filename
         (setf buffer-pathnames (append buffer-pathnames (list buffer-pathname))
               buffer-filenames (append buffer-filenames (list buffer-filename))
               buffer-paths (append buffer-paths (list buffer-path))  
               buffer-objs (append buffer-objs (list buffer-obj)))
         (when (and modified-p return-modified-buffer-names-p)
           (setf modified-buffer-names (append modified-buffer-names 
                                               (list buffer-filename))))
         ;;end mvb,when,loop
         )))
    (setf n-buffers (list-length buffer-filenames))
    (cond
     ((null return-details-p)
      (values  buffer-filenames buffer-pathnames n-buffers modified-buffer-names))
     (t (values  buffer-filenames buffer-pathnames n-buffers modified-buffer-names buffer-paths buffer-objs)))
    ;;end let, get-buffers-info
    ))
;;TEST
;; (get-buffers-info :return-details-p T)
;; works= ("U-LW-editor.lisp" "U-files-backup.lisp" "1-CODE-TEMPLATES.lisp" "U-Files.lisp" "H-Editor.lisp" "U-System.lisp" "U-capi-input-interfaces.lisp" "0 MY-LW-INIT.lisp" "1-KEY BINDS ETC.lisp" "1-LW 7-1 NOTES.lisp" "U-fonts.lisp" "U-symbol-info.lisp" "U-function-plotter.lisp" "U-Arrays.lisp" "U-clos.lisp" "U-IDE-tools.lisp" "U-lists.lisp" "U-debug.lisp" "U-sequences.lisp" "U-tstring.lisp" "U-BASIC-functions.lisp")
#|  ("C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-files-backup.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-LW-editor.lisp" "C:\\3-TS\\LISP PROJECTS TS\\H-HELP\\H-Editor.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-System.lisp"....ETC...... "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-Files.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-BASIC-functions.lisp")
n-buffers= 21
modified-buffer-names= ("U-LW-editor.lisp" "U-files-backup.lisp" "U-System.lisp" "U-capi-input-interfaces.lisp")
(#P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-LW-editor.lisp" #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-files-backup.lisp" #P"C:/3-TS/LISP PROJECTS TS/1-CODE-TEMPLATES.lisp" #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-Files.lisp" ... ETC ...   #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-BASIC-functions.lisp")
(#<EDITOR:BUFFER U-LW-editor.lisp> #<EDITOR:BUFFER CAPI interactive-pane 26> #<EDITOR:BUFFER CAPI editor-pane 59> #<EDITOR:BUFFER CAPI editor-pane 17> #<EDITOR:BUFFER U-files-backup.lisp> #<EDITOR:BUFFER CAPI editor-pane 58> #<EDITOR:BUFFER CAPI interactive-pane 25> #<EDITOR:BUFFER CAPI interactive-pane 24> #<EDITOR:BUFFER CAPI interactive-pane 23> #<EDITOR:BUFFER CAPI editor-pane 53> #<EDITOR:BUFFER CAPI interactive-pane 21>  .... ETC ...  #<EDITOR:BUFFER CAPI editor-pane 11> #<EDITOR:BUFFER CAPI editor-pane 10> #<EDITOR:BUFFER CAPI editor-pane 9> #<EDITOR:BUFFER CAPI editor-pane #<EDITOR:BUFFER U-function-plotter.lisp> #<EDITOR:BUFFER U-Arrays.lisp> #<EDITOR:BUFFER U-clos.lisp> #<EDITOR:BUFFER U-IDE-tools.lisp> #<EDITOR:BUFFER U-lists.lisp> #<EDITOR:BUFFER U-debug.lisp> #<EDITOR:BUFFER U-sequences.lisp> #<EDITOR:BUFFER U-tstring.lisp> #<EDITOR:BUFFER U-BASIC-functions.lisp>) |#


;;********** EDITOR COMMANDS TO BUFFER FUNCTIONS *********


;;MY-GOTO-LINE --THIS FUNCTION DOESN'T WORK -- FIX???
;;2020
;;ddd
(defun my-goto-line (line-n buffer)
  "U-LW-editor, "
  (multiple-value-bind (buf-name buf-pathname modified-p buffer-obj cur-buf-point filename filetype  buffer-path  modified-tick)
      (get-buffer-info buffer)
    (break "THIS FUNCTION DOESN'T WORK")
    (my-set-current-buffer buffer-obj)

#|    (capi:apply-in-pane-process buffer-obj 'CAPI:CALL-EDITOR
                           "BEGINNING OF BUFFER" buffer-obj)|#

#|    (apply-in-pane-process buffer-obj 'CAPI:CALL-EDITOR
                           "NEXT LINE 30" buffer-obj)|#
    ;;SSSS DOESN'T WORK, BUT SHOULD??
    (capi:apply-in-pane-process buffer-obj 'CAPI:CALL-EDITOR
                           "GOTO LINE 100" buffer-obj)
    
      ;; (format nil "NEXT LINE ~A" line-n) buffer-obj)   

    ;;(editor:beginning-of-buffer-command nil)
    ;;(editor:next-line-command line-n)))
))
;;TEST
;; (my-goto-line 30 "BUFFER-TEST-1.LISP")

















;;********************************OLD FROM U-editor.lisp **********************
;;
;; Utilities to change editor behavior etc


;;***>>> MOST FUNCTIONS ARE IN U-LW-editor.lisp


(SETF CAPI::*PC-CURSOR-STYLES* 
      '(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR CAPI:*EDITOR-CURSOR-DRAG-STYLE* :LEFT-BAR CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* :RED))

(setf CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR
      CAPI:*EDITOR-CURSOR-COLOR* :RED)

#|
6.3 PROGRAMMING THE EDITOR   xxx
The editor functions described in this section can be combined and provided with arguments to CREATE NEW COMMANDS.

Existing editor commands can also be used in the creation of new commands.

xxx EVERY EDITOR COMMAND documented in this manual is NAMED BY A STRING COMMAND which CAN BE USED TO INVOKE THE COMMAND INTERACTIVELY, 
but there is also associated with this 
A STANDARD LISP FUNCTION (the "command function")
 named by a SYMBOL EXPORTED FROM THE EDITOR PACKAGE. 

*****>>>> You can use this symbol 
  TO CALL THE COMMAND PROGRAMMATICALLY. 
For example, the editor command "FORWARD CHARACTER"
              is referred to by EDITOR:FORWARD-CHARACTER-COMMAND.
****>>> The FIRST ARGUMENT OF ANY COMMAND FUNCTION IS THE PREFIX ARGUMENT P,
 and this MUST therefore be included in any programmatic call, even if the prefix argument is ignored. Some commands have additional optional arguments.  USE:  (declare (ignore p)   FUNCTION-CALL  ) for the p

For example to insert 42 #\! characters, you would call
(editor:self-insert-command 42 #\!)
Details of these optional arguments are provided in the command descriptions
throughout this manual.
See editor:defcommand for the details of how to create new commands.
NOTE: code which MODIFIES THE CONTENTS OF A CAPI:EDITOR-PANE (for example a displayed editor buffer) 
  MUST BE RUN ONLY IN THE INTERFACE PROCESS OF THAT PANE.
The following sections describe editor functions that are not interactive editor
commands.

6.3.1 CALLING EDITOR FUNCTIONS  xxx
All editor commands and some other editor functions expect to be called within a DYNAMIC CONTEXT that includes settings for the CURRENT BUFFER and CURRENT WINDOW. This happens automatically when using the editor interactively.

You can set up the context IN A CAPI APPLICATION by using the function
CAPI:CALL-EDITOR (see the CAPI User Guide and Reference Manual).

You can also use the following function TO CALL EDITOR COMMANDS AND FUNCTIONS.

EDITOR:PROCESS-CHARACTER FUNCTION  xxx

EDITOR:PROCESS-CHARACTER CHAR WINDOW
Processes char in a dynamic context where the current window is window and the current buffer is the buffer currently displayed in window.6 Advanced Features
214
The CHAR can be one of the following:
• A STRING, NAMING AN EDITOR COMMAND to invoke.
• A LIST OF THE FORM (FUNCTION . ARGS), which causes function to be called with args. The ITEMS IN ARGS ARE NOT EVALUATED.

• A FUNCTION OR SYMBOL, which is CALLED WITH NIL AS ITS ARGUMENT (LIKE A COMMAND FUNCTION WOULD BE IF THERE IS NO PREFIX ARGUMENT).

• A CHARACTER OR SYSTEM:GESTURE-SPEC OBJECT, which is treated as if it has been TYPED ON THE KEYBOARD.

THERE IS NO RETURN VALUE. The processing may happen in another thread, so may not have competed before this function returns.

6.3.2 DEFINING COMMANDS xxx

EDITOR:DEFCOMMAND MACRO

DEFCOMMAND NAME LAMBDA-LIST COMMAND-DOC FUNCTION-DOC &BODY FORMS => command-function
Defines a new EDITOR COMMAND. name is a USUALLY STRING naming the new editor command which can invoked in the editor via Extended Command,
and command-function is a symbol naming the new command function
which can be called programmatically. The command-function symbol is
interned in the current package.
lambda-list is the lambda list of the new command, which must have at
least one argument which is usually denoted p, the prefix argument.
command-doc and function-doc should be strings giving detailed and brief
descriptions of the new command respectively.
forms is the Lisp code for the command.
The name of the command must be a string, while the name of the associated command function must be a symbol. There are two ways in which
name can be supplied. Most simply, name is given as a string, and the string
is taken to be the name of the editor command. The symbol naming the
command function is computed from that string: spaces are replaced with
hyphens and alphabetic characters are uppercased, but otherwise the sym6.3 Programmin

6.3.3.2 BUFFER OPERATIONS
editor:*buffer-list* Variable
Contains a list of all the buffers in the editor.

EDITOR:CURRENT-BUFFER Function
editor:current-buffer
Returns the current buffer.
editor:buffer-name Function
EDITOR:BUFFER-NAME BUFFER
Returns the name of buffer.
editor:window-buffer Function
EDITOR:WINDOW-BUFFER WINDOW
Returns the buffer currently associated with window.
editor:buffers-start Function
EDITOR:BUFFERS-START BUFFER
Returns the starting point of buffer.
editor:buffers-end Function
EDITOR:BUFFERS-END BUFFER
6.3 Programming the editor221
Returns the end point of buffer.
editor:buffer-pointeditor:buffer-point buffer
Returns the current point in buffer.
editor:use-buffer Macro
EDITOR:USE-BUFFER BUFFER &BODY FORMS  [macro]
Makes buffer the current buffer DURING THE EVALUATION OF FORMS.
editor:buffer-from-name Function
EDITOR:BUFFER-FROM-NAME NAME
Returns the buffer called name (which should be a string). If there is no
buffer with that name, nil is returned.
editor:make-buffer Function
MAKE-BUFFER NAME &key modes contents temporary base-name name-pattern
Creates or returns an existing buffer. 
name should be a string or nil.
modes should be a list of strings naming modes. The first mode must be a
major mode, and the rest minor modes. The default value of modes is the
value of default-modes.
base-name should be a string or nil. If name and temporary are both nil
then base-name must be a string.
contents should be a string, nil or t (default value nil).
temporary is a boolean (default value nil).
name-pattern should be a string (default value "~a<~a>").
When name is non-nil, it is the name of the buffer. If there is already a
buffer with this name which is not temporary and the temporary argument6 Advanced Features  222
is nil, make-buffer returns that buffer. Before doing so, it sets its contents
to contents unless contents is t. When contents is nil, the buffer is made
empty.
If name is nil or temporary is non-nil or a buffer with the name cannot be
found, then a new buffer is made and returned. The buffer's contents is set
to contents if contents is a string, and otherwise the buffer is made empty.
The name of the buffer is set to name if name is non-nil.
If temporary is nil, the buffer is added to the internal tables of the editor. If
name is non-nil, it is used. Otherwise make-buffer tries to use base-name. If
there is already a buffer with this name, it constructs another name by
(format nil name-pattern base-name n)
with different integers n until it constructs an unused name, which it uses
as the buffer’s name.
If temporary is non-nil, the buffer is not added to the internal tables. It is
also marked as temporary, which mainly means that it does not have autosave and backup files, and avoids calling general hooks when it is modified.
Notes:
USING :TEMPORARY T gives you a buffer that is 'yours', that is the editor
does NOT DO ANYTHING WITH IT EXCEPT IN RESPONSE TO EXPLICIT CALLS FROM YOUR
CODE. Except when actually editing files, this is the most useful way of
using buffers in most cases.
capi:editor-pane with the :buffer :temp initarg uses
(make-buffer ... :temporary t)
editor:goto-buffer Function
EDITOR:GOTO-BUFFER BUFFER IN-SAME-WINDOW
Makes buffer the CURRENT BUFFER. If buffer is currently being shown in a window then the CURSOR is moved there. If buffer is not currently in a window and in-same-window is non-nil then it is shown in the current window, otherwise a new window is created for it|#

#|GO BACK Editor Command
Arguments: None
Key sequence: Ctrl+X C
Takes you back to the MOST RECENTLY RECORDED LOCATION. If a prefix argument
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

#|230
6.3.8 Files
editor:find-file-buffer Function
EDITOR:FIND-FILE-BUFFER pathname &optional check-function
Returns a buffer associated with the file pathname, reading the file into a
new buffer if necessary. The second value returned is T if a new buffer is
created, and nil otherwise. If the file already exists in a buffer, its consistency is first checked by means of check-function. If no value is supplied for
check-function, editor:check-disk-version-consistent is used.
editor:fast-save-all-buffers Function
editor:fast-save-all-buffers &optional ask
Saves all modified buffers which are associated with a file. If ask is non-nil
then confirmation is asked for before saving each buffer. If ask is not set, all
buffers are saved without further prompting.
Unlike the editor command Save All Files this function can be run
without any window interaction. It is thus suitable for use in code which
does not intend to allow the user to leave any buffers unsaved, and from
the console if it is necessary to save buffers without re-entering the full
window system.
editor:check-disk-version-consistent Function
editor:check-disk-version-consistent pathname buffer
Checks that the date of the file pathname is not more recent than the last
time buffer was saved. If pathname is more recent, the user is prompted on
how to proceed. Returns t if there is no need to read the file from disk and
nil if it should be read from disk.
editor:buffer-pathname Function
editor:buffer-pathname buffer6.3 Programming the editor
231
Returns the pathname of the file associated with buffer. If no file is associated with buffer, nil is returned
|#
#|
6.4 EDITOR SOURCE CODE -- page 197 eduser-manual
The section does not apply to LispWorks Personal Edition.
LispWorks comes with source code for the editor, which you can refer to when
adding editor extensions.
6.4.1 Contents
The directory lib/6-1-0-0/src/editor/ contains most of the source files of the LispWorks editor. Some low-level source code is not distributed.
6.4.2 Source location
To enable location of editor definitions by Find Source and related commands, configure LispWorks as described under "Finding source code" in the LispWorks User Guide and Reference Manual.
6.4.3 Guidelines for use of the editor source code
Some care is needed when working with the supplied editor source code, to ensure that you do not compromise the IDE or introduce a dependancy on a particular release of LispWorks.
In particular please note:
• The editor source code may not match the compiled code in the Lisp-
Works image exactly, for example if editor patches have been loaded.
• Modifications to the EDITOR package definition are not allowed.
6 Advanced Features
198
• Redefining existing definitions is not recommended. It is better to define a new command to do what you want. If you find a bug or have a useful extension to an existing definition then please let us know.
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






;;MY-BUFFER-PATHS
;;2020
;;ddd
(defun list/search-buffer-paths (&optional search-strings
                                      &key return-only-search-results-p)
  "U-LW-editor Uses  editor::get-active-buffer-paths.  RETURNS: (values buffer-paths found-buffers other-buffers) unless return-only-search-results-p."
  (let*
      ((buffer-paths (editor::get-active-buffer-paths))         
       )
    (multiple-value-bind (found-buffers other-buffers)
        (find-matched-strings-from-substrings search-strings buffer-paths)
    (cond
     (return-only-search-results-p
      (values  found-buffers other-buffers))
     (T  (values buffer-paths found-buffers other-buffers)))
    ;;end mvb,let, list/search-buffer-paths
    )))
;;TEST
;; (list/search-buffer-paths '("tom" "config") :return-only-search-results-p T)
;;works = ("C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\1 TOM-All-CSQ-DATA-2020-03-19.lisp" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\CS-config.lisp" "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\1 TOM-All-CSQ-DATA-2020-03-18.lisp" "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\1 2020-03 TOM SHAQ RESULTS-orig.lisp" ...ETC... "C:\\3-TS\\LISP PROJECTS TS\\CogSysOutputs\\Tom-AllData2017-01-copy.lisp")
;; ("C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-tstring.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-LW-editor.lisp" "C:\\3-TS\\LISP PROJECTS TS\\1-LW-INIT\\1-CODE-TEMPLATES.lisp" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\CSQ-value-rank-frame.lisp" "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\U-CS.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp" ...)





;;MY-CURRENT-BUFFER&POINT
;;2020
;;ddd
(defun my-current-buffer&point ()
  "U-LW-editor, RETURNS: (values buf-name cur-buf-pt cur-buf-obj)
   Only works in ?? contexts?"
  (declare (ignore p))
  (let*
      ((cur-buf-obj (editor:current-buffer))
       (buf-name (when cur-buf-obj (editor:buffer-name cur-buf-obj)))
       (cur-buf-pt (when cur-buf-obj  (editor:point-buffer cur-buf-obj)))
       )
    (values buf-name cur-buf-pt cur-buf-obj)
    ;;end let,my-current-buffer&point
    ))
;;TEST
;; (my-current-buffer&point)




(defun get-buf&pt-info (buf-name &key (buf-start-loc 0))
  "U-LW-editor, returns current buffer point"
  (declare (ignore p))
  (let*
      ((buf-obj (editor:buffer-from-name buf-name))
       ;;not work? (buf-lock (editor::buffer-lock buf-obj))
       (buf-pt (editor:buffer-point buf-obj))
       (buf-start) ;;no (setf (editor:buffer-start buf-obj) buf-start-loc))
       (buf-pt-sym) ;;no (editor:buffer-symbol-at-point buf-pt))
       (buf-end) ;;no (editor:buffer-end buf-obj))
       (mid-buf) ;;no (editor::points-to-buffer-string buf-pt buf-end))
  )
    (values   buf-obj buf-pt buf-start buf-end buf-pt-sym mid-buf) 
    ;;end let,get-buf&pt-info
    ))
;;TEST
;; (get-buf&pt-info "U-LW-editor.lisp")
;;works= #<EDITOR::I-POINT "U-LW-editor.lisp" 14000 offset 1845 256A288B>                 #<EDITOR:BUFFER U-LW-editor.lisp>
;; (get-buf&pt-info "BUFFER-TEST-1.lisp")


;; (editor:with-buffer-locked f **cur-buf (editor:buffer-from-name "U-LW-editor.lisp"))   (cur-buf&pt))


#|  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-symbol-info.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-capi.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-math.lisp")
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-clos.lisp")|#
#|Save Buffer Pathname Editor Command
;; (editor:save-buffer-pathname
Arguments: None
Key sequence: None
Pushes the namestring of the pathname of the current buffer onto the kill
ring. This namestring can then be inserted elsewhere by commands which
access the kill ring, described in “Inserting text” on page 70
|#
;;EDITOR COMMANDS
;; "MOVE TO WINDOW LINE" (beginning of line N)
;; "GO TO LINE"
;; "WHAT LINE"


;;MY-SEARCH-IN-BUFFER  -- one of BEST WORKING BUFFER FUNCS
;;2020
;;ddd
(defun my-search-in-buffer (match-string buffer  &key buf-obj buffer-name
                                       pathname ;;must be used for unopened buffers!!
                                       match-dspec (forwardp t)
                                       (prompt "Regular Expression:") 
                                       limit to-end  brackets-limits (highlight-dspec-p T)
                                       (goto-buffer-p T) (if-exists-open-p T)
                                       use-search-buffers-p (to-end-p T)
                                       same-window-p  temporary-p )
  "U-LW-editor WORKS WELL!  RETURNS (values search-result buf-obj start-pt-obj end-pt-obj new-temp-pt)    INPUT: buffer can be NAME or OBJECT 
  PATHNAME ;;must be used to open unopened buffers!!
  NOTE: MATCH-DSPEC (functions,etc). USE-SEARCH-BUFFERS-P opens search results window.
  THEREFORE: Use (defun 1MK ()) etc to MARK the locs.--works well!"
  ;;IF BUFFER FILE NOT OPEN, OPEN
  (setf buf-obj (editor:buffer-from-name buffer))
  ;;If buffer tnot open, must open
  (unless buf-obj
    (cond
     ((stringp buffer)
      (setf buffer-name buffer
       buf-obj (editor:make-buffer buffer)))
     (buffer-name
      (setf buf-obj (editor:make-buffer buffer-name)))
      (T (setf buf-obj buffer
               buffer-name (editor:buffer-name buf-obj)))))
  ;;if (null buf-obj), file not open in buffers? pathname must exist to open it.
  (unless buf-obj
    (my-edit-file pathname)
    (setf buffer-name (file-namestring pathname)
          buf-obj (editor:make-buffer buffer-name)))

  (let*
        ((start-pt-obj (editor:buffers-start buf-obj))
         (end-pt-obj (editor:buffers-end buf-obj))
        (buf-cur-dspec) ;; (editor:buffer-current-dspec buf-obj))
        (search-result)
        (dspec-result-pt)
        (goto-pt) 
        (p)
        )
    (declare (ignore p))
      ;;NOTE: editor:regular-expression-search MOVES CUR-POINT TO END OF MATCH STRING if found (or beginning if :to-end = NIL

      ;;MUST USE A TEMP PT OBJ OR RUINS BUFFER
      ;;editor acts as if buffer begins with the (moved) start-pt-obj, etc
      (EDITOR:WITH-POINT ((NEW-TEMP-PT start-pt-obj :temporary))
        (cond
         (match-string
          ;;CAREFUL RE MOVING  POINTS--can't navigate in buffer!!
          (cond
           (use-search-buffers-p  ;;HERENOW
              ;;error, p unbound, no matter what I tried.
            (EDITOR::SEARCH-BUFFERS-COMMAND p match-string ))
           ;;begin-or-end-found-exp 
           (T
            (setf search-result
                  (EDITOR:REGULAR-EXPRESSION-SEARCH
                   new-temp-pt match-string :to-end to-end-p))
            ;;(editor::do-simple-search match-string new-temp-pt :forward nil )
            )))
         (match-dspec
          (setf search-result
                (editor:find-dspec-in-buffer match-dspec buf-obj new-temp-pt)
                goto-pt dspec-result-pt)
          ;;(EDITOR::REMOVE-BUFFER-MARK buf-obj)
          ;;HIGHLIGHTS THE DSPEC IN BUFFER
          ;;(when highlight-dspec-p (editor:show-dspec-in-buffer buf-obj match-dspec T))
          )
         (T nil))
        ;;GOTO BUFFER?
        (when goto-buffer-p
          (EDITOR:GOTO-BUFFER-POINT buf-obj new-temp-pt  
                                    :in-same-window same-window-p))
        ;;(break)
        (values search-result buf-obj start-pt-obj end-pt-obj new-temp-pt)  
        ;;end with-pt, let, my-search-in-buffer
        )))
;;TEST
;;MATCH-STRING
;; (my-search-in-buffer "3MK" "BUFFER-TEST-1.LISP")
;;works:  Goes to pt after match-string, returns:
;; 3    #<EDITOR:BUFFER BUFFER-TEST-1.lisp>
#| #<EDITOR:POINT "BUFFER-TEST-1.lisp" 0 offset 0 32E94DD7>
#<EDITOR:POINT "BUFFER-TEST-1.lisp" 1000 offset 8121 32E94DB3>
#<EDITOR:POINT "BUFFER-TEST-1.lisp" 0 offset 3807 3362C4C3> |#

;; (my-search-in-buffer NIL "BUFFER-TEST-1.lisp" :match-dspec "3MK")
;; results: NIL etc; goes to buffer, but can't find  3MK bec not a defspec.
;;MATCH-DSPEC
;;(my-search-in-buffer nil "BUFFER-TEST-1.lisp" :match-dspec 'my-make-symbols)
;;works: goes to BEGINNING of match-dspec
;; returns:
#|  #<EDITOR:POINT "BUFFER-TEST-1.lisp" 0 offset 7135 3320717F>
#<EDITOR:BUFFER BUFFER-TEST-1.lisp>
#<EDITOR:POINT "BUFFER-TEST-1.lisp" 0 offset 0 32E94DD7>
#<EDITOR:POINT "BUFFER-TEST-1.lisp" 1000 offset 8121 32E94DB3>
#<EDITOR:POINT "BUFFER-TEST-1.lisp" 0 offset 7135 3320717F>  |#




;;EVAL-FORMS-WITH-BUFFER
;;2020
;;ddd
(defun eval-forms-with-buffer (buffer-name &rest forms)
  "U-LW-editor Makes buffer the current buffer DURING THE EVALUATION OF FORMS RETURNS: (values result buf-obj )"
  (let*
      ((buf-obj (editor:buffer-from-name buffer-name))
       (expr-to-eval (append (list 'editor:use-buffer buf-obj) forms))
       (result (eval expr-to-eval))
       )
    (editor:use-buffer buf-obj forms)
    (values result buf-obj )
    ;;end let,eval-forms-with-buffer
    ))
;;TEST
;; (eval-forms-with-buffer "unamed-10" '(setf this23 'that))
;; result=  THAT NIL
;; also: THIS23= THAT


;;MY-SET-CURRENT-BUFFER
;;2020
;;ddd
(defun my-set-current-buffer (buffer &key (goto-p T) in-same-window-p)
  "U-LW-editor buffer can be buffer NAME or OBJ. RETURNS: buf-object
 If goto-p, opens it, unless in-same-window-p, in dif window"
  (let*
      ((buf-obj (cond ((stringp buffer)(editor:buffer-from-name buffer))
                      (T buffer)))
       )                       
    (editor::set-current-buffer buf-obj)
    (when goto-p
      (setf buf-obj (editor:goto-buffer buf-obj in-same-window-p)))
    buf-obj
    ))
;;TEST
;; (my-set-current-buffer "U-tstring.lisp")
;; WORKS=#<EDITOR::WM-WINDOW "U-tstring.lisp" 2F4A86BB>

;; (editor:regular-expression-search nil "defun")



;;MY-FIND-BUFFER-LINE
;;2020  
;;ddd
;; SSSSS FIX--ERRORS
(defun my-find-buffer-line (line-n buffer  &key (markstr ";;MKLN") 
                                   find-end-p  (goto-buffer-point-p T) 
                                   (if-exists-open-p T) same-window-p  temporary-p )
  "U-LW-editor makes a new buffer window.   RETURNS new-buffer 
   NOTE: Use my-open-buffer instead to open/eval existing files first.  
   INPUT: buffer can be name or obj."
  (multiple-value-bind (buf-name buf-path modified-p  filename filetype  buffer-path  buf-obj cur-buf-point modified-tick)
      (get-buffer-info buffer)
    (let*
        ((point (editor:buffer-point buf-obj))
         (start-pt (editor:buffers-start buf-obj))
         ;;(new-point      )       ;;NO (set (editor:buffer-point buffer) 100))
         (end-pt (editor:buffers-end buf-obj))  
         (new-point)
         (p)
         )
      (declare (ignore p))
      (editor:with-point ((temp-pt start-pt  :temporary T))
        (cond
         (find-end-p  ;;THIS WORKS, MOVES POINT TO END
                      (setf temp-pt (editor:move-point temp-pt end-pt)))
         #|(T 
          (editor:move-point temp-pt (editor:goto-line-command line-n))
          ;;ABOVE should work, doesn't
            (setf  markstr (format nil "~A=~A" markstr line-n))
             ;;INSERT WORKS, BUT TEMP-PT STILL AT START
             (editor::insert-buffer-string temp-pt markstr) 
        ;;(break)
        )|#
        ;;OLD WAY--COULDN'T GET TO WORK
         (T     
          (loop
           for n from 1 to line-n
           do
;;           (editor:move-point temp-pt
            ;;(editor:move-point temp-pt (editor:next-line-command p))
            (editor:next-line-command p)
                    ;;(break)
           (when (= n line-n)
             (setf temp-pt (editor:buffer-point buf-obj))
             (setf  markstr (format nil "~A=~A" markstr line-n))
             ;;INSERT WORKS, BUT TEMP-PT STILL AT START
             (editor::insert-buffer-string temp-pt markstr) 
        ;;(break)
             (return))
           )))
        (when goto-buffer-point-p
          (my-goto-buffer-point buf-name temp-pt))

        ;;(setf line-n-obj (editor::find-line-in-buffer line-n  buffer point))
        ;;(editor:goto-line-command line-n-obj)
        (values temp-pt buf-name buf-obj start-pt end-pt buf-path)
        ;;end with-pt, let, mvb,my-find-buffer-line
        ))))
;;TEST
;; (my-find-buffer-line 20 "buffer-test-1.lisp") 
#|#<EDITOR:POINT "BUFFER-TEST-1.lisp" 0 offset 0 34566D27>
"buffer-test-1.lisp"
#<EDITOR:BUFFER BUFFER-TEST-1.lisp>
#<EDITOR:POINT "BUFFER-TEST-1.lisp" 0 offset 0 325F9D9B>
#<EDITOR:POINT "BUFFER-TEST-1.lisp" 1000 offset 8121 325F9DBF>
"C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\BUFFER-TEST-1.lisp"|#
;; moves to new buffer, but line-n not exactly righ
;; results= NIL  #<EDITOR:BUFFER U-lists.lisp>  #<EDITOR::I-POINT "U-lists.lisp" 0 offset 937 2198787B>  #<EDITOR:POINT "U-lists.lisp" 0 offset 776 2198780B>
;;(my-find-buffer-line 200 "U-lists.lisp")
#|#<EDITOR::I-POINT "U-lists.lisp" 32000 offset 5712 2198787B>
#<EDITOR::I-POINT "U-lists.lisp" 32000 offset 5712 2198787B>
#<EDITOR:BUFFER U-lists.lisp>
#<EDITOR:POINT "U-lists.lisp" 0 offset 776 2198780B>
#<EDITOR:POINT "U-lists.lisp" 118000 offset 8132 2198782F>|#


;;FROM LW 7.1
(editor:defcommand "Current Line" (p)
     "Computes the line number of the current point and
prints it in the Echo Area"
     "Prints the line number of the current point"
  (let* ((cpoint (editor:current-point))
         (svpoint (editor:copy-point cpoint))
         (count 0))
    (editor:beginning-of-buffer-command nil)
    (loop
     (if (editor:point> cpoint svpoint)
         (return))
     (unless (editor:next-line-command nil)
       (return))
     (incf count))
    (editor:move-point cpoint svpoint)
    (editor:message "Current Line Number: ~S " count)))

;;6.3.18.2 Example 2
;;This example creates a new editor command called Goto Line which moves the
;; current point to the specified line number.
;;GOTO LINE
(editor:defcommand "Goto Line" (p)
     "Moves the current point to a specified line number.
The number can either be supplied via the prefix
argument, or, if this is nil, it is prompted for."
     "Moves the current point to a specified line number."
  (let ((line-number
         (or p (editor:prompt-for-integer
                :prompt "Line number: "
                :help "Type in the number of the line to
go to"))))
    (editor:beginning-of-buffer-command nil)
    (editor:next-line-command line-number)))

;;Ex3
(defun copy-string (from-buf to-buf)
  (let ((string (editor:points-to-string
                 (editor:buffers-start from-buf)
                 (editor:buffers-end from-buf))))
    (editor:insert-string (editor:buffers-end to-buf) string)))
;;TEST
#|To test this example, two buffers named t1 and t2 should be created. Then, to
copy all the text from t1 to the end of t2:
(copy-string (editor:buffer-from-name "t1")
(editor:buffer-from-name "t2"))|#


;;As another example this command changes all the text in a writable buffer
;;to be uppercase:
(editor:defcommand "Uppercase Buffer" (p)
     "Uppercase the buffer contents" ""
  (declare (ignore p))
  (let* ((buffer (editor:current-buffer))
         (point (editor:buffer-point buffer))
         (start (editor:buffers-start buffer))
         (end (editor:buffers-end buffer)))
    (editor:set-current-mark start)
    (editor:move-point point end)
    (editor:uppercase-region-command nil)))

#|
EDITOR:USE-BUFFER Macro
editor:use-buffer buffer &body forms
Makes buffer the current buffer during the evaluation of forms.

EDITOR:BUFFER-FROM-NAME Function
editor:buffer-from-name name
Returns the buffer called name (which should be a string). If there is no
buffer with that name, nil is returned.
|#




;;NEW 2020-03  
;; Use of    CAPI:CALL-EDITOR to use predefined EDITOR COMMANDS

;;COPY-BUFFER-TO-WINDOW
;;2020
;;ddd
(defun copy-buffer-to-window (&optional buffer)
  "U-LW-editor Works only if type in buffer name.MODIFY "
  (let*
      ;;NOTE: This creates a separate window that works as an editor window
      ( (editor (capi:contain
                (make-instance 'capi:editor-pane
                               :text "MY MARKS w/ some text")))
       )
    ;;NOTE: CAPI:CALL-EDITOR is able to CALL BASIC EDITOR FUNCTIONS
    (cond
     ((null buffer)
     (capi:apply-in-pane-process  editor 'CAPI:CALL-EDITOR editor "Select Buffer" buffer))
     (T (setf buffer (capi:apply-in-pane-process  editor 'CAPI:CALL-EDITOR editor (format nil "Select Buffer Other Window")))))
                                                  ;;"Select Buffer Other Window" ))))
      (values buffer editor)
    ))
;;TEST
;; (copy-buffer-to-window "U-tstring.lisp")



;;SEARCH-FOR-BUFFER-INFO
;;2020
;;ddd
(defun search-for-buffer-info (buf-name-substr)
  "U-LW-editor   RETURNS (values  buf-name  buf-path  buf-obj buf-n)
 INPUT: Finds ANY (first) SUBSTRING of a buffer-name"
  (when (not (string-equal buf-name-substr ""))
  (let*
      ((found-buffer-n)
       (found-buffernames)
       (other)
       (found-ns)
       (buf-name)
       (buf-n)
       (buf-path)
       (buf-obj)
       )
    (multiple-value-bind ( buffer-filenames buffer-pathnames
                                            n-buffers modified-buffer-names buffer-paths buffers)
         (get-buffers-info :return-details-p T)
      (multiple-value-bind (found-buffernames other found-ns)
            (find-matched-strings-from-substrings (list buf-name-substr)
                                                  buffer-filenames)
        (cond
         (found-buffernames
          (setf buf-name (car found-buffernames)
                buf-n (car found-ns)
                buf-path (nth buf-n buffer-pathnames)
                buf-obj (nth buf-n buffers))
          )
         (T nil))
        ;;end when
        )
    (values  buf-name  buf-path  buf-obj buf-n)
    ;;end let, search-for-buffer-info
    ))))
;;TEST
;; (search-for-buffer-info "list")
;; works= "U-lists.lisp"   "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"  #<EDITOR:BUFFER U-files-backup.lisp>    12




(defun my-goto-buffer-line (line-n  buffer-name)
  "U-LW-editor Works only if type in buffer name.MODIFY "
  (let*
      ;;NOTE: This creates a separate window that works as an editor window
      ((buffer (make-buffer-window buffer-name :goto-buffer-p nil))

       #|(cur-buffer (cond ((null buffer) (editor:current-buffer))
                         (t (declare (ignore p) 
                                     (editor::goto-buffer-if-unflagged-current p buffer)))))|#
       (what-line1)
       (what-line2)
       (result)
       )
    ;;NOTE: CAPI:CALL-EDITOR is able to CALL BASIC EDITOR FUNCTIONS
    ;;SSSSSS START HERENOW  my-goto-buffer-line

    #|(capi:apply-in-pane-process  buffer 'CAPI:CALL-EDITOR                                              buffer "BEGINNING OF BUFFER")|#
                                              ;;"TOP OF WINDOW")
#|    (setf what-line1
          (capi:apply-in-pane-process  buffer 'CAPI:CALL-EDITOR
                                       buffer "TOP OF WINDOW"))|#
      ;;DOESN'T WORK, BUT SHOULD??
     (capi:apply-in-pane-process  buffer 'CAPI:CALL-EDITOR
                                           buffer (format nil "GOTO LINE ~A" line-n))
       ;;buffer "GOTO LINE 20")

#|     (capi:apply-in-pane-process  buffer 'CAPI:CALL-EDITOR
                                              buffer (format nil "GOTO LINE ~A" line-n))|#
                                                  ;;"Select Buffer Other Window" ))))
      (values buffer result what-line1)
    ))
;; (my-goto-buffer-line 10 "buffer-test-1.lisp")




;;EG FROM USE IN OTHER FILES:
#|(defun my-keybinds ()
  "U-BASIC-functions Lists all key binds in separate window"
  (let
      ;;NOTE: This creates a separate window that works as an editor window
      ((editor (capi:contain
                (make-instance 'capi:editor-pane
                               :text "MY-KEY-BINDS")))
       )     
    ;;NOTE: CAPI:CALL-EDITOR is able to CALL BASIC EDITOR FUNCTIONS
    (capi:apply-in-pane-process  editor 'CAPI:CALL-EDITOR editor "DESCRIBE BINDINGS")
    ))|#


;;USE CAPI:CALL-EDITOR TO CALL EDITOR FUNCTIONS PROGRAMATICALLY??
;;
;; FOLLOWING WORKS
(defun my-keybinds ()
  "U-BASIC-functions Lists all key binds in separate window"
  (let
      ;;NOTE: This creates a separate window that works as an editor window
      ((editor (capi:contain
                (make-instance 'capi:editor-pane
                               :text "MY-KEY-BINDS")))
       )     
    ;;NOTE: CAPI:CALL-EDITOR is able to CALL BASIC EDITOR FUNCTIONS
    (capi:apply-in-pane-process  editor 'CAPI:CALL-EDITOR editor "DESCRIBE BINDINGS")
    ))
;;TEST
;; (my-keybinds) 
;; creates 2 windows, one title is MY-KEY-BINDS and contains no text.
;; the other title is Help Interface NUM and contains the key binds; NUM starts with 1 and increases with each call of this function.
;; Different version in U-basic-commands creates interface, then closes it.




;;SSSSSS FIX--NOT WORKING
;;SEARCH-ALL-BUFFERS-OR-FILES
;;2020
;;ddd
(defun search-all-buffers-or-files (string &key (buffers-only-p T))
  "U-LW-editor"
  (let*
      ((result)
       )
    (cond
     (buffers-only-p
      (setf result (editor::search-all-buffers-command string)))
     (T
      (setf result (editor::search-all-files-command string))))
    result
    ))
;;TEST
;; (search-all-buffers-or-files "7MK")



;;================ FROM LW7.1 EDITOR SOURCE CODE =========
;;NOTE: ALL (in-package "EDITOR")

#|(defun buffer-count-new-lines (buffer)
  (declare (optimize (safety 0)))
  (let ((bigline (point-bigline (buffer-%start buffer)))
        (count 0))
    (loop (incf count (bigline-count-new-lines bigline))
          (unless (setq bigline (ed-bigline-next bigline))
            (return nil)))
    count))


(defun buffer-limit (buffer forward)
  (let ((limited (buffer-limited buffer)))
    (or  (and limited 
	     (if forward (buffer-end-edited buffer) (buffer-start-edited buffer)))
        (if forward (buffer-%end buffer) (buffer-%start buffer)))))
      
;; the following are external symbols, for the editor interface; the defstruct
;; slots all have %'s in them

(defun buffers-start (buffer)
  "Returns, as a point, the start of the buffer, which must be of type buffer."
  (check-is-buffer buffer)
  (buffer-%start buffer))

(defun buffers-end (buffer)
  "Returns, as a point, the end of the buffer, which must be of type buffer."
  (check-is-buffer buffer)
  (buffer-%end buffer))


(defun update-buffer-windows (buffer)
  (check-is-buffer buffer)
  (dolist (window *window-list*)
    (check-if-was-display window buffer t)  )
  (dolist (window (buffer-windows buffer))
    (update-buffer-window  window)))

(defun to-end-buffer-windows (buffer)
  (check-is-buffer buffer)
  (let ((point (buffer-point buffer)))
    (buffer-end point) 
    (dolist (window (buffer-windows buffer))
      (call-with-window-and-buffer-locked
       window nil 
       #'(lambda (window buffer point)
           (when (eq (window-buffer window) buffer)
             (check-if-was-display window buffer t)  
             (move-point (window-display-start window) 
                         point)) )
       point))))


(defun update-buffer-active-window (buffer)
  (let ((windows (buffer-windows buffer)))
    (when windows
      (update-buffer-window (car windows)))))|#




;;=========== SOME USEFUL POINT FUNCTIONS? [from symbol browser]
#|  NOTE: There are many more point commands, search "point" w/ s-browser.
EDITOR:POINT-POSITION
(EDITOR:COPY-I-POINT 
EDITOR:COPY-POINT 
EDITOR::COPY-POINT-TEMPORARY 
EDITOR::COPY-TEXT-PROPERTY-POINT 
EDITOR:CURRENT-POINT 
EDITOR:DELETE-BETWEEN-POINTS)
(EDITOR::GET-DEFUN-START-AND-END-POINTS 
**EDITOR:GET-SYMBOL-FROM-POINT 
EDITOR:GOTO-BUFFER-POINT 
**EDITOR:INSERT-FORM-AT-POINT 
EDITOR:MOVE-POINT 
EDITOR::MOVE-POINT-BUFFER
 EDITOR::POINT-AFTER
 EDITOR::POINT-BEFORE 
EDITOR:POINT-BUFFER 
EDITOR::POINT-EQUAL 
**EDITOR::POINT-HISTORY-LIST 
EDITOR::POINT-NEXT)
(EDITOR::POINTS-TO-BUFFER-STRING
** EDITOR:POINTS-TO-STRING 
EDITOR::PRINT-POINT 
EDITOR::READ-SYMBOL-FROM-POINT 
EDITOR::SHOW-WINDOW-POINTS)
(EDITOR::SYMBOL-STRING-AT-POINT 
EDITOR:WHERE-IS-POINT-COMMAND 
**EDITOR:WITH-POINT 
EDITOR:WITH-POINT-LOCKED)

|#
;;

;;xxx LW-BASED COMMANDS 
;; (
;;(editor:buffer-pathname buffer)
;; ;;(editor:buffer-pathname                    ;;no"U-LW-editor.lisp")
;;
;;(editor:current-buffer)
;;(editor:set-current-buffer "
;;(editor:goto-buffer "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-symbol-info.lisp")
;;(editor:goto-line (editor:current-buffer) 20) ;;not work
;;(editor:goto-buffer-point buffer point :in-same-window ?? :warp ??
;;(editor:buffer-point object)
;;(editor:buffer-point object)
;;EDITOR set BUFFER-POINT object value
;;EDITOR set POINT-BUFFER object value
;;(editor:buffer-mark
;;(editor:remove-buffer-mark
;;(editor:
;;(editor: