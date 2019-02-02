;; ******************************* U-Files.lisp ***********************************
;; *****>> SEE FILE END FOR FILE-NAMESTRING AND PATHNAME functions
;;

;;(FP 599 *cd *filenameXX)

(defparameter *files-out nil  "The text from U-files used in fout for debugging, etc")

;;USE THE FUNCTION (CALL-SYSTEM DOS-COMAND-STRING)  FOR MANY FILE OPERATIONS ETC  -- SEE MY HELP FILE H-SYSTEM.LISP

(setf *cdroot "C:\\TOM\\LISP PROJECTS\\lispworks projects\\"
      *cdbranch "LispworksUtilties\\"
      *cd (format nil "~a~a" *cdroot *cdbranch))
(setf *filenameXX "U-Files.lsp")
;; 
;;
;;MY-DOS-COMMAND
;;CALL-SYSTEM COMMAND &KEY CURRENT-DIRECTORY WAIT SHELL-TYPE 
;;works (my-dos-command 'copy "C:\\TOM\\Temp\\ArtsCulture.pdf" :to-path "c:\\temp") 
;;ddd
(defun my-dos-command  (dos-command  filename &key to-path )
  "Use copyfiles, ETC  "
  ;;note:  MUST HAVE EXTRA QUOTES ON FILENAMES OR DOS WON'T READ IT RIGHT
  (let
      ((command)
       (rest-arg (format nil "\"~A\"" filename))
       )
    (if to-path (setf rest-arg (format nil "~A  \"~A\"" rest-arg to-path)))
    (setf command  (format nil "~A ~A" dos-command rest-arg))
    (show-text command 30 t)
    (format t "~A" command)
    (system:call-system command))
  )


;;MY-DOS-COPY
;;(my-dos-copy "C:\\TOM\\Temp\\ArtsCulture.pdf" "c:\\temp")
;;ddd
(defun my-dos-copy (from-path to-dir)
  "U-files.lisp"
  ;;note:  must have extra quotes or dos won't read it right
  (let ((command (format nil "copy \"~A\" \"~A\"" from-path to-dir))
    )
    (format t "~A" command)
  (sys:call-system command)))


#|
;;DOS COMMANDS ----------------------------------------------------------------
ASSOC          Displays or modifies file extension associations.
ATTRIB         Displays or changes file attributes.
BREAK          Sets or clears extended CTRL+C checking.
BCDEDIT        Sets properties in boot database to control boot loading.
CACLS          Displays or modifies access control lists (ACLs) of files.
CALL           Calls one batch program from another.
CD             Displays the name of or changes the current directory.
CHCP           Displays or sets the active code page number.
CHDIR          Displays the name of or changes the current directory.
CHKDSK         Checks a disk and displays a status report.
CHKNTFS        Displays or modifies the checking of disk at boot time.
CLS            Clears the screen.
CMD            Starts a new instance of the Windows command interpreter.
COLOR          Sets the default console foreground and background colors.
COMP           Compares the contents of two files or sets of files.
COMPACT        Displays or alters the compression of files on NTFS partitions.
CONVERT        Converts FAT volumes to NTFS.  You cannot convert the
               current drive.
COPY           Copies one or more files to another location.
DATE           Displays or sets the date.
DEL            Deletes one or more files.
DIR            Displays a list of files and subdirectories in a directory.
DISKCOMP       Compares the contents of two floppy disks.
DISKCOPY       Copies the contents of one floppy disk to another.
DISKPART       Displays or configures Disk Partition properties.
DOSKEY         Edits command lines, recalls Windows commands, and
               creates macros.
DRIVERQUERY    Displays current device driver status and properties.
ECHO           Displays messages, or turns command echoing on or off.
ENDLOCAL       Ends localization of environment changes in a batch file.
ERASE          Deletes one or more files.
EXIT           Quits the CMD.EXE program (command interpreter).
FC             Compares two files or sets of files, and displays the
               differences between them.
FIND           Searches for a text string in a file or files.
FINDSTR        Searches for strings in files.
FOR            Runs a specified command for each file in a set of files.
FORMAT         Formats a disk for use with Windows.
FSUTIL         Displays or configures the file system properties.
FTYPE          Displays or modifies file types used in file extension
               associations.
GOTO           Directs the Windows command interpreter to a labeled line in
               a batch program.
GPRESULT       Displays Group Policy information for machine or user.
GRAFTABL       Enables Windows to display an extended character set in
               graphics mode.
HELP           Provides Help information for Windows commands.
ICACLS         Display, modify, backup, or restore ACLs for files and
               directories.
IF             Performs conditional processing in batch programs.
LABEL          Creates, changes, or deletes the volume label of a disk.
MD             Creates a directory.
MKDIR          Creates a directory.
MKLINK         Creates Symbolic Links and Hard Links
MODE           Configures a system device.
MORE           Displays output one screen at a time.
MOVE           Moves one or more files from one directory to another
               directory.
OPENFILES      Displays files opened by remote users for a file share.
PATH           Displays or sets a search path for executable files.
PAUSE          Suspends processing of a batch file and displays a message.
POPD           Restores the previous value of the current directory saved by
               PUSHD.
PRINT          Prints a text file.
PROMPT         Changes the Windows command prompt.
PUSHD          Saves the current directory then changes it.
RD             Removes a directory.
RECOVER        Recovers readable information from a bad or defective disk.
REM            Records comments (remarks) in batch files or CONFIG.SYS.
REN            Renames a file or files.
RENAME         Renames a file or files.
REPLACE        Replaces files.
RMDIR          Removes a directory.
ROBOCOPY       Advanced utility to copy files and directory trees
SET            Displays, sets, or removes Windows environment variables.
SETLOCAL       Begins localization of environment changes in a batch file.
SC             Displays or configures services (background processes).
SCHTASKS       Schedules commands and programs to run on a computer.
SHIFT          Shifts the position of replaceable parameters in batch files.
SHUTDOWN       Allows proper local or remote shutdown of machine.
SORT           Sorts input.
START          Starts a separate window to run a specified program or command.
SUBST          Associates a path with a drive letter.
SYSTEMINFO     Displays machine specific properties and configuration.
TASKLIST       Displays all currently running tasks including services.
TASKKILL       Kill or stop a running process or application.
TIME           Displays or sets the system time.
TITLE          Sets the window title for a CMD.EXE session.
TREE           Graphically displays the directory structure of a drive or
               path.
TYPE           Displays the contents of a text file.
VER            Displays the Windows version.
VERIFY         Tells Windows whether to verify that your files are written
               correctly to a disk.
VOL            Displays a disk volume label and serial number.
XCOPY          Copies files and directory trees.
WMIC           Displays WMI information inside interactive command shell.
;;---------------------------------------------------------
|#



#|
(defun my-save-file (file pathname)
  (let
      ((command (format nil "SAVE ~A" )
       (call-system 
 )))))

(defun my-open-file (filename)
  (let
      ((command (format nil "OPEN"
)))))
|#

(DEFUN FP1 (FILE-POSITION pathname)
  "In U-Files.lisp, finds file-postion and prints line point lies on."
  (LET ((PRINT-STRING "")
        (file-length 0)
        )
    (WITH-OPEN-FILE (IN-STREAM PATHNAME :DIRECTION :INPUT) 
      (setf file-length (file-length in-stream))
      (cond
       ((< file-position file-length)
        (file-position in-stream file-position)
        (setf print-string (read-line in-stream))
        (format t "FILE-POSITION= ~A LINE=> ~A" file-position print-string))
       (t (format t "FILE-POSITION GREATER THAN FILE-LENGTH= ~A" file-length)))
      )))

(defun FP (file-position directory filename)
  "In U-Files.lisp; uses FP"
  (let ((pathname (format nil "~A~A" directory filename))
        )
    (FP1 file-position pathname)))

;;(defparameter *plotter-directory "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master")
;;  (my-load-directory *plotter-directory 'load t)

(defun my-load-directory (dir operation &optional load-print-p file-list)
  "In U-Files.lisp, for all lisp files If operation= 'print-filename 'print-path 'compile-load, compiles and loads all. If file-list, then loads it instead of files found in directory.
    Returns source-files list"
  (let
      ((source-files)
       (source-pathname)
       (compile-pathname)
       )
    (cond
     (file-list
      (setf source-files file-list))
     (t (setf source-files  (directory (format nil "~A\\*.*" dir)))))
    (format t "Lisp files in directory= ~A~%" dir)
    (format t "source-files= ~A~%" source-files)
    (cond
     ;;print only
     ((member operation '(print print-path print-filename print-file))
      (dolist (file source-files)
        (cond
         ((member (pathname-type file) '( "lisp" "lsp" "cl") :test 'equal)
            (setf file (file-namestring file))
            )
           (t nil ))      
          (format t "\"~A\"~%"  file)
          ))
     ;;compile and load
     ((equal operation 'compile-load)
      (dolist (file source-files)
        (cond
         ((member (pathname-type file) '( "lisp" "lsp" "cl") :test 'equal)
          (setf source-pathname (format nil "~A\\~A" dir file))
          (compile-file source-pathname)
          (setf compile-pathname (compile-file-pathname source-pathname))
          (load compile-pathname :print load-print-p))
         ((equal (pathname-type source-pathname) "fas")
          (load source-pathname :print load-print-p))
         (t nil))
        ))
     ;;load
     ((equal operation 'load)
      (dolist (file source-files)
        (setf source-pathname (format nil "~A\\~A" dir file))
        (format t "1 load source-pathname= ~A~% pathname-type= ~A~%"
                source-pathname (pathname-type source-pathname))
        (cond
         ((member (pathname-type source-pathname) '( "lisp" "lsp" "cl") :test 'equal)
          (load source-pathname :print load-print-p)
          (format t "loading file= ~A~%" source-pathname))
         (t nil))
        ))
     )
    ;;return source-files list
    source-files
    ))

;;LIST-DIRECTORY-FILENAMES  returns values  filename-list and pathname-list
;;uses Seibles list-directory
;;(list-directory-filenames "c:\\")p186?
;;(list-directory-filenames "c:\\temp")
;;ddd
(defun list-directory-filenames (dirname)
  "In U-files.lisp, uses Seibel function. Returns values filename-list and pathname-list, but NOT subdirectories "
  (let ((file-list)
        (filename)
        (filename-list)
        (pathname)
        (pathname-list)
        )
    (setf file-list (list-directory dirname))
    (dolist (file file-list)
      (setf pathname (namestring file)
            filename (file-namestring file))
      (if filename (setf  filename-list (append  filename-list (list filename))))
      (if pathname (setf pathname-list (append pathname-list (list pathname))))
      )
  (format t "filename-list= ~A~% pathname-list= ~A~%" filename-list pathname-list)
  (values filename-list pathname-list)
  ))
  
;;WALK-DIRECTORY -- applies a function to all members of a directory that pass a test :test
;;
;;from PSeibel Practical Common Lisp p187
;;ddd   
(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
          ((directory-pathname-p name)
           (when (and directories (funcall test name))
             (funcall fn name))
           (dolist (x (list-directory name)) (walk x)))
          ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))



;;sss START TESTING HERE -- LATER FIX THE ASCEND SORT PROBLEM IN U-LISTS

#|(defun testos ()
  (let ((dir "C:\\TOM\\LISP PROJECTS\\MyProjects\\screensaver")
        )
    (organize-subdirs dir  'randomp 'dir)))    |#
;;all these work? 'greaterp 'dir)))  'lessp 'file))) 'lessp 'dir))) 'randomp 'file)))  'randomp 'dir)))

;;ddd
(defun organize-subdirs (main-dir sort-type by-dir-or-file)
  "In U-files, used by screensaver, sorts subdirs GIVEN FLAT OR TREE DIR, uses flatten-list-tree function in U-lists.lisp. Returns organized-dir-list flat-list simple-list"
  (let
      ((flat-subdirs-list)
       (simple-all-files-list)
       (simple-all-files-list2)
       (organized-file-list)
       (last-sub-files-list)
       (ufiles-out)
       (r)
       (a)
       (d)
       )
    ;;first flatten the main dir tree
    (multiple-value-setq (flat-subdirs-list last-sub-files-list)
        (make-flat-dirs-list main-dir))
    ;;(afout  '*ufiles-out (format nil "In organize-subdirs, flat-subdirs-list= ~A~%" flat-subdirs-list))   ;;400 t)

    ;;first, determine type of organiztion 
    (cond
     ((equal sort-type 'randomp)
      (setf r t))
     ((equal sort-type 'lessp)
      (setf d t))
     ((equal sort-type 'greaterp)
      (setf a t)))

    ;;then organize the flatten-list-tree by whatever means is specified by sort-type
    ;;if organize by dir
    (cond
     ;;To get a flat random all files list, first find the simple-all-files-list
     ((equal by-dir-or-file 'file)
      (multiple-value-setq (flat-subdirs-list2 simple-all-files-list)
          (flatten-list-tree flat-subdirs-list))
      (setq organized-file-list  ;;xxx added list to avoid error of not deep enough lists
            (list (organize-sublists simple-all-files-list :randomize  r  :descend-sort d  :ascend-sort a))))
     ;;organize by dir is the default
     (t   
      (setq organized-file-list 
            (organize-sublists flat-subdirs-list :randomize  r  :descend-sort d  :ascend-sort a))))
    ;;return 3 lists
    (afout '*ufiles-out (format nil  "organized-file-list= ~A~%~%" organized-file-list))
    (fout *ufiles-out)
    (values organized-file-list flat-subdirs-list simple-all-files-list)))


;;(testmfd)
;; Test  works
#|(defun testmfd ()
  (make-flat-dirs-list  "C:\\TOM\\LISP PROJECTS\\MyProjects\\screensaver")
  ;;was "c:\\temp")
  )|#


;;MAKE-FLAT-DIRS-LIST
;;
;;;;USE AS A MODEL FOR RECURSION???
;;works
;;ddd
(defun make-flat-dirs-list (dir)
  "In U-Files.lisp, will take dir tree with scattered files at all levels, separate files in each dir into own list for each dir-branch in the file-tree.  Returns flat list of all these dirs irrespective of their level in the dir tree.  If randomize-dirs, randomizes the dirs in overall list"
  (setf *ufiles-out nil
        n 0)
  (let
      ((dir-almost-flat-list)
       (simple-all-files-list)
       (dir-list)
       (sub-dir-list  (list-directory dir))
       (sub-dir-name-list)
       (sub-file-list)
       (file-list)
       )
    ;;make list of sub-dirs of files and completely flat list of all files
    (multiple-value-setq (dir-flat-list sub-dir-list )  ;;had dir-almost-flat-list here
        (make-flat-dirs-list1 sub-dir-list 0))

#|    (afout '*ufiles-out (format nil "In make-flat-dirs-list, FINAL, dir-flat-list= ~A~% sub-dir-list= ~A~% ~% simple-all-files-list= ~A~%"dir-flat-list sub-dir-list simple-all-files-list ))
    ;;now print out the output to a output-window 
    (fout *ufiles-out)|#
    (values dir-flat-list simple-all-files-list )))


;;works after much work 
;;USE AS A MODEL FOR RECURSION???
;;
;;ddd
(defun make-flat-dirs-list1 (dir-list N &optional dir-flat-list)
  "In U-files.lisp, works with make-flat-dirs-list as recursor- USE AS MODEL?"
  (let
      ((sub-file-list)
       (new-dir-list)
       (sub-dir-list)
       (return-sub-dir-list)
       )   
    (incf n)
   ;;(afout '*ufiles-out (format nil "0-IN MAKE-FLAT-DIRS-LIST1, dir-list ~A~%" dir-list))

    ;;for each element in the list -- may be file or dir or subdir
    (dolist (element dir-list)
      ;;keeps from appending extra first dir element files extra time
      (setf return-sub-dir-list nil
            new-dir-list nil)
      
#|      (afout '*ufiles-out
             (format nil "1-IN MAKE-FLAT-DIRS-LIST1,element= ~A~%~%dir-list= ~A~%~% sub-dir-list= ~A~%~%"  element dir-list sub-dir-list)) ;; dir-flat-list ))|#

      (cond
       ((null (setf new-dir-list (list-directory element)))
        (setf sub-dir-list (append sub-dir-list (list element)))
        )
       ;;cut off process if limit reached
       ((>  n *recursion-limit) nil)
       ;;tests to see if it is a directory or a file- returns file list if a dir
        
       ((setf new-dir-list (list-directory element))
        (multiple-value-setq (dir-flat-list return-sub-dir-list)
            (make-flat-dirs-list1 new-dir-list  n dir-flat-list))
        ;;moved from end to here to avoid append extra list at bottom of recursion
        ;;adding the unless clause here and at end stopped the extra list being appended at end
        (unless (equal return-sub-dir-list (car  (last dir-flat-list)))
          (if return-sub-dir-list (setf dir-flat-list (append dir-flat-list (list return-sub-dir-list)))))
        )
      
       (t nil))
      ;;end dolist
      )
    ;;append the dir-flat-list for this level (end of dolist when all elements processed)
    ;;prevent repeating append of last appended list
    (unless (equal sub-dir-list (car (last dir-flat-list)))
      (if sub-dir-list (setf dir-flat-list (append dir-flat-list (list sub-dir-list)))))
    ;;set it to nil??  sub-dir-list nil)
    #|(afout '*ufiles-out  (format nil "3-At END OF make-flat-dirs-list1 N= ~A~%~% dir-flat-list= ~A~%~% sub-dir-list= ~A~%~%return-sub-dir-list= ~a~%" N dir-flat-list sub-dir-list return-sub-dir-list) )|# 
    (values dir-flat-list sub-dir-list)))
    

;;this works, both below work right
#|(defun testsf ()
  (separate-files-from-dirs "c:\\temp"))
(defun testsf1 ()
    (separate-files-from-dirs "c:\\temp\\sub-temp"))|#
;;ddd
(defun separate-files-from-dirs (dir-or-dir-list)
  "In U-files.lisp, removes and returns (values files sub-dirs) from a dir"
;;not needed or use in cond below?  (setf dir (pathname-as-directory dir-or-dir-list))
  ;;  (show-text (format nil "In separate-files-from-dirs, dir-or-dir-list ~A~%" dir-or-dir-list) 20 t)
  (let
      ((dir-list) 
       (file-list)
       (file-name-list)
       (sub-dir-list)
       (sub-dir-name-list)
       )
    ;;create a dir-list of files and sub-dirs
    (cond
     ((listp dir-or-dir-list) 
      (setf dir-list dir-or-dir-list))
     (t (setf  dir-list (list-directory dir-or-dir-list))))
  ;;  (show-text (format nil "2-In separate-files-from-dirs, dir-list ~A~%" dir-list) 200 t)
    (dolist (element dir-list)
     ;; (show-text (format nil "element= ~A~%" element) 20 t)
      (cond
       ((file-pathname-p element)
        (setf file-list (append file-list (list element))
              file-name-list (append file-name-list (list (file-namestring element))))
      ;;  (show-text (format nil "file-name= ~A~%" element) 20 t)
        )       
       ((directory-pathname-p element)
        (setf sub-dir-list (append sub-dir-list (list element))))   
       (t nil)))

    (values file-list sub-dir-list file-name-list)  ;; sub-dir-name-list)
    ))
    
       


;;LIST-DIRECTORY
;;from PSeibel Practical Common Lisp 
;;(list-directory "C:\\") ;;works--lists pathnames not strings for FILES ONLY
;;(list-directory "C:\\TOM\\LISP PROJECTS\\MyProjects\\MyUtilties\\U-lists.lisp")==>
;; returns nil if it is a file and not a directory
;; (list-directory "C:\\TOM\LISP PROJECTS\\MyProjects") => NIL  DOESN[T WORK
;;ddd
(defun list-directory (dirname)
  "In U-files.lisp, Seibel advises to use this instead of DIRECTORY function"
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

;;ddd
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

;;ddd
;;(directory-pathname-p "c:\\temp\\test-photo1.jpg") => nil
;;(directory-pathname-p "c:\\temp") = nil
;;(directory-pathname-p "c:\\temp\\*.*") = nil

(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

  
;;(pathname-as-directory "C:\\TOM\\LISP PROJECTS\\MyProjects\\1 TEST OUTPUT.docx") => converts it to a real FILE path
;;(pathname-as-directory "C:\\TOM\\LISP PROJECTS\\MyProjects") 
;;   => CONVERTS STRING TO A REAL DIRECTORY PATH
#|
(defun test-pad ()
  (let ((pad (pathname-as-directory "C:\\TOM\LISP PROJECTS\\MyProjects"))
        )
    (show-text (format nil "pathname-as-directory= ~A~%" pad) 30 t)
    ))
|#
;;ddd
(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
      pathname)))




;;----------------------------- From Peter Seibel PCL Ch-15 ----------------------------
;;functions not duplicated or modified above
;;ppp
;;ddd
(defun directory-wildcard (dirname)
  (make-pathname 
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

;;(file-pathname-p "C:\\TOM\\LISP PROJECTS\\MyProjects\\1 TEST OUTPUT.docx")
;;ddd
(defun file-pathname-p (p)
  "In U-files.lisp, Returns a file path string name from either real path or string name"
  (unless (directory-pathname-p p) p))

;;(pathname-as-file "C:\\TOM\\LISP PROJECTS\\MyProjects\\1 TEST OUTPUT.docx")
;;CONVERTS A STRING INTO A REAL FILE PATH
;;ddd
(defun pathname-as-file (name)
  "In U-files.lisp, Return a pathname reperesenting the given pathname in `file form',
i.e. with the name elements in the name and type component. Can't
convert wild pathnames because of problems mapping wild directory
component into name and type components. Returns its argument if
it is already in file form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname 
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname)))


;;(file-exists-p "C:\\TOM\\LISP PROJECTS\\MyProjects\\screensaver\\screensaverMP.lisp")
;;(probe-file "C:\\TOM\\LISP PROJECTS\\MyProjects\\screensaver\\screensaverMP.lisp")  ;;also returns a real file path
(defun file-exists-p (pathname)
  "Returns a file-path. Similar to CL:PROBE-FILE except it always returns directory names in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."

  #+(or sbcl lispworks openmcl)
  ;; These implementations do "The Right Thing" as far as we are
  ;; concerned. They return a truename of the file or directory if it
  ;; exists and the truename of a directory is in directory normal
  ;; form.
  (probe-file pathname)

  #+(or allegro cmu)
  ;; These implementations accept the name of a directory in either
  ;; form and return the name in the form given. However the name of a
  ;; file must be given in file form. So we try first with a directory
  ;; name which will return NIL if either the file doesn't exist at
  ;; all or exists and is not a directory. Then we try with a file
  ;; form name.
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

#|  (or (ignore-errors
        ;; PROBE-FILE will return the truename if file exists and is a
        ;; file or NIL if it doesn't exist at all. If it exists but is
        ;; a directory PROBE-FILE will signal an error which we
        ;; ignore.
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        ;; PROBE-DIRECTORY returns T if the file exists and is a
        ;; directory or NIL if it doesn't exist at all. If it exists
        ;; but is a file, PROBE-DIRECTORY will signal an error.
        (let ((directory-form (pathname-as-directory pathname)))

   ;;what is package ext in ext:probe-directory? It creates compile error.
  ;;        (when (ext:probe-directory directory-form)
   ;;         directory-form))))
|#

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))

(defun directory-p (name)
  "Is `name' the name of an existing directory."
  (let ((truename (file-exists-p name)))
    (and truename (directory-pathname-p name))))

(defun file-p (name)
  "Is `name' the name of an existing file, i.e. not a directory."
  (let ((truename (file-exists-p name)))
    (and truename (file-pathname-p name))))



;;(in-package #:com.gigamonkeys.pathnames)
#|
(defun list-directory (dirname)
  "Return a list of the contents of the directory named by dirname.
Names of subdirectories will be returned in `directory normal
form'. Unlike CL:DIRECTORY, LIST-DIRECTORY does not accept
wildcard pathnames; `dirname' should simply be a pathname that
names a directory. It can be in either file or directory form."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))

  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    ;; SBCL, CMUCL, and Lispworks return subdirectories in directory
    ;; form just the way we want.
    (directory wildcard)
    
    #+openmcl
    ;; OpenMCl by default doesn't return subdirectories at all. But
    ;; when prodded to do so with the special argument :directories,
    ;; it returns them in directory form.
    (directory wildcard :directories t)
            
    #+allegro
    ;; Allegro normally return directories in file form but we can
    ;; change that with the :directories-are-files argument.
    (directory wildcard :directories-are-files nil)
            
    #+clisp
    ;; CLISP has a particularly idiosyncratic view of things. But we
    ;; can bludgeon even it into doing what we want.
    (nconc 
     ;; CLISP won't list files without an extension when :type is
     ;; wild so we make a special wildcard for it.
     (directory wildcard)
     ;; And CLISP doesn't consider subdirectories to match unless
     ;; there is a :wild in the directory component.
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))




(defun file-exists-p (pathname)
  "Similar to CL:PROBE-FILE except it always returns directory names
in `directory normal form'. Returns truename which will be in
`directory form' if file named is, in fact, a directory."

  #+(or sbcl lispworks openmcl)
  ;; These implementations do "The Right Thing" as far as we are
  ;; concerned. They return a truename of the file or directory if it
  ;; exists and the truename of a directory is in directory normal
  ;; form.
  (probe-file pathname)

  #+(or allegro cmu)
  ;; These implementations accept the name of a directory in either
  ;; form and return the name in the form given. However the name of a
  ;; file must be given in file form. So we try first with a directory
  ;; name which will return NIL if either the file doesn't exist at
  ;; all or exists and is not a directory. Then we try with a file
  ;; form name.
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  ;; Once again CLISP takes a particularly unforgiving approach,
  ;; signalling ERRORs at the slightest provocation.

  ;; pathname in file form and actually a file      -- (probe-file file)      ==> truename
  ;; pathname in file form and doesn't exist        -- (probe-file file)      ==> NIL
  ;; pathname in dir form and actually a directory  -- (probe-directory file) ==> truename
  ;; pathname in dir form and doesn't exist         -- (probe-directory file) ==> NIL

  ;; pathname in file form and actually a directory -- (probe-file file)      ==> ERROR
  ;; pathname in dir form and actually a file       -- (probe-directory file) ==> ERROR
  (or (ignore-errors
        ;; PROBE-FILE will return the truename if file exists and is a
        ;; file or NIL if it doesn't exist at all. If it exists but is
        ;; a directory PROBE-FILE will signal an error which we
        ;; ignore.
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        ;; PROBE-DIRECTORY returns T if the file exists and is a
        ;; directory or NIL if it doesn't exist at all. If it exists
        ;; but is a file, PROBE-DIRECTORY will signal an error.
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))
    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))

|#


#|
(defun directory-pathname-p (p)
  "Is the given pathname the name of a directory? This function can
usefully be used to test whether a name returned by LIST-DIRECTORIES
or passed to the function in WALK-DIRECTORY is the name of a directory
in the file system since they always return names in `directory normal
form'."
  (flet ((component-present-p (value)
           (and value (not (eql value :unspecific)))))
    (and 
     (not (component-present-p (pathname-name p)))
     (not (component-present-p (pathname-type p)))
     p)))
|#



#|
(defun pathname-as-directory (name)
  "Return a pathname reperesenting the given pathname in
`directory normal form', i.e. with all the name elements in the
directory component and NIL in the name and type components. Can
not be used on wild pathnames because there's not portable way to
convert wildcards in the name and type into a single directory
component. Returns its argument if name and type are both nil or
:unspecific."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname 
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))
|#



#|
;;(walk-directory "

;;(setf xdirx "C:\\TOM\\LISP PROJECTS\\MyProjects\\screensaver\\images")

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  "Walk a directory invoking `fn' on each pathname found. If `test' is
supplied fn is invoked only on pathnames for which `test' returns
true. If `directories' is t invokes `test' and `fn' on directory
pathnames as well."
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
|#


;;hhh ------------------------------------------ HELP -----------------------------------------------------------

#|
CALL-SYSTEM Function
Package system
Signature 
CALL-SYSTEM COMMAND &KEY CURRENT-DIRECTORY WAIT SHELL-TYPE =>
status
Arguments
 COMMAND   A string, a list of strings, a simple-vector of strings, or nil.
CURRENT-DIRECTORY  A string. Implemented only on Microsoft Windows.
WAIT A boolean.
SHELL-TYPE A string or nil.
|#




;; hhh ---------------------------------------------- HELP -------------------------------------------------------------

#|
(directory "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\*.lisp")
=> LISTS all files matching *.lisp in directory; output format is a PATH,
last in output list is ... #P"C:/TOM/LISP PROJECTS/lispworks projects/LispPlotter-master/fastregex.lisp") |#


;;----------------------------------- USING THE NAMESTRING FUNCTIONS ----------------------
;;(NAMESTRING  #P"C:/TOM/LISP PROJECTS/lispworks projects/LispCodeLibraries/DMcClain-vmath-master/matrix.lisp")
;;=>"C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispCodeLibraries\\DMcClain-vmath-master\\matrix.lisp" 
;;(FILE-NAMESTRING  "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp") => "plotter.lisp" 
;;(DIRECTORY-NAMESTRING  "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp")
;;note: no drive letter C: or filename in return
;;=> "\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\" 

;;(ENOUGH-NAMESTRING  "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp") => "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp" 
#|in all cases, and the result of enough-namestring is the shortest reasonable string that will satisfy this criterion.|#


;;;-------using the PATHNAME FUNCTIONS ------------------------
;;(PATHNAME-NAME  "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master")
;;   => "LispPlotter-master" 
;;(pathname-name "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp")  
;;=> "plotter" 

;;(PATHNAME-TYPE "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp")
;;=> "lisp" 
;;(PATHNAME-DIRECTORY "C:\\TOM\\LISP PROJECTS\\lispworks projects\\LispPlotter-master\\plotter.lisp")
;;=> (:ABSOLUTE "TOM" "LISP PROJECTS" "lispworks projects" "LispPlotter-master") 

#|
 (setq q (make-pathname :host "KATHY"
                        :directory "CHAPMAN" 
                        :name "LOGIN" :type "COM"))
=>  #P"KATHY::[CHAPMAN]LOGIN.COM"
 (PATHNAME-HOST q) =>  "KATHY"
 (PATHNAME-NAME q) =>  "LOGIN"
 (PATHNAME-TYPE q) =>  "COM"
|#

#|
(DIRECTORY "c:\\temp\\*.*"
 => (#P"c:/temp/test-photo1.jpg" #P"c:/temp/screensaverMP-problems notes.docx" #P"c:/temp/Backup of screensaverMP-problems notes.wbk" #P"c:/temp/ArtsCulture.pdf")
ARGUMENTS: 
(SYSTEM::PATHSPEC &REST SYSTEM::OPTIONS &KEY SYSTEM::TEST SYSTEM::DIRECTORIES SYSTEM::NON-ACCESSIBLE-AS-EMPTY SYSTEM::FLAT-FILE-NAMESTRING SYSTEM::RELATIVE-TO-COMMON-PATH (SYSTEM::LINK-TRANSPARENCY SYSTEM:*DIRECTORY-LINK-TRANSPARENCY*) SYSTEM::NON-EXISTENT-LINK-DESTINATIONS)
|#

#|(append '(a b c) (list '(d e f))) => (A B C (D E F)) 
(append (list  '(a b c)) (list '(d e f))) => ((A B C) (D E F)) 
(cdr '(a b c d)) = (B C D) 
(cadr  '(a b c d)) = b
(cadr '((a b c d))) = nil
(cdr '((a b c d))) = nil|#