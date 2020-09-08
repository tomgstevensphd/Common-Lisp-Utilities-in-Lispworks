;;************************* U-files-backup.lisp **********************
;;
;;done in .LISPWORKS (compile-file "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-system.lisp" :load T)
#|
(editor:buffer-flag
 (editor:buffer-alive-p
  (editor:buffer-modification-count
   (editor:buffer-pathname

  ;;use?
    (editor:buffer-modified
     (editor:buffer-dspecs
  ;;use?
      (editor:buffer-name
;;use?
       (editor:list-buffers-command p)

(editor:*buffer-list*)
(editor:save-file-command

;;USE THIS TO WRITE FILE
(editor:write-file-command p &optional pathname buffer`

;;use for each drive a-h to check for making drive list panel
;; (my-call-system "dir c:\\" )
;; (my-call-system "dir e:\\") = works


(make-choice-list-interface-instance :title "New Title" :title-bkgr :green :info-align :center :info-font-color :blue :choice-items (list "A" "Boy" "C" 1 2 3 4) :choice-type :MULTIPLE-SELECTION :confirm-input-p T :go-button-args (list :visible-min-width 300) )
|#



;;SAVE-BUFFERS-TO-DRIVES
;;2019
;;ddd
(defun save-buffers-to-drives (&key (save-to-paths-p T)   save-all-files-p  
                                      (sel-drives) omit-popup-p checked-dirs
                                      (default-dir "0 LW-CUM-dated")(filetypes '("lisp"))
                                      (not-filetypes '(".lisp~"))
                                      (datestr-begin 3)(datestr-end 11)                                      
                                      (to-paths '("C:\\3-TS" "E:\\Dropbox")) (slashstr "\\")
                                      (drive-list  '( "F:" "G:" "H:"))
                                      return-details-p (sleep-betw-copy 0.1) ;;was 0.5
                                      (use-dos-copy-p T))
  "U-files-backup   RETURNS (values copied-paths failed-to-copy-paths modified-buffers modified-filenames newbufferpaths drive-savepaths) if RETURN-DETAILS-P. Note: file datestr begins w/ yr,digit 4, ends with min, 10s digit. MUST USE slashstr "\\" for my-dos-copy. SEL-DRIVES=If = :ALL or omit-popup-p, saves to all dirs."
  (let*
      ((p)
       (buffers editor:*buffer-list*)
       (buffer-filenames)
       (buffer-pathnames) ;; (editor::get-active-buffer-paths))
       (modified-buffers)
       (unmodified-buffers)
       (modified-filenames)
       (datestr (nth-value 9 (my-get-date-time :reverse-year-p T  :use-zeros-p T
                                               :return-unseparated-date-str-p T )))
       ;;"201902260847"
       (filedate (when (stringp datestr)
                   (format nil "~A" (subseq datestr datestr-begin datestr-end))))
       (selected-drive-labels)
       (modified-paths)
       (newbufferpaths)
       (orig-buffer-pathnames)
       (drive-savepaths (cond (save-to-paths-p to-paths)(t nil)))
       (copied-paths)
       (failed-to-copy-paths)
       (n-drives)
       (voldrivestrs to-paths)
       (drivestrs to-paths)
       (drive-names to-paths)
      )
    ;;    
    ;;MAKE LIST OF BUFFERS TO SAVE OR NOT
    (loop
     for buffer in buffers
     do
     (let*
       ((newfilename)
        (newbufferpath)
        (subdir-1 "")
        ;;(filetype1)
          )
       (multiple-value-bind (buffer-filename buffer-pathname 
                      MODIFIED-P filename filetype  buffer-path buffer1 cur-buf-point
                      modified-tick)
           (get-buffer-info buffer) 

       ;;Apply new filenames to modified buffers only, append lists of modified & unmod
       (cond
        ((and buffer-pathname (or modified-p save-all-files-p))
         (setf filetype (pathname-type buffer-pathname)) ;;was filetype1
         (when (or
                (and filetypes (member filetype filetypes :test 'string-equal))
                (and not-filetypes (not (member filetype not-filetypes :test 'string-equal)))
                filetype) ;;any filetype
         (setf newfilename (format nil "~A-~A.~A" filename filedate  filetype)
               modified-buffers (append modified-buffers (list buffer))
               modified-filenames (append modified-filenames (list newfilename))
               orig-buffer-pathnames (append orig-buffer-pathnames (list buffer-pathname)))
         ))
        (t 
         (setf unmodified-buffers (append unmodified-buffers (list buffer)))))

       ;;MAKE THE NEW PATH with original subdirs and new filename
       (when  newfilename
         (multiple-value-bind (newpath real-path subdirs) 
             (make-path-from-subdirs :dir-path buffer-pathname)
           ;;no, below  :filename newfilename  :add-filename-p T)))

           (when (car (last subdirs))
             (setf subdir-1 (car (last subdirs))))
           (setf newbufferpath 
                 (format nil "~A~A~A~A" slashstr subdir-1 slashstr newfilename))
           (when newbufferpath
             (setf newbufferpaths (append newbufferpaths (list newbufferpath))))
           ;;end mvb,when
           ))
     ;;end mvb,let,loop
     )))
    ;;(break "after buffer loop")

    ;;SAVE THE BUFFERS TO SELECTED DRIVES
    ;;FOR THE UNKNOWN DRIVES
    ;;1-FIND DRIVE NAMES, ETC
    (multiple-value-bind (drive-names1 voldrivestrs1 volinfos drivestrs1)
        (find-drives-names drive-list)
      ;;HERENOW
    ;;append to-paths to found drives
    (setf voldrivestrs (append voldrivestrs voldrivestrs1)
          drivestrs (append drivestrs drivestrs1)
          drive-names (append drive-names drive-names1))                            

    ;;WHICH DIRS TO PRE-SELECT?
    (cond
     ((or (equal sel-drives :all)  omit-popup-p)
      (setf n-drives (list-length drive-names)
            sel-drives (make-list-n-long n-drives '(1 2 3 4 5 6 7 8))))
     (sel-drives NIL)      
     (t (setf sel-drives '(0))))

    ;; 2-CHOOSE THE DRIVES
    (setf *choice-list-interface-data nil)

    ;;(break "voldrivestrs") ;; here1

    ;;MAKE POPUP MULTI CHOICE LIST (unless omit-popup-p)
    (unless omit-popup-p
      (make-choice-list-interface-instance
       :sel-items sel-drives
       :title "SAVE BUFFERS TO SELECTED DRIVES"
       :info-text (format nil "Select (or unselect) all backup drives to write the following buffers to: ~%~A" modified-filenames)
       :title-bkgr :green :info-align :center :info-font-color :blue
       :choice-items voldrivestrs
       :choice-type :MULTIPLE-SELECTION :confirm-input-p NIL
       :window-args 
       (list :title 
             (format nil "SELECT THE DRIVES to save files FROM DIR: ~A" default-dir)
             :visible-min-width 400 :visible-min-height 340
             :internal-border 10
             :background #(:rgb 0.3882353 0.94509805 0.6392157 1.0))
       :go-button-args (list :visible-min-width 300))
    (mp:current-process-pause 30)
      ;;end unless
      )
    ;;GET SELECTED DRIVES
    (cond
     (omit-popup-p
      (setf selected-drive-labels  voldrivestrs))
     (t (setf selected-drive-labels  *choice-list-interface-data)))
     ;;(break "selected-drive-labels")
    
    ;; 2-SAVE THE FILES TO ALL DRIVES SELECTED 
    (loop
     for drive-label in selected-drive-labels
     do
     (multiple-value-bind (drivestr voldrivestr nth-item)
         (get-list2-item-from-list1-matched-Nth-item  drive-label voldrivestrs drive-names )
       (let*
           ((vol-label )
            (label (format nil "~A~A~A"  drivestr slashstr vol-label))
            (drive-savepath (format nil "~A~A~A" drivestr slashstr default-dir))
            )
         (setf drive-savepaths (append drive-savepaths (list drive-savepath)))

         ;;(break "drive-savepath")

         ;;  GET INFO FROM EACH BUFFER TO CREATE SAVEPATS
        (loop
          for buffer in modified-buffers
          for newbufferpath in newbufferpaths
          for orig-buffer-pathname in orig-buffer-pathnames
          do
          (let*
              ((savepath (format nil "~A~A" drive-savepath newbufferpath))
               (savepath-dir (my-directory-namestring savepath :incl-host-p T))
               )
            ;;ENSURE DIR EXISTS
            (multiple-value-bind (dir-exists filename filetype pathobject new-dir)
                (my-ensure-dir-exists savepath-dir :chk-is-filepath-p T)
              ;;older  (my-ensure-dir-exists savepath-dir :checked-dirs checked-dirs                                        :return-filename-p T :chk-host-exist-p T :slashstr slashstr )
            ;;(break "drive-label")
            ;;USE MY-DOS-COPY MOST OF TIME
            (cond
             (use-dos-copy-p
              (setf drive-savepaths (append drive-savepaths 
                                            (list (list savepath :from orig-buffer-pathname) )))
              ;;get original file (hopefully up-to-date) contents
              (multiple-value-bind (dostext dosresult copied-p)
                  (my-dos-copy orig-buffer-pathname savepath)
                (cond
                 (copied-p
                  (setf copied-paths (append copied-paths (list savepath))))
                 (t
                  (setf failed-to-copy-paths (append failed-to-copy-paths (list savepath)))))
                ;;(break "end of my-dos-copy")
                (sleep sleep-betw-copy)
                ))
             ;;OR  USE WITH-OPEN AND USE BUFFER CONTENTS
             ;;PROBLEMS: 1-changes buffer names (adds datestr). 2-problems with denied access in dropbox, etc.
             (T
              (setf drive-savepaths (append drive-savepaths (list savepath)))              
              (with-open-file (out (pathname savepath)
                                   :if-exists :overwrite  :if-does-not-exist :create
                                   :direction :output)
                ;;not find file alone (write '("This is a test") :stream out)
                (editor:write-file-command p out buffer)
                ;;end with- t, cond
                )))
           ;;end mvb, inner let, loop
            )))
         ;;end let, mvb,outer loop
         )))
    (cond 
     ((null return-details-p)
      (values copied-paths failed-to-copy-paths modified-buffers))
      (t   (values copied-paths failed-to-copy-paths modified-buffers modified-filenames newbufferpaths drive-savepaths)))
    ;;end mvb, let, save-buffers-to-drives
    )))
;;TEST
;; (save-buffers-to-drives  :drive-list '("e:") :to-paths '("C:\\Dropbox-ADD"))
;; (save-buffers-to-drives    :omit-popup-p T)
;; (save-buffers-to-drives  :drive-list '("e:") :to-paths NIL)


;;SAVBUFS
;;2019
;;ddd
(defun savbufs (&optional (omit-popup-p T) &key (to-dropbox-p T) 
                          (drive-list  '("F:" "G:" "E:" "H:")))
  (save-buffers-to-drives :save-to-paths-p to-dropbox-p :drive-list drive-list 
                          :omit-popup-p  omit-popup-p)
  ;;end savbufs
  )
;;saved?
;;  (savbufs)
;;WORKS:
;;copied-paths= ("F:\\1-LW-CUM\\MyUtilities\\U-files-backup-90308152.lisp" "F:\\1-LW-CUM\\MyUtilities\\U-Files-90308152.lisp" "F:\\1-LW-CUM\\CogSys-Model\\U-CS-90308152.lisp" "F:\\1-LW-CUM\\CogSys-Model\\CS-notes-90308152.lisp" "F:\\1-LW-CUM\\1-LW-INIT\\0 MY-LW-INIT-90308152.lisp" "G:\\1-LW-CUM\\MyUtilities\\U-files-backup-90308152.lisp" "G:\\1-LW-CUM\\MyUtilities\\U-Files-90308152.lisp" "G:\\1-LW-CUM\\CogSys-Model\\U-CS-90308152.lisp" "G:\\1-LW-CUM\\CogSys-Model\\CS-notes-90308152.lisp" "G:\\1-LW-CUM\\1-LW-INIT\\0 MY-LW-INIT-90308152.lisp")
;;failed-to-copy-paths = [NO E DRIVE CONNECTED] ("E:\\1-LW-CUM\\MyUtilities\\U-files-backup-90308152.lisp" "E:\\1-LW-CUM\\MyUtilities\\U-Files-90308152.lisp" "E:\\1-LW-CUM\\CogSys-Model\\U-CS-90308152.lisp" "E:\\1-LW-CUM\\CogSys-Model\\CS-notes-90308152.lisp" "E:\\1-LW-CUM\\1-LW-INIT\\0 MY-LW-INIT-90308152.lisp")
;;modified-buffers= (#<EDITOR:BUFFER U-files-backup.lisp> #<EDITOR:BUFFER U-Files.lisp> #<EDITOR:BUFFER U-CS.lisp> #<EDITOR:BUFFER CS-notes.lisp> #<EDITOR:BUFFER 0 MY-LW-INIT.lisp>)

;; (with-open-file (out1 " F:\\1-LW-CUM\\MyUtilities\\test-doc123.txt" :if-exists :overwrite :if-does-not-exist :create  :direction :output) (write '("This is a test") :stream out1))
;; (with-open-file (out " C:\\DROPBOX\\1-LW-CUM\\MyUtilities\\test-doc.txt" :if-exists :overwrite :if-does-not-exist :create  :direction :output) (write "This is a test" :stream out))
;; (with-open-file (out1  " C:\\temp\\test-doc123.txt" :if-exists :overwrite :if-does-not-exist :create  :direction :output) (write '("This is a test") :stream out1))


;; (with-open-file (IN " F:\\1-LW-CUM\\MyUtilities\\test-doc.txt"  :direction :INPUT) (setf xxxx1 (READ-LINE :stream IN)))
;;C:\Temp

;; (setf testwobj '(list '(first (a b c (1 2 3 (4 5 6 (7 8 9)(10 11 12) h i j) k l) next-last) last)))
;;(WRITE-TO-FILE   "f:/1-LW-CUM/MyUtilities/my-write-test2.lisp" `(this (setf testwobj ,(eval testwobj))) :pretty T)
;;"This is a test"
;; (WRITE-TO-FILE   "f:/1-LW-CUM/MyUtilities/my-write-test123.lisp" `'("This is a test") :pretty nil)


;;weird sometimes MY-DOS-COPY not work  ? ONLY \\ SLASHES WORK??
              ;;works  (my-dos-copy "C:\\3-TS\\Temp\\YAHOO spam.txt" "c:/dropbox-ADD/temp" :new-filename "yahoo-new-spamname.txt")
              ;;not work (my-dos-copy "C:\\Temp1\\test-file.txt" "C\\Temp\\" :new-filename "test-newnameXX2.txt")
              ;;works (my-dos-copy "C:\\Temp1\\test-file.txt" "C:\\Temp" :new-filename "test-newname.txt") = "copy \"C:\\Temp1\\test-file.txt\" \"C:\\Temp/test-newname.txt\"          1 file(s) copied.  "
              ;;works  (my-dos-copy "C:\\Temp1\\test-file.txt" "C:\\Temp" :new-filename "test-newnameX3.txt") = "copy \"C:\\Temp1\\test-file.txt\" \"C:\\Temp/test-newnameX3.txt\"         1 file(s) copied.
;;not work (my-dos-copy "C:/Temp1/test-file.txt" "C:/Temp" :new-filename "test-newnameX4.txt")
;; works (my-dos-copy "C:\\Temp1\\test-file.txt" "C:\\Temp" :new-filename "test-newnameX4.txt")




;;SAVE-DIRS-TO-DRIVES
;;2019
;;ddd
(defun save-dirs-to-drives (&key subdir-save-keys 
                                             omit-popup-p (show-results-popup-p T)
                                             (from-dir "C:\\Dropbox")
                                             ;;(from-dirs '("C:\\Dropbox-ADD\\"))
                                             (save-all-files-p T)  save-to-paths-p
                                             sel-drives omit-popup-p
                                             (save-files-in-from-dir-p T)
                                             (default-to-dir "0 RECENT BUs")
                                             (settings-path "C:\\temp\\dir-db-settings.lisp")
                                             filetypes not-filetypes 
                                             add-datestrs-p
                                             (datestr-begin 3)(datestr-end 11)
                                             (to-paths  ) (slashstr "\\")
                                             (drive-list  '( "E:" "F:" "G:" "H:"))
                                             return-details-p (sleep-betw-copy 0.5)
                                            (DOCS-subdir "0 RECENT-DOCS")
                                             (MEDIA-subdir "0 RECENT-MEDIA")
                                             (LW-subdir "1 LW MISC BUS")
                                             (MISC-subdir "1 RECENT-MISC BUs")
                                             (OTHER-subdirs '("1 PHONE BUs" "2 OTHER BUS"))
                                             (use-dos-copy-p T))
  "U-files-backup  SAVES ALL DIRS AND FILES-NOT SUBDIRS--IN MAIN-SUBDIRS.  Uses KEYS :DOCS, :MEDIA, :LW, :MISC, :OTHER (uses other-subdirs list). Copies all files within the DIRS in each main-subdir selected. OTHER is a list of additional DIRS in from-dir.`
   RETURNS (values copied-paths failed-to-copy-paths modified-buffers modified-filenames newbufferpaths drive-savepaths) if RETURN-DETAILS-P. Note: file datestr begins w/ yr,digit 4, ends with min, 10s digit. MUST USE slashstr "\\" for my-dos-copy. SEL-DRIVES=If = :ALL or omit-popup-p, saves to all dirs."
  ;;For input dir menu selection
  (setf  *default-from-bu-dir-x  from-dir   ;;*backup-input-dir-x
         *default-to-bu-dir-x default-to-dir
         *dir-bu-settings-path settings-path)
  ;;for menu callback default
 ;; (setf *default-from-bu-dir-x *backup-input-dir-x)
  ;;check for saved settings 
   (load-eval-db *dir-bu-settings-path :if-does-not-exist :create)
    
  (let*
      ((p)
       (selected-main-subdirs)
       (datestr (nth-value 9 (my-get-date-time :reverse-year-p T  :use-zeros-p T
                                               :return-unseparated-date-str-p T )))
       ;;"201902260847"
       (filedate (format nil "~A" (subseq datestr datestr-begin datestr-end)))
       (selected-drive-labels)
       (all-from-subdirs  (append (list DOCS-subdir MEDIA-subdir LW-subdir 
                                MISC-subdir) OTHER-subdirs))
       (drive-names)
       (voldrivestrs)
       (volinfos)
       (drivestrs)
#|       (modified-files)
       (modified-filenames)|#
       (orig-file-pathnames)
       ;;(orig-save-pathnames)
       (newfilepaths)
       (all-save-pathnames)
       (all-newsave-filenames) ;;with modified filenames (or not)
       (drive-savepaths (cond (save-to-paths-p to-paths)(t nil)))
       (copied-paths)
       (failed-to-copy-paths)
       (n-drives)
       (results-text "ERROR: NO RESULTS")
       ;;interface vars
       (interface-inst)
       )
   ;;WHEN USE POPUP CHOOSE FOLDERS TO COPY VERSION (runtime version)
     ;;GET SELECT INPUT FOLDERS & MAIN-DIR
   (cond
    (omit-popup-p
     ;;TO SELECT INPUT-DIRS FROM ARGS
     (when (member :docs subdir-save-keys)
       (setf selected-main-subdirs (append selected-main-subdirs (list DOCS-subdir))))
     (when (member :media subdir-save-keys)
       (setf selected-main-subdirs (append selected-main-subdirs (list MEDIA-subdir))))
     (when (member :lw subdir-save-keys)
       (setf selected-main-subdirs (append selected-main-subdirs (list LW-subdir))))
     (when (member :misc subdir-save-keys)
       (setf selected-main-subdirs (append selected-main-subdirs (list MISC-subdir))))
     ;;for OTHER-a list
     (when (member :other subdir-save-keys)
       (setf selected-main-subdirs (append selected-main-subdirs OTHER-subdirs)))
     ;;For save-files-in-from-dir-p
     (when save-files-in-from-dir-p
       (setf orig-file-pathnames (append orig-file-pathnames 
                                         (nth-value 1 (separate-subdirs-files from-dir)))))
     ;;end omit-popup-p
     )
    ;;USE THE INTERFACE
    (t 
     ;;here2
         (multiple-value-setq (drive-names voldrivestrs volinfos drivestrs)
             (find-drives-names drive-list))
         ;;SSSSS PROBLEM find-drives-names IS COMING UP WITH E? E: STILL THE PREFIX WHEN SHOULD BE F:
     (setf *choice-list-interface-data NIL)

     ;;MAKE INTERFACE-INST
     (setf interface-inst (make-instance 'back-up-dirs-to-drives-interface
                                         :input-main-dir-args 
                                         (list :items (list *default-from-bu-dir-x))
                                                                         ;;was*backup-input-dir-x))
                                         :input-subdirs-args (list :items all-from-subdirs)
                                         :output-drives-apply-args 
                                         (list :items voldrivestrs
                                               :background #(:rgb 0.3882353 0.94509805 0.6392157 1.0))
                                         :output-main-dir-args
                                         (list :text  (format nil
                                            "     5a. CURRENT MAIN OUTPUT FOLDER NAME=>  ~A" 
                                            *default-to-bu-dir-x))
                                         ))

     ;;SET SLOT-VALUES
     ;;so callback(s) can poke this process
     (setf (slot-value interface-inst 'sv-make-func-process) (mp:get-current-process)
           (slot-value interface-inst 'sv-from-dir) *default-from-bu-dir-x)

     ;;display
     (capi:display interface-inst)           
     ;;(break "after display")  ;;here2
    ;;end , t use popup,cond
     ))

   ;;PAUSE HERE 3 MINUTES
       (mp:current-process-pause 180) 

    ;;(BREAK "1")
    ;;MAKE ONE GIANT LIST OF ALL FILES TO BE SAVED
        ;;EXPLORE MAIN SUBDIRS (DOCS, MEDIA, etc) 
    ;;        AND ADD TO ALL-SAVE-PATHS ;;here5
    (when (null omit-popup-p)
      (setf selected-main-subdirs (slot-value interface-inst 'sv-selected-main-subdirs)))
    (when (slot-value interface-inst 'sv-from-dir)
      (setf from-dir (slot-value interface-inst 'sv-from-dir)))

    (loop
     for main-subdir in selected-main-subdirs
     do
     (let*
         ((main-subdir-pathname (format nil "~A\\~A" from-dir main-subdir))
          )
       (multiple-value-bind (flat-dirs-paths flat-dirs-filenames
                                     flat-file-lists  maindir-flat-filenames)
           (make-flat-dirs-list main-subdir-pathname nil :return-flat-filelists-p T
                                :return-namestrings-p T)
         (setf all-newsave-filenames (append all-newsave-filenames maindir-flat-filenames))
         ;;end mvb,let,loop
         )))
    ;;(break "after make-flat-dirs-list")  ;;orig filenames were ok in all-newsave-filenames

    ;;EXPLORE MAIN SUBDIRS (DOCS, MEDIA, etc) 
    ;;        AND ADD TO ALL-SAVE-PATHS 
    #|(loop
     for main-subdir in selected-main-subdirs
     do
     (let*
         ((main-subdir-pathname (format nil "~A\\~A" from-dir main-subdir))
           )
     (multiple-value-bind (subdirs files)
         (separate-subdirs-files main-subdir-pathname)
       (setf orig-file-pathnames (append orig-file-pathnames files)
             all-save-subdirs (append all-save-subdirs subdirs))
         ;;(BREAK "subdirs files")
       ;;end dir mvb,let, loop
       )))|#

    ;;SS FINISH REPLACING WITH 
    ;;MAKE LIST OF FILES TO SAVE OR NOT
    (loop
     for file-pathname in all-newsave-filenames ;;orig-file-pathnames
     do
     (let*
         ((newfilename)
          (newfilepath)
          (typedir)
          (subdir-1 "")
          ;;(filetype1) ;; (pathname-type file-pathname))
          (save-file-p)
          )
       ;;copying all files in from-dir (assumed they are modified)
  ;; dir filename filetype host)
       (multiple-value-bind (dir filename filetype host)
           (get-file-dir-name-type file-pathname)
           ;;NO (get-file-info file-pathname :return-details-p T )
      
         ;;FILTER FILE?
         (cond
          ((null save-all-files-p)
               (cond
                ((or (and not-filetypes (member filetype  not-filetypes :test 'string-equal))
                     (and filetypes (not (member filetype filetypes :test 'string-equal))))
                 (setf failed-to-copy-paths (append failed-to-copy-paths (list file-pathname))))
                (t (setf save-file-p T))))
          (t (setf save-file-p T)))

         ;;FOR SAVE FILES NOT  OUT
         (when save-file-p
           (setf  orig-file-pathnames (append orig-file-pathnames (list file-pathname)))          
           ;;add datestrs to filenames?
           (cond
            (add-datestrs-p
             (setf newfilename (format nil "~A-~A.~A" filename filedate  filetype)
                   ;;modified-files (append modified-files (list file))
                   all-newsave-filenames (append all-newsave-filenames (list newfilename))
                   ))             
            (t (setf newfilename (format nil "~A.~A" filename   filetype)
                     all-newsave-filenames (append all-newsave-filenames (list newfilename))
                     )))
           ;;(setf unmodified-files (append unmodified-files (list file)))
           ;;(break "newfilename")
           ;;MAKE THE NEW PATH with original subdirs and new filename
           (when  newfilename
             (multiple-value-bind (newpath real-path subdirs) 
                 (make-path-from-subdirs :dir-path file-pathname)
               ;;no, below  :filename newfilename  :add-filename-p T)))
               ;;(break "subdirs")
               (when subdirs
                 (setf typedir (nth (- (length subdirs) 2) subdirs)))
               (when (car (last subdirs))
                 (setf subdir-1 (car (last subdirs))))
               (setf newfilepath 
                     (format nil "~A~A~A~A~A~A" slashstr typedir slashstr
                             subdir-1 slashstr newfilename))
               (when newfilepath
                 (setf newfilepaths (append newfilepaths (list newfilepath))))
               ;;end mvb,when
               ))
           ;;end when save,mvb,let,loop
           ))))
         ;;(break "newfilepaths after file loop")

     ;;SAVE THE FILES TO SELECTED DRIVES
     ;;FOR THE UNKNOWN DRIVES
     ;;1-FIND DRIVE NAMES, ETC

#|     (multiple-value-bind (drive-names voldrivestrs volinfos drivestrs)
         (find-drives-names drive-list)

       ;;append to-paths to found drives
       (when to-paths
         (setf voldrivestrs (append voldrivestrs to-paths)
               drivestrs (append drivestrs to-paths)
               drive-names (append drive-names to-paths)))                       

       ;;WHICH DIRS TO PRE-SELECT?
       (cond
        ((or (equal sel-drives :all)  omit-popup-p)
         (setf n-drives (list-length drive-names)
               sel-drives (make-list-n-long n-drives '(1 2 3 4 5 6 7 8))))
        (sel-drives NIL)      
        (t (setf sel-drives '(0))))

       ;; 2-CHOOSE THE DRIVES
       (setf *choice-list-interface-data nil)
       ;;(break "voldrivestrs") 

       ;;MAKE POPUP MULTI CHOICE LIST (unless omit-popup-p)
       (unless omit-popup-p
         (make-choice-list-interface-instance
          :sel-items sel-drives
          :title "SAVE FILES TO SELECTED DRIVES"
          :info-text (format nil "Select (or unselect) all backup drives to write the following files to: ~%~A" modified-filenames)
          :title-bkgr :green :info-align :center :info-font-color :blue
          :choice-items voldrivestrs
          :choice-type :MULTIPLE-SELECTION :confirm-input-p NIL
          :window-args 
          (list :title 
                (format nil "SELECT THE DRIVES to save to DIR: ~A" default-to-dir)
                :visible-min-width 400 :visible-min-height 340
                :internal-border 10
                :background #(:rgb 0.3882353 0.94509805 0.6392157 1.0))
          :go-button-args (list :visible-min-width 300))

         (mp:current-process-pause 30)
         ;;end unless
         )
       ;;GET SELECTED DRIVES
       (cond
        (omit-popup-p
         (setf selected-drive-labels  voldrivestrs))
        (t (setf selected-drive-labels  *choice-list-interface-data)))
       ;;(break "selected-drive-labels")
|#
       ;;END NEW
    
       ;; 2-SAVE THE FILES TO ALL DRIVES SELECTED 
       (setf selected-drive-labels (slot-value interface-inst 'sv-selected-drives))
       (when (slot-value interface-inst 'sv-default-to-bu-dir)
         (setf default-to-dir (slot-value interface-inst 'sv-default-to-bu-dir)))

       (loop
        for drive-label in selected-drive-labels
        do
        (multiple-value-bind (drivestr voldrivestr nth-item)
            (get-list2-item-from-list1-matched-Nth-item  drive-label voldrivestrs drive-names )

          (let*
              ((vol-label )
               (label (format nil "~A~A~A"  drivestr slashstr vol-label))
               (drive-savepath (format nil "~A~A~A" drivestr slashstr default-to-dir))
               
                                       ;;SSS USE A SLOT VALUE NOT GLOBAL? default-to-dir))
               )
            (setf drive-savepaths (append drive-savepaths (list drive-savepath)))
            ;;(break "drive-savepath")

            ;;  GET INFO FROM EACH FILE TO CREATE SAVEPATHS here2
            (loop
             for file in all-newsave-filenames  ;;modified-files
             for newfilepath in newfilepaths
             for orig-file-pathname in orig-file-pathnames
             do
             (let*
                 ((savepath (format nil "~A~A" drive-savepath newfilepath))
                  (savepath-dir (my-directory-namestring savepath :incl-host-p T))
                  )
               ;;ENSURE DIR EXISTS
               (my-ensure-path-exists savepath-dir)
              ;;(break "savepath, savepath-dir")
               ;;USE MY-DOS-COPY?
               (cond
                (use-dos-copy-p
                 (setf drive-savepaths (append drive-savepaths 
                                               (list (list savepath :from orig-file-pathname) )))
                 ;;get original file (hopefully up-to-date) contents
                 (multiple-value-bind (dostext dosresult copied-p)
                     (my-dos-copy orig-file-pathname savepath)
                   (cond
                    (copied-p
                     (setf copied-paths (append copied-paths (list savepath))))
                    (t
                     (setf failed-to-copy-paths (append failed-to-copy-paths (list savepath)))))
                   (sleep sleep-betw-copy)
                   ))
                ;;OR  USE WITH-OPEN AND USE FILE CONTENTS
                ;;PROBLEMS: 1-changes file names (adds datestr). 2-problems with denied access in dropbox, etc.
                (T NIL
                   ;;end  t, cond
                   ))
             ;;end inner let, loop
             ))
          ;;end let,mvb,outer loop
          )))
       ;;show results popup?
       (when show-results-popup-p
         (setf results-text (format nil "
    COPIED FROM DIR: ~A
    COPIED TO DRIVES: ~A~%
    COPIED PATHS: ~A~%~%   FAILED TO COPY PATHS: ~A~%" 
                                    from-dir selected-drive-labels copied-paths 
                                    failed-to-copy-paths))
         (show-text results-text 100 "BACKED-UP FILES RESULTS" :min-width 150 ))
     ;;return
     (cond 
      ((null return-details-p)
       (values copied-paths failed-to-copy-paths ))
      (t   (values copied-paths failed-to-copy-paths   newfilepaths drive-savepaths)))
     ;;end  let, save-dirs-to-drives
     ))
;;TEST
;; SSSSS START TESTING HERE save-dirs-to-drives
;; (save-dirs-to-drives )
;; WORKS= 
#|    COPIED FROM DIR: C:\Dropbox-ADD
    COPIED TO DRIVES: (USBN1-128) 
    COPIED PATHS: (F:\1 MISC-BU-TEST\DOCS\cancer stopper_files\arrow.gif F:\1 MISC-BU-TEST\DOCS\cancer stopper_files\bnr_overstock_020905.gif F:\1 MISC-BU-TEST\DOCS\cancer stopper_files\button_search.gif F:\1 MISC-BU-TEST\DOCS\cancer stopper_files\de.gif F:\1 MISC-BU-TEST\DOCS\cancer stopper_files\du.gif  .... ETC....     F:\1 MISC-BU-TEST\DOCS\2018 tax year\ToMaui Hertz E-Return 830889010.pdf F:\1 MISC-BU-TEST\DOCS\2018 tax year\TomTiaa-5498.pdf F:\1 MISC-BU-TEST\Dropbox-ADD\DOCS\cancer stopper.htm F:\1 MISC-BU-TEST\Dropbox-ADD\DOCS\TomSherryLetterHead.docx)
   FAILED TO COPY PATHS: NIL|#
;; (save-dirs-to-drives :subdir-save-keys '(:DOCS))
;; (make-input-dirs-choice-popup :subdir-save-keys '("DOCS" "MEDIA" "MISC"))


;;SELECT-INPUT-SUBFOLDERS-CALLBACK
;;2019
;;ddd
(defun select-input-subfolders-callback (item interface)
  "interface is calling button"
  (let
      ;;PROBLEM -- NOT FIND TOP-LEVEL
      ((main-inst (capi:top-level-interface interface ))
       (sel-subdirs)
       )                   
    (with-slots (input-subdirs-choice-list-pane) main-inst
    (setf sel-subdirs (capi:choice-selected-items input-subdirs-choice-list-pane)
          (slot-value main-inst  'sv-selected-main-subdirs) sel-subdirs)
         ;; (break "items")
         (show-text (format nil "NEW INPUT-SUBFOLDERS LOCATION=>~% 
          ~A~%" sel-subdirs) 40  "NOTICE of CHANGE"  :min-width 100)
          )))




;;SET-BU-TO-DRIVES-CALLBACK
;;2019
;;ddd
(defun set-bu-to-drives-callback (subdirs interface)
  "   RETURNS    INPUT:  "
  (with-slots (output-drives-choice-list-pane ) interface    
  (let
      ((drives (capi:choice-selected-items output-drives-choice-list-pane))
       )
;;here4
    ;;(break "subdirs")
    (setf (slot-value interface 'sv-selected-drives) drives)
    (show-text (format nil "NEW SELECTED OUTPUT DRIVES=>~% 
          ~A~%" drives) 40  "NOTICE of CHANGE"  :min-width 100)
    ;;end let, defun
    )))
;;TEST
;;


;;CHANGE-SAVE-TO-FOLDER-NAME-CALLBACK
;;2019
;;ddd
(defun change-save-to-folder-name-callback (interface)
  "U-files-backup.lisp"
  (with-slots (new-output-main-dir output-main-dir) interface
    (let
        ((new-location  (capi:text-input-pane-text new-output-main-dir ))
#|(text-input-inst (make-text-input-or-button-interface-instance :instr-text "Type MAIN OUTPUT folder name below. Use only the name--NO SLASHES."
                                                                       :make-func-process (mp:get-current-process)))|#
         )
      (setf *default-to-bu-dir-x new-location
       (slot-value interface 'sv-default-to-bu-dir) new-location)
      ;;change text in info box
      (capi:apply-in-pane-process output-main-dir
                                  #'(setf capi:rich-text-pane-text ) (format nil
                                            "     5a. CURRENT MAIN OUTPUT FOLDER NAME=>  ~A" 
                                             new-location) output-main-dir)
      ;;end save-to
      )))


;;SAVE-USING-SEL-SETTINGS-CALLBACK
;;2019
;;ddd
(defun save-using-sel-settings-callback (interface)
 "U-files-backup"
 (let
     ((process (slot-value interface 'sv-make-func-process))
      )
 (mp:process-poke process)
 ))


;;SELECT-DIR-SETTINGS-CALLBACK
;;2019
;;ddd
(defun select-dir-settings-callback (item interface)  ;;works
  "U-files-backup.lisp"
  (let*
      ((new-location)
        )
  (cond
   ((string-equal item "Choose MAIN Save-FROM Folder")
    (setf  new-location 
           (select-dir&poke-callback  (list "Select MAIN Save-FROM Folder"
                             *default-from-bu-dir-x) interface))
    ;;pause process
    (mp:current-process-pause 40)
    (setf *default-from-bu-dir-x new-location))
   ((string-equal item "Choose MAIN Save-TO Folder")
    (let
        ((text-input-inst (make-text-input-or-button-interface-instance :instr-text "Type MAIN OUTPUT folder name below. Use only the name--NO SLASHES."
                                                                       :make-func-process (mp:get-current-process)))
         )
      (mp:current-process-pause 30)
    (setf new-location (format nil "\\~A"   *text-input-or-button-interface-textdata)
          *text-input-or-button-interface-textdata nil
          *default-to-bu-dir-x new-location)
          ;;(break "new-loc")
    (with-slots (output-main-dir) interface
      (capi:apply-in-pane-process output-main-dir
                                  #'(setf capi:rich-text-pane-text ) (format nil
                                            "     5a. CURRENT MAIN OUTPUT FOLDER NAME=>  ~A" 
                                             new-location) output-main-dir)
      ;;end save-to
      ))) ;;here1
   ((string-equal item "Choose SETTINGS Folder")
    (setf  new-location 
           (select-dir&poke-callback  (list "Change SETTINGS Folder. IF CHANGE, MUST SELECT EACH TIME."
                             *default-settings-dir-x) interface))
    ;;pause process
    (mp:current-process-pause 40)
    (setf *dir-bu-settings-dir new-location)
#|          (capi:prompt-for-directory "Change SETTINGS Folder. IF CHANGE, MUST SELECT EACH TIME." :pathname *default-settings-dir-x)
          *dir-bu-settings-dir new-location)))     |#   
  ;;show text popup
         (show-text (format nil "NEW DEFAULT FOLDER LOCATION=>~% 
          ~A~%" new-location) 40  "NOTICE of CHANGE"  :min-width 100)))
  ;;end let, select-dir-settings-callback
  ))
;; 

;;SELECT-DIR&POKE-CALLBACK
;;2019
;;ddd
(defun select-dir&poke-callback (data interface)
  "U-files-backup. Selects dir and pokes interface.  INPUT: data is nil or a LIST with (text pathname); text= new instruction text.  If pathname nil, uses *select-dir&poke-callback global var. RETURNS: *select-dir&poke-callback-dir"
  (let
      ((text "  SELECT FOLDER:   ")
       )
    (when data
      (when (car data)
        (setf text (car data)))
      (when (second data)
        (setf *select-dir&poke-callback-dir (second data))))           
    
    (setf *select-dir&poke-callback-dir
          (capi:prompt-for-directory text
                                     :pathname *select-dir&poke-callback-dir))
    (mp:process-poke interface)    
    *select-dir&poke-callback-dir
   ;;end let, SELECT-DIR&POKE-CALLBACK
    ))




;;CHANGE-SAVE-TO-FOLDER-NAME-CALLBACK
;;2019
;;ddd
#|(defun change-save-to-folder-name-callback (interface)
  (with-slots (new-output-main-dir) interface
      (setf  *default-backup-dir-x (capi:text-input-pane-text new-output-main-dir)
             *default-from-bu-dir-x *default-backup-dir-x)
                                                              
      (show-text (format nil "MAIN OUTPUT FOLDER RENAMED TO=>~% 
          ~A~%" *default-backup-dir-x) 40 "NEW OUTPUT FOLDER NAMED"
                 :min-width 100)
  ;;end w/slots, change-save-to-folder-name-callback
  ))|#






;;BACK-UP-DIRS-TO-DRIVES-INTERFACE
;;2019
;;ddd
(capi:define-interface back-up-dirs-to-drives-interface ()
  ((sv-var1
    :initarg :sv-var1
    :accessor sv-var1
    :initform NIL
    :type  NIL
    :documentation  "Variable 1 to store misc info from calling function, etc"
    )
   (sv-from-dir
    :initarg :sv-from-dir
    :accessor sv-from-dir
    :initform nil
    :type  nil
    :documentation  "sv-from-dir"
    )
   (sv-selected-main-subdirs
    :initarg :sv-selected-main-subdirs
    :accessor sv-selected-main-subdirs
    :initform NIL
    :type NIL
    :documentation  "sv-selected-main-subdirs for bu input")
   (sv-default-to-bu-dir
    :initarg :sv-default-to-bu-dir
    :accessor sv-default-to-bu-dir
    :initform NIL
    :type NIL
    :documentation  "sv-default-to-bu-dir for bu")
   (sv-selected-drives
    :initarg :sv-selected-drives
    :accessor sv-selected-drives
    :initform NIL
    :type NIL
    :documentation  "sv-selected-drives for bu")
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

   )
  ;;MENUS
  ;;SSSSS START HERE ADD MENUS, THEN FIX CALLBACKS
  (:menu-bar file-menu save-settings-menu)
  (:menus
  (file-menu
   "Default Folders"
   ("Choose MAIN Save-FROM Folder" "Choose MAIN Save-TO Folder" "Choose SETTINGS Folder" )
   :selection-callback 'select-dir-settings-callback
   :callback-type :data-interface)
  (save-settings-menu
   "Save Default Settings NOW"
   ("Save File Settings Now")
   :callback 'save-dir-bu-settings-callback
   :callback-type :interface 
   )
       ;;end menus
       )
  ;;PANES
  (:panes
   (title-pane
    capi:rich-text-pane    :accepts-focus-p NIL
  ;;  :make-instance-extra-apply-args :title-pane-args
    :visible-border T
    :internal-border 20 
    :text  (format nil "TOM'S APP~%FOR BACKING UP FOLDERS ON MULTIPLE DRIVES~%")
      :character-format *charformat-12B-RED
      :paragraph-format *paraformat-center-tab5
    :background :yellow        
    :visible-max-height 20
    ;;end title-pane
    )
   (info-pane
    capi:rich-text-pane    :accepts-focus-p NIL
   ;; :make-instance-extra-apply-args :info-pane-args
    :visible-border T
    :internal-border 20
    :visible-min-height 120
      :character-format *charformat-10
    :text (format nil   
 "   
    INSTRUCTIONS: 
     1. Select the MAIN INPUT FOLDER containing FOLDERS TO BE BACKED UP 
     2. OR Select a new MAIN INPUT FOLDER.
     3. Select which SPECIFIC SUBFOLDERS ARE TO BE BACKED UP.
         (These may be subfolders of the main input folder (or not).
     4. Select the DRIVES to back up the above subfolders to.
     5. Select the MAIN DESTINATION BACKUP FOLDER NAME 
         (The folder doesn't need to exist).  All subfolders WILL BE SAVED HERE.
         They will be saved using the subfolder names from step 3 above.   " )
    :background :light-blue
    :make-instance-extra-apply-args :info-pane-apply-args
    )
   ;;MAIN DIR CHOICE
   (input-main-dir-choice-list-pane
    capi:list-panel   
    ;; :selection 0
    :title "   1.  SELECT MAIN FOLDER where input subfolders are located.    "
    :callback-type :item-interface
    :visible-max-height 40
    :make-instance-extra-apply-args :input-main-dir-args
    :interaction :multiple-selection
    )
   (select-new-main-dir-button
    capi:push-button
    :background :green
    :text  "        2. PUSH THIS BUTTON TO CHANGE MAIN INPUT FOLDER         "
    :font  *go-frame-button-font 
    :callback-type :interface
    :callback 'select-dir-callback
    ;;   :color-requirements 
    ;;   :selected T
    :default-p T  ;;means if return hit, selects this button
    )
   ;;SUBDIRS CHOICE-LIST
   (input-subdirs-choice-list-pane
    capi:list-panel   
    ;; :selection 0
    :title "    3a. SELECT SUBFOLDERS TO BACK UP"
    :callback-type :item-interface
    :make-instance-extra-apply-args :input-subdirs-args
    :interaction :multiple-selection
    )
   ;;SUBDIRS OK BUTTON
   (find-subdirs-button
    capi:push-button
    :background :green
    :text  "        3b.  PUSH AFTER SELECT SUBFOLDERS TO BACK UP ABOVE           "
    :font  *go-frame-button-font 
    :callback-type :item-interface
    :callback  'select-input-subfolders-callback
    ;;   :color-requirements 
    ;;   :selected T
    :default-p T  ;;means if return hit, selects this button
    )
   
   ;;SELECT OUTPUT DRIVES
   (output-drives-choice-list-pane
    capi:list-panel   
    ;; :selection 0
    :title "    4a. SELECT DRIVES FOR BACKUPS   "
    :make-instance-extra-apply-args :output-drives-apply-args
    :interaction :multiple-selection
    )
   ;;OUTPUT DRIVES OK BUTTON
   (drives-ok-button
    capi:push-button
    :background :green
    :text  "           4b. OK: USE SELECTED DRIVES FOR BACKUP  <<<                "
    :font  *go-frame-button-font 
    :default-p T  ;;means if return hit, selects this button
    :callback-type :item-interface
    :callback  'set-bu-to-drives-callback
    )
   ;;OUTPUT MAIN-DIR
   ;;current default output-main dir
   (output-main-dir
    capi:rich-text-pane
    :visible-max-height 20
    :make-instance-extra-apply-args :output-main-dir-args
    )    
   ;;type in a name
   (new-output-main-dir
    capi:text-input-pane
    :title "   5b. FOR NEW  MAIN OUTPUT FOLDER NAME type here=> "
    )
   (change-main-to-dir-button
    capi:push-button
    :background :green
    :text  "       5c.  PUSH THIS BUTTON TO CHANGE  MAIN OUTPUT FOLDER NAME ABOVE     "
    :font  *go-frame-button-font 
    :callback-type :interface
    :callback 'change-save-to-folder-name-callback
    :default-p T  ;;means if return hit, selects this button
    )
   (go-fr-button
    capi:push-button
    :background :green
    :text  "               >>>>>>>>  6.  FINISHED: BACK UP FOLDERS TO ABOVE DRIVES  <<<<<<<<                   "
    :font  *go-frame-button-font 
    ;;   :color-requirements 
    ;;   :selected T
    :default-p T  ;;means if return hit, selects this button
    ;;was (gp:make-font-description :size *button-font-size  :weight :bold) ;; :slant :italic)
    ;; choose in calling function
    ;;  :callback 'text-input-OR-button-interface-callback ;;'go-select-scales-frame-callback
    :callback 'save-using-sel-settings-callback
    :make-instance-extra-apply-args :go-button-args
    :callback-type :interface
    )
   ;;end panes
   )   
  ;;LAYOUTS
  (:layouts
   (column-layout
    capi:column-layout
    '(title-pane  info-pane   input-main-dir-choice-list-pane 
                  select-new-main-dir-button :separator
                  input-subdirs-choice-list-pane
                  find-subdirs-button   :separator
                  output-drives-choice-list-pane drives-ok-button :separator
                                      output-main-dir   new-output-main-dir :separator
                                      change-main-to-dir-button :separator  :separator go-fr-button))
   ;;end layouts
   )
  (:default-initargs
   :best-width 600
   :best-height 800
   :layout 'column-layout
   :title "  TOM'S APP for FOLDERS BACKUP TO SELECTED DRIVES  "
   :internal-border 20
   :background :YELLOW
   )
  ;;END back-up-dirs-to-drives-interface
  )
;;test
;; (let ((inst (make-instance 'back-up-dirs-to-drives-interface))) (capi:display inst))





(defparameter *dir-bu-settings-filename "dir-bu-settings.lisp" "Default file for saving generic settings from save-settings-callback")
(defparameter  *dir-bu-settings-db-X-list* NIL)
(defparameter  *default-settings-dir-x "c:\\temp\\")

;;SAVE-DIR-BU-SETTINGS-CALLBACK
;;2019
;;ddd 
(defun save-dir-bu-settings-callback (interface)
  "U-capi-input-interfaces.lisp"
  ;;make *settings-db-X-list* saving (often reinitializing) all settings
  (setf *dir-bu-settings-filename (delete-final-string '("\\" "/") *dir-bu-settings-filename)
   *dir-bu-settings-path (format nil "~A~A" *dir-bu-settings-dir 
                                      *dir-bu-settings-filename)
        *dir-bu-settings-db-X-list*              
        `((*dir-bu-settings-path ,*dir-bu-settings-path)
          (*default-from-bu-dir-x ,*default-from-bu-dir-x  )
          (*default-to-bu-dir-x, *default-to-bu-dir-x )
          ))
     ;;here3
  (save-db *dir-bu-settings-db-X-list* *dir-bu-settings-path)
   (show-text (format nil "Settings saved to: ~A~%" *dir-bu-settings-path) 30 nil)
        )






#|
;;MAKE-FLAT-SAVEFILES-FROM-FILETREE
;;2019
;;ddd
(defun make-flat-savefiles-from-filetree (main-dirpath 
                                          &key (incl-subdirs :all)
                                          (save-all-files-p T)                                                
                                             (save-files-in-from-dir-p T)
                                             (default-to-dir "\\EXTRA-FILES")
                                             filetypes not-filetypes 
                                             add-datestrs-p
                                             (datestr-begin 3)(datestr-end 11)
                                             (to-paths  ) (slashstr "\\")
                                             (all-savepaths)
                                             )
  "U-file-backup RETURNS
      NOTE:  SUBDIR filenames ARE NOT CHANGED, BUT ALL LISTED AS IS.
"
  (let*
      ((main-dir-items (list-directory-namestrings main-dir))
       (subdirpaths)
       (newfilepaths)
       )
    ;;separate subdirs, files
    (multiple-value-bind (main-subdirs filepaths other)
        (separate-subdirs-files main-dir-items)

      ;;FOR SUBDIRS
      (cond
       ((equal incl-subdirs :all)
        (setf  subdirpaths main-subdirs))
       ((null incl-subdirs) NIL)
       (t (setf subdirpaths  incl-subdirs)))

      ;;FOR FILES      
      (loop
       for file-pathname in file-pathnames
       do
       (let*
           ((newfilename)
            (newfilepath)
            (subdir-1 "")
            ;;(filetype1) ;; (pathname-type file-pathname))
            (save-file-p)
            )
         ;;copying all files in from-dir (assumed they are modified)
         ;; dir filename filetype host)
         (multiple-value-bind (dir filename filetype host)
             (get-file-dir-name-type file-pathname)
           ;;NO (get-file-info file-pathname :return-details-p T )
      
           ;;FILTER FILE?
           (cond
            ((null save-all-files-p)
             (cond
              ((or (and not-filetypes (member filetype  not-filetypes :test 'string-equal))
                   (and filetypes (not (member filetype filetypes :test 'string-equal))))
               (setf failed-to-copy-paths (append failed-to-copy-paths (list file-pathname))))
              (t (setf save-file-p T))))
            (t (setf save-file-p T)))

           ;;FOR SAVE FILES NOT  OUT
           (when save-file-p
             (setf  orig-file-pathnames (append orig-file-pathnames (list file-pathname)))          
             ;;add datestrs to filenames?
             (cond
              (add-datestrs-p
               (setf newfilename (format nil "~A-~A.~A" filename filedate  filetype)
                     ;;modified-files (append modified-files (list file))
                     all-newsave-filenames (append all-newsave-filenames (list newfilename))
                     ))             
              (t (setf newfilename (format nil "~A.~A" filename   filetype)
                       all-newsave-filenames (append all-newsave-filenames (list newfilename))
                       )))
             ;;(setf unmodified-files (append unmodified-files (list file)))

             ;;MAKE THE NEW PATH with original subdirs and new filename
             (when  newfilename
               (multiple-value-bind (newpath real-path subdirs) 
                   (make-path-from-subdirs :dir-path file-pathname)
                 ;;no, below  :filename newfilename  :add-filename-p T)))
                 ;;(break "subdirs")
                 (when (car (last subdirs))
                   (setf subdir-1 (car (last subdirs))))
                 (setf newfilepath 
                       (format nil "~A~A~A~A" slashstr subdir-1 slashstr newfilename))
                 (when newfilepath
                   (setf newfilepaths (append newfilepaths (list newfilepath))))
                 ;;end mvb,when
                 ))
             ;;end when save,mvb,let,loop
             ))))
     ;;end mvb,
     )
    (values newfilepaths subdirpaths orig-file-pathnames main-subdirs)
    ;;end let, make-flat-savefiles-from-filetree    
    ))
;;TEST
;; (make-flat-savefiles-from-filetree
;; (make-flat-savefiles-from-filetree|#






;;OLD--DELETE
#|(defun make-input-dirs-choice-popup (&key input-folders
                                      (default-dir "C:\\Dropbox-ADD\\" )
                                     make-func-process)
  "U-files-backup INPUT: input-folders= list of strings RETURNS: 
  SETS global *make-input-dirs-choice-popup-result = selected list of input folders."
  ;;for global variable used in callback
  (setf *default-from-bu-dir-x default-dir)
  (unless input-folders
    (setf input-folders (namestrings-list (list-directory default-dir))))
  (let*
      ((default-dir-menu
        (make-file-menu1 :main-menu-title "File" 
                         :main-menu-text "Select Below"
                         :main-menu-callback nil
                         :sel-dir-title "Select Folder"
                         :sel-dir-text "Folder"
                         :sel-dir-default-folder default-dir
                         :sel-dir-callback 'select-dir1-callback))    
       ;;main popup-instance
       (choice-popup-inst
        (make-choice-list-interface-instance
         :sel-items nil
         :menu-bar-items (list default-dir-menu)
         :title "SAVE FILES FROM SELECTED FOLDERS"
         :info-text (format nil "Select (or unselect) all FOLDERS to backup")
         :title-bkgr :green :info-align :center :info-font-color :blue
         :choice-items  input-folders
         :choice-type :MULTIPLE-SELECTION :confirm-input-p NIL
         :window-args 
         (list :title 
               (format nil "SELECT THE FOLDERS IN DIR: ~A TO SAVE." default-dir)
               :visible-min-width 400 :visible-min-height 340
               :internal-border 10
               :background #(:rgb 0.3882353 0.94509805 0.6392157 1.0))
         :go-button-args (list :visible-min-width 300)
         ;;  :sv-var1        :sv-var2f
         :close-make-func-process-p T
         :make-func-process make-func-process
         ;;end let make-input-dirs-choice-popup
         ))
       ;;end let vars
       )
    ;;display, pause, get return-value
    (capi:display choice-popup-inst)
          
    (setf *make-input-dirs-choice-popup-result 
          *choice-list-interface-data
          ;;reset 
          *choice-list-interface-data NIL)
    ;;poke
    (mp:process-poke make-func-process)
    ;;end let,make-input-dirs-choice-popup
    ))|#



;;WORKS, BUT DELETE? REPLACED
#|(defun make-back-up-dirs-to-drives-interface (&key input-folders
                                      (default-dir "C:\\Dropbox-ADD\\" )
                                     make-func-process)
  "U-files-backup INPUT: input-folders= list of strings RETURNS: 
  SETS global *make-back-up-dirs-to-drives-interface-result = selected list of input folders."
  ;;for global variable used in callback
  (setf *default-from-bu-dir-x default-dir)
  (unless input-folders
    (setf input-folders (namestrings-list (list-directory default-dir))))
  (let*
      ((default-dir-menu
        (make-file-menu1 :main-menu-title "File" 
                         :main-menu-text "Select Below"
                         :main-menu-callback nil
                         :sel-dir-title "Select Folder"
                         :sel-dir-text "Folder"
                         :sel-dir-default-folder default-dir
                         :sel-dir-callback 'select-dir1-callback))   
       ;;main popup-instance
       (choice-popup-inst
        (make-choice-list-interface-instance
         :sel-items nil
         :menu-bar-items (list default-dir-menu)
         :title "SAVE FILES FROM SELECTED FOLDERS"
         :info-text (format nil "Select (or unselect) all FOLDERS to backup")
         :title-bkgr :green :info-align :center :info-font-color :blue
         :choice-items  input-folders
         :choice-type :MULTIPLE-SELECTION :confirm-input-p NIL
         :window-args 
         (list :title 
               (format nil "SELECT THE FOLDERS IN DIR: ~A TO SAVE." default-dir)
               :visible-min-width 400 :visible-min-height 340
               :internal-border 10
               :background #(:rgb 0.3882353 0.94509805 0.6392157 1.0))
         :go-button-args (list :visible-min-width 300)
         ;;  :sv-var1        :sv-var2f
         :close-make-func-process-p T
         :make-func-process make-func-process
         ;;end let make-back-up-dirs-to-drives-interface
         ))
       ;;end let vars
       )
    ;;display, pause, get return-value
    (capi:display choice-popup-inst)
          
    (setf *make-back-up-dirs-to-drives-interface-result 
          *choice-list-interface-data
          ;;reset 
          *choice-list-interface-data NIL)
    ;;poke
    (mp:process-poke make-func-process)
    ;;end let,make-back-up-dirs-to-drives-interface
    ))|#


;;MAKE-BACK-UP-DIRS-TO-DRIVES-INTERFACE
;;2019
;;ddd
#|(defun make-back-up-dirs-to-drives-interface (&key input-folders
                                                   (default-to-dirs '("C:\\Dropbox-ADD\\"))
                                      (default-to-dir "C:\\Dropbox-ADD\\" )
                                     make-func-process)
  "U-files-backup INPUT: input-folders= list of strings RETURNS: 
  SETS global *make-back-up-dirs-to-drives-interface-result = selected list of input folders."
  ;;for global variable used in callback
  (setf *default-from-bu-dir-x default-to-dir)
  (unless input-folders
    (setf input-folders (namestrings-list (list-directory default-to-dir))))
  (let*
      ((default-to-dir-menu
        (make-file-menu1 :main-menu-title "File" 
                         :main-menu-text "Select Below"
                         :main-menu-callback nil
                         :sel-dir-title "Select Folder"
                         :sel-dir-text "Folder"
                         :sel-dir-default-folder default-to-dir
                         :sel-dir-callback 'select-dir1-callback))   
       ;;main popup-instance
       (choice-popup-inst
        (make-choice-list-interface-instance
         :sel-items nil
         :menu-bar-items (list default-dir-menu)
         :title "SAVE FILES FROM SELECTED FOLDERS"
         :info-text (format nil "Select (or unselect) all FOLDERS to backup")
         :title-bkgr :green :info-align :center :info-font-color :blue
         :choice-items  input-folders
         :choice-type :MULTIPLE-SELECTION :confirm-input-p NIL
         :window-args 
         (list :title 
               (format nil "SELECT THE FOLDERS IN DIR: ~A TO SAVE." default-to-dir)
               :visible-min-width 400 :visible-min-height 340
               :internal-border 10
               :background #(:rgb 0.3882353 0.94509805 0.6392157 1.0))
         :go-button-args (list :visible-min-width 300)
         ;;  :sv-var1        :sv-var2f
         :close-make-func-process-p T
         :make-func-process make-func-process
         ;;end let make-back-up-dirs-to-drives-interface
         ))
       ;;end let vars
       )
    ;;display, pause, get return-value
    (capi:display choice-popup-inst)
          
    (setf *make-back-up-dirs-to-drives-interface-result 
          *choice-list-interface-data
          ;;reset 
          *choice-list-interface-data NIL)
    ;;poke
    (mp:process-poke make-func-process)
    ;;end let,make-back-up-dirs-to-drives-interface
    ))|#
;;TEST
;; (make-back-up-dirs-to-drives-interface '("DOCS" "MEDIA" "MISC"))
