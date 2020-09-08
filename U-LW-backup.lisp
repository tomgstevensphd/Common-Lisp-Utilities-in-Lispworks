;;******************** U-LW-backup.lisp **************************
;;
;;FUNCTIONS TO BACK UP MY LW PROJECT FILES SIMPLY
;;
(when (not (boundp '*current-lw-editing-files))
  (defparameter *current-lw-editing-files NIL))

(when (or (not (boundp '*current-lw-budirs)) (null *current-lw-budirs))
  (defparameter *current-lw-budirs '("c:/3-TS/LISP PROJECTS TS/" )))

;;LWBU
;;2018
;;ddd
(defun LWBU (&key (backup-list *current-lw-editing-files)(backup-dirs  *current-lw-budirs ))
  "U-LW-backup  RETURNS    INPUT:  file-list items can be EITHER a complete pathname OR A LIST of form (:DIR dirpath (filename1 filename2 ...))"
  (let
      ((nfiles (list-length file-list))
       (not-backedup-items)
       (filepaths)
       )
    (loop
     for item in backup-list
     do
    (cond
     ((listp item)
      (setf dir (second item)
            filelist (third item))
      (loop
       for file in filelist
       do
       (setf filepaths (append filepaths (list (format nil "~A/~A" dir file))))
       ;;end loop, listp
      ))
     ((stringp item)    
      (setf filepaths (append filepaths (list item))))
     (t (setf not-backedup-items (append not-backedup-items (list item)))))
    ;end loop
    )


    ;;COPY THE FILES
    (loop
     for path in filepaths
     for n 
     do
     (let*
         ((filename)
          (pathname)
          (last-subdir (car (last (FIND-SUBDIR-NAMESTRINGS  path ))
          )
;;SSSSSS FIX TO WORK ON FORWARD SLASHES
#|CL-USER 1 > (find-subdir-namestrings "C:\\3-TS/LISP PROJECTS TS/MyUtilities\\U-lists.lisp")
("C:" "3-TS/LISP PROJECTS TS/MyUtilities" "U-lists.lisp")
"C:"|#


     (loop
      for dir in 

     (lw:copy-file filename 
     
     ;;end loop
     )

    (values    )
    ;;end let, LWBU
    ))
;;TEST
;; (LWBU)