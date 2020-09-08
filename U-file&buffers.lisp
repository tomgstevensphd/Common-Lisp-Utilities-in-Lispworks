;;********************** U-file&buffers.lisp *************************
;;
;;
;; *** ALSO SEE: U-BUFFERS.LISP
;;                              U-files&buffers-Frames.lisp
;;                              U-LW-editor




;;GET-FILE-LINES
;;2020
;;ddd
(defun get-file-lines (pathname start-line-n &optional end-line-n
                                &key mark-at-lines-start  mark-at-lines-end
                                (return-formated-string-p T) (line-width 200)
                                add-newlines (left-margin-spaces 3)
                                (file-max-lines 25000))
  "U-files&buffers  RETURNS: (values all-selected-lines start-line end-line file-pos-start-line file-pos-end-line) "
  (let
      (;;(line)
       (all-selected-lines)
       (n-lines 0)
       (start-line)
       (end-line)
       (file-pos-start-line)
       (file-pos-end-line)
       (formated-string)
       )
    ;;read the file lines
    (with-open-file (stream pathname :direction :io :if-exists :overwrite)
      (loop 
       for n from 1 to file-max-lines
       do
       (let*
           ((line (read-line stream nil 'eof ))
            )
         (cond
          ((and (numberp start-line-n)(= n start-line-n))
           (setf start-line line
                 all-selected-lines (append all-selected-lines (list line))
                 file-pos-start-line (file-position stream))
           (when mark-at-lines-start             
             (format stream ";;~A~%" mark-at-lines-start))
           (when (null end-line-n)
             (return))
           )
          ((and (numberp end-line-n )(= n end-line-n))
           (setf end-line line
                 all-selected-lines (append all-selected-lines (list line))
                 file-pos-end-line (file-position stream))
           (when mark-at-lines-end             
             (format stream ";;~A~%" mark-at-lines-end))
           (return))
          ((and (numberp start-line-n)(> n start-line-n)
                (numberp end-line-n )(< n end-line-n))
           (setf all-selected-lines (append all-selected-lines (list line))))
           ;;(format stream "~A" mark-at-lines-start)                   
          (T nil))
         ;;end let,loop, with
         )))
             (when return-formated-string-p
               (setf formated-string 
                     (format-string-list all-selected-lines :line-width line-width 
                                         :add-newlines add-newlines
                                         :left-margin-spaces left-margin-spaces)))
    (values all-selected-lines start-line end-line file-pos-start-line file-pos-end-line formated-string)
    ;;end let,get-file-lines
    ))
;;TEST
;; (get-file-lines "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\cs-memory-test.lisp" 5 11)
;;works= ("(defparameter *memtest-list2" "  '(hat hand dog mailbox egg hammer lightbulb needle onion can))" "(defparameter *memtest-list3 nil)" "(defparameter *memtest-list4 nil)" "(defparameter *memtest-list5 nil)" "" ";;RESULTS")         "(defparameter *memtest-list2"     ";;RESULTS"    210   394
;; (get-file-lines "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\cs-memory-test.lisp" 5)
;;works= ("(defparameter *memtest-list2")   "(defparameter *memtest-list2"  NIL  210 NIL

;;1 MARK
;; (get-file-lines "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\cs-memory-test.lisp" 5 NIL :mark-at-lines-start "MARK")
;;2 MARKS
;; (get-file-lines "C:\\3-TS\\LISP PROJECTS TS\\CogSys-Model\\cs-memory-test.lisp" 5 12 :mark-at-lines-start "MK1" :mark-at-lines-end "MK2")
;;works= ("(defparameter *memtest-list2" "" "box egg hammer lightbulb needle onion can))" "(defparameter *memtest-list3 nil)" "(defparameter *memtest-list4 nil)" "(defparameter *memtest-list5 nil)" "" ";;MK2")
;;"(defparameter *memtest-list2"
;;";;MK2"    210    378





;;COUNT-FILE-LINES
;;2020
;;ddd
(defun count-file-lines (pathname &key (start-line 1) end-line                              
                                  (file-max-lines 25000))
  "U-files. Counts file lines"
  (let*
      ((n-lines 0)
       (count-lines-p)
       )
    (with-open-file (instr pathname :direction :input)
      (loop
       for n from 1 to file-max-lines
       do
       (let*
           ((line (read-line instr nil 'eof))
            )
         (when  (equal line 'eof)
           (return))
         (cond
          ((= start-line n)
           (setf count-lines-p T))
          ((and (numberp end-line)
                (= end-line n))
           (setf count-lines-p NIL))
          (count-lines-p
           (incf n-lines))
          (T nil))
         ;;end let,loop,with
         )))
    n-lines
    ;;end let,count-file-lines
    ))
;;TEST
;; (count-file-lines "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-math.lisp")
;; works= 2393
;; (count-file-lines "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-math.lisp" :start-line 75) = 2317
;; (count-file-lines "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-math.lisp" :start-line 75 :end-line 2100) = 2024



;;LIST-BUFFER-UNWRITEABLE-CHARS -- SSS FIX??
;;2020
;;ddd
(defun list-buffer-unwriteable-chars (buffer-name)
  "U-file&buffers   RETURNS unwriteable-chars   INPUT:  "
  (let*
      ((buffer-obj (editor:get-buffer buffer-name))
       (unwriteable-chars) ;; (editor::list-unwritable-characters-command buffer-obj))
       (info
    (format nil  " THIS FUNC NEEDS FIXING-USE EDITOR COMMAND?
 ==>  FROM LISPWORKS:
3.5.3.2 UNWRITABLE CHARACTERS
**There are two ways to resolve this:
    1. Set the external format to one which includes char, or
    2. Delete char from the buffer before saving. 
You may want a file which is Unicode UTF-16 encoded (external format :unicode), UTF-8 encoding (:utf-8) or a language-specific encoding such as :shift-jis or :gbk. Or you may want a Latin-1 encoded file, in which case you could supply :latin-1-safe.

USE Editor Command: FIND UNWRITABLE CHARACTER
Arguments: None  Key sequence: None
Finds the next occurrence of a character in the current buffer that cannot be written using the buffer external format. The prefix argument is ignored.
--OR--
USE Editor Command  LIST UNWRITABLE CHARACTERS
Arguments: None Key sequence: None
Lists the characters in the current buffer that cannot be written with the buffer external format. The prefix argument is ignored."))
       )
    (values info unwriteable-chars)
    ;;end let, list-buffer-unwriteable-chars
    ))
;;TEST
;; (list-buffer-unwriteable-chars "U-file&buffers.lisp")