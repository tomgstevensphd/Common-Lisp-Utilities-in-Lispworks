;;********************************* U-Input-output.lisp *****************************
;;
;; OPEN ARGS
#|direction---one of :INPUT, :OUTPUT, :IO, OR :PROBE. The default is :input.

ELEMENT-TYPE---a type specifier for recognizable subtype of character; or a type specifier for a finite recognizable subtype of integer; or one of the symbols signed-byte, unsigned-byte, or :default. The default is character.

IF-EXISTS---one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE, :OVERWRITE, :APPEND, :SUPERSEDE, OR NIL. The default is :new-version if the version component of filespec is :newest, or :error otherwise.

IF-DOES-NOT-EXIST---one of :error, :create, or nil. The default is :error if direction is :input or if-exists is :overwrite or :append; :create if direction is :output or :io, and if-exists is neither :overwrite nor :append; or nil when direction is :probe.

EXTERNAL-FORMAT---an external file format designator. The default is :default.
If direction is :input or :probe, or if if-exists is not :new-version and the version component of the filespec is :newest, then the file opened is that file already existing in the file system that has a version greater than that of any other file in the file system whose other pathname components are the same as those of filespec.
|#
;;
;;
;;READ-FILE-LINES
;;ddd
(defun read-file-lines (stream &key incl-newline)
  "In U-Input-output.lisp, reads an entire file and returns it as a single string"
  (let
      ((file-string "")
       )
    (loop
     with line
     do
     (setf  line (read-line stream nil :eof)) 
     (when (equal line :eof)
       (return))
     (cond
      (incl-newline
       (setf file-string (format nil "~A~%~A" file-string line)))
      (t 
       (setf file-string (format nil "~A~A" file-string line)))))
    file-string))
;;
;;test -- works
#|(defun testfl ()
  (setf out nil)
  (let
      ((file "C:\\TOM\\My Webs Exp\\~tstevens\\index.html")
       (file-string)
       )
    (with-open-file (stream file :direction :input) 
   (setf file-string (read-file-lines stream   :incl-newline t))
   (afout 'out (format nil "file-string= ~A~%" file-string))
   )))|#


;;READ-FILE-CHARS
;;
;;ddd
(defun read-file-chars  (stream &key incl-newline)
  "In U-Input-output.lisp, reads an entire file by chars? and returns it as a single string"
  (let
      ((file-string)
       )
    #|(format stream "GET / HTTP/1.0~C~C~C~C"
            (code-char 13) (code-char 10)
            (code-char 13) (code-char 10))|#
  ;;  (force-output stream)
  ;;  (write-string "Waiting to reply...")
    (loop for ch = (read-char-no-hang stream nil :eof)
          until ch
          do (write-char #\.)
          (sleep 0.25)
          finally (unless (eq ch :eof)
                    (unread-char ch stream)))
    (terpri)
    (loop for line = (read-line stream nil nil)
          while line
         ;;was do (write-line line)
         do
         (cond
      (incl-newline
       (setf file-string (format nil "~A~%~A" file-string line)))
      (t 
       (setf file-string (format nil "~A~A" file-string line))))
          )
    file-string
    ))
;;test
#|(defun testfc ()
  (setf out nil)
  (let
      ((file "C:\\TOM\\My Webs Exp\\~tstevens\\index.html")
       (file-string)
       )
    (with-open-file (stream file :direction :input) 
   (setf file-string (read-file-chars stream   :incl-newline t))
   (afout 'out (format nil "file-string= ~A~%" file-string))
   )))|#


;;MOVED TO U-FILES --very versitle and robust
;;WRITE-TO-FILE
;;2017
;;ddd
#|(defun write-to-file (filename objects &key (if-exists :append) 
                               (if-does-not-exist  :create)
                               (element-type :default) (direction :output) 
                               (pretty *print-pretty*) 
                               incl-nils-p  (newline-after-objects-p T)(final-newline-p T)
                               (array *print-array*) (base *print-base*)  
                               (case *print-case*) (circle *print-circle*) 
                               (escape *print-escape*) (gensym *print-gensym*) 
                               (length *print-length*) (level *print-level*)  
                               (lines *print-lines*) (miser-width *print-miser-width*) 
                               (pprint-dispatch *print-pprint-dispatch*) 
                               (radix *print-radix*)(readably *print-readably*)  
                               (right-margin *print-right-margin*) )
  "In U-Input-output  USE FOR PPRINT TO A FILE,etc. plus lots more varisions using print.  U-Input-output  RETURNS    INPUT:  (if incl-nils-p= NIL, omits NILs) Flexible, works well."
  (let
      ((stream)
       (n-objects 0)
       )
    (with-open-file (stream filename :if-exists  if-exists  :if-does-not-exist if-does-not-exist
                            :element-type  element-type  :direction direction)

      (loop
       for object in objects
       do
       (when (or object incl-nils-p)
         (write  object  :stream stream :array array :base base
                 :case case :circle circle 
                 :escape escape :gensym gensym 
                 :length length :level level  
                 :lines lines :miser-width miser-width 
                 :pprint-dispatch pprint-dispatch :pretty pretty 
                 :radix radix :readably readably  
                 :right-margin right-margin)

         (when newline-after-objects-p
           (format stream "~%")) 

         (incf n-objects)
       
         ;;(break "here")
         ;;end while, loop
         ))
      (when final-newline-p
        (format stream "~%"))
      ;;end with-open
      )
    (format nil ">> N-OBJECTS= ~A WRITTEN TO FILE=> ~A (pprint= ~A)" 
            n-objects filename pretty)
    ;;end let, write-to-file
    ))|#
;;TEST
;;(setf testwobj '(list '(first (a b c (1 2 3 (4 5 6 (7 8 9)(10 11 12) h i j) k l) next-last) last)))
;; (WRITE-TO-FILE   "c:/temp/my-write-test2.lisp" `( (setf testwobj ,(eval testwobj))))
;; (WRITE-TO-FILE   "c:/temp/my-write-test2.lisp" `(this (setf testwobj ,(eval testwobj))) :pretty T)



;;NOTE FOR WRITE PARAMETERS
#|write is the general entry point to the Lisp printer. For each explicitly supplied keyword parameter named in the next figure, the corresponding printer control variable is dynamically bound to its value while printing goes on; for each keyword parameter in the next figure that is not explicitly supplied, the value of the corresponding printer control variable is the same as it was at the time write was invoked. Once the appropriate bindings are established, the object is output by the Lisp printer.

PARAMETER        CORRESPONDING DYNAMIC VARIABLE  
array            *print-array*                   
base             *print-base*                    
case             *print-case*                    
circle           *print-circle*                  
escape           *print-escape*                  
gensym           *print-gensym*                  
length           *print-length*                  
level            *print-level*                   
lines            *print-lines*                   
miser-width      *print-miser-width*             
pprint-dispatch  *print-pprint-dispatch*         
pretty           *print-pretty*                  
radix            *print-radix*                   
readably         *print-readably*                
right-margin     *print-right-margin*  |#



    
                                       






;;HHH USE OF INPUT AND OUTPUT TO STRINGS

#|(with-output-to-string (out)
  (format out "hello, world ")
  (format out "~s" (list 1 2 3)))
|#
;;HHH USE OF ECHO-STREAMS, make-string-output-stream, ETC.
;; 
;;
  ;; (setf *test-out-string (make-string-output-stream))
;;  = #<SYSTEM::STRING-OUTPUT-STREAM 200914F7>
;; (setf *test-echo (make-echo-stream *standard-output* *test-out-string))
;; = #<Echo Stream Input = #<EDITOR::RUBBER-STREAM #<EDITOR:BUFFER CAPI interactive-pane 78> 2B7275F3>, Output = #<SYSTEM::STRING-OUTPUT-STREAM 200914F7>>
;; (format t "TEST OUT")
;; (get-output-stream-string *test-out-string)
;; (echo-stream-output-stream *test-echo)  = #<SYSTEM::STRING-OUTPUT-STREAM 200914F7>

  ;; (setf *test-in-string "        "   *test-in-string2 (make-string-input-stream *test-in-string))
;;#<SYSTEM::STRING-INPUT-STREAM 2F86FDBB>

;; (format t "TEST OUT")
;;

;; (format *test-out-string "FORMAT TEST OUT")
;; (get-output-stream-string *test-out-string) = "FORMAT TEST OUT"
;; NOTE: WITH EACH READ, get-output-stream-string RESETS STRING TO ""

;; (format *standard-output* "FORMAT STANDAND-OUTPUT")
;; = FORMAT STANDAND-OUTPUT   NIL
;; (get-output-stream-string *test-out-string) = "" ;;DOESN'T WORK on *standard-output*

;; (format *TEST-ECHO "FORMAT STANDAND-OUTPUT3")
;;      = NIL
;; ;;works when use ECHO stream= (get-output-stream-string *test-out-string) = "FORMAT STANDAND-OUTPUT3"


;; (get-get-output-stream-string *test-out-string) = "" 
