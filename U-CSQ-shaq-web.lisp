;;*********************** U-CSQ-SHAQ-web.lisp *****************************
;;
;;LOAD DRAKMA FIRST IF NEEDED
;;1. LOAD QUICKLISP -- USED TO LOAD DRAKMA
(unless (packagep 'drakma)
 (load "C:\\3-TS\\LISP PROJECTS TS\\CL-Utilities\\Quicklisp\\my-quicklisp-load.lisp")
;;2. LOAD DRAKMA
 (ql:quickload :drakma)
;;USES DRAKMA FUNCTIONS
;;(unless (functionp 'drakma:http-request)
  (load "C:\\3-TS\\LISP PROJECTS TS\\MyLispWebs\\my-drakma-load.lisp")
;;end unless
  )

;;MY-POST
;;
(defun my-post (url &key key value  post-parameter-list)
  "In U-web.lisp, post-parameter-list form '((key1 . value1) (key2 . value2) etc) plus content-type etc can be included in parameter list--READ DRAKMA INFO"
  (let
      ((v1-body-or-stream)
       (v2-status-code)
       (v3-headers)
       (v4-newURI)
       (v5-stream)
       (v6-must-close)
       (v7-reason-phrase)
       (parameter-list)
       )
    (cond
     (post-parameter-list
      (setf parameter-list post-parameter-list))
     (t (setf parameter-list `((,key . ,value)))))

      (multiple-value-setq (v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase v8)
          ;;  (drakma:http-request url))
          (drakma:http-request url   ;; "http://www.phpsecurepages.com/test/test.php"
                               :method :post
                               :parameters post-parameter-list
                               ))
      
      ;;(afout 'out (format nil "In http-dcon, v1-body-or-stream= ~A~% v2-status-code= ~A~% v3-headers= ~A~% v4-uri3-newURI= ~A~% v5-stream= ~A~% v6-must-close= ~A~% v7-reason-phrase= ~A~% "  v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase ))
    (values v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase v8)
    ))



;;MY-GET    
;;
(defun my-get (url)
  "In U-web.lisp, can add more variations LATER--see DRAKMA INFO"
;;(setf out nil)
  (let
      ((v1-body-or-stream)
       (v2-status-code)
       (v3-headers)
       (v4-newURI)
       (v5-stream)
       (v6-must-close)
       (v7-reason-phrase)
       )
   
    (multiple-value-setq (v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase v8)
        (drakma:http-request url :method :get)) ;;added 2014-7
#| WORKS      (drakma:http-request url   ;; "http://www.phpsecurepages.com/test/test.php"
                             :method :post
                             :parameters '(("dataText" .  "This is the drakma post dataText data")) 
                             ))|#
    ;;(afout 'out (format nil "In http-dcon, v1-body-or-stream= ~A~% v2-status-code= ~A~% v3-headers= ~A~% v4-uri3-newURI= ~A~% v5-stream= ~A~% v6-must-close= ~A~% v7-reason-phrase= ~A~% "  v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase ))
  ;;(fout out)
  (values v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase v8)
    ))
;;TEST
#|(defun testmg ()
  (let
      ((url "http://web.csulb.edu/~tstevens/index.html")
       )
    (my-get url)
    ))|#
;;WORKS--FOR OUTPUT, GO TO U-web-test-output.lisp


;;SEND-SHAQ-DAT
;;
;;ddd
(defun send-shaq-dat (&key path  (csq-data-list *CSQ-DATA-LIST)
                           (shaq-scaledata-list *shaq-scaledata-list))
  "In U-web.lisp, "
  (setf out nil)
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time (get-universal-time))
    (let*
        ((url "http://web.csulb.edu/~tstevens/test/SendData.php")
         ;;find date and time
         (time (format nil "~a:~a" hour min))
         (date (format nil "~a.~a.~a" month day year))
         ;; "http://web.csulb.edu/~tstevens/test/sendtest1.php")
         ;;"http://web.csulb.edu/~tstevens/test/SendData.php")
         ;;"http://web.csulb.edu/~tstevens/success/hidden/dataFile.txt")
         ;; "http://web.csulb.edu/~tstevens/success/rtfs/1jstk.htm") ;; "http://web.csulb.edu/~tstevens")
         ;; "http://web.csulb.edu/~tstevens/health%20tips.htm");;  "http://web.csulb.edu/~tstevens/success/")  ;; "http://web.csulb.edu")
         ;;body-or-stream, status-code, headers, uri, stream, must-close, reason-phrase
         (shaq-alldata-text (format nil "~%;;;>>>DATE: ~A TIME: ~A~%~A"date time ;;was (list csq-data-list shaq-scaledata-list) printed NO quotes on strings
          (format nil "(~S ~S)" csq-data-list shaq-scaledata-list)
          ))
         (v1-body-or-stream)  ;;no effect? "THIS IS TEST XX")
         (v2-status-code)
         (v3-headers)
         (v4-newURI)
         (v5-stream)
         (v6-must-close)
         (v7-reason-phrase)
         ;; (shaq-data-text (format nil "~A" shaq-alldata-list))
         (parameter-list `(("dataText" .  ,shaq-alldata-text) ;;was "This is my DRAKMA test data 12345 10-22-2014 written from LISP")
                          ;; ("resultsText"  .  "RESULTS TEXT GOES HERE")
                          ;; ("resultsFilePath" .  "/resultsFilename.txt \NEWLINE"))
                         ))
         )
       
      (multiple-value-setq (v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase )
          (my-post url :post-parameter-list parameter-list)
        ;;was (http-dcon url)
        )
      ;;   (afout 'out (format nil "v1-body-or-stream= ~A~% v2-status-code= ~A~% v3-headers= ~A~% v4-uri3-newURI= ~A~% v5-stream= ~A~% v6-must-close= ~A~% v7= ~A~% v8= ~A~% "  v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase ))
      (values v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase v8)
      ;;(fout out)
      ;;end mvb,let, send-shaq-dat
      )))
;;TEST
;; (send-shaq-dat :csq-data-list '("This is SHAQ send-shaq-dat TEST DATA") :shaq-scaledata-list '("This is SHAQ send-data-dat SCALE DATA TEST DATA"))
;; 2017-11-10 RESULT-ERROR:
#|"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>500 Internal Server Error</title>
</head><body>
<h1>Internal Server Error</h1>
<p>The server encountered an internal error or
misconfiguration and was unable to complete
your request.</p>
<p>Please contact the server administrator at 
 webmaster@csulb.edu to inform them of the time this error occurred,
 and the actions you performed just before this error.</p>
<p>More information about this error may be available
in the server error log.</p>
</body></html>
"
500
((:DATE . "Fri, 10 Nov 2017 21:19:38 GMT") (:SERVER . "Apache/2.4.25 (Red Hat) OpenSSL/1.0.1e-fips") (:CONTENT-LENGTH . "532") (:CONTENT-TYPE . "text/html; charset=iso-8859-1") (:CONNECTION . "close") (:CONTENT-LANGUAGE . "en"))
#<PURI:URI http://web.csulb.edu/~tstevens/test/SendData.php>
#<FLEXI-STREAMS:FLEXI-IO-STREAM 200CCDE3>
T
"Internal Server Error"
NIL|#
;;END 2017-11-10 RESULT-ERROR:

;;  (send-shaq-dat)
;; WORKS: returns ALL data plus following:
#|((:DATE . "Thu, 23 Oct 2014 02:44:14 GMT") (:SERVER . "Apache/2.0.63") (:CONTENT-TYPE . "text/html") (:SET-COOKIE . "CSULB=76.93.224.249.1414032254871699; path=/; max-age=10368000; domain=.csulb.edu") (:CONNECTION . "close"))
#<PURI:URI http://web.csulb.edu/~tstevens/test/SendData.php>
#<FLEXI-STREAMS:FLEXI-IO-STREAM 2395EB87>
T
"OK"|#
;;HOWEVER, ERROR SOMEWHERE: THIS ALSO COMES BACK:
#|<b>Notice</b>:  Undefined variable: filename1 in <b>/home/dh/tstevens/htdocs/test/SendData.php</b> on line <b>52</b><br />
From myWrite; Success, wrote () to file ()From myWrite; post =  = ()<br />
<b>Notice</b>:  Undefined variable: filename1 in <b>/home/dh/tstevens/htdocs/test/SendData.php</b> on line <b>52</b><br />
From myWrite; Success, wrote (RESULTS TEXT GOES HERE) to file ()From myWrite; post = RESULTS TEXT GOES HERE = (RESULTS TEXT GOES HERE)WROTE POST5 TO sdatlist.lispEND OF WRITING TO FILE
    </BODY>
</HTML>"|# 




#|(defun testdc ()
  "In U-web.lisp, "
  (setf out nil)
  (let
      ((url "http://web.csulb.edu/~tstevens/test/SendData.php")
;; "http://web.csulb.edu/~tstevens/test/sendtest1.php")
;;"http://web.csulb.edu/~tstevens/test/SendData.php")
;;"http://web.csulb.edu/~tstevens/success/hidden/dataFile.txt")
;; "http://web.csulb.edu/~tstevens/success/rtfs/1jstk.htm") ;; "http://web.csulb.edu/~tstevens")
 ;; "http://web.csulb.edu/~tstevens/health%20tips.htm");;  "http://web.csulb.edu/~tstevens/success/")  ;; "http://web.csulb.edu")
;;body-or-stream, status-code, headers, uri, stream, must-close, reason-phrase
       (v1-body-or-stream)  ;;no effect? "THIS IS TEST XX")
       (v2-status-code)
       (v3-headers)
       (v4-newURI)
       (v5-stream)
       (v6-must-close)
       (v7-reason-phrase)
       (parameter-list '(("dataText" . "This is my DRAKMA test data 12345 10-22-2014
                         written from LISP")("resultsText"  .  "This is my DRAKMA #newline test results text 999~%99")
                         ("resultsFilePath" .  "/resultsFilename.txt /NEWLINE")))
       )
    (multiple-value-setq (v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase )
       (my-post url :post-parameter-list parameter-list)
       ;;was (http-dcon url)
      )
 ;;   (afout 'out (format nil "v1-body-or-stream= ~A~% v2-status-code= ~A~% v3-headers= ~A~% v4-uri3-newURI= ~A~% v5-stream= ~A~% v6-must-close= ~A~% v7= ~A~% v8= ~A~% "  v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase ))
(values v1-body-or-stream v2-status-code v3-headers v4-newURI v5-stream v6-must-close v7-reason-phrase v8)
 (fout out)
    ))|#
;;TEST
;; THIS WORKS




;;*shaq-alldata-list

