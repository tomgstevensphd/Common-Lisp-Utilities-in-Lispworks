;;********************************* U-Comm.lisp ****************************
;;
;;
(require  "comm")

(defun read-tcp-file (host service)
  (with-open-stream (http (comm:open-tcp-stream host service))
                          ;;was "www.lispworks.com" 80))
    (format http "GET / HTTP/1.0~C~C~C~C"
            (code-char 13) (code-char 10)
            (code-char 13) (code-char 10))
    (force-output http)
    (write-string "Waiting to reply...")
#|    (loop for ch = (read-char-no-hang http nil :eof)
          until ch
          do (write-char #\.)
          (sleep 0.25)
          finally (unless (eq ch :eof)
                    (unread-char ch http)))|#
    (terpri)
   (loop for line = (read-line http nil nil)
          while line
          do (write-line line))
    ))
;;test works
(defun testrtf ()
  (setf out nil)
  (let
      ((host "web.csulb.edu")                         ;; "www.lispworks.com")
       (service 80)
       )
    (read-tcp-file host service)
    ))

#|
RETURNS FOR WWW.LISPWORKS.COM
CL-USER 2 > (testrtf)
Waiting to reply...
HTTP/1.1 301 Moved Permanently
Date: Mon, 06 Jan 2014 02:08:54 GMT
Server: Apache/2.2.22 (Unix) mod_ssl/2.2.22 OpenSSL/1.0.1c mod_apreq2-20051231/2.6.0 mod_perl/2.0.5 Perl/v5.8.9
Location: http://www.lispworks.com/
Content-Length: 409
Connection: close
Content-Type: text/html; charset=iso-8859-1

<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
<html><head>
<title>301 Moved Permanently</title>
</head><body>
<h1>Moved Permanently</h1>
<p>The document has moved <a href="http://www.lispworks.com/">here</a>.</p>
<hr>
<address>Apache/2.2.22 (Unix) mod_ssl/2.2.22 OpenSSL/1.0.1c mod_apreq2-20051231/2.6.0 mod_perl/2.0.5 Perl/v5.8.9 Server at lispworksvpsv3.veriovps.co.uk Port 80</address>
</body></html>
NIL

CL-USER 3 > (testrtf)
RETURNS FOR WEB.CSULB.EDU
Waiting to reply...
HTTP/1.1 302 Found
Date: Mon, 06 Jan 2014 02:09:57 GMT
Server: Apache/2.0.63
Location: http://web.csulb.edu
Content-Length: 204
Connection: close
Content-Type: text/html; charset=iso-8859-1

<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
<html><head>
<title>302 Found</title>
</head><body>
<h1>Found</h1>
<p>The document has moved <a href="http://web.csulb.edu">here</a>.</p>
</body></html>
NIL
|#


#|
(defun read-tcp-file (host service)
  (with-open-stream (http (comm:open-tcp-stream host service))
                          ;;was "www.lispworks.com" 80))
    (format http "GET / HTTP/1.0~C~C~C~C"
            (code-char 13) (code-char 10)
            (code-char 13) (code-char 10))
    (force-output http)
    (write-string "Waiting to reply...")
    (loop for ch = (read-char-no-hang http nil :eof)
          until ch
          do (write-char #\.)
          (sleep 0.25)
          finally (unless (eq ch :eof)
                    (unread-char ch http)))
    (terpri)
    (loop for line = (read-line http nil nil)
          while line
          do (write-line line)))
  )
;;test works
(defun testrtf ()
  (setf out nil)
  (let
      ((host "www.lispworks.com")
       (service 80)
       )
    (read-tcp-file host service)
    ))
|#
    

;;SERVICE on Windows NT-based systems it is the file
;;%SystemRoot%\\system32\\drivers\etc\\SERVICES
;;me- services is actually located at  C:\Windows\System32\drivers\etc\services

;;SSS  START DEBUGGING -- WHY DOES ABOVE CONNECT AND WORK AND THIS DOESN'T???
;;MY-CONNECT
;;ddd
(defun my-connect (host &key port read-type)
  "In U-Comm.lisp, connects to a host and reads entire page into a string unless no-read is t"
  (let
      (;;not needed (socket-handle)
      (service "%SystemRoot%\\system32\\drivers\etc\\SERVICES") ;;can also be a port number
       (socket-stream)
       (host-ipv4s)
       (host-ipv6s)
       (host-name)
       (host-aliases)
       (socket-addr)
       (socket-port)
       (file-string)
       )
    (if port 
        (setf service port))

    (with-open-stream (http-stream (comm:open-tcp-stream host service))
                          ;;was "www.lispworks.com" 80))
            
  (format http-stream "GET / HTTP/1.0~C~C~C~C"
            (code-char 13) (code-char 10)
            (code-char 13) (code-char 10))
    (force-output http-stream)
    (write-string "Waiting to reply...")
#|
    (loop for ch = (read-char-no-hang http-stream nil :eof)
          until ch
          do (write-char #\.)
          (sleep 0.25)
          finally (unless (eq ch :eof)
                    (unread-char ch http)))
    (terpri)
    (loop for line = (read-line http-stream nil nil)
          while line
         ;;was do (write-line line)
         do
         (setf file-string (format nil "~A~%~A" file-string line))
          )
  |#

   ;;put inside (with-open-stream as above (setf socket-stream (comm:open-tcp-stream host service))
    #| a longer way -- instead of using comm:open-tcp-stream
    (setf socket-handle
          (comm:connect-to-tcp-server host "%SystemRoot%\\system32\\drivers\etc\\SERVICES"))
     (setf socket-stream (make-instance 'comm:socket-stream
                                       :socket socket-handle :direction :io element-type 'base-char))|#

    (multiple-value-setq (host-ipv4s  host-ipv6s host-name host-aliases)
                           (comm:get-host-entry host :fields '(:ipv4-addresses  :ipv6-addresses :name :aliases)))
  ;;causes error  (multiple-value-setq (socket-addr socket-port)
  ;;                           (comm:get-socket-address socket-stream))


        (terpri)
   (loop for line = (read-line http-stream nil nil)
          while line
          do   ;;was (write-line line)
          (setf file-string (format nil "~A~%~A" file-string line))
          )

#| SHOULD WORK BUT DOESN'T SEEM TO
   (cond
      ((equal read-type 'char)
      (setf file-string (read-file-chars socket-stream :incl-newline t)))
      ((equal read-type 'none) nil)
      (t
      (setf file-string (read-file-lines socket-stream :incl-newline t))))|#

#|    (afout 'out (format nil "In my-connect, socket-stream= ~A~% host-ipv4s= ~A~% host-ipv6s= ~A~% host-name= ~A~% host-aliases= ~A~% socket-addr= ~A~% socket-port= ~A~%"  socket-stream host-ipv4s host-ipv6s host-name host-aliases socket-addr socket-port))|#
    ;;end with-open
    )
    (values file-string (format nil "In my-connect, socket-stream= ~A~% host-ipv4s= ~A~% host-ipv6s= ~A~% host-name= ~A~% host-aliases= ~A~% "  socket-stream host-ipv4s host-ipv6s host-name host-aliases))  ;;was socket-addr socket-port))
    ;;end with-open
    ))

#| host = "web.csulb.edu" prints
"In my-connect, socket-stream= NIL
 host-ipv4s= (2257256764)
 host-ipv6s= NIL
 host-name= gaggle.its.csulb.edu
 host-aliases= (web.csulb.edu)
If set host = "gaggle.its.csulb.edu", get:
"In my-connect, socket-stream= NIL
 host-ipv4s= (2257256764)
 host-ipv6s= NIL
 host-name= gaggle.its.csulb.edu
 host-aliases= NIL|#

;;test
(defun testmc ()
  (setf out nil)
  (let
      ((host "web.csulb.edu") ;; "gaggle.its.csulb.edu") ;;  ;;gaggle.its.csulb.edu\\index.html") ;; "www.csulb.edu")
;;SSS SPECIFIC FILE ADDRESSES DON'T WORK-- HOW TO GET THEM???
;;\\index.html" ) ;;\~tstevens") ;;success\rtfs\academic.htm") ;; "www.lispworks.com") ;; "www.csulb.edu\\~tstevens\\index.html")  ;;try  "www.csulb.edu\\~tstevens")
       ;;"web.csulb.edu\\~tstevens\\success\\rtfs\\academic.htm"
       (read-type 'line)
       (page)
       (connection)
       )
    (multiple-value-setq (page  connection) 
        (my-connect host :port 80 :read-type read-type))
    (values page connection)
    ))


;;TEST 3-2015 USING WEB.CSULB.EDU AS URL
#|
CL-USER 3 > (testmc)
Waiting to reply...
"NIL
HTTP/1.1 302 Found
Date: Thu, 26 Mar 2015 23:56:24 GMT
Server: Apache/2.0.63
Location: http://www.csulb.edu/colleges/cnsm/
Content-Length: 219
Connection: close
Content-Type: text/html; charset=iso-8859-1

<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>302 Found</title>
</head><body>
<h1>Found</h1>
<p>The document has moved <a href=\"http://www.csulb.edu/colleges/cnsm/\">here</a>.</p>
</body></html>"
"In my-connect, socket-stream= NIL
 host-ipv4s= (2257261361)
 host-ipv6s= NIL
 host-name= gaggle.its.csulb.edu
 host-aliases= (web.csulb.edu)
 "
|#


;;TEST 3-2015 using www.csulb.edu as url
#|
Waiting to reply...
"NIL
HTTP/1.1 404 Not Found
Accept-Ranges: bytes
Age: 0
Content-Type: text/html
Date: Thu, 26 Mar 2015 20:20:03 GMT
ETag: \"54ca3eda-3cb\"
Server: nginx
Via: 1.1 varnish
X-Cache: MISS
X-Varnish: 1661719795
Content-Length: 971
Connection: Close

<html>
<head>
<title>Web Site Not Found</title>
<style type=\"text/css\">
  A:link {text-decoration: none;  color: #333333;}
  A:visited {text-decoration: none; color: #333333;}
  A:active {text-decoration: none}
  A:hover {text-decoration: underline;}
</style>
</head>
<body bgcolor=white text=#333333 style=\"padding:5px 15px 5px 15px; font-family: myriad-pro-1,myriad-pro-2,corbel,sans-serif;\">

<H3>Web Site Not Found</H3>

<p>Sorry, we could not find any content for this web address.  Please check the URL.</p>
<p>If you are an Acquia Cloud customer and expect to see your site at this address, you'll need to add 
this domain name to your site via the <a href=\"http://network.acquia.com\">Acquia Network</a>
management console.</p>


<p style=\"color:#999999; font-size: 75%;\">
<a href=\"http://www.acquia.com\">Acquia Inc.</a>
</p>
<!-- Acquia scripts look for the following keyword to confirm the initial site install: ACQUIA_DEFAULT_HTML_INSTALLED -->
</body>
</html>"
"In my-connect, socket-stream= NIL
 host-ipv4s= (919998207 921992851)
 host-ipv6s= NIL
 host-name= mc-8238-1321475763.us-west-2.elb.amazonaws.com
 host-aliases= (www.csulb.edu)
 "

CL-USER 2 > 
|#
