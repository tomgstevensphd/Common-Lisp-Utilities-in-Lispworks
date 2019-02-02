;;**************************** U-photos.lisp ************************
;;
;;
;;
;;sss  start here
;;
(in-package "CL-USER")

;;ddd
;;
;;both work
;;( find-exif-date-time   "C:\\TOM\\LISP PROJECTS TS\\screensaver\\test-photo1.jpg" :delete-seconds t ) = "2012:11:19" "19:15"
;;( find-exif-date-time   "C:\\TOM\\LISP PROJECTS TS\\screensaver\\test-photo1.jpg" ) = "2012:11:19" "19:15"

;; PROBLEM WITH ERROR ON CAN'T READ EXIF STREAM ON THIS FILE:
;;  ( find-exif-date-time "C:\\OUR PARTIAL PHOTOS-VIDEOS\MY PHOTOS\\0-Our HEALTH IMAGES\Sherry MRI complete brain-eyes 12-8-07 Eisenhower MC\\data\ali\\Controls\\next.jpg":delete-seconds t ) ;; WORKS WHEN DONE ALONE, CRASHES IN SCREENSAVER
;;
(defun find-exif-date-time (jpg-pathname &key delete-seconds)
  "In U-photos.lisp, for jpg photo-file only, uses zpb-exif library in U-photo-info, returns values date and time"
  (let
      ((path jpg-pathname)
       (exif)
       (DateTimeOriginal "Not Available")
       (date "Not Available ")
       (time "Not Available ")
       (file-type (pathname-type jpg-pathname))
       )
    (cond
     ((and (probe-file jpg-pathname) 
           (member file-type '("jpg" "JPG" "jpeg" "JPEG") :test 'equal))
      ;;make exif object from file   
      (cl-user::afout 'out (format nil "In find-exif-date-time, path=~A~%" path))
      (setf exif (zpb-exif:make-exif path)) 
      (cl-user::afout 'out (format nil "In find-exif-date-time, exif=~A~%" exif)))
     (t (setf date "Not Available (not JPG file)"
               time "Not Available (not JPG file)")))
    (cond
     (exif 
      ;;use zpb-exif library to find date-time string
      (setf DateTimeOriginal (zpb-exif:exif-value :DateTimeOriginal exif ))
      ;;following prevents error from searching with index = nil
      (cond
       ((null DateTimeOriginal) 
        (setf DateTimeOriginal "N/A N/A     "))
        ;;separate date and time using my utility function
        (t 
         (cond
          (delete-seconds
           (setf time 
                 (match-subseq '#\SPACE DateTimeOriginal :trim-items 1 :trim-end-items 3)))
          (t  (setf time 
                    (match-subseq '#\SPACE DateTimeOriginal :trim-items 1))))
         (setf date
               (match-subseq 0 DateTimeOriginal
                             :end-item '#\space))
         )))
      (t (setf date "Not Available (no JPG data?)"
               time "Not Available (no JPG data?)")))

    (values date time)))

;;was
#|(defun find-exif-date-time (jpg-pathname &key delete-seconds)
  "In U-photos.lisp, for jpg photo-file only, uses zpb-exif library in U-photo-info, returns values date and time"
  (let
      ((path jpg-pathname)
       (exif)
       (DateTimeOriginal "Not Available")
       (date "Not Available ")
       (time "Not Available ")
       (file-type (pathname-type jpg-pathname))
       )
    (cond
     ((and (probe-file jpg-pathname) 
           (member file-type '("jpg" "JPG" "jpeg" "JPEG") :test 'equal))
      ;;make exif object from file   
      (cl-user::afout 'out (format nil "In find-exif-date-time, path=~A~%" path))
      (setf exif (zpb-exif:make-exif path)) 
      (cl-user::afout 'out (format nil "In find-exif-date-time, exif=~A~%" exif))
      ;;added

      ;;was exif    
      ;;use zpb-exif library to find date-time string
      (setf DateTimeOriginal (zpb-exif:exif-value :DateTimeOriginal exif ))
      ;;following prevents error from searching with index = nil
      (cond
       ((null DateTimeOriginal) 
        (setf DateTimeOriginal "N/A N/A     "))
        ;;separate date and time using my utility function
        (t 
         (cond
          (delete-seconds
           (setf time 
                 (match-subseq '#\SPACE DateTimeOriginal :trim-items 1 :trim-end-items 3)))
          (t  (setf time 
                    (match-subseq '#\SPACE DateTimeOriginal :trim-items 1))))
         (setf date
               (match-subseq 0 DateTimeOriginal
                             :end-item '#\space))
         )))
      (t (setf date "Not Available (no JPG data?)"
               time "Not Available (no JPG data?)")))

    (values date time)))|#

;;  (defun match-subseq (item seq &key  end-item trim-items trim-end-items match-all-from-end )    




#|
(defparameter *exif* (zpb-exif:make-exif #p"C:\\TOM\\LISP PROJECTS TS\\screensaver\\test-photo1.jpg"))                       ;;#p"gg_gps.JPG"))
;;=> *EXIF*
|#

(defun texif ()
  (let ((FNumber (zpb-exif:EXIF-VALUE "FNumber" zpb-exif::*exif*))
        ;;=> 9/2
        (MeteringMode (zpb-exif:parsed-exif-value "MeteringMode" *exif*))
        ;;=> :PATTERN
        (DateTimeOriginal (zpb-exif:exif-value :DateTimeOriginal *exif*))
        ;;=> "2003:11:23 18:07:37"
        ;; (zpb-exif:exif-value #x0112 *exif*)   ; #x0112 => orientation tag in the image IFD
        ;;=> 6
        ;; (zpb-exif:parsed-exif-value #x0112 *exif*)
        ;;=> :ROTATED-270
        (ISOSpeedRating (zpb-exif:exif-value :ISOSpeedRating *exif*))
        )
   ; Field not present in this particular image ;;=> NIL

    #| (list (zpb-exif:exif-value "GPSLatitude" *exif*)
        (zpb-exif:exif-value "GPSLatitudeRef" *exif*)
        (zpb-exif:exif-value "GPSLongitude" *exif*)
        (zpb-exif:exif-value "GPSLongitudeRef" *exif*))|#
    ;;=> (#(39 54 56) "N" #(116 23 27) "E")

    (values FNumber MeteringMode DateTimeOriginal ISOSpeedRating)
    ))


 ;;EXIF-ALIST exif &key parsedp => alist
;;
;;(zpb-exif:exif-alist *exif* :parsedp t)
;;
;;IFD-ALIST exif &key parsedp => alist
;;
;;(zpb-exif:ifd-alist *exif* :parsedp t)
