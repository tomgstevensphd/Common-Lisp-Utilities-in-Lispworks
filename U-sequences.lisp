;;******************************* U-sequences.lisp ****************************
;;
;;
;;
(in-package "CL-USER")


;;CONVERT-SEQ-TO-LIST
;;
;;ddd
(defun convert-seq-to-list (seq)
  "In U-sequences RETURNS (values list n-seq) for any sequence incl string, single-dim array, and vectors."
   (when (sequencep seq)
  (let
      ((n-seq (length seq))
       (item)
       (list)
       )
    (loop
     for n from 0 to (- n-seq 1)
     do
     (setf item (elt seq n)
           list (append list (list item)))
     ;;end loop
     )
   (values list n-seq)
  ;;end let, when, convert-seq-to-list
  )))
;;TEST
;; (convert-seq-to-list "THIS IS A STRING")
;; works= (#\T #\H #\I #\S #\Space #\I #\S #\Space #\A #\Space #\S #\T #\R #\I #\N #\G)   16
;; (setf *ar6 (make-array 5 :initial-contents '(1 2 3 4 5)))  = #(1 2 3 4 5)
;; (convert-seq-to-list *ar6)
;; works=  (1 2 3 4 5)    5
;; (setf *ar5 (make-array 5 :initial-contents '(a b c d e)))  = #(A B C D E)
;; (convert-seq-to-list *ar5)
;; works= (A B C D E)   5






;;COERCE object result-type => result
;;Examples:
#| 
;;list to vector,string,
(coerce '(a b c) 'vector) =>  #(A B C)
(coerce '(a b c) 'string) => error
(string #\A) = "A"
;;symbol to char
 (coerce 'a 'character) =>  #\A
;;numbers
 (coerce 4.56 'complex) =>  #C(4.56 0.0)
 (coerce 4.5s0 'complex) =>  #C(4.5s0 0.0s0)
 (coerce 7/2 'complex) =>  7/2
 (coerce 0 'short-float) =>  0.0s0
 (coerce 3.5L0 'float) =>  3.5L0
 (coerce 7/2 'float) =>  3.5
;; cons
 (coerce (cons 1 2) t) =>  (1 . 2)
|#

(defun my-coerce-list-to-string (list &key include-parens-p)
  "In U-sequences.lisp, returns list as string with or without parens via key."
  (let
      ((item)
       (new-char)
       (new-string "")
       (list-length (list-length list))
       )
    ;;include the first parens in new-string?
    (if include-parens-p
        (setf new-string "("))

    ;;coerce each element/char into a string and add it to new-string
    (dotimes (n list-length)
      ;;note- dotimes... for each integer from 0 up to but not including the value of count-form,
      (setf item (nth n list)
            new-char (string (coerce item 'character ))
            new-string (format nil "~A~A" new-string new-char))
      )
    ;;include the last parens in new-string?
    (if include-parens-p
        (setf new-string (format nil "~A)" new-string)))
    new-string))
;;test, works   returns "(ABC)" if  include-parens-p T or "ABC" if  include-parens-p NIL
;;
(defun testls ()
  (let
      ((list '(a b c))
       (include-parens-p nil)
       )
  (my-coerce-list-to-string list :include-parens-p include-parens-p)))


;;MATCH-SUBSEQ
;;   For string to match all chars in an item to a substring in a string, 
;;           USE MATCH-SUBSTRING (In U-tstring.lisp)
;;
;;ddd
(defun match-subseq (item seq &key end-item trim-items
                          trim-end-items match-all-from-end )
  "In U-sequences.lisp, returns a sub-sequence. end-item specifies end-item in sequence (nil or chars/string; trim-items (integer) indicates to trim from beginning. trim-end-items (integer) indicates to trim from end; match-all-from-end (boolean) indicates that both beginning and end items are matched starting from the seq end. Set item to 0 or nil to begin at seq beginning.NOTE: MATCH-SUBSTRING WORKS BETTER FOR STRINGS??"
;;  (afout 'out (format nil ">>>> ARGS~% item=~A~% seq=~A~%  end-item=~A~% trim-items=~A~% trim-end-items=~A~% match-all-from-end=~A~%"item seq  end-item trim-items trim-end-items match-all-from-end))
  (let ((matched-item-elt) 
        (matched-end-item-elt) 
        (subseq seq)
        )
    ;;Can set item to 0 or nil to begin at beginning
    (when (or (null item)(equal item 0))
      (setf matched-item-elt 0))
    ;; (when (or (null end-item)(equal end-item 0))
    ;;  (setf matched-end-item-elt (leng))

    ;;if NOT match-all-from-end
    (cond
     ((null match-all-from-end)
      (if (null matched-item-elt)
          (setf matched-item-elt (position item seq)))
      (setf subseq (subseq seq matched-item-elt))
      (when end-item
        (setf matched-end-item-elt (position end-item subseq))
        (setf subseq (subseq subseq 0 matched-end-item-elt)))
      ;;end first cond clause
      )
     ;;if match-all-from-end
     (match-all-from-end
      (cond
       ((null item)
        (setf matched-item-elt 0))
       (t (setf matched-item-elt (position item seq :from-end t))))
    ;;(afout 'out (format nil "==> PRE BEGIN ANEW **~% subseq= ~A~%matched-item-elt= ~A~%  matched-end-item-elt= ~A" subseq matched-item-elt matched-end-item-elt))
       (if matched-item-elt
          (setf subseq (subseq  seq matched-item-elt))) ;;otherwise subseq = seq
      (when end-item
        (setf matched-end-item-elt (position end-item subseq :from-end t))
        (if matched-end-item-elt
            (setf subseq (subseq subseq 0 matched-end-item-elt))))
      ;;end second cond clause
      )      
     (t nil))
    ;;(afout 'out (format nil "==> BEGIN ANEW **~% subseq= ~A~% matched-item-elt= ~A~%  matched-end-item-elt= ~A" subseq matched-item-elt matched-end-item-elt))

    ;;find the original subseq from matched item (whether matched from beginning or from end)
    ;;  (if  matched-item-elt  (setf subseq (subseq seq matched-item-elt)))
    ;;(afout 'out (format nil "3 matched-item-elt= ~A~% subseq= ~A~%" matched-item-elt subseq))
    ;;(fout out)

    ;; trim-items trim-end-items
    (when (and trim-items (<= trim-items (length subseq)))
      (setf subseq (subseq subseq  trim-items))) 
    (when (and trim-end-items (<= trim-end-items (length subseq)))
      (setf subseq (subseq subseq 0 (- (length subseq) trim-end-items))))     

    ;;(afout 'out (format nil "4 subseq= ~A~%" subseq))

    ;; in case no matches were found, must reset subseq to nil
    (if (equal subseq seq)
        (setf subseq nil))
    ;;(afout 'out (format nil "5 subseq= ~A~%" subseq))
    ;; (fout out)
    (values subseq matched-item-elt matched-end-item-elt)
    ))
;;test
;;All tests appear to work
;;
#|
(defun itest ()
(setf out nil)
;;OK(match-subseq '#\9 "2012:11:19 19:15:06") ;; = "9 19:15:06" 9 NIL
;;(setf out nil)
;;OK(match-subseq '#\9 "2012:11:19 19:15:06" :end-item '#\0 ) ;;= "9 19:15:" 9 8
;;(setf out nil)
;;OK(match-subseq '#\9 "2012:11:19 19:15:06" :trim-items 1) ;;= " 19:15:06" 9 nil
;;(setf out nil)
;;OK (match-subseq 0 "2012:11:19 19:15:06":end-item '#\space :trim-end-items 1)  ;;  = "2012:11:1" 
;;(setf out nil)
;;OK (match-subseq '#\space "19:15:06":end-item '#\: :trim-end-items 2 :match-all-from-end t) ;; =  "19:"
;;(setf out nil)
;;OK (match-subseq '#\space "19:15:06":end-item '#\:  :match-all-from-end t) ;;= "19:15" (note: eliminates seconds)
(setf out nil)
;;OK (match-subseq  '#\: "19:15:06" :match-all-from-end  t )  ;; = ":06"
(setf out nil)
;;OK (match-subseq  nil "19:15:06" :end-item '#\:  :match-all-from-end t) ;;= ":06" 5 NIL  (returns time w/o seconds)
;;(setf out nil)
;;OK (match-subseq  nil "19:15:06":end-item '#\:) = "19:15" 0 5
)
;; (match-subseq     item seq &key  end-item trim-items trim-end-items match-all-from-end )
;;  (match-subseq 
;; (position  "PCategory"   "PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);")
|#



;;MAX-SEQ-N
;;2020
;;ddd
(defun max-seq-n (max-n seq  &key (trim-end-p T) )
  "U-sequences.   RETURNS (values new-seq trimmed-seq)"
  (let*
      ((new-seq)
       (n-seq (length seq))
       (trim-n (- max-n 1))
       (trimmed-seq)
       )
    (cond
     ((<= n-seq max-n)
      (setf new-seq seq))
     (T
      (cond
       (trim-end-p
        (setf new-seq (subseq seq 0 max-n)
              trimmed-seq (subseq seq max-n n-seq)))
       (T
        (setf new-seq (subseq seq (- trim-n 1)  n-seq)
              trimmed-seq (subseq seq 0 (- trim-n 1)))))))
    (values new-seq trimmed-seq)
    ;;end let, max-seq-n
    ))
;;TEST
;; (max-seq-n  6 "1234567890")
;; works= "123456"  "7890"
;; (max-seq-n  6 "1234567890" :trim-end-p nil)
;; works= "567890"  "1234"
;;for bigger nums
;; ;; (max-seq-n  20  "1234567890")
;; works= "1234567890"  NIL
;; for 0
;;  (max-seq-n  0  "1234567890")
;;works  ""  "1234567890"
;;for lists
;; (max-seq-n  6 '(1 2 3 4 5 6 7 8 9 0))
;; works= (1 2 3 4 5 6)   (7 8 9 0)