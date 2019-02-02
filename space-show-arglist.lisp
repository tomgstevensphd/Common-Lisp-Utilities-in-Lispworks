;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/editor:commands:space-show-arglist.lisp,v 1.8.8.1 2009/08/21 21:12:58 davef Exp $" -*-

;; Copyright (c) 1987--2010 LispWorks Ltd. All rights reserved.


;; This code illustrates use of a timer to display the argument list 
;; of a function after space is inserted to the right of it

(in-package "CL-USER")

(defvar *arglist-delay* 1)
(defvar *arglist-timer* nil)

(defvar *setf-names-p* nil "when true, show the argument list for setf names too.")

(editor:defcommand  "Insert Space and Show Arglist" (p)
     "Display the argument list in the echo area a while after inserting Space to the right of the function."
     "Display the argument list."
  (editor:self-insert-command p #\Space)
  (let* ((x (if *setf-names-p*
                (editor:with-point ((temp1 (editor:current-point))
                                    (temp2 (editor:current-point)))
                  (when (editor:form-offset temp1 -1)
                    (ignore-errors
                      (let ((*package* (editor::buffer-package-to-use temp1)))
                        (read-from-string 
                         (editor:points-to-string temp1 temp2))))))
              (editor:buffer-symbol-at-point (editor:current-buffer))))
         (window (editor:current-window))
         (function (find-function-for-arglist x)))
    (when (fboundp function)
      (show-arglist function 
                    (capi:top-level-interface 
                     (editor:window-text-pane window)) 
                    window))))

(defun show-arglist (function interface editor-window)
    (setq *arglist-timer* 
          (mp:make-timer 'capi:execute-with-interface interface 
                         'editor:process-character 
                         (list 'editor:function-arglist-command nil function)
                         editor-window))
    (mp:schedule-timer-relative *arglist-timer* *arglist-delay*))

(defun find-function-for-arglist (x)
  (typecase x
    (symbol x)
    (list (unless (dotted-list-p x)
            (if (eq (length x) 1)
                (find-function-for-arglist (car x))
              (case (car x)
                ((quote function) (find-function-for-arglist (cdr x)))
                (setf (and (= (length x) 2)
                           (symbolp (second x))
                           x)))))))) 

(editor:bind-key "Insert Space and Show Arglist" #\Space :mode "Lisp") 
(editor:bind-key "Insert Space and Show Arglist" #\Space :mode "Execute") 



#| test cases
foo
defun
car
'car
#''car
#'car
(setf capi:choice-selected-item)    
'(setf capi:choice-selected-item)  
#'(setf capi:choice-selected-item)     
(setf *bar* (foo))  
123  
#1a(0 1 2) 
(defpackage "MY-PACKAGE")
(in-package "MY-PACKAGE")
(defun my-foo ()) 
my-package::my-foo  
|#
