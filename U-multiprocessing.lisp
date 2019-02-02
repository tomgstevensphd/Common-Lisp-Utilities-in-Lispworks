;; ********************** U-multiprocessing.lisp **************************

;;xxx
#|
15.2.8.1 RETURNING A VALUE from another process. Rather than using GLOBAL VARIABLES to pass values between processes, you can use CLOSURES instead. For example:
|#
;;SEND-WITH-RESULT
;;
;;ddd
(defun send-with-result (process function)
  "In U-multiprocessing.lisp Could be a very useful function? -- from capi manual. RETURNING A VALUE from another process Rather than using GLOBAL VARIABLES to pass values between processes, you can use CLOSURES instead. example:"
  (let ((remote-result :none))
    ;;flet defines to local functions (resultp () (listp remote-result)) and
    ;;   (run-it () (setq remote-result (multiple-value-list (funcall function))))
    (flet ((resultp ()
             (listp remote-result))
           (run-it ()
             (setq remote-result
                   (multiple-value-list (funcall function)))))
      ;;then the following two processes USE these 2 functions below to send and receive
      (MP:PROCESS-SEND process (list #'run-it))
      (MP:PROCESS-WAIT "Waiting for result" #'resultp)
      (values-list remote-result))))

(defun continue-process ()
  "In U-multiprocessing.lisp, to be used  by send-with-result for starting and stopping a process. Returns T."
  T )
(defun hold-process ()
  "In U-multiprocessing.lisp, to be used  by send-with-result for starting and stopping a process. Returns NIL."
  NIL )
  



; xxx ----------------------- MULTI-PROCESSING SELECTED HELP -------------
;;PROCESS-WAIT

#|
From Screensaver2013.lisp
(defun my-lock-and-condition-variable-wait 
       ( text height lock lock-timeout lock-wait-reason
              condvar condvar-timeout condvar-wait-reason
              predicate predicate-args 
              return-function return-function-args)
  (let ((text (format nil "PRE Function lock-and-condition-variable-wait, text= ~A~%height=~A~%
                lock= ~A lock-timeout=~A lock-wait-reason= ~A~% 
                condvar= ~A condvar-timeout= ~A condvar-timeout-reason= ~A~%
                predicate= ~A predicate-args= ~A~%
                return-function= ~A return-function-args= ~A~%"
                      text height lock lock-timeout lock-wait-reason
                      condvar condvar-timeout condvar-wait-reason
                      predicate predicate-args 
                      return-function return-function-args))
        )
   (if *show-details (afout 'out (format nil "text= ~A~% height= ~A~%" text height)))
    (MP:LOCK-AND-CONDITION-VARIABLE-WAIT  lock condvar predicate
                                         :args predicate-args
                                         :return-function return-function
                                         :return-function-args return-function-args
                                         :lock-timeout lock-timeout
                                         :lock-wait-reason lock-wait-reason
                                         :condvar-timeout condvar-timeout
                                         :condvar-wait-reason condvar-wait-reason)
    ))
(defun photo-viewer-auto-select-callback (interface)
  " Step 5: calls my-lock-and-condition-variable-wait called by the interface-instance when "go" button pushed and Step 6: uses process-run-function to start calculator-process "
  (with-slots (image rich-text-pane1 output-pane1)   interface ;;*photo-viewer-instance
    (let ((x)
          )  
        (if *show-details  (afout 'out (format nil "STEP 5:IN START-CALLBACK, starting the CALCULATOR PROCESS")))
      ;;starting the calculator process
      ;;reset the *current-photo-info-list to default values
      (set-current-photo-info-list)
      (setf  *calculator-process
             (MP:PROCESS-RUN-FUNCTION "calculator-process" '()
                                      'calculator-process-work-function interface))

      ;;locking the MP condition-variable so  process will wait and later interupt on signal
      (my-lock-and-condition-variable-wait 
       "Step 6: SETTING *return-function-value TO my-lock-and-condition-variable-wait result"
       *default-height *lock *lock-timeout *lock-wait-reason
      *condvar  *condvar-timeout *condvar-timeout-reason
       *predicate  *predicate-args  'get-current-photo-info-list 
       (list "FROM START-CALLBACK, my-lock-and-condition-variable-wait" NIL))
      ;;If the above function is held in a wait, then perhaps it won't execute the following until the
      ;;   global *current-photo-info-list has been set from the calculator

      ;;end of let, with-slots, defun
      )))
;; end of from Screensaver ------------------------------------

15.6.1 CONDITION VARIABLES
A condition variable ALLOWS YOU TO WAIT FOR SOME CONDITION TO BE SATISFIED,
based on the VALUES STORED IN SHARED DATA that is PROTECTED BY A LOCK. 
The condition is typically something like DATA BECOMING AVAILABLE IN A QUEUE.

The function CONDITION-VARIABLE-WAIT is used to wait for a condition variable
to be signalled. It is always called with the lock held, which is automatically
released while waiting and reclaimed before continuing. More than one
thread can wait for a particular condition variable, so after being notified
about the condition changing, you should check the shared data to see if it
represents a useful state and call condition-variable-wait again if not.
The function condition-variable-signal is used to wake exactly one
thread that is waiting for the condition variable. If no threads are waiting,
then nothing happens.

Alternatively, the function CONDITION-VARIABLE-BROADCAST can be used to
WAKE ALL OF THE THREADS THAT ARE WAITING AT THE TIME IT IS CALLED.
Any threads that wait after the call to condition-variable-signal or condition-
variable-broadcast will not be woken until the next call.

In most uses of condition variables, the call to CONDITION-VARIABLE-SIGNAL
or CONDITION-VARIABLE-BROADCAST should be made while holding the lock
that waiter used when calling condition-variable-wait for this condition
variable. This ensures that the signal is not lost if another thread is just about
to call condition-variable-wait.
187
The function CONDITION-VARIABLE-WAIT-COUNT can be used to determine
the current NUMBER OF THREADS WAITING for a condition variable.
The condition variable implementation in LispWorks aims to comply with the
POSIX standard where possible.

CONDITION-VARIABLE-WAIT, condition-variable-signal and conditionvariable-
broadcast 
have CORRESPONDING functions (me-USE THESE?)
LOCK-AND-CONDITIONVARIABLE-WAIT, 
lock-and-condition-variable-signal and 
lock-andcondition-variable-broadcast. 
For condition-variable-wait there is
also SIMPLE-LOCK-AND-CONDITION-VARIABLE-WAIT, which is simpler to use.
The lock-and-condition-* functions perform the equivalent of locking and
in the scope of the lock doing something and calling the corresponding condition-*
function.
The LOCK-AND-CONDITION-* FUNCTIONS NOT ONLY MAKE IT SIMPLER TO CODE, they
also make it EASIER TO AVOID MISTAKES, and can optimize some cases (in particular,
the quite common case when there is no need to lock on exit from condition-
variable-wait). THEY ARE THE RECOMMENDED INTERFACE.
The lock-and-condition-* functions can be used together with condition-* functions 
on the same locks and condition variables.
Note: In cases when only one process waits for the condition, using processwait-
local for waiting and process-poke for signalling is easier, and
involves less overhead.
|#




#|
15.2.9 STOPPING and unstopping processes
This section describes a typical way of using process-stop and processunstop.
Suppose a pool of "worker" processes is MANAGED BY A "MANAGER" PROCESS. A
process in the worker pool MARKS ITSELF AS AVAILABLE FOR WORK, and then calls
PROCESS-STOP. 
The manager process later finds a worker process that is
marked as available for work, puts the work in a place known to the worker
process, and then calls PROCESS-UNSTOP on the worker process.

15.2 The process programming interface
175
For this scheme to work properly, the check of WHETHER THE WORKER IS AVAILABLE
needs to INCLUDE A CALL TO PROCESS-STOPPED-P. Otherwise, it is possible for the
following sequence of events to occur:
1. A worker marks itself as available.
2. The manager process finds the worker and gives it the work. (below: call PROCESSSTOPPED-P)
3. The manager process calls process-unstop on the worker.
4. The worker process proceeds and calls process-stop, and never wakes
up.
To guard against this possibility, then the manager should 
call PROCESSSTOPPED-P when finding the worker in the second step above. 
Alternatively, it could check the result of process-unstop.

15.2.10 Example
The following example allows two (or more) multiplication tables to be
printed out simultaneously.
First, the function to print out a multiplication table.
|#
;;xxx
(in-package "USER")
(defun print-table (number total stream)
  (do ((i 1 (+ i 1)))
      ((> i total))
    (format stream "~S X ~S = ~S~%" number i (* i number))
    (mp:process-allow-scheduling)))
#|Note the use of PROCESS-ALLOW-SCHEDULING to ALLOW THE PROCESS TO BE INTERRUPTED
once during each iteration of the do loop.
Now we define the function that calls print-table within multiprocessing:|#
(defun process-print-table (name number total)
  (mp:process-run-function name NIL
                           `print-table number total *standard-output*))
#|
The NIL ARGUMENT IS USED BECAUSE NO KEYWORDS ARE SPECIFIED.
process-print-table can now be called from two separate Listener windows
to PRINT OUT DIFFERENT MULTIPLICATION TABLES SIMULTANEOUSLY, for example:
176
(process-print-table "t1" 5 50)
;;in one Listener and:
(process-print-table "t2" 6 50)
in another Listener.
|#





;; --------------------------- end multi-processing help ----------------------

