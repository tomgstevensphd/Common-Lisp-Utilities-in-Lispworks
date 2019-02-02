;;****************************** U-system.lisp ******************************
;;
;;FUNCTIONS FOR INTERACTING WITH THE WINDOWS, CMD, DOS (or other) SYSTEM



;;MY-CALL-SYSTEM
;;2018
;;ddd
(defun my-call-system  (command  &optional args  
                                 &key to-path (show-output-t T)
                                 (wait t) current-directory
                                 (prefix "") show-cmd output-stream  wait  
                                 kill-process-on-abort extra-environ)
  "In U-system, Use copyfiles, ETC SHOW-OUTPUT-T causes output to show in  prefix is a prefix to be printed at the start of any output line. The default value is \"; \". show-cmd specifies whether or not the cmd invoked will be  printed as well as the output for that command. If t then cmd will be printed. The default value for show-cmd is t.  OUTPUT-STREAM specifies where the output will be sent to. NIL causes it to be a second value, otherwise can be a STREAM if :tty= *standard-output* RETURNS (values dos-return dos-output) "
  (let
      ((dos-command) 
       (all-args args)
       (dos-out-string)
       (dos-output)
       (output-stream-str) 
       )
    (cond
     ((streamp output-stream)
      (setf output-stream-str output-stream))
     (t  (setf output-stream-str (make-string-output-stream))))

    ;;ARGS STRING OR LIST
    (cond
     ((and (null args)(stringp command)) 
      (setf dos-command command))
     ((stringp args)
      (if to-path (setf all-args (format nil "~A~A" args to-path)))
      (setf dos-command
            (format nil "~A ~A" command all-args)))
     ((listp all-args)
      (when to-path (setf all-args (append all-args (list to-path))))
      (setf dos-command (format nil "~A ~A" command 
                                (convert-list-items-to-string all-args))))
     (t nil))

    ;;SHOW-OUTPUT-T?
    (cond
     ((or show-output-t  prefix show-cmd output-stream   
          kill-process-on-abort)

#|      (multiple-value-setq (dos-output
          (sys:call-system-showing-output dos-command 
                                          :current-directory current-directory  :wait wait
                                          :prefix prefix :show-cmd  show-cmd 
                                          :output-stream  output-stream-str
                                          :kill-process-on-abort kill-process-on-abort))

        (setf dos-out-string (get-output-stream-string output-stream-str))
        (break "1")
|#

    (multiple-value-setq (dos-out-string dos-output)
        (CALL-WITH-SHOW-DOS-OUTPUT dos-command :prefix prefix
                    :show-cmd show-cmd :output-stream output-stream
                       :kill-process-on-abort kill-process-on-abort
                       :extra-environ extra-environ))
        )
     (t
      (setf dos-output
            (sys:call-system dos-command 
                         :current-directory current-directory  :wait wait))))
    (values  dos-out-string dos-output)
    ;;end let, my-call-system
    ))
;;TEST
;; (my-call-system "dir c:\\temp" )
;; works=
#|"dir c:\\temp
 Volume in drive C is OS
 Volume Serial Number is 4058-766B

 Directory of c:\\temp

04/14/2018  04:40 PM    <DIR>          .
04/14/2018  04:40 PM    <DIR>          ..
11/13/2017  09:53 AM         4,791,427 ajpegcompr.exe
02/08/2017  11:54 PM        11,842,648 bitdefender_windows_bc3db0e0-1194-4117-97ec-46995edd53a7.exe
02/08/2017  11:29 PM         1,130,496 Chrome Setup.exe
etc etc etc
|#





;;CALL-WITH-SHOW-DOS-OUTPUT
;;2017
;;ddd
(defun call-with-show-dos-output (dos-command 
                                  &Key prefix   output-stream wait kill-process-on-abort
                                  (show-cmd t)  extra-environ )
  "In U-system, Uses sys:call-system-showing-output to run any dos command (string or vector--see refs) RETURNS dos-out-string and the actual dos-output call-sys-output. VERY USEFUL"
  (let
      ((echo-stream)
       (dos-out-string "")
       (call-sys-output)
       )
  (unless output-stream
    (setf output-stream (make-string-output-stream)))
    (unless prefix
      (setf prefix ""))

    (setf  call-sys-output      
          (sys:call-system-showing-output dos-command :prefix prefix
                       :show-cmd show-cmd :output-stream output-stream
                       :kill-process-on-abort kill-process-on-abort
                       :extra-environ extra-environ))

    (setf dos-out-string (get-output-stream-string output-stream))
   (values dos-out-string call-sys-output)
   ;;end let, call-with-show-dos-output
    ))
;;TEST
#|
   (setf *testx-out (call-with-show-dos-output "VOL E:" ))
 works=  
*testx-out= 
 "VOL E:
 Volume in drive E is SD01-128
 Volume Serial Number is 3634-6637
"
|#





;;
;;TESTING  -- RETURNS 0 WHEN WORKS, 1 IF NOT
;;
;;    (SYS:call-system '(  "C:\\Program Files (x86)\\Windows NT\\Accessories\\wordpad.exe")) = 0 WORKS
;;  (SYS:call-system  '("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe")) = 0 , WORKS
;;
;; from manual
;;  (sys:call-system '("notepad" "myfile.txt")) = WORKS

;;   (SYS:call-system  '("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" "C:\\3-TS\\LISP PROJECTS TS\\screensaver\\video-playback-utility\\MVI_0095.MP4"))

;;(SYS:call-system  '("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" "C:\\3-TS\\LISP PROJECTS TS\\screensaver\\video-playback-utility\\VTS_01_1.VOB" ":FULLSCREEN"))




;;FOLLOWING WORKS IN A DOS WINDOW (fullscreen, no repeats/loops)
#|  CD  "C:\Program Files (x86)\VideoLAN\VLC"
VLC  --fullscreen --no-repeat --no-loop "C:\3-TS\LISP PROJECTS TS\screensaver\video-playback-utility\VTS_01_1.VOB"|#
;;END


;;TESTING PROCESS-WAIT
;;  (mp:process-wait "Waiting for video" #'play-video "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" '("MVI_0095.MP4" "MVI_2594.MOV" "VTS_01_1.VOB")  :common-path "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\")

;;TESTING PROCESS-RUN-FUNCTION
;;  (mp:process-run-function "Play video"  #'run-video-function ) )

;; (setf process-x (mp:process-run-function "VLC" nil  #'sys:call-system  '("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_0095.MP4"))) = #<MP:PROCESS Name "VLC" Priority 85000000 State "Running">
;; (mp:process-terminate process-x)
;;CL-USER 43 > process-x = #<MP:PROCESS Name "VLC" Priority 0 State "Dead">
;; ABOVE APPEARS TO WORK, BUT DOESN'T CLOSE VLC

;; ************* TEST **********************
;;THIS WORKS AND CLOSES SOMETIMES--OFTEN NOT
;; (sys:call-system-showing-output ' ("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc" "--fullscreen" "--no-repeat" "--no-loop" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_2594.MOV" "vlc://pause: 3" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_0095.MP4" "vlc://pause: 3" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\VTS_01_1.VOB"  "vlc://pause: 3"  "vlc://quit"))

(defun testvlc ()
  (sys:call-system-showing-output  '("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc" "--fullscreen" "--no-repeat" "--no-loop" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_2594.MOV" "vlc://pause: 3" "vlc://quit")) 
 (sleep  5)   
  (sys:call-system-showing-output   '("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc" "--fullscreen" "--no-repeat" "--no-loop"   "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_0095.MP4" "vlc://pause: 3" "vlc://quit"))  
 (sleep  5)    
  (sys:call-system-showing-output  '("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc" "--fullscreen" "--no-repeat" "--no-loop"  "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\VTS_01_1.VOB"  "vlc://pause: 3"  "vlc://quit"))
  )
;; JUST FLASHES ON SCREEN BRIEFLY
;; (testvlc)
 
;; (sys:call-system-showing-output  '( "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc"   "--fullscreen"   "--no-repeat"   "--no-loop"    "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_0095.MP4"   "vlc://pause: 3"    "vlc://quit" ))
;; OUTPUT 
; C:\Program Files (x86)\VideoLAN\VLC\vlc --fullscreen --no-repeat --no-loop C:\3-TS\LISP PROJECTS TS\video-playback-utility\MVI_0095.MP4 vlc://pause: 3 vlc://quit 
; [0206df70] main libvlc: Running vlc with the default interface. Use 'cvlc' to use vlc without interface.
; [021488f0] idummy demux: command `pause 3.000000'
; [021488f0] idummy demux: command `quit'
;;0

;; (sys:call-system-showing-output '("dir *.*")) = error


;;  (sys:run-shell-command "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc") = 1
;; (sys:run-shell-command "dir *.*") = 0

(defun testshell ()
  (multiple-value-bind (out err pid)
      (sys:run-shell-command "dir *.*"
                             :wait  nil
                           ;;  :output :stream
                          ;;   :error-output :stream
                          )
    #|(with-open-stream (out out)
      (with-open-stream (err err)
        (values (read-line out) (read-line err))))|#
  (values out err pid)    
  ))
;; (testshell)


#| from LW
(multiple-value-bind (out err pid)
    (sys:run-shell-command "sh -c 'echo foo >&2; echo
bar'"
                           :wait nil
                           :output :stream
                           :error-output :stream)
  (with-open-stream (out out)
    (with-open-stream (err err)
      (values (read-line out) (read-line err)))))
|#

;;  (sys:call-system-showing-output  '("cmd"))
;;  (sys:call-system-showing-output  '("dir *.*"))

;;
;; (sys:call-system-showing-output ' ("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc" "--fullscreen" "--no-repeat" "--no-loop" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_2594.MOV"  "vlc://pause: 5"  "vlc://quit"))

;;THIS SEEMS USELESS :current-directory "C:\\Program Files (x86)\\VideoLAN\\VLC")
;;"C:\3-TS\LISP PROJECTS TS\video-playback-utility\MVI_0095.MP4"  "vlc://pause: 5"
;;(sys:call-system-showing-output ' ("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" "--fullscreen" "--no-repeat" "--no-loop" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_2594.MOV" "C:\3-TS\LISP PROJECTS TS\video-playback-utility\MVI_0095.MP4"  "vlc://pause: 5" "vlc://quit"))
 ;;"C:\3-TS\\LISP PROJECTS TS\\video-playback-utility\\VTS_01_1.VOB"

;;  TRY TO DUPLICATE BAT FILE THAT WORKS
;;  (sys:call-system '("cmd"  "cd"  "C:\\Program Files (x86)\\VideoLAN\\VLC" ))
;;  (sys:call-system '("cd  C:\\Program Files (x86)\\VideoLAN\\VLC"))
;;  (sys:call-system '( "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_0095.MP4" )) ;; "vlc://pause: 5" "vlc://quit"))
;;  (sys:call-system  '("TASKKILL" "/F"  "/IM"  "vlc.exe" "/T"))

(defun my-call&end-sys ()
   (let
       ((process (mp:process-run-function "Name" nil #'sys:call-system  '( "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_0095.MP4")))
        )
     ;;(mp:process-terminate process)
     (sys:call-system  '("TASKKILL" "/F"  "/IM"  "vlc.exe" "/T"))
     ))
;;  (my-call&end-sys)
;; PROBLEM, VLC DOESN'T SIGNAL ANYTHING WHEN FINISHES THE VIDEOS, SO LISP FUNCTIONS CAN'T KNOW WHEN TO TERMINATE IT.
;; READ VLC TO SEE IF CAN SET UP A DOS SIGNAL OF SOME SORT??



(defun write-this-x (x)
  (setf *write-this-x (format nil "~A~%" x)))

(defun do-this-x (x)
  (let 
      ((process (mp:process-run-function "Do this" nil #'write-this-x x))
       )
    (mp:process-terminate process)
    ))
;; (do-this-x "This is x")
                
(defparameter *video-complete-p nil)

;;FOLLOWING DOES KILL VLC FROM 
;; CL-USER 44 > (sys:call-system  '("TASKKILL" "/F"  "/IM"  "vlc.exe" "/T")) = 0

(defun test-run-video ()
  (let
      ((process)
       )
    ;;(setf process (mp:process-run-function "VLC" nil  #'sys:call-system  '("C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\MVI_0095.MP4")))
    (setf process (mp:process-wait "VLC Running" #'play-video  "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" '("MVI_0095.MP4" )  :common-path "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\"))
          ;;was (mp:process-run-function "VLC" nil  #'run-video-function))
  
    ;;SSS -- TASKKILL WORKS, BUT WILL NOT BE CALLED UNTIL THE VLC IS SHUT DOWN, THEREFORE DEFEATING ITS PURPOSE
    (sys:call-system  '("TASKKILL" "/F"  "/IM"  "vlc.exe" "/T"))
    (mp:process-terminate process)
    ))
;; (test-run-video)

     

;;main bottom level
(defun run-video-function ()
  (let
      ((result)
       )
    (when
        (play-video "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" '("MVI_0095.MP4" )  :common-path "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\") ;;"MVI_2594.MOV" "VTS_01_1.VOB"
      (setf result T 
            *video-complete-p T)
      ;;(format T "This is after the video was run and closed.")
      )
    result
    ))
;; (run-video-function)


;;(mp:process-wait "Waiting for video" #'play-video "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe" '("MVI_0095.MP4" "MVI_2594.MOV" "VTS_01_1.VOB")  :common-path "C:\\3-TS\\LISP PROJECTS TS\\video-playback-utility\\")


;;XXX ================= DOS HELP MENU ===============
;;(call-with-show-dos-output "VOL E:" )

#|Microsoft Windows [Version 10.0.14393]
(c) 2016 Microsoft Corporation. All rights reserved.

C:\Windows\System32>HELP
For more information on a specific command, type HELP command-name
ASSOC          Displays or modifies file extension associations.
ATTRIB         Displays or changes file attributes.
BREAK          Sets or clears extended CTRL+C checking.
BCDEDIT        Sets properties in boot database to control boot loading.
CACLS          Displays or modifies access control lists (ACLs) of files.
CALL           Calls one batch program from another.
CD             Displays the name of or changes the current directory.
CHCP           Displays or sets the active code page number.
CHDIR          Displays the name of or changes the current directory.
CHKDSK         Checks a disk and displays a status report.
CHKNTFS        Displays or modifies the checking of disk at boot time.
CLS            Clears the screen.
CMD            Starts a new instance of the Windows command interpreter.
COLOR          Sets the default console foreground and background colors.
COMP           Compares the contents of two files or sets of files.
COMPACT        Displays or alters the compression of files on NTFS partitions.
CONVERT        Converts FAT volumes to NTFS.  You cannot convert the
               current drive.
COPY           Copies one or more files to another location.
DATE           Displays or sets the date.
DEL            Deletes one or more files.
DIR            Displays a list of files and subdirectories in a directory.
DISKPART       Displays or configures Disk Partition properties.
DOSKEY         Edits command lines, recalls Windows commands, and
               creates macros.
DRIVERQUERY    Displays current device driver status and properties.
ECHO           Displays messages, or turns command echoing on or off.
ENDLOCAL       Ends localization of environment changes in a batch file.
ERASE          Deletes one or more files.
EXIT           Quits the CMD.EXE program (command interpreter).
FC             Compares two files or sets of files, and displays the
               differences between them.
FIND           Searches for a text string in a file or files.
FINDSTR        Searches for strings in files.
FOR            Runs a specified command for each file in a set of files.
FORMAT         Formats a disk for use with Windows.
FSUTIL         Displays or configures the file system properties.
FTYPE          Displays or modifies file types used in file extension
               associations.
GOTO           Directs the Windows command interpreter to a labeled line in
               a batch program.
GPRESULT       Displays Group Policy information for machine or user.
GRAFTABL       Enables Windows to display an extended character set in
               graphics mode.
HELP           Provides Help information for Windows commands.
ICACLS         Display, modify, backup, or restore ACLs for files and
               directories.
IF             Performs conditional processing in batch programs.
LABEL          Creates, changes, or deletes the volume label of a disk.
MD             Creates a directory.
MKDIR          Creates a directory.
MKLINK         Creates Symbolic Links and Hard Links
MODE           Configures a system device.
MORE           Displays output one screen at a time.
MOVE           Moves one or more files from one directory to another
               directory.
OPENFILES      Displays files opened by remote users for a file share.
PATH           Displays or sets a search path for executable files.
PAUSE          Suspends processing of a batch file and displays a message.
POPD           Restores the previous value of the current directory saved by
               PUSHD.
PRINT          Prints a text file.
PROMPT         Changes the Windows command prompt.
PUSHD          Saves the current directory then changes it.
RD             Removes a directory.
RECOVER        Recovers readable information from a bad or defective disk.
REM            Records comments (remarks) in batch files or CONFIG.SYS.
REN            Renames a file or files.
RENAME         Renames a file or files.
REPLACE        Replaces files.
RMDIR          Removes a directory.
ROBOCOPY       Advanced utility to copy files and directory trees
SET            Displays, sets, or removes Windows environment variables.
SETLOCAL       Begins localization of environment changes in a batch file.
SC             Displays or configures services (background processes).
SCHTASKS       Schedules commands and programs to run on a computer.
SHIFT          Shifts the position of replaceable parameters in batch files.
SHUTDOWN       Allows proper local or remote shutdown of machine.
SORT           Sorts input.
START          Starts a separate window to run a specified program or command.
SUBST          Associates a path with a drive letter.
SYSTEMINFO     Displays machine specific properties and configuration.
TASKLIST       Displays all currently running tasks including services.
TASKKILL       Kill or stop a running process or application.
TIME           Displays or sets the system time.
TITLE          Sets the window title for a CMD.EXE session.
TREE           Graphically displays the directory structure of a drive or
               path.
TYPE           Displays the contents of a text file.
VER            Displays the Windows version.
VERIFY         Tells Windows whether to verify that your files are written
               correctly to a disk.
VOL            Displays a disk volume label and serial number.
XCOPY          Copies files and directory trees.
WMIC           Displays WMI information inside interactive command shell.

For more information on tools see the command-line reference in the online help.

C:\Windows\System32>(call-with-show-dos-output "VOL E:" )
'call-with-show-dos-output' is not recognized as an internal or external command,
operable program or batch file.

C:\Windows\System32>|#