;;******************** U-FLI.lisp *************************




#|The example uses the FLI to find the position of the cursor using the Windows function GetCursorPos, which has the following C prototype:
BOOL GetCursorPos( LPPOINT )
The LPPOINT argument is a pointer to the POINT structure, which has the following
C definition:
1.2 Using the FLI to get the cursor position
5
typedef struct tagPOINT { 
   LONG x;
   LONG y;
   } POINT;
|#

;;LISP CODE FOR  C  GetCursorPos

;;STEP 1: DEFINE FLI DATA TYPES
(fli:define-c-typedef bool (:boolean :int))
(fli:define-c-typedef long :long)

#|Next, we need to define a structure for the FLI which is used to get the coordinates of the cursor. These coordinates will consist of an x and a y position. We use the define-c-typedef macro for this, and the resulting Lisp FLI code has obvious parallels with the C tagPOINT structure.|#
(fli:define-c-struct tagpoint
  (x long)
  (y long))

;;The tagPOINT structure for the FLI, corresponding to the C structure of the same name, has been defined. This now needs to be further defined as a type for the FLI, using define-c-typedef.
(fli:define-c-typedef point (:struct tagpoint))

;;Finally, a pointer type to point to the structure is required. It is this FLI pointer which will be passed to the Windows function GetCursorPos, so that Get-CursorPos can change the x and y values of the structure pointed to.
(fli:define-c-typedef lppoint (:pointer point))
;;END OF DEFINE FLI TYPES

;;STEP 2: DEFINITION OF FLI FUNCTIONS TO PERFORM THE INTERFACING.

(fli:define-foreign-function (get-cursor-position "GetCursorPos")
    ((lp-point lppoint))
  :result-type bool)
;;In this example, the defined FLI function is get-cursor-position. It takes as its argument a pointer of type lppoint, converts this to a C format, and calls GetCursorPos. It takes the return value it receives from GetCursorPos and converts it into the FLI bool type we defined earlier.

;;STEP 3: ALLOCATE MEMORY for an instance of the tagPOINT structure using allocate-foreign-object.
;;The following line of code binds location to a pointer that points to such an instance.
 (setq location (fli:allocate-foreign-object :type 'point))

;;STEP 4: LISP FUNCTION CALL: Use our interface function get-cursor-position to get the cursor position:
  (get-cursor-position location)

;;STEP 5: USING THE RESULTS
;;The position of the cursor is now stored in a POINT structure in memory, and location is a pointer to that location. To find out what values are stored we use the FOREIGN-SLOT-VALUE ACCESSOR, which returns the value stored in the specified field of the structure.
  (fli:foreign-slot-value location 'x)
  (fli:foreign-slot-value location 'y)

;;1.3 USING THE FLI TO SET THE CURSOR POSITION
;;A similar Windows function, SetCursorPos, can be used to set the cursor position. The SetCursorPos function takes two LONGs. The following code defines an interface function to call SetCursorPos.
(fli:define-foreign-function (set-cursor-position "SetCursorPos")
    ((x :long)
     (y :long))
  :result-type :boolean)

;;For example, THE CURSOR POSITION CAN NOW BE SET to be near the top left corner by simply using the following command:
  (set-cursor-position 20 20)

;;For a more extravagant example, define and execute the following function:
#|(defun test-cursor ()
  (dotimes (x 10)
    (dotimes (d 300)
      (let ((r (/ (+ d (* 300 x)) 10.0)))
        (set-cursor-position
         (+ 300 (floor (* r (cos (/ (* d pi) 150.0)))))
         (+ 300 (floor (* r (sin (/ (* d pi) 150.0)))))
         )))))|#
;;(test-cursor)

;;1.4 AN EXAMPLE OF DYNAMIC MEMORY ALLOCATION
#|In the previous example our defined interface function get-cursorposition
used the function allocate-foreign-object to allocate memory
for an instance of a POINT structure. This memory is now reserved, with a
pointer to its location bound to the variable location. More detailed information
on pointers is available in Chapter 3, “FLI Pointers”. To free the memory
associated with the foreign object requires the use of the function free-foreign-
object.|#
;;(fli:free-foreign-object location)
;;There are other methods for dealing with the question of memory management. The following example defines a Lisp function that returns the x and y coordinates of the cursor without permanently tying up memory for structures that are only used once.
#|(defun current-cursor-position ()
  (fli:with-dynamic-foreign-objects ()
    (let ((lppoint (fli:allocate-dynamic-foreign-object
                    :pointer-type 'lppoint)))
      (if (get-cursor-position lppoint)
          (values t (fli:foreign-slot-value lppoint 'x)
                  (fli:foreign-slot-value lppoint 'y))
        (values nil 0 0)))))|#
#|On calling current-cursor-position the following happens:
1. The macro with-dynamic-foreign-objects is called, which ensures
that the lifetime of any allocated objects is within the scope of the code
specified in its body.
2. The function allocate-dynamic-foreign-object is called to create an
instance of the relevant data structure required to get the cursor position.
Refer to it using the lppoint pointer.
3. The previously defined foreign function get-cursor-position is called
with lppoint.
4. Provided the call to GetCursorPos was successful the function foreign-
slot-value is called twice, once to return the value in the x slot
and again to return the value in the y slot. If the call was unsuccessful
then 0 0 nil is returned.
|#

;;4.1 PASSING A STRING TO A WINDOWS FUNCTION
;;The following example shows how to define a Lisp function which calls a Win32 API function to change the title of the active window. It demonstrates the use of define-foreign-function and with-foreign-string to pass a Lisp string to a Windows function.

;;STEP 1: The first step involves DEFINING A FLI TYPE to correspond to the Windows HWND  TYPE, which is the window HANDLE TYPE.
(fli:define-c-typedef fli-hwnd
  (:unsigned :long))

;;STEP 2: FOREIGN FUNCTION DEFINITIONS. The first foreign function RETURNS THE WINDOW HANDLE OF THE ACTIVE WINDOW, by calling the Windows function GetActiveWindow. It takes no arguments.
  (fli:define-foreign-function (get-act-window "GetActiveWindow")
      ()
    :result-type fli-hwnd
    :documentation "Returns the window handle of the active window
for the current thread. If no active window is associated with the
current thread then it returns 0."
    )

;;The next foreign function uses the Windows function SETWINDOWTEXT to set the text of the active window titlebar. It takes a window handle and a pointer to a FLI string as its arguments.
(fli:define-foreign-function (set-win-text "SetWindowText" :dbcs)
    ((hwnd fli-hwnd)
     (lpstring :pointer))
  :result-type :boolean
  :documentation "Sets the text of the window titlebar.")

;;The foreign function set-win-text returns a boolean to indicate whether it has successfully changed the title bar. The required FLI data types and foreign functions have been defined. What is now required is a Lisp function which uses them to change the titlebar of the active window. The next function does this:
(defun set-active-window-text (new-text)
  (let ((active-window (get-act-window))
        (external-format (if (string= (software-type)
                                      "Windows NT")
                             :unicode
                           :ascii)))
    (unless (zerop active-window)
      (fli:with-foreign-string (new-ptr element-count byte-count
                                        :external-format external-format)
          new-text
        (declare (ignore element-count byte-count))
        (set-win-text active-window new-ptr)))))

;;The function set-active-window-text takes a Lisp string as its argument, and does the following:
;;1. It calls the foreign function get-act-window to set the variable activewindow to be the handle of the active window. If no window is active, this will be zero.
#|2. The variable external-format is set to be :unicode if the operating
system is Windows NT or a later system based on it (which expects
strings to be passed to it in Unicode format), otherwise it is set to be
:ascii.
3. If active-window is zero, then there is no active window, and the function
terminates, returning nil.
4. If active-window is not zero, then it contains a window handle, and the
following happens:
The function uses with-foreign-string to convert the Lisp string
argument of the function into a FLI string, and a pointer to the FLI string
is allocated, ready to be handed to the foreign function set-win-text
that we defined earlier. The encoding of the string is external-format,
which is the encoding suitable for the operating system running on the
computer. Once the window title has been set, with-foreign-string
automatically deallocates the memory that was allocated for the FLI
string and the pointer. The function then terminates, returning t.
|#
;;You can test that this is what happens by entering the command:
;;  (set-active-window-text "A new title for the active window")

;;See with-foreign-string, page 181, for more details on the use of foreign strings.

;;4.6 USING DLLS WITHIN THE LISPWORKS FLI
;; me-I THINK the following ONLY REFERS TO CALLING C FUNCTIONS that are defined WITHIN C FILES--NOT TO RUNNING WHOLE C DLL FILES.
;;In order to use FUNCTIONS defined in a dynamically linked library (DLL) withinthe LispWorks FLI, the functions need to be exported from the DLL.
;;4.6.1 Using C DLLs. You can export C functions in three ways:
#|1. Use a __declspec(dllexport) declaration in the C file.
In this case you should also make the functions use the cdecl calling
convention, which removes another level of name mangling.
2. Use an /export directive in the link command.
3. Use a .def file. THIS MAY BE THE BEST
MAYBE EASY:
1. Create file multiply.def
2. with one line:
 exports multiply=multiply

|#

;;MISC EXAMPLES
;;  (example-edit-file "fli/foreign-callable-example")


;;TESTING WHETHER CERTAIN FUNCTIONS ARE NOW AVAILABLE.
;;To detect when a C function name is defined, call 
#|(not (fli:null-pointer-p
      (fli:make-pointer :symbol-name name
                        :errorp nil)))|#

#|4.6.2 Using C++ DLLs
You must make the exported names match the FLI definitions. To do this:
• If you can alter the C++ code, wrap extern "C" {} around the C++
function definitions, or
• Create a second DLL with C functions that wrap around each C++ function,
and make those C functions accessible as described in “Using C
DLLs” on page 52.
Note: watch out for the calling convention of the exported function, which
must match the :calling-convention in the FLI definitions.|#


