;;************************** U-capi-graphics.lisp **************************
;;
;;


;;



;;PLACE-OBJECTS-IN-WINDOW
;;2018
;;ddd
;;SSS DO THIS LATER???

;; SEE ;;PLACE-NODES-IN-WINDOW IN CS-NET-VIEWER.LISP


;;FUNCTIONS FOR REDISPLAY
;;WILL DISPLAY--NOT REDISPLAY (see below example)
;; (CAPI:OUTPUT-PANE-DISPLAY-CALLBACK OUTPUT-PANE)  
;;
;;Following DO NOT WORK in output-pane: (capi:redisplay self/element), (capi:redisplay-collection-item collection item), (capi:redisplay-interface interface), 
;; INITARG IN OUTPUT-PANE
;;   :display-callback
;; (setf (capi:output-pane-display-callback output-pane ) value) [within with-slots??]
;; eg??  (with-slots (output-pane) interface
;;             (apply-in-pane-process  
;;                #'(setf capi:output-pane-display-callback) value output-pane)

;;ONLY WAY TO GET REDISPLAY (eg from minimized window) THAT WORKED SO FAR:
; :DISPLAY-CALLBACK  EXAMPLE FROM ART ART-multipane-interface.lisp
;;from interface
#|   (output-pane-3
    capi:output-pane
    :visible-min-height 130
    :visible-max-height 130
    :DISPLAY-CALLBACK 'DISPLAY-WUP-CALLBACK)|#

;;FOR W-UP GRAPH
#|(defun DISPLAY-WUP-CALLBACK  (OUTPUT-PANE-3 X Y WIDTH HEIGHT)
   ;;(initialize-ART-data)
   (let
       ((self (capi:top-level-interface output-pane-3))
        (wup-points1)
        )
   (with-slots (rich-text-pane-3 wup-points) self ;;was *ART-instance ;; (CAPI:TOP-LEVEL-INTERFACE output-pane-3)
     (setf wup-points1 (slot-value self 'wup-points))
     ;;main function
      (write-to-graph+text-pane self    output-pane-3 rich-text-pane-3
       wup-points1 ;;was *wup-points  
       *x-max 100 40 30  ;;was 20 20
       :multiple-graphs-p t
       :begin-num 0 
                         :label-margin 10 :graph-color :blue 
                     :x-mark-interval 40 :y-mark-interval 10  :half-hash-length 3 
                     :label-margin 10
                            :x-edit-pane-label "Wup DATA:"
                             :y-label-list nil
                             ;;not needed? :x-increment  20 
                             :y-increment 1.0 ;;was 10.0
                            :x-label-color :black :y-label-color :black
                             :x-string "Wup (Weights-LTM)"
                             :y-string "" ;; '("AA" "BB" "CC" "DD" "YY")
                              :x-string-margin 30 :y-element-space 10
                              :x-string-color :red :y-string-color :blue
                              :center-x-string-p t :center-y-string-p t
                              :scale-to-y-max-val-p T
                              :x-multiplier 1.0  :y-multiplier 1.0 :y-floor-value 0
                              :display-instance-p NIL)
   ;;end with-slots, let, display-wup-callback
   )))|#


#|Notice that this circle is not permanently drawn on the output-pane, and
when the window is next redisplayed it vanishes. To prove this to yourself,
force the window to be redisplayed (for example by iconifying or resizing it).
At this point, you can draw the circle again yourself but it will not happen
automatically.
(capi:apply-in-pane-process
output-pane 'gp:draw-circle output-pane 100 100 50)
In order to create a permanent display, you need to provide a function to the
output-pane that is called to redraw sections of the pane when they are
exposed. This function is called the display-callback, and it is automatically
called in the correct process. When the CAPI needs to redisplay a region of an12 Creating Panes with Your Own Drawing and Input
174
output-pane, it calls that output pane’s display-callback function, passing it the
pane and the region in question.
For example, to create a pane that has a permanent circle drawn inside it, do
the following:|#
#|(defun draw-a-circle (pane x y
                           width height)
  (gp:draw-circle pane 100 100 50))
(capi:contain
 (make-instance
  'capi:output-pane
  :display-callback 'draw-a-circle)
 :best-width 300
 :best-height 300)|#
;;ABOVE WORKS--IS REDRAW AFTER MINIMIZED
#|Notice that the callback in this example ignores the region that needs redrawing and just redraws everything. This is possible because the CAPI clips the
drawing to the region that needs redisplaying, and hence only the needed part
of the drawing gets done. For maximum efficiency, it would be better to only
draw the minimum area necessary.
The arguments :best-width and :best-height specify the initial width and
height of the interface. More detail can be found in the manual page for
interface.
Now that we can create output panes with our own display functions, we can
create a new class of window by using defclass as follows.|#
#|(defclass circle-pane (output-pane)
  ()
  (:default-initargs
   :display-callback 'draw-a-circle))
(capi:contain
 (make-instance 'circle-pane))|#


;;xxx
;;TRY SETTING DISPLAY-CALLBACK DYNAMICALLY
(capi:define-interface interface-1 ()
  ((ellipse-args                                 
    :initarg :ellipse-args
    :accessor ellipse-args
    :initform nil
    :documentation "ellipse-args")
   )
  (:panes
   (editor-pane-1
    capi:editor-pane)
   (output-pane
    capi:output-pane
        :visible-min-height 350
     ;;works to redisplay if minimized   :display-callback 'DRAW-A-CIRCLE
        )
   )
  (:layouts
   (column-layout-1
    capi:column-layout
    '(editor-pane-1 output-pane)))
  (:default-initargs
   :best-height 400
   :best-width 400
   :layout 'column-layout-1
   :title "Interface-1"))

;;NOTE: (PANE X Y WIDTH HEIGHT) MUST BE ARGS
;;  FOR ALL DISPLAY-CALLBACK FUNCTIONS
(defun draw-a-circle (pane x y width height)
  (gp:draw-circle pane 100 100 50))

(defun test-display-callback ()
  (let
      ((inst (make-instance 'interface-1))
       )
    (capi:display inst)

    (with-slots (ellipse-args output-pane) inst
      (setf  (slot-value inst 'ellipse-args) '(20 20 80 50))

      ;;NOTE THE ARGS AFTER THE 'DRAW-A-CIRCLE (but MUST be these same 5?)
#|      (capi:apply-in-pane-process  
       #'(setf capi:output-pane-display-callback) 'DRAW-A-CIRCLE output-pane 30 30 100 50)|#
        (capi:apply-in-pane-process  
         #'(setf capi::redisplay ) 'DRAW-A-CIRCLE output-pane 30 30 100 50)
      ;;(my-draw-ellipse inst output-pane  20 20 80 50)
      ;;end with slots, let, test-display-callback
      )))
;;TEST
;; (test-display-callback)
;; PARTIALLY WORKS  1-writes interface def in editor, 2-draws circle in output pane, 
;; 3-WILL NOT REDISPLAY if minimized














;;MY-DRAW-ELLIPSE
;;2018-02
;;ddd
(defun my-draw-ellipse (instance output-pane x y width height &key filled foreground)
  "In  U-capi-graphics.lisp. RETURNS ellipse. Includes apply-in-pane-process."  
  (let
      ((x)
       )
    (with-slots (output-pane) instance
      (capi:apply-in-pane-process output-pane
                                  'gp:draw-ellipse output-pane x y width height :filled filled
                                  :foreground foreground)
      ;;end with-slots,let, my-draw-ellipse
      )))
;;TEST
;; (my-draw-ellipse inst output-pane  20 20 80 50)


          ;;EXTRA CIRCLE & RECTANGLE
#|          (capi:apply-in-pane-process output-pane
                                      'gp:draw-circle output-pane 100 100 50)

          (capi:apply-in-pane-process output-pane
                                      'gp:draw-rectangle output-pane 90 20 200 50 :filled t 
                                      :foreground :blue
                                      :compositing-mode :copy)|#
          #|  (gp:draw-image pixmap (capi:capi-object-property pane 'the-image)
                 10 5 :compositing-mode mode)
  (gp:copy-area pane pixmap x y 200 50 0 0)|#





;;OLD BUT HAS EGs that might be useful IN PINBOARD-LAYOUT
#|(capi:define-interface TEST-NetView-Interface ()
  ()
  (:PANES
   (editor-pane-1
    capi:rich-text-pane
    :visible-max-height 20
    )
   (editor-pane-2
    capi:rich-text-pane
        :visible-max-height 20
    )
   (editor-pane-3
    capi:rich-text-pane
        :visible-max-height 20
    )
   ;;OUTPUT PANE
   (output-pane
    capi:output-pane
    :visible-min-height 500
    )
   (arrow-pinboard-object-1
    capi:arrow-pinboard-object
    :x 50 :y 100
    :width 50
    :height 50)
   (double-headed-arrow-pinboard-object-1
    capi:double-headed-arrow-pinboard-object
    :x 300 :y 200
    :width 100
    :height 50)
   (ellipse-1
    capi:ellipse
    )
   (expandable-item-pinboard-object-1
    capi:expandable-item-pinboard-object
    :text "Expandable-Item-Pinboard-Object-1"
    :x 200 :y 200
    )
   (labelled-line-pinboard-object-1
    capi:labelled-line-pinboard-object
    :width 50
    :height 50)
#|   (pinboard-objects-displayer-1
    lw-gt:pinboard-objects-displayer)|#
   (rectangle-1
    capi:rectangle
    :accepts-focus-p T
    :default-height 30
    :default-width 50
    :filled T
    :x 100 :y 100
    ;;The graphics-state parameters transform, foreground, background, operation, thickness, scale-thickness, dashed, dash, linejoint-style, mask, pattern, shape-mode and compositing-mode are used. 
    :graphics-args  '(:foreground :yellow) ;;works to fill
 ;; error: (gp:port-graphics-state '(:background :yellow)) ;; ;; (gp:port-graphics-state '(:background :yellow))
    )   

   (drawn-pinboard-object-1
    capi:drawn-pinboard-object)
   (push-button-panel-1
    capi:push-button-panel
    :items '("Push-Button-Panel-1" "Button 2" "Button 3")
    :max-width t
    :max-height t)
   ;;END PANES
   )
  (:LAYOUTS
   (column-layout-1
    capi:column-layout
    '(editor-pane-1 editor-pane-2 editor-pane-3 output-pane pinboard-layout-1 row-layout-1))
   (pinboard-layout-1
    capi:pinboard-layout
    '(arrow-pinboard-object-1 double-headed-arrow-pinboard-object-1 ellipse-1 expandable-item-pinboard-object-1 labelled-line-pinboard-object-1  rectangle-1 drawn-pinboard-object-1))
   (row-layout-1
    capi:row-layout
    '(push-button-panel-1 )))
  (:menu-bar menu-1)
  (:MENUS
   (menu-1
    "Menu-1"
    ("Item-1"
     "Item-2"
     "Item-3")))
  (:DEFAULT-INITARGS
   :best-height 800
   :best-width 1000
   :internal-border 10
   :background :red
   :layout 'column-layout-1
   :title "Net View Interface"))|#


#|(defun compositing-mode-example-draw-image-with-compositing-mode (pane pixmap mode background x y)
  (gp:draw-rectangle pixmap 0 0 200 50 :filled t 
                     :foreground background
                     :compositing-mode :copy)
  (gp:draw-image pixmap (capi:capi-object-property pane 'the-image)
                 10 5 :compositing-mode mode)
  (gp:copy-area pane pixmap x y 200 50 0 0))|#


