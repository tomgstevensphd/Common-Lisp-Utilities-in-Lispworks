;; **************************** U-graph-panes.lisp ************
;;


;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:graph-pane.lisp,v 1.3.12.1 2014/05/27 20:55:58 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; FROM:   EXAMPLES/CAPI/GRAPHICS/GRAPH-PANE.LISP
;;
;; This example demonstrates the uses of graph-panes in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-GRAPH-PANE)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


(capi:define-interface graph-pane-test ()
  ()
  (:panes
   (no-selection-graph-pane
    capi:graph-pane
    :title "No Selection:"
    :interaction :no-selection
    :roots '(1)
    :children-function 'children-function
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (single-selection-graph-pane
    capi:graph-pane
    :title "Single Selection:"
    :interaction :single-selection
    :roots '(1)
    :children-function 'children-function
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (multiple-selection-graph-pane
    capi:graph-pane
    :title "Multiple Selection:"
    :interaction :multiple-selection
    :roots '(1)
    :children-function 'children-function
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (extended-selection-graph-pane
    capi:graph-pane
    :title "Extended Selection:"
    :interaction :extended-selection
    :roots '(1)
    :children-function 'children-function
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (advanced-graph-pane
    capi:graph-pane
    :interaction :no-selection
    :title "Advanced Graph:"
    :roots '(1)
    :children-function 'children-function
    :node-pane-function 'make-pane-for-node
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (collector
    capi:collector-pane))
  (:layouts
   (row
    capi:row-layout
    '(no-selection-graph-pane single-selection-graph-pane
			     multiple-selection-graph-pane
			     extended-selection-graph-pane))
   (main-layout
    capi:column-layout
    '(row advanced-graph-pane collector)))
  (:default-initargs
   :layout 'main-layout
   :title "Graph Pane Test"
   :best-width 700
   :best-height 550))


;;----------------------------------------------------------------------------
;; A generic callback
;;----------------------------------------------------------------------------

(defun graph-pane-callback (type data interface)
  (with-slots (collector) interface
    (format (capi:collector-pane-stream collector)
            "~S: ~A ~S ~%"
            (capi:interface-title interface) type data)))

(defun graph-pane-selection-callback (&rest args)
  (apply 'graph-pane-callback "selected" args))

(defun graph-pane-action-callback (&rest args)
  (apply 'graph-pane-callback "action on" args))

(defun graph-pane-extend-callback (&rest args)
  (apply 'graph-pane-callback "extended" args))

(defun graph-pane-retract-callback (&rest args)
  (apply 'graph-pane-callback "retract" args))


;;----------------------------------------------------------------------------
;; Graph default routines
;;----------------------------------------------------------------------------

(defun make-pane-for-node (graph-pane node)
  (declare (ignore graph-pane))
  (cond
   ((< node 4)
    (make-instance 'capi:push-button :data node))
   ((< node 8)
    (make-instance 'capi:check-button :data node))
   ((eq (mod node 2) 1)
    (make-instance 'capi:text-input-pane :text (princ-to-string node)))
   (t
    (make-instance 'capi:item-pinboard-object :data node))))

(defun children-function (x)
  (when (< x 8)
    (list (* x 2) (1+ (* x 2)))))


;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-graph-pane ()
  (capi:display (make-instance 'graph-pane-test)))
;;TEST
;;  (test-graph-pane)
















#|
;;BELOW WAS ALL COPIED TO H-graph-panes.lisp
;;XXX============================== HELP =================================
;;FROM CAPI LW7.0 UM
#|
5.5 GRAPH PANES
Another kind of choice is the graph-pane. This is a special pane that can draw
graphs, whose nodes and edges can be selected, and for which callbacks can
be specified, as usual.
While graph-pane is a subclass of choice and hence collection, the concept
of collection items is not applicable to a graph. Instead, a graph-pane has a list
of "roots" (user-supplied arbitrary objects) which are specified by the initarg
:roots and can be accessed later by graph-pane-roots, and a children-function. The roots define the initial nodes, and when the user expands a node, the
children-function is called to compute the children, which is a list of more
items, which specify the children nodes of the expanded node. Thus the actual
items in the graph are changed as nodes are expanded or collapsed.
The concepts of selection, that is the functions choice-selected-items and
so on, are applicable to graph-pane.5 Choices - panes with items
56
Here is a simple example of a graph pane. It draws a small rooted tree:|#
(contain
 (make-instance
  'graph-pane
  :roots '(1)
  :children-function
  #'(lambda (x)
      (when (< x 8)
        (list (* 2 x) (1+ (* 2 x)))))))
Figure 5.7 A graph pane

#|The graph pane is supplied with a :children-function which it uses to calculate the children of the root node, and from those children it continues to calculate more children until the termination condition is reached. For more details of this, see the manual page for graph-pane.

GRAPH-PANE provides a gesture which expands or collapses a node, depending on its current state. Click on the circle alongside the node to expand or collapse it.

page 57
You can associate selection, retraction, extension, and action callbacks with any or all elements of a graph. Here is a simple graph pane that has an action callback on its nodes.
First we need a pane which will display the callback messages. Executing the following form to create this pane:|#
(defvar *the-collector*
  (contain (make-instance 'collector-pane)))
;;Then, define the following four callback functions:
(defun test-action-callback (&rest args)
  (format (collector-pane-stream
           *the-collector*) "Action"))
(defun test-selection-callback (&rest args)
  (format (collector-pane-stream *the-collector*)
          "Selection"))
(defun test-extend-callback (&rest args)
  (format (collector-pane-stream *the-collector*)
          "Extend"))
(defun test-retract-callback (&rest args)
  (format (collector-pane-stream *the-collector*)
          "Retract"))
;;Now create an EXTENDED SELECTION GRAPH PANE which uses each of these callbacks, the callback used depending on the action taken:
(contain
 (make-instance
  'graph-pane
  :interaction :extended-selection
  :roots '(1)
  :children-function
  #'(lambda (x)
      (when (< x 8)
        (list (* 2 x) (1+ (* 2 x)))))
  :action-callback 'test-action-callback
  :selection-callback 'test-selection-callback
  :extend-callback 'test-extend-callback
  :retract-callback 'test-retract-callback))
#|The selection callback function is called whenever any node in the graph is selected.

The extension callback function is called when the selection is extended by middle clicking on another node (thus selecting it too).
The retract callback function is called whenever an already selected node is deselected.
The action callback function is called whenever an action is performed on a node (that is, whenever it gets a double-click, or Return is pressed while the node is selected).

5.5.1 CHANGING THE GRAPHICS IN THE GRAPH

GRAPH-PANE is actually a subclass of PINBOARD-LAYOUT, and displays the graph using pinboard-objects. You can specify the class of these pinboardobjects, as well as a function to actually create the object for each node. This ALLOWS YOU TO MODIFY THE APPEARANCE OF THE GRAPH WITHOUT AFFECTING or accessing the topology of the graph.

5.5.2 CONTROLLING THE LAYOUT
The roots of the graph are placed at one side of the panes and the graph grows into the pane. The side on WHICH THE ROOTS ARE PLACED is defined by the layoutfunction and accessor graph-pane-layout-function, which takes one of the
keyword values :left-right, :top-down, :right-left and :bottom-up,
where the first word in a keyword is the side where the roots are placed. 

There is also an accessor GRAPH-PANE-DIRECTION, which maps :forward to/from :left-right and :left-right, and maps :backward to/from :right-left and :bottom-up, which makes it easier to set the direction WITHOUT changing the vertical/horizontal dimension.

5.5.3 ACCESSING THE TOPOLOGY OF THE GRAPH
The TOPOLOGY of the graph is represented by GRAPH-NODE objects and GRAPHEDGE objects. The list of graph-nodes and graph-edges of the graph-pane can be found by GRAPH-PANE-EDGES and GRAPH-PANE-NODES. 
Note, however, that these are subject to change as the user interacts with the graph.

You can find the node associated with an item (if any) by using FIND-GRAPHNODE. 
You can find the children of a supplied node by 
 Option panes
page 59
You can find the edges from the node (that is, to its children) by the reader GRAPH-NODE-OUT-EDGES, and edges in by GRAPH-NODE-IN-EDGES. 
You can also SEARCH FOR AN EDGE BETWEEN A PARENT AND CHILD by FIND-GRAPH-EDGE.
From a GRAPH-EDGE, you can find the the parent and child that are connected by it by the accessors GRAPH-EDGE-FROM and GRAPH-EDGE-TO respectively. 
It is possible to SELECT SPECIFIC NODES by GRAPH-PANE-SELECT-GRAPH-NODES, which takes a predicate that is applied to all the nodes.

You can find the GEOMETRY OF A NODE, that is the part of the pane occupied by the pinboard-object that is associated with the node, by the graph-node readers GRAPH-NODE-X, GRAPH-NODE-Y, GRAPH-NODE-HEIGHT and GRAPHNODE-WIDTH. 
You can find whether a POINT in the pane IS WITHIN THE AREA OF A GRAPH OBJECT, either a graph-node or graph-edge, by using GRAPH-PANEOBJECT-AT-POSITION.

It is possible to modify the graph explicitly by GRAPH-PANE-DELETE-OBJECT, 
GRAPH-PANE-DELETE-OBJECTS, GRAPH-PANE-DELETE-SELECTED-OBJECTS
and GRAPH-PANE-ADD-GRAPH-NODE. 
However, that WILL BE OVERRIDDEN NEXT TIME THE GRAPH-PANE COMPUTES THE LAYOUT.
The USER CAN INTERACTIVELY MOVE NODES (and hence also edges) in the graph.
 If you need to know when that happens, you make a subclass of graph-pane,
and then specialize graph-pane-update-moved-objects on it.

;;FROM THE CAPI UM REFERENCE SECTION:
Page 495

GRAPH-PANE CLASS
Summary A graph pane is a pane that displays a hierarchy of items in a graph.495
Package capi
Superclasses simple-pinboard-layout choice
Subclasses simple-network-pane 

INITARGS
 :roots The roots of the graph.
:children-function, Returns the children of a node.
:layout-function, A keyword denoting how to layout the nodes.
:layout-x-adjust The adjust value for the x direction.
:layout-y-adjust The adjust value for the y direction.
:node-pinboard-class The class of pane to represent nodes.
:edge-pinboard-class The class of pane to represent edges.
:node-pane-function A function to return a pane for each node.

ACCESSORS 
graph-pane-layout-function
graph-pane-roots21 CAPI Reference Entries
496
Description A graph pane calculates the items of the graph by calling the
children-function on each of its roots, and then calling it again
on each of the children recursively until no more children are
found. The children-function gets called with an item of the
graph and should return a list of the children of that item.
Each item is represented by a node in the graph.
The layout-function tells the graph pane how to lay out its
nodes. It can be one these values:
:left-right Lay the graph out from the left to the right.
:top-down Lay the graph out from the top down.
:right-left Lay the graph out from the right to the left.
:bottom-up Lay the graph out from the bottom up.
layout-x-adjust and layout-y-adjust act on the underlying
layout to decide where to place the nodes. The values should
be a keyword or a list of the form (keyword n) where n is an
integer. These values of adjust are interpreted as by paneadjusted-position. :top is the default for layout-y-adjust
and :left is the default for layout-x-adjust.
When a graph pane wants to display nodes and edges, it creates instances of node-pinboard-class and edge-pinboard-class which default to item-pinboard-object and line-pinboard-object respectively.

 These classes must be subclasses of simple-pane or pinboard-object, and there are some
examples of the use of these keywords below.
The NODE-PANE-FUNCTION is called to create a pane for each node, and by default it creates an instance of node-pinboard-class. It gets passed the graph pane and the item corresponding to the
node, and should return an instance of a subclass of simplepane or pinboard-object. 
If you use your own class which has its own geometry requirements, you should define a calculate-constraints method for it, which should use with-geometry on the object to set %min-width% and
%width% to the desired width, and %height% and %min-height% to the desired height. See the example in:
(example-edit-file "capi/graphics/circled-graph-nodes")

TO EXPAND OR CONTRACT A NODE, the user clicks on the circle next to the node. An expandable node has an unfilled circle and a collapsible node has a filled circle.

GRAPH-PANE IS A SUBCLASS OF CHOICe, so for details of its selection handling, see choice.
The HIGHLIGHTING OF THE CHILDREN is controlled as described for pinboard-layout, but for graph-pane the default value of highlight-style is :standard.
Notes The output-pane initarg :drawing-mode controls quality of drawing in a graph-pane, including anti-aliasing of any text displayed on Microsoft Windows and GTK+.
|#
;;EXAMPLE 
(defun node-children (node)
  (when (< node 16)
    (list (* node 2)
          (1+ (* node 2)))))
(setq graph
      (capi:contain
       (make-instance 'capi:graph-pane
                      :roots '(1)
                      :children-function
                      'node-children)
       :best-width 300 :best-height 400))
(capi:apply-in-pane-process
 graph #'(setf capi:graph-pane-roots) '(2 6) graph)

(capi:contain
 (make-instance 'capi:graph-pane
                :roots '(1)
                :children-function
                'node-children
                :layout-function :top-down)
 :best-width 300 :best-height 400)
(capi:contain
 (make-instance 'capi:graph-pane
                :roots '(1)
                :children-function
                'node-children
                :layout-function :top-down
                :layout-x-adjust :left)
 :best-width 300 :best-height 400)
;;This example demonstrates a different style of graph output with right-angle edges and parent nodes being adjusted towards the top instead of at the center.
(capi:contain
 (make-instance
  'capi:graph-pane
  :roots '(1)
  :children-function 'node-children
  :layout-y-adjust '(:top 10)
  :edge-pinboard-class
  'capi:right-angle-line-pinboard-object)
 :best-width 300
 :best-height 400)
;;This example demonstrates the use of :node-pinboard-class to specify that the nodes are drawn as push buttons.
(capi:contain
 (make-instance
  'capi:graph-pane
  :roots '(1)
  :children-function 'node-children
  :node-pinboard-class 'capi:push-button)
 :best-width 300
 :best-height 400)
;;There are more examples here: p499 
(example-edit-file "capi/graphics/*graph*")
#|
XXX
SEE ALSO 
find-graph-edge
find-graph-node
graph-edge
graph-node
graph-node-children
graph-pane-add-graph-node
graph-pane-delete-object
graph-pane-delete-objects
graph-pane-delete-selected-objects
graph-pane-direction
graph-pane-edges
graph-pane-nodes
graph-pane-object-at-position
graph-pane-select-graph-nodes
graph-pane-update-moved-objects
*maximum-moving-objects-to-track-edges*
output-pane
“CAPI elements” on page 2
Chapter 5, “Choices - panes with items”
Chapter 12, “Creating Panes with Your Own Drawing and
Input

|#
|#