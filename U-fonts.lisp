;;******************* U-fonts.lisp ********************
;;File for predefined fonts
;;
;; FILE CONTAINS
;;LIST OF ALL MY FONTS
;; 
;; '(*font-italic-bold14times *font-italic-bold12times *font-italic-bold10times  *font-title-times20b *font-title-times18b  *font-title-times16b *font-title-times14b-RED *font-title-times12b *font-title-times11b *font-title-times10b *font-text-times12 *font-text-times11 *font-text-times10 *font-text-times9 *font-text-times8 *font-text-times7 *paraformat-center-tab5)
;;
(defparameter *my-fonts (format nil "
  IN RICH-TEXT-PANE WITH=> :CHARACTER-FORMAT
 [Attributes= :selectionp :bold :color :face :charset :italic :offset :protected :size :strikeout :underline ]
       *charformat-16B-RED *charformat-14B-RED 
       *charformat-12B-RED
    BLACK-BOLD
    *charformat-12B  *charformat-11B  *charformat-10B *charformat-9B
    BLACK NOT-BOLD
    *charformat-12  *charformat-11  *charformat-10 *charformat-9
  IN RICH-TEXT-PANE WITH=> :PARAGRAPH-FORMAT
        *paraformat-center-tab5 
  WITH GP:MAKE-FONT-DESCRIPTION=>
   *font-title-times20b
   *font-title-times18b   *font-title-times16b  *font-title-times14b
   *font-title-times12b  *font-title-times11b  *font-title-times10b 
   NOT-BOLD TEXT=>
   *font-text-times12    *font-text-times11     *font-text-times9  
   *font-text-times9  *font-text-times8 *font-text-times7
 ITALIC:  *font-italic-bold14times  *font-italic-bold12times
                  *font-italic-bold10times)") "LIST OF ALL MY FONTS for both :character-format and gp:make-font-description")

;;FOR :CHARACTER-FORMAT (IN capi:rich-text-pane, ETC) FONTS
;; (:selectionp :bold :color :face :charset :italic :offset :protected :size :strikeout :underline)
(defparameter *charformat-16B-RED '(:face "times-new-roman" :size 16 :bold T :color :RED) "In rich-text-pane :character-format init-arg")
(defparameter *charformat-14B-RED '(:size 14 :bold T :color :RED) "In rich-text-pane :character-format init-arg")
(defparameter *charformat-12B-RED '(:size 12 :bold T :color :RED) "In rich-text-pane :character-format init-arg")
(defparameter *charformat-12B '(:size 12 :bold T :color :black) "In rich-text-pane :character-format init-arg")
(defparameter *charformat-11B '(:size 11 :bold T :color :black) "In rich-text-pane :character-format init-arg")
(defparameter *charformat-10B '(:face "times-new-roman" :size 10 :bold T :color :black) "In rich-text-pane :character-format init-arg")
(defparameter *charformat-9B '(:size 9  :bold T :color :black) "In rich-text-pane :character-format init-arg")
;;NOT-BOLD
(defparameter *charformat-12 '(:size 12  :color :black) "In rich-text-pane :character-format init-arg")
(defparameter *charformat-11 '(:size 11  :color :black) "In rich-text-pane :character-format init-arg")
(defparameter *charformat-10 '(:face "times-new-roman" :size 10  :color :black) "In rich-text-pane :character-format init-arg")
(defparameter *charformat-9 '(:size 9  :color :black) "In rich-text-pane :character-format init-arg")

;;FOR :PARAGRAPH-FORMAT  (IN capi:rich-text-pane) PANE PARAGRAPHS
#|      (list :alignment :center  ;; :left :right
                           ;;no effect?  :start-indent 20
                            ;;no effect? :offset-indent 20
                             ;;  :relative-indent 1.0  ;;relative indent for rest of paragraphs
                             :tab-stops  '(5 10 15 20)
                             :numbering nil 
                             ;;OR :bullet, :arabic, :lowercase,:uppercase,
                             :lower-roman or :upper-roman. )|#
(defparameter *paraformat-center-tab5  (list :alignment :center 
   :tab-stops  '(5 10 15 20)) "Used in rich-text-pane with init-arg :paragraph-format")

;;FOR GP:MAKE-FONT-DESCRIPTION FONTS
;;Italic bold fonts
(defparameter *font-italic-bold14times (gp:make-font-description :family "times new roman" :size 14 :weight  :bold :slant :italic  :underline nil))
(defparameter *font-italic-bold12times (gp:make-font-description :family "times new roman" :size 12 :weight  :bold :slant :italic  :underline nil))
(defparameter *font-italic-bold10times (gp:make-font-description :family "times new roman" :size 10 :weight  :bold :slant :italic  :underline nil))

;;TITLE fonts--bold and size 10-20
(defparameter *font-title-times20b (gp:make-font-description :weight :bold :family "times new roman" :size 20))
(defparameter *font-title-times18b (gp:make-font-description :weight :bold :family "times new roman" :size 18))
(defparameter *font-title-times16b (gp:make-font-description :weight :bold :family "times new roman" :size 16))
(defparameter *font-title-times14b (gp:make-font-description :weight :bold :family "times new roman" :size 14))
;;red
(defparameter *font-title-times14b-RED (gp:make-font-description :weight :bold :family "times new roman" :size 14 :COLOR :RED))
(defparameter *font-title-times12b (gp:make-font-description :weight :bold :family "times new roman" :size 12))
(defparameter *font-title-times11b (gp:make-font-description :weight :bold :family "times new roman" :size 11))
(defparameter *font-title-times10b (gp:make-font-description :weight :bold :family "times new roman" :size 10))
;;TEXT :normal times 7-12
(defparameter *font-text-times12 (gp:make-font-description :weight :normal :family "times new roman" :size 12))
(defparameter *font-text-times11 (gp:make-font-description :weight :normal :family "times new roman" :size 11))
(defparameter *font-text-times10 (gp:make-font-description :weight :normal :family "times new roman" :size 10))
(defparameter *font-text-times9 (gp:make-font-description :weight :normal :family "times new roman" :size 9))
(defparameter *font-text-times8 (gp:make-font-description :weight :normal :family "times new roman" :size 8))
(defparameter *font-text-times7 (gp:make-font-description :weight :normal :family "times new roman" :size 7))