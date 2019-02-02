;;******************* U-fonts.lisp ********************
;;File for predefined fonts
;;
;;FONTS FOR FONT-DESCRIPTION
;;Italic bold fonts
(defparameter *font-italic-bold14times (gp:make-font-description :family "times new roman" :size 14 :weight  :bold :slant :italic  :underline nil))
(defparameter *font-italic-bold12times (gp:make-font-description :family "times new roman" :size 12 :weight  :bold :slant :italic  :underline nil))
(defparameter *font-italic-bold10times (gp:make-font-description :family "times new roman" :size 10 :weight  :bold :slant :italic  :underline nil))

;;TITLE fonts--bold and size 10-20
(defparameter *font-title-times20b (gp:make-font-description :weight :bold :family "times new roman" :size 20))
(defparameter *font-title-times18b (gp:make-font-description :weight :bold :family "times new roman" :size 18))
(defparameter *font-title-times16b (gp:make-font-description :weight :bold :family "times new roman" :size 16))
(defparameter *font-title-times14b (gp:make-font-description :weight :bold :family "times new roman" :size 14))
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