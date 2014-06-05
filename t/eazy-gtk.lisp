#|
  This file is a part of eazy-gtk project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage eazy-gtk-test
  (:use :cl
        :eazy-gtk
        :fiveam))
(in-package :eazy-gtk-test)


;; blah blah blah.


;; sample code

(defun reflesh (canvas)
  (with-context (w h) canvas
    (with-push-group
      (with-saved-context 
        (cairo:set-source-rgba 0.8 0.8 0.8 1)
        (cairo:set-operator :source)
        (cairo:paint))
      ;; (with-saved-context
      ;;   (let ((factor (scaling-factor *scale*)))
      ;;     (cairo:scale factor factor))
      ;;   (with-slots (x y) *translation*
      ;;     (cairo:translate x y))

      ;;   (cairo:set-line-width 0.1)

      ;;   (cairo:rectangle 0 0 *width* *height*)
      ;;   (cairo:stroke)
      ;;   (cairo:set-source-rgb 1 0.3 0.3)
      ;;   (cairo:arc (+ 0.5 *colony-x*)
      ;;              (+ 0.5 *colony-y*) 5 0 +2pi+)
      ;;   (cairo:stroke)

      ;;   ;; draws field
      ;;   (with-iter-array (f x y) *field*
      ;;     (cairo:rectangle x y 1 1)
      ;;     (cairo:set-source-rgba
      ;;      1 1 0.3 (/ (field-food f) *field-max-food*))
      ;;     (cairo:fill-path)

      ;;     (cairo:rectangle x y 1 1)
      ;;     (let ((a (/ (field-pheromon f) *field-max-pheromon*)))
      ;;       (if (plusp a)
      ;;           (cairo:set-source-rgba 0.3 0.3 1 a)
      ;;           (cairo:set-source-rgba 0.3 1 0.3 (- a))))
      ;;     (cairo:fill-path))

      ;;   ;; draws ants
      ;;   (dolist (ant *ants*)
      ;;     (with-slots (x y) ant
      ;;       (cond
      ;;         ((and (plusp (ant-food ant)) (eql (ant-mode ant) :random-walk))
      ;;          (cairo:set-source-rgb 0 0 0))
      ;;         ((and (plusp (ant-food ant)) (eql (ant-mode ant) :heuristics))
      ;;          (cairo:set-source-rgb 0.8 0.1 0.1))
      ;;         (t
      ;;          (cairo:set-source-rgb 0.1 0.8 0.1)))
      ;;       (cairo:rectangle x y 1 1)
      ;;       (cairo:fill-path)))

      ;;   ;;draw walls
      ;;   (with-iter-array (o x y) *obstacles*
      ;;     (when o
      ;;       (cairo:rectangle x y 1 1)
      ;;       (cairo:set-source-rgba 0 0 0 0.5)
      ;;       (cairo:fill-path)))

      ;;   ;; draw status
      ;;   ;; (pango:
      ;;   )
      )))

(main #'reflesh)
