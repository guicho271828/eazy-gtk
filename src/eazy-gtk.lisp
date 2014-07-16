#|
This file is a part of eazy-gtk project.
Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage eazy-gtk
  (:use :cl
        :annot.doc
        :anaphora
        :iterate
        :alexandria
        :gtk
        :gdk
        :gobject
        :guicho-geometry
        :annot.eval-when)
  (:shadow :scale :rotate :range)
  (:export :apply-current-position))
(in-package :eazy-gtk)

;; blah blah blah.
