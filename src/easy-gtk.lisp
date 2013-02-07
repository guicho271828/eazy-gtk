#|
  This file is a part of easy-gtk project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage easy-gtk
  (:use :cl
		:annot.doc
		:anaphora
		:iterate
		:alexandria
		:gtk
		:gdk
		:gobject
		:annot.eval-when)
  (:shadow :scale :rotate :range))
(in-package :easy-gtk)

;; blah blah blah.
