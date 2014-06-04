#|
  This file is a part of eazy-gtk project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  private utility which offers an interactive 2d GUI canvas with  a minimum set of features like scrolling and dragging the view.

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage eazy-gtk-asd
  (:use :cl :asdf))
(in-package :eazy-gtk-asd)

(defsystem eazy-gtk
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:anaphora
               :cl-gtk2-cairo
               :cl-gtk2-glib
               :cl-gtk2-gdk
               :cl-gtk2-pango
               :cl-gtk2-gtk
               :iterate
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "eazy-gtk")
				 (:file :events)
				 (:file :graphics))))
  :description "private utility which offers an interactive 2d GUI canvas with  a minimum set of features like scrolling and dragging the view."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op eazy-gtk-test))))
