#|
  This file is a part of eazy-gtk project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage eazy-gtk-test-asd
  (:use :cl :asdf))
(in-package :eazy-gtk-test-asd)

(defsystem eazy-gtk-test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:eazy-gtk
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "eazy-gtk"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
