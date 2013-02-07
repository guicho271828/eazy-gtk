#|
  This file is a part of easy-gtk project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage easy-gtk-test-asd
  (:use :cl :asdf))
(in-package :easy-gtk-test-asd)

(defsystem easy-gtk-test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:easy-gtk
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "easy-gtk"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
