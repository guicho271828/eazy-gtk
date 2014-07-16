
(in-package :eazy-gtk)
(annot:enable-annot-syntax)

(defun safety-wrapper (fn)
  (tagbody
   start
     (return-from safety-wrapper
       (restart-case (funcall fn)
         (close-window () (gtk-main-quit))
         (ignore ())
         (retry () (go start))))))

@export @doc "opens a new window and a canvas, then refresh it by
REFLESH-FN with certain intervals specified by
MILLISECONDS. REFLESH-FN should accept `gtk:drawing-area' as its only
argument."
(defun main (reflesh-fn 
             &key
               (milliseconds 17) (title "Main window")
               (button-press #'button-press)
               (button-release #'button-release)
               (motion-notify #'motion-notify)
               (key-press #'key-press)
               (key-release #'key-release)
               (scroll #'scroll)
               (quit #'quit))
  (gtk:within-main-loop
   (let ((window (make-instance 'gtk:gtk-window
                                :type :toplevel
                                :window-position :center
                                :default-width 400
                                :default-height 400
                                :keep-above t
                                :title title))
         (canvas (make-instance 'gtk:drawing-area
                                :default-width 300
                                :default-height 300))
         (vbox (make-instance 'gtk:v-box)))
     (gtk:container-add window vbox)
     (gtk:box-pack-start vbox canvas)
     (gtk:widget-show window)
     (push :all-events-mask
           (gdk:gdk-window-events (gtk:widget-window canvas)))
     (mapc
      (lambda (pair)
        (destructuring-bind (key fn) pair
          (gobject:connect-signal
           window key (lambda (&rest args)
                        (safety-wrapper
                         (lambda () (apply fn args)))))))
      `(("delete-event" ,quit)
        ("button-press-event" ,button-press)
        ("button-release-event" ,button-release)
        ("key-press-event" ,key-press)
        ("key-release-event" ,key-release)
        ("motion-notify-event" ,motion-notify)
        ("scroll-event" ,scroll)
        ("destroy" ,quit)))
     (gtk:gtk-main-add-timeout
      milliseconds
      (lambda (&rest args)
        @ignore args
        (safety-wrapper
         (lambda ()
           (funcall reflesh-fn canvas)))
        t)))))

(defun draw-in-context (canvas fn)
  (let ((drawable (gtk:widget-window canvas)))
    (cl-gtk2-cairo:with-gdk-context
        (ctx drawable)
      (multiple-value-bind (width height)
          (gdk:drawable-get-size drawable)
        (cairo:with-context (ctx)
          (funcall fn width height))))))

@eval-always
@export
@doc "macro for double-buffering"
(defmacro with-push-group (&body body)
  `(unwind-protect
        (progn
          (cairo:push-group)
          ,@body)
     (progn
       (cairo:pop-group-to-source)
       (cairo:paint))))

@eval-always
@export
(defmacro with-context ((&optional width height) canvas &body body)
  (once-only (canvas)
    (unless width (setf width (gensym)))
    (unless height (setf height (gensym)))
    `(draw-in-context
      ,canvas
      (lambda (,width ,height)
        (declare (ignorable ,width ,height))
        ,@body))))

@eval-always
@export
(defmacro with-saved-context (&body body)
  `(unwind-protect
        (progn
          (cairo:save)
          ,@body)
     (cairo:restore)))

@export
(defun toggle-start-stop (stepper)
  (if *stepping-id*
      (progn (glib:g-source-remove *stepping-id*)
             (setf *stepping-id* nil))
      (setf *stepping-id*
            (gtk:gtk-main-add-timeout
             (floor *step-ms*)
             (lambda ()
               (funcall stepper)
               t)))))

