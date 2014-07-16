
(in-package :eazy-gtk)
(annot:enable-annot-syntax)

(declaim (notinline button-press
                    button-release
                    motion-notify
                    key-press
                    key-release
                    scroll
                    quit))

;; translation and scaling in the user space
(defvar *translation* (2dv 0 0))
(defvar *scale* 0)
(defvar *stepping-id* nil)
(eval-when (:compile-toplevel :load-toplevel :execute)
  @export
  (defvar *shared-output* *standard-output*))
(defparameter +pixel-step+ 10)
(defparameter +scaling-base+ 1.2)
@export
(defun scaling-factor (&optional (x *scale*))
  (expt +scaling-base+ x))

(defun scroll-at (x y next-scale)
  (let* ((w (2dv x y))
         (u (user-space w *translation* *scale*))
         (u-next (user-space w *translation* next-scale)))
    (setf *translation*
          (add *translation* (sub u-next u)))
    (setf *scale* next-scale)))

@export
(defun user-space (window-v &optional
                              (translation *translation*)
                              (scale *scale*))
  (sub (scale-vector window-v
                     (/ (scaling-factor scale)))
       translation))

@export
(defun window-space (user-v  &optional
                              (translation *translation*)
                              (scale *scale*))
  (scale-vector (add user-v translation)
                (scaling-factor scale)))

@export
(defvar *previous-pointer-position* nil)

@export
(defun apply-current-position ()
  "call within with-saved-context so that the current
scaling and translation is applied"
  (let ((factor (scaling-factor)))
    (cairo:scale factor factor))
  (with-slots (x y) *translation*
    (cairo:translate x y)))

@export
@doc "the default button-press function"
(defun button-press (canvas e)
  @ignorable canvas e
  (format *shared-output*
          "~%pressed  ~a state: ~a window: [~a ~a] user: ~a"
          (event-button-button e)
          (event-button-state e)
          (event-button-x e)
          (event-button-y e)
          (user-space (2dv (event-button-x e) (event-button-y e))
                      *translation* *scale*))
  (case (event-button-button e)
    (1 (setf *previous-pointer-position*
             (2dv (event-button-x e)
                  (event-button-y e))))))

@export
(defun button-release (canvas e)
  @ignorable canvas e
  (format *shared-output*
          "~%released ~a at: [~a ~a]"
          (event-button-button e)
          (event-button-x e)
          (event-button-y e))
  (setf *previous-pointer-position* nil))

@export
(defun motion-notify (canvas e)
  @ignorable canvas e
  (cond
    ((member :button1-mask (event-motion-state e))
     (let ((v *previous-pointer-position*)
           (v2 (2dv (event-motion-x e) (event-motion-y e))))
       (setf *translation*
             (add *translation*
                  (scale-vector (sub v2 v)
                                (/ (scaling-factor)))))
       (setf *previous-pointer-position* v2)))))

@export
(defun key-press (canvas e)
  @ignorable canvas e
  (case (code-char (event-key-keyval e))
    (#\+ (multiple-value-bind (width height)
             (gdk:drawable-get-size (widget-window canvas))
           (scroll-at (/ width 2) (/ height 2) (+ *scale* 1))))
    (#\- (multiple-value-bind (width height)
             (gdk:drawable-get-size (widget-window canvas))
           (scroll-at (/ width 2) (/ height 2) (- *scale* 1))))
    ;; (#\< (setf *step-ms* (* *step-ms* +scaling-base+)))
    ;; (#\> (setf *step-ms* (/ *step-ms* +scaling-base+)))
    (#\r (setf *scale* 0 *translation* (2dv 0.0d0 0.0d0)))
    (#\d
     (format *shared-output*
             "~%*scale*: ~a^~a *translation*: ~a"
             +scaling-base+
             *scale*
             *translation*))
    (t
     (case (event-key-keyval e)
       (65361
        ;;left
        (setf *translation*
              (add *translation*
                   (scale-vector (2dv +pixel-step+ 0)
                                 (/ (scaling-factor))))))
       (65363
        ;; right
        (setf *translation*
              (add *translation*
                   (scale-vector (2dv (- +pixel-step+) 0)
                                 (/ (scaling-factor))))))
       (65362
        ;; up
        (setf *translation*
              (add *translation*
                   (scale-vector (2dv 0 (- +pixel-step+))
                                 (/ (scaling-factor))))))
       (65364
        ;; down
        (setf *translation*
              (add *translation*
                   (scale-vector (2dv 0 +pixel-step+)
                                 (/ (scaling-factor))))))
       (t
        (format *shared-output*
                "~%key pressed: ~a keyval: ~a hardware: ~a"
                (code-char (event-key-keyval e))
                (event-key-keyval e)
                (event-key-hardware-keycode e))
        (force-output *shared-output*))))))

@export
(defun key-release (canvas e)
  @ignorable canvas e
  )

@export
(defun scroll (canvas e)
  @ignorable canvas e
  (format *shared-output*
          "~%scrolled: ~a at: [~a ~a]"
          (event-scroll-direction e)
          (event-scroll-x e)
          (event-scroll-y e))
  (scroll-at 
   (event-scroll-x e)
   (event-scroll-y e)
   (+ *scale*
      (case (event-scroll-direction e)
        (:up 1) (:down -1) (t 0)))))

@export
(defun quit (&rest rest)
  (declare (ignore rest))
  ;; http://www.crategus.com/books/cl-gtk/gtk-tutorial_2.html
  ;; the function leave-gtk-main is special for the Lisp binding. It calls
  ;; internally the C function gtk_main_quit(), but does some extra work to finish
  ;; the Lisp program. The C function gtk_main_quit() is available in the Lisp
  ;; binding as gtk-main-quit, but do not call this function in your code.
  (leave-gtk-main))
