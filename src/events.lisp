
(in-package :eazy-gtk)
(annot:enable-annot-syntax)

;; translation and scaling in the user space
(defvar *translation* (2dv* 0 0))
(defvar *scale* 0)
(defvar *stepping-id* nil)

(defparameter +pixel-step+ 10)
(defparameter +scaling-base+ 1.2)
(defun scaling-factor (x)
  (expt +scaling-base+ (desired x)))

(defun scroll-at (x y next-scale)
  (let* ((w (2dv-coerce x y))
         (u (user-space w *translation* *scale*))
         (u-next (user-space w *translation* next-scale)))
    (setf *translation*
          (add *translation* (sub u-next u)))
    (setf *scale* next-scale)))

(defun user-space (window-v translation scale)
  (sub (scale-vector window-v
                     (/ (scaling-factor scale)))
       translation))

(defun window-space (user-v translation scale)
  (scale-vector (add user-v translation)
                (scaling-factor scale)))

(defvar *previous-pointer-position* nil)

@export
@doc "the default button-press function"
(defun button-press (canvas e)
  @ignorable canvas e
  (format *main-thread-output*
          "~%pressed  ~a at: [~a ~a] ~a"
          (event-button-button e)
          (event-button-x e)
          (event-button-y e)
          (event-button-state e))
  
  (case (event-button-button e)
    (1 (setf *previous-pointer-position* (2dv (event-button-x e)
                                              (event-button-y e))))))

@export
(defun button-release (canvas e)
  @ignorable canvas e
  (format *main-thread-output*
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
     (let ((v2 (2dv (event-motion-x e) (event-motion-y e))))
       (setf *translation*
             (add *translation*
                  (scale-vector (sub v2 v)
                                (d/ (scaling-factor *scale*)))))
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
    (#\< (setf *step-ms* (* *step-ms* +scaling-base+)))
    (#\> (setf *step-ms* (/ *step-ms* +scaling-base+)))
    (#\r (setf *scale* 0 *translation* (2dv 0.0d0 0.0d0)))
    (#\d
     (format *main-thread-output*
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
                   (scale-vector (2dv-coerce +pixel-step+ 0)
                                 (/ (scaling-factor *scale*))))))
       (65363
        ;; right
        (setf *translation*
              (add *translation*
                   (scale-vector (2dv-coerce (- +pixel-step+) 0)
                                 (/ (scaling-factor *scale*))))))
       (65362
        ;; up
        (setf *translation*
              (add *translation*
                   (scale-vector (2dv-coerce 0 (- +pixel-step+))
                                 (/ (scaling-factor *scale*))))))
       (65364
        ;; down
        (setf *translation*
              (add *translation*
                   (scale-vector (2dv-coerce 0 +pixel-step+)
                                 (/ (scaling-factor *scale*))))))
       (t
        (format *main-thread-output*
                "~%key pressed: ~a keyval: ~a hardware: ~a"
                (code-char (event-key-keyval e))
                (event-key-keyval e)
                (event-key-hardware-keycode e))
        (force-output *main-thread-output*))))))

@export
(defun key-release (canvas e)
  @ignorable canvas e
  )

@export
(defun scroll (canvas e)
  @ignorable canvas e
  (format *main-thread-output*
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

