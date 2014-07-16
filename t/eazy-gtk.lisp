#|
  This file is a part of eazy-gtk project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage eazy-gtk-test
  (:use :cl
        :guicho-geometry
        :iterate
        :eazy-gtk
        :function-cache
        :fiveam)
  (:shadow :fail))
(in-package :eazy-gtk-test)

(def-suite :eazy-gtk)
(in-suite :eazy-gtk)

;; recursion and iteration is basically the same thing, theres just the difference
;; in how the continuation is saved in the memory. In recursion it is on the
;; stack. In iteration it is on the program counter.
(defun decasteljau (points s)
  (case (length points)
    (0 (error "no points!"))
    (1 (first points))
    (t (decasteljau
        (iter (for p1 in points) ;; iterate over the list
              (for p2 previous p1)
              (unless p2 (next-iteration))
              (collect (add (scale-vector p1 (- 1 s))
                            (scale-vector p2 s))))
        s))))


;; deadly inefficient, memoization needed
(defun binom1 (n i)
  (if (or (= i n) (= i 0) (= n 0))
      1
      (+ (binom1 (1- n) (1- i))
         (binom1 (1- n) i))))

;; (time (binom1 32 16))
;; Evaluation took:
;;   76.856 seconds of real time
;;   77.088123 seconds of total run time (77.022431 user, 0.065692 system)
;;   100.30% CPU
;;   215,207,212,869 processor cycles
;;   491,936 bytes consed

;; 601080390

;; now the function-cache:defcached provides a higher-level abstraction macro for the memoization.
;; In Common Lisp, I don't have to ever write a hash-table-based memoization or some kind.
;; Instead, macros write the memoized program for me.
(defcached binom (n i)
  (if (or (= i n) (= i 0) (= n 0))
      1
      (+ (binom (1- n) (1- i))
         (binom (1- n) i))))

;; (time (binom 32 16))
;; Evaluation took:
;;   0.029 seconds of real time
;;   0.029575 seconds of total run time (0.029562 user, 0.000013 system)
;;   103.45% CPU
;;   50 lambdas converted
;;   82,912,456 processor cycles
;;   2,897,264 bytes consed

;; 601080390

;; the macro expands into:
;; (PROGN
;;  (DEFVAR *BINOM-CACHE* NIL)
;;  (PUSHNEW '*BINOM-CACHE* *CACHE-NAMES*)
;;  (SETF *BINOM-CACHE*
;;          (MAKE-INSTANCE 'HASH-TABLE-FUNCTION-CACHE :BODY-FN
;;                         (LAMBDA (N I)
;;                           (IF (OR (= I N) (= I 0) (= N 0))
;;                               1
;;                               (+ (BINOM (1- N) (1- I)) (BINOM (1- N) I))))
;;                         :NAME 'BINOM :LAMBDA-LIST '(N I) :SHARED-RESULTS? NIL
;;                         :CACHED-RESULTS NIL))
;;  (DEFUN BINOM (N I) NIL (FUNCTION-CACHE::CACHER *BINOM-CACHE* (LIST N I))))


;; bernstein polynomial basis
(defun bernstein (n i s)
  (* (binom n i) (expt s i) (expt (- 1 s) (- n i))))

;; compute a bezier point directly via bernstein basis
(defun direct-bezier (points s)
  (iter (with n = (length points))
        (with result = (2dv 0 0))
        (for i below n)
        (for p in points)
        (setf result (add result (scale-vector p (bernstein n i s))))
        (finally (return result))))

;; sample code

(defun draw-point (v &optional (size 0.5))
  (cairo:arc
   (- (x-of v) size)
   (- (y-of v) size)
   (* 2 size) 0 (* 2 pi)))

(defvar *points* (list (2dv 0 0)))
(defvar *prev* (2dv 0 0))
(defun myclick (canvas e)
  (declare (ignorable canvas e))
  (let ((p (2dv (gdk:event-button-x e) (gdk:event-button-y e))))
    (when (< (distance *prev* p) (scaling-factor))
      (push (user-space p) *points*)
      (format *shared-output*  "~&point ~a was added" p))
    (setf *prev* p)
    (button-release canvas e)))

(defun reflesh (canvas &optional callback)
  (declare (ignorable callback))
  (with-context (w h) canvas
    (with-push-group ;; double buffering
      (with-saved-context 
        ;; scale and translate with the saved position (moves with the mouse)
        (apply-current-position)
        (cairo:set-line-width 0.1)

        ;; draw background
        (cairo:set-source-rgba 0 0 0 1)
        (cairo:set-operator :source)
        (cairo:paint)
        ;; draws field
        (cairo:set-source-rgba 1 1 1 0.3)
        (let ((size 10))
          (iter (for x from 0 to 100 by size)
                (iter (for y from 0 to 100 by size)
                      (cairo:rectangle x y size size)
                      (cairo:stroke))))
        (when callback
          (funcall callback w h))))))

(defun draw-bezier (w h)
  (declare (ignore w h))
  ;; draw bezier curve
  (iter (for s from 0.0 to 1.0 by 0.01)
        (with-saved-context
          (cairo:set-source-rgba 1 0 0 0.5) ;; red
          (draw-point (decasteljau *points* s))
          (cairo:fill-path))
        (with-saved-context
          (cairo:set-source-rgba 0 1 0 0.5) ;; green, larger
          (draw-point (direct-bezier *points* s) 1)
          (cairo:fill-path))))

(test draw-bezier
  (finishes
   (main (lambda (c) (funcall #'reflesh c #'draw-bezier))
       :button-release #'myclick)))

;; draw bernstein graphs

(defun draw-bernstein (w h)
  (declare (ignore w h))
  (let* ((n 7)
         (pointss
          (iter (for i below n)
                (collect
                    (iter 
                      (for j below n)
                      (for x from 0.0 by (/ 1 n))
                      (for y = (if (= i j) 1 0))
                      (collect (2dv x y)))))))
    (iter (for points in pointss)
          (iter (for s from 0.0 to 1.0 by 0.01)
                (with-saved-context
                  (cairo:set-source-rgba 1 0 0 0.5) ;; red
                  (draw-point (decasteljau points s) 0.01)
                  (cairo:fill-path))
                ;; (with-saved-context
                ;;   (cairo:set-source-rgba 0 1 0 0.5) ;; green, larger
                ;;   (draw-point (direct-bezier points s) 0.01)
                ;;   (cairo:fill-path))
                ))))

(test draw-bernstein
  (finishes
   (main (lambda (c) (funcall #'reflesh c #'draw-bernstein)))))
