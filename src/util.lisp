;;;; util.lisp — Utility routines for zpb-ttf
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(in-package #:zpb-ttf)

(defun bounded-aref (vector index)
  "Return the element at INDEX, or the last element if INDEX is past the end."
  (if (or (null vector) (zerop (length vector)))
      0
      (aref vector (min (1- (length vector)) (max 0 index)))))

(defun (setf bounded-aref) (new-value vector index)
  (when (and vector (plusp (length vector)))
    (setf (aref vector (min (1- (length vector)) (max 0 index))) new-value)))

(defun make-transformer (a b c d e f)
  "Given the elements of the transformation matrix specified by A, B,
C, D, E, and F, return a function of two arguments that returns the
arguments transformed as multiple values. The offsets E and F are
applied unscaled, as the OpenType spec requires by default."
  (lambda (x y)
    (values (round (+ (* a x) (* c y) e))
            (round (+ (* b x) (* d y) f)))))

(defun transform-contours (fn contours)
  (loop for contour across contours do
        (loop for p across contour do
              (multiple-value-bind (new-x new-y) (funcall fn (x p) (y p))
                (setf (x p) new-x
                      (y p) new-y))))
  contours)

(defun tag->number (tag)
  "Convert the four-character string TAG to a 32-bit number based on
the CHAR-CODE of each character."
  (declare (simple-string tag))
  (loop for char across tag
        for offset from 24 downto 0 by 8
        summing (ash (char-code char) offset)))

(defun number->tag (number)
  "Convert the 32-bit NUMBER to a string of four characters based on
the CODE-CHAR of each octet in the number."
  (let ((tag (make-string 4)))
    (loop for i below 4
          for offset from 24 downto 0 by 8
          do (setf (schar tag i)
                   (code-char (ldb (byte 8 offset) number))))
    tag))
