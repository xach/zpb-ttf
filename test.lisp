(defpackage #:zpb-ttf-test
  (:use :cl)
  (:import-from :zpb-ttf
                #:on-curve-p
                #:x #:y
                #:do-contour-segments*
                #:do-contour-segments
                #:explicit-contour-points)
  (:local-nicknames (:z :zpb-ttf)))
(in-package #:zpb-ttf-test)

(defmacro contour (&rest points)
  `(make-array ,(length points)
               :initial-contents
               (list ,@ (loop for (x y c) in points
                              collect `(z::make-control-point ,x ,y ,c)))))
(defun point= (a b)
  (or (and (not a) (not b))
      (and (typep a 'z::control-point)
           (typep b 'z::control-point)
           (eql (on-curve-p a) (on-curve-p b))
           (eql (x a) (x b))
           (eql (y a) (y b)))))

(defun contour= (a b)
  (and (= (length a) (length b))
       (loop for a across a
             for b across b
             always (point= a b))))

(defmacro check-dcs* (contour &body points)
  `(let ((contour ,contour)
         (points ',points))
     (flet ((next-point ()
              (let ((x (pop points)))
                (when x
                  (destructuring-bind (x y &optional c) x
                    (z::make-control-point x y c ))))))
       (do-contour-segments* (b c) contour
         (assert (point= b (next-point)))
         (assert (point= c (next-point))))
       (assert (null points)))
     t))

(check-dcs* #())
;; normal contour
(check-dcs* (contour (0 0 t) (1 2) (3 4 t) (5 6))
  (1 2) (3 4 t)
  (5 6) (0 0 t))

;; starts on control point
(check-dcs* (contour (1 2) (3 4 t) (5 6) (0 0 t))
  (1 2) (3 4 t)
  (5 6) (0 0 t))

;; only control points
(check-dcs* (contour (0 0) (2 2) (4 0) (2 -2))
  (0 0) (1 1 t)
  (2 2) (3 1 t)
  (4 0) (3 -1 t)
  (2 -2) (1 -1 t))

(defmacro check-dcs (contour &body points)
  `(let ((contour ,contour)
         (points ',points))
     (flet ((next-point ()
              (let ((x (pop points)))
                (when x
                  (destructuring-bind (x y &optional c) x
                    (z::make-control-point x y c ))))))
       (do-contour-segments (a b c) contour
         (assert (point= a (next-point)))
         (assert (point= b (next-point)))
         (assert (point= c (next-point))))
       (assert (null points)))
     t))

(check-dcs #())

;; normal contour
(check-dcs (contour (0 0 t) (1 2) (3 4 t) (5 6))
  (0 0 t) (1 2) (3 4 t)
  (3 4 t) (5 6) (0 0 t))

;; starts on control point
(check-dcs (contour (1 2) (3 4 t) (5 6) (0 0 t))
  (0 0 t) (1 2) (3 4 t)
  (3 4 t) (5 6) (0 0 t))

;; only control points
(check-dcs (contour (0 0) (2 2) (4 0) (2 -2))
  (1 -1 t) (0 0) (1 1 t)
  (1 1 t) (2 2) (3 1 t)
  (3 1 t) (4 0) (3 -1 t)
  (3 -1 t) (2 -2) (1 -1 t))

(assert (contour= (contour (0 1) (2 3 t))
                  (contour (0 1) (2 3 t))))

(assert (not (contour= (contour (0 1 t) (2 3 t))
                       (contour (0 1) (2 3 t)))))
(assert (not (contour= (contour (0 1))
                       (contour (0 1) (2 3 t)))))

(assert (equalp (explicit-contour-points #()) #()))

(assert
 (contour= (explicit-contour-points (contour (0 0 t) (1 2) (3 4 t) (5 6)))
           (contour (0 0 t) (1 2) (3 4 t) (5 6))))

(assert
 (contour= (explicit-contour-points (contour (1 2) (3 4 t) (5 6) (0 0 t)))
           (contour (1 2) (3 4 t) (5 6) (0 0 t))))

(assert
 (contour= (explicit-contour-points (contour (0 0) (2 2) (4 0) (2 -2)))
           (contour (0 0) (1 1 t)
                    (2 2) (3 1 t)
                    (4 0) (3 -1 t)
                    (2 -2) (1 -1 t))))

