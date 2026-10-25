;;;; glyph.lisp — Glyph objects, metrics, and contour extraction
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(in-package #:zpb-ttf)

(defclass control-point ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (on-curve-p :initarg :on-curve-p :reader on-curve-p)))

(defun make-control-point (x y on-curve-p)
  (make-instance 'control-point
                 :x x
                 :y y
                 :on-curve-p on-curve-p))

(defmethod print-object ((control-point control-point) stream)
  (print-unreadable-object (control-point stream :type t)
    (format stream "~D,~D~:[~;*~]"
            (x control-point) (y control-point) (on-curve-p control-point))))

;;; Contour iteration macros

(defmacro do-contour-segments* ((p1 p2) contour &body body)
  (let ((length (gensym))
        (i (gensym))
        (stack (gensym))
        (next (gensym))
        (next-point (gensym "NEXT-POINT"))
        (midpoint (gensym "MIDPOINT"))
        (contour* (gensym))
        (loop (gensym "LOOP"))
        (body-tag (gensym "BODY"))
        (done-tag (gensym "DONE"))
        (mid p1)
        (end p2))
    `(let* ((,i 1)
            (,contour* ,contour)
            (,length (length ,contour*))
            ,stack ,next ,mid ,end)
       (unless (zerop ,length)
         (unless (on-curve-p (aref ,contour* 0))
           (setf ,stack (aref ,contour* 0)))
         (flet ((,next-point ()
                  (when (< ,i ,length)
                    (prog1 (aref ,contour* ,i) (incf ,i))))
                (,midpoint (p0 p1)
                  (make-control-point (/ (+ (x p0) (x p1)) 2)
                                      (/ (+ (y p0) (y p1)) 2)
                                      t)))
           (tagbody
              ,loop
              (setf ,mid nil
                    ,next (,next-point))
              (unless ,next
                (setf ,mid ,stack
                      ,end (aref ,contour* 0))
                (cond
                  ((on-curve-p ,end)
                   (go ,body-tag))
                  (,stack
                   (setf ,mid ,stack
                         ,end (,midpoint ,stack ,end))
                   (go ,body-tag))
                  (t (go ,done-tag))))
              (if (on-curve-p ,next)
                  (setf ,end ,next
                        ,mid ,stack
                        ,stack nil)
                  (cond (,stack
                         (setf ,mid ,stack
                               ,end (,midpoint ,stack ,next)
                               ,stack ,next))
                        (t
                         (setf ,stack ,next)
                         (go ,loop))))
              ,body-tag
              ,@body
              (when ,next
                (go ,loop))
              ,done-tag))))))

(defun start-of-contour (contour)
  (let ((first (aref contour 0)))
   (if (on-curve-p first)
       first
       (let ((last (aref contour (1- (length contour)))))
         (if (on-curve-p last)
             last
             (make-control-point (/ (+ (x first) (x last)) 2)
                                 (/ (+ (y first) (y last)) 2)
                                 t))))))

(defmacro do-contour-segments ((p0 p1 p2) contour &body body)
  "A contour is made up of segments. A segment may be a straight line
or a curve. For each segment, bind the P0 and P2 variables to the
start and end points of the segment. If the segment is a curve, set P1
to the control point of the curve, otherwise set P1 to NIL."
  (let ((start p0)
        (contour* (gensym "CONTOUR")))
    `(let ((,contour* ,contour))
       (when (plusp (length ,contour*))
         (let ((,start (start-of-contour ,contour*)))
           (do-contour-segments* (,p1 ,p2)
               ,contour*
             (progn ,@body)
             (setf ,start ,p2)))))))

(defun explicit-contour-points (contour)
  (let ((new-contour (make-array (length contour)
                                 :adjustable t
                                 :fill-pointer 0)))
    (when (and (plusp (length contour))
               (on-curve-p (aref contour 0)))
      (vector-push-extend (aref contour 0) new-contour))
    (do-contour-segments* (p1 p2)
        contour
      (when p1
        (vector-push-extend p1 new-contour))
      (unless (eql p2 (aref contour 0))
        (vector-push-extend p2 new-contour)))
    new-contour))

(defun explicit-contours (glyph)
  (map 'vector #'explicit-contour-points (contours glyph)))

(defparameter *empty-contours*
  (make-array 0 :element-type t))

(defparameter *empty-bounding-box*
  (make-array 4 :initial-element 0 :element-type '(signed-byte 16)))

(defun empty-bounding-box ()
  (copy-seq *empty-bounding-box*))

(defun empty-contours ()
  (copy-seq *empty-contours*))

;;; Maximum component nesting depth (cf. maxComponentDepth in 'maxp').
(defparameter *max-compound-glyph-depth* 16)

(defvar *compound-glyph-stack* '()
  "Glyph indices of the compound glyphs whose contours are being read.")

;;; The Glyph class

(defclass glyph ()
  ((font-loader
    :initarg :font-loader
    :reader font-loader)
   (font-index
    :initarg :font-index
    :accessor font-index)
   (code-point
    :initarg :code-point
    :accessor code-point)
   (contours
    :initarg :contours
    :accessor contours)
   (bounding-box
    :initarg :bounding-box
    :accessor bounding-box)))

(defmethod print-object ((glyph glyph) stream)
  (print-unreadable-object (glyph stream :type t :identity nil)
    (format stream "GID=~D U+~4,'0X"
            (font-index glyph)
            (code-point glyph))))

;;; Glyph metrics

(defgeneric left-side-bearing (object)
  (:method ((glyph glyph))
    (bounded-aref (left-side-bearings (font-loader glyph))
                  (font-index glyph))))

(defgeneric advance-width (object)
  (:method ((glyph glyph))
    (bounded-aref (advance-widths (font-loader glyph))
                  (font-index glyph))))

(defgeneric top-side-bearing (object)
  (:method ((glyph glyph))
    (let ((loader (font-loader glyph)))
      (if (vmtx-missing-p loader)
          (- (ascender loader) (ymax glyph))
          (bounded-aref (top-side-bearings loader)
                        (font-index glyph))))))

(defgeneric advance-height (object)
  (:method ((glyph glyph))
    (bounded-aref (advance-heights (font-loader glyph))
                  (font-index glyph))))

(defgeneric right-side-bearing (object)
  (:method ((glyph glyph))
    (- (advance-width glyph)
       (- (+ (left-side-bearing glyph) (xmax glyph))
          (xmin glyph)))))

(defgeneric kerning-offset (left right loader))

(defmethod kerning-offset ((left-glyph glyph) (right-glyph glyph)
                           (font-loader font-loader))
  (let ((key (logior (ash (font-index left-glyph) 16)
                     (font-index right-glyph))))
    (gethash key (kerning-table font-loader) 0)))

(defmethod kerning-offset ((left character) (right character)
                           (font-loader font-loader))
  (kerning-offset (find-glyph left font-loader)
                  (find-glyph right font-loader)
                  font-loader))

(defmethod kerning-offset ((left null) right font-loader)
  (declare (ignore left right font-loader))
  0)

(defmethod kerning-offset (left (right null) font-loader)
  (declare (ignore left right font-loader))
  0)

(defgeneric kerned-advance-width (object next)
  (:method ((object glyph) next)
    (+ (advance-width object)
       (kerning-offset object next (font-loader object)))))

(defgeneric contour-count (object)
  (:method (object)
    (length (contours object))))

(defgeneric contour (object index)
  (:method (object index)
    (aref (contours object) index)))

(defmacro do-contours ((contour object &optional result) &body body)
  (let ((i (gensym))
        (obj (gensym)))
    `(let ((,obj ,object))
       (dotimes (,i (contour-count ,obj) ,result)
         (let ((,contour (contour ,obj ,i)))
           ,@body)))))

(defgeneric string-bounding-box (string loader &key kerning)
  (:method (string (loader font-loader) &key (kerning t))
    (if (zerop (length string))
        (empty-bounding-box)
        (let* ((g1 (find-glyph (char string 0) loader))
               (bbox (copy-seq (bounding-box g1)))
               (x 0))
          (loop for i from 1 below (length string)
                for g2 = (find-glyph (char string i) loader)
                do (incf x (advance-width g1))
                   (when kerning
                     (incf x (kerning-offset g1 g2 loader)))
                   (let ((gb (bounding-box g2)))
                     (setf (xmin bbox) (min (xmin bbox) (+ x (xmin gb)))
                           (ymin bbox) (min (ymin bbox) (ymin gb))
                           (xmax bbox) (max (xmax bbox) (+ x (xmax gb)))
                           (ymax bbox) (max (ymax bbox) (ymax gb))))
                   (setf g1 g2))
          bbox))))

;;; Zero-copy TrueType contour extraction

(defun glyph-loca-info (loader gid)
  (let ((loca (table-info "loca" loader)))
    (if (null loca)
        (values 0 0)
        (let ((fd (font-data loader))
              (loc-off (offset loca))
              (is-short (eql (loca-offset-format loader) :short)))
          (if is-short
              (let ((off1 (* (zpb-otf:font-data-read-u16 fd (+ loc-off (* gid 2))) 2))
                    (off2 (* (zpb-otf:font-data-read-u16 fd (+ loc-off (* (1+ gid) 2))) 2)))
                (values off1 (- off2 off1)))
              (let ((off1 (zpb-otf:font-data-read-u32 fd (+ loc-off (* gid 4))))
                    (off2 (zpb-otf:font-data-read-u32 fd (+ loc-off (* (1+ gid) 4)))))
                (values off1 (- off2 off1))))))))

(defmethod initialize-bounding-box ((glyph glyph))
  (let ((loader (font-loader glyph))
        (gid (font-index glyph)))
    (multiple-value-bind (g-off len) (glyph-loca-info loader gid)
      (if (or (zerop len) (null (table-info "glyf" loader)))
          (setf (bounding-box glyph) (empty-bounding-box))
          (let* ((fd (font-data loader))
                 (pos (+ (offset (table-info "glyf" loader)) g-off))
                 (xmin (zpb-otf:font-data-read-i16 fd (+ pos 2)))
                 (ymin (zpb-otf:font-data-read-i16 fd (+ pos 4)))
                 (xmax (zpb-otf:font-data-read-i16 fd (+ pos 6)))
                 (ymax (zpb-otf:font-data-read-i16 fd (+ pos 8))))
            (setf (bounding-box glyph) (vector xmin ymin xmax ymax)))))))

(defmethod initialize-contours ((glyph glyph))
  (let ((loader (font-loader glyph))
        (gid (font-index glyph)))
    (multiple-value-bind (g-off len) (glyph-loca-info loader gid)
      (if (or (zerop len) (null (table-info "glyf" loader)))
          (setf (contours glyph) (empty-contours))
          (let* ((fd (font-data loader))
                 (pos (+ (offset (table-info "glyf" loader)) g-off))
                 (nc (zpb-otf:font-data-read-i16 fd pos)))
            (setf (contours glyph)
                  (cond
                    ((zerop nc) (empty-contours))
                    ((minusp nc)
                     (let ((*compound-glyph-stack* (cons gid *compound-glyph-stack*)))
                       (read-compound-contours-fd loader (+ pos 10))))
                    (t (read-simple-contours-fd fd (+ pos 10) nc)))))))))

(defmethod bounding-box :before ((glyph glyph))
  (unless (slot-boundp glyph 'bounding-box)
    (initialize-bounding-box glyph)))

(defmethod code-point :before ((glyph glyph))
  (unless (slot-boundp glyph 'code-point)
    (setf (code-point glyph)
          (font-index-code-point (font-index glyph) (font-loader glyph)))))

(defmethod contours :before ((glyph glyph))
  (unless (slot-boundp glyph 'contours)
    (initialize-contours glyph)))

(defun read-simple-contours-fd (fd pos contour-count)
  (when (zerop contour-count)
    (return-from read-simple-contours-fd (empty-contours)))
  (let ((end-pts (make-array contour-count :element-type '(unsigned-byte 16))))
    (dotimes (i contour-count)
      (setf (aref end-pts i) (zpb-otf:font-data-read-u16 fd pos))
      (incf pos 2))
    (let* ((n-points (1+ (aref end-pts (1- contour-count))))
           (instruction-len (zpb-otf:font-data-read-u16 fd pos)))
      (incf pos (+ 2 instruction-len))
      ;; Read flags
      (let ((flags (make-array n-points :element-type '(unsigned-byte 8)))
            (i 0))
        (loop while (< i n-points) do
              (let ((fl (zpb-otf:font-data-read-u8 fd pos)))
                (incf pos 1)
                (setf (aref flags i) fl)
                (incf i)
                (when (logbitp 3 fl) ; repeat
                  (let ((reps (zpb-otf:font-data-read-u8 fd pos)))
                    (incf pos 1)
                    (dotimes (r reps)
                      (setf (aref flags i) fl)
                      (incf i))))))
        ;; Read X coordinates
        (let ((x-coords (make-array n-points :element-type '(signed-byte 32)))
              (curr-x 0))
          (dotimes (j n-points)
            (let* ((fl (aref flags j))
                   (short-p (logbitp 1 fl))
                   (same-p (logbitp 4 fl))
                   (dx 0))
              (cond
                (short-p
                 (let ((val (zpb-otf:font-data-read-u8 fd pos)))
                   (incf pos 1)
                   (setf dx (if same-p val (- val)))))
                (t
                 (if same-p
                     (setf dx 0)
                     (progn
                       (setf dx (zpb-otf:font-data-read-i16 fd pos))
                       (incf pos 2)))))
              (incf curr-x dx)
              (setf (aref x-coords j) curr-x)))
          ;; Read Y coordinates
          (let ((y-coords (make-array n-points :element-type '(signed-byte 32)))
                (curr-y 0))
            (dotimes (j n-points)
              (let* ((fl (aref flags j))
                     (short-p (logbitp 2 fl))
                     (same-p (logbitp 5 fl))
                     (dy 0))
                (cond
                  (short-p
                   (let ((val (zpb-otf:font-data-read-u8 fd pos)))
                     (incf pos 1)
                     (setf dy (if same-p val (- val)))))
                  (t
                   (if same-p
                       (setf dy 0)
                       (progn
                         (setf dy (zpb-otf:font-data-read-i16 fd pos))
                         (incf pos 2)))))
                (incf curr-y dy)
                (setf (aref y-coords j) curr-y)))
            ;; Build contour array
            (let ((contours (make-array contour-count))
                  (start 0))
              (dotimes (c contour-count contours)
                (let* ((end (aref end-pts c))
                       (c-len (1+ (- end start)))
                       (pts (make-array c-len)))
                  (dotimes (p c-len)
                    (let ((idx (+ start p)))
                      (setf (aref pts p)
                            (make-control-point (aref x-coords idx)
                                                (aref y-coords idx)
                                                (logbitp 0 (aref flags idx))))))
                  (setf (aref contours c) pts)
                  (setf start (1+ end)))))))))))

(defun merge-contours (contours-list)
  (let* ((total (loop for c in contours-list summing (length c)))
         (merged (make-array total))
         (i 0))
    (dolist (cs contours-list merged)
      (loop for c across cs do
            (setf (aref merged i) c)
            (incf i)))))

(defun nth-contour-point (contours-list n)
  "Return the Nth point counting across CONTOURS-LIST, a list of contour vectors."
  (dolist (cs contours-list)
    (loop for c across cs
          do (if (< n (length c))
                 (return-from nth-contour-point (aref c n))
                 (decf n (length c)))))
  nil)

(defun read-compound-contours-fd (loader pos)
  (when (> (length *compound-glyph-stack*) *max-compound-glyph-depth*)
    (error 'unsupported-value
           :description "Compound glyph nesting too deep"
           :location (format nil "glyph ~D" (first *compound-glyph-stack*))
           :actual-value (length *compound-glyph-stack*)
           :expected-values (list *max-compound-glyph-depth*)))
  (let ((fd (font-data loader))
        (contours-list '()))
    (loop
     (let ((flags (zpb-otf:font-data-read-u16 fd pos))
           (glyph-index (zpb-otf:font-data-read-u16 fd (+ pos 2))))
       (incf pos 4)
       (when (member glyph-index *compound-glyph-stack*)
         (error 'unsupported-value
                :description "Recursive compound glyph"
                :location (format nil "glyph ~D" (first *compound-glyph-stack*))
                :actual-value glyph-index
                :expected-values '("a glyph that does not contain itself")))
       (let* ((sub-glyph (index-glyph glyph-index loader))
              (contours (contours sub-glyph))
              (args-words-p (logbitp 0 flags))
              (args-xy-values-p (logbitp 1 flags))
              (more-components-p (logbitp 5 flags))
              arg1 arg2)
         (cond
           ((and args-words-p args-xy-values-p)
            (setf arg1 (zpb-otf:font-data-read-i16 fd pos)
                  arg2 (zpb-otf:font-data-read-i16 fd (+ pos 2)))
            (incf pos 4))
           (args-words-p
            (setf arg1 (zpb-otf:font-data-read-u16 fd pos)
                  arg2 (zpb-otf:font-data-read-u16 fd (+ pos 2)))
            (incf pos 4))
           (args-xy-values-p
            (setf arg1 (zpb-otf:font-data-read-i8 fd pos)
                  arg2 (zpb-otf:font-data-read-i8 fd (+ pos 1)))
            (incf pos 2))
           (t
            (setf arg1 (zpb-otf:font-data-read-u8 fd pos)
                  arg2 (zpb-otf:font-data-read-u8 fd (+ pos 1)))
            (incf pos 2)))
         ;; Read transformation
         (let ((a 1.0) (b 0.0) (c 0.0) (d 1.0)
               (e (if args-xy-values-p arg1 0))
               (f (if args-xy-values-p arg2 0)))
           (cond
             ((logbitp 3 flags) ; WE_HAVE_A_SCALE
              (let ((scale (zpb-otf:font-data-read-f2dot14 fd pos)))
                (incf pos 2)
                (setf a scale d scale)))
             ((logbitp 6 flags) ; WE_HAVE_AN_X_AND_Y_SCALE
              (setf a (zpb-otf:font-data-read-f2dot14 fd pos)
                    d (zpb-otf:font-data-read-f2dot14 fd (+ pos 2)))
              (incf pos 4))
             ((logbitp 7 flags) ; WE_HAVE_A_TWO_BY_TWO
              (setf a (zpb-otf:font-data-read-f2dot14 fd pos)
                    b (zpb-otf:font-data-read-f2dot14 fd (+ pos 2))
                    c (zpb-otf:font-data-read-f2dot14 fd (+ pos 4))
                    d (zpb-otf:font-data-read-f2dot14 fd (+ pos 6)))
              (incf pos 8)))
           (let ((transformed (transform-contours (make-transformer a b c d e f)
                                                  (map 'vector (lambda (c)
                                                                 (map 'vector (lambda (p)
                                                                                (make-control-point (x p) (y p) (on-curve-p p)))
                                                                      c))
                                                       contours))))
             (unless args-xy-values-p
               ;; Point matching: move the component so that its point ARG2
               ;; lands on point ARG1 of the components placed so far.
               (let ((parent-point (nth-contour-point (reverse contours-list) arg1))
                     (child-point (nth-contour-point (list transformed) arg2)))
                 (unless (and parent-point child-point)
                   (error 'unsupported-value
                          :description "Compound glyph anchor point out of range"
                          :location (format nil "glyph ~D" (first *compound-glyph-stack*))
                          :actual-value (list arg1 arg2)
                          :expected-values '("existing point indices")))
                 (let ((dx (- (x parent-point) (x child-point)))
                       (dy (- (y parent-point) (y child-point))))
                   (transform-contours (lambda (x y) (values (+ x dx) (+ y dy)))
                                       transformed))))
             (push transformed contours-list)))
         (unless more-components-p
           (return (merge-contours (nreverse contours-list)))))))))
