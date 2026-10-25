;;;; font-loader.lisp — Font loader object and metadata accessors
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(in-package #:zpb-ttf)

(defclass font-loader ()
  ((tables :initform (make-hash-table :test 'equal) :reader tables)
   (font-data :initarg :font-data :accessor font-data)
   (input-stream :initarg :input-stream :initform nil :accessor input-stream)
   (table-count :initarg :table-count :initform 0 :reader table-count)
   ;; from 'head' table
   (units/em :accessor units/em :initform 2048)
   (bounding-box :accessor bounding-box :initform (vector 0 0 0 0))
   (loca-offset-format :accessor loca-offset-format :initform :short)
   ;; from 'loca' table
   (glyph-locations :accessor glyph-locations :initform nil)
   ;; from 'cmap' table
   ;; Glyph index -> lowest code point mapping to it; built on first use.
   (inverse-character-map :accessor inverse-character-map :initform nil)
   (cmap-lookup-fn :accessor cmap-lookup-fn :initform nil)
   ;; Function of one argument FN, calling (FN code-point glyph-index) for each mapping.
   (cmap-mappings-fn :accessor cmap-mappings-fn :initform nil)
   ;; from 'maxp' table
   (glyph-count :accessor glyph-count :initform 0)
   ;; from 'hhea' table
   (ascender :accessor ascender :initform 0)
   (descender :accessor descender :initform 0)
   (line-gap :accessor line-gap :initform 0)
   (max-width :accessor max-width :initform 0)
   ;; from 'hmtx' table
   (advance-widths :accessor advance-widths :initform (vector))
   (left-side-bearings :accessor left-side-bearings :initform (vector))
   ;; from 'vhea' table
   (vhea-missing-p :initform nil :accessor vhea-missing-p)
   (vascender :accessor vascender :initform 0)
   (vdescender :accessor vdescender :initform 0)
   ;; from 'vhea' and 'vmtx' tables
   (vmtx-missing-p :initform nil :accessor vmtx-missing-p)
   (advance-heights :accessor advance-heights :initform (vector))
   (top-side-bearings :accessor top-side-bearings :initform (vector))
   ;; from 'kern' table
   (kerning-table :initform (make-hash-table) :accessor kerning-table)
   ;; from 'name' table
   (name-entries :initform (vector) :accessor name-entries)
   ;; from 'post' table
   (italic-angle :accessor italic-angle :initform 0)
   (fixed-pitch-p :accessor fixed-pitch-p :initform nil)
   (underline-position :accessor underline-position :initform 0)
   (underline-thickness :accessor underline-thickness :initform 0)
   (postscript-glyph-names :accessor postscript-glyph-names :initform nil)
   ;; glyph cache
   (glyph-cache :accessor glyph-cache :initform nil)
   ;; TTC info
   (collection-font-count :reader collection-font-count :initform nil :initarg :collection-font-count)
   (collection-font-index :reader collection-font-index :initform nil :initarg :collection-font-index)))

(defclass table-info ()
  ((name :initarg :name :reader name)
   (offset :initarg :offset :reader offset)
   (size :initarg :size :reader size)))

(defmethod print-object ((object table-info) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (name object))))

(defmethod table-info ((tag string) (font-loader font-loader))
  (gethash tag (tables font-loader)))

(defmethod table-info ((tag integer) (font-loader font-loader))
  (gethash (tag->number-string tag) (tables font-loader)))

(defun tag->number-string (num)
  (let ((s (make-string 4)))
    (setf (char s 0) (code-char (ldb (byte 8 24) num))
          (char s 1) (code-char (ldb (byte 8 16) num))
          (char s 2) (code-char (ldb (byte 8 8) num))
          (char s 3) (code-char (ldb (byte 8 0) num)))
    s))

(defmethod table-exists-p (tag font-loader)
  (nth-value 1 (table-info tag font-loader)))

(defmethod table-position ((tag string) (font-loader font-loader))
  (let ((info (table-info tag font-loader)))
    (if info
        (offset info)
        (error "No such table -- ~A" tag))))

(defmethod table-size ((tag string) (font-loader font-loader))
  (let ((info (table-info tag font-loader)))
    (if info
        (size info)
        (error "No such table -- ~A" tag))))

;;; Name table entries

(defclass name-entry ()
  ((font-loader :initarg :font-loader :reader font-loader)
   (platform-id :initarg :platform-id :reader platform-id)
   (encoding-id :initarg :encoding-id :reader encoding-id)
   (language-id :initarg :language-id :reader language-id)
   (name-id :initarg :name-id :reader name-id)
   (value :initarg :value :reader value)))

(defmethod print-object ((entry name-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "name-id=~D: ~S" (name-id entry) (value entry))))

(defvar *name-identifiers*
  #(:copyright-notice
    :font-family
    :font-subfamily
    :unique-subfamily
    :full-name
    :name-table-version
    :postscript-name
    :trademark-notice
    :manufacturer-name
    :designer
    :description
    :vendor-url
    :designer-url
    :license-description
    :licence-info-url
    :reserved
    :preferred-family
    :preferred-subfamily
    :compatible-full
    :sample-text))

(defun name-identifier-id (symbol)
  (let ((id (position symbol *name-identifiers*)))
    (if id id (error "Unknown NAME identifier: ~S" symbol))))

(defmethod find-name-entry (platform-id encoding-id language-id name-id
                            (font-loader font-loader))
  (loop for name-entry across (name-entries font-loader)
        when (and (or (null platform-id) (= (platform-id name-entry) platform-id))
                  (or (null encoding-id) (= (encoding-id name-entry) encoding-id))
                  (or (null language-id) (= (language-id name-entry) language-id))
                  (or (null name-id) (= (name-id name-entry) name-id)))
        return name-entry))

(defun name-entry-rank (entry)
  "Preference rank of ENTRY, lower is better."
  (let ((plat (platform-id entry))
        (enc (encoding-id entry))
        (lang (language-id entry)))
    (cond ((and (= plat 3) (= enc 1) (= lang #x0409)) 0) ; Windows US-English
          ((and (= plat 3) (= enc 10) (= lang #x0409)) 1)
          ((= plat 3) 2)
          ((= plat 0) 3)                                 ; Unicode
          ((and (= plat 1) (= enc 0) (= lang 0)) 4)      ; Mac Roman
          (t 5))))

(defmethod name-entry-value (name-designator (font-loader font-loader))
  (let ((name-id (etypecase name-designator
                   (keyword (name-identifier-id name-designator))
                   (integer name-designator)))
        (best nil)
        (best-rank nil))
    (loop for entry across (name-entries font-loader)
          when (= (name-id entry) name-id)
            do (let ((rank (name-entry-rank entry)))
                 (when (or (null best-rank) (< rank best-rank))
                   (setf best entry best-rank rank))))
    (when best
      (value best))))

(defmethod postscript-name ((font-loader font-loader))
  (name-entry-value :postscript-name font-loader))

(defmethod family-name ((font-loader font-loader))
  (name-entry-value :font-family font-loader))

(defmethod subfamily-name ((font-loader font-loader))
  (name-entry-value :font-subfamily font-loader))

(defmethod full-name ((font-loader font-loader))
  (name-entry-value :full-name font-loader))

;;; Character mapping & glyph indexing

(defgeneric code-point-font-index (code-point font-loader)
  (:method (code-point (font-loader font-loader))
    (let ((fn (cmap-lookup-fn font-loader)))
      (if fn
          (funcall fn code-point)
          0))))

(defun build-inverse-character-map (font-loader)
  (let ((inv (make-array (glyph-count font-loader) :initial-element 0))
        (fn (cmap-mappings-fn font-loader)))
    (when fn
      (funcall fn (lambda (cp gid)
                    (when (and (< 0 gid (length inv))
                               (or (zerop (aref inv gid)) (< cp (aref inv gid))))
                      (setf (aref inv gid) cp)))))
    inv))

(defgeneric font-index-code-point (glyph-index font-loader)
  (:method (glyph-index (font-loader font-loader))
    (let ((inv (or (inverse-character-map font-loader)
                   (setf (inverse-character-map font-loader)
                         (build-inverse-character-map font-loader)))))
      (if (and inv (< glyph-index (length inv)))
          (let ((cp (aref inv glyph-index)))
            (if (plusp cp) cp 0))
          0))))

(defmethod glyph-exists-p (char-or-code (font-loader font-loader))
  (let ((cp (if (characterp char-or-code) (char-code char-or-code) char-or-code)))
    (not (zerop (code-point-font-index cp font-loader)))))

(defmethod index-glyph (index (font-loader font-loader))
  (let ((cache (glyph-cache font-loader)))
    (unless (and (integerp index) (< -1 index (glyph-count font-loader)) cache)
      (error 'unsupported-value
             :description "Glyph index out of range"
             :actual-value index
             :expected-values (list (format nil "0..~D" (1- (glyph-count font-loader))))))
    (or (aref cache index)
        (setf (aref cache index)
              (make-instance 'glyph
                             :font-loader font-loader
                             :font-index index)))))

(defmethod find-glyph (char-or-code (font-loader font-loader))
  (let* ((cp (if (characterp char-or-code) (char-code char-or-code) char-or-code))
         (index (code-point-font-index cp font-loader)))
    (index-glyph index font-loader)))

(defmethod all-kerning-pairs ((font-loader font-loader))
  (let ((pairs '()))
    (maphash (lambda (k v)
               (let ((g1-idx (ldb (byte 16 16) k))
                     (g2-idx (ldb (byte 16 0) k)))
                 (push (list (index-glyph g1-idx font-loader)
                             (index-glyph g2-idx font-loader)
                             v)
                       pairs)))
             (kerning-table font-loader))
    (nreverse pairs)))