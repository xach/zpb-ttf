;;;; feat.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+feat-tag+ +feature-name-size+ +setting-name-size+ feat feat--reserved1
   feat--reserved2 feat-feature-name-count feat-names-count feat-names-ref
   feat-p feat-version feature-name feature-name-feature
   feature-name-feature-flags feature-name-n-settings feature-name-name-index
   feature-name-p feature-name-setting-table-offset read-feat
   read-setting-name-array sanitize-feat sanitize-setting-name-array
   setting-name setting-name-array setting-name-array-p
   setting-name-array-settings-count setting-name-array-settings-ref
   setting-name-name-index setting-name-p setting-name-setting))

(defstruct
    (feature-name (:include record-ref)
     (:constructor %make-feature-name (%record-font-data %record-offset)))
  "Type, flags and names for a feature.")

(define-constant +feature-name-size+ 12)

(declaim
 (inline feature-name-feature feature-name-n-settings
  feature-name-setting-table-offset feature-name-feature-flags
  feature-name-name-index))

(defun feature-name-feature (rec)
  "Feature type."
  (declare (type feature-name rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun feature-name-n-settings (rec)
  "The number of records in the setting name array."
  (declare (type feature-name rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun feature-name-setting-table-offset (rec)
  "Offset in bytes from the beginning of this table to this feature's setting name array. The actual type of record this offset refers to will depend on the exclusivity value, as described below."
  (declare (type feature-name rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun feature-name-feature-flags (rec)
  "Flags associated with the feature type."
  (declare (type feature-name rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defun feature-name-name-index (rec)
  "The name table index for the feature's name."
  (declare (type feature-name rec))
  (font-data-read-u16 (record-ref-font-data rec)
                      (+ (record-ref-offset rec) 10)))

(defstruct
    (setting-name (:include record-ref)
     (:constructor %make-setting-name (%record-font-data %record-offset)))
  "Associates a setting with a name identifier.")

(define-constant +setting-name-size+ 4)

(declaim (inline setting-name-setting setting-name-name-index))

(defun setting-name-setting (rec)
  "The setting."
  (declare (type setting-name rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun setting-name-name-index (rec)
  "The name table index for the setting's name."
  (declare (type setting-name rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (feat (:include table-ref)
     (:constructor %make-feat (%table-font-data %table-offset %table-length)))
  "The [feature name](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6feat.html) table.")

(declaim
 (inline feat-version feat-version-raw feat-feature-name-count
  feat-feature-name-count-raw feat--reserved1 feat--reserved1-raw
  feat--reserved2 feat--reserved2-raw feat-names-count feat-names-ref))

(define-constant +feat-tag+ "feat")

(defun feat-version (table)
  "Version number of the feature name table (0x00010000 for the current version)."
  (declare (type feat table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun feat-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun feat-feature-name-count (table)
  "The number of entries in the feature name array."
  (declare (type feat table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun feat-feature-name-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun feat--reserved1 (table)
  "Reserved (set to 0)."
  (declare (type feat table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun feat--reserved1-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun feat--reserved2 (table)
  "Reserved (set to 0)."
  (declare (type feat table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun feat--reserved2-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun feat-names-count (table)
  "The feature name array, sorted by feature type."
  (declare (type feat table))
  (declare (ignorable table))
  (feat-feature-name-count table))

(defun feat-names-ref (table index)
  "The feature name array, sorted by feature type."
  (declare (type feat table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 12))))
    (unless
        (<= (+ pos 12) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 12 :length
             (table-ref-length table)))
    (%make-feature-name (table-ref-font-data table) pos)))

(defun sanitize-feat (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  (let* ((cnt (feat-feature-name-count-raw font-data offset))
         (pos (+ 12 (* cnt 12))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-feat
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-feat font-data offset length)
  (%make-feat font-data offset length))

(defstruct
    (setting-name-array (:include table-ref)
     (:constructor %make-setting-name-array
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline setting-name-array-settings-count setting-name-array-settings-ref))

(defun setting-name-array-settings-count (table n-settings)
  "List of setting names for a feature."
  (declare (type setting-name-array table)
           (ignorable n-settings))
  (declare (ignorable table))
  n-settings)

(defun setting-name-array-settings-ref (table n-settings index)
  "List of setting names for a feature."
  (declare (type setting-name-array table)
           (ignorable n-settings index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-setting-name (table-ref-font-data table) pos)))

(defun sanitize-setting-name-array (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-setting-name-array
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-setting-name-array font-data offset length)
  (%make-setting-name-array font-data offset length))
