;;;; fvar.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+fvar-tag+ +variation-axis-record-size+ axis-instance-arrays
   axis-instance-arrays-axes-count axis-instance-arrays-axes-ref
   axis-instance-arrays-instances-start axis-instance-arrays-p fvar
   fvar--reserved fvar-axis-count fvar-axis-instance-arrays-offset
   fvar-axis-instance-arrays-offset-offset fvar-axis-size fvar-instance-count
   fvar-instance-size fvar-p fvar-version read-axis-instance-arrays read-fvar
   sanitize-axis-instance-arrays sanitize-fvar variation-axis-record
   variation-axis-record-axis-name-id variation-axis-record-axis-tag
   variation-axis-record-default-value variation-axis-record-flags
   variation-axis-record-max-value variation-axis-record-min-value
   variation-axis-record-p))

(defstruct
    (variation-axis-record (:include record-ref)
     (:constructor %make-variation-axis-record
      (%record-font-data %record-offset)))
  "The [VariationAxisRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar#variationaxisrecord)")

(define-constant +variation-axis-record-size+ 20)

(declaim
 (inline variation-axis-record-axis-tag variation-axis-record-min-value
  variation-axis-record-default-value variation-axis-record-max-value
  variation-axis-record-flags variation-axis-record-axis-name-id))

(defun variation-axis-record-axis-tag (rec)
  "Tag identifying the design variation for the axis."
  (declare (type variation-axis-record rec))
  (font-data-read-tag-string (record-ref-font-data rec)
                             (record-ref-offset rec)))

(defun variation-axis-record-min-value (rec)
  "The minimum coordinate value for the axis."
  (declare (type variation-axis-record rec))
  (font-data-read-fixed (record-ref-font-data rec)
                        (+ (record-ref-offset rec) 4)))

(defun variation-axis-record-default-value (rec)
  "The default coordinate value for the axis."
  (declare (type variation-axis-record rec))
  (font-data-read-fixed (record-ref-font-data rec)
                        (+ (record-ref-offset rec) 8)))

(defun variation-axis-record-max-value (rec)
  "The maximum coordinate value for the axis."
  (declare (type variation-axis-record rec))
  (font-data-read-fixed (record-ref-font-data rec)
                        (+ (record-ref-offset rec) 12)))

(defun variation-axis-record-flags (rec)
  "Axis qualifiers — see details below."
  (declare (type variation-axis-record rec))
  (font-data-read-u16 (record-ref-font-data rec)
                      (+ (record-ref-offset rec) 16)))

(defun variation-axis-record-axis-name-id (rec)
  "The name ID for entries in the 'name' table that provide a display name for this axis."
  (declare (type variation-axis-record rec))
  (font-data-read-u16 (record-ref-font-data rec)
                      (+ (record-ref-offset rec) 18)))

(defstruct
    (fvar (:include table-ref)
     (:constructor %make-fvar (%table-font-data %table-offset %table-length)))
  "The [fvar (Font Variations)](https://docs.microsoft.com/en-us/typography/opentype/spec/fvar) table")

(declaim
 (inline fvar-version fvar-version-raw fvar-axis-instance-arrays-offset-offset
  fvar-axis-instance-arrays-offset fvar--reserved fvar--reserved-raw
  fvar-axis-count fvar-axis-count-raw fvar-axis-size fvar-axis-size-raw
  fvar-instance-count fvar-instance-count-raw fvar-instance-size
  fvar-instance-size-raw))

(define-constant +fvar-tag+ "fvar")

(defun fvar-version (table)
  "Major version number of the font variations table — set to 1. Minor version number of the font variations table — set to 0."
  (declare (type fvar table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun fvar-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun fvar-axis-instance-arrays-offset-offset (table)
  "Offset in bytes from the beginning of the table to the start of the VariationAxisRecord array. The InstanceRecord array directly follows."
  (declare (type fvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun fvar-axis-instance-arrays-offset (table)
  "Offset in bytes from the beginning of the table to the start of the VariationAxisRecord array. The InstanceRecord array directly follows."
  (declare (type fvar table))
  (let ((off (fvar-axis-instance-arrays-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-axis-instance-arrays (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun fvar--reserved (table)
  "This field is permanently reserved. Set to 2."
  (declare (type fvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun fvar--reserved-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun fvar-axis-count (table)
  "The number of variation axes in the font (the number of records in the axes array)."
  (declare (type fvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun fvar-axis-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun fvar-axis-size (table)
  "The size in bytes of each VariationAxisRecord — set to 20 (0x0014) for this version."
  (declare (type fvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun fvar-axis-size-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun fvar-instance-count (table)
  "The number of named instances defined in the font (the number of records in the instances array)."
  (declare (type fvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun fvar-instance-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 12)))

(defun fvar-instance-size (table)
  "The size in bytes of each InstanceRecord — set to either axisCount * sizeof(Fixed) + 4, or to axisCount * sizeof(Fixed) + 6."
  (declare (type fvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 14)))

(defun fvar-instance-size-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 14)))

(defun sanitize-fvar (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  t)

(defun read-fvar
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-fvar font-data offset length)
  (%make-fvar font-data offset length))

(defstruct
    (axis-instance-arrays (:include table-ref)
     (:constructor %make-axis-instance-arrays
      (%table-font-data %table-offset %table-length)))
  "Shim table to handle combined axis and instance arrays.")

(declaim
 (inline axis-instance-arrays-axes-count axis-instance-arrays-axes-ref
  axis-instance-arrays-instances-start))

(defun axis-instance-arrays-axes-count (table axis-count)
  "Variation axis record array."
  (declare (type axis-instance-arrays table)
           (ignorable axis-count))
  (declare (ignorable table))
  axis-count)

(defun axis-instance-arrays-axes-ref (table axis-count index)
  "Variation axis record array."
  (declare (type axis-instance-arrays table)
           (ignorable axis-count index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 20))))
    (unless
        (<= (+ pos 20) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 20 :length
             (table-ref-length table)))
    (%make-variation-axis-record (table-ref-font-data table) pos)))

(defun axis-instance-arrays-instances-start (table axis-count)
  "Byte position of the instances field; its elements are variable-length INSTANCE-RECORD data."
  (declare (type axis-instance-arrays table)
           (ignorable axis-count))
  (+ (table-ref-offset table)
     (* (axis-instance-arrays-axes-count table axis-count) 20)))

(defun sanitize-axis-instance-arrays (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-axis-instance-arrays
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-axis-instance-arrays font-data offset length)
  (%make-axis-instance-arrays font-data offset length))
