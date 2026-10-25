;;;; stat.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+axis-record-size+ +axis-value-record-size+
   +axis-value-table-flags-elidable-axis-value-name+
   +axis-value-table-flags-older-sibling-font-attribute+ +stat-tag+ axis-record
   axis-record-axis-name-id axis-record-axis-ordering axis-record-axis-tag
   axis-record-p axis-value-array axis-value-array-axis-value-offsets-count
   axis-value-array-axis-value-offsets-ref axis-value-array-p
   axis-value-format1 axis-value-format1-axis-index axis-value-format1-flags
   axis-value-format1-format-table axis-value-format1-p
   axis-value-format1-value axis-value-format1-value-name-id axis-value-format2
   axis-value-format2-axis-index axis-value-format2-flags
   axis-value-format2-format-table axis-value-format2-nominal-value
   axis-value-format2-p axis-value-format2-range-max-value
   axis-value-format2-range-min-value axis-value-format2-value-name-id
   axis-value-format3 axis-value-format3-axis-index axis-value-format3-flags
   axis-value-format3-format-table axis-value-format3-linked-value
   axis-value-format3-p axis-value-format3-value
   axis-value-format3-value-name-id axis-value-format4
   axis-value-format4-axis-count axis-value-format4-axis-values-count
   axis-value-format4-axis-values-ref axis-value-format4-flags
   axis-value-format4-format-table axis-value-format4-p
   axis-value-format4-value-name-id axis-value-record
   axis-value-record-axis-index axis-value-record-p axis-value-record-value
   axis-value-table-flags-elidable-axis-value-name-p
   axis-value-table-flags-older-sibling-font-attribute-p read-axis-value
   read-axis-value-array read-axis-value-format1 read-axis-value-format2
   read-axis-value-format3 read-axis-value-format4 read-stat
   sanitize-axis-value-array sanitize-axis-value-format1
   sanitize-axis-value-format2 sanitize-axis-value-format3
   sanitize-axis-value-format4 sanitize-stat stat stat-axis-value-count
   stat-design-axes-offset stat-design-axes-offset-offset
   stat-design-axis-count stat-design-axis-size stat-elided-fallback-name-id
   stat-offset-to-axis-value-offsets stat-offset-to-axis-value-offsets-offset
   stat-p stat-version))

(define-constant +axis-value-table-flags-older-sibling-font-attribute+ 1
                 "[Axis value table flags](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#flags).")

(declaim (inline axis-value-table-flags-older-sibling-font-attribute-p))

(defun axis-value-table-flags-older-sibling-font-attribute-p (flags)
  (logtest flags +axis-value-table-flags-older-sibling-font-attribute+))

(define-constant +axis-value-table-flags-elidable-axis-value-name+ 2
                 "[Axis value table flags](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#flags).")

(declaim (inline axis-value-table-flags-elidable-axis-value-name-p))

(defun axis-value-table-flags-elidable-axis-value-name-p (flags)
  (logtest flags +axis-value-table-flags-elidable-axis-value-name+))

(defstruct
    (axis-record (:include record-ref)
     (:constructor %make-axis-record (%record-font-data %record-offset)))
  "[Axis Records](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-records)")

(define-constant +axis-record-size+ 8)

(declaim
 (inline axis-record-axis-tag axis-record-axis-name-id
  axis-record-axis-ordering))

(defun axis-record-axis-tag (rec)
  "A tag identifying the axis of design variation."
  (declare (type axis-record rec))
  (font-data-read-tag-string (record-ref-font-data rec)
                             (record-ref-offset rec)))

(defun axis-record-axis-name-id (rec)
  "The name ID for entries in the 'name' table that provide a display string for this axis."
  (declare (type axis-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun axis-record-axis-ordering (rec)
  "A value that applications can use to determine primary sorting of face names, or for ordering of labels when composing family or face names."
  (declare (type axis-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 6)))

(defstruct
    (axis-value-record (:include record-ref)
     (:constructor %make-axis-value-record (%record-font-data %record-offset)))
  "Part of [AxisValueFormat4]")

(define-constant +axis-value-record-size+ 6)

(declaim (inline axis-value-record-axis-index axis-value-record-value))

(defun axis-value-record-axis-index (rec)
  "Zero-base index into the axis record array identifying the axis to which this value applies. Must be less than designAxisCount."
  (declare (type axis-value-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun axis-value-record-value (rec)
  "A numeric value for this attribute value."
  (declare (type axis-value-record rec))
  (font-data-read-fixed (record-ref-font-data rec)
                        (+ (record-ref-offset rec) 2)))

(defstruct
    (stat (:include table-ref)
     (:constructor %make-stat (%table-font-data %table-offset %table-length)))
  "[STAT](https://docs.microsoft.com/en-us/typography/opentype/spec/stat) (Style Attributes Table)")

(declaim
 (inline stat-version stat-version-raw stat-design-axis-size
  stat-design-axis-size-raw stat-design-axis-count stat-design-axis-count-raw
  stat-design-axes-offset-offset stat-design-axes-offset stat-axis-value-count
  stat-axis-value-count-raw stat-offset-to-axis-value-offsets-offset
  stat-offset-to-axis-value-offsets stat-elided-fallback-name-id
  stat-elided-fallback-name-id-raw))

(define-constant +stat-tag+ "STAT")

(defun stat-version (table)
  "Major/minor version number. Set to 1.2 for new fonts."
  (declare (type stat table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun stat-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun stat-design-axis-size (table)
  "The size in bytes of each axis record."
  (declare (type stat table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun stat-design-axis-size-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun stat-design-axis-count (table)
  "The number of axis records. In a font with an 'fvar' table, this value must be greater than or equal to the axisCount value in the 'fvar' table. In all fonts, must be greater than zero if axisValueCount is greater than zero."
  (declare (type stat table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun stat-design-axis-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun stat-design-axes-offset-offset (table)
  "Offset in bytes from the beginning of the STAT table to the start of the design axes array. If designAxisCount is zero, set to zero; if designAxisCount is greater than zero, must be greater than zero."
  (declare (type stat table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun stat-design-axes-offset (table)
  "Offset in bytes from the beginning of the STAT table to the start of the design axes array. If designAxisCount is zero, set to zero; if designAxisCount is greater than zero, must be greater than zero."
  (declare (type stat table))
  (let ((off (stat-design-axes-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun stat-axis-value-count (table)
  "The number of axis value tables."
  (declare (type stat table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun stat-axis-value-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 12)))

(defun stat-offset-to-axis-value-offsets-offset (table)
  "Offset in bytes from the beginning of the STAT table to the start of the design axes value offsets array. If axisValueCount is zero, set to zero; if axisValueCount is greater than zero, must be greater than zero."
  (declare (type stat table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 14)))

(defun stat-offset-to-axis-value-offsets (table)
  "Offset in bytes from the beginning of the STAT table to the start of the design axes value offsets array. If axisValueCount is zero, set to zero; if axisValueCount is greater than zero, must be greater than zero."
  (declare (type stat table))
  (let ((off (stat-offset-to-axis-value-offsets-offset table)))
    (when (and off (not (zerop off)))
      (read-axis-value-array (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun stat-elided-fallback-name-id (table)
  "Name ID used as fallback when projection of names into a particular font model produces a subfamily name containing only elidable elements."
  (declare (type stat table))
  (when
      (and (>= (stat-version table) 65537)
           (<= (+ (+ (table-ref-offset table) 18) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 18))))

(defun stat-elided-fallback-name-id-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 18)))

(defun sanitize-stat (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 18 length) (font-data-valid-range-p font-data offset 18))
    (error 'font-out-of-bounds-error :pos offset :size 18 :length
           (font-data-length font-data)))
  t)

(defun read-stat
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-stat font-data offset length)
  (%make-stat font-data offset length))

(defstruct
    (axis-value-array (:include table-ref)
     (:constructor %make-axis-value-array
      (%table-font-data %table-offset %table-length)))
  "An array of [AxisValue] tables.")

(declaim
 (inline axis-value-array-axis-value-offsets-count
  axis-value-array-axis-value-offsets-ref))

(defun axis-value-array-axis-value-offsets-count (table axis-value-count)
  "Array of offsets to axis value tables, in bytes from the start of the axis value offsets array."
  (declare (type axis-value-array table)
           (ignorable axis-value-count))
  (declare (ignorable table))
  axis-value-count)

(defun axis-value-array-axis-value-offsets-ref (table axis-value-count index)
  "Array of offsets to axis value tables, in bytes from the start of the axis value offsets array."
  (declare (type axis-value-array table)
           (ignorable axis-value-count index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u16 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-axis-value (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-axis-value-array (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-axis-value-array
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-axis-value-array font-data offset length)
  (%make-axis-value-array font-data offset length))

(defun read-axis-value
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "[Axis Value Tables](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-tables)"
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-axis-value-format1 font-data offset length))
      (2 (read-axis-value-format2 font-data offset length))
      (3 (read-axis-value-format3 font-data offset length))
      (4 (read-axis-value-format4 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'axis-value)))))

(defstruct
    (axis-value-format1 (:include table-ref)
     (:constructor %make-axis-value-format1
      (%table-font-data %table-offset %table-length)))
  "[Axis value table format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-table-format-1)")

(declaim
 (inline axis-value-format1-format-table axis-value-format1-format-table-raw
  axis-value-format1-axis-index axis-value-format1-axis-index-raw
  axis-value-format1-flags axis-value-format1-flags-raw
  axis-value-format1-value-name-id axis-value-format1-value-name-id-raw
  axis-value-format1-value axis-value-format1-value-raw))

(defun axis-value-format1-format-table (table)
  "Format identifier — set to 1."
  (declare (type axis-value-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun axis-value-format1-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun axis-value-format1-axis-index (table)
  "Zero-base index into the axis record array identifying the axis of design variation to which the axis value table applies. Must be less than designAxisCount."
  (declare (type axis-value-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun axis-value-format1-axis-index-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun axis-value-format1-flags (table)
  "Flags — see below for details."
  (declare (type axis-value-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun axis-value-format1-flags-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun axis-value-format1-value-name-id (table)
  "The name ID for entries in the 'name' table that provide a display string for this attribute value."
  (declare (type axis-value-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun axis-value-format1-value-name-id-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun axis-value-format1-value (table)
  "A numeric value for this attribute value."
  (declare (type axis-value-format1 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun axis-value-format1-value-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 8)))

(defun sanitize-axis-value-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-axis-value-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-axis-value-format1 font-data offset length)
  (%make-axis-value-format1 font-data offset length))

(defstruct
    (axis-value-format2 (:include table-ref)
     (:constructor %make-axis-value-format2
      (%table-font-data %table-offset %table-length)))
  "[Axis value table format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-table-format-2)")

(declaim
 (inline axis-value-format2-format-table axis-value-format2-format-table-raw
  axis-value-format2-axis-index axis-value-format2-axis-index-raw
  axis-value-format2-flags axis-value-format2-flags-raw
  axis-value-format2-value-name-id axis-value-format2-value-name-id-raw
  axis-value-format2-nominal-value axis-value-format2-nominal-value-raw
  axis-value-format2-range-min-value axis-value-format2-range-min-value-raw
  axis-value-format2-range-max-value axis-value-format2-range-max-value-raw))

(defun axis-value-format2-format-table (table)
  "Format identifier — set to 2."
  (declare (type axis-value-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun axis-value-format2-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun axis-value-format2-axis-index (table)
  "Zero-base index into the axis record array identifying the axis of design variation to which the axis value table applies. Must be less than designAxisCount."
  (declare (type axis-value-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun axis-value-format2-axis-index-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun axis-value-format2-flags (table)
  "Flags — see below for details."
  (declare (type axis-value-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun axis-value-format2-flags-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun axis-value-format2-value-name-id (table)
  "The name ID for entries in the 'name' table that provide a display string for this attribute value."
  (declare (type axis-value-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun axis-value-format2-value-name-id-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun axis-value-format2-nominal-value (table)
  "A nominal numeric value for this attribute value."
  (declare (type axis-value-format2 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun axis-value-format2-nominal-value-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 8)))

(defun axis-value-format2-range-min-value (table)
  "The minimum value for a range associated with the specified name ID."
  (declare (type axis-value-format2 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun axis-value-format2-range-min-value-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 12)))

(defun axis-value-format2-range-max-value (table)
  "The maximum value for a range associated with the specified name ID."
  (declare (type axis-value-format2 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 16)))

(defun axis-value-format2-range-max-value-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 16)))

(defun sanitize-axis-value-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 20 length) (font-data-valid-range-p font-data offset 20))
    (error 'font-out-of-bounds-error :pos offset :size 20 :length
           (font-data-length font-data)))
  t)

(defun read-axis-value-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-axis-value-format2 font-data offset length)
  (%make-axis-value-format2 font-data offset length))

(defstruct
    (axis-value-format3 (:include table-ref)
     (:constructor %make-axis-value-format3
      (%table-font-data %table-offset %table-length)))
  "[Axis value table format 3](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-table-format-3)")

(declaim
 (inline axis-value-format3-format-table axis-value-format3-format-table-raw
  axis-value-format3-axis-index axis-value-format3-axis-index-raw
  axis-value-format3-flags axis-value-format3-flags-raw
  axis-value-format3-value-name-id axis-value-format3-value-name-id-raw
  axis-value-format3-value axis-value-format3-value-raw
  axis-value-format3-linked-value axis-value-format3-linked-value-raw))

(defun axis-value-format3-format-table (table)
  "Format identifier — set to 3."
  (declare (type axis-value-format3 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun axis-value-format3-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun axis-value-format3-axis-index (table)
  "Zero-base index into the axis record array identifying the axis of design variation to which the axis value table applies. Must be less than designAxisCount."
  (declare (type axis-value-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun axis-value-format3-axis-index-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun axis-value-format3-flags (table)
  "Flags — see below for details."
  (declare (type axis-value-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun axis-value-format3-flags-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun axis-value-format3-value-name-id (table)
  "The name ID for entries in the 'name' table that provide a display string for this attribute value."
  (declare (type axis-value-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun axis-value-format3-value-name-id-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun axis-value-format3-value (table)
  "A numeric value for this attribute value."
  (declare (type axis-value-format3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun axis-value-format3-value-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 8)))

(defun axis-value-format3-linked-value (table)
  "The numeric value for a style-linked mapping from this value."
  (declare (type axis-value-format3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun axis-value-format3-linked-value-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 12)))

(defun sanitize-axis-value-format3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  t)

(defun read-axis-value-format3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-axis-value-format3 font-data offset length)
  (%make-axis-value-format3 font-data offset length))

(defstruct
    (axis-value-format4 (:include table-ref)
     (:constructor %make-axis-value-format4
      (%table-font-data %table-offset %table-length)))
  "[Axis value table format 4](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-table-format-4)")

(declaim
 (inline axis-value-format4-format-table axis-value-format4-format-table-raw
  axis-value-format4-axis-count axis-value-format4-axis-count-raw
  axis-value-format4-flags axis-value-format4-flags-raw
  axis-value-format4-value-name-id axis-value-format4-value-name-id-raw
  axis-value-format4-axis-values-count axis-value-format4-axis-values-ref))

(defun axis-value-format4-format-table (table)
  "Format identifier — set to 4."
  (declare (type axis-value-format4 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun axis-value-format4-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun axis-value-format4-axis-count (table)
  "The total number of axes contributing to this axis-values combination."
  (declare (type axis-value-format4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun axis-value-format4-axis-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun axis-value-format4-flags (table)
  "Flags — see below for details."
  (declare (type axis-value-format4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun axis-value-format4-flags-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun axis-value-format4-value-name-id (table)
  "The name ID for entries in the 'name' table that provide a display string for this combination of axis values."
  (declare (type axis-value-format4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun axis-value-format4-value-name-id-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun axis-value-format4-axis-values-count (table)
  "Array of AxisValue records that provide the combination of axis values, one for each contributing axis."
  (declare (type axis-value-format4 table))
  (declare (ignorable table))
  (axis-value-format4-axis-count table))

(defun axis-value-format4-axis-values-ref (table index)
  "Array of AxisValue records that provide the combination of axis values, one for each contributing axis."
  (declare (type axis-value-format4 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-axis-value-record (table-ref-font-data table) pos)))

(defun sanitize-axis-value-format4 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  (let* ((cnt (axis-value-format4-axis-count-raw font-data offset))
         (pos (+ 8 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-axis-value-format4
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-axis-value-format4 font-data offset length)
  (%make-axis-value-format4 font-data offset length))
