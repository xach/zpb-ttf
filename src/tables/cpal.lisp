;;;; cpal.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+color-record-size+ +cpal-tag+ +palette-type-usable-with-dark-background+
   +palette-type-usable-with-light-background+ color-record color-record-alpha
   color-record-blue color-record-green color-record-p color-record-red cpal
   cpal-color-record-indices-count cpal-color-record-indices-ref
   cpal-color-records-array-offset cpal-color-records-array-offset-offset
   cpal-num-color-records cpal-num-palette-entries cpal-num-palettes cpal-p
   cpal-palette-entry-labels-array-offset
   cpal-palette-entry-labels-array-offset-offset
   cpal-palette-labels-array-offset cpal-palette-labels-array-offset-offset
   cpal-palette-types-array-offset cpal-palette-types-array-offset-offset
   cpal-version palette-type-usable-with-dark-background-p
   palette-type-usable-with-light-background-p read-cpal sanitize-cpal))

(define-constant +palette-type-usable-with-light-background+ 1
                 "The [PaletteType](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-type-array) flags.")

(declaim (inline palette-type-usable-with-light-background-p))

(defun palette-type-usable-with-light-background-p (flags)
  (logtest flags +palette-type-usable-with-light-background+))

(define-constant +palette-type-usable-with-dark-background+ 2
                 "The [PaletteType](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-type-array) flags.")

(declaim (inline palette-type-usable-with-dark-background-p))

(defun palette-type-usable-with-dark-background-p (flags)
  (logtest flags +palette-type-usable-with-dark-background+))

(defstruct
    (color-record (:include record-ref)
     (:constructor %make-color-record (%record-font-data %record-offset)))
  "[CPAL (Color Record)](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-entries-and-color-records) record  Contains a color in non-premultiplied BGRA form, in the sRGB color space.")

(define-constant +color-record-size+ 4)

(declaim
 (inline color-record-blue color-record-green color-record-red
  color-record-alpha))

(defun color-record-blue (rec)
  "Blue value (B0)."
  (declare (type color-record rec))
  (font-data-read-u8 (record-ref-font-data rec) (record-ref-offset rec)))

(defun color-record-green (rec)
  "Green value (B1)."
  (declare (type color-record rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 1)))

(defun color-record-red (rec)
  "Red value (B2)."
  (declare (type color-record rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun color-record-alpha (rec)
  "Alpha value (B3)."
  (declare (type color-record rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 3)))

(defstruct
    (cpal (:include table-ref)
     (:constructor %make-cpal (%table-font-data %table-offset %table-length)))
  "[CPAL (Color Palette Table)](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-table-header) table")

(declaim
 (inline cpal-version cpal-version-raw cpal-num-palette-entries
  cpal-num-palette-entries-raw cpal-num-palettes cpal-num-palettes-raw
  cpal-num-color-records cpal-num-color-records-raw
  cpal-color-records-array-offset-offset cpal-color-records-array-offset
  cpal-color-record-indices-count cpal-color-record-indices-ref
  cpal-palette-types-array-offset-offset cpal-palette-types-array-offset
  cpal-palette-labels-array-offset-offset cpal-palette-labels-array-offset
  cpal-palette-entry-labels-array-offset-offset
  cpal-palette-entry-labels-array-offset))

(define-constant +cpal-tag+ "CPAL")

(defun cpal-version (table)
  "Table version number (=0)."
  (declare (type cpal table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun cpal-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun cpal-num-palette-entries (table)
  "Number of palette entries in each palette."
  (declare (type cpal table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun cpal-num-palette-entries-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun cpal-num-palettes (table)
  "Number of palettes in the table."
  (declare (type cpal table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun cpal-num-palettes-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun cpal-num-color-records (table)
  "Total number of color records, combined for all palettes."
  (declare (type cpal table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun cpal-num-color-records-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun cpal-color-records-array-offset-offset (table)
  "Offset from the beginning of CPAL table to the first ColorRecord."
  (declare (type cpal table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun cpal-color-records-array-offset (table)
  "Offset from the beginning of CPAL table to the first ColorRecord."
  (declare (type cpal table))
  (let ((off (cpal-color-records-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun cpal-color-record-indices-count (table)
  "Index of each palette’s first color record in the combined color record array."
  (declare (type cpal table))
  (declare (ignorable table))
  (cpal-num-palettes table))

(defun cpal-color-record-indices-ref (table index)
  "Index of each palette’s first color record in the combined color record array."
  (declare (type cpal table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun cpal-palette-types-array-offset-offset (table)
  "Offset from the beginning of CPAL table to the [Palette Types Array][].  This is an array of 32-bit flag fields that describe properties of each palette.  [Palette Types Array]: https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-type-array"
  (declare (type cpal table))
  (when
      (and (>= (cpal-version table) 1)
           (<=
            (+
             (+ (table-ref-offset table) 12
                (* (cpal-color-record-indices-count table) 2))
             4)
            (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 12
                           (* (cpal-color-record-indices-count table) 2)))))

(defun cpal-palette-types-array-offset (table)
  "Offset from the beginning of CPAL table to the [Palette Types Array][].  This is an array of 32-bit flag fields that describe properties of each palette.  [Palette Types Array]: https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-type-array"
  (declare (type cpal table))
  (let ((off (cpal-palette-types-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun cpal-palette-labels-array-offset-offset (table)
  "Offset from the beginning of CPAL table to the [Palette Labels Array][].  This is an array of 'name' table IDs (typically in the font-specific name ID range) that specify user interface strings associated with  each palette. Use 0xFFFF if no name ID is provided for a palette.  [Palette Labels Array]: https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-labels-array"
  (declare (type cpal table))
  (when
      (and (>= (cpal-version table) 1)
           (<=
            (+
             (+ (table-ref-offset table) 16
                (* (cpal-color-record-indices-count table) 2))
             4)
            (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 16
                           (* (cpal-color-record-indices-count table) 2)))))

(defun cpal-palette-labels-array-offset (table)
  "Offset from the beginning of CPAL table to the [Palette Labels Array][].  This is an array of 'name' table IDs (typically in the font-specific name ID range) that specify user interface strings associated with  each palette. Use 0xFFFF if no name ID is provided for a palette.  [Palette Labels Array]: https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-labels-array"
  (declare (type cpal table))
  (let ((off (cpal-palette-labels-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun cpal-palette-entry-labels-array-offset-offset (table)
  "Offset from the beginning of CPAL table to the [Palette Entry Labels Array][].  This is an array of 'name' table IDs (typically in the font-specific name ID range) that specify user interface strings associated with  each palette entry, e.g. “Outline”, “Fill”. This set of palette entry labels applies to all palettes in the font. Use  0xFFFF if no name ID is provided for a palette entry.  [Palette Entry Labels Array]: https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-entry-label-array"
  (declare (type cpal table))
  (when
      (and (>= (cpal-version table) 1)
           (<=
            (+
             (+ (table-ref-offset table) 20
                (* (cpal-color-record-indices-count table) 2))
             4)
            (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 20
                           (* (cpal-color-record-indices-count table) 2)))))

(defun cpal-palette-entry-labels-array-offset (table)
  "Offset from the beginning of CPAL table to the [Palette Entry Labels Array][].  This is an array of 'name' table IDs (typically in the font-specific name ID range) that specify user interface strings associated with  each palette entry, e.g. “Outline”, “Fill”. This set of palette entry labels applies to all palettes in the font. Use  0xFFFF if no name ID is provided for a palette entry.  [Palette Entry Labels Array]: https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-entry-label-array"
  (declare (type cpal table))
  (let ((off (cpal-palette-entry-labels-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun sanitize-cpal (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  (let* ((cnt (cpal-num-palettes-raw font-data offset)) (pos (+ 12 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-cpal
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cpal font-data offset length)
  (%make-cpal font-data offset length))
