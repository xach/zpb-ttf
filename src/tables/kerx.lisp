;;;; kerx.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+kerx-subtable0-pair-size+ +kerx-tag+ kerx kerx-n-tables kerx-p kerx-padding
   kerx-subtable kerx-subtable-coverage kerx-subtable-data-bytes
   kerx-subtable-data-count kerx-subtable-data-ref kerx-subtable-length
   kerx-subtable-p kerx-subtable-tuple-count kerx-subtable0
   kerx-subtable0-entry-selector kerx-subtable0-n-pairs kerx-subtable0-p
   kerx-subtable0-pair kerx-subtable0-pair-left kerx-subtable0-pair-p
   kerx-subtable0-pair-right kerx-subtable0-pair-value
   kerx-subtable0-pairs-count kerx-subtable0-pairs-ref
   kerx-subtable0-range-shift kerx-subtable0-search-range kerx-subtables-start
   kerx-version read-kerx read-kerx-subtable read-kerx-subtable0 sanitize-kerx
   sanitize-kerx-subtable sanitize-kerx-subtable0))

(defstruct
    (kerx-subtable0-pair (:include record-ref)
     (:constructor %make-kerx-subtable0-pair
      (%record-font-data %record-offset)))
  "The type 0 `kerx` subtable kerning record.")

(define-constant +kerx-subtable0-pair-size+ 6)

(declaim
 (inline kerx-subtable0-pair-left kerx-subtable0-pair-right
  kerx-subtable0-pair-value))

(defun kerx-subtable0-pair-left (rec)
  "The glyph index for the lefthand glyph in the kerning pair."
  (declare (type kerx-subtable0-pair rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun kerx-subtable0-pair-right (rec)
  "The glyph index for the righthand glyph in the kerning pair."
  (declare (type kerx-subtable0-pair rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun kerx-subtable0-pair-value (rec)
  "Kerning value."
  (declare (type kerx-subtable0-pair rec))
  (font-data-read-i16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defstruct
    (kerx (:include table-ref)
     (:constructor %make-kerx (%table-font-data %table-offset %table-length)))
  "The [kerx (Extended Kerning)](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6morx.html) table.")

(declaim
 (inline kerx-version kerx-version-raw kerx-padding kerx-padding-raw
  kerx-n-tables kerx-n-tables-raw kerx-subtables-start))

(define-constant +kerx-tag+ "kerx")

(defun kerx-version (table)
  "The version number of the extended kerning table (currently 2, 3, or 4)"
  (declare (type kerx table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun kerx-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun kerx-padding (table)
  "Unused; set to zero."
  (declare (type kerx table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun kerx-padding-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun kerx-n-tables (table)
  "The number of subtables included in the extended kerning table."
  (declare (type kerx table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun kerx-n-tables-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun kerx-subtables-start (table)
  "Byte position of the subtables field; its elements are variable-length SUBTABLE data."
  (declare (type kerx table))
  (+ (table-ref-offset table) 8))

(defun sanitize-kerx (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-kerx
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-kerx font-data offset length)
  (%make-kerx font-data offset length))

(defstruct
    (kerx-subtable (:include table-ref)
     (:constructor %make-kerx-subtable
      (%table-font-data %table-offset %table-length)))
  "A subtable in a `kerx` table.")

(declaim
 (inline kerx-subtable-length kerx-subtable-length-raw kerx-subtable-coverage
  kerx-subtable-coverage-raw kerx-subtable-tuple-count
  kerx-subtable-tuple-count-raw kerx-subtable-data-count kerx-subtable-data-ref
  kerx-subtable-data-bytes))

(defun kerx-subtable-length (table)
  "The length of this subtable in bytes, including this header."
  (declare (type kerx-subtable table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun kerx-subtable-length-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun kerx-subtable-coverage (table)
  "Circumstances under which this table is used."
  (declare (type kerx-subtable table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun kerx-subtable-coverage-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun kerx-subtable-tuple-count (table)
  "The tuple count. This value is only used with variation fonts and should be 0 for all other fonts. The subtable's tupleCount will be ignored if the 'kerx' table version is less than 4."
  (declare (type kerx-subtable table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun kerx-subtable-tuple-count-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun kerx-subtable-data-count (table)
  "Subtable specific data."
  (declare (type kerx-subtable table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 12))
        1)))

(defun kerx-subtable-data-ref (table index)
  "Subtable specific data."
  (declare (type kerx-subtable table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun kerx-subtable-data-bytes (table)
  "Subtable specific data."
  (declare (type kerx-subtable table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 12)
                   (kerx-subtable-data-count table)))

(defun sanitize-kerx-subtable (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-kerx-subtable
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-kerx-subtable font-data offset length)
  (%make-kerx-subtable font-data offset length))

(defstruct
    (kerx-subtable0 (:include table-ref)
     (:constructor %make-kerx-subtable0
      (%table-font-data %table-offset %table-length)))
  "The type 0 `kerx` subtable.")

(declaim
 (inline kerx-subtable0-n-pairs kerx-subtable0-n-pairs-raw
  kerx-subtable0-search-range kerx-subtable0-search-range-raw
  kerx-subtable0-entry-selector kerx-subtable0-entry-selector-raw
  kerx-subtable0-range-shift kerx-subtable0-range-shift-raw
  kerx-subtable0-pairs-count kerx-subtable0-pairs-ref))

(defun kerx-subtable0-n-pairs (table)
  "The number of kerning pairs in this subtable."
  (declare (type kerx-subtable0 table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun kerx-subtable0-n-pairs-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun kerx-subtable0-search-range (table)
  "The largest power of two less than or equal to the value of nPairs, multiplied by the size in bytes of an entry in the subtable."
  (declare (type kerx-subtable0 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun kerx-subtable0-search-range-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun kerx-subtable0-entry-selector (table)
  "This is calculated as log2 of the largest power of two less than or equal to the value of nPairs. This value indicates how many iterations of the search loop have to be made. For example, in a list of eight items, there would be three iterations of the loop."
  (declare (type kerx-subtable0 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun kerx-subtable0-entry-selector-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun kerx-subtable0-range-shift (table)
  "The value of nPairs minus the largest power of two less than or equal to nPairs. This is multiplied by the size in bytes of an entry in the table."
  (declare (type kerx-subtable0 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun kerx-subtable0-range-shift-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 12)))

(defun kerx-subtable0-pairs-count (table)
  "Kerning records."
  (declare (type kerx-subtable0 table))
  (declare (ignorable table))
  (kerx-subtable0-n-pairs table))

(defun kerx-subtable0-pairs-ref (table index)
  "Kerning records."
  (declare (type kerx-subtable0 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 16) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-kerx-subtable0-pair (table-ref-font-data table) pos)))

(defun sanitize-kerx-subtable0 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  (let* ((cnt (kerx-subtable0-n-pairs-raw font-data offset))
         (pos (+ 16 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-kerx-subtable0
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-kerx-subtable0 font-data offset length)
  (%make-kerx-subtable0 font-data offset length))
