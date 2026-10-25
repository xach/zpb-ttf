;;;; gvar.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+gvar-extended-tag+ +gvar-flags-long-offsets+ +gvar-tag+
   glyph-variation-data-header glyph-variation-data-header-extended
   glyph-variation-data-header-extended-p
   glyph-variation-data-header-extended-serialized-data-offset
   glyph-variation-data-header-extended-serialized-data-offset-offset
   glyph-variation-data-header-extended-tuple-variation-count
   glyph-variation-data-header-extended-tuple-variation-headers-start
   glyph-variation-data-header-p
   glyph-variation-data-header-serialized-data-offset
   glyph-variation-data-header-serialized-data-offset-offset
   glyph-variation-data-header-tuple-variation-count
   glyph-variation-data-header-tuple-variation-headers-start gvar
   gvar-axis-count gvar-extended gvar-extended-axis-count gvar-extended-flags
   gvar-extended-glyph-count gvar-extended-glyph-variation-data-array-offset
   gvar-extended-glyph-variation-data-offsets-start gvar-extended-p
   gvar-extended-shared-tuple-count gvar-extended-shared-tuples-offset
   gvar-extended-shared-tuples-offset-offset gvar-extended-version gvar-flags
   gvar-flags-long-offsets-p gvar-glyph-count
   gvar-glyph-variation-data-array-offset
   gvar-glyph-variation-data-offsets-start gvar-p gvar-shared-tuple-count
   gvar-shared-tuples-offset gvar-shared-tuples-offset-offset gvar-version
   read-glyph-variation-data-header read-glyph-variation-data-header-extended
   read-gvar read-gvar-extended read-shared-tuples
   sanitize-glyph-variation-data-header
   sanitize-glyph-variation-data-header-extended sanitize-gvar
   sanitize-gvar-extended sanitize-shared-tuples shared-tuples shared-tuples-p
   shared-tuples-tuples-start))

(define-constant +gvar-flags-long-offsets+ 1)

(declaim (inline gvar-flags-long-offsets-p))

(defun gvar-flags-long-offsets-p (flags)
  (logtest flags +gvar-flags-long-offsets+))

(defstruct
    (gvar (:include table-ref)
     (:constructor %make-gvar (%table-font-data %table-offset %table-length)))
  "The ['gvar' header](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar#gvar-header)")

(declaim
 (inline gvar-version gvar-version-raw gvar-axis-count gvar-axis-count-raw
  gvar-shared-tuple-count gvar-shared-tuple-count-raw
  gvar-shared-tuples-offset-offset gvar-shared-tuples-offset gvar-glyph-count
  gvar-glyph-count-raw gvar-flags gvar-flags-raw
  gvar-glyph-variation-data-array-offset
  gvar-glyph-variation-data-array-offset-raw
  gvar-glyph-variation-data-offsets-start))

(define-constant +gvar-tag+ "gvar")

(defun gvar-version (table)
  "Major/minor version number of the glyph variations table — set to (1,0)."
  (declare (type gvar table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun gvar-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun gvar-axis-count (table)
  "The number of variation axes for this font. This must be the same number as axisCount in the 'fvar' table."
  (declare (type gvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun gvar-axis-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun gvar-shared-tuple-count (table)
  "The number of shared tuple records. Shared tuple records can be referenced within glyph variation data tables for multiple glyphs, as opposed to other tuple records stored directly within a glyph variation data table."
  (declare (type gvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun gvar-shared-tuple-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun gvar-shared-tuples-offset-offset (table)
  "Offset from the start of this table to the shared tuple records."
  (declare (type gvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun gvar-shared-tuples-offset (table)
  "Offset from the start of this table to the shared tuple records."
  (declare (type gvar table))
  (let ((off (gvar-shared-tuples-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-shared-tuples (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gvar-glyph-count (table)
  "The number of glyphs in this font. This must match the number of glyphs stored elsewhere in the font."
  (declare (type gvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun gvar-glyph-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 12)))

(defun gvar-flags (table)
  "Bit-field that gives the format of the offset array that follows. If bit 0 is clear, the offsets are uint16; if bit 0 is set, the offsets are uint32."
  (declare (type gvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 14)))

(defun gvar-flags-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 14)))

(defun gvar-glyph-variation-data-array-offset (table)
  "Offset from the start of this table to the array of GlyphVariationData tables."
  (declare (type gvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun gvar-glyph-variation-data-array-offset-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 16)))

(defun gvar-glyph-variation-data-offsets-start (table)
  "Byte position of the glyph-variation-data-offsets field; its elements are variable-length U16-OR32 data."
  (declare (type gvar table))
  (+ (table-ref-offset table) 20))

(defun sanitize-gvar (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 20 length) (font-data-valid-range-p font-data offset 20))
    (error 'font-out-of-bounds-error :pos offset :size 20 :length
           (font-data-length font-data)))
  t)

(defun read-gvar
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-gvar font-data offset length)
  (%make-gvar font-data offset length))

(defstruct
    (gvar-extended (:include table-ref)
     (:constructor %make-gvar-extended
      (%table-font-data %table-offset %table-length)))
  "GVAR glyph variations table (ISO/IEC 14496-22:2026, 7.3.9).")

(declaim
 (inline gvar-extended-version gvar-extended-version-raw
  gvar-extended-axis-count gvar-extended-axis-count-raw
  gvar-extended-shared-tuple-count gvar-extended-shared-tuple-count-raw
  gvar-extended-shared-tuples-offset-offset gvar-extended-shared-tuples-offset
  gvar-extended-glyph-count gvar-extended-glyph-count-raw gvar-extended-flags
  gvar-extended-flags-raw gvar-extended-glyph-variation-data-array-offset
  gvar-extended-glyph-variation-data-array-offset-raw
  gvar-extended-glyph-variation-data-offsets-start))

(define-constant +gvar-extended-tag+ "GVAR")

(defun gvar-extended-version (table)
  (declare (type gvar-extended table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun gvar-extended-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun gvar-extended-axis-count (table)
  (declare (type gvar-extended table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun gvar-extended-axis-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun gvar-extended-shared-tuple-count (table)
  (declare (type gvar-extended table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun gvar-extended-shared-tuple-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun gvar-extended-shared-tuples-offset-offset (table)
  (declare (type gvar-extended table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun gvar-extended-shared-tuples-offset (table)
  (declare (type gvar-extended table))
  (let ((off (gvar-extended-shared-tuples-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-shared-tuples (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gvar-extended-glyph-count (table)
  "Number of glyphs, equal to numGlyphs in MAXP."
  (declare (type gvar-extended table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun gvar-extended-glyph-count-raw (font-data offset)
  (font-data-read-u24 font-data (+ offset 12)))

(defun gvar-extended-flags (table)
  (declare (type gvar-extended table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 15)))

(defun gvar-extended-flags-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 15)))

(defun gvar-extended-glyph-variation-data-array-offset (table)
  (declare (type gvar-extended table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 17)))

(defun gvar-extended-glyph-variation-data-array-offset-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 17)))

(defun gvar-extended-glyph-variation-data-offsets-start (table)
  "Byte position of the glyph-variation-data-offsets field; its elements are variable-length U16-OR32 data."
  (declare (type gvar-extended table))
  (+ (table-ref-offset table) 21))

(defun sanitize-gvar-extended (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 21 length) (font-data-valid-range-p font-data offset 21))
    (error 'font-out-of-bounds-error :pos offset :size 21 :length
           (font-data-length font-data)))
  t)

(defun read-gvar-extended
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-gvar-extended font-data offset length)
  (%make-gvar-extended font-data offset length))

(defstruct
    (shared-tuples (:include table-ref)
     (:constructor %make-shared-tuples
      (%table-font-data %table-offset %table-length)))
  "Array of tuple records shared across all glyph variation data tables.")

(declaim (inline shared-tuples-tuples-start))

(defun shared-tuples-tuples-start (table)
  "Byte position of the tuples field; its elements are variable-length TUPLE data."
  (declare (type shared-tuples table))
  (table-ref-offset table))

(defun sanitize-shared-tuples (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-shared-tuples
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-shared-tuples font-data offset length)
  (%make-shared-tuples font-data offset length))

(defstruct
    (glyph-variation-data-header (:include table-ref)
     (:constructor %make-glyph-variation-data-header
      (%table-font-data %table-offset %table-length)))
  "The [GlyphVariationData](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar#the-glyphvariationdata-table-array) table")

(declaim
 (inline glyph-variation-data-header-tuple-variation-count
  glyph-variation-data-header-tuple-variation-count-raw
  glyph-variation-data-header-serialized-data-offset-offset
  glyph-variation-data-header-serialized-data-offset
  glyph-variation-data-header-tuple-variation-headers-start))

(defun glyph-variation-data-header-tuple-variation-count (table)
  "A packed field. The high 4 bits are flags, and the low 12 bits are the number of tuple variation tables for this glyph. The number of tuple variation tables can be any number between 1 and 4095."
  (declare (type glyph-variation-data-header table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun glyph-variation-data-header-tuple-variation-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun glyph-variation-data-header-serialized-data-offset-offset (table)
  "Offset from the start of the GlyphVariationData table to the serialized data"
  (declare (type glyph-variation-data-header table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun glyph-variation-data-header-serialized-data-offset (table)
  "Offset from the start of the GlyphVariationData table to the serialized data"
  (declare (type glyph-variation-data-header table))
  (let ((off (glyph-variation-data-header-serialized-data-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun glyph-variation-data-header-tuple-variation-headers-start (table)
  "Byte position of the tuple-variation-headers field; its elements are variable-length TUPLE-VARIATION-HEADER data."
  (declare (type glyph-variation-data-header table))
  (+ (table-ref-offset table) 4))

(defun sanitize-glyph-variation-data-header (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-glyph-variation-data-header
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-glyph-variation-data-header font-data offset length)
  (%make-glyph-variation-data-header font-data offset length))

(defstruct
    (glyph-variation-data-header-extended (:include table-ref)
     (:constructor %make-glyph-variation-data-header-extended
      (%table-font-data %table-offset %table-length)))
  "Per-glyph variation header in GVAR (ISO/IEC 14496-22:2026, 7.3.9.1.3).")

(declaim
 (inline glyph-variation-data-header-extended-tuple-variation-count
  glyph-variation-data-header-extended-tuple-variation-count-raw
  glyph-variation-data-header-extended-serialized-data-offset-offset
  glyph-variation-data-header-extended-serialized-data-offset
  glyph-variation-data-header-extended-tuple-variation-headers-start))

(defun glyph-variation-data-header-extended-tuple-variation-count (table)
  (declare (type glyph-variation-data-header-extended table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun glyph-variation-data-header-extended-tuple-variation-count-raw
       (font-data offset)
  (font-data-read-u16 font-data offset))

(defun glyph-variation-data-header-extended-serialized-data-offset-offset
       (table)
  (declare (type glyph-variation-data-header-extended table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun glyph-variation-data-header-extended-serialized-data-offset (table)
  (declare (type glyph-variation-data-header-extended table))
  (let ((off
         (glyph-variation-data-header-extended-serialized-data-offset-offset
          table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun glyph-variation-data-header-extended-tuple-variation-headers-start
       (table)
  "Byte position of the tuple-variation-headers field; its elements are variable-length TUPLE-VARIATION-HEADER data."
  (declare (type glyph-variation-data-header-extended table))
  (+ (table-ref-offset table) 5))

(defun sanitize-glyph-variation-data-header-extended (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 5 length) (font-data-valid-range-p font-data offset 5))
    (error 'font-out-of-bounds-error :pos offset :size 5 :length
           (font-data-length font-data)))
  t)

(defun read-glyph-variation-data-header-extended
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-glyph-variation-data-header-extended font-data offset length)
  (%make-glyph-variation-data-header-extended font-data offset length))
