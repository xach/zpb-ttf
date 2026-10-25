;;;; kern.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(aat-kern aat-kern-n-tables aat-kern-p aat-kern-subtable-data-bytes
   aat-kern-subtable-data-count aat-kern-subtable-data-ref aat-kern-version
   aat-subtable aat-subtable-coverage aat-subtable-data-bytes
   aat-subtable-data-count aat-subtable-data-ref aat-subtable-length
   aat-subtable-p aat-subtable-tuple-index kern-subtable0
   kern-subtable0-entry-selector kern-subtable0-n-pairs kern-subtable0-p
   kern-subtable0-pairs-count kern-subtable0-pairs-ref
   kern-subtable0-range-shift kern-subtable0-search-range ot-kern
   ot-kern-n-tables ot-kern-p ot-kern-subtable-data-bytes
   ot-kern-subtable-data-count ot-kern-subtable-data-ref ot-kern-version
   ot-subtable ot-subtable-coverage ot-subtable-data-bytes
   ot-subtable-data-count ot-subtable-data-ref ot-subtable-length ot-subtable-p
   ot-subtable-version read-aat-kern read-aat-subtable read-kern-subtable0
   read-ot-kern read-ot-subtable read-subtable2-class-table read-subtable3
   sanitize-aat-kern sanitize-aat-subtable sanitize-kern-subtable0
   sanitize-ot-kern sanitize-ot-subtable sanitize-subtable2-class-table
   sanitize-subtable3 subtable2-class-table subtable2-class-table-first-glyph
   subtable2-class-table-n-glyphs subtable2-class-table-offsets-count
   subtable2-class-table-offsets-ref subtable2-class-table-p subtable3
   subtable3-flags subtable3-glyph-count subtable3-kern-index-bytes
   subtable3-kern-index-count subtable3-kern-index-ref
   subtable3-kern-value-array-count subtable3-kern-value-count
   subtable3-kern-value-ref subtable3-left-class-array-count
   subtable3-left-class-bytes subtable3-left-class-count
   subtable3-left-class-ref subtable3-p subtable3-right-class-array-count
   subtable3-right-class-bytes subtable3-right-class-count
   subtable3-right-class-ref))

(defstruct
    (ot-kern (:include table-ref)
     (:constructor %make-ot-kern
      (%table-font-data %table-offset %table-length)))
  "The OpenType [kerning](https://learn.microsoft.com/en-us/typography/opentype/spec/kern) table.")

(declaim
 (inline ot-kern-version ot-kern-version-raw ot-kern-n-tables
  ot-kern-n-tables-raw ot-kern-subtable-data-count ot-kern-subtable-data-ref
  ot-kern-subtable-data-bytes))

(defun ot-kern-version (table)
  "Table version number—set to 0."
  (declare (type ot-kern table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun ot-kern-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun ot-kern-n-tables (table)
  "Number of subtables in the kerning table."
  (declare (type ot-kern table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun ot-kern-n-tables-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun ot-kern-subtable-data-count (table)
  "Data for subtables, immediately following the header."
  (declare (type ot-kern table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 4))
        1)))

(defun ot-kern-subtable-data-ref (table index)
  "Data for subtables, immediately following the header."
  (declare (type ot-kern table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun ot-kern-subtable-data-bytes (table)
  "Data for subtables, immediately following the header."
  (declare (type ot-kern table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 4)
                   (ot-kern-subtable-data-count table)))

(defun sanitize-ot-kern (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-ot-kern
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ot-kern font-data offset length)
  (%make-ot-kern font-data offset length))

(defstruct
    (aat-kern (:include table-ref)
     (:constructor %make-aat-kern
      (%table-font-data %table-offset %table-length)))
  "The Apple Advanced Typography [kerning](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html) table.")

(declaim
 (inline aat-kern-version aat-kern-version-raw aat-kern-n-tables
  aat-kern-n-tables-raw aat-kern-subtable-data-count aat-kern-subtable-data-ref
  aat-kern-subtable-data-bytes))

(defun aat-kern-version (table)
  "The version number of the kerning table (0x00010000 for the current version)."
  (declare (type aat-kern table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun aat-kern-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun aat-kern-n-tables (table)
  "The number of subtables included in the kerning table."
  (declare (type aat-kern table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun aat-kern-n-tables-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun aat-kern-subtable-data-count (table)
  "Data for subtables, immediately following the header."
  (declare (type aat-kern table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 8))
        1)))

(defun aat-kern-subtable-data-ref (table index)
  "Data for subtables, immediately following the header."
  (declare (type aat-kern table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun aat-kern-subtable-data-bytes (table)
  "Data for subtables, immediately following the header."
  (declare (type aat-kern table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 8)
                   (aat-kern-subtable-data-count table)))

(defun sanitize-aat-kern (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-aat-kern
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-aat-kern font-data offset length)
  (%make-aat-kern font-data offset length))

(defstruct
    (ot-subtable (:include table-ref)
     (:constructor %make-ot-subtable
      (%table-font-data %table-offset %table-length)))
  "A subtable in an OT `kern` table.")

(declaim
 (inline ot-subtable-version ot-subtable-version-raw ot-subtable-length
  ot-subtable-length-raw ot-subtable-coverage ot-subtable-coverage-raw
  ot-subtable-data-count ot-subtable-data-ref ot-subtable-data-bytes))

(defun ot-subtable-version (table)
  "Kern subtable version number-- set to 0."
  (declare (type ot-subtable table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun ot-subtable-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun ot-subtable-length (table)
  "The length of this subtable in bytes, including this header."
  (declare (type ot-subtable table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun ot-subtable-length-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun ot-subtable-coverage (table)
  "Circumstances under which this table is used."
  (declare (type ot-subtable table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun ot-subtable-coverage-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun ot-subtable-data-count (table)
  "Subtable specific data."
  (declare (type ot-subtable table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 6))
        1)))

(defun ot-subtable-data-ref (table index)
  "Subtable specific data."
  (declare (type ot-subtable table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 6) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun ot-subtable-data-bytes (table)
  "Subtable specific data."
  (declare (type ot-subtable table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 6)
                   (ot-subtable-data-count table)))

(defun sanitize-ot-subtable (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  t)

(defun read-ot-subtable
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ot-subtable font-data offset length)
  (%make-ot-subtable font-data offset length))

(defstruct
    (aat-subtable (:include table-ref)
     (:constructor %make-aat-subtable
      (%table-font-data %table-offset %table-length)))
  "A subtable in an AAT `kern` table.")

(declaim
 (inline aat-subtable-length aat-subtable-length-raw aat-subtable-coverage
  aat-subtable-coverage-raw aat-subtable-tuple-index
  aat-subtable-tuple-index-raw aat-subtable-data-count aat-subtable-data-ref
  aat-subtable-data-bytes))

(defun aat-subtable-length (table)
  "The length of this subtable in bytes, including this header."
  (declare (type aat-subtable table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun aat-subtable-length-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun aat-subtable-coverage (table)
  "Circumstances under which this table is used."
  (declare (type aat-subtable table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun aat-subtable-coverage-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun aat-subtable-tuple-index (table)
  "The tuple index (used for variations fonts). This value specifies which tuple this subtable covers."
  (declare (type aat-subtable table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun aat-subtable-tuple-index-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun aat-subtable-data-count (table)
  "Subtable specific data."
  (declare (type aat-subtable table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 8))
        1)))

(defun aat-subtable-data-ref (table index)
  "Subtable specific data."
  (declare (type aat-subtable table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun aat-subtable-data-bytes (table)
  "Subtable specific data."
  (declare (type aat-subtable table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 8)
                   (aat-subtable-data-count table)))

(defun sanitize-aat-subtable (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-aat-subtable
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-aat-subtable font-data offset length)
  (%make-aat-subtable font-data offset length))

(defstruct
    (kern-subtable0 (:include table-ref)
     (:constructor %make-kern-subtable0
      (%table-font-data %table-offset %table-length)))
  "The type 0 `kern` subtable.")

(declaim
 (inline kern-subtable0-n-pairs kern-subtable0-n-pairs-raw
  kern-subtable0-search-range kern-subtable0-search-range-raw
  kern-subtable0-entry-selector kern-subtable0-entry-selector-raw
  kern-subtable0-range-shift kern-subtable0-range-shift-raw
  kern-subtable0-pairs-count kern-subtable0-pairs-ref))

(defun kern-subtable0-n-pairs (table)
  "The number of kerning pairs in this subtable."
  (declare (type kern-subtable0 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun kern-subtable0-n-pairs-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun kern-subtable0-search-range (table)
  "The largest power of two less than or equal to the value of nPairs, multiplied by the size in bytes of an entry in the subtable."
  (declare (type kern-subtable0 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun kern-subtable0-search-range-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun kern-subtable0-entry-selector (table)
  "This is calculated as log2 of the largest power of two less than or equal to the value of nPairs. This value indicates how many iterations of the search loop have to be made. For example, in a list of eight items, there would be three iterations of the loop."
  (declare (type kern-subtable0 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun kern-subtable0-entry-selector-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun kern-subtable0-range-shift (table)
  "The value of nPairs minus the largest power of two less than or equal to nPairs. This is multiplied by the size in bytes of an entry in the table."
  (declare (type kern-subtable0 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun kern-subtable0-range-shift-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun kern-subtable0-pairs-count (table)
  "Kerning records."
  (declare (type kern-subtable0 table))
  (declare (ignorable table))
  (kern-subtable0-n-pairs table))

(defun kern-subtable0-pairs-ref (table index)
  "Kerning records."
  (declare (type kern-subtable0 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-kerx-subtable0-pair (table-ref-font-data table) pos)))

(defun sanitize-kern-subtable0 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  (let* ((cnt (kern-subtable0-n-pairs-raw font-data offset))
         (pos (+ 8 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-kern-subtable0
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-kern-subtable0 font-data offset length)
  (%make-kern-subtable0 font-data offset length))

(defstruct
    (subtable2-class-table (:include table-ref)
     (:constructor %make-subtable2-class-table
      (%table-font-data %table-offset %table-length)))
  "Class table for the type 2 `kern` subtable.")

(declaim
 (inline subtable2-class-table-first-glyph
  subtable2-class-table-first-glyph-raw subtable2-class-table-n-glyphs
  subtable2-class-table-n-glyphs-raw subtable2-class-table-offsets-count
  subtable2-class-table-offsets-ref))

(defun subtable2-class-table-first-glyph (table)
  "First glyph in class range."
  (declare (type subtable2-class-table table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun subtable2-class-table-first-glyph-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun subtable2-class-table-n-glyphs (table)
  "Number of glyph in class range."
  (declare (type subtable2-class-table table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun subtable2-class-table-n-glyphs-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun subtable2-class-table-offsets-count (table)
  "The offsets array for all of the glyphs in the range."
  (declare (type subtable2-class-table table))
  (declare (ignorable table))
  (subtable2-class-table-n-glyphs table))

(defun subtable2-class-table-offsets-ref (table index)
  "The offsets array for all of the glyphs in the range."
  (declare (type subtable2-class-table table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun sanitize-subtable2-class-table (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (subtable2-class-table-n-glyphs-raw font-data offset))
         (pos (+ 4 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-subtable2-class-table
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-subtable2-class-table font-data offset length)
  (%make-subtable2-class-table font-data offset length))

(defstruct
    (subtable3 (:include table-ref)
     (:constructor %make-subtable3
      (%table-font-data %table-offset %table-length)))
  "The type 3 'kern' subtable.")

(declaim
 (inline subtable3-glyph-count subtable3-glyph-count-raw
  subtable3-kern-value-count subtable3-kern-value-count-raw
  subtable3-left-class-count subtable3-left-class-count-raw
  subtable3-right-class-count subtable3-right-class-count-raw subtable3-flags
  subtable3-flags-raw subtable3-kern-value-array-count subtable3-kern-value-ref
  subtable3-left-class-array-count subtable3-left-class-ref
  subtable3-left-class-bytes subtable3-right-class-array-count
  subtable3-right-class-ref subtable3-right-class-bytes
  subtable3-kern-index-count subtable3-kern-index-ref
  subtable3-kern-index-bytes))

(defun subtable3-glyph-count (table)
  "The number of glyphs in this font."
  (declare (type subtable3 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun subtable3-glyph-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun subtable3-kern-value-count (table)
  "The number of kerning values."
  (declare (type subtable3 table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 2)))

(defun subtable3-kern-value-count-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 2)))

(defun subtable3-left-class-count (table)
  "The number of left-hand classes."
  (declare (type subtable3 table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 3)))

(defun subtable3-left-class-count-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 3)))

(defun subtable3-right-class-count (table)
  "The number of right-hand classes."
  (declare (type subtable3 table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 4)))

(defun subtable3-right-class-count-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 4)))

(defun subtable3-flags (table)
  "Set to zero (reserved for future use)."
  (declare (type subtable3 table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 5)))

(defun subtable3-flags-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 5)))

(defun subtable3-kern-value-array-count (table)
  "The kerning values."
  (declare (type subtable3 table))
  (declare (ignorable table))
  (subtable3-kern-value-count table))

(defun subtable3-kern-value-ref (table index)
  "The kerning values."
  (declare (type subtable3 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 6) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-i16 (table-ref-font-data table) pos)))

(defun subtable3-left-class-array-count (table)
  "The left-hand classes."
  (declare (type subtable3 table))
  (declare (ignorable table))
  (subtable3-glyph-count table))

(defun subtable3-left-class-ref (table index)
  "The left-hand classes."
  (declare (type subtable3 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 6
             (* (subtable3-kern-value-array-count table) 2))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun subtable3-left-class-bytes (table)
  "The left-hand classes."
  (declare (type subtable3 table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 6
                      (* (subtable3-kern-value-array-count table) 2))
                   (subtable3-left-class-array-count table)))

(defun subtable3-right-class-array-count (table)
  "The right-hand classes."
  (declare (type subtable3 table))
  (declare (ignorable table))
  (subtable3-glyph-count table))

(defun subtable3-right-class-ref (table index)
  "The right-hand classes."
  (declare (type subtable3 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 6
             (* (subtable3-kern-value-array-count table) 2)
             (* (subtable3-left-class-array-count table) 1))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun subtable3-right-class-bytes (table)
  "The right-hand classes."
  (declare (type subtable3 table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 6
                      (* (subtable3-kern-value-array-count table) 2)
                      (* (subtable3-left-class-array-count table) 1))
                   (subtable3-right-class-array-count table)))

(defun subtable3-kern-index-count (table)
  "The indices into the kernValue array."
  (declare (type subtable3 table))
  (declare (ignorable table))
  (* (+ (subtable3-left-class-count table) 0)
     (subtable3-right-class-count table)))

(defun subtable3-kern-index-ref (table index)
  "The indices into the kernValue array."
  (declare (type subtable3 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 6
             (* (subtable3-kern-value-array-count table) 2)
             (* (subtable3-left-class-array-count table) 1)
             (* (subtable3-right-class-array-count table) 1))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun subtable3-kern-index-bytes (table)
  "The indices into the kernValue array."
  (declare (type subtable3 table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 6
                      (* (subtable3-kern-value-array-count table) 2)
                      (* (subtable3-left-class-array-count table) 1)
                      (* (subtable3-right-class-array-count table) 1))
                   (subtable3-kern-index-count table)))

(defun sanitize-subtable3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  (let* ((cnt (subtable3-kern-value-count-raw font-data offset))
         (pos (+ 6 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-subtable3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-subtable3 font-data offset length)
  (%make-subtable3 font-data offset length))
