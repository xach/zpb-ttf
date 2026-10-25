;;;; aat.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+lookup-segment4-size+ class-subtable class-subtable-class-array-bytes
   class-subtable-class-array-count class-subtable-class-array-ref
   class-subtable-first-glyph class-subtable-n-glyphs class-subtable-p
   lookup-segment4 lookup-segment4-first-glyph lookup-segment4-last-glyph
   lookup-segment4-p lookup-segment4-value-offset lookup0 lookup0-format-table
   lookup0-p lookup0-values-data-bytes lookup0-values-data-count
   lookup0-values-data-ref lookup10 lookup10-first-glyph lookup10-format-table
   lookup10-glyph-count lookup10-p lookup10-unit-size
   lookup10-values-data-bytes lookup10-values-data-count
   lookup10-values-data-ref lookup2 lookup2-entry-selector lookup2-format-table
   lookup2-n-units lookup2-p lookup2-range-shift lookup2-search-range
   lookup2-segments-data-bytes lookup2-segments-data-count
   lookup2-segments-data-ref lookup2-unit-size lookup4 lookup4-entry-selector
   lookup4-format-table lookup4-n-units lookup4-p lookup4-range-shift
   lookup4-search-range lookup4-segments-count lookup4-segments-ref
   lookup4-unit-size lookup6 lookup6-entries-data-bytes
   lookup6-entries-data-count lookup6-entries-data-ref lookup6-entry-selector
   lookup6-format-table lookup6-n-units lookup6-p lookup6-range-shift
   lookup6-search-range lookup6-unit-size lookup8 lookup8-first-glyph
   lookup8-format-table lookup8-glyph-count lookup8-p lookup8-value-array-count
   lookup8-value-array-ref raw-bytes raw-bytes-data-bytes raw-bytes-data-count
   raw-bytes-data-ref raw-bytes-p raw-words raw-words-data-count
   raw-words-data-ref raw-words-p read-aat-lookup read-class-subtable
   read-lookup0 read-lookup10 read-lookup2 read-lookup4 read-lookup6
   read-lookup8 read-raw-bytes read-raw-words read-state-header read-stx-header
   sanitize-class-subtable sanitize-lookup0 sanitize-lookup10 sanitize-lookup2
   sanitize-lookup4 sanitize-lookup6 sanitize-lookup8 sanitize-raw-bytes
   sanitize-raw-words sanitize-state-header sanitize-stx-header state-header
   state-header-class-table-offset state-header-class-table-offset-offset
   state-header-entry-table-offset state-header-entry-table-offset-offset
   state-header-p state-header-state-array-offset
   state-header-state-array-offset-offset state-header-state-size stx-header
   stx-header-class-table-offset stx-header-class-table-offset-offset
   stx-header-entry-table-offset stx-header-entry-table-offset-offset
   stx-header-n-classes stx-header-p stx-header-state-array-offset
   stx-header-state-array-offset-offset))

(defstruct
    (lookup-segment4 (:include record-ref)
     (:constructor %make-lookup-segment4 (%record-font-data %record-offset)))
  "Lookup segment for format 4.")

(define-constant +lookup-segment4-size+ 6)

(declaim
 (inline lookup-segment4-last-glyph lookup-segment4-first-glyph
  lookup-segment4-value-offset))

(defun lookup-segment4-last-glyph (rec)
  "Last glyph index in this segment."
  (declare (type lookup-segment4 rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun lookup-segment4-first-glyph (rec)
  "First glyph index in this segment."
  (declare (type lookup-segment4 rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun lookup-segment4-value-offset (rec)
  "A 16-bit offset from the start of the table to the data."
  (declare (type lookup-segment4 rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun read-aat-lookup
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "Lookup tables provide a way of looking up information about a glyph index. The different cmap subtable formats."
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (0 (read-lookup0 font-data offset length))
      (2 (read-lookup2 font-data offset length))
      (4 (read-lookup4 font-data offset length))
      (6 (read-lookup6 font-data offset length))
      (8 (read-lookup8 font-data offset length))
      (10 (read-lookup10 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'aat-lookup)))))

(defstruct
    (lookup0 (:include table-ref)
     (:constructor %make-lookup0
      (%table-font-data %table-offset %table-length)))
  "Simple array format. The lookup data is an array of lookup values, indexed by glyph index.")

(declaim
 (inline lookup0-format-table lookup0-format-table-raw
  lookup0-values-data-count lookup0-values-data-ref lookup0-values-data-bytes))

(defun lookup0-format-table (table)
  "Format number is set to 0."
  (declare (type lookup0 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun lookup0-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun lookup0-values-data-count (table)
  "Values, indexed by glyph index."
  (declare (type lookup0 table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 2))
        1)))

(defun lookup0-values-data-ref (table index)
  "Values, indexed by glyph index."
  (declare (type lookup0 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun lookup0-values-data-bytes (table)
  "Values, indexed by glyph index."
  (declare (type lookup0 table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 2)
                   (lookup0-values-data-count table)))

(defun sanitize-lookup0 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  t)

(defun read-lookup0
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-lookup0 font-data offset length)
  (%make-lookup0 font-data offset length))

(defstruct
    (lookup2 (:include table-ref)
     (:constructor %make-lookup2
      (%table-font-data %table-offset %table-length)))
  "Segment single format. Each non-overlapping segment has a single lookup value that applies to all glyphs in the segment. A segment is defined as a contiguous range of glyph indexes.")

(declaim
 (inline lookup2-format-table lookup2-format-table-raw lookup2-unit-size
  lookup2-unit-size-raw lookup2-n-units lookup2-n-units-raw
  lookup2-search-range lookup2-search-range-raw lookup2-entry-selector
  lookup2-entry-selector-raw lookup2-range-shift lookup2-range-shift-raw
  lookup2-segments-data-count lookup2-segments-data-ref
  lookup2-segments-data-bytes))

(defun lookup2-format-table (table)
  "Format number is set to 2."
  (declare (type lookup2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun lookup2-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun lookup2-unit-size (table)
  "Size of a lookup unit for this search in bytes."
  (declare (type lookup2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun lookup2-unit-size-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun lookup2-n-units (table)
  "Number of units of the preceding size to be searched."
  (declare (type lookup2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun lookup2-n-units-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun lookup2-search-range (table)
  "The value of unitSize times the largest power of 2 that is less than or equal to the value of nUnits."
  (declare (type lookup2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun lookup2-search-range-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun lookup2-entry-selector (table)
  "The log base 2 of the largest power of 2 less than or equal to the value of nUnits."
  (declare (type lookup2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun lookup2-entry-selector-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun lookup2-range-shift (table)
  "The value of unitSize times the difference of the value of nUnits minus the largest power of 2 less than or equal to the value of nUnits."
  (declare (type lookup2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun lookup2-range-shift-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun lookup2-segments-data-count (table)
  "Segments."
  (declare (type lookup2 table))
  (declare (ignorable table))
  (* (+ (lookup2-unit-size table) 0) (lookup2-n-units table)))

(defun lookup2-segments-data-ref (table index)
  "Segments."
  (declare (type lookup2 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun lookup2-segments-data-bytes (table)
  "Segments."
  (declare (type lookup2 table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 12)
                   (lookup2-segments-data-count table)))

(defun sanitize-lookup2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-lookup2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-lookup2 font-data offset length)
  (%make-lookup2 font-data offset length))

(defstruct
    (lookup4 (:include table-ref)
     (:constructor %make-lookup4
      (%table-font-data %table-offset %table-length)))
  "Segment array format. A segment mapping is performed (as with Format 2), but instead of a single lookup value for all the glyphs in the segment, each glyph in the segment gets its own separate lookup value.")

(declaim
 (inline lookup4-format-table lookup4-format-table-raw lookup4-unit-size
  lookup4-unit-size-raw lookup4-n-units lookup4-n-units-raw
  lookup4-search-range lookup4-search-range-raw lookup4-entry-selector
  lookup4-entry-selector-raw lookup4-range-shift lookup4-range-shift-raw
  lookup4-segments-count lookup4-segments-ref))

(defun lookup4-format-table (table)
  "Format number is set to 4."
  (declare (type lookup4 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun lookup4-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun lookup4-unit-size (table)
  "Size of a lookup unit for this search in bytes."
  (declare (type lookup4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun lookup4-unit-size-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun lookup4-n-units (table)
  "Number of units of the preceding size to be searched."
  (declare (type lookup4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun lookup4-n-units-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun lookup4-search-range (table)
  "The value of unitSize times the largest power of 2 that is less than or equal to the value of nUnits."
  (declare (type lookup4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun lookup4-search-range-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun lookup4-entry-selector (table)
  "The log base 2 of the largest power of 2 less than or equal to the value of nUnits."
  (declare (type lookup4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun lookup4-entry-selector-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun lookup4-range-shift (table)
  "The value of unitSize times the difference of the value of nUnits minus the largest power of 2 less than or equal to the value of nUnits."
  (declare (type lookup4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun lookup4-range-shift-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun lookup4-segments-count (table)
  "Segments."
  (declare (type lookup4 table))
  (declare (ignorable table))
  (lookup4-n-units table))

(defun lookup4-segments-ref (table index)
  "Segments."
  (declare (type lookup4 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-lookup-segment4 (table-ref-font-data table) pos)))

(defun sanitize-lookup4 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  (let* ((cnt (lookup4-n-units-raw font-data offset)) (pos (+ 12 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-lookup4
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-lookup4 font-data offset length)
  (%make-lookup4 font-data offset length))

(defstruct
    (lookup6 (:include table-ref)
     (:constructor %make-lookup6
      (%table-font-data %table-offset %table-length)))
  "Single table format. The lookup data is a sorted list of <glyph index,lookup value> pairs.")

(declaim
 (inline lookup6-format-table lookup6-format-table-raw lookup6-unit-size
  lookup6-unit-size-raw lookup6-n-units lookup6-n-units-raw
  lookup6-search-range lookup6-search-range-raw lookup6-entry-selector
  lookup6-entry-selector-raw lookup6-range-shift lookup6-range-shift-raw
  lookup6-entries-data-count lookup6-entries-data-ref
  lookup6-entries-data-bytes))

(defun lookup6-format-table (table)
  "Format number is set to 6."
  (declare (type lookup6 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun lookup6-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun lookup6-unit-size (table)
  "Size of a lookup unit for this search in bytes."
  (declare (type lookup6 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun lookup6-unit-size-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun lookup6-n-units (table)
  "Number of units of the preceding size to be searched."
  (declare (type lookup6 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun lookup6-n-units-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun lookup6-search-range (table)
  "The value of unitSize times the largest power of 2 that is less than or equal to the value of nUnits."
  (declare (type lookup6 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun lookup6-search-range-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun lookup6-entry-selector (table)
  "The log base 2 of the largest power of 2 less than or equal to the value of nUnits."
  (declare (type lookup6 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun lookup6-entry-selector-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun lookup6-range-shift (table)
  "The value of unitSize times the difference of the value of nUnits minus the largest power of 2 less than or equal to the value of nUnits."
  (declare (type lookup6 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun lookup6-range-shift-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun lookup6-entries-data-count (table)
  "Values, indexed by glyph index."
  (declare (type lookup6 table))
  (declare (ignorable table))
  (* (+ (lookup6-unit-size table) 0) (lookup6-n-units table)))

(defun lookup6-entries-data-ref (table index)
  "Values, indexed by glyph index."
  (declare (type lookup6 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun lookup6-entries-data-bytes (table)
  "Values, indexed by glyph index."
  (declare (type lookup6 table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 12)
                   (lookup6-entries-data-count table)))

(defun sanitize-lookup6 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-lookup6
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-lookup6 font-data offset length)
  (%make-lookup6 font-data offset length))

(defstruct
    (lookup8 (:include table-ref)
     (:constructor %make-lookup8
      (%table-font-data %table-offset %table-length)))
  "Trimmed array format. The lookup data is a simple trimmed array indexed by glyph index.")

(declaim
 (inline lookup8-format-table lookup8-format-table-raw lookup8-first-glyph
  lookup8-first-glyph-raw lookup8-glyph-count lookup8-glyph-count-raw
  lookup8-value-array-count lookup8-value-array-ref))

(defun lookup8-format-table (table)
  "Format number is set to 8."
  (declare (type lookup8 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun lookup8-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun lookup8-first-glyph (table)
  "First glyph index included in the trimmed array."
  (declare (type lookup8 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun lookup8-first-glyph-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun lookup8-glyph-count (table)
  "Total number of glyphs (equivalent to the last glyph minus the value of firstGlyph plus 1)."
  (declare (type lookup8 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun lookup8-glyph-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun lookup8-value-array-count (table)
  "The lookup values (indexed by the glyph index minus the value of firstGlyph). Entries in the value array must be two bytes."
  (declare (type lookup8 table))
  (declare (ignorable table))
  (lookup8-glyph-count table))

(defun lookup8-value-array-ref (table index)
  "The lookup values (indexed by the glyph index minus the value of firstGlyph). Entries in the value array must be two bytes."
  (declare (type lookup8 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 6) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun sanitize-lookup8 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  (let* ((cnt (lookup8-glyph-count-raw font-data offset)) (pos (+ 6 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-lookup8
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-lookup8 font-data offset length)
  (%make-lookup8 font-data offset length))

(defstruct
    (lookup10 (:include table-ref)
     (:constructor %make-lookup10
      (%table-font-data %table-offset %table-length)))
  "Trimmed array format. The lookup data is a simple trimmed array indexed by glyph index.")

(declaim
 (inline lookup10-format-table lookup10-format-table-raw lookup10-unit-size
  lookup10-unit-size-raw lookup10-first-glyph lookup10-first-glyph-raw
  lookup10-glyph-count lookup10-glyph-count-raw lookup10-values-data-count
  lookup10-values-data-ref lookup10-values-data-bytes))

(defun lookup10-format-table (table)
  "Format number is set to 10."
  (declare (type lookup10 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun lookup10-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun lookup10-unit-size (table)
  "Size of a lookup unit for this lookup table in bytes. Allowed values are 1, 2, 4, and 8."
  (declare (type lookup10 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun lookup10-unit-size-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun lookup10-first-glyph (table)
  "First glyph index included in the trimmed array."
  (declare (type lookup10 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun lookup10-first-glyph-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun lookup10-glyph-count (table)
  "Total number of glyphs (equivalent to the last glyph minus the value of firstGlyph plus 1)."
  (declare (type lookup10 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun lookup10-glyph-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun lookup10-values-data-count (table)
  "The lookup values (indexed by the glyph index minus the value of firstGlyph)."
  (declare (type lookup10 table))
  (declare (ignorable table))
  (* (+ (lookup10-glyph-count table) 0) (lookup10-unit-size table)))

(defun lookup10-values-data-ref (table index)
  "The lookup values (indexed by the glyph index minus the value of firstGlyph)."
  (declare (type lookup10 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun lookup10-values-data-bytes (table)
  "The lookup values (indexed by the glyph index minus the value of firstGlyph)."
  (declare (type lookup10 table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 8)
                   (lookup10-values-data-count table)))

(defun sanitize-lookup10 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-lookup10
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-lookup10 font-data offset length)
  (%make-lookup10 font-data offset length))

(defstruct
    (state-header (:include table-ref)
     (:constructor %make-state-header
      (%table-font-data %table-offset %table-length)))
  "Header for a state table.")

(declaim
 (inline state-header-state-size state-header-state-size-raw
  state-header-class-table-offset-offset state-header-class-table-offset
  state-header-state-array-offset-offset state-header-state-array-offset
  state-header-entry-table-offset-offset state-header-entry-table-offset))

(defun state-header-state-size (table)
  "Size of a state, in bytes. The size is limited to 8 bits, although the field is 16 bits for alignment."
  (declare (type state-header table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun state-header-state-size-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun state-header-class-table-offset-offset (table)
  "Byte offset from the beginning of the state table to the class subtable."
  (declare (type state-header table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun state-header-class-table-offset (table)
  "Byte offset from the beginning of the state table to the class subtable."
  (declare (type state-header table))
  (let ((off (state-header-class-table-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-class-subtable (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun state-header-state-array-offset-offset (table)
  "Byte offset from the beginning of the state table to the state array."
  (declare (type state-header table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun state-header-state-array-offset (table)
  "Byte offset from the beginning of the state table to the state array."
  (declare (type state-header table))
  (let ((off (state-header-state-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-raw-bytes (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun state-header-entry-table-offset-offset (table)
  "Byte offset from the beginning of the state table to the entry subtable."
  (declare (type state-header table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun state-header-entry-table-offset (table)
  "Byte offset from the beginning of the state table to the entry subtable."
  (declare (type state-header table))
  (let ((off (state-header-entry-table-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-raw-bytes (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-state-header (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-state-header
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-state-header font-data offset length)
  (%make-state-header font-data offset length))

(defstruct
    (class-subtable (:include table-ref)
     (:constructor %make-class-subtable
      (%table-font-data %table-offset %table-length)))
  "Maps the glyph indexes of your font into classes.")

(declaim
 (inline class-subtable-first-glyph class-subtable-first-glyph-raw
  class-subtable-n-glyphs class-subtable-n-glyphs-raw
  class-subtable-class-array-count class-subtable-class-array-ref
  class-subtable-class-array-bytes))

(defun class-subtable-first-glyph (table)
  "Glyph index of the first glyph in the class table."
  (declare (type class-subtable table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun class-subtable-first-glyph-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun class-subtable-n-glyphs (table)
  "Number of glyphs in class table."
  (declare (type class-subtable table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun class-subtable-n-glyphs-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun class-subtable-class-array-count (table)
  "The class codes (indexed by glyph index minus firstGlyph). Class codes range from 0 to the value of stateSize minus 1."
  (declare (type class-subtable table))
  (declare (ignorable table))
  (class-subtable-n-glyphs table))

(defun class-subtable-class-array-ref (table index)
  "The class codes (indexed by glyph index minus firstGlyph). Class codes range from 0 to the value of stateSize minus 1."
  (declare (type class-subtable table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun class-subtable-class-array-bytes (table)
  "The class codes (indexed by glyph index minus firstGlyph). Class codes range from 0 to the value of stateSize minus 1."
  (declare (type class-subtable table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 4)
                   (class-subtable-class-array-count table)))

(defun sanitize-class-subtable (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (class-subtable-n-glyphs-raw font-data offset))
         (pos (+ 4 (* cnt 1))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-class-subtable
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-class-subtable font-data offset length)
  (%make-class-subtable font-data offset length))

(defstruct
    (raw-bytes (:include table-ref)
     (:constructor %make-raw-bytes
      (%table-font-data %table-offset %table-length)))
  "Used for the `state_array` and `entry_table` fields in [`StateHeader`].")

(declaim (inline raw-bytes-data-count raw-bytes-data-ref raw-bytes-data-bytes))

(defun raw-bytes-data-count (table)
  (declare (type raw-bytes table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (table-ref-offset table))
        1)))

(defun raw-bytes-data-ref (table index)
  (declare (type raw-bytes table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun raw-bytes-data-bytes (table)
  (declare (type raw-bytes table))
  (font-data-slice (table-ref-font-data table) (table-ref-offset table)
                   (raw-bytes-data-count table)))

(defun sanitize-raw-bytes (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-raw-bytes
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-raw-bytes font-data offset length)
  (%make-raw-bytes font-data offset length))

(defstruct
    (stx-header (:include table-ref)
     (:constructor %make-stx-header
      (%table-font-data %table-offset %table-length)))
  "Header for an extended state table.")

(declaim
 (inline stx-header-n-classes stx-header-n-classes-raw
  stx-header-class-table-offset-offset stx-header-class-table-offset
  stx-header-state-array-offset-offset stx-header-state-array-offset
  stx-header-entry-table-offset-offset stx-header-entry-table-offset))

(defun stx-header-n-classes (table)
  "Number of classes, which is the number of 16-bit entry indices in a single line in the state array."
  (declare (type stx-header table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun stx-header-n-classes-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun stx-header-class-table-offset-offset (table)
  "Byte offset from the beginning of the state table to the class subtable."
  (declare (type stx-header table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun stx-header-class-table-offset (table)
  "Byte offset from the beginning of the state table to the class subtable."
  (declare (type stx-header table))
  (let ((off (stx-header-class-table-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun stx-header-state-array-offset-offset (table)
  "Byte offset from the beginning of the state table to the state array."
  (declare (type stx-header table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun stx-header-state-array-offset (table)
  "Byte offset from the beginning of the state table to the state array."
  (declare (type stx-header table))
  (let ((off (stx-header-state-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-raw-words (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun stx-header-entry-table-offset-offset (table)
  "Byte offset from the beginning of the state table to the entry subtable."
  (declare (type stx-header table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun stx-header-entry-table-offset (table)
  "Byte offset from the beginning of the state table to the entry subtable."
  (declare (type stx-header table))
  (let ((off (stx-header-entry-table-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-raw-bytes (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-stx-header (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  t)

(defun read-stx-header
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-stx-header font-data offset length)
  (%make-stx-header font-data offset length))

(defstruct
    (raw-words (:include table-ref)
     (:constructor %make-raw-words
      (%table-font-data %table-offset %table-length)))
  "Used for the `state_array` in [`StxHeader`].")

(declaim (inline raw-words-data-count raw-words-data-ref))

(defun raw-words-data-count (table)
  (declare (type raw-words table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (table-ref-offset table))
        2)))

(defun raw-words-data-ref (table index)
  (declare (type raw-words table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun sanitize-raw-words (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-raw-words
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-raw-words font-data offset length)
  (%make-raw-words font-data offset length))
