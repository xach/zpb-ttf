;;;; bitmap.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+bdt-component-size+ +big-glyph-metrics-size+
   +bitmap-flags-horizontal-metrics+ +bitmap-flags-vertical-metrics+
   +bitmap-size-size+ +glyph-id-offset-pair-size+ +index-subtable-record-size+
   +sbit-line-metrics-size+ +small-glyph-metrics-size+ bdt-component
   bdt-component-glyph-id bdt-component-p bdt-component-x-offset
   bdt-component-y-offset big-glyph-metrics big-glyph-metrics-height
   big-glyph-metrics-hori-advance big-glyph-metrics-hori-bearing-x
   big-glyph-metrics-hori-bearing-y big-glyph-metrics-p
   big-glyph-metrics-vert-advance big-glyph-metrics-vert-bearing-x
   big-glyph-metrics-vert-bearing-y big-glyph-metrics-width
   bitmap-flags-horizontal-metrics-p bitmap-flags-vertical-metrics-p
   bitmap-size bitmap-size-bit-depth bitmap-size-color-ref
   bitmap-size-end-glyph-index bitmap-size-flags bitmap-size-hori
   bitmap-size-index-subtable-list-offset bitmap-size-index-subtable-list-size
   bitmap-size-number-of-index-subtables bitmap-size-p bitmap-size-ppem-x
   bitmap-size-ppem-y bitmap-size-start-glyph-index bitmap-size-vert
   glyph-id-offset-pair glyph-id-offset-pair-glyph-id glyph-id-offset-pair-p
   glyph-id-offset-pair-sbit-offset index-subtable-list
   index-subtable-list-index-subtable-records-count
   index-subtable-list-index-subtable-records-ref index-subtable-list-p
   index-subtable-record index-subtable-record-first-glyph-index
   index-subtable-record-index-subtable-offset
   index-subtable-record-last-glyph-index index-subtable-record-p
   index-subtable1 index-subtable1-image-data-offset
   index-subtable1-image-format index-subtable1-index-format index-subtable1-p
   index-subtable1-sbit-offsets-count index-subtable1-sbit-offsets-ref
   index-subtable2 index-subtable2-big-metrics
   index-subtable2-image-data-offset index-subtable2-image-format
   index-subtable2-image-size index-subtable2-index-format index-subtable2-p
   index-subtable3 index-subtable3-image-data-offset
   index-subtable3-image-format index-subtable3-index-format index-subtable3-p
   index-subtable3-sbit-offsets-count index-subtable3-sbit-offsets-ref
   index-subtable4 index-subtable4-glyph-array-count
   index-subtable4-glyph-array-ref index-subtable4-image-data-offset
   index-subtable4-image-format index-subtable4-index-format
   index-subtable4-num-glyphs index-subtable4-p index-subtable5
   index-subtable5-big-metrics index-subtable5-glyph-array-count
   index-subtable5-glyph-array-ref index-subtable5-image-data-offset
   index-subtable5-image-format index-subtable5-image-size
   index-subtable5-index-format index-subtable5-num-glyphs index-subtable5-p
   read-index-subtable-list read-index-subtable1 read-index-subtable2
   read-index-subtable3 read-index-subtable4 read-index-subtable5
   sanitize-index-subtable-list sanitize-index-subtable1
   sanitize-index-subtable2 sanitize-index-subtable3 sanitize-index-subtable4
   sanitize-index-subtable5 sbit-line-metrics sbit-line-metrics-ascender
   sbit-line-metrics-caret-offset sbit-line-metrics-caret-slope-denominator
   sbit-line-metrics-caret-slope-numerator sbit-line-metrics-descender
   sbit-line-metrics-max-before-bl sbit-line-metrics-min-advance-sb
   sbit-line-metrics-min-after-bl sbit-line-metrics-min-origin-sb
   sbit-line-metrics-p sbit-line-metrics-pad1 sbit-line-metrics-pad2
   sbit-line-metrics-width-max small-glyph-metrics small-glyph-metrics-advance
   small-glyph-metrics-bearing-x small-glyph-metrics-bearing-y
   small-glyph-metrics-height small-glyph-metrics-p small-glyph-metrics-width))

(define-constant +bitmap-flags-horizontal-metrics+ 1
                 "[Bitmap flags](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#bitmap-flags).")

(declaim (inline bitmap-flags-horizontal-metrics-p))

(defun bitmap-flags-horizontal-metrics-p (flags)
  (logtest flags +bitmap-flags-horizontal-metrics+))

(define-constant +bitmap-flags-vertical-metrics+ 2
                 "[Bitmap flags](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#bitmap-flags).")

(declaim (inline bitmap-flags-vertical-metrics-p))

(defun bitmap-flags-vertical-metrics-p (flags)
  (logtest flags +bitmap-flags-vertical-metrics+))

(defstruct
    (bitmap-size (:include record-ref)
     (:constructor %make-bitmap-size (%record-font-data %record-offset)))
  "[BitmapSize](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#bitmapsize-record) record.")

(define-constant +bitmap-size-size+ 48)

(declaim
 (inline bitmap-size-index-subtable-list-offset
  bitmap-size-index-subtable-list-size bitmap-size-number-of-index-subtables
  bitmap-size-color-ref bitmap-size-hori bitmap-size-vert
  bitmap-size-start-glyph-index bitmap-size-end-glyph-index bitmap-size-ppem-x
  bitmap-size-ppem-y bitmap-size-bit-depth bitmap-size-flags))

(defun bitmap-size-index-subtable-list-offset (rec)
  "Offset to IndexSubtableList, from beginning of EBLC/CBLC."
  (declare (type bitmap-size rec))
  (font-data-read-u32 (record-ref-font-data rec) (record-ref-offset rec)))

(defun bitmap-size-index-subtable-list-size (rec)
  "Total size in bytes of the IndexSubtableList including its array of IndexSubtables."
  (declare (type bitmap-size rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun bitmap-size-number-of-index-subtables (rec)
  "Number of IndexSubtables in the IndexSubtableList."
  (declare (type bitmap-size rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defun bitmap-size-color-ref (rec)
  "Not used; set to 0."
  (declare (type bitmap-size rec))
  (font-data-read-u32 (record-ref-font-data rec)
                      (+ (record-ref-offset rec) 12)))

(defun bitmap-size-hori (rec)
  "Line metrics for text rendered horizontally."
  (declare (type bitmap-size rec))
  (%make-sbit-line-metrics (record-ref-font-data rec)
   (+ (record-ref-offset rec) 16)))

(defun bitmap-size-vert (rec)
  "Line metrics for text rendered vertically."
  (declare (type bitmap-size rec))
  (%make-sbit-line-metrics (record-ref-font-data rec)
   (+ (record-ref-offset rec) 28)))

(defun bitmap-size-start-glyph-index (rec)
  "Lowest glyph index for this size."
  (declare (type bitmap-size rec))
  (font-data-read-u16 (record-ref-font-data rec)
                      (+ (record-ref-offset rec) 40)))

(defun bitmap-size-end-glyph-index (rec)
  "Highest glyph index for this size."
  (declare (type bitmap-size rec))
  (font-data-read-u16 (record-ref-font-data rec)
                      (+ (record-ref-offset rec) 42)))

(defun bitmap-size-ppem-x (rec)
  "Horizontal pixels per em."
  (declare (type bitmap-size rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 44)))

(defun bitmap-size-ppem-y (rec)
  "Vertical pixels per em."
  (declare (type bitmap-size rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 45)))

(defun bitmap-size-bit-depth (rec)
  "The Microsoft rasterizer v.1.7 or greater supports the following bitDepth values, as described below: 1, 2, 4, and 8 (and 32 for CBLC)."
  (declare (type bitmap-size rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 46)))

(defun bitmap-size-flags (rec)
  "Vertical or horizontal."
  (declare (type bitmap-size rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 47)))

(defstruct
    (sbit-line-metrics (:include record-ref)
     (:constructor %make-sbit-line-metrics (%record-font-data %record-offset)))
  "[SbitLineMetrics](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#sbitlinemetrics-record) record.")

(define-constant +sbit-line-metrics-size+ 12)

(declaim
 (inline sbit-line-metrics-ascender sbit-line-metrics-descender
  sbit-line-metrics-width-max sbit-line-metrics-caret-slope-numerator
  sbit-line-metrics-caret-slope-denominator sbit-line-metrics-caret-offset
  sbit-line-metrics-min-origin-sb sbit-line-metrics-min-advance-sb
  sbit-line-metrics-max-before-bl sbit-line-metrics-min-after-bl
  sbit-line-metrics-pad1 sbit-line-metrics-pad2))

(defun sbit-line-metrics-ascender (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (record-ref-offset rec)))

(defun sbit-line-metrics-descender (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 1)))

(defun sbit-line-metrics-width-max (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun sbit-line-metrics-caret-slope-numerator (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 3)))

(defun sbit-line-metrics-caret-slope-denominator (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun sbit-line-metrics-caret-offset (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 5)))

(defun sbit-line-metrics-min-origin-sb (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 6)))

(defun sbit-line-metrics-min-advance-sb (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 7)))

(defun sbit-line-metrics-max-before-bl (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defun sbit-line-metrics-min-after-bl (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 9)))

(defun sbit-line-metrics-pad1 (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 10)))

(defun sbit-line-metrics-pad2 (rec)
  (declare (type sbit-line-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 11)))

(defstruct
    (big-glyph-metrics (:include record-ref)
     (:constructor %make-big-glyph-metrics (%record-font-data %record-offset)))
  "[BigGlyphMetrics](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#bigglyphmetrics) record.")

(define-constant +big-glyph-metrics-size+ 8)

(declaim
 (inline big-glyph-metrics-height big-glyph-metrics-width
  big-glyph-metrics-hori-bearing-x big-glyph-metrics-hori-bearing-y
  big-glyph-metrics-hori-advance big-glyph-metrics-vert-bearing-x
  big-glyph-metrics-vert-bearing-y big-glyph-metrics-vert-advance))

(defun big-glyph-metrics-height (rec)
  "Number of rows of data."
  (declare (type big-glyph-metrics rec))
  (font-data-read-u8 (record-ref-font-data rec) (record-ref-offset rec)))

(defun big-glyph-metrics-width (rec)
  "Number of columns of data."
  (declare (type big-glyph-metrics rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 1)))

(defun big-glyph-metrics-hori-bearing-x (rec)
  "Distance in pixels from the horizontal origin to the left edge of the bitmap."
  (declare (type big-glyph-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun big-glyph-metrics-hori-bearing-y (rec)
  "Distance in pixels from the horizontal origin to the top edge of the bitmap."
  (declare (type big-glyph-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 3)))

(defun big-glyph-metrics-hori-advance (rec)
  "Horizontal advance width in pixels."
  (declare (type big-glyph-metrics rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun big-glyph-metrics-vert-bearing-x (rec)
  "Distance in pixels from the vertical origin to the left edge of the bitmap."
  (declare (type big-glyph-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 5)))

(defun big-glyph-metrics-vert-bearing-y (rec)
  "Distance in pixels from the vertical origin to the top edge of the bitmap."
  (declare (type big-glyph-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 6)))

(defun big-glyph-metrics-vert-advance (rec)
  "Vertical advance width in pixels."
  (declare (type big-glyph-metrics rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 7)))

(defstruct
    (small-glyph-metrics (:include record-ref)
     (:constructor %make-small-glyph-metrics
      (%record-font-data %record-offset)))
  "[SmallGlyphMetrics](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#smallglyphmetrics) record.")

(define-constant +small-glyph-metrics-size+ 5)

(declaim
 (inline small-glyph-metrics-height small-glyph-metrics-width
  small-glyph-metrics-bearing-x small-glyph-metrics-bearing-y
  small-glyph-metrics-advance))

(defun small-glyph-metrics-height (rec)
  "Number of rows of data."
  (declare (type small-glyph-metrics rec))
  (font-data-read-u8 (record-ref-font-data rec) (record-ref-offset rec)))

(defun small-glyph-metrics-width (rec)
  "Number of columns of data."
  (declare (type small-glyph-metrics rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 1)))

(defun small-glyph-metrics-bearing-x (rec)
  "Distance in pixels from the horizontal origin to the left edge of the bitmap (for horizontal text); or distance in pixels from the vertical origin to the top edge of the bitmap (for vertical text)."
  (declare (type small-glyph-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun small-glyph-metrics-bearing-y (rec)
  "Distance in pixels from the horizontal origin to the top edge of the bitmap (for horizontal text); or distance in pixels from the vertical origin to the left edge of the bitmap (for vertical text)."
  (declare (type small-glyph-metrics rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 3)))

(defun small-glyph-metrics-advance (rec)
  "Horizontal or vertical advance width in pixels."
  (declare (type small-glyph-metrics rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defstruct
    (index-subtable-record (:include record-ref)
     (:constructor %make-index-subtable-record
      (%record-font-data %record-offset))))

(define-constant +index-subtable-record-size+ 8)

(declaim
 (inline index-subtable-record-first-glyph-index
  index-subtable-record-last-glyph-index
  index-subtable-record-index-subtable-offset))

(defun index-subtable-record-first-glyph-index (rec)
  "First glyph ID of this range."
  (declare (type index-subtable-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun index-subtable-record-last-glyph-index (rec)
  "Last glyph ID of this range (inclusive)."
  (declare (type index-subtable-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun index-subtable-record-index-subtable-offset (rec)
  "Offset to an IndexSubtable from the start of the IndexSubtableList."
  (declare (type index-subtable-record rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defstruct
    (glyph-id-offset-pair (:include record-ref)
     (:constructor %make-glyph-id-offset-pair
      (%record-font-data %record-offset)))
  "[GlyphIdOffsetPair](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#glyphidoffsetpair-record) record.")

(define-constant +glyph-id-offset-pair-size+ 4)

(declaim
 (inline glyph-id-offset-pair-glyph-id glyph-id-offset-pair-sbit-offset))

(defun glyph-id-offset-pair-glyph-id (rec)
  "Glyph ID of glyph present."
  (declare (type glyph-id-offset-pair rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun glyph-id-offset-pair-sbit-offset (rec)
  "Location in EBDT."
  (declare (type glyph-id-offset-pair rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (bdt-component (:include record-ref)
     (:constructor %make-bdt-component (%record-font-data %record-offset)))
  "[EbdtComponent](https://learn.microsoft.com/en-us/typography/opentype/spec/ebdt#ebdtcomponent-record) record.")

(define-constant +bdt-component-size+ 4)

(declaim
 (inline bdt-component-glyph-id bdt-component-x-offset bdt-component-y-offset))

(defun bdt-component-glyph-id (rec)
  "Component glyph ID."
  (declare (type bdt-component rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun bdt-component-x-offset (rec)
  "Position of component left."
  (declare (type bdt-component rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun bdt-component-y-offset (rec)
  "Position of component top."
  (declare (type bdt-component rec))
  (font-data-read-i8 (record-ref-font-data rec) (+ (record-ref-offset rec) 3)))

(defstruct
    (index-subtable-list (:include table-ref)
     (:constructor %make-index-subtable-list
      (%table-font-data %table-offset %table-length)))
  "[IndexSubtableList](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtablelist) table.")

(declaim
 (inline index-subtable-list-index-subtable-records-count
  index-subtable-list-index-subtable-records-ref))

(defun index-subtable-list-index-subtable-records-count
       (table number-of-index-subtables)
  "Array of IndexSubtableRecords."
  (declare (type index-subtable-list table)
           (ignorable number-of-index-subtables))
  (declare (ignorable table))
  number-of-index-subtables)

(defun index-subtable-list-index-subtable-records-ref
       (table number-of-index-subtables index)
  "Array of IndexSubtableRecords."
  (declare (type index-subtable-list table)
           (ignorable number-of-index-subtables index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 8))))
    (unless
        (<= (+ pos 8) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 8 :length
             (table-ref-length table)))
    (%make-index-subtable-record (table-ref-font-data table) pos)))

(defun sanitize-index-subtable-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-index-subtable-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-index-subtable-list font-data offset length)
  (%make-index-subtable-list font-data offset length))

(defstruct
    (index-subtable1 (:include table-ref)
     (:constructor %make-index-subtable1
      (%table-font-data %table-offset %table-length)))
  "[IndexSubTable1](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable1-variable-metrics-glyphs-with-4-byte-offsets): variable-metrics glyphs with 4-byte offsets.")

(declaim
 (inline index-subtable1-index-format index-subtable1-index-format-raw
  index-subtable1-image-format index-subtable1-image-format-raw
  index-subtable1-image-data-offset index-subtable1-image-data-offset-raw
  index-subtable1-sbit-offsets-count index-subtable1-sbit-offsets-ref))

(defun index-subtable1-index-format (table)
  "Format of this IndexSubTable."
  (declare (type index-subtable1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun index-subtable1-index-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun index-subtable1-image-format (table)
  "Format of EBDT image data."
  (declare (type index-subtable1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun index-subtable1-image-format-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun index-subtable1-image-data-offset (table)
  "Offset to image data in EBDT table."
  (declare (type index-subtable1 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun index-subtable1-image-data-offset-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun index-subtable1-sbit-offsets-count
       (table last-glyph-index first-glyph-index)
  (declare (type index-subtable1 table)
           (ignorable last-glyph-index first-glyph-index))
  (declare (ignorable table))
  (+ (max 0 (- last-glyph-index first-glyph-index)) 2))

(defun index-subtable1-sbit-offsets-ref
       (table last-glyph-index first-glyph-index index)
  (declare (type index-subtable1 table)
           (ignorable last-glyph-index first-glyph-index index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (font-data-read-u32 (table-ref-font-data table) pos)))

(defun sanitize-index-subtable1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-index-subtable1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-index-subtable1 font-data offset length)
  (%make-index-subtable1 font-data offset length))

(defstruct
    (index-subtable2 (:include table-ref)
     (:constructor %make-index-subtable2
      (%table-font-data %table-offset %table-length)))
  "[IndexSubTable2](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable2-all-glyphs-have-identical-metrics): all glyphs have identical metrics.")

(declaim
 (inline index-subtable2-index-format index-subtable2-index-format-raw
  index-subtable2-image-format index-subtable2-image-format-raw
  index-subtable2-image-data-offset index-subtable2-image-data-offset-raw
  index-subtable2-image-size index-subtable2-image-size-raw
  index-subtable2-big-metrics))

(defun index-subtable2-index-format (table)
  "Format of this IndexSubTable."
  (declare (type index-subtable2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun index-subtable2-index-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun index-subtable2-image-format (table)
  "Format of EBDT image data."
  (declare (type index-subtable2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun index-subtable2-image-format-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun index-subtable2-image-data-offset (table)
  "Offset to image data in EBDT table."
  (declare (type index-subtable2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun index-subtable2-image-data-offset-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun index-subtable2-image-size (table)
  "All the glyphs are of the same size."
  (declare (type index-subtable2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun index-subtable2-image-size-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun index-subtable2-big-metrics (table)
  "All glyphs have the same metrics; glyph data may be compressed, byte-aligned, or bit-aligned."
  (declare (type index-subtable2 table))
  (%make-big-glyph-metrics (table-ref-font-data table)
   (+ (table-ref-offset table) 12)))

(defun sanitize-index-subtable2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 20 length) (font-data-valid-range-p font-data offset 20))
    (error 'font-out-of-bounds-error :pos offset :size 20 :length
           (font-data-length font-data)))
  t)

(defun read-index-subtable2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-index-subtable2 font-data offset length)
  (%make-index-subtable2 font-data offset length))

(defstruct
    (index-subtable3 (:include table-ref)
     (:constructor %make-index-subtable3
      (%table-font-data %table-offset %table-length)))
  "[IndexSubTable3](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable3-variable-metrics-glyphs-with-2-byte-offsets): variable-metrics glyphs with 2-byte offsets.")

(declaim
 (inline index-subtable3-index-format index-subtable3-index-format-raw
  index-subtable3-image-format index-subtable3-image-format-raw
  index-subtable3-image-data-offset index-subtable3-image-data-offset-raw
  index-subtable3-sbit-offsets-count index-subtable3-sbit-offsets-ref))

(defun index-subtable3-index-format (table)
  "Format of this IndexSubTable."
  (declare (type index-subtable3 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun index-subtable3-index-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun index-subtable3-image-format (table)
  "Format of EBDT image data."
  (declare (type index-subtable3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun index-subtable3-image-format-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun index-subtable3-image-data-offset (table)
  "Offset to image data in EBDT table."
  (declare (type index-subtable3 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun index-subtable3-image-data-offset-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun index-subtable3-sbit-offsets-count
       (table last-glyph-index first-glyph-index)
  (declare (type index-subtable3 table)
           (ignorable last-glyph-index first-glyph-index))
  (declare (ignorable table))
  (+ (max 0 (- last-glyph-index first-glyph-index)) 2))

(defun index-subtable3-sbit-offsets-ref
       (table last-glyph-index first-glyph-index index)
  (declare (type index-subtable3 table)
           (ignorable last-glyph-index first-glyph-index index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun sanitize-index-subtable3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-index-subtable3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-index-subtable3 font-data offset length)
  (%make-index-subtable3 font-data offset length))

(defstruct
    (index-subtable4 (:include table-ref)
     (:constructor %make-index-subtable4
      (%table-font-data %table-offset %table-length)))
  "[IndexSubTable4](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable3-variable-metrics-glyphs-with-2-byte-offsets): variable-metrics glyphs with sparse glyph codes.")

(declaim
 (inline index-subtable4-index-format index-subtable4-index-format-raw
  index-subtable4-image-format index-subtable4-image-format-raw
  index-subtable4-image-data-offset index-subtable4-image-data-offset-raw
  index-subtable4-num-glyphs index-subtable4-num-glyphs-raw
  index-subtable4-glyph-array-count index-subtable4-glyph-array-ref))

(defun index-subtable4-index-format (table)
  "Format of this IndexSubTable."
  (declare (type index-subtable4 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun index-subtable4-index-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun index-subtable4-image-format (table)
  "Format of EBDT image data."
  (declare (type index-subtable4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun index-subtable4-image-format-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun index-subtable4-image-data-offset (table)
  "Offset to image data in EBDT table."
  (declare (type index-subtable4 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun index-subtable4-image-data-offset-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun index-subtable4-num-glyphs (table)
  "Array length."
  (declare (type index-subtable4 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun index-subtable4-num-glyphs-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun index-subtable4-glyph-array-count (table)
  "One per glyph."
  (declare (type index-subtable4 table))
  (declare (ignorable table))
  (+ (index-subtable4-num-glyphs table) 1))

(defun index-subtable4-glyph-array-ref (table index)
  "One per glyph."
  (declare (type index-subtable4 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-glyph-id-offset-pair (table-ref-font-data table) pos)))

(defun sanitize-index-subtable4 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-index-subtable4
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-index-subtable4 font-data offset length)
  (%make-index-subtable4 font-data offset length))

(defstruct
    (index-subtable5 (:include table-ref)
     (:constructor %make-index-subtable5
      (%table-font-data %table-offset %table-length)))
  "[IndexSubTable5](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable5-constant-metrics-glyphs-with-sparse-glyph-codes): constant-metrics glyphs with sparse glyph codes")

(declaim
 (inline index-subtable5-index-format index-subtable5-index-format-raw
  index-subtable5-image-format index-subtable5-image-format-raw
  index-subtable5-image-data-offset index-subtable5-image-data-offset-raw
  index-subtable5-image-size index-subtable5-image-size-raw
  index-subtable5-big-metrics index-subtable5-num-glyphs
  index-subtable5-num-glyphs-raw index-subtable5-glyph-array-count
  index-subtable5-glyph-array-ref))

(defun index-subtable5-index-format (table)
  "Format of this IndexSubTable."
  (declare (type index-subtable5 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun index-subtable5-index-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun index-subtable5-image-format (table)
  "Format of EBDT image data."
  (declare (type index-subtable5 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun index-subtable5-image-format-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun index-subtable5-image-data-offset (table)
  "Offset to image data in EBDT table."
  (declare (type index-subtable5 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun index-subtable5-image-data-offset-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun index-subtable5-image-size (table)
  "All glyphs have the same data size."
  (declare (type index-subtable5 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun index-subtable5-image-size-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun index-subtable5-big-metrics (table)
  "All glyphs have the same metrics."
  (declare (type index-subtable5 table))
  (%make-big-glyph-metrics (table-ref-font-data table)
   (+ (table-ref-offset table) 12)))

(defun index-subtable5-num-glyphs (table)
  "Array length."
  (declare (type index-subtable5 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 20)))

(defun index-subtable5-num-glyphs-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 20)))

(defun index-subtable5-glyph-array-count (table)
  "One per glyph, sorted by glyhph ID."
  (declare (type index-subtable5 table))
  (declare (ignorable table))
  (index-subtable5-num-glyphs table))

(defun index-subtable5-glyph-array-ref (table index)
  "One per glyph, sorted by glyhph ID."
  (declare (type index-subtable5 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 24) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun sanitize-index-subtable5 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 24 length) (font-data-valid-range-p font-data offset 24))
    (error 'font-out-of-bounds-error :pos offset :size 24 :length
           (font-data-length font-data)))
  (let* ((cnt (index-subtable5-num-glyphs-raw font-data offset))
         (pos (+ 24 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-index-subtable5
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-index-subtable5 font-data offset length)
  (%make-index-subtable5 font-data offset length))
