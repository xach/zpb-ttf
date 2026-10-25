;;;; math.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+glyph-part-record-size+ +math-glyph-variant-record-size+
   +math-kern-info-record-size+ +math-tag+ +math-value-record-size+
   +part-flags-extender-flag+ glyph-assembly glyph-assembly-italics-correction
   glyph-assembly-p glyph-assembly-part-count glyph-assembly-part-records-count
   glyph-assembly-part-records-ref glyph-part-record
   glyph-part-record-end-connector-length glyph-part-record-full-advance
   glyph-part-record-glyph-id glyph-part-record-p glyph-part-record-part-flags
   glyph-part-record-start-connector-length math math-constants
   math-constants-accent-base-height math-constants-axis-height
   math-constants-delimited-sub-formula-min-height
   math-constants-display-operator-min-height
   math-constants-flattened-accent-base-height
   math-constants-fraction-denom-display-style-gap-min
   math-constants-fraction-denominator-display-style-shift-down
   math-constants-fraction-denominator-gap-min
   math-constants-fraction-denominator-shift-down
   math-constants-fraction-num-display-style-gap-min
   math-constants-fraction-numerator-display-style-shift-up
   math-constants-fraction-numerator-gap-min
   math-constants-fraction-numerator-shift-up
   math-constants-fraction-rule-thickness
   math-constants-lower-limit-baseline-drop-min
   math-constants-lower-limit-gap-min math-constants-math-leading
   math-constants-overbar-extra-ascender math-constants-overbar-rule-thickness
   math-constants-overbar-vertical-gap math-constants-p
   math-constants-radical-degree-bottom-raise-percent
   math-constants-radical-display-style-vertical-gap
   math-constants-radical-extra-ascender
   math-constants-radical-kern-after-degree
   math-constants-radical-kern-before-degree
   math-constants-radical-rule-thickness math-constants-radical-vertical-gap
   math-constants-script-percent-scale-down
   math-constants-script-script-percent-scale-down
   math-constants-skewed-fraction-horizontal-gap
   math-constants-skewed-fraction-vertical-gap
   math-constants-space-after-script
   math-constants-stack-bottom-display-style-shift-down
   math-constants-stack-bottom-shift-down
   math-constants-stack-display-style-gap-min math-constants-stack-gap-min
   math-constants-stack-top-display-style-shift-up
   math-constants-stack-top-shift-up
   math-constants-stretch-stack-bottom-shift-down
   math-constants-stretch-stack-gap-above-min
   math-constants-stretch-stack-gap-below-min
   math-constants-stretch-stack-top-shift-up
   math-constants-sub-superscript-gap-min
   math-constants-subscript-baseline-drop-min
   math-constants-subscript-shift-down math-constants-subscript-top-max
   math-constants-superscript-baseline-drop-max
   math-constants-superscript-bottom-max-with-subscript
   math-constants-superscript-bottom-min math-constants-superscript-shift-up
   math-constants-superscript-shift-up-cramped
   math-constants-underbar-extra-descender
   math-constants-underbar-rule-thickness math-constants-underbar-vertical-gap
   math-constants-upper-limit-baseline-rise-min
   math-constants-upper-limit-gap-min math-glyph-construction
   math-glyph-construction-glyph-assembly-offset
   math-glyph-construction-glyph-assembly-offset-offset
   math-glyph-construction-math-glyph-variant-records-count
   math-glyph-construction-math-glyph-variant-records-ref
   math-glyph-construction-p math-glyph-construction-variant-count
   math-glyph-info math-glyph-info-extended-shape-coverage-offset
   math-glyph-info-extended-shape-coverage-offset-offset
   math-glyph-info-math-italics-correction-info-offset
   math-glyph-info-math-italics-correction-info-offset-offset
   math-glyph-info-math-kern-info-offset
   math-glyph-info-math-kern-info-offset-offset
   math-glyph-info-math-top-accent-attachment-offset
   math-glyph-info-math-top-accent-attachment-offset-offset math-glyph-info-p
   math-glyph-variant-record math-glyph-variant-record-advance-measurement
   math-glyph-variant-record-p math-glyph-variant-record-variant-glyph
   math-italics-correction-info math-italics-correction-info-coverage-offset
   math-italics-correction-info-coverage-offset-offset
   math-italics-correction-info-italics-correction-array-count
   math-italics-correction-info-italics-correction-count
   math-italics-correction-info-italics-correction-ref
   math-italics-correction-info-p math-kern math-kern-correction-height-count
   math-kern-correction-height-ref math-kern-height-count math-kern-info
   math-kern-info-math-kern-count math-kern-info-math-kern-coverage-offset
   math-kern-info-math-kern-coverage-offset-offset
   math-kern-info-math-kern-info-records-count
   math-kern-info-math-kern-info-records-ref math-kern-info-p
   math-kern-info-record math-kern-info-record-bottom-left-math-kern-offset
   math-kern-info-record-bottom-right-math-kern-offset math-kern-info-record-p
   math-kern-info-record-top-left-math-kern-offset
   math-kern-info-record-top-right-math-kern-offset math-kern-kern-values-count
   math-kern-kern-values-ref math-kern-p math-math-constants-offset
   math-math-constants-offset-offset math-math-glyph-info-offset
   math-math-glyph-info-offset-offset math-math-variants-offset
   math-math-variants-offset-offset math-p math-top-accent-attachment
   math-top-accent-attachment-p
   math-top-accent-attachment-top-accent-attachment-array-count
   math-top-accent-attachment-top-accent-attachment-count
   math-top-accent-attachment-top-accent-attachment-ref
   math-top-accent-attachment-top-accent-coverage-offset
   math-top-accent-attachment-top-accent-coverage-offset-offset
   math-value-record math-value-record-device-offset math-value-record-p
   math-value-record-value math-variants
   math-variants-horiz-glyph-construction-offsets-count
   math-variants-horiz-glyph-construction-offsets-ref
   math-variants-horiz-glyph-count math-variants-horiz-glyph-coverage-offset
   math-variants-horiz-glyph-coverage-offset-offset
   math-variants-min-connector-overlap math-variants-p
   math-variants-vert-glyph-construction-offsets-count
   math-variants-vert-glyph-construction-offsets-ref
   math-variants-vert-glyph-count math-variants-vert-glyph-coverage-offset
   math-variants-vert-glyph-coverage-offset-offset math-version
   part-flags-extender-flag-p read-glyph-assembly read-math read-math-constants
   read-math-glyph-construction read-math-glyph-info
   read-math-italics-correction-info read-math-kern read-math-kern-info
   read-math-top-accent-attachment read-math-variants sanitize-glyph-assembly
   sanitize-math sanitize-math-constants sanitize-math-glyph-construction
   sanitize-math-glyph-info sanitize-math-italics-correction-info
   sanitize-math-kern sanitize-math-kern-info
   sanitize-math-top-accent-attachment sanitize-math-variants))

(define-constant +part-flags-extender-flag+ 1
                 "Qualifiers for a [`GlyphPartRecord`].")

(declaim (inline part-flags-extender-flag-p))

(defun part-flags-extender-flag-p (flags)
  (logtest flags +part-flags-extender-flag+))

(defstruct
    (math-value-record (:include record-ref)
     (:constructor %make-math-value-record (%record-font-data %record-offset)))
  "[MathValueRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathvaluerecord)  The device offset is measured from the beginning of the table that contains the record, not from the record itself.")

(define-constant +math-value-record-size+ 4)

(declaim (inline math-value-record-value math-value-record-device-offset))

(defun math-value-record-value (rec)
  "The X or Y value in design units."
  (declare (type math-value-record rec))
  (font-data-read-fword (record-ref-font-data rec) (record-ref-offset rec)))

(defun math-value-record-device-offset (rec)
  "Offset to the Device table (non-variable font) / VariationIndex table (variable font) for this value, from the beginning of the parent table. May be NULL."
  (declare (type math-value-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (math-kern-info-record (:include record-ref)
     (:constructor %make-math-kern-info-record
      (%record-font-data %record-offset)))
  "[MathKernInfoRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathkerninforecord)  The offsets are measured from the beginning of the MathKernInfo table.")

(define-constant +math-kern-info-record-size+ 8)

(declaim
 (inline math-kern-info-record-top-right-math-kern-offset
  math-kern-info-record-top-left-math-kern-offset
  math-kern-info-record-bottom-right-math-kern-offset
  math-kern-info-record-bottom-left-math-kern-offset))

(defun math-kern-info-record-top-right-math-kern-offset (rec)
  "Offset to the MathKern table for the top right corner, from the beginning of the MathKernInfo table. May be NULL."
  (declare (type math-kern-info-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun math-kern-info-record-top-left-math-kern-offset (rec)
  "Offset to the MathKern table for the top left corner, from the beginning of the MathKernInfo table. May be NULL."
  (declare (type math-kern-info-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun math-kern-info-record-bottom-right-math-kern-offset (rec)
  "Offset to the MathKern table for the bottom right corner, from the beginning of the MathKernInfo table. May be NULL."
  (declare (type math-kern-info-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun math-kern-info-record-bottom-left-math-kern-offset (rec)
  "Offset to the MathKern table for the bottom left corner, from the beginning of the MathKernInfo table. May be NULL."
  (declare (type math-kern-info-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 6)))

(defstruct
    (math-glyph-variant-record (:include record-ref)
     (:constructor %make-math-glyph-variant-record
      (%record-font-data %record-offset)))
  "[MathGlyphVariantRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathglyphvariantrecord)")

(define-constant +math-glyph-variant-record-size+ 4)

(declaim
 (inline math-glyph-variant-record-variant-glyph
  math-glyph-variant-record-advance-measurement))

(defun math-glyph-variant-record-variant-glyph (rec)
  "Glyph ID for the variant."
  (declare (type math-glyph-variant-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun math-glyph-variant-record-advance-measurement (rec)
  "Advance width/height, in design units, of the variant, in the direction of requested glyph extension."
  (declare (type math-glyph-variant-record rec))
  (font-data-read-ufword (record-ref-font-data rec)
                         (+ (record-ref-offset rec) 2)))

(defstruct
    (glyph-part-record (:include record-ref)
     (:constructor %make-glyph-part-record (%record-font-data %record-offset)))
  "[GlyphPartRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/math#glyphpartrecord)")

(define-constant +glyph-part-record-size+ 10)

(declaim
 (inline glyph-part-record-glyph-id glyph-part-record-start-connector-length
  glyph-part-record-end-connector-length glyph-part-record-full-advance
  glyph-part-record-part-flags))

(defun glyph-part-record-glyph-id (rec)
  "Glyph ID for the part."
  (declare (type glyph-part-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun glyph-part-record-start-connector-length (rec)
  "Advance width/ height, in design units, of the straight bar connector material at the start of the glyph in the direction of the extension."
  (declare (type glyph-part-record rec))
  (font-data-read-ufword (record-ref-font-data rec)
                         (+ (record-ref-offset rec) 2)))

(defun glyph-part-record-end-connector-length (rec)
  "Advance width/ height, in design units, of the straight bar connector material at the end of the glyph in the direction of the extension."
  (declare (type glyph-part-record rec))
  (font-data-read-ufword (record-ref-font-data rec)
                         (+ (record-ref-offset rec) 4)))

(defun glyph-part-record-full-advance (rec)
  "Full advance width/height for this part in the direction of the extension, in design units."
  (declare (type glyph-part-record rec))
  (font-data-read-ufword (record-ref-font-data rec)
                         (+ (record-ref-offset rec) 6)))

(defun glyph-part-record-part-flags (rec)
  "Part qualifiers."
  (declare (type glyph-part-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defstruct
    (math (:include table-ref)
     (:constructor %make-math (%table-font-data %table-offset %table-length)))
  "The [MATH](https://learn.microsoft.com/en-us/typography/opentype/spec/math) table")

(declaim
 (inline math-version math-version-raw math-math-constants-offset-offset
  math-math-constants-offset math-math-glyph-info-offset-offset
  math-math-glyph-info-offset math-math-variants-offset-offset
  math-math-variants-offset))

(define-constant +math-tag+ "MATH")

(defun math-version (table)
  "(major, minor) Version for the MATH table (1,0)"
  (declare (type math table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun math-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun math-math-constants-offset-offset (table)
  "Offset to MathConstants table, from beginning of MATH table."
  (declare (type math table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun math-math-constants-offset (table)
  "Offset to MathConstants table, from beginning of MATH table."
  (declare (type math table))
  (let ((off (math-math-constants-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-math-constants (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-math-glyph-info-offset-offset (table)
  "Offset to MathGlyphInfo table, from beginning of MATH table."
  (declare (type math table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun math-math-glyph-info-offset (table)
  "Offset to MathGlyphInfo table, from beginning of MATH table."
  (declare (type math table))
  (let ((off (math-math-glyph-info-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-math-glyph-info (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-math-variants-offset-offset (table)
  "Offset to MathVariants table, from beginning of MATH table."
  (declare (type math table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun math-math-variants-offset (table)
  "Offset to MathVariants table, from beginning of MATH table."
  (declare (type math table))
  (let ((off (math-math-variants-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-math-variants (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-math (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  t)

(defun read-math
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-math font-data offset length)
  (%make-math font-data offset length))

(defstruct
    (math-constants (:include table-ref)
     (:constructor %make-math-constants
      (%table-font-data %table-offset %table-length)))
  "[MathConstants](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathconstants-table)")

(declaim
 (inline math-constants-script-percent-scale-down
  math-constants-script-percent-scale-down-raw
  math-constants-script-script-percent-scale-down
  math-constants-script-script-percent-scale-down-raw
  math-constants-delimited-sub-formula-min-height
  math-constants-delimited-sub-formula-min-height-raw
  math-constants-display-operator-min-height
  math-constants-display-operator-min-height-raw math-constants-math-leading
  math-constants-axis-height math-constants-accent-base-height
  math-constants-flattened-accent-base-height
  math-constants-subscript-shift-down math-constants-subscript-top-max
  math-constants-subscript-baseline-drop-min
  math-constants-superscript-shift-up
  math-constants-superscript-shift-up-cramped
  math-constants-superscript-bottom-min
  math-constants-superscript-baseline-drop-max
  math-constants-sub-superscript-gap-min
  math-constants-superscript-bottom-max-with-subscript
  math-constants-space-after-script math-constants-upper-limit-gap-min
  math-constants-upper-limit-baseline-rise-min
  math-constants-lower-limit-gap-min
  math-constants-lower-limit-baseline-drop-min
  math-constants-stack-top-shift-up
  math-constants-stack-top-display-style-shift-up
  math-constants-stack-bottom-shift-down
  math-constants-stack-bottom-display-style-shift-down
  math-constants-stack-gap-min math-constants-stack-display-style-gap-min
  math-constants-stretch-stack-top-shift-up
  math-constants-stretch-stack-bottom-shift-down
  math-constants-stretch-stack-gap-above-min
  math-constants-stretch-stack-gap-below-min
  math-constants-fraction-numerator-shift-up
  math-constants-fraction-numerator-display-style-shift-up
  math-constants-fraction-denominator-shift-down
  math-constants-fraction-denominator-display-style-shift-down
  math-constants-fraction-numerator-gap-min
  math-constants-fraction-num-display-style-gap-min
  math-constants-fraction-rule-thickness
  math-constants-fraction-denominator-gap-min
  math-constants-fraction-denom-display-style-gap-min
  math-constants-skewed-fraction-horizontal-gap
  math-constants-skewed-fraction-vertical-gap
  math-constants-overbar-vertical-gap math-constants-overbar-rule-thickness
  math-constants-overbar-extra-ascender math-constants-underbar-vertical-gap
  math-constants-underbar-rule-thickness
  math-constants-underbar-extra-descender math-constants-radical-vertical-gap
  math-constants-radical-display-style-vertical-gap
  math-constants-radical-rule-thickness math-constants-radical-extra-ascender
  math-constants-radical-kern-before-degree
  math-constants-radical-kern-after-degree
  math-constants-radical-degree-bottom-raise-percent
  math-constants-radical-degree-bottom-raise-percent-raw))

(defun math-constants-script-percent-scale-down (table)
  "Percentage of scaling down for level 1 superscripts and subscripts."
  (declare (type math-constants table))
  (font-data-read-i16 (table-ref-font-data table) (table-ref-offset table)))

(defun math-constants-script-percent-scale-down-raw (font-data offset)
  (font-data-read-i16 font-data offset))

(defun math-constants-script-script-percent-scale-down (table)
  "Percentage of scaling down for level 2 (scriptScript) superscripts and subscripts."
  (declare (type math-constants table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun math-constants-script-script-percent-scale-down-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun math-constants-delimited-sub-formula-min-height (table)
  "Minimum height required for a delimited expression (contained within parentheses, etc.) to be treated as a sub-formula."
  (declare (type math-constants table))
  (font-data-read-ufword (table-ref-font-data table)
                         (+ (table-ref-offset table) 4)))

(defun math-constants-delimited-sub-formula-min-height-raw (font-data offset)
  (font-data-read-ufword font-data (+ offset 4)))

(defun math-constants-display-operator-min-height (table)
  "Minimum height of n-ary operators (such as integral and summation) for formulas in display mode (that is, appearing as standalone page elements, not embedded inline within text)."
  (declare (type math-constants table))
  (font-data-read-ufword (table-ref-font-data table)
                         (+ (table-ref-offset table) 6)))

(defun math-constants-display-operator-min-height-raw (font-data offset)
  (font-data-read-ufword font-data (+ offset 6)))

(defun math-constants-math-leading (table)
  "White space to be left between math formulas to ensure proper line spacing."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 8)))

(defun math-constants-axis-height (table)
  "Axis height of the font."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 12)))

(defun math-constants-accent-base-height (table)
  "Maximum (ink) height of accent base that does not require raising the accents."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 16)))

(defun math-constants-flattened-accent-base-height (table)
  "Maximum (ink) height of accent base that does not require flattening the accents."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 20)))

(defun math-constants-subscript-shift-down (table)
  "The standard shift down applied to subscript elements."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 24)))

(defun math-constants-subscript-top-max (table)
  "Maximum allowed height of the (ink) top of subscripts that does not require moving subscripts further down."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 28)))

(defun math-constants-subscript-baseline-drop-min (table)
  "Minimum allowed drop of the baseline of subscripts relative to the (ink) bottom of the base."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 32)))

(defun math-constants-superscript-shift-up (table)
  "Standard shift up applied to superscript elements."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 36)))

(defun math-constants-superscript-shift-up-cramped (table)
  "Standard shift of superscripts relative to the base, in cramped style."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 40)))

(defun math-constants-superscript-bottom-min (table)
  "Minimum allowed height of the (ink) bottom of superscripts that does not require moving subscripts further up."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 44)))

(defun math-constants-superscript-baseline-drop-max (table)
  "Maximum allowed drop of the baseline of superscripts relative to the (ink) top of the base."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 48)))

(defun math-constants-sub-superscript-gap-min (table)
  "Minimum gap between the superscript and subscript ink."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 52)))

(defun math-constants-superscript-bottom-max-with-subscript (table)
  "The maximum level to which the (ink) bottom of superscript can be pushed to increase the gap between superscript and subscript, before subscript starts being moved down."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 56)))

(defun math-constants-space-after-script (table)
  "Extra white space to be added after each subscript and superscript."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 60)))

(defun math-constants-upper-limit-gap-min (table)
  "Minimum gap between the (ink) bottom of the upper limit, and the (ink) top of the base operator."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 64)))

(defun math-constants-upper-limit-baseline-rise-min (table)
  "Minimum distance between baseline of upper limit and (ink) top of the base operator."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 68)))

(defun math-constants-lower-limit-gap-min (table)
  "Minimum gap between (ink) top of the lower limit, and (ink) bottom of the base operator."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 72)))

(defun math-constants-lower-limit-baseline-drop-min (table)
  "Minimum distance between baseline of the lower limit and (ink) bottom of the base operator."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 76)))

(defun math-constants-stack-top-shift-up (table)
  "Standard shift up applied to the top element of a stack."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 80)))

(defun math-constants-stack-top-display-style-shift-up (table)
  "Standard shift up applied to the top element of a stack in display style."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 84)))

(defun math-constants-stack-bottom-shift-down (table)
  "Standard shift down applied to the bottom element of a stack."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 88)))

(defun math-constants-stack-bottom-display-style-shift-down (table)
  "Standard shift down applied to the bottom element of a stack in display style."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 92)))

(defun math-constants-stack-gap-min (table)
  "Minimum gap between (ink) bottom of the top element of a stack, and the (ink) top of the bottom element."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 96)))

(defun math-constants-stack-display-style-gap-min (table)
  "Minimum gap between (ink) bottom of the top element of a stack, and the (ink) top of the bottom element in display style."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 100)))

(defun math-constants-stretch-stack-top-shift-up (table)
  "Standard shift up applied to the top element of the stretch stack."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 104)))

(defun math-constants-stretch-stack-bottom-shift-down (table)
  "Standard shift down applied to the bottom element of the stretch stack."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 108)))

(defun math-constants-stretch-stack-gap-above-min (table)
  "Minimum gap between the ink of the stretched element, and the (ink) bottom of the element above."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 112)))

(defun math-constants-stretch-stack-gap-below-min (table)
  "Minimum gap between the ink of the stretched element, and the (ink) top of the element below."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 116)))

(defun math-constants-fraction-numerator-shift-up (table)
  "Standard shift up applied to the numerator."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 120)))

(defun math-constants-fraction-numerator-display-style-shift-up (table)
  "Standard shift up applied to the numerator in display style."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 124)))

(defun math-constants-fraction-denominator-shift-down (table)
  "Standard shift down applied to the denominator."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 128)))

(defun math-constants-fraction-denominator-display-style-shift-down (table)
  "Standard shift down applied to the denominator in display style."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 132)))

(defun math-constants-fraction-numerator-gap-min (table)
  "Minimum tolerated gap between the (ink) bottom of the numerator and the ink of the fraction bar."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 136)))

(defun math-constants-fraction-num-display-style-gap-min (table)
  "Minimum tolerated gap between the (ink) bottom of the numerator and the ink of the fraction bar in display style."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 140)))

(defun math-constants-fraction-rule-thickness (table)
  "Thickness of the fraction bar."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 144)))

(defun math-constants-fraction-denominator-gap-min (table)
  "Minimum tolerated gap between the ink of the fraction bar and the (ink) top of the denominator."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 148)))

(defun math-constants-fraction-denom-display-style-gap-min (table)
  "Minimum tolerated gap between the ink of the fraction bar and the (ink) top of the denominator in display style."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 152)))

(defun math-constants-skewed-fraction-horizontal-gap (table)
  "Horizontal distance between the top and bottom elements of a skewed fraction."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 156)))

(defun math-constants-skewed-fraction-vertical-gap (table)
  "Vertical distance between the ink of the top and bottom elements of a skewed fraction."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 160)))

(defun math-constants-overbar-vertical-gap (table)
  "Distance between the overbar and the (ink) top of he base."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 164)))

(defun math-constants-overbar-rule-thickness (table)
  "Thickness of overbar."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 168)))

(defun math-constants-overbar-extra-ascender (table)
  "Extra white space reserved above the overbar."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 172)))

(defun math-constants-underbar-vertical-gap (table)
  "Distance between underbar and (ink) bottom of the base."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 176)))

(defun math-constants-underbar-rule-thickness (table)
  "Thickness of underbar."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 180)))

(defun math-constants-underbar-extra-descender (table)
  "Extra white space reserved below the underbar."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 184)))

(defun math-constants-radical-vertical-gap (table)
  "Space between the (ink) top of the expression and the bar over it."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 188)))

(defun math-constants-radical-display-style-vertical-gap (table)
  "Space between the (ink) top of the expression and the bar over it in display style."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 192)))

(defun math-constants-radical-rule-thickness (table)
  "Thickness of the radical rule."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 196)))

(defun math-constants-radical-extra-ascender (table)
  "Extra white space reserved above the radical."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 200)))

(defun math-constants-radical-kern-before-degree (table)
  "Extra horizontal kern before the degree of a radical, if such is present."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 204)))

(defun math-constants-radical-kern-after-degree (table)
  "Negative kern after the degree of a radical, if such is present."
  (declare (type math-constants table))
  (%make-math-value-record (table-ref-font-data table)
   (+ (table-ref-offset table) 208)))

(defun math-constants-radical-degree-bottom-raise-percent (table)
  "Height of the bottom of the radical degree, if such is present, in proportion to the ascender of the radical sign."
  (declare (type math-constants table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 212)))

(defun math-constants-radical-degree-bottom-raise-percent-raw
       (font-data offset)
  (font-data-read-i16 font-data (+ offset 212)))

(defun sanitize-math-constants (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 214 length) (font-data-valid-range-p font-data offset 214))
    (error 'font-out-of-bounds-error :pos offset :size 214 :length
           (font-data-length font-data)))
  t)

(defun read-math-constants
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-math-constants font-data offset length)
  (%make-math-constants font-data offset length))

(defstruct
    (math-glyph-info (:include table-ref)
     (:constructor %make-math-glyph-info
      (%table-font-data %table-offset %table-length)))
  "[MathGlyphInfo](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathglyphinfo-table)")

(declaim
 (inline math-glyph-info-math-italics-correction-info-offset-offset
  math-glyph-info-math-italics-correction-info-offset
  math-glyph-info-math-top-accent-attachment-offset-offset
  math-glyph-info-math-top-accent-attachment-offset
  math-glyph-info-extended-shape-coverage-offset-offset
  math-glyph-info-extended-shape-coverage-offset
  math-glyph-info-math-kern-info-offset-offset
  math-glyph-info-math-kern-info-offset))

(defun math-glyph-info-math-italics-correction-info-offset-offset (table)
  "Offset to MathItalicsCorrectionInfo table, from the beginning of the MathGlyphInfo table. May be NULL."
  (declare (type math-glyph-info table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun math-glyph-info-math-italics-correction-info-offset (table)
  "Offset to MathItalicsCorrectionInfo table, from the beginning of the MathGlyphInfo table. May be NULL."
  (declare (type math-glyph-info table))
  (let ((off
         (math-glyph-info-math-italics-correction-info-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-math-italics-correction-info (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-glyph-info-math-top-accent-attachment-offset-offset (table)
  "Offset to MathTopAccentAttachment table, from the beginning of the MathGlyphInfo table. May be NULL."
  (declare (type math-glyph-info table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun math-glyph-info-math-top-accent-attachment-offset (table)
  "Offset to MathTopAccentAttachment table, from the beginning of the MathGlyphInfo table. May be NULL."
  (declare (type math-glyph-info table))
  (let ((off (math-glyph-info-math-top-accent-attachment-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-math-top-accent-attachment (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-glyph-info-extended-shape-coverage-offset-offset (table)
  "Offset to the Coverage table for glyphs with extended shapes, from the beginning of the MathGlyphInfo table. May be NULL."
  (declare (type math-glyph-info table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun math-glyph-info-extended-shape-coverage-offset (table)
  "Offset to the Coverage table for glyphs with extended shapes, from the beginning of the MathGlyphInfo table. May be NULL."
  (declare (type math-glyph-info table))
  (let ((off (math-glyph-info-extended-shape-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-glyph-info-math-kern-info-offset-offset (table)
  "Offset to the MathKernInfo table, from the beginning of the MathGlyphInfo table. May be NULL."
  (declare (type math-glyph-info table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun math-glyph-info-math-kern-info-offset (table)
  "Offset to the MathKernInfo table, from the beginning of the MathGlyphInfo table. May be NULL."
  (declare (type math-glyph-info table))
  (let ((off (math-glyph-info-math-kern-info-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-math-kern-info (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-math-glyph-info (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-math-glyph-info
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-math-glyph-info font-data offset length)
  (%make-math-glyph-info font-data offset length))

(defstruct
    (math-italics-correction-info (:include table-ref)
     (:constructor %make-math-italics-correction-info
      (%table-font-data %table-offset %table-length)))
  "[MathItalicsCorrectionInfo](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathitalicscorrectioninfo-table)")

(declaim
 (inline math-italics-correction-info-coverage-offset-offset
  math-italics-correction-info-coverage-offset
  math-italics-correction-info-italics-correction-count
  math-italics-correction-info-italics-correction-count-raw
  math-italics-correction-info-italics-correction-array-count
  math-italics-correction-info-italics-correction-ref))

(defun math-italics-correction-info-coverage-offset-offset (table)
  "Offset to the Coverage table, from the beginning of the MathItalicsCorrectionInfo table."
  (declare (type math-italics-correction-info table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun math-italics-correction-info-coverage-offset (table)
  "Offset to the Coverage table, from the beginning of the MathItalicsCorrectionInfo table."
  (declare (type math-italics-correction-info table))
  (let ((off (math-italics-correction-info-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-italics-correction-info-italics-correction-count (table)
  "Number of italics correction values. Should coincide with the number of covered glyphs."
  (declare (type math-italics-correction-info table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun math-italics-correction-info-italics-correction-count-raw
       (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun math-italics-correction-info-italics-correction-array-count (table)
  "Array of MathValueRecords defining italics correction values for each covered glyph."
  (declare (type math-italics-correction-info table))
  (declare (ignorable table))
  (math-italics-correction-info-italics-correction-count table))

(defun math-italics-correction-info-italics-correction-ref (table index)
  "Array of MathValueRecords defining italics correction values for each covered glyph."
  (declare (type math-italics-correction-info table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-math-value-record (table-ref-font-data table) pos)))

(defun sanitize-math-italics-correction-info (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt
          (math-italics-correction-info-italics-correction-count-raw font-data
           offset))
         (pos (+ 4 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-math-italics-correction-info
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-math-italics-correction-info font-data offset length)
  (%make-math-italics-correction-info font-data offset length))

(defstruct
    (math-top-accent-attachment (:include table-ref)
     (:constructor %make-math-top-accent-attachment
      (%table-font-data %table-offset %table-length)))
  "[MathTopAccentAttachment](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathtopaccentattachment-table)")

(declaim
 (inline math-top-accent-attachment-top-accent-coverage-offset-offset
  math-top-accent-attachment-top-accent-coverage-offset
  math-top-accent-attachment-top-accent-attachment-count
  math-top-accent-attachment-top-accent-attachment-count-raw
  math-top-accent-attachment-top-accent-attachment-array-count
  math-top-accent-attachment-top-accent-attachment-ref))

(defun math-top-accent-attachment-top-accent-coverage-offset-offset (table)
  "Offset to the Coverage table, from the beginning of the MathTopAccentAttachment table."
  (declare (type math-top-accent-attachment table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun math-top-accent-attachment-top-accent-coverage-offset (table)
  "Offset to the Coverage table, from the beginning of the MathTopAccentAttachment table."
  (declare (type math-top-accent-attachment table))
  (let ((off
         (math-top-accent-attachment-top-accent-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-top-accent-attachment-top-accent-attachment-count (table)
  "Number of top accent attachment point values. Should coincide with the number of covered glyphs."
  (declare (type math-top-accent-attachment table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun math-top-accent-attachment-top-accent-attachment-count-raw
       (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun math-top-accent-attachment-top-accent-attachment-array-count (table)
  "Array of MathValueRecords defining top accent attachment points for each covered glyph."
  (declare (type math-top-accent-attachment table))
  (declare (ignorable table))
  (math-top-accent-attachment-top-accent-attachment-count table))

(defun math-top-accent-attachment-top-accent-attachment-ref (table index)
  "Array of MathValueRecords defining top accent attachment points for each covered glyph."
  (declare (type math-top-accent-attachment table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-math-value-record (table-ref-font-data table) pos)))

(defun sanitize-math-top-accent-attachment (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt
          (math-top-accent-attachment-top-accent-attachment-count-raw font-data
           offset))
         (pos (+ 4 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-math-top-accent-attachment
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-math-top-accent-attachment font-data offset length)
  (%make-math-top-accent-attachment font-data offset length))

(defstruct
    (math-kern-info (:include table-ref)
     (:constructor %make-math-kern-info
      (%table-font-data %table-offset %table-length)))
  "[MathKernInfo](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathkerninfo-table)")

(declaim
 (inline math-kern-info-math-kern-coverage-offset-offset
  math-kern-info-math-kern-coverage-offset math-kern-info-math-kern-count
  math-kern-info-math-kern-count-raw
  math-kern-info-math-kern-info-records-count
  math-kern-info-math-kern-info-records-ref))

(defun math-kern-info-math-kern-coverage-offset-offset (table)
  "Offset to the Coverage table, from the beginning of the MathKernInfo table."
  (declare (type math-kern-info table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun math-kern-info-math-kern-coverage-offset (table)
  "Offset to the Coverage table, from the beginning of the MathKernInfo table."
  (declare (type math-kern-info table))
  (let ((off (math-kern-info-math-kern-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-kern-info-math-kern-count (table)
  "Number of MathKernInfoRecords. Should coincide with the number of covered glyphs."
  (declare (type math-kern-info table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun math-kern-info-math-kern-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun math-kern-info-math-kern-info-records-count (table)
  "Array of MathKernInfoRecords, one for each covered glyph."
  (declare (type math-kern-info table))
  (declare (ignorable table))
  (math-kern-info-math-kern-count table))

(defun math-kern-info-math-kern-info-records-ref (table index)
  "Array of MathKernInfoRecords, one for each covered glyph."
  (declare (type math-kern-info table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 8))))
    (unless
        (<= (+ pos 8) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 8 :length
             (table-ref-length table)))
    (%make-math-kern-info-record (table-ref-font-data table) pos)))

(defun sanitize-math-kern-info (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (math-kern-info-math-kern-count-raw font-data offset))
         (pos (+ 4 (* cnt 8))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-math-kern-info
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-math-kern-info font-data offset length)
  (%make-math-kern-info font-data offset length))

(defstruct
    (math-kern (:include table-ref)
     (:constructor %make-math-kern
      (%table-font-data %table-offset %table-length)))
  "[MathKern](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathkern-table)")

(declaim
 (inline math-kern-height-count math-kern-height-count-raw
  math-kern-correction-height-count math-kern-correction-height-ref
  math-kern-kern-values-count math-kern-kern-values-ref))

(defun math-kern-height-count (table)
  "Number of heights at which the kern value changes."
  (declare (type math-kern table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun math-kern-height-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun math-kern-correction-height-count (table)
  "Array of correction heights, in design units, sorted from lowest to highest."
  (declare (type math-kern table))
  (declare (ignorable table))
  (math-kern-height-count table))

(defun math-kern-correction-height-ref (table index)
  "Array of correction heights, in design units, sorted from lowest to highest."
  (declare (type math-kern table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-math-value-record (table-ref-font-data table) pos)))

(defun math-kern-kern-values-count (table)
  "Array of kerning values for different height ranges, in design units.  There is one more of these than there are correction heights: the first applies below the first correction height, and the last above the last."
  (declare (type math-kern table))
  (declare (ignorable table))
  (+ (math-kern-height-count table) 1))

(defun math-kern-kern-values-ref (table index)
  "Array of kerning values for different height ranges, in design units.  There is one more of these than there are correction heights: the first applies below the first correction height, and the last above the last."
  (declare (type math-kern table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 2
             (* (math-kern-correction-height-count table) 4))
          (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-math-value-record (table-ref-font-data table) pos)))

(defun sanitize-math-kern (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  (let* ((cnt (math-kern-height-count-raw font-data offset))
         (pos (+ 2 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-math-kern
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-math-kern font-data offset length)
  (%make-math-kern font-data offset length))

(defstruct
    (math-variants (:include table-ref)
     (:constructor %make-math-variants
      (%table-font-data %table-offset %table-length)))
  "[MathVariants](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathvariants-table)")

(declaim
 (inline math-variants-min-connector-overlap
  math-variants-min-connector-overlap-raw
  math-variants-vert-glyph-coverage-offset-offset
  math-variants-vert-glyph-coverage-offset
  math-variants-horiz-glyph-coverage-offset-offset
  math-variants-horiz-glyph-coverage-offset math-variants-vert-glyph-count
  math-variants-vert-glyph-count-raw math-variants-horiz-glyph-count
  math-variants-horiz-glyph-count-raw
  math-variants-vert-glyph-construction-offsets-count
  math-variants-vert-glyph-construction-offsets-ref
  math-variants-horiz-glyph-construction-offsets-count
  math-variants-horiz-glyph-construction-offsets-ref))

(defun math-variants-min-connector-overlap (table)
  "Minimum overlap of connecting glyphs during glyph construction, in design units."
  (declare (type math-variants table))
  (font-data-read-ufword (table-ref-font-data table) (table-ref-offset table)))

(defun math-variants-min-connector-overlap-raw (font-data offset)
  (font-data-read-ufword font-data offset))

(defun math-variants-vert-glyph-coverage-offset-offset (table)
  "Offset to the Coverage table for shapes growing in the vertical direction, from the beginning of the MathVariants table. May be NULL."
  (declare (type math-variants table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun math-variants-vert-glyph-coverage-offset (table)
  "Offset to the Coverage table for shapes growing in the vertical direction, from the beginning of the MathVariants table. May be NULL."
  (declare (type math-variants table))
  (let ((off (math-variants-vert-glyph-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-variants-horiz-glyph-coverage-offset-offset (table)
  "Offset to the Coverage table for shapes growing in the horizontal direction, from the beginning of the MathVariants table. May be NULL."
  (declare (type math-variants table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun math-variants-horiz-glyph-coverage-offset (table)
  "Offset to the Coverage table for shapes growing in the horizontal direction, from the beginning of the MathVariants table. May be NULL."
  (declare (type math-variants table))
  (let ((off (math-variants-horiz-glyph-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-variants-vert-glyph-count (table)
  "Number of glyphs for which information is provided for vertically growing variants. Must be the same as the number of glyph IDs referenced in the vertical Coverage table."
  (declare (type math-variants table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun math-variants-vert-glyph-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun math-variants-horiz-glyph-count (table)
  "Number of glyphs for which information is provided for horizontally growing variants. Must be the same as the number of glyph IDs referenced in the horizontal Coverage table."
  (declare (type math-variants table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun math-variants-horiz-glyph-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun math-variants-vert-glyph-construction-offsets-count (table)
  "Array of offsets to MathGlyphConstruction tables, from the beginning of the MathVariants table, for shapes growing in the vertical direction."
  (declare (type math-variants table))
  (declare (ignorable table))
  (math-variants-vert-glyph-count table))

(defun math-variants-vert-glyph-construction-offsets-ref (table index)
  "Array of offsets to MathGlyphConstruction tables, from the beginning of the MathVariants table, for shapes growing in the vertical direction."
  (declare (type math-variants table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 10) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u16 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-math-glyph-construction (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun math-variants-horiz-glyph-construction-offsets-count (table)
  "Array of offsets to MathGlyphConstruction tables, from the beginning of the MathVariants table, for shapes growing in the horizontal direction."
  (declare (type math-variants table))
  (declare (ignorable table))
  (math-variants-horiz-glyph-count table))

(defun math-variants-horiz-glyph-construction-offsets-ref (table index)
  "Array of offsets to MathGlyphConstruction tables, from the beginning of the MathVariants table, for shapes growing in the horizontal direction."
  (declare (type math-variants table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 10
             (* (math-variants-vert-glyph-construction-offsets-count table) 2))
          (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u16 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-math-glyph-construction (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-math-variants (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  (let* ((cnt (math-variants-vert-glyph-count-raw font-data offset))
         (pos (+ 10 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-math-variants
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-math-variants font-data offset length)
  (%make-math-variants font-data offset length))

(defstruct
    (math-glyph-construction (:include table-ref)
     (:constructor %make-math-glyph-construction
      (%table-font-data %table-offset %table-length)))
  "[MathGlyphConstruction](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathglyphconstruction-table)")

(declaim
 (inline math-glyph-construction-glyph-assembly-offset-offset
  math-glyph-construction-glyph-assembly-offset
  math-glyph-construction-variant-count
  math-glyph-construction-variant-count-raw
  math-glyph-construction-math-glyph-variant-records-count
  math-glyph-construction-math-glyph-variant-records-ref))

(defun math-glyph-construction-glyph-assembly-offset-offset (table)
  "Offset to the GlyphAssembly table for this shape, from the beginning of the MathGlyphConstruction table. May be NULL."
  (declare (type math-glyph-construction table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun math-glyph-construction-glyph-assembly-offset (table)
  "Offset to the GlyphAssembly table for this shape, from the beginning of the MathGlyphConstruction table. May be NULL."
  (declare (type math-glyph-construction table))
  (let ((off (math-glyph-construction-glyph-assembly-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-glyph-assembly (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun math-glyph-construction-variant-count (table)
  "Count of glyph growing variants for this glyph."
  (declare (type math-glyph-construction table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun math-glyph-construction-variant-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun math-glyph-construction-math-glyph-variant-records-count (table)
  "MathGlyphVariantRecords for alternative variants of the base glyph."
  (declare (type math-glyph-construction table))
  (declare (ignorable table))
  (math-glyph-construction-variant-count table))

(defun math-glyph-construction-math-glyph-variant-records-ref (table index)
  "MathGlyphVariantRecords for alternative variants of the base glyph."
  (declare (type math-glyph-construction table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-math-glyph-variant-record (table-ref-font-data table) pos)))

(defun sanitize-math-glyph-construction (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (math-glyph-construction-variant-count-raw font-data offset))
         (pos (+ 4 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-math-glyph-construction
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-math-glyph-construction font-data offset length)
  (%make-math-glyph-construction font-data offset length))

(defstruct
    (glyph-assembly (:include table-ref)
     (:constructor %make-glyph-assembly
      (%table-font-data %table-offset %table-length)))
  "[GlyphAssembly](https://learn.microsoft.com/en-us/typography/opentype/spec/math#glyphassembly-table)")

(declaim
 (inline glyph-assembly-italics-correction glyph-assembly-part-count
  glyph-assembly-part-count-raw glyph-assembly-part-records-count
  glyph-assembly-part-records-ref))

(defun glyph-assembly-italics-correction (table)
  "Italics correction of this GlyphAssembly. Should not depend on the assembly size."
  (declare (type glyph-assembly table))
  (%make-math-value-record (table-ref-font-data table)
   (table-ref-offset table)))

(defun glyph-assembly-part-count (table)
  "Number of parts in this assembly."
  (declare (type glyph-assembly table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun glyph-assembly-part-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun glyph-assembly-part-records-count (table)
  "Array of part records, from left to right (for assemblies that extend horizontally) or bottom to top (for assemblies that extend vertically)."
  (declare (type glyph-assembly table))
  (declare (ignorable table))
  (glyph-assembly-part-count table))

(defun glyph-assembly-part-records-ref (table index)
  "Array of part records, from left to right (for assemblies that extend horizontally) or bottom to top (for assemblies that extend vertically)."
  (declare (type glyph-assembly table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 6) (* index 10))))
    (unless
        (<= (+ pos 10) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 10 :length
             (table-ref-length table)))
    (%make-glyph-part-record (table-ref-font-data table) pos)))

(defun sanitize-glyph-assembly (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  (let* ((cnt (glyph-assembly-part-count-raw font-data offset))
         (pos (+ 6 (* cnt 10))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-glyph-assembly
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-glyph-assembly font-data offset length)
  (%make-glyph-assembly font-data offset length))
