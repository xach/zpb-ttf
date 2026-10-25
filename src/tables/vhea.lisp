;;;; vhea.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+vhea-extended-tag+ +vhea-tag+ read-vhea read-vhea-extended sanitize-vhea
   sanitize-vhea-extended vhea vhea-advance-height-max vhea-ascender
   vhea-caret-offset vhea-caret-slope-rise vhea-caret-slope-run vhea-descender
   vhea-extended vhea-extended-advance-height-max vhea-extended-ascender
   vhea-extended-caret-offset vhea-extended-caret-slope-rise
   vhea-extended-caret-slope-run vhea-extended-descender vhea-extended-line-gap
   vhea-extended-metric-data-format vhea-extended-min-bottom-side-bearing
   vhea-extended-min-top-side-bearing vhea-extended-number-of-long-ver-metrics
   vhea-extended-p vhea-extended-reserved1 vhea-extended-reserved2
   vhea-extended-reserved3 vhea-extended-reserved4 vhea-extended-version
   vhea-extended-y-max-extent vhea-line-gap vhea-metric-data-format
   vhea-min-bottom-side-bearing vhea-min-top-side-bearing
   vhea-number-of-long-ver-metrics vhea-p vhea-reserved1 vhea-reserved2
   vhea-reserved3 vhea-reserved4 vhea-version vhea-y-max-extent))

(defstruct
    (vhea (:include table-ref)
     (:constructor %make-vhea (%table-font-data %table-offset %table-length)))
  "The [vhea](https://docs.microsoft.com/en-us/typography/opentype/spec/vhea) Vertical Header Table")

(declaim
 (inline vhea-version vhea-version-raw vhea-ascender vhea-ascender-raw
  vhea-descender vhea-descender-raw vhea-line-gap vhea-line-gap-raw
  vhea-advance-height-max vhea-advance-height-max-raw vhea-min-top-side-bearing
  vhea-min-top-side-bearing-raw vhea-min-bottom-side-bearing
  vhea-min-bottom-side-bearing-raw vhea-y-max-extent vhea-y-max-extent-raw
  vhea-caret-slope-rise vhea-caret-slope-rise-raw vhea-caret-slope-run
  vhea-caret-slope-run-raw vhea-caret-offset vhea-caret-offset-raw
  vhea-reserved1 vhea-reserved1-raw vhea-reserved2 vhea-reserved2-raw
  vhea-reserved3 vhea-reserved3-raw vhea-reserved4 vhea-reserved4-raw
  vhea-metric-data-format vhea-metric-data-format-raw
  vhea-number-of-long-ver-metrics vhea-number-of-long-ver-metrics-raw))

(define-constant +vhea-tag+ "vhea")

(defun vhea-version (table)
  "The major/minor version (1, 0 or 1, 1)"
  (declare (type vhea table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun vhea-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun vhea-ascender (table)
  "Typographic ascent."
  (declare (type vhea table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun vhea-ascender-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun vhea-descender (table)
  "Typographic descent."
  (declare (type vhea table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun vhea-descender-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun vhea-line-gap (table)
  "Typographic line gap. Negative LineGap values are treated as zero in some legacy platform implementations."
  (declare (type vhea table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun vhea-line-gap-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun vhea-advance-height-max (table)
  "Maximum advance height value in 'vmtx' table."
  (declare (type vhea table))
  (font-data-read-ufword (table-ref-font-data table)
                         (+ (table-ref-offset table) 10)))

(defun vhea-advance-height-max-raw (font-data offset)
  (font-data-read-ufword font-data (+ offset 10)))

(defun vhea-min-top-side-bearing (table)
  "Minimum top sidebearing value in 'vmtx' table for glyphs with contours (empty glyphs should be ignored)."
  (declare (type vhea table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun vhea-min-top-side-bearing-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 12)))

(defun vhea-min-bottom-side-bearing (table)
  "Minimum bottom sidebearing value"
  (declare (type vhea table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 14)))

(defun vhea-min-bottom-side-bearing-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 14)))

(defun vhea-y-max-extent (table)
  "Defined as max( tsb + (yMax-yMin))."
  (declare (type vhea table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 16)))

(defun vhea-y-max-extent-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 16)))

(defun vhea-caret-slope-rise (table)
  "Used to calculate the slope of the cursor (rise/run); 1 for vertical caret, 0 for horizontal."
  (declare (type vhea table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 18)))

(defun vhea-caret-slope-rise-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 18)))

(defun vhea-caret-slope-run (table)
  "0 for vertical caret, 1 for horizontal."
  (declare (type vhea table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 20)))

(defun vhea-caret-slope-run-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 20)))

(defun vhea-caret-offset (table)
  "The amount by which a slanted highlight on a glyph needs to be shifted to produce the best appearance. Set to 0 for non-slanted fonts"
  (declare (type vhea table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 22)))

(defun vhea-caret-offset-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 22)))

(defun vhea-reserved1 (table)
  "set to 0"
  (declare (type vhea table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 24)))

(defun vhea-reserved1-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 24)))

(defun vhea-reserved2 (table)
  "set to 0"
  (declare (type vhea table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 26)))

(defun vhea-reserved2-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 26)))

(defun vhea-reserved3 (table)
  "set to 0"
  (declare (type vhea table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 28)))

(defun vhea-reserved3-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 28)))

(defun vhea-reserved4 (table)
  "set to 0"
  (declare (type vhea table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 30)))

(defun vhea-reserved4-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 30)))

(defun vhea-metric-data-format (table)
  "0 for current format."
  (declare (type vhea table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 32)))

(defun vhea-metric-data-format-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 32)))

(defun vhea-number-of-long-ver-metrics (table)
  "Number of advance heights in the vertical metrics (`vmtx`) table."
  (declare (type vhea table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 34)))

(defun vhea-number-of-long-ver-metrics-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 34)))

(defun sanitize-vhea (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 36 length) (font-data-valid-range-p font-data offset 36))
    (error 'font-out-of-bounds-error :pos offset :size 36 :length
           (font-data-length font-data)))
  t)

(defun read-vhea
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-vhea font-data offset length)
  (%make-vhea font-data offset length))

(defstruct
    (vhea-extended (:include table-ref)
     (:constructor %make-vhea-extended
      (%table-font-data %table-offset %table-length)))
  "VHEA header table (ISO/IEC 14496-22:2026, 5.6.13).")

(declaim
 (inline vhea-extended-version vhea-extended-version-raw vhea-extended-ascender
  vhea-extended-ascender-raw vhea-extended-descender
  vhea-extended-descender-raw vhea-extended-line-gap vhea-extended-line-gap-raw
  vhea-extended-advance-height-max vhea-extended-advance-height-max-raw
  vhea-extended-min-top-side-bearing vhea-extended-min-top-side-bearing-raw
  vhea-extended-min-bottom-side-bearing
  vhea-extended-min-bottom-side-bearing-raw vhea-extended-y-max-extent
  vhea-extended-y-max-extent-raw vhea-extended-caret-slope-rise
  vhea-extended-caret-slope-rise-raw vhea-extended-caret-slope-run
  vhea-extended-caret-slope-run-raw vhea-extended-caret-offset
  vhea-extended-caret-offset-raw vhea-extended-reserved1
  vhea-extended-reserved1-raw vhea-extended-reserved2
  vhea-extended-reserved2-raw vhea-extended-reserved3
  vhea-extended-reserved3-raw vhea-extended-reserved4
  vhea-extended-reserved4-raw vhea-extended-metric-data-format
  vhea-extended-metric-data-format-raw vhea-extended-number-of-long-ver-metrics
  vhea-extended-number-of-long-ver-metrics-raw))

(define-constant +vhea-extended-tag+ "VHEA")

(defun vhea-extended-version (table)
  (declare (type vhea-extended table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun vhea-extended-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun vhea-extended-ascender (table)
  (declare (type vhea-extended table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun vhea-extended-ascender-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun vhea-extended-descender (table)
  (declare (type vhea-extended table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun vhea-extended-descender-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun vhea-extended-line-gap (table)
  (declare (type vhea-extended table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun vhea-extended-line-gap-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun vhea-extended-advance-height-max (table)
  (declare (type vhea-extended table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun vhea-extended-advance-height-max-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun vhea-extended-min-top-side-bearing (table)
  (declare (type vhea-extended table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun vhea-extended-min-top-side-bearing-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 12)))

(defun vhea-extended-min-bottom-side-bearing (table)
  (declare (type vhea-extended table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 14)))

(defun vhea-extended-min-bottom-side-bearing-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 14)))

(defun vhea-extended-y-max-extent (table)
  (declare (type vhea-extended table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 16)))

(defun vhea-extended-y-max-extent-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 16)))

(defun vhea-extended-caret-slope-rise (table)
  (declare (type vhea-extended table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 18)))

(defun vhea-extended-caret-slope-rise-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 18)))

(defun vhea-extended-caret-slope-run (table)
  (declare (type vhea-extended table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 20)))

(defun vhea-extended-caret-slope-run-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 20)))

(defun vhea-extended-caret-offset (table)
  (declare (type vhea-extended table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 22)))

(defun vhea-extended-caret-offset-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 22)))

(defun vhea-extended-reserved1 (table)
  (declare (type vhea-extended table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 24)))

(defun vhea-extended-reserved1-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 24)))

(defun vhea-extended-reserved2 (table)
  (declare (type vhea-extended table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 26)))

(defun vhea-extended-reserved2-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 26)))

(defun vhea-extended-reserved3 (table)
  (declare (type vhea-extended table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 28)))

(defun vhea-extended-reserved3-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 28)))

(defun vhea-extended-reserved4 (table)
  (declare (type vhea-extended table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 30)))

(defun vhea-extended-reserved4-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 30)))

(defun vhea-extended-metric-data-format (table)
  (declare (type vhea-extended table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 32)))

(defun vhea-extended-metric-data-format-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 32)))

(defun vhea-extended-number-of-long-ver-metrics (table)
  "Number of long metric records in VMTX."
  (declare (type vhea-extended table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 34)))

(defun vhea-extended-number-of-long-ver-metrics-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 34)))

(defun sanitize-vhea-extended (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 38 length) (font-data-valid-range-p font-data offset 38))
    (error 'font-out-of-bounds-error :pos offset :size 38 :length
           (font-data-length font-data)))
  t)

(defun read-vhea-extended
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-vhea-extended font-data offset length)
  (%make-vhea-extended font-data offset length))
