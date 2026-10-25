;;;; head.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+flags-baseline-at-y-0+ +flags-converted-font+ +flags-force-integer-ppem+
   +flags-instructions-may-alter-advance-width+
   +flags-instructions-may-depend-on-point-size+ +flags-last-resort-font+
   +flags-lossless-transformed-font-data+ +flags-lsb-at-x-0+
   +flags-optimized-for-cleartype+ +head-tag+ +mac-style-bold+
   +mac-style-condensed+ +mac-style-extended+ +mac-style-italic+
   +mac-style-outline+ +mac-style-shadow+ +mac-style-underline+
   flags-baseline-at-y-0-p flags-converted-font-p flags-force-integer-ppem-p
   flags-instructions-may-alter-advance-width-p
   flags-instructions-may-depend-on-point-size-p flags-last-resort-font-p
   flags-lossless-transformed-font-data-p flags-lsb-at-x-0-p
   flags-optimized-for-cleartype-p head head-checksum-adjustment head-created
   head-flags head-font-direction-hint head-font-revision
   head-glyph-data-format head-index-to-loc-format head-lowest-rec-ppem
   head-mac-style head-magic-number head-modified head-p head-units-per-em
   head-version head-x-max head-x-min head-y-max head-y-min mac-style-bold-p
   mac-style-condensed-p mac-style-extended-p mac-style-italic-p
   mac-style-outline-p mac-style-shadow-p mac-style-underline-p read-head
   sanitize-head))

(define-constant +mac-style-bold+ 1 "The `macStyle` field for the head table.")

(declaim (inline mac-style-bold-p))

(defun mac-style-bold-p (flags) (logtest flags +mac-style-bold+))

(define-constant +mac-style-italic+ 2
                 "The `macStyle` field for the head table.")

(declaim (inline mac-style-italic-p))

(defun mac-style-italic-p (flags) (logtest flags +mac-style-italic+))

(define-constant +mac-style-underline+ 4
                 "The `macStyle` field for the head table.")

(declaim (inline mac-style-underline-p))

(defun mac-style-underline-p (flags) (logtest flags +mac-style-underline+))

(define-constant +mac-style-outline+ 8
                 "The `macStyle` field for the head table.")

(declaim (inline mac-style-outline-p))

(defun mac-style-outline-p (flags) (logtest flags +mac-style-outline+))

(define-constant +mac-style-shadow+ 16
                 "The `macStyle` field for the head table.")

(declaim (inline mac-style-shadow-p))

(defun mac-style-shadow-p (flags) (logtest flags +mac-style-shadow+))

(define-constant +mac-style-condensed+ 32
                 "The `macStyle` field for the head table.")

(declaim (inline mac-style-condensed-p))

(defun mac-style-condensed-p (flags) (logtest flags +mac-style-condensed+))

(define-constant +mac-style-extended+ 64
                 "The `macStyle` field for the head table.")

(declaim (inline mac-style-extended-p))

(defun mac-style-extended-p (flags) (logtest flags +mac-style-extended+))

(define-constant +flags-baseline-at-y-0+ 1
                 "The `flags` field for the head table.")

(declaim (inline flags-baseline-at-y-0-p))

(defun flags-baseline-at-y-0-p (flags) (logtest flags +flags-baseline-at-y-0+))

(define-constant +flags-lsb-at-x-0+ 2 "The `flags` field for the head table.")

(declaim (inline flags-lsb-at-x-0-p))

(defun flags-lsb-at-x-0-p (flags) (logtest flags +flags-lsb-at-x-0+))

(define-constant +flags-instructions-may-depend-on-point-size+ 4
                 "The `flags` field for the head table.")

(declaim (inline flags-instructions-may-depend-on-point-size-p))

(defun flags-instructions-may-depend-on-point-size-p (flags)
  (logtest flags +flags-instructions-may-depend-on-point-size+))

(define-constant +flags-force-integer-ppem+ 8
                 "The `flags` field for the head table.")

(declaim (inline flags-force-integer-ppem-p))

(defun flags-force-integer-ppem-p (flags)
  (logtest flags +flags-force-integer-ppem+))

(define-constant +flags-instructions-may-alter-advance-width+ 16
                 "The `flags` field for the head table.")

(declaim (inline flags-instructions-may-alter-advance-width-p))

(defun flags-instructions-may-alter-advance-width-p (flags)
  (logtest flags +flags-instructions-may-alter-advance-width+))

(define-constant +flags-lossless-transformed-font-data+ 2048
                 "The `flags` field for the head table.")

(declaim (inline flags-lossless-transformed-font-data-p))

(defun flags-lossless-transformed-font-data-p (flags)
  (logtest flags +flags-lossless-transformed-font-data+))

(define-constant +flags-converted-font+ 4096
                 "The `flags` field for the head table.")

(declaim (inline flags-converted-font-p))

(defun flags-converted-font-p (flags) (logtest flags +flags-converted-font+))

(define-constant +flags-optimized-for-cleartype+ 8192
                 "The `flags` field for the head table.")

(declaim (inline flags-optimized-for-cleartype-p))

(defun flags-optimized-for-cleartype-p (flags)
  (logtest flags +flags-optimized-for-cleartype+))

(define-constant +flags-last-resort-font+ 16384
                 "The `flags` field for the head table.")

(declaim (inline flags-last-resort-font-p))

(defun flags-last-resort-font-p (flags)
  (logtest flags +flags-last-resort-font+))

(defstruct
    (head (:include table-ref)
     (:constructor %make-head (%table-font-data %table-offset %table-length)))
  "The [head](https://docs.microsoft.com/en-us/typography/opentype/spec/head) (font header) table.")

(declaim
 (inline head-version head-version-raw head-font-revision
  head-font-revision-raw head-checksum-adjustment head-checksum-adjustment-raw
  head-magic-number head-magic-number-raw head-flags head-flags-raw
  head-units-per-em head-units-per-em-raw head-created head-created-raw
  head-modified head-modified-raw head-x-min head-x-min-raw head-y-min
  head-y-min-raw head-x-max head-x-max-raw head-y-max head-y-max-raw
  head-mac-style head-mac-style-raw head-lowest-rec-ppem
  head-lowest-rec-ppem-raw head-font-direction-hint
  head-font-direction-hint-raw head-index-to-loc-format
  head-index-to-loc-format-raw head-glyph-data-format
  head-glyph-data-format-raw))

(define-constant +head-tag+ "head")

(defun head-version (table)
  "Version number of the font header table, set to (1, 0)"
  (declare (type head table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun head-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun head-font-revision (table)
  "Set by font manufacturer."
  (declare (type head table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun head-font-revision-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 4)))

(defun head-checksum-adjustment (table)
  "To compute: set it to 0, sum the entire font as uint32, then store 0xB1B0AFBA - sum. If the font is used as a component in a font collection file, the value of this field will be invalidated by changes to the file structure and font table directory, and must be ignored."
  (declare (type head table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun head-checksum-adjustment-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun head-magic-number (table)
  "Set to 0x5F0F3CF5."
  (declare (type head table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun head-magic-number-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 12)))

(defun head-flags (table)
  "See the flags enum."
  (declare (type head table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun head-flags-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 16)))

(defun head-units-per-em (table)
  "Set to a value from 16 to 16384. Any value in this range is valid. In fonts that have TrueType outlines, a power of 2 is recommended as this allows performance optimizations in some rasterizers."
  (declare (type head table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 18)))

(defun head-units-per-em-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 18)))

(defun head-created (table)
  "Number of seconds since 12:00 midnight that started January 1st 1904 in GMT/UTC time zone."
  (declare (type head table))
  (font-data-read-u64 (table-ref-font-data table)
                      (+ (table-ref-offset table) 20)))

(defun head-created-raw (font-data offset)
  (font-data-read-u64 font-data (+ offset 20)))

(defun head-modified (table)
  "Number of seconds since 12:00 midnight that started January 1st 1904 in GMT/UTC time zone."
  (declare (type head table))
  (font-data-read-u64 (table-ref-font-data table)
                      (+ (table-ref-offset table) 28)))

(defun head-modified-raw (font-data offset)
  (font-data-read-u64 font-data (+ offset 28)))

(defun head-x-min (table)
  "Minimum x coordinate across all glyph bounding boxes."
  (declare (type head table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 36)))

(defun head-x-min-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 36)))

(defun head-y-min (table)
  "Minimum y coordinate across all glyph bounding boxes."
  (declare (type head table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 38)))

(defun head-y-min-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 38)))

(defun head-x-max (table)
  "Maximum x coordinate across all glyph bounding boxes."
  (declare (type head table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 40)))

(defun head-x-max-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 40)))

(defun head-y-max (table)
  "Maximum y coordinate across all glyph bounding boxes."
  (declare (type head table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 42)))

(defun head-y-max-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 42)))

(defun head-mac-style (table)
  "Bits identifying the font's style; see [MacStyle]"
  (declare (type head table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 44)))

(defun head-mac-style-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 44)))

(defun head-lowest-rec-ppem (table)
  "Smallest readable size in pixels."
  (declare (type head table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 46)))

(defun head-lowest-rec-ppem-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 46)))

(defun head-font-direction-hint (table)
  "Deprecated (Set to 2)."
  (declare (type head table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 48)))

(defun head-font-direction-hint-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 48)))

(defun head-index-to-loc-format (table)
  "0 for short offsets (Offset16), 1 for long (Offset32)."
  (declare (type head table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 50)))

(defun head-index-to-loc-format-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 50)))

(defun head-glyph-data-format (table)
  "0 for current format."
  (declare (type head table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 52)))

(defun head-glyph-data-format-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 52)))

(defun sanitize-head (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 54 length) (font-data-valid-range-p font-data offset 54))
    (error 'font-out-of-bounds-error :pos offset :size 54 :length
           (font-data-length font-data)))
  t)

(defun read-head
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-head font-data offset length)
  (%make-head font-data offset length))
