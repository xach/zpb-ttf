;;;; glyf.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+composite-glyph-flags-arg-1-and-2-are-words+
   +composite-glyph-flags-args-are-xy-values+
   +composite-glyph-flags-gid-is-24-bit+
   +composite-glyph-flags-more-components+
   +composite-glyph-flags-overlap-compound+
   +composite-glyph-flags-round-xy-to-grid+
   +composite-glyph-flags-scaled-component-offset+
   +composite-glyph-flags-unscaled-component-offset+
   +composite-glyph-flags-use-my-metrics+
   +composite-glyph-flags-we-have-a-scale+
   +composite-glyph-flags-we-have-a-two-by-two+
   +composite-glyph-flags-we-have-an-x-and-y-scale+
   +composite-glyph-flags-we-have-instructions+ +glyf-tag+
   +simple-glyph-flags-cubic+ +simple-glyph-flags-on-curve-point+
   +simple-glyph-flags-overlap-simple+ +simple-glyph-flags-repeat-flag+
   +simple-glyph-flags-x-is-same-or-positive-x-short-vector+
   +simple-glyph-flags-x-short-vector+
   +simple-glyph-flags-y-is-same-or-positive-y-short-vector+
   +simple-glyph-flags-y-short-vector+ composite-glyph
   composite-glyph-component-data-bytes composite-glyph-component-data-count
   composite-glyph-component-data-ref
   composite-glyph-flags-arg-1-and-2-are-words-p
   composite-glyph-flags-args-are-xy-values-p
   composite-glyph-flags-gid-is-24-bit-p
   composite-glyph-flags-more-components-p
   composite-glyph-flags-overlap-compound-p
   composite-glyph-flags-round-xy-to-grid-p
   composite-glyph-flags-scaled-component-offset-p
   composite-glyph-flags-unscaled-component-offset-p
   composite-glyph-flags-use-my-metrics-p
   composite-glyph-flags-we-have-a-scale-p
   composite-glyph-flags-we-have-a-two-by-two-p
   composite-glyph-flags-we-have-an-x-and-y-scale-p
   composite-glyph-flags-we-have-instructions-p
   composite-glyph-number-of-contours composite-glyph-p composite-glyph-x-max
   composite-glyph-x-min composite-glyph-y-max composite-glyph-y-min glyf
   glyf-p read-composite-glyph read-glyf read-glyph read-simple-glyph
   sanitize-composite-glyph sanitize-glyf sanitize-simple-glyph simple-glyph
   simple-glyph-end-pts-of-contours-count simple-glyph-end-pts-of-contours-ref
   simple-glyph-flags-cubic-p simple-glyph-flags-on-curve-point-p
   simple-glyph-flags-overlap-simple-p simple-glyph-flags-repeat-flag-p
   simple-glyph-flags-x-is-same-or-positive-x-short-vector-p
   simple-glyph-flags-x-short-vector-p
   simple-glyph-flags-y-is-same-or-positive-y-short-vector-p
   simple-glyph-flags-y-short-vector-p simple-glyph-glyph-data-bytes
   simple-glyph-glyph-data-count simple-glyph-glyph-data-ref
   simple-glyph-instruction-length simple-glyph-instructions-bytes
   simple-glyph-instructions-count simple-glyph-instructions-ref
   simple-glyph-number-of-contours simple-glyph-p simple-glyph-x-max
   simple-glyph-x-min simple-glyph-y-max simple-glyph-y-min))

(define-constant +simple-glyph-flags-on-curve-point+ 1
                 "Flags used in [SimpleGlyph]")

(declaim (inline simple-glyph-flags-on-curve-point-p))

(defun simple-glyph-flags-on-curve-point-p (flags)
  (logtest flags +simple-glyph-flags-on-curve-point+))

(define-constant +simple-glyph-flags-x-short-vector+ 2
                 "Flags used in [SimpleGlyph]")

(declaim (inline simple-glyph-flags-x-short-vector-p))

(defun simple-glyph-flags-x-short-vector-p (flags)
  (logtest flags +simple-glyph-flags-x-short-vector+))

(define-constant +simple-glyph-flags-y-short-vector+ 4
                 "Flags used in [SimpleGlyph]")

(declaim (inline simple-glyph-flags-y-short-vector-p))

(defun simple-glyph-flags-y-short-vector-p (flags)
  (logtest flags +simple-glyph-flags-y-short-vector+))

(define-constant +simple-glyph-flags-repeat-flag+ 8
                 "Flags used in [SimpleGlyph]")

(declaim (inline simple-glyph-flags-repeat-flag-p))

(defun simple-glyph-flags-repeat-flag-p (flags)
  (logtest flags +simple-glyph-flags-repeat-flag+))

(define-constant +simple-glyph-flags-x-is-same-or-positive-x-short-vector+ 16
                 "Flags used in [SimpleGlyph]")

(declaim (inline simple-glyph-flags-x-is-same-or-positive-x-short-vector-p))

(defun simple-glyph-flags-x-is-same-or-positive-x-short-vector-p (flags)
  (logtest flags +simple-glyph-flags-x-is-same-or-positive-x-short-vector+))

(define-constant +simple-glyph-flags-y-is-same-or-positive-y-short-vector+ 32
                 "Flags used in [SimpleGlyph]")

(declaim (inline simple-glyph-flags-y-is-same-or-positive-y-short-vector-p))

(defun simple-glyph-flags-y-is-same-or-positive-y-short-vector-p (flags)
  (logtest flags +simple-glyph-flags-y-is-same-or-positive-y-short-vector+))

(define-constant +simple-glyph-flags-overlap-simple+ 64
                 "Flags used in [SimpleGlyph]")

(declaim (inline simple-glyph-flags-overlap-simple-p))

(defun simple-glyph-flags-overlap-simple-p (flags)
  (logtest flags +simple-glyph-flags-overlap-simple+))

(define-constant +simple-glyph-flags-cubic+ 128 "Flags used in [SimpleGlyph]")

(declaim (inline simple-glyph-flags-cubic-p))

(defun simple-glyph-flags-cubic-p (flags)
  (logtest flags +simple-glyph-flags-cubic+))

(define-constant +composite-glyph-flags-arg-1-and-2-are-words+ 1
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-arg-1-and-2-are-words-p))

(defun composite-glyph-flags-arg-1-and-2-are-words-p (flags)
  (logtest flags +composite-glyph-flags-arg-1-and-2-are-words+))

(define-constant +composite-glyph-flags-args-are-xy-values+ 2
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-args-are-xy-values-p))

(defun composite-glyph-flags-args-are-xy-values-p (flags)
  (logtest flags +composite-glyph-flags-args-are-xy-values+))

(define-constant +composite-glyph-flags-round-xy-to-grid+ 4
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-round-xy-to-grid-p))

(defun composite-glyph-flags-round-xy-to-grid-p (flags)
  (logtest flags +composite-glyph-flags-round-xy-to-grid+))

(define-constant +composite-glyph-flags-we-have-a-scale+ 8
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-we-have-a-scale-p))

(defun composite-glyph-flags-we-have-a-scale-p (flags)
  (logtest flags +composite-glyph-flags-we-have-a-scale+))

(define-constant +composite-glyph-flags-more-components+ 32
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-more-components-p))

(defun composite-glyph-flags-more-components-p (flags)
  (logtest flags +composite-glyph-flags-more-components+))

(define-constant +composite-glyph-flags-we-have-an-x-and-y-scale+ 64
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-we-have-an-x-and-y-scale-p))

(defun composite-glyph-flags-we-have-an-x-and-y-scale-p (flags)
  (logtest flags +composite-glyph-flags-we-have-an-x-and-y-scale+))

(define-constant +composite-glyph-flags-we-have-a-two-by-two+ 128
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-we-have-a-two-by-two-p))

(defun composite-glyph-flags-we-have-a-two-by-two-p (flags)
  (logtest flags +composite-glyph-flags-we-have-a-two-by-two+))

(define-constant +composite-glyph-flags-we-have-instructions+ 256
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-we-have-instructions-p))

(defun composite-glyph-flags-we-have-instructions-p (flags)
  (logtest flags +composite-glyph-flags-we-have-instructions+))

(define-constant +composite-glyph-flags-use-my-metrics+ 512
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-use-my-metrics-p))

(defun composite-glyph-flags-use-my-metrics-p (flags)
  (logtest flags +composite-glyph-flags-use-my-metrics+))

(define-constant +composite-glyph-flags-overlap-compound+ 1024
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-overlap-compound-p))

(defun composite-glyph-flags-overlap-compound-p (flags)
  (logtest flags +composite-glyph-flags-overlap-compound+))

(define-constant +composite-glyph-flags-scaled-component-offset+ 2048
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-scaled-component-offset-p))

(defun composite-glyph-flags-scaled-component-offset-p (flags)
  (logtest flags +composite-glyph-flags-scaled-component-offset+))

(define-constant +composite-glyph-flags-unscaled-component-offset+ 4096
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-unscaled-component-offset-p))

(defun composite-glyph-flags-unscaled-component-offset-p (flags)
  (logtest flags +composite-glyph-flags-unscaled-component-offset+))

(define-constant +composite-glyph-flags-gid-is-24-bit+ 8192
                 "Flags used in [CompositeGlyph]")

(declaim (inline composite-glyph-flags-gid-is-24-bit-p))

(defun composite-glyph-flags-gid-is-24-bit-p (flags)
  (logtest flags +composite-glyph-flags-gid-is-24-bit+))

(defstruct
    (glyf (:include table-ref)
     (:constructor %make-glyf (%table-font-data %table-offset %table-length)))
  "The [glyf (Glyph Data)](https://docs.microsoft.com/en-us/typography/opentype/spec/glyf) table")

(define-constant +glyf-tag+ "glyf")

(defun sanitize-glyf (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-glyf
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-glyf font-data offset length)
  (%make-glyf font-data offset length))

(defstruct
    (simple-glyph (:include table-ref)
     (:constructor %make-simple-glyph
      (%table-font-data %table-offset %table-length)))
  "The [Glyph Header](https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#glyph-headers)")

(declaim
 (inline simple-glyph-number-of-contours simple-glyph-number-of-contours-raw
  simple-glyph-x-min simple-glyph-x-min-raw simple-glyph-y-min
  simple-glyph-y-min-raw simple-glyph-x-max simple-glyph-x-max-raw
  simple-glyph-y-max simple-glyph-y-max-raw
  simple-glyph-end-pts-of-contours-count simple-glyph-end-pts-of-contours-ref
  simple-glyph-instruction-length simple-glyph-instructions-count
  simple-glyph-instructions-ref simple-glyph-instructions-bytes
  simple-glyph-glyph-data-count simple-glyph-glyph-data-ref
  simple-glyph-glyph-data-bytes))

(defun simple-glyph-number-of-contours (table)
  "If the number of contours is greater than or equal to zero, this is a simple glyph. If negative, this is a composite glyph — the value -1 should be used for composite glyphs."
  (declare (type simple-glyph table))
  (font-data-read-i16 (table-ref-font-data table) (table-ref-offset table)))

(defun simple-glyph-number-of-contours-raw (font-data offset)
  (font-data-read-i16 font-data offset))

(defun simple-glyph-x-min (table)
  "Minimum x for coordinate data."
  (declare (type simple-glyph table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun simple-glyph-x-min-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun simple-glyph-y-min (table)
  "Minimum y for coordinate data."
  (declare (type simple-glyph table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun simple-glyph-y-min-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 4)))

(defun simple-glyph-x-max (table)
  "Maximum x for coordinate data."
  (declare (type simple-glyph table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun simple-glyph-x-max-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 6)))

(defun simple-glyph-y-max (table)
  "Maximum y for coordinate data."
  (declare (type simple-glyph table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun simple-glyph-y-max-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 8)))

(defun simple-glyph-end-pts-of-contours-count (table)
  "Array of point indices for the last point of each contour, in increasing numeric order"
  (declare (type simple-glyph table))
  (declare (ignorable table))
  (simple-glyph-number-of-contours table))

(defun simple-glyph-end-pts-of-contours-ref (table index)
  "Array of point indices for the last point of each contour, in increasing numeric order"
  (declare (type simple-glyph table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 10) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun simple-glyph-instruction-length (table)
  "Total number of bytes for instructions. If instructionLength is zero, no instructions are present for this glyph, and this field is followed directly by the flags field."
  (declare (type simple-glyph table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10
                         (* (simple-glyph-end-pts-of-contours-count table) 2))))

(defun simple-glyph-instructions-count (table)
  "Array of instruction byte code for the glyph."
  (declare (type simple-glyph table))
  (declare (ignorable table))
  (simple-glyph-instruction-length table))

(defun simple-glyph-instructions-ref (table index)
  "Array of instruction byte code for the glyph."
  (declare (type simple-glyph table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 12
             (* (simple-glyph-end-pts-of-contours-count table) 2))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun simple-glyph-instructions-bytes (table)
  "Array of instruction byte code for the glyph."
  (declare (type simple-glyph table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 12
                      (* (simple-glyph-end-pts-of-contours-count table) 2))
                   (simple-glyph-instructions-count table)))

(defun simple-glyph-glyph-data-count (table)
  "the raw data for flags & x/y coordinates"
  (declare (type simple-glyph table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 12
              (* (simple-glyph-end-pts-of-contours-count table) 2)
              (* (simple-glyph-instructions-count table) 1)))
        1)))

(defun simple-glyph-glyph-data-ref (table index)
  "the raw data for flags & x/y coordinates"
  (declare (type simple-glyph table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 12
             (* (simple-glyph-end-pts-of-contours-count table) 2)
             (* (simple-glyph-instructions-count table) 1))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun simple-glyph-glyph-data-bytes (table)
  "the raw data for flags & x/y coordinates"
  (declare (type simple-glyph table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 12
                      (* (simple-glyph-end-pts-of-contours-count table) 2)
                      (* (simple-glyph-instructions-count table) 1))
                   (simple-glyph-glyph-data-count table)))

(defun sanitize-simple-glyph (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  (let* ((cnt (simple-glyph-number-of-contours-raw font-data offset))
         (pos (+ 10 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-simple-glyph
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-simple-glyph font-data offset length)
  (%make-simple-glyph font-data offset length))

(defstruct
    (composite-glyph (:include table-ref)
     (:constructor %make-composite-glyph
      (%table-font-data %table-offset %table-length)))
  "[CompositeGlyph](https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#glyph-headers)")

(declaim
 (inline composite-glyph-number-of-contours
  composite-glyph-number-of-contours-raw composite-glyph-x-min
  composite-glyph-x-min-raw composite-glyph-y-min composite-glyph-y-min-raw
  composite-glyph-x-max composite-glyph-x-max-raw composite-glyph-y-max
  composite-glyph-y-max-raw composite-glyph-component-data-count
  composite-glyph-component-data-ref composite-glyph-component-data-bytes))

(defun composite-glyph-number-of-contours (table)
  "If the number of contours is greater than or equal to zero, this is a simple glyph. If negative, this is a composite glyph — the value -1 should be used for composite glyphs."
  (declare (type composite-glyph table))
  (font-data-read-i16 (table-ref-font-data table) (table-ref-offset table)))

(defun composite-glyph-number-of-contours-raw (font-data offset)
  (font-data-read-i16 font-data offset))

(defun composite-glyph-x-min (table)
  "Minimum x for coordinate data."
  (declare (type composite-glyph table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun composite-glyph-x-min-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun composite-glyph-y-min (table)
  "Minimum y for coordinate data."
  (declare (type composite-glyph table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun composite-glyph-y-min-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 4)))

(defun composite-glyph-x-max (table)
  "Maximum x for coordinate data."
  (declare (type composite-glyph table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun composite-glyph-x-max-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 6)))

(defun composite-glyph-y-max (table)
  "Maximum y for coordinate data."
  (declare (type composite-glyph table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun composite-glyph-y-max-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 8)))

(defun composite-glyph-component-data-count (table)
  "component flag glyph index of component"
  (declare (type composite-glyph table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 10))
        1)))

(defun composite-glyph-component-data-ref (table index)
  "component flag glyph index of component"
  (declare (type composite-glyph table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 10) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun composite-glyph-component-data-bytes (table)
  "component flag glyph index of component"
  (declare (type composite-glyph table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 10)
                   (composite-glyph-component-data-count table)))

(defun sanitize-composite-glyph (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  t)

(defun read-composite-glyph
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-composite-glyph font-data offset length)
  (%make-composite-glyph font-data offset length))

(defun read-glyph
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "Simple or composite glyph."
  (declare (ignorable length))
  (let ((fmt (font-data-read-i16 font-data (+ offset 0))))
    (if (minusp fmt)
        (read-composite-glyph font-data offset length)
        (read-simple-glyph font-data offset length))))
