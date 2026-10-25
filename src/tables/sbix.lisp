;;;; sbix.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+header-flags-always-set+ +header-flags-draw-outlines+ +sbix-tag+ glyph-data
   glyph-data-data-bytes glyph-data-data-count glyph-data-data-ref
   glyph-data-graphic-type glyph-data-origin-offset-x
   glyph-data-origin-offset-y glyph-data-p header-flags-always-set-p
   header-flags-draw-outlines-p read-glyph-data read-sbix read-strike
   sanitize-glyph-data sanitize-sbix sanitize-strike sbix sbix-flags
   sbix-num-strikes sbix-p sbix-strike-offsets-count sbix-strike-offsets-ref
   sbix-version strike strike-glyph-data-offsets-count
   strike-glyph-data-offsets-ref strike-p strike-ppem strike-ppi))

(define-constant +header-flags-always-set+ 1 "Sbix header flags.")

(declaim (inline header-flags-always-set-p))

(defun header-flags-always-set-p (flags)
  (logtest flags +header-flags-always-set+))

(define-constant +header-flags-draw-outlines+ 2 "Sbix header flags.")

(declaim (inline header-flags-draw-outlines-p))

(defun header-flags-draw-outlines-p (flags)
  (logtest flags +header-flags-draw-outlines+))

(defstruct
    (sbix (:include table-ref)
     (:constructor %make-sbix (%table-font-data %table-offset %table-length)))
  "The [sbix (Standard Bitmap Graphics)](https://docs.microsoft.com/en-us/typography/opentype/spec/sbix) table")

(declaim
 (inline sbix-version sbix-version-raw sbix-flags sbix-flags-raw
  sbix-num-strikes sbix-num-strikes-raw sbix-strike-offsets-count
  sbix-strike-offsets-ref))

(define-constant +sbix-tag+ "sbix")

(defun sbix-version (table)
  "Table version number — set to 1."
  (declare (type sbix table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun sbix-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun sbix-flags (table)
  "Bit 0: Set to 1. Bit 1: Draw outlines. Bits 2 to 15: reserved (set to 0)."
  (declare (type sbix table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun sbix-flags-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun sbix-num-strikes (table)
  "Number of bitmap strikes."
  (declare (type sbix table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun sbix-num-strikes-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun sbix-strike-offsets-count (table)
  "Offsets from the beginning of the 'sbix' table to data for each individual bitmap strike."
  (declare (type sbix table))
  (declare (ignorable table))
  (sbix-num-strikes table))

(defun sbix-strike-offsets-ref (table index)
  "Offsets from the beginning of the 'sbix' table to data for each individual bitmap strike."
  (declare (type sbix table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u32 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-strike (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-sbix (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  (let* ((cnt (sbix-num-strikes-raw font-data offset)) (pos (+ 8 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-sbix
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-sbix font-data offset length)
  (%make-sbix font-data offset length))

(defstruct
    (strike (:include table-ref)
     (:constructor %make-strike
      (%table-font-data %table-offset %table-length)))
  "[Strike](https://learn.microsoft.com/en-us/typography/opentype/spec/sbix#strikes) header table")

(declaim
 (inline strike-ppem strike-ppem-raw strike-ppi strike-ppi-raw
  strike-glyph-data-offsets-count strike-glyph-data-offsets-ref))

(defun strike-ppem (table)
  "The PPEM size for which this strike was designed."
  (declare (type strike table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun strike-ppem-raw (font-data offset) (font-data-read-u16 font-data offset))

(defun strike-ppi (table)
  "The device pixel density (in PPI) for which this strike was designed. (E.g., 96 PPI, 192 PPI.)"
  (declare (type strike table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun strike-ppi-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun strike-glyph-data-offsets-count (table num-glyphs)
  "Offset from the beginning of the strike data header to bitmap data for an individual glyph ID."
  (declare (type strike table)
           (ignorable num-glyphs))
  (declare (ignorable table))
  (+ num-glyphs 1))

(defun strike-glyph-data-offsets-ref (table num-glyphs index)
  "Offset from the beginning of the strike data header to bitmap data for an individual glyph ID."
  (declare (type strike table)
           (ignorable num-glyphs index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (font-data-read-u32 (table-ref-font-data table) pos)))

(defun sanitize-strike (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-strike
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-strike font-data offset length)
  (%make-strike font-data offset length))

(defstruct
    (glyph-data (:include table-ref)
     (:constructor %make-glyph-data
      (%table-font-data %table-offset %table-length)))
  "[Glyph data](https://learn.microsoft.com/en-us/typography/opentype/spec/sbix#glyph-data) table")

(declaim
 (inline glyph-data-origin-offset-x glyph-data-origin-offset-x-raw
  glyph-data-origin-offset-y glyph-data-origin-offset-y-raw
  glyph-data-graphic-type glyph-data-graphic-type-raw glyph-data-data-count
  glyph-data-data-ref glyph-data-data-bytes))

(defun glyph-data-origin-offset-x (table)
  "The horizontal (x-axis) position of the left edge of the bitmap graphic in relation to the glyph design space origin."
  (declare (type glyph-data table))
  (font-data-read-i16 (table-ref-font-data table) (table-ref-offset table)))

(defun glyph-data-origin-offset-x-raw (font-data offset)
  (font-data-read-i16 font-data offset))

(defun glyph-data-origin-offset-y (table)
  "The vertical (y-axis) position of the bottom edge of the bitmap graphic in relation to the glyph design space origin."
  (declare (type glyph-data table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun glyph-data-origin-offset-y-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun glyph-data-graphic-type (table)
  "Indicates the format of the embedded graphic data: one of 'jpg ', 'png ' or 'tiff', or the special format 'dupe'."
  (declare (type glyph-data table))
  (font-data-read-tag-string (table-ref-font-data table)
                             (+ (table-ref-offset table) 4)))

(defun glyph-data-graphic-type-raw (font-data offset)
  (font-data-read-tag-string font-data (+ offset 4)))

(defun glyph-data-data-count (table)
  "The actual embedded graphic data. The total length is inferred from sequential entries in the glyphDataOffsets array and the fixed size (8 bytes) of the preceding fields."
  (declare (type glyph-data table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 8))
        1)))

(defun glyph-data-data-ref (table index)
  "The actual embedded graphic data. The total length is inferred from sequential entries in the glyphDataOffsets array and the fixed size (8 bytes) of the preceding fields."
  (declare (type glyph-data table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun glyph-data-data-bytes (table)
  "The actual embedded graphic data. The total length is inferred from sequential entries in the glyphDataOffsets array and the fixed size (8 bytes) of the preceding fields."
  (declare (type glyph-data table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 8)
                   (glyph-data-data-count table)))

(defun sanitize-glyph-data (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-glyph-data
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-glyph-data font-data offset length)
  (%make-glyph-data font-data offset length))
