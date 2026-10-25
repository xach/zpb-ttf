;;;; ankr.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+anchor-point-size+ +ankr-tag+ anchor-point anchor-point-p anchor-point-x
   anchor-point-y ankr ankr-flags ankr-glyph-data-table-offset
   ankr-lookup-table-offset ankr-lookup-table-offset-offset ankr-p ankr-version
   glyph-data-entry glyph-data-entry-anchor-points-count
   glyph-data-entry-anchor-points-ref glyph-data-entry-num-points
   glyph-data-entry-p read-ankr read-glyph-data-entry sanitize-ankr
   sanitize-glyph-data-entry))

(defstruct
    (anchor-point (:include record-ref)
     (:constructor %make-anchor-point (%record-font-data %record-offset)))
  "Individual anchor point.")

(define-constant +anchor-point-size+ 4)

(declaim (inline anchor-point-x anchor-point-y))

(defun anchor-point-x (rec)
  (declare (type anchor-point rec))
  (font-data-read-i16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun anchor-point-y (rec)
  (declare (type anchor-point rec))
  (font-data-read-i16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (ankr (:include table-ref)
     (:constructor %make-ankr (%table-font-data %table-offset %table-length)))
  "The [anchor point](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6ankr.html) table.")

(declaim
 (inline ankr-version ankr-version-raw ankr-flags ankr-flags-raw
  ankr-lookup-table-offset-offset ankr-lookup-table-offset
  ankr-glyph-data-table-offset ankr-glyph-data-table-offset-raw))

(define-constant +ankr-tag+ "ankr")

(defun ankr-version (table)
  "Version number (set to zero)."
  (declare (type ankr table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun ankr-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun ankr-flags (table)
  "Flags (currently unused; set to zero)."
  (declare (type ankr table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun ankr-flags-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun ankr-lookup-table-offset-offset (table)
  "Offset to the table's lookup table; currently this is always `0x0000000C`.  Lookup values are two byte offsets into the glyph data table."
  (declare (type ankr table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun ankr-lookup-table-offset (table)
  "Offset to the table's lookup table; currently this is always `0x0000000C`.  Lookup values are two byte offsets into the glyph data table."
  (declare (type ankr table))
  (let ((off (ankr-lookup-table-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun ankr-glyph-data-table-offset (table)
  "Offset to the glyph data table."
  (declare (type ankr table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun ankr-glyph-data-table-offset-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun sanitize-ankr (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-ankr
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ankr font-data offset length)
  (%make-ankr font-data offset length))

(defstruct
    (glyph-data-entry (:include table-ref)
     (:constructor %make-glyph-data-entry
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline glyph-data-entry-num-points glyph-data-entry-num-points-raw
  glyph-data-entry-anchor-points-count glyph-data-entry-anchor-points-ref))

(defun glyph-data-entry-num-points (table)
  "Number of anchor points for this glyph."
  (declare (type glyph-data-entry table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun glyph-data-entry-num-points-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun glyph-data-entry-anchor-points-count (table)
  "Individual anchor points."
  (declare (type glyph-data-entry table))
  (declare (ignorable table))
  (glyph-data-entry-num-points table))

(defun glyph-data-entry-anchor-points-ref (table index)
  "Individual anchor points."
  (declare (type glyph-data-entry table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-anchor-point (table-ref-font-data table) pos)))

(defun sanitize-glyph-data-entry (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (glyph-data-entry-num-points-raw font-data offset))
         (pos (+ 4 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-glyph-data-entry
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-glyph-data-entry font-data offset length)
  (%make-glyph-data-entry font-data offset length))
