;;;; hvar.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+hvar-tag+ hvar hvar-advance-width-mapping-offset
   hvar-advance-width-mapping-offset-offset hvar-item-variation-store-offset
   hvar-item-variation-store-offset-offset hvar-lsb-mapping-offset
   hvar-lsb-mapping-offset-offset hvar-p hvar-rsb-mapping-offset
   hvar-rsb-mapping-offset-offset hvar-version read-hvar sanitize-hvar))

(defstruct
    (hvar (:include table-ref)
     (:constructor %make-hvar (%table-font-data %table-offset %table-length)))
  "The [HVAR (Horizontal Metrics Variations)](https://docs.microsoft.com/en-us/typography/opentype/spec/hvar) table")

(declaim
 (inline hvar-version hvar-version-raw hvar-item-variation-store-offset-offset
  hvar-item-variation-store-offset hvar-advance-width-mapping-offset-offset
  hvar-advance-width-mapping-offset hvar-lsb-mapping-offset-offset
  hvar-lsb-mapping-offset hvar-rsb-mapping-offset-offset
  hvar-rsb-mapping-offset))

(define-constant +hvar-tag+ "HVAR")

(defun hvar-version (table)
  "Major version number of the horizontal metrics variations table — set to 1. Minor version number of the horizontal metrics variations table — set to 0."
  (declare (type hvar table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun hvar-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun hvar-item-variation-store-offset-offset (table)
  "Offset in bytes from the start of this table to the item variation store table."
  (declare (type hvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun hvar-item-variation-store-offset (table)
  "Offset in bytes from the start of this table to the item variation store table."
  (declare (type hvar table))
  (let ((off (hvar-item-variation-store-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-item-variation-store (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun hvar-advance-width-mapping-offset-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for advance widths (may be NULL)."
  (declare (type hvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun hvar-advance-width-mapping-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for advance widths (may be NULL)."
  (declare (type hvar table))
  (let ((off (hvar-advance-width-mapping-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-delta-set-index-map (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun hvar-lsb-mapping-offset-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for left side bearings (may be NULL)."
  (declare (type hvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun hvar-lsb-mapping-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for left side bearings (may be NULL)."
  (declare (type hvar table))
  (let ((off (hvar-lsb-mapping-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-delta-set-index-map (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun hvar-rsb-mapping-offset-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for right side bearings (may be NULL)."
  (declare (type hvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun hvar-rsb-mapping-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for right side bearings (may be NULL)."
  (declare (type hvar table))
  (let ((off (hvar-rsb-mapping-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-delta-set-index-map (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-hvar (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 20 length) (font-data-valid-range-p font-data offset 20))
    (error 'font-out-of-bounds-error :pos offset :size 20 :length
           (font-data-length font-data)))
  t)

(defun read-hvar
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-hvar font-data offset length)
  (%make-hvar font-data offset length))
