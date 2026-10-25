;;;; vvar.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+vvar-tag+ read-vvar sanitize-vvar vvar vvar-advance-height-mapping-offset
   vvar-advance-height-mapping-offset-offset vvar-bsb-mapping-offset
   vvar-bsb-mapping-offset-offset vvar-item-variation-store-offset
   vvar-item-variation-store-offset-offset vvar-p vvar-tsb-mapping-offset
   vvar-tsb-mapping-offset-offset vvar-v-org-mapping-offset
   vvar-v-org-mapping-offset-offset vvar-version))

(defstruct
    (vvar (:include table-ref)
     (:constructor %make-vvar (%table-font-data %table-offset %table-length)))
  "The [VVAR (Vertical Metrics Variations)](https://docs.microsoft.com/en-us/typography/opentype/spec/vvar) table")

(declaim
 (inline vvar-version vvar-version-raw vvar-item-variation-store-offset-offset
  vvar-item-variation-store-offset vvar-advance-height-mapping-offset-offset
  vvar-advance-height-mapping-offset vvar-tsb-mapping-offset-offset
  vvar-tsb-mapping-offset vvar-bsb-mapping-offset-offset
  vvar-bsb-mapping-offset vvar-v-org-mapping-offset-offset
  vvar-v-org-mapping-offset))

(define-constant +vvar-tag+ "VVAR")

(defun vvar-version (table)
  "Major version number of the horizontal metrics variations table — set to 1. Minor version number of the horizontal metrics variations table — set to 0."
  (declare (type vvar table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun vvar-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun vvar-item-variation-store-offset-offset (table)
  "Offset in bytes from the start of this table to the item variation store table."
  (declare (type vvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun vvar-item-variation-store-offset (table)
  "Offset in bytes from the start of this table to the item variation store table."
  (declare (type vvar table))
  (let ((off (vvar-item-variation-store-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-item-variation-store (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun vvar-advance-height-mapping-offset-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for advance heights (may be NULL)."
  (declare (type vvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun vvar-advance-height-mapping-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for advance heights (may be NULL)."
  (declare (type vvar table))
  (let ((off (vvar-advance-height-mapping-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-delta-set-index-map (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun vvar-tsb-mapping-offset-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for top side bearings (may be NULL)."
  (declare (type vvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun vvar-tsb-mapping-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for top side bearings (may be NULL)."
  (declare (type vvar table))
  (let ((off (vvar-tsb-mapping-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-delta-set-index-map (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun vvar-bsb-mapping-offset-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for bottom side bearings (may be NULL)."
  (declare (type vvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun vvar-bsb-mapping-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for bottom side bearings (may be NULL)."
  (declare (type vvar table))
  (let ((off (vvar-bsb-mapping-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-delta-set-index-map (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun vvar-v-org-mapping-offset-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for Y coordinates of vertical origins (may be NULL)."
  (declare (type vvar table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 20)))

(defun vvar-v-org-mapping-offset (table)
  "Offset in bytes from the start of this table to the delta-set index mapping for Y coordinates of vertical origins (may be NULL)."
  (declare (type vvar table))
  (let ((off (vvar-v-org-mapping-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-delta-set-index-map (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-vvar (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 24 length) (font-data-valid-range-p font-data offset 24))
    (error 'font-out-of-bounds-error :pos offset :size 24 :length
           (font-data-length font-data)))
  t)

(defun read-vvar
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-vvar font-data offset length)
  (%make-vvar font-data offset length))
