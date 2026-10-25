;;;; cvar.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+cvar-tag+ cvar cvar-data-offset cvar-data-offset-offset cvar-p
   cvar-tuple-variation-count cvar-tuple-variation-headers-start cvar-version
   read-cvar sanitize-cvar))

(defstruct
    (cvar (:include table-ref)
     (:constructor %make-cvar (%table-font-data %table-offset %table-length)))
  "The [cvar](https://learn.microsoft.com/en-us/typography/opentype/spec/cvar) table.")

(declaim
 (inline cvar-version cvar-version-raw cvar-tuple-variation-count
  cvar-tuple-variation-count-raw cvar-data-offset-offset cvar-data-offset
  cvar-tuple-variation-headers-start))

(define-constant +cvar-tag+ "cvar")

(defun cvar-version (table)
  "Major/minor version number of the CVT variations table — set to (1,0)."
  (declare (type cvar table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun cvar-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun cvar-tuple-variation-count (table)
  "A packed field. The high 4 bits are flags, and the low 12 bits are the number of tuple variation tables for this glyph. The number of tuple variation tables can be any number between 1 and 4095."
  (declare (type cvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun cvar-tuple-variation-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun cvar-data-offset-offset (table)
  "Offset from the start of the 'cvar' table to the serialized data."
  (declare (type cvar table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun cvar-data-offset (table)
  "Offset from the start of the 'cvar' table to the serialized data."
  (declare (type cvar table))
  (let ((off (cvar-data-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun cvar-tuple-variation-headers-start (table)
  "Byte position of the tuple-variation-headers field; its elements are variable-length TUPLE-VARIATION-HEADER data."
  (declare (type cvar table))
  (+ (table-ref-offset table) 8))

(defun sanitize-cvar (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-cvar
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cvar font-data offset length)
  (%make-cvar font-data offset length))
