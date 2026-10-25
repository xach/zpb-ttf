;;;; ebdt.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+ebdt-tag+ ebdt ebdt-major-version ebdt-minor-version ebdt-p read-ebdt
   sanitize-ebdt))

(defstruct
    (ebdt (:include table-ref)
     (:constructor %make-ebdt (%table-font-data %table-offset %table-length)))
  "The [Embedded Bitmap Data](https://learn.microsoft.com/en-us/typography/opentype/spec/ebdt) table")

(declaim
 (inline ebdt-major-version ebdt-major-version-raw ebdt-minor-version
  ebdt-minor-version-raw))

(define-constant +ebdt-tag+ "EBDT")

(defun ebdt-major-version (table)
  "Major version of the EBDT table, = 2."
  (declare (type ebdt table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun ebdt-major-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun ebdt-minor-version (table)
  "Minor version of EBDT table, = 0."
  (declare (type ebdt table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun ebdt-minor-version-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun sanitize-ebdt (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-ebdt
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ebdt font-data offset length)
  (%make-ebdt font-data offset length))
