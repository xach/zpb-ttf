;;;; cbdt.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+cbdt-tag+ cbdt cbdt-major-version cbdt-minor-version cbdt-p read-cbdt
   sanitize-cbdt))

(defstruct
    (cbdt (:include table-ref)
     (:constructor %make-cbdt (%table-font-data %table-offset %table-length)))
  "The [Color Bitmap Data](https://learn.microsoft.com/en-us/typography/opentype/spec/cbdt) table")

(declaim
 (inline cbdt-major-version cbdt-major-version-raw cbdt-minor-version
  cbdt-minor-version-raw))

(define-constant +cbdt-tag+ "CBDT")

(defun cbdt-major-version (table)
  "Major version of the CBDT table, = 3."
  (declare (type cbdt table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun cbdt-major-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun cbdt-minor-version (table)
  "Minor version of CBDT table, = 0."
  (declare (type cbdt table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun cbdt-minor-version-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun sanitize-cbdt (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-cbdt
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cbdt font-data offset length)
  (%make-cbdt font-data offset length))
