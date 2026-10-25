;;;; cblc.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+cblc-tag+ cblc cblc-bitmap-sizes-count cblc-bitmap-sizes-ref
   cblc-major-version cblc-minor-version cblc-num-sizes cblc-p read-cblc
   sanitize-cblc))

(defstruct
    (cblc (:include table-ref)
     (:constructor %make-cblc (%table-font-data %table-offset %table-length)))
  "The [Color Bitmap Location](https://learn.microsoft.com/en-us/typography/opentype/spec/cblc) table")

(declaim
 (inline cblc-major-version cblc-major-version-raw cblc-minor-version
  cblc-minor-version-raw cblc-num-sizes cblc-num-sizes-raw
  cblc-bitmap-sizes-count cblc-bitmap-sizes-ref))

(define-constant +cblc-tag+ "CBLC")

(defun cblc-major-version (table)
  "Major version of the CBLC table, = 3."
  (declare (type cblc table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun cblc-major-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun cblc-minor-version (table)
  "Minor version of CBLC table, = 0."
  (declare (type cblc table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun cblc-minor-version-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun cblc-num-sizes (table)
  "Number of BitmapSize records."
  (declare (type cblc table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun cblc-num-sizes-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun cblc-bitmap-sizes-count (table)
  "BitmapSize records array."
  (declare (type cblc table))
  (declare (ignorable table))
  (cblc-num-sizes table))

(defun cblc-bitmap-sizes-ref (table index)
  "BitmapSize records array."
  (declare (type cblc table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 48))))
    (unless
        (<= (+ pos 48) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 48 :length
             (table-ref-length table)))
    (%make-bitmap-size (table-ref-font-data table) pos)))

(defun sanitize-cblc (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  (let* ((cnt (cblc-num-sizes-raw font-data offset)) (pos (+ 8 (* cnt 48))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-cblc
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cblc font-data offset length)
  (%make-cblc font-data offset length))
