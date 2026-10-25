;;;; cff2.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(cff2-header cff2-header--padding-bytes cff2-header--padding-count
   cff2-header--padding-ref cff2-header-header-size cff2-header-major-version
   cff2-header-minor-version cff2-header-p cff2-header-top-dict-data-bytes
   cff2-header-top-dict-data-count cff2-header-top-dict-data-ref
   cff2-header-top-dict-length cff2-header-trailing-data-bytes
   cff2-header-trailing-data-count cff2-header-trailing-data-ref cff2-index
   cff2-index-count cff2-index-data-bytes cff2-index-data-count
   cff2-index-data-ref cff2-index-off-size cff2-index-offsets-bytes
   cff2-index-offsets-count cff2-index-offsets-ref cff2-index-p
   read-cff2-header read-cff2-index sanitize-cff2-header sanitize-cff2-index))

(defstruct
    (cff2-header (:include table-ref)
     (:constructor %make-cff2-header
      (%table-font-data %table-offset %table-length)))
  "[Compact Font Format (CFF) version 2](https://learn.microsoft.com/en-us/typography/opentype/spec/cff2) table header")

(declaim
 (inline cff2-header-major-version cff2-header-major-version-raw
  cff2-header-minor-version cff2-header-minor-version-raw
  cff2-header-header-size cff2-header-header-size-raw
  cff2-header-top-dict-length cff2-header-top-dict-length-raw
  cff2-header--padding-count cff2-header--padding-ref
  cff2-header--padding-bytes cff2-header-top-dict-data-count
  cff2-header-top-dict-data-ref cff2-header-top-dict-data-bytes
  cff2-header-trailing-data-count cff2-header-trailing-data-ref
  cff2-header-trailing-data-bytes))

(defun cff2-header-major-version (table)
  "Format major version (set to 2)."
  (declare (type cff2-header table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun cff2-header-major-version-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun cff2-header-minor-version (table)
  "Format minor version (set to 0)."
  (declare (type cff2-header table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 1)))

(defun cff2-header-minor-version-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 1)))

(defun cff2-header-header-size (table)
  "Header size (bytes)."
  (declare (type cff2-header table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 2)))

(defun cff2-header-header-size-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 2)))

(defun cff2-header-top-dict-length (table)
  "Length of Top DICT structure in bytes."
  (declare (type cff2-header table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 3)))

(defun cff2-header-top-dict-length-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 3)))

(defun cff2-header--padding-count (table)
  "Padding bytes before the start of the Top DICT."
  (declare (type cff2-header table))
  (declare (ignorable table))
  (max 0 (- (cff2-header-header-size table) 5)))

(defun cff2-header--padding-ref (table index)
  "Padding bytes before the start of the Top DICT."
  (declare (type cff2-header table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 5) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun cff2-header--padding-bytes (table)
  "Padding bytes before the start of the Top DICT."
  (declare (type cff2-header table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 5)
                   (cff2-header--padding-count table)))

(defun cff2-header-top-dict-data-count (table)
  "Data containing the Top DICT."
  (declare (type cff2-header table))
  (declare (ignorable table))
  (cff2-header-top-dict-length table))

(defun cff2-header-top-dict-data-ref (table index)
  "Data containing the Top DICT."
  (declare (type cff2-header table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 5
             (* (cff2-header--padding-count table) 1))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun cff2-header-top-dict-data-bytes (table)
  "Data containing the Top DICT."
  (declare (type cff2-header table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 5
                      (* (cff2-header--padding-count table) 1))
                   (cff2-header-top-dict-data-count table)))

(defun cff2-header-trailing-data-count (table)
  "Remaining table data."
  (declare (type cff2-header table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 5
              (* (cff2-header--padding-count table) 1)
              (* (cff2-header-top-dict-data-count table) 1)))
        1)))

(defun cff2-header-trailing-data-ref (table index)
  "Remaining table data."
  (declare (type cff2-header table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 5
             (* (cff2-header--padding-count table) 1)
             (* (cff2-header-top-dict-data-count table) 1))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun cff2-header-trailing-data-bytes (table)
  "Remaining table data."
  (declare (type cff2-header table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 5
                      (* (cff2-header--padding-count table) 1)
                      (* (cff2-header-top-dict-data-count table) 1))
                   (cff2-header-trailing-data-count table)))

(defun sanitize-cff2-header (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 5 length) (font-data-valid-range-p font-data offset 5))
    (error 'font-out-of-bounds-error :pos offset :size 5 :length
           (font-data-length font-data)))
  t)

(defun read-cff2-header
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cff2-header font-data offset length)
  (%make-cff2-header font-data offset length))

(defstruct
    (cff2-index (:include table-ref)
     (:constructor %make-cff2-index
      (%table-font-data %table-offset %table-length)))
  "An array of variable-sized objects in a `CFF2` table.")

(declaim
 (inline cff2-index-count cff2-index-count-raw cff2-index-off-size
  cff2-index-off-size-raw cff2-index-offsets-count cff2-index-offsets-ref
  cff2-index-offsets-bytes cff2-index-data-count cff2-index-data-ref
  cff2-index-data-bytes))

(defun cff2-index-count (table)
  "Number of objects stored in INDEX."
  (declare (type cff2-index table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun cff2-index-count-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun cff2-index-off-size (table)
  "Object array element size."
  (declare (type cff2-index table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 4)))

(defun cff2-index-off-size-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 4)))

(defun cff2-index-offsets-count (table)
  "Bytes containing `count + 1` offsets each of `off_size`."
  (declare (type cff2-index table))
  (declare (ignorable table))
  (* (+ (cff2-index-count table) 1) (cff2-index-off-size table)))

(defun cff2-index-offsets-ref (table index)
  "Bytes containing `count + 1` offsets each of `off_size`."
  (declare (type cff2-index table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 5) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun cff2-index-offsets-bytes (table)
  "Bytes containing `count + 1` offsets each of `off_size`."
  (declare (type cff2-index table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 5)
                   (cff2-index-offsets-count table)))

(defun cff2-index-data-count (table)
  "Array containing the object data."
  (declare (type cff2-index table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 5
              (* (cff2-index-offsets-count table) 1)))
        1)))

(defun cff2-index-data-ref (table index)
  "Array containing the object data."
  (declare (type cff2-index table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 5 (* (cff2-index-offsets-count table) 1))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun cff2-index-data-bytes (table)
  "Array containing the object data."
  (declare (type cff2-index table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 5
                      (* (cff2-index-offsets-count table) 1))
                   (cff2-index-data-count table)))

(defun sanitize-cff2-index (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 5 length) (font-data-valid-range-p font-data offset 5))
    (error 'font-out-of-bounds-error :pos offset :size 5 :length
           (font-data-length font-data)))
  t)

(defun read-cff2-index
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cff2-index font-data offset length)
  (%make-cff2-index font-data offset length))
