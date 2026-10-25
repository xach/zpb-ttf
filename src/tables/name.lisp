;;;; name.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+lang-tag-record-size+ +name-record-size+ +name-tag+ lang-tag-record
   lang-tag-record-lang-tag-offset lang-tag-record-length lang-tag-record-p
   name name-count name-lang-tag-count name-lang-tag-record-count
   name-lang-tag-record-ref name-name-record-count name-name-record-ref name-p
   name-record name-record-encoding-id name-record-language-id
   name-record-length name-record-name-id name-record-p name-record-platform-id
   name-record-string-offset name-storage-offset name-version read-name
   sanitize-name))

(defstruct
    (lang-tag-record (:include record-ref)
     (:constructor %make-lang-tag-record (%record-font-data %record-offset)))
  "Part of [Name]")

(define-constant +lang-tag-record-size+ 4)

(declaim (inline lang-tag-record-length lang-tag-record-lang-tag-offset))

(defun lang-tag-record-length (rec)
  "Language-tag string length (in bytes)"
  (declare (type lang-tag-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun lang-tag-record-lang-tag-offset (rec)
  "Language-tag string offset from start of storage area (in bytes)."
  (declare (type lang-tag-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (name-record (:include record-ref)
     (:constructor %make-name-record (%record-font-data %record-offset)))
  "[Name Records](https://docs.microsoft.com/en-us/typography/opentype/spec/name#name-records)")

(define-constant +name-record-size+ 12)

(declaim
 (inline name-record-platform-id name-record-encoding-id
  name-record-language-id name-record-name-id name-record-length
  name-record-string-offset))

(defun name-record-platform-id (rec)
  "Platform ID."
  (declare (type name-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun name-record-encoding-id (rec)
  "Platform-specific encoding ID."
  (declare (type name-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun name-record-language-id (rec)
  "Language ID."
  (declare (type name-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun name-record-name-id (rec)
  "Name ID."
  (declare (type name-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 6)))

(defun name-record-length (rec)
  "String length (in bytes)."
  (declare (type name-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defun name-record-string-offset (rec)
  "String offset from start of storage area (in bytes)."
  (declare (type name-record rec))
  (font-data-read-u16 (record-ref-font-data rec)
                      (+ (record-ref-offset rec) 10)))

(defstruct
    (name (:include table-ref)
     (:constructor %make-name (%table-font-data %table-offset %table-length)))
  "[Naming table version 1](https://docs.microsoft.com/en-us/typography/opentype/spec/name#naming-table-version-1)")

(declaim
 (inline name-version name-version-raw name-count name-count-raw
  name-storage-offset name-storage-offset-raw name-name-record-count
  name-name-record-ref name-lang-tag-count name-lang-tag-record-count
  name-lang-tag-record-ref))

(define-constant +name-tag+ "name")

(defun name-version (table)
  "Table version number (0 or 1)"
  (declare (type name table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun name-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun name-count (table)
  "Number of name records."
  (declare (type name table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun name-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun name-storage-offset (table)
  "Offset to start of string storage (from start of table)."
  (declare (type name table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun name-storage-offset-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun name-name-record-count (table)
  "The name records where count is the number of records."
  (declare (type name table))
  (declare (ignorable table))
  (name-count table))

(defun name-name-record-ref (table index)
  "The name records where count is the number of records."
  (declare (type name table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 6) (* index 12))))
    (unless
        (<= (+ pos 12) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 12 :length
             (table-ref-length table)))
    (%make-name-record (table-ref-font-data table) pos)))

(defun name-lang-tag-count (table)
  "Number of language-tag records."
  (declare (type name table))
  (when
      (and (>= (name-version table) 1)
           (<=
            (+
             (+ (table-ref-offset table) 6
                (* (name-name-record-count table) 12))
             2)
            (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 6
                           (* (name-name-record-count table) 12)))))

(defun name-lang-tag-record-count (table)
  "The language-tag records where langTagCount is the number of records."
  (declare (type name table))
  (declare (ignorable table))
  (if (and (>= (name-version table) 1)
           (<=
            (+
             (+ (table-ref-offset table) 8
                (* (name-name-record-count table) 12))
             0)
            (+ (table-ref-offset table) (table-ref-length table))))
      (or (name-lang-tag-count table) 0)
      0))

(defun name-lang-tag-record-ref (table index)
  "The language-tag records where langTagCount is the number of records."
  (declare (type name table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 8 (* (name-name-record-count table) 12))
          (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-lang-tag-record (table-ref-font-data table) pos)))

(defun sanitize-name (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  (let* ((cnt (name-count-raw font-data offset)) (pos (+ 6 (* cnt 12))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-name
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-name font-data offset length)
  (%make-name font-data offset length))
