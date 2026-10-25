;;;; font.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+table-record-size+ read-table-directory read-ttc-header
   sanitize-table-directory sanitize-ttc-header table-directory
   table-directory-entry-selector table-directory-num-tables table-directory-p
   table-directory-range-shift table-directory-search-range
   table-directory-sfnt-version table-directory-table-records-count
   table-directory-table-records-ref table-record table-record-checksum
   table-record-length table-record-offset table-record-p table-record-tag
   ttc-header ttc-header-dsig-length ttc-header-dsig-offset ttc-header-dsig-tag
   ttc-header-num-fonts ttc-header-p ttc-header-table-directory-offsets-count
   ttc-header-table-directory-offsets-ref ttc-header-ttc-tag
   ttc-header-version))

(defstruct
    (table-record (:include record-ref)
     (:constructor %make-table-record (%record-font-data %record-offset)))
  "Record for a table in a font.")

(define-constant +table-record-size+ 16)

(declaim
 (inline table-record-tag table-record-checksum table-record-offset
  table-record-length))

(defun table-record-tag (rec)
  "Table identifier."
  (declare (type table-record rec))
  (font-data-read-tag-string (record-ref-font-data rec)
                             (record-ref-offset rec)))

(defun table-record-checksum (rec)
  "Checksum for the table."
  (declare (type table-record rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun table-record-offset (rec)
  "Offset from the beginning of the font data."
  (declare (type table-record rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defun table-record-length (rec)
  "Length of the table."
  (declare (type table-record rec))
  (font-data-read-u32 (record-ref-font-data rec)
                      (+ (record-ref-offset rec) 12)))

(defstruct
    (table-directory (:include table-ref)
     (:constructor %make-table-directory
      (%table-font-data %table-offset %table-length)))
  "The OpenType [Table Directory](https://docs.microsoft.com/en-us/typography/opentype/spec/otff#table-directory)")

(declaim
 (inline table-directory-sfnt-version table-directory-sfnt-version-raw
  table-directory-num-tables table-directory-num-tables-raw
  table-directory-search-range table-directory-search-range-raw
  table-directory-entry-selector table-directory-entry-selector-raw
  table-directory-range-shift table-directory-range-shift-raw
  table-directory-table-records-count table-directory-table-records-ref))

(defun table-directory-sfnt-version (table)
  "0x00010000 or 0x4F54544F"
  (declare (type table-directory table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun table-directory-sfnt-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun table-directory-num-tables (table)
  "Number of tables."
  (declare (type table-directory table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun table-directory-num-tables-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun table-directory-search-range (table)
  (declare (type table-directory table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun table-directory-search-range-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun table-directory-entry-selector (table)
  (declare (type table-directory table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun table-directory-entry-selector-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun table-directory-range-shift (table)
  (declare (type table-directory table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun table-directory-range-shift-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun table-directory-table-records-count (table)
  "Table records array—one for each top-level table in the font"
  (declare (type table-directory table))
  (declare (ignorable table))
  (table-directory-num-tables table))

(defun table-directory-table-records-ref (table index)
  "Table records array—one for each top-level table in the font"
  (declare (type table-directory table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 16))))
    (unless
        (<= (+ pos 16) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 16 :length
             (table-ref-length table)))
    (%make-table-record (table-ref-font-data table) pos)))

(defun sanitize-table-directory (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  (let* ((cnt (table-directory-num-tables-raw font-data offset))
         (pos (+ 12 (* cnt 16))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-table-directory
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-table-directory font-data offset length)
  (%make-table-directory font-data offset length))

(defstruct
    (ttc-header (:include table-ref)
     (:constructor %make-ttc-header
      (%table-font-data %table-offset %table-length)))
  "[TTC Header](https://learn.microsoft.com/en-us/typography/opentype/spec/otff#ttc-header)")

(declaim
 (inline ttc-header-ttc-tag ttc-header-ttc-tag-raw ttc-header-version
  ttc-header-version-raw ttc-header-num-fonts ttc-header-num-fonts-raw
  ttc-header-table-directory-offsets-count
  ttc-header-table-directory-offsets-ref ttc-header-dsig-tag
  ttc-header-dsig-length ttc-header-dsig-offset))

(defun ttc-header-ttc-tag (table)
  "Font Collection ID string: \"ttcf\""
  (declare (type ttc-header table))
  (font-data-read-tag-string (table-ref-font-data table)
                             (table-ref-offset table)))

(defun ttc-header-ttc-tag-raw (font-data offset)
  (font-data-read-tag-string font-data offset))

(defun ttc-header-version (table)
  "Major/minor version of the TTC Header"
  (declare (type ttc-header table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun ttc-header-version-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun ttc-header-num-fonts (table)
  "Number of fonts in TTC"
  (declare (type ttc-header table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun ttc-header-num-fonts-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun ttc-header-table-directory-offsets-count (table)
  "Array of offsets to the TableDirectory for each font from the beginning of the file"
  (declare (type ttc-header table))
  (declare (ignorable table))
  (ttc-header-num-fonts table))

(defun ttc-header-table-directory-offsets-ref (table index)
  "Array of offsets to the TableDirectory for each font from the beginning of the file"
  (declare (type ttc-header table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (font-data-read-u32 (table-ref-font-data table) pos)))

(defun ttc-header-dsig-tag (table)
  "Tag indicating that a DSIG table exists, 0x44534947 ('DSIG') (null if no signature)"
  (declare (type ttc-header table))
  (when
      (and (>= (ttc-header-version table) 131072)
           (<=
            (+
             (+ (table-ref-offset table) 12
                (* (ttc-header-table-directory-offsets-count table) 4))
             4)
            (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 12
                           (* (ttc-header-table-directory-offsets-count table)
                              4)))))

(defun ttc-header-dsig-length (table)
  "The length (in bytes) of the DSIG table (null if no signature)"
  (declare (type ttc-header table))
  (when
      (and (>= (ttc-header-version table) 131072)
           (<=
            (+
             (+ (table-ref-offset table) 16
                (* (ttc-header-table-directory-offsets-count table) 4))
             4)
            (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 16
                           (* (ttc-header-table-directory-offsets-count table)
                              4)))))

(defun ttc-header-dsig-offset (table)
  "The offset (in bytes) of the DSIG table from the beginning of the TTC file (null if no signature)"
  (declare (type ttc-header table))
  (when
      (and (>= (ttc-header-version table) 131072)
           (<=
            (+
             (+ (table-ref-offset table) 20
                (* (ttc-header-table-directory-offsets-count table) 4))
             4)
            (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 20
                           (* (ttc-header-table-directory-offsets-count table)
                              4)))))

(defun sanitize-ttc-header (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  (let* ((cnt (ttc-header-num-fonts-raw font-data offset))
         (pos (+ 12 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-ttc-header
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ttc-header font-data offset length)
  (%make-ttc-header font-data offset length))
