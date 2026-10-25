;;;; svg.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+svg-document-record-size+ +svg-tag+ read-svg read-svg-document-list
   sanitize-svg sanitize-svg-document-list svg svg--reserved svg-document-list
   svg-document-list-document-records-count
   svg-document-list-document-records-ref svg-document-list-num-entries
   svg-document-list-p svg-document-record svg-document-record-end-glyph-id
   svg-document-record-p svg-document-record-start-glyph-id
   svg-document-record-svg-doc-length svg-document-record-svg-doc-offset svg-p
   svg-svg-document-list-offset svg-svg-document-list-offset-offset
   svg-version))

(defstruct
    (svg-document-record (:include record-ref)
     (:constructor %make-svg-document-record
      (%record-font-data %record-offset)))
  "[SVGDocumentRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/svg)")

(define-constant +svg-document-record-size+ 12)

(declaim
 (inline svg-document-record-start-glyph-id svg-document-record-end-glyph-id
  svg-document-record-svg-doc-offset svg-document-record-svg-doc-length))

(defun svg-document-record-start-glyph-id (rec)
  "The first glyph ID for the range covered by this record."
  (declare (type svg-document-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun svg-document-record-end-glyph-id (rec)
  "The last glyph ID for the range covered by this record."
  (declare (type svg-document-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun svg-document-record-svg-doc-offset (rec)
  "Offset from the beginning of the SVGDocumentList to an SVG document. Must be non-zero."
  (declare (type svg-document-record rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun svg-document-record-svg-doc-length (rec)
  "Length of the SVG document data. Must be non-zero."
  (declare (type svg-document-record rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defstruct
    (svg (:include table-ref)
     (:constructor %make-svg (%table-font-data %table-offset %table-length)))
  "The [SVG](https://learn.microsoft.com/en-us/typography/opentype/spec/svg) table")

(declaim
 (inline svg-version svg-version-raw svg-svg-document-list-offset-offset
  svg-svg-document-list-offset svg--reserved svg--reserved-raw))

(define-constant +svg-tag+ "SVG ")

(defun svg-version (table)
  "Table version (starting at 0). Set to 0."
  (declare (type svg table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun svg-version-raw (font-data offset) (font-data-read-u16 font-data offset))

(defun svg-svg-document-list-offset-offset (table)
  "Offset to the SVGDocumentList, from the start of the SVG table. Must be non-zero."
  (declare (type svg table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun svg-svg-document-list-offset (table)
  "Offset to the SVGDocumentList, from the start of the SVG table. Must be non-zero."
  (declare (type svg table))
  (let ((off (svg-svg-document-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-svg-document-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun svg--reserved (table)
  "Set to 0."
  (declare (type svg table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun svg--reserved-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun sanitize-svg (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-svg
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-svg font-data offset length)
  (%make-svg font-data offset length))

(defstruct
    (svg-document-list (:include table-ref)
     (:constructor %make-svg-document-list
      (%table-font-data %table-offset %table-length)))
  "[SVGDocumentList](https://learn.microsoft.com/en-us/typography/opentype/spec/svg)")

(declaim
 (inline svg-document-list-num-entries svg-document-list-num-entries-raw
  svg-document-list-document-records-count
  svg-document-list-document-records-ref))

(defun svg-document-list-num-entries (table)
  "Number of SVGDocumentRecords. Must be non-zero."
  (declare (type svg-document-list table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun svg-document-list-num-entries-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun svg-document-list-document-records-count (table)
  "Array of SVGDocumentRecords."
  (declare (type svg-document-list table))
  (declare (ignorable table))
  (svg-document-list-num-entries table))

(defun svg-document-list-document-records-ref (table index)
  "Array of SVGDocumentRecords."
  (declare (type svg-document-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 12))))
    (unless
        (<= (+ pos 12) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 12 :length
             (table-ref-length table)))
    (%make-svg-document-record (table-ref-font-data table) pos)))

(defun sanitize-svg-document-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  (let* ((cnt (svg-document-list-num-entries-raw font-data offset))
         (pos (+ 2 (* cnt 12))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-svg-document-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-svg-document-list font-data offset length)
  (%make-svg-document-list font-data offset length))
