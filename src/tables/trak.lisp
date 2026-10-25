;;;; trak.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+track-table-entry-size+ +trak-tag+ read-track-data read-trak
   sanitize-track-data sanitize-trak track-data track-data-n-sizes
   track-data-n-tracks track-data-p track-data-size-table-offset
   track-data-track-table-count track-data-track-table-ref track-table-entry
   track-table-entry-name-index track-table-entry-offset track-table-entry-p
   track-table-entry-track trak trak-format-table trak-horiz-offset
   trak-horiz-offset-offset trak-p trak-reserved trak-version trak-vert-offset
   trak-vert-offset-offset))

(defstruct
    (track-table-entry (:include record-ref)
     (:constructor %make-track-table-entry (%record-font-data %record-offset)))
  "Single entry in a tracking table.")

(define-constant +track-table-entry-size+ 8)

(declaim
 (inline track-table-entry-track track-table-entry-name-index
  track-table-entry-offset))

(defun track-table-entry-track (rec)
  "Track value for this record."
  (declare (type track-table-entry rec))
  (font-data-read-fixed (record-ref-font-data rec) (record-ref-offset rec)))

(defun track-table-entry-name-index (rec)
  "The 'name' table index for this track (a short word or phrase like \"loose\" or \"very tight\"). NameIndex has a value greater than 255 and less than 32768."
  (declare (type track-table-entry rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun track-table-entry-offset (rec)
  "Offset from the start of the tracking table to per-size tracking values for this track."
  (declare (type track-table-entry rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 6)))

(defstruct
    (trak (:include table-ref)
     (:constructor %make-trak (%table-font-data %table-offset %table-length)))
  "The [tracking (trak)](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6trak.html) table.")

(declaim
 (inline trak-version trak-version-raw trak-format-table trak-format-table-raw
  trak-horiz-offset-offset trak-horiz-offset trak-vert-offset-offset
  trak-vert-offset trak-reserved trak-reserved-raw))

(define-constant +trak-tag+ "trak")

(defun trak-version (table)
  "Version number of the tracking table (0x00010000 for the current version)."
  (declare (type trak table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun trak-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun trak-format-table (table)
  "Format of the tracking table (set to 0)."
  (declare (type trak table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun trak-format-table-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun trak-horiz-offset-offset (table)
  "Offset from start of tracking table to TrackData for horizontal text (or 0 if none)."
  (declare (type trak table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun trak-horiz-offset (table)
  "Offset from start of tracking table to TrackData for horizontal text (or 0 if none)."
  (declare (type trak table))
  (let ((off (trak-horiz-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-track-data (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun trak-vert-offset-offset (table)
  "Offset from start of tracking table to TrackData for vertical text (or 0 if none)."
  (declare (type trak table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun trak-vert-offset (table)
  "Offset from start of tracking table to TrackData for vertical text (or 0 if none)."
  (declare (type trak table))
  (let ((off (trak-vert-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-track-data (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun trak-reserved (table)
  "Reserved. Set to 0."
  (declare (type trak table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun trak-reserved-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun sanitize-trak (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-trak
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-trak font-data offset length)
  (%make-trak font-data offset length))

(defstruct
    (track-data (:include table-ref)
     (:constructor %make-track-data
      (%table-font-data %table-offset %table-length)))
  "The tracking data table.")

(declaim
 (inline track-data-n-tracks track-data-n-tracks-raw track-data-n-sizes
  track-data-n-sizes-raw track-data-size-table-offset
  track-data-size-table-offset-raw track-data-track-table-count
  track-data-track-table-ref))

(defun track-data-n-tracks (table)
  "Number of separate tracks included in this table."
  (declare (type track-data table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun track-data-n-tracks-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun track-data-n-sizes (table)
  "Number of point sizes included in this table."
  (declare (type track-data table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun track-data-n-sizes-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun track-data-size-table-offset (table)
  "Offset from the start of the tracking table to the start of the size subtable."
  (declare (type track-data table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun track-data-size-table-offset-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun track-data-track-table-count (table)
  "Array of TrackTableEntry records."
  (declare (type track-data table))
  (declare (ignorable table))
  (track-data-n-tracks table))

(defun track-data-track-table-ref (table index)
  "Array of TrackTableEntry records."
  (declare (type track-data table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 8))))
    (unless
        (<= (+ pos 8) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 8 :length
             (table-ref-length table)))
    (%make-track-table-entry (table-ref-font-data table) pos)))

(defun sanitize-track-data (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  (let* ((cnt (track-data-n-tracks-raw font-data offset)) (pos (+ 8 (* cnt 8))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-track-data
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-track-data font-data offset length)
  (%make-track-data font-data offset length))
