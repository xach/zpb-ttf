;;;; vmtx.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+vmtx-extended-tag+ +vmtx-tag+ read-vmtx read-vmtx-extended sanitize-vmtx
   sanitize-vmtx-extended vmtx vmtx-extended vmtx-extended-p
   vmtx-extended-top-side-bearings-count vmtx-extended-top-side-bearings-ref
   vmtx-extended-v-metrics-count vmtx-extended-v-metrics-ref vmtx-p
   vmtx-top-side-bearings-count vmtx-top-side-bearings-ref vmtx-v-metrics-count
   vmtx-v-metrics-ref))

(defstruct
    (vmtx (:include table-ref)
     (:constructor %make-vmtx (%table-font-data %table-offset %table-length)))
  "The [vmtx (Vertical Metrics)](https://docs.microsoft.com/en-us/typography/opentype/spec/vmtx) table")

(declaim
 (inline vmtx-v-metrics-count vmtx-v-metrics-ref vmtx-top-side-bearings-count
  vmtx-top-side-bearings-ref))

(define-constant +vmtx-tag+ "vmtx")

(defun vmtx-v-metrics-count (table number-of-long-ver-metrics)
  "Paired advance height and top side bearing values for each glyph. Records are indexed by glyph ID."
  (declare (type vmtx table)
           (ignorable number-of-long-ver-metrics))
  (declare (ignorable table))
  number-of-long-ver-metrics)

(defun vmtx-v-metrics-ref (table number-of-long-ver-metrics index)
  "Paired advance height and top side bearing values for each glyph. Records are indexed by glyph ID."
  (declare (type vmtx table)
           (ignorable number-of-long-ver-metrics index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-long-metric (table-ref-font-data table) pos)))

(defun vmtx-top-side-bearings-count (table number-of-long-ver-metrics)
  "Top side bearings for glyph IDs greater than or equal to numberOfLongMetrics."
  (declare (type vmtx table)
           (ignorable number-of-long-ver-metrics))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table)
              (* (vmtx-v-metrics-count table number-of-long-ver-metrics) 4)))
        2)))

(defun vmtx-top-side-bearings-ref (table number-of-long-ver-metrics index)
  "Top side bearings for glyph IDs greater than or equal to numberOfLongMetrics."
  (declare (type vmtx table)
           (ignorable number-of-long-ver-metrics index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table)
             (* (vmtx-v-metrics-count table number-of-long-ver-metrics) 4))
          (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-i16 (table-ref-font-data table) pos)))

(defun sanitize-vmtx (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-vmtx
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-vmtx font-data offset length)
  (%make-vmtx font-data offset length))

(defstruct
    (vmtx-extended (:include table-ref)
     (:constructor %make-vmtx-extended
      (%table-font-data %table-offset %table-length)))
  "VMTX vertical metrics (ISO/IEC 14496-22:2026, 5.6.14).")

(declaim
 (inline vmtx-extended-v-metrics-count vmtx-extended-v-metrics-ref
  vmtx-extended-top-side-bearings-count vmtx-extended-top-side-bearings-ref))

(define-constant +vmtx-extended-tag+ "VMTX")

(defun vmtx-extended-v-metrics-count (table number-of-long-ver-metrics)
  (declare (type vmtx-extended table)
           (ignorable number-of-long-ver-metrics))
  (declare (ignorable table))
  number-of-long-ver-metrics)

(defun vmtx-extended-v-metrics-ref (table number-of-long-ver-metrics index)
  (declare (type vmtx-extended table)
           (ignorable number-of-long-ver-metrics index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-long-metric (table-ref-font-data table) pos)))

(defun vmtx-extended-top-side-bearings-count (table number-of-long-ver-metrics)
  (declare (type vmtx-extended table)
           (ignorable number-of-long-ver-metrics))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table)
              (*
               (vmtx-extended-v-metrics-count table number-of-long-ver-metrics)
               4)))
        2)))

(defun vmtx-extended-top-side-bearings-ref
       (table number-of-long-ver-metrics index)
  (declare (type vmtx-extended table)
           (ignorable number-of-long-ver-metrics index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table)
             (*
              (vmtx-extended-v-metrics-count table number-of-long-ver-metrics)
              4))
          (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-i16 (table-ref-font-data table) pos)))

(defun sanitize-vmtx-extended (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-vmtx-extended
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-vmtx-extended font-data offset length)
  (%make-vmtx-extended font-data offset length))
