;;;; gasp.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+gasp-range-behavior-gasp-dogray+ +gasp-range-behavior-gasp-gridfit+
   +gasp-range-behavior-gasp-symmetric-gridfit+
   +gasp-range-behavior-gasp-symmetric-smoothing+ +gasp-range-size+ +gasp-tag+
   gasp gasp-gasp-ranges-count gasp-gasp-ranges-ref gasp-num-ranges gasp-p
   gasp-range gasp-range-behavior-gasp-dogray-p
   gasp-range-behavior-gasp-gridfit-p
   gasp-range-behavior-gasp-symmetric-gridfit-p
   gasp-range-behavior-gasp-symmetric-smoothing-p gasp-range-p
   gasp-range-range-gasp-behavior gasp-range-range-max-ppem gasp-version
   read-gasp sanitize-gasp))

(define-constant +gasp-range-behavior-gasp-gridfit+ 1)

(declaim (inline gasp-range-behavior-gasp-gridfit-p))

(defun gasp-range-behavior-gasp-gridfit-p (flags)
  (logtest flags +gasp-range-behavior-gasp-gridfit+))

(define-constant +gasp-range-behavior-gasp-dogray+ 2)

(declaim (inline gasp-range-behavior-gasp-dogray-p))

(defun gasp-range-behavior-gasp-dogray-p (flags)
  (logtest flags +gasp-range-behavior-gasp-dogray+))

(define-constant +gasp-range-behavior-gasp-symmetric-gridfit+ 4)

(declaim (inline gasp-range-behavior-gasp-symmetric-gridfit-p))

(defun gasp-range-behavior-gasp-symmetric-gridfit-p (flags)
  (logtest flags +gasp-range-behavior-gasp-symmetric-gridfit+))

(define-constant +gasp-range-behavior-gasp-symmetric-smoothing+ 8)

(declaim (inline gasp-range-behavior-gasp-symmetric-smoothing-p))

(defun gasp-range-behavior-gasp-symmetric-smoothing-p (flags)
  (logtest flags +gasp-range-behavior-gasp-symmetric-smoothing+))

(defstruct
    (gasp-range (:include record-ref)
     (:constructor %make-gasp-range (%record-font-data %record-offset))))

(define-constant +gasp-range-size+ 4)

(declaim (inline gasp-range-range-max-ppem gasp-range-range-gasp-behavior))

(defun gasp-range-range-max-ppem (rec)
  "Upper limit of range, in PPEM"
  (declare (type gasp-range rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun gasp-range-range-gasp-behavior (rec)
  "Flags describing desired rasterizer behavior."
  (declare (type gasp-range rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (gasp (:include table-ref)
     (:constructor %make-gasp (%table-font-data %table-offset %table-length)))
  "[gasp](https://learn.microsoft.com/en-us/typography/opentype/spec/gasp#gasp-table-formats)")

(declaim
 (inline gasp-version gasp-version-raw gasp-num-ranges gasp-num-ranges-raw
  gasp-gasp-ranges-count gasp-gasp-ranges-ref))

(define-constant +gasp-tag+ "gasp")

(defun gasp-version (table)
  "Version number (set to 1)"
  (declare (type gasp table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun gasp-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun gasp-num-ranges (table)
  "Number of records to follow"
  (declare (type gasp table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun gasp-num-ranges-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun gasp-gasp-ranges-count (table)
  "Sorted by ppem"
  (declare (type gasp table))
  (declare (ignorable table))
  (gasp-num-ranges table))

(defun gasp-gasp-ranges-ref (table index)
  "Sorted by ppem"
  (declare (type gasp table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-gasp-range (table-ref-font-data table) pos)))

(defun sanitize-gasp (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (gasp-num-ranges-raw font-data offset)) (pos (+ 4 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-gasp
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-gasp font-data offset length)
  (%make-gasp font-data offset length))
