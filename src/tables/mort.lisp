;;;; mort.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+mort-contextual-entry-data-size+ +mort-feature-size+
   +mort-insertion-entry-data-size+ +mort-tag+ mort mort-chain
   mort-chain-chain-length mort-chain-default-flags mort-chain-features-count
   mort-chain-features-ref mort-chain-n-feature-entries mort-chain-n-subtables
   mort-chain-p mort-chain-subtables-start mort-chains-start
   mort-contextual-entry-data mort-contextual-entry-data-current-offset
   mort-contextual-entry-data-mark-offset mort-contextual-entry-data-p
   mort-feature mort-feature-disable-flags mort-feature-enable-flags
   mort-feature-feature-settings mort-feature-feature-type mort-feature-p
   mort-insertion-entry-data mort-insertion-entry-data-current-insert-index
   mort-insertion-entry-data-marked-insert-index mort-insertion-entry-data-p
   mort-n-chains mort-p mort-subtable mort-subtable-coverage
   mort-subtable-data-bytes mort-subtable-data-count mort-subtable-data-ref
   mort-subtable-length mort-subtable-p mort-subtable-sub-feature-flags
   mort-unused mort-version read-mort read-mort-chain read-mort-subtable
   sanitize-mort sanitize-mort-chain sanitize-mort-subtable))

(defstruct
    (mort-feature (:include record-ref)
     (:constructor %make-mort-feature (%record-font-data %record-offset)))
  "Used to compute the sub-feature flags for a list of requested features and settings.")

(define-constant +mort-feature-size+ 12)

(declaim
 (inline mort-feature-feature-type mort-feature-feature-settings
  mort-feature-enable-flags mort-feature-disable-flags))

(defun mort-feature-feature-type (rec)
  "The type of feature."
  (declare (type mort-feature rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun mort-feature-feature-settings (rec)
  "The feature's setting (aka selector)."
  (declare (type mort-feature rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun mort-feature-enable-flags (rec)
  "Flags for the settings that this feature and setting enables."
  (declare (type mort-feature rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun mort-feature-disable-flags (rec)
  "Complement of flags for the settings that this feature and setting disable."
  (declare (type mort-feature rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defstruct
    (mort-contextual-entry-data (:include record-ref)
     (:constructor %make-mort-contextual-entry-data
      (%record-font-data %record-offset)))
  "Entry payload in a contextual subtable state machine.")

(define-constant +mort-contextual-entry-data-size+ 4)

(declaim
 (inline mort-contextual-entry-data-mark-offset
  mort-contextual-entry-data-current-offset))

(defun mort-contextual-entry-data-mark-offset (rec)
  "Signed word offset of the substitution array for the marked glyph (0 for none)."
  (declare (type mort-contextual-entry-data rec))
  (font-data-read-i16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun mort-contextual-entry-data-current-offset (rec)
  "Signed word offset of the substitution array for the current glyph (0 for none)."
  (declare (type mort-contextual-entry-data rec))
  (font-data-read-i16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (mort-insertion-entry-data (:include record-ref)
     (:constructor %make-mort-insertion-entry-data
      (%record-font-data %record-offset)))
  "Entry payload in an insertion subtable state machine.")

(define-constant +mort-insertion-entry-data-size+ 4)

(declaim
 (inline mort-insertion-entry-data-current-insert-index
  mort-insertion-entry-data-marked-insert-index))

(defun mort-insertion-entry-data-current-insert-index (rec)
  "Zero-based index into the insertion glyph table (0xFFFF for none)."
  (declare (type mort-insertion-entry-data rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun mort-insertion-entry-data-marked-insert-index (rec)
  "Zero-based index into the insertion glyph table (0xFFFF for none)."
  (declare (type mort-insertion-entry-data rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (mort (:include table-ref)
     (:constructor %make-mort (%table-font-data %table-offset %table-length)))
  "The [mort (Glyph Metamorphosis)](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6mort.html) table.")

(declaim
 (inline mort-version mort-version-raw mort-unused mort-unused-raw
  mort-n-chains mort-n-chains-raw mort-chains-start))

(define-constant +mort-tag+ "mort")

(defun mort-version (table)
  "Version number of the glyph metamorphosis table."
  (declare (type mort table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mort-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mort-unused (table)
  "Not used; set to 0."
  (declare (type mort table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun mort-unused-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun mort-n-chains (table)
  "Number of metamorphosis chains contained in this table."
  (declare (type mort table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun mort-n-chains-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun mort-chains-start (table)
  "Byte position of the chains field; its elements are variable-length MORT-CHAIN data."
  (declare (type mort table))
  (+ (table-ref-offset table) 8))

(defun sanitize-mort (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-mort
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mort font-data offset length)
  (%make-mort font-data offset length))

(defstruct
    (mort-chain (:include table-ref)
     (:constructor %make-mort-chain
      (%table-font-data %table-offset %table-length)))
  "A chain in a `mort` table.")

(declaim
 (inline mort-chain-default-flags mort-chain-default-flags-raw
  mort-chain-chain-length mort-chain-chain-length-raw
  mort-chain-n-feature-entries mort-chain-n-feature-entries-raw
  mort-chain-n-subtables mort-chain-n-subtables-raw mort-chain-features-count
  mort-chain-features-ref mort-chain-subtables-start))

(defun mort-chain-default-flags (table)
  "The default specification for subtables."
  (declare (type mort-chain table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun mort-chain-default-flags-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun mort-chain-chain-length (table)
  "Total byte count, including this header."
  (declare (type mort-chain table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun mort-chain-chain-length-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun mort-chain-n-feature-entries (table)
  "Number of feature subtable entries."
  (declare (type mort-chain table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun mort-chain-n-feature-entries-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun mort-chain-n-subtables (table)
  "The number of subtables in the chain."
  (declare (type mort-chain table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun mort-chain-n-subtables-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun mort-chain-features-count (table)
  "Feature entries for this chain."
  (declare (type mort-chain table))
  (declare (ignorable table))
  (mort-chain-n-feature-entries table))

(defun mort-chain-features-ref (table index)
  "Feature entries for this chain."
  (declare (type mort-chain table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 12))))
    (unless
        (<= (+ pos 12) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 12 :length
             (table-ref-length table)))
    (%make-mort-feature (table-ref-font-data table) pos)))

(defun mort-chain-subtables-start (table)
  "Byte position of the subtables field; its elements are variable-length MORT-SUBTABLE data."
  (declare (type mort-chain table))
  (+ (table-ref-offset table) 12 (* (mort-chain-features-count table) 12)))

(defun sanitize-mort-chain (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  (let* ((cnt (mort-chain-n-feature-entries-raw font-data offset))
         (pos (+ 12 (* cnt 12))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-mort-chain
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mort-chain font-data offset length)
  (%make-mort-chain font-data offset length))

(defstruct
    (mort-subtable (:include table-ref)
     (:constructor %make-mort-subtable
      (%table-font-data %table-offset %table-length)))
  "A subtable in a `mort` chain.")

(declaim
 (inline mort-subtable-length mort-subtable-length-raw mort-subtable-coverage
  mort-subtable-coverage-raw mort-subtable-sub-feature-flags
  mort-subtable-sub-feature-flags-raw mort-subtable-data-count
  mort-subtable-data-ref mort-subtable-data-bytes))

(defun mort-subtable-length (table)
  "Total subtable length, including this header."
  (declare (type mort-subtable table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mort-subtable-length-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mort-subtable-coverage (table)
  "Coverage flags and subtable type."
  (declare (type mort-subtable table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun mort-subtable-coverage-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun mort-subtable-sub-feature-flags (table)
  "The 32-bit mask identifying which subtable this is."
  (declare (type mort-subtable table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun mort-subtable-sub-feature-flags-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun mort-subtable-data-count (table)
  "Data for the specific subtable type."
  (declare (type mort-subtable table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 8))
        1)))

(defun mort-subtable-data-ref (table index)
  "Data for the specific subtable type."
  (declare (type mort-subtable table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun mort-subtable-data-bytes (table)
  "Data for the specific subtable type."
  (declare (type mort-subtable table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 8)
                   (mort-subtable-data-count table)))

(defun sanitize-mort-subtable (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-mort-subtable
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mort-subtable font-data offset length)
  (%make-mort-subtable font-data offset length))
