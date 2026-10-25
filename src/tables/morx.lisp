;;;; morx.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+contextual-entry-data-size+ +insertion-entry-data-size+ +morx-feature-size+
   +morx-tag+ chain chain-chain-length chain-default-flags chain-features-count
   chain-features-ref chain-n-feature-entries chain-n-subtables chain-p
   chain-subtables-start contextual-entry-data
   contextual-entry-data-current-index contextual-entry-data-mark-index
   contextual-entry-data-p insertion-entry-data
   insertion-entry-data-current-insert-index
   insertion-entry-data-marked-insert-index insertion-entry-data-p morx
   morx-chains-start morx-feature morx-feature-disable-flags
   morx-feature-enable-flags morx-feature-feature-settings
   morx-feature-feature-type morx-feature-p morx-n-chains morx-p morx-unused
   morx-version read-chain read-morx read-subtable sanitize-chain sanitize-morx
   sanitize-subtable subtable subtable-coverage subtable-data-bytes
   subtable-data-count subtable-data-ref subtable-length subtable-p
   subtable-sub-feature-flags))

(defstruct
    (morx-feature (:include record-ref)
     (:constructor %make-morx-feature (%record-font-data %record-offset)))
  "Used to compute the sub-feature flags for a list of requested features and settings.")

(define-constant +morx-feature-size+ 12)

(declaim
 (inline morx-feature-feature-type morx-feature-feature-settings
  morx-feature-enable-flags morx-feature-disable-flags))

(defun morx-feature-feature-type (rec)
  "The type of feature."
  (declare (type morx-feature rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun morx-feature-feature-settings (rec)
  "The feature's setting (aka selector)."
  (declare (type morx-feature rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun morx-feature-enable-flags (rec)
  "Flags for the settings that this feature and setting enables."
  (declare (type morx-feature rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun morx-feature-disable-flags (rec)
  "Complement of flags for the settings that this feature and setting disable."
  (declare (type morx-feature rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defstruct
    (contextual-entry-data (:include record-ref)
     (:constructor %make-contextual-entry-data
      (%record-font-data %record-offset)))
  "Entry payload in a contextual subtable state machine.")

(define-constant +contextual-entry-data-size+ 4)

(declaim
 (inline contextual-entry-data-mark-index contextual-entry-data-current-index))

(defun contextual-entry-data-mark-index (rec)
  "Index of the substitution table for the marked glyph (use 0xFFFF for none)."
  (declare (type contextual-entry-data rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun contextual-entry-data-current-index (rec)
  "Index of the substitution table for the current glyph (use 0xFFFF for none)"
  (declare (type contextual-entry-data rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (insertion-entry-data (:include record-ref)
     (:constructor %make-insertion-entry-data
      (%record-font-data %record-offset)))
  "Entry payload in an insertion subtable state machine.")

(define-constant +insertion-entry-data-size+ 4)

(declaim
 (inline insertion-entry-data-current-insert-index
  insertion-entry-data-marked-insert-index))

(defun insertion-entry-data-current-insert-index (rec)
  "Zero-based index into the insertion glyph table. The number of glyphs to be inserted is contained in the currentInsertCount field in the flags (see below). A value of 0xFFFF indicates no insertion is to be done."
  (declare (type insertion-entry-data rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun insertion-entry-data-marked-insert-index (rec)
  "Zero-based index into the insertion glyph table. The number of glyphs to be inserted is contained in the markedInsertCount field in the flags (see below). A value of 0xFFFF indicates no insertion is to be done."
  (declare (type insertion-entry-data rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (morx (:include table-ref)
     (:constructor %make-morx (%table-font-data %table-offset %table-length)))
  "The [morx (Extended Glyph Metamorphosis)](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6morx.html) table.")

(declaim
 (inline morx-version morx-version-raw morx-unused morx-unused-raw
  morx-n-chains morx-n-chains-raw morx-chains-start))

(define-constant +morx-tag+ "morx")

(defun morx-version (table)
  "Version number of the extended glyph metamorphosis table (either 2 or 3)."
  (declare (type morx table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun morx-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun morx-unused (table)
  "Not used; set to 0."
  (declare (type morx table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun morx-unused-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun morx-n-chains (table)
  "Number of metamorphosis chains contained in this table."
  (declare (type morx table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun morx-n-chains-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun morx-chains-start (table)
  "Byte position of the chains field; its elements are variable-length CHAIN data."
  (declare (type morx table))
  (+ (table-ref-offset table) 8))

(defun sanitize-morx (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-morx
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-morx font-data offset length)
  (%make-morx font-data offset length))

(defstruct
    (chain (:include table-ref)
     (:constructor %make-chain (%table-font-data %table-offset %table-length)))
  "A chain in a `morx` table.")

(declaim
 (inline chain-default-flags chain-default-flags-raw chain-chain-length
  chain-chain-length-raw chain-n-feature-entries chain-n-feature-entries-raw
  chain-n-subtables chain-n-subtables-raw chain-features-count
  chain-features-ref chain-subtables-start))

(defun chain-default-flags (table)
  "The default specification for subtables."
  (declare (type chain table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun chain-default-flags-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun chain-chain-length (table)
  "Total byte count, including this header; must be a multiple of 4."
  (declare (type chain table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun chain-chain-length-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun chain-n-feature-entries (table)
  "Number of feature subtable entries."
  (declare (type chain table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun chain-n-feature-entries-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun chain-n-subtables (table)
  "The number of subtables in the chain."
  (declare (type chain table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun chain-n-subtables-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 12)))

(defun chain-features-count (table)
  "Feature entries for this chain."
  (declare (type chain table))
  (declare (ignorable table))
  (chain-n-feature-entries table))

(defun chain-features-ref (table index)
  "Feature entries for this chain."
  (declare (type chain table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 16) (* index 12))))
    (unless
        (<= (+ pos 12) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 12 :length
             (table-ref-length table)))
    (%make-morx-feature (table-ref-font-data table) pos)))

(defun chain-subtables-start (table)
  "Byte position of the subtables field; its elements are variable-length SUBTABLE data."
  (declare (type chain table))
  (+ (table-ref-offset table) 16 (* (chain-features-count table) 12)))

(defun sanitize-chain (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  (let* ((cnt (chain-n-feature-entries-raw font-data offset))
         (pos (+ 16 (* cnt 12))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-chain
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-chain font-data offset length)
  (%make-chain font-data offset length))

(defstruct
    (subtable (:include table-ref)
     (:constructor %make-subtable
      (%table-font-data %table-offset %table-length)))
  "A subtable in a `morx` chain.")

(declaim
 (inline subtable-length subtable-length-raw subtable-coverage
  subtable-coverage-raw subtable-sub-feature-flags
  subtable-sub-feature-flags-raw subtable-data-count subtable-data-ref
  subtable-data-bytes))

(defun subtable-length (table)
  "Total subtable length, including this header."
  (declare (type subtable table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun subtable-length-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun subtable-coverage (table)
  "Coverage flags and subtable type."
  (declare (type subtable table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun subtable-coverage-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun subtable-sub-feature-flags (table)
  "The 32-bit mask identifying which subtable this is (the subtable being executed if the AND of this value and the processed defaultFlags is nonzero)."
  (declare (type subtable table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun subtable-sub-feature-flags-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun subtable-data-count (table)
  "Data for specific subtable."
  (declare (type subtable table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 12))
        1)))

(defun subtable-data-ref (table index)
  "Data for specific subtable."
  (declare (type subtable table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 12) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun subtable-data-bytes (table)
  "Data for specific subtable."
  (declare (type subtable table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 12)
                   (subtable-data-count table)))

(defun sanitize-subtable (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-subtable
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-subtable font-data offset length)
  (%make-subtable font-data offset length))
