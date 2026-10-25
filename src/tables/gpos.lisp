;;;; gpos.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+entry-exit-record-size+ +entry-exit-record2-size+ +gpos-tag+
   +mark-record-size+ +mark-record2-size+ +value-format-x-advance+
   +value-format-x-advance-device+ +value-format-x-placement+
   +value-format-x-placement-device+ +value-format-y-advance+
   +value-format-y-advance-device+ +value-format-y-placement+
   +value-format-y-placement-device+ anchor-format1
   anchor-format1-anchor-format anchor-format1-p anchor-format1-x-coordinate
   anchor-format1-y-coordinate anchor-format2 anchor-format2-anchor-format
   anchor-format2-anchor-point anchor-format2-p anchor-format2-x-coordinate
   anchor-format2-y-coordinate anchor-format3 anchor-format3-anchor-format
   anchor-format3-p anchor-format3-x-coordinate anchor-format3-x-device-offset
   anchor-format3-x-device-offset-offset anchor-format3-y-coordinate
   anchor-format3-y-device-offset anchor-format3-y-device-offset-offset
   base-array base-array-base-count base-array-base-records-start base-array-p
   base-array2 base-array2-base-count base-array2-base-records-start
   base-array2-p base-record base-record-base-anchor-offsets-count
   base-record-base-anchor-offsets-ref base-record-p base-record2
   base-record2-base-anchor-offsets-count base-record2-base-anchor-offsets-ref
   base-record2-p class1-record class1-record-class2-records-start
   class1-record-p class2-record class2-record-p
   class2-record-value-record1-start component-record
   component-record-ligature-anchor-offsets-count
   component-record-ligature-anchor-offsets-ref component-record-p
   component-record2 component-record2-ligature-anchor-offsets-count
   component-record2-ligature-anchor-offsets-ref component-record2-p
   cursive-pos-format1 cursive-pos-format1-coverage-offset
   cursive-pos-format1-coverage-offset-offset
   cursive-pos-format1-entry-exit-count
   cursive-pos-format1-entry-exit-record-count
   cursive-pos-format1-entry-exit-record-ref cursive-pos-format1-p
   cursive-pos-format1-pos-format cursive-pos-format2
   cursive-pos-format2-coverage-offset
   cursive-pos-format2-coverage-offset-offset
   cursive-pos-format2-entry-exit-count
   cursive-pos-format2-entry-exit-record-count
   cursive-pos-format2-entry-exit-record-ref cursive-pos-format2-p
   cursive-pos-format2-pos-format entry-exit-record
   entry-exit-record-entry-anchor-offset entry-exit-record-exit-anchor-offset
   entry-exit-record-p entry-exit-record2
   entry-exit-record2-entry-anchor-offset entry-exit-record2-exit-anchor-offset
   entry-exit-record2-p extension-pos-format1
   extension-pos-format1-extension-lookup-type
   extension-pos-format1-extension-offset
   extension-pos-format1-extension-offset-offset extension-pos-format1-p
   extension-pos-format1-pos-format gpos gpos-feature-list-offset
   gpos-feature-list-offset-offset gpos-feature-list2-offset
   gpos-feature-list2-offset-offset gpos-feature-variations-offset
   gpos-feature-variations-offset-offset gpos-lookup-list-offset
   gpos-lookup-list-offset-offset gpos-lookup-list2-offset
   gpos-lookup-list2-offset-offset gpos-p gpos-script-list-offset
   gpos-script-list-offset-offset gpos-script-list2-offset
   gpos-script-list2-offset-offset gpos-version ligature-array
   ligature-array-ligature-attach-offsets-count
   ligature-array-ligature-attach-offsets-ref ligature-array-ligature-count
   ligature-array-p ligature-array2
   ligature-array2-ligature-attach-offsets-count
   ligature-array2-ligature-attach-offsets-ref ligature-array2-ligature-count
   ligature-array2-p ligature-attach ligature-attach-component-count
   ligature-attach-component-records-start ligature-attach-p ligature-attach2
   ligature-attach2-component-count ligature-attach2-component-records-start
   ligature-attach2-p mark-array mark-array-mark-count
   mark-array-mark-records-count mark-array-mark-records-ref mark-array-p
   mark-array2 mark-array2-mark-count mark-array2-mark-records-count
   mark-array2-mark-records-ref mark-array2-p mark-base-pos-format1
   mark-base-pos-format1-base-array-offset
   mark-base-pos-format1-base-array-offset-offset
   mark-base-pos-format1-base-coverage-offset
   mark-base-pos-format1-base-coverage-offset-offset
   mark-base-pos-format1-mark-array-offset
   mark-base-pos-format1-mark-array-offset-offset
   mark-base-pos-format1-mark-class-count
   mark-base-pos-format1-mark-coverage-offset
   mark-base-pos-format1-mark-coverage-offset-offset mark-base-pos-format1-p
   mark-base-pos-format1-pos-format mark-base-pos-format2
   mark-base-pos-format2-base-array-offset
   mark-base-pos-format2-base-array-offset-offset
   mark-base-pos-format2-base-coverage-offset
   mark-base-pos-format2-base-coverage-offset-offset
   mark-base-pos-format2-mark-array-offset
   mark-base-pos-format2-mark-array-offset-offset
   mark-base-pos-format2-mark-class-count
   mark-base-pos-format2-mark-coverage-offset
   mark-base-pos-format2-mark-coverage-offset-offset mark-base-pos-format2-p
   mark-base-pos-format2-pos-format mark-lig-pos-format1
   mark-lig-pos-format1-ligature-array-offset
   mark-lig-pos-format1-ligature-array-offset-offset
   mark-lig-pos-format1-ligature-coverage-offset
   mark-lig-pos-format1-ligature-coverage-offset-offset
   mark-lig-pos-format1-mark-array-offset
   mark-lig-pos-format1-mark-array-offset-offset
   mark-lig-pos-format1-mark-class-count
   mark-lig-pos-format1-mark-coverage-offset
   mark-lig-pos-format1-mark-coverage-offset-offset mark-lig-pos-format1-p
   mark-lig-pos-format1-pos-format mark-lig-pos-format2
   mark-lig-pos-format2-ligature-array-offset
   mark-lig-pos-format2-ligature-array-offset-offset
   mark-lig-pos-format2-ligature-coverage-offset
   mark-lig-pos-format2-ligature-coverage-offset-offset
   mark-lig-pos-format2-mark-array-offset
   mark-lig-pos-format2-mark-array-offset-offset
   mark-lig-pos-format2-mark-class-count
   mark-lig-pos-format2-mark-coverage-offset
   mark-lig-pos-format2-mark-coverage-offset-offset mark-lig-pos-format2-p
   mark-lig-pos-format2-pos-format mark-mark-pos-format1
   mark-mark-pos-format1-mark-class-count
   mark-mark-pos-format1-mark1-array-offset
   mark-mark-pos-format1-mark1-array-offset-offset
   mark-mark-pos-format1-mark1-coverage-offset
   mark-mark-pos-format1-mark1-coverage-offset-offset
   mark-mark-pos-format1-mark2-array-offset
   mark-mark-pos-format1-mark2-array-offset-offset
   mark-mark-pos-format1-mark2-coverage-offset
   mark-mark-pos-format1-mark2-coverage-offset-offset mark-mark-pos-format1-p
   mark-mark-pos-format1-pos-format mark-mark-pos-format2
   mark-mark-pos-format2-mark-class-count
   mark-mark-pos-format2-mark1-array-offset
   mark-mark-pos-format2-mark1-array-offset-offset
   mark-mark-pos-format2-mark1-coverage-offset
   mark-mark-pos-format2-mark1-coverage-offset-offset
   mark-mark-pos-format2-mark2-array-offset
   mark-mark-pos-format2-mark2-array-offset-offset
   mark-mark-pos-format2-mark2-coverage-offset
   mark-mark-pos-format2-mark2-coverage-offset-offset mark-mark-pos-format2-p
   mark-mark-pos-format2-pos-format mark-record mark-record-mark-anchor-offset
   mark-record-mark-class mark-record-p mark-record2
   mark-record2-mark-anchor-offset mark-record2-mark-class mark-record2-p
   mark2-array mark2-array-mark2-count mark2-array-mark2-records-start
   mark2-array-p mark2-array2 mark2-array2-mark2-count
   mark2-array2-mark2-records-start mark2-array2-p mark2-record
   mark2-record-mark2-anchor-offsets-count
   mark2-record-mark2-anchor-offsets-ref mark2-record-p mark2-record2
   mark2-record2-mark2-anchor-offsets-count
   mark2-record2-mark2-anchor-offsets-ref mark2-record2-p pair-pos-format1
   pair-pos-format1-coverage-offset pair-pos-format1-coverage-offset-offset
   pair-pos-format1-p pair-pos-format1-pair-set-count
   pair-pos-format1-pair-set-offsets-count
   pair-pos-format1-pair-set-offsets-ref pair-pos-format1-pos-format
   pair-pos-format1-value-format1 pair-pos-format1-value-format2
   pair-pos-format2 pair-pos-format2-class-def1-offset
   pair-pos-format2-class-def1-offset-offset pair-pos-format2-class-def2-offset
   pair-pos-format2-class-def2-offset-offset pair-pos-format2-class1-count
   pair-pos-format2-class1-records-start pair-pos-format2-class2-count
   pair-pos-format2-coverage-offset pair-pos-format2-coverage-offset-offset
   pair-pos-format2-p pair-pos-format2-pos-format
   pair-pos-format2-value-format1 pair-pos-format2-value-format2
   pair-pos-format3 pair-pos-format3-coverage-offset
   pair-pos-format3-coverage-offset-offset pair-pos-format3-p
   pair-pos-format3-pair-set-count pair-pos-format3-pair-set-offsets-count
   pair-pos-format3-pair-set-offsets-ref pair-pos-format3-pos-format
   pair-pos-format3-value-format1 pair-pos-format3-value-format2
   pair-pos-format4 pair-pos-format4-class-def1-offset
   pair-pos-format4-class-def1-offset-offset pair-pos-format4-class-def2-offset
   pair-pos-format4-class-def2-offset-offset pair-pos-format4-class1-count
   pair-pos-format4-class1-records-start pair-pos-format4-class2-count
   pair-pos-format4-coverage-offset pair-pos-format4-coverage-offset-offset
   pair-pos-format4-p pair-pos-format4-pos-format
   pair-pos-format4-value-format1 pair-pos-format4-value-format2 pair-set
   pair-set-p pair-set-pair-value-count pair-set-pair-value-records-start
   pair-set2 pair-set2-p pair-set2-pair-value-count
   pair-set2-pair-value-records-start pair-value-record pair-value-record-p
   pair-value-record-second-glyph pair-value-record-value-record1-start
   pair-value-record2 pair-value-record2-p pair-value-record2-second-glyph
   pair-value-record2-value-record1-start read-anchor-format1
   read-anchor-format2 read-anchor-format3 read-anchor-table read-base-array
   read-base-array2 read-cursive-pos read-cursive-pos-format1
   read-cursive-pos-format2 read-extension-pos-format1 read-gpos
   read-ligature-array read-ligature-array2 read-ligature-attach
   read-ligature-attach2 read-mark-array read-mark-array2 read-mark-base-pos
   read-mark-base-pos-format1 read-mark-base-pos-format2 read-mark-lig-pos
   read-mark-lig-pos-format1 read-mark-lig-pos-format2 read-mark-mark-pos
   read-mark-mark-pos-format1 read-mark-mark-pos-format2 read-mark2-array
   read-mark2-array2 read-pair-pos read-pair-pos-format1 read-pair-pos-format2
   read-pair-pos-format3 read-pair-pos-format4 read-pair-set read-pair-set2
   read-single-pos read-single-pos-format1 read-single-pos-format2
   read-single-pos-format3 read-single-pos-format4 sanitize-anchor-format1
   sanitize-anchor-format2 sanitize-anchor-format3 sanitize-base-array
   sanitize-base-array2 sanitize-cursive-pos-format1
   sanitize-cursive-pos-format2 sanitize-extension-pos-format1 sanitize-gpos
   sanitize-ligature-array sanitize-ligature-array2 sanitize-ligature-attach
   sanitize-ligature-attach2 sanitize-mark-array sanitize-mark-array2
   sanitize-mark-base-pos-format1 sanitize-mark-base-pos-format2
   sanitize-mark-lig-pos-format1 sanitize-mark-lig-pos-format2
   sanitize-mark-mark-pos-format1 sanitize-mark-mark-pos-format2
   sanitize-mark2-array sanitize-mark2-array2 sanitize-pair-pos-format1
   sanitize-pair-pos-format2 sanitize-pair-pos-format3
   sanitize-pair-pos-format4 sanitize-pair-set sanitize-pair-set2
   sanitize-single-pos-format1 sanitize-single-pos-format2
   sanitize-single-pos-format3 sanitize-single-pos-format4 single-pos-format1
   single-pos-format1-coverage-offset single-pos-format1-coverage-offset-offset
   single-pos-format1-p single-pos-format1-pos-format
   single-pos-format1-value-format single-pos-format1-value-record-start
   single-pos-format2 single-pos-format2-coverage-offset
   single-pos-format2-coverage-offset-offset single-pos-format2-p
   single-pos-format2-pos-format single-pos-format2-value-count
   single-pos-format2-value-format single-pos-format2-value-records-start
   single-pos-format3 single-pos-format3-coverage-offset
   single-pos-format3-coverage-offset-offset single-pos-format3-p
   single-pos-format3-pos-format single-pos-format3-value-format
   single-pos-format3-value-record-start single-pos-format4
   single-pos-format4-coverage-offset single-pos-format4-coverage-offset-offset
   single-pos-format4-p single-pos-format4-pos-format
   single-pos-format4-value-count single-pos-format4-value-format
   single-pos-format4-value-records-start value-format-x-advance-device-p
   value-format-x-advance-p value-format-x-placement-device-p
   value-format-x-placement-p value-format-y-advance-device-p
   value-format-y-advance-p value-format-y-placement-device-p
   value-format-y-placement-p))

(define-constant +value-format-x-placement+ 1 "See [ValueRecord]")

(declaim (inline value-format-x-placement-p))

(defun value-format-x-placement-p (flags)
  (logtest flags +value-format-x-placement+))

(define-constant +value-format-y-placement+ 2 "See [ValueRecord]")

(declaim (inline value-format-y-placement-p))

(defun value-format-y-placement-p (flags)
  (logtest flags +value-format-y-placement+))

(define-constant +value-format-x-advance+ 4 "See [ValueRecord]")

(declaim (inline value-format-x-advance-p))

(defun value-format-x-advance-p (flags)
  (logtest flags +value-format-x-advance+))

(define-constant +value-format-y-advance+ 8 "See [ValueRecord]")

(declaim (inline value-format-y-advance-p))

(defun value-format-y-advance-p (flags)
  (logtest flags +value-format-y-advance+))

(define-constant +value-format-x-placement-device+ 16 "See [ValueRecord]")

(declaim (inline value-format-x-placement-device-p))

(defun value-format-x-placement-device-p (flags)
  (logtest flags +value-format-x-placement-device+))

(define-constant +value-format-y-placement-device+ 32 "See [ValueRecord]")

(declaim (inline value-format-y-placement-device-p))

(defun value-format-y-placement-device-p (flags)
  (logtest flags +value-format-y-placement-device+))

(define-constant +value-format-x-advance-device+ 64 "See [ValueRecord]")

(declaim (inline value-format-x-advance-device-p))

(defun value-format-x-advance-device-p (flags)
  (logtest flags +value-format-x-advance-device+))

(define-constant +value-format-y-advance-device+ 128 "See [ValueRecord]")

(declaim (inline value-format-y-advance-device-p))

(defun value-format-y-advance-device-p (flags)
  (logtest flags +value-format-y-advance-device+))

(defstruct
    (mark-record (:include record-ref)
     (:constructor %make-mark-record (%record-font-data %record-offset)))
  "Part of [MarkArray]")

(define-constant +mark-record-size+ 4)

(declaim (inline mark-record-mark-class mark-record-mark-anchor-offset))

(defun mark-record-mark-class (rec)
  "Class defined for the associated mark."
  (declare (type mark-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun mark-record-mark-anchor-offset (rec)
  "Offset to Anchor table, from beginning of MarkArray table."
  (declare (type mark-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (pair-value-record (:include record-ref)
     (:constructor %make-pair-value-record (%record-font-data %record-offset)))
  "Part of [PairSet]")

(declaim (inline pair-value-record-second-glyph))

(defun pair-value-record-second-glyph (rec)
  "Glyph ID of second glyph in the pair (first glyph is listed in the Coverage table)."
  (declare (type pair-value-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun pair-value-record-value-record1-start (rec)
  "Byte position of the value-record1 field; its data is variable-length GPOS-VALUE-RECORD."
  (declare (type pair-value-record rec))
  (+ (record-ref-offset rec) 2))

(defstruct
    (class1-record (:include record-ref)
     (:constructor %make-class1-record (%record-font-data %record-offset)))
  "Part of [PairPosFormat2]")

(defun class1-record-class2-records-start (rec)
  "Byte position of the class2-records field; its data is variable-length CLASS2-RECORD."
  (declare (type class1-record rec))
  (record-ref-offset rec))

(defstruct
    (class2-record (:include record-ref)
     (:constructor %make-class2-record (%record-font-data %record-offset)))
  "Part of [PairPosFormat2]")

(defun class2-record-value-record1-start (rec)
  "Byte position of the value-record1 field; its data is variable-length GPOS-VALUE-RECORD."
  (declare (type class2-record rec))
  (record-ref-offset rec))

(defstruct
    (entry-exit-record (:include record-ref)
     (:constructor %make-entry-exit-record (%record-font-data %record-offset)))
  "Part of [CursivePosFormat1]")

(define-constant +entry-exit-record-size+ 4)

(declaim
 (inline entry-exit-record-entry-anchor-offset
  entry-exit-record-exit-anchor-offset))

(defun entry-exit-record-entry-anchor-offset (rec)
  "Offset to entryAnchor table, from beginning of CursivePos subtable (may be NULL)."
  (declare (type entry-exit-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun entry-exit-record-exit-anchor-offset (rec)
  "Offset to exitAnchor table, from beginning of CursivePos subtable (may be NULL)."
  (declare (type entry-exit-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (base-record (:include record-ref)
     (:constructor %make-base-record (%record-font-data %record-offset)))
  "Part of [BaseArray]")

(defun base-record-base-anchor-offsets-count (rec &optional mark-class-count)
  "Array of offsets (one per mark class) to Anchor tables. Offsets are from beginning of BaseArray table, ordered by class (offsets may be NULL)."
  (declare (type base-record rec)
           (ignore rec))
  mark-class-count)

(defun base-record-base-anchor-offsets-ref (rec index)
  "Array of offsets (one per mark class) to Anchor tables. Offsets are from beginning of BaseArray table, ordered by class (offsets may be NULL)."
  (declare (type base-record rec)
           (type (unsigned-byte 32) index))
  (let ((pos (+ (record-ref-offset rec) (* index 2))))
    (font-data-read-u16 (record-ref-font-data rec) pos)))

(defstruct
    (component-record (:include record-ref)
     (:constructor %make-component-record (%record-font-data %record-offset)))
  "Part of [MarkLigPosFormat1]")

(defun component-record-ligature-anchor-offsets-count
       (rec &optional mark-class-count)
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of LigatureAttach table, ordered by class (offsets may be NULL)."
  (declare (type component-record rec)
           (ignore rec))
  mark-class-count)

(defun component-record-ligature-anchor-offsets-ref (rec index)
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of LigatureAttach table, ordered by class (offsets may be NULL)."
  (declare (type component-record rec)
           (type (unsigned-byte 32) index))
  (let ((pos (+ (record-ref-offset rec) (* index 2))))
    (font-data-read-u16 (record-ref-font-data rec) pos)))

(defstruct
    (mark2-record (:include record-ref)
     (:constructor %make-mark2-record (%record-font-data %record-offset)))
  "Part of [MarkMarkPosFormat1]")

(defun mark2-record-mark2-anchor-offsets-count (rec &optional mark-class-count)
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of Mark2Array table, in class order (offsets may be NULL)."
  (declare (type mark2-record rec)
           (ignore rec))
  mark-class-count)

(defun mark2-record-mark2-anchor-offsets-ref (rec index)
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of Mark2Array table, in class order (offsets may be NULL)."
  (declare (type mark2-record rec)
           (type (unsigned-byte 32) index))
  (let ((pos (+ (record-ref-offset rec) (* index 2))))
    (font-data-read-u16 (record-ref-font-data rec) pos)))

(defstruct
    (pair-value-record2 (:include record-ref)
     (:constructor %make-pair-value-record2
      (%record-font-data %record-offset)))
  "PairValueRecord2 (ISO/IEC 14496-22:2026).")

(declaim (inline pair-value-record2-second-glyph))

(defun pair-value-record2-second-glyph (rec)
  "Glyph ID of second glyph in the pair (first glyph is listed in the Coverage table)."
  (declare (type pair-value-record2 rec))
  (font-data-read-u24 (record-ref-font-data rec) (record-ref-offset rec)))

(defun pair-value-record2-value-record1-start (rec)
  "Byte position of the value-record1 field; its data is variable-length GPOS-VALUE-RECORD."
  (declare (type pair-value-record2 rec))
  (+ (record-ref-offset rec) 3))

(defstruct
    (entry-exit-record2 (:include record-ref)
     (:constructor %make-entry-exit-record2
      (%record-font-data %record-offset)))
  "EntryExitRecord2 (ISO/IEC 14496-22:2026).")

(define-constant +entry-exit-record2-size+ 6)

(declaim
 (inline entry-exit-record2-entry-anchor-offset
  entry-exit-record2-exit-anchor-offset))

(defun entry-exit-record2-entry-anchor-offset (rec)
  "Offset to entryAnchor table, from beginning of CursivePos subtable (may be NULL)."
  (declare (type entry-exit-record2 rec))
  (font-data-read-u24 (record-ref-font-data rec) (record-ref-offset rec)))

(defun entry-exit-record2-exit-anchor-offset (rec)
  "Offset to exitAnchor table, from beginning of CursivePos subtable (may be NULL)."
  (declare (type entry-exit-record2 rec))
  (font-data-read-u24 (record-ref-font-data rec) (+ (record-ref-offset rec) 3)))

(defstruct
    (base-record2 (:include record-ref)
     (:constructor %make-base-record2 (%record-font-data %record-offset)))
  "BaseRecord2 (ISO/IEC 14496-22:2026).")

(defun base-record2-base-anchor-offsets-count (rec &optional mark-class-count)
  "Array of offsets (one per mark class) to Anchor tables. Offsets are from beginning of BaseArray table, ordered by class (offsets may be NULL)."
  (declare (type base-record2 rec)
           (ignore rec))
  mark-class-count)

(defun base-record2-base-anchor-offsets-ref (rec index)
  "Array of offsets (one per mark class) to Anchor tables. Offsets are from beginning of BaseArray table, ordered by class (offsets may be NULL)."
  (declare (type base-record2 rec)
           (type (unsigned-byte 32) index))
  (let ((pos (+ (record-ref-offset rec) (* index 3))))
    (font-data-read-u24 (record-ref-font-data rec) pos)))

(defstruct
    (component-record2 (:include record-ref)
     (:constructor %make-component-record2 (%record-font-data %record-offset)))
  "ComponentRecord2 (ISO/IEC 14496-22:2026).")

(defun component-record2-ligature-anchor-offsets-count
       (rec &optional mark-class-count)
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of LigatureAttach table, ordered by class (offsets may be NULL)."
  (declare (type component-record2 rec)
           (ignore rec))
  mark-class-count)

(defun component-record2-ligature-anchor-offsets-ref (rec index)
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of LigatureAttach table, ordered by class (offsets may be NULL)."
  (declare (type component-record2 rec)
           (type (unsigned-byte 32) index))
  (let ((pos (+ (record-ref-offset rec) (* index 3))))
    (font-data-read-u24 (record-ref-font-data rec) pos)))

(defstruct
    (mark2-record2 (:include record-ref)
     (:constructor %make-mark2-record2 (%record-font-data %record-offset)))
  "Mark2Record2 (ISO/IEC 14496-22:2026).")

(defun mark2-record2-mark2-anchor-offsets-count
       (rec &optional mark-class-count)
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of Mark2Array table, in class order (offsets may be NULL)."
  (declare (type mark2-record2 rec)
           (ignore rec))
  mark-class-count)

(defun mark2-record2-mark2-anchor-offsets-ref (rec index)
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of Mark2Array table, in class order (offsets may be NULL)."
  (declare (type mark2-record2 rec)
           (type (unsigned-byte 32) index))
  (let ((pos (+ (record-ref-offset rec) (* index 3))))
    (font-data-read-u24 (record-ref-font-data rec) pos)))

(defstruct
    (mark-record2 (:include record-ref)
     (:constructor %make-mark-record2 (%record-font-data %record-offset)))
  "MarkRecord2 (ISO/IEC 14496-22:2026).")

(define-constant +mark-record2-size+ 5)

(declaim (inline mark-record2-mark-class mark-record2-mark-anchor-offset))

(defun mark-record2-mark-class (rec)
  "Class defined for the associated mark."
  (declare (type mark-record2 rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun mark-record2-mark-anchor-offset (rec)
  "Offset to Anchor table, from beginning of MarkArray table."
  (declare (type mark-record2 rec))
  (font-data-read-u24 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (gpos (:include table-ref)
     (:constructor %make-gpos (%table-font-data %table-offset %table-length)))
  "[Class Definition Table Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#class-definition-table-format-1) [GPOS Version 1.0](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#gpos-header)")

(declaim
 (inline gpos-version gpos-version-raw gpos-script-list-offset-offset
  gpos-script-list-offset gpos-feature-list-offset-offset
  gpos-feature-list-offset gpos-lookup-list-offset-offset
  gpos-lookup-list-offset gpos-feature-variations-offset-offset
  gpos-feature-variations-offset gpos-script-list2-offset-offset
  gpos-script-list2-offset gpos-feature-list2-offset-offset
  gpos-feature-list2-offset gpos-lookup-list2-offset-offset
  gpos-lookup-list2-offset))

(define-constant +gpos-tag+ "GPOS")

(defun gpos-version (table)
  "The major and minor version of the GPOS table, as a tuple (u16, u16)"
  (declare (type gpos table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun gpos-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun gpos-script-list-offset-offset (table)
  "Offset to ScriptList table, from beginning of GPOS table"
  (declare (type gpos table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun gpos-script-list-offset (table)
  "Offset to ScriptList table, from beginning of GPOS table"
  (declare (type gpos table))
  (let ((off (gpos-script-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-script-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gpos-feature-list-offset-offset (table)
  "Offset to FeatureList table, from beginning of GPOS table"
  (declare (type gpos table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun gpos-feature-list-offset (table)
  "Offset to FeatureList table, from beginning of GPOS table"
  (declare (type gpos table))
  (let ((off (gpos-feature-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-feature-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gpos-lookup-list-offset-offset (table)
  "Offset to LookupList table, from beginning of GPOS table"
  (declare (type gpos table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun gpos-lookup-list-offset (table)
  "Offset to LookupList table, from beginning of GPOS table"
  (declare (type gpos table))
  (let ((off (gpos-lookup-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun gpos-feature-variations-offset-offset (table)
  (declare (type gpos table))
  (when
      (and (>= (gpos-version table) 65537)
           (<= (+ (+ (table-ref-offset table) 10) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 10))))

(defun gpos-feature-variations-offset (table)
  (declare (type gpos table))
  (let ((off (gpos-feature-variations-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-feature-variations (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gpos-script-list2-offset-offset (table)
  "32-bit offset to ScriptList, taking precedence when nonzero."
  (declare (type gpos table))
  (when
      (and (>= (gpos-version table) 65538)
           (<= (+ (+ (table-ref-offset table) 14) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 14))))

(defun gpos-script-list2-offset (table)
  "32-bit offset to ScriptList, taking precedence when nonzero."
  (declare (type gpos table))
  (let ((off (gpos-script-list2-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-script-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gpos-feature-list2-offset-offset (table)
  "32-bit offset to FeatureList, taking precedence when nonzero."
  (declare (type gpos table))
  (when
      (and (>= (gpos-version table) 65538)
           (<= (+ (+ (table-ref-offset table) 18) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 18))))

(defun gpos-feature-list2-offset (table)
  "32-bit offset to FeatureList, taking precedence when nonzero."
  (declare (type gpos table))
  (let ((off (gpos-feature-list2-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-feature-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gpos-lookup-list2-offset-offset (table)
  "32-bit offset to LookupList2, taking precedence when nonzero."
  (declare (type gpos table))
  (when
      (and (>= (gpos-version table) 65538)
           (<= (+ (+ (table-ref-offset table) 22) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 22))))

(defun gpos-lookup-list2-offset (table)
  "32-bit offset to LookupList2, taking precedence when nonzero."
  (declare (type gpos table))
  (let ((off (gpos-lookup-list2-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun sanitize-gpos (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  t)

(defun read-gpos
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-gpos font-data offset length)
  (%make-gpos font-data offset length))

(defun read-anchor-table
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "[Anchor Tables](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#anchor-tables) position one glyph with respect to another."
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-anchor-format1 font-data offset length))
      (2 (read-anchor-format2 font-data offset length))
      (3 (read-anchor-format3 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'anchor-table)))))

(defstruct
    (anchor-format1 (:include table-ref)
     (:constructor %make-anchor-format1
      (%table-font-data %table-offset %table-length)))
  "[Anchor Table Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#anchor-table-format-1-design-units): Design Units")

(declaim
 (inline anchor-format1-anchor-format anchor-format1-anchor-format-raw
  anchor-format1-x-coordinate anchor-format1-x-coordinate-raw
  anchor-format1-y-coordinate anchor-format1-y-coordinate-raw))

(defun anchor-format1-anchor-format (table)
  "Format identifier, = 1"
  (declare (type anchor-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun anchor-format1-anchor-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun anchor-format1-x-coordinate (table)
  "Horizontal value, in design units"
  (declare (type anchor-format1 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun anchor-format1-x-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun anchor-format1-y-coordinate (table)
  "Vertical value, in design units"
  (declare (type anchor-format1 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun anchor-format1-y-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 4)))

(defun sanitize-anchor-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  t)

(defun read-anchor-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-anchor-format1 font-data offset length)
  (%make-anchor-format1 font-data offset length))

(defstruct
    (anchor-format2 (:include table-ref)
     (:constructor %make-anchor-format2
      (%table-font-data %table-offset %table-length)))
  "[Anchor Table Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#anchor-table-format-2-design-units-plus-contour-point): Design Units Plus Contour Point")

(declaim
 (inline anchor-format2-anchor-format anchor-format2-anchor-format-raw
  anchor-format2-x-coordinate anchor-format2-x-coordinate-raw
  anchor-format2-y-coordinate anchor-format2-y-coordinate-raw
  anchor-format2-anchor-point anchor-format2-anchor-point-raw))

(defun anchor-format2-anchor-format (table)
  "Format identifier, = 2"
  (declare (type anchor-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun anchor-format2-anchor-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun anchor-format2-x-coordinate (table)
  "Horizontal value, in design units"
  (declare (type anchor-format2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun anchor-format2-x-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun anchor-format2-y-coordinate (table)
  "Vertical value, in design units"
  (declare (type anchor-format2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun anchor-format2-y-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 4)))

(defun anchor-format2-anchor-point (table)
  "Index to glyph contour point"
  (declare (type anchor-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun anchor-format2-anchor-point-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun sanitize-anchor-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-anchor-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-anchor-format2 font-data offset length)
  (%make-anchor-format2 font-data offset length))

(defstruct
    (anchor-format3 (:include table-ref)
     (:constructor %make-anchor-format3
      (%table-font-data %table-offset %table-length)))
  "[Anchor Table Format 3](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#anchor-table-format-3-design-units-plus-device-or-variationindex-tables): Design Units Plus Device or VariationIndex Tables")

(declaim
 (inline anchor-format3-anchor-format anchor-format3-anchor-format-raw
  anchor-format3-x-coordinate anchor-format3-x-coordinate-raw
  anchor-format3-y-coordinate anchor-format3-y-coordinate-raw
  anchor-format3-x-device-offset-offset anchor-format3-x-device-offset
  anchor-format3-y-device-offset-offset anchor-format3-y-device-offset))

(defun anchor-format3-anchor-format (table)
  "Format identifier, = 3"
  (declare (type anchor-format3 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun anchor-format3-anchor-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun anchor-format3-x-coordinate (table)
  "Horizontal value, in design units"
  (declare (type anchor-format3 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun anchor-format3-x-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun anchor-format3-y-coordinate (table)
  "Vertical value, in design units"
  (declare (type anchor-format3 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun anchor-format3-y-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 4)))

(defun anchor-format3-x-device-offset-offset (table)
  "Offset to Device table (non-variable font) / VariationIndex table (variable font) for X coordinate, from beginning of Anchor table (may be NULL)"
  (declare (type anchor-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun anchor-format3-x-device-offset (table)
  "Offset to Device table (non-variable font) / VariationIndex table (variable font) for X coordinate, from beginning of Anchor table (may be NULL)"
  (declare (type anchor-format3 table))
  (let ((off (anchor-format3-x-device-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-device-or-variation-index (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun anchor-format3-y-device-offset-offset (table)
  "Offset to Device table (non-variable font) / VariationIndex table (variable font) for Y coordinate, from beginning of Anchor table (may be NULL)"
  (declare (type anchor-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun anchor-format3-y-device-offset (table)
  "Offset to Device table (non-variable font) / VariationIndex table (variable font) for Y coordinate, from beginning of Anchor table (may be NULL)"
  (declare (type anchor-format3 table))
  (let ((off (anchor-format3-y-device-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-device-or-variation-index (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-anchor-format3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  t)

(defun read-anchor-format3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-anchor-format3 font-data offset length)
  (%make-anchor-format3 font-data offset length))

(defstruct
    (mark-array (:include table-ref)
     (:constructor %make-mark-array
      (%table-font-data %table-offset %table-length)))
  "[Mark Array Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#mark-array-table)")

(declaim
 (inline mark-array-mark-count mark-array-mark-count-raw
  mark-array-mark-records-count mark-array-mark-records-ref))

(defun mark-array-mark-count (table)
  "Number of MarkRecords"
  (declare (type mark-array table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark-array-mark-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark-array-mark-records-count (table)
  "Array of MarkRecords, ordered by corresponding glyphs in the associated mark Coverage table."
  (declare (type mark-array table))
  (declare (ignorable table))
  (mark-array-mark-count table))

(defun mark-array-mark-records-ref (table index)
  "Array of MarkRecords, ordered by corresponding glyphs in the associated mark Coverage table."
  (declare (type mark-array table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-mark-record (table-ref-font-data table) pos)))

(defun sanitize-mark-array (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  (let* ((cnt (mark-array-mark-count-raw font-data offset))
         (pos (+ 2 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-mark-array
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark-array font-data offset length)
  (%make-mark-array font-data offset length))

(defun read-single-pos
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "[Lookup Type 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#lookup-type-1-single-adjustment-positioning-subtable): Single Adjustment Positioning Subtable"
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-single-pos-format1 font-data offset length))
      (2 (read-single-pos-format2 font-data offset length))
      (3 (read-single-pos-format3 font-data offset length))
      (4 (read-single-pos-format4 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'single-pos)))))

(defstruct
    (single-pos-format1 (:include table-ref)
     (:constructor %make-single-pos-format1
      (%table-font-data %table-offset %table-length)))
  "[Single Adjustment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#single-adjustment-positioning-format-1-single-positioning-value): Single Positioning Value")

(declaim
 (inline single-pos-format1-pos-format single-pos-format1-pos-format-raw
  single-pos-format1-coverage-offset-offset single-pos-format1-coverage-offset
  single-pos-format1-value-format single-pos-format1-value-format-raw
  single-pos-format1-value-record-start))

(defun single-pos-format1-pos-format (table)
  "Format identifier: format = 1"
  (declare (type single-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun single-pos-format1-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun single-pos-format1-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of SinglePos subtable."
  (declare (type single-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun single-pos-format1-coverage-offset (table)
  "Offset to Coverage table, from beginning of SinglePos subtable."
  (declare (type single-pos-format1 table))
  (let ((off (single-pos-format1-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun single-pos-format1-value-format (table)
  "Defines the types of data in the ValueRecord."
  (declare (type single-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun single-pos-format1-value-format-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun single-pos-format1-value-record-start (table)
  "Byte position of the value-record field; its elements are variable-length GPOS-VALUE-RECORD data."
  (declare (type single-pos-format1 table))
  (+ (table-ref-offset table) 6))

(defun sanitize-single-pos-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  t)

(defun read-single-pos-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-single-pos-format1 font-data offset length)
  (%make-single-pos-format1 font-data offset length))

(defstruct
    (single-pos-format2 (:include table-ref)
     (:constructor %make-single-pos-format2
      (%table-font-data %table-offset %table-length)))
  "[Single Adjustment Positioning Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#single-adjustment-positioning-format-2-array-of-positioning-values): Array of Positioning Values")

(declaim
 (inline single-pos-format2-pos-format single-pos-format2-pos-format-raw
  single-pos-format2-coverage-offset-offset single-pos-format2-coverage-offset
  single-pos-format2-value-format single-pos-format2-value-format-raw
  single-pos-format2-value-count single-pos-format2-value-count-raw
  single-pos-format2-value-records-start))

(defun single-pos-format2-pos-format (table)
  "Format identifier: format = 2"
  (declare (type single-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun single-pos-format2-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun single-pos-format2-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of SinglePos subtable."
  (declare (type single-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun single-pos-format2-coverage-offset (table)
  "Offset to Coverage table, from beginning of SinglePos subtable."
  (declare (type single-pos-format2 table))
  (let ((off (single-pos-format2-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun single-pos-format2-value-format (table)
  "Defines the types of data in the ValueRecords."
  (declare (type single-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun single-pos-format2-value-format-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun single-pos-format2-value-count (table)
  "Number of ValueRecords — must equal glyphCount in the Coverage table."
  (declare (type single-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun single-pos-format2-value-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun single-pos-format2-value-records-start (table)
  "Byte position of the value-records field; its elements are variable-length GPOS-VALUE-RECORD data."
  (declare (type single-pos-format2 table))
  (+ (table-ref-offset table) 8))

(defun sanitize-single-pos-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-single-pos-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-single-pos-format2 font-data offset length)
  (%make-single-pos-format2 font-data offset length))

(defun read-pair-pos
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "[Lookup Type 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#lookup-type-1-single-adjustment-positioning-subtable): Single Adjustment Positioning Subtable"
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-pair-pos-format1 font-data offset length))
      (2 (read-pair-pos-format2 font-data offset length))
      (3 (read-pair-pos-format3 font-data offset length))
      (4 (read-pair-pos-format4 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'pair-pos)))))

(defstruct
    (pair-pos-format1 (:include table-ref)
     (:constructor %make-pair-pos-format1
      (%table-font-data %table-offset %table-length)))
  "[Pair Adjustment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#pair-adjustment-positioning-format-1-adjustments-for-glyph-pairs): Adjustments for Glyph Pairs")

(declaim
 (inline pair-pos-format1-pos-format pair-pos-format1-pos-format-raw
  pair-pos-format1-coverage-offset-offset pair-pos-format1-coverage-offset
  pair-pos-format1-value-format1 pair-pos-format1-value-format1-raw
  pair-pos-format1-value-format2 pair-pos-format1-value-format2-raw
  pair-pos-format1-pair-set-count pair-pos-format1-pair-set-count-raw
  pair-pos-format1-pair-set-offsets-count
  pair-pos-format1-pair-set-offsets-ref))

(defun pair-pos-format1-pos-format (table)
  "Format identifier: format = 1"
  (declare (type pair-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun pair-pos-format1-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun pair-pos-format1-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of PairPos subtable."
  (declare (type pair-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun pair-pos-format1-coverage-offset (table)
  "Offset to Coverage table, from beginning of PairPos subtable."
  (declare (type pair-pos-format1 table))
  (let ((off (pair-pos-format1-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun pair-pos-format1-value-format1 (table)
  "Defines the types of data in valueRecord1 — for the first glyph in the pair (may be zero)."
  (declare (type pair-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun pair-pos-format1-value-format1-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun pair-pos-format1-value-format2 (table)
  "Defines the types of data in valueRecord2 — for the second glyph in the pair (may be zero)."
  (declare (type pair-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun pair-pos-format1-value-format2-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun pair-pos-format1-pair-set-count (table)
  "Number of PairSet tables"
  (declare (type pair-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun pair-pos-format1-pair-set-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun pair-pos-format1-pair-set-offsets-count (table)
  "Array of offsets to PairSet tables. Offsets are from beginning of PairPos subtable, ordered by Coverage Index."
  (declare (type pair-pos-format1 table))
  (declare (ignorable table))
  (pair-pos-format1-pair-set-count table))

(defun pair-pos-format1-pair-set-offsets-ref (table index)
  "Array of offsets to PairSet tables. Offsets are from beginning of PairPos subtable, ordered by Coverage Index."
  (declare (type pair-pos-format1 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 10) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u16 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-pair-set (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-pair-pos-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  (let* ((cnt (pair-pos-format1-pair-set-count-raw font-data offset))
         (pos (+ 10 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-pair-pos-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-pair-pos-format1 font-data offset length)
  (%make-pair-pos-format1 font-data offset length))

(defstruct
    (pair-set (:include table-ref)
     (:constructor %make-pair-set
      (%table-font-data %table-offset %table-length)))
  "Part of [PairPosFormat1]")

(declaim
 (inline pair-set-pair-value-count pair-set-pair-value-count-raw
  pair-set-pair-value-records-start))

(defun pair-set-pair-value-count (table)
  "Number of PairValueRecords"
  (declare (type pair-set table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun pair-set-pair-value-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun pair-set-pair-value-records-start (table)
  "Byte position of the pair-value-records field; its elements are variable-length PAIR-VALUE-RECORD data."
  (declare (type pair-set table))
  (+ (table-ref-offset table) 2))

(defun sanitize-pair-set (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  t)

(defun read-pair-set
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-pair-set font-data offset length)
  (%make-pair-set font-data offset length))

(defstruct
    (pair-pos-format2 (:include table-ref)
     (:constructor %make-pair-pos-format2
      (%table-font-data %table-offset %table-length)))
  "[Pair Adjustment Positioning Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#pair-adjustment-positioning-format-2-class-pair-adjustment): Class Pair Adjustment")

(declaim
 (inline pair-pos-format2-pos-format pair-pos-format2-pos-format-raw
  pair-pos-format2-coverage-offset-offset pair-pos-format2-coverage-offset
  pair-pos-format2-value-format1 pair-pos-format2-value-format1-raw
  pair-pos-format2-value-format2 pair-pos-format2-value-format2-raw
  pair-pos-format2-class-def1-offset-offset pair-pos-format2-class-def1-offset
  pair-pos-format2-class-def2-offset-offset pair-pos-format2-class-def2-offset
  pair-pos-format2-class1-count pair-pos-format2-class1-count-raw
  pair-pos-format2-class2-count pair-pos-format2-class2-count-raw
  pair-pos-format2-class1-records-start))

(defun pair-pos-format2-pos-format (table)
  "Format identifier: format = 2"
  (declare (type pair-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun pair-pos-format2-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun pair-pos-format2-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of PairPos subtable."
  (declare (type pair-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun pair-pos-format2-coverage-offset (table)
  "Offset to Coverage table, from beginning of PairPos subtable."
  (declare (type pair-pos-format2 table))
  (let ((off (pair-pos-format2-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun pair-pos-format2-value-format1 (table)
  "ValueRecord definition — for the first glyph of the pair (may be zero)."
  (declare (type pair-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun pair-pos-format2-value-format1-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun pair-pos-format2-value-format2 (table)
  "ValueRecord definition — for the second glyph of the pair (may be zero)."
  (declare (type pair-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun pair-pos-format2-value-format2-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun pair-pos-format2-class-def1-offset-offset (table)
  "Offset to ClassDef table, from beginning of PairPos subtable — for the first glyph of the pair."
  (declare (type pair-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun pair-pos-format2-class-def1-offset (table)
  "Offset to ClassDef table, from beginning of PairPos subtable — for the first glyph of the pair."
  (declare (type pair-pos-format2 table))
  (let ((off (pair-pos-format2-class-def1-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-class-def (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun pair-pos-format2-class-def2-offset-offset (table)
  "Offset to ClassDef table, from beginning of PairPos subtable — for the second glyph of the pair."
  (declare (type pair-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun pair-pos-format2-class-def2-offset (table)
  "Offset to ClassDef table, from beginning of PairPos subtable — for the second glyph of the pair."
  (declare (type pair-pos-format2 table))
  (let ((off (pair-pos-format2-class-def2-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-class-def (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun pair-pos-format2-class1-count (table)
  "Number of classes in classDef1 table — includes Class 0."
  (declare (type pair-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun pair-pos-format2-class1-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 12)))

(defun pair-pos-format2-class2-count (table)
  "Number of classes in classDef2 table — includes Class 0."
  (declare (type pair-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 14)))

(defun pair-pos-format2-class2-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 14)))

(defun pair-pos-format2-class1-records-start (table)
  "Byte position of the class1-records field; its elements are variable-length CLASS1-RECORD data."
  (declare (type pair-pos-format2 table))
  (+ (table-ref-offset table) 16))

(defun sanitize-pair-pos-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  t)

(defun read-pair-pos-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-pair-pos-format2 font-data offset length)
  (%make-pair-pos-format2 font-data offset length))

(defstruct
    (cursive-pos-format1 (:include table-ref)
     (:constructor %make-cursive-pos-format1
      (%table-font-data %table-offset %table-length)))
  "[Cursive Attachment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#cursive-attachment-positioning-format1-cursive-attachment): Cursvie attachment")

(declaim
 (inline cursive-pos-format1-pos-format cursive-pos-format1-pos-format-raw
  cursive-pos-format1-coverage-offset-offset
  cursive-pos-format1-coverage-offset cursive-pos-format1-entry-exit-count
  cursive-pos-format1-entry-exit-count-raw
  cursive-pos-format1-entry-exit-record-count
  cursive-pos-format1-entry-exit-record-ref))

(defun cursive-pos-format1-pos-format (table)
  "Format identifier: format = 1"
  (declare (type cursive-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun cursive-pos-format1-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun cursive-pos-format1-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of CursivePos subtable."
  (declare (type cursive-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun cursive-pos-format1-coverage-offset (table)
  "Offset to Coverage table, from beginning of CursivePos subtable."
  (declare (type cursive-pos-format1 table))
  (let ((off (cursive-pos-format1-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun cursive-pos-format1-entry-exit-count (table)
  "Number of EntryExit records"
  (declare (type cursive-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun cursive-pos-format1-entry-exit-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun cursive-pos-format1-entry-exit-record-count (table)
  "Array of EntryExit records, in Coverage index order."
  (declare (type cursive-pos-format1 table))
  (declare (ignorable table))
  (cursive-pos-format1-entry-exit-count table))

(defun cursive-pos-format1-entry-exit-record-ref (table index)
  "Array of EntryExit records, in Coverage index order."
  (declare (type cursive-pos-format1 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 6) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-entry-exit-record (table-ref-font-data table) pos)))

(defun sanitize-cursive-pos-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  (let* ((cnt (cursive-pos-format1-entry-exit-count-raw font-data offset))
         (pos (+ 6 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-cursive-pos-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cursive-pos-format1 font-data offset length)
  (%make-cursive-pos-format1 font-data offset length))

(defstruct
    (mark-base-pos-format1 (:include table-ref)
     (:constructor %make-mark-base-pos-format1
      (%table-font-data %table-offset %table-length)))
  "[Mark-to-Base Attachment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#mark-to-base-attachment-positioning-format-1-mark-to-base-attachment-point): Mark-to-base Attachment Point")

(declaim
 (inline mark-base-pos-format1-pos-format mark-base-pos-format1-pos-format-raw
  mark-base-pos-format1-mark-coverage-offset-offset
  mark-base-pos-format1-mark-coverage-offset
  mark-base-pos-format1-base-coverage-offset-offset
  mark-base-pos-format1-base-coverage-offset
  mark-base-pos-format1-mark-class-count
  mark-base-pos-format1-mark-class-count-raw
  mark-base-pos-format1-mark-array-offset-offset
  mark-base-pos-format1-mark-array-offset
  mark-base-pos-format1-base-array-offset-offset
  mark-base-pos-format1-base-array-offset))

(defun mark-base-pos-format1-pos-format (table)
  "Format identifier: format = 1"
  (declare (type mark-base-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark-base-pos-format1-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark-base-pos-format1-mark-coverage-offset-offset (table)
  "Offset to markCoverage table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun mark-base-pos-format1-mark-coverage-offset (table)
  "Offset to markCoverage table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format1 table))
  (let ((off (mark-base-pos-format1-mark-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-base-pos-format1-base-coverage-offset-offset (table)
  "Offset to baseCoverage table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun mark-base-pos-format1-base-coverage-offset (table)
  "Offset to baseCoverage table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format1 table))
  (let ((off (mark-base-pos-format1-base-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-base-pos-format1-mark-class-count (table)
  "Number of classes defined for marks"
  (declare (type mark-base-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun mark-base-pos-format1-mark-class-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun mark-base-pos-format1-mark-array-offset-offset (table)
  "Offset to MarkArray table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun mark-base-pos-format1-mark-array-offset (table)
  "Offset to MarkArray table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format1 table))
  (let ((off (mark-base-pos-format1-mark-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark-array (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-base-pos-format1-base-array-offset-offset (table)
  "Offset to BaseArray table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun mark-base-pos-format1-base-array-offset (table)
  "Offset to BaseArray table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format1 table))
  (let ((off (mark-base-pos-format1-base-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-base-array (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-mark-base-pos-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-mark-base-pos-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark-base-pos-format1 font-data offset length)
  (%make-mark-base-pos-format1 font-data offset length))

(defstruct
    (base-array (:include table-ref)
     (:constructor %make-base-array
      (%table-font-data %table-offset %table-length)))
  "Part of [MarkBasePosFormat1]")

(declaim
 (inline base-array-base-count base-array-base-count-raw
  base-array-base-records-start))

(defun base-array-base-count (table)
  "Number of BaseRecords"
  (declare (type base-array table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun base-array-base-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun base-array-base-records-start (table)
  "Byte position of the base-records field; its elements are variable-length BASE-RECORD data."
  (declare (type base-array table))
  (+ (table-ref-offset table) 2))

(defun sanitize-base-array (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  t)

(defun read-base-array
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-array font-data offset length)
  (%make-base-array font-data offset length))

(defstruct
    (mark-lig-pos-format1 (:include table-ref)
     (:constructor %make-mark-lig-pos-format1
      (%table-font-data %table-offset %table-length)))
  "[Mark-to-Ligature Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#mark-to-ligature-attachment-positioning-format-1-mark-to-ligature-attachment): Mark-to-Ligature Attachment")

(declaim
 (inline mark-lig-pos-format1-pos-format mark-lig-pos-format1-pos-format-raw
  mark-lig-pos-format1-mark-coverage-offset-offset
  mark-lig-pos-format1-mark-coverage-offset
  mark-lig-pos-format1-ligature-coverage-offset-offset
  mark-lig-pos-format1-ligature-coverage-offset
  mark-lig-pos-format1-mark-class-count
  mark-lig-pos-format1-mark-class-count-raw
  mark-lig-pos-format1-mark-array-offset-offset
  mark-lig-pos-format1-mark-array-offset
  mark-lig-pos-format1-ligature-array-offset-offset
  mark-lig-pos-format1-ligature-array-offset))

(defun mark-lig-pos-format1-pos-format (table)
  "Format identifier: format = 1"
  (declare (type mark-lig-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark-lig-pos-format1-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark-lig-pos-format1-mark-coverage-offset-offset (table)
  "Offset to markCoverage table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun mark-lig-pos-format1-mark-coverage-offset (table)
  "Offset to markCoverage table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format1 table))
  (let ((off (mark-lig-pos-format1-mark-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-lig-pos-format1-ligature-coverage-offset-offset (table)
  "Offset to ligatureCoverage table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun mark-lig-pos-format1-ligature-coverage-offset (table)
  "Offset to ligatureCoverage table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format1 table))
  (let ((off (mark-lig-pos-format1-ligature-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-lig-pos-format1-mark-class-count (table)
  "Number of defined mark classes"
  (declare (type mark-lig-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun mark-lig-pos-format1-mark-class-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun mark-lig-pos-format1-mark-array-offset-offset (table)
  "Offset to MarkArray table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun mark-lig-pos-format1-mark-array-offset (table)
  "Offset to MarkArray table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format1 table))
  (let ((off (mark-lig-pos-format1-mark-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark-array (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-lig-pos-format1-ligature-array-offset-offset (table)
  "Offset to LigatureArray table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun mark-lig-pos-format1-ligature-array-offset (table)
  "Offset to LigatureArray table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format1 table))
  (let ((off (mark-lig-pos-format1-ligature-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-ligature-array (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-mark-lig-pos-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-mark-lig-pos-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark-lig-pos-format1 font-data offset length)
  (%make-mark-lig-pos-format1 font-data offset length))

(defstruct
    (ligature-array (:include table-ref)
     (:constructor %make-ligature-array
      (%table-font-data %table-offset %table-length)))
  "Part of [MarkLigPosFormat1]")

(declaim
 (inline ligature-array-ligature-count ligature-array-ligature-count-raw
  ligature-array-ligature-attach-offsets-count
  ligature-array-ligature-attach-offsets-ref))

(defun ligature-array-ligature-count (table)
  "Number of LigatureAttach table offsets"
  (declare (type ligature-array table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun ligature-array-ligature-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun ligature-array-ligature-attach-offsets-count (table)
  "Array of offsets to LigatureAttach tables. Offsets are from beginning of LigatureArray table, ordered by ligatureCoverage index."
  (declare (type ligature-array table))
  (declare (ignorable table))
  (ligature-array-ligature-count table))

(defun ligature-array-ligature-attach-offsets-ref (table index)
  "Array of offsets to LigatureAttach tables. Offsets are from beginning of LigatureArray table, ordered by ligatureCoverage index."
  (declare (type ligature-array table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u16 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-ligature-attach (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-ligature-array (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  (let* ((cnt (ligature-array-ligature-count-raw font-data offset))
         (pos (+ 2 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-ligature-array
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ligature-array font-data offset length)
  (%make-ligature-array font-data offset length))

(defstruct
    (ligature-attach (:include table-ref)
     (:constructor %make-ligature-attach
      (%table-font-data %table-offset %table-length)))
  "Part of [MarkLigPosFormat1]")

(declaim
 (inline ligature-attach-component-count ligature-attach-component-count-raw
  ligature-attach-component-records-start))

(defun ligature-attach-component-count (table)
  "Number of ComponentRecords in this ligature"
  (declare (type ligature-attach table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun ligature-attach-component-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun ligature-attach-component-records-start (table)
  "Byte position of the component-records field; its elements are variable-length COMPONENT-RECORD data."
  (declare (type ligature-attach table))
  (+ (table-ref-offset table) 2))

(defun sanitize-ligature-attach (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  t)

(defun read-ligature-attach
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ligature-attach font-data offset length)
  (%make-ligature-attach font-data offset length))

(defstruct
    (mark-mark-pos-format1 (:include table-ref)
     (:constructor %make-mark-mark-pos-format1
      (%table-font-data %table-offset %table-length)))
  "[Mark-to-Mark Attachment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#mark-to-mark-attachment-positioning-format-1-mark-to-mark-attachment): Mark-to-Mark Attachment")

(declaim
 (inline mark-mark-pos-format1-pos-format mark-mark-pos-format1-pos-format-raw
  mark-mark-pos-format1-mark1-coverage-offset-offset
  mark-mark-pos-format1-mark1-coverage-offset
  mark-mark-pos-format1-mark2-coverage-offset-offset
  mark-mark-pos-format1-mark2-coverage-offset
  mark-mark-pos-format1-mark-class-count
  mark-mark-pos-format1-mark-class-count-raw
  mark-mark-pos-format1-mark1-array-offset-offset
  mark-mark-pos-format1-mark1-array-offset
  mark-mark-pos-format1-mark2-array-offset-offset
  mark-mark-pos-format1-mark2-array-offset))

(defun mark-mark-pos-format1-pos-format (table)
  "Format identifier: format = 1"
  (declare (type mark-mark-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark-mark-pos-format1-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark-mark-pos-format1-mark1-coverage-offset-offset (table)
  "Offset to Combining Mark Coverage table, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun mark-mark-pos-format1-mark1-coverage-offset (table)
  "Offset to Combining Mark Coverage table, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format1 table))
  (let ((off (mark-mark-pos-format1-mark1-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-mark-pos-format1-mark2-coverage-offset-offset (table)
  "Offset to Base Mark Coverage table, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun mark-mark-pos-format1-mark2-coverage-offset (table)
  "Offset to Base Mark Coverage table, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format1 table))
  (let ((off (mark-mark-pos-format1-mark2-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-mark-pos-format1-mark-class-count (table)
  "Number of Combining Mark classes defined"
  (declare (type mark-mark-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun mark-mark-pos-format1-mark-class-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun mark-mark-pos-format1-mark1-array-offset-offset (table)
  "Offset to MarkArray table for mark1, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun mark-mark-pos-format1-mark1-array-offset (table)
  "Offset to MarkArray table for mark1, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format1 table))
  (let ((off (mark-mark-pos-format1-mark1-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark-array (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-mark-pos-format1-mark2-array-offset-offset (table)
  "Offset to Mark2Array table for mark2, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun mark-mark-pos-format1-mark2-array-offset (table)
  "Offset to Mark2Array table for mark2, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format1 table))
  (let ((off (mark-mark-pos-format1-mark2-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark2-array (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-mark-mark-pos-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-mark-mark-pos-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark-mark-pos-format1 font-data offset length)
  (%make-mark-mark-pos-format1 font-data offset length))

(defstruct
    (mark2-array (:include table-ref)
     (:constructor %make-mark2-array
      (%table-font-data %table-offset %table-length)))
  "Part of [MarkMarkPosFormat1]Class2Record")

(declaim
 (inline mark2-array-mark2-count mark2-array-mark2-count-raw
  mark2-array-mark2-records-start))

(defun mark2-array-mark2-count (table)
  "Number of Mark2 records"
  (declare (type mark2-array table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark2-array-mark2-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark2-array-mark2-records-start (table)
  "Byte position of the mark2-records field; its elements are variable-length MARK2-RECORD data."
  (declare (type mark2-array table))
  (+ (table-ref-offset table) 2))

(defun sanitize-mark2-array (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  t)

(defun read-mark2-array
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark2-array font-data offset length)
  (%make-mark2-array font-data offset length))

(defstruct
    (extension-pos-format1 (:include table-ref)
     (:constructor %make-extension-pos-format1
      (%table-font-data %table-offset %table-length)))
  "[Extension Positioning Subtable Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#extension-positioning-subtable-format-1)")

(declaim
 (inline extension-pos-format1-pos-format extension-pos-format1-pos-format-raw
  extension-pos-format1-extension-lookup-type
  extension-pos-format1-extension-lookup-type-raw
  extension-pos-format1-extension-offset-offset
  extension-pos-format1-extension-offset))

(defun extension-pos-format1-pos-format (table)
  "Format identifier: format = 1"
  (declare (type extension-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun extension-pos-format1-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun extension-pos-format1-extension-lookup-type (table)
  "Lookup type of subtable referenced by extensionOffset (i.e. the extension subtable)."
  (declare (type extension-pos-format1 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun extension-pos-format1-extension-lookup-type-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun extension-pos-format1-extension-offset-offset (table)
  "Offset to the extension subtable, of lookup type extensionLookupType, relative to the start of the ExtensionPosFormat1 subtable."
  (declare (type extension-pos-format1 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun extension-pos-format1-extension-offset (table)
  "Offset to the extension subtable, of lookup type extensionLookupType, relative to the start of the ExtensionPosFormat1 subtable."
  (declare (type extension-pos-format1 table))
  (let ((off (extension-pos-format1-extension-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun sanitize-extension-pos-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-extension-pos-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-extension-pos-format1 font-data offset length)
  (%make-extension-pos-format1 font-data offset length))

(defstruct
    (single-pos-format3 (:include table-ref)
     (:constructor %make-single-pos-format3
      (%table-font-data %table-offset %table-length)))
  "SinglePosFormat3 (ISO/IEC 14496-22:2026).")

(declaim
 (inline single-pos-format3-pos-format single-pos-format3-pos-format-raw
  single-pos-format3-coverage-offset-offset single-pos-format3-coverage-offset
  single-pos-format3-value-format single-pos-format3-value-format-raw
  single-pos-format3-value-record-start))

(defun single-pos-format3-pos-format (table)
  "Format identifier: format = 3"
  (declare (type single-pos-format3 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun single-pos-format3-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun single-pos-format3-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of SinglePos subtable."
  (declare (type single-pos-format3 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun single-pos-format3-coverage-offset (table)
  "Offset to Coverage table, from beginning of SinglePos subtable."
  (declare (type single-pos-format3 table))
  (let ((off (single-pos-format3-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun single-pos-format3-value-format (table)
  "Defines the types of data in the ValueRecord."
  (declare (type single-pos-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun single-pos-format3-value-format-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun single-pos-format3-value-record-start (table)
  "Byte position of the value-record field; its elements are variable-length GPOS-VALUE-RECORD data."
  (declare (type single-pos-format3 table))
  (+ (table-ref-offset table) 8))

(defun sanitize-single-pos-format3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-single-pos-format3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-single-pos-format3 font-data offset length)
  (%make-single-pos-format3 font-data offset length))

(defstruct
    (single-pos-format4 (:include table-ref)
     (:constructor %make-single-pos-format4
      (%table-font-data %table-offset %table-length)))
  "SinglePosFormat4 (ISO/IEC 14496-22:2026).")

(declaim
 (inline single-pos-format4-pos-format single-pos-format4-pos-format-raw
  single-pos-format4-coverage-offset-offset single-pos-format4-coverage-offset
  single-pos-format4-value-format single-pos-format4-value-format-raw
  single-pos-format4-value-count single-pos-format4-value-count-raw
  single-pos-format4-value-records-start))

(defun single-pos-format4-pos-format (table)
  "Format identifier: format = 4"
  (declare (type single-pos-format4 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun single-pos-format4-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun single-pos-format4-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of SinglePos subtable."
  (declare (type single-pos-format4 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun single-pos-format4-coverage-offset (table)
  "Offset to Coverage table, from beginning of SinglePos subtable."
  (declare (type single-pos-format4 table))
  (let ((off (single-pos-format4-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun single-pos-format4-value-format (table)
  "Defines the types of data in the ValueRecords."
  (declare (type single-pos-format4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun single-pos-format4-value-format-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun single-pos-format4-value-count (table)
  "Number of ValueRecords — must equal glyphCount in the Coverage table."
  (declare (type single-pos-format4 table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun single-pos-format4-value-count-raw (font-data offset)
  (font-data-read-u24 font-data (+ offset 8)))

(defun single-pos-format4-value-records-start (table)
  "Byte position of the value-records field; its elements are variable-length GPOS-VALUE-RECORD data."
  (declare (type single-pos-format4 table))
  (+ (table-ref-offset table) 11))

(defun sanitize-single-pos-format4 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 11 length) (font-data-valid-range-p font-data offset 11))
    (error 'font-out-of-bounds-error :pos offset :size 11 :length
           (font-data-length font-data)))
  t)

(defun read-single-pos-format4
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-single-pos-format4 font-data offset length)
  (%make-single-pos-format4 font-data offset length))

(defstruct
    (pair-pos-format3 (:include table-ref)
     (:constructor %make-pair-pos-format3
      (%table-font-data %table-offset %table-length)))
  "PairPosFormat3 (ISO/IEC 14496-22:2026).")

(declaim
 (inline pair-pos-format3-pos-format pair-pos-format3-pos-format-raw
  pair-pos-format3-coverage-offset-offset pair-pos-format3-coverage-offset
  pair-pos-format3-value-format1 pair-pos-format3-value-format1-raw
  pair-pos-format3-value-format2 pair-pos-format3-value-format2-raw
  pair-pos-format3-pair-set-count pair-pos-format3-pair-set-count-raw
  pair-pos-format3-pair-set-offsets-count
  pair-pos-format3-pair-set-offsets-ref))

(defun pair-pos-format3-pos-format (table)
  "Format identifier: format = 3"
  (declare (type pair-pos-format3 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun pair-pos-format3-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun pair-pos-format3-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of PairPos subtable."
  (declare (type pair-pos-format3 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun pair-pos-format3-coverage-offset (table)
  "Offset to Coverage table, from beginning of PairPos subtable."
  (declare (type pair-pos-format3 table))
  (let ((off (pair-pos-format3-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun pair-pos-format3-value-format1 (table)
  "Defines the types of data in valueRecord1 — for the first glyph in the pair (may be zero)."
  (declare (type pair-pos-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun pair-pos-format3-value-format1-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun pair-pos-format3-value-format2 (table)
  "Defines the types of data in valueRecord2 — for the second glyph in the pair (may be zero)."
  (declare (type pair-pos-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun pair-pos-format3-value-format2-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun pair-pos-format3-pair-set-count (table)
  "Number of PairSet tables"
  (declare (type pair-pos-format3 table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun pair-pos-format3-pair-set-count-raw (font-data offset)
  (font-data-read-u24 font-data (+ offset 10)))

(defun pair-pos-format3-pair-set-offsets-count (table)
  "Array of offsets to PairSet tables. Offsets are from beginning of PairPos subtable, ordered by Coverage Index."
  (declare (type pair-pos-format3 table))
  (declare (ignorable table))
  (pair-pos-format3-pair-set-count table))

(defun pair-pos-format3-pair-set-offsets-ref (table index)
  "Array of offsets to PairSet tables. Offsets are from beginning of PairPos subtable, ordered by Coverage Index."
  (declare (type pair-pos-format3 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 13) (* index 3))))
    (unless
        (<= (+ pos 3) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 3 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u24 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-pair-set2 (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-pair-pos-format3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 13 length) (font-data-valid-range-p font-data offset 13))
    (error 'font-out-of-bounds-error :pos offset :size 13 :length
           (font-data-length font-data)))
  (let* ((cnt (pair-pos-format3-pair-set-count-raw font-data offset))
         (pos (+ 13 (* cnt 3))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-pair-pos-format3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-pair-pos-format3 font-data offset length)
  (%make-pair-pos-format3 font-data offset length))

(defstruct
    (pair-set2 (:include table-ref)
     (:constructor %make-pair-set2
      (%table-font-data %table-offset %table-length)))
  "PairSet2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline pair-set2-pair-value-count pair-set2-pair-value-count-raw
  pair-set2-pair-value-records-start))

(defun pair-set2-pair-value-count (table)
  "Number of PairValueRecords"
  (declare (type pair-set2 table))
  (font-data-read-u24 (table-ref-font-data table) (table-ref-offset table)))

(defun pair-set2-pair-value-count-raw (font-data offset)
  (font-data-read-u24 font-data offset))

(defun pair-set2-pair-value-records-start (table)
  "Byte position of the pair-value-records field; its elements are variable-length PAIR-VALUE-RECORD2 data."
  (declare (type pair-set2 table))
  (+ (table-ref-offset table) 3))

(defun sanitize-pair-set2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  t)

(defun read-pair-set2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-pair-set2 font-data offset length)
  (%make-pair-set2 font-data offset length))

(defstruct
    (pair-pos-format4 (:include table-ref)
     (:constructor %make-pair-pos-format4
      (%table-font-data %table-offset %table-length)))
  "PairPosFormat4 (ISO/IEC 14496-22:2026).")

(declaim
 (inline pair-pos-format4-pos-format pair-pos-format4-pos-format-raw
  pair-pos-format4-coverage-offset-offset pair-pos-format4-coverage-offset
  pair-pos-format4-value-format1 pair-pos-format4-value-format1-raw
  pair-pos-format4-value-format2 pair-pos-format4-value-format2-raw
  pair-pos-format4-class-def1-offset-offset pair-pos-format4-class-def1-offset
  pair-pos-format4-class-def2-offset-offset pair-pos-format4-class-def2-offset
  pair-pos-format4-class1-count pair-pos-format4-class1-count-raw
  pair-pos-format4-class2-count pair-pos-format4-class2-count-raw
  pair-pos-format4-class1-records-start))

(defun pair-pos-format4-pos-format (table)
  "Format identifier: format = 4"
  (declare (type pair-pos-format4 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun pair-pos-format4-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun pair-pos-format4-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of PairPos subtable."
  (declare (type pair-pos-format4 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun pair-pos-format4-coverage-offset (table)
  "Offset to Coverage table, from beginning of PairPos subtable."
  (declare (type pair-pos-format4 table))
  (let ((off (pair-pos-format4-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun pair-pos-format4-value-format1 (table)
  "ValueRecord definition — for the first glyph of the pair (may be zero)."
  (declare (type pair-pos-format4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun pair-pos-format4-value-format1-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun pair-pos-format4-value-format2 (table)
  "ValueRecord definition — for the second glyph of the pair (may be zero)."
  (declare (type pair-pos-format4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun pair-pos-format4-value-format2-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun pair-pos-format4-class-def1-offset-offset (table)
  "Offset to ClassDef table, from beginning of PairPos subtable — for the first glyph of the pair."
  (declare (type pair-pos-format4 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun pair-pos-format4-class-def1-offset (table)
  "Offset to ClassDef table, from beginning of PairPos subtable — for the first glyph of the pair."
  (declare (type pair-pos-format4 table))
  (let ((off (pair-pos-format4-class-def1-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-class-def (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun pair-pos-format4-class-def2-offset-offset (table)
  "Offset to ClassDef table, from beginning of PairPos subtable — for the second glyph of the pair."
  (declare (type pair-pos-format4 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 14)))

(defun pair-pos-format4-class-def2-offset (table)
  "Offset to ClassDef table, from beginning of PairPos subtable — for the second glyph of the pair."
  (declare (type pair-pos-format4 table))
  (let ((off (pair-pos-format4-class-def2-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-class-def (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun pair-pos-format4-class1-count (table)
  "Number of classes in classDef1 table — includes Class 0."
  (declare (type pair-pos-format4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 18)))

(defun pair-pos-format4-class1-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 18)))

(defun pair-pos-format4-class2-count (table)
  "Number of classes in classDef2 table — includes Class 0."
  (declare (type pair-pos-format4 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 20)))

(defun pair-pos-format4-class2-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 20)))

(defun pair-pos-format4-class1-records-start (table)
  "Byte position of the class1-records field; its elements are variable-length CLASS1-RECORD data."
  (declare (type pair-pos-format4 table))
  (+ (table-ref-offset table) 22))

(defun sanitize-pair-pos-format4 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 22 length) (font-data-valid-range-p font-data offset 22))
    (error 'font-out-of-bounds-error :pos offset :size 22 :length
           (font-data-length font-data)))
  t)

(defun read-pair-pos-format4
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-pair-pos-format4 font-data offset length)
  (%make-pair-pos-format4 font-data offset length))

(defstruct
    (cursive-pos-format2 (:include table-ref)
     (:constructor %make-cursive-pos-format2
      (%table-font-data %table-offset %table-length)))
  "CursivePosFormat2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline cursive-pos-format2-pos-format cursive-pos-format2-pos-format-raw
  cursive-pos-format2-coverage-offset-offset
  cursive-pos-format2-coverage-offset cursive-pos-format2-entry-exit-count
  cursive-pos-format2-entry-exit-count-raw
  cursive-pos-format2-entry-exit-record-count
  cursive-pos-format2-entry-exit-record-ref))

(defun cursive-pos-format2-pos-format (table)
  "Format identifier: format = 2"
  (declare (type cursive-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun cursive-pos-format2-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun cursive-pos-format2-coverage-offset-offset (table)
  "Offset to Coverage table, from beginning of CursivePos subtable."
  (declare (type cursive-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun cursive-pos-format2-coverage-offset (table)
  "Offset to Coverage table, from beginning of CursivePos subtable."
  (declare (type cursive-pos-format2 table))
  (let ((off (cursive-pos-format2-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun cursive-pos-format2-entry-exit-count (table)
  "Number of EntryExit records"
  (declare (type cursive-pos-format2 table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun cursive-pos-format2-entry-exit-count-raw (font-data offset)
  (font-data-read-u24 font-data (+ offset 6)))

(defun cursive-pos-format2-entry-exit-record-count (table)
  "Array of EntryExit records, in Coverage index order."
  (declare (type cursive-pos-format2 table))
  (declare (ignorable table))
  (cursive-pos-format2-entry-exit-count table))

(defun cursive-pos-format2-entry-exit-record-ref (table index)
  "Array of EntryExit records, in Coverage index order."
  (declare (type cursive-pos-format2 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 9) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-entry-exit-record2 (table-ref-font-data table) pos)))

(defun sanitize-cursive-pos-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 9 length) (font-data-valid-range-p font-data offset 9))
    (error 'font-out-of-bounds-error :pos offset :size 9 :length
           (font-data-length font-data)))
  (let* ((cnt (cursive-pos-format2-entry-exit-count-raw font-data offset))
         (pos (+ 9 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-cursive-pos-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cursive-pos-format2 font-data offset length)
  (%make-cursive-pos-format2 font-data offset length))

(defstruct
    (mark-base-pos-format2 (:include table-ref)
     (:constructor %make-mark-base-pos-format2
      (%table-font-data %table-offset %table-length)))
  "MarkBasePosFormat2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline mark-base-pos-format2-pos-format mark-base-pos-format2-pos-format-raw
  mark-base-pos-format2-mark-coverage-offset-offset
  mark-base-pos-format2-mark-coverage-offset
  mark-base-pos-format2-base-coverage-offset-offset
  mark-base-pos-format2-base-coverage-offset
  mark-base-pos-format2-mark-class-count
  mark-base-pos-format2-mark-class-count-raw
  mark-base-pos-format2-mark-array-offset-offset
  mark-base-pos-format2-mark-array-offset
  mark-base-pos-format2-base-array-offset-offset
  mark-base-pos-format2-base-array-offset))

(defun mark-base-pos-format2-pos-format (table)
  "Format identifier: format = 2"
  (declare (type mark-base-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark-base-pos-format2-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark-base-pos-format2-mark-coverage-offset-offset (table)
  "Offset to markCoverage table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun mark-base-pos-format2-mark-coverage-offset (table)
  "Offset to markCoverage table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format2 table))
  (let ((off (mark-base-pos-format2-mark-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-base-pos-format2-base-coverage-offset-offset (table)
  "Offset to baseCoverage table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun mark-base-pos-format2-base-coverage-offset (table)
  "Offset to baseCoverage table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format2 table))
  (let ((off (mark-base-pos-format2-base-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-base-pos-format2-mark-class-count (table)
  "Number of classes defined for marks"
  (declare (type mark-base-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun mark-base-pos-format2-mark-class-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun mark-base-pos-format2-mark-array-offset-offset (table)
  "Offset to MarkArray table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun mark-base-pos-format2-mark-array-offset (table)
  "Offset to MarkArray table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format2 table))
  (let ((off (mark-base-pos-format2-mark-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark-array2 (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-base-pos-format2-base-array-offset-offset (table)
  "Offset to BaseArray table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun mark-base-pos-format2-base-array-offset (table)
  "Offset to BaseArray table, from beginning of MarkBasePos subtable."
  (declare (type mark-base-pos-format2 table))
  (let ((off (mark-base-pos-format2-base-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-base-array2 (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-mark-base-pos-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 20 length) (font-data-valid-range-p font-data offset 20))
    (error 'font-out-of-bounds-error :pos offset :size 20 :length
           (font-data-length font-data)))
  t)

(defun read-mark-base-pos-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark-base-pos-format2 font-data offset length)
  (%make-mark-base-pos-format2 font-data offset length))

(defstruct
    (base-array2 (:include table-ref)
     (:constructor %make-base-array2
      (%table-font-data %table-offset %table-length)))
  "BaseArray2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline base-array2-base-count base-array2-base-count-raw
  base-array2-base-records-start))

(defun base-array2-base-count (table)
  "Number of BaseRecords"
  (declare (type base-array2 table))
  (font-data-read-u24 (table-ref-font-data table) (table-ref-offset table)))

(defun base-array2-base-count-raw (font-data offset)
  (font-data-read-u24 font-data offset))

(defun base-array2-base-records-start (table)
  "Byte position of the base-records field; its elements are variable-length BASE-RECORD2 data."
  (declare (type base-array2 table))
  (+ (table-ref-offset table) 3))

(defun sanitize-base-array2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  t)

(defun read-base-array2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-array2 font-data offset length)
  (%make-base-array2 font-data offset length))

(defstruct
    (mark-lig-pos-format2 (:include table-ref)
     (:constructor %make-mark-lig-pos-format2
      (%table-font-data %table-offset %table-length)))
  "MarkLigPosFormat2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline mark-lig-pos-format2-pos-format mark-lig-pos-format2-pos-format-raw
  mark-lig-pos-format2-mark-coverage-offset-offset
  mark-lig-pos-format2-mark-coverage-offset
  mark-lig-pos-format2-ligature-coverage-offset-offset
  mark-lig-pos-format2-ligature-coverage-offset
  mark-lig-pos-format2-mark-class-count
  mark-lig-pos-format2-mark-class-count-raw
  mark-lig-pos-format2-mark-array-offset-offset
  mark-lig-pos-format2-mark-array-offset
  mark-lig-pos-format2-ligature-array-offset-offset
  mark-lig-pos-format2-ligature-array-offset))

(defun mark-lig-pos-format2-pos-format (table)
  "Format identifier: format = 2"
  (declare (type mark-lig-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark-lig-pos-format2-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark-lig-pos-format2-mark-coverage-offset-offset (table)
  "Offset to markCoverage table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun mark-lig-pos-format2-mark-coverage-offset (table)
  "Offset to markCoverage table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format2 table))
  (let ((off (mark-lig-pos-format2-mark-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-lig-pos-format2-ligature-coverage-offset-offset (table)
  "Offset to ligatureCoverage table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun mark-lig-pos-format2-ligature-coverage-offset (table)
  "Offset to ligatureCoverage table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format2 table))
  (let ((off (mark-lig-pos-format2-ligature-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-lig-pos-format2-mark-class-count (table)
  "Number of defined mark classes"
  (declare (type mark-lig-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun mark-lig-pos-format2-mark-class-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun mark-lig-pos-format2-mark-array-offset-offset (table)
  "Offset to MarkArray table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun mark-lig-pos-format2-mark-array-offset (table)
  "Offset to MarkArray table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format2 table))
  (let ((off (mark-lig-pos-format2-mark-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark-array2 (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-lig-pos-format2-ligature-array-offset-offset (table)
  "Offset to LigatureArray table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun mark-lig-pos-format2-ligature-array-offset (table)
  "Offset to LigatureArray table, from beginning of MarkLigPos subtable."
  (declare (type mark-lig-pos-format2 table))
  (let ((off (mark-lig-pos-format2-ligature-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-ligature-array2 (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-mark-lig-pos-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 20 length) (font-data-valid-range-p font-data offset 20))
    (error 'font-out-of-bounds-error :pos offset :size 20 :length
           (font-data-length font-data)))
  t)

(defun read-mark-lig-pos-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark-lig-pos-format2 font-data offset length)
  (%make-mark-lig-pos-format2 font-data offset length))

(defstruct
    (ligature-array2 (:include table-ref)
     (:constructor %make-ligature-array2
      (%table-font-data %table-offset %table-length)))
  "LigatureArray2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline ligature-array2-ligature-count ligature-array2-ligature-count-raw
  ligature-array2-ligature-attach-offsets-count
  ligature-array2-ligature-attach-offsets-ref))

(defun ligature-array2-ligature-count (table)
  "Number of LigatureAttach table offsets"
  (declare (type ligature-array2 table))
  (font-data-read-u24 (table-ref-font-data table) (table-ref-offset table)))

(defun ligature-array2-ligature-count-raw (font-data offset)
  (font-data-read-u24 font-data offset))

(defun ligature-array2-ligature-attach-offsets-count (table)
  "Array of offsets to LigatureAttach tables. Offsets are from beginning of LigatureArray2 table, ordered by ligatureCoverage index."
  (declare (type ligature-array2 table))
  (declare (ignorable table))
  (ligature-array2-ligature-count table))

(defun ligature-array2-ligature-attach-offsets-ref (table index)
  "Array of offsets to LigatureAttach tables. Offsets are from beginning of LigatureArray2 table, ordered by ligatureCoverage index."
  (declare (type ligature-array2 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 3) (* index 3))))
    (unless
        (<= (+ pos 3) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 3 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u24 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-ligature-attach2 (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-ligature-array2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  (let* ((cnt (ligature-array2-ligature-count-raw font-data offset))
         (pos (+ 3 (* cnt 3))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-ligature-array2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ligature-array2 font-data offset length)
  (%make-ligature-array2 font-data offset length))

(defstruct
    (ligature-attach2 (:include table-ref)
     (:constructor %make-ligature-attach2
      (%table-font-data %table-offset %table-length)))
  "LigatureAttach2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline ligature-attach2-component-count ligature-attach2-component-count-raw
  ligature-attach2-component-records-start))

(defun ligature-attach2-component-count (table)
  "Number of ComponentRecords in this ligature"
  (declare (type ligature-attach2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun ligature-attach2-component-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun ligature-attach2-component-records-start (table)
  "Byte position of the component-records field; its elements are variable-length COMPONENT-RECORD2 data."
  (declare (type ligature-attach2 table))
  (+ (table-ref-offset table) 2))

(defun sanitize-ligature-attach2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  t)

(defun read-ligature-attach2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ligature-attach2 font-data offset length)
  (%make-ligature-attach2 font-data offset length))

(defstruct
    (mark-mark-pos-format2 (:include table-ref)
     (:constructor %make-mark-mark-pos-format2
      (%table-font-data %table-offset %table-length)))
  "MarkMarkPosFormat2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline mark-mark-pos-format2-pos-format mark-mark-pos-format2-pos-format-raw
  mark-mark-pos-format2-mark1-coverage-offset-offset
  mark-mark-pos-format2-mark1-coverage-offset
  mark-mark-pos-format2-mark2-coverage-offset-offset
  mark-mark-pos-format2-mark2-coverage-offset
  mark-mark-pos-format2-mark-class-count
  mark-mark-pos-format2-mark-class-count-raw
  mark-mark-pos-format2-mark1-array-offset-offset
  mark-mark-pos-format2-mark1-array-offset
  mark-mark-pos-format2-mark2-array-offset-offset
  mark-mark-pos-format2-mark2-array-offset))

(defun mark-mark-pos-format2-pos-format (table)
  "Format identifier: format = 2"
  (declare (type mark-mark-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark-mark-pos-format2-pos-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark-mark-pos-format2-mark1-coverage-offset-offset (table)
  "Offset to Combining Mark Coverage table, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun mark-mark-pos-format2-mark1-coverage-offset (table)
  "Offset to Combining Mark Coverage table, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format2 table))
  (let ((off (mark-mark-pos-format2-mark1-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-mark-pos-format2-mark2-coverage-offset-offset (table)
  "Offset to Base Mark Coverage table, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun mark-mark-pos-format2-mark2-coverage-offset (table)
  "Offset to Base Mark Coverage table, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format2 table))
  (let ((off (mark-mark-pos-format2-mark2-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-mark-pos-format2-mark-class-count (table)
  "Number of Combining Mark classes defined"
  (declare (type mark-mark-pos-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun mark-mark-pos-format2-mark-class-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 10)))

(defun mark-mark-pos-format2-mark1-array-offset-offset (table)
  "Offset to MarkArray table for mark1, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun mark-mark-pos-format2-mark1-array-offset (table)
  "Offset to MarkArray table for mark1, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format2 table))
  (let ((off (mark-mark-pos-format2-mark1-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark-array2 (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun mark-mark-pos-format2-mark2-array-offset-offset (table)
  "Offset to Mark2Array table for mark2, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun mark-mark-pos-format2-mark2-array-offset (table)
  "Offset to Mark2Array table for mark2, from beginning of MarkMarkPos subtable."
  (declare (type mark-mark-pos-format2 table))
  (let ((off (mark-mark-pos-format2-mark2-array-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark2-array2 (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-mark-mark-pos-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 20 length) (font-data-valid-range-p font-data offset 20))
    (error 'font-out-of-bounds-error :pos offset :size 20 :length
           (font-data-length font-data)))
  t)

(defun read-mark-mark-pos-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark-mark-pos-format2 font-data offset length)
  (%make-mark-mark-pos-format2 font-data offset length))

(defstruct
    (mark2-array2 (:include table-ref)
     (:constructor %make-mark2-array2
      (%table-font-data %table-offset %table-length)))
  "Mark2Array2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline mark2-array2-mark2-count mark2-array2-mark2-count-raw
  mark2-array2-mark2-records-start))

(defun mark2-array2-mark2-count (table)
  "Number of Mark2 records"
  (declare (type mark2-array2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark2-array2-mark2-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark2-array2-mark2-records-start (table)
  "Byte position of the mark2-records field; its elements are variable-length MARK2-RECORD2 data."
  (declare (type mark2-array2 table))
  (+ (table-ref-offset table) 2))

(defun sanitize-mark2-array2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  t)

(defun read-mark2-array2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark2-array2 font-data offset length)
  (%make-mark2-array2 font-data offset length))

(defstruct
    (mark-array2 (:include table-ref)
     (:constructor %make-mark-array2
      (%table-font-data %table-offset %table-length)))
  "MarkArray2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline mark-array2-mark-count mark-array2-mark-count-raw
  mark-array2-mark-records-count mark-array2-mark-records-ref))

(defun mark-array2-mark-count (table)
  "Number of MarkRecords"
  (declare (type mark-array2 table))
  (font-data-read-u24 (table-ref-font-data table) (table-ref-offset table)))

(defun mark-array2-mark-count-raw (font-data offset)
  (font-data-read-u24 font-data offset))

(defun mark-array2-mark-records-count (table)
  "Array of MarkRecords, ordered by corresponding glyphs in the associated mark Coverage table."
  (declare (type mark-array2 table))
  (declare (ignorable table))
  (mark-array2-mark-count table))

(defun mark-array2-mark-records-ref (table index)
  "Array of MarkRecords, ordered by corresponding glyphs in the associated mark Coverage table."
  (declare (type mark-array2 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 3) (* index 5))))
    (unless
        (<= (+ pos 5) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 5 :length
             (table-ref-length table)))
    (%make-mark-record2 (table-ref-font-data table) pos)))

(defun sanitize-mark-array2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  (let* ((cnt (mark-array2-mark-count-raw font-data offset))
         (pos (+ 3 (* cnt 5))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-mark-array2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark-array2 font-data offset length)
  (%make-mark-array2 font-data offset length))

(defun read-cursive-pos
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "CursivePos (ISO/IEC 14496-22:2026)."
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-cursive-pos-format1 font-data offset length))
      (2 (read-cursive-pos-format2 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'cursive-pos)))))

(defun read-mark-base-pos
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "MarkBasePos (ISO/IEC 14496-22:2026)."
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-mark-base-pos-format1 font-data offset length))
      (2 (read-mark-base-pos-format2 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'mark-base-pos)))))

(defun read-mark-lig-pos
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "MarkLigPos (ISO/IEC 14496-22:2026)."
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-mark-lig-pos-format1 font-data offset length))
      (2 (read-mark-lig-pos-format2 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'mark-lig-pos)))))

(defun read-mark-mark-pos
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "MarkMarkPos (ISO/IEC 14496-22:2026)."
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-mark-mark-pos-format1 font-data offset length))
      (2 (read-mark-mark-pos-format2 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'mark-mark-pos)))))
