;;;; base.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+base-lang-sys-record-size+ +base-script-record-size+ +base-tag+
   +feat-min-max-record-size+ axis axis-base-script-list-offset
   axis-base-script-list-offset-offset axis-base-tag-list-offset
   axis-base-tag-list-offset-offset axis-p base base-coord-format1
   base-coord-format1-base-coord-format base-coord-format1-coordinate
   base-coord-format1-p base-coord-format2 base-coord-format2-base-coord-format
   base-coord-format2-base-coord-point base-coord-format2-coordinate
   base-coord-format2-p base-coord-format2-reference-glyph base-coord-format3
   base-coord-format3-base-coord-format base-coord-format3-coordinate
   base-coord-format3-device-offset base-coord-format3-device-offset-offset
   base-coord-format3-p base-horiz-axis-offset base-horiz-axis-offset-offset
   base-item-var-store-offset base-item-var-store-offset-offset
   base-lang-sys-record base-lang-sys-record-base-lang-sys-tag
   base-lang-sys-record-min-max-offset base-lang-sys-record-p base-p
   base-script base-script-base-lang-sys-count
   base-script-base-lang-sys-records-count
   base-script-base-lang-sys-records-ref base-script-base-values-offset
   base-script-base-values-offset-offset base-script-default-min-max-offset
   base-script-default-min-max-offset-offset base-script-list
   base-script-list-base-script-count
   base-script-list-base-script-records-count
   base-script-list-base-script-records-ref base-script-list-p base-script-p
   base-script-record base-script-record-base-script-offset
   base-script-record-base-script-tag base-script-record-p base-tag-list
   base-tag-list-base-tag-count base-tag-list-baseline-tags-count
   base-tag-list-baseline-tags-ref base-tag-list-p base-values
   base-values-base-coord-count base-values-base-coord-offsets-count
   base-values-base-coord-offsets-ref base-values-default-baseline-index
   base-values-p base-version base-vert-axis-offset
   base-vert-axis-offset-offset feat-min-max-record
   feat-min-max-record-feature-table-tag feat-min-max-record-max-coord-offset
   feat-min-max-record-min-coord-offset feat-min-max-record-p min-max
   min-max-feat-min-max-count min-max-feat-min-max-records-count
   min-max-feat-min-max-records-ref min-max-max-coord-offset
   min-max-max-coord-offset-offset min-max-min-coord-offset
   min-max-min-coord-offset-offset min-max-p read-axis read-base
   read-base-coord read-base-coord-format1 read-base-coord-format2
   read-base-coord-format3 read-base-script read-base-script-list
   read-base-tag-list read-base-values read-min-max sanitize-axis sanitize-base
   sanitize-base-coord-format1 sanitize-base-coord-format2
   sanitize-base-coord-format3 sanitize-base-script sanitize-base-script-list
   sanitize-base-tag-list sanitize-base-values sanitize-min-max))

(defstruct
    (base-script-record (:include record-ref)
     (:constructor %make-base-script-record
      (%record-font-data %record-offset)))
  "[BaseScriptRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basescriptrecord)")

(define-constant +base-script-record-size+ 6)

(declaim
 (inline base-script-record-base-script-tag
  base-script-record-base-script-offset))

(defun base-script-record-base-script-tag (rec)
  "4-byte script identification tag"
  (declare (type base-script-record rec))
  (font-data-read-tag-string (record-ref-font-data rec)
                             (record-ref-offset rec)))

(defun base-script-record-base-script-offset (rec)
  "Offset to BaseScript table, from beginning of BaseScriptList"
  (declare (type base-script-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defstruct
    (base-lang-sys-record (:include record-ref)
     (:constructor %make-base-lang-sys-record
      (%record-font-data %record-offset)))
  "[BaseLangSysRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/base#baselangsysrecord)")

(define-constant +base-lang-sys-record-size+ 6)

(declaim
 (inline base-lang-sys-record-base-lang-sys-tag
  base-lang-sys-record-min-max-offset))

(defun base-lang-sys-record-base-lang-sys-tag (rec)
  "4-byte language system identification tag"
  (declare (type base-lang-sys-record rec))
  (font-data-read-tag-string (record-ref-font-data rec)
                             (record-ref-offset rec)))

(defun base-lang-sys-record-min-max-offset (rec)
  "Offset to MinMax table, from beginning of BaseScript table"
  (declare (type base-lang-sys-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defstruct
    (feat-min-max-record (:include record-ref)
     (:constructor %make-feat-min-max-record
      (%record-font-data %record-offset)))
  "[FeatMinMaxRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/base#baselangsysrecord)")

(define-constant +feat-min-max-record-size+ 8)

(declaim
 (inline feat-min-max-record-feature-table-tag
  feat-min-max-record-min-coord-offset feat-min-max-record-max-coord-offset))

(defun feat-min-max-record-feature-table-tag (rec)
  "4-byte feature identification tag — must match feature tag in FeatureList"
  (declare (type feat-min-max-record rec))
  (font-data-read-tag-string (record-ref-font-data rec)
                             (record-ref-offset rec)))

(defun feat-min-max-record-min-coord-offset (rec)
  "Offset to BaseCoord table that defines the minimum extent value, from beginning of MinMax table (may be NULL)"
  (declare (type feat-min-max-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun feat-min-max-record-max-coord-offset (rec)
  "Offset to BaseCoord table that defines the maximum extent value, from beginning of MinMax table (may be NULL)"
  (declare (type feat-min-max-record rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 6)))

(defstruct
    (base (:include table-ref)
     (:constructor %make-base (%table-font-data %table-offset %table-length)))
  "The [BASE](https://learn.microsoft.com/en-us/typography/opentype/spec/base) (Baseline) table")

(declaim
 (inline base-version base-version-raw base-horiz-axis-offset-offset
  base-horiz-axis-offset base-vert-axis-offset-offset base-vert-axis-offset
  base-item-var-store-offset-offset base-item-var-store-offset))

(define-constant +base-tag+ "BASE")

(defun base-version (table)
  "(major, minor) Version for the BASE table (1,0) or (1,1)"
  (declare (type base table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun base-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun base-horiz-axis-offset-offset (table)
  "Offset to horizontal Axis table, from beginning of BASE table (may be NULL)"
  (declare (type base table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun base-horiz-axis-offset (table)
  "Offset to horizontal Axis table, from beginning of BASE table (may be NULL)"
  (declare (type base table))
  (let ((off (base-horiz-axis-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-axis (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun base-vert-axis-offset-offset (table)
  "Offset to vertical Axis table, from beginning of BASE table (may be NULL)"
  (declare (type base table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun base-vert-axis-offset (table)
  "Offset to vertical Axis table, from beginning of BASE table (may be NULL)"
  (declare (type base table))
  (let ((off (base-vert-axis-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-axis (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun base-item-var-store-offset-offset (table)
  "Offset to Item Variation Store table, from beginning of BASE table (may be null)"
  (declare (type base table))
  (when
      (and (>= (base-version table) 65537)
           (<= (+ (+ (table-ref-offset table) 8) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 8))))

(defun base-item-var-store-offset (table)
  "Offset to Item Variation Store table, from beginning of BASE table (may be null)"
  (declare (type base table))
  (let ((off (base-item-var-store-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-item-variation-store (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-base (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-base
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base font-data offset length)
  (%make-base font-data offset length))

(defstruct
    (axis (:include table-ref)
     (:constructor %make-axis (%table-font-data %table-offset %table-length)))
  "[Axis Table](https://learn.microsoft.com/en-us/typography/opentype/spec/base#axis-tables-horizaxis-and-vertaxis)")

(declaim
 (inline axis-base-tag-list-offset-offset axis-base-tag-list-offset
  axis-base-script-list-offset-offset axis-base-script-list-offset))

(defun axis-base-tag-list-offset-offset (table)
  "Offset to BaseTagList table, from beginning of Axis table (may be NULL)"
  (declare (type axis table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun axis-base-tag-list-offset (table)
  "Offset to BaseTagList table, from beginning of Axis table (may be NULL)"
  (declare (type axis table))
  (let ((off (axis-base-tag-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-base-tag-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun axis-base-script-list-offset-offset (table)
  "Offset to BaseScriptList table, from beginning of Axis table"
  (declare (type axis table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun axis-base-script-list-offset (table)
  "Offset to BaseScriptList table, from beginning of Axis table"
  (declare (type axis table))
  (let ((off (axis-base-script-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-base-script-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-axis (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-axis
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-axis font-data offset length)
  (%make-axis font-data offset length))

(defstruct
    (base-tag-list (:include table-ref)
     (:constructor %make-base-tag-list
      (%table-font-data %table-offset %table-length)))
  "[BaseTagList Table](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basetaglist-table)")

(declaim
 (inline base-tag-list-base-tag-count base-tag-list-base-tag-count-raw
  base-tag-list-baseline-tags-count base-tag-list-baseline-tags-ref))

(defun base-tag-list-base-tag-count (table)
  "Number of baseline identification tags in this text direction — may be zero (0)"
  (declare (type base-tag-list table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun base-tag-list-base-tag-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun base-tag-list-baseline-tags-count (table)
  "Array of 4-byte baseline identification tags — must be in alphabetical order"
  (declare (type base-tag-list table))
  (declare (ignorable table))
  (base-tag-list-base-tag-count table))

(defun base-tag-list-baseline-tags-ref (table index)
  "Array of 4-byte baseline identification tags — must be in alphabetical order"
  (declare (type base-tag-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (font-data-read-tag-string (table-ref-font-data table) pos)))

(defun sanitize-base-tag-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  (let* ((cnt (base-tag-list-base-tag-count-raw font-data offset))
         (pos (+ 2 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-base-tag-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-tag-list font-data offset length)
  (%make-base-tag-list font-data offset length))

(defstruct
    (base-script-list (:include table-ref)
     (:constructor %make-base-script-list
      (%table-font-data %table-offset %table-length)))
  "[BaseScriptList Table](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basescriptlist-table)")

(declaim
 (inline base-script-list-base-script-count
  base-script-list-base-script-count-raw
  base-script-list-base-script-records-count
  base-script-list-base-script-records-ref))

(defun base-script-list-base-script-count (table)
  "Number of BaseScriptRecords defined"
  (declare (type base-script-list table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun base-script-list-base-script-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun base-script-list-base-script-records-count (table)
  "Array of BaseScriptRecords, in alphabetical order by baseScriptTag"
  (declare (type base-script-list table))
  (declare (ignorable table))
  (base-script-list-base-script-count table))

(defun base-script-list-base-script-records-ref (table index)
  "Array of BaseScriptRecords, in alphabetical order by baseScriptTag"
  (declare (type base-script-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-base-script-record (table-ref-font-data table) pos)))

(defun sanitize-base-script-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  (let* ((cnt (base-script-list-base-script-count-raw font-data offset))
         (pos (+ 2 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-base-script-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-script-list font-data offset length)
  (%make-base-script-list font-data offset length))

(defstruct
    (base-script (:include table-ref)
     (:constructor %make-base-script
      (%table-font-data %table-offset %table-length)))
  "[BaseScript Table](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basescript-table)")

(declaim
 (inline base-script-base-values-offset-offset base-script-base-values-offset
  base-script-default-min-max-offset-offset base-script-default-min-max-offset
  base-script-base-lang-sys-count base-script-base-lang-sys-count-raw
  base-script-base-lang-sys-records-count
  base-script-base-lang-sys-records-ref))

(defun base-script-base-values-offset-offset (table)
  "Offset to BaseValues table, from beginning of BaseScript table (may be NULL)"
  (declare (type base-script table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun base-script-base-values-offset (table)
  "Offset to BaseValues table, from beginning of BaseScript table (may be NULL)"
  (declare (type base-script table))
  (let ((off (base-script-base-values-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-base-values (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun base-script-default-min-max-offset-offset (table)
  "Offset to MinMax table, from beginning of BaseScript table (may be NULL)"
  (declare (type base-script table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun base-script-default-min-max-offset (table)
  "Offset to MinMax table, from beginning of BaseScript table (may be NULL)"
  (declare (type base-script table))
  (let ((off (base-script-default-min-max-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-min-max (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun base-script-base-lang-sys-count (table)
  "Number of BaseLangSysRecords defined — may be zero (0)"
  (declare (type base-script table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun base-script-base-lang-sys-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun base-script-base-lang-sys-records-count (table)
  "Array of BaseLangSysRecords, in alphabetical order by BaseLangSysTag"
  (declare (type base-script table))
  (declare (ignorable table))
  (base-script-base-lang-sys-count table))

(defun base-script-base-lang-sys-records-ref (table index)
  "Array of BaseLangSysRecords, in alphabetical order by BaseLangSysTag"
  (declare (type base-script table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 6) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-base-lang-sys-record (table-ref-font-data table) pos)))

(defun sanitize-base-script (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  (let* ((cnt (base-script-base-lang-sys-count-raw font-data offset))
         (pos (+ 6 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-base-script
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-script font-data offset length)
  (%make-base-script font-data offset length))

(defstruct
    (base-values (:include table-ref)
     (:constructor %make-base-values
      (%table-font-data %table-offset %table-length)))
  "[BaseValues](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basevalues-table) table")

(declaim
 (inline base-values-default-baseline-index
  base-values-default-baseline-index-raw base-values-base-coord-count
  base-values-base-coord-count-raw base-values-base-coord-offsets-count
  base-values-base-coord-offsets-ref))

(defun base-values-default-baseline-index (table)
  "Index number of default baseline for this script — equals index position of baseline tag in baselineTags array of the BaseTagList"
  (declare (type base-values table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun base-values-default-baseline-index-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun base-values-base-coord-count (table)
  "Number of BaseCoord tables defined — should equal baseTagCount in the BaseTagList"
  (declare (type base-values table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun base-values-base-coord-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun base-values-base-coord-offsets-count (table)
  "Array of offsets to BaseCoord tables, from beginning of BaseValues table — order matches baselineTags array in the BaseTagList"
  (declare (type base-values table))
  (declare (ignorable table))
  (base-values-base-coord-count table))

(defun base-values-base-coord-offsets-ref (table index)
  "Array of offsets to BaseCoord tables, from beginning of BaseValues table — order matches baselineTags array in the BaseTagList"
  (declare (type base-values table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u16 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-base-coord (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-base-values (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (base-values-base-coord-count-raw font-data offset))
         (pos (+ 4 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-base-values
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-values font-data offset length)
  (%make-base-values font-data offset length))

(defstruct
    (min-max (:include table-ref)
     (:constructor %make-min-max
      (%table-font-data %table-offset %table-length)))
  "[MinMax](https://learn.microsoft.com/en-us/typography/opentype/spec/base#minmax-table) table")

(declaim
 (inline min-max-min-coord-offset-offset min-max-min-coord-offset
  min-max-max-coord-offset-offset min-max-max-coord-offset
  min-max-feat-min-max-count min-max-feat-min-max-count-raw
  min-max-feat-min-max-records-count min-max-feat-min-max-records-ref))

(defun min-max-min-coord-offset-offset (table)
  "Offset to BaseCoord table that defines the minimum extent value, from the beginning of MinMax table (may be NULL)"
  (declare (type min-max table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun min-max-min-coord-offset (table)
  "Offset to BaseCoord table that defines the minimum extent value, from the beginning of MinMax table (may be NULL)"
  (declare (type min-max table))
  (let ((off (min-max-min-coord-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-base-coord (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun min-max-max-coord-offset-offset (table)
  "Offset to BaseCoord table that defines maximum extent value, from the beginning of MinMax table (may be NULL)"
  (declare (type min-max table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun min-max-max-coord-offset (table)
  "Offset to BaseCoord table that defines maximum extent value, from the beginning of MinMax table (may be NULL)"
  (declare (type min-max table))
  (let ((off (min-max-max-coord-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-base-coord (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun min-max-feat-min-max-count (table)
  "Number of FeatMinMaxRecords — may be zero (0)"
  (declare (type min-max table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun min-max-feat-min-max-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun min-max-feat-min-max-records-count (table)
  "Array of FeatMinMaxRecords, in alphabetical order by featureTableTag"
  (declare (type min-max table))
  (declare (ignorable table))
  (min-max-feat-min-max-count table))

(defun min-max-feat-min-max-records-ref (table index)
  "Array of FeatMinMaxRecords, in alphabetical order by featureTableTag"
  (declare (type min-max table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 6) (* index 8))))
    (unless
        (<= (+ pos 8) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 8 :length
             (table-ref-length table)))
    (%make-feat-min-max-record (table-ref-font-data table) pos)))

(defun sanitize-min-max (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  (let* ((cnt (min-max-feat-min-max-count-raw font-data offset))
         (pos (+ 6 (* cnt 8))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-min-max
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-min-max font-data offset length)
  (%make-min-max font-data offset length))

(defun read-base-coord
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-base-coord-format1 font-data offset length))
      (2 (read-base-coord-format2 font-data offset length))
      (3 (read-base-coord-format3 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'base-coord)))))

(defstruct
    (base-coord-format1 (:include table-ref)
     (:constructor %make-base-coord-format1
      (%table-font-data %table-offset %table-length)))
  "[BaseCoordFormat1](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basecoord-format-1)")

(declaim
 (inline base-coord-format1-base-coord-format
  base-coord-format1-base-coord-format-raw base-coord-format1-coordinate
  base-coord-format1-coordinate-raw))

(defun base-coord-format1-base-coord-format (table)
  "Format identifier — format = 1"
  (declare (type base-coord-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun base-coord-format1-base-coord-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun base-coord-format1-coordinate (table)
  "X or Y value, in design units"
  (declare (type base-coord-format1 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun base-coord-format1-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun sanitize-base-coord-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-base-coord-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-coord-format1 font-data offset length)
  (%make-base-coord-format1 font-data offset length))

(defstruct
    (base-coord-format2 (:include table-ref)
     (:constructor %make-base-coord-format2
      (%table-font-data %table-offset %table-length)))
  "[BaseCoordFormat2](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basecoord-format-2)")

(declaim
 (inline base-coord-format2-base-coord-format
  base-coord-format2-base-coord-format-raw base-coord-format2-coordinate
  base-coord-format2-coordinate-raw base-coord-format2-reference-glyph
  base-coord-format2-reference-glyph-raw base-coord-format2-base-coord-point
  base-coord-format2-base-coord-point-raw))

(defun base-coord-format2-base-coord-format (table)
  "Format identifier — format = 2"
  (declare (type base-coord-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun base-coord-format2-base-coord-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun base-coord-format2-coordinate (table)
  "X or Y value, in design units"
  (declare (type base-coord-format2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun base-coord-format2-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun base-coord-format2-reference-glyph (table)
  "Glyph ID of control glyph"
  (declare (type base-coord-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun base-coord-format2-reference-glyph-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun base-coord-format2-base-coord-point (table)
  "Index of contour point on the reference glyph"
  (declare (type base-coord-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun base-coord-format2-base-coord-point-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun sanitize-base-coord-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-base-coord-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-coord-format2 font-data offset length)
  (%make-base-coord-format2 font-data offset length))

(defstruct
    (base-coord-format3 (:include table-ref)
     (:constructor %make-base-coord-format3
      (%table-font-data %table-offset %table-length)))
  "[BaseCoordFormat3](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basecoord-format-3)")

(declaim
 (inline base-coord-format3-base-coord-format
  base-coord-format3-base-coord-format-raw base-coord-format3-coordinate
  base-coord-format3-coordinate-raw base-coord-format3-device-offset-offset
  base-coord-format3-device-offset))

(defun base-coord-format3-base-coord-format (table)
  "Format identifier — format = 3"
  (declare (type base-coord-format3 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun base-coord-format3-base-coord-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun base-coord-format3-coordinate (table)
  "X or Y value, in design units"
  (declare (type base-coord-format3 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun base-coord-format3-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun base-coord-format3-device-offset-offset (table)
  "Offset to Device table (non-variable font) / Variation Index table (variable font) for X or Y value, from beginning of BaseCoord table (may be NULL)."
  (declare (type base-coord-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun base-coord-format3-device-offset (table)
  "Offset to Device table (non-variable font) / Variation Index table (variable font) for X or Y value, from beginning of BaseCoord table (may be NULL)."
  (declare (type base-coord-format3 table))
  (let ((off (base-coord-format3-device-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-device-or-variation-index (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-base-coord-format3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  t)

(defun read-base-coord-format3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-coord-format3 font-data offset length)
  (%make-base-coord-format3 font-data offset length))
