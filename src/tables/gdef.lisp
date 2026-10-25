;;;; gdef.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+gdef-tag+ +glyph-class-def-base+ +glyph-class-def-component+
   +glyph-class-def-ligature+ +glyph-class-def-mark+ attach-list
   attach-list-attach-point-offsets-count attach-list-attach-point-offsets-ref
   attach-list-coverage-offset attach-list-coverage-offset-offset
   attach-list-glyph-count attach-list-p attach-point attach-point-p
   attach-point-point-count attach-point-point-indices-count
   attach-point-point-indices-ref caret-value-format1
   caret-value-format1-caret-value-format caret-value-format1-coordinate
   caret-value-format1-p caret-value-format2
   caret-value-format2-caret-value-format
   caret-value-format2-caret-value-point-index caret-value-format2-p
   caret-value-format3 caret-value-format3-caret-value-format
   caret-value-format3-coordinate caret-value-format3-device-offset
   caret-value-format3-device-offset-offset caret-value-format3-p gdef
   gdef-attach-list-offset gdef-attach-list-offset-offset
   gdef-attach-list2-offset gdef-attach-list2-offset-offset
   gdef-glyph-class-def-offset gdef-glyph-class-def-offset-offset
   gdef-glyph-class-def2-offset gdef-glyph-class-def2-offset-offset
   gdef-item-var-store-offset gdef-item-var-store-offset-offset
   gdef-lig-caret-list-offset gdef-lig-caret-list-offset-offset
   gdef-lig-caret-list2-offset gdef-lig-caret-list2-offset-offset
   gdef-mark-attach-class-def-offset gdef-mark-attach-class-def-offset-offset
   gdef-mark-attach-class-def2-offset gdef-mark-attach-class-def2-offset-offset
   gdef-mark-glyph-sets-def-offset gdef-mark-glyph-sets-def-offset-offset
   gdef-mark-glyph-sets-def2-offset gdef-mark-glyph-sets-def2-offset-offset
   gdef-p gdef-version lig-caret-list lig-caret-list-coverage-offset
   lig-caret-list-coverage-offset-offset lig-caret-list-lig-glyph-count
   lig-caret-list-lig-glyph-offsets-count lig-caret-list-lig-glyph-offsets-ref
   lig-caret-list-p lig-caret-list2 lig-caret-list2-coverage-offset
   lig-caret-list2-coverage-offset-offset lig-caret-list2-lig-glyph-count
   lig-caret-list2-lig-glyph-offsets-count
   lig-caret-list2-lig-glyph-offsets-ref lig-caret-list2-p lig-glyph
   lig-glyph-caret-count lig-glyph-caret-value-offsets-count
   lig-glyph-caret-value-offsets-ref lig-glyph-p mark-glyph-sets
   mark-glyph-sets-coverage-offsets-count mark-glyph-sets-coverage-offsets-ref
   mark-glyph-sets-format-table mark-glyph-sets-mark-glyph-set-count
   mark-glyph-sets-p read-attach-list read-attach-point read-caret-value
   read-caret-value-format1 read-caret-value-format2 read-caret-value-format3
   read-gdef read-lig-caret-list read-lig-caret-list2 read-lig-glyph
   read-mark-glyph-sets sanitize-attach-list sanitize-attach-point
   sanitize-caret-value-format1 sanitize-caret-value-format2
   sanitize-caret-value-format3 sanitize-gdef sanitize-lig-caret-list
   sanitize-lig-caret-list2 sanitize-lig-glyph sanitize-mark-glyph-sets))

(define-constant +glyph-class-def-base+ 1
                 "Used in the [Glyph Class Definition Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#glyph-class-definition-table)")

(define-constant +glyph-class-def-ligature+ 2
                 "Used in the [Glyph Class Definition Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#glyph-class-definition-table)")

(define-constant +glyph-class-def-mark+ 3
                 "Used in the [Glyph Class Definition Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#glyph-class-definition-table)")

(define-constant +glyph-class-def-component+ 4
                 "Used in the [Glyph Class Definition Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#glyph-class-definition-table)")

(defstruct
    (gdef (:include table-ref)
     (:constructor %make-gdef (%table-font-data %table-offset %table-length)))
  "[GDEF](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#gdef-header) 1.0")

(declaim
 (inline gdef-version gdef-version-raw gdef-glyph-class-def-offset-offset
  gdef-glyph-class-def-offset gdef-attach-list-offset-offset
  gdef-attach-list-offset gdef-lig-caret-list-offset-offset
  gdef-lig-caret-list-offset gdef-mark-attach-class-def-offset-offset
  gdef-mark-attach-class-def-offset gdef-mark-glyph-sets-def-offset-offset
  gdef-mark-glyph-sets-def-offset gdef-item-var-store-offset-offset
  gdef-item-var-store-offset gdef-glyph-class-def2-offset-offset
  gdef-glyph-class-def2-offset gdef-attach-list2-offset-offset
  gdef-attach-list2-offset gdef-lig-caret-list2-offset-offset
  gdef-lig-caret-list2-offset gdef-mark-attach-class-def2-offset-offset
  gdef-mark-attach-class-def2-offset gdef-mark-glyph-sets-def2-offset-offset
  gdef-mark-glyph-sets-def2-offset))

(define-constant +gdef-tag+ "GDEF")

(defun gdef-version (table)
  "The major/minor version of the GDEF table"
  (declare (type gdef table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun gdef-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun gdef-glyph-class-def-offset-offset (table)
  "Offset to class definition table for glyph type, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun gdef-glyph-class-def-offset (table)
  "Offset to class definition table for glyph type, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (let ((off (gdef-glyph-class-def-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-class-def (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-attach-list-offset-offset (table)
  "Offset to attachment point list table, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun gdef-attach-list-offset (table)
  "Offset to attachment point list table, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (let ((off (gdef-attach-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-attach-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-lig-caret-list-offset-offset (table)
  "Offset to ligature caret list table, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun gdef-lig-caret-list-offset (table)
  "Offset to ligature caret list table, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (let ((off (gdef-lig-caret-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-lig-caret-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-mark-attach-class-def-offset-offset (table)
  "Offset to class definition table for mark attachment type, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun gdef-mark-attach-class-def-offset (table)
  "Offset to class definition table for mark attachment type, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (let ((off (gdef-mark-attach-class-def-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-class-def (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-mark-glyph-sets-def-offset-offset (table)
  "Offset to the table of mark glyph set definitions, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (when
      (and (>= (gdef-version table) 65538)
           (<= (+ (+ (table-ref-offset table) 12) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 12))))

(defun gdef-mark-glyph-sets-def-offset (table)
  "Offset to the table of mark glyph set definitions, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (let ((off (gdef-mark-glyph-sets-def-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark-glyph-sets (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-item-var-store-offset-offset (table)
  "Offset to the Item Variation Store table, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (when
      (and (>= (gdef-version table) 65539)
           (<= (+ (+ (table-ref-offset table) 14) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 14))))

(defun gdef-item-var-store-offset (table)
  "Offset to the Item Variation Store table, from beginning of GDEF header (may be NULL)"
  (declare (type gdef table))
  (let ((off (gdef-item-var-store-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-item-variation-store (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-glyph-class-def2-offset-offset (table)
  "32-bit offset taking precedence over glyph_class_def_offset when nonzero."
  (declare (type gdef table))
  (when
      (and (>= (gdef-version table) 65540)
           (<= (+ (+ (table-ref-offset table) 18) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 18))))

(defun gdef-glyph-class-def2-offset (table)
  "32-bit offset taking precedence over glyph_class_def_offset when nonzero."
  (declare (type gdef table))
  (let ((off (gdef-glyph-class-def2-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-class-def (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-attach-list2-offset-offset (table)
  "32-bit offset taking precedence over attach_list_offset when nonzero."
  (declare (type gdef table))
  (when
      (and (>= (gdef-version table) 65540)
           (<= (+ (+ (table-ref-offset table) 22) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 22))))

(defun gdef-attach-list2-offset (table)
  "32-bit offset taking precedence over attach_list_offset when nonzero."
  (declare (type gdef table))
  (let ((off (gdef-attach-list2-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-attach-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-lig-caret-list2-offset-offset (table)
  "32-bit offset taking precedence over lig_caret_list_offset when nonzero."
  (declare (type gdef table))
  (when
      (and (>= (gdef-version table) 65540)
           (<= (+ (+ (table-ref-offset table) 26) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 26))))

(defun gdef-lig-caret-list2-offset (table)
  "32-bit offset taking precedence over lig_caret_list_offset when nonzero."
  (declare (type gdef table))
  (let ((off (gdef-lig-caret-list2-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-lig-caret-list2 (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-mark-attach-class-def2-offset-offset (table)
  "32-bit offset taking precedence over mark_attach_class_def_offset when nonzero."
  (declare (type gdef table))
  (when
      (and (>= (gdef-version table) 65540)
           (<= (+ (+ (table-ref-offset table) 30) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 30))))

(defun gdef-mark-attach-class-def2-offset (table)
  "32-bit offset taking precedence over mark_attach_class_def_offset when nonzero."
  (declare (type gdef table))
  (let ((off (gdef-mark-attach-class-def2-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-class-def (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun gdef-mark-glyph-sets-def2-offset-offset (table)
  "32-bit offset taking precedence over mark_glyph_sets_def_offset when nonzero."
  (declare (type gdef table))
  (when
      (and (>= (gdef-version table) 65540)
           (<= (+ (+ (table-ref-offset table) 34) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 34))))

(defun gdef-mark-glyph-sets-def2-offset (table)
  "32-bit offset taking precedence over mark_glyph_sets_def_offset when nonzero."
  (declare (type gdef table))
  (let ((off (gdef-mark-glyph-sets-def2-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mark-glyph-sets (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-gdef (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-gdef
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-gdef font-data offset length)
  (%make-gdef font-data offset length))

(defstruct
    (attach-list (:include table-ref)
     (:constructor %make-attach-list
      (%table-font-data %table-offset %table-length)))
  "[Attachment Point List Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#attachment-point-list-table)")

(declaim
 (inline attach-list-coverage-offset-offset attach-list-coverage-offset
  attach-list-glyph-count attach-list-glyph-count-raw
  attach-list-attach-point-offsets-count attach-list-attach-point-offsets-ref))

(defun attach-list-coverage-offset-offset (table)
  "Offset to Coverage table - from beginning of AttachList table"
  (declare (type attach-list table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun attach-list-coverage-offset (table)
  "Offset to Coverage table - from beginning of AttachList table"
  (declare (type attach-list table))
  (let ((off (attach-list-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun attach-list-glyph-count (table)
  "Number of glyphs with attachment points"
  (declare (type attach-list table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun attach-list-glyph-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun attach-list-attach-point-offsets-count (table)
  "Array of offsets to AttachPoint tables-from beginning of AttachList table-in Coverage Index order"
  (declare (type attach-list table))
  (declare (ignorable table))
  (attach-list-glyph-count table))

(defun attach-list-attach-point-offsets-ref (table index)
  "Array of offsets to AttachPoint tables-from beginning of AttachList table-in Coverage Index order"
  (declare (type attach-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u16 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-attach-point (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-attach-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (attach-list-glyph-count-raw font-data offset))
         (pos (+ 4 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-attach-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-attach-list font-data offset length)
  (%make-attach-list font-data offset length))

(defstruct
    (attach-point (:include table-ref)
     (:constructor %make-attach-point
      (%table-font-data %table-offset %table-length)))
  "Part of [AttachList]")

(declaim
 (inline attach-point-point-count attach-point-point-count-raw
  attach-point-point-indices-count attach-point-point-indices-ref))

(defun attach-point-point-count (table)
  "Number of attachment points on this glyph"
  (declare (type attach-point table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun attach-point-point-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun attach-point-point-indices-count (table)
  "Array of contour point indices -in increasing numerical order"
  (declare (type attach-point table))
  (declare (ignorable table))
  (attach-point-point-count table))

(defun attach-point-point-indices-ref (table index)
  "Array of contour point indices -in increasing numerical order"
  (declare (type attach-point table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun sanitize-attach-point (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  (let* ((cnt (attach-point-point-count-raw font-data offset))
         (pos (+ 2 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-attach-point
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-attach-point font-data offset length)
  (%make-attach-point font-data offset length))

(defstruct
    (lig-caret-list (:include table-ref)
     (:constructor %make-lig-caret-list
      (%table-font-data %table-offset %table-length)))
  "[Ligature Caret List Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#ligature-caret-list-table)")

(declaim
 (inline lig-caret-list-coverage-offset-offset lig-caret-list-coverage-offset
  lig-caret-list-lig-glyph-count lig-caret-list-lig-glyph-count-raw
  lig-caret-list-lig-glyph-offsets-count lig-caret-list-lig-glyph-offsets-ref))

(defun lig-caret-list-coverage-offset-offset (table)
  "Offset to Coverage table - from beginning of LigCaretList table"
  (declare (type lig-caret-list table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun lig-caret-list-coverage-offset (table)
  "Offset to Coverage table - from beginning of LigCaretList table"
  (declare (type lig-caret-list table))
  (let ((off (lig-caret-list-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun lig-caret-list-lig-glyph-count (table)
  "Number of ligature glyphs"
  (declare (type lig-caret-list table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun lig-caret-list-lig-glyph-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun lig-caret-list-lig-glyph-offsets-count (table)
  "Array of offsets to LigGlyph tables, from beginning of LigCaretList table —in Coverage Index order"
  (declare (type lig-caret-list table))
  (declare (ignorable table))
  (lig-caret-list-lig-glyph-count table))

(defun lig-caret-list-lig-glyph-offsets-ref (table index)
  "Array of offsets to LigGlyph tables, from beginning of LigCaretList table —in Coverage Index order"
  (declare (type lig-caret-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u16 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-lig-glyph (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-lig-caret-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (lig-caret-list-lig-glyph-count-raw font-data offset))
         (pos (+ 4 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-lig-caret-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-lig-caret-list font-data offset length)
  (%make-lig-caret-list font-data offset length))

(defstruct
    (lig-glyph (:include table-ref)
     (:constructor %make-lig-glyph
      (%table-font-data %table-offset %table-length)))
  "[Ligature Glyph Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#ligature-glyph-table)")

(declaim
 (inline lig-glyph-caret-count lig-glyph-caret-count-raw
  lig-glyph-caret-value-offsets-count lig-glyph-caret-value-offsets-ref))

(defun lig-glyph-caret-count (table)
  "Number of CaretValue tables for this ligature (components - 1)"
  (declare (type lig-glyph table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun lig-glyph-caret-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun lig-glyph-caret-value-offsets-count (table)
  "Array of offsets to CaretValue tables, from beginning of LigGlyph table — in increasing coordinate order"
  (declare (type lig-glyph table))
  (declare (ignorable table))
  (lig-glyph-caret-count table))

(defun lig-glyph-caret-value-offsets-ref (table index)
  "Array of offsets to CaretValue tables, from beginning of LigGlyph table — in increasing coordinate order"
  (declare (type lig-glyph table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u16 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-caret-value (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-lig-glyph (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  (let* ((cnt (lig-glyph-caret-count-raw font-data offset))
         (pos (+ 2 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-lig-glyph
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-lig-glyph font-data offset length)
  (%make-lig-glyph font-data offset length))

(defun read-caret-value
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "[Caret Value Tables](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#caret-value-tables)"
  (declare (ignorable length))
  (let ((fmt (font-data-read-u16 font-data (+ offset 0))))
    (case fmt
      (1 (read-caret-value-format1 font-data offset length))
      (2 (read-caret-value-format2 font-data offset length))
      (3 (read-caret-value-format3 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'caret-value)))))

(defstruct
    (caret-value-format1 (:include table-ref)
     (:constructor %make-caret-value-format1
      (%table-font-data %table-offset %table-length)))
  "[CaretValue Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#caretvalue-format-1)")

(declaim
 (inline caret-value-format1-caret-value-format
  caret-value-format1-caret-value-format-raw caret-value-format1-coordinate
  caret-value-format1-coordinate-raw))

(defun caret-value-format1-caret-value-format (table)
  "Format identifier: format = 1"
  (declare (type caret-value-format1 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun caret-value-format1-caret-value-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun caret-value-format1-coordinate (table)
  "X or Y value, in design units"
  (declare (type caret-value-format1 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun caret-value-format1-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun sanitize-caret-value-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-caret-value-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-caret-value-format1 font-data offset length)
  (%make-caret-value-format1 font-data offset length))

(defstruct
    (caret-value-format2 (:include table-ref)
     (:constructor %make-caret-value-format2
      (%table-font-data %table-offset %table-length)))
  "[CaretValue Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#caretvalue-format-2)")

(declaim
 (inline caret-value-format2-caret-value-format
  caret-value-format2-caret-value-format-raw
  caret-value-format2-caret-value-point-index
  caret-value-format2-caret-value-point-index-raw))

(defun caret-value-format2-caret-value-format (table)
  "Format identifier: format = 2"
  (declare (type caret-value-format2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun caret-value-format2-caret-value-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun caret-value-format2-caret-value-point-index (table)
  "Contour point index on glyph"
  (declare (type caret-value-format2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun caret-value-format2-caret-value-point-index-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun sanitize-caret-value-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-caret-value-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-caret-value-format2 font-data offset length)
  (%make-caret-value-format2 font-data offset length))

(defstruct
    (caret-value-format3 (:include table-ref)
     (:constructor %make-caret-value-format3
      (%table-font-data %table-offset %table-length)))
  "[CaretValue Format 3](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#caretvalue-format-3)")

(declaim
 (inline caret-value-format3-caret-value-format
  caret-value-format3-caret-value-format-raw caret-value-format3-coordinate
  caret-value-format3-coordinate-raw caret-value-format3-device-offset-offset
  caret-value-format3-device-offset))

(defun caret-value-format3-caret-value-format (table)
  "Format identifier-format = 3"
  (declare (type caret-value-format3 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun caret-value-format3-caret-value-format-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun caret-value-format3-coordinate (table)
  "X or Y value, in design units"
  (declare (type caret-value-format3 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun caret-value-format3-coordinate-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun caret-value-format3-device-offset-offset (table)
  "Offset to Device table (non-variable font) / Variation Index table (variable font) for X or Y value-from beginning of CaretValue table"
  (declare (type caret-value-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun caret-value-format3-device-offset (table)
  "Offset to Device table (non-variable font) / Variation Index table (variable font) for X or Y value-from beginning of CaretValue table"
  (declare (type caret-value-format3 table))
  (let ((off (caret-value-format3-device-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-device-or-variation-index (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-caret-value-format3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  t)

(defun read-caret-value-format3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-caret-value-format3 font-data offset length)
  (%make-caret-value-format3 font-data offset length))

(defstruct
    (mark-glyph-sets (:include table-ref)
     (:constructor %make-mark-glyph-sets
      (%table-font-data %table-offset %table-length)))
  "[Mark Glyph Sets Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#mark-glyph-sets-table)")

(declaim
 (inline mark-glyph-sets-format-table mark-glyph-sets-format-table-raw
  mark-glyph-sets-mark-glyph-set-count mark-glyph-sets-mark-glyph-set-count-raw
  mark-glyph-sets-coverage-offsets-count mark-glyph-sets-coverage-offsets-ref))

(defun mark-glyph-sets-format-table (table)
  "Format identifier == 1"
  (declare (type mark-glyph-sets table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun mark-glyph-sets-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun mark-glyph-sets-mark-glyph-set-count (table)
  "Number of mark glyph sets defined"
  (declare (type mark-glyph-sets table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun mark-glyph-sets-mark-glyph-set-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun mark-glyph-sets-coverage-offsets-count (table)
  "Array of offsets to mark glyph set coverage tables, from the start of the MarkGlyphSets table."
  (declare (type mark-glyph-sets table))
  (declare (ignorable table))
  (mark-glyph-sets-mark-glyph-set-count table))

(defun mark-glyph-sets-coverage-offsets-ref (table index)
  "Array of offsets to mark glyph set coverage tables, from the start of the MarkGlyphSets table."
  (declare (type mark-glyph-sets table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u32 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-coverage-table (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-mark-glyph-sets (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (mark-glyph-sets-mark-glyph-set-count-raw font-data offset))
         (pos (+ 4 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-mark-glyph-sets
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mark-glyph-sets font-data offset length)
  (%make-mark-glyph-sets font-data offset length))

(defstruct
    (lig-caret-list2 (:include table-ref)
     (:constructor %make-lig-caret-list2
      (%table-font-data %table-offset %table-length)))
  "LigCaretList2 (ISO/IEC 14496-22:2026).")

(declaim
 (inline lig-caret-list2-coverage-offset-offset lig-caret-list2-coverage-offset
  lig-caret-list2-lig-glyph-count lig-caret-list2-lig-glyph-count-raw
  lig-caret-list2-lig-glyph-offsets-count
  lig-caret-list2-lig-glyph-offsets-ref))

(defun lig-caret-list2-coverage-offset-offset (table)
  "Offset to Coverage table - from beginning of LigCaretList2 table"
  (declare (type lig-caret-list2 table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun lig-caret-list2-coverage-offset (table)
  "Offset to Coverage table - from beginning of LigCaretList2 table"
  (declare (type lig-caret-list2 table))
  (let ((off (lig-caret-list2-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun lig-caret-list2-lig-glyph-count (table)
  "Number of ligature glyphs"
  (declare (type lig-caret-list2 table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun lig-caret-list2-lig-glyph-count-raw (font-data offset)
  (font-data-read-u24 font-data (+ offset 4)))

(defun lig-caret-list2-lig-glyph-offsets-count (table)
  "Array of offsets to LigGlyph tables, from beginning of LigCaretList2 table —in Coverage Index order"
  (declare (type lig-caret-list2 table))
  (declare (ignorable table))
  (lig-caret-list2-lig-glyph-count table))

(defun lig-caret-list2-lig-glyph-offsets-ref (table index)
  "Array of offsets to LigGlyph tables, from beginning of LigCaretList2 table —in Coverage Index order"
  (declare (type lig-caret-list2 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 7) (* index 3))))
    (unless
        (<= (+ pos 3) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 3 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u24 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-lig-glyph (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-lig-caret-list2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 7 length) (font-data-valid-range-p font-data offset 7))
    (error 'font-out-of-bounds-error :pos offset :size 7 :length
           (font-data-length font-data)))
  (let* ((cnt (lig-caret-list2-lig-glyph-count-raw font-data offset))
         (pos (+ 7 (* cnt 3))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-lig-caret-list2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-lig-caret-list2 font-data offset length)
  (%make-lig-caret-list2 font-data offset length))
