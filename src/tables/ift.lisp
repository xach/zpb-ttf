;;;; ift.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+compatibility-id-size+ +design-space-segment-size+
   +entry-format-flags-child-indices+ +entry-format-flags-codepoints-bit-1+
   +entry-format-flags-codepoints-bit-2+ +entry-format-flags-entry-id-delta+
   +entry-format-flags-features-and-design-space+ +entry-format-flags-ignored+
   +entry-format-flags-patch-format+ +entry-format-flags-reserved+
   +glyph-keyed-flags-none+ +glyph-keyed-flags-wide-glyph-ids+
   +patch-map-field-presence-flags-cff-charstrings-offset+
   +patch-map-field-presence-flags-cff2-charstrings-offset+
   +table-patch-flags-drop-table+ +table-patch-flags-replace-table+
   compatibility-id compatibility-id-id0 compatibility-id-id1
   compatibility-id-id2 compatibility-id-id3 compatibility-id-p
   design-space-segment design-space-segment-axis-tag design-space-segment-end
   design-space-segment-p design-space-segment-start entry-data
   entry-data-child-indices-count entry-data-child-indices-ref
   entry-data-design-space-count entry-data-design-space-segments-count
   entry-data-design-space-segments-ref entry-data-feature-count
   entry-data-feature-tags-count entry-data-feature-tags-ref
   entry-data-format-flags entry-data-match-mode-and-count entry-data-p
   entry-data-trailing-data-bytes entry-data-trailing-data-count
   entry-data-trailing-data-ref entry-format-flags-child-indices-p
   entry-format-flags-codepoints-bit-1-p entry-format-flags-codepoints-bit-2-p
   entry-format-flags-entry-id-delta-p
   entry-format-flags-features-and-design-space-p entry-format-flags-ignored-p
   entry-format-flags-patch-format-p entry-format-flags-reserved-p
   glyph-keyed-flags-none-p glyph-keyed-flags-wide-glyph-ids-p
   glyph-keyed-patch glyph-keyed-patch--reserved
   glyph-keyed-patch-brotli-stream-bytes glyph-keyed-patch-brotli-stream-count
   glyph-keyed-patch-brotli-stream-ref glyph-keyed-patch-compatibility-id
   glyph-keyed-patch-flags glyph-keyed-patch-format-table
   glyph-keyed-patch-max-uncompressed-length glyph-keyed-patch-p glyph-patches
   glyph-patches-glyph-count glyph-patches-glyph-ids-start glyph-patches-p
   glyph-patches-table-count id-string-data id-string-data-id-data-bytes
   id-string-data-id-data-count id-string-data-id-data-ref id-string-data-p
   ift-glyph-data ift-glyph-data-data-bytes ift-glyph-data-data-count
   ift-glyph-data-data-ref ift-glyph-data-p ift-patch-map
   ift-patch-map--reserved-0 ift-patch-map--reserved-1
   ift-patch-map--reserved-2 ift-patch-map-cff-charstrings-offset
   ift-patch-map-cff2-charstrings-offset ift-patch-map-compatibility-id
   ift-patch-map-default-patch-format ift-patch-map-entries-offset
   ift-patch-map-entries-offset-offset ift-patch-map-entry-count
   ift-patch-map-entry-id-string-data-offset
   ift-patch-map-entry-id-string-data-offset-offset ift-patch-map-field-flags
   ift-patch-map-format-table ift-patch-map-p ift-patch-map-url-template-bytes
   ift-patch-map-url-template-count ift-patch-map-url-template-length
   ift-patch-map-url-template-ref mapping-entries
   mapping-entries-entry-data-bytes mapping-entries-entry-data-count
   mapping-entries-entry-data-ref mapping-entries-p
   patch-map-field-presence-flags-cff-charstrings-offset-p
   patch-map-field-presence-flags-cff2-charstrings-offset-p read-entry-data
   read-glyph-keyed-patch read-glyph-patches read-id-string-data
   read-ift-glyph-data read-ift-patch-map read-mapping-entries
   read-table-keyed-patch read-table-patch sanitize-entry-data
   sanitize-glyph-keyed-patch sanitize-glyph-patches sanitize-id-string-data
   sanitize-ift-glyph-data sanitize-ift-patch-map sanitize-mapping-entries
   sanitize-table-keyed-patch sanitize-table-patch table-keyed-patch
   table-keyed-patch--reserved table-keyed-patch-compatibility-id
   table-keyed-patch-format-table table-keyed-patch-p
   table-keyed-patch-patch-offsets-count table-keyed-patch-patch-offsets-ref
   table-keyed-patch-patches-count table-patch table-patch-brotli-stream-bytes
   table-patch-brotli-stream-count table-patch-brotli-stream-ref
   table-patch-flags table-patch-flags-drop-table-p
   table-patch-flags-replace-table-p table-patch-max-uncompressed-length
   table-patch-p table-patch-tag))

(define-constant +patch-map-field-presence-flags-cff-charstrings-offset+ 1)

(declaim (inline patch-map-field-presence-flags-cff-charstrings-offset-p))

(defun patch-map-field-presence-flags-cff-charstrings-offset-p (flags)
  (logtest flags +patch-map-field-presence-flags-cff-charstrings-offset+))

(define-constant +patch-map-field-presence-flags-cff2-charstrings-offset+ 2)

(declaim (inline patch-map-field-presence-flags-cff2-charstrings-offset-p))

(defun patch-map-field-presence-flags-cff2-charstrings-offset-p (flags)
  (logtest flags +patch-map-field-presence-flags-cff2-charstrings-offset+))

(define-constant +entry-format-flags-features-and-design-space+ 1)

(declaim (inline entry-format-flags-features-and-design-space-p))

(defun entry-format-flags-features-and-design-space-p (flags)
  (logtest flags +entry-format-flags-features-and-design-space+))

(define-constant +entry-format-flags-child-indices+ 2)

(declaim (inline entry-format-flags-child-indices-p))

(defun entry-format-flags-child-indices-p (flags)
  (logtest flags +entry-format-flags-child-indices+))

(define-constant +entry-format-flags-entry-id-delta+ 4)

(declaim (inline entry-format-flags-entry-id-delta-p))

(defun entry-format-flags-entry-id-delta-p (flags)
  (logtest flags +entry-format-flags-entry-id-delta+))

(define-constant +entry-format-flags-patch-format+ 8)

(declaim (inline entry-format-flags-patch-format-p))

(defun entry-format-flags-patch-format-p (flags)
  (logtest flags +entry-format-flags-patch-format+))

(define-constant +entry-format-flags-codepoints-bit-1+ 16)

(declaim (inline entry-format-flags-codepoints-bit-1-p))

(defun entry-format-flags-codepoints-bit-1-p (flags)
  (logtest flags +entry-format-flags-codepoints-bit-1+))

(define-constant +entry-format-flags-codepoints-bit-2+ 32)

(declaim (inline entry-format-flags-codepoints-bit-2-p))

(defun entry-format-flags-codepoints-bit-2-p (flags)
  (logtest flags +entry-format-flags-codepoints-bit-2+))

(define-constant +entry-format-flags-ignored+ 64)

(declaim (inline entry-format-flags-ignored-p))

(defun entry-format-flags-ignored-p (flags)
  (logtest flags +entry-format-flags-ignored+))

(define-constant +entry-format-flags-reserved+ 128)

(declaim (inline entry-format-flags-reserved-p))

(defun entry-format-flags-reserved-p (flags)
  (logtest flags +entry-format-flags-reserved+))

(define-constant +table-patch-flags-replace-table+ 1)

(declaim (inline table-patch-flags-replace-table-p))

(defun table-patch-flags-replace-table-p (flags)
  (logtest flags +table-patch-flags-replace-table+))

(define-constant +table-patch-flags-drop-table+ 2)

(declaim (inline table-patch-flags-drop-table-p))

(defun table-patch-flags-drop-table-p (flags)
  (logtest flags +table-patch-flags-drop-table+))

(define-constant +glyph-keyed-flags-none+ 0)

(declaim (inline glyph-keyed-flags-none-p))

(defun glyph-keyed-flags-none-p (flags)
  (logtest flags +glyph-keyed-flags-none+))

(define-constant +glyph-keyed-flags-wide-glyph-ids+ 1)

(declaim (inline glyph-keyed-flags-wide-glyph-ids-p))

(defun glyph-keyed-flags-wide-glyph-ids-p (flags)
  (logtest flags +glyph-keyed-flags-wide-glyph-ids+))

(defstruct
    (design-space-segment (:include record-ref)
     (:constructor %make-design-space-segment
      (%record-font-data %record-offset))))

(define-constant +design-space-segment-size+ 12)

(declaim
 (inline design-space-segment-axis-tag design-space-segment-start
  design-space-segment-end))

(defun design-space-segment-axis-tag (rec)
  (declare (type design-space-segment rec))
  (font-data-read-tag-string (record-ref-font-data rec)
                             (record-ref-offset rec)))

(defun design-space-segment-start (rec)
  (declare (type design-space-segment rec))
  (font-data-read-fixed (record-ref-font-data rec)
                        (+ (record-ref-offset rec) 4)))

(defun design-space-segment-end (rec)
  (declare (type design-space-segment rec))
  (font-data-read-fixed (record-ref-font-data rec)
                        (+ (record-ref-offset rec) 8)))

(defstruct
    (compatibility-id (:include record-ref)
     (:constructor %make-compatibility-id (%record-font-data %record-offset)))
  "[Compatibility ID](https://w3c.github.io/IFT/Overview.html): uint32[4].")

(define-constant +compatibility-id-size+ 16)

(declaim
 (inline compatibility-id-id0 compatibility-id-id1 compatibility-id-id2
  compatibility-id-id3))

(defun compatibility-id-id0 (rec)
  (declare (type compatibility-id rec))
  (font-data-read-u32 (record-ref-font-data rec) (record-ref-offset rec)))

(defun compatibility-id-id1 (rec)
  (declare (type compatibility-id rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defun compatibility-id-id2 (rec)
  (declare (type compatibility-id rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 8)))

(defun compatibility-id-id3 (rec)
  (declare (type compatibility-id rec))
  (font-data-read-u32 (record-ref-font-data rec)
                      (+ (record-ref-offset rec) 12)))

(defstruct
    (ift-patch-map (:include table-ref)
     (:constructor %make-ift-patch-map
      (%table-font-data %table-offset %table-length)))
  "[Patch Map Table](https://w3c.github.io/IFT/Overview.html#patch-map-table)")

(declaim
 (inline ift-patch-map-format-table ift-patch-map-format-table-raw
  ift-patch-map--reserved-0 ift-patch-map--reserved-0-raw
  ift-patch-map--reserved-1 ift-patch-map--reserved-1-raw
  ift-patch-map--reserved-2 ift-patch-map--reserved-2-raw
  ift-patch-map-field-flags ift-patch-map-field-flags-raw
  ift-patch-map-compatibility-id ift-patch-map-default-patch-format
  ift-patch-map-default-patch-format-raw ift-patch-map-entry-count
  ift-patch-map-entry-count-raw ift-patch-map-entries-offset-offset
  ift-patch-map-entries-offset ift-patch-map-entry-id-string-data-offset-offset
  ift-patch-map-entry-id-string-data-offset ift-patch-map-url-template-length
  ift-patch-map-url-template-length-raw ift-patch-map-url-template-count
  ift-patch-map-url-template-ref ift-patch-map-url-template-bytes
  ift-patch-map-cff-charstrings-offset ift-patch-map-cff2-charstrings-offset))

(defun ift-patch-map-format-table (table)
  "Format identifier: format = 2"
  (declare (type ift-patch-map table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun ift-patch-map-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun ift-patch-map--reserved-0 (table)
  (declare (type ift-patch-map table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 1)))

(defun ift-patch-map--reserved-0-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 1)))

(defun ift-patch-map--reserved-1 (table)
  (declare (type ift-patch-map table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 2)))

(defun ift-patch-map--reserved-1-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 2)))

(defun ift-patch-map--reserved-2 (table)
  (declare (type ift-patch-map table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 3)))

(defun ift-patch-map--reserved-2-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 3)))

(defun ift-patch-map-field-flags (table)
  (declare (type ift-patch-map table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 4)))

(defun ift-patch-map-field-flags-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 4)))

(defun ift-patch-map-compatibility-id (table)
  "Unique ID that identifies compatible patches."
  (declare (type ift-patch-map table))
  (%make-compatibility-id (table-ref-font-data table)
   (+ (table-ref-offset table) 5)))

(defun ift-patch-map-default-patch-format (table)
  "Patch format number for patches referenced by this mapping."
  (declare (type ift-patch-map table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 21)))

(defun ift-patch-map-default-patch-format-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 21)))

(defun ift-patch-map-entry-count (table)
  (declare (type ift-patch-map table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 22)))

(defun ift-patch-map-entry-count-raw (font-data offset)
  (font-data-read-u24 font-data (+ offset 22)))

(defun ift-patch-map-entries-offset-offset (table)
  (declare (type ift-patch-map table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 25)))

(defun ift-patch-map-entries-offset (table)
  (declare (type ift-patch-map table))
  (let ((off (ift-patch-map-entries-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-mapping-entries (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun ift-patch-map-entry-id-string-data-offset-offset (table)
  (declare (type ift-patch-map table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 29)))

(defun ift-patch-map-entry-id-string-data-offset (table)
  (declare (type ift-patch-map table))
  (let ((off (ift-patch-map-entry-id-string-data-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-id-string-data (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun ift-patch-map-url-template-length (table)
  (declare (type ift-patch-map table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 33)))

(defun ift-patch-map-url-template-length-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 33)))

(defun ift-patch-map-url-template-count (table)
  (declare (type ift-patch-map table))
  (declare (ignorable table))
  (ift-patch-map-url-template-length table))

(defun ift-patch-map-url-template-ref (table index)
  (declare (type ift-patch-map table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 35) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun ift-patch-map-url-template-bytes (table)
  (declare (type ift-patch-map table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 35)
                   (ift-patch-map-url-template-count table)))

(defun ift-patch-map-cff-charstrings-offset (table)
  (declare (type ift-patch-map table))
  (when
      (logtest +patch-map-field-presence-flags-cff-charstrings-offset+
               (ift-patch-map-field-flags table))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 35
                           (* (ift-patch-map-url-template-count table) 1)))))

(defun ift-patch-map-cff2-charstrings-offset (table)
  (declare (type ift-patch-map table))
  (when
      (logtest +patch-map-field-presence-flags-cff2-charstrings-offset+
               (ift-patch-map-field-flags table))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 35
                           (* (ift-patch-map-url-template-count table) 1)
                           (if (logtest
                                +patch-map-field-presence-flags-cff-charstrings-offset+
                                (ift-patch-map-field-flags table))
                               4
                               0)))))

(defun sanitize-ift-patch-map (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 35 length) (font-data-valid-range-p font-data offset 35))
    (error 'font-out-of-bounds-error :pos offset :size 35 :length
           (font-data-length font-data)))
  (let* ((cnt (ift-patch-map-url-template-length-raw font-data offset))
         (pos (+ 35 (* cnt 1))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-ift-patch-map
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ift-patch-map font-data offset length)
  (%make-ift-patch-map font-data offset length))

(defstruct
    (mapping-entries (:include table-ref)
     (:constructor %make-mapping-entries
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline mapping-entries-entry-data-count mapping-entries-entry-data-ref
  mapping-entries-entry-data-bytes))

(defun mapping-entries-entry-data-count (table)
  (declare (type mapping-entries table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (table-ref-offset table))
        1)))

(defun mapping-entries-entry-data-ref (table index)
  (declare (type mapping-entries table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun mapping-entries-entry-data-bytes (table)
  (declare (type mapping-entries table))
  (font-data-slice (table-ref-font-data table) (table-ref-offset table)
                   (mapping-entries-entry-data-count table)))

(defun sanitize-mapping-entries (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-mapping-entries
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-mapping-entries font-data offset length)
  (%make-mapping-entries font-data offset length))

(defstruct
    (entry-data (:include table-ref)
     (:constructor %make-entry-data
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline entry-data-format-flags entry-data-format-flags-raw
  entry-data-feature-count entry-data-feature-count-raw
  entry-data-feature-tags-count entry-data-feature-tags-ref
  entry-data-design-space-count entry-data-design-space-segments-count
  entry-data-design-space-segments-ref entry-data-match-mode-and-count
  entry-data-child-indices-count entry-data-child-indices-ref
  entry-data-trailing-data-count entry-data-trailing-data-ref
  entry-data-trailing-data-bytes))

(defun entry-data-format-flags (table)
  (declare (type entry-data table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun entry-data-format-flags-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun entry-data-feature-count (table)
  (declare (type entry-data table))
  (when
      (logtest +entry-format-flags-features-and-design-space+
               (entry-data-format-flags table))
    (font-data-read-u8 (table-ref-font-data table)
                       (+ (table-ref-offset table) 1))))

(defun entry-data-feature-count-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 1)))

(defun entry-data-feature-tags-count (table)
  (declare (type entry-data table))
  (declare (ignorable table))
  (if (logtest +entry-format-flags-features-and-design-space+
               (entry-data-format-flags table))
      (entry-data-feature-count table)
      0))

(defun entry-data-feature-tags-ref (table index)
  (declare (type entry-data table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 1
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 1
                 0))
          (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (font-data-read-tag-string (table-ref-font-data table) pos)))

(defun entry-data-design-space-count (table)
  (declare (type entry-data table))
  (when
      (logtest +entry-format-flags-features-and-design-space+
               (entry-data-format-flags table))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 1
                           (if (logtest
                                +entry-format-flags-features-and-design-space+
                                (entry-data-format-flags table))
                               1
                               0)
                           (if (logtest
                                +entry-format-flags-features-and-design-space+
                                (entry-data-format-flags table))
                               (* (entry-data-feature-tags-count table) 4)
                               0)))))

(defun entry-data-design-space-segments-count (table)
  (declare (type entry-data table))
  (declare (ignorable table))
  (if (logtest +entry-format-flags-features-and-design-space+
               (entry-data-format-flags table))
      (entry-data-design-space-count table)
      0))

(defun entry-data-design-space-segments-ref (table index)
  (declare (type entry-data table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 1
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 1
                 0)
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 (* (entry-data-feature-tags-count table) 4)
                 0)
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 2
                 0))
          (* index 12))))
    (unless
        (<= (+ pos 12) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 12 :length
             (table-ref-length table)))
    (%make-design-space-segment (table-ref-font-data table) pos)))

(defun entry-data-match-mode-and-count (table)
  (declare (type entry-data table))
  (when
      (logtest +entry-format-flags-child-indices+
               (entry-data-format-flags table))
    (font-data-read-u8 (table-ref-font-data table)
                       (+ (table-ref-offset table) 1
                          (if (logtest
                               +entry-format-flags-features-and-design-space+
                               (entry-data-format-flags table))
                              1
                              0)
                          (if (logtest
                               +entry-format-flags-features-and-design-space+
                               (entry-data-format-flags table))
                              (* (entry-data-feature-tags-count table) 4)
                              0)
                          (if (logtest
                               +entry-format-flags-features-and-design-space+
                               (entry-data-format-flags table))
                              2
                              0)
                          (if (logtest
                               +entry-format-flags-features-and-design-space+
                               (entry-data-format-flags table))
                              (* (entry-data-design-space-segments-count table)
                                 12)
                              0)))))

(defun entry-data-child-indices-count (table)
  (declare (type entry-data table))
  (declare (ignorable table))
  (if (logtest +entry-format-flags-child-indices+
               (entry-data-format-flags table))
      (entry-data-match-mode-and-count table)
      0))

(defun entry-data-child-indices-ref (table index)
  (declare (type entry-data table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 1
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 1
                 0)
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 (* (entry-data-feature-tags-count table) 4)
                 0)
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 2
                 0)
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 (* (entry-data-design-space-segments-count table) 12)
                 0)
             (if (logtest +entry-format-flags-child-indices+
                          (entry-data-format-flags table))
                 1
                 0))
          (* index 3))))
    (unless
        (<= (+ pos 3) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 3 :length
             (table-ref-length table)))
    (font-data-read-u24 (table-ref-font-data table) pos)))

(defun entry-data-trailing-data-count (table)
  (declare (type entry-data table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 1
              (if (logtest +entry-format-flags-features-and-design-space+
                           (entry-data-format-flags table))
                  1
                  0)
              (if (logtest +entry-format-flags-features-and-design-space+
                           (entry-data-format-flags table))
                  (* (entry-data-feature-tags-count table) 4)
                  0)
              (if (logtest +entry-format-flags-features-and-design-space+
                           (entry-data-format-flags table))
                  2
                  0)
              (if (logtest +entry-format-flags-features-and-design-space+
                           (entry-data-format-flags table))
                  (* (entry-data-design-space-segments-count table) 12)
                  0)
              (if (logtest +entry-format-flags-child-indices+
                           (entry-data-format-flags table))
                  1
                  0)
              (if (logtest +entry-format-flags-child-indices+
                           (entry-data-format-flags table))
                  (* (entry-data-child-indices-count table) 3)
                  0)))
        1)))

(defun entry-data-trailing-data-ref (table index)
  (declare (type entry-data table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 1
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 1
                 0)
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 (* (entry-data-feature-tags-count table) 4)
                 0)
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 2
                 0)
             (if (logtest +entry-format-flags-features-and-design-space+
                          (entry-data-format-flags table))
                 (* (entry-data-design-space-segments-count table) 12)
                 0)
             (if (logtest +entry-format-flags-child-indices+
                          (entry-data-format-flags table))
                 1
                 0)
             (if (logtest +entry-format-flags-child-indices+
                          (entry-data-format-flags table))
                 (* (entry-data-child-indices-count table) 3)
                 0))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun entry-data-trailing-data-bytes (table)
  (declare (type entry-data table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 1
                      (if (logtest
                           +entry-format-flags-features-and-design-space+
                           (entry-data-format-flags table))
                          1
                          0)
                      (if (logtest
                           +entry-format-flags-features-and-design-space+
                           (entry-data-format-flags table))
                          (* (entry-data-feature-tags-count table) 4)
                          0)
                      (if (logtest
                           +entry-format-flags-features-and-design-space+
                           (entry-data-format-flags table))
                          2
                          0)
                      (if (logtest
                           +entry-format-flags-features-and-design-space+
                           (entry-data-format-flags table))
                          (* (entry-data-design-space-segments-count table) 12)
                          0)
                      (if (logtest +entry-format-flags-child-indices+
                                   (entry-data-format-flags table))
                          1
                          0)
                      (if (logtest +entry-format-flags-child-indices+
                                   (entry-data-format-flags table))
                          (* (entry-data-child-indices-count table) 3)
                          0))
                   (entry-data-trailing-data-count table)))

(defun sanitize-entry-data (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 1 length) (font-data-valid-range-p font-data offset 1))
    (error 'font-out-of-bounds-error :pos offset :size 1 :length
           (font-data-length font-data)))
  (when
      (and
       (logtest +entry-format-flags-features-and-design-space+
                (entry-data-format-flags-raw font-data offset))
       (> 2 length))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length length))
  t)

(defun read-entry-data
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-entry-data font-data offset length)
  (%make-entry-data font-data offset length))

(defstruct
    (id-string-data (:include table-ref)
     (:constructor %make-id-string-data
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline id-string-data-id-data-count id-string-data-id-data-ref
  id-string-data-id-data-bytes))

(defun id-string-data-id-data-count (table)
  (declare (type id-string-data table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (table-ref-offset table))
        1)))

(defun id-string-data-id-data-ref (table index)
  (declare (type id-string-data table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun id-string-data-id-data-bytes (table)
  (declare (type id-string-data table))
  (font-data-slice (table-ref-font-data table) (table-ref-offset table)
                   (id-string-data-id-data-count table)))

(defun sanitize-id-string-data (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-id-string-data
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-id-string-data font-data offset length)
  (%make-id-string-data font-data offset length))

(defstruct
    (table-keyed-patch (:include table-ref)
     (:constructor %make-table-keyed-patch
      (%table-font-data %table-offset %table-length)))
  "[Table Keyed Patch](https://w3c.github.io/IFT/Overview.html#table-keyed)")

(declaim
 (inline table-keyed-patch-format-table table-keyed-patch-format-table-raw
  table-keyed-patch--reserved table-keyed-patch--reserved-raw
  table-keyed-patch-compatibility-id table-keyed-patch-patches-count
  table-keyed-patch-patches-count-raw table-keyed-patch-patch-offsets-count
  table-keyed-patch-patch-offsets-ref))

(defun table-keyed-patch-format-table (table)
  (declare (type table-keyed-patch table))
  (font-data-read-tag-string (table-ref-font-data table)
                             (table-ref-offset table)))

(defun table-keyed-patch-format-table-raw (font-data offset)
  (font-data-read-tag-string font-data offset))

(defun table-keyed-patch--reserved (table)
  (declare (type table-keyed-patch table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun table-keyed-patch--reserved-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun table-keyed-patch-compatibility-id (table)
  "Unique ID that identifies compatible patches."
  (declare (type table-keyed-patch table))
  (%make-compatibility-id (table-ref-font-data table)
   (+ (table-ref-offset table) 8)))

(defun table-keyed-patch-patches-count (table)
  (declare (type table-keyed-patch table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 24)))

(defun table-keyed-patch-patches-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 24)))

(defun table-keyed-patch-patch-offsets-count (table)
  (declare (type table-keyed-patch table))
  (declare (ignorable table))
  (+ (table-keyed-patch-patches-count table) 1))

(defun table-keyed-patch-patch-offsets-ref (table index)
  (declare (type table-keyed-patch table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 26) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u32 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-table-patch (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-table-keyed-patch (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 26 length) (font-data-valid-range-p font-data offset 26))
    (error 'font-out-of-bounds-error :pos offset :size 26 :length
           (font-data-length font-data)))
  t)

(defun read-table-keyed-patch
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-table-keyed-patch font-data offset length)
  (%make-table-keyed-patch font-data offset length))

(defstruct
    (table-patch (:include table-ref)
     (:constructor %make-table-patch
      (%table-font-data %table-offset %table-length)))
  "[TablePatch](https://w3c.github.io/IFT/Overview.html#tablepatch)")

(declaim
 (inline table-patch-tag table-patch-tag-raw table-patch-flags
  table-patch-flags-raw table-patch-max-uncompressed-length
  table-patch-max-uncompressed-length-raw table-patch-brotli-stream-count
  table-patch-brotli-stream-ref table-patch-brotli-stream-bytes))

(defun table-patch-tag (table)
  (declare (type table-patch table))
  (font-data-read-tag-string (table-ref-font-data table)
                             (table-ref-offset table)))

(defun table-patch-tag-raw (font-data offset)
  (font-data-read-tag-string font-data offset))

(defun table-patch-flags (table)
  (declare (type table-patch table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 4)))

(defun table-patch-flags-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 4)))

(defun table-patch-max-uncompressed-length (table)
  (declare (type table-patch table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 5)))

(defun table-patch-max-uncompressed-length-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 5)))

(defun table-patch-brotli-stream-count (table)
  (declare (type table-patch table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 9))
        1)))

(defun table-patch-brotli-stream-ref (table index)
  (declare (type table-patch table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 9) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun table-patch-brotli-stream-bytes (table)
  (declare (type table-patch table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 9)
                   (table-patch-brotli-stream-count table)))

(defun sanitize-table-patch (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 9 length) (font-data-valid-range-p font-data offset 9))
    (error 'font-out-of-bounds-error :pos offset :size 9 :length
           (font-data-length font-data)))
  t)

(defun read-table-patch
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-table-patch font-data offset length)
  (%make-table-patch font-data offset length))

(defstruct
    (glyph-keyed-patch (:include table-ref)
     (:constructor %make-glyph-keyed-patch
      (%table-font-data %table-offset %table-length)))
  "[Glyph Keyed Patch](https://w3c.github.io/IFT/Overview.html#glyph-keyed)")

(declaim
 (inline glyph-keyed-patch-format-table glyph-keyed-patch-format-table-raw
  glyph-keyed-patch--reserved glyph-keyed-patch--reserved-raw
  glyph-keyed-patch-flags glyph-keyed-patch-flags-raw
  glyph-keyed-patch-compatibility-id glyph-keyed-patch-max-uncompressed-length
  glyph-keyed-patch-max-uncompressed-length-raw
  glyph-keyed-patch-brotli-stream-count glyph-keyed-patch-brotli-stream-ref
  glyph-keyed-patch-brotli-stream-bytes))

(defun glyph-keyed-patch-format-table (table)
  (declare (type glyph-keyed-patch table))
  (font-data-read-tag-string (table-ref-font-data table)
                             (table-ref-offset table)))

(defun glyph-keyed-patch-format-table-raw (font-data offset)
  (font-data-read-tag-string font-data offset))

(defun glyph-keyed-patch--reserved (table)
  (declare (type glyph-keyed-patch table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun glyph-keyed-patch--reserved-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 4)))

(defun glyph-keyed-patch-flags (table)
  (declare (type glyph-keyed-patch table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 8)))

(defun glyph-keyed-patch-flags-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 8)))

(defun glyph-keyed-patch-compatibility-id (table)
  (declare (type glyph-keyed-patch table))
  (%make-compatibility-id (table-ref-font-data table)
   (+ (table-ref-offset table) 9)))

(defun glyph-keyed-patch-max-uncompressed-length (table)
  (declare (type glyph-keyed-patch table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 25)))

(defun glyph-keyed-patch-max-uncompressed-length-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 25)))

(defun glyph-keyed-patch-brotli-stream-count (table)
  (declare (type glyph-keyed-patch table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 29))
        1)))

(defun glyph-keyed-patch-brotli-stream-ref (table index)
  (declare (type glyph-keyed-patch table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 29) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun glyph-keyed-patch-brotli-stream-bytes (table)
  (declare (type glyph-keyed-patch table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 29)
                   (glyph-keyed-patch-brotli-stream-count table)))

(defun sanitize-glyph-keyed-patch (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 29 length) (font-data-valid-range-p font-data offset 29))
    (error 'font-out-of-bounds-error :pos offset :size 29 :length
           (font-data-length font-data)))
  t)

(defun read-glyph-keyed-patch
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-glyph-keyed-patch font-data offset length)
  (%make-glyph-keyed-patch font-data offset length))

(defstruct
    (glyph-patches (:include table-ref)
     (:constructor %make-glyph-patches
      (%table-font-data %table-offset %table-length)))
  "[GlyphPatches](https://w3c.github.io/IFT/Overview.html#glyphpatches)")

(declaim
 (inline glyph-patches-glyph-count glyph-patches-glyph-count-raw
  glyph-patches-table-count glyph-patches-table-count-raw
  glyph-patches-glyph-ids-start))

(defun glyph-patches-glyph-count (table)
  (declare (type glyph-patches table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun glyph-patches-glyph-count-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun glyph-patches-table-count (table)
  (declare (type glyph-patches table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 4)))

(defun glyph-patches-table-count-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 4)))

(defun glyph-patches-glyph-ids-start (table)
  "Byte position of the glyph-ids field; its elements are variable-length U16-OR24 data."
  (declare (type glyph-patches table))
  (+ (table-ref-offset table) 5))

(defun sanitize-glyph-patches (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 5 length) (font-data-valid-range-p font-data offset 5))
    (error 'font-out-of-bounds-error :pos offset :size 5 :length
           (font-data-length font-data)))
  t)

(defun read-glyph-patches
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-glyph-patches font-data offset length)
  (%make-glyph-patches font-data offset length))

(defstruct
    (ift-glyph-data (:include table-ref)
     (:constructor %make-ift-glyph-data
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline ift-glyph-data-data-count ift-glyph-data-data-ref
  ift-glyph-data-data-bytes))

(defun ift-glyph-data-data-count (table)
  (declare (type ift-glyph-data table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (table-ref-offset table))
        1)))

(defun ift-glyph-data-data-ref (table index)
  (declare (type ift-glyph-data table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (table-ref-offset table) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun ift-glyph-data-data-bytes (table)
  (declare (type ift-glyph-data table))
  (font-data-slice (table-ref-font-data table) (table-ref-offset table)
                   (ift-glyph-data-data-count table)))

(defun sanitize-ift-glyph-data (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 0 length) (font-data-valid-range-p font-data offset 0))
    (error 'font-out-of-bounds-error :pos offset :size 0 :length
           (font-data-length font-data)))
  t)

(defun read-ift-glyph-data
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-ift-glyph-data font-data offset length)
  (%make-ift-glyph-data font-data offset length))
