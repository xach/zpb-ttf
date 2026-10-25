;;;; varc.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+varc-flags-axis-values-have-variation+ +varc-flags-gid-is-24-bit+
   +varc-flags-have-axes+ +varc-flags-have-condition+
   +varc-flags-have-rotation+ +varc-flags-have-scale-x+
   +varc-flags-have-scale-y+ +varc-flags-have-skew-x+ +varc-flags-have-skew-y+
   +varc-flags-have-tcenter-x+ +varc-flags-have-tcenter-y+
   +varc-flags-have-translate-x+ +varc-flags-have-translate-y+
   +varc-flags-reserved-mask+ +varc-flags-reset-unspecified-axes+
   +varc-flags-transform-has-variation+ +varc-tag+ condition-list
   condition-list-condition-count condition-list-condition-offsets-count
   condition-list-condition-offsets-ref condition-list-p
   multi-item-variation-data multi-item-variation-data-delta-sets-offset
   multi-item-variation-data-delta-sets-offset-offset
   multi-item-variation-data-format-table multi-item-variation-data-p
   multi-item-variation-data-region-index-count
   multi-item-variation-data-region-indices-count
   multi-item-variation-data-region-indices-ref multi-item-variation-store
   multi-item-variation-store-format-table multi-item-variation-store-p
   multi-item-variation-store-region-list-offset
   multi-item-variation-store-region-list-offset-offset
   multi-item-variation-store-variation-data-count
   multi-item-variation-store-variation-data-offsets-count
   multi-item-variation-store-variation-data-offsets-ref read-condition-list
   read-multi-item-variation-data read-multi-item-variation-store
   read-sparse-region-axis-coordinates read-sparse-variation-region
   read-sparse-variation-region-list read-varc sanitize-condition-list
   sanitize-multi-item-variation-data sanitize-multi-item-variation-store
   sanitize-sparse-region-axis-coordinates sanitize-sparse-variation-region
   sanitize-sparse-variation-region-list sanitize-varc
   sparse-region-axis-coordinates sparse-region-axis-coordinates-axis-index
   sparse-region-axis-coordinates-end sparse-region-axis-coordinates-p
   sparse-region-axis-coordinates-peak sparse-region-axis-coordinates-start
   sparse-variation-region
   sparse-variation-region-axis-coordinate-offsets-count
   sparse-variation-region-axis-coordinate-offsets-ref
   sparse-variation-region-list sparse-variation-region-list-p
   sparse-variation-region-list-region-count
   sparse-variation-region-list-region-offsets-count
   sparse-variation-region-list-region-offsets-ref sparse-variation-region-p
   sparse-variation-region-region-axis-count varc varc-axis-indices-list-offset
   varc-axis-indices-list-offset-offset varc-condition-list-offset
   varc-condition-list-offset-offset varc-coverage-offset
   varc-coverage-offset-offset varc-flags-axis-values-have-variation-p
   varc-flags-gid-is-24-bit-p varc-flags-have-axes-p
   varc-flags-have-condition-p varc-flags-have-rotation-p
   varc-flags-have-scale-x-p varc-flags-have-scale-y-p varc-flags-have-skew-x-p
   varc-flags-have-skew-y-p varc-flags-have-tcenter-x-p
   varc-flags-have-tcenter-y-p varc-flags-have-translate-x-p
   varc-flags-have-translate-y-p varc-flags-reserved-mask-p
   varc-flags-reset-unspecified-axes-p varc-flags-transform-has-variation-p
   varc-multi-var-store-offset varc-multi-var-store-offset-offset varc-p
   varc-var-composite-glyphs-offset varc-var-composite-glyphs-offset-offset
   varc-version))

(define-constant +varc-flags-reset-unspecified-axes+ 1
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-reset-unspecified-axes-p))

(defun varc-flags-reset-unspecified-axes-p (flags)
  (logtest flags +varc-flags-reset-unspecified-axes+))

(define-constant +varc-flags-have-axes+ 2
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-axes-p))

(defun varc-flags-have-axes-p (flags) (logtest flags +varc-flags-have-axes+))

(define-constant +varc-flags-axis-values-have-variation+ 4
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-axis-values-have-variation-p))

(defun varc-flags-axis-values-have-variation-p (flags)
  (logtest flags +varc-flags-axis-values-have-variation+))

(define-constant +varc-flags-transform-has-variation+ 8
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-transform-has-variation-p))

(defun varc-flags-transform-has-variation-p (flags)
  (logtest flags +varc-flags-transform-has-variation+))

(define-constant +varc-flags-have-translate-x+ 16
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-translate-x-p))

(defun varc-flags-have-translate-x-p (flags)
  (logtest flags +varc-flags-have-translate-x+))

(define-constant +varc-flags-have-translate-y+ 32
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-translate-y-p))

(defun varc-flags-have-translate-y-p (flags)
  (logtest flags +varc-flags-have-translate-y+))

(define-constant +varc-flags-have-rotation+ 64
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-rotation-p))

(defun varc-flags-have-rotation-p (flags)
  (logtest flags +varc-flags-have-rotation+))

(define-constant +varc-flags-have-condition+ 128
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-condition-p))

(defun varc-flags-have-condition-p (flags)
  (logtest flags +varc-flags-have-condition+))

(define-constant +varc-flags-have-scale-x+ 256
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-scale-x-p))

(defun varc-flags-have-scale-x-p (flags)
  (logtest flags +varc-flags-have-scale-x+))

(define-constant +varc-flags-have-scale-y+ 512
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-scale-y-p))

(defun varc-flags-have-scale-y-p (flags)
  (logtest flags +varc-flags-have-scale-y+))

(define-constant +varc-flags-have-tcenter-x+ 1024
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-tcenter-x-p))

(defun varc-flags-have-tcenter-x-p (flags)
  (logtest flags +varc-flags-have-tcenter-x+))

(define-constant +varc-flags-have-tcenter-y+ 2048
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-tcenter-y-p))

(defun varc-flags-have-tcenter-y-p (flags)
  (logtest flags +varc-flags-have-tcenter-y+))

(define-constant +varc-flags-gid-is-24-bit+ 4096
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-gid-is-24-bit-p))

(defun varc-flags-gid-is-24-bit-p (flags)
  (logtest flags +varc-flags-gid-is-24-bit+))

(define-constant +varc-flags-have-skew-x+ 8192
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-skew-x-p))

(defun varc-flags-have-skew-x-p (flags)
  (logtest flags +varc-flags-have-skew-x+))

(define-constant +varc-flags-have-skew-y+ 16384
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-have-skew-y-p))

(defun varc-flags-have-skew-y-p (flags)
  (logtest flags +varc-flags-have-skew-y+))

(define-constant +varc-flags-reserved-mask+ 4294934528
                 "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")

(declaim (inline varc-flags-reserved-mask-p))

(defun varc-flags-reserved-mask-p (flags)
  (logtest flags +varc-flags-reserved-mask+))

(defstruct
    (varc (:include table-ref)
     (:constructor %make-varc (%table-font-data %table-offset %table-length)))
  "[VARC](https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md) (Variable Composites / Components Table)  [FontTools VARC](https://github.com/fonttools/fonttools/blob/5e6b12d12fa08abafbeb7570f47707fbedf69a45/Lib/fontTools/ttLib/tables/otData.py#L3459-L3476)")

(declaim
 (inline varc-version varc-version-raw varc-coverage-offset-offset
  varc-coverage-offset varc-multi-var-store-offset-offset
  varc-multi-var-store-offset varc-condition-list-offset-offset
  varc-condition-list-offset varc-axis-indices-list-offset-offset
  varc-axis-indices-list-offset varc-var-composite-glyphs-offset-offset
  varc-var-composite-glyphs-offset))

(define-constant +varc-tag+ "VARC")

(defun varc-version (table)
  "Major/minor version number. Set to 1.0."
  (declare (type varc table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun varc-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun varc-coverage-offset-offset (table)
  (declare (type varc table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun varc-coverage-offset (table)
  (declare (type varc table))
  (let ((off (varc-coverage-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-coverage-table (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun varc-multi-var-store-offset-offset (table)
  (declare (type varc table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun varc-multi-var-store-offset (table)
  (declare (type varc table))
  (let ((off (varc-multi-var-store-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-multi-item-variation-store (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun varc-condition-list-offset-offset (table)
  (declare (type varc table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun varc-condition-list-offset (table)
  (declare (type varc table))
  (let ((off (varc-condition-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-condition-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun varc-axis-indices-list-offset-offset (table)
  (declare (type varc table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun varc-axis-indices-list-offset (table)
  (declare (type varc table))
  (let ((off (varc-axis-indices-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun varc-var-composite-glyphs-offset-offset (table)
  (declare (type varc table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 20)))

(defun varc-var-composite-glyphs-offset (table)
  (declare (type varc table))
  (let ((off (varc-var-composite-glyphs-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun sanitize-varc (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 24 length) (font-data-valid-range-p font-data offset 24))
    (error 'font-out-of-bounds-error :pos offset :size 24 :length
           (font-data-length font-data)))
  t)

(defun read-varc
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-varc font-data offset length)
  (%make-varc font-data offset length))

(defstruct
    (multi-item-variation-store (:include table-ref)
     (:constructor %make-multi-item-variation-store
      (%table-font-data %table-offset %table-length)))
  "* <https://github.com/fonttools/fonttools/blob/5e6b12d12fa08abafbeb7570f47707fbedf69a45/Lib/fontTools/ttLib/tables/otData.py#L3451-L3457> * <https://github.com/harfbuzz/harfbuzz/blob/7be12b33e3f07067c159d8f516eb31df58c75876/src/hb-ot-layout-common.hh#L3517-L3520C3>")

(declaim
 (inline multi-item-variation-store-format-table
  multi-item-variation-store-format-table-raw
  multi-item-variation-store-region-list-offset-offset
  multi-item-variation-store-region-list-offset
  multi-item-variation-store-variation-data-count
  multi-item-variation-store-variation-data-count-raw
  multi-item-variation-store-variation-data-offsets-count
  multi-item-variation-store-variation-data-offsets-ref))

(defun multi-item-variation-store-format-table (table)
  (declare (type multi-item-variation-store table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun multi-item-variation-store-format-table-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun multi-item-variation-store-region-list-offset-offset (table)
  (declare (type multi-item-variation-store table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun multi-item-variation-store-region-list-offset (table)
  (declare (type multi-item-variation-store table))
  (let ((off (multi-item-variation-store-region-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-sparse-variation-region-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun multi-item-variation-store-variation-data-count (table)
  (declare (type multi-item-variation-store table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun multi-item-variation-store-variation-data-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun multi-item-variation-store-variation-data-offsets-count (table)
  (declare (type multi-item-variation-store table))
  (declare (ignorable table))
  (multi-item-variation-store-variation-data-count table))

(defun multi-item-variation-store-variation-data-offsets-ref (table index)
  (declare (type multi-item-variation-store table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 8) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u32 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-multi-item-variation-data (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-multi-item-variation-store (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  (let* ((cnt
          (multi-item-variation-store-variation-data-count-raw font-data
           offset))
         (pos (+ 8 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-multi-item-variation-store
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-multi-item-variation-store font-data offset length)
  (%make-multi-item-variation-store font-data offset length))

(defstruct
    (sparse-variation-region-list (:include table-ref)
     (:constructor %make-sparse-variation-region-list
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline sparse-variation-region-list-region-count
  sparse-variation-region-list-region-count-raw
  sparse-variation-region-list-region-offsets-count
  sparse-variation-region-list-region-offsets-ref))

(defun sparse-variation-region-list-region-count (table)
  (declare (type sparse-variation-region-list table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun sparse-variation-region-list-region-count-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun sparse-variation-region-list-region-offsets-count (table)
  (declare (type sparse-variation-region-list table))
  (declare (ignorable table))
  (sparse-variation-region-list-region-count table))

(defun sparse-variation-region-list-region-offsets-ref (table index)
  (declare (type sparse-variation-region-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u32 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-sparse-variation-region (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-sparse-variation-region-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (sparse-variation-region-list-region-count-raw font-data offset))
         (pos (+ 4 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-sparse-variation-region-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-sparse-variation-region-list font-data offset length)
  (%make-sparse-variation-region-list font-data offset length))

(defstruct
    (sparse-variation-region (:include table-ref)
     (:constructor %make-sparse-variation-region
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline sparse-variation-region-region-axis-count
  sparse-variation-region-region-axis-count-raw
  sparse-variation-region-axis-coordinate-offsets-count
  sparse-variation-region-axis-coordinate-offsets-ref))

(defun sparse-variation-region-region-axis-count (table)
  (declare (type sparse-variation-region table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun sparse-variation-region-region-axis-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun sparse-variation-region-axis-coordinate-offsets-count (table)
  (declare (type sparse-variation-region table))
  (declare (ignorable table))
  (sparse-variation-region-region-axis-count table))

(defun sparse-variation-region-axis-coordinate-offsets-ref (table index)
  (declare (type sparse-variation-region table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 2) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u32 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-sparse-region-axis-coordinates (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-sparse-variation-region (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 2 length) (font-data-valid-range-p font-data offset 2))
    (error 'font-out-of-bounds-error :pos offset :size 2 :length
           (font-data-length font-data)))
  (let* ((cnt (sparse-variation-region-region-axis-count-raw font-data offset))
         (pos (+ 2 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-sparse-variation-region
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-sparse-variation-region font-data offset length)
  (%make-sparse-variation-region font-data offset length))

(defstruct
    (sparse-region-axis-coordinates (:include table-ref)
     (:constructor %make-sparse-region-axis-coordinates
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline sparse-region-axis-coordinates-axis-index
  sparse-region-axis-coordinates-axis-index-raw
  sparse-region-axis-coordinates-start sparse-region-axis-coordinates-start-raw
  sparse-region-axis-coordinates-peak sparse-region-axis-coordinates-peak-raw
  sparse-region-axis-coordinates-end sparse-region-axis-coordinates-end-raw))

(defun sparse-region-axis-coordinates-axis-index (table)
  (declare (type sparse-region-axis-coordinates table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun sparse-region-axis-coordinates-axis-index-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun sparse-region-axis-coordinates-start (table)
  (declare (type sparse-region-axis-coordinates table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 2)))

(defun sparse-region-axis-coordinates-start-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 2)))

(defun sparse-region-axis-coordinates-peak (table)
  (declare (type sparse-region-axis-coordinates table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun sparse-region-axis-coordinates-peak-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun sparse-region-axis-coordinates-end (table)
  (declare (type sparse-region-axis-coordinates table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 6)))

(defun sparse-region-axis-coordinates-end-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 6)))

(defun sanitize-sparse-region-axis-coordinates (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-sparse-region-axis-coordinates
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-sparse-region-axis-coordinates font-data offset length)
  (%make-sparse-region-axis-coordinates font-data offset length))

(defstruct
    (multi-item-variation-data (:include table-ref)
     (:constructor %make-multi-item-variation-data
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline multi-item-variation-data-format-table
  multi-item-variation-data-format-table-raw
  multi-item-variation-data-region-index-count
  multi-item-variation-data-region-index-count-raw
  multi-item-variation-data-region-indices-count
  multi-item-variation-data-region-indices-ref
  multi-item-variation-data-delta-sets-offset-offset
  multi-item-variation-data-delta-sets-offset))

(defun multi-item-variation-data-format-table (table)
  (declare (type multi-item-variation-data table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun multi-item-variation-data-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun multi-item-variation-data-region-index-count (table)
  (declare (type multi-item-variation-data table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun multi-item-variation-data-region-index-count-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 1)))

(defun multi-item-variation-data-region-indices-count (table)
  (declare (type multi-item-variation-data table))
  (declare (ignorable table))
  (multi-item-variation-data-region-index-count table))

(defun multi-item-variation-data-region-indices-ref (table index)
  (declare (type multi-item-variation-data table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 3) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun multi-item-variation-data-delta-sets-offset-offset (table)
  (declare (type multi-item-variation-data table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 3
                         (*
                          (multi-item-variation-data-region-indices-count
                           table)
                          2))))

(defun multi-item-variation-data-delta-sets-offset (table)
  (declare (type multi-item-variation-data table))
  (let ((off (multi-item-variation-data-delta-sets-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun sanitize-multi-item-variation-data (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  (let* ((cnt
          (multi-item-variation-data-region-index-count-raw font-data offset))
         (pos (+ 3 (* cnt 2))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-multi-item-variation-data
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-multi-item-variation-data font-data offset length)
  (%make-multi-item-variation-data font-data offset length))

(defstruct
    (condition-list (:include table-ref)
     (:constructor %make-condition-list
      (%table-font-data %table-offset %table-length))))

(declaim
 (inline condition-list-condition-count condition-list-condition-count-raw
  condition-list-condition-offsets-count condition-list-condition-offsets-ref))

(defun condition-list-condition-count (table)
  (declare (type condition-list table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun condition-list-condition-count-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun condition-list-condition-offsets-count (table)
  (declare (type condition-list table))
  (declare (ignorable table))
  (condition-list-condition-count table))

(defun condition-list-condition-offsets-ref (table index)
  (declare (type condition-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u32 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-condition (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-condition-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (condition-list-condition-count-raw font-data offset))
         (pos (+ 4 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-condition-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-condition-list font-data offset length)
  (%make-condition-list font-data offset length))
