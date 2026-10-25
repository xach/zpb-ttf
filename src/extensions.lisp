;;;; extensions.lisp — Hand-written accessors for data-dependent table layouts
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; The generated tables cannot express arrays whose element size or
;;; count depends on packed flag fields. These helpers decode them.

(in-package #:zpb-otf)

(export '(+tuple-index-embedded-peak-tuple+ +tuple-index-intermediate-region+
          +tuple-index-private-point-numbers+ +tuple-index-tuple-index-mask+
          +tuple-variation-count-shared-point-numbers+ +tuple-variation-count-count-mask+
          tuple-variation-header-size map-tuple-variation-headers
          cvar-tuple-count cvar-tuple-variation-headers
          gvar-glyph-variation-data-offset-ref gvar-glyph-variation-data-header
          glyph-variation-data-header-tuple-count
          glyph-variation-data-header-tuple-variation-headers
          strike-glyph-data
          item-variation-data-long-words-p item-variation-data-word-count
          item-variation-data-row-size item-variation-data-delta-set-ref
          gpos-value-record gpos-value-record-p
          gpos-value-record-x-placement gpos-value-record-y-placement
          gpos-value-record-x-advance gpos-value-record-y-advance
          gpos-value-record-x-placement-device gpos-value-record-y-placement-device
          gpos-value-record-x-advance-device gpos-value-record-y-advance-device
          value-format-size read-gpos-value-record
          single-pos-format1-value single-pos-format3-value
          single-pos-format2-value-ref single-pos-format4-value-ref
          pair-set-pair-value-record-ref pair-set2-pair-value-record-ref
          pair-pos-format2-class-values pair-pos-format4-class-values
          mvar-value-record-ref
          fvar-axis-instance-arrays-table fvar-axis-ref fvar-instance-ref))

;;; Tuple variation headers ('cvar', 'gvar')

(define-constant +tuple-index-embedded-peak-tuple+   #x8000)
(define-constant +tuple-index-intermediate-region+   #x4000)
(define-constant +tuple-index-private-point-numbers+ #x2000)
(define-constant +tuple-index-tuple-index-mask+      #x0FFF)
(define-constant +tuple-variation-count-shared-point-numbers+ #x8000)
(define-constant +tuple-variation-count-count-mask+           #x0FFF)

(defun tuple-variation-header-size (font-data pos axis-count)
  "Size in bytes of the TupleVariationHeader at POS."
  (let ((tuple-index (font-data-read-u16 font-data (+ pos 2))))
    (+ 4
       (if (logtest tuple-index +tuple-index-embedded-peak-tuple+)
           (* 2 axis-count)
           0)
       (if (logtest tuple-index +tuple-index-intermediate-region+)
           (* 4 axis-count)
           0))))

(defun map-tuple-variation-headers (fn font-data start count axis-count)
  "Call FN with each of the COUNT TupleVariationHeader tables starting at
POS START, in order. Return the position just past the last header."
  (let ((pos start))
    (dotimes (i count pos)
      (let ((size (tuple-variation-header-size font-data pos axis-count)))
        (funcall fn (read-tuple-variation-header font-data pos size))
        (incf pos size)))))

(defun cvar-tuple-count (cvar)
  (logand (cvar-tuple-variation-count cvar) +tuple-variation-count-count-mask+))

(defun cvar-tuple-variation-headers (cvar axis-count)
  "List of the cvar table's TupleVariationHeader tables. AXIS-COUNT comes from 'fvar'."
  (let ((headers '()))
    (map-tuple-variation-headers (lambda (h) (push h headers))
                                 (table-ref-font-data cvar)
                                 (cvar-tuple-variation-headers-start cvar)
                                 (cvar-tuple-count cvar)
                                 axis-count)
    (nreverse headers)))

;;; 'gvar'

(defun gvar-glyph-variation-data-offset-ref (gvar index)
  "Offset INDEX (0 to glyphCount inclusive) of glyphVariationDataOffsets,
decoded per the long-offsets flag: short offsets are stored halved."
  (let ((fd (table-ref-font-data gvar))
        (start (gvar-glyph-variation-data-offsets-start gvar)))
    (unless (<= index (gvar-glyph-count gvar))
      (error 'font-out-of-bounds-error :pos index :size 1 :length (1+ (gvar-glyph-count gvar))))
    (if (gvar-flags-long-offsets-p (gvar-flags gvar))
        (font-data-read-u32 fd (+ start (* index 4)))
        (* 2 (font-data-read-u16 fd (+ start (* index 2)))))))

(defun gvar-glyph-variation-data-header (gvar glyph-id)
  "The GlyphVariationData header for GLYPH-ID, or NIL if the glyph has no variation data."
  (let* ((start (gvar-glyph-variation-data-offset-ref gvar glyph-id))
         (end (gvar-glyph-variation-data-offset-ref gvar (1+ glyph-id)))
         (base (+ (table-ref-offset gvar) (gvar-glyph-variation-data-array-offset gvar))))
    (when (< start end)
      (read-glyph-variation-data-header (table-ref-font-data gvar)
                                        (+ base start)
                                        (- end start)))))

(defun glyph-variation-data-header-tuple-count (header)
  (logand (glyph-variation-data-header-tuple-variation-count header)
          +tuple-variation-count-count-mask+))

(defun glyph-variation-data-header-tuple-variation-headers (header axis-count)
  "List of HEADER's TupleVariationHeader tables. AXIS-COUNT comes from 'gvar' or 'fvar'."
  (let ((headers '()))
    (map-tuple-variation-headers (lambda (h) (push h headers))
                                 (table-ref-font-data header)
                                 (glyph-variation-data-header-tuple-variation-headers-start header)
                                 (glyph-variation-data-header-tuple-count header)
                                 axis-count)
    (nreverse headers)))

;;; 'sbix'

(defun strike-glyph-data (strike num-glyphs glyph-id)
  "The glyph-data table for GLYPH-ID in STRIKE, or NIL if the glyph has no bitmap.
NUM-GLYPHS comes from 'maxp'."
  (unless (< glyph-id num-glyphs)
    (error 'font-out-of-bounds-error :pos glyph-id :size 1 :length num-glyphs))
  (let ((start (strike-glyph-data-offsets-ref strike num-glyphs glyph-id))
        (end (strike-glyph-data-offsets-ref strike num-glyphs (1+ glyph-id))))
    (when (< start end)
      (read-glyph-data (table-ref-font-data strike)
                       (+ (table-ref-offset strike) start)
                       (- end start)))))

;;; ItemVariationData

(defun item-variation-data-long-words-p (ivd)
  "True if the word deltas are 32-bit and the remaining deltas 16-bit (the LONG_WORDS flag)."
  (logbitp 15 (item-variation-data-word-delta-count ivd)))

(defun item-variation-data-word-count (ivd)
  "Number of deltas per row stored in the wider (word) size."
  (logand #x7FFF (item-variation-data-word-delta-count ivd)))

(defun item-variation-data-row-size (ivd)
  "Size in bytes of one delta-set row."
  (let ((words (item-variation-data-word-count ivd))
        (regions (item-variation-data-region-index-count ivd)))
    (if (item-variation-data-long-words-p ivd)
        (+ (* 4 words) (* 2 (- regions words)))
        (+ (* 2 words) (- regions words)))))

(defun item-variation-data-delta-set-ref (ivd item region)
  "Delta for ITEM (row) and REGION (column index into regionIndexes)."
  (let* ((words (item-variation-data-word-count ivd))
         (long-p (item-variation-data-long-words-p ivd))
         (wide (if long-p 4 2))
         (narrow (if long-p 2 1))
         (fd (table-ref-font-data ivd)))
    (unless (and (< item (item-variation-data-item-count ivd))
                 (< region (item-variation-data-region-index-count ivd)))
      (error 'font-out-of-bounds-error :pos item :size 1 :length (item-variation-data-item-count ivd)))
    (let* ((rows-start (+ (table-ref-offset ivd) 6 (* 2 (item-variation-data-region-index-count ivd))))
           (pos (+ rows-start
                   (* item (item-variation-data-row-size ivd))
                   (if (< region words)
                       (* region wide)
                       (+ (* words wide) (* (- region words) narrow))))))
      (if (< region words)
          (if long-p
              (font-data-read-i32 fd pos)
              (font-data-read-i16 fd pos))
          (if long-p
              (font-data-read-i16 fd pos)
              (font-data-read-i8 fd pos))))))

;;; GPOS ValueRecord
;;;
;;; A ValueRecord holds one 16-bit field per bit set in the low byte of its
;;; ValueFormat. Device offsets are relative to the table that contains the
;;; record: the SinglePos or PairPos subtable, or the PairSet for PairPos format 1.

(defstruct (gpos-value-record (:constructor %make-gpos-value-record))
  (x-placement 0) (y-placement 0) (x-advance 0) (y-advance 0)
  (x-placement-device nil) (y-placement-device nil)
  (x-advance-device nil) (y-advance-device nil))

(defun value-format-size (value-format)
  "Size in bytes of a ValueRecord with VALUE-FORMAT."
  (* 2 (logcount (logand value-format #xFF))))

(defun read-gpos-value-record (font-data pos value-format parent-offset)
  "Decode the ValueRecord at POS. PARENT-OFFSET is the start of the table its
device offsets are relative to."
  (let ((rec (%make-gpos-value-record)))
    (flet ((next-i16 () (prog1 (font-data-read-i16 font-data pos) (incf pos 2)))
           (next-device ()
             (let ((off (font-data-read-u16 font-data pos)))
               (incf pos 2)
               (unless (zerop off)
                 (read-device-or-variation-index font-data (+ parent-offset off))))))
      (when (logbitp 0 value-format) (setf (gpos-value-record-x-placement rec) (next-i16)))
      (when (logbitp 1 value-format) (setf (gpos-value-record-y-placement rec) (next-i16)))
      (when (logbitp 2 value-format) (setf (gpos-value-record-x-advance rec)   (next-i16)))
      (when (logbitp 3 value-format) (setf (gpos-value-record-y-advance rec)   (next-i16)))
      (when (logbitp 4 value-format) (setf (gpos-value-record-x-placement-device rec) (next-device)))
      (when (logbitp 5 value-format) (setf (gpos-value-record-y-placement-device rec) (next-device)))
      (when (logbitp 6 value-format) (setf (gpos-value-record-x-advance-device rec)   (next-device)))
      (when (logbitp 7 value-format) (setf (gpos-value-record-y-advance-device rec)   (next-device))))
    rec))

(defun table-value-record (table pos value-format)
  "Bounds-checked ValueRecord at POS inside TABLE, which device offsets are relative to."
  (let ((size (value-format-size value-format)))
    (unless (<= (+ pos size) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size size :length (table-ref-length table)))
    (read-gpos-value-record (table-ref-font-data table) pos value-format (table-ref-offset table))))

(defun single-pos-format1-value (subtable)
  (table-value-record subtable (single-pos-format1-value-record-start subtable)
                      (single-pos-format1-value-format subtable)))

(defun single-pos-format3-value (subtable)
  (table-value-record subtable (single-pos-format3-value-record-start subtable)
                      (single-pos-format3-value-format subtable)))

(defun single-pos-format2-value-ref (subtable index)
  "ValueRecord INDEX, in Coverage order."
  (let ((fmt (single-pos-format2-value-format subtable)))
    (unless (< index (single-pos-format2-value-count subtable))
      (error 'font-out-of-bounds-error :pos index :size 1 :length (single-pos-format2-value-count subtable)))
    (table-value-record subtable
                        (+ (single-pos-format2-value-records-start subtable) (* index (value-format-size fmt)))
                        fmt)))

(defun single-pos-format4-value-ref (subtable index)
  "ValueRecord INDEX, in Coverage order."
  (let ((fmt (single-pos-format4-value-format subtable)))
    (unless (< index (single-pos-format4-value-count subtable))
      (error 'font-out-of-bounds-error :pos index :size 1 :length (single-pos-format4-value-count subtable)))
    (table-value-record subtable
                        (+ (single-pos-format4-value-records-start subtable) (* index (value-format-size fmt)))
                        fmt)))

(defun pair-value-record-at (pair-set index count start glyph-size glyph-reader value-format1 value-format2)
  (unless (< index count)
    (error 'font-out-of-bounds-error :pos index :size 1 :length count))
  (let* ((size1 (value-format-size value-format1))
         (pos (+ start (* index (+ glyph-size size1 (value-format-size value-format2))))))
    (unless (<= (+ pos glyph-size) (+ (table-ref-offset pair-set) (table-ref-length pair-set)))
      (error 'font-out-of-bounds-error :pos pos :size glyph-size :length (table-ref-length pair-set)))
    (values (funcall glyph-reader (table-ref-font-data pair-set) pos)
            (table-value-record pair-set (+ pos glyph-size) value-format1)
            (table-value-record pair-set (+ pos glyph-size size1) value-format2))))

(defun pair-set-pair-value-record-ref (pair-set index value-format1 value-format2)
  "PairValueRecord INDEX of PAIR-SET, as (values second-glyph value-record1 value-record2).
The value formats come from the parent PairPosFormat1 subtable."
  (pair-value-record-at pair-set index (pair-set-pair-value-count pair-set)
                        (pair-set-pair-value-records-start pair-set) 2 #'font-data-read-u16
                        value-format1 value-format2))

(defun pair-set2-pair-value-record-ref (pair-set index value-format1 value-format2)
  "PairValueRecord2 INDEX of PAIR-SET, as (values second-glyph value-record1 value-record2).
The value formats come from the parent PairPosFormat3 subtable."
  (pair-value-record-at pair-set index (pair-set2-pair-value-count pair-set)
                        (pair-set2-pair-value-records-start pair-set) 3 #'font-data-read-u24
                        value-format1 value-format2))

(defun class-values-at (subtable start class1 class2 class1-count class2-count value-format1 value-format2)
  (unless (and (< class1 class1-count) (< class2 class2-count))
    (error 'font-out-of-bounds-error :pos class1 :size 1 :length class1-count))
  (let* ((size1 (value-format-size value-format1))
         (record-size (+ size1 (value-format-size value-format2)))
         (pos (+ start (* (+ (* class1 class2-count) class2) record-size))))
    (values (table-value-record subtable pos value-format1)
            (table-value-record subtable (+ pos size1) value-format2))))

(defun pair-pos-format2-class-values (subtable class1 class2)
  "The Class2Record for CLASS1, CLASS2 as (values value-record1 value-record2)."
  (class-values-at subtable (pair-pos-format2-class1-records-start subtable) class1 class2
                   (pair-pos-format2-class1-count subtable) (pair-pos-format2-class2-count subtable)
                   (pair-pos-format2-value-format1 subtable) (pair-pos-format2-value-format2 subtable)))

(defun pair-pos-format4-class-values (subtable class1 class2)
  "The Class2Record for CLASS1, CLASS2 as (values value-record1 value-record2)."
  (class-values-at subtable (pair-pos-format4-class1-records-start subtable) class1 class2
                   (pair-pos-format4-class1-count subtable) (pair-pos-format4-class2-count subtable)
                   (pair-pos-format4-value-format1 subtable) (pair-pos-format4-value-format2 subtable)))

;;; 'MVAR'

(defun mvar-value-record-ref (mvar index)
  "Value record INDEX, stepping by valueRecordSize so that records larger
than the 8 bytes defined today are read correctly."
  (let ((size (mvar-value-record-size mvar)))
    (unless (>= size +mvar-value-record-size+)
      (error 'font-out-of-bounds-error :pos (table-ref-offset mvar)
                                       :size +mvar-value-record-size+
                                       :length size))
    (unless (< index (mvar-value-record-count mvar))
      (error 'font-out-of-bounds-error :pos index
                                       :size 1
                                       :length (mvar-value-record-count mvar)))
    (let ((pos (+ (table-ref-offset mvar) 12 (* index size))))
      (unless (<= (+ pos +mvar-value-record-size+)
                  (+ (table-ref-offset mvar) (table-ref-length mvar)))
        (error 'font-out-of-bounds-error :pos pos
                                         :size size
                                         :length (table-ref-length mvar)))
      (%make-mvar-value-record (table-ref-font-data mvar) pos))))

;;; 'fvar'

(defun fvar-axis-instance-arrays-table (fvar)
  "The axis and instance arrays, bounded by the end of the fvar table."
  (let ((off (fvar-axis-instance-arrays-offset-offset fvar)))
    (read-axis-instance-arrays (table-ref-font-data fvar)
                               (+ (table-ref-offset fvar) off)
                               (max 0 (- (table-ref-length fvar) off)))))

(defun fvar-axis-ref (fvar index)
  "VariationAxisRecord INDEX, stepping by axisSize."
  (let ((size (fvar-axis-size fvar))
        (arrays (fvar-axis-instance-arrays-table fvar)))
    (unless (< index (fvar-axis-count fvar))
      (error 'font-out-of-bounds-error :pos index
                                       :size 1
                                       :length (fvar-axis-count fvar)))
    (if (= size +variation-axis-record-size+)
        (axis-instance-arrays-axes-ref arrays (fvar-axis-count fvar) index)
        (%make-variation-axis-record (table-ref-font-data arrays)
                                     (+ (table-ref-offset arrays) (* index size))))))

(defun fvar-instance-ref (fvar index)
  "Named instance INDEX as (values subfamily-name-id flags coordinates postscript-name-id).
COORDINATES is a vector of user-space axis values; POSTSCRIPT-NAME-ID is NIL when absent."
  (let* ((axis-count (fvar-axis-count fvar))
         (size (fvar-instance-size fvar))
         (arrays (fvar-axis-instance-arrays-table fvar))
         (fd (table-ref-font-data arrays))
         (pos (+ (axis-instance-arrays-instances-start arrays axis-count) (* index size))))
    (unless (< index (fvar-instance-count fvar))
      (error 'font-out-of-bounds-error :pos index
                                       :size 1
                                       :length (fvar-instance-count fvar)))
    (unless (and (>= size (+ 4 (* 4 axis-count)))
                 (<= (+ pos size) (+ (table-ref-offset arrays) (table-ref-length arrays))))
      (error 'font-out-of-bounds-error :pos pos
                                       :size size
                                       :length (table-ref-length arrays)))
    (let ((coords (make-array axis-count)))
      (dotimes (i axis-count)
        (setf (aref coords i) (font-data-read-fixed fd (+ pos 4 (* 4 i)))))
      (values (font-data-read-u16 fd pos)
              (font-data-read-u16 fd (+ pos 2))
              coords
              (when (>= size (+ 6 (* 4 axis-count)))
                (font-data-read-u16 fd (+ pos 4 (* 4 axis-count))))))))
