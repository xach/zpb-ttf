;;;; os2.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+os2-tag+ +selection-flags-bold+ +selection-flags-italic+
   +selection-flags-negative+ +selection-flags-oblique+
   +selection-flags-outlined+ +selection-flags-regular+
   +selection-flags-strikeout+ +selection-flags-underscore+
   +selection-flags-use-typo-metrics+ +selection-flags-wws+ os2 os2-ach-vend-id
   os2-fs-selection os2-fs-type os2-p os2-panose-10-bytes os2-panose-10-count
   os2-panose-10-ref os2-s-cap-height os2-s-family-class os2-s-typo-ascender
   os2-s-typo-descender os2-s-typo-line-gap os2-sx-height
   os2-ul-code-page-range-1 os2-ul-code-page-range-2 os2-ul-unicode-range-1
   os2-ul-unicode-range-2 os2-ul-unicode-range-3 os2-ul-unicode-range-4
   os2-us-break-char os2-us-default-char os2-us-first-char-index
   os2-us-last-char-index os2-us-lower-optical-point-size os2-us-max-context
   os2-us-upper-optical-point-size os2-us-weight-class os2-us-width-class
   os2-us-win-ascent os2-us-win-descent os2-version os2-x-avg-char-width
   os2-y-strikeout-position os2-y-strikeout-size os2-y-subscript-x-offset
   os2-y-subscript-x-size os2-y-subscript-y-offset os2-y-subscript-y-size
   os2-y-superscript-x-offset os2-y-superscript-x-size
   os2-y-superscript-y-offset os2-y-superscript-y-size read-os2 sanitize-os2
   selection-flags-bold-p selection-flags-italic-p selection-flags-negative-p
   selection-flags-oblique-p selection-flags-outlined-p
   selection-flags-regular-p selection-flags-strikeout-p
   selection-flags-underscore-p selection-flags-use-typo-metrics-p
   selection-flags-wws-p))

(define-constant +selection-flags-italic+ 1
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-italic-p))

(defun selection-flags-italic-p (flags)
  (logtest flags +selection-flags-italic+))

(define-constant +selection-flags-underscore+ 2
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-underscore-p))

(defun selection-flags-underscore-p (flags)
  (logtest flags +selection-flags-underscore+))

(define-constant +selection-flags-negative+ 4
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-negative-p))

(defun selection-flags-negative-p (flags)
  (logtest flags +selection-flags-negative+))

(define-constant +selection-flags-outlined+ 8
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-outlined-p))

(defun selection-flags-outlined-p (flags)
  (logtest flags +selection-flags-outlined+))

(define-constant +selection-flags-strikeout+ 16
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-strikeout-p))

(defun selection-flags-strikeout-p (flags)
  (logtest flags +selection-flags-strikeout+))

(define-constant +selection-flags-bold+ 32
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-bold-p))

(defun selection-flags-bold-p (flags) (logtest flags +selection-flags-bold+))

(define-constant +selection-flags-regular+ 64
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-regular-p))

(defun selection-flags-regular-p (flags)
  (logtest flags +selection-flags-regular+))

(define-constant +selection-flags-use-typo-metrics+ 128
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-use-typo-metrics-p))

(defun selection-flags-use-typo-metrics-p (flags)
  (logtest flags +selection-flags-use-typo-metrics+))

(define-constant +selection-flags-wws+ 256
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-wws-p))

(defun selection-flags-wws-p (flags) (logtest flags +selection-flags-wws+))

(define-constant +selection-flags-oblique+ 512
                 "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")

(declaim (inline selection-flags-oblique-p))

(defun selection-flags-oblique-p (flags)
  (logtest flags +selection-flags-oblique+))

(defstruct
    (os2 (:include table-ref)
     (:constructor %make-os2 (%table-font-data %table-offset %table-length)))
  "[`OS/2`](https://docs.microsoft.com/en-us/typography/opentype/spec/os2)")

(declaim
 (inline os2-version os2-version-raw os2-x-avg-char-width
  os2-x-avg-char-width-raw os2-us-weight-class os2-us-weight-class-raw
  os2-us-width-class os2-us-width-class-raw os2-fs-type os2-fs-type-raw
  os2-y-subscript-x-size os2-y-subscript-x-size-raw os2-y-subscript-y-size
  os2-y-subscript-y-size-raw os2-y-subscript-x-offset
  os2-y-subscript-x-offset-raw os2-y-subscript-y-offset
  os2-y-subscript-y-offset-raw os2-y-superscript-x-size
  os2-y-superscript-x-size-raw os2-y-superscript-y-size
  os2-y-superscript-y-size-raw os2-y-superscript-x-offset
  os2-y-superscript-x-offset-raw os2-y-superscript-y-offset
  os2-y-superscript-y-offset-raw os2-y-strikeout-size os2-y-strikeout-size-raw
  os2-y-strikeout-position os2-y-strikeout-position-raw os2-s-family-class
  os2-s-family-class-raw os2-panose-10-count os2-panose-10-ref
  os2-panose-10-bytes os2-ul-unicode-range-1 os2-ul-unicode-range-1-raw
  os2-ul-unicode-range-2 os2-ul-unicode-range-2-raw os2-ul-unicode-range-3
  os2-ul-unicode-range-3-raw os2-ul-unicode-range-4 os2-ul-unicode-range-4-raw
  os2-ach-vend-id os2-ach-vend-id-raw os2-fs-selection os2-fs-selection-raw
  os2-us-first-char-index os2-us-first-char-index-raw os2-us-last-char-index
  os2-us-last-char-index-raw os2-s-typo-ascender os2-s-typo-ascender-raw
  os2-s-typo-descender os2-s-typo-descender-raw os2-s-typo-line-gap
  os2-s-typo-line-gap-raw os2-us-win-ascent os2-us-win-ascent-raw
  os2-us-win-descent os2-us-win-descent-raw os2-ul-code-page-range-1
  os2-ul-code-page-range-1-raw os2-ul-code-page-range-2
  os2-ul-code-page-range-2-raw os2-sx-height os2-sx-height-raw os2-s-cap-height
  os2-s-cap-height-raw os2-us-default-char os2-us-default-char-raw
  os2-us-break-char os2-us-break-char-raw os2-us-max-context
  os2-us-max-context-raw os2-us-lower-optical-point-size
  os2-us-lower-optical-point-size-raw os2-us-upper-optical-point-size
  os2-us-upper-optical-point-size-raw))

(define-constant +os2-tag+ "OS/2")

(defun os2-version (table)
  (declare (type os2 table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun os2-version-raw (font-data offset) (font-data-read-u16 font-data offset))

(defun os2-x-avg-char-width (table)
  "[Average weighted escapement](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#xavgcharwidth).  The Average Character Width parameter specifies the arithmetic average of the escapement (width) of all non-zero width glyphs in the font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun os2-x-avg-char-width-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 2)))

(defun os2-us-weight-class (table)
  "[Weight class](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#usweightclass).  Indicates the visual weight (degree of blackness or thickness of strokes) of the characters in the font. Values from 1 to 1000 are valid."
  (declare (type os2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun os2-us-weight-class-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun os2-us-width-class (table)
  "[Width class](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#uswidthclass).  Indicates a relative change from the normal aspect ratio (width to height ratio) as specified by a font designer for the glyphs in a font."
  (declare (type os2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun os2-us-width-class-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 6)))

(defun os2-fs-type (table)
  "[Type flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fstype).  Indicates font embedding licensing rights for the font."
  (declare (type os2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun os2-fs-type-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 8)))

(defun os2-y-subscript-x-size (table)
  "The recommended horizontal size in font design units for subscripts for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun os2-y-subscript-x-size-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 10)))

(defun os2-y-subscript-y-size (table)
  "The recommended vertical size in font design units for subscripts for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun os2-y-subscript-y-size-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 12)))

(defun os2-y-subscript-x-offset (table)
  "The recommended horizontal offset in font design units for subscripts for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 14)))

(defun os2-y-subscript-x-offset-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 14)))

(defun os2-y-subscript-y-offset (table)
  "The recommended vertical offset in font design units for subscripts for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun os2-y-subscript-y-offset-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 16)))

(defun os2-y-superscript-x-size (table)
  "The recommended horizontal size in font design units for superscripts for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 18)))

(defun os2-y-superscript-x-size-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 18)))

(defun os2-y-superscript-y-size (table)
  "The recommended vertical size in font design units for superscripts for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 20)))

(defun os2-y-superscript-y-size-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 20)))

(defun os2-y-superscript-x-offset (table)
  "The recommended horizontal offset in font design units for superscripts for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 22)))

(defun os2-y-superscript-x-offset-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 22)))

(defun os2-y-superscript-y-offset (table)
  "The recommended vertical offset in font design units for superscripts for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 24)))

(defun os2-y-superscript-y-offset-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 24)))

(defun os2-y-strikeout-size (table)
  "Thickness of the strikeout stroke in font design units."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 26)))

(defun os2-y-strikeout-size-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 26)))

(defun os2-y-strikeout-position (table)
  "The position of the top of the strikeout stroke relative to the baseline in font design units."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 28)))

(defun os2-y-strikeout-position-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 28)))

(defun os2-s-family-class (table)
  "[Font-family class and subclass](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#sfamilyclass). This parameter is a classification of font-family design."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 30)))

(defun os2-s-family-class-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 30)))

(defun os2-panose-10-count (table)
  "[PANOSE classification number](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#panose).  Additional specifications are required for PANOSE to classify non-Latin character sets."
  (declare (type os2 table))
  (declare (ignorable table))
  10)

(defun os2-panose-10-ref (table index)
  "[PANOSE classification number](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#panose).  Additional specifications are required for PANOSE to classify non-Latin character sets."
  (declare (type os2 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 32) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun os2-panose-10-bytes (table)
  "[PANOSE classification number](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#panose).  Additional specifications are required for PANOSE to classify non-Latin character sets."
  (declare (type os2 table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 32)
                   (os2-panose-10-count table)))

(defun os2-ul-unicode-range-1 (table)
  "[Unicode Character Range](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#ulunicoderange1-bits-031ulunicoderange2-bits-3263ulunicoderange3-bits-6495ulunicoderange4-bits-96127).  Unicode Character Range (bits 0-31)."
  (declare (type os2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 42)))

(defun os2-ul-unicode-range-1-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 42)))

(defun os2-ul-unicode-range-2 (table)
  "Unicode Character Range (bits 32-63)."
  (declare (type os2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 46)))

(defun os2-ul-unicode-range-2-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 46)))

(defun os2-ul-unicode-range-3 (table)
  "Unicode Character Range (bits 64-95)."
  (declare (type os2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 50)))

(defun os2-ul-unicode-range-3-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 50)))

(defun os2-ul-unicode-range-4 (table)
  "Unicode Character Range (bits 96-127)."
  (declare (type os2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 54)))

(defun os2-ul-unicode-range-4-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 54)))

(defun os2-ach-vend-id (table)
  "[Font Vendor Identification](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#achvendid).  The four-character identifier for the vendor of the given type face."
  (declare (type os2 table))
  (font-data-read-tag-string (table-ref-font-data table)
                             (+ (table-ref-offset table) 58)))

(defun os2-ach-vend-id-raw (font-data offset)
  (font-data-read-tag-string font-data (+ offset 58)))

(defun os2-fs-selection (table)
  "[Font selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection).  Contains information concerning the nature of the font patterns."
  (declare (type os2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 62)))

(defun os2-fs-selection-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 62)))

(defun os2-us-first-char-index (table)
  "The minimum Unicode index (character code) in this font."
  (declare (type os2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 64)))

(defun os2-us-first-char-index-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 64)))

(defun os2-us-last-char-index (table)
  "The maximum Unicode index (character code) in this font."
  (declare (type os2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 66)))

(defun os2-us-last-char-index-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 66)))

(defun os2-s-typo-ascender (table)
  "The typographic ascender for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 68)))

(defun os2-s-typo-ascender-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 68)))

(defun os2-s-typo-descender (table)
  "The typographic descender for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 70)))

(defun os2-s-typo-descender-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 70)))

(defun os2-s-typo-line-gap (table)
  "The typographic line gap for this font."
  (declare (type os2 table))
  (font-data-read-i16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 72)))

(defun os2-s-typo-line-gap-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 72)))

(defun os2-us-win-ascent (table)
  "The “Windows ascender” metric.  This should be used to specify the height above the baseline for a clipping region."
  (declare (type os2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 74)))

(defun os2-us-win-ascent-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 74)))

(defun os2-us-win-descent (table)
  "The “Windows descender” metric.  This should be used to specify the vertical extent below the baseline for a clipping region."
  (declare (type os2 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 76)))

(defun os2-us-win-descent-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 76)))

(defun os2-ul-code-page-range-1 (table)
  "Code page character range bits 0-31."
  (declare (type os2 table))
  (when
      (and (>= (os2-version table) 1)
           (<= (+ (+ (table-ref-offset table) 78) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 78))))

(defun os2-ul-code-page-range-1-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 78)))

(defun os2-ul-code-page-range-2 (table)
  "Code page character range bits 32-63."
  (declare (type os2 table))
  (when
      (and (>= (os2-version table) 1)
           (<= (+ (+ (table-ref-offset table) 82) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 82))))

(defun os2-ul-code-page-range-2-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 82)))

(defun os2-sx-height (table)
  "This metric specifies the distance between the baseline and the approximate height of non-ascending lowercase letters measured in FUnits."
  (declare (type os2 table))
  (when
      (and (>= (os2-version table) 2)
           (<= (+ (+ (table-ref-offset table) 86) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-i16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 86))))

(defun os2-sx-height-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 86)))

(defun os2-s-cap-height (table)
  "This metric specifies the distance between the baseline and the approximate height of uppercase letters measured in FUnits."
  (declare (type os2 table))
  (when
      (and (>= (os2-version table) 2)
           (<= (+ (+ (table-ref-offset table) 88) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-i16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 88))))

(defun os2-s-cap-height-raw (font-data offset)
  (font-data-read-i16 font-data (+ offset 88)))

(defun os2-us-default-char (table)
  "This is the Unicode codepoint, in UTF-16 encoding, of a character that can be used for a default glyph."
  (declare (type os2 table))
  (when
      (and (>= (os2-version table) 2)
           (<= (+ (+ (table-ref-offset table) 90) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 90))))

(defun os2-us-default-char-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 90)))

(defun os2-us-break-char (table)
  "This is the Unicode codepoint, in UTF-16 encoding, of a character that can be used as a default break character."
  (declare (type os2 table))
  (when
      (and (>= (os2-version table) 2)
           (<= (+ (+ (table-ref-offset table) 92) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 92))))

(defun os2-us-break-char-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 92)))

(defun os2-us-max-context (table)
  "This field is used for fonts with multiple optical styles."
  (declare (type os2 table))
  (when
      (and (>= (os2-version table) 2)
           (<= (+ (+ (table-ref-offset table) 94) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 94))))

(defun os2-us-max-context-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 94)))

(defun os2-us-lower-optical-point-size (table)
  "This field is used for fonts with multiple optical styles."
  (declare (type os2 table))
  (when
      (and (>= (os2-version table) 5)
           (<= (+ (+ (table-ref-offset table) 96) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 96))))

(defun os2-us-lower-optical-point-size-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 96)))

(defun os2-us-upper-optical-point-size (table)
  "This field is used for fonts with multiple optical styles."
  (declare (type os2 table))
  (when
      (and (>= (os2-version table) 5)
           (<= (+ (+ (table-ref-offset table) 98) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 98))))

(defun os2-us-upper-optical-point-size-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 98)))

(defun sanitize-os2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 78 length) (font-data-valid-range-p font-data offset 78))
    (error 'font-out-of-bounds-error :pos offset :size 78 :length
           (font-data-length font-data)))
  t)

(defun read-os2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-os2 font-data offset length)
  (%make-os2 font-data offset length))
