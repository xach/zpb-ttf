;;;; os2.sexp — OpenType table schema
;;;; Generated from Fontations os2.rs spec


(flags u16 selection-flags
 (:doc
  "OS/2 [selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection)")
 (italic 1) (underscore 2) (negative 4) (outlined 8) (strikeout 16) (bold 32)
 (regular 64) (use-typo-metrics 128) (wws 256) (oblique 512))


(table os2 (:tag "OS/2")
 (:doc
  "[`OS/2`](https://docs.microsoft.com/en-us/typography/opentype/spec/os2)")
 (field version u16)
 (field x-avg-char-width i16 :doc
  "[Average weighted escapement](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#xavgcharwidth).  The Average Character Width parameter specifies the arithmetic average of the escapement (width) of all non-zero width glyphs in the font.")
 (field us-weight-class u16 :doc
  "[Weight class](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#usweightclass).  Indicates the visual weight (degree of blackness or thickness of strokes) of the characters in the font. Values from 1 to 1000 are valid.")
 (field us-width-class u16 :doc
  "[Width class](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#uswidthclass).  Indicates a relative change from the normal aspect ratio (width to height ratio) as specified by a font designer for the glyphs in a font.")
 (field fs-type u16 :doc
  "[Type flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fstype).  Indicates font embedding licensing rights for the font.")
 (field y-subscript-x-size i16 :doc
  "The recommended horizontal size in font design units for subscripts for this font.")
 (field y-subscript-y-size i16 :doc
  "The recommended vertical size in font design units for subscripts for this font.")
 (field y-subscript-x-offset i16 :doc
  "The recommended horizontal offset in font design units for subscripts for this font.")
 (field y-subscript-y-offset i16 :doc
  "The recommended vertical offset in font design units for subscripts for this font.")
 (field y-superscript-x-size i16 :doc
  "The recommended horizontal size in font design units for superscripts for this font.")
 (field y-superscript-y-size i16 :doc
  "The recommended vertical size in font design units for superscripts for this font.")
 (field y-superscript-x-offset i16 :doc
  "The recommended horizontal offset in font design units for superscripts for this font.")
 (field y-superscript-y-offset i16 :doc
  "The recommended vertical offset in font design units for superscripts for this font.")
 (field y-strikeout-size i16 :doc
  "Thickness of the strikeout stroke in font design units.")
 (field y-strikeout-position i16 :doc
  "The position of the top of the strikeout stroke relative to the baseline in font design units.")
 (field s-family-class i16 :doc
  "[Font-family class and subclass](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#sfamilyclass). This parameter is a classification of font-family design.")
 (field panose-10 (array u8) :count 10 :doc
  "[PANOSE classification number](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#panose).  Additional specifications are required for PANOSE to classify non-Latin character sets.")
 (field ul-unicode-range-1 u32 :doc
  "[Unicode Character Range](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#ulunicoderange1-bits-031ulunicoderange2-bits-3263ulunicoderange3-bits-6495ulunicoderange4-bits-96127).  Unicode Character Range (bits 0-31).")
 (field ul-unicode-range-2 u32 :doc "Unicode Character Range (bits 32-63).")
 (field ul-unicode-range-3 u32 :doc "Unicode Character Range (bits 64-95).")
 (field ul-unicode-range-4 u32 :doc "Unicode Character Range (bits 96-127).")
 (field ach-vend-id tag :doc
  "[Font Vendor Identification](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#achvendid).  The four-character identifier for the vendor of the given type face.")
 (field fs-selection selection-flags :doc
  "[Font selection flags](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection).  Contains information concerning the nature of the font patterns.")
 (field us-first-char-index u16 :doc
  "The minimum Unicode index (character code) in this font.")
 (field us-last-char-index u16 :doc
  "The maximum Unicode index (character code) in this font.")
 (field s-typo-ascender i16 :doc "The typographic ascender for this font.")
 (field s-typo-descender i16 :doc "The typographic descender for this font.")
 (field s-typo-line-gap i16 :doc "The typographic line gap for this font.")
 (field us-win-ascent u16 :doc
  "The “Windows ascender” metric.  This should be used to specify the height above the baseline for a clipping region.")
 (field us-win-descent u16 :doc
  "The “Windows descender” metric.  This should be used to specify the vertical extent below the baseline for a clipping region.")
 (field ul-code-page-range-1 u32 :since-version "1" :doc
  "Code page character range bits 0-31.")
 (field ul-code-page-range-2 u32 :since-version "1" :doc
  "Code page character range bits 32-63.")
 (field sx-height i16 :since-version "2" :doc
  "This metric specifies the distance between the baseline and the approximate height of non-ascending lowercase letters measured in FUnits.")
 (field s-cap-height i16 :since-version "2" :doc
  "This metric specifies the distance between the baseline and the approximate height of uppercase letters measured in FUnits.")
 (field us-default-char u16 :since-version "2" :doc
  "This is the Unicode codepoint, in UTF-16 encoding, of a character that can be used for a default glyph.")
 (field us-break-char u16 :since-version "2" :doc
  "This is the Unicode codepoint, in UTF-16 encoding, of a character that can be used as a default break character.")
 (field us-max-context u16 :since-version "2" :doc
  "This field is used for fonts with multiple optical styles.")
 (field us-lower-optical-point-size u16 :since-version "5" :doc
  "This field is used for fonts with multiple optical styles.")
 (field us-upper-optical-point-size u16 :since-version "5" :doc
  "This field is used for fonts with multiple optical styles."))

