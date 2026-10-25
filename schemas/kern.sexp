;;;; kern.sexp — OpenType table schema
;;;; Generated from Fontations kern.rs spec


(table ot-kern
 (:doc
  "The OpenType [kerning](https://learn.microsoft.com/en-us/typography/opentype/spec/kern) table.")
 (field version u16 :doc "Table version number—set to 0.")
 (field n-tables u16 :doc "Number of subtables in the kerning table.")
 (field subtable-data (array u8) :count :remainder :doc
  "Data for subtables, immediately following the header."))


(table aat-kern
 (:doc
  "The Apple Advanced Typography [kerning](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html) table.")
 (field version major-minor :doc
  "The version number of the kerning table (0x00010000 for the current version).")
 (field n-tables u32 :doc
  "The number of subtables included in the kerning table.")
 (field subtable-data (array u8) :count :remainder :doc
  "Data for subtables, immediately following the header."))


(table ot-subtable (:doc "A subtable in an OT `kern` table.")
 (field version u16 :doc "Kern subtable version number-- set to 0.")
 (field length u16 :doc
  "The length of this subtable in bytes, including this header.")
 (field coverage u16 :doc "Circumstances under which this table is used.")
 (field data (array u8) :count :remainder :doc "Subtable specific data."))


(table aat-subtable (:doc "A subtable in an AAT `kern` table.")
 (field length u32 :doc
  "The length of this subtable in bytes, including this header.")
 (field coverage u16 :doc "Circumstances under which this table is used.")
 (field tuple-index u16 :doc
  "The tuple index (used for variations fonts). This value specifies which tuple this subtable covers.")
 (field data (array u8) :count :remainder :doc "Subtable specific data."))


(table kern-subtable0 (:doc "The type 0 `kern` subtable.")
 (field n-pairs u16 :doc "The number of kerning pairs in this subtable.")
 (field search-range u16 :doc
  "The largest power of two less than or equal to the value of nPairs, multiplied by the size in bytes of an entry in the subtable.")
 (field entry-selector u16 :doc
  "This is calculated as log2 of the largest power of two less than or equal to the value of nPairs. This value indicates how many iterations of the search loop have to be made. For example, in a list of eight items, there would be three iterations of the loop.")
 (field range-shift u16 :doc
  "The value of nPairs minus the largest power of two less than or equal to nPairs. This is multiplied by the size in bytes of an entry in the table.")
 (field pairs (array kerx-subtable0-pair) :count n-pairs :doc
  "Kerning records."))


(table subtable2-class-table
 (:doc "Class table for the type 2 `kern` subtable.")
 (field first-glyph glyph-id16 :doc "First glyph in class range.")
 (field n-glyphs u16 :doc "Number of glyph in class range.")
 (field offsets (array u16) :count n-glyphs :doc
  "The offsets array for all of the glyphs in the range."))


(table subtable3 (:doc "The type 3 'kern' subtable.")
 (field glyph-count u16 :doc "The number of glyphs in this font.")
 (field kern-value-count u8 :doc "The number of kerning values.")
 (field left-class-count u8 :doc "The number of left-hand classes.")
 (field right-class-count u8 :doc "The number of right-hand classes.")
 (field flags u8 :doc "Set to zero (reserved for future use).")
 (field kern-value (array i16) :count kern-value-count :doc
  "The kerning values.")
 (field left-class (array u8) :count glyph-count :doc "The left-hand classes.")
 (field right-class (array u8) :count glyph-count :doc
  "The right-hand classes.")
 (field kern-index (array u8) :count
  (add_multiply left-class-count 0 right-class-count) :doc
  "The indices into the kernValue array."))

