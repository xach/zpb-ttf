;;;; kerx.sexp — OpenType table schema
;;;; Generated from Fontations kerx.rs spec


(table kerx (:tag "kerx")
 (:doc
  "The [kerx (Extended Kerning)](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6morx.html) table.")
 (field version u16 :doc
  "The version number of the extended kerning table (currently 2, 3, or 4)")
 (field padding u16 :doc "Unused; set to zero.")
 (field n-tables u32 :doc
  "The number of subtables included in the extended kerning table.")
 (field subtables var-len-array<subtable<a>>))


(table kerx-subtable (:doc "A subtable in a `kerx` table.")
 (field length u32 :doc
  "The length of this subtable in bytes, including this header.")
 (field coverage u32 :doc "Circumstances under which this table is used.")
 (field tuple-count u32 :doc
  "The tuple count. This value is only used with variation fonts and should be 0 for all other fonts. The subtable's tupleCount will be ignored if the 'kerx' table version is less than 4.")
 (field data (array u8) :count :remainder :doc "Subtable specific data."))


(table kerx-subtable0 (:doc "The type 0 `kerx` subtable.")
 (field n-pairs u32 :doc "The number of kerning pairs in this subtable.")
 (field search-range u32 :doc
  "The largest power of two less than or equal to the value of nPairs, multiplied by the size in bytes of an entry in the subtable.")
 (field entry-selector u32 :doc
  "This is calculated as log2 of the largest power of two less than or equal to the value of nPairs. This value indicates how many iterations of the search loop have to be made. For example, in a list of eight items, there would be three iterations of the loop.")
 (field range-shift u32 :doc
  "The value of nPairs minus the largest power of two less than or equal to nPairs. This is multiplied by the size in bytes of an entry in the table.")
 (field pairs (array kerx-subtable0-pair) :count n-pairs :doc
  "Kerning records."))


(record kerx-subtable0-pair (:doc "The type 0 `kerx` subtable kerning record.")
 (field left glyph-id16 :doc
  "The glyph index for the lefthand glyph in the kerning pair.")
 (field right glyph-id16 :doc
  "The glyph index for the righthand glyph in the kerning pair.")
 (field value i16 :doc "Kerning value."))

