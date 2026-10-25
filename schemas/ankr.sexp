;;;; ankr.sexp — OpenType table schema
;;;; Generated from Fontations ankr.rs spec


(table ankr (:tag "ankr")
 (:doc
  "The [anchor point](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6ankr.html) table.")
 (field version u16 :doc "Version number (set to zero).")
 (field flags u16 :doc "Flags (currently unused; set to zero).")
 (field lookup-table-offset (offset32 lookup-u16) :doc
  "Offset to the table's lookup table; currently this is always `0x0000000C`.  Lookup values are two byte offsets into the glyph data table.")
 (field glyph-data-table-offset u32 :doc "Offset to the glyph data table."))


(table glyph-data-entry
 (field num-points u32 :doc "Number of anchor points for this glyph.")
 (field anchor-points (array anchor-point) :count num-points :doc
  "Individual anchor points."))


(record anchor-point (:doc "Individual anchor point.") (field x i16)
 (field y i16))

