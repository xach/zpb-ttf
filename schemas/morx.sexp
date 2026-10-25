;;;; morx.sexp — OpenType table schema
;;;; Generated from Fontations morx.rs spec


(table morx (:tag "morx")
 (:doc
  "The [morx (Extended Glyph Metamorphosis)](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6morx.html) table.")
 (field version u16 :doc
  "Version number of the extended glyph metamorphosis table (either 2 or 3).")
 (field unused u16 :doc "Not used; set to 0.")
 (field n-chains u32 :doc
  "Number of metamorphosis chains contained in this table.")
 (field chains var-len-array<chain<a>>))


(table chain (:doc "A chain in a `morx` table.")
 (field default-flags u32 :doc "The default specification for subtables.")
 (field chain-length u32 :doc
  "Total byte count, including this header; must be a multiple of 4.")
 (field n-feature-entries u32 :doc "Number of feature subtable entries.")
 (field n-subtables u32 :doc "The number of subtables in the chain.")
 (field features (array morx-feature) :count n-feature-entries :doc
  "Feature entries for this chain.")
 (field subtables var-len-array<subtable<a>> :doc "Array of chain subtables."))


(record morx-feature
 (:doc
  "Used to compute the sub-feature flags for a list of requested features and settings.")
 (field feature-type u16 :doc "The type of feature.")
 (field feature-settings u16 :doc "The feature's setting (aka selector).")
 (field enable-flags u32 :doc
  "Flags for the settings that this feature and setting enables.")
 (field disable-flags u32 :doc
  "Complement of flags for the settings that this feature and setting disable."))


(table subtable (:doc "A subtable in a `morx` chain.")
 (field length u32 :doc "Total subtable length, including this header.")
 (field coverage u32 :doc "Coverage flags and subtable type.")
 (field sub-feature-flags u32 :doc
  "The 32-bit mask identifying which subtable this is (the subtable being executed if the AND of this value and the processed defaultFlags is nonzero).")
 (field data (array u8) :count :remainder :doc "Data for specific subtable."))


(record contextual-entry-data
 (:doc "Entry payload in a contextual subtable state machine.")
 (field mark-index u16 :doc
  "Index of the substitution table for the marked glyph (use 0xFFFF for none).")
 (field current-index u16 :doc
  "Index of the substitution table for the current glyph (use 0xFFFF for none)"))


(record insertion-entry-data
 (:doc "Entry payload in an insertion subtable state machine.")
 (field current-insert-index u16 :doc
  "Zero-based index into the insertion glyph table. The number of glyphs to be inserted is contained in the currentInsertCount field in the flags (see below). A value of 0xFFFF indicates no insertion is to be done.")
 (field marked-insert-index u16 :doc
  "Zero-based index into the insertion glyph table. The number of glyphs to be inserted is contained in the markedInsertCount field in the flags (see below). A value of 0xFFFF indicates no insertion is to be done."))

