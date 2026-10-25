;;;; mort.sexp — OpenType table schema
;;;; Generated from Fontations mort.rs spec


(table mort (:tag "mort")
 (:doc
  "The [mort (Glyph Metamorphosis)](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6mort.html) table.")
 (field version u16 :doc
  "Version number of the glyph metamorphosis table.")
 (field unused u16 :doc "Not used; set to 0.")
 (field n-chains u32 :doc
  "Number of metamorphosis chains contained in this table.")
 (field chains var-len-array<mort-chain<a>>))


(table mort-chain (:doc "A chain in a `mort` table.")
 (field default-flags u32 :doc
  "The default specification for subtables.")
 (field chain-length u32 :doc
  "Total byte count, including this header.")
 (field n-feature-entries u16 :doc
  "Number of feature subtable entries.")
 (field n-subtables u16 :doc
  "The number of subtables in the chain.")
 (field features (array mort-feature) :count
  n-feature-entries :doc "Feature entries for this chain.")
 (field subtables var-len-array<mort-subtable<a>> :doc
  "Array of chain subtables."))


(record mort-feature
 (:doc
  "Used to compute the sub-feature flags for a list of requested features and settings.")
 (field feature-type u16 :doc "The type of feature.")
 (field feature-settings u16 :doc
  "The feature's setting (aka selector).")
 (field enable-flags u32 :doc
  "Flags for the settings that this feature and setting enables.")
 (field disable-flags u32 :doc
  "Complement of flags for the settings that this feature and setting disable."))


(table mort-subtable (:doc "A subtable in a `mort` chain.")
 (field length u16 :doc
  "Total subtable length, including this header.")
 (field coverage u16 :doc "Coverage flags and subtable type.")
 (field sub-feature-flags u32 :doc
  "The 32-bit mask identifying which subtable this is.")
 (field data (array u8) :count :remainder :doc
  "Data for the specific subtable type."))


(record mort-contextual-entry-data
 (:doc "Entry payload in a contextual subtable state machine.")
 (field mark-offset i16 :doc
  "Signed word offset of the substitution array for the marked glyph (0 for none).")
 (field current-offset i16 :doc
  "Signed word offset of the substitution array for the current glyph (0 for none)."))


(record mort-insertion-entry-data
 (:doc "Entry payload in an insertion subtable state machine.")
 (field current-insert-index u16 :doc
  "Zero-based index into the insertion glyph table (0xFFFF for none).")
 (field marked-insert-index u16 :doc
  "Zero-based index into the insertion glyph table (0xFFFF for none)."))

