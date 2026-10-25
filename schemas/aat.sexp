;;;; aat.sexp — OpenType table schema
;;;; Generated from Fontations aat.rs spec


(format-table u16 aat-lookup
 (:doc
  "Lookup tables provide a way of looking up information about a glyph index. The different cmap subtable formats.")
 (variant format0 lookup0) (variant format2 lookup2) (variant format4 lookup4)
 (variant format6 lookup6) (variant format8 lookup8)
 (variant format10 lookup10))


(table lookup0
 (:doc
  "Simple array format. The lookup data is an array of lookup values, indexed by glyph index.")
 (field format-table u16 :format 0 :doc "Format number is set to 0.")
 (field values-data (array u8) :count :remainder :doc
  "Values, indexed by glyph index."))


(table lookup2
 (:doc
  "Segment single format. Each non-overlapping segment has a single lookup value that applies to all glyphs in the segment. A segment is defined as a contiguous range of glyph indexes.")
 (field format-table u16 :format 2 :doc "Format number is set to 2.")
 (field unit-size u16 :doc "Size of a lookup unit for this search in bytes.")
 (field n-units u16 :doc
  "Number of units of the preceding size to be searched.")
 (field search-range u16 :doc
  "The value of unitSize times the largest power of 2 that is less than or equal to the value of nUnits.")
 (field entry-selector u16 :doc
  "The log base 2 of the largest power of 2 less than or equal to the value of nUnits.")
 (field range-shift u16 :doc
  "The value of unitSize times the difference of the value of nUnits minus the largest power of 2 less than or equal to the value of nUnits.")
 (field segments-data (array u8) :count (add_multiply unit-size 0 n-units) :doc
  "Segments."))


(table lookup4
 (:doc
  "Segment array format. A segment mapping is performed (as with Format 2), but instead of a single lookup value for all the glyphs in the segment, each glyph in the segment gets its own separate lookup value.")
 (field format-table u16 :format 4 :doc "Format number is set to 4.")
 (field unit-size u16 :doc "Size of a lookup unit for this search in bytes.")
 (field n-units u16 :doc
  "Number of units of the preceding size to be searched.")
 (field search-range u16 :doc
  "The value of unitSize times the largest power of 2 that is less than or equal to the value of nUnits.")
 (field entry-selector u16 :doc
  "The log base 2 of the largest power of 2 less than or equal to the value of nUnits.")
 (field range-shift u16 :doc
  "The value of unitSize times the difference of the value of nUnits minus the largest power of 2 less than or equal to the value of nUnits.")
 (field segments (array lookup-segment4) :count n-units :doc "Segments."))


(record lookup-segment4 (:doc "Lookup segment for format 4.")
 (field last-glyph u16 :doc "Last glyph index in this segment.")
 (field first-glyph u16 :doc "First glyph index in this segment.")
 (field value-offset u16 :doc
  "A 16-bit offset from the start of the table to the data."))


(table lookup6
 (:doc
  "Single table format. The lookup data is a sorted list of <glyph index,lookup value> pairs.")
 (field format-table u16 :format 6 :doc "Format number is set to 6.")
 (field unit-size u16 :doc "Size of a lookup unit for this search in bytes.")
 (field n-units u16 :doc
  "Number of units of the preceding size to be searched.")
 (field search-range u16 :doc
  "The value of unitSize times the largest power of 2 that is less than or equal to the value of nUnits.")
 (field entry-selector u16 :doc
  "The log base 2 of the largest power of 2 less than or equal to the value of nUnits.")
 (field range-shift u16 :doc
  "The value of unitSize times the difference of the value of nUnits minus the largest power of 2 less than or equal to the value of nUnits.")
 (field entries-data (array u8) :count (add_multiply unit-size 0 n-units) :doc
  "Values, indexed by glyph index."))


(table lookup8
 (:doc
  "Trimmed array format. The lookup data is a simple trimmed array indexed by glyph index.")
 (field format-table u16 :format 8 :doc "Format number is set to 8.")
 (field first-glyph u16 :doc
  "First glyph index included in the trimmed array.")
 (field glyph-count u16 :doc
  "Total number of glyphs (equivalent to the last glyph minus the value of firstGlyph plus 1).")
 (field value-array (array u16) :count glyph-count :doc
  "The lookup values (indexed by the glyph index minus the value of firstGlyph). Entries in the value array must be two bytes."))


(table lookup10
 (:doc
  "Trimmed array format. The lookup data is a simple trimmed array indexed by glyph index.")
 (field format-table u16 :format 10 :doc "Format number is set to 10.")
 (field unit-size u16 :doc
  "Size of a lookup unit for this lookup table in bytes. Allowed values are 1, 2, 4, and 8.")
 (field first-glyph u16 :doc
  "First glyph index included in the trimmed array.")
 (field glyph-count u16 :doc
  "Total number of glyphs (equivalent to the last glyph minus the value of firstGlyph plus 1).")
 (field values-data (array u8) :count (add_multiply glyph-count 0 unit-size)
  :doc
  "The lookup values (indexed by the glyph index minus the value of firstGlyph)."))


(table state-header (:doc "Header for a state table.")
 (field state-size u16 :doc
  "Size of a state, in bytes. The size is limited to 8 bits, although the field is 16 bits for alignment.")
 (field class-table-offset (offset16 class-subtable) :doc
  "Byte offset from the beginning of the state table to the class subtable.")
 (field state-array-offset (offset16 raw-bytes) :doc
  "Byte offset from the beginning of the state table to the state array.")
 (field entry-table-offset (offset16 raw-bytes) :doc
  "Byte offset from the beginning of the state table to the entry subtable."))


(table class-subtable
 (:doc "Maps the glyph indexes of your font into classes.")
 (field first-glyph u16 :doc
  "Glyph index of the first glyph in the class table.")
 (field n-glyphs u16 :doc "Number of glyphs in class table.")
 (field class-array (array u8) :count n-glyphs :doc
  "The class codes (indexed by glyph index minus firstGlyph). Class codes range from 0 to the value of stateSize minus 1."))


(table raw-bytes
 (:doc
  "Used for the `state_array` and `entry_table` fields in [`StateHeader`].")
 (field data (array u8) :count :remainder))


(table stx-header (:doc "Header for an extended state table.")
 (field n-classes u32 :doc
  "Number of classes, which is the number of 16-bit entry indices in a single line in the state array.")
 (field class-table-offset (offset32 lookup-u16) :doc
  "Byte offset from the beginning of the state table to the class subtable.")
 (field state-array-offset (offset32 raw-words) :doc
  "Byte offset from the beginning of the state table to the state array.")
 (field entry-table-offset (offset32 raw-bytes) :doc
  "Byte offset from the beginning of the state table to the entry subtable."))


(table raw-words (:doc "Used for the `state_array` in [`StxHeader`].")
 (field data (array u16) :count :remainder))

