;;;; cmap.sexp — OpenType table schema
;;;; Generated from Fontations cmap.rs spec


(table cmap (:tag "cmap")
 (:doc
  "[cmap](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#overview)")
 (field version u16 :doc "Table version number (0).")
 (field num-tables u16 :doc "Number of encoding tables that follow.")
 (field encoding-records (array encoding-record) :count num-tables))


(table dmap (:tag "DMAP")
 (:doc "Delta map table (ISO/IEC 14496-22:2026, 5.6.15).")
 (field version u16 :doc "Table version number (0).")
 (field num-tables u16 :doc "Number of encoding tables that follow.")
 (field encoding-records (array encoding-record) :count num-tables))


(record encoding-record
 (:doc
  "[Encoding Record](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#encoding-records-and-encodings)")
 (field platform-id platform-id :doc "Platform ID.")
 (field encoding-id u16 :doc "Platform-specific encoding ID.")
 (field subtable-offset (offset32 cmap-subtable) :doc
  "Byte offset from beginning of the [`Cmap`] or [`Dmap`] table to the subtable for this encoding."))


(enum u16 platform-id
 (:doc
  "<https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#platform-ids>")
 (unicode 0) (macintosh 1) (iso 2) (windows 3) (custom 4))


(format-table u16 cmap-subtable (:doc "The different cmap subtable formats.")
 (variant format0 cmap0) (variant format2 cmap2) (variant format4 cmap4)
 (variant format6 cmap6) (variant format8 cmap8) (variant format10 cmap10)
 (variant format12 cmap12) (variant format13 cmap13) (variant format14 cmap14)
 (variant format15 cmap15))


(table cmap0
 (:doc
  "[cmap Format 0](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-0-byte-encoding-table): Byte encoding table")
 (field format-table u16 :format 0 :doc "Format number is set to 0.")
 (field length u16 :doc "This is the length in bytes of the subtable.")
 (field language u16 :doc
  "For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.")
 (field glyph-id-array (array u8) :count 256 :doc
  "An array that maps character codes to glyph index values."))


(table cmap2
 (:doc
  "[cmap Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-2-high-byte-mapping-through-table): High-byte mapping through table")
 (field format-table u16 :format 2 :doc "Format number is set to 2.")
 (field length u16 :doc "This is the length in bytes of the subtable.")
 (field language u16 :doc
  "For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.")
 (field sub-header-keys (array u16) :count 256 :doc
  "Array that maps high bytes to subHeaders: value is subHeader index × 8."))


(record sub-header (:doc "Part of [Cmap2]")
 (field first-code u16 :doc "First valid low byte for this SubHeader.")
 (field entry-count u16 :doc "Number of valid low bytes for this SubHeader.")
 (field id-delta i16 :doc "See text below.")
 (field id-range-offset u16 :doc "See text below."))


(table cmap4
 (:doc
  "[cmap Format 4](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-4-segment-mapping-to-delta-values): Segment mapping to delta values")
 (field format-table u16 :format 4 :doc "Format number is set to 4.")
 (field length u16 :doc "This is the length in bytes of the subtable.")
 (field language u16 :doc
  "For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.")
 (field seg-count-x2 u16 :doc "2 × segCount.")
 (field search-range u16 :doc
  "Maximum power of 2 less than or equal to segCount, times 2 ((2**floor(log2(segCount))) * 2, where “**” is an exponentiation operator)")
 (field entry-selector u16 :doc
  "Log2 of the maximum power of 2 less than or equal to numTables (log2(searchRange/2), which is equal to floor(log2(segCount)))")
 (field range-shift u16 :doc
  "segCount times 2, minus searchRange ((segCount * 2) - searchRange)")
 (field end-code (array u16) :count (half seg-count-x2) :doc
  "End characterCode for each segment, last=0xFFFF.")
 (field reserved-pad u16 :doc "Set to 0.")
 (field start-code (array u16) :count (half seg-count-x2) :doc
  "Start character code for each segment.")
 (field id-delta (array i16) :count (half seg-count-x2) :doc
  "Delta for all character codes in segment.")
 (field id-range-offsets (array u16) :count (half seg-count-x2) :doc
  "Offsets into glyphIdArray or 0")
 (field glyph-id-array (array u16) :count :remainder :doc
  "Glyph index array (arbitrary length)"))


(table cmap6
 (:doc
  "[cmap Format 6](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-6-trimmed-table-mapping): Trimmed table mapping")
 (field format-table u16 :format 6 :doc "Format number is set to 6.")
 (field length u16 :doc "This is the length in bytes of the subtable.")
 (field language u16 :doc
  "For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.")
 (field first-code u16 :doc "First character code of subrange.")
 (field entry-count u16 :doc "Number of character codes in subrange.")
 (field glyph-id-array (array u16) :count entry-count :doc
  "Array of glyph index values for character codes in the range."))


(table cmap8
 (:doc
  "[cmap Format 8](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-8-mixed-16-bit-and-32-bit-coverage): mixed 16-bit and 32-bit coverage")
 (field format-table u16 :format 8 :doc "Subtable format; set to 8.")
 (field reserved u16 :doc "Reserved; set to 0")
 (field length u32 :doc "Byte length of this subtable (including the header)")
 (field language u32 :doc
  "For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.")
 (field is32 (array u8) :count 8192 :doc
  "Tightly packed array of bits (8K bytes total) indicating whether the particular 16-bit (index) value is the start of a 32-bit character code")
 (field num-groups u32 :doc "Number of groupings which follow")
 (field groups (array sequential-map-group) :count num-groups :doc
  "Array of SequentialMapGroup records."))


(record sequential-map-group (:doc "Used in [Cmap8] and [Cmap12]")
 (field start-char-code u32 :doc
  "First character code in this group; note that if this group is for one or more 16-bit character codes (which is determined from the is32 array), this 32-bit value will have the high 16-bits set to zero")
 (field end-char-code u32 :doc
  "Last character code in this group; same condition as listed above for the startCharCode")
 (field start-glyph-id u32 :doc
  "Glyph index corresponding to the starting character code"))


(table cmap10
 (:doc
  "[cmap Format 10](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-10-trimmed-array): Tr")
 (field format-table u16 :format 10 :doc "Subtable format; set to 10.")
 (field reserved u16 :doc "Reserved; set to 0")
 (field length u32 :doc "Byte length of this subtable (including the header)")
 (field language u32 :doc
  "For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.")
 (field start-char-code u32 :doc "First character code covered")
 (field num-chars u32 :doc "Number of character codes covered")
 (field glyph-id-array (array u16) :count num-chars :doc
  "Array of glyph indices for the character codes covered"))


(table cmap12
 (:doc
  "[cmap Format 12](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-12-segmented-coverage): Segmented coverage")
 (field format-table u16 :format 12 :doc "Subtable format; set to 12.")
 (field reserved u16 :doc "Reserved; set to 0")
 (field length u32 :doc "Byte length of this subtable (including the header)")
 (field language u32 :doc
  "For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.")
 (field num-groups u32 :doc "Number of groupings which follow")
 (field groups (array sequential-map-group) :count num-groups :doc
  "Array of SequentialMapGroup records."))


(table cmap13
 (:doc
  "[cmap Format 13](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-13-many-to-one-range-mappings): Many-to-one range mappings")
 (field format-table u16 :format 13 :doc "Subtable format; set to 13.")
 (field reserved u16 :doc "Reserved; set to 0")
 (field length u32 :doc "Byte length of this subtable (including the header)")
 (field language u32 :doc
  "For requirements on use of the language field, see “Use of the language field in 'cmap' subtables” in this document.")
 (field num-groups u32 :doc "Number of groupings which follow")
 (field groups (array constant-map-group) :count num-groups :doc
  "Array of ConstantMapGroup records."))


(record constant-map-group (:doc "Part of [Cmap13]")
 (field start-char-code u32 :doc "First character code in this group")
 (field end-char-code u32 :doc "Last character code in this group")
 (field glyph-id u32 :doc
  "Glyph index to be used for all the characters in the group’s range."))


(table cmap14
 (:doc
  "[cmap Format 14](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-14-unicode-variation-sequences): Unicode Variation Sequences")
 (field format-table u16 :format 14 :doc "Subtable format. Set to 14.")
 (field length u32 :doc "Byte length of this subtable (including this header)")
 (field num-var-selector-records u32 :doc
  "Number of variation Selector Records")
 (field var-selector (array variation-selector) :count num-var-selector-records
  :doc "Array of VariationSelector records."))


(table cmap15
 (:doc
  "Extended glyph repertoire UVS mapping (ISO/IEC 14496-22:2026, 5.1.2.5.11).")
 (field format-table u16 :format 15 :doc "Subtable format. Set to 15.")
 (field length u32 :doc
  "Byte length of this subtable (including this header).")
 (field num-var-selector-records u32 :doc
  "Number of variation selector records.")
 (field var-selector (array variation-selector15) :count
  num-var-selector-records))


(record variation-selector15 (:doc "Part of [Cmap15].")
 (field var-selector uint24) (field default-uvs-offset (offset32 default-uvs))
 (field non-default-uvs-offset (offset32 non-default-uvs24)))


(record variation-selector (:doc "Part of [Cmap14]")
 (field var-selector uint24 :doc "Variation selector")
 (field default-uvs-offset (offset32 default-uvs) :doc
  "Offset from the start of the [`Cmap14`] subtable to Default UVS Table. May be NULL.")
 (field non-default-uvs-offset (offset32 non-default-uvs) :doc
  "Offset from the start of the [`Cmap14`] subtable to Non-Default UVS Table. May be NULL."))


(table default-uvs
 (:doc
  "[Default UVS table](https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#default-uvs-table)")
 (field num-unicode-value-ranges u32 :doc
  "Number of Unicode character ranges.")
 (field ranges (array unicode-range) :count num-unicode-value-ranges :doc
  "Array of UnicodeRange records."))


(table non-default-uvs
 (:doc
  "[Non-Default UVS table](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#non-default-uvs-table)")
 (field num-uvs-mappings u32)
 (field uvs-mapping (array uvs-mapping) :count num-uvs-mappings))


(table non-default-uvs24
 (:doc "Non-default UVS mappings with 24-bit glyph IDs, used by [Cmap15].")
 (field num-uvs-mappings u32)
 (field uvs-mapping (array uvs-mapping24) :count num-uvs-mappings))


(record uvs-mapping24
 (:doc "UVSMapping24 record (ISO/IEC 14496-22:2026, 5.1.2.5.11).")
 (field unicode-value uint24) (field glyph-id uint24))


(record uvs-mapping (:doc "Part of [Cmap14]")
 (field unicode-value uint24 :doc "Base Unicode value of the UVS")
 (field glyph-id u16 :doc "Glyph ID of the UVS"))


(record unicode-range (:doc "Part of [Cmap14]")
 (field start-unicode-value uint24 :doc "First value in this range")
 (field additional-count u8 :doc "Number of additional values in this range"))

