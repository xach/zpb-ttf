;;;; gvar.sexp — OpenType table schema
;;;; Generated from Fontations gvar.rs spec


(table gvar (:tag "gvar")
 (:doc
  "The ['gvar' header](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar#gvar-header)")
 (field version major-minor :doc
  "Major/minor version number of the glyph variations table — set to (1,0).")
 (field axis-count u16 :doc
  "The number of variation axes for this font. This must be the same number as axisCount in the 'fvar' table.")
 (field shared-tuple-count u16 :doc
  "The number of shared tuple records. Shared tuple records can be referenced within glyph variation data tables for multiple glyphs, as opposed to other tuple records stored directly within a glyph variation data table.")
 (field shared-tuples-offset (offset32 shared-tuples) :doc
  "Offset from the start of this table to the shared tuple records.")
 (field glyph-count u16 :doc
  "The number of glyphs in this font. This must match the number of glyphs stored elsewhere in the font.")
 (field flags gvar-flags :doc
  "Bit-field that gives the format of the offset array that follows. If bit 0 is clear, the offsets are uint16; if bit 0 is set, the offsets are uint32.")
 (field glyph-variation-data-array-offset u32 :doc
  "Offset from the start of this table to the array of GlyphVariationData tables.")
 (field glyph-variation-data-offsets computed-array<u16-or32> :doc
  "Offsets from the start of the GlyphVariationData array to each GlyphVariationData table."))


(table gvar-extended (:tag "GVAR")
 (:doc "GVAR glyph variations table (ISO/IEC 14496-22:2026, 7.3.9).")
 (field version major-minor) (field axis-count u16)
 (field shared-tuple-count u16)
 (field shared-tuples-offset (offset32 shared-tuples))
 (field glyph-count uint24 :doc
  "Number of glyphs, equal to numGlyphs in MAXP.")
 (field flags gvar-flags) (field glyph-variation-data-array-offset u32)
 (field glyph-variation-data-offsets computed-array<u16-or32>))


(flags u16 gvar-flags (long-offsets 1))


(table shared-tuples
 (:doc "Array of tuple records shared across all glyph variation data tables.")
 (field tuples computed-array<tuple<a>>))


(table glyph-variation-data-header
 (:doc
  "The [GlyphVariationData](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar#the-glyphvariationdata-table-array) table")
 (field tuple-variation-count tuple-variation-count :doc
  "A packed field. The high 4 bits are flags, and the low 12 bits are the number of tuple variation tables for this glyph. The number of tuple variation tables can be any number between 1 and 4095.")
 (field serialized-data-offset (offset16 font-data) :doc
  "Offset from the start of the GlyphVariationData table to the serialized data")
 (field tuple-variation-headers var-len-array<tuple-variation-header<->> :doc
  "Array of tuple variation headers."))


(table glyph-variation-data-header-extended
 (:doc
  "Per-glyph variation header in GVAR (ISO/IEC 14496-22:2026, 7.3.9.1.3).")
 (field tuple-variation-count tuple-variation-count)
 (field serialized-data-offset (offset24 font-data))
 (field tuple-variation-headers var-len-array<tuple-variation-header<->>))

