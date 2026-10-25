;;;; gsub.sexp — OpenType table schema
;;;; Generated from Fontations gsub.rs spec


(table gsub (:tag "GSUB")
 (:doc
  "[GSUB](https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#gsub-header)")
 (field version major-minor :doc
  "The major and minor version of the GSUB table, as a tuple (u16, u16)")
 (field script-list-offset (offset16 script-list) :doc
  "Offset to ScriptList table, from beginning of GSUB table")
 (field feature-list-offset (offset16 feature-list) :doc
  "Offset to FeatureList table, from beginning of GSUB table")
 (field lookup-list-offset (offset16 original-substitution-lookup-list) :doc
  "Offset to LookupList table, from beginning of GSUB table")
 (field feature-variations-offset (offset32 feature-variations) :since-version
  "1.1" :doc
  "Offset to FeatureVariations table, from beginning of the GSUB table (may be NULL)")
 (field script-list2-offset (offset32 script-list) :since-version "1.2" :doc
  "32-bit offset to ScriptList, taking precedence when nonzero.")
 (field feature-list2-offset (offset32 feature-list) :since-version "1.2" :doc
  "32-bit offset to FeatureList, taking precedence when nonzero.")
 (field lookup-list2-offset (offset32 substitution-lookup-list2) :since-version
  "1.2" :doc "32-bit offset to LookupList2, taking precedence when nonzero."))


(format-table u16 single-subst
 (:doc
  "LookupType 1: [Single Substitution](https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#lookuptype-1-single-substitution-subtable) Subtable")
 (variant format1 single-subst-format1) (variant format2 single-subst-format2)
 (variant format3 single-subst-format3) (variant format4 single-subst-format4))


(table single-subst-format1
 (:doc
  "[Single Substitution Format 1](https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#11-single-substitution-format-1)")
 (field subst-format u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field delta-glyph-id i16 :doc
  "Add to original glyph ID to get substitute glyph ID"))


(table single-subst-format2
 (:doc
  "[Single Substitution Format 2](https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#12-single-substitution-format-2)")
 (field subst-format u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field glyph-count u16 :doc
  "Number of glyph IDs in the substituteGlyphIDs array")
 (field substitute-glyph-ids (array glyph-id16) :count glyph-count :doc
  "Array of substitute glyph IDs — ordered by Coverage index"))


(table multiple-subst-format1
 (:doc
  "[Multiple Substitution Format 1](https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#21-multiple-substitution-format-1)")
 (field subst-format u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field sequence-count u16 :doc
  "Number of Sequence table offsets in the sequenceOffsets array")
 (field sequence-offsets (array (offset16 sequence-table)) :count
  sequence-count :doc
  "Array of offsets to Sequence tables. Offsets are from beginning of substitution subtable, ordered by Coverage index"))


(table sequence-table (:doc "Part of [MultipleSubstFormat1]")
 (field glyph-count u16 :doc
  "Number of glyph IDs in the substituteGlyphIDs array. This must always be greater than 0.")
 (field substitute-glyph-ids (array glyph-id16) :count glyph-count :doc
  "String of glyph IDs to substitute"))


(table alternate-subst-format1
 (:doc
  "[Alternate Substitution Format 1](https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#31-alternate-substitution-format-1)")
 (field subst-format u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field alternate-set-count u16 :doc "Number of AlternateSet tables")
 (field alternate-set-offsets (array (offset16 alternate-set)) :count
  alternate-set-count :doc
  "Array of offsets to AlternateSet tables. Offsets are from beginning of substitution subtable, ordered by Coverage index"))


(table alternate-set (:doc "Part of [AlternateSubstFormat1]")
 (field glyph-count u16 :doc
  "Number of glyph IDs in the alternateGlyphIDs array")
 (field alternate-glyph-ids (array glyph-id16) :count glyph-count :doc
  "Array of alternate glyph IDs, in arbitrary order"))


(table ligature-subst-format1
 (:doc
  "[Ligature Substitution Format 1](https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#41-ligature-substitution-format-1)")
 (field subst-format u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field ligature-set-count u16 :doc "Number of LigatureSet tables")
 (field ligature-set-offsets (array (offset16 ligature-set)) :count
  ligature-set-count :doc
  "Array of offsets to LigatureSet tables. Offsets are from beginning of substitution subtable, ordered by Coverage index"))


(table ligature-set (:doc "Part of [LigatureSubstFormat1]")
 (field ligature-count u16 :doc "Number of Ligature tables")
 (field ligature-offsets (array (offset16 ligature)) :count ligature-count :doc
  "Array of offsets to Ligature tables. Offsets are from beginning of LigatureSet table, ordered by preference."))


(table ligature (:doc "Part of [LigatureSubstFormat1]")
 (field ligature-glyph glyph-id16 :doc "glyph ID of ligature to substitute")
 (field component-count u16 :doc "Number of components in the ligature")
 (field component-glyph-ids (array glyph-id16) :count
  (subtract component-count 1) :doc
  "Array of component glyph IDs — start with the second component, ordered in writing direction"))


(table extension-subst-format1
 (:doc
  "[Extension Substitution Subtable Format 1](https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#71-extension-substitution-subtable-format-1)")
 (field subst-format u16 :format 1 :doc "Format identifier. Set to 1.")
 (field extension-lookup-type u16 :doc
  "Lookup type of subtable referenced by extensionOffset (that is, the extension subtable).")
 (field extension-offset (offset32 t) :doc
  "Offset to the extension subtable, of lookup type extensionLookupType, relative to the start of the ExtensionSubstFormat1 subtable."))


(table reverse-chain-single-subst-format1
 (:doc
  "[Reverse Chaining Contextual Single Substitution Format 1](https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#81-reverse-chaining-contextual-single-substitution-format-1-coverage-based-glyph-contexts)")
 (field subst-format u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable.")
 (field backtrack-glyph-count u16 :doc
  "Number of glyphs in the backtrack sequence.")
 (field backtrack-coverage-offsets (array (offset16 coverage-table)) :count
  backtrack-glyph-count :doc
  "Array of offsets to coverage tables in backtrack sequence, in glyph sequence order.")
 (field lookahead-glyph-count u16 :doc
  "Number of glyphs in lookahead sequence.")
 (field lookahead-coverage-offsets (array (offset16 coverage-table)) :count
  lookahead-glyph-count :doc
  "Array of offsets to coverage tables in lookahead sequence, in glyph sequence order.")
 (field glyph-count u16 :doc
  "Number of glyph IDs in the substituteGlyphIDs array.")
 (field substitute-glyph-ids (array glyph-id16) :count glyph-count :doc
  "Array of substitute glyph IDs — ordered by Coverage index."))


(table single-subst-format3
 (:doc "SingleSubstFormat3 (ISO/IEC 14496-22:2026).")
 (field subst-format u16 :format 3 :doc "Format identifier: format = 3")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field delta-glyph-id int24 :doc
  "Add to original glyph ID to get substitute glyph ID"))


(table single-subst-format4
 (:doc "SingleSubstFormat4 (ISO/IEC 14496-22:2026).")
 (field subst-format u16 :format 4 :doc "Format identifier: format = 4")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field glyph-count uint24 :doc
  "Number of glyph IDs in the substituteGlyphIDs array")
 (field substitute-glyph-ids (array glyph-id24) :count glyph-count :doc
  "Array of substitute glyph IDs — ordered by Coverage index"))


(table multiple-subst-format2
 (:doc "MultipleSubstFormat2 (ISO/IEC 14496-22:2026).")
 (field subst-format u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field sequence-count uint24 :doc
  "Number of Sequence table offsets in the sequenceOffsets array")
 (field sequence-offsets (array (offset24 sequence2)) :count sequence-count
  :doc
  "Array of offsets to Sequence tables. Offsets are from beginning of substitution subtable, ordered by Coverage index"))


(table alternate-subst-format2
 (:doc "AlternateSubstFormat2 (ISO/IEC 14496-22:2026).")
 (field subst-format u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field alternate-set-count uint24 :doc "Number of AlternateSet tables")
 (field alternate-set-offsets (array (offset24 alternate-set2)) :count
  alternate-set-count :doc
  "Array of offsets to AlternateSet tables. Offsets are from beginning of substitution subtable, ordered by Coverage index"))


(table ligature-subst-format2
 (:doc "LigatureSubstFormat2 (ISO/IEC 14496-22:2026).")
 (field subst-format u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable")
 (field ligature-set-count uint24 :doc "Number of LigatureSet tables")
 (field ligature-set-offsets (array (offset24 ligature-set2)) :count
  ligature-set-count :doc
  "Array of offsets to LigatureSet tables. Offsets are from beginning of substitution subtable, ordered by Coverage index"))


(table reverse-chain-single-subst-format2
 (:doc "ReverseChainSingleSubstFormat2 (ISO/IEC 14496-22:2026).")
 (field subst-format u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of substitution subtable.")
 (field backtrack-glyph-count u16 :doc
  "Number of glyphs in the backtrack sequence.")
 (field backtrack-coverage-offsets (array (offset24 coverage-table)) :count
  backtrack-glyph-count :doc
  "Array of offsets to coverage tables in backtrack sequence, in glyph sequence order.")
 (field lookahead-glyph-count u16 :doc
  "Number of glyphs in lookahead sequence.")
 (field lookahead-coverage-offsets (array (offset24 coverage-table)) :count
  lookahead-glyph-count :doc
  "Array of offsets to coverage tables in lookahead sequence, in glyph sequence order.")
 (field glyph-count uint24 :doc
  "Number of glyph IDs in the substituteGlyphIDs array.")
 (field substitute-glyph-ids (array glyph-id24) :count glyph-count :doc
  "Array of substitute glyph IDs — ordered by Coverage index."))


(table sequence2 (:doc "Sequence2 (ISO/IEC 14496-22:2026).")
 (field glyph-count u16 :doc
  "Number of glyph IDs in the substituteGlyphIDs array. This must always be greater than 0.")
 (field substitute-glyph-ids (array glyph-id24) :count glyph-count :doc
  "String of glyph IDs to substitute"))


(table alternate-set2 (:doc "AlternateSet2 (ISO/IEC 14496-22:2026).")
 (field glyph-count u16 :doc
  "Number of glyph IDs in the alternateGlyphIDs array")
 (field alternate-glyph-ids (array glyph-id24) :count glyph-count :doc
  "Array of alternate glyph IDs, in arbitrary order"))


(table ligature-set2 (:doc "LigatureSet2 (ISO/IEC 14496-22:2026).")
 (field ligature-count u16 :doc "Number of Ligature tables")
 (field ligature-offsets (array (offset24 ligature2)) :count ligature-count
  :doc
  "Array of offsets to Ligature tables. Offsets are from beginning of LigatureSet2 table, ordered by preference."))


(table ligature2 (:doc "Ligature2 (ISO/IEC 14496-22:2026).")
 (field ligature-glyph glyph-id24 :doc "glyph ID of ligature to substitute")
 (field component-count u16 :doc "Number of components in the ligature")
 (field component-glyph-ids (array glyph-id24) :count
  (subtract component-count 1) :doc
  "Array of component glyph IDs — start with the second component, ordered in writing direction"))


(format-table u16 multiple-subst
 (:doc "MultipleSubst (ISO/IEC 14496-22:2026).")
 (variant format1 multiple-subst-format1)
 (variant format2 multiple-subst-format2))


(format-table u16 alternate-subst
 (:doc "AlternateSubst (ISO/IEC 14496-22:2026).")
 (variant format1 alternate-subst-format1)
 (variant format2 alternate-subst-format2))


(format-table u16 ligature-subst
 (:doc "LigatureSubst (ISO/IEC 14496-22:2026).")
 (variant format1 ligature-subst-format1)
 (variant format2 ligature-subst-format2))


(format-table u16 reverse-chain-single-subst
 (:doc "ReverseChainSingleSubst (ISO/IEC 14496-22:2026).")
 (variant format1 reverse-chain-single-subst-format1)
 (variant format2 reverse-chain-single-subst-format2))

