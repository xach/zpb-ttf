;;;; layout.sexp — OpenType table schema
;;;; Generated from Fontations layout.rs spec


(table script-list
 (:doc
  "[Script List Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#script-list-table-and-script-record)")
 (field script-count u16 :doc "Number of ScriptRecords")
 (field script-records (array script-record) :count script-count :doc
  "Array of ScriptRecords, listed alphabetically by script tag"))


(record script-record
 (:doc
  "[Script Record](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#script-list-table-and-script-record)")
 (field script-tag tag :doc "4-byte script tag identifier")
 (field script-offset (offset16 script) :doc
  "Offset to Script table, from beginning of ScriptList"))


(table script
 (:doc
  "[Script Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#script-table-and-language-system-record)")
 (field default-lang-sys-offset (offset16 lang-sys) :doc
  "Offset to default LangSys table, from beginning of Script table — may be NULL")
 (field lang-sys-count u16 :doc
  "Number of LangSysRecords for this script — excluding the default LangSys")
 (field lang-sys-records (array lang-sys-record) :count lang-sys-count :doc
  "Array of LangSysRecords, listed alphabetically by LangSys tag"))


(record lang-sys-record
 (field lang-sys-tag tag :doc "4-byte LangSysTag identifier")
 (field lang-sys-offset (offset16 lang-sys) :doc
  "Offset to LangSys table, from beginning of Script table"))


(table lang-sys
 (:doc
  "[Language System Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#language-system-table)")
 (field lookup-order-offset u16 :doc
  "= NULL (reserved for an offset to a reordering table)")
 (field required-feature-index u16 :doc
  "Index of a feature required for this language system; if no required features = 0xFFFF")
 (field feature-index-count u16 :doc
  "Number of feature index values for this language system — excludes the required feature")
 (field feature-indices (array u16) :count feature-index-count :doc
  "Array of indices into the FeatureList, in arbitrary order"))


(table feature-list
 (:doc
  "[Feature List Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#feature-list-table)")
 (field feature-count u16 :doc "Number of FeatureRecords in this table")
 (field feature-records (array feature-record) :count feature-count :doc
  "Array of FeatureRecords — zero-based (first feature has FeatureIndex = 0), listed alphabetically by feature tag"))


(record feature-record (:doc "Part of [FeatureList]")
 (field feature-tag tag :doc "4-byte feature identification tag")
 (field feature-offset (offset16 feature) :doc
  "Offset to Feature table, from beginning of FeatureList"))


(table feature
 (:doc
  "[Feature Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#feature-table)")
 (field feature-params-offset (offset16 feature-params) :doc
  "Offset from start of Feature table to FeatureParams table, if defined for the feature and present, else NULL")
 (field lookup-index-count u16 :doc
  "Number of LookupList indices for this feature")
 (field lookup-list-indices (array u16) :count lookup-index-count :doc
  "Array of indices into the LookupList — zero-based (first lookup is LookupListIndex = 0)"))


(table lookup-list
 (:doc
  "[Lookup List Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookup-list-table)")
 (field lookup-count u16 :doc "Number of lookups in this table")
 (field lookup-offsets (array (offset16 t)) :count lookup-count :doc
  "Array of offsets to Lookup tables, from beginning of LookupList — zero based (first lookup is Lookup index = 0)"))


(table lookup
 (:doc
  "[Lookup Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookup-table)")
 (field lookup-type u16 :doc "Different enumerations for GSUB and GPOS")
 (field lookup-flag lookup-flag :doc "Lookup qualifiers")
 (field sub-table-count u16 :doc "Number of subtables for this lookup")
 (field subtable-offsets (array (offset16 t)) :count sub-table-count :doc
  "Array of offsets to lookup subtables, from beginning of Lookup table")
 (field mark-filtering-set u16 :when-flag (lookup-flag use-mark-filtering-set) :doc
  "Index (base 0) into GDEF mark glyph sets structure. This field is only present if the USE_MARK_FILTERING_SET lookup flag is set."))


(table coverage-format1
 (:doc
  "[Coverage Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#coverage-format-1)")
 (field coverage-format u16 :format 1 :doc "Format identifier — format = 1")
 (field glyph-count u16 :doc "Number of glyphs in the glyph array")
 (field glyph-array (array glyph-id16) :count glyph-count :doc
  "Array of glyph IDs — in numerical order"))


(table coverage-format2
 (:doc
  "[Coverage Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#coverage-format-2)")
 (field coverage-format u16 :format 2 :doc "Format identifier — format = 2")
 (field range-count u16 :doc "Number of RangeRecords")
 (field range-records (array range-record) :count range-count :doc
  "Array of glyph ranges — ordered by startGlyphID."))


(record range-record (:doc "Used in [CoverageFormat2]")
 (field start-glyph-id glyph-id16 :doc "First glyph ID in the range")
 (field end-glyph-id glyph-id16 :doc "Last glyph ID in the range")
 (field start-coverage-index u16 :doc
  "Coverage Index of first glyph ID in range"))


(format-table u16 coverage-table
 (:doc
  "[Coverage Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#coverage-table)")
 (variant format1 coverage-format1) (variant format2 coverage-format2)
 (variant format3 coverage-format3) (variant format4 coverage-format4))


(table class-def-format1
 (:doc
  "[Class Definition Table Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#class-definition-table-format-1)")
 (field class-format u16 :format 1 :doc "Format identifier — format = 1")
 (field start-glyph-id glyph-id16 :doc "First glyph ID of the classValueArray")
 (field glyph-count u16 :doc "Size of the classValueArray")
 (field class-value-array (array u16) :count glyph-count :doc
  "Array of Class Values — one per glyph ID"))


(table class-def-format2
 (:doc
  "[Class Definition Table Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#class-definition-table-format-2)")
 (field class-format u16 :format 2 :doc "Format identifier — format = 2")
 (field class-range-count u16 :doc "Number of ClassRangeRecords")
 (field class-range-records (array class-range-record) :count class-range-count
  :doc "Array of ClassRangeRecords — ordered by startGlyphID"))


(record class-range-record (:doc "Used in [ClassDefFormat2]")
 (field start-glyph-id glyph-id16 :doc "First glyph ID in the range")
 (field end-glyph-id glyph-id16 :doc "Last glyph ID in the range")
 (field class u16 :doc "Applied to all glyphs in the range"))


(format-table u16 class-def
 (:doc
  "A [Class Definition Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#class-definition-table)")
 (variant format1 class-def-format1) (variant format2 class-def-format2)
 (variant format3 class-def-format3) (variant format4 class-def-format4))


(record sequence-lookup-record
 (:doc
  "[Sequence Lookup Record](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#sequence-lookup-record)")
 (field sequence-index u16 :doc
  "Index (zero-based) into the input glyph sequence")
 (field lookup-list-index u16 :doc "Index (zero-based) into the LookupList"))


(table sequence-context-format1
 (:doc
  "[Sequence Context Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#sequence-context-format-1-simple-glyph-contexts)")
 (field format-table u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of SequenceContextFormat1 table")
 (field seq-rule-set-count u16 :doc "Number of SequenceRuleSet tables")
 (field seq-rule-set-offsets (array (offset16 sequence-rule-set)) :count
  seq-rule-set-count :doc
  "Array of offsets to SequenceRuleSet tables, from beginning of SequenceContextFormat1 table (offsets may be NULL)"))


(table sequence-rule-set (:doc "Part of [SequenceContextFormat1]")
 (field seq-rule-count u16 :doc "Number of SequenceRule tables")
 (field seq-rule-offsets (array (offset16 sequence-rule)) :count seq-rule-count
  :doc
  "Array of offsets to SequenceRule tables, from beginning of the SequenceRuleSet table"))


(table sequence-rule (:doc "Part of [SequenceContextFormat1]")
 (field glyph-count u16 :doc "Number of glyphs in the input glyph sequence")
 (field seq-lookup-count u16 :doc "Number of SequenceLookupRecords")
 (field input-sequence (array glyph-id16) :count (subtract glyph-count 1) :doc
  "Array of input glyph IDs—starting with the second glyph")
 (field seq-lookup-records (array sequence-lookup-record) :count
  seq-lookup-count :doc "Array of Sequence lookup records"))


(table sequence-context-format2
 (:doc
  "[Sequence Context Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#sequence-context-format-2-class-based-glyph-contexts)")
 (field format-table u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of SequenceContextFormat2 table")
 (field class-def-offset (offset16 class-def) :doc
  "Offset to ClassDef table, from beginning of SequenceContextFormat2 table")
 (field class-seq-rule-set-count u16 :doc
  "Number of ClassSequenceRuleSet tables")
 (field class-seq-rule-set-offsets (array (offset16 class-sequence-rule-set))
  :count class-seq-rule-set-count :doc
  "Array of offsets to ClassSequenceRuleSet tables, from beginning of SequenceContextFormat2 table (may be NULL)"))


(table class-sequence-rule-set (:doc "Part of [SequenceContextFormat2]")
 (field class-seq-rule-count u16 :doc "Number of ClassSequenceRule tables")
 (field class-seq-rule-offsets (array (offset16 class-sequence-rule)) :count
  class-seq-rule-count :doc
  "Array of offsets to ClassSequenceRule tables, from beginning of ClassSequenceRuleSet table"))


(table class-sequence-rule (:doc "Part of [SequenceContextFormat2]")
 (field glyph-count u16 :doc "Number of glyphs to be matched")
 (field seq-lookup-count u16 :doc "Number of SequenceLookupRecords")
 (field input-sequence (array u16) :count (subtract glyph-count 1) :doc
  "Sequence of classes to be matched to the input glyph sequence, beginning with the second glyph position")
 (field seq-lookup-records (array sequence-lookup-record) :count
  seq-lookup-count :doc "Array of SequenceLookupRecords"))


(table sequence-context-format3
 (:doc
  "[Sequence Context Format 3](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#sequence-context-format-3-coverage-based-glyph-contexts)")
 (field format-table u16 :format 3 :doc "Format identifier: format = 3")
 (field glyph-count u16 :doc "Number of glyphs in the input sequence")
 (field seq-lookup-count u16 :doc "Number of SequenceLookupRecords")
 (field coverage-offsets (array (offset16 coverage-table)) :count glyph-count
  :doc
  "Array of offsets to Coverage tables, from beginning of SequenceContextFormat3 subtable")
 (field seq-lookup-records (array sequence-lookup-record) :count
  seq-lookup-count :doc "Array of SequenceLookupRecords"))


(format-table u16 sequence-context (variant format1 sequence-context-format1)
 (variant format2 sequence-context-format2)
 (variant format3 sequence-context-format3)
 (variant format4 sequence-context-format4)
 (variant format5 sequence-context-format5)
 (variant format6 sequence-context-format6))


(table chained-sequence-context-format1
 (:doc
  "[Chained Sequence Context Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#chained-sequence-context-format-1-simple-glyph-contexts)")
 (field format-table u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of ChainSequenceContextFormat1 table")
 (field chained-seq-rule-set-count u16 :doc
  "Number of ChainedSequenceRuleSet tables")
 (field chained-seq-rule-set-offsets
  (array (offset16 chained-sequence-rule-set)) :count
  chained-seq-rule-set-count :doc
  "Array of offsets to ChainedSeqRuleSet tables, from beginning of ChainedSequenceContextFormat1 table (may be NULL)"))


(table chained-sequence-rule-set
 (:doc "Part of [ChainedSequenceContextFormat1]")
 (field chained-seq-rule-count u16 :doc "Number of ChainedSequenceRule tables")
 (field chained-seq-rule-offsets (array (offset16 chained-sequence-rule))
  :count chained-seq-rule-count :doc
  "Array of offsets to ChainedSequenceRule tables, from beginning of ChainedSequenceRuleSet table"))


(table chained-sequence-rule (:doc "Part of [ChainedSequenceContextFormat1]")
 (field backtrack-glyph-count u16 :doc
  "Number of glyphs in the backtrack sequence")
 (field backtrack-sequence (array glyph-id16) :count backtrack-glyph-count :doc
  "Array of backtrack glyph IDs")
 (field input-glyph-count u16 :doc "Number of glyphs in the input sequence")
 (field input-sequence (array glyph-id16) :count (subtract input-glyph-count 1)
  :doc "Array of input glyph IDs—start with second glyph")
 (field lookahead-glyph-count u16 :doc
  "Number of glyphs in the lookahead sequence")
 (field lookahead-sequence (array glyph-id16) :count lookahead-glyph-count :doc
  "Array of lookahead glyph IDs")
 (field seq-lookup-count u16 :doc "Number of SequenceLookupRecords")
 (field seq-lookup-records (array sequence-lookup-record) :count
  seq-lookup-count :doc "Array of SequenceLookupRecords"))


(table chained-sequence-context-format2
 (:doc
  "[Chained Sequence Context Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#chained-sequence-context-format-2-class-based-glyph-contexts)")
 (field format-table u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of ChainedSequenceContextFormat2 table")
 (field backtrack-class-def-offset (offset16 class-def) :doc
  "Offset to ClassDef table containing backtrack sequence context, from beginning of ChainedSequenceContextFormat2 table")
 (field input-class-def-offset (offset16 class-def) :doc
  "Offset to ClassDef table containing input sequence context, from beginning of ChainedSequenceContextFormat2 table")
 (field lookahead-class-def-offset (offset16 class-def) :doc
  "Offset to ClassDef table containing lookahead sequence context, from beginning of ChainedSequenceContextFormat2 table")
 (field chained-class-seq-rule-set-count u16 :doc
  "Number of ChainedClassSequenceRuleSet tables")
 (field chained-class-seq-rule-set-offsets
  (array (offset16 chained-class-sequence-rule-set)) :count
  chained-class-seq-rule-set-count :doc
  "Array of offsets to ChainedClassSequenceRuleSet tables, from beginning of ChainedSequenceContextFormat2 table (may be NULL)"))


(table chained-class-sequence-rule-set
 (:doc "Part of [ChainedSequenceContextFormat2]")
 (field chained-class-seq-rule-count u16 :doc
  "Number of ChainedClassSequenceRule tables")
 (field chained-class-seq-rule-offsets
  (array (offset16 chained-class-sequence-rule)) :count
  chained-class-seq-rule-count :doc
  "Array of offsets to ChainedClassSequenceRule tables, from beginning of ChainedClassSequenceRuleSet"))


(table chained-class-sequence-rule
 (:doc "Part of [ChainedSequenceContextFormat2]")
 (field backtrack-glyph-count u16 :doc
  "Number of glyphs in the backtrack sequence")
 (field backtrack-sequence (array u16) :count backtrack-glyph-count :doc
  "Array of backtrack-sequence classes")
 (field input-glyph-count u16 :doc
  "Total number of glyphs in the input sequence")
 (field input-sequence (array u16) :count (subtract input-glyph-count 1) :doc
  "Array of input sequence classes, beginning with the second glyph position")
 (field lookahead-glyph-count u16 :doc
  "Number of glyphs in the lookahead sequence")
 (field lookahead-sequence (array u16) :count lookahead-glyph-count :doc
  "Array of lookahead-sequence classes")
 (field seq-lookup-count u16 :doc "Number of SequenceLookupRecords")
 (field seq-lookup-records (array sequence-lookup-record) :count
  seq-lookup-count :doc "Array of SequenceLookupRecords"))


(table chained-sequence-context-format3
 (:doc
  "[Chained Sequence Context Format 3](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#chained-sequence-context-format-3-coverage-based-glyph-contexts)")
 (field format-table u16 :format 3 :doc "Format identifier: format = 3")
 (field backtrack-glyph-count u16 :doc
  "Number of glyphs in the backtrack sequence")
 (field backtrack-coverage-offsets (array (offset16 coverage-table)) :count
  backtrack-glyph-count :doc
  "Array of offsets to coverage tables for the backtrack sequence")
 (field input-glyph-count u16 :doc "Number of glyphs in the input sequence")
 (field input-coverage-offsets (array (offset16 coverage-table)) :count
  input-glyph-count :doc
  "Array of offsets to coverage tables for the input sequence")
 (field lookahead-glyph-count u16 :doc
  "Number of glyphs in the lookahead sequence")
 (field lookahead-coverage-offsets (array (offset16 coverage-table)) :count
  lookahead-glyph-count :doc
  "Array of offsets to coverage tables for the lookahead sequence")
 (field seq-lookup-count u16 :doc "Number of SequenceLookupRecords")
 (field seq-lookup-records (array sequence-lookup-record) :count
  seq-lookup-count :doc "Array of SequenceLookupRecords"))


(format-table u16 chained-sequence-context
 (variant format1 chained-sequence-context-format1)
 (variant format2 chained-sequence-context-format2)
 (variant format3 chained-sequence-context-format3)
 (variant format4 chained-sequence-context-format4)
 (variant format5 chained-sequence-context-format5))


(enum u16 delta-format
 (:doc
  "[Device](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#device-and-variationindex-tables) delta formats")
 (local2-bit-deltas 1) (local4-bit-deltas 2) (local8-bit-deltas 3)
 (variation-index 32768))


(table device
 (:doc
  "[Device Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#device-and-variationindex-tables)")
 (field start-size u16 :doc "Smallest size to correct, in ppem")
 (field end-size u16 :doc "Largest size to correct, in ppem")
 (field delta-format delta-format :doc
  "Format of deltaValue array data: 0x0001, 0x0002, or 0x0003")
 (field delta-value (array u16) :count
  (delta_value_count delta-format start-size end-size) :doc
  "Array of compressed data"))


(table variation-index (:doc "Variation index table")
 (field delta-set-outer-index u16 :doc
  "A delta-set outer index — used to select an item variation data subtable within the item variation store.")
 (field delta-set-inner-index u16 :doc
  "A delta-set inner index — used to select a delta-set row within an item variation data subtable.")
 (field delta-format delta-format :doc "Format, = 0x8000"))


(table pending-variation-index
 (:doc
  "A type representing a temporary identifier for a set of variation deltas.  The final indices used in the VariationIndex table are not known until all deltas have been collected. This variant is used to assign a temporary identifier during compilation.  This type is not part of the spec and will never appear in an actual font file. It is intended to serve as a sentinel value, and will panic when written, ensuring that all VariationIndex tables have been correctly mapped before the font is compiled.")
 (field delta-set-id u32 :doc "A unique identifier for a given set of deltas."))


(format-table delta-format device-or-variation-index (:offset 4)
 (:doc
  "Either a [Device] table (in a non-variable font) or a [VariationIndex] table (in a variable font)")
 (variant device device) (variant variation-index variation-index)
 (variant pending-variation-index pending-variation-index))


(table feature-variations
 (:doc
  "[FeatureVariations Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#featurevariations-table) Version 1.1 is defined in ISO/IEC 14496-22:2026, 6.2.11.")
 (field version major-minor)
 (field feature-variation-record-count u32 :doc
  "Number of feature variation records.")
 (field feature-variation-records (array feature-variation-record) :count
  feature-variation-record-count :doc "Array of feature variation records.")
 (field lookup-variation-record-count u32 :since-version "1.1" :doc
  "Number of lookup variation records.")
 (field lookup-variation-records (array lookup-variation-record) :count
  lookup-variation-record-count :since-version "1.1" :doc
  "Lookup variations, sorted by feature index."))


(record feature-variation-record (:doc "Part of [FeatureVariations]")
 (field condition-set-offset (offset32 condition-set) :doc
  "Offset to a condition set table, from beginning of FeatureVariations table.")
 (field feature-table-substitution-offset (offset32 feature-table-substitution)
  :doc
  "Offset to a feature table substitution table, from beginning of the FeatureVariations table."))


(record lookup-variation-record
 (:doc "Lookup variations for one feature (ISO/IEC 14496-22:2026, 6.2.11).")
 (field feature-index u16)
 (field feature-lookups-offset (offset32 feature-lookups) :doc
  "Offset from the beginning of FeatureVariations."))


(table feature-lookups
 (:doc
  "Conditional lookup sets for one feature (ISO/IEC 14496-22:2026, 6.2.11).")
 (field version major-minor) (field flags feature-lookups-flags)
 (field lookup-condition-count u32)
 (field lookup-condition-records (array lookup-condition-record) :count
  lookup-condition-count))


(flags u16 feature-lookups-flags (:doc "Qualifiers for [FeatureLookups].")
 (add-default-lookups 1))


(record lookup-condition-record
 (:doc "Lookups to include when a condition is true.")
 (field condition-offset (offset32 condition) :doc
  "Offset from the beginning of FeatureLookups; null means true.")
 (field lookup-index-list-offset (offset32 lookup-index-list) :doc
  "Offset from the beginning of FeatureLookups."))


(table lookup-index-list (:doc "Indices into the GSUB or GPOS LookupList.")
 (field lookup-index-count u16)
 (field lookup-indices (array u16) :count lookup-index-count))


(table condition-set
 (:doc
  "[ConditionSet Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#conditionset-table)")
 (field condition-count u16 :doc
  "Number of conditions for this condition set.")
 (field condition-offsets (array (offset32 condition)) :count condition-count
  :doc
  "Array of offsets to condition tables, from beginning of the ConditionSet table."))


(format-table u16 condition
 (:doc
  "[Condition Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#condition-table)  Formats 2..5 are defined in ISO/IEC 14496-22:2026, 6.2.10.")
 (variant format1-axis-range condition-format1)
 (variant format2-variable-value condition-format2)
 (variant format3-and condition-format3) (variant format4-or condition-format4)
 (variant format5-negate condition-format5))


(table condition-format1
 (:doc
  "[Condition Table Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#condition-table-format-1-font-variation-axis-range): Font Variation Axis Range")
 (field format-table u16 :format 1 :doc "Format, = 1")
 (field axis-index u16 :doc
  "Index (zero-based) for the variation axis within the 'fvar' table.")
 (field filter-range-min-value f2-dot14 :doc
  "Minimum value of the font variation instances that satisfy this condition.")
 (field filter-range-max-value f2-dot14 :doc
  "Maximum value of the font variation instances that satisfy this condition."))


(table condition-format2
 (:doc
  "[Condition Table Format 2](https://github.com/fonttools/fonttools/blob/5e6b12d12fa08abafbeb7570f47707fbedf69a45/Lib/fontTools/ttLib/tables/otData.py#L3237-L3255): Variation index")
 (field format-table u16 :format 2 :doc "Format, = 2")
 (field default-value i16 :doc "Value at default instance.")
 (field var-index u32 :doc
  "Variation index to vary the value based on current designspace location."))


(table condition-format3
 (:doc
  "[Condition Table Format 3](https://github.com/fonttools/fonttools/blob/5e6b12d12fa08abafbeb7570f47707fbedf69a45/Lib/fontTools/ttLib/tables/otData.py#L3257-L3275): AND")
 (field format-table u16 :format 3 :doc "Format, = 3")
 (field condition-count u8 :doc "Number of conditions.")
 (field condition-offsets (array (offset24 condition)) :count condition-count
  :doc "Array of condition tables for this conjunction (AND) expression."))


(table condition-format4
 (:doc
  "[Condition Table Format 4](https://github.com/fonttools/fonttools/blob/5e6b12d12fa08abafbeb7570f47707fbedf69a45/Lib/fontTools/ttLib/tables/otData.py#L3276-L3295): OR")
 (field format-table u16 :format 4 :doc "Format, = 4")
 (field condition-count u8 :doc "Number of conditions.")
 (field condition-offsets (array (offset24 condition)) :count condition-count
  :doc "Array of condition tables for this disjunction (OR) expression."))


(table condition-format5
 (:doc
  "[Condition Table Format 5](https://github.com/fonttools/fonttools/blob/5e6b12d12fa08abafbeb7570f47707fbedf69a45/Lib/fontTools/ttLib/tables/otData.py#L3296-L3308): NOT")
 (field format-table u16 :format 5 :doc "Format, = 5")
 (field condition-offset (offset24 condition) :doc "Condition to negate."))


(table feature-table-substitution
 (:doc
  "[FeatureTableSubstitution Table](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#featuretablesubstitution-table)")
 (field version major-minor :doc "Major & minor version of the table: (1, 0)")
 (field substitution-count u16 :doc
  "Number of feature table substitution records.")
 (field substitutions (array feature-table-substitution-record) :count
  substitution-count :doc "Array of feature table substitution records."))


(record feature-table-substitution-record
 (:doc "Used in [FeatureTableSubstitution]")
 (field feature-index u16 :doc "The feature table index to match.")
 (field alternate-feature-offset (offset32 feature) :doc
  "Offset to an alternate feature table, from start of the FeatureTableSubstitution table."))


(table size-params
 (field design-size u16 :doc
  "The first value represents the design size in 720/inch units (decipoints).  The design size entry must be non-zero. When there is a design size but no recommended size range, the rest of the array will consist of zeros.")
 (field identifier u16 :doc
  "The second value has no independent meaning, but serves as an identifier that associates fonts in a subfamily.  All fonts which share a Typographic or Font Family name and which differ only by size range shall have the same subfamily value, and no fonts which differ in weight or style shall have the same subfamily value. If this value is zero, the remaining fields in the array will be ignored.")
 (field name-entry u16 :doc
  "The third value enables applications to use a single name for the subfamily identified by the second value.  If the preceding value is non-zero, this value must be set in the range 256 – 32767 (inclusive). It records the value of a field in the 'name' table, which must contain English-language strings encoded in Windows Unicode and Macintosh Roman, and may contain additional strings localized to other scripts and languages. Each of these strings is the name an application should use, in combination with the family name, to represent the subfamily in a menu. Applications will choose the appropriate version based on their selection criteria.")
 (field range-start u16 :doc
  "The fourth and fifth values represent the small end of the recommended usage range (exclusive) and the large end of the recommended usage range (inclusive), stored in 720/inch units (decipoints).  Ranges must not overlap, and should generally be contiguous.")
 (field range-end u16))


(table stylistic-set-params (field version u16)
 (field ui-name-id name-id :doc
  "The 'name' table name ID that specifies a string (or strings, for multiple languages) for a user-interface label for this feature.  The value of uiLabelNameId is expected to be in the font-specific name ID range (256-32767), though that is not a requirement in this Feature Parameters specification. The user-interface label for the feature can be provided in multiple languages. An English string should be included as a fallback. The string should be kept to a minimal length to fit comfortably with different application interfaces."))


(table character-variant-params
 (:doc
  "featureParams for ['cv01'-'cv99'](https://docs.microsoft.com/en-us/typography/opentype/spec/features_ae#cv01-cv99)")
 (field format-table u16 :format 0 :doc "Format number is set to 0.")
 (field feat-ui-label-name-id name-id :doc
  "The 'name' table name ID that specifies a string (or strings, for multiple languages) for a user-interface label for this feature. (May be NULL.)")
 (field feat-ui-tooltip-text-name-id name-id :doc
  "The 'name' table name ID that specifies a string (or strings, for multiple languages) that an application can use for tooltip text for this feature. (May be NULL.)")
 (field sample-text-name-id name-id :doc
  "The 'name' table name ID that specifies sample text that illustrates the effect of this feature. (May be NULL.)")
 (field num-named-parameters u16 :doc
  "Number of named parameters. (May be zero.)")
 (field first-param-ui-label-name-id name-id :doc
  "The first 'name' table name ID used to specify strings for user-interface labels for the feature parameters. (Must be zero if numParameters is zero.)")
 (field char-count u16 :doc
  "The count of characters for which this feature provides glyph variants. (May be zero.)")
 (field character-table (array uint24) :count char-count :doc
  "The Unicode Scalar Value of the characters for which this feature provides glyph variants."))


(table coverage-format3 (:doc "CoverageFormat3 (ISO/IEC 14496-22:2026).")
 (field coverage-format u16 :format 3 :doc "Format identifier — format = 3")
 (field glyph-count uint24 :doc "Number of glyphs in the glyph array")
 (field glyph-array (array glyph-id24) :count glyph-count :doc
  "Array of glyph IDs — in numerical order"))


(table coverage-format4 (:doc "CoverageFormat4 (ISO/IEC 14496-22:2026).")
 (field coverage-format u16 :format 4 :doc "Format identifier — format = 4")
 (field range-count uint24 :doc "Number of RangeRecords")
 (field range-records (array range-record2) :count range-count :doc
  "Array of glyph ranges — ordered by startGlyphID."))


(record range-record2 (:doc "RangeRecord2 (ISO/IEC 14496-22:2026).")
 (field start-glyph-id glyph-id24 :doc "First glyph ID in the range")
 (field end-glyph-id glyph-id24 :doc "Last glyph ID in the range")
 (field start-coverage-index uint24 :doc
  "Coverage Index of first glyph ID in range"))


(table class-def-format3 (:doc "ClassDefFormat3 (ISO/IEC 14496-22:2026).")
 (field class-format u16 :format 3 :doc "Format identifier — format = 3")
 (field start-glyph-id glyph-id24 :doc "First glyph ID of the classValueArray")
 (field glyph-count uint24 :doc "Size of the classValueArray")
 (field class-value-array (array uint24) :count glyph-count :doc
  "Array of Class Values — one per glyph ID"))


(table class-def-format4 (:doc "ClassDefFormat4 (ISO/IEC 14496-22:2026).")
 (field class-format u16 :format 4 :doc "Format identifier — format = 4")
 (field class-range-count uint24 :doc "Number of ClassRangeRecords")
 (field class-range-records (array class-range-record2) :count
  class-range-count :doc
  "Array of ClassRangeRecords — ordered by startGlyphID"))


(record class-range-record2 (:doc "ClassRangeRecord2 (ISO/IEC 14496-22:2026).")
 (field start-glyph-id glyph-id24 :doc "First glyph ID in the range")
 (field end-glyph-id glyph-id24 :doc "Last glyph ID in the range")
 (field class u16 :doc "Applied to all glyphs in the range"))


(table sequence-context-format4
 (:doc "SequenceContextFormat4 (ISO/IEC 14496-22:2026).")
 (field format-table u16 :format 4 :doc "Format identifier: format = 4")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of SequenceContextFormat4 table")
 (field seq-rule-set-count uint24 :doc "Number of SequenceRuleSet tables")
 (field seq-rule-set-offsets (array (offset24 sequence-rule-set2)) :count
  seq-rule-set-count :doc
  "Array of offsets to SequenceRuleSet tables, from beginning of SequenceContextFormat4 table (offsets may be NULL)"))


(table sequence-rule-set2 (:doc "SequenceRuleSet2 (ISO/IEC 14496-22:2026).")
 (field seq-rule-count u16 :doc "Number of SequenceRule tables")
 (field seq-rule-offsets (array (offset16 sequence-rule2)) :count
  seq-rule-count :doc
  "Array of offsets to SequenceRule tables, from beginning of the SequenceRuleSet2 table"))


(table sequence-rule2 (:doc "SequenceRule2 (ISO/IEC 14496-22:2026).")
 (field glyph-count u16 :doc "Number of glyphs in the input glyph sequence")
 (field seq-lookup-count u16 :doc "Number of SequenceLookupRecords")
 (field input-sequence (array glyph-id24) :count (subtract glyph-count 1) :doc
  "Array of input glyph IDs—starting with the second glyph")
 (field seq-lookup-records (array sequence-lookup-record) :count
  seq-lookup-count :doc "Array of Sequence lookup records"))


(table sequence-context-format5
 (:doc "SequenceContextFormat5 (ISO/IEC 14496-22:2026).")
 (field format-table u16 :format 5 :doc "Format identifier: format = 5")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of SequenceContextFormat5 table")
 (field class-def-offset (offset32 class-def) :doc
  "Offset to ClassDef table, from beginning of SequenceContextFormat5 table")
 (field class-seq-rule-set-count uint24 :doc
  "Number of ClassSequenceRuleSet tables")
 (field class-seq-rule-set-offsets (array (offset24 class-sequence-rule-set2))
  :count class-seq-rule-set-count :doc
  "Array of offsets to ClassSequenceRuleSet tables, from beginning of SequenceContextFormat5 table (may be NULL)"))


(table class-sequence-rule-set2
 (:doc "ClassSequenceRuleSet2 (ISO/IEC 14496-22:2026).")
 (field class-seq-rule-count u16 :doc "Number of ClassSequenceRule tables")
 (field class-seq-rule-offsets (array (offset24 class-sequence-rule)) :count
  class-seq-rule-count :doc
  "Array of offsets to ClassSequenceRule tables, from beginning of ClassSequenceRuleSet2 table"))


(table sequence-context-format6
 (:doc "SequenceContextFormat6 (ISO/IEC 14496-22:2026).")
 (field format-table u16 :format 6 :doc "Format identifier: format = 6")
 (field glyph-count u16 :doc "Number of glyphs in the input sequence")
 (field seq-lookup-count u16 :doc "Number of SequenceLookupRecords")
 (field coverage-offsets (array (offset24 coverage-table)) :count glyph-count
  :doc
  "Array of offsets to Coverage tables, from beginning of SequenceContextFormat6 subtable")
 (field seq-lookup-records (array sequence-lookup-record) :count
  seq-lookup-count :doc "Array of SequenceLookupRecords"))


(table chained-sequence-context-format4
 (:doc "ChainedSequenceContextFormat4 (ISO/IEC 14496-22:2026).")
 (field format-table u16 :format 4 :doc "Format identifier: format = 4")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of ChainedSequenceContextFormat4 table")
 (field chained-seq-rule-set-count uint24 :doc
  "Number of ChainedSequenceRuleSet tables")
 (field chained-seq-rule-set-offsets
  (array (offset24 chained-sequence-rule-set2)) :count
  chained-seq-rule-set-count :doc
  "Array of offsets to ChainedSeqRuleSet tables, from beginning of ChainedSequenceContextFormat4 table (may be NULL)"))


(table chained-sequence-rule-set2
 (:doc "ChainedSequenceRuleSet2 (ISO/IEC 14496-22:2026).")
 (field chained-seq-rule-count u16 :doc "Number of ChainedSequenceRule tables")
 (field chained-seq-rule-offsets (array (offset24 chained-sequence-rule2))
  :count chained-seq-rule-count :doc
  "Array of offsets to ChainedSequenceRule tables, from beginning of ChainedSequenceRuleSet2 table"))


(table chained-sequence-rule2
 (:doc "ChainedSequenceRule2 (ISO/IEC 14496-22:2026).")
 (field backtrack-glyph-count u16 :doc
  "Number of glyphs in the backtrack sequence")
 (field backtrack-sequence (array glyph-id24) :count backtrack-glyph-count :doc
  "Array of backtrack glyph IDs")
 (field input-glyph-count u16 :doc "Number of glyphs in the input sequence")
 (field input-sequence (array glyph-id24) :count (subtract input-glyph-count 1)
  :doc "Array of input glyph IDs—start with second glyph")
 (field lookahead-glyph-count u16 :doc
  "Number of glyphs in the lookahead sequence")
 (field lookahead-sequence (array glyph-id24) :count lookahead-glyph-count :doc
  "Array of lookahead glyph IDs")
 (field seq-lookup-count u16 :doc "Number of SequenceLookupRecords")
 (field seq-lookup-records (array sequence-lookup-record) :count
  seq-lookup-count :doc "Array of SequenceLookupRecords"))


(table chained-sequence-context-format5
 (:doc "ChainedSequenceContextFormat5 (ISO/IEC 14496-22:2026).")
 (field format-table u16 :format 5 :doc "Format identifier: format = 5")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of ChainedSequenceContextFormat5 table")
 (field backtrack-class-def-offset (offset32 class-def) :doc
  "Offset to ClassDef table containing backtrack sequence context, from beginning of ChainedSequenceContextFormat5 table")
 (field input-class-def-offset (offset32 class-def) :doc
  "Offset to ClassDef table containing input sequence context, from beginning of ChainedSequenceContextFormat5 table")
 (field lookahead-class-def-offset (offset32 class-def) :doc
  "Offset to ClassDef table containing lookahead sequence context, from beginning of ChainedSequenceContextFormat5 table")
 (field chained-class-seq-rule-set-count u16 :doc
  "Number of ChainedClassSequenceRuleSet tables")
 (field chained-class-seq-rule-set-offsets
  (array (offset24 chained-class-sequence-rule-set2)) :count
  chained-class-seq-rule-set-count :doc
  "Array of offsets to ChainedClassSequenceRuleSet tables, from beginning of ChainedSequenceContextFormat5 table (may be NULL)"))


(table chained-class-sequence-rule-set2
 (:doc "ChainedClassSequenceRuleSet2 (ISO/IEC 14496-22:2026).")
 (field chained-class-seq-rule-count u16 :doc
  "Number of ChainedClassSequenceRule tables")
 (field chained-class-seq-rule-offsets
  (array (offset24 chained-class-sequence-rule)) :count
  chained-class-seq-rule-count :doc
  "Array of offsets to ChainedClassSequenceRule tables, from beginning of ChainedClassSequenceRuleSet2"))


(table lookup-list2 (:doc "LookupList2 (ISO/IEC 14496-22:2026).")
 (field lookup-count u16 :doc "Number of lookups in this table")
 (field lookup-offsets (array (offset32 t)) :count lookup-count :doc
  "Array of offsets to Lookup tables, from beginning of LookupList2 — zero based (first lookup is Lookup index = 0)"))



(flags u16 lookup-flag (:doc "[Lookup flags](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookupFlags).")
 (right-to-left #x0001) (ignore-base-glyphs #x0002) (ignore-ligatures #x0004)
 (ignore-marks #x0008) (use-mark-filtering-set #x0010)
 (mark-attachment-class-filter #xFF00))
