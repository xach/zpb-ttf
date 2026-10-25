;;;; gpos.sexp — OpenType table schema
;;;; Generated from Fontations gpos.rs spec

;; ValueRecord size depends on a ValueFormat stored in the parent table;
;; it is decoded in src/extensions.lisp.
(extern gpos-value-record)


(table gpos (:tag "GPOS")
 (:doc
  "[Class Definition Table Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#class-definition-table-format-1) [GPOS Version 1.0](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#gpos-header)")
 (field version major-minor :doc
  "The major and minor version of the GPOS table, as a tuple (u16, u16)")
 (field script-list-offset (offset16 script-list) :doc
  "Offset to ScriptList table, from beginning of GPOS table")
 (field feature-list-offset (offset16 feature-list) :doc
  "Offset to FeatureList table, from beginning of GPOS table")
 (field lookup-list-offset (offset16 original-position-lookup-list) :doc
  "Offset to LookupList table, from beginning of GPOS table")
 (field feature-variations-offset (offset32 feature-variations) :since-version
  "1.1")
 (field script-list2-offset (offset32 script-list) :since-version "1.2" :doc
  "32-bit offset to ScriptList, taking precedence when nonzero.")
 (field feature-list2-offset (offset32 feature-list) :since-version "1.2" :doc
  "32-bit offset to FeatureList, taking precedence when nonzero.")
 (field lookup-list2-offset (offset32 position-lookup-list2) :since-version
  "1.2" :doc "32-bit offset to LookupList2, taking precedence when nonzero."))


(flags u16 value-format (:doc "See [ValueRecord]") (x-placement 1)
 (y-placement 2) (x-advance 4) (y-advance 8) (x-placement-device 16)
 (y-placement-device 32) (x-advance-device 64) (y-advance-device 128))


(format-table u16 anchor-table
 (:doc
  "[Anchor Tables](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#anchor-tables) position one glyph with respect to another.")
 (variant format1 anchor-format1) (variant format2 anchor-format2)
 (variant format3 anchor-format3))


(table anchor-format1
 (:doc
  "[Anchor Table Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#anchor-table-format-1-design-units): Design Units")
 (field anchor-format u16 :format 1 :doc "Format identifier, = 1")
 (field x-coordinate i16 :doc "Horizontal value, in design units")
 (field y-coordinate i16 :doc "Vertical value, in design units"))


(table anchor-format2
 (:doc
  "[Anchor Table Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#anchor-table-format-2-design-units-plus-contour-point): Design Units Plus Contour Point")
 (field anchor-format u16 :format 2 :doc "Format identifier, = 2")
 (field x-coordinate i16 :doc "Horizontal value, in design units")
 (field y-coordinate i16 :doc "Vertical value, in design units")
 (field anchor-point u16 :doc "Index to glyph contour point"))


(table anchor-format3
 (:doc
  "[Anchor Table Format 3](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#anchor-table-format-3-design-units-plus-device-or-variationindex-tables): Design Units Plus Device or VariationIndex Tables")
 (field anchor-format u16 :format 3 :doc "Format identifier, = 3")
 (field x-coordinate i16 :doc "Horizontal value, in design units")
 (field y-coordinate i16 :doc "Vertical value, in design units")
 (field x-device-offset (offset16 device-or-variation-index) :doc
  "Offset to Device table (non-variable font) / VariationIndex table (variable font) for X coordinate, from beginning of Anchor table (may be NULL)")
 (field y-device-offset (offset16 device-or-variation-index) :doc
  "Offset to Device table (non-variable font) / VariationIndex table (variable font) for Y coordinate, from beginning of Anchor table (may be NULL)"))


(table mark-array
 (:doc
  "[Mark Array Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#mark-array-table)")
 (field mark-count u16 :doc "Number of MarkRecords")
 (field mark-records (array mark-record) :count mark-count :doc
  "Array of MarkRecords, ordered by corresponding glyphs in the associated mark Coverage table."))


(record mark-record (:doc "Part of [MarkArray]")
 (field mark-class u16 :doc "Class defined for the associated mark.")
 (field mark-anchor-offset (offset16 anchor-table) :doc
  "Offset to Anchor table, from beginning of MarkArray table."))


(format-table u16 single-pos
 (:doc
  "[Lookup Type 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#lookup-type-1-single-adjustment-positioning-subtable): Single Adjustment Positioning Subtable")
 (variant format1 single-pos-format1) (variant format2 single-pos-format2)
 (variant format3 single-pos-format3) (variant format4 single-pos-format4))


(table single-pos-format1
 (:doc
  "[Single Adjustment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#single-adjustment-positioning-format-1-single-positioning-value): Single Positioning Value")
 (field pos-format u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of SinglePos subtable.")
 (field value-format value-format :doc
  "Defines the types of data in the ValueRecord.")
 (field value-record gpos-value-record :doc
  "Defines positioning value(s) — applied to all glyphs in the Coverage table."))


(table single-pos-format2
 (:doc
  "[Single Adjustment Positioning Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#single-adjustment-positioning-format-2-array-of-positioning-values): Array of Positioning Values")
 (field pos-format u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of SinglePos subtable.")
 (field value-format value-format :doc
  "Defines the types of data in the ValueRecords.")
 (field value-count u16 :doc
  "Number of ValueRecords — must equal glyphCount in the Coverage table.")
 (field value-records computed-array<gpos-value-record<a>> :doc
  "Array of ValueRecords — positioning values applied to glyphs."))


(format-table u16 pair-pos
 (:doc
  "[Lookup Type 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#lookup-type-1-single-adjustment-positioning-subtable): Single Adjustment Positioning Subtable")
 (variant format1 pair-pos-format1) (variant format2 pair-pos-format2)
 (variant format3 pair-pos-format3) (variant format4 pair-pos-format4))


(table pair-pos-format1
 (:doc
  "[Pair Adjustment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#pair-adjustment-positioning-format-1-adjustments-for-glyph-pairs): Adjustments for Glyph Pairs")
 (field pos-format u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of PairPos subtable.")
 (field value-format1 value-format :doc
  "Defines the types of data in valueRecord1 — for the first glyph in the pair (may be zero).")
 (field value-format2 value-format :doc
  "Defines the types of data in valueRecord2 — for the second glyph in the pair (may be zero).")
 (field pair-set-count u16 :doc "Number of PairSet tables")
 (field pair-set-offsets (array (offset16 pair-set)) :count pair-set-count :doc
  "Array of offsets to PairSet tables. Offsets are from beginning of PairPos subtable, ordered by Coverage Index."))


(table pair-set (:doc "Part of [PairPosFormat1]")
 (field pair-value-count u16 :doc "Number of PairValueRecords")
 (field pair-value-records computed-array<pair-value-record<a>> :doc
  "Array of PairValueRecords, ordered by glyph ID of the second glyph."))


(record pair-value-record (:doc "Part of [PairSet]")
 (field second-glyph glyph-id16 :doc
  "Glyph ID of second glyph in the pair (first glyph is listed in the Coverage table).")
 (field value-record1 gpos-value-record :doc
  "Positioning data for the first glyph in the pair.")
 ;; value-record2 follows the variable-size value-record1; see src/extensions.lisp.
 )


(table pair-pos-format2
 (:doc
  "[Pair Adjustment Positioning Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#pair-adjustment-positioning-format-2-class-pair-adjustment): Class Pair Adjustment")
 (field pos-format u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of PairPos subtable.")
 (field value-format1 value-format :doc
  "ValueRecord definition — for the first glyph of the pair (may be zero).")
 (field value-format2 value-format :doc
  "ValueRecord definition — for the second glyph of the pair (may be zero).")
 (field class-def1-offset (offset16 class-def) :doc
  "Offset to ClassDef table, from beginning of PairPos subtable — for the first glyph of the pair.")
 (field class-def2-offset (offset16 class-def) :doc
  "Offset to ClassDef table, from beginning of PairPos subtable — for the second glyph of the pair.")
 (field class1-count u16 :doc
  "Number of classes in classDef1 table — includes Class 0.")
 (field class2-count u16 :doc
  "Number of classes in classDef2 table — includes Class 0.")
 (field class1-records computed-array<class1-record<a>> :doc
  "Array of Class1 records, ordered by classes in classDef1."))


(record class1-record (:doc "Part of [PairPosFormat2]")
 (field class2-records computed-array<class2-record<a>> :doc
  "Array of Class2 records, ordered by classes in classDef2."))


(record class2-record (:doc "Part of [PairPosFormat2]")
 (field value-record1 gpos-value-record :doc
  "Positioning for first glyph — empty if valueFormat1 = 0.")
 ;; value-record2 follows the variable-size value-record1; see src/extensions.lisp.
 )


(table cursive-pos-format1
 (:doc
  "[Cursive Attachment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#cursive-attachment-positioning-format1-cursive-attachment): Cursvie attachment")
 (field pos-format u16 :format 1 :doc "Format identifier: format = 1")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table, from beginning of CursivePos subtable.")
 (field entry-exit-count u16 :doc "Number of EntryExit records")
 (field entry-exit-record (array entry-exit-record) :count entry-exit-count
  :doc "Array of EntryExit records, in Coverage index order."))


(record entry-exit-record (:doc "Part of [CursivePosFormat1]")
 (field entry-anchor-offset (offset16 anchor-table) :doc
  "Offset to entryAnchor table, from beginning of CursivePos subtable (may be NULL).")
 (field exit-anchor-offset (offset16 anchor-table) :doc
  "Offset to exitAnchor table, from beginning of CursivePos subtable (may be NULL)."))


(table mark-base-pos-format1
 (:doc
  "[Mark-to-Base Attachment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#mark-to-base-attachment-positioning-format-1-mark-to-base-attachment-point): Mark-to-base Attachment Point")
 (field pos-format u16 :format 1 :doc "Format identifier: format = 1")
 (field mark-coverage-offset (offset16 coverage-table) :doc
  "Offset to markCoverage table, from beginning of MarkBasePos subtable.")
 (field base-coverage-offset (offset16 coverage-table) :doc
  "Offset to baseCoverage table, from beginning of MarkBasePos subtable.")
 (field mark-class-count u16 :doc "Number of classes defined for marks")
 (field mark-array-offset (offset16 mark-array) :doc
  "Offset to MarkArray table, from beginning of MarkBasePos subtable.")
 (field base-array-offset (offset16 base-array) :doc
  "Offset to BaseArray table, from beginning of MarkBasePos subtable."))


(table base-array (:doc "Part of [MarkBasePosFormat1]")
 (field base-count u16 :doc "Number of BaseRecords")
 (field base-records computed-array<base-record<a>> :doc
  "Array of BaseRecords, in order of baseCoverage Index."))


(record base-record (:doc "Part of [BaseArray]")
 (field base-anchor-offsets (array (offset16 anchor-table)) :count
  mark-class-count :doc
  "Array of offsets (one per mark class) to Anchor tables. Offsets are from beginning of BaseArray table, ordered by class (offsets may be NULL)."))


(table mark-lig-pos-format1
 (:doc
  "[Mark-to-Ligature Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#mark-to-ligature-attachment-positioning-format-1-mark-to-ligature-attachment): Mark-to-Ligature Attachment")
 (field pos-format u16 :format 1 :doc "Format identifier: format = 1")
 (field mark-coverage-offset (offset16 coverage-table) :doc
  "Offset to markCoverage table, from beginning of MarkLigPos subtable.")
 (field ligature-coverage-offset (offset16 coverage-table) :doc
  "Offset to ligatureCoverage table, from beginning of MarkLigPos subtable.")
 (field mark-class-count u16 :doc "Number of defined mark classes")
 (field mark-array-offset (offset16 mark-array) :doc
  "Offset to MarkArray table, from beginning of MarkLigPos subtable.")
 (field ligature-array-offset (offset16 ligature-array) :doc
  "Offset to LigatureArray table, from beginning of MarkLigPos subtable."))


(table ligature-array (:doc "Part of [MarkLigPosFormat1]")
 (field ligature-count u16 :doc "Number of LigatureAttach table offsets")
 (field ligature-attach-offsets (array (offset16 ligature-attach)) :count
  ligature-count :doc
  "Array of offsets to LigatureAttach tables. Offsets are from beginning of LigatureArray table, ordered by ligatureCoverage index."))


(table ligature-attach (:doc "Part of [MarkLigPosFormat1]")
 (field component-count u16 :doc "Number of ComponentRecords in this ligature")
 (field component-records computed-array<component-record<a>> :doc
  "Array of Component records, ordered in writing direction."))


(record component-record (:doc "Part of [MarkLigPosFormat1]")
 (field ligature-anchor-offsets (array (offset16 anchor-table)) :count
  mark-class-count :doc
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of LigatureAttach table, ordered by class (offsets may be NULL)."))


(table mark-mark-pos-format1
 (:doc
  "[Mark-to-Mark Attachment Positioning Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#mark-to-mark-attachment-positioning-format-1-mark-to-mark-attachment): Mark-to-Mark Attachment")
 (field pos-format u16 :format 1 :doc "Format identifier: format = 1")
 (field mark1-coverage-offset (offset16 coverage-table) :doc
  "Offset to Combining Mark Coverage table, from beginning of MarkMarkPos subtable.")
 (field mark2-coverage-offset (offset16 coverage-table) :doc
  "Offset to Base Mark Coverage table, from beginning of MarkMarkPos subtable.")
 (field mark-class-count u16 :doc "Number of Combining Mark classes defined")
 (field mark1-array-offset (offset16 mark-array) :doc
  "Offset to MarkArray table for mark1, from beginning of MarkMarkPos subtable.")
 (field mark2-array-offset (offset16 mark2-array) :doc
  "Offset to Mark2Array table for mark2, from beginning of MarkMarkPos subtable."))


(table mark2-array (:doc "Part of [MarkMarkPosFormat1]Class2Record")
 (field mark2-count u16 :doc "Number of Mark2 records")
 (field mark2-records computed-array<mark2-record<a>> :doc
  "Array of Mark2Records, in Coverage order."))


(record mark2-record (:doc "Part of [MarkMarkPosFormat1]")
 (field mark2-anchor-offsets (array (offset16 anchor-table)) :count
  mark-class-count :doc
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of Mark2Array table, in class order (offsets may be NULL)."))


(table extension-pos-format1
 (:doc
  "[Extension Positioning Subtable Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#extension-positioning-subtable-format-1)")
 (field pos-format u16 :format 1 :doc "Format identifier: format = 1")
 (field extension-lookup-type u16 :doc
  "Lookup type of subtable referenced by extensionOffset (i.e. the extension subtable).")
 (field extension-offset (offset32 t) :doc
  "Offset to the extension subtable, of lookup type extensionLookupType, relative to the start of the ExtensionPosFormat1 subtable."))


(table single-pos-format3 (:doc "SinglePosFormat3 (ISO/IEC 14496-22:2026).")
 (field pos-format u16 :format 3 :doc "Format identifier: format = 3")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of SinglePos subtable.")
 (field value-format value-format :doc
  "Defines the types of data in the ValueRecord.")
 (field value-record gpos-value-record :doc
  "Defines positioning value(s) — applied to all glyphs in the Coverage table."))


(table single-pos-format4 (:doc "SinglePosFormat4 (ISO/IEC 14496-22:2026).")
 (field pos-format u16 :format 4 :doc "Format identifier: format = 4")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of SinglePos subtable.")
 (field value-format value-format :doc
  "Defines the types of data in the ValueRecords.")
 (field value-count uint24 :doc
  "Number of ValueRecords — must equal glyphCount in the Coverage table.")
 (field value-records computed-array<gpos-value-record<a>> :doc
  "Array of ValueRecords — positioning values applied to glyphs."))


(table pair-pos-format3 (:doc "PairPosFormat3 (ISO/IEC 14496-22:2026).")
 (field pos-format u16 :format 3 :doc "Format identifier: format = 3")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of PairPos subtable.")
 (field value-format1 value-format :doc
  "Defines the types of data in valueRecord1 — for the first glyph in the pair (may be zero).")
 (field value-format2 value-format :doc
  "Defines the types of data in valueRecord2 — for the second glyph in the pair (may be zero).")
 (field pair-set-count uint24 :doc "Number of PairSet tables")
 (field pair-set-offsets (array (offset24 pair-set2)) :count pair-set-count
  :doc
  "Array of offsets to PairSet tables. Offsets are from beginning of PairPos subtable, ordered by Coverage Index."))


(table pair-set2 (:doc "PairSet2 (ISO/IEC 14496-22:2026).")
 (field pair-value-count uint24 :doc "Number of PairValueRecords")
 (field pair-value-records computed-array<pair-value-record2<a>> :doc
  "Array of PairValueRecords, ordered by glyph ID of the second glyph."))


(record pair-value-record2 (:doc "PairValueRecord2 (ISO/IEC 14496-22:2026).")
 (field second-glyph glyph-id24 :doc
  "Glyph ID of second glyph in the pair (first glyph is listed in the Coverage table).")
 (field value-record1 gpos-value-record :doc
  "Positioning data for the first glyph in the pair.")
 ;; value-record2 follows the variable-size value-record1; see src/extensions.lisp.
 )


(table pair-pos-format4 (:doc "PairPosFormat4 (ISO/IEC 14496-22:2026).")
 (field pos-format u16 :format 4 :doc "Format identifier: format = 4")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of PairPos subtable.")
 (field value-format1 value-format :doc
  "ValueRecord definition — for the first glyph of the pair (may be zero).")
 (field value-format2 value-format :doc
  "ValueRecord definition — for the second glyph of the pair (may be zero).")
 (field class-def1-offset (offset32 class-def) :doc
  "Offset to ClassDef table, from beginning of PairPos subtable — for the first glyph of the pair.")
 (field class-def2-offset (offset32 class-def) :doc
  "Offset to ClassDef table, from beginning of PairPos subtable — for the second glyph of the pair.")
 (field class1-count u16 :doc
  "Number of classes in classDef1 table — includes Class 0.")
 (field class2-count u16 :doc
  "Number of classes in classDef2 table — includes Class 0.")
 (field class1-records computed-array<class1-record<a>> :doc
  "Array of Class1 records, ordered by classes in classDef1."))


(table cursive-pos-format2 (:doc "CursivePosFormat2 (ISO/IEC 14496-22:2026).")
 (field pos-format u16 :format 2 :doc "Format identifier: format = 2")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table, from beginning of CursivePos subtable.")
 (field entry-exit-count uint24 :doc "Number of EntryExit records")
 (field entry-exit-record (array entry-exit-record2) :count entry-exit-count
  :doc "Array of EntryExit records, in Coverage index order."))


(record entry-exit-record2 (:doc "EntryExitRecord2 (ISO/IEC 14496-22:2026).")
 (field entry-anchor-offset (offset24 anchor-table) :doc
  "Offset to entryAnchor table, from beginning of CursivePos subtable (may be NULL).")
 (field exit-anchor-offset (offset24 anchor-table) :doc
  "Offset to exitAnchor table, from beginning of CursivePos subtable (may be NULL)."))


(table mark-base-pos-format2
 (:doc "MarkBasePosFormat2 (ISO/IEC 14496-22:2026).")
 (field pos-format u16 :format 2 :doc "Format identifier: format = 2")
 (field mark-coverage-offset (offset32 coverage-table) :doc
  "Offset to markCoverage table, from beginning of MarkBasePos subtable.")
 (field base-coverage-offset (offset32 coverage-table) :doc
  "Offset to baseCoverage table, from beginning of MarkBasePos subtable.")
 (field mark-class-count u16 :doc "Number of classes defined for marks")
 (field mark-array-offset (offset32 mark-array2) :doc
  "Offset to MarkArray table, from beginning of MarkBasePos subtable.")
 (field base-array-offset (offset32 base-array2) :doc
  "Offset to BaseArray table, from beginning of MarkBasePos subtable."))


(table base-array2 (:doc "BaseArray2 (ISO/IEC 14496-22:2026).")
 (field base-count uint24 :doc "Number of BaseRecords")
 (field base-records computed-array<base-record2<a>> :doc
  "Array of BaseRecords, in order of baseCoverage Index."))


(record base-record2 (:doc "BaseRecord2 (ISO/IEC 14496-22:2026).")
 (field base-anchor-offsets (array (offset24 anchor-table)) :count
  mark-class-count :doc
  "Array of offsets (one per mark class) to Anchor tables. Offsets are from beginning of BaseArray table, ordered by class (offsets may be NULL)."))


(table mark-lig-pos-format2 (:doc "MarkLigPosFormat2 (ISO/IEC 14496-22:2026).")
 (field pos-format u16 :format 2 :doc "Format identifier: format = 2")
 (field mark-coverage-offset (offset32 coverage-table) :doc
  "Offset to markCoverage table, from beginning of MarkLigPos subtable.")
 (field ligature-coverage-offset (offset32 coverage-table) :doc
  "Offset to ligatureCoverage table, from beginning of MarkLigPos subtable.")
 (field mark-class-count u16 :doc "Number of defined mark classes")
 (field mark-array-offset (offset32 mark-array2) :doc
  "Offset to MarkArray table, from beginning of MarkLigPos subtable.")
 (field ligature-array-offset (offset32 ligature-array2) :doc
  "Offset to LigatureArray table, from beginning of MarkLigPos subtable."))


(table ligature-array2 (:doc "LigatureArray2 (ISO/IEC 14496-22:2026).")
 (field ligature-count uint24 :doc "Number of LigatureAttach table offsets")
 (field ligature-attach-offsets (array (offset24 ligature-attach2)) :count
  ligature-count :doc
  "Array of offsets to LigatureAttach tables. Offsets are from beginning of LigatureArray2 table, ordered by ligatureCoverage index."))


(table ligature-attach2 (:doc "LigatureAttach2 (ISO/IEC 14496-22:2026).")
 (field component-count u16 :doc "Number of ComponentRecords in this ligature")
 (field component-records computed-array<component-record2<a>> :doc
  "Array of Component records, ordered in writing direction."))


(record component-record2 (:doc "ComponentRecord2 (ISO/IEC 14496-22:2026).")
 (field ligature-anchor-offsets (array (offset24 anchor-table)) :count
  mark-class-count :doc
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of LigatureAttach table, ordered by class (offsets may be NULL)."))


(table mark-mark-pos-format2
 (:doc "MarkMarkPosFormat2 (ISO/IEC 14496-22:2026).")
 (field pos-format u16 :format 2 :doc "Format identifier: format = 2")
 (field mark1-coverage-offset (offset32 coverage-table) :doc
  "Offset to Combining Mark Coverage table, from beginning of MarkMarkPos subtable.")
 (field mark2-coverage-offset (offset32 coverage-table) :doc
  "Offset to Base Mark Coverage table, from beginning of MarkMarkPos subtable.")
 (field mark-class-count u16 :doc "Number of Combining Mark classes defined")
 (field mark1-array-offset (offset32 mark-array2) :doc
  "Offset to MarkArray table for mark1, from beginning of MarkMarkPos subtable.")
 (field mark2-array-offset (offset32 mark2-array2) :doc
  "Offset to Mark2Array table for mark2, from beginning of MarkMarkPos subtable."))


(table mark2-array2 (:doc "Mark2Array2 (ISO/IEC 14496-22:2026).")
 (field mark2-count u16 :doc "Number of Mark2 records")
 (field mark2-records computed-array<mark2-record2<a>> :doc
  "Array of Mark2Records, in Coverage order."))


(record mark2-record2 (:doc "Mark2Record2 (ISO/IEC 14496-22:2026).")
 (field mark2-anchor-offsets (array (offset24 anchor-table)) :count
  mark-class-count :doc
  "Array of offsets (one per class) to Anchor tables. Offsets are from beginning of Mark2Array table, in class order (offsets may be NULL)."))


(table mark-array2 (:doc "MarkArray2 (ISO/IEC 14496-22:2026).")
 (field mark-count uint24 :doc "Number of MarkRecords")
 (field mark-records (array mark-record2) :count mark-count :doc
  "Array of MarkRecords, ordered by corresponding glyphs in the associated mark Coverage table."))


(record mark-record2 (:doc "MarkRecord2 (ISO/IEC 14496-22:2026).")
 (field mark-class u16 :doc "Class defined for the associated mark.")
 (field mark-anchor-offset (offset24 anchor-table) :doc
  "Offset to Anchor table, from beginning of MarkArray table."))


(format-table u16 cursive-pos (:doc "CursivePos (ISO/IEC 14496-22:2026).")
 (variant format1 cursive-pos-format1) (variant format2 cursive-pos-format2))


(format-table u16 mark-base-pos (:doc "MarkBasePos (ISO/IEC 14496-22:2026).")
 (variant format1 mark-base-pos-format1)
 (variant format2 mark-base-pos-format2))


(format-table u16 mark-lig-pos (:doc "MarkLigPos (ISO/IEC 14496-22:2026).")
 (variant format1 mark-lig-pos-format1) (variant format2 mark-lig-pos-format2))


(format-table u16 mark-mark-pos (:doc "MarkMarkPos (ISO/IEC 14496-22:2026).")
 (variant format1 mark-mark-pos-format1)
 (variant format2 mark-mark-pos-format2))

