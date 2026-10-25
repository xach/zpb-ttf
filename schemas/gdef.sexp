;;;; gdef.sexp — OpenType table schema
;;;; Generated from Fontations gdef.rs spec


(table gdef (:tag "GDEF")
 (:doc
  "[GDEF](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#gdef-header) 1.0")
 (field version major-minor :doc "The major/minor version of the GDEF table")
 (field glyph-class-def-offset (offset16 class-def) :doc
  "Offset to class definition table for glyph type, from beginning of GDEF header (may be NULL)")
 (field attach-list-offset (offset16 attach-list) :doc
  "Offset to attachment point list table, from beginning of GDEF header (may be NULL)")
 (field lig-caret-list-offset (offset16 lig-caret-list) :doc
  "Offset to ligature caret list table, from beginning of GDEF header (may be NULL)")
 (field mark-attach-class-def-offset (offset16 class-def) :doc
  "Offset to class definition table for mark attachment type, from beginning of GDEF header (may be NULL)")
 (field mark-glyph-sets-def-offset (offset16 mark-glyph-sets) :since-version
  "1.2" :doc
  "Offset to the table of mark glyph set definitions, from beginning of GDEF header (may be NULL)")
 (field item-var-store-offset (offset32 item-variation-store) :since-version
  "1.3" :doc
  "Offset to the Item Variation Store table, from beginning of GDEF header (may be NULL)")
 (field glyph-class-def2-offset (offset32 class-def) :since-version "1.4" :doc
  "32-bit offset taking precedence over glyph_class_def_offset when nonzero.")
 (field attach-list2-offset (offset32 attach-list) :since-version "1.4" :doc
  "32-bit offset taking precedence over attach_list_offset when nonzero.")
 (field lig-caret-list2-offset (offset32 lig-caret-list2) :since-version "1.4"
  :doc
  "32-bit offset taking precedence over lig_caret_list_offset when nonzero.")
 (field mark-attach-class-def2-offset (offset32 class-def) :since-version "1.4"
  :doc
  "32-bit offset taking precedence over mark_attach_class_def_offset when nonzero.")
 (field mark-glyph-sets-def2-offset (offset32 mark-glyph-sets) :since-version
  "1.4" :doc
  "32-bit offset taking precedence over mark_glyph_sets_def_offset when nonzero."))


(enum u16 glyph-class-def
 (:doc
  "Used in the [Glyph Class Definition Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#glyph-class-definition-table)")
 (base 1) (ligature 2) (mark 3) (component 4))


(table attach-list
 (:doc
  "[Attachment Point List Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#attachment-point-list-table)")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table - from beginning of AttachList table")
 (field glyph-count u16 :doc "Number of glyphs with attachment points")
 (field attach-point-offsets (array (offset16 attach-point)) :count glyph-count
  :doc
  "Array of offsets to AttachPoint tables-from beginning of AttachList table-in Coverage Index order"))


(table attach-point (:doc "Part of [AttachList]")
 (field point-count u16 :doc "Number of attachment points on this glyph")
 (field point-indices (array u16) :count point-count :doc
  "Array of contour point indices -in increasing numerical order"))


(table lig-caret-list
 (:doc
  "[Ligature Caret List Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#ligature-caret-list-table)")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to Coverage table - from beginning of LigCaretList table")
 (field lig-glyph-count u16 :doc "Number of ligature glyphs")
 (field lig-glyph-offsets (array (offset16 lig-glyph)) :count lig-glyph-count
  :doc
  "Array of offsets to LigGlyph tables, from beginning of LigCaretList table —in Coverage Index order"))


(table lig-glyph
 (:doc
  "[Ligature Glyph Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#ligature-glyph-table)")
 (field caret-count u16 :doc
  "Number of CaretValue tables for this ligature (components - 1)")
 (field caret-value-offsets (array (offset16 caret-value)) :count caret-count
  :doc
  "Array of offsets to CaretValue tables, from beginning of LigGlyph table — in increasing coordinate order"))


(format-table u16 caret-value
 (:doc
  "[Caret Value Tables](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#caret-value-tables)")
 (variant format1 caret-value-format1) (variant format2 caret-value-format2)
 (variant format3 caret-value-format3))


(table caret-value-format1
 (:doc
  "[CaretValue Format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#caretvalue-format-1)")
 (field caret-value-format u16 :format 1 :doc "Format identifier: format = 1")
 (field coordinate i16 :doc "X or Y value, in design units"))


(table caret-value-format2
 (:doc
  "[CaretValue Format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#caretvalue-format-2)")
 (field caret-value-format u16 :format 2 :doc "Format identifier: format = 2")
 (field caret-value-point-index u16 :doc "Contour point index on glyph"))


(table caret-value-format3
 (:doc
  "[CaretValue Format 3](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#caretvalue-format-3)")
 (field caret-value-format u16 :format 3 :doc "Format identifier-format = 3")
 (field coordinate i16 :doc "X or Y value, in design units")
 (field device-offset (offset16 device-or-variation-index) :doc
  "Offset to Device table (non-variable font) / Variation Index table (variable font) for X or Y value-from beginning of CaretValue table"))


(table mark-glyph-sets
 (:doc
  "[Mark Glyph Sets Table](https://docs.microsoft.com/en-us/typography/opentype/spec/gdef#mark-glyph-sets-table)")
 (field format-table u16 :format 1 :doc "Format identifier == 1")
 (field mark-glyph-set-count u16 :doc "Number of mark glyph sets defined")
 (field coverage-offsets (array (offset32 coverage-table)) :count
  mark-glyph-set-count :doc
  "Array of offsets to mark glyph set coverage tables, from the start of the MarkGlyphSets table."))


(table lig-caret-list2 (:doc "LigCaretList2 (ISO/IEC 14496-22:2026).")
 (field coverage-offset (offset32 coverage-table) :doc
  "Offset to Coverage table - from beginning of LigCaretList2 table")
 (field lig-glyph-count uint24 :doc "Number of ligature glyphs")
 (field lig-glyph-offsets (array (offset24 lig-glyph)) :count lig-glyph-count
  :doc
  "Array of offsets to LigGlyph tables, from beginning of LigCaretList2 table —in Coverage Index order"))

