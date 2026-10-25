;;;; math.sexp — OpenType table schema
;;;; Generated from Fontations math.rs spec


(table math (:tag "MATH")
 (:doc
  "The [MATH](https://learn.microsoft.com/en-us/typography/opentype/spec/math) table")
 (field version major-minor :doc
  "(major, minor) Version for the MATH table (1,0)")
 (field math-constants-offset (offset16 math-constants) :doc
  "Offset to MathConstants table, from beginning of MATH table.")
 (field math-glyph-info-offset (offset16 math-glyph-info) :doc
  "Offset to MathGlyphInfo table, from beginning of MATH table.")
 (field math-variants-offset (offset16 math-variants) :doc
  "Offset to MathVariants table, from beginning of MATH table."))


(record math-value-record
 (:doc
  "[MathValueRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathvaluerecord)  The device offset is measured from the beginning of the table that contains the record, not from the record itself.")
 (field value f-word :doc "The X or Y value in design units.")
 (field device-offset (offset16 device-or-variation-index) :doc
  "Offset to the Device table (non-variable font) / VariationIndex table (variable font) for this value, from the beginning of the parent table. May be NULL."))


(table math-constants
 (:doc
  "[MathConstants](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathconstants-table)")
 (field script-percent-scale-down i16 :doc
  "Percentage of scaling down for level 1 superscripts and subscripts.")
 (field script-script-percent-scale-down i16 :doc
  "Percentage of scaling down for level 2 (scriptScript) superscripts and subscripts.")
 (field delimited-sub-formula-min-height uf-word :doc
  "Minimum height required for a delimited expression (contained within parentheses, etc.) to be treated as a sub-formula.")
 (field display-operator-min-height uf-word :doc
  "Minimum height of n-ary operators (such as integral and summation) for formulas in display mode (that is, appearing as standalone page elements, not embedded inline within text).")
 (field math-leading math-value-record :doc
  "White space to be left between math formulas to ensure proper line spacing.")
 (field axis-height math-value-record :doc "Axis height of the font.")
 (field accent-base-height math-value-record :doc
  "Maximum (ink) height of accent base that does not require raising the accents.")
 (field flattened-accent-base-height math-value-record :doc
  "Maximum (ink) height of accent base that does not require flattening the accents.")
 (field subscript-shift-down math-value-record :doc
  "The standard shift down applied to subscript elements.")
 (field subscript-top-max math-value-record :doc
  "Maximum allowed height of the (ink) top of subscripts that does not require moving subscripts further down.")
 (field subscript-baseline-drop-min math-value-record :doc
  "Minimum allowed drop of the baseline of subscripts relative to the (ink) bottom of the base.")
 (field superscript-shift-up math-value-record :doc
  "Standard shift up applied to superscript elements.")
 (field superscript-shift-up-cramped math-value-record :doc
  "Standard shift of superscripts relative to the base, in cramped style.")
 (field superscript-bottom-min math-value-record :doc
  "Minimum allowed height of the (ink) bottom of superscripts that does not require moving subscripts further up.")
 (field superscript-baseline-drop-max math-value-record :doc
  "Maximum allowed drop of the baseline of superscripts relative to the (ink) top of the base.")
 (field sub-superscript-gap-min math-value-record :doc
  "Minimum gap between the superscript and subscript ink.")
 (field superscript-bottom-max-with-subscript math-value-record :doc
  "The maximum level to which the (ink) bottom of superscript can be pushed to increase the gap between superscript and subscript, before subscript starts being moved down.")
 (field space-after-script math-value-record :doc
  "Extra white space to be added after each subscript and superscript.")
 (field upper-limit-gap-min math-value-record :doc
  "Minimum gap between the (ink) bottom of the upper limit, and the (ink) top of the base operator.")
 (field upper-limit-baseline-rise-min math-value-record :doc
  "Minimum distance between baseline of upper limit and (ink) top of the base operator.")
 (field lower-limit-gap-min math-value-record :doc
  "Minimum gap between (ink) top of the lower limit, and (ink) bottom of the base operator.")
 (field lower-limit-baseline-drop-min math-value-record :doc
  "Minimum distance between baseline of the lower limit and (ink) bottom of the base operator.")
 (field stack-top-shift-up math-value-record :doc
  "Standard shift up applied to the top element of a stack.")
 (field stack-top-display-style-shift-up math-value-record :doc
  "Standard shift up applied to the top element of a stack in display style.")
 (field stack-bottom-shift-down math-value-record :doc
  "Standard shift down applied to the bottom element of a stack.")
 (field stack-bottom-display-style-shift-down math-value-record :doc
  "Standard shift down applied to the bottom element of a stack in display style.")
 (field stack-gap-min math-value-record :doc
  "Minimum gap between (ink) bottom of the top element of a stack, and the (ink) top of the bottom element.")
 (field stack-display-style-gap-min math-value-record :doc
  "Minimum gap between (ink) bottom of the top element of a stack, and the (ink) top of the bottom element in display style.")
 (field stretch-stack-top-shift-up math-value-record :doc
  "Standard shift up applied to the top element of the stretch stack.")
 (field stretch-stack-bottom-shift-down math-value-record :doc
  "Standard shift down applied to the bottom element of the stretch stack.")
 (field stretch-stack-gap-above-min math-value-record :doc
  "Minimum gap between the ink of the stretched element, and the (ink) bottom of the element above.")
 (field stretch-stack-gap-below-min math-value-record :doc
  "Minimum gap between the ink of the stretched element, and the (ink) top of the element below.")
 (field fraction-numerator-shift-up math-value-record :doc
  "Standard shift up applied to the numerator.")
 (field fraction-numerator-display-style-shift-up math-value-record :doc
  "Standard shift up applied to the numerator in display style.")
 (field fraction-denominator-shift-down math-value-record :doc
  "Standard shift down applied to the denominator.")
 (field fraction-denominator-display-style-shift-down math-value-record :doc
  "Standard shift down applied to the denominator in display style.")
 (field fraction-numerator-gap-min math-value-record :doc
  "Minimum tolerated gap between the (ink) bottom of the numerator and the ink of the fraction bar.")
 (field fraction-num-display-style-gap-min math-value-record :doc
  "Minimum tolerated gap between the (ink) bottom of the numerator and the ink of the fraction bar in display style.")
 (field fraction-rule-thickness math-value-record :doc
  "Thickness of the fraction bar.")
 (field fraction-denominator-gap-min math-value-record :doc
  "Minimum tolerated gap between the ink of the fraction bar and the (ink) top of the denominator.")
 (field fraction-denom-display-style-gap-min math-value-record :doc
  "Minimum tolerated gap between the ink of the fraction bar and the (ink) top of the denominator in display style.")
 (field skewed-fraction-horizontal-gap math-value-record :doc
  "Horizontal distance between the top and bottom elements of a skewed fraction.")
 (field skewed-fraction-vertical-gap math-value-record :doc
  "Vertical distance between the ink of the top and bottom elements of a skewed fraction.")
 (field overbar-vertical-gap math-value-record :doc
  "Distance between the overbar and the (ink) top of he base.")
 (field overbar-rule-thickness math-value-record :doc "Thickness of overbar.")
 (field overbar-extra-ascender math-value-record :doc
  "Extra white space reserved above the overbar.")
 (field underbar-vertical-gap math-value-record :doc
  "Distance between underbar and (ink) bottom of the base.")
 (field underbar-rule-thickness math-value-record :doc
  "Thickness of underbar.")
 (field underbar-extra-descender math-value-record :doc
  "Extra white space reserved below the underbar.")
 (field radical-vertical-gap math-value-record :doc
  "Space between the (ink) top of the expression and the bar over it.")
 (field radical-display-style-vertical-gap math-value-record :doc
  "Space between the (ink) top of the expression and the bar over it in display style.")
 (field radical-rule-thickness math-value-record :doc
  "Thickness of the radical rule.")
 (field radical-extra-ascender math-value-record :doc
  "Extra white space reserved above the radical.")
 (field radical-kern-before-degree math-value-record :doc
  "Extra horizontal kern before the degree of a radical, if such is present.")
 (field radical-kern-after-degree math-value-record :doc
  "Negative kern after the degree of a radical, if such is present.")
 (field radical-degree-bottom-raise-percent i16 :doc
  "Height of the bottom of the radical degree, if such is present, in proportion to the ascender of the radical sign."))


(table math-glyph-info
 (:doc
  "[MathGlyphInfo](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathglyphinfo-table)")
 (field math-italics-correction-info-offset
  (offset16 math-italics-correction-info) :doc
  "Offset to MathItalicsCorrectionInfo table, from the beginning of the MathGlyphInfo table. May be NULL.")
 (field math-top-accent-attachment-offset (offset16 math-top-accent-attachment)
  :doc
  "Offset to MathTopAccentAttachment table, from the beginning of the MathGlyphInfo table. May be NULL.")
 (field extended-shape-coverage-offset (offset16 coverage-table) :doc
  "Offset to the Coverage table for glyphs with extended shapes, from the beginning of the MathGlyphInfo table. May be NULL.")
 (field math-kern-info-offset (offset16 math-kern-info) :doc
  "Offset to the MathKernInfo table, from the beginning of the MathGlyphInfo table. May be NULL."))


(table math-italics-correction-info
 (:doc
  "[MathItalicsCorrectionInfo](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathitalicscorrectioninfo-table)")
 (field coverage-offset (offset16 coverage-table) :doc
  "Offset to the Coverage table, from the beginning of the MathItalicsCorrectionInfo table.")
 (field italics-correction-count u16 :doc
  "Number of italics correction values. Should coincide with the number of covered glyphs.")
 (field italics-correction (array math-value-record) :count
  italics-correction-count :doc
  "Array of MathValueRecords defining italics correction values for each covered glyph."))


(table math-top-accent-attachment
 (:doc
  "[MathTopAccentAttachment](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathtopaccentattachment-table)")
 (field top-accent-coverage-offset (offset16 coverage-table) :doc
  "Offset to the Coverage table, from the beginning of the MathTopAccentAttachment table.")
 (field top-accent-attachment-count u16 :doc
  "Number of top accent attachment point values. Should coincide with the number of covered glyphs.")
 (field top-accent-attachment (array math-value-record) :count
  top-accent-attachment-count :doc
  "Array of MathValueRecords defining top accent attachment points for each covered glyph."))


(table math-kern-info
 (:doc
  "[MathKernInfo](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathkerninfo-table)")
 (field math-kern-coverage-offset (offset16 coverage-table) :doc
  "Offset to the Coverage table, from the beginning of the MathKernInfo table.")
 (field math-kern-count u16 :doc
  "Number of MathKernInfoRecords. Should coincide with the number of covered glyphs.")
 (field math-kern-info-records (array math-kern-info-record) :count
  math-kern-count :doc
  "Array of MathKernInfoRecords, one for each covered glyph."))


(record math-kern-info-record
 (:doc
  "[MathKernInfoRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathkerninforecord)  The offsets are measured from the beginning of the MathKernInfo table.")
 (field top-right-math-kern-offset (offset16 math-kern) :doc
  "Offset to the MathKern table for the top right corner, from the beginning of the MathKernInfo table. May be NULL.")
 (field top-left-math-kern-offset (offset16 math-kern) :doc
  "Offset to the MathKern table for the top left corner, from the beginning of the MathKernInfo table. May be NULL.")
 (field bottom-right-math-kern-offset (offset16 math-kern) :doc
  "Offset to the MathKern table for the bottom right corner, from the beginning of the MathKernInfo table. May be NULL.")
 (field bottom-left-math-kern-offset (offset16 math-kern) :doc
  "Offset to the MathKern table for the bottom left corner, from the beginning of the MathKernInfo table. May be NULL."))


(table math-kern
 (:doc
  "[MathKern](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathkern-table)")
 (field height-count u16 :doc
  "Number of heights at which the kern value changes.")
 (field correction-height (array math-value-record) :count height-count :doc
  "Array of correction heights, in design units, sorted from lowest to highest.")
 (field kern-values (array math-value-record) :count (add height-count 1) :doc
  "Array of kerning values for different height ranges, in design units.  There is one more of these than there are correction heights: the first applies below the first correction height, and the last above the last."))


(table math-variants
 (:doc
  "[MathVariants](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathvariants-table)")
 (field min-connector-overlap uf-word :doc
  "Minimum overlap of connecting glyphs during glyph construction, in design units.")
 (field vert-glyph-coverage-offset (offset16 coverage-table) :doc
  "Offset to the Coverage table for shapes growing in the vertical direction, from the beginning of the MathVariants table. May be NULL.")
 (field horiz-glyph-coverage-offset (offset16 coverage-table) :doc
  "Offset to the Coverage table for shapes growing in the horizontal direction, from the beginning of the MathVariants table. May be NULL.")
 (field vert-glyph-count u16 :doc
  "Number of glyphs for which information is provided for vertically growing variants. Must be the same as the number of glyph IDs referenced in the vertical Coverage table.")
 (field horiz-glyph-count u16 :doc
  "Number of glyphs for which information is provided for horizontally growing variants. Must be the same as the number of glyph IDs referenced in the horizontal Coverage table.")
 (field vert-glyph-construction-offsets
  (array (offset16 math-glyph-construction)) :count vert-glyph-count :doc
  "Array of offsets to MathGlyphConstruction tables, from the beginning of the MathVariants table, for shapes growing in the vertical direction.")
 (field horiz-glyph-construction-offsets
  (array (offset16 math-glyph-construction)) :count horiz-glyph-count :doc
  "Array of offsets to MathGlyphConstruction tables, from the beginning of the MathVariants table, for shapes growing in the horizontal direction."))


(table math-glyph-construction
 (:doc
  "[MathGlyphConstruction](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathglyphconstruction-table)")
 (field glyph-assembly-offset (offset16 glyph-assembly) :doc
  "Offset to the GlyphAssembly table for this shape, from the beginning of the MathGlyphConstruction table. May be NULL.")
 (field variant-count u16 :doc
  "Count of glyph growing variants for this glyph.")
 (field math-glyph-variant-records (array math-glyph-variant-record) :count
  variant-count :doc
  "MathGlyphVariantRecords for alternative variants of the base glyph."))


(record math-glyph-variant-record
 (:doc
  "[MathGlyphVariantRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathglyphvariantrecord)")
 (field variant-glyph glyph-id16 :doc "Glyph ID for the variant.")
 (field advance-measurement uf-word :doc
  "Advance width/height, in design units, of the variant, in the direction of requested glyph extension."))


(table glyph-assembly
 (:doc
  "[GlyphAssembly](https://learn.microsoft.com/en-us/typography/opentype/spec/math#glyphassembly-table)")
 (field italics-correction math-value-record :doc
  "Italics correction of this GlyphAssembly. Should not depend on the assembly size.")
 (field part-count u16 :doc "Number of parts in this assembly.")
 (field part-records (array glyph-part-record) :count part-count :doc
  "Array of part records, from left to right (for assemblies that extend horizontally) or bottom to top (for assemblies that extend vertically)."))


(record glyph-part-record
 (:doc
  "[GlyphPartRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/math#glyphpartrecord)")
 (field glyph-id glyph-id16 :doc "Glyph ID for the part.")
 (field start-connector-length uf-word :doc
  "Advance width/ height, in design units, of the straight bar connector material at the start of the glyph in the direction of the extension.")
 (field end-connector-length uf-word :doc
  "Advance width/ height, in design units, of the straight bar connector material at the end of the glyph in the direction of the extension.")
 (field full-advance uf-word :doc
  "Full advance width/height for this part in the direction of the extension, in design units.")
 (field part-flags part-flags :doc "Part qualifiers."))


(flags u16 part-flags (:doc "Qualifiers for a [`GlyphPartRecord`].")
 (extender-flag 1))

