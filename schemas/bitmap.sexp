;;;; bitmap.sexp — OpenType table schema
;;;; Generated from Fontations bitmap.rs spec


(record bitmap-size
 (:doc
  "[BitmapSize](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#bitmapsize-record) record.")
 (field index-subtable-list-offset u32 :doc
  "Offset to IndexSubtableList, from beginning of EBLC/CBLC.")
 (field index-subtable-list-size u32 :doc
  "Total size in bytes of the IndexSubtableList including its array of IndexSubtables.")
 (field number-of-index-subtables u32 :doc
  "Number of IndexSubtables in the IndexSubtableList.")
 (field color-ref u32 :doc "Not used; set to 0.")
 (field hori sbit-line-metrics :doc
  "Line metrics for text rendered horizontally.")
 (field vert sbit-line-metrics :doc
  "Line metrics for text rendered vertically.")
 (field start-glyph-index glyph-id16 :doc "Lowest glyph index for this size.")
 (field end-glyph-index glyph-id16 :doc "Highest glyph index for this size.")
 (field ppem-x u8 :doc "Horizontal pixels per em.")
 (field ppem-y u8 :doc "Vertical pixels per em.")
 (field bit-depth u8 :doc
  "The Microsoft rasterizer v.1.7 or greater supports the following bitDepth values, as described below: 1, 2, 4, and 8 (and 32 for CBLC).")
 (field flags bitmap-flags :doc "Vertical or horizontal."))


(record sbit-line-metrics
 (:doc
  "[SbitLineMetrics](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#sbitlinemetrics-record) record.")
 (field ascender i8) (field descender i8) (field width-max u8)
 (field caret-slope-numerator i8) (field caret-slope-denominator u8)
 (field caret-offset i8) (field min-origin-sb i8) (field min-advance-sb i8)
 (field max-before-bl i8) (field min-after-bl i8) (field pad1 i8)
 (field pad2 i8))


(flags u8 bitmap-flags
 (:doc
  "[Bitmap flags](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#bitmap-flags).")
 (horizontal-metrics 1) (vertical-metrics 2))


(record big-glyph-metrics
 (:doc
  "[BigGlyphMetrics](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#bigglyphmetrics) record.")
 (field height u8 :doc "Number of rows of data.")
 (field width u8 :doc "Number of columns of data.")
 (field hori-bearing-x i8 :doc
  "Distance in pixels from the horizontal origin to the left edge of the bitmap.")
 (field hori-bearing-y i8 :doc
  "Distance in pixels from the horizontal origin to the top edge of the bitmap.")
 (field hori-advance u8 :doc "Horizontal advance width in pixels.")
 (field vert-bearing-x i8 :doc
  "Distance in pixels from the vertical origin to the left edge of the bitmap.")
 (field vert-bearing-y i8 :doc
  "Distance in pixels from the vertical origin to the top edge of the bitmap.")
 (field vert-advance u8 :doc "Vertical advance width in pixels."))


(record small-glyph-metrics
 (:doc
  "[SmallGlyphMetrics](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#smallglyphmetrics) record.")
 (field height u8 :doc "Number of rows of data.")
 (field width u8 :doc "Number of columns of data.")
 (field bearing-x i8 :doc
  "Distance in pixels from the horizontal origin to the left edge of the bitmap (for horizontal text); or distance in pixels from the vertical origin to the top edge of the bitmap (for vertical text).")
 (field bearing-y i8 :doc
  "Distance in pixels from the horizontal origin to the top edge of the bitmap (for horizontal text); or distance in pixels from the vertical origin to the left edge of the bitmap (for vertical text).")
 (field advance u8 :doc "Horizontal or vertical advance width in pixels."))


(table index-subtable-list
 (:doc
  "[IndexSubtableList](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtablelist) table.")
 (field index-subtable-records (array index-subtable-record) :count
  number-of-index-subtables :doc "Array of IndexSubtableRecords."))


(record index-subtable-record
 (field first-glyph-index glyph-id16 :doc "First glyph ID of this range.")
 (field last-glyph-index glyph-id16 :doc
  "Last glyph ID of this range (inclusive).")
 (field index-subtable-offset (offset32 index-subtable) :doc
  "Offset to an IndexSubtable from the start of the IndexSubtableList."))


(table index-subtable1
 (:doc
  "[IndexSubTable1](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable1-variable-metrics-glyphs-with-4-byte-offsets): variable-metrics glyphs with 4-byte offsets.")
 (field index-format u16 :format 1 :doc "Format of this IndexSubTable.")
 (field image-format u16 :doc "Format of EBDT image data.")
 (field image-data-offset u32 :doc "Offset to image data in EBDT table.")
 (field sbit-offsets (array u32) :count
  (subtract_add_two last-glyph-index first-glyph-index)))


(table index-subtable2
 (:doc
  "[IndexSubTable2](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable2-all-glyphs-have-identical-metrics): all glyphs have identical metrics.")
 (field index-format u16 :format 2 :doc "Format of this IndexSubTable.")
 (field image-format u16 :doc "Format of EBDT image data.")
 (field image-data-offset u32 :doc "Offset to image data in EBDT table.")
 (field image-size u32 :doc "All the glyphs are of the same size.")
 (field big-metrics big-glyph-metrics :doc
  "All glyphs have the same metrics; glyph data may be compressed, byte-aligned, or bit-aligned."))


(table index-subtable3
 (:doc
  "[IndexSubTable3](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable3-variable-metrics-glyphs-with-2-byte-offsets): variable-metrics glyphs with 2-byte offsets.")
 (field index-format u16 :format 3 :doc "Format of this IndexSubTable.")
 (field image-format u16 :doc "Format of EBDT image data.")
 (field image-data-offset u32 :doc "Offset to image data in EBDT table.")
 (field sbit-offsets (array u16) :count
  (subtract_add_two last-glyph-index first-glyph-index)))


(table index-subtable4
 (:doc
  "[IndexSubTable4](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable3-variable-metrics-glyphs-with-2-byte-offsets): variable-metrics glyphs with sparse glyph codes.")
 (field index-format u16 :format 4 :doc "Format of this IndexSubTable.")
 (field image-format u16 :doc "Format of EBDT image data.")
 (field image-data-offset u32 :doc "Offset to image data in EBDT table.")
 (field num-glyphs u32 :doc "Array length.")
 (field glyph-array (array glyph-id-offset-pair) :count (add num-glyphs 1) :doc
  "One per glyph."))


(record glyph-id-offset-pair
 (:doc
  "[GlyphIdOffsetPair](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#glyphidoffsetpair-record) record.")
 (field glyph-id glyph-id16 :doc "Glyph ID of glyph present.")
 (field sbit-offset u16 :doc "Location in EBDT."))


(table index-subtable5
 (:doc
  "[IndexSubTable5](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc#indexsubtable5-constant-metrics-glyphs-with-sparse-glyph-codes): constant-metrics glyphs with sparse glyph codes")
 (field index-format u16 :format 5 :doc "Format of this IndexSubTable.")
 (field image-format u16 :doc "Format of EBDT image data.")
 (field image-data-offset u32 :doc "Offset to image data in EBDT table.")
 (field image-size u32 :doc "All glyphs have the same data size.")
 (field big-metrics big-glyph-metrics :doc "All glyphs have the same metrics.")
 (field num-glyphs u32 :doc "Array length.")
 (field glyph-array (array glyph-id16) :count num-glyphs :doc
  "One per glyph, sorted by glyhph ID."))


(record bdt-component
 (:doc
  "[EbdtComponent](https://learn.microsoft.com/en-us/typography/opentype/spec/ebdt#ebdtcomponent-record) record.")
 (field glyph-id glyph-id16 :doc "Component glyph ID.")
 (field x-offset i8 :doc "Position of component left.")
 (field y-offset i8 :doc "Position of component top."))

