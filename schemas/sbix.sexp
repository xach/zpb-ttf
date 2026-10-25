;;;; sbix.sexp — OpenType table schema
;;;; Generated from Fontations sbix.rs spec


(flags u16 header-flags (:doc "Sbix header flags.") (always-set 1)
 (draw-outlines 2))


(table sbix (:tag "sbix")
 (:doc
  "The [sbix (Standard Bitmap Graphics)](https://docs.microsoft.com/en-us/typography/opentype/spec/sbix) table")
 (field version u16 :doc "Table version number — set to 1.")
 (field flags header-flags :doc
  "Bit 0: Set to 1. Bit 1: Draw outlines. Bits 2 to 15: reserved (set to 0).")
 (field num-strikes u32 :doc "Number of bitmap strikes.")
 (field strike-offsets (array (offset32 strike)) :count num-strikes :doc
  "Offsets from the beginning of the 'sbix' table to data for each individual bitmap strike."))


(table strike
 (:doc
  "[Strike](https://learn.microsoft.com/en-us/typography/opentype/spec/sbix#strikes) header table")
 (field ppem u16 :doc "The PPEM size for which this strike was designed.")
 (field ppi u16 :doc
  "The device pixel density (in PPI) for which this strike was designed. (E.g., 96 PPI, 192 PPI.)")
 (field glyph-data-offsets (array u32) :count (add num-glyphs 1) :doc
  "Offset from the beginning of the strike data header to bitmap data for an individual glyph ID."))


(table glyph-data
 (:doc
  "[Glyph data](https://learn.microsoft.com/en-us/typography/opentype/spec/sbix#glyph-data) table")
 (field origin-offset-x i16 :doc
  "The horizontal (x-axis) position of the left edge of the bitmap graphic in relation to the glyph design space origin.")
 (field origin-offset-y i16 :doc
  "The vertical (y-axis) position of the bottom edge of the bitmap graphic in relation to the glyph design space origin.")
 (field graphic-type tag :doc
  "Indicates the format of the embedded graphic data: one of 'jpg ', 'png ' or 'tiff', or the special format 'dupe'.")
 (field data (array u8) :count :remainder :doc
  "The actual embedded graphic data. The total length is inferred from sequential entries in the glyphDataOffsets array and the fixed size (8 bytes) of the preceding fields."))

