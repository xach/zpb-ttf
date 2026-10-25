;;;; cpal.sexp — OpenType table schema
;;;; Generated from Fontations cpal.rs spec


(table cpal (:tag "CPAL")
 (:doc
  "[CPAL (Color Palette Table)](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-table-header) table")
 (field version u16 :doc "Table version number (=0).")
 (field num-palette-entries u16 :doc
  "Number of palette entries in each palette.")
 (field num-palettes u16 :doc "Number of palettes in the table.")
 (field num-color-records u16 :doc
  "Total number of color records, combined for all palettes.")
 (field color-records-array-offset (offset32 [color-record]) :doc
  "Offset from the beginning of CPAL table to the first ColorRecord.")
 (field color-record-indices (array u16) :count num-palettes :doc
  "Index of each palette’s first color record in the combined color record array.")
 (field palette-types-array-offset (offset32 [palette-type]) :since-version "1"
  :doc
  "Offset from the beginning of CPAL table to the [Palette Types Array][].  This is an array of 32-bit flag fields that describe properties of each palette.  [Palette Types Array]: https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-type-array")
 (field palette-labels-array-offset (offset32 [name-id]) :since-version "1"
  :doc
  "Offset from the beginning of CPAL table to the [Palette Labels Array][].  This is an array of 'name' table IDs (typically in the font-specific name ID range) that specify user interface strings associated with  each palette. Use 0xFFFF if no name ID is provided for a palette.  [Palette Labels Array]: https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-labels-array")
 (field palette-entry-labels-array-offset (offset32 [name-id]) :since-version
  "1" :doc
  "Offset from the beginning of CPAL table to the [Palette Entry Labels Array][].  This is an array of 'name' table IDs (typically in the font-specific name ID range) that specify user interface strings associated with  each palette entry, e.g. “Outline”, “Fill”. This set of palette entry labels applies to all palettes in the font. Use  0xFFFF if no name ID is provided for a palette entry.  [Palette Entry Labels Array]: https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-entry-label-array"))


(flags u32 palette-type
 (:doc
  "The [PaletteType](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-type-array) flags.")
 (usable-with-light-background 1) (usable-with-dark-background 2))


(record color-record
 (:doc
  "[CPAL (Color Record)](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-entries-and-color-records) record  Contains a color in non-premultiplied BGRA form, in the sRGB color space.")
 (field blue u8 :doc "Blue value (B0).")
 (field green u8 :doc "Green value (B1).")
 (field red u8 :doc "Red value (B2).")
 (field alpha u8 :doc "Alpha value (B3)."))

