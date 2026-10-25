;;;; head.sexp — OpenType table schema
;;;; Generated from Fontations head.rs spec


(flags u16 mac-style (:doc "The `macStyle` field for the head table.") (bold 1)
 (italic 2) (underline 4) (outline 8) (shadow 16) (condensed 32) (extended 64))


(flags u16 flags (:doc "The `flags` field for the head table.")
 (baseline-at-y-0 1) (lsb-at-x-0 2) (instructions-may-depend-on-point-size 4)
 (force-integer-ppem 8) (instructions-may-alter-advance-width 16)
 (lossless-transformed-font-data 2048) (converted-font 4096)
 (optimized-for-cleartype 8192) (last-resort-font 16384))


(table head (:tag "head")
 (:doc
  "The [head](https://docs.microsoft.com/en-us/typography/opentype/spec/head) (font header) table.")
 (field version major-minor :doc
  "Version number of the font header table, set to (1, 0)")
 (field font-revision fixed :doc "Set by font manufacturer.")
 (field checksum-adjustment u32 :doc
  "To compute: set it to 0, sum the entire font as uint32, then store 0xB1B0AFBA - sum. If the font is used as a component in a font collection file, the value of this field will be invalidated by changes to the file structure and font table directory, and must be ignored.")
 (field magic-number u32 :doc "Set to 0x5F0F3CF5.")
 (field flags flags :doc "See the flags enum.")
 (field units-per-em u16 :doc
  "Set to a value from 16 to 16384. Any value in this range is valid. In fonts that have TrueType outlines, a power of 2 is recommended as this allows performance optimizations in some rasterizers.")
 (field created long-date-time :doc
  "Number of seconds since 12:00 midnight that started January 1st 1904 in GMT/UTC time zone.")
 (field modified long-date-time :doc
  "Number of seconds since 12:00 midnight that started January 1st 1904 in GMT/UTC time zone.")
 (field x-min i16 :doc "Minimum x coordinate across all glyph bounding boxes.")
 (field y-min i16 :doc "Minimum y coordinate across all glyph bounding boxes.")
 (field x-max i16 :doc "Maximum x coordinate across all glyph bounding boxes.")
 (field y-max i16 :doc "Maximum y coordinate across all glyph bounding boxes.")
 (field mac-style mac-style :doc
  "Bits identifying the font's style; see [MacStyle]")
 (field lowest-rec-ppem u16 :doc "Smallest readable size in pixels.")
 (field font-direction-hint i16 :doc "Deprecated (Set to 2).")
 (field index-to-loc-format i16 :doc
  "0 for short offsets (Offset16), 1 for long (Offset32).")
 (field glyph-data-format i16 :doc "0 for current format."))

