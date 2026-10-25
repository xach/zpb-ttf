;;;; post.sexp — OpenType table schema
;;;; Generated from Fontations post.rs spec


(table post (:tag "post")
 (:doc
  "[post (PostScript)](https://docs.microsoft.com/en-us/typography/opentype/spec/post#header) table")
 (field version major-minor :doc
  "0x00010000 for version 1.0 0x00020000 for version 2.0 0x00025000 for version 2.5 (deprecated) 0x00030000 for version 3.0")
 (field italic-angle fixed :doc
  "Italic angle in counter-clockwise degrees from the vertical. Zero for upright text, negative for text that leans to the right (forward).")
 (field underline-position f-word :doc
  "This is the suggested distance of the top of the underline from the baseline (negative values indicate below baseline). The PostScript definition of this FontInfo dictionary key (the y coordinate of the center of the stroke) is not used for historical reasons. The value of the PostScript key may be calculated by subtracting half the underlineThickness from the value of this field.")
 (field underline-thickness f-word :doc
  "Suggested values for the underline thickness. In general, the underline thickness should match the thickness of the underscore character (U+005F LOW LINE), and should also match the strikeout thickness, which is specified in the OS/2 table.")
 (field is-fixed-pitch u32 :doc
  "Set to 0 if the font is proportionally spaced, non-zero if the font is not proportionally spaced (i.e. monospaced).")
 (field min-mem-type42 u32 :doc
  "Minimum memory usage when an OpenType font is downloaded.")
 (field max-mem-type42 u32 :doc
  "Maximum memory usage when an OpenType font is downloaded.")
 (field min-mem-type1 u32 :doc
  "Minimum memory usage when an OpenType font is downloaded as a Type 1 font.")
 (field max-mem-type1 u32 :doc
  "Maximum memory usage when an OpenType font is downloaded as a Type 1 font.")
 (field num-glyphs u16 :since-version "2.0" :doc
  "Number of glyphs (this should be the same as numGlyphs in 'maxp' table).")
 (field glyph-name-index (array u16) :count num-glyphs :since-version "2.0"
  :doc "Array of indices into the string data. See below for details.")
 (field string-data var-len-array<p-string<a>> :since-version "2.0" :doc
  "Storage for the string data."))

