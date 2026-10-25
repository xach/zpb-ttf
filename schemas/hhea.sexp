;;;; hhea.sexp — OpenType table schema
;;;; Generated from Fontations hhea.rs spec


(table hhea (:tag "hhea")
 (:doc
  "[hhea](https://docs.microsoft.com/en-us/typography/opentype/spec/hhea) Horizontal Header Table")
 (field version major-minor :doc "The major/minor version (1, 0)")
 (field ascender f-word :doc "Typographic ascent.")
 (field descender f-word :doc "Typographic descent.")
 (field line-gap f-word :doc
  "Typographic line gap. Negative LineGap values are treated as zero in some legacy platform implementations.")
 (field advance-width-max uf-word :doc
  "Maximum advance width value in 'hmtx' table.")
 (field min-left-side-bearing f-word :doc
  "Minimum left sidebearing value in 'hmtx' table for glyphs with contours (empty glyphs should be ignored).")
 (field min-right-side-bearing f-word :doc
  "Minimum right sidebearing value; calculated as min(aw - (lsb + xMax - xMin)) for glyphs with contours (empty glyphs should be ignored).")
 (field x-max-extent f-word :doc "Max(lsb + (xMax-xMin))")
 (field caret-slope-rise i16 :doc
  "Used to calculate the slope of the cursor (rise/run); 1 for vertical caret, 0 for horizontal.")
 (field caret-slope-run i16 :doc "0 for vertical caret, 1 for horizontal.")
 (field caret-offset i16 :doc
  "The amount by which a slanted highlight on a glyph needs to be shifted to produce the best appearance. Set to 0 for non-slanted fonts")
 (field reserved1 i16 :doc "set to 0") (field reserved2 i16 :doc "set to 0")
 (field reserved3 i16 :doc "set to 0") (field reserved4 i16 :doc "set to 0")
 (field metric-data-format i16 :doc "0 for current format.")
 (field number-of-h-metrics u16 :doc
  "Number of hMetric entries in 'hmtx' table"))


(table hhea-extended (:tag "HHEA")
 (:doc "HHEA header table (ISO/IEC 14496-22:2026, 5.1.10).")
 (field version major-minor) (field ascender f-word) (field descender f-word)
 (field line-gap f-word) (field advance-width-max uf-word)
 (field min-left-side-bearing f-word) (field min-right-side-bearing f-word)
 (field x-max-extent f-word) (field caret-slope-rise i16)
 (field caret-slope-run i16) (field caret-offset i16) (field reserved1 i16)
 (field reserved2 i16) (field reserved3 i16) (field reserved4 i16)
 (field metric-data-format i16)
 (field number-of-h-metrics u32 :doc "Number of long metric records in HMTX."))

