;;;; maxp.sexp — OpenType table schema
;;;; Generated from Fontations maxp.rs spec


(table maxp (:tag "maxp")
 (:doc
  "[`maxp`](https://docs.microsoft.com/en-us/typography/opentype/spec/maxp)")
 (field version major-minor :doc
  "The version: 0x00005000 for version 0.5, 0x00010000 for version 1.0.")
 (field num-glyphs u16 :doc "The number of glyphs in the font.")
 (field max-points u16 :since-version "1.0" :doc
  "Maximum points in a non-composite glyph.")
 (field max-contours u16 :since-version "1.0" :doc
  "Maximum contours in a non-composite glyph.")
 (field max-composite-points u16 :since-version "1.0" :doc
  "Maximum points in a composite glyph.")
 (field max-composite-contours u16 :since-version "1.0" :doc
  "Maximum contours in a composite glyph.")
 (field max-zones u16 :since-version "1.0" :doc
  "1 if instructions do not use the twilight zone (Z0), or 2 if instructions do use Z0; should be set to 2 in most cases.")
 (field max-twilight-points u16 :since-version "1.0" :doc
  "Maximum points used in Z0.")
 (field max-storage u16 :since-version "1.0" :doc
  "Number of Storage Area locations.")
 (field max-function-defs u16 :since-version "1.0" :doc
  "Number of FDEFs, equal to the highest function number + 1.")
 (field max-instruction-defs u16 :since-version "1.0" :doc "Number of IDEFs.")
 (field max-stack-elements u16 :since-version "1.0" :doc
  "Maximum stack depth across Font Program ('fpgm' table), CVT Program ('prep' table) and all glyph instructions (in the 'glyf' table).")
 (field max-size-of-instructions u16 :since-version "1.0" :doc
  "Maximum byte count for glyph instructions.")
 (field max-component-elements u16 :since-version "1.0" :doc
  "Maximum number of components referenced at “top level” for any composite glyph.")
 (field max-component-depth u16 :since-version "1.0" :doc
  "Maximum levels of recursion; 1 for simple components."))


(table maxp-extended (:tag "MAXP")
 (:doc "MAXP maximum profile table (ISO/IEC 14496-22:2026, 5.1.12).")
 (field version major-minor)
 (field num-glyphs uint24 :doc "The number of glyphs in the font.")
 (field max-points u16 :since-version "1.0" :doc
  "Maximum points in a non-composite glyph.")
 (field max-contours u16 :since-version "1.0" :doc
  "Maximum contours in a non-composite glyph.")
 (field max-composite-points u16 :since-version "1.0" :doc
  "Maximum points in a composite glyph.")
 (field max-composite-contours u16 :since-version "1.0" :doc
  "Maximum contours in a composite glyph.")
 (field max-zones u16 :since-version "1.0" :doc
  "Number of zones used by instructions.")
 (field max-twilight-points u16 :since-version "1.0" :doc
  "Maximum points in the twilight zone.")
 (field max-storage u16 :since-version "1.0" :doc
  "Number of storage locations.")
 (field max-function-defs u16 :since-version "1.0" :doc
  "Number of function definitions.")
 (field max-instruction-defs u16 :since-version "1.0" :doc
  "Number of instruction definitions.")
 (field max-stack-elements u16 :since-version "1.0" :doc
  "Maximum interpreter stack depth.")
 (field max-size-of-instructions u16 :since-version "1.0" :doc
  "Maximum glyph instruction byte count.")
 (field max-component-elements u16 :since-version "1.0" :doc
  "Maximum top-level component count.")
 (field max-component-depth u16 :since-version "1.0" :doc
  "Maximum component recursion depth."))

