;;;; cvar.sexp — OpenType table schema
;;;; Generated from Fontations cvar.rs spec


(table cvar (:tag "cvar")
 (:doc
  "The [cvar](https://learn.microsoft.com/en-us/typography/opentype/spec/cvar) table.")
 (field version major-minor :doc
  "Major/minor version number of the CVT variations table — set to (1,0).")
 (field tuple-variation-count tuple-variation-count :doc
  "A packed field. The high 4 bits are flags, and the low 12 bits are the number of tuple variation tables for this glyph. The number of tuple variation tables can be any number between 1 and 4095.")
 (field data-offset (offset16 font-data) :doc
  "Offset from the start of the 'cvar' table to the serialized data.")
 (field tuple-variation-headers var-len-array<tuple-variation-header<->> :doc
  "Array of tuple variation headers."))

