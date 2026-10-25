;;;; ltag.sexp — OpenType table schema
;;;; Generated from Fontations ltag.rs spec


(table ltag (:tag "ltag")
 (:doc
  "The [language tag](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6ltag.html) table.")
 (field version u32 :doc "Table version; currently 1.")
 (field flags u32 :doc "Table flags; currently none defined.")
 (field num-tags u32 :doc "Number of language tags which follow.")
 (field tag-ranges (array ft-string-range) :count num-tags :doc
  "Range of each tag's string."))


(record ft-string-range (:doc "Offset and length of string in `ltag` table.")
 (field offset u16 :doc
  "Offset from the start of the table to the beginning of the string.")
 (field length u16 :doc "String length (in bytes)."))

