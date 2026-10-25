;;;; cbdt.sexp — OpenType table schema
;;;; Generated from Fontations cbdt.rs spec


(table cbdt (:tag "CBDT")
 (:doc
  "The [Color Bitmap Data](https://learn.microsoft.com/en-us/typography/opentype/spec/cbdt) table")
 (field major-version u16 :doc "Major version of the CBDT table, = 3.")
 (field minor-version u16 :doc "Minor version of CBDT table, = 0."))

