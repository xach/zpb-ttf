;;;; eblc.sexp — OpenType table schema
;;;; Generated from Fontations eblc.rs spec


(table eblc (:tag "EBLC")
 (:doc
  "The [Embedded Bitmap Location](https://learn.microsoft.com/en-us/typography/opentype/spec/eblc) table")
 (field major-version u16 :doc "Major version of the EBLC table, = 2.")
 (field minor-version u16 :doc "Minor version of EBLC table, = 0.")
 (field num-sizes u32 :doc "Number of BitmapSize records.")
 (field bitmap-sizes (array bitmap-size) :count num-sizes :doc
  "BitmapSize records array."))

