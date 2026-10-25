;;;; cblc.sexp — OpenType table schema
;;;; Generated from Fontations cblc.rs spec


(table cblc (:tag "CBLC")
 (:doc
  "The [Color Bitmap Location](https://learn.microsoft.com/en-us/typography/opentype/spec/cblc) table")
 (field major-version u16 :doc "Major version of the CBLC table, = 3.")
 (field minor-version u16 :doc "Minor version of CBLC table, = 0.")
 (field num-sizes u32 :doc "Number of BitmapSize records.")
 (field bitmap-sizes (array bitmap-size) :count num-sizes :doc
  "BitmapSize records array."))

