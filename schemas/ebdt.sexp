;;;; ebdt.sexp — OpenType table schema
;;;; Generated from Fontations ebdt.rs spec


(table ebdt (:tag "EBDT")
 (:doc
  "The [Embedded Bitmap Data](https://learn.microsoft.com/en-us/typography/opentype/spec/ebdt) table")
 (field major-version u16 :doc "Major version of the EBDT table, = 2.")
 (field minor-version u16 :doc "Minor version of EBDT table, = 0."))

