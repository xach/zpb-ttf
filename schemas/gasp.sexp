;;;; gasp.sexp — OpenType table schema
;;;; Generated from Fontations gasp.rs spec


(table gasp (:tag "gasp")
 (:doc
  "[gasp](https://learn.microsoft.com/en-us/typography/opentype/spec/gasp#gasp-table-formats)")
 (field version u16 :doc "Version number (set to 1)")
 (field num-ranges u16 :doc "Number of records to follow")
 (field gasp-ranges (array gasp-range) :count num-ranges :doc "Sorted by ppem"))


(record gasp-range
 (field range-max-ppem u16 :doc "Upper limit of range, in PPEM")
 (field range-gasp-behavior gasp-range-behavior :doc
  "Flags describing desired rasterizer behavior."))


(flags u16 gasp-range-behavior (gasp-gridfit 1) (gasp-dogray 2)
 (gasp-symmetric-gridfit 4) (gasp-symmetric-smoothing 8))

