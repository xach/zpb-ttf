;;;; vmtx.sexp — OpenType table schema
;;;; Generated from Fontations vmtx.rs spec


(table vmtx (:tag "vmtx")
 (:doc
  "The [vmtx (Vertical Metrics)](https://docs.microsoft.com/en-us/typography/opentype/spec/vmtx) table")
 (field v-metrics (array long-metric) :count number-of-long-ver-metrics :doc
  "Paired advance height and top side bearing values for each glyph. Records are indexed by glyph ID.")
 (field top-side-bearings (array i16) :count :remainder :doc
  "Top side bearings for glyph IDs greater than or equal to numberOfLongMetrics."))


(table vmtx-extended (:tag "VMTX")
 (:doc "VMTX vertical metrics (ISO/IEC 14496-22:2026, 5.6.14).")
 (field v-metrics (array long-metric) :count number-of-long-ver-metrics)
 (field top-side-bearings (array i16) :count :remainder))

