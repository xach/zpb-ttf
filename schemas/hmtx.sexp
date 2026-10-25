;;;; hmtx.sexp — OpenType table schema
;;;; Generated from Fontations hmtx.rs spec


(table hmtx (:tag "hmtx")
 (:doc
  "The [hmtx (Horizontal Metrics)](https://docs.microsoft.com/en-us/typography/opentype/spec/hmtx) table")
 (field h-metrics (array long-metric) :count number-of-h-metrics :doc
  "Paired advance width/height and left/top side bearing values for each glyph. Records are indexed by glyph ID.")
 (field left-side-bearings (array i16) :count :remainder :doc
  "Leading (left/top) side bearings for glyph IDs greater than or equal to numberOfLongMetrics."))


(table hmtx-extended (:tag "HMTX")
 (:doc "HMTX horizontal metrics (ISO/IEC 14496-22:2026, 5.1.11).")
 (field h-metrics (array long-metric) :count number-of-h-metrics)
 (field left-side-bearings (array i16) :count :remainder))


;; The advance is uint16, also in the extended HMTX/VMTX tables, which reuse
;; this record unchanged (as Fontations' read-fonts and write-fonts do). No
;; current standard text defines a 32-bit advance; don't widen it.
(record long-metric
 (field advance u16 :doc "Advance width/height, in font design units.")
 (field side-bearing i16 :doc
  "Glyph leading (left/top) side bearing, in font design units."))
