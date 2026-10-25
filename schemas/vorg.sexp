;;;; vorg.sexp — OpenType table schema
;;;; Generated from Fontations vorg.rs spec


(table vorg (:tag "VORG")
 (:doc
  "The [VORG (Vertical Origin)](https://docs.microsoft.com/en-us/typography/opentype/spec/vorg) table.")
 (field version major-minor :doc "Major/minor version number. Set to 1.0.")
 (field default-vert-origin-y i16 :doc
  "The y coordinate of a glyph’s vertical origin, in the font’s design coordinate system, to be used if no entry is present for the glyph in the vertOriginYMetrics array.")
 (field num-vert-origin-y-metrics u16 :doc
  "Number of elements in the vertOriginYMetrics array.")
 (field vert-origin-y-metrics (array vert-origin-y-metrics) :count
  num-vert-origin-y-metrics :doc
  "Array of VertOriginYMetrics records, sorted by glyph ID."))


(record vert-origin-y-metrics (:doc "Vertical origin Y metrics record.")
 (field glyph-index glyph-id16 :doc "Glyph index.")
 (field vert-origin-y i16 :doc
  "Y coordinate, in the font’s design coordinate system, of the glyph’s vertical origin."))

