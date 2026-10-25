;;;; avar.sexp — OpenType table schema
;;;; Generated from Fontations avar.rs spec


(table avar (:tag "avar")
 (:doc
  "The [avar (Axis Variations)](https://docs.microsoft.com/en-us/typography/opentype/spec/avar) table")
 (field version major-minor :doc
  "Major version number of the axis variations table — set to 1 or 2. Minor version number of the axis variations table — set to 0.")
 (field -reserved u16 :doc "Permanently reserved; set to zero.")
 (field axis-count u16 :doc
  "The number of variation axes for this font. This must be the same number as axisCount in the 'fvar' table.")
 (field axis-segment-maps var-len-array<segment-maps<a>> :doc
  "The segment maps array — one segment map for each axis, in the order of axes specified in the 'fvar' table.")
  ;; avar 2.0 axisIndexMapOffset and varStoreOffset follow the variable-length
  ;; segment maps, so their position must be computed by walking the maps.
  )


(record segment-maps
 (:doc
  "[SegmentMaps](https://learn.microsoft.com/en-us/typography/opentype/spec/avar#table-formats) record")
 (field position-map-count u16 :doc
  "The number of correspondence pairs for this axis.")
 (field axis-value-maps (array axis-value-map) :count position-map-count :doc
  "The array of axis value map records for this axis."))


(record axis-value-map
 (:doc
  "[AxisValueMap](https://learn.microsoft.com/en-us/typography/opentype/spec/avar#table-formats) record")
 (field from-coordinate f2-dot14 :doc
  "A normalized coordinate value obtained using default normalization.")
 (field to-coordinate f2-dot14 :doc
  "The modified, normalized coordinate value."))

