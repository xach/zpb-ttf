;;;; fvar.sexp — OpenType table schema
;;;; Generated from Fontations fvar.rs spec


(table fvar (:tag "fvar")
 (:doc
  "The [fvar (Font Variations)](https://docs.microsoft.com/en-us/typography/opentype/spec/fvar) table")
 (field version major-minor :doc
  "Major version number of the font variations table — set to 1. Minor version number of the font variations table — set to 0.")
 (field axis-instance-arrays-offset (offset16 axis-instance-arrays) :doc
  "Offset in bytes from the beginning of the table to the start of the VariationAxisRecord array. The InstanceRecord array directly follows.")
 (field -reserved u16 :doc "This field is permanently reserved. Set to 2.")
 (field axis-count u16 :doc
  "The number of variation axes in the font (the number of records in the axes array).")
 (field axis-size u16 :doc
  "The size in bytes of each VariationAxisRecord — set to 20 (0x0014) for this version.")
 (field instance-count u16 :doc
  "The number of named instances defined in the font (the number of records in the instances array).")
 (field instance-size u16 :doc
  "The size in bytes of each InstanceRecord — set to either axisCount * sizeof(Fixed) + 4, or to axisCount * sizeof(Fixed) + 6."))


(table axis-instance-arrays
 (:doc "Shim table to handle combined axis and instance arrays.")
 (field axes (array variation-axis-record) :count axis-count :doc
  "Variation axis record array.")
 (field instances computed-array<instance-record<a>> :doc
  "Instance record array."))


(record variation-axis-record
 (:doc
  "The [VariationAxisRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar#variationaxisrecord)")
 (field axis-tag tag :doc "Tag identifying the design variation for the axis.")
 (field min-value fixed :doc "The minimum coordinate value for the axis.")
 (field default-value fixed :doc "The default coordinate value for the axis.")
 (field max-value fixed :doc "The maximum coordinate value for the axis.")
 (field flags u16 :doc "Axis qualifiers — see details below.")
 (field axis-name-id name-id :doc
  "The name ID for entries in the 'name' table that provide a display name for this axis."))

