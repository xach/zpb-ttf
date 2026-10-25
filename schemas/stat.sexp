;;;; stat.sexp — OpenType table schema
;;;; Generated from Fontations stat.rs spec


(table stat (:tag "STAT")
 (:doc
  "[STAT](https://docs.microsoft.com/en-us/typography/opentype/spec/stat) (Style Attributes Table)")
 (field version major-minor :doc
  "Major/minor version number. Set to 1.2 for new fonts.")
 (field design-axis-size u16 :doc "The size in bytes of each axis record.")
 (field design-axis-count u16 :doc
  "The number of axis records. In a font with an 'fvar' table, this value must be greater than or equal to the axisCount value in the 'fvar' table. In all fonts, must be greater than zero if axisValueCount is greater than zero.")
 (field design-axes-offset (offset32 [axis-record]) :doc
  "Offset in bytes from the beginning of the STAT table to the start of the design axes array. If designAxisCount is zero, set to zero; if designAxisCount is greater than zero, must be greater than zero.")
 (field axis-value-count u16 :doc "The number of axis value tables.")
 (field offset-to-axis-value-offsets (offset32 axis-value-array) :doc
  "Offset in bytes from the beginning of the STAT table to the start of the design axes value offsets array. If axisValueCount is zero, set to zero; if axisValueCount is greater than zero, must be greater than zero.")
 (field elided-fallback-name-id name-id :since-version "1.1" :doc
  "Name ID used as fallback when projection of names into a particular font model produces a subfamily name containing only elidable elements."))


(record axis-record
 (:doc
  "[Axis Records](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-records)")
 (field axis-tag tag :doc "A tag identifying the axis of design variation.")
 (field axis-name-id name-id :doc
  "The name ID for entries in the 'name' table that provide a display string for this axis.")
 (field axis-ordering u16 :doc
  "A value that applications can use to determine primary sorting of face names, or for ordering of labels when composing family or face names."))


(table axis-value-array (:doc "An array of [AxisValue] tables.")
 (field axis-value-offsets (array (offset16 axis-value)) :count
  axis-value-count :doc
  "Array of offsets to axis value tables, in bytes from the start of the axis value offsets array."))


(format-table u16 axis-value
 (:doc
  "[Axis Value Tables](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-tables)")
 (variant format1 axis-value-format1) (variant format2 axis-value-format2)
 (variant format3 axis-value-format3) (variant format4 axis-value-format4))


(table axis-value-format1
 (:doc
  "[Axis value table format 1](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-table-format-1)")
 (field format-table u16 :format 1 :doc "Format identifier — set to 1.")
 (field axis-index u16 :doc
  "Zero-base index into the axis record array identifying the axis of design variation to which the axis value table applies. Must be less than designAxisCount.")
 (field flags axis-value-table-flags :doc "Flags — see below for details.")
 (field value-name-id name-id :doc
  "The name ID for entries in the 'name' table that provide a display string for this attribute value.")
 (field value fixed :doc "A numeric value for this attribute value."))


(table axis-value-format2
 (:doc
  "[Axis value table format 2](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-table-format-2)")
 (field format-table u16 :format 2 :doc "Format identifier — set to 2.")
 (field axis-index u16 :doc
  "Zero-base index into the axis record array identifying the axis of design variation to which the axis value table applies. Must be less than designAxisCount.")
 (field flags axis-value-table-flags :doc "Flags — see below for details.")
 (field value-name-id name-id :doc
  "The name ID for entries in the 'name' table that provide a display string for this attribute value.")
 (field nominal-value fixed :doc
  "A nominal numeric value for this attribute value.")
 (field range-min-value fixed :doc
  "The minimum value for a range associated with the specified name ID.")
 (field range-max-value fixed :doc
  "The maximum value for a range associated with the specified name ID."))


(table axis-value-format3
 (:doc
  "[Axis value table format 3](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-table-format-3)")
 (field format-table u16 :format 3 :doc "Format identifier — set to 3.")
 (field axis-index u16 :doc
  "Zero-base index into the axis record array identifying the axis of design variation to which the axis value table applies. Must be less than designAxisCount.")
 (field flags axis-value-table-flags :doc "Flags — see below for details.")
 (field value-name-id name-id :doc
  "The name ID for entries in the 'name' table that provide a display string for this attribute value.")
 (field value fixed :doc "A numeric value for this attribute value.")
 (field linked-value fixed :doc
  "The numeric value for a style-linked mapping from this value."))


(table axis-value-format4
 (:doc
  "[Axis value table format 4](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#axis-value-table-format-4)")
 (field format-table u16 :format 4 :doc "Format identifier — set to 4.")
 (field axis-count u16 :doc
  "The total number of axes contributing to this axis-values combination.")
 (field flags axis-value-table-flags :doc "Flags — see below for details.")
 (field value-name-id name-id :doc
  "The name ID for entries in the 'name' table that provide a display string for this combination of axis values.")
 (field axis-values (array axis-value-record) :count axis-count :doc
  "Array of AxisValue records that provide the combination of axis values, one for each contributing axis."))


(record axis-value-record (:doc "Part of [AxisValueFormat4]")
 (field axis-index u16 :doc
  "Zero-base index into the axis record array identifying the axis to which this value applies. Must be less than designAxisCount.")
 (field value fixed :doc "A numeric value for this attribute value."))


(flags u16 axis-value-table-flags
 (:doc
  "[Axis value table flags](https://docs.microsoft.com/en-us/typography/opentype/spec/stat#flags).")
 (older-sibling-font-attribute 1) (elidable-axis-value-name 2))

