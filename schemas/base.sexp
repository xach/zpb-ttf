;;;; base.sexp — OpenType table schema
;;;; Generated from Fontations base.rs spec


(table base (:tag "BASE")
 (:doc
  "The [BASE](https://learn.microsoft.com/en-us/typography/opentype/spec/base) (Baseline) table")
 (field version major-minor :doc
  "(major, minor) Version for the BASE table (1,0) or (1,1)")
 (field horiz-axis-offset (offset16 axis) :doc
  "Offset to horizontal Axis table, from beginning of BASE table (may be NULL)")
 (field vert-axis-offset (offset16 axis) :doc
  "Offset to vertical Axis table, from beginning of BASE table (may be NULL)")
 (field item-var-store-offset (offset32 item-variation-store) :since-version
  "1.1" :doc
  "Offset to Item Variation Store table, from beginning of BASE table (may be null)"))


(table axis
 (:doc
  "[Axis Table](https://learn.microsoft.com/en-us/typography/opentype/spec/base#axis-tables-horizaxis-and-vertaxis)")
 (field base-tag-list-offset (offset16 base-tag-list) :doc
  "Offset to BaseTagList table, from beginning of Axis table (may be NULL)")
 (field base-script-list-offset (offset16 base-script-list) :doc
  "Offset to BaseScriptList table, from beginning of Axis table"))


(table base-tag-list
 (:doc
  "[BaseTagList Table](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basetaglist-table)")
 (field base-tag-count u16 :doc
  "Number of baseline identification tags in this text direction — may be zero (0)")
 (field baseline-tags (array tag) :count base-tag-count :doc
  "Array of 4-byte baseline identification tags — must be in alphabetical order"))


(table base-script-list
 (:doc
  "[BaseScriptList Table](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basescriptlist-table)")
 (field base-script-count u16 :doc "Number of BaseScriptRecords defined")
 (field base-script-records (array base-script-record) :count base-script-count
  :doc "Array of BaseScriptRecords, in alphabetical order by baseScriptTag"))


(record base-script-record
 (:doc
  "[BaseScriptRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basescriptrecord)")
 (field base-script-tag tag :doc "4-byte script identification tag")
 (field base-script-offset (offset16 base-script) :doc
  "Offset to BaseScript table, from beginning of BaseScriptList"))


(table base-script
 (:doc
  "[BaseScript Table](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basescript-table)")
 (field base-values-offset (offset16 base-values) :doc
  "Offset to BaseValues table, from beginning of BaseScript table (may be NULL)")
 (field default-min-max-offset (offset16 min-max) :doc
  "Offset to MinMax table, from beginning of BaseScript table (may be NULL)")
 (field base-lang-sys-count u16 :doc
  "Number of BaseLangSysRecords defined — may be zero (0)")
 (field base-lang-sys-records (array base-lang-sys-record) :count
  base-lang-sys-count :doc
  "Array of BaseLangSysRecords, in alphabetical order by BaseLangSysTag"))


(record base-lang-sys-record
 (:doc
  "[BaseLangSysRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/base#baselangsysrecord)")
 (field base-lang-sys-tag tag :doc "4-byte language system identification tag")
 (field min-max-offset (offset16 min-max) :doc
  "Offset to MinMax table, from beginning of BaseScript table"))


(table base-values
 (:doc
  "[BaseValues](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basevalues-table) table")
 (field default-baseline-index u16 :doc
  "Index number of default baseline for this script — equals index position of baseline tag in baselineTags array of the BaseTagList")
 (field base-coord-count u16 :doc
  "Number of BaseCoord tables defined — should equal baseTagCount in the BaseTagList")
 (field base-coord-offsets (array (offset16 base-coord)) :count
  base-coord-count :doc
  "Array of offsets to BaseCoord tables, from beginning of BaseValues table — order matches baselineTags array in the BaseTagList"))


(table min-max
 (:doc
  "[MinMax](https://learn.microsoft.com/en-us/typography/opentype/spec/base#minmax-table) table")
 (field min-coord-offset (offset16 base-coord) :doc
  "Offset to BaseCoord table that defines the minimum extent value, from the beginning of MinMax table (may be NULL)")
 (field max-coord-offset (offset16 base-coord) :doc
  "Offset to BaseCoord table that defines maximum extent value, from the beginning of MinMax table (may be NULL)")
 (field feat-min-max-count u16 :doc
  "Number of FeatMinMaxRecords — may be zero (0)")
 (field feat-min-max-records (array feat-min-max-record) :count
  feat-min-max-count :doc
  "Array of FeatMinMaxRecords, in alphabetical order by featureTableTag"))


(record feat-min-max-record
 (:doc
  "[FeatMinMaxRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/base#baselangsysrecord)")
 (field feature-table-tag tag :doc
  "4-byte feature identification tag — must match feature tag in FeatureList")
 (field min-coord-offset (offset16 base-coord) :doc
  "Offset to BaseCoord table that defines the minimum extent value, from beginning of MinMax table (may be NULL)")
 (field max-coord-offset (offset16 base-coord) :doc
  "Offset to BaseCoord table that defines the maximum extent value, from beginning of MinMax table (may be NULL)"))


(format-table u16 base-coord (variant format1 base-coord-format1)
 (variant format2 base-coord-format2) (variant format3 base-coord-format3))


(table base-coord-format1
 (:doc
  "[BaseCoordFormat1](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basecoord-format-1)")
 (field base-coord-format u16 :format 1 :doc "Format identifier — format = 1")
 (field coordinate i16 :doc "X or Y value, in design units"))


(table base-coord-format2
 (:doc
  "[BaseCoordFormat2](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basecoord-format-2)")
 (field base-coord-format u16 :format 2 :doc "Format identifier — format = 2")
 (field coordinate i16 :doc "X or Y value, in design units")
 (field reference-glyph u16 :doc "Glyph ID of control glyph")
 (field base-coord-point u16 :doc
  "Index of contour point on the reference glyph"))


(table base-coord-format3
 (:doc
  "[BaseCoordFormat3](https://learn.microsoft.com/en-us/typography/opentype/spec/base#basecoord-format-3)")
 (field base-coord-format u16 :format 3 :doc "Format identifier — format = 3")
 (field coordinate i16 :doc "X or Y value, in design units")
 (field device-offset (offset16 device-or-variation-index) :doc
  "Offset to Device table (non-variable font) / Variation Index table (variable font) for X or Y value, from beginning of BaseCoord table (may be NULL)."))

