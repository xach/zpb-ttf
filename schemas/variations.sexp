;;;; variations.sexp — OpenType table schema
;;;; Generated from Fontations variations.rs spec


(table tuple-variation-header
 (:doc
  "[TupleVariationHeader](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#tuplevariationheader)")
 (field variation-data-size u16 :doc
  "The size in bytes of the serialized data for this tuple variation table.")
 (field tuple-index tuple-index :doc
  "A packed field. The high 4 bits are flags (see below). The low 12 bits are an index into a shared tuple records array.")
 (field peak-tuple (array f2-dot14) :count (tuple_len tuple-index axis-count 0)
  :doc
  "Peak tuple record for this tuple variation table — optional, determined by flags in the tupleIndex value.  Note that this must always be included in the 'cvar' table.")
 (field intermediate-start-tuple (array f2-dot14) :count
  (tuple_len tuple-index axis-count 1) :doc
  "Intermediate start tuple record for this tuple variation table — optional, determined by flags in the tupleIndex value.")
 (field intermediate-end-tuple (array f2-dot14) :count
  (tuple_len tuple-index axis-count 1) :doc
  "Intermediate end tuple record for this tuple variation table — optional, determined by flags in the tupleIndex value."))


(record tuple
 (:doc
  "A [Tuple Record](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#tuple-records)  The tuple variation store formats reference regions within the font’s variation space using tuple records. A tuple record identifies a position in terms of normalized coordinates, which use F2DOT14 values.")
 (field values (array f2-dot14) :count axis-count :doc
  "Coordinate array specifying a position within the font’s variation space.  The number of elements must match the axisCount specified in the 'fvar' table."))


(table delta-set-index-map-format0
 (:doc
  "The [DeltaSetIndexMap](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#associating-target-items-to-variation-data) table format 0")
 (field format-table u8 :format 0 :doc "DeltaSetIndexMap format: set to 0.")
 (field entry-format entry-format :doc
  "A packed field that describes the compressed representation of delta-set indices. See details below.")
 (field map-count u16 :doc "The number of mapping entries.")
 (field map-data (array u8) :count
  (delta_set_index_data entry-format map-count) :doc
  "The delta-set index mapping data. See details below."))


(table delta-set-index-map-format1
 (:doc
  "The [DeltaSetIndexMap](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#associating-target-items-to-variation-data) table format 1")
 (field format-table u8 :format 1 :doc "DeltaSetIndexMap format: set to 1.")
 (field entry-format entry-format :doc
  "A packed field that describes the compressed representation of delta-set indices. See details below.")
 (field map-count u32 :doc "The number of mapping entries.")
 (field map-data (array u8) :count
  (delta_set_index_data entry-format map-count) :doc
  "The delta-set index mapping data. See details below."))


(format-table u8 delta-set-index-map
 (:doc
  "The [DeltaSetIndexMap](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#associating-target-items-to-variation-data) table")
 (variant format0 delta-set-index-map-format0)
 (variant format1 delta-set-index-map-format1))


(flags u8 entry-format (:doc "Entry format for a [DeltaSetIndexMap].")
 (inner-index-bit-count-mask 15) (map-entry-size-mask 48))


(table variation-region-list
 (:doc
  "The [VariationRegionList](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#variation-regions) table")
 (field axis-count u16 :doc
  "The number of variation axes for this font. This must be the same number as axisCount in the 'fvar' table.")
 (field region-count u16 :doc
  "The number of variation region tables in the variation region list. Must be less than 32,768.")
 (field variation-regions computed-array<variation-region<a>> :doc
  "Array of variation regions."))


(record variation-region
 (:doc
  "The [VariationRegion](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#variation-regions) record")
 (field region-axes (array region-axis-coordinates) :count axis-count :doc
  "Array of region axis coordinates records, in the order of axes given in the 'fvar' table."))


(record region-axis-coordinates
 (:doc
  "The [RegionAxisCoordinates](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#variation-regions) record")
 (field start-coord f2-dot14 :doc
  "The region start coordinate value for the current axis.")
 (field peak-coord f2-dot14 :doc
  "The region peak coordinate value for the current axis.")
 (field end-coord f2-dot14 :doc
  "The region end coordinate value for the current axis."))


(table item-variation-store
 (:doc
  "The [ItemVariationStore](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#item-variation-store-header-and-item-variation-data-subtables) table")
 (field format-table u16 :doc "Format— set to 1")
 (field variation-region-list-offset (offset32 variation-region-list) :doc
  "Offset in bytes from the start of the item variation store to the variation region list.")
 (field item-variation-data-count u16 :doc
  "The number of item variation data subtables.")
 (field item-variation-data-offsets (array (offset32 item-variation-data))
  :count item-variation-data-count :doc
  "Offsets in bytes from the start of the item variation store to each item variation data subtable."))


(table item-variation-data
 (:doc
  "The [ItemVariationData](https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#item-variation-store-header-and-item-variation-data-subtables) subtable")
 (field item-count u16 :doc "The number of delta sets for distinct items.")
 (field word-delta-count u16 :doc
  "A packed field: the high bit is a flag—see details below.")
 (field region-index-count u16 :doc
  "The number of variation regions referenced.")
 (field region-indexes (array u16) :count region-index-count :doc
  "Array of indices into the variation region list for the regions referenced by this item variation data table.")
 (field delta-sets (array u8) :count
  (item_variation_data_len item-count word-delta-count region-index-count) :doc
  "Delta-set rows."))

