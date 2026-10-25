;;;; mvar.sexp — OpenType table schema
;;;; Generated from Fontations mvar.rs spec


(table mvar (:tag "MVAR")
 (:doc
  "The [MVAR (Metrics Variations)](https://docs.microsoft.com/en-us/typography/opentype/spec/mvar) table")
 (field version major-minor :doc
  "Major version number of the horizontal metrics variations table — set to 1. Minor version number of the horizontal metrics variations table — set to 0.")
 (field -reserved u16 :doc "Not used; set to 0.")
 (field value-record-size u16 :doc
  "The size in bytes of each value record — must be greater than zero.")
 (field value-record-count u16 :doc
  "The number of value records — may be zero.")
 (field item-variation-store-offset (offset16 item-variation-store) :doc
  "Offset in bytes from the start of this table to the item variation store table. If valueRecordCount is zero, set to zero; if valueRecordCount is greater than zero, must be greater than zero.")
 (field value-records (array mvar-value-record) :count value-record-count :doc
  "Array of value records that identify target items and the associated delta-set index for each. The valueTag records must be in binary order of their valueTag field."))


(record mvar-value-record
 (:doc
  "[ValueRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/mvar#table-formats) metrics variation record")
 (field value-tag tag :doc "Four-byte tag identifying a font-wide measure.")
 (field delta-set-outer-index u16 :doc
  "A delta-set outer index — used to select an item variation data subtable within the item variation store.")
 (field delta-set-inner-index u16 :doc
  "A delta-set inner index — used to select a delta-set row within an item variation data subtable."))

