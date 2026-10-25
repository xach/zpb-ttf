;;;; hvar.sexp — OpenType table schema
;;;; Generated from Fontations hvar.rs spec


(table hvar (:tag "HVAR")
 (:doc
  "The [HVAR (Horizontal Metrics Variations)](https://docs.microsoft.com/en-us/typography/opentype/spec/hvar) table")
 (field version major-minor :doc
  "Major version number of the horizontal metrics variations table — set to 1. Minor version number of the horizontal metrics variations table — set to 0.")
 (field item-variation-store-offset (offset32 item-variation-store) :doc
  "Offset in bytes from the start of this table to the item variation store table.")
 (field advance-width-mapping-offset (offset32 delta-set-index-map) :doc
  "Offset in bytes from the start of this table to the delta-set index mapping for advance widths (may be NULL).")
 (field lsb-mapping-offset (offset32 delta-set-index-map) :doc
  "Offset in bytes from the start of this table to the delta-set index mapping for left side bearings (may be NULL).")
 (field rsb-mapping-offset (offset32 delta-set-index-map) :doc
  "Offset in bytes from the start of this table to the delta-set index mapping for right side bearings (may be NULL)."))

