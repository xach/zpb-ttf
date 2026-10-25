;;;; vvar.sexp — OpenType table schema
;;;; Generated from Fontations vvar.rs spec


(table vvar (:tag "VVAR")
 (:doc
  "The [VVAR (Vertical Metrics Variations)](https://docs.microsoft.com/en-us/typography/opentype/spec/vvar) table")
 (field version major-minor :doc
  "Major version number of the horizontal metrics variations table — set to 1. Minor version number of the horizontal metrics variations table — set to 0.")
 (field item-variation-store-offset (offset32 item-variation-store) :doc
  "Offset in bytes from the start of this table to the item variation store table.")
 (field advance-height-mapping-offset (offset32 delta-set-index-map) :doc
  "Offset in bytes from the start of this table to the delta-set index mapping for advance heights (may be NULL).")
 (field tsb-mapping-offset (offset32 delta-set-index-map) :doc
  "Offset in bytes from the start of this table to the delta-set index mapping for top side bearings (may be NULL).")
 (field bsb-mapping-offset (offset32 delta-set-index-map) :doc
  "Offset in bytes from the start of this table to the delta-set index mapping for bottom side bearings (may be NULL).")
 (field v-org-mapping-offset (offset32 delta-set-index-map) :doc
  "Offset in bytes from the start of this table to the delta-set index mapping for Y coordinates of vertical origins (may be NULL)."))

