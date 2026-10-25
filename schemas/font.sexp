;;;; font.sexp — OpenType table schema
;;;; Generated from Fontations font.rs spec


(table table-directory
 (:doc
  "The OpenType [Table Directory](https://docs.microsoft.com/en-us/typography/opentype/spec/otff#table-directory)")
 (field sfnt-version u32 :doc "0x00010000 or 0x4F54544F")
 (field num-tables u16 :doc "Number of tables.") (field search-range u16)
 (field entry-selector u16) (field range-shift u16)
 (field table-records (array table-record) :count num-tables :doc
  "Table records array—one for each top-level table in the font"))


(record table-record (:doc "Record for a table in a font.")
 (field tag tag :doc "Table identifier.")
 (field checksum u32 :doc "Checksum for the table.")
 (field offset u32 :doc "Offset from the beginning of the font data.")
 (field length u32 :doc "Length of the table."))


(table ttc-header
 (:doc
  "[TTC Header](https://learn.microsoft.com/en-us/typography/opentype/spec/otff#ttc-header)")
 (field ttc-tag tag :doc "Font Collection ID string: \"ttcf\"")
 (field version major-minor :doc "Major/minor version of the TTC Header")
 (field num-fonts u32 :doc "Number of fonts in TTC")
 (field table-directory-offsets (array u32) :count num-fonts :doc
  "Array of offsets to the TableDirectory for each font from the beginning of the file")
 (field dsig-tag u32 :since-version "2.0" :doc
  "Tag indicating that a DSIG table exists, 0x44534947 ('DSIG') (null if no signature)")
 (field dsig-length u32 :since-version "2.0" :doc
  "The length (in bytes) of the DSIG table (null if no signature)")
 (field dsig-offset u32 :since-version "2.0" :doc
  "The offset (in bytes) of the DSIG table from the beginning of the TTC file (null if no signature)"))

