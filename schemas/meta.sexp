;;;; meta.sexp — OpenType table schema
;;;; Generated from Fontations meta.rs spec


(table meta (:tag "meta")
 (:doc
  "[`meta`](https://docs.microsoft.com/en-us/typography/opentype/spec/meta)")
 (field version u32 :doc "Version number of the metadata table — set to 1.")
 (field flags u32 :doc "Flags — currently unused; set to 0.")
 (field reserved u32 :doc "Not used; set to 0.")
 (field data-maps-count u32 :doc "The number of data maps in the table.")
 (field data-maps (array data-map-record) :count data-maps-count :doc
  "Array of data map records."))


(record data-map-record
 (:doc
  "<https://learn.microsoft.com/en-us/typography/opentype/spec/meta#table-formats>")
 (field tag tag :doc "A tag indicating the type of metadata.")
 (field data-offset (offset32 metadata) :doc
  "Offset in bytes from the beginning of the metadata table to the data for this tag.")
 (field data-length u32 :doc
  "Length of the data, in bytes. The data is not required to be padded to any byte boundary."))

