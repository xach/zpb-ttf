;;;; cff2.sexp — OpenType table schema
;;;; Generated from Fontations cff2.rs spec


(table cff2-header
 (:doc
  "[Compact Font Format (CFF) version 2](https://learn.microsoft.com/en-us/typography/opentype/spec/cff2) table header")
 (field major-version u8 :doc "Format major version (set to 2).")
 (field minor-version u8 :doc "Format minor version (set to 0).")
 (field header-size u8 :doc "Header size (bytes).")
 (field top-dict-length u16 :doc "Length of Top DICT structure in bytes.")
 (field -padding (array u8) :count (subtract header-size 5) :doc
  "Padding bytes before the start of the Top DICT.")
 (field top-dict-data (array u8) :count top-dict-length :doc
  "Data containing the Top DICT.")
 (field trailing-data (array u8) :count :remainder :doc
  "Remaining table data."))


(table cff2-index
 (:doc "An array of variable-sized objects in a `CFF2` table.")
 (field count u32 :doc "Number of objects stored in INDEX.")
 (field off-size u8 :doc "Object array element size.")
 (field offsets (array u8) :count (add_multiply count 1 off-size) :doc
  "Bytes containing `count + 1` offsets each of `off_size`.")
 (field data (array u8) :count :remainder :doc
  "Array containing the object data."))

