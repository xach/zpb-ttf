;;;; feat.sexp — OpenType table schema
;;;; Generated from Fontations feat.rs spec


(table feat (:tag "feat")
 (:doc
  "The [feature name](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6feat.html) table.")
 (field version major-minor :doc
  "Version number of the feature name table (0x00010000 for the current version).")
 (field feature-name-count u16 :doc
  "The number of entries in the feature name array.")
 (field -reserved1 u16 :doc "Reserved (set to 0).")
 (field -reserved2 u32 :doc "Reserved (set to 0).")
 (field names (array feature-name) :count feature-name-count :doc
  "The feature name array, sorted by feature type."))


(record feature-name (:doc "Type, flags and names for a feature.")
 (field feature u16 :doc "Feature type.")
 (field n-settings u16 :doc "The number of records in the setting name array.")
 (field setting-table-offset (offset32 setting-name-array) :doc
  "Offset in bytes from the beginning of this table to this feature's setting name array. The actual type of record this offset refers to will depend on the exclusivity value, as described below.")
 (field feature-flags u16 :doc "Flags associated with the feature type.")
 (field name-index name-id :doc "The name table index for the feature's name."))


(table setting-name-array
 (field settings (array setting-name) :count n-settings :doc
  "List of setting names for a feature."))


(record setting-name (:doc "Associates a setting with a name identifier.")
 (field setting u16 :doc "The setting.")
 (field name-index name-id :doc "The name table index for the setting's name."))

