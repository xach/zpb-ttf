;;;; trak.sexp — OpenType table schema
;;;; Generated from Fontations trak.rs spec


(table trak (:tag "trak")
 (:doc
  "The [tracking (trak)](https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6trak.html) table.")
 (field version major-minor :doc
  "Version number of the tracking table (0x00010000 for the current version).")
 (field format-table u16 :doc "Format of the tracking table (set to 0).")
 (field horiz-offset (offset16 track-data) :doc
  "Offset from start of tracking table to TrackData for horizontal text (or 0 if none).")
 (field vert-offset (offset16 track-data) :doc
  "Offset from start of tracking table to TrackData for vertical text (or 0 if none).")
 (field reserved u16 :doc "Reserved. Set to 0."))


(table track-data (:doc "The tracking data table.")
 (field n-tracks u16 :doc "Number of separate tracks included in this table.")
 (field n-sizes u16 :doc "Number of point sizes included in this table.")
 (field size-table-offset u32 :doc
  "Offset from the start of the tracking table to the start of the size subtable.")
 (field track-table (array track-table-entry) :count n-tracks :doc
  "Array of TrackTableEntry records."))


(record track-table-entry (:doc "Single entry in a tracking table.")
 (field track fixed :doc "Track value for this record.")
 (field name-index name-id :doc
  "The 'name' table index for this track (a short word or phrase like \"loose\" or \"very tight\"). NameIndex has a value greater than 255 and less than 32768.")
 (field offset u16 :doc
  "Offset from the start of the tracking table to per-size tracking values for this track."))

