;;;; svg.sexp — OpenType table schema
;;;; Generated from Fontations svg.rs spec


(table svg (:tag "SVG ")
 (:doc
  "The [SVG](https://learn.microsoft.com/en-us/typography/opentype/spec/svg) table")
 (field version u16 :doc "Table version (starting at 0). Set to 0.")
 (field svg-document-list-offset (offset32 svg-document-list) :doc
  "Offset to the SVGDocumentList, from the start of the SVG table. Must be non-zero.")
 (field -reserved u16 :doc "Set to 0."))


(table svg-document-list
 (:doc
  "[SVGDocumentList](https://learn.microsoft.com/en-us/typography/opentype/spec/svg)")
 (field num-entries u16 :doc "Number of SVGDocumentRecords. Must be non-zero.")
 (field document-records (array svg-document-record) :count num-entries :doc
  "Array of SVGDocumentRecords."))


(record svg-document-record
 (:doc
  "[SVGDocumentRecord](https://learn.microsoft.com/en-us/typography/opentype/spec/svg)")
 (field start-glyph-id glyph-id16 :doc
  "The first glyph ID for the range covered by this record.")
 (field end-glyph-id glyph-id16 :doc
  "The last glyph ID for the range covered by this record.")
 (field svg-doc-offset u32 :doc
  "Offset from the beginning of the SVGDocumentList to an SVG document. Must be non-zero.")
 (field svg-doc-length u32 :doc
  "Length of the SVG document data. Must be non-zero."))

