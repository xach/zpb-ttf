;;;; name.sexp — OpenType table schema
;;;; Generated from Fontations name.rs spec


(table name (:tag "name")
 (:doc
  "[Naming table version 1](https://docs.microsoft.com/en-us/typography/opentype/spec/name#naming-table-version-1)")
 (field version u16 :doc "Table version number (0 or 1)")
 (field count u16 :doc "Number of name records.")
 (field storage-offset u16 :doc
  "Offset to start of string storage (from start of table).")
 (field name-record (array name-record) :count count :doc
  "The name records where count is the number of records.")
 (field lang-tag-count u16 :since-version "1" :doc
  "Number of language-tag records.")
 (field lang-tag-record (array lang-tag-record) :count lang-tag-count
  :since-version "1" :doc
  "The language-tag records where langTagCount is the number of records."))


(record lang-tag-record (:doc "Part of [Name]")
 (field length u16 :doc "Language-tag string length (in bytes)")
 (field lang-tag-offset (offset16 name-string) :doc
  "Language-tag string offset from start of storage area (in bytes)."))


(record name-record
 (:doc
  "[Name Records](https://docs.microsoft.com/en-us/typography/opentype/spec/name#name-records)")
 (field platform-id u16 :doc "Platform ID.")
 (field encoding-id u16 :doc "Platform-specific encoding ID.")
 (field language-id u16 :doc "Language ID.")
 (field name-id name-id :doc "Name ID.")
 (field length u16 :doc "String length (in bytes).")
 (field string-offset (offset16 name-string) :doc
  "String offset from start of storage area (in bytes)."))

