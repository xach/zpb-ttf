;;;; dsig.sexp — OpenType table schema
;;;; Generated from Fontations dsig.rs spec


(table dsig (:tag "DSIG")
 (:doc
  "[DSIG (Digital Signature Table)](https://docs.microsoft.com/en-us/typography/opentype/spec/dsig#table-structure) table")
 (field version u32 :doc "Version number of the DSIG table (0x00000001)")
 (field num-signatures u16 :doc "Number of signatures in the table")
 (field flags permission-flags :doc "Permission flags")
 (field signature-records (array signature-record) :count num-signatures :doc
  "Array of signature records"))


(flags u16 permission-flags
 (:doc
  "[Permission flags](https://learn.microsoft.com/en-us/typography/opentype/spec/dsig#table-structure)")
 (cannot-be-resigned 1))


(record signature-record
 (:doc
  "[Signature Record](https://learn.microsoft.com/en-us/typography/opentype/spec/dsig#table-structure)")
 (field format-table u32 :doc "Format of the signature")
 (field length u32 :doc "Length of signature in bytes")
 (field signature-block-offset (offset32 signature-block-format1) :doc
  "Offset to the signature block from the beginning of the table"))


(table signature-block-format1
 (:doc
  "[Signature Block Format 1](https://learn.microsoft.com/en-us/typography/opentype/spec/dsig#table-structure)")
 (field -reserved1 u16 :doc "Reserved for future use; set to zero.")
 (field -reserved2 u16 :doc "Reserved for future use; set to zero.")
 (field signature-length u32 :doc
  "Length (in bytes) of the PKCS#7 packet in the signature field.")
 (field signature (array u8) :count signature-length :doc "PKCS#7 packet"))

