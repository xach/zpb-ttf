;;;; hdmx.sexp — OpenType table schema
;;;; Generated from Fontations hdmx.rs spec


(table hdmx (:tag "hdmx")
 (:doc
  "The [Horizontal Device Metrics](https://learn.microsoft.com/en-us/typography/opentype/spec/hdmx) table.")
 (field version u16 :doc "Table version number (set to 0).")
 (field num-records u16 :doc "Number of device records.")
 (field size-device-record u32 :doc "Size of device record, 32-bit aligned.")
 (field records computed-array<device-record<a>> :doc
  "Array of device records."))

